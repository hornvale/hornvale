//! Deterministic map renders in the First Light tradition: an
//! equirectangular PNG elevation map (decision 0018) and an ASCII map for
//! the REPL. Same globe, same seed, same bytes — a changed artifact in
//! review means changed behavior. Pixel→cell lookup uses the kernel's
//! `NearestCellIndex` (a latitude-band index, 30 bands of 6°): the nearest
//! cell center at level ≥ 4 is within ~2.5°, so the pixel's band plus both
//! neighbors always contains it.

use crate::globe::TectonicGlobe;
use crate::streams;
use hornvale_kernel::{Geosphere, NearestCellIndex, Seed, math, noise};

/// Raster image width in pixels; the image is equirectangular, so height is
/// `MAP_WIDTH / 2`. 1024×512; pixel ≈ 0.35°, fine enough to show the
/// refined coastline.
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: u32 = 1024;
/// ASCII map width in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters (2:1 world on ~2:1-tall glyphs).
/// type-audit: bare-ok(render-internal)
pub const ASCII_HEIGHT: u32 = 24;

/// Peak coastal displacement, meters. Bounds |refined − interpolated|.
const COAST_AMP_M: f64 = 150.0;
/// Gaussian envelope width, meters: displacement fades as the interpolated
/// elevation leaves sea level and is exactly zero beyond three widths.
const COAST_ENVELOPE_M: f64 = 300.0;
/// Base spatial frequency of the coastline noise over unit-sphere
/// coordinates (features ~1/24 rad ≈ 2.4° at the base octave).
const COAST_FREQ: f64 = 24.0;
/// fBm octaves for the coastline noise (base 2.4° down to ~0.15°).
const COAST_OCTAVES: u32 = 5;

/// Unit vector for a latitude/longitude in degrees (inverse of the
/// kernel's `Geosphere::coord` convention).
fn direction(latitude: f64, longitude: f64) -> [f64; 3] {
    let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
    [
        math::cos(lat) * math::cos(lon),
        math::cos(lat) * math::sin(lon),
        math::sin(lat),
    ]
}

/// Seam-free coastline noise in [−1, 1) at a unit-sphere position: the
/// mean of three orthogonal 2D fBm slices, recentred. Stateless hash-noise
/// under `terrain/coast-render` — no `Stream` is ever consumed.
fn coast_noise(noise_seed: Seed, p: [f64; 3]) -> f64 {
    let slices = [
        (noise_seed.derive("slice-0"), p[0], p[1]),
        (noise_seed.derive("slice-1"), p[1], p[2]),
        (noise_seed.derive("slice-2"), p[2], p[0]),
    ];
    let mean = slices
        .iter()
        .map(|(s, a, b)| noise::fbm_2d(*s, COAST_FREQ * a, COAST_FREQ * b, COAST_OCTAVES))
        .sum::<f64>()
        / 3.0;
    2.0 * mean - 1.0
}

/// Gaussian-weighted elevation over the nearest cell and its neighbors —
/// the refinement's prior, a convex combination of nearby cell values.
/// The weight width is half the nearest cell's mean neighbor spacing, so
/// candidates entering or leaving the set as the nearest cell flips carry
/// negligible weight (no visible seams).
fn interpolated_elevation(
    geo: &Geosphere,
    index: &NearestCellIndex,
    globe: &TectonicGlobe,
    latitude: f64,
    longitude: f64,
) -> f64 {
    let p = direction(latitude, longitude);
    let nearest = index.nearest(geo, latitude, longitude);
    let neighbors = geo.neighbors(nearest);
    let spacing = neighbors
        .iter()
        .map(|&n| angle(geo.position(nearest), geo.position(n)))
        .sum::<f64>()
        / neighbors.len() as f64;
    let sigma = spacing / 2.0;
    let mut weighted = 0.0;
    let mut total = 0.0;
    for cell in std::iter::once(nearest).chain(neighbors.iter().copied()) {
        let theta = angle(p, geo.position(cell));
        let weight = math::exp(-(theta * theta) / (sigma * sigma));
        weighted += weight * globe.elevation.get(cell).get();
        total += weight;
    }
    weighted / total
}

/// Angular distance between two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0))
}

/// The refined per-pixel elevation: the interpolated prior plus bounded
/// coastal displacement (coarse constrains fine, applied at the lens —
/// spec §3). Identical to the prior beyond three envelope widths.
fn refined_elevation(
    geo: &Geosphere,
    index: &NearestCellIndex,
    globe: &TectonicGlobe,
    noise_seed: Seed,
    latitude: f64,
    longitude: f64,
) -> f64 {
    let interp = interpolated_elevation(geo, index, globe, latitude, longitude);
    let d = (interp - globe.sea_level.get()) / COAST_ENVELOPE_M;
    if d.abs() > 3.0 {
        return interp;
    }
    let envelope = math::exp(-d * d);
    interp + COAST_AMP_M * envelope * coast_noise(noise_seed, direction(latitude, longitude))
}

/// Color a cell by elevation relative to sea level: ocean blues deepen with
/// depth; land climbs green → tan → brown → white.
fn color(elevation_m: f64, sea_level_m: f64) -> [u8; 3] {
    fn lerp(a: [u8; 3], b: [u8; 3], t: f64) -> [u8; 3] {
        let t = t.clamp(0.0, 1.0);
        let channel =
            |a: u8, b: u8| (f64::from(a) + (f64::from(b) - f64::from(a)) * t).round() as u8;
        [
            channel(a[0], b[0]),
            channel(a[1], b[1]),
            channel(a[2], b[2]),
        ]
    }
    if elevation_m < sea_level_m {
        let depth = sea_level_m - elevation_m;
        lerp([70, 130, 200], [10, 30, 80], depth / 6000.0)
    } else {
        let height = elevation_m - sea_level_m;
        if height < 800.0 {
            lerp([60, 140, 70], [150, 160, 90], height / 800.0)
        } else if height < 2500.0 {
            lerp([150, 160, 90], [140, 100, 70], (height - 800.0) / 1700.0)
        } else {
            lerp([140, 100, 70], [245, 245, 245], (height - 2500.0) / 2500.0)
        }
    }
}

/// Raw RGB pixels of the equirectangular elevation map (row-major, top row
/// first): longitude −180 → 180 across, latitude 90 → −90 down, pixel
/// centers sampled. Pixels are coastal-noise-refined from the world seed,
/// not raw cell values — see `refined_elevation`.
fn elevation_pixels(geo: &Geosphere, globe: &TectonicGlobe, world_seed: Seed) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestCellIndex::new(geo);
    let noise_seed = world_seed
        .derive(streams::ROOT)
        .derive(streams::COAST_RENDER);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let elevation = refined_elevation(geo, &index, globe, noise_seed, latitude, longitude);
            out.extend_from_slice(&color(elevation, globe.sea_level.get()));
        }
    }
    out
}

/// Render the globe as an equirectangular PNG (decision 0018). Same globe,
/// same seed, same bytes.
/// type-audit: bare-ok(artifact)
pub fn elevation_png(geo: &Geosphere, globe: &TectonicGlobe, world_seed: Seed) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(
        MAP_WIDTH,
        MAP_WIDTH / 2,
        &elevation_pixels(geo, globe, world_seed),
    )
}

/// Render the globe as a 72×24 ASCII map: '~' ocean, '.' lowland, '+'
/// hills, '^' mountains, 'A' high peaks. One newline per row.
/// type-audit: bare-ok(artifact)
pub fn elevation_ascii(geo: &Geosphere, globe: &TectonicGlobe) -> String {
    let index = NearestCellIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            let height = *globe.elevation.get(cell) - globe.sea_level;
            out.push(if height < 0.0 {
                '~'
            } else if height < 500.0 {
                '.'
            } else if height < 1500.0 {
                '+'
            } else if height < 3000.0 {
                '^'
            } else {
                'A'
            });
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn png_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let a = elevation_png(&geo, &globe, Seed(42));
        assert_eq!(a, elevation_png(&geo, &globe, Seed(42)));
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        // IHDR width and height, big-endian, at offsets 16 and 20.
        assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
        assert_eq!(&a[20..24], &(MAP_WIDTH / 2).to_be_bytes());
    }

    #[test]
    fn ascii_map_shows_both_ocean_and_land() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let map = elevation_ascii(&geo, &globe);
        assert_eq!(map.lines().count(), ASCII_HEIGHT as usize);
        for line in map.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH as usize);
        }
        assert!(map.contains('~'), "no ocean rendered");
        assert!(map.contains('.'), "no land rendered");
        assert_eq!(elevation_ascii(&geo, &globe), map);
    }

    #[test]
    fn refinement_respects_the_prior() {
        for seed in [7u64, 42, 99] {
            let geo = Geosphere::new(4);
            let globe = generate(Seed(seed), &geo, &TerrainPins::default())
                .unwrap()
                .globe;
            let index = NearestCellIndex::new(&geo);
            let noise_seed = Seed(seed)
                .derive(crate::streams::ROOT)
                .derive(crate::streams::COAST_RENDER);
            let (width, height) = (128u32, 64u32);
            for py in 0..height {
                let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
                for px in 0..width {
                    let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
                    let interp = interpolated_elevation(&geo, &index, &globe, latitude, longitude);
                    let refined =
                        refined_elevation(&geo, &index, &globe, noise_seed, latitude, longitude);
                    // Bounded displacement, always.
                    assert!((refined - interp).abs() <= COAST_AMP_M + 1e-9);
                    // Exactly the prior away from the coast.
                    if (interp - globe.sea_level.get()).abs() > 3.0 * COAST_ENVELOPE_M {
                        assert_eq!(refined, interp);
                    }
                    // A land/ocean flip only happens inside the displacement band.
                    let flipped =
                        (refined >= globe.sea_level.get()) != (interp >= globe.sea_level.get());
                    if flipped {
                        assert!((interp - globe.sea_level.get()).abs() <= COAST_AMP_M + 1e-9);
                    }
                }
            }
        }
    }

    #[test]
    fn interpolation_stays_within_the_candidate_envelope() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(7), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let index = NearestCellIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (45.5, -120.25), (-67.0, 13.0), (89.0, 179.0)] {
            let interp = interpolated_elevation(&geo, &index, &globe, latitude, longitude);
            let nearest = index.nearest(&geo, latitude, longitude);
            let mut lo = globe.elevation.get(nearest).get();
            let mut hi = lo;
            for &n in geo.neighbors(nearest) {
                lo = lo.min(globe.elevation.get(n).get());
                hi = hi.max(globe.elevation.get(n).get());
            }
            assert!(
                (lo..=hi).contains(&interp),
                "interp {interp} outside [{lo}, {hi}]"
            );
        }
    }
}
