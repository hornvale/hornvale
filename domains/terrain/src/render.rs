//! Deterministic map renders in the First Light tradition: an
//! equirectangular P6 PPM elevation map and an ASCII map for the REPL.
//! Same globe, same bytes — a changed artifact in review means changed
//! behavior. Pixel→cell lookup uses a latitude-band index (30 bands of 6°):
//! the nearest cell center at level ≥ 4 is within ~2.5°, so the pixel's
//! band plus both neighbors always contains it.

use crate::globe::TectonicGlobe;
use crate::plates::dot;
use hornvale_kernel::{CellId, Geosphere};

/// PPM image width in pixels; the image is equirectangular, so height is
/// `MAP_WIDTH / 2`.
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters (2:1 world on ~2:1-tall glyphs).
pub const ASCII_HEIGHT: u32 = 24;

/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

/// Latitude-band index for pixel→cell lookups: cells bucketed into 30 bands
/// of 6° (built in ascending cell order, so lookups are deterministic).
/// Searching the query's band ± 1 always contains the nearest cell center
/// (which lies within ~2.5° at level 4, ~1.3° at level 5).
struct LatBandIndex {
    bands: Vec<Vec<CellId>>,
}

impl LatBandIndex {
    /// Bucket every cell of `geo` by latitude.
    fn new(geo: &Geosphere) -> LatBandIndex {
        let mut bands = vec![Vec::new(); BAND_COUNT];
        for cell in geo.cells() {
            let latitude = geo.coord(cell).latitude;
            let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
            bands[band].push(cell);
        }
        LatBandIndex { bands }
    }

    /// The cell nearest a coordinate (degrees), by maximum dot product.
    /// Inverts the kernel's coord convention: latitude = asin(z),
    /// longitude = atan2(y, x).
    fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
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

/// Render the globe as an equirectangular binary P6 PPM: longitude −180 →
/// 180 across, latitude 90 → −90 down, pixel centers sampled. Same globe,
/// same bytes.
pub fn elevation_ppm(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = format!("P6\n{width} {height}\n255\n").into_bytes();
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&color(*globe.elevation.get(cell), globe.sea_level));
        }
    }
    out
}

/// Render the globe as a 72×24 ASCII map: '~' ocean, '.' lowland, '+'
/// hills, '^' mountains, 'A' high peaks. One newline per row.
pub fn elevation_ascii(geo: &Geosphere, globe: &TectonicGlobe) -> String {
    let index = LatBandIndex::new(geo);
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
    fn ppm_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let a = elevation_ppm(&geo, &globe);
        let b = elevation_ppm(&geo, &globe);
        assert_eq!(a, b);
        let header = format!("P6\n{} {}\n255\n", MAP_WIDTH, MAP_WIDTH / 2);
        assert!(a.starts_with(header.as_bytes()));
        assert_eq!(
            a.len(),
            header.len() + (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize
        );
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
    fn band_index_agrees_with_brute_force_nearest() {
        let geo = Geosphere::new(4);
        let index = LatBandIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (89.0, 10.0), (-89.0, -170.0), (45.5, 179.5)] {
            let banded = index.nearest(&geo, latitude, longitude);
            let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
            let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let mut best = hornvale_kernel::CellId(0);
            let mut best_dot = f64::NEG_INFINITY;
            for cell in geo.cells() {
                let d = crate::plates::dot(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
            assert_eq!(banded, best, "at ({latitude}, {longitude})");
        }
    }
}
