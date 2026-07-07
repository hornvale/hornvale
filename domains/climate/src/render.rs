//! Deterministic biome-map renders: an equirectangular P6 PPM and a 72×24
//! ASCII map, recolored from the elevation-map tradition. Same biome field,
//! same bytes — a changed artifact in review means changed behavior.
//! Pixel→cell lookup uses a latitude-band index (30 bands of 6°), the same
//! projection the elevation renderer uses.

use crate::biome::Biome;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// PPM image width in pixels; equirectangular, so height is `MAP_WIDTH / 2`.
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters.
pub const ASCII_HEIGHT: u32 = 24;

/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

/// Dot product.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Latitude-band index for pixel→cell lookups (30 bands of 6°, built in
/// ascending cell order for determinism). Searching the query's band ± 1
/// always contains the nearest cell center at level ≥ 4.
struct LatBandIndex {
    bands: Vec<Vec<CellId>>,
}

impl LatBandIndex {
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

/// Render the biome field as an equirectangular binary P6 PPM. Same field,
/// same bytes.
pub fn biome_ppm(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = format!("P6\n{width} {height}\n255\n").into_bytes();
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&biomes.get(cell).color());
        }
    }
    out
}

/// Render the biome field as a 72×24 ASCII map, one newline per row.
pub fn biome_ascii(geo: &Geosphere, biomes: &CellMap<Biome>) -> String {
    let index = LatBandIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.push(biomes.get(cell).glyph());
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::biome::Biome;
    use hornvale_kernel::Geosphere;

    fn checker(geo: &Geosphere) -> CellMap<Biome> {
        CellMap::from_fn(geo, |c| {
            if geo.position(c)[2] > 0.0 {
                Biome::TropicalRainforest
            } else {
                Biome::Abyssal
            }
        })
    }

    #[test]
    fn ppm_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let a = biome_ppm(&geo, &biomes);
        let b = biome_ppm(&geo, &biomes);
        assert_eq!(a, b);
        let header = format!("P6\n{} {}\n255\n", MAP_WIDTH, MAP_WIDTH / 2);
        assert!(a.starts_with(header.as_bytes()));
        assert_eq!(
            a.len(),
            header.len() + (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize
        );
    }

    #[test]
    fn ascii_map_has_right_shape_and_shows_land_and_sea() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let map = biome_ascii(&geo, &biomes);
        assert_eq!(map.lines().count(), ASCII_HEIGHT as usize);
        for line in map.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH as usize);
        }
        assert!(map.contains(Biome::TropicalRainforest.glyph()));
        assert_eq!(biome_ascii(&geo, &biomes), map);
    }
}
