//! Deterministic biome-map renders: an equirectangular PNG and a 72×24
//! ASCII map, recolored from the elevation-map tradition. Same biome field,
//! same bytes — a changed artifact in review means changed behavior.
//! Pixel→cell lookup uses the kernel's `NearestCellIndex` (a latitude-band
//! index, 30 bands of 6°), the same projection the elevation renderer uses.

use crate::biome::Biome;
use hornvale_kernel::{CellMap, Geosphere, NearestCellIndex};

/// Raster image width in pixels; equirectangular, so height is `MAP_WIDTH / 2`.
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_HEIGHT: u32 = 24;

/// Raw RGB pixels of the equirectangular biome map (row-major, top row
/// first) — also the base image settlement's overlay stamps at the
/// composition root.
/// type-audit: bare-ok(artifact)
pub fn biome_pixels(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestCellIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
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

/// Render the biome field as an equirectangular PNG (decision 0018). Same
/// field, same bytes.
/// type-audit: bare-ok(artifact)
pub fn biome_png(geo: &Geosphere, biomes: &CellMap<Biome>) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &biome_pixels(geo, biomes))
}

/// Render the biome field as a 72×24 ASCII map, one newline per row.
/// type-audit: bare-ok(artifact)
pub fn biome_ascii(geo: &Geosphere, biomes: &CellMap<Biome>) -> String {
    let index = NearestCellIndex::new(geo);
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
    fn png_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let a = biome_png(&geo, &biomes);
        assert_eq!(a, biome_png(&geo, &biomes));
        assert!(a.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&a[16..20], &MAP_WIDTH.to_be_bytes());
        assert_eq!(&a[20..24], &(MAP_WIDTH / 2).to_be_bytes());
    }

    #[test]
    fn pixel_buffer_is_exactly_the_raster() {
        let geo = Geosphere::new(4);
        let biomes = checker(&geo);
        let pixels = biome_pixels(&geo, &biomes);
        assert_eq!(pixels.len(), (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize);
        assert_eq!(pixels, biome_pixels(&geo, &biomes));
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
