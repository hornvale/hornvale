//! Deterministic map renders in the First Light tradition: an
//! equirectangular PNG elevation map (decision 0018) and an ASCII map for
//! the REPL. Same globe, same bytes — a changed artifact in review means
//! changed behavior. Pixel→cell lookup uses the kernel's
//! `NearestCellIndex` (a latitude-band index, 30 bands of 6°): the nearest
//! cell center at level ≥ 4 is within ~2.5°, so the pixel's band plus both
//! neighbors always contains it.

use crate::globe::TectonicGlobe;
use hornvale_kernel::{Geosphere, NearestCellIndex};

/// Raster image width in pixels; the image is equirectangular, so height is
/// `MAP_WIDTH / 2`.
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters (2:1 world on ~2:1-tall glyphs).
pub const ASCII_HEIGHT: u32 = 24;

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
/// centers sampled.
fn elevation_pixels(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestCellIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
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

/// Render the globe as an equirectangular PNG (decision 0018). Same globe,
/// same bytes.
pub fn elevation_png(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &elevation_pixels(geo, globe))
}

/// Render the globe as a 72×24 ASCII map: '~' ocean, '.' lowland, '+'
/// hills, '^' mountains, 'A' high peaks. One newline per row.
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
        let a = elevation_png(&geo, &globe);
        assert_eq!(a, elevation_png(&geo, &globe));
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
}
