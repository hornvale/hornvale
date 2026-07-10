//! Deterministic paleoclimate renders: an equirectangular PNG and a 72×24
//! ASCII map of the strata (ice envelope, refugia, fossil shorelines) over the
//! present globe. Same record, same bytes — a changed artifact means changed
//! behavior. Projection uses the kernel's `NearestCellIndex`, as the biome and
//! elevation renderers do.

use crate::strata::PaleoRecord;
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex};

/// Raster width, pixels (equirectangular → height is half).
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width, characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height, characters.
pub const ASCII_HEIGHT: u32 = 24;

/// RGB triple for a cell's stratum (ice → white, refugium → green,
/// shoreline → blue, else → grey), in envelope>refugium>shoreline priority.
fn color(record: &PaleoRecord, cell: CellId) -> [u8; 3] {
    if *record.envelope.get(cell) {
        [235, 235, 245]
    } else if *record.refugia.get(cell) {
        [70, 160, 80]
    } else if *record.shoreline.get(cell) {
        [70, 110, 200]
    } else {
        [90, 90, 90]
    }
}

/// ASCII glyph for a cell's stratum.
fn glyph(record: &PaleoRecord, cell: CellId) -> char {
    if *record.envelope.get(cell) {
        '#'
    } else if *record.refugia.get(cell) {
        '*'
    } else if *record.shoreline.get(cell) {
        '~'
    } else {
        '.'
    }
}

/// Raw RGB pixels, row-major, top row first.
pub fn paleo_pixels(geo: &Geosphere, record: &PaleoRecord) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestCellIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&color(record, cell));
        }
    }
    out
}

/// Equirectangular PNG of the strata (decision 0018).
pub fn paleo_png(geo: &Geosphere, record: &PaleoRecord) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &paleo_pixels(geo, record))
}

/// 72×24 ASCII strata map, one newline per row.
pub fn paleo_ascii(geo: &Geosphere, record: &PaleoRecord) -> String {
    let index = NearestCellIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.push(glyph(record, cell));
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::{EraClimate, extract};
    use hornvale_kernel::CellMap;

    fn record(geo: &Geosphere) -> PaleoRecord {
        let elev = CellMap::from_fn(geo, |_| 100.0);
        let eras = vec![EraClimate {
            day: 0.0,
            ice: CellMap::from_fn(geo, |c| geo.coord(c).latitude.abs() > 60.0),
            habitable: CellMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 30.0),
            sea_level: -40.0,
            ice_fraction: 0.3,
        }];
        extract(geo, &elev, 0.0, &eras)
    }

    #[test]
    fn ascii_is_the_right_shape_and_deterministic() {
        let geo = Geosphere::new(4);
        let rec = record(&geo);
        let a = paleo_ascii(&geo, &rec);
        assert_eq!(a.lines().count(), ASCII_HEIGHT as usize);
        assert_eq!(paleo_ascii(&geo, &rec), a);
    }

    #[test]
    fn png_is_nonempty_and_deterministic() {
        let geo = Geosphere::new(4);
        let rec = record(&geo);
        let png = paleo_png(&geo, &rec);
        assert!(!png.is_empty());
        assert_eq!(paleo_png(&geo, &rec), png);
    }
}
