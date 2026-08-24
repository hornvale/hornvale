//! Deterministic paleoclimate renders: an equirectangular PNG and a 72×24
//! ASCII map of the strata (ice envelope, refugia, fossil shorelines) over the
//! present globe. Same record, same bytes — a changed artifact means changed
//! behavior. Projection uses the kernel's `NearestVertexIndex`, as the biome and
//! elevation renderers do.

use crate::strata::PaleoRecord;
use hornvale_kernel::{Geosphere, NearestVertexIndex, Vertex};

/// Raster width, pixels (equirectangular → height is half).
/// type-audit: bare-ok(render-internal)
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width, characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height, characters.
/// type-audit: bare-ok(render-internal)
pub const ASCII_HEIGHT: u32 = 24;

/// RGB triple for a vertex's stratum (ice → white, refugium → green,
/// shoreline → blue, else → grey), in envelope>refugium>shoreline priority.
fn color(record: &PaleoRecord, vertex: Vertex) -> [u8; 3] {
    if *record.envelope.get(vertex) {
        [235, 235, 245]
    } else if *record.refugia.get(vertex) {
        [70, 160, 80]
    } else if *record.shoreline.get(vertex) {
        [70, 110, 200]
    } else {
        [90, 90, 90]
    }
}

/// ASCII glyph for a vertex's stratum.
fn glyph(record: &PaleoRecord, vertex: Vertex) -> char {
    if *record.envelope.get(vertex) {
        '#'
    } else if *record.refugia.get(vertex) {
        '*'
    } else if *record.shoreline.get(vertex) {
        '~'
    } else {
        '.'
    }
}

/// Raw RGB pixels, row-major, top row first.
/// type-audit: bare-ok(artifact)
pub fn paleo_pixels(geo: &Geosphere, record: &PaleoRecord) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = NearestVertexIndex::new(geo);
    let mut out = Vec::with_capacity((width * height * 3) as usize);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let vertex = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&color(record, vertex));
        }
    }
    out
}

/// Equirectangular PNG of the strata (decision 0018).
/// type-audit: bare-ok(artifact)
pub fn paleo_png(geo: &Geosphere, record: &PaleoRecord) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(MAP_WIDTH, MAP_WIDTH / 2, &paleo_pixels(geo, record))
}

/// 72×24 ASCII strata map, one newline per row.
/// type-audit: bare-ok(artifact)
pub fn paleo_ascii(geo: &Geosphere, record: &PaleoRecord) -> String {
    let index = NearestVertexIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let vertex = index.nearest(geo, latitude, longitude);
            out.push(glyph(record, vertex));
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::{EraClimate, extract};
    use hornvale_kernel::{ReferenceElevation, VertexMap};

    /// Test-only helper: a validated `ReferenceElevation`.
    fn e(m: f64) -> ReferenceElevation {
        ReferenceElevation::new(m).unwrap()
    }

    fn record(geo: &Geosphere) -> PaleoRecord {
        let elev = VertexMap::from_fn(geo, |_| e(100.0));
        let eras = vec![EraClimate {
            day: 0.0,
            ice: VertexMap::from_fn(geo, |c| geo.coord(c).latitude.abs() > 60.0),
            habitable: VertexMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 30.0),
            sea_level: e(-40.0),
            ice_fraction: 0.3,
        }];
        extract(geo, &elev, e(0.0), &eras)
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
