//! Deterministic debug render of a carrying-capacity field: a fixed-size
//! ASCII density map. Follows `paleoclimate::render`'s pattern exactly (same
//! `NearestCellIndex` projection, same fixed-grid/one-glyph-per-cell shape,
//! same `String` return, one trailing newline per row) — bucketing K into a
//! discrete glyph ramp is the render-time analogue of that module's discrete
//! stratum glyphs, so no float ever crosses into the emitted text and there
//! is nothing to quantize.

use hornvale_kernel::{CellMap, Geosphere, NearestCellIndex};

/// ASCII map width, characters.
/// type-audit: bare-ok(render-internal)
pub const DENSITY_WIDTH: u32 = 72;
/// ASCII map height, characters.
/// type-audit: bare-ok(render-internal)
pub const DENSITY_HEIGHT: u32 = 24;

/// Glyph ramp, sparsest to densest.
const RAMP: [char; 5] = [' ', '.', ':', '*', '#'];

/// The densest K value in the field (0.0 for an empty/all-zero field).
fn max_k(geo: &Geosphere, k: &CellMap<f64>) -> f64 {
    geo.cells()
        .map(|c| *k.get(c))
        .fold(0.0_f64, |acc, v| acc.max(v))
}

/// Bucket a cell's K into a glyph on the ramp, scaled against the field's
/// densest cell (`peak`). A non-positive peak (an empty field) renders as
/// all-blank.
fn glyph(k: f64, peak: f64) -> char {
    if peak <= 0.0 {
        return RAMP[0];
    }
    let frac = (k / peak).clamp(0.0, 1.0);
    let last = RAMP.len() - 1;
    let idx = (frac * last as f64).round() as usize;
    RAMP[idx.min(last)]
}

/// 72×24 ASCII density map of a carrying-capacity field, one newline per row,
/// darkest glyph at the field's densest cell. See module docs for the render
/// pattern this matches.
/// type-audit: bare-ok(artifact)
pub fn density_ppm(geo: &Geosphere, k: &CellMap<f64>) -> String {
    let index = NearestCellIndex::new(geo);
    let peak = max_k(geo, k);
    let mut out = String::with_capacity(((DENSITY_WIDTH + 1) * DENSITY_HEIGHT) as usize);
    for py in 0..DENSITY_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(DENSITY_HEIGHT) * 180.0;
        for px in 0..DENSITY_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(DENSITY_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.push(glyph(*k.get(cell), peak));
        }
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellId, Geosphere};

    fn bump_k(geo: &Geosphere) -> CellMap<f64> {
        let peak = geo.position(CellId(0));
        CellMap::from_fn(geo, |c| {
            let p = geo.position(c);
            (p[0] * peak[0] + p[1] * peak[1] + p[2] * peak[2]).max(0.0)
        })
    }

    #[test]
    fn density_map_is_the_right_shape_and_deterministic() {
        let geo = Geosphere::new(4);
        let k = bump_k(&geo);
        let a = density_ppm(&geo, &k);
        assert_eq!(a.lines().count(), DENSITY_HEIGHT as usize);
        assert!(
            a.lines()
                .all(|l| l.chars().count() == DENSITY_WIDTH as usize)
        );
        assert_eq!(density_ppm(&geo, &k), a, "same field, same bytes");
    }

    #[test]
    fn an_all_zero_field_renders_as_all_blank() {
        let geo = Geosphere::new(3);
        let k = CellMap::from_fn(&geo, |_| 0.0);
        let a = density_ppm(&geo, &k);
        assert!(a.chars().all(|c| c == ' ' || c == '\n'));
    }

    #[test]
    fn the_peak_cell_renders_as_the_densest_glyph() {
        let geo = Geosphere::new(4);
        let k = bump_k(&geo);
        let a = density_ppm(&geo, &k);
        assert!(a.contains(*RAMP.last().unwrap()), "densest glyph appears");
    }
}
