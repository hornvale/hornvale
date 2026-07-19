//! Ocean surface currents: prevailing band winds deflected by the Coriolis
//! effect (Ekman-style, modeled here as a fixed 45° deflection rather than a
//! full spiral) and projected off the coastline so flow along a shore never
//! points into land. Zero on land and zero everywhere in a locked world (no
//! bands to drive it).

use crate::circulation::prevailing_wind;
use hornvale_kernel::{CellId, CellMap, Geosphere, math};

/// Fixed Ekman deflection angle: 45°, applied about the local outward
/// normal, signed by hemisphere (right in the north, left in the south).
/// type-audit: bare-ok(ratio)
const DEFLECT_RAD: f64 = std::f64::consts::FRAC_PI_4;

/// Cross product a × b.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Dot product a · b.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Elementwise sum a + b.
fn add(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

/// Scale a vector by a scalar.
fn scale(v: [f64; 3], s: f64) -> [f64; 3] {
    [v[0] * s, v[1] * s, v[2] * s]
}

/// Normalize a vector; zero stays zero (guards the degenerate/no-neighbor
/// case rather than dividing by zero).
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let len = (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt();
    if len < 1e-9 {
        [0.0, 0.0, 0.0]
    } else {
        scale(v, 1.0 / len)
    }
}

/// Project `w` onto the tangent plane at `up` (remove the radial component).
fn tangent_project(w: [f64; 3], up: [f64; 3]) -> [f64; 3] {
    let radial = dot(w, up);
    [
        w[0] - radial * up[0],
        w[1] - radial * up[1],
        w[2] - radial * up[2],
    ]
}

/// The ocean surface-current vector at a single cell: the prevailing band
/// wind, Ekman-deflected 45° about the local outward normal (right in the
/// north, left in the south), then projected off any land neighbor so the
/// flow never points into shore. Zero over land, and zero wherever the
/// underlying wind is zero (the poles, where east is undefined).
/// type-audit: bare-ok(count: bands), bare-ok(ratio: return)
pub fn ocean_current(
    geo: &Geosphere,
    is_ocean: &dyn Fn(CellId) -> bool,
    cell: CellId,
    bands: u32,
) -> [f64; 3] {
    if !is_ocean(cell) {
        return [0.0, 0.0, 0.0];
    }
    let wind = prevailing_wind(geo, cell, bands);
    if wind == [0.0, 0.0, 0.0] {
        return [0.0, 0.0, 0.0];
    }
    let up = normalize(geo.position(cell));

    // Ekman deflection: rotate `wind` about `up` by a hemisphere-signed
    // angle. Northern hemisphere deflects RIGHT (clockwise viewed from
    // outside = negative rotation about the outward `up`); southern
    // deflects LEFT.
    let hemi = geo.coord(cell).latitude;
    let sign = if hemi >= 0.0 { -1.0 } else { 1.0 };
    let theta = sign * DEFLECT_RAD;
    let deflected = add(
        scale(wind, math::cos(theta)),
        scale(cross(up, wind), math::sin(theta)),
    );

    // Coastline projection: remove the component pointing into land.
    let mut toward_land_sum = [0.0, 0.0, 0.0];
    let pos = geo.position(cell);
    for &n in geo.neighbors(cell) {
        if !is_ocean(n) {
            let npos = geo.position(n);
            let dir = tangent_project([npos[0] - pos[0], npos[1] - pos[1], npos[2] - pos[2]], up);
            toward_land_sum = add(toward_land_sum, dir);
        }
    }
    let toward_land = normalize(toward_land_sum);
    if toward_land == [0.0, 0.0, 0.0] {
        deflected
    } else {
        let into_land = dot(deflected, toward_land).max(0.0);
        add(deflected, scale(toward_land, -into_land))
    }
}

/// The whole-world ocean-current field: [`ocean_current`] evaluated at every
/// cell. All-zero when `bands` is `None` (a tidally locked world has no
/// circulation bands to drive a current).
/// type-audit: bare-ok(count: bands), bare-ok(ratio: return)
pub fn ocean_current_field(
    geo: &Geosphere,
    is_ocean: &dyn Fn(CellId) -> bool,
    bands: Option<u32>,
) -> CellMap<[f64; 3]> {
    match bands {
        None => CellMap::from_fn(geo, |_| [0.0, 0.0, 0.0]),
        Some(bands) => CellMap::from_fn(geo, |cell| ocean_current(geo, is_ocean, cell, bands)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{CellId, Geosphere};

    fn all_ocean(_: CellId) -> bool {
        true
    }
    fn no_ocean(_: CellId) -> bool {
        false
    }

    // Land cells carry no current.
    #[test]
    fn land_cells_are_zero() {
        let geo = Geosphere::new(4);
        for cell in geo.cells() {
            let c = ocean_current(&geo, &no_ocean, cell, 3);
            assert_eq!(c, [0.0, 0.0, 0.0], "land current must be zero");
        }
    }

    // The current is a tangent field (orthogonal to the radial) and nonzero over
    // open ocean away from the poles.
    #[test]
    fn current_is_tangent_and_nonzero_over_ocean() {
        let geo = Geosphere::new(5);
        let mut saw_nonzero = false;
        for cell in geo.cells() {
            let p = geo.position(cell);
            let c = ocean_current(&geo, &all_ocean, cell, 3);
            let radial = c[0] * p[0] + c[1] * p[1] + c[2] * p[2];
            assert!(
                radial.abs() < 1e-9,
                "current must be tangent; radial={radial}"
            );
            if c[0] * c[0] + c[1] * c[1] + c[2] * c[2] > 1e-6 {
                saw_nonzero = true;
            }
        }
        assert!(saw_nonzero, "open ocean should carry a current somewhere");
    }

    // Hemisphere-correct rotation: the current is deflected to the RIGHT of the
    // wind in the N hemisphere (and left in S). "Right" = the deflected vector has
    // a negative component along (up × wind) in the north. This is the
    // gyres-spin-the-right-way guard. (Mutation: flip `sign` → this fails.)
    #[test]
    fn deflection_is_rightward_in_the_north() {
        let geo = Geosphere::new(5);
        // pick an ocean cell at clearly-northern mid-latitude with a nonzero wind
        let cell = geo
            .cells()
            .min_by(|a, b| {
                (geo.coord(*a).latitude - 40.0)
                    .abs()
                    .total_cmp(&(geo.coord(*b).latitude - 40.0).abs())
            })
            .unwrap();
        let wind = crate::prevailing_wind(&geo, cell, 3);
        let up = {
            let p = geo.position(cell);
            let l = (p[0] * p[0] + p[1] * p[1] + p[2] * p[2]).sqrt();
            [p[0] / l, p[1] / l, p[2] / l]
        };
        let left = [
            up[1] * wind[2] - up[2] * wind[1],
            up[2] * wind[0] - up[0] * wind[2],
            up[0] * wind[1] - up[1] * wind[0],
        ]; // up × wind = "left"
        let c = ocean_current(&geo, &all_ocean, cell, 3);
        let along_left = c[0] * left[0] + c[1] * left[1] + c[2] * left[2];
        assert!(
            along_left < 0.0,
            "N-hemisphere current must deflect RIGHT (negative along up×wind); got {along_left}"
        );
    }

    // A whole-locked world (no bands) has an empty field.
    #[test]
    fn locked_field_is_all_zero() {
        let geo = Geosphere::new(4);
        let field = ocean_current_field(&geo, &all_ocean, None);
        for cell in geo.cells() {
            assert_eq!(*field.get(cell), [0.0, 0.0, 0.0]);
        }
    }
}
