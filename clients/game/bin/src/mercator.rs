//! Oblique Mercator projection between geographic coordinates and the
//! terminal map plate.
//!
//! Ported from `windows/worldgen/examples/portolan_spike.rs` (decision
//! 0022 puts rendering in the client, not the sim, so this lives in
//! `bin` rather than a domain crate). The spike's own math is kept
//! byte-for-byte identical -- it routes through `hornvale_kernel::math`'s
//! libm-backed transcendentals, which is what keeps the projection
//! cross-platform deterministic -- but its CLAMP POLICY is not: the spike
//! always returns a cell (right for placing a label), while this module
//! returns `None` above the clamp (right for drawing terrain, since spec
//! §6 refuses polar fabrication).

/// Mercator's standard pole clamp: the projection diverges at ±90°, so
/// latitude is clamped to ±`LAT_CLAMP_DEG` before projecting. A point past
/// the clamp is not drawn (see the module doc for why this differs from
/// the spike).
pub const LAT_CLAMP_DEG: f64 = 85.0;

/// The rotation taking world coordinates into projection coordinates,
/// expressed as the geographic position of the PROJECTION's north pole.
///
/// A great circle's pole is 90° from every point on it, so choosing which
/// line the projection holds true is the same act as choosing this pole.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Frame {
    /// Geographic latitude of the projection's pole, in degrees.
    pub pole_lat_deg: f64,
    /// Geographic longitude of the projection's pole, in degrees.
    pub pole_lon_deg: f64,
}

/// The frame a world's own physics chooses (spec §3.1).
///
/// A spinning world's rotation axis defines its geographic poles, and the
/// projection holds the geographic equator true, so the projection's pole
/// IS the geographic pole: `Frame { pole_lat_deg: 90.0, pole_lon_deg: 0.0 }`.
///
/// A locked world's habitable band is the terminator, the great circle 90°
/// from the substellar point -- so the terminator's pole IS the substellar
/// point, which `kernel/src/geosphere.rs` puts at (lat 0, lon 0):
/// `latitude = asin(z)`, `longitude = atan2(y, x)`, so `+x` is exactly
/// (0, 0), matching the convention `domains/astronomy` states twice.
pub fn frame_for(locked: bool) -> Frame {
    if locked {
        Frame {
            pole_lat_deg: 0.0,
            pole_lon_deg: 0.0,
        }
    } else {
        Frame {
            pole_lat_deg: 90.0,
            pole_lon_deg: 0.0,
        }
    }
}

/// Mercator y (unnormalized) at a clamped latitude, degrees. The internal
/// clamp is what makes the projection finite; it is not the same clamp as
/// `project`'s out-of-band `None` (see the module doc).
fn mercator_y(lat_deg: f64) -> f64 {
    let lat = lat_deg.clamp(-LAT_CLAMP_DEG, LAT_CLAMP_DEG);
    let lat_rad = lat.to_radians();
    hornvale_kernel::math::ln(hornvale_kernel::math::tan(
        std::f64::consts::FRAC_PI_4 + lat_rad / 2.0,
    ))
}

/// `mercator_y` at the clamp latitude -- the half-height of the projected
/// range, used to normalize.
fn mercator_y_max() -> f64 {
    mercator_y(LAT_CLAMP_DEG)
}

/// Rotate a geographic (lat, lon), in degrees, into the frame whose pole
/// sits at `(f.pole_lat_deg, f.pole_lon_deg)`.
///
/// **F1b, answered by deriving the rotation, not by assuming it:** the
/// two poles this module actually uses do NOT behave the same way. Every
/// vector component of a geographic point is `x = cos(lat)cos(lon)`,
/// `y = cos(lat)sin(lon)`, `z = sin(lat)` -- always a PRODUCT of two
/// trig terms, except `z`, which is a clean function of `lat` alone. A
/// rotation's frame-latitude is `asin` of the frame's new `z`-component,
/// so that component comes out trig-free **only when the frame's pole is
/// the geographic pole itself** (`pole_lat_deg == ±90`): then the new `z`
/// literally IS `sin(lat)`, so `asin(sin(lat)) == lat` and no transcendental
/// call is needed at all -- a true coordinate permutation. That is the
/// spinning-world frame, handled below as the identity.
///
/// **The locked-world frame's pole sits on the geographic equator**
/// (`pole_lat_deg == 0`), so its new `z`-component is
/// `cos(lat)cos(lon-pole_lon)` -- a product, never reducible to plain
/// lat/lon arithmetic. That case genuinely needs `asin`/`atan2`, so it
/// routes through `hornvale_kernel::math`. (An earlier draft of this
/// function tried to fake a trig-free permutation for this case by
/// swapping lat and lon directly; it satisfied the two seam tests below
/// -- which only ever sample the pole point and the lon=90 meridian --
/// but was not a valid rotation: the geographic north pole, a single
/// point, mapped to a different frame position depending on which
/// arbitrary longitude was used to name it. The round-trip property test
/// (Step 9) is what would have caught this on a general point; the
/// general spherical-rotation formula below is exact everywhere.)
fn to_frame(f: &Frame, lat_deg: f64, lon_deg: f64) -> (f64, f64) {
    if f.pole_lat_deg.abs() >= 90.0 - f64::EPSILON {
        // Identity (up to sign): the frame's pole IS a geographic pole.
        let sign = f.pole_lat_deg.signum();
        (sign * lat_deg, wrap_deg_signed(lon_deg - f.pole_lon_deg))
    } else {
        let lat = lat_deg.to_radians();
        let lon = lon_deg.to_radians();
        let pole_lat = f.pole_lat_deg.to_radians();
        let pole_lon = f.pole_lon_deg.to_radians();
        let d_lon = lon - pole_lon;

        let sin_lat = hornvale_kernel::math::sin(lat);
        let cos_lat = hornvale_kernel::math::cos(lat);
        let sin_pole = hornvale_kernel::math::sin(pole_lat);
        let cos_pole = hornvale_kernel::math::cos(pole_lat);
        let cos_dlon = hornvale_kernel::math::cos(d_lon);
        let sin_dlon = hornvale_kernel::math::sin(d_lon);

        let fz = (sin_lat * sin_pole + cos_lat * cos_pole * cos_dlon).clamp(-1.0, 1.0);
        let fy = cos_lat * sin_dlon;
        let fx = cos_lat * sin_pole * cos_dlon - sin_lat * cos_pole;

        let frame_lat = hornvale_kernel::math::asin(fz).to_degrees();
        let frame_lon = hornvale_kernel::math::atan2(fy, fx).to_degrees();
        (frame_lat, frame_lon)
    }
}

/// Inverse of `to_frame`: rotate a frame (lat, lon), in degrees, back to
/// geographic coordinates. Exact inverse of the rotation above (the
/// transpose of its rotation matrix, since a rotation matrix is
/// orthogonal).
fn from_frame(f: &Frame, frame_lat_deg: f64, frame_lon_deg: f64) -> (f64, f64) {
    if f.pole_lat_deg.abs() >= 90.0 - f64::EPSILON {
        let sign = f.pole_lat_deg.signum();
        (
            sign * frame_lat_deg,
            wrap_deg_signed(frame_lon_deg + f.pole_lon_deg),
        )
    } else {
        let flat = frame_lat_deg.to_radians();
        let flon = frame_lon_deg.to_radians();
        let pole_lat = f.pole_lat_deg.to_radians();
        let pole_lon = f.pole_lon_deg.to_radians();

        let sin_flat = hornvale_kernel::math::sin(flat);
        let cos_flat = hornvale_kernel::math::cos(flat);
        let sin_flon = hornvale_kernel::math::sin(flon);
        let cos_flon = hornvale_kernel::math::cos(flon);
        let sin_pole = hornvale_kernel::math::sin(pole_lat);
        let cos_pole = hornvale_kernel::math::cos(pole_lat);
        let sin_polelon = hornvale_kernel::math::sin(pole_lon);
        let cos_polelon = hornvale_kernel::math::cos(pole_lon);

        let z = (sin_flat * sin_pole - cos_flat * cos_flon * cos_pole).clamp(-1.0, 1.0);
        let common = cos_flat * cos_flon * sin_pole + sin_flat * cos_pole;
        let x = common * cos_polelon - cos_flat * sin_flon * sin_polelon;
        let y = common * sin_polelon + cos_flat * sin_flon * cos_polelon;

        let lat_deg = hornvale_kernel::math::asin(z).to_degrees();
        let lon_deg = hornvale_kernel::math::atan2(y, x).to_degrees();
        (lat_deg, lon_deg)
    }
}

/// Wrap a longitude difference (degrees) into `(-180, 180]`.
fn wrap_deg_signed(deg: f64) -> f64 {
    let wrapped = (deg + 180.0).rem_euclid(360.0) - 180.0;
    if wrapped <= -180.0 {
        wrapped + 360.0
    } else {
        wrapped
    }
}

/// Forward-project a geographic (lat, lon), in degrees, to a `(row, col)`
/// cell on a `w`x`h` Mercator grid under `f`'s rotation. Longitude wraps;
/// a point whose FRAME latitude is past `LAT_CLAMP_DEG` returns `None`
/// rather than being fabricated at the plate's edge (spec §6).
pub fn project(f: &Frame, lat_deg: f64, lon_deg: f64, w: u32, h: u32) -> Option<(u32, u32)> {
    let (frame_lat, frame_lon) = to_frame(f, lat_deg, lon_deg);
    if frame_lat.abs() > LAT_CLAMP_DEG {
        return None;
    }

    // NOTE: the spike's own `project` used `lon_deg.rem_euclid(360.0)`
    // here (a [0, 360) branch cut), while its `unproject` used a
    // `(col+0.5)/w*360 - 180` cell-center formula (a [-180, 180) branch
    // cut). The spike never round-trips a coordinate through both
    // functions, so the mismatch is invisible there; it produces a
    // systematic ~180° error here, caught by Step 9's round-trip test.
    // Fixed by using the same [-180, 180) branch cut on both sides.
    let lon = (frame_lon + 180.0).rem_euclid(360.0);
    let col = ((lon / 360.0) * f64::from(w)).floor() as i64;
    let col = col.rem_euclid(i64::from(w)) as u32;

    let y = mercator_y(frame_lat);
    let y_max = mercator_y_max();
    let row = ((y_max - y) / (2.0 * y_max) * f64::from(h)).floor();
    let row = row.clamp(0.0, f64::from(h) - 1.0) as u32;
    Some((row, col))
}

/// Inverse-project a `(row, col)` grid cell, on a `w`x`h` Mercator grid
/// under `f`'s rotation, back to a geographic (lat, lon) sample point.
/// Used to paint the terrain backdrop, never for feature placement (which
/// uses the feature's own anchor coordinate).
pub fn unproject(f: &Frame, row: u32, col: u32, w: u32, h: u32) -> (f64, f64) {
    let lon = (f64::from(col) + 0.5) / f64::from(w) * 360.0 - 180.0;
    let y_max = mercator_y_max();
    let y = y_max - (f64::from(row) + 0.5) / f64::from(h) * (2.0 * y_max);
    let lat_rad = 2.0 * hornvale_kernel::math::atan(hornvale_kernel::math::exp(y))
        - std::f64::consts::FRAC_PI_2;
    let frame_lat = lat_rad.to_degrees();
    from_frame(f, frame_lat, lon)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_spinning_world_holds_the_geographic_equator() {
        let f = frame_for(false);
        // The projection's pole IS the geographic pole, so the equator is
        // the line held true: lat 0 lands on the vertical centre of the
        // plate. `project` returns (ROW, COL) -- the spike's own order;
        // see the module doc.
        let (row, _col) = project(&f, 0.0, 0.0, 80, 40).expect("the equator is inside the clamp");
        assert_eq!(row, 20, "lat 0 sits on the plate's horizontal midline");
    }

    #[test]
    fn a_locked_world_holds_the_terminator_and_clamps_the_substellar_point() {
        let f = frame_for(true);
        // The terminator runs pole-to-pole through longitudes ±90°. Every
        // point on it must be INSIDE the drawn area.
        for lat in [-80.0, -40.0, 0.0, 40.0, 80.0] {
            assert!(
                project(&f, lat, 90.0, 80, 40).is_some(),
                "the terminator at lat {lat} must be drawn, not clamped"
            );
        }
        // And the substellar point -- where nobody lives -- is what the
        // clamp eats.
        assert!(
            project(&f, 0.0, 0.0, 80, 40).is_none(),
            "the substellar point is the projection's pole and falls in the clamp"
        );
    }

    #[test]
    fn unproject_inverts_project_in_both_frames() {
        for locked in [false, true] {
            let f = frame_for(locked);
            for &(lat, lon) in &[(0.0, 0.0), (31.5, -117.25), (-64.0, 88.0), (12.0, 179.0)] {
                if let Some((row, col)) = project(&f, lat, lon, 360, 180) {
                    let (rlat, rlon) = unproject(&f, row, col, 360, 180);
                    // One cell of tolerance: project quantizes to a character.
                    assert!(
                        (rlat - lat).abs() < 2.0,
                        "lat {lat} -> {rlat} (locked={locked})"
                    );
                    assert!(
                        ((rlon - lon + 540.0) % 360.0 - 180.0).abs() < 2.0,
                        "lon {lon} -> {rlon}"
                    );
                }
            }
        }
    }
}
