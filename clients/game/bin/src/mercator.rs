//! Oblique Mercator projection between geographic coordinates and the
//! terminal map plate.
//!
//! Ported from `windows/worldgen/examples/portolan_spike.rs` (deleted at
//! this campaign's close; see git history at `0292de87f^` for the file
//! itself -- decision 0022 puts rendering in the client, not the sim, so
//! this lives in `bin` rather than a domain crate). The spike's own math
//! is kept byte-for-byte identical -- it routes through
//! `hornvale_kernel::math`'s libm-backed transcendentals, which is what
//! keeps the projection cross-platform deterministic -- but its CLAMP
//! POLICY is not: the spike always returns a cell (right for placing a
//! label), while this module
//! returns `None` above the clamp (right for drawing terrain, since spec
//! §6 refuses polar fabrication).

/// Mercator's standard pole clamp: the projection diverges at ±90°, so
/// latitude is clamped to ±`LAT_CLAMP_DEG` before projecting. A point past
/// the clamp is not drawn (see the module doc for why this differs from
/// the spike).
pub const LAT_CLAMP_DEG: f64 = 85.0;

/// Design spec §3.3's caption: states the clamp and names the central line
/// it is measured from — a stated refusal, not decoration. "±85°" alone
/// tells a locked-world reader nothing useful: on a locked world the
/// projection's own poles sit at the substellar and antistellar points
/// ([`frame_for`]'s own doc), so what the clamp discards there is the
/// substellar desert and the antistellar ice, not the geographic poles at
/// all — a different pair of places than "±85°" alone would suggest.
///
/// **Derived from `f` itself, never from a `locked: bool` threaded a
/// second time past [`frame_for`]** — the frame already knows which line
/// it holds: a spinning world's frame puts its pole at the geographic pole
/// (`pole_lat_deg == 90.0`, `pole_lon_deg == 0.0`, exactly, by construction
/// in `frame_for`); a locked world's puts it at the substellar point
/// (`pole_lat_deg == 0.0`, `pole_lon_deg == 0.0`).
///
/// **A THIRD case, found on review: [`centre_on`] (§3.2's re-centre
/// command, a shipped gesture — `.` on the map) rolls the projection to
/// wherever the cursor points, which is neither of those two poles in
/// general.** A first version of this function fell through to the locked
/// arm for anything that was not exactly the spinning pole — which meant
/// one ordinary `.` press on a SPINNING world, off the equator, made the
/// strip state "clamped ... from the terminator — the substellar desert
/// and antistellar ice are off the map" on a world with no terminator, no
/// substellar desert and no antistellar ice: a confidently wrong claim on
/// the one surface §3.3 exists to keep honest. There is no third named
/// line to fall back on — a re-centred frame's own central line is an
/// oblique great circle through wherever the player last centred, and it
/// is neither "the equator" nor "the terminator" in either rotation
/// regime — so the fix is a third, generic wording that names NEITHER,
/// rather than guessing which of the two is closer.
///
/// Exact float equality against `frame_for`'s own two literals is safe
/// here: both are constructed from bare constants with no transcendental
/// computation, so a `Frame` that did not come from one of them (including
/// one `centre_on` produced) essentially never collides with either by
/// accident — and if a re-centre coincidentally lands exactly back on a
/// pole, that IS the same physical central line, so treating it as such is
/// correct, not merely convenient.
///
/// **The one collision this doc used to leave undefended (final review):
/// a SPINNING world producing the LOCKED caption is structurally
/// impossible, not merely unobserved.** For that to happen `centre_on`
/// would need to return a `Frame` exactly equal to `frame_for(true)`
/// (`pole_lat_deg == 0.0`) on a spinning world. `centre_on`'s own formula
/// is `pole_lat_deg = 90.0 - lat_deg.abs()`, so that requires
/// `lat_deg.abs() == 90.0` exactly — but every `lat_deg` `centre_on` is
/// ever called with comes from `unproject`, whose frame latitude is bounded
/// to `±LAT_CLAMP_DEG` (85°) by construction on a spinning world (`to_frame`
/// takes the identity branch there, per the invariant above `to_frame`
/// itself now asserts). So `pole_lat_deg >= 90.0 - 85.0 == 5.0` always, on
/// a spinning world, and the locked-caption collision cannot occur. The
/// converse (a locked world producing the SPINNING caption) is bounded the
/// same way and is the one this doc already defended above.
pub fn clamp_caption(f: &Frame) -> String {
    if *f == frame_for(false) {
        format!(
            "clamped at ±{LAT_CLAMP_DEG:.0}° from the equator — the polar ice caps are off \
             the map"
        )
    } else if *f == frame_for(true) {
        format!(
            "clamped at ±{LAT_CLAMP_DEG:.0}° from the terminator — the substellar desert and \
             antistellar ice are off the map"
        )
    } else {
        format!(
            "clamped at ±{LAT_CLAMP_DEG:.0}° from the re-centred line — what falls outside \
             the map depends on where you last centred it"
        )
    }
}

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
        // Identity: the frame's pole IS a geographic pole. This branch is
        // reachable only with `pole_lat_deg == +90.0` (`frame_for(false)`)
        // -- `frame_for` never yields `-90.0` and `centre_on` always
        // yields `90.0 - |lat_deg| >= 0.0` -- so a `-90.0` pole, and the
        // reflected (`sign == -1.0`) case a prior version of this branch
        // handled, is unreachable by construction. A version of this
        // function that special-cased it accordingly (`sign *
        // lat_deg`) was a REFLECTION for that case, not a rotation, and
        // there is no test that could distinguish the two without first
        // reaching a pole this crate never produces. Asserted, not
        // silently trusted:
        debug_assert!(
            f.pole_lat_deg >= 0.0,
            "Frame::pole_lat_deg is never negative in any frame this crate constructs \
             (frame_for, centre_on) -- if this fires, to_frame's identity branch needs \
             its sign handling back, correctly, not merely restored"
        );
        (lat_deg, wrap_deg_signed(lon_deg - f.pole_lon_deg))
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
        // See `to_frame`'s identical invariant and its own comment: this
        // branch is reachable only at `pole_lat_deg == +90.0`.
        debug_assert!(f.pole_lat_deg >= 0.0);
        (
            frame_lat_deg,
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

/// A new [`Frame`] whose central line (the projection's own equator, the
/// undistorted great circle) passes through the geographic point
/// `(lat_deg, lon_deg)` — the map's re-centring gesture (spec §3.2's
/// explicit "roll the projection" command; `bin`'s `.` key in the map
/// focus).
///
/// **Derivation.** A frame's pole must sit 90° (angular distance) from
/// every point on its own equator, so recentring on `P` means choosing a
/// pole 90° from `P` — a whole great circle of valid choices (the "roll"
/// degree of freedom a true oblique Mercator carries). This picks the one
/// reached by moving 90° due NORTH from `P` along `P`'s own meridian —
/// parameter-free, no extra roll angle to invent.
///
/// In Cartesian terms, with `v` = `P`'s unit vector
/// (`cos(lat)cos(lon), cos(lat)sin(lon), sin(lat)`) and `n` = the
/// geographic north pole `(0, 0, 1)`, the new pole is `n` projected
/// orthogonal to `v` and renormalized: `n - (n·v)v`. Since `n·v =
/// sin(lat_deg)`, this reduces to a closed form with no explicit
/// pole-crossing case-split: `new_pole_lat = 90° - |lat_deg|`, and
/// `new_pole_lon = lon_deg + 180°` when `lat_deg > 0` (the meridian path
/// crossed the north pole on the way there) or `lon_deg` unchanged
/// otherwise. At `lat_deg == 0` both branches agree — the new pole IS the
/// geographic north pole, where longitude is moot. `mercator::tests`
/// checks the closed form against the general definition (`P` lands on
/// the new frame's equator) rather than trusting the algebra alone.
pub fn centre_on(lat_deg: f64, lon_deg: f64) -> Frame {
    let pole_lat_deg = 90.0 - lat_deg.abs();
    let pole_lon_deg = if lat_deg > 0.0 {
        wrap_deg_signed(lon_deg + 180.0)
    } else {
        lon_deg
    };
    Frame {
        pole_lat_deg,
        pole_lon_deg,
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

    // -- Task 4, Step 3 / §3.3: the clamp caption, both rotation regimes --

    /// A spinning-world fixture cannot see the locked case, and the locked
    /// case is the entire reason §3.3 exists (the task brief's own words)
    /// — so this pins BOTH regimes rather than one.
    #[test]
    fn the_clamp_caption_names_the_equator_on_a_spinning_world() {
        let f = frame_for(false);
        let text = clamp_caption(&f);
        assert!(text.contains("equator"), "got {text:?}");
        assert!(!text.contains("terminator"), "got {text:?}");
        assert!(
            text.contains("±85°") || text.contains("85"),
            "the clamp's own number must appear, got {text:?}"
        );
    }

    /// The locked arm: the central line is the terminator, not the
    /// equator, and what the clamp discards is the substellar desert and
    /// the antistellar ice — not the polar ice caps, which is what a
    /// reader would wrongly assume from "±85°" alone.
    #[test]
    fn the_clamp_caption_names_the_terminator_on_a_locked_world() {
        let f = frame_for(true);
        let text = clamp_caption(&f);
        assert!(text.contains("terminator"), "got {text:?}");
        assert!(!text.contains("equator"), "got {text:?}");
        assert!(text.contains("desert"), "got {text:?}");
        assert!(text.contains("ice"), "got {text:?}");
    }

    /// **Fix round 1 (reviewer finding 1): a RE-CENTRED frame must never
    /// be mistaken for the locked regime's own terminator wording.**
    /// `centre_on` (§3.2's `.` gesture) rolls the projection to an
    /// arbitrary point, starting from the SPINNING default -- this
    /// reproduces the reviewer's own repro at the pure-function level
    /// (the end-to-end version lives in `driver.rs`'s
    /// `recentring_a_spinning_world_off_the_equator_never_claims_the_
    /// terminator`): the recentred frame's own pole is neither the
    /// geographic pole nor the substellar point, so the caption must name
    /// NEITHER "equator" nor "terminator" -- guessing the nearer of the
    /// two would still be a false claim.
    #[test]
    fn a_recentred_spinning_frame_claims_neither_the_equator_nor_the_terminator() {
        let recentred = centre_on(30.0, 40.0); // off both the equator and either pole
        assert_ne!(
            recentred,
            frame_for(false),
            "sanity: this must not collide with the default"
        );
        assert_ne!(
            recentred,
            frame_for(true),
            "sanity: this must not collide with the default"
        );
        let text = clamp_caption(&recentred);
        assert!(
            !text.contains("terminator"),
            "a recentred SPINNING world has no terminator, got {text:?}"
        );
        assert!(
            !text.contains("equator"),
            "the recentred line is not the equator either, got {text:?}"
        );
        assert!(
            !text.contains("desert")
                && !text.contains("substellar")
                && !text.contains("antistellar"),
            "no locked-world claim may leak into a spinning world's caption, got {text:?}"
        );
    }

    /// The symmetric case starting from the LOCKED default: recentring
    /// away from the substellar point must not keep claiming "terminator"
    /// either -- the fix must generalise to both regimes, not just repair
    /// the spinning one the reviewer happened to reproduce.
    #[test]
    fn a_recentred_locked_frame_claims_neither_the_equator_nor_the_terminator() {
        let recentred = centre_on(-15.0, 100.0);
        assert_ne!(recentred, frame_for(false), "sanity");
        assert_ne!(recentred, frame_for(true), "sanity");
        let text = clamp_caption(&recentred);
        assert!(!text.contains("terminator"), "got {text:?}");
        assert!(!text.contains("equator"), "got {text:?}");
    }

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

    /// `centre_on`'s general definition, not the closed-form algebra:
    /// whatever point it is asked to centre lands on the NEW frame's own
    /// equator (`to_frame`'s frame-latitude is ~0), for a spread of points
    /// including the ones the closed form case-splits on (`lat == 0`, the
    /// crossing threshold `lat > 0` vs. `lat <= 0`, and the poles
    /// themselves).
    #[test]
    fn centre_on_puts_the_point_on_the_new_frames_equator() {
        for &(lat, lon) in &[
            (0.0, 0.0),
            (0.0, 137.0),
            (30.0, -60.0),
            (-30.0, -60.0),
            (84.9, 12.0),
            (-84.9, 12.0),
            (45.0, 179.9),
            (-45.0, -179.9),
        ] {
            let f = centre_on(lat, lon);
            let (frame_lat, _) = to_frame(&f, lat, lon);
            assert!(
                frame_lat.abs() < 1e-6,
                "centring on ({lat}, {lon}) left it at frame latitude {frame_lat}, not on \
                 the equator"
            );
        }
    }

    /// A no-op re-centre (already on the geographic equator, at the prime
    /// meridian) must not silently do nothing OR blow up — the pole ends
    /// up at the geographic north pole either way (`lat == 0` is the
    /// closed form's own boundary case), and the point must still land on
    /// the new equator.
    #[test]
    fn centre_on_the_equator_itself_is_well_defined() {
        let f = centre_on(0.0, 0.0);
        assert!((f.pole_lat_deg - 90.0).abs() < 1e-9);
        let (frame_lat, _) = to_frame(&f, 0.0, 0.0);
        assert!(frame_lat.abs() < 1e-6);
    }
}
