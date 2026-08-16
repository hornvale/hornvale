//! Rhumb-course geometry: the pure half of compass navigation.
//!
//! Nothing here takes a `World`, a `Session` or a `Ledger`. That is what
//! lets this module's own unit tests assert on a walk without paying for a
//! genesis, and it is why this is a module rather than three private
//! functions inside `session.rs`. Wiring these functions into the `go` verb
//! is a later task's job, not this module's.

use hornvale_kernel::{GeoCoord, RoomAddr, math};
use hornvale_locale::Compass;
use std::f64::consts::{FRAC_PI_2, FRAC_PI_4};

/// How close to a pole a course may come, in radians. A rhumb at any
/// bearing off the cardinal axes approaches a pole asymptotically, winding
/// infinitely in finite distance; the clamp stops the reckoned point just
/// short so `nearest_neighbour` always has a well-defined target.
const POLE_LIMIT: f64 = FRAC_PI_2 - 1e-9;

/// The point reached by travelling `delta_rad` along the rhumb of
/// `bearing_deg` from `from`.
///
/// A rhumb (loxodrome) holds a CONSTANT bearing; it is not the shortest
/// path. Latitude therefore advances linearly with distance, and longitude
/// advances by the distance divided by `q`, the ratio of the latitude
/// difference to the STRETCHED (Mercator) latitude difference. As the
/// course approaches due east or west that ratio becomes `0/0`, and the
/// limit is `cos(phi1)` — which is why the epsilon branch below is a
/// mathematical necessity rather than a defensive guard.
/// type-audit: pending(wave-3: bearing_deg), bare-ok(ratio: delta_rad)
pub fn rhumb_advance(from: GeoCoord, bearing_deg: f64, delta_rad: f64) -> GeoCoord {
    let phi1 = from.latitude.to_radians();
    let lam1 = from.longitude.to_radians();
    let theta = bearing_deg.to_radians();

    let dphi = delta_rad * math::cos(theta);
    let phi2 = (phi1 + dphi).clamp(-POLE_LIMIT, POLE_LIMIT);

    let dpsi = math::ln(math::tan(FRAC_PI_4 + phi2 / 2.0) / math::tan(FRAC_PI_4 + phi1 / 2.0));
    // The limit as the course approaches due east/west. `dphi/dpsi` is the
    // ratio of true to stretched latitude change; both go to zero together.
    let q = if dpsi.abs() > 1e-12 {
        (phi2 - phi1) / dpsi
    } else {
        math::cos(phi1)
    };

    let dlam = delta_rad * math::sin(theta) / q;

    GeoCoord {
        latitude: phi2.to_degrees(),
        longitude: normalize_lon((lam1 + dlam).to_degrees()),
    }
}

/// Squared chord distance between two unit-sphere points.
///
/// Squared and un-rooted deliberately: it is MONOTONIC in the angle, so it
/// orders candidates identically to a great-circle distance while avoiding
/// both a `sqrt` and an `acos`. `acos` near zero loses precision badly, and
/// adjacent cells are exactly the near-zero case.
fn chord_sq(a: [f64; 3], b: [f64; 3]) -> f64 {
    let (dx, dy, dz) = (a[0] - b[0], a[1] - b[1], a[2] - b[2]);
    dx * dx + dy * dy + dz * dz
}

/// The angle between two coordinates, in radians.
///
/// `2*asin(chord/2)` rather than `acos(dot)`: the two are equal in exact
/// arithmetic, but `acos` is ill-conditioned for small angles and every
/// angle this function is asked for is a small one.
fn angle_between_coords(a: GeoCoord, b: GeoCoord) -> f64 {
    let pa = math::unit_sphere_from_lat_lon(a.latitude, a.longitude);
    let pb = math::unit_sphere_from_lat_lon(b.latitude, b.longitude);
    let chord = chord_sq(pa, pb).sqrt();
    2.0 * math::asin((chord / 2.0).clamp(-1.0, 1.0))
}

/// One step, in radians: the mean angular distance from `addr` to its three
/// edge-neighbours.
///
/// Derived rather than a constant, so it scales with refinement depth on
/// its own. A constant would silently become a tuned number the first time
/// the walk band moved.
/// type-audit: bare-ok(ratio: return)
pub fn step_length_rad(addr: &RoomAddr) -> f64 {
    let here = addr.coord();
    let ns = addr.neighbors();
    let total: f64 = ns
        .iter()
        .map(|n| angle_between_coords(here, n.coord()))
        .sum();
    total / ns.len() as f64
}

/// Which of `position`'s three edge-neighbours lies nearest `target`.
///
/// Ties break on ascending packed room id, so the result never depends on
/// the order `neighbors()` happens to return. `total_cmp` rather than
/// `partial_cmp`: float ordering in this project is total and deterministic
/// by rule.
pub fn nearest_neighbour(position: &RoomAddr, target: GeoCoord) -> RoomAddr {
    let t = math::unit_sphere_from_lat_lon(target.latitude, target.longitude);
    let mut best: Option<(f64, u64, RoomAddr)> = None;
    for n in position.neighbors() {
        let c = n.coord();
        let d = chord_sq(math::unit_sphere_from_lat_lon(c.latitude, c.longitude), t);
        let key = n.pack().map(|r| r.0).unwrap_or(u64::MAX);
        let take = match &best {
            None => true,
            Some((bd, bk, _)) => d.total_cmp(bd).then(key.cmp(bk)).is_lt(),
        };
        if take {
            best = Some((d, key, n));
        }
    }
    // `RoomAddr::neighbors` returns a fixed-size `[RoomAddr; 3]`, so the
    // loop always runs three times and `best` is always `Some`.
    best.expect("a room always has three edge-neighbours").2
}

/// The canonical bearing of a compass point, degrees clockwise from north.
///
/// Exhaustive by construction: adding a `Compass` variant fails to compile
/// here until it is given a bearing, which is the same discipline
/// `compass_variants_must_all_be_rostered` holds one crate over.
/// type-audit: pending(wave-3: return)
pub fn bearing_of(c: Compass) -> f64 {
    match c {
        Compass::N => 0.0,
        Compass::Ne => 45.0,
        Compass::E => 90.0,
        Compass::Se => 135.0,
        Compass::S => 180.0,
        Compass::Sw => 225.0,
        Compass::W => 270.0,
        Compass::Nw => 315.0,
    }
}

/// Fold a longitude in degrees into `GeoCoord`'s documented `(-180, 180]`.
///
/// `%` alone is not enough: Rust's remainder keeps the sign of the dividend,
/// so `-180.0 % 360.0` is `-180.0`, which is outside the range.
fn normalize_lon(deg: f64) -> f64 {
    let wrapped = ((deg + 180.0) % 360.0 + 360.0) % 360.0 - 180.0;
    if wrapped == -180.0 { 180.0 } else { wrapped }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Longitude comes back in `(-180, 180]`, matching `GeoCoord`'s own
    /// documented range. 180 stays 180; -180 folds to 180.
    ///
    /// FIRES WHEN: the fold uses `%` alone, which keeps -180 negative.
    #[test]
    fn longitude_normalises_into_the_geocoord_range() {
        assert_eq!(normalize_lon(0.0), 0.0);
        assert_eq!(normalize_lon(180.0), 180.0);
        assert_eq!(normalize_lon(-180.0), 180.0);
        assert_eq!(normalize_lon(190.0), -170.0);
        assert_eq!(normalize_lon(-190.0), 170.0);
        assert_eq!(normalize_lon(540.0), 180.0);
    }

    /// The eight compass points map to their canonical bearings, clockwise
    /// from north. This is the one place the mapping exists; `Compass` itself
    /// carries no bearing.
    ///
    /// FIRES WHEN: a variant is added, or two variants collide on one bearing.
    #[test]
    fn every_compass_point_has_its_canonical_bearing() {
        use hornvale_locale::Compass;
        assert_eq!(bearing_of(Compass::N), 0.0);
        assert_eq!(bearing_of(Compass::Ne), 45.0);
        assert_eq!(bearing_of(Compass::E), 90.0);
        assert_eq!(bearing_of(Compass::Se), 135.0);
        assert_eq!(bearing_of(Compass::S), 180.0);
        assert_eq!(bearing_of(Compass::Sw), 225.0);
        assert_eq!(bearing_of(Compass::W), 270.0);
        assert_eq!(bearing_of(Compass::Nw), 315.0);
    }

    /// Due east from the equator holds latitude exactly and advances longitude
    /// by the angular distance travelled. This is the degenerate branch where
    /// the stretched-latitude difference is zero and `q` falls back to
    /// `cos(phi1)`; getting it wrong divides by zero.
    ///
    /// FIRES WHEN: the `dpsi ~ 0` fallback is removed or its epsilon inverted.
    #[test]
    fn due_east_on_the_equator_holds_latitude_and_advances_longitude() {
        let from = GeoCoord {
            latitude: 0.0,
            longitude: 0.0,
        };
        let out = rhumb_advance(from, 90.0, 0.1);
        assert!(
            out.latitude.abs() < 1e-12,
            "latitude drifted to {}",
            out.latitude
        );
        assert!((out.longitude - 0.1_f64.to_degrees()).abs() < 1e-9);
    }

    /// Due east at 60 degrees north also holds latitude, but covers TWICE the
    /// longitude the equator did for the same distance, because a parallel at
    /// 60N is half the length of the equator. This is the test that would
    /// catch a `q` that ignored latitude entirely.
    ///
    /// FIRES WHEN: `q` is hardcoded to 1.0, which passes the equator test.
    #[test]
    fn due_east_at_sixty_north_covers_twice_the_longitude() {
        let from = GeoCoord {
            latitude: 60.0,
            longitude: 0.0,
        };
        let out = rhumb_advance(from, 90.0, 0.1);
        assert!((out.latitude - 60.0).abs() < 1e-9);
        assert!((out.longitude - 2.0 * 0.1_f64.to_degrees()).abs() < 1e-6);
    }

    /// A non-cardinal bearing (45 degrees, "NE") from a mid-latitude start.
    /// Every other `rhumb_advance` test above is due-north, due-east, or a
    /// pole clamp — the generic case where `q` actually mixes both
    /// latitude and longitude is untested by any of them, and that is
    /// exactly where an inverted or transposed `q` would hide.
    ///
    /// Expected values are derived independently of `rhumb_advance`'s own
    /// closed form (the log-tan / Mercator-latitude `q` ratio): latitude is
    /// exact by construction (`dphi/ds = cos(theta)` is constant along a
    /// rhumb, so `phi2 = phi1 + delta*cos(theta)` has no approximation to
    /// make), and longitude comes from numerically integrating the
    /// loxodrome ODE `dlambda/ds = sin(theta) / cos(phi(s))` with Simpson's
    /// rule at 2,000,000 intervals — a different derivation from the one
    /// under test, sharing no code with it. That independent integration
    /// and `rhumb_advance`'s closed form agree to ~7.5e-13 degrees, so the
    /// epsilon below is generous by many orders of magnitude.
    ///
    /// FIRES WHEN: `q` is inverted (`* q` instead of `/ q`), or `sin`/`cos`
    /// are transposed between the latitude and longitude updates.
    #[test]
    fn a_non_cardinal_course_matches_an_independently_derived_longitude() {
        let from = GeoCoord {
            latitude: 40.0,
            longitude: -30.0,
        };
        let out = rhumb_advance(from, 45.0, 0.2);
        assert!(
            (out.latitude - 48.10284684541396).abs() < 1e-9,
            "latitude was {}, expected 48.10284684541396",
            out.latitude
        );
        assert!(
            (out.longitude - (-18.69883715923524)).abs() < 1e-9,
            "longitude was {}, expected -18.69883715923524",
            out.longitude
        );
    }

    /// Due north advances latitude by exactly the distance travelled and does
    /// not touch longitude.
    #[test]
    fn due_north_advances_latitude_only() {
        let from = GeoCoord {
            latitude: 10.0,
            longitude: 20.0,
        };
        let out = rhumb_advance(from, 0.0, 0.05);
        assert!((out.latitude - (10.0 + 0.05_f64.to_degrees())).abs() < 1e-9);
        assert!((out.longitude - 20.0).abs() < 1e-12);
    }

    /// A rhumb approaches a pole and stops at it rather than walking over the
    /// top, which would flip the bearing's meaning without saying so.
    ///
    /// FIRES WHEN: the latitude clamp is removed.
    #[test]
    fn a_rhumb_stops_at_the_pole_rather_than_crossing_it() {
        let from = GeoCoord {
            latitude: 89.0,
            longitude: 0.0,
        };
        let out = rhumb_advance(from, 0.0, 1.0);
        assert!(
            out.latitude <= 90.0,
            "walked past the pole to {}",
            out.latitude
        );
        assert!(out.latitude > 89.0);
    }

    /// Latitude CONVERGES under a sustained non-cardinal course and never
    /// oscillates: once clamped, further steps leave it fixed while longitude
    /// keeps advancing. That is a loxodrome's real polar behaviour — infinite
    /// winding in finite distance — and it is why there is no termination rule.
    ///
    /// **Do not assert a cycle period here.** Task 1 measured the current
    /// system circling the pole with period THREE (three rooms ~120° apart in
    /// longitude at lat 89.99°), not two; the period is a fact about one seed's
    /// lattice, not about the course. Assert convergence and liveness only.
    ///
    /// FIRES WHEN: the clamp oscillates, or latitude resumes climbing.
    #[test]
    fn a_sustained_polar_course_converges_in_latitude_and_keeps_moving() {
        let mut c = GeoCoord {
            latitude: 80.0,
            longitude: 0.0,
        };
        let mut last_lat = c.latitude;
        let mut moved_in_longitude = false;
        for _ in 0..500 {
            let next = rhumb_advance(c, 45.0, 0.01);
            assert!(next.latitude >= last_lat - 1e-9, "latitude went backwards");
            assert!(next.latitude <= 90.0);
            if (next.longitude - c.longitude).abs() > 1e-12 {
                moved_in_longitude = true;
            }
            last_lat = next.latitude;
            c = next;
        }
        assert!(
            c.latitude < 90.0,
            "reached the pole exactly; POLE_LIMIT failed"
        );
        assert!(moved_in_longitude, "the course froze instead of winding");
    }

    // These need a real `RoomAddr`, so they build one from a known face and
    // path rather than a world. `walk_depth` (in the `agent` module) needs a
    // `LocaleContext`, which a pure unit test cannot cheaply build, so the
    // depth is the literal 12 (walk depth on the canonical grid: globe level
    // 6 + 6), matching `agent::walk_depth`'s own documented default.

    /// A step length is positive, and small — adjacent cells at walk depth are
    /// a tiny fraction of a radian apart. An implementation that returned a
    /// CHORD rather than an ANGLE would also be positive and small, so the
    /// upper bound alone does not discriminate; the neighbour-distance
    /// agreement below is what does.
    #[test]
    fn a_step_length_is_positive_and_sub_radian() {
        let addr = RoomAddr {
            face: 0,
            path: vec![0; 12],
        };
        let d = step_length_rad(&addr);
        assert!(d > 0.0, "step length was {d}");
        assert!(
            d < 1.0,
            "step length was {d}, which is not a walk-band step"
        );
    }

    /// The step length is the MEAN of the three neighbour distances, computed
    /// here independently (sum the three angles, divide by three) and
    /// compared to `step_length_rad`'s output directly. A range check against
    /// `[min, max]` is not enough: a first-neighbour sample is itself one of
    /// the three values, so it always lies inside that range too. Comparing
    /// to the exact mean is what tells the two apart.
    ///
    /// FIRES WHEN: the loop returns early, samples one neighbour instead of
    /// averaging all three, or divides by the wrong count.
    #[test]
    fn a_step_length_is_the_mean_of_its_three_neighbour_distances() {
        let addr = RoomAddr {
            face: 0,
            path: vec![0; 12],
        };
        let each: Vec<f64> = addr
            .neighbors()
            .iter()
            .map(|n| angle_between_coords(addr.coord(), n.coord()))
            .collect();
        let expected_mean: f64 = each.iter().sum::<f64>() / each.len() as f64;
        let mean = step_length_rad(&addr);
        assert!(
            (mean - expected_mean).abs() < 1e-12,
            "step_length_rad returned {mean}, expected the mean {expected_mean}"
        );
    }

    /// Resolution returns one of the three edge-neighbours and never the cell
    /// itself — a target that happens to land nearest the observer's own
    /// centroid must still produce a MOVE.
    ///
    /// FIRES WHEN: the candidate list includes `position`.
    #[test]
    fn resolution_always_returns_a_neighbour_never_the_cell_itself() {
        let addr = RoomAddr {
            face: 0,
            path: vec![0; 12],
        };
        let here = addr.coord();
        let dest = nearest_neighbour(&addr, here);
        assert_ne!(dest, addr);
        assert!(addr.neighbors().contains(&dest));
    }

    /// Aiming at a neighbour's own centroid resolves to that neighbour. This is
    /// the positive control for the distance metric: a metric with an inverted
    /// sign would pass every "returns a neighbour" test above.
    #[test]
    fn aiming_at_a_neighbours_centroid_resolves_to_that_neighbour() {
        let addr = RoomAddr {
            face: 0,
            path: vec![0; 12],
        };
        for n in addr.neighbors() {
            assert_eq!(nearest_neighbour(&addr, n.coord()), n);
        }
    }
}
