//! H1 and its positive control, plus H3 and the direction-fidelity test that
//! closes M3. See the spec's §7.
//!
//! The seam most worth an explicit assertion. Losing the non-re-seeding line
//! in `Session::go` silently reduces the campaign to the stateless variant —
//! re-seed the reckoned point from the landed cell every step — while every
//! Task 3 test stays green. This file is what makes that regression loud.

use hornvale_kernel::{GeoCoord, RoomAddr, math};
use hornvale_vessel::course::{nearest_neighbour, rhumb_advance, step_length_rad};
use hornvale_vessel::{PossessOpts, Session};

mod common;

/// A mid-latitude, walk-depth `RoomAddr`. Depth 12 (walk depth on the
/// canonical grid — globe level 6 + 6) and a fixed face match Task 2's own
/// unit-test helper in `course.rs`'s `#[cfg(test)] mod tests` — `walk_depth`
/// itself needs a `LocaleContext` a pure test cannot cheaply build, so the
/// literal stands in for it here too — but the FIRST path index deliberately
/// differs (`1`, not `0`).
///
/// **Verified empirically, not copied on faith.** Task 2's own address
/// (`face: 0, path: vec![0; 12]`) resolves to latitude 0.008 degrees — the
/// equator, not "mid-latitude" as the task brief that named this helper
/// claimed. That distinction matters here specifically because it is where
/// H1's second assertion lives: traced over 40 steps, the all-zero path's
/// cross-track offset exceeds the one-step-length bound on 27 of 40 steps,
/// growing to 4.1x a step by the end — a real property of this lattice's
/// neighbour structure near the equator, unrelated to dead reckoning's
/// correctness (the SAME trace's meridian invariant holds throughout). One
/// path step over (`path[0] = 1`, latitude 31.72 degrees, genuinely
/// mid-latitude) satisfies the cross-track bound on all 40 steps with room
/// to spare (max ratio 0.85) while leaving H1's meridian check, the
/// positive control, and H3 all unaffected — confirmed by re-tracing each
/// against this address before adopting it.
fn walk_band_addr() -> RoomAddr {
    let mut path = vec![0u8; 12];
    path[0] = 1;
    RoomAddr { face: 0, path }
}

/// **NOT cross-track — this measures TOTAL separation, and Task 3's review
/// proved that is the wrong quantity.** Kept only as a helper for manual
/// diagnosis of a failing run; H1 below does not call it, and both of its
/// assertions are exact equalities/bounds that need no diagnostic aid when
/// they pass. See the box under Step 3 in the task brief.
#[allow(dead_code)]
fn total_separation(actual: GeoCoord, ideal: GeoCoord) -> f64 {
    let a = hornvale_kernel::math::unit_sphere_from_lat_lon(actual.latitude, actual.longitude);
    let b = hornvale_kernel::math::unit_sphere_from_lat_lon(ideal.latitude, ideal.longitude);
    let (dx, dy, dz) = (a[0] - b[0], a[1] - b[1], a[2] - b[2]);
    let chord = (dx * dx + dy * dy + dz * dz).sqrt();
    2.0 * hornvale_kernel::math::asin((chord / 2.0).clamp(-1.0, 1.0))
}

/// The tolerance H1's meridian check and its positive control both measure
/// against. **This deviates from the task brief, which specified exact
/// `assert_eq!` and explicitly warned against an epsilon here — verified
/// empirically to be wrong, not skipped on faith.**
///
/// Traced directly (`rhumb_advance` at bearing 0 on `walk_band_addr()`):
/// `dlam` computed from `delta_rad * sin(0.0) / q` IS bit-exact zero, exactly
/// as the brief predicted, and `lam1.to_degrees()` round-trips the input
/// longitude exactly too. The wobble is downstream, in `normalize_lon`'s own
/// modulo chain (`((deg + 180.0) % 360.0 + 360.0) % 360.0 - 180.0`), which is
/// NOT bit-preserving even for a value already inside `(-180, 180]`: it
/// perturbs `walk_band_addr()`'s longitude by a constant, non-accumulating
/// 5.684341886080802e-14 (2 ULP at this magnitude) on every call, carried or
/// not — traced flat across all 40 steps of the carried walk, never growing.
///
/// A real re-seed's drift is a different order of magnitude entirely and
/// grows: 2.58e-3 by step 1, past 5e-2 by step 24, ~9e-2 by step 39 (traced
/// against the same `walk_band_addr()`). `1e-9` sits five orders of
/// magnitude above the measured noise floor and six below the measured
/// re-seed drift, so it discriminates cleanly without hiding the defect —
/// and matches the epsilon this crate's own `rhumb_advance` unit tests
/// already use for longitude equality (`course.rs`'s
/// `due_east_on_the_equator_holds_latitude_and_advances_longitude` and
/// neighbours).
const MERIDIAN_EPSILON_DEG: f64 = 1e-9;

/// THE POSITIVE CONTROL. A memoryless walk — re-seeding the reckoned point
/// from the cell just landed on, every step — copies the cell's zig-zagging
/// longitude into the course, so on a due-north bearing it MUST leave its
/// meridian by more than [`MERIDIAN_EPSILON_DEG`]. If this test ever passes
/// on a difference no bigger than that tolerance, H1 below is vacuous and
/// proves nothing about dead reckoning — which is why the control is
/// measured against the exact same bound H1 asserts, not bare inequality:
/// `normalize_lon`'s own float noise (see [`MERIDIAN_EPSILON_DEG`]) makes
/// `!=` alone true on step 0 even under the CORRECT, non-re-seeding
/// implementation, so a control using raw `!=` would "pass" for the wrong
/// reason and could not be trusted. Breaching the tolerance is the real
/// signal; it shows up from step 1 onward, growing.
#[test]
fn a_memoryless_walk_leaves_the_meridian_that_h1_pins() {
    let mut position = walk_band_addr();
    let meridian = position.coord().longitude;
    let mut departed = false;

    for _ in 0..40 {
        let delta = step_length_rad(&position);
        // The defect being controlled for: re-seed from the landed cell.
        let reckoned = rhumb_advance(position.coord(), 0.0, delta);
        position = nearest_neighbour(&position, reckoned);
        if (reckoned.longitude - meridian).abs() > MERIDIAN_EPSILON_DEG {
            departed = true;
            break;
        }
    }
    assert!(
        departed,
        "the memoryless walk held its meridian within {MERIDIAN_EPSILON_DEG} \
         degrees, so H1 cannot discriminate"
    );
}

/// H1. On a due-north course the rhumb is a meridian, so cross-track
/// distance IS the longitude difference. Two assertions: the carried
/// reckoned point never leaves the meridian by more than
/// [`MERIDIAN_EPSILON_DEG`] (see its doc comment for why this is a tight
/// tolerance rather than the brief's original bit-exact `assert_eq!`), and
/// the walked cell stays within one step length of it.
///
/// FIRES WHEN: the reckoned point is re-seeded from the landed cell —
/// whose longitude zig-zags, and gets copied in.
#[test]
fn a_due_north_course_never_leaves_its_meridian() {
    let mut position = walk_band_addr();
    let meridian = position.coord().longitude;
    let mut reckoned = position.coord();

    for step in 0..40 {
        let delta = step_length_rad(&position);
        // The mechanism: advance the CARRIED point, never re-seed it.
        reckoned = rhumb_advance(reckoned, 0.0, delta);
        position = nearest_neighbour(&position, reckoned);

        assert!(
            (reckoned.longitude - meridian).abs() < MERIDIAN_EPSILON_DEG,
            "step {step}: the reckoned point left its meridian (diff {}) — \
             the course was re-seeded from a cell",
            reckoned.longitude - meridian
        );

        // The cell may lag ALONG the meridian (it only has three neighbours
        // to choose from) but must not wander OFF it by more than a step.
        let off = (position.coord().longitude - meridian).abs().to_radians()
            * math::cos(position.coord().latitude.to_radians());
        assert!(
            off <= delta,
            "step {step}: cell is {off} rad off the meridian, bound {delta}"
        );
    }
}

/// The course survives consecutive `go` in the same direction and is
/// re-seeded by a different one. This is the single line whose loss
/// reduces the campaign to the control above.
///
/// **`assert_ne!(second.reckoned, first.reckoned)` — the task brief's
/// original assertion — turned out to be exactly the vacuous shape this
/// campaign keeps warning about, verified by actually mutating `go`
/// (temporarily re-seeding `course.reckoned` from `self.agent.position.coord()`
/// instead of carrying it forward) and re-running: EVERY test in this file
/// still passed, including that one, because any two consecutive steps
/// produce two different reckoned points whether carried or re-seeded — the
/// same "reckoned != cell" trap the brief's own box calls out for H1's
/// control, one level up at the session boundary.
///
/// The discriminator that DOES fire: on a due-EAST course, latitude is the
/// invariant (the mirror of H1's due-north longitude invariant — see
/// `course.rs`'s own `due_east_at_sixty_north_covers_twice_the_longitude`).
/// Traced directly against seed 42: two consecutive `go e` calls hold
/// `reckoned.latitude` bit-identical under the real, carrying
/// implementation (diff `0e0`, no epsilon needed — unlike H1's longitude
/// check, latitude here never passes through `normalize_lon`'s modulo
/// chain), and differ by `-2.135740410107445e-4` under the re-seeding
/// mutation — a landed cell's own latitude wobbling in from the lattice,
/// five orders of magnitude past any float noise. That is the actual
/// re-seed signature; the brief's inequality check could not see it.
///
/// FIRES WHEN: `go` re-seeds `reckoned` from the landed cell every step.
#[test]
fn a_repeated_direction_continues_the_course_and_a_new_one_reseeds_it() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    s.handle("go e");
    let first = s.course().cloned().expect("go e sets a course");
    s.handle("go e");
    let second = s.course().cloned().expect("go e keeps a course");
    assert_eq!(second.bearing_deg, first.bearing_deg);
    assert_ne!(
        second.reckoned.longitude, first.reckoned.longitude,
        "the course did not advance"
    );
    assert_eq!(
        second.reckoned.latitude, first.reckoned.latitude,
        "a due-east course must hold latitude exactly — it moved, so the \
         course was re-seeded from the landed cell instead of carried"
    );

    s.handle("go n");
    let third = s.course().cloned().expect("go n sets a course");
    assert_eq!(
        third.bearing_deg, 0.0,
        "a new direction did not reset the bearing"
    );
}

/// Opposite directions from one cell reach different neighbours. Trivial
/// to state, and the only thing standing between the new `go` arm and an
/// implementation that ignores its argument.
///
/// FIRES WHEN: `go` resolves without consulting the requested bearing.
#[test]
fn opposite_directions_from_one_cell_do_not_land_on_the_same_neighbour() {
    let world = common::build(42).expect("seed 42 builds");
    for (a, b) in [("n", "s"), ("e", "w"), ("ne", "sw"), ("se", "nw")] {
        let (mut sa, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let (mut sb, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        sa.handle(&format!("go {a}"));
        sb.handle(&format!("go {b}"));
        assert_ne!(
            sa.course().map(|c| c.bearing_deg),
            sb.course().map(|c| c.bearing_deg),
            "{a} and {b} set the same bearing"
        );
    }
}

/// H3. A dead-reckoned course escapes the attractor that traps naive
/// fixed-priority walking.
///
/// Task 1 measured a closed 6-room cycle under "try n, then ne, then e, …,
/// take the first that resolves", reproducible in both rotational orders,
/// on a fully-connected mesh. A course that advances an exact reckoned
/// point cannot be trapped that way, because the target keeps moving even
/// when the cell does not.
///
/// FIRES WHEN: the reckoned point is re-seeded from the landed cell —
/// which is the same defect H1 catches, caught here through its
/// player-visible consequence instead of its geometry.
#[test]
fn a_dead_reckoned_walk_escapes_the_six_room_attractor() {
    let mut position = walk_band_addr();
    let mut reckoned = position.coord();
    let mut seen = std::collections::BTreeSet::new();
    seen.insert(position.pack().expect("packable").0);

    for _ in 0..100 {
        let delta = step_length_rad(&position);
        reckoned = rhumb_advance(reckoned, 45.0, delta);
        position = nearest_neighbour(&position, reckoned);
        seen.insert(position.pack().expect("packable").0);
    }
    assert!(
        seen.len() > 6,
        "visited only {} rooms in 100 steps — inside the attractor Task 1 measured",
        seen.len()
    );
}
