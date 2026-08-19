//! H1 and its positive control, H3 and the fixed-priority control it was
//! missing, and the direction-fidelity test that closes M3. See the spec's
//! §7.
//!
//! The seam most worth an explicit assertion. Losing the non-re-seeding line
//! in `Session::go` silently reduces the campaign to the stateless variant —
//! re-seed the reckoned point from the landed cell every step — while every
//! Task 3 test stays green. This file is what makes that regression loud.
//!
//! **Revised after review.** Three findings from the first round changed
//! what is asserted here, each verified by actually mutating `go` and
//! re-running rather than by reasoning about it — the doc comment on each
//! affected item says what changed and what evidence justifies it:
//! H3 originally asserted a claim (bearing-based resolution escapes Task
//! 1's attractor) with no control that could discriminate it, the direction-
//! fidelity test asserted on a value assigned upstream of resolution, and
//! the cross-track bound's own difference computation had a live ±180°
//! wraparound bug.

use hornvale_kernel::{GeoCoord, RoomAddr, math, quantize};
use hornvale_locale::Compass;
use hornvale_vessel::course::{bearing_of, nearest_neighbour, rhumb_advance, step_length_rad};
use hornvale_vessel::{PossessOpts, Session, Turn, WorldContext};

use crate::common;

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
///
/// **Left as-is on review, per campaign decision.** This address sits 9.3
/// milli-degrees from the antimeridian, which exposed a real wraparound bug
/// in the cross-track difference itself (see [`wrapped_lon_diff_deg`]) — the
/// fix is in the computation, not in moving this fixture again.
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
/// **Margins, corrected on review** (the first pass mislabelled the
/// pre-correction equatorial address's drift as this address's own): traced
/// against the ACTUAL `walk_band_addr()` used here, the memoryless control's
/// real re-seed drift is `9.339037759048097e-3` by step 1 — not monotonic
/// (it is `1.39e-3` by step 39), but never below noise. `1e-9` sits **4.25
/// orders of magnitude above** the measured noise floor
/// (`log10(1e-9 / 5.684e-14) ≈ 4.245`) and **~6.97 orders below** the
/// measured re-seed drift (`log10(9.339e-3 / 1e-9) ≈ 6.970`), so it
/// discriminates cleanly without hiding the defect — and matches the epsilon
/// this crate's own `rhumb_advance` unit tests already use for longitude
/// equality (`course.rs`'s
/// `due_east_on_the_equator_holds_latitude_and_advances_longitude` and
/// neighbours).
const MERIDIAN_EPSILON_DEG: f64 = 1e-9;

/// Signed longitude difference `a - b`, folded into `(-180, 180]` before any
/// caller takes its magnitude.
///
/// **Required, not optional — verified live on review, not theoretical.**
/// [`walk_band_addr`] sits 9.3 milli-degrees from the antimeridian, and its
/// walked cell lands on longitude exactly `180.0` on roughly half of its
/// steps. A naive `(a - b).abs()` happens to survive at THIS address only
/// because both the meridian and every landed cell stay on the same side of
/// the ±180 seam — but the identical computation blows up without warning
/// one step away: the face-16 mirror of this address (same latitude, same
/// local geometry, meridian on the NEGATIVE side of the seam while its
/// landed cells still read `+180.0`) measured `off / delta = 32816.853` from
/// the unwrapped subtraction alone, against `0.851` from the wrapped one —
/// four orders of magnitude, entirely an artifact of the seam, nothing to do
/// with dead reckoning or lattice geometry. Folding here is what makes the
/// bound's meaning independent of which side of the antimeridian a fixture
/// happens to sit on, rather than a fact about this one address's luck.
fn wrapped_lon_diff_deg(a: f64, b: f64) -> f64 {
    let raw = a - b;
    ((raw + 180.0) % 360.0 + 360.0) % 360.0 - 180.0
}

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
/// signal; it shows up from step 1 onward.
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

/// H1. Two assertions of very different epistemic status — read them as two
/// separate claims, not one.
///
/// **The meridian invariant (first assertion) is a genuine dead-reckoning
/// property and holds everywhere.** On a due-north course the rhumb IS a
/// meridian; the carried reckoned point's longitude never moves (up to
/// [`MERIDIAN_EPSILON_DEG`] — see its doc for why an exact `assert_eq!` is
/// wrong). This is the real H1, and it is address-independent.
///
/// **The cross-track bound (second assertion) is a LATTICE-QUANTIZATION
/// property, not a dead-reckoning one, and it is FALSE IN GENERAL.**
/// Confirmed on review: the walked cell's distance from the meridian is
/// unbounded over a long enough walk — growing roughly 0.086 step-lengths
/// per step at some addresses, reaching 172x a single step by 2,000 steps.
/// It depends on the LOCAL TRIAD'S ALIGNMENT around the walker, not on
/// latitude: where one of the three edge-neighbours sits at bearing exactly
/// 0.00 degrees and the other two sit at symmetric bearings (+/-65.35
/// degrees) around it, repeatedly picking the nearest neighbour to a
/// due-north target closes into a 4-cycle that never drifts further; where
/// the near-north edges are asymmetric instead (e.g. +18.0 and -47.35
/// degrees, midpoint -14.7 degrees, not 0), the greedy nearest-neighbour
/// choice carries a small constant bias every step, and over enough steps
/// that bias is unbounded. `walk_band_addr()` happens to sit in the FIRST
/// regime (bounded) — verified by the 40-step trace this assertion actually
/// runs — but that is a fact about this one address's local triad, not a
/// property this test may claim generally. Do not read a pass here as
/// evidence about any other address, and do not read a future failure at a
/// DIFFERENT address as a dead-reckoning regression — check which regime
/// that address's triad is in before concluding anything.
///
/// FIRES WHEN (first assertion only): the reckoned point is re-seeded from
/// the landed cell — whose longitude zig-zags, and gets copied in. The
/// second assertion fires on re-seeding too (re-seeded drift is far larger
/// than even the unbounded-regime's per-step bias), but is not a clean
/// re-seed detector on its own — the first assertion is.
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
        // to choose from) but must not wander OFF it by more than a step —
        // true at THIS address's triad, not in general; see the doc above.
        let off = wrapped_lon_diff_deg(position.coord().longitude, meridian)
            .abs()
            .to_radians()
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

/// The eight compass directions this file exercises for direction fidelity,
/// paired with their canonical bearing via `course.rs`'s own `bearing_of`.
const DIRECTIONS: [(&str, Compass); 8] = [
    ("n", Compass::N),
    ("ne", Compass::Ne),
    ("e", Compass::E),
    ("se", Compass::Se),
    ("s", Compass::S),
    ("sw", Compass::Sw),
    ("w", Compass::W),
    ("nw", Compass::Nw),
];

/// Circular distance between two bearings in degrees, in `[0, 180]`.
fn circular_bearing_distance(a_deg: f64, b_deg: f64) -> f64 {
    let d = (a_deg - b_deg).rem_euclid(360.0);
    d.min(360.0 - d)
}

/// The neighbour of `origin` whose OWN bearing from `origin`
/// (`RoomAddr::bearing_to`) is closest to `want_deg`.
///
/// Deliberately independent of the production path: this never calls
/// `nearest_neighbour` or `rhumb_advance`, which would make the expectation
/// circular with the code under test — it would pass whenever `go` and this
/// helper made the SAME mistake together, not just when `go` is correct.
fn neighbour_nearest_by_bearing(origin: &RoomAddr, want_deg: f64) -> RoomAddr {
    let mut best: Option<(f64, RoomAddr)> = None;
    for n in origin.neighbors() {
        let d = circular_bearing_distance(origin.bearing_to(&n), want_deg);
        best = match best {
            None => Some((d, n)),
            Some((bd, _)) if d.total_cmp(&bd).is_lt() => Some((d, n)),
            some => some,
        };
    }
    best.expect("a room always has three edge-neighbours").1
}

/// `go`'s destination matches the neighbour whose OWN bearing from the
/// origin is closest to the requested compass bearing — a claim about
/// resolution's OUTPUT, downstream of anything `go` merely records.
///
/// **This replaces a vacuous predecessor, found by mutation, not
/// inspection.** The original version of this test compared
/// `course().bearing_deg` between two sessions issuing opposite directions.
/// But `bearing_deg` is assigned in `session.rs`'s `go` from
/// `bearing_of(wanted)` — BEFORE `nearest_neighbour` ever runs — so it
/// records what was ASKED FOR, never what resolution actually DID with it.
/// Verified: temporarily replacing `go`'s destination with
/// `self.agent.position.neighbors()[0]` (ignoring the requested bearing
/// entirely, landing on the same neighbour regardless of `dir`) still
/// PASSED the old test on all 8 directions — the bearing field it read was
/// set correctly regardless of what resolution did downstream of it.
///
/// FIRES WHEN: `go` resolves without consulting the requested bearing. Under
/// the `neighbors()[0]` mutation above, this test fails on 5 of 8 directions
/// (`n`, `s`, `sw`, `w`, `nw`; the other 3 land on the ignored default by
/// coincidence at the seed-42 flagship, which is exactly why a coincidental
/// match on SOME directions must not be mistaken for the mechanism working).
#[test]
fn go_lands_on_the_neighbour_nearest_its_requested_bearing() {
    let world = common::build(42).expect("seed 42 builds");
    for (dir, compass) in DIRECTIONS {
        let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let origin = s.agent().position.clone();
        s.handle(&format!("go {dir}"));
        let dest = s.agent().position.clone();
        let expected = neighbour_nearest_by_bearing(&origin, bearing_of(compass));
        assert_eq!(
            dest,
            expected,
            "go {dir}: landed on {:?}, expected the bearing-nearest neighbour {:?}",
            dest.pack().ok(),
            expected.pack().ok()
        );
    }
}

/// Bucket a bearing (degrees clockwise from north) to one of eight compass
/// points. Mirrors `windows-locale`'s own (private) `compass()` function —
/// reproduced here, rather than imported, because that function isn't
/// `pub` — and this specific bucketing is exactly what the OLD "Ways on"
/// exit system used to decide which of the 8 compass directions a room
/// offered: a room has exactly 3 edge-neighbours, so only 3 of the 8
/// buckets are ever occupied.
fn compass_bucket(bearing_deg: f64) -> Compass {
    let b = quantize((bearing_deg % 360.0 + 360.0) % 360.0);
    let idx = (((b + 22.5) / 45.0).floor() as i64).rem_euclid(8);
    [
        Compass::N,
        Compass::Ne,
        Compass::E,
        Compass::Se,
        Compass::S,
        Compass::Sw,
        Compass::W,
        Compass::Nw,
    ][idx as usize]
}

/// Task 1's fixed rotational priority order, repeated as a scripted cycle:
/// n, ne, e, se, s, sw, w, nw.
const FIXED_PRIORITY_CYCLE: [Compass; 8] = [
    Compass::N,
    Compass::Ne,
    Compass::E,
    Compass::Se,
    Compass::S,
    Compass::Sw,
    Compass::W,
    Compass::Nw,
];

/// One attempted move of Task 1's ORIGINAL system: `want` either matches one
/// of the room's 3 occupied compass buckets (move there) or it doesn't
/// (refuse — "No way X from here" under the old locale-exit system — a
/// no-op here).
fn fixed_priority_attempt(addr: &RoomAddr, want: Compass) -> RoomAddr {
    match addr
        .neighbors()
        .into_iter()
        .find(|n| compass_bucket(addr.bearing_to(n)) == want)
    {
        Some(n) => n,
        None => addr.clone(),
    }
}

/// THE CONTROL H3 WAS MISSING. Task 1's own reproduction: a repeating
/// scripted cycle of `go n, go ne, go e, go se, go s, go sw, go w, go nw`
/// under the ORIGINAL exit-matching system, where a move succeeds only if
/// the requested direction is one of the room's 3 occupied compass buckets
/// and otherwise refuses (stays put). Task 1 measured this trapping in a
/// closed 6-room cycle at the seed-42 flagship, reproducible in both
/// rotational orders, over 3,200+ attempted moves.
///
/// **Found by review, not by design.** The first version of this file
/// asserted only that a bearing-based walk escapes a `> 6` room bound, with
/// no control proving that bound could ever be breached by anything. It
/// could not: both the dead-reckoned walk AND a memoryless one select by
/// bearing (never by fixed-priority exit matching), so BOTH visit 101 of
/// 101 possible rooms in 100 steps — the same defect this campaign's H1
/// control chapter already warns about, one level up. Reproducing Task 1's
/// actual mechanism (fixed-priority SCRIPTED CYCLE, not "first bearing that
/// resolves") is what recovers a real trap: traced against
/// `walk_band_addr()` and swept across 24 other addresses (varying face and
/// the first path index), every sampled address traps at 5 or 6 distinct
/// rooms within 50 cycles (400 attempted moves) and stays there through 500
/// cycles (4,000 moves) — genuinely closed, not slow convergence.
#[test]
fn a_fixed_priority_exit_matching_cycle_traps_in_a_closed_room_set() {
    let mut addr = walk_band_addr();
    let mut seen = std::collections::BTreeSet::new();
    seen.insert(addr.pack().expect("packable").0);
    for _ in 0..50 {
        for want in FIXED_PRIORITY_CYCLE {
            addr = fixed_priority_attempt(&addr, want);
            seen.insert(addr.pack().expect("packable").0);
        }
    }
    assert!(
        seen.len() <= 6,
        "the fixed-priority exit-matching cycle visited {} rooms — expected \
         it to trap at ~6, matching Task 1's measured attractor; if it does \
         not trap here, H3 below has no control and should be dropped",
        seen.len()
    );
}

/// H3, RE-SCOPED ON REVIEW. Bearing-based resolution (dead reckoning's
/// `nearest_neighbour` + `rhumb_advance`) escapes the fixed-priority
/// attractor the control above demonstrates.
///
/// **What this is no longer claiming.** Task 1's 6-room trap is a property
/// of FIXED-PRIORITY EXIT-MATCHING selection — "always try n, then ne, …,
/// take the one that matches" — not of memorylessness. A memoryless walk
/// (re-seeding every step) also selects by bearing and escapes this same
/// attractor just as trivially as the real implementation does, so this
/// test is not, and was never, evidence that dead reckoning specifically is
/// correct; H1's meridian invariant carries that claim. What this test DOES
/// show, honestly: resolving toward a continuously-advancing bearing target
/// — dead-reckoned or not — is what breaks Task 1's trap, where matching a
/// fixed list of named exits does not.
///
/// FIRES WHEN: resolution stops choosing by bearing and reverts to
/// fixed-priority exit matching (the mechanism the control above
/// reproduces).
#[test]
fn bearing_based_resolution_escapes_the_fixed_priority_attractor() {
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
        "visited only {} rooms in 100 steps — inside the fixed-priority \
         attractor the control above measures",
        seen.len()
    );
}

/// Every compass point moves the possession, from any walk-band cell.
/// There is no lateral passability model: `go` checks nothing about the
/// destination's biome or water, and `exits_of` filters nothing, so a
/// marine room is as walkable as any other. Task 1 confirmed this by
/// reading `go` and by a 300,000-room walk that never met a refusal it
/// did not cause itself.
///
/// **Strengthened to its preregistered shape (spec §7, final review F3).**
/// This shipped at one seed, at most nine cells along one eastward
/// trajectory — an order of magnitude under H2's frozen text ("at least 200
/// walk-band cells across at least 8 seeds"), and reduced on exactly the
/// axis (`≥8 seeds`) the spec pre-emptively defended: "one world is an
/// anecdote, and a triangle's orientation is exactly the kind of property a
/// single trajectory can fail to vary." The reduction shipped unrecorded;
/// this is the correction, not a new finding — the claim itself was never in
/// doubt (a triangular mesh's three edges are always the candidate set for
/// `nearest_neighbour`, so resolution cannot fail by construction), but
/// preregistration exists to stop a claim shipping "likely true" instead of
/// measured.
///
/// Now the first 8 seeds that build (the same search discipline as
/// `common::world_where`) times 26 cells each = 208 walk-band cells, both
/// bounds cleared. Measured cost on this
/// Mac: **27.6 s** for an identically-shaped 200-cell/8-seed probe (see the
/// campaign's fix-final-review report) — cheap enough to live in the suite
/// outright, so there is no §7.2 reduction to record. It is not in the
/// sub-floor roster (course_properties.rs has none), so `gate-commit` never
/// pays for it; the stage/merge tier does.
///
/// **Why one session per seed, not one per (cell, direction) as the reduced
/// version had.** `Session::start` pays for a fresh `WorldContext` (terrain
/// sculpt, climate fit, demography fit) every call — ~920 ms measured, the
/// dominant cost by two orders of magnitude. `Session::start_in` over one
/// `WorldContext` built once per seed cuts that to ~15 ms; walking forward
/// with `go e` and probing each of the eight directions with `go <dir>` +
/// `back` (which clears the course and restores `agent.position` from the
/// trail) avoids re-deriving anything per probe while still exercising the
/// same `Session::go` an isolated fresh-session probe would. This is a cost
/// optimisation only — the property under test (`go <dir>` never refuses
/// laterally) is unchanged, and `back` is exercised elsewhere
/// (`back_clears_the_course` in `session.rs`) so its own correctness is not
/// resting on this test alone.
///
/// FIRES WHEN: someone adds a passability check without a decision record.
///
/// claim: invariant(forall-seed) — H2 is universally quantified over the
/// sample: every one of the eight compass points must resolve to a
/// neighbour from every sampled walk-band cell, across the first 8 seeds
/// that build (decision 0093).
#[test]
fn no_lateral_refusal_survives_anywhere_on_the_walk_band() {
    /// Seeds attempted, in order, until [`H2_SEEDS_WANTED`] have built —
    /// wide enough that "fewer than 8 seeds build in here" would itself be a
    /// finding about the sim, not about the sample. Mirrors
    /// `common::SIGHT_SEEDS`'s own search discipline.
    const H2_SEED_RANGE: std::ops::Range<u64> = 0..32;
    /// How many distinct, successfully-built seeds H2 needs — the spec's own
    /// floor.
    const H2_SEEDS_WANTED: usize = 8;
    /// Walk-band cells sampled per seed. `H2_SEEDS_WANTED * H2_CELLS_PER_SEED`
    /// (208) clears the spec's 200-cell floor with a small margin.
    const H2_CELLS_PER_SEED: usize = 26;

    let mut seeds_tried = 0usize;
    let mut total_cells = 0usize;
    for seed in H2_SEED_RANGE {
        if seeds_tried >= H2_SEEDS_WANTED {
            break;
        }
        let Some(world) = common::build(seed) else {
            continue;
        };
        let Ok(ctx) = WorldContext::build(&world) else {
            continue;
        };
        let Ok((mut s, _)) = Session::start_in(&ctx, &PossessOpts::default()) else {
            continue;
        };
        seeds_tried += 1;
        for cell in 0..H2_CELLS_PER_SEED {
            match s.handle("go e") {
                Turn::Out(t) => assert!(
                    !t.contains("No way"),
                    "seed {seed}, advancing to cell {cell}: go e refused: {t}"
                ),
                Turn::Released(t) => panic!("seed {seed}: go e released the possession: {t}"),
            }
            total_cells += 1;
            for dir in ["n", "ne", "e", "se", "s", "sw", "w", "nw"] {
                let text = match s.handle(dir) {
                    Turn::Out(t) => t,
                    Turn::Released(t) => {
                        panic!("seed {seed}, cell {cell}: {dir} released the possession: {t}")
                    }
                };
                assert!(
                    !text.contains("No way"),
                    "seed {seed}, cell {cell}: {dir} refused: {text}"
                );
                // Undo the probe so the next direction (and the next
                // advance) starts from the same cell, not wherever the
                // probe landed.
                s.handle("back");
            }
        }
    }
    assert!(
        seeds_tried >= H2_SEEDS_WANTED,
        "only {seeds_tried} of {H2_SEED_RANGE:?} seeds built and possessed — \
         H2 needs at least {H2_SEEDS_WANTED}"
    );
    assert!(
        total_cells >= 200,
        "sampled only {total_cells} walk-band cells — H2 needs at least 200"
    );
}
