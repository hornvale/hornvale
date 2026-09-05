//! **A DIAGONAL COSTS ROOT TWO, SO A ZIGZAG BUYS NO GROUND** (The Pavement,
//! Task 7) — the clock half of the octile fix, and the campaign's H2 with its
//! positive control.
//!
//! # The defect
//!
//! `clock::base_cost` prices `Action::MoveTo(_)` at a flat 10,000 ticks
//! whatever room it leads to, and that was correct while the walk band was
//! 4-connected. The Pavement made it 8-connected. On an 8-connected lattice a
//! diagonal step covers `√2 ≈ 1.414` times the ground of an edge step, so a
//! flat charge means **~41% faster travel by zigzagging** — a physics
//! falsehood rather than a preference, and one a player and every creature
//! would both exploit. `cost_of`'s fourth parameter, `step_factor`, is the
//! fix.
//!
//! # Why the positive control is not optional
//!
//! With the multiplier in place the cost ratio is `√2` by construction, which
//! is not evidence: an assertion that a constant equals itself would pass on a
//! probe wired to nothing. So the tests below reconstruct the exploit from two
//! independently-derived quantities — the lattice's own MEASURED diagonal/edge
//! separation (pure kernel geometry, no clock) and the clock's cost ratio (no
//! geometry) — and show that forcing `step_factor` to `1.0` makes the ~41% gap
//! reappear in ground-covered-per-tick. If the probe could not reproduce the
//! defect it would not be measuring the fix.
//!
//! # Addendum 2's trap, avoided explicitly
//!
//! Nothing here averages over all eight neighbours. The deleted
//! `course.rs::step_length_rad` divided a total by `ns.len()` and so returned
//! ~1.21 edge steps once the mesh went 8-connected — a number that reads as
//! "one step" and is not. The two groups are measured SEPARATELY below, which
//! is the only way the ratio between them means anything.

use hornvale_kernel::room::Facet;
use hornvale_vessel::action::Action;
use hornvale_vessel::clock::{
    DIAGONAL_STEP_FACTOR, REFERENCE_MASS_KG, climb_factor, cost_of, step_factor,
};

/// The walk band's refinement depth — **called, never restated.**
///
/// `hornvale_locale::walk_depth` is the one statement of this arithmetic in the
/// repository (sixteen sites restated it once, two of them production
/// `--depth` defaults that had fallen a whole band behind), and
/// `cli/tests/suite/walk_depth_agreement.rs` scans the tree for a second one.
/// The committed seed-42 world avoids genesis while supplying the facts a
/// `LocaleContext` requires, and this declares no absolute-depth constant for
/// that guard's `absolute_roster` to have to track.
fn walk_depth() -> u32 {
    let world = hornvale_worldgen::fixture::seed_42_world();
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("seed 42 builds a context");
    hornvale_vessel::walk_depth(&ctx)
}

/// The four edge-adjacent neighbours are `neighbors()[..4]` — the kernel's
/// pinned prefix, mirrored here for the same reason `clock` and `action` each
/// name it rather than spelling `4`.
const EDGE_ADJACENT: usize = 4;

/// The heaviest creature the authored biosphere actually places (a woolly
/// mammoth) and the lightest end of the clamped mass band, so the sweep spans
/// the tempo range a real world reaches.
const MASS_SWEEP: [f64; 5] = [0.001, 1.0, REFERENCE_MASS_KG, 1_000.0, 6_000.0];

/// A room at an explicit face-lattice position, encoding the path digits the
/// kernel's `face_lattice` decodes: a digit is `(hi_x << 1) | hi_y`, coarsest
/// level first. The kernel's own constructor is private, and
/// `a_constructed_room_lands_where_it_was_asked_for` checks this encoder
/// against the public decoder rather than trusting it.
fn facet_at(face: u8, x: i64, y: i64, depth: u32) -> Facet {
    Facet {
        face,
        path: (0..depth)
            .rev()
            .map(|i| ((((x >> i) & 1) as u8) << 1) | ((y >> i) & 1) as u8)
            .collect(),
    }
}

/// A deterministic spread of interior walk-depth rooms across all six faces —
/// `count` of them, by coprime strides over the face lattice so no face gets
/// a clustered sample.
///
/// Deliberately NOT seeded: this is a sweep over geometry, and geometry has no
/// seed. Every room is at least one cell inside its face's border, so all
/// eight neighbours exist and the sample says nothing about the corner case
/// (which `a_cube_corner_room_still_prices_its_four_edges_as_edges` covers on
/// purpose).
fn interior_rooms(count: usize) -> Vec<Facet> {
    let depth = walk_depth();
    let scale = 1i64 << depth;
    (0..count)
        .map(|i| {
            let k = i as i64;
            let face = (i % 6) as u8;
            let x = 1 + (k * 1_367) % (scale - 2);
            let y = 1 + (k * 2_741) % (scale - 2);
            facet_at(face, x, y, depth)
        })
        .collect()
}

/// The mean angular separation of a room's centroid from its EDGE neighbours
/// and from its DIAGONAL neighbours, as two separate means — never one mean
/// over all eight (Addendum 2).
fn edge_and_diagonal_separations(room: &Facet) -> (f64, f64) {
    let ns = room.neighbors();
    let mean = |slice: &[Facet]| -> f64 {
        slice.iter().map(|n| room.distance_rad_to(n)).sum::<f64>() / slice.len() as f64
    };
    (mean(&ns[..EDGE_ADJACENT]), mean(&ns[EDGE_ADJACENT..]))
}

/// The encoder above is checked against the kernel's public decoder, so a
/// mis-encoded path cannot make every other test in this file measure a room
/// nobody asked for.
#[test]
fn a_constructed_room_lands_where_it_was_asked_for() {
    let depth = walk_depth();
    for (face, x, y) in [(0u8, 0i64, 0i64), (3, 4_095, 1), (5, 1_234, 7_777)] {
        let f = facet_at(face, x, y, depth);
        let l = f.face_lattice();
        assert_eq!(f.depth(), depth);
        assert_eq!((l.x, l.y, l.scale), (x, y, 1i64 << depth));
        assert_eq!(f.face, face);
    }
}

/// **THE BRIEF'S TEST.** A diagonal step costs `√2` orthogonal steps.
#[test]
fn a_diagonal_step_costs_root_two_orthogonal_steps() {
    let a = Action::MoveTo(facet_at(0, 4_000, 4_000, walk_depth()));
    let orth = cost_of(&a, 70.0, 1.0, 1.0).ticks() as f64;
    let diag = cost_of(&a, 70.0, 1.0, DIAGONAL_STEP_FACTOR).ticks() as f64;
    let ratio = diag / orth;
    assert!(
        (ratio - std::f64::consts::SQRT_2).abs() < 0.005,
        "diagonal/orthogonal was {ratio}, wanted sqrt(2)"
    );
}

/// **H2, the preregistered form** (spec §7): travelling `k` cells diagonally
/// costs within 0.5% of `√2 · k` orthogonal steps' worth of ticks, across the
/// body-mass and climb range.
///
/// # `k` IS NOT A DIMENSION OF THIS SWEEP, and saying so is the point
///
/// A UNIFORM journey of `k` identical steps makes the metric *invariant* in
/// `k`, exactly:
///
/// ```text
/// rel(k) = |k·diag − √2·k·orth| / (√2·k·orth) = |diag − √2·orth| / (√2·orth)
/// ```
///
/// the `k` cancels. An earlier version of this test looped `k` in `1..=64` and
/// its report claimed "5 masses × 4 climbs × 64 `k` = 1,280 combinations". That
/// is **20** combinations, each measured 64 times, and the "worst case at
/// k = 3" it printed was float noise in the last bits, not a finding. This is
/// the tests-whose-input-collapses-to-one-value trap, inside a preregistered
/// measurement, which is the worst place for it: a vacuous dimension inflates
/// the apparent evidence for a claim a human is about to act on.
///
/// So the uniform arm below states its real size — **20** — and `k` earns its
/// place in a second arm where it does NOT cancel: a journey whose terrain
/// VARIES step by step. There every step rounds to its own integer tick count
/// independently, so a `k`-step path is not a scalar multiple of a one-step
/// path and the accumulated rounding is a genuine function of `k`.
///
/// The rounding is why either arm is worth running at all: `cost_of` rounds to
/// a whole tick and floors at one, so a light creature on a cheap step is
/// where the ratio could drift out of tolerance.
#[test]
fn a_diagonal_journey_costs_root_two_the_same_journey_walked_orthogonally() {
    let a = Action::MoveTo(facet_at(0, 4_000, 4_000, walk_depth()));
    let terrains: Vec<f64> = [0.0f64, 250.0, 500.0, 1.0e9]
        .iter()
        .map(|&m| climb_factor(0.0, m))
        .collect();

    // ARM 1 — the uniform journey. 5 masses x 4 climbs = 20 combinations, and
    // that is the honest count: the journey length cancels out of the ratio.
    let mut worst = (0.0f64, String::new());
    let mut uniform_combinations = 0usize;
    for mass in MASS_SWEEP {
        for &terrain in &terrains {
            let orth = cost_of(&a, mass, terrain, 1.0).ticks();
            let diag = cost_of(&a, mass, terrain, DIAGONAL_STEP_FACTOR).ticks();
            let want = std::f64::consts::SQRT_2 * orth as f64;
            let rel = (diag as f64 - want).abs() / want;
            uniform_combinations += 1;
            if rel > worst.0 {
                worst = (
                    rel,
                    format!("mass {mass} kg, terrain {terrain}: {diag} vs sqrt(2)*{orth}"),
                );
            }
        }
    }
    assert_eq!(
        uniform_combinations,
        MASS_SWEEP.len() * terrains.len(),
        "the uniform arm's size is 5 masses x 4 climbs, and nothing else"
    );
    println!(
        "H2 uniform ({uniform_combinations} combinations): worst relative error {:.6}% — {}",
        worst.0 * 100.0,
        worst.1
    );
    assert!(
        worst.0 < 0.005,
        "H2 FAILED (uniform): worst relative error {:.4}% exceeds the preregistered 0.5% over \
         {uniform_combinations} (mass, climb) combinations — {}",
        worst.0 * 100.0,
        worst.1
    );

    // ARM 2 — the VARYING journey, where `k` is real. Each step takes its own
    // terrain from the sweep, so its cost rounds independently and the total is
    // not `k` times anything. `k` runs to 64 because accumulated rounding is
    // what could drift, and it can only show up in a sum.
    let mut worst_path = (0.0f64, String::new());
    let mut path_lengths = 0usize;
    for mass in MASS_SWEEP {
        for k in 1..=64usize {
            let (mut orth_total, mut diag_total) = (0i64, 0i64);
            for step in 0..k {
                // A deliberately non-uniform, deterministic terrain profile:
                // consecutive steps take different climbs, so no prefix of the
                // path is a scalar multiple of any other.
                let terrain = terrains[(step * 3 + 1) % terrains.len()];
                orth_total += cost_of(&a, mass, terrain, 1.0).ticks();
                diag_total += cost_of(&a, mass, terrain, DIAGONAL_STEP_FACTOR).ticks();
            }
            let want = std::f64::consts::SQRT_2 * orth_total as f64;
            let rel = (diag_total as f64 - want).abs() / want;
            path_lengths += 1;
            if rel > worst_path.0 {
                worst_path = (
                    rel,
                    format!(
                        "mass {mass} kg, {k}-step mixed-terrain path: {diag_total} vs \
                         sqrt(2)*{orth_total} = {want}"
                    ),
                );
            }
        }
    }
    println!(
        "H2 mixed-terrain ({path_lengths} paths): worst relative error {:.6}% — {}",
        worst_path.0 * 100.0,
        worst_path.1
    );
    assert!(
        worst_path.0 < 0.005,
        "H2 FAILED (mixed terrain): worst relative error {:.4}% over {path_lengths} paths — {}",
        worst_path.0 * 100.0,
        worst_path.1
    );
}

/// **The `√2` in the constant is a claim about the real lattice, and the real
/// lattice was asked.**
///
/// The cube-sphere's tangent warp distorts a quad, so there was no a-priori
/// reason for a diagonal to sit at exactly `√2` edges — the brief asserted it
/// and this is the check. Measured over 4,000 interior walk-depth rooms spread
/// across all six faces: the mean diagonal separation is **1.411786** edge
/// separations (`-0.172%` against `√2`), with per-room ratios spanning
/// **1.366086–1.434180**. So `√2` is honest as a single authored factor, and the
/// residual is projection distortion the lattice itself carries rather than
/// anything the clock could fix with a different constant.
///
/// Both groups are averaged SEPARATELY (Addendum 2).
#[test]
fn the_diagonal_is_root_two_edges_on_the_lattice_we_actually_walk() {
    let rooms = interior_rooms(4_000);
    let (mut edge_total, mut diag_total) = (0.0f64, 0.0f64);
    let mut per_room: Vec<f64> = Vec::with_capacity(rooms.len());
    for room in &rooms {
        let (edge, diag) = edge_and_diagonal_separations(room);
        assert!(edge > 0.0 && diag > 0.0, "a room with no separation at all");
        edge_total += edge;
        diag_total += diag;
        per_room.push(diag / edge);
    }
    per_room.sort_by(f64::total_cmp);
    // TWO STATISTICS, NAMED SEPARATELY. The first is a ratio of sums over every
    // room in the sample; the second is the mean of the per-room ratios, and
    // min/max belong to THAT population. They are close here but they are not
    // the same estimator, and reporting one word for both is how a spread gets
    // attributed to the wrong quantity.
    let aggregate_ratio = diag_total / edge_total;
    let mean_of_per_room = per_room.iter().sum::<f64>() / per_room.len() as f64;
    println!(
        "lattice diagonal/edge over {} rooms: aggregate (sum/sum) {:.6}, \
         mean of per-room ratios {:.6}, per-room min {:.6}, per-room max {:.6} \
         (sqrt2 {:.6})",
        rooms.len(),
        aggregate_ratio,
        mean_of_per_room,
        per_room[0],
        per_room[per_room.len() - 1],
        std::f64::consts::SQRT_2
    );
    for (label, stat) in [
        ("aggregate (sum/sum)", aggregate_ratio),
        ("mean of per-room ratios", mean_of_per_room),
    ] {
        assert!(
            (stat - std::f64::consts::SQRT_2).abs() / std::f64::consts::SQRT_2 < 0.005,
            "the lattice's {label} diagonal is {stat} edges, which is further \
             from sqrt(2) than the 0.5% the authored DIAGONAL_STEP_FACTOR assumes"
        );
    }
    // The spread is real and bounded — recorded, not asserted tightly, because
    // it is a property of the projection and not of this campaign's code.
    assert!(
        per_room[0] > 1.30 && per_room[per_room.len() - 1] < 1.50,
        "the per-room spread has moved outside the recorded 1.366086-1.434180 band"
    );
}

/// **H2's POSITIVE CONTROL: with the multiplier removed, the ~41% gap
/// reappears — and read the next section before quoting its power.**
///
/// The observable is GROUND COVERED PER TICK, which is what a zigzagging
/// walker actually gains, and it is assembled from two quantities neither of
/// which knows about the other: the lattice's measured diagonal/edge
/// separation (kernel geometry, no clock) and the clock's diagonal/orthogonal
/// tick ratio (no geometry). A diagonal's speed advantage is
/// `separation_ratio / cost_ratio`.
///
/// - **Fixed** (`step_factor = DIAGONAL_STEP_FACTOR`): the cost ratio is `√2`
///   and the advantage collapses to `1.411774 / 1.414214` — a **-0.172%**
///   residual, on the wrong side of parity, i.e. a zigzag is very slightly
///   *slower*. Closed.
/// - **Control** (`step_factor = 1.0`, the pre-Task-7 behaviour): the cost
///   ratio is `1.0` and the advantage is the whole separation ratio,
///   **+41.177%**. The defect reproduces, so the probe is measuring the fix
///   and not a tautology.
///
/// # WHICH ARM ACTUALLY DISCRIMINATES, precisely
///
/// **The control arm has ZERO power over `cost_of`.** It passes `1.0` for
/// `step_factor` and divides by an `orth` computed from the identical call, so
/// `control_cost_ratio` is `1.0` by construction whatever `cost_of` does — it
/// would still be `1.0` in a build where `cost_of` ignored `step_factor`
/// entirely. What that arm tests is the LATTICE: that a diagonal really does
/// buy ~41% more ground, which is the premise the whole fix rests on and which
/// nothing else in this file would notice going false. Calling it a control
/// over the clock, as an earlier draft's report did, overstates it.
///
/// **The `fixed` arm is the one with power.** It divides a real
/// `DIAGONAL_STEP_FACTOR` call by a real `1.0` call, so an implementation that
/// dropped `step_factor` on the floor would put `fixed_advantage` at +41% and
/// trip that assertion. That is the discrimination, and it is worth the same
/// 41% either way — which is exactly why the two are easy to confuse.
///
/// What NEITHER arm can do is exercise a pre-Task-7 build of `cost_of`, because
/// that code no longer exists; `step_factor = 1.0` is the exact behaviour it
/// had, which is the closest a live probe can get.
#[test]
fn removing_the_multiplier_brings_the_forty_one_percent_gap_back() {
    let rooms = interior_rooms(600);
    let (mut edge_total, mut diag_total) = (0.0f64, 0.0f64);
    for room in &rooms {
        let (edge, diag) = edge_and_diagonal_separations(room);
        edge_total += edge;
        diag_total += diag;
    }
    let separation_ratio = diag_total / edge_total;

    let a = Action::MoveTo(rooms[0].clone());
    let orth = cost_of(&a, REFERENCE_MASS_KG, 1.0, 1.0).ticks() as f64;
    let fixed_cost_ratio =
        cost_of(&a, REFERENCE_MASS_KG, 1.0, DIAGONAL_STEP_FACTOR).ticks() as f64 / orth;
    // THE CONTROL. Identically `1.0` by construction — see the doc above: this
    // arm measures the LATTICE (does a diagonal still buy ~41% of ground?),
    // not `cost_of`. The `fixed` ratio above is the arm that would catch a
    // `step_factor`-ignoring implementation.
    let control_cost_ratio = cost_of(&a, REFERENCE_MASS_KG, 1.0, 1.0).ticks() as f64 / orth;
    assert_eq!(
        control_cost_ratio, 1.0,
        "the control arm is 1.0 by construction; if it is not, this test has \
         acquired power it does not claim and its doc needs rewriting"
    );

    let fixed_advantage = separation_ratio / fixed_cost_ratio;
    let control_advantage = separation_ratio / control_cost_ratio;
    println!(
        "separation {separation_ratio:.6}; ground-per-tick advantage of a \
         diagonal: fixed {:+.3}%, control {:+.3}%",
        (fixed_advantage - 1.0) * 100.0,
        (control_advantage - 1.0) * 100.0
    );

    assert!(
        (control_advantage - 1.0) > 0.40,
        "THE LATTICE PREMISE FAILED: with step_factor forced to 1.0 a diagonal \
         should still buy ~41% more ground per tick, and it bought {:+.3}%. \
         The exploit this campaign is fixing would not exist.",
        (control_advantage - 1.0) * 100.0
    );
    assert!(
        (fixed_advantage - 1.0).abs() < 0.01,
        "THE FIX FAILED: a {:+.3}% zigzag advantage survives, which is more \
         than the projection distortion alone accounts for. This is the arm \
         with power over `cost_of` — a build that ignored `step_factor` would \
         land here at about +41%.",
        (fixed_advantage - 1.0) * 100.0
    );
}

/// `step_factor` prices exactly the diagonal steps and nothing else — both
/// directions, so the test is not blind to under-pricing.
#[test]
fn step_factor_prices_a_real_diagonal_and_only_a_real_diagonal() {
    for room in interior_rooms(120) {
        let ns = room.neighbors();
        assert_eq!(ns.len(), 8, "an interior room has all eight neighbours");
        for (i, n) in ns.iter().enumerate() {
            let want = if i < EDGE_ADJACENT {
                1.0
            } else {
                DIAGONAL_STEP_FACTOR
            };
            assert_eq!(
                step_factor(&room, n),
                want,
                "neighbour {i} of {room:?} priced wrong"
            );
        }
        // A room is not its own neighbour, and pricing the null step at the
        // orthogonal unit is the total-function arm. This is the one
        // non-adjacent pair `step_factor`'s `debug_assert!` deliberately
        // ADMITS; a genuinely distant pair is refused loudly, which
        // `pricing_a_step_between_rooms_that_do_not_touch_fails_loudly`
        // holds one test down.
        assert_eq!(step_factor(&room, &room), 1.0);
    }
}

/// **A step between rooms that do not touch is refused LOUDLY**, in any build
/// with debug assertions — this project's standing rule, applied to the one
/// arm of `step_factor` whose safety rested on an argument ("no caller can
/// reach it") rather than on a check.
///
/// `#[cfg(debug_assertions)]` rather than unconditional: the release fallback
/// is a deliberate `1.0` (a mid-walk panic is worse than an under-charge), so
/// the panic this asserts genuinely does not exist in a release build, and a
/// test that claimed otherwise would be asserting the profile rather than the
/// behaviour.
#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "rooms that do not touch")]
fn pricing_a_step_between_rooms_that_do_not_touch_fails_loudly() {
    let depth = walk_depth();
    let here = facet_at(0, 4_000, 4_000, depth);
    // Far enough that no seam or corner rule could make these adjacent.
    let far = facet_at(3, 100, 100, depth);
    assert!(
        !here.neighbors().contains(&far),
        "precondition: the two rooms must genuinely not touch"
    );
    let _ = step_factor(&here, &far);
}

/// A cube-corner room drops a DIAGONAL, never an edge, so its four edge steps
/// are still priced as edges and its three surviving diagonals still as
/// diagonals.
///
/// 8 cube corners, three quads meeting at each: **24 rooms**, and the two
/// numbers are not the same number.
#[test]
fn a_cube_corner_room_still_prices_its_four_edges_as_edges() {
    let depth = walk_depth();
    let scale = 1i64 << depth;
    let hi = scale - 1;
    let mut corner_rooms = 0usize;
    for face in 0u8..6 {
        for (x, y) in [(0i64, 0i64), (hi, 0), (0, hi), (hi, hi)] {
            let room = facet_at(face, x, y, depth);
            let ns = room.neighbors();
            if ns.len() == 8 {
                continue; // not one of the cube's own corners
            }
            assert_eq!(ns.len(), 7, "a cube corner drops exactly one step");
            corner_rooms += 1;
            for (i, n) in ns.iter().enumerate() {
                let want = if i < EDGE_ADJACENT {
                    1.0
                } else {
                    DIAGONAL_STEP_FACTOR
                };
                assert_eq!(step_factor(&room, n), want, "corner neighbour {i}");
            }
        }
    }
    assert_eq!(
        corner_rooms, 24,
        "8 cube corners with three quads meeting at each is 24 rooms"
    );
}

/// No action is free at any step geometry — the totality property survives the
/// new parameter, including the degenerate values a caller could pass.
#[test]
fn no_step_geometry_makes_an_action_free_or_explodes_it() {
    let a = Action::MoveTo(facet_at(0, 1, 1, walk_depth()));
    for bad in [0.0f64, -1.0, f64::NAN, f64::INFINITY] {
        let ticks = cost_of(&a, REFERENCE_MASS_KG, 1.0, bad).ticks();
        assert_eq!(
            ticks,
            cost_of(&a, REFERENCE_MASS_KG, 1.0, 1.0).ticks(),
            "a nonsense step_factor ({bad}) must fall back to the orthogonal unit"
        );
        assert!(ticks > 0);
    }
    for a in [Action::Drink, Action::Eat, Action::Rest] {
        assert!(cost_of(&a, REFERENCE_MASS_KG, 1.0, DIAGONAL_STEP_FACTOR).ticks() > 0);
    }
}
