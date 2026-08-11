//! Channel-network properties on a real seed-42 world (The Ford, Task 5):
//! the provider builds and exposes `ChannelNetwork`. See the comment above
//! `provider_transverse_at_agrees_with_the_network` for why this file does
//! not carry a pin-isolation test of its own.
//!
//! Stage 2, Task 1 adds the **bank convention** tests at the bottom: the sign
//! of a channel distance means *left bank facing downstream*, and the tests
//! that pin it are the only things asserting the referent is downstream rather
//! than build order.

use hornvale_kernel::{CellId, Geosphere, Seed, math};
use hornvale_terrain::{ChannelNetwork, GeneratedTerrain, TerrainPins, generate};

/// Level 5: the minimum subdivision that actually accumulates
/// `RIVER_MIN_DRAINAGE` anywhere for seed 42 — level 4 has zero river cells
/// (see `channel.rs`'s own `a_built_network_is_well_formed_and_deterministic`
/// test comment). Level 6 (the canonical grid) is Task 6's measurement, not
/// this task's, and `transverse_at` is a known, already-reviewed O(total
/// vertices)-per-query cost, so this file keeps both the mesh and its
/// sample counts modest rather than paying for the canonical grid.
const TEST_LEVEL: u32 = 5;

fn build_seed_42_terrain() -> GeneratedTerrain {
    let geo = Geosphere::new(TEST_LEVEL);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    GeneratedTerrain::new(geo, outcome)
}

/// Mean angular separation of `c` from its neighbours — the same quantity
/// `channel.rs`'s private `cell_spacing` computes, re-derived here from two
/// adjacent cell positions because `Geosphere` has no public
/// `mean_cell_edge()` and one integration-test caller does not earn it a
/// new kernel method.
fn local_cell_edge(geo: &hornvale_kernel::Geosphere, c: CellId) -> f64 {
    let neighbors = geo.neighbors(c);
    assert!(!neighbors.is_empty(), "cell {c:?} has no neighbours");
    let p = geo.position(c);
    let sum: f64 = neighbors
        .iter()
        .map(|&n| {
            let q = geo.position(n);
            let dot = p[0] * q[0] + p[1] * q[1] + p[2] * q[2];
            math::acos(dot.clamp(-1.0, 1.0))
        })
        .sum();
    sum / neighbors.len() as f64
}

#[test]
fn seed_42_has_channels_and_they_are_narrower_than_a_cell() {
    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    assert!(!net.polylines.is_empty(), "seed 42 has no channels at all");
    let widest = net.widest_half_width() * 2.0;
    let cell_edge = local_cell_edge(terrain.geosphere(), CellId(0));
    assert!(
        widest < cell_edge / 10.0,
        "widest channel {widest} is not far narrower than a cell edge {cell_edge}"
    );
}

// WHY THIS FILE HAS NO PIN-ISOLATION TEST.
//
// The brief's version built two terrains from the same seed, queried
// `.channels()` on only one, and compared `elevation_at`/`drainage_at`
// between them. It cannot discriminate: `GeneratedTerrain::new` builds the
// channel network UNCONDITIONALLY at construction (Task 5's own design —
// "not lazily per call"), so both terrains already have a built network
// before either is queried, and `elevation_at`/`drainage_at` read fields
// `generate()` populated entirely BEFORE `GeneratedTerrain::new` (and
// therefore the channel network) ever runs.
//
// A first replacement attempt tried diffing a `generate()` baseline that
// never touched the network against one taken after heavy channel-network
// use, reasoning that a hidden global/`static` leak would show up there.
// Review mutation-proved that one vacuous too: inserting a literal stray
// draw at the top of `ChannelNetwork::build` (`seed.stream().next_f64()`)
// left it green, because `build` takes `Seed` by value — a `Copy` type with
// no interior mutability — so a draw off a `Stream` built from it is
// structurally unobservable outside that call frame. What the test then
// actually asserted (two `generate()` calls from the same seed agree) is
// already covered by
// `tectonic_properties.rs::genesis_is_deterministic_across_the_sweep`
// (which includes seed 42), so it added cost without adding detection
// power. A test whose name promises a property it cannot detect is worse
// than no test — deleted rather than kept for appearances.
//
// The property this file's provider changes actually rely on — that no
// caller of `channel_seed`/`channel_noise_seed` can derive any stream but
// `streams::CHANNEL_MEANDER` — is enforced by the type itself: the field
// stores the already-derived leg (see `TectonicGlobe::channel_seed`'s doc
// in `globe.rs`), not the terrain-root seed, so there is no live draw
// sequence left for a caller here to perturb even in principle.

/// The provider's `transverse_at` delegates to the network's own — a
/// smoke test that the two agree pointwise, since `provider.rs` must never
/// silently diverge from `ChannelNetwork::transverse_at`.
#[test]
fn provider_transverse_at_agrees_with_the_network() {
    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    for c in terrain.geosphere().cells().step_by(53) {
        let p = terrain.geosphere().position(c);
        assert_eq!(terrain.transverse_at(p), net.transverse_at(p));
    }
}

// ---------------------------------------------------------------------------
// The bank convention (stage 2, Task 1).
//
// The sign of a channel distance is stage 2's load-bearing quantity — a ford
// IS a sign change — and stage 2 stores it in a document. Stage 1 gave it a
// referent it cannot keep: "left of the winning polyline's travel direction",
// where the polyline index is build order. These tests re-anchor it to
// hydrology's own convention, LEFT BANK FACING DOWNSTREAM, and the anchoring
// rests on exactly one property of `ChannelNetwork::build` —
// `the_polyline_vertex_order_is_downstream_order` below is that property,
// asserted rather than assumed.
// ---------------------------------------------------------------------------

/// The perpendicular offset the mirrored pairs are taken at, radians. An
/// order of magnitude below the level-5 cell spacing (~0.0378 rad), so a
/// mirrored pair straddles its own segment rather than wandering into a
/// neighbouring reach.
///
/// **It is 2e-3 and not 2e-4 for a conditioning reason, measured rather than
/// guessed.** A signed distance is `acos(dot)` with `dot = cos(off) ≈ 1`,
/// where `acos` is ill-conditioned: an ULP of error in the dot product
/// (~2.2e-16) becomes `2.2e-16 / sin(off)` radians of error in the angle. At
/// 2e-4 that is ~1.1e-12 rad, and the mirrored pair below really did read
/// 0.0001999999991704747 against -0.00020000000028069776 — a 1.1e-12 gap that
/// is float conditioning, not an asymmetry in the construction. At 2e-3 the
/// same bound is ~1.1e-13, an order of magnitude inside the tolerance. Do not
/// "fix" a failure here by loosening the tolerance without first checking
/// whether the offset is what put you against it.
const MIRROR_OFFSET: f64 = 2.0e-3;

/// The seeds `the_polyline_vertex_order_is_downstream_order` sweeps. 42, 7 and
/// 1234 are the campaign's usual trio; 99 and 2024 widen it to five so the
/// downstream-order claim is a property rather than one world's anecdote. All
/// five build in ~0.2 s each at level 5.
const SWEEP_SEEDS: [u64; 5] = [42, 7, 1234, 99, 2024];

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn norm(v: [f64; 3]) -> f64 {
    dot(v, v).sqrt()
}

fn normalize(v: [f64; 3]) -> [f64; 3] {
    let n = norm(v);
    assert!(n > 0.0, "cannot normalize a zero vector");
    [v[0] / n, v[1] / n, v[2] / n]
}

/// Two points at exactly `off` radians from the great circle through `a` and
/// `b`, on opposite sides of it: the first on the +normal (left-of-travel)
/// side, the second its exact mirror image.
///
/// The construction is a **reflection across the segment's own plane**, which
/// is an isometry of the sphere — that is what makes the two magnitudes agree
/// and lets this test assert a SIGN-ONLY difference. `channel_golden.rs`'s
/// module doc names the trap to avoid here: `normalize(2b - t)` looks like a
/// mirror, is not distance-preserving, and perturbs the magnitude in the
/// eighth significant digit, so a test built on it reddens on magnitude and
/// says nothing about sign.
///
/// Concretely, with `n` the unit normal and `m` the arc midpoint (which is
/// perpendicular to `n`), the pair is `cos(off)·m ± sin(off)·n`. Both are unit
/// vectors, both project onto `m`, and both sit `off` radians from the circle.
///
/// `None` for a degenerate segment: coincident vertices have no normal to
/// reflect across. Meander displacement can collapse adjacent vertices, so
/// that branch is reachable and is skipped rather than asserted away.
fn mirrored_pair_across(a: [f64; 3], b: [f64; 3], off: f64) -> Option<([f64; 3], [f64; 3])> {
    let n = cross(a, b);
    if norm(n) == 0.0 {
        return None;
    }
    let n = normalize(n);
    let m = normalize([a[0] + b[0], a[1] + b[1], a[2] + b[2]]);
    let (c, s) = (math::cos(off), math::sin(off));
    let left = [
        c * m[0] + s * n[0],
        c * m[1] + s * n[1],
        c * m[2] + s * n[2],
    ];
    let right = [
        c * m[0] - s * n[0],
        c * m[1] - s * n[1],
        c * m[2] - s * n[2],
    ];
    Some((left, right))
}

/// Positions strung out either side of every channel vertex, at a spread of
/// perpendicular offsets — the sweep the stability hypothesis (H2-1) is
/// scored over. Deterministic in order and count; no draws.
fn sample_positions_near_channels(net: &ChannelNetwork, wanted: usize) -> Vec<[f64; 3]> {
    let mut out = Vec::new();
    for line in &net.polylines {
        for j in 0..line.points.len() {
            let base = line.points[j];
            let ahead = if j + 1 < line.points.len() {
                line.points[j + 1]
            } else {
                line.points[j - 1]
            };
            let travel = [ahead[0] - base[0], ahead[1] - base[1], ahead[2] - base[2]];
            let side = cross(base, travel);
            if norm(side) == 0.0 {
                continue;
            }
            let side = normalize(side);
            for step in 1..=5 {
                for sign in [1.0_f64, -1.0] {
                    if out.len() == wanted {
                        return out;
                    }
                    let off = sign * f64::from(step) * 5.0e-4;
                    out.push(normalize([
                        base[0] + side[0] * off,
                        base[1] + side[1] * off,
                        base[2] + side[2] * off,
                    ]));
                }
            }
        }
    }
    out
}

/// THE PROPERTY THE WHOLE CONVENTION RESTS ON. `ChannelNetwork::build` starts
/// each run at a head and follows `TectonicGlobe.downhill`, appending as it
/// goes, so `run_cells[i]` — and the parallel `polylines[i].points` — run
/// **downstream**. That is what makes "left of the winning segment's travel
/// direction" mean "left bank facing downstream" rather than "left of whatever
/// order the run happened to be built in".
///
/// It is asserted here, on a real world, because it is a property of `build`'s
/// loop rather than of any type: nothing would stop a future change from
/// collecting a run upstream, and the only symptom would be every bank in the
/// world silently swapping sides.
///
/// **It is the only one of the three bank-convention tests that sees that
/// mutation, and it was measured.** Inserting `run.reverse()` before the
/// `run.len() >= 2` check in `ChannelNetwork::build` — which reverses every
/// river in every world — leaves
/// `the_bank_sign_is_left_of_downstream_and_mirrors_exactly` **green**, because
/// that test reads its own travel direction off `points[j] -> points[j + 1]`
/// and the reversal flips both sides of the comparison together. It leaves
/// `the_bank_sign_is_identical_across_two_builds` green too, since both builds
/// reverse alike. A test that derives its reference from the thing under test
/// cannot detect that thing being reoriented; the reference has to come from
/// outside, and here it comes from `TectonicGlobe.downhill`.
///
/// **This is not, however, the only guard in the suite that reddens on it** —
/// do not read the paragraph above as licence to delete the others. At
/// whole-suite scope the same mutation also reddens
/// `channel.rs::a_built_network_is_well_formed_and_deterministic` (which
/// asserts the downhill chain as part of well-formedness),
/// `channel.rs::a_tributary_mouth_sits_exactly_on_the_trunk_vertex_it_joins`
/// (the confluence repair reads run order) and
/// `channel_golden.rs::the_channel_network_is_pinned`. What this test adds is
/// the *statement of intent*: those three redden because a reversed run breaks
/// something else they happened to depend on, whereas this one names vertex
/// order as the referent the bank sign is defined against.
///
/// **Swept across five seeds, not one.** This is the only test whose reference
/// comes from outside the object under test, so it carries the durability
/// guarantee by itself, and one seed makes that an anecdote about one world's
/// drainage rather than a property of `build`. The five together contribute
/// **347** segments (seed 42: 33, seed 7: 104, seed 1234: 50, seed 99: 88,
/// seed 2024: 72), which is also the answer to "would this notice if a seed
/// stopped producing rivers" — the per-seed floor below is what notices.
#[test]
fn the_polyline_vertex_order_is_downstream_order() {
    let mut total = 0usize;
    for seed in SWEEP_SEEDS {
        let geo = Geosphere::new(TEST_LEVEL);
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo, outcome);
        total += downstream_segments_of(&terrain, seed);
    }
    assert!(
        total >= 300,
        "only {total} channel segments across {} seeds (measured 347) — the assertion ran on \
         far less than the population it was calibrated against",
        SWEEP_SEEDS.len()
    );
}

/// Assert every segment of one world's network is a downhill step, and return
/// how many there were. Split out so the sweep's per-seed floor and its total
/// are both stated where they are checked.
fn downstream_segments_of(terrain: &GeneratedTerrain, seed: u64) -> usize {
    let net = terrain.channels();
    let globe = terrain.globe();
    let mut edges = 0usize;
    for (i, cells) in net.run_cells.iter().enumerate() {
        assert_eq!(
            cells.len(),
            net.polylines[i].points.len(),
            "seed {seed}: run cells are not parallel to polyline points"
        );
        for pair in cells.windows(2) {
            assert_eq!(
                *globe.downhill.get(pair[0]),
                Some(pair[1]),
                "seed {seed}, line {i}: {:?} -> {:?} is not a downhill step, so vertex order \
                 is not downstream order and the bank sign has no meaning",
                pair[0],
                pair[1]
            );
            edges += 1;
        }
    }
    // Per-seed, so a single world going riverless is visible rather than being
    // absorbed by the other four in the total. The measured minimum across the
    // sweep is seed 42's 33.
    assert!(
        edges >= 25,
        "only {edges} channel segments on seed {seed} at level {TEST_LEVEL} — too few for this \
         assertion to have run on anything"
    );
    edges
}

/// The sign means left-of-downstream, and two points mirrored across a
/// segment differ in sign while agreeing in magnitude.
///
/// Asserted over EVERY segment of the real network rather than one hand-picked
/// vertex: a single pair could pass by accident of where it sits, and the
/// claim is universal. Pairs whose nearest line is not the segment's own line
/// are skipped — for those the reading is about a different river and says
/// nothing about this one — and the surviving population is floored so the
/// skip cannot quietly empty the test. **Measured:** 33 pairs survive on seed
/// 42 at level 5 — which is *every* segment the network has (13 lines, 46
/// vertices), so the filter is not currently masking a single case — with a
/// worst magnitude gap of 1.11e-13 rad. The floor of 25 leaves room for
/// ordinary terrain drift without leaving room for the filter to start
/// swallowing the population.
#[test]
fn the_bank_sign_is_left_of_downstream_and_mirrors_exactly() {
    let terrain = build_seed_42_terrain();
    let net = terrain.channels();
    let mut checked = 0usize;
    let mut worst = 0.0_f64;
    for (i, line) in net.polylines.iter().enumerate() {
        for j in 0..line.points.len() - 1 {
            let (a, b) = (line.points[j], line.points[j + 1]);
            let Some((left, right)) = mirrored_pair_across(a, b, MIRROR_OFFSET) else {
                continue;
            };
            if net.nearest_line(left).map(|(k, _)| k) != Some(i)
                || net.nearest_line(right).map(|(k, _)| k) != Some(i)
            {
                continue;
            }
            let dl = net
                .bank_signed_distance(left)
                .expect("the network is non-empty");
            let dr = net
                .bank_signed_distance(right)
                .expect("the network is non-empty");
            assert!(
                dl > 0.0,
                "line {i} segment {j}: the +normal side must be LEFT of downstream, got {dl}"
            );
            assert!(
                dr < 0.0,
                "line {i} segment {j}: the -normal side must be RIGHT of downstream, got {dr}"
            );
            let gap = (dl.abs() - dr.abs()).abs();
            worst = worst.max(gap);
            assert!(
                gap < 1e-12,
                "line {i} segment {j}: a mirrored pair must be equidistant, got {dl} vs {dr}"
            );
            checked += 1;
        }
    }
    assert!(
        checked >= 25,
        "only {checked} mirrored pairs survived the same-line filter (worst magnitude gap \
         {worst}) — the assertion above ran on almost nothing"
    );
}

/// H2-1: the sign is stable across builds. Two independently built terrains
/// from the same seed must agree bit for bit on the signed distance at every
/// sampled position.
///
/// This is entailed by `build` being deterministic, which is already asserted
/// elsewhere; it is preregistered and kept anyway because the hypothesis is
/// about the SIGN specifically, and because the failure it guards against —
/// a referent that renumbers with build order — would show up here first.
#[test]
fn the_bank_sign_is_identical_across_two_builds() {
    let a = build_seed_42_terrain();
    let b = build_seed_42_terrain();
    let (na, nb) = (a.channels(), b.channels());
    let mut checked = 0usize;
    let mut signed = 0usize;
    for p in sample_positions_near_channels(na, 400) {
        let (sa, sb) = (na.bank_signed_distance(p), nb.bank_signed_distance(p));
        assert_eq!(
            sa.map(f64::to_bits),
            sb.map(f64::to_bits),
            "diverged at {p:?}"
        );
        if sa.is_some_and(|d| d < 0.0) {
            signed += 1;
        }
        checked += 1;
    }
    assert!(
        checked >= 200,
        "only {checked} positions sampled; the sweep is too thin"
    );
    // Anti-vacuity: a sweep on which every reading came out the same sign
    // would agree across builds no matter how the sign was computed. The
    // sampler offsets both ways from every vertex, so the measured split is
    // 200 of 400 — a floor of "both signs occur" is what this can honestly
    // claim, since the balance is a property of the sampler and not of the
    // world.
    assert!(
        signed > 0 && signed < checked,
        "{signed} of {checked} readings are right-bank — the sweep does not straddle the \
         channel, so agreeing on the sign proves nothing"
    );
}
