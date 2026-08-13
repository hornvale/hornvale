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
/// **19,186** segments (seed 42: 2770, seed 7: 4766, seed 1234: 2904, seed 99:
/// 3990, seed 2024: 4756), which is also the answer to "would this notice if a
/// seed stopped producing rivers" — the per-seed floor below is what notices.
///
/// **The floors were RAISED at The Rill's Task 3** (fix round 1), which made
/// the network render the whole land flow tree: they had been 25 per seed and
/// 300 in total against measurements of 539 and 128, and against the new
/// population that is 110x and 64x of headroom — a floor that cannot fire until
/// the world loses 99% of its rivers is not a floor. Earlier readings, kept
/// because the ratios are the finding: 347 at The Ford (33 / 104 / 50 / 88 /
/// 72), 539 after Task 2 (54 / 159 / 76 / 128 / 122).
///
/// claim: invariant(forall-seed) — every polyline segment is a downhill step,
/// so vertex order is downstream order and the bank sign has a referent
#[test]
fn the_polyline_vertex_order_is_downstream_order() {
    let mut total = 0usize;
    for seed in SWEEP_SEEDS {
        let geo = Geosphere::new(TEST_LEVEL);
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo, outcome);
        total += downstream_segments_of(&terrain, seed);
    }
    // Printed, not only asserted: the doc block above quotes a per-seed
    // breakdown, and the total is the one figure a reader would otherwise
    // have to take on trust.
    println!(
        "downstream segments across {} seeds: {total}",
        SWEEP_SEEDS.len()
    );
    assert!(
        total >= 9_000,
        "only {total} channel segments across {} seeds (measured 19,186) — the assertion ran on \
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
    // sweep is seed 42's 2770 (it was 54 after The Rill's Task 2 and 33 at The
    // Ford; Task 3 renders the whole land flow tree, so a "segment" is now any
    // downhill step on land rather than one between two river cells).
    assert!(
        edges >= 1_300,
        "only {edges} channel segments on seed {seed} at level {TEST_LEVEL} (the five sweep \
         seeds measure 2770 / 4766 / 2904 / 3990 / 4756) — too few for this assertion to have \
         run on anything"
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
/// skip cannot quietly empty the test.
///
/// **Measured, re-taken at The Rill's Task 3 (fix round 2):** 2770 pairs
/// survive on seed 42 at level 5, on a network of 1117 lines and 3887 vertices,
/// with a worst magnitude gap of 2.22e-13 rad.
/// Earlier readings, kept because the ratios are the finding: 54 of 54 after
/// Task 2 (22 lines, 76 vertices, worst magnitude gap 1.67e-13 rad); 33 of 33
/// at The Ford (13 lines, 46 vertices). **In every one of those readings the
/// surviving population is EVERY segment the network has**, so the same-line
/// filter has never masked a case — which is the property this paragraph exists
/// to record, and the one that would change silently if a denser network began
/// putting a segment's mirrored probes nearer some other line. The count is
/// printed by the test so the log carries it too.
///
/// **THE FLOOR WAS 25 UNTIL FIX ROUND 2, AND THAT IS THE INTERESTING PART.**
/// Task 3's own floor sweep raised the two floors above in this very file and
/// walked past this one sixty lines below them, leaving 110x headroom against a
/// stated reference (54) that the same commit was replacing with 2770 two
/// screens up. A class sweep can miss a member of its class in the file it is
/// editing; grep for the assertion shape, do not read down the file.
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
    let segments: usize = net.polylines.iter().map(|l| l.points.len() - 1).sum();
    println!(
        "mirrored pairs: {checked} survived the same-line filter of {segments} segments \
         ({} lines, {} vertices); worst magnitude gap {worst:e} rad",
        net.polylines.len(),
        net.polylines.iter().map(|l| l.points.len()).sum::<usize>(),
    );
    assert!(
        checked >= 1_300,
        "only {checked} mirrored pairs survived the same-line filter of {segments} segments \
         (measured 2770 of 2770) — the assertion above ran on almost nothing"
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
    //
    // PRINTED as well as written down, and The Rill re-measured it: still
    // exactly 200 of 400 on a network eight times denser, which is what the
    // sentence above predicts — the cap binds long before the world does, so
    // this figure is a property of `sample_positions_near_channels` and is the
    // one number in this file that a denser network CANNOT move.
    println!("H2-1: {checked} positions sampled across two builds, {signed} right-bank");
    assert!(
        signed > 0 && signed < checked,
        "{signed} of {checked} readings are right-bank — the sweep does not straddle the \
         channel, so agreeing on the sign proves nothing"
    );
}

// ---------------------------------------------------------------------------
// The index equality battery (The Millrace, Task 4).
//
// `ChannelNetwork::nearest_line` is indexed: it narrows a candidate LINE SET
// with a spherical bucket grid over the network's vertices, then runs the
// unchanged scan over that set. The correctness argument is one inequality —
// every segment within `D` of the query has an endpoint inside the cap of
// radius `D + L_max/2` — and it is an argument about COVERAGE, so the way it
// fails is a true winner that never enters the set. That is silent: the answer
// is still *a* nearby line with a well-formed distance, and it reaches the
// SERIALIZED SIGN of `bank_signed_distance`. A wrong index does not go red; it
// commits a different world and then drift-checks green forever.
//
// So the linear scan survives as `nearest_line_reference` and these tests hold
// the two against each other on real worlds. The sample is not uniform, and
// every category below is present for a reason the correctness argument names.
// ---------------------------------------------------------------------------

/// The subdivision levels the equality battery sweeps: the whole legal range
/// (`TerrainPins` admits 4-7). Level matters because it moves both quantities
/// the index is built from at once — `L_max` halves with cell spacing while
/// the vertex count quadruples — so a grid sized correctly at one level is not
/// thereby sized correctly at another.
///
/// **All four are in the commit gate, and level 7 was measured before it was
/// kept there.** The battery's cost is almost entirely the REFERENCE arm, which
/// is `O(lines x segments)` per probe — level 7 carries 10,949 lines and 54,838
/// vertices against level 6's 3,606 and 14,606 — and the whole four-level sweep
/// runs in ~4.5 s. A `heavy:` ignore would have been the cheaper reflex and the
/// wrong call: the heavy tier is invisible to `make gate`, and this is the one
/// test standing between a coverage bug in the index and a silently different
/// committed world.
const EQUALITY_LEVELS: [u32; 4] = [4, 5, 6, 7];

/// Positions per category per level, past which the sampler stops. The battery
/// is O(positions x lines x segments) in the REFERENCE arm — that is the whole
/// point of it — so the budget is what keeps a level-6 sweep in the commit
/// gate at all.
const CATEGORY_BUDGET: usize = 240;

/// One sampled position and the reason it is in the sample. The label is
/// carried so a failure says which part of the correctness argument broke,
/// rather than printing three unit-vector components and leaving the reader to
/// work out what was special about them.
struct Probe {
    /// The unit position to query.
    at: [f64; 3],
    /// Why this position is in the sample.
    why: &'static str,
}

/// The comparable form of a `nearest_line` answer: the line index and the
/// **bits** of the signed distance.
///
/// Bits rather than the `f64` itself for two reasons, both of which would
/// otherwise weaken the assertion silently: `NaN != NaN` would make a pair of
/// NaN answers compare unequal and a pair of differing NaNs compare... also
/// unequal, but `-0.0 == 0.0` would make a SIGN FLIP at a channel centre
/// compare EQUAL, and the sign is exactly what this battery is protecting.
fn answer_bits(answer: Option<(usize, f64)>) -> Option<(usize, u64)> {
    answer.map(|(i, d)| (i, d.to_bits()))
}

/// Extend the direction `from -> to` past `to` by `t` times its length, back
/// on the sphere. `None` if the two points coincide, which meander
/// displacement can produce.
fn extend_beyond(from: [f64; 3], to: [f64; 3], t: f64) -> Option<[f64; 3]> {
    let step = [to[0] - from[0], to[1] - from[1], to[2] - from[2]];
    if norm(step) == 0.0 {
        return None;
    }
    let p = [
        to[0] + step[0] * t,
        to[1] + step[1] * t,
        to[2] + step[2] * t,
    ];
    if norm(p) == 0.0 {
        return None;
    }
    Some(normalize(p))
}

/// Positions strung out past the two ENDPOINTS of every line, in the
/// line's own direction.
///
/// Present because beyond a segment's endpoints `signed_distance` measures to
/// the nearer *vertex* rather than to the arc, and the sign is a fact about
/// the polyline soup rather than about a river (see
/// `bank_signed_distance`'s own doc). The index must still answer identically
/// out there, and this is the region where the winning line is NOT the line
/// whose bucket the query falls in.
fn beyond_endpoint_probes(net: &ChannelNetwork, budget: usize) -> Vec<Probe> {
    let mut out = Vec::new();
    for line in &net.polylines {
        let n = line.points.len();
        if n < 2 {
            continue;
        }
        for t in [0.25_f64, 1.0, 4.0] {
            for (from, to) in [
                (line.points[n - 2], line.points[n - 1]),
                (line.points[1], line.points[0]),
            ] {
                if out.len() == budget {
                    return out;
                }
                if let Some(at) = extend_beyond(from, to, t) {
                    out.push(Probe {
                        at,
                        why: "beyond a line's endpoint",
                    });
                }
            }
        }
    }
    out
}

/// The two poles, and rings of longitudes at four latitudes closing on the
/// north and south poles.
///
/// Present because The Bearing's index shipped with a near-pole coverage hole
/// and its all-levels equality test is what caught it. A lat/lon bucket grid
/// needs a longitude window that widens as `cos(lat)` shrinks, and the failure
/// mode is a window computed once from a fixed constant instead of per query
/// from the search radius. 89.99 degrees is inside the innermost band of any
/// plausible grid; the poles themselves are the degenerate case where longitude
/// stops being defined at all.
fn polar_probes() -> Vec<Probe> {
    let mut out = vec![
        Probe {
            at: [0.0, 0.0, 1.0],
            why: "the north pole exactly",
        },
        Probe {
            at: [0.0, 0.0, -1.0],
            why: "the south pole exactly",
        },
    ];
    for lat in [89.99_f64, 89.0, 85.0, 75.0] {
        for hemisphere in [1.0_f64, -1.0] {
            for step in 0..12 {
                let lon = -180.0 + f64::from(step) * 30.0;
                out.push(Probe {
                    at: math::unit_sphere_from_lat_lon(hemisphere * lat, lon),
                    why: "a high-latitude ring",
                });
            }
        }
    }
    out
}

/// Positions offset perpendicularly from every vertex across a spread of
/// **ranges**, from a thousandth of a radian out to a tenth.
///
/// This category exists because measurement showed the battery had almost no
/// power without it, and the reason is worth stating precisely. The index
/// decides membership by BUCKET, and the grid's bucket edge is a few
/// hundredths of a radian; `sample_positions_near_channels` offsets by at most
/// 2.5e-3 rad, so every one of its probes sits in the same bucket as the very
/// vertex it was generated from. Such a probe finds its winner however badly
/// the search radius is computed — it is a test of the scan, not of the
/// coverage argument.
///
/// The offsets here deliberately straddle and exceed the bucket edge, so the
/// nearest vertex of the winning line is often one or several buckets away and
/// the query reaches it only if the radius and the window are right.
///
/// **It did not, in the event, catch anything either** — with the `L_max / 2`
/// term deleted from the pruning bound it reports zero disagreements, for the
/// reason `segment_midpoint_probes` records: the bucket neighbourhood a query
/// visits is more generous than the cap it asks for, so shrinking the cap does
/// not shrink what is searched. The category is kept for the region it covers,
/// not for a detection claim it has not earned.
fn ranged_offset_probes(net: &ChannelNetwork, budget: usize) -> Vec<Probe> {
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
            for off in [1.0e-3_f64, 5.0e-3, 1.0e-2, 2.0e-2, 5.0e-2, 1.0e-1] {
                for sign in [1.0_f64, -1.0] {
                    if out.len() == budget {
                        return out;
                    }
                    let step = sign * off;
                    out.push(Probe {
                        at: normalize([
                            base[0] + side[0] * step,
                            base[1] + side[1] * step,
                            base[2] + side[2] * step,
                        ]),
                        why: "offset from a vertex by up to a tenth of a radian",
                    });
                }
            }
        }
    }
    out
}

/// Positions on the perpendicular of the **midpoint** of the network's longest
/// segments, both sides, at a spread of offsets.
///
/// This is the **equality case of the coverage inequality**, and it is the one
/// place in the sample where the pruning bound is the binding constraint rather
/// than a formality. The index gathers lines by the cap `D + L_max / 2`, and
/// the `L / 2` term exists for exactly this configuration: a probe beside the
/// middle of a long arc is at distance `D` from the arc but `~ D + L / 2` from
/// its nearest VERTEX, which is what the grid actually holds. Everywhere else
/// the two are close together and the term is slack.
///
/// The longest segments are chosen because `L_max` is a single network-wide
/// number: a bound error is only visible where `L` is near it. Ordering is by
/// `total_cmp` on the length with the segment's own `(line, vertex)` as the
/// tie-break, so the choice is deterministic rather than dependent on sort
/// stability.
///
/// **Its worth was measured, and the measurement REFUTED the reason it was
/// added.** It was written to sharpen the battery against a shrunken pruning
/// bound, and it did not: with the `L_max / 2` term deleted outright, this
/// category reports zero disagreements at every level, exactly as before. The
/// mechanism is that the grid's bucket edge exceeds `L_max` at every level, so
/// the cap the bound asks for is always smaller than the bucket neighbourhood
/// actually visited — the analytic bound is not the binding constraint for any
/// query near water, and no placement of a near-water probe can make it one.
/// That is why the coverage argument is pinned DIRECTLY, in
/// `channel.rs::the_gather_covers_every_line_with_a_vertex_in_the_cap`, rather
/// than through answers that happen to move.
///
/// It is kept because the region is right even though the current grid makes it
/// slack: this is the one place in the sample where `min(angle(p,a),
/// angle(p,b)) = d + L / 2` is actually attained, so a future grid sized nearer
/// the cap would make it the first category to fire. Do not read its silence as
/// evidence the bound is unimportant — read it as the measured size of the
/// margin the grid gives it.
fn segment_midpoint_probes(net: &ChannelNetwork, budget: usize) -> Vec<Probe> {
    let mut segments: Vec<(f64, usize, usize)> = Vec::new();
    for (i, line) in net.polylines.iter().enumerate() {
        for j in 0..line.points.len().saturating_sub(1) {
            let (a, b) = (line.points[j], line.points[j + 1]);
            let dot = dot(a, b).clamp(-1.0, 1.0);
            segments.push((math::acos(dot), i, j));
        }
    }
    segments.sort_by(|x, y| {
        y.0.total_cmp(&x.0)
            .then_with(|| x.1.cmp(&y.1))
            .then_with(|| x.2.cmp(&y.2))
    });
    let mut out = Vec::new();
    for &(_, i, j) in &segments {
        let (a, b) = (net.polylines[i].points[j], net.polylines[i].points[j + 1]);
        for off in [1.0e-5_f64, 1.0e-3, 1.0e-2] {
            let Some((left, right)) = mirrored_pair_across(a, b, off) else {
                continue;
            };
            for at in [left, right] {
                if out.len() == budget {
                    return out;
                }
                out.push(Probe {
                    at,
                    why: "beside the midpoint of one of the longest segments",
                });
            }
        }
    }
    out
}

/// Positions at and immediately around every confluence — the points where the
/// build's repair pass has placed a tributary's mouth vertex EXACTLY on its
/// trunk's vertex for the shared cell.
///
/// Present because a coincident vertex is the one place two different lines are
/// guaranteed to be exactly equidistant from a probe placed on it, so it is
/// where the `(|d|, index)` tie-break is most likely to be exercised on a real
/// world — and the tie-break is what the index must not move. The offsets
/// straddle it so the neighbourhood is covered as well as the point.
fn confluence_probes(net: &ChannelNetwork, budget: usize) -> Vec<Probe> {
    let mut out = Vec::new();
    for (i, cells) in net.run_cells.iter().enumerate() {
        let Some(&mouth) = cells.last() else {
            continue;
        };
        let Some((trunk, _)) = net.trunk_vertex(mouth) else {
            continue;
        };
        if trunk == i {
            continue;
        }
        let Some(&at) = net.polylines[i].points.last() else {
            continue;
        };
        for off in [0.0_f64, 1.0e-6, 1.0e-4, 1.0e-2] {
            for axis in 0..3 {
                if out.len() == budget {
                    return out;
                }
                let mut p = at;
                p[axis] += off;
                if norm(p) == 0.0 {
                    continue;
                }
                out.push(Probe {
                    at: normalize(p),
                    why: "at or beside a confluence",
                });
            }
        }
    }
    out
}

/// The whole position sample for one world, category by category.
fn equality_probes(terrain: &GeneratedTerrain) -> Vec<Probe> {
    let net = terrain.channels();
    let geo = terrain.geosphere();
    let mut out = Vec::new();
    // Near the water: the ordinary case, and the one the index is optimised
    // for — a small search radius and a handful of candidates.
    for at in sample_positions_near_channels(net, CATEGORY_BUDGET) {
        out.push(Probe {
            at,
            why: "beside a channel",
        });
    }
    // Cell centres, strided across the whole globe. This is where the sample
    // gets its FAR-FROM-NETWORK positions — mid-ocean, deep desert — which the
    // near-channel sampler cannot produce and which are the only positions
    // that exercise the search radius growing past its first guess.
    let stride = (geo.cell_count() / CATEGORY_BUDGET).max(1);
    for c in geo.cells().step_by(stride) {
        out.push(Probe {
            at: geo.position(c),
            why: "a cell centre",
        });
    }
    out.extend(beyond_endpoint_probes(net, CATEGORY_BUDGET));
    out.extend(ranged_offset_probes(net, CATEGORY_BUDGET));
    out.extend(segment_midpoint_probes(net, CATEGORY_BUDGET));
    out.extend(polar_probes());
    out.extend(confluence_probes(net, CATEGORY_BUDGET));
    out
}

/// Assert the indexed `nearest_line` and the unindexed `nearest_line_reference`
/// return the identical `Option<(usize, f64)>` — same line index, bit-equal
/// distance — at every probe, and return how many probes there were.
fn assert_index_equals_reference(terrain: &GeneratedTerrain, level: u32) -> usize {
    let net = terrain.channels();
    assert!(
        !net.polylines.is_empty(),
        "level {level} produced no channels at all, so the comparison below is vacuous"
    );
    let probes = equality_probes(terrain);
    for probe in &probes {
        let indexed = net.nearest_line(probe.at);
        let reference = net.nearest_line_reference_for_test(probe.at);
        assert_eq!(
            answer_bits(indexed),
            answer_bits(reference),
            "level {level}: the index and the reference scan disagree at {:?} ({}) — \
             indexed {indexed:?}, reference {reference:?}",
            probe.at,
            probe.why
        );
    }
    probes.len()
}

/// THE KEYSTONE'S GUARD. The indexed `nearest_line` answers exactly what the
/// linear scan answers, on real worlds, at every level the pins admit.
///
/// The full return value is compared, not the index alone and not an
/// approximate distance: the `f64` is bit-equal because the index changes only
/// WHICH lines are evaluated, never HOW — `signed_distance` is called
/// unchanged, on the same inputs, so a different number would mean the winner
/// itself had changed.
///
/// **What this test can and cannot see.** It is the CONTRACT — the indexed
/// answer equals the unindexed one — and that is the assertion that matters,
/// because it is the answer, not the mechanism, that reaches a serialized sign.
/// But it can only notice a coverage bug that actually changes an answer, and
/// that is measured to be a thin margin: with the `L_max / 2` term deleted from
/// the pruning bound outright, exactly **one probe in 6,175** disagreed. The
/// mechanism is therefore pinned directly and separately, in
/// `channel.rs::the_gather_covers_every_line_with_a_vertex_in_the_cap`, which
/// asserts the coverage property at radii no world's queries reach and reddens
/// on window and band mutations this test sleeps through. Read the two as one
/// guard, and do not weaken either on the strength of the other being green.
///
/// The sample deliberately spans the regions the coverage argument has to
/// survive: beside a channel (the common case), at a cell centre far from any
/// (the search radius grows past its first guess), beyond a line's endpoints
/// (the winner is not the line whose bucket the query is in), at the poles
/// (where a longitude window must widen without bound), and at a confluence
/// (where two lines are exactly equidistant and the tie-break decides a
/// serialized sign). Each carries its label into the failure message.
#[test]
fn the_indexed_nearest_line_equals_the_linear_scan() {
    let mut total = 0usize;
    for level in EQUALITY_LEVELS {
        let geo = Geosphere::new(level);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo, outcome);
        let net = terrain.channels();
        let probed = assert_index_equals_reference(&terrain, level);
        println!(
            "level {level}: {probed} probes agreed, on {} lines / {} vertices",
            net.polylines.len(),
            net.polylines.iter().map(|l| l.points.len()).sum::<usize>(),
        );
        total += probed;
    }
    assert!(
        total >= 1_500,
        "only {total} probes across levels {EQUALITY_LEVELS:?} — the sampler collapsed and \
         the equality above ran on almost nothing"
    );
}
