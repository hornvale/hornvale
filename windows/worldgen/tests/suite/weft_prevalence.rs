//! `prevalence` must be position-continuous, never address-hashed — The
//! Weft, Task 5; spec §5.1. Paired with an anti-vacuity companion (fix round
//! 1, F3) per spec §7's H2 requirement that coherence be "paired with an
//! anti-vacuity companion … so a degenerate world cannot score perfectly" —
//! the same shape The Ford pairs `channel-band-monotonicity` with
//! `channel-transect-dry-reach`.
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Facet, NearestVertexIndex, Vertex};
use hornvale_worldgen::WeftKind;

/// Walk depth used everywhere in this file: `windows/locale::walk_depth`
/// documents its own value as "the globe level plus 7"
/// (`windows/worldgen/src/placement.rs` restates the same relationship at its
/// own call site); this file cannot import `windows/locale` at all (it
/// depends on `hornvale-worldgen`, the crate under test — the same
/// circularity Ruling 1 of this task's dispatch names), so the relationship
/// is reproduced directly rather than imported.
const WALK_DEPTH_BELOW_GRID: u32 = 7;

/// Walk `STEPS` edge-adjacent facets from vertex 0 (`neighbors()[0]` is
/// always the "+a" edge step — `Facet::neighbor_steps`'s own doc: "the first
/// four neighbours are always the four edge-adjacent rooms", present even at
/// a cube corner, so this index never panics), returning each step's
/// `prevalence` and whether `occurs` fired there. Shared by both tests in
/// this file so the coherence check and its anti-vacuity companion measure
/// literally the same walk.
fn walk_spring(steps: usize) -> Vec<(f64, bool)> {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let pack = hornvale_worldgen::field_pack_from(&terrain);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    let mut facet = Facet::containing(geo.position(Vertex(0)), walk_depth);
    let mut out = Vec::with_capacity(steps);
    for _ in 0..steps {
        let p =
            hornvale_worldgen::prevalence(WeftKind::Spring, &facet, geo, &index, &pack, world.seed)
                .expect("a walk-depth facet is always deeper than the geosphere's own level");
        let occ = hornvale_worldgen::occurs(WeftKind::Spring, &facet, world.seed, p);
        out.push((p, occ));
        facet = facet
            .neighbors()
            .into_iter()
            .next()
            .expect("a facet always has an edge neighbour");
    }
    out
}

/// Adjacent facets mostly agree, because prevalence is position-continuous.
///
/// **Why this discriminates an address-hashed implementation, measured, not
/// assumed — and re-measured after fix round 1 changed the mechanism under
/// it.** During Task 5, `prevalence`'s noise sample was temporarily mutated
/// in place — `facet.centroid()` swapped for
/// `facet.seed(seed).stream().next_f64()`, everything else untouched — and
/// this exact test re-run against it.
///
/// **Round 0 measured `SPRING_CONTEXTUALITY = 0.7` against a raw (non-
/// uniformized) noise field: real max delta `~0.0025`, mutant max `~0.10`.**
/// Review found this comparison unsound: at the round-0 shipped bound
/// (`0.02`), the ORIGINAL `0.85` also discriminates (mutant: 64 of 199 steps
/// over bound) — moving `SPRING_CONTEXTUALITY` bought no additional
/// separation (42.6× at 0.85 vs 38.7× at 0.7, scale-invariant since lowering
/// contextuality scales both sides identically); the defect was a bound set
/// too loose, not the constant. `SPRING_CONTEXTUALITY` is restored to `0.85`
/// (see `kinds.rs`).
///
/// **Round 1 also fixed F1: every noise sample is now passed through
/// [`hornvale_terrain::features::uniformize`]** before use, which widens the
/// noise term's own variance (from SD ≈0.076 raw to a genuine `[0,1]`
/// uniform), so both the real mechanism's per-step deltas and the mutant's
/// grew versus round 0's numbers — re-measured against the FINAL
/// (uniformized, `c=0.85`) mechanism, 6 seed-42 starting vertices, 200 steps
/// each: real max delta `0.00677`, mutant max delta `0.0527` (94-110 of 199
/// steps over the bound below at every start). The bound is set at `0.02` —
/// a 2.9× margin below the real mechanism's measured max and comfortably
/// under the mutant's typical violation range, not merely "somewhere above
/// the real number".
#[test]
fn prevalence_is_continuous_across_adjacent_facets() {
    const MAX_ALLOWED_DELTA: f64 = 0.02;
    const STEPS: usize = 200;

    let walk = walk_spring(STEPS);
    let prevalences: Vec<f64> = walk.iter().map(|(p, _)| *p).collect();

    let mut violations = Vec::new();
    for i in 1..prevalences.len() {
        let delta = (prevalences[i] - prevalences[i - 1]).abs();
        if delta > MAX_ALLOWED_DELTA {
            violations.push((i, delta));
        }
    }
    assert!(
        violations.is_empty(),
        "prevalence must stay position-continuous across adjacent facets; \
         {} of {} steps exceeded {MAX_ALLOWED_DELTA}: {violations:?}\nfull series: {prevalences:?}",
        violations.len(),
        prevalences.len() - 1,
    );
}

/// The anti-vacuity companion spec §7's H2 requires (F3, fix round 1):
/// `prevalence_is_continuous_across_adjacent_facets` alone passes for a
/// **constant** function — `violations.is_empty()` on a flat series is
/// vacuously true — and the shipped field came uncomfortably close to one
/// before this round's F1 fix (`occurs` fired **zero** times in 21,640
/// seed-42 walk facets; the review measured `prevalence` sitting near a
/// near-constant floor almost everywhere). A continuity test over a constant
/// field is not evidence of continuity; this test is the floor that makes it
/// evidence.
///
/// Same walk as the coherence test (same starting vertex, same step count),
/// so a reviewer can hold both results against literally the same series.
/// Measured on the current (post-fix) mechanism: `prevalence` over this walk
/// spans `[0.00071, 0.04641]` (range `0.0457`) and `occurs` fires `7` of 200
/// times — asserted here at thresholds well inside those measurements, so a
/// future regression that flattens the field or silences occurrence fails
/// loudly rather than being caught only by eye.
#[test]
fn the_walk_is_not_degenerate() {
    const STEPS: usize = 200;
    const MIN_PREVALENCE_SPREAD: f64 = 0.01;

    let walk = walk_spring(STEPS);
    let prevalences: Vec<f64> = walk.iter().map(|(p, _)| *p).collect();
    let min = prevalences.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = prevalences
        .iter()
        .cloned()
        .fold(f64::NEG_INFINITY, f64::max);
    let spread = max - min;
    assert!(
        spread >= MIN_PREVALENCE_SPREAD,
        "prevalence must actually vary over the walk, not sit near a constant \
         floor; spread was {spread:.5} (min {min:.5}, max {max:.5}), wanted >= {MIN_PREVALENCE_SPREAD}"
    );

    let occurs_count = walk.iter().filter(|(_, occ)| *occ).count();
    assert!(
        occurs_count >= 1,
        "occurs must fire at least once over a 200-facet walk of real terrain; \
         it fired {occurs_count} times — a mechanism that never fires produces \
         an empty surface regardless of how smoothly prevalence behaves"
    );
}
