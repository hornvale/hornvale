//! `prevalence` must be position-continuous, never address-hashed — The
//! Weft, Task 5; spec §5.1. Paired with an anti-vacuity companion (fix round
//! 1, F3) per spec §7's H2 requirement that coherence be "paired with an
//! anti-vacuity companion … so a degenerate world cannot score perfectly" —
//! the same shape The Ford pairs `channel-band-monotonicity` with
//! `channel-transect-dry-reach`. Task 7 adds the eligibility regression
//! `spring_never_occurs_off_land` (controller ruling R1) and, with it, the
//! remaining three kinds (each growing `KIND_BOUNDS` below by one commit).
//!
//! **The fixed walk moved from vertex 0 to vertex 14 (Task 7).** Task 7's
//! eligibility gate (R1 — see `weft::kinds`'s own module doc) makes
//! ineligible ground a hard `prevalence == 0.0` cliff, a REAL geographic
//! discontinuity (a coastline), not decorrelated noise. Vertex 0's own
//! 200-step walk crosses one; conflating that cliff with the continuous-
//! noise-smoothness property this file tests would either mask a real
//! address-hashing regression (bound raised to swallow the cliff) or
//! false-positive on every run (bound left tight). Vertex 14's own 200-step
//! walk stays land-eligible throughout. The eligibility cliff itself is
//! exercised directly by `spring_never_occurs_off_land` below, over the
//! WHOLE grid rather than one walk.
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{Facet, NearestVertexIndex, Vertex, blend_corner_weights};
use hornvale_worldgen::WeftKind;

/// Walk depth used everywhere in this file: `windows/locale::walk_depth`
/// documents its own value as "the globe level plus 7"
/// (`windows/worldgen/src/placement.rs` restates the same relationship at its
/// own call site); this file cannot import `windows/locale` at all (it
/// depends on `hornvale-worldgen`, the crate under test — the same
/// circularity Ruling 1 of this task's dispatch names), so the relationship
/// is reproduced directly rather than imported.
const WALK_DEPTH_BELOW_GRID: u32 = 7;

/// The fixed walk's starting vertex (Task 7) — see this file's own module
/// doc for why it moved off vertex 0.
const WALK_START: u32 = 14;

/// Walk `STEPS` edge-adjacent facets from [`WALK_START`] (`neighbors()[0]` is
/// always the "+a" edge step — `Facet::neighbor_steps`'s own doc: "the first
/// four neighbours are always the four edge-adjacent rooms", present even at
/// a cube corner, so this index never panics), returning each step's
/// `prevalence` and whether `occurs` fired there. Shared by every test in
/// this file so the coherence check and its anti-vacuity companion measure
/// literally the same walk, per kind.
fn walk_kind(kind: WeftKind, steps: usize) -> Vec<(f64, bool)> {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let pack = hornvale_worldgen::field_pack_from(&terrain);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    let mut facet = Facet::containing(geo.position(Vertex(WALK_START)), walk_depth);
    let mut out = Vec::with_capacity(steps);
    for _ in 0..steps {
        let p = hornvale_worldgen::prevalence(kind, &facet, geo, &index, &pack, world.seed)
            .expect("a walk-depth facet is always deeper than the geosphere's own level");
        let occ = hornvale_worldgen::occurs(kind, &facet, world.seed, p);
        out.push((p, occ));
        facet = facet
            .neighbors()
            .into_iter()
            .next()
            .expect("a facet always has an edge neighbour");
    }
    out
}

/// Per-kind `(kind, max_allowed_delta, min_prevalence_spread,
/// min_occurs_over_200)` — measured over [`WALK_START`]'s own 200-step walk,
/// current mechanism, and asserted here at thresholds with margin over the
/// measured value. Spring/seep: measured max delta `0.00385`, spread
/// `0.04664`, occurs `10`/200 — bound `0.02` sits ~5.2x over the measured
/// max. Grows by one row per kind Task 7 adds.
const KIND_BOUNDS: [(WeftKind, f64, f64, usize); 1] = [(WeftKind::Spring, 0.02, 0.01, 1)];

/// Adjacent facets mostly agree, because prevalence is position-continuous —
/// checked for every kind in [`KIND_BOUNDS`].
///
/// **Why this discriminates an address-hashed implementation, measured, not
/// assumed.** During Task 5, `prevalence`'s noise sample was temporarily
/// mutated in place — `facet.centroid()` swapped for
/// `facet.seed(seed).stream().next_f64()`, everything else untouched — and
/// this exact shape of test re-run against it.
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
/// uniform).
///
/// **Task 7 moved the walk from vertex 0 to [`WALK_START`]** — see this
/// file's own module doc for why (the eligibility cliff R1 introduced is a
/// real discontinuity this test must not conflate with noise smoothness).
#[test]
fn prevalence_is_continuous_across_adjacent_facets() {
    const STEPS: usize = 200;

    for (kind, max_allowed_delta, _, _) in KIND_BOUNDS {
        let walk = walk_kind(kind, STEPS);
        let prevalences: Vec<f64> = walk.iter().map(|(p, _)| *p).collect();

        let mut violations = Vec::new();
        for i in 1..prevalences.len() {
            let delta = (prevalences[i] - prevalences[i - 1]).abs();
            if delta > max_allowed_delta {
                violations.push((i, delta));
            }
        }
        assert!(
            violations.is_empty(),
            "{kind:?}: prevalence must stay position-continuous across adjacent facets; \
             {} of {} steps exceeded {max_allowed_delta}: {violations:?}\nfull series: {prevalences:?}",
            violations.len(),
            prevalences.len() - 1,
        );
    }
}

/// The anti-vacuity companion spec §7's H2 requires (F3, fix round 1):
/// `prevalence_is_continuous_across_adjacent_facets` alone passes for a
/// **constant** function — `violations.is_empty()` on a flat series is
/// vacuously true. Checked for every kind in [`KIND_BOUNDS`], over the same
/// walk the coherence test above uses, so a reviewer can hold both results
/// against literally the same series.
#[test]
fn the_walk_is_not_degenerate() {
    const STEPS: usize = 200;

    for (kind, _, min_spread, min_occurs) in KIND_BOUNDS {
        let walk = walk_kind(kind, STEPS);
        let prevalences: Vec<f64> = walk.iter().map(|(p, _)| *p).collect();
        let min = prevalences.iter().cloned().fold(f64::INFINITY, f64::min);
        let max = prevalences
            .iter()
            .cloned()
            .fold(f64::NEG_INFINITY, f64::max);
        let spread = max - min;
        assert!(
            spread >= min_spread,
            "{kind:?}: prevalence must actually vary over the walk, not sit near a constant \
             floor; spread was {spread:.5} (min {min:.5}, max {max:.5}), wanted >= {min_spread}"
        );

        let occurs_count = walk.iter().filter(|(_, occ)| *occ).count();
        assert!(
            occurs_count >= min_occurs,
            "{kind:?}: occurs must fire at least {min_occurs} time(s) over a 200-facet walk of \
             real terrain; it fired {occurs_count} times — a mechanism that never fires \
             produces an empty surface regardless of how smoothly prevalence behaves"
        );
    }
}

/// The eligibility regression (Task 7, controller ruling R1) — the
/// deliverable that number is, not the code: Task 5's review measured **59%
/// of all spring occurrences landing on facets with no macro cause at all,
/// including open ocean** (654 of 1109, over every seed-42 walk-depth facet
/// across all 40,962 vertices; `zero-macro` meaning `spring_macro_state ==
/// 0.0`, dominated by the ocean's 27,645-vertex zero-macro population). This
/// test reproduces the identical measurement — same population, same
/// definition of "causeless" (recomputed from `blend_corner_weights` over
/// `pack.carbonate`/`pack.drainage`, mirroring spring/seep's own
/// `pub(crate)` recipe exactly, rather than reading it directly, the same
/// posture Task 5's re-review probe used) — against the CURRENT (post-fix)
/// mechanism, and asserts it stays near zero rather than merely printing a
/// number a human might forget to check.
///
/// **Measured here: `0` of `403` occurrences are causeless** (seed 42, full
/// grid) — down from 654 of 1109 (58.97%). The asserted bound (`5%`) leaves
/// headroom for a legitimately land-based zero-macro occurrence (bare rock
/// with no drainage still has a nonzero floor via `(1 - contextuality) *
/// noise`; Task 5's review measured that land-only slice at `0.58%` of land,
/// contributing ~1 of the 654) while still catching a regression that
/// reopens the ocean case wholesale.
#[test]
fn spring_never_occurs_off_land() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let pack = hornvale_worldgen::field_pack_from(&terrain);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;
    let n = geo.vertex_count();

    /// Spring/seep's own macro-state recipe, recomputed here rather than
    /// read from the crate's `pub(crate)` `spring_macro_state` (this file is
    /// an external integration-test crate and cannot see it) — mirrors
    /// `kinds.rs`'s `spring_macro_state` exactly: `SPRING_DRAINAGE_SATURATION
    /// = 12.0`.
    fn spring_macro(weights: [(Vertex, u64); 4], pack: &hornvale_worldgen::FieldPack) -> f64 {
        let carbonate = blend_corner_weights(weights, &pack.carbonate);
        let drainage = blend_corner_weights(weights, &pack.drainage);
        (carbonate * (drainage / 12.0).tanh()).clamp(0.0, 1.0)
    }

    let mut occurs_n = 0usize;
    let mut causeless_occurs_n = 0usize;
    let mut off_land_occurs_n = 0usize;
    for v in 0..n {
        let facet = Facet::containing(geo.position(Vertex(v as u32)), walk_depth);
        let weights = facet
            .corner_weights(geo, &index)
            .expect("a level-0 walk-depth facet always has corner weights");
        let land = blend_corner_weights(weights, &pack.land);
        let macro_state = spring_macro(weights, &pack);

        let p =
            hornvale_worldgen::prevalence(WeftKind::Spring, &facet, geo, &index, &pack, world.seed)
                .expect("a level-0 walk-depth facet always has corner weights");
        let occ = hornvale_worldgen::occurs(WeftKind::Spring, &facet, world.seed, p);
        if occ {
            occurs_n += 1;
            if macro_state == 0.0 {
                causeless_occurs_n += 1;
            }
            if land < 0.5 {
                off_land_occurs_n += 1;
            }
        }
    }

    assert!(
        occurs_n > 0,
        "spring must occur somewhere over the full seed-42 grid, or this test measures nothing"
    );
    assert_eq!(
        off_land_occurs_n, 0,
        "spring must never occur on ineligible (majority-ocean) ground — R1's whole point; \
         {off_land_occurs_n} of {occurs_n} occurrences did"
    );

    let causeless_share = causeless_occurs_n as f64 / occurs_n as f64;
    assert!(
        causeless_share <= 0.05,
        "causeless-occurrence share must stay near zero (measured 0/403 at fix time); \
         got {causeless_occurs_n}/{occurs_n} = {:.4}% (bound 5%)",
        causeless_share * 100.0
    );
}
