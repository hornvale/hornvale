//! `prevalence` must be position-continuous, never address-hashed — The
//! Weft, Task 5; spec §5.1.
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

/// Adjacent facets mostly agree, because prevalence is position-continuous.
///
/// **Why this discriminates an address-hashed implementation, measured, not
/// assumed.** During this task, `prevalence`'s noise sample was temporarily
/// mutated in place — `facet.centroid()` swapped for
/// `facet.seed(seed).stream().next_f64()`, everything else (the real,
/// varying macro-state term; `SPRING_CONTEXTUALITY`; `SPRING_ABUNDANCE`)
/// left untouched — and this exact test re-run against it. Real mechanism:
/// max delta `~0.0025` over 200 steps at each of 6 seed-42 starting
/// vertices, zero steps over the bound below. Mutated mechanism, same six
/// starts: max delta `~0.10`, 119-134 of 199 steps (60-67%) over the SAME
/// bound. `SPRING_CONTEXTUALITY`'s own doc in `kinds.rs` records the first
/// value tried (`0.85`) failing to separate these two cases at all — the
/// mutation's max delta came out `~0.049`, indistinguishable from real
/// texture — and why `0.7` was chosen instead. The bound here sits between
/// the two measured maxima with an 8x margin on the real side and a 3x
/// margin on the mutated side, not merely above the real side.
#[test]
fn prevalence_is_continuous_across_adjacent_facets() {
    const MAX_ALLOWED_DELTA: f64 = 0.02;
    const STEPS: usize = 200;

    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let pack = hornvale_worldgen::field_pack_from(&terrain);
    let geo = terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let walk_depth = geo.depth() + WALK_DEPTH_BELOW_GRID;

    // Walk a straight line of edge-adjacent facets (`neighbors()[0]` is
    // always the "+a" edge step — `Facet::neighbor_steps`'s own doc: "the
    // first four neighbours are always the four edge-adjacent rooms", present
    // even at a cube corner, so this index never panics).
    let mut facet = Facet::containing(geo.position(Vertex(0)), walk_depth);
    let mut prevalences = Vec::with_capacity(STEPS);
    for _ in 0..STEPS {
        let p =
            hornvale_worldgen::prevalence(WeftKind::Spring, &facet, geo, &index, &pack, world.seed)
                .expect("a walk-depth facet is always deeper than the geosphere's own level");
        prevalences.push(p);
        facet = facet
            .neighbors()
            .into_iter()
            .next()
            .expect("a facet always has an edge neighbour");
    }

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
