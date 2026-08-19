//! The per-approach defensibility field (spec §2.3): strictly monotone in
//! route cost, bounded in `[DEF_MIN, DEF_MAX)` with `DEF_MIN` ATTAINED at a
//! free route, parallel edges resolved by MAXIMUM conductance, and a pure
//! function of the graph.

use hornvale_kernel::CellId;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use hornvale_worldgen::defensibility_for_test as defensibility;
use hornvale_worldgen::weakest_point_defensibility;

fn link(conductances: &[(EdgeKind, f64)]) -> ConnectionGraph {
    let mut g = ConnectionGraph::new(2);
    for &(kind, c) in conductances {
        g.add_edge(
            CellId(0),
            Edge {
                to: CellId(1),
                kind,
                conductance: c,
            },
        );
    }
    g
}

#[test]
fn defensibility_rises_strictly_as_the_route_gets_dearer() {
    let mut prev = f64::NEG_INFINITY;
    for step in 1..40 {
        let c = 1.0 / (step as f64);
        let d = defensibility(&link(&[(EdgeKind::Adjacency, c)]), CellId(0), CellId(1));
        assert!(
            d > prev,
            "must rise strictly as conductance falls, at step {step}"
        );
        prev = d;
    }
}

#[test]
fn the_bounds_are_approached_and_never_attained() {
    // A free sea lane is the most exposed ground there is and sits AT the
    // floor to six places — but not exactly, because `tanh` is asymptotic in
    // both directions and DEF_CENTER is finite. Strict interiority, with the
    // approach pinned to a stated tolerance.
    let free = defensibility(&link(&[(EdgeKind::WaterRoute, 1.0)]), CellId(0), CellId(1));
    assert!(free > 0.75, "strictly above DEF_MIN: got {free}");
    assert!(free < 0.75 + 1.0e-5, "within 1e-5 of DEF_MIN: got {free}");

    let dear = defensibility(
        &link(&[(EdgeKind::Adjacency, 1.0e-9)]),
        CellId(0),
        CellId(1),
    );
    assert!(dear < 1.25, "strictly below DEF_MAX: got {dear}");
    assert!(dear > 1.25 - 1.0e-5, "within 1e-5 of DEF_MAX: got {dear}");
}

#[test]
fn the_median_approach_is_exactly_neutral() {
    // THE centring property, and the one the pre-amendment form claimed and
    // did not have (it put the median at 0.905). An approach whose cost
    // exponent equals DEF_CENTER must map to exactly 1.0, so the median world
    // is genuinely unchanged and only the extremes of the terrain move.
    // exp(-6.256709) — the conductance whose -ln IS DEF_CENTER.
    let at_center = 0.0019175460679594725_f64;
    let d = defensibility(
        &link(&[(EdgeKind::Adjacency, at_center)]),
        CellId(0),
        CellId(1),
    );
    assert!(
        (d - 1.0).abs() < 1.0e-9,
        "the median approach must be neutral: got {d}"
    );
}

#[test]
fn parallel_edges_resolve_by_maximum_conductance() {
    // 6.7% of real cells carry an Adjacency AND a LandRoute to the same
    // neighbour (Task 2b). An attacker uses the EASIEST road, so the max wins.
    // A `min` would over-defend and a `sum` would double-count.
    let both = link(&[(EdgeKind::Adjacency, 0.001), (EdgeKind::LandRoute, 0.02)]);
    let only_easy = link(&[(EdgeKind::LandRoute, 0.02)]);
    assert_eq!(
        defensibility(&both, CellId(0), CellId(1)),
        defensibility(&only_easy, CellId(0), CellId(1)),
        "the easiest parallel route must decide"
    );
}

#[test]
fn defensibility_is_deterministic_across_recomputation() {
    let g = link(&[(EdgeKind::Adjacency, 0.0031)]);
    let first = defensibility(&g, CellId(0), CellId(1));
    for _ in 0..8 {
        assert_eq!(defensibility(&g, CellId(0), CellId(1)), first);
    }
}

// --- The Contour (Task 4, round 2): the per-cell weakest-point view. ---

#[test]
fn weakest_point_defensibility_is_the_minimum_over_distinct_approaches() {
    // A 3-node star: cell 0 approaches hub 2 by an easy water lane (low
    // defensibility), cell 1 approaches the SAME hub by a dear, near-
    // impassable adjacency (high defensibility). The weakest point is the
    // EASY approach — the minimum, not the maximum or a blend of the two.
    let mut g = ConnectionGraph::new(3);
    g.add_edge(
        CellId(0),
        Edge {
            to: CellId(2),
            kind: EdgeKind::WaterRoute,
            conductance: 1.0,
        },
    );
    g.add_edge(
        CellId(1),
        Edge {
            to: CellId(2),
            kind: EdgeKind::Adjacency,
            conductance: 1.0e-9,
        },
    );

    let via_0 = defensibility(&g, CellId(0), CellId(2));
    let via_1 = defensibility(&g, CellId(1), CellId(2));
    assert!(
        via_0 < via_1,
        "test setup: approach 0 must be easier (less defensible) than approach 1"
    );

    let weakest = weakest_point_defensibility(&g, CellId(2));
    assert_eq!(
        weakest, via_0,
        "the weakest point is the MINIMUM (easiest) approach, not the dear one"
    );
}

#[test]
fn weakest_point_defensibility_ignores_a_parallel_easier_route_between_the_same_pair() {
    // Two neighbours, 0 and 1, both reach hub 2. Neighbour 0 additionally
    // carries a second, easier PARALLEL route to 2 (mirroring the 6.7% of
    // real cells with an Adjacency+LandRoute pair). `defensibility` already
    // resolves that parallel pair by MAXIMUM conductance (the easier of the
    // two roads to 0 wins); this view must then take the MINIMUM across the
    // two DISTINCT neighbours 0 and 1, not re-apply a minimum inside the
    // parallel pair too.
    let mut g = ConnectionGraph::new(3);
    g.add_edge(
        CellId(0),
        Edge {
            to: CellId(2),
            kind: EdgeKind::Adjacency,
            conductance: 1.0e-9,
        },
    );
    g.add_edge(
        CellId(0),
        Edge {
            to: CellId(2),
            kind: EdgeKind::WaterRoute,
            conductance: 1.0,
        },
    );
    g.add_edge(
        CellId(1),
        Edge {
            to: CellId(2),
            kind: EdgeKind::Adjacency,
            conductance: 0.5,
        },
    );

    let via_0 = defensibility(&g, CellId(0), CellId(2));
    let via_1 = defensibility(&g, CellId(1), CellId(2));
    assert_eq!(weakest_point_defensibility(&g, CellId(2)), via_0.min(via_1));
}

#[test]
fn weakest_point_defensibility_is_def_max_with_no_traversable_approach() {
    // An isolated cell (no edges at all) reads exactly as maximally defended
    // as `defensibility` itself reads a nonexistent link — the same ceiling,
    // read without hard-coding its value here.
    let isolated = ConnectionGraph::new(1);
    let no_edge_at_all = link(&[]);
    assert_eq!(
        weakest_point_defensibility(&isolated, CellId(0)),
        defensibility(&no_edge_at_all, CellId(0), CellId(1)),
        "an unreachable cell must read as maximally (vacuously) defended"
    );
}
