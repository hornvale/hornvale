//! Integration tests for `least_cost`: least-cost cell routing over the
//! kernel's deterministic A* (`hornvale_kernel::astar`).
//!
//! The test substrate is a real `Geosphere` (there is no way to hand-build a
//! toy adjacency graph — `Geosphere`'s fields are private and its topology is
//! always a subdivided icosahedron), so the "cheap corridor vs. expensive
//! wall" scenario is built by choosing a cost field over `Geosphere::new(1)`
//! (42 cells) rather than a hand-drawn grid. `Geosphere::new(1)` is fully
//! deterministic (fixed subdivision of the base icosahedron), so the cell ids
//! used below are stable across runs.

use hornvale_kernel::{Geosphere, Vertex, VertexMap};
use hornvale_topology::least_cost;

/// The two direct 2-hop cells between `Vertex(0)` and `Vertex(21)` on
/// `Geosphere::new(1)` are `Vertex(12)` and `Vertex(20)` — `0`'s and `21`'s
/// neighbor lists intersect only at those two cells. A cost field that makes
/// both very expensive (the "wall") forces `least_cost` onto a longer,
/// cheaper corridor through cells left at the default cost of 1.
fn wall_and_corridor_cost(geo: &Geosphere) -> VertexMap<u64> {
    VertexMap::from_fn(geo, |id| match id {
        Vertex(12) | Vertex(20) => 100,
        _ => 1,
    })
}

#[test]
fn least_cost_picks_the_cheap_corridor_over_the_expensive_wall() {
    let geo = Geosphere::new(1);
    let cost = wall_and_corridor_cost(&geo);
    let (path, total) = least_cost(&geo, &cost, Vertex(0), Vertex(21), 1000)
        .expect("a corridor around the wall exists");

    // The only 2-hop routes (through 12 or 20) cost 100 + 1 = 101; the
    // cheapest route avoiding the wall costs 4 (four cells at cost 1 each).
    // If least_cost took "any" path rather than the CHEAPEST, it would
    // return 101, not 4.
    assert_eq!(
        total, 4,
        "expected the cheap corridor's cost, not the wall's"
    );
    assert!(
        !path.contains(&Vertex(12)) && !path.contains(&Vertex(20)),
        "path {path:?} routed through the expensive wall instead of around it"
    );
    assert_eq!(path.first(), Some(&Vertex(0)));
    assert_eq!(path.last(), Some(&Vertex(21)));
}

#[test]
fn least_cost_reports_the_correct_total_cost_along_the_returned_path() {
    let geo = Geosphere::new(1);
    let cost = wall_and_corridor_cost(&geo);
    let (path, total) =
        least_cost(&geo, &cost, Vertex(0), Vertex(21), 1000).expect("reachable within budget");

    // Recompute the cost independently by summing the per-cell cost of every
    // cell entered after `from` — this must match astar's own tally exactly.
    let recomputed: u64 = path[1..].iter().map(|&c| *cost.get(c)).sum();
    assert_eq!(total, recomputed);
}

#[test]
fn least_cost_returns_none_when_the_budget_is_too_small() {
    let geo = Geosphere::new(1);
    let cost = wall_and_corridor_cost(&geo);
    // Vertex(21) is several hops from Vertex(0); a budget of 1 node
    // expansion cannot reach it.
    assert_eq!(least_cost(&geo, &cost, Vertex(0), Vertex(21), 1), None);
}

#[test]
fn least_cost_is_deterministic_across_repeated_calls() {
    let geo = Geosphere::new(1);
    let cost = wall_and_corridor_cost(&geo);
    let first = least_cost(&geo, &cost, Vertex(0), Vertex(21), 1000);
    for _ in 0..25 {
        assert_eq!(
            least_cost(&geo, &cost, Vertex(0), Vertex(21), 1000),
            first,
            "identical inputs must yield an identical path and cost every run"
        );
    }
}

#[test]
fn least_cost_of_a_cell_to_itself_is_the_trivial_zero_cost_path() {
    let geo = Geosphere::new(1);
    let cost = wall_and_corridor_cost(&geo);
    let (path, total) =
        least_cost(&geo, &cost, Vertex(5), Vertex(5), 0).expect("start is already the goal");
    assert_eq!(path, vec![Vertex(5)]);
    assert_eq!(total, 0);
}

#[test]
fn least_cost_skips_impassable_cells_marked_with_u64_max() {
    // Make the ONLY route (via Vertex(12) or Vertex(20)) impassable rather
    // than merely expensive, and confirm the search still finds the
    // 4-hop corridor without overflowing the cost summation.
    let geo = Geosphere::new(1);
    let cost = VertexMap::from_fn(&geo, |id| match id {
        Vertex(12) | Vertex(20) => u64::MAX,
        _ => 1,
    });
    let (path, total) = least_cost(&geo, &cost, Vertex(0), Vertex(21), 1000)
        .expect("a corridor avoiding the impassable cells exists");
    assert_eq!(total, 4);
    assert!(!path.contains(&Vertex(12)) && !path.contains(&Vertex(20)));
}
