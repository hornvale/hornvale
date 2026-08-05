//! Least-cost cell routing: a `SearchSpace` over a `Geosphere`'s cell
//! adjacency, weighted by a per-cell traversal cost, solved by the kernel's
//! deterministic `astar`. This is the pathfinder later domains derive
//! natural land routes over (a terrain cost field is Task 3; deriving
//! `ConnectionGraph` edges from it is Task 4) — this module only finds the
//! least-cost path between two cells.

use hornvale_kernel::{CellId, CellMap, Geosphere, SearchSpace, astar};
use std::collections::BTreeSet;

/// A least-cost search space over `geo`'s cell adjacency: each step from a
/// cell to one of its neighbors costs `cost`'s value for the destination
/// cell, and the goal is reaching `goal`.
///
/// Impassable cells are represented by a per-cell cost of `u64::MAX`; such a
/// neighbor is skipped entirely in `successors` rather than treated as an
/// ordinary (very expensive) step. This both encodes "cannot enter" and
/// guards the running cost total against overflow — no path this search
/// space returns ever sums a `u64::MAX` step cost.
pub struct CellRoute<'a> {
    geo: &'a Geosphere,
    cost: &'a CellMap<u64>,
    goal: CellId,
}

impl<'a> CellRoute<'a> {
    /// Build a search space over `geo`'s adjacency, weighted by `cost`,
    /// aimed at `goal`.
    /// type-audit: bare-ok(count: cost)
    pub fn new(geo: &'a Geosphere, cost: &'a CellMap<u64>, goal: CellId) -> CellRoute<'a> {
        CellRoute { geo, cost, goal }
    }
}

impl<'a> SearchSpace for CellRoute<'a> {
    type State = CellId;
    type Action = CellId;

    fn successors(&self, s: &CellId) -> Vec<(CellId, CellId, u64)> {
        self.geo
            .neighbors(*s)
            .iter()
            .filter_map(|&next| {
                let step_cost = *self.cost.get(next);
                (step_cost != u64::MAX).then_some((next, next, step_cost))
            })
            .collect()
    }

    fn goal(&self, s: &CellId) -> bool {
        *s == self.goal
    }

    /// `0` — plain Dijkstra rather than a true admissible heuristic. Cell
    /// cost is a caller-defined, dimensionless traversal weight (Task 3 will
    /// give it terrain meaning), not a physical distance, so there is no
    /// general way to derive a lower bound on remaining cost from geometry
    /// alone without knowing the cheapest possible per-hop cost in advance.
    /// `0` is trivially admissible (never overestimates), so `astar` still
    /// finds the true optimum — it just explores more nodes than a tighter
    /// heuristic would. Correctness over pruning: get this right first.
    fn heuristic(&self, _s: &CellId) -> u64 {
        0
    }
}

/// The least-cost path from `from` to `to` over `geo`'s cell adjacency,
/// weighted by `cost` (the cost of a step is paid on the cell entered).
/// Returns the full path `[from, …, to]` (a single-element path if
/// `from == to`) and its total cost, or `None` if `to` is not reachable from
/// `from` within `budget` node expansions.
/// type-audit: bare-ok(count: cost), bare-ok(count: budget), bare-ok(count: return)
pub fn least_cost(
    geo: &Geosphere,
    cost: &CellMap<u64>,
    from: CellId,
    to: CellId,
    budget: usize,
) -> Option<(Vec<CellId>, u64)> {
    let space = CellRoute::new(geo, cost, to);
    let actions = astar(&space, from, budget)?;

    let mut path = Vec::with_capacity(actions.len() + 1);
    path.push(from);
    let mut total = 0u64;
    for cell in actions {
        total = total.saturating_add(*cost.get(cell));
        path.push(cell);
    }
    Some((path, total))
}

/// The result of a single-source cost sweep from [`least_cost_from`]: every
/// cell's least-cost total from the sweep's source, plus enough to
/// reconstruct the optimal path to any cell on demand.
///
/// **The tie-break is a pure function of the cost field, never of expansion
/// order.** When two edges reach the same cell at the exact same total cost,
/// the predecessor is fixed by comparing the two candidate predecessors'
/// `CellId` and keeping the lower one — not by which edge the sweep happened
/// to relax first. This exists because a consumer of `path_to` measures
/// whether *the world* changed which route is cheapest (weather shifting the
/// optimal path between two settlements) by comparing paths across two
/// nearly-identical cost fields. If the tie-break depended on iteration
/// order instead of the cost field alone, two equally-cheap paths could swap
/// between runs for reasons that have nothing to do with either field,
/// reporting rerouting that never happened — measuring the router instead of
/// the world.
pub struct CostSweep {
    /// The sweep's source cell.
    from: CellId,
    /// `dist[cell.0]` is the least-cost total from `from` to `cell`, or
    /// `None` if unreachable.
    dist: Vec<Option<u64>>,
    /// `prev[cell.0]` is the predecessor `cell` was reached from on its
    /// least-cost path, or `None` for `from` itself (which has none) and for
    /// any unreached cell.
    prev: Vec<Option<CellId>>,
}

impl CostSweep {
    /// The least-cost total from the sweep's source to `cell`, or `None` if
    /// `cell` is unreachable. `Some(0)` when `cell` is the source itself.
    /// type-audit: bare-ok(count: return)
    pub fn cost_to(&self, cell: CellId) -> Option<u64> {
        self.dist[cell.0 as usize]
    }

    /// The least-cost path from the sweep's source to `cell`, `[from, …,
    /// cell]` (a single-element path if `cell` is the source), reconstructed
    /// by walking [`CostSweep`]'s predecessor chain backward from `cell` to
    /// `from`. `None` if `cell` is unreachable.
    pub fn path_to(&self, cell: CellId) -> Option<Vec<CellId>> {
        self.dist[cell.0 as usize]?;
        let mut path = vec![cell];
        let mut current = cell;
        while current != self.from {
            current = self.prev[current.0 as usize]?;
            path.push(current);
        }
        path.reverse();
        Some(path)
    }
}

/// Least-cost totals and paths from `from` to **every** cell, in one sweep.
///
/// Exactly [`least_cost`]'s cost function evaluated everywhere at once: the
/// cost of a step is paid on the cell entered, `from` itself costs `0`, and
/// impassable cells (`u64::MAX`) are skipped in expansion rather than summed.
/// Unreachable cells report `None` from both [`CostSweep::cost_to`] and
/// [`CostSweep::path_to`]. There is no node budget — the sweep is bounded by
/// the mesh, not by a search horizon.
///
/// **Why a sweep rather than repeated [`least_cost`]:** a caller needing
/// distances (and now paths) from `S` sources to many destinations pays `S`
/// sweeps instead of `S²` single-target searches, and tracking predecessors
/// alongside distances makes every destination's path a free by-product of
/// the same sweep rather than a second search. The costs are identical to
/// repeated `least_cost` calls; only the work is shared. Determinism comes
/// from the `BTreeSet` frontier keyed on `(cost, CellId)` — a total order
/// with no hash seed and no float — matching the guarantee
/// `hornvale_kernel::astar` makes, and from `CostSweep`'s tie-break rule
/// (documented there) for predecessor choice among equal-cost routes.
/// type-audit: bare-ok(count: cost)
pub fn least_cost_from(geo: &Geosphere, cost: &CellMap<u64>, from: CellId) -> CostSweep {
    let mut dist: Vec<Option<u64>> = vec![None; geo.cell_count()];
    let mut prev: Vec<Option<CellId>> = vec![None; geo.cell_count()];
    let mut frontier: BTreeSet<(u64, CellId)> = BTreeSet::new();

    dist[from.0 as usize] = Some(0);
    frontier.insert((0, from));

    while let Some(&(d, cell)) = frontier.iter().next() {
        frontier.remove(&(d, cell));
        // A stale frontier entry: this cell was already settled more cheaply.
        if dist[cell.0 as usize] != Some(d) {
            continue;
        }
        for &next in geo.neighbors(cell) {
            let step = *cost.get(next);
            if step == u64::MAX {
                continue;
            }
            let candidate = d.saturating_add(step);
            match dist[next.0 as usize] {
                None => {
                    dist[next.0 as usize] = Some(candidate);
                    prev[next.0 as usize] = Some(cell);
                    frontier.insert((candidate, next));
                }
                Some(old) if candidate < old => {
                    frontier.remove(&(old, next));
                    dist[next.0 as usize] = Some(candidate);
                    prev[next.0 as usize] = Some(cell);
                    frontier.insert((candidate, next));
                }
                Some(old) if candidate == old => {
                    // Exact tie: keep the lower-CellId predecessor. Pure
                    // function of the cost field — see CostSweep's doc.
                    if let Some(existing) = prev[next.0 as usize]
                        && cell < existing
                    {
                        prev[next.0 as usize] = Some(cell);
                    }
                }
                _ => {}
            }
        }
    }

    CostSweep { from, dist, prev }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_cell_with_no_impassable_neighbors_lists_every_neighbor() {
        let geo = Geosphere::new(0);
        let cost = CellMap::from_fn(&geo, |_| 1);
        let route = CellRoute::new(&geo, &cost, CellId(0));
        let successors = route.successors(&CellId(1));
        assert_eq!(successors.len(), geo.neighbors(CellId(1)).len());
        for (action, next, step_cost) in successors {
            assert_eq!(action, next);
            assert_eq!(step_cost, 1);
        }
    }

    #[test]
    fn an_impassable_neighbor_is_skipped_not_included_at_high_cost() {
        let geo = Geosphere::new(0);
        let blocked = geo.neighbors(CellId(0))[0];
        let cost = CellMap::from_fn(&geo, |id| if id == blocked { u64::MAX } else { 1 });
        let route = CellRoute::new(&geo, &cost, CellId(0));
        let successors = route.successors(&CellId(0));
        assert!(
            successors.iter().all(|&(_, next, _)| next != blocked),
            "an impassable neighbor must not appear in successors at all"
        );
        assert_eq!(successors.len(), geo.neighbors(CellId(0)).len() - 1);
    }

    #[test]
    fn heuristic_is_always_zero() {
        let geo = Geosphere::new(0);
        let cost = CellMap::from_fn(&geo, |_| 1);
        let route = CellRoute::new(&geo, &cost, CellId(0));
        for cell in geo.cells() {
            assert_eq!(route.heuristic(&cell), 0);
        }
    }

    #[test]
    fn a_source_reaches_itself_at_zero_cost() {
        let geo = Geosphere::new(0);
        let cost = CellMap::from_fn(&geo, |_| 10u64);
        let d = least_cost_from(&geo, &cost, CellId(0));
        assert_eq!(d.cost_to(CellId(0)), Some(0));
        assert_eq!(d.path_to(CellId(0)), Some(vec![CellId(0)]));
    }

    #[test]
    fn impassable_cells_are_unreachable_and_do_not_overflow() {
        let geo = Geosphere::new(0);
        // Everything impassable except the source itself.
        let cost = CellMap::from_fn(&geo, |c| if c == CellId(0) { 10 } else { u64::MAX });
        let d = least_cost_from(&geo, &cost, CellId(0));
        assert_eq!(d.cost_to(CellId(0)), Some(0));
        for c in geo.cells().filter(|&c| c != CellId(0)) {
            assert_eq!(d.cost_to(c), None, "cell {c:?} should be unreachable");
            assert_eq!(d.path_to(c), None, "cell {c:?} should have no path");
        }
    }

    #[test]
    fn the_sweep_agrees_with_the_shipped_single_target_search() {
        // THE KEYSTONE. `least_cost_from` is only useful if it is the same
        // function as `least_cost`, evaluated everywhere at once. A
        // non-uniform cost field is essential: a uniform one would let a
        // plain hop-count agree by accident.
        //
        // This does NOT assert the reconstructed path equals `least_cost`'s
        // path cell-for-cell — both are optimal, but the two functions break
        // ties differently, so equal paths would be a coincidence and an
        // unequal one would not be a bug. What must hold, and is asserted
        // below for every cell: reachability agrees, the path is
        // well-formed (starts at the source, ends at `to`, every step is an
        // adjacency), and the path's own summed cost equals `cost_to`'s
        // number — i.e. the reconstructed path is genuinely optimal, not
        // merely present.
        let geo = Geosphere::new(1);
        let cost = CellMap::from_fn(&geo, |c| 10 + (c.0 as u64 % 7) * 13);
        let from = CellId(0);
        let swept = least_cost_from(&geo, &cost, from);
        for to in geo.cells() {
            let single = least_cost(&geo, &cost, from, to, 1_000_000).map(|(_, total)| total);
            assert_eq!(
                swept.cost_to(to),
                single,
                "sweep and single-target disagree on cost for {to:?}"
            );

            let path = swept.path_to(to);
            assert_eq!(
                path.is_some(),
                single.is_some(),
                "path_to and least_cost disagree on reachability for {to:?}"
            );
            if let Some(path) = path {
                assert_eq!(
                    path.first(),
                    Some(&from),
                    "path for {to:?} does not start at from"
                );
                assert_eq!(path.last(), Some(&to), "path for {to:?} does not end at to");
                for pair in path.windows(2) {
                    assert!(
                        geo.neighbors(pair[0]).contains(&pair[1]),
                        "path for {to:?} steps from {:?} to {:?}, which are not adjacent",
                        pair[0],
                        pair[1]
                    );
                }
                let path_cost: u64 = path[1..].iter().map(|&c| *cost.get(c)).sum();
                assert_eq!(
                    Some(path_cost),
                    swept.cost_to(to),
                    "path for {to:?} sums to {path_cost}, not cost_to's {:?}",
                    swept.cost_to(to)
                );
            }
        }
    }

    #[test]
    fn a_tie_is_broken_by_the_lower_cell_id_predecessor_not_expansion_order() {
        // `Geosphere::new(0)` is the level-0 icosphere: CellId(0)'s
        // neighbors are [1, 5, 7, 10, 11] (all at cost 1 under a uniform
        // field), and CellId(9) is a common neighbor of both CellId(1) and
        // CellId(5) — two routes into 9 (via 1, or via 5) tie at cost 2.
        // The rule picks the lower CellId predecessor, here 1, regardless of
        // which edge the BTreeSet frontier happens to relax first (it always
        // relaxes CellId(1) before CellId(5) here, since the frontier is
        // ordered by (cost, CellId) and both are at cost 1 — but the
        // tie-resolution logic itself does not depend on that order, only on
        // the final comparison, which this test pins).
        let geo = Geosphere::new(0);
        assert_eq!(
            geo.neighbors(CellId(0)),
            &[CellId(1), CellId(5), CellId(7), CellId(10), CellId(11)]
        );
        assert!(geo.neighbors(CellId(9)).contains(&CellId(1)));
        assert!(geo.neighbors(CellId(9)).contains(&CellId(5)));

        let cost = CellMap::from_fn(&geo, |_| 1u64);
        let sweep = least_cost_from(&geo, &cost, CellId(0));
        assert_eq!(sweep.cost_to(CellId(9)), Some(2));
        assert_eq!(
            sweep.path_to(CellId(9)),
            Some(vec![CellId(0), CellId(1), CellId(9)]),
            "the tie between predecessors 1 and 5 should resolve to the lower CellId, 1"
        );
    }
}
