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

/// Least-cost totals from `from` to **every** cell, in one sweep.
///
/// Exactly [`least_cost`]'s cost function evaluated everywhere at once: the
/// cost of a step is paid on the cell entered, `from` itself is `Some(0)`, and
/// impassable cells (`u64::MAX`) are skipped in expansion rather than summed.
/// `None` means unreachable. There is no node budget — the sweep is bounded by
/// the mesh, not by a search horizon.
///
/// **Why a sweep rather than repeated [`least_cost`]:** a caller needing
/// distances from `S` sources to many destinations pays `S` sweeps instead of
/// `S²` single-target searches. The result is identical; only the work is
/// shared. Determinism comes from the `BTreeSet` frontier keyed on
/// `(cost, CellId)` — a total order with no hash seed and no float — matching
/// the guarantee `hornvale_kernel::astar` makes.
/// type-audit: bare-ok(count: cost), bare-ok(count: return)
pub fn least_cost_from(geo: &Geosphere, cost: &CellMap<u64>, from: CellId) -> CellMap<Option<u64>> {
    let mut dist: Vec<Option<u64>> = vec![None; geo.cell_count()];
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
            let improved = dist[next.0 as usize].is_none_or(|old| candidate < old);
            if improved {
                if let Some(old) = dist[next.0 as usize] {
                    frontier.remove(&(old, next));
                }
                dist[next.0 as usize] = Some(candidate);
                frontier.insert((candidate, next));
            }
        }
    }

    CellMap::from_fn(geo, |c| dist[c.0 as usize])
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
        assert_eq!(*d.get(CellId(0)), Some(0));
    }

    #[test]
    fn impassable_cells_are_unreachable_and_do_not_overflow() {
        let geo = Geosphere::new(0);
        // Everything impassable except the source itself.
        let cost = CellMap::from_fn(&geo, |c| if c == CellId(0) { 10 } else { u64::MAX });
        let d = least_cost_from(&geo, &cost, CellId(0));
        assert_eq!(*d.get(CellId(0)), Some(0));
        for c in geo.cells().filter(|&c| c != CellId(0)) {
            assert_eq!(*d.get(c), None, "cell {c:?} should be unreachable");
        }
    }

    #[test]
    fn the_sweep_agrees_with_the_shipped_single_target_search() {
        // THE KEYSTONE. `least_cost_from` is only useful if it is the same
        // function as `least_cost`, evaluated everywhere at once. A
        // non-uniform cost field is essential: a uniform one would let a
        // plain hop-count agree by accident.
        let geo = Geosphere::new(1);
        let cost = CellMap::from_fn(&geo, |c| 10 + (c.0 as u64 % 7) * 13);
        let from = CellId(0);
        let swept = least_cost_from(&geo, &cost, from);
        for to in geo.cells() {
            let single = least_cost(&geo, &cost, from, to, 1_000_000).map(|(_, total)| total);
            assert_eq!(
                *swept.get(to),
                single,
                "sweep and single-target disagree for {to:?}"
            );
        }
    }
}
