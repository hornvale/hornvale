//! Least-cost cell routing: a `SearchSpace` over a `Geosphere`'s cell
//! adjacency, weighted by a per-cell traversal cost, solved by the kernel's
//! deterministic `astar`. This is the pathfinder later domains derive
//! natural land routes over (a terrain cost field is Task 3; deriving
//! `ConnectionGraph` edges from it is Task 4) — this module only finds the
//! least-cost path between two cells.

use hornvale_kernel::{CellId, CellMap, Geosphere, SearchSpace, astar};

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
}
