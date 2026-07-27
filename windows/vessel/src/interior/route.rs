//! Within-room movement: a [`SearchSpace`] over the anchor graph, so the kernel
//! planner serves the fine layer exactly as it serves navigation, GOAP and
//! prophecy (UNI-19). NO LATTICE and NO COORDINATE SOLVE — a creature does not
//! need a grid to walk on, it needs somewhere to walk to.

use super::anchor::{AnchorId, Interior};
use hornvale_kernel::astar::{SearchSpace, astar};

/// The within-room search problem: step between anchors that touch (`Ec`) OR
/// contain one another (`Ntpp`) until the goal anchor is reached. Unit cost — a
/// step is a step; introducing a distance here would violate the topology rule
/// (spec §2.1).
pub struct InteriorSpace<'a> {
    /// The room's anchor graph.
    interior: &'a Interior,
    /// The anchor being sought.
    goal: AnchorId,
}

impl SearchSpace for InteriorSpace<'_> {
    type State = AnchorId;
    type Action = AnchorId;

    fn successors(&self, s: &AnchorId) -> Vec<(AnchorId, AnchorId, u64)> {
        // Adjacency (`Ec`) AND containment (`Ntpp`) are both walkable: standing
        // in the alcove you can step to the hearth inside it, and back out.
        // `is_connected` has always walked both; routing must agree with it, or
        // the validator accepts rooms a creature cannot cross. `walkable_neighbors`
        // is the single shared definition (`Occupancy::walk` uses the same one),
        // so this planner and the thing that executes its plan can never disagree
        // about what one hop is.
        self.interior
            .walkable_neighbors(*s)
            .into_iter()
            .map(|n| (n, n, 1))
            .collect()
    }

    fn goal(&self, s: &AnchorId) -> bool {
        *s == self.goal
    }

    fn heuristic(&self, _s: &AnchorId) -> u64 {
        // Admissible and trivial: with unit costs over a handful of nodes, the
        // zero heuristic (Dijkstra) is correct and cheapest to be sure of. A
        // graph-distance heuristic would be admissible too and is not worth the
        // precompute at this size.
        0
    }
}

/// The least-cost sequence of anchors to step through to reach `to`, or `None`
/// if it is unreachable within `budget` expansions. An empty `Vec` means the
/// creature is already there.
/// type-audit: bare-ok(count: budget)
pub fn route_within(
    interior: &Interior,
    from: AnchorId,
    to: AnchorId,
    budget: usize,
) -> Option<Vec<AnchorId>> {
    astar(&InteriorSpace { interior, goal: to }, from, budget)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::{AnchorKind, Interior};

    #[test]
    fn a_creature_routes_across_the_room_to_the_hearth() {
        // door — hall — hearth: the shortest path is two steps, and it is the
        // path A* returns. No coordinates exist anywhere in this test.
        let mut i = Interior::new();
        let door = i.push(AnchorKind::Threshold, None);
        let hall = i.push(AnchorKind::Pool, None);
        let hearth = i.push(AnchorKind::Hearth, None);
        let bed = i.push(AnchorKind::Bed, None);
        i.connect(door, hall);
        i.connect(hall, hearth);
        i.connect(hall, bed);

        let plan = route_within(&i, door, hearth, 64).expect("the hearth is reachable");
        assert_eq!(
            plan,
            vec![hall, hearth],
            "it steps through the hall to the fire"
        );
    }

    #[test]
    fn an_unreachable_anchor_yields_no_route() {
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Threshold, None);
        let b = i.push(AnchorKind::Hearth, None);
        // deliberately unconnected
        assert_eq!(route_within(&i, a, b, 64), None);
    }

    #[test]
    fn standing_at_the_goal_is_an_empty_route() {
        let mut i = Interior::new();
        let a = i.push(AnchorKind::Hearth, None);
        assert_eq!(route_within(&i, a, a, 64), Some(Vec::new()));
    }
}
