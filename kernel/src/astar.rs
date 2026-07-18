//! A general, tense-agnostic, deterministic A* — a pure least-cost graph search
//! (UNI-19: one kernel planner serves navigation, GOAP, confabulation, prophecy,
//! each with its own state space + cost). It knows nothing of "time" or "GOAP";
//! a `SearchSpace` supplies the semantics. Determinism (the keystone): BTree
//! frontier + a TOTAL order over (f-cost, g-cost, state) — no HashMap, no RNG —
//! so the returned path is a pure function of the graph, even with ties.
use std::collections::BTreeMap;
use std::collections::BTreeSet;

/// The problem an `astar` search solves: a state space with weighted actions, a
/// goal test, and an admissible heuristic. `State: Ord` supplies the
/// deterministic tie-break; costs are `u64` (integer — no float non-determinism).
pub trait SearchSpace {
    /// A search state (a node). `Ord` for the deterministic frontier/closed set.
    type State: Ord + Clone;
    /// An action (an edge label) returned in the plan.
    type Action: Clone;
    /// The `(action, next-state, cost)` triples reachable in one step from `s`.
    /// type-audit: bare-ok(count: return)
    fn successors(&self, s: &Self::State) -> Vec<(Self::Action, Self::State, u64)>;
    /// Is `s` a goal state?
    /// type-audit: bare-ok(flag: return)
    fn goal(&self, s: &Self::State) -> bool;
    /// An admissible (never-overestimating) estimate of the remaining cost.
    /// type-audit: bare-ok(count: return)
    fn heuristic(&self, s: &Self::State) -> u64;
}

/// The least-cost action sequence from `start` to a goal, or `None` if no goal is
/// reachable within `budget` node expansions. Deterministic: the frontier is
/// ordered by `(f, g, state)` (a total order), so ties resolve identically every
/// run. An empty `Vec` means `start` is already a goal.
/// type-audit: bare-ok(count: budget)
pub fn astar<S: SearchSpace>(space: &S, start: S::State, budget: usize) -> Option<Vec<S::Action>> {
    // Frontier: a BTreeSet ordered by (f_cost, g_cost, state) — the total order
    // IS the tie-break. best_g: least cost-so-far per state. came_from: the
    // (prev-state, action) that reached each state on its best path.
    let mut frontier: BTreeSet<(u64, u64, S::State)> = BTreeSet::new();
    let mut best_g: BTreeMap<S::State, u64> = BTreeMap::new();
    let mut came_from: BTreeMap<S::State, (S::State, S::Action)> = BTreeMap::new();

    let h0 = space.heuristic(&start);
    frontier.insert((h0, 0, start.clone()));
    best_g.insert(start.clone(), 0);

    let mut expansions = 0usize;
    while let Some(&(_f, g, ref state)) = frontier.iter().next() {
        let (f, g, state) = (_f, g, state.clone());
        frontier.remove(&(f, g, state.clone()));
        // Skip a stale frontier entry (a better path to `state` was found later).
        if best_g.get(&state).is_some_and(|&bg| bg < g) {
            continue;
        }
        if space.goal(&state) {
            // Reconstruct the action path by walking came_from back to start.
            let mut actions = Vec::new();
            let mut cur = state;
            while let Some((prev, act)) = came_from.get(&cur) {
                actions.push(act.clone());
                cur = prev.clone();
            }
            actions.reverse();
            return Some(actions);
        }
        expansions += 1;
        if expansions > budget {
            return None;
        }
        for (action, next, cost) in space.successors(&state) {
            let ng = g + cost;
            if best_g.get(&next).is_none_or(|&bg| ng < bg) {
                best_g.insert(next.clone(), ng);
                came_from.insert(next.clone(), (state.clone(), action));
                let nf = ng + space.heuristic(&next);
                frontier.insert((nf, ng, next));
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    // A tiny explicit weighted digraph search space for testing.
    struct Graph {
        edges: BTreeMap<u32, Vec<(char, u32, u64)>>, // node -> [(action-label, next, cost)]
        goal_node: u32,
        h: BTreeMap<u32, u64>, // heuristic per node (0 if absent)
    }
    impl SearchSpace for Graph {
        type State = u32;
        type Action = char;
        fn successors(&self, s: &u32) -> Vec<(char, u32, u64)> {
            self.edges.get(s).cloned().unwrap_or_default()
        }
        fn goal(&self, s: &u32) -> bool {
            *s == self.goal_node
        }
        fn heuristic(&self, s: &u32) -> u64 {
            *self.h.get(s).unwrap_or(&0)
        }
    }

    #[test]
    fn finds_the_least_cost_path() {
        // 0 -a-> 1 (1), 0 -b-> 2 (5), 1 -c-> 3 (1), 2 -d-> 3 (1); goal 3.
        // cheapest: a,c (cost 2) beats b,d (cost 6).
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 5)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph {
            edges,
            goal_node: 3,
            h: BTreeMap::new(),
        };
        assert_eq!(astar(&g, 0, 1000), Some(vec!['a', 'c']));
    }

    #[test]
    fn unreachable_goal_is_none() {
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1)]); // 1 is a dead end; goal 9 unreachable
        let g = Graph {
            edges,
            goal_node: 9,
            h: BTreeMap::new(),
        };
        assert_eq!(astar(&g, 0, 1000), None);
    }

    #[test]
    fn start_is_goal_is_empty_plan() {
        let g = Graph {
            edges: BTreeMap::new(),
            goal_node: 0,
            h: BTreeMap::new(),
        };
        assert_eq!(astar(&g, 0, 1000), Some(vec![]));
    }

    #[test]
    fn equal_cost_paths_break_ties_deterministically() {
        // THE TIE-BREAK KEYSTONE: two DISTINCT equal-cost paths to the goal.
        // 0 -a-> 1 (1) -c-> 3 (1);  0 -b-> 2 (1) -d-> 3 (1). Both cost 2.
        // The result must be STABLE and identical on repeat (a total order over
        // (f, g, state) picks one). Assert it equals itself across 100 runs.
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 1)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph {
            edges,
            goal_node: 3,
            h: BTreeMap::new(),
        };
        let first = astar(&g, 0, 1000).unwrap();
        assert_eq!(first.len(), 2);
        for _ in 0..100 {
            assert_eq!(astar(&g, 0, 1000), Some(first.clone()));
        }
    }

    #[test]
    fn a_nonzero_admissible_heuristic_still_finds_the_optimum() {
        // Same graph as finds_the_least_cost_path, with an admissible heuristic
        // (<= true remaining cost): h(1)=1, h(2)=1, h(3)=0. Optimum unchanged.
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 5)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let mut h = BTreeMap::new();
        h.insert(1u32, 1);
        h.insert(2u32, 1);
        let g = Graph {
            edges,
            goal_node: 3,
            h,
        };
        assert_eq!(astar(&g, 0, 1000), Some(vec!['a', 'c']));
    }

    #[test]
    fn budget_bounds_the_search() {
        // A long chain; a tiny budget returns None rather than exploring forever.
        let mut edges = BTreeMap::new();
        for i in 0u32..1000 {
            edges.insert(i, vec![('n', i + 1, 1)]);
        }
        let g = Graph {
            edges,
            goal_node: 999,
            h: BTreeMap::new(),
        };
        assert_eq!(astar(&g, 0, 5), None); // 5 expansions can't reach node 999
    }
}
