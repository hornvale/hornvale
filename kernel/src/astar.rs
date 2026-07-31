//! A general, tense-agnostic, deterministic A* — a pure least-cost graph search
//! (UNI-19: one kernel planner serves navigation, GOAP, confabulation, prophecy,
//! each with its own state space + cost). It knows nothing of "time" or "GOAP";
//! a `SearchSpace` supplies the semantics. Determinism (the keystone): BTree
//! frontier + a TOTAL order over (f-cost, g-cost, state) — no HashMap, no RNG —
//! so the returned path is a pure function of the graph, even with ties.
//!
//! The-waymark, Task 6 (the solver seam): the search ALGORITHM is abstracted
//! behind [`Solver`] the same way the state space already was behind
//! [`SearchSpace`] — [`AStarSolver`] is the original algorithm unchanged (a
//! thin wrapper; [`astar`] is now a delegator to it, so every existing caller
//! is byte-identical by construction), and [`FieldSolver`] is a second,
//! independently-implemented backend proving the trait is genuinely
//! substitutable, not a one-impl abstraction. Both take an optional
//! [`crate::room::RoomMeshMemo`] a memo-aware `SearchSpace` (see
//! [`SearchSpace::successors_memo`]) can consult instead of recomputing
//! [`crate::room::RoomAddr::neighbors`] on every expansion.
use crate::room::RoomMeshMemo;
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

    /// [`Self::successors`], given an optional caller-owned
    /// [`RoomMeshMemo`] a memo-aware space may consult/fill instead of
    /// recomputing (the-waymark, Task 6). The default simply ignores `memo`
    /// and delegates to `successors` — every `SearchSpace` impl that has no
    /// use for the memo (a `CellId`/`AnchorId` state, or a `RoomAddr` state
    /// with no session memo in scope) is unaffected and needs no change.
    /// A space that DOES want the memo (e.g. a `RoomAddr` state whose
    /// `successors` calls `RoomAddr::neighbors`) overrides this to call
    /// [`crate::room::RoomAddr::neighbors_memo`] instead when `memo` is
    /// `Some`, while leaving every other edge-cost/avoid-set rule in
    /// `successors` untouched — the memo boundary is the raw neighbor
    /// lookup ALONE, never the successor list a caller-specific cost
    /// function (e.g. a remembered-danger penalty) then builds from it.
    /// The concrete [`RoomMeshMemo`] type (rather than a generic memo
    /// parameter) is a deliberate narrowing: it is the one cross-call cache
    /// the kernel has today, and keeping this monomorphic avoids a generic
    /// associated type nothing yet needs (the boring solution, not a
    /// premature abstraction) — a second domain wanting a different memo
    /// would need this generalized then, not now.
    /// type-audit: bare-ok(count: return)
    fn successors_memo(
        &self,
        s: &Self::State,
        _memo: Option<&mut RoomMeshMemo>,
    ) -> Vec<(Self::Action, Self::State, u64)> {
        self.successors(s)
    }
}

/// A pluggable search ALGORITHM over a fixed [`SearchSpace`] (the-waymark,
/// Task 6) — the seam that lets [`AStarSolver`] and [`FieldSolver`] answer
/// the exact same question, `solve(space, start, budget, memo)`, by two
/// independently-implemented strategies. `memo` is `None` for a caller with
/// no session-lived [`RoomMeshMemo`] in scope (or a space that ignores it);
/// `Some` for one that does — see [`SearchSpace::successors_memo`] for what
/// the memo half actually changes (nothing about the ANSWER, only how many
/// times the underlying pure function reruns).
pub trait Solver<S: SearchSpace> {
    /// The least-cost action sequence from `start` to a goal, or `None` if no
    /// goal is reachable within `budget` node expansions. An empty `Vec`
    /// means `start` is already a goal.
    /// type-audit: bare-ok(count: budget)
    fn solve(
        &self,
        space: &S,
        start: S::State,
        budget: usize,
        memo: Option<&mut RoomMeshMemo>,
    ) -> Option<Vec<S::Action>>;
}

/// The original `astar` algorithm, now behind [`Solver`] — zero behavior
/// change from the free function this module used to export directly (see
/// [`astar`], which is now a thin delegator to `AStarSolver.solve(..., None)`
/// so every existing caller stays byte-identical). Frontier: a `BTreeSet`
/// ordered by `(f_cost, g_cost, state)` — the total order IS the tie-break.
/// `best_g`: least cost-so-far per state. `came_from`: the `(prev-state,
/// action)` that reached each state on its best path.
#[derive(Debug, Default, Clone, Copy)]
pub struct AStarSolver;

impl<S: SearchSpace> Solver<S> for AStarSolver {
    fn solve(
        &self,
        space: &S,
        start: S::State,
        budget: usize,
        mut memo: Option<&mut RoomMeshMemo>,
    ) -> Option<Vec<S::Action>> {
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
            for (action, next, cost) in space.successors_memo(&state, memo.as_deref_mut()) {
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
}

/// The least-cost action sequence from `start` to a goal, or `None` if no goal is
/// reachable within `budget` node expansions. Deterministic: the frontier is
/// ordered by `(f, g, state)` (a total order), so ties resolve identically every
/// run. An empty `Vec` means `start` is already a goal. A thin delegator to
/// [`AStarSolver`] (the-waymark, Task 6) — every existing caller is unchanged.
/// type-audit: bare-ok(count: budget)
pub fn astar<S: SearchSpace>(space: &S, start: S::State, budget: usize) -> Option<Vec<S::Action>> {
    AStarSolver.solve(space, start, budget, None)
}

/// A budget-bounded, whole-region single-source [`Solver`] (the-waymark, Task
/// 6) — the promoted form of Task 5's reverse-Dijkstra field builder, which
/// lived test-only in `windows/vessel::liveness` (`ReverseField`/
/// `build_reverse_field`) because it had no `SearchSpace`-shaped seam to
/// implement against. Rather than returning at the FIRST goal state popped
/// (as [`AStarSolver`] does), it keeps expanding every state reachable
/// within `budget` — the field characteristic ("reaching everything within
/// budget", per the original test's own doc) — and only AFTER exploration
/// ends reconstructs the path to whichever goal state was popped first.
///
/// **Substitutability, proven not asserted.** `FieldSolver::solve` returns
/// the exact SAME state `AStarSolver::solve` would have returned early: the
/// first-goal-pop is recorded (not expanded further) the moment it is
/// popped, using the identical `(f, g, state)` total order and the
/// identical first-strict-improvement-wins relaxation rule, so nothing
/// exploration does AFTER that pop can retroactively change which state was
/// recorded or how `came_from` reconstructs its path. `astar_field_matches_
/// astar_solver` below pins this byte-for-byte across every existing
/// fixture, including the tie-break keystone case.
///
/// **This is a DIFFERENT claim than Task 5's own equivalence test.** The
/// disabled `windows/vessel::liveness` property test compared a field
/// ROOTED AT A SHARED DESTINATION against MANY independent per-room forward
/// searches — a cross-query reuse shape this trait's single-`start`-per-call
/// signature does not express — and found 52/346 rooms disagree in
/// `first_step` (root-relative tie-breaking; distance always agreed). Here,
/// `start` is the SAME root a caller would hand `AStarSolver`, so there is
/// no second root to disagree with, and therefore none of that risk either
/// — nor, by the same token, its query-amortization win. This solver exists
/// to give Task 7's bench a second, independently-implemented backend
/// (`kernel::astar::tests::astar_field_matches_astar_solver` is the
/// determinism half of that story), not to reproduce the disabled field's
/// cross-query reuse — home_nav stays on `AStarSolver` alone, per the
/// equivalence null (see the property test's own doc for the mechanism).
#[derive(Debug, Default, Clone, Copy)]
pub struct FieldSolver;

impl<S: SearchSpace> Solver<S> for FieldSolver {
    fn solve(
        &self,
        space: &S,
        start: S::State,
        budget: usize,
        mut memo: Option<&mut RoomMeshMemo>,
    ) -> Option<Vec<S::Action>> {
        let mut frontier: BTreeSet<(u64, u64, S::State)> = BTreeSet::new();
        let mut best_g: BTreeMap<S::State, u64> = BTreeMap::new();
        let mut came_from: BTreeMap<S::State, (S::State, S::Action)> = BTreeMap::new();

        let h0 = space.heuristic(&start);
        frontier.insert((h0, 0, start.clone()));
        best_g.insert(start.clone(), 0);

        // The first goal state popped, in frontier order — exactly the state
        // `AStarSolver` would return at, recorded rather than returned so
        // exploration can keep going (the field characteristic).
        let mut found: Option<S::State> = None;
        let mut expansions = 0usize;
        while let Some(&(_f, g, ref state)) = frontier.iter().next() {
            let (f, g, state) = (_f, g, state.clone());
            frontier.remove(&(f, g, state.clone()));
            if best_g.get(&state).is_some_and(|&bg| bg < g) {
                continue;
            }
            if found.is_none() && space.goal(&state) {
                // Mirrors `AStarSolver`'s early return exactly: this pop is
                // never counted as an expansion (the same is true there —
                // the `expansions += 1` below is never reached on the pop
                // that returns), and its successors are never generated.
                found = Some(state);
                continue;
            }
            expansions += 1;
            if expansions > budget {
                break;
            }
            for (action, next, cost) in space.successors_memo(&state, memo.as_deref_mut()) {
                let ng = g + cost;
                if best_g.get(&next).is_none_or(|&bg| ng < bg) {
                    best_g.insert(next.clone(), ng);
                    came_from.insert(next.clone(), (state.clone(), action));
                    let nf = ng + space.heuristic(&next);
                    frontier.insert((nf, ng, next));
                }
            }
        }

        let goal_state = found?;
        let mut actions = Vec::new();
        let mut cur = goal_state;
        while let Some((prev, act)) = came_from.get(&cur) {
            actions.push(act.clone());
            cur = prev.clone();
        }
        actions.reverse();
        Some(actions)
    }
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

    #[test]
    fn astar_is_a_thin_delegator_to_astar_solver() {
        // `astar` and `AStarSolver.solve(..., None)` must be the SAME call
        // (the-waymark, Task 6) — not merely produce the same answer by
        // coincidence, but literally be the delegator relationship the
        // module doc claims.
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 5)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph {
            edges,
            goal_node: 3,
            h: BTreeMap::new(),
        };
        assert_eq!(astar(&g, 0, 1000), AStarSolver.solve(&g, 0, 1000, None));
    }

    // --- FieldSolver substitutability (the-waymark, Task 6): proven, not
    // merely asserted — see FieldSolver's own doc for the argument. These
    // pin it byte-for-byte across every fixture the module already used to
    // characterize AStarSolver, including the tie-break keystone.

    #[test]
    fn field_solver_matches_astar_solver_on_the_least_cost_path() {
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 5)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph {
            edges,
            goal_node: 3,
            h: BTreeMap::new(),
        };
        assert_eq!(
            FieldSolver.solve(&g, 0, 1000, None),
            AStarSolver.solve(&g, 0, 1000, None)
        );
        assert_eq!(FieldSolver.solve(&g, 0, 1000, None), Some(vec!['a', 'c']));
    }

    #[test]
    fn field_solver_matches_astar_solver_on_the_tie_break_keystone() {
        // Mirrors `equal_cost_paths_break_ties_deterministically` above: two
        // DISTINCT equal-cost paths, so the total order over (f, g, state)
        // is the only thing deciding the answer. If FieldSolver picked its
        // "first goal popped" any differently than AStarSolver, this is
        // exactly the case that would show it.
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 1)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph {
            edges,
            goal_node: 3,
            h: BTreeMap::new(),
        };
        let astar_answer = AStarSolver.solve(&g, 0, 1000, None);
        assert_eq!(FieldSolver.solve(&g, 0, 1000, None), astar_answer);
        for _ in 0..100 {
            assert_eq!(FieldSolver.solve(&g, 0, 1000, None), astar_answer);
        }
    }

    #[test]
    fn field_solver_matches_astar_solver_when_unreachable() {
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1)]); // 1 is a dead end; goal 9 unreachable
        let g = Graph {
            edges,
            goal_node: 9,
            h: BTreeMap::new(),
        };
        assert_eq!(FieldSolver.solve(&g, 0, 1000, None), None);
    }

    #[test]
    fn field_solver_matches_astar_solver_when_start_is_goal() {
        let g = Graph {
            edges: BTreeMap::new(),
            goal_node: 0,
            h: BTreeMap::new(),
        };
        assert_eq!(FieldSolver.solve(&g, 0, 1000, None), Some(vec![]));
    }

    #[test]
    fn field_solver_matches_astar_solver_under_a_tight_budget() {
        // A long chain; a tiny budget must return None from BOTH solvers —
        // exercising FieldSolver's own budget accounting (the `break` path
        // rather than AStarSolver's `return None`), not just its happy path.
        let mut edges = BTreeMap::new();
        for i in 0u32..1000 {
            edges.insert(i, vec![('n', i + 1, 1)]);
        }
        let g = Graph {
            edges,
            goal_node: 999,
            h: BTreeMap::new(),
        };
        assert_eq!(FieldSolver.solve(&g, 0, 5, None), None);
        assert_eq!(
            FieldSolver.solve(&g, 0, 5, None),
            AStarSolver.solve(&g, 0, 5, None)
        );
        // A budget generous enough to reach the goal: both solvers agree on
        // the exact path too, not merely on reachability.
        assert_eq!(
            FieldSolver.solve(&g, 0, 1000, None),
            AStarSolver.solve(&g, 0, 1000, None)
        );
    }
}
