# The Foresight Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land PSY-6's goal rung — GOAP as A\*: a general deterministic A\* kernel primitive, instantiated for a tiny GOAP problem (a precondition chain: `Drink` gated by at-water forces a planned `[move*, drink]` journey), replacing The Wanting's reactive `decide` body via the reserved seam.

**Architecture:** The kernel gains a pure, tense-agnostic, deterministic A\* (`SearchSpace` trait + `astar`). The vessel instantiates it for GOAP (`Action`, `PlanState`, successors/goal/heuristic), refactors the drive to fold `drank` events (drinking is a planned action, not proximity), makes `decide` run the planner and return the plan's first action (`Intent::Do(Action)`), and refactors the tick to walk the plan step by step. Census-free, genesis byte-identical. Spec: `docs/superpowers/specs/2026-07-18-the-foresight-design.md` (G3-approved).

**Tech Stack:** Rust edition 2024, std + serde/serde_json only. `cargo nextest`.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. **A\* determinism depends on this** — the open/closed sets and the frontier tie-break MUST be BTree/total-order.
- **No new dependencies**; **no wall-clock**; **no RNG** (A\* is natively deterministic; the tie-break is a total order over state, not random).
- **Domains untouched** (Walk §11): the A\* goes in the KERNEL (domain-agnostic, like the tick/fact-index); the GOAP parts in `windows/vessel`. No `domains/*` change (`cli/tests/architecture.rs` enforces).
- **The reserved seam is filled, not re-architected (The Wanting #9):** `decide` keeps its role (the tick depends only on its `Intent` output); `Intent` generalizes `{GoTo, Hold}` → `{Do(Action), Hold}`. Do NOT change the tick's dependency on `Intent`.
- **A\* is general and tense-agnostic; GOAP is its ONLY instantiation this campaign.** Do NOT build navigation/confabulation instantiations (followups). Do NOT build the MAP-27 verb DSL, arbitration, a psychology cost, or belief-reading — those are reserved (`cost` stays uniform=1; `view`/state stays ground-truth; one drive).
- **Every public item: doc comment + `type-audit:` verdict tag** (struct/fn-level). **Run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` (exit 0) before every commit** — every bare primitive at a pub boundary (params AND returns) needs a `bare-ok(<class>: <name>)`; regenerate `docs/audits/type-audit-report.md` if you add pub items.
- **`cargo fmt` last**; clippy `-D warnings` clean. Commit trailer: `Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm`.
- **Byte-identical genesis (spec §3):** commit NO plan/goal/`drank` fact at world build; genesis worlds, census, gallery/laboratory artifacts stay byte-identical. After each task, `git diff --stat book/src/laboratory/ cli/tests/fixtures/` must be empty (the only expected book change is Task 4's re-baselined game-layer transcript). If a genesis artifact drifts, STOP.
- **Mutation-verify every new test**; report each.
- **The A\* keystone: PATH DETERMINISM.** Its generator coverage is part of correctness (the c4/c5 lesson): PLANT equal-cost paths (to prove the tie-break selects one deterministically), unreachable goals, and a zero-heuristic (Dijkstra) case. A property test whose generator never produces two equal-cost paths does not test the tie-break.
- Worktree: `.claude/worktrees/the-foresight`, branch `worktree-the-foresight`.

---

### Task 0: Baseline gate

**Files:** none modified.

- [ ] **Step 1: Preflight** — Run: `make preflight`. Expected: GO (absorb main into the branch on NO-GO, re-gate, note it).
- [ ] **Step 2: Baseline** — Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-f-baseline.txt`. Expected: exit 0. If red, STOP and diff against main.
- [ ] **Step 3: fmt+clippy** — Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Expected: clean. No commit.

---

### Task 1: The general deterministic A\* (kernel)

**Files:**
- Create: `kernel/src/astar.rs`
- Modify: `kernel/src/lib.rs` (`pub mod astar;` + re-exports)

**Interfaces:**
- Produces:
  - `pub trait SearchSpace { type State: Ord + Clone; type Action: Clone; fn successors(&self, s: &Self::State) -> Vec<(Self::Action, Self::State, u64)>; fn goal(&self, s: &Self::State) -> bool; fn heuristic(&self, s: &Self::State) -> u64; }`
  - `pub fn astar<S: SearchSpace>(space: &S, start: S::State, budget: usize) -> Option<Vec<S::Action>>` — least-cost action sequence from `start` to a goal state, or `None` (unreachable within `budget` node expansions). Deterministic.

- [ ] **Step 1: Write the failing tests** (`kernel/src/astar.rs` tests)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    // A tiny explicit weighted digraph search space for testing.
    struct Graph {
        edges: BTreeMap<u32, Vec<(char, u32, u64)>>, // node -> [(action-label, next, cost)]
        goal_node: u32,
        h: BTreeMap<u32, u64>,                        // heuristic per node (0 if absent)
    }
    impl SearchSpace for Graph {
        type State = u32;
        type Action = char;
        fn successors(&self, s: &u32) -> Vec<(char, u32, u64)> {
            self.edges.get(s).cloned().unwrap_or_default()
        }
        fn goal(&self, s: &u32) -> bool { *s == self.goal_node }
        fn heuristic(&self, s: &u32) -> u64 { *self.h.get(s).unwrap_or(&0) }
    }

    #[test]
    fn finds_the_least_cost_path() {
        // 0 -a-> 1 (1), 0 -b-> 2 (5), 1 -c-> 3 (1), 2 -d-> 3 (1); goal 3.
        // cheapest: a,c (cost 2) beats b,d (cost 6).
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1), ('b', 2, 5)]);
        edges.insert(1u32, vec![('c', 3, 1)]);
        edges.insert(2u32, vec![('d', 3, 1)]);
        let g = Graph { edges, goal_node: 3, h: BTreeMap::new() };
        assert_eq!(astar(&g, 0, 1000), Some(vec!['a', 'c']));
    }

    #[test]
    fn unreachable_goal_is_none() {
        let mut edges = BTreeMap::new();
        edges.insert(0u32, vec![('a', 1, 1)]); // 1 is a dead end; goal 9 unreachable
        let g = Graph { edges, goal_node: 9, h: BTreeMap::new() };
        assert_eq!(astar(&g, 0, 1000), None);
    }

    #[test]
    fn start_is_goal_is_empty_plan() {
        let g = Graph { edges: BTreeMap::new(), goal_node: 0, h: BTreeMap::new() };
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
        let g = Graph { edges, goal_node: 3, h: BTreeMap::new() };
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
        h.insert(1u32, 1); h.insert(2u32, 1);
        let g = Graph { edges, goal_node: 3, h };
        assert_eq!(astar(&g, 0, 1000), Some(vec!['a', 'c']));
    }

    #[test]
    fn budget_bounds_the_search() {
        // A long chain; a tiny budget returns None rather than exploring forever.
        let mut edges = BTreeMap::new();
        for i in 0u32..1000 { edges.insert(i, vec![('n', i + 1, 1)]); }
        let g = Graph { edges, goal_node: 999, h: BTreeMap::new() };
        assert_eq!(astar(&g, 0, 5), None); // 5 expansions can't reach node 999
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel --lib astar::`
Expected: compile error (types missing).

- [ ] **Step 3: Implement** (`kernel/src/astar.rs`)

```rust
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
    fn successors(&self, s: &Self::State) -> Vec<(Self::Action, Self::State, u64)>;
    /// Is `s` a goal state?
    fn goal(&self, s: &Self::State) -> bool;
    /// An admissible (never-overestimating) estimate of the remaining cost.
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
```

(Note: `BTreeSet::iter().next()` yields the minimum `(f, g, state)` — the deterministic pop. The `is_some_and`/`is_none_or` are stable std methods; if the toolchain lacks `is_none_or`, use `map_or(true, ...)`.)

- [ ] **Step 4: Run + mutation-verify + re-export + commit**

Run: `cargo test -p hornvale-kernel --lib astar::`
Expected: PASS (all 6). Mutation-verify the tie-break: change the frontier from `BTreeSet<(f,g,state)>` to something that drops the `state` from the key (e.g. only `(f,g)`, forcing a non-total order / arbitrary state pick) — `equal_cost_paths_break_ties_deterministically` must become flaky/RED; restore. (If a `Vec`-linear-min is used instead, ensure it still breaks ties by the full tuple.)

Add to `kernel/src/lib.rs`: `pub mod astar;` and `pub use astar::{SearchSpace, astar};`.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check   # exit 0
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add -A && git commit -m "feat(kernel): a general deterministic A* — SearchSpace + astar, BTree frontier, total-order tie-break (the-foresight T1)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 2: The GOAP space (vessel) — actions, state, precondition chain

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`Action`, `PlanState`, `GoapSpace`, `plan_to_water`; additive)

**Interfaces:**
- Consumes: Task 1's `SearchSpace`/`astar`; `RoomAddr`.
- Produces:
  - `pub enum Action { MoveTo(RoomAddr), Drink }`
  - `pub struct PlanState { pub position: RoomAddr, pub hydrated: bool }` (derive `Ord, Clone, PartialEq, Eq`)
  - `pub struct GoapSpace { pub water: RoomAddr }` impl `SearchSpace<State=PlanState, Action=Action>` — successors: one `MoveTo(n)` per mesh neighbor (cost 1) + `Drink` if `position == water` (cost 1, effect `hydrated=true`); goal: `hydrated`; heuristic: `0` (Dijkstra-mode — trivially admissible; a geometric heuristic is a followup).
  - `pub fn plan_to_water(from: &RoomAddr, water: &RoomAddr, budget: usize) -> Option<Vec<Action>>` — the `[move*, drink]` plan (or `None`).
  - `pub fn plan_to_room(from: &RoomAddr, dest: &RoomAddr, budget: usize) -> Option<Vec<Action>>` — pure navigation (the home-return goal; a space whose goal is `position == dest`, no Drink).

- [ ] **Step 1: Write the failing tests** (append to `liveness.rs` tests)

```rust
fn raddr(seed: f64) -> RoomAddr { RoomAddr::containing([seed, 0.0, 0.0], 6) }

#[test]
fn plan_to_water_is_a_precondition_chain_move_then_drink() {
    // Water is a mesh neighbor of home (one step away): plan is [MoveTo(water), Drink].
    let home = raddr(1.0);
    let water = home.neighbors()[0].clone();
    let plan = plan_to_water(&home, &water, 10_000).expect("reachable");
    assert_eq!(plan.len(), 2);
    assert!(matches!(plan[0], Action::MoveTo(ref r) if *r == water));
    assert!(matches!(plan[1], Action::Drink));
}

#[test]
fn plan_to_water_when_already_there_is_just_drink() {
    let water = raddr(1.0);
    let plan = plan_to_water(&water, &water, 10_000).unwrap();
    assert_eq!(plan, vec![Action::Drink]);
}

#[test]
fn every_action_in_a_plan_has_its_precondition_satisfied_in_sequence() {
    // Execute the plan from `home`, checking each action's precondition holds in
    // order (the precondition-chain validity: Drink is only ever preceded by
    // arrival at water). Water two rooms away for a genuine multi-step chain.
    let home = raddr(1.0);
    let mid = home.neighbors()[0].clone();
    let water = mid.neighbors().iter().find(|n| **n != home).unwrap().clone();
    let plan = plan_to_water(&home, &water, 10_000).expect("reachable");
    let mut pos = home.clone();
    let mut hydrated = false;
    for a in &plan {
        match a {
            Action::MoveTo(n) => {
                assert!(pos.neighbors().contains(n), "MoveTo precondition: adjacency");
                pos = n.clone();
            }
            Action::Drink => {
                assert_eq!(pos, water, "Drink precondition: at water");
                hydrated = true;
            }
        }
    }
    assert!(hydrated, "the plan achieves the goal");
    assert!(plan.len() >= 3, "multi-step: at least two moves then a drink");
}

#[test]
fn plan_to_room_is_pure_navigation_no_drink() {
    let home = raddr(1.0);
    let dest = home.neighbors()[0].clone();
    let plan = plan_to_room(&home, &dest, 10_000).unwrap();
    assert!(plan.iter().all(|a| matches!(a, Action::MoveTo(_))));
    assert!(!plan.is_empty());
}
```

- [ ] **Step 2: Run to verify failure** — `cargo test -p hornvale-vessel --lib liveness::tests::plan_`. Expected: compile error.

- [ ] **Step 3: Implement** (in `liveness.rs`)

```rust
use hornvale_kernel::{SearchSpace, astar};

/// A GOAP action — a precondition/effect transformation over the plan state.
/// Minimal + heterogeneous (the precondition chain needs two kinds); the MAP-27
/// authored-verb DSL is a followup.
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Walk to an adjacent room (precondition: adjacency; effect: position).
    MoveTo(RoomAddr),
    /// Drink (precondition: at the water room; effect: hydrated).
    Drink,
}

/// The GOAP planning state A* searches: where the agent is and whether it has
/// drunk. `Ord` for the deterministic search.
/// type-audit: bare-ok(flag: hydrated)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PlanState {
    /// The agent's room.
    pub position: RoomAddr,
    /// Whether the sustenance goal is met (has drunk this plan).
    pub hydrated: bool,
}

/// The GOAP search space for the sustenance goal: reach water and drink.
/// type-audit: bare-ok(return)
pub struct GoapSpace {
    /// The water room the `Drink` action requires.
    pub water: RoomAddr,
}
impl SearchSpace for GoapSpace {
    type State = PlanState;
    type Action = Action;
    fn successors(&self, s: &PlanState) -> Vec<(Action, PlanState, u64)> {
        if s.hydrated {
            return Vec::new(); // goal reached; no need to expand
        }
        let mut out: Vec<(Action, PlanState, u64)> = s
            .position
            .neighbors()
            .into_iter()
            .map(|n| (Action::MoveTo(n.clone()), PlanState { position: n, hydrated: false }, 1))
            .collect();
        if s.position == self.water {
            out.push((Action::Drink, PlanState { position: s.position.clone(), hydrated: true }, 1));
        }
        out
    }
    fn goal(&self, s: &PlanState) -> bool { s.hydrated }
    fn heuristic(&self, _s: &PlanState) -> u64 { 0 } // Dijkstra-mode; geometric heuristic is a followup
}

/// Plan the `[move*, drink]` journey to satisfy the sustenance goal, or `None`
/// if water is unreachable within `budget`.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_water(from: &RoomAddr, water: &RoomAddr, budget: usize) -> Option<Vec<Action>> {
    astar(&GoapSpace { water: water.clone() }, PlanState { position: from.clone(), hydrated: false }, budget)
}

/// A navigation-only space (the home-return goal — no Drink): goal is arrival.
struct NavSpace { dest: RoomAddr }
impl SearchSpace for NavSpace {
    type State = RoomAddr;
    type Action = Action;
    fn successors(&self, s: &RoomAddr) -> Vec<(Action, RoomAddr, u64)> {
        s.neighbors().into_iter().map(|n| (Action::MoveTo(n.clone()), n, 1)).collect()
    }
    fn goal(&self, s: &RoomAddr) -> bool { *s == self.dest }
    fn heuristic(&self, _s: &RoomAddr) -> u64 { 0 }
}

/// Plan a pure navigation path to `dest` (the home-return goal), or `None`.
/// type-audit: bare-ok(count: budget)
pub fn plan_to_room(from: &RoomAddr, dest: &RoomAddr, budget: usize) -> Option<Vec<Action>> {
    astar(&NavSpace { dest: dest.clone() }, from.clone(), budget)
}
```

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel --lib liveness::tests::plan_`
Expected: PASS. Mutation-verify the precondition chain: in `successors`, always emit `Drink` (drop the `position == water` gate) → `every_action_in_a_plan_has_its_precondition_satisfied_in_sequence` reds (Drink appears before arrival). Restore.

```bash
git diff --stat book/src/laboratory/ cli/tests/fixtures/   # empty
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the GOAP space — Action/PlanState/GoapSpace, the precondition chain (the-foresight T2)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 3: The drive refactor + the planner in `decide` + the tick walks the plan

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (drive folds `drank`; `DRANK` const; `Intent::Do(Action)`; `decide` runs the planner; `DriveMovements::step` walks the plan)
- Modify: `windows/vessel/src/session.rs` (register `DRANK`; the tick call unchanged in shape)
- Modify: `windows/vessel/tests/possession_moves.rs` (the observed-behavior tests, now planned)

**Interfaces:**
- Consumes: Tasks 1–2 + The Wanting's `Npc`/`AGENT_AT`/`agent_position`/`SUSTENANCE`/`DriveParams`, the tick.
- Produces: `pub const DRANK: &str = "drank";`; `Intent { Do(Action), Hold }`; `decide(npc, view, budget) -> Intent` (planner body); a `DriveMovements::step` that walks the plan.

- [ ] **Step 1: Write the failing tests** (append to `liveness.rs` tests + edit `possession_moves.rs`)

```rust
#[test]
fn drive_folds_drank_events_rising_since_the_last_drink() {
    // drive = rise * (t - last_drank_day), clamped [0,1]; last_drank = latest DRANK day.
    let p = SUSTENANCE;
    let mut ledger = Ledger::default();
    let mut reg = hornvale_kernel::ConceptRegistry::default();
    reg.register_predicate(DRANK, false, "drank").unwrap();
    let e = ledger.mint_entity();
    // no drank yet: rises from day 0
    assert!((drive_at(&ledger, e, WorldTime { day: 2.0 }, &p) - (p.rise * 2.0)).abs() < 1e-9);
    // drank on day 5 -> resets; by day 6 it has risen rise*1
    ledger.commit(hornvale_kernel::Fact { subject: e, predicate: DRANK.to_string(),
        object: Value::Flag(true), place: None, day: Some(5.0), provenance: "t".into() }, &reg).unwrap();
    assert!((drive_at(&ledger, e, WorldTime { day: 6.0 }, &p) - (p.rise * 1.0)).abs() < 1e-9);
}

#[test]
fn a_thirsty_agent_plans_to_water_and_the_tick_walks_it() {
    // Over a wait long enough to grow thirsty, the tick commits a run of agent-at
    // moves ending at water plus a `drank`, and the drive resets.
    let mut world_reg = hornvale_kernel::ConceptRegistry::default();
    world_reg.register_predicate(AGENT_AT, false, "pos").unwrap();
    world_reg.register_predicate(DRANK, false, "drank").unwrap();
    let mut ledger = Ledger::default();
    let e = ledger.mint_entity();
    let home = raddr(1.0);
    let water = home.neighbors()[0].neighbors().iter().find(|n| **n != home).unwrap().clone();
    let npc = Npc { entity: e, home: home.clone(), resource: water.clone(),
                    activity: hornvale_species::ActivityCycle::Diurnal, label: "herder".into() };
    let sys = DriveMovements { npcs: vec![npc.clone()], from: WorldTime{day:0.0}, to: WorldTime{day:40.0}, params: SUSTENANCE };
    let next = hornvale_kernel::tick(&ledger, &[&sys], &["drive-movements"], &world_reg).unwrap();
    // At least one drank committed (the agent reached water and drank).
    assert!(next.find(DRANK).filter(|f| f.subject == e).count() >= 1, "the agent drank");
    // agent-at moves committed (the journey), and the last position before a
    // drank was the water room.
    let moves = next.find(AGENT_AT).filter(|f| f.subject == e).count();
    assert!(moves >= 1, "the agent walked");
    let _ = ledger;
}
```

Update `possession_moves.rs`: the co-located-NPC-moves-and-is-observed test stays (the agent still moves on a `wait`), but now via a planned journey; adjust the `wait` interval so a full seek-drink cycle completes; keep the day-0-unchanged, byte-determinism, and named-observation assertions.

- [ ] **Step 2: Run to verify failure** — `cargo test -p hornvale-vessel liveness::tests::drive_folds_drank`. Expected: compile error (`DRANK`, new `drive_at` signature).

- [ ] **Step 3: Implement**

**(a) The drive folds `drank`.** Change `drive_at` to fold `DRANK` events (drop the `resource` param and the agent-at/position fold):

```rust
/// A game-layer predicate: the agent drank (satisfied its sustenance goal) on
/// this day. Registered by the session, NOT at genesis.
/// type-audit: bare-ok(identifier-text)
pub const DRANK: &str = "drank";

/// The drive at `t`: time since the last drink, a fold over committed `drank`
/// events. Rises `p.rise`/day since `last_drank_day` (0 before any drink),
/// clamped [0,1]. DRIVE == FOLD, now over `drank` — drinking is a PLANNED action
/// (The Foresight), not automatic proximity (The Wanting).
/// type-audit: bare-ok(ratio: return)
pub fn drive_at(ledger: &Ledger, entity: EntityId, t: WorldTime, p: &DriveParams) -> f64 {
    let last_drank = ledger
        .find(DRANK)
        .filter(|f| f.subject == entity)
        .filter_map(|f| f.day)
        .fold(0.0_f64, f64::max);
    (p.rise * (t.day - last_drank)).clamp(0.0, 1.0)
}
```

Update every `drive_at(...)` call site (the tick, `session.rs`'s `needs`) to the new signature (drop the `resource` arg). Note `DriveParams.fall`/`sated` are now only used by the goal-state logic (still authored).

**(b) `Intent` generalizes; `decide` runs the planner** (the reserved-seam body-swap):

```rust
/// The decision's output — the FIRST action of the agent's current plan, or
/// Hold. The tick depends only on this; the planner fills the body without
/// changing the seam (The Wanting decision #9).
/// type-audit: bare-ok(return)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    /// Perform this action next (the first step of the least-cost plan).
    Do(Action),
    /// No action (goal already met and at home).
    Hold,
}

/// The planner (The Foresight): the drive generates a goal, A* plans to it, and
/// `decide` returns the plan's first action. Two goal-states of the ONE drive
/// (no arbitration): thirsty (`view.drive >= act`) -> plan to water + drink;
/// otherwise -> return home. `budget` bounds the search (Hold if unreachable).
///
/// PRESERVES The Wanting's reserved `view` seam: `decide` reads `view`
/// (`Perceived { position, drive }`) — ground-truth today; PSY-6's
/// "plan over belief, not truth" (UNI-16) is later a change to what FILLS
/// `view`, not to this signature. `water`/`home`/`budget` are the planner's
/// static inputs. The tick still depends only on the `Intent` output.
/// type-audit: bare-ok(count: budget), bare-ok(return)
pub fn decide(view: &Perceived, home: &RoomAddr, water: &RoomAddr, p: &DriveParams, budget: usize) -> Intent {
    if view.drive >= p.act {
        match plan_to_water(&view.position, water, budget).and_then(|plan| plan.into_iter().next()) {
            Some(a) => Intent::Do(a),
            None => Intent::Hold, // water unreachable within budget
        }
    } else if &view.position != home {
        match plan_to_room(&view.position, home, budget).and_then(|plan| plan.into_iter().next()) {
            Some(a) => Intent::Do(a),
            None => Intent::Hold,
        }
    } else {
        Intent::Hold
    }
}
```

**(c) The tick walks the plan.** Rewrite `DriveMovements::step` to step the agent through actions over `(from, to]`: at each step, compute the drive (fold `drank`), call `decide`, execute the returned `Action` (a `MoveTo` commits `agent-at` at `day += MOVE_DURATION`; a `Drink` commits `drank` at `day` and resets the drive), and stop when `day > to` or `Intent::Hold` with the drive below `act` and at home (idle). Use a **strict-progress guard** (each executed action advances `day` by `MOVE_DURATION > 0`; a `Hold` while idle breaks; cap total steps at a budget to guarantee termination). `MOVE_DURATION` is an authored const (e.g. `0.1` day/room — the one action-duration judgment call, §8). Provenance: a move → `"walking to water (thirst)"`, a drink → `"drank (thirst sated)"`.

Concretely the loop shape:
```rust
fn step(&self, frozen: &Ledger) -> Vec<Fact> {
    let mut out = Vec::new();
    for npc in &self.npcs {
        let water = &npc.resource;
        let mut pos = agent_position(frozen, npc, self.from);
        let mut day = self.from.day;
        // A scratch ledger view isn't available; track drank locally: derive the
        // starting drive from `frozen`, then simulate forward, updating a local
        // last_drank as we emit DRANK facts.
        let mut last_drank = frozen.find(DRANK).filter(|f| f.subject == npc.entity)
            .filter_map(|f| f.day).fold(0.0_f64, f64::max);
        let mut steps = 0usize;
        const MAX_STEPS: usize = 10_000;
        loop {
            if day > self.to.day || steps >= MAX_STEPS { break; }
            steps += 1;
            let drive = (self.params.rise * (day - last_drank)).clamp(0.0, 1.0);
            let view = Perceived { position: pos.clone(), drive };
            match decide(&view, &npc.home, water, &self.params, PLAN_BUDGET) {
                Intent::Do(Action::MoveTo(n)) => {
                    day += MOVE_DURATION;
                    if day > self.to.day { break; }
                    out.push(agent_at_fact(npc.entity, &n, day, "walking (thirst)"));
                    pos = n;
                }
                Intent::Do(Action::Drink) => {
                    out.push(drank_fact(npc.entity, day, "drank (sated)"));
                    last_drank = day;
                }
                Intent::Hold => {
                    // idle at home: jump to the next act-crossing (closed-form)
                    let next_act = last_drank + self.params.act / self.params.rise;
                    if next_act <= day || next_act > self.to.day { break; }
                    day = next_act;
                }
            }
        }
    }
    out
}
```

(Provide `agent_at_fact`/`drank_fact` helpers; `PLAN_BUDGET`/`MOVE_DURATION` authored consts. The `Intent::Hold` idle-jump keeps the loop advancing to the next thirst rather than spinning. Guard: `next_act <= day` breaks — the strict-progress guarantee.)

- [ ] **Step 4: Run + full suite + mutation-verify + commit**

Run: `cargo test -p hornvale-vessel` then `cargo nextest run --workspace 2>&1 | tee /tmp/hv-f-t3.txt`
Expected: PASS. Mutation-verify: (a) drop the `drink` from the plan execution (never commit `drank`) → the drive never resets → `a_thirsty_agent_plans_to_water_and_the_tick_walks_it`'s drank-count reds; (b) collapse the `Intent::Hold` idle-jump guard (`next_act <= day` allowed) — confirm the MAX_STEPS cap still terminates (no hang). Report both. Confirm the genesis-zero-`agent-at` pin still green and `git diff --stat book/src/laboratory/ cli/tests/fixtures/` empty.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(vessel): the planner walks — drive folds drank, decide runs A*, the tick walks the plan (the-foresight T3)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 4: Observation + close

**Files:**
- Modify: `windows/vessel/src/session.rs` (`why` recounts the journey + drink; the `needs` felt-state read still works with the new `drive_at`)
- Re-baseline: `book/src/gallery/possession-over-time-seed-42.md` (game-layer — planned journeys + drink)
- Create: `book/src/chronicle/the-foresight.md` (+ SUMMARY.md); `docs/retrospectives/the-foresight.md`
- Modify: `book/src/frontier/idea-registry.md` (PSY-6, UNI-19, MAP-27 re-score), stale "reactive"/"clock"/"one-step" liveness lines, the spec header (STATUS)

- [ ] **Step 1: `why` recounts the journey + felt state**

Confirm `why <npc>`/`recount` now shows the dated `agent-at` moves and the `drank` (its provenance naming the goal). Confirm `needs` reads the new `drive_at` (no `resource` arg). Add/extend a test: after a `wait` where an NPC journeyed and drank, `why` recounts the moves + the drink. Mutation-verify (blank the drink provenance → reds).

- [ ] **Step 2: Re-baseline the over-time transcript + final byte-identity**

```bash
bash scripts/regenerate-artifacts.sh
git status --porcelain
```
Expected: ONLY `book/src/gallery/possession-over-time-seed-42.md` (and possibly the day-0 `possession-seed-42.md` `wait` line, as in The Wanting/Quickening) changed — game-layer, now planned journeys. If any census/laboratory/fixture/almanac drifts, STOP (BLOCKED). Then:
```bash
git diff $(git merge-base HEAD main)..HEAD --stat -- book/src/laboratory/ cli/tests/fixtures/ book/src/gallery/almanac-seed-42-sky.md | cat   # EMPTY
make gate 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-f-final.txt
```
Expected: `make gate` green; full suite exit 0; the genesis diff empty.

- [ ] **Step 3: Chronicle** (`book/src/chronicle/the-foresight.md`)

At the book's altitude: the agent gains foresight — it can PLAN, not just react. GOAP is A*; the precondition chain (Drink gated by at-water forces the journey — the difference from pathfinding); the general kernel A* engine (UNI-19: one engine across the tenses — name the future tenses: navigation, backward-confabulation, prophecy); the reserved-seam payoff (the planner is the body The Wanting reserved); PATH DETERMINISM (A* natively deterministic, no RNG — why the sim can plan); the shadow posture (planner runs only in the possess session; genesis byte-identical); what stays reserved (arbitration, the psychology-vector cost, planning-over-belief — the next three body-swaps). Add SUMMARY.md line. `mdbook build book` clean.

- [ ] **Step 4: Freshness sweep + re-score + retro**

- `grep -rn "reactive\|one-step\|clock schedule\|goes to the water one step" book/src/` — update the liveness chapters (movement is now PLANNED).
- `book/src/frontier/idea-registry.md`: re-score **PSY-6** (the goal rung / A* planner shipped; arbitration/cost/belief deferred), **UNI-19** (the kernel A* engine shipped; the other tenses its instantiations), note **MAP-27** as the action-DSL followup. UNI-20 unchanged. Edit ONLY this worktree's copy.
- `book/src/open-questions.md`: check the "someone walks in it"/living-world bet — the first *planning* agent likely moves it; re-score per decision 0030 if so, SAY SO.
- `cargo test -p hornvale --test docs_consistency` — PASS.
- Retrospective `docs/retrospectives/the-foresight.md` (one page, process): the UNI-19 general-engine decision (build A* once, tense-agnostic); ideonomy's two passes (the graph organon → determinism keystone + tense-agnosticism; the scale organon → the precondition chain); the reserved-seam payoff (The Wanting's #9 designed exactly this); the drive refactor (proximity → planned drink); whatever execution taught. Spec header STATUS line.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A && git commit -m "docs(the-foresight): close — why/journey, chronicle, PSY-6/UNI-19 re-score, retrospective (the-foresight T4)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

**STOP after this task: G6 is a hard stop.** Present the post-G3 ledger digest (the A\*-determinism #3, the general-engine #2, and the determinism #8 leading) to Nathan; the FF, push, pull, teardown are his calls, under `closing-a-campaign`.

---

## Execution notes for the dispatcher

- Subagent-driven (dispatching-hornvale-subagents): sonnet floor, dispatch preamble verbatim, `cd` into the worktree, challenge-response on return. **Require `type-audit check` exit 0 in every dispatch** (the-wanting T2 shipped a broken tag by skipping it). **Watch for parking** — resume via SendMessage.
- `make preflight` at every task boundary; absorb main INTO the branch on NO-GO (parallel sessions are relentless — expect ≥1 absorption, possibly touching windows/vessel again).
- Stage 1 is kernel-only (the A* — the reusable engine); Stage 2 is pure vessel (the GOAP space); Stage 3 is the integration + the drive/tick refactor (the churn); Stage 4 re-baselines only the game-layer transcript.
- **The A\* determinism keystone (#3):** a reviewer must confirm the tie-break is a TOTAL order (no HashMap, the frontier keyed by `(f, g, state)`) and that the generator PLANTS equal-cost paths — the c4/c5 generator-coverage lesson.
