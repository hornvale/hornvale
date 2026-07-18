# The Ordination (ECS Campaign 6) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive the sim's system-execution order from declared read/write predicate sets instead of a hand-written sequence: a kernel `CapabilitySchema` with a topological `schedule()` (stable-label tie-break), a single-writer-per-functional-predicate load check, the genesis pipeline formalized as declared systems (proven a valid topological sort of the declared DAG), and the bulk-synchronous tick mechanism made concrete (toy-tested) — all byte-identical.

**Architecture:** Kernel gets the domain-agnostic types (`System` declaration, `CapabilitySchema`, `schedule()`, `single_writer_check()`, the `TickSystem`/`tick` BSP mechanism). Worldgen composes one `System` declaration per genesis stage and validates them at load. Genesis keeps executing through `build_to`'s hand-order unchanged — the schedule *validates* that order (shadow), it does not reroute execution. Nothing is serialized; the campaign commits no fact and registers no predicate. Spec: `docs/superpowers/specs/2026-07-17-systems-schedule-ecs-campaign-6-design.md` (approved at G3).

**Tech Stack:** Rust edition 2024, std + serde/serde_json only. `cargo nextest`.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy-enforced).
- **No new dependencies** — serde + serde_json workspace allowlist.
- **No wall-clock time**; **no new seed stream draws** (do not touch any `streams` module — the schedule/tick add no randomness).
- **Every public item gets a one-line doc comment** (`#![warn(missing_docs)]`) **and a `type-audit:` verdict tag on every primitive at a pub boundary** — tags are struct/fn-level doc comments, NOT per-field (the c5 convention). type-audit runs in `make gate`.
- **`cargo fmt` as the final step before every commit**; clippy `-D warnings`.
- **Byte-identical (spec §3):** c6 commits NO fact, registers NO predicate, changes NO serialized byte. The artifact drift-check is the proof — after every task, `git diff` over `book/` and `cli/tests/fixtures/` must be empty (no regen needed; nothing derived from a world changes). If any world/almanac/census/fixture drifts, STOP — that is a contract violation, not noise.
- **Mutation-verify every new test** before counting it done: break the code the test guards, see RED, restore, see GREEN. Report it.
- **The keystone is anti-vacuity-guarded:** a topological-consistency check passes *vacuously* if the declarations under-specify dependencies. Every keystone assertion pairs a positive (the hand-order is a valid topo sort) with a negative (a dependency-violating order is REJECTED). A keystone without its negative half is incomplete.
- Worktree: `.claude/worktrees/the-ordination`, branch `worktree-the-ordination`. All commands from the worktree root.
- Commit-message trailer on every commit: `Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm`.

---

### Task 0: Baseline gate in the cold worktree

**Files:** none modified.

**Interfaces:** Produces a pinned GREEN baseline and confirms the branch base is current with main.

- [ ] **Step 1: Preflight**

Run: `make preflight`
Expected: GO. If main moved, merge main INTO `worktree-the-ordination`, re-run the full gate, note the absorption.

- [ ] **Step 2: Baseline suite**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-c6-baseline.txt`
Expected: exit 0, all pass. If red, STOP and diff against main's known-green state — do not proceed on a red baseline.

- [ ] **Step 3: fmt + clippy baseline**

Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: clean. No commit.

---

### Task 1: `System` declaration + `CapabilitySchema` + `schedule()`

**Files:**
- Create: `kernel/src/schedule.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod schedule;` + re-exports)

**Interfaces:**
- Produces:
  - `System { label: &'static str, reads: BTreeSet<&'static str>, writes: BTreeSet<&'static str> }` with `System::new(label, reads: &[&'static str], writes: &[&'static str]) -> System`.
  - `CapabilitySchema { systems: Vec<System> }` with `CapabilitySchema::new(systems: Vec<System>) -> Self`.
  - `CapabilitySchema::schedule(&self) -> Result<Vec<&'static str>, ScheduleError>` — topological order of the read/write DAG, ties broken by ascending `label`.
  - `CapabilitySchema::is_valid_order(&self, order: &[&'static str]) -> bool` — true iff `order` is a permutation of the systems' labels that respects every dependency edge (used by the keystone and its anti-vacuity negative).
  - `ScheduleError::Cycle { labels: Vec<&'static str> }`.
- Re-export from `kernel/src/lib.rs`: `pub use schedule::{System, CapabilitySchema, ScheduleError};`

- [ ] **Step 1: Write the failing tests** (`kernel/src/schedule.rs` `tests`)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn sys(label: &'static str, reads: &[&'static str], writes: &[&'static str]) -> System {
        System::new(label, reads, writes)
    }

    #[test]
    fn schedule_orders_a_chain() {
        // a writes P, b reads P and writes Q, c reads Q -> a,b,c
        let schema = CapabilitySchema::new(vec![
            sys("c", &["Q"], &[]),
            sys("a", &[], &["P"]),
            sys("b", &["P"], &["Q"]),
        ]);
        assert_eq!(schema.schedule().unwrap(), vec!["a", "b", "c"]);
    }

    #[test]
    fn independent_systems_break_ties_by_label() {
        // no edges: pure label order, NOT input order
        let schema = CapabilitySchema::new(vec![
            sys("terrain", &[], &["elev"]),
            sys("sky", &[], &["sun"]),
        ]);
        assert_eq!(schema.schedule().unwrap(), vec!["sky", "terrain"]);
    }

    #[test]
    fn diamond_respects_all_edges_and_ties_by_label() {
        // root -> {left, right} -> join. left/right independent -> label order.
        let schema = CapabilitySchema::new(vec![
            sys("join", &["L", "R"], &[]),
            sys("right", &["base"], &["R"]),
            sys("left", &["base"], &["L"]),
            sys("root", &[], &["base"]),
        ]);
        let order = schema.schedule().unwrap();
        assert_eq!(order, vec!["root", "left", "right", "join"]);
    }

    #[test]
    fn a_cycle_is_an_error_naming_its_systems() {
        // a reads Q writes P; b reads P writes Q -> cycle
        let schema = CapabilitySchema::new(vec![
            sys("a", &["Q"], &["P"]),
            sys("b", &["P"], &["Q"]),
        ]);
        match schema.schedule() {
            Err(ScheduleError::Cycle { labels }) => {
                assert!(labels.contains(&"a") && labels.contains(&"b"));
            }
            other => panic!("expected Cycle, got {other:?}"),
        }
    }

    #[test]
    fn is_valid_order_accepts_topo_sorts_and_rejects_violations() {
        let schema = CapabilitySchema::new(vec![
            sys("a", &[], &["P"]),
            sys("b", &["P"], &[]),
        ]);
        assert!(schema.is_valid_order(&["a", "b"]));   // edge a->b respected
        assert!(!schema.is_valid_order(&["b", "a"]));  // edge violated
        assert!(!schema.is_valid_order(&["a"]));       // not a permutation
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel --lib schedule::`
Expected: compile error (types don't exist). That is the red.

- [ ] **Step 3: Implement** (`kernel/src/schedule.rs`)

```rust
//! The capability schema and the derived execution schedule (metaplan §4.6).
//! A `System` declares the predicates it reads and writes; the schedule is the
//! topological order of the resulting data-dependency DAG, tie-broken by stable
//! label — a derived view over the declarations, never serialized. Genesis is
//! this schedule's first turn (tick 0); the running tick is deferred.
use std::collections::{BTreeMap, BTreeSet};

/// One system's capability declaration: a stable label plus the predicate
/// names it reads and writes. A declaration, not a behavior (the runnable
/// interface is `TickSystem`). The schedulable unit UNI-21 names.
/// type-audit: bare-ok(identifier-text: label)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct System {
    /// Stable id; the schedule's tie-break key (never registration position).
    pub label: &'static str,
    /// Predicate names this system reads.
    pub reads: BTreeSet<&'static str>,
    /// Predicate names this system writes.
    pub writes: BTreeSet<&'static str>,
}

impl System {
    /// Declare a system from slices (deduped into the sets).
    /// type-audit: bare-ok(identifier-text: label, reads, writes)
    pub fn new(label: &'static str, reads: &[&'static str], writes: &[&'static str]) -> System {
        System {
            label,
            reads: reads.iter().copied().collect(),
            writes: writes.iter().copied().collect(),
        }
    }
}

/// The systems' declarations — the capability schema. Build-state, never
/// serialized; the schedule and the single-writer check are pure functions of it.
#[derive(Clone, Debug, Default)]
pub struct CapabilitySchema {
    /// The declared systems.
    pub systems: Vec<System>,
}

/// Why a schedule could not be derived.
/// type-audit: bare-ok(identifier-text: labels)
#[derive(Debug, PartialEq, Eq)]
pub enum ScheduleError {
    /// The read/write DAG has a cycle; no total order exists. Names the
    /// systems remaining when Kahn's algorithm stalled.
    Cycle {
        /// The labels caught in (or downstream of) the cycle.
        labels: Vec<&'static str>,
    },
    /// A functional predicate is written by more than one system.
    MultipleWriters {
        /// The over-written functional predicate.
        predicate: &'static str,
        /// The systems declaring it in `writes`, ascending by label.
        systems: Vec<&'static str>,
    },
}

impl CapabilitySchema {
    /// Construct from declarations.
    pub fn new(systems: Vec<System>) -> Self {
        CapabilitySchema { systems }
    }

    /// The dependency edges `writer_label -> reader_label` for every predicate a
    /// system writes and another reads. Deterministic (BTree-ordered).
    fn edges(&self) -> BTreeSet<(&'static str, &'static str)> {
        let mut edges = BTreeSet::new();
        for w in &self.systems {
            for r in &self.systems {
                if w.label == r.label {
                    continue;
                }
                if w.writes.iter().any(|p| r.reads.contains(p)) {
                    edges.insert((w.label, r.label));
                }
            }
        }
        edges
    }

    /// Topological order, ties broken by ascending label (Kahn's algorithm with
    /// a stable-label ready set). `Err(Cycle)` if no total order exists.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn schedule(&self) -> Result<Vec<&'static str>, ScheduleError> {
        let edges = self.edges();
        let mut indegree: BTreeMap<&'static str, usize> =
            self.systems.iter().map(|s| (s.label, 0usize)).collect();
        for (_, to) in &edges {
            *indegree.get_mut(to).expect("edge endpoints are systems") += 1;
        }
        let mut order = Vec::with_capacity(self.systems.len());
        loop {
            // ready = indegree 0, not yet emitted; BTreeMap iteration is label-sorted
            let next = indegree.iter().find(|(_, &d)| d == 0).map(|(&l, _)| l);
            let Some(label) = next else { break };
            indegree.remove(&label);
            order.push(label);
            for (from, to) in &edges {
                if *from == label
                    && let Some(d) = indegree.get_mut(to)
                {
                    *d -= 1;
                }
            }
        }
        if order.len() != self.systems.len() {
            let mut labels: Vec<&'static str> = indegree.keys().copied().collect();
            labels.sort_unstable();
            return Err(ScheduleError::Cycle { labels });
        }
        Ok(order)
    }

    /// True iff `order` is a permutation of the systems' labels that respects
    /// every dependency edge (writer before reader). The keystone's checker.
    /// type-audit: bare-ok(identifier-text: order)
    pub fn is_valid_order(&self, order: &[&'static str]) -> bool {
        let want: BTreeSet<&'static str> = self.systems.iter().map(|s| s.label).collect();
        let got: BTreeSet<&'static str> = order.iter().copied().collect();
        if want != got || order.len() != self.systems.len() {
            return false;
        }
        let pos: BTreeMap<&'static str, usize> =
            order.iter().enumerate().map(|(i, &l)| (l, i)).collect();
        self.edges().iter().all(|(from, to)| pos[from] < pos[to])
    }
}
```

(The `let ... && let ...` and `if let` chains are edition-2024 let-chains — already used in the codebase. If clippy/rustc rejects the exact form, use a nested `if let`.)

- [ ] **Step 4: Run to pass**

Run: `cargo test -p hornvale-kernel --lib schedule::`
Expected: PASS (all 5). Mutation-verify: change the ready-set pick from label-sorted (`iter().find`) to `.last()` or input order — `independent_systems_break_ties_by_label` and `diamond...` must RED. Restore.

- [ ] **Step 5: Re-export + fmt + clippy + commit**

Add to `kernel/src/lib.rs`: `pub mod schedule;` and `pub use schedule::{CapabilitySchema, ScheduleError, System};`.

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
cargo test -p hornvale-kernel --lib schedule::
git add -A && git commit -m "feat(kernel): System declaration + CapabilitySchema::schedule (Kahn, stable-label tie-break) (ecs-c6 T1)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 2: The single-writer-per-functional-predicate check

**Files:**
- Modify: `kernel/src/schedule.rs` (`single_writer_check` + tests)

**Interfaces:**
- Consumes: `CapabilitySchema`, `ScheduleError::MultipleWriters` (Task 1); `ConceptRegistry` + `PredicateDef.functional` (`kernel/src/registry.rs`).
- Produces: `CapabilitySchema::single_writer_check(&self, registry: &ConceptRegistry) -> Result<(), ScheduleError>`.

- [ ] **Step 1: Write the failing tests** (append to `kernel/src/schedule.rs` `tests`)

```rust
#[test]
fn single_writer_check_passes_when_each_functional_predicate_has_one_writer() {
    use crate::registry::ConceptRegistry;
    let mut r = ConceptRegistry::default();
    r.register_predicate("elev", true, "functional").unwrap();
    r.register_predicate("here", false, "non-functional").unwrap();
    let schema = CapabilitySchema::new(vec![
        sys("terrain", &[], &["elev"]),
        // two writers of a NON-functional predicate is allowed
        sys("a", &[], &["here"]),
        sys("b", &[], &["here"]),
    ]);
    assert!(schema.single_writer_check(&r).is_ok());
}

#[test]
fn single_writer_check_rejects_two_writers_of_a_functional_predicate() {
    use crate::registry::ConceptRegistry;
    let mut r = ConceptRegistry::default();
    r.register_predicate("elev", true, "functional").unwrap();
    let schema = CapabilitySchema::new(vec![
        sys("terrain", &[], &["elev"]),
        sys("rogue", &[], &["elev"]),
    ]);
    match schema.single_writer_check(&r) {
        Err(ScheduleError::MultipleWriters { predicate, systems }) => {
            assert_eq!(predicate, "elev");
            assert_eq!(systems, vec!["rogue", "terrain"]); // ascending label
        }
        other => panic!("expected MultipleWriters, got {other:?}"),
    }
}

#[test]
fn single_writer_check_ignores_unregistered_predicates() {
    // A predicate not in the registry has unknown functionality; the check only
    // constrains registered functional predicates (the load path registers all
    // real predicates before checking).
    use crate::registry::ConceptRegistry;
    let r = ConceptRegistry::default();
    let schema = CapabilitySchema::new(vec![
        sys("a", &[], &["ghost"]),
        sys("b", &[], &["ghost"]),
    ]);
    assert!(schema.single_writer_check(&r).is_ok());
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel --lib schedule::single_writer`
Expected: compile error (method missing).

- [ ] **Step 3: Implement** (in `impl CapabilitySchema`)

```rust
    /// Enforce metaplan §7: each *functional* predicate is written by at most
    /// one system, making same-tick write conflicts unrepresentable. Reads the
    /// registry for functionality; non-functional and unregistered predicates
    /// are unconstrained. `Err(MultipleWriters)` names the first offending
    /// predicate (ascending) and its writers (ascending label).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn single_writer_check(
        &self,
        registry: &crate::registry::ConceptRegistry,
    ) -> Result<(), ScheduleError> {
        let mut writers: BTreeMap<&'static str, Vec<&'static str>> = BTreeMap::new();
        for s in &self.systems {
            for &p in &s.writes {
                writers.entry(p).or_default().push(s.label);
            }
        }
        for (&predicate, systems) in &writers {
            let functional = registry.predicate(predicate).is_some_and(|d| d.functional);
            if functional && systems.len() > 1 {
                let mut systems = systems.clone();
                systems.sort_unstable();
                return Err(ScheduleError::MultipleWriters { predicate, systems });
            }
        }
        Ok(())
    }
```

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-kernel --lib schedule::`
Expected: PASS. Mutation-verify: drop the `functional &&` guard — `single_writer_check_passes...` (the non-functional two-writer case) must RED; restore.

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add -A && git commit -m "feat(kernel): single-writer-per-functional-predicate check (metaplan §7) (ecs-c6 T2)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 3: Worldgen declarations + the SCHEDULE ≡ HAND-ORDER keystone + load-path check

**Files:**
- Create: `windows/worldgen/src/schedule.rs` (the genesis-stage declarations + the keystone test)
- Modify: `windows/worldgen/src/lib.rs` (add `mod schedule;`, call `single_writer_check` in `build_to`'s load path, add a `BuildError::Schedule` variant)

**Interfaces:**
- Consumes: everything from Tasks 1–2.
- Produces: `hornvale_worldgen::schedule::genesis_systems() -> CapabilitySchema` — one `System` per genesis stage; `hornvale_worldgen::schedule::GENESIS_HAND_ORDER: &[&'static str]` — the `build_to` execution order (the ground-truth reference). `BuildError::Schedule(ScheduleError)`.

**Context for the implementer — the hand-order ground truth (from `build_to`):**
The genesis stages commit in this order: `world-entity` (mints + names the world entity, a seed-only source), then `sky` (`hornvale_astronomy::facts::genesis`), `terrain` (`hornvale_terrain::facts::genesis`), `settlement` (`hornvale_settlement::genesis`), `culture` (`hornvale_culture::genesis`), `religion` (`hornvale_religion::genesis`), `species` (`species_genesis`), `paleoclimate` (`hornvale_paleoclimate::genesis`). This list is `GENESIS_HAND_ORDER`.

**The reads/writes per stage** you DERIVE by reading each genesis function's committed predicates (`.commit(... PREDICATE ...)`) for `writes`, and the facts it looks up (`value_of`/`text_of`/`find`/`facts_about` on predicates from OTHER stages) for `reads`. The predicate constants are each domain's exported `pub const` (e.g. `hornvale_terrain`'s elevation predicates, `hornvale_settlement`'s `PEOPLED_BY`, etc.). **The keystone (below) is what proves you got the dependency edges right** — declare a stage's real cross-stage reads, or the keystone's negative half fails.

- [ ] **Step 1: Write the keystone test FIRST** (`windows/worldgen/src/schedule.rs`)

```rust
//! The genesis pipeline as declared systems, and the keystone that pins the
//! declarations to the working pipeline: the genesis hand-order is a valid
//! topological sort of the declared read/write DAG (and a dependency-violating
//! order is rejected — the anti-vacuity half). Genesis still executes via
//! `build_to`'s hand-order; this schema validates it (shadow, spec §4.5).
use hornvale_kernel::{CapabilitySchema, System};

/// The order genesis stages actually commit in `build_to` — the ground truth
/// the derived schedule is validated against.
pub const GENESIS_HAND_ORDER: &[&str] = &[
    "world-entity", "sky", "terrain", "settlement", "culture", "religion", "species",
    "paleoclimate",
];

/// One capability declaration per genesis stage. Reads/writes are the predicate
/// constants each stage actually reads and commits (spec §4.5).
pub fn genesis_systems() -> CapabilitySchema {
    // The implementer fills each System's reads/writes from the genesis code.
    // Example shape (NOT the final content — derive the real predicates):
    //   System::new("terrain", &[], &[hornvale_terrain::ELEV_PREDICATE, ...])
    //   System::new("settlement", &[hornvale_terrain::ELEV_PREDICATE, ...], &[PEOPLED_BY, ...])
    CapabilitySchema::new(vec![
        // ... one System::new(...) per GENESIS_HAND_ORDER entry ...
    ])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hand_order_is_a_valid_topological_sort_of_the_declarations() {
        // POSITIVE half: the order genesis actually runs respects every declared
        // dependency edge. (Byte-identity is automatic — genesis uses this order;
        // this proves the DECLARATIONS capture the pipeline's real dependencies.)
        let schema = genesis_systems();
        assert!(
            schema.is_valid_order(GENESIS_HAND_ORDER),
            "GENESIS_HAND_ORDER must respect every declared read/write edge"
        );
        // schedule() must also succeed (no declared cycle) and be a valid order.
        let derived = schema.schedule().expect("declared DAG is acyclic");
        assert!(schema.is_valid_order(&derived));
    }

    #[test]
    fn a_dependency_violating_order_is_rejected() {
        // ANTI-VACUITY half: an order that runs a reader before its writer must
        // be REJECTED. If the declarations under-specify (miss a real edge), this
        // wrongly passes — so this test guards against vacuous declarations.
        // Pick two stages with a real dependency: settlement READS terrain's
        // output, so [terrain before settlement] is required; the swap must fail.
        let schema = genesis_systems();
        let mut broken: Vec<&str> = GENESIS_HAND_ORDER.to_vec();
        let ti = broken.iter().position(|&s| s == "terrain").unwrap();
        let si = broken.iter().position(|&s| s == "settlement").unwrap();
        broken.swap(ti, si);
        assert!(
            !schema.is_valid_order(&broken),
            "swapping terrain and settlement must violate a declared edge — if this \
             passes, the settlement->terrain read dependency is not declared"
        );
    }

    #[test]
    fn genesis_declarations_pass_the_single_writer_check() {
        // The §7 contract MEASURED on the real declarations against the full
        // registry (all domains registered). Its outcome is reported at close.
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        hornvale_worldgen::register_all_concepts(&mut world.registry)
            .expect("concept registration");
        genesis_systems()
            .single_writer_check(&world.registry)
            .expect("no functional predicate has two declared genesis writers");
    }
}
```

**Note for the implementer:** the third test names `hornvale_worldgen::register_all_concepts` — use whatever the worldgen public function is that registers every domain's concepts into a registry (grep `register_concepts` / how `World::new` + worldgen wire the registry in `build_to`). If none is public, build the registry the same way `build_to` does. The point: the check runs against the REAL, fully-populated registry.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen --lib schedule::`
Expected: FAIL — `genesis_systems()` is empty, so `is_valid_order` over an empty schema returns false (labels mismatch) and the anti-vacuity test's swap wrongly passes. That is the red that drives filling in the declarations.

- [ ] **Step 3: Fill in the real declarations**

Read each genesis function and populate `genesis_systems()` with the true reads/writes. Derive:
- `writes` = every predicate constant the stage `.commit(...)`s.
- `reads` = every predicate (from ANOTHER stage) the stage looks up (`value_of`/`text_of`/`find`/`facts_about`/`query_by_object`).
Iterate until BOTH keystone tests pass: the positive (hand-order valid) AND the negative (terrain/settlement swap rejected). The negative passing is the proof the terrain→settlement edge is really declared.

- [ ] **Step 4: Run to pass**

Run: `cargo test -p hornvale-worldgen --lib schedule::`
Expected: PASS (all 3). Mutation-verify: delete `terrain`'s output predicate from `settlement`'s `reads` (or delete the whole edge) → `a_dependency_violating_order_is_rejected` must RED (the swap now wrongly passes). Restore. This proves the anti-vacuity guard bites.

- [ ] **Step 5: Wire the single-writer check into the load path**

In `windows/worldgen/src/lib.rs`: add `mod schedule;` and `pub use` it if needed; add `BuildError::Schedule(hornvale_kernel::ScheduleError)` with a `Display` arm; and in `build_to` (near the existing `check_integrity`/`WorldComponents::assemble` load validation, AFTER all `register_concepts` have populated `world.registry`) call:

```rust
    schedule::genesis_systems()
        .single_writer_check(&world.registry)
        .map_err(BuildError::Schedule)?;
```

(This makes the §7 contract a real load-time gate on every world build, not just a test. If it ever fails on a real build, that is a genuine multi-writer bug — the spec §3 risk note.)

- [ ] **Step 6: Full build + byte-identity confirm**

Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-c6-t3.txt`
Expected: exit 0 (the load-path check passes on every world-building test — the measured outcome of the §7 contract on real declarations). Then:

Run: `git status --porcelain && git diff --stat book/ cli/tests/fixtures/`
Expected: EMPTY — c6 commits no fact and registers no predicate, so no artifact drifts. If anything drifts, STOP: a declaration accidentally committed or registered something.

- [ ] **Step 7: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add -A && git commit -m "feat(worldgen): genesis stages as declared systems + SCHEDULE==HAND-ORDER keystone + load-time single-writer check (ecs-c6 T3)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 4: The BSP tick mechanism (toy-tested)

**Files:**
- Modify: `kernel/src/schedule.rs` (`TickSystem` trait + `tick` fn + toy-system tests)
- Modify: `kernel/src/lib.rs` (re-export `TickSystem`, `tick`)

**Interfaces:**
- Consumes: `Ledger`, `Fact`, `Value`, `ConceptRegistry`, `EntityId` (kernel); a schedule order (`&[&'static str]`).
- Produces:
  - `trait TickSystem { fn label(&self) -> &'static str; fn step(&self, frozen: &Ledger) -> Vec<Fact>; }`
  - `fn tick(frozen: &Ledger, systems: &[&dyn TickSystem], order: &[&'static str], registry: &ConceptRegistry) -> Result<Ledger, LedgerError>`

- [ ] **Step 1: Write the failing tests** (append to `kernel/src/schedule.rs` `tests`)

```rust
// A toy system that reads the frozen ledger and writes one fact. Proves the
// bulk-synchronous mechanism without any real domain.
struct CountThenName {
    label: &'static str,
    subject: crate::EntityId,
    predicate: &'static str,
    // writes Number(count of facts it saw in the frozen ledger)
}
impl TickSystem for CountThenName {
    fn label(&self) -> &'static str { self.label }
    fn step(&self, frozen: &crate::Ledger) -> Vec<crate::Fact> {
        vec![crate::Fact {
            subject: self.subject,
            predicate: self.predicate.to_string(),
            object: crate::Value::Number(frozen.len() as f64),
            place: None,
            day: None,
            provenance: self.label.to_string(),
        }]
    }
}

#[test]
fn tick_reads_frozen_snapshot_so_systems_do_not_see_each_others_writes() {
    use crate::registry::ConceptRegistry;
    let mut r = ConceptRegistry::default();
    r.register_predicate("count-a", false, "").unwrap();
    r.register_predicate("count-b", false, "").unwrap();
    let mut base = crate::Ledger::default();
    let e = base.mint_entity();
    // frozen ledger has 0 facts. Two systems each write frozen.len() == 0.
    let a = CountThenName { label: "a", subject: e, predicate: "count-a" };
    let b = CountThenName { label: "b", subject: e, predicate: "count-b" };
    let systems: Vec<&dyn TickSystem> = vec![&a, &b];
    let next = tick(&base, &systems, &["a", "b"], &r).unwrap();
    // BOTH see the frozen snapshot (0 facts) — b does NOT see a's write.
    assert_eq!(next.value_of(e, "count-a"), Some(&crate::Value::Number(0.0)));
    assert_eq!(next.value_of(e, "count-b"), Some(&crate::Value::Number(0.0)));
    // The next tick's ledger has the base fact-count plus the two writes.
    assert_eq!(next.len(), base.len() + 2);
}

#[test]
fn tick_commits_in_schedule_order_then_is_deterministic() {
    use crate::registry::ConceptRegistry;
    let mut r = ConceptRegistry::default();
    r.register_predicate("count-a", false, "").unwrap();
    r.register_predicate("count-b", false, "").unwrap();
    let mut base = crate::Ledger::default();
    let e = base.mint_entity();
    let a = CountThenName { label: "a", subject: e, predicate: "count-a" };
    let b = CountThenName { label: "b", subject: e, predicate: "count-b" };
    let systems: Vec<&dyn TickSystem> = vec![&b, &a]; // input order reversed...
    // ...but `order` is the schedule: commits follow it, not input order.
    let n1 = tick(&base, &systems, &["a", "b"], &r).unwrap();
    let n2 = tick(&base, &systems, &["a", "b"], &r).unwrap();
    // Deterministic: same base + same order -> byte-identical ledgers.
    assert_eq!(
        serde_json::to_string(&n1).unwrap(),
        serde_json::to_string(&n2).unwrap()
    );
    // Commit order follows the schedule: a's fact precedes b's.
    let preds: Vec<&str> = n1.iter().map(|f| f.predicate.as_str()).collect();
    let ia = preds.iter().position(|&p| p == "count-a").unwrap();
    let ib = preds.iter().position(|&p| p == "count-b").unwrap();
    assert!(ia < ib, "commits follow schedule order [a, b], not input order [b, a]");
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel --lib schedule::tick`
Expected: compile error (`TickSystem`, `tick` missing).

- [ ] **Step 3: Implement** (`kernel/src/schedule.rs`)

```rust
use crate::ledger::{Fact, Ledger, LedgerError};
use crate::registry::ConceptRegistry;

/// The runnable interface a sim-dynamic system implements — the bulk-
/// synchronous step (decision-ledger #40). `step` reads the FROZEN tick-N
/// ledger (it never sees another system's same-tick write) and returns the
/// facts to commit into tick-N+1. Exercised by a toy system this campaign; no
/// real domain implements it yet (genesis is not rerouted through `tick`).
pub trait TickSystem {
    /// The system's stable label (its schedule key).
    /// type-audit: bare-ok(identifier-text: return)
    fn label(&self) -> &'static str;
    /// Read the frozen tick-N ledger; return the facts to commit next tick.
    fn step(&self, frozen: &Ledger) -> Vec<Fact>;
}

/// Run one bulk-synchronous tick: every system reads the same `frozen` ledger
/// (parallel-safe, pure), then their writes are committed into a fresh tick-N+1
/// ledger **in `order`** (the schedule), single-threaded and deterministic.
/// Cross-system effects have one tick of latency by default. Returns the
/// tick-N+1 ledger. `order` lists system labels; systems not in `order` are a
/// caller error (skipped).
/// type-audit: bare-ok(identifier-text: order)
pub fn tick(
    frozen: &Ledger,
    systems: &[&dyn TickSystem],
    order: &[&'static str],
    registry: &ConceptRegistry,
) -> Result<Ledger, LedgerError> {
    let mut next = frozen.clone();
    for &label in order {
        if let Some(s) = systems.iter().find(|s| s.label() == label) {
            for fact in s.step(frozen) {
                next.commit(fact, registry)?;
            }
        }
    }
    Ok(next)
}
```

(`CountThenName`'s struct literal in the test omits a field the compiler will flag if the struct shape differs — match the fields you declared. Add `Fact`/`Value`/`EntityId` imports the tests need.)

- [ ] **Step 4: Run + mutation-verify + commit**

Run: `cargo test -p hornvale-kernel --lib schedule::tick`
Expected: PASS. Mutation-verify: in `tick`, change `s.step(frozen)` to `s.step(&next)` (read the mutating ledger instead of the frozen snapshot) → `tick_reads_frozen_snapshot...` must RED (b would see a's write, count-b becomes 1.0). Restore.

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add -A && git commit -m "feat(kernel): the bulk-synchronous tick mechanism — TickSystem + tick, frozen-snapshot isolation, schedule-order commit (ecs-c6 T4)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

---

### Task 5: Deterministic schedule budget + book + retro + close

**Files:**
- Create: the schedule-order pin test (append to `windows/worldgen/src/schedule.rs` tests)
- Create: `book/src/chronicle/the-ordination.md` (+ `book/src/SUMMARY.md` entry)
- Modify: `book/src/frontier/idea-registry.md` (UNI-22 row)
- Modify: any stale book chapter describing the genesis order as hand-authored
- Create: `docs/retrospectives/the-ordination.md`
- Modify: the spec header (STATUS line)

- [ ] **Step 1: Pin the derived schedule (deterministic budget, spec §6)** (`windows/worldgen/src/schedule.rs` tests)

```rust
#[test]
fn derived_schedule_is_pinned() {
    // The derived schedule of the real genesis declarations is a fixed,
    // drift-checkable sequence. If this changes, a declaration's reads/writes
    // moved — re-derive deliberately and confirm byte-identity still holds
    // (genesis executes the hand-order, not this, so worlds are unaffected).
    let order = genesis_systems().schedule().expect("acyclic");
    // Fill EXPECTED from the actual derived order once the declarations are
    // final (label-sorted tie-break, NOT the hand-order):
    let expected: Vec<&str> = vec![/* the implementer pins the real derived order */];
    assert_eq!(order, expected);
}
```

**Note:** run `genesis_systems().schedule()` once, read the printed order, and pin it as `expected`. This order is label-tie-broken and will differ from `GENESIS_HAND_ORDER` — that difference is expected and harmless (spec §8), and the `hand_order_is_a_valid_topological_sort` keystone already proved both are valid.

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-worldgen --lib schedule::derived_schedule_is_pinned`
Expected: PASS.

- [ ] **Step 3: Final byte-identity + full gate**

```bash
git status --porcelain            # expect clean (nothing regenerated all campaign)
make gate 2>&1 | tail -20
cargo nextest run --workspace 2>&1 | tee /tmp/hv-c6-final.txt
git diff $(git merge-base HEAD main)..HEAD --stat -- book/src/gallery/ book/src/laboratory/ cli/tests/fixtures/ | cat
```
Expected: `make gate` green; full suite exit 0; the last diff EMPTY (no world artifact touched all campaign — the byte-identity proof).

- [ ] **Step 4: Chronicle** (`book/src/chronicle/the-ordination.md`)

Write at the book's altitude (technical, comprehensible without the code): order as a derived view — the genesis pipeline's hand-written sequence replaced (in principle) by a topological sort of declared read/write dependencies; the keystone (the hand-order is a valid topological sort of the declared DAG, its anti-vacuity negative); genesis as tick 0 and the bulk-synchronous mechanism proven by a toy system; the single-writer contract as a measured load-time gate; the shadow posture (the schedule validates, does not yet execute — the running tick is deferred). Add the `book/src/SUMMARY.md` chronicle-list line, matching neighbors. Run `mdbook build book` — expect clean.

- [ ] **Step 5: Freshness sweep + UNI-22 re-score**

- `grep -rn "hand-written\|hand-authored\|registration order\|build_to" book/src/` — update any chapter that presents the genesis order as inherently hand-authored (note it is now a validated topological sort of declared dependencies).
- `book/src/frontier/idea-registry.md` UNI-22 row: append campaign-6 shipped (systems-as-declarations, the capability-schema-derived schedule, the single-writer contract, the BSP tick mechanism proven at genesis/tick-0; byte-identical; the running tick deferred) and that campaign 7 (spatial partition) is the last substrate campaign. **Edit ONLY this worktree's copy** (parallel sessions may touch main's copy — a merge conflict at close is normal, resolved additively).
- Check `book/src/open-questions.md` for any bet this moves (per the c4/c5 precedent, a substrate milestone usually moves no taste bet — grep the schedule/systems/ECS terms and SAY SO in the report if unchanged).
- Run `cargo test -p hornvale --test docs_consistency` — expect PASS.

- [ ] **Step 6: Retrospective** (`docs/retrospectives/the-ordination.md`)

One page, process not product: the "genesis = tick 0" reframing that scoped the campaign; the anti-vacuity keystone (a topological-consistency check is vacuous without its negative half — the PROC-11 construction-not-recognition lesson); the single-writer check's MEASURED outcome on the real declarations (pass, or the violation it found); the 5 followup-register rows (copy from `.superpowers/sdd/followups.md`); whatever execution taught. Update the spec header with a `> **STATUS: SHIPPED** …` line.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A && git commit -m "docs(the-ordination): close — chronicle, UNI-22 re-score, freshness sweep, retrospective + derived-schedule pin (ecs-c6 T5)

Claude-Session: https://claude.ai/code/session_012WgbEfkHW1kByYK9vPMasm"
```

**STOP after this task: G6 is a hard stop.** Present the post-G3 ledger digest (with the single-writer check's measured outcome leading, and any byte-identity risk if a reconciliation was needed) to Nathan; the FF, push, and any teardown are his calls, run under `closing-a-campaign`.

---

## Execution notes for the dispatcher

- Subagent-driven (dispatching-hornvale-subagents): sonnet floor, dispatch
  preamble verbatim, `cd` into the worktree, challenge-response on return.
- `make preflight` at every task boundary; absorb main INTO the branch on NO-GO.
- No task regenerates artifacts — c6 is byte-identical and commits nothing. Task 3
  and Task 5 explicitly confirm the empty artifact diff; a non-empty diff is a bug.
- Tasks 1, 2, 4 are kernel-only and fast; Task 3 is the substantive one (deriving
  the real declarations) and its keystone is the campaign's spine.
