# The Brattice Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put gates on the Crosscut's series-parallel descent plan — doors with keys, sumps that need swimming, chutes that drop you a floor and need flight to climb — stamped by a post-growth pass from a frozen ten-row pattern inventory, realized in the rock as places, carried in the ledger as Things, and enforced by the walk, with solvability for a body holding nothing as the invariant.

**Architecture:** A gate is four co-located parts kept apart. (1) `windows/worldgen/src/brattice.rs` stamps `Gate`s on `Edge`s and `KeyFor`s on `Node`s of a finished `DescentPlan`, one pattern draw per realm, refusing any placement the product-graph solver says is unsolvable. (2) `windows/vessel/src/underworld_level/` realizes places: a `Threshold` cell on every passage's divider, `Deep` runs for sumps, `Drop` lips for chutes. (3) `windows/vessel/src/thing.rs` + a new `descent_thing.rs` give doors and keys identities keyed on plan positions, with the Chattel's openness/lockedness/custody folds. (4) `Underground::peek` gains an actor-aware seam beside `movement_mode`, reading the body's `Locomotion` (a sparse `hornvale_species` component store) and the door's fold. A realization-witness test pins that (2) and (3) carry exactly what (1) stamped.

**Tech Stack:** Rust 2024, std only (`serde`/`serde_json`/`libm` allowlist); `BTreeMap`/`BTreeSet`/`Vec` only; `cargo nextest`; the type-audit and placement-audit lints; the game client is Rust outside the workspace (`clients/game`, its own `deno`/cargo gate in the `clients` lane set).

**Spec:** `docs/superpowers/specs/2026-09-02-the-brattice-design.md` (approved at `f2488619f`; ledger `docs/superpowers/ledgers/2026-09-02-the-brattice.md`, entries #1–#8 and the G3 record).

## Global Constraints

- **Layering:** `kernel → domains/* → windows/* → cli`. `hornvale_worldgen` may not depend on `hornvale_vessel`; the plan's `Capability` enum is worldgen's own and vessel maps it onto `MovementMode`.
- **Determinism:** no `HashMap`/`HashSet`; float sorts use `total_cmp`; no wall clock. Every new draw is counted in `DescentPlan.dof` and the exact identity of spec §3.8 holds: `dof = 1 + levels + stairs + 4·(realms − fallback_realms) + 2·extensions + failed_draws + realms`.
- **Stream labels are permanent contracts.** One new leg, `underworld/gate/v1/pattern`, additive; the four `underworld/plan/v1` legs are unchanged in label and consumption order. Never bump a plan leg in this campaign.
- **FRAME-tier (decision 0069):** nothing built here is serialized. The one save-format consequence is the descent key's identity (spec §5); Task 0's grep guards that no OTHER committing reader of a level appears.
- **Every crate is `#![warn(missing_docs)]`:** every pub item, field and variant gets a one-line doc. Every primitive at a pub boundary carries a `type-audit:` tag (`bare-ok(<class>)`, e.g. `bare-ok(index: pattern)`, `bare-ok(flag: swim)`, `bare-ok(count: skipped_patterns)`); `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` is in the commit gate and is default-deny.
- **Every seed-looping test carries a `/// claim:` tag** (decision 0093): `claim: invariant(seed: 0..N) — …`, `claim: rate(…)`, `claim: readout(…)`, `claim: structural(…)`, `claim: sanctioned-sweep(…)`. `gate-commit` reddens without one.
- **Brief snippets are not fmt-clean.** Run `cargo fmt` before every commit; `cargo clippy --workspace --all-targets -- -D warnings` must pass.
- **Commit gate:** `make gate-commit` runs on every commit through `scripts/hooks/pre-commit`. Stage gates via `make sluice-stage BRANCH=campaign/the-brattice REF=<full-sha>` after Tasks 2, 4 and 6. Absorb `origin/main` locally when the mouth refuses.
- **Generated artifacts:** `docs/audits/underworld-circuit-seed-panel.md` (declared as a FILE in `docs/generated-paths.txt`) and `clients/game/core/tests/fixtures/` regenerate via `make rebaseline`; read the diff against the spec's §5 branch tables, never assume it.
- **Inventory freeze:** `CYCLE_PATTERNS.len() == 10`, asserted. Adding a row after Task 1 lands is an epoch of `underworld/gate/v1`.
- **Ledger discipline (decision 0486):** rulings made during execution go into `docs/superpowers/ledgers/2026-09-02-the-brattice.md` as they occur, in the task's own section, and are committed with the task.
- **The board:** post a `technique` when something costs you an hour; post `notice` if you change a shared meaning (`MovementMode`, `LevelCellKind`, `THING_KINDS`).

---

## File structure

```
  windows/worldgen/src/brattice.rs           NEW  gate types, CYCLE_PATTERNS, the pass, the solver
  windows/worldgen/src/circuit.rs            MOD  Node.key, Edge.gate, DescentPlan.patterns/skipped_patterns,
                                                  gate_between(), plan_descent() calls the pass
  windows/worldgen/src/streams.rs            MOD  UNDERWORLD_GATE_PATTERN
  windows/worldgen/src/lib.rs                MOD  pub mod brattice
  windows/worldgen/src/circuit_readout.rs    MOD  four new sections (§4.1–4.4)
  cli/src/streams.rs                         MOD  stamp roster row
  windows/vessel/src/underworld_level/mod.rs MOD  three LevelCellKind variants, movement_mode, Level.thresholds,
                                                  threshold/sump/chute placement, repair exclusions, witness
  windows/vessel/src/level_doc.rs            MOD  three palette kinds; door marks
  windows/vessel/src/underground.rs          MOD  actor-aware peek, peek_stairs with Locomotion, refusals
  windows/vessel/src/body.rs                 MOD  Body::locomotion() accessor
  windows/vessel/src/session.rs              MOD  narration, take_stairs (chute), look/examine/take/open/close/drop underground
  windows/vessel/src/descent_thing.rs        NEW  key_role/door_role/ids, door_at, keys_in_region
  windows/vessel/src/thing.rs                MOD  set_lockedness_role, lying_at_place
  windows/vessel/src/affordance.rs           MOD  door row in object_registry
  domains/thing/src/lib.rs                   MOD  "door" in THING_KINDS + kinds::DOOR
  domains/species/src/lib.rs                 MOD  Locomotion + locomotion_registry()
  clients/game/core/src/level.rs             MOD  three glyph pairs + door mark glyph
  docs/audits/underworld-circuit-seed-panel.md  REGEN
  clients/game/core/tests/fixtures/          REGEN (branch table)
  book/src/chronicle/the-brattice.md         NEW; book/src/SUMMARY.md MOD
  docs/decisions/0616..0620-*.md             NEW
  docs/retrospectives/the-brattice.md        NEW
```

---

### Task 0: Preflight — the epoch grep, the baselines, the count

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-02-the-brattice.md` (append "## Task 0 record")
- No code.

**Interfaces:** Produces the two baseline copies later tasks diff against: `/tmp` is NOT allowed — use the scratchpad dir printed by your harness, or `.superpowers/sdd/baselines/` in the worktree (git-ignored).

- [ ] **Step 1: Re-run the Crosscut's epoch grep, unchanged.**

```bash
cd "$(git rev-parse --show-toplevel)"
grep -rn "Level\b\|LevelCellKind\|CellGrid" --include=*.rs windows domains kernel cli \
  | grep -v "windows/vessel/src/underworld_level/\|windows/vessel/src/underground.rs\|/tests/\|#\[cfg(test)\]" \
  | grep -n "commit\|Fact\|ledger"
echo "exit: $?"
```

Branch table (the spec's, §5):
- zero lines (exit 1) → proceed.
- any line → STOP. Paste the lines into the ledger and report to the controller; the campaign is an epoch and returns to G3.

- [ ] **Step 2: Baseline the two artifacts that will move.**

```bash
mkdir -p .superpowers/sdd/baselines
cp docs/audits/underworld-circuit-seed-panel.md .superpowers/sdd/baselines/circuit-panel.before.md
cp -r clients/game/core/tests/fixtures .superpowers/sdd/baselines/fixtures.before
git log -1 --format=%H > .superpowers/sdd/baselines/at-sha
```

- [ ] **Step 3: Count the construction sites the new fields will break, and record them.**

```bash
grep -n "Node {$\|Node { level\|Edge {$\|Edge { a" windows/worldgen/src/circuit.rs | wc -l
grep -rn "Level {$" --include=*.rs windows/vessel | grep -v "SessionLevel\|//" | wc -l
```

Expected shape: circuit.rs ≈ 10 sites; `Level {` literal sites in vessel ≈ 1–3. Whatever the numbers are, write them down; Tasks 1 and 3 must touch exactly those.

- [ ] **Step 4: Confirm the inventory count the spec froze.**

The spec §3.2 table has ten rows. Nathan confirmed the names at G3 (ledger, G3 record). Task 1 asserts `CYCLE_PATTERNS.len() == 10`. Nothing to run; note it.

- [ ] **Step 5: Ledger and commit.**

Append to the ledger:

```markdown
## Task 0 record

Epoch grep: <paste the command and its (empty) output and exit code>.
Baselines copied to `.superpowers/sdd/baselines/` at `<sha>`.
Literal sites: circuit.rs `Node {`/`Edge {` = <n>; vessel `Level {` = <m>.
Inventory frozen at 10 (G3 record).
```

```bash
git add docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "ledger(the-brattice): Task 0 preflight — epoch grep empty, baselines taken"
```

---

### Task 1: The gate types, the inventory, the pass, the solver

**Files:**
- Create: `windows/worldgen/src/brattice.rs`
- Modify: `windows/worldgen/src/circuit.rs` (`Node`, `Edge`, `DescentPlan`, `plan_descent` tail at `:786-791`, every `Node {`/`Edge {` literal, `dof_counts_every_draw` at `:1383-1460`)
- Modify: `windows/worldgen/src/streams.rs:228` (add one label after `UNDERWORLD_PLAN_STAIR`)
- Modify: `windows/worldgen/src/lib.rs` (`pub mod brattice;`)
- Modify: `cli/src/streams.rs:678-681` (add `"underworld/gate/pattern v1",` in sorted position)
- Test: in-module `#[cfg(test)] mod tests` of `brattice.rs`, plus the amended `dof_counts_every_draw`

**Interfaces:**
- Consumes: `circuit::{DescentPlan, Node, Edge, EdgeKind, Realm, RealmId, NodeId, LengthClass}`, `hornvale_terrain::CaveKind`, `crate::character::Character`, `hornvale_kernel::Stream` (`next_f64()`), `crate::streams::stream_labels`.
- Produces (read by Tasks 2–6):

```rust
// brattice.rs
pub enum Capability { Swim, Fly }
pub enum Requirement { Key(NodeId), Mode(Capability) }
pub enum Way { Open, Needs(Requirement) }
pub enum Hazard { Dangerous, Uncertain }
pub enum Persistence { Permanent, Collapsing }
pub struct Gate { pub toward_b: Way, pub toward_a: Way, pub hazard: Option<Hazard>,
                  pub persistence: Persistence, pub pattern: usize }
pub struct KeyFor(pub usize);                 // edge index into plan.edges
pub enum Skip { Inadmissible, Claimed, NoRoom, Unsolvable }
pub enum Outcome { Applied { pattern: usize }, Skipped { pattern: usize, why: Skip }, Inadmissible }
pub struct CyclePattern { pub name: &'static str, pub source: &'static str,
                          pub classes: &'static [LengthClass], pub span: Span,
                          pub gates: &'static [GateSpec], pub key: Option<KeySpec>,
                          pub hazard: Option<(Side, Hazard)>,
                          pub persistence: Option<(Side, Slot, Persistence)> }
pub const CYCLE_PATTERNS: &[CyclePattern];     // len 10
pub struct Body { pub swim: bool, pub fly: bool, pub keys: u64 }   // solver state; bit i = key node i held
pub const DEFAULT_BODY: Body;                  // walk+wade, nothing held
pub struct Reach { pub terminus: Option<u32>, pub keys: Vec<Option<u32>>, pub reached: Vec<bool> }
pub fn solvable(plan: &DescentPlan, body: Body) -> Reach;
pub fn resident(plan: &DescentPlan) -> Body;   // every key, every mode
pub fn key_nodes(plan: &DescentPlan) -> Vec<NodeId>;   // sorted; index = bit
pub fn gated_round_trip(plan: &DescentPlan, body: Body) -> Option<u32>;
pub fn ungated_round_trip(plan: &DescentPlan) -> u32;
pub(crate) fn stamp(plan: &mut DescentPlan, kind: CaveKind, character: Character,
                    pattern_leg: &mut Stream, dof: &mut u32);
// circuit.rs additions
pub struct Node { …, pub key: Option<crate::brattice::KeyFor> }
pub struct Edge { …, pub gate: Option<crate::brattice::Gate> }
pub struct DescentPlan { …, pub patterns: Vec<crate::brattice::Outcome>, pub skipped_patterns: u32 }
impl DescentPlan { pub fn gate_between(&self, a: NodeId, b: NodeId) -> Option<(usize, &Gate)>;
                   pub fn edge_index(&self, a: NodeId, b: NodeId) -> Option<usize>; }
```

- [ ] **Step 1: Add the stream label (worldgen) and the stamp roster row (cli).**

In `windows/worldgen/src/streams.rs`, directly after the `UNDERWORLD_PLAN_STAIR` line (`:228`), inside the same macro block:

```rust
    /// The Brattice: which cycle pattern a realm draws from the frozen
    /// inventory. One draw per realm, made even when zero or one row is
    /// admissible, so the draw count is data-independent (spec §3.2 step 2).
    UNDERWORLD_GATE_PATTERN = "underworld/gate/v1/pattern" => "which cycle pattern a realm draws from the frozen inventory";
```

In `cli/src/streams.rs`, the roster array around `:678-681` is sorted; add `"underworld/gate/pattern v1",` before `"underworld/plan/cycle v1",`. Run the roster's own test to learn the exact expected string shape if it differs:

```bash
cargo test -p hornvale --test suite -- streams 2>&1 | tail -20
```

If the test names a different spelling, use the spelling it prints and note it in the ledger.

- [ ] **Step 2: Write the failing tests for the types and the inventory (in `brattice.rs`).**

Create `windows/worldgen/src/brattice.rs` with the module doc and an empty body plus this test module; the tests will not compile yet, which is the red.

```rust
//! Gates on the descent plan (The Brattice; spec §3.1–3.4).
//!
//! A gate is a REQUIREMENT ON A WAY: what a body must hold or be to take one
//! direction of one edge. The pattern says WHERE (which side of a realm, near
//! or far); the rock and the work say WHAT (a door needs a maker, a sump
//! needs karst or fracture, a chute needs a floor below). The pass runs after
//! growth — `try_extend` splices chains into realm paths, so nothing may be
//! stamped on an edge the grammar might still rewrite — and every placement
//! is checked against solvability for a body holding nothing.
//!
//! FRAME-tier (decision 0069): derived with the plan, never serialized. The
//! one save-format consequence (spec §5) is that a descent key's IDENTITY is
//! a plan position, so a later grammar change is a real epoch.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::character::Character;
    use crate::circuit::{DescentPlan, LengthClass, plan_descent};
    use hornvale_kernel::{Band, Seed, Vertex};
    use hornvale_terrain::CaveKind;

    fn habitation_rungs() -> Vec<Band> {
        hornvale_terrain::rungs().iter().copied().filter(|r| *r != Band::Surface).collect()
    }
    fn plan(seed: u64, vertex: u32, kind: CaveKind, character: Character) -> DescentPlan {
        plan_descent(Seed(seed), Vertex(vertex), &habitation_rungs(), kind, character)
    }

    #[test]
    fn the_inventory_is_frozen_at_ten_rows() {
        assert_eq!(CYCLE_PATTERNS.len(), 10, "spec §3.2: ten rows, frozen at G3");
        let mut names: Vec<&str> = CYCLE_PATTERNS.iter().map(|p| p.name).collect();
        names.sort_unstable();
        names.dedup();
        assert_eq!(names.len(), 10, "pattern names are unique");
    }

    #[test]
    fn every_key_row_places_a_key_and_no_natural_row_does() {
        for p in CYCLE_PATTERNS {
            let wants_key = p.gates.iter().any(|g| matches!(g.way, WaySpec::Symmetric(ReqKind::Key)));
            assert_eq!(wants_key, p.key.is_some(), "{}: a Key gate needs a KeySpec and vice versa", p.name);
            let down_free = p.gates.iter().any(|g| matches!(g.way, WaySpec::DownFreeUpNeeds(_)));
            if down_free {
                assert_eq!(p.span, Span::CrossFloor, "{}: DownFreeUpNeeds is a stair's way", p.name);
            }
        }
    }

    /// claim: invariant(seed: 0..200) — every plan on every kind × character
    /// is SOLVABLE for the default body (terminus and every key reached),
    /// and every standable node is reachable by the resident (spec §3.4, §3.8).
    #[test]
    fn every_plan_is_solvable_for_a_body_holding_nothing() {
        for seed in 0..200u64 {
            for kind in [CaveKind::LavaTube, CaveKind::Fracture, CaveKind::Karst] {
                for ch in [Character::WildCave, Character::FungalGardens, Character::DrowTier] {
                    let p = plan(seed, 3, kind, ch);
                    let r = solvable(&p, DEFAULT_BODY);
                    assert!(r.terminus.is_some(), "seed {seed} {kind:?} {ch:?}: terminus unreachable");
                    assert!(r.keys.iter().all(|k| k.is_some()), "seed {seed} {kind:?} {ch:?}: a key is unreachable");
                    let all = solvable(&p, resident(&p));
                    assert!(all.reached.iter().all(|&x| x), "seed {seed}: the resident cannot reach every node");
                }
            }
        }
    }

    /// claim: invariant(seed: 0..200) — a node holds at most one key and an
    /// edge at most one gate; every gate lies on an existing edge and every
    /// key at an existing node (spec §3.8).
    #[test]
    fn gates_and_keys_claim_at_most_once_and_add_nothing() {
        for seed in 0..200u64 {
            let p = plan(seed, 1, CaveKind::Karst, Character::DrowTier);
            let keyed: Vec<_> = p.nodes.iter().filter(|n| n.key.is_some()).collect();
            for n in &keyed {
                let e = n.key.as_ref().unwrap().0;
                assert!(e < p.edges.len(), "key names a real edge");
                let g = p.edges[e].gate.as_ref().expect("a key's edge carries a gate");
                assert!(matches!(g.toward_a, Way::Needs(Requirement::Key(_))), "a key's gate needs a key");
            }
            // one key per lock and one lock per key
            let mut locks: Vec<usize> = keyed.iter().map(|n| n.key.as_ref().unwrap().0).collect();
            locks.sort_unstable();
            let before = locks.len();
            locks.dedup();
            assert_eq!(before, locks.len(), "seed {seed}: two keys for one lock");
        }
    }

    #[test]
    fn a_worked_karst_descent_eventually_carries_a_door_and_a_wild_one_never_does() {
        let mut doors_worked = 0;
        for seed in 0..60u64 {
            let p = plan(seed, 2, CaveKind::Karst, Character::DrowTier);
            doors_worked += p.edges.iter().filter(|e| matches!(e.gate.as_ref().map(|g| &g.toward_a), Some(Way::Needs(Requirement::Key(_))))).count();
            let w = plan(seed, 2, CaveKind::Karst, Character::WildCave);
            assert!(w.edges.iter().all(|e| !matches!(e.gate.as_ref().map(|g| &g.toward_a), Some(Way::Needs(Requirement::Key(_))))),
                "seed {seed}: a wild cave hung a door");
        }
        assert!(doors_worked > 0, "sixty worked karst descents and not one door: the Key rows never apply");
    }

    #[test]
    fn a_lava_tube_never_carries_a_sump_and_some_descent_carries_a_chute() {
        let mut chutes = 0;
        for seed in 0..60u64 {
            let p = plan(seed, 2, CaveKind::LavaTube, Character::WildCave);
            for e in &p.edges {
                if let Some(g) = &e.gate {
                    assert!(!matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Swim))), "seed {seed}: a sump in a lava tube");
                    if matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Fly))) { chutes += 1; }
                }
            }
        }
        assert!(chutes > 0, "sixty lava-tube descents and no chute");
    }

    #[test]
    fn a_chute_is_free_down_and_needs_flight_up_and_sits_on_a_stair() {
        for seed in 0..60u64 {
            let p = plan(seed, 4, CaveKind::Fracture, Character::WildCave);
            for e in &p.edges {
                if let Some(g) = &e.gate {
                    if matches!(g.toward_a, Way::Needs(Requirement::Mode(Capability::Fly))) {
                        assert!(matches!(e.kind, crate::circuit::EdgeKind::Stair { .. }));
                        assert!(matches!(g.toward_b, Way::Open), "down is free");
                    }
                }
            }
        }
    }

    #[test]
    fn the_plan_and_its_gates_are_deterministic_and_read_the_vertex() {
        let a = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        let b = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        assert_eq!(a, b);
        let c = plan(42, 2, CaveKind::Karst, Character::DrowTier);
        assert_ne!(a.patterns, c.patterns, "two vertices draw different patterns (or the leg ignores the vertex)");
    }

    #[test]
    fn length_class_gives_each_path_a_side() {
        assert_eq!(sides(LengthClass::LongShort), (Side::Long, Side::Short));
        assert_eq!(sides(LengthClass::ShortLong), (Side::Short, Side::Long));
        assert_eq!(sides(LengthClass::LongLong), (Side::Long, Side::Short), "tie-break: path_a is Long");
        assert_eq!(sides(LengthClass::ShortShort), (Side::Long, Side::Short));
    }
}
```

- [ ] **Step 3: Run the tests to see them fail to compile.**

```bash
cargo test -p hornvale-worldgen brattice 2>&1 | grep -E "^error|cannot find" | head -5
```

Expected: `cannot find` errors for `CYCLE_PATTERNS`, `solvable`, etc.

- [ ] **Step 4: Add the fields to `circuit.rs` and fix every literal.**

`Node` gains `pub key: Option<crate::brattice::KeyFor>` (doc: "The key this node holds, for the gate on that edge; `None` almost everywhere."). `Edge` gains `pub gate: Option<crate::brattice::Gate>` (doc: "A requirement on this edge's ways, if a pattern placed one (spec §3.1)."). Both types derive `Clone, Copy, Debug, PartialEq, Eq` today — `Gate` and `KeyFor` must therefore derive `Clone, Copy, Debug, PartialEq, Eq` too (they hold only enums and `usize`). `DescentPlan` gains:

```rust
    /// Per realm, in `realms` order: which pattern was drawn and whether it
    /// was applied or why it was skipped (spec §3.1).
    pub patterns: Vec<crate::brattice::Outcome>,
    /// Realms whose drawn pattern was skipped for any reason; one per skip.
    /// type-audit: bare-ok(count: skipped_patterns)
    pub skipped_patterns: u32,
```

Fix every `Node { … }` / `Edge { … }` literal in `circuit.rs` by adding `key: None` / `gate: None` (Task 0 counted ≈10). The `DescentPlan` literal in `Builder::new` gains `patterns: Vec::new(), skipped_patterns: 0`.

Add two accessors to `impl DescentPlan`:

```rust
    /// The index of the edge joining `a` and `b` in either order, if any.
    pub fn edge_index(&self, a: NodeId, b: NodeId) -> Option<usize> {
        self.edges.iter().position(|e| (e.a == a && e.b == b) || (e.a == b && e.b == a))
    }

    /// The gate on the edge joining `a` and `b`, with the edge's index.
    pub fn gate_between(&self, a: NodeId, b: NodeId) -> Option<(usize, &crate::brattice::Gate)> {
        let ix = self.edge_index(a, b)?;
        self.edges[ix].gate.as_ref().map(|g| (ix, g))
    }
```

Hook the pass into `plan_descent` (`:786-791`), AFTER `assign_realms` and `assign_depth` (the pass reads `depth` for Near/Far) and BEFORE `b.plan.dof = dof`:

```rust
    // 3. Attributes.
    assign_realms(&mut b.plan);
    assign_depth(&mut b.plan);
    // 4. Gates (The Brattice): one pattern draw per realm, after growth so
    // `extend` can no longer orphan an edge attribute.
    let mut pattern_leg = leg(seed, crate::streams::UNDERWORLD_GATE_PATTERN, vertex);
    crate::brattice::stamp(&mut b.plan, kind, character, &mut pattern_leg, &mut dof);
    b.plan.dof = dof;
    b.plan
```

Add `pub mod brattice;` to `windows/worldgen/src/lib.rs` beside `pub mod circuit;`.

- [ ] **Step 5: Write the types and the inventory in `brattice.rs`.**

```rust
use crate::character::Character;
use crate::circuit::{DescentPlan, EdgeKind, LengthClass, NodeId, Realm};
use hornvale_kernel::Stream;
use hornvale_terrain::CaveKind;
use std::collections::{BTreeSet, VecDeque};

/// What a body must BE to take a way (spec §3.1). Worldgen's own enum — the
/// vessel maps it onto its `MovementMode`, never the reverse (layering).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Capability {
    /// Deep water: a sump.
    Swim,
    /// The way back up a chute.
    Fly,
}

/// What a way demands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Requirement {
    /// The key lying at this node must be held.
    Key(NodeId),
    /// The body must carry this mode.
    Mode(Capability),
}

/// One direction of one edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Way {
    /// Nothing asked.
    Open,
    /// A requirement.
    Needs(Requirement),
}

/// Dormans' non-conditional lock flavours — STAMPED here, realized by nothing
/// (spec §3.1). Danger is The Plat's hoarder; secrecy needs the render seam.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hazard {
    /// The route costs something to take.
    Dangerous,
    /// The route is not known to exist.
    Uncertain,
}

/// Whether a lock stays as it is left — STAMPED, unread this campaign.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Persistence {
    /// Opened stays opened.
    Permanent,
    /// Shuts behind you (Dormans' blocked retreat).
    Collapsing,
}

/// A requirement on an edge's two ways (spec §3.1). `toward_b` is the way
/// from `Edge.a` to `Edge.b`; for a `Stair`, `a` is the upper node, so
/// `toward_b` is DOWN.
/// type-audit: bare-ok(index: pattern)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Gate {
    /// From `a` to `b` (down, on a stair).
    pub toward_b: Way,
    /// From `b` to `a` (up, on a stair).
    pub toward_a: Way,
    /// Dormans' flavour, stamped.
    pub hazard: Option<Hazard>,
    /// Stamped.
    pub persistence: Persistence,
    /// Index into [`CYCLE_PATTERNS`] of the row that placed this.
    pub pattern: usize,
}

/// A node holds the key for the gate on this edge (index into `plan.edges`).
/// type-audit: bare-ok(index: edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KeyFor(pub usize);

/// Why a drawn pattern was not applied (spec §3.2 step 3–4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Skip {
    /// No row admitted this realm's class, span, rock and work.
    Inadmissible,
    /// An edge or node the row wanted already carries a gate or key.
    Claimed,
    /// A side has no interior node where the key wants one.
    NoRoom,
    /// The default body could no longer reach the terminus or a key.
    Unsolvable,
}

/// One realm's result, in `plan.realms` order.
/// type-audit: bare-ok(index: pattern)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    /// The row at `pattern` was stamped in full.
    Applied {
        /// Index into [`CYCLE_PATTERNS`].
        pattern: usize,
    },
    /// The row at `pattern` was drawn and refused.
    Skipped {
        /// Index into [`CYCLE_PATTERNS`].
        pattern: usize,
        /// Why.
        why: Skip,
    },
    /// The admissible set was empty; the draw was made and discarded.
    Inadmissible,
}

/// Which floors a realm's two paths touch.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Span {
    /// Both paths on the anchor level.
    SameFloor,
    /// `path_b` descends.
    CrossFloor,
    /// Either.
    Either,
}

/// A realm's two paths by relative length. For `LongLong`/`ShortShort`,
/// `path_a` is `Long` by convention (spec §3.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    /// The longer path (or `path_a` on a tie).
    Long,
    /// The shorter path (or `path_b` on a tie).
    Short,
}

/// Which end of a path, by `depth`: the shared endpoint nearer the entrance is `Near`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    /// The endpoint with the smaller depth.
    Near,
    /// The other.
    Far,
}

/// What kind of requirement a row asks for; the rock and work resolve it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReqKind {
    /// A door and its key — needs a maker.
    Key,
    /// A sump on a passage, a chute on a stair.
    Natural,
}

/// The shape of a gate a row places.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WaySpec {
    /// The same requirement both ways.
    Symmetric(ReqKind),
    /// Down free, up needs — legal only on a stair (a cross-floor short side's near end).
    DownFreeUpNeeds(ReqKind),
}

/// Where a row puts a gate.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GateSpec {
    /// Which path.
    pub side: Side,
    /// Which end of it; the gate sits on the edge adjacent to that endpoint.
    pub slot: Slot,
    /// Symmetric or asymmetric, key or natural.
    pub way: WaySpec,
}

/// Where a row puts the key: the interior node of `side` adjacent to `slot`'s endpoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KeySpec {
    /// Which path.
    pub side: Side,
    /// Which end.
    pub slot: Slot,
}

/// One row of the frozen inventory (spec §3.2). Data, not code: the pass
/// reads rows, never matches on names.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: source)
#[derive(Debug)]
pub struct CyclePattern {
    /// Dormans' label or the Crosscut organon's cell.
    pub name: &'static str,
    /// Where the row comes from.
    pub source: &'static str,
    /// Which realm classes admit it.
    pub classes: &'static [LengthClass],
    /// Which spans admit it.
    pub span: Span,
    /// Gates to place.
    pub gates: &'static [GateSpec],
    /// Key to place, if a gate wants one.
    pub key: Option<KeySpec>,
    /// A hazard stamped on every edge of one side.
    pub hazard: Option<(Side, Hazard)>,
    /// A persistence stamped on one edge.
    pub persistence: Option<(Side, Slot, Persistence)>,
}

use LengthClass::{LongLong, LongShort, ShortLong, ShortShort};

/// The inventory, frozen at G3 (spec §3.2). Ten rows; the count is asserted.
pub const CYCLE_PATTERNS: &[CyclePattern] = &[
    CyclePattern { name: "two-alternative-paths", source: "Dormans Fig. 9.8", classes: &[LongLong], span: Span::Either, gates: &[], key: None, hazard: None, persistence: None },
    CyclePattern { name: "hidden-shortcut", source: "Dormans Fig. 9.8", classes: &[LongShort], span: Span::SameFloor, gates: &[], key: None, hazard: Some((Side::Short, Hazard::Uncertain)), persistence: None },
    CyclePattern { name: "dangerous-route", source: "Dormans Fig. 9.8", classes: &[LongShort], span: Span::SameFloor, gates: &[], key: None, hazard: Some((Side::Short, Hazard::Dangerous)), persistence: None },
    CyclePattern { name: "lock-and-key-cycle", source: "Dormans Fig. 9.8", classes: &[ShortLong], span: Span::SameFloor,
        gates: &[GateSpec { side: Side::Short, slot: Slot::Near, way: WaySpec::Symmetric(ReqKind::Key) }],
        key: Some(KeySpec { side: Side::Long, slot: Slot::Far }), hazard: None, persistence: None },
    CyclePattern { name: "the-sump", source: "Dormans, the lock-and-key aside (conditional lock)", classes: &[ShortLong], span: Span::SameFloor,
        gates: &[GateSpec { side: Side::Short, slot: Slot::Near, way: WaySpec::Symmetric(ReqKind::Natural) }],
        key: None, hazard: None, persistence: None },
    CyclePattern { name: "patrol-path", source: "Dormans Fig. 9.8", classes: &[ShortShort], span: Span::SameFloor, gates: &[], key: None, hazard: Some((Side::Long, Hazard::Dangerous)), persistence: None },
    CyclePattern { name: "blocked-retreat", source: "Dormans Fig. 9.8", classes: &[LongLong, ShortLong], span: Span::SameFloor, gates: &[], key: None, hazard: None,
        persistence: Some((Side::Long, Slot::Near, Persistence::Collapsing)) },
    CyclePattern { name: "the-chute", source: "Crosscut organon, PREDICTED (long a / short b, two floors)", classes: &[LongShort], span: Span::CrossFloor,
        gates: &[GateSpec { side: Side::Short, slot: Slot::Near, way: WaySpec::DownFreeUpNeeds(ReqKind::Natural) }],
        key: None, hazard: None, persistence: None },
    CyclePattern { name: "key-downstairs-lock-upstairs", source: "Crosscut organon, PREDICTED (short a / long b, two floors)", classes: &[ShortLong], span: Span::CrossFloor,
        gates: &[GateSpec { side: Side::Short, slot: Slot::Near, way: WaySpec::Symmetric(ReqKind::Key) }],
        key: Some(KeySpec { side: Side::Long, slot: Slot::Far }), hazard: None, persistence: None },
    CyclePattern { name: "the-landing-hall", source: "Crosscut organon, PREDICTED (short a / short b, two floors); Alexander 133", classes: &[ShortShort], span: Span::CrossFloor, gates: &[], key: None, hazard: None, persistence: None },
];
```

Note on `patrol-path`: the spec says "hazard Dangerous on both" sides; a row stamps one side. Stamp `Side::Long` (path_a, the existing segment — the patrolled way in) and record in the row's doc that "both" is the realm; a second stamp on `Short` would `Claimed`-collide with nothing but is redundant for a stamp nothing reads. **Ledger this as a Task 1 ruling** (one line: "patrol-path stamps path_a; both sides is the realm's reading").

- [ ] **Step 6: Write the geometry helpers and the pass.**

```rust
/// Which side each path is, from the class (spec §3.2: ties give `path_a` Long).
pub fn sides(class: LengthClass) -> (Side, Side) {
    match class {
        LongShort | LongLong | ShortShort => (Side::Long, Side::Short),
        ShortLong => (Side::Short, Side::Long),
    }
}

fn path_of<'a>(realm: &'a Realm, side: Side, class: LengthClass) -> &'a [NodeId] {
    let (a_side, _) = sides(class);
    if side == a_side { &realm.path_a } else { &realm.path_b }
}

fn span_of(plan: &DescentPlan, realm: &Realm) -> Span {
    let anchor = realm.anchor_level;
    if realm.path_b.iter().any(|&n| plan.nodes[n].level != anchor) { Span::CrossFloor } else { Span::SameFloor }
}

/// The shared endpoints as (near, far) by `depth`; ties give `path_a[0]` Near.
fn ends(plan: &DescentPlan, realm: &Realm) -> (NodeId, NodeId) {
    let s = realm.path_a[0];
    let e = *realm.path_a.last().expect("a path has two endpoints");
    if plan.nodes[e].depth < plan.nodes[s].depth { (e, s) } else { (s, e) }
}

/// The edge of `path` adjacent to the endpoint `slot` names, as `(a, b)` in path order.
fn edge_at(path: &[NodeId], near: NodeId, slot: Slot) -> (NodeId, NodeId) {
    let at_start = path[0] == near;
    match (slot, at_start) {
        (Slot::Near, true) | (Slot::Far, false) => (path[0], path[1]),
        _ => (path[path.len() - 2], path[path.len() - 1]),
    }
}

/// The interior node of `path` adjacent to the endpoint `slot` names; `None` if the path has no interior.
fn interior_at(path: &[NodeId], near: NodeId, slot: Slot) -> Option<NodeId> {
    if path.len() < 3 { return None; }
    let at_start = path[0] == near;
    Some(match (slot, at_start) {
        (Slot::Near, true) | (Slot::Far, false) => path[1],
        _ => path[path.len() - 2],
    })
}

fn worked(character: Character) -> bool {
    matches!(character, Character::DrowTier)
}

/// Spec §3.3: is this row's every requirement realizable in this rock and work?
fn admissible(row: &CyclePattern, kind: CaveKind, character: Character, class: LengthClass, span: Span) -> bool {
    if !row.classes.contains(&class) { return false; }
    if !(row.span == Span::Either || row.span == span) { return false; }
    row.gates.iter().all(|g| match g.way {
        WaySpec::Symmetric(ReqKind::Key) => worked(character),
        WaySpec::Symmetric(ReqKind::Natural) => !matches!(kind, CaveKind::LavaTube),
        WaySpec::DownFreeUpNeeds(ReqKind::Natural) => true,
        WaySpec::DownFreeUpNeeds(ReqKind::Key) => false, // no row says this; refuse rather than invent
    })
}

/// The pass (spec §3.2). One draw per realm, always; deterministic thereafter.
pub(crate) fn stamp(plan: &mut DescentPlan, kind: CaveKind, character: Character, pattern_leg: &mut Stream, dof: &mut u32) {
    let realms = plan.realms.clone();
    for (rix, realm) in realms.iter().enumerate() {
        let span = span_of(plan, realm);
        let admissible: Vec<usize> = (0..CYCLE_PATTERNS.len())
            .filter(|&i| admissible(&CYCLE_PATTERNS[i], kind, character, realm.class, span))
            .collect();
        let r = pattern_leg.next_f64();
        *dof += 1;
        if admissible.is_empty() {
            plan.patterns.push(Outcome::Inadmissible);
            plan.skipped_patterns += 1;
            continue;
        }
        let pick = admissible[((r * admissible.len() as f64) as usize).min(admissible.len() - 1)];
        match try_apply(plan, realm, rix, pick) {
            Ok(()) => plan.patterns.push(Outcome::Applied { pattern: pick }),
            Err(why) => {
                plan.patterns.push(Outcome::Skipped { pattern: pick, why });
                plan.skipped_patterns += 1;
            }
        }
    }
    debug_assert_eq!(plan.patterns.len(), plan.realms.len());
}

fn try_apply(plan: &mut DescentPlan, realm: &Realm, _rix: usize, pick: usize) -> Result<(), Skip> {
    let row = &CYCLE_PATTERNS[pick];
    let class = realm.class;
    let (near, _far) = ends(plan, realm);
    // Resolve everything first, then check claims, then stamp tentatively.
    let key_node = match row.key {
        Some(k) => Some(interior_at(path_of(realm, k.side, class), near, k.slot).ok_or(Skip::NoRoom)?),
        None => None,
    };
    let mut stamps: Vec<(usize, Gate)> = Vec::new();
    for g in row.gates {
        let (a, b) = edge_at(path_of(realm, g.side, class), near, g.slot);
        let ix = plan.edge_index(a, b).expect("a realm path edge exists");
        if plan.edges[ix].gate.is_some() { return Err(Skip::Claimed); }
        let req = match g.way {
            WaySpec::Symmetric(ReqKind::Key) | WaySpec::DownFreeUpNeeds(ReqKind::Key) => Requirement::Key(key_node.expect("checked by every_key_row_places_a_key")),
            WaySpec::Symmetric(ReqKind::Natural) => Requirement::Mode(Capability::Swim),
            WaySpec::DownFreeUpNeeds(ReqKind::Natural) => Requirement::Mode(Capability::Fly),
        };
        let gate = match g.way {
            WaySpec::Symmetric(_) => Gate { toward_b: Way::Needs(req), toward_a: Way::Needs(req), hazard: None, persistence: Persistence::Permanent, pattern: pick },
            WaySpec::DownFreeUpNeeds(_) => {
                if !matches!(plan.edges[ix].kind, EdgeKind::Stair { .. }) { return Err(Skip::NoRoom); }
                // `a` is the upper node on a Stair, so toward_b is down.
                Gate { toward_b: Way::Open, toward_a: Way::Needs(req), hazard: None, persistence: Persistence::Permanent, pattern: pick }
            }
        };
        stamps.push((ix, gate));
    }
    if let Some((side, hz)) = row.hazard {
        let path = path_of(realm, side, class);
        for w in path.windows(2) {
            let ix = plan.edge_index(w[0], w[1]).expect("path edge");
            if plan.edges[ix].gate.is_some() || stamps.iter().any(|(s, _)| *s == ix) { return Err(Skip::Claimed); }
            stamps.push((ix, Gate { toward_b: Way::Open, toward_a: Way::Open, hazard: Some(hz), persistence: Persistence::Permanent, pattern: pick }));
        }
    }
    if let Some((side, slot, ps)) = row.persistence {
        let (a, b) = edge_at(path_of(realm, side, class), near, slot);
        let ix = plan.edge_index(a, b).expect("path edge");
        if plan.edges[ix].gate.is_some() || stamps.iter().any(|(s, _)| *s == ix) { return Err(Skip::Claimed); }
        stamps.push((ix, Gate { toward_b: Way::Open, toward_a: Way::Open, hazard: None, persistence: ps, pattern: pick }));
    }
    if let Some(n) = key_node {
        if plan.nodes[n].key.is_some() { return Err(Skip::Claimed); }
    }
    // Tentative stamp.
    for (ix, g) in &stamps { plan.edges[*ix].gate = Some(*g); }
    if let Some(n) = key_node {
        let lock_ix = stamps.iter().find(|(_, g)| matches!(g.toward_a, Way::Needs(Requirement::Key(_)))).map(|(ix, _)| *ix).expect("a key row has a key gate");
        plan.nodes[n].key = Some(KeyFor(lock_ix));
    }
    let reach = solvable(plan, DEFAULT_BODY);
    if reach.terminus.is_none() || reach.keys.iter().any(|k| k.is_none()) {
        for (ix, _) in &stamps { plan.edges[*ix].gate = None; }
        if let Some(n) = key_node { plan.nodes[n].key = None; }
        return Err(Skip::Unsolvable);
    }
    Ok(())
}
```

- [ ] **Step 7: Write the solver.**

```rust
/// The traverser's state for the solver: modes it carries and keys it holds
/// (bit `i` = the `i`-th node of [`key_nodes`]).
/// type-audit: bare-ok(flag: swim), bare-ok(flag: fly), bare-ok(bitset: keys)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Body {
    /// Carries `Swim`.
    pub swim: bool,
    /// Carries `Fly`.
    pub fly: bool,
    /// Keys held, as a bitset over [`key_nodes`].
    pub keys: u64,
}

/// Walk and wade, holding nothing: the intruder (spec §3.4).
pub const DEFAULT_BODY: Body = Body { swim: false, fly: false, keys: 0 };

/// Every key, every mode: the resident.
pub fn resident(plan: &DescentPlan) -> Body {
    let n = key_nodes(plan).len();
    Body { swim: true, fly: true, keys: if n >= 64 { u64::MAX } else { (1u64 << n) - 1 } }
}

/// Nodes holding a key, ascending; the bit index of each.
pub fn key_nodes(plan: &DescentPlan) -> Vec<NodeId> {
    plan.nodes.iter().enumerate().filter(|(_, n)| n.key.is_some()).map(|(i, _)| i).collect()
}

/// What the default body — or any body — reaches (spec §3.4): shortest gated
/// distances to the terminus and to each key node, and which nodes were
/// reached at all. Product graph over `(node, keys)`; keys are only gained.
pub struct Reach {
    /// Hops to the terminus, if reachable. type-audit: bare-ok(count: terminus)
    pub terminus: Option<u32>,
    /// Hops to each key node in [`key_nodes`] order. type-audit: bare-ok(count: keys)
    pub keys: Vec<Option<u32>>,
    /// Per node: reached in any state. type-audit: bare-ok(flag: reached)
    pub reached: Vec<bool>,
}

fn passes(way: Way, body: Body, keys: &[NodeId]) -> bool {
    match way {
        Way::Open => true,
        Way::Needs(Requirement::Mode(Capability::Swim)) => body.swim,
        Way::Needs(Requirement::Mode(Capability::Fly)) => body.fly,
        Way::Needs(Requirement::Key(n)) => {
            let bit = keys.iter().position(|&k| k == n).expect("a key requirement names a key node");
            body.keys & (1 << bit) != 0
        }
    }
}

/// Breadth-first over the product graph from `(entrance, body.keys)`;
/// returns the first-visit distance per `(node, keys)`.
fn bfs(plan: &DescentPlan, start: NodeId, body: Body) -> std::collections::BTreeMap<(NodeId, u64), u32> {
    let keys = key_nodes(plan);
    assert!(keys.len() <= 64, "the bitset holds 64 keys; a descent has a handful");
    let mut dist = std::collections::BTreeMap::new();
    let mut q = VecDeque::new();
    let mut held = body.keys;
    if let Some(bit) = keys.iter().position(|&k| k == start) { held |= 1 << bit; }
    dist.insert((start, held), 0u32);
    q.push_back((start, held));
    while let Some((n, held)) = q.pop_front() {
        let d = dist[&(n, held)];
        let here = Body { keys: held, ..body };
        for (ix, e) in plan.edges.iter().enumerate() {
            let (to, way) = if e.a == n { (e.b, e.gate.map_or(Way::Open, |g| g.toward_b)) }
                            else if e.b == n { (e.a, e.gate.map_or(Way::Open, |g| g.toward_a)) }
                            else { continue };
            let _ = ix;
            if !passes(way, here, &keys) { continue; }
            let mut next_held = held;
            if let Some(bit) = keys.iter().position(|&k| k == to) { next_held |= 1 << bit; }
            if dist.contains_key(&(to, next_held)) { continue; }
            dist.insert((to, next_held), d + 1);
            q.push_back((to, next_held));
        }
    }
    dist
}

/// Spec §3.4.
pub fn solvable(plan: &DescentPlan, body: Body) -> Reach {
    let keys = key_nodes(plan);
    let dist = bfs(plan, plan.entrance, body);
    let best = |n: NodeId| dist.iter().filter(|((m, _), _)| *m == n).map(|(_, d)| *d).min();
    let mut reached = vec![false; plan.nodes.len()];
    for ((n, _), _) in &dist { reached[*n] = true; }
    Reach { terminus: best(plan.terminus), keys: keys.iter().map(|&k| best(k)).collect(), reached }
}

/// Shortest entrance → terminus → entrance for `body` through the gates
/// (spec §4.2), or `None` if the terminus is unreachable.
pub fn gated_round_trip(plan: &DescentPlan, body: Body) -> Option<u32> {
    let out = bfs(plan, plan.entrance, body);
    let mut best: Option<u32> = None;
    for ((n, held), d) in &out {
        if *n != plan.terminus { continue; }
        let back = bfs(plan, plan.terminus, Body { keys: *held, ..body });
        if let Some(r) = back.iter().filter(|((m, _), _)| *m == plan.entrance).map(|(_, d)| *d).min() {
            let total = d + r;
            best = Some(best.map_or(total, |b| b.min(total)));
        }
    }
    best
}

/// The same round trip with every gate ignored: twice the ungated distance.
pub fn ungated_round_trip(plan: &DescentPlan) -> u32 {
    2 * plan.nodes[plan.terminus].depth as u32
}
```

(`Node.depth` is BFS hops from the entrance on the ungated graph, assigned by `assign_depth`; doubling it is the ungated round trip exactly.)

- [ ] **Step 8: Amend `dof_counts_every_draw` (`circuit.rs:1383-1460`).**

Change the identity's doc block and the assertion to add `+ realms` (one pattern draw per realm, every realm, including fallback realms):

```text
      + realms                 ONE pattern draw per realm (The Brattice, spec §3.8)
```

and in the test body, wherever `floor` is computed, add `+ plan.realms.len() as u32`. Keep the `dof == floor` assertion outright where nothing failed.

- [ ] **Step 9: Run the worldgen tests; fix; fmt; clippy.**

```bash
cargo fmt && cargo test -p hornvale-worldgen 2>&1 | tail -25
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings 2>&1 | tail -5
cargo run --quiet --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -5
```

Expected: all green, including the 15 existing circuit tests unchanged, `the_plan_is_deterministic_and_reads_the_vertex`, and `realms_are_the_mesh_count_and_every_level_is_inside_the_clip` (the pass adds no realm). If `every_plan_is_solvable_for_a_body_holding_nothing` fails, the pass has a bug — it must never let an unsolvable stamp stand; do not weaken the test.

- [ ] **Step 10: The Crosscut deferred minor in your path.** `try_extend` (`circuit.rs:895-931`) tests its capability invariant against the pre-extend passage set. Change the `would_still_cycle` call to evaluate against the post-extend set (the interior cells spent AND the removed edge `u–v` no longer available). Run the 400-seed sweep test (`realms_are_the_mesh_count_and_every_level_is_inside_the_clip`) to confirm zero starved levels still. Ledger it as taken.

- [ ] **Step 11: Regenerate the audit page and read the diff against the spec's §5 branch table.**

```bash
cargo run -p hornvale -- circuit --seed 42 > .superpowers/sdd/baselines/panel42.after.txt
diff <(cargo run -q -p hornvale -- circuit --seed 42) <(sed -n '/```text/,/```/p' .superpowers/sdd/baselines/circuit-panel.before.md | head -200) | head -40
```

Branch table: only the `dof`-related lines differ (if the page prints dof) → proceed; any of the four Crosscut numbers moved → STOP, the pass touched the graph. Do not regenerate the committed page yet — Task 2 adds the sections and regenerates once.

- [ ] **Step 12: Commit.**

```bash
git add windows/worldgen cli/src/streams.rs docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "feat(worldgen): the Brattice pass — gates as requirements on ways, ten-row inventory, product-graph solvability

One pattern draw per realm from underworld/gate/v1/pattern; substance from
rock and work; every placement checked against the default body. dof
identity extended by +realms. Takes the Crosscut's try_extend deferred minor."
```

---

### Task 2: The readouts and the audit page

**Files:**
- Modify: `windows/worldgen/src/circuit_readout.rs` (`render_circuit_panel`, `:54-230`)
- Modify: `windows/worldgen/src/brattice.rs` (readout helpers)
- Modify: `scripts/regenerate-artifacts.sh:870-895` only if the framing prose needs a Brattice sentence (it prints hand-authored lines before the fenced block)
- Regenerate: `docs/audits/underworld-circuit-seed-panel.md`
- Test: `circuit_readout.rs::tests` (extend `the_panel_is_deterministic_and_carries_every_verdict`), `brattice.rs::tests`

**Interfaces:**
- Consumes: Task 1's `solvable`, `gated_round_trip`, `ungated_round_trip`, `DEFAULT_BODY`, `Outcome`, `Skip`, `CYCLE_PATTERNS`, `Gate`, `Way`, `Requirement`, `Capability`.
- Produces:

```rust
pub fn gate_yield(plan: &DescentPlan) -> Option<f64>;      // applied / (applied + skipped-not-inadmissible); None if no admissible realm
pub fn detour_cost(plan: &DescentPlan) -> Option<f64>;     // gated/ungated round trip; None if no realized requirement
pub fn realized_requirements(plan: &DescentPlan) -> (usize, usize, usize); // (doors, sumps, chutes)
pub fn return_differs(plan: &DescentPlan) -> Option<bool>;  // default body's outbound edge set != return edge set
pub fn skip_histogram(plan: &DescentPlan) -> [u32; 4];      // Inadmissible, Claimed, NoRoom, Unsolvable
```

- [ ] **Step 1: Write the failing readout tests in `brattice.rs::tests`.**

```rust
    #[test]
    fn gate_yield_is_a_ratio_and_none_without_an_admissible_realm() {
        let p = plan(42, 1, CaveKind::Karst, Character::DrowTier);
        if let Some(y) = gate_yield(&p) { assert!((0.0..=1.0).contains(&y)); }
        let applied = p.patterns.iter().filter(|o| matches!(o, Outcome::Applied { .. })).count();
        let skipped = p.patterns.iter().filter(|o| matches!(o, Outcome::Skipped { .. })).count();
        if applied + skipped == 0 { assert!(gate_yield(&p).is_none()); }
        else { assert_eq!(gate_yield(&p), Some(applied as f64 / (applied + skipped) as f64)); }
    }

    #[test]
    fn detour_cost_is_at_least_one_and_none_without_a_realized_requirement() {
        for seed in 0..30u64 {
            let p = plan(seed, 1, CaveKind::Karst, Character::DrowTier);
            let (d, s, c) = realized_requirements(&p);
            match detour_cost(&p) {
                None => assert_eq!(d + s + c, 0, "seed {seed}: gates exist but no cost"),
                Some(r) => { assert!(d + s + c > 0); assert!(r >= 1.0, "seed {seed}: a gated trip shorter than the ungated one"); }
            }
        }
    }

    #[test]
    fn a_plan_with_no_gates_has_unit_detour_and_no_return_difference() {
        let mut p = plan(7, 1, CaveKind::Karst, Character::WildCave);
        for e in &mut p.edges { e.gate = None; }
        for n in &mut p.nodes { n.key = None; }
        assert_eq!(gated_round_trip(&p, DEFAULT_BODY), Some(ungated_round_trip(&p)));
        assert_eq!(return_differs(&p), None, "no realized requirement, no reading");
    }
```

`return_differs`: BFS parent-chain outbound from entrance to terminus, then back; compare the two edge sets as `BTreeSet<(NodeId, NodeId)>` with ordered pairs normalized `(min, max)`. Return `None` when `realized_requirements` is all zero.

- [ ] **Step 2: Run; see red; implement the five helpers in `brattice.rs`.**

`realized_requirements` counts edges whose `toward_a` is `Needs(Key(_))` (doors), `Needs(Mode(Swim))` (sumps), `Needs(Mode(Fly))` (chutes). `gate_yield` per the test. `detour_cost` = `gated_round_trip(plan, DEFAULT_BODY)? as f64 / ungated_round_trip(plan) as f64`, `None` if no realized requirement. `skip_histogram` from `plan.patterns`.

- [ ] **Step 3: Add four sections to `render_circuit_panel` after the §4.4 block (`circuit_readout.rs:192-…`).**

Follow the file's own style (`out.push_str(&format!(...))`, `median(&mut v)`, `show(...)`, `NOT MEASURABLE` where a seed has nothing to measure). Frozen words verbatim:

```rust
    // The Brattice §4.1 gate yield (frozen floor 0.70).
    let gy = median(&mut yields);
    out.push_str(&format!(
        "gate yield: median {} (frozen floor 0.70; FROM realms with an admissible drawn row TO rows applied in full) -> {}\n",
        show(gy), verdict(gy, 0.70)
    ));
    out.push_str(&format!("  skips: inadmissible {} claimed {} no-room {} unsolvable {} (report only)\n", …));
    // The Brattice §4.2 detour cost (frozen floor 1.10), round trip, descents with >=1 realized requirement.
    let dc = median(&mut costs);
    out.push_str(&format!(
        "detour cost: median {} over {} gated descents (frozen floor 1.10; default body's round trip gated / ungated) -> {}\n",
        show(dc), gated_descents, verdict(dc, 1.10)
    ));
    // The Brattice §4.3 solvability — a guard: printed, never a verdict word.
    out.push_str(&format!("solvable for a body holding nothing: {} of {} descents (a guard; a miss is a red test, not a number)\n", solvable_count, descents));
    // The Brattice §4.4 report only.
    out.push_str(&format!("gates: doors {} sumps {} chutes {}; worked descents with a door {} of {} (the production walk reaches none yet, spec §1)\n", …));
    out.push_str(&format!("return differs from outbound: {} of {} gated descents (report only; follows from a chute by construction)\n", …));
    out.push_str("patterns by class and span (report only):\n");   // one line per (class, span, pattern name) with a count
```

where `fn verdict(m: Option<f64>, floor: f64) -> &'static str` returns `"NOT MEASURABLE"` for `None`, else `"PASSED"`/`"FALSIFIED"`. Also add the Crosscut's four numbers as a **byte-identical prefix**: do not touch the code above the new block.

- [ ] **Step 4: Extend the renderer test** to assert the new headings are present and that two renders are byte-identical (the existing test's shape). Run `cargo test -p hornvale-worldgen circuit_readout`.

- [ ] **Step 5: Regenerate and read the diff.**

```bash
make rebaseline 2>&1 | tail -5
git diff --stat
git diff docs/audits/underworld-circuit-seed-panel.md | head -80
```

Branch table (spec §5): the diff is ONLY added lines in the new sections (and, if the page prints it, a `dof` line) → proceed. Any changed Crosscut number → STOP and report. `docs/audits/type-audit-report.md` will also move (new pub items): that is expected; include it. If `clients/game/core/tests/fixtures/` moved here, STOP — nothing in this task touches a level.

- [ ] **Step 6: Record the verdicts in the ledger** (Task 2 section: the three medians per seed and their verdict words, and the skip histogram), commit, and **submit the stage gate**.

```bash
git add -A windows/worldgen docs/audits docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "feat(readout): the Brattice's four sections on the circuit panel — gate yield, detour cost, solvability guard, report-only counts"
git push -u origin campaign/the-brattice
make sluice-stage BRANCH=campaign/the-brattice REF=$(git rev-parse HEAD)
```

Watch `make sluice-status`; if the mouth refuses with a conflict, absorb `origin/main` locally (the three-file shape is on the board), regenerate, recommit, resubmit.

---

### Task 3: Realization — threshold, sump, chute; the wire; the client; the witness

**Files:**
- Modify: `windows/vessel/src/underworld_level/mod.rs` (`LevelCellKind` `:21-34`, `movement_mode` `:64-72`, `Level` `:79-91`, `generate_level_with_origin` `:184-283`, `standable_cells_in_rect` `:321-340`, `place_stair` `:377`, `shortest_route_within_rect` `:459-498`, tests)
- Modify: `windows/vessel/src/level_doc.rs:351-357` (`entry_for`)
- Modify: `windows/vessel/src/session.rs` (`map` glyph fn near `:8640`; the `_ => '?'` arm)
- Modify: `clients/game/core/src/level.rs:111-185` and its tests at `:355`
- Regenerate: `clients/game/core/tests/fixtures/` (branch table)
- Test: `underworld_level/mod.rs::tests`, `windows/vessel/tests/suite/underworld_level_generation.rs`

**Interfaces:**
- Consumes: Task 1's `DescentPlan::gate_between`, `Gate`, `Way`, `Requirement`, `Capability`.
- Produces:

```rust
pub enum LevelCellKind { Floor, Wall, Flooded, StairsDown, StairsUp, Threshold, Deep, Drop }
pub fn movement_mode(kind) -> Option<MovementMode>   // Threshold, Drop -> Walk; Deep -> Swim
pub struct Level { …, pub thresholds: Vec<(usize, usize, Cell)> }   // (node a, node b, the crossing cell), like Lattice.doorways
// wire: "threshold" | "deep" | "drop"
```

- [ ] **Step 1: Write the failing tests (in `underworld_level/mod.rs::tests`).**

```rust
    /// claim: invariant(seed: 0..200) — THE BRATTICE's realization witness
    /// (spec §3.5): every Passage edge realizes exactly one crossing cell on the
    /// divider — `Threshold`, or `Deep` when the edge is a sump; every
    /// `Needs(Mode(Swim))` passage has >=1 `Deep` cell and no `Threshold`;
    /// every `Needs(Mode(Fly))` stair is a `Drop` above and a standable
    /// non-stair cell below; every other stair still pairs `StairsDown` /
    /// `StairsUp`; and no Threshold/Deep/Drop exists that no edge asked for.
    #[test]
    fn the_realization_witnesses_exactly_what_the_plan_stamped() {
        for seed in 0..200u64 {
            let rungs = habitation_rungs();
            for (kind, ch) in [(CaveKind::Karst, Character::DrowTier), (CaveKind::Fracture, Character::WildCave), (CaveKind::LavaTube, Character::WildCave)] {
                let plan = plan_descent(Seed(seed), Vertex(1), &rungs, kind, ch);
                let levels = generate_descent_for_character(&rungs, kind, &vec![ChamberOrigin::Found; rungs.len()], &vec![10.0; rungs.len()], 1000.0, ch, &plan, Seed(seed));
                for (l, level) in levels.iter().enumerate() {
                    // (a) every passage has one crossing cell, of the right kind
                    for (a, b) in plan.passages_on(l) {
                        let crossings: Vec<Cell> = level.thresholds.iter().filter(|(x, y, _)| (*x == a && *y == b) || (*x == b && *y == a)).map(|t| t.2).collect();
                        assert_eq!(crossings.len(), 1, "seed {seed} level {l} edge {a}-{b}: {} crossings", crossings.len());
                        let is_sump = matches!(plan.gate_between(a, b).map(|(_, g)| g.toward_a), Some(Way::Needs(Requirement::Mode(Capability::Swim))));
                        let want = if is_sump { LevelCellKind::Deep } else { LevelCellKind::Threshold };
                        assert_eq!(level.cells.get(crossings[0]), Some(want), "seed {seed} level {l} edge {a}-{b}");
                    }
                    // (b) the count of Threshold cells equals the count of non-sump passages
                    let non_sump = plan.passages_on(l).iter().filter(|(a, b)| !matches!(plan.gate_between(*a, *b).map(|(_, g)| g.toward_a), Some(Way::Needs(Requirement::Mode(Capability::Swim))))).count();
                    let thresholds = level.cells.iter().filter(|(_, k)| *k == LevelCellKind::Threshold).count();
                    assert_eq!(thresholds, non_sump, "seed {seed} level {l}: a Threshold nobody asked for, or one missing");
                    // (c) drops
                    for (upper, _lower, x, y) in plan.stairs_from(l) {
                        let is_chute = matches!(plan.gate_between(upper, _lower).map(|(_, g)| g.toward_a), Some(Way::Needs(Requirement::Mode(Capability::Fly))));
                        let here = level.cells.get(Cell(x, y));
                        let below = levels[l + 1].cells.get(Cell(x, y));
                        if is_chute {
                            assert_eq!(here, Some(LevelCellKind::Drop), "seed {seed} level {l}: chute lip");
                            assert!(matches!(below, Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded) | Some(LevelCellKind::Threshold)), "seed {seed} level {l}: a chute lands on {below:?}");
                        } else {
                            assert_eq!(here, Some(LevelCellKind::StairsDown));
                            assert_eq!(below, Some(LevelCellKind::StairsUp));
                        }
                    }
                    let drops = level.cells.iter().filter(|(_, k)| *k == LevelCellKind::Drop).count();
                    let chutes = plan.stairs_from(l).iter().filter(|(u, lo, _, _)| matches!(plan.gate_between(*u, *lo).map(|(_, g)| g.toward_a), Some(Way::Needs(Requirement::Mode(Capability::Fly))))).count();
                    assert_eq!(drops, chutes, "seed {seed} level {l}: a Drop nobody asked for");
                }
            }
        }
    }

    #[test]
    fn movement_mode_answers_swim_for_deep_and_walk_for_a_threshold_and_a_drop() {
        assert_eq!(movement_mode(LevelCellKind::Deep), Some(MovementMode::Swim));
        assert_eq!(movement_mode(LevelCellKind::Threshold), Some(MovementMode::Walk));
        assert_eq!(movement_mode(LevelCellKind::Drop), Some(MovementMode::Walk));
    }
```

`CellGrid::iter()` — check `dense.rs` for the iterator's name and item shape (`(Cell, LevelCellKind)` in ascending `(x, y)`); if it is named differently, use that name. Amend `stairs_pair_by_coordinate_across_adjacent_rungs` (`:1249-1315`): where it asserts every `StairsDown` below the last rung pairs with a `StairsUp`, keep that; add that every `Drop` has a standable non-stair cell below and no `StairsUp` under it; its claim tag gains a sentence saying so. Update `movement_mode_answers_one_mode_per_passable_kind` (`:807`) for the three new variants.

- [ ] **Step 2: Run; see red (compile errors on the variants).**

- [ ] **Step 3: Implement.**

Add the three variants with docs (spec §3.5 wording); `movement_mode` arms. `Level` gains:

```rust
    /// Every passage's crossing cell — `(node a, node b, the divider cell)` —
    /// the underworld's `Lattice::doorways` (The Brattice, spec §3.5). A door
    /// Thing is anchored at one of these; a sump's crossing is `Deep`.
    pub thresholds: Vec<(usize, usize, Cell)>,
```

and its literal site(s) (Task 0's count) fill it. In `generate_level_with_origin`'s passage loop (`:232-241`):

```rust
    let mut thresholds = Vec::new();
    for (a, b) in plan.passages_on(level) {
        let ra = rect_of(plan, a);
        let rb = rect_of(plan, b);
        let ca = walkable_cells_in_rect(ra, &cells);
        let cb = walkable_cells_in_rect(rb, &cells);
        if let Some((pa, pb)) = nearest_pair(&ca, &cb) {
            let l_cells = l_corridor(pa, pb);
            let rock_before: Vec<Cell> = l_cells.iter().copied().filter(|c| cells.get(*c) == Some(LevelCellKind::Wall)).collect();
            connect_cells(pa, pb, &mut cells);
            // The crossing: the one L cell in neither rect (spec §3.5; asserted, not assumed).
            let crossing: Vec<Cell> = l_cells.iter().copied().filter(|c| !ra.contains(*c) && !rb.contains(*c)).collect();
            debug_assert_eq!(crossing.len(), 1, "an L between grid-adjacent regions crosses the divider once");
            let cross = crossing[0];
            let is_sump = matches!(plan.gate_between(a, b).map(|(_, g)| g.toward_a),
                Some(hornvale_worldgen::brattice::Way::Needs(hornvale_worldgen::brattice::Requirement::Mode(hornvale_worldgen::brattice::Capability::Swim))));
            if is_sump {
                for c in rock_before { cells.set(c, LevelCellKind::Deep); }   // includes the crossing
            } else {
                cells.set(cross, LevelCellKind::Threshold);
            }
            thresholds.push((a, b, cross));
        }
    }
```

with `fn l_corridor(a: Cell, b: Cell) -> Vec<Cell>` returning the same cells `connect_cells` sets (extract it so the two cannot disagree; have `connect_cells` call it). **Execution amendment to record in the ledger:** a sump's crossing cell is `Deep`, not `Threshold` — a drowned squeeze — and the witness says so; spec §3.5 read "every passage has one Threshold". In the stairs loops (`:242-259`):

```rust
    for (upper, lower, x, y) in plan.stairs_from(level) {
        let chute = is_chute(plan, upper, lower);
        let kind = if chute { LevelCellKind::Drop } else { LevelCellKind::StairsDown };
        place_stair(Cell(x, y), kind, rect_of(plan, upper), &mut cells);
    }
    for (upper, lower, x, y) in plan.stairs_into(level) {
        if is_chute(plan, upper, lower) {
            // The landing: standable floor, no StairsUp (spec §3.5). `place_stair`
            // with Floor sets the cell and repairs the region as for a foot.
            place_stair(Cell(x, y), LevelCellKind::Floor, rect_of(plan, lower), &mut cells);
        } else {
            place_stair(Cell(x, y), LevelCellKind::StairsUp, rect_of(plan, lower), &mut cells);
        }
    }
```

Read `place_stair`'s body first: if it asserts the kind is a stair, widen the assertion to admit `Drop` and `Floor`. `shortest_route_within_rect` (`:485-490`) skips `Threshold | Deep | Drop` alongside the stairs; `standable_cells_in_rect` adds `Threshold | Drop`. `entry_for` gains `"threshold"`, `"deep"`, `"drop"`. The session's `map` glyph function near `session.rs:8640` gains arms (pick `'`, `=`, `v`; remembered variants are the client's business — check whether that fn distinguishes them).

- [ ] **Step 4: Teach the client (outside the workspace).**

In `clients/game/core/src/level.rs`: add `THRESHOLD_GLYPH '\''`, `THRESHOLD_REMEMBERED_GLYPH '`'`, `DEEP_GLYPH '='`, `DEEP_REMEMBERED_GLYPH '_'`, `DROP_GLYPH 'v'`, `DROP_REMEMBERED_GLYPH 'u'`, arms in `glyph_of`, the twin table in the module doc (`:52`), and the test at `:355`. A door is a mark (Task 5 emits it with `kind: "door"`); in the marks pass, draw `kind == "door"` as `'+'` instead of `MARK_GLYPH`. Run the client's own gate:

```bash
make clients-check-run 2>&1 | tail -10
```

- [ ] **Step 5: Regenerate fixtures and read the diff.**

```bash
make rebaseline 2>&1 | tail -5
git status --short clients/game/core/tests/fixtures/ | head
git diff clients/game/core/tests/fixtures/ | grep '^[-+]' | grep -v '^[-+][-+]' | head -40
```

Branch table (spec §5): no fixture differs → nothing to do; a fixture differs only in `palette`/`cells` rows carrying `threshold`/`deep`/`drop` → `REBASELINE=1 make rebaseline-goldens` if a golden guards it, review, commit in the same commit as the kinds; any other difference → STOP and report.

- [ ] **Step 6: Run the vessel tests; take the two Crosscut deferred minors in your path.**

```bash
cargo fmt && cargo test -p hornvale-vessel underworld_level 2>&1 | tail -30
cargo test -p hornvale-vessel --test suite -- underworld_level_generation 2>&1 | tail -10
```

`unlinked_neighbours_keep_their_wall` must pass **unmodified**. `every_walkable_cell_is_reachable_from_every_other` (`:1735`): read its walkability predicate; if it is `movement_mode(..).is_some()` it now admits `Deep` and is the resident's view — fine; if it compares kinds, widen it to the predicate and say so in its doc. The terminus write (`:260-276`) gains a `debug_assert!` naming the cause when the region's only walkable cell is already a landing (deferred minor). The `CELLULAR` obituary rustdoc in `carve.rs` moves to a module-level comment (deferred minor).

- [ ] **Step 7: Commit.** Post a board `notice` (`polarity=fyi`, `PATHS='windows/vessel/src/underworld_level/'`) that `LevelCellKind` gained three variants.

```bash
git add -A windows/vessel clients/game docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "feat(vessel): realize the plan's gates — a Threshold on every passage, Deep runs for sumps, Drop lips for chutes; the realization witness; wire and client glyphs"
```

---

### Task 4: The walk — locomotion, the actor-aware seam, three refusals, the chute's verbs

**Files:**
- Modify: `domains/species/src/lib.rs` (after `habitat_realm_registry`, `:2504-2540`)
- Modify: `windows/vessel/src/body.rs` (accessor)
- Modify: `windows/vessel/src/underground.rs` (`peek` `:427-451`, `peek_stairs` `:498-524`, `take_stairs` `:539-544`, refusal consts `:40-70`)
- Modify: `windows/vessel/src/session.rs` (`step_underground` `:4992-5050`, `take_stairs` `:5158-5200`, the `MovementMode` match `:5041-5045`, `underground_footing_word` `:8630`)
- Test: species in-module; `underground.rs` tests; session tests

**Interfaces:**
- Consumes: Task 1's `Gate`/`Way`/`Requirement`/`Capability`; Task 3's cell kinds and `Level.thresholds`.
- Produces:

```rust
// hornvale_species
pub struct Locomotion { pub swim: bool, pub fly: bool }
impl Component for Locomotion {}
pub fn locomotion_registry() -> ComponentStore<KindId, Locomotion>;   // sparse; 6 swim rows, 3 fly rows
pub const WALKER: Locomotion = Locomotion { swim: false, fly: false };
// vessel
impl Body { pub fn locomotion(&self) -> Locomotion }          // registry by species label, WALKER default
pub(crate) struct Traverser<'a> { pub locomotion: Locomotion, pub door_open: &'a dyn Fn(Cell) -> bool }
impl Underground {
    pub(crate) fn peek(&self, dir: Compass, who: &Traverser) -> Result<Cell, &'static str>;
    pub(crate) fn peek_stairs(&self, loc: Locomotion) -> Result<(usize, Cell), &'static str>;
    pub(crate) fn threshold_edge(&self, cell: Cell) -> Option<(usize, usize)>;   // from level.thresholds
}
const UNDERGROUND_DEEP_WATER_REFUSAL, UNDERGROUND_LOCKED_DOOR_REFUSAL, NO_WAY_UP_REFUSAL
```

- [ ] **Step 1: Species: failing test, then the store.**

```rust
    #[test]
    fn the_locomotion_store_is_sparse_and_non_empty_in_both_modes() {
        let reg = locomotion_registry();
        assert!(reg.get(&KindId("human")).is_none(), "a walker has no row");
        assert_eq!(reg.get(&KindId("reef-shark")).map(|l| l.swim), Some(true));
        assert_eq!(reg.get(&KindId("red-dragon")).map(|l| l.fly), Some(true));
        let swim = THING_LESS_KINDS_PLACEHOLDER; // (delete this line — see below)
    }
```

(Do not write the placeholder line; write instead:) iterate the registry's rows — `ComponentStore` exposes iteration; check `kernel/src/component.rs:18-70` for its name (`iter()` or `rows()`) — and assert at least one `swim` and one `fly` row exist, and that every row's `KindId` is also in `biosphere_registry()` (a locomotion for a kind that does not exist is a typo).

```rust
/// How a kind moves besides walking (The Brattice, spec §3.6): the
/// capability keys `Swim` and `Fly`. Sparse like [`habitat_realm_registry`]
/// — one consumer (the walk's actor-aware seam), rows only for kinds that
/// are not plain walkers. Decision 0576: a `KindId`-keyed build-state
/// capability lives here, never in the ledger.
/// type-audit: bare-ok(flag: swim), bare-ok(flag: fly)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Locomotion {
    /// Crosses deep water.
    pub swim: bool,
    /// Climbs a chute.
    pub fly: bool,
}

impl Component for Locomotion {}

/// Walk and wade only — every kind absent from [`locomotion_registry`].
pub const WALKER: Locomotion = Locomotion { swim: false, fly: false };

/// The sparse store. Aquatic and amphibious kinds swim; the dragons fly.
pub fn locomotion_registry() -> ComponentStore<KindId, Locomotion> {
    const SWIM: Locomotion = Locomotion { swim: true, fly: false };
    const FLY: Locomotion = Locomotion { swim: false, fly: true };
    [
        (KindId("reef-shark"), SWIM), (KindId("killer-whale"), SWIM), (KindId("giant-octopus"), SWIM),
        (KindId("giant-squid"), SWIM), (KindId("giant-crocodile"), SWIM), (KindId("sea-elf"), SWIM),
        (KindId("black-dragon"), FLY), (KindId("red-dragon"), FLY), (KindId("white-dragon"), FLY),
    ].into_iter().collect()
}
```

Run `cargo test -p hornvale-species locomotion`.

- [ ] **Step 2: `Body::locomotion()` — an accessor, not a field.**

Spec §3.6 says `Body` "gains `locomotion`, threaded at derivation". Task 0 counted ~43 `Body {` literal sites across seven files; a stored field would touch every one for a value that is a pure function of `species`. Implement as an accessor and **ledger the reading**:

```rust
impl Body {
    /// How this body moves besides walking (The Brattice): read from
    /// [`hornvale_species::locomotion_registry`] by species label; a kind with
    /// no row walks and wades. An accessor rather than a stored field because it
    /// is a pure function of `species` and a field would touch ~43 literal sites
    /// for no new information.
    pub fn locomotion(&self) -> hornvale_species::Locomotion {
        hornvale_species::locomotion_registry()
            .get_by_label(&self.species)
            .copied()
            .unwrap_or(hornvale_species::WALKER)
    }
}
```

- [ ] **Step 3: Failing walk tests in `underground.rs` (in-module).** Build a two-level fixture by hand as `corner_rule.rs`/existing underground tests do (look at how `Underground` is constructed in tests — there is a test-only constructor or the tests go through `enter`; mirror them):

```rust
    #[test]
    fn deep_water_refuses_a_walker_and_admits_a_swimmer() { /* a level with a Deep cell east of the possession; peek east with WALKER -> Err(UNDERGROUND_DEEP_WATER_REFUSAL); with swim -> Ok */ }
    #[test]
    fn a_threshold_with_a_shut_door_refuses_and_an_open_one_admits() { /* door_open closure false -> Err(UNDERGROUND_LOCKED_DOOR_REFUSAL); true -> Ok; a threshold with no door (closure irrelevant) -> Ok */ }
    #[test]
    fn a_drop_goes_down_and_only_a_flier_comes_back_up() { /* on a Drop cell: peek_stairs(WALKER) -> Ok((rung+1, same cell)); from the landing: peek_stairs(WALKER) -> Err(NO_WAY_UP_REFUSAL); peek_stairs(fly) -> Ok((rung, same cell)) */ }
```

Write each body fully against the fixture you build; the comments above are the assertions, not the code.

- [ ] **Step 4: Implement the seam.**

```rust
const UNDERGROUND_DEEP_WATER_REFUSAL: &str = "The water here is over your head, and you cannot swim.";
const UNDERGROUND_LOCKED_DOOR_REFUSAL: &str = "A door bars the way, and it is locked.";
const NO_WAY_UP_REFUSAL: &str = "The chute's lip is out of reach above you.";

/// Who is trying to move: the body's modes and, for a threshold, whether a
/// door anchored there stands open (the session answers from the ledger;
/// this module holds no ledger). The actor-aware seam spec §3.6 puts beside
/// `movement_mode`.
pub(crate) struct Traverser<'a> {
    pub locomotion: hornvale_species::Locomotion,
    pub door_open: &'a dyn Fn(Cell) -> bool,
}

impl Underground {
    /// The `(a, b)` plan edge whose crossing cell this is, if any.
    pub(crate) fn threshold_edge(&self, cell: Cell) -> Option<(usize, usize)> {
        self.level().thresholds.iter().find(|t| t.2 == cell).map(|t| (t.0, t.1))
    }

    /// May `who` stand on `cell`? The actor-aware half; the corner rule keeps
    /// asking `movement_mode` alone (a threshold is an opening whether or not a
    /// door in it is shut).
    fn admits(&self, cell: Cell, who: &Traverser) -> Result<(), &'static str> {
        let kind = self.level().cells.get(cell).ok_or(UNDERGROUND_ROCK_REFUSAL)?;
        match crate::underworld_level::movement_mode(kind) {
            None => Err(UNDERGROUND_ROCK_REFUSAL),
            Some(MovementMode::Swim) if !who.locomotion.swim => Err(UNDERGROUND_DEEP_WATER_REFUSAL),
            Some(MovementMode::Fly) if !who.locomotion.fly => Err(NO_WAY_UP_REFUSAL),
            Some(_) if kind == LevelCellKind::Threshold && self.has_door(cell) && !(who.door_open)(cell) => Err(UNDERGROUND_LOCKED_DOOR_REFUSAL),
            Some(_) => Ok(()),
        }
    }

    /// Does the plan hang a door at this threshold (a `Needs(Key(_))` gate)?
    pub(crate) fn has_door(&self, cell: Cell) -> bool {
        self.threshold_edge(cell).and_then(|(a, b)| self.plan.gate_between(a, b)).is_some_and(|(_, g)| matches!(g.toward_a, Way::Needs(Requirement::Key(_))))
    }
}
```

`peek(dir, who)` keeps the corner rule exactly as is (`open` closure over `movement_mode(..).is_some()`), then replaces `if !open(target)` with `self.admits(target, who)?`. `peek_stairs(loc)`:

```rust
            Some(LevelCellKind::Drop) => {
                let next = self.rung + 1;
                if next >= self.descent.len() { return Err(STAIRS_LEAD_NOWHERE_REFUSAL); }
                Ok((next, self.cell))
            }
            Some(_) if self.rung > 0 && self.descent[self.rung - 1].cells.get(self.cell) == Some(LevelCellKind::Drop) => {
                if !loc.fly { return Err(NO_WAY_UP_REFUSAL); }
                Ok((self.rung - 1, self.cell))
            }
```

placed before the `_ => Err(NOT_ON_STAIRS_REFUSAL)` arm. `take_stairs` passes the locomotion through. The `plan` field's `#[allow(dead_code)]` comes off — it is read now.

- [ ] **Step 5: Session.** In `step_underground`, build the `Traverser` from `self.driven_body().locomotion()` (find the accessor that yields the driven `Body`; `agent_entity()` is at `:2625`, and the roster/driven index is near `other_bodies` at `:1249`) and a `door_open` closure that, until Task 5 wires the ledger, returns `false` — a door with no fact is shut and locked (the Chattel's default), so this is the correct behaviour, not a stub; Task 5 replaces the body of the closure with the fold. The `MovementMode` match at `:5041-5045` gains `Swim => "swim"` (and `Fly` is unreachable for a lateral step — `unreachable!("no lateral cell answers Fly")` is honest here since `movement_mode` never returns it). `take_stairs` (`:5158`): accept `Drop` for `want_down` and "the cell above is a `Drop`" for `!want_down`; pass `loc`; narrate `"You let yourself down the chute."` / `"You fly up the chute."`; the stair sentence is unchanged for stairs. `underground_footing_word`: `Deep => "deep water"`, `Threshold => "a narrow squeeze"`, `Drop => "the lip of a chute"`; check the callers' sentence still reads.

- [ ] **Step 6: Run, fmt, clippy, commit; stage gate.**

```bash
cargo fmt && cargo test -p hornvale-vessel 2>&1 | tail -15
cargo test -p hornvale-species 2>&1 | tail -3
git add -A domains/species windows/vessel docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "feat(walk): locomotion as a species component; the actor-aware seam beside movement_mode; deep water, locked door and no-way-up refusals; the chute's verbs are the stairs' verbs"
git push && make sluice-stage BRANCH=campaign/the-brattice REF=$(git rev-parse HEAD)
```

---

### Task 5: Doors and keys as Things underground

**Files:**
- Modify: `domains/thing/src/lib.rs` (`THING_KINDS` `:64-81`, a `kinds::DOOR` handle beside `THRESHOLD` `:136`, a `ThingTraits` row if the traits table is per kind — read `:47-60`)
- Modify: `windows/vessel/src/affordance.rs:272-305` (door row)
- Create: `windows/vessel/src/descent_thing.rs`
- Modify: `windows/vessel/src/thing.rs` (add `set_lockedness_role` mirroring `set_openness_role` `:534`; generalize `lying_in` `:796` into `lying_at_place(ledger, place_key: &str, day)` and have `lying_in` call it)
- Modify: `windows/vessel/src/session.rs` (`describe_underground_here` `:5300`; `examine` `:7601`; `take` `:3821`; `open_or_close` `:3466`; `drop_carried` `:4096`; the `door_open` closure from Task 4; `underground_level` marks `:6446`)
- Modify: `windows/vessel/src/lib.rs` (`mod descent_thing;`)
- Test: `descent_thing.rs` in-module; session tests

**Interfaces:**
- Consumes: Task 1's plan types; Task 3's `Level.thresholds`; Task 4's `has_door`, `threshold_edge`, `Traverser`; the Chattel's `promote_role`, `id_for_role`, `set_openness_role`, `located_in_holder_fact`, `held_by`, `is_open`, `is_locked`, `is_latent`-style default.
- Produces:

```rust
// descent_thing.rs
pub(crate) fn region_key(vertex: Vertex, level: usize, cell: GridCell) -> String;   // "descent/<v>/<l>/<col>.<row>"
pub(crate) fn key_role(vertex: Vertex, level: usize, cell: GridCell) -> String;     // "thing@descent/<v>/<l>/<col>.<row>/key"
pub(crate) fn door_role(vertex: Vertex, level: usize, a: GridCell, b: GridCell) -> String; // lesser cell first
pub(crate) fn key_id(...) -> EntityId;  pub(crate) fn door_id(...) -> EntityId;   // via thing::id_for_role(role, 0)
pub(crate) fn door_at(ug: &Underground, cell: Cell) -> Option<(EntityId, NodeId /*key node*/)>;
pub(crate) fn key_lying_in_region(ug: &Underground, ledger: &Ledger, day: WorldTime) -> Vec<EntityId>; // latent key at this node + dropped things at region_key
```

- [ ] **Step 1: Failing tests in `descent_thing.rs`.**

```rust
    #[test]
    fn roles_are_pure_functions_of_the_plan_position_and_order_the_door_cells() {
        let v = Vertex(7);
        assert_eq!(region_key(v, 2, GridCell { col: 3, row: 1 }), "descent/7/2/3.1");
        assert_eq!(key_role(v, 2, GridCell { col: 3, row: 1 }), "thing@descent/7/2/3.1/key");
        let a = GridCell { col: 3, row: 1 }; let b = GridCell { col: 4, row: 1 };
        assert_eq!(door_role(v, 2, a, b), door_role(v, 2, b, a), "a door has one name however you cross it");
        assert_eq!(door_role(v, 2, a, b), "thing@descent/7/2/3.1-4.1/door");
    }

    #[test]
    fn the_door_kind_carries_passage_lid_and_lock() {
        let reg = crate::affordance::object_registry();
        let t = reg.get(&hornvale_kernel::KindId("door")).expect("door is registered");
        for p in [ObjectProperty::AffordsPassage, ObjectProperty::Openable, ObjectProperty::Lockable] {
            assert!(t.properties.contains(&p));
        }
        assert!(hornvale_thing::THING_KINDS.contains(&"door"));
    }
```

Also confirm `the_lock_wants_a_property_and_exactly_one_kind_supplies_it` (session.rs) still passes: `door` is not `Portable`, so the count of portable kinds is unchanged. `lockable_kinds_are_also_openable` passes because `door` carries both.

- [ ] **Step 2: Implement the kind, the roles, the folds.**

`THING_KINDS` gains `"door"` (alphabetical: after `"cave-mouth"`); `kinds::DOOR: KindId = KindId("door")` with doc "A door hung in a threshold underground; shut and locked until its key turns (The Brattice)." `object_registry` gains `(KindId("door"), traits(&[AffordsPassage, Openable, Lockable]))`. `descent_thing.rs`:

```rust
//! Identities for things the descent plan places (The Brattice, spec §3.7):
//! a key at a node, a door on an edge. Keyed on the PLAN POSITION —
//! `(vertex, level, grid cell[s])` — on the `thing@passage/<addr>/<kind>`
//! precedent (`passage.rs`). Spec §5: from the first saved custody fact
//! naming one of these, the plan grammar is a save-format contract.

pub(crate) fn region_key(vertex: Vertex, level: usize, cell: GridCell) -> String {
    format!("descent/{}/{}/{}.{}", vertex.0, level, cell.col, cell.row)
}
pub(crate) fn key_role(vertex: Vertex, level: usize, cell: GridCell) -> String {
    format!("thing@{}/key", region_key(vertex, level, cell))
}
pub(crate) fn door_role(vertex: Vertex, level: usize, a: GridCell, b: GridCell) -> String {
    let (lo, hi) = if (a.col, a.row) <= (b.col, b.row) { (a, b) } else { (b, a) };
    format!("thing@descent/{}/{}/{}.{}-{}.{}/door", vertex.0, level, lo.col, lo.row, hi.col, hi.row)
}
```

`door_at(ug, cell)`: `ug.threshold_edge(cell)` → `ug.plan.gate_between(a, b)` → if `toward_a` is `Needs(Key(n))` → `Some((door_id(vertex, level, plan.nodes[a].cell, plan.nodes[b].cell), n))`. `Underground` must expose its `vertex` — add `pub(crate) vertex: Vertex` to the struct, set in `enter` (it has `vertex` in scope). `thing::set_lockedness_role` mirrors `set_openness_role` exactly (`:534-560`), posting `lockedness_fact`. `lying_at_place(ledger, place: &str, day)` is `lying_in`'s body with `Value::Text(place.to_string())`.

- [ ] **Step 3: Session wiring, one verb at a time, each with a test in the session's test module.**

- **`look`** (`describe_underground_here`): after the ways sentence, if the possession's plan node holds a key that is latent (no `located-in` fact for `key_id`) or `lying_at_place(region_key)` is non-empty, append `" A key lies here."` (or the nouns). If any of the eight neighbours is a `Threshold` with `door_at` `Some`, append `" A door stands to the <bearing>{, open|, shut}."` using `is_open`.
- **`examine key` / `examine door`** underground: one datum each ("A heavy iron key." / "A door, shut and locked." / "A door, standing open.").
- **`take key`** underground: resolve the key in the current region (latent or lying); `promote_role(ledger, registry, key_role, "key", 0, day)` then commit `located_in_holder_fact(key, agent, day)`. Refuse "There is no key here." otherwise.
- **`open door` / `close door`**: find the adjacent threshold with a door (if several, the first by bearing order, and say which). Locked check: `is_locked(ledger, door)` `None`→locked (latent), `Some(l)`→`l`. If locked and `held_by(ledger, agent, day)` does not contain `key_id(key node)` → `LOCKED_WITHOUT_A_KEY_REFUSAL`'s underground twin: `"It is locked, and the key that fits it is not in your hand."`. Else `promote_role(door)`, `set_lockedness_role(false)` then `set_openness_role(true)` — the key turns before the lid lifts, the Chattel's order. `close` sets openness false only (decision 0399).
- **`drop <thing>`** underground: `located_fact(thing, Value::Text(region_key(...)), day)` — use `thing::located_in_room_fact`'s sibling; write `located_in_place_fact(thing, place: &str, day)` in `thing.rs` if none exists.
- **The `door_open` closure** in `step_underground` becomes `|cell| descent_thing::door_at(ug, cell).is_some_and(|(id, _)| thing::is_open(&self.world.ledger, id, day) == Some(true))`.
- **Marks:** in `underground_level` (`:6446`), push a `PlanMark { x, y, noun: "door", kind: "door", datum: <open/shut sentence>, salience: 2 }` for every lit threshold with a door.

Each verb's test: build a session on a DrowTier descent with a door (see Task 6 Step 1 for the helper — write it here and Task 6 reuses it), drive the verb, assert the sentence and the fact.

- [ ] **Step 4: Fix the stale prose while the file is open.** `session.rs:3747` names `the-key-on-the-ledge`; the shipped pattern is `the-key-by-the-loom` in `Role::Loomroom`. Correct it.

- [ ] **Step 5: fmt, clippy, tests, commit.**

```bash
cargo fmt && cargo test -p hornvale-vessel 2>&1 | tail -15 && cargo test -p hornvale-thing 2>&1 | tail -3
git add -A domains/thing windows/vessel docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "feat(things): a door is an object at a threshold — the door kind, plan-position identities for doors and keys, and look/examine/take/open/close/drop reaching underground"
```

---

### Task 6: The acceptance walks

**Files:**
- Modify: `windows/vessel/src/session.rs` tests, or `windows/vessel/tests/suite/brattice_walks.rs` added to `tests/suite.rs` (mirror how `underworld_level_generation.rs` is included)
- Modify: `windows/vessel/src/underground.rs` — a `pub(crate) fn enter_with_character(...)` twin of `enter` taking `Character`, used only by tests (doc says so)

**Interfaces:** Consumes everything above. Produces the three walks of spec §7.1–7.3 as tests.

- [ ] **Step 1: A test helper that finds a descent with a door.** Loop `seed in 0..` and vertices until `plan_descent(seed, v, rungs, Karst, DrowTier)` has an edge with `Needs(Key(_))` on level 0 or 1 (so the walk is short); tag the loop `/// claim: structural(seed: 0..200) — a search for a fixture, not a claim over the range`. Build a `Session` for that seed, install `Underground::enter_with_character(..., DrowTier)` at that vertex (the Crosscut's walk test shows how the session's underground is set for a test).

- [ ] **Step 2: Walk 1 — the locked door (spec §7.1).** From the entrance, BFS the plan (ungated) to the door's near node; drive `go <bearing>` steps along the corridor cells to the threshold; assert the refusal `"A door bars the way, and it is locked."`; walk to the key node's region; assert `look` contains `"A key lies here."`; `take key`; assert `carrying` names it; return to the threshold; `open door`; assert the sentence and `is_locked == Some(false)`, `is_open == Some(true)`; step through; step back — the door is still open.

- [ ] **Step 3: Walk 2 — the chute (spec §7.2).** Find a WildCave Fracture descent with a chute; walk to the `Drop`; `down` → landed one rung below, same coordinate, sentence `"You let yourself down the chute."`; `up` → `"The chute's lip is out of reach above you."`; walk the lower path to the realm's far stairway (`StairsUp` at the paired coordinate); `up`; walk the upper path back to the lip's region. Every move by `go`/`down`/`up`.

- [ ] **Step 4: Walk 3 — capability keys (spec §7.3).** Same fixtures, but set the driven body's `species` to `"reef-shark"` (the roster body the session drives — find the field; `Body.species` is a `String`) and cross a sump with the narration `"You swim …"`; then `"red-dragon"` beneath a chute: `up` → `"You fly up the chute."`. A `Body` whose species is not in the registry must still be refused (the negative half of each).

- [ ] **Step 5: The 400-seed sweep of §3.8 as a `sanctioned-sweep` test in `brattice.rs`** if Task 1's 200-seed invariant is not already at 400 over 3 kinds × 3 characters × 4 vertices; make it so and time it (must stay under a few seconds; it is plan-only).

- [ ] **Step 6: fmt, clippy, tests, commit; stage gate.**

```bash
cargo fmt && cargo test -p hornvale-vessel --test suite -- brattice 2>&1 | tail -10
git add -A windows/vessel windows/worldgen docs/superpowers/ledgers/2026-09-02-the-brattice.md
git commit -m "test(walks): the Brattice's three acceptance walks — a locked door and its key, a chute and the way round, a shark's sump and a dragon's climb"
git push && make sluice-stage BRANCH=campaign/the-brattice REF=$(git rev-parse HEAD)
```

---

### Task 7: Book, decisions, registry, retrospective, close

**Files:**
- Create: `book/src/chronicle/the-brattice.md`; Modify: `book/src/SUMMARY.md`
- Create: `docs/decisions/0616-a-gate-is-a-requirement-on-a-way.md`, `0617-a-locks-substance-is-derived-from-rock-and-work.md`, `0618-a-descent-key-makes-the-plan-a-save-format-contract.md`, `0619-the-drop-is-a-stairway-with-its-up-half-omitted.md`, `0620-the-cycle-pattern-inventory-is-a-frozen-corpus.md` (shape: `docs/decisions/0567-*.md`)
- Modify: `book/src/frontier/idea-registry.md` — `MAP-drop-is-a-cross-floor-valve` → `shipped`; `MAP-underworld-traversal-grammar` gains the gate half; `MAP-walk-ignores-the-lattice` and `MAP-descent-carves-are-per-world` gain the sentence ledger #3 promised; `PLAY-key-placement-stands-in-for-a-resident` notes the descent key is placed by the plan, not a resident
- Modify: `book/src/chronicle/the-chattel.md` — correct "with `lock` and `unlock` beside them" (no such verbs)
- Modify: `windows/vessel/CLAUDE.md` or `windows/CLAUDE.md` if either describes `LevelCellKind`/`MovementMode` (grep first)
- Create: `docs/retrospectives/the-brattice.md`
- Run: `make rebaseline` (digest's decisions-in-force and delta move with five new decisions), `cargo test -p hornvale --test suite -- docs_consistency`

- [ ] **Step 1: Chronicle** at the book's altitude (technical, comprehensible without the code): the four-part gate; why the pass runs after growth; the substance table; the drop as a stair minus its up half; the door-is-not-a-cell ruling and the tomb's false door; the readouts with their verdicts in the frozen words and what each measured from and to; the save-format contract; what is deliberately not here (hazards, collapsing, secrecy, world-conditional gates, a door a player can reach today). Add to `SUMMARY.md` after the Crosscut's line.
- [ ] **Step 2: Five decisions**, each with Context / Decision / Consequences; 0618 quotes the exact custody-fact shape and names the epoch a grammar change would be; 0620 states the count and the freeze rule.
- [ ] **Step 3: Registry sweep** per the list above; run the drift check.
- [ ] **Step 4: Retrospective** (`docs/retrospectives/the-crosscut.md` is the shape): process lessons only; promote the ledger's follow-ups section; list deferred minors with the cost named.
- [ ] **Step 5: `make rebaseline`; `git diff --stat`; commit with the `Sluice-Headline:` trailer in the last paragraph and read it back.**

```bash
make rebaseline 2>&1 | tail -3
git add -A
git commit -m "docs(the-brattice): chronicle, decisions 0616-0620, registry sweep, retrospective

Sluice-Headline: the underworld gets something in the way — doors with keys, sumps that need swimming, chutes that need flight to climb, stamped on the plan from a frozen inventory, a door an object at a threshold and never a cell"
bash -c 'source scripts/sluice-lib.sh 2>/dev/null; sluice_headline_of HEAD' || grep -n "sluice_headline_of" scripts/*.sh | head -2
```

- [ ] **Step 6: Close.** Invoke the `closing-a-campaign` skill: census (`make sluice-census BRANCH=campaign/the-brattice REF=<full-sha>`, ordinary queued work), the G6 digest for Nathan (post-G3 ledger entries, save-format entries leading), then `make sluice BRANCH=campaign/the-brattice REF=<full-sha>`.

---

## Self-review against the spec

- §3.1 types → Task 1 (Gate/Way/Requirement/Hazard/Persistence, Node.key, Edge.gate, patterns/skipped_patterns). ✓
- §3.2 inventory (ten rows, one draw per realm, four skip reasons, creation order) → Task 1. ✓ The `patrol-path` "both sides" reading is a recorded Task 1 ruling.
- §3.3 substance table → Task 1 `admissible` + `try_apply`. ✓
- §3.4 solver, default body, resident, 0516 untouched → Task 1. ✓
- §3.5 three kinds, threshold on every passage, sump, chute, repairs, pairing test amended, wire, client, witness → Task 3. ✓ Execution amendment recorded: a sump's crossing is `Deep`.
- §3.6 locomotion store, accessor (recorded deviation: accessor not field), two seams, three refusals, drop verbs, narration → Task 4. ✓
- §3.7 door kind, identities, structural binding, Chattel state, verbs underground, anchor rule → Task 5. ✓ (False door: none placed; rule stated in spec only.)
- §3.8 properties → Tasks 1, 3, 6 tests; dof identity → Task 1 Step 8. ✓
- §4.1–4.5 → Task 2 (+ Task 1 determinism). ✓
- §5 epoch grep, fixture branch table, panel branch table → Tasks 0, 2, 3. ✓
- §7 acceptance 1–5 → Task 6 (1–3), Task 2 (4), Task 3 (5). ✓
- §9 decisions → Task 7. ✓
- Type consistency: `Gate.toward_a`/`toward_b` used the same way in Tasks 1, 3, 4, 5 (`toward_a` = from `b` to `a` = UP on a stair; a symmetric gate is read from `toward_a` everywhere). `Level.thresholds: Vec<(usize, usize, Cell)>` in Tasks 3, 4, 5. `Locomotion { swim, fly }` in Tasks 4, 6. `key_id`/`door_id` via `id_for_role(role, 0)` in Task 5. ✓
- Placeholders: none; the one "until Task 5" closure returns `false`, which is the Chattel's own default for a latent lock, and Task 5 replaces it.
