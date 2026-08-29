# The Gallery Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the underworld a band a player can stand in, walk, see, remember, and meet something living in — with the client's pane showing the cave rather than the forest overhead.

**Architecture:** `Session.underground` becomes a real position inside a generated `Level` (mirroring the existing `Inside`), `SpatialChannel` gains a third variant carrying `vessel/level/v1`, and `clients/game` gains one plate arm. Fog of war is a per-rung seen-bitset ORed with each step's shadowcast. Two performance changes ride along because they are in the blast radius and the kernel's own rules already demand them: `Level.cells` becomes dense storage, and the per-keypress full-snapshot parse in `walk_band_scene` is retired.

**Tech Stack:** Rust 2024, workspace crates `hornvale-vessel` / `hornvale-worldgen` / `hornvale-kernel`; `clients/game/{core,bin}` outside the workspace (gated by `make game-check`, NOT by `cargo clippy --workspace`).

**Spec:** `docs/superpowers/specs/2026-08-28-the-gallery-design.md`

## Global Constraints

- **Determinism is constitutional.** Same seed + pins produce byte-identical worlds, panes and transcripts. Any task that could move a seeded draw states so and proves it did not.
- **No `HashMap`/`HashSet`** (`clippy.toml` `disallowed-types`). `BTreeMap`/`BTreeSet`/`Vec` only.
- **No new external dependencies.** The allowlist is `serde`, `serde_json`, `libm` (`ALLOWED_EXTERNAL` in `cli/tests/suite/architecture.rs`).
- **Dense-index storage uses `Vec`, not a map** (`kernel/CLAUDE.md`).
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the most common review finding.
- **FRAME tier (decision 0069):** nothing this campaign adds to the underworld is serialized into the world file. The descent, the level, and the fog all die with the session.
- **Layering:** `kernel/` then `domains/*` then `windows/*` then `cli/`. A domain never depends on a sibling.
- **Decision block: 0406-0415.** Author records as `docs/decisions/0406-<slug>.md` upward.
- **Per-task gate:** `make gate-commit` before each commit. It is seconds-scale for `windows/`-layer edits and ~470 s after a `kernel/`-layer edit. No task here is kernel-layer.

---

## File Structure

**Created:**
- `windows/vessel/src/underworld_level/dense.rs` — the dense cell grid backing `Level`.
- `windows/vessel/src/underground.rs` — the `Underground` session-state struct, movement, stairs, and the seen-bitset. Kept out of `session.rs`, which is already ~9,000 lines.
- `windows/vessel/src/level_doc.rs` — projection of a `Level` plus fog into `vessel/level/v1`, mirroring `plan.rs`'s role for `vessel/plan/v1`.
- `clients/game/core/src/level.rs` — the client's level renderer, mirroring `plan.rs`.
- `docs/decisions/0406-*.md` and up — one per spec section 10 decision.

**Modified:**
- `windows/vessel/src/underworld_level/mod.rs` — `Level.cells` becomes dense.
- `windows/vessel/src/lattice/sight.rs` — `shadowcast` generalized over a transparency predicate.
- `windows/vessel/src/session.rs` — `underground` field retyped; `delve`/`climb`/`go`/`look`/`examine`/`map` arms; the fold pin's successor.
- `windows/vessel/src/snapshot.rs` — `SpatialChannel::Underground`.
- `clients/game/core/src/spread.rs` — one plate arm.
- `clients/game/bin/src/driver.rs` — `walk_band_scene`'s parse retired.
- `book/src/frontier/idea-registry.md`, `book/src/chronicle/the-gallery.md`, `docs/retrospectives/the-gallery.md`.

---

## Task 0: Measure the descent before choosing the water rule

The spec (section 3.2) leaves the flooded-cell rule open **deliberately**,
because a rule that makes the deepest rungs unreachable is a different campaign
from one that does not. This task produces the number; it changes no shipped
behaviour.

**Files:**
- Test: `windows/vessel/tests/suite/underworld_level_generation.rs` (add)

**Interfaces:**
- Consumes: `underworld_level::generate_descent_for_character`, `hornvale_terrain::rungs()`
- Produces: a reported measurement consumed by Task 4's rule choice. No API.

- [ ] **Step 1: Write a reporting probe**

It sweeps at least 50 seeds, builds a full descent per seed through the shipped
entry point, and prints per rung: total cells, walkable cells, flooded cells,
and — the number that decides the rule — **the fraction of each rung's walkable
area reachable from that rung's entry cell when `Flooded` is treated as
impassable**, versus when it is treated as passable.

The entry cell is the `StairsUp` cell for every rung below the first. Rung 0 has
none — `place_connections` emits `StairsUp` only when `has_up` is true — so
rung 0 measures from the same cell `delve` will place the possession on.

**The descent's inputs must be the production ones, and this is the whole
validity of the measurement.** Flooding is decided by depth against the water
table, so invented depths measure a fiction rather than the world. Derive them
exactly as `windows/worldgen/src/lib.rs:3259-3267` does at a cave-bearing
vertex:

```rust
let gradient   = terrain.geothermal_gradient_at(vertex);
let porosity   = terrain.material_at(vertex).porosity;
let water_table_m = hornvale_terrain::water_table_depth_m(
    terrain.drainage_at(vertex), porosity, surface.height_asl_m.get(),
);
// per rung, skipping the Nones (Band::Surface names no chamber):
let depth_m = hornvale_terrain::delve::rung_evaluation_depth_m(
    rung, gradient, cave.depth_reach_m,
);
```

`cave_kind` is `cave.kind`, never a hardcoded `Karst`. Reach a cave-bearing
vertex the way `session.rs`'s test module already does — `find_open_cave_vertex`
— rather than scanning for one by hand.

Report, never assert. This is a measurement, and whichever way it lands is the
finding — the posture `deep_realm_rehome.rs` takes, and the reason its header
says so out loud.

- [ ] **Step 2: Run it and record the numbers**

```
cargo test -p hornvale-vessel --test suite -- underworld_level_generation --nocapture > /tmp/hv-flood.log 2>&1; echo "exit=$?"
grep -E "rung|reachable" /tmp/hv-flood.log
```

- [ ] **Step 3: Choose the rule from the numbers, and write it into the spec**

**Decision rule, not a prediction:**
- If treating `Flooded` as impassable leaves every rung's stairs mutually
  reachable on 95% or more of seeds, then **flooded cells are impassable**,
  refused with a physical reason. Simplest, and costs nothing.
- If it strands stairs on a material fraction of seeds, then **flooded cells are
  walkable**, described as wading. Cheapest way to keep the world connected.
- If flooding is so extensive that walking it makes deep rungs
  indistinguishable from dry ones, then **route into the `submerged` band**, and
  say so. This is the expensive branch, and it is the one that may push Task 11
  out of the campaign.

Amend the spec's section 3.2 with the measurement and the chosen rule,
replacing the open question.

- [ ] **Step 4: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/tests/suite/underworld_level_generation.rs docs/superpowers/specs/2026-08-28-the-gallery-design.md
git commit -m "measure(the-gallery): how much of a descent is under water, and the rule that follows"
```

---

## Task 1: `Level.cells` becomes dense storage

`Level.cells` is a `BTreeMap<Cell, LevelCellKind>` whose own doc says it is
**TOTAL** over the extent — a dense, complete index in a tree map, against
`kernel/CLAUDE.md`'s rule and the measurement behind it (The Lookup: a
dense-keyed `BTreeMap` was ~22% of genesis self-time; a `Vec` dropped it to
~2%). Every cell read on the walk path pays an O(log N) traversal today.

**This is byte-identical by construction, and that was verified before the task
was written**, not asserted. No production path iterates the map implicitly:

```
$ awk '/#\[cfg\(test\)\]/{t=1} !t' windows/vessel/src/underworld_level/mod.rs \
    | grep -n "cells\.\(iter\|values\|keys\|retain\)"
$ awk '/#\[cfg\(test\)\]/{t=1} !t' windows/vessel/src/underworld_level/carve.rs \
    | grep -n "cells\.\(iter\|values\|keys\|retain\)"
(both empty)
```

Every access is `get`/`insert` at a computed `Cell`, and `first_walkable_cell`
spells its own column-major order in nested `for` loops rather than borrowing
the container's. No seeded choice can observe the change.

**Files:**
- Create: `windows/vessel/src/underworld_level/dense.rs`
- Modify: `windows/vessel/src/underworld_level/mod.rs`, `carve.rs`, `region.rs`

**Interfaces:**
- Produces:
  - `pub struct CellGrid { extent: Rect, cells: Vec<LevelCellKind> }`
  - `pub fn CellGrid::new(extent: Rect, fill: LevelCellKind) -> CellGrid`
  - `pub fn CellGrid::get(&self, c: Cell) -> Option<LevelCellKind>` — `None` if and only if outside the extent
  - `pub fn CellGrid::set(&mut self, c: Cell, k: LevelCellKind)` — no-op outside the extent
  - `pub fn CellGrid::iter(&self) -> impl Iterator<Item = (Cell, LevelCellKind)>` — **ascending `(x, y)`, matching `BTreeMap<Cell, _>`'s order exactly**, because `Cell(pub i32, pub i32)`'s derived `Ord` compares field 0 then field 1
  - `Level.cells` retyped from `BTreeMap<Cell, LevelCellKind>` to `CellGrid`

- [ ] **Step 1: Write the failing test**

In `dense.rs`'s own test module:

```rust
#[test]
fn a_dense_grid_agrees_with_a_btreemap_on_every_cell_and_on_order() {
    let extent = Rect { x: -3, y: 7, w: 11, h: 5 };
    let mut grid = CellGrid::new(extent, LevelCellKind::Wall);
    let mut map: std::collections::BTreeMap<Cell, LevelCellKind> =
        std::collections::BTreeMap::new();
    for x in extent.x..(extent.x + extent.w) {
        for y in extent.y..(extent.y + extent.h) {
            map.insert(Cell(x, y), LevelCellKind::Wall);
        }
    }
    let writes = [
        (0, 0, LevelCellKind::Floor),
        (10, 4, LevelCellKind::Flooded),
        (5, 2, LevelCellKind::StairsUp),
        (1, 3, LevelCellKind::StairsDown),
    ];
    for (dx, dy, k) in writes {
        let c = Cell(extent.x + dx, extent.y + dy);
        grid.set(c, k);
        map.insert(c, k);
    }
    let from_grid: Vec<(Cell, LevelCellKind)> = grid.iter().collect();
    let from_map: Vec<(Cell, LevelCellKind)> =
        map.iter().map(|(&c, &k)| (c, k)).collect();
    assert_eq!(
        from_grid, from_map,
        "dense iteration must match BTreeMap order exactly"
    );
    assert_eq!(
        grid.get(Cell(extent.x - 1, extent.y)),
        None,
        "outside the extent is None"
    );
    assert_eq!(grid.get(Cell(extent.x, extent.y)), Some(LevelCellKind::Floor));
}
```

- [ ] **Step 2: Run it and watch it fail**

```
cargo test -p hornvale-vessel dense:: > /tmp/hv-dense.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-dense.log
```
Expected: FAIL — `CellGrid` does not exist.

- [ ] **Step 3: Implement `CellGrid`**

Backing `Vec` indexed `(x - extent.x) * extent.h + (y - extent.y)`, so walking
the backing store front to back yields ascending `(x, y)` — the same order
`BTreeMap<Cell, _>` gives, for free, which is the property Step 1 pins. `get`
returns `None` outside the extent; `set` outside is a no-op.

- [ ] **Step 4: Run it and watch it pass**

```
cargo test -p hornvale-vessel dense:: > /tmp/hv-dense.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-dense.log
```

- [ ] **Step 5: Retype `Level.cells` and fix every call site**

`cargo check -p hornvale-vessel --all-targets` enumerates them. Do **not**
introduce a compatibility shim that converts back to a `BTreeMap` — that would
reintroduce the allocation this task removes, invisibly.

- [ ] **Step 6: Prove byte-identity against the pre-change generator**

The existing suite is the oracle. `underworld_level_generation.rs` already
asserts on generated levels; if any of its expectations move, the change is not
byte-identical and the task has failed.

```
cargo nextest run -p hornvale-vessel > /tmp/hv-vessel.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-vessel.log
```

**Decision rule:** any failure in `underworld_level_generation.rs` means the
conversion changed generation — STOP and find which access became
order-dependent, rather than rebaselining the test.

- [ ] **Step 7: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/underworld_level/
git commit -m "perf(underworld): a level's cells are dense storage, not a tree map"
```

---

## Task 2: Generalize the shadowcaster over a transparency predicate

`shadowcast(lattice: &Lattice, from: Cell, radius: i32)` is concrete. A `Level`
is the same shape (a `Rect` extent, one kind per cell) with a different value
type, and a cave has no chambers to fill `CellKind::Floor(usize)` with — so
converting a `Level` into a `Lattice` is not available, and a second copy of a
property-tested symmetric shadowcaster is not acceptable.

`CellKind::passable`'s own doc already argues the direction: *"a rule written
against the variant breaks the day `Rubble` arrives; a rule written against the
predicate survives it."*

**Files:**
- Modify: `windows/vessel/src/lattice/sight.rs`

**Interfaces:**
- Produces:
  - `pub fn shadowcast_with(transparent: impl Fn(Cell) -> bool, in_bounds: impl Fn(Cell) -> bool, from: Cell, radius: i32) -> BTreeSet<Cell>`
    — **no `extent` parameter.** An earlier draft of this brief mandated one;
    verified against the code before dispatch, `shadowcast` never reads
    `lattice.extent` (it reaches the lattice only through `transparent` and
    `contains_key`), so an extent argument would arrive dead. The recursive
    `scan` helper threads the two predicates by reference.
  - `pub fn shadowcast(lattice: &Lattice, from: Cell, radius: i32) -> BTreeSet<Cell>` — unchanged signature, now a thin wrapper

- [ ] **Step 1: Pin current behaviour before touching it**

Add a characterization test recording `shadowcast`'s exact output for a
hand-built lattice with a wall, a doorway and an open run — the full
`BTreeSet<Cell>`, not a count. This is the oracle for Step 4.

- [ ] **Step 2: Run it and watch it PASS**

```
cargo test -p hornvale-vessel sight:: > /tmp/hv-sight.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-sight.log
```
Expected: PASS. A characterization test that fails against unchanged code is
recording the wrong thing, and must be fixed before proceeding.

- [ ] **Step 3: Extract the predicate**

Move the body to `shadowcast_with`, taking `transparent` and `in_bounds` as
closures. Reimplement `shadowcast` as a wrapper passing
`|c| lattice.cells.get(&c).is_some_and(|k| k.passable())` and
`|c| lattice.cells.contains_key(&c)`.

The two closures stay separate because the existing implementation
distinguishes them, at `sight.rs:222` and `:226` of the same loop:

```rust
let opaque = !transparent(lattice, cell);                  // outside extent -> opaque
if (opaque || is_symmetric(..)) && lattice.cells.contains_key(&cell) {   // but not insertable
    lit.insert(cell);
}
```

A cell outside the extent BLOCKS sight and must never enter the result. Folding
the two into one predicate either leaks light past the boundary (if outside
becomes transparent) or inserts out-of-bounds cells into `lit`. Verified by
reading the loop, not inferred.

- [ ] **Step 4: Run the whole sight suite**

```
cargo nextest run -p hornvale-vessel sight > /tmp/hv-sight2.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-sight2.log
```
Expected: PASS, including `sight_is_symmetric` (a property test over every
ordered floor pair), `a_wall_blocks_what_lies_behind_it`, and Step 1's
characterization test byte for byte.

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/lattice/sight.rs
git commit -m "refactor(sight): shadowcast over a transparency predicate, so two bands share one caster"
```

---

## Task 3: `Underground` — the session's position in a descent

**Files:**
- Create: `windows/vessel/src/underground.rs`
- Modify: `windows/vessel/src/session.rs`, `windows/vessel/src/lib.rs`

**Interfaces:**
- Consumes: Task 1's `CellGrid`, `underworld_level::generate_descent_for_character`
- Produces:
  - `pub struct Underground { descent: Vec<Level>, rung: usize, cell: Cell, seed: Seed }`
    — Task 6 adds the `seen: Vec<SeenBits>` field. It is **not** declared here:
    a field with no reader is a field a reviewer cannot judge, and `SeenBits`
    is not defined until Task 6.
  - `pub fn Underground::level(&self) -> &Level`
  - `pub fn Underground::rung_band(&self) -> hornvale_kernel::Band`
  - `Session.underground: Option<Underground>` (was `Option<Chamber>`)

The struct is named `Underground` and Task 7's `SpatialChannel` variant is also
`Underground`. They live in different modules and never appear in one scope. If
that reads badly at implementation time, rename **the session struct** — the
wire tag `band: "underground"` is fixed by the spec, and the channel variant
should match it.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn delve_places_the_possession_on_a_real_cell_of_a_generated_level() {
    let world = seam_world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let terrain = session.wctx.terrain.clone().expect("seed 42 builds terrain");
    let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
    session.delve_at(vertex, cave);

    let ug = session.underground.as_ref().expect("a resolved descent");
    assert!(!ug.descent.is_empty(), "a descent has at least one rung");
    assert_eq!(ug.rung, 0, "you enter at the top rung");
    assert!(
        matches!(
            ug.level().cells.get(ug.cell),
            Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)
        ),
        "the possession stands on a standable cell, not inside rock"
    );
}
```

`find_open_cave_vertex` and `seam_world` already exist in `session.rs`'s test
module — the fold-pin test uses both.

- [ ] **Step 2: Run it and watch it fail**

```
cargo test -p hornvale-vessel delve_places > /tmp/hv-t3.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t3.log
```
Expected: FAIL — `underground` is still `Option<Chamber>`.

- [ ] **Step 3: Implement**

`delve_at` keeps every existing early refusal, in order (no cave here; a barred
passage; a sealed chamber). Those are The Latch's and are not this campaign's
to move. Where it currently assigns `self.underground = Some(chamber)`, it now
builds the descent through `generate_descent_for_character` and places the
possession on the entrance rung's first standable cell.

**The descent's inputs are the production recipe, exactly as Task 0's probe
derives them** — and this matters more here than it did there, because Task 0
only measured while this ships the descent a player walks. An invented depth or
water table produces a fiction in every world, and nothing goes red:

```rust
let gradient   = terrain.geothermal_gradient_at(vertex);
let porosity   = terrain.material_at(vertex).porosity;
let water_table_m = hornvale_terrain::water_table_depth_m(
    terrain.drainage_at(vertex), porosity, surface.height_asl_m.get(),
);
// per rung, skipping the Nones — Band::Surface names no chamber:
let depth_m = hornvale_terrain::delve::rung_evaluation_depth_m(
    rung, gradient, cave.depth_reach_m,
);
```

`cave_kind` is `cave.kind`, never a hardcoded `Karst`. Origins are
`ChamberOrigin::Found` for every rung (the shipped path constructs no
`ChamberOverrides`, so `Made` is unreachable there) and the character is
`Character::WildCave`, matching `generate_descent`'s own hardcoded value.

`windows/vessel/tests/suite/underworld_level_generation.rs`'s
`measure_flooded_cell_reachability_across_the_descent` (Task 0) already does
all of this — **read it as the worked example rather than re-deriving it.**
Both come from the same terrain handle `delve_at` has already resolved, so no
second independently-chosen lookup is introduced, which is the property
`delve_at`'s existing doc comment claims for itself.

**`Underground`'s fields are `pub(crate)`.** The struct lives in a new module
while `session.rs`'s test module reaches into it; the precedent it mirrors,
`Inside`, is private only because it is declared in `session.rs` itself.
`pub(crate)` is the narrowest visibility that compiles. Unit tests for
`Underground`'s own mechanics belong in `underground.rs`'s test module, as
`lattice/sight.rs` already does; only session-level integration tests stay in
`session.rs`.

`climb` clears `underground` only from `rung == 0`. From a deeper rung it
refuses and says to take the stairs up.

- [ ] **Step 4: Run it and watch it pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t3b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t3b.log
```
Expected: PASS. `the_underground_band_folds_into_walk_as_map_does` is still
green here — the pane does not change until Task 7.

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/underground.rs windows/vessel/src/session.rs windows/vessel/src/lib.rs
git commit -m "feat(underground): a descent, and a cell you stand on in it"
```

---

## Task 4: Walking

**Files:**
- Modify: `windows/vessel/src/underground.rs`, `windows/vessel/src/session.rs`

**Interfaces:**
- Produces: `pub fn Underground::step(&mut self, dir: Compass) -> StepOutcome`,
  where `StepOutcome` is `Moved`, `Blocked(&'static str)`, or `NeedsStairs`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_compass_step_underground_moves_one_cell() {
    let world = seam_world();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let terrain = session.wctx.terrain.clone().expect("seed 42 builds terrain");
    let (vertex, cave) = find_open_cave_vertex(&terrain, world.seed);
    session.delve_at(vertex, cave);

    let before = session.underground.as_ref().expect("descended").cell;
    // Try each bearing until one is not rock; at least one must be, because
    // the possession was placed on a cell of a connected level.
    let moved = ["n", "s", "e", "w"].iter().any(|d| {
        session.handle(&format!("go {d}"));
        session.underground.as_ref().expect("still below").cell != before
    });
    assert!(moved, "at least one bearing from a standable cell must be walkable");
}

#[test]
fn rock_refuses_a_step_with_a_physical_reason() {
    // Assert the refusal is PHYSICAL: not a parse error, and it does not
    // mention verbs or modes. The exact wording is the implementer's.
}

#[test]
fn the_lateral_refusal_is_gone() {
    // UNDERGROUND_LATERAL_REFUSAL's text appears nowhere in a walking
    // session's output. Its own doc says "there is nowhere down here for a
    // bearing to mean", which this task makes false.
}
```

- [ ] **Step 2: Run them and watch them fail**

```
cargo test -p hornvale-vessel underground > /tmp/hv-t4.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t4.log
```

- [ ] **Step 3: Implement**

Delete `UNDERGROUND_LATERAL_REFUSAL` and its `"go" if self.underground.is_some()`
arm. Route a compass step to `Underground::step`.

**The water rule is spec 3.2, as rewritten at the Task 0 stop — read it, do not
infer it from Task 0's commit message.** Three parts, and this is the only
place any of them is expressed; do not scatter them across the movement, sight
and rendering paths.

1. **Wet cells are walkable — you wade.** `Flooded` is passable. The probe's
   own `reach % (Flooded passable)` column is 100.0 at every rung, so this
   keeps every level connected.

2. **Wetness keys on `LeafStyle.worked`.** A worked leaf is drained; a natural
   leaf is wet. `leaf_styles` is index-aligned with `region::leaves(&tree)`
   (both built in one pass), so the flooding pass in
   `generate_level_with_water` reads its own leaf's `worked` — it does not
   re-derive the tree for it. This makes a drow-tier descent dungeon-dry and a
   wild cave wet, from a dial that is already turning.

3. **Movement is a mode, not a boolean.** One named seam answers "how can this
   body move through this cell", returning `Walk` or `Wade` today, with `Swim`
   and `Fly` reserved as variants it will return later. Same shape as Task 6's
   reach seam.

**Three constraints inherited from the indoor compass step
(`session.rs:3413`), which is this task's precedent — read it before writing
`step`:**

- **Lateral movement never changes band** (metaplan 1b.6). A cell step stays
  inside the underworld band and must NOT read or write the possession's
  walk-band `position`. The indoor path states this explicitly about itself;
  the same law binds here, and getting it wrong ("I moved, so update
  position") would teleport the possession on the surface.
- **Orthogonal only.** Refuse a diagonal before any lookup, as
  `INDOOR_DIAGONAL_REFUSAL` does — slipping through the corner where two walls
  meet is not a way through rock. Task 0's own reachability BFS is 4-way, so
  the geometry already assumes this.
- **Ask the passability predicate, never `== LevelCellKind::Wall`.** The same
  rule `CellKind::passable`'s doc gives: a rule written against the variant
  breaks the day a new impassable kind arrives.

**Not in this task:** drowned rungs, dive-entry from above, and swimming.
They are designed in spec 3.2 and deferred — they need a capability model that
does not exist, and The Chattel is building the object model that would carry
it. The mode enum reserves their variants; nothing returns them yet.

**Add a regression test for part 2**, since it is the part with a number
behind it: a descent generated with a heavily-worked character must come out
substantially drier than one generated with a natural-cave character, on the
same seed. Assert the DIRECTION and a margin, not a fixed percentage — the
percentage is a calibration this task does not own.

- [ ] **Step 4: Run and watch pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t4b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t4b.log
```

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/underground.rs windows/vessel/src/session.rs
git commit -m "feat(underground): go walks the level, and rock refuses"
```

---

## Task 5: Stairs

**Files:**
- Modify: `windows/vessel/src/underground.rs`, `windows/vessel/src/session.rs`

**Interfaces:**
- Produces: `pub fn Underground::take_stairs(&mut self) -> Option<hornvale_kernel::Band>`

- [ ] **Step 1: Write the failing test**

The property that matters is **round-tripping**: descending from rung *n* and
climbing back arrives on rung *n* — not necessarily the same cell, since the
generator places up and down stairs independently, but the same rung, with a
stable identity.

```rust
#[test]
fn stairs_connect_adjacent_rungs_in_both_directions() {
    // Place the possession directly on the StairsDown cell — found by
    // scanning the level — rather than walking there. Steering a walk to a
    // particular cell is impractical from a test, which is the same reason
    // `delve_at` exists as a seam.
}
```

- [ ] **Step 2: Run it and watch it fail**

```
cargo test -p hornvale-vessel stairs > /tmp/hv-t5.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t5.log
```

- [ ] **Step 3: Implement**

Moving between rungs requires standing on a stairs cell, and refuses with a
physical reason otherwise. Whether the verbs are new (`down`/`up`) or the
existing `delve`/`climb` re-pointed is the implementer's call; **record which,
and why, in the task report** — it is a player-facing vocabulary decision, and
Task 12's chronicle needs it.

**If you add a verb, it goes in both lists a verb has to be in, and here they
are by name:**

- `IN_CHARACTER_VERBS` (`session.rs:126`), the roster the body-state gate
  consults. It is declared `[&str; 20]` — an explicit array length, so adding
  an entry forces you to bump it and the compiler will not let you forget.
- `HELP` (`session.rs:~401`), the text a player reads. Its existing `delve`
  and `climb` lines are the format to match.

The Latch shipped a verb that was in NEITHER and no gate caught it: a verb
absent from both satisfies an agreement check in both directions, so the
structural check between the two lists is blind to it. What catches it is the
behavioural test — put the body to sleep, type the verb, require the refusal —
and the pattern to copy is `warm_is_refused_while_asleep`, which the roster's
own doc comment names.

**If you instead re-point `delve`/`climb`, both lists already contain them and
neither needs an entry — but their HELP text becomes wrong**, because it
describes surface-to-cave movement and would now also mean rung-to-rung. Update
it in that case; a help line that describes half of what a verb does is the
same defect one layer along.

- [ ] **Step 4: Run and watch pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t5b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t5b.log
```

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/underground.rs windows/vessel/src/session.rs
git commit -m "feat(underground): stairs connect the rungs"
```

---

## Task 6: Fog of war — the seen-bitset and the reach seam

**Files:**
- Modify: `windows/vessel/src/underground.rs`, `windows/vessel/src/session.rs`

**Interfaces:**
- Produces:
  - `pub struct SeenBits { extent: Rect, bits: Vec<u64> }`
  - `pub fn SeenBits::saw(&self, c: Cell) -> bool`
  - `pub fn SeenBits::mark_all(&mut self, cells: &BTreeSet<Cell>)`
  - `pub fn Session::sight_reach(&self) -> i32` — **the seam.** Today it returns
    the implicit torch's reach; later it reads carried light.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn walking_only_ever_adds_to_what_is_remembered() {
    // Monotonicity (spec 4.1.2, acceptance 4b). Walk a route; after each
    // step assert the seen-set is a superset of the previous one. Structural
    // under a bitset — this pins that the wiring above it never clears bits.
}

#[test]
fn a_wider_reach_does_not_change_what_was_already_seen() {
    // Mark from a cell at a small reach, then from the SAME cell at a larger
    // one. Bits already set stay set; no bit is cleared. A bitset gets this
    // for free; the test says so out loud so a future refactor to a
    // recomputed representation cannot quietly lose it.
}

#[test]
fn the_reach_seam_is_the_only_source_of_the_radius() {
    // Property, not a prescribed mutation: no literal sight radius appears on
    // the underground sight path. The implementer picks the check that
    // demonstrates it; a default-deny source scan has precedent in this repo.
}
```

- [ ] **Step 2: Run them and watch them fail**

```
cargo test -p hornvale-vessel seen > /tmp/hv-t6.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t6.log
```

- [ ] **Step 3: Implement**

`SeenBits` is one bit per cell of the rung's extent, row-major, packed into
`Vec<u64>`. The largest rung is 60x34 = 2,040 cells = 255 bytes; a five-rung
descent is about 1.2 KB.

Each step ORs `shadowcast_with(...)` (Task 2) into the current rung's bitset, at
`Session::sight_reach()`.

`sight_reach` must also become the source of `chamber_sources`' TORCH radius,
which today hardcodes `radius: SIGHT_RADIUS` separately from the shadowcaster's
own constant — two places holding one number, and neither one a place a lantern
could plug into.

**`chamber_sources` has THREE `radius: SIGHT_RADIUS` sites and only ONE is
yours.** Verified against the code before dispatch:

| line | source | whose reach |
|---|---|---|
| `session.rs:4100` | the implicit torch, at the possession's own cell | **the body's — this is the seam** |
| `session.rs:4120` | the hearth | the fire's |
| `session.rs:4128` | each doorway | the opening's, carrying daylight |

They are textually identical and semantically different. **Change 4100 only.**
Routing the other two through the body's reach would mean picking up a lantern
brightens every hearth and every doorway in the building, which is not what
carrying a lamp does.

- [ ] **Step 4: Pin the lifetime, in both directions**

Spec acceptance criterion 5, which no other task covers:

```rust
#[test]
fn fog_survives_moving_between_rungs_and_dies_on_climbing_out() {
    // Descend, walk to accumulate seen cells, take the stairs down, then take
    // them back up: the first rung is still remembered (fog is per-rung and
    // per-descent, not per-visit).
    //
    // Then `climb` to the surface and `delve` again: the fog is EMPTY. That
    // is the session-lifetime cut the spec makes deliberately (3.5), and it
    // must be asserted rather than left to be discovered — if a later
    // campaign makes fog durable, this is the test it comes to rewrite.
}
```

- [ ] **Step 5: Run and watch pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t6b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t6b.log
```

- [ ] **Step 6: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/underground.rs windows/vessel/src/session.rs
git commit -m "feat(underground): fog of war, and a reach seam a lantern can fill"
```

---

## Task 7: `vessel/level/v1` and the third `SpatialChannel` variant

**SCHEMA TASK.** `vessel/level/v1`'s field order becomes contract on landing.

**Files:**
- Create: `windows/vessel/src/level_doc.rs`
- Modify: `windows/vessel/src/snapshot.rs`, `windows/vessel/src/session.rs`

**Interfaces:**
- Produces:
  - `pub struct SessionLevel { schema, rung, depth_m, extent, palette, cells, you, marks }`
  - `SpatialChannel::Underground { level: Box<SessionLevel> }`, wire tag `band: "underground"`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_underground_pane_reads_band_underground() {
    // descend, snapshot, assert the JSON contains r#""band":"underground""#
}

#[test]
fn a_never_seen_cell_is_absent_from_the_document() {
    // Spec 4.1.1. On the first turn after descending, the document's cell
    // count is strictly less than the rung's extent area.
}

#[test]
fn the_three_visibility_states_are_distinguishable_without_colour() {
    // Spec 4.1. Assert on the STATE field, never on a colour: the document
    // must distinguish here / lit / remembered with colour absent entirely.
}
```

- [ ] **Step 2: Run and watch them fail**

```
cargo test -p hornvale-vessel level_doc > /tmp/hv-t7.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t7.log
```

- [ ] **Step 3: Implement**

`SessionLevel` mirrors `SessionPlan`'s shape (extent, palette, row-major cell
indices, `you`, `marks`) and adds a per-cell **visibility state**, plus `rung`
and `depth_m`.

Never-seen cells are **omitted, not flagged** (spec 4.1.1). The palette interns
`(LevelCellKind, state)`.

Box the variant's payload: `SpatialChannel` already boxes `SurroundsScene` for
`clippy::large_enum_variant` (The Grain), and `Box<T>` serializes exactly as
`T`, so this changes no byte on the wire.

Field order is JSON key order and is contract — say so in the doc comment, as
`SessionPlan`'s does.

- [ ] **Step 4: Run and watch pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t7b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t7b.log
```

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/level_doc.rs windows/vessel/src/snapshot.rs windows/vessel/src/session.rs
git commit -m "feat(wire): vessel/level/v1, and the underground band on the pane"
```

---

## Task 8: The fold, retired

**Files:**
- Modify: `windows/vessel/src/session.rs` (the pin, and `map`'s band arms)

- [ ] **Step 1: Rewrite the pin — do not delete it**

`the_underground_band_folds_into_walk_as_map_does` states its own disposition:

> the invariant worth pinning is not "the pane is right here" but "the pane and
> the verb cannot drift apart here": whichever answer the sim settles on, one
> change must move both.

The successor asserts that same agreement against the new answer: the pane
reads `band: "underground"`, and `map` in the same state draws the level rather
than the country overhead. Rename it for what it now pins, and carry the
original's reasoning forward in its doc comment.

- [ ] **Step 2: Run it and watch it fail**

```
cargo test -p hornvale-vessel underground_band > /tmp/hv-t8.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t8.log
```
Expected: FAIL — `map` still guards on `inside` alone.

- [ ] **Step 3: Give `map` an underground arm**

- [ ] **Step 4: Run the whole vessel suite**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t8b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t8b.log
```

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/session.rs
git commit -m "feat(underground): the pane and the verb both show the cave now"
```

---

## Task 9: The client draws the level

**Files:**
- Create: `clients/game/core/src/level.rs`
- Modify: `clients/game/core/src/spread.rs`, `clients/game/core/src/lib.rs`

**Gate note:** `clients/game/core` and `clients/game/bin` are in the workspace
`exclude` list, so `cargo clippy --workspace` does not build them and
`make gate-commit` will not catch a break here. `make game-check` is this
task's gate.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_remembered_cell_and_a_lit_cell_draw_different_glyphs() {
    // The monochrome property (spec 4.1), tested at the RENDERER: build a
    // SessionLevel with one lit and one remembered floor cell, draw it, and
    // assert the two grid positions hold different characters.
}
```

- [ ] **Step 2: Run and watch it fail**

```
cargo test --manifest-path clients/game/core/Cargo.toml level > /tmp/hv-t9.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t9.log
```

- [ ] **Step 3: Implement the renderer and the plate arm**

**The client has its OWN `Spatial` enum** (`clients/game/core/src/schema.rs:100`),
a `#[serde(tag = "band")]` deserialization mirror carrying `Walk` and
`Chamber`. It does not match the sim's type. **Nothing forces this update** —
verified: `cargo check -p hornvale-game-core --all-targets` exits 0 today, with
`SpatialChannel::Underground` already landed. An earlier draft of this plan and
of spec 4.3 both claimed a compile error would enforce it; both were wrong and
both are corrected.

So the mirror enum gains an `Underground { level: Level }` variant, `spread.rs`
gains its arm, and — because the compiler will not — **you also build the
enforcement**:

- [ ] **Step 3b: Pin that the client can read every band the sim emits**

A test in `clients/game/core` that parses a snapshot for each band and asserts
it deserializes. It fails the moment the sim grows a band the client cannot
read. Without it the next band repeats this exactly: a silent blank pane,
because `Driver::refresh` swallows a parse failure by design
(`.unwrap_or_default()`, `if let Ok(snap)`) and leaves the previous state
standing.

Use the committed fixtures where they serve
(`clients/game/core/tests/fixtures/session-seed-42-turn-0.json` is `walk`,
`session-seed-42-chamber.json` is `chamber`); an underground fixture does not
exist yet, so construct that case rather than skipping it — a two-band test
that silently omits the third is the same gap one layer up.

Glyph selection is the CLIENT's (decision 0022). Remembered cells get a **glyph
twin**, the way the walk band's `faded()` maps `.` to `,`, never a dimmer
colour.

- [ ] **Step 4: Run the client gate**

```
make game-check > /tmp/hv-game.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-game.log
```

- [ ] **Step 5: Commit**

```
cargo fmt --manifest-path clients/game/core/Cargo.toml
git add clients/game/core/src/
git commit -m "feat(game): the plate draws the cave you are standing in"
```

---

## Task 10: Retire the per-keypress snapshot parse

Not a detour: `walk_band_scene` matches exhaustively on `SpatialChannel`, so
Task 7 forces an edit here regardless.

`walk_band_scene` (`driver.rs:2250`) parses the **entire** cached snapshot JSON
to read one discriminant, then makes a sim call — and it runs from
`compose_perception_layer` on every redraw. That is The Quadrat's measured
"full snapshot parse plus a purview call on every keypress, including keypresses
that are just typing", 0.089 ms to 2.124 ms.

The `Driver` already caches that answer in `on_walk_band`, whose own doc says it
exists because *"parsing the snapshot JSON in each of them would put several
serde_json passes on the cursor path to answer a question that changes only when
a turn does."* This call site did not get that fix.

**Files:**
- Modify: `clients/game/bin/src/driver.rs`

- [ ] **Step 1: Measure before**

Time a fixed sequence of ordinary typing keypresses on the walk band. Record
the number; it is Step 4's baseline. Take it on an unloaded machine — this repo
has discarded a timing pair taken at load average 50 as 3.3x wrong.

- [ ] **Step 2: Write the failing test**

```rust
#[test]
fn composing_the_perception_layer_does_not_parse_the_snapshot() {
    // Property, not a prescribed mutation: the implementer picks the check
    // that demonstrates the parse is gone from this path. A counter on the
    // parse site, or a source-level assertion, are both acceptable.
}
```

- [ ] **Step 3: Read the band from `on_walk_band`**

It is already maintained by `refresh`, which runs once per turn, and it stays
correct with a third band, since underground is not walk.

- [ ] **Step 4: Measure after, and report both numbers**

**Decision rule:** if the measurement does not improve, say so and keep the
change anyway on correctness grounds — one source of truth for the band. A null
is a result, and this repo ships nulls as headlines.

- [ ] **Step 5: Commit**

```
make game-check
git add clients/game/bin/src/driver.rs
git commit -m "perf(game): the perception layer reads the cached band, not a fresh JSON parse"
```

---

## Task 11: Inhabitants

**Files:**
- Modify: `windows/vessel/src/underground.rs`, `windows/vessel/src/level_doc.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::subterranean_substrate`,
  `hornvale_worldgen::energy::{subterranean_energy, dominant_source}`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn who_is_underground_derives_from_the_chambers_own_conditions() {
    // The property, not a fixed roster: perturb the chamber's energy input
    // and require the roster to move; a chamber that can feed nothing holds
    // nothing. Do NOT assert a named species — that would pin a calibration
    // this task does not own.
}

#[test]
fn a_creature_underground_is_drawn_only_while_lit() {
    // Spec 4.1.2: entities enter at the `lit` rung, terrain at `remembered`.
    // The chamber band already does this correctly (marks are a strict
    // subset of present); this asserts the underground band does too.
}
```

- [ ] **Step 2: Run and watch them fail**

```
cargo test -p hornvale-vessel inhabit > /tmp/hv-t11.log 2>&1; echo "exit=$?"
tail -20 /tmp/hv-t11.log
```

- [ ] **Step 3: Implement**

Compose the shipped readings at the chamber's own depth. Do not build a spawn
table, and do not read the surface roster.

- [ ] **Step 4: Run and watch pass**

```
cargo nextest run -p hornvale-vessel > /tmp/hv-t11b.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-t11b.log
```

- [ ] **STOP CONDITION (spec section 9).** If the derivation cannot be expressed
  as a read over existing fields within this task, **do not grow a placement
  model here.** Ship geometry, pane, and the empty-chamber case; add an
  idea-registry row for the placement model; record it in the task report; and
  continue to Task 12. That is an accepted outcome, not a failure.

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add windows/vessel/src/
git commit -m "feat(underground): something lives down here, and its rock decides what"
```

---

## Task 12: Decisions, registry, artifacts, book

**Files:**
- Create: `docs/decisions/0406-*.md` and up (one per spec section 10 decision)
- Modify: `book/src/frontier/idea-registry.md`, `book/src/chronicle/the-gallery.md`, `docs/retrospectives/the-gallery.md`

- [ ] **Step 1: Write the decision records**

One per spec section 10, in the reserved block 0406-0415. Gaps inside the block
are fine and cost nothing.

- [ ] **Step 2: Update the idea registry**

- `MAP-underworld-chart` becomes `shipped`.
- `CLIENT-band-fold` is **narrowed, not closed.** The `submerged` band still
  folds into `walk`, deliberately. The row must say so, or the next reader
  believes a solved problem is open — or worse, that an open one is solved.
- `MAP-underworld-dressing`: note that its stated blocker (no chart) is gone.

- [ ] **Step 2b: Re-score the systems corpus, because nothing will tell you it
  is stale**

`docs/audits/system-coverage-wolverson-2021.md` is GENERATED from **authored
verdicts** in `systems/wolverson-2021.system.json`. If nobody edits those
verdicts, the report does not drift, `git diff --exit-code` passes, and the
audit keeps reporting a resolved gap as open. **The drift check cannot see
this**, which is exactly the failure shape this campaign has hit repeatedly:
a record outliving its subject, with the guard green.

Three items cite `registry:MAP-underworld-chart` as their blocker, and all
three describe the fold this campaign retires:

| item | line | what its note currently claims |
|---|---|---|
| 2.11 Delving Deeper | 84 | "the underground band FOLDS INTO `walk`… You can descend and cannot see where you are" |
| 5.13 Into the caverns | 126 | "the underworld has no chart of its own, so the pane and the verb both draw the surface overhead" |
| 5.17 Deep caverns | 130 | "RENDER half is the deciding one and is absent" |

Re-score each against what the campaign actually shipped, and **re-score
honestly rather than optimistically** — the corpus's own provenance says
`present` is the verdict it is least entitled to, and a weakest-half item is
decided by its weaker half. If the render half is delivered but thin, say
which. If an item's blocker moved rather than cleared, cite the new one.

Regenerate with `cargo run -p hornvale -- systems --corpus
systems/wolverson-2021.system.json report` and commit the moved report with
the corpus edit that caused it.

Check the same way for any OTHER audit under `docs/audits/` whose authored
content names the fold, the `map` verb underground, or
`the_underground_band_folds_into_walk_as_map_does`. A generated file with
authored inputs is stale-by-default, not fresh-by-default.

- [ ] **Step 3: Regenerate artifacts and read the diff**

```
make rebaseline > /tmp/hv-rebase.log 2>&1; echo "exit=$?"
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Decision rule (spec section 7):**
- Only `docs/audits/` moved: expected. The type-audit report drifts on any
  pub-boundary change, and this campaign adds several. Commit in the same
  commit.
- `docs/digest/` moved: expected. The in-force decision index drifts when
  decisions are added.
- A **walk-band or chamber-band** client fixture moved: **STOP.** That is a
  schema regression, not drift.
- The **census** moved: **STOP and investigate.** Nothing here changes world
  generation; if a census column moved, find out why before merging.

- [ ] **Step 3b: Pin determinism end to end**

Spec acceptance criterion 9, which no other task covers. Every prior task
asserts a behaviour; none asserts the campaign's constitutional property.

```rust
#[test]
fn the_same_seed_and_pins_produce_a_byte_identical_descent_and_pane() {
    // Two independent sessions on the same seed, driven through the SAME
    // scripted walk (delve, several steps, stairs, several more). Assert the
    // two snapshot JSON strings are byte-equal at every turn — not merely the
    // final one, which would pass even if the two diverged and reconverged.
    //
    // This is the test that would catch Task 1's dense-storage conversion
    // having changed an iteration order, Task 2's shadowcaster refactor
    // having changed a result set, and Task 11's inhabitant derivation having
    // read something unstable. It is cheap and it is the campaign's floor.
}
```

Run it before the artifact regeneration below, not after: a determinism failure
makes every diff in Step 3 meaningless.

```
cargo nextest run -p hornvale-vessel > /tmp/hv-det.log 2>&1; echo "exit=$?"
grep -E "^ *Summary|FAILED|panicked" /tmp/hv-det.log
```

- [ ] **Step 3c: Sweep the stale verb-count prose**

`windows/lab/tests/suite/reticence_calibration.rs:343` says "20-verb roster".
Task 5 took `IN_CHARACTER_VERBS` to 22. It is non-assertive prose and fails no
gate, which is exactly why it rots — fix it here rather than leaving a wrong
number in a file the next reader will trust.

`clients/game/bin/src/driver.rs:55` (module doc) still says the scene "is
re-derived with `self.session.purview(0)`". Task 10 made that false — the scene
is a cached read refreshed once per turn. The conclusion the paragraph draws
(resolver and picture "cannot disagree") is still true, but now for a different
reason: a shared cache rather than two independent deterministic calls. Fix the
reason, keep the conclusion.

**Grep for the words the STALE prose uses, not the words you would use.** Task
10's own sweep searched `per keypress|per redraw|every keypress|every redraw`
and missed this line because it says "re-derived". That is the documented
failure mode of the grep-the-crate remedy, and it is why this sweep is a task
step rather than a habit.

- [ ] **Step 4: Chronicle and retrospective**

The chronicle is written at the book's altitude — technical, comprehensible
without reading the code it may show. The retrospective carries the follow-up
register from `.superpowers/sdd/followups.md`, which dies with the worktree, so
promote it before teardown.

- [ ] **Step 5: Commit**

```
cargo fmt && make gate-commit
git add docs/ book/
git commit -m "docs(the-gallery): decisions, registry, chronicle, retrospective"
```

---

## Closing

Stage-gate at the Task 6 boundary and again before merge:

```
make sluice-stage BRANCH=campaign/the-gallery REF=<full-sha>
```

Then `closing-a-campaign`.
