# The Crosscut Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give every walked underworld descent a series-parallel plan graph with cycles — some spanning floors — generated before any level is carved, and realize it into the existing level generator.

**Architecture:** A new `windows/worldgen/src/circuit.rs` grows a `DescentPlan` (nodes = grid regions per level, edges = passages and coordinate-paired stairs, realms = cycles with Dormans length classes) from four seeded stream legs keyed by `(seed, vertex)`. `windows/vessel/src/underworld_level/` stops building a partition tree and instead carves one region per plan node with the Adit's existing algorithms, cuts passages along plan edges, and places stairs at the plan's shared coordinates. `Underground::peek_stairs` lands on the same coordinate one rung over. A `hornvale circuit --seed N` readout writes the preregistered verdicts to a committed audit page.

**Tech Stack:** Rust 2024 workspace; `hornvale_kernel::{Seed, Stream, StreamLabel, Band, Vertex}`; `hornvale_terrain::CaveKind`; `BTreeMap`/`BTreeSet`/`Vec` only (no `HashMap`); no new crates.

**Spec:** `docs/superpowers/specs/2026-09-01-the-crosscut-design.md` (read §3 and §4 before any task). Metaplan: `docs/superpowers/specs/2026-09-01-the-circuit-metaplan.md`. Ledger: `docs/superpowers/ledgers/2026-09-01-the-crosscut.md` — append a ruling there the moment you make one.

## Global Constraints

- Layering: `kernel -> domains/* -> windows/* -> cli`. `windows/worldgen` may not depend on `windows/vessel`; `vessel` already depends on `worldgen`.
- `#![warn(missing_docs)]` everywhere: every `pub` item, field and variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `/// type-audit: bare-ok(<class>: <name>)` tag (classes used here: `count`, `index`, `ratio`, `flag`, `diagnostic-value`). `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` is default-deny and part of `make gate-commit`.
- No `HashMap`/`HashSet`; no wall-clock; float sorts use `total_cmp`.
- Stream labels are permanent contracts. New labels are additive. `underworld/level/v1/partition` is RETIRED OUTRIGHT (removed from `windows/vessel/src/streams.rs` and from the `cli/src/streams.rs` stamp roster with a comment), exactly as `chamber/branch-root/v1` was — never bumped, never reused.
- Every generated draw is counted into a `dof` field at the draw site, never inferred from loop shape.
- `cargo fmt` before every commit; `make gate-commit` runs as the pre-commit hook.
- Nothing this campaign builds is serialized (decision 0069). If Task 0's grep finds a committing reader of a level type, STOP and report — the campaign becomes an epoch and returns to G3.
- Preregistered readouts (spec §4) are reported in the frozen words `PASSED` / `FALSIFIED`; a falsified prediction is a finding. Never retune `cycle_budget` to rescue one.
- Stage boundaries: after Task 2, after Task 4, after Task 5 — run `make sluice-stage BRANCH=campaign/the-crosscut REF=<full-sha>` (the `submitting-to-the-sluice` skill) before continuing.

---

## File structure

| File | Responsibility |
|---|---|
| `windows/worldgen/src/circuit.rs` (new) | Grid geometry, plan types, growth grammar, attributes, metrics |
| `windows/worldgen/src/streams.rs` (modify) | Four new plan legs |
| `windows/worldgen/src/lib.rs` (modify) | `pub mod circuit;` and `pub mod circuit_readout;` |
| `windows/worldgen/src/circuit_readout.rs` (new) | `render_circuit_panel` — the audit page body |
| `windows/vessel/src/underworld_level/mod.rs` (modify) | Realizer: regions from plan, passages, stairs, terminus |
| `windows/vessel/src/underworld_level/region.rs` (shrink) | Keeps only `cut` (used by `carve.rs`) |
| `windows/vessel/src/streams.rs` (modify) | Remove `UNDERWORLD_LEVEL_PARTITION` |
| `windows/vessel/src/underground.rs` (modify) | Plan built in `enter`; `peek_stairs` pairs by coordinate |
| `windows/vessel/src/session.rs` (modify tests) | Stair tests' expectations; the cross-floor walk test |
| `windows/vessel/tests/suite/underworld_level_generation.rs` (modify) | Plan threaded into every `generate_*` call |
| `cli/src/main.rs` (modify) | `hornvale circuit --seed <N>` |
| `cli/src/streams.rs` (modify) | Stamp roster: four added, one gone |
| `scripts/regenerate-artifacts.sh`, `docs/generated-paths.txt` (modify) | The audit page |
| `docs/audits/underworld-circuit-seed-panel.md` (new, generated) | Committed readout |
| `docs/decisions/0566..0568-*.md`, `book/src/chronicle/the-crosscut.md`, `book/src/SUMMARY.md`, `book/src/frontier/idea-registry.md`, `docs/retrospectives/the-crosscut.md` | Close |

---

### Task 0: Baseline and premise check

**Files:**
- Modify: `windows/vessel/src/underworld_level/region.rs` (test module only)
- Modify: `docs/superpowers/ledgers/2026-09-01-the-crosscut.md` (append results)

**Interfaces:** none produced; this task measures before anything changes.

- [ ] **Step 1: Pin the tree claim with a test, before the tree is removed**

Add to `region.rs`'s `mod tests`:

```rust
    /// THE CROSSCUT, Task 0 — the campaign's premise, measured rather than
    /// read: a level's region graph is a tree. `connect_split_boundaries`
    /// carves exactly one passage per `Split`, so passages == leaves - 1
    /// and the cyclomatic number E - V + 1 is 0. Deleted with the tree in
    /// Task 3; its result is recorded in the campaign ledger.
    #[test]
    fn the_region_graph_is_a_tree_today() {
        fn splits(region: &Region) -> usize {
            match region {
                Region::Leaf(_) => 0,
                Region::Split(a, b) => 1 + splits(a) + splits(b),
            }
        }
        for seed_value in 0..200u64 {
            let (region, _dof) = build_region(EXTENT, Seed(seed_value));
            let v = leaves(&region).len();
            let e = splits(&region);
            assert_eq!(e + 1, v, "seed {seed_value}: passages must be leaves - 1");
            assert_eq!(e as i64 - v as i64 + 1, 0, "seed {seed_value}: cyclomatic number");
        }
    }
```

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-vessel the_region_graph_is_a_tree_today`
Expected: PASS. Branch table: PASS → the spec's §1 premise stands, proceed. FAIL → STOP; the premise is wrong, re-read `connect_split_boundaries` and report before any further task.

- [ ] **Step 3: The epoch grep (spec §5)**

Run, from the worktree root:

```bash
grep -rn "Level\b\|LevelCellKind\|CellGrid" --include=*.rs windows domains kernel cli \
  | grep -v "windows/vessel/src/underworld_level/\|windows/vessel/src/underground.rs\|/tests/\|#\[cfg(test)\]" \
  | grep -n "commit\|Fact\|ledger" 
```

Then read every hit by hand. Branch table: a hit that COMMITS a fact derived from a level cell or level shape → STOP, return to G3 with the reader named. Hits that only render, snapshot to the wire (`SessionLevel`, `vessel/level/v1`), or read for movement → proceed. Record the command and its output verbatim in the ledger under a new entry `#10 [G5] — Task 0 epoch grep`.

- [ ] **Step 4: Fixture and golden inventory**

Run:

```bash
grep -rln "vessel/level/v1\|\"underground\"" clients/game/core/tests/fixtures kernel/tests windows/*/tests 2>/dev/null
grep -rn "underground\|StairsDown" docs/generated-paths.txt book/src/gallery/*.md | head
```

Record what you find in the ledger entry. Branch table: no committed fixture embeds a level → nothing to rebaseline; a fixture does → list it in the ledger, and Task 5 rebaselines it with `REBASELINE=1` and a reviewed diff.

- [ ] **Step 5: Commit**

```bash
git add windows/vessel/src/underworld_level/region.rs docs/superpowers/ledgers/2026-09-01-the-crosscut.md
git commit -m "test(crosscut): pin the tree premise and record the Task 0 epoch grep"
```

---

### Task 1: Plan geometry, types and stream legs

**Files:**
- Create: `windows/worldgen/src/circuit.rs`
- Modify: `windows/worldgen/src/streams.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod circuit;` in alphabetical position after `pub mod chorus;`)

**Interfaces:**
- Produces (Task 2, 3, 4 and 5 rely on these exact names):
  - `pub const REGION_SPAN: i32 = 8;`
  - `pub fn level_extent_wh(rung: Band) -> (i32, i32)`
  - `pub struct GridDims { pub cols: u8, pub rows: u8 }` and `pub fn grid_dims(rung: Band) -> GridDims`
  - `pub struct GridCell { pub col: u8, pub row: u8 }` (derives `Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord`)
  - `pub struct RegionRect { pub x: i32, pub y: i32, pub w: i32, pub h: i32 }` with `pub fn intersect(&self, other: &RegionRect) -> Option<RegionRect>`
  - `pub fn region_rect(rung: Band, cell: GridCell) -> RegionRect`
  - `pub type NodeId = usize; pub type RealmId = usize;`
  - `pub struct Node { pub level: u8, pub cell: GridCell, pub depth: u16, pub realm: Option<RealmId> }`
  - `pub enum EdgeKind { Passage, Stair { x: i32, y: i32 } }`, `pub struct Edge { pub a: NodeId, pub b: NodeId, pub kind: EdgeKind }`
  - `pub enum LengthClass { LongLong, LongShort, ShortLong, ShortShort }`
  - `pub struct Realm { pub parent: Option<RealmId>, pub anchor_level: u8, pub path_a: Vec<NodeId>, pub path_b: Vec<NodeId>, pub class: LengthClass }`
  - `pub struct DescentPlan { pub rungs: Vec<Band>, pub nodes: Vec<Node>, pub edges: Vec<Edge>, pub entrance: NodeId, pub terminus: NodeId, pub realms: Vec<Realm>, pub dof: u32 }` with methods `nodes_on(level) -> Vec<NodeId>`, `passages_on(level) -> Vec<(NodeId, NodeId)>`, `stairs_from(level) -> Vec<(NodeId, NodeId, i32, i32)>` (upper node, lower node, x, y), `stairs_into(level) -> Vec<(NodeId, NodeId, i32, i32)>`, `region_of(node) -> RegionRect`, `neighbours(node) -> Vec<NodeId>`
  - Stream labels `UNDERWORLD_PLAN_SPINE`, `UNDERWORLD_PLAN_CYCLE`, `UNDERWORLD_PLAN_EXTEND`, `UNDERWORLD_PLAN_STAIR`.

- [ ] **Step 1: Declare the four legs**

In `windows/worldgen/src/streams.rs`, inside the `hornvale_kernel::stream_labels! { … }` block, after `BRANCH_BARRIER`'s entry, in the same flat form the file already uses:

```rust
    /// The Crosscut: the descent plan's spine — the entrance cell and each
    /// level's stair cell. Additive at v1; keyed further by vertex at the
    /// call, the way `BRANCH_CHARACTER` is.
    UNDERWORLD_PLAN_SPINE = "underworld/plan/v1/spine" => "entrance and per-level stair cells of a descent's spine";
    /// The Crosscut: which edge a cycle attaches to, how long its existing
    /// segment is, and whether it runs on this floor or the one below.
    UNDERWORLD_PLAN_CYCLE = "underworld/plan/v1/cycle" => "cycle attachment, segment length and same- or cross-floor choice";
    /// The Crosscut: which edge a series extension replaces.
    UNDERWORLD_PLAN_EXTEND = "underworld/plan/v1/extend" => "which passage a series extension lengthens";
    /// The Crosscut: which shared coordinate a stairway's two ends land on.
    UNDERWORLD_PLAN_STAIR = "underworld/plan/v1/stair" => "the coordinate a stairway shares between two floors";
```

- [ ] **Step 2: Write the failing geometry tests**

Create `windows/worldgen/src/circuit.rs` with the module doc and a `#[cfg(test)] mod tests` block first:

```rust
//! The descent plan (The Crosscut, The Circuit campaign 1): **a place is a
//! graph before it is a map, and the graph is series-parallel.**
//!
//! A [`DescentPlan`] is grown for the five-rung descent `Underground::enter`
//! (`windows/vessel/src/underground.rs`) walks under one vertex, before any
//! level is carved. Nodes are grid regions per level, edges are same-floor
//! passages or stairways whose two ends share one coordinate, and every
//! cycle is a [`Realm`] recorded with its two paths and Dormans' length
//! class. Growth uses exactly two operations — series (`extend`) and
//! parallel (`cycle`) — so planarity, reachability and one-innermost-realm
//! hold by construction (spec §3.4).
//!
//! FRAME-tier (decision 0069): derived from `(seed, vertex)` on every call,
//! never serialized. Density is DERIVED from rock and workmanship
//! ([`cycle_budget`]); the `[1, 5]` clip is the one authored constant and is
//! Dormans' (spec §3.2 step 5).

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Band;

    #[test]
    fn the_rank_zero_grid_is_five_by_three() {
        let d = grid_dims(Band::Undercroft);
        assert_eq!((d.cols, d.rows), (5, 3));
    }

    #[test]
    fn region_rects_tile_the_extent_with_one_cell_walls() {
        for &rung in hornvale_terrain::rungs().iter().filter(|r| **r != Band::Surface) {
            let (w, h) = level_extent_wh(rung);
            let d = grid_dims(rung);
            let mut claimed = std::collections::BTreeSet::new();
            for col in 0..d.cols {
                for row in 0..d.rows {
                    let r = region_rect(rung, GridCell { col, row });
                    assert!(r.w >= 1 && r.h >= 1, "{rung:?} ({col},{row}) degenerate {r:?}");
                    assert!(r.x >= 0 && r.y >= 0 && r.x + r.w < w && r.y + r.h < h,
                        "{rung:?} ({col},{row}) leaves no wall: {r:?} in {w}x{h}");
                    for x in r.x..r.x + r.w {
                        for y in r.y..r.y + r.h {
                            assert!(claimed.insert((x, y)), "{rung:?} cell ({x},{y}) claimed twice");
                        }
                    }
                }
            }
        }
    }

    /// Spec §3.3: a stairway's two ends share a coordinate, so the region
    /// above and the region below must overlap for EVERY grid cell of EVERY
    /// adjacent rung pair — asserted, not assumed.
    #[test]
    fn every_grid_cell_overlaps_its_twin_one_rung_down() {
        let rungs: Vec<Band> = hornvale_terrain::rungs().iter().copied()
            .filter(|r| *r != Band::Surface).collect();
        for pair in rungs.windows(2) {
            let (upper, lower) = (pair[0], pair[1]);
            let d = grid_dims(upper);
            let dl = grid_dims(lower);
            for col in 0..d.cols.min(dl.cols) {
                for row in 0..d.rows.min(dl.rows) {
                    let a = region_rect(upper, GridCell { col, row });
                    let b = region_rect(lower, GridCell { col, row });
                    assert!(a.intersect(&b).is_some(), "{upper:?}/{lower:?} ({col},{row}): {a:?} vs {b:?}");
                }
            }
        }
    }
}
```

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen circuit::tests`
Expected: compile error — `grid_dims`, `region_rect`, `level_extent_wh`, `GridCell`, `RegionRect` not defined.

- [ ] **Step 4: Implement geometry and types**

Above the test module in `circuit.rs`:

```rust
use std::collections::{BTreeMap, BTreeSet, VecDeque};

use hornvale_kernel::{Band, Seed, Stream, StreamLabel, Vertex};
use hornvale_terrain::CaveKind;

use crate::character::Character;

/// A region's span in cells, either axis — the Adit's `MIN_REGION_SPAN`,
/// now the grid pitch. The grid is `w / REGION_SPAN` by `h / REGION_SPAN`.
/// type-audit: bare-ok(count)
pub const REGION_SPAN: i32 = 8;
/// Dormans' clutter ceiling ("two to five cycles give each level a distinct
/// and recognizable shape"). The one authored constant in the grammar.
/// type-audit: bare-ok(count)
pub const MAX_CYCLES_PER_LEVEL: u8 = 5;
/// The floor of the same clip: every level gets at least one loop.
/// type-audit: bare-ok(count)
pub const MIN_CYCLES_PER_LEVEL: u8 = 1;

/// Level width before rank scaling (moved verbatim from
/// `windows/vessel/src/underworld_level/mod.rs`'s `BASE_LEVEL_W`).
const BASE_LEVEL_W: i32 = 40;
/// See `BASE_LEVEL_W`.
const BASE_LEVEL_H: i32 = 24;

/// The extent a level of `rung` gets, `(w, h)` in cells: deeper rungs get
/// more room. The formula `generate_level_extent` in `windows/vessel` used
/// to own; it now delegates here so the plan and the realizer agree.
/// type-audit: bare-ok(count: return)
pub fn level_extent_wh(rung: Band) -> (i32, i32) {
    let rank = hornvale_terrain::rungs()
        .iter()
        .position(|r| *r == rung)
        .unwrap_or(0) as i32;
    (BASE_LEVEL_W + 4 * rank, BASE_LEVEL_H + 2 * rank)
}

/// How many region columns and rows a level of a rung has.
/// type-audit: bare-ok(count: cols), bare-ok(count: rows)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GridDims {
    /// Region columns.
    pub cols: u8,
    /// Region rows.
    pub rows: u8,
}

/// The region grid of a rung: a DERIVED shape of the extent, never an
/// authored count (spec §3.1).
pub fn grid_dims(rung: Band) -> GridDims {
    let (w, h) = level_extent_wh(rung);
    GridDims {
        cols: (w / REGION_SPAN) as u8,
        rows: (h / REGION_SPAN) as u8,
    }
}

/// One region's position in a level's grid.
/// type-audit: bare-ok(index: col), bare-ok(index: row)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GridCell {
    /// Column, `0..cols`.
    pub col: u8,
    /// Row, `0..rows`.
    pub row: u8,
}

/// A region's carveable rectangle in level cells. `worldgen` cannot name
/// `vessel`'s `Rect`, so this is the plan's own; the realizer converts.
/// type-audit: bare-ok(count: x), bare-ok(count: y), bare-ok(count: w), bare-ok(count: h)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegionRect {
    /// Left edge, in cells.
    pub x: i32,
    /// Top edge, in cells.
    pub y: i32,
    /// Width in cells.
    pub w: i32,
    /// Height in cells.
    pub h: i32,
}

impl RegionRect {
    /// The overlap of two rectangles, or `None` if they do not overlap.
    pub fn intersect(&self, other: &RegionRect) -> Option<RegionRect> {
        let x0 = self.x.max(other.x);
        let y0 = self.y.max(other.y);
        let x1 = (self.x + self.w).min(other.x + other.w);
        let y1 = (self.y + self.h).min(other.y + other.h);
        (x1 > x0 && y1 > y0).then_some(RegionRect { x: x0, y: y0, w: x1 - x0, h: y1 - y0 })
    }
}

/// The rectangle a grid cell owns, minus its right and bottom cell, which
/// stay wall — the same one-cell dividing wall `region::cut` left between
/// siblings (spec §3.3). The extent's last column and row are therefore
/// always wall too.
pub fn region_rect(rung: Band, cell: GridCell) -> RegionRect {
    let (w, h) = level_extent_wh(rung);
    let d = grid_dims(rung);
    let x0 = cell.col as i32 * w / d.cols as i32;
    let x1 = (cell.col as i32 + 1) * w / d.cols as i32;
    let y0 = cell.row as i32 * h / d.rows as i32;
    let y1 = (cell.row as i32 + 1) * h / d.rows as i32;
    RegionRect { x: x0, y: y0, w: x1 - x0 - 1, h: y1 - y0 - 1 }
}

/// Index of a [`Node`] in [`DescentPlan::nodes`].
/// type-audit: bare-ok(index)
pub type NodeId = usize;
/// Index of a [`Realm`] in [`DescentPlan::realms`].
/// type-audit: bare-ok(index)
pub type RealmId = usize;

/// One region of one level.
/// type-audit: bare-ok(index: level), bare-ok(count: depth)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Node {
    /// Which level (index into [`DescentPlan::rungs`]).
    pub level: u8,
    /// Which grid cell of that level.
    pub cell: GridCell,
    /// Hops from the entrance along a shortest path — the gradient The Plat
    /// reads as intimacy and The Brattice as danger.
    pub depth: u16,
    /// The innermost cycle holding this node; `None` for a spine node on no
    /// cycle.
    pub realm: Option<RealmId>,
}

/// How two nodes connect.
/// type-audit: bare-ok(count: x), bare-ok(count: y)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EdgeKind {
    /// A same-level passage between grid-adjacent regions.
    Passage,
    /// A stairway between the same grid cell on adjacent levels; both ends
    /// stand on cell `(x, y)` of their level (spec §3.3).
    Stair {
        /// Shared column coordinate.
        x: i32,
        /// Shared row coordinate.
        y: i32,
    },
}

/// One connection. For a `Stair`, `a` is the upper node and `b` the lower.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Edge {
    /// One end.
    pub a: NodeId,
    /// The other end.
    pub b: NodeId,
    /// Passage or stairway.
    pub kind: EdgeKind,
}

/// Dormans' four cycle classes by relative path length (Fig. 9.8).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LengthClass {
    /// Both paths long and within one of each other.
    LongLong,
    /// The existing segment is the long way round.
    LongShort,
    /// The new path is the long way round.
    ShortLong,
    /// Both short and within one of each other.
    ShortShort,
}

/// A cycle: two node-disjoint paths between the same two nodes.
/// type-audit: bare-ok(index: anchor_level)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Realm {
    /// The realm whose path this one was cut from, if nested.
    pub parent: Option<RealmId>,
    /// The level of `path_a`; the level whose budget this realm counts to.
    pub anchor_level: u8,
    /// The pre-existing segment, endpoints included.
    pub path_a: Vec<NodeId>,
    /// The new path, endpoints included (shared with `path_a`).
    pub path_b: Vec<NodeId>,
    /// Dormans' class from the two lengths.
    pub class: LengthClass,
}

/// The plan for one descent: a series-parallel graph over grid regions.
/// type-audit: bare-ok(count: dof)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DescentPlan {
    /// The rungs, shallowest first; `level ℓ` is `rungs[ℓ]`.
    pub rungs: Vec<Band>,
    /// Every region.
    pub nodes: Vec<Node>,
    /// Every connection.
    pub edges: Vec<Edge>,
    /// Where the descent is entered (level 0, west edge).
    pub entrance: NodeId,
    /// The deepest level's terminus, carrying today's dangling stairs down.
    pub terminus: NodeId,
    /// Every cycle, in creation order (a nested realm follows its parent).
    pub realms: Vec<Realm>,
    /// Draws made while growing, counted at each draw.
    pub dof: u32,
}

impl DescentPlan {
    /// Every node on `level`, ascending id.
    /// type-audit: bare-ok(index: level)
    pub fn nodes_on(&self, level: usize) -> Vec<NodeId> {
        (0..self.nodes.len()).filter(|&i| self.nodes[i].level as usize == level).collect()
    }
    /// Every passage on `level`.
    /// type-audit: bare-ok(index: level)
    pub fn passages_on(&self, level: usize) -> Vec<(NodeId, NodeId)> {
        self.edges.iter()
            .filter(|e| e.kind == EdgeKind::Passage && self.nodes[e.a].level as usize == level)
            .map(|e| (e.a, e.b)).collect()
    }
    /// Every stairway whose UPPER end is on `level`: `(upper, lower, x, y)`.
    /// type-audit: bare-ok(index: level)
    pub fn stairs_from(&self, level: usize) -> Vec<(NodeId, NodeId, i32, i32)> {
        self.edges.iter().filter_map(|e| match e.kind {
            EdgeKind::Stair { x, y } if self.nodes[e.a].level as usize == level => Some((e.a, e.b, x, y)),
            _ => None,
        }).collect()
    }
    /// Every stairway whose LOWER end is on `level`: `(upper, lower, x, y)`.
    /// type-audit: bare-ok(index: level)
    pub fn stairs_into(&self, level: usize) -> Vec<(NodeId, NodeId, i32, i32)> {
        self.edges.iter().filter_map(|e| match e.kind {
            EdgeKind::Stair { x, y } if self.nodes[e.b].level as usize == level => Some((e.a, e.b, x, y)),
            _ => None,
        }).collect()
    }
    /// The carveable rectangle of `node`'s region.
    pub fn region_of(&self, node: NodeId) -> RegionRect {
        let n = self.nodes[node];
        region_rect(self.rungs[n.level as usize], n.cell)
    }
    /// Every node one edge away from `node`, ascending id.
    pub fn neighbours(&self, node: NodeId) -> Vec<NodeId> {
        let mut out: Vec<NodeId> = self.edges.iter().filter_map(|e| {
            if e.a == node { Some(e.b) } else if e.b == node { Some(e.a) } else { None }
        }).collect();
        out.sort_unstable();
        out.dedup();
        out
    }
}
```

Add `pub mod circuit;` to `windows/worldgen/src/lib.rs` after `pub mod chorus;`. The unused imports (`BTreeMap`, `BTreeSet`, `VecDeque`, `Seed`, `Stream`, `StreamLabel`, `Vertex`, `CaveKind`, `Character`) will warn until Task 2; add `#[allow(unused_imports)]` on the `use` lines with a comment `// Task 2 consumes these` and REMOVE the allow in Task 2.

- [ ] **Step 5: Run the tests**

Run: `cargo test -p hornvale-worldgen circuit::tests`
Expected: 3 passed.

- [ ] **Step 6: Lint and commit**

Run: `cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: clean. If type-audit names an untagged field, add the tag it names.

```bash
git add windows/worldgen/src/circuit.rs windows/worldgen/src/streams.rs windows/worldgen/src/lib.rs
git commit -m "feat(circuit): plan geometry, types and the four plan stream legs"
```

---

### Task 2: Growth — spine, cycle, extend, budget, attributes

**Files:**
- Modify: `windows/worldgen/src/circuit.rs`

**Interfaces:**
- Consumes: everything Task 1 produced.
- Produces:
  - `pub fn cycle_budget(kind: CaveKind, character: Character) -> u8`
  - `pub fn plan_descent(seed: Seed, vertex: Vertex, rungs: &[Band], kind: CaveKind, character: Character) -> DescentPlan`
  - `pub fn anchored_realms(plan: &DescentPlan, level: usize) -> usize`
  - `pub fn loop_share(plan: &DescentPlan) -> f64`
  - `pub fn has_cross_floor_realm(plan: &DescentPlan) -> bool`
  - `pub fn semilattice_overlap(plan: &DescentPlan) -> Option<f64>`
  - `pub fn length_class(len_a: usize, len_b: usize) -> LengthClass`

- [ ] **Step 1: Write the failing tests**

Append to `circuit::tests`:

```rust
    fn habitation_rungs() -> Vec<Band> {
        hornvale_terrain::rungs().iter().copied().filter(|r| *r != Band::Surface).collect()
    }
    fn plan(seed: u64, vertex: u32) -> DescentPlan {
        plan_descent(Seed(seed), Vertex(vertex), &habitation_rungs(), CaveKind::Karst, Character::WildCave)
    }

    /// Spec §3.4 (1): every edge joins grid-adjacent cells on one level or
    /// the same cell on adjacent levels — planar and embedded by construction.
    #[test]
    fn every_edge_is_grid_adjacent_or_a_vertical_stair() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            for e in &p.edges {
                let (a, b) = (p.nodes[e.a], p.nodes[e.b]);
                match e.kind {
                    EdgeKind::Passage => {
                        assert_eq!(a.level, b.level, "seed {s}: passage crosses levels");
                        let dc = (a.cell.col as i32 - b.cell.col as i32).abs();
                        let dr = (a.cell.row as i32 - b.cell.row as i32).abs();
                        assert_eq!(dc + dr, 1, "seed {s}: passage {e:?} not grid-adjacent");
                    }
                    EdgeKind::Stair { x, y } => {
                        assert_eq!(a.level + 1, b.level, "seed {s}: stair {e:?} not one rung down");
                        assert_eq!(a.cell, b.cell, "seed {s}: stair {e:?} changes grid cell");
                        let ra = p.region_of(e.a);
                        let rb = p.region_of(e.b);
                        for r in [ra, rb] {
                            assert!(x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h,
                                "seed {s}: stair coordinate ({x},{y}) outside {r:?}");
                        }
                    }
                }
            }
        }
    }

    /// Spec §3.4 (2): every node reachable from the entrance.
    #[test]
    fn every_node_is_reachable_from_the_entrance() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            let mut seen = BTreeSet::new();
            let mut q = VecDeque::from([p.entrance]);
            seen.insert(p.entrance);
            while let Some(n) = q.pop_front() {
                for m in p.neighbours(n) {
                    if seen.insert(m) { q.push_back(m); }
                }
            }
            assert_eq!(seen.len(), p.nodes.len(), "seed {s}: unreachable regions");
        }
    }

    /// Spec §3.4 (3) and §3.2 step 5: every realm adds exactly one to the
    /// Kirchhoff mesh count, and every level sits inside the clip.
    #[test]
    fn realms_are_the_mesh_count_and_every_level_is_inside_the_clip() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            let cyclomatic = p.edges.len() as i64 - p.nodes.len() as i64 + 1;
            assert_eq!(cyclomatic, p.realms.len() as i64, "seed {s}: E-V+1 != realms");
            for level in 0..p.rungs.len() {
                let n = anchored_realms(&p, level);
                assert!(n >= MIN_CYCLES_PER_LEVEL as usize && n <= MAX_CYCLES_PER_LEVEL as usize,
                    "seed {s} level {level}: {n} realms");
            }
        }
    }

    /// Every node on a cycle names the innermost realm containing it; every
    /// nested realm's parent contains both its endpoints.
    #[test]
    fn realm_ownership_is_innermost_and_parents_contain_their_children() {
        for s in 0..100u64 {
            let p = plan(s, 7);
            for (rid, r) in p.realms.iter().enumerate() {
                if let Some(parent) = r.parent {
                    assert!(parent < rid, "seed {s}: parent after child");
                    let pr = &p.realms[parent];
                    let members: BTreeSet<NodeId> = pr.path_a.iter().chain(pr.path_b.iter()).copied().collect();
                    assert!(members.contains(&r.path_a[0]) && members.contains(r.path_a.last().unwrap()),
                        "seed {s}: realm {rid}'s endpoints not in parent {parent}");
                }
                assert_eq!(r.path_a.first(), r.path_b.first(), "seed {s}: paths start apart");
                assert_eq!(r.path_a.last(), r.path_b.last(), "seed {s}: paths end apart");
                let inner_a: BTreeSet<_> = r.path_a[1..r.path_a.len() - 1].iter().collect();
                let inner_b: BTreeSet<_> = r.path_b[1..r.path_b.len() - 1].iter().collect();
                assert!(inner_a.is_disjoint(&inner_b), "seed {s}: realm {rid} paths share an interior node");
            }
            for (nid, n) in p.nodes.iter().enumerate() {
                let holding: Vec<RealmId> = p.realms.iter().enumerate()
                    .filter(|(_, r)| r.path_a.contains(&nid) || r.path_b.contains(&nid))
                    .map(|(i, _)| i).collect();
                assert_eq!(n.realm, holding.last().copied(), "seed {s}: node {nid} realm");
            }
        }
    }

    #[test]
    fn the_plan_is_deterministic_and_reads_the_vertex() {
        assert_eq!(plan(42, 7), plan(42, 7));
        assert_ne!(plan(42, 7), plan(42, 8), "two caves in one world must not share a plan");
        assert_ne!(plan(42, 7), plan(43, 7));
    }

    #[test]
    fn dof_counts_every_draw() {
        let p = plan(42, 7);
        assert!(p.dof > 0);
        // Recount independently: the spine draws one entrance row and one
        // stair cell per level, and every realm and extension is at least one
        // draw, so dof is at least that floor.
        let floor = 1 + p.rungs.len() as u32 + p.realms.len() as u32;
        assert!(p.dof >= floor, "dof {} below its floor {floor}", p.dof);
    }

    #[test]
    fn budget_is_derived_from_rock_and_workmanship() {
        assert!(cycle_budget(CaveKind::LavaTube, Character::WildCave) < cycle_budget(CaveKind::Fracture, Character::WildCave));
        assert!(cycle_budget(CaveKind::Fracture, Character::WildCave) < cycle_budget(CaveKind::Karst, Character::WildCave));
        assert!(cycle_budget(CaveKind::Karst, Character::DrowTier) > cycle_budget(CaveKind::Karst, Character::WildCave));
        assert!(cycle_budget(CaveKind::Karst, Character::DrowTier) <= MAX_CYCLES_PER_LEVEL);
        assert!(cycle_budget(CaveKind::LavaTube, Character::WildCave) >= MIN_CYCLES_PER_LEVEL);
    }

    #[test]
    fn length_class_follows_the_frozen_rule() {
        assert_eq!(length_class(1, 3), LengthClass::ShortLong);
        assert_eq!(length_class(3, 1), LengthClass::LongShort);
        assert_eq!(length_class(3, 4), LengthClass::LongLong);
        assert_eq!(length_class(1, 2), LengthClass::ShortShort);
        assert_eq!(length_class(2, 2), LengthClass::ShortShort);
    }

    /// Spec §4.3's move must be REACHABLE for the readout to mean anything:
    /// somewhere in 100 seeds a realm spans two floors.
    #[test]
    fn some_seed_produces_a_cross_floor_realm() {
        assert!((0..100u64).any(|s| has_cross_floor_realm(&plan(s, 7))));
    }

    #[test]
    fn a_tree_has_zero_loop_share_and_a_cycle_has_full() {
        let mut p = plan(1, 1);
        // Remove every realm's path_b interior to leave the spine: loop share 0.
        let spine_only: BTreeSet<NodeId> = {
            let mut cut = p.clone();
            let interiors: BTreeSet<NodeId> = cut.realms.iter()
                .flat_map(|r| r.path_b[1..r.path_b.len() - 1].iter().copied()).collect();
            cut.edges.retain(|e| !interiors.contains(&e.a) && !interiors.contains(&e.b)
                && !cut.realms.iter().any(|r| r.path_b.len() == 2 && ((r.path_b[0] == e.a && r.path_b[1] == e.b) || (r.path_b[0] == e.b && r.path_b[1] == e.a))));
            interiors
        };
        p.edges.retain(|e| !spine_only.contains(&e.a) && !spine_only.contains(&e.b));
        p.nodes.iter_mut().for_each(|_| {});
        // The pruned plan still names the removed nodes; loop_share must
        // ignore nodes unreachable from the entrance rather than count them.
        assert_eq!(loop_share(&p), 0.0);
        assert!(loop_share(&plan(1, 1)) > 0.0);
    }
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-worldgen circuit::tests`
Expected: compile error — `plan_descent`, `cycle_budget`, `anchored_realms`, `loop_share`, `has_cross_floor_realm`, `length_class` not defined.

- [ ] **Step 3: Implement growth**

Add to `circuit.rs` (remove the `#[allow(unused_imports)]` from Task 1):

```rust
/// Target cycles per level: DERIVED from the rock (karst dissolves many
/// routes, a lava tube is one conduit, a fracture system sits between) and
/// from workmanship (a worked place must ventilate, so it loops), clipped
/// to Dormans' `[MIN_CYCLES_PER_LEVEL, MAX_CYCLES_PER_LEVEL]`. `ChamberOrigin::Made`
/// joins the `worked` term when The Plat gives it a production writer.
/// type-audit: bare-ok(count: return)
pub fn cycle_budget(kind: CaveKind, character: Character) -> u8 {
    let base: u8 = match kind {
        CaveKind::LavaTube => 1,
        CaveKind::Fracture => 2,
        CaveKind::Karst => 3,
    };
    let worked: u8 = match character {
        Character::DrowTier => 1,
        Character::WildCave | Character::FungalGardens => 0,
    };
    (base + worked).clamp(MIN_CYCLES_PER_LEVEL, MAX_CYCLES_PER_LEVEL)
}

/// Dormans' class from two path lengths (edge counts). "Long" is RELATIVE:
/// a path is the long way round when it is strictly longer than the other
/// plus one. Within one of each other, both count long at three or more
/// edges and short below. Frozen here (spec §3.2 step 6) and exported.
/// type-audit: bare-ok(count: len_a), bare-ok(count: len_b)
pub fn length_class(len_a: usize, len_b: usize) -> LengthClass {
    if len_a > len_b + 1 {
        LengthClass::LongShort
    } else if len_b > len_a + 1 {
        LengthClass::ShortLong
    } else if len_a >= 3 && len_b >= 3 {
        LengthClass::LongLong
    } else {
        LengthClass::ShortShort
    }
}

/// The decimal key every plan leg is further derived by: the vertex.
fn vertex_key(vertex: Vertex) -> String {
    format!("{}", vertex.0)
}

fn leg(seed: Seed, label: StreamLabel<'static>, vertex: Vertex) -> Stream {
    seed.derive(label).derive(StreamLabel::dynamic(&vertex_key(vertex))).stream()
}

/// Draw an index in `0..n` from `stream`, counting the draw. `n >= 1`.
fn draw_index(stream: &mut Stream, n: usize, dof: &mut u32) -> usize {
    *dof += 1;
    (stream.next_u64() % n as u64) as usize
}

/// The growing plan and the per-level occupancy the grammar needs.
struct Builder {
    plan: DescentPlan,
    dims: Vec<GridDims>,
    /// Which grid cells each level has already spent.
    used: Vec<BTreeSet<GridCell>>,
    /// `(level, cell) -> node`.
    index: BTreeMap<(u8, GridCell), NodeId>,
}

impl Builder {
    fn node_at(&self, level: u8, cell: GridCell) -> Option<NodeId> {
        self.index.get(&(level, cell)).copied()
    }

    fn add_node(&mut self, level: u8, cell: GridCell) -> NodeId {
        let id = self.plan.nodes.len();
        self.plan.nodes.push(Node { level, cell, depth: 0, realm: None });
        self.used[level as usize].insert(cell);
        self.index.insert((level, cell), id);
        id
    }

    fn add_passage(&mut self, a: NodeId, b: NodeId) {
        let (a, b) = (a.min(b), a.max(b));
        self.plan.edges.push(Edge { a, b, kind: EdgeKind::Passage });
    }

    fn remove_passage(&mut self, a: NodeId, b: NodeId) {
        let (a, b) = (a.min(b), a.max(b));
        self.plan.edges.retain(|e| !(e.kind == EdgeKind::Passage && e.a == a && e.b == b));
    }

    /// Grid-adjacent cells of `cell` on a level of `dims`, N, E, S, W.
    fn grid_neighbours(dims: GridDims, cell: GridCell) -> Vec<GridCell> {
        let mut out = Vec::with_capacity(4);
        if cell.row > 0 { out.push(GridCell { col: cell.col, row: cell.row - 1 }); }
        if cell.col + 1 < dims.cols { out.push(GridCell { col: cell.col + 1, row: cell.row }); }
        if cell.row + 1 < dims.rows { out.push(GridCell { col: cell.col, row: cell.row + 1 }); }
        if cell.col > 0 { out.push(GridCell { col: cell.col - 1, row: cell.row }); }
        out
    }

    /// The INTERIOR cells of a shortest path from `from` to `to` on `level`
    /// through cells not yet used, by breadth-first search with a fixed
    /// neighbour order — deterministic, no draw. `from` and `to` may be used
    /// (they are the endpoints); every interior cell must be free. `None`
    /// when no such path exists. A path with no interior (adjacent
    /// endpoints) is refused when `min_interior` is 1 or more.
    fn free_path(&self, level: u8, from: GridCell, to: GridCell, min_interior: usize) -> Option<Vec<GridCell>> {
        let dims = self.dims[level as usize];
        let used = &self.used[level as usize];
        let mut prev: BTreeMap<GridCell, GridCell> = BTreeMap::new();
        let mut q = VecDeque::new();
        for n in Self::grid_neighbours(dims, from) {
            if n == to {
                if min_interior == 0 { return Some(Vec::new()); }
                continue;
            }
            if !used.contains(&n) && !prev.contains_key(&n) {
                prev.insert(n, from);
                q.push_back(n);
            }
        }
        while let Some(c) = q.pop_front() {
            for n in Self::grid_neighbours(dims, c) {
                if n == to {
                    let mut path = vec![c];
                    let mut cur = c;
                    while let Some(&p) = prev.get(&cur) {
                        if p == from { break; }
                        path.push(p);
                        cur = p;
                    }
                    path.reverse();
                    if path.len() >= min_interior { return Some(path); }
                    continue;
                }
                if !used.contains(&n) && !prev.contains_key(&n) {
                    prev.insert(n, c);
                    q.push_back(n);
                }
            }
        }
        None
    }

    /// A stairway's shared coordinate, drawn inside the overlap of the two
    /// regions (asserted non-empty by `every_grid_cell_overlaps_its_twin_one_rung_down`).
    fn stair_coordinate(&self, upper: NodeId, lower: NodeId, stair: &mut Stream, dof: &mut u32) -> (i32, i32) {
        let overlap = self.plan.region_of(upper).intersect(&self.plan.region_of(lower))
            .expect("adjacent rungs' twin regions overlap (Task 1 test)");
        let x = overlap.x + draw_index(stair, overlap.w as usize, dof) as i32;
        let y = overlap.y + draw_index(stair, overlap.h as usize, dof) as i32;
        (x, y)
    }

    fn add_stair(&mut self, upper: NodeId, lower: NodeId, stair: &mut Stream, dof: &mut u32) {
        let (x, y) = self.stair_coordinate(upper, lower, stair, dof);
        self.plan.edges.push(Edge { a: upper, b: lower, kind: EdgeKind::Stair { x, y } });
    }

    /// Lay a passage chain through `interior` from `from` to `to`, creating
    /// nodes for the interior cells. Returns the node path, endpoints included.
    fn lay_path(&mut self, level: u8, from: NodeId, interior: &[GridCell], to: NodeId) -> Vec<NodeId> {
        let mut path = vec![from];
        let mut prev = from;
        for &c in interior {
            let n = self.add_node(level, c);
            self.add_passage(prev, n);
            path.push(n);
            prev = n;
        }
        self.add_passage(prev, to);
        path.push(to);
        path
    }

    fn degree(&self, n: NodeId) -> usize {
        self.plan.neighbours(n).len()
    }

    /// From `v`, walk away from `u` through degree-two nodes along passages
    /// for up to `hops`, returning the segment `[u, v, ...]`.
    fn segment(&self, u: NodeId, v: NodeId, hops: usize) -> Vec<NodeId> {
        let mut seg = vec![u, v];
        for _ in 0..hops {
            let last = *seg.last().unwrap();
            let before = seg[seg.len() - 2];
            if self.degree(last) != 2 { break; }
            let next = self.plan.neighbours(last).into_iter().find(|&n| n != before);
            match next {
                Some(n) if self.plan.edges.iter().any(|e| e.kind == EdgeKind::Passage
                    && ((e.a == last && e.b == n) || (e.a == n && e.b == last))) => seg.push(n),
                _ => break,
            }
        }
        seg
    }

    /// The innermost realm containing both `u` and `v`, if any.
    fn realm_containing(&self, u: NodeId, v: NodeId) -> Option<RealmId> {
        self.plan.realms.iter().enumerate().rev()
            .find(|(_, r)| { let m = |n| r.path_a.contains(&n) || r.path_b.contains(&n); m(u) && m(v) })
            .map(|(i, _)| i)
    }
}

/// Grow the plan for one descent (spec §3.2). `rungs` is the walked list,
/// shallowest first (`hornvale_terrain::rungs()` minus `Surface`).
pub fn plan_descent(seed: Seed, vertex: Vertex, rungs: &[Band], kind: CaveKind, character: Character) -> DescentPlan {
    assert!(!rungs.is_empty(), "a descent has at least one rung");
    let mut spine = leg(seed, crate::streams::UNDERWORLD_PLAN_SPINE, vertex);
    let mut cycle = leg(seed, crate::streams::UNDERWORLD_PLAN_CYCLE, vertex);
    let mut extend = leg(seed, crate::streams::UNDERWORLD_PLAN_EXTEND, vertex);
    let mut stair = leg(seed, crate::streams::UNDERWORLD_PLAN_STAIR, vertex);
    let mut dof = 0u32;
    let dims: Vec<GridDims> = rungs.iter().map(|&r| grid_dims(r)).collect();
    let mut b = Builder {
        plan: DescentPlan { rungs: rungs.to_vec(), nodes: Vec::new(), edges: Vec::new(),
            entrance: 0, terminus: 0, realms: Vec::new(), dof: 0 },
        used: vec![BTreeSet::new(); rungs.len()],
        index: BTreeMap::new(),
        dims,
    };

    // 1. The spine.
    let row0 = draw_index(&mut spine, b.dims[0].rows as usize, &mut dof) as u8;
    let mut arrival = b.add_node(0, GridCell { col: 0, row: row0 });
    b.plan.entrance = arrival;
    for level in 0..rungs.len() as u8 {
        let d = b.dims[level as usize];
        let all: Vec<GridCell> = (0..d.cols).flat_map(|col| (0..d.rows).map(move |row| GridCell { col, row }))
            .filter(|c| !b.used[level as usize].contains(c)).collect();
        let target_cell = all[draw_index(&mut spine, all.len(), &mut dof)];
        let interior = b.free_path(level, b.plan.nodes[arrival].cell, target_cell, 0)
            .expect("an otherwise empty level always has a free path");
        let target = b.add_node(level, target_cell);
        b.lay_path(level, arrival, &interior, target);
        if (level as usize) + 1 < rungs.len() {
            let below = b.add_node(level + 1, target_cell);
            b.add_stair(target, below, &mut stair, &mut dof);
            arrival = below;
        } else {
            b.plan.terminus = target;
        }
    }

    // 2. Cycles and extensions, level by level, to a derived budget.
    let budget = cycle_budget(kind, character) as usize;
    for level in 0..rungs.len() as u8 {
        let mut attempts = 0;
        while anchored_realms(&b.plan, level as usize) < budget && attempts < 40 {
            attempts += 1;
            let passages = b.plan.passages_on(level as usize);
            if passages.is_empty() { break; }
            let (u, v) = passages[draw_index(&mut cycle, passages.len(), &mut dof)];
            let op = draw_index(&mut cycle, 10, &mut dof);
            if op < 7 {
                try_cycle(&mut b, level, u, v, &mut cycle, &mut stair, &mut dof);
            } else {
                try_extend(&mut b, level, u, v, &mut extend, &mut dof);
            }
        }
    }

    // 3. Attributes.
    assign_realms(&mut b.plan);
    assign_depth(&mut b.plan);
    b.plan.dof = dof;
    b.plan
}

fn try_cycle(b: &mut Builder, level: u8, u: NodeId, v: NodeId, cycle: &mut Stream, stair: &mut Stream, dof: &mut u32) {
    let hops = draw_index(cycle, 3, dof);
    let path_a = b.segment(u, v, hops);
    let end = *path_a.last().unwrap();
    let (cu, ce) = (b.plan.nodes[u].cell, b.plan.nodes[end].cell);
    let cross = draw_index(cycle, 100, dof) < 35
        && (level as usize) + 1 < b.plan.rungs.len()
        && cu != ce
        && !b.used[level as usize + 1].contains(&cu)
        && !b.used[level as usize + 1].contains(&ce);
    let parent = b.realm_containing(u, end);
    if cross {
        let Some(interior) = b.free_path(level + 1, cu, ce, 0) else { return };
        let lu = b.add_node(level + 1, cu);
        let le = b.add_node(level + 1, ce);
        b.add_stair(u, lu, stair, dof);
        let mut path_b = vec![u];
        path_b.extend(b.lay_path(level + 1, lu, &interior, le));
        b.add_stair(end, le, stair, dof);
        path_b.push(end);
        let class = length_class(path_a.len() - 1, path_b.len() - 1);
        b.plan.realms.push(Realm { parent, anchor_level: level, path_a, path_b, class });
    } else {
        let Some(interior) = b.free_path(level, cu, ce, 1) else { return };
        let path_b = b.lay_path(level, u, &interior, end);
        let class = length_class(path_a.len() - 1, path_b.len() - 1);
        b.plan.realms.push(Realm { parent, anchor_level: level, path_a, path_b, class });
    }
}

fn try_extend(b: &mut Builder, level: u8, u: NodeId, v: NodeId, extend: &mut Stream, dof: &mut u32) {
    *dof += 1;
    let _ = extend.next_u64(); // the choice of WHICH edge was drawn by the caller; this draw is the leg's witness
    let (cu, cv) = (b.plan.nodes[u].cell, b.plan.nodes[v].cell);
    let Some(interior) = b.free_path(level, cu, cv, 1) else { return };
    // Every realm path that ran through u-v now runs through the detour.
    b.remove_passage(u, v);
    let path = b.lay_path(level, u, &interior, v);
    for r in &mut b.plan.realms {
        for p in [&mut r.path_a, &mut r.path_b] {
            if let Some(i) = p.windows(2).position(|w| (w[0] == u && w[1] == v) || (w[0] == v && w[1] == u)) {
                let forward = p[i] == u;
                let mut mids: Vec<NodeId> = path[1..path.len() - 1].to_vec();
                if !forward { mids.reverse(); }
                p.splice(i + 1..i + 1, mids);
            }
        }
    }
}

/// `Node.realm` = the LAST realm (creation order) whose paths hold the node:
/// a nested realm is created after its parent, so last is innermost.
fn assign_realms(plan: &mut DescentPlan) {
    for n in &mut plan.nodes { n.realm = None; }
    for (rid, r) in plan.realms.iter().enumerate() {
        for &n in r.path_a.iter().chain(r.path_b.iter()) {
            plan.nodes[n].realm = Some(rid);
        }
    }
}

/// Breadth-first hops from the entrance.
fn assign_depth(plan: &mut DescentPlan) {
    let mut dist: BTreeMap<NodeId, u16> = BTreeMap::new();
    let mut q = VecDeque::from([plan.entrance]);
    dist.insert(plan.entrance, 0);
    while let Some(n) = q.pop_front() {
        let d = dist[&n];
        for m in plan.neighbours(n) {
            if !dist.contains_key(&m) { dist.insert(m, d + 1); q.push_back(m); }
        }
    }
    for (i, n) in plan.nodes.iter_mut().enumerate() {
        n.depth = dist.get(&i).copied().unwrap_or(u16::MAX);
    }
}

/// Realms whose `path_a` sits on `level` — the level's own density (spec §4.2).
/// type-audit: bare-ok(index: level), bare-ok(count: return)
pub fn anchored_realms(plan: &DescentPlan, level: usize) -> usize {
    plan.realms.iter().filter(|r| r.anchor_level as usize == level).count()
}

/// Spec §4.1: the share of non-entrance regions reachable from the entrance
/// that stay reachable under the removal of ANY single edge — two
/// edge-disjoint routes home (Menger). Regions unreachable from the entrance
/// are excluded from both numerator and denominator.
/// type-audit: bare-ok(ratio: return)
pub fn loop_share(plan: &DescentPlan) -> f64 {
    fn reach(plan: &DescentPlan, skip: Option<usize>) -> BTreeSet<NodeId> {
        let mut seen = BTreeSet::from([plan.entrance]);
        let mut q = VecDeque::from([plan.entrance]);
        while let Some(n) = q.pop_front() {
            for (i, e) in plan.edges.iter().enumerate() {
                if Some(i) == skip { continue; }
                let m = if e.a == n { e.b } else if e.b == n { e.a } else { continue };
                if seen.insert(m) { q.push_back(m); }
            }
        }
        seen
    }
    let base = reach(plan, None);
    let candidates: Vec<NodeId> = base.iter().copied().filter(|&n| n != plan.entrance).collect();
    if candidates.is_empty() { return 0.0; }
    let mut robust: BTreeSet<NodeId> = candidates.iter().copied().collect();
    for i in 0..plan.edges.len() {
        let r = reach(plan, Some(i));
        robust.retain(|n| r.contains(n));
    }
    robust.len() as f64 / candidates.len() as f64
}

/// Spec §4.3: does any realm's `path_b` touch a level other than its anchor?
/// type-audit: bare-ok(flag: return)
pub fn has_cross_floor_realm(plan: &DescentPlan) -> bool {
    plan.realms.iter().any(|r| r.path_b.iter().any(|&n| plan.nodes[n].level != r.anchor_level))
}

/// Spec §4.4: among nodes on at least one realm, the share on two or more.
/// `None` when no node is on any realm.
/// type-audit: bare-ok(ratio: return)
pub fn semilattice_overlap(plan: &DescentPlan) -> Option<f64> {
    let mut count: BTreeMap<NodeId, usize> = BTreeMap::new();
    for r in &plan.realms {
        let members: BTreeSet<NodeId> = r.path_a.iter().chain(r.path_b.iter()).copied().collect();
        for n in members { *count.entry(n).or_insert(0) += 1; }
    }
    if count.is_empty() { return None; }
    let on_two = count.values().filter(|&&c| c >= 2).count();
    Some(on_two as f64 / count.len() as f64)
}
```

Note on `try_extend`: the removed edge `u–v` may already be part of a realm path; the splice keeps every recorded path a real walk through the graph. If the `windows(2)` search finds the pair in BOTH paths of one realm (impossible for node-disjoint interiors, but assert it in debug): add `debug_assert!` that at most one path per realm changed.

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-worldgen circuit::tests`
Expected: all pass. If `realms_are_the_mesh_count_and_every_level_is_inside_the_clip` fails on the LOWER bound for some seed, that is a real finding about the grid, not a tuning invitation: record the seed and level in the ledger, then raise `attempts` from 40 to 80 ONCE and re-run; if it still fails, stop and report — the spec's "at least one" guard is falsified for that grid size.

The `a_tree_has_zero_loop_share_and_a_cycle_has_full` test is deliberately clumsy about pruning; if it proves brittle, replace its first half with a hand-built three-node path plan (`nodes` on level 0 at cells (0,0),(1,0),(2,0), two passages, `entrance = 0`) and assert `loop_share == 0.0` on it, keeping the second assertion.

- [ ] **Step 5: Lint, then commit**

Run: `cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

```bash
git add windows/worldgen/src/circuit.rs
git commit -m "feat(circuit): grow the series-parallel descent plan — spine, cycle, extend, attributes, metrics"
```

- [ ] **Step 6: Stage gate**

Push the branch and run `make sluice-stage BRANCH=campaign/the-crosscut REF=$(git rev-parse HEAD)` per the `submitting-to-the-sluice` skill. Nothing world-visible has changed yet; this stage proves the plan compiles against main and its tests hold there.

---

### Task 3: Realize the plan into levels

**Files:**
- Modify: `windows/vessel/src/underworld_level/mod.rs`
- Modify: `windows/vessel/src/underworld_level/region.rs` (delete the tree; keep `cut`)
- Modify: `windows/vessel/src/streams.rs` (remove `UNDERWORLD_LEVEL_PARTITION`)
- Modify: `windows/vessel/tests/suite/underworld_level_generation.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::circuit::{DescentPlan, plan_descent, level_extent_wh}`.
- Produces (Task 4 relies on these exact signatures):
  - `pub fn generate_level_extent(rung: Band) -> Rect` (unchanged signature; now delegates to `level_extent_wh`)
  - `pub fn generate_level_with_origin(extent: Rect, cave_kind: CaveKind, origin: ChamberOrigin, character: Character, inherited_worked_bias: f64, plan: &DescentPlan, level: usize, seed: Seed) -> Level`
  - `pub fn generate_level_with_water(extent, cave_kind, origin, character, depth_m, water_table_m, inherited_worked_bias, plan: &DescentPlan, level: usize, seed) -> Level`
  - `pub fn generate_descent(rungs, cave_kind, origins, depths_m, water_table_m, plan: &DescentPlan, seed) -> Vec<Level>`
  - `pub fn generate_descent_for_character(rungs, cave_kind, origins, depths_m, water_table_m, character, plan: &DescentPlan, seed) -> Vec<Level>`
  - `pub fn generate_level(extent: Rect, seed: Seed) -> Level` stays as the single-level test helper and builds its own one-rung plan for `Band::Undercroft` at `Vertex(0)`; its `extent` MUST equal `generate_level_extent(Band::Undercroft)` (debug-asserted).
  - `Level.leaf_styles[i]` is the style of `plan.nodes_on(level)[i]`'s region (replaces the leaf alignment).

- [ ] **Step 1: Write the failing pairing test (replacing two old tests)**

In `underworld_level/mod.rs`'s test module, DELETE `every_level_but_the_first_has_stairs_up_every_level_has_stairs_down` and `stairs_down_and_stairs_up_never_share_a_cell`, and add:

```rust
    fn two_rung_plan(seed: u64) -> hornvale_worldgen::circuit::DescentPlan {
        hornvale_worldgen::circuit::plan_descent(
            Seed(seed),
            hornvale_kernel::Vertex(0),
            &[hornvale_kernel::Band::Undercroft, hornvale_kernel::Band::Shallows],
            hornvale_terrain::CaveKind::Fracture,
            Character::WildCave,
        )
    }

    /// claim: invariant(seed: 0..200) — THE CROSSCUT's stairs contract (spec
    /// §3.3, decision 0567): every `StairsDown` on rung `i` (below the last)
    /// has a `StairsUp` at the SAME coordinate on rung `i+1` and vice versa;
    /// rung 0 has no `StairsUp`; the last rung has exactly one `StairsDown`,
    /// the dangling terminus; no cell carries both kinds. Replaces
    /// `stairs_down_and_stairs_up_never_share_a_cell`, whose "exactly one"
    /// the plan deliberately breaks.
    #[test]
    fn stairs_pair_by_coordinate_across_adjacent_rungs() {
        use hornvale_kernel::Band;
        use hornvale_terrain::CaveKind;
        use hornvale_worldgen::chamber::ChamberOrigin;
        let rungs = [Band::Undercroft, Band::Shallows];
        let origins = [ChamberOrigin::Found, ChamberOrigin::Found];
        let depths_m = [20.0, 60.0];
        let mut paired_stairs_seen = 0usize;
        for s in 0..200u64 {
            let plan = two_rung_plan(s);
            let levels = generate_descent(&rungs, CaveKind::Fracture, &origins, &depths_m, 500.0, &plan, Seed(s));
            let cells_of = |l: &Level, k: LevelCellKind| -> Vec<Cell> {
                l.cells.iter().filter(|(_, kk)| *kk == k).map(|(c, _)| c).collect()
            };
            assert!(cells_of(&levels[0], LevelCellKind::StairsUp).is_empty(), "seed {s}: rung 0 has stairs up");
            let downs0 = cells_of(&levels[0], LevelCellKind::StairsDown);
            let ups1 = cells_of(&levels[1], LevelCellKind::StairsUp);
            assert_eq!(downs0, ups1, "seed {s}: stairs down on rung 0 must equal stairs up on rung 1, by coordinate");
            assert!(!downs0.is_empty(), "seed {s}: no stairway at all");
            paired_stairs_seen += downs0.len();
            let downs1 = cells_of(&levels[1], LevelCellKind::StairsDown);
            assert_eq!(downs1.len(), 1, "seed {s}: the last rung carries exactly the dangling terminus: {downs1:?}");
            for l in &levels {
                for (c, k) in l.cells.iter() {
                    if k == LevelCellKind::StairsDown {
                        assert_ne!(l.cells.get(c), Some(LevelCellKind::StairsUp));
                    }
                }
            }
        }
        assert!(paired_stairs_seen > 200, "the sweep must exercise multi-stair rungs, not one stair each");
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel stairs_pair_by_coordinate_across_adjacent_rungs`
Expected: compile error (`generate_descent` has no `plan` parameter).

- [ ] **Step 3: Rewrite the realizer**

In `underworld_level/mod.rs`:

1. Replace `BASE_LEVEL_W`/`BASE_LEVEL_H` and `generate_level_extent`'s body with:

```rust
/// The extent a level gets, scaled by rung (deeper rungs get more room).
/// The formula lives in `hornvale_worldgen::circuit::level_extent_wh` since
/// The Crosscut so the plan's grid and this rectangle cannot disagree.
pub fn generate_level_extent(rung: hornvale_kernel::Band) -> Rect {
    let (w, h) = hornvale_worldgen::circuit::level_extent_wh(rung);
    Rect { x: 0, y: 0, w, h }
}
```

2. Replace `generate_level_with_origin`'s body. Signature gains `plan: &hornvale_worldgen::circuit::DescentPlan, level: usize` before `seed`:

```rust
    debug_assert_eq!(extent, generate_level_extent(plan.rungs[level]), "extent must be the rung's own");
    let mut cells = CellGrid::new(extent, LevelCellKind::Wall);
    let mut dof = 0u32;
    let mut style_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_STYLE).stream();
    let mut cellular_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_CELLULAR).stream();
    let mut tunneler_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_TUNNELER).stream();
    let mut rooms_stream = seed.derive(crate::streams::UNDERWORLD_LEVEL_ROOMS).stream();
    let mut leaf_styles = Vec::new();
    let node_ids = plan.nodes_on(level);
    for &id in &node_ids {
        let rect = rect_of(plan, id);
        let style = choose_leaf_style(cave_kind, origin, character, inherited_worked_bias, &mut style_stream, &mut dof);
        let stream = match style.algorithm {
            carve::Algorithm::CellularCave => &mut cellular_stream,
            carve::Algorithm::Tunneler => &mut tunneler_stream,
            carve::Algorithm::AngularRooms | carve::Algorithm::RoomsAndCorridors => &mut rooms_stream,
        };
        dof += carve::carve(style.algorithm, rect, stream, &mut cells);
        ensure_standable(rect, &mut cells);
        leaf_styles.push(style);
    }
    // Passages: one L-corridor between the nearest walkable pair of each
    // connected region pair. Two grid-adjacent regions with NO plan edge
    // keep their wall — the non-adjacency the partition tree could never say.
    for (a, b) in plan.passages_on(level) {
        let ca = walkable_cells_in_rect(rect_of(plan, a), &cells);
        let cb = walkable_cells_in_rect(rect_of(plan, b), &cells);
        if let Some((pa, pb)) = nearest_pair(&ca, &cb) {
            connect_cells(pa, pb, &mut cells);
        }
    }
    // Stairs: the plan's shared coordinate, made standable and joined to its
    // region if the carve left it in rock.
    for (upper, _lower, x, y) in plan.stairs_from(level) {
        place_stair(Cell(x, y), LevelCellKind::StairsDown, rect_of(plan, upper), &mut cells);
    }
    for (_upper, lower, x, y) in plan.stairs_into(level) {
        place_stair(Cell(x, y), LevelCellKind::StairsUp, rect_of(plan, lower), &mut cells);
    }
    // The deepest level's terminus keeps today's dangling stairs down, so
    // `STAIRS_LEAD_NOWHERE_REFUSAL` keeps its one firing case (spec §3.1).
    if level + 1 == plan.rungs.len() {
        let rect = rect_of(plan, plan.terminus);
        if let Some(c) = first_walkable_cell_in(rect, &cells) {
            cells.set(c, LevelCellKind::StairsDown);
        }
    }
    Level { extent, cells, dof, leaf_styles }
```

with these helpers (replacing `walkable_cells_in`, `connect_split_boundaries`, `place_connections`, `first_walkable_cell`):

```rust
/// The plan's region rectangle as the lattice's `Rect`.
fn rect_of(plan: &hornvale_worldgen::circuit::DescentPlan, node: hornvale_worldgen::circuit::NodeId) -> Rect {
    let r = plan.region_of(node);
    Rect { x: r.x, y: r.y, w: r.w, h: r.h }
}

/// Every `Floor`/`Flooded` cell inside `rect`, ascending `(x, y)`.
fn walkable_cells_in_rect(rect: Rect, cells: &CellGrid) -> Vec<Cell> {
    let mut out = Vec::new();
    for x in rect.x..(rect.x + rect.w) {
        for y in rect.y..(rect.y + rect.h) {
            let cell = Cell(x, y);
            if matches!(cells.get(cell), Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded)) {
                out.push(cell);
            }
        }
    }
    out
}

/// The first `Floor`/`Flooded` cell in `rect`, column-major.
fn first_walkable_cell_in(rect: Rect, cells: &CellGrid) -> Option<Cell> {
    walkable_cells_in_rect(rect, cells).into_iter().next()
}

/// A region always has somewhere to stand: if a carve left `rect` all rock,
/// open its centre cell. A guarantee, so no downstream step (passages,
/// stairs, the entrance) can find an empty region.
fn ensure_standable(rect: Rect, cells: &mut CellGrid) {
    if first_walkable_cell_in(rect, cells).is_none() {
        cells.set(Cell(rect.x + rect.w / 2, rect.y + rect.h / 2), LevelCellKind::Floor);
    }
}

/// Cut a stairs cell at `at` and, if the carve had left that cell in rock,
/// join it to the nearest walkable cell of its own region so a landing is
/// never sealed off. A stairway has a foot (spec §3.3).
fn place_stair(at: Cell, kind: LevelCellKind, region: Rect, cells: &mut CellGrid) {
    let was_walkable = matches!(cells.get(at), Some(LevelCellKind::Floor) | Some(LevelCellKind::Flooded));
    if !was_walkable {
        let walkable = walkable_cells_in_rect(region, cells);
        if let Some((_, target)) = nearest_pair(&[at], &walkable) {
            connect_cells(at, target, cells);
        }
    }
    cells.set(at, kind);
}
```

Keep `nearest_pair` and `connect_cells` as they are.

3. `generate_level_with_water`: same two new parameters; replace the `build_region`/`leaves` re-derivation with `let rects: Vec<Rect> = plan.nodes_on(level).iter().map(|&id| rect_of(plan, id)).collect();` and zip `rects` with `leaf_styles`.

4. `generate_descent` and `generate_descent_for_character`: add `plan: &hornvale_worldgen::circuit::DescentPlan` before `seed`; `assert_eq!(plan.rungs.as_slice(), rungs, "the plan was grown for these rungs")`; pass `plan, i` into `generate_level_with_water`; DELETE the `place_connections` call and the function.

5. `generate_level(extent, seed)`: build `let plan = hornvale_worldgen::circuit::plan_descent(seed, hornvale_kernel::Vertex(0), &[hornvale_kernel::Band::Undercroft], CaveKind::Karst, Character::WildCave);` and call `generate_level_with_origin(extent, …, &plan, 0, seed)`. Update its doc: extent must be `generate_level_extent(Band::Undercroft)`.

6. `region.rs`: delete `Region`, `build_region`, `build_node`, `split_probability`, `leaves`, `collect_leaves`, `MAX_COMPOSITE_DEPTH`, `MIN_REGION_SPAN` and every test except none (the tests all exercised the tree). Keep `cut` with its doc. Rewrite the module doc: "The one primitive left from the Adit's partition tree: `cut`, which `carve.rs`'s partitioned-rooms carver still uses. The level scaffold is the plan's region grid since The Crosscut (`hornvale_worldgen::circuit`)."

7. `windows/vessel/src/streams.rs`: delete the `UNDERWORLD_LEVEL_PARTITION` entry and add, in a doc comment above `UNDERWORLD_LEVEL_CELLULAR`: "`underworld/level/v1/partition` is RETIRED OUTRIGHT (The Crosscut): the partition tree it drew is gone, nothing derives from it, and a leg nothing reads must not sit in the manifest — the same treatment `chamber/branch-root/v1` got in The Drift. Never reused."

8. Fix every test in `mod.rs` that constructs its own `Rect { w: 20, h: 12 }` or `w: 40, h: 24`: use `generate_level_extent(hornvale_kernel::Band::Undercroft)` and thread a plan (`two_rung_plan` or a one-rung equivalent) through `generate_level_with_origin`. The connectivity test `every_walkable_cell_is_reachable_from_every_other` keeps its assertion unchanged.

- [ ] **Step 4: Update the integration tests**

In `windows/vessel/tests/suite/underworld_level_generation.rs`: every `generate_descent(`/`generate_descent_for_character(` call gains a plan built with `hornvale_worldgen::circuit::plan_descent(seed, vertex_or_Vertex(0), &rungs, cave_kind, character)` for the same rungs. In `measure_flooded_cell_reachability_across_the_descent`, the real descent has a real `vertex` — pass it. Update `find_cell_of_kind`'s doc to "the FIRST such cell; a rung may hold several since The Crosscut" and, for rung 0's entry cell, use the plan's entrance region: `first walkable cell in plan.region_of(plan.entrance)` (write a small helper). Remove the paragraph about `place_connections` from `standable_cells`'s doc and say instead that stairs cells are standable and join regions.

- [ ] **Step 5: Run the vessel tests**

Run: `cargo test -p hornvale-vessel 2>&1 | tee /tmp/hv-vessel.txt | tail -20`
Expected: all pass, including `stairs_pair_by_coordinate_across_adjacent_rungs`, `every_walkable_cell_is_reachable_from_every_other` and the flooded-reachability measure (which REPORTS; read its new numbers and paste them into the ledger — they are the campaign's first look at how wet the new levels are).

- [ ] **Step 6: Lint and commit**

Run: `cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings && cargo run --manifest-path tools/type-audit/Cargo.toml -- check`

```bash
git add windows/vessel/src/underworld_level windows/vessel/src/streams.rs windows/vessel/tests/suite/underworld_level_generation.rs
git commit -m "feat(underworld): realize the descent plan — grid regions, plan passages, coordinate-paired stairs; retire the partition tree"
```

---

### Task 4: The walk — plan in `enter`, stairs pair by coordinate, the cross-floor test

**Files:**
- Modify: `windows/vessel/src/underground.rs`
- Modify: `windows/vessel/src/session.rs` (tests at ~14145, ~14624; one new test)

**Interfaces:**
- Consumes: Task 3's `generate_descent_for_character(.., plan, seed)`; `circuit::{plan_descent, DescentPlan, EdgeKind}`.
- Produces: `Underground.plan: DescentPlan` (`pub(crate)`); `peek_stairs` semantics: a `StairsDown` at cell `c` on rung `r` lands at `(r + 1, c)`; a `StairsUp` at `c` lands at `(r - 1, c)`.

- [ ] **Step 1: Write the failing cross-floor walk test (spec §7 acceptance 2)**

In `session.rs`'s test module, beside `stairs_connect_adjacent_rungs_in_both_directions`:

```rust
    /// THE CROSSCUT, spec §7 acceptance 2: a stairway down leads to a floor
    /// whose route returns you to the floor above by a DIFFERENT stairway —
    /// walked through `down`, `go <dir>` and `up`, never read off the graph.
    /// Searches seed 42's open cave mouths for a plan whose level-0 realm
    /// crosses to level 1; the sweep must find one (the plan's own
    /// `some_seed_produces_a_cross_floor_realm` says the move is reachable).
    #[test]
    fn a_cross_floor_cycle_is_walked_down_along_and_back_up_another_stair() {
        use crate::underworld_level::LevelCellKind;
        use hornvale_worldgen::circuit::EdgeKind;
        let world = seam_world();
        let (mut session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
        let terrain = session.wctx.terrain.clone().expect("seed 42 builds terrain");
        let pins = hornvale_worldgen::BarrierPins::default();
        let candidates: Vec<_> = cave_entrance_states(&terrain, world.seed)
            .filter(|(vertex, _, is_open)| *is_open
                && seeded_entrance_barrier(world.seed, *vertex, &pins) == hornvale_worldgen::BarrierState::Open)
            .map(|(v, c, _)| (v, c))
            .collect();
        let mut walked = false;
        for (vertex, cave) in candidates {
            session.delve_at(vertex, cave);
            let (down_at, up_at) = {
                let ug = session.underground.as_ref().expect("descended");
                let Some(realm) = ug.plan.realms.iter().find(|r| r.anchor_level == 0
                    && r.path_b.iter().any(|&n| ug.plan.nodes[n].level == 1)) else {
                    session.underground = None;
                    continue;
                };
                // path_b = [u, lu, ..., le, end]: the stairs are u->lu and end->le.
                let u = realm.path_b[0];
                let end = *realm.path_b.last().unwrap();
                let stair_of = |upper| ug.plan.edges.iter().find_map(|e| match e.kind {
                    EdgeKind::Stair { x, y } if e.a == upper => Some(crate::lattice::Cell(x, y)),
                    _ => None,
                }).expect("a cross-floor realm's endpoints each carry a stairway");
                (stair_of(u), stair_of(end))
            };
            assert_ne!(down_at, up_at, "a cross-floor cycle uses two different stairways");
            session.underground.as_mut().expect("descended").cell = down_at;
            let _ = session.handle("down");
            assert_eq!(session.underground.as_ref().unwrap().rung, 1);
            assert_eq!(session.underground.as_ref().unwrap().cell, down_at, "lands on the same coordinate");
            // Walk level 1 from the landing to the other stairway's foot with
            // `go <dir>`, along a path the test computes over standable cells.
            let route = {
                let ug = session.underground.as_ref().unwrap();
                let level = ug.level();
                let passable = |c: crate::lattice::Cell| matches!(level.cells.get(c),
                    Some(LevelCellKind::Floor | LevelCellKind::Flooded | LevelCellKind::StairsUp | LevelCellKind::StairsDown));
                let mut prev = std::collections::BTreeMap::new();
                let mut q = std::collections::VecDeque::from([down_at]);
                prev.insert(down_at, down_at);
                while let Some(c) = q.pop_front() {
                    if c == up_at { break; }
                    for (dx, dy) in [(0, -1), (1, 0), (0, 1), (-1, 0)] {
                        let n = crate::lattice::Cell(c.0 + dx, c.1 + dy);
                        if passable(n) && !prev.contains_key(&n) { prev.insert(n, c); q.push_back(n); }
                    }
                }
                assert!(prev.contains_key(&up_at), "level 1 must connect the two stairways");
                let mut route = vec![up_at];
                while *route.last().unwrap() != down_at { let p = prev[route.last().unwrap()]; route.push(p); }
                route.reverse();
                route
            };
            for w in route.windows(2) {
                let (dx, dy) = (w[1].0 - w[0].0, w[1].1 - w[0].1);
                let dir = match (dx, dy) { (0, -1) => "north", (1, 0) => "east", (0, 1) => "south", (-1, 0) => "west", _ => unreachable!() };
                session.handle(&format!("go {dir}"));
                assert_eq!(session.underground.as_ref().unwrap().cell, w[1], "step {dir} refused mid-route");
            }
            let _ = session.handle("up");
            let ug = session.underground.as_ref().unwrap();
            assert_eq!(ug.rung, 0, "back on the upper floor");
            assert_eq!(ug.cell, up_at, "by the OTHER stairway");
            walked = true;
            break;
        }
        assert!(walked, "no open cave on seed 42 offered a level-0 cross-floor realm — widen the search before weakening this test");
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-vessel a_cross_floor_cycle_is_walked`
Expected: compile error — `Underground` has no field `plan`.

- [ ] **Step 3: Implement**

In `underground.rs`:

1. Add the field and its doc:

```rust
    /// The series-parallel plan this descent was realized from (The
    /// Crosscut). FRAME-tier like everything else here; kept so stairs can
    /// be paired and tests can read the structure a walk should exhibit.
    pub(crate) plan: hornvale_worldgen::circuit::DescentPlan,
```

2. In `enter`, before `generate_descent_for_character`:

```rust
        let plan = hornvale_worldgen::circuit::plan_descent(
            seed,
            vertex,
            &rungs,
            cave.kind,
            hornvale_worldgen::character::Character::WildCave,
        );
```

pass `&plan` to `generate_descent_for_character`, and choose the entrance cell as the first `Floor`/`Flooded` cell inside the entrance region:

```rust
        let entrance_rect = {
            let r = plan.region_of(plan.entrance);
            Rect { x: r.x, y: r.y, w: r.w, h: r.h }
        };
        let cell = descent[0]
            .cells
            .iter()
            .filter(|(c, _)| entrance_rect.contains(*c))
            .find(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
            .map(|(c, _)| c)
            .expect("the entrance region always has a standable cell (ensure_standable)");
```

Store `plan` in the struct.

3. Rewrite `peek_stairs`'s body and doc:

```rust
    /// … (keep the first three paragraphs of the existing doc) …
    ///
    /// **Stairs pair by COORDINATE (The Crosscut, decision 0567).** A
    /// stairway's two ends share one cell: descending from `StairsDown` at
    /// `c` on rung `n` lands on `c` of rung `n + 1`, which the realizer
    /// guarantees is that rung's `StairsUp`; ascending is the mirror. No
    /// scan, no ordinal, no table — `stairs_pair_by_coordinate_across_adjacent_rungs`
    /// (`underworld_level/mod.rs`) pins the guarantee over 200 seeds. The
    /// one `StairsDown` with no twin is the deepest rung's terminus, which
    /// still refuses with `STAIRS_LEAD_NOWHERE_REFUSAL`.
    pub(crate) fn peek_stairs(&self) -> Result<(usize, Cell), &'static str> {
        match self.descent[self.rung].cells.get(self.cell) {
            Some(LevelCellKind::StairsDown) => {
                let next = self.rung + 1;
                if next >= self.descent.len()
                    || self.descent[next].cells.get(self.cell) != Some(LevelCellKind::StairsUp)
                {
                    return Err(STAIRS_LEAD_NOWHERE_REFUSAL);
                }
                Ok((next, self.cell))
            }
            Some(LevelCellKind::StairsUp) => {
                let next = self
                    .rung
                    .checked_sub(1)
                    .expect("a StairsUp cell only exists at rung > 0 (the realizer emits none on rung 0)");
                debug_assert_eq!(
                    self.descent[next].cells.get(self.cell),
                    Some(LevelCellKind::StairsDown),
                    "stairs pair by coordinate"
                );
                Ok((next, self.cell))
            }
            _ => Err(NOT_ON_STAIRS_REFUSAL),
        }
    }
```

Update `STAIRS_LEAD_NOWHERE_REFUSAL`'s doc comment: it now names the terminus cut by the realizer on the deepest level rather than `place_connections`.

4. In `session.rs` tests: replace the two `expect("every rung has exactly one StairsDown cell")`-style messages with `expect("every rung has at least one StairsDown cell")`; the assertions themselves stand. In `the_deepest_rungs_stairs_down_refuses_without_moving`, the last rung's single `StairsDown` is the terminus — unchanged behaviour.

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-vessel stairs 2>&1 | tail -20 && cargo test -p hornvale-vessel a_cross_floor_cycle_is_walked`
Expected: all pass. If the walk test cannot find a candidate on seed 42, do NOT weaken it: record which seeds' plans do carry a level-0 cross-floor realm (loop over `cave_entrance_states`) in the ledger and switch `seam_world()` to a seed that has one only if seed 42 genuinely has none among its open mouths — say so in the test's doc.

- [ ] **Step 5: Whole-crate check, lint, commit**

Run: `cargo test -p hornvale-vessel 2>&1 | tail -5 && cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings`

```bash
git add windows/vessel/src/underground.rs windows/vessel/src/session.rs
git commit -m "feat(underground): build the plan on entry, pair stairs by coordinate, walk a cross-floor cycle"
```

- [ ] **Step 6: Stage gate**

`make sluice-stage BRANCH=campaign/the-crosscut REF=$(git rev-parse HEAD)`. This is the first stage whose merge product changes what a player walks; read the chamber's `gate` output for any fixture drift it reports.

---

### Task 5: The readout, the CLI verb, the audit page, the roster

**Files:**
- Create: `windows/worldgen/src/circuit_readout.rs`
- Modify: `windows/worldgen/src/lib.rs` (`pub mod circuit_readout;`)
- Modify: `cli/src/main.rs` (help text line beside `underworld`; `Some("circuit") => cmd_circuit(&args)`; `fn cmd_circuit` mirroring `cmd_underworld` at ~1326)
- Modify: `cli/src/streams.rs` (stamp roster)
- Modify: `scripts/regenerate-artifacts.sh`, `docs/generated-paths.txt`
- Create (generated): `docs/audits/underworld-circuit-seed-panel.md`

**Interfaces:**
- Consumes: `circuit::{plan_descent, loop_share, anchored_realms, has_cross_floor_realm, semilattice_overlap}`; `GeneratedTerrain::{geosphere().vertices(), cave_at, is_ocean}` as `underworld_readout::render_underworld` uses them.
- Produces: `pub fn render_circuit_panel(seed: Seed, terrain: &GeneratedTerrain) -> String`.

- [ ] **Step 1: Write the failing determinism test**

Create `circuit_readout.rs` with a test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    /// The page is a witness: two renders of one seed are byte-identical,
    /// and every frozen verdict word appears exactly once per section.
    #[test]
    fn the_panel_is_deterministic_and_carries_every_verdict() {
        let wc = crate::WorldComponents::assemble().expect("registries");
        let terrain = crate::build_terrain_for_tests(Seed(42), &wc);
        let a = render_circuit_panel(Seed(42), &terrain);
        let b = render_circuit_panel(Seed(42), &terrain);
        assert_eq!(a, b);
        for heading in ["loop share", "density ordering", "cross-floor", "semilattice overlap"] {
            assert!(a.contains(heading), "missing section {heading}");
        }
        assert!(a.contains("PASSED") || a.contains("FALSIFIED"));
    }
}
```

If `build_terrain_for_tests` does not exist under that name, use whatever helper `underworld_readout.rs`'s own tests use to obtain a `GeneratedTerrain` for seed 42 — read that file's test module and copy its exact call.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen circuit_readout`
Expected: compile error — `render_circuit_panel` undefined.

- [ ] **Step 3: Implement the readout**

```rust
//! The Crosscut's committed witness: `hornvale circuit --seed <N>` renders
//! the preregistered readouts of spec §4 over every cave-bearing vertex of
//! one seed. Verdicts use the frozen words PASSED / FALSIFIED and nothing
//! here is tuned to reach one — a null is a finding (decision 0016).

use hornvale_kernel::{Band, Seed};
use hornvale_terrain::{CaveKind, GeneratedTerrain};

use crate::character::Character;
use crate::circuit::{anchored_realms, has_cross_floor_realm, loop_share, plan_descent, semilattice_overlap};

fn habitation_rungs() -> Vec<Band> {
    hornvale_terrain::rungs().iter().copied().filter(|r| *r != Band::Surface).collect()
}

fn median(v: &mut [f64]) -> Option<f64> {
    if v.is_empty() { return None; }
    v.sort_by(|a, b| a.total_cmp(b));
    Some(v[v.len() / 2])
}

/// Render the panel for one seed.
pub fn render_circuit_panel(seed: Seed, terrain: &GeneratedTerrain) -> String {
    let rungs = habitation_rungs();
    let mut out = String::new();
    let mut loop_shares = Vec::new();
    let mut cross = 0usize;
    let mut descents = 0usize;
    let mut overlaps = Vec::new();
    // density per (kind, character): every level's anchored count
    let mut density: std::collections::BTreeMap<(String, String), Vec<f64>> = std::collections::BTreeMap::new();
    for vertex in terrain.geosphere().vertices() {
        let Some(cave) = terrain.cave_at(vertex) else { continue };
        if terrain.is_ocean(vertex) { continue; }
        descents += 1;
        let plan = plan_descent(seed, vertex, &rungs, cave.kind, Character::WildCave);
        loop_shares.push(loop_share(&plan));
        if has_cross_floor_realm(&plan) { cross += 1; }
        if let Some(o) = semilattice_overlap(&plan) { overlaps.push(o); }
        for character in [Character::WildCave, Character::DrowTier] {
            let p = if character == Character::WildCave { plan.clone() } else { plan_descent(seed, vertex, &rungs, cave.kind, character) };
            let key = (format!("{:?}", cave.kind), format!("{character:?}"));
            let entry = density.entry(key).or_default();
            for level in 0..rungs.len() { entry.push(anchored_realms(&p, level) as f64); }
        }
    }
    out.push_str(&format!("seed {}: {descents} descents\n\n", seed.0));
    // §4.1
    let ls = median(&mut loop_shares).unwrap_or(0.0);
    out.push_str(&format!("loop share: median {ls:.4} (frozen floor 0.50) -> {}\n", if ls >= 0.5 { "PASSED" } else { "FALSIFIED" }));
    // §4.2
    let med = |kind: &str, ch: &str| density.get(&(kind.to_string(), ch.to_string())).cloned().and_then(|mut v| median(&mut v));
    let (lt, fr, ka) = (med("LavaTube", "WildCave"), med("Fracture", "WildCave"), med("Karst", "WildCave"));
    let kind_ordered = matches!((lt, fr, ka), (Some(a), Some(b), Some(c)) if a < b && b < c);
    let worked_ordered = ["LavaTube", "Fracture", "Karst"].iter().all(|k| match (med(k, "DrowTier"), med(k, "WildCave")) {
        (Some(d), Some(w)) => d > w, _ => true });
    out.push_str("density ordering (median anchored realms per level):\n");
    for k in ["LavaTube", "Fracture", "Karst"] {
        out.push_str(&format!("  {k:<9} WildCave {:?}  DrowTier {:?}\n", med(k, "WildCave"), med(k, "DrowTier")));
    }
    out.push_str(&format!("  LavaTube < Fracture < Karst -> {}\n", if kind_ordered { "PASSED" } else { "FALSIFIED" }));
    out.push_str(&format!("  DrowTier > WildCave within kind -> {}\n", if worked_ordered { "PASSED" } else { "FALSIFIED" }));
    // §4.3
    let share = if descents == 0 { 0.0 } else { cross as f64 / descents as f64 };
    out.push_str(&format!("cross-floor: {cross}/{descents} descents = {share:.4} (frozen floor 0.25) -> {}\n", if share >= 0.25 { "PASSED" } else { "FALSIFIED" }));
    // §4.4
    out.push_str(&format!("semilattice overlap: median {:?} (report only)\n", median(&mut overlaps)));
    out
}
```

Note the kind medians compare `Option<f64>` — when a seed has no cave of one kind, print `None` and treat the ordering as PASSED-vacuously ONLY if you say so in the line (`-> NOT MEASURABLE (no <kind> caves this seed)`); do not print PASSED for a comparison that did not happen. Adjust the `kind_ordered` arm to a three-way `PASSED` / `FALSIFIED` / `NOT MEASURABLE`.

- [ ] **Step 4: CLI verb, script, path list, roster**

1. `cli/src/main.rs`: add help line `  hornvale circuit --seed <N>              dump one seed's descent-plan readouts (The Crosscut witness)` beside the `underworld` line; add `Some("circuit") => cmd_circuit(&args),`; write `fn cmd_circuit` as a copy of `cmd_underworld` calling `world_builder::circuit_readout::render_circuit_panel`.
2. `scripts/regenerate-artifacts.sh`: add `gen_underworld_circuit()` after `gen_underworld_lattice()` — a `printf` header (title `# The Circuits of Seeds 42, 7 and 1234`, two paragraphs: what the page witnesses, and that verdict words are frozen by spec §4 and never tuned toward) then a fenced `text` block with `run -p hornvale -- circuit --seed 42`, `7`, `1234` as the lattice function does. Add `spawn gen_underworld_circuit > docs/audits/underworld-circuit-seed-panel.md` directly under the lattice `spawn` line.
3. `docs/generated-paths.txt`: add the line `docs/audits/underworld-circuit-seed-panel.md	artifacts` (TAB-separated) beside the lattice line.
4. `cli/src/streams.rs` roster (`the_stamp_is_exactly_this_roster`): add `"underworld/plan/cycle v1"`, `"underworld/plan/extend v1"`, `"underworld/plan/spine v1"`, `"underworld/plan/stair v1"` in whatever order the golden sorts, and REMOVE `"underworld/level/partition v1"` with a comment: `// **underworld/level/partition v1 is GONE** (The Crosscut): the partition tree retired with it, so the label is absent, not unchanged — retired and never reused, as chamber/branch-root/v1 was.` Run `cargo test -p hornvale --test suite -- streams` and follow its diff.

- [ ] **Step 5: Regenerate and verify**

Run: `make rebaseline 2>&1 | tail -20` then `git status --short`.
Expected drift: `docs/audits/underworld-circuit-seed-panel.md` (new), `docs/audits/type-audit-report.md`, the stream-manifest reference page, `docs/digest/*` possibly, `docs/generated-path-writes.tsv`. Branch table for `docs/audits/underworld-lattice-seed-panel.md`: unchanged → as the spec expects; changed → paste the diff into the ledger and explain it before committing (the chamber lattice reads no level, so a change here means something else moved). Branch table for any client fixture under `clients/game/core/tests/fixtures/`: unchanged → fine; changed → review the diff (a chamber-band or walk-band fixture must NOT move; an underground one, if Task 0 found any, may).

Read the new audit page. Copy its four verdict lines into the ledger verbatim.

- [ ] **Step 6: Commit (adding the new file explicitly)**

```bash
git add docs/audits/underworld-circuit-seed-panel.md docs/generated-paths.txt scripts/regenerate-artifacts.sh \
  cli/src/main.rs cli/src/streams.rs windows/worldgen/src/circuit_readout.rs windows/worldgen/src/lib.rs \
  docs/audits docs/digest book/src/reference docs/generated-path-writes.tsv docs/superpowers/ledgers/2026-09-01-the-crosscut.md
git commit -m "feat(circuit): the readout — hornvale circuit, the committed seed-panel witness, roster and manifest"
```

- [ ] **Step 7: Stage gate**

`make sluice-stage BRANCH=campaign/the-crosscut REF=$(git rev-parse HEAD)`.

---

### Task 6: Book, decisions, retrospective, close

**Files:**
- Create: `docs/decisions/0566-a-place-is-a-graph-before-it-is-a-map.md`, `docs/decisions/0567-stairs-pair-by-coordinate.md`, `docs/decisions/0568-cycle-density-is-derived-not-authored.md`
- Create: `book/src/chronicle/the-crosscut.md`; Modify: `book/src/SUMMARY.md` (add `- [The Crosscut](./chronicle/the-crosscut.md)` after the latest chronicle entry)
- Modify: `book/src/frontier/idea-registry.md`
- Create: `docs/retrospectives/the-crosscut.md`
- Modify: `docs/superpowers/ledgers/2026-09-01-the-crosscut.md` (final entries)

- [ ] **Step 1: Decisions**

Each in the house format (copy the header shape of `docs/decisions/0459-*.md`: `# NNNN. Title`, `**Status:** Accepted (2026-09-DD) · **Decider:** Nathan (autopilot) · **Relates:** …`, then "In the context of …, facing …, we decided that **…** — accepting …", a Context paragraph, a Consequence paragraph, See also).

- 0566: the structural primitive is the cycle; the grammar is series-parallel (two operations); the derivation tree is the realm tree; the plan is generated per walked descent before any level is carved. Relates: 0069, 0011, `MAP-underworld-traversal-grammar`.
- 0567: a stairway's two ends share a coordinate; that is how a landing is found; the ordinal pairing was rejected as the id-as-offset bug. Relates: 0102.
- 0568: cycle density derives from `CaveKind` and workmanship; the `[1, 5]` clip is Dormans' and the one authored constant; the preregistered ordering and its verdict (paste from the audit page). Relates: 0016, 0009.

Run `cargo test -p hornvale --test suite -- docs_consistency` — it checks title/filename agreement and the decision-block declaration.

- [ ] **Step 2: Chronicle**

`book/src/chronicle/the-crosscut.md`, at the book's altitude (technical, no code, comprehensible without the source): open with the crosscut etymology; the finding that every scale was a tree; Dormans and Alexander as two readings of one skeleton; the series-parallel grammar in prose with one worked example (a lock-and-key-shaped cycle, unstamped); stairs pairing by coordinate; the four readouts with their verdicts in the frozen words and the numbers from the audit page; what the null (if any) means; what is deliberately not here (gates, residents, junctions). Add the SUMMARY line.

- [ ] **Step 3: Frontier sweep**

In `idea-registry.md`: `MAP-cycle-density-is-derived` → status `shipped`, append the verdict; `MAP-underworld-traversal-grammar` → append "The Crosscut shipped the structural half (2026-09)"; `CLIENT-semilattice-caution` → append one sentence with the measured overlap median; `TOOL-underworld-embedder-unification` → status `shipped`. Add two rows from the ledger followups: `MAP-walk-ignores-the-lattice` (the descent walk builds one level per rung and never reads the Stope/Drift runs) and `MAP-descent-carves-are-per-world` (carve streams are keyed on the world seed with no vertex, so every cave in a world carves identical interiors; the plan is per-vertex, the carves are not). Keep every Idea cell ≤ 600 characters (`python3 -c` count before committing). Run `cargo test -p hornvale --test suite -- docs_consistency`.

- [ ] **Step 4: Retrospective**

`docs/retrospectives/the-crosscut.md` — process lessons only: the spec-vs-code premise correction (RunAddr vs walked descent) found by reading `enter`, and what check would have caught it at G3; anything the stage gates surfaced; the follow-ups promoted from the ledger.

- [ ] **Step 5: Commit and hand to close**

```bash
git add docs/decisions book/src/chronicle/the-crosscut.md book/src/SUMMARY.md book/src/frontier/idea-registry.md docs/retrospectives/the-crosscut.md docs/superpowers/ledgers/2026-09-01-the-crosscut.md
git commit -m "docs(the-crosscut): chronicle, decisions 0566-0568, frontier sweep, retrospective"
```

Then `make rebaseline` once more (the digest's decision index drifts on new decisions), commit the drift, and stop: the G6 stop presents the post-G3 ledger digest to Nathan before `closing-a-campaign` submits `make sluice`.

---

## Self-review

**Spec coverage.** §3.1 plan types and grid → Task 1. §3.2 growth, budget, attributes, legs → Task 2. §3.3 realizer, passages, stairs by coordinate, retired label → Task 3 (+ roster in Task 5). §3.4 three invariants → Task 2 tests. §4.1–4.5 readouts and determinism → Task 5 (readout) and Task 2 (plan determinism), audit page declared as a file. §5 epoch grep and fixture branch tables → Task 0 and Task 5 step 5. §7 acceptance 1 (a cycle a player can choose between) is exercised by the cross-floor walk in Task 4 and by `every_node_is_reachable_from_the_entrance` plus the loop-share readout; acceptance 3 by the pairing test and the unchanged deepest-rung refusal test; acceptance 5 by the carve tests passing unmodified. §8/§9 → Task 6.

**Placeholders.** None: every step carries its code or its exact command. Two implementer judgment calls are named as such with a rule (Task 2 step 4's `attempts` bump and the brittle loop-share test; Task 4 step 4's seed choice).

**Type consistency.** `plan_descent(seed, vertex, &rungs, kind, character)` is called with that argument order in Tasks 2, 3, 4 and 5. `generate_descent(rungs, kind, origins, depths_m, water_table_m, &plan, seed)` matches Task 3's interface block and Task 3's test. `stairs_from`/`stairs_into` return `(upper, lower, x, y)` in Tasks 1, 3. `Realm.anchor_level` is used by `anchored_realms`, `has_cross_floor_realm` and the Task 4 test. `EdgeKind::Stair { x, y }` matches the Task 4 pattern.
