# The Blocking Implementation Plan

> **INCOMPLETE — WRITTEN THROUGH TASK 2. DO NOT START EXECUTION.**
>
> Tasks 1–2 are complete and executable. **Tasks 3–8 are not yet written**, and
> the plan is not ready for subagent-driven execution until they are. Resume by
> writing, in order:
>
> - **Task 3 — the checker.** Amendment 2 §1b.8's seven rules against a real
>   lattice: soundness, the wall law, closure, doorway agreement, one creature
>   per cell, determinism, and DOF reported as a number. Rule 7 is what makes the
>   embedder discipline checkable rather than aspirational.
> - **Task 4 — the ASCII render, its verb, and the parity test.** Every noun the
>   render depicts must be `examine`-able and every destination command-reachable,
>   generalizing `the_purview.rs`'s `examine_accepts_exactly_the_union_of_both_grains`.
> - **Task 5 — intra-chamber `go`,** reversing The Lintel's indoor refusal, and the
>   four documents that assert it (spec, chronicle, the `CLIENT-scale-bands` row,
>   metaplan §1b.6). Movement stays `FRAME`-tier.
> - **Task 6 — THE EPOCH.** Chamber roles, the new anchor kinds and patterns they
>   need, `room/furnishing/v1 → v2`. **This is where byte-identity breaks by design
>   and the health battery becomes the GATE rather than a check.** Galleries re-pin
>   in an isolated commit. Tasks 1–5 must each verify a *clean* drift check; this
>   one must not.
> - **Task 7 — the epoch stamp** in the world, so a reload can say "the rooms are
>   not as you remember" rather than silently rearranging a memory.
> - **Task 8 — close.** Decisions for the layout label's causality and the
>   furnishing bump; chronicle; freshness sweep; retrospective; registry flips;
>   Gradient re-score. Amend spec §10 risk 1 with Task 1 Step 5's measured number.
>
> **Two design questions are open and should be settled before Task 3** — both
> recorded in `.superpowers/sdd/decision-ledger.md` (#7, #8): whether the layout
> stream label should key on the embedding *method*, and how a structure's extent
> is derived rather than assumed. Task 1's tests hard-code a 24×16 extent as a
> placeholder for the second.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A structure's chambers become regions of one drawn floor plan, and they stop being identical.

**Architecture:** The anchor graph already exists, so layout is **floor-plan synthesis, not dungeon generation** — an *embedder* that adds no information beyond the residual degrees of freedom. v1 embeds by using BSP **inversely**: splitting a rectangle to allocate space among chambers we already have, rather than to invent rooms. The method is brief-selected (rectilinear for built places, region growing for wild), the lattice is `FRAME`-tier and never serialized, and every drawn wall is definitionally a non-adjacency.

**Tech Stack:** Rust 2024, `hornvale-vessel`. Integer arithmetic only in the layout path. Tests are `cargo nextest`. No new dependencies — the workspace allowlist is `serde` + `serde_json`.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-07-28-the-blocking-design.md`. Parent: `2026-07-25-the-rose-window-metaplan-design.md` §1b.
- **No float in the layout path.** Cross-platform byte-identity depends on it. `clippy.toml` bans `f64` transcendentals outside `hornvale_kernel::math`; this code should contain no `f64` at all.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. **No wall-clock time** except in a sanctioned benchmark with `#[allow(clippy::disallowed_types)]` and a comment, as `cli/tests/graph_cost.rs` does.
- **Tasks 1–5 must be byte-identical.** Verify with `regenerate-artifacts.sh` then `git diff --exit-code` over `book/src/gallery/ book/src/reference/ book/src/laboratory/`. **Task 6 breaks byte-identity deliberately** and is the only task that may show gallery drift.
- **`INVENTORY` and `selection` are frozen until Task 6.** Adding or reordering a pattern is an epoch (`ROOM_FURNISHING`'s own doc comment). Tasks 1–5 must not touch `windows/vessel/src/interior/pattern.rs`.
- **`room/chambers/v1` must never be bumped by this campaign.** Free today (nothing commits at chamber granularity; facts carry `place: None`), and it stops being free at the first in-chamber mark.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **Type audit is default-deny**, tags exactly one line, on the **struct's** doc attrs for field tags (`bare-ok(class: field)`) — a field-level tag is silently ignored (`tools/type-audit/src/extract.rs:150`).
- **`cargo fmt` as the final step before every commit.**
- Registry rows are capped at **600 chars** and the cap is append-never; if this campaign adds rows, write them as index entries.

## File Structure

```
  windows/vessel/src/lattice/           NEW — mirrors interior/'s shape
    mod.rs        Lattice, Cell, CellId, Rect; re-exports
    allocate.rs   rectilinear BSP allocation (built places)
    grow.rs       region growing (wild places)
    classify.rs   read relations back off a lattice (the realized graph)
    render.rs     ASCII, plus the legend the parity test walks
  windows/vessel/src/interior/pattern.rs   MODIFIED in Task 6 only (the epoch)
  windows/vessel/src/interior/anchor.rs    MODIFIED in Task 6 only (new kinds)
  windows/vessel/src/chamber_prose.rs      MODIFIED in Task 6 (new nouns)
  windows/vessel/src/session.rs            MODIFIED in Tasks 4, 5, 7
  windows/vessel/src/streams.rs            MODIFIED in Task 6 (labels)
  windows/vessel/tests/the_blocking.rs     NEW — the observable end
```

`lattice/` is a directory rather than one file because five responsibilities with one shared type is exactly the shape `interior/` already has, and the allocator and the grower must be independently testable.

---

### Task 1: The lattice type, and rectilinear allocation

**Files:**
- Create: `windows/vessel/src/lattice/mod.rs`, `windows/vessel/src/lattice/allocate.rs`
- Modify: `windows/vessel/src/lib.rs` (register the module **in this step**, before the tests — a test-only module outside the module tree is not compiled, so the filter matches zero tests instead of failing)

**Interfaces:**
- Consumes: `crate::structure::{Structure, MAX_CHAMBERS}`, `hornvale_kernel::Seed`.
- Produces: `Rect { x: i32, y: i32, w: i32, h: i32 }`, `Cell(pub i32, pub i32)`, `Lattice { extent: Rect, regions: Vec<Rect>, walls: BTreeSet<(Cell, Cell)>, doorways: Vec<(usize, usize, Cell)> }`, and `allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice`.

**The embedder's discipline:** `allocate` receives the chamber *count and link structure* and must produce one region per chamber, adjacent regions sharing an edge wherever `links` says so. It may choose *where* to split (the residual DOF) and nothing else.

- [ ] **Step 1: Write the failing tests**

Create `windows/vessel/src/lattice/mod.rs`:

```rust
//! The LATTICE: a structure's chambers embedded as regions of one grid.
//!
//! This is floor-plan synthesis, not dungeon generation. The anchor graph
//! already exists, so the job is **contents → map**: given chambers and their
//! adjacencies, produce a subdivision that realizes exactly those adjacencies.
//! An embedder is judged by FIDELITY, where a generator is judged by variety —
//! so this code may add no information beyond the residual degrees of freedom
//! (Rose Window Amendment 1 §1a.7).
//!
//! NOTHING HERE IS SERIALIZED (decision 0069). Cells are `FRAME`-tier: derived
//! on entry, discarded on exit, so re-walking a place is byte-identical by
//! construction rather than by policy.

pub mod allocate;

pub use allocate::allocate;

use std::collections::BTreeSet;

/// An integer axis-aligned rectangle: origin plus extent, half-open on the far
/// edges, so `w` and `h` are counts of cells rather than coordinates.
/// type-audit: bare-ok(count: x), bare-ok(count: y), bare-ok(count: w), bare-ok(count: h)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rect {
    /// Left edge, in cells.
    pub x: i32,
    /// Top edge, in cells.
    pub y: i32,
    /// Width in cells; always `>= 1` for a region the embedder emits.
    pub w: i32,
    /// Height in cells; always `>= 1` for a region the embedder emits.
    pub h: i32,
}

impl Rect {
    /// Does this rectangle contain `cell`?
    pub fn contains(&self, cell: Cell) -> bool {
        cell.0 >= self.x && cell.0 < self.x + self.w && cell.1 >= self.y && cell.1 < self.y + self.h
    }
    /// Cell count.
    /// type-audit: bare-ok(count: return)
    pub fn area(&self) -> i32 {
        self.w * self.h
    }
}

/// One cell of the lattice, in lattice-local coordinates. `FRAME`-tier: never
/// serialized, never a fact's object (decision 0069).
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cell(pub i32, pub i32);

/// A structure embedded as one grid.
/// type-audit: bare-ok(index: doorways)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lattice {
    /// The whole plan's bounds.
    pub extent: Rect,
    /// One region per chamber, indexed as `Structure::chambers` is.
    pub regions: Vec<Rect>,
    /// Unordered cell pairs a mover may NOT cross. A wall is definitionally a
    /// non-adjacency: a drawn wall with no entry here is a lie (§7 rule 2).
    pub walls: BTreeSet<(Cell, Cell)>,
    /// `(chamber a, chamber b, the cell you pass through)`, one per link in
    /// `Structure::links`.
    pub doorways: Vec<(usize, usize, Cell)>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::structure::structure_at;
    use hornvale_kernel::{RoomAddr, Seed};

    const WALK: u32 = 12;

    fn locale() -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    fn built() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    fn extent() -> Rect {
        Rect { x: 0, y: 0, w: 24, h: 16 }
    }

    fn embed(seed: u64) -> (crate::structure::Structure, Lattice) {
        let s = structure_at(&locale(), &built(), Seed(seed), WALK).expect("built");
        let l = allocate(&s, extent(), Seed(seed));
        (s, l)
    }

    #[test]
    fn one_region_per_chamber() {
        let (s, l) = embed(42);
        assert_eq!(l.regions.len(), s.chambers.len());
    }

    #[test]
    fn regions_tile_the_extent_without_overlapping() {
        let (_, l) = embed(42);
        let total: i32 = l.regions.iter().map(Rect::area).sum();
        assert_eq!(
            total,
            l.extent.area(),
            "regions must exactly partition the extent — a gap is unreachable \
             space and an overlap is two chambers claiming one cell"
        );
        for a in 0..l.regions.len() {
            for b in (a + 1)..l.regions.len() {
                for cx in l.regions[b].x..(l.regions[b].x + l.regions[b].w) {
                    for cy in l.regions[b].y..(l.regions[b].y + l.regions[b].h) {
                        assert!(
                            !l.regions[a].contains(Cell(cx, cy)),
                            "regions {a} and {b} overlap at ({cx},{cy})"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn every_link_gets_exactly_one_doorway() {
        let (s, l) = embed(42);
        assert_eq!(l.doorways.len(), s.links.len());
        for &(a, b) in &s.links {
            assert!(
                l.doorways
                    .iter()
                    .any(|&(da, db, _)| (da, db) == (a, b) || (da, db) == (b, a)),
                "link ({a},{b}) has no doorway"
            );
        }
    }

    #[test]
    fn a_doorway_cell_lies_on_the_shared_edge_of_both_regions() {
        let (_, l) = embed(42);
        for &(a, b, cell) in &l.doorways {
            let (ra, rb) = (l.regions[a], l.regions[b]);
            assert!(
                ra.contains(cell) || rb.contains(cell),
                "doorway cell {cell:?} is in neither region {a} nor {b}"
            );
            // The two regions must actually touch: their spans overlap on one
            // axis and abut on the other.
            let touches_x = ra.x + ra.w == rb.x || rb.x + rb.w == ra.x;
            let touches_y = ra.y + ra.h == rb.y || rb.y + rb.h == ra.y;
            assert!(
                touches_x || touches_y,
                "regions {a} and {b} are linked but do not abut"
            );
        }
    }

    #[test]
    fn the_embedding_is_pure() {
        for seed in 0..8u64 {
            let a = embed(seed);
            let b = embed(seed);
            assert_eq!(a.1, b.1, "seed {seed}: embedding is not a pure function");
        }
    }

    #[test]
    fn the_seed_is_read_at_all() {
        // Where a chamber count leaves freedom, the split position is the
        // residual DOF and the seed fills it. Eight seeds must not all agree.
        let plans: Vec<Lattice> = (0..8u64).map(|s| embed(s).1).collect();
        assert!(
            plans.iter().any(|p| *p != plans[0]),
            "eight seeds produced identical plans — the seed is ignored"
        );
    }

    #[test]
    fn no_region_is_degenerate() {
        for seed in 0..8u64 {
            let (_, l) = embed(seed);
            for (i, r) in l.regions.iter().enumerate() {
                assert!(r.w >= 2 && r.h >= 2, "seed {seed}: region {i} is {r:?} — a \
                    chamber narrower than 2 cells has no interior to stand in");
            }
        }
    }
}
```

Add to `windows/vessel/src/lib.rs`, alongside the existing `pub mod` block and re-export block, in the file's existing alphabetical order:

```rust
pub mod lattice;
```
```rust
pub use lattice::{Cell, Lattice, Rect, allocate};
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-vessel lattice:: 2>&1 | tail -20`
Expected: FAIL to compile — `cannot find function allocate` (the `pub mod allocate;` line refers to a file that does not exist yet).

- [ ] **Step 3: Implement the allocator**

Create `windows/vessel/src/lattice/allocate.rs`:

```rust
//! Rectilinear allocation: BSP run INVERSELY.
//!
//! Wolverson's chapter 4 splits a rectangle to *invent* rooms. This splits one
//! to *allocate* space among chambers that already exist, in `links` order, so
//! consecutive chambers share the edge their doorway sits on and adjacency is
//! realized by construction rather than checked afterwards.
//!
//! `structure_at` builds a PATH graph, so the recursion is a chain: split off
//! the first chamber, recurse on the remainder. Integer arithmetic only — no
//! float enters world identity.

use super::{Cell, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::BTreeSet;

/// The smallest a chamber may be on either axis. Below 2 there is no interior
/// cell to stand in once walls take the boundary.
/// type-audit: bare-ok(count)
pub const MIN_CHAMBER_SPAN: i32 = 2;

/// Embed `structure` in `extent`.
///
/// Splits along the longer axis at each step, at a position the seed chooses
/// within the band that leaves both sides at least `MIN_CHAMBER_SPAN` — that
/// band IS the residual degree of freedom, and the seed fills exactly it.
/// type-audit: bare-ok(count: return)
pub fn allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT).stream();
    let mut regions: Vec<Rect> = Vec::with_capacity(n);
    let mut remaining = extent;

    for i in 0..n {
        if i + 1 == n {
            regions.push(remaining);
            break;
        }
        // Give this chamber a fair share of what is left, then let the seed
        // move the cut inside the legal band.
        let parts = (n - i) as i32;
        let (mine, rest) = split(remaining, parts, &mut stream);
        regions.push(mine);
        remaining = rest;
    }

    let doorways = structure
        .links
        .iter()
        .map(|&(a, b)| {
            let cell = shared_edge_cell(regions[a], regions[b]);
            (a, b, cell)
        })
        .collect::<Vec<_>>();

    let walls = walls_between(&regions, &doorways);
    Lattice { extent, regions, walls, doorways }
}

/// Split `r` into a first part sized about `1/parts` of it and the remainder,
/// cutting the longer axis. The cut position is drawn from the band that keeps
/// both sides at `MIN_CHAMBER_SPAN` or more.
fn split(r: Rect, parts: i32, stream: &mut hornvale_kernel::Stream) -> (Rect, Rect) {
    let horizontal = r.w >= r.h;
    let span = if horizontal { r.w } else { r.h };
    let ideal = (span / parts).max(MIN_CHAMBER_SPAN);
    let lo = MIN_CHAMBER_SPAN;
    let hi = span - MIN_CHAMBER_SPAN;
    // Jitter the ideal cut inside [lo, hi]; a degenerate band collapses to lo.
    let cut = if hi <= lo {
        lo
    } else {
        let width = (hi - lo + 1) as u64;
        let jitter = (stream.next_u64() % width) as i32;
        // Bias toward `ideal` by averaging it with the jittered position, so
        // shares stay roughly fair while the seed still moves the wall.
        (((ideal + (lo + jitter)) / 2).max(lo)).min(hi)
    };
    if horizontal {
        (
            Rect { x: r.x, y: r.y, w: cut, h: r.h },
            Rect { x: r.x + cut, y: r.y, w: r.w - cut, h: r.h },
        )
    } else {
        (
            Rect { x: r.x, y: r.y, w: r.w, h: cut },
            Rect { x: r.x, y: r.y + cut, w: r.w, h: r.h - cut },
        )
    }
}

/// A cell on the shared boundary of two abutting regions, chosen at the
/// midpoint of their overlap so a doorway is never in a corner.
fn shared_edge_cell(a: Rect, b: Rect) -> Cell {
    if a.x + a.w == b.x || b.x + b.w == a.x {
        let x = if a.x + a.w == b.x { a.x + a.w - 1 } else { b.x + b.w - 1 };
        let y0 = a.y.max(b.y);
        let y1 = (a.y + a.h).min(b.y + b.h);
        Cell(x, y0 + (y1 - y0) / 2)
    } else {
        let y = if a.y + a.h == b.y { a.y + a.h - 1 } else { b.y + b.h - 1 };
        let x0 = a.x.max(b.x);
        let x1 = (a.x + a.w).min(b.x + b.w);
        Cell(x0 + (x1 - x0) / 2, y)
    }
}

/// Every cell pair that straddles a region boundary, minus the doorway cells.
/// This is the wall set, and it is derived rather than drawn: a wall exists
/// exactly where two regions meet and no doorway was cut.
fn walls_between(regions: &[Rect], doorways: &[(usize, usize, Cell)]) -> BTreeSet<(Cell, Cell)> {
    let door_cells: BTreeSet<Cell> = doorways.iter().map(|&(_, _, c)| c).collect();
    let mut walls = BTreeSet::new();
    let region_of = |cell: Cell| regions.iter().position(|r| r.contains(cell));
    for r in regions {
        for cx in r.x..(r.x + r.w) {
            for cy in r.y..(r.y + r.h) {
                let here = Cell(cx, cy);
                for (dx, dy) in [(1, 0), (0, 1)] {
                    let there = Cell(cx + dx, cy + dy);
                    if region_of(here) != region_of(there)
                        && region_of(there).is_some()
                        && !door_cells.contains(&here)
                        && !door_cells.contains(&there)
                    {
                        walls.insert((here.min(there), here.max(there)));
                    }
                }
            }
        }
    }
    walls
}
```

Declare the stream label in `windows/vessel/src/streams.rs`, inside the existing `stream_labels!` invocation:

```rust
    /// Stream label for WHERE the embedder puts things (The Blocking).
    ///
    /// Split from `room/furnishing` on purpose (decision 0073): furnishing
    /// governs WHAT a place has and has a large blast radius; layout governs
    /// WHERE and is expected to churn as legibility is tuned. **This label is
    /// CAUSAL** — Amendment 2 §1b.7 supersedes 0075's promise that a layout
    /// solve is free to retune, because promoted incidental relations gate
    /// outcomes. Bumping it is an epoch whose blast radius is future outcomes
    /// only; committed history survives.
    ROOM_LAYOUT = "room/layout/v1" => "where the embedder places chambers";
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel lattice:: 2>&1 | tail -12`
Expected: PASS, 7 tests.

If `no_region_is_degenerate` fails at a high chamber count, the extent is too small for `MAX_CHAMBERS` chambers at `MIN_CHAMBER_SPAN` — raise the test's extent rather than lowering the minimum, and note in your report what extent a 4-chamber structure needs. That number matters to Task 4's render.

- [ ] **Step 5: Measure the embedding — the spec's risk 1**

The spec forbids a budget claim that has not been measured. Add a timed check following `cli/tests/graph_cost.rs`'s sanctioned pattern (that file shows the `#[allow(clippy::disallowed_types)]` + comment form for `Instant`, which is otherwise banned):

```bash
cargo test --release -p hornvale-vessel lattice:: -- --nocapture 2>&1 | tail -8
```

Report the median wall time of one `allocate` call at a 24×16 extent, **in release**, and state the profile — a debug number is not a measurement (this project measured a ~10× debug/release gap during The Lintel). Put the number in your report; the spec's §10 risk 1 gets amended from it at close.

- [ ] **Step 6: Format, audit, verify byte-identity, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -6
bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

The drift check **must be clean** — this task adds a module nothing calls yet. The stream manifest page *will* change (a new label), so regenerate and commit that; it is `book/src/reference/stream-manifest-generated.md`, not the `{{#include}}` wrapper.

```bash
git add windows/vessel/src/lattice/ windows/vessel/src/lib.rs windows/vessel/src/streams.rs book/src/reference/
git commit -m "feat(vessel): the lattice, and BSP run inversely

Floor-plan synthesis rather than dungeon generation: the anchor graph
exists, so this embeds contents into a map instead of generating a map
and populating it. Splitting allocates space among chambers we already
have, so adjacency is realized by construction and the seed fills only
the residual degree of freedom -- the band a cut may legally fall in.

Declares room/layout/v1, causal per Amendment 2 §1b.7. Nothing calls the
allocator yet, so the galleries are unchanged."
```

---

### Task 2: Region growing, and brief-selected method

**Files:**
- Create: `windows/vessel/src/lattice/grow.rs`
- Modify: `windows/vessel/src/lattice/mod.rs` (re-export, and the selector)

**Interfaces:**
- Produces: `grow(structure, extent, seed) -> Lattice`, and `embed(structure, brief, extent, seed) -> Lattice` which selects the method on `brief.built`.

**Why a second method at all:** the spec's §3.2 grid predicts that splitting a rectangle cannot serve a cave. A wild place wants an organic cavity, and region growing gives one from the same inputs — so the *selector* is the deliverable as much as the grower is, because it is the seam the predicted methods (radial, branching) will later plug into.

- [ ] **Step 1: Write the failing tests**

Append to `windows/vessel/src/lattice/mod.rs`'s test module:

```rust
    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, false, true)
    }

    #[test]
    fn a_wild_place_grows_instead_of_splitting() {
        // The selector is the deliverable: same structure, different method,
        // chosen on `built` alone.
        let s = structure_at(&locale(), &built(), Seed(42), WALK).expect("built");
        let rect = embed_with(&s, &built(), extent(), Seed(42));
        let organic = embed_with(&s, &wild(), extent(), Seed(42));
        assert_ne!(
            rect, organic,
            "a built place and a wild one must not embed identically"
        );
    }

    #[test]
    fn a_grown_lattice_still_covers_its_chambers_and_links() {
        let s = structure_at(&locale(), &built(), Seed(42), WALK).expect("built");
        let l = embed_with(&s, &wild(), extent(), Seed(42));
        assert_eq!(l.regions.len(), s.chambers.len());
        assert_eq!(l.doorways.len(), s.links.len());
    }

    #[test]
    fn growing_is_pure_and_reads_the_seed() {
        let s = structure_at(&locale(), &built(), Seed(7), WALK).expect("built");
        let a = embed_with(&s, &wild(), extent(), Seed(7));
        let b = embed_with(&s, &wild(), extent(), Seed(7));
        assert_eq!(a, b);
        let plans: Vec<Lattice> = (0..8u64)
            .map(|sd| embed_with(&s, &wild(), extent(), Seed(sd)))
            .collect();
        assert!(plans.iter().any(|p| *p != plans[0]), "the seed is ignored");
    }
```

Rename the helper the earlier tests use so both methods share one entry point: replace `embed`'s body to call `embed_with(&s, &built(), extent(), Seed(seed))`, and add `use super::embed_with;` if needed.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel lattice:: 2>&1 | tail -12`
Expected: FAIL — `cannot find function embed_with`.

- [ ] **Step 3: Implement growing and the selector**

Create `windows/vessel/src/lattice/grow.rs`:

```rust
//! Region growing: the organic embedding, for places nobody built.
//!
//! A cave is not a partition of a rectangle. Chambers get seed cells spread
//! across the extent, then claim cells outward in `links` order until the
//! extent is exhausted — so regions are contiguous blobs rather than rects,
//! and the boundary between two of them is wherever they met.
//!
//! Returned in the same `Lattice` shape as `allocate`, with one difference the
//! caller must respect: `regions` holds each blob's BOUNDING rect, so
//! `Rect::contains` is necessary-but-not-sufficient for membership. `cells`
//! carries the authoritative assignment.

use super::{Cell, Lattice, Rect};
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, BTreeSet};

/// Grow `structure`'s chambers into `extent` as contiguous blobs.
/// type-audit: bare-ok(count: return)
pub fn grow(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT).stream();

    // Seed cells: deterministic spread, jittered inside each nominal band.
    let mut frontier: Vec<Vec<Cell>> = Vec::with_capacity(n);
    let mut owner: BTreeMap<Cell, usize> = BTreeMap::new();
    for i in 0..n {
        let band = extent.w / n as i32;
        let bx = extent.x + band * i as i32 + (stream.next_u64() % band.max(1) as u64) as i32;
        let by = extent.y + (stream.next_u64() % extent.h.max(1) as u64) as i32;
        let c = Cell(bx.min(extent.x + extent.w - 1), by.min(extent.y + extent.h - 1));
        owner.entry(c).or_insert(i);
        frontier.push(vec![c]);
    }

    // Round-robin flood, so no chamber starves. BTreeMap keeps the order total.
    let mut progress = true;
    while progress {
        progress = false;
        for i in 0..n {
            let Some(from) = frontier[i].pop() else { continue };
            for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let next = Cell(from.0 + dx, from.1 + dy);
                if !extent.contains(next) || owner.contains_key(&next) {
                    continue;
                }
                owner.insert(next, i);
                frontier[i].push(next);
                progress = true;
            }
        }
    }

    let regions = (0..n).map(|i| bounding(&owner, i, extent)).collect::<Vec<_>>();
    let doorways = structure
        .links
        .iter()
        .map(|&(a, b)| (a, b, meeting_cell(&owner, a, b, extent)))
        .collect::<Vec<_>>();
    let walls = grown_walls(&owner, &doorways);
    Lattice { extent, regions, walls, doorways }
}

/// The bounding rect of chamber `i`'s claimed cells.
fn bounding(owner: &BTreeMap<Cell, usize>, i: usize, extent: Rect) -> Rect {
    let mut r: Option<(i32, i32, i32, i32)> = None;
    for (c, &o) in owner {
        if o != i {
            continue;
        }
        r = Some(match r {
            None => (c.0, c.1, c.0, c.1),
            Some((x0, y0, x1, y1)) => (x0.min(c.0), y0.min(c.1), x1.max(c.0), y1.max(c.1)),
        });
    }
    match r {
        Some((x0, y0, x1, y1)) => Rect { x: x0, y: y0, w: x1 - x0 + 1, h: y1 - y0 + 1 },
        None => Rect { x: extent.x, y: extent.y, w: 1, h: 1 },
    }
}

/// A cell owned by `a` that touches a cell owned by `b`; the lowest such cell
/// in `BTreeMap` order, so the choice is total and seed-free.
fn meeting_cell(owner: &BTreeMap<Cell, usize>, a: usize, b: usize, extent: Rect) -> Cell {
    for (c, &o) in owner {
        if o != a {
            continue;
        }
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let n = Cell(c.0 + dx, c.1 + dy);
            if owner.get(&n) == Some(&b) {
                return *c;
            }
        }
    }
    Cell(extent.x, extent.y)
}

/// Boundaries between differently-owned neighbours, minus doorway cells.
fn grown_walls(
    owner: &BTreeMap<Cell, usize>,
    doorways: &[(usize, usize, Cell)],
) -> BTreeSet<(Cell, Cell)> {
    let door_cells: BTreeSet<Cell> = doorways.iter().map(|&(_, _, c)| c).collect();
    let mut walls = BTreeSet::new();
    for (c, &o) in owner {
        for (dx, dy) in [(1, 0), (0, 1)] {
            let n = Cell(c.0 + dx, c.1 + dy);
            if let Some(&other) = owner.get(&n) {
                if other != o && !door_cells.contains(c) && !door_cells.contains(&n) {
                    walls.insert((*c.min(&n), *c.max(&n)));
                }
            }
        }
    }
    walls
}
```

Add to `windows/vessel/src/lattice/mod.rs`:

```rust
pub mod grow;

pub use grow::grow;

/// Embed `structure`, choosing the method the brief calls for.
///
/// The selector is the point: rectilinear allocation for places somebody built,
/// region growing for places nobody did. Radial (temples) and branching (mines)
/// are predicted by the spec's §3.2 grid and plug in here — this function is the
/// seam, which is why it exists at two methods rather than being inlined.
/// type-audit: bare-ok(count: return)
pub fn embed_with(
    structure: &Structure,
    brief: &crate::brief::Brief,
    extent: Rect,
    seed: Seed,
) -> Lattice {
    if brief.built {
        allocate(structure, extent, seed)
    } else {
        grow(structure, extent, seed)
    }
}
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-vessel lattice:: 2>&1 | tail -12`
Expected: PASS, 10 tests.

- [ ] **Step 5: Format, audit, drift, commit**

Same command block as Task 1 Step 6. The drift check must be clean — still nothing calls this.

```bash
git add windows/vessel/src/lattice/
git commit -m "feat(vessel): region growing, and the method the brief chooses

A cave is not a partition of a rectangle, so a wild place grows blobs
from spread seed cells instead of splitting. The deliverable is as much
the SELECTOR as the grower: radial temples and branching mines are
predicted by the spec's geometry grid and plug into embed_with, which is
why it exists at two methods rather than being inlined at one."
```
