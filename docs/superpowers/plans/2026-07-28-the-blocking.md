# The Blocking Implementation Plan

> **COMPLETE — Tasks 1–8 written 2026-07-28. Ready for execution.**
>
> The two questions the earlier draft left open are **settled** in
> `.superpowers/sdd/decision-ledger.md` and folded into the tasks below:
>
> - **#7 — the layout label keys on the METHOD.** Two flat labels,
>   `room/layout/v1/rectilinear` and `room/layout/v1/grown`, declared in Task 1.
>   Only for methods that exist; the predicted ones get none.
> - **#8 — the extent derives from CHAMBER COUNT alone.** `extent_for` in Task 1,
>   replacing the 24×16 placeholder. No brief read, no draw. The candidate answer
>   (count + `notability`) was overturned: it makes a building shrink when its
>   people leave.
>
> Three findings from writing Tasks 3–8 changed the plan's shape, all verified by
> grep rather than reasoned (ledger #10–#12). Read these before Task 6:
>
> - **The epoch is a prediction, not a fact.** `ROOM_FURNISHING` has exactly one
>   occurrence in the workspace — its own declaration — so nothing draws from it.
>   NPC warmth reads the *locale* interior, not a chamber's. Task 6 therefore
>   **measures** and branches three ways: RE-PIN, EPOCH, or LATENT. Do not enter
>   Task 6 expecting drift, and do not read a green health battery as an epoch
>   survived when it may mean no epoch occurred.
> - **"Tasks 1–5 byte-identical" was wrong.** §9 requires the floor plan in the
>   committed gallery, which means editing `scripts/possession-walk.txt`. Tasks
>   1–3 are clean; Tasks 4–5 move transcripts and no metric golden.
> - **The seed-42 structure is warm, two-chambered, and not a Seat.** A role table
>   whose differentiation lives in the hearth patterns or in `Seat` leaves the
>   flagship transcript unchanged with every check green.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A structure's chambers become regions of one drawn floor plan, and they stop being identical.

**Architecture:** The anchor graph already exists, so layout is **floor-plan synthesis, not dungeon generation** — an *embedder* that adds no information beyond the residual degrees of freedom. v1 embeds by using BSP **inversely**: splitting a rectangle to allocate space among chambers we already have, rather than to invent rooms. The method is brief-selected (rectilinear for built places, region growing for wild), the lattice is `FRAME`-tier and never serialized, and every drawn wall is definitionally a non-adjacency.

**Tech Stack:** Rust 2024, `hornvale-vessel`. Integer arithmetic only in the layout path. Tests are `cargo nextest`. No new dependencies — the workspace allowlist is `serde` + `serde_json`.

## Global Constraints

- **Spec:** `docs/superpowers/specs/2026-07-28-the-blocking-design.md`. Parent: `2026-07-25-the-rose-window-metaplan-design.md` §1b.
- **No float in the layout path.** Cross-platform byte-identity depends on it. `clippy.toml` bans `f64` transcendentals outside `hornvale_kernel::math`; this code should contain no `f64` at all.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec`, plus `VecDeque` where a FIFO is the point (ratified in Task 3; precedent and the determinism reasoning are in `windows/scene/src/surrounds.rs:189-191`, whose iteration order is positional and therefore byte-identical). **No wall-clock time** except in a sanctioned benchmark with `#[allow(clippy::disallowed_types)]` and a comment, as `cli/tests/graph_cost.rs` does.
- **Drift, by task** — corrected per ledger #11; the earlier "Tasks 1–5 byte-identical" contradicted §9's own artifact criterion. Verify with `regenerate-artifacts.sh` then `git diff --exit-code` over `book/src/gallery/ book/src/reference/ book/src/laboratory/`.
  - **Tasks 1–3: clean**, except the generated stream-manifest page in Task 1 (a new label is a new row). Nothing calls the embedder yet.
  - **Tasks 4, 4b and 5: transcripts move, metric goldens do NOT.** These tasks add verbs to `scripts/possession-walk.txt`, so `book/src/gallery/possession-seed-42.md` moves by construction. Inspect that diff for its **content** — §9 wants a floor plan *in* it — never merely for its absence. Anything under `book/src/laboratory/` moving in these tasks is a defect, not a re-pin.
  - **Task 6: the only task that may move a metric golden**, and only after its Step 1 measurement says which of the three outcomes landed (RE-PIN / EPOCH / LATENT — spec §5.2).
  - `make gate-full` always dirties `book/src/laboratory/generated/the-sounding/` with wall-clock timings — **revert, never re-pin** (followup 14: last pinned 748 commits ago, and no campaign since has re-pinned them despite many gate-full runs).
- **`INVENTORY` and `selection` are frozen until Task 6.** Tasks 1–5 must not touch `windows/vessel/src/interior/pattern.rs`. In Task 6, patterns are **appended, never inserted or reordered**: `selection` iterates `INVENTORY` in order and filters, so an append leaves every existing `(built, cold)` selection byte-identical, while an insertion before an existing pattern silently re-composes every room in the world.
- **`room/chambers/v1` must never be bumped by this campaign.** Free today (nothing commits at chamber granularity; facts carry `place: None`), and it stops being free at the first in-chamber mark.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a one-line doc comment.
- **Type audit is default-deny**, tags exactly one line, on the **struct's** doc attrs for field tags (`bare-ok(class: field)`) — a field-level tag is silently ignored (`tools/type-audit/src/extract.rs:150`).
- **Enum payload positions are named `Variant.index`, not `Variant`** (`tools/type-audit/src/extract.rs::push_enum`). So `bare-ok(index: Floor)` on an enum is a *stale tag position* — an error, not a verdict — while the real positions go untagged. Prefer a blanket `bare-ok(index)` when every payload is the same class, and say why in the doc comment. Found in Task 4b; **fifth** appearance of the tag-placement trap in this campaign, which is why it now has three bullets.
- **A tag on a signature with no tracked primitive is DEAD, and worse than absent.** `extract.rs` only pushes an item when the signature has a tracked position, so a tag on a function returning `Rect`, `Lattice` or any opaque type is never enforced — it reads as a verdict the tool never gave, and it can be flatly wrong (`count: return` describing a `Lattice`) with nothing to catch it. Tag `-> bool` / `-> i32` / `-> u32` / index and count parameters; do not tag opaque returns. Found in Task 1, where the plan's own snippets carried three such tags.
- **`regenerate-artifacts.sh` also rewrites `docs/audits/type-audit-report.md`**, which moves whenever tags change. It is tracked and normally committed (precedent: `c3f3af95`). Include it in the commit, or the next task opens on a dirty tree and reads it as spurious drift.
- **Run the lattice filter with `--lib`.** `cargo test -p hornvale-vessel lattice::` runs the integration binaries after the lib, so `tail` shows eight `0 passed; … filtered out` blocks and the real result scrolls off — a green suite looks like one that ran nothing.
- **`cargo fmt` as the final step before every commit.**
- Registry rows are capped at **600 chars** and the cap is append-never; if this campaign adds rows, write them as index entries.

## File Structure

```
  windows/vessel/src/lattice/           NEW — mirrors interior/'s shape
    mod.rs        Lattice, Cell, Rect, extent_for; re-exports
    allocate.rs   rectilinear BSP allocation (built places)
    grow.rs       region growing (wild places)
    classify.rs   read relations back off a lattice (the realized graph) + the checker
    occupancy.rs  one creature per cell, by construction (§7 rule 5)
    render.rs     ASCII, plus the legend the parity test walks
  windows/vessel/src/interior/pattern.rs   MODIFIED in Task 6 only (roles)
  windows/vessel/src/interior/anchor.rs    MODIFIED in Task 6 only (new kinds)
  windows/vessel/src/interior/derive.rs    MODIFIED in Task 6 (chamber_interior_of)
  windows/vessel/src/brief.rs              MODIFIED in Task 6 (peak_population)
  windows/vessel/src/chamber_prose.rs      MODIFIED in Tasks 4 and 6 (details, nouns)
  windows/vessel/src/session.rs            MODIFIED in Tasks 4, 5, 7
  windows/vessel/src/streams.rs            MODIFIED in Task 1 (layout labels), 6 (bump?)
  windows/vessel/tests/the_blocking.rs     NEW — the observable end
  scripts/possession-walk.txt              MODIFIED in Tasks 4, 5 — the gallery's input
  domains/history/src/flesh.rs             MODIFIED in Task 6 (hoist one constant)
```

`lattice/` is a directory rather than one file because five responsibilities with one shared type is exactly the shape `interior/` already has, and the allocator and the grower must be independently testable.

---

### Task 1: The lattice type, and rectilinear allocation

**Files:**
- Create: `windows/vessel/src/lattice/mod.rs`, `windows/vessel/src/lattice/allocate.rs`
- Modify: `windows/vessel/src/lib.rs` (register the module **in this step**, before the tests — a test-only module outside the module tree is not compiled, so the filter matches zero tests instead of failing)

**Interfaces:**
- Consumes: `crate::structure::{Structure, MAX_CHAMBERS}`, `hornvale_kernel::Seed`.
- Produces: `Rect { x: i32, y: i32, w: i32, h: i32 }`, `Cell(pub i32, pub i32)`, `Lattice { extent: Rect, regions: Vec<Rect>, walls: BTreeSet<(Cell, Cell)>, doorways: Vec<(usize, usize, Cell)> }`, `extent_for(structure: &Structure) -> Rect`, and `allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice`.

**Why `extent` stays a parameter of `allocate` even though `extent_for` derives it:** the embedder is a pure function of its inputs, and the tests must be able to hand it a hostile rectangle. `extent_for` is what the *production* caller uses. Keeping them separate is also what makes §7 rule 7's DOF count legible — extent is an input, never a draw.

**The embedder's discipline:** `allocate` receives the chamber *count and link structure* and must produce one region per chamber, adjacent regions sharing an edge wherever `links` says so. It may choose *where* to split (the residual DOF) and nothing else.

- [x] **Step 1: Write the failing tests**

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

use crate::structure::Structure;
use std::collections::BTreeSet;

// `extent_for`'s block arrangement is exhaustive only while four chambers fit a
// 2x2 of blocks. Raising MAX_CHAMBERS past 4 without widening the arrangement
// would silently pack six chambers into four blocks and produce slivers, so the
// coupling is asserted at compile time rather than left as a coincidence of two
// independent 4s — the same guard `structure.rs` puts on its own collision scan.
const _: () = assert!(crate::structure::MAX_CHAMBERS <= 4);

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

/// The side of one chamber's nominal block, in cells. Chosen against two bounds,
/// both checked rather than trusted: at the bottom, `MIN_CHAMBER_SPAN` must still
/// fit after a chain of splits; at the top, the widest plan any chamber count can
/// produce must fit an 80-column transcript, which
/// `the_largest_plan_fits_a_terminal` asserts.
/// type-audit: bare-ok(count)
pub const CHAMBER_SIDE: i32 = 8;

/// How big `structure`'s plan is: **exactly as big as the rooms it must hold.**
///
/// A pure function of the chamber COUNT — no brief field, no seed, no draw
/// (decision-ledger #8). Two richer formulas were rejected: `peak_population`
/// already governs how MANY buildings a settlement has rather than how big one is,
/// and `notability` describes only the ALIVE occupation, so deriving floor area
/// from it would make a building SHRINK when its people leave. Grandeur belongs in
/// what a room CONTAINS (the `hall` role's high seat), not in its floor area —
/// `CLIENT-language-not-catalogue`.
///
/// Because this consumes no draw, §7 rule 7's residual DOF stays exactly the cut
/// positions: the coarse constraint on a fine derivation is not itself a die roll.
///
/// Origin-anchored, always: cells are lattice-LOCAL, so a plan has no place in any
/// wider coordinate system to be offset into.
pub fn extent_for(structure: &Structure) -> Rect {
    // Blocks, not area: an exhaustive arrangement over 1..=MAX_CHAMBERS avoids an
    // integer square root and states the coupling to MAX_CHAMBERS out loud. The
    // regions still PARTITION the extent, so at three chambers one of them simply
    // gets the larger share — which reads as a bigger room, not as waste.
    let (cols, rows) = match structure.chambers.len() {
        0 | 1 => (1, 1),
        2 => (2, 1),
        _ => (2, 2),
    };
    Rect {
        x: 0,
        y: 0,
        w: cols * CHAMBER_SIDE,
        h: rows * CHAMBER_SIDE,
    }
}

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

    fn embed(seed: u64) -> (crate::structure::Structure, Lattice) {
        let s = structure_at(&locale(), &built(), Seed(seed), WALK).expect("built");
        let l = allocate(&s, extent_for(&s), Seed(seed));
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

    /// A structure of `n` chambers, built by hand: `extent_for` reads only the
    /// count, so the addresses need not be real places.
    fn structure_of(n: usize) -> crate::structure::Structure {
        let chambers: Vec<RoomAddr> = (0..n)
            .map(|i| RoomAddr {
                face: 3,
                path: (0..WALK).map(|_| (i % 4) as u8).collect(),
            })
            .collect();
        crate::structure::Structure {
            threshold: chambers[0].clone(),
            chambers,
            links: (1..n).map(|i| (i - 1, i)).collect(),
        }
    }

    #[test]
    fn the_plan_grows_with_the_rooms_it_must_hold() {
        let areas: Vec<i32> = (1..=crate::structure::MAX_CHAMBERS)
            .map(|n| extent_for(&structure_of(n)).area())
            .collect();
        assert!(
            areas.windows(2).all(|w| w[1] >= w[0]),
            "a structure with more chambers must not get a smaller plan: {areas:?}"
        );
        assert!(
            areas[crate::structure::MAX_CHAMBERS - 1] > areas[0],
            "the plan does not grow at all: {areas:?}"
        );
    }

    #[test]
    fn the_extent_reads_only_the_count() {
        // The overturned candidate answer keyed on `brief.notability`, which would
        // have made a building shrink when its people left (ledger #8). The
        // signature admits no brief at all, so this test guards the DERIVATION:
        // two structures of equal size get equal plans however different the
        // places are.
        assert_eq!(extent_for(&structure_of(2)), extent_for(&structure_of(2)));
    }

    #[test]
    fn the_largest_plan_fits_a_terminal() {
        // A floor plan is read in a transcript, so the ceiling on CHAMBER_SIDE is
        // a rendering fact and belongs in a test rather than in a hope. The render
        // adds a border and a legend, hence the margin.
        for n in 1..=crate::structure::MAX_CHAMBERS {
            let e = extent_for(&structure_of(n));
            assert!(
                e.w <= 72 && e.h <= 22,
                "{n} chambers derive a {}x{} plan, which does not fit an 80x24 \
                 transcript once the render's border and legend are added",
                e.w,
                e.h
            );
        }
    }
}
```

Add to `windows/vessel/src/lib.rs`, alongside the existing `pub mod` block and re-export block, in the file's existing alphabetical order:

```rust
pub mod lattice;
```
```rust
pub use lattice::{Cell, Lattice, Rect, allocate, extent_for};
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -20`
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
pub fn allocate(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT_RECTILINEAR).stream();
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

Declare the stream labels in `windows/vessel/src/streams.rs`, **appended at the end** of the existing `stream_labels!` invocation. Two labels, one per method — not one shared `room/layout/v1` (ledger #7). Append rather than insert: the generated manifest emits a crate's labels in `stream_labels()` order (`cli/src/streams.rs:32-43`), so appending adds rows and moves none.

```rust
    /// Stream label for WHERE the RECTILINEAR method puts things (The Blocking).
    ///
    /// Split from `room/furnishing` on purpose (decision 0073): furnishing
    /// governs WHAT a place has and has a large blast radius; layout governs
    /// WHERE and is expected to churn as legibility is tuned. **This label is
    /// CAUSAL** — Amendment 2 §1b.7 supersedes 0075's promise that a layout
    /// solve is free to retune, because promoted incidental relations gate
    /// outcomes. Bumping it is an epoch whose blast radius is future outcomes
    /// only; committed history survives.
    ///
    /// **One label PER METHOD**, because the unit of independent change is the
    /// algorithm: retuning the grower's flood order has nothing to do with the
    /// cut band here, and a shared label would make a grower tweak relocate
    /// every built place's floor plan too. 0073 fixes epoch granularity at
    /// declaration, so the split is made before either label has been bumped.
    ///
    /// **A change to something the two methods SHARE — `extent_for`, or what a
    /// `Lattice` means — bumps BOTH literals.** That obligation is the price of
    /// the flat form; there is no shared root segment to bump once.
    ROOM_LAYOUT_RECTILINEAR = "room/layout/v1/rectilinear"
        => "where the rectilinear method places chambers";
    /// Stream label for WHERE the REGION-GROWING method puts things.
    /// See [`ROOM_LAYOUT_RECTILINEAR`] for why this is a second label rather
    /// than a shared one, and for what bumps both.
    ROOM_LAYOUT_GROWN = "room/layout/v1/grown"
        => "where the growing method places chambers";
```

No label is declared for the spec's *predicted* methods (radial, branching). A published label is permanent — labels accumulate and never decay — so one is declared when its method has code, never in advance.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -14`
Expected: PASS, 10 tests.

If `no_region_is_degenerate` fails at a high chamber count, `CHAMBER_SIDE` is too small for `MAX_CHAMBERS` chambers at `MIN_CHAMBER_SPAN` — **raise `CHAMBER_SIDE`, never lower `MIN_CHAMBER_SPAN`**, then re-run `the_largest_plan_fits_a_terminal`, which is the opposing bound. If the two cannot both be satisfied, stop: that is a real finding about the block arrangement (a chain of splits packing four chambers into a square), not a number to fudge. Report the value you landed on — Task 4's render depends on it.

- [ ] **Step 5: Measure the embedding — the spec's risk 1**

The spec forbids a budget claim that has not been measured. Add a timed check following `cli/tests/graph_cost.rs`'s sanctioned pattern (that file shows the `#[allow(clippy::disallowed_types)]` + comment form for `Instant`, which is otherwise banned):

```bash
cargo test --release -p hornvale-vessel lattice:: -- --nocapture 2>&1 | tail -8
```

Report the median wall time of one `allocate` call at the extent a **4-chamber** structure derives (the worst case `extent_for` can produce), **in release**, and state the profile — a debug number is not a measurement (this project measured a ~10× debug/release gap during The Lintel). Put the number in your report; the spec's §10 risk 1 gets amended from it at close.

- [ ] **Step 6: Format, audit, verify byte-identity, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -6
bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

The drift check **must be clean apart from the stream manifest** — this task adds a module nothing calls yet. The manifest page *will* gain two rows; regenerate and commit it. Find its real path rather than assuming (it is the generated file, not the `{{#include}}` wrapper):

```bash
grep -rln 'GENERATED FILE' book/src/reference/ | xargs grep -ln 'room/chambers'
```

Confirm the two new rows **appended** and no existing row moved. If existing vessel rows moved, the labels were inserted rather than appended — fix that, don't re-pin it.

```bash
git add windows/vessel/src/lattice/ windows/vessel/src/lib.rs windows/vessel/src/streams.rs book/src/reference/
git commit -m "feat(vessel): the lattice, and BSP run inversely

Floor-plan synthesis rather than dungeon generation: the anchor graph
exists, so this embeds contents into a map instead of generating a map
and populating it. Splitting allocates space among chambers we already
have, so adjacency is realized by construction and the seed fills only
the residual degree of freedom -- the band a cut may legally fall in.

A plan is exactly as big as the rooms it must hold: extent_for reads the
chamber count and nothing else. Deriving it from notability would have
made a building shrink when its people left, since a brief describes only
the living occupation while a shell outlives it (ledger #8).

Declares room/layout/v1/rectilinear and room/layout/v1/grown -- one label
per METHOD, because the unit of independent change is the algorithm
(ledger #7, decision 0073). Both causal per Amendment 2 §1b.7. Nothing
calls the allocator yet, so only the manifest page moves."
```

---

### Task 2: Region growing, and brief-selected method

**Files:**
- Create: `windows/vessel/src/lattice/grow.rs`
- Modify: `windows/vessel/src/lattice/mod.rs` (re-export, and the selector)

**Interfaces:**
- Produces: `grow(structure, extent, seed) -> Lattice`, and `embed(structure, brief, extent, seed) -> Lattice` which selects the method on `brief.built`.

**Why a second method at all:** the spec's §3.2 grid predicts that splitting a rectangle cannot serve a cave. A wild place wants an organic cavity, and region growing gives one from the same inputs — so the *selector* is the deliverable as much as the grower is, because it is the seam the predicted methods (radial, branching) will later plug into.

**A note on the labels, which changes what one test proves:** each method derives from its own label (Task 1), so the two methods draw from *independent* streams. `a_wild_place_grows_instead_of_splitting` therefore tests the selector rather than resting on two algorithms happening to consume one shared stream differently — which is the weaker guarantee it would have had under a single shared label, and which could have held by coincidence at a low chamber count.

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
        let rect = embed_with(&s, &built(), extent_for(&s), Seed(42));
        let organic = embed_with(&s, &wild(), extent_for(&s), Seed(42));
        assert_ne!(
            rect, organic,
            "a built place and a wild one must not embed identically"
        );
    }

    #[test]
    fn a_grown_lattice_still_covers_its_chambers_and_links() {
        let s = structure_at(&locale(), &built(), Seed(42), WALK).expect("built");
        let l = embed_with(&s, &wild(), extent_for(&s), Seed(42));
        assert_eq!(l.regions.len(), s.chambers.len());
        assert_eq!(l.doorways.len(), s.links.len());
    }

    #[test]
    fn growing_is_pure_and_reads_the_seed() {
        let s = structure_at(&locale(), &built(), Seed(7), WALK).expect("built");
        let a = embed_with(&s, &wild(), extent_for(&s), Seed(7));
        let b = embed_with(&s, &wild(), extent_for(&s), Seed(7));
        assert_eq!(a, b);
        let plans: Vec<Lattice> = (0..8u64)
            .map(|sd| embed_with(&s, &wild(), extent_for(&s), Seed(sd)))
            .collect();
        assert!(plans.iter().any(|p| *p != plans[0]), "the seed is ignored");
    }
```

Rename the helper the earlier tests use so both methods share one entry point: replace `embed`'s body to call `embed_with(&s, &built(), extent_for(&s), Seed(seed))`, and add `use super::embed_with;` if needed.

- [x] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -14`
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
pub fn grow(structure: &Structure, extent: Rect, seed: Seed) -> Lattice {
    let n = structure.chambers.len().max(1);
    let mut stream = seed.derive(crate::streams::ROOM_LAYOUT_GROWN).stream();

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

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -14`
Expected: PASS, 13 tests (Task 1's ten plus these three).

- [ ] **Step 5: Format, audit, drift, commit**

Same command block as Task 1 Step 6. The drift check must be clean — still nothing calls this.

```bash
git add windows/vessel/src/lattice/ docs/audits/type-audit-report.md
git commit -m "feat(vessel): region growing, and the method the brief chooses

A cave is not a partition of a rectangle, so a wild place grows blobs
from spread seed cells instead of splitting. The deliverable is as much
the SELECTOR as the grower: radial temples and branching mines are
predicted by the spec's geometry grid and plug into embed_with, which is
why it exists at two methods rather than being inlined at one."
```

---

### Task 3: The checker — Amendment 2 §1b.8's seven rules

**Files:**
- Create: `windows/vessel/src/lattice/classify.rs`, `windows/vessel/src/lattice/occupancy.rs`
- Modify: `windows/vessel/src/lattice/mod.rs` (the `dof` and `owner` fields, re-exports), `windows/vessel/src/lattice/allocate.rs` and `grow.rs` (report `dof`, publish `owner`)

**Interfaces:**
- Produces: `realized_links(&Lattice) -> BTreeSet<(usize, usize)>`, `openings(&Lattice) -> Vec<(Cell, Cell)>`, `Occupancy`, `Lattice.dof: u32`, and `Lattice.owner: BTreeMap<Cell, usize>`.

**What makes this task the spine of the campaign:** rules 1–4 turn "the embedder is faithful" from a claim in the spec into a property of a value. Rule 7 turns "it adds no information beyond the residual DOF" from an aspiration into an integer comparison. Everything after this task renders, moves through, or furnishes a lattice, and each of those is only as trustworthy as this check.

**The rules, and the honest state of each:**

```
  1  soundness     every link in `structure.links` is realized      TESTED
  2  wall law      every wall IS a non-adjacency                    TESTED
  3  closure       no opening is unaccounted for                    TESTED
  4  doorways      both sides derive the same doorway               TESTED
  5  occupancy     at most one creature per cell                    TESTED as a
                   TYPE property; no creature stands in a cell
                   until The Sighting
  6  determinism   same inputs -> identical lattice, from scratch   TESTED
  7  DOF           residual freedom reported as a number            TESTED
```

Rule 5 is the one that could have been faked. There are no creatures in cells yet — that is The Sighting — so a test over generated lattices would pass vacuously and a vacuous test is worse than a missing one, because it reads as coverage. Instead the rule becomes a **type** whose `insert` cannot hold two occupants for one cell, and the test asserts *that*. Fifteen lines, real, and it is the seam The Sighting plugs into rather than a stub it will have to undo.

- [ ] **Step 1: Add the DOF report AND per-cell ownership to `Lattice`, and both embedders**

**Why `owner` is here, added after Task 2 rather than planned into Task 1** (ledger #17): a grown lattice's `regions` are *bounding* rects and they **overlap heavily** — at seed 2, regions 0 and 1 both start at `x = 0` and span 5 and 10 columns. So "which region owns this cell" cannot be answered by scanning rects, and rules 1–4 run over a grown lattice would be measuring the rects rather than the blobs. The alternative was to run rules 1–4 on rectilinear lattices only, which would leave the grower unchecked — vacuous coverage, the thing rule 5 was deliberately written to avoid. The authoritative assignment belongs in the type.

`grow` already builds exactly this map internally and throws it away; `allocate` fills it from its rects, which for rectilinear lattices is exact by construction. It is `FRAME`-tier like everything else here, so nothing is serialized and no epoch is involved.

```rust
    /// Which chamber owns each cell — the AUTHORITATIVE assignment.
    ///
    /// `regions` is a summary: for a grown lattice those rects are bounding boxes
    /// and they OVERLAP, so `Rect::contains` is necessary but not sufficient and
    /// scanning them answers a different question than the one asked. Consult this
    /// map instead. Every cell of `extent` appears exactly once.
    /// type-audit: bare-ok(index: owner)
    pub owner: BTreeMap<Cell, usize>,
```

In `mod.rs`, also add the DOF field:

```rust
    /// How many independent choices the embedder made — one per stream draw it
    /// consumed. **Reported, not recomputed**, because §7 rule 7 asks for a
    /// number and a number derived by a second, independent traversal of the
    /// result is a number that can disagree with what the embedder actually did.
    ///
    /// The checker compares this against what the anchor graph leaves free. If it
    /// exceeds that, the embedder is INVENTING, which is the one thing an
    /// embedder may not do (Amendment 1 §1a.7).
    /// type-audit: bare-ok(count: dof)
    pub dof: u32,
```

In `allocate.rs`, count the draws rather than inferring them — increment where `next_u64` is called, and return it in the `Lattice`. The chain consumes exactly one draw per cut, and there are `n - 1` cuts, but **count it, do not assert it from the loop shape**: the point of the field is to catch a future edit that spends a draw somewhere new.

In `grow.rs`, the same: two draws per chamber for its seed cell, counted.

- [ ] **Step 2: Write the failing tests**

Create `windows/vessel/src/lattice/classify.rs`:

```rust
//! Read the realized graph back off a solved lattice, and check it against the
//! graph that was specified.
//!
//! This is the direction that makes the embedder falsifiable. `allocate` claims
//! to realize `structure.links` by construction; this module does not believe it.
//! Reading adjacency back off the geometry — rather than trusting the code path
//! that wrote it — is what turns Amendment 2 §1b.8's seven rules from prose into
//! assertions.

use super::{Cell, Lattice};
use std::collections::BTreeSet;

/// Which chamber owns `cell`. `None` outside the extent.
///
/// Reads `Lattice::owner`, never `regions`. Scanning `regions` would be wrong for a
/// grown lattice, whose rects are overlapping bounding boxes — a scan answers
/// "which bounding box does this fall in first", which is a different question and
/// happens to agree only for rectilinear lattices. That near-agreement is exactly
/// what would have made the bug survive review.
/// type-audit: bare-ok(index: return)
pub fn region_of(lattice: &Lattice, cell: Cell) -> Option<usize> {
    lattice.owner.get(&cell).copied()
}

/// Every unordered pair of chambers whose regions actually touch across a cell
/// boundary that is NOT walled — the adjacency a mover can use, read off the
/// geometry rather than taken from `links`.
/// type-audit: bare-ok(index: return)
pub fn realized_links(lattice: &Lattice) -> BTreeSet<(usize, usize)> {
    let mut out = BTreeSet::new();
    for (a, b) in openings(lattice) {
        if let (Some(ra), Some(rb)) = (region_of(lattice, a), region_of(lattice, b))
            && ra != rb
        {
            out.insert((ra.min(rb), ra.max(rb)));
        }
    }
    out
}

/// Every adjacent cell pair inside the extent that no wall separates — the
/// complete set of ways a mover may pass between two cells.
pub fn openings(lattice: &Lattice) -> Vec<(Cell, Cell)> {
    let mut out = Vec::new();
    let e = lattice.extent;
    for cx in e.x..(e.x + e.w) {
        for cy in e.y..(e.y + e.h) {
            let here = Cell(cx, cy);
            for (dx, dy) in [(1, 0), (0, 1)] {
                let there = Cell(cx + dx, cy + dy);
                if !e.contains(there) {
                    continue;
                }
                let pair = (here.min(there), here.max(there));
                if !lattice.walls.contains(&pair) {
                    out.push(pair);
                }
            }
        }
    }
    out
}

/// How many independent choices the anchor graph LEAVES free for a chain
/// embedding: one cut per interior boundary.
///
/// This is the number rule 7 compares `Lattice::dof` against. It is written as a
/// function of the chamber count alone because that is all the graph determines —
/// if a future method needs more freedom than this, the honest move is to widen
/// this function and say why, never to stop comparing.
/// type-audit: bare-ok(count: chambers), bare-ok(count: return)
pub fn freedom_of_a_chain(chambers: usize) -> u32 {
    chambers.saturating_sub(1) as u32
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::lattice::{allocate, embed_with, extent_for, grow};
    use crate::structure::structure_at;
    use hornvale_kernel::{RoomAddr, Seed};

    const WALK: u32 = 12;
    const SEEDS: std::ops::Range<u64> = 0..24;

    fn locale(n: u64) -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| ((i as u64 + n) % 4) as u8).collect(),
        }
    }

    fn built() -> Brief {
        Brief::from_parts(None, None, None, None, true, true)
    }

    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, false, true)
    }

    /// Which method produced a lattice. Carried through the corpus because rule 7
    /// budgets the two methods differently — a cut is one choice, a seed cell is
    /// two — and a test that cannot tell them apart can only check the looser
    /// bound, which means not checking the tighter one at all.
    #[derive(Clone, Copy, Debug)]
    enum Method {
        Rectilinear,
        Grown,
    }

    /// Every (structure, lattice, method) triple the rules are checked over: both
    /// methods, many seeds, and therefore every chamber count `structure_at`
    /// produces.
    fn corpus() -> Vec<(crate::structure::Structure, crate::lattice::Lattice, Method)> {
        let mut out = Vec::new();
        for s in SEEDS {
            let st = structure_at(&locale(s), &built(), Seed(s), WALK).expect("built");
            let e = extent_for(&st);
            out.push((
                st.clone(),
                embed_with(&st, &built(), e, Seed(s)),
                Method::Rectilinear,
            ));
            out.push((st.clone(), embed_with(&st, &wild(), e, Seed(s)), Method::Grown));
        }
        assert!(
            out.iter().map(|(s, _, _)| s.chambers.len()).max().unwrap()
                == crate::structure::MAX_CHAMBERS,
            "the corpus never reaches MAX_CHAMBERS, so the rules are unchecked at \
             the count most likely to break them"
        );
        out
    }

    #[test]
    fn rule_1_the_realized_graph_is_exactly_the_specified_one() {
        // BOTH directions, and the second is the half that makes this an embedder
        // check rather than a completeness check. A missing adjacency is a dropped
        // relation; an EXTRA one is invented information, which is the single
        // thing an embedder may not add (Amendment 1 §1a.7). Rule 1 as the spec
        // words it names only the first, so this asserts the stronger property and
        // says so.
        for (s, l, m) in corpus() {
            let realized = realized_links(&l);
            let specified: BTreeSet<(usize, usize)> =
                s.links.iter().map(|&(a, b)| (a.min(b), a.max(b))).collect();
            for pair in &specified {
                assert!(
                    realized.contains(pair),
                    "{m:?}: link {pair:?} is specified but not realized — the \
                     embedder dropped an adjacency the anchor graph asserts"
                );
            }
            for pair in &realized {
                assert!(
                    specified.contains(pair),
                    "{m:?}: {pair:?} are adjacent in the lattice but not linked in \
                     the graph — the embedder invented a relation"
                );
            }
        }
    }

    #[test]
    fn rule_2_every_wall_is_a_non_adjacency() {
        for (_, l, _) in corpus() {
            for &(a, b) in &l.walls {
                let (ra, rb) = (region_of(&l, a), region_of(&l, b));
                assert!(
                    ra != rb || ra.is_none(),
                    "a wall stands between {a:?} and {b:?}, which are both in \
                     region {ra:?} — a decorative wall is a lie about the world"
                );
            }
        }
    }

    #[test]
    fn rule_3_no_opening_is_unaccounted_for() {
        // Closure: every way between two REGIONS must be a declared doorway. An
        // undeclared gap is a hole in the plan the render would draw as floor and
        // the mover would walk through without a door.
        for (_, l, m) in corpus() {
            let doors: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
            for (a, b) in openings(&l) {
                let (ra, rb) = (region_of(&l, a), region_of(&l, b));
                if ra == rb {
                    continue;
                }
                assert!(
                    doors.contains(&a) || doors.contains(&b),
                    "{m:?}: cells {a:?} and {b:?} lie in different regions with no \
                     wall and no doorway between them"
                );
            }
        }
    }

    #[test]
    fn rule_4_two_chambers_cannot_disagree_about_a_doorway() {
        // The doorway derives from the shared EDGE, so reading it from either
        // side must give one answer. Asserted as uniqueness per unordered pair:
        // two entries for one pair is exactly how two chambers come to disagree.
        for (s, l, _) in corpus() {
            let mut seen: BTreeSet<(usize, usize)> = BTreeSet::new();
            for &(a, b, _) in &l.doorways {
                let key = (a.min(b), a.max(b));
                assert!(
                    seen.insert(key),
                    "chambers {a} and {b} have two doorways between them, so the \
                     two sides can disagree about which cell is the door"
                );
            }
            assert_eq!(
                seen.len(),
                s.links.len(),
                "one doorway per link, no more and no fewer"
            );
        }
    }

    #[test]
    fn rule_6_the_solve_carries_no_state() {
        // Same inputs, solved from scratch, in an order that would expose a
        // carried cache: A, then B, then A again.
        let st = structure_at(&locale(1), &built(), Seed(1), WALK).expect("built");
        let e = extent_for(&st);
        let a1 = allocate(&st, e, Seed(1));
        let _b = allocate(&st, e, Seed(2));
        let a2 = allocate(&st, e, Seed(1));
        assert_eq!(a1, a2, "an intervening solve changed a later one's result");
        let g1 = grow(&st, e, Seed(1));
        let _g2 = grow(&st, e, Seed(2));
        assert_eq!(g1, grow(&st, e, Seed(1)));
    }

    #[test]
    fn rule_7_the_embedder_spends_only_the_freedom_the_graph_leaves() {
        for (s, l, m) in corpus() {
            // Each method's budget is stated in ITS OWN terms, up front. A budget
            // computed from the result -- widened whenever it is exceeded -- is not
            // a check, and that is the shape this test must not take.
            let n = s.chambers.len();
            let budget = match m {
                // One cut per interior boundary; the seed moves the cut and
                // nothing else.
                Method::Rectilinear => freedom_of_a_chain(n),
                // A seed cell is a POINT, so two draws per chamber, not one.
                Method::Grown => 2 * n as u32,
            };
            assert_eq!(
                l.dof, budget,
                "{m:?} with {n} chambers spent {} choices against a budget of \
                 {budget}. Over budget means the embedder is INVENTING, which is \
                 the one thing it may not do. Under budget is also a finding: the \
                 residual freedom the graph leaves is going unused, so either the \
                 budget is wrong or the method is more rigid than it claims.",
                l.dof
            );
        }
    }
}
```

Create `windows/vessel/src/lattice/occupancy.rs`:

```rust
//! §7 rule 5 — at most one creature per cell — as a TYPE rather than as a test
//! over data that does not exist yet.
//!
//! No creature stands in a cell until The Sighting, so a test asserting the rule
//! over today's lattices would pass without examining anything. A vacuous test is
//! worse than a missing one: it reads as coverage. So the rule is enforced by the
//! only structure that can hold an occupant — one keyed by cell, whose placement
//! REFUSES rather than overwrites.
//!
//! Refuses rather than overwrites deliberately: silently displacing whoever was
//! there is how two creatures come to believe they hold one cell.

use super::Cell;
use hornvale_kernel::EntityId;
use std::collections::BTreeMap;

/// Who stands where. `FRAME`-tier like the lattice itself (decision 0069) —
/// derived on entry, discarded on exit, never serialized.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Occupancy(BTreeMap<Cell, EntityId>);

impl Occupancy {
    /// Place `who` at `cell`, or refuse with whoever already holds it.
    pub fn place(&mut self, cell: Cell, who: EntityId) -> Result<(), EntityId> {
        match self.0.get(&cell) {
            Some(&held) if held != who => Err(held),
            _ => {
                self.0.insert(cell, who);
                Ok(())
            }
        }
    }

    /// Who stands at `cell`, if anyone.
    pub fn at(&self, cell: Cell) -> Option<EntityId> {
        self.0.get(&cell).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `EntityId` wraps a `NonZeroU64` (`kernel/src/ledger.rs:14`), so an id is
    /// built rather than written as a literal.
    fn id(n: u64) -> EntityId {
        EntityId(std::num::NonZeroU64::new(n).expect("nonzero"))
    }

    #[test]
    fn rule_5_a_cell_holds_at_most_one_creature() {
        let mut o = Occupancy::default();
        let (a, b) = (id(1), id(2));
        assert!(o.place(Cell(3, 3), a).is_ok());
        assert_eq!(
            o.place(Cell(3, 3), b),
            Err(a),
            "the second creature must be refused, and told who holds the cell"
        );
        assert_eq!(o.at(Cell(3, 3)), Some(a), "the refusal must not displace");
        assert!(
            o.place(Cell(3, 3), a).is_ok(),
            "placing the same creature where it already stands is not a conflict"
        );
    }
}
```

Register both in `mod.rs`:

```rust
pub mod classify;
pub mod occupancy;

pub use classify::{freedom_of_a_chain, openings, realized_links, region_of};
pub use occupancy::Occupancy;
```

- [ ] **Step 3: Run to verify failure**

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -20`
Expected: FAIL to compile — `Lattice` has no field `dof` until Step 1's field is threaded through both embedders. If it compiles, Step 1 was skipped.

- [ ] **Step 4: Make them pass, and treat a failure as a finding**

Run: `cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -20`
Expected: PASS, 21 tests (Task 2's fourteen, plus six rules in `classify` and one in `occupancy`). Task 1 landed **eleven** lattice tests, not the ten its own Step 4 predicted — its Step 5 added the cost check without revising the count. Trust the run, not the arithmetic.

**If rule 1, 2, 3 or 4 fails, do not adjust the rule.** These are the campaign's correctness claims; a failure means Task 1 or Task 2's embedder is wrong, and the fix belongs in the embedder. Report which rule failed and what the embedder was doing, then fix the embedder.

Two failures are predicted, and both are in the embedder rather than in the rule:

- **Rule 1's second direction** (no invented relation). `walls_between` exempts a doorway cell *entirely* — every pair touching it is left unwalled — so a doorway sitting where three regions meet opens a way to the third one, which no link specified. The fix is to exempt only the pair that crosses the boundary the doorway serves, not every pair at that cell.
- **Rule 3** (no unaccounted opening), for the mirror reason: `walls_between` derives walls from region boundaries while `shared_edge_cell` picks doorways independently, so a doorway on a boundary the wall pass did not exempt reads as a walled door, and a boundary pair that is neither reads as a hole.

Both are the same underlying defect — two passes deciding independently what a boundary is — and the honest fix is to make the doorway choice an input to the wall derivation rather than a parallel computation over the same geometry.

**Rule 7 is an `assert_eq!`, not an upper bound, and that is deliberate.** Over budget means the embedder is inventing. *Under* budget is also a finding — the graph's residual freedom is going unused, so either the budget is wrong or the method is more rigid than it claims. If it fails, report the two numbers and which method, and change the code or the budget with a stated reason. Do not relax it to `<=`: a bound that is only ever an upper one stops being a measurement of the discipline and becomes a smoke test.

- [ ] **Step 5: Format, audit, drift, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -6
bash scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

The drift check **must be clean** — nothing calls the embedder yet, and this task adds no label.

```bash
git add windows/vessel/src/lattice/ docs/audits/type-audit-report.md
git commit -m "feat(vessel): the seven rules, against a real lattice

Reads adjacency back OFF the solved geometry rather than trusting the code
path that wrote it, which is what makes the embedder falsifiable: allocate
claims to realize the anchor graph by construction, and this does not
believe it.

Rule 7 is the one that matters most -- the embedder now reports how many
independent choices it made, compared against what the graph leaves free.
That turns 'adds no information beyond the residual DOF' from an
aspiration into an integer comparison.

Rule 5 is a TYPE, not a test: no creature stands in a cell until The
Sighting, so an assertion over today's lattices would pass without
examining anything, and a vacuous test reads as coverage. Occupancy
refuses a second placement instead of displacing the first."
```

---

### Task 4: The ASCII render, the verb it hangs on, and the parity test

> **DONE.** Four corrections found in execution, recorded here because Tasks 5–8
> read this section:
>
> 1. **The plan's render draws no `#`, ever.** Its rule — a cell is wall when
>    every way out of it is walled — cannot fire: `MIN_CHAMBER_SPAN` is 2, so
>    every region is at least 2x2 and every cell has a same-region neighbour no
>    wall separates. A wall is a property of the BOUNDARY between two cells and a
>    1:1 grid has nowhere to draw it, so the render is doubled:
>    `(2w+1) x (2h+1)`, odd positions cells, even positions boundaries. Seed 42's
>    two-chamber plan is 33x17.
> 2. **`the_largest_plan_fits_a_terminal` (Task 1) was a proxy for a render that
>    did not exist**, and its 1:1-plus-a-border assumption is wrong by a factor
>    of two. Re-founded: `mod.rs`'s copy now asserts `2w + 1 <= 80` and
>    `render::tests::the_widest_plan_fits_a_terminal` asserts it on the drawn
>    picture. WIDTH is the hard bound (past 80 a transcript wraps); height is not
>    (a tall plan scrolls), so the 4-chamber worst case is 33x33 and that is
>    accepted rather than forcing `CHAMBER_SIDE` down.
> 3. **`every_destination_the_plan_depicts_is_command_reachable` compared the
>    wrong two numbers.** The plan draws every doorway of the WHOLE structure but
>    the footer names at most two ways out of ONE chamber, so at four chambers it
>    is `3 <= 2` — false. It passes at two chambers by luck. Rewritten to WALK the
>    structure and assert one drawn `+` per aperture actually stepped through,
>    which is what §6 asks for.
> 4. **`plan.matches('+')` counts the legend line.** The legend spells
>    `+ a doorway`, so glyph counts must be taken over the picture rows only. This
>    fired on the first run.
>
> `examine` indoors resolves against the chamber's anchors AND the plan's legend
> nouns: `the floor` and `a wall` answer (`chamber_prose::glyph_detail`), because
> a legend that names them while `examine` denies them is The Lintel's defect
> wearing a floor plan. `render` takes the lattice ALONE — `structure`/`at` were
> dropped under the plan's own licence, since a "you are here" mark is a CELL
> position and Task 5 is what creates one; the session's `[plan: chamber N, i of
> n]` header names where you stand instead.

**Files:**
- Create: `windows/vessel/src/lattice/render.rs`, `windows/vessel/tests/the_blocking.rs`
- Modify: `windows/vessel/src/lattice/mod.rs` (re-export), `windows/vessel/src/session.rs` (`map` becomes band-aware; the indoor `examine` refusal is retired), `windows/vessel/src/chamber_prose.rs` (a detail per noun), `scripts/possession-walk.txt`

**Interfaces:**
- Produces: `render(&Lattice, &Structure, at: usize) -> Plan`, where `Plan { picture: String, legend: Vec<(char, &'static str)> }`. The legend is not decoration — it is what the parity test walks.

**Budget note carried forward from Task 3:** one `allocate` now costs ~27.6 µs release / ~209 µs debug, up 3.3× from Task 1's 6.79 µs because `owner` adds 256 `BTreeMap` inserts. Still inside the 1000 µs ceiling but with ~5× headroom rather than ~16×, so a render that re-derives the lattice per cell, or anything that raises `CHAMBER_SIDE`, needs measuring rather than assuming. `lattice_here()` derives **once per call**, not per cell.

**The verb is `map`, and it already exists.** `session.rs:613` dispatches `"map" => self.map(rest)`, which draws the locale chart. Indoors it draws the floor plan instead. That is the same band-awareness `look` already has (`"look" if self.inside.is_some()`), and it is why no new verb is invented: §6's contract is that every pane capability must first be a verb, so the fewer verbs that mean one thing each, the better. `map out [N]` indoors refuses — a plan has no coarser rung, and the refusal names the verb that fixes it (`out`).

**Two things this task must reverse, both byte-pinned:**

1. `INDOOR_EXAMINE_REFUSAL` (`session.rs:50`) — "nothing here rewards a closer look yet." The parity test *requires* every depicted noun to be `examine`-able, so this refusal must go, which means authoring a detail line per `AnchorKind`. Its own doc comment says "Authoring real chamber detail is a later campaign's work"; this is that campaign. Its assertion at `session.rs:2145` moves with it.
2. Nothing else. `INDOOR_LATERAL_REFUSAL` is **Task 5's** business — do not touch it here, so that each reversal lands with the capability that justifies it.

- [x] **Step 0: Make rule 3 non-vacuous, before building on it**

Task 3 established that rule 3 is **tautological as written**: it asserts that every unwalled cross-region pair touches a door cell, which is the contrapositive of `walls_around`'s own exemption condition evaluated over the same ownership map. It therefore checks the wall derivation's *self-consistency*, not closure independently, and it passed without ever being able to fail.

That is honest but thin, and this task is the first one to build a *picture* on top of the wall set. Add a **negative control**: a hand-authored `Lattice` whose `walls` omits one boundary pair that its `owner` map says is a boundary, asserted to **fail** rule 3's condition. Same posture as Task 1's implementer deleting a type-audit tag to prove the tool reads the file.

```rust
    #[test]
    fn rule_3_actually_fails_on_an_unclosed_lattice() {
        // Rule 3 passes on every derived lattice by construction. A rule that
        // cannot fail is not checking anything, so prove it can: take a real
        // lattice, delete one wall, and assert the closure check catches it.
        let (_, mut l) = /* any two-region lattice from the corpus */;
        let victim = *l.walls.iter().next().expect("a two-region plan has walls");
        l.walls.remove(&victim);
        let doors: BTreeSet<Cell> = l.doorways.iter().map(|&(_, _, c)| c).collect();
        let leak = openings(&l).into_iter().find(|&(a, b)| {
            region_of(&l, a) != region_of(&l, b) && !doors.contains(&a) && !doors.contains(&b)
        });
        assert!(
            leak.is_some(),
            "removing a wall did not produce an unaccounted opening, so rule 3              cannot detect one either"
        );
    }
```

Record in rule 3's own doc comment what would make it load-bearing rather than self-consistent: a `walls` set written by anything other than `walls_around` — the spec's predicted radial and branching methods, or a hand-authored fixture. Then it is a real check waiting for a second writer, which is a different thing from a check that passed.

- [ ] **Step 1: Write the failing tests**

Create `windows/vessel/tests/the_blocking.rs` — an integration test, because the parity claim is about the SESSION, not about the renderer:

```rust
//! The Blocking's observable end: a floor plan a player can read, every noun on
//! it examinable, every destination on it reachable by a command.
//!
//! This is `the_purview.rs`'s parity test one band down. That file proved map and
//! prose are two grains of one lens at the locale; this proves the same of the
//! floor plan and the chamber, which is the pane The Panes will later draw with
//! pixels. If it fails, they are two pipelines wearing one name.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

/// Walk the seed-42 possession into the structure the gallery enters.
///
/// `enter` bare is the same step the committed transcript takes
/// (`book/src/gallery/possession-seed-42.md`), so if this helper stops finding a
/// chamber the gallery has stopped showing one too — which is the defect The
/// Lintel shipped and followup 15 named. Panics loudly rather than skipping:
/// a parity test that silently tests nothing is worse than a red one.
fn inside(session: &mut Session) {
    let reply = out(session.handle("enter"));
    assert!(
        reply.starts_with("[chamber "),
        "the possession did not get indoors, so nothing below is tested: {reply}"
    );
}

#[test]
fn map_indoors_draws_a_floor_plan() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    // The picture must be a picture: several lines, made of the plan's own
    // alphabet, and wide enough to hold a room.
    let lines: Vec<&str> = plan
        .lines()
        .filter(|l| l.chars().all(|c| "#.+ ".contains(c)) && l.len() > 2)
        .collect();
    assert!(
        lines.len() >= 4,
        "a floor plan needs more than a few rows to be a plan: {plan}"
    );
    assert!(
        plan.contains('#') && plan.contains('.'),
        "a plan with no wall or no floor is not a plan: {plan}"
    );
    assert!(
        plan.contains('+'),
        "the seed-42 structure has two chambers, so its plan must show a doorway: {plan}"
    );
}

#[test]
fn map_outdoors_still_draws_the_chart() {
    // The band-awareness must not have eaten the locale chart.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let chart = out(session.handle("map"));
    assert!(
        !chart.is_empty() && !chart.starts_with("[chamber "),
        "outdoors, `map` must still draw the locale chart: {chart}"
    );
}

#[test]
fn every_noun_the_plan_depicts_is_examinable() {
    // The parity contract's tested half (spec §6), generalizing
    // `the_purview.rs::examine_accepts_exactly_the_union_of_both_grains`.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let nouns = session.plan_legend_nouns();
    assert!(
        !nouns.is_empty(),
        "a plan whose legend names nothing cannot be checked, and a player cannot \
         read it either"
    );
    for noun in &nouns {
        let reply = out(session.handle(&format!("examine {noun}")));
        assert!(
            !reply.starts_with("You see no"),
            "'{noun}' is drawn on the plan but examine refused it: {reply}"
        );
        assert!(!reply.is_empty(), "'{noun}' resolved to nothing");
    }
    let refused = out(session.handle("examine a-noun-no-grain-surfaced"));
    assert!(
        refused.starts_with("You see no"),
        "examine must still refuse what nothing depicts, or it accepts everything \
         and the test above proves nothing: {refused}"
    );
}

#[test]
fn every_destination_the_plan_depicts_is_command_reachable() {
    // A doorway drawn is a promise. The plan draws '+' for each doorway, and the
    // footer names the aperture; the two must not disagree about how many ways
    // out of this chamber exist.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let plan = out(session.handle("map"));
    let drawn = plan.matches('+').count();
    let named = out(session.handle("look"))
        .lines()
        .find(|l| l.starts_with("Ways on:"))
        .map(|l| l.matches(',').count() + 1)
        .expect("a chamber rendering always names its ways");
    assert!(
        drawn <= named,
        "the plan draws {drawn} doorways but the footer names {named} ways: a \
         drawn destination no command reaches is the defect this test exists for"
    );
}
```

`plan_legend_nouns()` is a new `pub fn` on `Session`: the legend's nouns for the chamber the possession stands in. It exists so the test reads the same structure the render does rather than re-parsing the picture — `the_purview.rs` does the same thing with `focalized().nouns` and `purview(0).legend`.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test the_blocking 2>&1 | tail -20`
Expected: FAIL to compile — no `plan_legend_nouns`, no `render`.

- [x] **Step 3: Implement the render**

Create `windows/vessel/src/lattice/render.rs`. The alphabet is deliberately tiny — `#` wall, `.` floor, `+` doorway, and the standing chamber marked in the legend rather than by a fourth glyph:

```rust
//! ASCII, and the legend that makes it checkable.
//!
//! The legend is the deliverable as much as the picture is. §6's parity contract
//! is tested by walking what the render CLAIMS to depict and demanding that
//! `examine` accept each of it — so a render that draws a thing without naming it
//! in the legend is a render the parity test cannot check, which is how The
//! Lintel's `look`-named-but-`examine`-denied jar shipped.
//!
//! Walls are drawn from `Lattice::walls`, never inferred from region boundaries a
//! second time. A wall drawn by independent arithmetic is exactly how a picture
//! comes to disagree with the world it depicts (§7 rule 2) — and Task 3 found
//! exactly that defect twice, in two passes deciding independently what a boundary
//! was, so this is not a hypothetical.
//!
//! Which chamber a cell belongs to comes from `Lattice::owner`, NEVER from
//! `regions`: a grown lattice's rects are overlapping bounding boxes, so a rect
//! scan answers a different question and happens to agree only for rectilinear
//! plans (ledger #17).

use super::{Cell, Lattice};
use crate::structure::Structure;

/// A drawn plan: the picture, and what each glyph means.
/// type-audit: bare-ok(prose: picture), bare-ok(identifier-text: legend)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Plan {
    /// The picture, one line per lattice row.
    pub picture: String,
    /// Glyph and its meaning, in drawing order. What the parity test walks.
    pub legend: Vec<(char, &'static str)>,
}

/// Draw `lattice`, marking the chamber at index `at` as the one stood in.
/// type-audit: bare-ok(index: at)
pub fn render(lattice: &Lattice, structure: &Structure, at: usize) -> Plan {
    let e = lattice.extent;
    let doors: std::collections::BTreeSet<Cell> =
        lattice.doorways.iter().map(|&(_, _, c)| c).collect();
    let mut picture = String::new();
    for cy in e.y..(e.y + e.h) {
        for cx in e.x..(e.x + e.w) {
            let here = Cell(cx, cy);
            // A cell is drawn as wall when every way out of it is walled: the
            // wall set is the authority, and this reads it rather than
            // recomputing boundaries.
            let walled_in = [(1, 0), (-1, 0), (0, 1), (0, -1)].iter().all(|&(dx, dy)| {
                let there = Cell(cx + dx, cy + dy);
                !e.contains(there) || {
                    let pair = (here.min(there), here.max(there));
                    lattice.walls.contains(&pair)
                }
            });
            picture.push(if doors.contains(&here) {
                '+'
            } else if walled_in {
                '#'
            } else {
                '.'
            });
        }
        picture.push('\n');
    }
    let mut legend = vec![('.', "the floor"), ('#', "a wall")];
    if !lattice.doorways.is_empty() {
        legend.push(('+', "a doorway"));
    }
    let _ = (structure, at);
    Plan { picture, legend }
}
```

**On `structure` and `at` being unused in v1:** they are in the signature because Task 6's roles give each region a name to put in the legend, and changing a signature that three call sites use is churn this plan can avoid by paying one `let _ =` now. If the implementer prefers, drop both parameters and let Task 6 add them — say which you did in your report; either is fine, but do not leave an unexplained `let _ =`.

- [x] **Step 4: Wire the verb, retire the examine refusal**

`lib.rs` re-exports `allocate` and `extent_for` but not `grow`/`embed_with` (Task 2's file list excluded `lib.rs`). This task is the first caller of the selector, so promote it: add `embed_with` to the `pub use lattice::{...}` list.

First, the helper both this task and Task 5 need — and the one detail in it that can produce a plausible, self-consistent, uniformly wrong world:

```rust
    /// The floor plan of the structure being stood in, or `None` out of doors.
    ///
    /// Derived on demand and kept by nobody: the lattice is `FRAME`-tier
    /// (decision 0069), so re-deriving it costs microseconds and holding it
    /// across a descent boundary would be how derived state stops being derived.
    fn lattice_here(&self) -> Option<crate::lattice::Lattice> { /* ... */ }
```

**The seed must be the LOCALE's, not the world's.** `structure_at` keys its draw with `locale.seed(seed)` (`structure.rs:80`) precisely so no other locale's draw can perturb it. The lattice must be keyed the same way:

```rust
        let locale = crate::band::truncate_to_walk(chamber, self.walk_depth());
        let l = crate::lattice::embed_with(&structure, &brief, extent_for(&structure), locale.seed(world_seed));
```

Keyed to the world's seed instead, **every building in the world gets one identical floor plan** — a world that is self-consistent, passes every test written so far, and is uniformly wrong. Add a test that two different built locales derive different plans.

Then, in `session.rs`:

```rust
            // `map` is band-aware for exactly the reason `look` is: indoors the
            // chart would draw the LOCALE the structure sits in, which is not
            // where the possession is standing. A plan has no coarser rung, so
            // an argument indoors is refused rather than silently ignored.
            "map" if self.inside.is_some() && rest.is_empty() => self.out(self.plan_here()),
            "map" if self.inside.is_some() => Turn::Out(INDOOR_CHART_REFUSAL.to_string()),
            "map" => self.map(rest),
```

and `examine` indoors resolves against the chamber's own nouns instead of refusing:

```rust
            "examine" if self.inside.is_some() && !rest.is_empty() => {
                Turn::Out(self.examine_chamber(rest))
            }
```

`examine_chamber` resolves `rest` against `chamber_nouns` (the catalogue `enter <named>` already shares) and answers with the noun's authored detail, falling back to the same `"You see no {rest} here."` shape the outdoor path uses — **the same prefix**, because the parity test asserts on it and two different refusal wordings for one question is the drift §6 exists to prevent.

Declare `INDOOR_CHART_REFUSAL` next to the two existing refusal constants (`session.rs:45-60`), in their voice and with their kind of comment — say *why* a plan has no coarser rung, and name the verb that fixes it:

```rust
/// What `map out [N]` says INDOORS. A plan is one building, so there is no
/// coarser rung of it to draw; the chart the player wants is the LAND, which is
/// out of doors. Names the verb rather than refusing blankly.
const INDOOR_CHART_REFUSAL: &str =
    "Inside, the chart is the floor you stand on; step 'out' to read the land.";
```

`INDOOR_EXAMINE_REFUSAL` is deleted in this task, so remove the constant too rather than leaving a dead one — and check for other references first (`grep -n INDOOR_EXAMINE_REFUSAL windows/vessel/src/session.rs` finds the declaration, the arm, and the test at line 2145).

Author one detail line per `AnchorKind` in `chamber_prose.rs`, as an **exhaustive match** — the guard The Lintel built deliberately, and Task 6 will make it fire again:

```rust
/// One authored line per kind: what a closer look at this thing gives you.
///
/// Exhaustive on purpose. A new `AnchorKind` fails to compile here until someone
/// writes what it looks like, which is the guard that stopped `look` and `examine`
/// disagreeing in The Lintel.
pub(crate) fn detail(kind: AnchorKind) -> &'static str {
    match kind {
        AnchorKind::Ground => "Trodden floor, swept toward the walls.",
        // ... one line per kind; keep them short, concrete, and free of terrain
        // words — `a_chamber_never_speaks_of_terrain` is already a test.
    }
}
```

Add `map` (indoors) and one `examine` of a chamber noun to `scripts/possession-walk.txt`, immediately after the existing `enter further in` at line 7, so the transcript shows the plan in the chamber the walk already reaches:

```
enter further in
map
examine a water jar
```

- [x] **Step 5: Run to verify pass**

```bash
cargo test -p hornvale-vessel --test the_blocking 2>&1 | tail -20
cargo test -p hornvale-vessel 2>&1 | tail -12
```
Expected: PASS, 4 new integration tests. The old `INDOOR_EXAMINE_REFUSAL` assertion (`session.rs:2145`) will fail until it is rewritten to assert the new behaviour — rewrite it to assert that an examine of a noun the chamber names is *accepted*, and that an unknown noun is refused with the outdoor wording. **Do not delete it.**

If `examine a water jar` refuses in the transcript, the noun the walk asks for is not one this chamber holds — read the committed transcript's own prose line and ask for a noun that appears in it. Do not weaken the test; fix the script.

- [x] **Step 6: Format, audit, inspect the drift, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -6
bash scripts/regenerate-artifacts.sh
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

**The gallery WILL move, and that is this task's deliverable, not its accident.** Two checks, in this order:

```bash
git diff book/src/gallery/possession-seed-42.md | head -60
git diff --exit-code book/src/laboratory/
```

1. The transcript must **contain a floor plan** — glyphs, several rows, a `+`. A transcript that merely regenerated without error is followup 15's exact failure: green checks over an unmet headline. Paste the drawn plan into your report.
2. `book/src/laboratory/` must be **clean**. A metric golden moving in this task means the render or the examine path reached derived state it has no business touching — stop and report rather than re-pinning.

```bash
git add windows/vessel/src/lattice/render.rs windows/vessel/src/lattice/mod.rs \
        windows/vessel/src/session.rs windows/vessel/src/chamber_prose.rs \
        windows/vessel/tests/the_blocking.rs scripts/possession-walk.txt \
        book/src/gallery/
git commit -m "feat(vessel): a floor plan you can read, and every noun on it answers

map becomes band-aware exactly as look already is: indoors it draws the
plan, out of doors the chart. No new verb, because §6's contract is that a
pane capability must first BE a verb, and the fewer verbs meaning one thing
each the better.

Retires INDOOR_EXAMINE_REFUSAL, whose own doc comment deferred authored
chamber detail to a later campaign -- this is it. The parity test walks
what the render CLAIMS to depict and demands examine accept each of it,
which is The Lintel's look-named-but-examine-denied jar turned into a
standing check one band down.

The gallery transcript moves by construction: the walk script now asks for
the plan. The laboratory goldens do not."
```

---

### Task 4b: The reification — a wall is a cell

**Files:**
- Modify: `windows/vessel/src/lattice/mod.rs` (`CellKind`, `cells`, `extent_for`, delete `regions`), `allocate.rs`, `grow.rs`, `classify.rs`, `render.rs`, `occupancy.rs`, `windows/vessel/src/session.rs`, `windows/vessel/tests/the_blocking.rs`
- Re-pin: `book/src/gallery/possession-seed-42.md` (the drawn plan changes shape)

**Interfaces:**
- Produces: `CellKind`, `Lattice.cells: BTreeMap<Cell, CellKind>`, `kind_of`, `bounds_of`.
- Removes: `Lattice.regions`, `Lattice.owner`, `Lattice.walls` as a set of *pairs*.

**Why this task exists, and why it is not a rewrite of Tasks 1–4.** Nathan's call, 2026-07-28: a wall should be a **cell that is occupied**, not a property of the boundary between two cells. The chronicle must record that the model changed and what it bought, rather than pretending it was always this way — which is why this is its own task rather than an amendment to Task 1.

What it buys, in the order the arguments actually landed:

1. **The picture is 1:1 again.** Task 4 had to double the render to `(2w+1) x (2h+1)` because a 1:1 grid has nowhere to draw a boundary. Walls-as-cells deletes the doubling, the coordinate mapping, and its whole off-by-one class — which Task 5 was about to inherit.
2. **It is the model every roguelike and every tilemap engine already speaks**, so The Panes inherits the standard rather than a translation layer, and The Sighting's shadowcast gets blocking *cells*, which is what its measured timings assumed.
3. **Wall thickness is more accurate, not less.** A cell is roughly a metre here, and this world models neolithic through classical: turf, cob and rubble-stone walls genuinely run 0.5–2 m. A zero-thickness wall was the less faithful choice.
4. **Two anchor kinds that already ship gain a place.** `Screen` ("affords nothing, shapes sightlines") is a partition; `Alcove` ("a recess off the main space") is *literally a passable wall cell*. Under the boundary model neither had a location. And `the-fire` attaching `Within(Alcove)` has been describing **a fireplace** since The Hearth with no geometry to make it legible.
5. **A threshold becomes a place**, so it can later hold a door, be barred, or be blocked by rubble.
6. **Rule 3 stops being tautological — today, not eventually.** See Step 3.

**No label bump.** `room/layout/v1/rectilinear` and `/grown` were declared *in this campaign* and nothing on `main` draws from them, so v1 is still being authored rather than re-versioned (ledger #23). The gallery re-pins, which Tasks 4–5 already expect.

- [x] **Step 1: The type, and the invariant it re-founds**

```rust
/// What occupies one cell of the lattice.
///
/// **Closed at three variants on purpose.** The moment this enum becomes the place
/// where richness lives, the lattice is a tile catalogue and
/// `CLIENT-language-not-catalogue` has been violated one band down. A window is an
/// ANCHOR at a wall cell, never `CellKind::Window`. The only variants that should
/// ever join these three are states a cell can *transition into over time*
/// (`Rubble`, `Barred`), and neither is this campaign's business.
/// type-audit: bare-ok(index: Floor), bare-ok(index: Threshold)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CellKind {
    /// Standing room, owned by exactly one chamber.
    Floor(usize),
    /// The building's fabric. Impassable, and a place in its own right — an
    /// alcove, a screen or a fireplace is an anchor AT one of these.
    Wall,
    /// A designed opening between two chambers. Distinct from a future breach,
    /// which is damage: a threshold derives from a link in the anchor graph, and
    /// a breach never will.
    Threshold(usize, usize),
}

impl CellKind {
    /// May a mover pass through this cell?
    ///
    /// **Every rule in `classify` asks this, never `== CellKind::Wall`.** A rule
    /// written against the variant breaks the day `Rubble` arrives; a rule written
    /// against the predicate survives it.
    /// type-audit: bare-ok(flag: return)
    pub fn passable(&self) -> bool { !matches!(self, CellKind::Wall) }

    /// Does this cell serve `chamber` — as its floor, or as one side of its door?
    /// type-audit: bare-ok(index: chamber), bare-ok(flag: return)
    pub fn serves(&self, chamber: usize) -> bool { /* ... */ }
}
```

On `Lattice`, `cells` **replaces** `owner`, `regions` and the pair-valued `walls`:

```rust
    /// Every cell of `extent`, with its kind. TOTAL: every cell appears exactly
    /// once, so `kind_of` returning `None` means "outside the extent" and NOTHING
    /// else.
    ///
    /// That totality is the point. A partial map with walls simply absent would
    /// make `None` mean "outside the extent OR is a wall" — two distinct facts in
    /// one value, which is the exact shape of the rect-scan defect Task 2 found
    /// (ledger #17). Wall-ness is a positive fact here, so every rule that
    /// compares two cells must say out loud what it means about walls.
    /// type-audit: bare-ok(index: cells)
    pub cells: BTreeMap<Cell, CellKind>,
```

**Delete `regions`.** It has been a trap twice — grown rects overlap, and a rect-scanning `region_of` agreed with the truth for exactly one method — and under this model it is ambiguous besides (does a region's rect include its wall ring?). Replace with `bounds_of(&Lattice, chamber) -> Option<Rect>`, derived from `cells` in one pass, which is what Task 6's legend actually needs.

**The extent grows to hold the fabric:** `cols * CHAMBER_SIDE + (cols + 1)` per axis — the wall lines are one more than the interiors they separate. That gives 10x10 at one chamber, 19x10 at two, 19x19 at four, and the drawn picture is now the same size, so every chamber count fits an 80x24 terminal and **ledger #22's height relaxation becomes moot**. Amend `CHAMBER_SIDE`'s doc comment accordingly: the ceiling is no longer half the terminal.

Update spec §3.4's wording too — the plan is now as big as "the rooms it must hold **plus the fabric between them**", and roughly 20% of the extent is the exterior shell. That is a deliberate cost: it is what makes the picture read as a building rather than a floating partition diagram.

- [x] **Step 2: Restate the rules, and add the one this model needs**

The seven survive, three of them in better form:

```
  2  wall law    WAS: every wall pair is a non-adjacency.
                 NOW: two Floor cells of DIFFERENT chambers are never adjacent.
                 A cleaner claim, and a property of the kind map rather than of a
                 separately-derived pair set.
  1  soundness   a link (a,b) is realized iff some Threshold(a,b) cell is adjacent
                 to both a Floor(a) and a Floor(b). Converse: every Threshold(a,b)
                 must appear in `links`, or the embedder invented a relation.
  3  closure     NO LONGER TAUTOLOGICAL. Two independent assertions:
                 (i) the extent's outer ring is entirely Wall -- the plan is
                     ENCLOSED, which the embedder could fail to do and which the
                     boundary model had nothing to say about;
                 (ii) every passable cell is Floor or Threshold, and every
                      Threshold is in `doorways`.
  5  occupancy   gains a companion that was previously meaningless: a creature
                 cannot be placed in an impassable cell.
  7  DOF         unchanged. One cut per split, so rectilinear still spends n-1.
```

**And a new rule, because this model introduces a new failure mode:**

```
  8  reachability  every Floor cell is reachable from the threshold chamber,
                   through passable cells only.
```

Under the boundary model connectivity was guaranteed by construction — regions tiled and doorways linked. **Walls-as-cells can seal a pocket of floor**, and the grower is where it will happen: carving a wall between two blobs can split a concave blob, stranding its far half. This is the mirror of the unclaimed-cell defect Task 2 found, and it is not optional. Name it rule 8 in the code and note in the spec that Amendment 2 §1b.8 listed seven; this is the eighth, earned by the model change.

- [x] **Step 3: Rework the two embedders**

**`allocate`** — chain-split the *interior* (the extent shrunk by one on every side), where each split consumes one cell for its wall line: splitting a span `L` into `a` and `b` now means `a + 1 + b == L`. Then every cell on a split line and every cell of the outer ring is `Wall`, and one cell per split line becomes `Threshold(i, j)`. `MIN_CHAMBER_SPAN` still governs *interiors*. DOF is unchanged at one cut per split, so rule 7 must still pass at `{0,1,2,3}` — if it does not, the rework changed how many draws are spent and that needs saying, not absorbing.

**`grow` — claim with a separation rule, and never take a cell back.** A cell is claimable only if it has no neighbour owned by a *different* chamber; leftover unclaimed interior cells become `Wall`. This is deliberately not "grow then carve": nothing is ever removed from a blob, so **blobs are connected by construction** and rule 8 holds by the same argument rather than by luck. Task 3's tunnelling fix must be adjusted to match — seed chamber `i+1` **two** cells from chamber `i` rather than adjacent, so exactly one wall cell sits between them and is available to carve into a threshold. Keep the FIFO frontier; a depth-first tendril was a real defect.

- [x] **Step 4: Simplify the render**

1:1. Delete the `(2w+1)` machinery and the coordinate mapping. `Floor` → `.`, `Wall` → `#`, `Threshold` → `+`. Task 4's picture-readback test gets *simpler*, and it should still read every glyph back and assert it against `cells`. Keep the legend and keep every legend noun `examine`-able — the parity contract does not change.

Confirm the width assertion now reads `extent.w <= 80` rather than `2w + 1 <= 80`, in both places Task 4 put it.

- [x] **Step 5: Run everything, then read the transcript**

```bash
cargo test -p hornvale-vessel --lib lattice:: 2>&1 | tail -14
cargo test -p hornvale-vessel --test the_blocking 2>&1 | tail -14
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -6
cargo test -p hornvale 2>&1 | tail -6
bash scripts/regenerate-artifacts.sh
git diff book/src/gallery/possession-seed-42.md
git diff --exit-code book/src/laboratory/ book/src/reference/
```

**The gallery re-pins and the plan changes shape** — it should now be a 19x10 picture with a visible exterior wall and one doorway in the dividing wall. **Paste it.** `book/src/laboratory/` and `book/src/reference/` must be clean.

```bash
git add windows/vessel/ book/src/gallery/ docs/audits/type-audit-report.md docs/superpowers/specs/
git commit -m "refactor(vessel): a wall is a cell, not a boundary

Nathan's call. A wall occupies a cell rather than sitting on the boundary
between two, which is the model every roguelike and every tilemap engine
already speaks -- so the picture is 1:1 again, the (2w+1) coordinate mapping
and its off-by-one class are gone, and The Sighting gets blocking CELLS,
which is what its measured shadowcast timings assumed.

The thickness this concedes is more accurate, not less: a cell is about a
metre and this world models turf, cob and rubble-stone building, where walls
genuinely run half a metre to two.

Two anchor kinds that already shipped gain a place. A screen is a partition;
an alcove is literally a passable wall cell. And the-fire attaching
Within(Alcove) has been describing a FIREPLACE since The Hearth with no
geometry to make it legible.

CellKind is closed at three variants deliberately -- a window is an anchor
at a wall cell, never CellKind::Window, or the lattice becomes the tile
catalogue the pattern language forbids one band up. Rules ask passable(),
never == Wall, so they survive Rubble.

Rule 3 stops being tautological: the outer ring must be entirely Wall, which
the embedder could fail to do and the boundary model had nothing to say
about. And rule 8 is new, because this model can seal a pocket of floor --
so the grower claims with a separation rule and never takes a cell back,
making reachability true by construction rather than by luck."
```

---

### Task 5: Intra-chamber `go`, and the sentences that said it was impossible

**Files:**
- Modify: `windows/vessel/src/session.rs` (the cell position; `go` indoors), `scripts/possession-walk.txt`, `windows/vessel/tests/the_blocking.rs`
- Modify (documents): `book/src/chronicle/the-lintel.md`, `book/src/frontier/idea-registry.md` (`CLIENT-scale-bands`), and whichever of the two specs assert the refusal — find them, do not assume:

```bash
grep -rn 'refused indoors\|no north\|not a step at all' docs/ book/src --include=*.md
```

**The reversal is smaller than it looks, and getting that right matters more than the code.** The metaplan's §1b.6 law — *lateral movement never changes band* — **survives intact**, because a cell step stays inside the chamber band. What was wrong was the *inference* The Lintel drew from it, recorded in `INDOOR_LATERAL_REFUSAL`'s own doc comment: "a chamber address carries no bearing to walk along." That was true of a chamber with no interior geometry and is false of one with a lattice. So this task reverses **one refusal** and **clarifies** the documents that recorded its reasoning; it does not amend the law. Say so in each edit, because "the campaign reversed a band law" is the wrong history to leave behind.

`back` stays refused indoors. It retraces the *walk-band* trail, so it is a walk-band operation whatever the interior looks like — and un-refusing both at once would blur which capability justified which reversal.

- [ ] **Step 1: Write the failing tests**

Append to `windows/vessel/tests/the_blocking.rs`:

```rust
#[test]
fn go_indoors_moves_one_cell_and_says_where_you_are() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    // At least one of the four bearings must be walkable from wherever `enter`
    // lands, or the chamber has no interior to stand in — which
    // `no_region_is_degenerate` already forbids at the lattice level.
    let replies: Vec<String> = ["n", "s", "e", "w"]
        .iter()
        .map(|d| out(session.handle(&format!("go {d}"))))
        .collect();
    assert!(
        replies.iter().any(|r| !r.contains("wall")),
        "every bearing from the entry cell is walled: {replies:?}"
    );
    assert!(
        replies.iter().all(|r| !r.starts_with("[room ")),
        "a compass step indoors must not put the possession out of doors: {replies:?}"
    );
}

#[test]
fn a_wall_refuses_with_a_physical_reason() {
    // The Lintel's own standard, from its `enter` work: refuse with a reason
    // drawn from the world, never with a grammar complaint.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    // Walk one bearing until it refuses — a bounded plan always ends in a wall.
    let mut refusal = String::new();
    for _ in 0..64 {
        let reply = out(session.handle("go n"));
        if reply.contains("wall") {
            refusal = reply;
            break;
        }
    }
    assert!(
        !refusal.is_empty(),
        "walking one bearing 64 times never met a wall: the plan is unbounded"
    );
    assert!(
        !refusal.contains("no north"),
        "the refusal still claims there is no north indoors: {refusal}"
    );
}

#[test]
fn back_stays_refused_indoors() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let reply = out(session.handle("back"));
    assert!(
        reply.contains("Inside"),
        "`back` retraces a WALK-band trail and must still refuse indoors: {reply}"
    );
}

#[test]
fn walking_a_chamber_commits_nothing() {
    // Decision 0069: intra-chamber position is FRAME-tier. The ledger must not
    // grow because someone crossed a room.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let before = session.committed_fact_count();
    for _ in 0..8 {
        session.handle("go e");
    }
    assert_eq!(
        session.committed_fact_count(),
        before,
        "walking inside a chamber committed a fact: fine position is never \
         serialized (0069)"
    );
}
```

`committed_fact_count()` is a new `pub fn` on `Session` returning the session ledger's fact count. If an equivalent accessor already exists, use it — grep before adding one.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test the_blocking 2>&1 | tail -20`
Expected: FAIL — `go` indoors still answers `INDOOR_LATERAL_REFUSAL`, so `a_wall_refuses_with_a_physical_reason` fails on the `no north` assertion.

**Two things Task 4 changed that this task must respect:**

1. **The picture is 1:1** — Task 4b reverted Task 4's `(2w+1)` doubling by making a wall a cell. Cell `(x, y)` is at picture `(x, y)`. A "you are here" mark goes straight at the cell, with no coordinate mapping to get wrong.
2. **`render` no longer takes `structure`/`at`.** Task 4 dropped them because a "you are here" mark is a *cell* position and nothing had one yet — marking a whole region would have claimed precision the session did not have. **This task is what creates that position**, so add the mark here:
   - a fourth glyph (`@`) at the standing cell, and a legend entry for it;
   - **the legend entry must be `examine`-able**, because `every_noun_the_plan_depicts_is_examinable` walks the legend and it is the parity contract. Resolve it to the session's existing self-description (`whoami`'s content) rather than authoring a second one — two descriptions of the possessed agent is exactly the drift §6 exists to prevent. If you conclude the mark should stay out of the legend, that is defensible, but then the picture depicts something it refuses to name, and you must say so.

- [ ] **Step 3: Give the possession a cell**

`self.inside` is `Option<(Structure, usize)>` at nine sites. **Promote it to a named struct** rather than a three-tuple — a bare `Cell` as a tuple's third element is where these call sites stop being readable:

```rust
/// Where the possession is while indoors. `FRAME`-tier in its entirety: derived
/// at `enter`, dropped at `leave`, never serialized (decision 0069). The lattice
/// is carried rather than re-derived per turn because it is a pure function of
/// the structure and the locale's seed — re-deriving it would be correct and
/// wasteful, while CACHING it would be neither if it outlived the descent, which
/// is why it dies with this struct.
struct Inside {
    /// The structure being stood in.
    structure: crate::structure::Structure,
    /// Which chamber, as an index into `structure.chambers`.
    /// type-audit: bare-ok(index: at)
    at: usize,
    /// The floor plan for the whole structure.
    lattice: crate::lattice::Lattice,
    /// Which cell of it the possession occupies.
    cell: crate::lattice::Cell,
}
```

**The seed the lattice is derived from must be the LOCALE's, not the world's.** `structure_at` keys its draw with `locale.seed(seed)` (`structure.rs:80`) precisely so no other locale's draw can perturb it; the lattice must be keyed the same way, or every structure in the world gets one identical floor plan. Pass `locale.seed(self.world_seed())` — the same expression the structure derivation uses — and add a test that two different built locales derive **different** plans. Getting this wrong produces a plausible, self-consistent, uniformly wrong world, which is the hardest kind of bug to see in a transcript.

`enter` sets `cell` to the doorway the possession came through, or the region's centre for the threshold chamber. `go <dir>` then:

1. translates the bearing to a cell delta (N is `-y`, matching the render's top-down rows);
2. refuses if the target cell is **not `passable()`**, or if it leaves the extent — never by matching `== CellKind::Wall`, so the refusal survives `Rubble`;
3. if the target cell is a doorway to another chamber, **moves chamber** — that is a `COMMIT`-tier band step in the same sense `enter` is, so it renders the new chamber, not a cell move;
4. otherwise updates `cell` and renders briefly — a cell step is not worth a full chamber description every time. Say what changed and what is now adjacent.

- [ ] **Step 4: Amend the documents, in the same commit as the code**

The reversal and its record land together — a code change whose documents lag is how the four-document sweep became necessary in the first place.

- `book/src/chronicle/the-lintel.md:107` — "apertures, and `go north` is refused indoors." Amend to say it *was* refused, that The Blocking reversed it, and **why that is not a flip-flop**: the refusal was correct for a chamber with no interior, and this campaign built the interior. One sentence, and name the campaign.
- `book/src/frontier/idea-registry.md`, `CLIENT-scale-bands` — the row says "Five band laws; 2 shipped". Update the count if this campaign ships another, and repoint **Where** at this campaign's chronicle entry. Rows are capped at **600 chars** and the cap is append-never: edit as an index entry, do not grow the row.
- The specs the grep finds — state that §1b.6's law is **unchanged** and that what changed is the inference drawn from it.

- [ ] **Step 5: Add it to the walk, run, inspect**

Add `go n` (or whichever bearing the entry cell can walk) to `scripts/possession-walk.txt` right after the `map` line Task 4 added, so the transcript shows a step *inside* a building. Then:

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test -p hornvale-vessel 2>&1 | tail -12
bash scripts/regenerate-artifacts.sh
git diff book/src/gallery/possession-seed-42.md | head -40
git diff --exit-code book/src/laboratory/
```

The transcript must show the step **succeeding**. If the chosen bearing walls immediately, pick another — and note in your report which one the entry cell can walk, because that is a fact about `extent_for` and the allocator worth having written down.

`book/src/laboratory/` must be clean: intra-chamber movement is `FRAME`-tier, so a metric golden moving here means something committed that should not have, and `walking_a_chamber_commits_nothing` should have caught it. Report rather than re-pin.

```bash
git add windows/vessel/src/session.rs windows/vessel/tests/the_blocking.rs \
        scripts/possession-walk.txt book/src/gallery/ book/src/chronicle/the-lintel.md \
        book/src/frontier/idea-registry.md docs/superpowers/specs/
git commit -m "feat(vessel): go, indoors, one cell at a time

Reverses The Lintel's indoor refusal, and the reversal is narrower than it
sounds: §1b.6's law -- lateral movement never changes band -- SURVIVES,
because a cell step stays inside the chamber band. What was wrong is the
inference the refusal recorded, that a chamber address carries no bearing
to walk along. True of a chamber with no interior; false of one with a
lattice, which is what this campaign built.

`back` stays refused: it retraces a WALK-band trail whatever the interior
looks like.

Position inside a chamber is FRAME-tier, so walking a room commits
nothing (0069) -- asserted, not asserted-to-be-obvious. The lattice is
keyed to the LOCALE's seed exactly as structure_at's draw is; keyed to the
world's it would have given every building in the world one identical
floor plan."
```

---

### Task 6: Differentiation — roles, and the epoch that may or may not be one

**Files:**
- Modify: `windows/vessel/src/interior/pattern.rs` (a `Role` on `Pattern`; `selection_for`), `windows/vessel/src/interior/anchor.rs` (new kinds), `windows/vessel/src/interior/derive.rs` (`chamber_interior_of` takes a role), `windows/vessel/src/brief.rs` (`peak_population`), `windows/vessel/src/chamber_prose.rs` (nouns and details for the new kinds), `windows/vessel/src/session.rs` (pass the role), `domains/history/src/flesh.rs` (hoist one constant), possibly `windows/vessel/src/streams.rs` (the bump)
- Modify: `book/src/gallery/` (re-pin, in an **isolated** commit)

**Read this before doing anything else in this task.**

The plan's earlier draft called this "THE EPOCH" and asserted that byte-identity breaks by design. **That is a prediction, and this task tests it** (ledger #10). Two facts, both verified by grep rather than reasoned:

1. **`ROOM_FURNISHING` has exactly one occurrence in the workspace — its own declaration.** Nothing draws from it. So `room/furnishing/v1 → v2` re-mints *nothing* on its own; by itself it would be an **empty** bump.
2. **A creature stands at a locale, not in a chamber.** NPC thermal drives read `interior_of` (`derive.rs:20`); `chamber_interior_of` is read only by `describe_chamber_here`. And `selection` iterates `INVENTORY` in order and filters (`pattern.rs:139-158`), so **appending** role-gated patterns leaves every existing `(built, cold)` selection byte-identical.

So the three outcomes in spec §5.2 are all reachable, and the response differs completely:

```
  RE-PIN   transcripts moved; no metric golden moved.   Not an epoch. Re-pin, no bump.
  EPOCH    a metric or census golden moved.             Health battery is the GATE.
  LATENT   nothing moved at all.                        Cheapest, and a TRAP -- see Step 6.
```

**Do not enter this task expecting drift, and do not read a green health battery as an epoch survived** when it may mean no epoch occurred. The bump follows the measurement.

**What must carry the headline** (ledger #12, verified from `book/src/gallery/possession-seed-42.md:30-37`): the seed-42 structure has **two chambers**, sits in **tropical seasonal forest** (so every `needs_cold` pattern is filtered out), and is **not a Seat**. Differentiation that lives in the hearth patterns or in `notability == Seat` leaves the flagship transcript unchanged with every check green — followup 15's failure exactly. The headline must be carried by the distribution of the **warm built patterns already in the inventory**: alcove, water jar, screen, doorway. And note every chamber has at least one link, so a doorway cannot be the threshold role's private property.

- [ ] **Step 1: Measure first — pin what the world looks like now**

Before any code changes, capture the baseline so the branch at Step 6 is a comparison rather than a memory:

```bash
git rev-parse HEAD > /tmp/hv-blocking-base.txt
bash scripts/regenerate-artifacts.sh
git status --porcelain book/src/gallery/ book/src/laboratory/ book/src/reference/
```

Expected: **clean** (Tasks 4–5 already committed their transcript moves). If it is not clean here, an earlier task left drift uncommitted — stop and resolve that first, because every conclusion in Step 6 depends on this being the zero point.

- [ ] **Step 2: Write the failing tests — differentiation, not pattern count**

`CLIENT-language-not-catalogue` binds (spec §4.3): if this task's substance turns out to be *how many* patterns exist, it has gone wrong. So the tests assert **adjacency and composition**, and the pattern count appears nowhere in them.

In `windows/vessel/tests/the_blocking.rs`:

```rust
#[test]
fn two_chambers_of_one_structure_do_not_read_alike() {
    // The Lintel's headline was literally true and experientially thin: four
    // doors onto one room (followup 11). This is the assertion that it stopped
    // being thin, made against the SAME structure the gallery walks.
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    inside(&mut session);
    let first = out(session.handle("look"));
    let stepped = out(session.handle("enter further in"));
    assert!(
        stepped.starts_with("[chamber "),
        "this structure has only one chamber, so the headline cannot be observed \
         here at all: {stepped}"
    );
    let second = out(session.handle("look"));
    let prose = |s: &str| {
        s.lines()
            .nth(1)
            .unwrap_or_default()
            .to_string()
    };
    assert_ne!(
        prose(&first),
        prose(&second),
        "two chambers of one structure still read identically"
    );
}

#[test]
fn a_role_admits_a_different_composition_not_a_bigger_one() {
    // The substance is WHICH patterns complete which, not how many exist. A role
    // whose composition is a superset of another's is a tier list, not a
    // vocabulary — so at least one pair of roles must each hold something the
    // other does not.
    use hornvale_vessel::interior::pattern::{Role, selection_for};
    let a: Vec<&str> = selection_for(Role::Threshold, true, false).iter().map(|p| p.name).collect();
    let b: Vec<&str> = selection_for(Role::Store, true, false).iter().map(|p| p.name).collect();
    assert!(
        a.iter().any(|n| !b.contains(n)) && b.iter().any(|n| !a.contains(n)),
        "one role's composition is a subset of the other's: {a:?} vs {b:?}"
    );
}

#[test]
fn a_locale_composition_is_untouched_by_the_role_layer() {
    // The load-bearing invariant of this task's DESIGN (ledger #10): whatever the
    // roles do to chambers, the band a creature stands in must be unaffected
    // unless we mean it to be. If this fails, the epoch is real -- which is a
    // finding, not a failure, but it must be a DELIBERATE one.
    use hornvale_vessel::interior::pattern::selection;
    let before = ["the-ground", "the-threshold", "the-alcove", "the-water-jar", "the-screen"];
    let now: Vec<&str> = selection(true, false).iter().map(|p| p.name).collect();
    assert_eq!(
        now, before,
        "a locale's warm built composition changed, so warmth changed, so \
         committed NPC drive history changed: this IS an epoch"
    );
}
```

**That `before` list is verified, not inferred.** Run while writing this plan:
`selection(true, false)` → `["the-ground", "the-threshold", "the-alcove", "the-water-jar", "the-screen"]`, and `selection(true, true)` → the same with `"the-fire"` and `"the-fireside-bed"` between `"the-alcove"` and `"the-water-jar"`. Use the warm list: the flagship structure is warm (ledger #12). If your run disagrees, an earlier task changed the inventory and the constraint that forbids that has already been broken — stop rather than updating the fixture.

- [ ] **Step 3: The role vocabulary**

```rust
/// What a chamber is FOR. A role admits a different pattern subset — the pattern
/// language one rung finer, where a role is a bundle of patterns that complete
/// each other.
///
/// Derived from the brief and the chamber's index, never authored per place.
pub enum Role { /* Threshold, Hearthroom, Store, Hall, Workroom */ }
```

Add `role: Role` (or a small role set) to `Pattern`, and `selection_for(role, built, cold)`. Keep `selection(built, cold)` as the **locale** path, unchanged in behaviour — that is what the third test above pins.

**Append new patterns; never insert or reorder.** `selection` iterates in order and admits a pattern only once its `requires` kind is present, so the order is the grammar's dependency order and an insertion silently re-composes every room in the world.

New anchor kinds, and the guard that makes them cheap: `chamber_prose::noun` and Task 4's `chamber_prose::detail` are both **exhaustive matches**, so each new kind fails to compile until someone writes what it is and what a closer look gives. That guard was built deliberately in The Lintel and this is its first real use — let it fire, do not add a catch-all arm.

`peak_population` on the brief, for the `store` role: add the field (free — nothing here is serialized, and `brief.rs`'s module doc licenses exactly this), wire it in `brief_of` from the alive `OccupationRecord`, and **hoist** `HAMLET_POPULATION_CEILING` from `flesh.rs:225` (it is a function-local `const` today) to `pub` rather than re-typing `150` in the vessel. One number, one meaning. The struct's doc attrs need `bare-ok(count: peak_population)` — on the **struct**, since a field-level tag is silently ignored (`extract.rs:150`).

`Brief::from_parts` has 11 call sites, all inside the vessel crate. Update them all; do not add a second constructor.

- [ ] **Step 4: Run the tests**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -12
cargo test -p hornvale-history 2>&1 | tail -6
```

If `a_locale_composition_is_untouched_by_the_role_layer` fails, **stop and read why** before proceeding. A new pattern reached a locale composition, which means the epoch is real. That is a legitimate outcome — but it must be because the role design needed it, not because a `role` field was defaulted somewhere permissive.

- [ ] **Step 5: The measurement**

```bash
bash scripts/regenerate-artifacts.sh
git status --porcelain book/src/gallery/ book/src/laboratory/ book/src/reference/
git diff --stat book/src/
```

Then run the health battery **as a gate**:

```bash
make gate 2>&1 | tail -20
```

Record, in your report, exactly three things: which files moved, whether any of them is under `book/src/laboratory/`, and whether `make gate` is green. Those three answers select the branch.

Remember followup 14: `make gate-full` always dirties `book/src/laboratory/generated/the-sounding/` with wall-clock timings. **Revert those, never re-pin them** — they are nanosecond measurements that differ every run, and no campaign has re-pinned them in 748 commits.

- [ ] **Step 6: Take exactly one branch**

**RE-PIN** — transcripts moved, `book/src/laboratory/` clean:
No label bump. A bump with no moved derivation is an *empty* epoch: it declares a discontinuity that did not occur, and it costs a permanent manifest row. Correct `ROOM_FURNISHING`'s doc comment (see LATENT below — the same correction applies), re-pin the galleries in this task's own commit, and say plainly in the commit message that the campaign did **not** need an epoch and why.

**EPOCH** — a metric golden moved:
1. Bump `room/furnishing/v1 → v2`, and state in the doc comment *what* moved and *which* read moved it. A bump whose justification is "the plan said so" is unauditable.
2. `make gate` is the **GATE**. If it is red, that is a calibration regression, not expected drift — the distinction §5.2 exists to protect. Do not re-pin through a red gate.
3. Re-pin the galleries in an **isolated commit** touching nothing else, so the epoch's blast radius is one reviewable diff.
4. **A census re-pin needs Nathan's explicit authorization** — an autopilot carve-out. Ask at the point of need; never assume it. Say which metric moved and by how much when you ask.
5. Check preregistered study pins: they must be **invariants** (ordering, sign, family membership, "stays zero"), never values (Amendment 1 §1a.5(b), decision 0016). A pin that has to be edited to match a new result is the seed-shopping 0016 forbids.

**LATENT** — nothing moved:
The response is **mandatory, not optional**. The inventory now holds patterns no live composition admits, so the discontinuity is merely deferred to whichever campaign opens the gate. Two obligations:
1. Correct `INVENTORY`'s doc comment. It says flatly that adding or reordering a pattern **is** an epoch. That is now over-strict — the true condition is *adding a pattern that a LIVE composition admits*, where live means read by something that commits (today: `interior_of` at a locale, because warmth feeds a committed drive). An over-strict warning is one that gets ignored, which is exactly how an **undeclared** epoch ships.
2. Record the gate condition as a followup and an idea-registry row: which patterns are gated, what opens the gate, and what it will cost then.

- [ ] **Step 7: Format, audit, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test --workspace 2>&1 | tail -12
```

Commit the code and the doc corrections together; commit any gallery re-pin **separately**, whichever branch you took, so that "what the world now derives" is never mixed with "what the code now does".

```bash
git commit -m "feat(vessel): chambers stop being identical

A role is a bundle of patterns that complete each other -- the pattern
language one rung finer, and the substance is WHICH patterns complete
which, not how many exist. Two chambers of one structure now read
differently, asserted against the same structure the gallery walks rather
than against a fixture built to succeed.

The headline is carried by the WARM built patterns, deliberately: the
seed-42 structure is tropical and not a seat, so differentiation resting on
the hearth patterns or on notability would have left the flagship
transcript unchanged with every check green.

[Then, whichever is true:]
The campaign did not need an epoch: nothing a creature reads moved, so
room/furnishing stays at v1 -- a bump with no moved derivation declares a
discontinuity that did not occur.
[or]
room/furnishing/v1 -> v2. <what moved, which read moved it>."
```

---

### Task 7: The epoch stamp — so a reload can say what moved

**Files:**
- Modify: `kernel/src/world.rs` (the stamp field), `cli/src/streams.rs` (hoist `versioned_labels`), `cli/src/main.rs` (stamp on save, diff on load)
- Create: a test asserting the round-trip and the diff

**Interfaces:**
- Produces: `World.derived_under: BTreeMap<String, String>`, `cli::streams::versioned_labels() -> BTreeMap<String, String>`.

**Where the stamp goes, and why not where the spec's wording suggests.** §5.3 says "record the epoch in the world," and a world is `{seed, registry, ledger}` — so the obvious reading is a committed fact at genesis. **Do not do that.** Two reasons, both checked:

1. **Entity ids are minted sequentially.** A stamp entity minted at genesis shifts every subsequent id, so every artifact in the project that mentions an entity id moves. That is a byte-identity break far larger than Task 6's, taken for metadata.
2. **The genesis crate cannot see the labels that matter.** `build_world` lives in `hornvale-worldgen`, and `room/furnishing` and `room/layout/*` live in `hornvale-vessel`, which is *downstream* of it. A genesis-time stamp would omit exactly the labels this campaign is about.

`World` already derives `Serialize, Deserialize` (`kernel/src/world.rs:54`) and `world.json` is a serde dump of it, so the stamp is a **field on `World`**, written by the **composition root** — `cli` is the only place that can see every crate's labels, which is precisely why `render_streams` lives there. No entity id moves, no fact is added, and it is still literally in the world.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn a_world_records_what_it_was_derived_under() {
    let w = /* build seed 42 */;
    let stamped = stamp(w, &versioned_labels());
    let json = serde_json::to_string(&stamped).unwrap();
    let back: World = serde_json::from_str(&json).unwrap();
    assert_eq!(back.derived_under, stamped.derived_under);
    assert!(
        back.derived_under.keys().any(|k| k.starts_with("room/layout/v1/")),
        "the stamp omits the labels this campaign declared, which is the failure \
         mode a genesis-time stamp would have had silently"
    );
    assert!(
        back.derived_under.keys().all(|k| k.contains("/v")),
        "an unversioned label is structural and can never differ, so recording it \
         adds a row that is always equal: {:?}",
        back.derived_under
    );
}

#[test]
fn a_world_saved_before_stamps_existed_still_loads() {
    // `#[serde(default)]`, asserted rather than assumed: a world.json written
    // before this campaign must not fail to parse, and an absent stamp is itself
    // informative — that world predates stamping.
    let json = r#"{"seed":42,"registry":{},"ledger":{}}"#; // shape it to match
    let w: Result<World, _> = serde_json::from_str(json);
    assert!(w.is_ok(), "an unstamped world must still load: {w:?}");
}

#[test]
fn a_changed_label_is_named_not_merely_noticed() {
    let mut then = versioned_labels();
    let (k, _) = then.iter().next().map(|(k, v)| (k.clone(), v.clone())).unwrap();
    then.insert(k.clone(), "v0".to_string());
    let moved = what_moved(&then, &versioned_labels());
    assert_eq!(moved, vec![k], "the diff must name the label, not just report one");
}
```

Shape the second test's JSON against what `serde_json::to_string(&World::new(Seed(42)))` actually emits — **run it and paste the real shape** rather than writing a plausible one. A hand-written fixture that does not match the real serialization tests nothing.

- [ ] **Step 2: Implement**

```rust
    /// Which versioned seed-derivation labels this world was derived under, as
    /// label -> version. Written by the composition root at save time, because
    /// only it can see every crate's labels (`cli::streams`).
    ///
    /// **Metadata about derivation, not derived content** — this is the one datum
    /// this campaign writes into a world, and it exists so that a reload after an
    /// epoch can say WHAT moved rather than silently rearranging someone's memory
    /// of a place (Rose Window Amendment 1 §1a.5).
    ///
    /// Absent on any world saved before stamping existed, which is itself the
    /// honest answer for such a world.
    /// type-audit: bare-ok(identifier-text: derived_under)
    #[serde(default)]
    pub derived_under: std::collections::BTreeMap<String, String>,
```

`versioned_labels()` is hoisted out of `render_streams`'s existing composition — the same roster, filtered to labels containing a `/v` segment, split into (label-without-version, version). One roster, two consumers: the manifest page and the stamp. Do not write a second list; that is the drift `stream_labels!` exists to prevent.

On load, `hornvale possess --world <PATH>` compares and, if anything differs, emits **one line before the first turn**, naming what moved:

> *You have been away. The rooms are not as you remember.* (room/layout)

Keep the message in the world's voice and derive the parenthetical from the diff. The comparison lives in `cli`, not in the vessel: the vessel must not learn about the composition root, and the session's prose stays a function of the world it was handed.

- [ ] **Step 3: Run, verify, check the drift**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo test --workspace 2>&1 | tail -12
bash scripts/regenerate-artifacts.sh
git status --porcelain book/src/
```

**The expectation is clean**, and if it is not, understand why before re-pinning. A committed artifact that dumps a world's JSON would gain a `derived_under` key — additive, and fine to re-pin, but say so explicitly in your report rather than folding it in. Anything under `book/src/laboratory/` moving here means a metric read a field it should not have.

```bash
git add kernel/src/world.rs cli/src/ book/src/
git commit -m "feat(kernel): a world records what it was derived under

Amendment 1 §1a.5 asked for the player-facing consequence of an epoch to be
STATED rather than discovered, and nothing recorded which epoch a world was
made under. Now a reload diffs two label sets and can name what moved --
the rooms, or the furnishings -- instead of one generic warning about an
unspecified rearrangement.

A field on World rather than a fact at genesis, deliberately: entity ids
are minted sequentially, so a stamp entity at genesis would shift every id
after it and move every artifact in the project for the sake of metadata.
And build_world lives upstream of the vessel, so a genesis-time stamp could
not have seen room/layout at all -- it would have omitted exactly the
labels it exists to record.

Written by the composition root, which is the only place that can see every
crate's labels; #[serde(default)], so a world saved before stamping loads
with an empty stamp, which is the honest answer for such a world."
```

---

### Task 8: Close

**Files:** `docs/decisions/`, `book/src/chronicle/the-blocking.md`, `book/src/SUMMARY.md`, `docs/retrospectives/the-blocking.md`, `book/src/frontier/idea-registry.md`, `book/src/frontier/frontier.md`, the spec

**This task is the campaign's G6 hard stop.** Present the post-G3 ledger digest to Nathan and wait. Then run `closing-a-campaign`, unchanged — it owns the merge, the worktree, and the final summary. Nothing in this task force-pushes, deletes a branch, or regenerates a census without Nathan's word.

- [ ] **Step 1: The full gate, before anything else**

```bash
make gate-full 2>&1 | tail -30
```

Then revert the wall-clock artifacts it always dirties (followup 14 — `book/src/laboratory/generated/the-sounding/{rows.csv,summary.md,sample-biographies.txt}`):

```bash
git checkout -- book/src/laboratory/generated/the-sounding/
git status --porcelain
```

**Revert, never re-pin.** They are nanosecond measurements that differ every run; precedent is 748 commits of every campaign doing the same.

- [ ] **Step 2: Decisions**

Next number is **0083** — verify with `ls docs/decisions/ | tail -3` before writing, and follow the house form (Status/Decider, context, consequence, see-also).

- **The layout labels.** One per method, causal, versioned from the first commit. The decision to record is not "we made a label" but **the granularity rule**: the unit of independent change is the algorithm, so a method gets a label and a *predicted* method does not get one in advance. Cites 0073, 0072, Amendment 2 §1b.7, ledger #7.
- **The furnishing label's fate**, whichever Task 6 measured — and if the answer was "no bump", record *that*, with the reason: a bump on a label nothing draws from declares a discontinuity that did not occur. A decision log that records only the changes made, never the ones correctly declined, teaches the wrong lesson.
- **Consider** a record for the extent derivation (ledger #8) if it reads as a law rather than a formula — the durable-shell argument generalizes past this campaign, and "derive geometry from what outlives the tenants, not from who lives there now" is the kind of sentence that silently regrows wrong. Nathan's call at G6; propose it, do not mint it unilaterally.
- **Consider** followup 7's band-notation record, which The Lintel left owed.

- [ ] **Step 3: Chronicle and freshness**

Write `book/src/chronicle/the-blocking.md` and register it in `book/src/SUMMARY.md`. The chronicle's job is the *argument*, not the changelog: the inversion (contents → map, §2), the embedder-not-generator discipline and how rule 7 makes it checkable, the reversal of The Lintel's refusal and why it is not a flip-flop, and — whichever it was — the epoch or its honest absence.

**Freshness sweep** of the room-mesh and possession chapters, plus the two the campaign contradicted:

```bash
grep -rn 'refused indoors\|no north\|identical\|four doors onto one room' book/src/architecture/ book/src/reference/ book/src/chronicle/the-lintel.md
```

Followup 13 is in scope here and cheap: `chamber_nouns`' doc comment (`chamber_prose.rs:26-30`) has been half-stale since The Lintel, and this campaign touches that file twice.

- [ ] **Step 4: Registry, and the 600-char cap**

Rows are capped at **600 characters** and the cap is **append-never** — write each edit as an index entry pointing at the chronicle, never as a narrative. Flip and repoint **Where**:

- `CLIENT-refinement-checker` — the seven rules now exist as tests (Task 3).
- `CLIENT-district-patterns` — partially: the same composer one band down.
- `CLIENT-tile-view` — the ASCII plan is the first tile view shipped.
- `CLIENT-scale-bands` — the band-law count, if Task 5 shipped one.
- `CLIENT-two-tier-position` — position inside a chamber is `FRAME`-tier and asserted so; that is the row's "byte-identical BY CONSTRUCTION" claim now carrying a test.

New rows for what was captured, not built: the **tech-as-span-cap** model, the **durable extent** (both ledger #8), the **content-addressed label** idea (#7), and — if Task 6 landed LATENT — the gated-pattern condition.

- [ ] **Step 5: Retrospective, spec amendments, gradient**

- `docs/retrospectives/the-blocking.md`, following the house form (decision 0020). Promote `.superpowers/sdd/followups.md` into its follow-up section — the register is scratch and does not survive the worktree.
- **Amend spec §10 risk 1** with Task 1 Step 5's measured number *and its build profile*. A number without a profile is not a measurement (followup 2a: the same spike measured ~10× slower in debug).
- Re-score the Confidence Gradient in `book/src/frontier/frontier.md` if a bet moved. The candidates: the embedder's fidelity claim moved from argued to checked (rule 7), and the parity contract moved from a design intention to a test.
- Carry outbound debt forward explicitly, including the two inherited items this campaign did not fix (`make vessel-check` red on `main`, and `sum::<f64>()` over an empty iterator serializing as `-0.0`) — say plainly that they were inherited and left, rather than letting them vanish.

- [ ] **Step 6: G6 — stop and present**

Assemble the digest, **save-format/epoch/determinism entries first**: ledger #7 (per-method labels), #9 (what the stamp records), #10 (the epoch measured, and which branch landed), then #8, #11, #12. One line each, pointing at the ledger. Say which of RE-PIN / EPOCH / LATENT Task 6 produced and what it cost.

Then wait. `closing-a-campaign` runs after Nathan's word, not before.
