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
pub mod classify;
pub mod grow;
pub mod occupancy;

pub use allocate::allocate;
pub use classify::{freedom_of_a_chain, openings, realized_links, region_of};
pub use grow::grow;
pub use occupancy::Occupancy;

use crate::brief::Brief;
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::{BTreeMap, BTreeSet};

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
    /// type-audit: bare-ok(flag: return)
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
/// type-audit: bare-ok(index: doorways), bare-ok(index: owner), bare-ok(count: dof)
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
    /// Which chamber owns each cell — the AUTHORITATIVE assignment.
    ///
    /// `regions` is a summary: for a grown lattice those rects are bounding boxes
    /// and they OVERLAP, so `Rect::contains` is necessary but not sufficient and
    /// scanning them answers a different question than the one asked. Consult this
    /// map instead. Every cell of `extent` appears exactly once.
    pub owner: BTreeMap<Cell, usize>,
    /// How many independent choices the embedder made — one per stream draw it
    /// consumed. **Reported, not recomputed**, because §7 rule 7 asks for a
    /// number and a number derived by a second, independent traversal of the
    /// result is a number that can disagree with what the embedder actually did.
    ///
    /// The checker compares this against what the anchor graph leaves free. If it
    /// exceeds that, the embedder is INVENTING, which is the one thing an
    /// embedder may not do (Amendment 1 §1a.7).
    pub dof: u32,
}

/// Every adjacent cell pair inside the lattice that separates two differently-owned
/// cells, minus the THRESHOLDS the doorways open.
///
/// One derivation, shared by both methods, over the authoritative ownership map,
/// and it takes the thresholds as an **input**. Both embedders used to decide
/// independently what a boundary was — walls from the geometry in one pass,
/// doorways from a second pass over the same geometry — and two passes over one
/// geometry is exactly how a doorway comes to open a way no link specified:
/// exempting every pair that *touches* a door cell unwalls all four of its sides,
/// so a door at a cell where three chambers meet lets a mover into the third.
/// That is §7 rule 1's second direction failing, and the fix is to make the
/// doorway an input here rather than a parallel computation.
///
/// A threshold is an unordered pair, normalized `(min, max)` as `walls` is.
fn walls_around(
    owner: &BTreeMap<Cell, usize>,
    thresholds: &BTreeSet<(Cell, Cell)>,
) -> BTreeSet<(Cell, Cell)> {
    let mut walls = BTreeSet::new();
    // `(1, 0)` and `(0, 1)` only: each unordered pair is then visited exactly
    // once, from its lower cell.
    for (here, &mine) in owner {
        for (dx, dy) in [(1, 0), (0, 1)] {
            let there = Cell(here.0 + dx, here.1 + dy);
            if let Some(&theirs) = owner.get(&there)
                && theirs != mine
            {
                let pair = (*here.min(&there), *here.max(&there));
                if !thresholds.contains(&pair) {
                    walls.insert(pair);
                }
            }
        }
    }
    walls
}

/// Embed `structure`, choosing the method the brief calls for.
///
/// The selector is the point: rectilinear allocation for places somebody built,
/// region growing for places nobody did. Radial (temples) and branching (mines)
/// are predicted by the spec's §3.2 grid and plug in here — this function is the
/// seam, which is why it exists at two methods rather than being inlined.
///
/// Each method derives its own stream (ledger #7), so the two draw independently:
/// adding a third method cannot move where an existing one puts things.
pub fn embed_with(structure: &Structure, brief: &Brief, extent: Rect, seed: Seed) -> Lattice {
    if brief.built {
        allocate(structure, extent, seed)
    } else {
        grow(structure, extent, seed)
    }
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

    fn wild() -> Brief {
        Brief::from_parts(None, None, None, None, false, true)
    }

    fn embed(seed: u64) -> (crate::structure::Structure, Lattice) {
        let s = structure_at(&locale(), &built(), Seed(seed), WALK).expect("built");
        let l = embed_with(&s, &built(), extent_for(&s), Seed(seed));
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
                assert!(
                    r.w >= 2 && r.h >= 2,
                    "seed {seed}: region {i} is {r:?} — a chamber narrower than 2 \
                     cells has no interior to stand in"
                );
            }
        }
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
        // The structure must have MORE THAN ONE chamber for the second half to
        // mean anything: one blob floods the whole extent whatever cell it starts
        // from, so a single-chamber structure forces eight identical plans and
        // "the seed is ignored" would be a false accusation rather than a finding.
        // Asserted rather than assumed, because the count is `structure_at`'s
        // business and could move under this test.
        let s = structure_at(&locale(), &built(), Seed(42), WALK).expect("built");
        assert!(
            s.chambers.len() > 1,
            "this test needs a structure with residual freedom to fill; {} chambers has none",
            s.chambers.len()
        );
        let a = embed_with(&s, &wild(), extent_for(&s), Seed(7));
        let b = embed_with(&s, &wild(), extent_for(&s), Seed(7));
        assert_eq!(a, b);
        let plans: Vec<Lattice> = (0..8u64)
            .map(|sd| embed_with(&s, &wild(), extent_for(&s), Seed(sd)))
            .collect();
        assert!(plans.iter().any(|p| *p != plans[0]), "the seed is ignored");
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

    // The measurement harness times ONE `allocate` call for a diagnostic (never
    // sim logic, never a fact, never seeded from wall-clock) -- exempt from the
    // wall-clock ban (clippy.toml / decision 0001), same pattern as
    // `cli/tests/graph_cost.rs`'s graph-derivation timing.
    #[allow(clippy::disallowed_types)]
    // benchmark harness: measuring the embedding, not sim logic
    use std::time::Instant;

    /// Wall-time ceiling for ONE `allocate` call on the widest extent
    /// `extent_for` can derive (4 chambers, 16x16). Measured on an M-series
    /// laptop: **27.6 us** median in RELEASE, **209 us** median in DEBUG — a
    /// 7.6x gap, so the profile has to be stated with the number or it means
    /// nothing (this project measured a similar ~10x gap during The Lintel).
    ///
    /// **Re-measured in Task 3, and it moved by 3.3x** (from 6.79 us release /
    /// 62.5 us debug), because `Lattice` now carries per-cell ownership: 256
    /// `BTreeMap` inserts plus `walls_around`'s probes cost more than the rect
    /// scans they replaced. That is the price of §7 rules 1–4 being checkable
    /// over a GROWN lattice at all, whose bounding rects overlap — so it is paid
    /// deliberately, and recorded rather than absorbed.
    ///
    /// The ceiling stays where Task 1 set it, at ~5x the new debug median. It is
    /// a falsification ceiling for a real regression — an accidental quadratic in
    /// `walls_around`, say, whose cost is currently `extent.area()` `BTreeMap`
    /// probes — not a target to approach.
    /// type-audit: bare-ok(count)
    const ALLOCATE_BUDGET_MICROS: u128 = 1_000;

    #[test]
    fn the_embedding_is_cheap_enough_to_re_derive() {
        // Spec §10 risk 1: no budget claim without a measurement. A lattice is
        // FRAME-tier and re-derived on every entry, so the cost that matters is
        // one call at the worst extent `extent_for` can produce.
        let s = structure_of(crate::structure::MAX_CHAMBERS);
        let extent = extent_for(&s);
        assert_eq!(extent.w, 2 * CHAMBER_SIDE, "the worst-case extent moved");

        const SAMPLES: usize = 1001;
        let mut nanos: Vec<u128> = Vec::with_capacity(SAMPLES);
        for i in 0..SAMPLES {
            #[allow(clippy::disallowed_types)] // benchmark harness
            let start = Instant::now();
            let l = allocate(&s, extent, Seed(i as u64));
            #[allow(clippy::disallowed_types)] // benchmark harness
            let elapsed = start.elapsed();
            std::hint::black_box(&l);
            nanos.push(elapsed.as_nanos());
        }
        nanos.sort_unstable();
        let median = nanos[SAMPLES / 2];
        eprintln!(
            "the_embedding_is_cheap_enough_to_re_derive: median {median} ns per \
             allocate at {}x{} ({} chambers), min {} ns, p99 {} ns",
            extent.w,
            extent.h,
            s.chambers.len(),
            nanos[0],
            nanos[SAMPLES * 99 / 100],
        );
        assert!(
            median / 1_000 < ALLOCATE_BUDGET_MICROS,
            "one allocate call took {median} ns, over the {ALLOCATE_BUDGET_MICROS} us ceiling"
        );
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
