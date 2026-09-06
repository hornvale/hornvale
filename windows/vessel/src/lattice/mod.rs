//! The LATTICE: a structure's chambers embedded as cells of one grid.
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
//!
//! # A wall is a CELL, not a boundary
//!
//! Nathan's call, 2026-07-28. Tasks 1–4 modelled a wall as a NON-ADJACENCY: a
//! property of the boundary between two cells, held in a set of cell pairs. Every
//! cell of the lattice was floor. That model works, and it cost more than it
//! bought:
//!
//! 1. **A 1:1 grid has nowhere to draw a boundary**, so the render had to double
//!    to `(2w+1) x (2h+1)` and carry a coordinate mapping between picture
//!    positions and cells. That mapping and its whole off-by-one class are gone.
//! 2. **It is the model every roguelike and every tilemap engine already
//!    speaks**, so The Panes inherits the standard rather than a translation
//!    layer, and The Sighting's shadowcast gets blocking CELLS, which is what its
//!    measured timings assumed.
//! 3. **The thickness this concedes is more accurate, not less.** A cell is about
//!    a metre and this world models neolithic through classical building — turf,
//!    cob and rubble-stone walls genuinely run half a metre to two. A
//!    zero-thickness wall was the less faithful choice.
//! 4. **Two anchor kinds that already ship gain a place.** `Screen` ("affords
//!    nothing, shapes sightlines") is a partition; `Alcove` ("a recess off the
//!    main space") is literally a passable wall cell. And `the-fire` attaching
//!    `Within(Alcove)` has been describing a FIREPLACE since The Hearth with no
//!    geometry to make it legible.
//! 5. **A threshold becomes a place**, so it can later hold a door, be barred, or
//!    be blocked by rubble.
//! 6. **§7 rule 3 stops being tautological** — the outer ring must be entirely
//!    `Wall`, which the embedder could fail to do and which the boundary model had
//!    nothing to say about.
//!
//! It also introduces a failure mode the boundary model could not have: walls as
//! cells can **seal a pocket of floor**. That is why there is a rule 8
//! (reachability) where Amendment 2 §1b.8 listed seven, and why `grow` claims with
//! a separation rule and never takes a cell back.

pub mod allocate;
pub mod anchor_cells;
pub mod classify;
pub mod grow;
pub mod occupancy;
pub mod render;
pub mod sight;

pub use allocate::allocate;
pub use anchor_cells::{anchor_cells, is_faithful};
pub use classify::{
    bounds_of, cell_beyond, doorway_between, freedom_of_a_tree, kind_of, openings, reachable_from,
    realized_links, standing_cell,
};
pub use grow::grow;
pub use occupancy::{Occupancy, Refusal};
pub use render::{Plan, render};
pub use sight::{shadowcast, shadowcast_with};

use crate::brief::Brief;
use crate::structure::Structure;
use hornvale_kernel::Seed;
use std::collections::BTreeMap;

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
    /// This rectangle shrunk by `by` cells on every side.
    ///
    /// How both embedders get the space they may claim: `extent.inset(1)` is the
    /// extent minus its exterior shell. Stated as one named operation rather than
    /// four open-coded arithmetic edits, because getting one of the four wrong is
    /// how a plan comes to have a gap in its outer wall — and §7 rule 3(i) is a
    /// check on exactly that.
    /// type-audit: bare-ok(count: by)
    pub fn inset(&self, by: i32) -> Rect {
        Rect {
            x: self.x + by,
            y: self.y + by,
            w: self.w - 2 * by,
            h: self.h - 2 * by,
        }
    }
}

/// One cell of the lattice, in lattice-local coordinates. `FRAME`-tier: never
/// serialized, never a fact's object (decision 0069).
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cell(pub i32, pub i32);

/// What occupies one cell of the lattice.
///
/// **Closed at three variants on purpose.** The moment this enum becomes the place
/// where richness lives, the lattice is a tile catalogue and
/// `CLIENT-language-not-catalogue` has been violated one band down. A window is an
/// ANCHOR at a wall cell, never `CellKind::Window`. The only variants that should
/// ever join these three are states a cell can *transition into over time*
/// (`Rubble`, `Barred`), and neither is this campaign's business.
///
/// The positions are `Floor.0`, `Threshold.0` and `Threshold.1` — the audit names
/// an enum payload `Variant.index` (`tools/type-audit/src/extract.rs`), so a
/// qualifier of `Floor` would be a stale tag, not a verdict. Tagged blanket
/// instead: all three are chamber indices into `Structure::chambers`.
/// type-audit: bare-ok(index)
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
    /// against the predicate survives it. That is the plan's constraint, not a
    /// style preference.
    /// type-audit: bare-ok(flag: return)
    pub fn passable(&self) -> bool {
        !matches!(self, CellKind::Wall)
    }

    /// Does this cell serve `chamber` — as its floor, or as one side of its door?
    ///
    /// A threshold serves BOTH of its chambers, which is why this is a predicate
    /// rather than an `Option<usize>` owner: asking "whose is this cell" of a
    /// doorway has two right answers, and the old `owner` map could only hold one.
    /// type-audit: bare-ok(index: chamber), bare-ok(flag: return)
    pub fn serves(&self, chamber: usize) -> bool {
        match self {
            CellKind::Floor(i) => *i == chamber,
            CellKind::Wall => false,
            CellKind::Threshold(a, b) => *a == chamber || *b == chamber,
        }
    }
}

/// The eight steps a mover — or a growing blob, or a flood — may take between
/// cells: four orthogonal, then four diagonal.
///
/// **The first four are always the four orthogonal headings, and every caller
/// that has never had reason to consider a corner may go on indexing or slicing
/// just those** — the same first-four contract [`hornvale_kernel::Facet::neighbors`]
/// states for the cube's own quads. A diagonal step alone says nothing about
/// whether it is a legal move: stepping between two cells that meet only at a
/// point can cut through solid fabric, and whether it does is [`Lattice`]
/// geometry, not [`HEADINGS`] geometry — see [`diagonal_is_blocked`].
///
/// **Two step paths DO wire diagonal movement now** (The Pavement, Task 6):
/// `crate::session::Session::step` and `crate::underground::Underground::peek`
/// both walk all eight bearings with [`diagonal_is_blocked`] applied. The
/// sentence here used to read "nothing wires diagonal movement into a step path
/// yet", and it was the load-bearing sentence for the 4-vs-8 decision below, so
/// leaving it would have made that decision look like an accident of timing
/// rather than a choice.
///
/// **Every consumer in this crate that floods or checks reachability through the
/// lattice still reads only the first four entries, and now does so on purpose
/// rather than by default.** The reason is not that nothing moves diagonally; it
/// is that these floods describe what the GENERATOR built, and the generator
/// builds orthogonally — `grow`'s flood, its separation rule, and its tunnels
/// alike. The argument is written out once, in `classify`'s module note "Why the
/// structural rules stay 4-connected while movement is 8-connected"; a flood that
/// quietly started admitting diagonals here would report adjacency no rule
/// sanctioned.
///
/// A heading is a CELL DELTA, which is the same quantity `Rect`'s `w` and `h` are
/// and tagged the same way. Not `index`: a `Cell` is a position and these are the
/// differences between positions, so a newtype over them would be a different one
/// from `Cell`'s and neither is earned at eight constants.
/// type-audit: bare-ok(count)
pub const HEADINGS: [(i32, i32); 8] = [
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1),
    (1, 1),
    (1, -1),
    (-1, -1),
    (-1, 1),
];

/// `cell`'s eight neighbours, in [`HEADINGS`] order: the four orthogonal ones
/// first, then the four diagonal ones. A caller that wants only the orthogonal
/// four — every flood in this crate, today — takes `neighbours(cell)[..4]`.
pub fn neighbours(cell: Cell) -> [Cell; 8] {
    HEADINGS.map(|(dx, dy)| Cell(cell.0 + dx, cell.1 + dy))
}

/// Would stepping diagonally by `d` from `from` cut through the corner where
/// two walls meet, given `open` as the passability oracle for a single cell?
///
/// **A passability CLOSURE, not a `Lattice`, because two different `Cell`-
/// addressed representations both need this rule.** Interiors are a
/// [`Lattice`], queried through [`kind_of`]/[`CellKind::passable`]; the
/// underground level is a `CellGrid<LevelCellKind>`
/// (`crate::underworld_level::CellGrid`), queried through
/// `crate::underworld_level::movement_mode` — never a `Lattice` and never a
/// `CellKind`. The geometry this function computes (which two cells flank a
/// diagonal, and whether both are closed) does not depend on which
/// representation is asking, so it is written once here rather than
/// duplicated per band. This is the shared implementation
/// [`crate::session::cell_delta`]'s own doc already promises: "a diagonal is
/// a diagonal whether the walls around it are built or natural rock."
///
/// A diagonal step passes between the two cells that flank it — for
/// `d = (1, 1)` those are the cells one step east and one step south of
/// `from`. **Refused only when BOTH flanks are impassable** (`open` returns
/// `false` for both). Brushing past a single standing corner is physical,
/// the way a shoulder can lean past one doorjamb; passing between two walls
/// that meet at a point is not. Reading only one flank would refuse a
/// diagonal no wall actually blocks — a one-directional check, structurally
/// blind to over-refusal, the same failure shape root `CLAUDE.md`'s
/// STALE-DECL rule names for a one-directional acknowledgement ("can only
/// ever be satisfied, so it rots") and decision 0456 names generally for a
/// rule stated in two places without a test that checks them against each
/// other — and `a_diagonal_is_refused_only_when_both_flanks_are_walls`
/// checks both directions for exactly that reason.
///
/// **What counts as "impassable" — including anything outside a band's own
/// bounds — is entirely `open`'s call, not this function's.** It asks the
/// same two cells either way and trusts the closure's answer.
///
/// **Checks only the two flanks, never the destination `from + d` itself.**
/// `!diagonal_is_blocked(...)` means "the corner does not refuse this step",
/// not "this step is walkable" — a caller must still ask `open(from + d)` (or
/// whatever the destination's own passability check is) separately, or it
/// can walk a mover straight into a wall the corner rule was never meant to
/// gate.
///
/// `d` is meant to be one of [`HEADINGS`]'s four diagonal entries (`dx != 0
/// && dy != 0`); called with an orthogonal delta there is no corner to cut,
/// so this returns `false`.
/// type-audit: bare-ok(count: d), bare-ok(flag: return)
pub fn diagonal_is_blocked(from: Cell, d: (i32, i32), open: impl Fn(Cell) -> bool) -> bool {
    let (dx, dy) = d;
    if dx == 0 || dy == 0 {
        return false;
    }
    let flank_a = Cell(from.0 + dx, from.1);
    let flank_b = Cell(from.0, from.1 + dy);
    !open(flank_a) && !open(flank_b)
}

/// The side of one chamber's nominal INTERIOR, in cells — the standing room,
/// not counting the fabric around it.
///
/// Chosen against two bounds, both checked rather than trusted: at the bottom,
/// `MIN_CHAMBER_SPAN` must still fit after a chain of splits; at the top, the
/// widest plan any chamber count can produce must fit an 80-column transcript.
///
/// **Task 4b halved the pressure at the top.** Task 4 had to note that a wall
/// living BETWEEN cells made the render draw `2w + 1` columns, so the real ceiling
/// was half what Task 1 assumed. A wall is a cell now, the picture is 1:1, and the
/// ceiling is the extent's own width again — which is what makes an exterior shell
/// affordable at all. Asserted twice on purpose:
/// `the_largest_extent_leaves_the_render_room_to_draw` here, from the extent, and
/// `render::tests::the_widest_plan_fits_a_terminal` from the drawn picture.
/// type-audit: bare-ok(count)
/// plumb: universal(derived against two checked bounds — MIN_CHAMBER_SPAN and terminal width — a lattice geometry constant)
pub const CHAMBER_SIDE: i32 = 8;

/// How big `structure`'s plan is: **as big as the rooms it must hold, plus the
/// fabric between them.**
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
/// **The `+ (cols + 1)` is the fabric**, and it is not overhead. Wall lines are one
/// more than the interiors they separate — a wall on each outside plus one between
/// each pair — so the extent is `cols * CHAMBER_SIDE + (cols + 1)` per axis: 10x10
/// at one chamber, 19x10 at two, 19x19 at three or four. Roughly a fifth of the
/// extent is exterior shell, spent deliberately: it is what makes the drawn plan
/// read as a BUILDING rather than as a floating partition diagram.
///
/// Origin-anchored, always: cells are lattice-LOCAL, so a plan has no place in any
/// wider coordinate system to be offset into.
pub fn extent_for(structure: &Structure) -> Rect {
    // Blocks, not area: an exhaustive arrangement over 1..=MAX_CHAMBERS avoids an
    // integer square root and states the coupling to MAX_CHAMBERS out loud. The
    // chamber interiors no longer tile the extent — the fabric between them is
    // part of it now — so at three chambers one chamber simply gets the larger
    // share, which reads as a bigger room rather than as waste.
    let (cols, rows) = match structure.chambers.len() {
        0 | 1 => (1, 1),
        2 => (2, 1),
        _ => (2, 2),
    };
    Rect {
        x: 0,
        y: 0,
        w: cols * CHAMBER_SIDE + (cols + 1),
        h: rows * CHAMBER_SIDE + (rows + 1),
    }
}

/// A structure embedded as one grid.
/// type-audit: bare-ok(index: doorways), bare-ok(count: dof)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lattice {
    /// The whole plan's bounds.
    pub extent: Rect,
    /// Every cell of `extent`, with its kind. TOTAL: every cell appears exactly
    /// once, so [`kind_of`] returning `None` means "outside the extent" and
    /// NOTHING else.
    ///
    /// That totality is the point. A partial map with walls simply absent would
    /// make `None` mean "outside the extent OR is a wall" — two distinct facts in
    /// one value, which is the exact shape of the rect-scan defect Task 2 found
    /// (ledger #17). Wall-ness is a POSITIVE fact here, so every rule that
    /// compares two cells must say out loud what it means about walls. §7 rule 3
    /// checks the totality rather than trusting this sentence.
    ///
    /// This replaces Tasks 1–4's `owner`, `regions` and pair-valued `walls`.
    /// `regions` was a trap twice over — grown blobs' bounding rects overlap, and
    /// a rect-scanning `region_of` agreed with the truth for exactly one of the
    /// two methods — and under this model it is ambiguous besides: does a
    /// region's rect include its wall ring? [`bounds_of`] answers the question
    /// Task 6 actually asks, derived from these cells in one pass.
    pub cells: BTreeMap<Cell, CellKind>,
    /// `(chamber a, chamber b, the cell you pass through)`, one per link in
    /// `Structure::links`. The cell is a `Threshold(a, b)` in [`Lattice::cells`],
    /// and §7 rule 3 checks the correspondence in both directions.
    pub doorways: Vec<(usize, usize, Cell)>,
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
    // Deliberately still `brief.built`, not `brief.site.is_some()` (The
    // Prospect, Decision 0666). This is a GENERATOR DISPATCH, not the
    // enterability gate: it asks "constructed or natural" to choose between
    // rectilinear rooms and a grown blob, which is exactly what `built` means.
    // A cave has a site but was never built, and must still `grow`; reading
    // this as the gate and rewriting it to the site would generate every cave
    // as a rectilinear building.
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
    use crate::housemark::{AuthorityMark, Housemark, ThresholdPosture};
    use crate::site::{Site, SiteKind};
    use crate::structure::structure_at;
    use hornvale_history::record::{Function, Notability};
    use hornvale_kernel::{Facet, Seed};

    const WALK: u32 = 13;

    fn locale() -> Facet {
        Facet {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    /// A living, warm, communal, plain-postured agrarian dwelling: the BUSH
    /// shape, four chambers, `T{ H, W, S }` — a fork of three at the door.
    ///
    /// The chamber count is the GRAMMAR's since The Cruck, Task 3, so a built
    /// fixture names the shape it wants with a brief instead of scanning
    /// locales for one. This is the fullest frame the grammar derives without a
    /// Seat, so it is what a corpus uses to reach `MAX_CHAMBERS`.
    fn built() -> Brief {
        Brief::from_parts(
            Some(Function::Agrarian),
            None,
            Some(Notability::Common),
            None,
            Some(Housemark {
                authority: AuthorityMark::Common,
                threshold: ThresholdPosture::Plain,
            }),
            0,
            true,
            false,
            Some(Site::placed(SiteKind::Settlement, None)),
            None,
        )
    }

    /// A CAVE: a site nobody built. Both the structure source for a grown
    /// fixture — a wild draw is still a chain of 1..=MAX_CHAMBERS, spec §3.5 —
    /// and the method selector `embed_with` reads to send it to `grow`.
    ///
    /// It carries a SITE now, and must: `structure_at` gates on `brief.site`
    /// (decision 0666), so a site-less method selector derives no structure at
    /// all. Giving it a real cave site is what lets a grown fixture come from
    /// the derivation production's caves actually take.
    fn wild() -> Brief {
        Brief::from_parts(
            None,
            None,
            None,
            None,
            None,
            0,
            false,
            true,
            Some(Site::placed(SiteKind::Cave, None)),
            None,
        )
    }

    fn embed(seed: u64) -> (crate::structure::Structure, Lattice) {
        let s = structure_at(&locale(), &built(), Seed(seed), WALK).expect("built");
        let l = embed_with(&s, &built(), extent_for(&s), Seed(seed));
        (s, l)
    }

    #[test]
    fn every_chamber_has_bounds_and_they_lie_inside_the_shell() {
        // What `one_region_per_chamber` used to assert, restated over the map that
        // is now authoritative: every chamber holds floor somewhere, and none of
        // it is in the exterior wall.
        let (s, l) = embed(42);
        let interior = l.extent.inset(1);
        for i in 0..s.chambers.len() {
            let b =
                bounds_of(&l, i).unwrap_or_else(|| panic!("chamber {i} owns no floor cell at all"));
            assert!(
                b.w >= 1 && b.h >= 1,
                "chamber {i} has degenerate bounds {b:?}"
            );
            assert!(
                interior.contains(Cell(b.x, b.y))
                    && interior.contains(Cell(b.x + b.w - 1, b.y + b.h - 1)),
                "chamber {i}'s floor at {b:?} escapes the interior {interior:?}"
            );
        }
    }

    #[test]
    fn the_chamber_floors_are_disjoint_and_leave_room_for_fabric() {
        // The old `regions_tile_the_extent_without_overlapping`. Floors no longer
        // TILE the extent — the fabric is part of it now — so the two claims that
        // survive are disjointness (two chambers must not claim one cell, which
        // `cells` makes structurally impossible and this states anyway) and that
        // the fabric is really there rather than the shell being the only wall.
        let (s, l) = embed(42);
        let floor: i32 = l
            .cells
            .values()
            .filter(|k| matches!(k, CellKind::Floor(_)))
            .count() as i32;
        let per_chamber: i32 = (0..s.chambers.len())
            .map(|i| {
                l.cells
                    .values()
                    .filter(|k| **k == CellKind::Floor(i))
                    .count() as i32
            })
            .sum();
        assert_eq!(
            floor, per_chamber,
            "a floor cell belongs to exactly one chamber"
        );
        assert!(
            floor < l.extent.area(),
            "every cell is floor, so the plan has no fabric at all"
        );
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
    fn a_doorway_cell_is_a_threshold_between_the_two_chambers_it_names() {
        // The old `a_doorway_cell_lies_on_the_shared_edge_of_both_regions`, and it
        // gets STRONGER rather than merely restated: a doorway used to be checked
        // against two bounding rects, which for a grown lattice is a scan of
        // overlapping boxes. Now it is checked against the cell itself.
        let (_, l) = embed(42);
        for &(a, b, cell) in &l.doorways {
            assert_eq!(
                kind_of(&l, cell),
                Some(CellKind::Threshold(a, b)),
                "doorway cell {cell:?} for ({a},{b}) is not a threshold between them"
            );
            for (chamber, other) in [(a, b), (b, a)] {
                assert!(
                    // First four: a mover reaches a doorway ORTHOGONALLY (see
                    // `cell_beyond`), so a diagonal touch must not count here
                    // either — that would let a defect that only reaches this
                    // chamber diagonally read as faithful.
                    neighbours(cell)[..4]
                        .iter()
                        .any(|n| kind_of(&l, *n) == Some(CellKind::Floor(chamber))),
                    "the doorway at {cell:?} has no {chamber} floor beside it, so \
                     chamber {other} cannot reach chamber {chamber} through it"
                );
            }
        }
    }

    /// claim: invariant(forall-seed) — determinism, over 0..8
    #[test]
    fn the_embedding_is_pure() {
        for seed in 0..8u64 {
            let a = embed(seed);
            let b = embed(seed);
            assert_eq!(a.1, b.1, "seed {seed}: embedding is not a pure function");
        }
    }

    /// claim: reachability(seed: 0..8) — non-degeneracy: the seed is not ignored
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

    /// claim: invariant(forall-seed) — over 0..8
    #[test]
    fn no_chamber_is_degenerate() {
        for seed in 0..8u64 {
            let (s, l) = embed(seed);
            for i in 0..s.chambers.len() {
                let b = bounds_of(&l, i).expect("every chamber holds floor");
                assert!(
                    b.w >= 2 && b.h >= 2,
                    "seed {seed}: chamber {i} spans {b:?} — a chamber narrower than \
                     2 cells has no interior to stand in"
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

    /// **Derived from the WILD brief, not the built one** (The Cruck, Task 3).
    /// The claim is about what `grow` covers, and production only ever grows a
    /// wild structure — a chain — so growing the grammar's four-chamber fork
    /// here would assert coverage for a shape `embed_with` never sends to this
    /// method. (It would also be a coin flip: the grower drops one link on 24
    /// of 1,536 fork/seed pairs, pinned in
    /// `classify::tests::the_grower_drops_a_link_on_exactly_these_fork_seeds`.)
    #[test]
    fn a_grown_lattice_still_covers_its_chambers_and_links() {
        let s = structure_at(&locale(), &wild(), Seed(42), WALK).expect("a cave is a site");
        let l = embed_with(&s, &wild(), extent_for(&s), Seed(42));
        for i in 0..s.chambers.len() {
            assert!(
                bounds_of(&l, i).is_some(),
                "grown chamber {i} owns no floor"
            );
        }
        assert_eq!(l.doorways.len(), s.links.len());
    }

    /// claim: structural(forall-seed) — the seed loop below is not a world
    /// sweep: it embeds the SAME hand-built structure under eight different
    /// embedding seeds and asserts the results are not all identical, which is
    /// a statement about the embedder's purity and seed-sensitivity rather
    /// than about any world. (The second, smaller loop searches seeds for a
    /// multi-chamber structure; it is a fixture search, and the assertion
    /// beside it is the premise guard, not a claim over seeds.)
    #[test]
    fn growing_is_pure_and_reads_the_seed() {
        // The structure must have MORE THAN ONE chamber for the second half to
        // mean anything: one blob floods the whole extent whatever cell it starts
        // from, so a single-chamber structure forces eight identical plans and
        // "the seed is ignored" would be a false accusation rather than a finding.
        // Asserted rather than assumed, because the count is `structure_at`'s
        // business and could move under this test.
        // The seed is SEARCHED, not written down. The WILD draw still takes its
        // chamber count from the locale's own seed, and the locale is built
        // from `WALK` — so when the walk band moved to `globe_level + 7`, seed
        // 42 started drawing a ONE-chamber structure here and this test's own
        // premise guard fired. The premise is "a structure with residual
        // freedom to fill", never "seed 42", so this asks for it.
        //
        // **The WILD brief, since The Cruck, Task 3.** This test embeds with
        // `grow`, and the wild path is the one whose count is still drawn —
        // which is what makes the search meaningful. A built brief would derive
        // the same four-chamber frame at every seed and the search would be a
        // loop that always stops at 0.
        let (seed, s) = (0u64..64)
            .find_map(|sd| {
                structure_at(&locale(), &wild(), Seed(sd), WALK)
                    .filter(|s| s.chambers.len() > 1)
                    .map(|s| (sd, s))
            })
            .expect(
                "no seed in 0..64 draws a multi-chamber structure at this locale, so the \
                 seed-sensitivity half of this test cannot be exercised at all",
            );
        assert!(
            s.chambers.len() > 1,
            "this test needs a structure with residual freedom to fill; {} chambers has none",
            s.chambers.len()
        );
        let _ = seed;
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
        let chambers: Vec<Facet> = (0..n)
            .map(|i| Facet {
                face: 3,
                path: (0..WALK).map(|_| (i % 4) as u8).collect(),
            })
            .collect();
        crate::structure::Structure {
            threshold: chambers[0].clone(),
            links: (1..n).map(|i| (i - 1, i)).collect(),
            roles: (0..n).map(crate::structure::index_role).collect(),
            chambers,
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
    fn the_extent_holds_the_fabric_as_well_as_the_rooms() {
        // The `+ (cols + 1)` stated as an arithmetic identity rather than as three
        // pinned numbers, so raising CHAMBER_SIDE fails the ceiling test above and
        // not this one. The fabric is one wall line per side plus one between each
        // pair of interiors — a formula an off-by-one would break here first.
        for (n, cols, rows) in [(1, 1, 1), (2, 2, 1), (3, 2, 2), (4, 2, 2)] {
            let e = extent_for(&structure_of(n));
            assert_eq!(e.w, cols * CHAMBER_SIDE + (cols + 1), "{n} chambers: width");
            assert_eq!(
                e.h,
                rows * CHAMBER_SIDE + (rows + 1),
                "{n} chambers: height"
            );
        }
        assert_eq!(
            (
                extent_for(&structure_of(2)).w,
                extent_for(&structure_of(2)).h
            ),
            (19, 10),
            "the two-chamber extent moved"
        );
    }

    #[test]
    fn the_exterior_shell_costs_a_fifth_to_two_fifths_of_the_plan() {
        // The shell is a deliberate cost and therefore a MEASURED one. The plan
        // text (Task 4b step 1) says "roughly 20% of the extent is the exterior
        // shell": that is the THREE-and-four-chamber figure (19%), and the
        // smallest plan pays nearly twice it (36%), because a ring's cost is a
        // perimeter against an area. Measured: 36%, 28%, 19%, 19% for one through
        // four chambers. Recorded as a range rather than as the one flattering
        // number.
        let mut measured = Vec::new();
        for n in 1..=crate::structure::MAX_CHAMBERS {
            let e = extent_for(&structure_of(n));
            let ring = 2 * (e.w + e.h) - 4;
            measured.push((n, ring, e.area(), 100 * ring / e.area()));
        }
        eprintln!("exterior shell, (chambers, ring cells, extent cells, %): {measured:?}");
        for &(n, ring, area, pct) in &measured {
            assert!(
                (18..=40).contains(&pct),
                "{n} chambers spend {ring} of {area} cells ({pct}%) on the shell — \
                 outside the 18-40% band this task measured, so either the \
                 arrangement or the claim has moved"
            );
        }
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
    /// `extent_for` can derive (4 chambers, 19x19).
    ///
    /// **Both profiles, on the same box, at the same extent — Task 5:**
    /// **~9 us** median in RELEASE (8.7 and 9.7 us on two runs — run-to-run spread
    /// at this size is a real part of the figure), **174.6 us** in DEBUG. A ~19x gap, so
    /// the profile has to be named with the number or the number means nothing.
    /// Task 4b reported "182.9 us at 19x19" without naming one and it was compared
    /// against an older debug figure; re-measuring in debug here lands at 174.6 us,
    /// which identifies 182.9 as the DEBUG number and leaves release the one that
    /// had never actually been taken at this extent. Spec §10 risk 1 wants the
    /// release number, and it is about 9 us — two orders of magnitude under the
    /// ceiling below.
    ///
    /// (The older note here claimed 27.6 us release "at Task 3's 16x16". That is
    /// superseded rather than contradicted: it was a different extent and, on the
    /// evidence of the two figures above, not a figure this extent reproduces.)
    ///
    /// Re-measured in Task 3 (it moved 3.3x when `Lattice` gained per-cell
    /// ownership), in Task 4b (the extent grew from 256 to 361 cells and the
    /// pair-valued wall set went away) and in Task 5. The measured number is
    /// printed by the test itself rather than restated here, so a reader gets the
    /// real one rather than a stale transcription.
    ///
    /// The ceiling stays where Task 1 set it. It is a falsification ceiling for a
    /// real regression — an accidental quadratic in a threshold search, say — not
    /// a target to approach.
    /// type-audit: bare-ok(count)
    const ALLOCATE_BUDGET_MICROS: u128 = 1_000;

    /// claim: readout — a benchmark distribution (median/min/p99 ns) over
    /// 1001 varying seeds, purely to avoid a single-seed allocation-cost
    /// measurement being unrepresentative (Fix round 1, Class 2: `for i in
    /// 0..SAMPLES` feeding `Seed(i as u64)`); not a world-property claim
    #[test]
    fn the_embedding_is_cheap_enough_to_re_derive() {
        // Spec §10 risk 1: no budget claim without a measurement. A lattice is
        // FRAME-tier and re-derived on every entry, so the cost that matters is
        // one call at the worst extent `extent_for` can produce.
        let s = structure_of(crate::structure::MAX_CHAMBERS);
        let extent = extent_for(&s);
        assert_eq!(
            extent.w,
            2 * CHAMBER_SIDE + 3,
            "the worst-case extent moved"
        );

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
    fn the_largest_extent_leaves_the_render_room_to_draw() {
        // Task 1 wrote this as `the_largest_plan_fits_a_terminal`, asserting
        // `w <= 72 && h <= 22` on the EXTENT as a proxy for the render, guessing
        // the render would be 1:1 plus a border. Task 4 found it was not — a wall
        // between cells forced a `(2w+1)` picture — and Task 4b makes the guess
        // right after all: a wall is a cell, so the picture is 1:1 AND the border
        // is part of the extent.
        //
        // What is left here is the bound that is genuinely `extent_for`'s, stated
        // as the arithmetic the render actually does so raising CHAMBER_SIDE fails
        // here as well as there rather than only there.
        for n in 1..=crate::structure::MAX_CHAMBERS {
            let e = extent_for(&structure_of(n));
            let drawn_columns = e.w;
            assert!(
                drawn_columns <= 80,
                "{n} chambers derive a {}x{} extent, which the render draws \
                 {drawn_columns} columns wide — past 80 a transcript wraps and a \
                 plan stops being legible",
                e.w,
                e.h,
            );
        }
    }
}
