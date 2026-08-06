//! `vessel/plan/v1` — the chamber band's spatial emit.
//!
//! A **palette plus a dense row-major index grid**, which is the shape
//! `scene/surrounds/v1` already uses (`biome_legend` and friends, with a
//! `u32` index per cell) and not an invention. A per-cell string would carry
//! exactly one attribute, so every later attribute would become another
//! array to keep length-synced with the grid — and the attributes actually
//! coming are not one character wide (a colour triple, an occupant's
//! `EntityId`, a temperature). A palette absorbs them as FIELDS ON AN ENTRY,
//! costing nothing per cell.
//!
//! # Types here, instances elsewhere
//!
//! A palette entry is a cell **type**. `CellKind`'s own doc closes the enum
//! at three variants on purpose — "the moment this enum becomes the place
//! where richness lives, the lattice is a tile catalogue and
//! `CLIENT-language-not-catalogue` has been violated one band down. A window
//! is an ANCHOR at a wall cell, never `CellKind::Window`." The palette
//! inherits that discipline exactly: type-level attributes (all walls are
//! grey) may join an entry; **individual** things standing on a cell may not,
//! and belong in a marks list keyed by cell. The Sighting is what adds one —
//! this schema has no marks field yet, because a field nothing writes cannot
//! be seen to be wrong.
//!
//! # Emit-only. Never persist this.
//!
//! Decision 0069's law is that an entity's PERSISTED position is its room and
//! no SAVED state may point into the fine layer — its stated consequence
//! being that nothing stored points there, "so it may regenerate differently
//! forever without corrupting a world." A snapshot is derived fresh each turn
//! and discarded, so it has exactly that property and this is legal.
//!
//! Writing one to disk is not. A replay file or a morgue file carrying these
//! cells would point into the fine layer and break 0069 **by the saving**.
//! `CLIENT-coverage-matrix` already holds the answer a replay campaign wants:
//! save-as-seed-plus-marks — a seed and the verb sequence, replayed to
//! regenerate snapshots, never a recording of them.

use crate::lattice::{Cell, CellKind, Lattice};
use serde::Serialize;
use std::collections::BTreeMap;

/// The schema tag every plan carries.
/// type-audit: bare-ok(identifier-text)
pub const PLAN_SCHEMA: &str = "vessel/plan/v1";

/// One distinct cell **type** in a plan. Not a cell: many cells share one.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(index: chambers)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PaletteEntry {
    /// `"wall"`, `"floor"` or `"threshold"` — the `CellKind` discriminant,
    /// never a glyph. The client chooses `#`/`.`/`+` (decision 0022).
    pub kind: String,
    /// Which chambers this cell type serves: none for a wall, one for a
    /// floor, **two for a threshold** — `CellKind::serves` is a predicate
    /// because that question has two right answers, and this must not pick
    /// one of them.
    pub chambers: Vec<usize>,
}

/// The plan's bounds, in lattice-local cells.
/// type-audit: bare-ok(count: x), bare-ok(count: y), bare-ok(count: w), bare-ok(count: h)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct PlanExtent {
    /// Left edge.
    pub x: i32,
    /// Top edge.
    pub y: i32,
    /// Width, in cells.
    pub w: i32,
    /// Height, in cells.
    pub h: i32,
}

/// One lattice-local cell position.
/// type-audit: bare-ok(index: x), bare-ok(index: y)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct PlanPoint {
    /// Column.
    pub x: i32,
    /// Row.
    pub y: i32,
}

/// One `vessel/plan/v1` document. Field order is JSON key order and is
/// contract — never reorder.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(index: chamber), bare-ok(index: at), bare-ok(count: of), bare-ok(index: cells)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SessionPlan {
    /// Always `vessel/plan/v1`.
    pub schema: String,
    /// The chamber id the prose names, so pane and prose agree on which
    /// room is meant.
    pub chamber: u64,
    /// Which chamber of the structure, zero-based.
    pub at: usize,
    /// How many chambers the structure has.
    pub of: usize,
    /// The plan's bounds.
    pub extent: PlanExtent,
    /// The distinct cell types, in first-seen row-major order.
    pub palette: Vec<PaletteEntry>,
    /// One palette index per cell, row-major. Length is exactly `w * h`.
    pub cells: Vec<u32>,
    /// The cell the possession stands in.
    pub you: PlanPoint,
}

/// Project `lattice` into `vessel/plan/v1`.
///
/// One row-major pass, interning each `CellKind` through a `BTreeMap`
/// (`CellKind` derives `Ord`, so this needs no new derives — and a
/// `HashMap` is banned workspace-wide anyway, decision 0005).
///
/// `at`/`of`/`chamber`/`you` come from the session, which owns them;
/// this function knows nothing about sessions.
/// type-audit: bare-ok(index: at), bare-ok(count: of), bare-ok(index: chamber)
pub fn plan_of(lattice: &Lattice, at: usize, of: usize, chamber: u64, you: Cell) -> SessionPlan {
    let e = lattice.extent;
    let mut interned: BTreeMap<CellKind, u32> = BTreeMap::new();
    let mut palette: Vec<PaletteEntry> = Vec::new();
    let mut cells: Vec<u32> = Vec::with_capacity((e.w * e.h) as usize);

    for y in e.y..e.y + e.h {
        for x in e.x..e.x + e.w {
            // `Lattice::cells` is documented TOTAL over the extent, and §7
            // rule 3 checks it rather than trusting the sentence. A miss is
            // a broken invariant upstream, not a cell to paper over — an
            // `unwrap_or(Wall)` here would draw a hole as solid rock and
            // hide the bug.
            let kind = *lattice
                .cells
                .get(&Cell(x, y))
                .expect("Lattice::cells is total over its extent");
            let next = interned.len() as u32;
            let ix = *interned.entry(kind).or_insert(next);
            if ix == next {
                palette.push(entry_for(kind));
            }
            cells.push(ix);
        }
    }

    SessionPlan {
        schema: PLAN_SCHEMA.to_string(),
        chamber,
        at,
        of,
        extent: PlanExtent {
            x: e.x,
            y: e.y,
            w: e.w,
            h: e.h,
        },
        palette,
        cells,
        you: PlanPoint { x: you.0, y: you.1 },
    }
}

/// The palette entry one `CellKind` becomes.
fn entry_for(kind: CellKind) -> PaletteEntry {
    match kind {
        CellKind::Wall => PaletteEntry {
            kind: "wall".to_string(),
            chambers: Vec::new(),
        },
        CellKind::Floor(i) => PaletteEntry {
            kind: "floor".to_string(),
            chambers: vec![i],
        },
        CellKind::Threshold(a, b) => PaletteEntry {
            kind: "threshold".to_string(),
            chambers: vec![a, b],
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lattice::{Cell, CellKind, Lattice, Rect};
    use std::collections::BTreeMap;

    /// A 3x3 plan: a ring of wall around one floor cell of chamber 0.
    fn tiny() -> Lattice {
        let mut cells = BTreeMap::new();
        for y in 0..3 {
            for x in 0..3 {
                let kind = if x == 1 && y == 1 {
                    CellKind::Floor(0)
                } else {
                    CellKind::Wall
                };
                cells.insert(Cell(x, y), kind);
            }
        }
        Lattice {
            extent: Rect {
                x: 0,
                y: 0,
                w: 3,
                h: 3,
            },
            cells,
            doorways: Vec::new(),
            dof: 0,
        }
    }

    #[test]
    fn the_index_grid_is_total_over_the_extent() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        assert_eq!(
            p.cells.len(),
            (p.extent.w * p.extent.h) as usize,
            "the grid must carry exactly one index per cell of the extent — \
             `Lattice::cells` is total and the projection must not lose that"
        );
    }

    #[test]
    fn every_index_names_a_real_palette_entry() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        for (i, &ix) in p.cells.iter().enumerate() {
            assert!(
                (ix as usize) < p.palette.len(),
                "cell {i} indexes palette entry {ix}, but the palette holds {}",
                p.palette.len()
            );
        }
    }

    #[test]
    fn the_palette_holds_no_entry_the_building_does_not_use() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        for (ix, entry) in p.palette.iter().enumerate() {
            assert!(
                p.cells.contains(&(ix as u32)),
                "palette entry {ix} ({entry:?}) is referenced by no cell — \
                 the projection invented a cell type the building does not have"
            );
        }
    }

    #[test]
    fn a_wall_owns_no_chamber_and_a_floor_owns_exactly_one() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        for entry in &p.palette {
            let expected = match entry.kind.as_str() {
                "wall" => 0,
                "floor" => 1,
                "threshold" => 2,
                other => panic!("unknown cell kind {other:?} in the palette"),
            };
            assert_eq!(
                entry.chambers.len(),
                expected,
                "a {:?} entry must name {expected} chamber(s), found {:?}",
                entry.kind,
                entry.chambers
            );
        }
    }

    #[test]
    fn a_threshold_keeps_both_of_its_chambers() {
        // `CellKind::serves` is a predicate precisely because "whose is this
        // doorway" has two right answers. The palette must not pick one.
        let mut lat = tiny();
        lat.cells.insert(Cell(1, 0), CellKind::Threshold(0, 1));
        let p = plan_of(&lat, 0, 2, 7, Cell(1, 1));
        let door = p
            .palette
            .iter()
            .find(|e| e.kind == "threshold")
            .expect("the threshold reached the palette");
        assert_eq!(door.chambers, vec![0, 1]);
    }

    #[test]
    fn identical_cell_kinds_share_one_palette_entry() {
        // The whole economy of the palette: 8 wall cells, 1 palette entry.
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        assert_eq!(
            p.palette.len(),
            2,
            "one wall type and one floor type, however many cells wear them: {:?}",
            p.palette
        );
    }

    #[test]
    fn the_projection_is_deterministic() {
        let lat = tiny();
        let a = serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1))).unwrap();
        let b = serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1))).unwrap();
        assert_eq!(a, b, "same lattice, same bytes");
    }

    #[test]
    fn the_standing_cell_is_carried_verbatim() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1));
        assert_eq!((p.you.x, p.you.y), (1, 1));
    }
}
