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
//! and belong in a marks list keyed by cell. The Sighting is that list's
//! first writer: `SessionPlan.marks` carries [`PlanMark`], each one a
//! `scene/surrounds/v2` [`Mark`](hornvale_scene::Mark) plus a cell — the same
//! shape deliberately, because it is the focalizer's `Focalized.nouns` shape
//! too, and that identity is what makes map and prose two grains of one
//! lens. A creature on the pane and a creature in the prose are the same
//! examinable thing; a free-form shape here would break that join.
//!
//! # Emit-only. Never persist this.
//!
//! Decision 0069's law is that an entity's PERSISTED position is its room and
//! no SAVED state may point into the fine layer — its stated consequence
//! being that nothing stored points there, "so it may regenerate differently
//! forever without corrupting a world." A snapshot is derived fresh each turn
//! and discarded, so it has exactly that property and this is legal.
//!
//! Writing one to disk is not — if what gets written is state the sim will
//! later **read back and resume from**. A replay file or a morgue file that
//! a later run replays forward would point into the fine layer and break
//! 0069 **by the saving**. `CLIENT-coverage-matrix` already holds the answer
//! a replay campaign wants: save-as-seed-plus-marks — a seed and the verb
//! sequence, replayed to regenerate snapshots, never a recording of them.
//!
//! That is a different thing from a **golden witness**: a fixture compared
//! byte-for-byte to catch drift, never fed back into a session to resume
//! play from. 0069 governs the first kind, not the second — a witness is
//! read by a diff, not by the sim. This campaign's own
//! `windows/vessel/tests/fixtures/snapshot-seed-42-{walk,chamber}.json`
//! commit plan cells to disk and are exactly that second kind.

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

/// A single individual standing on a cell of the plan — the palette's
/// deliberate complement, keyed by position rather than shared by type.
///
/// The shape is a `scene/surrounds/v2` [`Mark`](hornvale_scene::Mark) plus a
/// cell, and that is not an invention: `Mark`'s own doc chose `{noun, kind,
/// datum, salience}` because it is the focalizer's `Focalized.nouns` shape,
/// "because that identity is what makes map and prose two grains of one
/// lens." Reusing it here means a creature on this pane and the same
/// creature named in the prose are the same examinable thing — `examine
/// goblin` resolves identically whichever grain asked. A free-form shape
/// here (a different field set, a different key order) would sever that
/// join for no gain, since every field a mark could reasonably carry is
/// already in `Mark`.
/// type-audit: bare-ok(index: x), bare-ok(index: y), bare-ok(identifier-text: noun), bare-ok(identifier-text: kind), bare-ok(prose: datum), bare-ok(index: salience)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlanMark {
    /// Column, lattice-local — the same frame [`PlanPoint`] uses.
    pub x: i32,
    /// Row, lattice-local.
    pub y: i32,
    /// The examinable noun, shared with the prose's own noun catalog.
    pub noun: String,
    /// What kind of thing this is: `"settlement"`, `"agent"`, …
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
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
    /// The individuals standing on the plan, ascending by `(salience,
    /// noun)` so the bytes do not depend on discovery order. Empty is
    /// `[]`, never an omitted key — an omitted key and an empty list would
    /// be two representations of "nobody here."
    pub marks: Vec<PlanMark>,
}

/// Project `lattice` into `vessel/plan/v1`.
///
/// One row-major pass, interning each `CellKind` through a `BTreeMap`
/// (`CellKind` derives `Ord`, so this needs no new derives — and a
/// `HashMap` is banned workspace-wide anyway, decision 0005).
///
/// `at`/`of`/`chamber`/`you` come from the session, which owns them;
/// this function knows nothing about sessions. `marks` comes from the
/// caller too — this function is pure over the lattice and knows nothing
/// about sight, so it neither filters nor sorts marks by visibility; the
/// caller decides which individuals are visible and passes only those.
///
/// Each mark's cell is checked against the extent with `debug_assert!`, not
/// a hard panic and not a silent filter. A silent filter would hide a bug
/// in whatever built `marks` (a Task 5 concern, not this module's); a panic
/// in this snapshot path would turn that same bug into a crash of the
/// player's turn. `debug_assert!` catches it in every test and debug run —
/// which is where this campaign's own coverage lives — without putting a
/// release build at risk of a panic over player-visible state.
/// type-audit: bare-ok(index: at), bare-ok(count: of), bare-ok(index: chamber)
pub fn plan_of(
    lattice: &Lattice,
    at: usize,
    of: usize,
    chamber: u64,
    you: Cell,
    mut marks: Vec<PlanMark>,
) -> SessionPlan {
    let e = lattice.extent;
    for m in &marks {
        debug_assert!(
            m.x >= e.x && m.x < e.x + e.w && m.y >= e.y && m.y < e.y + e.h,
            "mark cell is inside the extent: ({}, {}) is outside x[{}, {}) y[{}, {})",
            m.x,
            m.y,
            e.x,
            e.x + e.w,
            e.y,
            e.y + e.h
        );
    }
    marks.sort_by(|a, b| {
        a.salience
            .cmp(&b.salience)
            .then_with(|| a.noun.cmp(&b.noun))
    });
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
        marks,
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
        assert_eq!(
            p.cells.len(),
            (p.extent.w * p.extent.h) as usize,
            "the grid must carry exactly one index per cell of the extent — \
             `Lattice::cells` is total and the projection must not lose that"
        );
    }

    #[test]
    fn every_index_names_a_real_palette_entry() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
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
        let p = plan_of(&lat, 0, 2, 7, Cell(1, 1), Vec::new());
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
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
        let a = serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1), Vec::new())).unwrap();
        let b = serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1), Vec::new())).unwrap();
        assert_eq!(a, b, "same lattice, same bytes");
    }

    #[test]
    fn the_standing_cell_is_carried_verbatim() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
        assert_eq!((p.you.x, p.you.y), (1, 1));
    }

    /// A mark on the floor cell at `(1, 1)` of `tiny()`.
    ///
    /// `kind` is [`crate::purview::AGENT_MARK_KIND`], not a literal, even though
    /// this fixture exercises extent and ordering and never a real creature: it
    /// is the nearest copy-source a later campaign would crib a `PlanMark` from,
    /// and a synthetic `"creature"` here is exactly how the value that fix round
    /// 1 unified would get re-forked.
    fn mark(noun: &str, salience: u32) -> PlanMark {
        PlanMark {
            x: 1,
            y: 1,
            noun: noun.to_string(),
            kind: crate::purview::AGENT_MARK_KIND.to_string(),
            datum: format!("{noun} stands here."),
            salience,
        }
    }

    #[test]
    fn marks_round_trip_into_the_plan() {
        let goblin = mark("goblin", 5);
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), vec![goblin.clone()]);
        assert_eq!(p.marks, vec![goblin]);
    }

    #[test]
    fn marks_serialize_in_ascending_salience_then_noun_order() {
        // Submitted out of order on purpose: the projection, not the caller,
        // must be what makes the bytes deterministic.
        let submitted = vec![mark("zeta", 5), mark("alpha", 5), mark("beta", 1)];
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), submitted);
        let order: Vec<(u32, &str)> = p
            .marks
            .iter()
            .map(|m| (m.salience, m.noun.as_str()))
            .collect();
        assert_eq!(
            order,
            vec![(1, "beta"), (5, "alpha"), (5, "zeta")],
            "marks must be ascending by (salience, noun) regardless of \
             discovery order, or the emitted bytes would depend on it"
        );
    }

    #[test]
    fn an_empty_marks_list_serializes_as_an_empty_array_not_an_omitted_key() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new());
        assert!(p.marks.is_empty());
        let json = serde_json::to_string(&p).unwrap();
        assert!(
            json.contains("\"marks\":[]"),
            "an empty marks list must still emit the key, as `[]` — an \
             omitted key would be a second representation of \"nobody here\": {json}"
        );
    }

    #[test]
    #[should_panic(expected = "mark cell is inside the extent")]
    fn a_mark_outside_the_extent_trips_the_debug_assertion() {
        // `tiny()`'s extent is 3x3 at the origin; (99, 99) is nowhere near it.
        let outside = PlanMark {
            x: 99,
            y: 99,
            noun: "ghost".to_string(),
            kind: crate::purview::AGENT_MARK_KIND.to_string(),
            datum: "A ghost, somehow off the map.".to_string(),
            salience: 1,
        };
        let _ = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), vec![outside]);
    }
}
