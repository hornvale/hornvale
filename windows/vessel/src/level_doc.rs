//! `vessel/level/v1` — the underground band's spatial emit (The Gallery,
//! Task 7; spec §4).
//!
//! **Mirrors `vessel/plan/v1`, on purpose.** [`SessionLevel`] carries what
//! [`crate::plan::SessionPlan`] carries — a palette, a row-major-swept cell
//! index, `you`, `marks` — and this module's own `entry_for`/interning shape
//! is [`crate::plan::plan_of`]'s, copied rather than reinvented. Two things
//! differ, and both are load-bearing rather than cosmetic:
//!
//! # The palette interns on visibility, never on colour
//!
//! `vessel/plan/v1`'s palette interns `(CellKind, Option<[u8; 3]>)` — a
//! chamber is always fully lit or fully dark by construction, so a dim
//! "remembered" wall and a bright "here" wall would already be two distinct
//! colours, two distinct palette entries, with no extra work. Underground has
//! no such luxury: `clients/game` is a character grid, and all three of its
//! renderers withhold tint from a mark by explicit rule (the systems audit
//! credits the walk band's field of view for exactly this — its seen/
//! remembered distinction survives an uncoloured cell because it is carried
//! by a glyph twin, never a dimmed tint). Encoding fog as colour here would
//! put this band's field of view *below* the walk band's on the one axis
//! that audit measures. So the palette interns `(LevelCellKind,
//! LevelVisibility)` instead: an explicit, three-valued STATE per cell,
//! never a shade (spec §4.1).
//!
//! # `cells` is sparse: a never-seen cell is OMITTED, not flagged
//!
//! `vessel/plan/v1`'s `cells: Vec<u32>` is TOTAL over its extent — a chamber
//! is small and always fully known, so a dense row-major index per cell
//! costs nothing extra. A cave rung is not: 60×34 is 2,040 cells, most of
//! them unwalked on any given turn, and every one of them would ride along
//! in a dense grid regardless of how little the possession has actually
//! seen. Spec §4.1.1 is explicit about why that is wrong twice over — it
//! would put the whole level in the first snapshot after descending, growing
//! nothing as you explore, and it would ship the client information the
//! possession has not earned, merely asking it politely to hide what it was
//! handed. So [`SessionLevel::cells`] is a `Vec<`[`LevelCell`]`>` carrying
//! only the cells the possession has ever seen, each naming its own `(x, y)`
//! — a document that grows as the descent is explored rather than one sized
//! to the rung from the first step.
//!
//! # `marks` reuses `PlanMark`, not a fresh type
//!
//! [`crate::plan::PlanMark`]'s own doc gives the reason directly: it is the
//! focalizer's `Focalized.nouns` shape too, "because that identity is what
//! makes map and prose two grains of one lens." That argument does not care
//! which band's floor a creature stands on — a creature on this pane and the
//! same creature named in the prose must stay the same examinable thing, so
//! this module reuses the type outright rather than defining a
//! structurally-identical `LevelMark` that would sever the join for no gain.
//!
//! **As of Task 11 (spec §3.6), `marks` is no longer always empty** — a
//! chamber's own conditions may derive a resident
//! (`crate::underground::chamber_resident`), drawn here exactly like any
//! other mark. That resident is not yet party to the map/prose identity join
//! this doc argues for above: it has no entity, so `examine` underground
//! does not yet answer to its noun (`crate::underground::inhabitant_datum`
//! is this module's own copy of its flavour text, read directly off the
//! mark rather than through a ledger entity the way an NPC's `datum` is).
//! `PlanMark` is reused here for its plain STRUCTURAL shape — one noun, one
//! kind, one salience-sortable mark — not yet for the fuller identity
//! argument above; closing that gap is future work, not this task's.
//!
//! # Emit-only. Never persist this.
//!
//! `FRAME`-tier under decision 0069, exactly as [`crate::plan::plan_of`] is:
//! derived fresh from the live [`crate::underworld_level::Level`] and the
//! session's own fog-of-war bitset on every snapshot, never serialized into
//! a world file. See `plan`'s module doc for the fuller argument (a replay
//! campaign's answer is save-as-seed-plus-marks, never a recording of this
//! document).

use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

use crate::lattice::Cell;
use crate::plan::PlanMark;
use crate::underworld_level::{Level, LevelCellKind};
use hornvale_kernel::Band;

/// The schema tag every level document carries.
/// type-audit: bare-ok(identifier-text)
pub const LEVEL_SCHEMA: &str = "vessel/level/v1";

/// The wire name a habitation [`Band`] carries on `SessionLevel::rung`,
/// hand-mapped rather than read off `Band`'s `Debug` impl.
///
/// The same choice [`crate::plan::entry_for`] makes for `CellKind`: a wire
/// value is a contract this document's readers depend on, and `Debug`
/// output is not one — `Band::Nadir` was named `Sunless` until The Stope
/// renamed it (`kernel/src/band.rs`), and nothing broke only because nothing
/// serialized the old name. `Band::Surface` never actually reaches here
/// (`Underground::rung_band` only ever indexes a habitation rung), but the
/// match must still be exhaustive, so it carries a name of its own rather
/// than an `unreachable!()` that would panic a real snapshot the day this
/// assumption stops holding.
fn band_wire_name(band: Band) -> &'static str {
    match band {
        Band::Surface => "surface",
        Band::Undercroft => "undercroft",
        Band::Shallows => "shallows",
        Band::Deeps => "deeps",
        Band::Underdeep => "underdeep",
        Band::Nadir => "nadir",
    }
}

/// The three-valued illumination state a cell carries (spec §4.1, §4.1.2) —
/// a quantization of "how much light reaches this cell right now," not three
/// free-standing tags: when reach becomes light-driven (spec §3.4), a light
/// model produces a falloff rather than three buckets, and this is that
/// falloff pre-quantized to the granularity the document actually needs.
///
/// Ordered `Here < Lit < Remembered` only so the type can key a `BTreeMap`
/// alongside [`LevelCellKind`] the way `(CellKind, Option<[u8; 3]>)` already
/// does for `vessel/plan/v1` — the order carries no meaning beyond that.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum LevelVisibility {
    /// The possession's own cell.
    Here,
    /// Lit by the current shadowcast, but not the possession's own cell.
    Lit,
    /// Seen on an earlier turn of this descent; not currently lit. Monotone
    /// across a descent (spec §4.1.2's acceptance 4b) — a fact about
    /// [`crate::underground::SeenBits`]'s own representation, not something
    /// this module enforces.
    Remembered,
}

impl LevelVisibility {
    /// The wire string this state serializes as. Terrain draws from
    /// `Remembered` upward; an entity draws only from `Lit` upward (spec
    /// §4.1.2's ordering) — a rule `Session::underground_level`
    /// (`session.rs`) follows when it fills [`SessionLevel::marks`] (The
    /// Gallery, Task 11), not a rule this module enforces itself: this
    /// module carries every state it is given without filtering.
    fn as_wire(self) -> &'static str {
        match self {
            LevelVisibility::Here => "here",
            LevelVisibility::Lit => "lit",
            LevelVisibility::Remembered => "remembered",
        }
    }
}

/// One distinct `(cell kind, visibility)` pair in a level's palette. Not a
/// cell: many cells of many states share one entry.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(identifier-text: state)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LevelPaletteEntry {
    /// `"floor"`, `"wall"`, `"flooded"`, `"stairs_down"` or `"stairs_up"` —
    /// the `LevelCellKind` discriminant, never a glyph (decision 0022).
    pub kind: String,
    /// `"here"`, `"lit"` or `"remembered"` — never a colour (spec §4.1). See
    /// this module's own doc for why the palette interns on this rather than
    /// on a shade the way `vessel/plan/v1`'s does.
    pub state: String,
}

/// A level's bounds, in level-local cells — the same shape
/// [`crate::plan::PlanExtent`] carries for a chamber, kept as its own type
/// rather than shared so the two schemas can evolve independently of one
/// another.
/// type-audit: bare-ok(count: x), bare-ok(count: y), bare-ok(count: w), bare-ok(count: h)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LevelExtent {
    /// Left edge.
    pub x: i32,
    /// Top edge.
    pub y: i32,
    /// Width, in cells.
    pub w: i32,
    /// Height, in cells.
    pub h: i32,
}

/// One level-local cell position.
/// type-audit: bare-ok(index: x), bare-ok(index: y)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LevelPoint {
    /// Column.
    pub x: i32,
    /// Row.
    pub y: i32,
}

/// One cell the possession has ever seen on this rung — absent for every
/// cell it has not (spec §4.1.1: never-seen is omitted, not flagged).
/// type-audit: bare-ok(index: x), bare-ok(index: y), bare-ok(index: ix)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LevelCell {
    /// Column, level-local.
    pub x: i32,
    /// Row, level-local.
    pub y: i32,
    /// Index into `SessionLevel::palette`.
    pub ix: u32,
}

/// One `vessel/level/v1` document. Field order is JSON key order and is
/// contract — never reorder.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(identifier-text: rung), bare-ok(ratio: depth_m)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SessionLevel {
    /// Always `vessel/level/v1`.
    pub schema: String,
    /// Which habitation band this rung is — `"undercroft"`, `"shallows"`,
    /// … — hand-mapped from `Band` rather than its `Debug` text (see
    /// [`band_wire_name`]).
    pub rung: String,
    /// The rung's own evaluation depth below the surface, in metres
    /// (`hornvale_terrain::rung_evaluation_depth_m`, read at the moment the
    /// descent was generated — `Underground::depths_m`'s own doc explains
    /// why it is carried rather than recomputed here).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub depth_m: f64,
    /// The level's bounds.
    pub extent: LevelExtent,
    /// The distinct `(cell kind, visibility)` pairs, in first-seen
    /// row-major-sweep order.
    pub palette: Vec<LevelPaletteEntry>,
    /// The cells the possession has ever seen on this rung — a strict
    /// subset of the extent's own area on every turn before the whole rung
    /// is explored (spec §4.1.1). A never-seen cell is simply absent from
    /// this list, never present with a flag.
    pub cells: Vec<LevelCell>,
    /// The cell the possession stands on.
    pub you: LevelPoint,
    /// The individuals standing on the level, ascending by `(salience,
    /// noun)` exactly as `vessel/plan/v1`'s own `marks` sorts (so the bytes
    /// do not depend on discovery order). **Empty through Task 10** — no
    /// creature was placed underground at all (spec §3.6). **Since Task
    /// 11**, may carry the chamber's own derived resident: `Session::
    /// underground_level` asks `crate::underground::chamber_resident` for
    /// which species (if any) this rung's own substrate and energy can
    /// feed, and includes it here only while its cell is lit — never merely
    /// remembered (spec §4.1.2). Still `[]`, never an omitted key, whenever
    /// nobody qualifies — for the same reason `PlanMark`'s own doc gives: an
    /// omitted key and an empty list would be two representations of
    /// "nobody here."
    pub marks: Vec<PlanMark>,
}

/// Project `level` into `vessel/level/v1`.
///
/// A single row-major sweep of `level`'s own extent, exactly the order
/// [`crate::plan::plan_of`] sweeps a chamber's lattice — the difference is
/// what happens at each cell: a cell that is neither `you`, nor in `lit`,
/// nor answers `true` from `seen` is skipped outright rather than pushed
/// (spec §4.1.1).
///
/// `you` is checked before `lit`, so the possession's own cell always reads
/// `"here"` rather than the generic `"lit"` every other cell in the current
/// shadowcast gets — [`crate::lattice::shadowcast_with`]'s own contract
/// guarantees `you` is a member of `lit` regardless (`lit.insert(from)` runs
/// unconditionally), so this ordering is what actually produces the
/// distinction rather than merely documenting one that would exist anyway.
///
/// `seen` is a predicate rather than a borrowed
/// [`crate::underground::SeenBits`] on purpose: that type is `pub(crate)`,
/// and this function is part of the crate's public surface (mirroring
/// [`crate::plan::plan_of`]'s own visibility) — a closure crosses that
/// boundary without widening `SeenBits` beyond the module that owns it.
///
/// `rung`/`depth_m`/`you` come from the caller (`Session::underground_level`
/// owns the descent); this function is pure over the level and knows
/// nothing about sessions, exactly as `plan_of` knows nothing about
/// sessions either.
///
/// Each mark's cell is checked against the extent with `debug_assert!`, not
/// a hard panic and not a silent filter — the same choice `plan_of` makes,
/// for the same reason: a silent filter would hide a bug in whatever built
/// `marks`, and a panic here would crash the player's turn over it instead.
/// type-audit: bare-ok(ratio: depth_m)
pub fn level_of(
    level: &Level,
    rung: Band,
    depth_m: f64,
    you: Cell,
    lit: &BTreeSet<Cell>,
    seen: impl Fn(Cell) -> bool,
    mut marks: Vec<PlanMark>,
) -> SessionLevel {
    let e = level.extent;
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

    let mut interned: BTreeMap<(LevelCellKind, LevelVisibility), u32> = BTreeMap::new();
    let mut palette: Vec<LevelPaletteEntry> = Vec::new();
    let mut cells: Vec<LevelCell> = Vec::new();

    for y in e.y..e.y + e.h {
        for x in e.x..e.x + e.w {
            let cell = Cell(x, y);
            let state = if cell == you {
                LevelVisibility::Here
            } else if lit.contains(&cell) {
                LevelVisibility::Lit
            } else if seen(cell) {
                LevelVisibility::Remembered
            } else {
                // Never seen: OMITTED, not flagged (spec §4.1.1).
                continue;
            };
            let kind = level
                .cells
                .get(cell)
                .expect("Level::cells is total over its extent");
            let next = interned.len() as u32;
            let ix = *interned.entry((kind, state)).or_insert(next);
            if ix == next {
                palette.push(entry_for(kind, state));
            }
            cells.push(LevelCell { x, y, ix });
        }
    }

    SessionLevel {
        schema: LEVEL_SCHEMA.to_string(),
        rung: band_wire_name(rung).to_string(),
        depth_m,
        extent: LevelExtent {
            x: e.x,
            y: e.y,
            w: e.w,
            h: e.h,
        },
        palette,
        cells,
        you: LevelPoint { x: you.0, y: you.1 },
        marks,
    }
}

/// The palette entry one `(LevelCellKind, LevelVisibility)` pair becomes.
fn entry_for(kind: LevelCellKind, state: LevelVisibility) -> LevelPaletteEntry {
    let kind = match kind {
        LevelCellKind::Floor => "floor",
        LevelCellKind::Wall => "wall",
        LevelCellKind::Flooded => "flooded",
        LevelCellKind::StairsDown => "stairs_down",
        LevelCellKind::StairsUp => "stairs_up",
        // The Brattice, spec §3.5. Additive: the palette is a sparse list
        // of `(kind, state)` pairs and a client that does not know these
        // three draws them as rock, so an old client shows a wall where a
        // squeeze is and nothing breaks. A DOOR is not here — it travels in
        // `marks`, never as a palette kind (§3.7).
        LevelCellKind::Threshold => "threshold",
        LevelCellKind::Deep => "deep",
        LevelCellKind::Drop => "drop",
    };
    LevelPaletteEntry {
        kind: kind.to_string(),
        state: state.as_wire().to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lattice::Rect;
    use crate::underworld_level::CellGrid;

    /// A 5x5 level: a ring of wall around a 3x3 floor interior, with a
    /// `StairsDown` at the centre — enough shapes to exercise every
    /// `LevelCellKind` this module hand-maps except `StairsUp`, which
    /// `stairs_up_gets_its_own_kind_string` covers on its own.
    fn tiny() -> Level {
        let extent = Rect {
            x: 0,
            y: 0,
            w: 5,
            h: 5,
        };
        let mut cells = CellGrid::new(extent, LevelCellKind::Wall);
        for y in 1..4 {
            for x in 1..4 {
                cells.set(Cell(x, y), LevelCellKind::Floor);
            }
        }
        cells.set(Cell(2, 2), LevelCellKind::StairsDown);
        Level {
            extent,
            cells,
            dof: 0,
            leaf_styles: Vec::new(),
            thresholds: Vec::new(),
        }
    }

    /// An empty lit/seen world: only `you` is ever drawn.
    fn nothing_else_lit_or_seen() -> BTreeSet<Cell> {
        BTreeSet::new()
    }

    #[test]
    fn a_never_seen_cell_is_omitted_from_the_document() {
        let level = tiny();
        let lit = nothing_else_lit_or_seen();
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        let area = (level.extent.w * level.extent.h) as usize;
        assert!(
            doc.cells.len() < area,
            "a document with nothing lit or remembered beyond `you` must not \
             carry the whole extent: {} of {area}",
            doc.cells.len()
        );
        assert_eq!(
            doc.cells.len(),
            1,
            "exactly one cell (`you`) should be present: {:?}",
            doc.cells
        );
    }

    #[test]
    fn the_three_visibility_states_are_distinguishable_without_colour() {
        let level = tiny();
        let mut lit = BTreeSet::new();
        lit.insert(Cell(2, 2)); // you
        lit.insert(Cell(3, 2)); // lit, not you
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |c| c == Cell(1, 2), // remembered, not currently lit
            Vec::new(),
        );
        let states: BTreeSet<&str> = doc
            .cells
            .iter()
            .map(|c| doc.palette[c.ix as usize].state.as_str())
            .collect();
        assert_eq!(
            states,
            BTreeSet::from(["here", "lit", "remembered"]),
            "all three states must appear, distinguished by name alone: {doc:?}"
        );
        let json = serde_json::to_string(&doc).unwrap();
        assert!(
            !json.contains("color") && !json.contains("colour"),
            "visibility must never be carried as a colour: {json}"
        );
    }

    #[test]
    fn the_possessions_own_cell_reads_here_not_generic_lit() {
        let level = tiny();
        let mut lit = BTreeSet::new();
        lit.insert(Cell(2, 2));
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        let you = doc
            .cells
            .iter()
            .find(|c| c.x == 2 && c.y == 2)
            .expect("you must appear in the document");
        assert_eq!(doc.palette[you.ix as usize].state, "here");
    }

    #[test]
    fn every_index_names_a_real_palette_entry() {
        let level = tiny();
        let mut lit = BTreeSet::new();
        for y in 1..4 {
            for x in 1..4 {
                lit.insert(Cell(x, y));
            }
        }
        let doc = level_of(
            &level,
            Band::Shallows,
            10.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        for c in &doc.cells {
            assert!(
                (c.ix as usize) < doc.palette.len(),
                "cell ({}, {}) indexes palette entry {}, but the palette holds {}",
                c.x,
                c.y,
                c.ix,
                doc.palette.len()
            );
        }
    }

    #[test]
    fn a_wall_and_a_stairs_down_get_their_own_kind_strings() {
        let level = tiny();
        let mut lit = BTreeSet::new();
        lit.insert(Cell(0, 0)); // a wall corner
        lit.insert(Cell(2, 2)); // the stairs, and `you`
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        let kinds: BTreeSet<&str> = doc
            .palette
            .iter()
            .map(|entry| entry.kind.as_str())
            .collect();
        assert!(kinds.contains("wall"), "{doc:?}");
        assert!(kinds.contains("stairs_down"), "{doc:?}");
    }

    #[test]
    fn stairs_up_gets_its_own_kind_string() {
        assert_eq!(
            entry_for(LevelCellKind::StairsUp, LevelVisibility::Here).kind,
            "stairs_up"
        );
    }

    #[test]
    fn the_projection_is_deterministic() {
        let level = tiny();
        let mut lit = BTreeSet::new();
        lit.insert(Cell(2, 2));
        lit.insert(Cell(3, 2));
        let a = serde_json::to_string(&level_of(
            &level,
            Band::Deeps,
            99.0,
            Cell(2, 2),
            &lit,
            |c| c == Cell(1, 1),
            Vec::new(),
        ))
        .unwrap();
        let b = serde_json::to_string(&level_of(
            &level,
            Band::Deeps,
            99.0,
            Cell(2, 2),
            &lit,
            |c| c == Cell(1, 1),
            Vec::new(),
        ))
        .unwrap();
        assert_eq!(a, b, "same level, same inputs, same bytes");
    }

    #[test]
    fn the_standing_cell_is_carried_verbatim() {
        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        assert_eq!((doc.you.x, doc.you.y), (2, 2));
    }

    #[test]
    fn the_rung_name_is_hand_mapped_never_debug_text() {
        assert_eq!(band_wire_name(Band::Undercroft), "undercroft");
        assert_eq!(band_wire_name(Band::Shallows), "shallows");
        assert_eq!(band_wire_name(Band::Deeps), "deeps");
        assert_eq!(band_wire_name(Band::Underdeep), "underdeep");
        assert_eq!(band_wire_name(Band::Nadir), "nadir");
    }

    #[test]
    fn an_empty_marks_list_serializes_as_an_empty_array_not_an_omitted_key() {
        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        assert!(doc.marks.is_empty());
        let json = serde_json::to_string(&doc).unwrap();
        assert!(
            json.contains("\"marks\":[]"),
            "an empty marks list must still emit the key, as `[]` — an \
             omitted key would be a second representation of \"nobody here\": {json}"
        );
    }

    #[test]
    fn marks_round_trip_and_sort_by_salience_then_noun() {
        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let mark = |noun: &str, salience: u32| PlanMark {
            x: 2,
            y: 2,
            noun: noun.to_string(),
            kind: crate::purview::AGENT_MARK_KIND.to_string(),
            datum: format!("{noun} stands here."),
            salience,
        };
        let submitted = vec![mark("zeta", 5), mark("alpha", 5), mark("beta", 1)];
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            submitted,
        );
        let order: Vec<(u32, &str)> = doc
            .marks
            .iter()
            .map(|m| (m.salience, m.noun.as_str()))
            .collect();
        assert_eq!(order, vec![(1, "beta"), (5, "alpha"), (5, "zeta")]);
    }

    #[test]
    #[should_panic(expected = "mark cell is inside the extent")]
    fn a_mark_outside_the_extent_trips_the_debug_assertion() {
        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let outside = PlanMark {
            x: 99,
            y: 99,
            noun: "ghost".to_string(),
            kind: crate::purview::AGENT_MARK_KIND.to_string(),
            datum: "A ghost, somehow off the map.".to_string(),
            salience: 1,
        };
        let _ = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            vec![outside],
        );
    }

    #[test]
    fn the_schema_tag_is_carried() {
        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let doc = level_of(
            &level,
            Band::Undercroft,
            42.0,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        assert_eq!(doc.schema, LEVEL_SCHEMA);
        let json = serde_json::to_string(&doc).unwrap();
        assert!(json.contains(r#""schema":"vessel/level/v1""#));
    }

    #[test]
    fn depth_m_is_quantized_at_the_emit_boundary() {
        // Copied from `snapshot.rs`'s own quantization test, and it
        // inherited that test's defect: a naive `contains("0.33333333")`
        // passes with or without quantization, since the RAW `f64` for
        // 1/3 serializes as `0.3333333333333333`, which contains that
        // substring too. This instead checks the field's own bounded
        // serialization and proves the raw form is genuinely excluded,
        // computing both reprs from `quantize` itself rather than a
        // hand-typed literal — see `snapshot.rs`'s own fix for the same
        // shape.
        let raw = 1.0 / 3.0;
        let quantized = hornvale_kernel::quantize::quantize(raw);
        let raw_repr = serde_json::to_string(&raw).unwrap();
        let quantized_repr = serde_json::to_string(&quantized).unwrap();
        assert_ne!(
            raw_repr, quantized_repr,
            "the fixture must pick a value quantization actually changes, \
             or this test cannot distinguish quantized from raw output"
        );

        let level = tiny();
        let lit = BTreeSet::from([Cell(2, 2)]);
        let doc = level_of(
            &level,
            Band::Undercroft,
            raw,
            Cell(2, 2),
            &lit,
            |_| false,
            Vec::new(),
        );
        let json = serde_json::to_string(&doc).unwrap();
        let quantized_needle = format!("\"depth_m\":{quantized_repr}");
        let raw_needle = format!("\"depth_m\":{raw_repr}");
        assert!(
            json.contains(&quantized_needle),
            "depth_m must serialize as its QUANTIZED value ({quantized_needle}): {json}"
        );
        assert!(
            !json.contains(&raw_needle),
            "depth_m must not serialize as the raw, unquantized f64 \
             ({raw_needle}) — quantize_serde::f64_field must have run: {json}"
        );
    }
}
