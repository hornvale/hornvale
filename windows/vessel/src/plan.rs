//! `vessel/plan/v1` — the chamber band's spatial emit.
//!
//! A **palette plus a dense row-major index grid**, which is the shape
//! `scene/surrounds/v1` already uses (`biome_legend` and friends, with a
//! `u32` index per cell) and not an invention. A per-cell string would carry
//! exactly one attribute, so every later attribute would become another
//! array to keep length-synced with the grid — and the attributes actually
//! coming are not one character wide (a colour triple, an occupant's
//! `EntityId`, a temperature). A palette absorbs them as FIELDS ON AN ENTRY,
//! costing nothing per cell. The colour triple landed as
//! [`PaletteEntry::color`] and shipped **empty** for one campaign, because the
//! building-fabric and interior-illuminant models it needed were unshipped and
//! inventing either from the bedrock's daylight reflectance is what
//! `RENDER-sourced-effects` forbids. The Lantern built both, so the slot now
//! carries a value: see [`Shading`].
//!
//! # Interning is on the TYPE AND ITS COLOUR
//!
//! One consequence is worth stating where a reader meets the palette rather
//! than leaving it to be discovered. Light falls off with distance, so two
//! walls of one chamber are genuinely different colours — and while the intern
//! key was `CellKind` alone, *every wall shared one entry* and a per-cell
//! gradient could not be expressed at all. The key is therefore `(CellKind,
//! Option<[u8; 3]>)`.
//!
//! That is not a schema change: the palette was already an intern table and
//! the client already keys on the index. It costs entries rather than bytes
//! per cell, and it is bounded because the `u8` triple **is** the
//! quantization — cells whose light differs below a screen step collapse onto
//! one entry. `the_palette_stays_bounded_after_interning_on_colour` measures
//! the result rather than assuming it.
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

use crate::fabric::{FabricContext, fabric_of, reflectance_of};
use crate::lattice::{Cell, CellKind, Lattice};
use hornvale_kernel::color::{Illuminant, Observer};
use serde::Serialize;
use std::collections::BTreeMap;

/// The schema tag every plan carries.
/// type-audit: bare-ok(identifier-text)
pub const PLAN_SCHEMA: &str = "vessel/plan/v1";

/// One distinct cell **type** in a plan. Not a cell: many cells share one.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(index: chambers), bare-ok(artifact: color)
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
    /// The cell type's display colour: its fabric's reflectance under the
    /// light reaching it, sensed through the possession's own eyes.
    ///
    /// **This slot shipped deliberately empty for a campaign, and the history
    /// is the point.** The Beholding added it and left it `None`, because
    /// `CellKind::Wall` is "the building's fabric" and carried no material,
    /// and indoors the illuminant is not the sun — filling it from the
    /// bedrock's daylight reflectance would have asserted two things the world
    /// did not model. The Lantern built both models, so what fills it now is
    /// not an invention: `fabric_of` derives the material from the lithology
    /// and biome the building stands on, `light_field` sums the illuminants
    /// actually reaching the cell, and `Observer::sense` + `to_srgb` make the
    /// triple. [`Shading`] is the whole of it.
    ///
    /// **Still `None` for three distinct reasons, all legitimate**, and a
    /// consumer may not tell them apart from the wire — absence means "no
    /// colour is claimed here", never "black":
    ///
    /// - the cell type has no fabric (a `Threshold`: an opening is not a
    ///   material);
    /// - no light reaches the cell (unlit is ABSENT from the light field, not
    ///   present at zero — `light_field`'s own doc explains why that
    ///   distinction is load-bearing);
    /// - the observer step was declined (`Eyes::Off`) or the observer declares
    ///   no projection, so no honest triple exists to emit.
    ///
    /// type-audit: bare-ok(artifact: color)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
}

/// Everything a cell's colour is derived from, borrowed for one projection.
///
/// Three references and nothing owned: [`plan_of`] reads them, emits `u8`
/// triples, and keeps none of it. Passing `None` instead is the **withholding**
/// case — no observer, or a world whose terrain never fit — and it leaves every
/// [`PaletteEntry::color`] absent, which is a different claim from black and is
/// the same posture The Beholding's `Eyes::Off` already takes one band up.
pub struct Shading<'a> {
    /// Whose eyes the plan is coloured through — the possessed species' own,
    /// unless the session was asked for another.
    pub observer: &'a Observer,
    /// The ground the building stands on, which is what its fabric is derived
    /// from. One context for the whole structure: a building is on one cell of
    /// the geosphere.
    pub fabric: &'a FabricContext,
    /// What light reaches each cell. **Absent means unlit**, and a cell absent
    /// here gets no colour at all rather than a black one.
    pub light: &'a BTreeMap<Cell, Illuminant>,
}

/// The screen triple one cell wears, or `None` where no colour can honestly be
/// claimed — see [`PaletteEntry::color`] for the three ways that happens.
///
/// The last three steps of the seam in one place: reflectance, then the
/// three-way product against the light actually arriving, then the projection
/// to bytes.
fn colour_of(kind: CellKind, cell: Cell, shading: &Shading<'_>) -> Option<[u8; 3]> {
    let fabric = fabric_of(kind, shading.fabric)?;
    let light = shading.light.get(&cell)?;
    let reflectance = reflectance_of(fabric, shading.fabric);
    let signal = shading.observer.sense(&reflectance, light);
    shading.observer.to_srgb(&signal)
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
/// One row-major pass, interning each `(CellKind, colour)` pair through a
/// `BTreeMap` (both halves derive `Ord`, so this needs no new derives — and a
/// `HashMap` is banned workspace-wide anyway, decision 0005). The colour is
/// part of the key rather than of the kind because a lit room has a *gradient*:
/// see this module's own doc for why the narrower key could not express one.
///
/// `shading` is `None` wherever no colour can be claimed — the withholding
/// case, described on [`Shading`] — and the whole palette then comes back
/// exactly as it did before The Lantern.
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
    shading: Option<&Shading<'_>>,
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
    let mut interned: BTreeMap<(CellKind, Option<[u8; 3]>), u32> = BTreeMap::new();
    let mut palette: Vec<PaletteEntry> = Vec::new();
    let mut cells: Vec<u32> = Vec::with_capacity((e.w * e.h) as usize);

    for y in e.y..e.y + e.h {
        for x in e.x..e.x + e.w {
            let cell = Cell(x, y);
            // `Lattice::cells` is documented TOTAL over the extent, and §7
            // rule 3 checks it rather than trusting the sentence. A miss is
            // a broken invariant upstream, not a cell to paper over — an
            // `unwrap_or(Wall)` here would draw a hole as solid rock and
            // hide the bug.
            let kind = *lattice
                .cells
                .get(&cell)
                .expect("Lattice::cells is total over its extent");
            let color = shading.and_then(|s| colour_of(kind, cell, s));
            let next = interned.len() as u32;
            let ix = *interned.entry((kind, color)).or_insert(next);
            if ix == next {
                palette.push(entry_for(kind, color));
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

/// The palette entry one `(CellKind, colour)` pair becomes.
///
/// The colour is handed in rather than computed here: it is a property of a
/// *cell* (what light reached it), and this function only knows the type.
fn entry_for(kind: CellKind, color: Option<[u8; 3]>) -> PaletteEntry {
    match kind {
        CellKind::Wall => PaletteEntry {
            kind: "wall".to_string(),
            chambers: Vec::new(),
            color,
        },
        CellKind::Floor(i) => PaletteEntry {
            kind: "floor".to_string(),
            chambers: vec![i],
            color,
        },
        CellKind::Threshold(a, b) => PaletteEntry {
            kind: "threshold".to_string(),
            chambers: vec![a, b],
            color,
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
        assert_eq!(
            p.cells.len(),
            (p.extent.w * p.extent.h) as usize,
            "the grid must carry exactly one index per cell of the extent — \
             `Lattice::cells` is total and the projection must not lose that"
        );
    }

    #[test]
    fn every_index_names_a_real_palette_entry() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let p = plan_of(&lat, 0, 2, 7, Cell(1, 1), Vec::new(), None);
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let a =
            serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1), Vec::new(), None)).unwrap();
        let b =
            serde_json::to_string(&plan_of(&lat, 0, 1, 7, Cell(1, 1), Vec::new(), None)).unwrap();
        assert_eq!(a, b, "same lattice, same bytes");
    }

    #[test]
    fn the_standing_cell_is_carried_verbatim() {
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), vec![goblin.clone()], None);
        assert_eq!(p.marks, vec![goblin]);
    }

    #[test]
    fn marks_serialize_in_ascending_salience_then_noun_order() {
        // Submitted out of order on purpose: the projection, not the caller,
        // must be what makes the bytes deterministic.
        let submitted = vec![mark("zeta", 5), mark("alpha", 5), mark("beta", 1)];
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), submitted, None);
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
        let p = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
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
        let _ = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), vec![outside], None);
    }

    /// WITHHOLDING, not black. With no [`Shading`] — the possession declined
    /// the observer step (`Eyes::Off`), or the world's terrain never fit —
    /// every entry's colour is absent and the key is not emitted at all. A
    /// consumer therefore cannot mistake "no colour is claimed" for "this cell
    /// is black", which is the same distinction `light_field` draws between an
    /// absent cell and a zero illuminant.
    ///
    /// FIRES WHEN: a default colour is invented for the unshaded path, or the
    /// serializer starts emitting `"color":null`.
    #[test]
    fn an_unshaded_plan_claims_no_colour_at_all() {
        let plan = plan_of(&tiny(), 0, 1, 7, Cell(1, 1), Vec::new(), None);
        for e in &plan.palette {
            assert!(
                e.color.is_none(),
                "{} claimed a colour with no light",
                e.kind
            );
        }
        let json = serde_json::to_string(&plan).unwrap();
        assert!(!json.contains("\"color\""), "an absent slot emits no key");
    }

    /// A synthetic ground for the interning tests below. **Authored, and it
    /// says so**: nothing here is a claim about what a building looks like —
    /// H1 and H2 measure that on real terrain in
    /// `windows/vessel/tests/lantern_{fabric,seam}.rs`. What these tests need
    /// is a fabric that exists, so that the *interning* can be exercised.
    fn synthetic_ground() -> FabricContext {
        FabricContext {
            rock: hornvale_terrain::lithology::RockClass::Granite,
            material: hornvale_terrain::lithology::MaterialBuffer {
                silica: 0.6,
                grain: 0.5,
                induration: 0.7,
                carbonate: 0.1,
                metamorphic_grade: 0.0,
                porosity: 0.2,
                margin: hornvale_terrain::lithology::MarginPolarity::Interior,
                soil_depth: hornvale_terrain::lithology::SoilDepth::new(0.5),
                basement: hornvale_terrain::lithology::Basement::Continental,
                thaumic: 0.0,
            },
            forested: false,
            temperate: false,
            deep_soil: false,
            dry: false,
        }
    }

    /// **The reason the intern key widened.** Two walls of one chamber under
    /// different light are different colours, and while the key was `CellKind`
    /// alone the palette could not say so — every wall shared one entry, so a
    /// per-cell gradient was inexpressible however correct the light field was.
    ///
    /// The two walls here are lit at strictly different levels, so a palette
    /// that still interned on the kind alone would return ONE wall entry and
    /// this fails.
    ///
    /// FIRES WHEN: the intern key narrows back to the kind, or the colour stops
    /// reaching `entry_for`.
    #[test]
    fn two_walls_at_different_light_levels_get_different_palette_entries() {
        let ground = synthetic_ground();
        let observer = hornvale_kernel::color::standard_observer();
        let bright = hornvale_kernel::color::blackbody(crate::light::TORCH_KELVIN);
        let dim = crate::light::attenuate(&bright, 3.0);
        let mut light = BTreeMap::new();
        light.insert(Cell(0, 0), bright);
        light.insert(Cell(2, 2), dim);
        let plan = plan_of(
            &tiny(),
            0,
            1,
            7,
            Cell(1, 1),
            Vec::new(),
            Some(&Shading {
                observer: &observer,
                fabric: &ground,
                light: &light,
            }),
        );
        let wall_colours: std::collections::BTreeSet<Option<[u8; 3]>> = plan
            .palette
            .iter()
            .filter(|e| e.kind == "wall")
            .map(|e| e.color)
            .collect();
        assert_eq!(
            wall_colours.len(),
            3,
            "the eight wall cells hold three distinct lightings (bright, dim, \
             unlit) and must intern to three entries, not {:?}",
            plan.palette
        );
        assert!(
            wall_colours.contains(&None),
            "the six unlit wall cells must stay colourless: {wall_colours:?}"
        );
    }

    /// An unlit cell gets NO colour, not a black one — the palette's half of
    /// the distinction `light_field` draws by leaving unreached cells out of
    /// its map entirely. `[0, 0, 0]` and absence render alike and are different
    /// models, and only absence lets a client decide for itself what unseen
    /// looks like.
    ///
    /// FIRES WHEN: a missing light is defaulted to a zero illuminant somewhere
    /// on the path, which would give every dark cell a black triple.
    #[test]
    fn a_cell_no_light_reaches_gets_no_colour_rather_than_black() {
        let ground = synthetic_ground();
        let observer = hornvale_kernel::color::standard_observer();
        // The floor cell is lit; every wall cell is outside the field.
        let mut light = BTreeMap::new();
        light.insert(
            Cell(1, 1),
            hornvale_kernel::color::blackbody(crate::light::TORCH_KELVIN),
        );
        let plan = plan_of(
            &tiny(),
            0,
            1,
            7,
            Cell(1, 1),
            Vec::new(),
            Some(&Shading {
                observer: &observer,
                fabric: &ground,
                light: &light,
            }),
        );
        let floor = plan
            .palette
            .iter()
            .find(|e| e.kind == "floor")
            .expect("the floor reached the palette");
        assert!(
            floor.color.is_some(),
            "the lit floor cell carries no colour, so absence below proves \
             nothing about darkness"
        );
        for wall in plan.palette.iter().filter(|e| e.kind == "wall") {
            assert_eq!(
                wall.color, None,
                "an unlit wall was given a colour: unlit must be ABSENT, not black"
            );
        }
    }
}
