//! The chamber-band floor plan (`vessel/plan/v1`) drawn into the cell grid.
//!
//! ## The lattice-to-grid projection
//!
//! Unlike [`crate::chart`]'s triangular-lattice projection, this one is
//! trivial: `windows/vessel/src/plan.rs::plan_of` already emits `cells` as a
//! dense row-major grid over `extent`, and `windows/vessel/src/lattice/
//! render.rs::render` (the sim's own console `map` verb, indoors) draws that
//! exact same grid 1:1 — "One glyph per cell, read straight off
//! `Lattice::cells`... There is no arithmetic between a picture position and
//! a cell, so there is no off-by-one class to get wrong." This module keeps
//! that property: cell `i` (`0`-based, row-major) lands at `(origin.x + i %
//! w, origin.y + i / w)`, and [`draw`]'s own golden test
//! (`tests/plan.rs::the_shape_matches_the_sims_own_ascii_render`) pins the
//! result against that same `map` verb's real output for seed 42, the way
//! `chart.rs`'s `the_shape_matches_the_sims_own_ascii_render` pins the
//! walk-band chart.
//!
//! `plan.you` and each [`crate::PlanMark`]'s `(x, y)` are lattice-local
//! coordinates in the *same* frame as `extent` (not `0`-based within it), so
//! both are offset by `(extent.x, extent.y)` before landing on the grid —
//! exactly the offset `plan_of`'s own doc describes the extent as carrying.
//!
//! ## Glyphs come from the palette, not a hard-coded index map
//!
//! `PaletteEntry` (`windows/vessel/src/plan.rs`) carries a `kind: String` —
//! `"wall"`, `"floor"`, or `"threshold"` — and deliberately no glyph field:
//! "The client chooses `#`/`.`/`+`" is `PaletteEntry::kind`'s own doc
//! comment (decision 0022). [`glyph_of`] is that choice, made once, in the
//! same shape `chart.rs`'s `glyph_of` makes it for chart cell states: a
//! `match` on the string the producer emits, not a positional assumption
//! about which palette index means what. A cell's glyph therefore always
//! comes from *reading* its palette entry's `kind`, never from where that
//! entry happens to sit in the `palette` array — swap two entries' order
//! and every cell that references them by index still draws correctly.
//!
//! An unrecognised `kind` string draws as [`WALL_GLYPH`], matching the sim's
//! own fallback: `lattice/render.rs::glyph`'s `None => WALL` arm treats "a
//! cell the map does not hold" as solid rock rather than a hole, and
//! `CellKind`'s own doc closes the enum at three variants on purpose, so a
//! fourth string here is a schema change the client did not agree to, not a
//! shape to invent ink for.
//!
//! ## Marks carry no glyph of their own this campaign
//!
//! The shipped vocabulary is four glyphs total (`#`/`.`/`+`/`@`), and none
//! is spare for `PlanMark`s the way `chart.rs`'s `PLACED_GLYPH` (`+`) was
//! spare for the walk band — every plan glyph is already claimed by a
//! `CellKind` or by `you`. Task 6's `chart.rs` met the identical question
//! (see that module's doc and `task-6-report.md`'s "Marks" section) and
//! answered it the same way this module does: the marks pass redraws the
//! *cell's own* glyph (from its palette entry, exactly as the cells pass
//! would) at the marked position, so it is a structural no-op against today's
//! vocabulary but guarantees a marked cell wins its box over a plain terrain
//! cell should a future geometry ever collide two cells onto one grid
//! position — which cannot happen under this module's 1:1 projection, but
//! could under a denser one later. `you` is drawn between the two passes
//! (cells, then `you`, then marks) per the brief; a mark is never emitted at
//! the possession's own cell (`PlanMark` describes *other* individuals —
//! `windows/vessel/src/session.rs`'s sighting-to-mark path only walks
//! creatures found by sight, not the possession itself), so the marks pass
//! never has occasion to overdraw the `@` it follows.

use crate::{Cell, Plan, PlanMark, Source, Weight};

/// The glyph for a wall cell — the building's fabric, impassable.
const WALL_GLYPH: char = '#';
/// The glyph for a floor cell — a cell a mover may stand in.
const FLOOR_GLYPH: char = '.';
/// The glyph for a threshold cell — a doorway between two chambers.
const THRESHOLD_GLYPH: char = '+';
/// The glyph for the cell the possession stands in. The same mark the
/// walk-band chart uses for `here` (`chart::HERE_GLYPH`), deliberately: one
/// verb, two bands, one "you are here" mark to learn.
const YOU_GLYPH: char = '@';

/// Glyph for a palette entry's `kind` string. See the module doc for why
/// this reads the string rather than assuming a palette index's meaning,
/// and why an unrecognised kind draws as [`WALL_GLYPH`].
fn glyph_of(kind: &str) -> char {
    match kind {
        "floor" => FLOOR_GLYPH,
        "threshold" => THRESHOLD_GLYPH,
        // "wall", and anything this client does not recognise: the sim's
        // own `lattice/render.rs::glyph` treats an unmapped cell the same
        // way — solid rock, never a hole.
        _ => WALL_GLYPH,
    }
}

/// The glyph a plan cell at row-major index `i` draws, read from its
/// palette entry. `None` if `i` or the index it names is out of range —
/// both schema violations the producer does not allow (`plan.cells`' own
/// doc: "Length is exactly `w * h`"; `plan_of`'s own doc: "every index
/// names a real palette entry"), but this function stays infallible rather
/// than panicking on a malformed wire document.
fn cell_glyph(plan: &Plan, i: usize) -> Option<char> {
    let ix = *plan.cells.get(i)?;
    let entry = plan.palette.get(ix as usize)?;
    Some(glyph_of(&entry.kind))
}

/// The grid position for a lattice-local point `(x, y)`, offset by the
/// plan's own extent and anchored at `origin` — the same "relative to
/// origin" contract [`crate::chart::draw`] follows. Returns `None` for a
/// point outside `into`'s bounds, matching `Grid::set`'s own discipline of
/// refusing an out-of-range write rather than wrapping it.
fn grid_pos(
    plan: &Plan,
    x: i32,
    y: i32,
    origin: (u16, u16),
    into: &crate::Grid,
) -> Option<(u16, u16)> {
    let gx = origin.0 as i64 + (x - plan.extent.x) as i64;
    let gy = origin.1 as i64 + (y - plan.extent.y) as i64;
    if gx < 0 || gy < 0 || gx >= into.width() as i64 || gy >= into.height() as i64 {
        None
    } else {
        Some((gx as u16, gy as u16))
    }
}

/// Draw `plan` into `into`, anchored so the plan's own `(extent.x,
/// extent.y)` lands at `origin`. Three passes, in order: every cell by its
/// palette glyph, then `you` as `@`, then marks — see the module doc for
/// why the marks pass draws no glyph a cells-only render would not already
/// have drawn, and why that is still the correct structure to ship.
pub fn draw(plan: &Plan, into: &mut crate::Grid, origin: (u16, u16)) {
    let w = plan.extent.w;
    if w > 0 {
        for i in 0..plan.cells.len() {
            let col = (i as i32) % w;
            let row = (i as i32) / w;
            let x = plan.extent.x + col;
            let y = plan.extent.y + row;
            let Some(glyph) = cell_glyph(plan, i) else {
                continue;
            };
            if let Some((gx, gy)) = grid_pos(plan, x, y, origin, into) {
                into.set(gx, gy, Cell::glyph(glyph, Weight::Normal, Source::Plan));
            }
        }
    }

    if let Some((gx, gy)) = grid_pos(plan, plan.you.x, plan.you.y, origin, into) {
        into.set(gx, gy, Cell::glyph(YOU_GLYPH, Weight::Bold, Source::Plan));
    }

    for m in &plan.marks {
        draw_mark(plan, m, origin, into);
    }
}

/// One mark's contribution to the marks pass: re-draw the glyph its own
/// cell's palette entry already names, at that cell's grid position. See
/// the module doc's "Marks carry no glyph of their own" section.
fn draw_mark(plan: &Plan, m: &PlanMark, origin: (u16, u16), into: &mut crate::Grid) {
    let w = plan.extent.w;
    if w <= 0 {
        return;
    }
    let col = m.x - plan.extent.x;
    let row = m.y - plan.extent.y;
    if col < 0 || row < 0 || col >= w {
        return;
    }
    let i = row as usize * w as usize + col as usize;
    let Some(glyph) = cell_glyph(plan, i) else {
        return;
    };
    if let Some((gx, gy)) = grid_pos(plan, m.x, m.y, origin, into) {
        into.set(gx, gy, Cell::glyph(glyph, Weight::Normal, Source::Plan));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PlanExtent, PlanPoint};

    fn small_plan() -> Plan {
        // A 3x2 plan: wall, floor, threshold on row 0; wall, floor, wall on
        // row 1. `you` stands on the floor at (1, 1).
        Plan {
            extent: PlanExtent {
                x: 0,
                y: 0,
                w: 3,
                h: 2,
            },
            palette: vec![
                crate::PaletteEntry {
                    kind: "wall".to_string(),
                    chambers: vec![],
                    color: None,
                },
                crate::PaletteEntry {
                    kind: "floor".to_string(),
                    chambers: vec![0],
                    color: None,
                },
                crate::PaletteEntry {
                    kind: "threshold".to_string(),
                    chambers: vec![0, 1],
                    color: None,
                },
            ],
            cells: vec![0, 1, 2, 0, 1, 0],
            you: PlanPoint { x: 1, y: 1 },
            marks: vec![],
        }
    }

    #[test]
    fn glyph_of_matches_the_shipped_vocabulary() {
        assert_eq!(glyph_of("wall"), WALL_GLYPH);
        assert_eq!(glyph_of("floor"), FLOOR_GLYPH);
        assert_eq!(glyph_of("threshold"), THRESHOLD_GLYPH);
    }

    #[test]
    fn an_unrecognised_kind_draws_as_wall() {
        assert_eq!(glyph_of("rubble"), WALL_GLYPH);
    }

    #[test]
    fn cells_land_at_their_row_major_position() {
        let p = small_plan();
        let mut g = crate::Grid::new(5, 5);
        draw(&p, &mut g, (0, 0));
        assert_eq!(g.get(0, 0).unwrap().glyph, Some(WALL_GLYPH));
        assert_eq!(g.get(1, 0).unwrap().glyph, Some(FLOOR_GLYPH));
        assert_eq!(g.get(2, 0).unwrap().glyph, Some(THRESHOLD_GLYPH));
        assert_eq!(g.get(0, 1).unwrap().glyph, Some(WALL_GLYPH));
        // (1, 1) is a floor cell, but `you` stands there, so `@` wins.
        assert_eq!(g.get(1, 1).unwrap().glyph, Some(YOU_GLYPH));
        assert_eq!(g.get(2, 1).unwrap().glyph, Some(WALL_GLYPH));
    }

    #[test]
    fn origin_offsets_every_cell() {
        let p = small_plan();
        let mut g = crate::Grid::new(10, 10);
        draw(&p, &mut g, (4, 3));
        assert_eq!(g.get(4, 3).unwrap().glyph, Some(WALL_GLYPH));
        assert_eq!(g.get(5, 4).unwrap().glyph, Some(YOU_GLYPH));
        assert!(g.get(0, 0).unwrap().is_blank());
    }

    #[test]
    fn a_nonzero_extent_origin_is_honoured() {
        // The same 3x2 plan, but the lattice-local frame starts at (10, 10)
        // rather than (0, 0) — `you` and every cell must offset by that,
        // not assume the extent starts at the origin.
        let mut p = small_plan();
        p.extent.x = 10;
        p.extent.y = 10;
        p.you = PlanPoint { x: 11, y: 11 };
        let mut g = crate::Grid::new(5, 5);
        draw(&p, &mut g, (0, 0));
        assert_eq!(g.get(0, 0).unwrap().glyph, Some(WALL_GLYPH));
        assert_eq!(g.get(1, 1).unwrap().glyph, Some(YOU_GLYPH));
    }

    #[test]
    fn a_mark_redraws_its_own_cells_glyph_and_never_erases_you() {
        let mut p = small_plan();
        p.marks = vec![PlanMark {
            x: 2,
            y: 0,
            noun: "goblin".to_string(),
            kind: "agent".to_string(),
            datum: "A goblin stands here.".to_string(),
            salience: 5,
        }];
        let mut g = crate::Grid::new(5, 5);
        draw(&p, &mut g, (0, 0));
        // The marked cell is a threshold; the mark pass must not change its
        // glyph, only prove it wins should a future geometry collide.
        assert_eq!(g.get(2, 0).unwrap().glyph, Some(THRESHOLD_GLYPH));
        assert_eq!(g.get(1, 1).unwrap().glyph, Some(YOU_GLYPH));
    }

    #[test]
    fn a_mark_outside_the_extent_is_silently_skipped() {
        let mut p = small_plan();
        p.marks = vec![PlanMark {
            x: 99,
            y: 99,
            noun: "ghost".to_string(),
            kind: "agent".to_string(),
            datum: "A ghost, somehow off the map.".to_string(),
            salience: 1,
        }];
        let mut g = crate::Grid::new(5, 5);
        // Must not panic.
        draw(&p, &mut g, (0, 0));
    }
}
