//! The underground band's cave level (`vessel/level/v1`) drawn into the cell
//! grid — The Gallery, Task 9; spec §4.
//!
//! ## `Level::cells` is swept directly, not indexed
//!
//! [`crate::plan::draw`] sweeps `Plan::extent` row-major and looks up each
//! cell's palette index from a dense `Vec<u32>` sized `w * h`. `Level::cells`
//! carries no such dense index — it is a `Vec<`[`crate::LevelCell`]`>`
//! naming only the cells the possession has ever seen (spec §4.1.1: a
//! never-seen cell is omitted, not flagged), so this module iterates that
//! list directly. There is no extent sweep and nothing to skip: every entry
//! in the list is drawn.
//!
//! ## The palette carries a visibility STATE, never a colour
//!
//! [`crate::LevelPaletteEntry`] interns on `(kind, state)`, and `state` is
//! one of `"here"`, `"lit"` or `"remembered"` — there is no `color` field at
//! all (contrast [`crate::PaletteEntry`], which carries one for the chamber
//! band). The producer's own module doc explains why: this crate's three
//! renderers withhold tint from a mark by explicit rule, so a dim
//! "remembered" cell encoded as a darker shade would put this band's field
//! of view *below* the walk band's on exactly the axis the systems audit
//! credits it for (`docs/audits/system-coverage-wolverson-2021.md`,
//! Field of View: the seen/remembered distinction "is carried by a GLYPH
//! twin rather than a dimmed tint … so it survives an uncoloured cell, an
//! uncoloured terminal, and the monochrome client alike").
//!
//! So [`glyph_of`] carries the whole of the distinction: a `"remembered"`
//! cell draws its kind's REMEMBERED twin — a different character, not a
//! dimmer [`Weight`] — while `"here"` and `"lit"` share one glyph per kind
//! (the two are drawn identically; a lit non-`you` cell and the
//! possession's own cell differ only because [`draw`]'s second pass
//! overwrites `you`'s position with [`YOU_GLYPH`] regardless of what its
//! terrain glyph would have been, exactly as [`crate::plan::draw`] does).
//! `level.rs` never reaches for [`Weight::Dim`] the way [`crate::chart`]'s
//! coarse `@`/`+` vocabulary does — that renderer has no per-kind glyphs to
//! carry the distinction and needs [`Weight`] to do it instead; this one
//! does, so it does not need to, and mixing the two signals for one state
//! would be redundant at best and, if a viewer's terminal cannot render
//! `Dim` at all, would leave `Weight` claiming a distinction the glyph
//! already made on its own.
//!
//! ## The glyph twins
//!
//! Eight kinds, each with a "seen" glyph (`here` or `lit`) and a
//! "remembered" twin:
//!
//! | kind | seen | remembered |
//! |---|---|---|
//! | floor | `.` | `,` |
//! | wall | `#` | `:` |
//! | flooded | `~` | `-` |
//! | stairs down | `>` | `)` |
//! | stairs up | `<` | `(` |
//! | threshold | `'` | `` ` `` |
//! | deep | `=` | `_` |
//! | drop | `v` | `u` |
//!
//! The last three arrived with The Brattice (sim spec §3.5). A **threshold**
//! is a squeeze, not a doorway — `'` is a gap in the wall's own stroke, and
//! the `+` of a built doorway belongs to the door, which is a MARK and not a
//! kind (below). **Deep** water is `=`, one stroke more than `flooded`'s
//! rippled `~`: too deep to wade. A **drop** is `v`, the lip of a chute
//! pointing the one way it goes. Their remembered twins follow the same
//! softening rule as the rest — a squeeze's upright tick leans over into a
//! backtick, deep water's two strokes settle to the lower one, and the
//! chute's sharp `v` rounds into a `u`.
//!
//! `.`→`,` matches the walk band's own convention exactly (the systems
//! audit's `faded()` example). The rest follow the same idea — a
//! remembered cell reads as a softer, less committal version of the same
//! shape: a solid wall's two strokes soften to two dots, a rippled water
//! surface flattens to a still line, and a stair's sharp chevron rounds
//! into a parenthesis, the way an edge softens in memory. An unrecognised
//! `kind` string draws as the wall pair, matching [`crate::plan::glyph_of`]'s
//! own fallback and the sim's own `lattice/render.rs::glyph` — "a cell the
//! map does not hold" reads as solid rock, never a hole.
//!
//! ## `you` is drawn a second time, matching `plan.rs`
//!
//! [`YOU_GLYPH`] is `@`, the same mark [`crate::plan::draw`] and
//! [`crate::chart`] use — one verb, three bands now, one "you are here" mark
//! to learn. The possession's own cell is always present in
//! [`crate::Level::cells`] at `"here"` (`level_of`'s own producer-side
//! contract), so the cells pass already draws *something* there; the `you`
//! pass then overwrites it with `@`, Bold, exactly as `plan::draw`'s two
//! passes do.
//!
//! ## Marks draw their own glyph, unlike the chamber band's
//!
//! Spec §3.6: an underground resident is "drawn on the plan as marks and
//! filtered by sight, which the chamber band already does correctly" — the
//! placement and sight-filtering precedent is [`crate::plan::draw`]'s, and
//! this module's marks pass copies its *shape* (a third pass, after cells
//! and `you`, iterating `level.marks`; see [`draw`]). It does **not** copy
//! `plan.rs`'s specific glyph choice, and the reason is that module's own
//! doc: `plan.rs` redraws a mark's own terrain glyph because chamber
//! marks have no glyph to spare — every character in that band's four-glyph
//! vocabulary (`#`/`.`/`+`/`@`) is already claimed by a `CellKind` or by
//! `you`. This band's sixteen-glyph terrain vocabulary (eight kinds, each
//! with a seen/remembered twin — the table above) never claims `&`, so a
//! resident draws as `&`
//! outright — the same glyph `clients/game/bin/src/plate.rs::AGENT_GLYPH`
//! and `windows/scene/src/surrounds_ascii.rs::terrain_glyph` already use for
//! an `"agent"`-kind mark elsewhere in this project, reused rather than
//! invented. Drawing the terrain glyph instead here would leave the
//! headline fact of this campaign — a creature the possession can now see —
//! literally invisible in the one place a player looks: the picture.
//!
//! **A door is a mark with its own glyph.** The sim carries a door as a
//! `PlanMark` whose `kind` is `"door"` (sim spec §3.7 — a door is a Thing, so
//! it is never a palette kind), and this pass draws that one kind as
//! [`DOOR_GLYPH`] `+` rather than [`MARK_GLYPH`] `&`. `+` is the chamber
//! band's own doorway glyph (`crate::plan`), reused rather than invented: a
//! built door reads the same in both bands, while the `'` of a bare squeeze
//! stays visibly a different thing. Every other mark kind still draws `&`.
//!
//! A mark is drawn only when its own `(x, y)` lands inside `into`'s bounds
//! ([`grid_pos`] returns `None` and the draw is skipped, matching the cells
//! and `you` passes' own discipline), and it draws unconditionally once it
//! does — the producer (`windows/vessel/src/session.rs::underground_level`,
//! `purview::AGENT_MARK_KIND`) already filters a mark to a genuinely `lit`
//! cell before it ever reaches the wire, so this pass does not re-check
//! visibility.

use crate::{Cell, Level, PlanMark, Source, Weight};

/// The glyph every mark on this band draws — see the module doc for why a
/// dedicated glyph is correct here where it would not be for
/// [`crate::plan::draw`]. Reuses `clients/game/bin/src/plate.rs::AGENT_GLYPH`
/// and `windows/scene/src/surrounds_ascii.rs`'s own `"agent"`-kind glyph
/// rather than inventing a fourth character for the same fact.
const MARK_GLYPH: char = '&';

/// The "seen" (here or lit) glyph for a floor cell.
const FLOOR_GLYPH: char = '.';
/// The remembered twin of [`FLOOR_GLYPH`] — spec §4.1's glyph twin, matching
/// the walk band's own `.`→`,` convention exactly.
const FLOOR_REMEMBERED_GLYPH: char = ',';

/// The "seen" glyph for a wall cell, and the fallback for any `kind` string
/// this client does not recognise (see the module doc).
const WALL_GLYPH: char = '#';
/// The remembered twin of [`WALL_GLYPH`] — a solid double-stroke thins to
/// two dots.
const WALL_REMEMBERED_GLYPH: char = ':';

/// The "seen" glyph for a flooded cell.
const FLOODED_GLYPH: char = '~';
/// The remembered twin of [`FLOODED_GLYPH`] — a rippled surface flattens to
/// a still line once it is memory rather than sight.
const FLOODED_REMEMBERED_GLYPH: char = '-';

/// The "seen" glyph for a connection down toward the next rung.
const STAIRS_DOWN_GLYPH: char = '>';
/// The remembered twin of [`STAIRS_DOWN_GLYPH`] — the sharp chevron rounds
/// into a parenthesis.
const STAIRS_DOWN_REMEMBERED_GLYPH: char = ')';

/// The glyph a `"door"`-kind mark draws, in place of [`MARK_GLYPH`] — the
/// chamber band's own doorway glyph (`crate::plan`), reused so a built door
/// reads the same in both bands. A door is a Thing on the wire (sim spec
/// §3.7), never a palette kind, so it is the marks pass that carries it.
const DOOR_GLYPH: char = '+';

/// The mark `kind` string [`DOOR_GLYPH`] answers to.
const DOOR_MARK_KIND: &str = "door";

/// The "seen" glyph for the one cell where a passage breaches the wall
/// between two regions — a SQUEEZE, not a doorway (sim spec §3.5).
const THRESHOLD_GLYPH: char = '\'';
/// The remembered twin of [`THRESHOLD_GLYPH`] — the upright tick leans over.
const THRESHOLD_REMEMBERED_GLYPH: char = '`';

/// The "seen" glyph for water too deep to wade — one stroke more than
/// [`FLOODED_GLYPH`]'s rippled surface.
const DEEP_GLYPH: char = '=';
/// The remembered twin of [`DEEP_GLYPH`] — the two strokes settle to the
/// lower one.
const DEEP_REMEMBERED_GLYPH: char = '_';

/// The "seen" glyph for a chute's lip, pointing the one way it goes.
const DROP_GLYPH: char = 'v';
/// The remembered twin of [`DROP_GLYPH`] — the sharp `v` rounds, the same
/// softening [`STAIRS_DOWN_REMEMBERED_GLYPH`] uses.
const DROP_REMEMBERED_GLYPH: char = 'u';

/// The "seen" glyph for a connection up toward the rung above.
const STAIRS_UP_GLYPH: char = '<';
/// The remembered twin of [`STAIRS_UP_GLYPH`], the same rounding as
/// [`STAIRS_DOWN_REMEMBERED_GLYPH`].
const STAIRS_UP_REMEMBERED_GLYPH: char = '(';

/// The glyph for the cell the possession stands in. The same mark
/// [`crate::plan::draw`] and [`crate::chart`] use for `here` — one verb,
/// three bands, one "you are here" mark to learn.
const YOU_GLYPH: char = '@';

/// Glyph for a palette entry's `(kind, state)` pair. See the module doc for
/// the full twin table and why an unrecognised `kind` draws as a wall.
fn glyph_of(kind: &str, state: &str) -> char {
    let remembered = state == "remembered";
    match kind {
        "floor" => {
            if remembered {
                FLOOR_REMEMBERED_GLYPH
            } else {
                FLOOR_GLYPH
            }
        }
        "flooded" => {
            if remembered {
                FLOODED_REMEMBERED_GLYPH
            } else {
                FLOODED_GLYPH
            }
        }
        "stairs_down" => {
            if remembered {
                STAIRS_DOWN_REMEMBERED_GLYPH
            } else {
                STAIRS_DOWN_GLYPH
            }
        }
        "stairs_up" => {
            if remembered {
                STAIRS_UP_REMEMBERED_GLYPH
            } else {
                STAIRS_UP_GLYPH
            }
        }
        "threshold" => {
            if remembered {
                THRESHOLD_REMEMBERED_GLYPH
            } else {
                THRESHOLD_GLYPH
            }
        }
        "deep" => {
            if remembered {
                DEEP_REMEMBERED_GLYPH
            } else {
                DEEP_GLYPH
            }
        }
        "drop" => {
            if remembered {
                DROP_REMEMBERED_GLYPH
            } else {
                DROP_GLYPH
            }
        }
        // "wall", and anything this client does not recognise: the sim's
        // own `lattice/render.rs::glyph` treats an unmapped cell the same
        // way — solid rock, never a hole.
        _ => {
            if remembered {
                WALL_REMEMBERED_GLYPH
            } else {
                WALL_GLYPH
            }
        }
    }
}

/// The grid position for a level-local point `(x, y)`, offset by the
/// level's own extent and anchored at `origin` — the same "relative to
/// origin" contract [`crate::plan::draw`]'s own `grid_pos` follows. Returns
/// `None` for a point outside `into`'s bounds.
fn grid_pos(
    level: &Level,
    x: i32,
    y: i32,
    origin: (u16, u16),
    into: &crate::Grid,
) -> Option<(u16, u16)> {
    let gx = origin.0 as i64 + (x - level.extent.x) as i64;
    let gy = origin.1 as i64 + (y - level.extent.y) as i64;
    if gx < 0 || gy < 0 || gx >= into.width() as i64 || gy >= into.height() as i64 {
        None
    } else {
        Some((gx as u16, gy as u16))
    }
}

/// Draw `level` into `into`, anchored so the level's own `(extent.x,
/// extent.y)` lands at `origin`. Three passes, in order: every seen cell by
/// its palette glyph, then `you` as `@`, then marks as `&` (a `"door"` mark
/// as `+`) — see the module doc's "Marks draw their own glyph" section for
/// why this band's marks pass draws a dedicated glyph where
/// [`crate::plan::draw`]'s cannot.
pub fn draw(level: &Level, into: &mut crate::Grid, origin: (u16, u16)) {
    for cell in &level.cells {
        let Some(entry) = level.palette.get(cell.ix as usize) else {
            continue;
        };
        if let Some((gx, gy)) = grid_pos(level, cell.x, cell.y, origin, into) {
            into.set(
                gx,
                gy,
                Cell::glyph(
                    glyph_of(&entry.kind, &entry.state),
                    Weight::Normal,
                    Source::Level,
                ),
            );
        }
    }

    // Spec §2.2's rule, restated for this band: `you` is identity, and
    // identity belongs to glyph — it is drawn Bold regardless of the
    // terrain beneath it, exactly as `plan::draw` draws its own `@`.
    if let Some((gx, gy)) = grid_pos(level, level.you.x, level.you.y, origin, into) {
        into.set(gx, gy, Cell::glyph(YOU_GLYPH, Weight::Bold, Source::Level));
    }

    for m in &level.marks {
        draw_mark(level, m, origin, into);
    }
}

/// One mark's contribution to the marks pass: draw [`MARK_GLYPH`] at the
/// mark's own cell — or [`DOOR_GLYPH`] for a `"door"`-kind mark, which is
/// how a door reaches this band at all (sim spec §3.7: a door is a Thing,
/// never a palette kind). See the module doc for why this band draws a
/// dedicated glyph rather than redrawing the terrain beneath, as
/// [`crate::plan::draw_mark`] does for the chamber band.
fn draw_mark(level: &Level, m: &PlanMark, origin: (u16, u16), into: &mut crate::Grid) {
    let glyph = if m.kind == DOOR_MARK_KIND {
        DOOR_GLYPH
    } else {
        MARK_GLYPH
    };
    if let Some((gx, gy)) = grid_pos(level, m.x, m.y, origin, into) {
        into.set(gx, gy, Cell::glyph(glyph, Weight::Normal, Source::Level));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LevelCell, LevelExtent, LevelPaletteEntry, LevelPoint};

    /// A 3x3 level: one floor cell lit, one wall cell remembered, `you`
    /// standing on the floor cell.
    fn small_level() -> Level {
        Level {
            rung: "undercroft".to_string(),
            depth_m: 12.5,
            extent: LevelExtent {
                x: 0,
                y: 0,
                w: 3,
                h: 3,
            },
            palette: vec![
                LevelPaletteEntry {
                    kind: "floor".to_string(),
                    state: "here".to_string(),
                },
                LevelPaletteEntry {
                    kind: "wall".to_string(),
                    state: "remembered".to_string(),
                },
            ],
            cells: vec![
                LevelCell { x: 1, y: 1, ix: 0 },
                LevelCell { x: 0, y: 0, ix: 1 },
            ],
            you: LevelPoint { x: 1, y: 1 },
            marks: vec![],
        }
    }

    /// The acceptance test named by Task 9's own brief: the monochrome
    /// property (spec §4.1), tested at the renderer. A lit cell and a
    /// remembered cell of DIFFERENT kinds already draw different glyphs
    /// trivially, so this builds one of EACH state and checks the drawn
    /// characters differ — the property that would fail if `state` were
    /// ever dropped from [`glyph_of`]'s inputs.
    #[test]
    fn a_remembered_cell_and_a_lit_cell_draw_different_glyphs() {
        let level = Level {
            rung: "undercroft".to_string(),
            depth_m: 12.5,
            extent: LevelExtent {
                x: 0,
                y: 0,
                w: 2,
                h: 1,
            },
            palette: vec![
                LevelPaletteEntry {
                    kind: "floor".to_string(),
                    state: "lit".to_string(),
                },
                LevelPaletteEntry {
                    kind: "floor".to_string(),
                    state: "remembered".to_string(),
                },
            ],
            cells: vec![
                LevelCell { x: 0, y: 0, ix: 0 },
                LevelCell { x: 1, y: 0, ix: 1 },
            ],
            you: LevelPoint { x: 5, y: 5 }, // off the extent: never drawn here
            marks: vec![],
        };
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        let lit = g.get(0, 0).unwrap().glyph.unwrap();
        let remembered = g.get(1, 0).unwrap().glyph.unwrap();
        assert_ne!(
            lit, remembered,
            "a lit floor cell and a remembered floor cell must draw \
             different glyphs — the epistemic state is carried by the \
             character, never a shade"
        );
        assert_eq!(lit, FLOOR_GLYPH);
        assert_eq!(remembered, FLOOR_REMEMBERED_GLYPH);
    }

    #[test]
    fn glyph_of_matches_the_shipped_vocabulary() {
        assert_eq!(glyph_of("floor", "here"), FLOOR_GLYPH);
        assert_eq!(glyph_of("floor", "lit"), FLOOR_GLYPH);
        assert_eq!(glyph_of("floor", "remembered"), FLOOR_REMEMBERED_GLYPH);
        assert_eq!(glyph_of("wall", "lit"), WALL_GLYPH);
        assert_eq!(glyph_of("wall", "remembered"), WALL_REMEMBERED_GLYPH);
        assert_eq!(glyph_of("flooded", "lit"), FLOODED_GLYPH);
        assert_eq!(glyph_of("flooded", "remembered"), FLOODED_REMEMBERED_GLYPH);
        assert_eq!(glyph_of("stairs_down", "lit"), STAIRS_DOWN_GLYPH);
        assert_eq!(
            glyph_of("stairs_down", "remembered"),
            STAIRS_DOWN_REMEMBERED_GLYPH
        );
        assert_eq!(glyph_of("stairs_up", "lit"), STAIRS_UP_GLYPH);
        assert_eq!(
            glyph_of("stairs_up", "remembered"),
            STAIRS_UP_REMEMBERED_GLYPH
        );
        assert_eq!(glyph_of("threshold", "lit"), THRESHOLD_GLYPH);
        assert_eq!(
            glyph_of("threshold", "remembered"),
            THRESHOLD_REMEMBERED_GLYPH
        );
        assert_eq!(glyph_of("deep", "lit"), DEEP_GLYPH);
        assert_eq!(glyph_of("deep", "remembered"), DEEP_REMEMBERED_GLYPH);
        assert_eq!(glyph_of("drop", "lit"), DROP_GLYPH);
        assert_eq!(glyph_of("drop", "remembered"), DROP_REMEMBERED_GLYPH);
        // Every glyph in the vocabulary is distinct: a twin that collided
        // with another kind's would carry the epistemic state and lose the
        // kind, which is the one thing this table exists to keep apart.
        let vocabulary = [
            FLOOR_GLYPH,
            FLOOR_REMEMBERED_GLYPH,
            WALL_GLYPH,
            WALL_REMEMBERED_GLYPH,
            FLOODED_GLYPH,
            FLOODED_REMEMBERED_GLYPH,
            STAIRS_DOWN_GLYPH,
            STAIRS_DOWN_REMEMBERED_GLYPH,
            STAIRS_UP_GLYPH,
            STAIRS_UP_REMEMBERED_GLYPH,
            THRESHOLD_GLYPH,
            THRESHOLD_REMEMBERED_GLYPH,
            DEEP_GLYPH,
            DEEP_REMEMBERED_GLYPH,
            DROP_GLYPH,
            DROP_REMEMBERED_GLYPH,
            YOU_GLYPH,
            MARK_GLYPH,
            DOOR_GLYPH,
        ];
        let mut sorted = vocabulary;
        sorted.sort_unstable();
        let mut deduped = sorted.to_vec();
        deduped.dedup();
        assert_eq!(
            deduped.len(),
            vocabulary.len(),
            "two glyphs in this band's vocabulary collide: {sorted:?}"
        );
    }

    /// A door reaches this band as a MARK, never a palette kind (sim spec
    /// §3.7), and it draws `+` — the chamber band's own doorway glyph —
    /// while every other mark kind keeps `&`. The producer exists now
    /// (`Session::underground_level` in `windows/vessel/src/session.rs`,
    /// pinned there by
    /// `a_lit_door_reaches_the_level_document_as_a_door_mark`), but this
    /// unit test still constructs a `"door"` mark directly rather than
    /// driving a session: the client crate cannot build one, only decode
    /// the document a session emits.
    #[test]
    fn a_door_mark_draws_a_doorway_and_every_other_mark_draws_the_mark_glyph() {
        let mut level = small_level();
        level.marks = vec![
            PlanMark {
                x: 0,
                y: 1,
                noun: "door".to_string(),
                kind: "door".to_string(),
                datum: "A heavy door.".to_string(),
                salience: 0,
            },
            PlanMark {
                x: 1,
                y: 0,
                noun: "rust monster".to_string(),
                kind: "agent".to_string(),
                datum: "It clicks.".to_string(),
                salience: 1,
            },
        ];
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        assert_eq!(g.get(0, 1).unwrap().glyph, Some(DOOR_GLYPH));
        assert_eq!(g.get(1, 0).unwrap().glyph, Some(MARK_GLYPH));
    }

    #[test]
    fn an_unrecognised_kind_draws_as_wall() {
        assert_eq!(glyph_of("rubble", "lit"), WALL_GLYPH);
        assert_eq!(glyph_of("rubble", "remembered"), WALL_REMEMBERED_GLYPH);
    }

    #[test]
    fn sparse_cells_land_at_their_named_position_and_you_overwrites_here() {
        let level = small_level();
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        // (1, 1) is the floor cell AND `you`'s own cell: `@` must win.
        assert_eq!(g.get(1, 1).unwrap().glyph, Some(YOU_GLYPH));
        assert_eq!(g.get(1, 1).unwrap().weight, Weight::Bold);
        // (0, 0) is the remembered wall.
        assert_eq!(g.get(0, 0).unwrap().glyph, Some(WALL_REMEMBERED_GLYPH));
        // A never-seen cell (e.g. (2, 2)) is simply never drawn.
        assert!(g.get(2, 2).unwrap().is_blank());
    }

    #[test]
    fn origin_offsets_every_cell() {
        let level = small_level();
        let mut g = crate::Grid::new(10, 10);
        draw(&level, &mut g, (4, 3));
        assert_eq!(g.get(4, 3).unwrap().glyph, Some(WALL_REMEMBERED_GLYPH));
        assert_eq!(g.get(5, 4).unwrap().glyph, Some(YOU_GLYPH));
        assert!(g.get(0, 0).unwrap().is_blank());
    }

    #[test]
    fn a_nonzero_extent_origin_is_honoured() {
        let mut level = small_level();
        level.extent.x = 10;
        level.extent.y = 10;
        level.cells = vec![
            LevelCell {
                x: 11,
                y: 11,
                ix: 0,
            },
            LevelCell {
                x: 10,
                y: 10,
                ix: 1,
            },
        ];
        level.you = LevelPoint { x: 11, y: 11 };
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        assert_eq!(g.get(0, 0).unwrap().glyph, Some(WALL_REMEMBERED_GLYPH));
        assert_eq!(g.get(1, 1).unwrap().glyph, Some(YOU_GLYPH));
    }

    #[test]
    fn no_cell_carries_a_colour() {
        // SAFETY: ink resolution reads process-global NO_COLOR; ENV_LOCK
        // serialises this read against every mutating sibling thread.
        let _env = crate::cell::test_env::ENV_LOCK.lock().unwrap();
        let level = small_level();
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        assert_eq!(
            g.get(0, 0).unwrap().ink,
            crate::Ink::Plain,
            "the underground band's palette carries no colour at all — the \
             epistemic state is glyph-only (spec §4.1)"
        );
    }

    #[test]
    fn a_point_outside_the_grid_is_silently_skipped() {
        let mut level = small_level();
        level.cells.push(LevelCell {
            x: 99,
            y: 99,
            ix: 0,
        });
        let mut g = crate::Grid::new(5, 5);
        // Must not panic.
        draw(&level, &mut g, (0, 0));
    }

    /// The regression this fix exists for: `Level::marks` used to reach this
    /// module and draw nothing at all (no marks-drawing pass existed).
    /// (0, 0) is `small_level`'s remembered wall — the mark drawn there must
    /// win over that terrain glyph, and the two must actually differ, or a
    /// deleted marks pass would leave this test green for the wrong reason.
    #[test]
    fn a_mark_draws_a_glyph_distinguishable_from_the_terrain_beneath() {
        let mut level = small_level();
        level.marks = vec![PlanMark {
            x: 0,
            y: 0,
            noun: "xorn".to_string(),
            kind: "agent".to_string(),
            datum: "A xorn chews through the rock nearby.".to_string(),
            salience: 5,
        }];
        let mut g = crate::Grid::new(5, 5);
        draw(&level, &mut g, (0, 0));
        let cell = g.get(0, 0).unwrap();
        assert_eq!(
            cell.glyph,
            Some(MARK_GLYPH),
            "a marked cell must draw the mark's own glyph"
        );
        assert_ne!(
            cell.glyph,
            Some(WALL_REMEMBERED_GLYPH),
            "the mark's glyph must be distinguishable from the terrain it stands on"
        );
    }

    #[test]
    fn a_mark_outside_the_grid_is_silently_skipped() {
        let mut level = small_level();
        level.marks = vec![PlanMark {
            x: 99,
            y: 99,
            noun: "ghost".to_string(),
            kind: "agent".to_string(),
            datum: "A ghost, somehow off the map.".to_string(),
            salience: 1,
        }];
        let mut g = crate::Grid::new(5, 5);
        // Must not panic, and must not draw anything at (99, 99).
        draw(&level, &mut g, (0, 0));
    }
}
