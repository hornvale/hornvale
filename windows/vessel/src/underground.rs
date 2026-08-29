//! The session's position within a generated underworld descent (The
//! Gallery, Task 3).
//!
//! Before this task, `delve` set a bucket: `Session.underground` held a
//! single [`hornvale_worldgen::chamber::Chamber`] — an address and a rock
//! type, nothing a possession could stand *in*. This module makes it a real
//! place: [`Underground::enter`] builds a whole descent through
//! [`generate_descent_for_character`] and stands the possession on a real
//! cell of a real generated level.
//!
//! `FRAME`-tier in its entirety (decision 0069): built fresh on every
//! `delve`, never serialized. A world is a seed plus a ledger; nothing here
//! is either.

use hornvale_kernel::{Band, Seed};
use hornvale_locale::Compass;

use crate::lattice::Cell;
use crate::underworld_level::{Level, LevelCellKind, generate_descent_for_character};

/// Underground's own diagonal refusal (The Gallery, Task 4) — the same
/// geometry `INDOOR_DIAGONAL_REFUSAL` states one band over
/// (`session.rs`): [`crate::session::cell_delta`] is orthogonal only, so a
/// diagonal step through the corner where two walls meet is not a way
/// through rock either.
const UNDERGROUND_DIAGONAL_REFUSAL: &str =
    "There is no slipping through a corner down here either; try north, south, east or west.";

/// The physical reason a rock cell refuses a lateral step underground.
/// Names no verb and no movement mode — a parse complaint this is not.
const UNDERGROUND_ROCK_REFUSAL: &str =
    "Solid rock closes off the way; there is no path through it.";

/// The physical reason [`Underground::peek_stairs`] refuses when the
/// current cell is not a stairs cell at all (The Gallery, Task 5). Never
/// surfaced through `Session::take_stairs` in practice — that caller checks
/// the current cell's kind against the direction it wants before ever
/// asking `peek_stairs`, and refuses with a direction-specific sentence of
/// its own instead — but `peek_stairs`/`take_stairs` are real seams a test,
/// or a future caller with no direction preference, can reach directly.
const NOT_ON_STAIRS_REFUSAL: &str = "There is no stairway underfoot to take.";

/// The physical reason the descent's own deepest rung refuses a `StairsDown`
/// (The Gallery, Task 5). `place_connections`
/// (`windows/vessel/src/underworld_level/mod.rs`) cuts a `StairsDown` cell
/// into every rung unconditionally — the bottom rung included — but the
/// generated descent itself stops at the last rung [`Underground::enter`]
/// built, so that rung's own down-stairs lead to a rung nothing has
/// generated yet.
const STAIRS_LEAD_NOWHERE_REFUSAL: &str =
    "The stairs continue down into unbroken dark, but nothing has delved that far yet.";

/// Every habitation rung the delve ladder names, [`Band::Surface`]
/// excluded — the same filter
/// `windows/vessel/tests/suite/underworld_level_generation.rs`'s
/// `measure_flooded_cell_reachability_across_the_descent` applies. The
/// index into this list *is* [`Underground::rung`]: `descent[i]` is always
/// the level [`Underground::enter`] generated for `habitation_rungs()[i]`.
fn habitation_rungs() -> Vec<Band> {
    hornvale_terrain::rungs()
        .iter()
        .copied()
        .filter(|&rung| rung != Band::Surface)
        .collect()
}

/// The session's position within one cave system's generated descent (The
/// Gallery, Task 3).
///
/// **The struct's fields are `pub(crate)`, not private.** It lives in its
/// own module while `session.rs`'s test module reaches into it directly —
/// the same reason `Inside` (`session.rs`) gets away with bare private
/// fields is exactly the reason this one cannot: `Inside` is declared
/// inside the module that reads it, this is not.
///
/// **No `seen` field yet.** Task 6 adds the fog-of-war bitset; a field with
/// no reader is a field a reviewer cannot judge, and `SeenBits` does not
/// exist until then.
pub(crate) struct Underground {
    /// One generated level per habitation rung, in [`habitation_rungs`]
    /// order: `descent[i]` is the level for `habitation_rungs()[i]`.
    pub(crate) descent: Vec<Level>,
    /// Which element of `descent` the possession currently occupies —
    /// always `0` as of this task ([`Underground::enter`] is the only
    /// constructor, and it always enters at the top). An index into
    /// `habitation_rungs()` as well as into `descent`; the two can never
    /// disagree because both are sized and ordered from the same
    /// `habitation_rungs()` call.
    pub(crate) rung: usize,
    /// Which cell of `descent[rung]` the possession stands on. Always a
    /// `Floor` or `Flooded` cell at the moment [`Underground::enter`]
    /// places it — never `Wall`, `StairsDown` or `StairsUp`.
    pub(crate) cell: Cell,
    /// The seed the descent was generated from (the world's own seed —
    /// [`Underground::enter`] derives nothing else). Carried alongside the
    /// generated content the same way [`crate::lattice::Lattice`]'s
    /// embedding seed rides beside `Inside`'s own `lattice`, so a later
    /// verb that needs to reproduce or extend this descent has it in hand
    /// rather than threading it through a second parameter.
    ///
    /// **Unused within this task** — this task's own tests read
    /// `self.underground.as_ref()` and the generated `descent`/`rung`/
    /// `cell` directly, never this field. The brief's own interface list
    /// names it as part of this task's produced struct regardless, and
    /// Task 6's fog-of-war bitset is the first thing that needs a seed to
    /// rebuild from.
    #[allow(dead_code)]
    pub(crate) seed: Seed,
}

impl Underground {
    /// Enter a cave system at its topmost rung: build the whole descent
    /// through [`generate_descent_for_character`] and stand the possession
    /// on the entrance rung's first standable cell — the first `Floor` or
    /// `Flooded` cell in the level's own ascending-`(x, y)` order
    /// ([`crate::underworld_level::CellGrid::iter`]).
    ///
    /// **The descent's inputs are the production recipe**, copied the way
    /// `windows/vessel/tests/suite/underworld_level_generation.rs`'s
    /// `measure_flooded_cell_reachability_across_the_descent` already
    /// copies `windows/worldgen/src/lib.rs`'s
    /// `subterranean_substrate_at_rung_with_cave`: gradient and porosity
    /// read off `terrain` at `vertex`, the water table from
    /// `hornvale_terrain::water_table_depth_m`, and a per-rung evaluation
    /// depth from `hornvale_terrain::rung_evaluation_depth_m`. `cave.kind`
    /// names the natural-leaf algorithm (never a hardcoded `CaveKind`).
    /// Every rung's origin is
    /// [`hornvale_worldgen::chamber::ChamberOrigin::Found`] — the shipped
    /// path constructs no `ChamberOverrides`, so `Made` is unreachable
    /// here — and the character is
    /// [`hornvale_worldgen::character::Character::WildCave`], matching
    /// [`crate::underworld_level::generate_descent`]'s own hardcoded
    /// value.
    ///
    /// Both `terrain` and `cave` come from the same terrain handle the
    /// caller (`Session::delve_at`) already resolved: no second,
    /// independently-chosen lookup is introduced here.
    ///
    /// Panics if the entrance rung's generated level has no `Floor` or
    /// `Flooded` cell at all. That is an invariant of the generator, not a
    /// case this task designs a refusal for — Task 9's own connectivity
    /// sweep (`every_walkable_cell_is_reachable_from_every_other`,
    /// `windows/vessel/src/underworld_level/mod.rs`) holds it for every
    /// level this generator produces.
    pub(crate) fn enter(
        terrain: &hornvale_terrain::GeneratedTerrain,
        vertex: hornvale_kernel::Vertex,
        cave: hornvale_terrain::Cave,
        seed: Seed,
    ) -> Underground {
        let rungs = habitation_rungs();
        let gradient = terrain.geothermal_gradient_at(vertex);
        let porosity = terrain.material_at(vertex).porosity;
        let height_asl_m = terrain
            .elevation_at(vertex)
            .above(terrain.sea_level())
            .get();
        let water_table_m = hornvale_terrain::water_table_depth_m(
            terrain.drainage_at(vertex),
            porosity,
            height_asl_m,
        );
        let depths_m: Vec<f64> = rungs
            .iter()
            .map(|&rung| {
                hornvale_terrain::rung_evaluation_depth_m(rung, gradient, cave.depth_reach_m)
                    .expect("every non-Surface rung has an evaluation depth")
            })
            .collect();
        let origins = vec![hornvale_worldgen::chamber::ChamberOrigin::Found; rungs.len()];
        let descent = generate_descent_for_character(
            &rungs,
            cave.kind,
            &origins,
            &depths_m,
            water_table_m,
            hornvale_worldgen::character::Character::WildCave,
            seed,
        );
        let cell = descent[0]
            .cells
            .iter()
            .find(|(_, k)| matches!(k, LevelCellKind::Floor | LevelCellKind::Flooded))
            .map(|(c, _)| c)
            .expect(
                "a generated level has at least one standable cell \
                 (Task 9's connectivity invariant)",
            );
        Underground {
            descent,
            rung: 0,
            cell,
            seed,
        }
    }

    /// The level the possession currently occupies — `descent[rung]`.
    pub(crate) fn level(&self) -> &Level {
        &self.descent[self.rung]
    }

    /// Which band `self.rung` names — `habitation_rungs()[rung]`, the same
    /// index [`Underground::enter`] built `descent` against.
    ///
    /// **Unused within this task.** Naming the current rung is Task 4's
    /// job (narrating movement between rungs) and Task 7's (the pane); this
    /// task ships no verb that reads it. Built now because the brief's own
    /// interface list names it as this task's produced surface.
    #[allow(dead_code)]
    pub(crate) fn rung_band(&self) -> Band {
        habitation_rungs()[self.rung]
    }

    /// A compass step underground: one cell, in the bearing named — the
    /// interface [`Underground::peek`] + [`Underground::commit_step`] split
    /// apart in Fix round 1 so a charging caller could run a cost check
    /// between them, recombined here as the uncharged convenience form.
    ///
    /// **Unused in production as of Fix round 1** —
    /// `Session::step_underground` calls `peek`/`commit_step` directly so it
    /// can charge in between, so nothing non-test calls this all-in-one
    /// form. Kept because it is this task's own documented produced
    /// interface (`Underground::step(&mut self, dir: Compass) ->
    /// StepOutcome`) and because it is the natural shape for a caller that
    /// does not need to charge anything — this crate's own low-level rock-
    /// refusal test (`session.rs`) is exactly that caller. The same
    /// "documented interface, no live production caller yet" shape
    /// `Underground::seed` and `Underground::rung_band` already carry.
    #[allow(dead_code)]
    pub(crate) fn step(&mut self, dir: Compass) -> StepOutcome {
        match self.peek(dir) {
            Err(reason) => StepOutcome::Blocked(reason),
            Ok(target) => self.commit_step(target),
        }
    }

    /// The would-be destination of a compass step underground, WITHOUT
    /// moving there (The Gallery, Task 4; reshaped in Fix round 1).
    ///
    /// **The reversal `UNDERGROUND_LATERAL_REFUSAL` (`session.rs`) owed the
    /// moment this task landed.** That constant's own doc said "there is
    /// nowhere down here for a bearing to mean" — true only while the cave
    /// lattice reached no further than its entrance chamber; Task 3 gave it
    /// a whole generated level, which is what this method (with
    /// [`Underground::commit_step`]) walks.
    ///
    /// Follows the indoor compass step's own precedent
    /// (`Session::step`, `session.rs:3413`) rule for rule:
    ///
    /// 1. **A diagonal is refused** ([`UNDERGROUND_DIAGONAL_REFUSAL`])
    ///    before anything is looked up — [`crate::session::cell_delta`] is
    ///    orthogonal only.
    /// 2. **An impassable target is refused with a physical reason**
    ///    ([`UNDERGROUND_ROCK_REFUSAL`]), asked through
    ///    [`crate::underworld_level::movement_mode`] rather than compared
    ///    against `LevelCellKind::Wall` directly — the same rule
    ///    `CellKind::passable`'s own doc gives, so the refusal survives the
    ///    day a new impassable kind arrives.
    ///
    /// **Split from a single all-in-one `step` (Fix round 1, review finding
    /// 2) so a caller can charge a cost BEFORE the move lands**, the same
    /// order the indoor compass step's own precedent uses (diagonal
    /// refused, then impassable refused, then `charge_within_room`, then —
    /// only once that succeeds — the cell mutates). The original
    /// all-in-one `step` moved the possession as part of computing its
    /// outcome, so a caller could only charge AFTER the move had already
    /// landed — silently making an underground step free and letting the
    /// possession move on a refused clock besides. `Session::
    /// step_underground` is the one caller: it calls this, charges, and
    /// only then calls [`Underground::commit_step`].
    pub(crate) fn peek(&self, dir: Compass) -> Result<Cell, &'static str> {
        let Some(delta) = crate::session::cell_delta(dir) else {
            return Err(UNDERGROUND_DIAGONAL_REFUSAL);
        };
        let target = Cell(self.cell.0 + delta.0, self.cell.1 + delta.1);
        let kind = self.descent[self.rung].cells.get(target);
        if kind
            .and_then(crate::underworld_level::movement_mode)
            .is_none()
        {
            return Err(UNDERGROUND_ROCK_REFUSAL);
        }
        Ok(target)
    }

    /// Commit a step already validated by [`Underground::peek`]: moves the
    /// possession to `target` (already known passable) and reports whether
    /// it landed on a stairs cell. Never itself refuses — `peek` is the only
    /// gate — which is what makes it safe to call only after a charge has
    /// already succeeded.
    pub(crate) fn commit_step(&mut self, target: Cell) -> StepOutcome {
        let kind = self.descent[self.rung].cells.get(target);
        self.cell = target;
        if matches!(
            kind,
            Some(LevelCellKind::StairsDown) | Some(LevelCellKind::StairsUp)
        ) {
            StepOutcome::NeedsStairs
        } else {
            StepOutcome::Moved
        }
    }

    /// Where taking the stairs at the possession's CURRENT cell would lead,
    /// without moving there (The Gallery, Task 5) — the same peek/commit
    /// split [`Underground::peek`]/[`Underground::commit_step`] already draw
    /// one rung over, so a caller can charge a cost before the move lands and
    /// never move the possession on a refused clock.
    ///
    /// **The direction is read off the CURRENT cell's own kind, never a
    /// parameter**: a `StairsDown` cell means descend, a `StairsUp` cell
    /// means ascend, anything else refuses. A caller that wants a SPECIFIC
    /// direction (the `down`/`up` verbs, `session.rs`) checks the current
    /// cell's kind against the one it wants BEFORE ever calling this — this
    /// method alone cannot refuse "wrong direction", only "no direction at
    /// all" or "no destination for the direction there is".
    ///
    /// **The landing cell is the connecting stairway's own cell on the far
    /// side, never a fresh scan for "somewhere standable"**: descending from
    /// rung `n` lands on rung `n + 1`'s own `StairsUp` cell, and ascending
    /// from rung `n` lands on rung `n - 1`'s own `StairsDown` cell — the same
    /// physical stairway, named from its other end. Both are guaranteed to
    /// exist by the generator's own invariant — every rung has exactly one
    /// `StairsDown` cell, and every rung but the first has exactly one
    /// `StairsUp` cell (`place_connections`' `has_up = i > 0`;
    /// `windows/vessel/src/underworld_level/mod.rs`), asserted across 200
    /// seeds by that module's own `stairs_down_and_stairs_up_never_share_a_
    /// cell` — so descending always has somewhere to land on any rung but
    /// the last, and ascending always has somewhere to land on any rung but
    /// the first (which has no `StairsUp` cell to be standing on in the
    /// first place, so that arm is never reached from rung `0`).
    ///
    /// Refuses when the current cell is not a stairs cell at all
    /// ([`NOT_ON_STAIRS_REFUSAL`]), or when the current cell is a
    /// `StairsDown` on the descent's own deepest rung — the one rung
    /// `place_connections` still cuts a down-stairs into (it never
    /// special-cases the last rung) even though [`Underground::enter`]
    /// generated nothing beneath it ([`STAIRS_LEAD_NOWHERE_REFUSAL`]).
    pub(crate) fn peek_stairs(&self) -> Result<(usize, Cell), &'static str> {
        match self.descent[self.rung].cells.get(self.cell) {
            Some(LevelCellKind::StairsDown) => {
                let next = self.rung + 1;
                if next >= self.descent.len() {
                    return Err(STAIRS_LEAD_NOWHERE_REFUSAL);
                }
                let landing = self.descent[next]
                    .cells
                    .iter()
                    .find(|(_, k)| matches!(k, LevelCellKind::StairsUp))
                    .map(|(c, _)| c)
                    .expect(
                        "every rung but the first has a StairsUp cell \
                         (place_connections' has_up = i > 0), and `next` \
                         is never 0",
                    );
                Ok((next, landing))
            }
            Some(LevelCellKind::StairsUp) => {
                let next = self.rung.checked_sub(1).expect(
                    "a StairsUp cell only exists at rung > 0 \
                     (place_connections' has_up = i > 0), so its own rung \
                     always has a predecessor",
                );
                let landing = self.descent[next]
                    .cells
                    .iter()
                    .find(|(_, k)| matches!(k, LevelCellKind::StairsDown))
                    .map(|(c, _)| c)
                    .expect(
                        "every rung has a StairsDown cell — place_connections \
                         cuts one unconditionally, the last rung included",
                    );
                Ok((next, landing))
            }
            _ => Err(NOT_ON_STAIRS_REFUSAL),
        }
    }

    /// Take the stairs at the possession's current cell (The Gallery, Task
    /// 5): commit a move [`Underground::peek_stairs`] already validated,
    /// moving the possession to the destination rung and cell and returning
    /// the new rung's own [`Band`] (`habitation_rungs()[rung]`, the same
    /// index [`Underground::rung_band`] reads).
    ///
    /// `None` on any of [`Underground::peek_stairs`]'s own refusals — never
    /// itself moves the possession on a call that would have refused, which
    /// is what makes it safe for a caller that does not need to charge
    /// anything (this module's own tests are exactly that caller) to call
    /// directly, and safe for a caller that DOES need to charge
    /// (`Session::take_stairs`, `session.rs`) to call only after peeking and
    /// charging first — the same division [`Underground::step`] and
    /// [`Underground::peek`]/[`Underground::commit_step`] already draw.
    pub(crate) fn take_stairs(&mut self) -> Option<Band> {
        let (next, landing) = self.peek_stairs().ok()?;
        self.rung = next;
        self.cell = landing;
        Some(habitation_rungs()[self.rung])
    }
}

/// The outcome of one lateral compass step underground (The Gallery, Task
/// 4; spec §3.2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum StepOutcome {
    /// The possession moved one cell.
    Moved,
    /// The step was refused, with the physical reason why — never a parse
    /// complaint and never a sentence naming a verb or a movement mode.
    Blocked(&'static str),
    /// The step landed on a stairs cell. Moving BETWEEN rungs needs the
    /// stairs verb (Task 5), not another compass bearing, so this is kept
    /// distinct from a plain [`StepOutcome::Moved`].
    NeedsStairs,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `rung_band` must name the rung `descent`'s own first element was
    /// generated for — [`Underground::enter`] always enters at rung `0`,
    /// which is `habitation_rungs()[0]`.
    #[test]
    fn rung_band_names_the_top_of_the_ladder() {
        assert_eq!(habitation_rungs()[0], Band::Undercroft);
    }

    /// `habitation_rungs` excludes `Surface` and keeps the ladder's order —
    /// the invariant [`Underground::rung`] relies on to index `descent`
    /// without drift.
    #[test]
    fn habitation_rungs_excludes_surface() {
        assert!(!habitation_rungs().contains(&Band::Surface));
        assert_eq!(habitation_rungs(), hornvale_terrain::rungs()[1..].to_vec());
    }
}
