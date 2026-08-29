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

use crate::lattice::Cell;
use crate::underworld_level::{Level, LevelCellKind, generate_descent_for_character};

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
