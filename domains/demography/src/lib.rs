//! Hornvale demography domain: a carrying-capacity field derived from climate
//! and terrain, and the flow-condensation that reads discrete settlements off
//! it as conserved attractors of a population flow. Kernel-only: the
//! composition root supplies each cell's bare climate/terrain inputs; this
//! crate never imports a climate or terrain crate.
//!
//! Condensation is the same field-to-fact projection the codebase performs
//! for biomes (Whittaker classification) and rivers (`terrain::drainage`);
//! lifting all three onto one kernel primitive is future work — see the
//! design spec's scope boundary.
#![warn(missing_docs)]

pub mod byproducts;
pub mod carrying_capacity;
pub mod coexist;
pub mod condense;
pub mod flow;
pub mod footprint;
pub mod founder;
pub mod niche;
pub mod render;
pub mod stack_condense;
pub use byproducts::{Byproducts, byproducts};
pub use carrying_capacity::{CarryingInput, carrying_capacity};
pub use coexist::{BETA, CoexistStack, FLOOR};
pub use condense::{Condensation, condense};
pub use flow::{Flow, flow};
pub use footprint::home_range;
pub use founder::condense_tagged;
pub use render::{density_ppm, refugia_ppm, stack_density_ppm, strife_ppm};
pub use stack_condense::StackSettlement;

use hornvale_kernel::{CellMap, Geosphere, Mass, ResourceVector};
use std::collections::BTreeMap;

/// The one-call demography report: each species' carrying-capacity field K
/// (kept in memory for reuse — the render and any downstream consumer read it
/// straight off this report rather than recomputing it), the settlements
/// condensed from all species' fields together (the [`founder::condense_tagged`]
/// path, still worldgen's source of truth — task A15 flips the consumer to
/// `stack_settlements`), and — ADDITIVELY, alongside it — the coexistence
/// stack ([`coexist::pack`]) and its own byproducts: [`stack_settlements`]
/// (mass-weighted, composition-aware settlements read off the stack) and
/// [`byproducts`] (strife/wilderness/refugia). Building the stack draws
/// nothing from the seed and changes no existing field's value, so adding it
/// is a zero-drift addition to any world already built off `per_species_k` /
/// `settlements`.
///
/// [`stack_settlements`]: DemographyReport::stack_settlements
/// [`byproducts`]: DemographyReport::byproducts
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: settlements)
pub struct DemographyReport {
    /// Each species' carrying-capacity field, tagged by species id. Retained
    /// so callers (the render, calibration) reuse the field instead of
    /// recomputing `carrying_capacity`.
    pub per_species_k: Vec<(u32, CellMap<f64>)>,
    /// Settlements condensed across all species' fields, tagged by the
    /// species that founded them. **This is what worldgen consumes** —
    /// unchanged by the additive fields below.
    pub settlements: Vec<(Condensation, u32)>,
    /// The overlap-weighted, trophically-coupled per-cell density stack —
    /// [`coexist::pack`]'s output. Additive: not yet consumed by worldgen.
    pub stack: coexist::CoexistStack,
    /// Settlements condensed off `stack` (mass-weighted, composition-aware).
    /// Additive: not yet consumed by worldgen (task A15).
    pub stack_settlements: Vec<stack_condense::StackSettlement>,
    /// The stack's derived byproducts (strife, wilderness, refugia).
    /// Additive: not yet consumed by worldgen.
    pub byproducts: byproducts::Byproducts,
}

/// Build the demography report for a world: each species' carrying-capacity
/// field from its inputs, then condensation into settlements (unchanged —
/// still `condense_tagged`, still what worldgen consumes). ADDITIVELY, also
/// packs the coexistence stack (`species` supplies the `(mass, niche)` the
/// packer needs beyond the bare K fields) and derives `stack_settlements` /
/// `byproducts` from it — a pure, seed-free computation that never alters
/// `per_species_k` or `settlements`.
/// type-audit: bare-ok(index: per_species_inputs), bare-ok(index: species), bare-ok(ratio: beta), bare-ok(count: floor), bare-ok(count: threshold)
pub fn report(
    geo: &Geosphere,
    per_species_inputs: &[(u32, CellMap<CarryingInput>)],
    species: &[(u32, Mass, ResourceVector)],
    beta: f64,
    floor: f64,
    threshold: f64,
) -> DemographyReport {
    let per_species_k: Vec<(u32, CellMap<f64>)> = per_species_inputs
        .iter()
        .map(|(tag, inputs)| (*tag, carrying_capacity(geo, inputs)))
        .collect();
    let settlements = condense_tagged(&per_species_k, geo, threshold);

    // Additive: the coexistence stack and its byproducts, alongside the
    // settlements above — never replacing them.
    let stack = coexist::pack(geo, &per_species_k, species, beta, floor);
    let mass_map: BTreeMap<u32, Mass> = species.iter().map(|(id, mass, _)| (*id, *mass)).collect();
    let stack_settlements = stack_condense::condense_stack(geo, &stack, &mass_map, threshold);
    let byproducts = byproducts::byproducts(geo, &stack, &per_species_k, floor);

    DemographyReport {
        per_species_k,
        settlements,
        stack,
        stack_settlements,
        byproducts,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Geosphere, PLANT_FORAGE};

    /// The single-species fixture `report`'s tests share: one omnivore-ish
    /// species (id 0) with a non-zero `PLANT_FORAGE` niche.
    fn one_species() -> Vec<(u32, Mass, ResourceVector)> {
        vec![(
            0u32,
            Mass::new(40.0).unwrap(),
            ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap(),
        )]
    }

    #[test]
    fn report_holds_k_fields_and_settlements() {
        let geo = Geosphere::new(3);
        let inputs = CellMap::from_fn(&geo, |c| CarryingInput {
            habitable: true,
            temperature_c: 20.0,
            moisture: 0.7,
            freshwater: 0.6,
            coastal: c.0 % 2 == 0,
            hostility: 0.0,
        });
        let rep = report(&geo, &[(0u32, inputs)], &one_species(), BETA, FLOOR, 0.0);
        assert_eq!(rep.per_species_k.len(), 1, "K retained in memory for reuse");
        assert!(
            !rep.stack.density.is_empty(),
            "the additive coexistence stack is built alongside settlements"
        );
        assert!(
            !rep.stack_settlements.is_empty(),
            "settlements condense off the stack too"
        );
        assert!(
            !rep.byproducts.strife.is_empty() && rep.byproducts.strife.len() == geo.cells().count(),
            "byproducts.strife is populated over the whole geosphere"
        );
        assert!(
            !rep.settlements.is_empty(),
            "a habitable world condenses settlements"
        );
    }
}
