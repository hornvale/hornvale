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

pub mod carrying_capacity;
pub mod condense;
pub mod flow;
pub mod founder;
pub mod render;
pub use carrying_capacity::{CarryingInput, carrying_capacity};
pub use condense::{Condensation, condense};
pub use flow::{Flow, flow};
pub use founder::condense_tagged;
pub use render::density_ppm;

use hornvale_kernel::{CellMap, Geosphere};

/// The one-call demography report: each species' carrying-capacity field K
/// (kept in memory for reuse — the render and any downstream consumer read it
/// straight off this report rather than recomputing it) and the settlements
/// condensed from all species' fields together.
/// type-audit: bare-ok(index: per_species_k), bare-ok(index: settlements)
pub struct DemographyReport {
    /// Each species' carrying-capacity field, tagged by species id. Retained
    /// so callers (the render, calibration) reuse the field instead of
    /// recomputing `carrying_capacity`.
    pub per_species_k: Vec<(u32, CellMap<f64>)>,
    /// Settlements condensed across all species' fields, tagged by the
    /// species that founded them.
    pub settlements: Vec<(Condensation, u32)>,
}

/// Build the demography report for a world: each species' carrying-capacity
/// field from its inputs, then condensation into settlements. The K fields
/// are kept on the report for in-memory reuse.
/// type-audit: bare-ok(index: per_species_inputs), bare-ok(count: threshold)
pub fn report(
    geo: &Geosphere,
    per_species_inputs: &[(u32, CellMap<CarryingInput>)],
    threshold: f64,
) -> DemographyReport {
    let per_species_k: Vec<(u32, CellMap<f64>)> = per_species_inputs
        .iter()
        .map(|(tag, inputs)| (*tag, carrying_capacity(geo, inputs)))
        .collect();
    let settlements = condense_tagged(&per_species_k, geo, threshold);
    DemographyReport {
        per_species_k,
        settlements,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

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
        let rep = report(&geo, &[(0u32, inputs)], 0.0);
        assert_eq!(rep.per_species_k.len(), 1, "K retained in memory for reuse");
        assert!(
            !rep.settlements.is_empty(),
            "a habitable world condenses settlements"
        );
    }
}
