//! The substrate proxy — a conservative stand-in for the unbuilt DOM-14
//! lithosphere. Infers only substrate distinctions defensible from existing
//! climate/terrain signals; everything else is `Ordinary`. A real lithology
//! field later replaces this function without touching any consumer.

// `substrate_at` is `pub(crate)`, so it is invisible to the crate's external
// reachability analysis until a non-test caller lands (Task 7's `describe`
// integration). Remove once that lands.
#![allow(dead_code)]

use crate::regime::Substrate;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, quantize};
use hornvale_terrain::GeneratedTerrain;

/// Thresholds are compared on quantized values so the discrete substrate
/// decision is cross-platform byte-identical (decision 0041).
pub(crate) fn substrate_at(
    climate: &GeneratedClimate,
    terrain: &GeneratedTerrain,
    cell: CellId,
) -> Substrate {
    let globe = terrain.globe();
    let elevation = quantize(*globe.elevation.get(cell));
    let sea_level = quantize(globe.sea_level);
    if elevation <= sea_level {
        // Underwater cells keep the ordinary substrate; marine biomes carry
        // their own identity via the base biome.
        return Substrate::Ordinary;
    }
    let unrest = quantize(*globe.unrest.get(cell));
    let moisture = quantize(climate.moisture_at(cell));
    let relief = quantize(elevation - sea_level);

    // Volcanic: high tectonic unrest → basalt (high relief) or ash (low).
    if unrest > 0.6 {
        return if relief > 500.0 {
            Substrate::Basaltic
        } else {
            Substrate::Ashen
        };
    }
    // Evaporite: very dry + flat (a salt pan / playa).
    if moisture < 0.15 && relief < 200.0 {
        return Substrate::Evaporite;
    }
    // Sand: arid lowland where sand seas / dunes form (the drier, flatter
    // Evaporite branch above catches salt pans; this is the broader arid case).
    if moisture < 0.25 && relief < 100.0 {
        return Substrate::Sand;
    }
    Substrate::Ordinary
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::World;
    use hornvale_worldgen::{climate_of, terrain_of};

    #[test]
    fn substrate_is_deterministic_and_total() {
        // Every cell resolves to a substrate; twice-sampled is identical.
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        for c in geo.cells() {
            let a = substrate_at(&climate, &terrain, c);
            let b = substrate_at(&climate, &terrain, c);
            assert_eq!(a, b);
        }
    }

    #[test]
    fn high_unrest_cells_read_volcanic() {
        // Every high-unrest land cell reads Basaltic or Ashen (a total
        // implication — never vacuously misleading).
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        let globe = terrain.globe();
        for c in geo.cells() {
            let above_sea = hornvale_kernel::quantize(*globe.elevation.get(c))
                > hornvale_kernel::quantize(globe.sea_level);
            if above_sea && hornvale_kernel::quantize(*globe.unrest.get(c)) > 0.6 {
                assert!(
                    matches!(
                        substrate_at(&climate, &terrain, c),
                        Substrate::Basaltic | Substrate::Ashen
                    ),
                    "high-unrest land cell {c:?} must read volcanic"
                );
            }
        }
    }
}
