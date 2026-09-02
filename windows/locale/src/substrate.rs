//! The substrate proxy — a conservative stand-in for the unbuilt DOM-14
//! lithosphere. Infers only substrate distinctions defensible from existing
//! climate/terrain signals; everything else is `Ordinary`. A real lithology
//! field later replaces this function without touching any consumer.

use hornvale_climate::{GeneratedClimate, GroundKind};
use hornvale_kernel::{Vertex, quantize};
use hornvale_terrain::GeneratedTerrain;

/// Thresholds are compared on quantized values so the discrete substrate
/// decision is cross-platform byte-identical (decision 0041).
pub(crate) fn substrate_at(
    climate: &GeneratedClimate,
    terrain: &GeneratedTerrain,
    vertex: Vertex,
) -> GroundKind {
    let globe = terrain.globe();
    let elevation = quantize(globe.elevation.get(vertex).get());
    let sea_level = quantize(globe.sea_level.get());
    if elevation <= sea_level {
        // Underwater vertices keep the ordinary substrate; marine biomes carry
        // their own identity via the base biome.
        return GroundKind::Ordinary;
    }
    let unrest = quantize(*globe.unrest.get(vertex));
    let moisture = quantize(climate.moisture_at(vertex));
    let relief = quantize(elevation - sea_level);

    // Volcanic: high tectonic unrest → basalt (high relief) or ash (low).
    if unrest > 0.6 {
        return if relief > 500.0 {
            GroundKind::Basaltic
        } else {
            GroundKind::Ashen
        };
    }
    // Evaporite: very dry + flat (a salt pan / playa).
    if moisture < 0.15 && relief < 200.0 {
        return GroundKind::Evaporite;
    }
    // Sand: arid lowland where sand seas / dunes form (the drier, flatter
    // Evaporite branch above catches salt pans; this is the broader arid case).
    if moisture < 0.25 && relief < 100.0 {
        return GroundKind::Sand;
    }
    GroundKind::Ordinary
}

#[cfg(test)]
mod tests {
    // Test fixture (decision 0092): calls the sculpt/fit derivation entry
    // points directly to build its own world state, once per test — the
    // sanctioned test-fixture posture the weir's spec carves out.
    #![allow(clippy::disallowed_methods)]
    use super::*;
    use hornvale_kernel::World;
    use hornvale_worldgen::{climate_of, terrain_of};

    #[test]
    fn substrate_is_deterministic_and_total() {
        // Every vertex resolves to a substrate; twice-sampled is identical.
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        for c in geo.vertices() {
            let a = substrate_at(&climate, &terrain, c);
            let b = substrate_at(&climate, &terrain, c);
            assert_eq!(a, b);
        }
    }

    #[test]
    fn high_unrest_vertices_read_volcanic() {
        // Every high-unrest land vertex reads Basaltic or Ashen (a total
        // implication — never vacuously misleading).
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        let globe = terrain.globe();
        for c in geo.vertices() {
            let above_sea = hornvale_kernel::quantize(globe.elevation.get(c).get())
                > hornvale_kernel::quantize(globe.sea_level.get());
            if above_sea && hornvale_kernel::quantize(*globe.unrest.get(c)) > 0.6 {
                assert!(
                    matches!(
                        substrate_at(&climate, &terrain, c),
                        GroundKind::Basaltic | GroundKind::Ashen
                    ),
                    "high-unrest land vertex {c:?} must read volcanic"
                );
            }
        }
    }
}
