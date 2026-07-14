//! Terrain: the tier-0 hand-placed Vale (still the social cascade's seam,
//! spec §8) and the tier-1 tectonic globe — plates, elevation, unrest —
//! computed over the shared kernel Geosphere.
#![warn(missing_docs)]

pub mod boundaries;
pub mod crust;
pub mod drainage;
pub mod elevation;
pub mod facts;
pub mod globe;
pub mod lithology;
pub mod pins;
pub mod plates;
pub mod provider;
pub mod render;
pub mod shape;
pub mod streams;

pub use boundaries::{BoundaryKind, CellBoundary};
pub use globe::{GenesisOutcome, GlobeSummary, TectonicGlobe, generate, summarize};
pub use lithology::{
    Basement, Fertility, Hydro, MarginPolarity, MaterialBuffer, RockClass, SoilDepth, SoilOrder,
    cave_proneness, classify_soil, fertility, hydrogeology,
};
pub use pins::{GenesisError, TerrainPins, parse_pin, pin_strings};
pub use plates::Plate;
pub use provider::GeneratedTerrain;

use hornvale_kernel::{ConceptKind, ConceptRegistry, EntityId, RegistryError, World};

/// The *default* subdivision level of the shared Geosphere (10 × 4^6 + 2 =
/// 40,962 cells, ~110 km resolution) — used when `TerrainPins.globe_level`
/// is `None`. Canonical grid raised from level 5 to level 6 in the Crust
/// epoch (spec §5): the coarser grid under-resolved shelf and coastline
/// structure for the sculpting work that campaign does. The composition
/// root builds `Geosphere::new(level)` (per-level cached) once per process
/// per level; every terrain (and, in Plan 3c, climate) CellMap in a world
/// is built against the mesh its level selected and must only ever be
/// queried with it.
/// type-audit: bare-ok(count)
pub const GLOBE_LEVEL: u32 = 6;

/// Predicate marking an entity as a traversable place.
/// type-audit: bare-ok(identifier-text)
pub const IS_PLACE: &str = "is-place";
/// Predicate giving a place's biome.
/// type-audit: bare-ok(identifier-text)
pub const BIOME: &str = "biome";

/// Every seed-derivation label this crate uses, with docs. All chains hang
/// off the world seed's "terrain" derivation. Labels are permanent
/// save-format contracts.
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("terrain", "root stream for tectonic genesis"),
        ("terrain/plate-count", "how many plates"),
        (
            "terrain/plate-seeds",
            "per-plate seed positions on the sphere",
        ),
        (
            "terrain/plate-motion",
            "per-plate Euler pole axis and rate draws",
        ),
        ("terrain/maturity", "per-plate orogenic maturity draws"),
        (
            "terrain/hotspots",
            "hotspot count, positions, and strengths",
        ),
        ("terrain/ocean-fraction", "target ocean fraction draw"),
        (
            "terrain/coast-render",
            "render-lens coastline noise (hash-noise only; no stream draws)",
        ),
        (
            "terrain/cratons",
            "margin draw (scales the ocean-fraction-derived budget, Task 9 \
             iteration 3'), craton count, then per-craton center/radius/age",
        ),
        (
            "terrain/plate-weights",
            "per-plate heavy-tailed Voronoi weight draws",
        ),
        (
            "terrain/plate-edge",
            "plate-edge noise (hash-noise only; no stream draws)",
        ),
        (
            "terrain/lithology",
            "lithology sub-cell hash-noise (hash-noise only; no stream draws)",
        ),
    ]
}

/// Register terrain's contribution to the concept registry: the tier-0
/// place predicates plus the tectonic summary predicates. Idempotent.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PLACE, true, "subject is a traversable place")?;
    registry.register_predicate(BIOME, true, "biome of a place")?;
    registry.register_predicate(
        facts::PLATE_COUNT,
        true,
        "how many tectonic plates the globe has",
    )?;
    registry.register_predicate(
        facts::OCEAN_FRACTION,
        true,
        "fraction of globe cells below sea level",
    )?;
    registry.register_predicate(facts::SEA_LEVEL_M, true, "sea level in meters")?;
    registry.register_predicate(
        facts::HIGHEST_ELEVATION_M,
        true,
        "highest globe cell elevation in meters",
    )?;
    registry.register_predicate(
        facts::TERRAIN_PIN,
        false,
        "a terrain scenario pin, round-trippable",
    )?;
    registry.register_predicate(
        facts::TERRAIN_NOTE,
        false,
        "a note recorded during tectonic genesis",
    )?;

    registry.register_concept("stone", "terrain", ConceptKind::Substance, "rock")?;
    registry.register_concept("mountain", "terrain", ConceptKind::Terrain, "high ground")?;
    registry.register_concept(
        "sea",
        "terrain",
        ConceptKind::Terrain,
        "a body of salt water",
    )
}

/// Terrain as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Terrain;

impl hornvale_kernel::Domain for Terrain {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

/// A place as terrain knows it.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: biome)
#[derive(Debug, Clone, PartialEq)]
pub struct PlaceInfo {
    /// The place's entity id.
    pub id: EntityId,
    /// The place's canonical name.
    pub name: String,
    /// The place's biome.
    pub biome: String,
}

/// Every known place, in commit order.
pub fn places(world: &World) -> Vec<PlaceInfo> {
    world
        .ledger
        .find(IS_PLACE)
        .map(|f| f.subject)
        .map(|id| PlaceInfo {
            id,
            name: world
                .ledger
                .text_of(id, hornvale_kernel::NAME)
                .map(str::to_string)
                .unwrap_or_else(|| format!("place {}", id.0)),
            biome: world
                .ledger
                .text_of(id, BIOME)
                .map(str::to_string)
                .unwrap_or_else(|| "unknown".to_string()),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.predicate(IS_PLACE).is_some());
        assert!(r.predicate(BIOME).is_some());
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        let stone = r.concept("stone").unwrap();
        assert_eq!(stone.domain, "terrain");
        assert_eq!(stone.kind, ConceptKind::Substance);
        for name in ["mountain", "sea"] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "terrain");
            assert_eq!(c.kind, ConceptKind::Terrain);
        }
    }

    #[test]
    fn stream_labels_are_fully_qualified_and_documented() {
        let labels = stream_labels();
        assert_eq!(labels.len(), 12);
        assert_eq!(labels[0].0, "terrain");
        for (label, doc) in &labels[1..] {
            assert!(label.starts_with("terrain/"), "unqualified label {label}");
            assert!(!doc.is_empty());
        }
    }
}
