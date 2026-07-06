//! Terrain: the tier-0 hand-placed Vale (still the social cascade's seam,
//! spec §8) and the tier-1 tectonic globe — plates, elevation, unrest —
//! computed over the shared kernel Geosphere.
#![warn(missing_docs)]

pub mod boundaries;
pub mod elevation;
pub mod facts;
pub mod globe;
pub mod pins;
pub mod plates;
pub mod provider;
pub mod render;
pub mod streams;

pub use boundaries::{BoundaryKind, CellBoundary};
pub use globe::{GenesisOutcome, GlobeSummary, TectonicGlobe, generate, summarize};
pub use pins::{GenesisError, TerrainPins, parse_pin, pin_strings};
pub use plates::Plate;
pub use provider::GeneratedTerrain;

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// The fixed subdivision level of the shared Geosphere (10 × 4^5 + 2 =
/// 10,242 cells). The composition root builds `Geosphere::new(GLOBE_LEVEL)`
/// once per process; every terrain (and, in Plan 3c, climate) CellMap in a
/// world is built against that mesh and must only ever be queried with it.
pub const GLOBE_LEVEL: u32 = 5;

/// Predicate marking an entity as a traversable place.
pub const IS_PLACE: &str = "is-place";
/// Predicate giving a place's biome.
pub const BIOME: &str = "biome";

/// Every seed-derivation label this crate uses, with docs. All chains hang
/// off the world seed's "terrain" derivation. Labels are permanent
/// save-format contracts.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("terrain", "root stream for tectonic genesis"),
        ("terrain/plate-count", "how many plates"),
        (
            "terrain/plate-seeds",
            "per-plate seed positions on the sphere",
        ),
        (
            "terrain/plate-kind",
            "continental fraction and per-plate continental rolls",
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
    )
}

/// A place as terrain knows it.
#[derive(Debug, Clone, PartialEq)]
pub struct PlaceInfo {
    /// The place's entity id.
    pub id: EntityId,
    /// The place's canonical name.
    pub name: String,
    /// The place's biome.
    pub biome: String,
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "terrain".to_string(),
    }
}

/// Tier-0 genesis: commit one hand-placed vale; return its entity id.
pub fn genesis(world: &mut World) -> Result<EntityId, LedgerError> {
    let vale = world.ledger.mint_entity();
    world.ledger.commit(
        fact(vale, "name", Value::Text("the Vale".to_string())),
        &world.registry,
    )?;
    world
        .ledger
        .commit(fact(vale, IS_PLACE, Value::Flag(true)), &world.registry)?;
    world.ledger.commit(
        fact(vale, BIOME, Value::Text("temperate forest".to_string())),
        &world.registry,
    )?;
    Ok(vale)
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
    use hornvale_kernel::Seed;

    fn world() -> World {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        w
    }

    #[test]
    fn genesis_commits_one_named_place() {
        let mut w = world();
        let vale = genesis(&mut w).unwrap();
        let all = places(&w);
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].id, vale);
        assert_eq!(all[0].name, "the Vale");
        assert_eq!(all[0].biome, "temperate forest");
    }

    #[test]
    fn genesis_is_idempotent_on_facts() {
        // Re-running genesis on the same world must not contradict; the
        // second entity is distinct but its facts must still commit cleanly.
        let mut w = world();
        genesis(&mut w).unwrap();
        genesis(&mut w).unwrap();
        assert_eq!(places(&w).len(), 2);
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.predicate(IS_PLACE).is_some());
        assert!(r.predicate(BIOME).is_some());
    }

    #[test]
    fn stream_labels_are_fully_qualified_and_documented() {
        let labels = stream_labels();
        assert_eq!(labels.len(), 8);
        assert_eq!(labels[0].0, "terrain");
        for (label, doc) in &labels[1..] {
            assert!(label.starts_with("terrain/"), "unqualified label {label}");
            assert!(!doc.is_empty());
        }
    }
}
