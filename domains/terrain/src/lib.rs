//! Terrain, tier 0: one hand-placed vale. Real generation (the region
//! graph, elevation fields) arrives in Campaign 3.
#![warn(missing_docs)]

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Predicate marking an entity as a traversable place.
pub const IS_PLACE: &str = "is-place";
/// Predicate giving a place's biome.
pub const BIOME: &str = "biome";

/// Every seed-derivation label this crate uses (none yet).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register terrain's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PLACE, true, "subject is a traversable place")?;
    registry.register_predicate(BIOME, true, "biome of a place")
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
}
