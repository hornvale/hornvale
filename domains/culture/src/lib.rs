//! Culture, tier 0: a fixed caste structure for the goblin village,
//! straight from the vision book's goblin village chapter.
#![warn(missing_docs)]

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Predicate relating a settlement to a caste present in it.
pub const HAS_CASTE: &str = "has-caste";

/// The tier-0 caste structure, lowest to highest.
pub const CASTES: [&str; 5] = ["slave", "fighter", "cook", "shaman", "chief"];

/// Every seed-derivation label this crate uses (none yet).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register culture's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(HAS_CASTE, false, "a caste present in a settlement")
}

/// Tier-0 genesis: give the village the fixed caste structure.
pub fn genesis(world: &mut World, village: EntityId) -> Result<(), LedgerError> {
    for caste in CASTES {
        world.ledger.commit(
            Fact {
                subject: village,
                predicate: HAS_CASTE.to_string(),
                object: Value::Text(caste.to_string()),
                place: None,
                day: Some(0.0),
                provenance: "culture".to_string(),
            },
            &world.registry,
        )?;
    }
    Ok(())
}

/// The castes of `village`, in commit order.
pub fn castes_of(world: &World, village: EntityId) -> Vec<String> {
    world
        .ledger
        .facts_about(village)
        .filter(|f| f.predicate == HAS_CASTE)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn genesis_gives_the_village_all_five_castes_in_order() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        genesis(&mut w, village).unwrap();
        assert_eq!(castes_of(&w, village), CASTES.to_vec());
    }

    #[test]
    fn other_entities_have_no_castes() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        let other = w.ledger.mint_entity();
        genesis(&mut w, village).unwrap();
        assert!(castes_of(&w, other).is_empty());
    }
}
