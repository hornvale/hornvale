//! Culture, tier 1: emergent caste/role structure derived from
//! environmental pressures (subsistence, surplus, population, threat).
#![warn(missing_docs)]

pub mod subsistence;
pub use subsistence::{BiomeClass, Subsistence, fertility, subsistence};

pub mod structure;
pub use structure::{EnvSummary, PsychSummary, RoleVocabulary, structure};

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// Predicate relating a settlement to a caste present in it.
/// type-audit: bare-ok(identifier-text)
pub const HAS_CASTE: &str = "has-caste";

/// Predicate: a settlement's subsistence mode (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SUBSISTENCE: &str = "subsistence";

/// Every seed-derivation label this crate uses (none yet).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register culture's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(HAS_CASTE, false, "a caste present in a settlement")?;
    registry.register_predicate(SUBSISTENCE, true, "a settlement's subsistence mode")
}

/// Culture as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Culture;

impl hornvale_kernel::Domain for Culture {
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

/// Tier-1 genesis: commit the settlement's subsistence mode and its emergent
/// caste/role structure (from `structure`). Replaces the tier-0 fixed ladder.
pub fn genesis(
    world: &mut World,
    settlement: EntityId,
    env: &EnvSummary,
    psych: &PsychSummary,
) -> Result<(), LedgerError> {
    world.ledger.commit(
        Fact {
            subject: settlement,
            predicate: SUBSISTENCE.to_string(),
            object: Value::Text(env.subsistence.name().to_string()),
            place: None,
            day: Some(0.0),
            provenance: "culture".to_string(),
        },
        &world.registry,
    )?;
    for caste in structure(env, psych) {
        world.ledger.commit(
            Fact {
                subject: settlement,
                predicate: HAS_CASTE.to_string(),
                object: Value::Text(caste),
                place: None,
                day: Some(0.0),
                provenance: "culture".to_string(),
            },
            &world.registry,
        )?;
    }
    Ok(())
}

/// The castes of `settlement`, in commit order.
/// type-audit: bare-ok(identifier-text)
pub fn castes_of(world: &World, settlement: EntityId) -> Vec<String> {
    world
        .ledger
        .facts_about(settlement)
        .filter(|f| f.predicate == HAS_CASTE)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect()
}

/// The subsistence mode committed for a settlement, if any.
/// type-audit: bare-ok(identifier-text)
pub fn subsistence_of(world: &World, settlement: EntityId) -> Option<String> {
    match world.ledger.value_of(settlement, SUBSISTENCE) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn genesis_commits_the_emergent_structure_and_subsistence() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let s = w.ledger.mint_entity();
        let env = EnvSummary {
            subsistence: Subsistence::Farming,
            surplus: 0.8,
            population: 500,
            threat: 0.1,
        };
        let psych = PsychSummary::default();
        genesis(&mut w, s, &env, &psych).unwrap();
        assert_eq!(castes_of(&w, s), structure(&env, &psych));
        assert_eq!(subsistence_of(&w, s).as_deref(), Some("farming"));
    }

    #[test]
    fn lean_vs_rich_yield_different_structures() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();

        let lean = w.ledger.mint_entity();
        let lean_env = EnvSummary {
            subsistence: Subsistence::Foraging,
            surplus: 0.1,
            population: 30,
            threat: 0.1,
        };
        let psych = PsychSummary::default();
        genesis(&mut w, lean, &lean_env, &psych).unwrap();

        let rich = w.ledger.mint_entity();
        let rich_env = EnvSummary {
            subsistence: Subsistence::Farming,
            surplus: 0.8,
            population: 500,
            threat: 0.1,
        };
        genesis(&mut w, rich, &rich_env, &psych).unwrap();

        let lean_castes = castes_of(&w, lean);
        let rich_castes = castes_of(&w, rich);

        assert!(lean_castes.len() < rich_castes.len());
        assert_eq!(subsistence_of(&w, lean).as_deref(), Some("foraging"));
        assert_eq!(subsistence_of(&w, rich).as_deref(), Some("farming"));
    }

    #[test]
    fn other_entities_have_no_castes() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let s = w.ledger.mint_entity();
        let env = EnvSummary {
            subsistence: Subsistence::Farming,
            surplus: 0.8,
            population: 500,
            threat: 0.1,
        };
        let psych = PsychSummary::default();
        genesis(&mut w, s, &env, &psych).unwrap();
        let other = w.ledger.mint_entity();
        assert!(castes_of(&w, other).is_empty());
        assert_eq!(subsistence_of(&w, other), None);
    }
}
