//! Tectonic genesis facts: summary truths only, never per-cell data — the
//! ledger keeps singular authored truths and saves stay small (spec §3).

use crate::globe::{GenesisOutcome, summarize};
use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// Predicate: how many plates the globe has (functional, Number).
pub const PLATE_COUNT: &str = "plate-count";
/// Predicate: achieved ocean fraction (functional, Number).
pub const OCEAN_FRACTION: &str = "ocean-fraction";
/// Predicate: sea level in meters (functional, Number).
pub const SEA_LEVEL_M: &str = "sea-level-m";
/// Predicate: highest cell elevation in meters (functional, Number).
pub const HIGHEST_ELEVATION_M: &str = "highest-elevation-m";
/// Predicate: one round-trippable terrain pin string per pinned value
/// (non-functional, Text) — the terrain sibling of astronomy's scenario-pin.
pub const TERRAIN_PIN: &str = "terrain-pin";
/// Predicate: a note recorded during tectonic genesis (non-functional, Text).
pub const TERRAIN_NOTE: &str = "terrain-note";

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

/// Commit the globe's summary facts and genesis notes against `subject`
/// (the world entity). Summary only — never per-cell facts.
pub fn genesis(
    world: &mut World,
    subject: EntityId,
    outcome: &GenesisOutcome,
) -> Result<(), LedgerError> {
    let summary = summarize(&outcome.globe);
    world.ledger.commit(
        fact(
            subject,
            PLATE_COUNT,
            Value::Number(f64::from(summary.plate_count)),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            OCEAN_FRACTION,
            Value::Number(summary.ocean_fraction),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(subject, SEA_LEVEL_M, Value::Number(summary.sea_level_m)),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HIGHEST_ELEVATION_M,
            Value::Number(summary.highest_elevation_m),
        ),
        &world.registry,
    )?;
    for note in &outcome.notes {
        world.ledger.commit(
            fact(subject, TERRAIN_NOTE, Value::Text(note.clone())),
            &world.registry,
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, Value, World};

    #[test]
    fn genesis_commits_the_summary_facts() {
        let mut world = World::new(Seed(42));
        crate::register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        let geo = hornvale_kernel::Geosphere::new(3);
        let outcome = crate::generate(Seed(42), &geo, &crate::TerrainPins::default()).unwrap();
        genesis(&mut world, subject, &outcome).unwrap();
        let summary = crate::summarize(&outcome.globe);
        assert_eq!(
            world.ledger.value_of(subject, PLATE_COUNT),
            Some(&Value::Number(f64::from(summary.plate_count)))
        );
        assert_eq!(
            world.ledger.value_of(subject, OCEAN_FRACTION),
            Some(&Value::Number(summary.ocean_fraction))
        );
        assert_eq!(
            world.ledger.value_of(subject, SEA_LEVEL_M),
            Some(&Value::Number(summary.sea_level_m))
        );
        assert_eq!(
            world.ledger.value_of(subject, HIGHEST_ELEVATION_M),
            Some(&Value::Number(summary.highest_elevation_m))
        );
    }

    #[test]
    fn register_concepts_is_idempotent() {
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        crate::register_concepts(&mut registry).unwrap();
        crate::register_concepts(&mut registry).unwrap();
        assert!(registry.predicate(TERRAIN_PIN).is_some());
        assert!(registry.predicate(TERRAIN_NOTE).is_some());
    }
}
