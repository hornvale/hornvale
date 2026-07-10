//! Tier-1 settlement genesis: commit a scatter of generated settlements, each
//! its own place entity tagged with its cell, coordinates, biome, name, and
//! population. The first entry is the flagship. Replaces the tier-0 single
//! hand-fed village.

use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// The fully-resolved per-cell data the composition root hands to genesis
/// (placement geometry plus the name/biome/population the root drew or read).
/// type-audit: pending(wave-3: cell), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(identifier-text: biome), bare-ok(identifier-text: name), bare-ok(count: population)
#[derive(Debug, Clone, PartialEq)]
pub struct PlacedSettlement {
    /// The Geosphere cell id this settlement sits on.
    pub cell: u32,
    /// Latitude, degrees.
    pub latitude: f64,
    /// Longitude, degrees.
    pub longitude: f64,
    /// Biome name (from the climate field, at the root).
    pub biome: String,
    /// Generated settlement name.
    pub name: String,
    /// Population.
    pub population: u32,
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(subject),
        day: Some(0.0),
        provenance: "settlement".to_string(),
    }
}

/// Commit a scatter of generated settlements, each its own place entity. The
/// first entry becomes the flagship (first `is-settlement` fact). Returns the
/// minted entity ids in the given order.
pub fn genesis(
    world: &mut World,
    settlements: &[PlacedSettlement],
) -> Result<Vec<EntityId>, LedgerError> {
    let mut ids = Vec::with_capacity(settlements.len());
    for s in settlements {
        let id = world.ledger.mint_entity();
        world.ledger.commit(
            fact(id, hornvale_kernel::NAME, Value::Text(s.name.clone())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::IS_PLACE, Value::Flag(true)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::BIOME, Value::Text(s.biome.clone())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::IS_SETTLEMENT, Value::Flag(true)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                crate::POPULATION,
                Value::Number(f64::from(s.population)),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::CELL_ID, Value::Number(f64::from(s.cell))),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::LATITUDE, Value::Number(s.latitude)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, crate::LONGITUDE, Value::Number(s.longitude)),
            &world.registry,
        )?;
        ids.push(id);
    }
    Ok(ids)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::register_concepts;
    use hornvale_kernel::Seed;

    fn placed(cell: u32, name: &str, pop: u32) -> PlacedSettlement {
        PlacedSettlement {
            cell,
            latitude: 10.0,
            longitude: 20.0,
            biome: "temperate-forest".to_string(),
            name: name.to_string(),
            population: pop,
        }
    }

    /// Settlement commits facts against `is-place`/`biome` but does not own
    /// their registration (terrain does, at the composition root). Isolated
    /// unit tests stand in for that root by registering them here, mirroring
    /// terrain's exact definitions so a real multi-domain wiring cannot
    /// conflict with this test setup.
    fn register_place_concepts_for_test(registry: &mut hornvale_kernel::ConceptRegistry) {
        registry
            .register_predicate(crate::IS_PLACE, true, "subject is a traversable place")
            .unwrap();
        registry
            .register_predicate(crate::BIOME, true, "biome of a place")
            .unwrap();
    }

    #[test]
    fn genesis_commits_each_as_a_place_and_settlement_flagship_first() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        register_place_concepts_for_test(&mut w.registry);
        let ids = genesis(
            &mut w,
            &[placed(5, "Bolgna", 300), placed(99, "Zagrak", 40)],
        )
        .unwrap();
        assert_eq!(ids.len(), 2);
        // Flagship (first) is the first is-settlement fact.
        assert_eq!(
            w.ledger.find(crate::IS_SETTLEMENT).next().unwrap().subject,
            ids[0]
        );
        // Each is both a place and a settlement, tagged with its cell.
        for (id, cell) in ids.iter().zip([5u32, 99]) {
            assert!(w.ledger.text_of(*id, hornvale_kernel::NAME).is_some());
            assert!(
                matches!(w.ledger.value_of(*id, crate::CELL_ID), Some(Value::Number(n)) if *n as u32 == cell)
            );
            assert_eq!(
                w.ledger.text_of(*id, crate::BIOME),
                Some("temperate-forest")
            );
        }
        // Population round-trips on the flagship.
        assert!(
            matches!(w.ledger.value_of(ids[0], crate::POPULATION), Some(Value::Number(n)) if *n as u32 == 300)
        );
    }

    #[test]
    fn empty_placement_commits_nothing() {
        let mut w = World::new(Seed(1));
        register_concepts(&mut w.registry).unwrap();
        register_place_concepts_for_test(&mut w.registry);
        assert!(genesis(&mut w, &[]).unwrap().is_empty());
        assert!(w.ledger.find(crate::IS_SETTLEMENT).next().is_none());
    }
}
