//! Paleoclimate genesis facts: summary truths only, never per-cell data. The
//! full strata live on the non-serialized `PaleoRecord`; here we commit the
//! glacial maximum, the ice extent, one salient fossil shoreline, one refugium,
//! and the narrative "the frost retreated" so historiography can recount it.

use crate::strata::PaleoRecord;
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Geosphere, LedgerError, RegistryError, Value, World,
};

/// Predicate: the standard day of peak ice (functional, Number).
pub const GLACIAL_MAXIMUM_ERA: &str = "glacial-maximum-era";
/// Predicate: land fraction under ice at the maximum (functional, Number).
pub const MAX_ICE_FRACTION: &str = "max-ice-fraction";
/// Predicate: a salient fossil shoreline, described (non-functional, Text).
pub const FOSSIL_SHORELINE: &str = "fossil-shoreline";
/// Predicate: a place that stayed habitable through the cold (non-functional, Text).
pub const REFUGIUM: &str = "refugium";
/// Predicate: the narrative record of deglaciation (non-functional, Text).
pub const FROST_RETREAT: &str = "frost-retreat";

/// Register paleoclimate's predicates. Value-kinds documented per ADR 0010.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(GLACIAL_MAXIMUM_ERA, true, "standard day of peak ice extent")?;
    registry.register_predicate(
        MAX_ICE_FRACTION,
        true,
        "land fraction under ice at the glacial maximum",
    )?;
    registry.register_predicate(
        FOSSIL_SHORELINE,
        false,
        "a fossil shoreline left by deep-time sea-level change",
    )?;
    registry.register_predicate(
        REFUGIUM,
        false,
        "a place habitable through the glacial maximum",
    )?;
    registry.register_predicate(FROST_RETREAT, false, "the deep-time record of deglaciation")?;
    Ok(())
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "paleoclimate".to_string(),
    }
}

/// A short human description of a representative band/refugium cell: the cell
/// with the largest latitude magnitude (deterministic; ties → lowest CellId).
fn representative(geo: &Geosphere, mask: &hornvale_kernel::CellMap<bool>) -> Option<String> {
    let cell = geo.cells().filter(|c| *mask.get(*c)).max_by(|a, b| {
        geo.coord(*a)
            .latitude
            .abs()
            .total_cmp(&geo.coord(*b).latitude.abs())
            .then(b.0.cmp(&a.0))
    })?;
    let coord = geo.coord(cell);
    Some(format!(
        "near {:.0}°, {:.0}°",
        coord.latitude, coord.longitude
    ))
}

/// Commit the paleoclimate summary facts against `subject` (the world entity).
pub fn genesis(
    world: &mut World,
    subject: EntityId,
    geo: &Geosphere,
    record: &PaleoRecord,
) -> Result<(), LedgerError> {
    world.ledger.commit(
        fact(
            subject,
            GLACIAL_MAXIMUM_ERA,
            Value::Number(record.glacial_maximum_day),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            MAX_ICE_FRACTION,
            Value::Number(record.max_ice_fraction),
        ),
        &world.registry,
    )?;
    // The narrative strata facts exist only if a glacial history happened at
    // all: under the zero-forcing null control the ice never advanced, so no
    // shoreline moved and no refugium is distinct from the present — committing
    // any of these would describe a glaciation that never occurred (spec §9).
    // The two Number summaries above stay unconditional: `max-ice-fraction`
    // is 0.0 there, which is itself the honest null-control measurement.
    if record.max_ice_fraction > 0.0 {
        if let Some(desc) = representative(geo, &record.shoreline) {
            world.ledger.commit(
                fact(
                    subject,
                    FOSSIL_SHORELINE,
                    Value::Text(format!("the sea once reached {desc}")),
                ),
                &world.registry,
            )?;
        }
        if let Some(desc) = representative(geo, &record.refugia) {
            world.ledger.commit(
                fact(
                    subject,
                    REFUGIUM,
                    Value::Text(format!("life persisted {desc} through the cold")),
                ),
                &world.registry,
            )?;
        }
        world.ledger.commit(
            fact(
                subject,
                FROST_RETREAT,
                Value::Text("the frost retreated".to_string()),
            ),
            &world.registry,
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::{EraClimate, extract};
    use hornvale_kernel::{CellMap, Seed, World};

    fn cold_world_record(geo: &Geosphere) -> PaleoRecord {
        let elev = CellMap::from_fn(geo, |_| 100.0);
        let eras = vec![EraClimate {
            day: 500_000.0,
            ice: CellMap::from_fn(geo, |_| true),
            habitable: CellMap::from_fn(geo, |c| geo.coord(c).latitude.abs() < 30.0),
            sea_level: -60.0,
            ice_fraction: 0.8,
        }];
        extract(geo, &elev, 0.0, &eras)
    }

    #[test]
    fn register_is_idempotent() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.predicate(GLACIAL_MAXIMUM_ERA).unwrap().functional);
        assert!(!r.predicate(FROST_RETREAT).unwrap().functional);
    }

    #[test]
    fn genesis_commits_the_frost_retreat_and_maximum() {
        let geo = Geosphere::new(3);
        let mut world = World::new(Seed(1));
        register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        let record = cold_world_record(&geo);
        genesis(&mut world, subject, &geo, &record).unwrap();
        let frost = world.ledger.find(FROST_RETREAT).count();
        assert_eq!(frost, 1, "a glaciated world records the frost retreating");
        let max = world.ledger.find(GLACIAL_MAXIMUM_ERA).next().unwrap();
        assert_eq!(max.object, Value::Number(500_000.0));
    }

    #[test]
    fn zero_ice_world_records_no_frost_retreat() {
        let geo = Geosphere::new(3);
        let elev = CellMap::from_fn(&geo, |_| 100.0);
        let eras = vec![EraClimate {
            day: 0.0,
            ice: CellMap::from_fn(&geo, |_| false),
            habitable: CellMap::from_fn(&geo, |_| true),
            sea_level: 0.0,
            ice_fraction: 0.0,
        }];
        let record = extract(&geo, &elev, 0.0, &eras);
        let mut world = World::new(Seed(1));
        register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        genesis(&mut world, subject, &geo, &record).unwrap();
        assert_eq!(world.ledger.find(FROST_RETREAT).count(), 0);
    }
}
