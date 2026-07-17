//! Tectonic genesis facts: summary truths only, never per-cell data — the
//! ledger keeps singular authored truths and saves stay small (spec §3).

use crate::globe::{GenesisOutcome, summarize};
use crate::rift;
use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// Predicate: how many plates the globe has (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const PLATE_COUNT: &str = "plate-count";
/// Predicate: achieved ocean fraction (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OCEAN_FRACTION: &str = "ocean-fraction";
/// Predicate: sea level in meters (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const SEA_LEVEL_M: &str = "sea-level-m";
/// Predicate: highest cell elevation in meters (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const HIGHEST_ELEVATION_M: &str = "highest-elevation-m";
/// Predicate: one round-trippable terrain pin string per pinned value
/// (non-functional, Text) — the terrain sibling of astronomy's scenario-pin.
/// type-audit: bare-ok(identifier-text)
pub const TERRAIN_PIN: &str = "terrain-pin";
/// Predicate: a note recorded during tectonic genesis (non-functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const TERRAIN_NOTE: &str = "terrain-note";
/// Predicate: one rifted-conjugate-pair fact per seam (non-functional,
/// Text — `"craton-{a} ↔ craton-{b}"`), rift-and-fit spec §9. Conjugate
/// margins fit up to subsequent erosion — the carve runs downstream of the
/// crust field unchanged and erodes each side independently (decision
/// ledger #10), so this predicate never promises byte-exact fit post-carve.
/// type-audit: bare-ok(identifier-text)
pub const RIFTED_FROM: &str = "rifted-from";
/// Predicate: a seam's derived breakup age (non-functional, Number) —
/// the pair's relative displacement angle (the sum of each craton's own
/// assembly-to-final rotation angle, `rift::rotation_for`) divided by the
/// globe's drawn `spreading_rate` (rift-and-fit spec §3.5). Narration
/// only, never a forward integrator.
/// type-audit: bare-ok(identifier-text)
pub const BREAKUP_AGE: &str = "breakup-age";
/// Predicate: the globe's one drawn global spreading rate, rad-per-unit-age
/// (functional, Number; rift-and-fit spec §3/§9).
/// type-audit: bare-ok(identifier-text)
pub const SPREADING_RATE: &str = "spreading-rate";

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

    let rift = &outcome.globe.rift;
    if !rift.seams.is_empty() {
        world.ledger.commit(
            fact(subject, SPREADING_RATE, Value::Number(rift.spreading_rate)),
            &world.registry,
        )?;
    }
    // Seams are already ascending (a, b) — `rift::draw_rift`'s own
    // canonicalization — so this iteration is deterministic without an
    // extra sort.
    for seam in &rift.seams {
        let a = seam.a as usize;
        let b = seam.b as usize;
        // Majors are id-indexed by construction (`crust::draw_cratons`
        // assigns `id: i` by position), so a seam's craton ids double as
        // indices into both `rift.assembly` (the pre-rift contact frame)
        // and `outcome.globe.cratons` (the final, post-rift centers).
        let (_, angle_a) = rift::rotation_for(rift.assembly[a], outcome.globe.cratons[a].center);
        let (_, angle_b) = rift::rotation_for(rift.assembly[b], outcome.globe.cratons[b].center);
        let breakup_age = (angle_a + angle_b) / rift.spreading_rate;
        world.ledger.commit(
            fact(
                subject,
                RIFTED_FROM,
                Value::Text(format!("craton-{} ↔ craton-{}", seam.a, seam.b)),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(subject, BREAKUP_AGE, Value::Number(breakup_age)),
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
        // The ledger quantizes numeric objects on commit, so each stored value
        // is the canonical form of the raw summary statistic.
        assert_eq!(
            world.ledger.value_of(subject, OCEAN_FRACTION),
            Some(&Value::Number(hornvale_kernel::quantize(
                summary.ocean_fraction
            )))
        );
        assert_eq!(
            world.ledger.value_of(subject, SEA_LEVEL_M),
            Some(&Value::Number(hornvale_kernel::quantize(
                summary.sea_level_m
            )))
        );
        assert_eq!(
            world.ledger.value_of(subject, HIGHEST_ELEVATION_M),
            Some(&Value::Number(hornvale_kernel::quantize(
                summary.highest_elevation_m
            )))
        );
    }

    #[test]
    fn register_concepts_is_idempotent() {
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        crate::register_concepts(&mut registry).unwrap();
        crate::register_concepts(&mut registry).unwrap();
        assert!(registry.predicate(TERRAIN_PIN).is_some());
        assert!(registry.predicate(TERRAIN_NOTE).is_some());
        assert!(registry.predicate(RIFTED_FROM).is_some());
        assert!(registry.predicate(BREAKUP_AGE).is_some());
        assert!(registry.predicate(SPREADING_RATE).is_some());
    }

    #[test]
    fn genesis_commits_rift_facts_on_a_default_world() {
        let mut world = World::new(Seed(42));
        crate::register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        let geo = hornvale_kernel::Geosphere::new(3);
        let outcome = crate::generate(Seed(42), &geo, &crate::TerrainPins::default()).unwrap();
        assert!(
            !outcome.globe.rift.seams.is_empty(),
            "seed 42's default assembly must seam"
        );
        genesis(&mut world, subject, &outcome).unwrap();
        let rifted: Vec<_> = world.ledger.find(RIFTED_FROM).collect();
        assert!(!rifted.is_empty(), "expected at least one rifted-from fact");
        assert_eq!(
            rifted.len(),
            outcome.globe.rift.seams.len(),
            "one rifted-from fact per seam"
        );
        let ages: Vec<_> = world.ledger.find(BREAKUP_AGE).collect();
        assert_eq!(ages.len(), rifted.len(), "one breakup-age fact per seam");
        let rates: Vec<_> = world.ledger.find(SPREADING_RATE).collect();
        assert_eq!(rates.len(), 1, "exactly one spreading-rate fact");
        assert_eq!(
            rates[0].object,
            Value::Number(hornvale_kernel::quantize(outcome.globe.rift.spreading_rate))
        );
    }

    #[test]
    fn genesis_commits_zero_rift_facts_on_a_single_craton_world() {
        let mut world = World::new(Seed(3));
        crate::register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        let geo = hornvale_kernel::Geosphere::new(3);
        let pins = crate::TerrainPins {
            continents: Some(1),
            ..crate::TerrainPins::default()
        };
        let outcome = crate::generate(Seed(3), &geo, &pins).unwrap();
        assert!(
            outcome.globe.rift.seams.is_empty(),
            "a lone craton must not seam"
        );
        genesis(&mut world, subject, &outcome).unwrap();
        assert_eq!(world.ledger.find(RIFTED_FROM).count(), 0);
        assert_eq!(world.ledger.find(BREAKUP_AGE).count(), 0);
        assert_eq!(world.ledger.find(SPREADING_RATE).count(), 0);
    }
}
