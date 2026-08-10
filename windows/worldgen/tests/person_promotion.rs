//! Promotion is a pure function of the world it reads: two builds of one seed
//! produce the same cast, and every promoted person is internally coherent.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

#[test]
fn every_person_is_born_before_they_die_and_after_their_community() {
    let w = world();
    let people: Vec<&hornvale_kernel::Fact> = w.ledger.find(hornvale_person::IS_PERSON).collect();
    assert!(!people.is_empty(), "seed 42 should remember some founders");

    for p in &people {
        let born = w
            .ledger
            .facts_about(p.subject)
            .find(|f| f.predicate == hornvale_person::PERSON_BORN)
            .and_then(|f| match f.object {
                hornvale_kernel::Value::Number(n) => Some(n),
                _ => None,
            })
            .expect("every person has a birth day");
        // THIS ARM IS CURRENTLY UNREACHABLE, and the test is green without it.
        // `person-died` is committed by no world: promotion derives the death
        // day by subtracting a maturity in DAYS from a founding day in YEARS
        // (`occ-founded` is a year — see `descent.rs::founded_year`), adding a
        // lifespan in DAYS, and comparing the sum against a present in YEARS.
        // The earliest death any species in the roster reaches is 14,379.2
        // against `now = 2000`, so the `<= now` filter never passes and every
        // founder is recorded as still living. Measured zero `Some` entries
        // across five seeds and 587 promoted founders.
        //
        // Whoever repairs that unit mismatch should also assert that SOME
        // person in the world carries a death fact, so this test cannot
        // silently lose its subject again (The Particular, F9/F10).
        if let Some(died) = w
            .ledger
            .facts_about(p.subject)
            .find(|f| f.predicate == hornvale_person::PERSON_DIED)
            .and_then(|f| match f.object {
                hornvale_kernel::Value::Number(n) => Some(n),
                _ => None,
            })
        {
            assert!(died > born, "death must follow birth: {died} vs {born}");
        }
        assert!(
            w.ledger
                .facts_about(p.subject)
                .any(|f| f.predicate == hornvale_person::PERSON_FOUNDED),
            "every person founded something"
        );
    }
}

#[test]
fn the_cast_is_byte_identical_across_two_builds() {
    let a = world();
    let b = world();
    let cast = |w: &hornvale_kernel::World| -> Vec<String> {
        w.ledger
            .find(hornvale_person::PERSON_BORN)
            .map(|f| format!("{:?}|{:?}", f.subject, f.object))
            .collect()
    };
    assert_eq!(
        cast(&a),
        cast(&b),
        "the same seed remembers the same founders"
    );
}
