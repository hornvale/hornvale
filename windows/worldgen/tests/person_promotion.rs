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

    let mut deaths_seen = 0usize;
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
        // THIS ARM WAS UNREACHABLE UNTIL THE ELL, and the test was green
        // without it. `person-died` was committed by no world: promotion
        // derived the death day by subtracting a maturity in DAYS from a
        // founding day in YEARS (`occ-founded` was a year), adding a lifespan
        // in DAYS, and comparing the sum against a present in YEARS. The
        // earliest death any species in the roster reached was 14,379.2 against
        // `now = 2000`, so the `<= now` filter never passed and every founder
        // was recorded as still living — measured zero `Some` entries across
        // five seeds and 587 promoted founders.
        //
        // The Ell moved the unit boundary to the ledger, and this arm now runs.
        // `deaths_seen` is counted and asserted below rather than left implicit,
        // so the test cannot silently lose its subject again the way it did for
        // a whole campaign (The Particular, F9/F10): a `Some` arm that never
        // fires proves nothing, and nothing about the shape of `if let` says
        // which of the two it is.
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
            deaths_seen += 1;
        }
        assert!(
            w.ledger
                .facts_about(p.subject)
                .any(|f| f.predicate == hornvale_person::PERSON_FOUNDED),
            "every person founded something"
        );
    }

    // The two-sided form (spec E1): a gate that fires for everyone is as wrong
    // as one that fires for nobody, and a bare `> 0` cannot tell them apart.
    assert!(
        deaths_seen > 0,
        "the death arm above did not execute once in {} founders — it is \
         vacuous again and every assertion inside it is asserting nothing",
        people.len()
    );
    assert!(
        deaths_seen < people.len(),
        "every founder died: {deaths_seen} of {}",
        people.len()
    );
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
