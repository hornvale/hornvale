//! The ledger holds a registered concept id for a star's class, never
//! Morgan-Keenan prose — and the prose the author's register renders from it
//! parses back to the same id, so `windows/book`'s knowledge round-trip (The
//! Echo's transfer law) still recovers the fact it started from.

use hornvale_kernel::{Seed, Value};

fn seed_42() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"))
}

#[test]
fn the_committed_star_class_is_a_registered_concept() {
    let world = seed_42();
    let mut checked = 0;
    for fact in world.ledger.find(hornvale_astronomy::facts::STAR_CLASS) {
        let Value::Text(id) = &fact.object else {
            panic!("star-class must be Text, got {:?}", fact.object)
        };
        assert!(
            world.registry.concept(id).is_some(),
            "star-class committed {id:?}, which is not a registered concept"
        );
        assert!(
            hornvale_astronomy::class_display(id).is_some(),
            "star-class committed {id:?}, which is not a spectral class"
        );
        checked += 1;
    }
    assert!(checked > 0, "seed 42 must commit a star-class fact");
}

#[test]
fn the_rendered_display_parses_back_to_the_committed_id() {
    for (concept, display) in hornvale_astronomy::SPECTRAL_CLASSES {
        let fragment = format!("orbiting a {display}");
        let recovered = hornvale_book::fact_for_public(&fragment)
            .unwrap_or_else(|| panic!("{fragment:?} must parse"));
        assert_eq!(
            recovered,
            (
                hornvale_astronomy::facts::STAR_CLASS.to_string(),
                Value::Text(concept.to_string())
            ),
            "the round-trip must recover the committed id, not the display"
        );
    }
}
