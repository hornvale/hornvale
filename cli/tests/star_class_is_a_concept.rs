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

/// `"a"` or `"an"` by `word`'s first letter — the same surface rule
/// `windows/book`'s private `indefinite_article` applies, reimplemented here
/// (not imported) so this test is an independent check of the article
/// `fragment_for` actually chose, rather than a re-assertion of whatever
/// `fragment_for` happens to produce.
fn expected_article(word: &str) -> &'static str {
    match word.chars().next().map(|c| c.to_ascii_lowercase()) {
        Some('a' | 'e' | 'i' | 'o' | 'u') => "an",
        _ => "a",
    }
}

#[test]
fn the_rendered_display_parses_back_to_the_committed_id() {
    // Drive the fragment from the renderer itself (`fragment_for_public`),
    // not a hand-rolled `"orbiting a {display}"` — the article ("a" vs
    // "an") is the renderer's choice, and a hand-rolled fragment with a
    // hardcoded article would never notice if `fragment_for` and `fact_for`
    // disagreed about it. Covers all nine of
    // `hornvale_astronomy::SPECTRAL_CLASSES`, not just whichever class
    // seed 42 happens to draw.
    //
    // Checking only that `fact_for_public` recovers the right concept is
    // NOT enough: `fact_for`'s parser strips whichever of "an "/"a " is
    // present, so it recovers the correct id even from a wrongly-articled
    // fragment (e.g. "orbiting a orange giant" still parses to
    // `orange-giant`). The article itself must be asserted against the
    // rendered TEXT, independently of the parse step.
    for (concept, display) in hornvale_astronomy::SPECTRAL_CLASSES {
        let value = Value::Text(concept.to_string());
        let fragment =
            hornvale_book::fragment_for_public(hornvale_astronomy::facts::STAR_CLASS, &value)
                .unwrap_or_else(|| panic!("{concept:?} must render a fragment"));
        let expected_fragment = format!("orbiting {} {display}", expected_article(display));
        assert_eq!(
            fragment, expected_fragment,
            "{concept:?} rendered the wrong article"
        );
        let recovered = hornvale_book::fact_for_public(&fragment)
            .unwrap_or_else(|| panic!("{fragment:?} must parse"));
        assert_eq!(
            recovered,
            (hornvale_astronomy::facts::STAR_CLASS.to_string(), value),
            "the round-trip must recover the committed id, not the display"
        );
    }
}
