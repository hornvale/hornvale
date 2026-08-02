//! `Void::Unnamed` must be a constraint, not a comment.
//!
//! A concept whose lexeme correspondent is `Absent(Void::Unnamed)` is
//! objectively real and has no word in this world. Nothing enforces that on
//! its own: `hornvale_language::build_lexicon` draws its universe from the
//! exposures map rather than the registry, so a concept could be declared
//! unnameable and still be handed a word.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{ConceptRegistry, Correspondent, Void};

/// Every concept the roster registers as `Unnamed`.
fn unnameable(registry: &ConceptRegistry) -> Vec<String> {
    registry
        .manifests()
        .filter(|m| matches!(m.lexeme, Correspondent::Absent(Void::Unnamed(_))))
        .map(|m| m.concept.name.clone())
        .collect()
}

/// The roster registers at least one unnameable concept. Guards the guard:
/// every assertion below is vacuous if this set is empty, and it was empty
/// for the whole life of the project until The Vernacular.
#[test]
fn the_unnameable_set_is_not_empty() {
    let mut registry = ConceptRegistry::default();
    hornvale_worldgen::register_all(&mut registry).expect("the roster registers");
    let names = unnameable(&registry);
    assert!(
        names.len() >= 9,
        "expected the nine spectral classes to be registered unnameable, got {names:?}"
    );
}

/// An unnameable concept is never handed a word by any species' lexicon.
#[test]
fn no_unnameable_concept_is_ever_lexicalized() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap_or_else(|e| panic!("seed 42 builds: {e}"));

    let forbidden = unnameable(&world.registry);
    assert!(!forbidden.is_empty(), "the fixture must not be vacuous");

    let terrain = hornvale_worldgen::terrain_of(&world).expect("terrain builds");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate builds");

    // The five peopled species in the seed-42 fixture (bugbear, gnoll,
    // goblin, hobgoblin, kobold) — checked, not just the brief's original
    // four, so the guard covers every lexicon the fixture actually builds.
    let mut checked = 0usize;
    for species in ["goblin", "hobgoblin", "bugbear", "kobold", "gnoll"] {
        let Ok(lexicon) = hornvale_worldgen::lexicon_from(&world, species, &terrain, &climate)
        else {
            continue;
        };
        for (concept, entry) in lexicon.entries() {
            checked += 1;
            let named = matches!(
                entry,
                hornvale_language::LexEntry::Root { .. }
                    | hornvale_language::LexEntry::Compound { .. }
            );
            assert!(
                !(named && forbidden.iter().any(|f| f == concept)),
                "{species} minted a word for {concept:?}, which is registered \
                 Unnamed — the declaration must bind, not decorate"
            );
        }
    }
    assert!(
        checked > 0,
        "no species produced a lexicon at all — the check ran vacuously"
    );
}
