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

    // The six peopled species in the seed-42 fixture (bugbear, gnoll,
    // goblin, hobgoblin, human, kobold) — checked, not just the brief's
    // original four, so the guard covers every lexicon the fixture actually
    // builds. The Generalist added human as the sixth (Fix round 1,
    // Finding 3); this list rotted once already (the brief's four grew to
    // five without this file noticing) and the campaign-wide fix for that
    // is deriving rather than hand-maintaining wherever the golden's row
    // order does not pin the list — no such constraint here, so this could
    // read `hornvale_worldgen::placed_peoples(&world)` instead, but is kept
    // as an explicit named roster for now to match this test's own existing
    // idiom; the count assertion below is what actually guards against the
    // list rotting silently a third time.
    let species_roster = ["goblin", "hobgoblin", "bugbear", "kobold", "gnoll", "human"];
    let mut checked = 0usize;
    for species in species_roster {
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
    // Every species' lexicon is a TOTAL map over the registered concepts
    // (established by `exposure.rs`'s own total-map guard), so the expected
    // count is derived from the live registry rather than a second
    // hand-maintained number — a species silently dropping out of
    // `species_roster` (or the registry gaining/losing concepts) moves this
    // assertion instead of leaving `checked > 0` to pass regardless.
    assert_eq!(
        checked,
        species_roster.len() * world.registry.concepts().count(),
        "expected every one of the {} listed species to contribute one entry per \
         registered concept ({} concepts) — a species was skipped or its lexicon \
         was not total, so this guard covered less than it claims",
        species_roster.len(),
        world.registry.concepts().count()
    );
}

/// `ConceptRegistry::manifests` is `#[serde(skip)]` (kernel/src/registry.rs)
/// — a saved world's JSON never carries the lexeme/percept/cognition edges,
/// only the concept anchors. A naive reload therefore comes back with an
/// EMPTY manifests map, and both of the exposure classifier's exclusion
/// sites (`windows/worldgen`'s FINAL/UNCONDITIONAL Unnameable overwrite,
/// `cli/src/proto.rs`'s `is_unnameable`) read exactly that map — so a
/// reloaded world silently fell back every `Unnameable` gap to
/// `Experiential`, and `hornvale dictionary --world w.json` disagreed with
/// an in-process render of the same seed. `cli`'s `load_world` fixes this by
/// re-running `register_all` into the freshly loaded registry (a no-op over
/// the concept/predicate/phenomenon-kind maps, since `register_all` is a
/// pure function of static domain code and idempotent on identical
/// redefinition — it only ever repopulates the missing in-memory
/// manifests). This test proves the round trip preserves the classification
/// without depending on the CLI binary (which has no `[lib]` target, so
/// `cli/tests/` cannot call `load_world` directly): it replicates the exact
/// save → `World::load` → `register_all` sequence and asserts the
/// classification survives unchanged.
#[test]
fn the_unnameable_classification_survives_a_save_load_round_trip() {
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
    let before = hornvale_worldgen::exposure_from(&world, "goblin", &terrain, &climate)
        .expect("exposure classifies in-process");
    for name in &forbidden {
        assert!(
            matches!(
                before.get(name),
                Some(hornvale_language::ExposureClass::Unknown {
                    reason: hornvale_language::GapReason::Unnameable(_)
                })
            ),
            "{name}: in-process classification must already be Unnameable (fixture sanity)"
        );
    }

    // Round-trip through the exact save format `hornvale dictionary --world
    // <PATH>` and `hornvale repl --world <PATH>` read.
    let path = std::env::temp_dir().join(format!(
        "the-unnameable-round-trip-{}-{:?}.json",
        std::process::id(),
        std::thread::current().id()
    ));
    world.save(&path).expect("world saves");
    let mut reloaded = hornvale_kernel::World::load(&path).expect("world reloads");
    std::fs::remove_file(&path).ok();

    // The fix's seam (`cli::load_world`): re-run `register_all` into the
    // freshly loaded registry, since `manifests` is `#[serde(skip)]` and
    // comes back empty otherwise.
    hornvale_worldgen::register_all(&mut reloaded.registry)
        .expect("register_all is idempotent over an already-populated registry");

    let after_forbidden = unnameable(&reloaded.registry);
    assert_eq!(
        after_forbidden, forbidden,
        "the unnameable set must survive a save/load round trip unchanged"
    );

    let reloaded_terrain = hornvale_worldgen::terrain_of(&reloaded).expect("terrain rebuilds");
    let reloaded_climate = hornvale_worldgen::climate_of(&reloaded).expect("climate rebuilds");
    let after =
        hornvale_worldgen::exposure_from(&reloaded, "goblin", &reloaded_terrain, &reloaded_climate)
            .expect("exposure classifies after reload");
    for name in &forbidden {
        assert!(
            matches!(
                after.get(name),
                Some(hornvale_language::ExposureClass::Unknown {
                    reason: hornvale_language::GapReason::Unnameable(_)
                })
            ),
            "{name}: reloaded classification must still be Unnameable, not fall back to Experiential"
        );
    }
}
