//! Species worlds under the four-people (goblinoid + kobold) roster: the
//! peoples that actually coexist in the unpinned default world place their
//! own flagships, each with its own culture and religion; every registry
//! people places a flagship when given exclusive placement via its own
//! species pin; an unknown species pin fails loudly.
use hornvale_worldgen::{BuildError, SettlementPins, SkyChoice, build_world, flagship_of};

fn pins(species: Option<&str>) -> SettlementPins {
    SettlementPins {
        species: species.map(str::to_string),
        ..SettlementPins::default()
    }
}

/// Since The Branches, the unpinned (default, four-people) roster's shared
/// joint-greedy placement pass is won only by hobgoblin and kobold at seed
/// 42, not by all four peoples: hobgoblin's psychology-derived suitability
/// weights (`hornvale_worldgen::species_weights`, over the authored
/// `hornvale_species::registry` psychology vectors) are at least as
/// favorable as goblin's and bugbear's on every terrain axis —
/// freshwater, coast, and hostility-tolerance — under the current
/// authoring, so hobgoblin weakly Pareto-dominates both at every cell and
/// wins every contested placement; confirmed across many seeds, not a
/// seed-42 coincidence, and not something this test suite can paper over
/// by widening its species list. This test therefore checks the two
/// peoples that actually coexist in today's shared world (goblin+kobold,
/// pre-Branches); `every_registry_people_can_flagship_when_pinned_alone`
/// below is the honest generalization to all four peoples — each CAN
/// carry a flagship, verified the one way that's currently true: given
/// exclusive placement, not shared coexistence.
#[test]
fn default_worlds_carry_their_coexisting_peoples_with_their_own_flagships() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(None),
    )
    .unwrap();
    let hobgoblin = flagship_of(&world, "hobgoblin").expect("hobgoblin flagship");
    let kobold = flagship_of(&world, "kobold").expect("kobold flagship");
    assert_ne!(hobgoblin.id, kobold.id);
    // Both flagships carry committed culture.
    assert!(hornvale_culture::subsistence_of(&world, hobgoblin.id).is_some());
    assert!(hornvale_culture::subsistence_of(&world, kobold.id).is_some());
    let kobold_castes = hornvale_culture::castes_of(&world, kobold.id);
    assert_eq!(kobold_castes.last().map(String::as_str), Some("elders"));
    assert!(!kobold_castes.contains(&"slave".to_string()));
    // Religion runs on every species-flagship (spec §5): both peoples hold
    // their own pantheon.
    let hobgoblin_beliefs = hornvale_religion::beliefs_held_by(&world, hobgoblin.id);
    let kobold_beliefs = hornvale_religion::beliefs_held_by(&world, kobold.id);
    assert!(
        !hobgoblin_beliefs.is_empty(),
        "the hobgoblin flagship must hold its own pantheon"
    );
    assert!(
        !kobold_beliefs.is_empty(),
        "the kobold flagship must hold its own pantheon"
    );
}

/// Every registry people, given EXCLUSIVE placement via its own species
/// pin (no competing psychology-derived suitability score to lose
/// against), places its own flagship with committed culture and its own
/// pantheon. The honest generalization of the old two-species
/// "both peoples flagship" test to today's four peoples: see the doc
/// comment above for why the default (unpinned, shared) world cannot
/// currently make this same claim for goblin or bugbear.
#[test]
fn every_registry_people_can_flagship_when_pinned_alone() {
    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let world = build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &pins(Some(species)),
        )
        .unwrap();
        let flagship = flagship_of(&world, species)
            .unwrap_or_else(|| panic!("{species} should hold its own flagship when pinned alone"));
        assert!(
            hornvale_culture::subsistence_of(&world, flagship.id).is_some(),
            "{species}'s flagship must carry committed culture"
        );
        assert!(
            !hornvale_culture::castes_of(&world, flagship.id).is_empty(),
            "{species}'s flagship must grow at least one caste"
        );
        assert!(
            !hornvale_religion::beliefs_held_by(&world, flagship.id).is_empty(),
            "{species}'s flagship must hold its own pantheon"
        );
    }
}

#[test]
fn species_pin_restricts_and_unknown_species_fail_loudly() {
    let goblin_only = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(Some("goblin")),
    )
    .unwrap();
    assert!(flagship_of(&goblin_only, "kobold").is_none());
    assert!(flagship_of(&goblin_only, "goblin").is_some());

    let err = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(Some("elf")),
    )
    .unwrap_err();
    let BuildError::Pins(msg) = err else {
        panic!("expected a pin error, got {err:?}")
    };
    assert!(msg.contains("elf") && msg.contains("goblin") && msg.contains("kobold"));
}
