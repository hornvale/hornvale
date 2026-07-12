//! Species worlds under the four-people (goblinoid + kobold) roster: the
//! founder floor (settlement's founder-reservation pass, MAP-22
//! allocation-at-K=1) guarantees every placed people its own single best
//! cell before the joint-greedy competitive pass runs, so the unpinned
//! default world's shared placement is won by ALL FOUR peoples at seed
//! 42 -- goblin and kobold each also win many additional cells on their
//! own suitability, hobgoblin and bugbear win exactly the one cell the
//! founder floor reserves for them (verified against actual `peopled-by`
//! counts: goblin 26, kobold 33, hobgoblin 1, bugbear 1, out of 61
//! settlements). Also: every registry people, given exclusive placement
//! via its own species pin, places its own flagship; an unknown species
//! pin fails loudly.
use hornvale_worldgen::{BuildError, SettlementPins, SkyChoice, build_world, flagship_of};

fn pins(species: Option<&str>) -> SettlementPins {
    SettlementPins {
        species: species.map(str::to_string),
        ..SettlementPins::default()
    }
}

/// The unpinned (default, four-people) roster's shared joint-greedy
/// placement pass, now guarded by the founder floor: every one of the
/// four peoples carries its own flagship -- a distinct settlement entity
/// with its own committed culture and its own pantheon.
#[test]
fn default_world_carries_all_four_peoples_with_their_own_flagships() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(None),
    )
    .unwrap();
    let mut flagship_ids = std::collections::BTreeSet::new();
    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let flagship = flagship_of(&world, species).unwrap_or_else(|| {
            panic!("{species} must place its own flagship under the founder floor")
        });
        assert!(
            flagship_ids.insert(flagship.id),
            "{species}'s flagship entity id must be distinct from every other people's"
        );
        assert!(
            hornvale_culture::subsistence_of(&world, flagship.id).is_some(),
            "{species}'s flagship must carry committed culture"
        );
        assert!(
            !hornvale_religion::beliefs_held_by(&world, flagship.id).is_empty(),
            "{species}'s flagship must hold its own pantheon"
        );
    }
    // Kobold's specific caste-ladder shape (top rung "elders", no "slave")
    // -- preserved from the pre-Branches two-species test as a structural
    // regression guard on its role vocabulary.
    let kobold = flagship_of(&world, "kobold").unwrap();
    let kobold_castes = hornvale_culture::castes_of(&world, kobold.id);
    assert_eq!(kobold_castes.last().map(String::as_str), Some("elders"));
    assert!(!kobold_castes.contains(&"slave".to_string()));
}

/// Every registry people, given EXCLUSIVE placement via its own species
/// pin (no competing psychology-derived suitability score to lose
/// against, and no founder floor needed since there is no competition to
/// guard against), places its own flagship with committed culture and
/// its own pantheon. A second, independent path to the same claim the
/// coexistence test above already establishes for the shared default
/// world -- this one isolates each species' own registry entry from the
/// placement dynamics entirely.
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
