//! Species worlds under the four-people (goblinoid + kobold) roster.
//!
//! Task A15a cut settlement genesis over onto the coexistence stack's
//! niche-differentiated K: ONE settlement per attractor, peopled by
//! whichever species locally DOMINATES it (`StackSettlement::dominant`),
//! with every other present species readable off that settlement's
//! `composition`. This replaced the old per-species `founder::condense_tagged`
//! path, where each species condensed its OWN K field independently and so
//! was guaranteed at least one settlement regardless of any other species'
//! strength there. Under the new stack-wide condensation, a species is
//! floored into its own founder settlement only if it is absent (zero
//! density) from EVERY settlement's composition; a species that is merely
//! outcompeted everywhere -- present in every settlement's composition but
//! never the locally densest -- gets no `peopled-by` fact and so no
//! flagship at all. At seed 42, under the frozen `BETA`/`FLOOR`, goblin and
//! hobgoblin win every attractor's dominance (29 and 37 of the world's 66
//! settlements respectively); bugbear and kobold never dominate a single
//! cell, so neither places a flagship, even though the coexistence stack
//! still packs nonzero density for both of them almost everywhere. Also:
//! every registry people, given exclusive placement via its own species
//! pin (no competing dominance to lose), places its own flagship; an
//! unknown species pin fails loudly.
use hornvale_worldgen::{BuildError, SettlementPins, SkyChoice, build_world, flagship_of};

fn pins(species: Option<&str>) -> SettlementPins {
    SettlementPins {
        species: species.map(str::to_string),
    }
}

/// The unpinned (default, four-people) roster's shared joint-greedy
/// placement pass: goblin and hobgoblin each win a share of the world's
/// attractors outright (dominance-winners, so each carries its own
/// flagship with committed culture and its own pantheon); bugbear and
/// kobold are outcompeted at every single attractor and so hold no
/// flagship of their own at this seed -- the coexistence stack's
/// composition-not-guaranteed-dominance semantics (see module docs).
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
    for species in ["goblin", "hobgoblin"] {
        let flagship = flagship_of(&world, species)
            .unwrap_or_else(|| panic!("{species} must dominate at least one attractor"));
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
    // bugbear and kobold place NO flagship in the shared default world at
    // this seed: both are present (nonzero density) in the coexistence
    // stack almost everywhere, but neither is ever the LOCAL dominant, so
    // neither triggers a `peopled-by` fact. This is the intended drift of
    // the niche cutover, not an omission -- contrast with
    // `every_registry_people_can_flagship_when_pinned_alone` below, where
    // each of these same two peoples DOES flagship once placed without
    // competing dominance.
    for species in ["bugbear", "kobold"] {
        assert!(
            flagship_of(&world, species).is_none(),
            "{species} is outcompeted at every attractor in the shared default world \
             and so should hold no flagship"
        );
    }
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
        if species == "kobold" {
            // Kobold's specific caste-ladder shape (top rung "elders", no
            // "slave") -- preserved from the pre-Branches two-species test
            // as a structural regression guard on its role vocabulary.
            // Moved here (off the shared default world) by the niche
            // cutover: kobold no longer flagships in the unpinned default
            // world at seed 42 (it is outcompeted everywhere), but it does
            // flagship here, pinned alone with no competing dominance.
            let kobold_castes = hornvale_culture::castes_of(&world, flagship.id);
            assert_eq!(kobold_castes.last().map(String::as_str), Some("elders"));
            assert!(!kobold_castes.contains(&"slave".to_string()));
        }
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
