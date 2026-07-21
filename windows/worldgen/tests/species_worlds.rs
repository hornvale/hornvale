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
    // The Living Community epoch: the deep-history bake seeds every people its
    // own proto-communities (GENESIS_TOP_CELLS is kept above the total genesis
    // count so no people is starved of sites), so ALL FOUR goblinoid peoples
    // hold their own flagships in the shared default world — fulfilling this
    // test's name. Under the retired demography condensation placer, bugbear
    // and kobold were outcompeted at every attractor and held no flagship;
    // history places by lineage, not local dominance, so each people persists.
    let mut flagship_ids = std::collections::BTreeSet::new();
    for species in ["goblin", "hobgoblin", "bugbear", "kobold"] {
        let flagship = flagship_of(&world, species)
            .unwrap_or_else(|| panic!("{species} must hold at least one settlement"));
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

#[test]
fn minting_validates_the_kind_against_the_union_roster() {
    let wc = hornvale_worldgen::WorldComponents::assemble().unwrap();
    let mut w = hornvale_kernel::World::new(hornvale_kernel::Seed(9));
    // A canonical non-species kind mints fine…
    let d =
        hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "deity", Some(0.0), "test").unwrap();
    assert_eq!(w.ledger.kind_of(d), Some("deity"));
    // …an unknown kind fails loudly with the physical reason.
    let err = hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "tarrasque", None, "test");
    assert!(err.is_err(), "unknown kind must be rejected: {err:?}");
}

#[test]
fn genesis_commits_one_instance_of_fact_per_placed_people() {
    // The ECS Individuation campaign's shadow contract (no shipped world
    // ever minted an instance) held through that campaign and stayed true
    // until C2 T5 deliberately lifted it for placed peopled species: each
    // one now gets exactly one `instance-of` collective (spec §3/§4). The
    // shadow contract otherwise still holds -- no other roster kind
    // (deities, culture, material, awakened) mints an instance yet, so the
    // count here is exactly the placed-people count, not more.
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let placed = hornvale_worldgen::placed_peoples(&w);
    assert_eq!(
        w.ledger.find(hornvale_kernel::INSTANCE_OF).count(),
        placed.len(),
        "one instance-of collective per placed peopled species"
    );
}
