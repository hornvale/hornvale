//! Two-species worlds: both peoples place, flagships are per-species, the
//! species pin restricts, and unknown species fail loudly.
use hornvale_kernel::Value;
use hornvale_worldgen::{BuildError, SettlementPins, SkyChoice, build_world, flagship_of};

fn pins(species: Option<&str>) -> SettlementPins {
    SettlementPins {
        species: species.map(str::to_string),
        ..SettlementPins::default()
    }
}

#[test]
fn default_worlds_carry_both_peoples_with_their_own_flagships() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(None),
    )
    .unwrap();
    let goblin = flagship_of(&world, "goblin").expect("goblin flagship");
    let kobold = flagship_of(&world, "kobold").expect("kobold flagship");
    assert_ne!(goblin.id, kobold.id);
    // Both flagships carry committed culture.
    assert!(hornvale_culture::subsistence_of(&world, goblin.id).is_some());
    assert!(hornvale_culture::subsistence_of(&world, kobold.id).is_some());
    let kobold_castes = hornvale_culture::castes_of(&world, kobold.id);
    assert_eq!(kobold_castes.last().map(String::as_str), Some("elders"));
    assert!(!kobold_castes.contains(&"slave".to_string()));
    // Religion runs on the goblin flagship only (spec §6). `beliefs_of` is
    // world-wide, not per-entity (see `domains/religion/src/lib.rs`), so we
    // check the pantheon's `held-by` subject directly instead of asserting
    // on a per-entity accessor that doesn't exist.
    assert!(!hornvale_religion::beliefs_of(&world).is_empty());
    let held_by_kobold = world
        .ledger
        .find(hornvale_religion::HELD_BY)
        .any(|f| f.object == Value::Entity(kobold.id));
    assert!(
        !held_by_kobold,
        "the kobold flagship must hold no beliefs — religion runs on goblin only"
    );
    let held_by_goblin = world
        .ledger
        .find(hornvale_religion::HELD_BY)
        .any(|f| f.object == Value::Entity(goblin.id));
    assert!(
        held_by_goblin,
        "the goblin flagship must hold the world's pantheon"
    );
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
