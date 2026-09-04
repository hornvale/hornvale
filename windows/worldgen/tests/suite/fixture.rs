//! `seed_42_world()` must equal a live build, or every test that reads it is
//! asserting against fiction.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, build_world, seed_42_world};

/// The load and the build agree byte for byte, in both directions of the
/// serialization boundary.
#[test]
fn the_fixture_equals_a_live_build() {
    let built = build_world(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let loaded = seed_42_world();

    assert_eq!(
        loaded.seed, built.seed,
        "the fixture is a different seed than the build"
    );
    assert_eq!(
        loaded.ledger.len(),
        built.ledger.len(),
        "the fixture holds {} facts, a live build produces {}",
        loaded.ledger.len(),
        built.ledger.len()
    );
    // The whole contract, in one line: identical serialized bytes.
    assert_eq!(
        loaded.to_json(),
        built.to_json(),
        "the fixture has drifted from a live seed-42 build -- regenerate it \
         with `make rebaseline-goldens` ONLY after confirming the world was \
         meant to move, then treat that as an epoch event"
    );
}

/// Anti-vacuity: the assertion above would pass if both sides were empty.
#[test]
fn the_fixture_is_a_real_world() {
    let w = seed_42_world();
    assert!(!w.ledger.is_empty(), "the fixture's ledger is empty");
    assert!(
        w.ledger.len() > 20_000,
        "expected ~21,635 facts in the seed-42 world; got {}",
        w.ledger.len()
    );
}
