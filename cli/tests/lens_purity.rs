//! World-identity guard: the seed-42 default world's JSON is a committed
//! fixture. If this test fails, world identity drifted — that is a terrain
//! or sky epoch and must be deliberate: regenerate the fixture in the same
//! commit and record why in the chronicle. Campaign 25 (The Measured
//! Coast) is contractually lens-only; under it this test must never fail.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world_with_roster, default_roster};

#[test]
fn seed_42_world_json_matches_the_committed_fixture() {
    let world = build_world_with_roster(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
        &default_roster(),
    )
    .expect("seed 42 builds");
    let fixture = include_str!("fixtures/world-seed-42.json");
    assert_eq!(
        world.to_json(),
        fixture,
        "world identity drifted from the committed fixture — see this file's module doc"
    );
}
