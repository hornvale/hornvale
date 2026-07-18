//! Genesis commits NO agent-at fact (spec §3): the world moves only in a
//! possess session. If this reddens, liveness leaked into world build — a
//! save-format event, not a bug fix.
#[test]
fn a_genesis_world_has_no_agent_at_facts() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();
    assert_eq!(
        world
            .ledger
            .find(hornvale_vessel::liveness::AGENT_AT)
            .count(),
        0
    );
}

/// Genesis commits NO `drank` fact either (The Foresight): drinking is a
/// planned action inside a possess session, never something world build
/// commits (same save-format contract as `agent-at` above).
#[test]
fn a_genesis_world_has_no_drank_facts() {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();
    assert_eq!(
        world.ledger.find(hornvale_vessel::liveness::DRANK).count(),
        0
    );
}
