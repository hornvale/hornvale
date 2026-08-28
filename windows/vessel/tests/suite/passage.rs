//! The Latch — restricted passage. Task 1 is a measurement, not a guard.
//!
//! Discharges the spec's top risk (a time-varying room graph against the
//! pure-function nav caches) by grep, not by argument: `delve` is a session
//! verb (`windows/vessel/src/action.rs` has no `Delve` `Action` variant) and
//! the catch-up path (`windows/vessel/src/liveness.rs`) never mentions
//! `delve`. Both greps returned no matches (2026-08-28), so the gate lands
//! on a mode change into the chamber lattice, never on an edge the NPC
//! catch-up path walks, and the room graph never becomes time-varying.
//!
//! Builds a real seed-42 world through `build_world_to_with_artifacts` (the
//! idiom `windows/vessel/tests/suite/lantern_fabric.rs` uses), the same
//! reason that file gives: `hornvale-vessel` is the shallowest crate that
//! can see both `hornvale-worldgen`'s `barrier_of` and a built
//! `GeneratedTerrain`.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BarrierPins, BarrierState, BuildDepth, SettlementPins, SkyChoice, WorldComponents, barrier_of,
    build_world_to_with_artifacts,
};

/// Task 1's probe: how many cave-bearing vertices in seed 42's terrain carry
/// each barrier state. Prints a census and asserts only that a barred vertex
/// EXISTS — if none does, acceptance criterion 1 is unreachable and the cut
/// must move.
///
/// Measured 2026-08-28 on seed 42's terrain (`BuildDepth::Settlements`,
/// `Band::Undercroft`, branch 0, non-ocean cave-bearing vertices only):
/// sealed=215 warded=209 thin=215 open=235 (639 barred of 874 total). Close
/// to the quarter-per-state prediction the brief made ahead of the run.
#[test]
fn seed_42_places_at_least_one_barred_cave_mouth() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed 42 failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .as_ref()
        .expect("BuildDepth::Settlements produces terrain");
    let pins = BarrierPins::default();

    let mut sealed = 0usize;
    let mut warded = 0usize;
    let mut thin = 0usize;
    let mut open = 0usize;

    for vertex in terrain.geosphere().vertices() {
        if terrain.is_ocean(vertex) {
            continue;
        }
        if terrain.cave_at(vertex).is_none() {
            continue;
        }
        match barrier_of(Seed(42), vertex, Band::Undercroft, 0, &pins) {
            BarrierState::Sealed => sealed += 1,
            BarrierState::Warded => warded += 1,
            BarrierState::Thin => thin += 1,
            BarrierState::Open => open += 1,
        }
    }

    println!(
        "barrier census (seed 42, Undercroft, branch 0): sealed={sealed} warded={warded} thin={thin} open={open}"
    );
    assert!(
        sealed + warded + thin > 0,
        "no barred cave mouth exists in seed 42 — acceptance criterion 1 is \
         unreachable and the campaign's cut must move"
    );
}

/// The address encoding must be injective across every field of
/// `ChamberAddr` — two different addresses must never collide on one key, or
/// clearing one passage would silently clear another.
///
/// MUTATION this must fail against: drop `branch` from `addr_key`'s format
/// string. Both addresses below then produce the same key and the assertion
/// fires.
///
/// Confirmed 2026-08-28: `assertion `left == right` failed: addr_key
/// collided: ["7/Undercroft/0", "8/Undercroft/0", "7/Undercroft/0",
/// "7/Undercroft/1"] left: 3 right: 4` — `base` and `by_branch` collided,
/// as expected once `branch` drops out of the key.
#[test]
fn addr_key_distinguishes_every_field() {
    use hornvale_kernel::{Band, Vertex};
    use hornvale_vessel::passage::addr_key;
    use hornvale_worldgen::chamber::ChamberAddr;

    let base = ChamberAddr {
        vertex: Vertex(7),
        band: Band::Undercroft,
        branch: 0,
        level: 0,
    };
    let by_vertex = ChamberAddr {
        vertex: Vertex(8),
        ..base
    };
    let by_branch = ChamberAddr { branch: 1, ..base };
    let by_level = ChamberAddr { level: 1, ..base };

    let keys = [
        addr_key(&base),
        addr_key(&by_vertex),
        addr_key(&by_branch),
        addr_key(&by_level),
    ];
    let unique: std::collections::BTreeSet<&String> = keys.iter().collect();
    assert_eq!(unique.len(), keys.len(), "addr_key collided: {keys:?}");
}
