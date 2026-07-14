//! `build_world_to(depth)` commits a byte-identical *prefix* of the full
//! build's facts — stopping early never changes a fact it does commit, and a
//! deeper build only ever appends. The whole depth ladder rests on this: a
//! shallow build must be exactly the first N facts a full build commits, in
//! the same order (Global Constraint: byte-identity is the acceptance gate).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, build_world, build_world_to, build_world_with_roster,
    default_roster,
};

fn shallow(depth: BuildDepth) -> hornvale_kernel::World {
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &default_roster(),
        depth,
    )
    .expect("seed 42 builds")
}

fn full() -> hornvale_kernel::World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

/// Assert every fact `shallow` committed is byte-identical to the same-index
/// fact in `deep`, and `shallow` committed strictly fewer. Byte-identity is
/// checked per fact via `serde_json` (the serialization boundary that quantizes
/// floats), not just `PartialEq`, so this is exactly the acceptance gate.
fn assert_prefix(shallow: &hornvale_kernel::World, deep: &hornvale_kernel::World) {
    assert!(
        shallow.ledger.len() < deep.ledger.len(),
        "a shallow build must commit strictly fewer facts ({} !< {})",
        shallow.ledger.len(),
        deep.ledger.len()
    );
    for (i, (s, d)) in shallow.ledger.iter().zip(deep.ledger.iter()).enumerate() {
        let s_json = serde_json::to_string(s).unwrap();
        let d_json = serde_json::to_string(d).unwrap();
        assert_eq!(
            s_json, d_json,
            "fact {i} diverges: shallow build is not a byte-identical prefix"
        );
    }
}

#[test]
fn terrain_depth_is_a_prefix_of_full() {
    assert_prefix(&shallow(BuildDepth::Terrain), &full());
}

#[test]
fn astronomy_depth_is_a_prefix_of_terrain_depth() {
    assert_prefix(
        &shallow(BuildDepth::Astronomy),
        &shallow(BuildDepth::Terrain),
    );
}

#[test]
fn settlements_depth_is_a_prefix_of_full() {
    assert_prefix(&shallow(BuildDepth::Settlements), &full());
}

#[test]
fn the_ladder_is_monotone() {
    // Every rung commits at least as many facts as the one below it, and each
    // is a byte-identical prefix of the next.
    let a = shallow(BuildDepth::Astronomy);
    let t = shallow(BuildDepth::Terrain);
    let s = shallow(BuildDepth::Settlements);
    let f = shallow(BuildDepth::Full);
    assert_prefix(&a, &t);
    assert_prefix(&t, &s);
    assert_prefix(&s, &f);
}

#[test]
fn full_depth_is_byte_identical_to_the_ordinary_full_build() {
    // `build_world_to(.., Full)` must be the ordinary build, byte for byte —
    // the Full path adds no early return, only delegates.
    let via_depth = shallow(BuildDepth::Full);
    let via_roster = build_world_with_roster(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &default_roster(),
    )
    .expect("seed 42 builds");
    assert_eq!(
        serde_json::to_string(&via_depth).unwrap(),
        serde_json::to_string(&via_roster).unwrap(),
        "Full depth diverged from build_world_with_roster"
    );
}
