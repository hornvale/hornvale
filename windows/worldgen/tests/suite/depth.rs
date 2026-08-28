//! `build_world_to(depth)` commits a byte-identical *prefix* of the full
//! build's facts — stopping early never changes a fact it does commit, and a
//! deeper build only ever appends. The whole depth ladder rests on this: a
//! shallow build must be exactly the first N facts a full build commits, in
//! the same order (Global Constraint: byte-identity is the acceptance gate).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world,
    build_world_from_components, build_world_observed, build_world_to,
};

fn shallow(depth: BuildDepth) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
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
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let via_components = build_world_from_components(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 builds");
    assert_eq!(
        serde_json::to_string(&via_depth).unwrap(),
        serde_json::to_string(&via_components).unwrap(),
        "Full depth diverged from build_world_from_components"
    );
}

/// Run an observed build to `depth`, returning the built world and the
/// sequence of rungs the observer saw. Every callback invocation asserts the
/// handed-in `World` is real (non-empty and, after the first callback,
/// strictly larger than the one before) — an observer that fired on an empty
/// or unchanging world would satisfy a bare rung-name count, so this is the
/// guard that keeps the test from being vacuous.
fn observed_run(depth: BuildDepth) -> (hornvale_kernel::World, Vec<BuildDepth>) {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut seen: Vec<BuildDepth> = Vec::new();
    let mut last_len = 0usize;
    let world = build_world_observed(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        depth,
        &mut |rung, world| {
            seen.push(rung);
            assert!(
                !world.ledger.is_empty(),
                "rung {rung:?} handed an empty world"
            );
            assert!(
                world.ledger.len() > last_len,
                "rung {rung:?} did not grow the ledger past the previous rung \
                 ({} !> {last_len})",
                world.ledger.len()
            );
            last_len = world.ledger.len();
        },
    )
    .expect("seed 42 builds");
    (world, seen)
}

#[test]
fn the_observer_fires_once_per_rung_in_ladder_order() {
    // The rungs are a byte-identical prefix chain, so an observer at each
    // boundary sees a REAL world at that depth, not a half-built one. That is
    // what lets a view render honestly at any rung.
    let (_world, seen) = observed_run(BuildDepth::Full);
    assert_eq!(
        seen,
        vec![
            BuildDepth::Astronomy,
            BuildDepth::Terrain,
            BuildDepth::Settlements,
            BuildDepth::Full,
        ],
        "rungs must fire once each, in ladder order"
    );
}

/// Non-vacuity per shallower depth: a request that stops at `Astronomy` must
/// not somehow report deeper rungs, and a request that stops at `Terrain` or
/// `Settlements` must report exactly the prefix of the ladder up to and
/// including its own rung — never the rungs past it. This is exactly the
/// off-by-one the brief's own `Full`-only sketch cannot see: a callback fired
/// unconditionally at the wrong place (e.g. inside the early-return arm
/// instead of before it) would still pass a `Full`-only test while getting a
/// shallower request wrong.
#[test]
fn the_observer_reports_only_the_rungs_a_shallow_request_reaches() {
    let (_world, seen) = observed_run(BuildDepth::Astronomy);
    assert_eq!(seen, vec![BuildDepth::Astronomy]);

    let (_world, seen) = observed_run(BuildDepth::Terrain);
    assert_eq!(seen, vec![BuildDepth::Astronomy, BuildDepth::Terrain]);

    let (_world, seen) = observed_run(BuildDepth::Settlements);
    assert_eq!(
        seen,
        vec![
            BuildDepth::Astronomy,
            BuildDepth::Terrain,
            BuildDepth::Settlements
        ]
    );
}

#[test]
fn an_observed_build_is_byte_identical_to_an_unobserved_one() {
    // The load-bearing one. An observer must be a READ. If observing changes a
    // byte, it has entered the determinism path and the whole design is unsafe.
    let plain = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let observed = build_world_observed(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
        &mut |_, _| {},
    )
    .expect("seed 42 builds");
    assert_eq!(
        plain.to_json(),
        observed.to_json(),
        "observing changed the world's bytes"
    );
}
