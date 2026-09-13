//! `build_world_to_with_artifacts` hands back the terrain and climate the
//! build already constructed, so consumers stop re-deriving them. Which
//! artifacts exist is a function of the requested depth: a rung that never
//! built one reports `None` rather than silently rebuilding it.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, VertexMap};
use hornvale_terrain::{Metaphysics, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, build_world_to_with_artifacts,
    terrain_of,
};

fn artifacts_at(depth: BuildDepth) -> hornvale_worldgen::BuildArtifacts {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        depth,
    )
    .expect("seed 42 builds")
}

#[test]
fn astronomy_depth_produces_no_terrain_and_no_climate() {
    let a = artifacts_at(BuildDepth::Astronomy);
    assert!(a.terrain.is_none(), "astronomy rung sculpts no terrain");
    assert!(a.climate.is_none(), "astronomy rung builds no climate");
}

#[test]
fn terrain_depth_produces_terrain_but_no_climate() {
    let a = artifacts_at(BuildDepth::Terrain);
    assert!(a.terrain.is_some(), "terrain rung sculpts terrain");
    assert!(a.climate.is_none(), "terrain rung builds no climate");
}

#[test]
fn full_depth_produces_both() {
    let a = artifacts_at(BuildDepth::Full);
    assert!(a.terrain.is_some(), "full build sculpts terrain");
    assert!(a.climate.is_some(), "full build builds climate");
}

/// Project a terrain onto the per-vertex fields metrics actually read, so two
/// terrains can be compared without `PartialEq` on the provider itself
/// (`GeneratedTerrain` derives only `Debug, Clone`). `VertexMap` DOES derive
/// `PartialEq`, which is what makes this comparison exact rather than
/// approximate.
fn projection(
    t: &hornvale_terrain::GeneratedTerrain,
) -> (
    VertexMap<hornvale_kernel::ReferenceElevation>,
    VertexMap<bool>,
) {
    let geo = t.geosphere();
    (
        VertexMap::from_fn(geo, |c| t.elevation_at(c)),
        VertexMap::from_fn(geo, |c| t.is_ocean(c)),
    )
}

/// The campaign's core claim: the terrain the build hands back is the same
/// terrain `terrain_of` would re-sculpt from the committed ledger. If this
/// ever fails, the hoist is changing physics and must not ship.
#[test]
fn hoisted_terrain_equals_the_re_derived_terrain() {
    let a = artifacts_at(BuildDepth::Full);
    let hoisted = a.terrain.as_ref().expect("full build sculpts terrain");
    let rederived = terrain_of(&a.world).expect("terrain re-derives from the ledger");
    assert!(
        projection(hoisted) == projection(&rederived),
        "hoisted terrain diverged from terrain_of — the hoist is not byte-identical"
    );
}

/// The same claim under NON-DEFAULT pins. The default path cannot diverge
/// (default pins commit no facts at all, so `terrain_of` reconstructs the
/// same defaults), which makes this the case that would actually catch a
/// lossy `pin_strings`/`parse_pin` round trip.
#[test]
fn hoisted_terrain_equals_the_re_derived_terrain_under_pins() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let pins = TerrainPins {
        plates: Some(12),
        ocean_fraction: Some(0.65),
        supercontinent: Some(true),
        globe_level: None,
        continents: Some(5),
        // The metaphysics gate (The Ground §8) is pinned here so a new field
        // cannot silently default, but THIS TEST CANNOT CATCH A LOSSY
        // METAPHYSICS ROUND TRIP and an earlier version of this comment
        // claimed it could. `projection` above compares `elevation_at` and
        // `is_ocean` only, and the gate moves neither: measured charged-vs-
        // inert at seed 42 level 6, `thaumic` moves 12,512 of 40,962 vertices
        // while elevation, is_ocean, rock and every `MaterialBuffer` axis move
        // ZERO. So a charged world re-deriving as inert is invisible here by
        // construction.
        //
        // Demonstrated rather than argued (Task 8): with `metaphysics` dropped
        // from `pin_strings`, all six `artifacts::` tests PASS while
        // `metaphysics_gate::a_charged_world_re_derives_from_its_ledger_as_
        // charged` fails at 12,512 vertices. That test is where the round-trip
        // claim actually lives; this one keeps the pin exhaustive.
        metaphysics: Some(Metaphysics::Thaumic),
    };
    let a = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        &pins,
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds under these pins");
    let hoisted = a.terrain.as_ref().expect("full build sculpts terrain");
    let rederived = terrain_of(&a.world).expect("terrain re-derives from the ledger");
    assert!(
        projection(hoisted) == projection(&rederived),
        "hoisted terrain diverged under pins — check the pin_strings/parse_pin round trip"
    );
}

/// `build_world_to` must still return exactly the world it always did: the
/// artifacts entry point is additive, not a behaviour change.
#[test]
fn the_wrapper_world_is_unchanged() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let via_wrapper = build_world_to(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds");
    let via_artifacts = artifacts_at(BuildDepth::Full).world;
    assert_eq!(via_wrapper.ledger.len(), via_artifacts.ledger.len());
    for (i, (a, b)) in via_wrapper
        .ledger
        .iter()
        .zip(via_artifacts.ledger.iter())
        .enumerate()
    {
        assert_eq!(a, b, "fact {i} diverged between the two entry points");
    }
}
