//! `build_world_to_with_artifacts` hands back the terrain and climate the
//! build already constructed, so consumers stop re-deriving them. Which
//! artifacts exist is a function of the requested depth: a rung that never
//! built one reports `None` rather than silently rebuilding it.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

fn artifacts_at(depth: BuildDepth) -> hornvale_worldgen::BuildArtifacts {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to_with_artifacts(
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
