//! Stage 1 properties for the additive Skyworld overlay.
#![allow(clippy::disallowed_methods)] // named construction site for the test fixture

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_climate::{Biome, BiomeExpr, GeneratedClimate};
use hornvale_kernel::{Seed, Vertex, World};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyEcology, SkyWorld, SkyWorldConfig, WorldComponents,
    build_world_to_with_artifacts, climate_from, skyworld_from,
};

struct Fixture {
    world: World,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
}

fn fixture(seed: u64) -> Fixture {
    let components = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &components,
        BuildDepth::Terrain,
    )
    .expect("the terrain-depth fixture builds");
    let terrain = built.terrain.expect("terrain depth returns terrain");
    let climate = climate_from(&built.world, &terrain).expect("climate derives from built terrain");
    Fixture {
        world: built.world,
        terrain,
        climate,
    }
}

fn config() -> SkyWorldConfig {
    SkyWorldConfig {
        max_projected_fraction: 0.10,
        trajectory_samples: 8,
        propagation_radius: 2,
    }
}

fn generate(fixture: &Fixture) -> SkyWorld {
    skyworld_from(&fixture.world, &fixture.terrain, &fixture.climate, config())
}

fn projected(skyworld: &SkyWorld) -> BTreeSet<Vertex> {
    skyworld
        .territories
        .iter()
        .flat_map(|territory| territory.physical.projected.iter().copied())
        .collect()
}

#[test]
fn the_activation_fixture_contains_land_and_ocean_candidates() {
    let fixture = fixture(42);
    let mut land = 0usize;
    let mut ocean = 0usize;
    for vertex in fixture.terrain.geosphere().vertices() {
        if fixture.terrain.is_ocean(vertex) {
            ocean += 1;
        } else {
            land += 1;
        }
    }
    assert!(land > 0, "VACUOUS: the fixture exposes no land candidates");
    assert!(
        ocean > 0,
        "VACUOUS: the fixture exposes no ocean candidates"
    );
}

#[test]
fn the_same_built_world_produces_byte_stable_sky_data() {
    let fixture = fixture(42);
    let first = generate(&fixture);
    let second = generate(&fixture);

    assert_eq!(first, second, "the same inputs produced different overlays");
    assert_eq!(
        format!("{first:#?}").as_bytes(),
        format!("{second:#?}").as_bytes(),
        "stable values were emitted in an unstable order"
    );
}

#[test]
fn different_world_seeds_change_the_generated_overlay() {
    let first = generate(&fixture(42));
    let second = generate(&fixture(43));

    assert_ne!(
        first, second,
        "two distinct world identities produced one overlay"
    );
    assert_ne!(
        first.fields, second.fields,
        "two distinct worlds produced one atmospheric profile"
    );
}

#[test]
fn projected_coverage_is_nonempty_and_never_exceeds_ten_percent() {
    let fixture = fixture(42);
    let covered = projected(&generate(&fixture));
    let vertex_count = fixture.terrain.geosphere().vertex_count();
    let hard_ceiling = ((vertex_count as f64) * 0.10).floor() as usize;

    assert!(
        !covered.is_empty(),
        "VACUOUS: the overlay covers no vertices"
    );
    assert!(
        covered.len() <= hard_ceiling,
        "{} projected vertices exceed the hard ceiling of {hard_ceiling}",
        covered.len()
    );
}

#[test]
fn coverage_projects_over_both_land_and_ocean() {
    let fixture = fixture(42);
    let covered = projected(&generate(&fixture));
    assert!(
        !covered.is_empty(),
        "VACUOUS: the overlay covers no vertices"
    );

    assert!(
        covered
            .iter()
            .any(|&vertex| !fixture.terrain.is_ocean(vertex)),
        "coverage contains no land projection"
    );
    assert!(
        covered
            .iter()
            .any(|&vertex| fixture.terrain.is_ocean(vertex)),
        "coverage contains no ocean projection"
    );
}

#[test]
fn clustered_and_isolated_territories_are_both_represented() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);

    assert!(
        skyworld
            .territories
            .iter()
            .any(|territory| territory.physical.projected.len() > 1),
        "the overlay contains no clustered territory"
    );
    assert!(
        skyworld
            .territories
            .iter()
            .any(|territory| territory.physical.projected.len() == 1),
        "the overlay contains no isolated territory"
    );
}

#[test]
fn generating_the_overlay_leaves_surface_biomes_intact() {
    let fixture = fixture(42);
    let before: Vec<(Biome, BiomeExpr)> = fixture
        .terrain
        .geosphere()
        .vertices()
        .map(|vertex| {
            (
                fixture.climate.biome_at(vertex),
                fixture.climate.biome_expr_at(vertex),
            )
        })
        .collect();

    let _ = generate(&fixture);

    let after: Vec<(Biome, BiomeExpr)> = fixture
        .terrain
        .geosphere()
        .vertices()
        .map(|vertex| {
            (
                fixture.climate.biome_at(vertex),
                fixture.climate.biome_expr_at(vertex),
            )
        })
        .collect();
    assert_eq!(
        before, after,
        "Skyworld replaced or mutated the surface layer"
    );
}

#[test]
fn altitude_profiles_are_stable_and_filter_radiation_before_moisture() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let low = skyworld.fields.at_altitude(1_000.0);
    let high = skyworld.fields.at_altitude(12_000.0);

    assert_eq!(low, skyworld.fields.at_altitude(1_000.0));
    assert!(
        low.moisture > 0.0,
        "the lower atmosphere retained no moisture"
    );
    assert!(
        low.high_sky_radiation < high.high_sky_radiation,
        "the lower atmosphere did not filter high-sky radiation"
    );
}

#[test]
fn aether_and_radiation_remain_independent_altitude_inputs() {
    let fixture = fixture(42);
    let mut low_aether = generate(&fixture).fields;
    let mut high_aether = low_aether;
    low_aether.aether = 0.1;
    high_aether.aether = 0.9;

    let low = low_aether.at_altitude(8_000.0);
    let high = high_aether.at_altitude(8_000.0);
    assert_ne!(low.aether, high.aether, "the aether input was ignored");
    assert_eq!(
        low.high_sky_radiation, high.high_sky_radiation,
        "radiation was inferred from aether"
    );
}

#[test]
fn stage_one_seeds_an_orchard_chain_without_deriving_later_behaviors() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let orchard = skyworld
        .territories
        .iter()
        .find(|territory| territory.phenotype.ecology == SkyEcology::OrchardBearing)
        .expect("the first slice contains its canonical mature orchard");

    assert!(orchard.stocks.plankton > 0.0);
    assert!(orchard.stocks.root_support > 0.0);
    assert!(orchard.stocks.soil_fertility > 0.0);
    assert!(orchard.stocks.canopy_biomass > 0.0);
    assert!(orchard.stocks.pollination > 0.0);
    assert!(orchard.stocks.fruit > 0.0);
    assert!(
        orchard.trajectory.is_empty(),
        "Stage 1 must not derive orchard trajectories"
    );
    assert!(
        orchard.influence.local.is_empty()
            && orchard.influence.corridors.is_empty()
            && orchard.influence.events.is_empty(),
        "Stage 1 must not derive propagation"
    );
    assert_eq!(
        orchard.exchange, orchard.physical,
        "before exchange derivation, the exchange envelope begins at the body"
    );
}
