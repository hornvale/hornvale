//! Stage 1 properties for the additive Skyworld overlay.
#![allow(clippy::disallowed_methods)] // named construction site for the test fixture

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_climate::{Biome, BiomeExpr, GeneratedClimate};
use hornvale_kernel::{NearestVertexIndex, Seed, Vertex, World};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyCorridorKind, SkyEcology, SkyEventKind, SkyFields,
    SkyPropagationDetail, SkyStocks, SkyWorld, SkyWorldConfig, SkyWorldDetail, WorldComponents,
    build_world_to_with_artifacts, climate_from, propagation_at,
    render_skyworld_diagnostic_readout, render_skyworld_png, render_skyworld_readout,
    skyworld_from, trajectory_at,
};

struct Fixture {
    world: World,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
}

fn fixture(seed: u64) -> Fixture {
    fixture_with_terrain_pins(seed, TerrainPins::default())
}

fn fixture_with_terrain_pins(seed: u64, terrain_pins: TerrainPins) -> Fixture {
    let components = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        &terrain_pins,
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

fn generate_with(fixture: &Fixture, config: SkyWorldConfig) -> SkyWorld {
    skyworld_from(&fixture.world, &fixture.terrain, &fixture.climate, config)
}

mod seams {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    struct SurfaceSample {
        is_ocean: bool,
        elevation_m: f64,
        mean_temperature_c: f64,
        moisture: f64,
        storm_propensity: f64,
        current: [f64; 3],
        prevailing_wind: [f64; 3],
        biome_expr: BiomeExpr,
        unrest: f64,
        has_boundary: bool,
        tectonic_feature_count: usize,
    }

    #[derive(Clone, Debug, PartialEq)]
    struct SkyworldSample {
        fields: SkyFields,
        territory_count: usize,
        projected: BTreeSet<Vertex>,
    }

    fn sample_surface(fixture: &Fixture, vertex: Vertex) -> SurfaceSample {
        let tectonic_feature_count = fixture
            .terrain
            .features()
            .all()
            .filter(|feature| feature.extent.contains(&vertex))
            .count();
        let prevailing_wind = fixture.climate.band_count().map_or([0.0; 3], |bands| {
            hornvale_climate::prevailing_wind(fixture.terrain.geosphere(), vertex, bands)
        });
        SurfaceSample {
            is_ocean: fixture.terrain.is_ocean(vertex),
            elevation_m: fixture.terrain.elevation_at(vertex).get(),
            mean_temperature_c: fixture.climate.mean_temperature_at(vertex).get(),
            moisture: fixture.climate.moisture_at(vertex),
            storm_propensity: fixture.climate.storm_propensity_at(vertex),
            current: fixture.climate.current_at(vertex),
            prevailing_wind,
            biome_expr: fixture.climate.biome_expr_at(vertex),
            unrest: fixture.terrain.unrest_at(vertex),
            has_boundary: fixture.terrain.boundary_at(vertex).is_some(),
            tectonic_feature_count,
        }
    }

    fn sample_skyworld(fixture: &Fixture) -> SkyworldSample {
        let skyworld = generate(fixture);
        SkyworldSample {
            fields: skyworld.fields,
            territory_count: skyworld.territories.len(),
            projected: projected(&skyworld),
        }
    }

    fn ascending_vertices(fixture: &Fixture) -> Vec<Vertex> {
        let mut vertices: Vec<Vertex> = fixture.terrain.geosphere().vertices().collect();
        vertices.sort_unstable();
        vertices
    }

    fn changed_surface_vertex(
        before: &Fixture,
        after: &Fixture,
    ) -> (Vertex, SurfaceSample, SurfaceSample) {
        ascending_vertices(before)
            .into_iter()
            .zip(ascending_vertices(after))
            .map(|(vertex, other)| {
                assert_eq!(vertex, other, "the pin changed the surface index space");
                (
                    vertex,
                    sample_surface(before, vertex),
                    sample_surface(after, vertex),
                )
            })
            .find(|(_, before, after)| before != after)
            .expect("VACUOUS: terrain pin did not change any sampled surface source")
    }

    #[test]
    fn surface_axis_is_a_read_only_substrate() {
        let fixture = fixture(42);
        let skyworld = sample_skyworld(&fixture);
        let vertices = ascending_vertices(&fixture);
        let land = vertices
            .iter()
            .copied()
            .find(|&vertex| !fixture.terrain.is_ocean(vertex))
            .expect("VACUOUS: fixture has no land vertex");
        let ocean = vertices
            .iter()
            .copied()
            .find(|&vertex| fixture.terrain.is_ocean(vertex))
            .expect("VACUOUS: fixture has no ocean vertex");

        for vertex in [land, ocean] {
            let surface = sample_surface(&fixture, vertex);
            assert_eq!(surface.is_ocean, fixture.terrain.is_ocean(vertex));
            assert_eq!(surface.biome_expr, fixture.climate.biome_expr_at(vertex));
            assert!(
                surface.tectonic_feature_count <= fixture.terrain.features().all().count(),
                "surface feature sampling invented a terrain feature"
            );
        }
        assert_ne!(land, ocean, "VACUOUS: land and ocean selected one vertex");
        assert!(
            !skyworld.projected.is_empty(),
            "VACUOUS: no Skyworld territory records the sampled substrate"
        );
    }

    #[test]
    fn environment_axes_have_non_vacuous_sources() {
        let sparse = fixture_with_terrain_pins(
            42,
            TerrainPins {
                plates: Some(2),
                ..TerrainPins::default()
            },
        );
        let active = fixture_with_terrain_pins(
            42,
            TerrainPins {
                plates: Some(64),
                ..TerrainPins::default()
            },
        );

        let (vertex, before, after) = changed_surface_vertex(&sparse, &active);
        assert_ne!(
            before, after,
            "VACUOUS: intended source perturbation did not change at {vertex:?}"
        );
        assert!(
            before.unrest != after.unrest
                || before.has_boundary != after.has_boundary
                || before.tectonic_feature_count != after.tectonic_feature_count,
            "the plate-count perturbation changed only non-tectonic source axes"
        );
    }

    #[test]
    fn skyworld_outputs_change_only_through_dependent_axes() {
        let sparse = fixture_with_terrain_pins(
            42,
            TerrainPins {
                plates: Some(2),
                ..TerrainPins::default()
            },
        );
        let active = fixture_with_terrain_pins(
            42,
            TerrainPins {
                plates: Some(64),
                ..TerrainPins::default()
            },
        );
        let (vertex, before, after) = changed_surface_vertex(&sparse, &active);
        assert_ne!(
            before, after,
            "VACUOUS: intended source perturbation did not change at {vertex:?}"
        );

        let sparse_skyworld = sample_skyworld(&sparse);
        let active_skyworld = sample_skyworld(&active);
        assert_ne!(
            sparse_skyworld, active_skyworld,
            "Skyworld ignored the changed terrain/climate substrate"
        );
        assert_eq!(
            sparse_skyworld.fields.pressure, active_skyworld.fields.pressure,
            "a terrain/climate perturbation rewrote the world-seeded pressure axis"
        );
        assert_eq!(
            sparse_skyworld.fields.aether, active_skyworld.fields.aether,
            "a terrain/climate perturbation rewrote the world-seeded aether axis"
        );
        assert_eq!(
            sparse_skyworld.fields.lunar_forcing, active_skyworld.fields.lunar_forcing,
            "a terrain/climate perturbation rewrote astronomical forcing"
        );
    }
}

fn strictly_ordered(vertices: &[Vertex]) -> bool {
    vertices.windows(2).all(|pair| pair[0] < pair[1])
}

fn projected(skyworld: &SkyWorld) -> BTreeSet<Vertex> {
    skyworld
        .territories
        .iter()
        .flat_map(|territory| territory.physical.projected.iter().copied())
        .collect()
}

fn png_rgb(png: &[u8]) -> Vec<u8> {
    let width = u32::from_be_bytes(png[16..20].try_into().unwrap()) as usize;
    let height = u32::from_be_bytes(png[20..24].try_into().unwrap()) as usize;
    let mut idat = Vec::new();
    let mut chunk = 8;
    while chunk < png.len() {
        let len = u32::from_be_bytes(png[chunk..chunk + 4].try_into().unwrap()) as usize;
        if &png[chunk + 4..chunk + 8] == b"IDAT" {
            idat.extend_from_slice(&png[chunk + 8..chunk + 8 + len]);
        }
        chunk += len + 12;
    }

    assert_eq!(
        &idat[..2],
        &[0x78, 0x01],
        "renderer stopped using stored deflate"
    );
    let mut raw = Vec::new();
    let mut cursor = 2;
    loop {
        let final_block = idat[cursor] == 1;
        let len = u16::from_le_bytes([idat[cursor + 1], idat[cursor + 2]]) as usize;
        raw.extend_from_slice(&idat[cursor + 5..cursor + 5 + len]);
        cursor += len + 5;
        if final_block {
            break;
        }
    }

    let mut rgb = Vec::with_capacity(width * height * 3);
    for row in raw.chunks_exact(width * 3 + 1) {
        assert_eq!(row[0], 0, "renderer introduced a filtered scanline");
        rgb.extend_from_slice(&row[1..]);
    }
    assert_eq!(rgb.len(), width * height * 3);
    rgb
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
fn zero_one_and_two_vertex_ceilings_are_never_exceeded() {
    let fixture = fixture(42);
    let vertex_count = fixture.terrain.geosphere().vertex_count();

    for ceiling in 0..=2 {
        let max_projected_fraction = if ceiling == 0 {
            0.0
        } else {
            (ceiling as f64 + 0.25) / vertex_count as f64
        };
        assert_eq!(
            (vertex_count as f64 * max_projected_fraction).floor() as usize,
            ceiling,
            "test setup did not realize its intended ceiling"
        );
        let skyworld = skyworld_from(
            &fixture.world,
            &fixture.terrain,
            &fixture.climate,
            SkyWorldConfig {
                max_projected_fraction,
                ..config()
            },
        );
        let covered = projected(&skyworld);

        assert_eq!(
            covered.len(),
            ceiling,
            "a configured ceiling of {ceiling} realized {} vertices",
            covered.len()
        );
    }
}

#[test]
fn coverage_target_reads_environmental_volcanism() {
    let sparse = fixture_with_terrain_pins(
        42,
        TerrainPins {
            plates: Some(2),
            ..TerrainPins::default()
        },
    );
    let active = fixture_with_terrain_pins(
        42,
        TerrainPins {
            plates: Some(64),
            ..TerrainPins::default()
        },
    );
    let volcanic_count = |fixture: &Fixture| {
        fixture
            .terrain
            .geosphere()
            .vertices()
            .filter(|&vertex| hornvale_worldgen::has_edifice(&fixture.terrain, vertex))
            .count()
    };
    let sparse_volcanism = volcanic_count(&sparse);
    let active_volcanism = volcanic_count(&active);
    assert_ne!(
        sparse_volcanism, active_volcanism,
        "VACUOUS: the conditioning fixtures carry equal volcanic signals"
    );

    let sparse_coverage = projected(&generate(&sparse)).len();
    let active_coverage = projected(&generate(&active)).len();
    assert_ne!(
        sparse_coverage, active_coverage,
        "coverage ignored distinct environmental volcanism ({sparse_volcanism} versus \
         {active_volcanism} edifice vertices) under the same seed"
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
fn altitude_sampling_is_independent_of_intermediate_samples() {
    let fixture = fixture(42);
    let fields = generate(&fixture).fields;
    let direct = fields.at_altitude(9_137.0);
    let chained = fields.at_altitude(2_731.0).at_altitude(9_137.0);
    let close = |left: f64, right: f64| {
        let scale = left.abs().max(right.abs()).max(1.0);
        (left - right).abs() <= scale * 1.0e-12
    };

    assert_eq!(direct.altitude_m, chained.altitude_m);
    assert!(close(direct.pressure, chained.pressure));
    assert!(close(direct.density, chained.density));
    assert!(close(direct.temperature_c, chained.temperature_c));
    assert!(close(direct.high_sky_radiation, chained.high_sky_radiation));
    assert!(close(direct.aether, chained.aether));
    assert!(close(direct.moisture, chained.moisture));
    for (direct, chained) in direct.wind.into_iter().zip(chained.wind) {
        assert!(
            close(direct, chained),
            "chained wind {chained} differs from direct wind {direct}"
        );
    }
    assert!(
        close(direct.wind_shear, chained.wind_shear),
        "chained shear {} differs from direct shear {}",
        chained.wind_shear,
        direct.wind_shear
    );
    assert_eq!(direct.lapse_rate_c_per_km, chained.lapse_rate_c_per_km);
    assert_eq!(direct.lunar_forcing, chained.lunar_forcing);
    assert_eq!(direct.stellar_forcing, chained.stellar_forcing);
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
fn orchard_territories_derive_task_two_movement_and_influence() {
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
        !orchard.trajectory.is_empty(),
        "Task 2 must derive the requested orchard trajectory samples"
    );
    assert!(
        !orchard.influence.local.is_empty()
            && !orchard.influence.corridors.is_empty()
            && !orchard.influence.events.is_empty(),
        "Task 2 must keep local, corridor, and event propagation explicit"
    );
    assert_ne!(
        orchard.exchange, orchard.physical,
        "Task 2 must separate exchange from the physical body"
    );
}

/// Removing any one ambient `Field` prerequisite must break the plankton
/// productivity link instead of leaving an unexplained fruit stock.
#[test]
fn orchard_productivity_requires_aether_radiation_and_moisture_independently() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let orchard = skyworld
        .territories
        .iter()
        .find(|territory| territory.phenotype.ecology == SkyEcology::OrchardBearing)
        .expect("the fixture contains its canonical orchard");
    let fields = skyworld.fields.at_altitude(orchard.origin.altitude_m);

    for without in ["aether", "radiation", "moisture"] {
        let mut limited = fields;
        match without {
            "aether" => limited.aether = 0.0,
            "radiation" => limited.high_sky_radiation = 0.0,
            "moisture" => limited.moisture = 0.0,
            _ => unreachable!(),
        }
        let stocks = SkyStocks::from_orchard_fields(&limited);
        assert_eq!(
            stocks.plankton, 0.0,
            "zero {without} did not stop plankton productivity"
        );
        assert_eq!(
            [
                stocks.root_support,
                stocks.soil_fertility,
                stocks.canopy_biomass,
                stocks.flowers,
                stocks.pollination,
                stocks.fruit,
                stocks.detritus,
                stocks.seed_spore_reserve,
                stocks.animal_forage,
            ],
            [0.0; 9],
            "zero {without} left a downstream orchard stock"
        );
    }
}

/// Breaking any multiplier or dependency link must violate this hand-ordered
/// mature-orchard chain; no expectation is derived by the production helper.
#[test]
fn orchard_stocks_expose_a_bounded_dependency_chain() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let orchard = skyworld
        .territories
        .iter()
        .find(|territory| territory.phenotype.ecology == SkyEcology::OrchardBearing)
        .expect("the fixture contains its canonical orchard");
    let fields = skyworld.fields.at_altitude(orchard.origin.altitude_m);
    let stocks = SkyStocks::from_orchard_fields(&fields);
    let chain = [
        stocks.plankton,
        stocks.root_support,
        stocks.soil_fertility,
        stocks.canopy_biomass,
        stocks.flowers,
        stocks.pollination,
        stocks.fruit,
    ];

    assert!(chain.iter().all(|value| *value > 0.0));
    assert!(
        chain.windows(2).all(|pair| pair[0] > pair[1]),
        "each aggregate stock must be bounded by its prerequisite: {chain:?}"
    );
    let all_stocks = [
        stocks.plankton,
        stocks.root_support,
        stocks.soil_fertility,
        stocks.canopy_biomass,
        stocks.flowers,
        stocks.pollination,
        stocks.fruit,
        stocks.cloud_water,
        stocks.detritus,
        stocks.seed_spore_reserve,
        stocks.animal_forage,
    ];
    assert!(
        all_stocks
            .iter()
            .all(|value| value.is_finite() && (0.0..=1.0).contains(value)),
        "the fixed stock vector escaped its finite unit bounds: {all_stocks:?}"
    );
}

/// Dropping the `(territory, time)` key or consulting query order must change
/// this result and fail the cache-independence comparison.
#[test]
fn trajectory_queries_are_deterministic_ordered_and_cache_independent() {
    let fixture = fixture(42);
    let first = generate(&fixture);
    let second = generate(&fixture);
    assert!(
        !first.territories.is_empty(),
        "VACUOUS: no trajectories exist"
    );

    for territory in &first.territories {
        assert_eq!(
            territory.trajectory.len(),
            usize::from(config().trajectory_samples)
        );
        assert!(
            territory
                .trajectory
                .windows(2)
                .all(|pair| pair[0].time_slice < pair[1].time_slice),
            "trajectory samples are not in ascending time order"
        );
        let rebuilt = second
            .territories
            .iter()
            .find(|candidate| candidate.id == territory.id)
            .expect("a regenerated world retains territory identity");
        for sample in territory.trajectory.iter().rev() {
            assert_eq!(
                trajectory_at(&first, territory.id, sample.time_slice),
                Some(sample)
            );
            assert_eq!(
                trajectory_at(&second, rebuilt.id, sample.time_slice),
                Some(sample),
                "a regenerated/query-reordered sample changed"
            );
        }
    }
}

/// Removing seed or movement-profile input from the trajectory derivation
/// makes these two world identities collapse to one path.
#[test]
fn trajectory_changes_with_world_identity_or_movement_profile() {
    let first = generate(&fixture(42));
    let second = generate(&fixture(43));
    let first_path: Vec<_> = first
        .territories
        .first()
        .expect("seed 42 produces a territory")
        .trajectory
        .iter()
        .map(|sample| sample.position)
        .collect();
    let second_path: Vec<_> = second
        .territories
        .first()
        .expect("seed 43 produces a territory")
        .trajectory
        .iter()
        .map(|sample| sample.position)
        .collect();

    assert_ne!(first_path, second_path);
}

/// Collapsing physical, exchange, and influence footprints into one set must
/// fail these strict subset checks.
#[test]
fn trajectory_samples_keep_three_world_footprints_distinct_and_ordered() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let territory = skyworld
        .territories
        .iter()
        .min_by_key(|territory| territory.physical.projected.len())
        .expect("the fixture contains an isolated territory");

    for sample in &territory.trajectory {
        let physical: BTreeSet<_> = sample.physical.projected.iter().copied().collect();
        let exchange: BTreeSet<_> = sample.exchange.projected.iter().copied().collect();
        let influence: BTreeSet<_> = sample.influence.projected.iter().copied().collect();
        assert!(strictly_ordered(&sample.physical.projected));
        assert!(strictly_ordered(&sample.exchange.projected));
        assert!(strictly_ordered(&sample.influence.projected));
        assert!(physical.is_subset(&exchange) && physical != exchange);
        assert!(exchange.is_subset(&influence) && exchange != influence);
    }
}

/// A stale vertical projection or an all-territory lateral broadcast must fail
/// these current-sample and candidate-membership checks.
#[test]
fn trajectory_adjacency_is_vertical_now_and_lateral_only_when_reachable() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let territory_ids: BTreeSet<_> = skyworld
        .territories
        .iter()
        .map(|territory| territory.id)
        .collect();

    for territory in &skyworld.territories {
        for sample in &territory.trajectory {
            assert_eq!(sample.adjacency.vertical, sample.position.surface);
            assert!(
                sample
                    .adjacency
                    .lateral_territories
                    .iter()
                    .all(|id| *id != territory.id && territory_ids.contains(id))
            );
            assert!(
                sample
                    .adjacency
                    .lateral_corridors
                    .iter()
                    .all(|index| usize::from(*index) < territory.influence.corridors.len())
            );
            assert!(
                !sample.adjacency.lateral_corridors.is_empty(),
                "VACUOUS: no reachable corridor candidate was exposed"
            );
        }
    }
}

/// Merging propagation channels or omitting their semantic records must fail
/// the detail-filter and cargo/event-kind assertions.
#[test]
fn propagation_keeps_local_corridor_and_event_shapes_separate() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let territory = skyworld
        .territories
        .first()
        .expect("the fixture contains a territory");

    let local = propagation_at(&skyworld, territory.id, SkyPropagationDetail::Local)
        .expect("the territory has local propagation");
    assert!(!local.local.is_empty());
    assert!(local.corridors.is_empty() && local.events.is_empty());

    let corridors = propagation_at(&skyworld, territory.id, SkyPropagationDetail::Corridors)
        .expect("the territory has corridor propagation");
    assert!(corridors.local.is_empty() && corridors.events.is_empty());
    assert!(!corridors.corridors.is_empty());
    let cargo: BTreeSet<_> = corridors
        .corridors
        .iter()
        .flat_map(|corridor| corridor.carries.iter().copied())
        .collect();
    assert_eq!(
        cargo,
        BTreeSet::from([
            SkyCorridorKind::Seeds,
            SkyCorridorKind::Spores,
            SkyCorridorKind::Plankton,
            SkyCorridorKind::Route,
        ])
    );

    let events = propagation_at(&skyworld, territory.id, SkyPropagationDetail::Events)
        .expect("the territory has sparse event propagation");
    assert!(events.local.is_empty() && events.corridors.is_empty());
    assert!(!events.events.is_empty());
    assert!(events.events.iter().all(|event| matches!(
        event.kind,
        SkyEventKind::Bloom | SkyEventKind::Storm | SkyEventKind::Collapse
    )));
}

/// Materializing a planet-by-time grid or ignoring the requested sample count
/// must fail these exact linear-work bounds.
#[test]
fn bounded_work_scales_with_active_territories_and_requested_samples() {
    let fixture = fixture(42);
    let short = generate_with(
        &fixture,
        SkyWorldConfig {
            trajectory_samples: 2,
            ..config()
        },
    );
    let long = generate_with(
        &fixture,
        SkyWorldConfig {
            trajectory_samples: 8,
            ..config()
        },
    );
    assert_eq!(short.territories.len(), long.territories.len());
    let short_samples: usize = short
        .territories
        .iter()
        .map(|territory| territory.trajectory.len())
        .sum();
    let long_samples: usize = long
        .territories
        .iter()
        .map(|territory| territory.trajectory.len())
        .sum();
    assert_eq!(short_samples, short.territories.len() * 2);
    assert_eq!(long_samples, long.territories.len() * 8);

    let materialized_work = long_samples
        + long
            .territories
            .iter()
            .map(|territory| {
                territory.influence.local.len()
                    + territory
                        .influence
                        .corridors
                        .iter()
                        .map(|corridor| corridor.projected.len())
                        .sum::<usize>()
                    + territory.influence.events.len()
            })
            .sum::<usize>();
    let dense_planet_time = fixture.terrain.geosphere().vertex_count() * 8;
    assert!(
        materialized_work < dense_planet_time,
        "{materialized_work} sparse records reached the {dense_planet_time}-vertex dense grid"
    );
    assert!(long.territories.iter().all(|territory| {
        territory.influence.events.len() <= 1
            && territory.influence.corridors.len() <= 1
            && territory
                .influence
                .corridors
                .iter()
                .all(|corridor| corridor.projected.len() <= territory.trajectory.len())
    }));
}

/// Consuming randomness, iterating territories in caller order, or changing
/// the requested detail's materialization must fail these byte comparisons.
#[test]
fn render_is_byte_deterministic_and_detail_specific() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let mut pngs = Vec::new();
    let mut readouts = Vec::new();

    for detail in [
        SkyWorldDetail::Planet,
        SkyWorldDetail::Regional,
        SkyWorldDetail::Habitat,
    ] {
        let png = render_skyworld_png(&skyworld, &fixture.terrain, detail);
        let readout = render_skyworld_readout(&skyworld, detail);
        assert_eq!(
            png,
            render_skyworld_png(&skyworld, &fixture.terrain, detail)
        );
        assert_eq!(readout, render_skyworld_readout(&skyworld, detail));
        assert!(png.starts_with(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]));
        assert_eq!(&png[16..20], &256_u32.to_be_bytes());
        assert_eq!(&png[20..24], &128_u32.to_be_bytes());
        pngs.push(png);
        readouts.push(readout);
    }

    assert_ne!(pngs[0], pngs[1], "planet and regional rasters collapsed");
    assert_ne!(pngs[1], pngs[2], "regional and habitat rasters collapsed");
    assert_ne!(readouts[0], readouts[1]);
    assert_ne!(readouts[1], readouts[2]);
}

/// Moving phenotype, lifecycle, or stocks into a coarser detail—or exposing
/// atmospheric causes through the ordinary lens—must fail these boundaries.
#[test]
fn ordinary_readout_keeps_detail_and_diagnostic_causes_separate() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let planet = render_skyworld_readout(&skyworld, SkyWorldDetail::Planet);
    let regional = render_skyworld_readout(&skyworld, SkyWorldDetail::Regional);
    let habitat = render_skyworld_readout(&skyworld, SkyWorldDetail::Habitat);
    let diagnostic = render_skyworld_diagnostic_readout(&skyworld, SkyWorldDetail::Habitat);

    assert!(planet.contains("coverage="));
    assert!(planet.contains("centroid="));
    assert!(planet.contains("corridor="));
    assert!(planet.contains("event="));
    assert!(!planet.contains("phenotype="));
    assert!(!planet.contains("lifecycle="));
    assert!(!planet.contains("stocks="));

    for visible in ["physical=", "projection=", "influence=", "route="] {
        assert!(
            regional.contains(visible),
            "regional view omitted {visible}"
        );
    }
    assert!(!regional.contains("phenotype="));
    assert!(!regional.contains("lifecycle="));
    assert!(!regional.contains("stocks="));

    for internal in ["phenotype=", "lifecycle=", "stocks="] {
        assert!(
            habitat.contains(internal),
            "habitat view omitted {internal}"
        );
    }
    for hidden in [
        "pressure=",
        "density=",
        "radiation=",
        "aether=",
        "wind=",
        "moisture=",
    ] {
        assert!(!planet.contains(hidden), "planet view leaked {hidden}");
        assert!(!regional.contains(hidden), "regional view leaked {hidden}");
        assert!(!habitat.contains(hidden), "habitat view leaked {hidden}");
        assert!(diagnostic.contains(hidden), "diagnostic omitted {hidden}");
    }
    assert!(diagnostic.contains("stocks="));
    assert!(diagnostic.contains("propagation="));
}

/// Visible consequences must be derived from present physical/exchange state;
/// deleting that state must remove the words rather than leave stock boilerplate.
#[test]
fn ordinary_readout_names_only_present_visible_consequences() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let ordinary = render_skyworld_readout(&skyworld, SkyWorldDetail::Regional);
    for consequence in ["shadow", "spores", "rain", "cloud-contact"] {
        assert!(
            ordinary.contains(consequence),
            "fixture omitted {consequence}"
        );
    }

    let mut absent = skyworld.clone();
    for territory in &mut absent.territories {
        territory.physical.projected.clear();
        territory.exchange.projected.clear();
        territory.influence.local.clear();
        territory.influence.corridors.clear();
        territory.influence.events.clear();
        territory.stocks.cloud_water = 0.0;
        territory.stocks.seed_spore_reserve = 0.0;
    }
    let ordinary = render_skyworld_readout(&absent, SkyWorldDetail::Regional);
    for consequence in ["shadow", "spores", "rain", "cloud-contact"] {
        assert!(
            !ordinary.contains(consequence),
            "absent consequence still rendered: {consequence}"
        );
    }
}

/// Replacing the surface base or merging the physical, exchange, and
/// influence layers must fail the untouched-pixel and palette assertions.
#[test]
fn regional_png_preserves_surface_and_distinguishes_three_world_footprints() {
    let fixture = fixture(42);
    let generated = generate(&fixture);
    let mut baseline_world = generated.clone();
    baseline_world.territories.clear();
    let baseline = png_rgb(&render_skyworld_png(
        &baseline_world,
        &fixture.terrain,
        SkyWorldDetail::Regional,
    ));

    let mut territory = generated.territories[0].clone();
    territory.influence.corridors.clear();
    territory.influence.events.clear();
    let physical: BTreeSet<_> = territory.physical.projected.iter().copied().collect();
    let exchange: BTreeSet<_> = territory.exchange.projected.iter().copied().collect();
    let influence: BTreeSet<_> = territory.influence.local.iter().copied().collect();
    assert!(physical.is_subset(&exchange) && physical != exchange);
    assert!(exchange.is_subset(&influence) && exchange != influence);
    let one = SkyWorld {
        fields: generated.fields,
        territories: vec![territory],
    };
    let rendered = png_rgb(&render_skyworld_png(
        &one,
        &fixture.terrain,
        SkyWorldDetail::Regional,
    ));

    let geo = fixture.terrain.geosphere();
    let index = NearestVertexIndex::new(geo);
    let mut physical_colors = BTreeSet::new();
    let mut exchange_colors = BTreeSet::new();
    let mut influence_colors = BTreeSet::new();
    let mut untouched_land = false;
    let mut untouched_ocean = false;
    for py in 0..128 {
        let latitude = 90.0 - (py as f64 + 0.5) / 128.0 * 180.0;
        for px in 0..256 {
            let longitude = (px as f64 + 0.5) / 256.0 * 360.0 - 180.0;
            let vertex = index.nearest(geo, latitude, longitude);
            let offset = (py as usize * 256 + px as usize) * 3;
            let color = rendered[offset..offset + 3].to_vec();
            if physical.contains(&vertex) {
                physical_colors.insert(color);
            } else if exchange.contains(&vertex) {
                exchange_colors.insert(color);
            } else if influence.contains(&vertex) {
                influence_colors.insert(color);
            } else if fixture.terrain.is_ocean(vertex) {
                untouched_ocean |= rendered[offset..offset + 3] == baseline[offset..offset + 3];
            } else {
                untouched_land |= rendered[offset..offset + 3] == baseline[offset..offset + 3];
            }
        }
    }
    assert_eq!(
        physical_colors.len(),
        1,
        "physical layer is not one projection"
    );
    assert_eq!(
        exchange_colors.len(),
        1,
        "exchange layer is not one projection"
    );
    assert_eq!(
        influence_colors.len(),
        1,
        "influence layer is not one projection"
    );
    assert_ne!(physical_colors, exchange_colors);
    assert_ne!(exchange_colors, influence_colors);
    assert_ne!(physical_colors, influence_colors);
    assert!(
        untouched_land,
        "ordinary land disappeared beneath the overlay"
    );
    assert!(
        untouched_ocean,
        "ordinary sea disappeared beneath the overlay"
    );
}

/// Repainting the whole image per route sample must fail this narrow delta:
/// replacing one route vertex may alter its old/new pixels, but no unrelated
/// part of the surface or another territory.
#[test]
fn changing_one_route_sample_changes_only_sparse_route_pixels() {
    let fixture = fixture(42);
    let generated = generate(&fixture);
    let mut one = generated.clone();
    one.territories.truncate(1);
    let territory = &mut one.territories[0];
    territory.physical.projected.clear();
    territory.exchange.projected.clear();
    territory.influence.local.clear();
    territory.influence.events.clear();
    let route = &mut territory.influence.corridors[0].projected;
    assert!(!route.is_empty(), "VACUOUS: fixture has no route sample");
    let replacement = fixture
        .terrain
        .geosphere()
        .vertices()
        .find(|vertex| !route.contains(vertex))
        .expect("the route is sparse");

    let before = png_rgb(&render_skyworld_png(
        &one,
        &fixture.terrain,
        SkyWorldDetail::Regional,
    ));
    one.territories[0].influence.corridors[0].projected[0] = replacement;
    let after = png_rgb(&render_skyworld_png(
        &one,
        &fixture.terrain,
        SkyWorldDetail::Regional,
    ));
    let changed = before
        .chunks_exact(3)
        .zip(after.chunks_exact(3))
        .filter(|(left, right)| left != right)
        .count();
    assert!(changed > 0, "the selected route sample was not rendered");
    assert!(
        changed < (256 * 128) / 100,
        "one sparse route sample repainted {changed} pixels"
    );
}

/// Rendering is a read-only lens: changing detail must not alter generated
/// territory state or the atmospheric baseline.
#[test]
fn rendering_does_not_mutate_generated_skyworld() {
    let fixture = fixture(42);
    let skyworld = generate(&fixture);
    let before = skyworld.clone();
    for detail in [
        SkyWorldDetail::Planet,
        SkyWorldDetail::Regional,
        SkyWorldDetail::Habitat,
    ] {
        let _ = render_skyworld_png(&skyworld, &fixture.terrain, detail);
        let _ = render_skyworld_readout(&skyworld, detail);
        let _ = render_skyworld_diagnostic_readout(&skyworld, detail);
    }
    assert_eq!(skyworld, before);
}
