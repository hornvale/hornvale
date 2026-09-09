//! Stage 1 properties for the additive Skyworld overlay.
#![allow(clippy::disallowed_methods)] // named construction site for the test fixture

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_climate::{Biome, BiomeExpr, GeneratedClimate};
use hornvale_kernel::{Seed, Vertex, World};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyCorridorKind, SkyEcology, SkyEventKind, SkyPropagationDetail,
    SkyStocks, SkyWorld, SkyWorldConfig, WorldComponents, build_world_to_with_artifacts,
    climate_from, propagation_at, skyworld_from, trajectory_at,
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
