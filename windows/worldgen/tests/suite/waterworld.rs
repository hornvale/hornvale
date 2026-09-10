//! Stage 1 seam probes for the additive Waterworld overlay.
#![allow(clippy::disallowed_methods)] // named construction site; decision 0092

use std::collections::BTreeMap;

use hornvale_astronomy::SkyPins;
use hornvale_climate::{BiomeExpr, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{Seed, Vertex, World, WorldTime};
use hornvale_terrain::{GeneratedTerrain, TerrainPins, WaterKind};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WaterSubstrate, WaterWorld, WaterWorldConfig, WorldComponents,
    build_world_to_with_artifacts, climate_from, waterworld_from,
};

struct Fixture {
    world: World,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
}

fn fixture(terrain_pins: TerrainPins) -> Fixture {
    let components = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        &terrain_pins,
        &SettlementPins::default(),
        &components,
        BuildDepth::Terrain,
    )
    .expect("the terrain-depth fixture builds");
    let terrain = built.terrain.expect("terrain depth returns terrain");
    let climate = climate_from(&built.world, &terrain).expect("climate derives from terrain");
    Fixture {
        world: built.world,
        terrain,
        climate,
    }
}

fn seed_42() -> Fixture {
    fixture(TerrainPins::default())
}

fn active(fixture: &Fixture) -> WaterWorld {
    waterworld_from(
        &fixture.world,
        &fixture.terrain,
        &fixture.climate,
        WaterWorldConfig { enabled: true },
    )
}

#[derive(Clone, Debug, PartialEq)]
struct SourceSample {
    water_kind: WaterKind,
    seabed_depth_m: f64,
    biome_expr: BiomeExpr,
    column: Vec<Stratum>,
    temperature_c: f64,
    insolation: f64,
    current: [f64; 3],
    has_boundary: bool,
    terrain_feature_count: usize,
}

fn source_at(fixture: &Fixture, vertex: Vertex) -> SourceSample {
    let terrain_feature_count = fixture
        .terrain
        .features()
        .all()
        .filter(|feature| feature.extent.contains(&vertex))
        .count();
    SourceSample {
        water_kind: fixture.terrain.water_kind_at(vertex),
        seabed_depth_m: (fixture.terrain.sea_level().get()
            - fixture.terrain.elevation_at(vertex).get())
        .max(0.0),
        biome_expr: fixture.climate.biome_expr_at(vertex),
        column: fixture.climate.strata_at(vertex),
        temperature_c: fixture
            .climate
            .temperature_at(vertex, WorldTime::GENESIS)
            .get(),
        insolation: fixture.climate.insolation(),
        current: fixture.climate.current_at(vertex),
        has_boundary: fixture.terrain.boundary_at(vertex).is_some(),
        terrain_feature_count,
    }
}

fn at_vertex(world: &WaterWorld, vertex: Vertex) -> Vec<&WaterSubstrate> {
    world
        .substrate
        .iter()
        .filter(|sample| sample.vertex == vertex)
        .collect()
}

mod seams {
    use super::*;

    #[test]
    fn seed_42_has_nonempty_seabed_and_water_column_witnesses() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let seabed: Vec<&WaterSubstrate> = generated
            .substrate
            .iter()
            .filter(|sample| sample.is_seabed)
            .collect();
        let column: Vec<&WaterSubstrate> = generated
            .substrate
            .iter()
            .filter(|sample| !sample.is_seabed)
            .collect();
        let seabed_depths: BTreeMap<Vertex, f64> = seabed
            .iter()
            .map(|sample| (sample.vertex, sample.depth_m))
            .collect();
        let depth_distinctions = column
            .iter()
            .filter(|sample| seabed_depths[&sample.vertex] != sample.depth_m)
            .count();

        eprintln!(
            "waterworld seam witnesses: substrate={} seabed={} column={} depth_distinctions={}",
            generated.substrate.len(),
            seabed.len(),
            column.len(),
            depth_distinctions
        );
        assert!(
            !generated.substrate.is_empty(),
            "VACUOUS: Waterworld projected no marine substrate"
        );
        assert!(!seabed.is_empty(), "VACUOUS: no seabed witnesses");
        assert!(!column.is_empty(), "VACUOUS: no water-column witnesses");
        assert!(
            depth_distinctions > 0,
            "VACUOUS: no vertex distinguishes a water-column depth from its seabed"
        );
        assert!(seabed.iter().all(|sample| {
            sample.water_kind == WaterKind::Ocean
                && sample.biome_expr == fixture.climate.biome_expr_at(sample.vertex)
                && sample.depth_band == sample.biome_expr.stratum
                && sample.render_vertex == sample.vertex
                && sample.seafloor_boundary
                    == fixture
                        .terrain
                        .boundary_at(sample.vertex)
                        .map(|boundary| boundary.kind)
                && sample.has_edifice == fixture.terrain.has_edifice(sample.vertex)
        }));
        assert!(
            generated
                .substrate
                .iter()
                .any(|sample| !sample.terrain_features.is_empty()),
            "VACUOUS: projected substrate carries no terrain-feature witness"
        );
        assert!(column.iter().all(|sample| {
            fixture
                .climate
                .biome_expr_at_stratum(sample.vertex, sample.depth_band)
                == Some(sample.biome_expr)
                && sample.biome_expr.realm == Realm::WATERWORLD
                && sample.render_vertex == sample.vertex
        }));
    }

    #[test]
    fn live_ambient_and_feature_sources_have_marine_witnesses() {
        let fixture = seed_42();
        let marine: Vec<(Vertex, SourceSample)> = fixture
            .terrain
            .geosphere()
            .vertices()
            .filter(|&vertex| fixture.terrain.water_kind_at(vertex) == WaterKind::Ocean)
            .map(|vertex| (vertex, source_at(&fixture, vertex)))
            .collect();
        let nonzero_currents = marine
            .iter()
            .filter(|(_, source)| source.current != [0.0; 3])
            .count();
        let boundary_witnesses = marine
            .iter()
            .filter(|(_, source)| source.has_boundary)
            .count();
        let feature_witnesses = marine
            .iter()
            .filter(|(_, source)| source.terrain_feature_count > 0)
            .count();

        eprintln!(
            "waterworld source witnesses: marine={} currents={} boundaries={} features={}",
            marine.len(),
            nonzero_currents,
            boundary_witnesses,
            feature_witnesses
        );
        assert!(!marine.is_empty(), "VACUOUS: seed 42 has no marine sources");
        assert!(
            marine
                .iter()
                .all(|(_, source)| source.temperature_c.is_finite()),
            "temperature_at returned a non-finite marine source"
        );
        assert!(
            marine
                .iter()
                .all(|(_, source)| source.insolation.is_finite())
                && marine[0].1.insolation > 0.0,
            "VACUOUS: insolation has no positive finite marine source"
        );
        assert!(nonzero_currents > 0, "VACUOUS: no nonzero ocean current");
        assert!(boundary_witnesses > 0, "VACUOUS: no seafloor boundary");
        assert!(feature_witnesses > 0, "VACUOUS: no ocean terrain feature");
    }

    #[test]
    fn changed_ocean_source_reaches_the_substrate_projection() {
        let sparse = fixture(TerrainPins {
            ocean_fraction: Some(0.05),
            ..TerrainPins::default()
        });
        let oceanic = fixture(TerrainPins {
            ocean_fraction: Some(0.95),
            ..TerrainPins::default()
        });
        let sparse_world = active(&sparse);
        let oceanic_world = active(&oceanic);

        let (vertex, before, after) = sparse
            .terrain
            .geosphere()
            .vertices()
            .find_map(|vertex| {
                let before = source_at(&sparse, vertex);
                let after = source_at(&oceanic, vertex);
                (before.water_kind == WaterKind::Ocean
                    && after.water_kind == WaterKind::Ocean
                    && before != after)
                    .then_some((vertex, before, after))
            })
            .expect("VACUOUS: ocean-fraction pin changed no shared ocean source");

        assert_ne!(
            before, after,
            "VACUOUS: intended source perturbation was a no-op at {vertex:?}"
        );
        let before_projection = at_vertex(&sparse_world, vertex);
        let after_projection = at_vertex(&oceanic_world, vertex);
        assert!(
            !before_projection.is_empty(),
            "VACUOUS: sparse projection absent"
        );
        assert!(
            !after_projection.is_empty(),
            "VACUOUS: oceanic projection absent"
        );
        assert_ne!(
            before_projection, after_projection,
            "changed source at {vertex:?} did not reach WaterSubstrate"
        );
    }

    #[test]
    fn disabled_waterworld_preserves_the_existing_world_snapshot() {
        let fixture = seed_42();
        let before_world = fixture.world.to_json();
        let before_sources: Vec<SourceSample> = fixture
            .terrain
            .geosphere()
            .vertices()
            .map(|vertex| source_at(&fixture, vertex))
            .collect();

        let disabled = waterworld_from(
            &fixture.world,
            &fixture.terrain,
            &fixture.climate,
            WaterWorldConfig { enabled: false },
        );

        let after_sources: Vec<SourceSample> = fixture
            .terrain
            .geosphere()
            .vertices()
            .map(|vertex| source_at(&fixture, vertex))
            .collect();
        assert!(disabled.substrate.is_empty());
        assert_eq!(fixture.world.to_json(), before_world);
        assert_eq!(after_sources, before_sources);
    }
}

#[test]
fn ambient_fields_are_finite_and_depth_changes_light_and_pressure() {
    let fixture = seed_42();
    let generated = active(&fixture);
    assert_eq!(generated.substrate.len(), generated.fields.len());
    let sample = generated
        .substrate
        .iter()
        .zip(&generated.fields)
        .find(|(substrate, _)| !substrate.is_seabed)
        .expect("VACUOUS: no open-column field witness");
    let (substrate, fields) = sample;
    assert!(fields.light.is_finite());
    assert!(fields.pressure.is_finite());
    assert!(fields.temperature_c.is_finite());
    assert!(fields.salinity.is_finite());
    assert!(fields.chemistry.is_finite());
    assert_eq!(fields.depth_m, substrate.depth_m);
    assert_eq!(fields.depth_band, substrate.depth_band);
    let brighter = hornvale_worldgen::waterworld::WaterFields::from_substrate(
        substrate,
        fixture.climate.insolation() + 1.0,
        fields.temperature_c,
        fields.current,
    );
    assert_ne!(
        brighter.light, fields.light,
        "source changed but light did not"
    );
    let deeper = hornvale_worldgen::waterworld::WaterFields::from_substrate(
        &WaterSubstrate {
            depth_m: substrate.depth_m + 1.0,
            ..substrate.clone()
        },
        fixture.climate.insolation(),
        fields.temperature_c,
        fields.current,
    );
    assert_ne!(
        deeper.pressure, fields.pressure,
        "depth changed but pressure did not"
    );
    assert_ne!(
        deeper.light, fields.light,
        "depth changed but light did not"
    );
}

#[test]
fn vents_are_sparse_seeded_and_require_a_live_seafloor_source() {
    let fixture = seed_42();
    let generated = active(&fixture);
    assert!(!generated.vents.is_empty(), "VACUOUS: no vent witnesses");
    assert!(generated.vents.iter().all(|vent| {
        vent.strength > 0.0
            && vent.strength <= 1.0
            && vent.temperature_delta > 0.0
            && vent.chemistry.is_finite()
            && (fixture.terrain.has_edifice(vent.vertex)
                || fixture.terrain.boundary_at(vent.vertex).is_some())
    }));
    let again = active(&fixture);
    assert_eq!(generated.vents, again.vents);
}
