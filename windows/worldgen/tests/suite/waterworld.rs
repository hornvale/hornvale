//! Stage 1 seam probes for the additive Waterworld overlay.
#![allow(clippy::disallowed_methods)] // named construction site; decision 0092

use std::collections::BTreeMap;

use hornvale_astronomy::SkyPins;
use hornvale_climate::{BiomeExpr, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{Seed, Vertex, World, WorldTime};
use hornvale_terrain::{GeneratedTerrain, TerrainPins, WaterKind};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, VentState, WaterSubstrate, WaterWorld, WaterWorldConfig,
    WaterWorldDetail, WaterWorldSnapshot, WorldComponents, build_world_to_with_artifacts,
    climate_from, observe_waterworld, waterworld_from,
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

fn source_at(fixture: &Fixture, vertex: Vertex, time: WorldTime) -> SourceSample {
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
        temperature_c: fixture.climate.temperature_at(vertex, time).get(),
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
        let mut depth_bands = Vec::new();
        for sample in &generated.substrate {
            if !depth_bands.contains(&sample.depth_band) {
                depth_bands.push(sample.depth_band);
            }
        }
        let distinct_depth_bands = depth_bands.len();

        eprintln!(
            "waterworld seam witnesses: substrate={} seabed={} column={} depth_distinctions={} depth_bands={}",
            generated.substrate.len(),
            seabed.len(),
            column.len(),
            depth_distinctions,
            distinct_depth_bands
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
        assert!(
            distinct_depth_bands > 1,
            "VACUOUS: projected column has no distinct marine depth bands"
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
            .map(|vertex| (vertex, source_at(&fixture, vertex, WorldTime::GENESIS)))
            .collect();
        let non_marine = fixture
            .terrain
            .geosphere()
            .vertices()
            .find(|&vertex| fixture.terrain.water_kind_at(vertex) != WaterKind::Ocean)
            .map(|vertex| (vertex, source_at(&fixture, vertex, WorldTime::GENESIS)))
            .expect("VACUOUS: seed 42 has no non-marine source");
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
        let edifice_witnesses = marine
            .iter()
            .filter(|(vertex, _)| fixture.terrain.has_edifice(*vertex))
            .count();

        eprintln!(
            "waterworld source witnesses: marine={} non_marine=1 currents={} boundaries={} edifices={} features={}",
            marine.len(),
            nonzero_currents,
            boundary_witnesses,
            edifice_witnesses,
            feature_witnesses
        );
        assert!(!marine.is_empty(), "VACUOUS: seed 42 has no marine sources");
        assert_ne!(
            marine[0].1, non_marine.1,
            "VACUOUS: marine and non-marine witnesses have identical sources"
        );
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
        assert!(edifice_witnesses > 0, "VACUOUS: no ocean edifice");
        assert!(feature_witnesses > 0, "VACUOUS: no ocean terrain feature");
    }

    #[test]
    fn genesis_snapshot_preserves_the_static_overlay_boundary() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let stable_before = generated.clone();
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);

        assert_eq!(generated, stable_before);
        assert_eq!(snapshot.fields.len(), generated.fields.len());
        assert_eq!(snapshot.stocks, generated.stocks);
        assert_eq!(snapshot.propagation, generated.propagation);
        assert_eq!(snapshot.vent_states.len(), generated.vents.len());
        assert_eq!(snapshot.vent_positions.len(), generated.vents.len());
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
                let before = source_at(&sparse, vertex, WorldTime::GENESIS);
                let after = source_at(&oceanic, vertex, WorldTime::GENESIS);
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
}

mod absent_overlay {
    use super::*;

    #[test]
    fn disabled_waterworld_is_empty_and_preserves_existing_sources() {
        let fixture = seed_42();
        let before_world = fixture.world.to_json();
        let before_sources: Vec<SourceSample> = fixture
            .terrain
            .geosphere()
            .vertices()
            .map(|vertex| source_at(&fixture, vertex, WorldTime::GENESIS))
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
            .map(|vertex| source_at(&fixture, vertex, WorldTime::GENESIS))
            .collect();
        assert!(disabled.substrate.is_empty());
        let genesis = disabled.at(&fixture.climate, WorldTime::GENESIS);
        let future = disabled.at(
            &fixture.climate,
            WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY * 10),
        );
        assert_eq!(genesis, WaterWorldSnapshot::default());
        assert_eq!(genesis, future);
        assert_eq!(fixture.world.to_json(), before_world);
        assert_eq!(after_sources, before_sources);
    }
}

mod temporal_red {
    use super::*;

    /// Catches a snapshot path that accepts `WorldTime` but keeps reading the
    /// genesis climate value.
    #[test]
    fn future_climate_temperature_reaches_the_present_field_readout() {
        let fixture = seed_42();
        let future = WorldTime::from_ticks(WorldTime::TICKS_PER_STD_DAY * 10);
        let (vertex, genesis_source, future_source) = fixture
            .terrain
            .geosphere()
            .vertices()
            .filter(|&vertex| fixture.terrain.water_kind_at(vertex) == WaterKind::Ocean)
            .find_map(|vertex| {
                let genesis_source = source_at(&fixture, vertex, WorldTime::GENESIS);
                let future_source = source_at(&fixture, vertex, future);
                (genesis_source.temperature_c != future_source.temperature_c).then_some((
                    vertex,
                    genesis_source,
                    future_source,
                ))
            })
            .expect("VACUOUS: ten future days changed no consumed marine temperature source");

        assert_ne!(
            genesis_source.temperature_c, future_source.temperature_c,
            "VACUOUS: climate-time witness did not change at {vertex:?}"
        );
        eprintln!(
            "waterworld temporal source witness: vertex={} genesis_temperature_c={} future_temperature_c={}",
            vertex.0, genesis_source.temperature_c, future_source.temperature_c
        );

        let stable = active(&fixture);
        let sample_index = stable
            .substrate
            .iter()
            .position(|sample| sample.vertex == vertex)
            .expect("VACUOUS: temporal source vertex absent from Waterworld substrate");
        let genesis = stable.at(&fixture.climate, WorldTime::GENESIS);
        let later = stable.at(&fixture.climate, future);
        assert_ne!(
            genesis.fields[sample_index].temperature_c, later.fields[sample_index].temperature_c,
            "static Waterworld field ignored the changed climate-time source at {vertex:?}"
        );
    }
}

mod succession {
    use super::*;

    const DAY: i64 = WorldTime::TICKS_PER_STD_DAY;
    const ABSENT_END: i64 = 20 * DAY;
    const NASCENT_END: i64 = 35 * DAY;
    const ACTIVE_END: i64 = 65 * DAY;
    const WEAKENING_END: i64 = 85 * DAY;
    const CYCLE_END: i64 = 100 * DAY;

    fn source_zero(vent: &hornvale_worldgen::WaterVent) -> i64 {
        -(i64::from(vent.vertex.0).rem_euclid(100) * DAY)
    }

    /// Catches an off-by-one phase selector, a reordered state, or a snapshot
    /// that reports every admitted source as permanently active.
    #[test]
    fn exact_tick_boundaries_cover_all_five_states_on_a_real_source() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let vent = generated
            .vents
            .first()
            .expect("VACUOUS: seed 42 has no admitted vent source");
        let zero = source_zero(vent);
        let cases = [
            (0, VentState::Absent, VentState::Failed),
            (ABSENT_END, VentState::Nascent, VentState::Absent),
            (NASCENT_END, VentState::Active, VentState::Nascent),
            (ACTIVE_END, VentState::Weakening, VentState::Active),
            (WEAKENING_END, VentState::Failed, VentState::Weakening),
            (CYCLE_END, VentState::Absent, VentState::Failed),
        ];

        for (boundary, at_boundary, before_boundary) in cases {
            let at = generated.at(&fixture.climate, WorldTime::from_ticks(zero + boundary));
            let before = generated.at(&fixture.climate, WorldTime::from_ticks(zero + boundary - 1));
            assert_eq!(at.vent_states[vent.id], at_boundary);
            assert_eq!(before.vent_states[vent.id], before_boundary);
        }
    }

    #[test]
    fn genesis_has_real_witnesses_for_every_succession_state() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        let states = [
            VentState::Absent,
            VentState::Nascent,
            VentState::Active,
            VentState::Weakening,
            VentState::Failed,
        ];
        let counts = states.map(|state| {
            snapshot
                .vent_states
                .iter()
                .filter(|candidate| **candidate == state)
                .count()
        });

        eprintln!("waterworld succession state witnesses at genesis: {counts:?}");
        assert!(
            counts.into_iter().all(|count| count > 0),
            "VACUOUS: seed 42 does not witness all five vent states"
        );
        assert!(
            snapshot
                .vent_phase_positions
                .iter()
                .all(|position| position.is_finite() && (0.0..1.0).contains(position))
        );
        assert!(
            snapshot
                .fields
                .iter()
                .all(|field| field.chemistry.is_finite() && (0.0..=1.0).contains(&field.chemistry))
        );
    }
}

mod fields {
    use super::*;

    fn active_time(vent: &hornvale_worldgen::WaterVent) -> WorldTime {
        let offset_days = i64::from(vent.vertex.0).rem_euclid(100);
        WorldTime::from_ticks((35 - offset_days) * WorldTime::TICKS_PER_STD_DAY)
    }

    fn field_index(world: &WaterWorld, vertex: Vertex) -> usize {
        world
            .substrate
            .iter()
            .position(|sample| sample.vertex == vertex && sample.is_seabed)
            .expect("vent influence vertex has a seabed sample")
    }

    fn has_open_chemistry_path(world: &WaterWorld, vent: &hornvale_worldgen::WaterVent) -> bool {
        world.vent_candidate_rings[vent.id]
            .iter()
            .take(2)
            .all(|vertex| !world.substrate[field_index(world, *vertex)].has_edifice)
    }

    /// Catches a coupled vent term where changing chemistry also changes the
    /// thermal or ambient field paths, and catches a snapshot that ignores the
    /// stable vent chemistry source entirely.
    #[test]
    fn chemistry_source_changes_only_local_chemistry() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let mut changed = generated.clone();
        let original = generated
            .vents
            .iter()
            .find(|vent| {
                generated.vent_candidate_rings[vent.id].len() >= 2
                    && has_open_chemistry_path(&generated, vent)
            })
            .copied()
            .expect("VACUOUS: no vent has open local chemistry capacity");
        changed.vents[original.id].chemistry = (original.chemistry + 0.25).min(1.0);
        assert_ne!(changed.vents[original.id].chemistry, original.chemistry);
        assert_eq!(changed.vents[original.id].strength, original.strength);
        assert_eq!(
            changed.vents[original.id].temperature_delta,
            original.temperature_delta
        );

        let time = active_time(&original);
        let before = generated.at(&fixture.climate, time);
        let after = changed.at(&fixture.climate, time);
        let changed_fields = before
            .fields
            .iter()
            .zip(&after.fields)
            .enumerate()
            .filter(|(_, (a, b))| a.chemistry != b.chemistry)
            .collect::<Vec<_>>();
        assert_eq!(
            changed_fields.len(),
            1,
            "one changed source must reach exactly one local field"
        );
        assert_eq!(
            changed_fields[0].0,
            field_index(&generated, before.vent_positions[original.id].unwrap())
        );
        assert!(
            before
                .fields
                .iter()
                .zip(&after.fields)
                .any(|(a, b)| a.chemistry != b.chemistry),
            "changed vent chemistry reached no local field"
        );
        assert!(before.fields.iter().zip(&after.fields).all(|(a, b)| {
            a.light == b.light
                && a.pressure == b.pressure
                && a.temperature_c == b.temperature_c
                && a.salinity == b.salinity
                && a.current == b.current
        }));
    }

    /// Catches a coupled vent term where changing thermal output also changes
    /// chemistry or ambient fields, and catches a snapshot that ignores the
    /// stable thermal source entirely.
    #[test]
    fn thermal_source_changes_only_local_temperature() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let mut changed = generated.clone();
        let original = generated.vents[0];
        changed.vents[0].temperature_delta = original.temperature_delta + 10.0;
        assert_ne!(
            changed.vents[0].temperature_delta,
            original.temperature_delta
        );
        assert_eq!(changed.vents[0].strength, original.strength);
        assert_eq!(changed.vents[0].chemistry, original.chemistry);

        let time = active_time(&original);
        let before = generated.at(&fixture.climate, time);
        let after = changed.at(&fixture.climate, time);
        let changed_fields = before
            .fields
            .iter()
            .zip(&after.fields)
            .enumerate()
            .filter(|(_, (a, b))| a.temperature_c != b.temperature_c)
            .collect::<Vec<_>>();
        assert_eq!(
            changed_fields.len(),
            1,
            "one changed source must reach exactly one local field"
        );
        assert_eq!(
            changed_fields[0].0,
            field_index(&generated, before.vent_positions[original.id].unwrap())
        );
        assert!(
            before
                .fields
                .iter()
                .zip(&after.fields)
                .any(|(a, b)| a.temperature_c != b.temperature_c),
            "changed vent thermal source reached no local field"
        );
        assert!(before.fields.iter().zip(&after.fields).all(|(a, b)| {
            a.light == b.light
                && a.pressure == b.pressure
                && a.salinity == b.salinity
                && a.chemistry == b.chemistry
                && a.current == b.current
        }));
    }

    /// Catches state labels that change without changing the two documented
    /// local consequences, or a failed source that keeps emitting.
    #[test]
    fn state_strength_changes_thermal_and_chemical_contributions() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let vent = generated
            .vents
            .iter()
            .find(|vent| {
                generated.vent_candidate_rings[vent.id].len() >= 2
                    && has_open_chemistry_path(&generated, vent)
            })
            .copied()
            .expect("VACUOUS: no vent has open nascent and active chemistry paths");
        let source_zero = -(vent.phase_offset_ticks);
        let nascent_time = WorldTime::from_ticks(source_zero + 20 * WorldTime::TICKS_PER_STD_DAY);
        let active_time = WorldTime::from_ticks(source_zero + 35 * WorldTime::TICKS_PER_STD_DAY);
        let failed_time = WorldTime::from_ticks(source_zero + 85 * WorldTime::TICKS_PER_STD_DAY);

        let local_delta = |time: WorldTime| {
            let snapshot = generated.at(&fixture.climate, time);
            let vertex = snapshot.vent_positions[vent.id]
                .expect("a contributing vent state has one influence position");
            let index = field_index(&generated, vertex);
            let ambient = hornvale_worldgen::waterworld::WaterFields::from_substrate(
                &generated.substrate[index],
                fixture.climate.insolation(),
                fixture.climate.temperature_at(vertex, time).get(),
                fixture.climate.current_at(vertex),
            );
            (
                snapshot.fields[index].temperature_c - ambient.temperature_c,
                snapshot.fields[index].chemistry - ambient.chemistry,
            )
        };
        let nascent = local_delta(nascent_time);
        let active = local_delta(active_time);
        assert!(nascent.0 > 0.0 && nascent.0 < active.0);
        assert!(nascent.1 > 0.0 && nascent.1 < active.1);

        let failed = generated.at(&fixture.climate, failed_time);
        assert_eq!(failed.vent_states[vent.id], VentState::Failed);
        assert_eq!(failed.vent_positions[vent.id], None);
        let mut changed_failed = generated.clone();
        changed_failed.vents[vent.id].temperature_delta += 10.0;
        changed_failed.vents[vent.id].chemistry += 0.25;
        assert_ne!(changed_failed.vents[vent.id], vent);
        let failed_after_source_change = changed_failed.at(&fixture.climate, failed_time);
        assert_eq!(
            failed_after_source_change.vent_states[vent.id],
            VentState::Failed
        );
        assert_eq!(failed_after_source_change.vent_positions[vent.id], None);
        assert_eq!(failed_after_source_change.fields, failed.fields);
    }
}

mod migration {
    use super::*;

    /// Catches an unbounded/unsorted candidate set, a selector that never
    /// migrates, or a temporal query that rewrites stable source/substrate data.
    #[test]
    fn succession_selects_one_bounded_candidate_without_moving_substrate() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let vent = generated
            .vents
            .iter()
            .find(|vent| generated.vent_candidate_rings[vent.id].len() >= 3)
            .copied()
            .expect("VACUOUS: no vent has three marine ring candidates");
        let ring = &generated.vent_candidate_rings[vent.id];
        assert!(ring.len() <= 5);
        assert!(ring.windows(2).all(|pair| pair[0] < pair[1]));
        assert!(
            generated.vents.iter().all(|source| {
                generated.vent_candidate_rings[source.id].contains(&source.vertex)
            })
        );
        assert!(ring.iter().all(|vertex| {
            generated
                .substrate
                .iter()
                .any(|sample| sample.vertex == *vertex && sample.is_seabed)
        }));
        let before_substrate = generated.substrate.clone();
        let before_source = vent;
        let zero = -vent.phase_offset_ticks;
        let times = [20, 35, 65]
            .map(|day| WorldTime::from_ticks(zero + day * WorldTime::TICKS_PER_STD_DAY));
        let positions =
            times.map(|time| generated.at(&fixture.climate, time).vent_positions[vent.id]);

        assert!(positions.iter().all(Option::is_some));
        assert!(
            positions
                .into_iter()
                .flatten()
                .all(|vertex| ring.contains(&vertex))
        );
        assert_ne!(positions[0], positions[1]);
        assert_ne!(positions[1], positions[2]);
        assert_eq!(generated.substrate, before_substrate);
        assert_eq!(generated.vents[vent.id], before_source);
    }
}

mod determinism {
    use super::*;

    #[test]
    fn repeated_and_reordered_snapshot_queries_are_byte_identical_and_pure() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let stable_before = generated.clone();
        let time = WorldTime::from_ticks(47 * WorldTime::TICKS_PER_STD_DAY + 123);
        let first = generated.at(&fixture.climate, time);
        let _other = generated.at(
            &fixture.climate,
            WorldTime::from_ticks(88 * WorldTime::TICKS_PER_STD_DAY),
        );
        let repeated = generated.at(&fixture.climate, time);

        assert_eq!(first, repeated);
        assert_eq!(
            format!("{first:?}").as_bytes(),
            format!("{repeated:?}").as_bytes()
        );
        assert_eq!(generated, stable_before);
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
    let changed_sources = hornvale_worldgen::waterworld::WaterFields::from_sources(
        substrate,
        fixture.climate.insolation(),
        fields.temperature_c + 1.0,
        fields.salinity + 1.0,
        fields.chemistry + 1.0,
        [
            fields.current[0] + 1.0,
            fields.current[1],
            fields.current[2],
        ],
    );
    assert_ne!(changed_sources.temperature_c, fields.temperature_c);
    assert_ne!(changed_sources.salinity, fields.salinity);
    assert_ne!(changed_sources.chemistry, fields.chemistry);
    assert_ne!(changed_sources.current, fields.current);
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

#[test]
fn stocks_and_propagation_are_bounded_ordered_and_counted() {
    let generated = active(&seed_42());
    assert_eq!(generated.substrate.len(), generated.stocks.len());
    assert!(generated.stocks.iter().all(|stock| {
        [
            stock.plankton,
            stock.chemosynthetic_bloom,
            stock.nutrients,
            stock.kelp_reef,
        ]
        .into_iter()
        .all(|value| value.is_finite() && (0.0..=1.0).contains(&value))
    }));
    assert!(generated.propagation.candidate_count >= generated.propagation.accepted_count);
    assert!(generated.propagation.accepted_count > 0);
    assert!(
        generated
            .propagation
            .samples
            .windows(2)
            .all(|pair| (pair[0].vertex, pair[0].depth_m) <= (pair[1].vertex, pair[1].depth_m))
    );
}

#[test]
fn observation_keeps_ordinary_and_diagnostic_claims_distinct_and_pure() {
    let generated = active(&seed_42());
    let ordinary = observe_waterworld(&generated, WaterWorldDetail::Habitat, false);
    let diagnostic = observe_waterworld(&generated, WaterWorldDetail::Habitat, true);
    assert!(ordinary.contains("substrate samples"));
    assert!(ordinary.contains("aggregate stock samples"));
    assert!(!ordinary.contains("diagnostic fields"));
    assert!(diagnostic.contains("diagnostic fields include depth, pressure, light"));
    assert!(diagnostic.contains("values are derived, not certainty"));
    assert_eq!(
        diagnostic,
        observe_waterworld(&generated, WaterWorldDetail::Habitat, true)
    );
}
