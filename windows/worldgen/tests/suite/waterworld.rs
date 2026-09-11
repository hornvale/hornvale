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
    climate_from, observe_waterworld, observe_waterworld_snapshot, waterworld_from,
};

struct Fixture {
    world: World,
    terrain: GeneratedTerrain,
    climate: GeneratedClimate,
}

fn fixture_for_seed(seed: Seed, terrain_pins: TerrainPins) -> Fixture {
    let components = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        seed,
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

fn fixture(terrain_pins: TerrainPins) -> Fixture {
    fixture_for_seed(Seed(42), terrain_pins)
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
        assert_eq!(snapshot.stocks.len(), generated.stocks.len());
        assert_eq!(
            snapshot.propagation.transported_influence.len(),
            generated.substrate.len()
        );
        assert!(snapshot.propagation.samples.len() <= generated.vents.len() * 3);
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
        -vent.phase_offset_ticks
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

    /// Catches a phase offset derived only from spatial identity instead of
    /// the existing seeded vent source draws.
    #[test]
    fn changed_seeded_source_fields_change_phase_offset_at_a_shared_anchor() {
        let first_fixture = fixture_for_seed(Seed(42), TerrainPins::default());
        let second_fixture = fixture_for_seed(Seed(43), TerrainPins::default());
        let first = active(&first_fixture);
        let second = active(&second_fixture);
        let (first_vent, second_vent) = first
            .vents
            .iter()
            .find_map(|first_vent| {
                second
                    .vents
                    .iter()
                    .find(|second_vent| {
                        second_vent.vertex == first_vent.vertex
                            && (second_vent.strength != first_vent.strength
                                || second_vent.temperature_delta != first_vent.temperature_delta
                                || second_vent.chemistry != first_vent.chemistry)
                    })
                    .map(|second_vent| (first_vent, second_vent))
            })
            .expect("VACUOUS: seeds 42 and 43 have no shared admitted anchor with changed sources");

        assert_eq!(first_vent.vertex, second_vent.vertex);
        assert_ne!(
            (
                first_vent.strength.to_bits(),
                first_vent.temperature_delta.to_bits(),
                first_vent.chemistry.to_bits(),
            ),
            (
                second_vent.strength.to_bits(),
                second_vent.temperature_delta.to_bits(),
                second_vent.chemistry.to_bits(),
            ),
            "VACUOUS: compared vent source fields did not change across seeds"
        );
        assert_ne!(
            first_vent.phase_offset_ticks, second_vent.phase_offset_ticks,
            "changed seeded vent source fields did not change the phase offset"
        );
    }
}

mod fields {
    use super::*;

    fn active_time(vent: &hornvale_worldgen::WaterVent) -> WorldTime {
        WorldTime::from_ticks(35 * WorldTime::TICKS_PER_STD_DAY - vent.phase_offset_ticks)
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

mod stocks {
    use super::*;

    fn seabed_index(world: &WaterWorld, vertex: Vertex) -> usize {
        world
            .substrate
            .iter()
            .position(|sample| sample.vertex == vertex && sample.is_seabed)
            .expect("stock witness has a seabed sample")
    }

    fn ring_has_open_chemistry(world: &WaterWorld, vent_id: usize, indices: &[usize]) -> bool {
        indices.iter().all(|&ring_index| {
            let vertex = world.vent_candidate_rings[vent_id][ring_index];
            !world.substrate[seabed_index(world, vertex)].has_edifice
        })
    }

    fn assert_bounded(stock: hornvale_worldgen::WaterStocks) {
        for value in [
            stock.plankton,
            stock.chemosynthetic_bloom,
            stock.nutrients,
            stock.kelp_reef,
            stock.local_source_influence,
            stock.transported_influence,
        ] {
            assert!(value.is_finite() && (0.0..=1.0).contains(&value));
        }
    }

    /// Catches snapshot stocks that remain copied from genesis when the
    /// consumed depth source changes present light.
    #[test]
    fn changed_light_source_changes_plankton_stock() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let mut changed = generated.clone();
        let index = generated
            .substrate
            .iter()
            .position(|sample| !sample.is_seabed && sample.depth_m > 0.0)
            .expect("VACUOUS: no lit water-column stock witness");
        changed.substrate[index].depth_m += 500.0;
        assert_ne!(
            changed.substrate[index].depth_m, generated.substrate[index].depth_m,
            "VACUOUS: depth perturbation did not change the consumed light source"
        );

        let before = generated.at(&fixture.climate, WorldTime::GENESIS);
        let after = changed.at(&fixture.climate, WorldTime::GENESIS);
        assert_ne!(before.fields[index].light, after.fields[index].light);
        assert_ne!(before.stocks[index].plankton, after.stocks[index].plankton);
        assert_eq!(
            before.fields[index].chemistry,
            after.fields[index].chemistry
        );
        assert_eq!(before.fields[index].current, after.fields[index].current);
        assert_eq!(
            before.stocks[index].chemosynthetic_bloom,
            after.stocks[index].chemosynthetic_bloom
        );
        assert_eq!(
            before.stocks[index].nutrients,
            after.stocks[index].nutrients
        );
        assert_eq!(
            before.stocks[index].local_source_influence,
            after.stocks[index].local_source_influence
        );
        assert_eq!(
            before.stocks[index].transported_influence,
            after.stocks[index].transported_influence
        );
        assert_bounded(before.stocks[index]);
        assert_bounded(after.stocks[index]);
    }

    /// Catches a chemosynthetic stock that ignores the present local
    /// chemistry contribution from its stable vent source.
    #[test]
    fn changed_chemistry_source_changes_chemosynthetic_bloom() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let mut changed = generated.clone();
        let vent = generated
            .vents
            .iter()
            .find(|vent| {
                vent.chemistry < 0.75
                    && generated.vent_candidate_rings[vent.id].len() >= 2
                    && ring_has_open_chemistry(&generated, vent.id, &[1])
            })
            .copied()
            .expect("VACUOUS: no vent chemistry source has perturbation headroom");
        changed.vents[vent.id].chemistry += 0.2;
        assert_ne!(changed.vents[vent.id].chemistry, vent.chemistry);
        let time =
            WorldTime::from_ticks(35 * WorldTime::TICKS_PER_STD_DAY - vent.phase_offset_ticks);
        let before = generated.at(&fixture.climate, time);
        let after = changed.at(&fixture.climate, time);
        let index = seabed_index(&generated, before.vent_positions[vent.id].unwrap());
        assert_ne!(
            before.fields[index].chemistry,
            after.fields[index].chemistry
        );
        assert_ne!(
            before.stocks[index].chemosynthetic_bloom,
            after.stocks[index].chemosynthetic_bloom
        );
        assert_eq!(before.fields[index].light, after.fields[index].light);
        assert_eq!(before.fields[index].pressure, after.fields[index].pressure);
        assert_eq!(
            before.fields[index].temperature_c,
            after.fields[index].temperature_c
        );
        assert_eq!(before.fields[index].salinity, after.fields[index].salinity);
        assert_eq!(before.fields[index].current, after.fields[index].current);
        assert_eq!(before.stocks[index].plankton, after.stocks[index].plankton);
        assert_eq!(
            before.stocks[index].nutrients,
            after.stocks[index].nutrients
        );
        assert_eq!(
            before.stocks[index].local_source_influence,
            after.stocks[index].local_source_influence
        );
        assert_eq!(
            before.stocks[index].transported_influence,
            after.stocks[index].transported_influence
        );
        assert_bounded(before.stocks[index]);
        assert_bounded(after.stocks[index]);
    }

    /// Catches a nutrient reserve copied from genesis instead of derived from
    /// the present substrate nutrient input.
    #[test]
    fn changed_nutrient_input_changes_nutrient_reserve() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let mut changed = generated.clone();
        let index = generated
            .substrate
            .iter()
            .position(|sample| !sample.terrain_features.is_empty())
            .expect("VACUOUS: no substrate nutrient-input witness");
        changed.substrate[index].terrain_features.clear();
        assert_ne!(
            changed.substrate[index].terrain_features.len(),
            generated.substrate[index].terrain_features.len()
        );

        let before = generated.at(&fixture.climate, WorldTime::GENESIS);
        let after = changed.at(&fixture.climate, WorldTime::GENESIS);
        assert_eq!(before.fields[index], after.fields[index]);
        assert_ne!(
            before.stocks[index].nutrients,
            after.stocks[index].nutrients
        );
        assert_eq!(before.stocks[index].plankton, after.stocks[index].plankton);
        assert_eq!(
            before.stocks[index].chemosynthetic_bloom,
            after.stocks[index].chemosynthetic_bloom
        );
        assert_eq!(
            before.stocks[index].local_source_influence,
            after.stocks[index].local_source_influence
        );
        assert_eq!(
            before.stocks[index].transported_influence,
            after.stocks[index].transported_influence
        );
        assert_bounded(before.stocks[index]);
        assert_bounded(after.stocks[index]);
    }

    /// Catches reef/kelp suitability that ignores its present substrate,
    /// temperature, and chemistry inputs.
    #[test]
    fn isolated_substrate_temperature_and_chemistry_sources_change_reef_kelp_suitability() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let substrate_index = generated
            .substrate
            .iter()
            .position(|sample| sample.is_seabed && !sample.has_edifice)
            .expect("VACUOUS: no open seabed suitability witness");
        let baseline = generated.at(&fixture.climate, WorldTime::GENESIS);
        let mut changed_substrate = generated.clone();
        changed_substrate.substrate[substrate_index].is_seabed = false;
        assert_ne!(
            changed_substrate.substrate[substrate_index].is_seabed,
            generated.substrate[substrate_index].is_seabed
        );
        let substrate_after = changed_substrate.at(&fixture.climate, WorldTime::GENESIS);
        assert_eq!(
            baseline.fields[substrate_index],
            substrate_after.fields[substrate_index]
        );
        assert_ne!(
            baseline.stocks[substrate_index].kelp_reef,
            substrate_after.stocks[substrate_index].kelp_reef
        );
        assert_eq!(
            baseline.stocks[substrate_index].plankton,
            substrate_after.stocks[substrate_index].plankton
        );
        assert_eq!(
            baseline.stocks[substrate_index].chemosynthetic_bloom,
            substrate_after.stocks[substrate_index].chemosynthetic_bloom
        );
        assert_eq!(
            baseline.stocks[substrate_index].nutrients,
            substrate_after.stocks[substrate_index].nutrients
        );
        assert_eq!(
            baseline.stocks[substrate_index].local_source_influence,
            substrate_after.stocks[substrate_index].local_source_influence
        );
        assert_eq!(
            baseline.stocks[substrate_index].transported_influence,
            substrate_after.stocks[substrate_index].transported_influence
        );

        let vent = generated
            .vents
            .iter()
            .find(|vent| {
                vent.chemistry < 0.75
                    && generated.vent_candidate_rings[vent.id].len() >= 2
                    && ring_has_open_chemistry(&generated, vent.id, &[1])
            })
            .copied()
            .expect("VACUOUS: no isolated vent suitability witness");
        let time =
            WorldTime::from_ticks(35 * WorldTime::TICKS_PER_STD_DAY - vent.phase_offset_ticks);
        let before = generated.at(&fixture.climate, time);
        let index = seabed_index(&generated, before.vent_positions[vent.id].unwrap());

        let mut changed_temperature = generated.clone();
        changed_temperature.vents[vent.id].temperature_delta += 10.0;
        assert_ne!(
            changed_temperature.vents[vent.id].temperature_delta,
            generated.vents[vent.id].temperature_delta
        );
        let temperature_after = changed_temperature.at(&fixture.climate, time);
        assert_ne!(
            before.fields[index].temperature_c,
            temperature_after.fields[index].temperature_c
        );
        assert_eq!(
            before.fields[index].chemistry,
            temperature_after.fields[index].chemistry
        );
        assert_ne!(
            before.stocks[index].kelp_reef,
            temperature_after.stocks[index].kelp_reef
        );
        assert_eq!(
            before.stocks[index].plankton,
            temperature_after.stocks[index].plankton
        );
        assert_eq!(
            before.stocks[index].chemosynthetic_bloom,
            temperature_after.stocks[index].chemosynthetic_bloom
        );
        assert_eq!(
            before.stocks[index].nutrients,
            temperature_after.stocks[index].nutrients
        );
        assert_eq!(
            before.stocks[index].local_source_influence,
            temperature_after.stocks[index].local_source_influence
        );
        assert_eq!(
            before.stocks[index].transported_influence,
            temperature_after.stocks[index].transported_influence
        );

        let mut changed_chemistry = generated.clone();
        changed_chemistry.vents[vent.id].chemistry += 0.2;
        assert_ne!(
            changed_chemistry.vents[vent.id].chemistry,
            generated.vents[vent.id].chemistry
        );
        let chemistry_after = changed_chemistry.at(&fixture.climate, time);
        assert_ne!(
            before.fields[index].chemistry,
            chemistry_after.fields[index].chemistry
        );
        assert_eq!(
            before.fields[index].light,
            chemistry_after.fields[index].light
        );
        assert_eq!(
            before.fields[index].pressure,
            chemistry_after.fields[index].pressure
        );
        assert_eq!(
            before.fields[index].temperature_c,
            chemistry_after.fields[index].temperature_c
        );
        assert_eq!(
            before.fields[index].salinity,
            chemistry_after.fields[index].salinity
        );
        assert_eq!(
            before.fields[index].current,
            chemistry_after.fields[index].current
        );
        assert_ne!(
            before.stocks[index].kelp_reef,
            chemistry_after.stocks[index].kelp_reef
        );
        assert_eq!(
            before.stocks[index].plankton,
            chemistry_after.stocks[index].plankton
        );
        assert_eq!(
            before.stocks[index].nutrients,
            chemistry_after.stocks[index].nutrients
        );
        assert_eq!(
            before.stocks[index].local_source_influence,
            chemistry_after.stocks[index].local_source_influence
        );
        assert_eq!(
            before.stocks[index].transported_influence,
            chemistry_after.stocks[index].transported_influence
        );
        assert_bounded(substrate_after.stocks[substrate_index]);
        assert_bounded(temperature_after.stocks[index]);
        assert_bounded(chemistry_after.stocks[index]);
    }
}

mod memory {
    use super::*;

    /// Decides whether present fields already distinguish the three declining
    /// consequences; a failure here would earn one analytical residue term.
    #[test]
    fn instantaneous_stocks_distinguish_active_weakening_and_failed() {
        let fixture = seed_42();
        let mut generated = active(&fixture);
        let mut vent = generated.vents[0];
        let ring = vec![vent.vertex];
        vent.id = 0;
        generated.vents = vec![vent];
        generated.vent_candidate_rings = vec![ring];
        let zero = -vent.phase_offset_ticks;
        let snapshots = [35, 75, 85].map(|day| {
            generated.at(
                &fixture.climate,
                WorldTime::from_ticks(zero + day * WorldTime::TICKS_PER_STD_DAY),
            )
        });
        assert_eq!(snapshots[0].vent_states[vent.id], VentState::Active);
        assert_eq!(snapshots[1].vent_states[vent.id], VentState::Weakening);
        assert_eq!(snapshots[2].vent_states[vent.id], VentState::Failed);
        let index = generated
            .substrate
            .iter()
            .position(|sample| sample.is_seabed && sample.vertex == vent.vertex)
            .expect("VACUOUS: vent anchor has no seabed stock row");
        let nutrients = snapshots
            .each_ref()
            .map(|snapshot| snapshot.stocks[index].nutrients);
        let kelp_reef = snapshots
            .each_ref()
            .map(|snapshot| snapshot.stocks[index].kelp_reef);
        assert!(nutrients[0] > nutrients[1] && nutrients[1] > nutrients[2]);
        assert!(kelp_reef[0] > kelp_reef[1] && kelp_reef[1] > kelp_reef[2]);
    }
}

mod transport {
    use super::*;
    use hornvale_worldgen::waterworld::WaterPropagation;

    /// Catches accepting a neighbour when the best current alignment is not
    /// positive.
    #[test]
    fn non_positive_alignment_stops_transport() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        let source_index = generated
            .substrate
            .iter()
            .position(|sample| sample.is_seabed)
            .expect("VACUOUS: no marine source row");
        let mut fields = snapshot.fields.clone();
        fields[source_index].current = [0.0; 3];
        let mut local = vec![0.0; generated.substrate.len()];
        local[source_index] = 1.0;

        let stopped = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &fields,
            &local,
            1,
            0.5,
        );
        assert!(stopped.samples.is_empty());
        assert!(
            stopped
                .transported_influence
                .iter()
                .all(|&value| value == 0.0)
        );
    }

    /// Catches attenuation based on a global coordinate component instead of
    /// the chosen neighbour alignment and current magnitude.
    #[test]
    fn stronger_aligned_current_increases_transport_without_moving_identity() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let stable_substrate = generated.substrate.clone();
        let stable_sources = generated.vents.clone();
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        let (source_index, direction) = generated
            .substrate
            .iter()
            .enumerate()
            .filter(|(_, sample)| sample.is_seabed)
            .find_map(|(index, sample)| {
                let source_position = fixture.climate.geosphere().position(sample.vertex);
                fixture
                    .climate
                    .geosphere()
                    .neighbors(sample.vertex)
                    .iter()
                    .find_map(|&neighbor| {
                        generated.substrate.iter().find(|candidate| {
                            candidate.is_seabed && candidate.vertex == neighbor
                        })?;
                        let target = fixture.climate.geosphere().position(neighbor);
                        let raw = [
                            target[0] - source_position[0],
                            target[1] - source_position[1],
                            target[2] - source_position[2],
                        ];
                        let magnitude =
                            (raw[0] * raw[0] + raw[1] * raw[1] + raw[2] * raw[2]).sqrt();
                        let unit = raw.map(|value| value / magnitude);
                        (unit[0] < -0.05).then_some((index, unit))
                    })
            })
            .expect("VACUOUS: no negative-x aligned marine path");
        let mut local = vec![0.0; generated.substrate.len()];
        local[source_index] = 1.0;
        let mut weak_fields = snapshot.fields.clone();
        weak_fields[source_index].current = direction.map(|value| value * 0.25);
        let mut strong_fields = snapshot.fields.clone();
        strong_fields[source_index].current = direction;
        assert_ne!(
            weak_fields[source_index].current, strong_fields[source_index].current,
            "VACUOUS: aligned current perturbation was a no-op"
        );

        let weak = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &weak_fields,
            &local,
            1,
            0.5,
        );
        let strong = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &strong_fields,
            &local,
            1,
            0.5,
        );
        assert_eq!(weak.samples[0].vertex, strong.samples[0].vertex);
        assert!(
            strong.transported_influence.iter().sum::<f64>()
                > weak.transported_influence.iter().sum::<f64>()
        );
        assert_eq!(generated.substrate, stable_substrate);
        assert_eq!(generated.vents, stable_sources);
    }
}

mod cost {
    use super::*;
    use hornvale_worldgen::waterworld::WaterPropagation;

    #[test]
    fn propagation_counter_scales_with_hop_bound_only() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        let mut one_source = vec![0.0; snapshot.stocks.len()];
        let source_index = snapshot
            .stocks
            .iter()
            .enumerate()
            .filter(|(_, stock)| stock.local_source_influence > 0.0)
            .map(|(index, _)| index)
            .next()
            .expect("VACUOUS: no active source");
        one_source[source_index] = 1.0;
        let one_hop = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &one_source,
            1,
            0.5,
        );
        let three_hops = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &one_source,
            3,
            0.5,
        );
        eprintln!(
            "waterworld hop-bound counters: one={:?} three={:?}",
            one_hop.counters, three_hops.counters
        );
        assert!(one_hop.counters.propagation > 0);
        assert!(three_hops.counters.propagation > one_hop.counters.propagation);
        assert!(one_hop.samples.len() <= 1);
        assert!(three_hops.samples.len() <= 3);
    }

    #[test]
    fn propagation_counter_scales_with_source_bound_only() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        let source_indices = snapshot
            .stocks
            .iter()
            .enumerate()
            .filter(|(_, stock)| stock.local_source_influence > 0.0)
            .map(|(index, _)| index)
            .take(2)
            .collect::<Vec<_>>();
        assert_eq!(
            source_indices.len(),
            2,
            "VACUOUS: fewer than two active sources"
        );
        let mut one_source = vec![0.0; snapshot.stocks.len()];
        one_source[source_indices[0]] = 1.0;
        let mut two_sources = one_source.clone();
        two_sources[source_indices[1]] = 1.0;
        let one = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &one_source,
            3,
            0.5,
        );
        let two = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &two_sources,
            3,
            0.5,
        );
        eprintln!(
            "waterworld source-bound counters: one={:?} two={:?}",
            one.counters, two.counters
        );
        assert!(two.counters.propagation > one.counters.propagation);
        assert!(one.samples.len() <= 3);
        assert!(two.samples.len() <= 6);
    }

    #[test]
    fn snapshot_counters_measure_present_loops_and_disabled_work_is_zero() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let snapshot = generated.at(&fixture.climate, WorldTime::GENESIS);
        eprintln!("waterworld snapshot counters: {:?}", snapshot.counters);
        assert_eq!(snapshot.counters.refresh, generated.substrate.len());
        assert_eq!(snapshot.counters.stock, generated.substrate.len());
        assert_eq!(snapshot.counters.candidate_ring, generated.vents.len());
        assert_eq!(
            snapshot.counters.propagation,
            snapshot.propagation.counters.propagation
        );
        assert_eq!(snapshot.counters.observation, 0);

        let disabled = waterworld_from(
            &fixture.world,
            &fixture.terrain,
            &fixture.climate,
            WaterWorldConfig { enabled: false },
        )
        .at(&fixture.climate, WorldTime::GENESIS);
        assert_eq!(disabled.counters, Default::default());
    }
}

mod purity {
    use super::*;
    use hornvale_worldgen::waterworld::WaterPropagation;

    #[test]
    fn snapshots_and_transport_are_reordered_query_pure() {
        let fixture = seed_42();
        let generated = active(&fixture);
        let stable = generated.clone();
        let time = WorldTime::from_ticks(47 * WorldTime::TICKS_PER_STD_DAY);
        let snapshot = generated.at(&fixture.climate, time);
        let local = snapshot
            .stocks
            .iter()
            .map(|stock| stock.local_source_influence)
            .collect::<Vec<_>>();
        let first = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &local,
            3,
            0.5,
        );
        let _other = generated.at(&fixture.climate, WorldTime::GENESIS);
        let repeated = WaterPropagation::transport(
            fixture.climate.geosphere(),
            &generated.substrate,
            &snapshot.fields,
            &local,
            3,
            0.5,
        );
        assert_eq!(first, repeated);
        assert_eq!(generated, stable);
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

mod observation {
    use super::*;

    #[test]
    fn snapshot_ordinary_reports_present_consequences_without_claiming_causes() {
        let fixture = seed_42();
        let source = active(&fixture);
        let snapshot = source.at(&fixture.climate, hornvale_kernel::WorldTime::GENESIS);

        let observation =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Habitat, false);

        assert!(observation.text.contains("marine substrate:"));
        assert!(observation.text.contains("present stocks:"));
        assert!(observation.text.contains("current transport:"));
        assert!(observation.text.contains("vent consequence:"));
        assert!(observation.text.contains("bloom"));
        assert!(observation.text.contains("nutrients"));
        assert!(observation.text.contains("reef/kelp"));
        assert!(!observation.text.contains("source phase"));
        assert!(!observation.text.contains("provenance"));
        assert!(!observation.text.contains("inferred cause"));
        assert_eq!(observation.counters.observation, source.substrate.len());
    }

    #[test]
    fn snapshot_diagnostic_labels_provenance_inference_and_zero_cases() {
        let fixture = seed_42();
        let mut source = active(&fixture);
        source.vents.truncate(3);
        source.vent_candidate_rings.truncate(3);
        for (vent, offset_days) in source.vents.iter_mut().zip([0_i64, 85, 20]) {
            vent.phase_offset_ticks = offset_days * hornvale_kernel::WorldTime::TICKS_PER_STD_DAY;
        }
        let snapshot = source.at(&fixture.climate, hornvale_kernel::WorldTime::GENESIS);
        assert_eq!(
            snapshot.vent_states,
            vec![VentState::Absent, VentState::Failed, VentState::Nascent]
        );

        let diagnostic =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Habitat, true);

        assert!(
            diagnostic
                .text
                .contains("source phase (derived, not directly observed):")
        );
        assert!(diagnostic.text.contains("provenance: stable vent source"));
        assert!(diagnostic.text.contains("local/transported split:"));
        assert!(
            diagnostic
                .text
                .contains("inferred cause; uncertain at observation scale")
        );
        assert!(
            diagnostic
                .text
                .contains("absent contribution (source remains admitted)")
        );
        assert!(
            diagnostic
                .text
                .contains("failed contribution (source and seabed remain present)")
        );
        assert!(
            diagnostic
                .text
                .contains("zero ambient baseline (measured value is zero)")
        );
        assert_eq!(
            diagnostic.counters.observation,
            source.substrate.len() + source.vents.len()
        );
    }

    #[test]
    fn snapshot_observation_is_exact_under_repetition_and_reordering() {
        let fixture = seed_42();
        let source = active(&fixture);
        let snapshot = source.at(&fixture.climate, hornvale_kernel::WorldTime::GENESIS);
        let source_before = source.clone();
        let snapshot_before = snapshot.clone();

        let ordinary_first =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Regional, false);
        let diagnostic_first =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Regional, true);
        let diagnostic_second =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Regional, true);
        let ordinary_second =
            observe_waterworld_snapshot(&source, &snapshot, WaterWorldDetail::Regional, false);

        assert_eq!(ordinary_first, ordinary_second);
        assert_eq!(diagnostic_first, diagnostic_second);
        assert_ne!(ordinary_first.text, diagnostic_first.text);
        assert_eq!(source, source_before);
        assert_eq!(snapshot, snapshot_before);
        assert!(ordinary_first.counters.observation > 0);
        assert!(diagnostic_first.counters.observation > ordinary_first.counters.observation);
    }
}
