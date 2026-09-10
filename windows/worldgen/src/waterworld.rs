//! Compact Waterworld projection over generated terrain and climate.

use hornvale_climate::{BiomeExpr, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Vertex, World, WorldTime};
use hornvale_terrain::landscape::FeatureId;
use hornvale_terrain::{BoundaryKind, GeneratedTerrain, WaterKind};

pub use crate::waterworld_propagation::{
    WaterPropagation, WaterTrajectorySample, WaterWorkCounters,
};

use crate::waterworld_propagation::{build_vent_candidate_ring, select_vent_position};

/// plumb: pending(wave-1)
const VENT_ABSENT_TICKS: i64 = 20 * WorldTime::TICKS_PER_STD_DAY;
/// plumb: pending(wave-1)
const VENT_NASCENT_TICKS: i64 = 15 * WorldTime::TICKS_PER_STD_DAY;
/// plumb: pending(wave-1)
const VENT_ACTIVE_TICKS: i64 = 30 * WorldTime::TICKS_PER_STD_DAY;
/// plumb: pending(wave-1)
const VENT_WEAKENING_TICKS: i64 = 20 * WorldTime::TICKS_PER_STD_DAY;
/// plumb: pending(wave-1)
const VENT_FAILED_TICKS: i64 = 15 * WorldTime::TICKS_PER_STD_DAY;
/// plumb: pending(wave-1)
const VENT_CYCLE_TICKS: i64 = VENT_ABSENT_TICKS
    + VENT_NASCENT_TICKS
    + VENT_ACTIVE_TICKS
    + VENT_WEAKENING_TICKS
    + VENT_FAILED_TICKS;
/// plumb: pending(wave-1)
const TRANSPORT_HOP_LIMIT: usize = 3;
/// plumb: pending(wave-1)
const TRANSPORT_ATTENUATION: f64 = 0.5;

/// Configuration for the compact Waterworld overlay.
/// type-audit: bare-ok(flag: enabled)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WaterWorldConfig {
    /// Whether the derived overlay is present.
    pub enabled: bool,
}

/// One sampled position in an existing marine column.
/// type-audit: bare-ok(flag: is_seabed), bare-ok(flag: has_edifice), bare-ok(diagnostic-value: depth_m)
#[derive(Clone, Debug, PartialEq)]
pub struct WaterSubstrate {
    /// Stable surface vertex owning this column.
    pub vertex: Vertex,
    /// Stable vertex used by rendering and later adjacency.
    pub render_vertex: Vertex,
    /// Whether this sample is the column's terminal seafloor expression.
    pub is_seabed: bool,
    /// Existing terrain water classification.
    pub water_kind: WaterKind,
    /// Sample depth below sea level, in metres.
    pub depth_m: f64,
    /// Existing marine depth band.
    pub depth_band: Stratum,
    /// Existing community expression at this band.
    pub biome_expr: BiomeExpr,
    /// Existing terrain boundary beneath this column, when present.
    pub seafloor_boundary: Option<BoundaryKind>,
    /// Whether terrain derives a volcanic edifice at this surface vertex.
    pub has_edifice: bool,
    /// Existing terrain features whose extent contains this vertex.
    pub terrain_features: Vec<FeatureId>,
}

/// Ambient fields sampled at one existing marine substrate position.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(ratio: light), bare-ok(diagnostic-value: pressure), bare-ok(diagnostic-value: temperature_c), bare-ok(diagnostic-value: salinity), bare-ok(ratio: chemistry), bare-ok(diagnostic-value: current)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WaterFields {
    /// Depth below the water surface, in metres.
    /// type-audit: bare-ok(diagnostic-value: depth_m)
    pub depth_m: f64,
    /// Existing marine depth band.
    pub depth_band: Stratum,
    /// Light remaining after depth attenuation.
    /// type-audit: bare-ok(ratio: light)
    pub light: f64,
    /// Relative pressure, with one atmosphere at the surface.
    /// type-audit: bare-ok(diagnostic-value: pressure)
    pub pressure: f64,
    /// Temperature at genesis, in Celsius.
    /// type-audit: bare-ok(diagnostic-value: temperature_c)
    pub temperature_c: f64,
    /// Deterministic salinity proxy, in practical salinity units.
    /// type-audit: bare-ok(diagnostic-value: salinity)
    pub salinity: f64,
    /// Local chemical-source indicator; vents refine this in the next pass.
    /// type-audit: bare-ok(ratio: chemistry)
    pub chemistry: f64,
    /// Existing climate current vector.
    /// type-audit: bare-ok(diagnostic-value: current)
    pub current: [f64; 3],
}

impl WaterFields {
    /// Derive fields from one substrate sample and the live ambient sources.
    /// type-audit: bare-ok(diagnostic-value: insolation), bare-ok(diagnostic-value: temperature_c), bare-ok(diagnostic-value: current)
    pub fn from_substrate(
        substrate: &WaterSubstrate,
        insolation: f64,
        temperature_c: f64,
        current: [f64; 3],
    ) -> Self {
        Self::from_sources(
            substrate,
            insolation,
            temperature_c,
            35.0 + substrate.depth_m / 10_000.0,
            if substrate.has_edifice { 1.0 } else { 0.0 },
            current,
        )
    }

    /// Derive fields from explicit ambient sources; used to prove source isolation.
    /// type-audit: bare-ok(diagnostic-value: insolation), bare-ok(diagnostic-value: temperature_c), bare-ok(diagnostic-value: salinity), bare-ok(ratio: chemistry), bare-ok(diagnostic-value: current)
    pub fn from_sources(
        substrate: &WaterSubstrate,
        insolation: f64,
        temperature_c: f64,
        salinity: f64,
        chemistry: f64,
        current: [f64; 3],
    ) -> Self {
        let depth_m = substrate.depth_m;
        Self {
            depth_m,
            depth_band: substrate.depth_band,
            light: insolation * hornvale_kernel::math::exp(-depth_m / 1_000.0),
            pressure: 1.0 + depth_m / 10.0,
            temperature_c,
            salinity,
            chemistry,
            current,
        }
    }
}

/// A sparse localized hydrothermal source, separate from ambient chemistry.
///
/// Every field here is stable source data. Temporal state belongs to a
/// [`WaterWorldSnapshot`], never to the admitted source.
/// type-audit: bare-ok(index: id), bare-ok(ratio: strength), bare-ok(diagnostic-value: temperature_delta), bare-ok(ratio: chemistry), bare-ok(count: phase_offset_ticks)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WaterVent {
    /// Stable vent ordinal in seabed vertex order.
    /// type-audit: bare-ok(index: id)
    pub id: usize,
    /// Seabed vertex hosting the source.
    pub vertex: Vertex,
    /// Positive source strength.
    /// type-audit: bare-ok(ratio: strength)
    pub strength: f64,
    /// Local temperature delta in Celsius.
    /// type-audit: bare-ok(diagnostic-value: temperature_delta)
    pub temperature_delta: f64,
    /// Local chemical availability.
    /// type-audit: bare-ok(ratio: chemistry)
    pub chemistry: f64,
    /// Stable source-keyed offset into the fixed succession cycle.
    /// type-audit: bare-ok(count: phase_offset_ticks)
    pub phase_offset_ticks: i64,
}

/// Present phase of one stable vent source.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VentState {
    /// The source has no present hydrothermal contribution.
    Absent,
    /// The source is beginning a new active interval.
    Nascent,
    /// The source is at full activity.
    Active,
    /// The source contribution is declining.
    Weakening,
    /// The source remains identifiable after its contribution has failed.
    Failed,
}

/// Bounded aggregate environmental stocks; no individual organisms are stored.
/// type-audit: bare-ok(ratio: plankton), bare-ok(ratio: chemosynthetic_bloom), bare-ok(ratio: nutrients), bare-ok(ratio: kelp_reef), bare-ok(ratio: local_source_influence), bare-ok(ratio: transported_influence)
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct WaterStocks {
    /// Photic plankton availability.
    pub plankton: f64,
    /// Chemosynthetic bloom availability.
    pub chemosynthetic_bloom: f64,
    /// Dissolved nutrient reserve.
    pub nutrients: f64,
    /// Kelp/reef substrate suitability.
    pub kelp_reef: f64,
    /// Aggregate influence produced at this exact substrate sample.
    pub local_source_influence: f64,
    /// Aggregate influence arriving from bounded current transport.
    pub transported_influence: f64,
}

/// Read-only generated Waterworld state.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterWorld {
    /// Existing marine substrate projected in stable vertex/column order.
    pub substrate: Vec<WaterSubstrate>,
    /// Ambient fields aligned one-for-one with `substrate`.
    pub fields: Vec<WaterFields>,
    /// Aggregate stocks aligned one-for-one with `substrate`.
    pub stocks: Vec<WaterStocks>,
    /// Sparse derived vent sources in stable seabed order.
    pub vents: Vec<WaterVent>,
    /// Ordered, bounded marine influence candidates aligned with `vents`.
    pub vent_candidate_rings: Vec<Vec<Vertex>>,
    /// Bounded current and vertical propagation samples.
    pub propagation: WaterPropagation,
    /// Work performed while building stable candidate rings.
    pub counters: WaterWorkCounters,
}

/// Dynamic Waterworld readout at one exact world instant.
///
/// Vent states are aligned one-for-one with [`WaterWorld::vents`]. The
/// stable substrate and source identities remain on [`WaterWorld`].
/// type-audit: bare-ok(ratio: vent_phase_positions)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterWorldSnapshot {
    /// Ambient fields aligned one-for-one with `WaterWorld::substrate`.
    pub fields: Vec<WaterFields>,
    /// Aggregate stocks aligned one-for-one with `WaterWorld::substrate`.
    pub stocks: Vec<WaterStocks>,
    /// Present states aligned one-for-one with `WaterWorld::vents`.
    pub vent_states: Vec<VentState>,
    /// Continuous position within each present state, in `[0, 1)`.
    pub vent_phase_positions: Vec<f64>,
    /// At most one present influence vertex for each stable vent source.
    pub vent_positions: Vec<Option<Vertex>>,
    /// Present bounded propagation readout.
    pub propagation: WaterPropagation,
    /// Work performed by the finite snapshot loops.
    pub counters: WaterWorkCounters,
}

impl WaterWorld {
    /// Read this stable overlay at an exact world instant.
    ///
    /// Succession uses exact integer ticks. The five finite intervals are,
    /// in spec section 3.2 order: 20 days absent, 15 nascent, 30 active,
    /// 20 weakening, and 15 failed. This call draws no stream, mutates no
    /// source, and populates no cache.
    pub fn at(&self, climate: &GeneratedClimate, time: WorldTime) -> WaterWorldSnapshot {
        if self.substrate.is_empty() {
            return WaterWorldSnapshot::default();
        }
        assert_eq!(
            self.vents.len(),
            self.vent_candidate_rings.len(),
            "Waterworld vents and candidate rings must remain aligned"
        );
        let mut counters = WaterWorkCounters {
            candidate_ring: self.counters.candidate_ring,
            ..WaterWorkCounters::default()
        };
        let mut fields = Vec::with_capacity(self.substrate.len());
        for sample in &self.substrate {
            counters.refresh += 1;
            fields.push(WaterFields::from_substrate(
                sample,
                climate.insolation(),
                climate.temperature_at(sample.vertex, time).get(),
                climate.current_at(sample.vertex),
            ));
        }
        let mut vent_states = Vec::with_capacity(self.vents.len());
        let mut vent_phase_positions = Vec::with_capacity(self.vents.len());
        let mut vent_positions = Vec::with_capacity(self.vents.len());
        let mut local_influence = vec![0.0_f64; self.substrate.len()];
        for (vent, ring) in self.vents.iter().zip(&self.vent_candidate_rings) {
            let phase = vent_phase(vent, time);
            let position = select_vent_position(ring, phase.state, phase.cycle_index);
            if let Some(vertex) = position {
                let sample_index = seabed_sample_index(&self.substrate, vertex)
                    .expect("a vent candidate ring contains only marine seabed vertices");
                let local_strength = vent.strength * phase.local_strength;
                fields[sample_index].temperature_c += vent.temperature_delta * local_strength;
                let chemistry = (vent.chemistry * local_strength).clamp(0.0, 1.0);
                fields[sample_index].chemistry +=
                    (1.0 - fields[sample_index].chemistry) * chemistry;
                local_influence[sample_index] =
                    (local_influence[sample_index] + local_strength).clamp(0.0, 1.0);
            }
            vent_states.push(phase.state);
            vent_phase_positions.push(phase.position);
            vent_positions.push(position);
        }
        let propagation = WaterPropagation::transport(
            climate.geosphere(),
            &self.substrate,
            &fields,
            &local_influence,
            TRANSPORT_HOP_LIMIT,
            TRANSPORT_ATTENUATION,
        );
        let mut stocks = Vec::with_capacity(self.substrate.len());
        for (((sample, field), local), &transported) in self
            .substrate
            .iter()
            .zip(&fields)
            .zip(local_influence)
            .zip(&propagation.transported_influence)
        {
            counters.stock += 1;
            stocks.push(derive_stocks(sample, field, local, transported));
        }
        counters.propagation = propagation.counters.propagation;
        WaterWorldSnapshot {
            fields,
            stocks,
            vent_states,
            vent_phase_positions,
            vent_positions,
            propagation,
            counters,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct VentPhase {
    state: VentState,
    position: f64,
    local_strength: f64,
    cycle_index: i64,
}

fn vent_phase(vent: &WaterVent, time: WorldTime) -> VentPhase {
    let shifted_ticks = i128::from(time.ticks()) + i128::from(vent.phase_offset_ticks);
    let cycle_ticks = i128::from(VENT_CYCLE_TICKS);
    let cycle_index = shifted_ticks.div_euclid(cycle_ticks) as i64;
    let tick = shifted_ticks.rem_euclid(cycle_ticks) as i64;
    let absent_end = VENT_ABSENT_TICKS;
    let nascent_end = absent_end + VENT_NASCENT_TICKS;
    let active_end = nascent_end + VENT_ACTIVE_TICKS;
    let weakening_end = active_end + VENT_WEAKENING_TICKS;

    if tick < absent_end {
        phase(VentState::Absent, tick, VENT_ABSENT_TICKS, 0.0, cycle_index)
    } else if tick < nascent_end {
        let local = tick - absent_end;
        let position = local as f64 / VENT_NASCENT_TICKS as f64;
        VentPhase {
            state: VentState::Nascent,
            position,
            local_strength: 0.25 + 0.75 * position,
            cycle_index,
        }
    } else if tick < active_end {
        phase(
            VentState::Active,
            tick - nascent_end,
            VENT_ACTIVE_TICKS,
            1.0,
            cycle_index,
        )
    } else if tick < weakening_end {
        let local = tick - active_end;
        let position = local as f64 / VENT_WEAKENING_TICKS as f64;
        VentPhase {
            state: VentState::Weakening,
            position,
            local_strength: 1.0 - 0.8 * position,
            cycle_index,
        }
    } else {
        phase(
            VentState::Failed,
            tick - weakening_end,
            VENT_FAILED_TICKS,
            0.0,
            cycle_index,
        )
    }
}

fn phase(
    state: VentState,
    local_tick: i64,
    duration_ticks: i64,
    local_strength: f64,
    cycle_index: i64,
) -> VentPhase {
    VentPhase {
        state,
        position: local_tick as f64 / duration_ticks as f64,
        local_strength,
        cycle_index,
    }
}

fn seabed_sample_index(substrate: &[WaterSubstrate], vertex: Vertex) -> Option<usize> {
    let start = substrate.partition_point(|sample| sample.vertex < vertex);
    substrate[start..]
        .iter()
        .take_while(|sample| sample.vertex == vertex)
        .position(|sample| sample.is_seabed)
        .map(|offset| start + offset)
}

/// Composition-root entry point for the Waterworld overlay.
pub fn waterworld_from(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    config: WaterWorldConfig,
) -> WaterWorld {
    if !config.enabled {
        return WaterWorld::default();
    }
    assert_eq!(
        terrain.geosphere().vertex_count(),
        climate.geosphere().vertex_count(),
        "Waterworld terrain and climate must share one vertex space"
    );

    let mut features_at = vec![Vec::new(); terrain.geosphere().vertex_count()];
    for feature in terrain.features().all() {
        for &vertex in &feature.extent {
            features_at[vertex.0 as usize].push(feature.id);
        }
    }

    let mut substrate = Vec::new();
    for vertex in terrain.geosphere().vertices() {
        if terrain.water_kind_at(vertex) != WaterKind::Ocean {
            continue;
        }
        let floor_expr = climate.biome_expr_at(vertex);
        assert_eq!(
            floor_expr.realm,
            Realm::WATERWORLD,
            "ocean terrain must have a Waterworld climate column at {vertex:?}"
        );
        let column = climate.strata_at(vertex);
        let seabed_depth_m =
            (terrain.sea_level().get() - terrain.elevation_at(vertex).get()).max(0.0);
        let last = column.len() - 1;
        for (index, depth_band) in column.into_iter().enumerate() {
            let biome_expr = climate
                .biome_expr_at_stratum(vertex, depth_band)
                .expect("a stratum returned by strata_at is present");
            let is_seabed = index == last;
            substrate.push(WaterSubstrate {
                vertex,
                render_vertex: vertex,
                is_seabed,
                water_kind: WaterKind::Ocean,
                depth_m: if is_seabed {
                    seabed_depth_m
                } else {
                    band_entry_depth_m(depth_band)
                },
                depth_band,
                biome_expr,
                seafloor_boundary: terrain.boundary_at(vertex).map(|boundary| boundary.kind),
                has_edifice: terrain.has_edifice(vertex),
                terrain_features: features_at[vertex.0 as usize].clone(),
            });
        }
    }
    let fields = substrate
        .iter()
        .map(|sample| {
            WaterFields::from_substrate(
                sample,
                climate.insolation(),
                climate
                    .temperature_at(sample.vertex, hornvale_kernel::WorldTime::GENESIS)
                    .get(),
                climate.current_at(sample.vertex),
            )
        })
        .collect::<Vec<_>>();
    let marine_vertices = substrate
        .iter()
        .filter(|sample| sample.is_seabed)
        .map(|sample| sample.vertex)
        .collect::<Vec<_>>();
    let mut vents = Vec::new();
    let mut vent_candidate_rings = Vec::new();
    let mut candidate_ring_count = 0;
    for sample in substrate.iter().filter(|sample| sample.is_seabed) {
        let source_exists = sample.has_edifice || sample.seafloor_boundary.is_some();
        if !source_exists {
            continue;
        }
        let key = format!("vertex/{}", sample.vertex.0);
        let mut stream = world
            .seed
            .derive(crate::streams::WATERWORLD_VENT)
            .derive(StreamLabel::dynamic(&key))
            .stream();
        let admission = stream.next_f64();
        if admission >= 0.25 {
            continue;
        }
        let strength = 0.25 + stream.next_f64() * 0.75;
        let temperature_delta = 5.0 + stream.next_f64() * 95.0;
        let chemistry = stream.next_f64();
        vents.push(WaterVent {
            id: vents.len(),
            vertex: sample.vertex,
            strength,
            temperature_delta,
            chemistry,
            phase_offset_ticks: vent_phase_offset_ticks(strength, temperature_delta, chemistry),
        });
        vent_candidate_rings.push(build_vent_candidate_ring(
            terrain.geosphere(),
            &marine_vertices,
            sample.vertex,
            &mut candidate_ring_count,
        ));
    }
    let stocks = substrate
        .iter()
        .zip(&fields)
        .map(|(sample, field)| derive_stocks(sample, field, 0.0, 0.0))
        .collect::<Vec<_>>();
    let propagation = WaterPropagation::from_substrate(&substrate, &fields);
    WaterWorld {
        substrate,
        fields,
        stocks,
        vents,
        vent_candidate_rings,
        propagation,
        counters: WaterWorkCounters {
            candidate_ring: candidate_ring_count,
            ..WaterWorkCounters::default()
        },
    }
}

fn derive_stocks(
    sample: &WaterSubstrate,
    field: &WaterFields,
    local_source_influence: f64,
    transported_influence: f64,
) -> WaterStocks {
    let plankton = (field.light / (field.light + 1.0)).clamp(0.0, 1.0);
    let chemosynthetic_bloom = field.chemistry.clamp(0.0, 1.0);
    let terrain_nutrients = (sample.terrain_features.len() as f64 / 4.0).clamp(0.0, 1.0);
    let nutrients =
        (terrain_nutrients + 0.35 * local_source_influence + 0.2 * transported_influence)
            .clamp(0.0, 1.0);
    let thermal_suitability = (1.0 - (field.temperature_c - 15.0).abs() / 40.0).clamp(0.0, 1.0);
    let chemistry_suitability = (1.0 - (field.chemistry - 0.35).abs() / 0.65).clamp(0.0, 1.0);
    let kelp_reef = if sample.is_seabed {
        (0.35 * thermal_suitability + 0.25 * chemistry_suitability + 0.25 * nutrients + 0.15)
            .clamp(0.0, 1.0)
    } else {
        0.0
    };
    WaterStocks {
        plankton,
        chemosynthetic_bloom,
        nutrients,
        kelp_reef,
        local_source_influence,
        transported_influence,
    }
}

/// Derive a cycle offset from the three existing seeded vent values without
/// extending or reordering `WATERWORLD_VENT` consumption.
fn vent_phase_offset_ticks(strength: f64, temperature_delta: f64, chemistry: f64) -> i64 {
    let mixed = strength.to_bits()
        ^ temperature_delta.to_bits().rotate_left(21)
        ^ chemistry.to_bits().rotate_left(42);
    (mixed % VENT_CYCLE_TICKS as u64) as i64
}

/// Shallow edge of each marine band, using the exact thresholds documented by
/// `hornvale_climate::Stratum::at_depth_m`. The terminal sample uses the
/// terrain-derived seabed depth instead; this proxy locates only open-column
/// samples where climate exposes a band but no inverse depth accessor.
fn band_entry_depth_m(stratum: Stratum) -> f64 {
    match stratum {
        Stratum::Epipelagic => 0.0,
        Stratum::Mesopelagic => 200.0,
        Stratum::Bathypelagic => 1_000.0,
        Stratum::Abyssal => 4_000.0,
        Stratum::Hadal => 6_000.0,
        Stratum::Surface | Stratum::Rock(_) => {
            unreachable!("a Waterworld column contains only marine strata")
        }
    }
}
