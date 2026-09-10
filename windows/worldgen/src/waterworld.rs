//! Compact Waterworld projection over generated terrain and climate.

use hornvale_climate::{BiomeExpr, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Vertex, World, WorldTime};
use hornvale_terrain::landscape::FeatureId;
use hornvale_terrain::{BoundaryKind, GeneratedTerrain, WaterKind};

pub use crate::waterworld_propagation::{WaterPropagation, WaterTrajectorySample};

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
/// type-audit: bare-ok(index: id), bare-ok(ratio: strength), bare-ok(diagnostic-value: temperature_delta), bare-ok(ratio: chemistry)
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
/// type-audit: bare-ok(ratio: plankton), bare-ok(ratio: chemosynthetic_bloom), bare-ok(ratio: nutrients), bare-ok(ratio: kelp_reef)
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
    /// Bounded current and vertical propagation samples.
    pub propagation: WaterPropagation,
}

/// Dynamic Waterworld readout at one exact world instant.
///
/// Vent states are aligned one-for-one with [`WaterWorld::vents`]. The
/// stable substrate and source identities remain on [`WaterWorld`].
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterWorldSnapshot {
    /// Ambient fields aligned one-for-one with `WaterWorld::substrate`.
    pub fields: Vec<WaterFields>,
    /// Aggregate stocks aligned one-for-one with `WaterWorld::substrate`.
    pub stocks: Vec<WaterStocks>,
    /// Present states aligned one-for-one with `WaterWorld::vents`.
    pub vent_states: Vec<VentState>,
    /// Present bounded propagation readout.
    pub propagation: WaterPropagation,
}

impl WaterWorld {
    /// Read this stable overlay at an exact world instant.
    ///
    /// Stage 1 preserves the predecessor's genesis derivations byte-for-byte:
    /// the climate and time parameters establish the pure query boundary, but
    /// no temporal term is connected until Stage 2. This call draws no stream,
    /// mutates no source, and populates no cache.
    pub fn at(&self, _climate: &GeneratedClimate, _time: WorldTime) -> WaterWorldSnapshot {
        WaterWorldSnapshot {
            fields: self.fields.clone(),
            stocks: self.stocks.clone(),
            vent_states: vec![VentState::Active; self.vents.len()],
            propagation: self.propagation.clone(),
        }
    }
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
    let mut vents = Vec::new();
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
        vents.push(WaterVent {
            id: vents.len(),
            vertex: sample.vertex,
            strength,
            temperature_delta: 5.0 + stream.next_f64() * 95.0,
            chemistry: stream.next_f64(),
        });
    }
    let stocks = substrate
        .iter()
        .zip(&fields)
        .map(|(sample, field)| WaterStocks {
            plankton: (field.light / (field.light + 1.0)).clamp(0.0, 1.0),
            chemosynthetic_bloom: field.chemistry.clamp(0.0, 1.0),
            nutrients: ((sample.terrain_features.len() as f64) / 4.0).clamp(0.0, 1.0),
            kelp_reef: if sample.is_seabed && (field.temperature_c > -2.0) {
                1.0
            } else {
                0.0
            },
        })
        .collect::<Vec<_>>();
    let propagation = WaterPropagation::from_substrate(&substrate, &fields);
    WaterWorld {
        substrate,
        fields,
        stocks,
        vents,
        propagation,
    }
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
