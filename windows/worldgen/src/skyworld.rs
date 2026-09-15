//! The compact Skyworld overlay.
//!
//! This module owns the cross-domain draw because coverage combines terrain,
//! climate, and astronomy inputs. It leaves the fixed surface biome maps
//! untouched. Aggregate orchard stocks, sampled trajectories, temporal
//! adjacency, and bounded propagation remain sparse additions to that surface.

use hornvale_climate::GeneratedClimate;
use hornvale_kernel::math;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Stream, Vertex, World};
use hornvale_terrain::GeneratedTerrain;

pub use crate::skyworld_propagation::{
    SkyAdjacency, SkyCorridor, SkyCorridorKind, SkyEvent, SkyEventKind, SkyPropagation,
    SkyPropagationDetail, SkyTrajectorySample,
};

/// plumb: pending(wave-1)
const HARD_COVERAGE_FRACTION: f64 = 0.10;
/// plumb: pending(wave-1)
const PRESSURE_SCALE_M: f64 = 8_500.0;
/// plumb: pending(wave-1)
const DENSITY_SCALE_M: f64 = 8_500.0;
/// plumb: pending(wave-1)
const MOISTURE_SCALE_M: f64 = 6_500.0;
/// plumb: pending(wave-1)
const RADIATION_SCALE_M: f64 = 6_000.0;
/// plumb: pending(wave-1)
const AETHER_SCALE_M: f64 = 4_500.0;
/// plumb: pending(wave-1)
const WIND_SCALE_M: f64 = 12_000.0;
/// plumb: pending(wave-1)
const SEA_LEVEL_RADIATION_FRACTION: f64 = 0.05;
/// plumb: pending(wave-1)
const SEA_LEVEL_AETHER_FRACTION: f64 = 0.15;
/// plumb: pending(wave-1)
const ORCHARD_ROOT_FRACTION: f64 = 0.80;
/// plumb: pending(wave-1)
const ORCHARD_SOIL_FRACTION: f64 = 0.75;
/// plumb: pending(wave-1)
const ORCHARD_CANOPY_FRACTION: f64 = 0.80;
/// plumb: pending(wave-1)
const ORCHARD_FLOWER_FRACTION: f64 = 0.70;
/// plumb: pending(wave-1)
const ORCHARD_POLLINATION_FRACTION: f64 = 0.60;
/// plumb: pending(wave-1)
const ORCHARD_FRUIT_FRACTION: f64 = 0.50;
/// plumb: pending(wave-1)
const ORCHARD_DETRITUS_FRACTION: f64 = 0.20;
/// plumb: pending(wave-1)
const ORCHARD_SEED_SPORE_FRACTION: f64 = 0.40;
/// plumb: pending(wave-1)
const ORCHARD_ANIMAL_FORAGE_FRACTION: f64 = 0.65;

/// Configuration for the compact Skyworld generation slice.
/// type-audit: bare-ok(ratio: max_projected_fraction), bare-ok(count: trajectory_samples), bare-ok(count: propagation_radius)
#[derive(Clone, Debug, PartialEq)]
pub struct SkyWorldConfig {
    /// Requested projected-vertex fraction, bounded by the global ten-percent
    /// Skyworld ceiling.
    pub max_projected_fraction: f64,
    /// Number of coarse trajectory samples to materialize.
    pub trajectory_samples: u16,
    /// Number of local influence rings beyond the exchange envelope.
    pub propagation_radius: u16,
}

/// A stable atmospheric and forcing profile at a chosen altitude.
/// type-audit: bare-ok(diagnostic-value: altitude_m), bare-ok(ratio: pressure), bare-ok(ratio: density), bare-ok(diagnostic-value: temperature_c), bare-ok(diagnostic-value: lapse_rate_c_per_km), bare-ok(ratio: high_sky_radiation), bare-ok(ratio: aether), bare-ok(ratio: moisture), bare-ok(diagnostic-value: wind), bare-ok(ratio: wind_shear), bare-ok(ratio: lunar_forcing), bare-ok(ratio: stellar_forcing)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SkyFields {
    /// Altitude of this sample above the surface, metres.
    pub altitude_m: f64,
    /// Relative atmospheric pressure.
    pub pressure: f64,
    /// Relative atmospheric density.
    pub density: f64,
    /// Temperature proxy, degrees Celsius.
    pub temperature_c: f64,
    /// Temperature lapse rate, degrees Celsius per kilometre.
    pub lapse_rate_c_per_km: f64,
    /// High-sky radiation availability.
    pub high_sky_radiation: f64,
    /// Luminiferous-aether availability.
    pub aether: f64,
    /// Moisture availability.
    pub moisture: f64,
    /// Coarse wind vector.
    pub wind: [f64; 3],
    /// Coarse wind-shear magnitude.
    pub wind_shear: f64,
    /// Lunar forcing proxy.
    pub lunar_forcing: f64,
    /// Stellar forcing proxy.
    pub stellar_forcing: f64,
}

impl SkyFields {
    /// Derive the same stable profile at another non-negative altitude.
    /// type-audit: bare-ok(diagnostic-value: altitude_m)
    pub fn at_altitude(&self, altitude_m: f64) -> SkyFields {
        let altitude_m = altitude_m.max(0.0);
        let delta = altitude_m - self.altitude_m;
        let radiation_ratio =
            radiation_transmission(altitude_m) / radiation_transmission(self.altitude_m);
        let aether_ratio = aether_profile(altitude_m) / aether_profile(self.altitude_m);
        let pressure_factor = math::exp(-delta / PRESSURE_SCALE_M);
        let density_factor = math::exp(-delta / DENSITY_SCALE_M);
        let moisture_factor = math::exp(-delta / MOISTURE_SCALE_M);
        let wind_ratio = wind_profile(altitude_m) / wind_profile(self.altitude_m);
        SkyFields {
            altitude_m,
            pressure: self.pressure * pressure_factor,
            density: self.density * density_factor,
            temperature_c: self.temperature_c - self.lapse_rate_c_per_km * delta / 1_000.0,
            lapse_rate_c_per_km: self.lapse_rate_c_per_km,
            high_sky_radiation: self.high_sky_radiation * radiation_ratio,
            aether: self.aether * aether_ratio,
            moisture: self.moisture * moisture_factor,
            wind: [
                self.wind[0] * wind_ratio,
                self.wind[1] * wind_ratio,
                self.wind[2] * wind_ratio,
            ],
            wind_shear: (self.wind_shear * wind_ratio).max(0.0),
            lunar_forcing: self.lunar_forcing,
            stellar_forcing: self.stellar_forcing,
        }
    }
}

/// The sky substrate axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkySubstrate {
    /// Condensed cloud substrate.
    Cloud,
    /// Organic microbial mat.
    OrganicMat,
    /// Rooted soil.
    Soil,
    /// Frozen substrate.
    Ice,
    /// Volcanic ash.
    Ash,
    /// Mineral substrate.
    Mineral,
    /// A mixed substrate.
    Mixed,
}

/// The sky energy axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyEnergy {
    /// Sunlight.
    Sunlight,
    /// Luminiferous aether.
    Aether,
    /// High-sky radiation.
    HighSkyRadiation,
    /// Storm charge.
    StormCharge,
    /// Thaumic flow.
    ThaumicFlow,
}

/// The sky water axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyWater {
    /// Little available water.
    Dry,
    /// Humid air.
    Humid,
    /// Cloud-fed water.
    CloudFed,
    /// Rain-fed water.
    RainFed,
    /// Saturated water.
    Saturated,
}

/// The movement axis of a sky territory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyMobility {
    /// Primarily free drift.
    Drifting,
    /// A repeating movement pattern.
    Cyclic,
    /// Movement following a current.
    CurrentFollowing,
    /// Movement following a field.
    FieldFollowing,
    /// A combination of drivers.
    Mixed,
}

/// The structural stability axis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyStability {
    /// Strong and inflexible.
    Rigid,
    /// Able to bend and recover.
    Flexible,
    /// Able to regenerate.
    Regenerative,
    /// Prone to brittle failure.
    Brittle,
}

/// The exchange relationship with the surface.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyExchangeMode {
    /// Little exchange with other habitats.
    Isolated,
    /// Exchange with the surface projection.
    SurfaceLinked,
    /// Exchange along a corridor.
    CorridorLinked,
    /// Exchange across an archipelago.
    Archipelagic,
}

/// The ecological structure of a territory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyEcology {
    /// Bare substrate.
    Bare,
    /// Microbial structure.
    Microbial,
    /// Rooted structure.
    Rooted,
    /// Wooded structure.
    Wooded,
    /// Orchard-bearing structure.
    OrchardBearing,
}

/// The descriptive lifecycle stage retained by a territory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyLifecycle {
    /// A wind-fed bloom.
    WindBloom,
    /// An adhesive microbial mat.
    AdhesiveMat,
    /// A buoyant raft.
    BuoyantRaft,
    /// A pioneer islet.
    PioneerIslet,
    /// A rooted island.
    RootedIsland,
    /// A mature orchard.
    MatureOrchard,
}

/// Independent phenotype axes and multidimensional stability traits.
/// type-audit: bare-ok(ratio: buoyancy), bare-ok(ratio: cohesion), bare-ok(ratio: flexibility), bare-ok(ratio: recovery)
#[derive(Clone, Debug, PartialEq)]
pub struct SkyPhenotype {
    /// Substrate composition.
    pub substrate: SkySubstrate,
    /// Dominant energy source.
    pub energy: SkyEnergy,
    /// Water relationship.
    pub water: SkyWater,
    /// Movement regime.
    pub mobility: SkyMobility,
    /// Structural stability.
    pub stability: SkyStability,
    /// Exchange regime.
    pub exchange: SkyExchangeMode,
    /// Ecological structure.
    pub ecology: SkyEcology,
    /// Buoyancy characteristic.
    pub buoyancy: f64,
    /// Cohesion characteristic.
    pub cohesion: f64,
    /// Flexibility characteristic.
    pub flexibility: f64,
    /// Recovery characteristic.
    pub recovery: f64,
}

/// A lineage's origin and descriptive present stage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkyLineage {
    /// The retained origin stage.
    pub origin: SkyLifecycle,
    /// The current descriptive stage.
    pub current: SkyLifecycle,
}

/// Fixed-vector ecological stocks for a sky territory.
///
/// Aether, high-sky radiation, and moisture are ambient
/// [`hornvale_kernel::ecology::ResourceKind::Field`] prerequisites. Every
/// quantity here is a bounded [`hornvale_kernel::ecology::ResourceKind::Stock`]
/// derived from those fields and the preceding aggregate stock; no individual
/// organism is materialized.
/// type-audit: bare-ok(ratio: plankton), bare-ok(ratio: root_support), bare-ok(ratio: soil_fertility), bare-ok(ratio: canopy_biomass), bare-ok(ratio: flowers), bare-ok(ratio: cloud_water), bare-ok(ratio: pollination), bare-ok(ratio: fruit), bare-ok(ratio: detritus), bare-ok(ratio: seed_spore_reserve), bare-ok(ratio: animal_forage)
#[derive(Clone, Debug, PartialEq)]
pub struct SkyStocks {
    /// Sky-plankton productivity stock.
    pub plankton: f64,
    /// Fungal and root support.
    pub root_support: f64,
    /// Soil fertility.
    pub soil_fertility: f64,
    /// Canopy biomass.
    pub canopy_biomass: f64,
    /// Flower stock supported by the canopy.
    pub flowers: f64,
    /// Cloud water.
    pub cloud_water: f64,
    /// Pollination capacity supported by flowers.
    pub pollination: f64,
    /// Fruit stock.
    pub fruit: f64,
    /// Dead organic matter available to decomposers.
    pub detritus: f64,
    /// Combined seed and spore reserve.
    pub seed_spore_reserve: f64,
    /// Aggregate animal forage.
    pub animal_forage: f64,
}

impl SkyStocks {
    /// Derive a mature orchard's stock chain from its three visible ambient
    /// field prerequisites.
    pub fn from_orchard_fields(fields: &SkyFields) -> SkyStocks {
        stocks_from_fields(fields, true)
    }
}

/// A position in the additive sky layer.
/// type-audit: bare-ok(diagnostic-value: altitude_m)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SkyPosition {
    /// The fixed surface vertex below the position.
    pub surface: Vertex,
    /// Altitude above that surface, metres.
    pub altitude_m: f64,
}

/// A sorted physical or exchange footprint over fixed surface vertices.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SkyFootprint {
    /// Surface vertices in ascending vertex order.
    pub projected: Vec<Vertex>,
}

/// One compact mobile territory in the additive sky layer.
/// type-audit: bare-ok(identifier-text: id)
#[derive(Clone, Debug, PartialEq)]
pub struct SkyTerritory {
    /// Stable identity derived from its origin surface vertex.
    pub id: u32,
    /// Lineage metadata.
    pub lineage: SkyLineage,
    /// Independent phenotype axes.
    pub phenotype: SkyPhenotype,
    /// Initial ecological stocks.
    pub stocks: SkyStocks,
    /// Genesis position.
    pub origin: SkyPosition,
    /// Requested coarse trajectory samples in ascending time order.
    pub trajectory: Vec<SkyTrajectorySample>,
    /// Physical body footprint.
    pub physical: SkyFootprint,
    /// Genesis exchange footprint.
    pub exchange: SkyFootprint,
    /// Explicit local, corridor, and event influence channels.
    pub influence: SkyPropagation,
}

/// The generated Skyworld overlay.
#[derive(Clone, Debug, PartialEq)]
pub struct SkyWorld {
    /// Stable world-level atmospheric fields at the surface reference band.
    pub fields: SkyFields,
    /// Territories in ascending stable identity order.
    pub territories: Vec<SkyTerritory>,
}

/// Test-only count of work performed at Skyworld's generation and raster loops.
///
/// This records loop bodies, rather than retained `SkyWorld` state, so the
/// bounded-work probes also reject work that a future implementation might
/// discard before returning.
#[cfg(test)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct SkyWorldWork {
    pub(crate) surface_vertices: usize,
    pub(crate) territories: usize,
    pub(crate) trajectory_samples: usize,
    pub(crate) rasters: usize,
    pub(crate) raster_pixels: usize,
    pub(crate) propagation_indexes: usize,
    pub(crate) movement_candidates: usize,
    pub(crate) physical_neighbors: usize,
    pub(crate) expansion_neighbors: usize,
    pub(crate) adjacency_pairs: usize,
    pub(crate) render_surface_vertices: usize,
    pub(crate) render_indexes: usize,
    pub(crate) overlay_marks: usize,
    pub(crate) occupied_vertices: usize,
    pub(crate) stamp_candidates: usize,
}

#[cfg(test)]
thread_local! {
    static SKYWORLD_WORK: std::cell::Cell<SkyWorldWork> = const { // lexicon: std::cell::Cell diagnostic counter idiom, not the mesh sense
        std::cell::Cell::new(SkyWorldWork { // lexicon: std::cell::Cell diagnostic counter idiom, not the mesh sense
            surface_vertices: 0,
            territories: 0,
            trajectory_samples: 0,
            rasters: 0,
            raster_pixels: 0,
            propagation_indexes: 0,
            movement_candidates: 0,
            physical_neighbors: 0,
            expansion_neighbors: 0,
            adjacency_pairs: 0,
            render_surface_vertices: 0,
            render_indexes: 0,
            overlay_marks: 0,
            occupied_vertices: 0,
            stamp_candidates: 0,
        })
    };
}

#[cfg(test)]
pub(crate) fn reset_skyworld_work() {
    SKYWORLD_WORK.with(|work| work.set(SkyWorldWork::default()));
}

#[cfg(test)]
pub(crate) fn skyworld_work() -> SkyWorldWork {
    SKYWORLD_WORK.with(|work| work.get())
}

#[cfg(test)]
pub(crate) fn record_skyworld_work(update: impl FnOnce(&mut SkyWorldWork)) {
    SKYWORLD_WORK.with(|work| {
        let mut current = work.get();
        update(&mut current);
        work.set(current);
    });
}

#[cfg(test)]
fn record_surface_vertex() {
    record_skyworld_work(|work| work.surface_vertices += 1);
}

#[cfg(test)]
fn record_territory() {
    record_skyworld_work(|work| work.territories += 1);
}

#[cfg(test)]
pub(crate) fn record_trajectory_sample() {
    record_skyworld_work(|work| work.trajectory_samples += 1);
}

#[cfg(test)]
pub(crate) fn record_raster() {
    record_skyworld_work(|work| work.rasters += 1);
}

#[cfg(test)]
pub(crate) fn record_raster_pixel() {
    record_skyworld_work(|work| work.raster_pixels += 1);
}

impl SkyWorld {
    /// Generate the compact additive overlay from an already-built world,
    /// terrain, and climate. The world must carry the generated astronomy
    /// provider fact, as every normal world build does.
    pub fn generate(
        world: &World,
        terrain: &GeneratedTerrain,
        climate: &GeneratedClimate,
        config: SkyWorldConfig,
    ) -> SkyWorld {
        assert_eq!(
            terrain.geosphere().vertex_count(),
            climate.geosphere().vertex_count(),
            "Skyworld inputs must share one geosphere"
        );
        let fields = derive_fields(world, terrain, climate);
        let mut territories = derive_territories(world, terrain, climate, &fields, &config);
        crate::skyworld_propagation::derive_movement_and_propagation(
            world,
            terrain,
            climate,
            &fields,
            &config,
            &mut territories,
        );
        SkyWorld {
            fields,
            territories,
        }
    }
}

/// Composition-root entry point for the Stage 1 Skyworld overlay.
pub fn skyworld_from(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    config: SkyWorldConfig,
) -> SkyWorld {
    SkyWorld::generate(world, terrain, climate, config)
}

fn radiation_transmission(altitude_m: f64) -> f64 {
    SEA_LEVEL_RADIATION_FRACTION
        + (1.0 - SEA_LEVEL_RADIATION_FRACTION)
            * (1.0 - math::exp(-altitude_m.max(0.0) / RADIATION_SCALE_M))
}

fn aether_profile(altitude_m: f64) -> f64 {
    SEA_LEVEL_AETHER_FRACTION
        + (1.0 - SEA_LEVEL_AETHER_FRACTION)
            * math::exp(-((altitude_m.max(0.0) - 8_000.0) / AETHER_SCALE_M).powi(2))
}

fn wind_profile(altitude_m: f64) -> f64 {
    1.0 + altitude_m.max(0.0) / WIND_SCALE_M
}

fn derive_fields(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> SkyFields {
    let mut atmosphere = world
        .seed
        .derive(crate::streams::SKYWORLD_ATMOSPHERE)
        .stream();
    let pressure = 0.82 + 0.36 * atmosphere.next_f64();
    let density = pressure * (0.92 + 0.16 * atmosphere.next_f64());
    let lapse_rate = 5.0 + 2.0 * atmosphere.next_f64();
    let radiation = climate.insolation() * (0.80 + 0.40 * atmosphere.next_f64());
    let aether = 0.40 + 0.60 * atmosphere.next_f64();
    let moisture_factor = 0.85 + 0.30 * atmosphere.next_f64();
    let wind_factor = 0.75 + 0.50 * atmosphere.next_f64();
    let sample = atmosphere.next_u64() as usize % terrain.geosphere().vertex_count();
    let vertex = Vertex(sample as u32);
    let mean_temperature = terrain
        .geosphere()
        .vertices()
        .map(|v| {
            #[cfg(test)]
            record_surface_vertex();
            let temperature = climate.mean_temperature_at(v).get();
            #[cfg(test)]
            let temperature = tests::source(tests::SourceAxis::Temperature, temperature);
            temperature
        })
        .sum::<f64>()
        / terrain.geosphere().vertex_count() as f64;
    let mean_moisture = terrain
        .geosphere()
        .vertices()
        .map(|v| {
            #[cfg(test)]
            record_surface_vertex();
            let moisture = climate.moisture_at(v);
            #[cfg(test)]
            let moisture = tests::source(tests::SourceAxis::Moisture, moisture);
            moisture
        })
        .sum::<f64>()
        / terrain.geosphere().vertex_count() as f64;
    let wind = climate
        .band_count()
        .map(|bands| hornvale_climate::prevailing_wind(terrain.geosphere(), vertex, bands))
        .unwrap_or([0.0; 3]);
    #[cfg(test)]
    let wind = wind.map(|value| tests::source(tests::SourceAxis::Wind, value));
    let wind_shear = climate.band_count().map_or(0.0, |bands| {
        terrain
            .geosphere()
            .neighbors(vertex)
            .iter()
            .map(|&neighbor| {
                let other = hornvale_climate::prevailing_wind(terrain.geosphere(), neighbor, bands);
                #[cfg(test)]
                let other = other.map(|value| tests::source(tests::SourceAxis::Wind, value));
                ((wind[0] - other[0]).powi(2)
                    + (wind[1] - other[1]).powi(2)
                    + (wind[2] - other[2]).powi(2))
                .sqrt()
            })
            .fold(0.0, f64::max)
    });
    let lunar = sky_forcing(world);
    SkyFields {
        altitude_m: 0.0,
        pressure,
        density,
        temperature_c: mean_temperature,
        lapse_rate_c_per_km: lapse_rate,
        high_sky_radiation: radiation * radiation_transmission(0.0),
        aether: aether * aether_profile(0.0),
        moisture: (mean_moisture * moisture_factor).clamp(0.0, 1.0),
        wind: [
            wind[0] * wind_factor,
            wind[1] * wind_factor,
            wind[2] * wind_factor,
        ],
        wind_shear: wind_shear * wind_factor,
        lunar_forcing: lunar,
        stellar_forcing: climate.insolation(),
    }
}

pub(crate) fn sky_forcing(world: &World) -> f64 {
    let sky = crate::sky_of(world).expect("Skyworld requires a built generated-sky world");
    let total_tide = sky
        .system()
        .moons
        .iter()
        .map(|moon| moon.tide_rel)
        .sum::<f64>();
    total_tide / (1.0 + total_tide)
}

/// Apply the per-vertex edifice bias to an already-derived environment score.
///
/// The boolean is deliberately supplied by the caller so the score's tectonic
/// contribution remains independently testable from terrain generation.
fn score_distribution_environment(
    non_tectonic_environment: f64,
    has_edifice: bool,
    draw: f64,
) -> (f64, f64) {
    let volcanic_bias = if has_edifice { 0.25 } else { 0.0 };
    let environment = (non_tectonic_environment + volcanic_bias).clamp(0.0, 1.0);
    (environment + draw * 0.45, environment)
}

fn distribution_score(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    scores: &mut Vec<f64>,
    altitudes: &mut Vec<f64>,
    vertex: Vertex,
) -> (f64, f64) {
    let key = format!("vertex/{}", vertex.0);
    let mut draw = world
        .seed
        .derive(crate::streams::SKYWORLD_DISTRIBUTION)
        .derive(StreamLabel::dynamic(&key))
        .stream();
    let elevation = terrain.elevation_at(vertex).get();
    #[cfg(test)]
    let elevation = tests::source(tests::SourceAxis::Elevation, elevation);
    let coastal = terrain
        .geosphere()
        .neighbors(vertex)
        .iter()
        .any(|&n| terrain.is_ocean(n) != terrain.is_ocean(vertex));
    let terrain_bias = if terrain.is_ocean(vertex) { 0.35 } else { 0.45 };
    let elevation_bias = (elevation / 8_000.0).clamp(-0.20, 0.35);
    let moisture = climate.moisture_at(vertex);
    let storm = climate.storm_propensity_at(vertex);
    #[cfg(test)]
    let moisture = tests::source(tests::SourceAxis::Moisture, moisture);
    #[cfg(test)]
    let storm = tests::source(tests::SourceAxis::Storm, storm);
    let climate_bias = moisture * 0.20 + storm * 0.15;
    let coastal_bias = if coastal { 0.25 } else { 0.0 };
    let non_tectonic_environment = terrain_bias + elevation_bias + climate_bias + coastal_bias;
    let (score, environment) = score_distribution_environment(
        non_tectonic_environment,
        crate::hazard::has_edifice(terrain, vertex),
        draw.next_f64(),
    );
    let altitude = 4_000.0 + 8_000.0 * draw.next_f64();
    if scores.len() <= vertex.0 as usize {
        scores.resize(vertex.0 as usize + 1, 0.0);
        altitudes.resize(vertex.0 as usize + 1, 0.0);
    }
    scores[vertex.0 as usize] = score;
    altitudes[vertex.0 as usize] = altitude;
    (score, environment)
}

fn coverage_target(ceiling: usize, draw: f64, environment: f64) -> usize {
    let minimum = ceiling.min(3);
    let span = ceiling - minimum;
    let conditioned = draw * environment.clamp(0.0, 1.0);
    minimum + (span as f64 * conditioned).round() as usize
}

fn derive_territories(
    world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    fields: &SkyFields,
    config: &SkyWorldConfig,
) -> Vec<SkyTerritory> {
    let vertex_count = terrain.geosphere().vertex_count();
    let requested = if config.max_projected_fraction.is_finite() {
        config
            .max_projected_fraction
            .clamp(0.0, HARD_COVERAGE_FRACTION)
    } else {
        HARD_COVERAGE_FRACTION
    };
    let ceiling = (vertex_count as f64 * requested).floor() as usize;
    if ceiling == 0 {
        return Vec::new();
    }
    let mut scores = Vec::with_capacity(vertex_count);
    let mut altitudes = Vec::with_capacity(vertex_count);
    let mut land = Vec::new();
    let mut ocean = Vec::new();
    let mut environment_total = 0.0;
    for vertex in terrain.geosphere().vertices() {
        #[cfg(test)]
        record_surface_vertex();
        let (score, environment) =
            distribution_score(world, terrain, climate, &mut scores, &mut altitudes, vertex);
        environment_total += environment;
        if terrain.is_ocean(vertex) {
            ocean.push((vertex, score));
        } else {
            land.push((vertex, score));
        }
    }
    let environment = environment_total / vertex_count as f64;
    let mut coverage = world
        .seed
        .derive(crate::streams::SKYWORLD_COVERAGE)
        .stream();
    let target = coverage_target(ceiling, coverage.next_f64(), environment);
    let by_score =
        |a: &(Vertex, f64), b: &(Vertex, f64)| b.1.total_cmp(&a.1).then_with(|| a.0.cmp(&b.0));
    land.sort_by(by_score);
    ocean.sort_by(by_score);
    let isolated = ocean
        .first()
        .map(|(v, _)| *v)
        .or_else(|| land.first().map(|(v, _)| *v));
    let excluded = isolated.map(|v| {
        let mut set = std::collections::BTreeSet::from([v]);
        set.extend(terrain.geosphere().neighbors(v).iter().copied());
        set
    });
    let cluster_seed = land
        .iter()
        .chain(ocean.iter())
        .map(|(v, _)| *v)
        .find(|v| excluded.as_ref().is_none_or(|set| !set.contains(v)));
    let cluster_size = target.saturating_sub(1);
    let mut cluster = std::collections::BTreeSet::new();
    let mut frontier = Vec::new();
    if cluster_size > 0
        && let Some(seed) = cluster_seed
    {
        cluster.insert(seed);
        frontier.push(seed);
    }
    let mut cursor = 0;
    while cluster.len() < cluster_size && cursor < frontier.len() {
        let current = frontier[cursor];
        cursor += 1;
        let mut neighbors: Vec<(Vertex, f64)> = terrain
            .geosphere()
            .neighbors(current)
            .iter()
            .filter_map(|&candidate| {
                if cluster.contains(&candidate)
                    || excluded
                        .as_ref()
                        .is_some_and(|set| set.contains(&candidate))
                {
                    None
                } else {
                    Some((candidate, scores[candidate.0 as usize]))
                }
            })
            .collect();
        neighbors.sort_by(by_score);
        for (candidate, _) in neighbors {
            if cluster.len() >= cluster_size {
                break;
            }
            if cluster.insert(candidate) {
                frontier.push(candidate);
            }
        }
    }
    if cluster.len() < cluster_size {
        for (vertex, _) in land.iter().chain(ocean.iter()) {
            if cluster.len() >= cluster_size {
                break;
            }
            if excluded.as_ref().is_none_or(|set| !set.contains(vertex)) {
                cluster.insert(*vertex);
            }
        }
    }
    let mut territories = Vec::new();
    if !cluster.is_empty() {
        let projected: Vec<Vertex> = cluster.into_iter().collect();
        let altitude = altitudes[projected[0].0 as usize];
        territories.push(make_territory(world, fields, projected, altitude, true));
    }
    if let Some(isolated) = isolated
        && !territories
            .iter()
            .any(|t| t.physical.projected.contains(&isolated))
    {
        territories.push(make_territory(
            world,
            fields,
            vec![isolated],
            altitudes[isolated.0 as usize],
            false,
        ));
    }
    territories.sort_by_key(|territory| territory.id);
    territories
}

fn make_territory(
    world: &World,
    fields: &SkyFields,
    mut projected: Vec<Vertex>,
    altitude: f64,
    orchard: bool,
) -> SkyTerritory {
    #[cfg(test)]
    record_territory();
    projected.sort();
    let surface = projected[0];
    let key = format!("territory/{}", surface.0);
    let mut phenotype_draw = world
        .seed
        .derive(crate::streams::SKYWORLD_PHENOTYPE)
        .derive(StreamLabel::dynamic(&key))
        .stream();
    let mut movement_draw = world
        .seed
        .derive(crate::streams::SKYWORLD_MOVEMENT)
        .derive(StreamLabel::dynamic(&key))
        .stream();
    let pick = |draw: &mut Stream, count: u64| (draw.next_u64() % count) as usize;
    let substrate = [
        SkySubstrate::Cloud,
        SkySubstrate::OrganicMat,
        SkySubstrate::Soil,
        SkySubstrate::Ice,
        SkySubstrate::Ash,
        SkySubstrate::Mineral,
        SkySubstrate::Mixed,
    ][pick(&mut phenotype_draw, 7)];
    let energy = [
        SkyEnergy::Sunlight,
        SkyEnergy::Aether,
        SkyEnergy::HighSkyRadiation,
        SkyEnergy::StormCharge,
        SkyEnergy::ThaumicFlow,
    ][pick(&mut phenotype_draw, 5)];
    let water = [
        SkyWater::Dry,
        SkyWater::Humid,
        SkyWater::CloudFed,
        SkyWater::RainFed,
        SkyWater::Saturated,
    ][pick(&mut phenotype_draw, 5)];
    let stability = [
        SkyStability::Rigid,
        SkyStability::Flexible,
        SkyStability::Regenerative,
        SkyStability::Brittle,
    ][pick(&mut phenotype_draw, 4)];
    let exchange = [
        SkyExchangeMode::Isolated,
        SkyExchangeMode::SurfaceLinked,
        SkyExchangeMode::CorridorLinked,
        SkyExchangeMode::Archipelagic,
    ][pick(&mut phenotype_draw, 4)];
    let mobility = [
        SkyMobility::Drifting,
        SkyMobility::Cyclic,
        SkyMobility::CurrentFollowing,
        SkyMobility::FieldFollowing,
        SkyMobility::Mixed,
    ][pick(&mut movement_draw, 5)];
    let ecology = [
        SkyEcology::Bare,
        SkyEcology::Microbial,
        SkyEcology::Rooted,
        SkyEcology::Wooded,
        SkyEcology::OrchardBearing,
    ][pick(&mut phenotype_draw, 5)];
    let local_fields = fields.at_altitude(altitude);
    let phenotype = SkyPhenotype {
        substrate,
        energy,
        water,
        mobility,
        stability,
        exchange,
        ecology: if orchard {
            SkyEcology::OrchardBearing
        } else {
            ecology
        },
        buoyancy: phenotype_draw.next_f64(),
        cohesion: phenotype_draw.next_f64(),
        flexibility: phenotype_draw.next_f64(),
        recovery: phenotype_draw.next_f64(),
    };
    let stocks = stocks_from_fields(&local_fields, orchard);
    let lineage = SkyLineage {
        origin: SkyLifecycle::WindBloom,
        current: if orchard {
            SkyLifecycle::MatureOrchard
        } else {
            SkyLifecycle::PioneerIslet
        },
    };
    let origin = SkyPosition {
        surface,
        altitude_m: altitude,
    };
    let physical = SkyFootprint { projected };
    SkyTerritory {
        id: surface.0,
        lineage,
        phenotype,
        stocks,
        origin,
        trajectory: Vec::new(),
        exchange: physical.clone(),
        physical,
        influence: SkyPropagation::default(),
    }
}

fn stocks_from_fields(fields: &SkyFields, orchard: bool) -> SkyStocks {
    let plankton = fields
        .aether
        .min(fields.high_sky_radiation)
        .min(fields.moisture)
        .clamp(0.0, 1.0);
    let root_support = if orchard {
        plankton * ORCHARD_ROOT_FRACTION
    } else {
        0.0
    };
    let soil_fertility = root_support * ORCHARD_SOIL_FRACTION;
    let canopy_biomass = soil_fertility * ORCHARD_CANOPY_FRACTION;
    let flowers = canopy_biomass * ORCHARD_FLOWER_FRACTION;
    let pollination = flowers * ORCHARD_POLLINATION_FRACTION;
    let fruit = pollination * ORCHARD_FRUIT_FRACTION;
    SkyStocks {
        plankton,
        root_support,
        soil_fertility,
        canopy_biomass,
        flowers,
        cloud_water: fields.moisture.clamp(0.0, 1.0),
        pollination,
        fruit,
        detritus: canopy_biomass * ORCHARD_DETRITUS_FRACTION,
        seed_spore_reserve: fruit * ORCHARD_SEED_SPORE_FRACTION,
        animal_forage: fruit * ORCHARD_ANIMAL_FORAGE_FRACTION,
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::{
        climate_from, propagation_at, render_skyworld_diagnostic_readout, render_skyworld_png,
        render_skyworld_readout, terrain_of, trajectory_at,
    };

    // The test fixture deliberately reconstructs the one substrate pair whose
    // reuse the probes measure (decision 0092's named construction-site rule).
    #[allow(clippy::disallowed_methods)]
    fn fixture() -> (World, GeneratedTerrain, GeneratedClimate) {
        let world = crate::fixture::seed_42_world();
        let terrain = terrain_of(&world).expect("terrain reconstructs");
        let climate = climate_from(&world, &terrain).expect("climate derives from terrain");
        (world, terrain, climate)
    }

    fn config() -> SkyWorldConfig {
        SkyWorldConfig {
            max_projected_fraction: 0.10,
            trajectory_samples: 8,
            propagation_radius: 2,
        }
    }

    // Override copied values only at the real consumption sites. The providers
    // and their biome projections remain intact; no runtime test API is added.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(crate) enum SourceAxis {
        Temperature,
        Moisture,
        Elevation,
        Storm,
        Wind,
        Current,
    }

    thread_local! {
        static SOURCE_AXIS: std::cell::Cell<Option<SourceAxis>> = const { std::cell::Cell::new(None) }; // lexicon: std::cell::Cell diagnostic idiom
        static SOURCE_CHANGES: std::cell::Cell<[usize; 6]> = const { std::cell::Cell::new([0; 6]) }; // lexicon: std::cell::Cell diagnostic idiom
    }

    pub(crate) fn source(axis: SourceAxis, value: f64) -> f64 {
        let changed = if SOURCE_AXIS.with(|selected| selected.get() == Some(axis)) {
            match axis {
                SourceAxis::Temperature => value + 10.0,
                SourceAxis::Moisture | SourceAxis::Storm => value * 0.5,
                SourceAxis::Elevation => value + 500.0,
                SourceAxis::Wind | SourceAxis::Current => -value,
            }
        } else {
            value
        };
        SOURCE_CHANGES.with(|counts| {
            let mut next = counts.get();
            next[axis as usize] += usize::from(value != changed);
            counts.set(next);
        });
        changed
    }

    pub(crate) fn with_source<T>(axis: SourceAxis, action: impl FnOnce() -> T) -> T {
        struct Reset;
        impl Drop for Reset {
            fn drop(&mut self) {
                SOURCE_AXIS.with(|value| value.set(None));
            }
        }
        SOURCE_CHANGES.with(|counts| counts.set([0; 6]));
        SOURCE_AXIS.with(|value| assert!(value.replace(Some(axis)).is_none()));
        let _reset = Reset;
        let result = action();
        let counts = SOURCE_CHANGES.with(|counts| counts.get());
        assert!(
            counts[axis as usize] > 0,
            "VACUOUS: {axis:?} source never changed"
        );
        for (index, count) in counts.into_iter().enumerate() {
            if index != axis as usize {
                assert_eq!(count, 0, "perturbation changed an unrelated source axis");
            }
        }
        result
    }

    #[derive(Debug, PartialEq)]
    struct SurfaceSample {
        elevation: hornvale_kernel::ReferenceElevation,
        ocean: bool,
        unrest: f64,
        temperature: hornvale_kernel::Temperature,
        moisture: f64,
        storm: f64,
        current: [f64; 3],
        biome: hornvale_climate::BiomeExpr,
    }

    fn surface_projection(
        terrain: &GeneratedTerrain,
        climate: &GeneratedClimate,
    ) -> Vec<SurfaceSample> {
        terrain
            .geosphere()
            .vertices()
            .map(|v| SurfaceSample {
                elevation: terrain.elevation_at(v),
                ocean: terrain.is_ocean(v),
                unrest: terrain.unrest_at(v),
                temperature: climate.mean_temperature_at(v),
                moisture: climate.moisture_at(v),
                storm: climate.storm_propensity_at(v),
                current: climate.current_at(v),
                biome: climate.biome_expr_at(v),
            })
            .collect()
    }

    #[test]
    fn surface_climate_axes_feed_distinct_overlay_values() {
        let (world, terrain, climate) = fixture();
        let surface = || surface_projection(&terrain, &climate);
        let original_surface = surface();
        let before = skyworld_from(&world, &terrain, &climate, config());
        for axis in [
            SourceAxis::Temperature,
            SourceAxis::Moisture,
            SourceAxis::Wind,
        ] {
            let mut after =
                with_source(axis, || skyworld_from(&world, &terrain, &climate, config()));
            match axis {
                SourceAxis::Temperature => {
                    assert!(
                        (after.fields.temperature_c - before.fields.temperature_c - 10.0).abs()
                            < 1e-8
                    );
                    assert_eq!(
                        after.territories, before.territories,
                        "temperature is not a stock or distribution input"
                    );
                    after.fields.temperature_c = before.fields.temperature_c;
                }
                SourceAxis::Moisture => {
                    assert!(after.fields.moisture < before.fields.moisture);
                    let low = stocks_from_fields(&after.fields.at_altitude(8_000.0), true);
                    let high = stocks_from_fields(&before.fields.at_altitude(8_000.0), true);
                    assert!(low.cloud_water < high.cloud_water);
                    after.fields.moisture = before.fields.moisture;
                }
                SourceAxis::Wind => {
                    assert_ne!(after.fields.wind, before.fields.wind);
                    assert_eq!(after.fields.wind, before.fields.wind.map(|value| -value));
                    after.fields.wind = before.fields.wind;
                    // Sign reversal preserves shear magnitude.
                }
                _ => unreachable!(),
            }
            assert_eq!(
                after.fields, before.fields,
                "{axis:?} changed unrelated fields"
            );
            assert_eq!(
                surface(),
                original_surface,
                "{axis:?} changed the provider projection"
            );
        }
    }

    #[test]
    fn distribution_axes_are_individually_observable() {
        let (world, terrain, climate) = fixture();
        let original_surface = surface_projection(&terrain, &climate);
        let scores = || {
            let mut scores = Vec::new();
            let mut altitudes = Vec::new();
            for vertex in terrain.geosphere().vertices() {
                distribution_score(
                    &world,
                    &terrain,
                    &climate,
                    &mut scores,
                    &mut altitudes,
                    vertex,
                );
            }
            (scores, altitudes)
        };
        let before = scores();
        let fields = derive_fields(&world, &terrain, &climate);
        for axis in [
            SourceAxis::Elevation,
            SourceAxis::Storm,
            SourceAxis::Moisture,
        ] {
            let after = with_source(axis, scores);
            assert_ne!(
                before.0, after.0,
                "{axis:?} did not affect distribution scores"
            );
            assert_eq!(
                before.1, after.1,
                "{axis:?} changed independently drawn altitude"
            );
            if axis != SourceAxis::Moisture {
                let after_fields = with_source(axis, || {
                    // Include the score reads to prove the override was exercised.
                    let _ = scores();
                    derive_fields(&world, &terrain, &climate)
                });
                assert_eq!(after_fields, fields, "{axis:?} changed ambient fields");
            }
            assert_eq!(surface_projection(&terrain, &climate), original_surface);
        }
    }

    #[test]
    fn wind_and_derived_current_independently_drive_movement() {
        let (world, terrain, climate) = fixture();
        let original_surface = surface_projection(&terrain, &climate);
        let fields = derive_fields(&world, &terrain, &climate);
        let bands = climate.band_count().expect("fixture has circulation");
        let vertex = terrain
            .geosphere()
            .vertices()
            .find(|&v| {
                hornvale_climate::ocean_current(
                    terrain.geosphere(),
                    &|v| terrain.is_ocean(v),
                    v,
                    bands,
                ) != [0.0; 3]
            })
            .expect("VACUOUS: fixture has no ocean-current source");
        for (axis, mobility) in [
            (SourceAxis::Wind, SkyMobility::Drifting),
            (SourceAxis::Current, SkyMobility::CurrentFollowing),
        ] {
            // Hold identity, origin, footprint, stocks and mobility fixed in
            // both arms. Only the sampled flow value is perturbed.
            let mut territory = make_territory(&world, &fields, vec![vertex], 8_000.0, false);
            territory.phenotype.mobility = mobility;
            let run = || {
                let mut territories = vec![territory.clone()];
                crate::skyworld_propagation::derive_movement_and_propagation(
                    &world,
                    &terrain,
                    &climate,
                    &fields,
                    &SkyWorldConfig {
                        trajectory_samples: 2,
                        ..config()
                    },
                    &mut territories,
                );
                territories.remove(0)
            };
            let before = run();
            let after = with_source(axis, run);
            assert_ne!(
                before.trajectory[1].position.surface, after.trajectory[1].position.surface,
                "{axis:?} did not change the next surface"
            );
            assert_eq!(before.origin, after.origin);
            assert_eq!(before.phenotype, after.phenotype);
            assert_eq!(before.stocks, after.stocks);
            assert_eq!(before.trajectory[0], after.trajectory[0]);
            assert_eq!(
                before.trajectory[1].position.altitude_m,
                after.trajectory[1].position.altitude_m
            );
            assert_eq!(surface_projection(&terrain, &climate), original_surface);
        }
    }

    #[test]
    fn altitude_changes_profile_without_changing_independent_forcing() {
        let (world, terrain, climate) = fixture();
        let original_surface = surface_projection(&terrain, &climate);
        let fields = derive_fields(&world, &terrain, &climate);
        let lower = fields.at_altitude(4_000.0);
        let upper = fields.at_altitude(8_000.0);
        assert_ne!(lower.altitude_m, upper.altitude_m);
        assert!(upper.temperature_c < lower.temperature_c);
        assert!(upper.pressure < lower.pressure);
        assert!(upper.density < lower.density);
        assert!(upper.moisture < lower.moisture);
        assert!(upper.high_sky_radiation > lower.high_sky_radiation);
        assert!(upper.aether > lower.aether);
        assert_ne!(upper.wind, lower.wind);
        assert_eq!(upper.lapse_rate_c_per_km, lower.lapse_rate_c_per_km);
        assert_eq!(upper.lunar_forcing, lower.lunar_forcing);
        assert_eq!(upper.stellar_forcing, lower.stellar_forcing);
        assert_eq!(derive_fields(&world, &terrain, &climate), fields);
        assert_eq!(surface_projection(&terrain, &climate), original_surface);
    }

    fn reset_substrate_calls() {
        crate::TERRAIN_OF_CALLS.with(|calls| calls.set(0));
        crate::CLIMATE_FROM_CALLS.with(|calls| calls.set(0));
    }

    fn assert_substrate_was_not_reconstructed(context: &str) {
        assert_eq!(
            crate::TERRAIN_OF_CALLS.with(|calls| calls.get()),
            0,
            "{context} reconstructed terrain"
        );
        assert_eq!(
            crate::CLIMATE_FROM_CALLS.with(|calls| calls.get()),
            0,
            "{context} reconstructed climate"
        );
    }

    #[test]
    fn edifice_bias_adds_to_the_distribution_score() {
        let without_edifice = score_distribution_environment(0.50, false, 0.0);
        let with_edifice = score_distribution_environment(0.50, true, 0.0);

        assert_eq!(without_edifice, (0.50, 0.50));
        assert_eq!(with_edifice, (0.75, 0.75));
    }

    #[test]
    fn skyworld_generation_reuses_passed_substrate() {
        let (world, terrain, climate) = fixture();
        reset_substrate_calls();

        let skyworld = skyworld_from(&world, &terrain, &climate, config());

        assert!(
            !skyworld.territories.is_empty(),
            "VACUOUS: fixture generated no territories"
        );
        assert_substrate_was_not_reconstructed("skyworld_from");
    }

    #[test]
    fn rendering_does_not_reconstruct_substrate() {
        let (world, terrain, climate) = fixture();
        let skyworld = skyworld_from(&world, &terrain, &climate, config());
        let before = skyworld.clone();
        reset_substrate_calls();

        for detail in [
            crate::SkyWorldDetail::Planet,
            crate::SkyWorldDetail::Regional,
            crate::SkyWorldDetail::Habitat,
        ] {
            assert_eq!(
                render_skyworld_png(&skyworld, &terrain, detail),
                render_skyworld_png(&skyworld, &terrain, detail),
                "{detail:?} PNG changed between repeated renders"
            );
            assert_eq!(
                render_skyworld_readout(&skyworld, detail),
                render_skyworld_readout(&skyworld, detail),
                "{detail:?} readout changed between repeated renders"
            );
            assert_eq!(
                render_skyworld_diagnostic_readout(&skyworld, detail),
                render_skyworld_diagnostic_readout(&skyworld, detail),
                "{detail:?} diagnostic readout changed between repeated renders"
            );
        }

        assert_eq!(
            skyworld, before,
            "rendering changed generated Skyworld state"
        );
        assert_substrate_was_not_reconstructed("rendering");
    }

    #[test]
    fn queries_do_not_consume_randomness() {
        let (world, terrain, climate) = fixture();
        let skyworld = skyworld_from(&world, &terrain, &climate, config());
        let before = skyworld.clone();
        reset_substrate_calls();

        for territory in &skyworld.territories {
            for sample in territory.trajectory.iter().rev() {
                assert_eq!(
                    trajectory_at(&skyworld, territory.id, sample.time_slice),
                    Some(sample)
                );
            }
            for detail in [
                SkyPropagationDetail::Local,
                SkyPropagationDetail::Corridors,
                SkyPropagationDetail::Events,
                SkyPropagationDetail::All,
            ] {
                assert_eq!(
                    propagation_at(&skyworld, territory.id, detail),
                    propagation_at(&skyworld, territory.id, detail),
                    "{detail:?} propagation changed between repeated queries"
                );
            }
        }

        assert_eq!(skyworld, before, "queries changed generated Skyworld state");
        assert_substrate_was_not_reconstructed("queries");
    }

    #[test]
    fn detail_changes_materialization_not_generation() {
        let (world, terrain, climate) = fixture();
        let skyworld = skyworld_from(&world, &terrain, &climate, config());
        let before = skyworld.clone();
        reset_substrate_calls();

        let planet = render_skyworld_png(&skyworld, &terrain, crate::SkyWorldDetail::Planet);
        let regional = render_skyworld_png(&skyworld, &terrain, crate::SkyWorldDetail::Regional);
        let habitat = render_skyworld_png(&skyworld, &terrain, crate::SkyWorldDetail::Habitat);

        assert_ne!(
            planet, regional,
            "planet and regional detail materialized the same PNG"
        );
        assert_ne!(
            regional, habitat,
            "regional and habitat detail materialized the same PNG"
        );
        assert_eq!(
            skyworld, before,
            "detail selection changed generated SkyWorld state"
        );
        assert_substrate_was_not_reconstructed("detail selection");
    }

    #[test]
    fn bounded_work_counts_generation_and_rendering_at_their_loops() {
        let (world, terrain, climate) = fixture();
        let inactive = SkyWorldConfig {
            max_projected_fraction: 0.0,
            ..config()
        };
        let short = SkyWorldConfig {
            trajectory_samples: 2,
            ..config()
        };
        let long = SkyWorldConfig {
            trajectory_samples: 8,
            ..config()
        };

        reset_skyworld_work();
        let no_territories = skyworld_from(&world, &terrain, &climate, inactive);
        let inactive_work = skyworld_work();
        assert!(no_territories.territories.is_empty());
        assert_eq!(inactive_work.territories, 0);
        assert_eq!(inactive_work.trajectory_samples, 0);
        assert_eq!(
            inactive_work.surface_vertices,
            terrain.geosphere().vertex_count() * 2
        );
        assert_eq!(inactive_work.surface_vertices, 81_924);
        assert_eq!(
            inactive_work.propagation_indexes, 1,
            "even an empty overlay builds the index"
        );
        assert_eq!(inactive_work.physical_neighbors, 0);
        assert_eq!(inactive_work.expansion_neighbors, 0);

        let single = SkyWorldConfig {
            max_projected_fraction: 1.5 / terrain.geosphere().vertex_count() as f64,
            trajectory_samples: 2,
            ..config()
        };
        reset_skyworld_work();
        let single_world = skyworld_from(&world, &terrain, &climate, single);
        let single_work = skyworld_work();
        assert_eq!(single_world.territories.len(), 1);
        assert_eq!(single_work.territories, 1);
        assert_eq!(single_work.trajectory_samples, 2);
        assert_eq!(single_work.propagation_indexes, 1);
        assert_eq!(single_work.surface_vertices, 122_886);
        assert_eq!(single_work.physical_neighbors, 0);
        assert!(single_work.expansion_neighbors > 0);
        assert_eq!(single_work.adjacency_pairs, 2);
        assert_eq!(single_work.movement_candidates, 6);
        assert_eq!(single_work.expansion_neighbors, 240);

        reset_skyworld_work();
        let short_world = skyworld_from(&world, &terrain, &climate, short);
        let short_work = skyworld_work();
        assert!(!short_world.territories.is_empty());
        assert!(short_work.territories > inactive_work.territories);
        assert_eq!(short_work.territories, short_world.territories.len());
        assert_eq!(short_work.territories, 2);
        assert_eq!(
            short_work.trajectory_samples,
            short_world.territories.len() * 2
        );
        assert_eq!(short_work.trajectory_samples, 4);
        assert_eq!(short_work.propagation_indexes, 1);
        assert!(short_work.physical_neighbors > 0);
        assert!(short_work.expansion_neighbors > single_work.expansion_neighbors);
        assert_eq!(short_work.adjacency_pairs, 8);
        assert_eq!(short_work.movement_candidates, 12);
        // MERGE RE-PIN (The Trencher absorbing The Tidemark, 2026-09-15):
        // world-byte tripwire, re-measured directly against the merged code.
        assert_eq!(short_work.physical_neighbors, 7_517);
        assert_eq!(short_work.expansion_neighbors, 19_824);

        reset_skyworld_work();
        let long_world = skyworld_from(&world, &terrain, &climate, long);
        let long_work = skyworld_work();
        assert_eq!(long_work.territories, long_world.territories.len());
        assert_eq!(long_work.territories, short_work.territories);
        assert_eq!(
            long_work.trajectory_samples,
            long_world.territories.len() * 8
        );
        assert_eq!(long_work.trajectory_samples, 16);
        assert_eq!(
            long_work.surface_vertices,
            terrain.geosphere().vertex_count() * 3
        );
        assert_eq!(long_work.surface_vertices, 122_886);
        assert_eq!(long_work.surface_vertices, short_work.surface_vertices);
        assert_eq!(long_work.propagation_indexes, 1);
        assert!(long_work.physical_neighbors > short_work.physical_neighbors);
        assert!(long_work.expansion_neighbors > short_work.expansion_neighbors);
        assert!(long_work.movement_candidates > short_work.movement_candidates);
        assert!(long_work.movement_candidates <= 2 * 7 * 6);
        assert_eq!(long_work.adjacency_pairs, 32);
        assert_eq!(long_work.movement_candidates, 84);
        // MERGE RE-PIN (The Trencher absorbing The Tidemark, 2026-09-15):
        // world-byte tripwire, re-measured directly against the merged code.
        assert_eq!(long_work.physical_neighbors, 30_071);
        assert_eq!(long_work.expansion_neighbors, 79_296);

        let mut render_counts = Vec::new();
        for (detail, marks, stamps) in [
            (crate::SkyWorldDetail::Planet, 736, 724),
            (crate::SkyWorldDetail::Regional, 2_616, 1_060),
            (crate::SkyWorldDetail::Habitat, 2_616, 1_060),
        ] {
            reset_skyworld_work();
            let _ = render_skyworld_png(&long_world, &terrain, detail);
            let work = skyworld_work();
            assert_eq!(work.rasters, 1);
            assert_eq!(work.raster_pixels, 32_768);
            assert_eq!(work.render_surface_vertices, 81_924);
            assert_eq!(work.render_indexes, 1);
            assert_eq!(work.propagation_indexes, 0);
            assert_eq!(work.trajectory_samples, 0);
            assert_eq!(work.overlay_marks, marks);
            assert_eq!(work.occupied_vertices, 2_598);
            assert_eq!(work.stamp_candidates, stamps);
            render_counts.push(work);
        }
        assert!(render_counts[0].overlay_marks < render_counts[1].overlay_marks);
        assert_eq!(
            render_counts[1], render_counts[2],
            "habitat PNG changes palette, not traversal"
        );
    }
}
