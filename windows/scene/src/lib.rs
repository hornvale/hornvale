//! The scene window: deterministic, semantic-only scene descriptions over
//! the query surface (rendering-strategy Ring 2; decision 0022). A scene
//! says *what an observer can see* — raw quantities, catalog names, point
//! features — never how to draw it. Schemas are save-format-class
//! contracts: additive changes stay in-version, changed meaning mints a
//! new version alongside. This crate holds the cartographic pole:
//! `scene/tiles/v1`, the equirectangular tile lattice; and the orrery pole:
//! `scene/system/v1`, the star system's orbital elements.

#![warn(missing_docs)]

use hornvale_kernel::{NearestCellIndex, World};
use serde::Serialize;

mod region;
pub use region::*;

/// The schema identifier this crate emits.
/// type-audit: bare-ok(identifier-text)
pub const TILES_SCHEMA: &str = "scene/tiles/v1";
/// Smallest legal lattice width.
/// type-audit: bare-ok(count)
pub const MIN_WIDTH: u32 = 16;
/// Largest legal lattice width.
/// type-audit: bare-ok(count)
pub const MAX_WIDTH: u32 = 1024;

/// Scene construction failed; the reason, loudly (the GenesisError manner).
/// type-audit: bare-ok(diagnostic-value: WidthOdd.0), bare-ok(diagnostic-value: WidthOutOfRange.0), bare-ok(prose: Build.0), bare-ok(diagnostic-value: RegionFaceOutOfRange.0), bare-ok(diagnostic-value: RegionLevelOutOfRange.0), bare-ok(diagnostic-value: RegionTileOutOfRange.ix), bare-ok(diagnostic-value: RegionTileOutOfRange.iy), bare-ok(diagnostic-value: RegionTileOutOfRange.level), bare-ok(diagnostic-value: RegionSamplesOutOfRange.0)
#[derive(Debug, Clone, PartialEq)]
pub enum SceneError {
    /// Width must be even (height is width / 2).
    WidthOdd(u32),
    /// Width must lie in `MIN_WIDTH..=MAX_WIDTH`.
    WidthOutOfRange(u32),
    /// The world could not be rebuilt from its ledger.
    Build(String),
    /// Regional query: `face` must be 0..=5.
    RegionFaceOutOfRange(u32),
    /// Regional query: `level` must be 0..=MAX_REGION_LEVEL.
    RegionLevelOutOfRange(u32),
    /// Regional query: `ix`/`iy` must be < 2^level.
    RegionTileOutOfRange {
        /// The offending column.
        ix: u32,
        /// The offending row.
        iy: u32,
        /// The level whose 2^level bound they violated.
        level: u32,
    },
    /// Regional query: `samples` must be 1..=MAX_REGION_SAMPLES.
    RegionSamplesOutOfRange(u32),
}

impl std::fmt::Display for SceneError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SceneError::WidthOdd(w) => write!(
                f,
                "--width {w} is odd; height is width/2, so width must be even"
            ),
            SceneError::WidthOutOfRange(w) => {
                write!(f, "--width {w} is outside {MIN_WIDTH}..={MAX_WIDTH}")
            }
            SceneError::Build(e) => write!(f, "building the world: {e}"),
            SceneError::RegionFaceOutOfRange(f_) => {
                write!(f, "--face {f_} is outside 0..=5 (six cube faces)")
            }
            SceneError::RegionLevelOutOfRange(l) => {
                write!(f, "--level {l} is outside 0..={MAX_REGION_LEVEL}")
            }
            SceneError::RegionTileOutOfRange { ix, iy, level } => write!(
                f,
                "--ix {ix}/--iy {iy} out of range for level {level} (must be < {})",
                1u64 << level
            ),
            SceneError::RegionSamplesOutOfRange(s) => {
                write!(f, "--samples {s} is outside 1..={MAX_REGION_SAMPLES}")
            }
        }
    }
}

/// A named point on the lattice — settlements today, more kinds later.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: kind), pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Feature {
    /// The feature's canonical name.
    pub name: String,
    /// What kind of point this is: `"settlement"` or `"flagship"`.
    pub kind: String,
    /// Degrees north.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Degrees east.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// One `scene/tiles/v1` document (scene-protocol spec §2). Field order is
/// the JSON key order and is contract — never reorder. Layers are
/// row-major, top row first: latitude 90→−90 down, longitude −180→180
/// across, pixel centers.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(identifier-text: biome_legend), bare-ok(constructor-edge: seed), bare-ok(count: width), bare-ok(count: height), pending(wave-3: sea_level_m), waiver(elevation-convention: elevation_m), bare-ok(flag: ocean), bare-ok(index: biome), bare-ok(index: plate), bare-ok(ratio: unrest), bare-ok(diagnostic-value: t_mean_c), bare-ok(diagnostic-value: t_swing_c), bare-ok(diagnostic-value: season_period_days), bare-ok(count: circulation_bands), bare-ok(ratio: moisture)
#[derive(Debug, Serialize)]
pub struct TilesScene {
    /// Always `scene/tiles/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// Lattice width in tiles.
    pub width: u32,
    /// Lattice height in tiles (always `width / 2`).
    pub height: u32,
    /// Sea level in meters.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
    /// Elevation in meters per tile.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub elevation_m: Vec<f64>,
    /// Whether each tile is ocean (sea level baked in).
    pub ocean: Vec<bool>,
    /// Biome per tile, as an index into `biome_legend`.
    pub biome: Vec<u16>,
    /// The full biome catalog, in stable order.
    pub biome_legend: Vec<String>,
    /// Tectonic plate id per tile.
    pub plate: Vec<u32>,
    /// Tectonic unrest per tile.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub unrest: Vec<f64>,
    /// Named points: settlements, the flagship last.
    pub features: Vec<Feature>,
    /// Annual-mean temperature per tile, °C.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_mean_c: Vec<f64>,
    /// Hemisphere-signed seasonal half-swing per tile, °C (0 when locked/zero-obliquity).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_swing_c: Vec<f64>,
    /// The seasonal sinusoid's period, standard days (the world's year).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub season_period_days: f64,
    /// Circulation bands per hemisphere; omitted entirely on tidally locked worlds.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub circulation_bands: Option<u32>,
    /// Moisture index per tile, dimensionless [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub moisture: Vec<f64>,
}

/// Build the `scene/tiles/v1` scene for `world` at `width` tiles across
/// (height is `width / 2`). Deterministic: same world + same width →
/// the same scene, byte-for-byte once serialized.
/// type-audit: bare-ok(count: width)
pub fn tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError> {
    if !(MIN_WIDTH..=MAX_WIDTH).contains(&width) {
        return Err(SceneError::WidthOutOfRange(width));
    }
    if !width.is_multiple_of(2) {
        return Err(SceneError::WidthOdd(width));
    }
    let height = width / 2;
    let terrain =
        hornvale_worldgen::terrain_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let climate =
        hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    // Two indices, not one: terrain and climate each carry their own
    // geosphere, and today both happen to share the same cell level, so one
    // index could in principle serve both. Keeping them separate is
    // deliberate defensiveness against that ever diverging — behavior is
    // identical while the two geospheres agree.
    let terrain_index = NearestCellIndex::new(terrain.geosphere());
    let climate_index = NearestCellIndex::new(climate.geosphere());
    let biomes = climate.biome_map();
    let catalog = hornvale_climate::Biome::catalog();
    let tiles = (width * height) as usize;
    let mut elevation_m = Vec::with_capacity(tiles);
    let mut ocean = Vec::with_capacity(tiles);
    let mut biome = Vec::with_capacity(tiles);
    let mut plate = Vec::with_capacity(tiles);
    let mut unrest = Vec::with_capacity(tiles);
    let mut t_mean_c = Vec::with_capacity(tiles);
    let mut t_swing_c = Vec::with_capacity(tiles);
    let mut moisture = Vec::with_capacity(tiles);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let t_cell = terrain_index.nearest(terrain.geosphere(), latitude, longitude);
            let c_cell = climate_index.nearest(climate.geosphere(), latitude, longitude);
            elevation_m.push(terrain.elevation_at(t_cell).get());
            ocean.push(terrain.is_ocean(t_cell));
            let b = *biomes.get(c_cell);
            let index = catalog
                .iter()
                .position(|entry| *entry == b)
                .expect("every biome is in the catalog") as u16;
            biome.push(index);
            plate.push(terrain.plate_of(t_cell));
            unrest.push(terrain.unrest_at(t_cell));
            t_mean_c.push(climate.mean_temperature_at(c_cell).get());
            t_swing_c.push(climate.seasonal_swing_at(c_cell));
            moisture.push(climate.moisture_at(c_cell));
        }
    }
    debug_assert!(
        elevation_m
            .iter()
            .chain(unrest.iter())
            .chain(t_mean_c.iter())
            .chain(t_swing_c.iter())
            .chain(moisture.iter())
            .all(|v| v.is_finite()),
        "scene layers must be finite; serde_json would emit null"
    );
    Ok(TilesScene {
        schema: TILES_SCHEMA.to_string(),
        seed: world.seed.0,
        width,
        height,
        sea_level_m: terrain.sea_level().get(),
        elevation_m,
        ocean,
        biome,
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        plate,
        unrest,
        features: features_of(world),
        t_mean_c,
        t_swing_c,
        season_period_days: climate.year_length_std(),
        circulation_bands: climate.band_count(),
        moisture,
    })
}

/// Per-tile actual temperature at `day`, °C, on the same lattice as
/// [`tiles_scene`] — `temperature_at` sampled at each tile's climate cell.
/// This is the sim's ground truth that a client reconstructs from the
/// `t_mean_c`/`t_swing_c` layers; the cross-repo contract test compares the
/// client's reconstruction against these values. Full precision (not
/// quantized) — callers that need portable bytes quantize at their own
/// boundary.
/// type-audit: bare-ok(count: width), bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
pub fn temperature_grid(world: &World, width: u32, day: f64) -> Result<Vec<f64>, SceneError> {
    if !(MIN_WIDTH..=MAX_WIDTH).contains(&width) {
        return Err(SceneError::WidthOutOfRange(width));
    }
    if !width.is_multiple_of(2) {
        return Err(SceneError::WidthOdd(width));
    }
    let height = width / 2;
    let climate =
        hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let climate_index = NearestCellIndex::new(climate.geosphere());
    let tiles = (width * height) as usize;
    let mut temperature = Vec::with_capacity(tiles);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let c_cell = climate_index.nearest(climate.geosphere(), latitude, longitude);
            temperature.push(climate.temperature_at(c_cell, day).get());
        }
    }
    Ok(temperature)
}

/// Settlement point features: every place holding both coordinate facts,
/// in `places` order, with the flagship excluded there and appended last
/// under its own kind (spec §2: the flagship appears exactly once).
fn features_of(world: &World) -> Vec<Feature> {
    let flagship = hornvale_settlement::village_info(world);
    let flagship_id = flagship.as_ref().map(|v| v.id);
    let mut features = Vec::new();
    for place in hornvale_terrain::places(world) {
        if Some(place.id) == flagship_id {
            continue;
        }
        if let Some((latitude, longitude)) = place_latlon(world, place.id) {
            features.push(Feature {
                name: place.name,
                kind: "settlement".to_string(),
                latitude,
                longitude,
            });
        }
    }
    if let Some(village) = flagship
        && let Some((latitude, longitude)) = place_latlon(world, village.id)
    {
        features.push(Feature {
            name: village.name,
            kind: "flagship".to_string(),
            latitude,
            longitude,
        });
    }
    features
}

/// Latitude/longitude of a place from settlement's coordinate facts;
/// `None` if either is missing (such a place is skipped, the
/// settlement-map precedent).
fn place_latlon(world: &World, id: hornvale_kernel::EntityId) -> Option<(f64, f64)> {
    let lat = match world.ledger.value_of(id, hornvale_settlement::LATITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    let lon = match world.ledger.value_of(id, hornvale_settlement::LONGITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    Some((lat, lon))
}

/// Serialize a scene as compact JSON — the wire and artifact form.
/// Deterministic: struct field order fixes key order; serde_json's float
/// text is shortest-round-trip.
/// type-audit: bare-ok(artifact: return)
pub fn scene_json(scene: &TilesScene) -> String {
    serde_json::to_string(scene).expect("a TilesScene always serializes")
}

/// The schema identifier for the system (orrery) scene kind.
/// type-audit: bare-ok(identifier-text)
pub const SYSTEM_SCHEMA: &str = "scene/system/v1";

/// The central star's semantic elements.
/// type-audit: bare-ok(identifier-text: class_name), pending(wave-3: luminosity_rel), pending(wave-3: hz_inner_au), pending(wave-3: hz_outer_au)
#[derive(Debug, Serialize)]
pub struct StarElem {
    /// Descriptive spectral class name (e.g. `"yellow dwarf (G)"`).
    pub class_name: String,
    /// Luminosity in solar luminosities.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub luminosity_rel: f64,
    /// Habitable-zone inner edge, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub hz_inner_au: f64,
    /// Habitable-zone outer edge, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub hz_outer_au: f64,
}

/// The anchor world's orbital and rotational elements.
/// type-audit: pending(wave-3: orbit_au), pending(wave-3: year_days), pending(wave-3: day_length_days), pending(wave-3: obliquity_deg), bare-ok(ratio: year_phase_offset)
#[derive(Debug, Serialize)]
pub struct WorldElem {
    /// Orbital radius, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub orbit_au: f64,
    /// Year length, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub year_days: f64,
    /// Solar-day length, standard days; `None` when tidally locked (no spin).
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field"
    )]
    pub day_length_days: Option<f64>,
    /// Mean axial obliquity, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub obliquity_deg: f64,
    /// Genesis orbital phase offset (turns) so day 0 is an ordinary day.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub year_phase_offset: f64,
}

/// One moon's orbital elements.
/// type-audit: pending(wave-3: sidereal_days), pending(wave-3: distance_mm), bare-ok(ratio: phase_offset), bare-ok(ratio: size_rel), pending(wave-1: inclination_deg), pending(wave-1: node_longitude_deg)
#[derive(Debug, Serialize)]
pub struct MoonElem {
    /// Sidereal orbital period, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sidereal_days: f64,
    /// Genesis synodic-phase offset (turns).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub phase_offset: f64,
    /// Orbital distance from the world, megameters.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_mm: f64,
    /// Angular-diameter ratio (the size-word input).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub size_rel: f64,
    /// Orbital inclination to the anchor's orbital plane, degrees; > 90 is
    /// retrograde (The Reckoning). Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub inclination_deg: f64,
    /// Ecliptic longitude of the ascending node at genesis, degrees.
    /// Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub node_longitude_deg: f64,
}

/// One `scene/system/v1` document: the system's orbital geometry as elements.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct SystemScene {
    /// Always `scene/system/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The central star.
    pub star: StarElem,
    /// The anchor world.
    pub world: WorldElem,
    /// The moons, generation order.
    pub moons: Vec<MoonElem>,
}

/// Build the `scene/system/v1` scene for `world`. Errors when the world has no
/// generated sky (the tier-0 constant sun has no orrery to draw).
pub fn system_scene(world: &World) -> Result<SystemScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let anchor = &system.anchor;
    let day_length_days = match &anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => Some(day.get()),
        hornvale_astronomy::Rotation::Locked => None,
    };
    let moons = system
        .moons
        .iter()
        .enumerate()
        .map(|(i, m)| MoonElem {
            sidereal_days: m.period.get(),
            phase_offset: system
                .forcing
                .moon_phase_offsets
                .get(i)
                .copied()
                .unwrap_or(0.0),
            distance_mm: m.distance.get(),
            size_rel: m.angular_diameter_rel,
            inclination_deg: m.inclination_deg,
            node_longitude_deg: m.node_longitude_deg,
        })
        .collect();
    Ok(SystemScene {
        schema: SYSTEM_SCHEMA.to_string(),
        seed: world.seed.0,
        star: StarElem {
            class_name: system.star.class_name.clone(),
            luminosity_rel: system.star.luminosity.get(),
            hz_inner_au: system.star.habitable_zone.inner().get(),
            hz_outer_au: system.star.habitable_zone.outer().get(),
        },
        world: WorldElem {
            orbit_au: anchor.orbit.get(),
            year_days: anchor.year.get(),
            day_length_days,
            obliquity_deg: system.forcing.obliquity_mean,
            year_phase_offset: system.forcing.year_phase_offset,
        },
        moons,
    })
}

/// Serialize a `SystemScene` to compact JSON (mirrors [`scene_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn system_json(scene: &SystemScene) -> String {
    serde_json::to_string(scene).expect("a SystemScene always serializes")
}

/// The `scene/moons/v1` schema tag.
/// type-audit: bare-ok(identifier-text)
pub const MOONS_SCHEMA: &str = "scene/moons/v1";

/// Newtonian gravitational constant, N·m²/kg² (CODATA recommended value).
const G_SI: f64 = 6.6743e-11;

/// Surface gravity (m/s²) of a uniform sphere from its radius (km) and bulk
/// density (g/cm³). `g = GM/r²` and, for a uniform sphere, `M = (4/3)πρr³`,
/// so substituting gives `g = (4/3)πGρr` — no mass-in-kg term needed, which
/// lets this compose directly with [`hornvale_astronomy::radius_km`] (The
/// Reckoning's real-density radius) without re-deriving mass or duplicating
/// that crate's radius formula. Anchors to the same figure the old
/// constant-density formula encoded: a 1.0-lunar-mass `GiantImpact` moon
/// (density 3.34 g/cm³, radius ≈1737.77 km) comes out to ≈1.6226 m/s²,
/// matching Luna's real 1.62.
fn surface_gravity_ms2(radius_km: f64, density_g_cm3: f64) -> f64 {
    let r_m = radius_km * 1000.0;
    let rho_kg_per_m3 = density_g_cm3 * 1000.0;
    (4.0 / 3.0) * std::f64::consts::PI * G_SI * rho_kg_per_m3 * r_m
}

/// The baseline albedo band, before maria darkening, for a moon's
/// composition: icy reads bright (0.5–0.7, real ice), rocky/impact reads
/// dark (0.1–0.2, real regolith) — supplying the visual referent
/// `bright-icy` lacked before density was real (The Reckoning). Takes the
/// already-classified `is_icy` flag rather than a raw density: this crate
/// holds no density threshold of its own (review follow-up to Task 5b) —
/// `hornvale_astronomy::is_icy` is the single place that decides, so the
/// only boundary-precision test for "how close can a density be to icy"
/// lives in that domain crate, not duplicated here.
fn albedo_band(is_icy: bool) -> (f64, f64) {
    if is_icy { (0.5, 0.7) } else { (0.1, 0.2) }
}

/// The multiplier applied to `maria_fraction` for an icy composition
/// (review follow-up to Task 5b): maria are flood basalts, which an ice
/// body does not have, so a composition-blind `maria_fraction` could put
/// basaltic plains on an ice ball. Damped, not zeroed — real icy bodies
/// still carry resurfaced terrain (Europa's chaos terrain, Enceladus's
/// tiger stripes), just not basaltic maria, so a reduced residual stays
/// physically plausible. 0.3 was chosen judgmentally, not measured: it
/// caps an icy moon's `maria_fraction` at roughly the bottom third of the
/// hash-driven range rather than forcing it to read as pristine (a hard
/// zero), while still making an icy moon visibly less maria-marked than a
/// rocky one of the same mass in the vast majority of draws.
const ICY_MARIA_DAMPING: f64 = 0.3;

/// A subtle near-gray tint, and the four seeded surface descriptors, as a
/// pure hash of the world seed and the moon index — no `Stream` draw. Mass
/// biases cratering/maria so a face reads plausibly: small moons cratered
/// highlands, large moons resurfaced maria plains ("models author, dice
/// roll", 0009). `is_icy` (from `hornvale_astronomy::is_icy`, real bulk
/// density) biases albedo's baseline band and damps `maria_fraction` (The
/// Reckoning; damping is a review follow-up) — an icy moon reads brighter
/// and less maria-marked than a rocky one of the same mass, while the hash
/// channel still perturbs within each band so two moons of a world differ.
fn seeded_descriptors(
    seed: hornvale_kernel::Seed,
    index: usize,
    mass_rel: f64,
    is_icy: bool,
) -> Descriptors {
    // value_noise_2d returns [0,1); integer coords sample the raw lattice
    // hash, one distinct channel per (index, channel) pair.
    let h = |channel: u32| hornvale_kernel::value_noise_2d(seed, index as f64, f64::from(channel));
    // "Largeness" in [0,1] over the drawn mass range [0.05, 2.5].
    let large = ((mass_rel - 0.05) / 2.45).clamp(0.0, 1.0);
    let small = 1.0 - large;

    // Cratering: hash pulled toward 1 as mass falls.
    let cratering = (0.35 * h(0) + 0.65 * small).clamp(0.0, 1.0);
    // Maria: hash pulled up with mass, then damped by cratering so a face is
    // not simultaneously all-craters and all-maria, then damped again for
    // an icy composition (basaltic maria are a rocky-body feature).
    let maria_fraction = ((0.35 * h(1) + 0.65 * large) * (1.0 - 0.5 * cratering)).clamp(0.0, 1.0);
    let maria_fraction = if is_icy {
        maria_fraction * ICY_MARIA_DAMPING
    } else {
        maria_fraction
    };
    // Albedo: a composition-baseline band (bright ice, dark rock), a hash
    // channel perturbing within it, then darkened where maria (dark plains)
    // is high.
    let (albedo_lo, albedo_hi) = albedo_band(is_icy);
    let albedo = (albedo_lo + (albedo_hi - albedo_lo) * h(2)) * (1.0 - 0.6 * maria_fraction);
    let albedo = albedo.clamp(0.04, 0.7);
    // Tint: three near-gray channels, deliberately subtle (moons are gray),
    // enough to tell two moons of a world apart.
    let tint = [
        (0.70 + 0.15 * (h(3) - 0.5)).clamp(0.0, 1.0),
        (0.70 + 0.15 * (h(4) - 0.5)).clamp(0.0, 1.0),
        (0.70 + 0.15 * (h(5) - 0.5)).clamp(0.0, 1.0),
    ];
    Descriptors {
        albedo,
        cratering,
        maria_fraction,
        tint,
    }
}

/// Stable lowercase word for a moon's formation mechanism (The Reckoning) —
/// serialized as text rather than the astronomy domain's enum discriminant,
/// the same convention `surface_class` already follows.
fn formation_word(formation: hornvale_astronomy::Formation) -> &'static str {
    match formation {
        hornvale_astronomy::Formation::GiantImpact => "giant-impact",
        hornvale_astronomy::Formation::Capture => "capture",
    }
}

struct Descriptors {
    albedo: f64,
    cratering: f64,
    maria_fraction: f64,
    tint: [f64; 3],
}

/// The normative `surface_class` classifier: a stable, append-only word for
/// clients that want a name, not a texture. Precedence (most specific
/// first): a bright icy face; else maria-rich; else heavily cratered; else
/// a cratered highland. Thresholds are the reference page's normative
/// table. **Since The Reckoning, `bright-icy` keys off the moon's real bulk
/// density, not the hash-noise `albedo`** — Nathan-ratified: The Faces
/// shipped the word for an icy moon before the model had a concept of ice;
/// this is the referent. Takes the already-classified `is_icy` flag
/// (`hornvale_astronomy::is_icy`) rather than a raw density (review
/// follow-up to Task 5b) — this crate holds no density threshold of its
/// own; the domain decides composition, this classifier only presents it.
/// type-audit: bare-ok(flag: is_icy), bare-ok(ratio: cratering), bare-ok(ratio: maria_fraction), bare-ok(identifier-text: return)
pub fn moon_surface_class(is_icy: bool, cratering: f64, maria_fraction: f64) -> &'static str {
    if is_icy {
        "bright-icy"
    } else if maria_fraction > 0.4 {
        "maria-rich"
    } else if cratering > 0.6 {
        "heavily-cratered"
    } else {
        "cratered-highland"
    }
}

/// One moon's surface document entry. Field order is fixed; every f64
/// quantizes at emit (decision 0033).
/// type-audit: bare-ok(count: index), bare-ok(ratio: mass_rel), bare-ok(ratio: albedo), bare-ok(ratio: cratering), bare-ok(ratio: maria_fraction), bare-ok(identifier-text: surface_class), pending(wave-3: radius_km), pending(wave-3: surface_gravity_ms2), bare-ok(ratio: tint), pending(wave-3: density_g_cm3), bare-ok(identifier-text: formation)
#[derive(Debug, Serialize)]
pub struct MoonSurface {
    /// The moon's generation index (matches `scene/system/v1`).
    pub index: usize,
    /// Mass in lunar masses (the generator's drawn value, surfaced).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub mass_rel: f64,
    /// Physical radius, km — derived from mass and the moon's real bulk
    /// density (`density_g_cm3`); see `hornvale_astronomy::radius_km`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub radius_km: f64,
    /// Surface gravity, m/s² — derived from `radius_km` and that same real
    /// density: `g = (4/3)πGρr` (see `surface_gravity_ms2` in this crate).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub surface_gravity_ms2: f64,
    /// Seeded reflectance in [0.04, 0.7]; composition-biased (bright for
    /// icy, dark for rocky/impact) and darkened where maria is high.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub albedo: f64,
    /// Seeded cratering intensity in [0, 1]; biased high for small moons.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub cratering: f64,
    /// Seeded smooth-maria fraction in [0, 1]; biased high for large moons,
    /// damped for an icy composition (maria are basaltic plains, which an
    /// ice body does not have).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub maria_fraction: f64,
    /// Seeded near-gray linear-RGB tint, each channel in [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub tint: [f64; 3],
    /// Derived descriptive class name (see `moon_surface_class`).
    pub surface_class: String,
    /// Bulk density, g/cm³ (The Reckoning) — the moon's real drawn/derived
    /// density (`hornvale_astronomy::Moon::density`), the physical basis
    /// `radius_km`, `surface_gravity_ms2`, and `surface_class`'s `bright-icy`
    /// branch all derive from. Additive field; appended after every
    /// existing field per the schema's stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub density_g_cm3: f64,
    /// How this moon formed (The Reckoning): `"giant-impact"` or
    /// `"capture"` — see `hornvale_astronomy::Formation`. Additive field;
    /// appended after every existing field per the schema's stability
    /// contract.
    pub formation: String,
}

/// One `scene/moons/v1` document: each moon's surface as derived physics
/// plus seeded procedural descriptors.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct MoonsScene {
    /// Always `scene/moons/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The moons, in generation order (matches `scene/system/v1`).
    pub moons: Vec<MoonSurface>,
}

/// Build the `scene/moons/v1` scene for `world`. Errors when the world has
/// no generated sky (the tier-0 constant sun has no moons) — mirrors
/// [`system_scene`]. A pure read plus hash: consumes no `Stream` draws.
pub fn moons_scene(world: &World) -> Result<MoonsScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let moons = system
        .moons
        .iter()
        .enumerate()
        .map(|(index, m)| {
            let mass_rel = m.mass.get();
            let density_g_cm3 = m.density.get();
            // Composition (review follow-up to Task 5b): the domain's own
            // predicate, not a local copy of a density threshold.
            let is_icy = hornvale_astronomy::is_icy(m);
            // Real-density physics (The Reckoning): call the domain's own
            // radius formula rather than re-deriving it, then compose
            // gravity from that radius and the same real density.
            let radius_km = hornvale_astronomy::radius_km(m);
            let gravity_ms2 = surface_gravity_ms2(radius_km, density_g_cm3);
            let d = seeded_descriptors(world.seed, index, mass_rel, is_icy);
            MoonSurface {
                index,
                mass_rel,
                radius_km,
                surface_gravity_ms2: gravity_ms2,
                albedo: d.albedo,
                cratering: d.cratering,
                maria_fraction: d.maria_fraction,
                tint: d.tint,
                surface_class: moon_surface_class(is_icy, d.cratering, d.maria_fraction)
                    .to_string(),
                density_g_cm3,
                formation: formation_word(m.formation).to_string(),
            }
        })
        .collect();
    Ok(MoonsScene {
        schema: MOONS_SCHEMA.to_string(),
        seed: world.seed.0,
        moons,
    })
}

/// Serialize a `MoonsScene` to compact JSON (mirrors [`system_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn moons_json(scene: &MoonsScene) -> String {
    serde_json::to_string(scene).expect("a MoonsScene always serializes")
}

/// The `scene/neighbors/v1` schema tag.
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBORS_SCHEMA: &str = "scene/neighbors/v1";

/// One notable neighbor star as drawn by the generator, brightest first.
/// type-audit: bare-ok(count: index), bare-ok(identifier-text: class_name), bare-ok(identifier-text: color), pending(wave-1: distance_ly), bare-ok(ratio: brightness_rel), pending(wave-1: ra_deg), pending(wave-1: dec_deg)
#[derive(Debug, Serialize)]
pub struct NeighborElem {
    /// Generation index (stable identity; matches the ledger entity order).
    pub index: usize,
    /// Prose spectral class ("red giant"; `color` is the producer's color
    /// word, not this).
    pub class_name: String,
    /// The producer's color word (e.g. "smoldering red").
    pub color: String,
    /// Distance in light-years (drawn, 4-80).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_ly: f64,
    /// Apparent brightness, relative units (derived L/d²).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub brightness_rel: f64,
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
}

/// One anonymous background field star (texture, not a ledger entity).
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg), bare-ok(count: magnitude_class)
#[derive(Debug, Serialize)]
pub struct FieldStarElem {
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
    /// Brightness class, 1 (brightest) ..= 5 (faintest).
    pub magnitude_class: u8,
}

/// One `scene/neighbors/v1` document: the night sky's two populations.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct NeighborsScene {
    /// Always `scene/neighbors/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The notable neighbors, generation order (brightest first).
    pub neighbors: Vec<NeighborElem>,
    /// The background starfield, derivation order.
    pub stars: Vec<FieldStarElem>,
}

/// Build the `scene/neighbors/v1` scene for `world`. Errors when the world
/// has no generated sky — mirrors [`moons_scene`]. Pure reads: consumes no
/// genesis draws (the starfield derives on demand from the astronomy seed,
/// exactly as the almanac's figures path does).
pub fn neighbors_scene(world: &World) -> Result<NeighborsScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let neighbors = system
        .neighbors
        .iter()
        .enumerate()
        .map(|(index, n)| NeighborElem {
            index,
            class_name: hornvale_astronomy::class_name(n.class).to_string(),
            color: n.color.clone(),
            distance_ly: n.distance.get(),
            brightness_rel: n.apparent_brightness,
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        })
        .collect();
    let astronomy_seed = world.seed.derive(hornvale_astronomy::streams::ROOT);
    let stars = hornvale_astronomy::starfield(astronomy_seed)
        .into_iter()
        .map(|s| FieldStarElem {
            ra_deg: s.ra_deg,
            dec_deg: s.dec_deg,
            magnitude_class: s.magnitude_class,
        })
        .collect();
    Ok(NeighborsScene {
        schema: NEIGHBORS_SCHEMA.to_string(),
        seed: world.seed.0,
        neighbors,
        stars,
    })
}

/// Serialize a `NeighborsScene` to compact JSON (mirrors [`moons_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn neighbors_json(scene: &NeighborsScene) -> String {
    serde_json::to_string(scene).expect("a NeighborsScene always serializes")
}

#[cfg(test)]
mod tests {
    use super::*;

    // Build a world exactly the way the CLI's tests do (see cli/src/repl.rs
    // tests for the canonical minimal build_world call). Each pin argument's
    // type is fixed by build_world's signature, so `&Default::default()`
    // infers; add a domain crate to [dev-dependencies] only if a pin type
    // turns out not to be re-exported by worldgen.
    fn world() -> World {
        hornvale_worldgen::build_world(
            hornvale_kernel::Seed(1),
            &Default::default(),
            hornvale_worldgen::SkyChoice::Constant,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 1 builds")
    }

    fn mooned_world() -> World {
        gen_world_for(42)
    }

    fn gen_world_for(seed: u64) -> World {
        hornvale_worldgen::build_world(
            hornvale_kernel::Seed(seed),
            &Default::default(),
            hornvale_worldgen::SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed builds a generated sky")
    }

    /// Significant digits in a JSON number token (sign, leading zeros, the
    /// decimal point, and trailing zeros stripped). A cheap proxy for
    /// "quantized": raw libm-derived floats carry 15–16.
    fn significant_digits(token: &str) -> usize {
        let t = token.trim_start_matches('-').replace('.', "");
        let t = t.trim_start_matches('0');
        let t = t.trim_end_matches('0');
        t.len()
    }

    #[test]
    fn serialized_tiles_carry_no_more_than_eight_significant_digits() {
        let json = scene_json(&tiles_scene(&world(), 32).unwrap());
        // Split into candidate number tokens and check each.
        for token in json.split(|c: char| !(c.is_ascii_digit() || c == '.' || c == '-')) {
            if token.is_empty() || !token.contains('.') {
                continue;
            }
            assert!(
                significant_digits(token) <= 8,
                "un-quantized float in scene JSON: {token}"
            );
        }
    }

    #[test]
    fn scene_is_byte_deterministic() {
        let w = world();
        let a = scene_json(&tiles_scene(&w, 32).unwrap());
        let b = scene_json(&tiles_scene(&w, 32).unwrap());
        assert_eq!(a, b);
        let rebuilt = world();
        assert_eq!(a, scene_json(&tiles_scene(&rebuilt, 32).unwrap()));
    }

    #[test]
    fn layers_are_sized_and_legend_is_the_catalog() {
        let scene = tiles_scene(&world(), 32).unwrap();
        assert_eq!(scene.height, 16);
        let tiles = (scene.width * scene.height) as usize;
        assert_eq!(scene.elevation_m.len(), tiles);
        assert_eq!(scene.ocean.len(), tiles);
        assert_eq!(scene.biome.len(), tiles);
        assert_eq!(scene.plate.len(), tiles);
        assert_eq!(scene.unrest.len(), tiles);
        assert_eq!(scene.biome_legend.len(), 22);
        assert!(scene.biome.iter().all(|&i| (i as usize) < 22));
    }

    #[test]
    fn climate_layers_are_sized_and_present() {
        let scene = tiles_scene(&world(), 32).unwrap(); // seed-1 constant sky: spins, obliquity 23.5
        let tiles = (scene.width * scene.height) as usize;
        assert_eq!(scene.t_mean_c.len(), tiles);
        assert_eq!(scene.t_swing_c.len(), tiles);
        assert_eq!(scene.moisture.len(), tiles);
        assert!(scene.moisture.iter().all(|&m| (0.0..=1.0).contains(&m)));
        assert_eq!(scene.season_period_days, 365.25); // constant-sun default year
        assert_eq!(scene.circulation_bands, Some(3)); // Earth-like day → 3 bands
        // A spinning, obliquity-23.5 world has a nonzero swing somewhere.
        assert!(scene.t_swing_c.iter().any(|&s| s != 0.0));
        // Signed: some tile north-positive, some south-negative.
        assert!(scene.t_swing_c.iter().any(|&s| s > 0.0));
        assert!(scene.t_swing_c.iter().any(|&s| s < 0.0));
    }

    #[test]
    fn locked_world_omits_circulation_bands_and_zeroes_swing() {
        use hornvale_astronomy::{RotationPin, SkyPins};
        use hornvale_kernel::Seed;
        use hornvale_worldgen::{SkyChoice, build_world};
        let sky = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..Default::default()
        };
        let world = build_world(
            Seed(42),
            &sky,
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 42 builds locked");
        let scene = tiles_scene(&world, 32).unwrap();
        assert_eq!(scene.circulation_bands, None, "locked world has no bands");
        assert!(
            scene.t_swing_c.iter().all(|&s| s == 0.0),
            "locked world has no seasonal swing"
        );
        // The omitted field must not appear in the JSON.
        let json = scene_json(&scene);
        assert!(
            !json.contains("circulation_bands"),
            "absent field must not serialize"
        );
        assert!(json.contains("season_period_days"));
    }

    #[test]
    fn width_violations_are_loud() {
        // TilesScene has no PartialEq, so assert on the error side only.
        let w = world();
        assert!(matches!(
            tiles_scene(&w, 15),
            Err(SceneError::WidthOutOfRange(15))
        ));
        assert!(matches!(
            tiles_scene(&w, 8),
            Err(SceneError::WidthOutOfRange(8))
        ));
        assert!(matches!(
            tiles_scene(&w, 2048),
            Err(SceneError::WidthOutOfRange(2048))
        ));
        assert!(matches!(tiles_scene(&w, 17), Err(SceneError::WidthOdd(17))));
        assert!(tiles_scene(&w, 18).is_ok());
    }

    #[test]
    fn flagship_appears_exactly_once() {
        let scene = tiles_scene(&world(), 32).unwrap();
        let flagships = scene
            .features
            .iter()
            .filter(|f| f.kind == "flagship")
            .count();
        assert!(flagships <= 1);
        if let Some(f) = scene.features.iter().find(|f| f.kind == "flagship") {
            assert_eq!(
                scene.features.iter().filter(|g| g.name == f.name).count(),
                1,
                "flagship duplicated as a settlement"
            );
        }
    }

    #[test]
    fn system_scene_has_the_schema_moons_and_is_deterministic() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::{SkyChoice, build_world};
        // `gen` is a reserved keyword under this workspace's 2024 edition
        // (the brief's original name); `gen_world` sidesteps it.
        let gen_world = || {
            build_world(
                Seed(42),
                &Default::default(),
                SkyChoice::Generated,
                &Default::default(),
                &Default::default(),
            )
            .expect("seed 42 builds")
        };
        let scene = system_scene(&gen_world()).expect("generated world has a system");
        assert_eq!(scene.schema, "scene/system/v1");
        assert_eq!(scene.seed, 42);
        assert_eq!(scene.moons.len(), 2, "seed 42 has two moons");
        assert!(scene.world.year_days > 0.0);
        assert!(scene.world.day_length_days.is_some(), "seed 42 spins");
        assert!(scene.star.hz_inner_au < scene.star.hz_outer_au);
        for m in &scene.moons {
            assert!((0.0..=180.0).contains(&m.inclination_deg));
            assert!((0.0..360.0).contains(&m.node_longitude_deg));
        }
        assert!(
            scene.moons.iter().any(|m| m.inclination_deg > 90.0),
            "seed 42 has a retrograde captured moon (The Reckoning)"
        );
        // Byte-identical when serialized: determinism.
        assert_eq!(
            system_json(&scene),
            system_json(&system_scene(&gen_world()).unwrap())
        );
    }

    #[test]
    fn temperature_grid_matches_direct_sampling_and_zero_phase_mean() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::{SkyChoice, build_world, climate_of};
        let world = build_world(
            Seed(42),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 42 builds");
        let width = 32;
        let height = width / 2;
        let climate = climate_of(&world).expect("climate builds");
        let climate_index = NearestCellIndex::new(climate.geosphere());
        let day = 91.3;
        let grid = temperature_grid(&world, width, day).expect("grid builds");
        assert_eq!(grid.len(), (width * height) as usize);

        // Independently reconstruct the same lattice sampling in the test
        // (not by calling into tiles_scene's loop) and compare tile-by-tile
        // against the direct `temperature_at` reading.
        let mut i = 0;
        for py in 0..height {
            let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
            for px in 0..width {
                let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
                let c_cell = climate_index.nearest(climate.geosphere(), latitude, longitude);
                let expected = climate.temperature_at(c_cell, day).get();
                assert_eq!(grid[i], expected, "tile {i} mismatch at day {day}");
                i += 1;
            }
        }

        // At day 0 the seasonal sine term is exactly zero, so temperature_grid
        // must agree with tiles_scene's t_mean_c (mean at zero phase).
        let zero_grid = temperature_grid(&world, width, 0.0).expect("grid builds");
        let scene = tiles_scene(&world, width).expect("scene builds");
        for (g, m) in zero_grid.iter().zip(scene.t_mean_c.iter()) {
            assert_eq!(*g, *m, "day-0 temperature_grid must equal t_mean_c");
        }
    }

    #[test]
    fn system_scene_errors_on_a_constant_sun() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::{SkyChoice, build_world};
        let world = build_world(
            Seed(42),
            &Default::default(),
            SkyChoice::Constant,
            &Default::default(),
            &Default::default(),
        )
        .unwrap();
        assert!(system_scene(&world).is_err(), "constant sun has no system");
    }

    #[test]
    fn moons_scene_has_schema_indices_and_is_deterministic() {
        let a = moons_scene(&mooned_world()).expect("generated world has moons");
        assert_eq!(a.schema, "scene/moons/v1");
        assert_eq!(a.seed, mooned_world().seed.0);
        assert_eq!(a.moons.len(), 2, "seed 42 has two moons");
        assert_eq!(a.moons[0].index, 0);
        assert_eq!(a.moons[1].index, 1);
        // Byte-identical on rebuild (determinism from the seed alone).
        assert_eq!(
            moons_json(&a),
            moons_json(&moons_scene(&mooned_world()).unwrap())
        );
    }

    #[test]
    fn derived_params_follow_real_density_physics() {
        // Independently recompute r = (3M/4piρ)^(1/3) and g = GM/r^2 from
        // each moon's own surfaced mass_rel/density_g_cm3 fields — not by
        // calling hornvale_astronomy::radius_km or super::surface_gravity_ms2
        // again — so this catches a wiring bug (wrong mass/density passed
        // through) as well as a physics regression.
        const LUNAR_MASS_KG: f64 = 7.342e22;
        let scene = moons_scene(&mooned_world()).unwrap();
        assert_eq!(scene.moons.len(), 2, "seed 42 has two moons");
        for m in &scene.moons {
            let mass_kg = m.mass_rel * LUNAR_MASS_KG;
            let rho_kg_m3 = m.density_g_cm3 * 1000.0;
            let v_m3 = 3.0 * mass_kg / (4.0 * std::f64::consts::PI * rho_kg_m3);
            let expected_r_km = hornvale_kernel::math::powf(v_m3, 1.0 / 3.0) / 1000.0;
            assert!(
                (m.radius_km - expected_r_km).abs() < 1e-6,
                "radius must equal (3M/4piρ)^(1/3) at density {}: got {}, expected {}",
                m.density_g_cm3,
                m.radius_km,
                expected_r_km
            );
            const G_SI: f64 = 6.6743e-11;
            let expected_g = G_SI * mass_kg / (expected_r_km * 1000.0 * expected_r_km * 1000.0);
            assert!(
                (m.surface_gravity_ms2 - expected_g).abs() < 1e-9,
                "gravity must equal GM/r^2: got {}, expected {}",
                m.surface_gravity_ms2,
                expected_g
            );
        }
    }

    #[test]
    fn derived_params_equal_luna_at_one_lunar_mass_and_giant_impact_density() {
        use hornvale_astronomy::{GramsPerCm3, LunarMasses};
        // Base every non-overridden field on a real generated moon (never a
        // from-scratch literal), then override mass/density to the Luna
        // calibration point — same pattern the astronomy domain's own
        // `radius_follows_from_mass_and_real_density_not_an_assumption` uses.
        let w = mooned_world();
        let sky = hornvale_worldgen::sky_of(&w).unwrap();
        let base = sky.system().unwrap().moons[0].clone();
        let luna = hornvale_astronomy::Moon {
            mass: LunarMasses::new(1.0).unwrap(),
            density: GramsPerCm3::new(3.34).unwrap(),
            ..base
        };
        let r = hornvale_astronomy::radius_km(&luna);
        // Real physics from the real lunar mass (7.342e22 kg) and density
        // (3.34 g/cm3) gives ~1737.77 km — a hair above the old assumed-
        // constant anchor of 1737.4 km (the two agree to 4 significant
        // figures because the old anchor WAS Luna's real observed radius,
        // and 1737.77 is what the same mass/density pair yields when run
        // through r = (3M/4piρ)^(1/3) instead of being looked up).
        assert!((r - 1737.77).abs() < 0.01, "radius came out {r} km");
        let g = super::surface_gravity_ms2(r, 3.34);
        assert!((g - 1.6226).abs() < 0.001, "gravity came out {g} m/s2");

        // An icy body of the same mass is ~28% larger (task calibration:
        // (3.34/1.6)^(1/3) - 1 ≈ 0.278).
        let icy = hornvale_astronomy::Moon {
            density: GramsPerCm3::new(1.6).unwrap(),
            ..luna
        };
        let r_icy = hornvale_astronomy::radius_km(&icy);
        let ratio = r_icy / r;
        assert!(
            (1.27..1.29).contains(&ratio),
            "icy/impact radius ratio came out {ratio}"
        );
    }

    #[test]
    fn albedo_band_is_bright_for_icy_and_dark_for_rocky() {
        // `albedo_band` takes the already-classified flag, not a raw
        // density (review follow-up to Task 5b) — so this crate has only
        // the two branches to check; the "how close can a density be to
        // icy" boundary-precision question now belongs entirely to
        // `hornvale_astronomy::is_icy`'s own test.
        assert_eq!(super::albedo_band(true), (0.5, 0.7), "icy composition");
        assert_eq!(super::albedo_band(false), (0.1, 0.2), "non-icy composition");
    }

    #[test]
    fn descriptors_are_in_range() {
        let scene = moons_scene(&mooned_world()).unwrap();
        for m in &scene.moons {
            assert!((0.04..=0.7).contains(&m.albedo), "albedo in [0.04,0.7]");
            assert!((0.0..=1.0).contains(&m.cratering));
            assert!((0.0..=1.0).contains(&m.maria_fraction));
            for c in m.tint {
                assert!((0.0..=1.0).contains(&c));
            }
        }
    }

    #[test]
    fn icy_albedo_can_exceed_the_old_zero_point_five_cap() {
        // Seed 42's two real moons both land below 0.5 (neither is icy), so
        // `descriptors_are_in_range` alone never exercises the widened cap.
        // This directly proves an icy composition needs headroom above the
        // old [0.04,0.5] range: at least one index/seed with density 1.6
        // must land above 0.5, still within [0.04,0.7].
        let found_above_half = (0u64..50).any(|seed| {
            let d = super::seeded_descriptors(hornvale_kernel::Seed(seed), 0, 1.0, true);
            d.albedo > 0.5
        });
        assert!(
            found_above_half,
            "no icy-density draw exceeded the old 0.5 cap across 50 seeds"
        );
        // And the clamp still holds even at the icy band's own upper bound.
        for seed in 0u64..50 {
            let d = super::seeded_descriptors(hornvale_kernel::Seed(seed), 0, 1.0, true);
            assert!(
                (0.04..=0.7).contains(&d.albedo),
                "seed {seed}: {}",
                d.albedo
            );
        }
    }

    #[test]
    fn icy_composition_damps_maria_fraction_but_never_zeroes_it() {
        // Review follow-up: composition-blind maria_fraction let an icy
        // moon read as basaltic plains on an ice ball. Compare the SAME
        // (seed, index, mass) pair under is_icy=false vs true so only the
        // damping under test varies; every seed's icy value must be
        // strictly smaller than its rocky counterpart whenever the rocky
        // value is nonzero, and the damping ratio must match
        // ICY_MARIA_DAMPING exactly (a smooth multiplicative damping, not
        // a hard zero).
        let mut compared_a_nonzero_case = false;
        for seed in 0u64..50 {
            let s = hornvale_kernel::Seed(seed);
            let rocky = super::seeded_descriptors(s, 0, 1.0, false);
            let icy = super::seeded_descriptors(s, 0, 1.0, true);
            if rocky.maria_fraction > 0.0 {
                compared_a_nonzero_case = true;
                assert!(
                    icy.maria_fraction < rocky.maria_fraction,
                    "seed {seed}: icy maria_fraction {} must be damped below rocky {}",
                    icy.maria_fraction,
                    rocky.maria_fraction
                );
                assert!(
                    (icy.maria_fraction - rocky.maria_fraction * super::ICY_MARIA_DAMPING).abs()
                        < 1e-12,
                    "seed {seed}: damping must be exactly the ICY_MARIA_DAMPING multiplier"
                );
            }
        }
        assert!(
            compared_a_nonzero_case,
            "no seed produced a nonzero rocky maria_fraction to damp against"
        );
    }

    #[test]
    fn moons_scene_surfaces_density_and_formation() {
        // Seed 42: moon 0 is GiantImpact (density 3.34), moon 1 is Capture
        // (density either the rocky or icy reservoir).
        let scene = moons_scene(&mooned_world()).unwrap();
        assert_eq!(scene.moons[0].formation, "giant-impact");
        assert!(
            (scene.moons[0].density_g_cm3 - 3.34).abs() < 1e-9,
            "giant-impact density must be the derived constant 3.34, got {}",
            scene.moons[0].density_g_cm3
        );
        assert_eq!(scene.moons[1].formation, "capture");
        assert!(
            scene.moons[1].density_g_cm3 == 3.0 || scene.moons[1].density_g_cm3 == 1.6,
            "capture density must be one of the two drawn reservoirs, got {}",
            scene.moons[1].density_g_cm3
        );
    }

    #[test]
    fn mass_bias_actually_bites_across_seeds() {
        // The bias is a pure function of (seed, index, mass) — exercise it
        // directly over synthetic masses spanning the drawn range [0.05, 2.5]
        // (real moon mass is 0.05 + u·2.45), no world-building. Small moons
        // must read higher cratering and large moons higher maria in aggregate
        // (the bias, not a single-seed fluke).
        let small_masses = [0.05, 0.2, 0.4, 0.6];
        let large_masses = [1.6, 1.9, 2.2, 2.5];
        let mut small_cratering = 0.0;
        let mut large_cratering = 0.0;
        let mut small_maria = 0.0;
        let mut large_maria = 0.0;
        // Composition held fixed (non-icy, so the maria damping added by
        // the icy-composition review follow-up never fires) across every
        // call so only the mass bias under test varies.
        const FIXED_IS_ICY: bool = false;
        for seed in 1..200u64 {
            let s = hornvale_kernel::Seed(seed);
            for (i, &m) in small_masses.iter().enumerate() {
                let d = super::seeded_descriptors(s, i, m, FIXED_IS_ICY);
                small_cratering += d.cratering;
                small_maria += d.maria_fraction;
            }
            for (i, &m) in large_masses.iter().enumerate() {
                let d = super::seeded_descriptors(s, i, m, FIXED_IS_ICY);
                large_cratering += d.cratering;
                large_maria += d.maria_fraction;
            }
        }
        assert!(
            small_cratering > large_cratering,
            "small moons more cratered in aggregate"
        );
        assert!(
            large_maria > small_maria,
            "large moons more maria in aggregate"
        );
    }

    #[test]
    fn moons_scene_errors_on_a_constant_sun() {
        assert!(moons_scene(&world()).is_err(), "constant sun has no moons");
    }

    #[test]
    fn moons_scene_does_not_consume_draws_or_mutate_the_world() {
        // The save-format guard: the document is a pure read + hash, so building
        // it leaves the world byte-identical (no Stream draw, no mutation).
        let w = mooned_world();
        let before = serde_json::to_string(&w).unwrap();
        let _ = moons_scene(&w).unwrap();
        let after = serde_json::to_string(&w).unwrap();
        assert_eq!(before, after, "moons_scene must not alter the world");
    }

    #[test]
    fn surface_class_table_is_normative() {
        // is_icy wins even under high cratering/maria — it is checked
        // first (most specific), per the reference page's normative table.
        // `moon_surface_class` takes the already-classified flag, not a
        // raw density (review follow-up to Task 5b): the "which densities
        // actually read icy" question — formerly this test's job via
        // literal density values (1.6, 1.9, 3.0, 3.34) — now belongs
        // entirely to `hornvale_astronomy::is_icy`'s own
        // `is_icy_agrees_with_the_domains_own_density_reservoirs` test, so
        // it is not duplicated here.
        assert_eq!(moon_surface_class(true, 0.9, 0.9), "bright-icy");
        assert_eq!(moon_surface_class(false, 0.1, 0.5), "maria-rich");
        assert_eq!(moon_surface_class(false, 0.8, 0.1), "heavily-cratered");
        assert_eq!(moon_surface_class(false, 0.3, 0.1), "cratered-highland");
    }

    #[test]
    fn neighbors_scene_has_schema_populations_and_is_deterministic() {
        let a = neighbors_scene(&mooned_world()).expect("generated world has a sky");
        assert_eq!(a.schema, "scene/neighbors/v1");
        assert_eq!(a.seed, mooned_world().seed.0);
        assert!(
            (2..=5).contains(&a.neighbors.len()),
            "2-5 notable neighbors"
        );
        assert!((100..=300).contains(&a.stars.len()), "100-300 field stars");
        for (i, n) in a.neighbors.iter().enumerate() {
            assert_eq!(n.index, i);
        }
        // Brightest first — the generator's own ordering, preserved.
        for w in a.neighbors.windows(2) {
            assert!(w[0].brightness_rel >= w[1].brightness_rel);
        }
        // Byte-identical on rebuild (determinism from the seed alone).
        assert_eq!(
            neighbors_json(&a),
            neighbors_json(&neighbors_scene(&mooned_world()).unwrap())
        );
    }

    #[test]
    fn neighbors_scene_fields_are_in_range() {
        let a = neighbors_scene(&mooned_world()).unwrap();
        for n in &a.neighbors {
            assert!((-90.0..=90.0).contains(&n.dec_deg));
            assert!((0.0..360.0).contains(&n.ra_deg));
            assert!(n.brightness_rel > 0.0);
            assert!((4.0..=80.0).contains(&n.distance_ly));
            assert!(!n.class_name.is_empty() && !n.color.is_empty());
        }
        for s in &a.stars {
            assert!((-90.0..=90.0).contains(&s.dec_deg));
            assert!((0.0..360.0).contains(&s.ra_deg));
            assert!((1..=5).contains(&s.magnitude_class));
        }
    }

    #[test]
    fn neighbors_scene_errors_on_a_constant_sun() {
        assert!(
            neighbors_scene(&world()).is_err(),
            "constant sun has no neighbors"
        );
    }

    #[test]
    fn neighbors_scene_does_not_consume_draws_or_mutate_the_world() {
        // The save-format guard: the document is a pure read (plus an
        // on-demand starfield derivation from the astronomy seed), so
        // building it leaves the world byte-identical.
        let w = mooned_world();
        let before = serde_json::to_string(&w).unwrap();
        let _ = neighbors_scene(&w).unwrap();
        let after = serde_json::to_string(&w).unwrap();
        assert_eq!(before, after, "neighbors_scene must not alter the world");
    }
}
