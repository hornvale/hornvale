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
/// type-audit: pending(wave-3: sidereal_days), pending(wave-3: distance_mm), bare-ok(ratio: phase_offset), bare-ok(ratio: size_rel)
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
pub const MOONS_SCHEMA: &str = "scene/moons/v1";

/// Luna's reference radius, km (the constant-density anchor).
const LUNA_RADIUS_KM: f64 = 1737.4;
/// Luna's reference surface gravity, m/s².
const LUNA_GRAVITY_MS2: f64 = 1.62;

/// Physical radius (km) and surface gravity (m/s²) derived from a moon's
/// mass in lunar masses, at an assumed constant density equal to Luna's:
/// volume ∝ mass ⇒ radius ∝ mass^(1/3), and g = GM/r² reduces to
/// g ∝ mass^(1/3). Uses the kernel's libm-routed `powf` for cross-platform
/// byte-identity (decision 0041).
pub(crate) fn derived_from_mass(mass_rel: f64) -> (f64, f64) {
    let f = hornvale_kernel::math::powf(mass_rel, 1.0 / 3.0);
    (LUNA_RADIUS_KM * f, LUNA_GRAVITY_MS2 * f)
}

/// A subtle near-gray tint, and the four seeded surface descriptors, as a
/// pure hash of the world seed and the moon index — no `Stream` draw. Mass
/// biases each so a face reads plausibly: small moons cratered highlands,
/// large moons resurfaced maria plains ("models author, dice roll", 0009).
fn seeded_descriptors(seed: hornvale_kernel::Seed, index: usize, mass_rel: f64) -> Descriptors {
    // value_noise_2d returns [0,1); integer coords sample the raw lattice
    // hash, one distinct channel per (index, channel) pair.
    let h = |channel: u32| hornvale_kernel::value_noise_2d(seed, index as f64, f64::from(channel));
    // "Largeness" in [0,1] over the drawn mass range [0.05, 2.5].
    let large = ((mass_rel - 0.05) / 2.45).clamp(0.0, 1.0);
    let small = 1.0 - large;

    // Cratering: hash pulled toward 1 as mass falls.
    let cratering = (0.35 * h(0) + 0.65 * small).clamp(0.0, 1.0);
    // Maria: hash pulled up with mass, then damped by cratering so a face is
    // not simultaneously all-craters and all-maria.
    let maria_fraction = ((0.35 * h(1) + 0.65 * large) * (1.0 - 0.5 * cratering)).clamp(0.0, 1.0);
    // Albedo in [0.04,0.5], darkened where maria (dark plains) is high.
    let albedo = (0.04 + 0.46 * h(2)) * (1.0 - 0.6 * maria_fraction);
    let albedo = albedo.clamp(0.04, 0.5);
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

struct Descriptors {
    albedo: f64,
    cratering: f64,
    maria_fraction: f64,
    tint: [f64; 3],
}

/// The normative `surface_class` classifier over the descriptors: a stable,
/// append-only word for clients that want a name, not a texture. Precedence
/// (most specific first): a bright icy face; else maria-rich; else heavily
/// cratered; else a cratered highland. Thresholds are the reference page's
/// normative table.
pub fn moon_surface_class(albedo: f64, cratering: f64, maria_fraction: f64) -> &'static str {
    if albedo > 0.4 {
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
/// type-audit: bare-ok(count: index), bare-ok(ratio: mass_rel), bare-ok(ratio: albedo), bare-ok(ratio: cratering), bare-ok(ratio: maria_fraction), bare-ok(identifier-text: surface_class)
#[derive(Debug, Serialize)]
pub struct MoonSurface {
    /// The moon's generation index (matches `scene/system/v1`).
    pub index: usize,
    /// Mass in lunar masses (the generator's drawn value, surfaced).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub mass_rel: f64,
    /// Physical radius, km — derived from mass at constant lunar density.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub radius_km: f64,
    /// Surface gravity, m/s² — derived, `1.62 × mass^(1/3)`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub surface_gravity_ms2: f64,
    /// Seeded reflectance in [0.04, 0.5]; darkened where maria is high.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub albedo: f64,
    /// Seeded cratering intensity in [0, 1]; biased high for small moons.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub cratering: f64,
    /// Seeded smooth-maria fraction in [0, 1]; biased high for large moons.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub maria_fraction: f64,
    /// Seeded near-gray linear-RGB tint, each channel in [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub tint: [f64; 3],
    /// Derived descriptive class name (see `moon_surface_class`).
    pub surface_class: String,
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
            let (radius_km, surface_gravity_ms2) = derived_from_mass(mass_rel);
            let d = seeded_descriptors(world.seed, index, mass_rel);
            MoonSurface {
                index,
                mass_rel,
                radius_km,
                surface_gravity_ms2,
                albedo: d.albedo,
                cratering: d.cratering,
                maria_fraction: d.maria_fraction,
                tint: d.tint,
                surface_class: moon_surface_class(d.albedo, d.cratering, d.maria_fraction)
                    .to_string(),
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
    fn derived_params_follow_the_constant_density_physics() {
        let scene = moons_scene(&mooned_world()).unwrap();
        for m in &scene.moons {
            let f = hornvale_kernel::math::powf(m.mass_rel, 1.0 / 3.0);
            assert!(
                (m.radius_km - 1737.4 * f).abs() < 1e-6,
                "radius = 1737.4·mass^(1/3)"
            );
            assert!(
                (m.surface_gravity_ms2 - 1.62 * f).abs() < 1e-6,
                "g = 1.62·mass^(1/3)"
            );
        }
    }

    #[test]
    fn derived_params_equal_luna_at_one_lunar_mass() {
        // A unit-mass moon reproduces Luna's reference radius and gravity.
        let (r, g) = super::derived_from_mass(1.0);
        assert!((r - 1737.4).abs() < 1e-9);
        assert!((g - 1.62).abs() < 1e-9);
    }

    #[test]
    fn descriptors_are_in_range() {
        let scene = moons_scene(&mooned_world()).unwrap();
        for m in &scene.moons {
            assert!((0.04..=0.5).contains(&m.albedo), "albedo in [0.04,0.5]");
            assert!((0.0..=1.0).contains(&m.cratering));
            assert!((0.0..=1.0).contains(&m.maria_fraction));
            for c in m.tint {
                assert!((0.0..=1.0).contains(&c));
            }
        }
    }

    #[test]
    fn mass_bias_actually_bites_across_seeds() {
        // Over many seeds, small moons read higher cratering and large moons
        // higher maria than chance — the mass bias, not a single-seed fluke.
        let mut small_cratering = 0.0;
        let mut large_cratering = 0.0;
        let mut small_maria = 0.0;
        let mut large_maria = 0.0;
        let mut n = 0u32;
        for seed in 1..200u64 {
            let Ok(scene) = moons_scene(&gen_world_for(seed)) else {
                continue;
            };
            for m in &scene.moons {
                if m.mass_rel < 0.8 {
                    small_cratering += m.cratering;
                    small_maria += m.maria_fraction;
                } else {
                    large_cratering += m.cratering;
                    large_maria += m.maria_fraction;
                }
            }
            n += 1;
        }
        assert!(n > 20, "enough mooned worlds sampled");
        // Normalize is unnecessary for the direction test; means suffice because
        // the bias term dominates the hash term.
        assert!(
            small_cratering / large_cratering.max(1e-9) > 1.0,
            "small moons more cratered in aggregate"
        );
        assert!(
            large_maria / small_maria.max(1e-9) > 1.0,
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
        assert_eq!(moon_surface_class(0.45, 0.9, 0.1), "bright-icy");
        assert_eq!(moon_surface_class(0.2, 0.1, 0.5), "maria-rich");
        assert_eq!(moon_surface_class(0.2, 0.8, 0.1), "heavily-cratered");
        assert_eq!(moon_surface_class(0.2, 0.3, 0.1), "cratered-highland");
    }
}
