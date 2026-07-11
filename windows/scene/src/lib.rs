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
/// type-audit: bare-ok(diagnostic-value: WidthOdd.0), bare-ok(diagnostic-value: WidthOutOfRange.0), bare-ok(prose: Build.0)
#[derive(Debug, Clone, PartialEq)]
pub enum SceneError {
    /// Width must be even (height is width / 2).
    WidthOdd(u32),
    /// Width must lie in `MIN_WIDTH..=MAX_WIDTH`.
    WidthOutOfRange(u32),
    /// The world could not be rebuilt from its ledger.
    Build(String),
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
/// type-audit: bare-ok(identifier-text: schema), bare-ok(identifier-text: biome_legend), bare-ok(constructor-edge: seed), bare-ok(count: width), bare-ok(count: height), pending(wave-3: sea_level_m), waiver(elevation-convention: elevation_m), bare-ok(flag: ocean), bare-ok(index: biome), bare-ok(index: plate), bare-ok(ratio: unrest)
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
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let t_cell = terrain_index.nearest(terrain.geosphere(), latitude, longitude);
            let c_cell = climate_index.nearest(climate.geosphere(), latitude, longitude);
            elevation_m.push(terrain.elevation_at(t_cell));
            ocean.push(terrain.is_ocean(t_cell));
            let b = *biomes.get(c_cell);
            let index = catalog
                .iter()
                .position(|entry| *entry == b)
                .expect("every biome is in the catalog") as u16;
            biome.push(index);
            plate.push(terrain.plate_of(t_cell));
            unrest.push(terrain.unrest_at(t_cell));
        }
    }
    debug_assert!(
        elevation_m
            .iter()
            .chain(unrest.iter())
            .all(|v| v.is_finite()),
        "scene layers must be finite; serde_json would emit null"
    );
    Ok(TilesScene {
        schema: TILES_SCHEMA.to_string(),
        seed: world.seed.0,
        width,
        height,
        sea_level_m: terrain.sea_level(),
        elevation_m,
        ocean,
        biome,
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        plate,
        unrest,
        features: features_of(world),
    })
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
}
