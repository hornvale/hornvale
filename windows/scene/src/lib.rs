//! The scene window: deterministic, semantic-only scene descriptions over
//! the query surface (rendering-strategy Ring 2; decision 0022). A scene
//! says *what an observer can see* — raw quantities, catalog names, point
//! features — never how to draw it. Schemas are save-format-class
//! contracts: additive changes stay in-version, changed meaning mints a
//! new version alongside. This crate holds the cartographic pole:
//! `scene/tiles/v1`, the equirectangular tile lattice.

#![warn(missing_docs)]

use hornvale_kernel::{NearestCellIndex, World};
use serde::Serialize;

/// The schema identifier this crate emits.
pub const TILES_SCHEMA: &str = "scene/tiles/v1";
/// Smallest legal lattice width.
pub const MIN_WIDTH: u32 = 16;
/// Largest legal lattice width.
pub const MAX_WIDTH: u32 = 1024;

/// Scene construction failed; the reason, loudly (the GenesisError manner).
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
#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Feature {
    /// The feature's canonical name.
    pub name: String,
    /// What kind of point this is: `"settlement"` or `"flagship"`.
    pub kind: String,
    /// Degrees north.
    pub latitude: f64,
    /// Degrees east.
    pub longitude: f64,
}

/// One `scene/tiles/v1` document (scene-protocol spec §2). Field order is
/// the JSON key order and is contract — never reorder. Layers are
/// row-major, top row first: latitude 90→−90 down, longitude −180→180
/// across, pixel centers.
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
    pub sea_level_m: f64,
    /// Elevation in meters per tile.
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
    pub unrest: Vec<f64>,
    /// Named points: settlements, the flagship last.
    pub features: Vec<Feature>,
}

/// Build the `scene/tiles/v1` scene for `world` at `width` tiles across
/// (height is `width / 2`). Deterministic: same world + same width →
/// the same scene, byte-for-byte once serialized.
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
pub fn scene_json(scene: &TilesScene) -> String {
    serde_json::to_string(scene).expect("a TilesScene always serializes")
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
}
