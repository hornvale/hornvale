# The Scene Protocol (Cartographic Pole) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `scene/tiles/v1` — the deterministic, semantic-only tile-lattice JSON scene — via a new `windows/scene` crate and `hornvale scene tiles`, with a committed drift-checked example scene, after promoting the duplicated nearest-cell index into the kernel.

**Architecture:** Task 1 deduplicates: the private `LatBandIndex` in terrain and climate render modules becomes the kernel's public `NearestCellIndex` (moved, not rewritten). Task 2 builds `hornvale-scene`: `tiles_scene(world, width)` samples the public provider surface at pixel centers into a columnar `TilesScene` (serde-serialized compact JSON — field order fixes key order, ryu fixes float text, so bytes are deterministic). Tasks 3–5: CLI + gallery artifact + CI, the schema reference page, and the campaign documents.

**Tech Stack:** Rust edition 2024; serde + serde_json only. New crate `windows/scene` (the workspace `members = ["…", "windows/*", …]` glob includes it automatically).

**Spec:** `docs/superpowers/specs/2026-07-09-scene-protocol-design.md` (governs; cite it on judgment calls).

## Global Constraints

- No new dependencies (decision 0004); no wall-clock; no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only); `total_cmp` for float sorts.
- Layering (decision 0002): domains depend only on the kernel; windows may depend on kernel + domains + windows; the scene window builds worlds only through `hornvale-worldgen`.
- **Save-format contract:** `scene/tiles/v1` bytes are pinned by a golden fixture. Same seed + same width → byte-identical JSON. Additive changes stay v1; changed meaning/order/type of an existing field mints v2 alongside. JSON key order = `TilesScene` field declaration order = spec §2 order — do not reorder fields.
- Semantic-only: no colors, glyphs, projection hints, or producer identity in the schema.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line doc comment.
- Full gate before every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings` (run `cargo fmt` first). Never `--no-verify`.
- Workspace root: `/Users/nathan/Projects/hornvale/hornvale` (or the worktree); run all commands from the root.

---

### Task 1: Kernel `NearestCellIndex` (dedup)

**Files:**
- Modify: `kernel/src/geosphere.rs` (add the index + tests), `kernel/src/lib.rs` (re-export)
- Modify: `domains/terrain/src/render.rs` (delete `LatBandIndex`, use the kernel type)
- Modify: `domains/climate/src/render.rs` (delete `LatBandIndex` + its private `dot`, use the kernel type)

**Interfaces:**
- Consumes: existing `Geosphere` (`cells()`, `coord()`, `position()`).
- Produces: `hornvale_kernel::NearestCellIndex` with `new(geo: &Geosphere) -> NearestCellIndex` and `nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId` (degrees). Task 2 consumes it.

- [ ] **Step 1: Add the kernel type with its test (red)**

In `kernel/src/geosphere.rs`, append (adapting the module's existing doc style):

```rust
/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

/// Dot product of two unit vectors.
fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Latitude-band index for pixel→cell lookups: cells bucketed into 30
/// bands of 6° (built in ascending cell order, so lookups are
/// deterministic). Searching the query's band ± 1 always contains the
/// nearest cell center at level ≥ 4 (which lies within ~2.5°).
pub struct NearestCellIndex {
    /// One bucket of cells per latitude band, north to south.
    bands: Vec<Vec<CellId>>,
}

impl NearestCellIndex {
    /// Bucket every cell of `geo` by latitude.
    pub fn new(geo: &Geosphere) -> NearestCellIndex {
        let mut bands = vec![Vec::new(); BAND_COUNT];
        for cell in geo.cells() {
            let latitude = geo.coord(cell).latitude;
            let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
            bands[band].push(cell);
        }
        NearestCellIndex { bands }
    }

    /// The cell nearest a coordinate (degrees), by maximum dot product.
    /// Inverts the coord convention: latitude = asin(z), longitude =
    /// atan2(y, x).
    pub fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot3(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
}
```

This is the terrain module's implementation moved verbatim (modulo `dot3` living locally); do not "improve" it — its behavior is pinned by every committed raster artifact.

In the geosphere test module, add the relocated equivalence test (this is `band_index_agrees_with_brute_force_nearest` from `domains/terrain/src/render.rs`, rewritten against the kernel type):

```rust
    #[test]
    fn nearest_cell_index_agrees_with_brute_force() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (89.0, 10.0), (-89.0, -170.0), (45.5, 179.5)] {
            let banded = index.nearest(&geo, latitude, longitude);
            let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
            let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let mut best = CellId(0);
            let mut best_dot = f64::NEG_INFINITY;
            for cell in geo.cells() {
                let d = super::dot3(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
            assert_eq!(banded, best, "at ({latitude}, {longitude})");
        }
    }
```

In `kernel/src/lib.rs`, extend the geosphere re-export line:

```rust
pub use geosphere::{CellId, CellMap, GeoCoord, Geosphere, NearestCellIndex};
```

Run: `cargo test -p hornvale-kernel nearest` — Expected: PASS (the kernel side is self-contained; red/green here is the equivalence test itself, which fails if the move mangled anything).

- [ ] **Step 2: Switch terrain and climate to the kernel type**

`domains/terrain/src/render.rs`: delete the `LatBandIndex` struct + impl and the `BAND_COUNT`/`BAND_DEGREES` consts and the `band_index_agrees_with_brute_force_nearest` test (now in the kernel); replace both `LatBandIndex::new(...)` call sites with `NearestCellIndex::new(...)`; import via the existing `use hornvale_kernel::{...}` list (add `NearestCellIndex`). If `use crate::plates::dot;` becomes unused, remove it; update the module doc's index description to say the index now lives in the kernel.

`domains/climate/src/render.rs`: same — delete its `LatBandIndex`, its private `fn dot`, and its `BAND_COUNT`/`BAND_DEGREES`; switch call sites; extend its `hornvale_kernel` import.

Run: `cargo test -p hornvale-terrain && cargo test -p hornvale-climate`
Expected: PASS with zero test-expectation changes — the dedup must not move a byte.

- [ ] **Step 3: Prove the committed rasters are unmoved**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-scene-sky.json
cargo run -q -p hornvale -- map --world /tmp/hv-scene-sky.json --out /tmp/dedup-elev.png > /dev/null
cargo run -q -p hornvale -- biome-map --world /tmp/hv-scene-sky.json --out /tmp/dedup-biome.png > /dev/null
cmp /tmp/dedup-elev.png book/src/gallery/elevation-seed-42.png && cmp /tmp/dedup-biome.png book/src/gallery/biome-seed-42.png && echo DEDUP-CLEAN
```

Expected: `DEDUP-CLEAN`. Anything else means the move changed behavior — stop and diff.

- [ ] **Step 4: Full gate, then commit**

```bash
git add kernel/src/geosphere.rs kernel/src/lib.rs domains/terrain/src/render.rs domains/climate/src/render.rs
git commit -m "refactor(kernel): promote the nearest-cell index out of two render modules

LatBandIndex moved verbatim to kernel::geosphere::NearestCellIndex;
terrain and climate render on it; committed rasters proven byte-unmoved."
```

---

### Task 2: The `windows/scene` crate — `scene/tiles/v1`

**Files:**
- Create: `windows/scene/Cargo.toml`, `windows/scene/src/lib.rs`, `windows/scene/tests/golden.rs`, `windows/scene/tests/fixtures/tiles-seed-1-w16.json`
- Modify: `domains/climate/src/biome.rs` (add `Biome::catalog()`)

**Interfaces:**
- Consumes: `hornvale_kernel::NearestCellIndex` (Task 1); `world_builder::{terrain_of, climate_of}`; `GeneratedTerrain::{geosphere, elevation_at, is_ocean, plate_of, unrest_at, sea_level}`; `GeneratedClimate::{geosphere, biome_map}`; `hornvale_terrain::places`; `hornvale_settlement::{village_info, LATITUDE, LONGITUDE}`; `Biome::{catalog, name}`.
- Produces: crate `hornvale-scene` with `tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError>`, `scene_json(scene: &TilesScene) -> String`, `pub struct TilesScene`, `pub struct Feature`, `pub enum SceneError`. Task 3 consumes all of it.

- [ ] **Step 1: `Biome::catalog()` with its test (red → green)**

In `domains/climate/src/biome.rs`, inside `impl Biome`:

```rust
    /// Every biome, in declaration order — the stable legend order for
    /// `scene/tiles` (scene-protocol spec §2). Appending a variant appends
    /// a legend entry; never reorder.
    pub fn catalog() -> &'static [Biome] {
        const CATALOG: [Biome; 22] = [
            Biome::Ice,
            Biome::Tundra,
            Biome::Taiga,
            Biome::TemperateGrassland,
            Biome::Shrubland,
            Biome::TemperateForest,
            Biome::TemperateRainforest,
            Biome::Desert,
            Biome::Savanna,
            Biome::TropicalSeasonalForest,
            Biome::TropicalRainforest,
            Biome::Alpine,
            Biome::SeaIce,
            Biome::CoralReef,
            Biome::KelpForest,
            Biome::HydrothermalVent,
            Biome::HadalTrench,
            Biome::Upwelling,
            Biome::Epipelagic,
            Biome::Mesopelagic,
            Biome::Bathypelagic,
            Biome::Abyssal,
        ];
        &CATALOG
    }
```

(The order above is the enum's declaration order — verify against the `enum Biome` block while editing; if the enum ever disagrees, the enum wins and this list is wrong.) Test, in the file's test module:

```rust
    #[test]
    fn catalog_is_complete_and_distinct() {
        let catalog = Biome::catalog();
        assert_eq!(catalog.len(), 22);
        let names: std::collections::BTreeSet<&str> =
            catalog.iter().map(|b| b.name()).collect();
        assert_eq!(names.len(), catalog.len(), "duplicate biome in catalog");
    }
```

Run red first (`cargo test -p hornvale-climate catalog` fails: no `catalog`), implement, run green.

- [ ] **Step 2: Create the crate**

`windows/scene/Cargo.toml`:

```toml
[package]
name = "hornvale-scene"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale scene window: deterministic semantic-only scene JSON over the query surface."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-worldgen = { path = "../worldgen" }
hornvale-terrain = { path = "../../domains/terrain" }
hornvale-climate = { path = "../../domains/climate" }
hornvale-settlement = { path = "../../domains/settlement" }
serde = { workspace = true }
serde_json = { workspace = true }
```

`windows/scene/src/lib.rs`:

```rust
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
            SceneError::WidthOdd(w) => write!(f, "--width {w} is odd; height is width/2, so width must be even"),
            SceneError::WidthOutOfRange(w) => write!(f, "--width {w} is outside {MIN_WIDTH}..={MAX_WIDTH}"),
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
    if width % 2 != 0 {
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
    if let Some(village) = flagship {
        if let Some((latitude, longitude)) = place_latlon(world, village.id) {
            features.push(Feature {
                name: village.name,
                kind: "flagship".to_string(),
                latitude,
                longitude,
            });
        }
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
```

(Adjust the two `world_builder` call paths if the crate re-exports differ — `hornvale_worldgen::terrain_of` vs a `world_builder` module alias; use whatever `cli/src/main.rs` uses, which is the canonical caller. Likewise confirm `PlaceInfo.id`/`VillageInfo.id` field access matches `cli/src/main.rs`'s `place_latlon`, which this function mirrors.)

- [ ] **Step 3: In-crate tests (red as written, green when the code lands)**

In `windows/scene/src/lib.rs`'s test module:

```rust
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
        assert!(matches!(tiles_scene(&w, 15), Err(SceneError::WidthOutOfRange(15))));
        assert!(matches!(tiles_scene(&w, 8), Err(SceneError::WidthOutOfRange(8))));
        assert!(matches!(tiles_scene(&w, 2048), Err(SceneError::WidthOutOfRange(2048))));
        assert!(matches!(tiles_scene(&w, 17), Err(SceneError::WidthOdd(17))));
        assert!(tiles_scene(&w, 18).is_ok());
    }

    #[test]
    fn flagship_appears_exactly_once() {
        let scene = tiles_scene(&world(), 32).unwrap();
        let flagships = scene.features.iter().filter(|f| f.kind == "flagship").count();
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
```

Note the check-order consequence encoded in `width_violations_are_loud`: range is validated before oddness, so 15 (< 16 and odd) reports `WidthOutOfRange`, while 17 (in range, odd) reports `WidthOdd`. Adapt the `world()` helper to the real `build_world` signature (see `cli/src/repl.rs` tests).

Run: `cargo test -p hornvale-scene` — Expected: PASS.

- [ ] **Step 4: The golden fixture (the v1 byte pin)**

Create `windows/scene/tests/golden.rs`:

```rust
//! The scene/tiles/v1 byte pin: this fixture changing is the epoch
//! decision point (scene-protocol spec §2). Regenerate deliberately, never
//! casually: `cargo test -p hornvale-scene --test golden -- --ignored`
//! rewrites it, then review the diff as a contract change.

use hornvale_scene::{scene_json, tiles_scene};

// Integration tests can't see #[cfg(test)] helpers, and the public API
// takes no test scaffolding — this 10-line duplicate of the lib tests'
// builder is the cheaper price.
fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(1),
        &Default::default(),
        hornvale_worldgen::SkyChoice::Constant,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 1 builds")
}

fn seed_1_json() -> String {
    scene_json(&tiles_scene(&world(), 16).unwrap())
}

#[test]
fn v1_bytes_are_pinned() {
    assert_eq!(seed_1_json(), include_str!("fixtures/tiles-seed-1-w16.json"));
}

#[test]
#[ignore = "regenerates the golden fixture; run deliberately"]
fn regenerate_golden() {
    std::fs::write("tests/fixtures/tiles-seed-1-w16.json", seed_1_json()).unwrap();
}
```

(`golden.rs` needs `hornvale-kernel` and `hornvale-worldgen` reachable from tests — they're already regular dependencies, so no `[dev-dependencies]` change.) Create the fixture by running the ignored test once:

```bash
mkdir -p windows/scene/tests/fixtures
cargo test -p hornvale-scene --test golden regenerate_golden -- --ignored
cargo test -p hornvale-scene --test golden
python3 -c "import json; d=json.load(open('windows/scene/tests/fixtures/tiles-seed-1-w16.json')); print(d['schema'], d['width'], d['height'], len(d['elevation_m']), len(d['biome_legend']))"
```

Expected: golden test PASS; sanity line `scene/tiles/v1 16 8 128 22`. Eyeball the fixture once (it's small): keys in spec §2 order, floats plain numbers, features plausible.

- [ ] **Step 5: Full gate, then commit**

```bash
git add windows/scene domains/climate/src/biome.rs
git commit -m "feat(scene): the scene window — scene/tiles/v1 (RENDER-1)

Columnar tile lattice (elevation/ocean/biome/plate/unrest + point
features) over the query surface; compact deterministic JSON; golden
fixture pins the v1 bytes; Biome::catalog() fixes legend order."
```

---

### Task 3: CLI `scene tiles`, gallery artifact, CI

**Files:**
- Modify: `cli/Cargo.toml` (add `hornvale-scene`), `cli/src/main.rs` (dispatch + usage + command + tests), `.github/workflows/ci.yml`
- Create (generated): `book/src/gallery/scene-tiles-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_scene::{tiles_scene, scene_json, MIN_WIDTH, MAX_WIDTH}` (Task 2).
- Produces: `hornvale scene tiles [--world <PATH>] [--width <N>]`.

- [ ] **Step 1: CLI tests (red)**

In `cli/src/main.rs`'s test module, following the existing command-test idioms:

```rust
    #[test]
    fn scene_with_no_subcommand_is_an_error() {
        let err = cmd_scene(&args(&["scene"])).unwrap_err();
        assert!(err.contains("tiles"), "should name the known kinds: {err}");
    }

    #[test]
    fn scene_unknown_kind_is_an_error() {
        let err = cmd_scene(&args(&["scene", "dioramas"])).unwrap_err();
        assert!(err.contains("tiles"));
    }

    #[test]
    fn usage_mentions_scene() {
        assert!(usage().contains("scene tiles"));
    }
```

(Adapt the `args` helper name to what the existing `lab` tests use.) Run: `cargo test -p hornvale scene` — Expected: FAIL, `cmd_scene` not found.

- [ ] **Step 2: Implement the command**

`cli/Cargo.toml` `[dependencies]`: add `hornvale-scene = { path = "../windows/scene" }`.

Dispatch arm after `star-chart`'s: `Some("scene") => cmd_scene(&args),`. Usage line after star-chart's:

```text
  hornvale scene tiles [--world <PATH>] [--width <N>] emit scene/tiles/v1 JSON to stdout
```

The command (mirroring `cmd_lab`'s subcommand shape and `cmd_map`'s world loading):

```rust
/// Emit a scene description as JSON on stdout: `scene tiles` renders the
/// cartographic tile lattice (scene/tiles/v1). Deterministic; CI
/// drift-checks the committed example scene.
fn cmd_scene(args: &[String]) -> Result<(), String> {
    match args.get(1).map(String::as_str) {
        Some("tiles") => {
            let world = load_world(args)?;
            let width = match flag_value(args, "--width") {
                Some(raw) => raw
                    .parse::<u32>()
                    .map_err(|e| format!("--width must be a u32: {e}"))?,
                None => 256,
            };
            let scene =
                hornvale_scene::tiles_scene(&world, width).map_err(|e| e.to_string())?;
            println!("{}", hornvale_scene::scene_json(&scene));
            Ok(())
        }
        Some(other) => Err(format!("unknown scene kind '{other}'; known kinds: tiles")),
        None => Err("scene needs a kind; known kinds: tiles".to_string()),
    }
}
```

(Check how `load_world`/`flag_value` treat the subcommand token — if `flag_value` scans all args this works as-is; mirror `cmd_lab`'s argument slicing if it slices instead.)

Run: `cargo test -p hornvale` — Expected: PASS.

- [ ] **Step 3: Generate the gallery artifact, wire CI**

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-scene-sky.json
cargo run -q -p hornvale -- scene tiles --world /tmp/hv-scene-sky.json > book/src/gallery/scene-tiles-seed-42.json
cargo run -q -p hornvale -- scene tiles --world /tmp/hv-scene-sky.json | cmp - book/src/gallery/scene-tiles-seed-42.json && echo SCENE-DETERMINISTIC
python3 -c "import json; d=json.load(open('book/src/gallery/scene-tiles-seed-42.json')); print(d['schema'], d['width'], len(d['elevation_m']), len(d['features']), [f['kind'] for f in d['features']].count('flagship'))"
```

Expected: `SCENE-DETERMINISTIC`; sanity line `scene/tiles/v1 256 32768 <N> 1`.

`.github/workflows/ci.yml`, after the `star-chart` line:

```yaml
          cargo run -p hornvale -- scene tiles --world /tmp/hv-ci-sky.json > book/src/gallery/scene-tiles-seed-42.json
```

- [ ] **Step 4: Full gate, then commit**

```bash
git add cli/Cargo.toml cli/src/main.rs .github/workflows/ci.yml book/src/gallery/scene-tiles-seed-42.json
git commit -m "feat(cli): hornvale scene tiles — the seam emits its first scene

scene/tiles/v1 on stdout; committed example scene drift-checked in CI,
laying the in-book viewer's test data (decision 0022's seam, concrete)."
```

---

### Task 4: The schema reference page

**Files:**
- Create: `book/src/reference/scene-tiles-v1.md`
- Modify: `book/src/SUMMARY.md` (reference section)

**Interfaces:**
- Consumes: the shipped schema (Tasks 2–3).
- Produces: the contract's public face.

- [ ] **Step 1: Write the page**

Hand-written (not generated), at the book's altitude — technical, self-contained, no process vocabulary or registry IDs. Read `book/src/reference/concept-registry.md`'s framing prose (or the nearest hand-written reference chapter) for tone. Required content, in order:

1. What a scene is (one paragraph): the simulation emits deterministic, semantic data; clients render pixels. A scene says what an observer can see, never how to draw it.
2. The `scene/tiles/v1` document: a field-by-field table (name, type, meaning) matching the shipped schema exactly — `schema`, `seed`, `width`, `height`, `sea_level_m`, the five layers, `biome_legend`, `features` (with the two `kind` values and the flagship-appears-once rule).
3. The grid convention (row-major, top row first, lat 90→−90, lon −180→180, pixel centers; height = width/2).
4. Stability rules: legend order is stable (append-only); additive fields stay v1; changed meaning mints `scene/tiles/v2` alongside; the committed example (`gallery/scene-tiles-seed-42.json`) is regenerated by CI and byte-checked.
5. How to get one: the `hornvale scene tiles` invocation and the width bounds (16..=1024, even).

Include one short inline JSON excerpt (the top-level keys with elided arrays), not the whole document.

`book/src/SUMMARY.md`: add `- [Scene Schema: tiles v1](./reference/scene-tiles-v1.md)` alongside the existing reference entries (match their indentation and ordering style).

- [ ] **Step 2: Build and verify**

```bash
mdbook build book
cargo test -p hornvale --test docs_consistency
```

Expected: clean build; docs test PASS (it checks SUMMARY/book link resolution rules that apply).

- [ ] **Step 3: Commit**

```bash
git add book/src/reference/scene-tiles-v1.md book/src/SUMMARY.md
git commit -m "docs(book): scene/tiles/v1 schema reference — the contract's public face"
```

---

### Task 5: Chronicle 20, retrospective, registry flip, final sweep

**Files:**
- Create: `book/src/chronicle/20-the-scene-window.md`, `docs/retrospectives/campaign-20.md`
- Modify: `book/src/SUMMARY.md` (chronicle line), `docs/vision/idea-registry.md` (RENDER-1 flip)

**Interfaces:**
- Consumes: everything merged in Tasks 1–4.
- Produces: the campaign's Definition-of-Done documents (decisions 0013, 0020).

- [ ] **Step 1: Chronicle entry**

Confirm `19-the-star-chart.md` is the highest-numbered chronicle; write `book/src/chronicle/20-the-scene-window.md` in the tone of 18/19, targeting their length (~150–170 lines), covering diegetically (no registry IDs, no "Task N"): the seam idea (the simulation emits deterministic data; clients render pixels — cite decision-log posture without process vocabulary, as prior chronicles handle decisions), the tile lattice and its five layers, why columnar-with-legend, the byte-pinned contract and what "additive vs. new version" means, the nearest-cell index finding its one home in the kernel, and what the committed scene sets up (an interactive page in this very book, one day). Include the short top-level JSON excerpt (keys only). Add the SUMMARY chronicle line after the `19-` entry.

- [ ] **Step 2: Registry flip**

`docs/vision/idea-registry.md`: RENDER-1 row — status `spec'd` → `shipped`, Where → `[scene-protocol spec](../superpowers/specs/2026-07-09-scene-protocol-design.md)`. Leave RENDER-2..4 untouched.

Run: `cargo test -p hornvale --test docs_consistency` — Expected: PASS.

- [ ] **Step 3: Retrospective**

`docs/retrospectives/campaign-20.md`, one page, process-not-product, in `campaign-19.md`'s section shape. Seed material: whatever actually happened during Tasks 1–4 (consult the reviews), plus: the golden-fixture-as-epoch-tripwire pattern is new — say whether it earned its place.

- [ ] **Step 4: Full CI artifact list locally, gate, commit**

Run every command in ci.yml's "Artifacts are current" step, then:

```bash
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
mdbook build book
```

Expected: no drift; clean build. Then:

```bash
git add book/src/chronicle/20-the-scene-window.md book/src/SUMMARY.md docs/vision/idea-registry.md docs/retrospectives/campaign-20.md
git commit -m "docs(book): chronicle 20 — the scene window; RENDER-1 shipped; retro"
```

---

## Definition of Done

- [ ] `NearestCellIndex` public in the kernel; both domain copies deleted; committed rasters proven byte-unmoved (`DEDUP-CLEAN`).
- [ ] `hornvale-scene` ships `tiles_scene`/`scene_json`/`TilesScene`/`Feature`/`SceneError`; `Biome::catalog()` pins legend order; golden fixture pins the v1 bytes.
- [ ] `hornvale scene tiles` in usage + dispatch; loud unknown-kind and width errors; `book/src/gallery/scene-tiles-seed-42.json` committed and in CI's regen list.
- [ ] Schema reference page in the book; SUMMARY updated (reference + chronicle).
- [ ] Chronicle 20, retrospective, RENDER-1 → `shipped`; docs_consistency green.
- [ ] Full gate green on every commit; final local CI artifact run shows zero drift.
