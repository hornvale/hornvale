# The Region Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `scene/tiles-region/v1` — a producer query returning the same per-tile layers for one cube-sphere quadtree tile's footprint at higher on-tile sample density, as a versioned cross-repo contract, with a `world-wasm-v3` export and producer-sourced contract tests, plus a minimal orrery consumer proving the data path.

**Architecture:** A new module `windows/scene/src/region.rs` holds the normative cube-sphere projection, a deterministic barycentric interpolation helper over geosphere cell triangles, the `RegionScene` document, and its builders. Continuous layers (elevation, unrest, temperature mean/swing, moisture) are barycentrically interpolated between the ~110 km cell samples for a smooth surface; discrete layers (ocean, biome, plate) stay nearest-cell. Interpolation is producer-internal and golden-pinned; the client consumes layer arrays positionally and reconstructs only the seasonal temperature evaluator, which commutes with interpolation because the seasonal period is global.

**Tech Stack:** Rust 2024 (kernel/domains/windows/cli), `serde`/`serde_json` only, `wasm32-unknown-unknown` (raw `extern "C"`, no wasm-bindgen); orrery is TypeScript/three.js/vitest/Playwright.

## Global Constraints

- **Determinism is constitutional.** Same seed + same address → byte-identical document. No `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only). Float sorting uses `total_cmp`. No wall-clock time.
- **Quantize at the emit boundary only** (8 significant digits, `hornvale_kernel::quantize`, decision 0033). Interpolation runs at full precision; quantization is applied by the serde field attributes, never in the compute path.
- **Cross-platform byte-identity**: transcendentals route through libm (decision 0041). The interpolation helper in this plan uses **only dot products and divisions — no transcendentals** — so it is trivially cross-platform.
- **Dependencies frozen**: `serde` + `serde_json` only, workspace-wide. No new crates.
- **Every crate `#![warn(missing_docs)]`**; every `pub` item, field, variant gets a one-line doc. Every primitive at a `pub` boundary carries a `type-audit:` verdict tag.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate` (fmt + clippy + nextest + doctests); scope intermediate runs with `cargo test -p <crate>`.
- **Layering** (enforced by `cli/tests/architecture.rs`): `kernel → domains/* → windows/* → cli`. `windows/scene` may depend on `hornvale-worldgen` and domain crates; it already does.
- **Additive-only**: new schema `scene/tiles-region/v1` alongside `scene/tiles/v1`; **no v1 change, no epoch, no census regen, no new streams/concepts.**
- **Address bounds** (fixed contract): `face ∈ 0..=5`, `level ∈ 0..=24`, `ix,iy ∈ 0..2^level`, `samples = N ∈ 1..=256` (node grid `(N+1)²`, row-major `i = row·(N+1) + col`).

---

### Task 1: The cube-sphere projection and node enumeration

**Files:**
- Create: `windows/scene/src/region.rs`
- Modify: `windows/scene/src/lib.rs` (add `mod region; pub use region::*;`; extend `SceneError`)

**Interfaces:**
- Produces: `pub struct RegionAddr { pub face: u32, pub level: u32, pub ix: u32, pub iy: u32, pub samples: u32 }`; `RegionAddr::validate(&self) -> Result<(), SceneError>`; `RegionAddr::node_units(&self) -> Vec<[f64; 3]>` (the `(N+1)²` node unit vectors, row-major); new `SceneError` variants `RegionFaceOutOfRange(u32)`, `RegionLevelOutOfRange(u32)`, `RegionTileOutOfRange { ix: u32, iy: u32, level: u32 }`, `RegionSamplesOutOfRange(u32)`.

- [ ] **Step 1: Extend `SceneError` in `windows/scene/src/lib.rs`**

Add these variants to the `enum SceneError` (after `Build(String)`), and update the `type-audit` tag on the enum and the `Display` impl:

```rust
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
```

In `impl Display for SceneError`, add match arms:

```rust
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
```

Update the enum's `type-audit` doc tag to append: `bare-ok(diagnostic-value: RegionFaceOutOfRange.0), bare-ok(diagnostic-value: RegionLevelOutOfRange.0), bare-ok(diagnostic-value: RegionTileOutOfRange fields), bare-ok(diagnostic-value: RegionSamplesOutOfRange.0)`.

- [ ] **Step 2: Create `windows/scene/src/region.rs` with the projection and bounds**

```rust
//! `scene/tiles-region/v1`: one cube-sphere quadtree tile's footprint, sampled
//! at higher on-tile density than the global lattice. Continuous layers are
//! barycentrically interpolated between geosphere cells (a smooth surface, not
//! new physics — the ~110 km cell spacing is the resolution floor); discrete
//! layers stay nearest-cell. The projection here is the normative one, shared
//! byte-for-byte with the orrery's `cubeSphere.ts` and the reference page.

use crate::SceneError;
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex};

/// Deepest addressable quadtree level (the client clamps its own to ~18; this
/// bound is a generous superset). Beyond the cell floor a tile is honest but
/// carries no detail the coarser tile lacked.
/// type-audit: bare-ok(count)
pub const MAX_REGION_LEVEL: u32 = 24;
/// Largest legal `samples` (quads per edge); the node grid is `(N+1)²`.
/// type-audit: bare-ok(count)
pub const MAX_REGION_SAMPLES: u32 = 256;

/// The six cube-face bases `(n, u, v)`: a face point is `normalize(n + a·u + b·v)`
/// for `(a, b) ∈ [-1, 1]²`. Identical to the orrery's `FACES`.
const FACES: [[[f64; 3]; 3]; 6] = [
    [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
    [[-1.0, 0.0, 0.0], [0.0, -1.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 1.0, 0.0], [-1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, -1.0, 0.0], [1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 0.0, 1.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
    [[0.0, 0.0, -1.0], [-1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
];

/// A face parameter: `-1 + 2·(index + offset)/2^level`, a dyadic rational in
/// [-1, 1]. `offset ∈ [0, 1]` walks a tile's edge.
fn param(index: u32, offset: f64, level: u32) -> f64 {
    -1.0 + 2.0 * (f64::from(index) + offset) / (1u64 << level) as f64
}

/// The unit vector for face parameters `(a, b)`: `normalize(n + a·u + b·v)`.
fn face_unit(face: usize, a: f64, b: f64) -> [f64; 3] {
    let [n, u, v] = FACES[face];
    let x = n[0] + a * u[0] + b * v[0];
    let y = n[1] + a * u[1] + b * v[1];
    let z = n[2] + a * u[2] + b * v[2];
    let len = (x * x + y * y + z * z).sqrt();
    [x / len, y / len, z / len]
}

/// One regional tile address (scene-protocol: The Region §3.1).
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegionAddr {
    /// One of the six cube faces (0..=5).
    pub face: u32,
    /// Quadtree depth; the face is `2^level × 2^level` tiles.
    pub level: u32,
    /// Tile column on the face (0..2^level).
    pub ix: u32,
    /// Tile row on the face (0..2^level).
    pub iy: u32,
    /// Quads per tile edge N; the node grid is `(N+1)²`.
    pub samples: u32,
}

impl RegionAddr {
    /// Validate the address against the fixed contract bounds, loudly.
    pub fn validate(&self) -> Result<(), SceneError> {
        if self.face > 5 {
            return Err(SceneError::RegionFaceOutOfRange(self.face));
        }
        if self.level > MAX_REGION_LEVEL {
            return Err(SceneError::RegionLevelOutOfRange(self.level));
        }
        let bound = 1u64 << self.level;
        if u64::from(self.ix) >= bound || u64::from(self.iy) >= bound {
            return Err(SceneError::RegionTileOutOfRange {
                ix: self.ix,
                iy: self.iy,
                level: self.level,
            });
        }
        if self.samples == 0 || self.samples > MAX_REGION_SAMPLES {
            return Err(SceneError::RegionSamplesOutOfRange(self.samples));
        }
        Ok(())
    }

    /// The `(N+1)²` node unit vectors, row-major (`i = row·(N+1) + col`, row is
    /// the `iy`/`b` direction). Assumes a validated address.
    pub fn node_units(&self) -> Vec<[f64; 3]> {
        let n = self.samples;
        let face = self.face as usize;
        let mut units = Vec::with_capacity(((n + 1) * (n + 1)) as usize);
        for row in 0..=n {
            let b = param(self.iy, f64::from(row) / f64::from(n), self.level);
            for col in 0..=n {
                let a = param(self.ix, f64::from(col) / f64::from(n), self.level);
                units.push(face_unit(face, a, b));
            }
        }
        units
    }
}
```

- [ ] **Step 3: Wire the module and write failing tests**

In `windows/scene/src/lib.rs`, after the `use` block add `mod region;` and `pub use region::*;`.

Append to the `#[cfg(test)] mod tests` in `region.rs` (create the test module):

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn addr(face: u32, level: u32, ix: u32, iy: u32, samples: u32) -> RegionAddr {
        RegionAddr { face, level, ix, iy, samples }
    }

    #[test]
    fn node_grid_is_sized_and_deterministic() {
        let a = addr(0, 3, 4, 4, 16);
        a.validate().unwrap();
        let u1 = a.node_units();
        let u2 = a.node_units();
        assert_eq!(u1.len(), 17 * 17);
        assert_eq!(u1, u2);
        assert!(u1.iter().all(|p| (p[0] * p[0] + p[1] * p[1] + p[2] * p[2] - 1.0).abs() < 1e-12));
    }

    #[test]
    fn level_zero_center_node_points_along_face_normal() {
        // level 0, samples 2 → the center node (row=1,col=1) is (a,b)=(0,0) → the face normal.
        let a = addr(0, 0, 0, 0, 2);
        let units = a.node_units();
        let center = units[1 * 3 + 1];
        assert!((center[0] - 1.0).abs() < 1e-12, "face 0 normal is +x: {center:?}");
    }

    #[test]
    fn validation_is_loud() {
        assert!(matches!(addr(6, 0, 0, 0, 1).validate(), Err(SceneError::RegionFaceOutOfRange(6))));
        assert!(matches!(addr(0, 25, 0, 0, 1).validate(), Err(SceneError::RegionLevelOutOfRange(25))));
        assert!(matches!(addr(0, 2, 4, 0, 1).validate(), Err(SceneError::RegionTileOutOfRange { .. })));
        assert!(matches!(addr(0, 0, 0, 0, 0).validate(), Err(SceneError::RegionSamplesOutOfRange(0))));
        assert!(matches!(addr(0, 0, 0, 0, 300).validate(), Err(SceneError::RegionSamplesOutOfRange(300))));
        assert!(addr(0, 0, 0, 0, 1).validate().is_ok());
    }
}
```

- [ ] **Step 4: Run tests — expect FAIL then PASS**

Run: `cargo test -p hornvale-scene region::`
Expected: compiles, all four tests pass. (If it fails to compile, fix signatures; if a test fails, fix the code — the test encodes the contract.)

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/scene/src/region.rs windows/scene/src/lib.rs
git commit -m "feat(scene): region — cube-sphere projection, address, node enumeration"
```

---

### Task 2: Deterministic barycentric interpolation over cell triangles

**Files:**
- Modify: `windows/scene/src/region.rs`

**Interfaces:**
- Produces: `fn triangle_weights(geo: &Geosphere, index: &NearestCellIndex, s: [f64; 3]) -> [(CellId, f64); 3]` (weights in [0,1], sum 1, exact `(cell,1.0)` on the two zero-weight slots at a cell center); `fn interp(geo, index, s, value: impl Fn(CellId) -> f64) -> f64`.

- [ ] **Step 1: Write the failing tests**

Append to `region.rs`'s test module:

```rust
    use hornvale_kernel::Geosphere;

    #[test]
    fn weights_are_exact_at_a_cell_center() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        for cell in geo.cells().take(50) {
            let s = geo.position(cell);
            let w = triangle_weights(&geo, &index, s);
            // The cell carries essentially all the weight; the sum is 1.
            let total: f64 = w.iter().map(|(_, x)| x).sum();
            assert!((total - 1.0).abs() < 1e-9, "weights sum to 1: {total}");
            let on_cell: f64 = w.iter().filter(|(c, _)| *c == cell).map(|(_, x)| x).sum();
            assert!(on_cell > 1.0 - 1e-9, "cell {cell:?} weight {on_cell} not ~1");
        }
    }

    #[test]
    fn weights_are_a_partition_of_unity_off_center() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        // A point between two cell centers.
        let a = geo.position(CellId(20));
        let b = geo.position(geo.neighbors(CellId(20))[0]);
        let mid = {
            let m = [(a[0] + b[0]) / 2.0, (a[1] + b[1]) / 2.0, (a[2] + b[2]) / 2.0];
            let len = (m[0] * m[0] + m[1] * m[1] + m[2] * m[2]).sqrt();
            [m[0] / len, m[1] / len, m[2] / len]
        };
        let w = triangle_weights(&geo, &index, mid);
        let total: f64 = w.iter().map(|(_, x)| x).sum();
        assert!((total - 1.0).abs() < 1e-9);
        assert!(w.iter().all(|(_, x)| *x >= -1e-9 && *x <= 1.0 + 1e-9), "weights in [0,1]: {w:?}");
    }

    #[test]
    fn interp_at_a_cell_center_returns_that_cells_value() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        let value = |c: CellId| c.0 as f64; // an arbitrary per-cell scalar
        for cell in geo.cells().take(50) {
            let got = interp(&geo, &index, geo.position(cell), value);
            assert!((got - cell.0 as f64).abs() < 1e-6, "cell {cell:?}: {got}");
        }
    }
```

- [ ] **Step 2: Run to verify FAIL**

Run: `cargo test -p hornvale-scene region::tests::weights`
Expected: FAIL (`triangle_weights` not found).

- [ ] **Step 3: Implement the interpolation helper**

Append to `region.rs` (before the test module):

```rust
/// Planar barycentric of `p` in triangle `(a, b, c)` (Ericson, *Real-Time
/// Collision Detection*). Returns `(u, v, w)` with `u + v + w = 1`; `u` is the
/// weight of `a`. Exact `(1, 0, 0)` when `p == a`.
fn barycentric(p: [f64; 3], a: [f64; 3], b: [f64; 3], c: [f64; 3]) -> (f64, f64, f64) {
    let sub = |x: [f64; 3], y: [f64; 3]| [x[0] - y[0], x[1] - y[1], x[2] - y[2]];
    let dot = |x: [f64; 3], y: [f64; 3]| x[0] * y[0] + x[1] * y[1] + x[2] * y[2];
    let (v0, v1, v2) = (sub(b, a), sub(c, a), sub(p, a));
    let d00 = dot(v0, v0);
    let d01 = dot(v0, v1);
    let d11 = dot(v1, v1);
    let d20 = dot(v2, v0);
    let d21 = dot(v2, v1);
    let denom = d00 * d11 - d01 * d01;
    if denom == 0.0 {
        return (1.0, 0.0, 0.0); // degenerate triangle: fall back to `a`
    }
    let v = (d11 * d20 - d01 * d21) / denom;
    let w = (d00 * d21 - d01 * d20) / denom;
    (1.0 - v - w, v, w)
}

/// The three geosphere cells whose triangle contains `s`, with barycentric
/// weights in [0,1] summing to 1. The triangle is the nearest cell `c` plus a
/// pair of its neighbours that are themselves adjacent (a fan triangle around
/// `c`); the one containing `s` wins. Transcendental-free (dot products only),
/// so cross-platform byte-identical. Exact at a cell centre. Degrades to
/// nearest-cell `(c, 1.0)` if no fan triangle contains `s` (a measure-zero
/// safety net, not a normal path).
fn triangle_weights(
    geo: &Geosphere,
    index: &NearestCellIndex,
    s: [f64; 3],
) -> [(CellId, f64); 3] {
    let c = index.nearest_to_position(geo, s);
    let pc = geo.position(c);
    let neigh = geo.neighbors(c);
    let mut best: Option<([(CellId, f64); 3], f64)> = None;
    for (i, &a) in neigh.iter().enumerate() {
        for &b in &neigh[i + 1..] {
            // Only adjacent neighbour pairs form a real fan triangle.
            if !geo.neighbors(a).contains(&b) {
                continue;
            }
            let (wc, wa, wb) = barycentric(s, pc, geo.position(a), geo.position(b));
            let min = wc.min(wa).min(wb);
            // Pick the triangle whose most-negative weight is closest to zero;
            // a containing triangle has all weights >= 0.
            if best.as_ref().map(|(_, m)| min > *m).unwrap_or(true) {
                best = Some(([(c, wc), (a, wa), (b, wb)], min));
            }
        }
    }
    match best {
        Some((mut w, min)) if min >= -1e-9 => {
            // Clamp tiny negatives and renormalise so weights are a clean
            // partition of unity.
            let mut sum = 0.0;
            for (_, x) in w.iter_mut() {
                if *x < 0.0 {
                    *x = 0.0;
                }
                sum += *x;
            }
            if sum > 0.0 {
                for (_, x) in w.iter_mut() {
                    *x /= sum;
                }
            }
            w
        }
        _ => [(c, 1.0), (c, 0.0), (c, 0.0)],
    }
}

/// Barycentrically interpolate a per-cell scalar at `s`.
fn interp(
    geo: &Geosphere,
    index: &NearestCellIndex,
    s: [f64; 3],
    value: impl Fn(CellId) -> f64,
) -> f64 {
    triangle_weights(geo, index, s)
        .iter()
        .map(|(c, w)| w * value(*c))
        .sum()
}
```

- [ ] **Step 4: Run to verify PASS**

Run: `cargo test -p hornvale-scene region::tests`
Expected: PASS (all Task 1 + Task 2 tests).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/scene/src/region.rs
git commit -m "feat(scene): region — deterministic barycentric cell-triangle interpolation"
```

---

### Task 3: The `RegionScene` document and `tiles_region_scene`

**Files:**
- Modify: `windows/scene/src/region.rs`

**Interfaces:**
- Consumes: `RegionAddr`, `triangle_weights`, `interp` (Tasks 1–2); `hornvale_worldgen::{terrain_of, climate_of}`; the terrain/climate provider readers used in `tiles_scene`.
- Produces: `pub struct RegionScene { .. }`; `pub fn tiles_region_scene(world: &World, face: u32, level: u32, ix: u32, iy: u32, samples: u32) -> Result<RegionScene, SceneError>`; `pub fn region_json(scene: &RegionScene) -> String`.

- [ ] **Step 1: Write the failing tests**

Append to `region.rs`'s test module:

```rust
    use hornvale_kernel::Seed;
    use hornvale_worldgen::{build_world, climate_of, SkyChoice};

    fn gen42() -> hornvale_kernel::World {
        build_world(Seed(42), &Default::default(), SkyChoice::Generated, &Default::default(), &Default::default())
            .expect("seed 42 builds")
    }

    #[test]
    fn region_scene_is_sized_and_byte_deterministic() {
        let w = gen42();
        let a = region_json(&tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap());
        let b = region_json(&tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap());
        assert_eq!(a, b);
        let scene = tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap();
        let nodes = 17 * 17;
        for len in [scene.elevation_m.len(), scene.ocean.len(), scene.biome.len(),
                    scene.plate.len(), scene.unrest.len(), scene.t_mean_c.len(),
                    scene.t_swing_c.len(), scene.moisture.len()] {
            assert_eq!(len, nodes);
        }
        assert_eq!(scene.schema, "scene/tiles-region/v1");
        assert_eq!(scene.biome_legend.len(), 22);
        assert!(scene.moisture.iter().all(|&m| (0.0..=1.0).contains(&m)));
    }

    #[test]
    fn discrete_layers_match_nearest_cell() {
        let w = gen42();
        let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
        let index = NearestCellIndex::new(terrain.geosphere());
        let a = RegionAddr { face: 2, level: 2, ix: 1, iy: 1, samples: 8 };
        let scene = tiles_region_scene(&w, a.face, a.level, a.ix, a.iy, a.samples).unwrap();
        for (i, s) in a.node_units().iter().enumerate() {
            let cell = index.nearest_to_position(terrain.geosphere(), *s);
            assert_eq!(scene.ocean[i], terrain.is_ocean(cell), "ocean node {i}");
            assert_eq!(scene.plate[i], terrain.plate_of(cell), "plate node {i}");
        }
    }

    #[test]
    fn locked_world_zeroes_swing_and_omits_bands() {
        use hornvale_astronomy::{RotationPin, SkyPins};
        let sky = SkyPins { rotation: Some(RotationPin::Locked), ..Default::default() };
        let w = build_world(Seed(42), &sky, SkyChoice::Generated, &Default::default(), &Default::default()).unwrap();
        let scene = tiles_region_scene(&w, 0, 2, 1, 1, 8).unwrap();
        assert!(scene.t_swing_c.iter().all(|&s| s == 0.0));
        assert_eq!(scene.circulation_bands, None);
        assert!(!region_json(&scene).contains("circulation_bands"));
    }
```

- [ ] **Step 2: Run to verify FAIL**

Run: `cargo test -p hornvale-scene region::tests::region_scene`
Expected: FAIL (`RegionScene`/`tiles_region_scene` not found).

- [ ] **Step 3: Implement `RegionScene` and the builder**

Add `use hornvale_kernel::World;` and `use serde::Serialize;` to `region.rs`'s imports, then append (before the tests):

```rust
/// One `scene/tiles-region/v1` document (The Region §3.3). Field order is the
/// JSON key order and is contract. Per-node layers are `(samples+1)²`,
/// row-major (`i = row·(samples+1) + col`). Continuous layers are barycentric;
/// discrete layers (`ocean`, `biome`, `plate`) are nearest-cell.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples), pending(wave-3: sea_level_m), bare-ok(diagnostic-value: season_period_days), bare-ok(count: circulation_bands), bare-ok(identifier-text: biome_legend), waiver(elevation-convention: elevation_m), bare-ok(flag: ocean), bare-ok(index: biome), bare-ok(index: plate), bare-ok(ratio: unrest), bare-ok(diagnostic-value: t_mean_c), bare-ok(diagnostic-value: t_swing_c), bare-ok(ratio: moisture)
#[derive(Debug, Serialize)]
pub struct RegionScene {
    /// Always `scene/tiles-region/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// Cube face (0..=5).
    pub face: u32,
    /// Quadtree level.
    pub level: u32,
    /// Tile column.
    pub ix: u32,
    /// Tile row.
    pub iy: u32,
    /// Quads per edge; the node grid is `(samples+1)²`.
    pub samples: u32,
    /// Sea level, meters (document-level).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
    /// Seasonal sinusoid period, standard days (document-level; self-contained).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub season_period_days: f64,
    /// Circulation bands per hemisphere; omitted when tidally locked.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub circulation_bands: Option<u32>,
    /// The biome catalog, v1's stable append-only order.
    pub biome_legend: Vec<String>,
    /// Elevation per node, meters (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub elevation_m: Vec<f64>,
    /// Ocean flag per node (nearest-cell; the categorical coastline truth).
    pub ocean: Vec<bool>,
    /// Biome index per node (nearest-cell).
    pub biome: Vec<u16>,
    /// Plate id per node (nearest-cell).
    pub plate: Vec<u32>,
    /// Unrest per node, [0,1] (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub unrest: Vec<f64>,
    /// Annual-mean temperature per node, °C (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_mean_c: Vec<f64>,
    /// Hemisphere-signed seasonal half-swing per node, °C (barycentric; 0 when locked).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_swing_c: Vec<f64>,
    /// Moisture index per node, [0,1] (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub moisture: Vec<f64>,
}

/// Build the `scene/tiles-region/v1` scene for one tile address. Deterministic:
/// same world + same address → byte-identical once serialized.
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples)
pub fn tiles_region_scene(
    world: &World,
    face: u32,
    level: u32,
    ix: u32,
    iy: u32,
    samples: u32,
) -> Result<RegionScene, SceneError> {
    let addr = RegionAddr { face, level, ix, iy, samples };
    addr.validate()?;
    let terrain = hornvale_worldgen::terrain_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let climate = hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let t_index = NearestCellIndex::new(terrain.geosphere());
    let c_index = NearestCellIndex::new(climate.geosphere());
    let biomes = climate.biome_map();
    let catalog = hornvale_climate::Biome::catalog();
    let units = addr.node_units();

    let mut elevation_m = Vec::with_capacity(units.len());
    let mut ocean = Vec::with_capacity(units.len());
    let mut biome = Vec::with_capacity(units.len());
    let mut plate = Vec::with_capacity(units.len());
    let mut unrest = Vec::with_capacity(units.len());
    let mut t_mean_c = Vec::with_capacity(units.len());
    let mut t_swing_c = Vec::with_capacity(units.len());
    let mut moisture = Vec::with_capacity(units.len());
    for s in &units {
        let tg = terrain.geosphere();
        let cg = climate.geosphere();
        // Discrete: nearest-cell.
        let t_cell = t_index.nearest_to_position(tg, *s);
        let c_cell = c_index.nearest_to_position(cg, *s);
        ocean.push(terrain.is_ocean(t_cell));
        let b = *biomes.get(c_cell);
        biome.push(catalog.iter().position(|e| *e == b).expect("biome in catalog") as u16);
        plate.push(terrain.plate_of(t_cell));
        // Continuous: barycentric.
        elevation_m.push(interp(tg, &t_index, *s, |c| terrain.elevation_at(c).get()));
        unrest.push(interp(tg, &t_index, *s, |c| terrain.unrest_at(c)));
        t_mean_c.push(interp(cg, &c_index, *s, |c| climate.mean_temperature_at(c).get()));
        t_swing_c.push(interp(cg, &c_index, *s, |c| climate.seasonal_swing_at(c)));
        moisture.push(interp(cg, &c_index, *s, |c| climate.moisture_at(c)));
    }
    Ok(RegionScene {
        schema: "scene/tiles-region/v1".to_string(),
        seed: world.seed.0,
        face,
        level,
        ix,
        iy,
        samples,
        sea_level_m: terrain.sea_level().get(),
        season_period_days: climate.year_length_std(),
        circulation_bands: climate.band_count(),
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        elevation_m,
        ocean,
        biome,
        plate,
        unrest,
        t_mean_c,
        t_swing_c,
        moisture,
    })
}

/// Serialize a `RegionScene` as compact JSON (mirrors `scene_json`).
/// type-audit: bare-ok(artifact: return)
pub fn region_json(scene: &RegionScene) -> String {
    serde_json::to_string(scene).expect("a RegionScene always serializes")
}
```

- [ ] **Step 4: Run to verify PASS**

Run: `cargo test -p hornvale-scene region::`
Expected: PASS.

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/scene/src/region.rs
git commit -m "feat(scene): region — RegionScene document and tiles_region_scene builder"
```

---

### Task 4: `temperature_grid_region` and the commutation contract test

**Files:**
- Modify: `windows/scene/src/region.rs`

**Interfaces:**
- Produces: `pub fn temperature_grid_region(world: &World, face: u32, level: u32, ix: u32, iy: u32, samples: u32, day: f64) -> Result<Vec<f64>, SceneError>` — full-precision interpolated actual temperature per node; the producer-sourced ground truth for the cross-repo contract test.

- [ ] **Step 1: Write the failing test (the commutation property)**

Append to `region.rs`'s test module:

```rust
    #[test]
    fn temperature_grid_region_commutes_with_the_evaluator() {
        let w = gen42();
        let (face, level, ix, iy, samples) = (0u32, 3u32, 4u32, 4u32, 16u32);
        // The provider values, at full precision (pre-quantization), from an
        // independent rebuild of the layers.
        let climate = climate_of(&w).unwrap();
        let c_index = NearestCellIndex::new(climate.geosphere());
        let addr = RegionAddr { face, level, ix, iy, samples };
        let period = climate.year_length_std();
        let tau = std::f64::consts::TAU;
        for day in [0.0_f64, 91.3, 200.0, 366.5] {
            let grid = temperature_grid_region(&w, face, level, ix, iy, samples, day).unwrap();
            let theta = (tau * (day / period).fract()).sin();
            for (i, s) in addr.node_units().iter().enumerate() {
                // `grid[i]` is interp(temperature_at) (form A); `rhs` is
                // interp(mean) + interp(swing)·θ (form B). Commutation is exact
                // in real arithmetic but only to float rounding here (weighted
                // sums reduce in a different order), so assert tight-approximate
                // — NOT assert_eq!.
                let mean = interp(climate.geosphere(), &c_index, *s, |c| climate.mean_temperature_at(c).get());
                let swing = interp(climate.geosphere(), &c_index, *s, |c| climate.seasonal_swing_at(c));
                let rhs = mean + swing * theta;
                assert!(
                    (grid[i] - rhs).abs() <= 1e-9 * grid[i].abs().max(1.0),
                    "commutation node {i} day {day}: {} vs {}", grid[i], rhs
                );
            }
        }
    }

    #[test]
    fn temperature_grid_region_at_day_zero_equals_t_mean() {
        let w = gen42();
        let grid = temperature_grid_region(&w, 0, 3, 4, 4, 16, 0.0).unwrap();
        let scene = tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap();
        for (g, m) in grid.iter().zip(scene.t_mean_c.iter()) {
            // grid is full precision; t_mean_c is quantized — agree to ~8 sig digits.
            assert!((g - m).abs() <= 1e-6 * g.abs().max(1.0), "day-0 grid vs t_mean: {g} vs {m}");
        }
    }
```

- [ ] **Step 2: Run to verify FAIL**

Run: `cargo test -p hornvale-scene region::tests::temperature_grid_region`
Expected: FAIL (`temperature_grid_region` not found).

- [ ] **Step 3: Implement `temperature_grid_region`**

Append to `region.rs` (before tests):

```rust
/// Per-node actual temperature at `day`, °C, on the same node grid as
/// [`tiles_region_scene`] — the barycentric interpolation of `temperature_at`.
/// This equals `t_mean_c[i] + t_swing_c[i]·sin(τ·frac(day/period))` because the
/// seasonal period is global, so interpolation commutes with the evaluator
/// (The Region §3.4). Full precision (not quantized); the cross-repo contract
/// test pins the client's reconstruction against these values.
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples), bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
pub fn temperature_grid_region(
    world: &World,
    face: u32,
    level: u32,
    ix: u32,
    iy: u32,
    samples: u32,
    day: f64,
) -> Result<Vec<f64>, SceneError> {
    let addr = RegionAddr { face, level, ix, iy, samples };
    addr.validate()?;
    let climate = hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let c_index = NearestCellIndex::new(climate.geosphere());
    let cg = climate.geosphere();
    Ok(addr
        .node_units()
        .iter()
        .map(|s| interp(cg, &c_index, *s, |c| climate.temperature_at(c, day).get()))
        .collect())
}
```

- [ ] **Step 4: Run to verify PASS**

Run: `cargo test -p hornvale-scene region::`
Expected: PASS. (The commutation test's `assert_eq!` on full-precision values must hold exactly — the same arithmetic on both sides.)

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add windows/scene/src/region.rs
git commit -m "feat(scene): region — temperature_grid_region ground truth + commutation test"
```

---

### Task 5: CLI subcommand `scene tiles-region`

**Files:**
- Modify: `cli/src/` (the `scene` command dispatch — find it with `grep -rn "scene tiles\|\"tiles\"\|tiles_scene" cli/src`)
- Test: `cli/tests/` (mirror the existing `scene tiles` integration test)

**Interfaces:**
- Consumes: `hornvale_scene::{tiles_region_scene, region_json}`.
- Produces: `hornvale scene tiles-region --face F --level L --ix X --iy Y --samples N [--world PATH]` printing one document to stdout.

- [ ] **Step 1: Locate the `scene` dispatch**

Run: `grep -rn "tiles_scene\|\"tiles\"\|scene_json\|fn scene" cli/src`
Read the matched file(s) to learn the existing `scene tiles` flag-parsing pattern (std-only arg parsing, `--world` default `world.json`, `--width` parse). Mirror it.

- [ ] **Step 2: Write the failing CLI test**

In the CLI's scene test file (alongside the existing `scene tiles` test), add:

```rust
#[test]
fn scene_tiles_region_prints_a_document() {
    // Build a world to a temp file, then run `scene tiles-region`.
    let dir = tempdir_like(); // reuse the file helper the existing scene test uses
    let world = dir.join("w.json");
    run_cli(&["new", "--seed", "42", "--out", world.to_str().unwrap()]).success();
    let out = run_cli(&[
        "scene", "tiles-region", "--world", world.to_str().unwrap(),
        "--face", "0", "--level", "3", "--ix", "4", "--iy", "4", "--samples", "16",
    ]);
    let json: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(json["schema"], "scene/tiles-region/v1");
    assert_eq!(json["elevation_m"].as_array().unwrap().len(), 17 * 17);
}
```

(Adapt `tempdir_like`/`run_cli` to the exact helpers the existing scene test uses — read that test first.)

- [ ] **Step 3: Run to verify FAIL**

Run: `cargo test -p hornvale --test <scene_test_name> scene_tiles_region`
Expected: FAIL (subcommand unknown).

- [ ] **Step 4: Implement the subcommand**

Add a `tiles-region` arm to the `scene` subcommand dispatch, parsing `--face`, `--level`, `--ix`, `--iy`, `--samples` (all `u32`, required) and `--world` (default `world.json`), loading the world, calling `tiles_region_scene`, printing `region_json`. On `SceneError`, print the Display message to stderr and exit non-zero — mirror the `scene tiles` error path exactly. Update the CLI help text.

- [ ] **Step 5: Run to verify PASS**

Run: `cargo test -p hornvale --test <scene_test_name> scene_tiles_region`
Expected: PASS. Then `cargo run -p hornvale -- scene tiles-region --help` shows the flags.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add cli/src cli/tests
git commit -m "feat(cli): scene tiles-region — regional tile document subcommand"
```

---

### Task 6: `world-wasm` export `hw_scene_tiles_region`

**Files:**
- Modify: `clients/world-wasm/src/lib.rs`
- Test: extend the wasm↔CLI byte-identity smoke (find it: `grep -rn "hw_scene_tiles\|drive.mjs\|byte-identity" clients/world-wasm cli/tests`)

**Interfaces:**
- Produces: `pub extern "C" fn hw_scene_tiles_region(face: u32, level: u32, ix: u32, iy: u32, samples: u32) -> i32` (0 ok; 2 scene error with envelope; -3 no world).

- [ ] **Step 1: Add the export**

After `hw_scene_tiles` in `clients/world-wasm/src/lib.rs`:

```rust
/// Emit the current world's `scene/tiles-region/v1` JSON for one tile address.
/// 0 ok; 2 scene error (bad address; envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_tiles_region(face: u32, level: u32, ix: u32, iy: u32, samples: u32) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::tiles_region_scene(world, face, level, ix, iy, samples) {
        Ok(s) => {
            set_out(hornvale_scene::region_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}
```

- [ ] **Step 2: Build the wasm and verify the export exists**

Run: `cargo build -p hornvale-world-wasm --target wasm32-unknown-unknown --release`
Then confirm the symbol: `cargo build` succeeds and (if `wasm-objdump`/`wasm-nm` available) the export is present; otherwise the drive smoke in Step 3 is the check.

- [ ] **Step 3: Extend the wasm↔CLI byte-identity smoke**

In the existing smoke that instantiates the wasm and compares `hw_scene_tiles` output to the CLI (`drive.mjs` and/or the Rust integration test), add a case: `hw_new(42)` + `hw_scene_tiles_region(0,3,4,4,16)` must byte-match `hornvale scene tiles-region --world <seed42> --face 0 --level 3 --ix 4 --iy 4 --samples 16`. Follow the exact shape of the existing tiles comparison.

- [ ] **Step 4: Run to verify PASS**

Run the smoke per its existing invocation (e.g. `node clients/world-wasm/drive.mjs` or `cargo test -p hornvale --test <wasm_smoke>`).
Expected: PASS — wasm and CLI emit byte-identical regional documents.

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add clients/world-wasm cli/tests
git commit -m "feat(world-wasm): hw_scene_tiles_region export + wasm==CLI region smoke"
```

---

### Task 7: Reference page, golden example, and artifact-freshness wiring

**Files:**
- Create: `book/src/reference/scene-tiles-region-v1.md`
- Create: `book/src/gallery/scene-tiles-region-seed-42.json` (committed golden)
- Modify: `book/src/SUMMARY.md`; `book/src/reference/scene-tiles-v1.md` (one-line cross-link); the artifact regen list (`.github/workflows/ci.yml` "Artifacts are current" step + `scripts/regenerate-artifacts.sh` — find with `grep -rn "scene-tiles-seed-42\|scene tiles" .github scripts Makefile`)

**Interfaces:** none (docs + artifact).

- [ ] **Step 1: Write the reference page**

Create `book/src/reference/scene-tiles-region-v1.md` covering, in order: the address + node grid with the **normative projection** (FACES, `param`, `faceUnit`, and that sampling uses the unit vector directly; lat/lon `= asin(z)/atan2(y,x)` is derived); the sampling table (continuous barycentric vs discrete nearest-cell); the field table linking `scene-tiles-v1.md` for per-layer meaning; the temperature evaluator + the commutation property; the two honesty caveats (the ~110 km floor; interpolated elevation vs the nearest-cell coastline); a Stability section (save-format-class, append-at-end, new fields never re-mean). Prose at the book's technical altitude. State the committed example's exact address (`seed 42, face 0, level 3, ix 4, iy 4, samples 16`).

- [ ] **Step 2: Generate the golden and wire freshness**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json
cargo run -p hornvale -- scene tiles-region --world /tmp/hv42.json --face 0 --level 3 --ix 4 --iy 4 --samples 16 > book/src/gallery/scene-tiles-region-seed-42.json
```

Add both the generation command (to the CI "Artifacts are current" step and `scripts/regenerate-artifacts.sh`) and `book/src/gallery/scene-tiles-region-seed-42.json` (to the `git diff --exit-code` drift check paths), mirroring how `scene-tiles-seed-42.json` is handled.

- [ ] **Step 3: SUMMARY + cross-link + docs consistency**

Add a SUMMARY line `- [Scene Schema: tiles-region v1](./reference/scene-tiles-region-v1.md)` after the tiles-v1 entry. Add a one-line "Going closer" pointer in `scene-tiles-v1.md` to the regional page. Run:

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS (all cross-links resolve).

- [ ] **Step 4: Verify drift check is green**

```bash
mdbook build book
git diff --exit-code book/src/gallery/ book/src/reference/
```
Expected: no diff (the committed golden matches fresh generation).

- [ ] **Step 5: commit**

```bash
git add book/src/reference/scene-tiles-region-v1.md book/src/gallery/scene-tiles-region-seed-42.json book/src/SUMMARY.md .github scripts
git commit -m "docs(the-region): scene-tiles-region-v1 reference page + seed-42 golden"
```

- [ ] **Step 6: Full hornvale gate**

Run: `make gate`
Expected: green (fmt + clippy + nextest + doctests). Also run the type audit: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` → passes. This closes the hornvale (producer) half.

---

### Task 8: Orrery — typed `RegionScene` parser

**Repo:** `~/Projects/hornvale/orrery` (separate repo; work there, not the hornvale worktree).

**Files:**
- Modify: `src/sim/scene.ts` (or a sibling; follow how `TilesScene` is parsed)
- Test: the vitest file covering scene parsing

**Interfaces:**
- Produces: a `RegionScene` TS type + strict parser `parseRegionScene(json): RegionScene`.

- [ ] **Step 1: Write the failing vitest**

Add a test that a hand-built minimal valid `scene/tiles-region/v1` object parses, and that a wrong array length / missing field / `schema` mismatch throws. Mirror the existing `TilesScene` parser test.

- [ ] **Step 2: Run to verify FAIL**

Run: `npm test -- scene` (in the orrery repo). Expected: FAIL.

- [ ] **Step 3: Implement the strict parser**

Add the `RegionScene` type (fields per §3.3: `schema, seed, face, level, ix, iy, samples, sea_level_m, season_period_days, circulation_bands?, biome_legend, elevation_m, ocean, biome, plate, unrest, t_mean_c, t_swing_c, moisture`) and `parseRegionScene` validating: `schema === "scene/tiles-region/v1"`, every per-node array length `=== (samples+1)²`, all numbers finite, `biome` indices `< biome_legend.length`, `circulation_bands >= 1` when present. Strict because the client is pinned to a `world-wasm-v3` release.

- [ ] **Step 4: Run to verify PASS**

Run: `npm test -- scene`. Expected: PASS.

- [ ] **Step 5: Commit** (in the orrery repo)

```bash
git add src/sim/scene.ts <test>
git commit -m "feat(scene): strict scene/tiles-region/v1 parser"
```

---

### Task 9: Orrery — extend the temperature evaluator to regional arrays

**Repo:** `~/Projects/hornvale/orrery`

**Files:**
- Modify: `src/sim/climate.ts` (extend `temperatureAt`/`coldestC` to accept regional layer arrays — do not fork)
- Test: the climate evaluator vitest

**Interfaces:**
- Consumes: `RegionScene` (Task 8).
- Produces: `temperatureAt` accepting a `{ t_mean_c, t_swing_c, season_period_days }`-shaped source (both `TilesScene` and `RegionScene` satisfy it).

- [ ] **Step 1: Write the failing evaluator-equivalence test**

Commit producer-pinned golden triples in orrery testdata. Generate them in the **hornvale** repo:

```bash
# in ~/Projects/hornvale/hornvale (or the worktree), a throwaway example or test print:
# for the golden address (0,3,4,4,16), print temperature_grid_region at a few days,
# quantized, for a handful of node indices, with a provenance comment.
```

Add a vitest asserting the extended `temperatureAt` over the parsed regional layers reproduces those Rust values to quantization precision (~1e-6 relative).

- [ ] **Step 2: Run to verify FAIL**

Run: `npm test -- climate`. Expected: FAIL.

- [ ] **Step 3: Implement the extension**

Generalize `temperatureAt(source, i, day)` so `source` is any object exposing `t_mean_c`, `t_swing_c`, `season_period_days` — the evaluator body is unchanged (`mean + swing·sin(τ·frac(day/period))`). Ensure `coldestC` likewise. One evaluator, both scene shapes.

- [ ] **Step 4: Run to verify PASS**

Run: `npm test -- climate`. Expected: PASS — the extended evaluator crosses the repo boundary, proving the commutation property end-to-end.

- [ ] **Step 5: Commit**

```bash
git add src/sim/climate.ts <test> <testdata>
git commit -m "feat(climate): extend temperature evaluator to regional layers"
```

---

### Task 10: Orrery — single-patch render proof, re-pin, seam smoke

**Repo:** `~/Projects/hornvale/orrery`

**Files:**
- Modify: `public/hornvale_world.wasm` (vendored v3 binary), `CATALOG_VERSION` constant, a view/module that builds one patch mesh
- Test: vitest wasm-fixture + a seam smoke; existing Playwright stays green

**Interfaces:**
- Consumes: the built `world-wasm-v3` binary; `cubeSphere.ts` (`tileGrid`, `faceUnit`); Tasks 8–9.

- [ ] **Step 1: Build and vendor the v3 wasm**

In hornvale: `cargo build -p hornvale-world-wasm --target wasm32-unknown-unknown --release`, then copy the `.wasm` to the orrery's `public/hornvale_world.wasm` (follow how the orrery's build/vendor script does it — `grep -rn "hornvale_world.wasm\|CATALOG_VERSION" .` in the orrery repo). Bump `CATALOG_VERSION` to `world-wasm-v3` **in the same commit** as the parser/evaluator so the client never strict-parses against an old binary.

- [ ] **Step 2: Write the failing wasm-fixture + seam test**

vitest: instantiate the vendored wasm, `hw_new(42)`, `hw_scene_tiles_region(0,3,4,4,16)`, assert `parseRegionScene` accepts the real document. Seam smoke: build the patch's `(N+1)²` node positions from the address via `cubeSphere.ts` and assert a shared-edge node's position matches the global surface at that lat/lon within tolerance (the patch registers on the globe).

- [ ] **Step 3: Run to verify FAIL then implement**

Run: `npm test`. Expected: FAIL until the vendored v3 binary + the single-patch builder exist. Implement the minimal patch builder: given an address, fetch the regional document, build a `(N+1)²` grid mesh with `elevation_m` displacing along the node unit vectors, place it on the globe. Not the full LOD tree — one patch, proving the path.

- [ ] **Step 4: Run to verify PASS**

Run: `npm test` and the Playwright smoke. Expected: PASS; the patch renders registered on the globe.

- [ ] **Step 5: Commit**

```bash
git add public/hornvale_world.wasm src <tests>
git commit -m "feat(lod): single regional patch proof + world-wasm-v3 re-pin"
```

---

## Close (G6 — Nathan-authorized)

Not a task; the campaign-close checklist (see `closing-a-campaign`): both gates green; `world-wasm-v3` tag push + release (Nathan authorizes); orrery re-pin merged; hv#3 closed with a comment on what shipped; orrery#2 commented (data path unblocked; full CDLOD renderer remains); chronicle entry (`book/src/chronicle/the-region.md`); retrospective (`docs/retrospectives/`); book freshness sweep incl. any Confidence-Gradient re-score; followups promoted; ledger digest presented at G6.

## Self-Review

**Spec coverage:** §3.1 projection/grid → Task 1; §3.2 sampling posture → Tasks 2–3; §3.3 document → Task 3; §3.4 evaluator + commutation → Task 4 (producer) + Task 9 (client); §3.5 honesty caveats → Task 7 (page); §4 producer → Tasks 1–4; wasm → Task 6; CLI → Task 5; §5 docs/golden → Task 7; §6 consumer → Tasks 8–10; §7 contract tests → Tasks 1–4, 6 (producer) + 8–10 (consumer); §8 additive-only → honored (no v1/census/epoch touch); §9 non-goals → full CDLOD explicitly deferred to Close note / orrery#2. No gaps.

**Placeholder scan:** the only deliberately-deferred specifics are the exact CLI/test helper names in Tasks 5/8–10, each gated behind an explicit "read the existing X first / `grep -rn`" step because those helpers live in files this plan doesn't quote — not open-ended TODOs.

**Type consistency:** `RegionAddr`/`tiles_region_scene`/`temperature_grid_region`/`region_json`/`triangle_weights`/`interp` names and signatures are consistent across Tasks 1–4, 6; `RegionScene` field names match between the Rust struct (Task 3) and the TS parser (Task 8); the golden address `(0,3,4,4,16)` is identical across Tasks 3, 4, 7, 9, 10.
