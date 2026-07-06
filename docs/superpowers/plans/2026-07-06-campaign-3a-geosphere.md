# Campaign 3a: The Geosphere — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `Geosphere` primitive to the kernel — a deterministic icosphere region graph (cells, adjacency, geographic coordinates) plus a cell-indexed data container `CellMap<T>` — the spatial substrate the whole of Campaign 3 (tectonics, climate, biomes) will compute over.

**Architecture:** The Geosphere is the vertices of a subdivided icosahedron: cells are vertices, adjacency is the triangulation's edges (twelve cells with five neighbors, the rest with six), and each cell has a unit-sphere position from which latitude/longitude derive. It is **seed-independent** — fully determined by its subdivision level — so it uses no `Seed`/`Stream` and no randomness, and it is recomputed rather than serialized (like `Field`). `CellMap<T>` is a `Vec<T>` indexed by `CellId`, the representation later domains use for per-cell elevation, temperature, and biome.

**Tech Stack:** Rust (edition 2024), `std` only. No serde (the Geosphere is recomputed from its level, never persisted). No new dependencies.

## Global Constraints

- Dependencies: `serde` + `serde_json` only, workspace-wide; **this plan adds no dependencies and no serde derives** (the Geosphere is not serialized). Standard library only.
- Determinism is constitutional: same subdivision level → byte-identical mesh, every run. No `HashMap`/`HashSet` in construction (use `BTreeMap`/`BTreeSet` so vertex-index assignment and neighbor ordering are order-obvious); no wall-clock; no `Math.random`-style entropy (there is none here — the mesh is pure geometry).
- Every crate sets `#![warn(missing_docs)]`; **every public item, field, and variant gets a one-line `///` doc comment** (lint-enforced).
- Rust edition 2024. Run `cargo fmt` as the final step before every commit; the full gate is `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
- The Geosphere lives in the kernel (`kernel/src/geosphere.rs`) and is re-exported from `kernel/src/lib.rs`. The kernel "changes rarely"; keep this a pure spatial substrate with no domain logic.
- Observability note: unlike a domain, the Geosphere is infrastructure with no standalone world output (like `Field`). It becomes observable when the terrain provider (Plan 3b) computes over it; this plan ships with tests as its correctness surface, no REPL/almanac/artifact.
- Cell count identity: a level-`L` icosphere has exactly `10 * 4^L + 2` cells (level 0 = 12, 1 = 42, 2 = 162, 3 = 642, 4 = 2562, 5 = 10242).

---

## File Structure

- `kernel/src/geosphere.rs` (new) — the entire Geosphere primitive: `CellId`, `GeoCoord`, `Geosphere`, `CellMap<T>`, and the private icosahedron/subdivision/adjacency construction. One file, one responsibility (the spatial substrate).
- `kernel/src/lib.rs` (modify) — declare `pub mod geosphere;` and re-export the public types.

No other files change. No domain, window, or CLI code is touched by 3a.

---

### Task 1: Base icosahedron and the `Geosphere` skeleton

**Files:**
- Create: `kernel/src/geosphere.rs`
- Modify: `kernel/src/lib.rs` (add `pub mod geosphere;` and re-exports)
- Test: inline `#[cfg(test)] mod tests` in `kernel/src/geosphere.rs`

**Interfaces:**
- Produces:
  - `pub struct CellId(pub u32)` — a cell identifier (index into the mesh).
  - `pub struct Geosphere` (opaque fields) with `pub fn new(level: u32) -> Geosphere`, `pub fn level(&self) -> u32`, `pub fn cell_count(&self) -> usize`, `pub fn cells(&self) -> impl Iterator<Item = CellId>`, `pub fn position(&self, id: CellId) -> [f64; 3]`.
  - In Task 1, `new` builds only the base icosahedron (level is stored but higher levels are subdivided in Task 2). All twelve base vertices are unit-length.

- [ ] **Step 1: Write the failing test**

Add to `kernel/src/geosphere.rs` (create the file with just this test block plus a `use super::*;`):

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_icosahedron_has_twelve_unit_cells() {
        let geo = Geosphere::new(0);
        assert_eq!(geo.level(), 0);
        assert_eq!(geo.cell_count(), 12);
        for id in geo.cells() {
            let [x, y, z] = geo.position(id);
            let len = (x * x + y * y + z * z).sqrt();
            assert!((len - 1.0).abs() < 1e-12, "cell {id:?} not unit-length: {len}");
        }
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel geosphere::tests::base_icosahedron_has_twelve_unit_cells`
Expected: FAIL to compile — `Geosphere` not found (the module isn't declared/implemented yet).

- [ ] **Step 3: Write the minimal implementation**

Put this at the top of `kernel/src/geosphere.rs` (above the test module):

```rust
//! The Geosphere: a deterministic icosphere region graph over the unit
//! sphere. Cells are the vertices of a subdivided icosahedron; adjacency is
//! the triangulation's edges. It is seed-independent (fully determined by its
//! subdivision level) and never serialized — recomputed on demand, like a
//! `Field`. This is the spatial substrate the terrain and climate domains
//! compute over.

/// Identifier for a cell — an index into the mesh's vertices.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellId(pub u32);

/// A discretized planetary surface. Cells are the vertices of an icosahedron
/// subdivided `level` times; every cell sits on the unit sphere.
#[derive(Clone, Debug)]
pub struct Geosphere {
    /// Subdivision level (0 = the bare icosahedron).
    level: u32,
    /// Unit-sphere position per cell, indexed by `CellId`.
    positions: Vec<[f64; 3]>,
    /// Triangular faces as cell-index triples (used to derive adjacency).
    faces: Vec<[u32; 3]>,
}

/// Normalize a 3-vector to unit length.
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let [x, y, z] = v;
    let len = (x * x + y * y + z * z).sqrt();
    [x / len, y / len, z / len]
}

/// The twelve icosahedron vertices (golden-ratio rectangles), unnormalized,
/// and the twenty triangular faces as vertex-index triples. Fixed data — the
/// source of the Geosphere's determinism.
fn base_icosahedron() -> (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    let p = (1.0 + 5.0_f64.sqrt()) / 2.0; // golden ratio
    let raw = [
        [-1.0, p, 0.0], [1.0, p, 0.0], [-1.0, -p, 0.0], [1.0, -p, 0.0],
        [0.0, -1.0, p], [0.0, 1.0, p], [0.0, -1.0, -p], [0.0, 1.0, -p],
        [p, 0.0, -1.0], [p, 0.0, 1.0], [-p, 0.0, -1.0], [-p, 0.0, 1.0],
    ];
    let vertices = raw.iter().map(|&v| normalize(v)).collect();
    let faces = vec![
        [0, 11, 5], [0, 5, 1], [0, 1, 7], [0, 7, 10], [0, 10, 11],
        [1, 5, 9], [5, 11, 4], [11, 10, 2], [10, 7, 6], [7, 1, 8],
        [3, 9, 4], [3, 4, 2], [3, 2, 6], [3, 6, 8], [3, 8, 9],
        [4, 9, 5], [2, 4, 11], [6, 2, 10], [8, 6, 7], [9, 8, 1],
    ];
    (vertices, faces)
}

impl Geosphere {
    /// Build an icosphere subdivided `level` times. (Task 1 handles the base;
    /// Task 2 adds subdivision for `level > 0`.)
    pub fn new(level: u32) -> Geosphere {
        let (positions, faces) = base_icosahedron();
        Geosphere { level, positions, faces }
    }

    /// The subdivision level.
    pub fn level(&self) -> u32 {
        self.level
    }

    /// The number of cells.
    pub fn cell_count(&self) -> usize {
        self.positions.len()
    }

    /// Iterate every cell id in ascending order.
    pub fn cells(&self) -> impl Iterator<Item = CellId> {
        (0..self.positions.len() as u32).map(CellId)
    }

    /// The unit-sphere position of a cell.
    pub fn position(&self, id: CellId) -> [f64; 3] {
        self.positions[id.0 as usize]
    }
}
```

Then add to `kernel/src/lib.rs` — a new module line (keep the existing alphabetical grouping) and re-export:

```rust
pub mod geosphere;
```
and in the `pub use` block:
```rust
pub use geosphere::{CellId, Geosphere};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel geosphere::tests::base_icosahedron_has_twelve_unit_cells`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add kernel/src/geosphere.rs kernel/src/lib.rs
git commit -m "feat(kernel): Geosphere base icosahedron skeleton"
```

---

### Task 2: Subdivision

**Files:**
- Modify: `kernel/src/geosphere.rs`
- Test: inline tests module

**Interfaces:**
- Consumes: `Geosphere::new(level)` from Task 1.
- Produces: `Geosphere::new(level)` now subdivides `level` times, so `cell_count() == 10 * 4usize.pow(level) + 2`. Vertex-index assignment is deterministic: faces are visited in order, and each new edge-midpoint vertex is assigned the next sequential index on first encounter (a `BTreeMap` edge cache dedups shared midpoints).

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn subdivision_yields_the_icosphere_cell_counts() {
    // 10 * 4^L + 2
    assert_eq!(Geosphere::new(0).cell_count(), 12);
    assert_eq!(Geosphere::new(1).cell_count(), 42);
    assert_eq!(Geosphere::new(2).cell_count(), 162);
    assert_eq!(Geosphere::new(3).cell_count(), 642);
}

#[test]
fn subdivided_cells_are_all_unit_length() {
    let geo = Geosphere::new(3);
    for id in geo.cells() {
        let [x, y, z] = geo.position(id);
        let len = (x * x + y * y + z * z).sqrt();
        assert!((len - 1.0).abs() < 1e-12, "cell {id:?} not unit-length: {len}");
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel geosphere::tests::subdivision_yields_the_icosphere_cell_counts`
Expected: FAIL — `cell_count()` returns 12 for every level (subdivision not implemented).

- [ ] **Step 3: Write the minimal implementation**

Add a `use` at the top of the file and a subdivision helper, and call it from `new`:

```rust
use std::collections::BTreeMap;
```

Add this free function:

```rust
/// Subdivide each triangular face into four, projecting new edge-midpoint
/// vertices onto the unit sphere. Shared midpoints are deduplicated via an
/// edge cache keyed by the ordered vertex-index pair, so vertex numbering is
/// deterministic (faces visited in order; a new midpoint takes the next
/// sequential index on first encounter).
fn subdivide(
    positions: Vec<[f64; 3]>,
    faces: Vec<[u32; 3]>,
) -> (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    let mut positions = positions;
    let mut cache: BTreeMap<(u32, u32), u32> = BTreeMap::new();
    let mut midpoint = |a: u32, b: u32, positions: &mut Vec<[f64; 3]>| -> u32 {
        let key = (a.min(b), a.max(b));
        if let Some(&idx) = cache.get(&key) {
            return idx;
        }
        let [ax, ay, az] = positions[a as usize];
        let [bx, by, bz] = positions[b as usize];
        let mid = normalize([(ax + bx) / 2.0, (ay + by) / 2.0, (az + bz) / 2.0]);
        let idx = positions.len() as u32;
        positions.push(mid);
        cache.insert(key, idx);
        idx
    };
    let mut new_faces = Vec::with_capacity(faces.len() * 4);
    for [a, b, c] in faces {
        let ab = midpoint(a, b, &mut positions);
        let bc = midpoint(b, c, &mut positions);
        let ca = midpoint(c, a, &mut positions);
        new_faces.push([a, ab, ca]);
        new_faces.push([b, bc, ab]);
        new_faces.push([c, ca, bc]);
        new_faces.push([ab, bc, ca]);
    }
    (positions, new_faces)
}
```

Change `new` to subdivide `level` times:

```rust
    pub fn new(level: u32) -> Geosphere {
        let (mut positions, mut faces) = base_icosahedron();
        for _ in 0..level {
            (positions, faces) = subdivide(positions, faces);
        }
        Geosphere { level, positions, faces }
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel geosphere::tests::subdivision`
Expected: PASS (both subdivision tests).

- [ ] **Step 5: Commit**

```bash
git add kernel/src/geosphere.rs
git commit -m "feat(kernel): Geosphere icosphere subdivision"
```

---

### Task 3: Adjacency (the region graph)

**Files:**
- Modify: `kernel/src/geosphere.rs`
- Test: inline tests module

**Interfaces:**
- Consumes: the `faces` built in Tasks 1–2.
- Produces: `pub fn neighbors(&self, id: CellId) -> &[CellId]` returning each cell's adjacent cells in ascending `CellId` order. Exactly twelve cells (the original icosahedron vertices, ids 0–11) have five neighbors; every other cell has six. Adjacency is mutual.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn adjacency_is_mutual_and_has_the_right_valence() {
    let geo = Geosphere::new(3);
    let mut fives = 0usize;
    let mut sixes = 0usize;
    for id in geo.cells() {
        let ns = geo.neighbors(id);
        match ns.len() {
            5 => fives += 1,
            6 => sixes += 1,
            other => panic!("cell {id:?} has {other} neighbors (expected 5 or 6)"),
        }
        // sorted ascending, no self, no duplicates
        let mut sorted = ns.to_vec();
        sorted.sort();
        sorted.dedup();
        assert_eq!(sorted.as_slice(), ns, "neighbors of {id:?} not sorted/deduped");
        assert!(!ns.contains(&id), "cell {id:?} lists itself as a neighbor");
        // mutual: each neighbor lists id back
        for &n in ns {
            assert!(geo.neighbors(n).contains(&id), "{n:?} not mutual with {id:?}");
        }
    }
    assert_eq!(fives, 12, "exactly twelve pentagonal cells expected");
    assert_eq!(sixes, geo.cell_count() - 12);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel geosphere::tests::adjacency_is_mutual_and_has_the_right_valence`
Expected: FAIL to compile — `neighbors` method not found.

- [ ] **Step 3: Write the minimal implementation**

Add `BTreeSet` to the import and a `neighbors` field + accessor, building adjacency in `new`:

```rust
use std::collections::{BTreeMap, BTreeSet};
```

Add a field to the struct:

```rust
    /// Adjacent cells per cell, ascending, indexed by `CellId`.
    neighbors: Vec<Vec<CellId>>,
```

Add a builder function:

```rust
/// Derive per-cell adjacency from the triangular faces: two cells are
/// adjacent iff they share a face edge. Neighbor lists are sorted ascending.
fn build_neighbors(cell_count: usize, faces: &[[u32; 3]]) -> Vec<Vec<CellId>> {
    let mut sets: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); cell_count];
    for &[a, b, c] in faces {
        for (u, v) in [(a, b), (b, c), (c, a)] {
            sets[u as usize].insert(v);
            sets[v as usize].insert(u);
        }
    }
    sets.into_iter()
        .map(|s| s.into_iter().map(CellId).collect())
        .collect()
}
```

Update `new` to build and store neighbors (after the subdivision loop):

```rust
    pub fn new(level: u32) -> Geosphere {
        let (mut positions, mut faces) = base_icosahedron();
        for _ in 0..level {
            (positions, faces) = subdivide(positions, faces);
        }
        let neighbors = build_neighbors(positions.len(), &faces);
        Geosphere { level, positions, faces, neighbors }
    }
```

Add the accessor:

```rust
    /// The cells adjacent to `id`, in ascending `CellId` order.
    pub fn neighbors(&self, id: CellId) -> &[CellId] {
        &self.neighbors[id.0 as usize]
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel geosphere::tests::adjacency_is_mutual_and_has_the_right_valence`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add kernel/src/geosphere.rs
git commit -m "feat(kernel): Geosphere adjacency (the region graph)"
```

---

### Task 4: Geographic coordinates

**Files:**
- Modify: `kernel/src/geosphere.rs`
- Test: inline tests module

**Interfaces:**
- Consumes: `Geosphere::position`.
- Produces:
  - `pub struct GeoCoord { pub latitude: f64, pub longitude: f64 }` — degrees; latitude in `[-90, 90]`, longitude in `(-180, 180]`.
  - `pub fn coord(&self, id: CellId) -> GeoCoord`.
  - `pub fn latitude(v: [f64; 3]) -> f64` and `pub fn longitude(v: [f64; 3]) -> f64` are NOT public; conversion is exposed only via `coord`. The mapping: `latitude = asin(z)`, `longitude = atan2(y, x)`, both converted to degrees, for a unit vector `[x, y, z]`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn coordinates_are_in_range_and_convert_correctly() {
    let geo = Geosphere::new(3);
    for id in geo.cells() {
        let c = geo.coord(id);
        assert!(c.latitude >= -90.0 && c.latitude <= 90.0, "lat out of range: {}", c.latitude);
        assert!(c.longitude > -180.0 && c.longitude <= 180.0, "lon out of range: {}", c.longitude);
    }
}

#[test]
fn coordinate_conversion_matches_known_directions() {
    // A cell whose position is the +z pole would read latitude +90; test the
    // conversion directly through a constructed sphere is awkward, so assert
    // the mapping via the closest cell to +z on a fine sphere instead.
    let geo = Geosphere::new(4);
    let north = geo
        .cells()
        .max_by(|a, b| geo.position(*a)[2].total_cmp(&geo.position(*b)[2]))
        .unwrap();
    let c = geo.coord(north);
    // The most-northern cell sits high but not exactly at the pole; assert it
    // is in the northern hemisphere and its longitude is well-defined.
    assert!(c.latitude > 60.0, "northernmost cell latitude {} too low", c.latitude);
    assert!(c.longitude > -180.0 && c.longitude <= 180.0);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel geosphere::tests::coordinate`
Expected: FAIL to compile — `coord`/`GeoCoord` not found.

- [ ] **Step 3: Write the minimal implementation**

```rust
/// A geographic coordinate in degrees.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GeoCoord {
    /// Latitude in degrees, `[-90, 90]` (north positive).
    pub latitude: f64,
    /// Longitude in degrees, `(-180, 180]` (east positive).
    pub longitude: f64,
}
```

Add the accessor to `impl Geosphere`:

```rust
    /// The geographic coordinate of a cell.
    pub fn coord(&self, id: CellId) -> GeoCoord {
        let [x, y, z] = self.positions[id.0 as usize];
        GeoCoord {
            latitude: z.asin().to_degrees(),
            longitude: y.atan2(x).to_degrees(),
        }
    }
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel geosphere::tests::coordinate`
Expected: PASS (both coordinate tests).

- [ ] **Step 5: Commit**

```bash
git add kernel/src/geosphere.rs
git commit -m "feat(kernel): Geosphere geographic coordinates"
```

---

### Task 5: `CellMap<T>`, determinism, and API polish

**Files:**
- Modify: `kernel/src/geosphere.rs`, `kernel/src/lib.rs` (re-export `CellMap`, `GeoCoord`)
- Test: inline tests module

**Interfaces:**
- Consumes: `Geosphere` (Tasks 1–4).
- Produces:
  - `pub struct CellMap<T>` with `pub fn from_fn(geo: &Geosphere, f: impl FnMut(CellId) -> T) -> CellMap<T>`, `pub fn get(&self, id: CellId) -> &T`, `pub fn len(&self) -> usize`, `pub fn is_empty(&self) -> bool`, `pub fn iter(&self) -> impl Iterator<Item = (CellId, &T)>`.
  - This is the cell-indexed container later domains use for elevation, temperature, and biome. A `CellMap` built from a Geosphere has exactly `cell_count()` entries.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn cellmap_covers_every_cell_and_indexes_by_id() {
    let geo = Geosphere::new(2);
    let doubled = CellMap::from_fn(&geo, |id| id.0 * 2);
    assert_eq!(doubled.len(), geo.cell_count());
    assert!(!doubled.is_empty());
    for id in geo.cells() {
        assert_eq!(*doubled.get(id), id.0 * 2);
    }
    let total: u32 = doubled.iter().map(|(_, v)| *v).sum();
    let expected: u32 = geo.cells().map(|id| id.0 * 2).sum();
    assert_eq!(total, expected);
}

#[test]
fn geosphere_is_deterministic() {
    // Same level -> byte-identical mesh (positions, neighbors, coords).
    let a = Geosphere::new(4);
    let b = Geosphere::new(4);
    assert_eq!(a.cell_count(), b.cell_count());
    for id in a.cells() {
        assert_eq!(a.position(id), b.position(id), "position drift at {id:?}");
        assert_eq!(a.neighbors(id), b.neighbors(id), "neighbor drift at {id:?}");
        assert_eq!(a.coord(id), b.coord(id), "coord drift at {id:?}");
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel geosphere::tests::cellmap_covers_every_cell_and_indexes_by_id`
Expected: FAIL to compile — `CellMap` not found.

- [ ] **Step 3: Write the minimal implementation**

```rust
/// A value per cell, indexed by `CellId`. Built from a `Geosphere`, so it has
/// exactly one entry per cell. This is the representation domains use for
/// derived per-cell data (elevation, temperature, biome).
#[derive(Clone, Debug, PartialEq)]
pub struct CellMap<T> {
    values: Vec<T>,
}

impl<T> CellMap<T> {
    /// Build a `CellMap` by evaluating `f` at every cell of `geo`, in
    /// ascending `CellId` order.
    pub fn from_fn(geo: &Geosphere, mut f: impl FnMut(CellId) -> T) -> CellMap<T> {
        CellMap { values: geo.cells().map(&mut f).collect() }
    }

    /// The value at a cell.
    pub fn get(&self, id: CellId) -> &T {
        &self.values[id.0 as usize]
    }

    /// The number of cells.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Whether the map is empty.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Iterate `(CellId, &value)` pairs in ascending `CellId` order.
    pub fn iter(&self) -> impl Iterator<Item = (CellId, &T)> {
        self.values.iter().enumerate().map(|(i, v)| (CellId(i as u32), v))
    }
}
```

Update the `kernel/src/lib.rs` re-export to the full set:

```rust
pub use geosphere::{CellId, CellMap, GeoCoord, Geosphere};
```

- [ ] **Step 4: Run test to verify it passes, then run the full gate**

Run: `cargo test -p hornvale-kernel geosphere::tests`
Expected: PASS (all geosphere tests).

Then the full gate:
```bash
cargo test --workspace
cargo fmt
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
```
Expected: all green; fmt clean; no clippy warnings.

- [ ] **Step 5: Commit**

```bash
git add kernel/src/geosphere.rs kernel/src/lib.rs
git commit -m "feat(kernel): CellMap and Geosphere determinism"
```

---

## Self-Review Notes

**Spec coverage (against `2026-07-06-campaign-3-the-land-design.md` §3):** The spec's kernel addition — "an icosphere region graph (cells, adjacency, per-cell latitude/longitude), seed-independent, computed once" plus the cell-indexed representation the grounding pass found missing — is fully covered: Tasks 1–2 (mesh), Task 3 (adjacency/region graph), Task 4 (lat/long), Task 5 (`CellMap<T>`, the per-cell data container elevation/temperature/biome will use). The tectonic and climate models (spec §4–§7) are explicitly out of scope for 3a and land in Plan 3b, which will target this merged API. Determinism (spec §12) is covered by `geosphere_is_deterministic` and the no-RNG/`BTreeMap`/`BTreeSet` construction.

**Placeholder scan:** No TBD/TODO. Every implementation step contains complete, compilable Rust (icosahedron constants and face list are given literally; the subdivision, adjacency, and coordinate math are complete). Test steps contain full assertions with concrete expected values.

**Type consistency:** `CellId(pub u32)`, `Geosphere`, `GeoCoord { latitude, longitude }`, and `CellMap<T>` names and signatures are identical across Tasks 1–5 and the re-export lines. `new(level: u32)`, `neighbors(&self, id) -> &[CellId]`, `coord(&self, id) -> GeoCoord`, `position(&self, id) -> [f64; 3]`, and `CellMap::{from_fn, get, len, is_empty, iter}` are used consistently. The re-export in Task 1 (`CellId`, `Geosphere`) is widened in Task 5 (`+ CellMap`, `GeoCoord`), matching each task's newly-public types.

**Deferred to Plan 3b (tectonic terrain), targeting this merged API:** plate generation, Euler-pole motion, boundary classification, elevation/unrest as `CellMap`s, sea level, pins, and the elevation artifact — all consume `Geosphere`/`CellMap` and are written once this primitive is real.
