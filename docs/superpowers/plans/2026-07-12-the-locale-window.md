# The Locale Window Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `windows/locale`, the first consumer of the room mesh — a crate that turns a `RoomAddr` into an observable `Locale` (inherited biome, blended fields, seed-derived texture, exits) plus a `hornvale locale` CLI "look".

**Architecture:** A new window crate depends on `hornvale-kernel` + `hornvale-worldgen` + `hornvale-climate` + `hornvale-terrain`. A `LocaleContext` builds the coarse world once (`climate_of`/`terrain_of`) and `describe(&addr, at)` composes room detail on top: max-weight categorical biome (inherited, never re-quantized), integer-weighted continuous fields, seed-derived texture, and base+vertical exits. One small presentation-side kernel helper, `RoomAddr::containing`, locates a room from a position. `Locale` serializes as the versioned schema `locale/room/v1`.

**Tech Stack:** Rust (edition 2024), std-only + `serde`/`serde_json`. No new crates. Determinism: integer identity, full-precision compute, `quantize` at the emit boundary.

## Global Constraints

- **Layering (enforced by `cli/tests/architecture.rs`):** `kernel → domains/* → windows/* → cli`. `hornvale-locale` is a window; it may depend only on kernel, domains, and other windows. It constructs **no providers** — coarse fields come through `hornvale_worldgen`.
- **Dependencies:** `serde` + `serde_json` only as externals. No `rand`/`chrono`/`clap`/`thiserror`.
- **No `HashMap`/`HashSet`:** `BTreeMap`/`BTreeSet`/`Vec`. Float sort via `total_cmp`. (Enforced by `clippy.toml`.)
- **No wall-clock:** observation time is `hornvale_kernel::WorldTime { day: f64 }`, threaded — never read from a clock.
- **Determinism:** same `(seed, addr)` → byte-identical `Locale`. Integer address seeding is platform-exact; blended/coordinate floats are presentation and pass through `hornvale_kernel::quantize` (8 significant digits) at the emit boundary.
- **Docs:** every crate sets `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line doc comment.
- **Type-audit:** every primitive at a `pub` boundary carries a `type-audit:` verdict tag; new float presentation helpers use `pending(wave-1)`. Run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before committing boundary changes.
- **fmt is the final step before every commit:** `cargo fmt`. The full gate is `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.

---

### Task 1: `RoomAddr::containing` — locate a room from a position (kernel)

**Files:**
- Modify: `kernel/src/room.rs` (add the `containing` method + private helpers near `corners`/`centroid`)
- Test: `kernel/src/room.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: private `base_data()` (geosphere), `slerp_mid`, `normalize` (already in `room.rs`); `RoomAddr { pub face: u8, pub path: Vec<u8> }`.
- Produces: `RoomAddr::containing(position: [f64; 3], depth: u32) -> RoomAddr` — the room at `depth` whose spherical triangle contains `position`. **Presentation-side** (float→address; a boundary-straddling position may resolve to adjacent rooms across platforms; content stays integer-exact once addressed). NOTE: the approved spec wrote `containing(geo, position, depth)`; the `geo` parameter is dropped because the room mesh is face-relative and independent of the globe level — an unused param would trip `-D warnings`.

- [ ] **Step 1: Write the failing round-trip + face test**

Add to the `tests` module in `kernel/src/room.rs`:

```rust
#[test]
fn containing_round_trips_room_centroids() {
    // Every interior room's own centroid must resolve back to that room.
    for depth in [1u32, 2, 3] {
        for face in 0..20u8 {
            // enumerate all rooms at `depth` on this face
            let mut stack = vec![RoomAddr { face, path: vec![] }];
            for _ in 0..depth {
                let mut next = Vec::new();
                for a in stack {
                    for d in 0..4u8 {
                        next.push(a.child(d).unwrap());
                    }
                }
                stack = next;
            }
            for room in stack {
                let got = RoomAddr::containing(room.centroid(), depth);
                assert_eq!(got, room, "centroid of {room:?} resolved to {got:?}");
            }
        }
    }
}

#[test]
fn containing_at_depth_zero_is_the_base_face() {
    let room = RoomAddr { face: 7, path: vec![1, 2] };
    let got = RoomAddr::containing(room.centroid(), 0);
    assert_eq!(got, RoomAddr { face: 7, path: vec![] });
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-kernel containing_ 2>&1 | tail -20`
Expected: FAIL — `no function or associated item named 'containing'`.

- [ ] **Step 3: Implement `containing` + helpers**

Add near `centroid`/`coord` in `kernel/src/room.rs`. If `dot`/`cross` vector helpers are not already present in the file, add them (module-private):

```rust
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Is unit point `p` inside the spherical triangle `a,b,c`? Orientation-robust:
/// `p` is on the same side of each edge's great circle as the opposite corner.
fn point_in_sph_tri(p: [f64; 3], a: [f64; 3], b: [f64; 3], c: [f64; 3]) -> bool {
    let side = |x: [f64; 3], y: [f64; 3], q: [f64; 3]| dot(q, cross(x, y));
    side(a, b, p) * side(a, b, c) >= 0.0
        && side(b, c, p) * side(b, c, a) >= 0.0
        && side(c, a, p) * side(c, a, b) >= 0.0
}
```

And the method inside `impl RoomAddr` (the block with `corners`/`centroid`):

```rust
    /// The room at `depth` whose spherical triangle contains `position`.
    /// PRESENTATION-SIDE: resolves a float coordinate to an integer address
    /// (float→address, the same determinism class as `NearestCellIndex::nearest`).
    /// Not an identity path — a boundary-straddling position may resolve to
    /// adjacent rooms on different platforms; a room's content stays
    /// integer-exact once addressed. Descends by the same `slerp_mid` midpoints
    /// and child order as `corners`, so `containing(r.centroid(), r.depth()) == r`
    /// for interior rooms.
    /// type-audit: pending(wave-1)
    pub fn containing(position: [f64; 3], depth: u32) -> RoomAddr {
        let p = normalize(position);
        let (verts, faces) = base_data();
        // 1. the base face whose spherical triangle contains p (fallback:
        //    the face whose centroid is nearest, for numerically-on-seam points).
        let corners_of = |f: u8| {
            let g = faces[f as usize];
            [
                verts[g[0] as usize],
                verts[g[1] as usize],
                verts[g[2] as usize],
            ]
        };
        let face = (0..faces.len() as u8)
            .find(|&f| {
                let c = corners_of(f);
                point_in_sph_tri(p, c[0], c[1], c[2])
            })
            .unwrap_or_else(|| {
                (0..faces.len() as u8)
                    .max_by(|&x, &y| {
                        let cx = corners_of(x);
                        let cy = corners_of(y);
                        let sx = normalize([
                            cx[0][0] + cx[1][0] + cx[2][0],
                            cx[0][1] + cx[1][1] + cx[2][1],
                            cx[0][2] + cx[1][2] + cx[2][2],
                        ]);
                        let sy = normalize([
                            cy[0][0] + cy[1][0] + cy[2][0],
                            cy[0][1] + cy[1][1] + cy[2][1],
                            cy[0][2] + cy[1][2] + cy[2][2],
                        ]);
                        dot(p, sx).total_cmp(&dot(p, sy))
                    })
                    .unwrap()
            });
        // 2. descend, mirroring `corners`'s slerp_mid split and child order.
        let mut v = corners_of(face);
        let mut path = Vec::with_capacity(depth as usize);
        for _ in 0..depth {
            let ab = slerp_mid(v[0], v[1]);
            let bc = slerp_mid(v[1], v[2]);
            let ca = slerp_mid(v[2], v[0]);
            let children = [
                [v[0], ab, ca],
                [v[1], bc, ab],
                [v[2], ca, bc],
                [ab, bc, ca],
            ];
            let d = (0..4u8)
                .find(|&d| {
                    let c = children[d as usize];
                    point_in_sph_tri(p, c[0], c[1], c[2])
                })
                .unwrap_or(3);
            v = children[d as usize];
            path.push(d);
        }
        RoomAddr { face, path }
    }
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel containing_ 2>&1 | tail -20`
Expected: PASS (2 tests). If `containing_round_trips_room_centroids` fails on a center-child chain, the `children` array order is wrong — it must exactly match `corners`'s `match d` arms (`0=[v0,ab,ca] 1=[v1,bc,ab] 2=[v2,ca,bc] 3=[ab,bc,ca]`).

- [ ] **Step 5: Verify the full kernel gate + type-audit**

Run: `cargo test -p hornvale-kernel 2>&1 | tail -5 && cargo clippy -p hornvale-kernel --all-targets -- -D warnings 2>&1 | tail -5 && cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -3`
Expected: tests pass, no clippy warnings, type-audit `check` OK.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add kernel/src/room.rs
git commit -m "feat(kernel): RoomAddr::containing — presentation-side coord→room locator

Descends the spherical mesh mirroring corners()'s slerp_mid + child order, so
a room's centroid round-trips to that room. Float→address presentation helper
(the NearestCellIndex::nearest determinism class), not an identity path.

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

### Task 2: The `hornvale-locale` crate — schema, `LocaleContext`, inheritance & blend

**Files:**
- Create: `windows/locale/Cargo.toml`
- Create: `windows/locale/src/lib.rs`
- Modify: `Cargo.toml` (workspace `members`)
- Test: `windows/locale/src/lib.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `hornvale_worldgen::{climate_of, terrain_of}`, `hornvale_kernel::{World, Seed, WorldTime, RoomAddr, RoomId, CellId, NearestCellIndex, quantize}`, `hornvale_climate::Biome`.
- Produces:
  - `pub const ROOM_SCHEMA: &str = "locale/room/v1";`
  - `pub struct Locale` (the DTO below — plain serializable values), `pub struct LocaleFields`, `pub struct SubCellTexture`, `pub struct Exit`, `pub enum Direction`, `pub enum Compass`, `pub enum ExitKind`, `pub enum LocaleError`.
  - `pub struct LocaleContext` with `pub fn build(world: &World) -> Result<LocaleContext, LocaleError>`, `pub fn describe(&self, addr: &RoomAddr, at: WorldTime) -> Result<Locale, LocaleError>`, `pub fn globe_level(&self) -> u32`.

- [ ] **Step 1: Create the crate manifest and register it in the workspace**

Create `windows/locale/Cargo.toml`:

```toml
[package]
name = "hornvale-locale"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale locale window: a RoomAddr rendered as an observable place."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-worldgen = { path = "../worldgen" }
hornvale-climate = { path = "../../domains/climate" }
hornvale-terrain = { path = "../../domains/terrain" }
serde = { workspace = true }
serde_json = { workspace = true }
```

In the root `Cargo.toml`, add `"windows/locale"` to the `members` array (keep the list sorted if it is).

- [ ] **Step 2: Write the failing inheritance/blend tests**

Create `windows/locale/src/lib.rs` with the test module (the types don't exist yet, so it won't compile — that is the red state):

```rust
#![warn(missing_docs)]
//! The locale window: a `RoomAddr` rendered as an observable place.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{RoomAddr, Seed, World, WorldTime};

    fn land_world() -> World {
        // Seed 42 is the project's canonical fixture; it has land.
        World::new(Seed(42))
    }

    #[test]
    fn describe_is_deterministic_across_two_contexts() {
        let world = land_world();
        let addr = RoomAddr { face: 0, path: vec![1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0] };
        let a = LocaleContext::build(&world).unwrap();
        let b = LocaleContext::build(&world).unwrap();
        let la = a.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let lb = b.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(
            serde_json::to_string(&la).unwrap(),
            serde_json::to_string(&lb).unwrap()
        );
    }

    #[test]
    fn describe_above_the_grid_errors() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // A room coarser than the canonical grid has no corner weights.
        let coarse = RoomAddr { face: 0, path: vec![1] };
        assert!(matches!(
            ctx.describe(&coarse, WorldTime { day: 0.0 }),
            Err(LocaleError::AboveGrid)
        ));
    }

    #[test]
    fn fields_are_within_the_corner_range() {
        // A weighted blend never leaves the min..max of its inputs.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        // elevation blends three real cells; the value must be finite.
        assert!(loc.fields.elevation_m.is_finite());
        assert!(loc.fields.temperature_c.is_finite());
        assert_eq!(loc.schema, ROOM_SCHEMA);
    }
}
```

- [ ] **Step 3: Run to verify it fails (does not compile)**

Run: `cargo test -p hornvale-locale 2>&1 | tail -15`
Expected: FAIL — `cannot find type LocaleContext` / `LocaleError` etc.

- [ ] **Step 4: Implement the schema types + `LocaleContext` (inheritance & blend)**

Prepend to `windows/locale/src/lib.rs` (above the test module):

```rust
use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::{
    quantize, CellId, NearestCellIndex, RoomAddr, Seed, World, WorldTime,
};
use hornvale_terrain::GeneratedTerrain;
use hornvale_worldgen::{climate_of, terrain_of};
use serde::Serialize;

/// The versioned semantic schema this window emits (save-format class; a
/// changed meaning mints `locale/room/v2` alongside).
/// type-audit: bare-ok(identifier-text)
pub const ROOM_SCHEMA: &str = "locale/room/v1";

/// A room rendered as an observable place — ground truth, re-derivable, never
/// stored (UNI-20 derived view). Plain serializable values only.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(index: id/face/path/depth), bare-ok(coordinate: latitude/longitude), waiver(presentation: fields), bare-ok(prose: biome), bare-ok(count: corners)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Locale {
    /// Schema tag (`locale/room/v1`).
    pub schema: &'static str,
    /// Packed room id (`RoomId.0`).
    pub id: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Child descent path.
    pub path: Vec<u8>,
    /// Refinement depth (`path.len()`).
    pub depth: u32,
    /// Centroid latitude, degrees (quantized).
    pub latitude: f64,
    /// Centroid longitude, degrees (quantized).
    pub longitude: f64,
    /// Inherited biome name (max-weight corner cell).
    pub biome: String,
    /// Blended continuous fields.
    pub fields: LocaleFields,
    /// The three canonical-grid corner cells and their integer weights.
    pub corners: Vec<CellWeight>,
    /// Seed-derived sub-cell texture.
    pub texture: SubCellTexture,
    /// Base + vertical exits.
    pub exits: Vec<Exit>,
}

/// A canonical-grid corner cell and its integer blend weight.
/// type-audit: bare-ok(index: cell), bare-ok(count: weight)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CellWeight {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Integer weight (numerator over the summed denominator).
    pub weight: u64,
}

/// The blended continuous fields at the room centroid (weighted mean of the
/// three corner cells; quantized at emit).
/// type-audit: waiver(presentation: temperature_c/moisture/elevation_m)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LocaleFields {
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Moisture (climate's dimensionless moisture field).
    pub moisture: f64,
    /// Elevation, meters.
    pub elevation_m: f64,
}

/// Seed-derived sub-cell texture — an explicit P3 placeholder, kept tiny.
/// type-audit: bare-ok(prose: aspect), bare-ok(ratio: relief_jitter)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SubCellTexture {
    /// One draw from a tiny biome-keyed pool (the P3 descriptor stand-in).
    pub aspect: String,
    /// A deterministic per-room character scalar in [-1, 1] (dimensionless).
    pub relief_jitter: f64,
}

/// Why a locale could not be described.
/// type-audit: bare-ok(prose: Build.0)
#[derive(Debug, Clone, PartialEq)]
pub enum LocaleError {
    /// Building the coarse world failed (worldgen).
    Build(String),
    /// The room is coarser than the canonical grid, so it has no inheritance.
    AboveGrid,
}

impl std::fmt::Display for LocaleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocaleError::Build(m) => write!(f, "building the coarse world: {m}"),
            LocaleError::AboveGrid => {
                write!(f, "room is coarser than the canonical grid (no inheritance)")
            }
        }
    }
}

/// The reusable coarse-world build. Constructed once, reused across every
/// `describe` — so a locale stays a cheap derived view.
pub struct LocaleContext {
    seed: Seed,
    climate: GeneratedClimate,
    terrain: GeneratedTerrain,
    index: NearestCellIndex,
    globe_level: u32,
}

impl LocaleContext {
    /// Build the coarse world (climate + terrain + nearest-cell index) once.
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError> {
        let climate = climate_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let terrain = terrain_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let index = NearestCellIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().level();
        Ok(LocaleContext {
            seed: world.seed,
            climate,
            terrain,
            index,
            globe_level,
        })
    }

    /// The canonical globe level (canonical-grid refinement depth).
    pub fn globe_level(&self) -> u32 {
        self.globe_level
    }

    /// A room's ground-truth locale at observation time `at`. Pure over
    /// (context, addr, at): same inputs → byte-identical `Locale`. v1 samples
    /// the time-independent annual mean and does not yet vary with `at`
    /// (threaded for the P8 temporal-phase layer).
    pub fn describe(&self, addr: &RoomAddr, _at: WorldTime) -> Result<Locale, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();

        // Categorical biome: max weight, tie-break lowest CellId. Inherited,
        // never re-quantized (decision 0038).
        let mut best = weights[0];
        for &cand in &weights[1..] {
            if cand.1 > best.1 || (cand.1 == best.1 && cand.0 .0 < best.0 .0) {
                best = cand;
            }
        }
        let biome = self.climate.biome_at(best.0);

        // Continuous fields: integer-weighted mean, full precision, quantize
        // at emit.
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            quantize(sum / denom as f64)
        };
        let fields = LocaleFields {
            temperature_c: blend(&|c| self.climate.mean_temperature_at(c)),
            moisture: blend(&|c| self.climate.moisture_at(c)),
            elevation_m: blend(&|c| *self.terrain.globe().elevation.get(c)),
        };

        let coord = addr.coord();
        Ok(Locale {
            schema: ROOM_SCHEMA,
            id: addr.pack().map(|r| r.0).unwrap_or(0),
            face: addr.face,
            path: addr.path.clone(),
            depth: addr.depth(),
            latitude: quantize(coord.latitude),
            longitude: quantize(coord.longitude),
            biome: biome_name(biome).to_string(),
            fields,
            corners: weights
                .iter()
                .map(|&(c, w)| CellWeight { cell: c.0, weight: w })
                .collect(),
            texture: texture_of(self.seed, addr, biome), // Task 3 fills this in
            exits: exits_of(addr),                        // Task 4 fills this in
        })
    }
}

/// Stable biome name for the `locale/room/v1` schema (owned here, not Debug).
fn biome_name(b: Biome) -> &'static str {
    match b {
        Biome::Ice => "ice",
        Biome::Tundra => "tundra",
        Biome::Taiga => "taiga",
        Biome::TemperateGrassland => "temperate grassland",
        Biome::Shrubland => "shrubland",
        Biome::TemperateForest => "temperate forest",
        Biome::TemperateRainforest => "temperate rainforest",
        Biome::Desert => "desert",
        Biome::Savanna => "savanna",
        Biome::TropicalSeasonalForest => "tropical seasonal forest",
        Biome::TropicalRainforest => "tropical rainforest",
        Biome::Alpine => "alpine",
        Biome::SeaIce => "sea ice",
        Biome::CoralReef => "coral reef",
        Biome::KelpForest => "kelp forest",
        Biome::HydrothermalVent => "hydrothermal vent",
        Biome::HadalTrench => "hadal trench",
        Biome::Upwelling => "upwelling",
        Biome::Epipelagic => "epipelagic",
        Biome::Mesopelagic => "mesopelagic",
        Biome::Bathypelagic => "bathypelagic",
        Biome::Abyssal => "abyssal",
    }
}
```

To let the file compile before Tasks 3–4 land, add **temporary** stubs at the bottom of the non-test code (they are replaced in Tasks 3 and 4):

```rust
// --- temporary stubs (replaced in Task 3 / Task 4) ---
fn texture_of(_seed: Seed, _addr: &RoomAddr, _biome: Biome) -> SubCellTexture {
    SubCellTexture { aspect: String::new(), relief_jitter: 0.0 }
}
fn exits_of(_addr: &RoomAddr) -> Vec<Exit> {
    Vec::new()
}
/// Placeholder types filled in by Task 4.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Exit {
    /// Exit direction.
    pub direction: String,
    /// Exit kind.
    pub kind: String,
    /// Destination packed room id.
    pub to: u64,
}
```

(Task 4 replaces the placeholder `Exit` with the real `Direction`/`Compass`/`ExitKind` model. Keeping a string-y stub now lets Task 2 compile and be committed independently.)

Confirm the biome variant list matches the source: run `grep -n 'pub enum Biome' -A40 domains/climate/src/biome.rs` and reconcile any variant the match is missing (the compiler will also force exhaustiveness).

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-locale 2>&1 | tail -15`
Expected: PASS (3 tests). If `describe_above_the_grid_errors` fails, verify the globe level: `path: vec![1]` is depth 1, which is `< globe_level` (default 6), so `corner_weights` returns `None`.

- [ ] **Step 6: Verify architecture + clippy + type-audit, then commit**

Run: `cargo test -p hornvale --test architecture 2>&1 | tail -5 && cargo clippy -p hornvale-locale --all-targets -- -D warnings 2>&1 | tail -5 && cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -3`
Expected: architecture test passes (the new window's deps are all kernel/domains/windows), no clippy warnings, type-audit OK.

```bash
cargo fmt
git add windows/locale/Cargo.toml windows/locale/src/lib.rs Cargo.toml Cargo.lock
git commit -m "feat(locale): the locale/room/v1 schema, LocaleContext, inheritance + blend

Build the coarse world once (climate + terrain + nearest-cell index) and
describe a room: max-weight inherited biome (never re-quantized), integer-
weighted continuous fields (temp/moisture/elevation), quantized at emit.
Texture and exits are stubbed pending Tasks 3-4.

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

### Task 3: Sub-cell texture + the `streams` seed-labels

**Files:**
- Create: `windows/locale/src/streams.rs`
- Modify: `windows/locale/src/lib.rs` (real `texture_of`, `aspect_pool`, module wiring, `stream_labels` re-export)
- Modify: `cli/src/streams.rs` (register the crate's labels in the manifest)
- Test: `windows/locale/src/lib.rs`

**Interfaces:**
- Consumes: `RoomAddr::seed(Seed) -> Seed`, `Seed::derive(&str) -> Seed`, `Seed::stream() -> Stream`, `Stream::pick(&[T]) -> Option<&T>`, `Stream::next_f64() -> f64`, `quantize`.
- Produces: `windows/locale/src/streams.rs` with `pub const LOCALE_ASPECT`, `pub const LOCALE_JITTER`, `pub fn stream_labels() -> Vec<(&'static str, &'static str)>`; re-exported as `hornvale_locale::stream_labels`.

- [ ] **Step 1: Write the failing texture tests**

Add to the `tests` module in `windows/locale/src/lib.rs`:

```rust
#[test]
fn texture_is_deterministic_and_siblings_differ() {
    let world = land_world();
    let ctx = LocaleContext::build(&world).unwrap();
    let a = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0] };
    let b = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1] };
    let ta = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().texture;
    let ta2 = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().texture;
    let tb = ctx.describe(&b, WorldTime { day: 0.0 }).unwrap().texture;
    assert_eq!(ta, ta2, "same room → identical texture");
    assert_ne!(
        (ta.aspect.clone(), ta.relief_jitter),
        (tb.aspect.clone(), tb.relief_jitter),
        "sibling rooms should differ"
    );
    assert!((-1.0..=1.0).contains(&ta.relief_jitter));
    assert!(!ta.aspect.is_empty());
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-locale texture_is_deterministic 2>&1 | tail -15`
Expected: FAIL — the stub returns an empty aspect and `0.0` jitter, so `!ta.aspect.is_empty()` fails (and siblings do not differ).

- [ ] **Step 3: Implement the streams module and real texture**

Create `windows/locale/src/streams.rs`:

```rust
//! Seed-derivation labels for the locale window — save-format contracts.
//! Changing a label silently moves every room's texture.

/// Stream label for a room's aspect draw.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_ASPECT: &str = "locale/aspect";
/// Stream label for a room's relief jitter.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_JITTER: &str = "locale/jitter";

/// Every locale seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (LOCALE_ASPECT, "room aspect draw"),
        (LOCALE_JITTER, "room relief jitter"),
    ]
}
```

In `windows/locale/src/lib.rs`, add the module declaration and re-export near the top (after the crate doc comment):

```rust
mod streams;
pub use streams::stream_labels;
use streams::{LOCALE_ASPECT, LOCALE_JITTER};
```

Replace the temporary `texture_of` stub with:

```rust
fn texture_of(seed: Seed, addr: &RoomAddr, biome: Biome) -> SubCellTexture {
    let room_seed = addr.seed(seed);
    let pool = aspect_pool(biome);
    let aspect = room_seed
        .derive(LOCALE_ASPECT)
        .stream()
        .pick(pool)
        .copied()
        .unwrap_or("unremarkable ground")
        .to_string();
    // next_f64 ∈ [0,1) → [-1,1), then quantize at emit.
    let jitter = quantize(room_seed.derive(LOCALE_JITTER).stream().next_f64() * 2.0 - 1.0);
    SubCellTexture {
        aspect,
        relief_jitter: jitter,
    }
}

/// A tiny biome-keyed aspect pool — the explicit stand-in for the P3
/// descriptor grammar. Kept ≤ 4 entries so it does not pre-empt MAP-29.
fn aspect_pool(biome: Biome) -> &'static [&'static str] {
    match biome {
        Biome::TemperateForest | Biome::TemperateRainforest => {
            &["dense understory", "old growth", "windthrow clearing", "mossy hollow"]
        }
        Biome::Taiga => &["boreal stand", "peat hollow", "burnt snag"],
        Biome::Desert => &["dune field", "gravel pan", "wadi cut"],
        Biome::Tundra | Biome::Alpine => &["frost heave", "boulder field", "wind scour"],
        Biome::Savanna | Biome::TemperateGrassland => {
            &["open sward", "scattered copse", "grazed flat"]
        }
        Biome::TropicalRainforest | Biome::TropicalSeasonalForest => {
            &["buttressed canopy", "liana tangle", "stream gully"]
        }
        _ => &["unremarkable ground", "broken terrain"],
    }
}
```

- [ ] **Step 4: Register the labels in the CLI manifest**

In `cli/src/streams.rs`, add to the list of `(crate, labels)` tuples (keep it alphabetical among the `hornvale-*` entries):

```rust
        ("hornvale-locale", hornvale_locale::stream_labels()),
```

The CLI crate needs the dependency — in `cli/Cargo.toml`, add under `[dependencies]`:

```toml
hornvale-locale = { path = "../windows/locale" }
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-locale texture 2>&1 | tail -10`
Expected: PASS. Then `cargo build -p hornvale 2>&1 | tail -5` — the CLI compiles with the new dependency.

- [ ] **Step 6: Regenerate the streams manifest artifact, verify, commit**

The manifest artifact is `book/src/reference/stream-manifest-generated.md`, regenerated by the single-source-of-truth script (which already runs the `streams` command — the new labels flow through automatically once the crate is registered):

Run: `bash scripts/regenerate-artifacts.sh && git diff --stat book/src/reference/stream-manifest-generated.md`
Expected: the diff shows the two new `locale/*` labels added under `hornvale-locale`. (If the script is slow, the manifest alone is `cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md`.)

```bash
cargo fmt
git add windows/locale/src/streams.rs windows/locale/src/lib.rs cli/src/streams.rs cli/Cargo.toml book/src/reference/stream-manifest-generated.md Cargo.lock
git commit -m "feat(locale): seed-derived sub-cell texture + streams labels

Aspect drawn from a tiny biome-keyed pool (the P3 stand-in) and a per-room
relief jitter in [-1,1], both off the integer room seed. Labels join the
frozen manifest (locale/aspect, locale/jitter).

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

### Task 4: Exits — base (compass) + vertical (enter/exit)

**Files:**
- Modify: `windows/locale/src/lib.rs` (replace the placeholder `Exit`; real `Direction`/`Compass`/`ExitKind`; real `exits_of` + `compass`)
- Test: `windows/locale/src/lib.rs`

**Interfaces:**
- Consumes: `RoomAddr::neighbors() -> [RoomAddr; 3]`, `RoomAddr::bearing_to(&RoomAddr) -> f64`, `RoomAddr::parent() -> Option<RoomAddr>`, `RoomAddr::child(u8) -> Result<RoomAddr, _>`, `quantize`.
- Produces: `pub struct Exit { pub direction: Direction, pub kind: ExitKind, pub to: u64 }`, `pub enum Direction { Compass(Compass), Enter(u8), Exit }`, `pub enum Compass { N, Ne, E, Se, S, Sw, W, Nw }`, `pub enum ExitKind { Edge, Vertical }`.

- [ ] **Step 1: Write the failing exit tests**

Add to the `tests` module:

```rust
#[test]
fn exits_are_three_lateral_plus_vertical() {
    let world = land_world();
    let ctx = LocaleContext::build(&world).unwrap();
    let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] };
    let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
    let lateral = loc
        .exits
        .iter()
        .filter(|e| e.kind == ExitKind::Edge)
        .count();
    assert_eq!(lateral, 3, "exactly three geometric edges");
    assert!(
        loc.exits.iter().any(|e| e.direction == Direction::Exit),
        "a mid-mesh room has a parent (Exit)"
    );
    let enters = loc
        .exits
        .iter()
        .filter(|e| matches!(e.direction, Direction::Enter(_)))
        .count();
    assert_eq!(enters, 4, "four children to enter");
    // every lateral destination is one of the substrate's neighbours
    let ns: Vec<u64> = addr
        .neighbors()
        .iter()
        .map(|n| n.pack().unwrap().0)
        .collect();
    for e in loc.exits.iter().filter(|e| e.kind == ExitKind::Edge) {
        assert!(ns.contains(&e.to), "lateral exit must be a neighbour");
    }
}

#[test]
fn compass_buckets_cover_the_circle() {
    assert_eq!(compass(0.0), Compass::N);
    assert_eq!(compass(90.0), Compass::E);
    assert_eq!(compass(180.0), Compass::S);
    assert_eq!(compass(270.0), Compass::W);
    assert_eq!(compass(45.0), Compass::Ne);
    assert_eq!(compass(359.9), Compass::N);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-locale exits_are 2>&1 | tail -15`
Expected: FAIL — `ExitKind`/`Direction`/`Compass`/`compass` unresolved, and the stub `exits_of` returns empty.

- [ ] **Step 3: Replace the placeholder `Exit` and implement exits**

In `windows/locale/src/lib.rs`, delete the temporary placeholder `Exit` struct (from Task 2) and the stub `exits_of`, and add:

```rust
/// A way out of a room. `ExitKind` is open so overlay kinds (river/road/
/// tunnel/portal) and passability compose additively later.
/// type-audit: bare-ok(count: to)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Exit {
    /// Which way this exit goes.
    pub direction: Direction,
    /// The kind of traversal.
    pub kind: ExitKind,
    /// Destination packed room id.
    pub to: u64,
}

/// An exit direction: a lateral compass bearing, or a vertical scale change.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Direction {
    /// A lateral edge, bucketed to eight compass points.
    Compass(Compass),
    /// Descend into finer child `digit` (0..4).
    Enter(u8),
    /// Step back out to the containing room.
    Exit,
}

/// Eight-point compass bucket.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Compass {
    /// North.
    N,
    /// North-east.
    Ne,
    /// East.
    E,
    /// South-east.
    Se,
    /// South.
    S,
    /// South-west.
    Sw,
    /// West.
    W,
    /// North-west.
    Nw,
}

/// The traversal class of an exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExitKind {
    /// A geometric base-mesh edge.
    Edge,
    /// A vertical scale change (enter/exit).
    Vertical,
}

/// Bucket a bearing (degrees clockwise from north) to eight points. Bucketing
/// on a quantized bearing keeps it cross-platform stable.
fn compass(bearing_deg: f64) -> Compass {
    let b = quantize((bearing_deg % 360.0 + 360.0) % 360.0);
    let idx = (((b + 22.5) / 45.0).floor() as i64).rem_euclid(8);
    [
        Compass::N,
        Compass::Ne,
        Compass::E,
        Compass::Se,
        Compass::S,
        Compass::Sw,
        Compass::W,
        Compass::Nw,
    ][idx as usize]
}

fn exits_of(addr: &RoomAddr) -> Vec<Exit> {
    let mut exits = Vec::new();
    for n in addr.neighbors() {
        exits.push(Exit {
            direction: Direction::Compass(compass(addr.bearing_to(&n))),
            kind: ExitKind::Edge,
            to: n.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    if let Some(parent) = addr.parent() {
        exits.push(Exit {
            direction: Direction::Exit,
            kind: ExitKind::Vertical,
            to: parent.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    for digit in 0..4u8 {
        if let Ok(child) = addr.child(digit) {
            exits.push(Exit {
                direction: Direction::Enter(digit),
                kind: ExitKind::Vertical,
                to: child.pack().map(|r| r.0).unwrap_or(0),
            });
        }
    }
    exits
}
```

The call site in `describe` (`exits: exits_of(addr)`) is unchanged.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-locale 2>&1 | tail -10`
Expected: PASS (all locale tests — determinism, blend, above-grid, texture, exits, compass).

- [ ] **Step 5: Verify clippy + type-audit, then commit**

Run: `cargo clippy -p hornvale-locale --all-targets -- -D warnings 2>&1 | tail -5 && cargo run --manifest-path tools/type-audit/Cargo.toml -- check 2>&1 | tail -3`
Expected: clean.

```bash
cargo fmt
git add windows/locale/src/lib.rs
git commit -m "feat(locale): base + vertical exits (compass edges, enter/exit)

Three geometric edges named by bearing (8-point compass on a quantized
bearing), plus Enter(child)/Exit(parent). ExitKind is open for future
overlay/passability layers. Enter/Exit, not Up/Down — the verb is a scale
change, keeping the Underdark's stratum descent semantics free.

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

### Task 5: The `hornvale locale` CLI + drift-checked artifact

**Files:**
- Modify: `cli/src/main.rs` (dispatch, `usage()`, `cmd_locale`)
- Create: `book/src/reference/locale-seed-42.json` (the artifact)
- Modify: `scripts/regenerate-artifacts.sh` (add the locale regen line — the single source of truth CI + `make rebaseline` both call)
- Modify: `.github/workflows/ci.yml` (add the artifact to the strict cross-platform diff **exclusion** list — see Step 7)
- Test: `cli/tests/locale_cli.rs`

> **Cross-platform determinism note (important).** `Locale`'s float fields are quantized → byte-identical across platforms. But the inherited `biome` is a per-cell **classification thresholded on host-libm transcendentals**, which diverge cross-platform in the last ULP — the exact reason the CI strict-diff already excludes `scene-tiles-seed-42.json` and the biome PNGs. So this artifact is drift-checked **locally** (a code change that alters it shows in the diff) but is **excluded from the strict cross-platform tail**, mirroring scene-tiles. Choosing the artifact room deep in a homogeneous biome (far from any threshold) further reduces the chance of divergence, but the exclusion is what makes the guarantee honest.

**Interfaces:**
- Consumes: `load_world(&[String]) -> Result<World, String>`, `flag_value(&[String], &str) -> Option<&str>` (existing CLI helpers); `hornvale_locale::{LocaleContext, ROOM_SCHEMA}`; `hornvale_kernel::{RoomAddr, RoomId, WorldTime}`.
- Produces: `hornvale locale --world W [--at LAT,LON | --room ID] [--depth D] [--json]`.

- [ ] **Step 1: Write a failing CLI smoke test**

Create `cli/tests/locale_cli.rs`:

```rust
//! The `hornvale locale` subcommand renders a room deterministically.
use std::process::Command;

fn run(args: &[&str]) -> (String, String, bool) {
    let out = Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(args)
        .output()
        .expect("run hornvale");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

#[test]
fn locale_json_is_deterministic_for_a_coordinate() {
    // Build a seed-42 world to a temp path.
    let dir = std::env::temp_dir();
    let world = dir.join("hv-locale-test.json");
    let (_o, e, ok) = run(&["new", "--seed", "42", "--out", world.to_str().unwrap()]);
    assert!(ok, "new failed: {e}");
    let w = world.to_str().unwrap();
    let (a, ea, ok_a) = run(&["locale", "--world", w, "--at", "35.0,-80.0", "--json"]);
    assert!(ok_a, "locale failed: {ea}");
    let (b, _eb, _) = run(&["locale", "--world", w, "--at", "35.0,-80.0", "--json"]);
    assert_eq!(a, b, "same coordinate → identical json");
    assert!(a.contains("\"schema\""), "json carries the schema tag");
    assert!(a.contains("locale/room/v1"));
}

#[test]
fn locale_requires_a_target() {
    let dir = std::env::temp_dir();
    let world = dir.join("hv-locale-test2.json");
    let _ = run(&["new", "--seed", "42", "--out", world.to_str().unwrap()]);
    let (_o, _e, ok) = run(&["locale", "--world", world.to_str().unwrap()]);
    assert!(!ok, "missing --at/--room must fail");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale --test locale_cli 2>&1 | tail -15`
Expected: FAIL — `unknown command 'locale'` (non-zero exit, so the determinism assert never gets a schema).

- [ ] **Step 3: Add the dispatch arm and `usage()` line**

In `cli/src/main.rs`, add to the `match` in `main` (near the other subcommands):

```rust
        Some("locale") => cmd_locale(&args),
```

In `usage()`, add a line describing it, matching the surrounding format, e.g.:

```
  locale --world W [--at LAT,LON | --room ID] [--depth D] [--json]
                          describe one room: biome, fields, texture, exits
```

- [ ] **Step 4: Implement `cmd_locale`**

Add to `cli/src/main.rs` (near `cmd_map`). Ensure the imports at the top include `use hornvale_kernel::{RoomAddr, RoomId, WorldTime};` (add what is missing):

```rust
/// Describe a single room as an observable place. Prose by default; `--json`
/// emits the `locale/room/v1` schema. Deterministic; CI drift-checks the
/// committed artifact.
fn cmd_locale(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let ctx = hornvale_locale::LocaleContext::build(&world).map_err(|e| e.to_string())?;

    let depth: u32 = match flag_value(args, "--depth") {
        Some(s) => s.parse().map_err(|_| format!("bad --depth: {s}"))?,
        None => ctx.globe_level() + 6,
    };

    let addr = if let Some(id) = flag_value(args, "--room") {
        let raw: u64 = id.parse().map_err(|_| format!("bad --room id: {id}"))?;
        RoomId(raw).unpack().map_err(|e| format!("invalid room id: {e:?}"))?
    } else if let Some(at) = flag_value(args, "--at") {
        let (lat, lon) = at
            .split_once(',')
            .ok_or_else(|| "expected --at LAT,LON".to_string())?;
        let lat: f64 = lat.trim().parse().map_err(|_| "bad latitude".to_string())?;
        let lon: f64 = lon.trim().parse().map_err(|_| "bad longitude".to_string())?;
        let (la, lo) = (lat.to_radians(), lon.to_radians());
        let position = [la.cos() * lo.cos(), la.cos() * lo.sin(), la.sin()];
        RoomAddr::containing(position, depth)
    } else {
        return Err("provide --at LAT,LON or --room ID".to_string());
    };

    let locale = ctx
        .describe(&addr, WorldTime { day: 0.0 })
        .map_err(|e| e.to_string())?;

    if args.iter().any(|a| a == "--json") {
        let json = serde_json::to_string_pretty(&locale).map_err(|e| e.to_string())?;
        println!("{json}");
    } else {
        println!("Locale {} (depth {})", locale.id, locale.depth);
        println!(
            "  at {:.4}, {:.4}",
            locale.latitude, locale.longitude
        );
        println!("  biome: {}", locale.biome);
        println!(
            "  temperature {:.1} °C · moisture {:.2} · elevation {:.0} m",
            locale.fields.temperature_c, locale.fields.moisture, locale.fields.elevation_m
        );
        println!("  aspect: {}", locale.texture.aspect);
        println!("  exits:");
        for e in &locale.exits {
            let label = match &e.direction {
                hornvale_locale::Direction::Compass(c) => format!("{c:?}"),
                hornvale_locale::Direction::Enter(d) => format!("enter[{d}]"),
                hornvale_locale::Direction::Exit => "exit".to_string(),
            };
            println!("    {label:<8} → room {}", e.to);
        }
    }
    Ok(())
}
```

- [ ] **Step 5: Run the CLI test to verify it passes**

Run: `cargo test -p hornvale --test locale_cli 2>&1 | tail -10`
Expected: PASS (2 tests).

- [ ] **Step 6: Generate the committed artifact (pick a land room)**

Find a land coordinate for seed 42 and record its room id, then emit the artifact:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json
# Inspect a candidate land coordinate (adjust lat,lon until biome is not an
# ocean biome), then capture its RoomId from the --json "id" field:
cargo run -p hornvale -- locale --world /tmp/hv42.json --at 35.0,-80.0 --json
# Emit the committed artifact by ROOM id (stable regardless of default depth):
ROOM_ID=$(cargo run -p hornvale -- locale --world /tmp/hv42.json --at 35.0,-80.0 --json | sed -n 's/.*"id": \([0-9]*\).*/\1/p')
cargo run -p hornvale -- locale --world /tmp/hv42.json --room "$ROOM_ID" --json > book/src/reference/locale-seed-42.json
```

Verify the file is over land (a non-ocean `biome`). Record `$ROOM_ID` in the CI step below.

- [ ] **Step 7: Wire regeneration into the script and exclude from the strict cross-platform tail**

Add the regeneration to `scripts/regenerate-artifacts.sh` (the single source of truth), beside the other `hornvale --` artifact commands. Match the seed-42 world path the script already builds (find it: `grep -n 'seed 42\|hv.json\|--out' scripts/regenerate-artifacts.sh`). Use the literal id recorded in Step 6:

```bash
cargo run -p hornvale -- locale --world "$WORLD" --room <ROOM_ID> --json > book/src/reference/locale-seed-42.json
```

Then, in `.github/workflows/ci.yml`, add the artifact to the strict-diff **exclusion** list (mirroring `scene-tiles-seed-42.json`), because the inherited `biome` classification diverges cross-platform (see the note under Files):

```yaml
            ':(exclude)book/src/reference/locale-seed-42.json'
```

Local regeneration (`bash scripts/regenerate-artifacts.sh`) still writes the file, and a local re-run drift-checks it; the exclusion only prevents a false Linux/macOS mismatch on the biome field.

- [ ] **Step 8: Confirm the full gate, then commit**

Run: `cargo test --workspace 2>&1 | tail -5 && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings 2>&1 | tail -5`
Expected: all green.

```bash
cargo fmt
git add cli/src/main.rs cli/tests/locale_cli.rs book/src/reference/locale-seed-42.json scripts/regenerate-artifacts.sh .github/workflows/ci.yml
git commit -m "feat(cli): hornvale locale — the room 'look' + locale/room/v1 artifact

Prose by default, --json emits the schema; --at LAT,LON locates via
RoomAddr::containing, --room ID round-trips a packed id, --depth overrides the
globe_level+6 default. Drift-checked seed-42 artifact under book/src/reference.

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

### Task 6: Campaign Definition of Done — book, registry, retrospective

**Files:**
- Create: `book/src/chronicle/the-locale-window.md` (+ its `SUMMARY.md` entry)
- Modify: `book/src/frontier/idea-registry.md` (MAP-29 note; MAP-28 consumer link)
- Modify: any stale room-scale/frontier chapter that named the locale window as "planned"
- Modify: `book/src/open-questions.md` (re-score any Confidence-Gradient bet this resolves)
- Create: `docs/retrospectives/the-locale-window.md`
- Modify: the generated layering page (regenerate; a new crate changed it)

This task is docs/process; use the **closing-a-campaign** skill to drive it. It carries no code test, but the book freshness sweep and the layering-page regeneration are gated by `cargo test --workspace` (`docs_consistency` + `architecture`).

- [ ] **Step 1: Regenerate the layering page and confirm the architecture test is green**

The new `hornvale-locale` crate changes the enforced dependency graph, so the golden `book/src/reference/layering-generated.md` must be rebaselined (it is authored by the enforcer, never by hand):

Run: `REBASELINE=1 cargo test -p hornvale --test architecture the_layering_page_matches_the_enforced_graph` (or `make rebaseline-goldens`), then confirm without the env var:

Run: `cargo test -p hornvale --test architecture 2>&1 | tail -5`
Expected: `the_layering_page_matches_the_enforced_graph` passes; the diff on `layering-generated.md` shows the `hornvale-locale` row added.

- [ ] **Step 2: Write the chronicle entry**

Create `book/src/chronicle/the-locale-window.md` — a prose chronicle at the book's altitude (technical, comprehensible without the code): the room mesh gets its first consumer; inheritance vs re-quantization (the §14 Q4 resolution); enter/exit vs up/down; the LocaleContext as the derived-view discipline; and the deliberate low-variety spine that P3/P5/P7 will enrich. Add its entry to `book/src/SUMMARY.md` under the Chronicle part.

- [ ] **Step 3: Freshness sweep + registry**

Update `book/src/frontier/idea-registry.md`: MAP-28 gains a "first consumer shipped" note linking the locale window; MAP-29's row notes the coarse-field inheritance path is now real (the biome bestiary rides a working spine). Sweep any room-scale chapter that described the locale window as planned. Re-score in `book/src/open-questions.md` any Confidence-Gradient bet this campaign moved.

Run: `cargo test -p hornvale --test docs_consistency 2>&1 | tail -5`
Expected: PASS (all frontier links/ToC/IDs resolve).

- [ ] **Step 4: Retrospective**

Create `docs/retrospectives/the-locale-window.md` — one page of process lessons (not product): e.g., the registry-first + ideonomy pass that caught the four interface-stability issues before code, and whether the TDD task slicing held.

- [ ] **Step 5: Final full gate + commit**

Run: `cargo test --workspace 2>&1 | tail -5 && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings 2>&1 | tail -5`
Expected: all green.

```bash
cargo fmt
git add book/ docs/retrospectives/the-locale-window.md
git commit -m "docs(locale): campaign DoD — chronicle, freshness sweep, retrospective

Claude-Session: https://claude.ai/code/session_0191iXcAsVTwuu9r5YAtY3ms"
```

---

## Notes for the executor

- **Absorb main at each stage boundary** (project process): run `make preflight` from the branch; on an ancestry NO-GO, merge main into `the-locale-window` and re-run the gate there. Do not absorb mid-artifact-generation.
- **Task 2 leaves temporary stubs** (`texture_of`, `exits_of`, a string-y `Exit`) so the crate compiles and commits independently; Tasks 3 and 4 replace them. This is deliberate task isolation, not a placeholder to leave behind — both stubs are gone by the end of Task 4.
- **The one real correctness risk is Task 1's child-order match.** If `containing_round_trips_room_centroids` fails, the `children` array does not match `corners`'s `match d` arms — fix the array, not the test.
- **Determinism debugging:** if the CI drift check fails cross-platform but not locally, the culprit is an un-quantized float reaching the schema (a blended field, a coordinate, or a compass bearing bucketed before quantization). Every float in `Locale` must pass through `quantize`, and `compass` must bucket the quantized bearing.
- **Type-audit class strings:** the `type-audit:` tags in the code blocks use plausible class names, but the exact allowed set is what `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` enforces. If a tag is rejected, use the class the nearest existing code uses (e.g. how `GeoCoord`'s lat/lon and `TectonicGlobe::elevation` are tagged) — the `check` gate in Tasks 1/2/4 will name any offender.
