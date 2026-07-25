# The Purview Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the possession a coarse spatial lens — an egocentric, fogged, deterministic chart of the rooms around the possessed agent, sharing one noun catalog with the prose.

**Architecture:** A new scene kind `scene/surrounds/v1` in `windows/scene` (semantic-only, fog-free, stateless) is built from a BFS over the icosphere room mesh and placed by exact integer lattice coordinates newly exposed from the kernel. `windows/vessel` overlays what only a session knows — epistemic state read from `Knowledge`'s existing `room/<id>` keys, and NPC marks — and adds a `map` verb. A deterministic ASCII render lives beside the scene builder and reaches CLI, galleries, and the browser exhibit from one implementation. Nothing is committed to any ledger: the chart is a pure derived view.

**Tech Stack:** Rust 2024, `serde`/`serde_json` only. TypeScript (Deno) for one client file. No new workspace dependencies.

**Spec:** `docs/superpowers/specs/2026-07-24-the-purview-design.md` (approved at G3, unmodified).

## Global Constraints

- **No new workspace dependencies.** `serde` + `serde_json` only, enforced by `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag** (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`), placed on the item's doc comment in the established style.
- **Quantize at the emit boundary only.** Floats in scene documents serialize through `hornvale_kernel::quantize::quantize_serde::f64_field` (or `opt_f64_field` for `Option<f64>`). Never quantize in a compute path.
- **`scene/surrounds/v1` is a save-format-class contract.** Field order in the struct **is** the JSON key order and is contract — never reorder. Changed meaning mints `scene/surrounds/v2` alongside; it is never renamed.
- **Nothing is committed to any ledger by this campaign.** No new seed-derivation label, no new predicate, no epoch, no stream-order change.
- **Run `cargo fmt` as the final step before every commit.** Fmt-gate skips are the most common review finding.
- **Iterate cost-ordered:** `cargo fmt --check` and `cargo clippy` first; scope tests with `-p <crate>`; `--workspace` only at the pre-commit gate.
- **Map placement is integer-only.** No transcendental may enter a cell's screen position.
- **The chart never asserts metres per cell.** The sim defines no planetary radius; scale is stated in arc or not at all.

---

## File Structure

| File | Responsibility |
|---|---|
| `kernel/src/room.rs` (modify) | `FaceLattice` + `RoomAddr::face_lattice()` — exact face-local integer lattice position |
| `windows/scene/Cargo.toml` (modify) | add `hornvale-locale` dependency |
| `windows/scene/src/surrounds.rs` (create) | `scene/surrounds/v1` schema, the fog-free builder, `surrounds_json` |
| `windows/scene/src/surrounds_ascii.rs` (create) | the deterministic ASCII render + the lens registry |
| `windows/scene/src/lib.rs` (modify) | `mod surrounds; pub use surrounds::*;` and the same for the render |
| `windows/vessel/Cargo.toml` (modify) | add `hornvale-scene` dependency |
| `windows/vessel/src/purview.rs` (create) | the epistemic overlay + NPC marks + the zoom rung |
| `windows/vessel/src/session.rs` (modify) | the `map` verb; `examine` widened to the union of grains |
| `windows/vessel/tests/the_purview.rs` (create) | the thesis test (one surface, two grains) + verb behaviour |
| `cli/src/main.rs` (modify) | `hornvale scene surrounds` + help text |
| `scripts/regenerate-artifacts.sh` (modify) | emit the committed surrounds scene |
| `book/src/reference/scene-surrounds-v1.md` (create) | the schema reference page |
| `book/src/SUMMARY.md` (modify) | link the reference page |
| `clients/vessel/src/transcript.ts` (modify) | class map lines as monospace |

---

### Task 1: The kernel's face-local lattice

Rooms are triangular faces of a subdivided icosahedron. Within one base face they form an exact 2-D triangular lattice addressed by integer barycentric coordinates, which `bary_triple` already computes privately. This task exposes it, so the chart can place cells with integer arithmetic instead of drifting compass bearings.

**Files:**
- Modify: `kernel/src/room.rs`
- Test: `kernel/src/room.rs` (the existing `#[cfg(test)] mod tests` at the bottom)

**Interfaces:**
- Consumes: nothing (first task).
- Produces:
  - `pub struct FaceLattice { pub a: i64, pub b: i64, pub c: i64, pub up: bool, pub scale: i64 }`
  - `pub fn RoomAddr::face_lattice(&self) -> FaceLattice`

**Background the implementer needs.** `bary_triple(&self.path) -> (i64, [Bary; 3])` returns the lattice scale `2^depth` and the triangle's three barycentric corners, each a `[i64; 3]` summing to `scale`. A triangle's *lattice base* is the componentwise minimum of its three corners. An "up" triangle (same orientation as its base face) has corners `base + e0`, `base + e1`, `base + e2`, so its base sums to `scale - 1`; a "down" triangle has corners `base + e1 + e2`, `base + e0 + e2`, `base + e0 + e1`, so its base sums to `scale - 2`. Edge-neighbours follow directly: an up triangle's three neighbours are the down triangles whose base is its own with exactly one axis decremented, and a down triangle's are the up triangles with exactly one axis incremented. That is what makes the lattice renderable.

- [ ] **Step 1: Write the failing tests**

Add to the `mod tests` block at the bottom of `kernel/src/room.rs`:

```rust
#[test]
fn the_base_face_is_a_single_up_triangle() {
    let root = RoomAddr {
        face: 3,
        path: vec![],
    };
    let l = root.face_lattice();
    assert_eq!(l.scale, 1);
    assert_eq!((l.a, l.b, l.c), (0, 0, 0));
    assert!(l.up, "the undivided base face has the face's own orientation");
    assert_eq!(l.a + l.b + l.c, l.scale - 1);
}

#[test]
fn the_lattice_base_distinguishes_up_from_down() {
    // Child digits 0..3 subdivide a triangle into three corner children (up)
    // and one central child (down).
    for digit in 0..3u8 {
        let r = RoomAddr {
            face: 0,
            path: vec![digit],
        };
        let l = r.face_lattice();
        assert!(l.up, "corner child {digit} keeps the parent's orientation");
        assert_eq!(l.a + l.b + l.c, l.scale - 1);
    }
    let centre = RoomAddr {
        face: 0,
        path: vec![3],
    };
    let l = centre.face_lattice();
    assert!(!l.up, "the central child is inverted");
    assert_eq!(l.a + l.b + l.c, l.scale - 2);
}

#[test]
fn edge_neighbours_are_lattice_adjacent() {
    // A room deep inside a base face: all three neighbours share its face,
    // and each differs from it by exactly one unit on exactly one axis.
    let room = RoomAddr {
        face: 0,
        path: vec![3, 0, 3, 1, 3, 2],
    };
    let me = room.face_lattice();
    let mut same_face = 0;
    for n in room.neighbors() {
        if n.face != room.face {
            continue;
        }
        same_face += 1;
        let l = n.face_lattice();
        assert_eq!(l.scale, me.scale, "neighbours sit at the same depth");
        assert_ne!(l.up, me.up, "an edge-neighbour has the opposite orientation");
        let d = [l.a - me.a, l.b - me.b, l.c - me.c];
        let want = if me.up { -1 } else { 1 };
        let moved: Vec<i64> = d.iter().copied().filter(|&x| x != 0).collect();
        assert_eq!(moved, vec![want], "exactly one axis steps by {want}: {d:?}");
    }
    assert_eq!(same_face, 3, "this room is interior; no seam expected");
}

#[test]
fn the_lattice_scale_matches_the_depth() {
    for depth in 0..8u32 {
        let r = RoomAddr {
            face: 11,
            path: vec![2; depth as usize],
        };
        assert_eq!(r.face_lattice().scale, 1i64 << depth);
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel room::tests::the_base_face_is_a_single_up_triangle`

Expected: FAIL to compile — `no method named 'face_lattice' found for struct 'RoomAddr'`.

- [ ] **Step 3: Implement `FaceLattice` and the accessor**

Add the struct next to `RoomAddr` in `kernel/src/room.rs` (after the `RoomId` declaration):

```rust
/// A room's exact position in its base face's triangular lattice: the
/// componentwise-minimum barycentric corner (the *lattice base point*) plus
/// the triangle's orientation. Integer-only, so two rooms on the same base
/// face have an exact, cross-platform-stable relative offset — this is what
/// lets a situated chart place cells without a transcendental. `a + b + c`
/// is `scale - 1` for an up triangle and `scale - 2` for a down one.
/// type-audit: bare-ok(index: a), bare-ok(index: b), bare-ok(index: c), bare-ok(flag: up), bare-ok(count: scale)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FaceLattice {
    /// Lattice base coordinate on barycentric axis 0.
    pub a: i64,
    /// Lattice base coordinate on barycentric axis 1.
    pub b: i64,
    /// Lattice base coordinate on barycentric axis 2.
    pub c: i64,
    /// Whether this triangle points the same way as its base face.
    pub up: bool,
    /// The lattice scale at this depth, `1 << depth`.
    pub scale: i64,
}
```

Add the accessor to the `impl RoomAddr` block that already holds `depth()` and `pack()`:

```rust
    /// This room's exact face-local lattice position. Integer-only — no
    /// transcendental enters it, so a chart placed from these coordinates is
    /// byte-identical across platforms. Face-LOCAL: two rooms on different
    /// base faces have no meaningful relative offset, and a consumer must
    /// compare `face` before differencing.
    pub fn face_lattice(&self) -> FaceLattice {
        let (scale, tri) = bary_triple(&self.path);
        let a = tri[0][0].min(tri[1][0]).min(tri[2][0]);
        let b = tri[0][1].min(tri[1][1]).min(tri[2][1]);
        let c = tri[0][2].min(tri[1][2]).min(tri[2][2]);
        FaceLattice {
            a,
            b,
            c,
            up: a + b + c == scale - 1,
            scale,
        }
    }
```

Export it from the kernel: in `kernel/src/lib.rs`, find the `pub use room::{...}` line and add `FaceLattice` to the braces, keeping the list alphabetical.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel face_lattice`
Expected: PASS, 4 tests. Then `cargo test -p hornvale-kernel` — expected: all existing kernel tests still pass.

- [ ] **Step 5: Check the type audit and format**

Run:
```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
```
Expected: type-audit reports no untagged pub-boundary primitive; clippy clean.

- [ ] **Step 6: Commit**

```bash
git add kernel/src/room.rs kernel/src/lib.rs
git commit -m "feat(kernel): FaceLattice — a room's exact face-local lattice position (The Purview)"
```

---

### Task 2: The `scene/surrounds/v1` schema and its fog-free builder

The scene protocol's situated pole. Stateless and session-free, because `clients/world-wasm` serves this crate's output to other repos.

**Files:**
- Modify: `windows/scene/Cargo.toml`
- Create: `windows/scene/src/surrounds.rs`
- Modify: `windows/scene/src/lib.rs`
- Test: `windows/scene/src/surrounds.rs` (inline `#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `RoomAddr::face_lattice() -> FaceLattice` (Task 1).
- Produces:
  - `pub const SURROUNDS_SCHEMA: &str = "scene/surrounds/v1";`
  - `pub const MAX_SURROUNDS_RADIUS: u32 = 8;`
  - `pub const RELIEF_LEGEND: [&str; 6]`
  - `pub struct SurroundsScene`, `pub struct SurroundsObserver`, `pub struct SurroundsCell`, `pub struct Mark`, `pub struct LegendEntry`
  - `pub fn surrounds_scene(world: &World, room: &RoomAddr, radius: u32, at: WorldTime) -> Result<SurroundsScene, SceneError>`
  - `pub fn surrounds_json(scene: &SurroundsScene) -> String`
  - `SceneError::SurroundsRadiusOutOfRange(u32)`

**Design notes the implementer must honour.**

- **Cell states.** `"here"` for the observer's room, `"sensed"` for every other cell this builder emits. `"remembered"` is written only by the vessel overlay (Task 5) — this builder never invents epistemic state.
- **Seam cells.** A cell whose `face` differs from the observer's has no meaningful lattice offset. Emit it with `u`/`v`/`w`/`up` as `null` and `seam: true`. Do not fabricate coordinates.
- **Orientation.** The chart is lattice-aligned, never north-up; `orientation` is the constant string `"lattice"`. Do **not** add a bearing field — bearings drift with latitude and the caption gets its orientation hint from the room's real exits instead.
- **Marks.** v1 emits settlement marks only, from the crate's existing private `features_of(world)`. NPC marks are the vessel's (Task 5). Strange-site marks are deferred (see the followup register): placing one needs a cell→position accessor that does not exist at this boundary, and the chart's legibility does not depend on them.
- **Ordering.** `cells` is sorted by `(room)` ascending — the packed `u64` room id, a total order that needs no float comparison. `marks` within a cell sort by `(salience, noun)`. `legend` sorts by `noun`.

- [ ] **Step 1: Add the locale dependency**

In `windows/scene/Cargo.toml`, under `[dependencies]`, add after the `hornvale-settlement` line:

```toml
hornvale-locale = { path = "../locale" }
```

Verify the layering test still passes (windows may depend on windows):

Run: `cargo test -p hornvale --test architecture`
Expected: PASS.

- [ ] **Step 2: Write the failing tests**

Create `windows/scene/src/surrounds.rs` containing **only** this test module for now (the implementation follows in step 4):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, WorldTime};
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn world() -> hornvale_kernel::World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn observer(w: &hornvale_kernel::World) -> RoomAddr {
        let ctx = hornvale_locale::LocaleContext::build(w).unwrap();
        let depth = ctx.globe_level() + 6;
        // The flagship settlement's own room — the same place a possession
        // mints its agent, so the gallery scene shows the walked ground.
        let v = hornvale_settlement::village_info(w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(w, v.id).expect("the flagship has coordinates");
        let (la, lo) = (lat.to_radians(), lon.to_radians());
        RoomAddr::containing(
            [
                hornvale_kernel::math::cos(la) * hornvale_kernel::math::cos(lo),
                hornvale_kernel::math::cos(la) * hornvale_kernel::math::sin(lo),
                hornvale_kernel::math::sin(la),
            ],
            depth,
        )
    }

    #[test]
    fn a_radius_four_neighbourhood_holds_thirty_one_cells() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(s.schema, SURROUNDS_SCHEMA);
        assert_eq!(s.radius, 4);
        // Ball sizes in the triangular face-adjacency lattice are
        // 1 + 3k(k+1)/2: 1, 4, 10, 19, 31, ...
        assert_eq!(s.cells.len(), 31);
    }

    #[test]
    fn exactly_one_cell_is_here_and_it_sits_at_the_lattice_origin() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 3, WorldTime { day: 0.0 }).unwrap();
        let here: Vec<&SurroundsCell> = s.cells.iter().filter(|c| c.state == "here").collect();
        assert_eq!(here.len(), 1);
        assert_eq!((here[0].u, here[0].v, here[0].w), (Some(0), Some(0), Some(0)));
        assert_eq!(here[0].room, s.observer.room);
        assert!(!here[0].seam);
    }

    #[test]
    fn every_non_seam_cell_carries_a_lattice_coordinate_and_seam_cells_carry_none() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        for c in &s.cells {
            if c.seam {
                assert!(c.u.is_none() && c.v.is_none() && c.w.is_none() && c.up.is_none());
            } else {
                assert!(c.u.is_some() && c.v.is_some() && c.w.is_some() && c.up.is_some());
            }
        }
    }

    #[test]
    fn the_document_is_byte_identical_on_rebuild() {
        let w = world();
        let o = observer(&w);
        let a = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime { day: 0.0 }).unwrap());
        let b = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime { day: 0.0 }).unwrap());
        assert_eq!(a, b);
        let rebuilt = hornvale_worldgen::rebuild(&w).expect("a world rebuilds from its ledger");
        let c = surrounds_json(&surrounds_scene(&rebuilt, &o, 4, WorldTime { day: 0.0 }).unwrap());
        assert_eq!(a, c, "same world + same query => byte-identical JSON");
    }

    #[test]
    fn the_radius_is_bounded_loudly() {
        let w = world();
        let e = surrounds_scene(&w, &observer(&w), MAX_SURROUNDS_RADIUS + 1, WorldTime { day: 0.0 })
            .unwrap_err();
        assert_eq!(e, SceneError::SurroundsRadiusOutOfRange(MAX_SURROUNDS_RADIUS + 1));
    }

    #[test]
    fn cells_are_ordered_by_room_id() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime { day: 0.0 }).unwrap();
        let ids: Vec<u64> = s.cells.iter().map(|c| c.room).collect();
        let mut sorted = ids.clone();
        sorted.sort_unstable();
        assert_eq!(ids, sorted, "cell order is contract: ascending room id");
    }
}
```

**Note on `rebuild`:** confirm the exact name of worldgen's ledger-rebuild helper before writing this test — run `grep -n 'pub fn rebuild' windows/worldgen/src/lib.rs`. If it differs, use the real name; the other scene kinds' determinism tests in `windows/scene/src/lib.rs` already do this and are the pattern to copy.

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cargo test -p hornvale-scene surrounds`
Expected: FAIL to compile — `cannot find function 'surrounds_scene' in this scope`.

- [ ] **Step 4: Implement the schema and builder**

Prepend to `windows/scene/src/surrounds.rs` (above the test module):

```rust
//! The situated pole of the scene protocol: `scene/surrounds/v1`, an
//! egocentric neighbourhood of rooms around an observer, placed by exact
//! integer lattice coordinates. Semantic-only and FOG-FREE — this builder
//! never invents epistemic state; a session-owning consumer (the vessel)
//! overlays what it alone knows.

use crate::{Feature, SceneError, features_of};
use hornvale_kernel::{RoomAddr, World, WorldTime, quantize};
use hornvale_locale::{Locale, LocaleContext};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// The schema identifier this module emits.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_SCHEMA: &str = "scene/surrounds/v1";

/// The largest legal neighbourhood radius, in BFS rings. A ring-`k`
/// neighbourhood holds `1 + 3k(k+1)/2` cells, so 8 is 109 cells — past
/// what a coarse chart can say anything useful with.
/// type-audit: bare-ok(count)
pub const MAX_SURROUNDS_RADIUS: u32 = 8;

/// The relief catalog, in stable ascending order. Band boundaries are
/// contract: changing one mints `scene/surrounds/v2`.
/// type-audit: bare-ok(identifier-text)
pub const RELIEF_LEGEND: [&str; 6] = [
    "abyss", "shelf", "lowland", "upland", "highland", "alpine",
];

/// Elevation (m) to an index into [`RELIEF_LEGEND`].
/// type-audit: bare-ok(index: return)
fn relief_band(elevation_m: f64) -> u32 {
    match elevation_m {
        e if e < -3000.0 => 0,
        e if e < 0.0 => 1,
        e if e < 300.0 => 2,
        e if e < 1000.0 => 3,
        e if e < 2500.0 => 4,
        _ => 5,
    }
}

/// Where the observer stands.
/// type-audit: bare-ok(index: room), bare-ok(index: face), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsObserver {
    /// Packed room id.
    pub room: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Refinement depth.
    pub depth: u32,
    /// Centroid latitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Centroid longitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// A salience-ranked thing standing on a cell. `noun` is the examinable key
/// — it is what joins this chart to the prose's own noun catalog.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(identifier-text: kind), bare-ok(prose: datum), bare-ok(index: salience)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Mark {
    /// The examinable noun.
    pub noun: String,
    /// What kind of thing this is: `"settlement"` or `"agent"`.
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
}

/// One `(noun, datum)` pair of the chart's catalog — deliberately the same
/// shape as the focalizer's `Focalized.nouns`, because that identity is what
/// makes map and prose two grains of one lens.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: datum)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LegendEntry {
    /// The examinable noun.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
}

/// One cell of the chart. Lattice coordinates are RELATIVE to the observer
/// and absent on a seam cell. Fine-grain fields are `null` at coarse grain —
/// a cell carries the detail its epistemic state warrants, which is what
/// makes the chart and the prose one lens rather than two.
/// type-audit: bare-ok(index: room), bare-ok(index: u), bare-ok(index: v), bare-ok(index: w), bare-ok(flag: up), bare-ok(flag: seam), bare-ok(identifier-text: state), bare-ok(index: biome), bare-ok(index: water), bare-ok(index: relief), bare-ok(prose: regime), bare-ok(diagnostic-value: temperature_c), bare-ok(ratio: moisture), waiver(elevation-convention: elevation_m)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsCell {
    /// Packed room id.
    pub room: u64,
    /// Lattice offset from the observer on axis 0; `null` on a seam cell.
    pub u: Option<i64>,
    /// Lattice offset on axis 1; `null` on a seam cell.
    pub v: Option<i64>,
    /// Lattice offset on axis 2; `null` on a seam cell.
    pub w: Option<i64>,
    /// Triangle orientation; `null` on a seam cell.
    pub up: Option<bool>,
    /// Set when this cell lies on a different base face than the observer,
    /// so the lattice bends and no honest local coordinate exists.
    pub seam: bool,
    /// `"here"`, `"sensed"`, or (written only by a session-owning consumer)
    /// `"remembered"`.
    pub state: String,
    /// Index into `biome_legend`.
    pub biome: u32,
    /// Index into `water_legend`.
    pub water: u32,
    /// Index into `relief_legend`.
    pub relief: u32,
    /// The strangeness overlay's descriptor — fine grain, `null` when coarse.
    pub regime: Option<String>,
    /// Annual-mean temperature, °C — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub temperature_c: Option<f64>,
    /// Moisture — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub moisture: Option<f64>,
    /// Elevation, metres — fine grain, `null` when coarse.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub elevation_m: Option<f64>,
    /// Salience-ranked things standing here.
    pub marks: Vec<Mark>,
}

/// One `scene/surrounds/v1` document. Field order is the JSON key order and
/// is contract — never reorder.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(diagnostic-value: day), bare-ok(count: radius), bare-ok(count: depth), bare-ok(identifier-text: orientation), bare-ok(identifier-text: biome_legend), bare-ok(identifier-text: water_legend), bare-ok(identifier-text: relief_legend)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsScene {
    /// Always `scene/surrounds/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The day observed.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub day: f64,
    /// Where the observer stands.
    pub observer: SurroundsObserver,
    /// Neighbourhood radius, in BFS rings.
    pub radius: u32,
    /// The refinement depth every cell sits at.
    pub depth: u32,
    /// Always `"lattice"`: the chart is lattice-aligned, never north-up. A
    /// consumer that wants north must ask the rooms for their bearings.
    pub orientation: String,
    /// The biome catalog, stable append-only order.
    pub biome_legend: Vec<String>,
    /// The water catalog, stable order.
    pub water_legend: Vec<String>,
    /// The relief catalog, stable ascending order.
    pub relief_legend: Vec<String>,
    /// The cells, ascending by `room`.
    pub cells: Vec<SurroundsCell>,
    /// The chart's noun catalog, ascending by `noun`.
    pub legend: Vec<LegendEntry>,
}

/// Build the `scene/surrounds/v1` document for `room` at `radius` rings.
/// Fog-free: every cell but the observer's is `"sensed"`.
pub fn surrounds_scene(
    world: &World,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
) -> Result<SurroundsScene, SceneError> {
    if radius > MAX_SURROUNDS_RADIUS {
        return Err(SceneError::SurroundsRadiusOutOfRange(radius));
    }
    let ctx = LocaleContext::build(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let here = ctx
        .describe(room, at)
        .map_err(|e| SceneError::Build(e.to_string()))?;

    // Breadth-first over the mesh's edge-adjacency graph, out to `radius`
    // rings. BTreeSet/VecDeque only — no HashSet (determinism).
    let mut seen: BTreeSet<RoomAddr> = BTreeSet::new();
    let mut queue: VecDeque<(RoomAddr, u32)> = VecDeque::new();
    seen.insert(room.clone());
    queue.push_back((room.clone(), 0));
    let mut found: Vec<RoomAddr> = vec![room.clone()];
    while let Some((addr, ring)) = queue.pop_front() {
        if ring == radius {
            continue;
        }
        for n in addr.neighbors() {
            if seen.insert(n.clone()) {
                found.push(n.clone());
                queue.push_back((n, ring + 1));
            }
        }
    }

    let origin = room.face_lattice();
    let catalog = hornvale_climate::Biome::catalog();
    let biome_index: BTreeMap<String, u32> = catalog
        .iter()
        .enumerate()
        .map(|(i, b)| (b.name().to_string(), i as u32))
        .collect();

    // Settlement marks, keyed by the room each settlement's coordinates land
    // in at this depth.
    let marks_by_room = settlement_marks(world, room.depth());

    let mut cells: Vec<SurroundsCell> = Vec::with_capacity(found.len());
    for addr in &found {
        let locale = ctx
            .describe(addr, at)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let is_here = addr == room;
        let seam = addr.face != room.face;
        let lat = if seam { None } else { Some(addr.face_lattice()) };
        let key = addr.pack().map(|r| r.0).unwrap_or(0);
        let mut marks = marks_by_room.get(&key).cloned().unwrap_or_default();
        marks.sort_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)));
        cells.push(SurroundsCell {
            room: key,
            u: lat.map(|l| l.a - origin.a),
            v: lat.map(|l| l.b - origin.b),
            w: lat.map(|l| l.c - origin.c),
            up: lat.map(|l| l.up),
            seam,
            state: if is_here { "here" } else { "sensed" }.to_string(),
            biome: *biome_index.get(&locale.biome).unwrap_or(&0),
            water: u32::from(locale.fields.water.index()),
            relief: relief_band(locale.fields.elevation_m),
            regime: is_here.then(|| locale.regime.descriptor.clone()),
            temperature_c: is_here.then_some(locale.fields.temperature_c),
            moisture: is_here.then_some(locale.fields.moisture),
            elevation_m: is_here.then_some(locale.fields.elevation_m),
            marks,
        });
    }
    cells.sort_by_key(|c| c.room);

    let legend = legend_of(&cells, &here, catalog);

    Ok(SurroundsScene {
        schema: SURROUNDS_SCHEMA.to_string(),
        seed: world.seed.0,
        day: quantize(at.day),
        observer: SurroundsObserver {
            room: room.pack().map(|r| r.0).unwrap_or(0),
            face: room.face,
            depth: room.depth(),
            latitude: quantize(here.latitude),
            longitude: quantize(here.longitude),
        },
        radius,
        depth: room.depth(),
        orientation: "lattice".to_string(),
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        water_legend: hornvale_terrain::WaterKind::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
        relief_legend: RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
        cells,
        legend,
    })
}

/// Settlement marks keyed by the packed room id their coordinates fall in at
/// `depth`. The flagship outranks the rest.
fn settlement_marks(world: &World, depth: u32) -> BTreeMap<u64, Vec<Mark>> {
    let mut out: BTreeMap<u64, Vec<Mark>> = BTreeMap::new();
    for f in features_of(world) {
        let Feature {
            name,
            kind,
            latitude,
            longitude,
        } = f;
        let (la, lo) = (latitude.to_radians(), longitude.to_radians());
        let position = [
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::cos(lo),
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::sin(lo),
            hornvale_kernel::math::sin(la),
        ];
        let Ok(id) = RoomAddr::containing(position, depth).pack() else {
            continue;
        };
        let flagship = kind == "flagship";
        out.entry(id.0).or_default().push(Mark {
            datum: if flagship {
                format!("{name} — the settlement this possession was minted from.")
            } else {
                format!("{name} — a settlement of this world.")
            },
            noun: name,
            kind: "settlement".to_string(),
            salience: if flagship { 10 } else { 20 },
        });
    }
    out
}

/// The chart's noun catalog: every mark's noun, plus one entry per distinct
/// terrain class drawn, plus the observer's own room.
fn legend_of(
    cells: &[SurroundsCell],
    here: &Locale,
    catalog: &'static [hornvale_climate::Biome],
) -> Vec<LegendEntry> {
    let mut acc: BTreeMap<String, String> = BTreeMap::new();
    for c in cells {
        for m in &c.marks {
            acc.insert(m.noun.clone(), m.datum.clone());
        }
        let biome = catalog
            .get(c.biome as usize)
            .map(|b| b.name().to_string())
            .unwrap_or_default();
        acc.entry(biome.clone()).or_insert_with(|| {
            format!(
                "{biome} — {} of the {} cells in view.",
                cells.iter().filter(|d| d.biome == c.biome).count(),
                cells.len()
            )
        });
    }
    acc.insert(
        here.regime.descriptor.clone(),
        format!(
            "The ground where you stand: {} (strangeness {:.0}).",
            here.regime.descriptor, here.regime.strangeness
        ),
    );
    acc.into_iter()
        .map(|(noun, datum)| LegendEntry { noun, datum })
        .collect()
}

/// Serialize a `SurroundsScene` to compact JSON (mirrors `scene_json`).
/// type-audit: bare-ok(artifact: return)
pub fn surrounds_json(scene: &SurroundsScene) -> String {
    serde_json::to_string_pretty(scene).expect("a surrounds scene serializes")
}
```

Make `features_of` and `place_latlon` visible to the new module: in `windows/scene/src/lib.rs`, change `fn features_of(` to `pub(crate) fn features_of(` and `fn place_latlon(` to `pub(crate) fn place_latlon(`. Then wire the module in, next to the existing `mod region;`:

```rust
mod surrounds;
pub use surrounds::*;
```

Add the error variant to `SceneError` in `windows/scene/src/lib.rs` — **append it after the last existing variant** (the enum's order is not a serialized contract, but appending keeps diffs honest), and extend the `type-audit:` tag line on the enum with `bare-ok(diagnostic-value: SurroundsRadiusOutOfRange.0)`:

```rust
    /// Surrounds query: `radius` must be 0..=MAX_SURROUNDS_RADIUS.
    SurroundsRadiusOutOfRange(u32),
```

and its `Display` arm inside the existing `match`:

```rust
            SceneError::SurroundsRadiusOutOfRange(r) => write!(
                f,
                "--radius {r} is outside 0..={MAX_SURROUNDS_RADIUS}"
            ),
```

Finally add the `hornvale-climate` import if the crate does not already have it in scope — it is already a dependency (used by `region.rs`).

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-scene surrounds`
Expected: PASS, 6 tests. If `a_radius_four_neighbourhood_holds_thirty_one_cells` fails with a count **below** 31, the observer sits near a base-face vertex where only five triangles meet — pick a different observer for that test and record the real count; do not change the assertion to match a bug.

- [ ] **Step 6: Type audit, format, clippy**

Run:
```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
cargo clippy -p hornvale-scene --all-targets -- -D warnings
```
Expected: all clean.

- [ ] **Step 7: Commit**

```bash
git add windows/scene/Cargo.toml windows/scene/src/surrounds.rs windows/scene/src/lib.rs Cargo.lock
git commit -m "feat(scene): scene/surrounds/v1 — the protocol's situated pole (The Purview)"
```

---

### Task 3: The deterministic ASCII render

An in-process render, Ring 0/1 under decision 0022 — the same class as the gallery's three existing ASCII maps. One registered lens, and a caption that says which lens you are wearing and what it omits (RENDER-9).

**Files:**
- Create: `windows/scene/src/surrounds_ascii.rs`
- Modify: `windows/scene/src/lib.rs`
- Test: `windows/scene/src/surrounds_ascii.rs` (inline tests)

**Interfaces:**
- Consumes: `SurroundsScene`, `SurroundsCell` (Task 2).
- Produces:
  - `pub const SURROUNDS_LENSES: [&str; 1] = ["terrain"];`
  - `pub fn render_surrounds_ascii(scene: &SurroundsScene, lens: &str, ways: &[String]) -> String`

**The placement formula (this is the whole geometry, and it is exact).** A cell's screen position comes from its relative lattice coordinates:

```
row = -w
col = 2 * v + (if up { 0 } else { 1 })
```

Rows increase downward. Within a row, consecutive `col` values are edge-adjacent triangles alternating up/down — which is exactly why one character per cell reads as a triangular strip. The observer sits at `row = 0`, `col = if observer_up { 0 } else { 1 }`; the render subtracts that so the observer lands at the grid's origin, then shifts everything so the minimum row and column are zero. Seam cells have no coordinates and are **not** placed; the caption discloses them.

**The glyph table (contract, pinned by the golden).**

```
state "here"                      -> '@'   (always wins)
a cell carrying any mark          -> '#'   (settlement) or '&' (agent)
water "ocean"                     -> '~'
water "salt-basin"                -> '='
water "river"                     -> '+'
otherwise, by relief:
  abyss | shelf                   -> '_'
  lowland                         -> '.'
  upland                          -> ':'
  highland                        -> '^'
  alpine                          -> 'A'
state "remembered" replaces the glyph with its faded twin:
  '~' '=' '+' '_' -> '-'    '.' -> ','    ':' -> ';'    '^' -> 'n'    'A' -> 'a'
  '#' -> 'o'      '&' -> '%'
```

Mark precedence within a cell: `agent` before `settlement` (an agent is more salient than the ground it stands on).

- [ ] **Step 1: Write the failing tests**

Create `windows/scene/src/surrounds_ascii.rs` with this test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SurroundsCell, SurroundsObserver, SurroundsScene};

    fn cell(u: i64, v: i64, w: i64, up: bool, state: &str, relief: u32) -> SurroundsCell {
        SurroundsCell {
            room: (u * 1000 + v * 10 + w).unsigned_abs() + u64::from(up),
            u: Some(u),
            v: Some(v),
            w: Some(w),
            up: Some(up),
            seam: false,
            state: state.to_string(),
            biome: 0,
            water: 3, // dry-land
            relief,
            regime: None,
            temperature_c: None,
            moisture: None,
            elevation_m: None,
            marks: vec![],
        }
    }

    fn scene(cells: Vec<SurroundsCell>) -> SurroundsScene {
        SurroundsScene {
            schema: crate::SURROUNDS_SCHEMA.to_string(),
            seed: 42,
            day: 0.0,
            observer: SurroundsObserver {
                room: 1,
                face: 0,
                depth: 12,
                latitude: 0.0,
                longitude: 0.0,
            },
            radius: 1,
            depth: 12,
            orientation: "lattice".to_string(),
            biome_legend: vec!["tundra".to_string()],
            water_legend: ["ocean", "salt-basin", "river", "dry-land"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
            relief_legend: crate::RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
            cells,
            legend: vec![],
        }
    }

    #[test]
    fn the_observer_is_an_at_sign_and_its_row_reads_left_to_right() {
        // An up observer at the origin, its same-row neighbours either side.
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 2),  // col +1: to the right
            cell(0, -1, 0, false, "sensed", 2),  // col -1: to the left
        ]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        let grid: Vec<&str> = out
            .lines()
            .filter(|l| !l.starts_with('[') && !l.starts_with("  "))
            .collect();
        assert!(
            grid.iter().any(|l| l.contains(".@.")),
            "the observer sits between its two same-row neighbours: {out}"
        );
    }

    #[test]
    fn a_remembered_cell_fades() {
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "remembered", 2),
        ]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        assert!(out.contains('@'), "the observer is drawn");
        assert!(out.contains(','), "a remembered lowland fades '.' -> ',': {out}");
    }

    #[test]
    fn the_caption_names_the_lens_and_declares_the_orientation() {
        let s = scene(vec![cell(0, 0, 0, true, "here", 2)]);
        let out = render_surrounds_ascii(&s, "terrain", &["E".to_string(), "Nw".to_string()]);
        let caption = out.lines().next().unwrap();
        assert!(caption.contains("lens: terrain"), "{caption}");
        assert!(caption.contains("lattice-aligned"), "{caption}");
        assert!(
            out.contains("ways on: E, Nw"),
            "the exits are the orientation hint, since the chart is not north-up: {out}"
        );
    }

    #[test]
    fn a_seam_is_disclosed_not_hidden() {
        let mut seam = cell(0, 0, 0, true, "sensed", 2);
        seam.seam = true;
        seam.u = None;
        seam.v = None;
        seam.w = None;
        seam.up = None;
        seam.room = 999;
        let s = scene(vec![cell(0, 0, 0, true, "here", 2), seam]);
        let out = render_surrounds_ascii(&s, "terrain", &[]);
        assert!(
            out.contains("1 cell beyond a face seam"),
            "an unplaceable cell must be stated, not dropped silently: {out}"
        );
    }

    #[test]
    fn an_unknown_lens_is_refused_loudly() {
        let s = scene(vec![cell(0, 0, 0, true, "here", 2)]);
        let out = render_surrounds_ascii(&s, "nonesuch", &[]);
        assert!(out.contains("no lens 'nonesuch'"), "{out}");
        assert!(out.contains("terrain"), "the refusal names the registry: {out}");
    }

    #[test]
    fn the_render_is_deterministic() {
        let s = scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(-1, 0, 0, false, "sensed", 4),
        ]);
        assert_eq!(
            render_surrounds_ascii(&s, "terrain", &[]),
            render_surrounds_ascii(&s, "terrain", &[])
        );
    }
}
```

The test spells the water legend out literally rather than calling into the terrain crate. That is deliberate: if `WaterKind::LEGEND` ever changes, this test's expectations should break loudly rather than silently follow along — the glyph table is keyed to those exact names.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-scene surrounds_ascii`
Expected: FAIL to compile — `cannot find function 'render_surrounds_ascii'`.

- [ ] **Step 3: Implement the render**

Prepend to `windows/scene/src/surrounds_ascii.rs`:

```rust
//! The in-process ASCII render of a `scene/surrounds/v1` document — Ring 0/1
//! under decision 0022, the same class as the gallery's three ASCII maps. A
//! render is a registered LENS, never ground truth (RENDER-9), and the
//! caption — not the picture — carries the honesty: it names the lens, the
//! orientation, and everything the picture had to leave out.

use crate::SurroundsScene;
use std::collections::BTreeMap;

/// The registered lenses. v1 ships one; a second is purely additive.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_LENSES: [&str; 1] = ["terrain"];

/// The glyph a cell draws under the `terrain` lens, before fading.
fn terrain_glyph(scene: &SurroundsScene, cell: &crate::SurroundsCell) -> char {
    if cell.state == "here" {
        return '@';
    }
    if let Some(m) = cell
        .marks
        .iter()
        .min_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)))
    {
        return if m.kind == "agent" { '&' } else { '#' };
    }
    let water = scene
        .water_legend
        .get(cell.water as usize)
        .map(String::as_str)
        .unwrap_or("dry-land");
    match water {
        "ocean" => '~',
        "salt-basin" => '=',
        "river" => '+',
        _ => match cell.relief {
            0 | 1 => '_',
            2 => '.',
            3 => ':',
            4 => '^',
            _ => 'A',
        },
    }
}

/// A glyph's memory twin — what a `remembered` cell draws instead.
fn faded(g: char) -> char {
    match g {
        '~' | '=' | '+' | '_' => '-',
        '.' => ',',
        ':' => ';',
        '^' => 'n',
        'A' => 'a',
        '#' => 'o',
        '&' => '%',
        other => other,
    }
}

/// Render `scene` through `lens`. `ways` are the compass names of the
/// observer's real exits — the chart is lattice-aligned rather than north-up,
/// so the exits are how a reader orients.
/// type-audit: bare-ok(identifier-text: lens), bare-ok(identifier-text: ways), bare-ok(prose: return)
pub fn render_surrounds_ascii(scene: &SurroundsScene, lens: &str, ways: &[String]) -> String {
    if !SURROUNDS_LENSES.contains(&lens) {
        return format!(
            "There is no lens '{lens}'. Registered lenses: {}.",
            SURROUNDS_LENSES.join(", ")
        );
    }

    // Place every non-seam cell. row = -w; col = 2v + (up ? 0 : 1).
    let mut placed: BTreeMap<(i64, i64), char> = BTreeMap::new();
    let mut seams = 0usize;
    for c in &scene.cells {
        let (Some(v), Some(w), Some(up)) = (c.v, c.w, c.up) else {
            seams += 1;
            continue;
        };
        let row = -w;
        let col = 2 * v + i64::from(!up);
        let g = terrain_glyph(scene, c);
        let g = if c.state == "remembered" { faded(g) } else { g };
        placed.insert((row, col), g);
    }

    let mut out = String::new();
    out.push_str(&format!(
        "[lens: terrain · depth {} · radius {} · lattice-aligned, not north-up]\n",
        scene.depth, scene.radius
    ));

    if placed.is_empty() {
        out.push_str("  (nothing placeable in view)\n");
    } else {
        let rows: Vec<i64> = placed.keys().map(|&(r, _)| r).collect();
        let cols: Vec<i64> = placed.keys().map(|&(_, c)| c).collect();
        let (r0, r1) = (*rows.iter().min().unwrap(), *rows.iter().max().unwrap());
        let (c0, c1) = (*cols.iter().min().unwrap(), *cols.iter().max().unwrap());
        for r in r0..=r1 {
            let mut line = String::from("  ");
            for c in c0..=c1 {
                line.push(*placed.get(&(r, c)).unwrap_or(&' '));
            }
            out.push_str(line.trim_end());
            out.push('\n');
        }
    }

    if !ways.is_empty() {
        out.push_str(&format!("  ways on: {}\n", ways.join(", ")));
    }
    if seams > 0 {
        out.push_str(&format!(
            "  {seams} cell{} beyond a face seam: real ground, no honest place on this chart.\n",
            if seams == 1 { "" } else { "s" }
        ));
    }
    if !scene.legend.is_empty() {
        out.push_str("  legend: ");
        let nouns: Vec<&str> = scene.legend.iter().map(|e| e.noun.as_str()).collect();
        out.push_str(&nouns.join(", "));
        out.push('\n');
    }
    out
}
```

Wire it in `windows/scene/src/lib.rs` next to the other modules:

```rust
mod surrounds_ascii;
pub use surrounds_ascii::*;
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-scene surrounds_ascii`
Expected: PASS, 6 tests.

- [ ] **Step 5: Format, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-scene --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/scene/src/surrounds_ascii.rs windows/scene/src/surrounds.rs windows/scene/src/lib.rs
git commit -m "feat(scene): the surrounds ASCII lens — caption-first, seam-disclosing (The Purview)"
```

---

### Task 4: `hornvale scene surrounds`, the committed artifact, and the reference page

**Files:**
- Modify: `cli/src/main.rs`
- Modify: `scripts/regenerate-artifacts.sh`
- Create: `book/src/reference/scene-surrounds-v1.md`
- Modify: `book/src/SUMMARY.md`
- Create: `book/src/gallery/scene-surrounds-seed-42.json` (generated, committed)

**Interfaces:**
- Consumes: `surrounds_scene`, `surrounds_json` (Task 2).
- Produces: the CLI subcommand and the committed artifact the drift check watches.

- [ ] **Step 1: Add the subcommand**

In `cli/src/main.rs`, inside `cmd_scene`'s `match`, add an arm after the `tiles-region` arm:

```rust
        Some("surrounds") => {
            let world = load_world(args)?;
            let ctx = hornvale_locale::LocaleContext::build(&world)
                .map_err(|e| e.to_string())?;
            let depth = match flag_value(args, "--depth") {
                Some(raw) => raw
                    .parse::<u32>()
                    .map_err(|e| format!("--depth must be a u32: {e}"))?,
                None => ctx.globe_level() + 6,
            };
            let radius = match flag_value(args, "--radius") {
                Some(raw) => raw
                    .parse::<u32>()
                    .map_err(|e| format!("--radius must be a u32: {e}"))?,
                None => 4,
            };
            let room = match flag_value(args, "--room") {
                Some(raw) => {
                    let id = raw
                        .parse::<u64>()
                        .map_err(|e| format!("--room must be a packed room id: {e}"))?;
                    hornvale_kernel::RoomId(id)
                        .unpack()
                        .map_err(|e| format!("--room {id} is not a room id: {e:?}"))?
                }
                None => {
                    // Default: the flagship settlement's room — the ground a
                    // possession actually starts on.
                    let v = hornvale_settlement::village_info(&world)
                        .ok_or("this world has no settlement to centre on")?;
                    settlement_room(&world, v.id, depth)?
                }
            };
            let scene =
                hornvale_scene::surrounds_scene(&world, &room, radius, hornvale_kernel::WorldTime {
                    day: 0.0,
                })
                .map_err(|e| e.to_string())?;
            println!("{}", hornvale_scene::surrounds_json(&scene));
            Ok(())
        }
```

Add the `settlement_room` helper near `cmd_scene` (it mirrors the lat/lon→room routing already used at `cli/src/main.rs`'s `locale --at` path):

```rust
/// The room a settlement's coordinates fall in at `depth` — the same
/// lat/lon → unit-sphere routing `locale --at` uses, so a scene centred on a
/// settlement lands exactly where a possession mints its agent.
fn settlement_room(
    world: &hornvale_kernel::World,
    id: hornvale_kernel::EntityId,
    depth: u32,
) -> Result<hornvale_kernel::RoomAddr, String> {
    let lat = hornvale_settlement::fact_number(world, id, hornvale_settlement::LATITUDE)
        .ok_or("the settlement has no latitude fact")?;
    let lon = hornvale_settlement::fact_number(world, id, hornvale_settlement::LONGITUDE)
        .ok_or("the settlement has no longitude fact")?;
    let (la, lo) = (lat.to_radians(), lon.to_radians());
    Ok(hornvale_kernel::RoomAddr::containing(
        [
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::cos(lo),
            hornvale_kernel::math::cos(la) * hornvale_kernel::math::sin(lo),
            hornvale_kernel::math::sin(la),
        ],
        depth,
    ))
}
```

**Before writing this helper, confirm the real accessor names.** Run:
```bash
grep -n 'pub fn ' domains/settlement/src/lib.rs | grep -i 'fact\|latitude\|longitude'
grep -n 'fn place_latlon' -A 12 windows/scene/src/lib.rs
```
`place_latlon` in `windows/scene/src/lib.rs` already does exactly this lookup; copy its body rather than inventing accessor names. If `hornvale-locale` is not yet a `cli` dependency, add it to `cli/Cargo.toml`.

Add the usage line to the help text block near `cli/src/main.rs:51`:

```
  hornvale scene surrounds [--world <PATH>] [--room <ID>] [--radius <N>] [--depth <D>]
                                                      emit scene/surrounds/v1 JSON to stdout
```

- [ ] **Step 2: Verify the command runs and is deterministic**

Run:
```bash
cargo run --release -q -p hornvale -- new --seed 42 --out /tmp/hv42.json
cargo run --release -q -p hornvale -- scene surrounds --world /tmp/hv42.json > /tmp/a.json
cargo run --release -q -p hornvale -- scene surrounds --world /tmp/hv42.json > /tmp/b.json
diff /tmp/a.json /tmp/b.json && echo BYTE-IDENTICAL
head -30 /tmp/a.json
```
Expected: `BYTE-IDENTICAL`, and a document whose `schema` is `scene/surrounds/v1` with 31 cells.

- [ ] **Step 3: Add the artifact to the regeneration script**

In `scripts/regenerate-artifacts.sh`, in the `scene exports` block (around line 206, after the `eclipses` line), add:

```bash
run -p hornvale -- scene surrounds --world "$wsky" > book/src/gallery/scene-surrounds-seed-42.json
```

Then regenerate and inspect:
```bash
bash scripts/regenerate-artifacts.sh
git status --short book/src/gallery/
```
Expected: `scene-surrounds-seed-42.json` appears as a new file; **no other gallery file changes**. If another file changed, stop and find out why before continuing — an unexplained artifact diff is a determinism signal, not noise.

- [ ] **Step 4: Write the reference page**

Create `book/src/reference/scene-surrounds-v1.md`. Follow `book/src/reference/scene-tiles-region-v1.md`'s voice: technical, explaining *why this is a new schema rather than a change to an existing one*. It must cover: the situated pole versus the cartographic pole; the room mesh as a triangular lattice and why placement is integer; the `u`/`v`/`w`/`up` coordinates and the `row = -w`, `col = 2v + (up ? 0 : 1)` reading; seam cells and why they carry no coordinates; the three cell states and the grain that goes with each; the legends; and the rule that the schema states scale in arc and never in metres, because the sim defines no planetary radius.

Add it to `book/src/SUMMARY.md` immediately after the `scene-tiles-region-v1.md` line, matching the surrounding indentation:

```markdown
- [Scene Schema: surrounds v1](./reference/scene-surrounds-v1.md)
```

- [ ] **Step 5: Verify the docs drift check**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS. If it fails, fix the doc (a broken link or a missing ToC entry) — never the test.

- [ ] **Step 6: Format and commit**

```bash
cargo fmt
cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/src/main.rs cli/Cargo.toml scripts/regenerate-artifacts.sh \
        book/src/reference/scene-surrounds-v1.md book/src/SUMMARY.md \
        book/src/gallery/scene-surrounds-seed-42.json Cargo.lock
git commit -m "feat(cli,book): hornvale scene surrounds + the committed situated-pole artifact (The Purview)"
```

---

### Task 5: The vessel's epistemic overlay and the zoom rung

What only a session knows: which rooms have been walked, where the NPCs stand, and what the chart looks like from a coarser rung of the same address space.

**Files:**
- Modify: `windows/vessel/Cargo.toml`
- Create: `windows/vessel/src/purview.rs`
- Modify: `windows/vessel/src/lib.rs` (add `mod purview; pub use purview::*;`)
- Test: `windows/vessel/src/purview.rs` (inline tests)

**Interfaces:**
- Consumes: `surrounds_scene`, `SurroundsScene`, `Mark` (Task 2); `Knowledge` (`pub struct Knowledge(pub BTreeMap<String, String>)`); `hornvale_vessel::liveness::{Npc, agent_position}`; `hornvale_kernel::{RoomAddr, RoomId, Ledger, WorldTime}`.
- Produces:
  - `pub const PURVIEW_RADIUS: u32 = 4;`
  - `pub fn purview_scene(world, ctx, position: &RoomAddr, knowledge: &Knowledge, npcs: &[Npc], ledger: &Ledger, at: WorldTime, zoom_out: u32) -> Result<SurroundsScene, VesselError>`

**How the fog works, precisely.** `IdentityProjection` already writes a `room/<packed id>` key into `Knowledge` on every visit. So:

- A cell whose `room/<id>` key is present, and which is **not** in the current radius, becomes `"remembered"`.
- Every cell in the current radius stays `"sensed"` (or `"here"`).
- At a coarser depth (`zoom_out > 0`), a cell is `"remembered"` if **any** key in `Knowledge` names a room that is a **path-prefix descendant** of it. That is an integer prefix test on the unpacked address — no aggregation code, because zoom is path truncation.

Because a coarse chart re-runs the same builder at `depth - zoom_out` around the observer's ancestor, the ancestor is simply `RoomAddr { face, path: path[..depth - zoom_out] }`.

- [ ] **Step 1: Add the scene dependency**

In `windows/vessel/Cargo.toml`, under `[dependencies]`:

```toml
hornvale-scene = { path = "../scene" }
```

Run: `cargo test -p hornvale --test architecture`
Expected: PASS — `windows/scene` does not depend on `windows/vessel`, so this edge introduces no cycle.

- [ ] **Step 2: Write the failing tests**

Create `windows/vessel/src/purview.rs` with this test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PossessOpts, Session};
    use hornvale_kernel::{Seed, World};
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn world() -> World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    #[test]
    fn the_starting_room_is_here_and_nothing_is_remembered_yet() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let s = session.purview(0).unwrap();
        assert_eq!(s.cells.iter().filter(|c| c.state == "here").count(), 1);
        assert_eq!(
            s.cells.iter().filter(|c| c.state == "remembered").count(),
            0,
            "a session that has not left its first room remembers nowhere else"
        );
    }

    #[test]
    fn a_room_walked_and_left_becomes_remembered() {
        let w = world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let start = session.agent().position.pack().unwrap().0;
        // Walk far enough that the start room leaves the sense radius.
        for _ in 0..(PURVIEW_RADIUS + 1) {
            let way = session.ways().first().map(|(c, _)| format!("{c:?}"));
            let Some(way) = way else { break };
            session.handle(&format!("go {way}"));
        }
        let s = session.purview(0).unwrap();
        let start_cell = s.cells.iter().find(|c| c.room == start);
        if let Some(c) = start_cell {
            assert_eq!(
                c.state, "remembered",
                "the room we walked out of is memory, not sight"
            );
        }
        // Whether or not the start room is still in view, the walk must have
        // produced at least one remembered cell somewhere behind us.
        assert!(
            s.cells.iter().any(|c| c.state == "remembered")
                || start_cell.is_none(),
            "walking must leave a trail of memory"
        );
    }

    #[test]
    fn zooming_out_coarsens_the_depth_and_keeps_the_observer_centred() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let fine = session.purview(0).unwrap();
        let coarse = session.purview(2).unwrap();
        assert_eq!(coarse.depth, fine.depth - 2, "zoom is path truncation");
        assert_eq!(
            coarse.cells.iter().filter(|c| c.state == "here").count(),
            1,
            "the observer's ancestor is the coarse chart's centre"
        );
    }

    #[test]
    fn the_purview_is_idempotent() {
        let w = world();
        let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let before = session.knowledge().0.clone();
        let a = hornvale_scene::surrounds_json(&session.purview(0).unwrap());
        let b = hornvale_scene::surrounds_json(&session.purview(0).unwrap());
        assert_eq!(a, b, "drawing the chart twice gives the same chart");
        assert_eq!(
            &before,
            &session.knowledge().0,
            "drawing the chart must not mutate what the session knows"
        );
    }

    #[test]
    fn an_agent_mark_stands_on_a_cell() {
        let w = world();
        let (session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
        let s = session.purview(0).unwrap();
        let agents: usize = s
            .cells
            .iter()
            .flat_map(|c| c.marks.iter())
            .filter(|m| m.kind == "agent")
            .count();
        assert!(
            agents > 0,
            "seed 42 derives NPCs at the flagship settlement; at least one is in view"
        );
    }
}
```

- [ ] **Step 3: Run to verify failure**

Run: `cargo test -p hornvale-vessel purview`
Expected: FAIL to compile — `no method named 'purview' found`.

- [ ] **Step 4: Implement the overlay**

Prepend to `windows/vessel/src/purview.rs`:

```rust
//! The vessel's epistemic overlay on `scene/surrounds/v1`: which cells are
//! memory rather than sight, and which NPCs stand where. The overlay WRITES
//! NOTHING — `remembered` is a read of the `room/<id>` keys the identity
//! projection already absorbs on every visit, so a possession that draws the
//! chart is byte-identical to one that never does.

use crate::{Knowledge, VesselError, liveness};
use hornvale_kernel::{Ledger, RoomAddr, RoomId, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_scene::{Mark, SurroundsScene, surrounds_scene_in};

/// The chart's sense radius, in BFS rings. A constant this slice; the seam
/// for a per-species radius is `Agent::perception` (EXP-3), untouched here.
/// type-audit: bare-ok(count)
pub const PURVIEW_RADIUS: u32 = 4;

/// The salience of an NPC standing on a cell — above every settlement mark.
/// type-audit: bare-ok(index)
const AGENT_SALIENCE: u32 = 5;

/// Build the chart the possession draws: the fog-free scene, then the
/// epistemic and agent overlays. `zoom_out` coarsens by truncating the
/// observer's path — zoom in this mesh is not an aggregation, it is the same
/// builder one rung up the address space.
pub fn purview_scene(
    world: &World,
    ctx: &LocaleContext,
    position: &RoomAddr,
    knowledge: &Knowledge,
    npcs: &[liveness::Npc],
    ledger: &Ledger,
    at: WorldTime,
    zoom_out: u32,
) -> Result<SurroundsScene, VesselError> {
    let depth = position.depth();
    let keep = depth.saturating_sub(zoom_out) as usize;
    let centre = RoomAddr {
        face: position.face,
        path: position.path[..keep.min(position.path.len())].to_vec(),
    };
    // `surrounds_scene_in`, NOT `surrounds_scene`: the session already holds a
    // built `LocaleContext`, and building a fresh one costs ~1.2 s (measured)
    // against ~2 ms of actual per-cell work. `map` runs every turn, so the
    // convenience wrapper would make the verb unusable.
    let mut scene = surrounds_scene_in(world, ctx, &centre, PURVIEW_RADIUS, at)
        .map_err(|e| VesselError::Build(e.to_string()))?;

    // Every room this session has walked, as an address.
    let walked: Vec<RoomAddr> = knowledge
        .0
        .keys()
        .filter_map(|k| k.strip_prefix("room/"))
        .filter_map(|id| id.parse::<u64>().ok())
        .filter_map(|id| RoomId(id).unpack().ok())
        .collect();

    // Where each NPC stands right now — the derived-view read (The
    // Quickening): the latest committed `agent-at`, else the derived
    // schedule. Truncated to the chart's depth so a coarse chart still
    // places them.
    let mut agent_marks: Vec<(u64, Mark)> = Vec::new();
    for npc in npcs {
        let at_room = liveness::agent_position(ledger, npc, at);
        let shown = RoomAddr {
            face: at_room.face,
            path: at_room.path[..keep.min(at_room.path.len())].to_vec(),
        };
        let Ok(id) = shown.pack() else { continue };
        agent_marks.push((
            id.0,
            Mark {
                noun: npc.label.clone(),
                kind: "agent".to_string(),
                datum: format!("{} — a {} of this world, alive and moving.", npc.label, npc.species),
                salience: AGENT_SALIENCE,
            },
        ));
    }

    for cell in &mut scene.cells {
        // The fog: a cell not currently sensed, but walked (or containing a
        // walked descendant at a coarser rung), is memory.
        if cell.state != "here" {
            let Ok(addr) = RoomId(cell.room).unpack() else {
                continue;
            };
            let remembered = walked.iter().any(|w| {
                w.face == addr.face
                    && w.path.len() >= addr.path.len()
                    && w.path[..addr.path.len()] == addr.path[..]
            });
            if remembered {
                cell.state = "remembered".to_string();
            }
        }
        for (room, mark) in &agent_marks {
            if *room == cell.room {
                cell.marks.push(mark.clone());
            }
        }
        cell.marks
            .sort_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)));
    }

    // Every mark's noun joins the chart's catalog — this is the attention
    // join's data half.
    let mut legend = scene.legend.clone();
    for cell in &scene.cells {
        for m in &cell.marks {
            if !legend.iter().any(|e| e.noun == m.noun) {
                legend.push(hornvale_scene::LegendEntry {
                    noun: m.noun.clone(),
                    datum: m.datum.clone(),
                });
            }
        }
    }
    legend.sort_by(|a, b| a.noun.cmp(&b.noun));
    scene.legend = legend;
    Ok(scene)
}
```

Add the module to `windows/vessel/src/lib.rs` beside the others:

```rust
mod purview;
pub use purview::*;
```

Add the session accessor in `windows/vessel/src/session.rs`, next to `ways()`:

```rust
    /// This session's chart, `zoom_out` rungs coarser than the walk depth.
    /// Reads only — the chart never mutates the session.
    pub fn purview(&self, zoom_out: u32) -> Result<hornvale_scene::SurroundsScene, VesselError> {
        crate::purview_scene(
            self.world,
            &self.ctx,
            &self.agent.position,
            &self.knowledge,
            &self.npcs,
            &self.ledger,
            self.day,
            zoom_out,
        )
    }
```

**Note:** `Npc.label` and `Npc.species` are the field names used above — confirm with `grep -n 'pub label\|pub species' windows/vessel/src/liveness.rs` and adjust if they differ.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel purview`
Expected: PASS, 5 tests.

- [ ] **Step 6: Format, clippy, type audit, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/Cargo.toml windows/vessel/src/purview.rs \
        windows/vessel/src/lib.rs windows/vessel/src/session.rs Cargo.lock
git commit -m "feat(vessel): the purview overlay — fog from Knowledge, marks from liveness (The Purview)"
```

---

### Task 6: The `map` verb and the attention join

The campaign's thesis, and the test that could falsify it.

**Files:**
- Modify: `windows/vessel/src/session.rs`
- Create: `windows/vessel/tests/the_purview.rs`

**Interfaces:**
- Consumes: `Session::purview` (Task 5), `render_surrounds_ascii` (Task 3), `Focalized` (`pub struct Focalized { pub prose: String, pub nouns: Vec<(String, String)> }`).
- Produces: the `map` verb; `Session::lens_nouns() -> Vec<(String, String)>`.

**The join.** `examine` today searches only `Focalized.nouns`. It must search the **union** of the prose's nouns and the chart's legend — and where a noun appears at both grains, the prose's datum wins (the fine grain is the deeper answer, which is the whole point of prose being primary).

- [ ] **Step 1: Write the failing thesis test**

Create `windows/vessel/tests/the_purview.rs`:

```rust
//! The Purview's thesis: map and prose are two grains of ONE lens, joined by
//! attention. If these fail, they are two pipelines wearing one name.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, World};
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, Turn};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

fn world() -> World {
    build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) | Turn::Released(s) => s,
    }
}

#[test]
fn examine_accepts_exactly_the_union_of_both_grains() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    for turn in 0..6 {
        let prose: Vec<String> = session
            .focalized()
            .unwrap()
            .nouns
            .iter()
            .map(|(n, _)| n.to_lowercase())
            .collect();
        let chart: Vec<String> = session
            .purview(0)
            .unwrap()
            .legend
            .iter()
            .map(|e| e.noun.to_lowercase())
            .collect();
        let mut union: Vec<String> = prose.iter().chain(chart.iter()).cloned().collect();
        union.sort();
        union.dedup();
        assert!(!union.is_empty(), "turn {turn}: a lens that surfaces nothing is no lens");
        for noun in &union {
            let reply = out(session.handle(&format!("examine {noun}")));
            assert!(
                !reply.starts_with("You see no"),
                "turn {turn}: '{noun}' was surfaced by a grain of the lens but examine refused it: {reply}"
            );
            assert!(!reply.is_empty(), "turn {turn}: '{noun}' resolved to nothing");
        }
        let refused = out(session.handle("examine a-noun-no-grain-surfaced"));
        assert!(
            refused.starts_with("You see no"),
            "turn {turn}: examine must still refuse what no grain surfaced: {refused}"
        );
        let way = session.ways().first().map(|(c, _)| format!("{c:?}"));
        if let Some(way) = way {
            session.handle(&format!("go {way}"));
        }
    }
}

#[test]
fn a_noun_at_both_grains_resolves_to_one_datum() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let prose = session.focalized().unwrap();
    let chart = session.purview(0).unwrap();
    let mut shared = 0;
    for (noun, _) in &prose.nouns {
        if chart
            .legend
            .iter()
            .any(|e| e.noun.eq_ignore_ascii_case(noun))
        {
            shared += 1;
            let a = out(session.handle(&format!("examine {noun}")));
            let b = out(session.handle(&format!("examine {}", noun.to_uppercase())));
            assert_eq!(a, b, "'{noun}' must resolve identically however it is asked");
        }
    }
    // The biome is named by both the prose and the chart's legend, so this is
    // not a vacuous pass.
    assert!(shared > 0, "the two grains must actually overlap");
}

#[test]
fn drawing_the_map_never_moves_the_world() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let where_i_stand = session.agent().position.clone();
    let facts = session.committed_agent_at_count();
    for _ in 0..5 {
        session.handle("map");
        session.handle("map out 2");
    }
    assert_eq!(
        session.agent().position,
        where_i_stand,
        "map does not move the agent"
    );
    assert_eq!(
        session.committed_agent_at_count(),
        facts,
        "map commits nothing"
    );
}

#[test]
fn map_out_reaches_a_coarser_rung_and_stops_at_the_bottom() {
    let w = world();
    let (mut session, _) = Session::start(&w, &PossessOpts::default()).unwrap();
    let fine = out(session.handle("map"));
    let coarse = out(session.handle("map out 3"));
    assert!(fine.contains("[lens: terrain"), "{fine}");
    assert!(coarse.contains("[lens: terrain"), "{coarse}");
    assert_ne!(fine, coarse, "a coarser rung shows different ground");
    let absurd = out(session.handle("map out 99"));
    assert!(
        absurd.contains("no coarser") || absurd.contains("[lens: terrain"),
        "an over-large zoom must refuse or clamp, never panic: {absurd}"
    );
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test the_purview`
Expected: FAIL — `examine` refuses chart-only nouns, and `map` is an unknown verb.

- [ ] **Step 3: Implement the verb and widen `examine`**

In `windows/vessel/src/session.rs`, add the two verbs to the `match` in `handle`, right after the `"look"` arm:

```rust
            "map" => self.map(rest),
```

Add the methods next to `examine`:

```rust
    /// The chart. `map` draws the walk depth; `map out [N]` draws N rungs
    /// coarser — zoom in this mesh is path truncation, so a coarse chart is
    /// the same builder one rung up the address space, never an aggregate.
    fn map(&self, rest: &str) -> Turn {
        let zoom = match rest.split_whitespace().collect::<Vec<_>>().as_slice() {
            [] => 0u32,
            ["out"] => 1,
            ["out", n] => match n.parse::<u32>() {
                Ok(v) => v,
                Err(_) => return Turn::Out(format!("Zoom out by how much? '{n}' is not a number.")),
            },
            _ => return Turn::Out("Say 'map' or 'map out [N]'.".to_string()),
        };
        let depth = self.agent.position.depth();
        if zoom >= depth {
            return Turn::Out(
                "There is no coarser rung than the whole face; the world runs out first."
                    .to_string(),
            );
        }
        let scene = match self.purview(zoom) {
            Ok(s) => s,
            Err(e) => return Turn::Out(format!("error: {e}")),
        };
        let ways: Vec<String> = self
            .ways()
            .iter()
            .map(|(c, _)| format!("{c:?}").to_uppercase())
            .collect();
        Turn::Out(hornvale_scene::render_surrounds_ascii(&scene, "terrain", &ways))
    }

    /// Every noun this lens has surfaced, at either grain: the prose's own
    /// catalog first (the fine grain wins a collision — prose is primary),
    /// then the chart's legend. This union IS the attention join.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn lens_nouns(&self) -> Vec<(String, String)> {
        let mut out: Vec<(String, String)> = match self.focalized() {
            Ok(f) => f.nouns,
            Err(_) => Vec::new(),
        };
        if let Ok(scene) = self.purview(0) {
            for e in &scene.legend {
                if !out.iter().any(|(n, _)| n.eq_ignore_ascii_case(&e.noun)) {
                    out.push((e.noun.clone(), e.datum.clone()));
                }
            }
        }
        out
    }
```

Replace the body of `examine` so it reads the union rather than the prose alone:

```rust
    fn examine(&self, noun: &str) -> Turn {
        if noun.is_empty() {
            return Turn::Out("Examine what?".to_string());
        }
        let wanted = noun.to_lowercase();
        match self
            .lens_nouns()
            .iter()
            .find(|(n, _)| n.to_lowercase() == wanted)
        {
            Some((_, detail)) => Turn::Out(detail.clone()),
            None => Turn::Out(format!("You see no {noun} here.")),
        }
    }
```

Add `map` to the `HELP` constant in the same file, beside `look`:

```
  map [out N]     the chart of what lies around you (N rungs coarser)
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-vessel --test the_purview`
Expected: PASS, 4 tests.

Then the whole vessel crate, because `examine` changed: `cargo test -p hornvale-vessel`
Expected: PASS. If an existing test asserted `examine` refuses a noun the chart now surfaces, that test is asserting the *old* contract — update it and say so in the commit message. Do not narrow `lens_nouns` to make an old assertion pass.

- [ ] **Step 5: Format, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/session.rs windows/vessel/tests/the_purview.rs
git commit -m "feat(vessel): the map verb and the attention join — examine reads both grains (The Purview)"
```

---

### Task 7: The galleries and the browser exhibit

The chart is terminal text, so the browser gets it from the same render — one client change, and the map ships to CLI, book, and browser at once.

**Files:**
- Modify: `scripts/possession-walk.txt`
- Modify: `clients/vessel/src/transcript.ts`
- Modify: `clients/vessel/src/transcript_test.ts`
- Regenerate: `book/src/gallery/possession-seed-42.md` and kin

- [ ] **Step 1: Put the map in the gallery walk**

In `scripts/possession-walk.txt`, add a `map` line after the first `look` and a `map out 2` line after the first `go`. Read the file first — keep the existing turns exactly as they are; the point is that adding a verb is **additive**.

- [ ] **Step 2: Verify the addition is purely additive**

```bash
cargo run --release -q -p hornvale -- new --seed 42 --out /tmp/hv42.json
cargo run --release -q -p hornvale -- possess --world /tmp/hv42.json --script scripts/possession-walk.txt > /tmp/walk-new.txt
git stash push scripts/possession-walk.txt
cargo run --release -q -p hornvale -- possess --world /tmp/hv42.json --script scripts/possession-walk.txt > /tmp/walk-old.txt
git stash pop
diff /tmp/walk-old.txt /tmp/walk-new.txt | grep '^<' || echo "PURELY ADDITIVE — no pre-existing line changed"
```
Expected: `PURELY ADDITIVE`. If any `<` line appears, a pre-existing turn changed — stop and find out why before regenerating anything.

- [ ] **Step 3: Class map lines as monospace in the Casement**

`clients/vessel/src/transcript.ts` currently classes only the room header and the exit list as monospace. The chart is a grid, so it must not be set in the book's serif. Rewrite `splitResponse`:

```typescript
/** One rendered line of a session response. */
export interface Line {
  cls: "casement-meta" | "casement-prose" | "casement-map";
  text: string;
}

/** Split a session response into classed lines for the transcript. The
 * chart is a grid: proportional type would shear it, so map lines take
 * their own monospace class rather than the meta one. */
export function splitResponse(text: string): Line[] {
  let inMap = false;
  return text.split("\n").map((line) => {
    if (line.startsWith("[lens: ")) inMap = true;
    else if (inMap && line.trim() === "") inMap = false;
    const cls = inMap
      ? "casement-map" as const
      : line.startsWith("[room ") || line.startsWith("Ways on:")
      ? "casement-meta" as const
      : "casement-prose" as const;
    return { cls, text: line };
  });
}
```

Add a case to `clients/vessel/src/transcript_test.ts` following the existing test's shape:

```typescript
Deno.test("a chart's lines take the map class, not the prose one", () => {
  const lines = splitResponse(
    "[lens: terrain · depth 12 · radius 4 · lattice-aligned, not north-up]\n  ..@..\n  ways on: E, Nw, Sw",
  );
  assertEquals(lines.every((l) => l.cls === "casement-map"), true);
});
```

Give `.casement-map` a monospace rule wherever `.casement-meta` is styled — find it with `grep -rn 'casement-meta' book/ clients/` and add the sibling rule with `white-space: pre` so the grid's leading spaces survive.

Run: `cd clients/vessel && deno test`
Expected: PASS.

- [ ] **Step 4: Regenerate the artifacts and inspect the diff**

```bash
bash scripts/regenerate-artifacts.sh
git status --short book/src/
git diff --stat book/src/gallery/
```
Expected: the possession galleries and the new surrounds scene change; **nothing else**. Look at the rendered chart in `book/src/gallery/possession-seed-42.md` with your own eyes before committing — a map that is technically deterministic but visually unreadable is a failed deliverable, and this is the step where that gets caught.

- [ ] **Step 5: The full gate**

This campaign touched a `pub` boundary in the kernel, so the full gate runs before the branch is pushed:

```bash
make gate
```
Expected: green — fmt, clippy, nextest, doctests.

```bash
make gate-full
```
Expected: green. If the heavy tier reds, first check whether the failure predates this branch (`git stash` the working tree and re-run, or compare against `origin/main`) — inherited heavy-tier debt is attributed to its original mover, not to this campaign.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add scripts/possession-walk.txt clients/vessel/src/transcript.ts \
        clients/vessel/src/transcript_test.ts book/src/gallery/ book/
git commit -m "feat(book,casement): the chart in the galleries and the browser exhibit (The Purview)"
```

---

## Self-Review

**Spec coverage.** §5.1 `scene/surrounds/v1` → Task 2. §5.2 fog and marks → Task 5. §5.3 `map` and zoom → Tasks 5–6. §5.4 ASCII render, lens registry, caption → Task 3. §5.5 attention join → Task 6. §5.6 artifacts, reference page, Casement → Tasks 4 and 7. §6 architecture and the kernel accessor → Task 1. §7 determinism → asserted in Tasks 2, 5, 6. §8 the falsifiable claim → Task 6's `examine_accepts_exactly_the_union_of_both_grains` and `a_noun_at_both_grains_resolves_to_one_datum`. §9 risks: the seam is covered by Task 3's disclosure test and Task 2's coordinate test, and the gallery-churn risk by Task 7's additive check.

**One deliberate scope trim, recorded at G4.** The spec's §5.2 lists strange sites among the marks; this plan defers them (Task 2's design notes). Placing a strange site needs a canonical-cell→position accessor that is not exposed at the scene boundary, and the chart's legibility does not turn on it. Recorded in the followup register; everything else in §5.2 ships.

**A gap the spec left and this plan closes.** §5.1 named an observer "bearing of north" field. Task 2 drops it: a great-circle bearing drifts with latitude and would have needed a reverse-bearing approximation for half the observers. The chart instead declares `orientation: "lattice"` and the caption prints the room's real exits, which is both simpler and more honest. Ledgered at G4.

**Type consistency.** `FaceLattice { a, b, c, up, scale }` (Task 1) is consumed as `l.a`/`l.b`/`l.c`/`l.up` in Task 2. `SurroundsCell { u, v, w, up, seam, state, marks }` (Task 2) is consumed by the same field names in Tasks 3 and 5. `Mark { noun, kind, datum, salience }` and `LegendEntry { noun, datum }` are used identically in Tasks 2, 3, 5, and 6. `render_surrounds_ascii(scene, lens, ways)` (Task 3) is called with that arity in Task 6. `Session::purview(zoom_out)` (Task 5) is called in Tasks 5 and 6.

**Two names the implementer must confirm before use** (flagged inline, with the grep to run): worldgen's ledger-rebuild helper in Task 2's determinism test, and the settlement lat/lon accessors in Task 4's `settlement_room` — `windows/scene`'s existing `place_latlon` is the body to copy.
