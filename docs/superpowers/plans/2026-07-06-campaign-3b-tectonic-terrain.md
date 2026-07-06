# Campaign 3b: Tectonic Terrain — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give every world a queryable, tectonically-grounded coarse globe — per-cell elevation, plate assignment, and an unrest field over the shared kernel Geosphere — behind a tier-1 `GeneratedTerrain` provider with tectonic pins, composition-root wiring, a deterministic elevation-map artifact, and a property battery.

**Architecture:** A new tier-1 generation path inside `domains/terrain` (kernel-only dependency): plates are Voronoi regions on the unit sphere with drawn Euler-pole motions; boundaries classify from relative velocities into convergent/divergent/transform; elevation is a continental/oceanic base plus boundary uplift decayed by BFS graph distance plus drawn hotspots, with sea level derived from a target ocean fraction. The composition root (`windows/worldgen`) is the only place the provider is constructed; it commits summary facts and terrain-pin strings to the ledger and rebuilds the provider from seed + pins on demand (the `sky_of` pattern). The tier-0 hand-placed Vale stays valid and keeps feeding the social cascade — a documented seam per spec §8.

**Tech Stack:** Rust edition 2024; `hornvale-kernel` (`Seed`/`Stream`, `Geosphere`/`CellMap`, ledger/registry); std only (`VecDeque`, `OnceLock`); hand-rolled P6 PPM encoder in the First Light tradition.

## Global Constraints

- `serde` + `serde_json` only; NO new deps. Terrain types are recomputed, NOT serialized — no serde derives on TectonicGlobe/Plate/GeneratedTerrain.
- Determinism: seed-independent Geosphere; all draws from labeled streams declared in `domains/terrain/src/streams.rs` and returned (fully-qualified) from `stream_labels()`, published through the crate array in `cli/src/streams.rs`; NO HashMap/HashSet in ordered/serialized paths (BTreeMap/BTreeSet); float sorts via `total_cmp`; no wall-clock.
- Pin isolation: pins consume the same draws as the unpinned path (tested).
- Domains depend only on the kernel: terrain depends on `hornvale-kernel` only (terrain currently lists only hornvale-kernel and derives no serde). Climate must be able to read elevation as `CellMap<f64>` without importing terrain.
- Every crate `#![warn(missing_docs)]`; every public item/field/variant a `///` doc comment.
- Edition 2024; `cargo fmt` final step; full gate = `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
- Address the 3a minors where relevant: document the coord convention (latitude = `asin(z)`, longitude = `atan2(y, x)`, degrees) wherever an inverse mapping is built; every CellMap is queried only against the Geosphere it was built from (all at `GLOBE_LEVEL`, asserted in `GeneratedTerrain::new`).
- Elevation is **f64 meters** (bare, documented convention — NOT a newtype): it crosses to climate/biomes via the composition root as `CellMap<f64>`, and climate must not import terrain. This is an accepted, documented deviation from the typed-quantities decision (Campaign 2 design principle 5), justified by the no-cross-domain-import rule.
- Stream labels are permanent save-format contracts: never rename; deliberate regeneration uses an epoch suffix (`terrain/plate-count/v2`).

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `domains/terrain/src/streams.rs` | Create | Seed-derivation labels for tectonic genesis (save-format contract). |
| `domains/terrain/src/pins.rs` | Create | `TerrainPins`, `GenesisError`, `parse_pin`, `pin_strings`, `validate`. |
| `domains/terrain/src/plates.rs` | Create | `Plate`, vector math, drawn plate count/seeds/kinds/motions/maturities, Voronoi assignment, Euler velocities. |
| `domains/terrain/src/boundaries.rs` | Create | `BoundaryKind`, `CellBoundary`, per-edge classification, per-cell boundary field, multi-source same-plate BFS distance. |
| `domains/terrain/src/elevation.rs` | Create | Elevation assembly (base + boundary uplift with maturity + hotspots + strict-ordering epsilon), sea level from target ocean fraction, unrest field. |
| `domains/terrain/src/globe.rs` | Create | `TectonicGlobe`, `GenesisOutcome`, `GlobeSummary`, top-level `generate`, `summarize`. |
| `domains/terrain/src/provider.rs` | Create | `GeneratedTerrain`: queryable provider owning its Geosphere. |
| `domains/terrain/src/facts.rs` | Create | Tectonic summary predicates + `facts::genesis` ledger commits. |
| `domains/terrain/src/render.rs` | Create | Deterministic P6 PPM elevation map + ASCII map + latitude-band nearest-cell index. |
| `domains/terrain/src/lib.rs` | Modify | Module declarations, `GLOBE_LEVEL`, re-exports, real `stream_labels()`, extended `register_concepts`; tier-0 Vale content **stays**. |
| `domains/terrain/tests/tectonic_properties.rs` | Create | Pin-isolation tests + N-seed property battery. |
| `windows/worldgen/src/lib.rs` | Modify | `BuildError::TerrainGenesis`, shared Geosphere (`OnceLock`), `build_world` 4th arg, terrain-pin facts + genesis commits, `terrain_of`, `land_lines`, almanac context field. |
| `windows/almanac/src/lib.rs` | Modify | `AlmanacContext.land_lines` field + rendering in "The Land". |
| `windows/lab/Cargo.toml` | Modify | Add `hornvale-terrain` dependency. |
| `windows/lab/src/metrics.rs` | Modify | `WorldView::build` passes default terrain pins. |
| `cli/src/main.rs` | Modify | `parse_terrain_args`, `--plates`/`--ocean-fraction`/`--supercontinent` flags, `map` command, usage text, call-site updates. |
| `cli/src/repl.rs` | Modify | `map` and `land <lat> <lon>` REPL commands; test-harness call-site updates. |
| `cli/src/streams.rs` | Modify | Manifest test asserts a terrain label. |
| `.github/workflows/ci.yml` | Modify | Elevation-map render added to the artifact drift check. |
| `book/src/SUMMARY.md` | Modify | Gallery entry "The Land of Seed 42"; chronicle entry "Campaign 3b". |
| `book/src/gallery/elevation-seed-42.md` / `.ppm` | Create (generated) | The drift-checked map artifact pair. |
| `book/src/chronicle/campaign-3b.md` | Create | Chronicle entry. |
| `book/src/reference/*-generated.md`, `book/src/gallery/almanac-*.md` | Regenerate | Committed generated artifacts (new predicates, new stream labels, new almanac Land lines). |

Dataflow inside the terrain crate: `pins` → `plates` → `boundaries` → `elevation` → `globe` (assembly) → `provider`/`facts`/`render`. Each module's generator takes the derived `terrain_seed` plus explicit inputs, so every stage is unit-testable with hand-built fixtures.

---

### Task 1: Terrain scaffolding — streams, pins, `GLOBE_LEVEL`

**Files:**
- Create: `domains/terrain/src/streams.rs`
- Create: `domains/terrain/src/pins.rs`
- Modify: `domains/terrain/src/lib.rs`
- Modify: `cli/src/streams.rs` (test assertion only)
- Regenerate: `book/src/reference/stream-manifest-generated.md`

**Interfaces:**
- Consumes: nothing new (kernel `Seed` conventions only).
- Produces:
  - `hornvale_terrain::GLOBE_LEVEL: u32` (= 5)
  - `hornvale_terrain::streams::{ROOT, PLATE_COUNT, PLATE_SEEDS, PLATE_KIND, PLATE_MOTION, MATURITY, HOTSPOTS, OCEAN_FRACTION}: &'static str`
  - `hornvale_terrain::TerrainPins { plates: Option<u32>, ocean_fraction: Option<f64>, supercontinent: Option<bool> }` (`Debug, Clone, Copy, PartialEq, Default`)
  - `hornvale_terrain::GenesisError::{InvalidPin{pin, reason}, UnsatisfiablePin{pin, reason}}` (impl `Display` + `Error`)
  - `hornvale_terrain::parse_pin(s: &str, pins: &mut TerrainPins) -> Result<(), String>`
  - `hornvale_terrain::pin_strings(pins: &TerrainPins) -> Vec<String>`
  - `pub(crate) fn pins::validate(pins: &TerrainPins) -> Result<(), GenesisError>`
  - `hornvale_terrain::stream_labels() -> Vec<(&'static str, &'static str)>` (8 fully-qualified entries)

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/pins.rs` containing ONLY the test module for now:

```rust
//! Tectonic scenario pins: parameters supplied instead of drawn. Downstream
//! generation conditions on pinned values identically to drawn ones;
//! out-of-range or unsatisfiable pins fail loudly.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        assert!(pin_strings(&TerrainPins::default()).is_empty());
    }

    #[test]
    fn pin_strings_round_trip_through_parse_pin() {
        let pins = TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.65),
            supercontinent: Some(true),
        };
        let mut rebuilt = TerrainPins::default();
        for s in pin_strings(&pins) {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
    }

    #[test]
    fn unknown_keys_and_bad_values_are_user_facing_errors() {
        let mut pins = TerrainPins::default();
        assert!(parse_pin("plates", &mut pins).unwrap_err().contains("key=value"));
        assert!(parse_pin("volcanoes=9", &mut pins).unwrap_err().contains("unknown"));
        assert!(parse_pin("plates=many", &mut pins).unwrap_err().contains("invalid"));
        assert!(
            parse_pin("supercontinent=maybe", &mut pins)
                .unwrap_err()
                .contains("true or false")
        );
    }

    #[test]
    fn validate_rejects_out_of_range_pins_with_the_physical_reason() {
        let bad_plates = TerrainPins {
            plates: Some(1),
            ..TerrainPins::default()
        };
        assert!(matches!(
            validate(&bad_plates),
            Err(GenesisError::InvalidPin { .. })
        ));
        let bad_ocean = TerrainPins {
            ocean_fraction: Some(1.5),
            ..TerrainPins::default()
        };
        assert!(matches!(
            validate(&bad_ocean),
            Err(GenesisError::InvalidPin { .. })
        ));
        assert!(validate(&TerrainPins::default()).is_ok());
    }
}
```

In `domains/terrain/src/lib.rs`, replace the crate doc comment and add the module declarations, `GLOBE_LEVEL`, and a test for `stream_labels` (leave every existing item — `IS_PLACE`, `BIOME`, `register_concepts`, `PlaceInfo`, `fact`, `genesis`, `places` — untouched). The top of the file becomes:

```rust
//! Terrain: the tier-0 hand-placed Vale (still the social cascade's seam,
//! spec §8) and the tier-1 tectonic globe — plates, elevation, unrest —
//! computed over the shared kernel Geosphere.
#![warn(missing_docs)]

pub mod pins;
pub mod streams;

pub use pins::{GenesisError, TerrainPins, parse_pin, pin_strings};

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World};

/// The fixed subdivision level of the shared Geosphere (10 × 4^5 + 2 =
/// 10,242 cells). The composition root builds `Geosphere::new(GLOBE_LEVEL)`
/// once per process; every terrain (and, in Plan 3c, climate) CellMap in a
/// world is built against that mesh and must only ever be queried with it.
pub const GLOBE_LEVEL: u32 = 5;
```

and replace the existing empty `stream_labels` with:

```rust
/// Every seed-derivation label this crate uses, with docs. All chains hang
/// off the world seed's "terrain" derivation. Labels are permanent
/// save-format contracts.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("terrain", "root stream for tectonic genesis"),
        ("terrain/plate-count", "how many plates"),
        ("terrain/plate-seeds", "per-plate seed positions on the sphere"),
        (
            "terrain/plate-kind",
            "continental fraction and per-plate continental rolls",
        ),
        (
            "terrain/plate-motion",
            "per-plate Euler pole axis and rate draws",
        ),
        ("terrain/maturity", "per-plate orogenic maturity draws"),
        (
            "terrain/hotspots",
            "hotspot count, positions, and strengths",
        ),
        ("terrain/ocean-fraction", "target ocean fraction draw"),
    ]
}
```

Add to the crate (bottom of `lib.rs`) a test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stream_labels_are_fully_qualified_and_documented() {
        let labels = stream_labels();
        assert_eq!(labels.len(), 8);
        assert_eq!(labels[0].0, "terrain");
        for (label, doc) in &labels[1..] {
            assert!(label.starts_with("terrain/"), "unqualified label {label}");
            assert!(!doc.is_empty());
        }
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain`
Expected: FAIL to compile — `error[E0583]: file not found for module 'streams'` (and, once `streams.rs` exists, `E0425` unresolved `TerrainPins`/`pin_strings`/`parse_pin`/`validate` in `pins.rs`).

- [ ] **Step 3: Write the implementation**

Create `domains/terrain/src/streams.rs`:

```rust
//! Seed-derivation stream labels for tectonic genesis. Labels are permanent
//! save-format contracts; deliberate regeneration uses an epoch suffix
//! (`terrain/plate-count/v2`), never a rename.

/// Root stream: every terrain chain hangs off `world_seed.derive(ROOT)`.
pub const ROOT: &str = "terrain";
/// Plate count draw.
pub const PLATE_COUNT: &str = "plate-count";
/// Per-plate seed-position draws (two `next_f64` per plate, sequential).
pub const PLATE_SEEDS: &str = "plate-seeds";
/// Continental-fraction draw, then one continental roll per plate.
pub const PLATE_KIND: &str = "plate-kind";
/// Per-plate Euler pole draws (axis then rate, sequential).
pub const PLATE_MOTION: &str = "plate-motion";
/// Per-plate orogenic maturity draws.
pub const MATURITY: &str = "maturity";
/// Hotspot count, then per-hotspot position and strength draws.
pub const HOTSPOTS: &str = "hotspots";
/// Target ocean fraction draw.
pub const OCEAN_FRACTION: &str = "ocean-fraction";
```

Fill in `domains/terrain/src/pins.rs` above its test module:

```rust
/// The scenario pins for tectonic genesis. Every field: `None` = drawn from
/// the seed; `Some` = supplied by the experimenter and conditioned on.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct TerrainPins {
    /// Plate count (legal 2–64); drawn 8–40 when `None`.
    pub plates: Option<u32>,
    /// Target ocean fraction (legal 0.05–0.95); drawn 0.5–0.75 when `None`.
    pub ocean_fraction: Option<f64>,
    /// `Some(true)` clusters the continental plates into one landmass;
    /// `Some(false)` explicitly re-affirms the drawn (scattered) layout.
    pub supercontinent: Option<bool>,
}

/// Why tectonic genesis refused to produce a globe.
#[derive(Debug, Clone, PartialEq)]
pub enum GenesisError {
    /// A pin's value is outside its legal range.
    InvalidPin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The rule it violates.
        reason: String,
    },
    /// A legal pin has no physically consistent solution under the model.
    UnsatisfiablePin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The physical conflict.
        reason: String,
    },
}

impl std::fmt::Display for GenesisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenesisError::InvalidPin { pin, reason } => {
                write!(f, "invalid pin '{pin}': {reason}")
            }
            GenesisError::UnsatisfiablePin { pin, reason } => {
                write!(f, "unsatisfiable pin '{pin}': {reason}")
            }
        }
    }
}

impl std::error::Error for GenesisError {}

/// Check every pinned value against its legal range. Called once at the top
/// of `generate`; pins constructed directly (not via `parse_pin`) are still
/// validated here.
pub(crate) fn validate(pins: &TerrainPins) -> Result<(), GenesisError> {
    if let Some(n) = pins.plates {
        if !(2..=64).contains(&n) {
            return Err(GenesisError::InvalidPin {
                pin: "plates".to_string(),
                reason: format!("{n} plates requested; the legal range is 2-64"),
            });
        }
    }
    if let Some(f) = pins.ocean_fraction {
        if !f.is_finite() || !(0.05..=0.95).contains(&f) {
            return Err(GenesisError::InvalidPin {
                pin: "ocean-fraction".to_string(),
                reason: format!("{f} requested; the legal range is 0.05-0.95"),
            });
        }
    }
    Ok(())
}

/// Render every pinned field as a round-trippable `key=value` string.
/// Unpinned (`None`) fields emit nothing.
pub fn pin_strings(pins: &TerrainPins) -> Vec<String> {
    let mut out = Vec::new();
    if let Some(n) = pins.plates {
        out.push(format!("plates={n}"));
    }
    if let Some(f) = pins.ocean_fraction {
        out.push(format!("ocean-fraction={f}"));
    }
    if let Some(s) = pins.supercontinent {
        out.push(format!("supercontinent={s}"));
    }
    out
}

/// Parse one `key=value` pin string (as produced by `pin_strings`) into
/// `pins`, overwriting whichever field it names. Unknown keys or malformed
/// values are user-facing errors naming the offending key/value.
pub fn parse_pin(s: &str, pins: &mut TerrainPins) -> Result<(), String> {
    let (key, value) = s
        .split_once('=')
        .ok_or_else(|| format!("malformed pin '{s}': expected key=value"))?;
    match key {
        "plates" => {
            let n: u32 = value
                .parse()
                .map_err(|_| format!("plates: invalid count '{value}'"))?;
            pins.plates = Some(n);
        }
        "ocean-fraction" => {
            let f: f64 = value
                .parse()
                .map_err(|_| format!("ocean-fraction: invalid number '{value}'"))?;
            pins.ocean_fraction = Some(f);
        }
        "supercontinent" => {
            pins.supercontinent = Some(match value {
                "true" => true,
                "false" => false,
                other => {
                    return Err(format!(
                        "supercontinent: expected true or false, got '{other}'"
                    ));
                }
            });
        }
        other => return Err(format!("unknown terrain pin key '{other}'")),
    }
    Ok(())
}
```

(Note: `pin_strings` uses `{f}` — Rust's `Display` for `f64` is round-trip exact, so `parse_pin` recovers the identical value.)

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain`
Expected: PASS (all new pin/label tests plus the crate's existing tests).

- [ ] **Step 5: Lock the manifest and regenerate it**

In `cli/src/streams.rs`, extend the `manifest_lists_every_crate_and_label` test's expected array by one entry so it reads:

```rust
        for expected in [
            "<!-- GENERATED FILE",
            "| `settlement/name` |",
            "| `religion/epithet` |",
            "| `terrain/plate-count` |",
            "octave-{n}",
        ] {
```

Run: `cargo test -p hornvale && cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md`
Expected: tests PASS; `git diff book/src/reference/stream-manifest-generated.md` shows the eight new terrain rows.

- [ ] **Step 6: Format and commit**

```bash
cargo fmt
git add domains/terrain/src cli/src/streams.rs book/src/reference/stream-manifest-generated.md
git commit -m "feat(terrain): tectonic stream labels, pins, and GLOBE_LEVEL scaffolding"
```

---

### Task 2: Plates — count, seeds, kinds, Euler-pole motion, Voronoi assignment

**Files:**
- Create: `domains/terrain/src/plates.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod plates;` and `pub use plates::Plate;`)

**Interfaces:**
- Consumes: `TerrainPins` (Task 1), `streams::{PLATE_COUNT, PLATE_SEEDS, PLATE_KIND, PLATE_MOTION, MATURITY}` (Task 1), kernel `Seed`, `Stream`, `Geosphere`, `CellMap`, `CellId`.
- Produces:
  - `pub struct Plate { pub id: u32, pub seed_position: [f64; 3], pub continental: bool, pub euler_axis: [f64; 3], pub rate: f64, pub maturity: f64 }` (`Debug, Clone, PartialEq`)
  - `pub fn generate_plates(terrain_seed: Seed, pins: &TerrainPins) -> Vec<Plate>` (assumes validated pins)
  - `pub fn assign_plates(geo: &Geosphere, plates: &[Plate]) -> CellMap<u32>`
  - `pub fn velocity_at(plate: &Plate, position: [f64; 3]) -> [f64; 3]`
  - `pub(crate) fn dot/cross/sub/scale/norm/normalize([f64;3]…)` and `pub(crate) fn unit_vector(&mut Stream) -> [f64; 3]` (shared vector math)

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/plates.rs` containing only:

```rust
//! Plates: Voronoi regions on the unit sphere, each flagged continental or
//! oceanic, moving about a drawn Euler pole, with a drawn orogenic maturity.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::TerrainPins;
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn plate_count_stays_in_the_drawn_range_and_pins_override() {
        for seed in 0..32u64 {
            let plates =
                generate_plates(Seed(seed).derive(streams::ROOT), &TerrainPins::default());
            assert!((8..=40).contains(&(plates.len() as u32)), "seed {seed}");
            for (i, p) in plates.iter().enumerate() {
                assert_eq!(p.id, i as u32);
                assert!((norm(p.seed_position) - 1.0).abs() < 1e-12);
                assert!((norm(p.euler_axis) - 1.0).abs() < 1e-12);
                assert!((0.2..=1.0).contains(&p.rate));
                assert!((0.0..=1.0).contains(&p.maturity));
            }
        }
        let pins = TerrainPins {
            plates: Some(12),
            ..TerrainPins::default()
        };
        assert_eq!(
            generate_plates(Seed(42).derive(streams::ROOT), &pins).len(),
            12
        );
    }

    #[test]
    fn every_cell_joins_exactly_one_plate() {
        let geo = Geosphere::new(2);
        let plates = generate_plates(Seed(42).derive(streams::ROOT), &TerrainPins::default());
        let assignment = assign_plates(&geo, &plates);
        assert_eq!(assignment.len(), geo.cell_count());
        for (_, plate) in assignment.iter() {
            assert!((*plate as usize) < plates.len());
        }
    }

    #[test]
    fn velocities_are_tangent_to_the_sphere() {
        let geo = Geosphere::new(2);
        let plates = generate_plates(Seed(7).derive(streams::ROOT), &TerrainPins::default());
        for cell in geo.cells() {
            let p = geo.position(cell);
            for plate in &plates {
                assert!(dot(velocity_at(plate, p), p).abs() < 1e-12);
            }
        }
    }

    #[test]
    fn supercontinent_clusters_without_perturbing_the_draws() {
        let seed = Seed(42).derive(streams::ROOT);
        let scattered = generate_plates(seed, &TerrainPins::default());
        let clustered = generate_plates(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
        );
        assert_eq!(scattered.len(), clustered.len());
        for (a, b) in scattered.iter().zip(&clustered) {
            assert_eq!(a.seed_position, b.seed_position);
            assert_eq!(a.euler_axis, b.euler_axis);
            assert_eq!(a.rate, b.rate);
            assert_eq!(a.maturity, b.maturity);
        }
        assert!(clustered.iter().filter(|p| p.continental).count() >= 1);
        // Explicitly re-affirming the scattered layout is byte-identical.
        let reaffirmed = generate_plates(
            seed,
            &TerrainPins {
                supercontinent: Some(false),
                ..TerrainPins::default()
            },
        );
        assert_eq!(scattered, reaffirmed);
    }
}
```

Add to `domains/terrain/src/lib.rs`, next to the existing module declarations:

```rust
pub mod plates;
```

and next to the existing re-exports:

```rust
pub use plates::Plate;
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain plates`
Expected: FAIL to compile — `error[E0425]: cannot find function 'generate_plates'` (etc.).

- [ ] **Step 3: Write the implementation**

Fill in `domains/terrain/src/plates.rs` above the test module:

```rust
use crate::pins::TerrainPins;
use crate::streams;
use hornvale_kernel::{CellMap, Geosphere, Seed, Stream};

/// A tectonic plate.
#[derive(Debug, Clone, PartialEq)]
pub struct Plate {
    /// Index into the globe's plate list (equals its position in the Vec).
    pub id: u32,
    /// Voronoi seed position, a unit vector.
    pub seed_position: [f64; 3],
    /// Continental (buoyant, high base) or oceanic (dense, low base).
    pub continental: bool,
    /// Euler-pole rotation axis, a unit vector.
    pub euler_axis: [f64; 3],
    /// Angular rate about the Euler pole (dimensionless model units, 0.2–1.0).
    pub rate: f64,
    /// Orogenic maturity in [0, 1]: 0 = young (sharp, high, restless),
    /// 1 = old (worn, low, quiet).
    pub maturity: f64,
}

/// Dot product.
pub(crate) fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product a × b.
pub(crate) fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// a − b.
pub(crate) fn sub(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

/// a scaled by s.
pub(crate) fn scale(a: [f64; 3], s: f64) -> [f64; 3] {
    [a[0] * s, a[1] * s, a[2] * s]
}

/// Euclidean length.
pub(crate) fn norm(a: [f64; 3]) -> f64 {
    dot(a, a).sqrt()
}

/// a normalized to unit length.
pub(crate) fn normalize(a: [f64; 3]) -> [f64; 3] {
    scale(a, 1.0 / norm(a))
}

/// Draw a uniformly distributed unit vector: z uniform in [-1, 1), azimuth
/// uniform in [0, 2π) — the area-preserving cylindrical construction.
/// Consumes exactly two `next_f64` draws.
pub(crate) fn unit_vector(stream: &mut Stream) -> [f64; 3] {
    let z = 2.0 * stream.next_f64() - 1.0;
    let azimuth = std::f64::consts::TAU * stream.next_f64();
    let r = (1.0 - z * z).sqrt();
    [r * azimuth.cos(), r * azimuth.sin(), z]
}

/// Drawn plate-count range (spec §4: ~8–40).
const PLATE_COUNT_MIN: u32 = 8;
/// Upper end of the drawn plate-count range.
const PLATE_COUNT_MAX: u32 = 40;

/// Generate the plate list: count, seed positions, continental flags,
/// Euler-pole motions, and maturities, each sub-step from its own labeled
/// stream. Pins override drawn values but never skip a draw (pin
/// isolation). Assumes `pins` already validated.
pub fn generate_plates(terrain_seed: Seed, pins: &TerrainPins) -> Vec<Plate> {
    let drawn_count = terrain_seed
        .derive(streams::PLATE_COUNT)
        .stream()
        .range_u32(PLATE_COUNT_MIN, PLATE_COUNT_MAX);
    let count = pins.plates.unwrap_or(drawn_count);

    let mut seed_stream = terrain_seed.derive(streams::PLATE_SEEDS).stream();
    let positions: Vec<[f64; 3]> = (0..count).map(|_| unit_vector(&mut seed_stream)).collect();

    let mut kind_stream = terrain_seed.derive(streams::PLATE_KIND).stream();
    let continental_fraction = 0.25 + 0.25 * kind_stream.next_f64();
    let rolls: Vec<f64> = (0..count).map(|_| kind_stream.next_f64()).collect();
    let continental =
        continental_flags(&positions, &rolls, continental_fraction, pins.supercontinent);

    let mut motion_stream = terrain_seed.derive(streams::PLATE_MOTION).stream();
    let motions: Vec<([f64; 3], f64)> = (0..count)
        .map(|_| {
            let axis = unit_vector(&mut motion_stream);
            let rate = 0.2 + 0.8 * motion_stream.next_f64();
            (axis, rate)
        })
        .collect();

    let mut maturity_stream = terrain_seed.derive(streams::MATURITY).stream();
    let maturities: Vec<f64> = (0..count).map(|_| maturity_stream.next_f64()).collect();

    (0..count)
        .map(|i| {
            let i = i as usize;
            Plate {
                id: i as u32,
                seed_position: positions[i],
                continental: continental[i],
                euler_axis: motions[i].0,
                rate: motions[i].1,
                maturity: maturities[i],
            }
        })
        .collect()
}

/// Continental flags. Scattered (the default and `Some(false)`): each plate
/// is continental when its roll clears the fraction. Supercontinent
/// (`Some(true)`): the same expected number of continental plates, but
/// clustered around the plate with the lowest roll — nearest seed positions
/// win. Both paths consume identical draws, so the pin never perturbs the
/// stream (pin isolation).
fn continental_flags(
    positions: &[[f64; 3]],
    rolls: &[f64],
    fraction: f64,
    supercontinent: Option<bool>,
) -> Vec<bool> {
    if supercontinent != Some(true) {
        return rolls.iter().map(|r| *r < fraction).collect();
    }
    let count = positions.len();
    let continental_count = ((fraction * count as f64).round() as usize).max(1);
    let mut center = 0usize;
    for i in 1..count {
        if rolls[i].total_cmp(&rolls[center]) == std::cmp::Ordering::Less {
            center = i;
        }
    }
    let mut by_closeness: Vec<usize> = (0..count).collect();
    by_closeness.sort_by(|a, b| {
        dot(positions[*b], positions[center])
            .total_cmp(&dot(positions[*a], positions[center]))
            .then(a.cmp(b))
    });
    let mut flags = vec![false; count];
    for i in by_closeness.into_iter().take(continental_count) {
        flags[i] = true;
    }
    flags
}

/// Assign each cell to its nearest plate seed: largest dot product =
/// smallest angular distance on the unit sphere. Ties break to the lower
/// plate id (strict `>` keeps the first best).
pub fn assign_plates(geo: &Geosphere, plates: &[Plate]) -> CellMap<u32> {
    CellMap::from_fn(geo, |cell| {
        let position = geo.position(cell);
        let mut best = 0u32;
        let mut best_dot = f64::NEG_INFINITY;
        for plate in plates {
            let d = dot(position, plate.seed_position);
            if d > best_dot {
                best_dot = d;
                best = plate.id;
            }
        }
        best
    })
}

/// A plate's surface velocity at a unit-sphere position: ω × r, always
/// tangent to the sphere.
pub fn velocity_at(plate: &Plate, position: [f64; 3]) -> [f64; 3] {
    cross(scale(plate.euler_axis, plate.rate), position)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain plates`
Expected: PASS (4 tests).

- [ ] **Step 5: Format and commit**

```bash
cargo fmt
git add domains/terrain/src
git commit -m "feat(terrain): plates - drawn count/seeds/kinds/motion, Voronoi assignment, Euler velocities"
```

---

### Task 3: Boundaries — classification and same-plate BFS distance

**Files:**
- Create: `domains/terrain/src/boundaries.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod boundaries;` and `pub use boundaries::{BoundaryKind, CellBoundary};`)

**Interfaces:**
- Consumes: `Plate`, `assign_plates`, `velocity_at`, `pub(crate)` vector math (Task 2); kernel `Geosphere`, `CellMap`, `CellId`; `std::collections::VecDeque`.
- Produces:
  - `pub enum BoundaryKind { ContinentalCollision, CoastalRange, IslandArc, ContinentalRift, OceanicRidge, Transform }` (`Debug, Clone, Copy, PartialEq, Eq`)
  - `pub struct CellBoundary { pub kind: BoundaryKind, pub magnitude: f64, pub other_plate: u32 }` (`Debug, Clone, Copy, PartialEq`)
  - `pub fn classify_contact(geo: &Geosphere, a: CellId, b: CellId, plate_a: &Plate, plate_b: &Plate) -> CellBoundary`
  - `pub fn boundary_field(geo: &Geosphere, plate_of: &CellMap<u32>, plates: &[Plate]) -> CellMap<Option<CellBoundary>>`
  - `pub fn boundary_distance(geo: &Geosphere, plate_of: &CellMap<u32>, boundaries: &CellMap<Option<CellBoundary>>) -> CellMap<Option<(u32, CellId)>>`

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/boundaries.rs` containing only:

```rust
//! Plate boundaries: classify each cross-plate contact from the relative
//! velocity's component along the great-circle direction between the two
//! cells, and measure every cell's graph distance to the nearest boundary
//! cell of its own plate.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plates::{Plate, assign_plates};
    use hornvale_kernel::Geosphere;

    /// Two continental hemisphere plates spinning against each other:
    /// convergent where y < 0, divergent where y > 0, transform near x = ±1.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                continental: true,
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                continental: true,
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
        ]
    }

    #[test]
    fn hemisphere_plates_produce_an_equatorial_boundary_of_every_regime() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        assert!(boundaries.iter().any(|(_, c)| c.is_some()));
        for (cell, contact) in boundaries.iter() {
            if contact.is_some() {
                assert!(
                    geo.position(cell)[2].abs() < 0.5,
                    "boundary cell {} far from the equator",
                    cell.0
                );
            }
        }
        let kinds: Vec<BoundaryKind> = boundaries
            .iter()
            .filter_map(|(_, c)| c.as_ref().map(|c| c.kind))
            .collect();
        assert!(kinds.contains(&BoundaryKind::ContinentalCollision));
        assert!(kinds.contains(&BoundaryKind::ContinentalRift));
    }

    #[test]
    fn classification_agrees_from_both_sides() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, &plates);
        for a in geo.cells() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(&geo, a, b, &plates[pa as usize], &plates[pb as usize]);
                let ba = classify_contact(&geo, b, a, &plates[pb as usize], &plates[pa as usize]);
                assert_eq!(ab.kind, ba.kind, "asymmetric kind {}-{}", a.0, b.0);
                assert_eq!(ab.magnitude, ba.magnitude, "asymmetric magnitude");
            }
        }
    }

    #[test]
    fn distances_start_at_zero_attribute_a_source_and_grow_by_at_most_one() {
        let geo = Geosphere::new(2);
        let plates = hemisphere_plates(0.5);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        for (cell, entry) in distances.iter() {
            let Some((distance, source)) = entry else {
                panic!("cell {} unreached on a connected hemisphere", cell.0);
            };
            assert!(
                boundaries.get(*source).is_some(),
                "source {} is not a boundary cell",
                source.0
            );
            assert_eq!(
                *plate_of.get(*source),
                *plate_of.get(cell),
                "source crossed a plate"
            );
            if *distance == 0 {
                assert!(boundaries.get(cell).is_some());
            }
            for &neighbor in geo.neighbors(cell) {
                if *plate_of.get(neighbor) != *plate_of.get(cell) {
                    continue;
                }
                let (nd, _) =
                    (*distances.get(neighbor)).expect("same-plate neighbor reached");
                assert!(
                    nd.abs_diff(*distance) <= 1,
                    "distance jumps between {} and {}",
                    cell.0,
                    neighbor.0
                );
            }
        }
    }
}
```

Add to `domains/terrain/src/lib.rs`: `pub mod boundaries;` (module list) and `pub use boundaries::{BoundaryKind, CellBoundary};` (re-exports).

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain boundaries`
Expected: FAIL to compile — `error[E0425]: cannot find function 'boundary_field'` (etc.).

- [ ] **Step 3: Write the implementation**

Fill in `domains/terrain/src/boundaries.rs` above the tests:

```rust
use crate::plates::{Plate, dot, norm, normalize, scale, sub, velocity_at};
use hornvale_kernel::{CellId, CellMap, Geosphere};
use std::collections::VecDeque;

/// How two plates meet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundaryKind {
    /// Convergent continent–continent: a collision range.
    ContinentalCollision,
    /// Convergent ocean–continent: a coastal range on the continental side,
    /// a trench on the oceanic side.
    CoastalRange,
    /// Convergent ocean–ocean: an island arc on the overriding side, a
    /// trench on the subducting side.
    IslandArc,
    /// Divergent continent–continent: a rift valley.
    ContinentalRift,
    /// Divergent with ocean on either side: a mid-ocean ridge.
    OceanicRidge,
    /// Near-tangential motion: a transform fault (unrest, little relief).
    Transform,
}

/// A cell's strongest cross-plate contact.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CellBoundary {
    /// The classified kind of the strongest contact.
    pub kind: BoundaryKind,
    /// Absolute closing (or opening) speed of that contact, model units
    /// (0 to 2: two rate-1.0 plates meeting head-on).
    pub magnitude: f64,
    /// The plate on the other side of the contact.
    pub other_plate: u32,
}

/// Fraction of the relative speed the normal component must exceed to count
/// as convergent/divergent rather than transform.
const TRANSFORM_THRESHOLD: f64 = 0.25;

/// Classify the contact between cell `a` (on `plate_a`) and its neighbor
/// `b` (on `plate_b`). The relative velocity is evaluated at the midpoint;
/// its component along the great-circle direction from `a` toward `b` is
/// the closing speed (positive = converging). Exactly symmetric: swapping
/// `(a, b)` flips both the direction and the relative velocity, so kind and
/// magnitude are bit-identical from either side.
pub fn classify_contact(
    geo: &Geosphere,
    a: CellId,
    b: CellId,
    plate_a: &Plate,
    plate_b: &Plate,
) -> CellBoundary {
    let pa = geo.position(a);
    let pb = geo.position(b);
    let mid = normalize([pa[0] + pb[0], pa[1] + pb[1], pa[2] + pb[2]]);
    let relative = sub(velocity_at(plate_a, mid), velocity_at(plate_b, mid));
    let speed = norm(relative);
    let chord = sub(pb, pa);
    let toward = normalize(sub(chord, scale(mid, dot(chord, mid))));
    let closing = dot(relative, toward);
    let other_plate = plate_b.id;
    if speed < 1e-12 || closing.abs() < TRANSFORM_THRESHOLD * speed {
        return CellBoundary {
            kind: BoundaryKind::Transform,
            magnitude: closing.abs(),
            other_plate,
        };
    }
    let kind = if closing > 0.0 {
        match (plate_a.continental, plate_b.continental) {
            (true, true) => BoundaryKind::ContinentalCollision,
            (false, false) => BoundaryKind::IslandArc,
            _ => BoundaryKind::CoastalRange,
        }
    } else {
        match (plate_a.continental, plate_b.continental) {
            (true, true) => BoundaryKind::ContinentalRift,
            _ => BoundaryKind::OceanicRidge,
        }
    };
    CellBoundary {
        kind,
        magnitude: closing.abs(),
        other_plate,
    }
}

/// Every cell's strongest boundary contact: among neighbors on other
/// plates, the contact with the greatest magnitude (the first neighbor in
/// ascending order wins ties, via strict `>`). `None` for plate interiors.
pub fn boundary_field(
    geo: &Geosphere,
    plate_of: &CellMap<u32>,
    plates: &[Plate],
) -> CellMap<Option<CellBoundary>> {
    CellMap::from_fn(geo, |cell| {
        let my_plate = *plate_of.get(cell);
        let mut best: Option<CellBoundary> = None;
        for &neighbor in geo.neighbors(cell) {
            let other = *plate_of.get(neighbor);
            if other == my_plate {
                continue;
            }
            let contact = classify_contact(
                geo,
                cell,
                neighbor,
                &plates[my_plate as usize],
                &plates[other as usize],
            );
            let better = match &best {
                None => true,
                Some(current) => contact.magnitude > current.magnitude,
            };
            if better {
                best = Some(contact);
            }
        }
        best
    })
}

/// Graph distance from every cell to the nearest boundary cell **of its own
/// plate**, with that boundary cell attributed as the source. Multi-source
/// BFS: seeds enqueued in ascending cell order, neighbors visited in
/// ascending order, propagation never crosses a plate boundary — fully
/// deterministic, O(cells). `None` only for a cell no same-plate boundary
/// cell can reach (a fragmented plate at coarse resolution, or a plate with
/// no boundary at all); callers treat that as "no boundary influence".
pub fn boundary_distance(
    geo: &Geosphere,
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
) -> CellMap<Option<(u32, CellId)>> {
    let mut result: Vec<Option<(u32, CellId)>> = vec![None; geo.cell_count()];
    let mut queue = VecDeque::new();
    for cell in geo.cells() {
        if boundaries.get(cell).is_some() {
            result[cell.0 as usize] = Some((0, cell));
            queue.push_back(cell);
        }
    }
    while let Some(cell) = queue.pop_front() {
        let (distance, source) = result[cell.0 as usize].expect("queued cells are labeled");
        let plate = *plate_of.get(cell);
        for &neighbor in geo.neighbors(cell) {
            if *plate_of.get(neighbor) != plate {
                continue;
            }
            if result[neighbor.0 as usize].is_none() {
                result[neighbor.0 as usize] = Some((distance + 1, source));
                queue.push_back(neighbor);
            }
        }
    }
    CellMap::from_fn(geo, |cell| result[cell.0 as usize])
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain boundaries`
Expected: PASS (3 tests).

- [ ] **Step 5: Format and commit**

```bash
cargo fmt
git add domains/terrain/src
git commit -m "feat(terrain): boundary classification and same-plate BFS distance field"
```

---

### Task 4: Elevation, sea level, and unrest

**Files:**
- Create: `domains/terrain/src/elevation.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod elevation;`)

**Interfaces:**
- Consumes: `Plate`, `unit_vector`, `dot` (Task 2); `BoundaryKind`, `CellBoundary`, `boundary_field`, `boundary_distance` (Task 3); `TerrainPins`, `streams::{MATURITY is on Plate already, HOTSPOTS, OCEAN_FRACTION}` (Task 1); kernel `Seed`, `Geosphere`, `CellMap`, `CellId`.
- Produces:
  - `pub fn generate_elevation(terrain_seed: Seed, geo: &Geosphere, plates: &[Plate], plate_of: &CellMap<u32>, boundaries: &CellMap<Option<CellBoundary>>, distances: &CellMap<Option<(u32, CellId)>>) -> CellMap<f64>` — meters, strictly ordered (per-cell epsilon)
  - `pub fn derive_sea_level(terrain_seed: Seed, pins: &TerrainPins, elevation: &CellMap<f64>) -> f64` — meters
  - `pub fn generate_unrest(geo: &Geosphere, plates: &[Plate], plate_of: &CellMap<u32>, boundaries: &CellMap<Option<CellBoundary>>, distances: &CellMap<Option<(u32, CellId)>>) -> CellMap<f64>` — values in [0, 1]

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/elevation.rs` containing only:

```rust
//! Elevation (meters, bare f64 by documented convention), sea level, and
//! the unrest field. Elevation = continental/oceanic base + the nearest
//! same-plate boundary's contribution decayed by graph distance and shaped
//! by the plate's maturity + drawn hotspots + a per-cell micro-epsilon that
//! guarantees a strict ordering (so the sea-level percentile is exact).

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boundaries::{BoundaryKind, boundary_distance, boundary_field};
    use crate::pins::TerrainPins;
    use crate::plates::{Plate, assign_plates, generate_plates};
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed};

    /// Two continental hemisphere plates spinning against each other:
    /// convergent where y < 0, divergent where y > 0.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                continental: true,
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                continental: true,
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
        ]
    }

    #[test]
    fn collision_uplift_towers_over_the_interior_and_decays_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let elevation = assemble_elevation(&geo, &plates, &plate_of, &boundaries, &distances, &[]);
        let mut peak = f64::NEG_INFINITY;
        for (cell, contact) in boundaries.iter() {
            if let Some(c) = contact {
                if c.kind == BoundaryKind::ContinentalCollision {
                    peak = peak.max(*elevation.get(cell));
                }
            }
        }
        assert!(peak > 3000.0, "collision peak {peak} too low");
        for (cell, entry) in distances.iter() {
            if let Some((distance, _)) = entry {
                if *distance >= 8 {
                    let e = *elevation.get(cell);
                    assert!(
                        (e - 400.0).abs() < 100.0,
                        "cell {} interior elevation {e} strays from base",
                        cell.0
                    );
                }
            }
        }
    }

    #[test]
    fn sea_level_hits_a_pinned_ocean_fraction() {
        let geo = Geosphere::new(3);
        let pins = TerrainPins {
            ocean_fraction: Some(0.65),
            ..TerrainPins::default()
        };
        for seed in [1u64, 7, 42] {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let plates = generate_plates(terrain_seed, &pins);
            let plate_of = assign_plates(&geo, &plates);
            let boundaries = boundary_field(&geo, &plate_of, &plates);
            let distances = boundary_distance(&geo, &plate_of, &boundaries);
            let elevation = generate_elevation(
                terrain_seed,
                &geo,
                &plates,
                &plate_of,
                &boundaries,
                &distances,
            );
            let sea = derive_sea_level(terrain_seed, &pins, &elevation);
            let below = elevation.iter().filter(|(_, e)| **e < sea).count();
            let achieved = below as f64 / elevation.len() as f64;
            assert!(
                (achieved - 0.65).abs() <= 0.01,
                "seed {seed}: achieved {achieved}"
            );
        }
    }

    #[test]
    fn unrest_is_high_on_young_convergent_boundaries_and_dies_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest = generate_unrest(&geo, &plates, &plate_of, &boundaries, &distances);
        let mut boundary_max = 0.0f64;
        for (cell, contact) in boundaries.iter() {
            if contact.is_some() {
                boundary_max = boundary_max.max(*unrest.get(cell));
            }
        }
        assert!(boundary_max > 0.5, "young boundary max {boundary_max}");
        for (cell, entry) in distances.iter() {
            if let Some((distance, _)) = entry {
                if *distance >= 8 {
                    assert!(
                        *unrest.get(cell) < 0.05,
                        "cell {} interior unrest {}",
                        cell.0,
                        unrest.get(cell)
                    );
                }
            }
        }
        for (_, u) in unrest.iter() {
            assert!((0.0..=1.0).contains(u));
        }
    }

    #[test]
    fn old_plates_are_quieter_than_young_ones() {
        let geo = Geosphere::new(3);
        let young = hemisphere_plates(0.0);
        let old = hemisphere_plates(1.0);
        let plate_of = assign_plates(&geo, &young);
        let boundaries = boundary_field(&geo, &plate_of, &young);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest_young = generate_unrest(&geo, &young, &plate_of, &boundaries, &distances);
        let unrest_old = generate_unrest(&geo, &old, &plate_of, &boundaries, &distances);
        let max_young = unrest_young.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        let max_old = unrest_old.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        assert!(
            max_old < max_young,
            "old {max_old} not quieter than young {max_young}"
        );
    }
}
```

(Note the tests use `assemble_elevation` — a private helper taking an explicit hotspot slice so fixtures can disable hotspots — and the public `generate_elevation`, which draws them.)

Add to `domains/terrain/src/lib.rs` module list: `pub mod elevation;`

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain elevation`
Expected: FAIL to compile — `error[E0425]: cannot find function 'assemble_elevation'` (etc.).

- [ ] **Step 3: Write the implementation**

Fill in `domains/terrain/src/elevation.rs` above the tests:

```rust
use crate::boundaries::{BoundaryKind, CellBoundary};
use crate::pins::TerrainPins;
use crate::plates::{Plate, dot, unit_vector};
use crate::streams;
use hornvale_kernel::{CellId, CellMap, Geosphere, Seed};

/// Continental platform base elevation, meters.
const CONTINENT_BASE_M: f64 = 400.0;
/// Abyssal oceanic base elevation, meters.
const OCEAN_BASE_M: f64 = -4000.0;
/// Per-cell-id micro-relief, meters. Breaks every elevation tie so the
/// sea-level percentile is exact; at 10,242 cells the total spread is
/// ~0.01 m — physically invisible, a declared approximation.
const CELL_EPSILON_M: f64 = 1e-6;
/// Maximum possible closing speed (two rate-1.0 plates head-on); boundary
/// magnitudes are normalized against it.
const MAX_CLOSING_SPEED: f64 = 2.0;

/// Signed peak amplitude (meters, at full closing speed, before the
/// maturity factor) a boundary kind contributes on a cell whose own plate
/// is `continental`. The mixed kinds are side-dependent: a coastal range
/// rises on the continent while the trench deepens offshore; an ocean–ocean
/// arc rises on the overriding side (`arc_side`, deterministically the
/// higher plate id) while the other side takes the trench.
fn boundary_amplitude_m(kind: BoundaryKind, continental: bool, arc_side: bool) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 5000.0,
        BoundaryKind::CoastalRange => {
            if continental {
                3000.0
            } else {
                -3000.0
            }
        }
        BoundaryKind::IslandArc => {
            if arc_side {
                1500.0
            } else {
                -2500.0
            }
        }
        BoundaryKind::ContinentalRift => -500.0,
        BoundaryKind::OceanicRidge => 1500.0,
        BoundaryKind::Transform => 0.0,
    }
}

/// A mantle hotspot: a fixed Gaussian dome of uplift.
struct Hotspot {
    /// Dome center, a unit vector.
    position: [f64; 3],
    /// Peak uplift, meters.
    strength_m: f64,
}

/// Angular half-width of a hotspot dome, radians (~3°: one to two cells at
/// level 5).
const HOTSPOT_SIGMA_RAD: f64 = 0.05;

impl Hotspot {
    /// Gaussian dome contribution at a unit-sphere position, meters.
    fn contribution_m(&self, position: [f64; 3]) -> f64 {
        let angle = dot(self.position, position).clamp(-1.0, 1.0).acos();
        self.strength_m
            * (-(angle * angle) / (2.0 * HOTSPOT_SIGMA_RAD * HOTSPOT_SIGMA_RAD)).exp()
    }
}

/// Draw 3–8 hotspots from the hotspots stream: count first, then position
/// (two draws) and strength (1000–3000 m) per hotspot, sequentially.
fn draw_hotspots(terrain_seed: Seed) -> Vec<Hotspot> {
    let mut stream = terrain_seed.derive(streams::HOTSPOTS).stream();
    let count = stream.range_u32(3, 8);
    (0..count)
        .map(|_| {
            let position = unit_vector(&mut stream);
            let strength_m = 1000.0 + 2000.0 * stream.next_f64();
            Hotspot {
                position,
                strength_m,
            }
        })
        .collect()
}

/// Pure elevation assembly over explicit inputs (hotspots included), so
/// tests can pin the hotspot list. See the module doc for the formula.
fn assemble_elevation(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
    hotspots: &[Hotspot],
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let plate = &plates[*plate_of.get(cell) as usize];
        let base = if plate.continental {
            CONTINENT_BASE_M
        } else {
            OCEAN_BASE_M
        };
        let boundary_term = match *distances.get(cell) {
            None => 0.0,
            Some((distance, source)) => {
                let contact = (*boundaries.get(source)).expect("BFS sources are boundary cells");
                let arc_side = plate.id > contact.other_plate;
                let amplitude = boundary_amplitude_m(contact.kind, plate.continental, arc_side);
                // Young plates (maturity 0): 1.5x amplitude, sharp 1.5-cell
                // falloff. Old plates (maturity 1): 0.5x amplitude, worn
                // 4.5-cell falloff.
                let factor = 1.5 - plate.maturity;
                let decay_cells = 1.5 + 3.0 * plate.maturity;
                amplitude
                    * (contact.magnitude / MAX_CLOSING_SPEED)
                    * factor
                    * (-f64::from(distance) / decay_cells).exp()
            }
        };
        let position = geo.position(cell);
        let hotspot_term: f64 = hotspots.iter().map(|h| h.contribution_m(position)).sum();
        base + boundary_term + hotspot_term + CELL_EPSILON_M * f64::from(cell.0)
    })
}

/// Per-cell elevation in meters: continental/oceanic base, the nearest
/// same-plate boundary's contribution decayed by graph distance and shaped
/// by maturity, drawn hotspots, and a strict-ordering micro-epsilon.
pub fn generate_elevation(
    terrain_seed: Seed,
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
) -> CellMap<f64> {
    let hotspots = draw_hotspots(terrain_seed);
    assemble_elevation(geo, plates, plate_of, boundaries, distances, &hotspots)
}

/// Derive sea level: draw (or pin) a target ocean fraction, then place sea
/// level at the elevation percentile that puts exactly that fraction of
/// cells strictly below it. The draw is consumed whether pinned or not
/// (pin isolation). Sort uses `total_cmp` — elevations are finite and
/// strictly ordered by construction.
pub fn derive_sea_level(terrain_seed: Seed, pins: &TerrainPins, elevation: &CellMap<f64>) -> f64 {
    let drawn = 0.5
        + 0.25
            * terrain_seed
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let target = pins.ocean_fraction.unwrap_or(drawn);
    let mut sorted: Vec<f64> = elevation.iter().map(|(_, e)| *e).collect();
    sorted.sort_by(|a, b| a.total_cmp(b));
    let index = ((target * sorted.len() as f64) as usize).min(sorted.len() - 1);
    sorted[index]
}

/// Relative restlessness of each boundary kind.
fn intensity(kind: BoundaryKind) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 0.8,
        BoundaryKind::CoastalRange => 0.9,
        BoundaryKind::IslandArc => 0.9,
        BoundaryKind::ContinentalRift => 0.7,
        BoundaryKind::OceanicRidge => 0.6,
        BoundaryKind::Transform => 0.5,
    }
}

/// Distance decay length for unrest, in cells.
const UNREST_DECAY_CELLS: f64 = 2.0;

/// The unrest field, per cell in [0, 1]: boundary intensity × normalized
/// closing speed × youth (inverse maturity), decayed by distance to the
/// nearest same-plate boundary; clamped. Old quiet interiors approach zero.
/// Nothing consumes it in C3 — it is banked (spec §15).
pub fn generate_unrest(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let Some((distance, source)) = *distances.get(cell) else {
            return 0.0;
        };
        let contact = (*boundaries.get(source)).expect("BFS sources are boundary cells");
        let plate = &plates[*plate_of.get(cell) as usize];
        let youth = 1.5 - plate.maturity;
        let raw = intensity(contact.kind)
            * (contact.magnitude / MAX_CLOSING_SPEED)
            * youth
            * (-f64::from(distance) / UNREST_DECAY_CELLS).exp();
        raw.clamp(0.0, 1.0)
    })
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain elevation`
Expected: PASS (4 tests).

- [ ] **Step 5: Format and commit**

```bash
cargo fmt
git add domains/terrain/src
git commit -m "feat(terrain): elevation with maturity-shaped boundary uplift, percentile sea level, unrest field"
```

---

### Task 5: `generate` — assembling the tectonic globe

**Files:**
- Create: `domains/terrain/src/globe.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod globe;` and `pub use globe::{GenesisOutcome, GlobeSummary, TectonicGlobe, generate, summarize};`)

**Interfaces:**
- Consumes: everything from Tasks 1–4 (`pins::validate`, `generate_plates`, `assign_plates`, `boundary_field`, `boundary_distance`, `generate_elevation`, `derive_sea_level`, `generate_unrest`).
- Produces:
  - `pub struct TectonicGlobe { pub plate_of: CellMap<u32>, pub elevation: CellMap<f64>, pub unrest: CellMap<f64>, pub sea_level: f64, pub plates: Vec<Plate> }` (`Debug, Clone, PartialEq`)
  - `pub struct GenesisOutcome { pub globe: TectonicGlobe, pub notes: Vec<String> }` (`Debug, Clone, PartialEq`)
  - `pub fn generate(world_seed: Seed, geosphere: &Geosphere, pins: &TerrainPins) -> Result<GenesisOutcome, GenesisError>`
  - `pub struct GlobeSummary { pub plate_count: u32, pub ocean_fraction: f64, pub sea_level_m: f64, pub highest_elevation_m: f64 }` (`Debug, Clone, Copy, PartialEq`)
  - `pub fn summarize(globe: &TectonicGlobe) -> GlobeSummary`

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/globe.rs` containing only:

```rust
//! The tectonic globe: the assembled outcome of terrain genesis. A world is
//! a seed plus a ledger — the globe is never serialized, always re-derived.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{GenesisError, TerrainPins};
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn genesis_is_deterministic() {
        let geo = Geosphere::new(3);
        let a = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        assert_eq!(a, b);
    }

    #[test]
    fn different_seeds_produce_different_globes() {
        let geo = Geosphere::new(3);
        let a = generate(Seed(1), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(2), &geo, &TerrainPins::default()).unwrap();
        assert_ne!(a.globe.elevation, b.globe.elevation);
    }

    #[test]
    fn summary_reports_the_globe() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let s = summarize(&outcome.globe);
        assert_eq!(s.plate_count as usize, outcome.globe.plates.len());
        assert!((0.4..=0.8).contains(&s.ocean_fraction));
        assert_eq!(s.sea_level_m, outcome.globe.sea_level);
        assert!(s.highest_elevation_m > s.sea_level_m);
    }

    #[test]
    fn invalid_pins_fail_loudly_and_never_retry() {
        let geo = Geosphere::new(2);
        let err = generate(
            Seed(1),
            &geo,
            &TerrainPins {
                plates: Some(1),
                ..TerrainPins::default()
            },
        )
        .unwrap_err();
        assert!(matches!(err, GenesisError::InvalidPin { .. }));
        let err = generate(
            Seed(1),
            &geo,
            &TerrainPins {
                ocean_fraction: Some(1.5),
                ..TerrainPins::default()
            },
        )
        .unwrap_err();
        assert!(matches!(err, GenesisError::InvalidPin { .. }));
    }
}
```

Add to `domains/terrain/src/lib.rs`: `pub mod globe;` and `pub use globe::{GenesisOutcome, GlobeSummary, TectonicGlobe, generate, summarize};`

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain globe`
Expected: FAIL to compile — `error[E0425]: cannot find function 'generate'` (etc.).

- [ ] **Step 3: Write the implementation**

Fill in `domains/terrain/src/globe.rs` above the tests:

```rust
use crate::pins::{self, GenesisError, TerrainPins};
use crate::plates::Plate;
use crate::streams;
use crate::{boundaries, elevation, plates};
use hornvale_kernel::{CellMap, Geosphere, Seed};

/// A generated tectonic globe over the shared Geosphere. Recomputed from
/// the seed on demand; never serialized.
#[derive(Debug, Clone, PartialEq)]
pub struct TectonicGlobe {
    /// Plate index per cell (an index into `plates`).
    pub plate_of: CellMap<u32>,
    /// Elevation per cell, in meters (bare f64 by documented convention;
    /// see the plan's Global Constraints for the typed-quantities waiver).
    pub elevation: CellMap<f64>,
    /// Unrest per cell, in [0, 1]. Banked for future consumers (spec §15).
    pub unrest: CellMap<f64>,
    /// Sea level, in meters: cells strictly below it are ocean.
    pub sea_level: f64,
    /// The plates, indexed by `plate_of`'s values.
    pub plates: Vec<Plate>,
}

/// What tectonic genesis produced: the globe plus degradation notes.
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisOutcome {
    /// The generated globe.
    pub globe: TectonicGlobe,
    /// Degradation notes (empty when genesis was untroubled).
    pub notes: Vec<String>,
}

/// Generate a tectonic globe. Derives the terrain root stream from the
/// world seed exactly once; every sub-step consumes its own labeled child
/// stream in a fixed order whether its value is pinned or drawn (pin
/// isolation). Pins fail loudly; generation never retries across seeds —
/// the seed is a world's identity.
pub fn generate(
    world_seed: Seed,
    geosphere: &Geosphere,
    pins: &TerrainPins,
) -> Result<GenesisOutcome, GenesisError> {
    pins::validate(pins)?;
    let terrain_seed = world_seed.derive(streams::ROOT);
    let plate_list = plates::generate_plates(terrain_seed, pins);
    let plate_of = plates::assign_plates(geosphere, &plate_list);
    let boundary_map = boundaries::boundary_field(geosphere, &plate_of, &plate_list);
    let distances = boundaries::boundary_distance(geosphere, &plate_of, &boundary_map);
    let elevation_map = elevation::generate_elevation(
        terrain_seed,
        geosphere,
        &plate_list,
        &plate_of,
        &boundary_map,
        &distances,
    );
    let sea_level = elevation::derive_sea_level(terrain_seed, pins, &elevation_map);
    let unrest = elevation::generate_unrest(
        geosphere,
        &plate_list,
        &plate_of,
        &boundary_map,
        &distances,
    );

    let mut notes = Vec::new();
    let mut populated = vec![false; plate_list.len()];
    for (_, plate) in plate_of.iter() {
        populated[*plate as usize] = true;
    }
    let empty = populated.iter().filter(|p| !**p).count();
    if empty > 0 {
        notes.push(format!(
            "{empty} plate(s) hold no cells at this resolution"
        ));
    }
    if boundary_map.iter().all(|(_, b)| b.is_none()) {
        notes.push("no plate boundaries at this resolution".to_string());
    }

    Ok(GenesisOutcome {
        globe: TectonicGlobe {
            plate_of,
            elevation: elevation_map,
            unrest,
            sea_level,
            plates: plate_list,
        },
        notes,
    })
}

/// Headline numbers of a globe, for facts and the almanac.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlobeSummary {
    /// How many plates the globe drew (or was pinned to).
    pub plate_count: u32,
    /// Achieved ocean fraction: cells strictly below sea level, over all cells.
    pub ocean_fraction: f64,
    /// Sea level, meters.
    pub sea_level_m: f64,
    /// Highest cell elevation, meters.
    pub highest_elevation_m: f64,
}

/// Summarize a globe's headline numbers. Deterministic: iteration is in
/// ascending cell order and elevations are finite.
pub fn summarize(globe: &TectonicGlobe) -> GlobeSummary {
    let ocean_cells = globe
        .elevation
        .iter()
        .filter(|(_, e)| **e < globe.sea_level)
        .count();
    let highest = globe
        .elevation
        .iter()
        .map(|(_, e)| *e)
        .fold(f64::NEG_INFINITY, f64::max);
    GlobeSummary {
        plate_count: globe.plates.len() as u32,
        ocean_fraction: ocean_cells as f64 / globe.elevation.len() as f64,
        sea_level_m: globe.sea_level,
        highest_elevation_m: highest,
    }
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain globe`
Expected: PASS (4 tests).

- [ ] **Step 5: Format and commit**

```bash
cargo fmt
git add domains/terrain/src
git commit -m "feat(terrain): top-level tectonic generate() and globe summary"
```

---

### Task 6: Provider, facts, and concept registration

**Files:**
- Create: `domains/terrain/src/provider.rs`
- Create: `domains/terrain/src/facts.rs`
- Modify: `domains/terrain/src/lib.rs` (modules, re-export, extended `register_concepts`)

**Interfaces:**
- Consumes: `TectonicGlobe`, `GenesisOutcome`, `summarize` (Task 5); kernel `Geosphere`, `CellId`, `EntityId`, `Fact`, `Value`, `World`, `LedgerError`, `ConceptRegistry`, `RegistryError`.
- Produces:
  - `pub struct GeneratedTerrain` (`Debug, Clone`) with `new(geosphere: Geosphere, outcome: GenesisOutcome) -> GeneratedTerrain` (panics on cell-count mismatch — fail fast), `geosphere() -> &Geosphere`, `globe() -> &TectonicGlobe`, `notes() -> &[String]`, `elevation_at(CellId) -> f64`, `unrest_at(CellId) -> f64`, `plate_of(CellId) -> u32`, `sea_level() -> f64`, `is_ocean(CellId) -> bool`, `nearest_cell(latitude: f64, longitude: f64) -> CellId`
  - `pub const facts::{PLATE_COUNT, OCEAN_FRACTION, SEA_LEVEL_M, HIGHEST_ELEVATION_M, TERRAIN_PIN, TERRAIN_NOTE}: &str` = `"plate-count"`, `"ocean-fraction"`, `"sea-level-m"`, `"highest-elevation-m"`, `"terrain-pin"`, `"terrain-note"`
  - `pub fn facts::genesis(world: &mut World, subject: EntityId, outcome: &GenesisOutcome) -> Result<(), LedgerError>`
  - `register_concepts` now also registers the six predicates above (first four functional Number; last two non-functional Text).

- [ ] **Step 1: Write the failing tests**

Create `domains/terrain/src/provider.rs` containing only:

```rust
//! The tier-1 terrain provider: a queryable generated tectonic globe.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{CellId, Geosphere, Seed};

    #[test]
    fn provider_answers_every_query_consistently() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
        let cell = CellId(0);
        assert_eq!(terrain.elevation_at(cell), *outcome.globe.elevation.get(cell));
        assert_eq!(terrain.plate_of(cell), *outcome.globe.plate_of.get(cell));
        assert_eq!(terrain.unrest_at(cell), *outcome.globe.unrest.get(cell));
        assert_eq!(terrain.sea_level(), outcome.globe.sea_level);
        assert_eq!(
            terrain.is_ocean(cell),
            terrain.elevation_at(cell) < terrain.sea_level()
        );
        assert_eq!(terrain.geosphere().cell_count(), geo.cell_count());
        assert_eq!(terrain.notes(), outcome.notes.as_slice());
    }

    #[test]
    fn nearest_cell_round_trips_cell_coordinates() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in [CellId(0), CellId(100), CellId(641)] {
            let coord = geo.coord(cell);
            assert_eq!(terrain.nearest_cell(coord.latitude, coord.longitude), cell);
        }
    }

    #[test]
    #[should_panic(expected = "disagree on cell count")]
    fn mismatched_geosphere_fails_fast() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        GeneratedTerrain::new(Geosphere::new(2), outcome);
    }
}
```

Create `domains/terrain/src/facts.rs` containing only:

```rust
//! Tectonic genesis facts: summary truths only, never per-cell data — the
//! ledger keeps singular authored truths and saves stay small (spec §3).

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, Value, World};

    #[test]
    fn genesis_commits_the_summary_facts() {
        let mut world = World::new(Seed(42));
        crate::register_concepts(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        let geo = hornvale_kernel::Geosphere::new(3);
        let outcome = crate::generate(Seed(42), &geo, &crate::TerrainPins::default()).unwrap();
        genesis(&mut world, subject, &outcome).unwrap();
        let summary = crate::summarize(&outcome.globe);
        assert_eq!(
            world.ledger.value_of(subject, PLATE_COUNT),
            Some(&Value::Number(f64::from(summary.plate_count)))
        );
        assert_eq!(
            world.ledger.value_of(subject, OCEAN_FRACTION),
            Some(&Value::Number(summary.ocean_fraction))
        );
        assert_eq!(
            world.ledger.value_of(subject, SEA_LEVEL_M),
            Some(&Value::Number(summary.sea_level_m))
        );
        assert_eq!(
            world.ledger.value_of(subject, HIGHEST_ELEVATION_M),
            Some(&Value::Number(summary.highest_elevation_m))
        );
    }

    #[test]
    fn register_concepts_is_idempotent() {
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        crate::register_concepts(&mut registry).unwrap();
        crate::register_concepts(&mut registry).unwrap();
        assert!(registry.predicate(TERRAIN_PIN).is_some());
        assert!(registry.predicate(TERRAIN_NOTE).is_some());
    }
}
```

Add to `domains/terrain/src/lib.rs`: `pub mod facts;` and `pub mod provider;` (module list), `pub use provider::GeneratedTerrain;` (re-exports).

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain provider facts`
Expected: FAIL to compile — `error[E0425]: cannot find` for `GeneratedTerrain`, `genesis`, `PLATE_COUNT` (etc.).

- [ ] **Step 3: Write the provider implementation**

Fill in `domains/terrain/src/provider.rs` above the tests:

```rust
use crate::globe::{GenesisOutcome, TectonicGlobe};
use crate::plates::dot;
use hornvale_kernel::{CellId, Geosphere};

/// A queryable tectonic terrain provider. Owns its Geosphere so queries and
/// the globe's CellMaps always agree on the cell space — a CellMap must
/// only ever be read with the mesh that built it.
#[derive(Debug, Clone)]
pub struct GeneratedTerrain {
    geosphere: Geosphere,
    globe: TectonicGlobe,
    notes: Vec<String>,
}

impl GeneratedTerrain {
    /// Wrap a genesis outcome with the Geosphere it was generated over.
    /// Panics (fail fast) if the mesh and the globe disagree on cell count —
    /// the caller must pass the same Geosphere it gave `generate`.
    pub fn new(geosphere: Geosphere, outcome: GenesisOutcome) -> GeneratedTerrain {
        assert_eq!(
            geosphere.cell_count(),
            outcome.globe.elevation.len(),
            "GeneratedTerrain: geosphere and globe disagree on cell count"
        );
        GeneratedTerrain {
            geosphere,
            globe: outcome.globe,
            notes: outcome.notes,
        }
    }

    /// The Geosphere the globe is computed over.
    pub fn geosphere(&self) -> &Geosphere {
        &self.geosphere
    }

    /// The full tectonic globe.
    pub fn globe(&self) -> &TectonicGlobe {
        &self.globe
    }

    /// Degradation notes recorded at genesis.
    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    /// Elevation at a cell, meters.
    pub fn elevation_at(&self, id: CellId) -> f64 {
        *self.globe.elevation.get(id)
    }

    /// Unrest at a cell, in [0, 1].
    pub fn unrest_at(&self, id: CellId) -> f64 {
        *self.globe.unrest.get(id)
    }

    /// The plate a cell belongs to.
    pub fn plate_of(&self, id: CellId) -> u32 {
        *self.globe.plate_of.get(id)
    }

    /// Sea level, meters.
    pub fn sea_level(&self) -> f64 {
        self.globe.sea_level
    }

    /// Whether a cell lies strictly below sea level.
    pub fn is_ocean(&self, id: CellId) -> bool {
        self.elevation_at(id) < self.globe.sea_level
    }

    /// The cell nearest a geographic coordinate (degrees), by maximum dot
    /// product with the coordinate's unit vector; ties break to the lower
    /// cell id. Inverts the kernel's coord convention — latitude = asin(z),
    /// longitude = atan2(y, x) — so `nearest_cell(coord(c))` returns `c`.
    pub fn nearest_cell(&self, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cell in self.geosphere.cells() {
            let d = dot(self.geosphere.position(cell), target);
            if d > best_dot {
                best_dot = d;
                best = cell;
            }
        }
        best
    }
}
```

- [ ] **Step 4: Write the facts implementation**

Fill in `domains/terrain/src/facts.rs` above the tests:

```rust
use crate::globe::{GenesisOutcome, summarize};
use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// Predicate: how many plates the globe has (functional, Number).
pub const PLATE_COUNT: &str = "plate-count";
/// Predicate: achieved ocean fraction (functional, Number).
pub const OCEAN_FRACTION: &str = "ocean-fraction";
/// Predicate: sea level in meters (functional, Number).
pub const SEA_LEVEL_M: &str = "sea-level-m";
/// Predicate: highest cell elevation in meters (functional, Number).
pub const HIGHEST_ELEVATION_M: &str = "highest-elevation-m";
/// Predicate: one round-trippable terrain pin string per pinned value
/// (non-functional, Text) — the terrain sibling of astronomy's scenario-pin.
pub const TERRAIN_PIN: &str = "terrain-pin";
/// Predicate: a note recorded during tectonic genesis (non-functional, Text).
pub const TERRAIN_NOTE: &str = "terrain-note";

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "terrain".to_string(),
    }
}

/// Commit the globe's summary facts and genesis notes against `subject`
/// (the world entity). Summary only — never per-cell facts.
pub fn genesis(
    world: &mut World,
    subject: EntityId,
    outcome: &GenesisOutcome,
) -> Result<(), LedgerError> {
    let summary = summarize(&outcome.globe);
    world.ledger.commit(
        fact(
            subject,
            PLATE_COUNT,
            Value::Number(f64::from(summary.plate_count)),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(subject, OCEAN_FRACTION, Value::Number(summary.ocean_fraction)),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(subject, SEA_LEVEL_M, Value::Number(summary.sea_level_m)),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HIGHEST_ELEVATION_M,
            Value::Number(summary.highest_elevation_m),
        ),
        &world.registry,
    )?;
    for note in &outcome.notes {
        world.ledger.commit(
            fact(subject, TERRAIN_NOTE, Value::Text(note.clone())),
            &world.registry,
        )?;
    }
    Ok(())
}
```

In `domains/terrain/src/lib.rs`, replace the existing `register_concepts` with:

```rust
/// Register terrain's contribution to the concept registry: the tier-0
/// place predicates plus the tectonic summary predicates. Idempotent.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PLACE, true, "subject is a traversable place")?;
    registry.register_predicate(BIOME, true, "biome of a place")?;
    registry.register_predicate(
        facts::PLATE_COUNT,
        true,
        "how many tectonic plates the globe has",
    )?;
    registry.register_predicate(
        facts::OCEAN_FRACTION,
        true,
        "fraction of globe cells below sea level",
    )?;
    registry.register_predicate(facts::SEA_LEVEL_M, true, "sea level in meters")?;
    registry.register_predicate(
        facts::HIGHEST_ELEVATION_M,
        true,
        "highest globe cell elevation in meters",
    )?;
    registry.register_predicate(
        facts::TERRAIN_PIN,
        false,
        "a terrain scenario pin, round-trippable",
    )?;
    registry.register_predicate(
        facts::TERRAIN_NOTE,
        false,
        "a note recorded during tectonic genesis",
    )
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain`
Expected: PASS (whole crate — provider, facts, and all earlier modules).

- [ ] **Step 6: Format and commit**

```bash
cargo fmt
git add domains/terrain/src
git commit -m "feat(terrain): GeneratedTerrain provider, summary facts, concept registration"
```

---

### Task 7: Composition-root wiring — `build_world`, `terrain_of`, almanac Land lines

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `windows/almanac/src/lib.rs`
- Modify: `windows/lab/Cargo.toml`, `windows/lab/src/metrics.rs`
- Modify: `cli/src/main.rs` (call sites), `cli/src/repl.rs` (test call sites)
- Regenerate: `book/src/reference/concept-registry-generated.md`, `book/src/gallery/almanac-seed-42.md`, `book/src/gallery/almanac-seed-42-sky.md`, `book/src/gallery/almanac-seed-42-locked.md`

**Interfaces:**
- Consumes: `hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins, generate, parse_pin, pin_strings, summarize, facts}` (Tasks 1–6); kernel `Geosphere`; `std::sync::OnceLock`.
- Produces:
  - `pub fn build_world(seed: Seed, pins: &SkyPins, sky: SkyChoice, terrain_pins: &TerrainPins) -> Result<World, BuildError>` (signature change: 4th parameter)
  - `pub fn terrain_of(world: &World) -> Result<GeneratedTerrain, BuildError>`
  - `pub fn land_lines(world: &World) -> Result<Vec<String>, BuildError>` (two lines: plates/ocean-fraction, highest land)
  - `BuildError::TerrainGenesis(hornvale_terrain::GenesisError)` variant
  - `AlmanacContext.land_lines: Vec<String>` field, rendered in "## The Land"

- [ ] **Step 1: Write the failing tests**

Append to the `mod tests` block in `windows/worldgen/src/lib.rs`:

```rust
    #[test]
    fn terrain_reconstructs_from_seed_and_pins() {
        let world = constant(42);
        let a = terrain_of(&world).unwrap();
        let b = terrain_of(&world).unwrap();
        assert_eq!(a.globe(), b.globe());
        assert_eq!(a.geosphere().level(), hornvale_terrain::GLOBE_LEVEL);
    }

    #[test]
    fn terrain_pins_round_trip_through_the_ledger() {
        let pins = hornvale_terrain::TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.7),
            ..hornvale_terrain::TerrainPins::default()
        };
        let world =
            build_world(Seed(42), &SkyPins::default(), SkyChoice::Constant, &pins).unwrap();
        let terrain = terrain_of(&world).unwrap();
        assert_eq!(terrain.globe().plates.len(), 12);
        let summary = hornvale_terrain::summarize(terrain.globe());
        assert!((summary.ocean_fraction - 0.7).abs() < 0.01);
    }

    #[test]
    fn terrain_facts_are_committed_at_build() {
        let world = constant(42);
        assert!(
            world
                .ledger
                .find(hornvale_terrain::facts::PLATE_COUNT)
                .next()
                .is_some()
        );
        assert!(
            world
                .ledger
                .find(hornvale_terrain::facts::OCEAN_FRACTION)
                .next()
                .is_some()
        );
    }

    #[test]
    fn land_lines_describe_the_globe() {
        let world = constant(42);
        let lines = land_lines(&world).unwrap();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].contains("plates"));
        assert!(lines[1].contains("above the sea"));
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-worldgen`
Expected: FAIL to compile — `error[E0425]: cannot find function 'terrain_of'` and `build_world` arity mismatch.

- [ ] **Step 3: Wire the composition root**

In `windows/worldgen/src/lib.rs`:

(a) Extend the imports — add `Geosphere` to the kernel import list, add the terrain and `OnceLock` imports (astronomy's `generate`/`parse_pin`/`pin_strings`/`GenesisError` stay imported by name; terrain's are always fully qualified to avoid collision):

```rust
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Geosphere, LedgerError, ObserverContext, PhenomenaSource,
    Phenomenon, RegistryError, Seed, Value, World, WorldTime, observe,
};
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins};
use std::sync::OnceLock;
```

(b) Add the variant to `BuildError` (with a `Display` arm):

```rust
    /// Terrain genesis refused a pin.
    TerrainGenesis(hornvale_terrain::GenesisError),
```

```rust
            BuildError::TerrainGenesis(e) => write!(f, "terrain genesis: {e}"),
```

(c) Add the shared mesh (below `register_all`):

```rust
/// The shared Geosphere at `GLOBE_LEVEL`: seed-independent, computed once
/// per process and cloned into providers, so per-world mesh cost is a
/// memcpy (spec §3: "computed once, shared across all worlds").
fn shared_geosphere() -> &'static Geosphere {
    static GEO: OnceLock<Geosphere> = OnceLock::new();
    GEO.get_or_init(|| Geosphere::new(GLOBE_LEVEL))
}
```

(d) Change `build_world`'s signature and body — the full replacement:

```rust
/// Build a complete world: mint the world entity and record its sky choice
/// and scenario pins first; run sky genesis for `Generated`; commit the
/// terrain pins and run tectonic genesis; then the tier-0 cascade
/// (terrain-Vale → settlement → culture → religion-from-phenomena). The
/// Vale stays the social cascade's seam (Campaign 3 spec §8); the tectonic
/// globe is an additional queryable capability, not a replacement.
pub fn build_world(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
) -> Result<World, BuildError> {
    let mut world = World::new(seed);
    register_all(&mut world.registry)?;

    let world_entity = world.ledger.mint_entity();
    let choice_text = match sky {
        SkyChoice::Constant => "constant",
        SkyChoice::Generated => "generated",
    };
    world.ledger.commit(
        scenario_fact(
            world_entity,
            facts::SKY_PROVIDER,
            Value::Text(choice_text.to_string()),
        ),
        &world.registry,
    )?;
    for pin_string in pin_strings(pins) {
        world.ledger.commit(
            scenario_fact(world_entity, facts::SCENARIO_PIN, Value::Text(pin_string)),
            &world.registry,
        )?;
    }

    if let SkyChoice::Generated = sky {
        let outcome = generate(seed, pins).map_err(BuildError::Genesis)?;
        facts::genesis(&mut world, world_entity, &outcome)?;
    }

    for pin_string in hornvale_terrain::pin_strings(terrain_pins) {
        world.ledger.commit(
            scenario_fact(
                world_entity,
                hornvale_terrain::facts::TERRAIN_PIN,
                Value::Text(pin_string),
            ),
            &world.registry,
        )?;
    }
    let terrain_outcome = hornvale_terrain::generate(seed, shared_geosphere(), terrain_pins)
        .map_err(BuildError::TerrainGenesis)?;
    hornvale_terrain::facts::genesis(&mut world, world_entity, &terrain_outcome)?;

    let vale = hornvale_terrain::genesis(&mut world)?;
    let village = hornvale_settlement::genesis(&mut world, vale)?;
    hornvale_culture::genesis(&mut world, village)?;
    let seen = observed_phenomena(&world, 0.0)?;
    hornvale_religion::genesis(&mut world, village, &seen)?;
    Ok(world)
}
```

(e) Add `terrain_of` and `land_lines` (below `sky_of`):

```rust
/// Reconstruct the tectonic terrain provider from the world's seed and its
/// committed terrain-pin facts (the `sky_of` pattern). Worlds saved before
/// terrain pins existed simply have none and regenerate with defaults. The
/// single construction site for the terrain provider.
pub fn terrain_of(world: &World) -> Result<GeneratedTerrain, BuildError> {
    let mut pins = TerrainPins::default();
    for pin_fact in world.ledger.find(hornvale_terrain::facts::TERRAIN_PIN) {
        if let Value::Text(s) = &pin_fact.object {
            hornvale_terrain::parse_pin(s, &mut pins).map_err(BuildError::Pins)?;
        }
    }
    let outcome = hornvale_terrain::generate(world.seed, shared_geosphere(), &pins)
        .map_err(BuildError::TerrainGenesis)?;
    Ok(GeneratedTerrain::new(shared_geosphere().clone(), outcome))
}

/// The land's headline lines for the almanac: plates and ocean coverage,
/// then the highest land above the sea.
pub fn land_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let terrain = terrain_of(world)?;
    let summary = hornvale_terrain::summarize(terrain.globe());
    Ok(vec![
        format!(
            "The globe breaks into {} plates; the sea claims {:.0}% of its surface.",
            summary.plate_count,
            summary.ocean_fraction * 100.0
        ),
        format!(
            "The highest land stands {:.0} m above the sea.",
            summary.highest_elevation_m - summary.sea_level_m
        ),
    ])
}
```

(f) In `almanac_context`, add the new field to the returned struct literal:

```rust
        land_lines: land_lines(world)?,
```

(g) Update the two test helpers in the same file:

```rust
    fn constant(seed: u64) -> World {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
        )
        .unwrap()
    }

    fn generated(seed: u64) -> World {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
        )
        .unwrap()
    }
```

and the two direct `build_world(Seed(42), &pins, SkyChoice::Generated)` calls in the existing pin tests (`locked_rotation_pin_yields_a_tidally_locked_calendar_line`, `two_moon_pin_yields_first_and_second_moon_lines`) become:

```rust
        let world = build_world(
            Seed(42),
            &pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
        )
        .unwrap();
```

- [ ] **Step 4: Extend the almanac window**

In `windows/almanac/src/lib.rs`:

(a) Add the field to `AlmanacContext` (after `places`):

```rust
    /// The tectonic globe's headline lines, from the composition root.
    pub land_lines: Vec<String>,
```

(b) In `render`, replace the start of the Land section so the lines lead it:

```rust
    doc.push_str("## The Land\n\n");
    for line in &ctx.land_lines {
        doc.push_str(&format!("{line}\n"));
    }
    if !ctx.land_lines.is_empty() {
        doc.push('\n');
    }
    if ctx.places.is_empty() {
```

(the rest of the section is unchanged).

(c) In the test `sample_context()`, add after `places: vec![...]`:

```rust
            land_lines: vec![
                "The globe breaks into 23 plates; the sea claims 63% of its surface.".to_string(),
            ],
```

(d) In `render_contains_every_section_and_datum`, add `"23 plates",` to the expected array.

- [ ] **Step 5: Update the remaining call sites**

(a) `windows/lab/Cargo.toml` — add to `[dependencies]`:

```toml
hornvale-terrain = { path = "../../domains/terrain" }
```

(b) `windows/lab/src/metrics.rs` — in `WorldView::build`, the first line becomes:

```rust
        let world = build_world(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
        )?;
```

(c) `cli/src/main.rs` — `cmd_new`'s build line becomes:

```rust
    let terrain_pins = hornvale_terrain::TerrainPins::default();
    let world = world_builder::build_world(Seed(seed), &pins, sky, &terrain_pins)
        .map_err(|e| e.to_string())?;
```

(the real flag parsing lands in Task 8) and `cmd_concepts`'s build becomes:

```rust
    let world = world_builder::build_world(
        Seed(0),
        &SkyPins::default(),
        world_builder::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
    )
    .map_err(|e| e.to_string())?;
```

(d) `cli/src/repl.rs` — in the test module, both `build_world` calls gain the same fourth argument `&hornvale_terrain::TerrainPins::default()` (in `drive()` and in `calendar_on_generated_world_reports_the_year`).

- [ ] **Step 6: Run the workspace tests to verify they pass**

Run: `cargo test --workspace`
Expected: PASS (including the four new worldgen tests and the updated almanac tests).

- [ ] **Step 7: Regenerate the committed generated artifacts**

The registry gained six predicates and the almanac gained Land lines, so the committed copies must be refreshed (CI drift-checks them):

```bash
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -p hornvale -- almanac --world /tmp/hv-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-locked.json
cargo run -p hornvale -- almanac --world /tmp/hv-locked.json > book/src/gallery/almanac-seed-42-locked.md
```

Expected: `git diff` shows the new predicates in the registry page and a plates/ocean line pair in each almanac's Land section.

- [ ] **Step 8: Format and commit**

```bash
cargo fmt
git add windows cli book/src/reference book/src/gallery
git commit -m "feat(worldgen): wire tectonic genesis into build_world; terrain_of reconstruction; almanac Land lines"
```

---

### Task 8: Terrain pin CLI flags and pin-isolation tests

**Files:**
- Modify: `cli/src/main.rs`
- Create: `domains/terrain/tests/tectonic_properties.rs`

**Interfaces:**
- Consumes: `hornvale_terrain::{TerrainPins, parse_pin, generate, summarize, streams, GLOBE_LEVEL}`; `flag_value` (existing CLI helper).
- Produces:
  - `fn parse_terrain_args(args: &[String]) -> Result<hornvale_terrain::TerrainPins, String>` in `cli/src/main.rs`
  - CLI flags `--plates N`, `--ocean-fraction F`, `--supercontinent true|false` on `hornvale new`
  - `domains/terrain/tests/tectonic_properties.rs` with pin-isolation and pin-error tests (the battery grows in Task 10)

- [ ] **Step 1: Write the failing pin-isolation tests**

Create `domains/terrain/tests/tectonic_properties.rs`:

```rust
//! The tectonic property battery (Campaign 3 spec §12): pin isolation here;
//! the N-seed invariant sweep joins in the final task.

use hornvale_kernel::{Geosphere, Seed};
use hornvale_terrain::{GenesisError, TerrainPins, generate, streams, summarize};

#[test]
fn pin_isolation_holds_at_the_globe_level() {
    let geo = Geosphere::new(4);
    let default = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();

    // Re-affirming the drawn plate count is byte-identical.
    let summary = summarize(&default.globe);
    let pins = TerrainPins {
        plates: Some(summary.plate_count),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);

    // Re-affirming the drawn ocean fraction (recovered by replaying its
    // labeled stream) is byte-identical.
    let drawn = 0.5
        + 0.25
            * Seed(42)
                .derive(streams::ROOT)
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let pins = TerrainPins {
        ocean_fraction: Some(drawn),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);

    // supercontinent=false re-affirms the drawn scattered layout.
    let pins = TerrainPins {
        supercontinent: Some(false),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);
}

#[test]
fn ocean_fraction_pin_perturbs_nothing_upstream() {
    let geo = Geosphere::new(4);
    let default = generate(Seed(7), &geo, &TerrainPins::default()).unwrap();
    let pinned = generate(
        Seed(7),
        &geo,
        &TerrainPins {
            ocean_fraction: Some(0.9),
            ..TerrainPins::default()
        },
    )
    .unwrap();
    assert_eq!(pinned.globe.elevation, default.globe.elevation);
    assert_eq!(pinned.globe.plate_of, default.globe.plate_of);
    assert_eq!(pinned.globe.unrest, default.globe.unrest);
    assert_ne!(pinned.globe.sea_level, default.globe.sea_level);
}

#[test]
fn out_of_range_pins_fail_loudly_with_the_reason() {
    let geo = Geosphere::new(2);
    let err = generate(
        Seed(1),
        &geo,
        &TerrainPins {
            plates: Some(200),
            ..TerrainPins::default()
        },
    )
    .unwrap_err();
    let GenesisError::InvalidPin { pin, reason } = err else {
        panic!("expected InvalidPin");
    };
    assert_eq!(pin, "plates");
    assert!(reason.contains("2-64"));
}
```

- [ ] **Step 2: Run the tests to verify they pass or fail honestly**

Run: `cargo test -p hornvale-terrain --test tectonic_properties`
Expected: PASS — these tests verify behavior already implemented in Tasks 1–5. If any FAILS, the pin-isolation contract is broken: fix the generator (a pin must never skip or reorder a draw), not the test.

- [ ] **Step 3: Write the failing CLI test**

In `cli/src/main.rs`'s `mod tests`, add:

```rust
    #[test]
    fn terrain_flags_fold_into_pins() {
        let a = args(&[
            "new",
            "--plates",
            "12",
            "--ocean-fraction",
            "0.7",
            "--supercontinent",
            "true",
        ]);
        let pins = parse_terrain_args(&a).unwrap();
        assert_eq!(pins.plates, Some(12));
        assert_eq!(pins.ocean_fraction, Some(0.7));
        assert_eq!(pins.supercontinent, Some(true));
        assert_eq!(
            parse_terrain_args(&args(&["new"])).unwrap(),
            hornvale_terrain::TerrainPins::default()
        );
    }
```

Run: `cargo test -p hornvale terrain_flags`
Expected: FAIL to compile — `error[E0425]: cannot find function 'parse_terrain_args'`.

- [ ] **Step 4: Implement the flags**

In `cli/src/main.rs`:

(a) Add below `parse_sky_args`:

```rust
/// Parse the terrain flags into pins. One parser: every flag becomes a
/// `key=value` pin string and folds through `hornvale_terrain::parse_pin`,
/// so pin-string syntax never drifts from flag syntax.
fn parse_terrain_args(args: &[String]) -> Result<hornvale_terrain::TerrainPins, String> {
    let mut pins = hornvale_terrain::TerrainPins::default();
    for (flag, key) in [
        ("--plates", "plates"),
        ("--ocean-fraction", "ocean-fraction"),
        ("--supercontinent", "supercontinent"),
    ] {
        if let Some(value) = flag_value(args, flag) {
            hornvale_terrain::parse_pin(&format!("{key}={value}"), &mut pins)?;
        }
    }
    Ok(pins)
}
```

(b) In `cmd_new`, replace `let terrain_pins = hornvale_terrain::TerrainPins::default();` (from Task 7) with:

```rust
    let terrain_pins = parse_terrain_args(args)?;
```

(c) Add the flags block and fold it into `usage()`:

```rust
const TERRAIN_FLAGS: &str = "\
  [--plates N]                             pin the plate count (2-64)
  [--ocean-fraction F]                     pin the target ocean fraction (0.05-0.95)
  [--supercontinent true|false]            cluster the continents into one landmass
";
```

```rust
fn usage() -> String {
    format!("{USAGE}{SKY_FLAGS}\nterrain flags (new only):\n{TERRAIN_FLAGS}")
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale`
Expected: PASS.

- [ ] **Step 6: Verify the flags end to end**

Run: `cargo run -p hornvale -- new --seed 42 --plates 12 --ocean-fraction 0.7 --out /tmp/hv-pinned.json && cargo run -p hornvale -- almanac --world /tmp/hv-pinned.json | grep "12 plates"`
Expected: the almanac's Land section reports "The globe breaks into 12 plates; the sea claims 70% of its surface."

Run: `cargo run -p hornvale -- new --seed 42 --plates 100 --out /tmp/hv-bad.json`
Expected: exits nonzero with `error: terrain genesis: invalid pin 'plates': 100 plates requested; the legal range is 2-64`.

- [ ] **Step 7: Format and commit**

```bash
cargo fmt
git add cli/src domains/terrain/tests
git commit -m "feat(cli): terrain pin flags --plates/--ocean-fraction/--supercontinent; pin-isolation tests"
```

---

### Task 9: The elevation-map artifact — PPM, ASCII, REPL, CI drift

**Files:**
- Create: `domains/terrain/src/render.rs`
- Modify: `domains/terrain/src/lib.rs` (add `pub mod render;`)
- Modify: `cli/src/main.rs` (`map` command + usage), `cli/src/repl.rs` (`map`, `land` commands + HELP + tests)
- Modify: `.github/workflows/ci.yml`, `book/src/SUMMARY.md`
- Create (generated): `book/src/gallery/elevation-seed-42.md`, `book/src/gallery/elevation-seed-42.ppm`

**Interfaces:**
- Consumes: `TectonicGlobe` (Task 5), `GeneratedTerrain`/`terrain_of`/`land_lines` (Tasks 6–7), `dot` (Task 2), kernel `Geosphere`/`CellId`/`GeoCoord`.
- Produces:
  - `pub fn render::elevation_ppm(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8>` — 256×128 binary P6 PPM, byte-deterministic
  - `pub fn render::elevation_ascii(geo: &Geosphere, globe: &TectonicGlobe) -> String` — 72×24 character map
  - CLI: `hornvale map [--world PATH] [--out PPM_PATH]` — markdown (title + land lines + ASCII map) to stdout, PPM to `--out`
  - REPL: `map` (ASCII map) and `land <lat> <lon>` (per-cell query)

- [ ] **Step 1: Write the failing render tests**

Create `domains/terrain/src/render.rs` containing only:

```rust
//! Deterministic map renders in the First Light tradition: an
//! equirectangular P6 PPM elevation map and an ASCII map for the REPL.
//! Same globe, same bytes — a changed artifact in review means changed
//! behavior. Pixel→cell lookup uses a latitude-band index (30 bands of 6°):
//! the nearest cell center at level ≥ 4 is within ~2.5°, so the pixel's
//! band plus both neighbors always contains it.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn ppm_is_well_formed_and_byte_deterministic() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let a = elevation_ppm(&geo, &globe);
        let b = elevation_ppm(&geo, &globe);
        assert_eq!(a, b);
        let header = format!("P6\n{} {}\n255\n", MAP_WIDTH, MAP_WIDTH / 2);
        assert!(a.starts_with(header.as_bytes()));
        assert_eq!(
            a.len(),
            header.len() + (MAP_WIDTH * (MAP_WIDTH / 2) * 3) as usize
        );
    }

    #[test]
    fn ascii_map_shows_both_ocean_and_land() {
        let geo = Geosphere::new(4);
        let globe = generate(Seed(42), &geo, &TerrainPins::default())
            .unwrap()
            .globe;
        let map = elevation_ascii(&geo, &globe);
        assert_eq!(map.lines().count(), ASCII_HEIGHT as usize);
        for line in map.lines() {
            assert_eq!(line.chars().count(), ASCII_WIDTH as usize);
        }
        assert!(map.contains('~'), "no ocean rendered");
        assert!(map.contains('.'), "no land rendered");
        assert_eq!(elevation_ascii(&geo, &globe), map);
    }

    #[test]
    fn band_index_agrees_with_brute_force_nearest() {
        let geo = Geosphere::new(4);
        let index = LatBandIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (89.0, 10.0), (-89.0, -170.0), (45.5, 179.5)] {
            let banded = index.nearest(&geo, latitude, longitude);
            let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
            let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let mut best = hornvale_kernel::CellId(0);
            let mut best_dot = f64::NEG_INFINITY;
            for cell in geo.cells() {
                let d = crate::plates::dot(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
            assert_eq!(banded, best, "at ({latitude}, {longitude})");
        }
    }
}
```

Add `pub mod render;` to `domains/terrain/src/lib.rs`.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain render`
Expected: FAIL to compile — `error[E0425]: cannot find function 'elevation_ppm'` (etc.).

- [ ] **Step 3: Write the render implementation**

Fill in `domains/terrain/src/render.rs` above the tests:

```rust
use crate::globe::TectonicGlobe;
use crate::plates::dot;
use hornvale_kernel::{CellId, Geosphere};

/// PPM image width in pixels; the image is equirectangular, so height is
/// `MAP_WIDTH / 2`.
pub const MAP_WIDTH: u32 = 256;
/// ASCII map width in characters.
pub const ASCII_WIDTH: u32 = 72;
/// ASCII map height in characters (2:1 world on ~2:1-tall glyphs).
pub const ASCII_HEIGHT: u32 = 24;

/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

/// Latitude-band index for pixel→cell lookups: cells bucketed into 30 bands
/// of 6° (built in ascending cell order, so lookups are deterministic).
/// Searching the query's band ± 1 always contains the nearest cell center
/// (which lies within ~2.5° at level 4, ~1.3° at level 5).
struct LatBandIndex {
    bands: Vec<Vec<CellId>>,
}

impl LatBandIndex {
    /// Bucket every cell of `geo` by latitude.
    fn new(geo: &Geosphere) -> LatBandIndex {
        let mut bands = vec![Vec::new(); BAND_COUNT];
        for cell in geo.cells() {
            let latitude = geo.coord(cell).latitude;
            let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
            bands[band].push(cell);
        }
        LatBandIndex { bands }
    }

    /// The cell nearest a coordinate (degrees), by maximum dot product.
    /// Inverts the kernel's coord convention: latitude = asin(z),
    /// longitude = atan2(y, x).
    fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
}

/// Color a cell by elevation relative to sea level: ocean blues deepen with
/// depth; land climbs green → tan → brown → white.
fn color(elevation_m: f64, sea_level_m: f64) -> [u8; 3] {
    fn lerp(a: [u8; 3], b: [u8; 3], t: f64) -> [u8; 3] {
        let t = t.clamp(0.0, 1.0);
        let channel =
            |a: u8, b: u8| (f64::from(a) + (f64::from(b) - f64::from(a)) * t).round() as u8;
        [
            channel(a[0], b[0]),
            channel(a[1], b[1]),
            channel(a[2], b[2]),
        ]
    }
    if elevation_m < sea_level_m {
        let depth = sea_level_m - elevation_m;
        lerp([70, 130, 200], [10, 30, 80], depth / 6000.0)
    } else {
        let height = elevation_m - sea_level_m;
        if height < 800.0 {
            lerp([60, 140, 70], [150, 160, 90], height / 800.0)
        } else if height < 2500.0 {
            lerp([150, 160, 90], [140, 100, 70], (height - 800.0) / 1700.0)
        } else {
            lerp([140, 100, 70], [245, 245, 245], (height - 2500.0) / 2500.0)
        }
    }
}

/// Render the globe as an equirectangular binary P6 PPM: longitude −180 →
/// 180 across, latitude 90 → −90 down, pixel centers sampled. Same globe,
/// same bytes.
pub fn elevation_ppm(geo: &Geosphere, globe: &TectonicGlobe) -> Vec<u8> {
    let (width, height) = (MAP_WIDTH, MAP_WIDTH / 2);
    let index = LatBandIndex::new(geo);
    let mut out = format!("P6\n{width} {height}\n255\n").into_bytes();
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            out.extend_from_slice(&color(*globe.elevation.get(cell), globe.sea_level));
        }
    }
    out
}

/// Render the globe as a 72×24 ASCII map: '~' ocean, '.' lowland, '+'
/// hills, '^' mountains, 'A' high peaks. One newline per row.
pub fn elevation_ascii(geo: &Geosphere, globe: &TectonicGlobe) -> String {
    let index = LatBandIndex::new(geo);
    let mut out = String::with_capacity(((ASCII_WIDTH + 1) * ASCII_HEIGHT) as usize);
    for py in 0..ASCII_HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(ASCII_HEIGHT) * 180.0;
        for px in 0..ASCII_WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(ASCII_WIDTH) * 360.0 - 180.0;
            let cell = index.nearest(geo, latitude, longitude);
            let height = *globe.elevation.get(cell) - globe.sea_level;
            out.push(if height < 0.0 {
                '~'
            } else if height < 500.0 {
                '.'
            } else if height < 1500.0 {
                '+'
            } else if height < 3000.0 {
                '^'
            } else {
                'A'
            });
        }
        out.push('\n');
    }
    out
}
```

Run: `cargo test -p hornvale-terrain render`
Expected: PASS (3 tests).

- [ ] **Step 4: Write the failing REPL test, then add the CLI/REPL commands**

In `cli/src/repl.rs` tests, add:

```rust
    #[test]
    fn map_and_land_answer_terrain_queries() {
        let out = drive("map\nland 45 -30\nquit\n");
        assert!(out.contains('~'), "no ascii ocean");
        assert!(out.contains("plate"), "no per-cell land report");
    }
```

Run: `cargo test -p hornvale map_and_land`
Expected: FAIL — output contains "unknown command" instead of a map.

Then, in `cli/src/repl.rs`:

(a) Add to `HELP` after the `climate` line:

```
  map              ASCII elevation map of the globe
  land <lat> <lon> the terrain at a coordinate (degrees)
```

(b) Add two match arms after the `"climate"` arm:

```rust
            "map" => match world_builder::terrain_of(world) {
                Ok(terrain) => write!(
                    output,
                    "{}",
                    hornvale_terrain::render::elevation_ascii(
                        terrain.geosphere(),
                        terrain.globe()
                    )
                )?,
                Err(e) => writeln!(output, "error: {e}")?,
            },
            "land" => {
                let coords = argument
                    .and_then(|lat| Some((lat, parts.next()?)))
                    .and_then(|(lat, lon)| {
                        Some((lat.parse::<f64>().ok()?, lon.parse::<f64>().ok()?))
                    });
                match coords {
                    None => writeln!(output, "usage: land <latitude> <longitude>")?,
                    Some((lat, lon)) => match world_builder::terrain_of(world) {
                        Ok(terrain) => {
                            let cell = terrain.nearest_cell(lat, lon);
                            let relative = terrain.elevation_at(cell) - terrain.sea_level();
                            let surface = if terrain.is_ocean(cell) {
                                format!("ocean, {:.0} m deep", -relative)
                            } else {
                                format!("land, {relative:.0} m above the sea")
                            };
                            writeln!(
                                output,
                                "cell {}: {surface}; plate {}; unrest {:.2}",
                                cell.0,
                                terrain.plate_of(cell),
                                terrain.unrest_at(cell)
                            )?;
                        }
                        Err(e) => writeln!(output, "error: {e}")?,
                    },
                }
            }
```

(c) In `cli/src/main.rs`, add the `map` command. Dispatch arm in `main()` after `"repl"`:

```rust
        Some("map") => cmd_map(&args),
```

usage line in `USAGE` after the `repl` line:

```
  hornvale map [--world <PATH>] [--out <PPM>] render the elevation map (markdown to stdout)
```

and the command itself after `cmd_repl`:

```rust
/// Render the world's elevation map: a markdown page (title, land lines,
/// ASCII map) to stdout and, with `--out`, the PPM image to disk. Both are
/// deterministic; CI drift-checks the committed copies.
fn cmd_map(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let terrain = world_builder::terrain_of(&world).map_err(|e| e.to_string())?;
    let mut doc = format!("# The Land of Seed {}\n\n", world.seed.0);
    for line in world_builder::land_lines(&world).map_err(|e| e.to_string())? {
        doc.push_str(&format!("{line}\n"));
    }
    doc.push_str("\n```text\n");
    doc.push_str(&hornvale_terrain::render::elevation_ascii(
        terrain.geosphere(),
        terrain.globe(),
    ));
    doc.push_str("```\n\n");
    if let Some(out) = flag_value(args, "--out") {
        let ppm = hornvale_terrain::render::elevation_ppm(terrain.geosphere(), terrain.globe());
        std::fs::write(out, ppm).map_err(|e| format!("writing {out}: {e}"))?;
        let name = std::path::Path::new(out)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(out);
        doc.push_str(&format!("Full-color render: [`{name}`](./{name})\n\n"));
    }
    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    print!("{doc}");
    Ok(())
}
```

Run: `cargo test -p hornvale`
Expected: PASS (including `map_and_land_answer_terrain_queries`).

- [ ] **Step 5: Generate the gallery artifact and wire the book**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -p hornvale -- map --world /tmp/hv-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
```

In `book/src/SUMMARY.md`, add after the `- [The Sky of Seed 42](./gallery/the-sky.md)` line:

```markdown
- [The Land of Seed 42](./gallery/elevation-seed-42.md)
```

Run: `mdbook build book`
Expected: builds cleanly; the new page appears in the Gallery.

- [ ] **Step 6: Extend the CI drift check**

In `.github/workflows/ci.yml`, in the "Artifacts are current (determinism check)" step, add this line directly after the `cargo run -p hornvale -- streams > ...` line (note it reuses `/tmp/hv-ci-sky.json`, created earlier in the same step):

```yaml
          cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
```

The existing `git diff --exit-code book/src/gallery/ ...` line already covers the new files.

- [ ] **Step 7: Verify artifact determinism end to end**

```bash
cargo run -p hornvale -- map --world /tmp/hv-sky.json --out /tmp/elevation-check.ppm > /tmp/elevation-check.md
cmp /tmp/elevation-check.ppm book/src/gallery/elevation-seed-42.ppm
diff /tmp/elevation-check.md book/src/gallery/elevation-seed-42.md
```

Expected: both comparisons silent (byte-identical on rerun).

- [ ] **Step 8: Format and commit**

```bash
cargo fmt
git add domains/terrain/src cli/src .github/workflows/ci.yml book/src/SUMMARY.md book/src/gallery
git commit -m "feat(terrain): deterministic elevation-map artifact (PPM + ASCII), map command, CI drift check"
```

---

### Task 10: Property battery, chronicle, and the full gate

**Files:**
- Modify: `domains/terrain/tests/tectonic_properties.rs`
- Create: `book/src/chronicle/campaign-3b.md`
- Modify: `book/src/SUMMARY.md`

**Interfaces:**
- Consumes: the whole public terrain API (`generate`, `summarize`, `plates::{generate_plates, assign_plates}`, `boundaries::classify_contact`, `streams`, `TerrainPins`).
- Produces: the merged battery; the chronicle entry; a green full gate.

- [ ] **Step 1: Write the battery (append to `domains/terrain/tests/tectonic_properties.rs`)**

```rust
#[test]
fn every_default_globe_satisfies_every_invariant() {
    let geo = Geosphere::new(4);
    for seed in 0..64u64 {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let globe = &outcome.globe;
        let plate_count = globe.plates.len() as u32;
        assert!(
            (8..=40).contains(&plate_count),
            "seed {seed}: plate count {plate_count}"
        );
        assert_eq!(globe.plate_of.len(), geo.cell_count());
        for (cell, plate) in globe.plate_of.iter() {
            assert!(
                (*plate as usize) < globe.plates.len(),
                "seed {seed}: cell {} in nonexistent plate {plate}",
                cell.0
            );
        }
        for (cell, e) in globe.elevation.iter() {
            assert!(
                e.is_finite() && (-12000.0..=12000.0).contains(e),
                "seed {seed}: cell {} elevation {e} out of envelope",
                cell.0
            );
        }
        for (_, u) in globe.unrest.iter() {
            assert!((0.0..=1.0).contains(u), "seed {seed}: unrest {u}");
        }
        let summary = summarize(globe);
        assert!(
            (0.49..=0.76).contains(&summary.ocean_fraction),
            "seed {seed}: ocean fraction {} misses the drawn range",
            summary.ocean_fraction
        );
        assert!(summary.highest_elevation_m > summary.sea_level_m);
    }
}

#[test]
fn boundary_classification_agrees_from_both_sides_across_seeds() {
    use hornvale_terrain::boundaries::classify_contact;
    use hornvale_terrain::plates::{assign_plates, generate_plates};
    let geo = Geosphere::new(3);
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let plates = generate_plates(terrain_seed, &TerrainPins::default());
        let plate_of = assign_plates(&geo, &plates);
        for a in geo.cells() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(&geo, a, b, &plates[pa as usize], &plates[pb as usize]);
                let ba = classify_contact(&geo, b, a, &plates[pb as usize], &plates[pa as usize]);
                assert_eq!(ab.kind, ba.kind, "seed {seed}: {}-{}", a.0, b.0);
                assert_eq!(ab.magnitude, ba.magnitude, "seed {seed}: {}-{}", a.0, b.0);
            }
        }
    }
}

#[test]
fn genesis_is_deterministic_across_the_sweep() {
    let geo = Geosphere::new(4);
    for seed in [0u64, 17, 42, 63] {
        let a = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        assert_eq!(a, b, "seed {seed}");
    }
}

#[test]
fn convergent_boundaries_stand_above_continental_interiors_on_average() {
    use hornvale_terrain::BoundaryKind;
    use hornvale_terrain::boundaries::{boundary_distance, boundary_field};
    use hornvale_terrain::plates::{assign_plates, generate_plates};
    let geo = Geosphere::new(4);
    let mut uplifted = Vec::new();
    let mut interior = Vec::new();
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let pins = TerrainPins::default();
        let plates = generate_plates(terrain_seed, &pins);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let globe = generate(Seed(seed), &geo, &pins).unwrap().globe;
        for (cell, contact) in boundaries.iter() {
            let continental = plates[*plate_of.get(cell) as usize].continental;
            match contact {
                Some(c)
                    if continental
                        && matches!(
                            c.kind,
                            BoundaryKind::ContinentalCollision | BoundaryKind::CoastalRange
                        ) =>
                {
                    uplifted.push(*globe.elevation.get(cell));
                }
                None => {
                    if continental {
                        if let Some((distance, _)) = distances.get(cell) {
                            if *distance >= 6 {
                                interior.push(*globe.elevation.get(cell));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }
    assert!(!uplifted.is_empty() && !interior.is_empty());
    let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len() as f64;
    assert!(
        mean(&uplifted) > mean(&interior),
        "convergent mean {} not above interior mean {}",
        mean(&uplifted),
        mean(&interior)
    );
}
```

- [ ] **Step 2: Run the battery**

Run: `cargo test -p hornvale-terrain --test tectonic_properties`
Expected: PASS (7 tests: 3 from Task 8 plus these 4). These verify already-implemented behavior; a failure is a real model defect — debug the generator, never loosen an assertion without recording why in the plan's Self-Review Notes.

- [ ] **Step 3: Write the chronicle entry**

Create `book/src/chronicle/campaign-3b.md`:

```markdown
# Campaign 3b: The Tectonic Globe

The land arrived the way the sky did: as a small set of causes, drawn once
and never revised. A world now carries a coarse planetary surface — 10,242
cells of a level-5 icosphere — partitioned into 8 to 40 Voronoi plates,
each flagged continental or oceanic, each spinning about its own Euler
pole. Where plates meet, the relative velocity's component along the
great-circle between two cells classifies the boundary: closing makes
collision ranges, coastal ranges and trenches, island arcs; opening makes
rifts and mid-ocean ridges; shear makes transform faults.

Elevation follows from geometry plus a rule table, not simulation.
Continents float at +400 m, abyssal floors sit at −4,000 m, and each
boundary's contribution decays inland by breadth-first graph distance —
shaped by a drawn per-plate *maturity*, so young belts stand sharp and
narrow while old ones lie worn and wide. A few drawn hotspots dome the
interiors. Sea level is then placed at exactly the elevation percentile
that drowns the drawn target ocean fraction. An *unrest* field — high in
young convergent zones, near zero in old interiors — is banked, unconsumed,
for future campaigns.

Nothing is serialized: a globe is re-derived from the world seed through
eight labeled streams, now in the manifest as permanent save-format
contracts. Three pins — `plates`, `ocean-fraction`, `supercontinent` —
round-trip through the ledger like every scenario pin, and the pin-isolation
battery proves a re-affirming pin yields a byte-identical globe. The gallery
gained "The Land of Seed 42", a hand-rolled PPM whose bytes are
drift-checked in CI, and the REPL now answers `map` and `land <lat> <lon>`.

The Vale still hosts the village: settlement, culture, and religion keep
running on the tier-0 seam (spec §8) until the local-refinement campaign
gives them a cell interior to stand in. Climate and biomes — the sky's
half of the thesis — are Plan 3c, which will read this globe's elevation
as an opaque `CellMap<f64>` without ever importing the terrain crate.
```

In `book/src/SUMMARY.md`, add after the `- [Campaign L0: The Laboratory](./chronicle/campaign-l0-lab.md)` line:

```markdown
- [Campaign 3b: The Tectonic Globe](./chronicle/campaign-3b.md)
```

Run: `mdbook build book`
Expected: builds cleanly with the new chronicle page.

- [ ] **Step 4: Run the full gate and the complete drift check**

```bash
cargo test --workspace
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-ci-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-ci-locked.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-locked.json > book/src/gallery/almanac-seed-42-locked.md
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
cargo run -p hornvale -- lab run studies/census-drift.study.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: every command succeeds and the final `git diff --exit-code` is silent. (The lab drift study reruns with terrain wired into `build_world`; its metrics are astronomy-only, so its published outputs must not change — if they do, that is a determinism regression in `build_world`, not a regeneration chore. Stop and debug.)

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/terrain/tests book/src
git commit -m "test(terrain): N-seed tectonic property battery; chronicle entry for campaign 3b"
```

---

## Self-Review Notes

**Spec coverage (Campaign 3 spec, terrain scope only — climate/biomes are Plan 3c by design):**
- §4 plates (count 8–40, sphere seeds, Voronoi by max dot, continental fraction) → Task 2. §4 motion (Euler pole, ω × r) → Task 2. §4 boundaries (convergent sub-typed by plate kinds / divergent rift-vs-ridge / transform, from the relative velocity's normal component) → Task 3. §4 elevation (base + boundary contribution decaying inland by flood-fill distance + hotspots + per-belt maturity young/sharp vs old/worn) → Task 4 (maturity is per-plate, the spec's allowed simplification). §4 sea level from target ocean fraction → Task 4. §4 unrest (proximity, type, inverse maturity; banked, unconsumed) → Task 4. §4 model card (drawn/derived/approximated) → doc comments on each module; declared approximations: per-cell epsilon micro-relief, arc-side-by-plate-id, static snapshot.
- §8 integration (tier-0 Vale stays; globe is additive; no Vale-pinning) → Task 7 keeps `hornvale_terrain::genesis` and the whole social cascade untouched.
- §9 pins (`--plates`, `--ocean-fraction`, `--supercontinent`; persisted as facts; round-trip; loud failure; stream isolation) → Tasks 1, 7, 8. §9 artifact (hand-rolled deterministic map, PPM + ASCII, drift-checked, gallery) → Task 9 (elevation map now; the biome map recolors it in 3c).
- §11 CI → Tasks 1, 7, 9, 10 regenerate every committed artifact and extend the drift step. §12 testing (determinism, pin isolation, tectonic battery: one-plate-per-cell, both-sides boundary agreement, elevation envelope, ocean-fraction tolerance, convergent-above-interior) → Tasks 8, 10 plus per-module tests. §14 book (chronicle, gallery, regenerated references) → Tasks 9, 10; the terrain *chapter* promotion and Census of Lands are deferred to the 3c/campaign-close plan alongside the biome map they describe. §16 risks: coarse resolution + static model keep generation O(cells); the map's taste gate is explicitly iterable (color ramp isolated in one function).
- Not in spec scope for 3b: §5–§6 (climate, biomes), §10 (Census of Lands metrics — needs biome fields), §13 openers (`Mm` rename, `HabitableZone`, `sky-epoch` — astronomy-side ride-alongs, none blocks terrain; they belong with 3c's climate work that consumes them).

**Deviations baked in deliberately (do not "fix" during execution):**
- Elevation is bare `f64` meters, not a newtype — documented waiver of the typed-quantities decision, because climate must consume `CellMap<f64>` without importing terrain (Global Constraints).
- The task prompt's "6→7 crate array" in `cli/src/streams.rs` does not apply: `hornvale-terrain` is already one of the six entries; Task 1 only adds a test assertion and regenerates the manifest.
- `GeneratedTerrain` owns a *clone* of the process-wide `OnceLock` Geosphere — per-world mesh cost is a memcpy, honoring "computed once, shared across all worlds" without a kernel change.
- Terrain pins persist under their own `terrain-pin` predicate (not astronomy's `scenario-pin`), because `sky_of` folds every `scenario-pin` fact through astronomy's `parse_pin`, which rejects unknown keys.

**Placeholder scan:** no TBD/TODO/"similar to"/"add validation" steps remain; every code step carries complete, compilable Rust; every test step carries full assertion code; every run step names the exact command and expected outcome. The two "verify" steps with conditional language (Task 2 of 8, Step 4 of 10) state the expected result and define failure as a bug to debug, not work to defer.

**Type consistency check:** `generate_plates(terrain_seed: Seed, pins: &TerrainPins) -> Vec<Plate>` (Tasks 2, 4-tests, 8, 10); `boundary_field(...) -> CellMap<Option<CellBoundary>>` and `boundary_distance(...) -> CellMap<Option<(u32, CellId)>>` (Tasks 3, 4, 5, 10); `CellBoundary { kind, magnitude, other_plate }` (Tasks 3, 4); `generate(world_seed: Seed, geosphere: &Geosphere, pins: &TerrainPins) -> Result<GenesisOutcome, GenesisError>` (Tasks 5–10); `GeneratedTerrain::new(Geosphere, GenesisOutcome)` and its accessors (Tasks 6, 7, 9); `build_world(seed, &SkyPins, SkyChoice, &TerrainPins)` at every call site (worldgen tests, lab, cli main, cli repl — Task 7); `summarize(&TectonicGlobe) -> GlobeSummary` (Tasks 5, 6, 7, 8, 10). Stream labels match between `streams.rs` constants, `stream_labels()`, and the pin-isolation test's replay (`ROOT`/`OCEAN_FRACTION`).
