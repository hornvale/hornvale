# Campaign 1a: The Kernel — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `hornvale-kernel` — the seed/noise substrate, field infrastructure, concept registry, fact ledger, trivial refinement engine, and trace-protocol (phenomena) vocabulary that every domain crate will depend on.

**Architecture:** A single pure-library crate at `kernel/` in a fresh Cargo workspace. No I/O except world save/load. All randomness flows from a hierarchical `Seed`; all cross-domain communication types (facts, fields, phenomena) live here. Companion plan "Campaign 1b: The Tier-0 Cascade & Windows" will be written after this plan lands and consumes these exact interfaces.

**Tech Stack:** Rust edition 2024 (toolchain 1.96+). Dependencies: `serde` (derive) and `serde_json` only.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`. Constitution §2 governs; especially: determinism is constitutional; coarse constrains fine; domains depend only on the kernel.
- **No other dependencies.** No `rand`, no `chrono`, no `thiserror`, no `noise` crate. Randomness and noise are implemented in-crate so we control determinism forever.
- **No wall-clock, ever.** `std::time`, `SystemTime::now()` are forbidden in kernel and domain code. All time is simulated (`WorldTime`).
- **Determinism:** any function of (seed, coordinates, ledger) must return identical results on every call, platform, and run. No `HashMap` iteration order leaks into output — use `BTreeMap`/`Vec` for anything ordered or serialized.
- **Naming is deferred** (spec §7): all names below are descriptive placeholders; do not invent cute names.
- Every commit compiles, passes `cargo test --workspace`, and is clean under `cargo fmt --check` and `cargo clippy --workspace -- -D warnings`.
- Public items get a one-line doc comment. Comments state constraints, not narration.

## File Structure

```
Cargo.toml                    — workspace root (members: kernel; 1b adds domains/*, windows/*, cli)
kernel/Cargo.toml             — package hornvale-kernel; deps: serde (derive), serde_json
kernel/src/lib.rs             — module declarations + re-exports
kernel/src/seed.rs            — Seed, Stream: hierarchical seeding, deterministic RNG
kernel/src/noise.rs           — lattice hash, value_noise_2d, fbm_2d
kernel/src/field.rs           — Position, WorldTime, Field trait, ConstantField, NoiseField
kernel/src/registry.rs        — PredicateDef, ConceptRegistry, RegistryError
kernel/src/ledger.rs          — EntityId, Value, Fact, Ledger, LedgerError
kernel/src/refine.rs          — choose_consistent (trivial refinement engine)
kernel/src/phenomena.rs       — Phenomenon, ObserverContext, PhenomenaSource, observe
kernel/src/world.rs           — World (seed + registry + ledger), core concepts, save/load
kernel/tests/determinism.rs   — cross-module determinism integration test
```

---

### Task 1: Workspace scaffold + seed substrate

**Files:**
- Create: `Cargo.toml` (workspace root)
- Create: `kernel/Cargo.toml`
- Create: `kernel/src/lib.rs`
- Create: `kernel/src/seed.rs` (tests inline in module)

**Interfaces:**
- Consumes: nothing (first task).
- Produces: `Seed(pub u64)` with `derive(&self, label: &str) -> Seed` and `stream(&self) -> Stream`; `Stream` with `next_u64(&mut self) -> u64`, `next_f64(&mut self) -> f64` (in `[0,1)`), `range_u32(&mut self, lo: u32, hi: u32) -> u32` (inclusive bounds), `pick<'a, T>(&mut self, items: &'a [T]) -> Option<&'a T>`. All later tasks derive randomness exclusively from these.

- [ ] **Step 1: Create the workspace and crate manifests**

`Cargo.toml` (root):

```toml
[workspace]
resolver = "3"
members = ["kernel"]

[workspace.package]
edition = "2024"
license = "Unlicense"

[workspace.dependencies]
serde = { version = "1", features = ["derive"] }
serde_json = "1"
```

`kernel/Cargo.toml`:

```toml
[package]
name = "hornvale-kernel"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale kernel: seeds, noise, fields, facts, refinement, phenomena."

[dependencies]
serde = { workspace = true }
serde_json = { workspace = true }
```

`kernel/src/lib.rs`:

```rust
//! Hornvale kernel: the substrate every domain depends on — and the only
//! thing any domain may depend on (Constitution §2.6).

pub mod seed;

pub use seed::{Seed, Stream};
```

- [ ] **Step 2: Write the failing tests**

`kernel/src/seed.rs`:

```rust
//! Hierarchical seeding and deterministic random streams.

use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derive_is_deterministic() {
        assert_eq!(Seed(42).derive("astronomy"), Seed(42).derive("astronomy"));
    }

    #[test]
    fn derive_differs_by_label() {
        assert_ne!(Seed(42).derive("astronomy"), Seed(42).derive("climate"));
    }

    #[test]
    fn derive_differs_by_parent() {
        assert_ne!(Seed(42).derive("astronomy"), Seed(43).derive("astronomy"));
    }

    #[test]
    fn derive_chains_compose() {
        let a = Seed(7).derive("settlement").derive("name");
        let b = Seed(7).derive("settlement").derive("name");
        assert_eq!(a, b);
    }

    #[test]
    fn stream_is_deterministic() {
        let mut a = Seed(9).stream();
        let mut b = Seed(9).stream();
        assert_eq!(
            [a.next_u64(), a.next_u64(), a.next_u64()],
            [b.next_u64(), b.next_u64(), b.next_u64()]
        );
    }

    #[test]
    fn next_f64_is_in_unit_interval() {
        let mut s = Seed(1).stream();
        for _ in 0..1000 {
            let x = s.next_f64();
            assert!((0.0..1.0).contains(&x));
        }
    }

    #[test]
    fn range_u32_is_inclusive_and_bounded() {
        let mut s = Seed(2).stream();
        let mut saw_lo = false;
        let mut saw_hi = false;
        for _ in 0..2000 {
            let x = s.range_u32(3, 5);
            assert!((3..=5).contains(&x));
            saw_lo |= x == 3;
            saw_hi |= x == 5;
        }
        assert!(saw_lo && saw_hi);
    }

    #[test]
    fn pick_returns_none_on_empty() {
        let mut s = Seed(3).stream();
        let empty: [u8; 0] = [];
        assert_eq!(s.pick(&empty), None);
    }

    #[test]
    fn pick_returns_an_element() {
        let mut s = Seed(3).stream();
        let items = ["a", "b", "c"];
        assert!(items.contains(s.pick(&items).unwrap()));
    }

    #[test]
    fn seed_serializes_roundtrip() {
        let s = Seed(42).derive("x");
        let json = serde_json::to_string(&s).unwrap();
        assert_eq!(serde_json::from_str::<Seed>(&json).unwrap(), s);
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-kernel`
Expected: compile error — `Seed` not defined.

- [ ] **Step 4: Implement Seed and Stream**

Add above the `tests` module in `kernel/src/seed.rs`:

```rust
/// A deterministic seed. The world seed is the root; everything else is
/// derived from it by labeled paths, so adding a new consumer never
/// perturbs existing streams.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Seed(pub u64);

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

impl Seed {
    /// Derive a child seed for `label`. FNV-1a over the label, mixed with
    /// the parent, then scrambled. Stable forever: changing this breaks
    /// every saved world.
    pub fn derive(&self, label: &str) -> Seed {
        let mut h = FNV_OFFSET ^ self.0;
        for byte in label.as_bytes() {
            h ^= u64::from(*byte);
            h = h.wrapping_mul(FNV_PRIME);
        }
        Seed(splitmix64(&mut h))
    }

    /// A fresh deterministic random stream rooted at this seed.
    pub fn stream(&self) -> Stream {
        Stream { state: self.0 }
    }
}

/// A splitmix64 random stream. Not serialized; always re-derived from a Seed.
#[derive(Clone, Debug)]
pub struct Stream {
    state: u64,
}

impl Stream {
    pub fn next_u64(&mut self) -> u64 {
        splitmix64(&mut self.state)
    }

    /// Uniform in [0, 1), using the top 53 bits.
    pub fn next_f64(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }

    /// Uniform in [lo, hi], inclusive. Panics if lo > hi.
    pub fn range_u32(&mut self, lo: u32, hi: u32) -> u32 {
        assert!(lo <= hi, "range_u32: lo > hi");
        let span = u64::from(hi - lo) + 1;
        lo + (self.next_u64() % span) as u32
    }

    /// Deterministically pick one element; None if empty.
    pub fn pick<'a, T>(&mut self, items: &'a [T]) -> Option<&'a T> {
        if items.is_empty() {
            return None;
        }
        let i = (self.next_u64() % items.len() as u64) as usize;
        Some(&items[i])
    }
}
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel`
Expected: all 10 tests PASS.

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add Cargo.toml kernel/
git commit -m "feat(kernel): workspace scaffold and seed substrate

Hierarchical labeled seed derivation (FNV-1a + splitmix64) and
deterministic streams. The only source of randomness in Hornvale."
```

---

### Task 2: Noise primitives

**Files:**
- Create: `kernel/src/noise.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: `Seed` from Task 1.
- Produces: `value_noise_2d(seed: Seed, x: f64, y: f64) -> f64` and `fbm_2d(seed: Seed, x: f64, y: f64, octaves: u32) -> f64`, both returning values in `[0, 1)`, random-access (no neighbor evaluation), deterministic.

- [ ] **Step 1: Write the failing tests**

`kernel/src/noise.rs`:

```rust
//! Coherent noise with random access: deterministic, evaluable at any
//! point without evaluating neighbors, locally coherent.

use crate::seed::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_noise_is_deterministic() {
        let s = Seed(42);
        assert_eq!(value_noise_2d(s, 3.7, -2.1), value_noise_2d(s, 3.7, -2.1));
    }

    #[test]
    fn value_noise_differs_by_seed() {
        assert_ne!(
            value_noise_2d(Seed(1), 3.7, 2.1),
            value_noise_2d(Seed(2), 3.7, 2.1)
        );
    }

    #[test]
    fn value_noise_is_in_unit_interval() {
        let s = Seed(7);
        for i in 0..50 {
            for j in 0..50 {
                let v = value_noise_2d(s, f64::from(i) * 0.37 - 9.0, f64::from(j) * 0.53 - 13.0);
                assert!((0.0..1.0).contains(&v), "out of range: {v}");
            }
        }
    }

    #[test]
    fn value_noise_is_locally_coherent() {
        // Nearby samples differ by less than distant samples do on average.
        let s = Seed(11);
        let base = value_noise_2d(s, 10.25, 10.25);
        let near = value_noise_2d(s, 10.26, 10.25);
        assert!((base - near).abs() < 0.05);
    }

    #[test]
    fn fbm_is_deterministic_and_bounded() {
        let s = Seed(5);
        let a = fbm_2d(s, 1.5, 2.5, 4);
        assert_eq!(a, fbm_2d(s, 1.5, 2.5, 4));
        assert!((0.0..1.0).contains(&a));
    }

    #[test]
    fn fbm_with_one_octave_matches_value_noise() {
        let s = Seed(5);
        assert_eq!(fbm_2d(s, 1.5, 2.5, 1), value_noise_2d(s, 1.5, 2.5));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-kernel noise`
Expected: compile error — module not declared / functions not defined. (Add `pub mod noise;` and `pub use noise::{fbm_2d, value_noise_2d};` to `lib.rs` now; the functions still don't exist, so it fails to compile.)

- [ ] **Step 3: Implement the noise functions**

Add above the `tests` module in `kernel/src/noise.rs`:

```rust
/// Hash a lattice point to [0, 1). Stable forever (save-format contract).
fn lattice(seed: Seed, xi: i64, yi: i64) -> f64 {
    let mut h = seed.0
        ^ (xi as u64).wrapping_mul(0x9e37_79b9_7f4a_7c15)
        ^ (yi as u64).wrapping_mul(0xc2b2_ae3d_27d4_eb4f);
    h = (h ^ (h >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    h = (h ^ (h >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    h ^= h >> 31;
    (h >> 11) as f64 / (1u64 << 53) as f64
}

fn smoothstep(t: f64) -> f64 {
    t * t * (3.0 - 2.0 * t)
}

/// Bilinear value noise in [0, 1). Random access: cost is O(1) per sample.
pub fn value_noise_2d(seed: Seed, x: f64, y: f64) -> f64 {
    let x0 = x.floor();
    let y0 = y.floor();
    let (xi, yi) = (x0 as i64, y0 as i64);
    let tx = smoothstep(x - x0);
    let ty = smoothstep(y - y0);
    let v00 = lattice(seed, xi, yi);
    let v10 = lattice(seed, xi + 1, yi);
    let v01 = lattice(seed, xi, yi + 1);
    let v11 = lattice(seed, xi + 1, yi + 1);
    let top = v00 + (v10 - v00) * tx;
    let bot = v01 + (v11 - v01) * tx;
    top + (bot - top) * ty
}

/// Fractal Brownian motion over `value_noise_2d`, normalized to [0, 1).
/// Gain 0.5, lacunarity 2.0. Octave 0 uses the seed directly so that
/// one-octave fbm equals plain value noise. Panics if octaves == 0.
pub fn fbm_2d(seed: Seed, x: f64, y: f64, octaves: u32) -> f64 {
    assert!(octaves > 0, "fbm_2d: octaves must be >= 1");
    let mut sum = 0.0;
    let mut amplitude = 1.0;
    let mut frequency = 1.0;
    let mut max = 0.0;
    for octave in 0..octaves {
        let s = if octave == 0 {
            seed
        } else {
            seed.derive(&format!("octave-{octave}"))
        };
        sum += amplitude * value_noise_2d(s, x * frequency, y * frequency);
        max += amplitude;
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    sum / max
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel noise`
Expected: all 6 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): value noise and fbm with random access"
```

---

### Task 3: Field infrastructure

**Files:**
- Create: `kernel/src/field.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: `Seed`, `fbm_2d` from Tasks 1–2.
- Produces: `Position { x: f64, y: f64 }`, `WorldTime { day: f64 }`, `trait Field<T> { fn sample(&self, pos: Position, time: WorldTime) -> T }`, `ConstantField<T: Clone>(pub T)`, `NoiseField { seed: Seed, octaves: u32, scale: f64 }` (implements `Field<f64>`). Domains implement `Field<T>` for their own types; consumers read fields without knowing contributors.

- [ ] **Step 1: Write the failing tests**

`kernel/src/field.rs`:

```rust
//! Fields: typed, lazily-evaluated functions over (space × time).
//! Fields are the "coarse" in coarse-constrains-fine (Constitution §2.2).

use crate::noise::fbm_2d;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;

    const NOON: WorldTime = WorldTime { day: 0.5 };

    #[test]
    fn constant_field_returns_its_value_everywhere() {
        let f = ConstantField(18.0_f64);
        assert_eq!(f.sample(Position { x: 0.0, y: 0.0 }, NOON), 18.0);
        assert_eq!(f.sample(Position { x: 1e6, y: -1e6 }, NOON), 18.0);
    }

    #[test]
    fn constant_field_works_for_non_numeric_types() {
        let f = ConstantField("temperate forest".to_string());
        assert_eq!(
            f.sample(Position { x: 3.0, y: 4.0 }, NOON),
            "temperate forest"
        );
    }

    #[test]
    fn noise_field_is_deterministic_and_bounded() {
        let f = NoiseField { seed: Seed(42), octaves: 3, scale: 10.0 };
        let p = Position { x: 12.5, y: -7.25 };
        let a = f.sample(p, NOON);
        assert_eq!(a, f.sample(p, NOON));
        assert!((0.0..1.0).contains(&a));
    }

    #[test]
    fn noise_field_scale_stretches_space() {
        // Two points one unit apart are nearly identical under a huge scale.
        let f = NoiseField { seed: Seed(42), octaves: 1, scale: 1000.0 };
        let a = f.sample(Position { x: 0.0, y: 0.0 }, NOON);
        let b = f.sample(Position { x: 1.0, y: 0.0 }, NOON);
        assert!((a - b).abs() < 0.01);
    }

    #[test]
    fn position_and_time_serialize_roundtrip() {
        let p = Position { x: 1.5, y: -2.5 };
        let t = WorldTime { day: 12.25 };
        let p2: Position = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();
        let t2: WorldTime = serde_json::from_str(&serde_json::to_string(&t).unwrap()).unwrap();
        assert_eq!((p2.x, p2.y, t2.day), (p.x, p.y, t.day));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod field;` and `pub use field::{ConstantField, Field, NoiseField, Position, WorldTime};`

Run: `cargo test -p hornvale-kernel field`
Expected: compile error — types not defined.

- [ ] **Step 3: Implement the field types**

Add above the `tests` module in `kernel/src/field.rs`:

```rust
/// A location in world space. Units and topology are a terrain-domain
/// concern; the kernel only requires a metric-ish plane.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub x: f64,
    pub y: f64,
}

/// Simulated time in fractional days since world genesis. There is no
/// wall-clock time anywhere in Hornvale.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct WorldTime {
    pub day: f64,
}

/// A typed field over (space × time). Implementations must be pure:
/// same (pos, time) → same value, always.
pub trait Field<T> {
    fn sample(&self, pos: Position, time: WorldTime) -> T;
}

/// The tier-0 field: the same value everywhere, forever.
#[derive(Clone, Debug)]
pub struct ConstantField<T: Clone>(pub T);

impl<T: Clone> Field<T> for ConstantField<T> {
    fn sample(&self, _pos: Position, _time: WorldTime) -> T {
        self.0.clone()
    }
}

/// A time-invariant fbm noise field in [0, 1). `scale` is the feature
/// wavelength in world units.
#[derive(Clone, Copy, Debug)]
pub struct NoiseField {
    pub seed: Seed,
    pub octaves: u32,
    pub scale: f64,
}

impl Field<f64> for NoiseField {
    fn sample(&self, pos: Position, _time: WorldTime) -> f64 {
        fbm_2d(self.seed, pos.x / self.scale, pos.y / self.scale, self.octaves)
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel field`
Expected: all 5 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): field infrastructure (Position, WorldTime, Field, Constant/NoiseField)"
```

---

### Task 4: Concept registry

**Files:**
- Create: `kernel/src/registry.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `PredicateDef { name: String, functional: bool, doc: String }`, `ConceptRegistry` with `register_predicate(&mut self, name: &str, functional: bool, doc: &str) -> Result<(), RegistryError>`, `predicate(&self, name: &str) -> Option<&PredicateDef>`, `register_phenomenon_kind(&mut self, name: &str, doc: &str) -> Result<(), RegistryError>`, `phenomenon_kind(&self, name: &str) -> Option<&str>`. Registration is idempotent when the definition is identical; conflicting redefinition is an error. The ledger (Task 5) checks every fact's predicate against this.

- [ ] **Step 1: Write the failing tests**

`kernel/src/registry.rs`:

```rust
//! The concept registry: the negotiated vocabulary boundary between
//! domains (spec §3.1.6). The envelope stays dumb; meaning lives here,
//! reviewed at every campaign close.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registered_predicate_is_retrievable() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let def = r.predicate("name").unwrap();
        assert!(def.functional);
    }

    #[test]
    fn unknown_predicate_is_none() {
        let r = ConceptRegistry::default();
        assert!(r.predicate("nope").is_none());
    }

    #[test]
    fn identical_reregistration_is_idempotent() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        assert!(
            r.register_predicate("name", true, "canonical name of an entity")
                .is_ok()
        );
    }

    #[test]
    fn conflicting_reregistration_is_an_error() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let err = r.register_predicate("name", false, "canonical name of an entity");
        assert!(matches!(
            err,
            Err(RegistryError::ConflictingDefinition { .. })
        ));
    }

    #[test]
    fn phenomenon_kinds_register_and_conflict_like_predicates() {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("celestial-body", "a body visible in the sky")
            .unwrap();
        assert_eq!(
            r.phenomenon_kind("celestial-body"),
            Some("a body visible in the sky")
        );
        assert!(
            r.register_phenomenon_kind("celestial-body", "something else")
                .is_err()
        );
    }

    #[test]
    fn registry_serializes_roundtrip() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        let json = serde_json::to_string(&r).unwrap();
        let r2: ConceptRegistry = serde_json::from_str(&json).unwrap();
        assert!(r2.predicate("name").unwrap().functional);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod registry;` and `pub use registry::{ConceptRegistry, PredicateDef, RegistryError};`

Run: `cargo test -p hornvale-kernel registry`
Expected: compile error — types not defined.

- [ ] **Step 3: Implement the registry**

Add above the `tests` module in `kernel/src/registry.rs`:

```rust
/// Definition of a fact predicate. `functional` means a subject may hold
/// at most one distinct object under this predicate — the contradiction
/// rule the ledger enforces.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PredicateDef {
    pub name: String,
    pub functional: bool,
    pub doc: String,
}

#[derive(Debug)]
pub enum RegistryError {
    ConflictingDefinition { name: String },
}

impl std::fmt::Display for RegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryError::ConflictingDefinition { name } => {
                write!(f, "conflicting redefinition of concept '{name}'")
            }
        }
    }
}

impl std::error::Error for RegistryError {}

/// The growing, reviewed vocabulary: predicates and phenomenon kinds.
/// BTreeMaps keep serialization order deterministic.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ConceptRegistry {
    predicates: BTreeMap<String, PredicateDef>,
    phenomenon_kinds: BTreeMap<String, String>,
}

impl ConceptRegistry {
    /// Idempotent for identical definitions; errors on conflict.
    pub fn register_predicate(
        &mut self,
        name: &str,
        functional: bool,
        doc: &str,
    ) -> Result<(), RegistryError> {
        let def = PredicateDef {
            name: name.to_string(),
            functional,
            doc: doc.to_string(),
        };
        match self.predicates.get(name) {
            Some(existing) if *existing == def => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: name.to_string(),
            }),
            None => {
                self.predicates.insert(name.to_string(), def);
                Ok(())
            }
        }
    }

    pub fn predicate(&self, name: &str) -> Option<&PredicateDef> {
        self.predicates.get(name)
    }

    /// Idempotent for identical docs; errors on conflict.
    pub fn register_phenomenon_kind(
        &mut self,
        name: &str,
        doc: &str,
    ) -> Result<(), RegistryError> {
        match self.phenomenon_kinds.get(name) {
            Some(existing) if existing == doc => Ok(()),
            Some(_) => Err(RegistryError::ConflictingDefinition {
                name: name.to_string(),
            }),
            None => {
                self.phenomenon_kinds.insert(name.to_string(), doc.to_string());
                Ok(())
            }
        }
    }

    pub fn phenomenon_kind(&self, name: &str) -> Option<&str> {
        self.phenomenon_kinds.get(name).map(String::as_str)
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel registry`
Expected: all 6 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): concept registry with idempotent registration and conflict errors"
```

---

### Task 5: Fact ledger

**Files:**
- Create: `kernel/src/ledger.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: `ConceptRegistry` (Task 4).
- Produces: `EntityId(pub u64)`; `Value` enum (`Entity(EntityId)`, `Text(String)`, `Number(f64)`, `Flag(bool)`); `Fact { subject: EntityId, predicate: String, object: Value, place: Option<EntityId>, day: Option<f64>, provenance: String }`; `Ledger` with `mint_entity(&mut self) -> EntityId`, `check(&self, fact: &Fact, registry: &ConceptRegistry) -> Result<(), LedgerError>`, `commit(&mut self, fact: Fact, registry: &ConceptRegistry) -> Result<bool, LedgerError>` (Ok(false) = idempotent duplicate), `facts_about(&self, subject: EntityId) -> impl Iterator<Item = &Fact>`, `find(&self, predicate: &str) -> impl Iterator<Item = &Fact>`, `value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value>`, `len(&self) -> usize`, `is_empty(&self) -> bool`. Refinement (Task 6) uses `check`; every domain genesis commits through `commit`.

- [ ] **Step 1: Write the failing tests**

`kernel/src/ledger.rs`:

```rust
//! The fact ledger: the append-only posterior. Once committed, a fact is
//! true forever (spec §3.1, §3.3). The envelope is deliberately dumb;
//! predicates carry meaning via the concept registry.

use crate::registry::ConceptRegistry;
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;

    fn registry() -> ConceptRegistry {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name").unwrap();
        r.register_predicate("located-in", false, "spatial containment")
            .unwrap();
        r
    }

    fn named(ledger: &mut Ledger, name: &str) -> Fact {
        let e = ledger.mint_entity();
        Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Text(name.to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        }
    }

    #[test]
    fn mint_entity_yields_distinct_ids() {
        let mut l = Ledger::default();
        assert_ne!(l.mint_entity(), l.mint_entity());
    }

    #[test]
    fn commit_and_query_roundtrip() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        assert!(l.commit(f, &r).unwrap());
        assert_eq!(
            l.value_of(subject, "name"),
            Some(&Value::Text("Zaggrak".to_string()))
        );
        assert_eq!(l.facts_about(subject).count(), 1);
        assert_eq!(l.find("name").count(), 1);
    }

    #[test]
    fn unknown_predicate_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let f = Fact {
            subject: e,
            predicate: "unregistered".to_string(),
            object: Value::Flag(true),
            place: None,
            day: None,
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(f, &r),
            Err(LedgerError::UnknownPredicate { .. })
        ));
    }

    #[test]
    fn functional_contradiction_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        l.commit(f, &r).unwrap();
        let contradiction = Fact {
            subject,
            predicate: "name".to_string(),
            object: Value::Text("Bolnar".to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(contradiction, &r),
            Err(LedgerError::Contradiction { .. })
        ));
    }

    #[test]
    fn identical_recommit_is_idempotent() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        l.commit(f.clone(), &r).unwrap();
        assert!(!l.commit(f, &r).unwrap());
        assert_eq!(l.len(), 1);
    }

    #[test]
    fn non_functional_predicate_allows_multiple_objects() {
        let r = registry();
        let mut l = Ledger::default();
        let village = l.mint_entity();
        let vale = l.mint_entity();
        let forest = l.mint_entity();
        for container in [vale, forest] {
            l.commit(
                Fact {
                    subject: village,
                    predicate: "located-in".to_string(),
                    object: Value::Entity(container),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &r,
            )
            .unwrap();
        }
        assert_eq!(l.facts_about(village).count(), 2);
    }

    #[test]
    fn ledger_serializes_roundtrip_including_minting_state() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        l.commit(f, &r).unwrap();
        let json = serde_json::to_string(&l).unwrap();
        let mut l2: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(l2.len(), 1);
        // Minting must resume without colliding with existing entities.
        let fresh = l2.mint_entity();
        assert!(l2.facts_about(fresh).count() == 0);
        assert!(fresh.0 > 1);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod ledger;` and `pub use ledger::{EntityId, Fact, Ledger, LedgerError, Value};`

Run: `cargo test -p hornvale-kernel ledger`
Expected: compile error — types not defined.

- [ ] **Step 3: Implement the ledger**

Add above the `tests` module in `kernel/src/ledger.rs`:

```rust
/// Opaque entity handle. Minted by the ledger, never reused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EntityId(pub u64);

/// A fact's object. Number equality is bitwise-exact f64 equality —
/// acceptable because all values are deterministic (tier-0 contract).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Entity(EntityId),
    Text(String),
    Number(f64),
    Flag(bool),
}

/// The dumb envelope (spec §3.1.6): subject, predicate, object, place,
/// time, provenance. Semantics live in the concept registry.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Fact {
    pub subject: EntityId,
    pub predicate: String,
    pub object: Value,
    pub place: Option<EntityId>,
    pub day: Option<f64>,
    pub provenance: String,
}

#[derive(Debug)]
pub enum LedgerError {
    UnknownPredicate { predicate: String },
    Contradiction { subject: EntityId, predicate: String },
}

impl std::fmt::Display for LedgerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LedgerError::UnknownPredicate { predicate } => {
                write!(f, "predicate '{predicate}' is not in the concept registry")
            }
            LedgerError::Contradiction { subject, predicate } => write!(
                f,
                "contradiction: entity {} already holds a different '{predicate}'",
                subject.0
            ),
        }
    }
}

impl std::error::Error for LedgerError {}

/// Append-only fact store. Facts are never mutated or removed.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Ledger {
    facts: Vec<Fact>,
    next_entity: u64,
}

impl Ledger {
    /// Mint a fresh entity id. Ids start at 1; 0 is reserved as "never valid".
    pub fn mint_entity(&mut self) -> EntityId {
        self.next_entity += 1;
        EntityId(self.next_entity)
    }

    /// Would this fact be accepted? Used by the refinement engine to test
    /// candidates without committing.
    pub fn check(&self, fact: &Fact, registry: &ConceptRegistry) -> Result<(), LedgerError> {
        let def = registry
            .predicate(&fact.predicate)
            .ok_or_else(|| LedgerError::UnknownPredicate {
                predicate: fact.predicate.clone(),
            })?;
        if def.functional {
            let clash = self
                .facts
                .iter()
                .any(|f| {
                    f.subject == fact.subject
                        && f.predicate == fact.predicate
                        && f.object != fact.object
                });
            if clash {
                return Err(LedgerError::Contradiction {
                    subject: fact.subject,
                    predicate: fact.predicate.clone(),
                });
            }
        }
        Ok(())
    }

    /// Commit a fact. Ok(true) = appended; Ok(false) = identical fact
    /// already present (idempotent no-op).
    pub fn commit(
        &mut self,
        fact: Fact,
        registry: &ConceptRegistry,
    ) -> Result<bool, LedgerError> {
        self.check(&fact, registry)?;
        if self.facts.contains(&fact) {
            return Ok(false);
        }
        self.facts.push(fact);
        Ok(true)
    }

    pub fn facts_about(&self, subject: EntityId) -> impl Iterator<Item = &Fact> {
        self.facts.iter().filter(move |f| f.subject == subject)
    }

    pub fn find<'a>(&'a self, predicate: &str) -> impl Iterator<Item = &'a Fact> + use<'a> {
        let predicate = predicate.to_string();
        self.facts.iter().filter(move |f| f.predicate == predicate)
    }

    /// First object for (subject, predicate). For functional predicates
    /// this is the unique value.
    pub fn value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value> {
        self.facts
            .iter()
            .find(|f| f.subject == subject && f.predicate == predicate)
            .map(|f| &f.object)
    }

    pub fn len(&self) -> usize {
        self.facts.len()
    }

    pub fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel ledger`
Expected: all 7 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): append-only fact ledger with contradiction checking

Dumb envelope per the trace protocol; functional-predicate
contradictions rejected; commits idempotent; minting state persists."
```

---

### Task 6: Trivial refinement engine

**Files:**
- Create: `kernel/src/refine.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: `Stream` (Task 1), `Ledger`, `Fact` (Task 5), `ConceptRegistry` (Task 4).
- Produces: `choose_consistent<T>(stream: &mut Stream, ledger: &Ledger, registry: &ConceptRegistry, candidates: &[T], to_fact: impl Fn(&T) -> Fact) -> Option<usize>` — deterministically picks the index of a candidate whose fact would commit without contradiction; None if no candidate survives. This is refinement tier 0: "choose among options that contradict nothing" (spec §3.1). Domain geneses use it for every choice that must respect committed facts.

- [ ] **Step 1: Write the failing tests**

`kernel/src/refine.rs`:

```rust
//! Refinement engine, tier 0: pick deterministically among candidates
//! that contradict nothing (spec §3.1). This function's sophistication
//! grows for years; its signature shape should not.

use crate::ledger::{Fact, Ledger};
use crate::registry::ConceptRegistry;
use crate::seed::Stream;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{EntityId, Value};
    use crate::seed::Seed;

    fn setup() -> (ConceptRegistry, Ledger, EntityId) {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name").unwrap();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        (r, l, e)
    }

    fn name_fact(subject: EntityId, name: &str) -> Fact {
        Fact {
            subject,
            predicate: "name".to_string(),
            object: Value::Text(name.to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        }
    }

    #[test]
    fn choice_is_deterministic() {
        let (r, l, e) = setup();
        let candidates = ["Zaggrak", "Bolnar", "Mokru"];
        let a = choose_consistent(&mut Seed(5).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        let b = choose_consistent(&mut Seed(5).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(a, b);
        assert!(a.is_some());
    }

    #[test]
    fn choice_varies_with_seed() {
        let (r, l, e) = setup();
        let candidates = ["a", "b", "c", "d", "e", "f", "g", "h"];
        let picks: Vec<Option<usize>> = (0..8)
            .map(|s| {
                choose_consistent(&mut Seed(s).stream(), &l, &r, &candidates, |n| {
                    name_fact(e, n)
                })
            })
            .collect();
        assert!(picks.windows(2).any(|w| w[0] != w[1]));
    }

    #[test]
    fn contradicting_candidates_are_skipped() {
        let (r, mut l, e) = setup();
        l.commit(name_fact(e, "Zaggrak"), &r).unwrap();
        // Committing a *different* name would contradict; only the
        // already-true name survives.
        let candidates = ["Bolnar", "Zaggrak", "Mokru"];
        for seed in 0..16 {
            let pick = choose_consistent(&mut Seed(seed).stream(), &l, &r, &candidates, |n| {
                name_fact(e, n)
            });
            assert_eq!(pick, Some(1));
        }
    }

    #[test]
    fn returns_none_when_nothing_survives() {
        let (r, mut l, e) = setup();
        l.commit(name_fact(e, "Zaggrak"), &r).unwrap();
        let candidates = ["Bolnar", "Mokru"];
        let pick = choose_consistent(&mut Seed(1).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(pick, None);
    }

    #[test]
    fn empty_candidates_yield_none() {
        let (r, l, e) = setup();
        let candidates: [&str; 0] = [];
        let pick = choose_consistent(&mut Seed(1).stream(), &l, &r, &candidates, |n| {
            name_fact(e, n)
        });
        assert_eq!(pick, None);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod refine;` and `pub use refine::choose_consistent;`

Run: `cargo test -p hornvale-kernel refine`
Expected: compile error — function not defined.

- [ ] **Step 3: Implement choose_consistent**

Add above the `tests` module in `kernel/src/refine.rs`:

```rust
/// Deterministically choose the index of a candidate whose fact would be
/// accepted by the ledger. Starts at a seeded offset and scans in order,
/// so the choice is uniform when unconstrained and consistent when
/// constrained. Returns None if no candidate survives.
pub fn choose_consistent<T>(
    stream: &mut Stream,
    ledger: &Ledger,
    registry: &ConceptRegistry,
    candidates: &[T],
    to_fact: impl Fn(&T) -> Fact,
) -> Option<usize> {
    if candidates.is_empty() {
        return None;
    }
    let offset = (stream.next_u64() % candidates.len() as u64) as usize;
    (0..candidates.len())
        .map(|i| (offset + i) % candidates.len())
        .find(|&i| ledger.check(&to_fact(&candidates[i]), registry).is_ok())
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel refine`
Expected: all 5 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): trivial refinement engine (choose_consistent)"
```

---

### Task 7: Phenomena vocabulary

**Files:**
- Create: `kernel/src/phenomena.rs` (tests inline)
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: `EntityId` (Task 5), `WorldTime` (Task 3).
- Produces: `Phenomenon { kind: String, description: String, period_days: Option<f64>, salience: f64 }`; `ObserverContext { place: EntityId, time: WorldTime }`; `trait PhenomenaSource { fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon> }`; `observe(sources: &[&dyn PhenomenaSource], ctx: &ObserverContext) -> Vec<Phenomenon>` (aggregated, sorted by salience descending with deterministic tie-breaks). This is the universal read of the trace protocol: astronomy and climate implement `PhenomenaSource` in plan 1b; religion consumes `observe` output without knowing sources exist.

- [ ] **Step 1: Write the failing tests**

`kernel/src/phenomena.rs`:

```rust
//! Phenomena: the universal read (spec §3.1.6). "What would an observer
//! at (place, time) notice?" Every meaning-making system consumes this
//! and only this; it never learns what produced a phenomenon.

use crate::field::WorldTime;
use crate::ledger::EntityId;
use serde::{Deserialize, Serialize};

#[cfg(test)]
mod tests {
    use super::*;

    struct FixedSource(Vec<Phenomenon>);

    impl PhenomenaSource for FixedSource {
        fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
            self.0.clone()
        }
    }

    fn ctx() -> ObserverContext {
        ObserverContext {
            place: EntityId(1),
            time: WorldTime { day: 0.0 },
        }
    }

    fn ph(kind: &str, salience: f64) -> Phenomenon {
        Phenomenon {
            kind: kind.to_string(),
            description: format!("the {kind}"),
            period_days: None,
            salience,
        }
    }

    #[test]
    fn observe_aggregates_all_sources() {
        let a = FixedSource(vec![ph("sun", 1.0)]);
        let b = FixedSource(vec![ph("breeze", 0.2), ph("river", 0.4)]);
        let out = observe(&[&a, &b], &ctx());
        assert_eq!(out.len(), 3);
    }

    #[test]
    fn observe_sorts_by_salience_descending() {
        let a = FixedSource(vec![ph("breeze", 0.2)]);
        let b = FixedSource(vec![ph("sun", 1.0), ph("river", 0.4)]);
        let kinds: Vec<String> = observe(&[&a, &b], &ctx())
            .into_iter()
            .map(|p| p.kind)
            .collect();
        assert_eq!(kinds, vec!["sun", "river", "breeze"]);
    }

    #[test]
    fn observe_breaks_salience_ties_deterministically() {
        // Equal salience: sorted by kind, then description.
        let a = FixedSource(vec![ph("zephyr", 0.5), ph("aurora", 0.5)]);
        let out = observe(&[&a], &ctx());
        assert_eq!(out[0].kind, "aurora");
        assert_eq!(out[1].kind, "zephyr");
    }

    #[test]
    fn observe_with_no_sources_is_empty() {
        assert!(observe(&[], &ctx()).is_empty());
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod phenomena;` and `pub use phenomena::{ObserverContext, PhenomenaSource, Phenomenon, observe};`

Run: `cargo test -p hornvale-kernel phenomena`
Expected: compile error — types not defined.

- [ ] **Step 3: Implement the phenomena vocabulary**

Add above the `tests` module in `kernel/src/phenomena.rs`:

```rust
/// Something an observer would notice. `kind` must be registered in the
/// concept registry by the producing domain. Consumers must not branch on
/// the producing system — only on kind, period, character, salience.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Phenomenon {
    pub kind: String,
    pub description: String,
    /// None = constant or aperiodic; Some(d) = recurs every d days.
    pub period_days: Option<f64>,
    /// How much this demands attention, in [0, 1].
    pub salience: f64,
}

/// Where and when the observation happens. Culture joins in a later
/// campaign; adding a field here must not break existing sources.
#[derive(Clone, Copy, Debug)]
pub struct ObserverContext {
    pub place: EntityId,
    pub time: WorldTime,
}

/// Anything that contributes observable phenomena. Implementations must
/// be pure: same context → same phenomena.
pub trait PhenomenaSource {
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon>;
}

/// Aggregate all sources, sorted by salience descending. Ties break by
/// kind then description so output order never depends on source order
/// alone being stable — determinism is constitutional.
pub fn observe(sources: &[&dyn PhenomenaSource], ctx: &ObserverContext) -> Vec<Phenomenon> {
    let mut all: Vec<Phenomenon> = sources.iter().flat_map(|s| s.phenomena(ctx)).collect();
    all.sort_by(|a, b| {
        b.salience
            .total_cmp(&a.salience)
            .then_with(|| a.kind.cmp(&b.kind))
            .then_with(|| a.description.cmp(&b.description))
    });
    all
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-kernel phenomena`
Expected: all 4 tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace -- -D warnings
git add kernel/
git commit -m "feat(kernel): phenomena vocabulary — the trace protocol's universal read"
```

---

### Task 8: World assembly, persistence, and the determinism suite

**Files:**
- Create: `kernel/src/world.rs` (tests inline)
- Create: `kernel/tests/determinism.rs`
- Modify: `kernel/src/lib.rs`

**Interfaces:**
- Consumes: everything above.
- Produces: `World { seed: Seed, registry: ConceptRegistry, ledger: Ledger }` with `World::new(seed: Seed) -> World` (registers core concepts: predicate `"name"`, functional), `to_json(&self) -> String` (pretty, deterministic), `from_json(&str) -> Result<World, serde_json::Error>`, `save(&self, path: &Path) -> std::io::Result<()>`, `load(path: &Path) -> std::io::Result<World>`. Plan 1b's CLI persists worlds exclusively through these.

- [ ] **Step 1: Write the failing tests**

`kernel/src/world.rs`:

```rust
//! World assembly: seed + registry + ledger. A saved world IS a seed
//! plus a ledger (Constitution §2.3); providers are stateless in tier 0
//! and reconstructed at load by the application.

use crate::ledger::Ledger;
use crate::registry::ConceptRegistry;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};
use std::path::Path;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{Fact, Value};

    #[test]
    fn new_world_registers_core_concepts() {
        let w = World::new(Seed(42));
        assert!(w.registry.predicate("name").unwrap().functional);
    }

    #[test]
    fn world_json_roundtrips() {
        let mut w = World::new(Seed(42));
        let e = w.ledger.mint_entity();
        w.ledger
            .commit(
                Fact {
                    subject: e,
                    predicate: "name".to_string(),
                    object: Value::Text("Zaggrak".to_string()),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &w.registry,
            )
            .unwrap();
        let w2 = World::from_json(&w.to_json()).unwrap();
        assert_eq!(w2.seed, Seed(42));
        assert_eq!(w2.ledger.len(), 1);
        assert_eq!(
            w2.ledger.value_of(e, "name"),
            Some(&Value::Text("Zaggrak".to_string()))
        );
    }

    #[test]
    fn world_json_is_deterministic() {
        assert_eq!(World::new(Seed(9)).to_json(), World::new(Seed(9)).to_json());
    }

    #[test]
    fn world_saves_and_loads_from_disk() {
        let dir = std::env::temp_dir().join("hornvale-kernel-test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("world.json");
        let w = World::new(Seed(42));
        w.save(&path).unwrap();
        let w2 = World::load(&path).unwrap();
        assert_eq!(w2.seed, Seed(42));
        std::fs::remove_file(&path).unwrap();
    }
}
```

`kernel/tests/determinism.rs`:

```rust
//! Cross-module determinism: the constitutional test (spec §6).
//! A mini-genesis run twice from the same seed must be byte-identical.

use hornvale_kernel::{
    ConstantField, EntityId, Fact, Field, Position, Seed, Value, World, WorldTime,
    choose_consistent, fbm_2d, observe, ObserverContext, PhenomenaSource, Phenomenon,
};

struct MiniSun;

impl PhenomenaSource for MiniSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }]
    }
}

/// A miniature genesis exercising every kernel module together.
fn mini_genesis(seed: Seed) -> String {
    let mut world = World::new(seed);
    world
        .registry
        .register_predicate("revered-phenomenon", true, "what a settlement reveres")
        .unwrap();
    world
        .registry
        .register_phenomenon_kind("celestial-body", "a body visible in the sky")
        .unwrap();

    // Terrain-ish: a place with a field-derived character.
    let vale = world.ledger.mint_entity();
    let roughness = fbm_2d(seed.derive("terrain"), 0.5, 0.5, 3);
    let biome = ConstantField("temperate forest".to_string())
        .sample(Position { x: 0.0, y: 0.0 }, WorldTime { day: 0.0 });
    assert!((0.0..1.0).contains(&roughness));
    assert_eq!(biome, "temperate forest");

    // Settlement-ish: a named village, name refined against the ledger.
    let village = world.ledger.mint_entity();
    let candidates = ["Zaggrak", "Bolnar", "Mokru", "Ishtor"];
    let mut stream = seed.derive("settlement").derive("name").stream();
    let idx = choose_consistent(&mut stream, &world.ledger, &world.registry, &candidates, |n| {
        Fact {
            subject: village,
            predicate: "name".to_string(),
            object: Value::Text((*n).to_string()),
            place: Some(vale),
            day: Some(0.0),
            provenance: "settlement".to_string(),
        }
    })
    .expect("a name must survive an empty ledger");
    world
        .ledger
        .commit(
            Fact {
                subject: village,
                predicate: "name".to_string(),
                object: Value::Text(candidates[idx].to_string()),
                place: Some(vale),
                day: Some(0.0),
                provenance: "settlement".to_string(),
            },
            &world.registry,
        )
        .unwrap();

    // Religion-ish: revere the most salient phenomenon, source-blind.
    let sun = MiniSun;
    let seen = observe(&[&sun], &ObserverContext {
        place: vale,
        time: WorldTime { day: 0.0 },
    });
    world
        .ledger
        .commit(
            Fact {
                subject: village,
                predicate: "revered-phenomenon".to_string(),
                object: Value::Text(seen[0].kind.clone()),
                place: Some(vale),
                day: Some(0.0),
                provenance: "religion".to_string(),
            },
            &world.registry,
        )
        .unwrap();

    world.to_json()
}

#[test]
fn same_seed_yields_byte_identical_worlds() {
    assert_eq!(mini_genesis(Seed(42)), mini_genesis(Seed(42)));
}

#[test]
fn different_seeds_yield_different_worlds() {
    // Village naming draws from the seed, so worlds should differ.
    // (Four candidates: a collision for one pair is possible but the
    // *entire serialized world* matching across these seeds is not.)
    let worlds: Vec<String> = (1..=4).map(|s| mini_genesis(Seed(s))).collect();
    assert!(worlds.windows(2).any(|w| w[0] != w[1]));
}

#[test]
fn saved_world_reloads_identically() {
    let json = mini_genesis(Seed(7));
    let world = World::from_json(&json).unwrap();
    assert_eq!(world.to_json(), json);
}

#[test]
fn entity_ids_are_never_reused_after_reload() {
    let json = mini_genesis(Seed(7));
    let mut world = World::from_json(&json).unwrap();
    let fresh = world.ledger.mint_entity();
    // vale = 1, village = 2 in mini_genesis; fresh must not collide.
    assert!(fresh != EntityId(1) && fresh != EntityId(2));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Add to `kernel/src/lib.rs`: `pub mod world;` and `pub use world::World;`

Run: `cargo test -p hornvale-kernel`
Expected: compile error — `World` not defined.

- [ ] **Step 3: Implement World**

Add above the `tests` module in `kernel/src/world.rs`:

```rust
/// A world is a seed plus everything ever observed about it.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct World {
    pub seed: Seed,
    pub registry: ConceptRegistry,
    pub ledger: Ledger,
}

impl World {
    /// Create an empty world and register kernel-core concepts.
    /// Domains register their own concepts at wiring time.
    pub fn new(seed: Seed) -> World {
        let mut registry = ConceptRegistry::default();
        registry
            .register_predicate("name", true, "canonical name of an entity")
            .expect("core concept registration cannot conflict in an empty registry");
        World {
            seed,
            registry,
            ledger: Ledger::default(),
        }
    }

    /// Deterministic pretty JSON. This string is the save format and the
    /// determinism-test currency: same world → same bytes.
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("World serialization cannot fail")
    }

    pub fn from_json(json: &str) -> Result<World, serde_json::Error> {
        serde_json::from_str(json)
    }

    pub fn save(&self, path: &Path) -> std::io::Result<()> {
        std::fs::write(path, self.to_json())
    }

    pub fn load(path: &Path) -> std::io::Result<World> {
        let json = std::fs::read_to_string(path)?;
        World::from_json(&json).map_err(std::io::Error::other)
    }
}
```

- [ ] **Step 4: Run the full suite to verify everything passes**

Run: `cargo test --workspace`
Expected: all unit tests from Tasks 1–7 plus 4 world tests plus 4 determinism tests PASS.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt && cargo clippy --workspace --all-targets -- -D warnings
git add kernel/
git commit -m "feat(kernel): world assembly, JSON persistence, determinism suite

A world is a seed plus a ledger. Mini-genesis integration test
exercises seed→noise→field→refine→ledger→phenomena end to end and
asserts byte-identical output for identical seeds."
```

---

## Self-Review Notes

- **Spec coverage:** Campaign 1's kernel items — seed/noise substrate (T1–2), field infrastructure (T3), fact ledger v0 (T5), trivial refinement engine (T6), provider registry *as concept registry + phenomena trait* (T4, T7), world persistence (T8). The tier-0 domain providers, REPL, and almanac are Plan 1b by design. A separate runtime "provider registry" object was considered and dropped for YAGNI: with stateless tier-0 providers, wiring is plain composition in the app crate; revisit when providers gain state or configuration-driven tiers.
- **Type consistency:** `Fact` field `day: Option<f64>` (not `time`) throughout; `range_u32` inclusive; `commit` returns `Result<bool, LedgerError>`; `observe` takes `&[&dyn PhenomenaSource]`.
- **Known follow-ups deferred to 1b:** domain crates, `register_concepts` conventions per domain, CLI (`new`/`repl`/`almanac`), almanac rendering, cross-crate integration tests, exit-criterion walkthrough.
