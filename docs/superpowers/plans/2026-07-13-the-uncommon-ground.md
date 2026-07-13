# The Uncommon Ground — Implementation Plan

**Status: SHIPPED** — all 10 tasks complete, full gate green, merged to main 2026-07-13.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Overlay a combinatorially-generated, natural-tier **strangeness regime** on each room's inherited climate biome, so an Earth-sized world has genuinely varied room-scale terrain — including *within* a homogeneous biome.

**Architecture:** A tiny kernel weighted-choice primitive feeds a locale-window **negation-slot engine** (a regime = one draw per exclusion slot). A **derived tier** (rung 15) reads inherited fields + a sub-cell micro-field for free per-room variety; a **placed tier** (rung 30) runs a bounded blue-noise **rarity budget** with a founder floor at world-build. Everything is a pure function of address (steady), quantized at the emit boundary; strangeness never replaces climate's categorical biome.

**Tech Stack:** Rust (edition 2024), workspace crates `hornvale-kernel` / `hornvale-climate` / `hornvale-terrain` / `hornvale-worldgen` / `hornvale-locale` / `hornvale` (cli). std-only + serde/serde_json. `cargo nextest` for tests.

**Spec:** `docs/superpowers/specs/2026-07-13-the-uncommon-ground-design.md`.

## Global Constraints

Copied verbatim from the spec / repo constitution; every task implicitly includes these.

- **Determinism:** same seed + pins → byte-identical output. No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock; time is `WorldTime { day: f64 }`. Float sorting uses `total_cmp`.
- **Cross-platform (decision 0041):** transcendentals route through `hornvale_kernel::math` (pure-Rust libm). Any **discrete decision on a continuous value** (a threshold comparison, an accept/reject) keys off an **integer or `quantize`d** quantity. Quantize only at the emit boundary (`hornvale_kernel::quantize`), never in the compute path.
- **Dependencies:** `serde` + `serde_json` only. No new crates.
- **Docs:** every crate `#![warn(missing_docs)]`; every `pub` item/field/variant gets a one-line doc comment. Every primitive at a `pub` boundary carries a `type-audit:` tag.
- **Save-format contracts:** new seed-derivation labels are declared as `const` in `windows/locale/src/streams.rs` and published via `stream_labels()`; per-slot **stream-consumption order is frozen**; the kernel weighted-choice draw semantics are frozen. Deliberate regeneration uses an epoch suffix (`locale/room/v2`), never a rename.
- **Layering:** `kernel` → `domains/*` → `windows/*` → `cli`. A domain depends only on the kernel. `windows/locale` may read domains. No domain→domain dependency introduced.
- **Gate:** `cargo fmt` is the final step before every commit. `make gate` = fmt + clippy (`-D warnings`) + `cargo nextest run --workspace` + doctests.

---

### Task 1: Kernel weighted-choice primitive

**Files:**
- Modify: `kernel/src/seed.rs` (add a method to `impl Stream`, ~line 78; add tests in the existing `mod tests`)

**Interfaces:**
- Consumes: nothing (uses existing `Stream::next_f64`).
- Produces: `Stream::weighted_index(&mut self, weights: &[f64]) -> Option<usize>` — the frozen weighted-draw semantics every grammar slot uses. Returns `None` iff `weights` is empty or every weight is ≤ 0. Weights are consumed in slice order; the threshold is `next_f64() * total`; the first cumulative bucket strictly exceeding the threshold wins.

- [ ] **Step 1: Write the failing tests** in `kernel/src/seed.rs` inside `mod tests`:

```rust
#[test]
fn weighted_index_is_deterministic() {
    let w = [1.0, 2.0, 3.0];
    let a = Seed(7).stream().weighted_index(&w);
    let b = Seed(7).stream().weighted_index(&w);
    assert_eq!(a, b);
    assert!(a.is_some());
}

#[test]
fn weighted_index_empty_and_zero_are_none() {
    assert_eq!(Seed(1).stream().weighted_index(&[]), None);
    assert_eq!(Seed(1).stream().weighted_index(&[0.0, 0.0]), None);
}

#[test]
fn weighted_index_respects_weights() {
    // With weights [0, 1], index 0 (weight 0) can never be chosen.
    for s in 0..64 {
        assert_eq!(Seed(s).stream().weighted_index(&[0.0, 1.0]), Some(1));
    }
}

#[test]
fn weighted_index_uniform_matches_pick_distribution() {
    // Equal weights → all indices reachable across seeds.
    let seen: std::collections::BTreeSet<usize> = (0..200)
        .filter_map(|s| Seed(s).stream().weighted_index(&[1.0, 1.0, 1.0, 1.0]))
        .collect();
    assert_eq!(seen, [0, 1, 2, 3].into_iter().collect());
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-kernel weighted_index`
Expected: FAIL — `no method named weighted_index found for struct Stream`.

- [ ] **Step 3: Implement `weighted_index`** in `impl Stream` (after `pick`, ~line 78):

```rust
    /// Deterministically pick an index with probability proportional to its
    /// weight. `None` if the slice is empty or all weights are ≤ 0. Weights
    /// are consumed in slice order; the threshold is `next_f64() * total`,
    /// and the first cumulative bucket strictly exceeding it wins. This
    /// draw-semantics is a frozen save-format contract.
    /// type-audit: bare-ok(index)
    pub fn weighted_index(&mut self, weights: &[f64]) -> Option<usize> {
        let total: f64 = weights.iter().filter(|w| **w > 0.0).sum();
        if total <= 0.0 {
            return None;
        }
        let threshold = self.next_f64() * total;
        let mut cumulative = 0.0;
        for (i, &w) in weights.iter().enumerate() {
            if w <= 0.0 {
                continue;
            }
            cumulative += w;
            if cumulative > threshold {
                return Some(i);
            }
        }
        // Floating-point tail: return the last positive-weight index.
        weights.iter().rposition(|&w| w > 0.0)
    }
```

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-kernel weighted_index`
Expected: PASS (4 tests).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add kernel/src/seed.rs
git commit -m "feat(kernel): Stream::weighted_index weighted-choice primitive

The frozen weighted-draw the room-scale grammar slots consume (MAP-29).

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 2: Regime data model + strangeness magnitude

**Files:**
- Create: `windows/locale/src/regime.rs`
- Modify: `windows/locale/src/lib.rs` (add `mod regime; pub use regime::…;` near the top, after `mod streams;`)

**Interfaces:**
- Consumes: nothing.
- Produces: the overlay types and the derived magnitude —
  - `Substrate { Ordinary, Sand, Evaporite, Basaltic, Ashen }`
  - `EnergySource { Sunlit, Chemosynthetic, Geothermal }`
  - `Kingdom { PlantAnimal, Fungal, Crystalline, Microbial }`
  - `Negations { substrate: Substrate, energy: EnergySource, kingdom: Kingdom, endemic: bool }`
  - `MicroField { relief: f64, aspect: f64, wetness: f64, openness: f64 }`
  - `Regime { negations: Negations, micro: MicroField, descriptor: String, strangeness: f64 }` (the overlay; the base biome stays on `Locale.biome` to avoid duplication)
  - `Negations::strangeness(&self) -> f64` — the derived magnitude, in `0..=30` (this campaign's ceiling).

- [ ] **Step 1: Write the failing tests** — create `windows/locale/src/regime.rs`:

```rust
#![allow(dead_code)] // consumers land in later tasks
#[cfg(test)]
mod tests {
    use super::*;

    fn mundane() -> Negations {
        Negations {
            substrate: Substrate::Ordinary,
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::PlantAnimal,
            endemic: false,
        }
    }

    #[test]
    fn mundane_has_zero_strangeness() {
        assert_eq!(mundane().strangeness(), 0.0);
    }

    #[test]
    fn substrate_negation_is_extreme_rung() {
        let n = Negations { substrate: Substrate::Sand, ..mundane() };
        assert_eq!(n.strangeness(), 15.0);
    }

    #[test]
    fn energy_or_kingdom_negation_is_exotic_rung() {
        let e = Negations { energy: EnergySource::Chemosynthetic, ..mundane() };
        let k = Negations { kingdom: Kingdom::Fungal, ..mundane() };
        assert_eq!(e.strangeness(), 30.0);
        assert_eq!(k.strangeness(), 30.0);
    }

    #[test]
    fn magnitude_is_the_max_departure_not_the_sum() {
        // basaltic (15) + chemo (30) reads as exotic (30), never rung-45.
        let n = Negations {
            substrate: Substrate::Basaltic,
            energy: EnergySource::Chemosynthetic,
            ..mundane()
        };
        assert_eq!(n.strangeness(), 30.0);
    }

    #[test]
    fn endemic_adds_a_capped_bonus() {
        let n = Negations { endemic: true, ..mundane() };
        assert_eq!(n.strangeness(), 5.0);
        let capped = Negations {
            energy: EnergySource::Geothermal,
            endemic: true,
            ..mundane()
        };
        assert_eq!(capped.strangeness(), 30.0); // clamped to the campaign ceiling
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-locale regime`
Expected: FAIL — types not found.

- [ ] **Step 3: Implement the model** at the top of `windows/locale/src/regime.rs` (above the test module):

```rust
//! The strangeness regime — the natural-tier overlay on a room's inherited
//! biome (MAP-29). One value per exclusion slot, so composites are always
//! coherent; `strangeness` is the derived magnitude of that vector.

use hornvale_kernel::quantize;
use serde::Serialize;

/// This campaign's strangeness ceiling (rung "exotic").
const STRANGENESS_CEILING: f64 = 30.0;

/// The material a room's ground is made of (substrate slot; proxy-earned only).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Substrate {
    /// Rock and soil — the mundane default.
    Ordinary,
    /// Wind-worked sand.
    Sand,
    /// Evaporite salt/gypsum crust.
    Evaporite,
    /// Bare volcanic basalt.
    Basaltic,
    /// Volcanic ash drifts.
    Ashen,
}

/// What powers a room's ecology (energy slot).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum EnergySource {
    /// Sunlight — the mundane default.
    Sunlit,
    /// Chemosynthesis (cold seeps, vents).
    Chemosynthetic,
    /// Geothermal heat.
    Geothermal,
}

/// The dominant kingdom of life (kingdom slot).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Kingdom {
    /// Plants then animals — the mundane default.
    PlantAnimal,
    /// A fungal kingdom.
    Fungal,
    /// Mineral/crystalline "flora".
    Crystalline,
    /// Microbial mats.
    Microbial,
}

/// The negation vector: one draw per exclusion slot plus the endemic toggle.
/// type-audit: bare-ok(prose: substrate), bare-ok(prose: energy), bare-ok(prose: kingdom), bare-ok(flag: endemic)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Negations {
    /// Substrate slot.
    pub substrate: Substrate,
    /// Energy slot.
    pub energy: EnergySource,
    /// Kingdom slot.
    pub kingdom: Kingdom,
    /// Isolation modifier (endemic biota); gated on an isolation signal.
    pub endemic: bool,
}

impl Negations {
    /// The derived strangeness magnitude: the *maximum* slot departure (so a
    /// basaltic vent reads "exotic", not the summed rung-45 we defer), plus a
    /// small endemic bonus, clamped to this campaign's ceiling.
    /// type-audit: bare-ok(ratio)
    pub fn strangeness(&self) -> f64 {
        let substrate = if self.substrate == Substrate::Ordinary { 0.0 } else { 15.0 };
        let energy = if self.energy == EnergySource::Sunlit { 0.0 } else { 30.0 };
        let kingdom = if self.kingdom == Kingdom::PlantAnimal { 0.0 } else { 30.0 };
        let max = substrate.max(energy).max(kingdom);
        let bonus = if self.endemic { 5.0 } else { 0.0 };
        quantize((max + bonus).min(STRANGENESS_CEILING))
    }
}

/// A few grounded per-room continuous axes (from address noise); the descriptor
/// reads these so homogeneous biome still varies room-to-room.
/// type-audit: bare-ok(ratio: relief), bare-ok(ratio: aspect), bare-ok(ratio: wetness), bare-ok(ratio: openness)
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct MicroField {
    /// Micro-relief, hollow (-1) .. rise (+1).
    pub relief: f64,
    /// Slope aspect / insolation, shaded (-1) .. sunlit (+1).
    pub aspect: f64,
    /// Local wetness, dry (-1) .. wet (+1).
    pub wetness: f64,
    /// Canopy openness, closed (-1) .. open (+1).
    pub openness: f64,
}

/// The strangeness overlay for a room. The base biome stays on `Locale.biome`.
/// type-audit: bare-ok(prose: descriptor), bare-ok(ratio: strangeness)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Regime {
    /// The negation vector.
    pub negations: Negations,
    /// The sub-cell micro-field.
    pub micro: MicroField,
    /// The rendered descriptor prose.
    pub descriptor: String,
    /// The derived strangeness magnitude (0..=30, quantized).
    pub strangeness: f64,
}
```

- [ ] **Step 4: Wire the module** — in `windows/locale/src/lib.rs`, after the existing `mod streams;` line, add:

```rust
mod regime;
pub use regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};
```

- [ ] **Step 5: Run to verify pass**

Run: `cargo test -p hornvale-locale regime`
Expected: PASS (5 tests).

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/regime.rs windows/locale/src/lib.rs
git commit -m "feat(locale): regime data model + derived strangeness magnitude (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 3: The substrate proxy (DOM-14 seam)

**Files:**
- Create: `windows/locale/src/substrate.rs`
- Modify: `windows/locale/src/lib.rs` (`mod substrate;`)

**Interfaces:**
- Consumes: `Substrate` (Task 2); `GeneratedClimate::moisture_at`, `GeneratedTerrain::globe()` fields `unrest: CellMap<f64>`, `elevation: CellMap<f64>`, `sea_level: f64`, `boundary: CellMap<Option<CellBoundary>>` (all already public).
- Produces: `pub(crate) fn substrate_at(climate: &GeneratedClimate, terrain: &GeneratedTerrain, cell: CellId) -> Substrate` — the conservative proxy standing in for the unbuilt DOM-14 lithosphere. Only distinctions defensible from existing signals; everything else returns `Ordinary`.

- [ ] **Step 1: Write the failing tests** — `windows/locale/src/substrate.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::World;
    use hornvale_worldgen::{climate_of, terrain_of};

    #[test]
    fn substrate_is_deterministic_and_total() {
        // Every cell resolves to a substrate; twice-sampled is identical.
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        for c in geo.cells() {
            let a = substrate_at(&climate, &terrain, c);
            let b = substrate_at(&climate, &terrain, c);
            assert_eq!(a, b);
        }
    }

    #[test]
    fn high_unrest_reads_volcanic() {
        // A high-unrest land cell is Basaltic or Ashen, never Ordinary.
        let w = World::new(hornvale_kernel::Seed(42));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let geo = climate.geosphere();
        let globe = terrain.globe();
        let volcanic = geo.cells().find(|&c| {
            *globe.unrest.get(c) > 0.7 && *globe.elevation.get(c) > globe.sea_level
        });
        if let Some(c) = volcanic {
            assert!(matches!(
                substrate_at(&climate, &terrain, c),
                Substrate::Basaltic | Substrate::Ashen
            ));
        }
    }
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-locale substrate`
Expected: FAIL — `substrate_at` not found.

- [ ] **Step 3: Implement the proxy** at the top of `windows/locale/src/substrate.rs`:

```rust
//! The substrate proxy — a conservative stand-in for the unbuilt DOM-14
//! lithosphere. Infers only substrate distinctions defensible from existing
//! climate/terrain signals; everything else is `Ordinary`. A real lithology
//! field later replaces this function without touching any consumer.

use crate::regime::Substrate;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, quantize};
use hornvale_terrain::GeneratedTerrain;

/// Thresholds are compared on quantized values so the discrete substrate
/// decision is cross-platform byte-identical (decision 0041).
pub(crate) fn substrate_at(
    climate: &GeneratedClimate,
    terrain: &GeneratedTerrain,
    cell: CellId,
) -> Substrate {
    let globe = terrain.globe();
    let elevation = quantize(*globe.elevation.get(cell));
    let sea_level = quantize(globe.sea_level);
    if elevation <= sea_level {
        // Underwater cells keep the ordinary substrate; marine biomes carry
        // their own identity via the base biome.
        return Substrate::Ordinary;
    }
    let unrest = quantize(*globe.unrest.get(cell));
    let moisture = quantize(climate.moisture_at(cell));
    let relief = quantize(elevation - sea_level);

    // Volcanic: high tectonic unrest → basalt (high relief) or ash (low).
    if unrest > 0.6 {
        return if relief > 500.0 { Substrate::Basaltic } else { Substrate::Ashen };
    }
    // Evaporite: very dry + flat (a salt pan / playa).
    if moisture < 0.15 && relief < 200.0 {
        return Substrate::Evaporite;
    }
    // Sand: dry + near the coast (low relief above sea level).
    if moisture < 0.25 && relief < 100.0 {
        return Substrate::Sand;
    }
    Substrate::Ordinary
}
```

Note: if `Geosphere::cells()` is not the exact iterator name, use the same iteration the existing `blend` closures use in `lib.rs`. Verify with `grep -n "fn cells" kernel/src/geosphere.rs`.

- [ ] **Step 4: Wire the module** — in `windows/locale/src/lib.rs` add `mod substrate;` after `mod regime;`.

- [ ] **Step 5: Run to verify pass**

Run: `cargo test -p hornvale-locale substrate`
Expected: PASS.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/substrate.rs windows/locale/src/lib.rs
git commit -m "feat(locale): conservative substrate proxy (DOM-14 seam, MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 4: The sub-cell micro-field

**Files:**
- Create: `windows/locale/src/micro.rs`
- Modify: `windows/locale/src/lib.rs` (`mod micro;`), `windows/locale/src/streams.rs` (new label)

**Interfaces:**
- Consumes: `MicroField` (Task 2); `RoomAddr::seed(Seed) -> Seed`, `Seed::derive`, `Stream::next_f64` (existing).
- Produces: `pub(crate) fn micro_field(room_seed: Seed) -> MicroField` — four axes in `[-1, 1]`, quantized at emit; deterministic per address, distinct between sibling rooms.

- [ ] **Step 1: Add the stream label** to `windows/locale/src/streams.rs`:

```rust
/// Stream label for a room's sub-cell micro-field.
/// type-audit: bare-ok(identifier-text)
pub const LOCALE_MICRO: &str = "locale/regime/micro";
```

And add it to the `stream_labels()` vec:

```rust
        (LOCALE_MICRO, "room sub-cell micro-field"),
```

- [ ] **Step 2: Write the failing tests** — `windows/locale/src/micro.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{RoomAddr, Seed};

    fn seed_for(path: Vec<u8>) -> Seed {
        RoomAddr { face: 3, path }.seed(Seed(42))
    }

    #[test]
    fn micro_field_is_deterministic_and_in_range() {
        let s = seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        let a = micro_field(s);
        let b = micro_field(s);
        assert_eq!(a, b);
        for v in [a.relief, a.aspect, a.wetness, a.openness] {
            assert!((-1.0..=1.0).contains(&v));
        }
    }

    #[test]
    fn sibling_rooms_differ() {
        let a = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0]));
        let b = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1]));
        assert_ne!(a, b);
    }
}
```

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-locale micro`
Expected: FAIL — `micro_field` not found.

- [ ] **Step 4: Implement** at the top of `windows/locale/src/micro.rs`:

```rust
//! The sub-cell micro-field: a few grounded per-room continuous axes drawn
//! from the room's address noise, so a walk through homogeneous biome still
//! varies room-to-room (the "miles and miles of forest" answer).

use crate::regime::MicroField;
use crate::streams::LOCALE_MICRO;
use hornvale_kernel::{Seed, quantize};

/// Four axes in [-1, 1], each a distinct sub-stream of the room's micro label,
/// quantized at emit (platform-exact).
pub(crate) fn micro_field(room_seed: Seed) -> MicroField {
    let mut s = room_seed.derive(LOCALE_MICRO).stream();
    let mut axis = || quantize(s.next_f64() * 2.0 - 1.0);
    MicroField {
        relief: axis(),
        aspect: axis(),
        wetness: axis(),
        openness: axis(),
    }
}
```

- [ ] **Step 5: Wire the module** — `mod micro;` in `lib.rs` after `mod substrate;`.

- [ ] **Step 6: Run to verify pass**

Run: `cargo test -p hornvale-locale micro`
Expected: PASS.

- [ ] **Step 7: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/micro.rs windows/locale/src/lib.rs windows/locale/src/streams.rs
git commit -m "feat(locale): sub-cell micro-field for within-biome variety (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 5: The descriptor grammar (derived tier, rung 15)

**Files:**
- Create: `windows/locale/src/grammar.rs`
- Modify: `windows/locale/src/lib.rs` (`mod grammar;`), `windows/locale/src/streams.rs` (slot labels)

**Interfaces:**
- Consumes: `Stream::weighted_index` (Task 1); `Negations`, `Regime`, `MicroField`, `Substrate` (Task 2); `substrate_at` (Task 3); `micro_field` (Task 4); `GeneratedClimate::biome_at`, `Biome`.
- Produces: `pub(crate) fn derived_regime(seed: Seed, addr: &RoomAddr, biome: Biome, substrate: Substrate, micro: MicroField) -> Regime` — the always-on derived overlay (energy=Sunlit, kingdom=PlantAnimal; substrate from the proxy; a rendered descriptor). Placed exotics (Task 6) override energy/kingdom before rendering, so this task also exposes `pub(crate) fn render(negations: Negations, micro: MicroField, biome: Biome, seed: Seed, addr: &RoomAddr) -> String` used by both tiers.

**Content source:** the descriptor pools are the authoring-time artifact (decision 0009). This task lands a *real, complete* first set drawn from cycle-02 Appendix A; it may later be model-amplified and re-committed. Each slot is a `&'static [(f64, &'static str)]` weighted pool selected per biome/substrate.

- [ ] **Step 1: Add slot labels** to `windows/locale/src/streams.rs` (mirroring `LOCALE_MICRO`): `LOCALE_SUBSTRATE_DETAIL = "locale/regime/substrate"`, `LOCALE_VARIETY = "locale/regime/variety"`, `LOCALE_MICRO_HABITAT = "locale/regime/habitat"`, each with a `stream_labels()` entry and a `type-audit: bare-ok(identifier-text)` tag.

- [ ] **Step 2: Write the failing tests** — `windows/locale/src/grammar.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_climate::Biome;
    use hornvale_kernel::{RoomAddr, Seed};

    fn micro0() -> MicroField {
        MicroField { relief: 0.0, aspect: 0.0, wetness: 0.0, openness: 0.0 }
    }

    #[test]
    fn derived_is_deterministic() {
        let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] };
        let a = derived_regime(Seed(42), &addr, Biome::Desert, Substrate::Sand, micro0());
        let b = derived_regime(Seed(42), &addr, Biome::Desert, Substrate::Sand, micro0());
        assert_eq!(a, b);
    }

    #[test]
    fn derived_tier_is_never_exotic() {
        // The derived tier only negates substrate; energy/kingdom stay mundane.
        let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] };
        let r = derived_regime(Seed(42), &addr, Biome::Desert, Substrate::Sand, micro0());
        assert_eq!(r.negations.energy, EnergySource::Sunlit);
        assert_eq!(r.negations.kingdom, Kingdom::PlantAnimal);
        assert!(r.strangeness <= 15.0);
        assert!(!r.descriptor.is_empty());
    }

    #[test]
    fn adjacent_rooms_in_one_biome_differ() {
        // The "miles and miles of forest" guard: sibling forest rooms with the
        // same biome + substrate still produce distinguishable descriptors.
        let biome = Biome::TemperateForest;
        let mut seen = std::collections::BTreeSet::new();
        for last in 0..4u8 {
            let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, last] };
            let micro = crate::micro::micro_field(addr.seed(Seed(42)));
            let r = derived_regime(Seed(42), &addr, biome, Substrate::Ordinary, micro);
            seen.insert(r.descriptor);
        }
        assert!(seen.len() >= 3, "adjacent forest rooms should mostly differ, got {seen:?}");
    }
}
```

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-locale grammar`
Expected: FAIL — functions/types not found.

- [ ] **Step 4: Implement the grammar** in `windows/locale/src/grammar.rs`. The engine: for each slot, select the biome/substrate-conditioned pool, draw with `weighted_index` off a per-slot stream, assemble the descriptor. Full code:

```rust
//! The descriptor grammar (P3): ordered weighted-choice slots, conditioned on
//! biome / substrate / micro-field, assembled to prose. Pools are the
//! authoring-time artifact (decision 0009); this is the first complete set.

use crate::micro::micro_field;
use crate::regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};
use crate::streams::{LOCALE_MICRO_HABITAT, LOCALE_SUBSTRATE_DETAIL, LOCALE_VARIETY};
use hornvale_climate::Biome;
use hornvale_kernel::{RoomAddr, Seed};

/// A weighted descriptor pool.
type Pool = &'static [(f64, &'static str)];

/// The derived overlay for a room (substrate-only negation; mundane energy/kingdom).
pub(crate) fn derived_regime(
    seed: Seed,
    addr: &RoomAddr,
    biome: Biome,
    substrate: Substrate,
    micro: MicroField,
) -> Regime {
    let negations = Negations {
        substrate,
        energy: EnergySource::Sunlit,
        kingdom: Kingdom::PlantAnimal,
        endemic: false,
    };
    let descriptor = render(negations, micro, biome, seed, addr);
    Regime { negations, micro, descriptor, strangeness: negations.strangeness() }
}

/// Render a descriptor for any negation vector (used by both tiers).
pub(crate) fn render(
    negations: Negations,
    micro: MicroField,
    biome: Biome,
    seed: Seed,
    addr: &RoomAddr,
) -> String {
    let room = addr.seed(seed);
    let variety = draw(room, LOCALE_VARIETY, variety_pool(biome, negations.substrate));
    let substrate_detail = draw(room, LOCALE_SUBSTRATE_DETAIL, substrate_pool(negations.substrate));
    let habitat = micro_habitat(room, micro);
    let exotic = exotic_clause(negations);
    // Assemble, dropping empty clauses.
    [variety, substrate_detail, habitat, exotic]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Draw one entry from a pool off a per-slot stream; "" if the pool is empty.
fn draw(room: Seed, label: &str, pool: Pool) -> String {
    if pool.is_empty() {
        return String::new();
    }
    let weights: Vec<f64> = pool.iter().map(|(w, _)| *w).collect();
    let i = room.derive(label).stream().weighted_index(&weights).unwrap_or(0);
    pool[i].1.to_string()
}

/// The micro-habitat clause reads the MicroField deterministically (no draw).
fn micro_habitat(room: Seed, micro: MicroField) -> String {
    let relief = if micro.relief > 0.33 { "on a rise" }
        else if micro.relief < -0.33 { "in a hollow" } else { "" };
    let aspect = if micro.aspect > 0.33 { "sun-warmed" }
        else if micro.aspect < -0.33 { "shaded" } else { "" };
    let wet = if micro.wetness > 0.33 { "damp" }
        else if micro.wetness < -0.33 { "dry" } else { "" };
    // One draw only to order the adjectives deterministically without bias.
    let _ = room; // room seed reserved for future ordering variation
    [aspect, wet, relief].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ")
}

/// Exotic clause for placed regimes (empty for the derived tier).
fn exotic_clause(n: Negations) -> String {
    let mut parts = Vec::new();
    match n.energy {
        EnergySource::Chemosynthetic => parts.push("fed by cold seeps"),
        EnergySource::Geothermal => parts.push("warmed from below"),
        EnergySource::Sunlit => {}
    }
    match n.kingdom {
        Kingdom::Fungal => parts.push("under a fungal canopy"),
        Kingdom::Crystalline => parts.push("grown with mineral crystal"),
        Kingdom::Microbial => parts.push("crusted with microbial mats"),
        Kingdom::PlantAnimal => {}
    }
    if n.endemic {
        parts.push("with biota found nowhere else");
    }
    parts.join(", ")
}

/// Base-variety pool per biome (+ substrate for deserts). Real content drawn
/// from cycle-02 Appendix A; extend as authoring amplifies (decision 0009).
fn variety_pool(biome: Biome, substrate: Substrate) -> Pool {
    match (biome, substrate) {
        (Biome::Desert, Substrate::Sand) => &[(3.0, "erg dunes"), (2.0, "a nabkha field")],
        (Biome::Desert, Substrate::Evaporite) => &[(3.0, "a cracked playa"), (2.0, "a salt pan")],
        (Biome::Desert, Substrate::Basaltic) => &[(3.0, "a hamada of bare rock")],
        (Biome::Desert, _) => &[(3.0, "a reg of wind-swept gravel"), (2.0, "a yardang field")],
        (Biome::TemperateForest | Biome::TemperateRainforest, _) => {
            &[(3.0, "old-growth timber"), (3.0, "dense understory"), (2.0, "a mossy hollow"), (2.0, "a windthrow gap")]
        }
        (Biome::Taiga, _) => &[(3.0, "a boreal stand"), (2.0, "a peat hollow"), (1.0, "a burnt snag")],
        (Biome::Tundra | Biome::Alpine, _) => &[(3.0, "frost-heaved ground"), (2.0, "a boulder field"), (2.0, "wind scour")],
        (Biome::Savanna | Biome::TemperateGrassland, _) => &[(3.0, "open sward"), (2.0, "a scattered copse")],
        (Biome::TropicalRainforest | Biome::TropicalSeasonalForest, _) => {
            &[(3.0, "buttressed canopy"), (2.0, "a liana tangle"), (2.0, "a stream gully")]
        }
        _ => &[(2.0, "broken terrain"), (2.0, "unremarkable ground")],
    }
}

/// Substrate-detail clause pool.
fn substrate_pool(substrate: Substrate) -> Pool {
    match substrate {
        Substrate::Ordinary => &[],
        Substrate::Sand => &[(1.0, "of shifting sand")],
        Substrate::Evaporite => &[(1.0, "of salt-white crust")],
        Substrate::Basaltic => &[(1.0, "of black basalt")],
        Substrate::Ashen => &[(1.0, "of drifted ash")],
    }
}
```

- [ ] **Step 5: Wire the module** — `mod grammar;` in `lib.rs` after `mod micro;`.

- [ ] **Step 6: Run to verify pass**

Run: `cargo test -p hornvale-locale grammar`
Expected: PASS (3 tests). If `adjacent_rooms_in_one_biome_differ` is flaky, widen the forest variety pool (more entries) — it is the local-variety guard and must hold.

- [ ] **Step 7: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/grammar.rs windows/locale/src/lib.rs windows/locale/src/streams.rs
git commit -m "feat(locale): descriptor grammar + derived tier (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 6: The rarity budget (placed tier, rung 30)

**Files:**
- Create: `windows/locale/src/budget.rs`
- Modify: `windows/locale/src/lib.rs` (`mod budget;`), `windows/locale/src/streams.rs` (`LOCALE_PLACE = "locale/strangeness/place"`)

**Interfaces:**
- Consumes: `Stream::weighted_index` (Task 1); `Negations`/`EnergySource`/`Kingdom` (Task 2); terrain `globe()` fields `unrest`, `boundary`, `elevation`, `sea_level`, `endorheic`; `GeneratedClimate::biome_at`.
- Produces:
  - `pub struct StrangeSite { pub cell: u32, pub energy: EnergySource, pub kingdom: Kingdom, pub endemic: bool }` (serializable, public — findability output).
  - `pub(crate) struct StrangenessBudget { sites: BTreeMap<u32, StrangeSite> }` with `pub(crate) fn build(seed, climate, terrain) -> StrangenessBudget`, `pub(crate) fn regime_at(&self, cell: CellId) -> Option<Negations>`, and `pub fn sites(&self) -> Vec<StrangeSite>`.

**Algorithm (per spec §7.2):** (1) score every land cell's *eligibility* for each exotic regime from fields; (2) **founder floor** — for each regime, reserve its single most-eligible cell; (3) **field-weighted blue-noise** — walk a seeded permutation of remaining eligible cells, accept while budget mass remains and the cell clears a repulsion radius (measured in integer cell-graph hops), assigning a regime by `weighted_index` over field-weighted regime scores; (4) endemic toggle set from the isolation signal (`endorheic`). All accept/reject comparisons are on `quantize`d values.

- [ ] **Step 1: Add the label** `LOCALE_PLACE` to `streams.rs` (with a `stream_labels()` entry).

- [ ] **Step 2: Write the failing tests** — `windows/locale/src/budget.rs`. A cheap determinism unit test (non-heavy) plus a `heavy:`-tagged census:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Seed, World};
    use hornvale_worldgen::{climate_of, terrain_of};

    fn budget_for(seed: u64) -> (StrangenessBudget, hornvale_climate::GeneratedClimate) {
        let w = World::new(Seed(seed));
        let climate = climate_of(&w).unwrap();
        let terrain = terrain_of(&w).unwrap();
        let b = StrangenessBudget::build(Seed(seed), &climate, &terrain);
        (b, climate)
    }

    #[test]
    fn budget_is_deterministic() {
        let a = budget_for(42).0.sites();
        let b = budget_for(42).0.sites();
        assert_eq!(a, b);
    }

    #[test]
    fn placed_sites_are_a_small_minority() {
        let (b, climate) = budget_for(42);
        let land = climate.geosphere().cells().count(); // upper bound
        assert!(b.sites().len() * 20 < land, "strange sites must stay rare");
    }

    #[test]
    #[ignore = "heavy: live worldgen census over many seeds"]
    fn census_budget_and_spacing_hold_across_seeds() {
        for seed in 0..25 {
            let (b, _climate) = budget_for(seed);
            // every placed regime is a real exotic (never all-mundane)
            for s in b.sites() {
                assert!(
                    s.energy != EnergySource::Sunlit || s.kingdom != Kingdom::PlantAnimal,
                    "a placed site must negate at least one exotic slot"
                );
            }
        }
    }
}
```

- [ ] **Step 3: Run to verify they fail**

Run: `cargo test -p hornvale-locale budget`
Expected: FAIL — types not found.

- [ ] **Step 4: Implement `budget.rs`.** Complete implementation (eligibility, founder floor, field-weighted blue-noise with integer-hop repulsion, endemic from `endorheic`):

```rust
//! The rarity budget (P7): a bounded genesis pass placing rung-30 exotic
//! regimes sparsely and spaced, with a founder floor so a strongly-warranted
//! exotic is never missed by an unlucky draw. All accept/reject comparisons
//! are on quantized values (decision 0041). Placement is *repulsive*; the
//! future negative wing will be *excitatory* — do not bake repulsion deeper.

use crate::regime::{EnergySource, Kingdom, Negations, Substrate};
use crate::streams::LOCALE_PLACE;
use hornvale_climate::GeneratedClimate;
use hornvale_kernel::{CellId, Seed, quantize};
use hornvale_terrain::GeneratedTerrain;
use serde::Serialize;
use std::collections::BTreeMap;

/// Target strangeness mass: at most this fraction of land cells host an exotic.
const BUDGET_FRACTION: f64 = 0.01;
/// Minimum spacing between placed sites, in integer cell-graph hops.
const REPULSION_HOPS: u32 = 3;

/// A placed exotic site — a derived, findable record (never stored in the save).
/// type-audit: bare-ok(index: cell), bare-ok(prose: energy), bare-ok(prose: kingdom), bare-ok(flag: endemic)
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StrangeSite {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// The energy negation, if any.
    pub energy: EnergySource,
    /// The kingdom negation, if any.
    pub kingdom: Kingdom,
    /// Whether the site's biota is endemic.
    pub endemic: bool,
}

/// The placed-regime map for one world.
pub(crate) struct StrangenessBudget {
    sites: BTreeMap<u32, StrangeSite>,
}

/// One candidate exotic regime and its per-cell eligibility score.
struct Candidate {
    energy: EnergySource,
    kingdom: Kingdom,
    score: fn(&GeneratedTerrain, CellId) -> f64,
}

/// The fixed candidate table (natural-tier exotics only).
fn candidates() -> [Candidate; 3] {
    [
        // Chemo/geothermal vents: warranted by tectonic unrest.
        Candidate {
            energy: EnergySource::Geothermal,
            kingdom: Kingdom::PlantAnimal,
            score: |t, c| quantize(*t.globe().unrest.get(c)),
        },
        Candidate {
            energy: EnergySource::Chemosynthetic,
            kingdom: Kingdom::Microbial,
            score: |t, c| quantize(*t.globe().unrest.get(c) * 0.8),
        },
        // Fungal kingdoms: warranted by damp, low-relief, non-volcanic ground.
        Candidate {
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::Fungal,
            score: |t, c| {
                let g = t.globe();
                let relief = quantize(*g.elevation.get(c) - g.sea_level);
                if relief > 0.0 && *g.unrest.get(c) < 0.4 { quantize(1.0 - *g.unrest.get(c)) } else { 0.0 }
            },
        },
    ]
}

impl StrangenessBudget {
    /// Build the placement for a world (bounded O(cells)).
    pub(crate) fn build(
        seed: Seed,
        climate: &GeneratedClimate,
        terrain: &GeneratedTerrain,
    ) -> StrangenessBudget {
        let geo = climate.geosphere();
        let globe = terrain.globe();
        // Land cells only, in CellId order (deterministic).
        let land: Vec<CellId> = geo
            .cells()
            .filter(|&c| quantize(*globe.elevation.get(c)) > quantize(globe.sea_level))
            .collect();
        let cands = candidates();
        let mut sites: BTreeMap<u32, StrangeSite> = BTreeMap::new();
        let mut accepted: Vec<CellId> = Vec::new();

        // (2) Founder floor: reserve each candidate's single most-eligible cell.
        for cand in &cands {
            if let Some(best) = land
                .iter()
                .copied()
                .filter(|&c| (cand.score)(terrain, c) > 0.0)
                .max_by(|&a, &b| (cand.score)(terrain, a).total_cmp(&(cand.score)(terrain, b)).then(b.0.cmp(&a.0)))
            {
                insert_site(&mut sites, &mut accepted, best, cand, *globe.endorheic.get(best));
            }
        }

        // (3) Field-weighted blue-noise over a seeded permutation of land cells.
        let budget = ((land.len() as f64) * BUDGET_FRACTION) as usize;
        let mut stream = seed.derive(LOCALE_PLACE).stream();
        let order = permute(&land, &mut stream);
        for c in order {
            if sites.len() >= budget {
                break;
            }
            if sites.contains_key(&c.0) || within_repulsion(&accepted, c, geo) {
                continue;
            }
            let weights: Vec<f64> = cands.iter().map(|k| (k.score)(terrain, c)).collect();
            if let Some(i) = stream.weighted_index(&weights) {
                insert_site(&mut sites, &mut accepted, c, &cands[i], *globe.endorheic.get(c));
            }
        }
        StrangenessBudget { sites }
    }

    /// The negation vector a placed cell carries, if any.
    pub(crate) fn regime_at(&self, cell: CellId) -> Option<Negations> {
        self.sites.get(&cell.0).map(|s| Negations {
            substrate: Substrate::Ordinary, // substrate comes from the derived proxy
            energy: s.energy,
            kingdom: s.kingdom,
            endemic: s.endemic,
        })
    }

    /// The placed sites, for findability (derived, not stored).
    pub fn sites(&self) -> Vec<StrangeSite> {
        self.sites.values().cloned().collect()
    }
}

fn insert_site(
    sites: &mut BTreeMap<u32, StrangeSite>,
    accepted: &mut Vec<CellId>,
    cell: CellId,
    cand: &Candidate,
    endemic: bool,
) {
    sites.insert(cell.0, StrangeSite { cell: cell.0, energy: cand.energy, kingdom: cand.kingdom, endemic });
    accepted.push(cell);
}

/// A Fisher–Yates permutation off the given stream (deterministic).
fn permute(cells: &[CellId], stream: &mut hornvale_kernel::Stream) -> Vec<CellId> {
    let mut v = cells.to_vec();
    for i in (1..v.len()).rev() {
        let j = (stream.next_u64() % (i as u64 + 1)) as usize;
        v.swap(i, j);
    }
    v
}

/// True if `c` is within REPULSION_HOPS graph-hops of any accepted cell.
/// Uses the geosphere's integer neighbour graph (no transcendentals).
/// `hops_between(a, b, max)` returns `Some(hops)` iff `b` is within `max`
/// hops of `a` (a bounded BFS), else `None`.
fn within_repulsion(accepted: &[CellId], c: CellId, geo: &hornvale_kernel::Geosphere) -> bool {
    accepted
        .iter()
        .any(|&a| geo.hops_between(a, c, REPULSION_HOPS).is_some())
}
```

**Note on `geo.hops_between` and `geo.cells()`:** verify the exact geosphere API — `grep -n "pub fn" kernel/src/geosphere.rs`. If a bounded hop-distance helper does not exist, add a small BFS helper `fn hops_between(&self, a: CellId, b: CellId, max: u32) -> Option<u32>` to `kernel/src/geosphere.rs` as part of this task (with its own TDD test), since repulsion needs it and it is a general geosphere capability. If `cells()` is named differently (e.g. `iter_cells`), use the actual name.

- [ ] **Step 5: Run to verify pass**

Run: `cargo test -p hornvale-locale budget` (heavy census skipped by default)
Expected: PASS (non-ignored tests).

- [ ] **Step 6: Run the census once** to confirm it holds:

Run: `cargo test -p hornvale-locale budget -- --ignored`
Expected: PASS.

- [ ] **Step 7: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/budget.rs windows/locale/src/lib.rs windows/locale/src/streams.rs kernel/src/geosphere.rs
git commit -m "feat(locale): rarity budget + founder floor placement (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 7: Integrate into `describe` — schema v2, strange_sites()

**Files:**
- Modify: `windows/locale/src/lib.rs` (replace `SubCellTexture`/`texture_of`/`aspect_pool` with `Regime`; bump schema; cache the budget in `LocaleContext`; add `strange_sites()`), `windows/locale/src/streams.rs` (remove retired `LOCALE_ASPECT`/`LOCALE_JITTER`)

**Interfaces:**
- Consumes: `Regime` (T2), `substrate_at` (T3), `micro_field` (T4), `derived_regime`/`render` (T5), `StrangenessBudget`/`StrangeSite` (T6).
- Produces: `Locale.regime: Regime` (replacing `texture`), `ROOM_SCHEMA = "locale/room/v2"`, `LocaleContext::strange_sites(&self) -> Vec<StrangeSite>`.

- [ ] **Step 1: Update the failing tests first.** In `windows/locale/src/lib.rs` tests: rename `texture_is_deterministic_and_siblings_differ` to assert on `describe(..).regime.descriptor` and `.regime.strangeness` (siblings differ, strangeness ≥ 0). Keep `blend_and_inheritance_pin_exact_values` unchanged — it pins `fields.temperature_c` and `corners`, which this task does not touch (pin-isolation). Add:

```rust
#[test]
fn schema_is_v2_and_regime_present() {
    let world = land_world();
    let ctx = LocaleContext::build(&world).unwrap();
    let addr = RoomAddr { face: 3, path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] };
    let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
    assert_eq!(loc.schema, "locale/room/v2");
    assert!(loc.regime.strangeness >= 0.0);
    assert!(!loc.regime.descriptor.is_empty());
}

#[test]
fn strange_sites_are_exposed() {
    let world = land_world();
    let ctx = LocaleContext::build(&world).unwrap();
    // A derived query; may be empty on a mundane world but must not panic.
    let _ = ctx.strange_sites();
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-locale --lib`
Expected: FAIL — `regime` field / `strange_sites` missing; schema mismatch.

- [ ] **Step 3: Edit `lib.rs`.** Concretely:
  1. Change `ROOM_SCHEMA` to `"locale/room/v2"`.
  2. In `struct Locale`: replace `pub texture: SubCellTexture,` with `pub regime: Regime,`; update the struct doc + `type-audit` tag (drop the texture entries, add `pending(wave-…)` as appropriate for `regime` sub-fields — keep the existing pattern).
  3. Delete `struct SubCellTexture`, `fn texture_of`, `fn aspect_pool` and the `LOCALE_ASPECT`/`LOCALE_JITTER` uses.
  4. In `LocaleContext`, add a field `budget: StrangenessBudget`; build it in `LocaleContext::build` after `terrain`/`index` via `StrangenessBudget::build(world.seed, &climate, &terrain)`.
  5. In `describe`, after computing `biome` and `best` cell: compute `let substrate = substrate::substrate_at(&self.climate, &self.terrain, best.0); let micro = micro::micro_field(addr.seed(self.seed)); let mut regime = grammar::derived_regime(self.seed, addr, biome, substrate, micro);` then overlay placement: `if let Some(placed) = self.budget.regime_at(best.0) { let negations = Negations { substrate: regime.negations.substrate, energy: placed.energy, kingdom: placed.kingdom, endemic: placed.endemic }; let descriptor = grammar::render(negations, micro, biome, self.seed, addr); regime = Regime { negations, micro, descriptor, strangeness: negations.strangeness() }; }` and set `regime` on the returned `Locale` in place of `texture`.
  6. Add `pub fn strange_sites(&self) -> Vec<StrangeSite> { self.budget.sites() }` and re-export `StrangeSite` from `budget`.
  7. Update `mod`/`pub use` lines so `Regime`, `StrangeSite` are exported.

- [ ] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-locale`
Expected: PASS. In particular `blend_and_inheritance_pin_exact_values` stays green (pin-isolation: strangeness draws are on separate labels).

- [ ] **Step 5: Full gate for the crate + clippy**

Run: `cargo clippy -p hornvale-locale --all-targets -- -D warnings && cargo test -p hornvale-locale`
Expected: clean.

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/locale/src/lib.rs windows/locale/src/streams.rs
git commit -m "feat(locale): integrate regime overlay; schema v2; strange_sites (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 8: CLI readout + artifact regeneration

**Files:**
- Modify: `cli/src/main.rs` (extend `cmd_locale` to render a regime sample; verify the existing flags), `cli/src/streams.rs` if the manifest lists locale labels
- Regenerate: `book/src/reference/locale-seed-42.json`, the stream manifest page, and any locale reference page

**Interfaces:**
- Consumes: `LocaleContext`, `Locale.regime`, `LocaleContext::strange_sites()`.
- Produces: updated committed artifacts; a `--sample N` (or existing) readout showing regimes across biomes.

- [ ] **Step 1: Inspect the current command.** Run `grep -n "fn cmd_locale" cli/src/main.rs` and read it. Identify how it currently emits `locale-seed-42.json`.

- [ ] **Step 2: Extend the readout.** Ensure the JSON/text output now includes the `regime` (descriptor, strangeness, negations) — it will automatically via `serde`, since `Locale` changed. Add a short human-readable sampling mode that walks N addresses across distinct biomes and prints `biome | strangeness | descriptor`, so the variety is visible. Add a `strange_sites` count line.

- [ ] **Step 3: Build the CLI and regenerate artifacts** (mirror the "Artifacts are current" step in `.github/workflows/ci.yml`):

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- locale --world /tmp/hv.json > book/src/reference/locale-seed-42.json  # match the existing invocation
cargo run -p hornvale -- streams > book/src/reference/streams.md  # if that is the manifest path; verify
```

Verify the exact commands against the CI workflow before running; use the ones the workflow uses.

- [ ] **Step 4: Confirm no unexpected drift**

Run: `git diff --stat book/src/`
Expected: only `locale-seed-42.json` and the stream manifest change (the new `locale/regime/*` + `locale/strangeness/place` labels appear; `locale/aspect`,`locale/jitter` disappear).

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add cli/src/main.rs book/src/reference/
git commit -m "feat(cli): locale regime readout; regenerate seed-42 artifact (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 9: Full gate + cross-platform determinism guard

**Files:**
- Modify: `windows/locale/src/budget.rs` (add the determinism guard test)

- [ ] **Step 1: Add a determinism guard test** asserting the placement's discrete decisions are stable — a byte-identity check on the serialized `strange_sites()` for a fixed seed, and that re-building yields identical sites (the cross-platform guarantee rides on the quantize/integer discipline, verified in CI on both platforms):

```rust
#[test]
fn placement_is_byte_identical_on_rebuild() {
    let a = serde_json::to_string(&budget_for(42).0.sites()).unwrap();
    let b = serde_json::to_string(&budget_for(42).0.sites()).unwrap();
    assert_eq!(a, b);
}
```

- [ ] **Step 2: Run the full workspace gate**

Run: `make gate`
Expected: PASS — fmt clean, clippy `-D warnings` clean, `cargo nextest run --workspace` green (heavy census skipped), doctests green.

- [ ] **Step 3: Run the heavy tier once**

Run: `cargo test -p hornvale-locale -- --ignored`
Expected: PASS.

- [ ] **Step 4: commit**

```bash
git add windows/locale/src/budget.rs
git commit -m "test(locale): placement determinism guard; full gate green (MAP-29)

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

### Task 10: Definition of Done (book, retrospective, registry)

**Files:**
- Create: `book/src/chronicle/the-uncommon-ground.md`, `docs/retrospectives/<seq>-the-uncommon-ground.md`
- Modify: `book/src/frontier/idea-registry.md` (MAP-29 status), touched book chapters (locale reference)

- [ ] **Step 1: Chronicle entry** — write `book/src/chronicle/the-uncommon-ground.md` at the book's altitude (technical, comprehensible without the code): the negation-slot engine, the derived/placed split, the founder floor, the micro-field answer to "interesting sameness," and what was deferred. Add it to `book/src/SUMMARY.md`.

- [ ] **Step 2: Retrospective** — `docs/retrospectives/<next-seq>-the-uncommon-ground.md`: process lessons (the four-ideonomy-pass design; the substrate-proxy DOM-14 seam; the terrain-signal reuse that dissolved the open question).

- [ ] **Step 3: Registry flip** — in `book/src/frontier/idea-registry.md`, update the MAP-29 row status from `elaborated` to note *partially shipped* (rungs 15–30, steady), pointing at the chronicle; keep the deferred seams (relict→MAP-30, halos→P5, phase→P8, metaphysics wings) visible. Do NOT cite the registry ID outside `book/src/frontier/`.

- [ ] **Step 4: Book freshness + drift-check**

Run: `cargo test -p hornvale --test docs_consistency && mdbook build book`
Expected: PASS / builds.

- [ ] **Step 5: Full gate + artifact drift**

Run: `make gate && git diff --exit-code book/src/gallery/ book/src/reference/`
Expected: PASS; no unexpected drift.

- [ ] **Step 6: commit**

```bash
git add book/ docs/retrospectives/
git commit -m "docs: The Uncommon Ground chronicle, retrospective, MAP-29 registry flip

Claude-Session: https://claude.ai/code/session_01MjffNpNSyusocqiUKB53ca"
```

---

## Self-Review notes (for the executor)

- **Spec coverage:** kernel weighted-choice (T1); negation-slot engine + magnitude (T2); substrate proxy/DOM-14 seam (T3); micro-field (T4); descriptor grammar + derived tier + local-variety guard (T5); rarity budget + founder floor + field-weighting + repulsion + endemic gate + strange_sites (T6); integration + schema v2 + pin-isolation (T7); observation surface + artifacts (T8); determinism guard + full gate (T9); DoD (T10).
- **Deferred, intentionally not in any task:** relict/palimpsest (MAP-30), ephemeral/phase (P8), engineered (settlement), aetheric/faerie/negative wing (metaphysics gate), halos/core→margin/contagion (P5), true lithology (DOM-14), Namer refactor, Grammar-to-kernel promotion, province-nested budget.
- **API verifications the executor must do before coding the dependent step:** `Geosphere::cells()` iterator name and a bounded `hops_between` (T6 Step 4 note); `GeneratedTerrain::globe()` return type is `&TectonicGlobe` (used by T3/T6); the exact `cmd_locale` invocation the CI workflow uses (T8 Step 3).
