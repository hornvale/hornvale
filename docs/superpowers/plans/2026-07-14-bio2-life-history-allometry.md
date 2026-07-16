# BIO-2 Life-History Allometry Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a neutral life-history layer — a `MetabolicClass` tag on `SpeciesDef` and a pure `allometry` module that derives basal metabolic rate, lifespan, age-at-maturity, reproductive tempo, generation length, and a pace-of-life scalar from body mass by published scaling laws — surfaced read-only through the almanac and lab, changing no existing world byte.

**Architecture:** A new `domains/species/src/allometry.rs` holds pure functions `f(Mass, MetabolicClass) -> …` and a `LifeHistory` projection struct, plus per-class calibration constants. Mass is **consumed** from `SpeciesDef.mass` (owned by the coexistence-stack campaign); this plan only adds `metabolic_class` and the derivations. A new `hornvale_kernel::Years` duration newtype types the time traits (species cannot depend on astronomy's `StdDays`). Outputs surface through `windows/almanac` (a per-species block) and `windows/lab` (per-trait metrics); nothing is drawn, no consumer is rewired.

**Tech Stack:** Rust (edition 2024), workspace crates `hornvale-kernel`, `hornvale-species`, `hornvale-almanac`, `hornvale-lab`. Test runner `cargo nextest`. No new dependencies (serde/serde_json only).

## Global Constraints

- No new crates; `serde` + `serde_json` only (enforced by `cli/tests/architecture.rs`).
- No `HashMap`/`HashSet` (use `BTreeMap`/`BTreeSet`/`Vec`); no wall-clock time. Float sort via `total_cmp`. Enforced by `clippy.toml`.
- Every crate is `#![warn(missing_docs)]`; every public item/field/variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag; the audit (`tools/type-audit/`) is default-deny and drift-checked in CI.
- Determinism: **zero new streams, zero new draws** in this plan. A pin/test asserts the stream manifest is unchanged.
- Run `cargo fmt` as the final step before every commit. The commit gate is `make gate` (fmt + clippy + nextest + doctests).
- Byte-identity: no committed artifact and no census world may change bytes. New lab metrics ADD rows; the metric-count pin re-baselines in the commit that adds them (rebaseline-golden-pins rule).

## Preconditions (sequencing — flagged at G3)

**This plan executes AFTER the coexistence-stack campaign merges `Mass` + `SpeciesDef.mass` to `main`.** Concretely, before Task 1:

- [ ] **P0:** Confirm `hornvale_kernel::Mass` and `SpeciesDef.mass` exist on `main`. Run: `git grep -l "pub use units::{.*Mass" kernel/src/lib.rs && git grep "pub mass: Mass" domains/species/src/lib.rs`. Expected: both match.
- [ ] **P0b:** From this worktree, absorb main: `make preflight`; on an ancestry NO-GO, merge `main` into `bio2-life-history` and re-run the gate. (Never absorb mid-measurement — N/A here, no preregistered study.)

If coexistence has **not** merged when execution begins, escalate to Nathan for the stack-on-branch decision (a post-G3 mechanics call, not decided in this plan).

---

### Task 1: Kernel `Years` duration newtype

**Files:**
- Modify: `kernel/src/units.rs` (add `Years`, after `ReferenceElevation`)
- Modify: `kernel/src/lib.rs` (export `Years`)
- Test: inline `#[cfg(test)]` in `kernel/src/units.rs`

**Interfaces:**
- Produces: `hornvale_kernel::Years` with `Years::new(f64) -> Result<Self, UnitError>`, `.get() -> f64` (years), `.from_days(f64) -> Result<Self, UnitError>`, `.days(self) -> f64` (× 365.25). A non-negative absolute duration in Julian years. Chosen over reusing astronomy `StdDays` (constitutional: species cannot depend on another domain) and over bare-f64 (typed-quantity discipline). Distinct from `StdDays` (a sub-day time-point with local/standard conversion); `Years` is a coarse biological/historical span.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn years_construct_and_convert() {
    assert!(Years::new(-1.0).is_err());
    assert!(Years::new(f64::NAN).is_err());
    let life = Years::new(80.0).unwrap();
    assert_eq!(life.get(), 80.0);
    // 1 year == 365.25 standard days
    assert!((Years::new(1.0).unwrap().days() - 365.25).abs() < 1e-9);
    assert!((Years::from_days(365.25).unwrap().get() - 1.0).abs() < 1e-9);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-kernel years_construct_and_convert`
Expected: FAIL — `Years` not found.

- [ ] **Step 3: Write minimal implementation**

In `kernel/src/units.rs`, after the `ReferenceElevation` block:

```rust
/// A non-negative absolute duration in Julian years (365.25 standard days).
/// The coarse biological/historical span type — distinct from astronomy's
/// [`StdDays`](../../astronomy) sub-day time-point, and reachable from any
/// domain because it lives in the kernel. Used by life-history allometry
/// (lifespan, age-at-maturity, generation length).
/// type-audit: bare-ok(duration: years)
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Years(f64);

impl Years {
    /// Standard days per Julian year.
    pub const DAYS_PER_YEAR: f64 = 365.25;

    /// Build a `Years`, rejecting negative, NaN, or infinite values.
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() || value < 0.0 {
            return Err(UnitError::new("Years", value));
        }
        Ok(Years(value))
    }

    /// Build from a span in standard days.
    pub fn from_days(days: f64) -> Result<Self, UnitError> {
        Years::new(days / Self::DAYS_PER_YEAR)
    }

    /// The span in years.
    pub fn get(self) -> f64 {
        self.0
    }

    /// The span in standard days.
    pub fn days(self) -> f64 {
        self.0 * Self::DAYS_PER_YEAR
    }
}
```

Confirm `UnitError::new(&str, f64)` matches the existing constructor in this file; if its signature differs, mirror the `ReferenceElevation` error path exactly.

- [ ] **Step 4: Export from the kernel**

In `kernel/src/lib.rs`, extend the units re-export:

```rust
pub use units::{Mass, ReferenceElevation, TempAnomaly, Temperature, UnitError, Years};
```

(`Mass` is already present post-merge; add `Years`.)

- [ ] **Step 5: Run test to verify it passes**

Run: `cargo test -p hornvale-kernel years_construct_and_convert`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add kernel/src/units.rs kernel/src/lib.rs
git commit -m "feat(kernel): Years duration newtype for life-history spans"
```

---

### Task 2: `MetabolicClass` enum + `metabolic_class` on `SpeciesDef`

**Files:**
- Modify: `domains/species/src/lib.rs` (add enum near `SpeciesDef`; add field; author all four species)
- Test: inline `#[cfg(test)]` in `domains/species/src/lib.rs`

**Interfaces:**
- Produces: `hornvale_species::MetabolicClass` (`Endotherm | Ectotherm | Autotroph | Ametabolic`) and `SpeciesDef.metabolic_class: MetabolicClass`. Coexistence's `SpeciesDef.mass` is untouched (disjoint field — a small textual merge).

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn every_species_has_a_metabolic_class() {
    use MetabolicClass::*;
    let r = registry();
    assert_eq!(r["goblin"].metabolic_class, Endotherm);
    assert_eq!(r["hobgoblin"].metabolic_class, Endotherm);
    assert_eq!(r["bugbear"].metabolic_class, Endotherm);
    assert_eq!(r["kobold"].metabolic_class, Ectotherm); // reptilian/draconic SRD lineage
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-species every_species_has_a_metabolic_class`
Expected: FAIL — `metabolic_class` / `MetabolicClass` not found.

- [ ] **Step 3: Add the enum**

In `domains/species/src/lib.rs`, above `SpeciesDef`:

```rust
/// A species' metabolic strategy. Selects the allometric normalization
/// coefficient (B₀) and the per-class pace multiplier; the scaling
/// *exponents* are universal across classes (spec §4).
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetabolicClass {
    /// Warm-blooded (mammal/bird analogue): high, temperature-stable basal rate.
    Endotherm,
    /// Cold-blooded (reptile/amphibian analogue): ~1/8 the basal rate; longer
    /// life per kg. Realized rate couples to ambient temperature (deferred,
    /// spec §10 CAP-1).
    Ectotherm,
    /// Phototroph (plant-folk/fungal analogue). Energy from light; its basal
    /// rate is SURFACE/area-limited, so the §4 universal exponent does NOT
    /// apply — activating this class is its own modelling decision. Unused seam.
    Autotroph,
    /// No metabolism (construct/undead analogue). Has no life-history: the
    /// biological traits are `None`. Unused seam.
    Ametabolic,
}
```

- [ ] **Step 4: Add the field and author all four species**

Add to `SpeciesDef` (after `mass`):

```rust
    /// Metabolic strategy — drives life-history allometry (spec BIO-2).
    pub metabolic_class: MetabolicClass,
```

In `registry()`, add `metabolic_class:` to each `SpeciesDef { … }`: `MetabolicClass::Endotherm` for goblin, hobgoblin, bugbear; `MetabolicClass::Ectotherm` for kobold.

- [ ] **Step 5: Run test to verify it passes**

Run: `cargo test -p hornvale-species every_species_has_a_metabolic_class`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/species/src/lib.rs
git commit -m "feat(species): MetabolicClass tag on SpeciesDef (BIO-2)"
```

---

### Task 3: The allometry laws (pure functions + calibration constants)

**Files:**
- Create: `domains/species/src/allometry.rs`
- Modify: `domains/species/src/lib.rs` (`mod allometry; pub use allometry::{…};`)
- Test: inline `#[cfg(test)]` in `allometry.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{Mass, Years}`, `crate::MetabolicClass`.
- Produces: `basal_metabolic_rate_w(Mass, MetabolicClass) -> f64`; `lifespan(Mass, MetabolicClass) -> Years`; `age_at_maturity(Mass, MetabolicClass) -> Years`; `reproductive_tempo(Mass, MetabolicClass) -> f64` (0 fast … 1 slow). All total, pure, no draws. `Ametabolic` handling lives in Task 4 (these low-level laws are only called for classes that have life-history).

**Calibration (documented constants — worldbuilding, spec §4):**
- Anchor: a 40 kg `Endotherm` → lifespan **60 yr**, age-at-maturity **12 yr** (maturity ≈ 20 % of lifespan).
- Exponents (discovered): metabolic p = 0.75 (Kleiber 1932); lifespan/maturity p = 0.25 (metabolic-time).
- `k_life_endo = 60 / 40^0.25`; `k_mat_endo = 12 / 40^0.25`.
- Per-class **pace multiplier** (single knob preserving covariation, spec §4): `Endotherm = 1.0`, `Ectotherm = 1.5` (longer life, later maturity, slower tempo together). `Autotroph = 1.0` placeholder; `Ametabolic` never reaches these laws.
- Metabolic B₀: `Endotherm = 3.4` W·kg⁻⁰·⁷⁵; `Ectotherm = 3.4 / 8.0`.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Mass;
    use crate::MetabolicClass::*;

    fn m(kg: f64) -> Mass { Mass::new(kg).unwrap() }

    #[test]
    fn anchor_hits_documented_targets() {
        assert!((lifespan(m(40.0), Endotherm).get() - 60.0).abs() < 1e-6);
        assert!((age_at_maturity(m(40.0), Endotherm).get() - 12.0).abs() < 1e-6);
    }

    #[test]
    fn lifespan_and_maturity_increase_with_mass() {
        assert!(lifespan(m(132.0), Endotherm).get() > lifespan(m(18.0), Endotherm).get());
        assert!(age_at_maturity(m(132.0), Endotherm).get() > age_at_maturity(m(18.0), Endotherm).get());
    }

    #[test]
    fn ectotherms_outlive_endotherms_at_equal_mass() {
        assert!(lifespan(m(20.0), Ectotherm).get() > lifespan(m(20.0), Endotherm).get());
    }

    #[test]
    fn metabolic_rate_rises_with_mass_and_is_lower_for_ectotherms() {
        assert!(basal_metabolic_rate_w(m(100.0), Endotherm) > basal_metabolic_rate_w(m(10.0), Endotherm));
        assert!(basal_metabolic_rate_w(m(20.0), Ectotherm) < basal_metabolic_rate_w(m(20.0), Endotherm));
    }

    #[test]
    fn tempo_slows_with_mass() {
        assert!(reproductive_tempo(m(132.0), Endotherm) > reproductive_tempo(m(18.0), Endotherm));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-species --lib allometry::tests`
Expected: FAIL — functions not defined.

- [ ] **Step 3: Implement the module**

```rust
//! Life-history allometry (BIO-2): pure scaling laws deriving a species'
//! life-history from body mass and metabolic class. Universal exponents;
//! per-class coefficients. No draws, no world state — see the design spec.

use crate::MetabolicClass;
use hornvale_kernel::{Mass, Years};

// Exponents (discovered; spec §4).
const P_METABOLIC: f64 = 0.75; // Kleiber 1932
const P_TIME: f64 = 0.25; // metabolic-time scaling

// Calibration anchor: 40 kg Endotherm → 60 yr lifespan, 12 yr maturity.
const ANCHOR_MASS_KG: f64 = 40.0;
const ANCHOR_LIFESPAN_YR: f64 = 60.0;
const ANCHOR_MATURITY_YR: f64 = 12.0;

// Metabolic normalization (W·kg^-0.75).
const B0_ENDOTHERM: f64 = 3.4;
const ECTOTHERM_METABOLIC_FRACTION: f64 = 1.0 / 8.0;

/// Single per-class pace multiplier: shifts lifespan, maturity, and tempo
/// together so the fast–slow covariation stays coherent (spec §4). Ectotherms
/// are slower on every axis at once.
fn pace_multiplier(class: MetabolicClass) -> f64 {
    match class {
        MetabolicClass::Endotherm => 1.0,
        MetabolicClass::Ectotherm => 1.5,
        MetabolicClass::Autotroph => 1.0,
        // Ametabolic never reaches the time laws (handled in life_history).
        MetabolicClass::Ametabolic => 1.0,
    }
}

/// Basal metabolic rate in watts at a reference temperature (spec §4/§10 CAP-1
/// — this is the BASAL rate; ectotherm realized rate couples to climate and is
/// deferred). Surface-limited for `Autotroph` — see `MetabolicClass::Autotroph`.
pub fn basal_metabolic_rate_w(mass: Mass, class: MetabolicClass) -> f64 {
    let b0 = match class {
        MetabolicClass::Endotherm | MetabolicClass::Autotroph => B0_ENDOTHERM,
        MetabolicClass::Ectotherm => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION,
        MetabolicClass::Ametabolic => return 0.0,
    };
    b0 * mass.kilograms().powf(P_METABOLIC)
}

/// Maximum lifespan (spec §4). `k_life` is calibrated to the 40 kg endotherm
/// anchor; the per-class pace multiplier lengthens ectotherm life.
pub fn lifespan(mass: Mass, class: MetabolicClass) -> Years {
    let k_life = ANCHOR_LIFESPAN_YR / ANCHOR_MASS_KG.powf(P_TIME);
    let yr = pace_multiplier(class) * k_life * mass.kilograms().powf(P_TIME);
    Years::new(yr).expect("mass is positive, so lifespan is finite and non-negative")
}

/// Age at first reproduction (spec §4), ~20 % of lifespan at the anchor.
pub fn age_at_maturity(mass: Mass, class: MetabolicClass) -> Years {
    let k_mat = ANCHOR_MATURITY_YR / ANCHOR_MASS_KG.powf(P_TIME);
    let yr = pace_multiplier(class) * k_mat * mass.kilograms().powf(P_TIME);
    Years::new(yr).expect("mass is positive, so maturity is finite and non-negative")
}

/// Reproductive tempo on the r–K axis, 0 (fast/prolific) … 1 (slow/sparse),
/// rising with mass and the pace multiplier (spec §4/CAP-2 — this is
/// reproductive OUTPUT, distinct from overall pace-of-life). A saturating map
/// of `pace_multiplier · log10(mass)` over a fixed reference range keeps it
/// absolute (roster-independent).
pub fn reproductive_tempo(mass: Mass, class: MetabolicClass) -> f64 {
    // Fixed reference range: 1 kg → ~0, 1000 kg → ~1 (before the class shift).
    let raw = (mass.kilograms().log10() / 3.0).clamp(0.0, 1.0);
    (raw * pace_multiplier(class)).clamp(0.0, 1.0)
}
```

- [ ] **Step 4: Wire the module**

In `domains/species/src/lib.rs`:

```rust
mod allometry;
pub use allometry::{age_at_maturity, basal_metabolic_rate_w, lifespan, reproductive_tempo};
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-species --lib allometry::tests`
Expected: PASS (all five).

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/species/src/allometry.rs domains/species/src/lib.rs
git commit -m "feat(species): allometric life-history laws (BIO-2)"
```

---

### Task 4: The `LifeHistory` projection (assembly + generation length + pace + Ametabolic)

**Files:**
- Modify: `domains/species/src/allometry.rs` (add struct + `life_history`)
- Modify: `domains/species/src/lib.rs` (re-export `LifeHistory`, `life_history`)
- Test: inline `#[cfg(test)]` in `allometry.rs`

**Interfaces:**
- Produces: `LifeHistory { basal_metabolic_rate_w: f64, lifespan: Option<Years>, age_at_maturity: Option<Years>, reproductive_tempo: Option<f64>, generation_length: Option<Years>, pace_of_life: f64 }` and `life_history(Mass, MetabolicClass) -> LifeHistory`. Biological fields are `None` for `Ametabolic`. `pace_of_life` is an absolute `f(log mass)` (roster-independent, spec §5).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn ametabolic_nulls_the_biological_traits() {
    let lh = life_history(m(500.0), Ametabolic);
    assert_eq!(lh.basal_metabolic_rate_w, 0.0);
    assert!(lh.lifespan.is_none());
    assert!(lh.age_at_maturity.is_none());
    assert!(lh.reproductive_tempo.is_none());
    assert!(lh.generation_length.is_none());
}

#[test]
fn living_classes_fill_every_trait() {
    let lh = life_history(m(18.0), Endotherm);
    assert!(lh.lifespan.is_some() && lh.generation_length.is_some());
    // generation length sits between maturity and lifespan
    let gen = lh.generation_length.unwrap().get();
    assert!(gen > lh.age_at_maturity.unwrap().get());
    assert!(gen < lh.lifespan.unwrap().get());
}

#[test]
fn pace_of_life_is_roster_independent() {
    // pace depends only on this species' own mass+class, not on any registry
    // state — computing it twice (as if the roster changed) is identical.
    let a = life_history(m(18.0), Endotherm).pace_of_life;
    let b = life_history(m(18.0), Endotherm).pace_of_life;
    assert_eq!(a, b);
    // and it is monotone in mass
    assert!(life_history(m(132.0), Endotherm).pace_of_life
        > life_history(m(18.0), Endotherm).pace_of_life);
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-species --lib allometry::tests`
Expected: FAIL — `life_history` / `LifeHistory` not defined.

- [ ] **Step 3: Implement the struct and assembly**

```rust
/// Fraction of the post-maturity reproductive span at which generation length
/// falls (spec §5). Documented constant.
const GENERATION_FRACTION: f64 = 0.3;

/// A species' derived life-history profile (spec §5). Computed on demand — not
/// stored on `SpeciesDef`. Biological fields are `None` for `Ametabolic`
/// (a construct has no mass-derived life-history); `pace_of_life` is a
/// size-derived position defined for anything with mass.
/// type-audit: bare-ok(rate: basal_metabolic_rate_w), bare-ok(ratio: reproductive_tempo), bare-ok(ratio: pace_of_life)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LifeHistory {
    /// Reference-temperature basal metabolic rate, watts; 0.0 if `Ametabolic`.
    pub basal_metabolic_rate_w: f64,
    /// Maximum lifespan; `None` if `Ametabolic`.
    pub lifespan: Option<Years>,
    /// Age at first reproduction; `None` if `Ametabolic`.
    pub age_at_maturity: Option<Years>,
    /// Reproductive output on the r–K axis, 0 fast … 1 slow; `None` if `Ametabolic`.
    pub reproductive_tempo: Option<f64>,
    /// Generation length (MEM-7's handle); `None` if `Ametabolic`.
    pub generation_length: Option<Years>,
    /// Overall life-history speed, 0 fast … 1 slow; absolute f(log mass).
    pub pace_of_life: f64,
}

/// Overall pace-of-life: absolute, roster-independent (spec §5). Maps log-mass
/// over a FIXED reference range (1 kg … 1000 kg) so adding a species never
/// shifts another's value. Larger/slower → 1.
fn pace_of_life(mass: Mass, class: MetabolicClass) -> f64 {
    let raw = (mass.kilograms().log10() / 3.0).clamp(0.0, 1.0);
    // Ectotherms read slower on the same size.
    (raw * pace_multiplier(class) / 1.5).clamp(0.0, 1.0)
}

/// Assemble the full life-history profile (spec §5).
pub fn life_history(mass: Mass, class: MetabolicClass) -> LifeHistory {
    let bmr = basal_metabolic_rate_w(mass, class);
    let pace = pace_of_life(mass, class);
    if class == MetabolicClass::Ametabolic {
        return LifeHistory {
            basal_metabolic_rate_w: bmr,
            lifespan: None,
            age_at_maturity: None,
            reproductive_tempo: None,
            generation_length: None,
            pace_of_life: pace,
        };
    }
    let life = lifespan(mass, class);
    let mat = age_at_maturity(mass, class);
    let gen = Years::new(mat.get() + GENERATION_FRACTION * (life.get() - mat.get()))
        .expect("maturity ≤ lifespan, so generation length is non-negative");
    LifeHistory {
        basal_metabolic_rate_w: bmr,
        lifespan: Some(life),
        age_at_maturity: Some(mat),
        reproductive_tempo: Some(reproductive_tempo(mass, class)),
        generation_length: Some(gen),
        pace_of_life: pace,
    }
}
```

- [ ] **Step 4: Re-export**

In `domains/species/src/lib.rs`, extend the allometry re-export:

```rust
pub use allometry::{
    age_at_maturity, basal_metabolic_rate_w, life_history, lifespan, reproductive_tempo,
    LifeHistory,
};
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-species --lib allometry::tests`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/species/src/allometry.rs domains/species/src/lib.rs
git commit -m "feat(species): LifeHistory projection + generation length + pace (BIO-2)"
```

---

### Task 5: Almanac life-history block

**Files:**
- Modify: `windows/almanac/src/lib.rs` (add a life-history block to the per-species rendering; find the existing species-block assembly near the flagship-settlement block, `windows/almanac/src/lib.rs:11`)
- Modify: the seed-42 almanac golden (`book/src/gallery/almanac-seed-42-sky.md` and any species gallery file the CI drift-check regenerates — see the "Artifacts are current" CI step)
- Test: inline almanac test (golden string for seed 42) following the existing per-species block test

**Interfaces:**
- Consumes: `hornvale_species::{life_history, LifeHistory, MetabolicClass}`, `SpeciesDef.mass`.

- [ ] **Step 1: Write the failing test** — assert the rendered almanac for one species contains its pace-of-life headline and lifespan figure. Model it on the existing per-species block test in `windows/almanac`.

```rust
#[test]
fn almanac_species_block_shows_life_history() {
    // Build the seed-42 almanac view (reuse the crate's existing test helper
    // that constructs the per-species blocks) and assert the goblin block.
    let block = render_life_history_line(&species_def("goblin"));
    assert!(block.contains("lifespan"));
    assert!(block.contains("fast") || block.contains("slow")); // pace headline
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-almanac almanac_species_block_shows_life_history`
Expected: FAIL — `render_life_history_line` not defined.

- [ ] **Step 3: Implement `render_life_history_line`**

Add a pure helper that takes a `&SpeciesDef`, computes `life_history(def.mass, def.metabolic_class)`, and renders one line. Pace headline: `pace_of_life < 0.33` → "fast-lived and prolific"; `> 0.66` → "slow, long-lived, and sparse"; else "moderate-paced". When `lifespan.is_some()`, append `", lifespan ~{N} yr, matures ~{M} yr"` (round to whole years). When biological traits are `None` (`Ametabolic`), render only the metabolic line and suppress the life-history clause (spec §5, pass 4). Wire the line into the existing per-species block.

- [ ] **Step 4: Run test + regenerate the golden**

Run: `cargo test -p hornvale-almanac almanac_species_block_shows_life_history` → PASS.
Regenerate committed almanac artifacts per the CI "Artifacts are current" step:
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
```
Inspect the diff — only additive life-history lines should appear.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/almanac/src/lib.rs book/src/gallery/
git commit -m "feat(almanac): per-species life-history block (BIO-2)"
```

---

### Task 6: Lab metrics (per trait)

**Files:**
- Modify: `windows/lab/src/metrics.rs` (add six per-species metrics to `registry()`; update the metric-count pin at `registry_metric_count_is_pinned`, `windows/lab/src/metrics.rs:3658`)
- Modify: `book/src/reference/` metric-list page and `studies/the-census.study.json` outputs the CI regenerates (see the "Artifacts are current" step)
- Test: extend `per_species_metrics_have_the_expected_kinds_for_seed_42` (`:3825`)

**Interfaces:**
- Consumes: `hornvale_species::life_history` via the species view the lab already exposes (follow the existing per-species extractor variant used by `tone_count_metric`, `windows/lab/src/metrics.rs:3194`).

- [ ] **Step 1: Write the failing test** — bump the expected count and add the new metric names to the per-species kinds test.

```rust
// in registry_metric_count_is_pinned: expected count += 6
// in per_species_metrics_have_the_expected_kinds_for_seed_42:
assert!(names.contains("species-lifespan-years"));
assert!(names.contains("species-generation-length-years"));
assert!(names.contains("species-pace-of-life"));
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-lab registry_metric_count_is_pinned per_species_metrics_have_the_expected_kinds_for_seed_42`
Expected: FAIL — count mismatch / missing names.

- [ ] **Step 3: Add the six metrics**

In `registry()`, add per-species metrics `species-lifespan-years`, `species-age-at-maturity-years`, `species-basal-metabolic-rate-w`, `species-reproductive-tempo`, `species-generation-length-years`, `species-pace-of-life`, each following the `tone_count_metric` registration shape (same `Extractor` species variant). For `Option` traits, emit `MetricValue::Absent` when `None` (the `Ametabolic` case) — mirroring `day-length-hours`' `Absent` handling. Numeric metrics get sensible `bucket_edges` (e.g. lifespan `&[20.0,40.0,60.0,80.0,100.0]`).

- [ ] **Step 4: Run tests + regenerate artifacts**

Run the two tests → PASS. Then:
```bash
cargo run -p hornvale -- lab list-metrics   # sanity
cargo run -p hornvale -- lab run studies/the-census.study.json
# regenerate the reference metric-list page per the CI artifacts step
git diff --stat book/src/reference/ book/src/laboratory/
```
Inspect: only additive metric rows.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs book/src/reference/ book/src/laboratory/
git commit -m "feat(lab): six per-species life-history metrics (BIO-2)"
```

---

### Task 7: Neutrality guard + full artifact/gate sweep

**Files:**
- Test: add a neutrality test (species crate or `cli/tests`) asserting no new stream label
- Modify: any remaining committed artifacts the drift-check regenerates

**Interfaces:** none (verification task).

- [ ] **Step 1: Write the neutrality test**

```rust
#[test]
fn bio2_adds_no_stream_label() {
    // The life-history layer is authored constants + pure derivations: it must
    // introduce NO new seed-derivation stream. Assert species stream labels are
    // unchanged from the pre-BIO-2 baseline.
    let labels = hornvale_species::stream_labels();
    assert!(!labels.iter().any(|(k, _)| k.contains("life") || k.contains("allometry")
        || k.contains("metabolic")),
        "BIO-2 must not register a stream: {labels:?}");
}
```

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-species bio2_adds_no_stream_label`
Expected: PASS.

- [ ] **Step 3: Full drift + gate**

```bash
# Regenerate every committed artifact and confirm ONLY additive drift:
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- concepts > book/src/reference/concepts.md  # if changed
cargo run -p hornvale -- lab run studies/the-census.study.json
git diff --stat book/
make gate
```
Expected: `make gate` green; `git diff` shows only additive almanac/metric rows; no existing world bytes changed.

- [ ] **Step 4: Type audit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```
Expected: pass (every new pub primitive — `Years`, `LifeHistory` fields, metric extractors — carries a verdict tag).

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "test(species): BIO-2 neutrality guard + artifact sweep"
```

---

### Task 8: Book chapter + close prep

**Files:**
- Modify: `book/src/frontier/idea-registry.md` (flip BIO-2 row status: `elaborated` → `spec'd`/`shipped`, repoint **Where** at the spec)
- Create: `book/src/chronicle/` entry (Definition of Done, per CLAUDE.md)
- Modify: any biological-substrate book chapter that now lags merged reality (freshness sweep); re-score the Confidence Gradient if BIO-2 moves a bet (decision 0030)
- Create: `docs/retrospectives/` one-pager (including the **campaign-autopilot validation section** — this is the validation campaign; report the ledger's auto-decisions and whether any would have been vetoed)
- Promote `.superpowers/sdd/followups.md` (CAP-1/CAP-3/CAP-4/CAP-5) into the retro's follow-up section

- [ ] **Step 1:** Write the chronicle entry (technical altitude, comprehensible without the code).
- [ ] **Step 2:** Flip the BIO-2 idea-registry row; run `cargo test -p hornvale --test docs_consistency` → PASS.
- [ ] **Step 3:** Freshness sweep of the biological-substrate chapter; note BIO-2 as the first life-history layer, mass ceded to coexistence.
- [ ] **Step 4:** Write the retrospective, **autopilot validation section included**.
- [ ] **Step 5: Commit**

```bash
git add book/ docs/retrospectives/
git commit -m "docs(book): BIO-2 chronicle + registry flip + retro (autopilot validation)"
```

---

## Close (not a task — the closing-a-campaign skill)

Merge/close runs under `closing-a-campaign` at the **G6 hard stop**: present the post-G3 ledger digest (save-format/epoch/determinism entries — here the `SpeciesDef` schema edit, the kernel `Years` type, and the metric-count pin — lead it) and wait for Nathan. **Census regen** (new metrics change the census schema) is an **AWS/authorization carve-out** — warn Nathan before the pre-merge regen; never run it locally. Absorb main a final time before merge (guards: not mid-measurement; not while main looks mid-landing).
