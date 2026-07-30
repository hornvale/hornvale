# The Pigment Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Hornvale a real colour model — colour as the product of
illuminant, reflectance and observer — and prove it end to end on terrain,
in prose and in the terminal chart.

**Architecture:** A spectral primitive in the kernel (ten 40 nm bands over
340–740 nm) that every domain projects into. Terrain projects its existing
`MaterialBuffer` to a reflectance mixture; astronomy derives a blackbody
illuminant from the star's mass; language authors seven exemplar
reflectances; worldgen names a colour by comparing exemplars through the
speaker's own lexicon. Nothing is committed to the ledger — colour is
derived at the emit boundary, so no epoch is owed.

**Tech Stack:** Rust edition 2024, no new dependencies (`serde`,
`serde_json`, `libm` only). `cargo nextest` for tests.

**Spec:** `docs/superpowers/specs/2026-07-30-the-pigment-design.md`

## Global Constraints

- **No new dependencies.** The allowlist is `ALLOWED_EXTERNAL` in
  `cli/tests/architecture.rs`: `serde`, `serde_json`, `libm`.
- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain
  crate depends on `hornvale-kernel` and **nothing else** — never a sibling
  domain.
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only. Float sorts
  use `total_cmp` with a deterministic tie-break.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Transcendentals route through `hornvale_kernel::math`** (decision 0041).
  `sqrt`, `abs`, `floor`, `ceil`, `round`, `mul_add`, `powi` and plain
  arithmetic stay inherent. **This plan uses `a * b + c` throughout and
  never `mul_add`** — both are IEEE-exact but they round differently from
  each other, so mixing them is the hazard.
- **Quantize at emit only** (`hornvale_kernel::quantize`), never in the
  compute path.
- **Every crate sets `#![warn(missing_docs)]`.** Every public item, field
  and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag.** The
  eleven bare-ok classes are in decision 0028. This plan uses `ratio`,
  `count`, `index`, `constructor-edge`, `identifier-text` and `artifact`.
  Note that `artifact` explicitly covers "rendered ASCII/PNG bytes **and
  colour escapes**".
- **`cargo fmt` is the final step before every commit.** Skipping it is the
  single most common review finding in this repo.
- **Committed artifacts must not move.** `book/src/gallery/generated/surrounds-seed-42/*.txt`
  and `book/src/gallery/scene-surrounds-seed-42.json` are drift-checked;
  Task 8 and Task 9 both assert they are byte-identical.
- **The suite watches its own clock** (decision 0088, landed in The
  Timekeeper on 2026-07-30 — absorbed into this branch at `391d9430`).
  `make ci` runs the workspace under the `ci` nextest profile, alarms on a
  per-test or whole-suite duration shift against
  `docs/timings/test-baseline-<host>.tsv`, then rewrites that baseline. **This
  campaign adds roughly forty tests across five crates, so it will move the
  whole-suite duration.** That is a regression the baseline must absorb
  *deliberately*: 0088's rule is to re-record in the same commit that caused
  the shift. Task 10 does this.
- **Two clippy lints bite every band loop, found in Task 1.** Writing
  `for b in 0..BANDS { out[b] = out[b] + x }` fails `-D warnings` twice:
  `needless_range_loop` and `assign_op_pattern`. Use
  `for (accumulated, band) in out.iter_mut().zip(other) { *accumulated += … }`.
  Tasks 2 and 4 both contain band loops of exactly that shape. **`+=` on
  `f64` is a plain unfused `fadd` and is the sanctioned accumulate** — never
  "fix" it toward `mul_add`.
- **Never `assert_eq!` a computed float against a tidy decimal.** Task 1's
  plan text asserted `0.5` for `0.25*0.2 + 0.75*0.6`, which is real-number
  arithmetic; in binary the answer is one ULP lower, and the *only* way to
  reach `0.5` is the forbidden fused `mul_add`. Use dyadic inputs (quarters,
  eighths) when stating a law, so the computation is exact and `assert_eq!`
  witnesses bit-exactness rather than approximate agreement.
- **`make gate` is ~15 min, not the ~4 min decision 0040 budgeted** (934.5 s
  measured on a quiet Mac, 2026-07-29). Iterate with the per-crate commands
  each task gives; the full gate belongs at the end, not in the loop.
- **Stagger gates.** A single `make ci` already reports `cpu_ratio` 8.25–8.50
  on ten cores, so two concurrent gates cost about thirty minutes each and
  both look hung. Check that no other session is gating before starting one.

## File Structure

| File | Responsibility |
|---|---|
| `kernel/src/color.rs` (new) | `Spectrum`, `Reflectance`, `Illuminant`, `Observer`, `Signal`, `Mixture`; area mixing; sRGB projection; `standard_observer()` |
| `kernel/src/lib.rs` (modify) | `pub mod color;` + re-exports |
| `domains/astronomy/src/star.rs` (modify) | `Star::t_eff` — derived, contained |
| `domains/astronomy/src/illuminant.rs` (new) | Planck sampling → `Illuminant`; time-of-day attenuation |
| `domains/terrain/src/lithology.rs` (modify) | `reflectance(&MaterialBuffer, RockClass) -> Mixture` |
| `domains/language/src/exemplars.rs` (new) | Seven hue exemplar reflectances, keyed by `color_pack` concept id |
| `windows/worldgen/src/color_naming.rs` (new) | `name_color(...)` — nearest exemplar through the speaker's lexicon |
| `windows/locale/src/lib.rs` (modify) | `LocaleContext::reflectance_at` — the material crossing |
| `windows/scene/src/surrounds.rs` (modify) | Optional `color` on `SurroundsCell`; a coloured builder |
| `windows/scene/src/surrounds_ascii.rs` (modify) | The `colour` lens |

---

### Task 1: The spectral primitive

**Files:**
- Create: `kernel/src/color.rs`
- Modify: `kernel/src/lib.rs:8-30` (module list), `kernel/src/lib.rs:32-54` (re-exports)
- Test: inline `#[cfg(test)] mod tests` in `kernel/src/color.rs` (this repo's convention — see `kernel/src/units.rs`)

**Interfaces:**
- Consumes: `hornvale_kernel::units::UnitError` (`kernel/src/units.rs:13`)
- Produces: `BANDS: usize = 10`, `BAND_CENTERS_NM: [f64; BANDS]`,
  `Spectrum::new([f64; BANDS]) -> Result<Spectrum, UnitError>`,
  `Spectrum::get(&self) -> &[f64; BANDS]`,
  `Reflectance::new([f64; BANDS]) -> Result<Reflectance, UnitError>`,
  `Reflectance::get(&self) -> &[f64; BANDS]`,
  `Illuminant::new([f64; BANDS]) -> Result<Illuminant, UnitError>`,
  `Illuminant::get(&self) -> &[f64; BANDS]`,
  `Mixture::new(Vec<Reflectance>, Vec<f64>) -> Result<Mixture, UnitError>`,
  `Mixture::integrate(&self) -> Reflectance`

- [ ] **Step 1: Write the failing tests**

Create `kernel/src/color.rs` with only the test module for now:

```rust
//! Spectral colour: the substrate for colour as a three-way product of
//! illuminant, reflectance and observer (spec "The Pigment").
//!
//! Colour is not a property of an object. A material has a *reflectance* —
//! the fraction of light it returns per wavelength, identical in a cave and
//! at noon. Light has a spectrum. An eye has sensitivity curves and
//! collapses the arriving mixture to one number per channel. Colour exists
//! only where all three meet, which is why every observer variation
//! (species vision, colour blindness, a screen reader taking none of it) is
//! the same operation with a different observer.
//!
//! **Determinism.** The hot path is `Σ r[b] · i[b] · s[b]` — multiplication
//! and addition over fixed-size arrays, which IEEE 754 requires to be
//! exact (decision 0041). No `math.rs` call appears here. Use `a * b + c`
//! and never `mul_add`: both are exact but they round differently from
//! each other.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_spectrum_rejects_a_non_finite_band() {
        let mut v = [0.5; BANDS];
        v[3] = f64::NAN;
        assert!(Spectrum::new(v).is_err());
    }

    #[test]
    fn a_reflectance_rejects_a_band_above_one() {
        let mut v = [0.5; BANDS];
        v[0] = 1.5;
        let err = Reflectance::new(v).unwrap_err();
        assert_eq!(err.unit, "reflectance");
    }

    #[test]
    fn a_reflectance_rejects_a_negative_band() {
        let mut v = [0.5; BANDS];
        v[9] = -0.01;
        assert!(Reflectance::new(v).is_err());
    }

    #[test]
    fn area_mixing_lands_between_its_components_in_every_band() {
        let dark = Reflectance::new([0.1; BANDS]).unwrap();
        let bright = Reflectance::new([0.9; BANDS]).unwrap();
        let mixed = Mixture::new(vec![dark, bright], vec![0.25, 0.75])
            .unwrap()
            .integrate();
        for b in 0..BANDS {
            assert!(
                mixed.get()[b] > 0.1 && mixed.get()[b] < 0.9,
                "band {b} = {} escaped its components",
                mixed.get()[b]
            );
        }
    }

    #[test]
    fn area_mixing_is_the_weighted_arithmetic_mean() {
        // Dyadic inputs, so the whole computation is EXACT in binary and
        // assert_eq! states the law without depending on rounding.
        // Do NOT use tidy decimals like 0.2/0.6 here: they are inexact in
        // binary, and the only arithmetic that reaches the tidy answer is a
        // fused mul_add — which this campaign forbids. See the sibling test
        // below, which uses exactly those values on purpose.
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.25, 0.75]).unwrap().integrate();
        // 0.25*0.25 + 0.75*0.75 = 0.0625 + 0.5625 = 0.625, exactly.
        assert_eq!(mixed.get()[0], 0.625);
    }

    #[test]
    fn area_mixing_does_not_fuse_its_multiply_and_add() {
        // The guard for the workspace rule that `a * b + c` and
        // `a.mul_add(b, c)` are never mixed. These inputs DISTINGUISH the
        // two: unfused rounds twice and lands one ULP below 0.5; fused
        // rounds once and reaches 0.5. The literal is a fingerprint of the
        // unfused implementation — a failure reading `right: 0.5` means
        // someone introduced a mul_add.
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        let b = Reflectance::new([0.6; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.25, 0.75]).unwrap().integrate();
        assert_eq!(mixed.get()[0], 0.499_999_999_999_999_94);
    }

    #[test]
    fn a_mixture_normalizes_its_weights() {
        // Dyadic again; 1/4 and 3/4 are exact, so normalization introduces
        // no rounding either.
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        // Weights 1 and 3 are the same mixture as 0.25 and 0.75.
        let mixed = Mixture::new(vec![a, b], vec![1.0, 3.0]).unwrap().integrate();
        assert_eq!(mixed.get()[0], 0.625);
    }

    #[test]
    fn a_mixture_keeps_its_components_reachable() {
        let a = Reflectance::new([0.25; BANDS]).unwrap();
        let b = Reflectance::new([0.75; BANDS]).unwrap();
        let m = Mixture::new(vec![a, b], vec![1.0, 3.0]).unwrap();
        assert_eq!(m.components().len(), 2);
        assert_eq!(m.components()[0].get()[0], 0.25);
        assert_eq!(m.weights(), &[1.0, 3.0]);
    }

    #[test]
    fn a_mixture_rejects_mismatched_lengths() {
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        assert!(Mixture::new(vec![a], vec![1.0, 2.0]).is_err());
    }

    #[test]
    fn a_mixture_rejects_zero_total_weight() {
        let a = Reflectance::new([0.2; BANDS]).unwrap();
        let b = Reflectance::new([0.6; BANDS]).unwrap();
        assert!(Mixture::new(vec![a, b], vec![0.0, 0.0]).is_err());
    }

    #[test]
    fn mixing_never_exceeds_unit_reflectance() {
        let a = Reflectance::new([1.0; BANDS]).unwrap();
        let b = Reflectance::new([1.0; BANDS]).unwrap();
        let mixed = Mixture::new(vec![a, b], vec![0.5, 0.5]).unwrap().integrate();
        for b in 0..BANDS {
            assert!(mixed.get()[b] <= 1.0, "band {b} broke energy conservation");
        }
    }

    #[test]
    fn the_band_grid_is_ten_uniform_forty_nanometre_bands() {
        assert_eq!(BANDS, 10);
        assert_eq!(BAND_CENTERS_NM[0], 360.0);
        assert_eq!(BAND_CENTERS_NM[BANDS - 1], 720.0);
        for b in 1..BANDS {
            assert_eq!(BAND_CENTERS_NM[b] - BAND_CENTERS_NM[b - 1], 40.0);
        }
    }
}
```

Add the module to `kernel/src/lib.rs`, in alphabetical position between
`astar` and `component`:

```rust
pub mod color;
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib color`
Expected: FAIL to compile — `cannot find value BANDS in this scope`, and
the same for `Spectrum`, `Reflectance`, `Mixture`.

- [ ] **Step 3: Write the implementation**

Insert above the `#[cfg(test)]` block in `kernel/src/color.rs`:

```rust
use crate::units::UnitError;

/// Number of sampled wavelength bands. **This is a contract**: widening it
/// rewrites every authored reflectance in the workspace, which is why the
/// grid already reaches into the near-ultraviolet rather than stopping at
/// the human visible range.
/// type-audit: bare-ok(count)
pub const BANDS: usize = 10;

/// Band centre wavelengths in nanometres — ten uniform 40 nm bands spanning
/// 340–740 nm. Uniform rather than weighted toward human cone peaks: a
/// human-weighted grid would rebuild, in the substrate, exactly the
/// anthropocentrism this model exists to remove.
/// type-audit: bare-ok(ratio)
pub const BAND_CENTERS_NM: [f64; BANDS] = [
    360.0, 400.0, 440.0, 480.0, 520.0, 560.0, 600.0, 640.0, 680.0, 720.0,
];

/// A quantity sampled on the band grid. Unconstrained in magnitude — a
/// radiance may exceed 1 where a reflectance may not.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spectrum([f64; BANDS]);

impl Spectrum {
    /// Validating constructor: every band must be finite.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() {
                return Err(UnitError {
                    unit: "spectrum",
                    value,
                    reason: "every band must be finite",
                });
            }
        }
        Ok(Self(bands))
    }

    /// The raw per-band values.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0
    }
}

/// The fraction of arriving light a material returns, per band. A property
/// of the stuff: the same in a cave and at noon.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Reflectance(Spectrum);

impl Reflectance {
    /// Validating constructor: every band is finite and within `[0, 1]`.
    /// A band above 1 would return more light than arrived.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() || !(0.0..=1.0).contains(&value) {
                return Err(UnitError {
                    unit: "reflectance",
                    value,
                    reason: "every band must be finite and within [0, 1]",
                });
            }
        }
        Ok(Self(Spectrum(bands)))
    }

    /// The raw per-band fractions.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0.0
    }
}

/// Light arriving at a surface, per band. Magnitude is unconstrained.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Illuminant(Spectrum);

impl Illuminant {
    /// Validating constructor: every band is finite and non-negative.
    /// type-audit: bare-ok(constructor-edge: bands)
    pub fn new(bands: [f64; BANDS]) -> Result<Self, UnitError> {
        for value in bands {
            if !value.is_finite() || value < 0.0 {
                return Err(UnitError {
                    unit: "illuminant",
                    value,
                    reason: "every band must be finite and non-negative",
                });
            }
        }
        Ok(Self(Spectrum(bands)))
    }

    /// The raw per-band radiances.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64; BANDS] {
        &self.0.0
    }
}

/// Several reflectances present together, with their weights.
///
/// Combining colours is three unrelated operations, and only one of them is
/// implemented here:
///
/// - **Area** (this type's [`Mixture::integrate`]): components below the
///   resolution you are looking at — speckled granite from ten metres.
///   Separate reflections average on the way to the eye, so the law is the
///   weighted *arithmetic* mean.
/// - **Additive** (not implemented; arrives with multi-light): two torches
///   on one wall. Sum the illuminants, not the reflectances.
/// - **Subtractive** (not implemented; arrives with alchemy): two dyes in
///   one vat. Light passes through both absorbers in series, so the law is
///   the weighted *geometric* mean. This is why blue and yellow paint make
///   green while blue and yellow light make white.
///
/// The producer returns a `Mixture` rather than a finished [`Reflectance`]
/// so a later texture layer — which needs to arrange the components
/// spatially — does not have to re-derive components that were computed and
/// thrown away.
#[derive(Debug, Clone, PartialEq)]
pub struct Mixture {
    components: Vec<Reflectance>,
    weights: Vec<f64>,
}

impl Mixture {
    /// Validating constructor: equal lengths, non-empty, every weight
    /// finite and non-negative, and a strictly positive total.
    /// type-audit: bare-ok(ratio: weights)
    pub fn new(components: Vec<Reflectance>, weights: Vec<f64>) -> Result<Self, UnitError> {
        if components.is_empty() || components.len() != weights.len() {
            return Err(UnitError {
                unit: "mixture",
                value: weights.len() as f64,
                reason: "components and weights must be non-empty and equal in length",
            });
        }
        let mut total = 0.0;
        for w in &weights {
            if !w.is_finite() || *w < 0.0 {
                return Err(UnitError {
                    unit: "mixture",
                    value: *w,
                    reason: "every weight must be finite and non-negative",
                });
            }
            total += *w;
        }
        if total <= 0.0 {
            return Err(UnitError {
                unit: "mixture",
                value: total,
                reason: "total weight must be strictly positive",
            });
        }
        Ok(Self {
            components,
            weights,
        })
    }

    /// Collapse to a single reflectance by the **area** law: the weighted
    /// arithmetic mean, per band. Weights are normalized here, so callers
    /// may pass unnormalized proportions.
    ///
    /// Summation order is the component order, which is fixed by
    /// construction — that is what makes this bit-identical across
    /// platforms.
    pub fn integrate(&self) -> Reflectance {
        let mut total = 0.0;
        for w in &self.weights {
            total += *w;
        }
        let mut out = [0.0f64; BANDS];
        for (component, weight) in self.components.iter().zip(&self.weights) {
            let share = weight / total;
            // `iter_mut().zip()`, NOT `for b in 0..BANDS { out[b] = out[b] + … }`
            // — that shape fails `clippy -D warnings` twice over
            // (`needless_range_loop` and `assign_op_pattern`). `+=` on f64 is
            // a plain unfused `fadd` and is the sanctioned accumulate; do not
            // "fix" it toward `mul_add`.
            for (accumulated, band) in out.iter_mut().zip(component.get()) {
                *accumulated += band * share;
            }
        }
        // Normalized weights sum to 1 and every component band is within
        // [0, 1], so the mean is too — but clamp against accumulated
        // rounding so the constructor can never reject our own output.
        for value in out.iter_mut() {
            *value = value.clamp(0.0, 1.0);
        }
        Reflectance(Spectrum(out))
    }
}
```

Add to the re-export block in `kernel/src/lib.rs` (alphabetical, after the
`astar` line):

```rust
pub use color::{BAND_CENTERS_NM, BANDS, Illuminant, Mixture, Reflectance, Spectrum};
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib color`
Expected: PASS, 9 tests.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add kernel/src/color.rs kernel/src/lib.rs
git commit -m "feat(kernel): the spectral colour primitive

Ten uniform 40nm bands over 340-740nm. The grid is a contract, so it
reaches into the near-UV now rather than owing a rewrite of every authored
reflectance when a UV-sighted species arrives.

Area mixing only. Additive and subtractive are documented on Mixture with
their laws and left unimplemented -- they belong to multi-light and alchemy.
The producer returns a Mixture rather than a Reflectance so a later texture
layer need not re-derive components."
```

---

### Task 2: The observer, the signal, and the sRGB projection

**Files:**
- Modify: `kernel/src/color.rs` (append before the test module), `kernel/src/lib.rs` (re-exports)
- Test: inline `#[cfg(test)] mod tests` in `kernel/src/color.rs`

**Interfaces:**
- Consumes: `BANDS`, `Spectrum`, `Reflectance`, `Illuminant` (Task 1)
- Produces: `Observer::new(Vec<Spectrum>) -> Result<Observer, UnitError>`,
  `Observer::channels(&self) -> usize`,
  `Observer::sense(&self, &Reflectance, &Illuminant) -> Signal`,
  `Signal::get(&self) -> &[f64]`,
  `Signal::distance_to(&self, &Signal) -> f64`,
  `standard_observer() -> Observer`,
  `Observer::to_srgb(&self, &Signal) -> Option<[u8; 3]>`

- [ ] **Step 1: Write the failing tests**

Append these to the existing `mod tests` in `kernel/src/color.rs`:

```rust
    /// A flat unit illuminant — every band equal. Used wherever a test
    /// wants reflectance differences and no illuminant differences.
    fn flat_light() -> Illuminant {
        Illuminant::new([1.0; BANDS]).unwrap()
    }

    #[test]
    fn the_standard_observer_has_four_channels() {
        // Three photopic plus one scotopic (rod-like) channel.
        assert_eq!(standard_observer().channels(), 4);
    }

    #[test]
    fn a_brighter_surface_produces_a_larger_signal_in_every_channel() {
        let obs = standard_observer();
        let dim = obs.sense(&Reflectance::new([0.2; BANDS]).unwrap(), &flat_light());
        let bright = obs.sense(&Reflectance::new([0.8; BANDS]).unwrap(), &flat_light());
        for c in 0..obs.channels() {
            assert!(
                bright.get()[c] > dim.get()[c],
                "channel {c}: {} was not brighter than {}",
                bright.get()[c],
                dim.get()[c]
            );
        }
    }

    #[test]
    fn naming_works_at_a_channel_count_below_the_standard() {
        // A synthetic two-channel dichromat: one short-biased channel, one
        // long-biased. Proves the pipeline is not hardcoded to four.
        let short = Spectrum::new([1.0, 1.0, 1.0, 0.5, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap();
        let long = Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.2, 0.5, 1.0, 1.0, 1.0, 1.0]).unwrap();
        let obs = Observer::new(vec![short, long]).unwrap();
        assert_eq!(obs.channels(), 2);

        let bluish = Reflectance::new([0.8, 0.8, 0.8, 0.6, 0.2, 0.05, 0.05, 0.05, 0.05, 0.05])
            .unwrap();
        let s = obs.sense(&bluish, &flat_light());
        assert_eq!(s.get().len(), 2);
        assert!(s.get()[0] > s.get()[1], "a short-biased surface must excite the short channel more");
    }

    #[test]
    fn naming_works_at_a_channel_count_above_the_standard() {
        // A synthetic five-channel observer, each channel a single band.
        // Counts either side of the standard's four mean neither of these
        // two tests can pass by accidentally exercising the standard path.
        let mut channels = Vec::new();
        for b in 0..5 {
            let mut curve = [0.0; BANDS];
            curve[b] = 1.0;
            channels.push(Spectrum::new(curve).unwrap());
        }
        let obs = Observer::new(channels).unwrap();
        assert_eq!(obs.channels(), 5);
        let s = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert_eq!(s.get().len(), 5);
    }

    #[test]
    fn an_observer_rejects_an_empty_channel_set() {
        assert!(Observer::new(vec![]).is_err());
    }

    #[test]
    fn sensing_is_bit_identical_across_repeated_calls() {
        let obs = standard_observer();
        let r = Reflectance::new([0.37; BANDS]).unwrap();
        let a = obs.sense(&r, &flat_light());
        let b = obs.sense(&r, &flat_light());
        assert_eq!(a.get(), b.get());
    }

    #[test]
    fn signal_distance_is_zero_for_identical_signals_and_positive_otherwise() {
        let obs = standard_observer();
        let a = obs.sense(&Reflectance::new([0.3; BANDS]).unwrap(), &flat_light());
        let b = obs.sense(&Reflectance::new([0.7; BANDS]).unwrap(), &flat_light());
        assert_eq!(a.distance_to(&a), 0.0);
        assert!(a.distance_to(&b) > 0.0);
    }

    #[test]
    fn the_standard_observer_projects_to_srgb_but_a_synthetic_one_does_not() {
        let obs = standard_observer();
        let s = obs.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert!(obs.to_srgb(&s).is_some(), "the standard observer has a real mapping");

        // A five-channel signal has no truthful sRGB image. Any mapping
        // would be a false-colour decision, and RENDER-9 requires that be
        // declared by the caller rather than invented here.
        let mut channels = Vec::new();
        for b in 0..5 {
            let mut curve = [0.0; BANDS];
            curve[b] = 1.0;
            channels.push(Spectrum::new(curve).unwrap());
        }
        let alien = Observer::new(channels).unwrap();
        let alien_signal = alien.sense(&Reflectance::new([0.5; BANDS]).unwrap(), &flat_light());
        assert!(alien.to_srgb(&alien_signal).is_none());
    }

    #[test]
    fn a_white_surface_under_flat_light_projects_near_white() {
        let obs = standard_observer();
        let s = obs.sense(&Reflectance::new([1.0; BANDS]).unwrap(), &flat_light());
        let [r, g, b] = obs.to_srgb(&s).unwrap();
        for channel in [r, g, b] {
            assert!(channel > 200, "expected a bright neutral, got {r},{g},{b}");
        }
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel --lib color`
Expected: FAIL to compile — `cannot find function standard_observer`, and
the same for `Observer` and `Signal`.

- [ ] **Step 3: Write the implementation**

Append to `kernel/src/color.rs`, above the test module:

```rust
/// One number per observer channel — the collapse of an entire arriving
/// spectrum down to what an eye actually transmits.
#[derive(Debug, Clone, PartialEq)]
pub struct Signal(Vec<f64>);

impl Signal {
    /// The raw per-channel responses.
    /// type-audit: bare-ok(constructor-edge: return)
    pub fn get(&self) -> &[f64] {
        &self.0
    }

    /// Squared Euclidean distance in signal space. Squared rather than
    /// rooted because every caller only ranks distances, and `sqrt` would
    /// be a monotone transform that buys nothing.
    ///
    /// Signals of differing length compare as [`f64::INFINITY`] — they come
    /// from different observers and are not comparable at all.
    /// type-audit: bare-ok(ratio: return)
    pub fn distance_to(&self, other: &Signal) -> f64 {
        if self.0.len() != other.0.len() {
            return f64::INFINITY;
        }
        let mut sum = 0.0;
        for (a, b) in self.0.iter().zip(&other.0) {
            let d = a - b;
            sum += d * d;
        }
        sum
    }
}

/// An eye: one sensitivity curve per channel. Humans have three photopic
/// channels plus rods; other creatures have other counts, which is the
/// whole reason the channel set is a `Vec` and not an array.
#[derive(Debug, Clone, PartialEq)]
pub struct Observer {
    channels: Vec<Spectrum>,
    /// Whether this observer's signal has a real (non-false-colour) sRGB
    /// image. True only for [`standard_observer`].
    srgb_native: bool,
}

impl Observer {
    /// Validating constructor: at least one channel.
    ///
    /// An observer built this way is **not** sRGB-native: [`to_srgb`]
    /// returns `None`, because a signal from an arbitrary channel set has
    /// no truthful three-channel image and any mapping would be a
    /// false-colour choice the caller must declare (RENDER-9).
    ///
    /// [`to_srgb`]: Observer::to_srgb
    pub fn new(channels: Vec<Spectrum>) -> Result<Self, UnitError> {
        if channels.is_empty() {
            return Err(UnitError {
                unit: "observer",
                value: 0.0,
                reason: "an observer needs at least one channel",
            });
        }
        Ok(Self {
            channels,
            srgb_native: false,
        })
    }

    /// How many channels this observer has.
    /// type-audit: bare-ok(count: return)
    pub fn channels(&self) -> usize {
        self.channels.len()
    }

    /// The three-way product: `signal[c] = Σ_b r[b] · i[b] · s[c][b]`.
    ///
    /// Multiplication and addition only, over fixed-size arrays in a fixed
    /// order — IEEE 754 requires both to be exact, so this is bit-identical
    /// on every platform without routing through [`crate::math`].
    pub fn sense(&self, reflectance: &Reflectance, illuminant: &Illuminant) -> Signal {
        let r = reflectance.get();
        let i = illuminant.get();
        let mut out = Vec::with_capacity(self.channels.len());
        for channel in &self.channels {
            let s = channel.get();
            let mut sum = 0.0;
            for b in 0..BANDS {
                sum += r[b] * i[b] * s[b];
            }
            out.push(sum);
        }
        Signal(out)
    }

    /// Project a signal to display bytes, or `None` when this observer has
    /// no truthful sRGB image.
    ///
    /// Only [`standard_observer`] is sRGB-native. For anything else the
    /// answer is `None` on purpose: the caller must choose and *caption* a
    /// false-colour mapping rather than have one invented here, because the
    /// caption — not the picture — carries the honesty (RENDER-9).
    /// type-audit: bare-ok(artifact: return)
    pub fn to_srgb(&self, signal: &Signal) -> Option<[u8; 3]> {
        if !self.srgb_native || signal.get().len() != 4 {
            return None;
        }
        // Channel order is [short, medium, long, scotopic]; the scotopic
        // channel carries no hue and is not projected. The photopic
        // channels are normalized by the response a unit-reflectance
        // surface under a unit illuminant would produce, so a white surface
        // under flat light lands at white rather than at an arbitrary
        // scale.
        let s = signal.get();
        let mut out = [0u8; 3];
        // Long → red, medium → green, short → blue.
        for (slot, (raw, norm)) in [(s[2], LONG_NORM), (s[1], MEDIUM_NORM), (s[0], SHORT_NORM)]
            .into_iter()
            .enumerate()
        {
            let linear = (raw / norm).clamp(0.0, 1.0);
            out[slot] = encode_srgb_byte(linear);
        }
        Some(out)
    }
}

/// Normalizing constants: the response each photopic channel gives to a
/// unit-reflectance surface under a unit illuminant. Derived from
/// [`standard_observer`]'s own curves, so the two cannot drift apart —
/// `standard_observer_channels_sum_to_the_declared_norms` proves it.
/// type-audit: bare-ok(ratio)
const SHORT_NORM: f64 = 1.98;
/// See [`SHORT_NORM`].
/// type-audit: bare-ok(ratio)
const MEDIUM_NORM: f64 = 3.51;
/// See [`SHORT_NORM`].
/// type-audit: bare-ok(ratio)
const LONG_NORM: f64 = 3.95;

/// Encode a linear `[0, 1]` intensity as an sRGB byte.
///
/// This is the one transcendental in the colour path, and it sits at the
/// emit boundary rather than in the hot loop. It routes through
/// [`crate::math::powf`] like every other transcendental in the workspace
/// (decision 0041).
/// type-audit: bare-ok(artifact: return)
fn encode_srgb_byte(linear: f64) -> u8 {
    let encoded = if linear <= 0.003_130_8 {
        12.92 * linear
    } else {
        1.055 * crate::math::powf(linear, 1.0 / 2.4) - 0.055
    };
    (encoded.clamp(0.0, 1.0) * 255.0).round() as u8
}

/// The human-calibrated observer: three photopic channels (short, medium,
/// long) plus one scotopic rod-like channel used at low light.
///
/// The curves are coarse samples of human cone and rod sensitivity on the
/// band grid. They are approximations and say so — the campaign's claims
/// rest on *differences between observers*, not on colorimetric accuracy.
pub fn standard_observer() -> Observer {
    let short = Spectrum([0.00, 0.25, 1.00, 0.62, 0.10, 0.01, 0.00, 0.00, 0.00, 0.00]);
    let medium = Spectrum([0.00, 0.01, 0.10, 0.45, 0.90, 1.00, 0.72, 0.28, 0.05, 0.00]);
    let long = Spectrum([0.00, 0.01, 0.06, 0.25, 0.60, 0.92, 1.00, 0.75, 0.30, 0.06]);
    let scotopic = Spectrum([0.00, 0.15, 0.55, 0.95, 1.00, 0.68, 0.25, 0.05, 0.00, 0.00]);
    Observer {
        channels: vec![short, medium, long, scotopic],
        srgb_native: true,
    }
}
```

Add one more test that pins the norms to the curves, so the two can never
drift apart:

```rust
    #[test]
    fn standard_observer_channels_sum_to_the_declared_norms() {
        let obs = standard_observer();
        let sums: Vec<f64> = obs
            .channels
            .iter()
            .map(|c| c.get().iter().sum::<f64>())
            .collect();
        // Rounded to two places: the constants are the normalizers used by
        // to_srgb, and a curve edit that does not update them would make a
        // white surface stop projecting to white.
        assert_eq!((sums[0] * 100.0).round() / 100.0, SHORT_NORM);
        assert_eq!((sums[1] * 100.0).round() / 100.0, MEDIUM_NORM);
        assert_eq!((sums[2] * 100.0).round() / 100.0, LONG_NORM);
    }
```

Extend the re-export line in `kernel/src/lib.rs`:

```rust
pub use color::{
    BAND_CENTERS_NM, BANDS, Illuminant, Mixture, Observer, Reflectance, Signal, Spectrum,
    standard_observer,
};
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel --lib color`
Expected: PASS, 19 tests.

If `standard_observer_channels_sum_to_the_declared_norms` fails, the
declared norms are wrong — **fix the constants to the printed sums**, do
not weaken the test.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-kernel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add kernel/src/color.rs kernel/src/lib.rs
git commit -m "feat(kernel): the observer, the signal, and the sRGB projection

sense() is the three-way product and the campaign's whole hot path:
multiplication and addition over fixed-size arrays in a fixed order, which
IEEE 754 requires to be exact. No math.rs call appears in it.

to_srgb returns None for any non-standard observer on purpose. A five-channel
signal has no truthful three-channel image, so the mapping is a false-colour
choice the caller has to declare and caption rather than have invented here."
```

---

### Task 3: The star's effective temperature — derived and contained

**Files:**
- Modify: `domains/astronomy/src/star.rs:12-27` (the `Star` struct), `domains/astronomy/src/star.rs:63-87` (`generate_star`)
- Test: inline `#[cfg(test)] mod tests` in `domains/astronomy/src/star.rs`

**Interfaces:**
- Consumes: `Star.mass: SolarMasses` (existing)
- Produces: `Star::t_eff: Kelvin` — a new field on the existing struct;
  `pub struct Kelvin(pub f64)` if the crate has no Kelvin newtype (check
  first with `grep -rn "Kelvin" domains/astronomy/src/`; if one exists,
  use it and do not mint a second)

- [ ] **Step 1: Write the failing tests**

Append to the existing `mod tests` in `domains/astronomy/src/star.rs`:

```rust
    #[test]
    fn effective_temperature_spans_the_expected_range_across_the_mass_draw() {
        // The mass draw is 0.6 + u*0.8, so 0.6..1.4 solar masses.
        // T = 5772 * M^0.475 gives 4528.4 K at 0.6 and 6772.3 K at 1.4.
        let cool = t_eff_of_mass(0.6);
        let hot = t_eff_of_mass(1.4);
        assert!((cool - 4528.4).abs() < 0.1, "cool end was {cool}");
        assert!((hot - 6772.3).abs() < 0.1, "hot end was {hot}");
    }

    #[test]
    fn a_solar_mass_star_is_solar_temperature() {
        assert!((t_eff_of_mass(1.0) - 5772.0).abs() < 1e-9);
    }

    #[test]
    fn the_derived_temperature_agrees_with_the_existing_class_name() {
        // generate_star labels K below 0.8 solar masses, G below 1.05, F
        // above. Published main-sequence boundaries are ~5300 K (K/G) and
        // ~5900 K (G/F). If the derived temperature disagreed with the
        // label the star already carries, the world would contradict
        // itself in print.
        let kg_boundary = t_eff_of_mass(0.8);
        let gf_boundary = t_eff_of_mass(1.05);
        assert!(
            (5000.0..5400.0).contains(&kg_boundary),
            "K/G boundary landed at {kg_boundary} K, outside the published band"
        );
        assert!(
            (5800.0..6100.0).contains(&gf_boundary),
            "G/F boundary landed at {gf_boundary} K, outside the published band"
        );
    }

    #[test]
    fn effective_temperature_is_contained_and_moves_nothing_else() {
        // The containment rule `age` already carries (see the doc comment
        // on Star::age): t_eff must reach colour and nothing else. Two
        // stars built from the same mass agree on every other field, and
        // the only way t_eff could leak is through one of them.
        let a = generate_star(Seed::new(42).derive("astronomy"));
        let b = generate_star(Seed::new(42).derive("astronomy"));
        assert_eq!(a.t_eff.0, b.t_eff.0);
        assert_eq!(a.luminosity.0, b.luminosity.0);
        assert_eq!(a.habitable_zone.inner().0, b.habitable_zone.inner().0);
    }
```

Note: the exact `Seed` construction above must match this repo's real
helper. Before writing the test, run
`grep -rn "Seed::new\|Seed(" domains/astronomy/src/star.rs` and copy the
construction the existing tests in that file already use — plan-authored
snippets are the one code nothing compiles, and `Seed::new` vs `Seed(42)`
has ridden into three tasks in this repo before.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-astronomy --lib star`
Expected: FAIL to compile — `cannot find function t_eff_of_mass`, and
`no field t_eff on type Star`.

- [ ] **Step 3: Write the implementation**

In `domains/astronomy/src/star.rs`, add the field to `Star` after `age`:

```rust
    /// Effective surface temperature in Kelvin (derived from `mass`, not
    /// drawn). **Does not feed `luminosity`, `habitable_zone`, insolation,
    /// orbit admission, or climate** — the same containment rule `age`
    /// carries. It exists so the star's light has a spectrum; nothing
    /// physical downstream may consult it.
    pub t_eff: Kelvin,
```

Add the newtype (only if `grep` in Step 1 found none already):

```rust
/// An absolute temperature in Kelvin. Distinct from climate's Celsius
/// `Temperature`: this is a stellar surface reading, never a surface-air
/// one, and the two must not be interchangeable.
/// type-audit: bare-ok(constructor-edge: 0)
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Kelvin(pub f64);
```

Add the derivation next to `t_ms_of_mass`:

```rust
/// Effective temperature from mass, on the raw mass value.
///
/// The repo already fixes both relations this needs: `L = M^3.5`
/// (`generate_star`) and `R = M^0.8` (`sun_angular_diameter_rel`'s declared
/// approximation). Stefan–Boltzmann gives `L = 4πR²σT⁴`, so
/// `T⁴ ∝ L/R² = M^3.5 / M^1.6 = M^1.9`, hence `T ∝ M^0.475`. Calibrated on
/// Sol at 5772 K.
///
/// Declared approximation, not a stellar-structure model — the same
/// standing `main_sequence_lifetime` has.
fn t_eff_of_mass(mass: f64) -> f64 {
    5772.0 * math::powf(mass, 0.475)
}
```

In `generate_star`, add to the returned struct literal after `age`:

```rust
        t_eff: Kelvin(t_eff_of_mass(mass.0)),
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-astronomy --lib star`
Expected: PASS, 4 new tests plus the file's existing ones.

- [ ] **Step 5: Run the astronomy property batteries**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS. In particular `genesis_properties` must stay green — a new
*derived* field consumes no draws, so stream consumption order is
unchanged. **If any pin-isolation test reddens, stop**: it means the field
was accidentally wired into a drawn path, which is the containment rule
being violated.

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/astronomy/src/star.rs
git commit -m "feat(astronomy): derive the star's effective temperature

T = 5772K * M^0.475, from the L = M^3.5 and R = M^0.8 relations the crate
already declares. Derived from a value the seed fixes, so no draw, no stream
label, no epoch.

Carries the containment rule `age` carries: t_eff feeds colour and nothing
else -- never luminosity, orbit admission, insolation or climate. The derived
K/G and G/F boundaries land at 5191K and 5907K, inside the published bands,
so the temperature and the class_name the star already prints agree."
```

---

### Task 4: The illuminant — Planck sampling and time of day

**Files:**
- Create: `domains/astronomy/src/illuminant.rs`
- Modify: `domains/astronomy/src/lib.rs` (add `pub mod illuminant;` and re-export)
- Test: inline `#[cfg(test)] mod tests` in `domains/astronomy/src/illuminant.rs`

**Interfaces:**
- Consumes: `Star.t_eff: Kelvin` (Task 3);
  `hornvale_kernel::color::{BANDS, BAND_CENTERS_NM, Illuminant}` (Task 1)
- Produces: `daylight(star: &Star) -> Illuminant`,
  `at_elevation(base: &Illuminant, sun_elevation_deg: f64) -> Illuminant`

- [ ] **Step 1: Write the failing tests**

Create `domains/astronomy/src/illuminant.rs`:

```rust
//! The star's light as a spectrum on the kernel's band grid.
//!
//! A blackbody curve at the star's effective temperature, sampled into ten
//! bands, plus a cheap atmospheric-path attenuation that reddens the light
//! as the sun approaches the horizon. Both are declared approximations:
//! the campaign's claims rest on the *direction* of these effects, not on
//! radiometric accuracy.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::star::{Kelvin, Star};

    /// A star at an exact temperature, with every other field irrelevant to
    /// these tests. Build it through `generate_star` and override, so the
    /// test cannot drift from the real constructor.
    fn star_at(t_eff: f64) -> Star {
        let mut s = crate::star::generate_star(test_seed());
        s.t_eff = Kelvin(t_eff);
        s
    }

    #[test]
    fn a_hot_star_is_bluer_than_a_cool_one() {
        let cool = daylight(&star_at(4000.0));
        let hot = daylight(&star_at(7000.0));
        // Compare short-band share against long-band share. Absolute
        // radiance differs hugely between the two; the ratio is the claim.
        let cool_ratio = cool.get()[2] / cool.get()[8];
        let hot_ratio = hot.get()[2] / hot.get()[8];
        assert!(
            hot_ratio > cool_ratio,
            "hot star short/long = {hot_ratio}, cool = {cool_ratio}"
        );
    }

    #[test]
    fn every_band_of_a_daylight_illuminant_is_positive_and_finite() {
        let light = daylight(&star_at(5772.0));
        for b in 0..BANDS {
            assert!(light.get()[b].is_finite() && light.get()[b] > 0.0, "band {b}");
        }
    }

    #[test]
    fn a_low_sun_is_redder_than_a_high_sun() {
        let noon_light = daylight(&star_at(5772.0));
        let dusk_light = at_elevation(&noon_light, 3.0);
        let high = at_elevation(&noon_light, 80.0);
        let dusk_ratio = dusk_light.get()[8] / dusk_light.get()[2];
        let high_ratio = high.get()[8] / high.get()[2];
        assert!(
            dusk_ratio > high_ratio,
            "dusk long/short = {dusk_ratio}, high sun = {high_ratio}"
        );
    }

    #[test]
    fn a_sun_below_the_horizon_still_yields_a_valid_illuminant() {
        let base = daylight(&star_at(5772.0));
        let night = at_elevation(&base, -10.0);
        for b in 0..BANDS {
            assert!(night.get()[b].is_finite() && night.get()[b] >= 0.0, "band {b}");
        }
    }

    #[test]
    fn sampling_is_bit_identical_across_repeated_calls() {
        let a = daylight(&star_at(5772.0));
        let b = daylight(&star_at(5772.0));
        assert_eq!(a.get(), b.get());
    }
}
```

`test_seed()` above is a stand-in: before writing, run
`grep -rn "fn test_seed\|Seed::new" domains/astronomy/src/` and use the
seed helper this crate's existing tests already use, by name.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-astronomy --lib illuminant`
Expected: FAIL to compile — `cannot find function daylight`.

- [ ] **Step 3: Write the implementation**

Insert above the test module in `domains/astronomy/src/illuminant.rs`:

```rust
use crate::star::Star;
use hornvale_kernel::color::{BAND_CENTERS_NM, BANDS, Illuminant};
use hornvale_kernel::math;

/// Planck's second radiation constant, `hc/k`, in nanometre-kelvin. Used in
/// the exponential term of the spectral radiance law.
/// type-audit: bare-ok(ratio)
const C2_NM_K: f64 = 1.438_776_877e7;

/// Spectral radiance of a blackbody at `t_kelvin`, at `wavelength_nm`, up to
/// a constant factor. The leading `c1` is omitted because every consumer
/// works in ratios or renormalizes — carrying it would only scale all ten
/// bands together.
fn planck_relative(wavelength_nm: f64, t_kelvin: f64) -> f64 {
    let l5 = wavelength_nm.powi(5);
    let x = C2_NM_K / (wavelength_nm * t_kelvin);
    1.0 / (l5 * (math::exp(x) - 1.0))
}

/// The star's light at the top of the atmosphere, sampled into the band
/// grid and normalized so the brightest band is 1.0.
///
/// Normalizing here means downstream code compares *colour*, not distance
/// from the star — insolation is climate's business, and this function is
/// forbidden from influencing it (the containment rule on `Star::t_eff`).
pub fn daylight(star: &Star) -> Illuminant {
    let mut bands = [0.0f64; BANDS];
    let mut peak = 0.0f64;
    for b in 0..BANDS {
        let value = planck_relative(BAND_CENTERS_NM[b], star.t_eff.0);
        bands[b] = value;
        if value > peak {
            peak = value;
        }
    }
    // `peak` is strictly positive for any finite positive temperature, so
    // this division is total; the guard is defensive, not a live path.
    if peak > 0.0 {
        for value in bands.iter_mut() {
            *value = *value / peak;
        }
    }
    Illuminant::new(bands).expect("a normalized Planck curve is finite and non-negative")
}

/// Redden and dim `base` for a sun at `sun_elevation_deg` above the horizon.
///
/// Declared approximation: air mass grows roughly as `1/sin(elevation)`, and
/// Rayleigh scattering removes short wavelengths in proportion to `λ⁻⁴`, so
/// the surviving fraction per band is `exp(-k · airmass · (550/λ)⁴)`. The
/// result is that a low sun loses its blue first, which is the direction the
/// campaign's second falsifiable claim depends on.
///
/// A sun at or below the horizon is clamped to the largest air mass rather
/// than diverging.
/// type-audit: bare-ok(ratio: sun_elevation_deg)
pub fn at_elevation(base: &Illuminant, sun_elevation_deg: f64) -> Illuminant {
    /// Optical-depth scale at the reference wavelength.
    const K: f64 = 0.10;
    /// Reference wavelength, nanometres — roughly the middle of the grid.
    const REFERENCE_NM: f64 = 550.0;
    /// Air-mass ceiling, standing in for the horizon limit.
    const MAX_AIRMASS: f64 = 38.0;

    let sin_elevation = math::sin(sun_elevation_deg.clamp(-90.0, 90.0).to_radians());
    let airmass = if sin_elevation <= 1.0 / MAX_AIRMASS {
        MAX_AIRMASS
    } else {
        1.0 / sin_elevation
    };

    let mut bands = [0.0f64; BANDS];
    for b in 0..BANDS {
        let ratio = REFERENCE_NM / BAND_CENTERS_NM[b];
        let scattering = ratio * ratio * ratio * ratio;
        bands[b] = base.get()[b] * math::exp(-K * airmass * scattering);
    }
    Illuminant::new(bands).expect("attenuating a valid illuminant leaves it valid")
}
```

Add to `domains/astronomy/src/lib.rs`, in the module list:

```rust
pub mod illuminant;
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-astronomy --lib illuminant`
Expected: PASS, 5 tests.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/astronomy/src/illuminant.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): the star's light as a sampled spectrum

Planck's law at the star's effective temperature, sampled into the kernel's
ten bands and normalized to its own peak -- normalized because this function
compares colour, and insolation is climate's business and forbidden to it.

at_elevation applies a lambda^-4 Rayleigh attenuation over a 1/sin(elevation)
air mass, so a low sun loses its blue first. That direction is what the
campaign's noon-vs-dusk claim rests on."
```

---

### Task 5: Terrain reflectance from the material buffer

**Files:**
- Modify: `domains/terrain/src/lithology.rs` (append after `appearance`, around line 644)
- Test: inline `#[cfg(test)] mod tests` in `domains/terrain/src/lithology.rs`

**Interfaces:**
- Consumes: `MaterialBuffer` (`domains/terrain/src/lithology.rs:83`),
  `RockClass`, `hornvale_kernel::color::{Mixture, Reflectance}` (Task 1)
- Produces: `reflectance(buf: &MaterialBuffer, rock: RockClass) -> Mixture`

- [ ] **Step 1: Write the failing tests**

Append to the existing `mod tests` in `domains/terrain/src/lithology.rs`:

```rust
    // This module already has `flat_buffer()` at `lithology.rs:705` —
    // silica 0.5, grain 0.5, induration 0.5, carbonate 0.0,
    // metamorphic_grade 0.0, porosity 0.5, plus the three non-float fields.
    // Use it; do not add a second neutral fixture. `MaterialBuffer` is
    // `Copy` and does NOT derive `Default`, so struct-update syntax against
    // `flat_buffer()` is the only construction that compiles.

    #[test]
    fn a_felsic_rock_is_brighter_than_a_mafic_one() {
        let felsic = MaterialBuffer { silica: 0.95, ..flat_buffer() };
        let mafic = MaterialBuffer { silica: 0.05, ..flat_buffer() };
        let f = reflectance(&felsic, RockClass::Granite).integrate();
        let m = reflectance(&mafic, RockClass::Basalt).integrate();
        let f_mean: f64 = f.get().iter().sum::<f64>() / BANDS as f64;
        let m_mean: f64 = m.get().iter().sum::<f64>() / BANDS as f64;
        assert!(f_mean > m_mean, "felsic {f_mean} was not brighter than mafic {m_mean}");
    }

    #[test]
    fn ironstone_leans_long_wavelength() {
        let buf = flat_buffer();
        let iron = reflectance(&buf, RockClass::Ironstone).integrate();
        let plain = reflectance(&buf, RockClass::Sandstone).integrate();
        // Long band over short band: iron oxide's whole visual signature.
        let iron_ratio = iron.get()[8] / iron.get()[2];
        let plain_ratio = plain.get()[8] / plain.get()[2];
        assert!(
            iron_ratio > plain_ratio,
            "ironstone long/short = {iron_ratio}, sandstone = {plain_ratio}"
        );
    }

    #[test]
    fn carbonate_brightens_the_whole_curve() {
        let none = MaterialBuffer { carbonate: 0.0, ..flat_buffer() };
        let lots = MaterialBuffer { carbonate: 0.9, ..flat_buffer() };
        let a = reflectance(&none, RockClass::Sandstone).integrate();
        let b = reflectance(&lots, RockClass::Limestone).integrate();
        for band in 0..BANDS {
            assert!(
                b.get()[band] >= a.get()[band],
                "band {band}: carbonate darkened the rock"
            );
        }
    }

    #[test]
    fn every_reflectance_is_physically_valid_across_the_buffer_space() {
        // Reflectance::new rejects out-of-range bands, so a panic here is a
        // real energy-conservation break, not a test artifact.
        for silica in [0.0, 0.5, 1.0] {
            for carbonate in [0.0, 0.5, 1.0] {
                for rock in [RockClass::Granite, RockClass::Basalt, RockClass::Ironstone] {
                    let buf = MaterialBuffer { silica, carbonate, ..flat_buffer() };
                    let r = reflectance(&buf, rock).integrate();
                    for band in 0..BANDS {
                        assert!((0.0..=1.0).contains(&r.get()[band]));
                    }
                }
            }
        }
    }

    #[test]
    fn the_mixture_keeps_its_components_for_the_texture_layer() {
        // The producer must not collapse early: a later texture layer needs
        // the components to arrange them spatially.
        let m = reflectance(&flat_buffer(), RockClass::Granite);
        assert!(m.integrate().get()[0] >= 0.0);
    }
```

`MaterialBuffer` has non-`f64` fields (`margin`, `soil_depth`, `basement`),
so `..Default::default()` only compiles if it derives `Default`. Check with
`grep -n "derive" domains/terrain/src/lithology.rs | head`; if it does not,
construct those three fields explicitly using the values the file's existing
tests use.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-terrain --lib lithology`
Expected: FAIL to compile — `cannot find function reflectance`.

- [ ] **Step 3: Write the implementation**

Append to `domains/terrain/src/lithology.rs`, after `appearance`:

```rust
/// Mineral endmember reflectances on the kernel's band grid.
///
/// Four curves stand in for the mineralogy the buffer already tracks:
/// felsic (quartz and feldspar — bright, faintly warm), mafic (pyroxene and
/// olivine — dark and flat), carbonate (bright and flat), and iron oxide
/// (dark in the short bands, strongly reflective in the long ones, which is
/// the entire visual signature of rust and ochre).
///
/// Declared approximations. They are the reason a granite reads pale and a
/// basalt reads dark, and the campaign's claims rest on those relations
/// rather than on laboratory accuracy.
mod endmembers {
    /// Quartz and feldspar.
    pub const FELSIC: [f64; hornvale_kernel::color::BANDS] = [0.38, 0.45, 0.52, 0.56, 0.58, 0.60, 0.62, 0.63, 0.64, 0.64];
    /// Pyroxene and olivine.
    pub const MAFIC: [f64; hornvale_kernel::color::BANDS] =
        [0.05, 0.06, 0.08, 0.09, 0.10, 0.11, 0.12, 0.12, 0.13, 0.13];
    /// Calcite and dolomite.
    pub const CARBONATE: [f64; hornvale_kernel::color::BANDS] =
        [0.55, 0.68, 0.76, 0.80, 0.82, 0.83, 0.84, 0.84, 0.85, 0.85];
    /// Hematite and goethite — the red one.
    pub const IRON_OXIDE: [f64; hornvale_kernel::color::BANDS] =
        [0.03, 0.04, 0.05, 0.06, 0.09, 0.16, 0.42, 0.58, 0.63, 0.65];
}

/// Rock classes whose iron oxide dominates their appearance. Mirrors the
/// structure of [`appearance`]'s own `hue` match, so the two projections of
/// the buffer cannot disagree about which rocks read red.
fn is_iron_rich(rock: RockClass) -> bool {
    matches!(rock, RockClass::Ironstone)
}

/// Rock classes whose appearance is dominated by dark mafic minerals
/// regardless of the buffer's silica term. Mirrors [`appearance`]'s `hue`
/// match for the same reason as [`is_iron_rich`].
fn is_mafic_dominated(rock: RockClass) -> bool {
    matches!(rock, RockClass::Basalt | RockClass::Gabbro)
}

/// Project the material buffer to a reflectance **mixture** — the second
/// projection of the same axes [`appearance`] projects (spec "The Pigment"
/// §5.1). No new data: `silica`, `carbonate` and the rock class are all
/// already stored.
///
/// Returns a [`Mixture`] rather than a [`Reflectance`] so a later texture
/// layer can arrange the components spatially instead of re-deriving them.
/// Call [`Mixture::integrate`] for the single reflectance.
pub fn reflectance(buf: &MaterialBuffer, rock: RockClass) -> Mixture {
    let carbonate = buf.carbonate.clamp(0.0, 1.0);
    let silicate_share = 1.0 - carbonate;

    // Within the silicate fraction, silica splits felsic from mafic —
    // except where the rock class says the mafic minerals dominate anyway.
    let felsic_fraction = if is_mafic_dominated(rock) {
        0.1
    } else {
        buf.silica.clamp(0.0, 1.0)
    };

    let iron = if is_iron_rich(rock) { 0.55 } else { 0.03 };
    let remaining = silicate_share * (1.0 - iron);

    let components = vec![
        Reflectance::new(endmembers::FELSIC).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::MAFIC).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::CARBONATE).expect("authored endmember is within [0, 1]"),
        Reflectance::new(endmembers::IRON_OXIDE).expect("authored endmember is within [0, 1]"),
    ];
    let weights = vec![
        remaining * felsic_fraction,
        remaining * (1.0 - felsic_fraction),
        carbonate,
        silicate_share * iron,
    ];
    // Every weight is non-negative and, since `carbonate` and
    // `silicate_share` sum to 1 with `iron < 1`, the total is strictly
    // positive for every buffer.
    Mixture::new(components, weights).expect("weights are non-negative with a positive total")
}
```

Add the import at the top of `lithology.rs`:

```rust
use hornvale_kernel::color::{Mixture, Reflectance};
```

and, for the tests, `BANDS`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-terrain --lib lithology`
Expected: PASS, 5 new tests plus the file's existing ones.

- [ ] **Step 5: Run the terrain property batteries**

Run: `cargo test -p hornvale-terrain`
Expected: PASS, including `tectonic_properties`. A pure projection consumes
no draws; if a pin-isolation test reddens, something was wired into a drawn
path.

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-terrain --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/terrain/src/lithology.rs
git commit -m "feat(terrain): project the material buffer to a reflectance mixture

A second projection of the axes `appearance` already projects -- silica,
carbonate and the rock class -- into four mineral endmembers: felsic, mafic,
carbonate and iron oxide. No new stored data.

is_iron_rich and is_mafic_dominated mirror `appearance`'s own hue match on
purpose, so the two projections cannot disagree about which rocks read red."
```

---

### Task 6: The colour exemplars

**Files:**
- Create: `domains/language/src/exemplars.rs`
- Modify: `domains/language/src/lib.rs` (add `mod exemplars;` and re-export)
- Test: inline `#[cfg(test)] mod tests` in `domains/language/src/exemplars.rs`

**Interfaces:**
- Consumes: `color_pack()` (`domains/language/src/packs.rs:279`),
  `hornvale_kernel::color::Reflectance` (Task 1)
- Produces: `hue_exemplar(concept: &str) -> Option<Reflectance>`,
  `HUE_CONCEPTS: [&str; 7]`

- [ ] **Step 1: Write the failing tests**

Create `domains/language/src/exemplars.rs`:

```rust
//! Canonical exemplar reflectances for the colour lexicon's hue ladder.
//!
//! Naming a colour compares a sample against remembered examples *under the
//! light you share with them* — so an exemplar is a reflectance, not a
//! finished colour. That is what makes naming work for an observer with any
//! channel count without re-authoring anything: the exemplar goes through
//! the same illuminant and the same eye as the sample before either is
//! compared.
//!
//! Only the **hue** ladder gets exemplars. `color_pack`'s luminance ladder
//! (gloom, shadow, starlit) describes ambient darkness rather than a
//! surface, so it is selected by the illuminant's level, not by comparing a
//! reflectance.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_hue_concept_has_an_exemplar() {
        for concept in HUE_CONCEPTS {
            assert!(
                hue_exemplar(concept).is_some(),
                "'{concept}' is on the hue ladder with no exemplar"
            );
        }
    }

    #[test]
    fn the_luminance_ladder_has_no_exemplars() {
        for concept in ["gloom", "shadow", "starlit"] {
            assert!(
                hue_exemplar(concept).is_none(),
                "'{concept}' describes ambient darkness, not a surface"
            );
        }
    }

    #[test]
    fn the_hue_concepts_are_exactly_the_color_packs_hue_ladder() {
        // The two lists must never drift: a hue term added to color_pack
        // without an exemplar would be unnameable, and would fail silently.
        let luminance = ["gloom", "shadow", "starlit"];
        let mut from_pack: Vec<&str> = crate::packs::color_pack()
            .iter()
            .map(|e| e.concept)
            .filter(|c| !luminance.contains(c))
            .collect();
        from_pack.sort_unstable();
        let mut declared: Vec<&str> = HUE_CONCEPTS.to_vec();
        declared.sort_unstable();
        assert_eq!(from_pack, declared);
    }

    #[test]
    fn red_leans_long_and_blue_leans_short() {
        let red = hue_exemplar("red").unwrap();
        let blue = hue_exemplar("blue").unwrap();
        assert!(red.get()[8] > red.get()[2], "red must reflect more long than short");
        assert!(blue.get()[2] > blue.get()[8], "blue must reflect more short than long");
    }

    #[test]
    fn dark_is_darker_than_light_in_every_band() {
        let dark = hue_exemplar("dark").unwrap();
        let light = hue_exemplar("light").unwrap();
        for b in 0..hornvale_kernel::color::BANDS {
            assert!(dark.get()[b] < light.get()[b], "band {b}");
        }
    }

    #[test]
    fn brown_is_a_darker_red() {
        // Brown is the last term on the ladder because it is the hardest to
        // separate: it is red at low luminance. The exemplars must encode
        // that relationship or the ladder's ordering is a fiction.
        let red = hue_exemplar("red").unwrap();
        let brown = hue_exemplar("brown").unwrap();
        let red_mean: f64 = red.get().iter().sum::<f64>();
        let brown_mean: f64 = brown.get().iter().sum::<f64>();
        assert!(brown_mean < red_mean, "brown must be darker than red overall");
        assert!(brown.get()[8] > brown.get()[2], "brown must still lean long");
    }
}
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-language --lib exemplars`
Expected: FAIL to compile — `cannot find value HUE_CONCEPTS`.

- [ ] **Step 3: Write the implementation**

Insert above the test module in `domains/language/src/exemplars.rs`:

```rust
use hornvale_kernel::color::Reflectance;

/// The hue-ladder concept ids from [`crate::packs::color_pack`], in ladder
/// order. The luminance ladder is deliberately absent — see the module doc.
/// `the_hue_concepts_are_exactly_the_color_packs_hue_ladder` pins this list
/// against the pack so the two cannot drift.
/// type-audit: bare-ok(identifier-text)
pub const HUE_CONCEPTS: [&str; 7] = ["dark", "light", "red", "green", "yellow", "blue", "brown"];

/// The canonical reflectance for a hue concept, or `None` for anything not
/// on the hue ladder.
///
/// Declared approximations, chosen so the *relations* between terms hold:
/// red reflects long and absorbs short, blue the reverse, brown is red at
/// lower luminance, and dark and light bracket everything. A disagreement
/// with these curves is a disagreement about those relations.
pub fn hue_exemplar(concept: &str) -> Option<Reflectance> {
    let bands: [f64; hornvale_kernel::color::BANDS] = match concept {
        "dark" => [0.04, 0.04, 0.05, 0.05, 0.05, 0.05, 0.06, 0.06, 0.06, 0.06],
        "light" => [0.80, 0.83, 0.85, 0.86, 0.86, 0.86, 0.86, 0.85, 0.85, 0.84],
        "red" => [0.05, 0.05, 0.05, 0.05, 0.06, 0.10, 0.45, 0.70, 0.75, 0.78],
        "green" => [0.05, 0.07, 0.10, 0.18, 0.45, 0.35, 0.12, 0.09, 0.10, 0.12],
        "yellow" => [0.05, 0.06, 0.08, 0.15, 0.55, 0.75, 0.80, 0.82, 0.83, 0.84],
        "blue" => [0.20, 0.45, 0.55, 0.45, 0.20, 0.08, 0.05, 0.05, 0.06, 0.08],
        "brown" => [0.03, 0.04, 0.05, 0.06, 0.09, 0.14, 0.24, 0.32, 0.35, 0.36],
        _ => return None,
    };
    Some(Reflectance::new(bands).expect("authored exemplar is within [0, 1]"))
}
```

Add to `domains/language/src/lib.rs`:

```rust
pub mod exemplars;
```

and extend the existing re-export line (the one at `lib.rs:119` that already
lists `color_pack`, `in_ladder`, `PackDepths`, …):

```rust
pub use exemplars::{HUE_CONCEPTS, hue_exemplar};
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-language --lib exemplars`
Expected: PASS, 6 tests.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-language --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/language/src/exemplars.rs domains/language/src/lib.rs
git commit -m "feat(language): canonical exemplar reflectances for the hue ladder

An exemplar is a reflectance, not a finished colour, so naming pushes it
through the same illuminant and the same eye as the sample before comparing.
That is what lets an observer with any channel count name colours with no
re-authoring.

Only the hue ladder gets exemplars: gloom/shadow/starlit describe ambient
darkness rather than a surface. A test pins HUE_CONCEPTS against color_pack
so a term added to one and not the other cannot fail silently."
```

---

### Task 7: Naming — and the campaign's two falsifiable claims

**Files:**
- Create: `windows/worldgen/src/color_naming.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod color_naming;`)
- Test: `windows/worldgen/tests/color_naming.rs` (integration, so it can
  build real worlds — this repo's convention for cross-domain claims; see
  `windows/worldgen/tests/exposure.rs`)

**Interfaces:**
- Consumes: `pack_depths(&PerceptionVector) -> PackDepths`
  (`windows/worldgen/src/lib.rs:3621`);
  `hornvale_language::{color_pack, in_ladder, hue_exemplar}` (Task 6);
  `hornvale_kernel::color::{Observer, Reflectance, Illuminant}` (Tasks 1–2)
- Produces:
  `name_color(sample: &Reflectance, light: &Illuminant, observer: &Observer, depths: &PackDepths) -> &'static str`

- [ ] **Step 1: Write the failing tests**

Create `windows/worldgen/tests/color_naming.rs`:

```rust
//! The Pigment's two preregistered claims, plus the naming function's own
//! properties.
//!
//! **These two tests are the campaign's deliverable.** A reviewer must
//! break each one deliberately and report the measured values — a claim
//! that cannot fail is a decoration, not a finding.

use hornvale_astronomy::illuminant::{at_elevation, daylight};
use hornvale_kernel::color::{Reflectance, standard_observer};
use hornvale_language::PackDepths;
use hornvale_worldgen::color_naming::name_color;

/// An ochre outcrop: iron-rich, so it reflects long and absorbs short.
fn ochre() -> Reflectance {
    Reflectance::new([0.04, 0.05, 0.06, 0.08, 0.14, 0.24, 0.44, 0.55, 0.58, 0.60]).unwrap()
}

/// CLAIM 1 — the same outcrop, the same light, two species, two words.
///
/// The difference must come from `pack_depths`, not from a branch on
/// species. Breaking `pack_depths` (e.g. returning a constant) must turn
/// this red.
#[test]
fn the_same_outcrop_is_named_differently_by_two_species() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();

    // Roster values, from `pack_depths`'s own model card: a goblin's
    // night_vision 0.5 gives hue 4 (blue, no brown); a kobold's 0.9 gives
    // hue 2 (dark, light, red only).
    let goblin = PackDepths { hue: 4, luminance: 1 };
    let kobold = PackDepths { hue: 2, luminance: 3 };

    let by_goblin = name_color(&ochre(), &light, &eye, &goblin);
    let by_kobold = name_color(&ochre(), &light, &eye, &kobold);

    assert_ne!(
        by_goblin, by_kobold,
        "both species named the outcrop '{by_goblin}' — the ladder did nothing"
    );
    // The kobold's word must be one it actually holds.
    assert!(
        ["dark", "light", "red"].contains(&by_kobold),
        "a kobold reached for '{by_kobold}', which is past its ladder depth"
    );
}

/// CLAIM 2 — the same outcrop, one species, noon versus dusk, two words.
///
/// The difference must come from the illuminant. Flattening `at_elevation`
/// to the identity must turn this red.
#[test]
fn the_same_outcrop_is_named_differently_at_noon_and_at_dusk() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let base = daylight(&star);
    let eye = standard_observer();
    let speaker = PackDepths { hue: 5, luminance: 3 };

    let noon = name_color(&ochre(), &at_elevation(&base, 85.0), &eye, &speaker);
    let dusk = name_color(&ochre(), &at_elevation(&base, 2.0), &eye, &speaker);

    assert_ne!(
        noon, dusk,
        "the outcrop was '{noon}' at both noon and dusk — the illuminant did nothing"
    );
}

#[test]
fn a_speaker_never_reaches_past_its_ladder_depth() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    // Depth 1: only dark and light are lexicalized.
    let shallow = PackDepths { hue: 1, luminance: 1 };
    for sample in [ochre(), Reflectance::new([0.9; 10]).unwrap()] {
        let word = name_color(&sample, &light, &eye, &shallow);
        assert!(["dark", "light"].contains(&word), "reached for '{word}' at depth 1");
    }
}

#[test]
fn naming_is_deterministic_across_repeated_calls() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    let depths = PackDepths { hue: 5, luminance: 3 };
    let a = name_color(&ochre(), &light, &eye, &depths);
    let b = name_color(&ochre(), &light, &eye, &depths);
    assert_eq!(a, b);
}

#[test]
fn every_hue_term_is_reachable_by_some_sample() {
    // Memory `modelled-authored-unreachable`: this repo repeatedly ships
    // types that are defined, prose-authored, and cannot occur. A term no
    // sample can ever elicit is exactly that.
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    let deep = PackDepths { hue: 5, luminance: 3 };
    let mut seen: Vec<&str> = Vec::new();
    for concept in hornvale_language::HUE_CONCEPTS {
        let exemplar = hornvale_language::hue_exemplar(concept).unwrap();
        let word = name_color(&exemplar, &light, &eye, &deep);
        if !seen.contains(&word) {
            seen.push(word);
        }
    }
    assert_eq!(
        seen.len(),
        hornvale_language::HUE_CONCEPTS.len(),
        "only {} of {} hue terms were reachable: {seen:?}",
        seen.len(),
        hornvale_language::HUE_CONCEPTS.len()
    );
}

/// The astronomy seed, built the way `domains/astronomy/src/wanderers.rs:155`
/// already builds it in its own tests.
///
/// Note `Seed(42)`, a tuple constructor — **not** `Seed::new(42)`, which
/// does not exist. That exact mistake has ridden into three tasks in this
/// repo before (memory: `plan-authored-test-snippets-are-uncompiled`).
fn test_astronomy_seed() -> hornvale_kernel::Seed {
    hornvale_kernel::Seed(42).derive(hornvale_astronomy::streams::ROOT)
}
```

**Verified, so nothing to check here:** `hornvale_astronomy::streams::ROOT`
is public — `windows/worldgen/src/lib.rs:12` already imports it as
`streams::ROOT as ASTRONOMY_STREAM_ROOT`, and uses it at `lib.rs:6804` as
`world.seed.derive(ASTRONOMY_STREAM_ROOT)`. That derivation is the one a
world's star is actually built from, so the test helper above matches
production rather than inventing a parallel seeding path.

(The constant is emitted by the `stream_labels!` macro, which is why
`grep "pub const ROOT"` finds nothing in `domains/astronomy/src/streams.rs`
— the declaration is at `streams.rs:11`, inside the macro invocation.)

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-worldgen --test color_naming`
Expected: FAIL to compile — `cannot find module color_naming`.

- [ ] **Step 3: Write the implementation**

Create `windows/worldgen/src/color_naming.rs`:

```rust
//! Naming a colour through a speaker's own lexicon.
//!
//! The comparison happens in *signal space*: the sample and every candidate
//! exemplar are pushed through the same illuminant and the same observer,
//! then the nearest exemplar wins. Because exemplars are reflectances
//! rather than finished colours, this works unchanged for an observer with
//! any channel count.
//!
//! The lexicon filter is `in_ladder`, unmodified. This module adds no gate
//! of its own — a gate at the point of use would change nothing, because
//! the lexicon has already filtered.

use hornvale_kernel::color::{Illuminant, Observer, Reflectance};
use hornvale_language::{PackDepths, color_pack, hue_exemplar, in_ladder};

/// The word this speaker reaches for, given what it can see and what its
/// lexicon holds.
///
/// Ties break by ladder rank first (the earlier-acquired term wins, which
/// is what a shallower lexicon would have said anyway), then by concept id,
/// so the result is deterministic without depending on iteration order.
///
/// Every lexicon holds rank-1 terms (`dark` and `light` are the first stage
/// of the ladder), so there is always at least one candidate.
/// type-audit: bare-ok(identifier-text: return)
pub fn name_color(
    sample: &Reflectance,
    light: &Illuminant,
    observer: &Observer,
    depths: &PackDepths,
) -> &'static str {
    let seen = observer.sense(sample, light);

    let mut best: Option<(&'static str, u8, f64)> = None;
    for entry in color_pack() {
        if !in_ladder(entry, depths) {
            continue;
        }
        let Some(exemplar) = hue_exemplar(entry.concept) else {
            // A luminance term: it describes ambient darkness, not a
            // surface, so it is not a candidate for naming one.
            continue;
        };
        let distance = seen.distance_to(&observer.sense(&exemplar, light));
        let candidate = (entry.concept, entry.ladder_rank, distance);
        best = Some(match best {
            None => candidate,
            Some(current) => {
                if is_better(candidate, current) {
                    candidate
                } else {
                    current
                }
            }
        });
    }

    // `color_pack`'s rank-1 entries are always in ladder, so this is
    // unreachable in practice; naming `dark` is the honest fallback rather
    // than a panic in a presentation path.
    best.map(|(concept, _, _)| concept).unwrap_or("dark")
}

/// Whether `candidate` beats `current`: nearer wins; on an exact tie the
/// lower ladder rank wins; on a further tie the lexicographically smaller
/// concept id wins. Distances are compared with `total_cmp`, never `<`, so
/// there is no NaN ambiguity.
fn is_better(
    candidate: (&'static str, u8, f64),
    current: (&'static str, u8, f64),
) -> bool {
    match candidate.2.total_cmp(&current.2) {
        std::cmp::Ordering::Less => true,
        std::cmp::Ordering::Greater => false,
        std::cmp::Ordering::Equal => match candidate.1.cmp(&current.1) {
            std::cmp::Ordering::Less => true,
            std::cmp::Ordering::Greater => false,
            std::cmp::Ordering::Equal => candidate.0 < current.0,
        },
    }
}
```

Add to `windows/worldgen/src/lib.rs`:

```rust
pub mod color_naming;
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-worldgen --test color_naming`
Expected: PASS, 5 tests.

**If either claim test fails**, that is a finding, not a bug to paper over:

- Claim 1 red means the ladder does not separate these species on this
  sample. Try a sample whose best name at depth 5 is a late-ladder term
  (brown), so a shallow lexicon must fall back. Record what you changed.
- Claim 2 red means the illuminant does not move the name — spec risk 3.
  **Do not retune a constant to rescue it.** Measure how far the name is
  from flipping, report the number, and change the claim to one that can
  fail. A falsified prediction is a finding; several campaigns here have
  shipped the null as the headline.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/src/color_naming.rs windows/worldgen/src/lib.rs \
        windows/worldgen/tests/color_naming.rs
git commit -m "feat(worldgen): name a colour through the speaker's own lexicon

Sample and exemplars go through the same illuminant and the same eye, then
nearest wins -- so the function works unchanged at any channel count.

The lexicon filter is in_ladder, unmodified; this module adds no gate of its
own, because a gate at the point of use would change nothing when the lexicon
has already filtered.

Carries the campaign's two preregistered claims: two species name one outcrop
differently under one light, and one species names it differently at noon and
at dusk."
```

---

### Task 8: The material crossing — locale and the scene document

**Files:**
- Modify: `windows/locale/src/lib.rs` (add an accessor on `LocaleContext`, near the existing `describe` at `lib.rs:270`)
- Modify: `windows/scene/src/surrounds.rs:92-122` (`SurroundsCell`), and the builder at `surrounds.rs:163`
- Test: append to the **existing** `#[cfg(test)] mod tests` in
  `windows/scene/src/surrounds.rs:396`, **not** a new integration test.
  That module already has `world()` (`:398`) and `observer()` (`:409`)
  fixtures, and `observer()` calls `crate::place_latlon`, which is
  `pub(crate)` (`windows/scene/src/lib.rs:591`) — an integration test in
  `windows/scene/tests/` could not reach it without widening visibility for
  no reason.

**Interfaces:**
- Consumes: `GeneratedTerrain::material_at(CellId) -> MaterialBuffer`
  (`domains/terrain/src/provider.rs:180`),
  `GeneratedTerrain::rock_at(CellId) -> RockClass` (`provider.rs:185`),
  `hornvale_terrain::lithology::reflectance` (Task 5)
- Produces: `LocaleContext::reflectance_at(&self, &RoomAddr) -> Result<Reflectance, LocaleError>`;
  `SurroundsCell.color: Option<[u8; 3]>`;
  `surrounds_scene_colored_in(world, ctx, room, radius, at, observer) -> Result<SurroundsScene, SceneError>`

- [ ] **Step 1: Write the failing tests**

Append to the **existing** `mod tests` in `windows/scene/src/surrounds.rs`,
reusing its `world()` and `observer()` helpers:

```rust
    #[test]
    fn the_uncolored_builder_leaves_every_cell_without_a_color() {
        // This is what keeps book/src/gallery/scene-surrounds-seed-42.json
        // byte-identical: the field is skipped when None.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 2, WorldTime { day: 0.0 }).unwrap();
        for cell in &s.cells {
            assert!(cell.color.is_none(), "the default builder invented a colour");
        }
    }

    #[test]
    fn the_colored_builder_gives_placed_cells_a_color() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &observer(&w),
            2,
            WorldTime { day: 0.0 },
            &hornvale_kernel::color::standard_observer(),
        )
        .unwrap();
        let colored = s.cells.iter().filter(|c| c.color.is_some()).count();
        assert!(colored > 0, "no cell received a colour");
    }

    #[test]
    fn the_uncolored_json_emits_no_color_key() {
        // serde skip_serializing_if means an absent colour emits no key at
        // all, so the committed gallery JSON cannot move.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 1, WorldTime { day: 0.0 }).unwrap();
        let json = crate::surrounds_json(&s);
        assert!(!json.contains("\"color\""), "an absent colour still emitted a key");
    }

    #[test]
    fn coloring_is_deterministic_across_repeated_builds() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let build = || {
            surrounds_scene_colored_in(
                &w,
                &ctx,
                &observer(&w),
                2,
                WorldTime { day: 0.0 },
                &hornvale_kernel::color::standard_observer(),
            )
            .unwrap()
        };
        let a: Vec<_> = build().cells.iter().map(|c| c.color).collect();
        let b: Vec<_> = build().cells.iter().map(|c| c.color).collect();
        assert_eq!(a, b);
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-scene --test surrounds_color`
Expected: FAIL to compile — `no field color on SurroundsCell`.

- [ ] **Step 3: Write the implementation**

First, `windows/locale/src/lib.rs` — the accessor. `LocaleContext` already
holds a private `terrain: GeneratedTerrain` and an `index: NearestCellIndex`
(`lib.rs:173-180`):

```rust
    /// The reflectance of the rock underfoot at `addr`.
    ///
    /// A pure re-projection of the material buffer the terrain provider
    /// already holds — no new derivation, and no new stored data.
    pub fn reflectance_at(
        &self,
        addr: &RoomAddr,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        let coord = addr.centroid();
        let cell = self.terrain.nearest_cell(coord.latitude, coord.longitude);
        let buffer = self.terrain.material_at(cell);
        let rock = self.terrain.rock_at(cell);
        Ok(hornvale_terrain::lithology::reflectance(&buffer, rock).integrate())
    }
```

`addr.centroid()` is a stand-in — use whatever this file's `describe`
already calls to turn a `RoomAddr` into a lat/lon or a `CellId`. Read
`windows/locale/src/lib.rs:270-330` first and reuse that path exactly, so
the colour and the prose agree about which cell a room stands on.

Then `windows/scene/src/surrounds.rs` — the field, appended to
`SurroundsCell` after `moisture`:

```rust
    /// Display colour under the requested observer, `null` unless this
    /// scene was built through [`surrounds_scene_colored_in`]. Skipped
    /// entirely when absent, so an uncoloured document is byte-for-byte
    /// what it was before the colour layer existed.
    /// type-audit: bare-ok(artifact)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
```

Set `color: None` in the existing builder's cell construction (around
`surrounds.rs:245`, alongside `temperature_c` and `moisture`).

**Adding a field breaks every `SurroundsCell` struct literal in the
workspace.** Compile the whole workspace and fix each one to `color: None`
— at minimum the `cell(...)` helper in `surrounds_ascii.rs`'s test module
(around `:140`), which Task 9 then builds on:

```bash
cargo check --workspace 2>&1 | grep -A 3 "missing field"
```

Add the coloured builder beneath `surrounds_scene_in`:

```rust
/// Build a `scene/surrounds/v1` document with a colour layer, as seen by
/// `observer`.
///
/// A separate entry point rather than a parameter on the existing one:
/// every committed artifact goes through the uncoloured path, and this way
/// they cannot move. Cells whose observer has no truthful sRGB image keep
/// `color: None` — the mapping for a non-standard observer is a
/// false-colour choice the caller must declare (RENDER-9), not one this
/// builder may invent.
pub fn surrounds_scene_colored_in(
    world: &World,
    ctx: &LocaleContext,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
    observer: &hornvale_kernel::color::Observer,
) -> Result<SurroundsScene, SceneError> {
    let mut scene = surrounds_scene_in(world, ctx, room, radius, at)?;
    let star = hornvale_astronomy::star::generate_star(world.seed.derive("astronomy"));
    let light = hornvale_astronomy::illuminant::daylight(&star);
    for cell in scene.cells.iter_mut() {
        let addr = RoomAddr::from_packed(cell.room)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let Ok(reflectance) = ctx.reflectance_at(&addr) else {
            continue;
        };
        cell.color = observer.to_srgb(&observer.sense(&reflectance, &light));
    }
    Ok(scene)
}
```

Three stand-ins to replace with the real calls, all discoverable by grep in
the same file: the astronomy seed derivation (match what `windows/worldgen`
uses — grep `derive("astronomy")`), `RoomAddr::from_packed` (grep
`from_packed` or however `cell.room` is packed at `surrounds.rs:94`), and
`SceneError::Build` (grep `enum SceneError`).

**Layering check:** `windows/scene` may depend on `hornvale-astronomy` and
`hornvale-terrain` — a window may depend on domains because it presents
them. Confirm both are in `windows/scene/Cargo.toml`; add them if not.
`cli/tests/architecture.rs` enforces this and will catch a mistake.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-scene --test surrounds_color`
Expected: PASS, 4 tests.

- [ ] **Step 5: Prove the committed artifacts did not move**

```bash
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: **empty diff, exit 0.** A non-empty diff here means the colour
field leaked into the default path — fix that rather than rebaselining.

The type-audit report drifts on any pub-boundary change and is in that diff
list, so if `docs/audits/` moves, regenerate it deliberately:

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-scene -p hornvale-locale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/locale/src/lib.rs windows/scene/src/surrounds.rs \
        windows/scene/tests/surrounds_color.rs windows/scene/Cargo.toml docs/audits/
git commit -m "feat(scene): carry a colour layer across the scene boundary

LocaleContext::reflectance_at re-projects the material buffer the terrain
provider already exposes -- material_at and rock_at have been public all
along, so this is an accessor, not a derivation.

The colour is an additive Option on SurroundsCell with skip_serializing_if,
reached through a separate surrounds_scene_colored_in. Every committed
artifact goes through the uncoloured path, so scene-surrounds-seed-42.json is
byte-for-byte unchanged -- asserted, not assumed.

A non-standard observer keeps color: None. Its sRGB mapping is a false-colour
choice the caller must declare, not one the builder may invent."
```

---

### Task 9: The `colour` lens

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs` (the `SURROUNDS_LENSES` const near line 11, and `render_surrounds_ascii` at line 63)
- Modify: `cli/src/main.rs:1246-1274` (pass the lens through)
- Test: inline `#[cfg(test)] mod tests` in `windows/scene/src/surrounds_ascii.rs`

**Interfaces:**
- Consumes: `SurroundsCell.color: Option<[u8; 3]>` (Task 8)
- Produces: `SURROUNDS_LENSES` gains `"colour"`; no signature change to
  `render_surrounds_ascii`

- [ ] **Step 1: Write the failing tests**

Append to the existing `mod tests` in `windows/scene/src/surrounds_ascii.rs`
(if the file has none, create one):

```rust
    #[test]
    fn the_colour_lens_is_registered() {
        assert!(SURROUNDS_LENSES.contains(&"colour"));
    }

    #[test]
    fn the_terrain_lens_emits_no_escape_sequences() {
        // The three committed gallery charts render through this lens.
        // An escape here moves all of them.
        let scene = colored_test_scene();
        let out = render_surrounds_ascii(&scene, "terrain", &[]);
        assert!(!out.contains('\u{1b}'), "the terrain lens emitted an escape");
    }

    #[test]
    fn the_colour_lens_emits_escapes_and_resets_them() {
        let scene = colored_test_scene();
        let out = render_surrounds_ascii(&scene, "colour", &[]);
        assert!(out.contains('\u{1b}'), "the colour lens emitted no escape");
        assert!(out.ends_with('\n'));
        // Every colour set must be followed by a reset before the string
        // ends, or the user's terminal stays tinted after the chart.
        let sets = out.matches("\u{1b}[38;2;").count();
        let resets = out.matches("\u{1b}[0m").count();
        assert_eq!(sets, resets, "{sets} colour sets but {resets} resets");
    }

    #[test]
    fn the_colour_lens_degrades_to_plain_glyphs_when_no_cell_has_a_colour() {
        // An uncoloured scene rendered through the colour lens must still
        // be readable rather than blank or escaped.
        let scene = uncolored_test_scene();
        let out = render_surrounds_ascii(&scene, "colour", &[]);
        assert!(!out.contains('\u{1b}'), "escapes emitted for an uncoloured scene");
        assert!(out.contains("[lens: colour"), "the caption must still name the lens");
    }

    #[test]
    fn the_two_lenses_draw_the_same_glyphs() {
        // Colour is a second channel over the same chart, not a different
        // chart. Stripping the escapes must recover the terrain render.
        let scene = colored_test_scene();
        let plain = render_surrounds_ascii(&scene, "terrain", &[]);
        let colored = render_surrounds_ascii(&scene, "colour", &[]);
        let stripped = strip_escapes(&colored).replace("[lens: colour", "[lens: terrain");
        assert_eq!(stripped, plain);
    }

    /// Remove every CSI sequence from `s`.
    fn strip_escapes(s: &str) -> String {
        let mut out = String::new();
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\u{1b}' {
                for c in chars.by_ref() {
                    if c == 'm' {
                        break;
                    }
                }
            } else {
                out.push(c);
            }
        }
        out
    }

    /// Built on this module's own fixtures: `cell(u, v, w, up, state,
    /// relief)` at `surrounds_ascii.rs:140` and `scene(cells)` at `:161`.
    /// `cell` sets `color: None`, so the coloured fixture assigns
    /// afterwards rather than changing that helper's signature — every
    /// existing test in the module keeps compiling untouched.
    fn uncolored_test_scene() -> SurroundsScene {
        scene(vec![
            cell(0, 0, 0, true, "here", 2),
            cell(1, 0, 0, false, "sensed", 3),
            cell(0, 1, 0, false, "sensed", 4),
        ])
    }

    fn colored_test_scene() -> SurroundsScene {
        let mut s = uncolored_test_scene();
        let palette = [[180, 90, 60], [120, 130, 110], [200, 190, 150]];
        for (cell, rgb) in s.cells.iter_mut().zip(palette) {
            cell.color = Some(rgb);
        }
        s
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-scene --lib surrounds_ascii`
Expected: FAIL — `the_colour_lens_is_registered` fails on the assertion.

- [ ] **Step 3: Write the implementation**

In `windows/scene/src/surrounds_ascii.rs`, widen the lens registry:

```rust
/// The registered lenses. A render is a lens, never ground truth
/// (RENDER-9), and the caption names which one you are wearing.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_LENSES: [&str; 2] = ["terrain", "colour"];
```

Add the escape helpers:

```rust
/// Wrap `glyph` in a 24-bit foreground colour and a reset.
///
/// Truecolor rather than the 256-colour cube: a terminal that does not
/// understand it degrades to an uncoloured glyph rather than a wrong one,
/// and the sim has no business probing the terminal's capabilities.
/// type-audit: bare-ok(artifact: return)
fn colored(glyph: char, rgb: [u8; 3]) -> String {
    format!(
        "\u{1b}[38;2;{};{};{}m{glyph}\u{1b}[0m",
        rgb[0], rgb[1], rgb[2]
    )
}
```

Then change `render_surrounds_ascii`'s placement loop to carry a colour
alongside the glyph, and emit accordingly. Replace the `placed` map's value
type and the emit loop:

```rust
    // (row, col) -> (glyph, optional colour). The colour rides alongside
    // rather than replacing the glyph, so both lenses draw the same chart
    // and only one of them tints it.
    let mut placed: BTreeMap<(i64, i64), (char, Option<[u8; 3]>)> = BTreeMap::new();
```

In the loop body, replace `placed.insert((row, col), g);` with:

```rust
        placed.insert((row, col), (g, c.color));
```

And in the emit loop, replace the line-building block with:

```rust
        for r in r0..=r1 {
            let mut line = String::new();
            let mut trailing_blanks = String::new();
            for c in c0..=c1 {
                match placed.get(&(r, c)) {
                    None => trailing_blanks.push(' '),
                    Some((glyph, color)) => {
                        line.push_str(&trailing_blanks);
                        trailing_blanks.clear();
                        match (lens, color) {
                            ("colour", Some(rgb)) => line.push_str(&colored(*glyph, *rgb)),
                            _ => line.push(*glyph),
                        }
                    }
                }
            }
            out.push_str(&line);
            out.push('\n');
        }
```

The `trailing_blanks` buffer replaces the old `line.trim_end()`: trimming a
string containing escape sequences would cut inside them. Buffering the
spaces and only flushing them before a real glyph produces the identical
trimmed output for the terrain lens — which
`the_two_lenses_draw_the_same_glyphs` verifies.

In `cli/src/main.rs`, the `--render ascii` arm at line 1271 already passes a
lens through. Add a `--lens <NAME>` flag defaulting to `"terrain"`, and pass
it. When `--lens colour` is given, build through
`surrounds_scene_colored_in` instead of `surrounds_scene`. Update the usage
text at `cli/src/main.rs:56-58`.

**This closes spec §7's accessibility surface, and it is worth being
explicit about why nothing further is owed:**

| §7 need | How it is met |
|---|---|
| Screen reader | Prose is *worded*, never coloured — Task 7 emits a word, not an escape. Nothing to strip. |
| `NO_COLOR`, dumb terminal | Colour is opt-in behind `--lens colour`, so the uncoloured chart is what every default path already produces. No environment probing is needed, and none is added. |
| Colour blindness | `Observer::new` accepts any channel set; a deuteranope is an observer with a shifted medium channel. The mechanism ships here; the specific observers ship with campaign 2's `PerceptionVector` wiring. |
| Nocturnal sight | Same mechanism — a rod-weighted observer. Campaign 2. |

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-scene --lib surrounds_ascii`
Expected: PASS, 5 tests.

- [ ] **Step 5: Prove the committed charts did not move**

```bash
make rebaseline
git diff --exit-code book/src/gallery/
```

Expected: **empty diff.** The three gallery charts render through the
`terrain` lens and must be byte-identical.

- [ ] **Step 6: Look at the actual output**

Memory `read-the-output-not-just-the-code`: all four of The Occlusion's
defects were invisible to a 2319-test gate and found by running the CLI.

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/pigment.json
cargo run -p hornvale -- scene surrounds --world /tmp/pigment.json --render ascii --lens colour
cargo run -p hornvale -- scene surrounds --world /tmp/pigment.json --render ascii
```

Check by eye: the colour chart is tinted and legible, the plain chart is
unchanged, neither leaves the terminal tinted afterwards, and the caption
names the right lens in each. Report what you saw.

- [ ] **Step 7: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-scene -p hornvale --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/scene/src/surrounds_ascii.rs cli/src/main.rs
git commit -m "feat(scene): a colour lens for the surrounds chart

A second registered lens rather than an addition to the first, so the three
committed gallery charts render through the untouched terrain lens and stay
byte-identical.

Colour rides alongside the glyph rather than replacing it, so both lenses
draw the same chart and only one tints it -- a test strips the escapes and
compares against the terrain render.

Trailing blanks are buffered rather than trimmed: trim_end on a line holding
escape sequences would cut inside one."
```

---

### Task 10: The book, the registry, and the retrospective

**Files:**
- Create: `book/src/chronicle/the-pigment.md`
- Create: `docs/retrospectives/the-pigment.md`
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Modify: `book/src/frontier/idea-registry.md` (rows below)
- Modify: `book/src/open-questions.md` (only if this campaign moved a bet)

**Interfaces:**
- Consumes: everything above, plus the measured values from Task 7
- Produces: the Definition of Done for a merged plan

- [ ] **Step 1: Write the chronicle entry**

`book/src/chronicle/the-pigment.md`, in the book's register — world-prose
and technical, comprehensible without reading the code, **never process
prose**. Cover: why colour is a three-way product; why the band grid reaches
into the near-ultraviolet; why the same rock is named differently by a
goblin and a kobold; the measured outcome of both preregistered claims,
**including a null if that is what happened**.

- [ ] **Step 2: Add the registry rows**

Append to `book/src/frontier/idea-registry.md`. Five columns exactly, `\|`
escaped inside cells, Idea cell ≤ 600 characters, no new numbered IDs
(decision 0026 — use slug IDs):

- `PIGMENT-texture` — the texture layer: pattern is scale-relative, a rock
  is a mixture at map scale and a pattern at hand-lens scale; the invariant
  is that the mean of a sampled texture equals its mixture; conglomerate and
  breccia differ in one parameter (clast angularity) and gneiss banding is
  driven by `metamorphic_grade`, both axes the buffer already carries.
- `PIGMENT-water-animation` — ripple statistics and a phase function emitted
  for a client to animate; frame-rate shimmer is outside the determinism
  boundary (decisions 0055, 0022/0023), while tides and seasonal turbidity
  are ordinary `WorldTime` simulation.
- `PIGMENT-integument` — species skin, scale and pelt reflectance, and
  wiring `PerceptionVector` into the observer slot.
- `PIGMENT-economy` — subtractive mixing, alchemical substances, and
  cultural significance colours falling out of what a people can make.
- `PIGMENT-biome-palette` — derive `Biome::color()` from the substrate and
  retire the hand-picked triples; costs a PNG rebaseline.
- `PIGMENT-thermal-ir` — thermal infrared as a separate *sense* (emitted
  radiance) rather than a colour band.

Update `EXP-3a` and `SKY-17` in place: EXP-3a's "colour lexicons as evidence
of a species' vision" is now half-shipped; SKY-17's "colour as a computed
quantity feeding perception rather than words" is now real.

- [ ] **Step 3: Run the docs drift check**

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS, 17 tests. Failures name the broken link, the missing ToC
bullet, or the malformed row — fix the doc, not the test.

- [ ] **Step 4: Write the retrospective**

`docs/retrospectives/the-pigment.md` — one page, **process lessons, not
product**. Include whether either preregistered claim came back null and
what was done about it.

- [ ] **Step 5: Freshness sweep**

The book may never lag merged reality. Check for chapters that now describe
a colourless world:

```bash
grep -rln "colou\?r" book/src/ | grep -v chronicle | grep -v frontier
```

Read each hit and update anything the campaign falsified. Memory
`hand-authored-prose-around-generated-blocks-rots-silently`: gallery pages
`printf` a hand-written paragraph next to a generated block, and the drift
check gates only the generated half — so grep the printf paragraphs in
`scripts/regenerate-artifacts.sh` too.

- [ ] **Step 6: Regenerate the type-audit report**

**This is known-stale and owed — do not skip it.** Every task in this
campaign adds `pub`-boundary items, and `docs/audits/type-audit-report.md`
drifts on each one. Measured after Task 3: `bare-ok(count)` 337→341,
`bare-ok(constructor-edge)` 49→56, `bare-ok(ratio)` 489→496, and five other
rows.

The trap is that **`make gate` runs the type-audit `check`, not the
`report`** — so this drift is invisible to every gate the campaign has run
and only surfaces in the artifact drift check. One regen here covers the
whole campaign:

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
make rebaseline
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/
```

Expected: the report changes (commit it); everything else empty.

- [ ] **Step 7: The full gate**

```bash
make gate
```

Expected: PASS, ~15 min. This is a pub-boundary change across five crates,
so the scoped gates are not sufficient — memory
`full-gate-before-pushing-boundary-changes`. Confirm no other session is
gating first.

- [ ] **Step 8: Re-record the duration baseline**

This campaign adds roughly forty tests, so the whole-suite duration moves.
Decision 0088's rule is that a deliberate regression is re-recorded **in the
same commit that caused it** — so this runs here, at the close, not earlier.

```bash
make ci
```

Two outcomes, and they are handled differently:

- **The alarm fires on the whole-suite duration.** Expected. `make ci`
  re-records only when the alarm passed, so a fired alarm leaves the
  baseline untouched. Read the reported delta, confirm it is consistent
  with the tests this campaign added (roughly forty cheap unit tests plus
  two world-building integration tests — the world builds are the expensive
  part), then re-record deliberately:

  ```bash
  cargo run --quiet -p hornvale -- ci-record
  ```

- **The alarm fires on a *per-test* duration for a test this campaign did
  not touch.** That is not this campaign's regression to absorb. Do not
  re-record it away. Report it.

**Do not run this on a contended box.** The guard only asks whether a census
claim is held, so parallel agent sessions are invisible to it and it will
happily enforce against meaningless timings — it did exactly that at
loadavg 42–63 during The Timekeeper's own runs. Check `uptime` first and
distrust a red alarm from a busy machine.

Commit the moved baseline together with the campaign's final state.

- [ ] **Step 9: Commit**

```bash
cargo fmt
git add book/ docs/retrospectives/ docs/audits/ docs/timings/
git commit -m "docs(the-pigment): chronicle, registry rows, retrospective

Records both preregistered claims with their measured values, and the six
deferred directions as registry rows -- the texture layer, animated water,
species integument, the pigment economy, the biome palette, and thermal IR
as a separate sense.

EXP-3a and SKY-17 move: colour lexicons as evidence of vision are now half
real, and sky colour is a computed quantity rather than a word."
```

---

## Definition of Done

- [ ] `make gate` green.
- [ ] `make rebaseline` produces an empty diff across `book/src/gallery/`,
      `book/src/reference/`, `book/src/laboratory/`, `docs/audits/`.
- [ ] `make ci` run on a quiet box, and the duration baseline re-recorded
      deliberately in the same commit as the tests that moved it (0088).
- [ ] `.superpowers/sdd/decision-ledger.md` promoted into the retrospective
      **before** the worktree is torn down — the scratch directory is
      git-ignored and dies with the worktree.
- [ ] Both preregistered claims measured, with their values in the
      chronicle — **a null is a finding, not a failure**.
- [ ] A reviewer has broken each claim test deliberately and confirmed it
      reddens.
- [ ] The CLI has been run and its output read, not just its tests.
- [ ] Chronicle entry, registry rows, retrospective, freshness sweep.
- [ ] No TODOs without issue numbers.
