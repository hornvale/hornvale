# Firm Ground II, Plan 1: The Moving Sky — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the anchor's frozen orbital elements into slow functions of `WorldTime` — the moon-coupled Milankovitch triad (obliquity, eccentricity, precession) plus per-body phase offsets — and ship the forcing pin the Year-3 exit criterion varies.

**Architecture:** A new `domains/astronomy/src/forcing.rs` module owns `OrbitalForcing`: the drawn amplitudes/phases and the pure `obliquity_at(t)`/`eccentricity_at(t)`/`precession_at(t)` functions. `Anchor.obliquity` is unchanged (it stays ε₀, the present/mean value, so `provider.rs` salience math is untouched); `StarSystem` gains a `forcing` field; `Calendar` reads the forcing for time-varying daylight and apsidal seasons. New labeled streams carry the draws; a `ForcingPin` sets them to zero for the null control. Nothing consumes the deep-time behavior yet — that is Campaign 20.

**Tech Stack:** Rust edition 2024, std only (decision 0004). Crates touched: `hornvale-astronomy` only (plus its Lab metric in `hornvale-lab`). Determinism via the kernel `Seed`/`Stream`; no `HashMap`/`HashSet`; `total_cmp` for float order.

## Global Constraints

- **`serde` + `serde_json` only; no new crates** (decision 0004; enforced by `cli/tests/architecture.rs`). This plan adds no dependency.
- **Determinism (constitutional):** same seed + same pins → byte-identical system. No wall-clock, no `HashMap`/`HashSet`. New labeled streams derive independently, so **existing draws (star mass, orbit, obliquity ε₀, moons, neighbors) must not shift** — verify by the existing `system::generate_is_deterministic` plus a new byte-identity check.
- **Save-format contracts (decision 0006):** new stream labels are permanent, declared as `pub const` in `streams.rs` and published via `stream_labels()`. A pin must **consume the same draws as the unpinned path** (pin-isolation), asserted by test in the `normal_pin_matches_the_unpinned_draw_for_spinning_worlds` pattern (`anchor.rs`).
- **The drift-anchor identity:** every pre-existing element evaluated at `t = 0` equals its genesis draw — `obliquity_at(0) == anchor.obliquity`, byte-for-byte. Achieved with the `− sin(φ)` anchor term (Task 1).
- **`#![warn(missing_docs)]`:** every public item, field, and variant gets a `///` doc comment.
- **The full gate before every commit:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` (no `--check`) as the final step of each task. Never `--no-verify`.
- **Periods** are fixed constants near real Milankovitch values, expressed in **standard days**: `P_OBLIQUITY = 41_000.0 * 365.25`, `P_ECCENTRICITY = 100_000.0 * 365.25`, `P_PRECESSION = 21_000.0 * 365.25`.
- **Baseline:** confirm the astronomy test count directly before starting (`cargo test -p hornvale-astronomy 2>&1 | tail -3`) and after each task.

---

## File Structure

| File | Action | Responsibility |
| --- | --- | --- |
| `domains/astronomy/src/streams.rs` | Modify | Add the forcing + phase-offset stream labels. |
| `domains/astronomy/src/forcing.rs` | Create | `OrbitalForcing`: drawn params, moon coupling, `element_at(t)`, phase offsets. |
| `domains/astronomy/src/lib.rs` | Modify | `pub mod forcing;` + `stream_labels()` gains the new labels. |
| `domains/astronomy/src/system.rs` | Modify | `StarSystem` gains `forcing`; `generate` builds it after moons. |
| `domains/astronomy/src/pins.rs` | Modify | `ForcingPin`, the `SkyPins.forcing` field, `pin_strings`, parsing. |
| `domains/astronomy/src/calendar.rs` | Modify | `Calendar` holds the forcing; `daylight_fraction`/`season_phase` become time-varying with an apsidal term. |
| `domains/astronomy/src/facts.rs` | Modify | Commit the forcing parameters as facts. |
| `domains/astronomy/tests/genesis_properties.rs` | Modify | Property battery: formula checks, `t=0` anchor, moon-coupling consequence, pin isolation. |
| `windows/lab/src/metrics.rs` | Modify | An obliquity-range metric for the moon-coupling calibration. |
| `windows/lab/tests/calibration.rs` | Modify | The obliquity-range ⇔ moon-presence calibration. |

Dataflow: `system::generate` draws the forcing (after moons, using them for coupling) → `StarSystem.forcing` → `calendar_of` copies it into `Calendar` → `daylight_fraction(t)` evaluates it. `pins.forcing = Some(ForcingPin::Zero)` zeroes the amplitudes while still consuming the draws.

---

### Task 1: The forcing math (pure, no draws)

**Files:**
- Create: `domains/astronomy/src/forcing.rs`
- Modify: `domains/astronomy/src/lib.rs` (add `pub mod forcing;`)

**Interfaces:**
- Produces: `pub struct OrbitalForcing { obliquity_mean, obliquity_amp, obliquity_phase, ecc_mean, ecc_amp, ecc_phase, precession_phase }` (all `f64`, degrees for obliquity, dimensionless for eccentricity, radians for phases); `pub fn obliquity_at(&self, t: f64) -> f64`, `pub fn eccentricity_at(&self, t: f64) -> f64`, `pub fn precession_at(&self, t: f64) -> f64`. `t` is absolute standard days.

- [ ] **Step 1: Add the module.** In `domains/astronomy/src/lib.rs`, add `pub mod forcing;` to the module list (alphabetical, after `pub mod facts;`).

- [ ] **Step 2: Write the failing test.** Create `domains/astronomy/src/forcing.rs`:

```rust
//! Deep-time orbital forcing (SKY-1/2/4/21): the Milankovitch triad as slow
//! functions of `WorldTime`. Produced at genesis, anchored so that every
//! element at `t = 0` equals its genesis draw. Consumed by paleoclimate
//! (Campaign 20), not here.
#![warn(missing_docs)]

use std::f64::consts::TAU;

/// Obliquity oscillation period, standard days (~41 kyr).
pub const P_OBLIQUITY: f64 = 41_000.0 * 365.25;
/// Eccentricity oscillation period, standard days (~100 kyr).
pub const P_ECCENTRICITY: f64 = 100_000.0 * 365.25;
/// Axial-precession period, standard days (~21 kyr).
pub const P_PRECESSION: f64 = 21_000.0 * 365.25;

/// The deep-time forcing of one world: means, amplitudes, and phases for the
/// Milankovitch triad. All fields are set at genesis; the `*_at` methods are
/// pure functions of absolute standard days.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OrbitalForcing {
    /// Mean obliquity, degrees (equals the anchor's genesis obliquity ε₀).
    pub obliquity_mean: f64,
    /// Obliquity oscillation amplitude, degrees (moon-coupled, Task 3).
    pub obliquity_amp: f64,
    /// Obliquity oscillation phase, radians.
    pub obliquity_phase: f64,
    /// Mean orbital eccentricity, dimensionless.
    pub ecc_mean: f64,
    /// Eccentricity oscillation amplitude, dimensionless.
    pub ecc_amp: f64,
    /// Eccentricity oscillation phase, radians.
    pub ecc_phase: f64,
    /// Axial-precession phase at genesis, radians.
    pub precession_phase: f64,
}

impl OrbitalForcing {
    /// Obliquity at absolute time `t` (standard days), anchored so
    /// `obliquity_at(0.0) == obliquity_mean` exactly.
    pub fn obliquity_at(&self, t: f64) -> f64 {
        self.obliquity_mean
            + self.obliquity_amp
                * ((TAU * t / P_OBLIQUITY + self.obliquity_phase).sin()
                    - self.obliquity_phase.sin())
    }
    /// Eccentricity at `t`; a genuinely new element, so `eccentricity_at(0.0)`
    /// is `ecc_mean + ecc_amp*sin(ecc_phase)` (the present gains eccentricity).
    pub fn eccentricity_at(&self, t: f64) -> f64 {
        (self.ecc_mean + self.ecc_amp * (TAU * t / P_ECCENTRICITY + self.ecc_phase).sin()).max(0.0)
    }
    /// Precession phase at `t`, radians, wrapping over `P_PRECESSION`.
    pub fn precession_at(&self, t: f64) -> f64 {
        self.precession_phase + TAU * t / P_PRECESSION
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> OrbitalForcing {
        OrbitalForcing {
            obliquity_mean: 23.5,
            obliquity_amp: 1.3,
            obliquity_phase: 0.7,
            ecc_mean: 0.017,
            ecc_amp: 0.02,
            ecc_phase: 1.1,
            precession_phase: 0.4,
        }
    }

    #[test]
    fn obliquity_is_anchored_at_t0() {
        assert_eq!(sample().obliquity_at(0.0), 23.5);
    }

    #[test]
    fn obliquity_oscillates_within_amplitude() {
        let f = sample();
        for k in 0..1000 {
            let t = k as f64 * P_OBLIQUITY / 500.0;
            let d = (f.obliquity_at(t) - f.obliquity_mean).abs();
            assert!(d <= f.obliquity_amp + 1e-9, "obliquity left its band at t={t}");
        }
    }

    #[test]
    fn eccentricity_never_negative() {
        let f = OrbitalForcing { ecc_mean: 0.01, ecc_amp: 0.05, ..sample() };
        for k in 0..1000 {
            let t = k as f64 * P_ECCENTRICITY / 500.0;
            assert!(f.eccentricity_at(t) >= 0.0);
        }
    }

    #[test]
    fn precession_advances_one_turn_per_period() {
        let f = sample();
        let d = f.precession_at(P_PRECESSION) - f.precession_at(0.0);
        assert!((d - TAU).abs() < 1e-9);
    }
}
```

- [ ] **Step 3: Run to see it fail.** `cargo test -p hornvale-astronomy forcing 2>&1 | tail -6` — expected FAIL (module compiles once `lib.rs` has `pub mod forcing;`; if you wrote the impl already, it passes — that is fine for a pure-math task, but confirm the tests run).

- [ ] **Step 4: Confirm pass + gate.** `cargo test -p hornvale-astronomy forcing && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`. Expected PASS. `cargo fmt`.

- [ ] **Step 5: Commit.**

```bash
git add domains/astronomy/src/forcing.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): OrbitalForcing — the Milankovitch triad as functions of WorldTime"
```

---

### Task 2: Draw the forcing and wire it into the system

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (labels)
- Modify: `domains/astronomy/src/forcing.rs` (`generate_forcing`)
- Modify: `domains/astronomy/src/system.rs` (`StarSystem.forcing`, build it)
- Modify: `domains/astronomy/src/lib.rs` (`stream_labels()`)

**Interfaces:**
- Consumes: `hornvale_kernel::Seed`; `crate::anchor::Anchor`; `crate::pins::SkyPins`.
- Produces: `pub fn generate_forcing(astronomy_seed: Seed, anchor: &Anchor, pins: &SkyPins) -> OrbitalForcing` (moon coupling arrives in Task 3; this task draws a plain amplitude). `StarSystem` gains `pub forcing: OrbitalForcing`.

- [ ] **Step 1: Add stream labels.** In `domains/astronomy/src/streams.rs`, append:

```rust
/// Deep-time orbital-forcing draws (eccentricity + obliquity oscillation + precession).
pub const FORCING: &str = "forcing";
/// Per-body genesis phase offsets (year, day, and each moon).
pub const PHASE_OFFSETS: &str = "phase-offsets";
```

- [ ] **Step 2: Publish them.** In `domains/astronomy/src/lib.rs`, find `stream_labels()` and add the two labels with one-line docs, in the same shape as the existing entries (e.g. `(streams::FORCING, "deep-time orbital forcing"), (streams::PHASE_OFFSETS, "per-body genesis phase offsets"),`). If `stream_labels()` builds from a slice of `(streams::X, "…")` tuples, append there; keep ordering stable (append at the end).

- [ ] **Step 3: Write the failing test.** In `forcing.rs` tests, add:

```rust
    #[test]
    fn generate_forcing_is_deterministic_and_anchored() {
        use crate::anchor::generate_anchor;
        use crate::pins::SkyPins;
        use crate::star::generate_star;
        use hornvale_kernel::Seed;
        let seed = Seed(42).derive(crate::streams::ROOT);
        let star = generate_star(seed);
        let anchor = generate_anchor(seed, &star, &SkyPins::default()).unwrap();
        let a = generate_forcing(seed, &anchor, &SkyPins::default());
        let b = generate_forcing(seed, &anchor, &SkyPins::default());
        assert_eq!(a, b);
        // ε₀ carries the anchor's genesis obliquity, so obliquity_at(0) matches.
        assert_eq!(a.obliquity_mean, anchor.obliquity.get());
        assert_eq!(a.obliquity_at(0.0), anchor.obliquity.get());
    }
```

- [ ] **Step 4: Implement `generate_forcing`.** In `forcing.rs`, add (all draws from the one `FORCING` stream, in a fixed order that is now a save-format contract):

```rust
use crate::anchor::Anchor;
use crate::pins::SkyPins;
use crate::streams;
use hornvale_kernel::Seed;

/// Draw the deep-time forcing. ε₀ is the anchor's genesis obliquity; the
/// oscillation amplitudes/phases and eccentricity are drawn on the `FORCING`
/// stream in a fixed order (a save-format contract). Amplitudes stay small so
/// the present sky is barely perturbed; deep time is where they show.
pub fn generate_forcing(astronomy_seed: Seed, anchor: &Anchor, pins: &SkyPins) -> OrbitalForcing {
    let mut s = astronomy_seed.derive(streams::FORCING).stream();
    // Fixed draw order — never reorder.
    let obliquity_amp_drawn = s.next_f64() * 2.5; // 0–2.5° base wobble
    let obliquity_phase = s.next_f64() * TAU;
    let ecc_mean = s.next_f64() * 0.05; // 0–0.05, Earth ~0.017
    let ecc_amp = s.next_f64() * 0.03;
    let ecc_phase = s.next_f64() * TAU;
    let precession_phase = s.next_f64() * TAU;
    // The forcing pin zeroes the amplitudes AFTER the draws (pin isolation).
    let zeroed = matches!(pins.forcing, Some(crate::pins::ForcingPin::Zero));
    OrbitalForcing {
        obliquity_mean: anchor.obliquity.get(),
        obliquity_amp: if zeroed { 0.0 } else { obliquity_amp_drawn },
        obliquity_phase,
        ecc_mean: if zeroed { 0.0 } else { ecc_mean },
        ecc_amp: if zeroed { 0.0 } else { ecc_amp },
        ecc_phase,
        precession_phase,
    }
}
```

Note: `ForcingPin` is added in Task 6; until then this references a type that does not compile. **Reorder the plan's execution so Task 6's `ForcingPin` enum is added first, OR** temporarily gate the pin read behind a `SkyPins.forcing` field introduced here as `Option<()>`. To keep tasks independent, add the `SkyPins.forcing: Option<ForcingPin>` field and the `ForcingPin` enum in **this** task's Step 4a below, and defer only the *parsing/CLI* surface to Task 6.

- [ ] **Step 4a: Add the pin type and field now (parsing waits for Task 6).** In `domains/astronomy/src/pins.rs`, add:

```rust
/// Deep-time orbital-forcing setting. `Zero` is the null control: a circular
/// orbit with no obliquity drift (all oscillation amplitudes zero).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ForcingPin {
    /// No forcing: circular orbit, fixed obliquity (the Year-3 null control).
    Zero,
}
```

and add to `SkyPins`: `/// Deep-time orbital forcing; None = drawn. pub forcing: Option<ForcingPin>,`.

- [ ] **Step 5: Wire into the system.** In `domains/astronomy/src/system.rs`, add `pub forcing: crate::forcing::OrbitalForcing,` to `StarSystem` (with a doc line), and in `generate`, after `let neighbors = …;`, add `let forcing = crate::forcing::generate_forcing(astronomy_seed, &anchor, pins);` and include `forcing` in the `StarSystem { … }` literal.

- [ ] **Step 6: Run + gate.** `cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`. Expected PASS (existing determinism tests still green — new labels do not perturb old draws). `cargo fmt`.

- [ ] **Step 7: Commit.**

```bash
git add domains/astronomy/src/streams.rs domains/astronomy/src/forcing.rs domains/astronomy/src/system.rs domains/astronomy/src/pins.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): draw orbital forcing into the star system (forcing pin scaffold)"
```

---

### Task 3: Moon-coupled obliquity amplitude (SKY-21)

**Files:**
- Modify: `domains/astronomy/src/forcing.rs` (`generate_forcing` signature + coupling)
- Modify: `domains/astronomy/src/system.rs` (pass moons)

**Interfaces:**
- Produces: `generate_forcing(astronomy_seed, anchor, moons: &[Moon], pins) -> OrbitalForcing` — a large stabilizing moon damps the obliquity amplitude; a moonless world keeps the full drawn amplitude.

- [ ] **Step 1: Write the failing test.** In `forcing.rs` tests:

```rust
    #[test]
    fn a_large_moon_damps_the_obliquity_wobble() {
        use crate::anchor::generate_anchor;
        use crate::moons::generate_moons;
        use crate::pins::{MoonsPin, SkyPins};
        use crate::star::generate_star;
        use hornvale_kernel::Seed;
        let seed = Seed(42).derive(crate::streams::ROOT);
        let star = generate_star(seed);
        let anchor = generate_anchor(seed, &star, &SkyPins::default()).unwrap();
        let mooned = SkyPins { moons: Some(MoonsPin::exact(1).unwrap()), ..SkyPins::default() };
        let moonless = SkyPins { moons: Some(MoonsPin::exact(0).unwrap()), ..SkyPins::default() };
        let (mm, _) = generate_moons(seed, &star, &anchor, &mooned).unwrap();
        let (ml, _) = generate_moons(seed, &star, &anchor, &moonless).unwrap();
        let f_mooned = generate_forcing(seed, &anchor, &mm, &mooned);
        let f_moonless = generate_forcing(seed, &anchor, &ml, &moonless);
        assert!(
            f_mooned.obliquity_amp < f_moonless.obliquity_amp,
            "a moon must damp the obliquity amplitude ({} !< {})",
            f_mooned.obliquity_amp, f_moonless.obliquity_amp
        );
    }
```

- [ ] **Step 2: Implement the coupling.** Change `generate_forcing` to take `moons: &[crate::moons::Moon]` and scale the amplitude by a stabilization factor from the summed tide (moons already carry `tide_rel`):

```rust
    // Moon coupling (SKY-21): total tidal stabilization damps the wobble.
    let stabilization: f64 = moons.iter().map(|m| m.tide_rel).sum();
    // Damping in (0,1]: no moon → 1.0 (full wobble); strong tide → small.
    let damping = 1.0 / (1.0 + stabilization);
    let obliquity_amp_drawn = base_wobble * damping; // base_wobble = s.next_f64() * 2.5;
```

Rename the earlier `obliquity_amp_drawn` draw to `base_wobble`, then apply `damping`. The moonless world (`stabilization = 0`) keeps `base_wobble`; a mooned world shrinks it. Amplitude range stays bounded (base ≤ 2.5°, moonless; smaller with moons — Earth-like).

- [ ] **Step 3: Pass moons at the call site.** In `system.rs`, change the call to `generate_forcing(astronomy_seed, &anchor, &moons, pins)`.

- [ ] **Step 4: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/forcing.rs domains/astronomy/src/system.rs
git commit -m "feat(astronomy): couple obliquity wobble to moon presence (SKY-21)"
```

If the test FAILS because seed 42's moonless and mooned draws differ in a confound: the two `generate_forcing` calls share the same seed/anchor and draw identically, so `base_wobble` and phases are equal; only `damping` differs. A failure means the coupling is not applied — fix the code, not the test.

---

### Task 4: Per-body phase offsets (SKY-4)

**Files:**
- Modify: `domains/astronomy/src/forcing.rs` (`OrbitalForcing` gains offsets; draw them)
- Modify: `domains/astronomy/src/calendar.rs` (thread offsets into `year_phase`/`local_day`/`moon_phase`)

**Interfaces:**
- Produces: `OrbitalForcing` gains `pub year_phase_offset: f64`, `pub day_phase_offset: f64`, `pub moon_phase_offsets: Vec<f64>` (fractions in `[0,1)`); `Calendar` applies them so genesis day 0 is an ordinary day.

- [ ] **Step 1: Write the failing test.** In `calendar.rs` tests:

```rust
    #[test]
    fn genesis_day_zero_is_not_a_grand_alignment() {
        let cal = calendar_of(&spinning_system());
        // At least one of year/day/moon phase is non-zero at t=0.
        let y = cal.year_phase(StdDays::new(0.0).unwrap());
        let (_, dfrac) = cal.local_day(StdDays::new(0.0).unwrap()).unwrap();
        let m = cal.moon_phase(StdDays::new(0.0).unwrap(), 0).unwrap_or(0.0);
        assert!(y != 0.0 || dfrac != 0.0 || m != 0.0, "day 0 is still a grand alignment");
    }
```

- [ ] **Step 2: Extend `OrbitalForcing` and draw the offsets.** Add the three fields (doc-commented). In `generate_forcing`, after the existing draws, draw from the `PHASE_OFFSETS` stream:

```rust
    let mut p = astronomy_seed.derive(streams::PHASE_OFFSETS).stream();
    let year_phase_offset = p.next_f64();
    let day_phase_offset = p.next_f64();
    let moon_phase_offsets: Vec<f64> = (0..moons.len()).map(|_| p.next_f64()).collect();
```

Populate them into the returned `OrbitalForcing`. The forcing pin does **not** zero these (a phase offset is not "forcing"; it only moves day 0 off the alignment).

- [ ] **Step 3: Apply the offsets in `Calendar`.** `calendar_of` copies the offsets (store the whole `OrbitalForcing` on `Calendar` — see Task 5, which already moves the forcing onto the calendar; if doing Task 4 before Task 5, add a `forcing: OrbitalForcing` field now and populate it in `calendar_of`). Then:

```rust
    pub fn year_phase(&self, t: StdDays) -> f64 {
        (t.0 / self.year.0 + self.forcing.year_phase_offset).fract()
    }
```

and in `local_day`, add `self.forcing.day_phase_offset` before `.fract()` on the fraction; in `moon_phase`, add `self.forcing.moon_phase_offsets.get(index).copied().unwrap_or(0.0)` before `.fract()`.

- [ ] **Step 4: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/forcing.rs domains/astronomy/src/calendar.rs
git commit -m "feat(astronomy): per-body genesis phase offsets — day 0 is an ordinary day (SKY-4)"
```

If existing `calendar.rs` tests that assumed `t=0` alignment (`moon_phase_and_months_derive_from_kepler_periods` checks `moon_phase(period*1.5) == 0.5`) now fail: the offset shifts the phase, so update those tests to add the offset, or assert the *difference* over a period rather than the absolute phase. This is expected churn, not a regression — the tests encoded the grand-alignment bug.

---

### Task 5: Time-varying daylight and apsidal seasons (SKY-1/2)

**Files:**
- Modify: `domains/astronomy/src/calendar.rs`

**Interfaces:**
- Produces: `Calendar` holds `forcing: OrbitalForcing` (replacing the standalone `obliquity: Degrees` field); `daylight_fraction(t)` uses `forcing.obliquity_at(t)` and adds an eccentricity apsidal term; `season_phase` present whenever obliquity **or** eccentricity is non-zero.

- [ ] **Step 1: Write the failing tests.** In `calendar.rs` tests:

```rust
    #[test]
    fn t0_daylight_matches_the_pre_forcing_value() {
        // At t=0 the obliquity term equals ε₀ (anchored). With no eccentricity
        // the daylight at year-phase 0 is 0.5 (equinoctial), unchanged.
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        assert_eq!(cal.daylight_fraction(StdDays::new(0.0).unwrap()).unwrap(), 0.5);
    }

    #[test]
    fn zero_tilt_world_still_has_a_year_from_eccentricity() {
        // Zero obliquity, but forcing (eccentricity) present → season_phase exists.
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            ..SkyPins::default() // forcing drawn, eccentricity non-zero
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        assert!(cal.season_phase(StdDays::new(50.0).unwrap()).is_some(),
            "eccentricity gives even a zero-tilt world a year (SKY-2)");
    }
```

- [ ] **Step 2: Replace the obliquity field with the forcing.** In `Calendar`, change `obliquity: Degrees` to `forcing: crate::forcing::OrbitalForcing`. In `calendar_of`, set `forcing: system.forcing`.

- [ ] **Step 3: Rewrite the seasonal methods:**

```rust
    /// Seasonal phase; present when either driver (tilt or eccentricity) acts.
    pub fn season_phase(&self, t: StdDays) -> Option<f64> {
        let obliquity = self.forcing.obliquity_at(t.0);
        let ecc = self.forcing.eccentricity_at(t.0);
        if obliquity == 0.0 && ecc == 0.0 {
            return None;
        }
        Some(self.year_phase(t))
    }

    /// Daylight fraction: the tilt sinusoid (time-varying obliquity) plus a
    /// smaller apsidal term from eccentricity (a tilt-independent driver).
    pub fn daylight_fraction(&self, t: StdDays) -> Option<f64> {
        self.day?;
        let obliquity = self.forcing.obliquity_at(t.0);
        let ecc = self.forcing.eccentricity_at(t.0);
        let phase = self.year_phase(t);
        let tilt_term = (obliquity / 90.0) * 0.5 * (std::f64::consts::TAU * phase).sin();
        let apsidal_term = ecc * 0.5 * (std::f64::consts::TAU * phase).sin();
        Some((0.5 + tilt_term + apsidal_term).clamp(0.0, 1.0))
    }
```

(Latitude enters in Plan 2; this keeps the planet-wide signature.)

- [ ] **Step 4: Update the pre-existing tests that read fixed obliquity.** `daylight_follows_the_declared_sinusoid` and `zero_obliquity_means_no_seasons_and_flat_daylight` assumed a fixed obliquity and no eccentricity. Update: for the sinusoid test, pin `forcing: Some(ForcingPin::Zero)` so eccentricity is 0 and obliquity is fixed at ε₀ — then the old expected value holds at `t=0`. For the zero-seasons test, it must now *also* zero the forcing (`ForcingPin::Zero`) to assert `season_phase` is `None`, because eccentricity would otherwise supply a season. Adjust both to add `forcing: Some(ForcingPin::Zero)`.

- [ ] **Step 5: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/calendar.rs
git commit -m "feat(astronomy): time-varying daylight + apsidal seasons from eccentricity (SKY-1/2)"
```

---

### Task 6: The forcing pin — parsing and round-trip

**Files:**
- Modify: `domains/astronomy/src/pins.rs` (`pin_strings`, parse)

**Interfaces:**
- Consumes: the `ForcingPin` enum + `SkyPins.forcing` field (added in Task 2/Step 4a).
- Produces: `pins.forcing = Some(ForcingPin::Zero)` round-trips through `pin_strings` as `forcing=zero`, and the sky-pin parser accepts `forcing=zero`.

- [ ] **Step 1: Write the failing test.** In `pins.rs` tests (find the existing `pin_strings` / parse tests and mirror them):

```rust
    #[test]
    fn forcing_pin_round_trips() {
        let pins = SkyPins { forcing: Some(ForcingPin::Zero), ..SkyPins::default() };
        let strings = pin_strings(&pins);
        assert!(strings.iter().any(|s| s == "forcing=zero"));
    }
```

- [ ] **Step 2: Render it.** In `pin_strings`, add near the other fields:

```rust
    if let Some(ForcingPin::Zero) = pins.forcing {
        out.push("forcing=zero".to_string());
    }
```

- [ ] **Step 3: Parse it.** Find the sky-pin key/value parser (the function that turns `key=value` into `SkyPins`; grep `"obliquity"` in `pins.rs`/`lib.rs` for the match arm). Add an arm: `"forcing" => pins.forcing = Some(match value { "zero" => ForcingPin::Zero, other => return Err(GenesisError::InvalidPin { pin: "forcing".to_string(), reason: format!("unknown forcing '{other}'; expected 'zero'") }) }),`.

- [ ] **Step 4: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/pins.rs
git commit -m "feat(astronomy): forcing=zero pin — the Year-3 null control, round-tripped"
```

---

### Task 7: Commit the forcing as facts

**Files:**
- Modify: `domains/astronomy/src/facts.rs`

**Interfaces:**
- Produces: new predicate constants + commits so `why` can recount the forcing (`ecc-mean`, `obliquity-amp`, and the moon-coupling verdict).

- [ ] **Step 1: Write the failing test.** In `facts.rs` tests (mirror `genesis_commits_the_expected_facts_for_a_locked_two_moon_system`):

```rust
    #[test]
    fn genesis_commits_the_forcing_parameters() {
        // Build a world through the normal genesis path and assert the facts.
        // (Follow the existing test's world-construction helper.)
        let world = /* existing helper that runs record_facts */;
        let subject = /* the anchor/system subject entity used by facts.rs */;
        assert!(world.ledger.number_of(subject, ECCENTRICITY_MEAN).is_some());
        assert!(world.ledger.number_of(subject, OBLIQUITY_AMPLITUDE).is_some());
    }
```

Use whatever accessor the existing facts tests use to read a committed `Value::Number` (the file already reads facts back — mirror it exactly).

- [ ] **Step 2: Add predicate constants + registration + commits.** Add:

```rust
/// Mean orbital eccentricity (deep-time forcing).
pub const ECCENTRICITY_MEAN: &str = "eccentricity-mean";
/// Obliquity oscillation amplitude, degrees (moon-coupled).
pub const OBLIQUITY_AMPLITUDE: &str = "obliquity-amplitude";
```

Register them where the other astronomy predicates register (find `register_predicate` for `OBLIQUITY_DEGREES` and mirror), and commit them in the same block that commits `OBLIQUITY_DEGREES`:

```rust
    world.ledger.commit(fact(subject, ECCENTRICITY_MEAN, Value::Number(system.forcing.ecc_mean)), &world.registry).unwrap();
    world.ledger.commit(fact(subject, OBLIQUITY_AMPLITUDE, Value::Number(system.forcing.obliquity_amp)), &world.registry).unwrap();
```

- [ ] **Step 3: Run + gate + commit.**

```bash
cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
cargo fmt
git add domains/astronomy/src/facts.rs
git commit -m "feat(astronomy): commit orbital-forcing facts so why recounts them"
```

---

### Task 8: Property battery + the moon-coupling calibration

**Files:**
- Modify: `domains/astronomy/tests/genesis_properties.rs`
- Modify: `windows/lab/src/metrics.rs`
- Modify: `windows/lab/tests/calibration.rs`

**Interfaces:**
- Consumes: `hornvale_astronomy::forcing::OrbitalForcing`, `hornvale_astronomy::generate`, the Lab's world-build.
- Produces: property tests (formula, `t=0` anchor, pin isolation) + an `obliquity-range` metric + the range ⇔ moon-presence calibration (the sixth in the family).

- [ ] **Step 1: Property battery.** In `genesis_properties.rs`, add tests over a seed sweep (mirror the file's existing `for seed in 0..N` style):
  - `obliquity_at(0) == system.anchor.obliquity.get()` for every seed (drift-anchor identity).
  - Forcing draws are pin-isolated: a world built with `forcing=zero` consumes the same draws as the default — assert the *non-forcing* parts of the system (star, orbit, moons, neighbors, obliquity ε₀, phase offsets) are byte-identical between `SkyPins::default()` and `SkyPins { forcing: Some(ForcingPin::Zero), .. }`, and only the forcing amplitudes differ (zeroed). This is the `normal_pin_matches_the_unpinned_draw` pattern applied to forcing.
  - `forcing=zero` yields `ecc_mean == 0 && obliquity_amp == 0 && ecc_amp == 0`.

```rust
#[test]
fn forcing_is_pin_isolated() {
    for seed in 0..64u64 {
        let base = hornvale_astronomy::generate(Seed(seed), &SkyPins::default()).unwrap().system;
        let zeroed = hornvale_astronomy::generate(
            Seed(seed),
            &SkyPins { forcing: Some(ForcingPin::Zero), ..SkyPins::default() },
        ).unwrap().system;
        assert_eq!(base.star, zeroed.star, "seed {seed}: star drifted under the pin");
        assert_eq!(base.anchor, zeroed.anchor, "seed {seed}: anchor drifted");
        assert_eq!(base.moons, zeroed.moons, "seed {seed}: moons drifted");
        assert_eq!(base.forcing.obliquity_phase, zeroed.forcing.obliquity_phase, "phase must be drawn identically");
        assert_eq!(zeroed.forcing.obliquity_amp, 0.0);
        assert_eq!(zeroed.forcing.ecc_mean, 0.0);
    }
}
```

- [ ] **Step 2: The obliquity-range metric.** In `windows/lab/src/metrics.rs`, add a `Numeric` metric `obliquity-range` = the peak-to-peak obliquity swing over one obliquity period = `2.0 * forcing.obliquity_amp` (reconstruct the sky provider from the world as the other astronomy metrics do). Update the metric-count test to the new total (find the current count, add 1).

- [ ] **Step 3: The calibration.** In `windows/lab/tests/calibration.rs`, add (mirroring the existing calibrations that load `census-lands-drift.study.json`):

```rust
#[test]
fn obliquity_range_is_wider_on_moonless_worlds() {
    // A world with zero moons keeps the full wobble; any moon damps it.
    // Assert: moonless worlds' obliquity-range strictly exceeds the max
    // obliquity-range among worlds with >=1 moon (at equal base draw this is
    // exact; across seeds it holds as a population claim — assert the means).
    // Load the census, split by moon-count, compare mean obliquity-range.
}
```

Implement the split-and-compare against the study rows (the file already reads `moon-count` and numeric columns — reuse that machinery). If the strict population claim is too strong across seeds, weaken to *mean* moonless range > *mean* mooned range and document why in a comment; do **not** delete the calibration.

- [ ] **Step 4: Run the full gate + commit.**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/astronomy/tests/genesis_properties.rs windows/lab/src/metrics.rs windows/lab/tests/calibration.rs
git commit -m "test(astronomy,lab): forcing property battery + obliquity-range⇔moon calibration"
```

Do **not** regenerate committed artifacts here — the re-baseline is Plan 3 (one re-baseline for both halves). The Lab metric addition will drift `census-lands-drift`; that drift is expected and lands in Plan 3.

---

## Self-Review Notes

**Spec coverage (against `2026-07-09-firm-ground-ii-design.md` §3, §5, §7, §8):**
- §3 the moving sky (obliquity/eccentricity/precession as `element_at(t)`, moon coupling, phase offsets, apsidal seasons) → Tasks 1–5. §3 facts → Task 7. §5 the forcing pin with zero=null-control → Tasks 2/Step 4a + 6. §8 property battery + the sixth calibration → Task 8.
- **Deferred to Plan 2:** the observer (`GeoCoord`, latitude daylight, longitude culling). **Deferred to Plan 3:** the re-baseline and the tier-2 book chapter. This plan deliberately leaves `census-lands-drift` drifted (Task 8) and does not touch `book/`.

**Ordering hazard:** `ForcingPin` is referenced by `generate_forcing` (Task 2) before the pin's *parsing* exists (Task 6). Resolved by Task 2/Step 4a adding the enum + `SkyPins` field up front; Task 6 only adds `pin_strings`/parse. Verify the enum lands in Task 2, not Task 6.

**Determinism watch:** new labels (`FORCING`, `PHASE_OFFSETS`) derive independent sub-streams, so existing draws are untouched — the `forcing_is_pin_isolated` and existing `generate_is_deterministic` tests are the guard. The fixed draw order inside the `FORCING` stream is a permanent save-format contract; never reorder it.

**Type consistency:** `OrbitalForcing` fields and `obliquity_at/eccentricity_at/precession_at` (Task 1) are read verbatim by `Calendar` (Task 5), the facts (Task 7), and the metric (Task 8). `generate_forcing` gains its `moons` parameter in Task 3 — the Task 2 call site is updated there.

**Pre-existing test churn (expected, not regressions):** the grand-alignment assumption in `moon_phase_and_months_derive_from_kepler_periods` (Task 4) and the fixed-obliquity assumptions in `daylight_follows_the_declared_sinusoid` / `zero_obliquity_means_no_seasons_and_flat_daylight` (Task 5) are updated in-place to add the phase offset / `ForcingPin::Zero`. These tests encoded the very bugs this plan fixes.
```
