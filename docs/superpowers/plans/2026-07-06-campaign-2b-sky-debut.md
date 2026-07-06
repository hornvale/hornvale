# Campaign 2b: The Sky's Debut — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire the 2a generator into worlds: typed quantities, graded moon pins with genesis-note refusal records, the calendar layer, the `GeneratedSky` provider, pins as CLI flags and ledger facts, the `scout` verb, and the rotation-flip / moons-flip exit demo.

**Architecture:** The typed-quantities retrofit (spec §2.5) lands FIRST, proven behavior-free by a golden-value test captured before the refactor. Then generator-side upgrades (graded pins, notes), then the calendar and provider inside `hornvale-astronomy`, then the composition root (`cli/src/world_builder.rs`) grows sky selection, fact persistence, and provider reconstruction from saved pins. Worlds carry their experimental configuration in their own ledgers; providers are stateless and rebuilt deterministically at load.

**Tech Stack:** Rust edition 2024. No new dependencies. No dimensional-analysis crates.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-campaign-2-the-sky-design.md` (as amended 2026-07-06: §2.5 typed quantities, §4 graded pins/notes/scout, §9 rotation-flip demo). Constitution and CLAUDE.md bind.
- **Typed quantities (§2.5):** units are hand-rolled newtypes with validating constructors; fields are `pub(crate)` so in-crate formulas use `.0`; external access via `get()`. Dimensionless ratios (salience, `tide_rel`, `angular_diameter_rel`, apparent brightness) stay bare `f64`.
- **Determinism unchanged:** the 2a draw code and stream-consumption order must not change — the golden test (Task 2) and the existing 128-seed battery prove the retrofit and all additions are draw-identical. Time points may be zero: `StdDays`/`LocalDays` validate non-negative; durations that must be positive are guaranteed by construction (Kepler, the 4–100h pin range).
- **The seed is identity:** generation never retries across seeds. `scout` is the only cross-seed search and is explicit.
- **Genesis notes:** every degradation commits a genesis-note fact; refusals are never ephemeral stderr.
- **Committed-artifact stability until Task 10:** the seed-42 constant-sun almanac must stay byte-identical through Tasks 1–9 (Task 6 regenerates the concepts dump — expected; Task 7 adds `--sky constant` to CI's seed-42 line in the same commit that changes the CLI default). CI's drift check is the referee.
- Kernel-only deps for domains; every commit passes `cargo test --workspace`, `cargo fmt --check` (run `cargo fmt` as the FINAL pre-commit step, paste raw output in reports), `cargo clippy --workspace --all-targets -- -D warnings`; `#![warn(missing_docs)]` everywhere; docs on all public items including fields/variants.

## File Structure

```
domains/astronomy/src/units.rs        — CREATE: quantity newtypes + UnitError (T1)
domains/astronomy/src/{star,anchor,moons,neighborhood,system}.rs — MODIFY: retrofit (T2)
domains/astronomy/src/pins.rs         — MODIFY: Degrees/LocalDays pins (T2); MoonsPin (T3)
domains/astronomy/src/system.rs       — MODIFY: GenesisOutcome (T3)
domains/astronomy/tests/golden_seed_42.rs — CREATE (T2)
domains/astronomy/src/calendar.rs     — CREATE (T4)
domains/astronomy/src/provider.rs     — CREATE: GeneratedSky (T5)
domains/astronomy/src/facts.rs        — CREATE: genesis facts (T6)
domains/astronomy/src/pins.rs         — MODIFY: pin_strings/parse_pin (T6)
domains/astronomy/src/lib.rs          — MODIFY: modules/re-exports/register_concepts/stream_labels (T1–T6)
cli/src/world_builder.rs              — MODIFY: SkyChoice, build_world, sky_of, calendar_lines (T6, T8)
cli/src/main.rs                       — MODIFY: pin flags, --sky, scout (T7)
cli/src/repl.rs                       — MODIFY: calendar command (T8)
windows/almanac/src/lib.rs            — MODIFY: Calendar/night-sky/notes sections (T8)
cli/tests/sky_exit_criterion.rs       — CREATE (T9)
book/src/gallery/{the-sky.md,almanac-seed-42-sky.md,almanac-seed-42-locked.md} — CREATE (T10)
book/src/SUMMARY.md, .github/workflows/ci.yml — MODIFY (T7, T10)
book/src/reference/concept-registry-generated.md — REGENERATED (T6)
```

---

### Task 1: Typed quantities

**Files:**
- Create: `domains/astronomy/src/units.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Produces: `SolarMasses`, `SolarLuminosities`, `EarthMasses`, `LunarMasses`, `Au`, `Mm`, `LightYears`, `StdDays`, `LocalDays`, `Degrees` — each `#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]` with a `pub(crate)` inner `f64`, `new(f64) -> Result<Self, UnitError>`, `get(self) -> f64`. `StdDays::from_hours(f64) -> Result<StdDays, UnitError>`; `StdDays::in_local(self, day_length: StdDays) -> LocalDays`; `LocalDays::in_std(self, day_length: StdDays) -> StdDays`. `UnitError { unit: &'static str, value: f64, reason: &'static str }` with `Display` + `Error`.
- Validation: all require finite; masses, luminosities, and distances require `> 0`; `StdDays`/`LocalDays` require `>= 0` (time *points* may be zero); `Degrees` requires `0.0 <= v < 360.0`.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/units.rs`:

```rust
//! Typed quantities (spec §2.5): coherent units as validating newtypes so
//! invalid states are unrepresentable at API boundaries. In-crate formulas
//! use the pub(crate) inner field; dimensionless ratios stay bare f64.

use std::fmt;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructors_accept_valid_and_reject_invalid() {
        assert!(SolarMasses::new(1.0).is_ok());
        assert!(SolarMasses::new(0.0).is_err());
        assert!(SolarMasses::new(-1.0).is_err());
        assert!(SolarMasses::new(f64::NAN).is_err());
        assert!(Au::new(1.0).is_ok());
        assert!(Au::new(f64::INFINITY).is_err());
        assert!(StdDays::new(0.0).is_ok(), "time points may be zero");
        assert!(StdDays::new(-0.5).is_err());
        assert!(LocalDays::new(0.0).is_ok());
        assert!(Degrees::new(0.0).is_ok());
        assert!(Degrees::new(359.9).is_ok());
        assert!(Degrees::new(360.0).is_err());
        assert!(Degrees::new(-1.0).is_err());
    }

    #[test]
    fn errors_name_the_unit_and_reason() {
        let e = SolarMasses::new(-2.0).unwrap_err();
        let text = e.to_string();
        assert!(text.contains("solar masses"));
        assert!(text.contains("-2"));
    }

    #[test]
    fn hours_and_local_conversions_round_trip() {
        let day = StdDays::from_hours(24.0).unwrap();
        assert_eq!(day.get(), 1.0);
        let day30 = StdDays::from_hours(30.0).unwrap();
        let local = StdDays::new(2.5).unwrap().in_local(day30);
        assert_eq!(local.get(), 2.0);
        assert_eq!(local.in_std(day30).get(), 2.5);
    }
}
```

- [ ] **Step 2: Verify failure**

Add to `domains/astronomy/src/lib.rs`: `pub mod units;` and
`pub use units::{Au, Degrees, EarthMasses, LightYears, LocalDays, LunarMasses, Mm, SolarLuminosities, SolarMasses, StdDays, UnitError};`

Run: `cargo test -p hornvale-astronomy units`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/units.rs`:

```rust
/// Why a quantity constructor refused a value.
#[derive(Debug, Clone, PartialEq)]
pub struct UnitError {
    /// Human name of the unit ("solar masses").
    pub unit: &'static str,
    /// The rejected value.
    pub value: f64,
    /// The rule it violates.
    pub reason: &'static str,
}

impl fmt::Display for UnitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is not a valid quantity of {}: {}", self.value, self.unit, self.reason)
    }
}

impl std::error::Error for UnitError {}

macro_rules! quantity {
    ($name:ident, $label:literal, positive, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        pub struct $name(pub(crate) f64);
        impl $name {
            /// Validating constructor: finite and strictly positive.
            pub fn new(value: f64) -> Result<Self, UnitError> {
                if !value.is_finite() {
                    return Err(UnitError { unit: $label, value, reason: "must be finite" });
                }
                if value <= 0.0 {
                    return Err(UnitError { unit: $label, value, reason: "must be positive" });
                }
                Ok(Self(value))
            }
            /// The raw value.
            pub fn get(self) -> f64 {
                self.0
            }
        }
    };
    ($name:ident, $label:literal, non_negative, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        pub struct $name(pub(crate) f64);
        impl $name {
            /// Validating constructor: finite and non-negative (time points may be zero).
            pub fn new(value: f64) -> Result<Self, UnitError> {
                if !value.is_finite() {
                    return Err(UnitError { unit: $label, value, reason: "must be finite" });
                }
                if value < 0.0 {
                    return Err(UnitError { unit: $label, value, reason: "must be non-negative" });
                }
                Ok(Self(value))
            }
            /// The raw value.
            pub fn get(self) -> f64 {
                self.0
            }
        }
    };
}

quantity!(SolarMasses, "solar masses", positive, "Stellar mass in solar masses.");
quantity!(SolarLuminosities, "solar luminosities", positive, "Luminosity in solar units.");
quantity!(EarthMasses, "Earth masses", positive, "Planetary mass in Earth masses.");
quantity!(LunarMasses, "lunar masses", positive, "Moon mass in lunar masses (Luna = 1).");
quantity!(Au, "astronomical units", positive, "Orbital distance in AU.");
quantity!(Mm, "megameters", positive, "Distance in Mm (1000 km; Luna orbits at 384.4).");
quantity!(LightYears, "light-years", positive, "Interstellar distance in light-years.");
quantity!(StdDays, "standard days", non_negative, "Absolute time or duration in standard days.");
quantity!(LocalDays, "local days", non_negative, "Time or duration in a world's own days.");

/// An angle in degrees, valid in [0, 360).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Degrees(pub(crate) f64);

impl Degrees {
    /// Validating constructor: finite, 0 <= v < 360.
    pub fn new(value: f64) -> Result<Self, UnitError> {
        if !value.is_finite() {
            return Err(UnitError { unit: "degrees", value, reason: "must be finite" });
        }
        if !(0.0..360.0).contains(&value) {
            return Err(UnitError { unit: "degrees", value, reason: "must be in [0, 360)" });
        }
        Ok(Self(value))
    }
    /// The raw value.
    pub fn get(self) -> f64 {
        self.0
    }
}

impl StdDays {
    /// A duration given in standard hours.
    pub fn from_hours(hours: f64) -> Result<StdDays, UnitError> {
        StdDays::new(hours / 24.0)
    }
    /// Express this absolute time/duration in a world's own days.
    pub fn in_local(self, day_length: StdDays) -> LocalDays {
        LocalDays(self.0 / day_length.0)
    }
}

impl LocalDays {
    /// Express local days back in standard days.
    pub fn in_std(self, day_length: StdDays) -> StdDays {
        StdDays(self.0 * day_length.0)
    }
}
```

- [ ] **Step 4: Gate, commit**

Run the full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): typed quantities — validating unit newtypes"
```

---

### Task 2: Retrofit the generator to typed quantities

**Files:**
- Create: `domains/astronomy/tests/golden_seed_42.rs`
- Modify: `domains/astronomy/src/{star,anchor,moons,neighborhood,system,pins}.rs` and their tests

**Interfaces:**
- Produces the retrofitted shapes every later task consumes (unit suffixes move from field names into types):
  - `Star { mass: SolarMasses, luminosity: SolarLuminosities, class_name: String, habitable_zone: (Au, Au) }`
  - `Anchor { mass: EarthMasses, orbit: Au, year: StdDays, rotation: Rotation, obliquity: Degrees }`; `Rotation::Spinning { day: StdDays } | Locked`
  - `Moon { mass: LunarMasses, distance: Mm, period: StdDays, angular_diameter_rel: f64, tide_rel: f64 }`
  - `Neighbor { class: NeighborClass, distance: LightYears, apparent_brightness: f64, color: String }`
  - `SkyPins { moons: Option<u32>, rotation: Option<RotationPin>, obliquity: Option<Degrees>, year_local_days: Option<LocalDays>, neighbor: Option<NeighborClass> }` (field renames: `obliquity_deg` → `obliquity`; values now typed; `RotationPin::PeriodHours(f64)` unchanged — hours validated at use).
  - `hill_radius_mm(star: &Star, anchor: &Anchor) -> f64` keeps its name and bare-f64 return (an internal Mm magnitude consumed by tests).
- **The math must not change.** Every formula keeps its exact arithmetic; only wrap/unwrap points move. Construction inside the crate uses the `pub(crate)` tuple field directly (`SolarMasses(mass)`) — values are positive by construction; validating constructors are for boundaries.

- [ ] **Step 1: Capture golden values BEFORE touching anything**

Add a temporary test to `domains/astronomy/tests/golden_seed_42.rs`:

```rust
//! Golden values for seed 42's default system: captured before the typed-
//! quantities retrofit, asserted after. Exact f64 equality — determinism
//! is constitutional.

use hornvale_astronomy::{generate, SkyPins};
use hornvale_kernel::Seed;

#[test]
fn print_golden() {
    let s = generate(Seed(42), &SkyPins::default()).unwrap();
    println!("GOLDEN star.mass={:?}", s.star.mass_solar);
    println!("GOLDEN star.lum={:?}", s.star.luminosity_solar);
    println!("GOLDEN anchor.orbit={:?}", s.anchor.orbit_au);
    println!("GOLDEN anchor.year={:?}", s.anchor.year_std_days);
    println!("GOLDEN anchor.rotation={:?}", s.anchor.rotation);
    println!("GOLDEN anchor.obliquity={:?}", s.anchor.obliquity_deg);
    println!("GOLDEN moons={:?}", s.moons.iter().map(|m| (m.distance_mm, m.period_std_days)).collect::<Vec<_>>());
    println!("GOLDEN neighbors.len={} first.distance={:?}", s.neighbors.len(), s.neighbors[0].distance_ly);
}
```

Run: `cargo test -p hornvale-astronomy --test golden_seed_42 -- --nocapture`
Copy the printed values verbatim into a real test replacing `print_golden`:

```rust
#[test]
fn seed_42_default_system_matches_the_pre_retrofit_golden_values() {
    let s = generate(Seed(42), &SkyPins::default()).unwrap().system;
    // Paste the captured values below EXACTLY (full f64 precision as printed).
    assert_eq!(s.star.mass.get(), /* GOLDEN star.mass */);
    assert_eq!(s.star.luminosity.get(), /* GOLDEN star.lum */);
    assert_eq!(s.anchor.orbit.get(), /* GOLDEN anchor.orbit */);
    assert_eq!(s.anchor.year.get(), /* GOLDEN anchor.year */);
    assert_eq!(s.anchor.obliquity.get(), /* GOLDEN anchor.obliquity */);
    // ...one assert per printed moon distance/period and the neighbor values.
}
```

(While capturing, the accessors are the OLD field names and `generate` returns
the bare system; after Tasks 2–3 the test reads `.system` and typed getters —
write it in the post-retrofit form with the captured numbers, and leave it
failing-to-compile until Step 3 completes. The numbers are the contract.)

- [ ] **Step 2: Retrofit types**

Apply the Interfaces shapes above. Mechanical rules:
- Field declarations change to the typed forms; all doc comments keep their
  unit phrasing ("in solar masses" etc. — now enforced by the type).
- Inside generation code, wrap at the point of creation:
  `SolarMasses(0.6 + stream.next_f64() * 0.8)`,
  `SolarLuminosities(mass.0.powf(3.5))`, `Au(inner.0 + stream... * (outer.0 - inner.0))`,
  `StdDays(365.25 * (orbit.0.powi(3) / star.mass.0).sqrt())`,
  `Rotation::Spinning { day: StdDays((16.0 + stream.next_f64() * 24.0) / 24.0) }`,
  `Degrees(stream.next_f64() * 35.0)`, `LunarMasses(mass)`, `Mm(distance)`,
  `LightYears(4.0 + stream.next_f64() * 76.0)` — the arithmetic inside is
  UNCHANGED, character for character.
- Formula bodies read `.0` on typed inputs (e.g. Kepler:
  `365.25 * (a.0.powi(3) / m.0).sqrt()`); `hill_radius_mm` reads
  `anchor.orbit.0`, `anchor.mass.0`, `star.mass.0`.
- Pins: `obliquity: Option<Degrees>` — the anchor's pin arm becomes
  `Some(deg) => deg` (range validation moves to `Degrees::new` at the CLI
  boundary later, but the anchor KEEPS its 0–35 obliquity check as an
  `InvalidPin` since Degrees permits up to 360):
  `if !(0.0..=35.0).contains(&deg.get()) { return Err(InvalidPin ...) }`.
  `year_local_days: Option<LocalDays>` — the non-positive guard from the 2a
  fix simplifies: `LocalDays` already rejects negatives/NaN at construction,
  but keep the `local_days.get() <= 0.0` zero check as `InvalidPin` (zero is
  a valid time POINT but not a valid year), with a comment saying so.
- Update every existing unit test and the 128-seed battery mechanically
  (`.get()` where values are compared; `Rotation::Spinning { day }` patterns;
  `SkyPins { obliquity: Some(Degrees::new(0.0).unwrap()), .. }` etc.).

- [ ] **Step 3: Prove it**

Run: `cargo test -p hornvale-astronomy` — everything green INCLUDING the
golden test (exact equality with pre-refactor values) and the battery.
Then `cargo test --workspace`.

- [ ] **Step 4: Gate, commit**

```bash
git add domains/astronomy/
git commit -m "refactor(astronomy): typed quantities throughout the generator — golden-proven draw-identical"
```

---

### Task 3: Graded moon pins and genesis notes

**Files:**
- Modify: `domains/astronomy/src/pins.rs` (MoonsPin), `moons.rs` (notes + min/want), `system.rs` (GenesisOutcome), battery + golden test call sites, `lib.rs` re-exports

**Interfaces:**
- Produces:
  - `MoonsPin` (private fields, `Copy`): `MoonsPin::exact(n: u32) -> Result<MoonsPin, GenesisError>` (n ≤ 3), `MoonsPin::graded(min: u32, extra: u32) -> Result<MoonsPin, GenesisError>` (min+extra ≤ 3), `min(&self) -> u32`, `want(&self) -> u32`. `SkyPins.moons: Option<MoonsPin>`.
  - `GenesisOutcome { pub system: StarSystem, pub notes: Vec<String> }`; `generate(world_seed, &pins) -> Result<GenesisOutcome, GenesisError>`.
  - `generate_moons(...) -> Result<(Vec<Moon>, Vec<String>), GenesisError>`.
- Semantics (spec §4 amendment): target = `want` (or drawn count); exhaustion at moon k: if admitted-so-far < `min` → `UnsatisfiablePin` (unchanged message); else stop and push the note, verbatim format:
  `format!("moon {} of {} was sought; no stable orbit exists within the Hill sphere, spacing, and tide budget", k, target)`.
  Drawn counts behave as min = 0 (every degradation is noted — seed 10 gains its note). Stream consumption is untouched.

- [ ] **Step 1: Write the failing tests**

Add to `moons.rs` tests:

```rust
    #[test]
    fn graded_pin_degrades_with_a_note_above_min() {
        // Seed 10 cannot hold a third moon; graded 2+1 accepts 2 + a note.
        let (star, anchor) = system(10);
        let pins = SkyPins {
            moons: Some(MoonsPin::graded(2, 1).unwrap()),
            ..SkyPins::default()
        };
        let (moons, notes) = generate_moons(Seed(10), &star, &anchor, &pins).unwrap();
        assert_eq!(moons.len(), 2);
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("moon 3 of 3 was sought"));
    }

    #[test]
    fn graded_pin_fails_loudly_below_min() {
        let (star, anchor) = system(10);
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(3).unwrap()),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_moons(Seed(10), &star, &anchor, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn drawn_degradation_is_noted() {
        let (star, anchor) = system(10);
        let (moons, notes) = generate_moons(Seed(10), &star, &anchor, &SkyPins::default()).unwrap();
        assert!(moons.len() < 3);
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("was sought"));
    }

    #[test]
    fn moons_pin_constructors_validate() {
        assert!(MoonsPin::exact(3).is_ok());
        assert!(MoonsPin::exact(4).is_err());
        assert!(MoonsPin::graded(2, 1).is_ok());
        assert!(MoonsPin::graded(2, 2).is_err());
        let p = MoonsPin::graded(1, 2).unwrap();
        assert_eq!((p.min(), p.want()), (1, 3));
    }
```

- [ ] **Step 2: Verify failure, implement**

`pins.rs` additions:

```rust
/// A graded moon request: `min` essential, `want` desired (spec §4).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MoonsPin {
    min: u32,
    want: u32,
}

impl MoonsPin {
    /// Exactly `n` moons: loud failure below n.
    pub fn exact(n: u32) -> Result<MoonsPin, GenesisError> {
        MoonsPin::graded(n, 0)
    }
    /// `min` essential plus up to `extra` desired.
    pub fn graded(min: u32, extra: u32) -> Result<MoonsPin, GenesisError> {
        let want = min + extra;
        if want > 3 {
            return Err(GenesisError::InvalidPin {
                pin: "moons".to_string(),
                reason: format!("{min}+{extra} moons requested; the legal maximum is 3"),
            });
        }
        Ok(MoonsPin { min, want })
    }
    /// The essential count.
    pub fn min(&self) -> u32 {
        self.min
    }
    /// The desired count.
    pub fn want(&self) -> u32 {
        self.want
    }
}
```

`SkyPins.moons` becomes `Option<MoonsPin>` (doc: "Moon request: exact or
graded; None = drawn."). `moons.rs`: `(count, min)` = pinned
`(p.want(), p.min())` or drawn `(weighted_draw, 0)` — the draw code
unchanged. Exhaustion branch:

```rust
        if !admitted {
            let sought = index + 1;
            if (moons.len() as u32) < min {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "moons".to_string(),
                    reason: format!(
                        "moon {sought} of {count} found no stable orbit within the attempt budget \
                         (Hill radius {hill:.0} Mm, tide cap {TIDE_CAP})"
                    ),
                });
            }
            notes.push(format!(
                "moon {sought} of {count} was sought; no stable orbit exists within the \
                 Hill sphere, spacing, and tide budget"
            ));
            break;
        }
```

`system.rs`:

```rust
/// A generated system plus the notes genesis recorded along the way.
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisOutcome {
    /// The system itself.
    pub system: StarSystem,
    /// Human-readable degradation records (become genesis-note facts).
    pub notes: Vec<String>,
}
```

`generate` returns `Result<GenesisOutcome, GenesisError>`. Update the
battery, golden test, and every call site: pin-matrix uses
`MoonsPin::exact(n).unwrap()`; the pin-vs-drawn isolation test pins
`MoonsPin::exact(drawn_len as u32)` and compares `.system`; the old
`drawn_counts_degrade...` test now also asserts the note.

- [ ] **Step 3: Gate, commit**

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): graded moon pins (min/want) and genesis notes"
```

---

### Task 4: The calendar layer

**Files:**
- Create: `domains/astronomy/src/calendar.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs`

**Interfaces:**
- Produces: `Calendar` and `calendar_of(system: &StarSystem) -> Calendar` with methods (`t` is absolute time as `StdDays`):
  - `day_length(&self) -> Option<StdDays>` (None = tidally locked)
  - `year_length(&self) -> StdDays`
  - `local_day(&self, t) -> Option<(u64, f64)>` — day index + fraction
  - `year_phase(&self, t) -> f64` — `(t/year).fract()`
  - `season_phase(&self, t) -> Option<f64>` — None when obliquity is 0
  - `daylight_fraction(&self, t) -> Option<f64>` — None when locked; else exactly `0.5 + (obliquity/90) * 0.5 * sin(TAU * year_phase)` (the model card's declared sinusoid; 0.5 flat at obliquity 0)
  - `is_daylight(&self, t) -> Option<bool>` — day fraction within the centered window `((1-f)/2, (1+f)/2)`
  - `moon_phase(&self, t, index) -> Option<f64>` — `(t/period).fract()`
  - `months_per_year(&self, index) -> Option<f64>` — `year/period`
- All pure; all deterministic; no wall-clock.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/calendar.rs`:

```rust
//! The calendar layer: translate absolute standard days into a world's own
//! cycles (spec §6, two clocks). Worlds without a cycle have calendars
//! without that column — truthfully.

use crate::anchor::Rotation;
use crate::system::StarSystem;
use crate::units::{Degrees, StdDays};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use hornvale_kernel::Seed;

    fn spinning_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().system
    }

    fn locked_system() -> StarSystem {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        generate(Seed(42), &pins).unwrap().system
    }

    #[test]
    fn local_days_advance_with_absolute_time() {
        let cal = calendar_of(&spinning_system());
        assert_eq!(cal.day_length().unwrap().get(), 1.0);
        let (index, fraction) = cal.local_day(StdDays::new(2.25).unwrap()).unwrap();
        assert_eq!(index, 2);
        assert!((fraction - 0.25).abs() < 1e-12);
    }

    #[test]
    fn locked_worlds_have_no_local_day_and_no_daylight_cycle() {
        let cal = calendar_of(&locked_system());
        assert!(cal.day_length().is_none());
        assert!(cal.local_day(StdDays::new(5.0).unwrap()).is_none());
        assert!(cal.daylight_fraction(StdDays::new(5.0).unwrap()).is_none());
        assert!(cal.is_daylight(StdDays::new(5.0).unwrap()).is_none());
    }

    #[test]
    fn daylight_follows_the_declared_sinusoid() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let year = cal.year_length().get();
        let obliquity = system.anchor.obliquity.get();
        // Midsummer (year phase 0.25): maximum daylight.
        let t = StdDays::new(0.25 * year).unwrap();
        let expected = 0.5 + (obliquity / 90.0) * 0.5;
        assert!((cal.daylight_fraction(t).unwrap() - expected).abs() < 1e-9);
    }

    #[test]
    fn zero_obliquity_means_no_seasons_and_flat_daylight() {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let t = StdDays::new(100.0).unwrap();
        assert!(cal.season_phase(t).is_none());
        assert_eq!(cal.daylight_fraction(t).unwrap(), 0.5);
    }

    #[test]
    fn moon_phase_and_months_derive_from_kepler_periods() {
        let system = spinning_system();
        let cal = calendar_of(&system);
        let period = system.moons[0].period.get();
        let t = StdDays::new(period * 1.5).unwrap();
        assert!((cal.moon_phase(t, 0).unwrap() - 0.5).abs() < 1e-9);
        let months = cal.months_per_year(0).unwrap();
        assert!((months - cal.year_length().get() / period).abs() < 1e-12);
        assert!(cal.moon_phase(t, 5).is_none());
    }
}
```

- [ ] **Step 2: Verify failure, implement**

Add `pub mod calendar;` + `pub use calendar::{calendar_of, Calendar};` to lib.rs.

```rust
/// A world's cycles, derived once from its star system.
#[derive(Debug, Clone, PartialEq)]
pub struct Calendar {
    day: Option<StdDays>,
    year: StdDays,
    obliquity: Degrees,
    moon_periods: Vec<StdDays>,
}

/// Derive the calendar from a generated system.
pub fn calendar_of(system: &StarSystem) -> Calendar {
    let day = match system.anchor.rotation {
        Rotation::Spinning { day } => Some(day),
        Rotation::Locked => None,
    };
    Calendar {
        day,
        year: system.anchor.year,
        obliquity: system.anchor.obliquity,
        moon_periods: system.moons.iter().map(|m| m.period).collect(),
    }
}

impl Calendar {
    /// Length of one local day, if the world has one.
    pub fn day_length(&self) -> Option<StdDays> {
        self.day
    }
    /// Length of the year in standard days.
    pub fn year_length(&self) -> StdDays {
        self.year
    }
    /// Local day index and fraction at absolute time `t`.
    pub fn local_day(&self, t: StdDays) -> Option<(u64, f64)> {
        let day = self.day?;
        let local = t.0 / day.0;
        Some((local as u64, local.fract()))
    }
    /// Fraction of the year elapsed at `t`.
    pub fn year_phase(&self, t: StdDays) -> f64 {
        (t.0 / self.year.0).fract()
    }
    /// Seasonal phase, absent on a world without axial tilt.
    pub fn season_phase(&self, t: StdDays) -> Option<f64> {
        if self.obliquity.0 == 0.0 {
            return None;
        }
        Some(self.year_phase(t))
    }
    /// Daylight fraction of the local day: the model card's sinusoid.
    /// Absent on a tidally locked world.
    pub fn daylight_fraction(&self, t: StdDays) -> Option<f64> {
        self.day?;
        Some(0.5 + (self.obliquity.0 / 90.0) * 0.5 * (std::f64::consts::TAU * self.year_phase(t)).sin())
    }
    /// Is it daylight at `t`? Daylight is a centered window of the local day.
    pub fn is_daylight(&self, t: StdDays) -> Option<bool> {
        let fraction = self.local_day(t)?.1;
        let f = self.daylight_fraction(t)?;
        Some(fraction > (1.0 - f) / 2.0 && fraction < (1.0 + f) / 2.0)
    }
    /// Phase of moon `index` at `t`, if that moon exists.
    pub fn moon_phase(&self, t: StdDays, index: usize) -> Option<f64> {
        let period = self.moon_periods.get(index)?;
        Some((t.0 / period.0).fract())
    }
    /// How many of moon `index`'s cycles fit in a year.
    pub fn months_per_year(&self, index: usize) -> Option<f64> {
        let period = self.moon_periods.get(index)?;
        Some(self.year.0 / period.0)
    }
}
```

- [ ] **Step 3: Gate, commit**

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): the calendar layer — two clocks, truthful columns"
```

---

### Task 5: The GeneratedSky provider

**Files:**
- Create: `domains/astronomy/src/provider.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs` (module, re-exports, phenomenon kinds in `register_concepts`)

**Interfaces:**
- Produces: `SEASONAL_CYCLE: &str = "seasonal-cycle"`, `NIGHT_STAR: &str = "night-star"` (both registered by `register_concepts` alongside `celestial-body`); `GeneratedSky` with `new(outcome: GenesisOutcome) -> GeneratedSky`, `system(&self) -> &StarSystem`, `calendar(&self) -> &Calendar`, `notes(&self) -> &[String]`, `sky_at(&self, time: WorldTime) -> SkyReport`, and `impl PhenomenaSource`.
- **Phenomena contract (deterministic; periods/saliences rounded to 2 decimals via `round2(x) = (x*100).round()/100`):**
  - Sun, spinning: kind `celestial-body`, desc `format!("the sun, a {}", class_name)`, period `Some(round2(day))`, salience 1.0. Sun, locked: desc `"a sun fixed forever above the day side"`, period None, salience 1.0.
  - Each moon: kind `celestial-body`, desc `format!("a {} moon", size_word)` where size_word = "vast" (angular ≥ 1.2), "full-sized" (≥ 0.7), else "small, distant"; period `Some(round2(period))`; salience `round2(0.35 + 0.35 * angular.min(2.0) / 2.0)`.
  - Season (spinning AND obliquity > 0): kind `seasonal-cycle`, desc `"the slow swelling and shrinking of daylight"`, period `Some(round2(year))`, salience `round2(0.5 * obliquity / 35.0)`.
  - Neighbors: emitted when locked (permanent night side) or when `is_daylight == Some(false)`; kind `night-star`, desc `format!("a {} star that does not wander", color)`, period None, salience `round2((0.1 + 0.1 * (1.0 + apparent_brightness).ln()).clamp(0.1, 0.6))`.
- `sky_at` renders: locked → both faces; spinning day → sun + season hint; spinning night → visible moons with phase words + neighbors. Deterministic strings only.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/provider.rs` (tests):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use hornvale_kernel::{EntityId, ObserverContext, PhenomenaSource, WorldTime, Seed};

    fn sky(pins: SkyPins) -> GeneratedSky {
        GeneratedSky::new(generate(Seed(42), &pins).unwrap())
    }

    fn ctx(day: f64) -> ObserverContext {
        ObserverContext { place: EntityId(1), time: WorldTime { day } }
    }

    #[test]
    fn a_spinning_sun_is_periodic_and_top_salience() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let sun = seen.iter().find(|p| p.description.starts_with("the sun")).unwrap();
        assert_eq!(sun.period_days, Some(1.0));
        assert_eq!(sun.salience, 1.0);
    }

    #[test]
    fn a_locked_sun_is_aperiodic() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let sun = seen.iter().find(|p| p.description.contains("fixed forever")).unwrap();
        assert_eq!(sun.period_days, None);
        assert_eq!(sun.salience, 1.0);
    }

    #[test]
    fn moons_and_seasons_carry_their_periods() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        });
        let seen = s.phenomena(&ctx(0.0));
        let moons: Vec<_> = seen.iter().filter(|p| p.description.contains("moon")).collect();
        assert_eq!(moons.len(), 2);
        for m in &moons {
            assert!(m.period_days.is_some());
            assert!(m.salience < 1.0);
        }
        if s.system().anchor.obliquity.get() > 0.0 {
            assert!(seen.iter().any(|p| p.kind == SEASONAL_CYCLE));
        }
    }

    #[test]
    fn neighbors_appear_only_in_darkness_or_on_locked_worlds() {
        let spinning = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            ..SkyPins::default()
        });
        let cal = spinning.calendar();
        // Find a midday and a midnight of local day 10.
        let day_len = cal.day_length().unwrap().get();
        let noon = 10.0 * day_len + 0.5 * day_len;
        let midnight = 10.0 * day_len + 0.01 * day_len;
        let at_noon = spinning.phenomena(&ctx(noon));
        let at_night = spinning.phenomena(&ctx(midnight));
        assert!(at_noon.iter().all(|p| p.kind != NIGHT_STAR));
        assert!(at_night.iter().any(|p| p.kind == NIGHT_STAR));

        let locked = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        });
        assert!(locked.phenomena(&ctx(noon)).iter().any(|p| p.kind == NIGHT_STAR));
    }

    #[test]
    fn sky_reports_vary_with_time_and_are_deterministic() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let day_len = s.calendar().day_length().unwrap().get();
        let noon = s.sky_at(WorldTime { day: 10.5 * day_len });
        let night = s.sky_at(WorldTime { day: 10.01 * day_len });
        assert_ne!(noon.description, night.description);
        assert_eq!(
            s.sky_at(WorldTime { day: 10.5 * day_len }).description,
            noon.description
        );
    }
}
```

- [ ] **Step 2: Verify failure, implement**

Key implementation (above tests; imports: calendar, system types, units,
kernel Phenomenon/PhenomenaSource/ObserverContext/WorldTime, pins consts):

```rust
/// Phenomenon kind for the annual daylight cycle.
pub const SEASONAL_CYCLE: &str = "seasonal-cycle";
/// Phenomenon kind for notable neighbor stars.
pub const NIGHT_STAR: &str = "night-star";

fn round2(x: f64) -> f64 {
    (x * 100.0).round() / 100.0
}

/// Tier-1/2 astronomy: the generated sky, time-varying.
#[derive(Debug, Clone, PartialEq)]
pub struct GeneratedSky {
    system: StarSystem,
    calendar: Calendar,
    notes: Vec<String>,
}

impl GeneratedSky {
    /// Wrap a genesis outcome as a live provider.
    pub fn new(outcome: GenesisOutcome) -> GeneratedSky {
        let calendar = calendar_of(&outcome.system);
        GeneratedSky { system: outcome.system, calendar, notes: outcome.notes }
    }
    /// The underlying system.
    pub fn system(&self) -> &StarSystem { &self.system }
    /// The derived calendar.
    pub fn calendar(&self) -> &Calendar { &self.calendar }
    /// Genesis notes recorded during generation.
    pub fn notes(&self) -> &[String] { &self.notes }

    fn t(&self, time: WorldTime) -> StdDays {
        StdDays(time.day.max(0.0))
    }

    /// The sky at a moment, rendered.
    pub fn sky_at(&self, time: WorldTime) -> SkyReport { /* build per the
        contract: locked → "A sun hangs motionless above the day side; the
        night side lives beneath {n} unmoving stars."; spinning daylight →
        "The sun, a {class}, stands in the sky." (+ " The days are growing." /
        " The days are shrinking." when season_phase is Some, by sign of
        cos(TAU*phase)); spinning night → "Night. {moons sentence} {neighbors
        sentence}" where each visible moon reads "the {size_word} moon shows
        its {phase_word} face" (phase_word: "new" <0.125 or >=0.875, "waxing"
        <0.5, "full" <0.625, else "waning") and neighbors read "Above, {color}
        stars keep their stations." bodies: "the sun" always; "moon {i+1}"
        per moon at night/locked; nothing else. */ }
}

impl PhenomenaSource for GeneratedSky { /* per the phenomena contract above,
    in this order: sun, moons (index order), season, neighbors (brightness
    order, gated by darkness or Locked). observe() re-sorts by salience. */ }
```

Write the two elided bodies EXACTLY to the contracts in the Interfaces
block and the comments — every string named there is normative (tests and,
later, committed artifacts depend on them). Register the two kinds in
`register_concepts`:

```rust
    registry.register_phenomenon_kind(SEASONAL_CYCLE, "the annual daylight cycle")?;
    registry.register_phenomenon_kind(NIGHT_STAR, "a fixed star notable in the night sky")?;
```

- [ ] **Step 3: Gate, commit**

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): GeneratedSky provider — time-varying sky and phenomena"
```

---

### Task 6: Genesis wiring — facts, pin persistence, provider reconstruction

**Files:**
- Create: `domains/astronomy/src/facts.rs` (tests inline)
- Modify: `domains/astronomy/src/pins.rs` (pin_strings/parse_pin + tests), `lib.rs` (predicates in register_concepts, re-exports)
- Modify: `cli/src/world_builder.rs` (SkyChoice, build_world, sky_of; tests)
- Modify: `cli/src/main.rs` (cmd_new passes defaults — constant sky until Task 7)

**Interfaces:**
- astronomy `register_concepts` additionally registers predicates (all provenance conventions per the book's reference chapter): `star-class` (functional, Text), `tidally-locked` (functional, Flag), `day-length-std` (functional, Number), `year-length-std` (functional, Number), `obliquity-degrees` (functional, Number), `moon-count` (functional, Number), `moon-period-days` (non-functional, Number), `notable-neighbor` (non-functional, Text), `genesis-note` (non-functional, Text), `sky-provider` (functional, Text), `scenario-pin` (non-functional, Text).
- `facts::genesis(world: &mut World, subject: EntityId, outcome: &GenesisOutcome) -> Result<(), LedgerError>` — commits (provenance `"astronomy"`): star-class; tidally-locked flag OR day-length-std; year-length-std; obliquity-degrees; moon-count; one moon-period-days per moon; one notable-neighbor Text per neighbor, `format!("a {} star at {:.0} light-years", color, distance.get())`; one genesis-note per note.
- `pins::pin_strings(pins: &SkyPins) -> Vec<String>` / `pins::parse_pin(s: &str, pins: &mut SkyPins) -> Result<(), String>` — round-trippable formats: `moons=N` | `moons=MIN+K`, `rotation=normal|locked`, `day-hours=F`, `obliquity=none|F`, `year-days=F`, `neighbor=blue-giant|red-giant|white-dwarf`.
- world_builder: `pub enum SkyChoice { Constant, Generated }`; `build_world(seed: Seed, pins: &SkyPins, sky: SkyChoice) -> Result<World, BuildError>` — mints the world entity FIRST, commits `sky-provider` Text ("constant"|"generated") + one `scenario-pin` Text per pin string (provenance `"scenario"`), then for Generated runs `generate` + `facts::genesis`; then terrain → settlement → culture → religion, with `observed_phenomena` routed through the provider. `pub enum Sky { Constant(ConstantSun), Generated(Box<GeneratedSky>) }`; `sky_of(world: &World) -> Result<Sky, BuildError>` reconstructs deterministically from the ledger (absent sky-provider fact → Constant, so 1a/1b saves load unchanged). `sky_report`/`observed_phenomena` route through `sky_of`. `BuildError` gains `Genesis(GenesisError)` and `Pins(String)` variants.
- **Behavior preservation this task:** `cmd_new` passes `(&SkyPins::default(), SkyChoice::Constant)` — seed-42 almanac stays byte-identical (the new facts don't render); the concepts dump CHANGES (11 new predicates + 2 kinds) and is regenerated + committed in this task, like 2a Task 6 did for the manifest.

- [ ] **Step 1: Tests (excerpts — write all)**

pins round-trip (pins.rs tests):

```rust
    #[test]
    fn pin_strings_round_trip_through_parse() {
        let pins = SkyPins {
            moons: Some(MoonsPin::graded(1, 2).unwrap()),
            rotation: Some(RotationPin::Locked),
            obliquity: Some(Degrees::new(12.5).unwrap()),
            year_local_days: None,
            neighbor: Some(NeighborClass::BlueGiant),
        };
        let mut rebuilt = SkyPins::default();
        for s in pin_strings(&pins) {
            parse_pin(&s, &mut rebuilt).unwrap();
        }
        assert_eq!(rebuilt, pins);
        assert!(pin_strings(&SkyPins::default()).is_empty());
    }
```

facts (facts.rs tests): build a world, mint entity, run
`facts::genesis` with a locked 2-moon outcome; assert `tidally-locked`
flag present, no `day-length-std`, `moon-count == 2`, two
`moon-period-days`, at least two `notable-neighbor`, and (for seed 10
default pins) one `genesis-note` containing "was sought".

world_builder (tests): `build_world(Seed(42), &SkyPins::default(), SkyChoice::Generated)`
→ `sky_of` returns `Sky::Generated`; beliefs non-empty; deterministic
(`to_json` equal across two builds); reload → `sky_of` reconstructs and
`sky_report(world, WorldTime{day:0.0})` equal before/after save+load.
`SkyChoice::Constant` → `sky_of` returns Constant and the almanac context
is unchanged vs 1b behavior. A 1b-era world (build via Constant, then
strip? — simpler: assert absent-fact fallback by constructing `World::new`
+ registrations without sky facts → `sky_of` = Constant).

- [ ] **Step 2: Implement**

Follow the Interfaces block exactly. `sky_of` reconstruction: find the
`sky-provider` fact (its subject is the world entity); "generated" →
collect all `scenario-pin` Text facts, fold through `parse_pin`, call
`generate(world.seed, &pins)` (deterministic), wrap in
`GeneratedSky::new`. Errors map into `BuildError::Pins`/`Genesis`.
`observed_phenomena(world, day)`: `match sky_of(world)` → sources array =
[sky source, UniformClimate] (Sky implements PhenomenaSource by
delegation). Religion genesis call site unchanged (it takes phenomena).

Regenerate the concepts artifact in this task's commit:

```bash
cargo run -q -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
```

Verify seed-42 almanac unchanged:
`cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv.json && cargo run -q -p hornvale -- almanac --world /tmp/hv.json | diff - book/src/gallery/almanac-seed-42.md` → empty.

- [ ] **Step 3: Gate, commit**

```bash
git add domains/astronomy/ cli/ book/src/reference/concept-registry-generated.md
git commit -m "feat(cli,astronomy): worlds carry their sky — pins as facts, provider reconstruction"
```

---

### Task 7: CLI pin flags, `--sky`, and `scout`

**Files:**
- Modify: `cli/src/main.rs` (flag parsing, cmd_new, cmd_scout, USAGE)
- Modify: `.github/workflows/ci.yml` (seed-42 almanac line gains `--sky constant` — same commit as the default flip)
- Test: extend `cli/src/world_builder.rs` tests only if parsing helpers land there; parsing tests live in main.rs `#[cfg(test)]`.

**Interfaces:**
- `hornvale new --seed N [--out P] [--sky constant|generated] [--moons N|MIN+K] [--rotation normal|locked] [--day-hours F] [--obliquity none|F] [--year-days F] [--neighbor blue-giant|red-giant|white-dwarf]` — default sky **generated** (spec §8); pin parse errors and GenesisErrors are user-facing with the physical reason.
- `hornvale scout [same pin flags] [--from-seed N=0] [--limit K=5] [--max-scan M=10000]` — deterministically scans seeds upward; for each satisfying seed prints one line `seed {N}: {moons} moons, {day or "tidally locked"}, year {:.1} std days, {neighbors} neighbors`; ends with `scanned {scanned} seeds, found {found}`. Never mutates anything.
- Parsing: `parse_sky_args(args: &[String]) -> Result<(SkyPins, SkyChoice), String>` — builds pin strings from flags and reuses `astronomy::parse_pin` (one parser, no drift); `--day-hours F` → `rotation=PeriodHours` via parse_pin format `day-hours=F`.
- Unit tests: each flag parses; `--moons 2+1` yields graded pin; bad values yield the constructor/parse error text; USAGE mentions scout.

- [ ] **Step 1: Tests, Step 2: Implement, Step 3: Wire CI**

`cmd_new` uses `parse_sky_args`; `cmd_scout`:

```rust
fn cmd_scout(args: &[String]) -> Result<(), String> {
    let (pins, _sky) = parse_sky_args(args)?;
    let from: u64 = flag_value(args, "--from-seed").unwrap_or("0").parse().map_err(|e| format!("--from-seed: {e}"))?;
    let limit: u64 = flag_value(args, "--limit").unwrap_or("5").parse().map_err(|e| format!("--limit: {e}"))?;
    let max_scan: u64 = flag_value(args, "--max-scan").unwrap_or("10000").parse().map_err(|e| format!("--max-scan: {e}"))?;
    let mut found = 0u64;
    let mut scanned = 0u64;
    for seed in from..from.saturating_add(max_scan) {
        scanned += 1;
        if let Ok(outcome) = hornvale_astronomy::generate(hornvale_kernel::Seed(seed), &pins) {
            let system = &outcome.system;
            let day = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day } => format!("{:.1}h day", day.get() * 24.0),
                hornvale_astronomy::Rotation::Locked => "tidally locked".to_string(),
            };
            println!(
                "seed {seed}: {} moons, {day}, year {:.1} std days, {} neighbors",
                system.moons.len(),
                system.anchor.year.get(),
                system.neighbors.len()
            );
            found += 1;
            if found == limit {
                break;
            }
        }
    }
    println!("scanned {scanned} seeds, found {found}");
    Ok(())
}
```

ci.yml, in the artifact step, change the seed-42 lines to:

```yaml
          cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-ci-42.json
```

(the almanac/concepts lines below it unchanged). Verify locally that the
committed almanac is still byte-identical with `--sky constant` and that a
default `new` (generated) differs — both facts go in the report.

- [ ] **Step 4: Gate, commit**

```bash
git add cli/ .github/
git commit -m "feat(cli): sky pins as flags, generated-sky default, and the scout verb"
```

---

### Task 8: REPL calendar and the almanac's new sections

**Files:**
- Modify: `cli/src/world_builder.rs` (`calendar_lines`, `night_sky_line`, `genesis_notes` accessors), `cli/src/repl.rs` (`calendar` command + HELP), `windows/almanac/src/lib.rs` (context fields + rendering + tests)

**Interfaces:**
- world_builder produces: `calendar_lines(world: &World) -> Vec<String>` — empty for constant-sky worlds; for generated: line 1 `"The year is {:.1} local days ({:.1} standard days)."` (spinning) or `"This world is tidally locked: no local day exists; the year is {:.1} standard days."`; if spinning and obliquity > 0: `"Daylight swells to {:.0}% at midsummer and shrinks to {:.0}% at midwinter."` (from `0.5 ± (obliquity/90)/2`, ×100); per moon (ordinals "first"/"second"/"third"): `"The {ordinal} moon circles every {:.1} local days — {:.1} months to a year."` (locked: `"... every {:.1} standard days ..."`). `night_sky_line(world) -> Option<String>`: `"By night: {desc1}; {desc2}; ..."` (neighbor descriptions, brightest first, from the provider's phenomena wording). `genesis_notes(world) -> Vec<String>` (from the reconstructed provider).
- `AlmanacContext` gains `pub calendar_lines: Vec<String>`, `pub night_sky: Option<String>`, `pub genesis_notes: Vec<String>`; `render` adds: `night_sky` as a paragraph at the end of "## The Sky"; a `"## The Calendar"` section (only when `calendar_lines` non-empty) listing the lines as bullets, followed by `"Notes from genesis:"` bullets when `genesis_notes` non-empty.
- REPL: `calendar` command prints `calendar_lines` (or `"this world has no generated sky; time is measured in standard days"`), added to HELP.
- Constant-sky worlds: all three fields empty/None → **almanac byte-identical to before** (the committed seed-42 artifact proves it).

- [ ] **Steps: tests → implement → gate → commit**

Almanac tests: extend `sample_context()` with empty new fields (existing
tests byte-stable); add a test with calendar lines + night sky + notes
asserting section headers and content ordering; empty-world test asserts NO
"## The Calendar" header. REPL test: `calendar` on a generated world
contains "year is"; on a constant world contains "no generated sky".
world_builder tests: calendar_lines for `--rotation locked` contains
"tidally locked"; ordinal/months formatting for a 2-moon pin.

Verify the seed-42 constant artifact diff is still empty; commit:

```bash
git add cli/ windows/
git commit -m "feat(almanac,repl): the calendar arrives — sections, notes, night sky"
```

---

### Task 9: Exit-criterion suite

**Files:**
- Create: `cli/tests/sky_exit_criterion.rs`

**Interfaces:** drives the built binary via `env!("CARGO_BIN_EXE_hornvale")`; per-process temp dirs (pattern from `cli/tests/exit_criterion.rs`); these tests PROVE existing behavior — if one fails, the defect is upstream; report, don't weaken.

- [ ] **Step 1: Write the tests**

Six tests, exact assertions:

1. `rotation_flip_flips_the_religion`: same seed, `--rotation normal` vs
   `--rotation locked` (both `--sky generated`); extract the "## The Gods"
   section from each almanac; assert the sections DIFFER; assert the locked
   one contains `"never"` (eternal template) and the normal one contains
   `"every"` and `"returns"` (cyclic template).
2. `moons_flip_flips_the_calendar_not_the_faith`: same seed +
   `--rotation normal`, `--moons 0` vs `--moons 3`; Gods sections EQUAL;
   Calendar sections differ; the 3-moon almanac contains `"third moon"`.
3. `worlds_survive_reload_byte_identically`: `new` a generated world, run
   `almanac` twice → byte-equal (provider reconstruction is deterministic).
4. `graded_pins_never_fail_above_min`: for seeds 1..=20,
   `new --seed N --moons 0+3` exits 0.
5. `scout_is_deterministic_and_finds_three_moon_worlds`:
   `scout --moons 3 --limit 2` twice → identical stdout, contains at least
   one `"seed "` line and a final `"scanned"` line.
6. `refusals_are_recorded_in_the_world`: `new --seed 10` (default pins,
   generated sky); almanac contains `"was sought"` under Notes from genesis.

- [ ] **Step 2: Run (expect green), gate, commit**

```bash
git add cli/tests/
git commit -m "test(cli): campaign 2 exit criterion — rotation flips faith, moons flip calendars"
```

---

### Task 10: Gallery artifacts and CI

**Files:**
- Create: `book/src/gallery/the-sky.md`, plus generated+committed `book/src/gallery/almanac-seed-42-sky.md` and `almanac-seed-42-locked.md`
- Modify: `book/src/SUMMARY.md`, `.github/workflows/ci.yml`

**Steps:**

1. Generate:

```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-sky.json
cargo run -q -p hornvale -- almanac --world /tmp/hv-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -q -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-locked.json
cargo run -q -p hornvale -- almanac --world /tmp/hv-locked.json > book/src/gallery/almanac-seed-42-locked.md
```

2. `book/src/gallery/the-sky.md` frames the pair (prose: the exit demo —
   one pinned cause, legible downstream difference; the same seed's goblins
   worship a returning god under a spinning sun and an unblinking one under
   a locked sun) and `{{#include}}`s both files with `---` separators.
   SUMMARY gains `- [The Sky of Seed 42](./gallery/the-sky.md)` in the
   Gallery section.

3. ci.yml artifact step gains, after the existing almanac line:

```yaml
          cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-sky.json > book/src/gallery/almanac-seed-42-sky.md
          cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-ci-locked.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-locked.json > book/src/gallery/almanac-seed-42-locked.md
```

4. `mdbook build book`; rerun all generation commands → `git status`
   clean (no-op proof); full gate; commit:

```bash
git add book/ .github/
git commit -m "feat(book): the sky enters the gallery — paired almanacs, CI drift-checked"
```

---

## Post-plan (not subagent work)

Chronicle entry for 2b + freshness sweep (introduction, astronomy chapter
tier status, first-light footnote if needed) per the merged-plan Definition
of Done; campaign-close ritual (concept-registry review — note
`tidally-locked` vs the `is-<kind>` convention; stream-manifest review;
vision-book chapter; model card mirrored into the book's astronomy chapter).

## Self-Review Notes

- **Spec coverage:** §2.5 (T1–T2), §4 amended incl. graded pins/notes/scout
  (T3, T6, T7), §6 (T4), §7 (T5), §8 (T6–T8), §9 amended (T9, T10),
  §10 determinism/property/pin tests woven through; §11 WASM stretch not
  taken.
- **Type consistency spot-checks:** `MoonsPin::exact/graded/min/want`;
  `GenesisOutcome{system, notes}`; `generate → Result<GenesisOutcome, _>`
  everywhere from T3 on; `SkyPins.obliquity: Option<Degrees>` (renamed);
  `Sky::{Constant, Generated}`; `calendar_of`; provider strings normative.
- **Artifact stability plan:** concepts regenerated at T6; seed-42 constant
  almanac protected through T9 (T7 flips the CLI default and pins CI to
  `--sky constant` in the same commit); new sky artifacts land with their CI
  lines at T10.
- **Known risk:** T5's `sky_at` prose and phenomena strings become committed-
  artifact contracts at T10 — the contracts in T5's Interfaces block are
  normative for exactly this reason; reviewers should treat string drift
  as Important.
