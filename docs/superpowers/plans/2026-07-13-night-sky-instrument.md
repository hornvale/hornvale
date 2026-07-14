# The Night Sky Instrument — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the night sky an instrument — seasonal visibility, heliacal rise/set events, circumpolar/pole-star verdicts, wandering planets with real Kepler orbits, and a figured background starfield — per the approved spec `docs/superpowers/specs/2026-07-13-night-sky-instrument-design.md`.

**Architecture:** Three stages on branch `night-sky-instrument`. Stage 1 (Tasks 1–5) is pure derivation from shipped draws: celestial coordinates with precession, the shared `SkyBand` twilight thresholds, the `NightSky::at` unified view, heliacal events as phenomena, pole-star facts. Stage 2 (Tasks 6–8) draws wanderers into `StarSystem` on new appended streams with one new pin. Stage 3 (Tasks 9–11) adds the derived starfield, deterministic figures gated by a calibration lab study, and the campaign's property batteries. Trace-protocol law (spec §5): wanderers → Facts, heliacal events → Phenomena, seasonal visibility → derived view, figures → minimal world-side facts.

**Tech Stack:** Rust edition 2024, `hornvale-kernel` only (no new crates). All transcendentals via `hornvale_kernel::math`. `cargo nextest run` is the test runner.

## Global Constraints

- Layering: `domains/astronomy` depends on `hornvale-kernel` and **nothing else**. Almanac changes go in `windows/almanac`; composition in `windows/worldgen`; CLI flag plumbing in `cli/`.
- Dependencies: `serde`/`serde_json` only, workspace-wide. No rand/chrono/clap.
- Determinism: no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only); float sorts use `total_cmp` with deterministic tie-breaks; no wall-clock time; quantization (`hornvale_kernel::quantize`) at emit boundaries only, never in compute.
- Save-format contracts: stream labels are permanent; **new streams are appended, existing draws never move**. New labels this campaign: `wanderer-count`, `wanderers`, `starfield`. Every new label goes in `streams.rs` AND `stream_labels()` in `lib.rs`.
- Pins fail loudly (`GenesisError`); generation never retries across seeds.
- Every crate has `#![warn(missing_docs)]`: every new public item, field, and variant gets a one-line doc comment, and every primitive at a `pub` boundary gets a `type-audit:` tag (`bare-ok(<class>)` for ratios/flags/counts/identifier-text/prose, `pending(wave-1)` for unclassified physical quantities — match the file's existing tags).
- Commit gate per task: `cargo fmt` (final step before every commit), then `make gate-fast` while iterating; full `make gate` before each stage-boundary merge. Golden fixtures re-pin **in the drifting commit**, never deferred. Censuses are never regenerated locally (`SKIP_CENSUS=1` is the local standard); artifact refresh is `scripts/regenerate-artifacts.sh` with censuses skipped.
- Stage boundaries (after Tasks 5 and 8): run `make preflight` from the branch; on ancestry NO-GO merge main INTO the branch and re-run the gate. The known collision is `domains/astronomy/src/facts.rs` + `lib.rs` (The Self-Describing Sky campaign, `7a48aa8`'s plan): on conflict, keep BOTH campaigns' predicates — both are purely additive.
- Commit messages end with the Claude-Session trailer used on this branch's earlier commits.

## File Structure

```
domains/astronomy/src/
  sky_position.rs   NEW  ecliptic/equatorial coords, precession (Task 1)
  calendar.rs       MOD  solar_equatorial, sky_band (Tasks 1–2)
  provider.rs       MOD  SkyBand adoption; heliacal + wanderer phenomena (Tasks 2, 4, 8)
  night_sky.rs      NEW  NightSky::at unified view (Task 3)
  heliacal.rs       NEW  rise/set/absence events (Task 4)
  facts.rs          MOD  pole-star, wanderer, figure facts (Tasks 5, 8, 10)
  lib.rs            MOD  modules, re-exports, stream_labels, register_concepts
  wanderers.rs      NEW  wanderer genesis (Task 6)
  system.rs         MOD  StarSystem.wanderers (Task 6)
  pins.rs           MOD  wanderers pin (Task 7)
  streams.rs        MOD  three new labels (Tasks 6, 9)
  starfield.rs      NEW  derived catalog (Task 9)
  figures.rs        NEW  clustering + descriptions (Task 10)
domains/astronomy/tests/
  night_sky_regimes.rs  NEW  regime × obliquity + epoch-drift batteries (Task 11)
  genesis_properties.rs MOD  wanderer pin-isolation battery (Task 7)
  tier_refinement.rs    MOD  wanderers keep tier 0's claim (Task 8)
windows/almanac/src/lib.rs  MOD  night-sky lines under "## The Sky" (Tasks 5, 8, 10)
windows/lab/src/metrics.rs  MOD  figure metrics (Task 10)
studies/census-of-figures.study.json  NEW  calibration study (Task 10)
cli/src/main.rs             MOD  --wanderers flag + help (Task 7)
```

---

### Task 0: Branch and worktree

- [ ] **Step 1:** Create the worktree and branch per house convention (`superpowers:using-git-worktrees`; worktree root `~/.config/superpowers/worktrees/hornvale/`): `git worktree add ~/.config/superpowers/worktrees/hornvale/night-sky-instrument -b night-sky-instrument` from repo root. All subsequent tasks run inside the worktree — subagent dispatches must prepend `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md` and `cd` first.

---

## Stage 1 — The Placed Sky

### Task 1: Celestial coordinates with precession (`sky_position.rs`)

**Files:**
- Create: `domains/astronomy/src/sky_position.rs`
- Modify: `domains/astronomy/src/calendar.rs` (three new methods), `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: `Calendar` internals (`forcing`, `year_phase`) — same crate.
- Produces (later tasks rely on these exact names):
  - `pub struct EclipticCoord { pub lon_deg: f64, pub lat_deg: f64 }`
  - `pub struct EquatorialCoord { pub ra_deg: f64, pub dec_deg: f64 }`
  - `pub fn ecliptic_of(eq: &EquatorialCoord, obliquity_deg: f64) -> EclipticCoord`
  - `pub fn equatorial_at(ecl: &EclipticCoord, obliquity_deg: f64, precession_offset_deg: f64) -> EquatorialCoord`
  - `Calendar::precession_offset_deg(&self, t: StdDays) -> f64`
  - `Calendar::solar_equatorial(&self, t: StdDays) -> EquatorialCoord`
  - `Calendar::star_equatorial_at(&self, genesis: &EquatorialCoord, t: StdDays) -> EquatorialCoord`

**Model (goes in the module doc, and the model card at close):** star ecliptic latitudes and longitudes are fixed for all time (no proper motion — deferred registry row). The equinox precesses: apparent ecliptic longitude at `t` is `lon + Δψ(t)` where `Δψ(t) = precession_at(t) − precession_at(0)`. Apparent equatorial coordinates re-project through the epoch's obliquity `obliquity_at(t)`. The sun's ecliptic longitude is `360° · year_phase(t)` (year phase 0 = the equinox, matching the shipped `solar_declination` convention). The shipped small-angle `solar_declination` (`ε·sin(2πφ)`) is **not touched** — byte-frozen; the new exact form (`asin(sin ε sin λ)`) serves star geometry only. Both are declared approximations of the same object at different tiers; they agree at equinoxes and solstices exactly.

- [ ] **Step 1: Write the failing tests** (in `sky_position.rs` `#[cfg(test)]`):

```rust
#[test]
fn round_trip_through_the_ecliptic_is_identity_at_zero_precession() {
    let genesis = EquatorialCoord { ra_deg: 123.4, dec_deg: -21.7 };
    let ecl = ecliptic_of(&genesis, 23.5);
    let back = equatorial_at(&ecl, 23.5, 0.0);
    assert!((back.ra_deg - genesis.ra_deg).abs() < 1e-9);
    assert!((back.dec_deg - genesis.dec_deg).abs() < 1e-9);
}

#[test]
fn a_full_precession_turn_returns_every_star_home() {
    let genesis = EquatorialCoord { ra_deg: 300.0, dec_deg: 55.0 };
    let ecl = ecliptic_of(&genesis, 20.0);
    let after = equatorial_at(&ecl, 20.0, 360.0);
    assert!((after.ra_deg - genesis.ra_deg).abs() < 1e-9);
    assert!((after.dec_deg - genesis.dec_deg).abs() < 1e-9);
}

#[test]
fn precession_moves_the_pole_separation() {
    // A star near the pole drifts as the pole circles the ecliptic pole.
    let genesis = EquatorialCoord { ra_deg: 40.0, dec_deg: 88.0 };
    let ecl = ecliptic_of(&genesis, 23.5);
    let half_turn = equatorial_at(&ecl, 23.5, 180.0);
    let sep0 = 90.0 - genesis.dec_deg;
    let sep1 = 90.0 - half_turn.dec_deg;
    assert!((sep1 - sep0).abs() > 1.0, "pole separation must move: {sep0} vs {sep1}");
}
```

And in `calendar.rs` tests (reuse the existing `spinning_system()` helper and the `at_phase` pattern from `solar_declination_swings_with_the_obliquity`):

```rust
/// The exact solar position agrees with the shipped small-angle declination
/// at the equinoxes and solstices, and its RA advances a full turn per year.
#[test]
fn solar_equatorial_agrees_at_the_cardinal_phases() {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        obliquity: Some(Degrees::new(23.5).unwrap()),
        forcing: Some(crate::pins::ForcingPin::Zero),
        ..SkyPins::default()
    };
    let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
    let year = cal.year_length().get();
    let at_phase = |p: f64| StdDays((p - cal.forcing.year_phase_offset).rem_euclid(1.0) * year);
    assert!(cal.solar_equatorial(at_phase(0.0)).dec_deg.abs() < 1e-6);
    assert!((cal.solar_equatorial(at_phase(0.25)).dec_deg - 23.5).abs() < 1e-6);
    assert!((cal.solar_equatorial(at_phase(0.25)).ra_deg - 90.0).abs() < 1e-6);
    assert!((cal.solar_equatorial(at_phase(0.75)).dec_deg + 23.5).abs() < 1e-6);
}

/// SKY-stale-alignments (sky half): precession finally has a reader — a
/// star's apparent position drifts between epochs kiloyears apart.
#[test]
fn star_positions_drift_under_precession() {
    let cal = calendar_of(&spinning_system());
    let genesis = crate::sky_position::EquatorialCoord { ra_deg: 10.0, dec_deg: 40.0 };
    let now = cal.star_equatorial_at(&genesis, StdDays(0.0));
    let later = cal.star_equatorial_at(&genesis, StdDays(crate::forcing::P_PRECESSION / 4.0));
    assert!((now.ra_deg - genesis.ra_deg).abs() < 1e-9, "epoch 0 is the genesis frame");
    assert!((later.ra_deg - now.ra_deg).abs() > 1.0, "RA must drift over kiloyears");
}
```

- [ ] **Step 2:** Run: `cargo test -p hornvale-astronomy sky_position 2>&1 | tee /tmp/hv-t1.txt` — expect FAIL (module unresolved).
- [ ] **Step 3: Implement.** `sky_position.rs`:

```rust
//! Celestial coordinates (campaign spec §2): where the sun and the fixed
//! stars sit on the sky's sphere at any epoch. The equinox precesses — this
//! module is `forcing::precession_at`'s first reader (SKY-stale-alignments,
//! sky half) — so every apparent position is a function of time. Star
//! ecliptic coordinates are fixed (no proper motion; declared approximation).

use hornvale_kernel::math;

/// A position on the ecliptic sphere, degrees. Fixed for all time.
/// type-audit: pending(wave-1: lon_deg), pending(wave-1: lat_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipticCoord {
    /// Ecliptic longitude, degrees in [0, 360), from the genesis equinox.
    pub lon_deg: f64,
    /// Ecliptic latitude, degrees in [-90, 90].
    pub lat_deg: f64,
}

/// A position on the equatorial sphere, degrees, valid at one epoch.
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EquatorialCoord {
    /// Right ascension, degrees in [0, 360).
    pub ra_deg: f64,
    /// Declination, degrees in [-90, 90].
    pub dec_deg: f64,
}

/// Convert genesis equatorial coordinates to (time-fixed) ecliptic ones,
/// through the genesis obliquity.
/// type-audit: pending(wave-1)
pub fn ecliptic_of(eq: &EquatorialCoord, obliquity_deg: f64) -> EclipticCoord {
    let (a, d) = (eq.ra_deg.to_radians(), eq.dec_deg.to_radians());
    let e = obliquity_deg.to_radians();
    let lat = math::asin(math::sin(d) * math::cos(e) - math::cos(d) * math::sin(e) * math::sin(a));
    let lon = math::atan2(
        math::sin(a) * math::cos(e) + math::tan(d) * math::sin(e),
        math::cos(a),
    );
    EclipticCoord { lon_deg: lon.to_degrees().rem_euclid(360.0), lat_deg: lat.to_degrees() }
}

/// Project ecliptic coordinates onto the equator of an epoch: the apparent
/// longitude is `lon + precession_offset`, re-tilted by that epoch's
/// obliquity.
/// type-audit: pending(wave-1)
pub fn equatorial_at(
    ecl: &EclipticCoord,
    obliquity_deg: f64,
    precession_offset_deg: f64,
) -> EquatorialCoord {
    let lon = (ecl.lon_deg + precession_offset_deg).to_radians();
    let b = ecl.lat_deg.to_radians();
    let e = obliquity_deg.to_radians();
    let dec = math::asin(math::sin(b) * math::cos(e) + math::cos(b) * math::sin(e) * math::sin(lon));
    let ra = math::atan2(
        math::sin(lon) * math::cos(e) - math::tan(b) * math::sin(e),
        math::cos(lon),
    );
    EquatorialCoord { ra_deg: ra.to_degrees().rem_euclid(360.0), dec_deg: dec.to_degrees() }
}
```

`calendar.rs` additions (inside `impl Calendar`; import `crate::sky_position::{EclipticCoord, EquatorialCoord, ecliptic_of, equatorial_at}`):

```rust
/// Degrees the equinox has precessed since genesis (epoch 0) — the first
/// reader `precession_at` has ever had.
/// type-audit: pending(wave-1)
pub fn precession_offset_deg(&self, t: StdDays) -> f64 {
    (self.forcing.precession_at(t.0) - self.forcing.precession_at(0.0)).to_degrees()
}
/// The sun's equatorial position at `t` (exact spherical form; the shipped
/// small-angle `solar_declination` is the coarse tier of the same object).
pub fn solar_equatorial(&self, t: StdDays) -> EquatorialCoord {
    let lam = (360.0 * self.year_phase(t)).to_radians();
    let e = self.forcing.obliquity_at(t.0).to_radians();
    EquatorialCoord {
        ra_deg: math::atan2(math::sin(lam) * math::cos(e), math::cos(lam))
            .to_degrees()
            .rem_euclid(360.0),
        dec_deg: math::asin(math::sin(e) * math::sin(lam)).to_degrees(),
    }
}
/// A fixed star's apparent equatorial position at `t`: genesis coordinates
/// through the genesis ecliptic, drifted by precession, re-projected at the
/// epoch's obliquity.
pub fn star_equatorial_at(&self, genesis: &EquatorialCoord, t: StdDays) -> EquatorialCoord {
    let ecl = ecliptic_of(genesis, self.forcing.obliquity_at(0.0));
    equatorial_at(&ecl, self.forcing.obliquity_at(t.0), self.precession_offset_deg(t))
}
```

`lib.rs`: add `pub mod sky_position;` and `pub use sky_position::{EclipticCoord, EquatorialCoord, ecliptic_of, equatorial_at};`.

- [ ] **Step 4:** Run: `cargo test -p hornvale-astronomy 2>&1 | tee /tmp/hv-t1.txt` — expect PASS (inspect the file, don't re-run).
- [ ] **Step 5:** `cargo fmt && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`, then commit: `feat(astronomy): celestial coordinates — precession gets its first reader (night-sky stage 1)`.

### Task 2: `SkyBand` — the shared twilight thresholds

**Files:**
- Modify: `domains/astronomy/src/calendar.rs`, `domains/astronomy/src/provider.rs` (~line 1020–1035, the placed twilight decision), `domains/astronomy/src/lib.rs` (re-export)

**Interfaces:**
- Produces: `pub enum SkyBand { Day, Twilight, Night }`, `pub const TWILIGHT_DEPTH_DEG: f64 = 12.0;`, `Calendar::sky_band(&self, t: StdDays, latitude: f64) -> Option<SkyBand>` (`None` = locked world).

- [ ] **Step 1: Failing tests** (calendar.rs):

```rust
/// The shared twilight thresholds (spec §2): Day above the horizon,
/// Twilight to −12°, Night below. Locked worlds have no band.
#[test]
fn sky_band_partitions_the_day_by_solar_altitude() {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        obliquity: Some(Degrees::new(0.0).unwrap()),
        forcing: Some(crate::pins::ForcingPin::Zero),
        ..SkyPins::default()
    };
    let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
    let at_fraction = |f: f64| StdDays(10.0 + (f - cal.forcing.day_phase_offset).rem_euclid(1.0));
    assert_eq!(cal.sky_band(at_fraction(0.5), 0.0), Some(SkyBand::Day));
    assert_eq!(cal.sky_band(at_fraction(0.0), 0.0), Some(SkyBand::Night));
    // Just past sunset (fraction 0.76 ≈ sun ~3.6° below on a zero-tilt equator).
    assert_eq!(cal.sky_band(at_fraction(0.76), 0.0), Some(SkyBand::Twilight));
    assert!(calendar_of(&locked_system()).sky_band(StdDays(5.0), 0.0).is_none());
}
```

- [ ] **Step 2:** Run scoped: `cargo test -p hornvale-astronomy sky_band` — FAIL (type missing).
- [ ] **Step 3: Implement** in calendar.rs:

```rust
/// The sky's brightness band at a placed moment — the one shared twilight
/// definition (spec §2): heliacal visibility, the morning/evening star, and
/// the prose renderer all read this, none owns it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SkyBand {
    /// The sun is above the horizon.
    Day,
    /// The sun is below the horizon but within `TWILIGHT_DEPTH_DEG` of it.
    Twilight,
    /// Full dark: the sun is deeper than the twilight band.
    Night,
}

/// How far below the horizon the sun still lights the sky, degrees
/// (model card; the classical astronomical-twilight midpoint).
/// type-audit: pending(wave-1)
pub const TWILIGHT_DEPTH_DEG: f64 = 12.0;

impl Calendar {
    /// The sky band at `t` for an observer at `latitude`; `None` on a
    /// locked world, which has no solar hour.
    pub fn sky_band(&self, t: StdDays, latitude: f64) -> Option<SkyBand> {
        let alt = self.solar_altitude_at(t, latitude)?;
        Some(if alt > 0.0 {
            SkyBand::Day
        } else if alt > -TWILIGHT_DEPTH_DEG {
            SkyBand::Twilight
        } else {
            SkyBand::Night
        })
    }
}
```

- [ ] **Step 4: Provider adoption — deviation recorded during execution.** provider.rs has no placed twilight decision (`sky_at` is placeless; phenomena culling is ever-visibility, not brightness). SkyBand therefore ships with its thresholds on `Calendar` and the prose path documented as the coarse tier (TWILIGHT_MARGIN doc extension); the first placed consumers are Task 4's heliacal machinery and Task 8's morning/evening star, per spec §2. No provider behavior change; zero fixture drift expected.
- [ ] **Step 5:** Run the crate suite once: `cargo test -p hornvale-astronomy 2>&1 | tee /tmp/hv-t2.txt`. If any provider prose fixture drifted, update the frozen strings **in this commit** (run-once-capture; inspect `/tmp/hv-t2.txt`, don't re-run to grep).
- [ ] **Step 6:** Check downstream fixtures: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-t2w.txt` — no artifact drift expected (Step 4's deviation means no provider behavior change); if the workspace run shows ANY drift, that's a bug — report it. `cargo fmt`, clippy, commit: `feat(astronomy): SkyBand — twilight thresholds become shared infrastructure (night-sky stage 1)`.

### Task 3: `NightSky::at` — the unified derived view

**Files:**
- Create: `domains/astronomy/src/night_sky.rs`
- Modify: `domains/astronomy/src/lib.rs` (module + re-export)

**Interfaces:**
- Consumes: `Calendar::{star_equatorial_at, solar_equatorial, sky_band, local_day}`, `StarSystem.neighbors` (each `Neighbor` has `right_ascension`, `declination`, `apparent_brightness`).
- Produces:
  - `pub enum Hemisphere { North, South }`
  - `pub struct PoleStar { pub neighbor: usize, pub pole: Hemisphere, pub separation_deg: f64 }`
  - `pub struct NightSky { pub visible: Vec<usize>, pub circumpolar: Vec<usize>, pub never_rises: Vec<usize>, pub pole_star: Option<PoleStar>, pub wheels_backward: bool, pub frozen: bool }`
  - `pub const POLE_STAR_MAX_SEPARATION_DEG: f64 = 10.0;`
  - `pub fn night_sky_at(system: &StarSystem, calendar: &Calendar, latitude: f64, t: StdDays) -> NightSky`

**Model:** indices index into `system.neighbors` (already brightness-sorted). A star is circumpolar at latitude φ when `dec.signum() == φ.signum() && dec.abs() > 90 − φ.abs()` (at φ=0 nothing is; at |φ|=90 everything same-sign is); it never rises when the mirror holds with opposite sign. All positions are apparent (`star_equatorial_at` at `t` — epoch-honest). Seasonal visibility: a non-circumpolar riser is visible tonight iff the sky band at its **transit** is not `Day`; the transit falls at local-day fraction `(0.5 + (ra_star − ra_sun)/360).rem_euclid(1.0)` from the current day's start (declared approximation, model card: "visible tonight = transits in the dark"). On a locked world (`local_day` is `None`) the sky is `frozen: true`: circumpolar/never-rises still partition by latitude, `visible` = every riser (the night hemisphere's map, not a calendar), `wheels_backward` false. `pole_star`: the brightest neighbor within `POLE_STAR_MAX_SEPARATION_DEG` of either celestial pole at epoch `t` (ties impossible: brightness order, first wins). `wheels_backward` mirrors `calendar`'s retrograde flag (read it via a new one-line `Calendar::is_retrograde(&self) -> bool` accessor).

- [ ] **Step 1: Failing tests** (in `night_sky.rs`):

```rust
#[test]
fn the_equatorial_observer_sees_every_star_across_a_year() {
    let system = spinning_system(); // same helper pattern as calendar.rs tests
    let cal = calendar_of(&system);
    let year = cal.year_length().get();
    let mut seen: std::collections::BTreeSet<usize> = std::collections::BTreeSet::new();
    for k in 0..48 {
        let sky = night_sky_at(&system, &cal, 0.0, StdDays(k as f64 * year / 48.0));
        assert!(sky.circumpolar.is_empty() && sky.never_rises.is_empty());
        seen.extend(sky.visible.iter().copied());
    }
    assert_eq!(seen.len(), system.neighbors.len(), "a year at the equator shows the whole sky");
}

#[test]
fn the_polar_observer_sees_one_unchanging_hemisphere() {
    let system = spinning_system();
    let cal = calendar_of(&system);
    let sky = night_sky_at(&system, &cal, 90.0, StdDays(0.0));
    assert_eq!(
        sky.circumpolar.len() + sky.never_rises.len(),
        system.neighbors.len(),
        "at the pole every star is circumpolar or invisible"
    );
}

#[test]
fn a_locked_world_has_a_frozen_sky() {
    let system = locked_system();
    let cal = calendar_of(&system);
    let a = night_sky_at(&system, &cal, 30.0, StdDays(0.0));
    let b = night_sky_at(&system, &cal, 30.0, StdDays(5000.0));
    assert!(a.frozen);
    assert_eq!(a.visible, b.visible, "the locked sky is a map, not a calendar");
}

#[test]
fn seasonal_visibility_actually_varies() {
    let system = spinning_system();
    let cal = calendar_of(&system);
    let year = cal.year_length().get();
    let skies: Vec<Vec<usize>> = (0..12)
        .map(|k| night_sky_at(&system, &cal, 30.0, StdDays(k as f64 * year / 12.0)).visible)
        .collect();
    assert!(skies.iter().any(|s| *s != skies[0]), "winter stars must differ from summer stars");
}
```

- [ ] **Step 2:** `cargo test -p hornvale-astronomy night_sky` — FAIL.
- [ ] **Step 3: Implement** (complete function):

```rust
/// The unified derived view (spec §2): everything a placed observer's night
/// holds, in one query. Committed nowhere — recomputed on demand.
pub fn night_sky_at(system: &StarSystem, calendar: &Calendar, latitude: f64, t: StdDays) -> NightSky {
    let frozen = calendar.day_length().is_none();
    let sun = calendar.solar_equatorial(t);
    let mut circumpolar = Vec::new();
    let mut never_rises = Vec::new();
    let mut visible = Vec::new();
    let mut pole_star: Option<PoleStar> = None;
    for (i, n) in system.neighbors.iter().enumerate() {
        let genesis = EquatorialCoord { ra_deg: n.right_ascension, dec_deg: n.declination };
        let pos = calendar.star_equatorial_at(&genesis, t);
        let same_side = pos.dec_deg.signum() == latitude.signum() && latitude != 0.0;
        if same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            circumpolar.push(i);
        } else if !same_side && pos.dec_deg.abs() > 90.0 - latitude.abs() {
            never_rises.push(i);
            continue;
        }
        let north_sep = 90.0 - pos.dec_deg;
        let south_sep = 90.0 + pos.dec_deg;
        if pole_star.is_none() {
            if north_sep <= POLE_STAR_MAX_SEPARATION_DEG {
                pole_star = Some(PoleStar { neighbor: i, pole: Hemisphere::North, separation_deg: north_sep });
            } else if south_sep <= POLE_STAR_MAX_SEPARATION_DEG {
                pole_star = Some(PoleStar { neighbor: i, pole: Hemisphere::South, separation_deg: south_sep });
            }
        }
        if circumpolar.contains(&i) {
            visible.push(i);
            continue;
        }
        if frozen {
            visible.push(i);
            continue;
        }
        // Visible tonight = transits in the dark (declared approximation).
        let day_start = t.0 - calendar.local_day(t).map(|(_, f)| f).unwrap_or(0.0)
            * calendar.day_length().map(|d| d.0).unwrap_or(1.0);
        let transit_fraction = (0.5 + (pos.ra_deg - sun.ra_deg) / 360.0).rem_euclid(1.0);
        let transit_t = StdDays(day_start + transit_fraction * calendar.day_length().map(|d| d.0).unwrap_or(1.0));
        if calendar.sky_band(transit_t, latitude) != Some(SkyBand::Day) {
            visible.push(i);
        }
    }
    NightSky { visible, circumpolar, never_rises, pole_star, wheels_backward: calendar.is_retrograde(), frozen }
}
```

Add the `Calendar::is_retrograde` accessor: `pub fn is_retrograde(&self) -> bool { self.retrograde }` with doc + `type-audit: bare-ok(flag)`.

- [ ] **Step 4:** `cargo test -p hornvale-astronomy 2>&1 | tee /tmp/hv-t3.txt` — PASS. Fix any determinism/edge findings from the file.
- [ ] **Step 5:** fmt, clippy, commit: `feat(astronomy): NightSky::at — the placed observer's unified night (night-sky stage 1)`.

### Task 4: Heliacal risings, settings, and the absence interval

**Files:**
- Create: `domains/astronomy/src/heliacal.rs`
- Modify: `domains/astronomy/src/provider.rs` (phenomena emission), `domains/astronomy/src/lib.rs` (module, re-exports, `register_concepts` gains two phenomenon kinds)

**Interfaces:**
- Produces:
  - `pub struct HeliacalPair { pub neighbor: usize, pub rising_frac: f64, pub setting_frac: f64 }` (year-phase fractions in `[0,1)` of the year containing `t`)
  - `impl HeliacalPair { pub fn absence_fraction(&self) -> f64 }` (`(rising − setting).rem_euclid(1.0)`)
  - `pub fn arcus_visionis_deg(class: NeighborClass) -> f64`
  - `pub fn heliacal_events(system: &StarSystem, calendar: &Calendar, latitude: f64, t: StdDays) -> Vec<HeliacalPair>`
  - Phenomenon kind consts in provider: `pub const HELIACAL_RISING: &str = "heliacal-rising";` `pub const HELIACAL_SETTING: &str = "heliacal-setting";`

**Model (model card):** arcus visionis per class — the sun's minimum depth below the horizon for the star to be glimpsed at its own rising/setting: BlueGiant/RedGiant 7.0°, OrangeGiant/SunLike 9.0°, WhiteDwarf/RedDwarf 11.0° (all within the shared `TWILIGHT_DEPTH_DEG` band — brighter surfaces earlier, reusing SkyBand's frame, spec §2). A star's **morning first visibility** on a given day: at the star's rising moment (local-day fraction `transit_fraction − rise_half_arc/360`, where `cos H₀ = −tan φ · tan δ` gives `rise_half_arc = H₀` in degrees), the sun's altitude must be ≤ −arcus. The **heliacal rising** is the first day of the year where this holds and it held on no previous scanned day after a stretch of invisibility; the **heliacal setting** mirrors it at the star's setting moment in the evening. Scan the year at 400 evenly spaced samples (deterministic fixed count, not day-length-dependent), evaluating the predicate per sample; return `None`-less pairs only for stars that have both events (circumpolar stars and never-risers have neither; locked worlds return an empty list).

- [ ] **Step 1: Failing tests** (`heliacal.rs`); build the **minimal sky** fixture — the spec §7 Sothic kernel — directly:

```rust
/// One bright star + the sun: the instrument's load-bearing kernel.
fn minimal_sky() -> (StarSystem, Calendar) {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        obliquity: Some(Degrees::new(23.5).unwrap()),
        forcing: Some(ForcingPin::Zero),
        ..SkyPins::default()
    };
    let mut system = generate(Seed(42), &pins).unwrap().system;
    system.neighbors.truncate(1);
    system.neighbors[0].declination = -10.0; // rises and sets at mid-northern latitudes
    system.neighbors[0].right_ascension = 45.0;
    let calendar = calendar_of(&system);
    (system, calendar)
}

#[test]
fn the_minimal_sky_yields_one_heliacal_pair_with_an_absence() {
    let (system, calendar) = minimal_sky();
    let pairs = heliacal_events(&system, &calendar, 35.0, StdDays(0.0));
    assert_eq!(pairs.len(), 1);
    let p = &pairs[0];
    assert!((0.0..1.0).contains(&p.rising_frac) && (0.0..1.0).contains(&p.setting_frac));
    let absence = p.absence_fraction();
    assert!(absence > 0.0 && absence < 0.5, "the star vanishes, then returns: {absence}");
}

#[test]
fn brighter_classes_surface_in_brighter_twilight() {
    assert!(arcus_visionis_deg(NeighborClass::BlueGiant) < arcus_visionis_deg(NeighborClass::RedDwarf));
    for class in [NeighborClass::BlueGiant, NeighborClass::RedDwarf] {
        assert!(arcus_visionis_deg(class) < TWILIGHT_DEPTH_DEG);
    }
}

#[test]
fn locked_worlds_have_no_heliacal_events() {
    let system = locked_system();
    let calendar = calendar_of(&system);
    assert!(heliacal_events(&system, &calendar, 35.0, StdDays(0.0)).is_empty());
}

#[test]
fn circumpolar_stars_have_no_heliacal_events() {
    let (mut system, _) = minimal_sky();
    system.neighbors[0].declination = 88.0;
    let calendar = calendar_of(&system);
    assert!(heliacal_events(&system, &calendar, 60.0, StdDays(0.0)).is_empty());
}
```

- [ ] **Step 2:** `cargo test -p hornvale-astronomy heliacal` — FAIL.
- [ ] **Step 3: Implement** `heliacal_events`: for each neighbor, compute apparent position once per scan sample (`star_equatorial_at`), skip circumpolar/never-rise at the latitude; per sample `k` of 400, `t_k = year_start + k/400 · year`; morning-visibility predicate as in the model above (sun altitude via `solar_altitude_at` at the star's rise moment of that local day); find the FALSE→TRUE edge for rising and the TRUE→FALSE edge (of evening visibility at the setting moment) for setting; convert edge samples to year-phase fractions. Where the predicate is true for all 400 samples or false for all, emit no pair (no absence, or never visible). Use only `Vec` state; iterate neighbors in index order.
- [ ] **Step 4: Phenomena.** In provider.rs: register kinds in `register_concepts` (`registry.register_phenomenon_kind(HELIACAL_RISING, "a star's first dawn return from behind the sun")?;` and the setting twin: `"a star's last evening before the sun swallows it"`). In `GeneratedSky::phenomena`, when `ctx.position` is `Some(pos)` and the world spins: compute `heliacal_events(...)` at `pos.latitude`; for each pair whose rising (or setting) falls within half a local day of `ctx.time`, push a `Phenomenon` with kind `HELIACAL_RISING`/`HELIACAL_SETTING`, salience `0.6`, and text naming the star's color and the event ("The {color} star returns before dawn." / "The {color} star takes its leave into the sunset."). Salience 0.6: above an ordinary night star, below every SKY-23 top-salience invariant (test asserts `< 1.0`).
- [ ] **Step 5: Provider test** (provider.rs tests, using the existing `ObserverContext::at_position` pattern): generate seed 42 spinning, scan a year of days at latitude 35 for at least one `HELIACAL_RISING` phenomenon; assert every emitted one has `salience < 1.0`.
- [ ] **Step 6:** Full crate run once with tee; fmt; clippy; commit: `feat(astronomy): heliacal risings, settings, and the absence between (night-sky stage 1)`.

### Task 5: Pole-star facts, the almanac's night instrument, stage close

**Files:**
- Modify: `domains/astronomy/src/facts.rs`, `domains/astronomy/src/lib.rs` (predicates), `windows/almanac/src/lib.rs` (Sky section lines), `windows/worldgen/src/lib.rs` (context assembly), `cli/src/main.rs` (only if the almanac context is assembled there — locate with `grep -rn "AlmanacContext" cli/ windows/`)

**Interfaces:**
- Produces: fact consts `pub const POLE_STAR_NORTH: &str = "pole-star-north";` `pub const POLE_STAR_SOUTH: &str = "pole-star-south";` (functional, Number = separation degrees, committed only when a pole star exists at genesis epoch — day-0-scoped by construction, spec §2 epoch honesty); almanac struct `pub struct NightSkyLines { pub pole_star: Option<String>, pub heliacal: Vec<String> }` consumed by `render`.

- [ ] **Step 1: Failing fact test** (facts.rs, following `genesis_commits_one_tide_fact_per_moon`'s shape): pin a system whose brightest neighbor is forced near a pole is not directly pinnable — instead test both branches over seeds: scan seeds 0..64, derive `night_sky_at(system, calendar, 0.0, StdDays(0.0))`, and assert `genesis` commits `POLE_STAR_NORTH`/`SOUTH` (quantized separation) exactly when the view finds one, never both, and nothing otherwise.
- [ ] **Step 2:** Implement in `facts::genesis` (after the neighbor loop): derive `let sky = crate::night_sky::night_sky_at(&system_calendar...)` — compute `calendar_of(system)` locally; commit the matching fact. Register both predicates in `register_concepts` (functional=true, docs: "a bright star stands within 10 degrees of the north/south celestial pole at genesis (epoch-scoped: precession retires pole stars)").
- [ ] **Step 3: Almanac.** Extend the `## The Sky` section (after the existing night-star line, before `## The Calendar`): one line when a pole star exists ("A {color} star stands {sep}° from the {north|south} celestial pole; the sky wheels around it{ backward, on this backward-spinning world}."), then up to three heliacal lines at the flagship vantage ("The {color} star returns before dawn at year-phase {frac:.2}, after {days:.0} days of absence."), built from `NightSkyLines` assembled in the same place the context's existing sky fields are (worldgen's almanac-context builder; locate with the grep in Files). Numbers formatted through the same quantize-then-format path the section already uses. Add a render test asserting the line renders under `## The Sky` and before `## The Calendar` when present, absent when `None` (follow `deep_time_section_renders_when_present_and_is_skipped_when_empty`).
- [ ] **Step 4:** Workspace run once (`cargo nextest run --workspace 2>&1 | tee /tmp/hv-t5.txt`); regenerate artifacts (`SKIP_CENSUS=1 scripts/regenerate-artifacts.sh`) — the seed-42 almanacs WILL drift (new facts + new lines); commit code + rebaselined artifacts + goldens together.
- [ ] **Step 5:** **Stage boundary:** `make gate 2>&1 | tail -20`, then `make preflight`; on NO-GO merge main into the branch (facts.rs/lib.rs conflicts: keep both campaigns' predicates) and re-run `make gate`. Commit: `feat(astronomy,almanac): pole-star facts + the night instrument in the almanac (night-sky stage 1 close)`.

---

## Stage 2 — The Wanderers

### Task 6: Wanderer genesis

**Files:**
- Create: `domains/astronomy/src/wanderers.rs`
- Modify: `domains/astronomy/src/streams.rs`, `domains/astronomy/src/system.rs`, `domains/astronomy/src/lib.rs` (module, re-exports, stream_labels)

**Interfaces:**
- Produces:
  - streams: `pub const WANDERER_COUNT: &str = "wanderer-count";` `pub const WANDERERS: &str = "wanderers";`
  - `pub enum WandererClass { Rock, Giant }`
  - `pub struct Wanderer { pub orbit: Au, pub period: StdDays, pub class: WandererClass, pub albedo: f64, pub max_elongation_deg: Option<f64>, pub synodic_period: StdDays, pub apparent_brightness: f64 }`
  - `pub fn generate_wanderers(astronomy_seed: Seed, star: &Star, anchor: &Anchor, pins: &SkyPins) -> Vec<Wanderer>`
  - `StarSystem` gains `pub wanderers: Vec<Wanderer>` (innermost first); `generate()` calls `generate_wanderers` **after** `generate_forcing` (appended last; stream separation means order is safety, not correctness).

**Model (model card):** count roll 1–100 on `WANDERER_COUNT`: 1–10 → 0, 11–35 → 1, 36–65 → 2, 66–90 → 3, 91–100 → 4. Per wanderer on `WANDERERS` (fixed draw order — never reorder): region roll (1–100; ≤ 40 inner), axis fraction `f`, class roll, albedo. Inner axis: `a = a_anchor · (0.25 + 0.5·f)` (0.25–0.75 of the anchor's orbit — Hill-safe by construction). Outer axis log-uniform: `a = a_anchor · exp(f · ln(20/1.8)) · 1.8` (1.8–20× the anchor). Class: inner always `Rock`; outer `Giant` when class roll ≤ 60 else `Rock`. Albedo `0.1 + 0.6·draw`. Derived, never drawn: `period = 365.25·sqrt(a³/M)` (the anchor's own Kepler form, `anchor.rs::year_from_orbit`); `synodic_period = |1/(1/P_w − 1/P_anchor)|`; `max_elongation_deg = asin(a/a_anchor)` for inner, `None` for outer; `apparent_brightness = albedo · r² / (a² · Δ²)` with `r` = 0.5 (Rock) or 4.0 (Giant) and `Δ = |a − a_anchor|` (closest approach; relative units like neighbor brightness). Sort by `orbit` ascending, `total_cmp`.

- [ ] **Step 1: Failing tests** (wanderers.rs):

```rust
#[test]
fn wanderers_are_deterministic_and_bounded() {
    let star = generate_star(Seed(42).derive(crate::streams::ROOT));
    let anchor = generate_anchor(Seed(42).derive(crate::streams::ROOT), &star, &SkyPins::default()).unwrap();
    let a = generate_wanderers(Seed(42).derive(crate::streams::ROOT), &star, &anchor, &SkyPins::default());
    let b = generate_wanderers(Seed(42).derive(crate::streams::ROOT), &star, &anchor, &SkyPins::default());
    assert_eq!(a, b);
    assert!(a.len() <= 4);
}

#[test]
fn kepler_holds_and_periods_are_monotone_in_axis() {
    for seed in 0..64u64 {
        let s = Seed(seed).derive(crate::streams::ROOT);
        let star = generate_star(s);
        let anchor = generate_anchor(s, &star, &SkyPins::default()).unwrap();
        let ws = generate_wanderers(s, &star, &anchor, &SkyPins::default());
        for pair in ws.windows(2) {
            assert!(pair[0].orbit.get() < pair[1].orbit.get(), "innermost first");
            assert!(pair[0].period.get() < pair[1].period.get(), "Kepler is monotone");
        }
        for w in &ws {
            let expected = 365.25 * (w.orbit.get().powi(3) / star.mass.get()).sqrt();
            assert!((w.period.get() - expected).abs() < 1e-9);
            match w.max_elongation_deg {
                Some(e) => {
                    assert!(w.orbit.get() < anchor.orbit.get(), "elongation-bound = inner");
                    assert!(e > 0.0 && e < 90.0, "inner elongation under 90°: {e}");
                }
                None => assert!(w.orbit.get() > anchor.orbit.get(), "outer loops at opposition"),
            }
        }
    }
}

#[test]
fn no_wanderer_crowds_the_anchor() {
    for seed in 0..64u64 {
        let s = Seed(seed).derive(crate::streams::ROOT);
        let star = generate_star(s);
        let anchor = generate_anchor(s, &star, &SkyPins::default()).unwrap();
        for w in generate_wanderers(s, &star, &anchor, &SkyPins::default()) {
            let ratio = w.orbit.get() / anchor.orbit.get();
            assert!(!(0.75..=1.8).contains(&ratio), "exclusion band violated: {ratio}");
        }
    }
}
```

- [ ] **Step 2:** FAIL run, scoped.
- [ ] **Step 3: Implement** per the model, with the two stream consts added to streams.rs (doc comments + type-audit tags, matching the file) and two rows appended to `stream_labels()` (`"astronomy/wanderer-count"`, `"astronomy/wanderers"`). Add `wanderers` to `StarSystem` with doc `/// Wandering sibling planets, innermost first (observational: no physical effect on the anchor — declared approximation).` and wire `generate()`. Update `system.rs`'s `generate_assembles_a_complete_system` to also assert `system.wanderers.len() <= 4`.
- [ ] **Step 4:** **Byte-identity check** — the load-bearing one: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-t6.txt`. The golden seed-42 battery and all existing fixtures must pass UNCHANGED (new streams are appended; nothing existing may move). Any golden drift here is a bug in this task, not a rebaseline.
- [ ] **Step 5:** fmt, clippy, commit: `feat(astronomy): wanderers — sibling planets with real Kepler orbits (night-sky stage 2)`.

### Task 7: The `--wanderers` pin

**Files:**
- Modify: `domains/astronomy/src/pins.rs`, `domains/astronomy/src/wanderers.rs`, `domains/astronomy/tests/genesis_properties.rs`, `cli/src/main.rs`

**Interfaces:**
- `SkyPins` gains `pub wanderers: Option<u32>` (0–4). Pin string `wanderers=N` via `pin_strings`/`parse_pin` (round-trips). CLI flag `--wanderers N` in `parse_sky_args`'s flag table and the `hornvale help` text (next to `--moons`).

- [ ] **Step 1: Failing tests.** pins.rs: extend `pin_strings_round_trip_through_parse` style with a `wanderers=3` round-trip test and a `parse_pin("wanderers=5", ...)` rejection test (`InvalidPin`, reason names "the legal maximum is 4"). wanderers.rs: pin honored for every value 0..=4 (mirror `moon_count_pin_is_honored_for_every_legal_value`). genesis_properties.rs — the isolation battery (mirror the existing pin-isolation tests in that file):

```rust
/// Save-format contract: pinning the wanderer count moves NOTHING else.
/// The count draw still happens (then is overridden); per-wanderer draws
/// live on their own stream, so star/anchor/moons/neighbors/forcing bytes
/// are identical pinned vs unpinned.
#[test]
fn wanderers_pin_leaves_the_rest_of_the_sky_untouched() {
    for seed in [1u64, 7, 42, 99] {
        let unpinned = generate(Seed(seed), &SkyPins::default()).unwrap().system;
        for n in 0..=4u32 {
            let pins = SkyPins { wanderers: Some(n), ..SkyPins::default() };
            let pinned = generate(Seed(seed), &pins).unwrap().system;
            assert_eq!(pinned.wanderers.len() as u32, n);
            assert_eq!(unpinned.star, pinned.star);
            assert_eq!(unpinned.anchor, pinned.anchor);
            assert_eq!(unpinned.moons, pinned.moons);
            assert_eq!(unpinned.neighbors, pinned.neighbors);
            assert_eq!(unpinned.forcing, pinned.forcing);
        }
    }
}
```

- [ ] **Step 2:** FAIL run. **Step 3: Implement**: in `generate_wanderers`, the `WANDERER_COUNT` roll is **always drawn**, then `pins.wanderers` overrides the mapped count; validation (`> 4` → `InvalidPin`) lives in `parse_pin` so `SkyPins` construction stays infallible (match the `moons=4` precedent — rejection at parse). `pin_strings` emits `wanderers=N`. CLI: add the flag to `parse_sky_args`'s match (folding to the pin string, like every other flag) and one help line: `  [--wanderers N]                          pin the wandering-planet count (0-4)`.
- [ ] **Step 4:** Workspace run once with tee (help-text goldens may drift → update in this commit); fmt; clippy; commit: `feat(astronomy,cli): the --wanderers pin and its isolation battery (night-sky stage 2)`.

### Task 8: Wanderer facts, phenomena, almanac; stage close

**Files:**
- Modify: `domains/astronomy/src/facts.rs`, `domains/astronomy/src/lib.rs`, `domains/astronomy/src/provider.rs`, `domains/astronomy/tests/tier_refinement.rs`, `windows/almanac/src/lib.rs` (+ its context assembly, same site as Task 5)

**Interfaces:**
- Fact consts (parallel-list pattern, innermost order): `WANDERER_COUNT_FACT = "wanderer-count"` (functional, Number), `WANDERER_ORBIT_AU = "wanderer-orbit-au"`, `WANDERER_PERIOD_STD = "wanderer-period-std"`, `WANDERER_CLASS = "wanderer-class"` (all three non-functional; class as Text `"rock"`/`"giant"`).
- Phenomenon kind: `pub const WANDERING_STAR: &str = "wandering-star";`

- [ ] **Step 1: Failing tests.** facts.rs: mirror `genesis_commits_one_tide_fact_per_moon` — pin `wanderers=2`, assert one orbit/period/class fact per wanderer (quantized) plus the count fact. provider.rs: with `wanderers=2` pinned and a night placed context, assert at least one `WANDERING_STAR` phenomenon whose text contains "wander", salience in `(0.0, 1.0)`. tier_refinement.rs: extend the seed sweep's pin grid with `wanderers=4` and assert the existing four invariants hold (the file's battery is data-driven over pins — add the case).
- [ ] **Step 2:** FAIL run. **Step 3: Implement.**
  - facts.rs: commit count + per-wanderer trio after the pole-star block; register the four predicates ("how many wandering planets cross this sky", "orbital distance of a wanderer, in AU", "orbital period of a wanderer, in standard days", "a wanderer's kind: rock or giant").
  - provider.rs: register `WANDERING_STAR` ("a bright star that will not keep its station"). At night (same placed branch as `NIGHT_STAR` emission): for each wanderer, visibility rule — **inner**: visible only when its synodic phase puts it within `max_elongation_deg` of the sun AND elongation ≥ 15° (else lost in glare); elongation at time `t` modeled as `max_elongation_deg · |sin(2π · t/synodic + phase)|` with `phase` from the shipped `PHASE_OFFSETS` pattern — draw **nothing new**: reuse the wanderer's index against `forcing.year_phase_offset` rotated by index (`(year_phase_offset + index as f64 * 0.37).fract()`, a declared approximation documented in the model card; no stream contract change). **Outer**: visible unless within 15° of the sun's RA; add ", drifting backward against the stars" to its text when within 30° of opposition (the retrograde loop). Morning/evening star: when an inner wanderer is visible and the sky band is `Twilight`, the text becomes "the morning star" / "the evening star" per local-day fraction < 0.5 or ≥ 0.5 — two names the observer has no reason to unify (spec §3). Salience 0.65.
- [ ] **Step 4:** Almanac: a wanderer block under `## The Sky` ("Two wanderers cross this sky: a rock on a 224-day round, a giant on a 4,332-day round." — counts and periods from facts context, same formatting path as Task 5). Render tests per Task 5's pattern.
- [ ] **Step 5:** Workspace run once with tee; seed-42 artifacts drift (new facts/lines) → regenerate with `SKIP_CENSUS=1`, re-pin goldens in this commit. fmt; clippy.
- [ ] **Step 6:** **Stage boundary:** `make gate`, `make preflight`, absorb main if needed, re-gate. Commit: `feat(astronomy,almanac): wanderer facts, phenomena, and the morning star (night-sky stage 2 close)`.

---

## Stage 3 — The Figured Sky

### Task 9: The derived starfield

**Files:**
- Create: `domains/astronomy/src/starfield.rs`
- Modify: `domains/astronomy/src/streams.rs` (`pub const STARFIELD: &str = "starfield";`), `domains/astronomy/src/lib.rs` (module, re-export, stream_labels row `"astronomy/starfield"`)

**Interfaces:**
- `pub struct FieldStar { pub ra_deg: f64, pub dec_deg: f64, pub magnitude_class: u8 }` (1 = brightest … 5 = faintest)
- `pub fn starfield(astronomy_seed: Seed) -> Vec<FieldStar>` — pure function, **never called from `generate()`** (a field, not a genesis draw; the catalog is re-derived on demand and never serialized).

**Model:** count = `range_u32(100, 300)`; per star: dec via the sphere-uniform transform (`asin(2u−1)`, exactly `neighborhood.rs`'s form), RA uniform, magnitude roll 1–100 → class (1: 1–5, 2: 6–15, 3: 16–35, 4: 36–65, 5: 66–100 — dim-heavy, like a real sky).

- [ ] **Step 1: Failing tests:** determinism (two calls equal); count in 100..=300; every dec in [−90,90], RA in [0,360), class in 1..=5; coarse isotropy (six 30°-dec bands each nonempty over the ~200 stars — with the sphere-uniform transform the equatorial bands hold most stars); and **genesis untouched**: `generate(Seed(42), &SkyPins::default())` byte-equal before/after the module exists (guaranteed structurally — the test documents it by asserting `generate` never consumes the `STARFIELD` stream: two systems generated with and without a preceding `starfield()` call are equal).
- [ ] **Step 2:** FAIL. **Step 3:** Implement (one stream, fixed draw order: count, then per-star dec-u, ra-u, mag-roll). **Step 4:** Crate run once; fmt; clippy; commit: `feat(astronomy): the derived starfield — texture, never facts (night-sky stage 3)`.

### Task 10: Figures, the ecliptic flag, and the calibration study

**Files:**
- Create: `domains/astronomy/src/figures.rs`, `studies/census-of-figures.study.json`
- Modify: `domains/astronomy/src/facts.rs` + `lib.rs` (figure predicates), `windows/lab/src/metrics.rs` (three metrics), `windows/almanac/src/lib.rs` (figures line)

**Interfaces:**
- `pub struct Figure { pub member_count: usize, pub centroid: EquatorialCoord, pub span_deg: f64, pub brightest_class: u8, pub on_ecliptic: bool }`
- `pub fn figures(astronomy_seed: Seed, system: &StarSystem) -> Vec<Figure>` (unifies neighbors at magnitude-class 1 with field stars of class ≤ `FIGURE_MAGNITUDE_FLOOR`)
- `pub fn describe(figure: &Figure) -> String` — deterministic structural description, no proper names
- Constants (provisional until Step 4 freezes them): `pub const FIGURE_SEPARATION_DEG: f64 = 8.0;` `pub const FIGURE_MAGNITUDE_FLOOR: u8 = 3;` `pub const FIGURE_MIN_MEMBERS: usize = 3;`
- Fact consts: `FIGURE_COUNT = "figure-count"` (functional, Number); per figure, committed in deterministic order (descending member count, then centroid RA ascending): `FIGURE_MEMBERS = "figure-members"` (Number), `FIGURE_REGION = "figure-region"` (Text: `"northern sky"` / `"southern sky"` / `"the equator's road"` by centroid dec ≥ 20 / ≤ −20 / between), `FIGURE_ON_ECLIPTIC = "figure-on-ecliptic"` (Flag, committed only when true).

**Model:** single-link clustering on the unit sphere: sort the unified bright-star list by `(magnitude_class asc, ra_deg via total_cmp, dec_deg via total_cmp)`; union–find over all pairs with angular separation ≤ `FIGURE_SEPARATION_DEG` (great-circle: `acos(sin δ₁ sin δ₂ + cos δ₁ cos δ₂ cos Δα)`, kernel math); keep clusters with ≥ `FIGURE_MIN_MEMBERS` members. Centroid: normalized Cartesian mean, converted back (deterministic; kernel math only). `on_ecliptic`: the genesis ecliptic latitude of the centroid (via `ecliptic_of` at genesis obliquity) satisfies `|lat| ≤ span/2 + 8.0` — the sun-and-wanderer road crosses the figure. **The brightness floor is the reference-observer convention (spec §4)** — say so in the const's doc comment. `describe`: "a {tight|loose} {chain|knot} of {count} in the {region}" — chain when `span_deg / member_count ≥ 3.0`, knot otherwise; tight when `span_deg < 10`, loose otherwise; count in words via a local copy of the count-word table extended to "many" past twelve.

- [ ] **Step 1: Failing tests** (figures.rs): determinism (two calls equal); every figure has ≥ `FIGURE_MIN_MEMBERS`; `describe()` contains no digits and mentions a region word; ecliptic-flag sanity as a property over seeds 0..32 — for every produced figure, `on_ecliptic == (centroid genesis-ecliptic latitude.abs() <= span_deg/2.0 + 8.0)`, recomputed independently in the test via `ecliptic_of`.
- [ ] **Step 2:** FAIL run. **Step 3:** Implement clustering + describe + facts (after the wanderer block in `facts::genesis`; register the four predicates) + the almanac line ("The sky holds {n} figures; {m} stand on the sun's road."). Workspace run once; artifacts drift → rebaseline in-commit; fmt; clippy; commit: `feat(astronomy): figures — the sky clusters, the zodiac emerges (night-sky stage 3)`.
- [ ] **Step 4: The calibration study (spec §4 gate — constants freeze only after this readout).**
  - `studies/census-of-figures.study.json`:

```json
{ "name": "census-of-figures",
  "description": "Figure-clustering calibration: count/size/zodiac distributions across seeds, before the thresholds freeze (spec 2026-07-13 night-sky-instrument §4).",
  "seeds": { "from": 0, "count": 1000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": ["figure-count", "largest-figure-members", "ecliptic-figure-count"] }
```

  - Three `Metric` entries in `windows/lab/src/metrics.rs`, `Extractor::Astronomy`, each `MetricValue::Text(...to_string())` with `SummaryKind::Categorical` (exact shape of the existing `neighbor-count` metric at line ~711); each derives `figures(v.world.seed.derive("astronomy"), &v.system)` — confirm the seed-derivation call against how `AstronomyView` reconstructs (`v.world` carries the world seed; the astronomy seed is `world_seed.derive("astronomy")`, the same derivation `system.rs::generate` performs).
  - Run locally (astronomy-rung builds are cheap; this is a development instrument, not a committed census fixture): `cargo run --release -p hornvale -- lab run studies/census-of-figures.study.json 2>&1 | tail -30`.
  - **Read the distribution.** Target: median figure count in 3–9, no seed with 0 figures in more than ~20% of worlds, ecliptic figures ≥ 1 in most worlds. Adjust `FIGURE_SEPARATION_DEG` / `FIGURE_MAGNITUDE_FLOOR` / `FIGURE_MIN_MEMBERS`, re-run, repeat until inside target; record the chosen values and the final distribution numbers in the commit message. If artifacts drift on the final values, rebaseline in-commit.
  - Commit: `feat(astronomy,lab): figure thresholds frozen by census (median N figures; the homophony lesson applied)`.

### Task 11: The campaign batteries and close

**Files:**
- Create: `domains/astronomy/tests/night_sky_regimes.rs`
- Modify: `book/src/frontier/idea-registry.md`, `book/src/chronicle/` (new entry), campaign retro in `docs/retrospectives/`, the astronomy model-card section of the Campaign 2 spec's model card home (follow where SKY-6/7 recorded theirs)

- [ ] **Step 1: The regime × obliquity matrix battery** (spec §7) — `night_sky_regimes.rs`, one test per row over seeds `[1, 7, 42]`:

```rust
//! The regime × feature matrix (spec §6–7): every stage-1 derivation
//! returns the regime-honest answer. Cheap enough for the commit gate
//! (3 seeds × 4 regimes; no live worldgen batteries).
```

  - `locked_worlds_freeze_the_instrument`: locked pin → `night_sky_at(...).frozen`, `heliacal_events(...).is_empty()`, `sky_band(...) == None`.
  - `zero_obliquity_keeps_heliacal_events_but_kills_seasons`: `obliquity=0` + `forcing=zero` + 24h rotation → `solar_equatorial(t).dec_deg ≈ 0` all year; `heliacal_events` at latitude 35 still nonempty for a riser (RA still moves); `season_phase` behavior unchanged from its shipped contract.
  - `retrograde_flips_wheeling_not_dates`: prograde vs retrograde pins → `wheels_backward` differs; `heliacal_events` pairs equal to 1e-9 (spin direction never changes timing, SKY-22's rule).
  - `epoch_drift_moves_the_equinox_referenced_and_spares_the_orbital` (spec §7): at `t = 0` vs `t = P_PRECESSION/4`: every neighbor's apparent RA moved > 1°; pole-star separation (when either epoch has one) differs; every wanderer's `period` and the calendar's `year_length` byte-identical — the timescale matrix's rule.
- [ ] **Step 2:** Run the new battery + `make gate 2>&1 | tail -20` — everything green.
- [ ] **Step 3: Registry and book (Definition of Done).** Flip SKY-9, SKY-12, SKY-seasonal-night-sky, SKY-heliacal-risings, SKY-circumpolar to `shipped` with one-sentence result summaries; update (not flip) SKY-stale-alignments (sky half shipped); file four new deferred rows (wanderer-synodic calendar; wanderer transits/occultations; per-species figure catalogs; periodic variable stars). Model card additions: precession/coordinate model, arcus-visionis table, wanderer bands/brightness/count weights + the elongation-phase approximation, starfield magnitude weights, figure thresholds (with the census numbers), `TWILIGHT_DEPTH_DEG`, `POLE_STAR_MAX_SEPARATION_DEG` — each labeled drawn/derived/approximated. Chronicle entry + freshness sweep of the sky chapters + Confidence Gradient re-score if a bet moved. Retro in `docs/retrospectives/`.
- [ ] **Step 4: Close** via the `closing-a-campaign` skill (final preflight, absorb, merge to main, worktree removal). Censuses refresh on AWS only, with warning to Nathan first, per house rule — the figures census study is a dev instrument and does NOT join the committed census fixtures this campaign.

---

## Self-Review Notes (run at execution time too)

- Spec coverage: §2 → Tasks 1–5; §3 → 6–8; §4 → 9–10; §5 trace-protocol law → Tasks 4 (phenomena), 5/8/10 (facts), 3 (view); §6 regimes → Task 11; §7 tests → per-task + Task 11; §8 close → Task 11; §9 boundaries → Tasks 5/8 stage-close steps.
- Type consistency: `EquatorialCoord`/`EclipticCoord` (Task 1) are the coordinate types everywhere; `night_sky_at` is a free function (not an inherent `NightSky::at`) — almanac and tests call `night_sky_at`; heliacal fractions are year-phase fractions, not day indices.
- Known judgment points for the implementer: the exact provider.rs insertion sites (twilight ~1026, night emission ~1055) shift with the parallel campaign's merges — anchor on the quoted comments, not line numbers; the almanac-context assembly site is located by grep in Tasks 5/8; the inner-wanderer elongation phase is a declared approximation and must be documented in the model card, not silently embedded.
