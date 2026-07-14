# Eclipse Seasons Implementation Plan

> **Closed 2026-07-14.** All 12 tasks complete (progress ledger:
> `.superpowers/sdd/progress.md`); the final whole-branch review (opus,
> `334faa1..0785e54`) came back Approved — ready to merge, only the
> pre-authorized AWS census regen standing between the tree and fully green.
> Close (Definition of Done) artifacts — chronicle entry, retrospective,
> registry flips, freshness sweep — land in this same commit.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn SKY-6's statistical eclipse rate into dated solar and lunar eclipse events with node geometry, ground tracks, the recurrence ladder (eclipse year / saros / exeligmos / series / parade), placed possess prose, ledger facts, an almanac Eclipses section, and four census metrics.

**Architecture:** One new drawn quantity (per-moon ascending-node longitude, stream `moon-nodes`, drawn after every existing draw); everything else is a pure derivation. A new `domains/astronomy/src/eclipses.rs` enumerates syzygies closed-form (no scanning); `provider.rs` keeps the shipped rate phenomenon byte-identical as the coarse tier and gains a lunar sibling plus placed event-day prose; `windows/worldgen` feeds a new almanac section; `windows/lab` gains four metrics.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only. All transcendentals through `hornvale_kernel::math` (libm, decision 0041). No `HashMap`/`HashSet`. No new crates.

**Spec:** `docs/superpowers/specs/2026-07-14-eclipse-seasons-design.md`

**Provenance / reconciliation:** Nathan adjudicated the same-day collision with The Long Count: *Eclipse Seasons owns the eclipse core* (see the scope amendments in both specs). Tasks 1, 2, 4, 5, and 6 here adopt the original Long Count plan's eclipse tasks (its pre-rescope Tasks 1–5) with our spec's deltas — event-time sun size, our fact naming, and the deeper consumers. The Long Count plan has since been re-scoped to the non-eclipse residuals (@657cc97) and carries a hands-off constraint on this campaign's surface; coordinate the one shared census regen at close.

## Global Constraints

- **Determinism is constitutional.** Same seed → byte-identical worlds. The new stream is `"moon-nodes"`, drawn per admitted moon *after* the `moon-inclinations` draws; no existing draw, constant, or formula may change. `golden_seed_42.rs`'s existing assertions must pass untouched until Task 6 deliberately extends them.
- **`f64` transcendentals** (`sin`, `cos`, `asin`, `acos`, `atan2`, `powf`, `tan`) go through `hornvale_kernel::math`, never `f64::sin` etc. (`sqrt`, `abs`, `floor`, `ceil`, `rem_euclid`, `to_radians`, `to_degrees` are IEEE-exact and stay as std methods — match surrounding code.)
- **Quantization at emit only:** facts are quantized by `Ledger::commit`; never quantize inside compute paths.
- **Float ordering:** `total_cmp` with a deterministic tie-break.
- **Every crate has `#![warn(missing_docs)]`** — every new public item, field, and variant gets a one-line doc comment, and every new pub-boundary primitive gets a `type-audit:` tag (`pending(wave-1)` for physical quantities, `bare-ok(ratio)`/`bare-ok(identifier-text)`/`bare-ok(index)`/`bare-ok(flag)` per class — copy the tag style of the adjacent field).
- **No wall-clock time.** Time is `StdDays` / `WorldTime { day: f64 }`.
- **The coarse tier is untouchable:** the shipped solar rate phenomenon (`eclipse_chance` path in `provider.rs`) must stay byte-identical through every task; its tests are the tripwire.
- **Evaluate at t, never cache:** angular sizes and rates are functions of the event's own time (the SKY-tidal-braking seam, spec §3).
- **Gate cadence:** during a task iterate with scoped tests (`cargo test -p hornvale-astronomy <name>`); before each task's commit run `cargo fmt` then `make gate-fast`; run the full `make gate` before committing Tasks 6, 10, and 12. Never `--no-verify`.
- **Worktree:** execute in a worktree at `~/.config/superpowers/worktrees/hornvale/eclipse-seasons/`, branch `eclipse-seasons` (superpowers:using-git-worktrees). SDD scratch lives in the worktree's `.superpowers/sdd/`, never the main checkout.
- **Absorb main at stage boundaries** (`make preflight`): The Long Count executes in parallel and touches neighboring lines in `provider.rs`, `facts.rs`, `lib.rs`, `NightSkyLines`, and `metrics.rs` (it adds `alignment`; we add `eclipses`). Merge main into the branch at every task-group boundary; the guards (not mid-measurement, not while main looks mid-landing) apply.
- **Censuses are never regenerated locally.** Artifact regeneration uses `SKIP_CENSUS=1`. The one census regen happens on AWS pre-merge with Nathan's explicit go-ahead (close-time, outside this plan's tasks).

---

### Task 1: The node-longitude draw

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (append)
- Modify: `domains/astronomy/src/lib.rs` (`stream_labels()`)
- Modify: `domains/astronomy/src/moons.rs` (field + draw + tests)
- Modify: `domains/astronomy/tests/genesis_properties.rs` (pin isolation)

**Interfaces:**
- Consumes: `streams::MOON_INCLINATIONS` pattern (moons.rs:131-137).
- Produces: `Moon.node_longitude_deg: f64` in `[0, 360)`; `streams::MOON_NODES: &str = "moon-nodes"`. Every later task reads `moon.node_longitude_deg`.

- [ ] **Step 1: Write the failing test** — in `domains/astronomy/src/moons.rs` `mod tests`, mirroring `every_moon_draws_an_inclination_in_range`:

```rust
    /// Eclipse Seasons: every admitted moon draws an ascending-node
    /// longitude in [0, 360)°, deterministically, from its own stream —
    /// the pre-node draws are pinned unchanged by `golden_seed_42.rs`.
    #[test]
    fn every_moon_draws_a_node_longitude_in_range() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for moon in &moons {
                assert!(
                    (0.0..360.0).contains(&moon.node_longitude_deg),
                    "seed {seed}: node {}",
                    moon.node_longitude_deg
                );
            }
        }
    }
```

- [ ] **Step 2: Run it, verify it fails to compile** (no such field):
  `cargo test -p hornvale-astronomy every_moon_draws_a_node_longitude` → error: no field `node_longitude_deg`.

- [ ] **Step 3: Implement.** In `streams.rs`, append after `STARFIELD`:

```rust
/// Per-moon ascending-node longitude draws (Eclipse Seasons).
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODES: &str = "moon-nodes";
```

In `moons.rs`, add to `Moon` (after `inclination_deg`):

```rust
    /// Ecliptic longitude of the ascending node at genesis, in degrees
    /// (drawn 0–360, own stream — Eclipse Seasons): with the inclination,
    /// the full node geometry that dates each eclipse.
    /// type-audit: pending(wave-1)
    pub node_longitude_deg: f64,
```

In `derive_moon`, initialize it exactly like `inclination_deg`:

```rust
        // Drawn after admission from its own stream (Eclipse Seasons),
        // like the inclination above it.
        node_longitude_deg: 0.0,
```

In `generate_moons`, immediately after the inclination loop (moons.rs:134-137):

```rust
    // Eclipse Seasons: node longitudes draw from their own stream, after
    // the inclinations, so every pre-node draw is byte-identical and the
    // draws are index-stable.
    let mut nodes = astronomy_seed.derive(streams::MOON_NODES).stream();
    for moon in &mut moons {
        moon.node_longitude_deg = nodes.next_f64() * 360.0;
    }
```

In `lib.rs`, find `stream_labels()` and add `streams::MOON_NODES` to the returned list in the position matching the streams.rs declaration order. Fix any other `Moon { .. }` struct literals the compiler flags (tests) with `node_longitude_deg: 0.0`.

- [ ] **Step 4: Write and pass the pin-isolation test** — in `domains/astronomy/tests/genesis_properties.rs`, next to the existing moon pin-isolation battery (mirror the inclination test's structure exactly):

```rust
/// Eclipse Seasons pin isolation: pinning the moon count consumes node
/// draws identically to the unpinned path — a pinned world with the same
/// admitted moons carries byte-identical node longitudes.
#[test]
fn pinned_moon_counts_draw_identical_node_longitudes() {
    for seed in 0..32u64 {
        let unpinned = generate(Seed(seed), &SkyPins::default()).unwrap().system;
        let n = unpinned.moons.len() as u32;
        if n == 0 {
            continue;
        }
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(n).unwrap()),
            ..SkyPins::default()
        };
        let pinned = generate(Seed(seed), &pins).unwrap().system;
        let a: Vec<f64> = unpinned.moons.iter().map(|m| m.node_longitude_deg).collect();
        let b: Vec<f64> = pinned.moons.iter().map(|m| m.node_longitude_deg).collect();
        assert_eq!(a, b, "seed {seed}");
    }
}
```

(Match the file's existing imports — `generate`, `Seed`, `SkyPins`, `MoonsPin` are already in use by the neighboring pin-isolation tests.)

- [ ] **Step 5: Run:** `cargo test -p hornvale-astronomy moons && cargo test -p hornvale-astronomy --test genesis_properties && cargo test -p hornvale-astronomy --test golden_seed_42` → PASS (golden pins prove the pre-node draws didn't move).

- [ ] **Step 6: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): per-moon ascending-node longitude draw (Eclipse Seasons)"
```

---

### Task 2: Node motion and the moon's ecliptic latitude (`eclipses.rs` core)

**Files:**
- Create: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (add `pub mod eclipses;` + re-exports)
- Modify: `domains/astronomy/src/provider.rs:949-969` (move the threshold constants; delegate)

**Interfaces:**
- Consumes: `Moon` (Task 1), `Calendar::{moon_phase, year_phase, year_length, synodic_month}`, `hornvale_kernel::math`.
- Produces (all in `hornvale_astronomy::eclipses`):
  - `pub const ANGULAR_UNIT_DEG: f64 = 0.53;` and `pub const ECLIPSE_PARALLAX_DEG: f64 = 1.0;` (moved verbatim from provider.rs)
  - `pub fn solar_eclipse_threshold_deg(sun_angular_rel: f64, moon_angular_rel: f64) -> f64`
  - `pub fn node_crossing_chance(threshold_deg: f64, inclination_deg: f64) -> f64`
  - `pub fn node_regression_period(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays`
  - `pub fn node_longitude_at(moon: &Moon, year: StdDays, t: StdDays) -> f64`
  - `pub fn moon_ecliptic_latitude_deg(calendar: &Calendar, moon: &Moon, index: usize, t: StdDays) -> Option<f64>`
  - `#[cfg(test)] pub(crate) fn luna_sol() -> (StarSystem, Calendar)` — the calibration fixture every later task reuses.

- [ ] **Step 1: Write the failing tests** — create `eclipses.rs` with module doc + tests only:

```rust
//! Dated eclipses (Eclipse Seasons, SKY-6 close-out): node geometry as a
//! function of `WorldTime`. The node longitude is drawn; its regression
//! period, the moon's ecliptic latitude, and every dated event are pure
//! derivations (model card: the lunar-theory leading term).

use crate::calendar::Calendar;
use crate::moons::Moon;
use crate::units::StdDays;
use hornvale_kernel::math;

#[cfg(test)]
mod tests {
    use super::*;

    /// Earth check for the regression period: Y=365.25, P_sid=27.32,
    /// i=5.14° gives ~6540 days (~17.9 yr) against the true 18.61 yr —
    /// the declared approximation's accuracy band.
    #[test]
    fn node_regression_reproduces_the_lunar_magnitude() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        assert!((6000.0..7000.0).contains(&p.0), "P_node {} days", p.0);
    }

    #[test]
    fn nodes_regress_westward_one_turn_per_period() {
        let moon = test_moon(5.14, 40.0);
        let year = StdDays(365.25);
        let p = node_regression_period(year, moon.period, moon.inclination_deg);
        let start = node_longitude_at(&moon, year, StdDays(0.0));
        assert_eq!(start, 40.0);
        let quarter = node_longitude_at(&moon, year, StdDays(p.0 / 4.0));
        assert!(((start - quarter).rem_euclid(360.0) - 90.0).abs() < 1e-6);
        let full = node_longitude_at(&moon, year, StdDays(p.0));
        assert!((full - start).rem_euclid(360.0) < 1e-6);
    }

    #[test]
    fn ecliptic_latitude_is_bounded_by_the_inclination() {
        let (system, calendar) = super::luna_sol();
        let moon = &system.moons[0];
        for k in 0..500 {
            let t = StdDays(k as f64 * 13.7);
            let b = moon_ecliptic_latitude_deg(&calendar, moon, 0, t).unwrap();
            assert!(b.abs() <= moon.inclination_deg + 1e-9, "β {b} at t {}", t.0);
        }
    }

    fn test_moon(inclination_deg: f64, node_longitude_deg: f64) -> Moon {
        use crate::units::{LunarMasses, Megameters};
        Moon {
            mass: LunarMasses(1.0),
            distance: Megameters(384.4),
            period: StdDays(27.32),
            angular_diameter_rel: 1.0,
            tide_rel: 1.0,
            inclination_deg,
            node_longitude_deg,
        }
    }
}

/// Sol + Luna exactly: 1 M☉, 1 AU, 365.25-day year, 24-hour day, zero
/// forcing, one moon at Luna's elements with node at 0°. The calibration
/// fixture Tasks 3–11 reuse — module scope (not inside `mod tests`) so
/// `provider.rs`'s consistency test can reach it as
/// `crate::eclipses::luna_sol()`.
#[cfg(test)]
pub(crate) fn luna_sol() -> (crate::system::StarSystem, Calendar) {
    use crate::calendar::calendar_of;
    use crate::pins::{ForcingPin, MoonsPin, RotationPin, SkyPins};
    use crate::system::generate;
    use crate::units::{Au, LunarMasses, Megameters, SolarLuminosities, SolarMasses};
    use hornvale_kernel::Seed;
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        moons: Some(MoonsPin::exact(1).unwrap()),
        forcing: Some(ForcingPin::Zero),
        ..SkyPins::default()
    };
    let mut system = generate(Seed(42), &pins).unwrap().system;
    // Force Sol/Luna numbers onto the generated skeleton.
    system.star.mass = SolarMasses(1.0);
    system.star.luminosity = SolarLuminosities(1.0);
    system.anchor.orbit = Au(1.0);
    system.anchor.year = StdDays(365.25);
    system.moons[0] = Moon {
        mass: LunarMasses(1.0),
        distance: Megameters(384.4),
        period: StdDays(27.32),
        angular_diameter_rel: 1.0,
        tide_rel: 1.0,
        inclination_deg: 5.14,
        node_longitude_deg: 0.0,
    };
    let calendar = calendar_of(&system);
    (system, calendar)
}
```

(If any unit newtype above rejects tuple construction from the test module — they are `pub struct X(pub f64)` in-crate today per `moons.rs`/`heliacal.rs` usage — use its `::new(x).unwrap()` constructor instead; do the same everywhere this plan constructs units. If `ForcingPin` has no `Zero` variant, check `pins.rs` for the zero-forcing pin the forcing tests use and substitute it; if none exists, build the system unpinned and overwrite `system.forcing` fields to zero amplitudes/offsets directly.)

Register the module in `lib.rs` (alphabetical position, after `calendar`):

```rust
pub mod eclipses;
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy eclipses` → compile errors: functions not defined.

- [ ] **Step 3: Implement** — in `eclipses.rs` above the tests:

```rust
/// One angular-diameter unit (Sol from 1 AU ≈ Luna from Earth) in degrees
/// — the shared scale of `sun_angular_diameter_rel` and a moon's
/// `angular_diameter_rel` (declared approximation: the two units differ
/// by under 1%). Moved here from `provider.rs` (Eclipse Seasons).
pub const ANGULAR_UNIT_DEG: f64 = 0.53;
/// How far (degrees of lunar ecliptic latitude) past the discs' own touch
/// an eclipse still falls somewhere on the world — the parallax allowance.
/// Calibrated so a Luna–Sol pair at 5.14° inclination eclipses at ~19% of
/// new moons (Earth's ~2.4 solar eclipses a year).
pub const ECLIPSE_PARALLAX_DEG: f64 = 1.0;

/// The node threshold (degrees of lunar ecliptic latitude) inside which a
/// new moon eclipses the sun somewhere on the world: half the summed
/// discs plus the parallax allowance.
/// type-audit: pending(wave-1)
pub fn solar_eclipse_threshold_deg(sun_angular_rel: f64, moon_angular_rel: f64) -> f64 {
    ANGULAR_UNIT_DEG * (sun_angular_rel + moon_angular_rel) / 2.0 + ECLIPSE_PARALLAX_DEG
}

/// The fraction of syzygies whose ecliptic latitude falls inside
/// `threshold_deg` for an orbit inclined `inclination_deg`: a sinusoidal
/// latitude distribution gives P = (2/π)·asin(threshold / i), saturating
/// at 1 for a flat orbit. (The statistical twin of the dated scan —
/// `rate_matches_the_dated_scan` in provider.rs holds them together.)
/// type-audit: bare-ok(ratio)
pub fn node_crossing_chance(threshold_deg: f64, inclination_deg: f64) -> f64 {
    let x = (threshold_deg / inclination_deg.max(f64::MIN_POSITIVE)).min(1.0);
    (2.0 / std::f64::consts::PI) * math::asin(x)
}

/// Nodal regression period from the lunar-theory leading term (declared
/// approximation, model card): P_node = (4/3)·Y²/(P_sid·cos i). Earth
/// check: ~17.9 yr against the true 18.61.
/// type-audit: pending(wave-1: inclination_deg)
pub fn node_regression_period(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays {
    StdDays(
        (4.0 / 3.0) * year.0 * year.0
            / (sidereal.0 * math::cos(inclination_deg.to_radians())),
    )
}

/// The ascending node's ecliptic longitude at `t`, degrees in [0, 360):
/// the genesis draw regressed westward one turn per regression period.
/// type-audit: pending(wave-1)
pub fn node_longitude_at(moon: &Moon, year: StdDays, t: StdDays) -> f64 {
    let p = node_regression_period(year, moon.period, moon.inclination_deg);
    (moon.node_longitude_deg - 360.0 * t.0 / p.0).rem_euclid(360.0)
}

/// The moon's ecliptic latitude at `t`, degrees (small-angle inclined-orbit
/// form, exact at the syzygies where it is consumed): β = i·sin(L−Ω), with
/// L = L_sun + 360·phase reusing the shipped phase machinery. `None` if
/// the moon has no synodic cycle (degenerate P_sid ≥ Y).
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_latitude_deg(
    calendar: &Calendar,
    moon: &Moon,
    index: usize,
    t: StdDays,
) -> Option<f64> {
    let phase = calendar.moon_phase(t, index)?;
    let l_sun = 360.0 * calendar.year_phase(t);
    let l_moon = l_sun + 360.0 * phase;
    let omega = node_longitude_at(moon, calendar.year_length(), t);
    Some(moon.inclination_deg * math::sin((l_moon - omega).to_radians()))
}
```

In `provider.rs`: delete the two consts at lines 949-958 and rewrite `eclipse_chance` (lines 960-969) as a delegating wrapper (its own tests stay green):

```rust
/// The fraction of new moons that eclipse the sun somewhere on the world
/// (SKY-6) — the threshold and chance now live in `eclipses.rs` (Eclipse
/// Seasons); this wrapper keeps the phenomenon path reading as before.
fn eclipse_chance(sun_angular_rel: f64, moon_angular_rel: f64, inclination_deg: f64) -> f64 {
    crate::eclipses::node_crossing_chance(
        crate::eclipses::solar_eclipse_threshold_deg(sun_angular_rel, moon_angular_rel),
        inclination_deg,
    )
}
```

In `lib.rs`, add the re-export:

```rust
pub use eclipses::{
    moon_ecliptic_latitude_deg, node_crossing_chance, node_longitude_at,
    node_regression_period, solar_eclipse_threshold_deg,
};
```

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses && cargo test -p hornvale-astronomy provider` → PASS (the provider eclipse tests prove the threshold move is byte-neutral).

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): nodal regression and moon ecliptic latitude (Eclipse Seasons)"
```

---

### Task 3: The sun's angular size at t (eccentricity coupling)

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (extend the re-export)

**Interfaces:**
- Consumes: `crate::star::sun_angular_diameter_rel`, `OrbitalForcing::eccentricity_at`, `Calendar::year_phase`.
- Produces: `pub fn sun_angular_rel_at(system: &StarSystem, calendar: &Calendar, t: StdDays) -> f64` — Task 4 reads it per-syzygy; the total/annular decision becomes a property of the event's own day.

- [ ] **Step 1: Write the failing tests** (in `eclipses.rs` `mod tests`):

```rust
    /// With zero eccentricity the event-time sun is the mean sun exactly.
    #[test]
    fn a_circular_orbit_keeps_the_mean_sun() {
        let (system, calendar) = super::luna_sol();
        let mean =
            crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        for k in 0..12 {
            let t = StdDays(k as f64 * 30.0);
            assert_eq!(sun_angular_rel_at(&system, &calendar, t), mean);
        }
    }

    /// Sol check: e = 0.0167 swings the apparent sun ±1.7% over the year,
    /// perihelion-largest.
    #[test]
    fn eccentricity_swings_the_sun_size_sol_scale() {
        let (mut system, _) = super::luna_sol();
        system.forcing.ecc_mean = 0.0167;
        system.forcing.ecc_amp = 0.0;
        let calendar = crate::calendar::calendar_of(&system);
        let mean =
            crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        let sizes: Vec<f64> = (0..360)
            .map(|d| sun_angular_rel_at(&system, &calendar, StdDays(d as f64)))
            .collect();
        let max = sizes.iter().cloned().fold(f64::MIN, f64::max);
        let min = sizes.iter().cloned().fold(f64::MAX, f64::min);
        assert!((max / mean - 1.017).abs() < 2e-3, "max ratio {}", max / mean);
        assert!((min / mean - 0.983).abs() < 2e-3, "min ratio {}", min / mean);
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy sun_angular_rel_at` → compile error: not defined.

- [ ] **Step 3: Implement:**

```rust
use crate::system::StarSystem;

/// The sun's apparent angular diameter (Luna-units) at `t`: the mean
/// orbital value scaled by the instantaneous star distance from the live
/// eccentricity. First-order: r/a = 1 − e·sin(2π·year-phase) — the same
/// perihelion convention the apsidal daylight term already uses
/// (insolation peaks at year phase 0.25). Declared approximation (model
/// card); evaluated at the event, never cached (the tidal-braking seam).
/// type-audit: pending(wave-1)
pub fn sun_angular_rel_at(system: &StarSystem, calendar: &Calendar, t: StdDays) -> f64 {
    let mean = crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
    let e = system.forcing.eccentricity_at(t.0);
    mean / (1.0 - e * math::sin(std::f64::consts::TAU * calendar.year_phase(t)))
}
```

(The `use crate::system::StarSystem;` line lands here if Task 4 hasn't added it yet.) Add `sun_angular_rel_at` to the `lib.rs` eclipses re-export.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): event-time solar angular size from live eccentricity"
```

---

### Task 4: Dated solar eclipse events

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (extend the eclipses re-export)

**Interfaces:**
- Consumes: Tasks 2–3; `StarSystem` fields.
- Produces:
  - `pub enum EclipseBody { Solar, Lunar }`, `pub enum EclipseKind { Total, Annular }`
  - `pub struct EclipseEvent { pub day: StdDays, pub moon: usize, pub body: EclipseBody, pub kind: EclipseKind }`
  - `pub fn eclipse_events(system: &StarSystem, calendar: &Calendar, from: StdDays, until: StdDays) -> Vec<EclipseEvent>` — day-ascending, tie-broken by moon index. (Task 5 adds the Lunar arm; until then the function emits Solar only.)

- [ ] **Step 1: Write the failing tests** (in `eclipses.rs` `mod tests`):

```rust
    /// Luna–Sol calibration: the dated scan reproduces Earth's ~2.4 solar
    /// eclipses/year over a 50-year window (the anywhere-on-the-world
    /// count the parallax allowance is calibrated to).
    #[test]
    fn luna_sol_dates_earths_solar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * years));
        let solar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
            .count() as f64;
        let per_year = solar / years;
        assert!((1.9..=2.9).contains(&per_year), "solar eclipses/year {per_year}");
    }

    /// Every dated solar event sits at a new moon and inside the node
    /// threshold — the geometry cannot lie.
    #[test]
    fn solar_events_fall_at_new_moons_inside_the_threshold() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
        assert!(!events.is_empty());
        for e in events.iter().filter(|e| matches!(e.body, EclipseBody::Solar)) {
            let moon = &system.moons[e.moon];
            let phase = calendar.moon_phase(e.day, e.moon).unwrap();
            let off_new = phase.min(1.0 - phase);
            assert!(off_new < 1e-6, "phase {phase} at day {}", e.day.0);
            let beta = moon_ecliptic_latitude_deg(&calendar, moon, e.moon, e.day).unwrap();
            let threshold = solar_eclipse_threshold_deg(
                sun_angular_rel_at(&system, &calendar, e.day),
                moon.angular_diameter_rel,
            );
            assert!(beta.abs() < threshold, "β {beta} vs θ {threshold}");
        }
    }

    /// A flat orbit eclipses at every single new moon (SKY-6's shipped
    /// rate limit, now dated).
    #[test]
    fn a_flat_orbit_eclipses_every_new_moon() {
        let (mut system, _) = luna_sol();
        system.moons[0].inclination_deg = 1e-9;
        let calendar = crate::calendar::calendar_of(&system);
        let synodic = calendar.synodic_month(0).unwrap();
        let window = StdDays(synodic.0 * 24.0);
        let events = eclipse_events(&system, &calendar, StdDays(0.0), window);
        let solar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
            .count();
        assert!((23..=25).contains(&solar), "solar count {solar}");
    }

    /// Eclipse seasons exist: every solar event's sun sits within a
    /// bounded arc of the node line (Luna scale: asin(θ/i) ≈ 17°, so 25°
    /// with slack) — events cluster, they don't smear.
    #[test]
    fn solar_events_cluster_at_the_node_line() {
        let (system, calendar) = luna_sol();
        let moon = &system.moons[0];
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
        for e in events.iter().filter(|e| matches!(e.body, EclipseBody::Solar)) {
            let l_sun = 360.0 * calendar.year_phase(e.day);
            let omega = node_longitude_at(moon, calendar.year_length(), e.day);
            let arc = (l_sun - omega).rem_euclid(180.0).min(
                180.0 - (l_sun - omega).rem_euclid(180.0),
            );
            assert!(arc < 25.0, "sun {arc}° from the node line at day {}", e.day.0);
        }
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy eclipses` → compile errors: `EclipseEvent` etc. not defined.

- [ ] **Step 3: Implement** in `eclipses.rs`:

```rust
/// Which body is darkened.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseBody {
    /// A moon crosses the sun at new moon.
    Solar,
    /// The anchor's shadow crosses a full moon.
    Lunar,
}

/// How completely the body is darkened (binary this campaign; partiality
/// grading is explicitly deferred).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseKind {
    /// The covering disc (or shadow) swallows the body whole.
    Total,
    /// The moon's disc is too small: a burning ring remains.
    Annular,
}

/// One dated eclipse: a syzygy whose ecliptic latitude fell inside the
/// node threshold.
/// type-audit: bare-ok(index: moon)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseEvent {
    /// The syzygy, in absolute standard days.
    pub day: StdDays,
    /// Which moon (distance-sorted index into `StarSystem::moons`).
    pub moon: usize,
    /// Solar (new moon) or lunar (full moon).
    pub body: EclipseBody,
    /// Total or annular (kind read at the event's own t — a borderline
    /// moon is total near aphelion and annular near perihelion).
    pub kind: EclipseKind,
}

/// Every dated eclipse in `[from, until]`, day-ascending (moon index as
/// the deterministic tie-break). Syzygies are closed-form — the synodic
/// phase is linear in `t` — so the scan visits each new (and, Task 5,
/// full) moon exactly, no sampling. A moon with no synodic cycle never
/// eclipses. Thresholds and the total/annular kind are evaluated at each
/// syzygy's own time (Task 3), never cached.
pub fn eclipse_events(
    system: &StarSystem,
    calendar: &Calendar,
    from: StdDays,
    until: StdDays,
) -> Vec<EclipseEvent> {
    let mut out = Vec::new();
    for (index, moon) in system.moons.iter().enumerate() {
        let Some(synodic) = calendar.synodic_month(index) else {
            continue;
        };
        let Some(phase0) = calendar.moon_phase(StdDays(0.0), index) else {
            continue;
        };
        // Syzygy k of each family sits at t = (k + half − phase0)·synodic.
        for (half, body) in syzygy_families() {
            let k_min = (from.0 / synodic.0 + phase0 - half).ceil() as i64;
            let k_max = (until.0 / synodic.0 + phase0 - half).floor() as i64;
            for k in k_min..=k_max {
                let t = StdDays((k as f64 + half - phase0) * synodic.0);
                if t.0 < from.0 || t.0 > until.0 {
                    continue;
                }
                let Some(beta) = moon_ecliptic_latitude_deg(calendar, moon, index, t)
                else {
                    continue;
                };
                let sun_angular = sun_angular_rel_at(system, calendar, t);
                let theta_solar =
                    solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
                let threshold = match body {
                    EclipseBody::Solar => theta_solar,
                    EclipseBody::Lunar => theta_solar, // Task 5 narrows this arm
                };
                if beta.abs() < threshold {
                    let kind = match body {
                        EclipseBody::Lunar => EclipseKind::Total,
                        EclipseBody::Solar if moon.angular_diameter_rel >= sun_angular => {
                            EclipseKind::Total
                        }
                        EclipseBody::Solar => EclipseKind::Annular,
                    };
                    out.push(EclipseEvent { day: t, moon: index, body, kind });
                }
            }
        }
    }
    out.sort_by(|a, b| a.day.0.total_cmp(&b.day.0).then(a.moon.cmp(&b.moon)));
    out
}

/// The syzygy families the scan walks. Solar only until Task 5 adds the
/// full-moon (lunar) family.
fn syzygy_families() -> Vec<(f64, EclipseBody)> {
    vec![(0.0, EclipseBody::Solar)]
}
```

Extend the `lib.rs` re-export with `EclipseBody, EclipseEvent, EclipseKind, eclipse_events`.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): dated solar eclipse events by closed-form syzygy enumeration"
```

---

### Task 5: Lunar eclipses

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`

**Interfaces:**
- Consumes: Task 4's enumeration.
- Produces: `pub const LUNAR_SHADOW_FACTOR: f64 = 0.64;` and `Lunar` events from `eclipse_events`.

- [ ] **Step 1: Write the failing tests:**

```rust
    /// Luna–Sol calibration for the shadow: ~1.5 umbral lunar
    /// eclipses/year. (Fewer than solar — the solar count includes the
    /// anywhere-on-the-world parallax allowance; the lunar one is the
    /// same for every observer on the night side.)
    #[test]
    fn luna_sol_dates_earths_lunar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * years));
        let lunar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Lunar))
            .count() as f64;
        let per_year = lunar / years;
        assert!((1.1..=1.9).contains(&per_year), "lunar eclipses/year {per_year}");
    }

    /// Every lunar event sits at a full moon, and every lunar event is
    /// Total (partiality grading is deferred).
    #[test]
    fn lunar_events_fall_at_full_moons() {
        let (system, calendar) = luna_sol();
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
        let lunar: Vec<_> = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Lunar))
            .collect();
        assert!(!lunar.is_empty());
        for e in lunar {
            let phase = calendar.moon_phase(e.day, e.moon).unwrap();
            assert!((phase - 0.5).abs() < 1e-6, "phase {phase}");
            assert_eq!(e.kind, EclipseKind::Total);
        }
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy lunar` → FAIL (no lunar events emitted).

- [ ] **Step 3: Implement.** Add the constant:

```rust
/// The anchor's shadow threshold at the moon, as a fraction of the solar
/// threshold (declared approximation, Luna–Sol-calibrated to ~1.5 umbral
/// lunar eclipses/year — the umbra at Luna's distance is ≈ 2.6 lunar
/// radii). Below 1.0 because the solar threshold carries the
/// anywhere-on-the-world parallax allowance the lunar case doesn't need —
/// the shadow is one shadow for every observer.
pub const LUNAR_SHADOW_FACTOR: f64 = 0.64;
```

and widen `syzygy_families` to both arms, narrowing the Lunar threshold arm in `eclipse_events`:

```rust
fn syzygy_families() -> Vec<(f64, EclipseBody)> {
    vec![(0.0, EclipseBody::Solar), (0.5, EclipseBody::Lunar)]
}
```

```rust
                let threshold = match body {
                    EclipseBody::Solar => theta_solar,
                    EclipseBody::Lunar => LUNAR_SHADOW_FACTOR * theta_solar,
                };
```

If the cadence test lands outside 1.1–1.9/yr, tune `LUNAR_SHADOW_FACTOR` in 0.02 steps until it centers near 1.5 (the analytic value is 0.636), and record the final value in the model-card row (Task 12).

Add `LUNAR_SHADOW_FACTOR` to the `lib.rs` eclipses re-export.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS (both cadences).

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): dated lunar eclipses with a calibrated shadow threshold"
```

---

### Task 6: Facts, the lunar phenomenon, the rate guard, golden pins

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (two predicates + commits)
- Modify: `domains/astronomy/src/lib.rs` (`register_concepts`)
- Modify: `domains/astronomy/src/provider.rs` (lunar phenomenon + consistency test)
- Modify: `domains/astronomy/tests/tier_refinement.rs` (one assertion)
- Modify: `domains/astronomy/tests/golden_seed_42.rs` (pin the new draws)

**Interfaces:**
- Consumes: Tasks 1–5.
- Produces: predicates `facts::MOON_NODE_LONGITUDE_DEGREES = "moon-node-longitude-degrees"`, `facts::MOON_NODE_PERIOD_DAYS = "moon-node-period-days"`; per-moon facts in every generated ledger; a `Venue::NightSky` lunar `eclipse` phenomenon.

- [ ] **Step 1: Write the failing fact test** — in `facts.rs` `mod tests`, next to the existing per-moon fact tests (follow their fixture style — they build a world, run `genesis`, then filter the ledger):

```rust
    /// Eclipse Seasons: each moon commits its node longitude and its
    /// nodal-regression period.
    #[test]
    fn each_moon_commits_node_facts() {
        let (world, subject, outcome) = committed_world(42); // reuse/extract the module's existing world+genesis fixture
        let moons = outcome.system.moons.len();
        assert!(moons > 0);
        for predicate in [MOON_NODE_LONGITUDE_DEGREES, MOON_NODE_PERIOD_DAYS] {
            let count = world
                .ledger
                .facts()
                .iter()
                .filter(|f| f.subject == subject && f.predicate == predicate)
                .count();
            assert_eq!(count, moons, "{predicate}");
        }
    }
```

(If the module has no shared fixture named `committed_world`, extract one from the existing per-moon fact test rather than duplicating its body.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy each_moon_commits_node` → compile error: predicates not defined.

- [ ] **Step 3: Implement facts.** In `facts.rs`, after `MOON_INCLINATION_DEGREES` (line 47):

```rust
/// Predicate: a moon's ascending-node ecliptic longitude at genesis,
/// degrees (non-functional, Number — one per moon; Eclipse Seasons).
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODE_LONGITUDE_DEGREES: &str = "moon-node-longitude-degrees";
/// Predicate: a moon's nodal-regression period, standard days
/// (non-functional, Number — one per moon; Eclipse Seasons). The
/// standstill interpretation of this beat is the deferred follow-up row.
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODE_PERIOD_DAYS: &str = "moon-node-period-days";
```

In `genesis()`, inside the per-moon commit loop that already emits `MOON_INCLINATION_DEGREES` (add alongside, same subject and order):

```rust
        world.ledger.commit(
            fact(
                subject,
                MOON_NODE_LONGITUDE_DEGREES,
                Value::Number(moon.node_longitude_deg),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                MOON_NODE_PERIOD_DAYS,
                Value::Number(
                    crate::eclipses::node_regression_period(
                        system.anchor.year,
                        moon.period,
                        moon.inclination_deg,
                    )
                    .get(),
                ),
            ),
            &world.registry,
        )?;
```

In `lib.rs` `register_concepts`, after the `MOON_INCLINATION_DEGREES` registration (both non-functional, like every per-moon predicate):

```rust
    registry.register_predicate(
        facts::MOON_NODE_LONGITUDE_DEGREES,
        false,
        "ascending-node ecliptic longitude of a moon at genesis, in degrees",
    )?;
    registry.register_predicate(
        facts::MOON_NODE_PERIOD_DAYS,
        false,
        "nodal-regression period of a moon, in standard days",
    )?;
```

Also update the `ECLIPSE` phenomenon-kind doc string in `register_concepts` to cover both bodies:

```rust
    registry.register_phenomenon_kind(
        ECLIPSE,
        "a syzygy shadow: a moon crossing the sun, or the world's shadow crossing a moon",
    )?;
```

- [ ] **Step 4: Write the failing phenomenon test** — in `provider.rs` `mod tests`, next to `a_covering_moon_promises_total_eclipses_on_the_node_beat` (reuse its fixture pattern):

```rust
    /// Eclipse Seasons: each mooned sky promises lunar eclipses in the
    /// night sky — bloodred, salience 0.8, on the wider-shadow node beat.
    #[test]
    fn a_mooned_sky_promises_bloodred_lunar_eclipses_at_night() {
        let ph = phenomena_for_default_seed_42_at_night(); // reuse the fixture the solar eclipse tests use, night venue
        let lunar = ph
            .iter()
            .find(|p| p.kind == ECLIPSE && p.venue == Venue::NightSky)
            .expect("lunar eclipse phenomenon");
        assert!(lunar.description.contains("bloodred"), "got: {}", lunar.description);
        assert!((lunar.salience - 0.8).abs() < 1e-9);
        assert!(lunar.period_days.expect("recurs") > 0.0);
    }
```

- [ ] **Step 5: Implement the phenomenon.** In `provider.rs`, inside the `show_moons` block (after the per-moon `CELESTIAL_BODY` loop at lines 1333-1344):

```rust
            // Eclipse Seasons: lunar eclipses — the night twin of SKY-6's
            // solar rate, on the wider-shadow node beat, seen wherever
            // the full moon is seen.
            let sun_angular =
                crate::star::sun_angular_diameter_rel(&self.system.star, self.system.anchor.orbit);
            for (index, moon) in self.system.moons.iter().enumerate() {
                let Some(synodic) = self.calendar.synodic_month(index) else {
                    continue;
                };
                let threshold = crate::eclipses::LUNAR_SHADOW_FACTOR
                    * crate::eclipses::solar_eclipse_threshold_deg(
                        sun_angular,
                        moon.angular_diameter_rel,
                    );
                let chance =
                    crate::eclipses::node_crossing_chance(threshold, moon.inclination_deg);
                out.push(Phenomenon {
                    kind: ECLIPSE.to_string(),
                    description: format!(
                        "an eclipse of the moon: the full {} moon darkens to a bloodred coal",
                        size_word(moon.angular_diameter_rel)
                    ),
                    period_days: Some(round2(synodic.get() / chance)),
                    salience: 0.8,
                    venue: Venue::NightSky,
                });
            }
```

- [ ] **Step 6: Write and pass the rate-consistency guard** — in `provider.rs` tests (the campaign's constitutional test — coarse constrains fine):

```rust
    /// The rate model and the date model are two views of one geometry:
    /// the dated scan's long-window cadence must agree with the
    /// phenomenon's statistical period within 25%.
    #[test]
    fn rate_matches_the_dated_scan() {
        use crate::eclipses::{EclipseBody, eclipse_events};
        use crate::units::StdDays;
        let (system, calendar) = crate::eclipses::luna_sol();
        let years = 100.0;
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * years));
        let solar =
            events.iter().filter(|e| matches!(e.body, EclipseBody::Solar)).count() as f64;
        let scanned_period = 365.25 * years / solar;
        let sun_angular =
            crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        let moon = &system.moons[0];
        let chance = super::eclipse_chance(
            sun_angular,
            moon.angular_diameter_rel,
            moon.inclination_deg,
        );
        let rate_period = calendar.synodic_month(0).unwrap().get() / chance;
        let ratio = scanned_period / rate_period;
        assert!((0.75..=1.25).contains(&ratio), "ratio {ratio}");
    }
```

Add the lunar twin in the same test body or a sibling test: identical structure with `EclipseBody::Lunar` and `chance` computed from `LUNAR_SHADOW_FACTOR * solar_eclipse_threshold_deg(...)` via `node_crossing_chance`, same 25% band.

- [ ] **Step 7: Tier refinement + golden pins.** In `tier_refinement.rs`, inside the existing per-regime phenomena loop of `refinement_adds_structure_only_beneath_the_sun`, add:

```rust
        for p in phenomena.iter().filter(|p| p.kind == "eclipse") {
            assert!(p.salience < 1.0, "an eclipse never outranks the sun");
        }
```

In `golden_seed_42.rs`, extend the moons assertion to pin the new draws: run

`cargo test -p hornvale-astronomy --test golden_seed_42 -- --nocapture`

then add (with the actual printed values — capture them via a deliberately failing `assert_eq!`):

```rust
    let nodes: Vec<f64> = s.moons.iter().map(|m| m.node_longitude_deg).collect();
    assert_eq!(nodes, vec![/* the captured full-precision values */]);
```

- [ ] **Step 8: Run the crate + full gate:** `cargo test -p hornvale-astronomy 2>&1 | tee /tmp/hv-es-t6.txt` → PASS; then `cargo fmt && make gate 2>&1 | tail -20` → PASS. (Ledger-shape consumers — schema/census tests in the heavy tier — are not in the commit gate; census drift is handled at close.)

- [ ] **Step 9: Commit:**

```bash
git add -A && git commit -m "feat(astronomy): node facts, the bloodred lunar phenomenon, and the rate guard"
```

---

### Task 7: The ground track and who sees what

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (extend the re-export)

**Interfaces:**
- Consumes: Tasks 2–5; `Calendar::{solar_declination, local_day, is_retrograde, day_length}`.
- Produces:
  - `pub fn moon_ecliptic_longitude_deg(calendar: &Calendar, index: usize, t: StdDays) -> Option<f64>` (the ephemeris half transits/standstills will reuse)
  - `pub fn sub_solar_longitude_deg(calendar: &Calendar, t: StdDays) -> f64`
  - `pub struct GroundTrack { pub center_lat_deg: f64, pub half_width_deg: f64, pub start_lon_deg: f64, pub end_lon_deg: f64, pub duration_days: f64 }`
  - `pub fn ground_track(system: &StarSystem, calendar: &Calendar, event: &EclipseEvent) -> Option<GroundTrack>` (`None` for Lunar — the whole night side sees those)
  - `pub enum EclipseSight { WholeSun, BurningRing, Bitten, Unseen }`
  - `pub fn solar_eclipse_sight(system: &StarSystem, calendar: &Calendar, event: &EclipseEvent, latitude: f64, longitude: f64) -> EclipseSight`
  - `pub fn lunar_eclipse_seen(calendar: &Calendar, event: &EclipseEvent, longitude: f64) -> bool`

- [ ] **Step 1: Write the failing tests:**

```rust
    /// A central eclipse (β = 0) tracks the sub-solar latitude; a
    /// threshold-grazing one runs toward a pole.
    #[test]
    fn track_latitude_runs_from_subsolar_to_polar_with_beta() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 30.0));
        let moon = &system.moons[0];
        for e in events.iter().filter(|e| matches!(e.body, EclipseBody::Solar)) {
            let track = ground_track(&system, &calendar, e).unwrap();
            let beta = moon_ecliptic_latitude_deg(&calendar, moon, e.moon, e.day).unwrap();
            let dec = calendar.solar_declination(e.day);
            if beta.abs() < 0.1 {
                assert!((track.center_lat_deg - dec).abs() < 5.0, "central near dec");
            }
            assert!((-90.0..=90.0).contains(&track.center_lat_deg));
        }
    }

    /// Luna check: the shadow crossing lasts hours, not minutes or days.
    #[test]
    fn track_duration_is_hours_luna_scale() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
        let solar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Solar))
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        assert!(
            (0.03..0.3).contains(&track.duration_days),
            "duration {} days",
            track.duration_days
        );
    }

    /// Lunar events have no track — every night-side observer sees them.
    #[test]
    fn lunar_events_have_no_track_and_night_visibility() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
        let lunar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Lunar))
            .unwrap();
        assert!(ground_track(&system, &calendar, lunar).is_none());
        let ss = sub_solar_longitude_deg(&calendar, lunar.day);
        let night_lon = (ss + 180.0).rem_euclid(360.0) - 180.0;
        assert!(lunar_eclipse_seen(&calendar, lunar, night_lon));
        assert!(!lunar_eclipse_seen(&calendar, lunar, ss));
    }

    /// Sight tiers: in-band day-side sees the full omen, off-band day-side
    /// sees a bite, the night side sees nothing.
    #[test]
    fn sight_tiers_partition_the_globe() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
        let solar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Solar))
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        let ss = sub_solar_longitude_deg(&calendar, solar.day);
        let in_band = solar_eclipse_sight(&system, &calendar, solar, track.center_lat_deg, ss);
        assert!(matches!(in_band, EclipseSight::WholeSun | EclipseSight::BurningRing));
        let off_band_lat = if track.center_lat_deg > 0.0 {
            track.center_lat_deg - track.half_width_deg - 20.0
        } else {
            track.center_lat_deg + track.half_width_deg + 20.0
        };
        assert!(matches!(
            solar_eclipse_sight(&system, &calendar, solar, off_band_lat, ss),
            EclipseSight::Bitten
        ));
        let night_lon = (ss + 180.0).rem_euclid(360.0) - 180.0;
        assert!(matches!(
            solar_eclipse_sight(&system, &calendar, solar, track.center_lat_deg, night_lon),
            EclipseSight::Unseen
        ));
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy track` → compile errors.

- [ ] **Step 3: Implement:**

```rust
/// Half-width of the full-omen band, degrees of latitude (declared
/// approximation, model card: real totality bands are ~1° of latitude;
/// ours is slightly generous so a band is findable at room scale).
pub const TRACK_HALF_WIDTH_DEG: f64 = 2.0;

/// The moon's ecliptic longitude at `t`, degrees in [0, 360): the sun's
/// longitude plus the synodic phase (Eclipse Seasons — the positional
/// ephemeris half transits and standstills will reuse). `None` if the
/// moon has no synodic cycle.
/// type-audit: bare-ok(index: index), pending(wave-1: return)
pub fn moon_ecliptic_longitude_deg(
    calendar: &Calendar,
    index: usize,
    t: StdDays,
) -> Option<f64> {
    let phase = calendar.moon_phase(t, index)?;
    Some((360.0 * calendar.year_phase(t) + 360.0 * phase).rem_euclid(360.0))
}

/// The sub-solar longitude at `t`, degrees in [−180, 180): local noon of
/// the position-blind observer (day fraction 0.5) sits at longitude 0,
/// matching the locked convention (substellar point = prime meridian);
/// the sun sweeps westward on a prograde world, eastward on a retrograde
/// one (SKY-22). A locked world's sun is fixed at 0.
/// type-audit: pending(wave-1)
pub fn sub_solar_longitude_deg(calendar: &Calendar, t: StdDays) -> f64 {
    let Some((_, fraction)) = calendar.local_day(t) else {
        return 0.0;
    };
    let direction = if calendar.is_retrograde() { 1.0 } else { -1.0 };
    (direction * (fraction - 0.5) * 360.0 + 180.0).rem_euclid(360.0) - 180.0
}

/// A solar eclipse's shadow geometry: a latitude band swept across
/// longitudes as the world turns under the shadow.
/// type-audit: pending(wave-1: center_lat_deg), pending(wave-1: half_width_deg), pending(wave-1: start_lon_deg), pending(wave-1: end_lon_deg), pending(wave-1: duration_days)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroundTrack {
    /// Latitude of the band's center at mid-event, degrees: the sub-solar
    /// latitude displaced poleward by the ecliptic-latitude miss.
    pub center_lat_deg: f64,
    /// Half-width of the full-omen band, degrees of latitude.
    pub half_width_deg: f64,
    /// Sub-solar longitude when the crossing begins, degrees [−180, 180).
    pub start_lon_deg: f64,
    /// Sub-solar longitude when the crossing ends, degrees [−180, 180).
    pub end_lon_deg: f64,
    /// Crossing duration, standard days (the moon's synodic drift across
    /// the combined discs).
    pub duration_days: f64,
}

/// The ground track of a dated solar eclipse; `None` for a lunar event —
/// the anchor's shadow is one shadow for the whole night side. The
/// latitude mapping is a declared approximation (model card): center =
/// solar declination + (β/θ)·(90° − |declination|), so a central pass
/// tracks the sub-solar latitude and a threshold-grazing one exits at a
/// pole.
pub fn ground_track(
    system: &StarSystem,
    calendar: &Calendar,
    event: &EclipseEvent,
) -> Option<GroundTrack> {
    if event.body == EclipseBody::Lunar {
        return None;
    }
    let moon = &system.moons[event.moon];
    let beta = moon_ecliptic_latitude_deg(calendar, moon, event.moon, event.day)?;
    let sun_angular = sun_angular_rel_at(system, calendar, event.day);
    let theta = solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
    let dec = calendar.solar_declination(event.day);
    let center_lat_deg = (dec + (beta / theta) * (90.0 - dec.abs())).clamp(-90.0, 90.0);
    let synodic = calendar.synodic_month(event.moon)?;
    let combined_deg = ANGULAR_UNIT_DEG * (sun_angular + moon.angular_diameter_rel);
    let duration_days = combined_deg / (360.0 / synodic.0);
    let start_lon_deg =
        sub_solar_longitude_deg(calendar, StdDays(event.day.0 - duration_days / 2.0));
    let end_lon_deg =
        sub_solar_longitude_deg(calendar, StdDays(event.day.0 + duration_days / 2.0));
    Some(GroundTrack {
        center_lat_deg,
        half_width_deg: TRACK_HALF_WIDTH_DEG,
        start_lon_deg,
        end_lon_deg,
        duration_days,
    })
}

/// What a placed observer sees of a dated solar eclipse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EclipseSight {
    /// Inside the band under a covering moon: the sun devoured whole.
    WholeSun,
    /// Inside the band under a too-small moon: the burning ring.
    BurningRing,
    /// On the day side but outside the band: the sun is bitten.
    Bitten,
    /// On the night side: nothing.
    Unseen,
}

/// Which tier of the omen an observer at (`latitude`, `longitude`) sees
/// for a dated solar `event`. Day-side membership is the equatorial
/// half-day approximation: within 90° of the sub-solar longitude
/// (declared; matches the locked-culling geometry).
/// type-audit: pending(wave-1: latitude), pending(wave-1: longitude)
pub fn solar_eclipse_sight(
    system: &StarSystem,
    calendar: &Calendar,
    event: &EclipseEvent,
    latitude: f64,
    longitude: f64,
) -> EclipseSight {
    let ss = sub_solar_longitude_deg(calendar, event.day);
    let lon_gap = (longitude - ss + 180.0).rem_euclid(360.0) - 180.0;
    if lon_gap.abs() >= 90.0 {
        return EclipseSight::Unseen;
    }
    let Some(track) = ground_track(system, calendar, event) else {
        return EclipseSight::Unseen;
    };
    if (latitude - track.center_lat_deg).abs() <= track.half_width_deg {
        match event.kind {
            EclipseKind::Total => EclipseSight::WholeSun,
            EclipseKind::Annular => EclipseSight::BurningRing,
        }
    } else {
        EclipseSight::Bitten
    }
}

/// Whether an observer at `longitude` has the full moon in their sky for
/// a dated lunar `event`: the night side, the complement of the solar
/// half-day window.
/// type-audit: pending(wave-1: longitude), bare-ok(flag: return)
pub fn lunar_eclipse_seen(calendar: &Calendar, event: &EclipseEvent, longitude: f64) -> bool {
    let ss = sub_solar_longitude_deg(calendar, event.day);
    let lon_gap = (longitude - ss + 180.0).rem_euclid(360.0) - 180.0;
    lon_gap.abs() >= 90.0
}
```

Extend the `lib.rs` re-export with `EclipseSight, GroundTrack, ground_track, lunar_eclipse_seen, moon_ecliptic_longitude_deg, solar_eclipse_sight, sub_solar_longitude_deg, TRACK_HALF_WIDTH_DEG`.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): eclipse ground tracks and the sight tiers"
```

---

### Task 8: The recurrence ladder

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (extend the re-export)

**Interfaces:**
- Consumes: Tasks 2–5.
- Produces:
  - `pub fn draconic_month(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays`
  - `pub fn eclipse_year(year: StdDays, node_period: StdDays) -> StdDays`
  - `pub struct EclipseCycle { pub synodic_count: u32, pub draconic_count: u32, pub period: StdDays, pub node_slip_deg: f64 }`
  - `pub fn best_cycle(synodic: StdDays, draconic: StdDays) -> Option<EclipseCycle>`
  - `pub fn series_returns(cycle: &EclipseCycle, threshold_deg: f64, inclination_deg: f64) -> u32`
  - `pub fn parade_days_per_year(year: StdDays, eclipse_year: StdDays) -> f64`
  - `pub fn coincidence_days(events: &[EclipseEvent]) -> u32`

- [ ] **Step 1: Write the failing tests:**

```rust
    /// Luna check: the draconic month is ~27.21 days.
    #[test]
    fn draconic_month_matches_the_luna_check_value() {
        let d = draconic_month(StdDays(365.25), StdDays(27.32), 5.14);
        assert!((d.0 - 27.21).abs() < 0.05, "draconic {}", d.0);
    }

    /// Luna check: the eclipse year is ~346 days (our approximated node
    /// period gives ~345.9 against the true 346.62).
    #[test]
    fn eclipse_year_matches_the_luna_check_value() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        let ey = eclipse_year(StdDays(365.25), p);
        assert!((340.0..352.0).contains(&ey.0), "eclipse year {}", ey.0);
    }

    /// Fed TRUE Luna periods, the cycle search finds the saros: 223
    /// synodic ≈ 242 draconic ≈ 6585.3 days. (The derived pipeline's own
    /// draconic month differs enough — 17.9 vs 18.61 yr node period —
    /// that a world's best cycle may legitimately be an octon-class one;
    /// this test pins the *search*, the next pins the pipeline.)
    #[test]
    fn the_search_finds_the_true_saros_from_true_inputs() {
        let c = best_cycle(StdDays(29.5306), StdDays(27.2122)).unwrap();
        assert_eq!((c.synodic_count, c.draconic_count), (223, 242));
        assert!((c.period.0 - 6585.3).abs() < 1.0, "period {}", c.period.0);
    }

    /// The derived pipeline always yields *some* long-lived cycle: slip
    /// small enough that the family survives at least ten returns.
    #[test]
    fn luna_sol_pipeline_yields_a_living_cycle() {
        let (system, calendar) = luna_sol();
        let moon = &system.moons[0];
        let synodic = calendar.synodic_month(0).unwrap();
        let d = draconic_month(calendar.year_length(), moon.period, moon.inclination_deg);
        let c = best_cycle(synodic, d).unwrap();
        let sun_angular =
            crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        let theta = solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
        let returns = series_returns(&c, theta, moon.inclination_deg);
        assert!(returns >= 10, "returns {returns}");
        // Order-of-magnitude lifetime: centuries to a few millennia.
        let lifetime_years = returns as f64 * c.period.0 / 365.25;
        assert!(
            (50.0..20_000.0).contains(&lifetime_years),
            "lifetime {lifetime_years} yr"
        );
    }

    /// Luna check: the eclipse seasons parade backward through the civil
    /// year at ~19 days/year.
    #[test]
    fn the_parade_matches_the_luna_check_value() {
        let p = node_regression_period(StdDays(365.25), StdDays(27.32), 5.14);
        let ey = eclipse_year(StdDays(365.25), p);
        let parade = parade_days_per_year(StdDays(365.25), ey);
        assert!((15.0..25.0).contains(&parade), "parade {parade}");
    }

    /// Coincidence days: same-day events from different moons count once
    /// per day; a single-moon world scores zero.
    #[test]
    fn coincidence_days_needs_two_moons() {
        let (system, calendar) = luna_sol();
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 50.0));
        assert_eq!(coincidence_days(&events), 0);
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy cycle` → compile errors.

- [ ] **Step 3: Implement:**

```rust
/// The draconic month — the moon's period relative to its regressing
/// node: frequencies add because the node moves against the motion.
/// type-audit: pending(wave-1: inclination_deg)
pub fn draconic_month(year: StdDays, sidereal: StdDays, inclination_deg: f64) -> StdDays {
    let p_node = node_regression_period(year, sidereal, inclination_deg);
    StdDays(1.0 / (1.0 / sidereal.0 + 1.0 / p_node.0))
}

/// The eclipse year — the sun's return period to the (regressing) node
/// line: 1/(1/Y + 1/P_node). Luna check ≈ 346 days.
pub fn eclipse_year(year: StdDays, node_period: StdDays) -> StdDays {
    StdDays(1.0 / (1.0 / year.0 + 1.0 / node_period.0))
}

/// A near-commensurability between the synodic and draconic months: the
/// world's saros-analog. `node_slip_deg` is the draconic-phase miss per
/// return, in degrees — the drift that ages a series.
/// type-audit: bare-ok(index: synodic_count), bare-ok(index: draconic_count), pending(wave-1: node_slip_deg)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EclipseCycle {
    /// Synodic months per return.
    pub synodic_count: u32,
    /// Draconic months per return (the nearest integer).
    pub draconic_count: u32,
    /// The return period, standard days.
    pub period: StdDays,
    /// Node-phase slip per return, degrees.
    pub node_slip_deg: f64,
}

/// The longest-lived eclipse cycle up to 300 synodic months: the s
/// minimizing the draconic-phase miss (ties broken toward the shorter
/// cycle, deterministically). `None` for degenerate inputs.
pub fn best_cycle(synodic: StdDays, draconic: StdDays) -> Option<EclipseCycle> {
    if !(synodic.0 > 0.0) || !(draconic.0 > 0.0) {
        return None;
    }
    let ratio = synodic.0 / draconic.0;
    let mut best: Option<(f64, u32, u32)> = None;
    for s in 1..=300u32 {
        let x = s as f64 * ratio;
        let d = x.round();
        let miss = (x - d).abs();
        let better = match best {
            None => true,
            Some((b_miss, ..)) => miss < b_miss,
        };
        if better && d >= 1.0 {
            best = Some((miss, s, d as u32));
        }
    }
    let (miss, s, d) = best?;
    Some(EclipseCycle {
        synodic_count: s,
        draconic_count: d,
        period: StdDays(s as f64 * synodic.0),
        node_slip_deg: miss * 360.0,
    })
}

/// How many returns a series survives: the ecliptic-latitude walk per
/// return is i·sin(slip); a series is born grazing one edge of the ±θ
/// window and dies at the other, so it lives ≈ 2θ / Δβ returns. Slipless
/// cycles saturate at 10,000.
/// type-audit: pending(wave-1: threshold_deg), pending(wave-1: inclination_deg), bare-ok(index: return)
pub fn series_returns(cycle: &EclipseCycle, threshold_deg: f64, inclination_deg: f64) -> u32 {
    let d_beta = inclination_deg * math::sin(cycle.node_slip_deg.to_radians()).abs();
    if d_beta <= 0.0 {
        return 10_000;
    }
    ((2.0 * threshold_deg / d_beta).floor() as u32).min(10_000)
}

/// How many days per civil year the eclipse seasons migrate backward
/// through the calendar: Y − eclipse-year. Luna check ≈ 19.
/// type-audit: bare-ok(ratio)
pub fn parade_days_per_year(year: StdDays, eclipse_year: StdDays) -> f64 {
    year.0 - eclipse_year.0
}

/// How many distinct integer days in `events` carry events from two or
/// more different moons — the grand-omen coincidence count. Zero for
/// worlds with fewer than two moons, by construction.
/// type-audit: bare-ok(index: return)
pub fn coincidence_days(events: &[EclipseEvent]) -> u32 {
    let mut days: Vec<(i64, usize)> = events
        .iter()
        .map(|e| (e.day.0.floor() as i64, e.moon))
        .collect();
    days.sort();
    days.dedup();
    let mut count = 0u32;
    let mut idx = 0;
    while idx < days.len() {
        let day = days[idx].0;
        let mut moons = 0;
        while idx < days.len() && days[idx].0 == day {
            moons += 1;
            idx += 1;
        }
        if moons >= 2 {
            count += 1;
        }
    }
    count
}
```

Extend the `lib.rs` re-export with `EclipseCycle, best_cycle, coincidence_days, draconic_month, eclipse_year, parade_days_per_year, series_returns`.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): the eclipse recurrence ladder — draconic beat to series lifetime"
```

---

### Task 9: Placed event-day prose (possess/day-sky)

**Files:**
- Modify: `domains/astronomy/src/provider.rs` (`phenomena` + tests)

**Interfaces:**
- Consumes: Tasks 4–7 (`eclipse_events`, `solar_eclipse_sight`, `lunar_eclipse_seen`).
- Produces: dated, placed `eclipse` phenomena on event days only — `period_days: None`, venue `DaySky` (solar) / `NightSky` (lunar). The vessel consumes these through the existing observation seam; **no `windows/vessel` source changes** (the spec's promise).

- [ ] **Step 1: Write the failing tests** — in `provider.rs` `mod tests` (reuse the `ObserverContext::at_position` pattern from the locked-eclipse test at provider.rs:483-506):

```rust
    /// Eclipse Seasons: on an event day, a placed in-band observer gets a
    /// dated eclipse observation (no period — it is happening, not
    /// promised); the same observer a synodic month early gets none.
    #[test]
    fn an_in_band_observer_sees_the_eclipse_on_its_day() {
        use crate::eclipses::{EclipseBody, eclipse_events, ground_track,
            sub_solar_longitude_deg};
        use crate::units::StdDays;
        let (system, calendar) = crate::eclipses::luna_sol();
        let sky = GeneratedSky::new(crate::system::GenesisOutcome {
            system: system.clone(),
            notes: Vec::new(),
        });
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
        let solar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Solar))
            .unwrap();
        let track = ground_track(&system, &calendar, solar).unwrap();
        let ss = sub_solar_longitude_deg(&calendar, solar.day);
        let at = |day: f64| {
            let obs = ObserverContext::at_position(
                EntityId(0),
                WorldTime { day },
                GeoCoord { latitude: track.center_lat_deg, longitude: ss },
            );
            sky.phenomena(&obs)
        };
        let on_day = at(solar.day.0);
        assert!(
            on_day
                .iter()
                .any(|p| p.kind == ECLIPSE && p.period_days.is_none()),
            "event-day observation present"
        );
        let month_early = at(solar.day.0 - 29.5);
        assert!(
            !month_early
                .iter()
                .any(|p| p.kind == ECLIPSE && p.period_days.is_none()),
            "no dated observation off the day"
        );
    }

    /// The night side gets the blood moon on a lunar event day.
    #[test]
    fn the_night_side_sees_the_blood_moon_on_its_day() {
        use crate::eclipses::{EclipseBody, eclipse_events, sub_solar_longitude_deg};
        use crate::units::StdDays;
        let (system, calendar) = crate::eclipses::luna_sol();
        let sky = GeneratedSky::new(crate::system::GenesisOutcome {
            system: system.clone(),
            notes: Vec::new(),
        });
        let events =
            eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 10.0));
        let lunar = events
            .iter()
            .find(|e| matches!(e.body, EclipseBody::Lunar))
            .unwrap();
        let ss = sub_solar_longitude_deg(&calendar, lunar.day);
        let night_lon = (ss + 180.0).rem_euclid(360.0) - 180.0;
        let obs = ObserverContext::at_position(
            EntityId(0),
            WorldTime { day: lunar.day.0 },
            GeoCoord { latitude: 0.0, longitude: night_lon },
        );
        let ph = sky.phenomena(&obs);
        assert!(
            ph.iter().any(|p| p.kind == ECLIPSE
                && p.period_days.is_none()
                && p.description.contains("bloodred")),
            "blood-moon observation present"
        );
    }
```

(Adapt the `GeneratedSky::new` construction to the actual constructor if it takes the outcome differently — see provider.rs:1119; the existing tests show the idiom.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy an_in_band_observer` → FAIL (no dated observations).

- [ ] **Step 3: Implement.** In `phenomena`, after the coarse eclipse blocks, add the placed event-day pass (placed observers only — `position == None` keeps the legacy sky byte-identical):

```rust
        // Eclipse Seasons: dated, placed event-day observations. A
        // placed observer whose day contains a syzygy event sees it (or
        // its partial bite) at their own sight tier — no period, because
        // it is happening, not promised.
        if let Some(coord) = ctx.position {
            use crate::eclipses::{
                EclipseBody, EclipseSight, eclipse_events, lunar_eclipse_seen,
                solar_eclipse_sight,
            };
            let half_day = self
                .calendar
                .day_length()
                .map(|d| d.get() / 2.0)
                .unwrap_or(0.5);
            let window_from = crate::units::StdDays(t.0 - half_day);
            let window_until = crate::units::StdDays(t.0 + half_day);
            for event in eclipse_events(&self.system, &self.calendar, window_from, window_until)
            {
                let moon = &self.system.moons[event.moon];
                match event.body {
                    EclipseBody::Solar => {
                        let (description, salience) = match solar_eclipse_sight(
                            &self.system,
                            &self.calendar,
                            &event,
                            coord.latitude,
                            coord.longitude,
                        ) {
                            EclipseSight::WholeSun => (
                                format!(
                                    "the {} moon devours the sun whole",
                                    size_word(moon.angular_diameter_rel)
                                ),
                                0.95,
                            ),
                            EclipseSight::BurningRing => (
                                format!(
                                    "the {} moon leaves a burning ring of the sun",
                                    size_word(moon.angular_diameter_rel)
                                ),
                                0.85,
                            ),
                            EclipseSight::Bitten => (
                                "a dark bite taken from the sun's edge".to_string(),
                                0.6,
                            ),
                            EclipseSight::Unseen => continue,
                        };
                        out.push(Phenomenon {
                            kind: ECLIPSE.to_string(),
                            description,
                            period_days: None,
                            salience,
                            venue: Venue::DaySky,
                        });
                    }
                    EclipseBody::Lunar => {
                        if lunar_eclipse_seen(&self.calendar, &event, coord.longitude) {
                            out.push(Phenomenon {
                                kind: ECLIPSE.to_string(),
                                description: format!(
                                    "the full {} moon darkens to a bloodred coal",
                                    size_word(moon.angular_diameter_rel)
                                ),
                                period_days: None,
                                salience: 0.8,
                                venue: Venue::NightSky,
                            });
                        }
                    }
                }
            }
        }
```

(`self.t(ctx.time)` already bound `t` at the top of `phenomena`. Salience stays below 1.0 — the tier-refinement assertion from Task 6 is the tripwire.)

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy provider 2>&1 | tee /tmp/hv-es-t9.txt` → PASS. Then check the walker battery: `cargo test -p hornvale-vessel 2>&1 | tail -5` — if a golden transcript drifted (an eclipse fell on a walked day), inspect the new line for honesty and re-pin the transcript **in this commit** (the standing re-baseline rule).

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): placed event-day eclipse observations at the sight tiers"
```

---

### Task 10: The almanac Eclipses lines

**Files:**
- Modify: `windows/almanac/src/lib.rs` (`NightSkyLines` + `render` + test)
- Modify: `windows/worldgen/src/lib.rs:2448-2546` (populate the new field)

**Interfaces:**
- Consumes: `eclipse_events`, `EclipseBody`, `EclipseKind`, `ground_track`, `node_regression_period`, `eclipse_year`, `draconic_month`, `best_cycle`, `series_returns`, `parade_days_per_year`.
- Produces: `NightSkyLines.eclipses: Vec<String>` — dated sentences plus the recurrence-ladder lines, rendered under **The Sky**. (The Long Count adds `alignment` beside it; expect a trivial merge there when absorbing main.)

- [ ] **Step 1: Write the failing almanac render test** (in `windows/almanac/src/lib.rs` `mod tests`, extending `sample_context`):

```rust
    #[test]
    fn eclipse_lines_render_under_the_sky() {
        let mut ctx = sample_context();
        if let Some(lines) = ctx.night_sky.as_mut() {
            lines.eclipses = vec![
                "On day 213, the great moon devours the sun whole along latitude 23°."
                    .to_string(),
                "The eclipse seasons parade backward through the year at 19 days a year."
                    .to_string(),
            ];
        }
        let doc = render(&ctx);
        let sky = doc.split("## The Calendar").next().unwrap();
        assert!(sky.contains("devours the sun whole"));
        assert!(sky.contains("parade backward"));
    }
```

(Adjust the field path to `sample_context`'s actual shape — the `NightSkyLines` value lives wherever the pole-star/heliacal lines already sit in `AlmanacContext`.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-almanac eclipse_lines` → compile error: no field `eclipses`.

- [ ] **Step 3: Implement the almanac side.** In `NightSkyLines` (lib.rs:68-79), add:

```rust
    /// Dated-eclipse sentences for the next two years (at most six,
    /// day-ascending) followed by the recurrence-ladder lines (Eclipse
    /// Seasons). One honest no-eclipse sentence when the world has moons
    /// but no event falls in the window.
    pub eclipses: Vec<String>,
```

In `render`, after the heliacal loop, following the same paragraph/blank-line convention the pole-star and heliacal lines use (inspect the surrounding pushes and mirror them exactly):

```rust
        for line in &lines.eclipses {
            doc.push_str(line);
            doc.push('\n');
        }
```

Fix every `NightSkyLines { .. }` literal in almanac and worldgen with `eclipses: Vec::new(),`.

- [ ] **Step 4: Write the failing worldgen test:**

```rust
    /// The seed-42 default world's almanac context carries dated eclipse
    /// lines and the recurrence-ladder lines (it has moons).
    #[test]
    fn almanac_context_dates_eclipses_and_the_ladder() {
        let world = build_seed_42_default_world(); // the fixture the heliacal/night-sky tests use
        let ctx = almanac_context(&world).unwrap();
        let lines = ctx.night_sky.expect("generated sky");
        assert!(!lines.eclipses.is_empty());
        assert!(
            lines.eclipses.iter().any(|l| l.contains("parade")),
            "ladder lines present"
        );
    }
```

- [ ] **Step 5: Implement the worldgen side** in the `NightSkyLines` construction site (lib.rs:2448-2546), after the heliacal lines (which already have `system`/`calendar`/`t` in scope):

```rust
    let year = calendar.year_length().get();
    let events = hornvale_astronomy::eclipse_events(
        system,
        calendar,
        t,
        hornvale_astronomy::StdDays::new(t.get() + 2.0 * year).unwrap(),
    );
    let ordinal = |i: usize| ["first", "second", "third"].get(i).copied().unwrap_or("far");
    let mut eclipses: Vec<String> = events
        .iter()
        .take(6)
        .map(|e| {
            use hornvale_astronomy::{EclipseBody, EclipseKind};
            match (e.body, e.kind) {
                (EclipseBody::Lunar, _) => format!(
                    "On day {:.0}, the full {} moon darkens to a bloodred coal.",
                    e.day.get(),
                    ordinal(e.moon)
                ),
                (EclipseBody::Solar, kind) => {
                    let verb = match kind {
                        EclipseKind::Total => "devours the sun whole",
                        EclipseKind::Annular => "leaves a burning ring of the sun",
                    };
                    match hornvale_astronomy::ground_track(system, calendar, e) {
                        Some(track) => format!(
                            "On day {:.0}, the {} moon {} along latitude {:.0}°.",
                            e.day.get(),
                            ordinal(e.moon),
                            verb,
                            track.center_lat_deg
                        ),
                        None => format!(
                            "On day {:.0}, the {} moon {}.",
                            e.day.get(),
                            ordinal(e.moon),
                            verb
                        ),
                    }
                }
            }
        })
        .collect();
    if eclipses.is_empty() && !system.moons.is_empty() {
        eclipses.push("No eclipse will darken the sun for two years.".to_string());
    }
    // The recurrence ladder, read off the innermost moon.
    if let (Some(moon), Some(synodic)) =
        (system.moons.first(), calendar.synodic_month(0))
    {
        let year_len = calendar.year_length();
        let p_node = hornvale_astronomy::node_regression_period(
            year_len,
            moon.period,
            moon.inclination_deg,
        );
        let ey = hornvale_astronomy::eclipse_year(year_len, p_node);
        let parade = hornvale_astronomy::parade_days_per_year(year_len, ey);
        eclipses.push(format!(
            "The eclipse seasons parade backward through the year at {parade:.0} days a year."
        ));
        let draconic = hornvale_astronomy::draconic_month(
            year_len,
            moon.period,
            moon.inclination_deg,
        );
        if let Some(cycle) = hornvale_astronomy::best_cycle(synodic, draconic) {
            let sun_angular = hornvale_astronomy::sun_angular_rel_at(system, calendar, t);
            let theta = hornvale_astronomy::solar_eclipse_threshold_deg(
                sun_angular,
                moon.angular_diameter_rel,
            );
            let returns =
                hornvale_astronomy::series_returns(&cycle, theta, moon.inclination_deg);
            eclipses.push(format!(
                "Eclipses of the first moon repeat every {:.0} days ({} months); \
                 a family of them lives about {:.0} years.",
                cycle.period.get(),
                cycle.synodic_count,
                returns as f64 * cycle.period.get() / 365.25
            ));
        }
    }
```

and add `eclipses,` to the `NightSkyLines { ... }` literal.

- [ ] **Step 6: Run:** `cargo test -p hornvale-almanac && cargo test -p hornvale-worldgen almanac 2>&1 | tee /tmp/hv-es-t10.txt` → PASS. Then the full gate: `cargo fmt && make gate 2>&1 | tail -20` → expect FAILURES only in artifact-freshness/golden-almanac tests that pin almanac text — re-pin those goldens in this commit (re-baseline in the drifting commit, the standing rule). Committed gallery artifacts regenerate in Task 12.

- [ ] **Step 7: Commit:**

```bash
git add -A && git commit -m "feat(almanac): the reckoning of eclipses — dated events and the recurrence ladder"
```

---

### Task 11: Census metrics

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`registry()` — four new `Metric` entries + two helpers)

**Interfaces:**
- Consumes: `AstronomyView { system, calendar, .. }`, `eclipse_events`, `node_regression_period`, `eclipse_year`, `coincidence_days`.
- Produces: metrics `eclipse-year-days`, `solar-eclipses-per-century`, `lunar-eclipses-per-century`, `coincidence-days-per-century`.

- [ ] **Step 1: Write the failing test** (extend the module's existing metric-coverage test the way its neighbors do, plus):

```rust
    #[test]
    fn the_eclipse_seasons_metrics_extract_on_seed_42() {
        let names = [
            "eclipse-year-days",
            "solar-eclipses-per-century",
            "lunar-eclipses-per-century",
            "coincidence-days-per-century",
        ];
        let reg = registry();
        for name in names {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
        // Extraction smoke: follow the file's existing apply-to-view test
        // helper exactly (there are tests applying Extractor variants to
        // built views — copy their invocation).
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-lab eclipse_seasons_metrics` → FAIL (metrics missing).

- [ ] **Step 3: Implement** — append to `registry()` after the last astronomy metric, matching the house entry style, with two file-private helpers:

```rust
/// The 100-year dated-eclipse scan shared by the cadence metrics — fixed
/// in standard days (a schedule constant, never a function of the drawn
/// year, so cost and precision are seed-independent).
fn scan_century(v: &AstronomyView) -> Vec<hornvale_astronomy::EclipseEvent> {
    hornvale_astronomy::eclipse_events(
        &v.system,
        &v.calendar,
        hornvale_astronomy::StdDays::new(0.0).unwrap(),
        hornvale_astronomy::StdDays::new(100.0 * 365.25).unwrap(),
    )
}

fn century_cadence(v: &AstronomyView, body: hornvale_astronomy::EclipseBody) -> MetricValue {
    let n = scan_century(v).iter().filter(|e| e.body == body).count();
    MetricValue::Number(n as f64)
}
```

```rust
        Metric {
            name: "eclipse-year-days",
            doc: "Eclipse year (the sun's return to the innermost moon's node line), standard days; Absent if moonless",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 100.0, 200.0, 300.0, 400.0, 600.0, 1000.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                match v.system.moons.first() {
                    None => MetricValue::Absent,
                    Some(m) => {
                        let p = hornvale_astronomy::node_regression_period(
                            v.system.anchor.year,
                            m.period,
                            m.inclination_deg,
                        );
                        MetricValue::Number(
                            hornvale_astronomy::eclipse_year(v.system.anchor.year, p).get(),
                        )
                    }
                }
            }),
        },
        Metric {
            name: "solar-eclipses-per-century",
            doc: "Dated solar eclipses anywhere on the world across a 100-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 50.0, 100.0, 200.0, 400.0, 800.0, 1600.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                century_cadence(v, hornvale_astronomy::EclipseBody::Solar)
            }),
        },
        Metric {
            name: "lunar-eclipses-per-century",
            doc: "Dated lunar eclipses across a 100-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 50.0, 100.0, 200.0, 400.0, 800.0, 1600.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                century_cadence(v, hornvale_astronomy::EclipseBody::Lunar)
            }),
        },
        Metric {
            name: "coincidence-days-per-century",
            doc: "Days in a 100-year scan carrying eclipse events from two or more different moons; zero for 0–1-moon worlds",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 1.0, 2.0, 5.0, 10.0, 25.0, 50.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(
                    hornvale_astronomy::coincidence_days(&scan_century(v)) as f64,
                )
            }),
        },
```

(Match the file's actual `Metric`/`Extractor`/`MetricValue`/`SummaryKind` spellings — copy an adjacent astronomy entry's shape exactly. If re-exports for `EclipseBody`/`EclipseEvent`/`coincidence_days` are missing from `hornvale_astronomy`'s root, they were added in Tasks 4 and 8.)

- [ ] **Step 4: Run:** `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-es-t11.txt` → PASS. Note: `hornvale lab list-metrics` reference output and census `schema.json` now drift — the dumps regenerate in Task 12; the census fixtures at the AWS close regen.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(lab): eclipse-year, cadence, and coincidence census metrics"
```

---

### Task 12: Artifacts, model card, docs freshness

**Files:**
- Modify: `docs/superpowers/specs/2026-07-05-campaign-2-the-sky-design.md` (§5 model card — append rows)
- Regenerate: `book/src/gallery/` almanacs, `book/src/reference/` dumps (concepts, streams, list-metrics)

**Interfaces:**
- Consumes: everything above.
- Produces: a green `make gate` on a tree whose committed artifacts match regenerated reality.

- [ ] **Step 1: Append the model-card rows** to the Campaign 2 spec §5 model card, matching its table format, one row per new formula: nodal regression period (approximated, lunar-theory leading term, ~17.9 yr vs true 18.61), moon ecliptic latitude (approximated, small-angle), event-time sun size (approximated, first-order apsidal), lunar shadow factor (approximated, calibrated `LUNAR_SHADOW_FACTOR = 0.64` or the tuned value from Task 5), ground-track latitude mapping (approximated), track half-width (declared constant 2.0°), draconic month / eclipse year / best cycle / series returns / parade (derived).

- [ ] **Step 2: Regenerate artifacts** (never the census locally):

```bash
SKIP_CENSUS=1 ./scripts/regenerate-artifacts.sh 2>&1 | tail -5
git status --short   # expect: gallery almanacs, reference dumps (streams gains moon-nodes, concepts gains the two predicates, list-metrics gains four)
```

(If the repo's regeneration entrypoint differs, the authoritative command list is the "Artifacts are current" step in `.github/workflows/ci.yml` — run those commands with the census-producing `lab run` steps under `SKIP_CENSUS=1`'s convention.)

- [ ] **Step 3: Inspect the diff for honesty** — the seed-42 almanacs should gain an Eclipses block under The Sky with dated sentences and ladder lines; nothing else in them should change. `git diff book/src/gallery/ | head -80`.

- [ ] **Step 4: Run the full gate:** `cargo fmt && make gate 2>&1 | tail -20` → PASS, including the docs-consistency test.

- [ ] **Step 5: Commit:**

```bash
git add -A && git commit -m "regen: eclipse-seasons artifacts — almanac eclipses, streams, concepts, metrics"
```

---

## Close checklist (outside the tasks — the closing-a-campaign skill drives these)

- Chronicle entry + campaign retrospective (decision 0020).
- Registry sweep per spec §7: `SKY-eclipse-seasons` → shipped (stale first clause corrected); `SKY-6` refinement note; new standstills row (prerequisite shipped: `moon_ecliptic_longitude_deg` + node machinery); `SKY-transits` / `SKY-tidal-braking` seam notes.
- Confidence Gradient re-score if a bet moves (decision 0030).
- Census regen on AWS with Nathan's explicit go-ahead (schema gained four metrics), then re-pin any census-schema tests.
- Verify The Long Count's descope held (its plan must not re-add the eclipse tasks).
