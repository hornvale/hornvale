# The Long Count Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the four astronomy residuals — dated solar/lunar eclipses with nodal regression (SKY-6 + SKY-eclipse-seasons), secular stellar brightening (SKY-1), the alignment ground half (SKY-stale-alignments), and the star/anchor/neighbor verification batteries (SKY-23).

**Architecture:** One new drawn quantity (per-moon node longitude Ω₀, stream `moon-nodes`, drawn after every existing draw); everything else is a pure derivation over elements already held. A new `domains/astronomy/src/eclipses.rs` computes dated events by closed-form syzygy scan (the `heliacal.rs` precedent); `star.rs` gains a draw-free luminosity slope; `calendar.rs` gains solstice-azimuth functions; the composition root (`windows/worldgen`) commits founding-alignment facts per settlement and feeds new almanac lines; `windows/lab` gains six metrics.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only. All transcendentals through `hornvale_kernel::math` (libm, decision 0041). No `HashMap`/`HashSet`. No new crates.

**Spec:** `docs/superpowers/specs/2026-07-14-the-long-count-design.md`

## Global Constraints

- **Determinism is constitutional.** Same seed → byte-identical worlds. The new stream is `"moon-nodes"`, drawn per admitted moon *after* the `moon-inclinations` draws; no existing draw, constant, or formula may change. `golden_seed_42.rs`'s existing assertions must pass untouched throughout.
- **`f64` transcendentals** (`sin`, `cos`, `asin`, `acos`, `atan2`, `powf`, `tan`) go through `hornvale_kernel::math`, never `f64::sin` etc. (`sqrt`, `abs`, `floor`, `ceil`, `rem_euclid`, `to_radians`, `to_degrees` are IEEE-exact and stay as std methods — match surrounding code.)
- **Quantization at emit only:** facts are quantized by `Ledger::commit`; never quantize inside compute paths.
- **Float ordering:** `total_cmp` with a deterministic tie-break.
- **Every crate has `#![warn(missing_docs)]`** — every new public item, field, and variant gets a one-line doc comment, and every new pub-boundary primitive gets a `type-audit:` tag (`pending(wave-1)` for physical quantities, `bare-ok(ratio)`/`bare-ok(identifier-text)`/`bare-ok(index)` per class — copy the tag style of the adjacent field).
- **No wall-clock time.** Time is `StdDays` / `WorldTime { day: f64 }`.
- **Gate cadence:** during a task iterate with scoped tests (`cargo test -p hornvale-astronomy <name>`); before each task's commit run `cargo fmt` then `make gate-fast`; run the full `make gate` at Tasks 5, 9, and 12 before committing. Never `--no-verify`.
- **Worktree:** execute in a worktree at `~/.config/superpowers/worktrees/hornvale/the-long-count/`, branch `the-long-count` (superpowers:using-git-worktrees). SDD scratch lives in the worktree's `.superpowers/sdd/`, never the main checkout.
- **Censuses are never regenerated locally.** Artifact regeneration uses `SKIP_CENSUS=1`. The one census regen happens on AWS pre-merge with Nathan's explicit go-ahead (close-time, outside this plan's tasks).

---

### Task 1: The node-longitude draw

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (append)
- Modify: `domains/astronomy/src/lib.rs:64` (`stream_labels()`)
- Modify: `domains/astronomy/src/moons.rs` (field + draw + tests)

**Interfaces:**
- Consumes: `streams::MOON_INCLINATIONS` pattern (moons.rs:131-137).
- Produces: `Moon.node_longitude_deg: f64` in `[0, 360)`; `streams::MOON_NODES: &str = "moon-nodes"`. Every later task reads `moon.node_longitude_deg`.

- [ ] **Step 1: Write the failing test** — in `domains/astronomy/src/moons.rs` `mod tests`, mirroring `every_moon_draws_an_inclination_in_range`:

```rust
    /// The Long Count: every admitted moon draws an ascending-node
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
  `cargo test -p hornvale-astronomy every_moon_draws_a_node_longitude -- --nocapture` → error: no field `node_longitude_deg`.

- [ ] **Step 3: Implement.** In `streams.rs`, append:

```rust
/// Per-moon ascending-node longitude draws (The Long Count).
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODES: &str = "moon-nodes";
```

In `moons.rs`, add to `Moon` (after `inclination_deg`):

```rust
    /// Ecliptic longitude of the ascending node at genesis, in degrees
    /// (drawn 0–360, own stream — The Long Count): with the inclination,
    /// the full node geometry that dates each eclipse.
    /// type-audit: pending(wave-1)
    pub node_longitude_deg: f64,
```

In `derive_moon`, add `node_longitude_deg: 0.0,` (comment: drawn after admission, like the inclination). At the end of `generate_moons`, after the inclinations loop:

```rust
    // The Long Count: node longitudes draw from their own stream, after
    // the inclinations, so every pre-node draw is byte-identical.
    let mut nodes = astronomy_seed.derive(streams::MOON_NODES).stream();
    for moon in &mut moons {
        moon.node_longitude_deg = nodes.next_f64() * 360.0;
    }
```

In `lib.rs` `stream_labels()`, append after the `moon-inclinations` entry (keep the existing `astronomy/…` prefix style of that list):

```rust
        (
            "astronomy/moon-nodes",
            "per-moon ascending-node longitude draws (The Long Count)",
        ),
```

- [ ] **Step 4: Run the tests** — new test, the golden pins, and the moons module:
  `cargo test -p hornvale-astronomy --test golden_seed_42 && cargo test -p hornvale-astronomy moons` → all PASS (golden pins prove pre-node draws didn't move).

- [ ] **Step 5: Fix the struct-literal fallout.** Any test or code constructing `Moon { .. }` by literal now needs the new field — `cargo build -p hornvale-astronomy --all-targets 2>&1 | head -30` and add `node_longitude_deg: 0.0` where the compiler points.

- [ ] **Step 6: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): per-moon node-longitude draw on its own stream (The Long Count)"
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

- [ ] **Step 1: Write the failing tests** — create `eclipses.rs` with module doc + tests only:

```rust
//! Dated eclipses (The Long Count, SKY-6 close-out): node geometry as a
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
        assert!(
            (6000.0..7000.0).contains(&p.0),
            "P_node {} days",
            p.0
        );
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
/// fixture Tasks 3–5 reuse — module scope (not inside `mod tests`) so
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

(If any unit newtype above rejects tuple construction from the test module — they are `pub struct X(pub f64)` in-crate today per `moons.rs`/`heliacal.rs` usage — use its `::new(x).unwrap()` constructor instead; do the same everywhere this plan constructs units.)

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
/// by under 1%). Moved here from `provider.rs` (The Long Count).
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
/// check: ~17.9 yr against the true 18.61. This IS the standstill period.
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
/// (SKY-6) — the threshold and chance now live in `eclipses.rs` (The Long
/// Count); this wrapper keeps the phenomenon path reading as before.
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
git add -A && git commit -m "feat(astronomy): nodal regression and moon ecliptic latitude (The Long Count)"
```

---

### Task 3: Dated solar eclipse events

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/lib.rs` (extend the eclipses re-export)

**Interfaces:**
- Consumes: Task 2's functions; `crate::star::sun_angular_diameter_rel(&Star, Au) -> f64`; `StarSystem` fields.
- Produces:
  - `pub enum EclipseBody { Solar, Lunar }`, `pub enum EclipseKind { Total, Annular }`
  - `pub struct EclipseEvent { pub day: StdDays, pub moon: usize, pub body: EclipseBody, pub kind: EclipseKind }`
  - `pub fn eclipse_events(system: &StarSystem, calendar: &Calendar, from: StdDays, until: StdDays) -> Vec<EclipseEvent>` — day-ascending, tie-broken by moon index. (Task 4 adds the Lunar arm; until then the function emits Solar only.)

- [ ] **Step 1: Write the failing tests** (in `eclipses.rs` `mod tests`):

```rust
    /// Luna–Sol calibration: the dated scan reproduces Earth's ~2.4 solar
    /// eclipses/year over a 50-year window (the anywhere-on-the-world
    /// count the parallax allowance is calibrated to).
    #[test]
    fn luna_sol_dates_earths_solar_cadence() {
        let (system, calendar) = luna_sol();
        let years = 50.0;
        let events = eclipse_events(
            &system,
            &calendar,
            StdDays(0.0),
            StdDays(365.25 * years),
        );
        let solar = events
            .iter()
            .filter(|e| matches!(e.body, EclipseBody::Solar))
            .count() as f64;
        let per_year = solar / years;
        assert!(
            (1.9..=2.9).contains(&per_year),
            "solar eclipses/year {per_year}"
        );
    }

    /// Every dated solar event sits at a new moon and inside the node
    /// threshold — the geometry cannot lie.
    #[test]
    fn solar_events_fall_at_new_moons_inside_the_threshold() {
        let (system, calendar) = luna_sol();
        let events = eclipse_events(&system, &calendar, StdDays(0.0), StdDays(365.25 * 20.0));
        assert!(!events.is_empty());
        let sun_angular =
            crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
        for e in events.iter().filter(|e| matches!(e.body, EclipseBody::Solar)) {
            let moon = &system.moons[e.moon];
            let phase = calendar.moon_phase(e.day, e.moon).unwrap();
            let off_new = phase.min(1.0 - phase);
            assert!(off_new < 1e-6, "phase {phase} at day {}", e.day.0);
            let beta = moon_ecliptic_latitude_deg(&calendar, moon, e.moon, e.day).unwrap();
            let threshold =
                solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
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
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy eclipses` → compile errors: `EclipseEvent` etc. not defined.

- [ ] **Step 3: Implement** in `eclipses.rs`:

```rust
use crate::system::StarSystem;

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
    /// Total or annular.
    pub kind: EclipseKind,
}

/// Every dated eclipse in `[from, until]`, day-ascending (moon index as
/// the deterministic tie-break). Syzygies are closed-form — the synodic
/// phase is linear in `t` — so the scan visits each new (and, Task 4,
/// full) moon exactly, no sampling. A moon with no synodic cycle never
/// eclipses.
pub fn eclipse_events(
    system: &StarSystem,
    calendar: &Calendar,
    from: StdDays,
    until: StdDays,
) -> Vec<EclipseEvent> {
    let mut out = Vec::new();
    let sun_angular =
        crate::star::sun_angular_diameter_rel(&system.star, system.anchor.orbit);
    for (index, moon) in system.moons.iter().enumerate() {
        let Some(synodic) = calendar.synodic_month(index) else {
            continue;
        };
        let Some(phase0) = calendar.moon_phase(StdDays(0.0), index) else {
            continue;
        };
        let theta_solar =
            solar_eclipse_threshold_deg(sun_angular, moon.angular_diameter_rel);
        // Syzygy k of each family sits at t = (k + half − phase0)·synodic.
        for (half, body, threshold) in syzygy_families(theta_solar) {
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
                if beta.abs() < threshold {
                    let kind = match body {
                        EclipseBody::Lunar => EclipseKind::Total,
                        EclipseBody::Solar if moon.angular_diameter_rel >= sun_angular => {
                            EclipseKind::Total
                        }
                        EclipseBody::Solar => EclipseKind::Annular,
                    };
                    out.push(EclipseEvent {
                        day: t,
                        moon: index,
                        body,
                        kind,
                    });
                }
            }
        }
    }
    out.sort_by(|a, b| a.day.0.total_cmp(&b.day.0).then(a.moon.cmp(&b.moon)));
    out
}

/// The syzygy families the scan walks. Solar only until Task 4 adds the
/// full-moon (lunar) family.
fn syzygy_families(theta_solar: f64) -> Vec<(f64, EclipseBody, f64)> {
    vec![(0.0, EclipseBody::Solar, theta_solar)]
}
```

Extend the `lib.rs` re-export with `EclipseBody, EclipseEvent, EclipseKind, eclipse_events`.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): dated solar eclipse events by closed-form syzygy scan"
```

---

### Task 4: Lunar eclipses

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`

**Interfaces:**
- Consumes: Task 3's scan.
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
        assert!(
            (1.1..=1.9).contains(&per_year),
            "lunar eclipses/year {per_year}"
        );
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
/// lunar eclipses/year). Below 1.0 because the solar threshold carries
/// the anywhere-on-the-world parallax allowance the lunar case doesn't
/// need — the shadow is one shadow for every observer.
pub const LUNAR_SHADOW_FACTOR: f64 = 0.64;
```

and widen `syzygy_families` to both arms (the caller already passes `theta_solar`):

```rust
fn syzygy_families(theta_solar: f64) -> Vec<(f64, EclipseBody, f64)> {
    vec![
        (0.0, EclipseBody::Solar, theta_solar),
        (0.5, EclipseBody::Lunar, LUNAR_SHADOW_FACTOR * theta_solar),
    ]
}
```

If the cadence test lands outside 1.1–1.9/yr, tune `LUNAR_SHADOW_FACTOR` in 0.02 steps until it centers near 1.5 (the analytic value is 0.636), and record the final value in the model-card row (Task 12).

Add `LUNAR_SHADOW_FACTOR` to the `lib.rs` eclipses re-export.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy eclipses` → PASS (both cadences).

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): dated lunar eclipses with a calibrated shadow threshold"
```

---

### Task 5: Facts, the lunar phenomenon, and the rate-consistency guard

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (two predicates + commits + doc update)
- Modify: `domains/astronomy/src/lib.rs:105` (`register_concepts`)
- Modify: `domains/astronomy/src/provider.rs` (lunar phenomenon + consistency test)
- Modify: `domains/astronomy/tests/tier_refinement.rs` (one assertion)
- Modify: `domains/astronomy/tests/golden_seed_42.rs` (pin the new draws)

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: predicates `facts::MOON_NODE_LONGITUDE_DEGREES = "moon-node-longitude-degrees"`, `facts::STANDSTILL_PERIOD_DAYS = "standstill-period-days"`; per-moon facts in every generated ledger; a `Venue::NightSky` lunar `eclipse` phenomenon.

- [ ] **Step 1: Write the failing fact test** — in `facts.rs` `mod tests`, next to the existing per-moon fact tests (follow their fixture style — they build a world, run `genesis`, then filter the ledger):

```rust
    /// The Long Count: each moon commits its node longitude and its
    /// standstill (nodal-regression) period.
    #[test]
    fn each_moon_commits_node_and_standstill_facts() {
        let (world, subject, outcome) = committed_world(42); // reuse/extract the module's existing world+genesis fixture
        let moons = outcome.system.moons.len();
        assert!(moons > 0);
        for predicate in [MOON_NODE_LONGITUDE_DEGREES, STANDSTILL_PERIOD_DAYS] {
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

(If the module has no shared fixture named `committed_world`, extract one from the test at facts.rs:107's neighborhood rather than duplicating its body.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy each_moon_commits_node` → compile error: predicates not defined.

- [ ] **Step 3: Implement facts.** In `facts.rs`, after `MOON_INCLINATION_DEGREES` (line 47):

```rust
/// Predicate: a moon's ascending-node ecliptic longitude at genesis,
/// degrees (The Long Count).
/// type-audit: bare-ok(identifier-text)
pub const MOON_NODE_LONGITUDE_DEGREES: &str = "moon-node-longitude-degrees";
/// Predicate: a moon's nodal-regression (lunar-standstill) period,
/// standard days (The Long Count) — the monument-scale ritual beat.
/// type-audit: bare-ok(identifier-text)
pub const STANDSTILL_PERIOD_DAYS: &str = "standstill-period-days";
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
                STANDSTILL_PERIOD_DAYS,
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
        facts::STANDSTILL_PERIOD_DAYS,
        false,
        "nodal-regression (standstill) period of a moon, in standard days",
    )?;
```

Also update the `ECLIPSE` phenomenon-kind doc string at lib.rs:109 to cover both bodies:

```rust
    registry.register_phenomenon_kind(
        ECLIPSE,
        "a syzygy shadow: a moon crossing the sun, or the world's shadow crossing a moon",
    )?;
```

- [ ] **Step 4: Write the failing phenomenon test** — in `provider.rs` `mod tests`, next to `a_covering_moon_promises_total_eclipses_on_the_node_beat` (reuse its fixture pattern):

```rust
    /// The Long Count: each mooned sky promises lunar eclipses in the
    /// night sky — bloodred, salience 0.8, on the wider-shadow node beat.
    #[test]
    fn a_mooned_sky_promises_bloodred_lunar_eclipses_at_night() {
        let ph = phenomena_for_default_seed_42_at_night(); // reuse the fixture the solar eclipse tests use, night venue
        let lunar = ph
            .iter()
            .find(|p| p.kind == ECLIPSE && p.venue == Venue::NightSky)
            .expect("lunar eclipse phenomenon");
        assert!(
            lunar.description.contains("bloodred"),
            "got: {}",
            lunar.description
        );
        assert!((lunar.salience - 0.8).abs() < 1e-9);
        assert!(lunar.period_days.expect("recurs") > 0.0);
    }
```

- [ ] **Step 5: Implement the phenomenon.** In `provider.rs`, inside the `show_moons` block (after the per-moon `CELESTIAL_BODY` loop at lines 1333-1344):

```rust
            // The Long Count: lunar eclipses — the night twin of SKY-6's
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

- [ ] **Step 6: Write and pass the rate-consistency guard** — in `provider.rs` tests:

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

(`luna_sol` is a module-scope `#[cfg(test)] pub(crate) fn` in `eclipses.rs` — Task 2 placed it there precisely so this cross-file test can reach it.)

- [ ] **Step 7: Tier refinement + golden pins.** In `tier_refinement.rs`, inside the existing per-regime phenomena loop of `refinement_adds_structure_only_beneath_the_sun`, add:

```rust
        for p in phenomena.iter().filter(|p| p.kind == "eclipse") {
            assert!(p.salience < 1.0, "an eclipse never outranks the sun");
        }
```

In `golden_seed_42.rs`, extend the moons assertion to pin the new draws: run

`cargo test -p hornvale-astronomy --test golden_seed_42 -- --nocapture`

then add (with the actual printed values — capture them by temporarily `eprintln!`-ing `s.moons[i].node_longitude_deg`, or via a deliberately failing `assert_eq!`):

```rust
    let nodes: Vec<f64> = s.moons.iter().map(|m| m.node_longitude_deg).collect();
    assert_eq!(nodes, vec![/* the two captured full-precision values */]);
```

- [ ] **Step 8: Run the crate + full gate:** `cargo test -p hornvale-astronomy 2>&1 | tee /tmp/hv-t5.txt` → PASS; then `cargo fmt && make gate` → PASS. (Ledger-shape consumers — schema/census tests in the heavy tier — are not in the commit gate; census drift is handled at close.)

- [ ] **Step 9: Commit:**

```bash
git add -A && git commit -m "feat(astronomy): node/standstill facts and the bloodred lunar-eclipse phenomenon"
```

---

### Task 6: Secular stellar brightening

**Files:**
- Modify: `domains/astronomy/src/star.rs`
- Modify: `domains/astronomy/src/facts.rs` (+ `lib.rs` registration and re-export)

**Interfaces:**
- Consumes: `Star`, `Anchor`, `StdDays`.
- Produces:
  - `pub const GYR_DAYS: f64 = 1.0e9 * 365.25;`
  - `pub fn brightening_per_gyr(star: &Star) -> f64` (= `0.10 · M^2.5`)
  - `pub fn luminosity_at(star: &Star, t: StdDays) -> SolarLuminosities`
  - `pub fn insolation_rel_at(star: &Star, anchor: &Anchor, t: StdDays) -> f64`
  - `facts::BRIGHTENING_PER_GYR = "brightening-per-gyr"` (functional, on the world subject)

- [ ] **Step 1: Write the failing tests** (in `star.rs` `mod tests`):

```rust
    /// The Long Count (SKY-1 close-out): luminosity is anchored at the
    /// present and brightens on the main-sequence slope — 10%/Gyr at one
    /// solar mass, faster for heavier stars (b = 0.10·M^2.5).
    #[test]
    fn luminosity_brightens_on_the_main_sequence_slope() {
        use crate::units::StdDays;
        let s = generate_star(Seed(42));
        assert_eq!(luminosity_at(&s, StdDays(0.0)), s.luminosity);
        let after_gyr = luminosity_at(&s, StdDays(GYR_DAYS));
        let expected = s.luminosity.get() * (1.0 + brightening_per_gyr(&s));
        assert!((after_gyr.get() - expected).abs() < 1e-12);
        let b = brightening_per_gyr(&s);
        assert!((b - 0.10 * math::powf(s.mass.get(), 2.5)).abs() < 1e-15);
    }

    /// Heavier stars age faster, and insolation follows luminosity.
    #[test]
    fn brightening_scales_with_mass_and_reaches_insolation() {
        use crate::pins::SkyPins;
        use crate::units::StdDays;
        let mut light = generate_star(Seed(42));
        light.mass = SolarMasses::new(0.7).unwrap();
        let mut heavy = light.clone();
        heavy.mass = SolarMasses::new(1.3).unwrap();
        assert!(brightening_per_gyr(&heavy) > brightening_per_gyr(&light));
        let star = generate_star(Seed(42));
        let anchor = crate::anchor::generate_anchor(Seed(42), &star, &SkyPins::default()).unwrap();
        assert_eq!(
            insolation_rel_at(&star, &anchor, StdDays(0.0)),
            insolation_rel(&star, &anchor)
        );
        assert!(
            insolation_rel_at(&star, &anchor, StdDays(GYR_DAYS))
                > insolation_rel(&star, &anchor)
        );
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy brighten` → compile errors.

- [ ] **Step 3: Implement** in `star.rs` (after `insolation_rel`):

```rust
/// Standard days per gigayear — the timescale secular brightening lives on.
/// type-audit: pending(wave-1)
pub const GYR_DAYS: f64 = 1.0e9 * 365.25;

/// Fractional main-sequence brightening per gigayear: b = 0.10 · M^2.5
/// (declared approximation, model card — Sol-calibrated at ~10%/Gyr,
/// scaled by the main-sequence lifetime t_MS ∝ M⁻²·⁵). Draw-free: mass
/// drawn, everything else derived.
/// type-audit: bare-ok(ratio)
pub fn brightening_per_gyr(star: &Star) -> f64 {
    0.10 * math::powf(star.mass.0, 2.5)
}

/// Luminosity at absolute time `t`, anchored so `luminosity_at(star, 0)`
/// equals the genesis luminosity exactly. The habitable zone remains a
/// genesis-epoch derivation from L₀ (the world lives on kiloyear scales,
/// where this slope is honestly negligible — deep time is where it shows).
pub fn luminosity_at(star: &Star, t: crate::units::StdDays) -> SolarLuminosities {
    SolarLuminosities(star.luminosity.0 * (1.0 + brightening_per_gyr(star) * t.0 / GYR_DAYS))
}

/// Time-aware insolation: `luminosity_at / a²` — [`insolation_rel`]'s
/// deep-time sibling (SKY-15's shared definition, evaluated at `t`).
/// type-audit: pending(wave-1)
pub fn insolation_rel_at(star: &Star, anchor: &crate::anchor::Anchor, t: crate::units::StdDays) -> f64 {
    luminosity_at(star, t).0 / (anchor.orbit.0 * anchor.orbit.0)
}
```

In `facts.rs` (after `INSOLATION_REL`, line 143):

```rust
/// Predicate: the star's fractional main-sequence brightening per
/// gigayear (The Long Count).
/// type-audit: bare-ok(identifier-text)
pub const BRIGHTENING_PER_GYR: &str = "brightening-per-gyr";
```

In `genesis()`, next to the `STAR_LUMINOSITY_SOLAR` commit:

```rust
    world.ledger.commit(
        fact(
            subject,
            BRIGHTENING_PER_GYR,
            Value::Number(crate::star::brightening_per_gyr(&system.star)),
        ),
        &world.registry,
    )?;
```

In `lib.rs`: register the predicate (functional — one per world):

```rust
    registry.register_predicate(
        facts::BRIGHTENING_PER_GYR,
        true,
        "the star's fractional main-sequence brightening per gigayear",
    )?;
```

and extend the star re-export: `pub use star::{GYR_DAYS, Star, brightening_per_gyr, generate_star, insolation_rel, insolation_rel_at, luminosity_at};`

Add a fact-count assertion to the same `facts.rs` test fixture used in Task 5 (one `brightening-per-gyr` fact on the world subject).

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy star && cargo test -p hornvale-astronomy facts` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): secular stellar brightening, derived and anchored (SKY-1 close-out)"
```

---

### Task 7: Alignment functions (calendar ground half)

**Files:**
- Modify: `domains/astronomy/src/calendar.rs` (three methods, after `precession_offset_deg` at line 652)

**Interfaces:**
- Consumes: `self.forcing.obliquity_at`, `self.forcing.{obliquity_mean, obliquity_amp, obliquity_phase}`, `crate::forcing::P_OBLIQUITY`, `self.day_length()`, `self.retrograde`.
- Produces (on `Calendar`):
  - `pub fn solstice_rise_azimuth_at(&self, latitude: f64, t: StdDays) -> Option<f64>`
  - `pub fn alignment_drift_deg(&self, latitude: f64, t0: StdDays, t1: StdDays) -> Option<f64>`
  - `pub fn alignment_epoch_of(&self, azimuth_deg: f64, latitude: f64, t_now: StdDays) -> Option<StdDays>`

- [ ] **Step 1: Write the failing tests** (in `calendar.rs` `mod tests`, reusing its `spinning_system`/`locked_system` fixtures):

```rust
    /// The Long Count: the solstice sunrise azimuth is north of east
    /// (< 90°), mirror-symmetric across the equator, and None on locked
    /// worlds and inside the polar circles.
    #[test]
    fn solstice_azimuth_geometry_holds() {
        let cal = calendar_of(&spinning_system());
        let t = StdDays(0.0);
        let az = cal.solstice_rise_azimuth_at(40.0, t).unwrap();
        assert!((0.0..90.0).contains(&az), "az {az}");
        let south = cal.solstice_rise_azimuth_at(-40.0, t).unwrap();
        assert!((az - south).abs() < 1e-9, "equator mirror: {az} vs {south}");
        assert!(cal.solstice_rise_azimuth_at(89.9, t).is_none(), "polar day");
        let locked = calendar_of(&locked_system());
        assert!(locked.solstice_rise_azimuth_at(40.0, t).is_none());
    }

    /// The drift IS the obliquity wobble: bounded, periodic, and zero
    /// under the zero-forcing pin.
    #[test]
    fn alignment_drift_is_the_obliquity_wobble() {
        let cal = calendar_of(&spinning_system());
        let half = StdDays(crate::forcing::P_OBLIQUITY / 2.0);
        let d = cal.alignment_drift_deg(40.0, StdDays(0.0), half).unwrap();
        assert!(d.abs() > 0.0, "a wobbling sky drifts");
        let full = StdDays(crate::forcing::P_OBLIQUITY);
        let round = cal.alignment_drift_deg(40.0, StdDays(0.0), full).unwrap();
        assert!(round.abs() < 1e-9, "one full period returns home: {round}");
    }

    /// The dating inverse round-trips within the nearest half-cycle, and
    /// refuses to date an unmoving sky (zero amplitude).
    #[test]
    fn alignment_epoch_round_trips() {
        let cal = calendar_of(&spinning_system());
        let t = StdDays(0.3 * crate::forcing::P_OBLIQUITY);
        let az = cal.solstice_rise_azimuth_at(40.0, t).unwrap();
        let epoch = cal
            .alignment_epoch_of(az, 40.0, StdDays(t.0 + 1.0))
            .unwrap();
        assert!((epoch.0 - t.0).abs() < 1.0, "epoch {} vs t {}", epoch.0, t.0);
        use crate::pins::{ForcingPin, RotationPin, SkyPins};
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            forcing: Some(ForcingPin::Zero),
            ..SkyPins::default()
        };
        let frozen = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let az0 = frozen.solstice_rise_azimuth_at(40.0, StdDays(0.0)).unwrap();
        assert!(frozen.alignment_epoch_of(az0, 40.0, StdDays(1e6)).is_none());
    }
```

(`spinning_system()` must have a nonzero drawn `obliquity_amp` for the drift test — the default seed-42 unpinned system does; if the fixture pins forcing to zero, build an unpinned spinning system for these tests instead.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy alignment` → compile errors.

- [ ] **Step 3: Implement** (after `precession_offset_deg`):

```rust
    /// Sunrise azimuth on the day of the sun's northern-solstice extreme
    /// (δ = +ε(t)), degrees clockwise from north: cos(az) = sin ε / cos φ
    /// (refraction and horizon dip ignored — declared approximation). A
    /// retrograde world's sun rises mirrored in the west (SKY-22). `None`
    /// on a locked world (no sunrise) and inside the polar circles
    /// (midnight sun / polar night at the solstice). The drift of this
    /// azimuth over kiloyears IS the obliquity wobble — the ground half
    /// of SKY-stale-alignments (the sky half is `star_equatorial_at`).
    /// type-audit: pending(wave-1: latitude), pending(wave-1: return)
    pub fn solstice_rise_azimuth_at(&self, latitude: f64, t: StdDays) -> Option<f64> {
        self.day_length()?;
        let eps = self.forcing.obliquity_at(t.0).to_radians();
        let phi = latitude.to_radians();
        let x = math::sin(eps) / math::cos(phi);
        if x.abs() > 1.0 {
            return None;
        }
        let az = math::acos(x).to_degrees();
        Some(if self.retrograde { 360.0 - az } else { az })
    }

    /// How far a solstice sightline cut at `t0` is off by `t1`, degrees
    /// (positive = the rise point moved clockwise). Bounded by the
    /// obliquity amplitude's azimuthal image; periodic on `P_OBLIQUITY`.
    /// type-audit: pending(wave-1: latitude), pending(wave-1: return)
    pub fn alignment_drift_deg(&self, latitude: f64, t0: StdDays, t1: StdDays) -> Option<f64> {
        Some(self.solstice_rise_azimuth_at(latitude, t1)? - self.solstice_rise_azimuth_at(latitude, t0)?)
    }

    /// The dating inverse (the sky as archaeological clock): the most
    /// recent epoch at or before `t_now` whose solstice sunrise azimuth
    /// at `latitude` matches `azimuth_deg`. Multi-valued over deep time —
    /// the wobble oscillates — so nearest-past is the contract. `None` on
    /// a locked world, when the azimuth is unreachable at this latitude,
    /// or when the obliquity amplitude is zero (an unmoving sky cannot be
    /// dated).
    /// type-audit: pending(wave-1)
    pub fn alignment_epoch_of(
        &self,
        azimuth_deg: f64,
        latitude: f64,
        t_now: StdDays,
    ) -> Option<StdDays> {
        self.day_length()?;
        if self.forcing.obliquity_amp == 0.0 {
            return None;
        }
        let az = if self.retrograde { 360.0 - azimuth_deg } else { azimuth_deg };
        let x = math::cos(az.to_radians()) * math::cos(latitude.to_radians());
        if x.abs() > 1.0 {
            return None;
        }
        let eps_target = math::asin(x).to_degrees();
        // Invert obliquity_at: sin(τt/P + φ₀) = sin φ₀ + (ε* − mean)/amp.
        let s = math::sin(self.forcing.obliquity_phase)
            + (eps_target - self.forcing.obliquity_mean) / self.forcing.obliquity_amp;
        if s.abs() > 1.0 {
            return None;
        }
        let a = math::asin(s);
        let p = crate::forcing::P_OBLIQUITY;
        let mut best: Option<f64> = None;
        // Two solution families per period: θ = a and θ = π − a.
        for theta in [a, std::f64::consts::PI - a] {
            let t_base =
                (theta - self.forcing.obliquity_phase) / std::f64::consts::TAU * p;
            let k = ((t_now.0 - t_base) / p).floor();
            let t = t_base + k * p;
            if t <= t_now.0 && best.map_or(true, |b| t > b) {
                best = Some(t);
            }
        }
        best.map(StdDays)
    }
```

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy calendar` → PASS.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): solstice-azimuth drift and the alignment dating inverse"
```

---

### Task 8: Founding-alignment facts at the composition root

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (predicate + commit helper)
- Modify: `domains/astronomy/src/lib.rs` (registration)
- Modify: `windows/worldgen/src/lib.rs` (an `alignments` pass after the settlement stage, ~line 1907's stage; reuse the settlement-latitude helper at lib.rs:2478-2483)

**Interfaces:**
- Consumes: Task 7's `solstice_rise_azimuth_at`; worldgen's existing settlement-places + `place_coord` helpers (the exact names are at `windows/worldgen/src/lib.rs:2478-2483` — the heliacal-latitude lookup); worldgen's `sky_of`/generated-sky reconstruction (as used by `windows/lab/src/metrics.rs:144`).
- Produces: `facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES = "founding-solstice-azimuth-degrees"`; `pub fn founding_alignment(world: &mut World, settlement: EntityId, azimuth_deg: f64) -> Result<(), LedgerError>` in astronomy facts; one fact per settlement in every generated spinning world built to settlement depth or deeper.

- [ ] **Step 1: Write the failing worldgen test** — in `windows/worldgen/src/lib.rs` `mod tests` (follow the style of `almanac_context_gathers_everything`, lib.rs:3096):

```rust
    /// The Long Count: every settlement in a spinning generated world
    /// carries its founding solstice-sunrise azimuth; a locked world
    /// carries none.
    #[test]
    fn settlements_carry_founding_alignments() {
        let world = build_seed_42_default_world(); // reuse the module's standard full/settlement-depth builder fixture
        let settlements: Vec<_> = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .iter()
            .map(|f| f.subject)
            .collect();
        assert!(!settlements.is_empty());
        for s in &settlements {
            let n = world
                .ledger
                .facts()
                .iter()
                .filter(|f| {
                    f.subject == *s
                        && f.predicate
                            == hornvale_astronomy::facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES
                })
                .count();
            assert_eq!(n, 1, "settlement {s:?}");
        }
    }
```

(Use the module's existing world-builder fixture; if none builds an unpinned seed-42 world at settlement depth, call the same `build_world_to(..., BuildDepth::Settlements)` invocation `almanac_context_gathers_everything`'s fixture uses. `world.ledger.find` / `facts()` accessors: match the usage at lib.rs:1288.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-worldgen settlements_carry_founding` → FAIL (no such predicate / zero facts).

- [ ] **Step 3: Implement the astronomy side.** In `facts.rs`:

```rust
/// Predicate: the solstice-sunrise azimuth at a settlement's founding
/// epoch, degrees clockwise from north (The Long Count) — the sightline
/// whose deep-time drift makes the sky an archaeological clock.
/// type-audit: bare-ok(identifier-text)
pub const FOUNDING_SOLSTICE_AZIMUTH_DEGREES: &str = "founding-solstice-azimuth-degrees";

/// Commit a settlement's founding solstice alignment (The Long Count).
/// Called by the composition root — the one place settlements and the
/// calendar meet; astronomy owns the predicate and the commit shape.
pub fn founding_alignment(
    world: &mut World,
    settlement: EntityId,
    azimuth_deg: f64,
) -> Result<(), LedgerError> {
    world.ledger.commit(
        fact(
            settlement,
            FOUNDING_SOLSTICE_AZIMUTH_DEGREES,
            Value::Number(azimuth_deg),
        ),
        &world.registry,
    )
}
```

In `lib.rs` `register_concepts` (functional — one founding per settlement):

```rust
    registry.register_predicate(
        facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES,
        true,
        "solstice-sunrise azimuth at a settlement's founding, degrees clockwise from north",
    )?;
```

- [ ] **Step 4: Implement the root pass.** In `windows/worldgen/src/lib.rs`, immediately after the settlement stage completes (after the `stage(...)` at ~line 1907 returns, before the culture stage), add — collecting first to avoid holding the sky borrow across commits:

```rust
    stage("alignments", || -> Result<(), BuildError> {
        // The Long Count: each settlement's founding sightline. Skipped
        // wholesale on locked worlds / polar latitudes (the azimuth
        // function returns None) and on the constant sky (no calendar).
        let pairs: Vec<(EntityId, f64)> = {
            let Some(calendar) = /* the generated sky's calendar, via the same
                reconstruction `almanac_context` uses (sky_of/live provider) */
            else {
                return Ok(());
            };
            settlement_places(&world) // the same places list the heliacal-latitude lookup walks (lib.rs:2478-2483)
                .iter()
                .filter_map(|p| {
                    let coord = place_coord(&world, p.id)?;
                    let az = calendar.solstice_rise_azimuth_at(
                        coord.latitude,
                        hornvale_astronomy::StdDays::new(0.0).unwrap(),
                    )?;
                    Some((p.id, az))
                })
                .collect()
        };
        for (id, az) in pairs {
            hornvale_astronomy::facts::founding_alignment(&mut world, id, az)?;
        }
        Ok(())
    })?;
```

The two commented seams are existing worldgen helpers — bind them to the *actual* local names: the calendar comes from the same live-sky reconstruction `almanac_context` (lib.rs:2694) uses, and the places list + `place_coord` are exactly the calls at lib.rs:2478-2483. Do not invent new reconstruction machinery.

- [ ] **Step 5: Run:** `cargo test -p hornvale-worldgen settlements_carry_founding && cargo test -p hornvale-worldgen 2>&1 | tee /tmp/hv-t8.txt` → PASS (watch for ledger-count golden tests in worldgen that may pin fact totals — update any that count facts per world, they now include one alignment fact per settlement).

- [ ] **Step 6: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(worldgen): founding-solstice-azimuth facts per settlement (The Long Count)"
```

---

### Task 9: Almanac surfacing — the eclipse table and the alignment line

**Files:**
- Modify: `windows/almanac/src/lib.rs` (`NightSkyLines` + `render`)
- Modify: `windows/worldgen/src/lib.rs:2448-2546` (populate the new fields)

**Interfaces:**
- Consumes: `eclipse_events`, `EclipseBody`, `EclipseKind`, `solstice_rise_azimuth_at`, `alignment_drift_deg`.
- Produces: `NightSkyLines.eclipses: Vec<String>` and `NightSkyLines.alignment: Option<String>`, rendered under **The Sky**.

- [ ] **Step 1: Write the failing almanac render test** (in `windows/almanac/src/lib.rs` `mod tests`, extending `sample_context`):

```rust
    #[test]
    fn eclipse_and_alignment_lines_render_under_the_sky() {
        let mut ctx = sample_context();
        if let Some(lines) = ctx.night_sky.as_mut() {
            lines.eclipses = vec![
                "On day 213, the great moon devours the sun whole.".to_string(),
            ];
            lines.alignment = Some(
                "From the first settlement, the midsummer sun rises at azimuth 63.4°; the sightline drifts 0.21° in a thousand years.".to_string(),
            );
        }
        let doc = render(&ctx);
        let sky = doc.split("## The Calendar").next().unwrap();
        assert!(sky.contains("devours the sun whole"));
        assert!(sky.contains("the sightline drifts"));
    }
```

(Adjust the field path to `sample_context`'s actual shape — the `NightSkyLines` value lives wherever the pole-star/heliacal lines already sit in `AlmanacContext`.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-almanac eclipse_and_alignment` → compile error: no field `eclipses`.

- [ ] **Step 3: Implement the almanac side.** In `NightSkyLines` (lib.rs:67-72 region), add:

```rust
    /// Dated-eclipse sentences for the next two years, at most six,
    /// day-ascending (The Long Count). One honest no-eclipse sentence
    /// when the world has moons but no event falls in the window.
    pub eclipses: Vec<String>,
    /// The flagship settlement's founding sightline and its drift rate,
    /// when the world has both a sunrise and a settlement.
    pub alignment: Option<String>,
```

In `render` (after the heliacal loop at lib.rs:155):

```rust
        for line in &lines.eclipses {
            doc.push_str(line);
            doc.push('\n');
        }
        if let Some(alignment) = &lines.alignment {
            doc.push_str(alignment);
            doc.push('\n');
        }
```

(Match the paragraph/blank-line convention the pole-star and heliacal lines use in that block — inspect the surrounding pushes and mirror them exactly.) Fix every `NightSkyLines { .. }` literal in almanac and worldgen with `eclipses: Vec::new(), alignment: None`.

- [ ] **Step 4: Write the failing worldgen test:**

```rust
    /// The seed-42 default world's almanac context carries dated eclipse
    /// lines (it has two moons) and a founding-alignment line.
    #[test]
    fn almanac_context_dates_eclipses_and_the_founding_sightline() {
        let world = build_seed_42_default_world(); // same fixture as Task 8
        let ctx = almanac_context(&world).unwrap();
        let lines = ctx.night_sky.expect("generated sky");
        assert!(!lines.eclipses.is_empty());
        assert!(lines.alignment.is_some());
    }
```

- [ ] **Step 5: Implement the worldgen side** in the `NightSkyLines` construction site (lib.rs:2448-2546), after the heliacal lines (which already computed `latitude` and have `system`/`calendar` in scope):

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
            let verb = match (e.body, e.kind) {
                (EclipseBody::Solar, EclipseKind::Total) => "devours the sun whole",
                (EclipseBody::Solar, EclipseKind::Annular) => {
                    "leaves a burning ring of the sun"
                }
                (EclipseBody::Lunar, _) => "darkens to a bloodred coal",
            };
            format!("On day {:.0}, the {} moon {}.", e.day.get(), ordinal(e.moon), verb)
        })
        .collect();
    if eclipses.is_empty() && !system.moons.is_empty() {
        eclipses.push("No eclipse will darken the sun for two years.".to_string());
    }
    let alignment = calendar
        .solstice_rise_azimuth_at(latitude, t)
        .and_then(|az| {
            let kyr = hornvale_astronomy::StdDays::new(t.get() + 1000.0 * 365.25).unwrap();
            let drift = calendar.alignment_drift_deg(latitude, t, kyr)?;
            Some(format!(
                "From the first settlement, the midsummer sun rises at azimuth {az:.1}°; the sightline drifts {:.2}° in a thousand years.",
                drift.abs()
            ))
        });
```

and add `eclipses, alignment,` to the `NightSkyLines { ... }` literal at lib.rs:2546.

- [ ] **Step 5b: The brightening deep-time line (spec §4).** The almanac already renders a **Deep Time** section when its lines are non-empty (almanac lib.rs:211; the `deep_time_section_renders_when_present_and_is_skipped_when_empty` test names the context field). In worldgen's `almanac_context` (lib.rs:2694-2709), where the deep-time lines vector is assembled, append — only for a generated sky (`system` in scope):

```rust
    deep_time.push(format!(
        "The sun brightens by {:.0} parts in a hundred over a gigayear — the slow fire under every deeper clock.",
        hornvale_astronomy::brightening_per_gyr(&system.star) * 100.0
    ));
```

(Bind `deep_time` to that vector's actual local name.) Extend the Step 4 worldgen test with:

```rust
        assert!(
            ctx.deep_time.iter().any(|l| l.contains("slow fire")),
            "the brightening line reaches Deep Time"
        );
```

(again matching the context field's actual name).

- [ ] **Step 6: Run:** `cargo test -p hornvale-almanac && cargo test -p hornvale-worldgen almanac 2>&1 | tee /tmp/hv-t9.txt` → PASS. Then the full gate: `cargo fmt && make gate 2>&1 | tail -20` → expect FAILURES only in artifact-freshness/golden-almanac tests if any pin the almanac text — re-pin those goldens in this commit (re-baseline in the drifting commit, the standing rule). Committed gallery artifacts regenerate in Task 12.

- [ ] **Step 7: Commit:**

```bash
git add -A && git commit -m "feat(almanac): the reckoning of eclipses and the founding sightline"
```

---

### Task 10: The SKY-23 batteries

**Files:**
- Modify: `domains/astronomy/tests/genesis_properties.rs` (append four batteries)

**Interfaces:**
- Consumes: everything shipped in Tasks 1–7; the file's existing helpers (`generate`, `SkyPins`).
- Produces: test coverage only — no runtime surface.

- [ ] **Step 1: Write the batteries** (they should pass immediately — they verify shipped machinery "on the scale moons enjoy"; any failure is a real find):

```rust
/// SKY-23 close-out, star battery: over 256 seeds, the drawn mass stays
/// in range and every derivation is monotone in it (luminosity, solar
/// disc, habitable zone, brightening).
#[test]
fn star_battery_mass_bounds_and_monotone_derivations() {
    use hornvale_astronomy::{brightening_per_gyr, generate_star};
    let mut last: Option<(f64, f64, f64, f64, f64)> = None;
    let mut stars: Vec<_> = (0..256u64)
        .map(|s| generate_star(hornvale_kernel::Seed(s).derive("astronomy")))
        .collect();
    stars.sort_by(|a, b| a.mass.get().total_cmp(&b.mass.get()));
    for s in &stars {
        assert!((0.6..=1.4).contains(&s.mass.get()));
        assert!(s.habitable_zone.inner().get() < s.habitable_zone.outer().get());
        let row = (
            s.mass.get(),
            s.luminosity.get(),
            s.habitable_zone.inner().get(),
            s.habitable_zone.outer().get(),
            brightening_per_gyr(s),
        );
        if let Some(prev) = last {
            assert!(row.1 >= prev.1 && row.2 >= prev.2 && row.3 >= prev.3 && row.4 >= prev.4,
                "derivations must be monotone in mass: {prev:?} -> {row:?}");
        }
        last = Some(row);
    }
}

/// SKY-23 close-out, anchor battery: over 256 seeds the orbit sits inside
/// the habitable zone, obliquity stays in its drawn range, the Kepler
/// relation holds, and locked worlds never have a solar hour.
#[test]
fn anchor_battery_orbit_kepler_and_rotation_invariants() {
    use hornvale_astronomy::{Rotation, SkyPins, calendar_of, generate};
    let mut saw_locked = false;
    let mut saw_spinning = false;
    let mut saw_retrograde = false;
    for seed in 0..256u64 {
        let outcome = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let s = &outcome.system;
        let zone = s.star.habitable_zone;
        assert!(
            (zone.inner().get()..=zone.outer().get()).contains(&s.anchor.orbit.get()),
            "seed {seed}: orbit outside the zone"
        );
        // Kepler III in the model card's own units: Y = 365.25·√(a³/M).
        let expected_year =
            365.25 * (s.anchor.orbit.get().powi(3) / s.star.mass.get()).sqrt();
        assert!(
            (s.anchor.year.get() - expected_year).abs() < 1e-6,
            "seed {seed}: year {} vs Kepler {expected_year}",
            s.anchor.year.get()
        );
        let calendar = calendar_of(s);
        match s.anchor.rotation {
            Rotation::Locked => {
                saw_locked = true;
                assert!(calendar.day_length().is_none());
            }
            Rotation::Spinning { retrograde, .. } => {
                saw_spinning = true;
                saw_retrograde |= retrograde;
                assert!(calendar.day_length().is_some());
            }
        }
    }
    assert!(saw_locked && saw_spinning && saw_retrograde, "every regime reachable");
}
```

(Before finalizing the Kepler assertion, read the year derivation in `domains/astronomy/src/anchor.rs` and copy its exact formula — if it differs from `365.25·√(a³/M)`, assert against the formula the model card actually declares. The obliquity range assertion likewise: copy the drawn bound from `anchor.rs`'s obliquity draw and assert it.)

```rust
/// SKY-23 close-out, neighbors battery: over 256 seeds the count stays in
/// 2–5, every position is a legal sky coordinate, distances are positive
/// and sorted brightest-first, and regeneration is byte-identical.
#[test]
fn neighbor_battery_counts_coordinates_and_determinism() {
    use hornvale_astronomy::{SkyPins, generate};
    for seed in 0..256u64 {
        let a = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let b = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        assert_eq!(a, b, "seed {seed}: regeneration must be byte-identical");
        let n = &a.system.neighbors;
        assert!((2..=5).contains(&n.len()), "seed {seed}: {} neighbors", n.len());
        for x in n {
            assert!((0.0..360.0).contains(&x.right_ascension));
            assert!((-90.0..=90.0).contains(&x.declination));
            assert!(x.distance.get() > 0.0);
        }
    }
}

/// The Long Count, alignment battery: node longitudes in range on every
/// seed, and the dating inverse round-trips across seeds and latitudes.
#[test]
fn alignment_battery_nodes_and_dating_round_trip() {
    use hornvale_astronomy::{Rotation, SkyPins, StdDays, calendar_of, generate};
    for seed in 0..128u64 {
        let outcome = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let s = &outcome.system;
        for m in &s.moons {
            assert!((0.0..360.0).contains(&m.node_longitude_deg), "seed {seed}");
        }
        if matches!(s.anchor.rotation, Rotation::Locked) || s.forcing.obliquity_amp == 0.0 {
            continue;
        }
        let calendar = calendar_of(s);
        for lat in [-55.0, -20.0, 20.0, 55.0] {
            let t = StdDays::new(0.27 * hornvale_astronomy::forcing::P_OBLIQUITY).unwrap();
            let Some(az) = calendar.solstice_rise_azimuth_at(lat, t) else { continue };
            let epoch = calendar
                .alignment_epoch_of(az, lat, StdDays::new(t.get() + 1.0).unwrap())
                .expect("a wobbling sky dates its own alignments");
            assert!((epoch.get() - t.get()).abs() < 1.0, "seed {seed} lat {lat}");
        }
    }
}
```

(`forcing` module visibility: `hornvale_astronomy::forcing::P_OBLIQUITY` is already `pub` — forcing.rs:17. Neighbor field names: copy from `neighborhood.rs` if `right_ascension`/`declination`/`distance` differ.)

- [ ] **Step 2: Run:** `cargo test -p hornvale-astronomy --test genesis_properties 2>&1 | tee /tmp/hv-t10.txt` → PASS; check runtime stays fast-tier (< ~30 s — genesis-only, it will be).

- [ ] **Step 3: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "test(astronomy): star/anchor/neighbor/alignment batteries (SKY-23 close-out)"
```

---

### Task 11: Lab metrics

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`registry()` — six new `Metric` entries, astronomy-rung except the last)

**Interfaces:**
- Consumes: `AstronomyView { system, calendar, .. }`, `SettlementView` (+ `AsRef<AstronomyView>`), `eclipse_events`, `node_regression_period`, `brightening_per_gyr`, `alignment_drift_deg`, ledger settlement-latitude facts.
- Produces: metrics `eclipse-cadence-solar`, `eclipse-cadence-lunar`, `totality-fraction`, `standstill-period-days`, `brightening-per-gyr`, `alignment-drift-deg-per-kyr`.

- [ ] **Step 1: Write the failing test** (in `metrics.rs` `mod tests` or the module's existing metric-coverage test — there is a test asserting every metric extracts on a default world; extend it, plus):

```rust
    #[test]
    fn the_long_count_metrics_extract_on_seed_42() {
        let names = [
            "eclipse-cadence-solar",
            "eclipse-cadence-lunar",
            "totality-fraction",
            "standstill-period-days",
            "brightening-per-gyr",
            "alignment-drift-deg-per-kyr",
        ];
        let reg = registry();
        for name in names {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
        // Extraction smoke: build once at the deepest rung these need.
        let v = SettlementView::build(Seed(42), &SkyPins::default()).unwrap();
        for name in names {
            let m = reg.iter().find(|m| m.name == name).unwrap();
            // Follow the file's existing apply-to-view test helper; a
            // Number or honest Absent both count as extracting.
            let _ = apply_for_test(m, &v);
        }
    }
```

(Match the file's actual test scaffolding: there are existing tests applying `Extractor` variants to built views — copy their invocation exactly rather than inventing `apply_for_test`.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-lab the_long_count_metrics` → FAIL (metrics missing).

- [ ] **Step 3: Implement** — append to `registry()` after the last astronomy metric, matching the house entry style (metrics.rs:621-641):

```rust
        Metric {
            name: "eclipse-cadence-solar",
            doc: "Dated solar eclipses per year anywhere on the world, from a 20-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.5, 1.0, 2.0, 4.0, 8.0, 12.0, 16.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                eclipse_cadence(v, hornvale_astronomy::eclipses::EclipseBody::Solar)
            }),
        },
        Metric {
            name: "eclipse-cadence-lunar",
            doc: "Dated lunar eclipses per year, from a 20-year scan",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.5, 1.0, 2.0, 4.0, 8.0, 12.0, 16.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                eclipse_cadence(v, hornvale_astronomy::eclipses::EclipseBody::Lunar)
            }),
        },
        Metric {
            name: "totality-fraction",
            doc: "Fraction of dated solar eclipses that are total (vs annular) over a 20-year scan; Absent if none fall",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                use hornvale_astronomy::eclipses::{EclipseBody, EclipseKind};
                let events = scan_20_years(v);
                let solar: Vec<_> = events
                    .iter()
                    .filter(|e| matches!(e.body, EclipseBody::Solar))
                    .collect();
                if solar.is_empty() {
                    return MetricValue::Absent;
                }
                let total =
                    solar.iter().filter(|e| matches!(e.kind, EclipseKind::Total)).count();
                MetricValue::Number(total as f64 / solar.len() as f64)
            }),
        },
        Metric {
            name: "standstill-period-days",
            doc: "Nodal-regression (standstill) period of the innermost moon, standard days; Absent if moonless",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0, 100000.0],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                match v.system.moons.first() {
                    None => MetricValue::Absent,
                    Some(m) => MetricValue::Number(
                        hornvale_astronomy::node_regression_period(
                            v.system.anchor.year,
                            m.period,
                            m.inclination_deg,
                        )
                        .get(),
                    ),
                }
            }),
        },
        Metric {
            name: "brightening-per-gyr",
            doc: "The star's fractional main-sequence brightening per gigayear",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.05, 0.10, 0.15, 0.20, 0.25],
            },
            extract: Extractor::Astronomy(|v: &AstronomyView| {
                MetricValue::Number(hornvale_astronomy::brightening_per_gyr(&v.system.star))
            }),
        },
        Metric {
            name: "alignment-drift-deg-per-kyr",
            doc: "Absolute solstice-sunrise azimuth drift over the first kiloyear at the flagship settlement's latitude; Absent when locked, unplaced, or polar",
            summary: SummaryKind::Numeric {
                bucket_edges: &[0.0, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0],
            },
            extract: Extractor::Settlement(|v| {
                let a: &AstronomyView = v.as_ref();
                // Flagship latitude from the ledger, exactly as the
                // worldgen heliacal lookup reads it (worldgen lib.rs:2478
                // and the latitude accessor at lib.rs:880-895).
                let Some(lat) = flagship_latitude(v) else {
                    return MetricValue::Absent;
                };
                let t0 = hornvale_astronomy::StdDays::new(0.0).unwrap();
                let t1 = hornvale_astronomy::StdDays::new(1000.0 * 365.25).unwrap();
                match a.calendar.alignment_drift_deg(lat, t0, t1) {
                    Some(d) => MetricValue::Number(d.abs()),
                    None => MetricValue::Absent,
                }
            }),
        },
```

with two file-private helpers above `registry()`:

```rust
/// The 20-year dated-eclipse scan shared by the cadence metrics.
fn scan_20_years(v: &AstronomyView) -> Vec<hornvale_astronomy::eclipses::EclipseEvent> {
    let until = hornvale_astronomy::StdDays::new(20.0 * 365.25).unwrap();
    hornvale_astronomy::eclipse_events(
        &v.system,
        &v.calendar,
        hornvale_astronomy::StdDays::new(0.0).unwrap(),
        until,
    )
}

fn eclipse_cadence(
    v: &AstronomyView,
    body: hornvale_astronomy::eclipses::EclipseBody,
) -> MetricValue {
    let n = scan_20_years(v).iter().filter(|e| e.body == body).count();
    MetricValue::Number(n as f64 / 20.0)
}
```

and a `flagship_latitude(v: &SettlementView) -> Option<f64>` helper reading the first `IS_SETTLEMENT` subject's `hornvale_settlement::LATITUDE` fact from `v.world()`'s ledger — copy the accessor pattern from `windows/worldgen/src/lib.rs:880-895` verbatim (same ledger API).

(The 20-year window is deliberately fixed in *standard* days — a schedule constant like `heliacal.rs`'s `SAMPLES = 400`, never a function of the drawn year, so the scan cost and precision are seed-independent.)

- [ ] **Step 4: Run:** `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t11.txt` → PASS. Note: `hornvale lab list-metrics` reference output and census `schema.json` now drift — regenerated in Task 12 / at the AWS close regen respectively.

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(lab): eclipse cadence, standstill, brightening, and alignment-drift metrics"
```

---

### Task 12: Artifacts, model card, decision 0054, registry re-scores

**Files:**
- Modify: `docs/superpowers/specs/2026-07-05-campaign-2-the-sky-design.md` (§5 model card — append rows)
- Create: `docs/decisions/0054-no-orbital-migration.md`
- Modify: `book/src/frontier/idea-registry.md` (five row re-scores)
- Regenerate: `book/src/gallery/*`, `book/src/reference/*`, `book/src/laboratory/*` (via script), `docs/audits/type-audit-report.md`

- [ ] **Step 1: Model card.** Append to the Campaign 2 spec's §5 derived/approximated/drawn card, under **approximated** (matching its row format):

```markdown
- **Nodal regression** (The Long Count): P_node = (4/3)·Y²/(P_sid·cos i) — the
  lunar-theory leading term; Earth check 17.9 yr vs the true 18.61.
- **Syzygy longitude** (The Long Count): L_moon = L_sun + 360·phase — exact at
  the syzygies where it is consumed.
- **Lunar shadow threshold** (The Long Count): θ_lunar = 0.64·θ_solar,
  Luna–Sol-calibrated to ~1.5 umbral lunar eclipses/year.
- **Secular brightening** (The Long Count): b = 0.10·M^2.5 per Gyr,
  Sol-calibrated; the habitable zone stays a genesis-epoch derivation.
- **Solstice-rise azimuth** (The Long Count): cos az = sin ε(t)/cos φ;
  refraction and horizon dip ignored.
```

and under **drawn**: `**Node longitude Ω₀** (The Long Count): uniform [0, 360) per admitted moon, stream "moon-nodes".` (If Task 4 tuned `LUNAR_SHADOW_FACTOR` off 0.64, write the shipped value.)

- [ ] **Step 2: Decision 0054.** Parallel campaigns claim decision numbers concurrently (0053 went to single-craton hypsometry mid-plan) — **re-check `ls docs/decisions/` for the next free number before writing** and renumber this file and every reference below if 0054 is taken. Create `docs/decisions/0054-no-orbital-migration.md` following the format of `docs/decisions/0052-deploy-built-wasm.md` (read it first; keep the same header/sections). Substance:

```markdown
# 0054 — No semi-major-axis migration

**Status:** ratified (The Long Count)
**Date:** 2026-07-14

SKY-1's residual listed secular semi-major-axis migration alongside stellar
brightening. Migration is declined: disk-driven migration ends before a
stable habitable world exists, and post-genesis secular drift of an isolated
planet's semi-major axis is effectively zero on every timescale the sim can
express. Adding it would be invention, not derivation — the "guess wearing
physics' clothes" failure mode. Tidal orbital decay is real physics but
belongs to SKY-tidal-braking (its own open row: day-lengthening, lunar
recession, the death of total eclipses). Brightening ships (star.rs
`luminosity_at`); the anchor's orbital elements beyond the Milankovitch
triad plus nodal regression stay fixed. Supersede with new information, not
a fresh opinion.
```

- [ ] **Step 3: Registry re-scores** in `book/src/frontier/idea-registry.md` (edit rows in place, keep IDs; per `book/src/frontier/CLAUDE.md`):
  - **SKY-6** → status `shipped`, drop the "Still open" clause, note dated occurrences + lunar eclipses + standstill facts shipped in The Long Count.
  - **SKY-eclipse-seasons** → status `shipped`, note "folded into SKY-6's close-out (The Long Count): inclination + nodal regression date the seasons and the standstill cycle."
  - **SKY-1** → status `shipped`, note brightening shipped, migration declined (decision 0054), "orbital drift beyond the triad" closed by 0054.
  - **SKY-stale-alignments** → status `shipped`, note the ground half (solstice azimuth drift, dating inverse, founding facts) shipped in The Long Count.
  - **SKY-23** → drop the "broader appetite remains open" clause; note the star/anchor/neighbor batteries shipped.
  - Add "enabled-by: The Long Count" notes to **SKY-tidal-braking** (standstill/eclipse machinery to kill) and **SKY-calendar-hardness** (standstill period now a census column).
  Then: `cargo test -p hornvale --test docs_consistency` → PASS.

- [ ] **Step 4: Regenerate artifacts** (never the census locally):

```bash
SKIP_CENSUS=1 scripts/regenerate-artifacts.sh
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
mdbook build book
```

Inspect the diff: the seed-42 almanacs gain the eclipse table + alignment line, the concepts dump gains four predicates + the reworded eclipse kind, the streams manifest gains `astronomy/moon-nodes`, the lab metric list gains six rows. Nothing else may drift — an unexpected diff is a bug, stop and investigate (systematic-debugging).

- [ ] **Step 5: Full gate:** `cargo fmt && make gate 2>&1 | tail -20` → PASS.

- [ ] **Step 6: Commit:**

```bash
git add -A && git commit -m "docs(the-long-count): model card, decision 0054, registry re-scores, artifact regen"
```

---

## Close-out (after all tasks; superpowers:closing-a-campaign governs)

Not plan tasks — the campaign close checklist, recorded here so nothing is lost:

1. `make preflight` from the branch; absorb main into the branch at stage boundaries per the standing rule.
2. **Warn Nathan, then** run the one AWS census regen (`make regen-remote`) — new facts and metrics changed the ledger and the census schema (`schema.json` bump rides the regen). Never locally.
3. Chronicle entry (`book/src/chronicle/the-long-count.md`), freshness sweep of the sky chapters, Confidence Gradient re-score if a bet moved.
4. Retrospective in `docs/retrospectives/` (decision 0020).
5. Merge per superpowers:finishing-a-development-branch; verify CI (artifact drift check) green.
