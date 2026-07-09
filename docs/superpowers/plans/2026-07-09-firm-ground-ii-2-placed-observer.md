# Firm Ground II, Plan 2: The Placed Observer — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the observer a real place on the globe — `ObserverContext` gains a `GeoCoord`, the default almanac observes from the flagship cell, daylight varies with latitude (SKY-8), and the visible sky is culled to what an observer at that place could *ever* see (SEQ-4/5).

**Architecture:** `ObserverContext` (a kernel type) gains an additive `position: Option<GeoCoord>` — reusing the kernel's existing `GeoCoord`, not a new type. The astronomy `Calendar` gains a latitude-aware `daylight_fraction_at(t, latitude)` (the sunrise equation). `GeneratedSky::phenomena` culls the sky by the observer's hemisphere: **on a spinning world every body rises and sets from every longitude, so the whole sky is visible; on a tidally locked world the sky is fixed, so the day hemisphere sees only the sun and the night hemisphere only the moons and stars.** A position-blind observation (`position == None`) preserves the legacy nowhere-in-particular sky byte-for-byte. `windows/worldgen` resolves the flagship's committed latitude/longitude facts into the vantage and observes through it, and reports the flagship's own daylight range in the almanac's calendar.

**Tech Stack:** Rust edition 2024, std only (decision 0004). Crates touched: `hornvale-kernel`, `hornvale-astronomy`, `hornvale-worldgen`. No new draws, no new streams, no new pins — Plan 2 is stream-neutral. Determinism via deterministic ledger reads.

## Global Constraints

- **Builds on Plan 1.** This plan assumes Plan 1 has landed: `Calendar` holds a `forcing: crate::forcing::OrbitalForcing` field (replacing the old `obliquity: Degrees`), and `daylight_fraction(t)`/`season_phase(t)` read it. Task 2's `daylight_fraction_at` reads `self.forcing.obliquity_at(t.0)`. **Before starting, confirm** `grep -n "forcing" domains/astronomy/src/calendar.rs` shows the `forcing` field; if not, Plan 1 is not yet merged into this worktree and this plan cannot proceed.
- **`serde` + `serde_json` only; no new crates** (decision 0004; enforced by `cli/tests/architecture.rs`). This plan adds no dependency.
- **Reuse the kernel `GeoCoord`** (`kernel/src/geosphere.rs:15`, exported at `kernel/src/lib.rs:17`), never a new position type (spec §4). It is `#[derive(Clone, Copy, Debug, PartialEq)]`, so `Option<GeoCoord>` keeps `ObserverContext`'s `Copy`.
- **Additive kernel seam.** `ObserverContext`'s doc contract — "adding a field here must not break existing sources" (`kernel/src/phenomena.rs:77`) — is honored: the field is additive, `ObserverContext::at` keeps setting it to `None`, and every tier-0 source (`ConstantSun`, `UniformClimate`) and its tests keep compiling and passing unchanged.
- **Determinism (constitutional):** same seed + same pins → byte-identical world and almanac. No wall-clock, no `HashMap`/`HashSet`. Plan 2 draws nothing, so there is no stream to isolate; the guard is that the placed observation is a pure function of committed facts (asserted by the determinism tests).
- **Ever-visible culling, not momentary culling** (design decision, ratified this session over the spec's literal §4 wording): a body is culled iff it is *never* visible from the observer's location over the world's cycle. Spinning → nothing culled (whole sky). Locked → the far hemisphere culled. This preserves the Y2 exit criterion (`cli/tests/sky_exit_criterion.rs` — spinning worlds keep the sun-headed, moon-seated pantheon) and fixes the real locked-world bug (today a locked world's `phenomena` emits *both* hemispheres at once, so a night-side warren "sees" a permanently-far-side sun). Religion's *affective* reaction to the vantage (a nocturnal species reading the sun as hostile) is **out of scope** — logged as idea PSY-4, a Year-2-family/epistemic-layer consumer feature; this campaign builds no consumer (spec §1, §11).
- **SKY-8 is spinning-only.** Latitude daylight means nothing on a tidally locked world (no day/night cycle; `Calendar::day_length()` is already `None` there), so `daylight_fraction_at` returns `None` for locked worlds and locked worlds cull purely by hemisphere.
- **The re-baseline is Plan 3.** Plan 2 changes derived output — the almanac's calendar daylight line (all spinning worlds, at the flagship's latitude) and locked worlds' culled phenomena/religion. Committed artifacts are expected to be stale after Plan 2; the single re-baseline is Plan 3. The per-commit gate is `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; do **not** regenerate `book/` artifacts in this plan.
- **`#![warn(missing_docs)]`:** every public item, field, and variant gets a `///` doc comment.
- **The full gate before every commit**, `cargo fmt` (no `--check`) as the final step of each task, never `--no-verify`.
- **Baseline:** confirm test counts before starting — `cargo test -p hornvale-kernel 2>&1 | tail -3`, `cargo test -p hornvale-astronomy 2>&1 | tail -3`, `cargo test -p hornvale-worldgen 2>&1 | tail -3` — and after each task.

---

## File Structure

| File | Action | Responsibility |
| --- | --- | --- |
| `kernel/src/phenomena.rs` | Modify | `ObserverContext` gains `position: Option<GeoCoord>`; `at` sets `None`; add `at_position`. |
| `domains/astronomy/src/calendar.rs` | Modify | `Calendar::daylight_fraction_at(t, latitude)` — the sunrise equation (SKY-8). |
| `domains/astronomy/src/provider.rs` | Modify | `GeneratedSky::phenomena` culls the sky by the observer's hemisphere (SEQ-5). |
| `windows/worldgen/src/lib.rs` | Modify | `place_coord` resolver; place the observer in `observed_phenomena`/`observed_phenomena_as_in`; report the flagship's latitude daylight in `calendar_lines`. |
| `domains/astronomy/tests/genesis_properties.rs` | Modify | Property battery: latitude-daylight endpoints; locked hemisphere culling; spinning whole-sky. |

**Dataflow:** `worldgen` reads a place's committed `LATITUDE`/`LONGITUDE` facts (`domains/settlement/src/lib.rs:31-33`, set from `Geosphere::coord` at genesis) → builds `ObserverContext { …, position }` → `GeneratedSky::phenomena` reads `ctx.position` and culls. `calendar_lines` reads the flagship latitude → `Calendar::daylight_fraction_at` → the almanac's daylight line.

**Why the blast radius is small and correct:** spinning worlds (including the default seed-42 world) cull nothing, so their `phenomena` and religion are byte-unchanged; only their almanac *calendar daylight line* changes (now at the flagship's latitude). Locked worlds cull by hemisphere, so their religion becomes place-dependent — the observable that proves the vantage is real.

---

### Task 1: The kernel seam — `ObserverContext` gains a position

**Files:**
- Modify: `kernel/src/phenomena.rs`
- Modify: `windows/worldgen/src/lib.rs` (one struct literal, compile-fix only)

**Interfaces:**
- Produces: `ObserverContext { place, time, lens, position: Option<GeoCoord> }`; `ObserverContext::at(place, time)` → `position: None`; `ObserverContext::at_position(place, time, coord: GeoCoord)` → `position: Some(coord)`, identity lens.

- [ ] **Step 1: Import `GeoCoord`.** At the top of `kernel/src/phenomena.rs`, alongside the existing `use crate::field::WorldTime;` / `use crate::ledger::EntityId;`, add:

```rust
use crate::geosphere::GeoCoord;
```

- [ ] **Step 2: Add the field.** In `struct ObserverContext` (`kernel/src/phenomena.rs:79`), add a field after `lens`:

```rust
    /// The observer's position on the globe, if placed. `None` is a
    /// position-blind observation (nowhere in particular) — the sky is not
    /// culled by horizon. Placed by the composition root from the flagship
    /// cell (SEQ-4); consumed by providers to cull the visible sky (SEQ-5).
    pub position: Option<GeoCoord>,
```

- [ ] **Step 3: Update `at` and add `at_position`.** Replace the `impl ObserverContext` block:

```rust
impl ObserverContext {
    /// An unlensed, position-blind observation at a place and time (identity
    /// lens, no globe position — the sky is not culled by horizon).
    pub fn at(place: EntityId, time: WorldTime) -> Self {
        ObserverContext {
            place,
            time,
            lens: PerceptionLens::identity(),
            position: None,
        }
    }

    /// An unlensed observation from a real place on the globe (identity lens).
    /// Providers cull the visible sky to this position's hemisphere.
    pub fn at_position(place: EntityId, time: WorldTime, position: GeoCoord) -> Self {
        ObserverContext {
            place,
            time,
            lens: PerceptionLens::identity(),
            position: Some(position),
        }
    }
}
```

- [ ] **Step 4: Write the failing tests.** In `kernel/src/phenomena.rs` `mod tests`, add (the test module's `ctx()` uses `at`, so it inherits `position: None`):

```rust
    #[test]
    fn at_is_position_blind_and_at_position_carries_a_coord() {
        let blind = ObserverContext::at(EntityId(1), WorldTime { day: 0.0 });
        assert!(blind.position.is_none());
        let placed = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord { latitude: 12.0, longitude: -30.0 },
        );
        assert_eq!(placed.position, Some(GeoCoord { latitude: 12.0, longitude: -30.0 }));
    }

    #[test]
    fn observe_ignores_observer_position() {
        // Aggregation is position-agnostic; culling is the provider's job.
        let a = FixedSource(vec![ph("breeze", 0.3333), ph("sun", 1.0)]);
        let blind = observe(&[&a], &ctx());
        let placed = observe(
            &[&a],
            &ObserverContext {
                position: Some(GeoCoord { latitude: 1.0, longitude: 2.0 }),
                ..ctx()
            },
        );
        assert_eq!(blind, placed);
    }
```

The test module needs `GeoCoord` in scope; `use super::*;` re-exports it via the module-level `use crate::geosphere::GeoCoord;` from Step 1.

- [ ] **Step 5: Compile-fix the one worldgen struct literal.** `windows/worldgen/src/lib.rs:448` builds an `ObserverContext { place, time, lens }` literal that now misses `position`. Add `position: None,` to it (Task 4 replaces this with the resolved coord):

```rust
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: perception_lens(&def.perception),
            position: None,
        },
```

- [ ] **Step 6: Run + gate.** `cargo test -p hornvale-kernel && cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`. Expected PASS, byte-identical everywhere (position is `None` on every existing path). `cargo fmt`.

- [ ] **Step 7: Commit.**

```bash
git add kernel/src/phenomena.rs windows/worldgen/src/lib.rs
git commit -m "feat(kernel): ObserverContext gains an optional globe position (SEQ-4)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 2: Latitude daylight — the sunrise equation (SKY-8)

**Files:**
- Modify: `domains/astronomy/src/calendar.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::GeoCoord` (latitude only, here); Plan 1's `Calendar.forcing` and `Calendar::year_phase`.
- Produces: `pub fn daylight_fraction_at(&self, t: StdDays, latitude: f64) -> Option<f64>` — the fraction of the local day that is lit, at a latitude; `None` on a tidally locked world. The existing planet-wide `daylight_fraction(t)` is unchanged (it stays the reference/position-blind value used by `is_daylight` and `observation_time`).

- [ ] **Step 1: Write the failing tests.** In `domains/astronomy/src/calendar.rs` `mod tests`, add:

```rust
    #[test]
    fn equatorial_daylight_is_a_flat_half_all_year() {
        let cal = calendar_of(&spinning_system());
        let year = cal.year_length().get();
        for k in 0..24 {
            let t = StdDays::new(k as f64 * year / 24.0).unwrap();
            let f = cal.daylight_fraction_at(t, 0.0).unwrap();
            assert!((f - 0.5).abs() < 1e-9, "equator not flat at t={t:?}: {f}");
        }
    }

    #[test]
    fn high_latitude_reaches_polar_day_and_night() {
        // A tilted, spinning world with the forcing zeroed (constant obliquity,
        // no drift): latitude 85° hits polar day near midsummer and polar night
        // near midwinter. The year-phase offset (Plan 1) shifts where in `t`
        // the solstices fall, so scan the year for the extremes rather than
        // assuming they sit at 0.25/0.75 of it.
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(23.5).unwrap()),
            forcing: Some(crate::pins::ForcingPin::Zero),
            ..SkyPins::default()
        };
        let cal = calendar_of(&generate(Seed(42), &pins).unwrap().system);
        let year = cal.year_length().get();
        let (mut max, mut min) = (0.0_f64, 1.0_f64);
        for k in 0..365 {
            let t = StdDays::new(k as f64 * year / 365.0).unwrap();
            let f = cal.daylight_fraction_at(t, 85.0).unwrap();
            max = max.max(f);
            min = min.min(f);
        }
        assert!(max > 0.999, "expected polar day at 85°, got {max}");
        assert!(min < 0.001, "expected polar night at 85°, got {min}");
    }

    #[test]
    fn a_locked_world_has_no_latitude_daylight() {
        let cal = calendar_of(&locked_system());
        assert!(cal.daylight_fraction_at(StdDays::new(0.0).unwrap(), 45.0).is_none());
    }
```

The test module imports `SkyPins`, `RotationPin`, `MoonsPin`, `Degrees` already; `ForcingPin` comes in via `crate::pins::ForcingPin` (fully qualified above).

- [ ] **Step 2: Run to see it fail.** `cargo test -p hornvale-astronomy daylight 2>&1 | tail -8` — expected FAIL (`no method named daylight_fraction_at`).

- [ ] **Step 3: Implement.** In `domains/astronomy/src/calendar.rs`, add the method inside `impl Calendar` (after `daylight_fraction`):

```rust
    /// Daylight fraction of the local day at a latitude (SKY-8): the standard
    /// sunrise equation. ~0.5 flat at the equator, running to 1.0/0.0 (polar
    /// day/night) toward the poles. Reads the time-varying obliquity, so a
    /// world's daylight geometry drifts with its axial tilt. `None` on a
    /// tidally locked world, which has no day/night cycle.
    pub fn daylight_fraction_at(&self, t: StdDays, latitude: f64) -> Option<f64> {
        self.day?;
        // Solar declination: the sub-solar latitude oscillates over the year,
        // its amplitude the (time-varying) obliquity.
        let obliquity = self.forcing.obliquity_at(t.0).to_radians();
        let declination = obliquity * (std::f64::consts::TAU * self.year_phase(t)).sin();
        let phi = latitude.to_radians();
        // cos H0 = −tan φ · tan δ; the clamp yields polar day (−1 → H0 = π,
        // fraction 1) and polar night (1 → H0 = 0, fraction 0) past the polar
        // circles.
        let cos_h0 = (-phi.tan() * declination.tan()).clamp(-1.0, 1.0);
        Some(cos_h0.acos() / std::f64::consts::PI)
    }
```

- [ ] **Step 4: Run + gate.** `cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`. Expected PASS. `cargo fmt`.

- [ ] **Step 5: Commit.**

```bash
git add domains/astronomy/src/calendar.rs
git commit -m "feat(astronomy): latitude-dependent daylight via the sunrise equation (SKY-8)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 3: Ever-visible hemisphere culling (SEQ-5)

**Files:**
- Modify: `domains/astronomy/src/provider.rs`

**Interfaces:**
- Consumes: `ObserverContext.position` (Task 1).
- Produces: `GeneratedSky::phenomena` culls the sky by the observer's hemisphere. Position-blind (`None`) is byte-identical to today.

- [ ] **Step 1: Write the failing tests.** In `domains/astronomy/src/provider.rs` `mod tests`, extend the imports and add tests. Change the test-module `use` line to include `GeoCoord` and `Venue`:

```rust
    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, Seed, Venue, WorldTime};
```

Then add:

```rust
    #[test]
    fn a_locked_night_side_vantage_sees_moons_and_stars_but_no_sun() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let night = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord { latitude: 10.0, longitude: 180.0 },
        );
        let ph = s.phenomena(&night);
        assert!(!ph.iter().any(|p| p.venue == Venue::DaySky), "night side must not see the sun");
        assert!(ph.iter().any(|p| p.kind == NIGHT_STAR), "night side sees the stars");
        assert!(ph.iter().any(|p| p.description.contains("moon")), "night side sees the moon");
    }

    #[test]
    fn a_locked_day_side_vantage_sees_the_sun_and_no_night_sky() {
        let s = sky(SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let day = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord { latitude: 10.0, longitude: 0.0 },
        );
        let ph = s.phenomena(&day);
        assert!(ph.iter().any(|p| p.venue == Venue::DaySky), "day side sees the sun");
        assert!(!ph.iter().any(|p| p.kind == NIGHT_STAR), "day side must not see the stars");
        assert!(!ph.iter().any(|p| p.description.contains("moon")), "day side must not see the moon");
    }

    #[test]
    fn a_spinning_placed_observer_sees_the_whole_sky() {
        // On a spinning world every body rises and sets, so the sky is whole
        // from any longitude at any hour — nothing is culled.
        let s = sky(SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::exact(1).unwrap()),
            ..SkyPins::default()
        });
        let obs = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 10.5 },
            GeoCoord { latitude: 40.0, longitude: 25.0 },
        );
        let ph = s.phenomena(&obs);
        assert!(ph.iter().any(|p| p.venue == Venue::DaySky), "sun present");
        assert!(ph.iter().any(|p| p.kind == NIGHT_STAR), "stars present (whole sky)");
        assert!(ph.iter().any(|p| p.description.contains("moon")), "moon present");
    }
```

- [ ] **Step 2: Run to see them fail.** `cargo test -p hornvale-astronomy vantage 2>&1 | tail -12` — expected FAIL (today's locked sky pushes both hemispheres, so the night-side test's sun assertion fails).

- [ ] **Step 3: Implement the culling.** In `domains/astronomy/src/provider.rs`, replace the body of `impl PhenomenaSource for GeneratedSky`'s `phenomena` with:

```rust
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon> {
        let t = self.t(ctx.time);
        let mut out = Vec::new();

        // Ever-visible hemisphere culling (SEQ-5): a placed observer sees only
        // the bodies that ever rise at their location. On a spinning world the
        // sky turns, so every body rises and sets from every longitude — the
        // whole sky is visible. On a tidally locked world the sky is fixed: the
        // substellar point sits on the prime meridian, so the day hemisphere
        // (|longitude| < 90°) sees only the sun and the night hemisphere only
        // the moons and stars. A position-blind observation (`position == None`)
        // preserves the legacy nowhere-in-particular sky, byte-for-byte: the
        // sun and moons are always present, the neighbor stars only in darkness.
        let locked = matches!(self.system.anchor.rotation, Rotation::Locked);
        let (show_sun, show_moons, show_stars) = match (locked, ctx.position) {
            (true, Some(coord)) => {
                let day_side = coord.longitude.abs() < 90.0;
                (day_side, !day_side, !day_side)
            }
            (false, Some(_)) => (true, true, true),
            (false, None) => (true, true, matches!(self.calendar.is_daylight(t), Some(false))),
            (true, None) => (true, true, true),
        };

        if show_sun {
            match &self.system.anchor.rotation {
                Rotation::Spinning { day } => out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: format!("the sun, a {}", self.system.star.class_name),
                    period_days: Some(round2(day.get())),
                    salience: 1.0,
                    venue: Venue::DaySky,
                }),
                Rotation::Locked => out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: "a sun fixed forever above the day side".to_string(),
                    period_days: None,
                    salience: 1.0,
                    venue: Venue::DaySky,
                }),
            }
        }

        if show_moons {
            for moon in &self.system.moons {
                let angular = moon.angular_diameter_rel;
                out.push(Phenomenon {
                    kind: CELESTIAL_BODY.to_string(),
                    description: format!("a {} moon", size_word(angular)),
                    period_days: Some(round2(moon.period.get())),
                    salience: round2(0.35 + 0.35 * angular.min(2.0) / 2.0),
                    venue: Venue::NightSky,
                });
            }
        }

        let spinning = matches!(self.system.anchor.rotation, Rotation::Spinning { .. });
        if spinning && self.system.anchor.obliquity.get() > 0.0 {
            out.push(Phenomenon {
                kind: SEASONAL_CYCLE.to_string(),
                description: "the slow swelling and shrinking of daylight".to_string(),
                period_days: Some(round2(self.system.anchor.year.get())),
                salience: round2(0.5 * self.system.anchor.obliquity.get() / 35.0),
                venue: Venue::Ambient,
            });
        }

        if show_stars {
            for neighbor in &self.system.neighbors {
                out.push(Phenomenon {
                    kind: NIGHT_STAR.to_string(),
                    description: neighbor.night_description(),
                    period_days: None,
                    salience: round2(
                        (0.1 + 0.1 * (1.0 + neighbor.apparent_brightness).ln()).clamp(0.1, 0.6),
                    ),
                    venue: Venue::NightSky,
                });
            }
        }

        out
    }
```

The seasonal cycle is `Venue::Ambient` (felt through the world, not a sky body) and is never hemisphere-culled — it keeps its exact prior gate (`spinning && obliquity > 0`).

- [ ] **Step 4: Run + gate.** `cargo test -p hornvale-astronomy && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`. Expected PASS — the new tests pass, and every existing provider test (all position-blind, using `ctx(day)` → `at`) stays byte-identical because the `None` arms reproduce today's behavior exactly. `cargo fmt`.

- [ ] **Step 5: Commit.**

```bash
git add domains/astronomy/src/provider.rs
git commit -m "feat(astronomy): cull the sky to the observer's hemisphere (SEQ-5)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

If a pre-existing provider test fails: it must be a test that passed a *position* (none do today) — check that the failing test is genuinely position-blind. The `None` arms are a transcription of the prior code; any drift there is the bug, not the test.

---

### Task 4: The composition root places the observer

**Files:**
- Modify: `windows/worldgen/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::GeoCoord`; settlement's `LATITUDE`/`LONGITUDE` predicates; Task 2's `Calendar::daylight_fraction_at`.
- Produces: `fn place_coord(world, place) -> Option<GeoCoord>`; `observed_phenomena`/`observed_phenomena_as_in` observe from the flagship's position; `calendar_lines` reports the flagship's latitude daylight.

- [ ] **Step 1: Import `GeoCoord`.** In `windows/worldgen/src/lib.rs`, add `GeoCoord` to the kernel `use` list (`kernel/src/lib.rs:17` exports it):

```rust
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, GeoCoord, Geosphere, LedgerError, ObserverContext,
    PerceptionLens, PhenomenaSource, Phenomenon, RegistryError, Seed, Value, World, WorldTime,
    observe,
};
```

- [ ] **Step 2: Add the resolver.** In `windows/worldgen/src/lib.rs`, near `observed_phenomena` (around line 337), add:

```rust
/// The geographic position of a place, from its committed latitude/longitude
/// facts (each set from `Geosphere::coord` at genesis — `domains/settlement`).
/// `None` for a place carrying no such facts (a legacy or non-settlement
/// place), leaving the observation position-blind — the pre-vantage behavior.
fn place_coord(world: &World, place: EntityId) -> Option<GeoCoord> {
    let latitude = match world.ledger.value_of(place, hornvale_settlement::LATITUDE)? {
        Value::Number(n) => *n,
        _ => return None,
    };
    let longitude = match world.ledger.value_of(place, hornvale_settlement::LONGITUDE)? {
        Value::Number(n) => *n,
        _ => return None,
    };
    Some(GeoCoord { latitude, longitude })
}
```

- [ ] **Step 3: Place the observer in `observed_phenomena`.** Replace the body of `observed_phenomena` (line 338):

```rust
/// The tier-0/1/2 phenomena sources, observed from the world's first place —
/// the flagship (SEQ-4). The vantage's hemisphere culls the sky (SEQ-5).
pub fn observed_phenomena(world: &World, day: f64) -> Result<Vec<Phenomenon>, BuildError> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let position = place_coord(world, place);
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: PerceptionLens::identity(),
            position,
        },
    ))
}
```

- [ ] **Step 4: Place the observer in `observed_phenomena_as_in`.** In `observed_phenomena_as_in` (line 433), resolve the coord and thread it into the struct literal Task 1 stubbed with `None`:

```rust
    let day = observation_time(world, def.perception.activity)?;
    let position = place_coord(world, place);
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: perception_lens(&def.perception),
            position,
        },
    ))
```

- [ ] **Step 5: Report the flagship's latitude daylight in `calendar_lines`.** In `calendar_lines` (line 930), replace the daylight-swell block (currently `if day_std.is_some() && system.anchor.obliquity.get() > 0.0 { … planet-wide half … }`) with a flagship-latitude version:

```rust
    if day_std.is_some() && system.anchor.obliquity.get() > 0.0 {
        // The daylight range at the flagship's own latitude (SKY-8) — the
        // placed observer's sky. The year-phase offset (Plan 1) moves where the
        // solstices fall in `t`, so scan the year for the extremes. Falls back
        // to the equator (a flat half) if no vantage resolves.
        let latitude = hornvale_terrain::places(world)
            .first()
            .and_then(|p| place_coord(world, p.id))
            .map(|c| c.latitude)
            .unwrap_or(0.0);
        let year = calendar.year_length().get();
        let (mut max, mut min) = (0.0_f64, 1.0_f64);
        for k in 0..365 {
            let t = hornvale_astronomy::StdDays::new(k as f64 * year / 365.0).unwrap();
            if let Some(f) = calendar.daylight_fraction_at(t, latitude) {
                max = max.max(f);
                min = min.min(f);
            }
        }
        lines.push(format!(
            "Daylight swells to {:.0}% at midsummer and shrinks to {:.0}% at midwinter.",
            max * 100.0,
            min * 100.0
        ));
    }
```

- [ ] **Step 6: Write the tests.** In `windows/worldgen/src/lib.rs` `mod tests`, add:

```rust
    #[test]
    fn the_default_vantage_resolves_to_the_flagship_and_is_deterministic() {
        let world = generated(42);
        let place = hornvale_terrain::places(&world).first().unwrap().id;
        assert!(place_coord(&world, place).is_some(), "the flagship must carry a coord");
        let a = observed_phenomena(&world, 0.0).unwrap();
        let b = observed_phenomena(&world, 0.0).unwrap();
        assert_eq!(a, b, "the placed observation must be deterministic");
    }

    #[test]
    fn a_locked_worlds_pantheon_sees_exactly_one_hemisphere() {
        // The placed observer made observable: on a locked world the flagship
        // sees the sun XOR the night sky, never both (the pre-Plan-2 bug).
        let locked = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(hornvale_astronomy::RotationPin::Locked),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let ph = observed_phenomena(&locked, 0.0).unwrap();
        let sees_sun = ph.iter().any(|p| p.description.contains("sun"));
        let sees_night = ph.iter().any(|p| p.kind == hornvale_astronomy::NIGHT_STAR);
        assert!(
            sees_sun ^ sees_night,
            "a locked flagship sees exactly one hemisphere (sun XOR night sky)"
        );
    }
```

- [ ] **Step 7: Run the full gate + fix expected churn.** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`.
  - **Expected to stay green:** the Y2 exit criterion `moons_flip_flips_the_calendar_and_grows_the_pantheon_without_displacing_the_head` (spinning → whole sky, unchanged) and `rotation_flip_flips_the_religion` (a locked world still commits an `eternal` sentiment — a day-side flagship from the aperiodic sun, a night-side one from the aperiodic neighbor stars).
  - **If a test asserting exact almanac/phenomena text fails:** the change is intended (spinning worlds' calendar daylight line now reflects the flagship latitude; locked worlds' sky is hemisphere-culled). Update the test's expectation in place to the new, correct value. Do **not** regenerate `book/` artifacts — that is Plan 3.
  - `cargo fmt`.

- [ ] **Step 8: Commit.**

```bash
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): observe from the flagship's place; report its latitude daylight (SEQ-4, SKY-8)

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

### Task 5: The property battery

**Files:**
- Modify: `domains/astronomy/tests/genesis_properties.rs`

**Interfaces:**
- Consumes: `Calendar::daylight_fraction_at`, `GeneratedSky::phenomena`, `ObserverContext::at_position`.
- Produces: seed-sweep properties for latitude daylight and hemisphere culling.

- [ ] **Step 1: Add the tests.** In `domains/astronomy/tests/genesis_properties.rs`, append (each test carries its own `use`s so the file header stays minimal):

```rust
#[test]
fn equatorial_daylight_is_flat_and_every_latitude_stays_in_range() {
    use hornvale_astronomy::{StdDays, calendar_of};
    for seed in 0..64u64 {
        let system = generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                ..SkyPins::default()
            },
        )
        .unwrap()
        .system;
        let cal = calendar_of(&system);
        let year = system.anchor.year.get();
        for k in 0..8 {
            let t = StdDays::new(k as f64 * year / 8.0).unwrap();
            let equator = cal.daylight_fraction_at(t, 0.0).unwrap();
            assert!((equator - 0.5).abs() < 1e-9, "seed {seed}: equator not flat: {equator}");
            for lat in [-80.0, -30.0, 30.0, 80.0] {
                let f = cal.daylight_fraction_at(t, lat).unwrap();
                assert!((0.0..=1.0).contains(&f), "seed {seed}: daylight {f} out of range at lat {lat}");
            }
        }
    }
}

#[test]
fn a_locked_worlds_hemispheres_cull_the_sky() {
    use hornvale_astronomy::{GeneratedSky, NIGHT_STAR};
    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, Venue, WorldTime};
    for seed in 0..32u64 {
        let outcome = generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::Locked),
                ..SkyPins::default()
            },
        )
        .unwrap();
        let sky = GeneratedSky::new(outcome);
        let day = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord { latitude: 5.0, longitude: 0.0 },
        );
        let night = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 0.0 },
            GeoCoord { latitude: 5.0, longitude: 179.0 },
        );
        let day_ph = sky.phenomena(&day);
        let night_ph = sky.phenomena(&night);
        assert!(day_ph.iter().any(|p| p.venue == Venue::DaySky), "seed {seed}: day side sees no sun");
        assert!(!day_ph.iter().any(|p| p.kind == NIGHT_STAR), "seed {seed}: day side sees stars");
        assert!(!night_ph.iter().any(|p| p.venue == Venue::DaySky), "seed {seed}: night side sees the sun");
        assert!(night_ph.iter().any(|p| p.kind == NIGHT_STAR), "seed {seed}: night side sees no stars");
    }
}

#[test]
fn a_spinning_worlds_sky_is_whole_from_any_placed_vantage() {
    use hornvale_astronomy::{GeneratedSky, NIGHT_STAR};
    use hornvale_kernel::{EntityId, GeoCoord, ObserverContext, PhenomenaSource, Venue, WorldTime};
    for seed in 0..32u64 {
        let outcome = generate(
            Seed(seed),
            &SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                moons: Some(MoonsPin::exact(2).unwrap()),
                ..SkyPins::default()
            },
        )
        .unwrap();
        let sky = GeneratedSky::new(outcome);
        let obs = ObserverContext::at_position(
            EntityId(1),
            WorldTime { day: 3.5 },
            GeoCoord { latitude: 55.0, longitude: -120.0 },
        );
        let ph = sky.phenomena(&obs);
        assert!(ph.iter().any(|p| p.venue == Venue::DaySky), "seed {seed}: no sun");
        assert!(ph.iter().any(|p| p.kind == NIGHT_STAR), "seed {seed}: no stars (should be whole sky)");
        assert_eq!(
            ph.iter().filter(|p| p.description.contains("moon")).count(),
            2,
            "seed {seed}: both moons should be visible on a spinning world"
        );
    }
}
```

- [ ] **Step 2: Run the full gate + commit.** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Expected PASS. `cargo fmt`.

```bash
git add domains/astronomy/tests/genesis_properties.rs
git commit -m "test(astronomy): latitude-daylight endpoints + hemisphere-culling property battery

Claude-Session: https://claude.ai/code/session_01KyEfpua8WtkbCJoMHHgsy9"
```

---

## Self-Review Notes

**Spec coverage (against `2026-07-09-firm-ground-ii-design.md` §4, §6, and the session's ratified refinements):**
- §4 kernel seam (`ObserverContext` gains a `GeoCoord`, additive) → Task 1. §4/§6 resolution and default vantage (worldgen resolves the flagship → observes through it) → Task 4. §4 latitude daylight (SKY-8) → Task 2, consumed in the almanac by Task 4. §4 the SEQ-5 culling (fixing `provider.rs`'s `is_daylight` bug) → Task 3. §8 property battery (latitude endpoints, locked-culling) → Task 5.
- **Ratified refinement (this session):** the spec's *momentary* day/night culling is realized as *ever-visible* culling — spinning worlds see the whole sky (preserving the Y2 exit criterion and leaving the default world's religion byte-unchanged), locked worlds cull by hemisphere (fixing the real both-hemispheres bug and making the vantage observable). Religion's *affective* reaction to the vantage is deferred (idea PSY-4), consistent with the spec's §11 deferral of "the epistemic almanac's consumption of the vantage (Year 4)."
- **Deferred to Plan 3:** the single re-baseline (the almanac gallery artifacts drift — spinning worlds' calendar daylight line, locked worlds' culled sky) and the tier-2 book chapter. This plan touches no `book/` file and regenerates no artifact.

**Read-the-engine findings that shaped the plan (standing rule 1):**
- `provider.rs:297,313,335` today pushes the sun *and* moons *and* (for locked worlds) neighbor stars unconditionally — a locked world's `phenomena` emits both hemispheres at once. Ever-visible culling fixes this; the `None` arms preserve it byte-for-byte for position-blind callers.
- `cli/tests/sky_exit_criterion.rs:127` (`moons_flip…`) asserts the sun heads the pantheon and moons seat deities on a **spinning** world → preserved (spinning = whole sky). `:79` (`rotation_flip…`) asserts a **locked** world commits an `eternal` sentiment → preserved (a locked flagship always sees an aperiodic body: the sun day-side, the neighbor stars night-side).
- Settlement facts already store `LATITUDE`/`LONGITUDE` (`domains/settlement/src/lib.rs:31-33`), each set from `Geosphere::coord` at genesis (`worldgen/src/lib.rs:731-748`), so `place_coord` is a ledger read — no geosphere rebuild, and it equals the cell's coord by construction.

**Determinism watch:** Plan 2 draws nothing and adds no stream or pin — there is no stream-consumption contract to isolate. The placed observation is a pure function of committed lat/lon facts and the deterministic sky; the determinism tests (`the_default_vantage_resolves…`, and the existing `generated_worlds_are_deterministic`) are the guard. `ObserverContext.position` is not serialized (the type is not `Serialize`); it is rebuilt at observation time from the ledger, so no save-format surface changes.

**Type consistency:** `ObserverContext.position: Option<GeoCoord>` (Task 1) is read by `GeneratedSky::phenomena` (Task 3) and written by worldgen (Task 4). `Calendar::daylight_fraction_at(StdDays, f64) -> Option<f64>` (Task 2) is called by `calendar_lines` (Task 4) and the battery (Task 5). `place_coord(&World, EntityId) -> Option<GeoCoord>` (Task 4) is the single resolver both observation sites and `calendar_lines` use.

**Pre-existing test churn (expected, not regressions):** any test asserting exact almanac calendar text for a spinning world (the daylight line now reads at the flagship's latitude) or exact locked-world phenomena/religion text (now hemisphere-culled). Update in place to the new correct value; defer `book/` artifact regeneration to Plan 3.
