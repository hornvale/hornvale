# The Long Count Implementation Plan (re-scoped)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **Re-scope note (2026-07-14):** the original 12-task plan included the
> eclipse package (node draw, dated events, lunar eclipses, standstill
> facts). That core was **ceded to the parallel campaign Eclipse Seasons**
> (spec @83efb1f; adjudicated by Nathan — see the scope amendment in this
> campaign's spec). This plan is the surviving, non-overlapping remainder.
> **This campaign draws no new stream and touches no draw path.**

**Goal:** Close the non-eclipse astronomy residuals — secular stellar brightening (SKY-1, with the migration decline), the alignment ground half (SKY-stale-alignments), and the star/anchor/neighbor verification batteries (SKY-23) — with their almanac lines and lab metrics.

**Architecture:** Pure derivations over elements already held: `star.rs` gains a draw-free luminosity slope; `calendar.rs` gains solstice-azimuth functions and a dating inverse; the composition root (`windows/worldgen`) commits one founding-alignment fact per settlement and feeds two new almanac lines; `windows/lab` gains two metrics; `genesis_properties.rs` gains four batteries.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only. All transcendentals through `hornvale_kernel::math` (libm, decision 0041). No `HashMap`/`HashSet`. No new crates.

**Spec:** `docs/superpowers/specs/2026-07-14-the-long-count-design.md` (as amended — §3 does not govern)

## Global Constraints

- **Determinism is constitutional.** Same seed → byte-identical worlds. **No new streams, no draw changes**: `golden_seed_42.rs` must pass untouched at every commit — if it moves, stop, something is wrong (systematic-debugging).
- **`f64` transcendentals** (`sin`, `cos`, `asin`, `acos`, `atan2`, `powf`, `tan`) go through `hornvale_kernel::math`, never `f64::sin` etc. (`sqrt`, `abs`, `floor`, `ceil`, `rem_euclid`, `to_radians`, `to_degrees` are IEEE-exact and stay as std methods — match surrounding code.)
- **Quantization at emit only:** facts are quantized by `Ledger::commit`; never quantize inside compute paths.
- **Float ordering:** `total_cmp` with a deterministic tie-break.
- **Every crate has `#![warn(missing_docs)]`** — every new public item, field, and variant gets a one-line doc comment, and every new pub-boundary primitive gets a `type-audit:` tag (`pending(wave-1)` for physical quantities, `bare-ok(ratio)`/`bare-ok(identifier-text)`/`bare-ok(index)` per class — copy the tag style of the adjacent field).
- **No wall-clock time.** Time is `StdDays` / `WorldTime { day: f64 }`.
- **Gate cadence:** during a task iterate with scoped tests (`cargo test -p hornvale-astronomy <name>`); before each task's commit run `cargo fmt` then `make gate-fast`; run the full `make gate` at Tasks 3, 4, and 6 before committing. Never `--no-verify`.
- **Worktree:** execute in the worktree at `~/.config/superpowers/worktrees/hornvale/the-long-count/`, branch `the-long-count` (already created). SDD scratch lives in the worktree's `.superpowers/sdd/`, never the main checkout.
- **Do not touch** `eclipses.rs`/`eclipse.rs`, `moons.rs`, `streams.rs`, or `provider.rs`'s eclipse block — that surface belongs to the parallel Eclipse Seasons campaign. If a step seems to need it, stop and escalate.
- **Censuses are never regenerated locally.** Artifact regeneration uses `SKIP_CENSUS=1`. The census regen happens on AWS pre-merge with Nathan's explicit go-ahead (close-time; coordinate with Eclipse Seasons for a single shared regen).

---

### Task 1: Secular stellar brightening

**Files:**
- Modify: `domains/astronomy/src/star.rs`
- Modify: `domains/astronomy/src/facts.rs` (predicate + commit)
- Modify: `domains/astronomy/src/lib.rs` (registration + re-export)

**Interfaces:**
- Consumes: `Star`, `Anchor`, `StdDays`, `hornvale_kernel::math`.
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

(If any unit newtype rejects in-crate tuple construction, use its `::new(x).unwrap()` constructor — do the same everywhere this plan constructs units.)

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-astronomy brighten` → compile errors (functions not defined).

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

Add an assertion to `facts.rs`'s existing genesis-commit test (the fixture near facts.rs:107's test neighborhood) that exactly one `brightening-per-gyr` fact lands on the world subject.

- [ ] **Step 4: Run:** `cargo test -p hornvale-astronomy star && cargo test -p hornvale-astronomy facts && cargo test -p hornvale-astronomy --test golden_seed_42` → PASS (goldens untouched — this task draws nothing).

- [ ] **Step 5: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "feat(astronomy): secular stellar brightening, derived and anchored (SKY-1 close-out)"
```

---

### Task 2: Alignment functions (calendar ground half)

**Files:**
- Modify: `domains/astronomy/src/calendar.rs` (three methods, after `precession_offset_deg` at line 652)

**Interfaces:**
- Consumes: `self.forcing.obliquity_at`, `self.forcing.{obliquity_mean, obliquity_amp, obliquity_phase}`, `crate::forcing::P_OBLIQUITY`, `self.day_length()`, `self.retrograde`.
- Produces (on `Calendar`):
  - `pub fn solstice_rise_azimuth_at(&self, latitude: f64, t: StdDays) -> Option<f64>`
  - `pub fn alignment_drift_deg(&self, latitude: f64, t0: StdDays, t1: StdDays) -> Option<f64>`
  - `pub fn alignment_epoch_of(&self, azimuth_deg: f64, latitude: f64, t_now: StdDays) -> Option<StdDays>`

- [ ] **Step 1: Write the failing tests** (in `calendar.rs` `mod tests`, reusing its `spinning_system`/`locked_system` fixtures; the drift and epoch tests need a nonzero drawn `obliquity_amp` — the unpinned seed-42 system has one; if the fixture pins forcing to zero, build an unpinned spinning system for these tests instead):

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

    /// The drift IS the obliquity wobble: nonzero across a half-period,
    /// and home again after a full one.
    #[test]
    fn alignment_drift_is_the_obliquity_wobble() {
        let cal = calendar_of(&generate(Seed(42), &SkyPins::default()).unwrap().system);
        if cal.day_length().is_none() {
            return; // a locked draw can't test drift; seed 42 spins today
        }
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
        let cal = calendar_of(&generate(Seed(42), &SkyPins::default()).unwrap().system);
        if cal.day_length().is_none() {
            return;
        }
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

### Task 3: Founding-alignment facts at the composition root

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (predicate + commit helper)
- Modify: `domains/astronomy/src/lib.rs` (registration)
- Modify: `windows/worldgen/src/lib.rs` (an `alignments` pass after the settlement stage, ~line 1907's stage; reuse the settlement-latitude helper at lib.rs:2478-2483)

**Interfaces:**
- Consumes: Task 2's `solstice_rise_azimuth_at`; worldgen's existing settlement-places + `place_coord` helpers (the exact names are at `windows/worldgen/src/lib.rs:2478-2483` — the heliacal-latitude lookup); worldgen's generated-sky reconstruction (as used by `almanac_context`, lib.rs:2694).
- Produces: `facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES = "founding-solstice-azimuth-degrees"`; `pub fn founding_alignment(world: &mut World, settlement: EntityId, azimuth_deg: f64) -> Result<(), LedgerError>` in astronomy facts; one fact per settlement in every generated spinning world built to settlement depth or deeper.

- [ ] **Step 1: Write the failing worldgen test** — in `windows/worldgen/src/lib.rs` `mod tests` (follow the style of `almanac_context_gathers_everything`, lib.rs:3096, and reuse its world-builder fixture; if none builds an unpinned seed-42 world at settlement depth, call the same `build_world_to(..., BuildDepth::Settlements)` invocation that fixture uses; ledger accessors: match the usage at lib.rs:1288):

```rust
    /// The Long Count: every settlement in a spinning generated world
    /// carries its founding solstice-sunrise azimuth; a locked world
    /// carries none.
    #[test]
    fn settlements_carry_founding_alignments() {
        let world = build_seed_42_default_world();
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
                reconstruction `almanac_context` (lib.rs:2694) uses */
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

- [ ] **Step 5: Run:** `cargo test -p hornvale-worldgen settlements_carry_founding && cargo test -p hornvale-worldgen 2>&1 | tee /tmp/hv-t3.txt` → PASS (watch for ledger-count golden tests in worldgen that may pin fact totals — update any that count facts per world; they now include one alignment fact per settlement). Then `cargo fmt && make gate 2>&1 | tail -20` → PASS.

- [ ] **Step 6: Commit:**

```bash
git add -A && git commit -m "feat(worldgen): founding-solstice-azimuth facts per settlement (The Long Count)"
```

---

### Task 4: Almanac surfacing — the founding sightline and the slow fire

**Files:**
- Modify: `windows/almanac/src/lib.rs` (`NightSkyLines` + `render`)
- Modify: `windows/worldgen/src/lib.rs:2448-2546` (populate the alignment line) and lib.rs:2694-2709 (the deep-time brightening line)

**Interfaces:**
- Consumes: `solstice_rise_azimuth_at`, `alignment_drift_deg`, `brightening_per_gyr`.
- Produces: `NightSkyLines.alignment: Option<String>` rendered under **The Sky**; one brightening sentence in **Deep Time**.

- [ ] **Step 1: Write the failing almanac render test** (in `windows/almanac/src/lib.rs` `mod tests`, extending `sample_context`; adjust the field path to `sample_context`'s actual shape — the `NightSkyLines` value lives wherever the pole-star/heliacal lines already sit in `AlmanacContext`):

```rust
    #[test]
    fn the_alignment_line_renders_under_the_sky() {
        let mut ctx = sample_context();
        if let Some(lines) = ctx.night_sky.as_mut() {
            lines.alignment = Some(
                "From the first settlement, the midsummer sun rises at azimuth 63.4°; the sightline drifts 0.21° in a thousand years.".to_string(),
            );
        }
        let doc = render(&ctx);
        let sky = doc.split("## The Calendar").next().unwrap();
        assert!(sky.contains("the sightline drifts"));
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-almanac the_alignment_line` → compile error: no field `alignment`.

- [ ] **Step 3: Implement the almanac side.** In `NightSkyLines` (lib.rs:67-72 region), add:

```rust
    /// The flagship settlement's founding sightline and its drift rate
    /// (The Long Count), when the world has both a sunrise and a
    /// settlement.
    pub alignment: Option<String>,
```

In `render` (after the heliacal loop at lib.rs:155), matching the paragraph/blank-line convention the pole-star and heliacal lines use in that block:

```rust
        if let Some(alignment) = &lines.alignment {
            doc.push_str(alignment);
            doc.push('\n');
        }
```

Fix every `NightSkyLines { .. }` literal in almanac and worldgen with `alignment: None`.

- [ ] **Step 4: Write the failing worldgen test:**

```rust
    /// The seed-42 default world's almanac context carries the founding
    /// sightline and the brightening deep-time line.
    #[test]
    fn almanac_context_carries_sightline_and_slow_fire() {
        let world = build_seed_42_default_world(); // same fixture as Task 3
        let ctx = almanac_context(&world).unwrap();
        let lines = ctx.night_sky.expect("generated sky");
        assert!(lines.alignment.is_some());
        assert!(
            ctx.deep_time.iter().any(|l| l.contains("slow fire")),
            "the brightening line reaches Deep Time"
        );
    }
```

(Match `ctx.deep_time` and `ctx.night_sky` to the context struct's actual field names.)

- [ ] **Step 5: Implement the worldgen side** in the `NightSkyLines` construction site (lib.rs:2448-2546), after the heliacal lines (which already computed `latitude` and have `system`/`calendar` in scope):

```rust
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

adding `alignment,` to the `NightSkyLines { ... }` literal at lib.rs:2546. Then in `almanac_context` (lib.rs:2694-2709), where the deep-time lines vector is assembled and only for a generated sky (`system` in scope; bind `deep_time` to that vector's actual local name):

```rust
    deep_time.push(format!(
        "The sun brightens by {:.0} parts in a hundred over a gigayear — the slow fire under every deeper clock.",
        hornvale_astronomy::brightening_per_gyr(&system.star) * 100.0
    ));
```

- [ ] **Step 6: Run:** `cargo test -p hornvale-almanac && cargo test -p hornvale-worldgen almanac 2>&1 | tee /tmp/hv-t4.txt` → PASS. Then the full gate: `cargo fmt && make gate 2>&1 | tail -20` → expect FAILURES only in artifact-freshness/golden-almanac tests if any pin the almanac text — re-pin those goldens in this commit (re-baseline in the drifting commit, the standing rule). Committed gallery artifacts regenerate in Task 6.

- [ ] **Step 7: Commit:**

```bash
git add -A && git commit -m "feat(almanac): the founding sightline and the slow fire under every deeper clock"
```

---

### Task 5: The SKY-23 batteries

**Files:**
- Modify: `domains/astronomy/tests/genesis_properties.rs` (append four batteries)

**Interfaces:**
- Consumes: Tasks 1–2; the file's existing helpers (`generate`, `SkyPins`).
- Produces: test coverage only — no runtime surface. (No node/eclipse assertions — that surface belongs to Eclipse Seasons.)

- [ ] **Step 1: Write the batteries** (they should pass immediately — they verify shipped machinery "on the scale moons enjoy"; any failure is a real find):

```rust
/// SKY-23 close-out, star battery: over 256 seeds, the drawn mass stays
/// in range and every derivation is monotone in it (luminosity, HZ,
/// brightening).
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
/// the habitable zone, the Kepler relation holds, and locked worlds never
/// have a solar hour.
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

(Before finalizing the Kepler assertion, read the year derivation in `domains/astronomy/src/anchor.rs` and copy its exact formula — if it differs from `365.25·√(a³/M)`, assert against the formula the model card actually declares. Add an obliquity-range assertion with the drawn bound copied from `anchor.rs`'s obliquity draw.)

```rust
/// SKY-23 close-out, neighbors battery: over 256 seeds the count stays in
/// 2–5, every position is a legal sky coordinate, distances are positive,
/// and regeneration is byte-identical.
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

/// The Long Count, alignment battery: the dating inverse round-trips
/// across seeds and latitudes.
#[test]
fn alignment_battery_dating_round_trip() {
    use hornvale_astronomy::{Rotation, SkyPins, StdDays, calendar_of, generate};
    for seed in 0..128u64 {
        let outcome = generate(hornvale_kernel::Seed(seed), &SkyPins::default()).unwrap();
        let s = &outcome.system;
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

(Neighbor field names: copy from `neighborhood.rs` if `right_ascension`/`declination`/`distance` differ. `hornvale_astronomy::forcing::P_OBLIQUITY` is `pub` — forcing.rs:17.)

- [ ] **Step 2: Run:** `cargo test -p hornvale-astronomy --test genesis_properties 2>&1 | tee /tmp/hv-t5.txt` → PASS; runtime stays fast-tier (genesis-only).

- [ ] **Step 3: Commit** — `cargo fmt && make gate-fast`, then:

```bash
git add -A && git commit -m "test(astronomy): star/anchor/neighbor/alignment batteries (SKY-23 close-out)"
```

---

### Task 6: Lab metrics, artifacts, decision 0054, registry re-scores

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`registry()` — two new `Metric` entries)
- Modify: `docs/superpowers/specs/2026-07-05-campaign-2-the-sky-design.md` (§5 model card — two rows)
- Create: `docs/decisions/0054-no-orbital-migration.md`
- Modify: `book/src/frontier/idea-registry.md` (three row re-scores)
- Regenerate: `book/src/gallery/*`, `book/src/reference/*`, `book/src/laboratory/*` (via script), `docs/audits/type-audit-report.md`

**Interfaces:**
- Consumes: `AstronomyView { system, calendar, .. }`, `SettlementView` (+ `AsRef<AstronomyView>`), `brightening_per_gyr`, `alignment_drift_deg`, ledger settlement-latitude facts.
- Produces: metrics `brightening-per-gyr`, `alignment-drift-deg-per-kyr`.

- [ ] **Step 1: Write the failing metric test** (extend the file's existing metric-coverage test scaffolding — there are tests applying `Extractor` variants to built views; copy their invocation exactly, and apply each new metric at the Settlement rung, treating a Number or an honest Absent as extracting):

```rust
    #[test]
    fn the_long_count_metrics_extract_on_seed_42() {
        let names = ["brightening-per-gyr", "alignment-drift-deg-per-kyr"];
        let reg = registry();
        for name in names {
            assert!(reg.iter().any(|m| m.name == name), "{name} registered");
        }
    }
```

- [ ] **Step 2: Run to verify failure:** `cargo test -p hornvale-lab the_long_count_metrics` → FAIL.

- [ ] **Step 3: Implement** — append to `registry()` after the last astronomy metric, matching the house entry style (metrics.rs:621-641):

```rust
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

with a file-private `flagship_latitude(v: &SettlementView) -> Option<f64>` helper reading the first `IS_SETTLEMENT` subject's `hornvale_settlement::LATITUDE` fact from `v.world()`'s ledger — copy the accessor pattern from `windows/worldgen/src/lib.rs:880-895` verbatim (same ledger API).

- [ ] **Step 4: Run:** `cargo test -p hornvale-lab 2>&1 | tee /tmp/hv-t6.txt` → PASS.

- [ ] **Step 5: Model card.** Append to the Campaign 2 spec's §5 derived/approximated/drawn card, under **approximated** (matching its row format):

```markdown
- **Secular brightening** (The Long Count): b = 0.10·M^2.5 per Gyr,
  Sol-calibrated; the habitable zone stays a genesis-epoch derivation.
- **Solstice-rise azimuth** (The Long Count): cos az = sin ε(t)/cos φ;
  refraction and horizon dip ignored.
```

(The nodal-regression and eclipse rows belong to Eclipse Seasons — do not add them here.)

- [ ] **Step 6: Decision 0054.** Parallel campaigns claim decision numbers concurrently (0053 went to single-craton hypsometry mid-plan) — **re-check `ls docs/decisions/` for the next free number before writing** and renumber this file and every reference if 0054 is taken. Create `docs/decisions/0054-no-orbital-migration.md` following the format of `docs/decisions/0052-deploy-built-wasm.md` (read it first; keep the same header/sections). Substance:

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
triad stay fixed. Supersede with new information, not a fresh opinion.
```

- [ ] **Step 7: Registry re-scores** in `book/src/frontier/idea-registry.md` (edit rows in place, keep IDs; per `book/src/frontier/CLAUDE.md`):
  - **SKY-1** → status `shipped`, note brightening shipped in The Long Count, migration declined (decision 0054).
  - **SKY-stale-alignments** → status `shipped`, note the ground half (solstice azimuth drift, dating inverse, founding facts) shipped in The Long Count; the sky half shipped earlier.
  - **SKY-23** → drop the "broader appetite remains open" clause; note the star/anchor/neighbor batteries shipped in The Long Count.
  - **Do not touch SKY-6 / SKY-eclipse-seasons** — those rows belong to Eclipse Seasons' close.
  Then: `cargo test -p hornvale --test docs_consistency` → PASS.

- [ ] **Step 8: Regenerate artifacts** (never the census locally):

```bash
SKIP_CENSUS=1 scripts/regenerate-artifacts.sh
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
mdbook build book
```

Inspect the diff: the seed-42 almanacs gain the sightline + slow-fire lines, the concepts dump gains two predicates, the lab metric list gains two rows. **No streams-manifest change** (this campaign draws nothing). Anything else drifting is a bug — stop and investigate (systematic-debugging).

- [ ] **Step 9: Full gate:** `cargo fmt && make gate 2>&1 | tail -20` → PASS.

- [ ] **Step 10: Commit:**

```bash
git add -A && git commit -m "docs(the-long-count): metrics, model card, decision 0054, registry re-scores, artifact regen"
```

---

## Close-out (after all tasks; superpowers:closing-a-campaign governs)

Not plan tasks — the campaign close checklist, recorded here so nothing is lost:

1. `make preflight` from the branch; absorb main into the branch at stage boundaries per the standing rule — **especially important this campaign: Eclipse Seasons is landing in parallel and both touch astronomy.**
2. **Warn Nathan, then** run the one AWS census regen (`make regen-remote`) — new facts and metrics changed the ledger and the census schema. **Coordinate with Eclipse Seasons for a single shared regen after both merge** rather than two.
3. Chronicle entry (`book/src/chronicle/the-long-count.md`), freshness sweep of the sky chapters, Confidence Gradient re-score if a bet moved.
4. Retrospective in `docs/retrospectives/` (decision 0020) — include the moon-nodes collision and its adjudication as a process lesson.
5. Merge per superpowers:finishing-a-development-branch; verify CI (artifact drift check) green.
6. **Standstill follow-up:** once Eclipse Seasons merges, a small follow-up can ship standstill facts + metric on its node machinery (its spec's deferred-standstills seam). Registry gets the pointer at close.
