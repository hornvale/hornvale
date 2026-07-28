# The Occlusion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Status:** COMPLETE — merged. See [the chronicle](../../../book/src/chronicle/the-occlusion.md).

**Goal:** Make the world stop hiding what it computes — cloud cover occludes
the sky it currently contradicts, weather follows the observer instead of the
capital, bare compass directions work, and the 101 placed strange sites become
reachable.

**Architecture:** Cloud is the *second* occluder; the codebase already has the
first (`Venue::DaySky`/`NightSky` — daylight hides the stars) and never named
it. Occlusion rides the existing multiplicative `PerceptionLens` plus a new
visibility floor in `kernel::observe`. `domains/astronomy` never learns that
weather exists: it receives an abstract `Visibility` ratio and decides for
itself what drops out, because it alone knows which bodies are bright.
`windows/worldgen` — the composition root — is the only place that turns a
`WeatherState` into a lens and a visibility.

**Tech Stack:** Rust 2024, std + `serde`/`serde_json` only. No new crates.
`cargo nextest` for tests, `cargo test --doc` for doctests.

## Global Constraints

- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain
  depends on `hornvale-kernel` and nothing else. Enforced by
  `cli/tests/architecture.rs`.
- **No new dependencies.** Allowlist enforced by `cli/tests/architecture.rs`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock
  time. Enforced workspace-wide by `clippy.toml` `disallowed-types`.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and
  variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` verdict tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). Regenerate the
  report at the end (Task 8), not per-task.
- **No epoch suffix in this campaign.** Phenomena are a read, not committed
  facts; `SkyReport` has no `Serialize`. No stream label, no consumption order,
  and no save-format field may change. If a task appears to require one, STOP
  and escalate.
- **Byte-identity under clear skies is by construction, and must be tested.**
  A clear sky yields `PerceptionLens::identity()` and `Visibility::CLEAR`
  exactly; `observe` already performs zero arithmetic for an identity lens.
- **Run `cargo fmt` as the final step before every commit.** Skipped fmt is the
  project's most common review finding.
- Gate per task: `cargo nextest run -p <crate>`. The full `make gate` runs once
  at Task 8.

---

### Task 1: The kernel's visibility floor

**Files:**
- Modify: `kernel/src/phenomena.rs` (add `Visibility`, `PerceptionLens::compose`, floor in `observe`)
- Modify: `kernel/src/lib.rs:44` (re-export `Visibility`)
- Test: `kernel/src/phenomena.rs` (inline `#[cfg(test)] mod tests`, existing)

**Interfaces:**
- Consumes: nothing (first task).
- Produces:
  - `pub struct Visibility(f64)` with `Visibility::CLEAR`, `Visibility::new(f64) -> Option<Visibility>`, `pub fn get(&self) -> f64`.
  - `pub fn PerceptionLens::compose(&self, other: &PerceptionLens) -> PerceptionLens` — component-wise product.
  - `observe` drops phenomena whose weighted salience is `< VISIBILITY_FLOOR`.
  - `pub const VISIBILITY_FLOOR: f64 = 0.05;`

- [ ] **Step 1: Write the failing tests**

Add to the existing `mod tests` in `kernel/src/phenomena.rs`:

```rust
    #[test]
    fn identity_lens_still_drops_nothing() {
        // The identity lens performs no arithmetic and the floor must not
        // retroactively cull a faint-but-real phenomenon on the legacy path.
        let faint = Phenomenon {
            kind: "k".to_string(),
            description: "d".to_string(),
            period_days: None,
            salience: 0.01,
            venue: Venue::NightSky,
        };
        let src = FixedSource(vec![faint.clone()]);
        let ctx = ObserverContext::at(EntityId::new(1).unwrap(), WorldTime { day: 0.0 });
        assert_eq!(observe(&[&src], &ctx), vec![faint]);
    }

    #[test]
    fn a_weighted_lens_culls_below_the_floor() {
        let bright = Phenomenon {
            kind: "bright".to_string(),
            description: "d".to_string(),
            period_days: None,
            salience: 1.0,
            venue: Venue::NightSky,
        };
        let faint = Phenomenon {
            kind: "faint".to_string(),
            description: "d".to_string(),
            period_days: None,
            salience: 0.1,
            venue: Venue::NightSky,
        };
        let src = FixedSource(vec![bright, faint]);
        let mut ctx = ObserverContext::at(EntityId::new(1).unwrap(), WorldTime { day: 0.0 });
        ctx.lens = PerceptionLens {
            day_sky: 1.0,
            night_sky: 0.2,
            ambient: 1.0,
        };
        let seen = observe(&[&src], &ctx);
        // bright: 1.0 * 0.2 = 0.20, survives. faint: 0.1 * 0.2 = 0.02, culled.
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, "bright");
        assert_eq!(seen[0].salience, 0.2);
    }

    #[test]
    fn lenses_compose_component_wise() {
        let a = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.4,
            ambient: 2.0,
        };
        let b = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.5,
            ambient: 0.5,
        };
        let c = a.compose(&b);
        assert_eq!(c.day_sky, 0.25);
        assert_eq!(c.night_sky, 0.2);
        assert_eq!(c.ambient, 1.0);
    }

    #[test]
    fn composing_with_identity_is_a_no_op() {
        let a = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.4,
            ambient: 2.0,
        };
        assert_eq!(a.compose(&PerceptionLens::identity()), a);
    }

    #[test]
    fn visibility_rejects_values_outside_the_unit_interval() {
        assert!(Visibility::new(-0.1).is_none());
        assert!(Visibility::new(1.1).is_none());
        assert!(Visibility::new(f64::NAN).is_none());
        assert_eq!(Visibility::new(0.5).map(|v| v.get()), Some(0.5));
        assert_eq!(Visibility::CLEAR.get(), 1.0);
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-kernel phenomena`
Expected: FAIL — `cannot find type Visibility`, `no method named compose`.

- [ ] **Step 3: Implement**

In `kernel/src/phenomena.rs`, add after the `PerceptionLens` impl block:

```rust
/// How much of the sky reaches the observer, in `[0, 1]`: `1.0` is an
/// unobstructed sky, `0.0` a sky completely hidden. Deliberately abstract —
/// a producer decides what it *means* for its own content, and never learns
/// what obstructed the view.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Visibility(f64);

impl Visibility {
    /// A wholly unobstructed sky. The legacy path: every producer must render
    /// exactly its pre-occlusion content at this value.
    pub const CLEAR: Visibility = Visibility(1.0);

    /// A visibility ratio, or `None` if `v` is not a finite value in `[0, 1]`.
    pub fn new(v: f64) -> Option<Visibility> {
        (v.is_finite() && (0.0..=1.0).contains(&v)).then_some(Visibility(v))
    }

    /// The ratio itself.
    /// type-audit: bare-ok(ratio)
    pub fn get(&self) -> f64 {
        self.0
    }
}

/// Weighted salience at or above which a phenomenon still reaches the
/// observer. Below it the phenomenon is culled rather than merely demoted:
/// a star dimmed to a hundredth is not a faint star, it is a star you cannot
/// see. Applied only when the lens is non-identity, so the legacy path is
/// untouched.
/// type-audit: bare-ok(ratio)
pub const VISIBILITY_FLOOR: f64 = 0.05;
```

Add to the `impl PerceptionLens` block:

```rust
    /// This lens seen through `other` — the component-wise product. Occlusion
    /// composes with a species' own perception rather than replacing it.
    pub fn compose(&self, other: &PerceptionLens) -> PerceptionLens {
        PerceptionLens {
            day_sky: self.day_sky * other.day_sky,
            night_sky: self.night_sky * other.night_sky,
            ambient: self.ambient * other.ambient,
        }
    }
```

Replace the weighting block in `observe` (currently `kernel/src/phenomena.rs:135-140`):

```rust
    if !ctx.lens.is_identity() {
        for p in &mut all {
            let w = ctx.lens.weight(p.venue);
            p.salience = ((p.salience * w).clamp(0.0, 1.0) * 100.0).round() / 100.0;
        }
        all.retain(|p| p.salience >= VISIBILITY_FLOOR);
    }
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel phenomena`
Expected: PASS, including the pre-existing lens tests at `:241` and `:262`.

> If either pre-existing test now fails, the floor has changed legacy
> behaviour for an already-lensed observer. That is expected *only* for
> phenomena below 0.05 — confirm the failing value, and if it is a real
> regression, STOP and escalate rather than adjusting the constant to fit.

- [ ] **Step 5: Re-export and commit**

Add `Visibility` and `VISIBILITY_FLOOR` to the `pub use phenomena::{…}` list in `kernel/src/lib.rs:44`.

```bash
cargo fmt
cargo test -p hornvale-kernel
git add kernel/src/phenomena.rs kernel/src/lib.rs
git commit -m "feat(kernel): a visibility floor, and lenses that compose"
```

---

### Task 2: Astronomy renders at a given visibility

**Files:**
- Modify: `domains/astronomy/src/provider.rs:1318-1413` (`sky_at`)
- Modify: `domains/astronomy/src/lib.rs:458` (`ConstantSun::sky_at`)
- Test: `domains/astronomy/src/provider.rs` (inline tests)

**Interfaces:**
- Consumes: `hornvale_kernel::Visibility` (Task 1).
- Produces:
  - `GeneratedSky::sky_at_visibility(&self, time: WorldTime, vis: Visibility) -> SkyReport`
  - `ConstantSun::sky_at_visibility(&self, time: WorldTime, vis: Visibility) -> SkyReport`
  - `sky_at(time)` on both types delegates to `sky_at_visibility(time, Visibility::CLEAR)`.

**Why astronomy owns the thresholds:** only astronomy knows which bodies are
bright. A moon punches through an overcast as a smear of light; a neighbour
star does not. Astronomy never learns *what* obstructs the sky.

- [ ] **Step 1: Write the failing tests**

Add to `mod tests` in `domains/astronomy/src/provider.rs`. Use the existing
test helper that builds a spinning-world `GeneratedSky` (find it in that
module; it is the one the current `sky_at` tests already call) and a night
`WorldTime`.

```rust
    #[test]
    fn a_clear_visibility_is_byte_identical_to_the_legacy_sky() {
        let sky = test_sky();
        let night = night_time(&sky);
        assert_eq!(
            sky.sky_at(night),
            sky.sky_at_visibility(night, Visibility::CLEAR)
        );
    }

    #[test]
    fn a_dimmed_sky_keeps_the_moons_and_loses_the_stars() {
        let sky = test_sky();
        let night = night_time(&sky);
        let d = sky
            .sky_at_visibility(night, Visibility::new(0.4).unwrap())
            .description;
        assert!(d.contains("moon"), "moons punch through: {d}");
        assert!(
            !d.contains("keep their stations"),
            "faint stars must not survive a dimmed sky: {d}"
        );
    }

    #[test]
    fn a_hidden_sky_names_the_dark_and_nothing_else() {
        let sky = test_sky();
        let night = night_time(&sky);
        let d = sky
            .sky_at_visibility(night, Visibility::new(0.0).unwrap())
            .description;
        assert!(!d.contains("moon"), "nothing celestial survives: {d}");
        assert!(!d.contains("keep their stations"), "{d}");
        assert!(d.contains("Night") || d.contains("Twilight"), "{d}");
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy sky_at`
Expected: FAIL — `no method named sky_at_visibility`.

- [ ] **Step 3: Implement**

In `domains/astronomy/src/provider.rs`, add these thresholds above `impl GeneratedSky`:

```rust
/// Visibility at or above which the moons still show a face. Below it they
/// are present but featureless — the deck glows where they stand.
/// type-audit: bare-ok(ratio)
const MOON_PHASE_VISIBILITY: f64 = 0.6;
/// Visibility at or above which a moon is still discernible at all.
/// type-audit: bare-ok(ratio)
const MOON_VISIBILITY: f64 = 0.25;
/// Visibility at or above which the fixed neighbour stars are discernible.
/// They are the faintest things in the sky and go first.
/// type-audit: bare-ok(ratio)
const STAR_VISIBILITY: f64 = 0.75;
```

Rename the existing `pub fn sky_at(&self, time: WorldTime) -> SkyReport` to
`pub fn sky_at_visibility(&self, time: WorldTime, vis: Visibility) -> SkyReport`,
and add the delegating original immediately above it:

```rust
    /// The sky at a moment, rendered under an unobstructed view.
    pub fn sky_at(&self, time: WorldTime) -> SkyReport {
        self.sky_at_visibility(time, Visibility::CLEAR)
    }
```

Inside the renamed method, replace the night branch's `parts` construction
(currently `provider.rs:1375-1395`) with:

```rust
                    let v = vis.get();
                    let mut parts: Vec<String> = if v >= MOON_VISIBILITY {
                        self.system
                            .moons
                            .iter()
                            .enumerate()
                            .map(|(index, moon)| {
                                let size = size_word(moon.angular_diameter_rel);
                                match self.calendar.moon_phase(t, index) {
                                    // Below the phase threshold the moon is a
                                    // presence, not a face.
                                    Some(_) if v < MOON_PHASE_VISIBILITY => capitalize(
                                        &format!("the {size} moon is a smear of light."),
                                    ),
                                    Some(phase) => capitalize(&format!(
                                        "the {} moon shows its {} face.",
                                        size,
                                        phase_word(phase)
                                    )),
                                    // Degenerate: P_sid ≥ Y, unreachable at
                                    // genesis (the Hill cap keeps P_sid ≤
                                    // ~0.15×Y) but handled honestly rather
                                    // than panicking.
                                    None => capitalize(&format!(
                                        "the {size} moon shows no phase — its orbit outpaces the year."
                                    )),
                                }
                            })
                            .collect()
                    } else {
                        Vec::new()
                    };
                    if v >= STAR_VISIBILITY {
                        parts.push(night_star_line(&self.system.neighbors));
                    }
```

Then replace the `SkyReport` construction at the end of that branch, so a
fully-hidden sky does not leave a trailing space:

```rust
                    let description = if parts.is_empty() {
                        dark_words
                    } else {
                        format!("{} {}", dark_words, parts.join(" "))
                    };
                    SkyReport {
                        description,
                        bodies,
                    }
```

In the daylight branch of the same method, leave the prose unchanged: the sun
is the one body that survives any overcast, and its phrasing is the weather
sentence's job (Task 3), not astronomy's.

In `domains/astronomy/src/lib.rs`, give `ConstantSun` the same pair:

```rust
    /// The sky at `_time` — which, at tier 0, never changes, and which no
    /// obstruction dims: the tier-0 sun is a stipulation, not a body.
    pub fn sky_at_visibility(&self, _time: WorldTime, _vis: Visibility) -> SkyReport {
        self.sky_at(_time)
    }
```

Add `Visibility` to the `use hornvale_kernel::{…}` imports in both files.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS, all pre-existing tests included.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/astronomy/
git commit -m "feat(astronomy): render the sky at a given visibility

Astronomy decides what survives a dimmed sky, because it alone knows which
bodies are bright — moons punch through, neighbour stars do not. It never
learns what obstructs the view."
```

---

### Task 3: The composition root turns weather into occlusion

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `occlusion`; wire the three `observe` sites at `:3024`, `:3051`, `:3304`)
- Test: `windows/worldgen/src/lib.rs` (inline tests)

**Interfaces:**
- Consumes: `Visibility`, `PerceptionLens::compose` (Task 1); `sky_at_visibility` (Task 2).
- Produces: `pub fn occlusion(state: WeatherState, cloud: CloudType) -> (PerceptionLens, Visibility)`.

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn a_clear_sky_occludes_nothing_by_construction() {
        use hornvale_climate::{CloudType, WeatherState};
        let (lens, vis) = occlusion(WeatherState::Clear, CloudType::None);
        assert!(lens.is_identity(), "a clear sky must yield the identity lens");
        assert_eq!(vis, Visibility::CLEAR);
    }

    #[test]
    fn thicker_cloud_occludes_monotonically() {
        use hornvale_climate::{CloudType, WeatherState};
        let states = [
            (WeatherState::Clear, CloudType::None),
            (WeatherState::Fair, CloudType::Cumulus),
            (WeatherState::Overcast, CloudType::Stratus),
            (WeatherState::Rain, CloudType::Nimbostratus),
            (WeatherState::Storm, CloudType::Cumulonimbus),
        ];
        let mut last = f64::INFINITY;
        for (s, c) in states {
            let (lens, vis) = occlusion(s, c);
            assert!(vis.get() <= last, "visibility must not rise with cloud: {s:?}");
            last = vis.get();
            assert!(lens.night_sky <= 1.0 && lens.ambient >= 1.0);
        }
    }

    #[test]
    fn a_storm_hides_the_sky_entirely() {
        use hornvale_climate::{CloudType, WeatherState};
        let (_, vis) = occlusion(WeatherState::Storm, CloudType::Cumulonimbus);
        assert_eq!(vis.get(), 0.0);
    }

    #[test]
    fn high_cirrus_over_a_clear_sky_dims_only_slightly() {
        use hornvale_climate::{CloudType, WeatherState};
        let (_, vis) = occlusion(WeatherState::Clear, CloudType::Cirrus);
        assert!(vis.get() > 0.75, "cirrus must not cull the stars");
        assert!(vis.get() < 1.0, "but it is not nothing");
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen occlu`
Expected: FAIL — `cannot find function occlusion`.

- [ ] **Step 3: Implement**

```rust
/// Turn a sky's weather into the occlusion it imposes: a perception lens
/// (how much attention each venue still earns) and a visibility (how much of
/// the sky reaches the eye). The composition root is the ONLY place weather
/// becomes occlusion — no domain learns that clouds exist.
///
/// Identity and `Visibility::CLEAR` under a clear, cloudless sky **by
/// construction**, the same discipline [`perception_lens`] keeps at the
/// goblin baseline: `observe` then performs no arithmetic at all, so an
/// unclouded world is byte-identical to its pre-occlusion self.
///
/// Attenuating the sky raises Ambient above 1.0 deliberately: under a deck
/// you notice the closeness of the air, and the salience sort does the rest.
pub fn occlusion(
    state: hornvale_climate::WeatherState,
    cloud: hornvale_climate::CloudType,
) -> (PerceptionLens, Visibility) {
    use hornvale_climate::{CloudType, WeatherState};
    // The fraction of the sky that still reaches the eye.
    let v = match (state, cloud) {
        (WeatherState::Clear, CloudType::Cirrus) => 0.85,
        (WeatherState::Clear, _) => 1.0,
        (WeatherState::Fair, _) => 0.7,
        (WeatherState::Overcast, _) => 0.3,
        (WeatherState::Rain, _) => 0.1,
        (WeatherState::Storm, _) => 0.0,
    };
    if v == 1.0 {
        return (PerceptionLens::identity(), Visibility::CLEAR);
    }
    let lens = PerceptionLens {
        day_sky: v,
        night_sky: v,
        // The occluder promotes itself: what hides the sky is itself worth
        // noticing. Bounded at 1.5 to match `perception_lens`'s ambient range.
        ambient: 1.0 + (1.0 - v) * 0.5,
    };
    // `v` is a literal in `[0, 1]` on every arm, so `new` cannot fail; the
    // fallback keeps the function total rather than panicking.
    (lens, Visibility::new(v).unwrap_or(Visibility::CLEAR))
}
```

Now wire the three `observe` sites. At `:3024` (`observed_phenomena`) and
`:3051` (`observed_phenomena_from_climate`), the observer is the flagship
place; both must read that place's weather. Replace
`lens: PerceptionLens::identity(),` in each with:

```rust
            lens: occlusion_lens_at(world, climate, position, day),
```

and at `:3304` (the species-lensed path) replace
`lens: perception_lens(perception),` with:

```rust
            lens: occlusion_lens_at(world, climate, position, day)
                .compose(&perception_lens(perception)),
```

Add the shared helper (it is the single place a cell's weather becomes a lens,
so the two call paths cannot drift):

```rust
/// The occlusion lens over a placed observer at `day`, or the identity lens
/// for a position-blind observation (nowhere in particular has no weather).
fn occlusion_lens_at(
    world: &World,
    climate: &GeneratedClimate,
    position: Option<hornvale_kernel::GeoCoord>,
    day: f64,
) -> PerceptionLens {
    let Some(coord) = position else {
        return PerceptionLens::identity();
    };
    let Ok(terrain) = terrain_of(world) else {
        return PerceptionLens::identity();
    };
    let cell = terrain.nearest_cell(coord.latitude, coord.longitude);
    let (lens, _) = occlusion(
        climate.weather_at(cell, day),
        climate.cloud_type_at(cell, day),
    );
    lens
}
```

> `observed_phenomena` at `:3024` builds its own climate today. Give it one
> via the existing `climate_from(world, &terrain)?` call pattern used
> elsewhere in the file, matching `observed_phenomena_from_climate`'s
> signature. Preserve the existing "no place, no phenomena" short-circuit
> exactly — it must still return `Ok(empty)` before building any provider.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS.

- [ ] **Step 5: Measure the floor against reality, and record it**

The `VISIBILITY_FLOOR` from Task 1 was chosen, not derived. Check it against
the real seed-42 distribution:

```bash
cargo run --release -p hornvale -- new --seed 42 --out /tmp/hv-occl.json
cargo run --release -p hornvale -- almanac --world /tmp/hv-occl.json | grep -E '^\- \[' | head -30
```

Read off the salience column. Confirm that under the day-0 overcast
(`v = 0.3`) the moons (pre-occlusion 0.64 and 0.47 → 0.19 and 0.14) survive
the 0.05 floor and the neighbour stars (0.10–0.11 → 0.03) do not. Record the
measured values in a comment beside `VISIBILITY_FLOOR` in
`kernel/src/phenomena.rs` so a later reader can re-derive the choice.

> If the measurement contradicts the constant, change the constant to fit the
> measurement and say so in the commit message — do not reword the comment to
> fit the constant.

- [ ] **Step 6: Test the byte-identity claim rather than asserting it**

"Identity by construction" is exactly the kind of claim that reads as
self-evidently true and has been wrong before. Two tests, both in
`windows/worldgen/src/lib.rs`:

```rust
    #[test]
    fn world_bytes_do_not_move_under_occlusion() {
        // Phenomena are a READ, not committed facts: whatever the sky is
        // doing, the serialized world is unaffected. This is the campaign's
        // core determinism claim and the reason it owes no epoch.
        let world = seed_42_world();
        let json = serde_json::to_string(&world).unwrap();
        assert_eq!(
            json,
            include_str!("../../../tests/fixtures/seed-42-world.json").trim_end(),
            "occlusion changed the serialized world — it must not"
        );
    }

    #[test]
    fn a_clear_cell_observes_exactly_what_it_did_before() {
        // Find a genuinely clear cell/day and assert the identity path: the
        // lens is identity, so `observe` does no arithmetic and no culling.
        let world = seed_42_world();
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_from(&world, &terrain).unwrap();
        let clear = terrain
            .geosphere()
            .cells()
            .find(|c| climate.weather_at(*c, 0.0) == hornvale_climate::WeatherState::Clear)
            .expect("some cell somewhere has a clear sky on day 0");
        let (lens, vis) = occlusion(
            climate.weather_at(clear, 0.0),
            climate.cloud_type_at(clear, 0.0),
        );
        // Cirrus over a clear sky is the one non-identity clear case.
        if climate.cloud_type_at(clear, 0.0) != hornvale_climate::CloudType::Cirrus {
            assert!(lens.is_identity());
            assert_eq!(vis, Visibility::CLEAR);
        }
    }
```

Create the fixture from the CURRENT tree, before Task 1's changes are in
effect — if you have already committed Tasks 1–3, generate it from
`git stash`/`origin/main` instead, or the test proves nothing:

```bash
git stash && cargo run --release -p hornvale -- new --seed 42 \
  --out windows/worldgen/tests/fixtures/seed-42-world.json && git stash pop
```

> If the fixture path above does not match the crate's existing test-fixture
> convention, follow the crate's convention and fix the `include_str!` path
> to match.

- [ ] **Step 7: Test that occlusion composes with a species lens**

```rust
    #[test]
    fn occlusion_composes_with_species_perception() {
        // perception_lens is already non-identity for non-goblin species, so
        // occlusion must multiply with it, not replace it.
        let nocturnal = hornvale_species::PerceptionVector {
            activity: hornvale_species::ActivityCycle::Nocturnal,
            sky_attention: 0.5,
            night_vision: 0.5,
        };
        let species = perception_lens(&nocturnal);
        assert!(!species.is_identity(), "fixture must be non-identity");
        let (occ, _) = occlusion(
            hornvale_climate::WeatherState::Overcast,
            hornvale_climate::CloudType::Stratus,
        );
        let composed = occ.compose(&species);
        assert_eq!(composed.night_sky, occ.night_sky * species.night_sky);
        assert_eq!(composed.ambient, occ.ambient * species.ambient);
    }
```

> Construct `PerceptionVector` using whatever constructor
> `domains/species` actually exposes — the literal above assumes public
> fields. If it has a validating constructor, use that.

- [ ] **Step 8: Commit**

```bash
cargo fmt
cargo test -p hornvale-worldgen -p hornvale-kernel
git add windows/worldgen/ kernel/src/phenomena.rs
git commit -m "feat(worldgen): weather becomes occlusion at the composition root

Identity under a clear sky by construction, so an unclouded world stays
byte-identical — tested against a pre-campaign fixture, not asserted. The
floor constant now carries its seed-42 measurement."
```

---

### Task 4: The sky belongs to the observer, not the capital

**Files:**
- Modify: `windows/worldgen/src/lib.rs:5672-5698` (`sky_report_from`)
- Test: `windows/worldgen/src/lib.rs` (inline tests)

**Interfaces:**
- Consumes: `occlusion` (Task 3), `sky_at_visibility` (Task 2).
- Produces: `sky_report_from(world, time, terrain, climate, at: Option<CellId>)` — a new final parameter. `None` means "no particular place".

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn two_places_can_have_different_skies_on_the_same_day() {
        // The assertion that cannot hold while weather is pinned to the
        // flagship settlement.
        let world = seed_42_world();
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_from(&world, &terrain).unwrap();
        let day = WorldTime { day: 0.0 };
        let mut seen = std::collections::BTreeSet::new();
        for cell in terrain.geosphere().cells().take(400) {
            let r = sky_report_from(&world, day, &terrain, &climate, Some(cell)).unwrap();
            seen.insert(r.description);
        }
        assert!(
            seen.len() > 1,
            "every cell reported an identical sky — weather is still pinned"
        );
    }

    #[test]
    fn a_placeless_world_reports_a_placeless_sky() {
        let world = seed_42_world();
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_from(&world, &terrain).unwrap();
        let r =
            sky_report_from(&world, WorldTime { day: 0.0 }, &terrain, &climate, None).unwrap();
        assert!(
            !r.description.contains("The sky is"),
            "nowhere in particular has no weather: {}",
            r.description
        );
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen sky`
Expected: FAIL — arity mismatch on `sky_report_from`.

- [ ] **Step 3: Implement**

Replace the body of `sky_report_from` with:

```rust
pub fn sky_report_from(
    world: &World,
    time: WorldTime,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    at: Option<hornvale_kernel::CellId>,
) -> Result<SkyReport, BuildError> {
    // No place, no weather: a placeless observation reports the unobstructed
    // sky rather than silently borrowing cell 0's, which is what the flagship
    // fallback used to do.
    let Some(cell) = at else {
        return Ok(sky_of(world)?.sky_at_visibility(time, Visibility::CLEAR));
    };
    let state = climate.weather_at(cell, time.day);
    let cloud = climate.cloud_type_at(cell, time.day);
    let (_, vis) = occlusion(state, cloud);
    let mut report = sky_of(world)?.sky_at_visibility(time, vis);
    report.description = format!("{} The sky is {}.", report.description, sky_phrase(state, cloud));
    Ok(report)
}
```

Delete the flagship-settlement lookup and its `unwrap_or(CellId(0))` entirely —
that block is what this task removes.

Update `sky_report` (the `:5664` wrapper) to resolve the flagship cell itself
and pass it through, preserving today's behaviour for its callers:

```rust
pub fn sky_report(world: &World, time: WorldTime) -> Result<SkyReport, BuildError> {
    let terrain = terrain_of(world)?;
    let climate = climate_from(world, &terrain)?;
    let at = flagship_cell(world, &terrain);
    sky_report_from(world, time, &terrain, &climate, at)
}

/// The canonical-grid cell of the world's flagship settlement, if it has one.
/// `None` for a settlement-less world — seed 123 generates one, and its sky is
/// placeless rather than cell 0's by accident.
fn flagship_cell(
    world: &World,
    terrain: &GeneratedTerrain,
) -> Option<hornvale_kernel::CellId> {
    hornvale_terrain::places(world)
        .into_iter()
        .find(|p| {
            world
                .ledger
                .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
                .is_some()
        })
        .and_then(|p| place_coord(world, p.id))
        .map(|c| terrain.nearest_cell(c.latitude, c.longitude))
}
```

Fix the remaining call site at `windows/vessel/src/vantage.rs:34` in Task 6.
For now, keep the crate compiling by passing `None` there.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo test -p hornvale-worldgen
git add windows/worldgen/src/lib.rs windows/vessel/src/vantage.rs
git commit -m "fix(worldgen): the sky report describes the observer's weather

It read the flagship settlement's cell and fell back to CellId(0) for a
settlement-less world, so walking never changed your weather."
```

---

### Task 5: Bare compass directions

**Files:**
- Modify: `windows/vessel/src/session.rs:563-590` (verb dispatch)
- Test: `windows/vessel/tests/session.rs`

**Interfaces:**
- Consumes: nothing from earlier tasks. Independent — may be done first.
- Produces: no new API.

- [ ] **Step 1: Write the failing test**

Add to `windows/vessel/tests/session.rs`:

```rust
#[test]
fn every_printed_way_out_is_a_command_you_can_type() {
    // The room prints "Ways on: SE, N, SW." — each of those tokens must be
    // accepted verbatim, which is the whole bug.
    let mut s = test_session();
    let here = s.feed("look");
    let ways = here
        .lines()
        .find(|l| l.starts_with("Ways on:"))
        .expect("a room lists its ways out");
    let tokens: Vec<String> = ways
        .trim_start_matches("Ways on:")
        .trim_end_matches('.')
        .split(',')
        .map(|t| t.trim().to_lowercase())
        .collect();
    assert!(!tokens.is_empty(), "no exits to test");
    for t in tokens {
        let out = s.feed(&t);
        assert!(
            !out.contains("No verb"),
            "the room printed '{t}' as a way out but the parser rejects it: {out}"
        );
        s.feed("back");
    }
}

#[test]
fn long_direction_names_work_too() {
    let mut s = test_session();
    assert!(!s.feed("northeast").contains("No verb"));
}

#[test]
fn a_genuine_non_verb_still_reports_itself_honestly() {
    let mut s = test_session();
    let out = s.feed("xyzzy");
    assert!(out.contains("No verb 'xyzzy'"), "{out}");
}
```

> Use whatever session-construction and feed helpers
> `windows/vessel/tests/session.rs` already defines — that file has an
> existing exit-token test at `:49` to model these on. Do not invent new
> helpers.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test session`
Expected: FAIL — "the room printed 'se' as a way out but the parser rejects it".

- [ ] **Step 3: Implement**

In the `match verb` block in `windows/vessel/src/session.rs:563`, change the
fallback arm. The dispatch currently ends with a catch-all that produces
`No verb '{verb}'`; replace that arm with:

```rust
            // A bare compass token is a movement command: the room prints
            // "Ways on: SE, N, SW." and those tokens must be typeable. The
            // parser already accepts them; only the dispatch was missing.
            other if parse_compass(other).is_some() => self.go(other),
            other => Turn::Out(format!("No verb '{other}' ('help' lists them).")),
```

Update `HELP` so the `go` line advertises it:

```rust
  go <dir>         walk a compass exit (n ne e se s sw w nw); the bare
                   direction works too
```

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-vessel --test session`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/
git commit -m "fix(vessel): accept the directions the room prints

'Ways on: SE, N, SW.' then 'No verb 'n''. parse_compass already accepted
every one of those tokens; the dispatch simply never reached it."
```

---

### Task 6: The walker's own sky, and knowledge that survives cloud

**Files:**
- Modify: `windows/vessel/src/vantage.rs:26-42` (`observable`)
- Test: `windows/vessel/tests/session.rs`

**Interfaces:**
- Consumes: `sky_report_from(..., at)` (Task 4).
- Produces: no new API — `Vantage` is unchanged.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_sky_follows_the_walker() {
    // Walk far enough to leave the capital's weather cell and assert the sky
    // is not simply the capital's, forever.
    let mut s = test_session();
    let mut skies = std::collections::BTreeSet::new();
    for _ in 0..60 {
        let out = s.feed("look");
        if let Some(l) = out.lines().find(|l| l.contains("The sky is")) {
            skies.insert(l.to_string());
        }
        s.feed("wait 7");
        // Follow whatever exit this room offers rather than assuming one.
        let ways = s.feed("look");
        if let Some(w) = ways
            .lines()
            .find(|l| l.starts_with("Ways on:"))
            .and_then(|l| l.trim_start_matches("Ways on:").split(',').next())
        {
            s.feed(w.trim().trim_end_matches('.'));
        }
    }
    assert!(skies.len() > 1, "the sky never changed across a long walk");
}

#[test]
fn clouding_over_does_not_unlearn_what_was_seen() {
    // Occlusion hides a percept; it must not erase knowledge already held.
    let mut s = test_session();
    s.feed("look");
    let before = s.feed("knows");
    let before_n: usize = before
        .split_whitespace()
        .next()
        .and_then(|n| n.parse().ok())
        .expect("knows reports a count");
    for _ in 0..40 {
        s.feed("wait 9");
        s.feed("look");
    }
    let after_n: usize = s
        .feed("knows")
        .split_whitespace()
        .next()
        .and_then(|n| n.parse().ok())
        .expect("knows reports a count");
    assert!(
        after_n >= before_n,
        "knowledge shrank from {before_n} to {after_n} as the sky changed"
    );
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-vessel --test session`
Expected: FAIL on `the_sky_follows_the_walker` — one sky for the whole walk.

- [ ] **Step 3: Implement**

In `windows/vessel/src/vantage.rs`, replace the `sky` binding in `observable`:

```rust
    // The walker's own cell, not the capital's: the sky over *here*.
    // NB `at` is already this function's WorldTime parameter — the cell gets
    // its own name rather than shadowing it.
    let cell = ctx
        .terrain()
        .nearest_cell(locale.latitude, locale.longitude);
    let sky =
        hornvale_worldgen::sky_report_from(world, at, ctx.terrain(), ctx.climate(), Some(cell))
            .map_err(|e| VesselError::Build(e.to_string()))?
            .description;
```

> `locale` is bound immediately above this in `observable`, so its centroid is
> already available — no second `describe` call.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-vessel`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/vessel/
git commit -m "feat(vessel): the walker's sky is the sky over the walker

Plus the knows-preservation guard: occlusion hides a percept, it must never
erase knowledge already held."
```

---

### Task 7: Strange sites become reachable

**Files:**
- Modify: `windows/locale/src/lib.rs` (a rendering helper beside `strange_sites`)
- Modify: `cli/src/main.rs:1291-1360` (`cmd_locale`) and the usage block at `:70`
- Create: `book/src/gallery/strange-sites-seed-42.md` (generated artifact)
- Modify: `.github/workflows/ci.yml` ("Artifacts are current" step)
- Test: `cli/src/main.rs` (inline tests), `windows/locale/src/lib.rs`

**Interfaces:**
- Consumes: `LocaleContext::strange_sites() -> Vec<StrangeSite>` (existing, `windows/locale/src/lib.rs:206`).
- Produces: `pub fn strange_site_rows(&self) -> Vec<StrangeSiteRow>` with public fields `cell: u32`, `latitude: f64`, `longitude: f64`, `biome: String`, `descriptor: String`.

**Why the descriptor is required, not cosmetic:** the sites are differentiated
by negation vector (energy × kingdom × endemic). A bare coordinate list would
render 101 wonders as 101 identical rows.

- [ ] **Step 1: Write the failing tests**

In `windows/locale/src/lib.rs` tests:

```rust
    #[test]
    fn strange_site_rows_carry_distinct_descriptors() {
        let ctx = seed_42_context();
        let rows = ctx.strange_site_rows();
        assert!(!rows.is_empty(), "seed 42 places exotic sites");
        let distinct: std::collections::BTreeSet<&str> =
            rows.iter().map(|r| r.descriptor.as_str()).collect();
        assert!(
            distinct.len() > 1,
            "101 wonders must not render as one repeated row"
        );
        for r in &rows {
            assert!(!r.descriptor.is_empty(), "cell {} has no descriptor", r.cell);
            assert!((-90.0..=90.0).contains(&r.latitude));
        }
    }
```

In `cli/src/main.rs` tests:

```rust
    #[test]
    fn locale_strange_lists_the_placed_sites() {
        let out = render_strange_sites(&seed_42_world(), None).unwrap();
        assert!(out.contains("| cell |"), "a markdown table: {out}");
        assert!(out.lines().count() > 10);
    }

    #[test]
    fn locale_strange_respects_a_limit() {
        let out = render_strange_sites(&seed_42_world(), Some(3)).unwrap();
        // header + separator + 3 rows, plus the count line
        assert_eq!(out.lines().filter(|l| l.starts_with("| ")).count(), 4);
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-locale strange && cargo test -p hornvale locale_strange`
Expected: FAIL — `no method named strange_site_rows`.

- [ ] **Step 3: Implement**

In `windows/locale/src/lib.rs`, beside `strange_sites`:

```rust
/// One placed exotic site, rendered for a reader: where it is and what makes
/// it strange.
/// type-audit: bare-ok(index: cell), bare-ok(degrees: latitude),
/// bare-ok(degrees: longitude), bare-ok(prose: biome), bare-ok(prose: descriptor)
#[derive(Debug, Clone, PartialEq)]
pub struct StrangeSiteRow {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Site latitude, degrees (quantized).
    pub latitude: f64,
    /// Site longitude, degrees (quantized).
    pub longitude: f64,
    /// The site's base biome.
    pub biome: String,
    /// What makes it strange — the exotic clause for its negation vector.
    pub descriptor: String,
}

impl LocaleContext {
    /// Every placed exotic site, rendered. Sites are differentiated by
    /// negation vector, so each row carries its own descriptor.
    pub fn strange_site_rows(&self) -> Vec<StrangeSiteRow> {
        self.strange_sites()
            .into_iter()
            .map(|s| {
                let cell = hornvale_kernel::CellId(s.cell);
                let coord = self.climate.geosphere().coord(cell);
                StrangeSiteRow {
                    cell: s.cell,
                    latitude: hornvale_kernel::quantize(coord.latitude),
                    longitude: hornvale_kernel::quantize(coord.longitude),
                    biome: biome_prose_name(self.climate.biome_at(cell)).to_string(),
                    descriptor: crate::grammar::exotic_clause(Negations {
                        energy: s.energy,
                        kingdom: s.kingdom,
                        endemic: s.endemic,
                    }),
                }
            })
            .collect()
    }
}
```

> `exotic_clause` is currently private to `grammar.rs`; make it `pub(crate)`.
> The three accessors above are the real ones already used in this file and
> its dependencies: `Geosphere::coord` (`kernel/src/geosphere.rs:256`),
> `GeneratedClimate::biome_at` (`domains/climate/src/provider.rs:413`), and
> `biome_prose_name` (`windows/locale/src/lib.rs:449`). `self.climate
> .geosphere()` is the same call already made at `windows/locale/src/lib.rs:173`.
> Do not add new terrain plumbing.

In `cli/src/main.rs`, add the renderer and dispatch:

```rust
/// Render the world's placed exotic sites as a markdown table.
fn render_strange_sites(world: &World, limit: Option<usize>) -> Result<String, String> {
    let ctx = locale_context(world)?;
    let rows = ctx.strange_site_rows();
    let total = rows.len();
    let shown = limit.unwrap_or(total).min(total);
    let mut out = format!("{total} placed exotic sites.\n\n");
    out.push_str("| cell | lat | lon | biome | what makes it strange |\n");
    out.push_str("|---|---|---|---|---|\n");
    for r in rows.iter().take(shown) {
        out.push_str(&format!(
            "| {} | {:.2} | {:.2} | {} | {} |\n",
            r.cell, r.latitude, r.longitude, r.biome, r.descriptor
        ));
    }
    if shown < total {
        out.push_str(&format!("\n…and {} more.\n", total - shown));
    }
    Ok(out)
}
```

In `cmd_locale` (`cli/src/main.rs:1305`), add before the `--sample` branch:

```rust
    if args.iter().any(|a| a == "--strange") {
        let limit = flag_value(args, "--limit")
            .map(|n| n.parse::<usize>().map_err(|_| format!("bad --limit: {n}")))
            .transpose()?;
        print!("{}", render_strange_sites(&ctx_world, limit)?);
        return Ok(());
    }
```

Add to the usage block at `cli/src/main.rs:70`:

```
  hornvale locale --world W --strange [--limit N]
                          list the placed exotic sites: where, and what makes them strange
```

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-locale -p hornvale`
Expected: PASS.

- [ ] **Step 5: Publish the artifact and drift-check it**

```bash
cargo run --release -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run --release -p hornvale -- locale --world /tmp/hv.json --strange \
  > book/src/gallery/strange-sites-seed-42.md
```

Add the page to `book/src/SUMMARY.md` beside the other gallery entries, and add
the two commands above to the "Artifacts are current" step in
`.github/workflows/ci.yml`, matching the shape of the existing almanac entries.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add windows/locale/ cli/src/main.rs book/ .github/workflows/ci.yml
git commit -m "feat(locale): the 101 strange sites become reachable

Each row carries its own descriptor: the sites differ by negation vector, and
a bare coordinate list would render 101 wonders as 101 identical rows."
```

---

### Task 8: Regenerate, sweep, close

**Files:**
- Modify: `book/src/gallery/almanac-seed-42*.md`, `book/src/gallery/possession-*.md` (regenerated)
- Modify: `docs/audits/type-audit-report.md` (regenerated)
- Create: `book/src/chronicle/the-occlusion.md`
- Create: `docs/retrospectives/the-occlusion.md`
- Modify: `book/src/frontier/idea-registry.md`, `book/src/open-questions.md`

- [ ] **Step 1: Absorb main before regenerating**

```bash
make preflight
```

On an ancestry NO-GO, merge `origin/main` INTO this branch and re-run. Absorb
BEFORE any regeneration — a pre-absorb regen reverts main's.

- [ ] **Step 2: Regenerate every committed artifact**

Run the full command list from the "Artifacts are current" step in
`.github/workflows/ci.yml` (that file is authoritative, not this plan), then:

```bash
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected: the five sky-bearing gallery files move
(`almanac-seed-42-sky.md`, `almanac-seed-42.md`, `almanac-seed-42-locked.md`,
`possession-seed-42.md`, `possession-over-time-seed-42.md`), plus the new
strange-sites page. **Nothing under `book/src/laboratory/` may move** — the
census is untouched by this campaign; if a census golden moves, STOP and
escalate.

- [ ] **Step 3: Read the regenerated prose**

Open `book/src/gallery/almanac-seed-42-sky.md` and confirm the contradiction is
gone: under "a flat overcast", the five neighbour stars must no longer be
enumerated and the moons should read as a smear. This is the campaign's whole
point — read it, do not just diff it.

- [ ] **Step 4: Regenerate the type-audit report**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report \
  > docs/audits/type-audit-report.md
```

- [ ] **Step 5: The full gate**

```bash
make gate
```

Expected: green. Boundary types changed in Tasks 1, 2, 4, and 7, so the scoped
gate is not sufficient here.

- [ ] **Step 6: Book, chronicle, retrospective, registry**

- Chronicle entry `book/src/chronicle/the-occlusion.md`, at the book's
  established altitude — technical and mathematical, comprehensible without
  the code. The through-line: the codebase already had one occluder and never
  named it.
- Retrospective `docs/retrospectives/the-occlusion.md` — process lessons only.
  Include: the ideonomy overturn (binary gating → graded attenuation, via the
  radio-astronomy re-instantiation) and the fact that four of these findings
  were only visible by *running* the system, not reading it.
- Idea registry rows in `book/src/frontier/idea-registry.md`: cloud-base
  altitude occlusion; moonlight as an occluder; NPC rumor and Book/`consult`
  as strange-site discovery paths; marine biome prose; pronounceable
  settlement names.
- Freshness sweep of any chapter describing the sky, possession, or locale
  prose. Re-score `book/src/open-questions.md` if this campaign moved a bet.
- Promote `.superpowers/sdd/followups.md` into the retrospective's follow-up
  section.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(the-occlusion): close — chronicle, retro, registry, artifacts"
```

---

## Notes for the executor

- **Task 5 is independent** of everything else and is the smallest real win in
  the campaign. If you want a green commit early, do it first.
- **Tasks 1 → 2 → 3 → 4 are a chain**; 6 depends on 4; 7 is independent of all
  of them.
- **The `map`-verb strangeness gradient is deliberately NOT in this plan.** The
  spec allowed dropping it if it needed real design rather than wiring; the
  `map` verb turned out to be a substantial chart renderer, so it went to the
  followup register instead of becoming a vague task.
- **If any task seems to require an epoch suffix or a stream-label change,
  STOP.** That contradicts the spec's determinism analysis and needs Nathan.
