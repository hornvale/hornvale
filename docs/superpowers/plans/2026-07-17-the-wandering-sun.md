# The Wandering Sun Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Phase the climate's seasonal temperature on the true orbital
`year_phase` so ice/hot-spot/terminator move as one (a librating substellar
on locked worlds, an offset-corrected sinusoid on spinning ones), and let the
orrery globe run a year forward with the planet held still.

**Architecture:** Producer-first (hornvale climate → scene → golden), then the
orrery consumer (temperature reconstruction + the globe seasonal-freeze). The
locked-world temperature stays a client reconstruction pinned by a
producer-sourced golden (the Isotherm pattern the existing `temperatureAt`
already is). Spec: `docs/superpowers/specs/2026-07-17-the-wandering-sun-design.md`.

**Tech Stack:** Rust (`hornvale-climate`, `hornvale-scene`, `hornvale-worldgen`,
`clients/world-wasm`), TypeScript + three.js + vitest (orrery).

## Global Constraints

- Hornvale worktree: `~/.config/superpowers/worktrees/hornvale/the-wandering-sun`, branch `the-wandering-sun`. Orrery worktree: `~/.config/superpowers/worktrees/orrery/the-wandering-sun`, branch `the-wandering-sun`. Never commit elsewhere.
- Dependencies: `serde`/`serde_json` only (Rust); no new npm packages.
- No `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only); no wall-clock time. Route f64 transcendentals through `hornvale_kernel::math` (never `f64::sin` directly) — determinism/cross-platform (decision 0041).
- Every serialized f64 quantizes at emit: `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]` (decision 0033), emit boundary only.
- New JSON fields are APPENDED after every existing field; field order is contract. Every new `pub` item gets a doc comment + a `type-audit:` tag on the struct line.
- **Determinism:** no ledger epoch, no new stream draws, no seed-derivation label changes. The only intended byte changes: locked-world `scene/tiles/v1` (a new `locked` field + its now-meaningful seasonal reconstruction inputs) and `temperature_at(day)` sampled values on obliquity≠0 worlds. Spinning-world `scene/tiles/v1` MUST stay byte-identical (verify).
- **Census regen is Nathan's carve-out.** Task 5 MEASURES census drift and reports; it does NOT run a regen. No AWS spend without Nathan's explicit OK.
- `cargo fmt` before every Rust commit; commit gate `make gate` (Bash `timeout: 3600000`). Orrery: `npm test` + `npx tsc --noEmit`.
- Commit trailer: `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: Retain `year_phase_offset` and `insolation` on the climate

**Files:**
- Modify: `domains/climate/src/provider.rs` (`ClimateInputs` ~line 17; `GeneratedClimate` struct ~line 40; its constructor ~line 130; the stored fields)
- Modify: `windows/worldgen/src/lib.rs` (the two `ClimateInputs { … }` construction sites, ~line 700 and ~line 966)

**Why both:** the spinning fix (Task 2) needs `year_phase_offset` at
evaluation time and the locked libration (Task 3) needs `insolation` (to
recompute the `S^{1/4}` scale as the substellar moves). `insolation` is
already a `ClimateInputs` field but is consumed only during generation and
**not retained** on `GeneratedClimate` — this task retains it. Settling
both here keeps `temperature_at`'s signature stable from Task 2 on.

**Interfaces:**
- Consumes: `system.forcing.year_phase_offset` (f64, turns) — already on the astronomy forcing (`domains/astronomy` `Calendar::year_phase` reads `self.forcing.year_phase_offset`); the worldgen ctx already carries forcing/obliquity nearby (see the existing `obliquity_deg:` field it passes). `ClimateInputs.insolation` already exists.
- Produces: `GeneratedClimate` stores `year_phase_offset: f64` and `insolation: f64`, with accessors `year_phase_offset()` and `insolation()`; `ClimateInputs.year_phase_offset: f64` (new, appended).

- [ ] **Step 1: Failing test** — in `provider.rs` tests, assert the stored offset round-trips. Add near the existing provider tests:

```rust
#[test]
fn climate_carries_the_year_phase_offset() {
    // Build a climate with a known offset and confirm it is retained for
    // the time-varying temperature phase (regression guard for the plumbing).
    let geo = Geosphere::new(2);
    let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0));
    let seafloor = CellMap::from_fn(&geo, |_| SeafloorFeature::None);
    let climate = GeneratedClimate::generate(&ClimateInputs {
        geosphere: &geo,
        elevation: &elevation,
        sea_level: ReferenceElevation::new(0.0),
        seafloor: &seafloor,
        insolation: 1.0,
        obliquity_deg: 23.5,
        regime: RotationRegime::Spinning { day_std: 1.0 },
        year_length_std: 360.0,
        year_phase_offset: 0.2,
    });
    assert_eq!(climate.year_phase_offset(), 0.2);
}
```

- [ ] **Step 2: Verify it fails** — `cargo test -p hornvale-climate climate_carries_the_year_phase_offset` (compile fail: no field/accessor).
- [ ] **Step 3: Implement** — add `pub year_phase_offset: f64` to `ClimateInputs` (after `year_length_std`, appended); add `year_phase_offset: f64` AND `insolation: f64` to `GeneratedClimate`; set both in the constructor (`year_phase_offset: inputs.year_phase_offset`, `insolation: inputs.insolation`); add `pub fn year_phase_offset(&self) -> f64` and `pub fn insolation(&self) -> f64` accessors with doc comments. In BOTH `windows/worldgen/src/lib.rs` construction sites add `year_phase_offset: <the forcing's offset>,` (find the forcing/system value in scope — grep `year_phase_offset` in worldgen; the composition root builds the sky/forcing before climate). If the value isn't in scope at a site, thread it from the same place `obliquity_deg` comes from.
- [ ] **Step 4: Green** — `cargo test -p hornvale-climate` all pass.
- [ ] **Step 5: fmt + clippy** — `cargo fmt && cargo clippy -p hornvale-climate -p hornvale-worldgen --all-targets -- -D warnings`.
- [ ] **Step 6: Commit** — `feat(climate): plumb year_phase_offset into ClimateInputs`.

---

### Task 2: Spinning worlds — phase the seasonal term on `year_phase`

**Files:**
- Modify: `domains/climate/src/temperature.rs` (`temperature_at` ~line 98–120; add the `year_phase_offset` param)
- Modify: `domains/climate/src/provider.rs` (the `temperature_at` wrapper ~line 157–170 passes the stored offset)

**Interfaces:**
- Consumes: `GeneratedClimate.year_phase_offset()` and `.insolation()` (Task 1).
- Produces: the FINAL `temperature_at` signature, stable from here on:
  `temperature_at(mean, geo, elevation, sea_level, obliquity_deg, insolation, year_length_std, year_phase_offset, regime, cell, day)`.
  This task adds BOTH `insolation: f64` (after `obliquity_deg`) and
  `year_phase_offset: f64` (after `year_length_std`). Spinning uses
  `year_phase_offset`; `insolation` is unused until Task 3's locked branch
  (an unused function parameter — no Rust warning). Settling both now means
  Task 3 changes no signature.

- [ ] **Step 1: Failing test** — in `temperature.rs` tests, assert the seasonal peak aligns with the orbital phase offset. A spinning world with a nonzero offset should reach its northern-summer extreme a quarter-year *before* the offset, i.e. at `day` where `frac(day/year + offset) = 0.25`:

```rust
#[test]
fn spinning_seasonal_peak_tracks_the_year_phase_offset() {
    let geo = Geosphere::new(3);
    let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0));
    let sea = ReferenceElevation::new(0.0);
    let regime = RotationRegime::Spinning { day_std: 1.0 };
    let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime);
    // A clearly-northern cell.
    let north = (0..geo.len()).map(CellId).max_by(|a, b| {
        geo.coord(*a).latitude.total_cmp(&geo.coord(*b).latitude)
    }).unwrap();
    let year = 360.0;
    let offset = 0.2;
    // Final signature: (…, obliquity, insolation, year_length, year_phase_offset, …)
    let t = |day: f64| temperature_at(&mean, &geo, &elevation, sea, 23.5, 1.0, year, offset, &regime, north, day).get();
    // Northern summer (max) is at frac(day/year + offset) = 0.25 -> day = (0.25 - offset)*year (mod year).
    let summer_day = ((0.25 - offset).rem_euclid(1.0)) * year;
    let winter_day = ((0.75 - offset).rem_euclid(1.0)) * year;
    assert!(t(summer_day) > t(winter_day) + 1.0, "north is warmest near its offset-shifted summer");
    // And at the offset-shifted equinox the anomaly is ~0 (mean).
    let equinox_day = ((0.0 - offset).rem_euclid(1.0)) * year;
    assert!((t(equinox_day) - mean.get(north).get()).abs() < 0.2);
}
```

- [ ] **Step 2: Verify it fails** — `cargo test -p hornvale-climate spinning_seasonal_peak_tracks` — fails: signature mismatch (no offset param) / wrong peak day.
- [ ] **Step 3: Implement** — add `insolation: f64` (after `obliquity_deg`) and `year_phase_offset: f64` (after `year_length_std`) to `temperature_at`'s signature. Change the spinning phase line:

```rust
let phase = (day / year_length_std + year_phase_offset).rem_euclid(1.0);
```

Update the doc comment (the seasonal sinusoid is "on the orbital year phase, `frac(day/year + year_phase_offset)`"). In `provider.rs`, the `temperature_at` wrapper passes `self.insolation` and `self.year_phase_offset`. Update every OTHER caller of `temperature.rs::temperature_at` (grep across the crate; the provider is the main one) to pass both. `insolation` is unused in this task's body (locked still returns `base`) — fine.
- [ ] **Step 4: Green** — `cargo test -p hornvale-climate`.
- [ ] **Step 5: Confirm spinning scene byte-identity** — `t_mean_c`/`t_swing_c` are phase-free (mean + `seasonal_swing_at` amplitude), so `scene/tiles/v1` is unchanged for spinning worlds. Verify: `cargo test -p hornvale-scene` (the existing tiles golden pins seed 1, a spinning/constant world — it MUST still pass unchanged). If it drifts, STOP — the phase leaked into a scene layer, which it must not.
- [ ] **Step 6: fmt + clippy + commit** — `feat(climate): phase spinning seasonal temperature on the true year phase`.

---

### Task 3: Locked worlds — a librating substellar temperature

**Files:**
- Modify: `domains/climate/src/substellar.rs` (add a `substellar_at(latitude_deg)` helper + `substellar_cosine_dir` + a shared `locked_cell_temperature` mapping)
- Modify: `domains/climate/src/temperature.rs` (`mean_temperature` locked branch to call the shared mapping; `temperature_at` locked branch to compute the moving-substellar temperature at `day`)

**Interfaces:**
- Consumes: the `insolation` and `year_phase_offset` params already on `temperature_at` (added in Task 2); `hornvale_kernel::math`. No signature change and no `provider.rs` change in this task — Task 1 retained `insolation`, Task 2 wired it through.
- Produces: `substellar_at(lat_deg: f64) -> [f64; 3]` (unit substellar direction at latitude `lat_deg`, longitude 0); `substellar_cosine_dir(p, dir) -> f64` (dot with an arbitrary direction); `locked_cell_temperature(cos_theta: f64, scale: f64, lapse: f64) -> f64` (the `-18 + 78·cos^0.3·scale` day / `-60` night mapping, minus lapse).

- [ ] **Step 1: Failing tests** — in `temperature.rs` tests:

```rust
#[test]
fn locked_substellar_hot_spot_librates_with_obliquity() {
    let geo = Geosphere::new(4);
    let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0));
    let sea = ReferenceElevation::new(0.0);
    let regime = RotationRegime::Locked;
    let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime);
    let year = 240.0;
    let obliq = 22.0;
    // At the northern solstice (frac(day/year + 0) = 0.25) the substellar
    // latitude is +obliquity, so the warmest cell sits near +22 deg lat,
    // not the equator.
    let solstice = 0.25 * year;
    let warmest = (0..geo.len()).map(CellId).max_by(|a, b| {
        let ta = temperature_at(&mean, &geo, &elevation, sea, obliq, 1.0, year, 0.0, &regime, *a, solstice).get();
        let tb = temperature_at(&mean, &geo, &elevation, sea, obliq, 1.0, year, 0.0, &regime, *b, solstice).get();
        ta.total_cmp(&tb)
    }).unwrap();
    let lat = geo.coord(warmest).latitude;
    assert!(lat > 10.0, "at northern solstice the hot spot has climbed north, got lat {lat}");
}

#[test]
fn locked_temperature_is_static_at_zero_obliquity() {
    let geo = Geosphere::new(3);
    let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0));
    let sea = ReferenceElevation::new(0.0);
    let regime = RotationRegime::Locked;
    let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime);
    let cell = CellId(0);
    let a = temperature_at(&mean, &geo, &elevation, sea, 0.0, 1.0, 240.0, 0.0, &regime, cell, 0.0).get();
    let b = temperature_at(&mean, &geo, &elevation, sea, 0.0, 1.0, 240.0, 0.0, &regime, cell, 120.0).get();
    assert_eq!(a, b, "zero obliquity: no libration, temperature is day-independent");
}
```

- [ ] **Step 2: Verify it fails** — `cargo test -p hornvale-climate locked_substellar` (signature mismatch + static locked temperature).
- [ ] **Step 3: Implement.** In `substellar.rs`:

```rust
/// Unit substellar direction at latitude `lat_deg`, longitude 0 — the
/// locked world's sub-solar point as it librates over the year. At
/// latitude 0 this is `SUBSTELLAR`.
/// type-audit: pending(wave-2: lat_deg), bare-ok(ratio: return)
pub fn substellar_at(lat_deg: f64) -> [f64; 3] {
    let lat = lat_deg.to_radians();
    [math::cos(lat), 0.0, math::sin(lat)]
}

/// Cosine of the angle between `p` and an arbitrary substellar direction.
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: dir), bare-ok(ratio: return)
pub fn substellar_cosine_dir(p: [f64; 3], dir: [f64; 3]) -> f64 {
    p[0] * dir[0] + p[1] * dir[1] + p[2] * dir[2]
}

/// The locked-world surface temperature (°C) from the substellar cosine, the
/// insolation `scale` (S^{1/4}), and the lapse cooling — the one mapping
/// `mean_temperature` and the librating `temperature_at` both use.
/// type-audit: bare-ok(ratio: cos_theta), bare-ok(ratio: scale), pending(wave-2: lapse), pending(wave-2: return)
pub fn locked_cell_temperature(cos_theta: f64, scale: f64, lapse: f64) -> f64 {
    if cos_theta > 0.0 {
        (-18.0 + 78.0 * math::powf(cos_theta, 0.3) * scale) - lapse
    } else {
        -60.0 - lapse
    }
}
```

(add `use hornvale_kernel::math;` to `substellar.rs`.) Refactor `mean_temperature`'s locked branch to `crate::locked_cell_temperature(crate::substellar_cosine(p), scale, lapse)` (byte-identical: `substellar_cosine` = `substellar_cosine_dir(p, SUBSTELLAR)` = `substellar_at(0.0)`). In `temperature_at` (signature already carries `insolation` from Task 2), replace the locked branch:

```rust
RotationRegime::Locked => {
    if obliquity_deg == 0.0 || year_length_std <= 0.0 {
        return base; // no libration to apply
    }
    let year_phase = (day / year_length_std + year_phase_offset).rem_euclid(1.0);
    let sub_lat = obliquity_deg * math::sin(std::f64::consts::TAU * year_phase);
    let dir = crate::substellar_at(sub_lat);
    let cos_theta = crate::substellar_cosine_dir(geo.position(cell), dir);
    let scale = math::powf(insolation.max(0.0), 0.25);
    let above = (*elevation.get(cell) - sea_level).max(0.0);
    let lapse = LAPSE_C_PER_M * above;
    Temperature::new(crate::locked_cell_temperature(cos_theta, scale, lapse))
        .expect("temperature is finite")
}
```

Re-export the new substellar helpers from `climate/src/lib.rs` if it re-exports the module's items (match how `substellar_cosine` is re-exported). No `provider.rs` change here — the wrapper already passes `self.insolation`/`self.year_phase_offset` (Task 2).
- [ ] **Step 4: Green** — `cargo test -p hornvale-climate`.
- [ ] **Step 5: Confirm the mean is byte-identical** — the seed-1 tiles golden (constant/spinning) is unaffected; if any locked-world mean test exists, it must still pass (the refactor is a pure extraction). `cargo test -p hornvale-climate -p hornvale-scene`.
- [ ] **Step 6: fmt + clippy + type-audit** — `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`; regenerate the report if flagged. Commit — `feat(climate): librating substellar temperature for locked worlds`.

---

### Task 4: `scene/tiles/v1` carries a `locked` marker

**Files:**
- Modify: `windows/scene/src/lib.rs` (`TilesScene` struct ~line 106 — append `locked`; `tiles_scene` builder ~line 217 — set it; the struct's `type-audit:` line)

**Interfaces:**
- Consumes: `hornvale_worldgen`'s climate regime — the builder already calls `climate.band_count()`; locked worlds return `None`. Prefer an explicit source: `climate.is_locked()` if it exists, else derive from the regime. Grep `is_locked`/`RotationRegime` in `windows/scene` + `windows/worldgen`; add `pub fn is_locked(&self) -> bool` on `GeneratedClimate` (returns `matches!(self.regime, RotationRegime::Locked)`) if absent.
- Produces: `TilesScene.locked: bool` in `scene/tiles/v1` JSON (appended after `circulation_bands`). Task 6/7 parse it as `locked: boolean`.

- [ ] **Step 1: Failing test** — extend the tiles determinism test (in `windows/scene/src/lib.rs` tests): a locked world's tiles document has `locked == true`; a spinning world's has `locked == false`. Use a locked seed (find one via `cargo run -p hornvale -- new --seed 8`; seed 8 is locked) and seed 42 (spinning).

```rust
#[test]
fn tiles_scene_marks_locked_worlds() {
    use hornvale_kernel::Seed;
    use hornvale_worldgen::{SkyChoice, build_world};
    let build = |s| build_world(Seed(s), &Default::default(), SkyChoice::Generated, &Default::default(), &Default::default()).unwrap();
    assert!(tiles_scene(&build(8), 16).unwrap().locked, "seed 8 is tidally locked");
    assert!(!tiles_scene(&build(42), 16).unwrap().locked, "seed 42 spins");
}
```

- [ ] **Step 2: Verify it fails** — `cargo test -p hornvale-scene tiles_scene_marks_locked_worlds` (no field).
- [ ] **Step 3: Implement** — append to `TilesScene`:

```rust
/// Whether this world is tidally locked — the client reads its seasonal
/// temperature from the librating-substellar reconstruction rather than the
/// hemisphere-signed sinusoid. Appended per the schema stability contract.
pub locked: bool,
```

Add `bare-ok(flag: locked)` to the struct's `type-audit:` line. In the builder set `locked: climate.is_locked(),`. `bool` is not quantized.
- [ ] **Step 4: Green + drift** — `cargo test -p hornvale-scene`. Then regenerate committed artifacts and inspect: `bash scripts/regenerate-artifacts.sh` (foreground, timeout 3600000) — the seed-42 gallery `scene-tiles-seed-42.json` gains `"locked":false`; the seed-1 golden (`tiles-seed-1-w16.json`) is constant-sun (check whether it is locked or spinning and that only the `locked` field is added). Re-pin the golden with its documented mechanism (`REBASELINE=1 cargo test -p hornvale-scene --test golden`) IN THIS COMMIT if it drifts. `git status` shows only the expected `locked` additions.
- [ ] **Step 5: type-audit + gate + commit** — `make gate` (timeout 3600000). Commit — `feat(scene): scene/tiles/v1 marks tidally-locked worlds`.

---

### Task 5: Producer-sourced locked golden + census-drift measurement (GATE)

**Files:**
- Create: `windows/scene/examples/locked_temperature_golden.rs` (mirrors `region_temperature_golden.rs`)
- Create (committed, orrery side later): the golden CSV goes to the orrery in Task 7; this task PRODUCES it and MEASURES census drift.

**Interfaces:**
- Consumes: the Task 3 producer `temperature_at` for a locked world; the census study definitions.
- Produces: `windows/scene/examples/locked_temperature_golden.rs` emitting `node_index,day,temperature_c` for seed 8 at a fixed tile lattice + days `{0, year/4, year/2, 3·year/4}`; a written **census-drift report** for the controller to relay to Nathan.

- [ ] **Step 1: Write the golden generator** — mirror `windows/scene/examples/region_temperature_golden.rs` (its structure), but sample the whole seed-8 tiles lattice temperature at 4 days spanning the year. Emit a provenance header (seed 8, locked, the day list) + `node_index,day,temperature_c` rows for a fixed node set (e.g. every 64th node) at full precision.
- [ ] **Step 2: Run it** — `cargo run -p hornvale-scene --example locked_temperature_golden > /tmp/locked-golden.csv`; sanity check: rows exist, values vary across days (the libration moves the field), and at day 0 / year/2 (substellar at equator) the warmest rows are near the equator, at year/4 near +22° lat.
- [ ] **Step 3: MEASURE census drift (do not regen).** Determine whether any census metric samples `temperature_at` at a *day* (drifts) vs annual statistics (mean/coldest, unaffected). Inspect `studies/*.study.json` and `windows/lab/src` for temperature metrics; grep `temperature_at`, `temperature`, `coldest`, `seasonal` in the lab metric code. For each temperature-derived census metric, classify: annual-only (safe) or day-sampled (drifts). Write the classification to `.superpowers/sdd/census-drift-report.md` in the worktree.
- [ ] **Step 4: Confirm no local artifact drift beyond the expected** — the census is NOT regenerated locally (SKIP by default). `bash scripts/regenerate-artifacts.sh` was run in Task 4; confirm `git status` is clean of unexpected drift now.
- [ ] **Step 5: Commit the generator** — `chore(scene): locked-world temperature golden generator`. (The CSV itself lands in the orrery in Task 7.)
- [ ] **Step 6: STOP for the controller** — report the census-drift classification to the controller; the controller relays it to Nathan and gets the regen decision before G6. Do NOT run any AWS regen.

---

### Task 6: Orrery — `temperatureAt` follows `year_phase`, parser reads `locked`

**Files (orrery worktree):**
- Modify: `src/sim/climate.ts` (`temperatureAt` ~line 14 — add `yearPhaseOffset`)
- Modify: `src/sim/scene.ts` (`TilesScene` interface + `parseTiles` — add `locked`)
- Modify: `src/views/ice.ts`, `src/views/lens.ts`, `src/views/globe.ts` (callers of `temperatureAt` — thread `sys.world.yearPhaseOffset`)
- Modify: `src/sim/climate.test.ts`, `src/sim/scene.test.ts`

**Interfaces:**
- Consumes: `sys.world.yearPhaseOffset` (already parsed in `scene/system/v1`); the producer golden shape from Task 5.
- Produces: `temperatureAt(src, i, day, yearPhaseOffset)` phasing on `frac(day/season_period + yearPhaseOffset)`; `TilesScene.locked: boolean`.

- [ ] **Step 1: Failing tests** — in `climate.test.ts`, a nonzero offset shifts the seasonal peak (mirror the Rust Task 2 test in TS against a hand-built `t_mean_c`/`t_swing_c` fixture). In `scene.test.ts`, `parseTiles` requires `locked` (a document without it throws; `locked:true`/`false` round-trips).
- [ ] **Step 2: Red** — `npx vitest run src/sim/climate.test.ts src/sim/scene.test.ts`.
- [ ] **Step 3: Implement** — `temperatureAt(src, i, day, yearPhaseOffset)`: `src.t_mean_c[i] + src.t_swing_c[i] * Math.sin(2*Math.PI * frac(day / src.season_period_days + yearPhaseOffset))`. Update the doc comment (it currently says the phase is NOT offset — reverse that). Add `locked: boolean` to the `TilesScene` interface and `parseTiles` (`requireBoolean`-style; follow the file's helpers). Thread `sys.world.yearPhaseOffset` at every `temperatureAt` caller (`ice.ts`, `lens.ts`, `globe.ts`). Note: for locked worlds this spinning-phase branch is superseded by Task 7's reconstruction — keep `temperatureAt` correct for spinning worlds here.
- [ ] **Step 4: Green** — `npx vitest run src/sim/climate.test.ts src/sim/scene.test.ts`, then `npm test` (whole suite) — the vendored binary must be a Task-4+ build carrying `locked`; if the fixture binary predates it, rebuild + copy per Task 8's binary step (or run this after Task 8). If `locked` is missing from the vendored doc, note it and coordinate the binary refresh.
- [ ] **Step 5: tsc + commit** — `npx tsc --noEmit`; `feat(the-wandering-sun): temperatureAt follows the year phase; parse the locked marker`.

---

### Task 7: Orrery — locked-world temperature reconstruction (pinned by the golden)

**Files (orrery worktree):**
- Create: `src/sim/lockedClimate.ts` + `src/sim/lockedClimate.test.ts`
- Create (committed): `testdata/locked-temperature-golden-seed8.csv` (from Task 5's generator)
- Modify: `src/views/ice.ts`, `src/views/lens.ts`, `src/views/globe.ts` (the `temperatureAt` call sites branch on `tiles.locked` → call `seasonalTemperatureAt`, a thin dispatcher, instead of `temperatureAt` directly)

**Interfaces:**
- Consumes: Task 5's `locked-temperature-golden-seed8.csv` (producer-sourced); the world's `obliquityDeg`/`yearPhaseOffset` (from `scene/system/v1`) and `insolation` (derived, Step 4); the tiles scene's `width`/`height`/`elevation_m`/`sea_level_m`/`season_period_days`.
- Produces: `lockedTemperatureAt(tiles: TilesScene, i: number, day: number, obliquityDeg: number, yearPhaseOffset: number, insolation: number): number` (the librating-substellar reconstruction; derives the tile's lat/lon from `tiles.width`/`height` + `i`), and a dispatcher `seasonalTemperatureAt(tiles, i, day, ctx)` where `ctx = { yearPhaseOffset, obliquityDeg, insolation }` that routes `tiles.locked ? lockedTemperatureAt(...) : temperatureAt(tiles, i, day, ctx.yearPhaseOffset)`. Keeping `temperatureAt` (Task 6) spinning-only avoids entangling its signature with world-level params.

- [ ] **Step 1: Vendor the golden** — copy Task 5's output: `cargo run -p hornvale-scene --example locked_temperature_golden` (in the hornvale worktree) `> ~/.config/superpowers/worktrees/orrery/the-wandering-sun/testdata/locked-temperature-golden-seed8.csv`.
- [ ] **Step 2: Failing test** — `lockedClimate.test.ts` reads the golden CSV and asserts `lockedTemperatureAt(...)` matches each row to a tolerance (the golden is full precision; the client mapping should match to ~1e-3, matching the existing climate-golden tolerance). The test needs each node's lat/lon (equirect mapping) and the world's obliquity/offset/period for seed 8 (hardcode from `scene system --world seed8` values in the test, or read from a committed seed-8 system fixture).
- [ ] **Step 3: Red** — `npx vitest run src/sim/lockedClimate.test.ts`.
- [ ] **Step 4: Implement** `lockedTemperatureAt`: `subLat = obliquityDeg * sin(2π·frac(day/seasonPeriodDays + yearPhaseOffset))`; `dir = (cos subLat, 0, sin subLat)`; `p = latLonToUnit(latDeg, lonDeg)`; `cosTheta = dot(p, dir)`; then the producer mapping `cosTheta>0 ? -18 + 78·cosTheta^0.3·scale : -60`, minus the elevation lapse (`LAPSE_C_PER_M · max(0, elevation_m − sea_level_m)`; read the constant from the producer — grep `LAPSE_C_PER_M` in `domains/climate/src/temperature.rs` and mirror the value with a comment citing it). **`scale` = insolation^0.25**, where insolation is computed from `scene/system/v1`: `insolation = star.luminosityRel / (world.orbitAu)²` (the producer's `insolation_rel(star, anchor)`; both fields are already parsed in the system scene). The **golden is the arbiter** — if the reconstructed values match the producer golden to 1e-3 (Step 2's test), the insolation derivation is correct; if they systematically miss by a scale factor, the producer used a different insolation and the fix is to append `insolation_rel` to `scene/tiles/v1` additively (quantized, documented) and read it there. Add the `seasonalTemperatureAt(tiles, i, day, ctx)` dispatcher (routes locked→`lockedTemperatureAt`, else→`temperatureAt`). Update the three call sites (`ice.ts`, `lens.ts`, `globe.ts`) to call `seasonalTemperatureAt` with a `ctx` built from the system scene's `obliquityDeg`/`yearPhaseOffset` + the derived `insolation`. `temperatureAt` (Task 6) is unchanged — the dispatcher wraps it.
- [ ] **Step 5: Green** — `npx vitest run src/sim/lockedClimate.test.ts`; `npm test`; `npx tsc --noEmit`.
- [ ] **Step 6: Commit** — `feat(the-wandering-sun): locked-world librating-substellar temperature reconstruction`.

> **Note for the controller/plan-writer:** Task 7 Step 4 exposes a real
> sub-decision (how the client gets `scale`/insolation and lapse). If reading
> them from existing scene layers is clean, do so; if it needs an additive
> `insolation_rel` on `scene/tiles/v1`, that is an additive field (append,
> quantize, document) — resolve during Task 4/7 and keep it additive.

---

### Task 8: Orrery — lift the globe speed cap + wasm binary refresh

**Files:**
- Modify (orrery): `src/time/speedPolicy.ts` (the `globe` cap ~line 14)
- Modify (orrery): `src/time/speedPolicy.test.ts` (if it pins the cap)
- Produces (not committed): a rebuilt `hornvale_world.wasm` (Task 4's `locked` marker) copied into the orrery worktree `public/`.

**Interfaces:**
- Consumes: the HUD's existing disable rule (`b.disabled = maxMult !== null && SPEED_STEPS[i].mult > maxMult`) — raising `maxMult` enables the fast buttons automatically.
- Produces: `SPEED_POLICY.globe.maxMult` raised so `10 d/s` (864000) and `~1 mo/s` (2.6e6) are enabled.

- [ ] **Step 1: Build + vendor the wasm** — in the hornvale worktree: `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release`, then `cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm ~/.config/superpowers/worktrees/orrery/the-wandering-sun/public/hornvale_world.wasm`. `ls -la` the destination (fresh timestamp).
- [ ] **Step 2: Failing test** — `speedPolicy.test.ts`: `clampMult('globe', 2.6e6)` returns `2.6e6` (was clamped to 86400). Add/adjust the assertion.
- [ ] **Step 3: Red** — `npx vitest run src/time/speedPolicy.test.ts`.
- [ ] **Step 4: Implement** — raise the globe cap: `globe: { defaultMult: 3600, maxMult: 2.6e6 }` (uncapped up to ~1 mo/s). The default stays 1 hr/s. (The freeze in Task 9 is what makes the fast rates watchable.)
- [ ] **Step 5: Green** — `npx vitest run src/time/speedPolicy.test.ts`; `npm test`.
- [ ] **Step 6: Commit** — `feat(the-wandering-sun): enable the fast rates at the globe`.

---

### Task 9: Orrery — the globe seasonal-freeze

**Files:**
- Modify (orrery): `src/views/globe.ts` (`subsolarPoint` and the `update(day)` loop ~line 424–429)
- Modify (orrery): `src/views/globe.test.ts`
- Modify (orrery): `src/main.ts` (pass the active rate / a "seasonal" flag into the globe `update`, or expose a `setSeasonalHold(on)` on the view)

**Interfaces:**
- Consumes: the active speed mult (from `main.ts`'s clock wiring) — the freeze engages when `mult > 86400` (the old cap).
- Produces: `SeasonalGlobeView.update(day, camera?)` holds `spinGroup.rotation.z` and the sub-solar longitude fixed while advancing seasonal terms, when seasonal-hold is on.

- [ ] **Step 1: Failing tests** — `globe.test.ts`: with seasonal-hold ON, advancing `day` does NOT change `spinGroup.rotation.z` (freeze), but the sub-solar *latitude* still moves with `day`; with hold OFF, `rotation.z` advances with `day` as today. Express via a small `seasonalRotationZ(sys, day, hold)` pure helper so it's testable without WebGL:

```ts
// hold=false -> rotationPhase(sys, day)*TAU (today's spin); hold=true -> a fixed reference (0).
export function seasonalSpinZ(sys, day, hold) { return hold ? 0 : rotationPhase(sys, day) * TAU; }
```

Assert `seasonalSpinZ(sys, 10, true) === seasonalSpinZ(sys, 200, true)` (frozen) and `!== ` for `hold=false` on a spinning world; and that `subsolarPoint(sys, day).lat` still varies with `day` regardless.
- [ ] **Step 2: Red** — `npx vitest run src/views/globe.test.ts`.
- [ ] **Step 3: Implement** — add `seasonalSpinZ`; in `update`, `spinGroup.rotation.z = seasonalSpinZ(sys, day, seasonalHold)`. When `seasonalHold`, place the light using the sub-solar *latitude* but a fixed longitude (already `latLonToUnit(sub.lat, 0)` — longitude is already fixed at 0, so the light needs no change; only the mesh spin freezes). Add a `setSeasonalHold(on: boolean)` to the view; `main.ts` calls it when the active mult crosses the old cap (`mult > 86400`). Add the caption note ("holding the daily spin — watching the year") when hold is on.
- [ ] **Step 4: Green** — `npx vitest run src/views/globe.test.ts`; `npm test`; `npx tsc --noEmit`.
- [ ] **Step 5: Commit** — `feat(the-wandering-sun): freeze the diurnal spin at the fast rates (watch a year)`.

---

### Task 10: Assembly verification + G6 package (controller-run)

- [ ] Hornvale worktree: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean; `mdbook build book`.
- [ ] Orrery worktree: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e, all green (with the Task-8 binary in `public/`).
- [ ] **Census-drift decision:** relay Task 5's `census-drift-report.md` to Nathan; get the regen go/no-go BEFORE close. If a regen is authorized, it runs via the AWS path (`make regen-remote`), never locally.
- [ ] **Visual verification (The Lens rule):** serve the built orrery at root (vite preview), render seed 8 globe at 1 mo/s across a year, confirm the ice cap now TRACKS the terminator (the seed-8 solstice offset closed) — the before/after this campaign exists to demonstrate. Screenshot for the G6 evidence.
- [ ] Conflict-marker sweep (all three markers) across both worktrees; followup register updated; G6 package assembled (ledger digest with the save-format/census entries leading, screenshots, gate evidence, world-wasm-v6 release + orrery re-pin + push plan for Nathan's carve-out approvals).
