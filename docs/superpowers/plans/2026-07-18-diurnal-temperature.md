# Diurnal Temperature Implementation Plan

> **Status: COMPLETE (2026-07-18).** All 7 tasks shipped + final-review fixes (per-longitude model correction, obliquity gate, 2 minors). See [chronicle](../../../book/src/chronicle/the-turning.md) and [retrospective](../../retrospectives/the-turning.md).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a zero-mean diurnal (day/night) term to the climate temperature field on spinning worlds, carried on `scene/tiles/v1` and pulsed on the orrery's temperature lens.

**Architecture:** A pure `diurnal` seam module in `domains/climate` (amplitude × waveform), wired into `temperature_at`'s Spinning branch; the per-cell amplitude precomputed in the provider and emitted as an additive `scene/tiles/v1` field; the orrery reads the amplitude and re-derives the waveform at the clock's time-of-day, golden-pinned. Byte-identical everything else (the diurnal term averages to zero over a rotation, so the mean the census reads never moves).

**Tech Stack:** Rust (edition 2024, `serde`+`serde_json` only, `kernel::math` for all trig), TypeScript/vite/three.js (orrery), cargo-nextest.

## Global Constraints

- **Byte-identical** worlds/ledger/census: the diurnal term is **zero-mean over one rotation**; `mean_temperature` is unchanged. No epoch, no new stream draws, no reordered labels.
- **Spinning worlds only.** `RotationRegime::Locked` returns exactly zero diurnal anomaly (untouched).
- **All transcendentals via `hornvale_kernel::math`** (libm, decision 0041) — never raw `f64::sin`/`cos`/`exp`/`powf`.
- **Quantize at emit only** (decision 0033): new scene floats use `quantize_serde::vec_f64_field`.
- **`scene/tiles/v1` stays v1** (additive field); its byte-pin fixture `windows/scene/tests/fixtures/tiles-seed-1-w16.json` **re-pins in the same commit** that adds the field (never deferred to close).
- **No `HashMap`/`HashSet`**; `#![warn(missing_docs)]` — every new pub item gets a one-line doc comment.
- **No Field-trait refactor** (that is `CLIM-operators`, the winds campaign — spec §7). Extend `temperature_at`.
- `cargo fmt` is the final step before every commit. Type-audit tags required on new pub-boundary primitives.

---

### Task 1: The `diurnal` seam module (pure model)

**Files:**
- Create: `domains/climate/src/diurnal.rs`
- Modify: `domains/climate/src/lib.rs` (add `mod diurnal;` + re-exports)

**Interfaces:**
- Produces:
  - `pub fn diurnal_amplitude(moisture: f64, continentality: f64, elevation_above_sea_m: f64) -> f64` — the per-cell diurnal half-range `A_climate` in °C (≥ 0).
  - `pub fn diurnal_waveform(latitude_deg: f64, obliquity_deg: f64, year_phase: f64, day_fraction: f64, day_length_std: f64) -> f64` — the normalized waveform `D`, zero-mean over `day_fraction ∈ [0,1)`, afternoon-peaked.
  - `pub fn diurnal_anomaly(amplitude: f64, latitude_deg: f64, obliquity_deg: f64, year_phase: f64, day_fraction: f64, day_length_std: f64) -> hornvale_kernel::units::TempAnomaly` — `TempAnomaly::from_offset_c(amplitude * D)`.

**The model (spec §2 — implement exactly):**
```
// diurnal_amplitude (all inputs already available per-cell in the provider):
//   dryness  = DRY_FLOOR + (1 - DRY_FLOOR) * (1 - moisture.clamp(0,1))
//   elev     = 1 + ELEV_GAIN_PER_M * elevation_above_sea_m.max(0)
//   A_climate = BASE_DTR_HALF_C * dryness * continentality.clamp(0,1) * elev   // °C, >= 0
// constants: BASE_DTR_HALF_C = 15.0, DRY_FLOOR = 0.15, ELEV_GAIN_PER_M = 3.0e-5

// diurnal_waveform:
//   decl_deg = obliquity_deg * math::sin(TAU * year_phase)          // solar declination
//   lat = latitude_deg.to_radians(); dec = decl_deg.to_radians()
//   noon_sin = math::sin(lat)*math::sin(dec) + math::cos(lat)*math::cos(dec)   // sin(noon elevation)
//   a_geo   = noon_sin.max(0.0)                                     // 0 in polar night / low winter sun
//   inertia = 1.0 - math::exp(-day_length_std.max(0.0) / TAU_THERMAL)  // longer day -> bigger swing
//   D = a_geo * inertia * math::cos(TAU * (day_fraction - PEAK_FRAC)) // zero-mean over the day
// constants: TAU_THERMAL = 0.5, PEAK_FRAC = 0.60 (afternoon thermal lag)
// TAU is std::f64::consts::TAU.
```

- [ ] **Step 1: Write the failing tests** in `domains/climate/src/diurnal.rs` (a `#[cfg(test)] mod tests`):

```rust
use super::*;
use hornvale_kernel::math;

const TAU: f64 = std::f64::consts::TAU;

// The keystone: the waveform integrates to ~0 over one rotation (so the daily
// mean, and thus mean_temperature, is preserved). Sample 1000 fractions.
#[test]
fn waveform_is_zero_mean_over_a_rotation() {
    let n = 1000;
    let sum: f64 = (0..n)
        .map(|i| diurnal_waveform(23.4, 23.4, 0.25, i as f64 / n as f64, 1.0))
        .sum();
    assert!((sum / n as f64).abs() < 1e-3, "diurnal waveform must be zero-mean; got {}", sum / n as f64);
}

// Anti-vacuity: a nonzero-amplitude equatorial day actually swings.
#[test]
fn equatorial_day_has_a_real_swing() {
    let peak = (0..100)
        .map(|i| diurnal_waveform(0.0, 23.4, 0.0, i as f64 / 100.0, 1.0))
        .fold(f64::MIN, f64::max);
    assert!(peak > 0.2, "equatorial day should swing; peak D = {peak}");
}

// Phase: the warmest moment is in the local afternoon (day_fraction > 0.5).
#[test]
fn peak_is_in_the_afternoon() {
    let (mut best_frac, mut best) = (0.0, f64::MIN);
    for i in 0..1000 {
        let f = i as f64 / 1000.0;
        let d = diurnal_waveform(0.0, 0.0, 0.0, f, 1.0);
        if d > best { best = d; best_frac = f; }
    }
    assert!(best_frac > 0.5 && best_frac < 0.75, "peak should be early afternoon; got frac {best_frac}");
}

// Polar night: sun never rises -> no diurnal swing. North pole at northern-winter solstice.
#[test]
fn polar_night_has_no_swing() {
    for i in 0..100 {
        let d = diurnal_waveform(89.0, 23.4, 0.75, i as f64 / 100.0, 1.0);
        assert!(d.abs() < 1e-6, "polar night must not swing; got {d}");
    }
}

// A longer rotation day swings harder (thermal inertia has more time to lose).
#[test]
fn longer_day_swings_harder() {
    let amp = |dl: f64| (0..200).map(|i| diurnal_waveform(0.0, 0.0, 0.0, i as f64 / 200.0, dl)).fold(f64::MIN, f64::max);
    assert!(amp(2.0) > amp(0.5), "a 2-day rotation should swing more than a half-day one");
}

// Amplitude drivers are monotone: drier > humid; interior > coastal; higher > lower.
#[test]
fn amplitude_is_monotone_in_its_drivers() {
    assert!(diurnal_amplitude(0.1, 1.0, 0.0) > diurnal_amplitude(0.9, 1.0, 0.0), "drier swings more");
    assert!(diurnal_amplitude(0.5, 1.0, 0.0) > diurnal_amplitude(0.5, 0.2, 0.0), "interior swings more than coast");
    assert!(diurnal_amplitude(0.5, 1.0, 3000.0) > diurnal_amplitude(0.5, 1.0, 0.0), "altitude swings more");
    assert!(diurnal_amplitude(0.5, 0.0, 0.0).abs() < 1e-9, "pure ocean (continentality 0) barely swings");
}

// Calibration sanity: a hot dry equatorial desert reads an Earth-like range.
#[test]
fn desert_range_is_earth_like() {
    let a = diurnal_amplitude(0.05, 1.0, 0.0); // A_climate (half-range)
    let geo_peak = (0..200).map(|i| diurnal_waveform(0.0, 0.0, 0.0, i as f64 / 200.0, 1.0)).fold(f64::MIN, f64::max);
    let peak_to_peak = 2.0 * a * geo_peak;
    assert!((15.0..35.0).contains(&peak_to_peak), "desert DTR should be ~20-30 C; got {peak_to_peak}");
}
```

- [ ] **Step 2: Run to verify they fail** — `cargo test -p hornvale-climate diurnal` → FAIL (module missing).
- [ ] **Step 3: Implement** `diurnal.rs` per the model block above (module doc comment; each pub fn a one-line doc; type-audit tags: amplitude/waveform inputs are `bare-ok(ratio: …)` / `bare-ok(diagnostic-value: …)` — mirror `temperature.rs`'s `locked_temperature_at_position` tag style). Add `mod diurnal;` + `pub use diurnal::{diurnal_amplitude, diurnal_anomaly, diurnal_waveform};` to `lib.rs`.
- [ ] **Step 4: Run to verify pass** — `cargo test -p hornvale-climate diurnal` → PASS (7 tests).
- [ ] **Step 5: Mutation-verify the keystone** — temporarily make `diurnal_waveform` return `math::cos(TAU * day_fraction) + 0.3` (breaks zero-mean); confirm `waveform_is_zero_mean_over_a_rotation` FAILS; revert.
- [ ] **Step 6: fmt + commit** — `cargo fmt`; `git add -A && git commit -m "feat(the-turning): diurnal seam module — zero-mean waveform + amplitude"` (+ Claude-Session trailer).

---

### Task 2: Wire diurnal into `temperature_at` + provider precompute

**Files:**
- Modify: `domains/climate/src/temperature.rs` (`temperature_at` Spinning branch + a new `diurnal_amp` param)
- Modify: `domains/climate/src/provider.rs` (precompute the amplitude field + a `diurnal_amp_at(cell)` accessor on the climate field object, mirroring `seasonal_swing_at`)

**Interfaces:**
- Consumes: Task 1's `diurnal_amplitude`, `diurnal_anomaly`; the existing `continentality(geo, elevation, sea_level, cell)` helper in `temperature.rs`; `moisture_field(...)` in `moisture.rs`.
- Produces: `temperature_at(..., diurnal_amp: &CellMap<f64>, ..., day)` gains the diurnal term; the climate field object gains `pub fn diurnal_amp_at(&self, cell: CellId) -> f64`.

- [ ] **Step 1: Write the failing tests** (in `domains/climate/src/temperature.rs` tests, or the crate's integration tests — match where the existing `temperature_at` tests live):

```rust
// Locked worlds have no diurnal term regardless of time-of-day.
#[test]
fn locked_worlds_have_no_diurnal_term() {
    // Build a locked-regime temperature_at call at two different day fractions
    // (day = 0.2 and day = 0.7 within the same integer day) and assert equal.
    // (Mirror the existing locked-branch test's world setup.)
}

// Spinning: temperature at local afternoon exceeds temperature at pre-dawn,
// for a dry interior cell, by a physically-sized margin.
#[test]
fn spinning_afternoon_is_warmer_than_predawn() {
    // Same cell, same integer day, day_fraction 0.60 vs 0.05 -> afternoon warmer.
}

// The daily MEAN is preserved: averaging temperature_at over a rotation equals
// mean + seasonal (the pre-diurnal value) to within quantization.
#[test]
fn daily_mean_is_unchanged_by_the_diurnal_term() {
    // Average temperature_at over 200 day-fractions at fixed integer day; compare
    // to mean_temperature + seasonal at that day. Assert |diff| < 1e-2.
}
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-climate temperature` (or the relevant test target) → FAIL.
- [ ] **Step 3: Implement.**
  - In `temperature.rs`, add `diurnal_amp: &CellMap<f64>` to `temperature_at`'s signature (place it beside `mean`), and in the **`Spinning` branch only**, after computing the seasonal anomaly, add:
    `+ diurnal_anomaly(*diurnal_amp.get(cell), geo.coord(cell).latitude, obliquity_deg, phase_of_year, day.rem_euclid(1.0), day_std)` where `phase_of_year` is the existing `(day / year_length_std + year_phase_offset).rem_euclid(1.0)` and `day_std` is the rotation period from the `Spinning { day_std }` regime. The `Locked` branch is unchanged.
  - Add a module-level `pub fn diurnal_amplitude_field(geo, elevation, sea_level, moisture: &CellMap<f64>) -> CellMap<f64>` that maps each cell to `diurnal_amplitude(*moisture.get(cell), continentality(geo, elevation, sea_level, cell), (*elevation.get(cell) - sea_level).max(0).get())`.
  - In `provider.rs`: precompute `diurnal_amp: CellMap<f64>` once (from the moisture field it already builds), store it on the field object, and add `pub fn diurnal_amp_at(&self, cell: CellId) -> f64 { *self.diurnal_amp.get(cell) }` beside `seasonal_swing_at`. Thread `&self.diurnal_amp` into the internal `temperature_at` call the object already makes.
  - Update every other `temperature_at` caller in the workspace to pass the amplitude field (compile errors will list them).
- [ ] **Step 4: Run to verify pass** — `cargo test -p hornvale-climate` → PASS.
- [ ] **Step 5: Byte-identity check** — `cargo test -p hornvale-cli --test architecture` and any frozen-world fixture test still pass (the mean is unchanged, so `cli/tests/fixtures/world-seed-42.json` is byte-identical). If a world fixture reddens, STOP — the mean moved, which means the diurnal term is not zero-mean; that is a bug, not a re-pin.
- [ ] **Step 6: fmt + commit** — `feat(the-turning): diurnal term on temperature_at (spinning) + provider precompute`.

---

### Task 3: `scene/tiles/v1` field + byte-pin re-pin + almanac line

**Files:**
- Modify: `windows/scene/src/lib.rs` (`TilesScene.t_diurnal_amp_c` + emit + type-audit tag + `serde` quantize)
- Modify: `windows/scene/tests/fixtures/tiles-seed-1-w16.json` (re-pin)
- Modify: the almanac window (`windows/almanac/...`) — a diurnal-range line
- Modify: `windows/scene/src/lib.rs` tests (`t_diurnal_amp_c.len()` assert)

**Interfaces:**
- Consumes: Task 2's `climate.diurnal_amp_at(cell)`.
- Produces: `TilesScene.t_diurnal_amp_c: Vec<f64>` (per-tile °C half-range, quantized), day-ascending tile order identical to `t_mean_c`.

- [ ] **Step 1: Write the failing test** in `windows/scene/src/lib.rs` tests (mirror the `t_mean_c.len()` assert at line ~1043):

```rust
assert_eq!(scene.t_diurnal_amp_c.len(), tiles);
// every amplitude is finite and >= 0
assert!(scene.t_diurnal_amp_c.iter().all(|a| a.is_finite() && *a >= 0.0));
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-scene` → FAIL (field missing).
- [ ] **Step 3: Implement.** In `TilesScene`: add `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")] pub t_diurnal_amp_c: Vec<f64>,` right after `t_swing_c` (with a one-line doc comment). Append `bare-ok(diagnostic-value: t_diurnal_amp_c)` to the struct's `type-audit:` tag line. In the builder: `let mut t_diurnal_amp_c = Vec::with_capacity(tiles);` then in the per-tile loop `t_diurnal_amp_c.push(climate.diurnal_amp_at(c_cell));`, add `.chain(t_diurnal_amp_c.iter())` to the finite-check chain, and add `t_diurnal_amp_c,` to the struct literal.
- [ ] **Step 4: Run to verify pass** — `cargo test -p hornvale-scene` (the `v1_bytes_are_pinned` test WILL fail — expected; re-pin next).
- [ ] **Step 5: Re-pin the byte fixture** — regenerate `windows/scene/tests/fixtures/tiles-seed-1-w16.json` by the method its header/`golden.rs` documents (there is a regen path; if none, the test failure message prints the expected bytes — write them). Re-run `cargo test -p hornvale-scene` → PASS. This re-pin lands in THIS commit.
- [ ] **Step 6: Almanac line** — in the almanac window, add a per-sample-site "diurnal range" readout: `2 * climate.diurnal_amp_at(cell) * <geo_peak at that latitude/day>` °C, plus the hottest/coldest local hour (from `PEAK_FRAC`). A unit test asserts the line renders a positive range on a dry-interior site and ~0 on an ocean site.
- [ ] **Step 7: Regenerate committed artifacts** — `bash scripts/regenerate-artifacts.sh` then `git add` the changed `book/src/gallery/` scene/almanac artifacts (drift-checked in CI).
- [ ] **Step 8: fmt + commit** — `feat(the-turning): scene/tiles/v1 t_diurnal_amp_c + almanac diurnal range (re-pin)`.

---

### Task 4: Producer-sourced golden + world-wasm binary handoff

**Files:**
- Create: `windows/scene/examples/diurnal_temperature_golden.rs` (mirror `windows/scene/examples/locked_temperature_golden.rs`)
- (Handoff) build the wasm and copy to the orrery worktree's `public/`

**Interfaces:**
- Consumes: Task 1's `diurnal_waveform`; the seed-42 climate field.
- Produces: a golden JSON the orrery's TS reconstruction pins against — rows of `{lat_deg, lon_deg, day, day_fraction, diurnal_c}` at **tile centers** (the eval point the client uses — the Wandering Sun lesson), for a fixed set of times-of-day.

- [ ] **Step 1** Write `diurnal_temperature_golden.rs`: for seed 42, for ~8 tile-center coordinates × 6 `day_fraction` values at a fixed integer `day`, print JSON rows of `{lat_deg, lon_deg, day_fraction, amplitude_c, diurnal_c}` where `diurnal_c = amplitude * diurnal_waveform(lat, obliquity, year_phase, day_fraction, day_length_std)`. Mirror the locked golden's structure and provenance comment.
- [ ] **Step 2** Generate the golden into the orrery worktree:
  `cargo run -p hornvale-scene --example diurnal_temperature_golden > /Users/nathan/.config/superpowers/worktrees/orrery/the-turning/testdata/diurnal-golden-seed42.json` — sanity-check it has rows with nonzero `diurnal_c` for a dry cell and ~0 for ocean.
- [ ] **Step 3** Build the release wasm from this worktree and copy it (dev-loop handoff; the v8 release is a G6 action):
  `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release` then `cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm /Users/nathan/.config/superpowers/worktrees/orrery/the-turning/public/hornvale_world.wasm`. Confirm the copied binary's tiles scene carries `t_diurnal_amp_c` (`strings … | grep -c t_diurnal_amp_c` ≥ 1).
- [ ] **Step 4** Commit the golden example (hornvale side only): `feat(the-turning): diurnal reconstruction golden example`. (The golden JSON + binary land in the orrery, committed in Task 5.)

> **Note:** Tasks 1-4 are hornvale (branch `the-turning`). Tasks 5-6 are the orrery (branch `the-turning` in the orrery worktree). Task 7 is the controller.

---

### Task 5: Orrery — parse `t_diurnal_amp_c` + TS diurnal waveform + golden

**Files (orrery worktree `~/.config/superpowers/worktrees/orrery/the-turning`):**
- Modify: `src/sim/scene.ts` (`TilesScene` interface + `parseTiles`)
- Create: `src/sim/diurnal.ts` (`diurnalWaveform(latDeg, obliquityDeg, yearPhase, dayFraction, dayLengthStd): number`) + `src/sim/diurnal.test.ts`
- Create (committed): `testdata/diurnal-golden-seed42.json` (from Task 4)
- Modify: `src/sim/scene.test.ts` (parse test), `src/sim/catalogFixture.test.ts` (vendored-binary field present)

**Interfaces:**
- Consumes: the Task-4 binary at `public/hornvale_world.wasm` (verify `ls -la` shows today's date; if not, STOP — Task 4 first). The golden from Task 4.
- Produces: `parseTiles` result gains `tDiurnalAmpC: number[]`; `diurnalWaveform(...)` mirrors the Rust `diurnal_waveform` **exactly** (same constants `TAU_THERMAL=0.5`, `PEAK_FRAC=0.60`, same formula), used by Task 6.

- [ ] **Step 1: Failing tests.** `scene.test.ts`: a tiles doc with `t_diurnal_amp_c` round-trips to `tDiurnalAmpC` (length = width·height; a wrong length throws). `diurnal.test.ts`: `diurnalWaveform` reproduces every row of `testdata/diurnal-golden-seed42.json` (`amplitude_c * diurnalWaveform(...)` ≈ `diurnal_c`, tol 1e-3); plus zero-mean over 1000 fractions and afternoon-peak (mirror the Rust tests).
- [ ] **Step 2: Red** — `npx vitest run src/sim/scene.test.ts src/sim/diurnal.test.ts`.
- [ ] **Step 3: Implement** `parseTiles` field (strict, quantized-number, house style) + `diurnalWaveform` (port the Rust formula verbatim; `Math.sin/cos/exp` are fine client-side — determinism is not required in the client, decision 0022, but match the golden within 1e-3).
- [ ] **Step 4: Vendored-binary test** in `catalogFixture.test.ts`: `loadSeed42Tiles(...)`'s result has `tDiurnalAmpC.length > 0` and at least one `> 0` value.
- [ ] **Step 5: Green** — `npm test` + `npx tsc --noEmit` (whole suite; the new binary feeds every fixture test — a red elsewhere is a real regression).
- [ ] **Step 6: Commit** (incl. the golden JSON) — `feat(the-turning): parse t_diurnal_amp_c + diurnal waveform (golden-pinned)`.

---

### Task 6: Orrery — diurnal pulse on the temperature lens + "watch a day"

**Files (orrery):**
- Modify: the temperature lens (`src/views/lens.ts` or wherever `t_mean_c`/`t_swing_c` are consumed — grep `tSwingC`) to add the diurnal term
- Modify: `src/main.ts` / `src/ui/hud.ts` (a "watch a day" toggle mirroring the "watch a year"/seasonal-hold control)

**Interfaces:**
- Consumes: Task 5's `tDiurnalAmpC` + `diurnalWaveform`; the clock's fractional day (already tracked — the HUD shows time-of-day); the system scene's obliquity, year length, rotation day.
- Produces: the temperature lens colors include `tDiurnalAmpC[i] * diurnalWaveform(lat_i, obliquity, yearPhase(day), frac(day), dayLengthStd)` added to the existing `mean + seasonal`.

- [ ] **Step 1: Failing unit test** for the lens' temperature-at-tile helper (the pure part): at a fixed dry tile, temperature at `day_fraction 0.60` exceeds `day_fraction 0.05`; over a full day it averages (to tol) to the non-diurnal value. Extract the per-tile temperature math into a pure function if it is not already, so it is unit-testable without WebGL.
- [ ] **Step 2: Red** — `npx vitest run` the lens test.
- [ ] **Step 3: Implement** the diurnal term in the temperature lens' per-tile color, evaluated at the clock's fractional day. Add the **"watch a day"** toggle: hold the seasonal `year_phase` fixed and ensure a spinning (non-seasonal-hold) rate, so the diurnal cycle runs; it composes with — does not fight — the existing seasonal-hold ("watch a year"). Caption text: "holding the season — watch a day".
- [ ] **Step 4: Green** — `npm test` + `npx tsc --noEmit`.
- [ ] **Step 5: Commit** — `feat(the-turning): diurnal pulse on the temperature lens + watch-a-day`.

---

### Task 7: Assembly + visual verification + G6 package (controller-run)

- [ ] Hornvale worktree: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean (no drift); `mdbook build book`.
- [ ] Orrery worktree: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e — all green with the Task-4 binary in `public/`.
- [ ] **Visual verification (The Lens rule):** serve the built orrery, seed 42, switch to the **temperature** lens, engage "watch a day", and screenshot the day running — confirm the **day side warms / night side cools**, a **desert swings hard**, the **ocean stays flat**, and the pulse tracks the terminator. Open the PNGs yourself. (This is exactly the check that caught the buried eclipse band.)
- [ ] Absorb main on both repos if it moved (regenerate `type-audit-report.md` on conflict; hand-resolve semantic collisions); conflict-marker sweep; followup register updated; G6 package assembled (ledger digest with the save-format entry leading, screenshots, gate evidence, world-wasm-v8 release + orrery re-pin + push plan for Nathan's carve-out approvals).
