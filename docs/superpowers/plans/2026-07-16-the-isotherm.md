# The Isotherm Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add per-tile temperature (mean + hemisphere-signed seasonal swing) and moisture, plus document-level season-period and circulation-band elements, to `scene/tiles/v1` as additive cross-repo contracts — documented to the digit and covered by contract tests on both the hornvale producer and orrery consumer sides; consumer proof is a seasonal-ice globe overlay in the orrery.

**Architecture:** The climate domain already computes everything (mean temperature, seasonal amplitude, moisture, circulation bands); this campaign *exposes* it. The producer adds two small provider accessors and five serialized fields, documents the client-side evaluators verbatim in the book, and proves the documented formulas restate the Rust exactly. The orrery re-pins to a rebuilt wasm binary, parses the new fields strictly, mirrors the evaluators, and renders seasonal ice as the walking-skeleton proof that every new element is wired correctly end-to-end.

**Tech Stack:** Rust (edition 2024, std + serde only; `cargo nextest`), TypeScript (orrery: vite + vitest + Playwright), the `hw_*` wasm ABI (no wasm-bindgen), mdBook.

## Global Constraints

- **Additive within `scene/tiles/v1` — no epoch, no `v2` schema.** New fields append after all existing fields; `biome_legend` order and every existing field are untouched (scene-protocol spec §2).
- **Determinism / quantization:** every serialized f64 is quantized at the emit boundary to 8 significant digits via `hornvale_kernel::quantize::quantize_serde` (decision 0033). Never quantize in the compute path.
- **No new dependencies** (serde + serde_json only, workspace-wide); **no `HashMap`/`HashSet`** (BTree/Vec only); **no wall-clock time**. Every public item gets a one-line doc comment (`#![warn(missing_docs)]`). Every primitive at a `pub` boundary carries a `type-audit:` tag.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate`; scope intermediate runs to the changed crate.
- **No new streams, no draw-order change, no worldgen physics change** — these are read-only projections of existing generated state. No census regen (decision 0046).
- **Layer wire order (fixed):** after `features` → `t_mean_c`, `t_swing_c`, `season_period_days`, `circulation_bands` (omitted when locked), `moisture`.
- **Seasonal evaluator (normative, one home = the book):** `t(tile, day) = t_mean_c[tile] + t_swing_c[tile] · sin(τ · frac(day / season_period_days))`, `frac(x) = x − floor(x)`. NOT offset by `year_phase_offset`.
- **Orrery tasks run in a separate `orrery` repo worktree/branch** (`~/.config/superpowers/worktrees/orrery/the-isotherm`), created at Task 5. This plan document lives in hornvale.

---

## Producer tasks (hornvale — worktree `the-isotherm`)

### Task 1: Climate provider — signed seasonal swing + season-period accessors

**Files:**
- Modify: `domains/climate/src/provider.rs` (add two methods to `impl GeneratedClimate`, ~after line 171; add a `#[cfg(test)]` restatement test in the existing `mod tests`)

**Interfaces:**
- Consumes: existing `GeneratedClimate` fields `mean_temp`, `geosphere`, `elevation`, `sea_level`, `obliquity_deg`, `year_length_std`, `regime`; `crate::temperature::seasonal_amplitude`; `hornvale_kernel::math::sin`.
- Produces:
  - `GeneratedClimate::seasonal_swing_at(&self, cell: CellId) -> f64` — hemisphere-signed seasonal half-swing in °C (`amplitude × sign(cell latitude)`); exactly `0.0` when `RotationRegime::Locked`, when `year_length_std <= 0.0`, or when `obliquity_deg == 0.0`.
  - `GeneratedClimate::year_length_std(&self) -> f64` — the seasonal period in standard days.

- [ ] **Step 1: Write the failing test**

In `domains/climate/src/provider.rs`, inside `mod tests`, add (the `inputs`/builder helpers already exist in this module — reuse them exactly as the neighboring tests do; build a spinning world with nonzero obliquity):

```rust
#[test]
fn documented_evaluator_restates_temperature_at_exactly() {
    use hornvale_kernel::math;
    let geo = Geosphere::new(4);
    let inp = inputs(&geo); // existing test helper in this module
    let climate = GeneratedClimate::generate(&inp);
    let period = climate.year_length_std();
    assert!(period > 0.0);
    // The documented client evaluator, restated in Rust with the SAME libm sin
    // and the SAME frac. Must equal temperature_at to the last bit.
    for cell in geo.cells() {
        let mean = climate.mean_temperature_at(cell).get();
        let swing = climate.seasonal_swing_at(cell);
        for &day in &[0.0_f64, 30.0, 91.3, 182.6, 300.0, 365.25, 800.0] {
            let phase = (day / period).rem_euclid(1.0);
            let documented = mean + swing * math::sin(std::f64::consts::TAU * phase);
            let actual = climate.temperature_at(cell, day).get();
            assert_eq!(
                documented.to_bits(),
                actual.to_bits(),
                "cell {} day {day}: documented {documented} != temperature_at {actual}",
                cell.0
            );
        }
    }
}

#[test]
fn seasonal_swing_is_zero_when_locked() {
    let geo = Geosphere::new(3);
    let mut inp = inputs(&geo);
    inp.regime = RotationRegime::Locked;
    let climate = GeneratedClimate::generate(&inp);
    for cell in geo.cells() {
        assert_eq!(climate.seasonal_swing_at(cell), 0.0);
    }
}
```

If the local `inputs` helper does not expose a mutable `regime`/`obliquity_deg`, construct `ClimateInputs { .. }` inline in the test the way `provider.rs`'s other tests do (see the existing `mod tests` for the exact literal — reuse it; do not invent field names).

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-climate --lib documented_evaluator_restates`
Expected: FAIL — `no method named seasonal_swing_at` / `year_length_std`.

- [ ] **Step 3: Add the two accessors**

In `impl GeneratedClimate` (after `temperature_at`, ~line 171):

```rust
/// The seasonal period this climate's temperature swing is phased on,
/// in standard days (the orbital year; a default on constant-sun worlds).
/// type-audit: bare-ok(diagnostic-value: return)
pub fn year_length_std(&self) -> f64 {
    self.year_length_std
}

/// The hemisphere-signed seasonal half-swing at a cell, °C: the
/// coefficient of the seasonal sinusoid, `amplitude × sign(latitude)`.
/// Positive north, negative south, exactly `0.0` when locked, when the
/// world has no year, or at zero obliquity — matching `temperature_at`,
/// which this factors: `temperature_at(cell, day) == mean + swing ·
/// sin(τ · frac(day / year_length_std))`.
/// type-audit: bare-ok(diagnostic-value: return)
pub fn seasonal_swing_at(&self, cell: CellId) -> f64 {
    match self.regime {
        RotationRegime::Locked => 0.0,
        RotationRegime::Spinning { .. } => {
            if self.year_length_std <= 0.0 || self.obliquity_deg == 0.0 {
                return 0.0;
            }
            let amp = crate::temperature::seasonal_amplitude(
                &self.geosphere,
                &self.elevation,
                self.sea_level,
                self.obliquity_deg,
                cell,
            );
            let hemi = self.geosphere.coord(cell).latitude.signum();
            amp * hemi
        }
    }
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p hornvale-climate --lib`
Expected: PASS (all, including the two new tests). If `documented_evaluator_restates` fails on a bit mismatch, the reconstruction and `temperature_at` have diverged — do NOT loosen to an epsilon; find the arithmetic difference (associativity, a missing `rem_euclid`, `std::sin` vs `math::sin`) and fix the accessor so the contract is exact.

- [ ] **Step 5: fmt + commit**

```bash
cargo fmt
git add domains/climate/src/provider.rs
git commit -m "feat(climate): signed seasonal swing + season-period accessors

seasonal_swing_at factors temperature_at's seasonal term into a per-cell
signed coefficient; a restatement test pins mean + swing·sin(τ·frac(day/
period)) == temperature_at to the bit. year_length_std exposes the period.
The Isotherm Task 1.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 2: `scene/tiles/v1` — the five new fields

**Files:**
- Modify: `windows/scene/src/lib.rs` (the `TilesScene` struct ~lines 73–102; `tiles_scene` builder ~lines 108–174; the `#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `GeneratedClimate::mean_temperature_at`, `::seasonal_swing_at`, `::moisture_at`, `::year_length_std`, `::band_count` (Task 1 + existing); `climate_index`/`c_cell` sampling already in `tiles_scene`.
- Produces: `TilesScene` gains public fields `t_mean_c: Vec<f64>`, `t_swing_c: Vec<f64>`, `season_period_days: f64`, `circulation_bands: Option<u32>`, `moisture: Vec<f64>` (this wire order, appended after `features`).

- [ ] **Step 1: Write the failing tests**

In `windows/scene/src/lib.rs` `mod tests`, extend `layers_are_sized_and_legend_is_the_catalog` and add new tests:

```rust
#[test]
fn climate_layers_are_sized_and_present() {
    let scene = tiles_scene(&world(), 32).unwrap(); // seed-1 constant sky: spins, obliquity 23.5
    let tiles = (scene.width * scene.height) as usize;
    assert_eq!(scene.t_mean_c.len(), tiles);
    assert_eq!(scene.t_swing_c.len(), tiles);
    assert_eq!(scene.moisture.len(), tiles);
    assert!(scene.moisture.iter().all(|&m| (0.0..=1.0).contains(&m)));
    assert_eq!(scene.season_period_days, 365.25); // constant-sun default year
    assert_eq!(scene.circulation_bands, Some(3)); // Earth-like day → 3 bands
    // A spinning, obliquity-23.5 world has a nonzero swing somewhere.
    assert!(scene.t_swing_c.iter().any(|&s| s != 0.0));
    // Signed: some tile north-positive, some south-negative.
    assert!(scene.t_swing_c.iter().any(|&s| s > 0.0));
    assert!(scene.t_swing_c.iter().any(|&s| s < 0.0));
}

#[test]
fn locked_world_omits_circulation_bands_and_zeroes_swing() {
    use hornvale_astronomy::{RotationPin, SkyPins};
    use hornvale_kernel::Seed;
    use hornvale_worldgen::{SkyChoice, build_world};
    let sky = SkyPins { rotation: Some(RotationPin::Locked), ..Default::default() };
    let world = build_world(Seed(42), &sky, SkyChoice::Generated, &Default::default(), &Default::default())
        .expect("seed 42 builds locked");
    let scene = tiles_scene(&world, 32).unwrap();
    assert_eq!(scene.circulation_bands, None, "locked world has no bands");
    assert!(scene.t_swing_c.iter().all(|&s| s == 0.0), "locked world has no seasonal swing");
    // The omitted field must not appear in the JSON.
    let json = scene_json(&scene);
    assert!(!json.contains("circulation_bands"), "absent field must not serialize");
    assert!(json.contains("season_period_days"));
}
```

(Confirm `RotationPin` and `SkyPins` are re-exported from `hornvale_astronomy`'s crate root; they are used this way in astronomy's own tests. If a dev-dependency on `hornvale-astronomy` is missing from `windows/scene/Cargo.toml`, add it under `[dev-dependencies]` only — worldgen already re-exports pin types, so prefer `hornvale_worldgen`'s re-export if present.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-scene --lib climate_layers`
Expected: FAIL — no field `t_mean_c` on `TilesScene`.

- [ ] **Step 3: Extend the struct**

In `TilesScene` (append after `features`, keeping the doc comment + type-audit tag convention of the surrounding fields):

```rust
    /// Named points: settlements, the flagship last.
    pub features: Vec<Feature>,
    /// Annual-mean temperature per tile, °C.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_mean_c: Vec<f64>,
    /// Hemisphere-signed seasonal half-swing per tile, °C (0 when locked/zero-obliquity).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_swing_c: Vec<f64>,
    /// The seasonal sinusoid's period, standard days (the world's year).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub season_period_days: f64,
    /// Circulation bands per hemisphere; omitted entirely on tidally locked worlds.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub circulation_bands: Option<u32>,
    /// Moisture index per tile, dimensionless [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub moisture: Vec<f64>,
```

Update the struct-level `type-audit:` doc tag to add the new primitives: `bare-ok(diagnostic-value: t_mean_c), bare-ok(diagnostic-value: t_swing_c), bare-ok(diagnostic-value: season_period_days), bare-ok(count: circulation_bands), bare-ok(ratio: moisture)`.

- [ ] **Step 4: Fill the fields in `tiles_scene`**

In the per-tile loop, alongside the existing `elevation_m.push(...)` etc., push the climate layers (sampled at the climate cell `c_cell`, which the loop already computes):

```rust
        t_mean_c.push(climate.mean_temperature_at(c_cell).get());
        t_swing_c.push(climate.seasonal_swing_at(c_cell));
        moisture.push(climate.moisture_at(c_cell));
```

Declare the three `Vec::with_capacity(tiles)` accumulators beside the existing ones, add `t_mean_c`/`t_swing_c`/`moisture` to the finiteness `debug_assert!` chain, and populate the returned struct:

```rust
        features: features_of(world),
        t_mean_c,
        t_swing_c,
        season_period_days: climate.year_length_std(),
        circulation_bands: climate.band_count(),
        moisture,
```

- [ ] **Step 5: Run scene tests**

Run: `cargo test -p hornvale-scene --lib`
Expected: PASS (new + existing, including `serialized_tiles_carry_no_more_than_eight_significant_digits` — the new f64 layers are quantized).

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/scene/src/lib.rs windows/scene/Cargo.toml
git commit -m "feat(scene): tiles/v1 climate layers — temperature, swing, moisture, period, bands

Five additive fields appended after features (additive-in-v1, no epoch):
t_mean_c, t_swing_c (hemisphere-signed), season_period_days,
circulation_bands (omitted when locked), moisture. The Isotherm Task 2.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 3: Producer contract tests — band restatement + golden/gallery rebaseline + catalog re-verify

**Files:**
- Create: `windows/scene/tests/wind_contract.rs`
- Modify (rebaseline): `windows/scene/tests/fixtures/tiles-seed-1-w16.json`, `book/src/gallery/scene-tiles-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_climate::circulation::{band_count_for, band_index, is_rising_band}`, `RotationRegime`; the documented §3.2 wind formula.
- Produces: a producer-side proof that the documented band derivation equals `circulation.rs`.

- [ ] **Step 1: Write the wind restatement test**

Create `windows/scene/tests/wind_contract.rs`:

```rust
//! The documented prevailing-wind derivation (scene/tiles/v1 schema page
//! §3.2) restated and pinned against domains/climate's circulation model.
//! If the wind model changes, this fails and names the contract break.

use hornvale_climate::circulation::{band_index, is_rising_band};

/// The schema page's documented band function, restated verbatim.
fn documented_band(latitude_deg: f64, bands: u32) -> u32 {
    let width = 90.0 / f64::from(bands);
    ((latitude_deg.abs() / width) as u32).min(bands - 1)
}

#[test]
fn documented_band_matches_circulation() {
    for &bands in &[1_u32, 3, 5, 7] {
        for lat in [-89.0, -60.0, -30.0, -1.0, 0.0, 1.0, 30.0, 60.0, 89.0] {
            assert_eq!(
                documented_band(lat, bands),
                band_index(lat, bands),
                "band mismatch at lat {lat}, bands {bands}"
            );
        }
    }
    // Parity → direction: even bands easterly (rising/wet), odd westerly.
    assert!(is_rising_band(0));
    assert!(!is_rising_band(1));
}
```

- [ ] **Step 2: Run it**

Run: `cargo test -p hornvale-scene --test wind_contract`
Expected: PASS (it restates the same arithmetic).

- [ ] **Step 3: Rebaseline the crate golden (deliberate contract change)**

Run: `REBASELINE=1 cargo test -p hornvale-scene --test golden`
Then inspect the diff — it must be **append-only** (the five new keys at the tail of the object; existing keys and values unchanged):

Run: `git diff windows/scene/tests/fixtures/tiles-seed-1-w16.json`
Expected: the object gains `t_mean_c`, `t_swing_c`, `season_period_days`, `circulation_bands`, `moisture` after `features`; nothing before `features` changes. If any existing value moved, STOP — that is an accidental epoch, not an additive change.

- [ ] **Step 4: Rebaseline the committed gallery artifact**

Run the CI artifact commands for the tiles gallery file (from `.github/workflows/ci.yml`'s "Artifacts are current" step):

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- scene tiles --world /tmp/hv.json --width 256 > book/src/gallery/scene-tiles-seed-42.json
git diff --stat book/src/gallery/scene-tiles-seed-42.json
```

Expected: the file changes (tail-appended fields). Confirm the atlas still parses it (Task-8-equivalent is orrery; here just eyeball that `schema`/`width`/`features` are intact at the head).

- [ ] **Step 5: Re-verify the wasm catalog is byte-identical to native**

Run: `make world-check`
Expected: builds the catalog wasm, runs `clients/world-wasm/drive.mjs`, prints `world-wasm smoke OK (...)`, and the size gate passes (≤ 1 MiB). This proves the new fields flow identically through the wasm ABI and native CLI. (No code change needed — the fields ride the existing `hw_scene_tiles`.)

- [ ] **Step 6: fmt + commit**

```bash
cargo fmt
git add windows/scene/tests/wind_contract.rs windows/scene/tests/fixtures/tiles-seed-1-w16.json book/src/gallery/scene-tiles-seed-42.json
git commit -m "test(scene): wind restatement + rebaseline tiles goldens (additive-in-v1)

Pins the documented §3.2 band derivation against circulation.rs. Golden and
gallery tiles artifacts rebaselined deliberately — append-only diff, no epoch
(scene-protocol §2). world-check re-proves wasm==CLI byte-identity. Task 3.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 4: The schema pages (the contract's public face)

**Files:**
- Modify: `book/src/reference/scene-tiles-v1.md`
- Create: `book/src/reference/scene-system-v1.md`
- Modify: `book/src/SUMMARY.md` (add the new page beside the existing scene-tiles reference entry)

**Interfaces:** documentation only; the normative evaluators (§3.1–3.3 of the spec) live here as their single home.

- [ ] **Step 1: Extend `scene-tiles-v1.md`**

Add the five new fields to the document field table (in wire order, after the `features` row), each described exactly as the spec §3 table. Then add three new `## ` sections copied from spec §§3.1–3.3 verbatim in intent:
- `## Reading temperature over the year` — the evaluator, the `frac(day / season_period_days)` phase, the explicit warning that it is **not** offset by `scene/system/v1`'s `year_phase_offset`, and the exact-vs-quantized precision note (producer-side exact; client-side to ~8 significant digits).
- `## Prevailing winds` — the `circulation_bands` derivation (band width, `band(φ)`, even=easterly/rising, odd=westerly/sinking), zonal/direction-only, absent-when-locked.
- `## Ice (a client derivation)` — coldest season `t_mean_c − |t_swing_c|`, the ≤ 0 °C suggested threshold (non-normative), seasonal advance/retreat, locked-world static sheet; note a future sim-side `ice` layer (CLIM-cryosphere) would supersede this additively.

Add to the **Stability** section the append-at-end additive convention: new fields always append after existing ones; wire order is accretion order; names are the compatibility contract.

- [ ] **Step 2: Create `scene-system-v1.md`**

Write the `scene/system/v1` reference page (none exists): the field-by-field table for `schema`, `seed`, `star` (`class_name`, `luminosity_rel`, `hz_inner_au`, `hz_outer_au`), `world` (`orbit_au`, `year_days`, `day_length_days` — absent when locked, `obliquity_deg`, `year_phase_offset`), and `moons[]` (`sidereal_days`, `phase_offset`, `distance_mm`, `size_rel`). Then a `## Reading positions and phases` section documenting the four evaluators the orrery implements, as normative formulas (`frac(x)=x−floor(x)`):
- `worldPhase(t) = frac(t / year_days + year_phase_offset)`
- `synodicDays(i) = period ≥ year ? ∞(never laps) : (period·year)/(year − period)`
- `moonPhase(i, t) = frac(t / synodic + phase_offset)` (0 new, 0.5 full)
- `rotationPhase(t) = day_length_days present ? frac(t / day_length_days) : 0`

State the `day_length_days`-absent (locked) convention and cross-link the tiles page's `season_period_days` note (both use `frac`, but the seasonal phase has no offset while `worldPhase` does — the exact distinction a client must respect).

- [ ] **Step 3: Add the SUMMARY entry + build the book**

Add a bullet for `scene/system/v1` beside the `scene/tiles/v1` reference entry in `book/src/SUMMARY.md`. Then:

```bash
mdbook build book
cargo test -p hornvale --test docs_consistency
```

Expected: book builds; `docs_consistency` PASS (all cross-links resolve, including any `../gallery/…` links you add).

- [ ] **Step 4: commit**

```bash
git add book/src/reference/scene-tiles-v1.md book/src/reference/scene-system-v1.md book/src/SUMMARY.md
git commit -m "docs(scene): document the tiles climate elements + a scene/system/v1 page

Five new tiles fields + the three evaluators (temperature/wind/ice) as the
single normative home; new scene/system/v1 reference page with the ephemeris
evaluators. The Isotherm Task 4.

Claude-Session: https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

- [ ] **Step 5: Run the producer commit gate**

Run: `make gate`
Expected: green (fmt, clippy, nextest, doctests). This closes the producer half.

---

## Consumer tasks (orrery — worktree `~/.config/superpowers/worktrees/orrery/the-isotherm`)

### Task 5: Vendor the rebuilt wasm + bump the catalog pin to v2

**Files:**
- Replace: `public/hornvale_world.wasm` (rebuilt from the hornvale branch)
- Modify: `src/sim/catalog.ts` (`CATALOG_VERSION`), `package.json` (`wasm:release` URL), `src/sim/catalog.test.ts` (version assertion if it pins the literal)

**Interfaces:**
- Produces: `CATALOG_VERSION = "world-wasm-v2"`; a vendored binary that emits the five new tiles fields.

- [ ] **Step 1: Create the orrery worktree**

```bash
git -C ~/Projects/hornvale/orrery worktree add ~/.config/superpowers/worktrees/orrery/the-isotherm -b the-isotherm
cd ~/.config/superpowers/worktrees/orrery/the-isotherm && npm install
```

- [ ] **Step 2: Build + vendor the wasm from the hornvale branch**

```bash
make -C ~/.config/superpowers/worktrees/hornvale/the-isotherm wasm-world
mkdir -p public && cp ~/.config/superpowers/worktrees/hornvale/the-isotherm/clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm public/hornvale_world.wasm
```

- [ ] **Step 3: Confirm the vendored binary carries the new fields**

Run: `node scripts/smoke.mjs`
Expected: `smoke: hw_new(42) -> 0, ... all ok`. Then eyeball a field is present:

```bash
node -e 'const fs=require("fs");(async()=>{const {instance}=await WebAssembly.instantiate(fs.readFileSync("public/hornvale_world.wasm"),{});const e=instance.exports;e.hw_new(42n);e.hw_scene_tiles(16);const t=new TextDecoder().decode(new Uint8Array(e.memory.buffer,e.hw_out_ptr(),e.hw_out_len()));const d=JSON.parse(t);console.log("has t_mean_c:",Array.isArray(d.t_mean_c),"bands:",d.circulation_bands,"period:",d.season_period_days)})()'
```
Expected: `has t_mean_c: true bands: 3 period: 365.25`.

- [ ] **Step 4: Bump the version pin**

In `src/sim/catalog.ts`: `export const CATALOG_VERSION = "world-wasm-v2";`
In `package.json`: change both `world-wasm-v1` occurrences in `wasm:release` to `world-wasm-v2`. (The tag is published at G6; until then `wasm:local` and the committed binary are authoritative — note this in the commit body.)
If `catalog.test.ts` asserts the literal `"world-wasm-v1"`, update it to `v2`.

- [ ] **Step 5: Run tests (still green on the version bump alone)**

Run: `npm test`
Expected: PASS (parser not yet strict about new fields — old tests unaffected).

- [ ] **Step 6: commit**

```bash
git add public/hornvale_world.wasm src/sim/catalog.ts package.json src/sim/catalog.test.ts
git commit -m "chore(catalog): vendor world-wasm with climate layers, pin v2

Rebuilt binary emits scene/tiles/v1 temperature/moisture/period/bands.
CATALOG_VERSION -> world-wasm-v2; the tag is published at campaign close.
The Isotherm Task 5.

https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 6: Strict typed parsing of the new fields

**Files:**
- Modify: `src/sim/scene.ts` (the `TilesScene` interface + its parser)
- Modify: `src/sim/scene.test.ts`

**Interfaces:**
- Consumes: existing `parseTiles`/`SceneFormatError`, the file's `validTiles()` test builder.
- Produces: `TilesScene` interface gains `t_mean_c: number[]`, `t_swing_c: number[]`, `season_period_days: number`, `circulationBands: number | null`, `moisture: number[]`. (Follow the file's existing convention: per-tile arrays keyed generically stay snake_case like `elevation_m`; scalars/property-read fields camelCase like `biomeLegend` — so `circulationBands` camelCase (mapped from wire `circulation_bands`), `t_mean_c`/`t_swing_c`/`moisture` snake_case to match `elevation_m`.)

- [ ] **Step 1: Write failing tests**

The file's parser is `parseTiles(text): TilesScene` and its test fixture is the `validTiles()` builder (a synthetic valid document — the repo has no committed real doc; Task 8's wasm loader is the first real-document exercise). Extend `validTiles()` to include the five new fields, then add acceptance + rejection tests in the existing `describe('parseTiles', ...)` block:

```ts
// In validTiles(), add to the returned object (after `features`):
    t_mean_c: Array(tiles).fill(15.0),
    t_swing_c: Array(tiles).fill(5.0),
    season_period_days: 365.25,
    circulation_bands: 3,
    moisture: Array(tiles).fill(0.5),
```

```ts
it('reads the climate layers with correct lengths', () => {
  const t = parseTiles(JSON.stringify(validTiles()));
  expect(t.t_mean_c.length).toEqual(128);
  expect(t.t_swing_c.length).toEqual(128);
  expect(t.moisture.length).toEqual(128);
  expect(t.season_period_days).toEqual(365.25);
  expect(t.circulationBands).toEqual(3);
});

it('treats an absent circulation_bands as null (locked world)', () => {
  const doc = validTiles();
  delete doc.circulation_bands;
  expect(parseTiles(JSON.stringify(doc)).circulationBands).toBeNull();
});

it('rejects a document missing a climate layer', () => {
  const doc = validTiles();
  delete doc.t_mean_c;
  expect(() => parseTiles(JSON.stringify(doc))).toThrow('t_mean_c');
});

it('rejects a t_swing_c whose length disagrees with the lattice', () => {
  const doc = validTiles();
  (doc.t_swing_c as number[]).pop();
  expect(() => parseTiles(JSON.stringify(doc))).toThrow('t_swing_c');
});
```

- [ ] **Step 2: Run to verify failure**

Run: `npm test -- scene`
Expected: FAIL — properties undefined / no validation throws.

- [ ] **Step 3: Extend the interface + parser**

Add the five fields to the `TilesScene` interface (doc-commented like the existing fields). In the parser, after the existing per-tile layer validation, validate each new numeric array is present, is an array of finite numbers, and has length `width × height`; validate `season_period_days` is a finite positive number; validate `circulationBands` is either absent (→ `null`) or an integer ≥ 1. Reuse the file's existing length/finiteness helpers (the ones already validating `elevation_m`) rather than writing new ones — DRY.

- [ ] **Step 4: Run to verify pass**

Run: `npm test -- scene`
Expected: PASS.

- [ ] **Step 5: commit**

```bash
git add src/sim/scene.ts src/sim/scene.test.ts
git commit -m "feat(scene): strict parse of the tiles climate layers

t_mean_c/t_swing_c/moisture (length-checked), season_period_days (positive),
circulationBands (null when locked, else >=1). The Isotherm Task 6.

https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 7: Client evaluators + producer-pinned equivalence

**Files:**
- Create: `src/sim/climate.ts`, `src/sim/climate.test.ts`
- Create: `testdata/climate-triples-seed-42.json` (producer-pinned golden triples)

**Interfaces:**
- Consumes: `TilesScene` (Task 6).
- Produces:
  - `temperatureAt(tiles: TilesScene, i: number, day: number): number` — `t_mean_c[i] + t_swing_c[i]·sin(τ·frac(day/season_period_days))`.
  - `windAt(bands: number, latitudeDeg: number): { band: number; direction: "easterly" | "westerly" }` — the §3.2 derivation.
  - `coldestC(tiles: TilesScene, i: number): number` — `t_mean_c[i] − |t_swing_c[i]|`.

- [ ] **Step 1: Generate the pinned golden triples (from the producer)**

From the hornvale branch, emit a handful of `(tileIndex, day) → t` triples through the CLI at a fixed width, sampled from the seed-42 world, and hand-place them in `testdata/climate-triples-seed-42.json` with a provenance comment field. Concretely, compute them with a tiny node driver over the vendored wasm so the values are exactly what the client will see (quantized), then freeze them:

```bash
node -e 'const fs=require("fs");(async()=>{
  const {instance}=await WebAssembly.instantiate(fs.readFileSync("public/hornvale_world.wasm"),{});
  const e=instance.exports; e.hw_new(42n); e.hw_scene_tiles(64);
  const d=JSON.parse(new TextDecoder().decode(new Uint8Array(e.memory.buffer,e.hw_out_ptr(),e.hw_out_len())));
  const TAU=2*Math.PI, frac=x=>x-Math.floor(x);
  const t=(i,day)=>d.t_mean_c[i]+d.t_swing_c[i]*Math.sin(TAU*frac(day/d.season_period_days));
  const rows=[[0,0],[100,0],[100,91.3],[100,182.6],[2000,300]].map(([i,day])=>({i,day,t:t(i,day)}));
  fs.writeFileSync("testdata/climate-triples-seed-42.json",JSON.stringify({
    provenance:"seed 42, width 64, world-wasm-v2; regenerate: this driver in Isotherm plan Task 7",
    width:64, season_period_days:d.season_period_days, rows},null,2));
  console.log("wrote",rows.length,"triples");
})()'
```

This driver **is** the reference evaluator; freezing its output makes the committed file the contract witness. (Because the driver and `climate.ts` share the same formula, the equivalence test guards against `climate.ts` drifting from the frozen values — a real regression guard, e.g. someone "optimizing" the phase.)

- [ ] **Step 2: Write the failing tests**

`src/sim/climate.test.ts`:

```ts
import { readFileSync } from "node:fs";
import { expect, test } from "vitest";
import { temperatureAt, windAt, coldestC } from "./climate";
import type { TilesScene } from "./scene";

const triples = JSON.parse(readFileSync("testdata/climate-triples-seed-42.json", "utf8"));

test("temperatureAt reproduces the producer-pinned triples", () => {
  // Reconstruct a minimal TilesScene view the evaluator needs from the vendored wasm,
  // OR (simpler) assert temperatureAt against the frozen scene the driver used.
  // Here we rebuild t from the same layers the driver read:
  // (load the width-64 seed-42 doc via the binary-as-fixture helper from Task 8,
  //  or inline a tiny loader; keep ONE loader — see Task 8 note.)
});

test("windAt buckets by latitude and alternates direction", () => {
  expect(windAt(3, 0).band).toBe(0);
  expect(windAt(3, 0).direction).toBe("easterly");
  expect(windAt(3, 45).band).toBe(1);
  expect(windAt(3, 45).direction).toBe("westerly");
  expect(windAt(3, 90).band).toBe(2); // clamped
});

test("coldestC is mean minus the swing magnitude", () => {
  const tiles = { t_mean_c: [10], t_swing_c: [-8] } as unknown as TilesScene;
  expect(coldestC(tiles, 0)).toBe(2);
});
```

Note on the temperature test: the cleanest witness is the binary-as-fixture loader from Task 8. To avoid two wasm loaders, **do Task 8 first if executing inline**, then import its `loadSeed42Tiles(width)` helper here. If Task 7 runs first, inline a minimal loader in the test and refactor to the shared helper in Task 8. Either way the assertion is: for each `row` in `triples.rows`, `temperatureAt(doc, row.i, row.day)` equals `row.t` within 1e-9 (both sides read the same quantized layers, so equality is essentially exact; the tiny epsilon absorbs JS `Math.sin` vs the driver being the same call — they are identical here, so `toBeCloseTo(row.t, 10)`).

- [ ] **Step 3: Run to verify failure**

Run: `npm test -- climate`
Expected: FAIL — `./climate` has no such exports.

- [ ] **Step 4: Implement `src/sim/climate.ts`**

```ts
/** Client-side evaluators for scene/tiles/v1 climate layers — the normative
 * formulas documented in the book's scene/tiles/v1 reference. Mirrors the
 * shape of ./ephemeris.ts (pure functions over parsed scene documents). */
import type { TilesScene } from "./scene";

const frac = (x: number) => x - Math.floor(x);

/** Temperature at tile `i` on absolute standard `day`, °C. Self-contained:
 * the seasonal period is the document's own `season_period_days`, and the
 * phase is NOT offset by scene/system/v1's year_phase_offset. */
export function temperatureAt(tiles: TilesScene, i: number, day: number): number {
  return tiles.t_mean_c[i]! + tiles.t_swing_c[i]! * Math.sin(2 * Math.PI * frac(day / tiles.season_period_days));
}

/** Coldest-season temperature at tile `i`, °C — the freeze test's input. */
export function coldestC(tiles: TilesScene, i: number): number {
  return tiles.t_mean_c[i]! - Math.abs(tiles.t_swing_c[i]!);
}

/** Prevailing wind band + direction at a latitude, given the document's
 * circulation_bands. Even bands easterly (rising/wet), odd westerly. */
export function windAt(bands: number, latitudeDeg: number): { band: number; direction: "easterly" | "westerly" } {
  const width = 90 / bands;
  const band = Math.min(Math.floor(Math.abs(latitudeDeg) / width), bands - 1);
  return { band, direction: band % 2 === 0 ? "easterly" : "westerly" };
}
```

- [ ] **Step 5: Run to verify pass**

Run: `npm test -- climate`
Expected: PASS.

- [ ] **Step 6: commit**

```bash
git add src/sim/climate.ts src/sim/climate.test.ts testdata/climate-triples-seed-42.json
git commit -m "feat(climate): client evaluators + producer-pinned equivalence

temperatureAt/windAt/coldestC mirror the book's normative scene/tiles/v1
formulas; frozen seed-42 triples guard against drift. The Isotherm Task 7.

https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 8: Binary-as-fixture contract test

**Files:**
- Create: `src/sim/catalogFixture.test.ts` (+ a small exported `loadSeed42Tiles`/`loadSeed42System` helper, colocated or in a tiny `testHelpers/wasmFixture.ts`)

**Interfaces:**
- Consumes: the vendored `public/hornvale_world.wasm`; `parseTiles`/`parseSystem` (Task 6 + existing).
- Produces: `loadSeed42Tiles(width): TilesScene` and `loadSeed42System(): SystemScene` — the single wasm loader the climate test (Task 7) reuses.

- [ ] **Step 1: Write the test + helper**

```ts
import { readFileSync } from "node:fs";
import { expect, test } from "vitest";
import { parseTiles, parseSystem } from "./scene";

async function exports() {
  const bytes = readFileSync("public/hornvale_world.wasm");
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return instance.exports as any;
}
function readOut(e: any): string {
  return new TextDecoder().decode(new Uint8Array(e.memory.buffer, e.hw_out_ptr(), e.hw_out_len()));
}
export async function loadSeed42Tiles(width: number) {
  const e = await exports(); e.hw_new(42n);
  if (e.hw_scene_tiles(width) !== 0) throw new Error(readOut(e));
  return parseTiles(readOut(e));
}
export async function loadSeed42System() {
  const e = await exports(); e.hw_new(42n);
  if (e.hw_scene_system() !== 0) throw new Error(readOut(e));
  return parseSystem(readOut(e));
}

test("the vendored binary's tiles document parses strictly", async () => {
  const tiles = await loadSeed42Tiles(64);
  expect(tiles.schema).toBe("scene/tiles/v1");
  expect(tiles.t_mean_c).toHaveLength(tiles.width * tiles.height);
  expect(tiles.circulationBands).toBe(3); // seed 42 spins Earth-like
});

test("the vendored binary's system document parses strictly", async () => {
  const sys = await loadSeed42System();
  expect(sys.schema).toBe("scene/system/v1");
  expect(sys.moons.length).toBeGreaterThan(0);
});
```

Match the real exported parser names from `scene.ts`. If Task 7's climate test needs the loader, import `loadSeed42Tiles` from here (single loader — DRY).

- [ ] **Step 2: Run**

Run: `npm test -- catalogFixture`
Expected: PASS — the real binary's real documents satisfy the strict parser. This is the contract's end-to-end proof: producer output ⇒ consumer parser, no committed JSON copy in between.

- [ ] **Step 3: commit**

```bash
git add src/sim/catalogFixture.test.ts
git commit -m "test(catalog): the vendored wasm binary IS the contract fixture

Instantiates public/hornvale_world.wasm, drives hw_new(42)+scene calls, and
asserts the strict parser accepts the real tiles+system documents. No
committed JSON copy to drift. The Isotherm Task 8.

https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

### Task 9: The seasonal-ice globe overlay (the tracer feature)

**Files:**
- Modify: `src/views/globe.ts` (add an ice tint pass driven by the sim clock's day)
- Create: `src/views/ice.ts`, `src/views/ice.test.ts`
- Possibly modify: `src/ui/hud.ts` (caption discipline: name what the overlay shows)

**Interfaces:**
- Consumes: `temperatureAt`, `coldestC` (Task 7); the sim clock's current `day`; `TilesScene` layers; the globe's per-tile sampling (`sampleTile`).
- Produces: `iceFraction(tiles, i, day, freezeC = 0): number` — 1 where frozen, 0 where not, a soft ramp near the threshold — and its wiring into the globe material.

- [ ] **Step 1: Write failing unit tests**

`src/views/ice.test.ts`:

```ts
import { expect, test } from "vitest";
import { iceFraction } from "./ice";
import type { TilesScene } from "../sim/scene";

function tile(mean: number, swing: number): TilesScene {
  return { t_mean_c: [mean], t_swing_c: [swing], season_period_days: 360 } as unknown as TilesScene;
}

test("a permanently cold tile is frozen year-round", () => {
  const t = tile(-20, 5);
  for (const day of [0, 90, 180, 270]) expect(iceFraction(t, 0, day)).toBe(1);
});

test("a warm tile never freezes", () => {
  const t = tile(25, 5);
  for (const day of [0, 90, 180, 270]) expect(iceFraction(t, 0, day)).toBe(0);
});

test("a seasonal tile freezes in its cold half and thaws in its warm half", () => {
  const t = tile(0, 15); // north tile: cold near day 270 (sin < 0), warm near day 90
  expect(iceFraction(t, 0, 270)).toBeGreaterThan(0.5);
  expect(iceFraction(t, 0, 90)).toBeLessThan(0.5);
});

test("locked tile (zero swing) is static — frozen iff mean below freeze", () => {
  expect(iceFraction(tile(-3, 0), 0, 0)).toBe(1);
  expect(iceFraction(tile(3, 0), 0, 999)).toBe(0);
});
```

- [ ] **Step 2: Run to verify failure**

Run: `npm test -- ice`
Expected: FAIL — no `./ice`.

- [ ] **Step 3: Implement `src/views/ice.ts`**

```ts
/** Client-side seasonal ice from scene/tiles/v1 temperature layers — the
 * derivation documented (non-normatively) in the book's tiles reference.
 * Presentation only; the sim has no cryosphere (decision 0022). */
import type { TilesScene } from "../sim/scene";
import { temperatureAt } from "../sim/climate";

/** Frozen fraction [0,1] at tile `i` on `day`: 1 below freeze, 0 above, a
 * soft 2 °C ramp so the ice edge is not a hard line. */
export function iceFraction(tiles: TilesScene, i: number, day: number, freezeC = 0): number {
  const t = temperatureAt(tiles, i, day);
  const ramp = 2;
  if (t <= freezeC - ramp) return 1;
  if (t >= freezeC) return 0;
  return (freezeC - t) / ramp;
}
```

- [ ] **Step 4: Run to verify pass**

Run: `npm test -- ice`
Expected: PASS.

- [ ] **Step 5: Wire it into the globe**

In `src/views/globe.ts`, blend an ice color (near-white) into each tile's rendered color by `iceFraction(tiles, i, currentDay)`, recomputed as the sim clock advances (the globe already re-renders per clock tick for the terminator — hook the same update). Keep the honest terminator working: ice on the night side still goes dark (apply the ice tint to the base color *before* the day/night lighting, not after). Add a HUD caption line naming the overlay ("sea ice / snow where the season's temperature drops below freezing — a client derivation, not simulated ice") per the orrery's caption discipline.

- [ ] **Step 6: Verify in the running app + smoke + e2e**

```bash
npm run smoke        # vendored binary still drives
npm test             # full vitest suite green
npm run build        # tsc typecheck + vite build
npm run e2e          # Playwright smoke stays green
```
Expected: all green. Then run `npm run dev` and confirm visually: the globe shows polar/seasonal ice; advancing the clock moves the ice edge; hemispheres are opposite-phased; a locked world (pin `rotation=locked` via the seed/pin UI if available, else skip) shows a static night-side sheet.

- [ ] **Step 7: commit**

```bash
git add src/views/ice.ts src/views/ice.test.ts src/views/globe.ts src/ui/hud.ts
git commit -m "feat(globe): seasonal ice overlay from the tiles temperature layers

Whitens tiles whose evaluated season temperature drops below freezing,
advancing with the sim clock; captioned as a client derivation. Closes the
consumer half of hornvale#2. The Isotherm Task 9.

https://claude.ai/code/session_01QTC8nduDfTjoYMwfwJGcyw"
```

---

## Self-review coverage map

- Spec §3 (5 fields, wire order, quantization) → Tasks 2, 3.
- Spec §3.1 (temperature evaluator, exact-vs-quantized) → Tasks 1 (Rust exact), 4 (docs), 7 (client).
- Spec §3.2 (wind evaluator) → Tasks 3 (Rust restatement), 4 (docs), 7 (`windAt`).
- Spec §3.3 (ice guidance) → Tasks 4 (docs), 9 (overlay).
- Spec §4 (producer impl) → Tasks 1, 2.
- Spec §5 (docs, new system page) → Task 4.
- Spec §6 (consumer impl) → Tasks 5, 6, 7, 9.
- Spec §7 (contract tests both sides) → Tasks 1, 3 (producer), 7, 8 (consumer).
- Spec §8 (goldens/gallery rebaseline, no epoch/census) → Task 3.
- Spec §5b/self-contained `season_period_days` → Tasks 2, 7.
- Deferred (§9) — no tasks, by design.
- G6 external actions (world-wasm-v2 tag/release, orrery re-pin already staged in Task 5, ticket closes) → campaign close, Nathan-authorized.

## Close (after all tasks green, at G6)

Per `closing-a-campaign`: chronicle entry, retrospective (fold in `.superpowers/sdd/followups.md`), book freshness sweep, registry flips (RENDER-8 / the scene rows if any move), ledger digest to Nathan. External at G6 with authorization: push both branches, cut `world-wasm-v2` tag + release asset, close hornvale#2, comment hv#5 (what shipped / what trails), recommend closing orrery#1.
