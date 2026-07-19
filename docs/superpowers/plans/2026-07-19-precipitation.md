# The Rains (Precipitation) Implementation Plan

> **Status: DRAFT.** REQUIRED SUB-SKILL: superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Replace the single-pass rain-shadow with an advected upwind moisture-budget trace (honest rain shadows + continental drying), add real mm/yr precipitation, a rain/snow split, a seasonal regime, and a diagnostic cloud field — the whole rendered on the Living Globe.

**Architecture:** The moisture generator in `domains/climate/moisture.rs` changes (a bounded upwind budget trace) — a **full derived-field EPOCH**: new moisture → new biomes → settlement/species/culture cascade. Precipitation (`mm/yr`, snow fraction, regime) and clouds are additive derived fields on the provider + scene. The orrery adds a precip lens + a clouds advection overlay.

**Tech Stack:** Rust (edition 2024, `serde`+`serde_json`, `kernel::math` for trig), TypeScript/vite/three.js (orrery), cargo-nextest.

## Global Constraints

- **This is a FULL EPOCH.** Changing moisture cascades through biomes → the whole society. Every seed re-derives. The census (`book/src/laboratory/generated/*/rows.csv`) refreshes ONLY at the AWS regen at close (never locally — SKIP_CENSUS is the local standard). `make gate` does NOT run censuses, so it stays green once fixtures are refrozen.
- **Refreeze in the drifting commit, not at close** (re-pin discipline): the epoch commit (Task 2) refreezes `cli/tests/fixtures/world-seed-42.json`, the scene byte-pin, and the gallery artifacts. A SECOND keystone refreeze from main's tip happens at close (to absorb main's deltas).
- **No new stream draws, no reordered labels** — moisture-transport is a pure derived read (winds/terrain/ocean). Determinism unchanged; same seed → same moisture.
- **Spinning worlds only** for the transport; **locked worlds keep their substellar moisture** (the trace needs band winds).
- **Biomes keep classifying on the moisture `[0,1]`** (now transport-derived) — no biome-classifier rewrite.
- **All trig via `hornvale_kernel::math`**; `.sqrt()`/`.min`/`.max`/`.clamp` on f64 are fine. **Quantize at emit** (new scene floats: `vec_f64_field`). **No HashMap/HashSet**; `#![warn(missing_docs)]`; type-audit tags on new pub items; `cargo fmt` before every commit; run `type-audit -- check` (exit 0) + regenerate its report when pub items change.
- **No golden** for the client (it parses + advects; the vendored-binary fixture test is the contract).
- **Non-goals:** no cloud→insolation feedback, no `CLIM-operators` solver, no lake evaporation (no lake surface), no Köppen biome rewrite, no cryosphere, no seasonal wind reversal.

---

### Task 1: kernel `Precipitation` newtype (mm/yr)

**Files:** Modify `kernel/src/units.rs` (+ its re-export in `kernel/src/lib.rs` if the units are re-exported).

**Interfaces:**
- Produces: `pub struct Precipitation(f64)` with `pub fn new(mm_per_year: f64) -> Result<Self, UnitError>` (reject non-finite or negative) and `pub fn get(&self) -> f64` — mirroring `ReferenceElevation`/`Temperature`.

- [ ] **Step 1: Failing test** in `units.rs` tests: `Precipitation::new(1200.0).unwrap().get() == 1200.0`; `Precipitation::new(-1.0).is_err()`; `Precipitation::new(f64::NAN).is_err()`.
- [ ] **Step 2: Red** — `cargo test -p hornvale-kernel precip`.
- [ ] **Step 3: Implement** `Precipitation` mirroring `ReferenceElevation` (validating constructor, `get`, doc comments, `#[derive(Debug, Clone, Copy, PartialEq)]`, type-audit tag `bare-ok(...)` matching the units style).
- [ ] **Step 4: Green**; **Step 5: fmt + commit** `feat(the-rains): kernel Precipitation newtype (mm/yr)`.

---

### Task 2: the moisture-budget trace — THE EPOCH

**Files:** Modify `domains/climate/src/moisture.rs`; refreeze `cli/tests/fixtures/world-seed-42.json`, `windows/scene/tests/fixtures/tiles-seed-1-w16.json`, and `book/src/gallery/*` (the cascade).

**Interfaces:**
- `moisture_field(geo, elevation, sea_level, regime) -> CellMap<f64>` keeps its signature; its **spinning branch** swaps `rain_shadow` for the budget trace. Locked branch unchanged.

**The model (replace the spinning branch's `base + ocean_bonus - 0.5*rain_shadow` with a budget trace):**
```
// moisture_budget(geo, elevation, sea_level, cell, wind, bands) -> f64  (normalized [0,1]):
//   1. Collect the upwind PATH from `cell`: path[0]=cell, then up to BUDGET_STEPS
//      hops to the most-upwind neighbor (reuse rain_shadow's upwind-neighbor pick:
//      the neighbor whose displacement is most opposite `wind`). Stop early on a
//      dead-end / self-loop.
//   2. Replay from the FARTHEST upwind end toward `cell`, carrying W (precipitable water):
//        W = 0
//        for i from path.len()-1 down to 0:
//          let c = path[i];
//          if is_ocean(c) {                       // ocean source
//            W = (W + EVAP * warmth(geo, c)).min(W_CAP);   // warmth = cos(latitude) clamped [0,1]
//          } else {                               // land: lose water
//            let prev = path[(i+1).min(path.len()-1)];     // the cell one step upwind
//            let uplift = (elevation(c) - elevation(prev)).max(0.0);  // rose along the wind
//            W = (W - OROG_K * (uplift / UPLIFT_SCALE_M)).max(0.0);    // orographic rainout
//            if is_rising_band(band_index(lat(c), bands)) { W = (W - CONVECTIVE).max(0.0); }
//            W *= 1.0 - DECAY;                     // per-overland-step distance decay
//          }
//   3. precipitation at cell = W * local_rainout(cell), where local_rainout =
//        BASE_RAINOUT + (rising-band ? CONVECTIVE_LOCAL : 0) + OROG_LOCAL * (uplift into cell)
//   4. return (precipitation / PRECIP_NORM).clamp(0.0, 1.0)   // → the [0,1] moisture
// constants (starting values, census/visual-tuned): BUDGET_STEPS=16, EVAP=0.5, W_CAP=1.0,
//   OROG_K=1.5, UPLIFT_SCALE_M=3000.0, CONVECTIVE=0.05, DECAY=0.06, BASE_RAINOUT=0.5,
//   CONVECTIVE_LOCAL=0.3, OROG_LOCAL=0.4, PRECIP_NORM=1.0. Ocean cells keep moisture≈1
//   (they are their own source). Keep the existing `ocean_bonus` OR fold it into the source.
// `warmth(geo, c)` = math::cos(latitude.to_radians()).clamp(0,1). All trig via kernel::math.
```

- [ ] **Step 1: Failing property tests** in `moisture.rs` tests (mutation-verified — the physics leads):

```rust
// A mountain range casts a rain shadow: the leeward cell is drier than the
// windward cell of the same range. Build a small world with a ridge across the
// prevailing wind; assert moisture(leeward) < moisture(windward). (Mutation:
// remove the orographic sink → this fails.)
#[test]
fn a_range_casts_a_rain_shadow() { /* mirror the existing moisture test's world build;
    find a windward/leeward pair across an elevation ridge; assert leeward drier */ }

// Continental interiors dry with distance from the sea: along a downwind
// overland transect, moisture decreases monotonically (allowing small noise).
#[test]
fn interiors_dry_with_distance_from_sea() { /* trace a downwind land transect from
    coast inland; assert the far-inland cell is drier than the near-coast cell */ }

// Ocean-source dependence: a land cell with NO ocean within BUDGET_STEPS upwind
// is dry (little precipitable water reaches it).
#[test]
fn a_cell_with_no_upwind_ocean_is_dry() { /* construct/find such a cell; assert low moisture */ }

// Bounded and locked unchanged.
#[test]
fn moisture_is_bounded_and_locked_is_substellar() { /* [0,1]; locked branch identical to before */ }
```

- [ ] **Step 2: Red** — `cargo test -p hornvale-climate moisture`.
- [ ] **Step 3: Implement** the budget trace per the model block (a `moisture_budget` fn generalizing `rain_shadow`; keep `rain_shadow` only if still used, else replace). Spinning branch calls it; locked branch untouched. Doc comments; type-audit tag on any new pub item.
- [ ] **Step 4: Green** — `cargo test -p hornvale-climate moisture` (the 4 tests). Mutation-verify `a_range_casts_a_rain_shadow` (remove the orographic sink → red; restore).
- [ ] **Step 5: THE EPOCH REFREEZE (this is expected drift, not a bug).** The frozen world + scene + gallery now differ because moisture → biomes → society changed. Refreeze them IN THIS COMMIT:
  - Regenerate the frozen world fixture: `cargo run -p hornvale -- new --seed 42 --out cli/tests/fixtures/world-seed-42.json` (confirm the path via `cli/tests/lens_purity.rs`'s constant if it differs).
  - Re-pin the scene byte fixture: `REBASELINE=1 cargo test -p hornvale-scene --test golden`.
  - Regenerate all committed artifacts: `bash scripts/regenerate-artifacts.sh` (this SKIPS censuses — correct; the census rows.csv LAG to the AWS regen at close). `git add` every changed `book/src/gallery/*`, `book/src/reference/*`, `cli/tests/fixtures/*`, `windows/scene/tests/fixtures/*`.
  - **Do NOT** run `make rebaseline`/censuses/`HV_CENSUS=1` — census regen is AWS-only, at close, Nathan-authorized.
- [ ] **Step 6: FULL gate green after refreeze** — `cargo nextest run --workspace` (timeout 3600000), `cargo test --workspace --doc`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`, `type-audit -- check` (exit 0). Confirm `git status` is clean (all drift refrozen). If any test still reddens, it is either (a) a fixture you missed regenerating, or (b) a real cascade bug — investigate, don't paper over.
- [ ] **Step 7: Commit** — `feat(the-rains): moisture-budget trace — advected precipitable water (EPOCH: moisture→biomes→society)`. The commit body must note it is the epoch and that census rows.csv lag to the AWS regen.

---

### Task 3: precipitation units — mm/yr + snow fraction + regime (provider)

**Files:** Modify `domains/climate/src/provider.rs`; add a small `domains/climate/src/precipitation.rs` for the pure derivations.

**Interfaces:**
- Consumes: Task 1's `Precipitation`; the moisture field (Task 2); `mean_temp` (already computed first in `generate()`); `band_count`.
- Produces on the climate field object: `precip_at(cell) -> Precipitation`, `snow_fraction_at(cell) -> f64` (`[0,1]`), `regime_at(cell) -> PrecipRegime` (an enum `Uniform|SummerMax|WinterMax|Monsoon`).

- [ ] **Step 1: Failing tests** (in `precipitation.rs`): `precip_mm_yr(moisture)` maps `moisture` monotonically into an Earth range (moisture 0 → near 0 mm; moisture 1 → > 1500 mm); `snow_fraction(mean_temp_c)` is ~1 well below 0°C, ~0 well above, smooth across 0; `precip_regime(band, continentality, hemisphere)` returns `Uniform` for the equatorial rising belt and (e.g.) `Monsoon` for a high-continentality mid-latitude cell. Keep them PURE (take scalars).
- [ ] **Step 2: Red** — `cargo test -p hornvale-climate precip`.
- [ ] **Step 3: Implement** the pure fns in `precipitation.rs` (`pub fn precip_mm_yr(moisture: f64) -> Precipitation`, `pub fn snow_fraction(mean_temp_c: f64) -> f64`, `pub fn precip_regime(...) -> PrecipRegime`; doc comments; type-audit tags). In `provider.rs`, precompute `precip: CellMap<Precipitation>`, `snow_fraction: CellMap<f64>`, `regime: CellMap<PrecipRegime>` in `generate()` (after moisture + mean_temp) and add the three accessors, mirroring `moisture_at`/`diurnal_amp_at`.
- [ ] **Step 4: Green**; **Step 5: fmt + commit** `feat(the-rains): precipitation mm/yr + snow fraction + seasonal regime`.

---

### Task 4: diagnostic clouds (`CLIM-clouds`)

**Files:** Modify `domains/climate/src/precipitation.rs` (or a `clouds.rs`) + `provider.rs`.

**Interfaces:**
- Produces: `cloud_fraction_at(cell) -> f64` (`[0,1]`) on the climate field object.

- [ ] **Step 1: Failing test**: `cloud_fraction(moisture, uplift, rising_band)` is high for moist + rising/uplifted, low for dry + sinking — monotone in moisture and in uplift; bounded `[0,1]`.
- [ ] **Step 2: Red**; **Step 3: Implement** `pub fn cloud_fraction(moisture: f64, uplift_m: f64, rising_band: bool) -> f64` (e.g. `(moisture * (BASE + RISING*rising + UPLIFT_K*uplift/scale)).clamp(0,1)`), precompute `cloud_fraction: CellMap<f64>` in the provider + `cloud_fraction_at`. Diagnostic only — it feeds NOTHING (no insolation change).
- [ ] **Step 4: Green**; **Step 5: fmt + commit** `feat(the-rains): diagnostic cloud-fraction field`.

---

### Task 5: scene fields + re-pin + almanac + `CLIM-precip-phase` registry row

**Files:** Modify `windows/scene/src/lib.rs`; re-pin `tiles-seed-1-w16.json`; almanac window; `book/src/frontier/idea-registry.md`.

**Interfaces:**
- Produces: `TilesScene.precip_mm_yr: Vec<f64>`, `snow_fraction: Vec<f64>`, `precip_regime: Vec<u8>` (the enum as a small index), `cloud_fraction: Vec<f64>` — additive, quantized (the float ones), zero-safe.

- [ ] **Step 1: Failing test** (scene lib tests): the four `.len() == tiles`; floats finite + in range; `precip_regime` values are valid enum indices.
- [ ] **Step 2: Red** — `cargo test -p hornvale-scene`.
- [ ] **Step 3: Implement** the four fields the `t_diurnal_amp_c`/`current_east` way (serde quantize attr on the floats — `precip_regime` is a `Vec<u8>`, no quantize; one-line docs; append the type-audit tags; `Vec::with_capacity`; per-tile push via `climate.precip_at(c).get()` / `snow_fraction_at` / `regime_at as u8` / `cloud_fraction_at`; finite-check chain for the floats; struct literal).
- [ ] **Step 4: Re-pin** — `REBASELINE=1 cargo test -p hornvale-scene --test golden`; inspect the diff — ONLY the four new arrays added over the Task-2 baseline (every prior field byte-identical); re-run → PASS.
- [ ] **Step 5: Almanac "the rains" line** — a per-sample-site precip readout (annual mm, "rain"/"snow", the regime word) in the climate/land section (near the diurnal/seas lines); a unit test asserts it renders (present on a spinning world).
- [ ] **Step 6: `CLIM-precip-phase` registry row** — add ONE row to `book/src/frontier/idea-registry.md` in the CLIM cluster (near `CLIM-precip-units`): `| CLIM-precip-phase | Hydrometeor phase — sleet, freezing rain, ice pellets, hail — classified from the vertical temperature profile (a melting layer aloft over a subfreezing surface). Sleet/freezing-rain are a level-0 diagnostic over a 2-layer temp column (a later refinement of rain/snow); hail is convective → the drawn-storms campaign | raw | med | [The Rains spec](...precipitation-design.md) §7 |`. Run `cargo test -p hornvale --test docs_consistency` (the frontier drift-check).
- [ ] **Step 7: Regenerate artifacts** — `bash scripts/regenerate-artifacts.sh`; `git add` changed `book/src/gallery/*`; `git status` clean.
- [ ] **Step 8: FULL gate + type-audit** green; **commit** `feat(the-rains): scene precip/snow/regime/cloud fields + almanac + CLIM-precip-phase (re-pin)`.

---

### Task 6: world-wasm binary handoff (no golden)

- [ ] Build `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release`; copy `…/hornvale_world_wasm.wasm` → `/Users/nathan/.config/superpowers/worktrees/orrery/the-rains/public/hornvale_world.wasm` (mkdir `public/`). Confirm `ls -la` (today's date) + `strings … | grep -c precip_mm_yr` ≥ 1. No hornvale commit (build artifact). If the orrery worktree is absent, STOP and tell the controller.

---

### Task 7: orrery — parse + precip lens + clouds advection overlay

**Files (orrery worktree `~/.config/superpowers/worktrees/orrery/the-rains`):**
- Modify `src/sim/scene.ts` (parse `precipMmYr`/`snowFraction`/`precipRegime`/`cloudFraction`), `src/views/lens.ts` (a precipitation lens), `src/ui/hud.ts` (a clouds toggle); Create `src/views/clouds.ts` (+ test) — the clouds advection overlay, sibling to `src/views/currents.ts`.
- Modify `src/sim/scene.test.ts`, `src/sim/catalogFixture.test.ts`.

**Interfaces:**
- Consumes: the Task-6 binary (verify `ls -la` is today; else STOP). Produces: `parseTiles` gains the four fields; a `createClouds(tiles, radius)` overlay (advect cloud particles/veil along the winds over high-cloud-fraction cells — MIRROR `currents.ts`'s `stepParticle`/`update(day)` seed/fade/re-seed; non-deterministic eyecandy, decision 0022); a precipitation lens (mm/yr arid→wet).

- [ ] **Step 1: Failing tests.** `scene.test.ts`: the four fields round-trip (`precipRegime` is an int array; the floats strict length + quantized-number). `clouds.test.ts`: the pure advection-step geometry (mirror `currents.test.ts` — moves along the wind tangent, stays on the sphere, fades, re-seeds; reborn full opacity). A precip-lens colorAt unit test: higher mm/yr → wetter color.
- [ ] **Step 2: Red** — `npx vitest run src/sim/scene.test.ts src/views/clouds.test.ts`.
- [ ] **Step 3: Implement** the parser fields; the precip lens (register it in the lens roster the way the existing lenses are); `createClouds` (study `src/views/currents.ts` + `src/views/winds.ts`; clouds seed over cells with `cloudFraction` above a threshold; advect along the wind field the client already reconstructs from `circulationBands`); the HUD `clouds` toggle; mount/gate in `globe.ts` beside the currents overlay; `null` on locked worlds.
- [ ] **Step 4: Green** — `npm test` + `npx tsc --noEmit` (whole suite; the epoch'd binary feeds every fixture test — a red elsewhere is a real regression from the new biomes, expected in fixtures that hard-code biome/moisture values; update those to the new values, non-vacuously).
- [ ] **Step 5: Commit** — `feat(the-rains): parse precip/clouds + precipitation lens + clouds advection`.

---

### Task 8: assembly + visual verification + G6 (controller-run)

- [ ] Hornvale: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean; `mdbook build book`. Census rows.csv LAG (AWS regen is a G6 carve-out).
- [ ] Orrery: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e — green with the Task-6 binary.
- [ ] **Visual verification (The Lens rule — it has caught a physics bug AND a legibility bug in this program):** serve, seed 42, the **precipitation lens** — confirm real **rain shadows** (dry leeward interiors, wet windward coasts), the wet tropical belt, dry horse latitudes; toggle **clouds** — confirm they gather over the wet rising belts and advect; confirm **snow** caps the poles; and confirm the **biomes have visibly rearranged** vs the old world (the epoch, made legible). Open the PNGs.
- [ ] Absorb main on both repos (regenerate `type-audit-report.md` on conflict; hand-resolve; **at close, refreeze the seed-42 fixture AGAIN from main's tip**); conflict sweep; followups; G6 package (epoch/census leads; **AWS census regen** + The Confluence sequencing + world-wasm-v10 + orrery re-pin + pushes for Nathan's carve-out approvals).
