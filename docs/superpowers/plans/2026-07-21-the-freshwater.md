# The Freshwater Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Surface the sim's inland water (`WaterKind`/drainage/waterfalls) into the scene and render it — full detail on the flat map, major features on the clean globe.

**Architecture:** Append `water`(index)+`water_legend`+`drainage`+`waterfalls` to `scene/tiles/v1` (`TilesScene`) and `scene/tiles-region/v1` (`RegionScene`), populated from the terrain provider (a pure read — no new draw, no re-sculpt). Release the world-wasm. Orrery: colour river/lake cells into the map pixel texture + waterfall icons; tint major water on the globe.

**Tech Stack:** Rust (`windows/scene`, `domains/terrain`), TypeScript/three.js (Orrery), the world-wasm release path.

## Global Constraints

- **Additive schema append, no epoch.** Both scene schemas carry the stability contract; new fields appended, field order preserved, existing consumers unaffected.
- **Determinism / byte-identity:** water is a pure read of the built terrain provider — no new seed draw, no stream-order change, no re-sculpt (thread the provided value; Single-Sculpt idiom). `water`/`water_legend` are integers/strings (exact); `drainage` + waterfall lat/lon are `f64`, quantized at emit via `hornvale_kernel::quantize::quantize_serde` (the existing scene idiom).
- **Every commit passes the gate** (`make gate`); scene-fixture drift is regen'd in the producer commit and the whole gate re-run (the Residue lesson).
- Rust: `#![warn(missing_docs)]`, type-audit verdict tag on every new pub-boundary primitive, `cargo fmt` last. Sonnet floor for subagents.
- **The wasm release (Stage 2) is a carve-out** — Nathan authorizes; client stages block on it.

---

## STAGE 1 — The producer field

### Task 1: A stable `WaterKind` index + legend (shared)

**Files:** Modify `domains/terrain/src/water.rs`; test in the same file.

**Interfaces:**
- Produces: `WaterKind::index(self) -> u8` (Ocean=0, SaltBasin=1, River=2, DryLand=3 — declaration order, stable), `WaterKind::name(self) -> &'static str` (`"ocean"`/`"salt-basin"`/`"river"`/`"dry-land"`), and `WaterKind::LEGEND: [&'static str; 4]` (names in index order). These are the one true water-name source; `windows/locale`'s private `water_kind_name` can later delegate here (not required this task).

- [ ] **Step 1: Failing test** — in `water.rs` tests: assert `WaterKind::River.index() == 2`, `WaterKind::River.name() == "river"`, `WaterKind::LEGEND[WaterKind::SaltBasin.index() as usize] == "salt-basin"`, and that all four indices are `0..4` and unique.
- [ ] **Step 2: Run, expect FAIL** — `cargo test -p hornvale-terrain water::`.
- [ ] **Step 3: Implement** the `index`/`name`/`LEGEND` on `WaterKind` (a `match`, no `as u8` reliance — explicit and stable). Doc-comment each; type-audit tag on `index`'s `u8` return.
- [ ] **Step 4: Run, expect PASS.**
- [ ] **Step 5: Commit** `feat(terrain): stable WaterKind index + name + legend`.

### Task 2: Water on `scene/tiles/v1` (`TilesScene`)

**Files:** Modify `windows/scene/src/lib.rs`; tests in the same file (mirror the existing tiles-scene tests).

**Interfaces:**
- Consumes: Task 1's `WaterKind::{index,LEGEND}`; provider `water_kind_at(CellId)`, `drainage_at(CellId)`, `waterfalls() -> &[CellId]`.
- Produces: `TilesScene` gains, APPENDED after the current last field (preserve order): `water: Vec<u8>`, `water_legend: Vec<String>`, `drainage: Vec<f64>` (quantized via `vec_f64_field`), `waterfalls: Vec<WaterfallPoint>` where `WaterfallPoint { latitude: f64, longitude: f64 }` (both quantized). Type-audit tags appended to the struct's `type-audit:` comment.

- [ ] **Step 1: Failing test** — extend the tiles-scene test: for seed 42 (or the test's seed), assert `scene.water.len() == tiles`, `scene.water_legend == ["ocean","salt-basin","river","dry-land"]`, `scene.drainage.len() == tiles`, at least one tile has `water == River.index()` (rivers exist — ~6.7% of land), ocean tiles have `drainage == 0.0`, and `scene.waterfalls` is non-empty (seed-42 has waterfalls). Assert the JSON serializes `water_legend` and `drainage` present.
- [ ] **Step 2: Run, expect FAIL** — `cargo test -p hornvale-scene`.
- [ ] **Step 3: Implement.** Add the fields to `TilesScene` (appended, with `#![warn(missing_docs)]` doc comments + the `vec_f64_field` quantize serde on `drainage`, `f64_field` on the waterfall coords). In the build loop (after `ocean.push(...)`), push `water.push(terrain.water_kind_at(t_cell).index())` and `drainage.push(terrain.drainage_at(t_cell))`. After the loop, build `waterfalls` by mapping `terrain.waterfalls()` (each `CellId`) to its lat/lon (reuse the geosphere→lat/lon the loop uses, inverted — or the provider's cell centroid; find the existing cell→lat/lon path). Set `water_legend: WaterKind::LEGEND.iter().map(|s| s.to_string()).collect()`. Populate in the `TilesScene { … }` literal. Extend the struct's `type-audit:` verdict comment with the new fields (`bare-ok(index: water)`, `bare-ok(identifier-text: water_legend)`, `bare-ok(diagnostic-value: drainage)`, waterfall coords per the lat/lon convention already used by `features`).
- [ ] **Step 4: Run, expect PASS + `cargo fmt` + `cargo clippy -p hornvale-scene`.**
- [ ] **Step 5: Commit** `feat(scene): water (kind+legend), drainage, waterfalls on scene/tiles/v1`.

### Task 3: Water on `scene/tiles-region/v1` (`RegionScene`)

**Files:** Modify `windows/scene/src/region.rs`; tests in the same file.

**Interfaces:**
- Same fields as Task 2, appended to `RegionScene` (struct L194) and populated in its builder (L243). `waterfalls` on a region = only those whose cell falls inside this tile's footprint.

- [ ] **Step 1: Failing test** — extend the region test (`assert_eq!(scene.schema, "scene/tiles-region/v1")` context): a region over land carries `water`/`drainage` of node length, `water_legend` the 4 names; a river-bearing region has at least one `River` node.
- [ ] **Step 2: Run, expect FAIL** — `cargo test -p hornvale-scene region::`.
- [ ] **Step 3: Implement** — mirror Task 2 in the region builder (per-node `water_kind_at`/`drainage_at`; `waterfalls` filtered to the region footprint — the builder already knows the tile's cell set / bounds). Appended fields, quantize serde, type-audit tags, docs.
- [ ] **Step 4: Run, expect PASS + fmt + clippy.**
- [ ] **Step 5: Commit** `feat(scene): water/drainage/waterfalls on scene/tiles-region/v1`.

### Task 4: Regenerate drift-checked artifacts + scene-schema docs + full gate

**Files:** the committed scene artifacts (almanacs/maps/reference pages), `book/src/reference/scene-tiles-v1.md`, `book/src/reference/scene-tiles-region-v1.md`.

- [ ] **Step 1:** Run the artifact regen the CI drift-check enforces (the "Artifacts are current" step: `cargo run -p hornvale -- new/almanac/lab`, and the scene-schema reference pages). `git diff` — the new water fields appear in every scene artifact; commit the regen.
- [ ] **Step 2:** Update the two scene-schema reference chapters (`scene-tiles-v1.md`, `scene-tiles-region-v1.md`) to document `water`/`water_legend`/`drainage`/`waterfalls` (the field table + the example). These are hand-written docs, not generated — edit them.
- [ ] **Step 3:** `make gate` (fmt + clippy + nextest + doctests) — full green. If `stream_labels()`/the stream manifest changed (it should NOT — pure read, no new draw), regen the manifest too and re-gate (the Few-and-Many gap).
- [ ] **Step 4: Commit** `build(scene): regen artifacts + scene-schema docs for the water fields`.

**Stage 1 deliverable:** the scene carries water, drift-checked, gate green. No client yet.

---

## STAGE 2 — The wasm release (carve-out)

### Task 5: Release the world-wasm + re-pin the Orrery

**Controller + Nathan, not a subagent.** The world-wasm (`clients/world-wasm`) serializes the scene structs, so it carries water once Stage 1 merges. Follow the established release path (as The Real Sky / prior wasm releases did):

- [ ] **Step 1:** Merge Stage 1 to hornvale main (its own close is folded into the campaign close, or a mid-campaign merge if the release needs a tagged main).
- [ ] **Step 2:** Build the world-wasm release artifact from main; **Nathan authorizes the release** (externally visible).
- [ ] **Step 3:** Re-pin the Orrery to the new wasm (the `public/hornvale_world.wasm` + any version catalog / release pin the orrery uses).
- [ ] **Step 4:** Confirm the orrery's fetched scenes carry `water`/`drainage`/`waterfalls` (a quick console/log check or a scene-shape test).

**Stage 2 deliverable:** the Orrery's scenes carry water. Client stages unblock.

---

## STAGE 3 — The map (full water)

### Task 6: Scene TS types + map pixel-texture water colouring + waterfall icons

**Files (Orrery):** `src/sim/scene.ts` (types), `src/views/mapTexture.ts` (colour), `src/views/symbols/sprites.ts` + `src/views/mapSymbols.ts` (waterfall icon).

- [ ] **Step 1:** Extend `TilesScene` + `RegionScene` TS interfaces with `water: number[]`, `waterLegend: string[]` (read by the wire name `water_legend`), `drainage: number[]`, `waterfalls: { latitude: number; longitude: number }[]`. Update any scene-validation.
- [ ] **Step 2 (test):** in `mapTexture.test.ts`, a region with a `River` node (water index for "river" per its `water_legend`) → that node's RGBA is a distinct flowing-blue (not the deep-ocean tone, not land); a `SaltBasin` node → a distinct lake tone. Add the water branch to `regionPixelRGBA`/`pixelColorFor` (river/salt-basin water classes override the biome/land colour; keyed to `drainage` for a subtle intensity). Run → FAIL then PASS.
- [ ] **Step 3:** Waterfall icons — add `buildWaterfallMaterial()` to `sprites.ts` (a cyan cascade glyph, jsdom fallback) and place waterfall sprites in `mapSymbols` from `region.waterfalls` (map lat/lon → the region's grid → quad position; tag `userData.kind='icon'`, `userData.icon='waterfall'`). Test: a region with a waterfall places a waterfall icon at near.
- [ ] **Step 4:** `npx tsc --noEmit`, `npm run build`, `npm test` green.
- [ ] **Step 5: Commit** `feat(cartographer): freshwater on the flat map — river/lake pixels + waterfall icons`.
- [ ] **Step 6 (controller): visual pass** — zoom into a river/lake region on seed 42; confirm rivers read as flowing water, lakes distinct from ocean, waterfalls placed. Tune the water palette + drainage intensity.

**Stage 3 deliverable:** the flat map shows the full river network, lakes, and waterfalls.

---

## STAGE 4 — The globe (major water)

### Task 7: Thresholded major-water tint on the globe

**Files (Orrery):** `src/views/globe.ts` (or a small `src/views/water.ts` overlay), `src/views/lens.ts` if the natural lens gains a water contribution.

- [ ] **Step 1:** In the globe base colour path, tint cells that are `River` with `drainage >= RIVER_GLOBE_THRESHOLD` a thin blue, and `SaltBasin` cells belonging to a lake of size `>= LAKE_GLOBE_MIN` a lake tint. Thresholds are named consts (visual-pass-tuned). Keep it a pure colour contribution over the existing biome/ocean base so the smooth overview survives; if per-vertex tint reads too chunky, a thin line/patch overlay mesh is the fallback (visual-pass decision).
- [ ] **Step 2 (test):** the water-tint colour helper is pure — a `River` cell above threshold → blue-dominant; below threshold or `DryLand` → unchanged base. Unit-test that.
- [ ] **Step 3:** `tsc`/`build`/`npm test` green.
- [ ] **Step 4: Commit** `feat(cartographer): major rivers + lakes on the globe overview (thresholded)`.
- [ ] **Step 5 (controller): visual pass** — the globe shows the largest rivers + biggest lakes without cluttering; tune thresholds with Nathan.

**Stage 4 deliverable:** the clean globe shows only the massive water features.

---

## After Stage 4
Whole-branch reviews (hornvale producer diff; orrery client diff), then **G6** — chronicle, retrospective, the MAP registry row (id computed at close), scene-schema/terrain chapter freshness, memory; merge hornvale + orrery, and the wasm release/deploy.
