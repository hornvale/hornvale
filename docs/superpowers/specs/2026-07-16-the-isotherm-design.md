# The Isotherm — Design

**Date:** 2026-07-16
**Status:** Draft (autopilot G3 pending)
**Tickets:** [hornvale#2](https://github.com/hornvale/hornvale/issues/2) (seasonal ice), the rainfall/temperature/winds core of [hornvale#5](https://github.com/hornvale/hornvale/issues/5); consumer proof in hornvale/orrery.
**Parent contracts:** `2026-07-09-scene-protocol-design.md` (schema discipline), `2026-07-14-goldengrove-design.md` §§3, 5 + decision 0055 (the wasm catalog and the cross-repo contract boundary), decision 0022 (sim emits data, clients render).
**Provenance:** Nathan's standing priority for this arc: *"the more explicit and precise we can make these API contracts, the better; really well-documented APIs and contract tests are my main priority."* This campaign extends `scene/tiles/v1` with the climate layers the orrery needs, and hardens the cross-repo contract discipline every later ticket will flow through.

---

## 1. Goal

Additive climate elements in `scene/tiles/v1` — per-tile temperature (annual mean + hemisphere-signed seasonal swing) and moisture, plus document-level season-period and circulation-band elements — each specified to the digit in the book's schema pages, with documented client-side evaluators (seasonal temperature, prevailing wind, freeze/ice) and contract tests on both sides of the repo boundary. The orrery consumes them: typed parsing, contract tests driven through the vendored wasm binary, and a seasonal-ice overlay on the globe as the walking-skeleton proof.

Closes hornvale#2 end-to-end; ships the temperature/rainfall/winds core of hornvale#5 (population and transport explicitly trail — §9).

## 2. Roadmap context (the ticket decomposition, non-binding beyond campaign 1)

1. **The Isotherm** (this spec) — hv#2 + hv#5 core; orrery parsing/tests/ice.
2. **The Region** — hv#3 regional tile queries + orrery#2 LOD streaming. The §3 layer table is written for reuse there.
3. **The Moons** — hv#4 moon surface data + orrery moon rendering.
4. orrery#4 surface view (needs 2 + 3).
5. orrery#5 (focus/follow) + orrery#6 (water glint): light client-polish campaign, no upstream dependency.
- hv#1 stays parked per its own text (third-species prerequisite). orrery#1 appears complete on orrery main (its spec deliberately out-scoped fresnel; the coincident-sphere case became orrery#6) — closing it is Nathan's call at G3.

## 3. The contract — `scene/tiles/v1` additions

All additions are **additive within v1** (scene-protocol spec §2: new fields and appended legend entries stay in-version). Every new number is quantized at the emit boundary exactly like the existing layers (8 significant digits, decision 0033).

**Additive-placement convention (new, codified in the schema page):** new fields append **after all existing fields** — after `features`. Wire order is historical accretion order; field *names* are the compatibility contract; the schema page's tables group semantically regardless of wire position. This makes every future additive round mechanical and keeps golden diffs tail-only.

New fields, in their wire order:

| Field | Type | Meaning |
|---|---|---|
| `t_mean_c` | array of number, one per tile | Annual-mean temperature at the tile, °C (the climate model's canonical unit; sampled through the same nearest-cell path as every other layer). |
| `t_swing_c` | array of number, one per tile | **Hemisphere-signed** seasonal half-swing, °C: the coefficient of the seasonal sinusoid, `amplitude × sign(source-cell latitude)`. Positive in the north, negative in the south, exactly `0` on tidally locked and zero-obliquity worlds. Signed at the source so clients never apply a hemisphere sign themselves (a tile near the equator may sample a cell on the other side of it — the sign travels with the data). |
| `season_period_days` | number | The period, in standard days, of the seasonal sinusoid the temperature layers parameterize — the tiles document is **self-contained**: its evaluator never needs a second document. On generated-sky worlds this equals `scene/system/v1`'s `year_days`; on constant-sun worlds (which have no system document) it is the tier-0 default year, and this field is the only honest way a client can know it. |
| `circulation_bands` | integer, **absent when tidally locked** | The number of atmospheric circulation cells per hemisphere (Earth-like day → 3). Document-level, not per-tile: the wind model is a pure function of latitude and this count, and the contract does not pretend otherwise. Absence follows the `day_length_days` precedent in `scene/system/v1`. |
| `moisture` | array of number, one per tile | Dimensionless moisture index in [0, 1] — the climate model's own quantity (band base, ocean proximity, single-pass rain shadow). Deliberately **not** mm/yr: a physical precipitation calibration would be invented precision; the promotion path is registry row CLIM-precip-units and would arrive as a *new* additive field, never a re-meaning of this one. |

### 3.1 The seasonal-temperature evaluator (normative)

Documented verbatim in the schema page; both repos pin it with contract tests:

```
t(tile, day) = t_mean_c[tile] + t_swing_c[tile] · sin(τ · frac(day / season_period_days))
```

- `day` is absolute standard days (`WorldTime`); `season_period_days` is the document's own field (§3), so the evaluator is self-contained.
- The seasonal phase is `frac(day / season_period_days)` — **not** offset by `scene/system/v1`'s `year_phase_offset`. That offset is orbital geometry (where the world sits on its orbit at day 0); the climate model's seasonal sinusoid starts at zero phase at day 0 by construction (`temperature_at`, `domains/climate/src/temperature.rs`). The schema page states this distinction explicitly; it is the likeliest client bug.
- Locked worlds need no special case: `t_swing_c` is 0 there, so the evaluator degenerates to the mean.
- This formula is `temperature_at` restated. A producer-side contract test asserts the restatement reproduces `temperature_at` exactly (same cells, sampled days across a year); if the climate model's seasonal shape ever changes, that test fails and the change is recognized as a contract change, not a refactor.

### 3.2 The prevailing-wind evaluator (normative)

No per-tile wind arrays: the model has no per-tile variance to ship. Given `circulation_bands` = B (present ⇒ the world spins):

```
width      = 90° / B
band(φ)    = min(floor(|φ| / width), B − 1)        # φ = tile latitude, degrees
direction  = easterly (blowing toward −east) when band(φ) is even
             westerly (blowing toward +east) when band(φ) is odd
```

Winds are purely zonal, unit-direction only (the model carries no speed — a speed would be invented), and undefined at the exact poles. Even bands are the rising (wet) belts, odd bands the sinking (dry) belts — stated in the page because it explains the `moisture` belts and serves future data modes. When `circulation_bands` is absent (locked world), there is no banded circulation; the schema page says what the model does instead (moisture organized around the terminator) without promising renderable wind data.

### 3.3 Ice (non-normative guidance)

Ice is a client-side derivation, not a layer — the sim has no cryosphere yet (if one ever ships — registry rows CLIM-cryosphere / CLIM-ice-albedo — a real `ice` layer would supersede this guidance additively):

- Coldest-season temperature at a tile: `t_mean_c − |t_swing_c|`.
- A natural freeze threshold is ≤ 0 °C (suggested, not contractual; clients own presentation per decision 0022).
- Seasonal ice: evaluate §3.1 at the current day and freeze what is below threshold — the ice edge then advances and retreats with the season, opposite-phased across hemispheres, for free.
- Locked worlds: `t_swing_c` = 0 makes ice static — the night-side sheet and twilight band fall out of `t_mean_c` alone (hv#2's locked-world ask).

## 4. Producer implementation (hornvale)

- **`domains/climate`**: expose the signed swing on the provider (e.g. `seasonal_swing_at(cell) -> f64`, °C, signed by the cell's latitude exactly as `temperature_at` signs it; 0 under `RotationRegime::Locked`), plus a `year_length_std()` accessor for `season_period_days`. Pure reads of existing state — no new streams, no draw-order changes, no worldgen physics change.
- **`windows/scene`**: extend `TilesScene` with the five fields (wire order per §3), sampled through the existing `NearestCellIndex` path; quantized serde on the new f64 values; `circulation_bands` as `Option<u32>` with `skip_serializing_if` (the `day_length_days` pattern).
- **`clients/world-wasm`**: no ABI change — the new fields flow through `hw_scene_tiles`. A `world-wasm-v2` release is cut at campaign close (Nathan authorizes the tag push; decision 0055's release-and-pin workflow).
- **CLI**: no flag changes; `hornvale scene tiles` output grows the fields.

## 5. Documentation (the contract's public face)

- **`book/src/reference/scene-tiles-v1.md`**: the five new fields in the field table; the additive-placement convention (§3) codified in Stability; the three evaluators (§3.1–3.3) as their own sections. Layer semantics written as a self-contained per-layer table that a future regional-query document (hv#3) can reference rather than redefine.
- **`book/src/reference/scene-system-v1.md` — new page** (gap found during exploration: the system schema has no reference page). Field-by-field meaning, and the ephemeris evaluators the orrery already implements (`worldPhase`, `synodicDays`, `moonPhase`, `rotationPhase`) documented here as normative formulas — evaluator formulas get exactly one home, the producer's schema pages, since decision 0055 promises one catalog, many clients.
- Book SUMMARY entry for the new page; `docs_consistency` covers the links.

## 6. Consumer implementation (orrery)

- **Typed parsing** (`src/sim/scene.ts`): the five new fields join `TilesScene` with strict validation (lengths, finiteness, `circulation_bands ≥ 1` when present) — strict is correct because the client is pinned to a catalog release that guarantees them.
- **Evaluators** (`src/sim/climate.ts`, new): `temperatureAt(tiles, i, day)` (§3.1 — self-contained, no system document needed), `windAt(bands, latitude)` (§3.2), `coldestC(tiles, i)` (§3.3) — each a line or three, mirroring `ephemeris.ts`'s shape.
- **Re-pin**: `CATALOG_VERSION` → `world-wasm-v2`; vendored `public/hornvale_world.wasm` refreshed in the same commit as the strict parser (never strict-parse against the old binary).
- **The tracer feature — seasonal ice overlay**: the globe whitens tiles whose evaluated temperature at the sim clock's current day is at/below freeze, blended near the edge; locked worlds show the static night-side sheet. Chosen because it exercises *every* new contract element (both layers, the season phase, the freeze derivation) in production posture — parse-only proofs cannot catch unit or sign mistakes. Full data-mode rendering styles stay orrery#3.

## 7. Contract tests (the campaign's spine)

**Producer (hornvale):**
- §3.1 restatement test: the documented formula, fed the **full-precision** mean and signed swing (the provider values, *before* the serialization quantize), reproduces `temperature_at` to exact f64 equality over all cells × sampled days — it is the same arithmetic. This is distinct from what a client sees: a client reads the *quantized* layers, so its evaluator reproduces `temperature_at` only to quantization precision (~8 significant digits). The schema page states both bounds so no consumer mistakes the quantized reconstruction for exact.
- §3.2 restatement test: documented band formula ≡ `circulation.rs` (`band_count_for`, `band_index`, parity signs).
- Layer invariants: lengths = width×height; `t_swing_c` all-zero on a locked-pin world and on zero-obliquity; `circulation_bands` absent on locked, present spinning; all values finite and quantized.
- Golden rebaseline: `tiles-seed-1-w16.json` bytes move (additive-in-v1, NOT an epoch — scene-protocol §2); rebaselined deliberately in the changing commit with the diff reviewed as a contract change (rebaseline-golden-pins discipline). Same for `book/src/gallery/scene-tiles-seed-42.json` via the CI artifact list.
- The existing wasm==CLI byte-identity smoke re-proves the catalog after the change.

**Consumer (orrery):**
- **The binary is the fixture**: vitest instantiates the vendored `public/hornvale_world.wasm` (node runs wasm; `clients/world-wasm/drive.mjs` is the proof), calls `hw_new(42)` + `hw_scene_tiles` + `hw_scene_system`, and asserts the strict parser accepts the real documents. No committed JSON copies — a second copy can only drift.
- Evaluator equivalence: a handful of producer-pinned golden triples `(tile, day) → t` (regenerated by a documented command, committed in orrery testdata with provenance comments) assert the TS evaluators reproduce the Rust values to quantization precision.
- The ice overlay gets ordinary unit tests (freeze classification, hemisphere opposition, locked-world static sheet) + the existing Playwright smoke stays green.

## 8. Consequences for existing artifacts

Golden/gallery rebaselines per §7 (deliberate, reviewed). The atlas viewer and orrery read fields by name — unaffected by appended fields; verified during execution. **No census regen** (no worldgen change; decision 0046 posture). **No epoch** (additive-in-v1). No new streams or concepts.

## 9. Non-goals

- **Population density / transport layers** (rest of hv#5) — trail deliberately: population needs multi-species stack contract design (per-species vs total; carrying-capacity prior vs settled population); transport has no upstream facts to emit. Ticket comment at close states what shipped and what trails.
- **mm/yr precipitation** — CLIM-precip-units, additive later.
- **Sim-side ice/cryosphere** — CLIM-cryosphere/CLIM-ice-albedo remain open frontier ideas.
- **Rendering styles** (orrery#3), **LOD/regional queries** (hv#3/orrery#2), **moon surfaces** (hv#4) — subsequent campaigns.
- **No new wasm exports**; no schema version bump.

## 10. Flagged for G3 (autopilot carve-out summary)

1. **Save-format-adjacent:** additive-in-v1 fields + deliberate golden/gallery rebaselines; the append-at-end additive convention codified into the schema page.
2. **Fidelity honesty calls:** `moisture` stays a dimensionless index (no invented mm/yr); winds ship as one band count + derivation (no invented per-tile variance or speeds); ice is client-derived (no invented cryosphere). Each is the model's own shape, stated plainly.
3. **External actions at close (G6):** `world-wasm-v2` tag push + release; orrery re-pin; ticket closes/comments (hv#2 close, hv#5 scope comment, orrery#1 close recommendation).

## 11. Definition of done

Both repos' gates green (hornvale `make gate`; orrery vitest + Playwright); contract tests of §7 in place on both sides; schema pages updated/created; goldens and gallery artifacts rebaselined and CI-drift-checked; world-wasm-v2 released and orrery re-pinned (Nathan-authorized); chronicle entry + retrospective + book freshness sweep per the standard campaign close.
