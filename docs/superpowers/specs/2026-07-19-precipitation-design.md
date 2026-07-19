# Precipitation — advected moisture, rain vs snow, diagnostic clouds (The Rains) — design

**Date:** 2026-07-19 · **Status:** awaiting G3 review
**Program:** Weather Program campaign 3 (after The Turning's diurnal, The Gyre's currents).
**Epoch:** YES — a full derived-field epoch (moisture → biomes → society), census regen.

## 0. Program context and the level it climbs

The Weather Program: diurnal (shipped) → wind/ocean-current vector fields
(shipped) → **precipitation (this spec)** → drawn storms, under the Lorenz
guard-rail (fields + drawn events, never a forward-integrated storm engine).

The Gyre established the coupling-level discipline: level 0 (drawn/derived,
byte-identical), level 1 (a diagnostic that changes nothing biomes read — a
grounding trap), level 2 (a feedback loop → the operator solver). Precipitation
has a sharper cleave because its real fidelity gain lives *in the field biomes
read* (moisture). This campaign is deliberately the **acyclic epoch**: it
replaces the moisture generator (changing biomes + the downstream cascade → an
epoch) but adds **no feedback loop** (no cloud→insolation dimming, no
relaxation solver — those stay in the level-2 campaign). Nathan confirmed the
full epoch at G3.

## 1. The moisture-budget model (`CLIM-moisture-transport`)

Replace `moisture.rs`'s single-pass rain-shadow with an **upwind
moisture-budget trace** — the acyclic generalization of the trace already
there. For each **spinning-world land cell**, walk *upwind* (against
`prevailing_wind`) up to `N` steps toward the sea, carrying a running
precipitable-water budget `W`:

- **Source:** each step over **ocean** adds evaporation (`W += EVAP`, scaled by
  local warmth, capped) — the sea is where moisture enters the air.
- **Orographic sink:** a step where terrain *rises along the wind* lifts the air
  and rains out (`W -= uplift · OROG_K`) — the windward slope wets; because `W`
  is now depleted the **leeward** side is dry, a rain shadow that **compounds**
  over successive ranges.
- **Convective sink + distance decay:** the rising belts (equatorial, ~60°)
  rain a baseline out; `W` decays per overland step, so **continental interiors
  dry with distance from the sea**.

The cell's **precipitation** is what rains out there (arriving `W` × local
uplift/convection); the **moisture `[0,1]`** biomes read is the normalized
precipitation. One bounded trace per cell — **acyclic, deterministic, no
solver** (Approach A; B — relaxation-to-steady-state — rejected as the deferred
level-2 solver; C — downwind accumulation sweep — rejected because the wind
field circulates, so its flow graph has cycles a single sweep can't order).

**Locked worlds keep their substellar moisture model** (wettest at the
terminator) — the trace needs band winds a locked world lacks; this also bounds
the epoch's churn on locked seeds.

**Fidelity knobs (calibrated, census/visual-tuned — the accuracy lives here):**
the step count `N` (inland reach), `EVAP`, and the orographic / convective /
decay coefficients — set so the field reads Earth-plausible (wet windward
coasts, dry leeward interiors, wet tropics, dry horse latitudes) and the
1000-seed census stays sane.

## 2. Precipitation units (`CLIM-precip-units`)

Additive typed fields derived from the new moisture + the existing temperature;
**biomes keep classifying on the moisture `[0,1]`** (now transport-derived), so
no biome-classifier rewrite (`CLIM-koppen` stays deferred).

- **`mm/yr`, a validating newtype** `Precipitation` (joining `Au`/`Mm`/`StdDays`
  — the typed-quantities rule): a calibrated map from rained-out quantity to
  annual precipitation, Earth-ranged (desert `<250`, temperate `~500–1500`,
  rainforest `>2000`). Exposed on the climate provider as `precip_at(cell) ->
  Precipitation` — the field The Confluence's rivers/lakes would weight on.
- **Rain vs snow** — a per-cell snow fraction from the temperature field (which
  carries the seasonal term): snow below freezing, rain when warm, smooth across
  the boundary.
- **Seasonal regime** — a per-cell categorical label (`uniform` / `summer-max` /
  `winter-max` / `monsoon`) from circulation band + continentality + hemisphere.
  A label, not a time series (true seasonal wind reversal is `CLIM-monsoon`).

## 3. Diagnostic clouds (`CLIM-clouds`, diagnostic only)

A per-cell **cloud-fraction `[0,1]`** from **moisture × uplift**: cloudy where
moist air rises (equatorial/~60° rising belts, windward slopes), clear where it
sinks or is dry. **Strictly diagnostic** — no insolation feedback, no
temperature change (the loop is level-2, deferred). Additive; an honest
observable (a cloud field claims nothing the biomes contradict). The client
shades it and may advect it along the winds (Living-Globe drift, sibling to the
currents).

## 4. The epoch and determinism

A **derived-field epoch** (the terrain-epoch family), not a save-format one:

- **No new stream draws, no reordered labels** — moisture-transport is a pure
  derived read (winds/terrain/ocean), consuming no seed draws.
- **It cascades — the reason it is an epoch:** new moisture → new biomes →
  settlement, species, culture re-derive. Every committed seed-42 fixture moves;
  census metrics shift.
- **Epoch discipline at close:** refreeze `cli/tests/fixtures/world-seed-42.json`
  from the new tree; regenerate *all* gallery artifacts (the full cascade, not
  just climate); **AWS census regen** (Nathan's authorization).
- **Deterministic, stable forward:** same seed → same transport-derived moisture
  (bounded trace, `kernel::math`, no wall-clock); byte-identical to itself on
  rebuild. The break is against the old tree, once, deliberately.
- **Additive scene fields** (`precip_mm_yr`, `snow_fraction`, `regime`,
  `cloud_fraction`) on top of the epoch'd moisture; byte-pin re-pins.
- **The Confluence coordination:** if the parallel hydrology campaign also
  epochs, **the two census regens must not race** (a census measures the tree it
  launched from — a mid-run epoch poisons it). Land one epoch, regen, then the
  other. Precip feeds hydrology, so landing **precipitation first** makes its
  field available for river/lake weighting — a natural order.

## 5. The orrery consumer (`the-rains` branch)

- `parseTiles` gains `precip_mm_yr`, `snow_fraction`, `regime`, `cloud_fraction`
  (additive, strict, quantized).
- **A precipitation lens** (mm/yr arid-tan → wet blue-green; rain shadows read
  directly; snow white where the snow fraction is high).
- **A clouds overlay** — cloud-fraction shaded white, advected along the winds
  (Living-Globe drift; non-deterministic eyecandy — 0022/0023).
- The **biome/temperature lenses visibly shift** — the epoch made legible.
- **No golden** (client parses + advects, recomputes nothing). **Re-pin:**
  `wasm:release` → **world-wasm-v10** at G6.

## 6. Testing

Producer property tests (mutation-verified; physics first):
- **Rain shadow** — windward wet, leeward dry across a range (flip the orographic
  sink → fails).
- **Interior dries with distance from sea**; **compounding shadows** (two ranges
  drier than one); **ocean-source dependence** (no ocean upwind → dry).
- **mm/yr in Earth ranges**; **snow where cold / rain where warm**; **cloud
  fraction tracks moisture × uplift**.
- **Locked worlds keep their substellar moisture** (unchanged).
- **Epoch:** the seed-42 fixture is refrozen (an *intentional* move, documented);
  a recorded biome-shift. **Determinism:** byte-identical rebuild.

Client: `parseTiles` field tests; the clouds overlay builds + advects; **visual
verification (The Lens rule)** — the precip lens shows real rain shadows, clouds
gather over the wet belts, snow caps the poles, biomes have visibly rearranged.

## 7. Non-goals (the fence — each deferred with a home)

- **No cloud→insolation feedback** — level-2 loop campaign (`CLIM-operators`).
- **No lake evaporation** — no lake surface exists (terrain marks endorheic
  basins but doesn't fill them); a named hook for after The Confluence.
- **No precipitation-phase** (sleet / freezing rain / ice pellets / hail) —
  captured as **`CLIM-precip-phase`** (a hydrometeor classifier over a vertical
  temp column; sleet/freezing-rain are a later level-0 refinement, hail is
  convective → the storms campaign). Captured here; the `CLIM-precip-phase`
  idea-registry row lands with the campaign (in the worktree, at plan time).
- **No biome reclassification to Köppen/mm-yr** (`CLIM-koppen`) — biomes keep
  reading the moisture `[0,1]`.
- **No snowpack/glaciers** (`CLIM-cryosphere`); **no seasonal wind reversal**
  (`CLIM-monsoon`); winds, currents, diurnal temperature unchanged.

## 8. Flagged for G3

1. **Epoch + census (leads):** a full derived-field epoch — new moisture →
   biomes → society re-derive; every seed-42 fixture moves; **AWS census
   regeneration** (your authorization). No new stream draws/labels (moisture is
   derived). Confirming this reading + the census spend is the main ask.
2. **The Confluence coordination:** epoch/census sequencing between the two
   parallel sessions (land one epoch + regen, then the other; precip-first is
   natural). Not a code conflict.
3. **world-wasm-v10 release + orrery re-pin + pushes** — G6 carve-outs.
4. **Fidelity:** the moisture-budget knobs (step count, evaporation, orographic/
   convective/decay coefficients) and the mm/yr calibration — census/visual-
   tuned; flag any target you want.

## 9. Decisions (promoted from the campaign ledger)

- **Full epoch** — replace the moisture generator (biomes/society cascade + AWS
  census). Nathan confirmed at G3.
- **Approach A** — upwind moisture-budget trace (acyclic, bounded, generalizes
  the existing rain-shadow trace). B (relaxation solver) is the deferred level-2;
  C (downwind sweep) hits wind-circulation cycles.
- **Clouds diagnostic only** — no insolation feedback (level-2).
- **Precip is additive typed** (`Precipitation` mm/yr, snow fraction, regime);
  biomes keep reading moisture `[0,1]`; no golden (client parses).
- **Lake evaporation deferred** (no lake surface yet — hook for The Confluence);
  **`CLIM-precip-phase` captured** (sleet/hail roadmap).
