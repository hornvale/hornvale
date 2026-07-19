# The Elements — Implementation Plan

**Spec:** `docs/superpowers/specs/2026-07-18-the-elements-design.md`
**Branch/worktree:** `the-elements`
**G3 calibration (Nathan):** salience = deviation from typical, **conservative
threshold** — only genuinely extreme climates cross the pantheon floor.

**Grounding (from the phenomena-architecture map):**
- `Domain` trait: `kernel/src/domain.rs:12` (`register_concepts`, `stream_labels`
  defaulted). Add `phenomena_source`.
- The six hardcoded fan-outs: `windows/worldgen/src/lib.rs` — `observed_phenomena`
  (~:1470) and `observed_phenomena_from` (~:1625), each building
  `let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];` with `climate =
  UniformClimate` (the tier-0 stub), ~:1477/:1641 (six variants total).
- `GeneratedClimate` (`domains/climate/src/provider.rs`): `temperature_at(cell,
  day)` (:178, seasonal+diurnal), `moisture_at(cell)` (:256), `prevailing_wind`
  (circulation.rs), `biome_at(cell)` (:269) — none wired to phenomena.
- `PhenomenaSource` template: `GeneratedSky` (`domains/astronomy/src/provider.rs
  :1416`). `ObserverContext { position: Option<GeoCoord>, time: WorldTime, … }`
  (`kernel/src/phenomena.rs:82`). Salience affine-magnitude, clamped per kind.
- `PANTHEON_FLOOR = 0.25` (`domains/religion/src/lib.rs:51`); `ambient` = 0.15.
- Tiers refine, never contradict (decision 0039); no new `Stream` draws → no
  epoch.

---

## Stage 1: The phenomena-source roster (byte-identical)
**Goal:** replace the six hardcoded `[&dyn PhenomenaSource; 2]` fan-outs with one
roster-driven path, yielding exactly today's sources — provably byte-identical.
**Deliverable:**
- `Domain` gains `fn phenomena_source(&self, world: &World, wc: &WorldContext)
  -> Option<Box<dyn PhenomenaSource>> { None }` (defaulted; finalize the exact
  args against the call sites — `WorldContext`/whatever worldgen threads).
- A worldgen helper `phenomena_sources(world, …) -> Vec<Box<dyn
  PhenomenaSource>>` that folds `DOMAINS` (each domain's `phenomena_source`),
  plus the sky (kept special until astronomy migrates), into one list. All six
  fan-out sites call it.
- For Stage 1 the roster yields the current pair (sky + tier-0 `UniformClimate`)
  so output is unchanged. `observe` already salience-sorts with a
  kind→description tie-break, so source order cannot affect bytes.
**Success criteria:** every committed artifact regenerates **byte-identical**
(`regenerate-artifacts.sh` + `git diff --exit-code`; the keystone
`world-seed-42.json` unchanged); `make gate` green. No new phenomenon exists yet.
**Tests:** the existing phenomena/almanac/religion suites unchanged and green;
byte-identity of all artifacts; a test that adding a fixture domain's
`phenomena_source` requires no edit to worldgen or another domain.
**Status:** Complete — `Domain::phenomena_source` + a `WorldContext` provisions
channel (composition root builds cross-domain providers, hands them via
`provide`/`claim` — layering-clean); the two fan-outs (four others funnel
through) fold `DOMAINS` behind the sky. Byte-identity independently verified
(regen + golden match); `make gate` green. No new phenomenon yet.

## Stage 2: The climate emitter (the fidelity change, conservative salience)
**Goal:** `GeneratedClimate` emits felt conditions varying with (place × time);
only extreme climates mint deities.
**Deliverable:**
- `impl PhenomenaSource for GeneratedClimate` (mirror `GeneratedSky`): read
  `ctx.position → CellId` (add a pure `GeoCoord→CellId` bridge if none exists in
  the emitter path) and `ctx.time → day`; emit `Venue::Ambient` phenomena as a
  **field × rate** family:
  - temperature: standing mean (`period_days: None` → Eternal), diurnal swing
    (`Some(~1)`), seasonal swing (`Some(season_period)`) → cyclic;
  - moisture: standing dry/wet (binds to `rain` when warm+wet, `snow`/`ice` when
    cold), plus seasonal if a seasonal moisture term exists (else omit);
  - wind: the prevailing wind (standing);
  - biome-as-experienced (standing).
  Each phenomenon's `kind` is a **registered concept key** (decide the new
  concept/kind names against the registry — e.g. a `heat`/`cold` pair or a
  `clime` kind; register any new kinds in climate's `register_concepts`).
- **Salience = deviation from a temperate baseline**, affine and clamped per
  kind, with the **conservative threshold** so only extreme cells reach ≥ 0.25.
- Route `GeneratedClimate` into the roster (`Climate::phenomena_source` returns
  it); the tier-0 `UniformClimate` path stays byte-identical (0039 — refine, not
  contradict: the ambient claim still holds).
- No new `Stream` draws (pure functions of world-state) → confirm no
  stream-consumption change, no epoch.
**Success criteria:** the world.json / religion / almanac delta is exactly the
new weather phenomena and the deities/names they mint — no unrelated drift;
temperate seeds gain few/no weather-gods, extreme seeds gain them (deviation
model working); keystone golden re-frozen; determinism/purity property test
green; `make gate` green.
**Tests:** purity (same (world, ctx) → same phenomena); tier-refinement (ambient
still present); the bounded-delta review; a probe that pantheon growth tracks
climatic extremity.
**Status:** Complete — GeneratedClimate emits felt heat/cold/rain/snow (Venue
Ambient) at deviation-from-14°C-baseline salience; **felt broadly (±2°C emit
band), gods rare (floor at ~±15°C)** per Nathan's calibration. Seed 42 gains 2
sub-floor lines (rain 0.05, cold 0.02), NO new deity, NO name churn — delta is
purely additive registry (verified). Tier-0 byte-identical. Perf: genesis
sources built once (was rebuilt per-observation → 52s; now 9.7s, byte-identical);
NearestCellIndex cached. `make gate` green.

## Stage 3: The correspondence payoff
**Goal:** climate's now-emitted concepts leave the audit's gap lists.
**Deliverable:**
- Flip `snow`/`rain`/`ice` and the biome-class concept percept edges from
  `Void::Gap("not emitted…")` to `Present(PerceptKind(<kind>))` in climate's
  `register_concepts` (only where Stage 2 actually emits them).
- Regenerate `concept-manifest-generated.md`: `Unperceived` shrinks, percept
  covered climbs, orphan phenomena (if the new kinds are all concept-named) stay
  empty on the climate side; the trial balance still foots.
**Success criteria:** the manifest view reflects the closures; the reconciliation
+ trial-balance tests pass; the flipped percepts reference registered kinds.
**Tests:** `cli/tests/correspondence.rs` (foots + reconciliation); the manifest
drift-check; `make gate`.
**Status:** Complete — climate's `rain`/`snow` percept edges flipped to
Present (they leave BOTH the Unperceived AND the Orphan lists — the two-sided
closure); `ice` honestly stays Gap (cold+wet reads as snow). World byte-identical
(edges serde-skip); percept covered 8->10; orphans back to the 4 astronomy
events. `make gate` green.

---

## Definition of Done
- Three stages; `make gate` green; Stage 1 byte-identical; Stage 2 delta bounded
  and re-frozen; Stage 3 closures verified.
- Chronicle + freshness sweep (decision 0013); retrospective (0020); Confidence
  Gradient re-score (the phenomena stream is now a "world checks itself" surface
  — PROC-5); idea-registry row (new capability) at close.
- G6 digest to Nathan: leads with the pantheon-reshape fidelity delta (the
  conservative-salience result) and the new phenomenon-kind save-format labels.

## Deferred (spec §11)
- Terrain standing conditions (elevation band / coastal / biome — a second,
  thinner source); demography's standing-condition fields.
- Stochastic events (no forward simulation — architecturally out of scope).
- Perception/cognition consuming the new phenomena (the cognition wave).
