# The Local Census — Implementation Plan

**Branch/worktree:** `the-local-census` (from main `52f380d2`)
**Kind:** byte-identical performance campaign — The Single Sculpt pattern
applied to the Lab metric path. No physics changes; no new draws; no
save-format change. The census goldens must not move.

## Why

A `metrics: all` census world costs ~245 CPU-s. The Explore map (this
session) found the cost is **not** the ~3 view-chain sculpts but ~**190+
redundant full-climate rebuilds per world** (each a terrain sculpt),
concentrated in two metric extractors:

- `name_gloss_true` (`windows/lab/src/metrics.rs`) loops **every settlement**
  (~182 on the seed-42 world) calling `settlement_site_concepts` →
  `observed_phenomena_as_at` → `climate_of` → sculpt, **no memoization**.
- `pantheon_sig` (~9/world) → `observed_phenomena_as_in` → `climate_of` →
  sculpt.
- The `ClimateView::build_to` chain re-derives climate once via
  `climate_of` when it already holds the built terrain (Sculpt 3).

Redundant re-sculpting is ~70–85% of the 245 CPU-s. The `FullView` **already
holds** the climate these extractors rebuild (`v.climate()`, built once in
the view chain). The fix threads that pre-built climate in — exactly what
worldgen's *genesis* loop already does internally (`phenomena_sources_from`
/ `observe_with_sources`, documented byte-identical because
`phenomena_sources` is a pure function of the world).

**Target:** ~245 → ~55 CPU-s/world (~4–6×). Full census (~2000 all-metric
worlds) on this 40-core box: ~3.4 h → ~25–45 min. The payoff is retiring the
recurring AWS census spend — a local run diffed against the committed AWS
goldens is itself the proof that local ≡ AWS.

## Determinism guard (the proof it stays byte-identical)

- `windows/lab/tests/depth_ladder.rs::depth_scoped_metrics_match_full_build`
  (+ the all-metric variant) — the metamorphic guard.
- `scripts/ci-census-probe.sh` — regenerates the first N=8 seeds of
  `the-census` + `census-of-the-meeting` and diffs against the committed
  head. A byte-identical refactor passes unchanged.
- The keystone `cli/tests/words_identity.rs` (name-gloss truth) still calls
  the unchanged `observed_phenomena_as_at` — the standalone wrappers stay.

## Stage 1: View-chain climate (`climate_of` → `climate_from`) — BYTE-IDENTICAL
**Goal:** drop Sculpt 3; de-risk the thread-pre-built-value pattern on one line.
**Deliverable:** `metrics.rs:249` `climate_of(&terrain.astronomy.world)?` →
`climate_from(&terrain.astronomy.world, &terrain.terrain)?` (`climate_from`
already `pub`; `terrain.terrain` is the just-built globe).
**Success:** `depth_ladder` + census-probe byte-identical; `cargo test -p
hornvale-lab` green.
**Status:** Complete — `climate_from(&terrain.astronomy.world, &terrain.terrain)`.

## Stage 2: Thread the pre-built climate through the observation metrics
**Goal:** kill the ~190 per-settlement/per-species sculpts.
**Deliverable:**
- worldgen (`windows/worldgen/src/lib.rs`): two public `_from` wrappers
  mirroring the existing public observers but reusing a pre-built climate via
  `phenomena_sources_from` instead of `phenomena_sources`:
  `observed_phenomena_as_at_from(world, wc, species, place, &climate)` and
  `observed_phenomena_as_in_from(world, wc, species, &climate)`. Both carry
  the byte-identity rationale in their doc + a `type-audit:` tag matching the
  existing observers (`bare-ok(identifier-text: species)`).
- lab (`metrics.rs`): `settlement_site_concepts` gains a `&GeneratedClimate`
  param; `name_gloss_true` passes `v.climate()` (built once, before the
  loop). `pantheon_sig` uses `observed_phenomena_as_in_from(.., v.climate())`.
**Success:** `depth_ladder` + census-probe byte-identical; a single-world
`metrics: all` timing drops multi-fold (measured).
**Status:** Complete — two public `_from` wrappers thread `v.climate()`;
`name_gloss_true` builds climate once before the settlement loop;
`pantheon_sig` + `epithet_honorific` (the third `_as_in` site the exhaustion
sweep caught) use `observed_phenomena_as_in_from`. Proven byte-identical by a
direct A/B (clean-main vs branch census rows identical) + the all-metric
metamorphic guard; measured **285 → 105 CPU-s/world (2.7×)**, wall 8:50 →
2:35 on 8 seeds. The census-probe "drift" is pre-existing golden staleness
(main is 26 commits past the last AWS regen), not this change.

## Stage 3: Measure, and decide on the residual
**Goal:** confirm the win; decide whether the per-call `phenomena_sources_from`
residual (sky rebuild + climate clone × ~182, no sculpt) warrants a
build-sources-once pass. Run the full census locally to firm up the
wall-clock.
**Deliverable:** before/after per-world timing; a full local census (opt-in,
local, no spend); a G6 digest to Nathan with the numbers and the **AWS-drop
recommendation** (carve-out: dropping the remote census pipeline is Nathan's
call).
**Status:** Complete.
- **Build-sources-once (2b) — TRIED AND REVERTED.** A `WorldPhenomena` handle
  to build the phenomena sources once per world. Measured **no speedup** (838s
  → 870s, noise): the sculpt was the whole cost, `sky_of`/clone cheap.
  Reverted per YAGNI. **The inference that "the ~105 CPU-s residual is genuine
  metric math" was WRONG** — a flamegraph (Stage 4) showed it was still ~91%
  terrain sculpting. This null result is why the campaign switched to
  profiling.

## Stage 4: The flamegraph campaign (profile, don't infer)
**Goal:** Nathan asked to keep hunting after 2.7×. Replace inference with
measurement — `cargo flamegraph` the count=8 census each round, thread the
pre-built terrain/climate into whatever worldgen readout the profile shows
sculpting, verify byte-identical (clean-main A/B), commit, re-profile.
**Deliverable / Status:** Complete — six byte-identical rounds:
1. `name_gloss_true` per-settlement re-observe → view climate (285→105).
2. ~14 lexicon metrics via `exposure_of` → `lexicon_from` (105→47) — the
   sculpt was buried inside `lexicon_of`, invisible to a source grep.
3. chorus `accounts_of` per-people → `accounts_from` (47→27.7).
4. chorus `cyclic_beliefs_of` sky re-observe → `cyclic_beliefs_from` (27.7→18).
5. demography → `demography_report_from` (18→15.1).
6. **genesis** re-sculpted 6–8× to *name* the world → `world_name_in_from`/
   `lexicon_of_in_from` (15.1→5.8) — byte-identical incl. the seed-42 fixture,
   almanac, gallery; sped up `new`/almanac too.
**Result: 284 → 5.8 CPU-s/world (~49×)**, full ~2000-world census ~4h → **~5
min** (clean-main baseline re-measured at low load: 2272s vs 46s, count=8).

## Definition of Done
- `make gate` green (final); census-probe byte-identical (against a fresh
  local golden — the committed goldens lag main by design).
- Chronicle (`book/src/chronicle/the-local-census.md`) rewritten for the
  6-fix arc + freshness sweep; retro (profile-don't-infer); idea-registry
  `PERF-lab-metric-rebuilds` → shipped, ~49×.
- Confidence-Gradient: no census-cost/AWS bet in `open-questions.md` (checked).
- G6 digest leading with the AWS-drop decision + the byte-identity proof.

## Deferred / follow-up
- **Metric-purity lint** — fail on a metric/readout reaching `climate_of`/
  `terrain_of`/`exposure_of` instead of a `_from`. Makes the tenth instance of
  this bug impossible.
- **Genesis-perf campaign** — "Fix D" (view chain re-sculpts terrain genesis
  built and discarded; changes `build_world_to`'s signature) + further
  genesis-internal sculpts a `new` profile surfaces. Its own scoping + G3.
