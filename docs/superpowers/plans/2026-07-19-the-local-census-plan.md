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
**Status:** In Progress.
- **Build-sources-once (2b) — TRIED AND REVERTED.** Introduced a
  `WorldPhenomena` handle to build the phenomena sources once per world
  (collapsing the residual ~182 per-settlement `sky_of` + climate-clone).
  Measured: **no speedup** (user 838s → 870s, within noise) — killing the
  sculpt in Stage 2 was the whole win; `sky_of`/clone are cheap. Reverted per
  YAGNI: public API surface (a struct + two methods) for zero measured
  benefit. The residual ~105 CPU-s/world is genuine metric math + the one
  intrinsic world build, not redundant source-building; the Explore's ~55
  CPU-s post-fix estimate was optimistic, the real floor is ~105.
- Anti-regrowth is better served by the captured follow-up (a metric-purity
  lint on direct `climate_of`/`terrain_of`) than by an unused abstraction.
- Full local census wall-clock: measuring (background run).

## Definition of Done
- `make gate` green; census-probe + depth_ladder byte-identical.
- Chronicle (`book/src/chronicle/the-local-census.md`) + freshness sweep;
  retro; idea-registry `PERF-lab-metric-rebuilds` → shipped, repointed.
- Confidence-Gradient: the census-cost / AWS-dependence bet re-scored if one
  exists in `open-questions.md`.
- G6 digest leading with the AWS-drop decision + the byte-identity proof.

## Deferred / follow-up
- Truly build-sources-once (opaque handle) if Stage 3 shows the sky/clone
  residual still dominates.
- View-chain Sculpt 2 (`build_world_to` discards its internal terrain) —
  low marginal value next to Stage 2; only if measurement demands it.
