# The Shuttle — thread the built world through the readouts

**Campaign:** the-shuttle
**Date:** 2026-07-30
**Status:** Draft — awaiting G3 review
**Thesis:** The gate's slowest tests spend 60–93 % of their cycles re-running
full terrain generation, because the chorus/diachronic readouts derive
everything from a bare `&World` and re-sculpt the globe on every call. The
fix is the Single Sculpt idiom the lab already ships — build the artifacts
once, thread them through — applied to the book/vessel/worldgen readout
paths. A shuttle carries the thread through the loom; this campaign carries
the built terrain through the readouts.

## 1. Evidence (measured 2026-07-30 on lefford, quiet box, frame-pointer dev builds)

A Full generated `build_world` costs **~3.2 s/seed** in the dev profile
(`cargo run -p hornvale-worldgen --example profile_build -- 2`: 6.321 s over
2 seeds; terrain stage 38.4 %, climate+settlements 49.4 %). The slow tests
cost 250–723 s each. The gap is per-call re-derivation:

| Test (lefford baseline) | perf result |
|---|---|
| `hornvale-book … tongue_lines_are_deterministic` (250 s) | **83.7 %** of cycles inside `hornvale_terrain::globe::generate` |
| `hornvale-worldgen deep_grammar::the_coherence_law` (309 s) | **93.1 %** under `chorus::day_schema_of`; 84.9 % terrain; **73.8 % via `noun_class_of`** |
| `hornvale-vessel session::the_stitch_law_end_to_end` (250 s) | **59.5 %** terrain (via `render_volume` and `Session::consult` → `esoteric_lines`); 20 % `demography::coexist::pack` |
| `hornvale-lab health_calibration … no_chronic_distress` (168 s) | 79 % liveness sim loop; **37.6 % `demography_report_with_beta_from`**, of which `coexist::cell_share`'s per-call sum is 13.3 % flat; A* 27.7 % |

The call chain (from the perf call graphs): `render_volume` /
`reckoning_epochs` / `chorus_sections` / `esoteric_lines` loop per species
and call `account_params_of` (~50 % incl.), `doctrine_of` (~37 %),
`cyclic_beliefs_of` (~31 %), `tongue_morphology_of`, `lexicon_of` — each of
which calls `hornvale_worldgen::terrain_of`, which regenerates plates,
elevation, and noise unconditionally (windows/worldgen/src/lib.rs:458 — no
cache, by design). Even `noun_class_of(world, kind, "earth")` re-sculpts,
because sky concepts route through `day_schema_of` → `account_params_of` →
`terrain_of`. Arithmetic: ~209 s of terrain in the 250 s book test at
~1.2 s/sculpt ≈ **~85 sculpts per rendered volume**.

Suite context (committed Timekeeper baselines): hornvale-book is 4 040 s of
the Mac suite's 9 240 s total CPU (44 %); vessel 2 452 s, worldgen 1 449 s,
lab 845 s. A single gate saturates the Mac (`cpu_ratio` 8.25–8.50 on ten
cores), which is why a second concurrent session is currently not viable.

Profile data: `book-tongue`, `worldgen-coherence`, `vessel-stitch`,
`lab-health` perf.data + SVGs in the 2026-07-30 session scratchpad; the
in-tree profiler is `windows/worldgen/examples/profile_build.rs`.

**Prior art in-tree (the precedent this campaign extends):** the lab hit the
identical disease in the census and fixed it — `windows/lab/src/metrics.rs`
(~line 4254): *"reusing the view's already-built terrain and climate instead
of re-sculpting the globe inside `exposure_of` — the census's dominant cost
… the terrain pipeline ran twice per `lexicon_of` call, ~14 metrics deep.
The Single Sculpt … byte-identical."* The artifact-taking API surface
already exists: `lexicon_from`, `accounts_from`, `climate_from`,
`sky_report_from`, `observed_phenomena_*_from`, and the pub
`BuildArtifacts { world, terrain, climate }` bundle
(windows/worldgen/src/lib.rs:208) returned by pub
`build_world_to_with_artifacts` (lib.rs:5035).

## 2. Design

### Stage 1 — thread `BuildArtifacts` through the readout paths

**Contract:** no readout reachable from the book's three public entry points
(`render_volume`, `reckoning_at`, `esoteric_lines`) or from a vessel
`Session` may call `terrain_of`/climate re-derivation; each receives the
already-built artifacts. `WorldComponents::assemble()` likewise happens once
per entry, not per readout.

Mechanics, following the house wrapper-pair idiom (`foo(world)` delegating
to `foo_in(world, wc)`, ~12 existing occurrences per worldgen/CLAUDE.md):

- **worldgen chorus readouts** (`account_params_of`, `cyclic_beliefs_of`,
  `doctrine_of`, `day_schema_of`, `noun_class_of`, `tongue_morphology_of`,
  and the diachronic readouts reachable from `reckoning_epochs`) gain
  `_from` variants taking the built artifacts. Prefer passing
  `&BuildArtifacts` (world + terrain + climate travel together, so a
  mismatched-world misuse is impossible by construction); use loose
  `(&terrain, &climate)` parts only where a caller (lab's `FullView`) holds
  parts, matching the existing `lexicon_from` shape. The existing
  `(world)`-shaped forms remain as delegating wrappers — zero consumers
  break, and tests that exercise them stay valid.
- **windows/book**: the three entry points gain `_from` twins; the `(world)`
  forms build artifacts once and delegate. Internal helpers
  (`chorus_sections`, `reckoning_epochs`, `tongue_probes`, the
  `noun_class_of` closure) thread the bundle. The closure captures the
  per-(world, kind) day-schema answer computed once, not per concept.
- **windows/vessel**: `Session` builds (or receives) the artifacts at
  `start` and reuses them across `handle`/`consult` — the session is the
  natural owner of a per-world view; `esoteric_lines` is called with the
  session's bundle.
- **cli**: the `book`/`possess` render paths switch to the artifacts-
  returning build entry so the CLI single-sculpts too (the committed gallery
  pages regenerate through this path).

**What this is not:** no cache, no global state, no new draws, no new facts,
no serialization change. Every `_from` function is the same pure derivation
over the same inputs; the only change is who pays for the inputs and how
often.

### Stage 2 — `demography::coexist::cell_share` precompute (independent, revertible)

`cell_share`'s per-call iterator sum over cells (13.3 % flat in the health
test, inside `coexist::pack` at 22.9 %) is hoisted into a precomputed
per-cell table (the dense-`Vec` rule, kernel/CLAUDE.md; precedent: The
Lookup, where the same shape was ~22 % of genesis self-time and dropped to
~2 %). **Constraint: the hoist must preserve the exact summation order** —
same adds, same order, computed once — because `cell_share` feeds
`demography_report`, whose values reach committed health/census pins. This
is a pure refactor provable by the health-calibration pins staying
byte-identical; it does NOT license a census regen.

### Explicitly out of scope (captured in the followup register)

Scene-window caching (the Casement line owns the client half), a census
metric sweep for residual `*_of` forms, threading the per-kind language tier
(minor next to terrain; re-profile after Stage 1), the health battery's A*
share, and any test consolidation.

## 3. Determinism and save-format analysis

- **No stream/draw changes.** The readouts draw nothing (chorus.rs's own
  module doc: "zero new draws … always reconstructs the identical answer
  byte-for-byte"); threading changes call topology, not draw order. Stream
  consumption order, seed labels, and the concept registry are untouched. No
  epoch is triggered.
- **Byte-identity is the acceptance bar, verified on the total route**
  (memory: verify-the-total-route): seed-42 world JSON `cmp` between a
  pre-campaign and post-campaign binary; `render_volume` output for seeds
  1–3 compared byte-for-byte across binaries; `make rebaseline` then
  `git diff --exit-code book/src/gallery/ book/src/reference/
  book/src/laboratory/ docs/audits/` must be empty (a non-empty diff is a
  defect, not a rebaseline).
- **The Timekeeper needs no special handling** (verified, not inferred: both
  alarms fire only on *slower* — `c.seconds > b * PER_TEST_MULTIPLE` at
  windows/lab/src/timings.rs:350, `now > was * (1 + SUITE_TOLERANCE)` at
  :401). A speedup runs green and the baseline rewrites itself on that first
  green `make ci` per host. The per-test alarm thereafter stands as the
  regression tripwire against any future readout re-introducing a sculpt.

## 4. Success criteria (testable)

1. **Byte-identity:** the three checks above pass; the full `make gate` is
   green; health-calibration pins unchanged (Stage 2's proof).
2. **Per-test wall time** (lefford, quiet, dev profile, measured before/after
   with the same command): `tongue_lines_are_deterministic` 250 s → **< 25 s**
   (two builds ≈ 6.4 s + rendering); `the_coherence_law` 309 s → **< 30 s**;
   `the_stitch_law_end_to_end` 250 s → **< 90 s** (its coexist/pack share is
   Stage-2-bound, its sim share stays).
3. **Suite total:** nextest `ci`-profile total CPU on lefford drops **≥ 35 %**
   vs the committed `test-baseline-lefford.tsv` sum (21 081 s), measured on a
   quiet box; the run and its wall time ledgered in `docs/timings.md`.
4. A falsified prediction here is a finding: if a target is missed, the
   post-fix flamegraph naming the new dominant cost goes in the chronicle,
   un-retuned.

## 5. Testing strategy

The existing suite IS the harness — it pins bytes at every layer (book line
literals, census fixtures, golden almanacs), so any threading mistake that
changes output reds an existing test. Additions are narrow: the plan's tasks
carry per-stage before/after timing probes (run once, recorded in the ledger
and `docs/timings.md`), and the transient old-vs-new binary comparison at
execution start (~90 s, the Occlusion lesson: cmp the seed-42 world as the
FIRST step, before any refactor lands on top).

## 6. Risks

- **`windows/worldgen/src/lib.rs` is merge-hot** (its CLAUDE.md's own
  warning). the-pigment (touches lib.rs, +1 line) and the-shibboleth (87
  files incl. worldgen tests) are unmerged as of branch time. Mitigation:
  Nathan's absorb-per-task rule (memory: absorb-main-between-tasks), and the
  `_from` additions are additive — wrappers keep every existing signature.
- **Wide mechanical diff** across worldgen/book/vessel. Mitigation: stages
  land as separate commits per crate seam; each commit compiles and passes
  the scoped tests (per the incremental-progress rule); byte-identity checked
  at each seam, not only at the end.
- **Session-lifetime borrow knots** (vessel `Session` holding artifacts
  alongside `&World`). Fallback shape if borrows fight: `Session` owns the
  `BuildArtifacts` by value (build once at `start`), which is the lab
  `FullView` shape — resolved at plan time, not a spec-level unknown.

## 7. Decisions (promoted from the autopilot ledger)

- **G1 — threading over memoization:** adopt the `_from`/`_in` artifact-
  threading idiom; rejected a thread-local `terrain_of` memo (implicit global
  state in the composition root; the future-regression concern it answers is
  already covered by the Timekeeper per-test alarm) and test consolidation
  (doesn't fix product paths). 2 ideonomy passes, 0 overturns.
- **Q — Stage 2 inclusion:** `cell_share` ships in this campaign as an
  independent stage under the summation-order constraint. 1 pass, 0
  overturns.
- **Q — siting:** the campaign runs on lefford (measurement-heavy work on
  the measurement box; Nathan opened the session here; box quiet), with
  `census-run.sh status` checked before every gate. Not a relitigation of
  0086 — its own exception structure. 1 pass, 0 overturns.
