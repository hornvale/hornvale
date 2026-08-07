# The Assay — the census as the suite's world-building pass

**Status:** spec, awaiting G3 · **Date:** 2026-08-07 · **Campaign:** The Assay
· **Branch:** `campaign/the-assay`

An assay does not ask whether an ore sample contains gold. It reports how much
gold per tonne. This campaign moves the test suite's world-dependent claims from
the first question to the second — and, where a claim genuinely is about one
world's behaviour, moves it off generated worlds entirely.

---

## 1. The problem

A test that sweeps seeds to *find* an instance of a property is doing the
census's job badly and the synthetic's job expensively. That is decision
[0093](../../decisions/0093-seed-hunting-is-not-a-test-mechanism.md), ratified
2026-07-31, whose consequences section charters "**the build-volume audit**" as
its own follow-up; `docs/retrospectives/the-weir.md:80` carries it as open.

The wider problem, and this campaign's actual subject: **the suite builds worlds
independently and throws them away.** 224 gate tests build a world at
`Settlements` or `Full` depth. Only 26 tests read a committed fixture instead.
Meanwhile the census already builds 1,000 worlds and runs ~200 extractions
against each, and its consumers pay nothing.

## 2. The finding: the mechanism already exists

`windows/lab/src/runner.rs:164` — one build per seed, every metric mapped over
it:

```rust
match BuiltView::build_to(Seed(seed_value), pins, roster, depth) {
    Ok(built) => Ok(Row {
        seed: seed_value,
        pin_set: label.to_string(),
        values: metrics.iter().map(|m| m.extract.apply(&built)).collect(),
        refusal: None,
    }),
```

Everything a consistency check needs is already first-class:

| need | already exists |
|---|---|
| a boolean check result | `MetricValue::Flag(bool)`, `SummaryKind::Flag` |
| one build shared by all checks | `build_row`, above |
| a check declaring the depth it needs | `Extractor::rung()` → the runner builds to the deepest selected rung |
| the gate reading results for free | decision [0032](../../decisions/0032-calibration-loads-the-census-fixture.md), `load_rows(committed rows.csv)` |
| a refusal-path claim | `Row.refusal` |

And the pattern is not merely available — **24 gate-resident tests in
`windows/lab/tests/calibration.rs` already use it** (25 in the file; the 25th is
the `#[ignore]`d `census_fixture_matches_live_run` full-proof guard):
`head_deity_is_eternal_exactly_when_tidally_locked`,
`band_count_matches_the_known_function_of_rotation`,
`phonotactic_validity_is_true_for_every_generated_name`,
`the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale`,
`name_gloss_true_is_100_percent_row_by_row`, and twenty more.

**This campaign therefore adds no architecture.** It adds a freshness guard, a
routing rubric, a lint, and a first tranche of migrations.

## 3. Measured evidence

Every number below was measured or read from a committed artifact during the
brainstorm, not inferred.

### 3.1 The cost asymmetry, in one comparison

| binary | checks | worlds per check | cost, lefford baseline |
|---|---:|---|---|
| `hornvale-lab::calibration` | 24 | 1,000 | **no rows above the 1 s floor** |
| `hornvale-worldgen::diachronic` | 7 | 3–5 | 137.9 s |
| `hornvale-worldgen::exposure` | 18 | 1–9 | 145.2 s |
| `hornvale-vessel::session` | 20 | 1–few | 419.7 s |

Twenty-four checks against a thousand worlds cost less than seven checks against
five.

### 3.2 Where a world's cost sits

`cargo run -p hornvale-worldgen --example profile_build -- 6`, debug, this Mac:

Per world (the profiler's 6-seed totals divided by 6):

```
astronomy                  0.0020 s    0.1%
terrain                    0.4920 s   12.6%
climate+settlements        3.1598 s   81.1%
alignments                 0.0007 s    0.0%
culture+religion+species   0.0147 s    0.4%
deep-time                  0.2027 s    5.2%
planet / peoples           0.0233 s    0.6%
                        ----------
Full world                 3.8952 s
```

Two consequences. **`BuildDepth` is a lever independent of the census** — an
Astronomy-rung world is ~1,950× cheaper than a Full one, which is why
astronomy's 256-seed batteries are already nearly free (~0.5 s of genesis) and
why they are *not* targets. And the census runs in **release**
(`scripts/census-run.sh:142`), at ~0.7 s/world — 5.6× cheaper per world than a
debug test build.

### 3.3 The migration surface

Gate tests (non-`#[ignore]`d) by the deepest build they reach:

| crate | Settlements/Full | terrain | astronomy | fixture-backed | no build |
|---|---:|---:|---:|---:|---:|
| `hornvale-worldgen` | **138** | 30 | 1 | 0 | 324 |
| `hornvale-book` | **40** | 0 | 0 | 0 | 11 |
| `hornvale` (cli) | **20** | 0 | 0 | 5 | 212 |
| `hornvale-vessel` | **11** | 1 | 0 | 2 | 406 |
| `hornvale-lab` | **9** | 2 | 0 | 7 | 292 |
| `hornvale-scene` | **6** | 3 | 0 | 1 | 84 |
| others | 0 | 215 | 1 | 11 | 1,280 |
| **total** | **224** | 251 | 2 | 26 | 2,609 |

### 3.4 The three seed hunts (0093's literal criterion)

Of 57 tests that build a world inside a seed loop, three use multiple seeds
*only* to locate an instance — all three in the commit gate:

| test | shape | builds | measured |
|---|---|---|---|
| `terrain/hydro_witness::every_hydro_variant_is_witnessed_on_a_real_world` | ∀variant ∃seed | 8 × L6 globe, breaks on seed 0 | **0.51 s** |
| `worldgen/exposure::every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_sweep` | ∀concept ∃seed | up to 9 Full | 8.412 s |
| `worldgen/diachronic::a_crisis_fires_on_a_real_generated_sky` | ∃seed | up to **200** Full | 7.617 s |

**Correction, found while planning (2026-08-07).** This spec's first draft routed
`a_crisis_fires` to a hand-built synthetic world, on the model of decision 0093's
own Stage 3 (`windows/worldgen/tests/doctrine.rs`'s `synthetic_flagship`). **That
is not feasible, and the plan must not assume it.** `crisis_from` calls
`observations_from` (`windows/worldgen/src/chorus.rs:1671`), which opens with
`crate::sky_of(world)?` and refuses anything but a `Sky::Generated`, then derives
its event list from real orbital mechanics via
`hornvale_astronomy::eclipse_events(sky.system(), sky.calendar(), …)`. A crisis
additionally needs ≥ `K_PREDICT` (8) witnessed events of one recurrence class and
a miss-run in the tail. None of that can be hand-committed as facts.

The crisis therefore routes to **`claim: rate(census: crisis-fires)` plus one
live arm at a census-identified seed** — see §5 and Stage 4. This is the better
answer anyway: the census does the searching once, in release, on lefford, and
the seed it finds is recorded rather than re-hunted on every commit. Note the
generated *sky* is cheap (0.0020 s/world, §3.2); it is `climate+settlements` at
81% that makes the current sweep expensive.

~16.5 s against a 352 s median gate — **4.7%**. This campaign is not justified
on that. It is justified on two other properties:

- **Unbounded tail.** All three are cheap only because the break hits early.
  `a_crisis_fires`'s own comment plans for the tail: *"If none of 1..=200 shows
  one, WIDEN the search range."* The price is a random variable another
  campaign's physics can move.
- **Unreported margin.** None can say how close it came — decision
  [0097](../../decisions/0097-assert-the-robust-half-measure-the-fragile-half.md)'s
  "a value pin's noise profile with an invariant's authority."

### 3.5 The population is heterogeneous

Fifty-four of the 57 are **not** hunts. Four of five kinds must be left alone:

| kind | claim shape | example | verdict |
|---|---|---|---|
| reachability checklist | ∀v ∃s | `hydro_witness`, `exposure` | retire → census coverage |
| instance hunt | ∃s | `diachronic::a_crisis_fires` | retire → synthetic |
| distribution readout | rate over s | `health_calibration` null control | keep (0093 protects) |
| property battery | ∀s | `tectonic::single_craton` (40 pinned), astronomy locked-hemispheres (32) | keep — not a hunt |
| seedless sweep | builds no world | `rule_witness`, `deep_realm_chamber` | keep — near-free |

`deep_realm_chamber::the_lattice_is_fixed_and_existence_is_sparse` — the caves
test — builds **no world**; it drives the pure function `chamber_exists` over 5
seeds. It reads like a hunt and is not one.

## 4. The mechanism

### 4.1 A consistency check is a metric

A claim that is a function of one world becomes a `Metric` whose extractor
returns `MetricValue::Flag(bool)` (an invariant) or `Number`/`Text` (a measured
quantity). It costs the census almost nothing — the world is already built and
held by `build_row` — and the gate nothing at all, because the gate reads the
committed `rows.csv`.

The gate-side assertion lives in the `windows/lab/tests/` calibration family and
follows the existing idiom: load the fixture, locate the column by name, assert
over all rows, and **name the offending seed** in the failure message. A `Flag`
invariant asserts the column is all-`true`; a rate asserts a bound; a coverage
table asserts each variant's share is non-zero.

### 4.2 The tripwire — the precondition for all of it

A census-resident check is verified when the census regenerates: **once per
campaign at close.** The risk is recorded rather than hypothetical — 0097 §4 and
The Siding found the census stale for **139 commits** while every gate ran
green.

So the campaign ships a live tripwire before it moves anything:

```
gate, every commit:
  build TRIPWIRE_SEEDS (3 fixed seeds) live, at the SHALLOWEST depth
    the moved checks require — never Full unless a moved check needs it
  run the SAME check functions the metrics call
  compare against those seeds' rows in the committed rows.csv
  -> any disagreement is a RED: the fixture is stale
```

**Its budget, and why it is affordable.** At the measured 3.90 s per Full world
(§3.2, debug), three Full-depth seeds cost ~11.7 s, so the budget is **≤ 15 s**,
not the couple of seconds a cheap probe suggests. That is close to cost-neutral
for this campaign: retiring the three hunts returns ~16.5 s (§3.4). It is not
free for the follow-on, and the depth rule above is what keeps it bounded — a
tranche of Terrain-rung checks needs only 3 × 0.49 s.

Three properties are load-bearing:

1. **It calls the same functions.** Not a reimplementation — the metric's
   extractor is the single definition, so the tripwire cannot drift from the
   thing it guards.
2. **It is cheap enough to stay in the commit gate.** This is what
   `fixture_staleness.rs` could not manage, because it probes all ~200 metrics
   and was exiled to the heavy tier. Scoping to check functions on 2–3 seeds is
   what buys the gate residency.
3. **It is mutation-tested before any check moves** (Nathan's condition, G1
   ledger #6). A guard against silent staleness that is itself silently broken
   is strictly worse than no guard — it converts an honest gap into a false
   assurance. Stage 2 does not exit until a deliberately perturbed fixture reds
   it.

### 4.3 What this does not change

Determinism, layering, and save-format contracts stay in the gate regardless of
sample size — 0097 §5, unamended.

## 5. The routing rubric and the claim notation

Each test's claim declares a shape; the shape picks the instrument.

```
claim: reachability(census: <metric>)     forall v in V. exists s. P(v, w_s)
        -> per-variant coverage table over committed rows; n = 1000
claim: rate(census: <metric>, [lo, hi])   #{s : P(w_s)} / |S| within a bound
        -> fixture-backed assertion; n = 1000
claim: invariant(census: <metric>)        forall s. P(w_s), s over the census
        -> Flag column asserted all-true; a violation names the seed
claim: behavior(synthetic)                P(w_synthetic)
        -> hand-built world carrying the committed fact; zero builds
        -> available ONLY when the behaviour reads committed facts. A
           derivation that re-derives from a generated sky or a sculpted
           globe cannot be synthesised — see §3.4's correction, where
           `crisis_from` fails this test and routes to a rate instead.
claim: invariant(forall-seed)             forall s in S. P(w_s), S pinned/small
        -> stays live (see §6.6)
claim: readout(preregistered, 0016)       a measured distribution
        -> stays put
claim: structural(seed: <n>)              byte-identity, prose, CLI surface
        -> stays live at ONE fixed seed, never a sweep
```

**The heavy tier is not a destination.** `carve_properties::shelf_width_asymmetry`
is a hunt-shaped test that was `heavy:`-ignored rather than fixed; deferring a
hunt hides it.

## 6. What cannot move

Named here so the follow-on campaign does not rediscover them one at a time.

1. **Byte-identity and determinism** — needs the same seed built twice and
   compared. A census row is one build.
2. **Prose and rendering** — `hornvale-book`'s 41 tests (712.7 s) assert
   rendered strings. A digest column would work mechanically but trades a
   readable golden for an opaque hash; the book already drift-checks its
   generated artifacts, which is the better lever.
3. **Save-format round-trip** — serialize → load → compare.
4. **CLI and REPL surface** — needs process invocation.
5. **Action sequences** — `hornvale-vessel` (2,079 s over 129 tests, the largest
   crate) asserts what holds after a scripted walk. Not a per-world scalar.
6. **Pinned regimes** — `the-census.study.json` has exactly one `pin_set`
   (`default`). Every pinned claim needs a new pin_set at 1,000 worlds each,
   which is why `tectonic::single_craton_...` (`continents: Some(1)`) and the
   zero-obliquity and locked-rotation batteries stay live.
7. **A cell- or entity-level rate** needs a per-world metric that aggregates
   first, and the aggregation choice (pooled vs. mean-of-ratios) *is* the claim.
   `shelf_width_asymmetry` is the worked example: two new metrics, not a
   lift-and-shift.

Two rejected implementations, recorded so they are not re-proposed:

- **A committed corpus of pre-built worlds.**
  `cli/tests/fixtures/world-seed-42.json` is 1.68 MB → ~1.7 GB at census scale.
  Worse, derived geometry is not serialized (decision 0069), so a *loaded* world
  still re-derives terrain and climate — 93.7% of build cost.
- **An in-process `LazyLock` shared world.** Dead by construction under
  nextest's process-per-test model; this is the reason 0032 rejected it.

## 7. Scope

**This campaign ships the mechanism and one tranche.** The full 224-test
migration is a follow-on, informed by what the first tranche measures. The
brainstorming discipline requires flagging an over-large spec rather than
refining its details.

In scope: the tripwire; the routing rubric; the three seed hunts; a first
tranche of `hornvale-worldgen` scalar invariants; the within-binary rebuild
merge; the claim-tag lint.

Out of scope, recorded as follow-ups: `hornvale-book` (40), `hornvale-vessel`
(11), `hornvale` (20), `hornvale-scene` (6), the remaining worldgen tests; a
second `pin_set` for pinned-regime claims; per-test durations for the heavy
tier; re-recording the stale Mac baseline.

## 8. Stages

### Stage 1 — The audit, committed as data
**Goal:** the classification of all 224 Settlements/Full-depth gate tests, by
claim shape and destination, plus the 57 seed-loop tests by kind.
**Success:** a committed table; every kept test carries its measurement so it is
not re-suspected from the code smell (the repo's "deferred WITH the
measurement" idiom).
**Tests:** none — this stage is a document.
**Status:** Not Started

### Stage 2 — The tripwire, mutation-tested
**Goal:** `windows/lab/tests/tripwire.rs`: build `TRIPWIRE_SEEDS` live, run the
metric extractors, compare against the committed rows.
**Success:** in the commit gate at ≤ 15 s measured (§4.2's budget); **and** a
deliberately perturbed fixture reds it, with the perturbation and both outputs
recorded in the stage report.
**Tests:** the tripwire itself; a mutation check that a one-cell fixture edit
fails it; a check that a *fresh* fixture passes.
**Status:** Not Started
**Gate:** no check moves until this stage's mutation evidence is recorded.

### Stage 3 — The metrics the retirements need
**Goal:** register the new metrics, each with a unit test driving its extractor
over a live view. No test is retired in this stage and no fixture is
regenerated — the columns do not exist in `rows.csv` yet.
- `hydro-variant-coverage` (Terrain rung, `Categorical`): the sorted set of
  `Hydro` variants `hydro_at` reads anywhere on this world, rendered as a
  stable joined string. The census has `karst-fraction`/`aquifer-fraction` but
  nothing for `Aquitard`/`Runoff`/`Spring`.
- `toponymic-roots-won` (Full rung, `Numeric`): how many of the world's
  toponymic-domain concepts reach `ExposureClass::Steeped` for some placed
  people.
- `crisis-fires` (Full rung, `Flag`): whether `crisis_from` returns `Some` for
  any placed people at the preregistered epoch.
**Success:** `cargo run -p hornvale -- lab list-metrics` shows all three; each
has a unit test that builds one live view and asserts the extractor's shape.
**Tests:** one per metric, at the shallowest rung that metric needs.
**Status:** Not Started

### Stage 4 — One census regen, then retire the three hunts
**Goal:** regenerate the census so the three new columns exist, review the
diff, then write the gate-side assertions and delete the hunts.
**Success:** `rows.csv` carries the new columns; `make lab-diff
STUDY=the-census` reviewed for unintended movement in the pre-existing columns;
the three hunts deleted; each retirement's before/after cost recorded; the
`crisis-fires` live arm pinned to a seed **read out of the regenerated census**,
with that seed named in the test's doc comment.
**Tests:** a coverage assertion per variant; a roots-won assertion; a
`crisis-fires` rate assertion plus the one live structural arm.
**Status:** Not Started
**Carve-out:** the regen needs Nathan's explicit authorization.
**Ordering note:** Stage 3 cannot be verified end-to-end before this stage,
because a metric's column does not exist until a regen writes it. That is
inherent to the census's cadence, not an accident of sequencing.

### Stage 5 — Within-binary rebuild merge
**Goal:** collapse tests that each rebuild the same seed set inside one binary
(`diachronic`: 7 tests × seeds 1..=5) into one build per seed set.
**Success:** measured before/after on the affected binaries; no assertion lost.
**Tests:** unchanged assertions, re-homed.
**Status:** Not Started
**Note:** this trades "one assertion per test" for build reuse — a deliberate,
documented exception to the global guideline, justified by process-per-test.

### Stage 6 — The claim-tag lint
**Goal:** `cli/tests/claim_shape.rs`, default-deny: a test containing a seed
loop must carry a `claim:` token from §5's vocabulary.
**Success:** green on the tree; adding an untagged seed loop reds it (verified
by injection, the `hydro_witness` discipline).
**Tests:** the scan's accept/reject boundary as a pure function, unit-tested
without touching the filesystem.
**Status:** Not Started

### Stage 7 — Book, chronicle, retrospective
**Goal:** Definition of Done per decision 0013.
**Success:** chronicle entry; freshness sweep; retrospective; a Confidence
Gradient re-score (0030) — this campaign is the third corner of 0097's triangle,
the check that *cries wolf*, and `book/src/open-questions.md` should say so.
**Status:** Not Started

## 9. Decisions to ratify at G3

1. **A reachability claim is a census question, answered as a coverage table.**
   Extends 0093's two-instrument split to three. Its own first application
   (`hydro_witness`) fits neither original box.
2. **A population claim is asserted in the commit gate over the committed
   fixture.** A reading of 0097 §2 against 0097 §4 and 0032; 25 existing tests
   already assume it.
3. **The census is the suite's shared world-building pass.** A check that is a
   function of one world belongs in the census as a metric, not in a test that
   builds its own world.
4. **A census-resident check requires a live tripwire in the gate.** The
   generator-paired-with-verifier rule of 0097 §4, given a mechanism.
5. **Build reuse outranks one-assertion-per-test under process-per-test.**

## 10. Risks

| risk | mitigation |
|---|---|
| the tripwire passes while the fixture is stale | Stage 2's mutation evidence is a hard exit criterion |
| 2–3 seeds miss a drift that moves only other seeds | accepted and stated: the tripwire bounds staleness, it does not eliminate it; the full guard remains `census_fixture_matches_live_run` |
| a moved check loses diagnosability | every assertion must name the offending seed; a bare `assert!` over 1,000 rows is a review finding |
| test-breadth reduction | flagged at G3, as 0093 did for its own Stage 3 |
| the new columns move existing calibration pins | `make lab-diff` reviewed as part of Stage 4, before the assertions are written |

## 11. Definition of Done

Stages 1–7 complete; `make gate` green; `make gate-full` green; the census
regenerated once with `make lab-diff` reviewed; chronicle, freshness sweep,
retrospective, and Gradient re-score landed; follow-ups promoted out of
`.superpowers/sdd/` before teardown.
