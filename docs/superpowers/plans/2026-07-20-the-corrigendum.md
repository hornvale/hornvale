# The Corrigendum Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A culture's taught eclipse prediction stops being omniscient and
becomes a naive model it could actually have computed — one that
genuinely, sometimes, misses — and a miss that recurs surfaces as a live
"crisis" in the Book's Reckoning-of-Years, with a later knowledge-state's
fresh re-derivation standing in for revision.

**Architecture:** `windows/worldgen/src/chorus.rs`'s `ladder_of` swaps its
omniscient future-query for a pure mean-interval extrapolation over
already-witnessed eclipse days; a new sibling function, `crisis_of`,
retrospectively replays that same model against history to detect two
consecutive misses. `windows/book/src/lib.rs`'s Reckoning renderer reads
`crisis_of` and adds a margin line (and, staged, a doctrine-voice line) —
no new ledger fact, no new stream, no new `Observability` row.

**Tech Stack:** Rust, existing `hornvale-worldgen`/`hornvale-book` crates,
`cargo nextest`.

## Global Constraints

- `PREDICTION_TOLERANCE_FRACTION = 0.05` — a naive prediction is a hit
  within 5% of its own fit's mean interval (spec "Hit or miss, and a
  tolerance").
- `CRISIS_MISS_RUN = 2` — a crisis is live exactly when the two most
  recent retrospective checks are both misses; a single miss is silently
  absorbed (spec "Crisis, not anomaly").
- No new predicate, no new `Observability` row, no new `FactShape`, no new
  stream label anywhere in this plan (spec "Determinism and save-format").
  Every new function is pure over facts `observations_of` already
  produces.
- Type-audit tags on every new `pub` primitive use the exact grammar
  `bare-ok(<class>: <field-name>)` — copy the class from the existing
  table (`ratio`, `count`, `index`, `diagnostic-value`, `identifier-text`,
  `prose`, `flag`, …), never a field name used as its own class. This
  exact malformed-tag bug recurred twice in The Consonance's own plan
  text — state it here once, copy it verbatim into every task below.
- `cargo fmt` is the final step of every task before committing.
- Keystone/golden refreezes (`make rebaseline-goldens`) happen only if a
  live check shows real drift — verify first, never refreeze
  speculatively.

---

## Before Task 1: two corrections to the spec, found while planning

Both are cheap to state now and would otherwise cost a review cycle
later, exactly the pattern The Consonance's own planning phase caught
repeatedly.

1. **The spec's Task 1 says the existing test
   `the_prediction_line_omits_honestly_beyond_the_teaching_horizon`
   (`windows/book/src/lib.rs`) needs updating to drive the rebased
   honest-omission arm.** Tracing the actual call chain: that test calls
   `reckoning_culture_lines(autonym, rung, held, prediction)` directly,
   passing a hand-built `Option<f64>` — it never calls `ladder_of` at
   all. `reckoning_culture_lines` is pure rendering logic that doesn't
   care *why* its `prediction` argument is `None`; it is completely
   unchanged by this campaign. The rebased omission check
   (`beyond_teaching_horizon`) lives entirely inside `ladder_of`
   (`chorus.rs`), and is tested there directly (Task 1, below). **No
   change to that existing book-crate test is needed at all.**
2. **`windows/worldgen/tests/diachronic.rs`** — not mentioned in the spec
   at all — has TWO tests that directly depend on `ladder_of`'s prediction
   being the true, omniscient future value, and both break the moment
   Task 1 lands: `LADDER_TABLE` (a pinned table of exact `(rung,
   prediction)` values per seed/culture) needs its `Option<f64>` values
   re-measured (rungs stay identical — a real regression check); and
   `the_prophecy_law`, whose entire assertion is that a taught day always
   exists in the true future event set (its own panic message: "doctrine's
   predictions are never wrong") — a guarantee this campaign deliberately
   removes. Both are folded into Task 2, below; `the_prophecy_law` needs a
   real rewrite, not a re-pin (its old claim is simply false under the new
   mechanism).

---

### Task 1: the naive model, retrospective checks, and crisis detection

**Files:**
- Modify: `windows/worldgen/src/chorus.rs` (new consts, new pure
  functions, `ladder_of`'s prediction branch, new `crisis_of`/
  `PredictionCrisis`)
- Modify: `windows/worldgen/src/lib.rs` (re-export `crisis_of`,
  `PredictionCrisis`)
- Test: `windows/worldgen/src/chorus.rs`'s own `#[cfg(test)] mod tests`

**Interfaces:**
- Produces: `pub fn crisis_of(world: &World, species: &str, at:
  hornvale_astronomy::StdDays) -> Result<Option<PredictionCrisis>,
  BuildError>` and `pub struct PredictionCrisis { pub last_predicted:
  f64, pub last_actual: f64 }` — Task 3 (windows/book) calls this.
- Consumes: `Observations` (existing), `observations_of`, `doctrine_of`,
  `recurrence_key` (all existing, already in `chorus.rs`).

- [ ] **Step 1: Locate the insertion points**

Run:
```bash
grep -n "const PREDICTION_HORIZON_STD_DAYS\|fn recurrence_key\|pub fn ladder_of\|^#\[cfg(test)\]" windows/worldgen/src/chorus.rs
```
Expected: four line numbers — the const block (~1578), `recurrence_key`
(~1653), `ladder_of` (~1670), and the test module (~1729). Use these to
place the edits below; if line numbers have drifted (other campaigns may
have landed on `main` since this plan was written), match by function/
const name instead of the literal number.

- [ ] **Step 2: Write the failing tests for the pure functions**

Add to the existing `#[cfg(test)] mod tests` block in
`windows/worldgen/src/chorus.rs` (near its top, alongside the other
`use super::*;` tests):

```rust
    /// The naive model's own worked example (spec "Hit or miss, and a
    /// tolerance"): intervals 30,30,30,30,45 read hit-hit-hit-miss and
    /// are NOT yet a crisis (only the tail two checks decide that, and
    /// the tail is hit-then-miss here, not miss-then-miss). Hand-verified
    /// before committing: prior [0,30] predicts 60 (exact hit); prior
    /// [0,30,60] predicts 90 (exact hit); prior [0,30,60,90] predicts
    /// 120 (exact hit); prior [0,30,60,90,120] (mean 30) predicts 150,
    /// actual is 165, a 15-day miss against a 1.5-day tolerance.
    #[test]
    fn the_naive_model_reads_hit_hit_hit_miss_and_no_crisis_yet() {
        let days = [0.0, 30.0, 60.0, 90.0, 120.0, 165.0];
        assert!(retrospective_hit(&days, 2), "prior [0,30] -> 60: exact hit");
        assert!(retrospective_hit(&days, 3), "prior [0,30,60] -> 90: exact hit");
        assert!(
            retrospective_hit(&days, 4),
            "prior [0,30,60,90] -> 120: exact hit"
        );
        assert!(
            !retrospective_hit(&days, 5),
            "prior [0,30,60,90,120] (mean 30) predicts 150, actual is 165: a miss"
        );
        assert!(
            !crisis_live(&days),
            "the two most recent checks are hit(i=4), miss(i=5) -- not both misses"
        );
    }

    /// The crisis case (spec "Hit or miss, and a tolerance"): intervals
    /// 30,30,30,45,60 miss on BOTH of the tail two checks. Hand-verified:
    /// prior [0,30,60,90] (mean 30) predicts 120, actual is 135, a 15-day
    /// miss against a 1.5-day tolerance; prior [0,30,60,90,135] (mean
    /// 33.75) predicts 168.75, actual is 195, a 26.25-day miss against a
    /// 1.6875-day tolerance.
    #[test]
    fn two_consecutive_misses_read_a_live_crisis() {
        let days = [0.0, 30.0, 60.0, 90.0, 135.0, 195.0];
        assert!(retrospective_hit(&days, 2), "prior [0,30] -> 60: exact hit");
        assert!(
            retrospective_hit(&days, 3),
            "prior [0,30,60] -> 90: exact hit"
        );
        assert!(
            !retrospective_hit(&days, 4),
            "prior [0,30,60,90] (mean 30) predicts 120, actual is 135: a miss"
        );
        assert!(
            !retrospective_hit(&days, 5),
            "prior [0,30,60,90,135] (mean 33.75) predicts 168.75, actual is 195: a miss"
        );
        assert!(
            crisis_live(&days),
            "the two most recent checks are both misses: a live crisis"
        );
    }

    /// Fewer than CRISIS_MISS_RUN + 2 witnessed days means fewer than
    /// CRISIS_MISS_RUN retrospective checks even exist yet -- never a
    /// crisis, regardless of how wild the one or two available checks
    /// would read.
    #[test]
    fn too_few_witnessed_days_are_never_a_crisis() {
        assert!(!crisis_live(&[0.0, 30.0, 1000.0]), "only one check exists (i=2)");
        assert!(!crisis_live(&[]), "empty");
    }

    /// The rebased honest-omission arm (spec Task 1): a naive prediction
    /// beyond the teaching horizon from `at` is treated exactly like the
    /// old omniscient-existence check used to be -- omitted, never taught
    /// as if it were certain.
    #[test]
    fn beyond_teaching_horizon_reads_true_past_the_prediction_horizon() {
        assert!(
            beyond_teaching_horizon(20_000.0, 5_000.0),
            "20,000 is beyond 5,000 + 10,000"
        );
        assert!(
            !beyond_teaching_horizon(10_000.0, 5_000.0),
            "10,000 is within 5,000 + 10,000"
        );
        assert!(
            !beyond_teaching_horizon(15_000.0, 5_000.0),
            "exactly at the horizon: not beyond it"
        );
    }
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `cargo test -p hornvale-worldgen --lib chorus::tests:: -- --nocapture 2>&1 | tail -40`
Expected: FAIL — `retrospective_hit`, `crisis_live`, `beyond_teaching_horizon`
not found in this scope (they don't exist yet).

- [ ] **Step 4: Implement the pure functions**

Insert immediately after the `PREDICTION_HORIZON_STD_DAYS` const (before
`observations_of`):

```rust
/// How close a naive prediction must land to its own fit's mean interval
/// to count as a hit (spec "Hit or miss, and a tolerance") -- 5%, the
/// same numeric convention as `hornvale_astronomy::resonance::
/// RATIO_TOLERANCE` without reusing that unrelated constant.
const PREDICTION_TOLERANCE_FRACTION: f64 = 0.05;

/// How many consecutive retrospective misses constitute a live crisis
/// (spec "Crisis, not anomaly") -- a single miss is silently absorbed;
/// only a run counts.
const CRISIS_MISS_RUN: usize = 2;
```

Insert immediately after `recurrence_key` (before `ladder_of`):

```rust
/// The mean interval between consecutive entries of an ascending,
/// >= 2-element day sequence -- the naive model's own building block.
fn mean_interval(days: &[f64]) -> f64 {
    let n = days.len();
    (days[n - 1] - days[0]) / (n - 1) as f64
}

/// The naive model's own next-day prediction from an ascending,
/// >= 2-element witnessed-day sequence: the last witnessed day plus the
/// mean interval between witnessed occurrences (spec "A naive model,
/// replacing the omniscient lookup").
fn naive_predicted_day(days: &[f64]) -> f64 {
    days[days.len() - 1] + mean_interval(days)
}

/// Whether a naive prediction's own day falls beyond the teaching horizon
/// from `at` -- the honest-omission arm, rebased from the naive value
/// rather than an omniscient existence check (spec: "the existing
/// honest-omission arm is preserved, re-based on the new value").
fn beyond_teaching_horizon(predicted_day: f64, at: f64) -> bool {
    predicted_day > at + PREDICTION_HORIZON_STD_DAYS
}

/// Whether the naive model, fit on `days[..i]`, correctly predicted
/// `days[i]` -- within `PREDICTION_TOLERANCE_FRACTION` of that fit's own
/// mean interval. `i` must be >= 2 (at least two prior occurrences to fit
/// a mean interval) -- every call site below satisfies this by
/// construction.
fn retrospective_hit(days: &[f64], i: usize) -> bool {
    let prior = &days[..i];
    let predicted = naive_predicted_day(prior);
    let tolerance = PREDICTION_TOLERANCE_FRACTION * mean_interval(prior);
    (predicted - days[i]).abs() <= tolerance
}

/// Whether a live crisis exists for an ascending witnessed-day sequence:
/// the `CRISIS_MISS_RUN` most recent retrospective checks are all misses
/// (spec "Crisis, not anomaly"). Fewer than `CRISIS_MISS_RUN + 2` entries
/// means fewer than `CRISIS_MISS_RUN` checks even exist yet -- no crisis.
fn crisis_live(days: &[f64]) -> bool {
    let n = days.len();
    if n < CRISIS_MISS_RUN + 2 {
        return false;
    }
    ((n - CRISIS_MISS_RUN)..n).all(|i| !retrospective_hit(days, i))
}

/// The most-witnessed recurrence class in an observation set, and its
/// count -- the same tie-break `ladder_of` has always used (max count,
/// then the numerically smallest `(moon index, body)` key). `None` for an
/// empty observation set. Factored out of `ladder_of` so `crisis_of`
/// (below) shares the identical class-selection logic rather than
/// re-deriving it.
fn top_recurrence_class(observations: &Observations) -> Option<((usize, u8), usize)> {
    let mut class_counts: BTreeMap<(usize, u8), usize> = BTreeMap::new();
    for &(_, moon, body) in &observations.events {
        *class_counts.entry(recurrence_key(moon, body)).or_insert(0) += 1;
    }
    class_counts
        .iter()
        .max_by_key(|(key, count)| (**count, std::cmp::Reverse(**key)))
        .map(|(&key, &count)| (key, count))
}

/// The ascending witnessed days of one recurrence class, filtered from a
/// culture's full observation set (already day-ascending per
/// `observations_of`'s own doc, so filtering to one class preserves
/// order).
fn class_days(observations: &Observations, key: (usize, u8)) -> Vec<f64> {
    observations
        .events
        .iter()
        .filter(|&&(_, moon, body)| recurrence_key(moon, body) == key)
        .map(|&(day, _, _)| day)
        .collect()
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-worldgen --lib chorus::tests:: -- --nocapture 2>&1 | tail -40`
Expected: PASS — all four new tests green.

- [ ] **Step 6: Rewrite `ladder_of`'s prediction branch, and add `crisis_of`**

Replace the entire body of `pub fn ladder_of` with:

```rust
pub fn ladder_of(
    world: &World,
    species: &str,
    at: hornvale_astronomy::StdDays,
) -> Result<(LadderRung, Option<f64>), BuildError> {
    let observations = observations_of(world, species, at)?;
    if observations.events.is_empty() {
        return Ok((LadderRung::Unknown, None));
    }

    if doctrine_of(world, species).is_none() {
        return Ok((LadderRung::Counted, None));
    }

    let (top_key, top_count) = top_recurrence_class(&observations)
        .expect("top_recurrence_class is non-empty whenever observations.events is");

    if top_count < K_PREDICT {
        let rung = if observations.events.len() >= K_COUNT {
            LadderRung::Numbered
        } else {
            LadderRung::Counted
        };
        return Ok((rung, None));
    }

    let days = class_days(&observations, top_key);
    let predicted_day = naive_predicted_day(&days);
    let prediction = if beyond_teaching_horizon(predicted_day, at.get()) {
        None
    } else {
        Some(predicted_day)
    };
    Ok((LadderRung::Predictive, prediction))
}
```

This drops the old `sky_of`/`eclipse_events`/`horizon_end`/`target_body`
locals entirely — `observations_of` (called at the top of this same
function) already guarantees a `Generated` sky by the time this branch
runs, so the old redundant `Sky::Generated` re-check is gone along with
the omniscient query it guarded. The function's own doc comment (the
paragraph above `pub fn ladder_of`) doesn't currently name the omniscient
mechanism explicitly, so nothing needs deleting — add one sentence at the
end of that doc comment: "The predicted day is the naive model's own
extrapolation (see `naive_predicted_day`), not an omniscient lookup — it
can be, and sometimes is, wrong (see `crisis_of`)."

Immediately after `ladder_of`, add:

```rust
/// One culture's predicted-vs-actual crisis state for its own top
/// recurrence class, at `at` (spec "Crisis, not anomaly") -- `Some`
/// exactly when a live crisis exists, carrying the most recent missed
/// retrospective check's own predicted and actual days for the Reckoning
/// margin to quote verbatim. Requires the same organized-cult gate
/// `ladder_of` itself checks (SOC-1): a folk-only culture never has a
/// crisis to report, since nothing was ever predicted for it to miss.
///
/// type-audit: bare-ok(identifier-text: species)
pub fn crisis_of(
    world: &World,
    species: &str,
    at: hornvale_astronomy::StdDays,
) -> Result<Option<PredictionCrisis>, BuildError> {
    let observations = observations_of(world, species, at)?;
    if doctrine_of(world, species).is_none() {
        return Ok(None);
    }
    let Some((top_key, top_count)) = top_recurrence_class(&observations) else {
        return Ok(None);
    };
    if top_count < K_PREDICT {
        return Ok(None);
    }
    let days = class_days(&observations, top_key);
    if !crisis_live(&days) {
        return Ok(None);
    }
    let n = days.len();
    Ok(Some(PredictionCrisis {
        last_predicted: naive_predicted_day(&days[..n - 1]),
        last_actual: days[n - 1],
    }))
}

/// One culture's live prediction crisis (see [`crisis_of`]): the most
/// recent missed retrospective check's own predicted and actual days, for
/// the Reckoning margin to quote verbatim.
/// type-audit: bare-ok(diagnostic-value: last_predicted), bare-ok(diagnostic-value: last_actual)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PredictionCrisis {
    /// What the naive model, fit on every occurrence before the most
    /// recent one, predicted for it.
    pub last_predicted: f64,
    /// What actually happened.
    pub last_actual: f64,
}
```

- [ ] **Step 7: Re-export the new items**

In `windows/worldgen/src/lib.rs`, add `crisis_of` and `PredictionCrisis`
to the existing `pub use chorus::{...}` block (`cargo fmt` will settle
the exact ordering — `PredictionCrisis` alongside the other capitalized
types, `crisis_of` alongside the other lowercase functions).

- [ ] **Step 8: Build and run the full chorus test module**

Run: `cargo test -p hornvale-worldgen --lib chorus:: 2>&1 | tail -60`
Expected: PASS — every existing test in this module still passes (the
rung-classification logic is unchanged, only refactored into
`top_recurrence_class`), plus the four new tests from Step 2.

- [ ] **Step 9: `cargo fmt`, clippy, and type-audit**

Run: `cargo fmt -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 10: Commit**

```bash
git add windows/worldgen/src/chorus.rs windows/worldgen/src/lib.rs docs/audits/type-audit-report.md
git commit -m "feat(worldgen): the naive prediction model and crisis detection -- the-corrigendum T1"
```

---

### Task 2: re-pin `LADDER_TABLE` and rewrite `the_prophecy_law`

**Files:**
- Modify: `windows/worldgen/tests/diachronic.rs`

**Interfaces:**
- Consumes: `ladder_of`'s new return values from Task 1 (rungs unchanged,
  `Option<f64>` predictions changed), `crisis_of`/`PredictionCrisis` from
  Task 1.

- [ ] **Step 1: Run the existing test to see it fail on the changed predictions**

Run: `cargo test -p hornvale-worldgen --test diachronic the_ladder_law -- --nocapture 2>&1 | tee /tmp/ladder-law.txt`
Expected: FAIL. Read the panic output carefully — it must show the SAME
rung for every row (`Predictive` stays `Predictive`, `Counted` stays
`Counted`, and so on: this is the regression check that the
`top_recurrence_class` refactor in Task 1 didn't change rung
classification). Only the `Option<f64>` prediction column differs. If any
rung differs, STOP — that is a real regression in Task 1, not something
to paper over here; go back and fix Task 1 first.

- [ ] **Step 2: Re-pin the table's prediction values from the real output**

Edit `LADDER_TABLE` in `windows/worldgen/tests/diachronic.rs`: for every
row whose `LadderRung::Predictive` prediction changed, replace the old
`Some(...)` literal with the new value shown in Step 1's panic output
(Rust's shortest-round-trip `f64` formatting, matching the table's own
existing literal style — e.g. `Some(9080.42957840976)`). Leave every
`None`/`Counted`/`Unknown` row untouched. Update the two explanatory
comments above rows re-pinned by prior campaigns (e.g. "Was:
`LadderRung::Predictive, Some(36526.1181181615)`") only if this task
itself changes THOSE specific rows — do not touch comments for rows this
task doesn't affect.

- [ ] **Step 3: Run the test again to verify it passes**

Run: `cargo test -p hornvale-worldgen --test diachronic the_ladder_law -- --nocapture 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 4: Run the full diachronic test file**

Run: `cargo test -p hornvale-worldgen --test diachronic 2>&1 | tail -40`
Expected: `the_prophecy_law` (line ~342) FAILS — its whole assertion is
"doctrine's predictions are never wrong" (its own panic message says so
literally), checking the taught day against the TRUE future event set.
That guarantee is exactly what this campaign removes by design. Every
other test in the file stays green.

Replace `the_prophecy_law`'s body entirely with:

```rust
#[test]
fn the_prophecy_law() {
    // C9 (The Corrigendum): the taught prediction is no longer
    // omniscient, so it is no longer necessarily the TRUE future event
    // (see `a_crisis_fires_on_a_real_generated_sky` for a seed where
    // it's demonstrably wrong). What still holds, and is the real law
    // now: a Predictive culture's taught day, when `Some`, is EXACTLY
    // what the naive model computes from that culture's OWN witnessed
    // days for its own top recurrence class -- self-consistency between
    // `ladder_of` and the model it's built from, not a truth guarantee.
    let t = at(EPOCH_2);
    let mut any_predictive = false;

    for seed in 1..=5u64 {
        let w = generated(seed);
        for (kind, _) in placed_peoples(&w) {
            let (rung, prediction) = ladder_of(&w, kind, t).unwrap();
            if rung != LadderRung::Predictive {
                continue;
            }
            any_predictive = true;
            let Some(day) = prediction else {
                continue;
            };

            // Re-derive the most-observed recurrence class with the same
            // deterministic tie-break ladder_of documents (max count,
            // ties toward the numerically smallest (moon, body) key).
            let obs = observations_of(&w, kind, t).unwrap();
            let mut counts: std::collections::BTreeMap<(usize, u8), usize> =
                std::collections::BTreeMap::new();
            for &(_, moon, body) in &obs.events {
                let d = match body {
                    EclipseBody::Solar => 0u8,
                    EclipseBody::Lunar => 1u8,
                };
                *counts.entry((moon, d)).or_insert(0) += 1;
            }
            let (&(moon, body_d), _) = counts
                .iter()
                .max_by_key(|(key, count)| (**count, std::cmp::Reverse(**key)))
                .expect("a Predictive culture has witnessed events");
            let target_body = if body_d == 0 {
                EclipseBody::Solar
            } else {
                EclipseBody::Lunar
            };
            let days: Vec<f64> = obs
                .events
                .iter()
                .filter(|&&(_, m, b)| m == moon && b == target_body)
                .map(|&(d, _, _)| d)
                .collect();
            let last = *days.last().expect("non-empty by construction");
            let mean = (last - days[0]) / (days.len() - 1) as f64;
            assert_eq!(
                day,
                last + mean,
                "seed {seed} {kind}: the taught day must equal the naive model's own \
                 extrapolation from this culture's own witnessed days"
            );
        }
    }

    assert!(
        any_predictive,
        "NO culture in seeds 1..=5 reaches Predictive at epoch 2 -- the ladder's top must be \
         visible somewhere (the preregistered demand); widen the epoch or seed sweep"
    );
}
```

This duplicates the tie-break logic inline rather than calling
`top_recurrence_class` — that function is private to `chorus.rs`, and the
ORIGINAL version of this test already duplicated the same logic inline
for the same reason (an integration test re-derives against the public
API surface, never reaches into private internals). The now-unused
`HORIZON` const and `EclipseKind`-adjacent imports this replacement no
longer needs should be removed if `cargo clippy` flags them as dead.

- [ ] **Step 5: `cargo fmt` and commit**

Run: `cargo fmt -p hornvale-worldgen`

```bash
git add windows/worldgen/tests/diachronic.rs
git commit -m "test(worldgen): re-pin LADDER_TABLE and the prophecy law for the naive model -- the-corrigendum T2"
```

---

### Task 3: the Reckoning margin gains a crisis line, plus a live-seed proof

**Files:**
- Modify: `windows/book/src/lib.rs` (`reckoning_epoch`, `ReckoningEpoch`
  doc comment)
- Modify: `windows/worldgen/tests/diachronic.rs` (a new live-seed search
  test)
- Test: `windows/book/src/lib.rs`'s own `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: `hornvale_worldgen::crisis_of`, `hornvale_worldgen::
  PredictionCrisis` (Task 1).
- Produces: `ReckoningEpoch.margin` may now carry more than one line (the
  doc comment change below is load-bearing for any future caller reading
  that field).

- [ ] **Step 1: Update `reckoning_epoch` to collect margin lines during the loop**

In `windows/book/src/lib.rs`, replace `reckoning_epoch`'s body (the
`let mut lines = Vec::new(); let mut falls_short = false;` block through
its closing `ReckoningEpoch { ... }`) with:

```rust
    let mut lines = Vec::new();
    let mut margin = Vec::new();
    let mut falls_short = false;
    for (kind, _village) in hornvale_worldgen::placed_peoples(world) {
        let Some(autonym) = autonyms.get(kind) else {
            continue;
        };
        let observations =
            hornvale_worldgen::observations_of(world, kind, at).unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires observations_of to succeed for placed culture \
                 {kind}: {e}"
                )
            });
        let (rung, prediction) =
            hornvale_worldgen::ladder_of(world, kind, at).unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires ladder_of to succeed for placed culture \
                     {kind}: {e}"
                )
            });

        if rung == hornvale_worldgen::LadderRung::Unknown {
            falls_short = true;
            continue;
        }

        let held = observations.events.len() as u64;
        lines.extend(reckoning_culture_lines(autonym, rung, held, prediction));

        if culture_falls_short(rung, held, true_count as u64) {
            falls_short = true;
        }

        if rung == hornvale_worldgen::LadderRung::Predictive {
            let crisis = hornvale_worldgen::crisis_of(world, kind, at).unwrap_or_else(|e| {
                panic!(
                    "the Reckoning section requires crisis_of to succeed for placed culture \
                     {kind}: {e}"
                )
            });
            if let Some(crisis) = crisis {
                margin.push(format!(
                    "In truth, the {autonym}'s priesthood taught the darkening would come on \
                     day {}; it came on day {} instead.",
                    crisis.last_predicted.trunc() as u64,
                    crisis.last_actual.trunc() as u64
                ));
            }
        }
    }

    if falls_short {
        margin.push(format!(
            "In truth, the darkenings {margin_phrase} number {}.",
            cardinal(true_count as u64)
        ));
    }

    ReckoningEpoch {
        heading: heading.to_string(),
        lines,
        margin,
    }
```

The count-shortfall line, if any, is always LAST — a deliberate ordering
choice (per-culture crisis lines, in `placed_peoples` order, then the
one world-level shortfall line), not an accident of the refactor.

- [ ] **Step 2: Update the `ReckoningEpoch.margin` and struct doc comments**

The struct's own doc comment (immediately above `pub struct
ReckoningEpoch`) currently ends with a sentence describing `margin` as
carrying the truth-margin sentence "exactly when some culture's held
knowledge falls short of the true count." Replace that sentence with:
"`margin` carries zero or more lines: one per placed culture with a live
prediction crisis (`hornvale_worldgen::crisis_of`, placed-culture order),
then — exactly when some culture's held knowledge falls short of the true
count — the world-level shortfall sentence, last." Update the `pub
margin: Vec<String>` field's own doc comment (just above the field) the
same way, and remove the now-inaccurate "at most one line" phrase.

- [ ] **Step 3: Write a unit test for the new margin line, driven synthetically**

Since this needs a real placed culture with a live crisis and no
guaranteed live seed exists yet (Step 5 below searches for one), first
write a test against `reckoning_epoch`'s own building block —
`crisis_of`'s consumer-facing format — is not directly testable without a
world. Instead, confirm the format string itself via the existing
`reckoning_culture_lines`-style pure-function pattern by adding this new
free function (place it right after `reckoning_culture_lines`) and
testing IT directly, then wire it into Step 1's `reckoning_epoch` call
site by replacing the inline `format!` with a call to it:

```rust
/// One placed culture's Reckoning margin line for a live prediction
/// crisis (spec Task 1's margin extension) -- pure and world-free, mirrors
/// [`reckoning_culture_lines`].
fn reckoning_crisis_margin_line(autonym: &str, crisis: hornvale_worldgen::PredictionCrisis) -> String {
    format!(
        "In truth, the {autonym}'s priesthood taught the darkening would come on day {}; it \
         came on day {} instead.",
        crisis.last_predicted.trunc() as u64,
        crisis.last_actual.trunc() as u64
    )
}
```

Replace Step 1's inline `margin.push(format!(...))` block with
`margin.push(reckoning_crisis_margin_line(autonym, crisis));`.

Add to the test module:

```rust
    #[test]
    fn the_crisis_margin_line_quotes_the_taught_and_true_days() {
        let crisis = hornvale_worldgen::PredictionCrisis {
            last_predicted: 41_200.3,
            last_actual: 40_850.9,
        };
        assert_eq!(
            reckoning_crisis_margin_line("Vavako", crisis),
            "In truth, the Vavako's priesthood taught the darkening would come on day 41200; \
             it came on day 40850 instead."
        );
    }
```

- [ ] **Step 4: Run the new test**

Run: `cargo test -p hornvale-book --lib the_crisis_margin_line_quotes_the_taught_and_true_days -- --nocapture`
Expected: PASS.

- [ ] **Step 5: Find a live seed exhibiting a real crisis**

Add to `windows/worldgen/tests/diachronic.rs` (needs `crisis_of` added to
its `use hornvale_worldgen::{...}` import list at the top of the file):

```rust
#[test]
fn a_crisis_fires_on_a_real_generated_sky() {
    // C9 (The Corrigendum) T1/T3: prove the naive model's crisis
    // detection fires on at least one live seed, not only on synthetic
    // data. If none of 1..=200 shows one, WIDEN the search range and
    // document the range that was needed -- never weaken
    // PREDICTION_TOLERANCE_FRACTION or CRISIS_MISS_RUN just to force a
    // hit; those are the spec's own considered values (decision ledger
    // #2).
    let mut found = None;
    for seed in 1..=200u64 {
        let w = generated(seed);
        for (kind, _) in placed_peoples(&w) {
            if let Some(crisis) = crisis_of(&w, kind, at(EPOCH_2)).unwrap() {
                found = Some((seed, kind.to_string(), crisis));
                break;
            }
        }
        if found.is_some() {
            break;
        }
    }
    let (seed, kind, crisis) = found.unwrap_or_else(|| {
        panic!(
            "no seed in 1..=200 exhibited a live prediction crisis by day {EPOCH_2} -- widen \
             the search range rather than shipping this mechanism unexercised"
        )
    });
    assert!(
        crisis.last_predicted != crisis.last_actual,
        "seed {seed} {kind}: a crisis's own last predicted/actual days must differ"
    );
}
```

Run: `cargo test -p hornvale-worldgen --test diachronic a_crisis_fires_on_a_real_generated_sky -- --nocapture 2>&1 | tail -20`

Expected: PASS, with a real `(seed, kind, crisis)` found. If it FAILS
(panics with "no seed in 1..=200..."), widen the loop's range (try
`1..=500`, then `1..=1000`) and re-run; once a seed is found, record the
actual range that was needed in a comment above the test (e.g. "found at
seed 173" or "widened to 1..=500, found at seed 341") — this is real,
measured information the plan cannot supply in advance.

- [ ] **Step 6: Check whether seed 42's Reckoning text drifted**

Run:
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json
cargo run -p hornvale -- almanac --world /tmp/hv42.json > /tmp/almanac-seed-42.md
diff /tmp/almanac-seed-42.md book/src/gallery/almanac-seed-42-sky.md
```
Also check the committed keystone fixture and Book gallery pages that
render the Reckoning:
```bash
cargo test -p hornvale --test lens_purity seed_42_world_json_matches_the_committed_fixture 2>&1 | tail -20
git status --short book/src/gallery/ book/src/reference/
```
If any of these show drift (seed 42 has a `Predictive`-rung culture whose
taught day or crisis state changed), run `make rebaseline-goldens` and
review the diff before committing — it should be purely additive/value
changes on the Reckoning's own lines, nothing structural. If there is no
drift (seed 42 never reaches a live crisis or even `Predictive` rung),
note that plainly and skip the refreeze — do not refreeze speculatively.

- [ ] **Step 7: Run the full book and worldgen test suites**

Run: `cargo test -p hornvale-book -p hornvale-worldgen 2>&1 | tail -60`
Expected: PASS, all tests, including every existing Reckoning test (`the_reckoning_renders_the_epoch_pair`, `reckoning_at_matches_the_fixed_pair_and_renders_arbitrary_days`, `every_reckoning_line_round_trips`, `the_margin_fires_exactly_when_knowledge_falls_short`, `goblin_section_speaks_and_margins_seed_1`, `seed_2_kobold_sees_moons_goblin_margins_them`).

- [ ] **Step 8: `cargo fmt`, clippy, type-audit, and the manifest check**

Run: `cargo fmt -p hornvale-book -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-book -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
Run:
```bash
cargo run -p hornvale -- streams > /tmp/hv-streams-check.md
diff /tmp/hv-streams-check.md book/src/reference/stream-manifest-generated.md
```
Expected: empty diff (this task adds no stream labels, so this should
already be clean — this step confirms it, matching the discipline that
caught the exact opposite gap in two prior campaigns).

- [ ] **Step 9: Commit**

```bash
git add windows/book/src/lib.rs windows/worldgen/tests/diachronic.rs docs/audits/type-audit-report.md
# If Step 6 found drift:
git add cli/tests/fixtures/world-seed-42.json book/src/gallery/ book/src/reference/ book/src/laboratory/
git commit -m "feat(book): the Reckoning margin gains a crisis line, proven on a real seed -- the-corrigendum T3"
```

---

### Task 4: a doctrine-voice acknowledgment (staged — only if Tasks 1–3 landed clean)

**Do not start this task if Task 3's review found any Critical or
Important issue still open.** The spec's own staging condition (decisions
#2, #4) is a hard gate, not a formality.

**Files:**
- Modify: `windows/book/src/lib.rs` (`reckoning_epoch`, a new
  `reckoning_doctrine_line` function)
- Test: `windows/book/src/lib.rs`'s own `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: `hornvale_worldgen::doctrine_of` (existing), the `crisis:
  Option<PredictionCrisis>` value Task 3's loop body already computes.

- [ ] **Step 1: Write the failing tests**

Add to the test module in `windows/book/src/lib.rs`:

```rust
    #[test]
    fn the_doctrine_line_is_none_for_a_folk_only_culture() {
        assert_eq!(reckoning_doctrine_line("Vavako", false, false), None);
        assert_eq!(
            reckoning_doctrine_line("Vavako", false, true),
            None,
            "a folk-only culture never has a doctrine to have taught anything wrongly"
        );
    }

    #[test]
    fn the_doctrine_line_names_the_crisis_when_one_is_live() {
        assert_eq!(
            reckoning_doctrine_line("Vavako", true, false),
            Some("None among the Vavako have shown the priesthood's teaching false.".to_string())
        );
        assert_eq!(
            reckoning_doctrine_line("Vavako", true, true),
            Some(
                "The Vavako's own priesthood taught wrongly, and could be shown wrong by any \
                 who kept their own count."
                    .to_string()
            )
        );
    }
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cargo test -p hornvale-book --lib the_doctrine_line -- --nocapture 2>&1 | tail -20`
Expected: FAIL — `reckoning_doctrine_line` not found.

- [ ] **Step 3: Implement `reckoning_doctrine_line`**

Add immediately after `reckoning_crisis_margin_line` (Task 3, Step 3):

```rust
/// One further Reckoning line for an organized culture at `Predictive`
/// rung (spec Task 2), thematically echoing LANG-39's own "Galileo cell"
/// framing WITHOUT routing through `ConflictState`/`conflict_of`
/// (decision ledger #3) -- `None` for a folk-only culture, matching every
/// other doctrine-gated render path's existing convention. `has_doctrine`
/// is always `true` at every real call site in this file today (a
/// `Predictive`-rung culture always has a doctrine, by `ladder_of`'s own
/// gate), so the `false` arm is unreachable through any live world --
/// kept and tested anyway (see the tests above), the same defensive
/// posture `reckoning_culture_lines`'s own `Unknown` arm already keeps.
fn reckoning_doctrine_line(autonym: &str, has_doctrine: bool, crisis_live: bool) -> Option<String> {
    if !has_doctrine {
        return None;
    }
    Some(if crisis_live {
        format!(
            "The {autonym}'s own priesthood taught wrongly, and could be shown wrong by any \
             who kept their own count."
        )
    } else {
        format!("None among the {autonym} have shown the priesthood's teaching false.")
    })
}
```

- [ ] **Step 4: Wire it into `reckoning_epoch`**

In the `if rung == hornvale_worldgen::LadderRung::Predictive` block Task 3
added, after the existing `if let Some(crisis) = crisis { margin.push(...)
}` block, add:

```rust
            if let Some(doctrine_line) = reckoning_doctrine_line(autonym, true, crisis.is_some()) {
                lines.push(doctrine_line);
            }
```

(`crisis` here is the same `Option<PredictionCrisis>` Task 3's block
already computed via `crisis_of`; `true` for `has_doctrine` because this
whole branch is already gated on `rung == Predictive`, which implies a
doctrine exists.)

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-book --lib the_doctrine_line -- --nocapture 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 6: Confirm the doctrine line appears on Task 3's own found seed**

Reuse the `(seed, kind)` Task 3 Step 5 found. Build that world, call
`reckoning_at(&world, at(36_525.0))`, and confirm the returned
`ReckoningEpoch.lines` contains "The {autonym}'s own priesthood taught
wrongly..." for that culture. If that seed's crisis culture does not hold
a doctrine (impossible per `ladder_of`'s own gate, but confirm rather than
assume), add a note; this should not happen.

Extend `a_crisis_fires_on_a_real_generated_sky`'s own final assertions
(added in Task 3, Step 5) in `windows/worldgen/tests/diachronic.rs` — reuse
its `(seed, kind, crisis)` tuple rather than re-searching:

```rust
    assert!(
        crisis.last_predicted != crisis.last_actual,
        "seed {seed} {kind}: a crisis's own last predicted/actual days must differ"
    );
    assert!(
        doctrine_of(&generated(seed), &kind).is_some(),
        "seed {seed} {kind}: a Predictive-rung culture with a crisis must hold a doctrine"
    );
```

- [ ] **Step 7: Run the full test suites**

Run: `cargo test -p hornvale-book -p hornvale-worldgen 2>&1 | tail -60`
Expected: PASS.

- [ ] **Step 8: Re-check the keystone fixture and artifacts**

Repeat Task 3 Step 6's checks (seed-42 diff, `lens_purity`, `git status`
on `book/src/gallery/`/`book/src/reference/`). If the doctrine line
changes seed 42's rendered Reckoning text (only possible if seed 42 has a
`Predictive`-rung culture at all — Task 3 already established this),
refreeze via `make rebaseline-goldens` and review the diff.

- [ ] **Step 9: `cargo fmt`, clippy, type-audit**

Run: `cargo fmt -p hornvale-book -p hornvale-worldgen`
Run: `cargo clippy -p hornvale-book -p hornvale-worldgen --all-targets -- -D warnings`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`

- [ ] **Step 10: Commit**

```bash
git add windows/book/src/lib.rs windows/worldgen/tests/diachronic.rs docs/audits/type-audit-report.md
# If Step 8 found drift:
git add cli/tests/fixtures/world-seed-42.json book/src/gallery/ book/src/reference/ book/src/laboratory/
git commit -m "feat(book): the doctrine-voice crisis acknowledgment, thematic not ConflictState-routed -- the-corrigendum T4"
```
