# 0088. The suite watches its own clock — a failing test, not a dashboard

**Status:** Accepted (2026-07-30) · **Decider:** Nathan · **Relates to:**
[0040](0040-nextest-is-the-gate-runner.md),
[0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md),
[0087](0087-a-benchmarks-timings-are-a-record-not-a-golden.md)

Decision 0040 adopted nextest to bring the commit gate "under 4 min," measured
at 234 s. Measured on a quiet Mac on 2026-07-29, `make gate` took **934.5 s** —
roughly 4× its documented budget, of which contention accounted for six
seconds. The suite simply grew. `docs/timings.md`, the ledger whose own header
says it exists because "a whole suite creeping 65 s → 43.5 min lived unnoticed
until it hurt," carried **zero rows labelled `gate`**. The instrument existed
and had never been wired to the most-run expensive command in the repo.

The Timekeeper wired it. Two questions had to be settled first, and both are
about *shape*, not thresholds — the thresholds are expected to move, these are
not.

## The ruling

**1. The review loop is a failing test plus `git log` archaeology. No
dashboard, no Jenkins, no time-series store.** Asked directly how he wanted to
learn that the suite got slower, Nathan answered: "Test failing on a duration
shift is fine, or just 'wtf, it's still running?' followed by git
archaeology." That forecloses the dashboard question outright, and it has a
design consequence that is the whole reason this is a decision rather than a
preference: **the baseline must be committed.** A ledger in `target/`, a
sqlite file, or a hosted service all satisfy "record the durations" and none of
them satisfy `git log -p`. `docs/timings/test-baseline-<host>.tsv` is one file
per host, rewritten in place each `make ci`, holding only the present — git
already holds the history, and reading that history *is* the review loop.

The corollary bites: a baseline whose rows churn on noise is not archaeology,
it is a wall of diff. This is why the file folds every sub-1 s test into a
single `<below-floor>` row and applies a 20 % hysteresis deadband before
storing a new value. Those two mechanisms are not tuning; they are what makes
the committed-file decision actually deliver the thing it was chosen for.

**2. Observation and enforcement are separate knobs.** nextest's
`slow-timeout.period` *marks* a test slow; `terminate-after` *kills* it. The
`ci` profile sets `period` only, and `terminate-after` is deliberately unset.

This is the slow-query-log / statement-timeout distinction, and conflating
them means a contention blip destroys a run instead of reporting one. The same
split governs the alarm: `make ci` always *records*, and enforces only when
there is no evidence the box is contended. A run that cannot be trusted still
produces data; it just does not get to fail the build or overwrite the
reference.

## Consequences

- **A red run never becomes the new baseline.** `make ci` records only when
  both the suite and the alarm are green. Re-recording a regression is
  therefore a deliberate act — you fix it, or you re-record in the same commit
  that caused it and say so. The alternative, discovered by mutation testing
  during execution, is a one-way ratchet: the first recipe drafted for this
  campaign ran the recorder *before* the alarm, so every run compared against
  itself and the alarm could never fire.
- **The tolerances are chosen, not derived, and this decision does not ratify
  them.** `PER_TEST_FLOOR_SECS = 5.0`, `PER_TEST_MULTIPLE = 2.0`,
  `SUITE_TOLERANCE = 0.25`. Spec assumption A1 says so plainly: a campaign
  whose thesis is "derive the limits from the process's own history" must
  hand-set its first constants, because there is no history until it creates
  some. The hysteresis parameters *were* derived, from measured jitter (median
  16.9 % across all tests, 3.8 % at ≥ 1 s, 2.9 % at ≥ 5 s), and the same method
  applies to the rest once several runs exist.
- **Nothing invokes `make ci` automatically.** CI is manual-only by decision
  [0042](0042-github-ci-is-manual-only.md); this decision does not change
  that. The
  alarm is a thing you run, which is the same standing every other gate in
  this repo has.
- **The suite alarm compares the id-intersection, not raw totals.** Raw totals
  would trip on pure test-count growth — this repo adds tests constantly — and
  an alarm that fires during normal development gets tuned out. It also cuts
  the other way: removed tests can shrink a raw total while the surviving
  tests got slower.
- **This is a build measurement, and the wall-clock ban does not reach it.**
  The Constitution forbids wall-clock time in the *world*. `windows/lab/src/timings.rs`
  measures the build, tagged `diagnostic-value` in the type audit for the same
  reason.
