# 0110. The census is the suite's shared world-building pass

**Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0032](0032-calibration-loads-the-census-fixture.md),
[0093](0093-seed-hunting-is-not-a-test-mechanism.md)

In the context of finding that 224 gate tests build a world at
`Settlements` or `Full` depth while only 26 read a committed fixture — and
that the census (`windows/lab/src/runner.rs:164`) already builds 1,000
worlds and maps ~200 extractions over each one, at a cost 24 gate-resident
checks could ride for less than three of the tests it replaced cost alone —
we decided that **a check that is a function of one world belongs in the
census as a metric, not in a test that builds its own world.**

## The finding

This campaign adds no new architecture. `BuiltView::build_to`, `MetricValue`
(including `Flag(bool)` for invariants), `Extractor::rung()`, and
`load_rows(committed rows.csv)` already existed and already carried the
weight — decision 0032's calibration suite already loads a committed
fixture and asserts 24 checks against it for less than the 1-second-floor
cost decision 0032's own record measured. The gap was not a missing
mechanism, it was a missing habit: three seed-hunting tests
(`hydro_witness`, the exposure toponymic sweep, the diachronic crisis hunt)
each built their own worlds from scratch to answer a question the shared
build already had the data to answer, at a combined cost the spec measured
as an order of magnitude more expensive than the twenty-four census-resident
checks put together.

## The decision

A consistency check whose only input is a single generated world — an
invariant, a rate, a coverage claim — is registered as a `Metric` against the
existing study rather than written as a standalone test that builds its own
world(s). The world is already held by `build_row` for every other metric
riding the same census pass; a new metric costs the census almost nothing to
add and the gate nothing at all, because the gate reads the committed
`rows.csv` (decision 0032) rather than rerunning the census.

This is deliberately narrower than "move every world-building test into the
census." Six classes of check cannot move (spec §6): byte-identity and
determinism (needs the same seed built twice, compared); prose and
rendering (`hornvale-book`'s 41 tests assert rendered strings, better served
by the existing drift check than a digest column); save-format round-trips;
CLI/REPL surface (needs process invocation); action sequences
(`hornvale-vessel` asserts what holds after a scripted walk, not a per-world
scalar); and pinned regimes (the census has exactly one `pin_set`, so a
pinned claim like `tectonic::single_craton_...` stays live until a second
`pin_set` exists). This decision targets the fifty-fourth of the fifty-seven
seed-looping tests the campaign's audit classified as *not* a hunt in the
narrow 0093 sense but still a function of one world at a time — the
population this campaign's Stage 3–4 tranche began migrating, not completed.

## Consequences

- **The migration is a follow-on, not a one-shot.** This campaign ships the
  mechanism (already existing) plus three retirements. The remaining
  ~224-test migration is explicitly out of scope and carried into the idea
  registry, informed by what this tranche measured rather than guessed in
  advance.
- **A test that builds N > 1 worlds to answer a one-world question is now a
  named smell**, in the same register as 0093's seed-hunt: not wrong by
  construction, but requiring a reason it is not a census metric instead.
- **This is downstream of, and does not relitigate, 0093.** A test that
  sweeps seeds to *find* an instance stays 0093's problem (routes to census
  coverage or synthetic); a test that builds *one* world per seed to check a
  property of *that* world is this decision's problem (routes to a
  per-world metric, read at 1,000-seed scale instead of the test's own
  smaller sweep).

## See also

`docs/superpowers/specs/2026-08-07-the-assay-design.md` §2, §3.3, §6, §7;
the Task 1 audit (committed as data, per the spec's Stage 1) for the
224/251/2/26 build-depth breakdown by crate.
