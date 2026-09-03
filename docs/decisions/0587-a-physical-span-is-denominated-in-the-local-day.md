# 0587. A span that models a physical duration is denominated in the LOCAL day

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) · **Campaign:** The Plumb

## Context

A standard day is a fixed 100,000 ticks (decision 0186). A world's **local**
day is its rotation period, and `RotationPin::PeriodHours` admits **4 to 100
standard hours** — so a legal world's day ranges from about a sixth of a
standard day to more than four of them.

`REST_BOUT` was `TICKS_PER_STD_DAY / 4`: a quarter of a *standard* day, on
every world. Its own doc asserted a calibration — one rest must carry a body
clear of the hysteresis band the drive re-engages inside, so the repayment must
exceed `HYSTERESIS_H` (0.1). Because the repayment works out to
`REST_FALL * 0.25 / L` in local days, that claim held only while `L < 1.25`
standard days. **On any world turning slower than about 30 hours a rest repaid
0.03 against a floor of 0.1** — the drive that proposed the rest was still
engaged when the body got up, restoring by a constant the seven-minute dozing
The Wicket's Task 8 removed by a span.

## Decision

A constant that models a **physical duration** — how long an act takes, how
long a body sleeps, how far a search over a daily signal must reach — is
denominated in the **local** day, reached through `clock::ticks_per_local_day`,
which returns the base standard-day rate when there is no day to divide.

A constant that models an **algorithm's own quantity** — an iteration budget, a
capacity, an array length — stays as it is. The line between them is not where
a constant is declared or what it is called; it is whether the quantity it
measures is a duration in the world.

`REST_BOUT` is converted. Every other span found on the wrong side of that line
is a **finding**, published under decision 0586's Fidelity table and left for
Nathan: `SLEEP_BOUT`, and — the ones that matter more — `SCAN_LIMIT` and
`ONE_DAY` inside `next_awake_day`, which cap and terminate the wake search in
standard days, and `WAKE_SCAN_STEP`, which samples a locally-periodic signal at
a fixed standard-day rate (about 3.3 samples per local day at the 4-hour legal
minimum, under a doc claiming it is fine enough to catch a crepuscular
creature's dawn band).

## Consequences

- The calibration now holds across the whole legal range: the repayment is
  L-invariant at ~0.125 against the 0.1 floor from 4 hours to 100.
- **Fixing one of two consistently-wrong quantities exposes the other, and that
  is not a reason to leave both wrong.** With the sleep path still capped at
  one standard day, a converted rest on a 100-hour world in permanent night
  runs 1.041660 std days against a sleep of 1.000000 — an inversion of the
  words' plain meaning. It was measured, not predicted, and it is shipped
  deliberately: the conversion fixes every rest on every world slower than
  ~30 hours, while the inversion needs the pin extreme *and* permanent night
  and costs an hour of ordering. `ONE_DAY` is the constant at fault, and it was
  already wrong before this campaign — a creature in permanent night on a slow
  world always slept exactly one standard day and woke into more night.
- The inversion is held by a **running** test asserting the current, wrong
  ordering, so the day someone converts the sleep path the tree goes red and
  the test must be deleted. An `#[ignore]`d test was written first and
  rejected: nothing runs it, nothing validates the roster rows that cite it,
  and a registration a reader takes for coverage is the `STALE-DECL` failure
  `CLAUDE.md` already warns about for the seam-guard roster.
- **A behaviour change on every world whose day is not exactly one standard day
  moves byte-goldens that the artifact machinery cannot see.** Seed 42's local
  day is 87,988 ticks, so the converted span differs ~12% from the old flat
  25,000: two possession transcripts moved, and so did
  `windows/lab/tests/fixtures/affect-trace-seed-42.txt` — 89 of ~410 lines,
  22 affect labels, 16 of them into `Lost`, which feeds `health.rs`'s distress
  classification. Neither `make rebaseline` nor the `docs/generated-paths.txt`
  drift check can reach a byte-golden, and `gate-commit` does not run that test
  (`affect_trace` has 0 entries in the sub-floor roster). **Run byte-golden
  tests explicitly after any behaviour change; never infer their state from the
  artifact diff.** The move was adjudicated with a measured breakdown, in the
  style the same file's two earlier moves established.
