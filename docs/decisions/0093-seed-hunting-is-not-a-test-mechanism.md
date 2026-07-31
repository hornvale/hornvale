# 0093. Seed-hunting is not a test mechanism

**Status:** Accepted (2026-07-31, G3) · **Decider:** Nathan · **Relates to:**
`docs/superpowers/specs/2026-07-31-the-weir-design.md` (§2 Stage 3, §4.1)

## The principle (Nathan, verbatim from the spec)

> A desirable but non-deterministic property is a **census** question
> (measure its frequency, pin the distribution); a *behavior* of that
> property is a **synthetic** question (hand-build a world that has it, test
> the behavior directly). A test that sweeps seeds to *find* a property
> instance is doing the census's job badly and the synthetic's job
> expensively.

Hornvale already has both proper instruments for exactly this shape of
question: a census measures how often a property holds across the
generator's real distribution and pins that distribution (decision 0011,
studies are data / metrics are code); a synthetic unit test hand-builds a
world with a committed fact already in the state the behavior needs, and
tests the behavior against it directly, at zero build cost and zero
dependence on which seeds happen to produce it. A test that instead sweeps
many seeds looking for one that happens to exhibit the property being tested
sits between the two instruments and is worse than either: it measures
nothing about *frequency* (a census's job) and it pays for dozens of full
world builds to exercise a behavior a single hand-built world would exercise
just as validly (a synthetic test's job) — and more expensively, since every
build not just the one that happens to qualify is sunk cost.

## Stage 3 — the first application

`windows/worldgen/tests/doctrine.rs`'s SOC-1 gate test built **61 Full
worlds** (seeds 1..=60 plus seed 1) at 3.25 s each — 203.0 s total, 87.0% of
it inside the test's own `generated()` helper — purely to *find* the ~11
seeds whose flagship culture happened to be organized (SOC-1) versus folk,
so the gate's both-arms behavior (`doctrine_from` returns `Some` for an
organized flagship, `None` for a folk one) could be asserted on a real
instance of each. The test's own panic message already prescribed the fix:
*"Add a synthetic-society unit test driving `doctrine_from` directly against
a hand-built world whose flagship's committed cult-form is 'folk' instead of
relying on this sweep."* Stage 3 implements exactly that: a hand-built
synthetic world with a `"folk"` cult-form fact committed for its flagship
(zero builds, deterministic negative-arm coverage), plus the live positive
arm kept at seed 1 (both gate arms) and one live folk smoke at seed 57 (the
known folk flagship, documented as epoch-sensitive; seed 56 at ratification —
drifted all-organized under The Wearing before merge — re-found at
57/bugbear by the bounded scan this record prescribes, which is the
mechanism working). The 60-seed hunt is deleted.

This is a **test-breadth reduction**, flagged for Nathan at G3 and ratified
under this principle: the sweep's property coverage ("doctrine iff
organized," measured across seeds) narrows to {seed 1, seed 57, synthetic}.
The wide sweep existed only to *find* a folk instance by brute force; the
synthetic world now supplies that instance by construction, at the cost of
no longer also incidentally sampling how often folk-vs-organized occurs
across seeds 1-60 — a frequency question this test was never actually
answering rigorously (it asserted on whichever instances it happened to
find, not on the distribution itself), and which the census family owns
when someone actually wants it.

## Consequences

- **The build-volume audit** (spec's out-of-scope followup register —
  "the broader build-volume audit, which other tests over-build") inherits
  this decision as its criterion: a test that builds N>1 worlds to find an
  instance of a property is a seed-hunting test until proven otherwise, and
  the fix is the same two-instrument split every time — a census entry if
  the frequency itself is the thing worth knowing, a synthetic world if only
  a behavior instance is needed.
- **What this does not license:** deleting or narrowing a sweep that is
  genuinely measuring a distribution (a census metric, a calibration sweep
  preregistered under decision 0016). The principle targets tests whose
  *only* use of multiple seeds is to locate an instance, not tests whose
  point is the distribution itself.
- A falsified expectation under a synthetic test is still a finding, not a
  reason to keep the sweep "just in case" — if the synthetic world's
  behavior ever disagrees with a live seed's, that is a bug in the synthetic
  fixture or the production path, not evidence the census-shaped sweep
  should have stayed.
