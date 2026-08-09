# 0109. A population claim is asserted in the commit gate, over the committed fixture

**Status:** Accepted (2026-08-07, G3) · **Decider:** Nathan · **Relates:**
[0032](0032-calibration-loads-the-census-fixture.md),
[0093](0093-seed-hunting-is-not-a-test-mechanism.md),
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md)

In the context of writing the gate-side assertions for `toponymic-roots-won`
and `crisis-fires` — both population claims (a rate, a coverage maximum) read
off the census's 1,000-world fixture — we decided that **a population claim
asserts in the commit gate, over the committed fixture, every commit** —
reconciling a tension in decision 0097's own text with the practice
decision 0032 already established and twenty-four existing tests in
`windows/lab/tests/calibration.rs` already assumed.

## The apparent tension

0097 §2 says the gate asserts only claims robust to another campaign's true
change, and reserves census-measured rates for the census's own review
(`make lab-diff`) rather than a gate assertion. Read narrowly, that could be
taken to mean a population claim is asserted only when the census itself
regenerates — i.e., never in the ordinary commit gate, only at the rare
moment of a fresh regen. But 0097 §4 says the opposite is the actual risk:
*"a census-measured claim must have its generator paired with its verifier
… an unpaired census claim scores as unchecked no matter how large its
sample."* The Siding found the census stale for 139 commits while every gate
ran green precisely because nothing in the gate ever checked the fixture
against anything. Read together, §2 says *don't recompute the census in the
gate*; §4 says *do verify the fixture in the gate*. Those are compatible, but
only if "asserted in the gate" and "measured live in the gate" are read as
two different things.

## The decision

The gate assertion loads the **committed** `rows.csv` (decision 0032's
mechanism, at its existing near-zero cost) and asserts the population claim
against it — a coverage floor, a rate bound, an invariant — every commit,
without rebuilding a single world. This satisfies 0097 §4's pairing
requirement (the verifier runs every time, in the gate) without violating
0097 §2's cost discipline (the gate never recomputes the census itself) and
without re-litigating 0093 (the underlying property is still measured by the
census, never re-hunted by the test). The freshness half of the pairing —
that the *fixture itself* has not drifted from what a live build would say —
is a separate concern, closed by decision 0111's tripwire, not by this
decision.

Twenty-four gate-resident tests in `calibration.rs` already did exactly this
before this campaign named it (`head_deity_is_eternal_exactly_when_tidally_
locked`, `band_count_matches_the_known_function_of_rotation`, and twenty-two
more) — this record ratifies an existing, load-bearing practice rather than
introducing a new one, and gives Tasks 8–10's new assertions
(`every_hydro_variant_is_reachable_somewhere_in_the_census`,
`some_census_world_steeps_every_toponymic_concept`,
`a_prediction_crisis_occurs_and_the_census_reports_its_rate`) the same
footing.

## Consequences

- **A population claim's failure mode is legible.** `a_prediction_crisis_
  occurs_and_the_census_reports_its_rate` fails loudly if `fired == 0` and
  explicitly forbids the sanctioned repair-that-isn't: *"Do NOT weaken
  `PREDICTION_TOLERANCE_FRACTION` or `CRISIS_MISS_RUN` to force a hit; those
  are the spec's own considered values."* A rate assertion that can be
  silenced by retuning the constant it measures is not a population claim
  anymore, it is a golden dressed as one.
- **This does not authorize asserting a rate at the threshold the way 0097
  warns against.** `crisis-fires` at 65.9% and `toponymic-roots-won` at
  13.1%-full are both far from their boundary; a population claim minted
  near its own threshold still needs 0097 §2's judgment about whether it
  belongs in the gate at all.
- **The tripwire (decision 0111) is this decision's precondition, not its
  consequence.** Asserting against a fixture every commit is only sound if
  something bounds how stale that fixture can get; this record assumes 0111
  ships alongside it, per the spec's Stage 2 gate ("no check moves until
  this stage's mutation evidence is recorded").

## See also

`docs/superpowers/specs/2026-08-07-the-assay-design.md` §4.1, §5;
decision 0032 (the mechanism this decision generalizes to population
claims); decision 0016 (preregistration — a population claim's threshold is
still fixed before the code that would move it).
