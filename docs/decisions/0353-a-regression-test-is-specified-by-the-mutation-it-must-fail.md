# 0353. A regression test is specified by the mutation it must fail

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot; adopted
mid-campaign after the second vacuous guard) · **Relates:**
[0016](0016-studies-preregister-hypotheses.md) (freeze the check before the
code that would move it),
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md),
[0350](0350-m-plus-n-is-proved-two-way-or-not-at-all.md),
[0011](0011-studies-are-data-metrics-are-code.md) ·
[The Offer](../../book/src/chronicle/the-offer.md)

In the context of a plan author writing the brief an implementer builds a
regression test from, we decided that **the plan must name the MUTATION the
test has to fail against, never only the property it should assert**,
accepting that a plan is longer and that the author must know the code well
enough to name a mutation that compiles.

## Context

Seven guards in one campaign asserted things that were true of *any*
implementation, and every one of them originated in spec or plan prose rather
than in implementer code:

- a test asserting a static fact about `required_properties` instead of
  exercising the query it was written to guard;
- a subset assertion over `offered_to = offered_by.filter(..)`, which re-proves
  an invariant of `Iterator::filter` and stays green for a predicate that
  returns `false` unconditionally;
- a source scanner looking for `"=> OfferedVerb::"`, a shape nobody would ever
  write, because the real table has to build a `BTreeSet`;
- two whole spec sections whose features could never fire;
- and two production paths held by nothing at all.

**Not one was found by reading.** Every one died to a command someone ran.
Re-reading cannot catch a vacuous assertion, because it checks a claim against
the model that produced it, and that model is precisely what is wrong.

## The rule

For a test whose job is to catch a specific regression, the specifying
document names a concrete mutation: the file, the line, and the edit — one
that **type-checks**, because a mutation that fails to compile says nothing
about whether an assertion would have caught the behaviour.

**A property can be asserted vacuously; a mutation cannot be failed
vacuously.**

## Consequences

- **A mutation must be checked for discriminating power, not assumed to have
  it.** The clearest instance in this campaign is a reviewer's own illustrative
  mutation, `Hearth → Bed`, which does not redden the test it was offered for:
  `the-fireside-bed` requires a `Hearth` in the same chamber, so the two anchor
  kinds are perfectly co-located in every real interior. The fixer found that
  by running the reviewer's mutation instead of trusting it, and used
  `Vessel` — whose roles exclude `Role::Hearthroom` outright — instead.
- **The mutation belongs in the test's own doc comment**, so the next reader
  can re-run it rather than re-derive it. Several tests in
  `windows/vessel/tests/suite/affordance.rs` carry theirs.
- This is a rule about *specifying* tests, not a mandate to run mutation
  coverage on every commit. `tools/seam-guard` remains the standing instrument
  for unguarded seams, and nothing schedules it.
