# 0094. A deliberate duplicate shares its *roster*, never its *derivation*

**Status:** Accepted (2026-07-31) · **Decider:** Nathan · **Relates to:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-measurement-is-preregistered.md);
[The Watershed](../../book/src/chronicle/the-watershed.md) (followup F8)

In the context of a Laboratory metric that re-implements production rules **on
purpose** — because a check that called the code under test would assert
nothing — facing the fact that such a duplicate silently falls behind whenever
the production side gains a rule, we decided that **a deliberate duplicate must
share the *roster* of things it answers for, while both sides keep computing
their own answers independently** — accepting that this costs a shared
declarative list, and that the list becomes a small coordination point two
otherwise-independent implementations must both edit.

## The principle

Split the duplication in two:

- **The roster** — *what classes of thing must be answered for* (which
  exposure classes exist; which phenomenon kinds gloss). **Shared.** One
  declarative list, read by both sides.
- **The derivation** — *what the answer is for each* (does this cell steep
  this crop; which concept does this kind gloss to). **Duplicated,
  deliberately, and never shared.**

Omission then becomes a compile error or an obviously-empty branch, while the
property the duplication exists to buy — two genuinely independent
computations — is untouched. A second opinion that agrees by construction is
not a second opinion; a second opinion that can silently *skip a question* is
not one either.

## Context

`windows/lab/src/metrics.rs::independently_steeped_concepts` duplicates
`hornvale_worldgen::exposure_of`'s Steeped rules. The duplication is correct
and its doc comment says why: calling `exposure_of` would make
`exposure-sound-*` an echo rather than a check.

It has now fallen behind **twice in eleven days**, both times in the same
shape:

- The Wearing added toponymic exposure rules; the duplicate did not learn
  them. Repaired at its Task 11d (`252 -> 1000` true).
- The Watershed added staple rules; the duplicate did not learn them.
  `exposure-sound` read **false on ~75% of all worlds** — goblin 233/1000
  true, kobold 241/1000 — while the worlds were correct throughout.

Neither lapse turned anything red. The metric ran, was drift-checked, appeared
in the published census, and reported a defect in itself as a defect in the
worlds. It was found because a campaign whose *central mechanism* is
exposure-gated naming went looking — which is to say, by the campaign least
able to treat the reading as background noise, and only after it had nearly
regenerated a census against it.

The cost is not the repair; the repair is an afternoon. The cost is that
**every measurement taken in the interval is untrustworthy and nothing says
so**, including measurements a campaign froze a hypothesis against under 0016.

## Why not the alternatives

**Delete the duplicate and call the production code.** Rejected: that is what
makes the check vacuous, and it is the reason the duplicate exists. Both
lapses would have been "fixed" by a check that could never have caught
anything.

**A count-parity test** ("both sides consider the same *number* of rule
classes"). Cheap, and catches omission. Rejected as the primary answer because
it catches nothing about *correctness* and reads as coverage — the failure
mode this decision exists to close. Acceptable as a stopgap where a shared
roster is genuinely impractical, never as the destination.

**Rely on the token guards.** Rejected on evidence. `heavy_tier.rs`'s own doc
already states the limit: a token guard proves a deferral is *findable*, never
that it is still *true*. Both lapses were findable and neither was found.

## Scope

Applies to any check the repo duplicates on purpose. Known instances beyond
the one that motivated this:

- `phenomenon_concept` — three copies (`windows/lab`, `cli/tests`, and
  worldgen's private original), each deliberately re-deriving the same mapping
  from public constants.
- `admissible_reflexes` in `domains/language/tests/speakable_properties.rs`
  and its near-twin in `cli/tests/branches_identity.rs`, whose doc already
  warns "the two must not drift."

None is required to convert immediately. What this decision settles is the
*shape* of the answer when one is written or repaired, so the next person does
not choose between "share everything" (vacuous) and "share nothing"
(silently divergent).

## Consequences

- A campaign adding an exposure class edits one roster and implements two
  derivations, rather than editing one implementation and hoping.
- The roster is a coordination point, and a small one: it names classes, not
  logic, so it moves far less often than either derivation.
- This does not make a duplicate *correct* — only non-omitting. A wrong answer
  to a listed question is still exactly as detectable as before, which is the
  point.
