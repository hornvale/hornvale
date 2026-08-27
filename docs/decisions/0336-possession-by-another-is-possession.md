# 0336. Possession by another is possession

**Status:** Accepted (2026-08-26) · **Decider:** Nathan (autopilot, spec §8) ·
**Relates:** [0227](0227-possession-selects-a-body-it-does-not-mint-one.md),
[0228](0228-a-controller-is-a-parameter-of-the-tick.md),
[0229](0229-one-body-type.md) (The Hand's controller stack, which this makes a
second real user of) · [The Coercion](../../book/src/chronicle/the-coercion.md)

In the context of an imposed controller arriving for the first time — a body
driven by something other than the player — we decided that **it is the same
mechanism the player already uses to drive a body, and it carries the same
word, `possess`**, rather than a second word for a second concept.

## Context

The metaplan's own gate table named this row `dominated`, and nothing shipped
used that word in this sense: every `dominat*` already in the tree was
unrelated ("dominant species", "SYS-dominated", "dominates the night sky").
Measured against the incumbent before choosing: `possess` appears 747 times in
shipped code and 1,642 times in prose against `dominated`'s zero in this
sense; `ridden`/`rider` appear only in doc-comment prose (46/97 times), never
as an identifier. `usurp` was proposed during the brainstorm that objected to
`dominated` and rejected on the same evidence — the count settled it and the
reasoning behind the proposal did not.

The deeper argument is not lexical. Player-possession and possession-by-
another are the same mechanism at the type level: both replace a body's
decision procedure with an external agent's ([`Controller`], decision 0228);
both leave the body co-present with its own drives and affect rather than
displaced (decision 0226); and both make the acts the body's, never the
driver's (decision 0168). The only difference is who the external agent is —
a fact `BodyState::PossessedByAnother`'s own name carries (decision 0338),
not a fact that wants a second verb. Introducing one would be the same
accretion `cli/tests/suite/lexicon_guard.rs` exists to catch, arriving from
the opposite direction: not a word smuggled in through convergent naming, but
one deliberately chosen for a distinction the mechanism does not have.

## The rule

`possess` is the only verb for taking a body's decision procedure, whether the
taker is the player (the CLI's `possess --seed …` entry point, unchanged) or
an imposed controller opened from inside a session (`!possess`/`!unpossess`,
this campaign). `dominate`, `usurp`, and `control` do not name this mechanism
anywhere the campaign wrote prose or code, and a later campaign reaching for a
second word here should read this record before minting one.

## Consequences

- `BodyState::PossessedByAnother` (`windows/vessel/src/gate.rs`) and the
  `possessed-by`/`possession-ended` predicates (`windows/vessel/src/
  session.rs`) are the only new surface this decision touches; both use
  `possess`'s root, never `dominat*`.
- `docs/superpowers/specs/2026-08-19-the-bridle-metaplan.md`'s own gate table
  and arc table, which predate this decision, are corrected in the same
  commit that records it — a spec that still renders the retired word would
  contradict a decision citing it.
- `possess` is reserved for body-control specifically; a later inventory
  system's have-a-thing sense is `carried`, recorded in the spec (§2.2) rather
  than here because no `possess`-as-owns collision exists yet to decide
  between.
