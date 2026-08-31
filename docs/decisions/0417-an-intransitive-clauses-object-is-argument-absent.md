# 0417. An intransitive clause's object is `Argument::Absent`

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0266](0266-an-utterance-is-a-fact.md),
[0416](0416-valence-is-a-closed-predication-strategy-taxonomy.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of `Clause.object: Argument` being mandatory while an
intransitive frame relates a subject to nothing, we decided to add
**`Argument::Absent`** rather than widen the field to `Option<Argument>`,
accepting a variant added for the *absence* of a role in an enum whose own
rule is that a variant is added when a role needs one.

## Why not the more honest type

`Option<Argument>` is the better type and it was measured rather than
dismissed: **105 full-literal construction sites across five files**, every
one found by the compiler, none subtle — against roughly five structural match
arms for the variant. The cost is not the edit; it is that 105 literal sites
are a large conflict surface against every live campaign branch, for a change
whose semantic content is one absent object.

## Why the fact-shape claim survives

Decision 0266's claim is that an utterance **is** a fact, and `Clause`'s shape
is how that claim is stated. A nullable object would weaken the shape for
every clause in order to serve one valence. It does not need to: **the kernel
already spells an objectless assertion.** `Fact.object` is mandatory too, and
`IS_PERSON`, `IS_BELIEF`, `IS_NEIGHBOR` and `TIDALLY_LOCKED` are all committed
today as `Value::Flag(true)`. The utterance is still a fact; `Absent` is the
object that fact carries. *The road is old* commits as *road old* with a flag,
objectless, which is why the property valence binds `Absent` as well.

## Why not `Argument::Flag(bool)`

Rejected on substance rather than passed over. `Flag(false)` with
`Polarity::Pos` and `Flag(true)` with `Polarity::Neg` would be two spellings
of one denial, and the round trip could not choose between them. `Absent`
carries no bit and cannot be ambiguous with polarity.

## The departure this variant makes, recorded rather than glossed

`Argument`'s standing rule is that a variant is added when a role needs it,
never speculatively. No role needs `Absent`; the absence of a role does. That
is a departure from the mechanism and not from the rationale, and it lives in
the variant's own doc comment so a later reader meets it where the code is.

## The site a compiler could not find

Three structural `Argument` matches needed a new arm and the compiler named
all three. A **fourth** site absorbed `Absent` silently: `realize_tongue_deep`
resolves its object through a pre-existing `other => …` catch-all, so a `sleep`
clause realized deep produced a wrong surface — an empty complement ordered
into the tongue's constituent sequence — with nothing to compile-error about,
because the catch-all predates the variant. Caught in review, fixed with an
explicit arm returning byte-identically to what the catch-all produced.

**The general form is worth the paragraph: "the compiler found nothing else"
is evidence about exhaustive matches only.** A catch-all older than the
variant converts a gap into a wrong answer, and the two are indistinguishable
from the build log.
