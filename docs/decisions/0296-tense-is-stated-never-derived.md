# 0296. Tense is stated, never derived — a clause has no clock

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0286](0286-each-realizer-ignores-part-of-the-clause.md),
[0266](0266-an-utterance-is-a-fact.md),
[0126](0126-fact-day-is-a-typed-world-time.md);
[The Inquest](../../book/src/chronicle/the-inquest.md),
[The Scarf](../../book/src/chronicle/the-scarf.md)

In the context of `Clause` gaining a `tense` field so the world stops asserting
that a settlement **is** the home of a people that left six hundred years ago,
we decided that **the caller states the tense and the realizer surfaces what it
is told** — never that the clause works it out for itself.

## Why this is a decision and not an implementation detail

Every feature the clause carried before this campaign is a property **of** the
clause. `number` is a fact about the referent; `definiteness` is a fact about
how it is being introduced; `evidential` is a fact about the speaker's access;
`polarity`, added in the same campaign, is a fact about whether the clause
asserts or denies. Each is fully determined by the clause's own content, and
each is therefore recoverable from the clause alone.

Tense is not. Tense is a **relation to a moment outside the clause** — it is
the first clause feature in this project requiring a deictic centre. To derive
it, a `Clause` would need to know when it is being uttered, which means a
`Clause` would need a clock.

The alternative was concrete and looked convenient: the occupation's own
`occ-ended` fact carries a `WorldTime`, so a realizer handed the fact could
compare it against "now" and pick the copula itself. That is the design this
record refuses. It would put a time source inside the one type in
`domains/language` that is deliberately world-free, and it would make the
correct rendering of a clause depend on when the rendering happened rather than
on what the clause says.

So the relation is computed **by the caller**, which is the only party holding
both instants: the fact's `WorldTime` and the utterance's. It arrives at the
realizer already reduced to `Present` or `Past`.

## The corollary, ratified with it: the tense host is the verb

This is why transitivity shipped **alongside** tense rather than after it, and
the sequencing is the load-bearing half of the record.

Noun class marks the complement. The evidential marks the copula, or — under a
zero copula — the predicate nominal. Tense wants a **verb**, and a zero-copula
nominal clause has none. Several drawn tongues have no copula at all, so
`X is a Y` in the past would have had nothing to bind to, forcing an arbitrary
rule about affixing a tense marker onto a noun and calling it tense.

`ConstituentOrder` was already `Sov/Svo/Vso/Vos/Ovs/Osv` — built for S-V-O,
with the copula standing in for V. A transitive clause supplies a real verb and
a natural host. **The extra feature made the core feature simpler rather than
harder**, which is the argument for the larger scope and the reason the two are
one campaign.

For a nominal clause in a zero-copula tongue, past tense falls to the predicate
nominal, mirroring exactly what the evidential already does in that position.
That is a precedent followed, not a rule invented.

## Consequences we accept

**A caller can state a tense that contradicts the world, and nothing objects.**
`realize_common` will faithfully say *was* about a settlement still standing if
handed `Tense::Past`. There is no cross-check and there cannot be one at this
layer: the clause does not know what world it came from. The mitigation is that
the derivation lives at the construction site, where the facts are, and the one
live site asserts its own result — `windows/almanac`'s interlinear demo selects
an occupation that ended and asserts the sentence says `was` and does not say
`is`.

**Present is the zero member and only past is drawn.** No present marker is
invented in any tongue, which matches what `paradigm.rs` already draws
(`draw_paradigm_affix_proto(seed, family, "tense", "past")`) and the
typologically standard shape. Polarity takes the same shape: `negative` is
marked, positive is zero.

**A future campaign wanting automatic tense adds a caller-side helper**, never
a clock inside the clause. Reichenbachian speech/reference/event assignment
(`LANG-narrative-tense`) is exactly such a helper, and it is a strictly
caller-side computation under this record rather than a change to it.
