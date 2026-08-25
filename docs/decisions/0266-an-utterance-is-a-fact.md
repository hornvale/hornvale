# 0266. An utterance is a fact, an event is an entity, and roles are predicates on that entity

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0002](0002-domains-depend-only-on-kernel.md),
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-measurement-is-preregistered.md),
[0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md);
[The Interlinear](../../book/src/chronicle/the-interlinear.md)

In the context of `domains/language` being able to say exactly one kind of
sentence — "X is a Y" — and of saying it in Common by privilege rather than by
peer status, we decided that **the clause structure is fact-shaped**: a subject,
a predicate, an object, and adjuncts that bind registered predicates to
arguments. Common becomes one realizer among the generated tongues.

```
Fact        subject  predicate  object  place/day  provenance
ClauseSpec  subject  predicate  object  adjuncts   speaker features
```

## Why not a role ontology

The linguistically richer answer is a thematic-role inventory — agent, patient,
experiencer, instrument, goal. It was seriously considered and rejected, because
it costs a second roster that must agree with the concept registry forever with
nothing mechanical forcing agreement. That is the shape 0094 warns about and the
one The Drift had to consolidate away (`DelveRung` / `DelveZone`).

**The decisive evidence is that the project already had the better answer.**
`domains/history` reifies an occupation as an *entity* — `is-occupation` — and
hangs its roles off it as ordinary predicates: `occ-people`, `occ-site`,
`occ-founded`, `occ-ended`, and, exactly to the point, `occ-cause` and
`occ-ended-by`. Those two **are** thematic roles. They live in the one registry,
they are covered by the drift-checked concept page, and they cost no second
ontology. Adding "who caused this death" is `death-by`, parallel to
`occ-ended-by`.

## What this decision does NOT claim

An earlier draft argued that a role ontology would let the language "invent
detail the world lacks" — that the ledger has no slot for who caused a death.
**Nathan corrected it: the world is deterministic and knows why.** The argument
was collapsing three different things, and the distinction is the useful part:

| level | what it is | where it lives |
|---|---|---|
| 1. world truth | total, re-derivable from the seed | the simulation |
| 2. the committed record | what was written down, and so witnessable and transmissible | `Ledger` |
| 3. what a creature holds | a subset, distorted in transit | `vessel::Knowledge`, `windows/hearsay` |

Committing a `cause` predicate is what moves a cause from level 1 to level 2 — a
deliberate authoring act per event kind, not a blanket rule. **A creature speaks
from level 3.** "I don't know why he killed her" is not a construction to be
special-cased; it is the honest rendering of a level-3 model with a hole in it,
at the granularity of one predicate.

## The corroborating find

`domains/language/src/account.rs` already implements the epistemic account: the
four-filter stack a culture's knowledge of ground truth passes through —
lexicon, knowledge, ontology, valence. It operates on **facts**, treats `"is-a"`
as one predicate among many, and yields dispositions like
`Substituted { truth: "planet", theirs: "earth" }` when a culture carves the
world differently.

So `Frame::Classify` was a second, weaker encoding of a relation the same crate
already handled as data. Deleting the enum and keying constructions on a
predicate id makes `clause.rs` agree with `account.rs`.

## The cost we accept

**A static guarantee becomes a runtime one.** `Frame::Classify` made the
construction lookup total by construction; a `&'static str` key does not. The
mitigation is that both ends now reference `hornvale_kernel::world::IS_A` rather
than a literal, so they cannot drift independently — but a missing construction
is a panic, not a compile error, and the campaign records that plainly rather
than pretending otherwise.

**Recognition does not follow generation.** `parse_common` recovers the clause
skeleton and returns no adjuncts at all. That loss is documented on the function
itself and pinned by a 400-case round trip, and it is deliberate: parsing is
where controlled languages historically rot, and the campaign froze parsing
coverage rather than growing it without a corpus to measure against.
