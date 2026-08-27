# 0297. A predicate's valence is stated once, and Common's parts are selected from it

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0296](0296-tense-is-stated-never-derived.md),
[0286](0286-each-realizer-ignores-part-of-the-clause.md),
[0266](0266-an-utterance-is-a-fact.md),
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md);
[The Inquest](../../book/src/chronicle/the-inquest.md)

In the context of adding a transitive clause — one whose predicate is an act
with an actor and a patient, rather than a classification — we decided that
**a predicate's argument structure is a property of the predicate, stated once
in `PREDICATE_VALENCE`, and Common's part list is SELECTED from that statement
rather than written beside it.**

## The problem this avoids

A tongue has no part list. Common has one, and before this campaign the shape
of a predicate was encoded *incidentally* in Common's spelling: a construction
carrying `Part::Copula` was nominal, one carrying `Part::Verb` would be
transitive. That is a fact about English, sitting where a fact about the
predicate belongs.

Two repairs were on the table and both were worse than the one taken. Letting a
tongue read Common's construction table structurally would make Common a
privileged path again, quietly undoing decision 0286's claim that it is one
realizer among the tongues. Writing a shared valence table *beside* Common's
part lists would create two statements of one fact, which decision 0261 says
must then carry a two-way agreement test — a test that exists only to detect
its own rot.

The shipped shape removes the duplication instead of guarding it.
`common_constructions()` builds each construction's parts **from**
`predicate_valence(id)`, so there is exactly one table and **no agreement test
to write and no agreement test to rot**. `grammar.rs` — the tongue realizer —
references neither `Part` nor `Construction`, so 0286's peer relationship holds
by construction rather than by convention.

## `Valence` is not the `Frame` enum The Interlinear deleted

This is the confusion the record exists to pre-empt, and the distinction is
many-to-one.

`Frame::Classify` was one variant **per relation**, and the construction lookup
was **keyed by it**, so every new predicate meant a new variant — which is why
it was deleted. `Valence` sorts predicates **into argument structures many of
them share**: `eat`, `kill` and `know` are one `Transitive` between them,
adding no variant. Nothing is keyed by `Valence`; the construction lookup stays
keyed by predicate id.

The tripwire is stated in the enum's own doc as well as here, because a
distinction that lives only in a reviewer's head is eroded by the next
campaign: **a campaign that finds itself adding a `Valence` variant per
predicate has rebuilt `Frame` and should stop.**

## Consequences we accept

**One part list per valence.** Every transitive predicate gets the same Common
surface. A predicate that needs its own surface at an existing valence earns a
per-row override, and the test that pins this selection
(`common_parts_are_selected_by_the_predicates_valence`) reddens at exactly that
moment rather than letting the override drift in silently.

**A predicate with no `PREDICATE_VALENCE` row cannot be realized at all.**
`realize_common` panics, matching 0286's rule for an unknown predicate: a
missing construction is an authoring hole, and returning a gap would assert
something false about the world. `know` is in that state deliberately — the
grammar for *"I didn't know her"* is built and the sentence is unsayable, which
is the lexical-versus-grammatical distinction `sentences/` exists to keep
visible.
