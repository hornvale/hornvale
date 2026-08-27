# 0286. A language-neutral clause states more than any one realizer surfaces

**Status:** Accepted (2026-08-26) · **Decider:** Nathan · **Relates:**
[0266](0266-an-utterance-is-a-fact.md),
[0011](0011-studies-are-data-metrics-are-code.md),
[0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md);
[The Scarf](../../book/src/chronicle/the-scarf.md),
[The Interlinear](../../book/src/chronicle/the-interlinear.md)

In the context of `TongueClause` being deleted and `ClauseSpec` renamed
`Clause`, so that Common and every generated tongue realize **one** structure,
we decided that **each realizer ignoring some of that structure's features is
the law, not a defect**.

```
feature       | Common               | a tongue
--------------+----------------------+----------------------
predicate     | construction table   | asserted == IS_A
subject       | Name | Pronoun       | Name; Pronoun GAPS
object        | all 4 Arguments      | all 4 Arguments
number        | copula + plural      | IGNORED (today)
definiteness  | determiner slot      | IGNORED (always)
evidential    | IGNORED (today)      | predicate-final mark
adjuncts      | per-role surface     | argument-order only
```

## Why this needs ratifying rather than merely documenting

The two gaps are a **matched pair**, and reading them separately invites a
plausible wrong repair. "Common can't do evidential" and "the tongue can't do
number" each look like a hole to be filled, and the obvious way to fill the
second is to teach a tongue to render English number — which would put the
author's register back inside the tongue, the exact leak decision 0266's clause
layer exists to close.

Read as a pair they are the correct shape. A language-neutral clause is not the
intersection of what its realizers can say; it is the union of what a speaker
might mean. Each realizer surfaces what its own grammar has and drops the rest,
and a feature reaching a realizer that has nowhere to put it is not a bug in
either.

Two of the `IGNORED` cells are marked `(today)` on purpose. `number` on the
tongue side is precisely what `paradigm.rs`'s already-drawn `number_depth` and
`draw_paradigm_affix_proto` consume; wiring them is a future campaign. The
Scarf makes `number` **reach** the tongue realizer and stop there.
`definiteness` is marked `(always)`: a tongue with no determiner system has
nowhere for it to go and never will, which is a fact about that grammar rather
than a deferral.

Both halves now carry a test, so a later campaign that changes either has to
delete an assertion deliberately rather than drift past it:
`common_ignores_the_evidential` and
`a_tongue_ignores_number_and_definiteness` in `domains/language/src/clause.rs`
and `grammar.rs`.

## The companion clause: the INPUT is symmetric, the OUTPUT is not

Ratified with it, because it is the one that would be "fixed" next:

```
realize_common(&Clause, &CommonVocabulary) -> String                    // total
realize_tongue(&Clause, ...)               -> Result<String, TongueGap> // partial
realize_tongue_deep(&Clause, ...)          -> Result<String, TongueGap> // partial
```

Both realizers take the same `&Clause`. They do **not** return the same type
and must not be made to. Common is the author's register and
`CommonVocabulary::word_for` is total, so a gap can only ever arise on the
tongue side — which is what makes a `TongueGap` mean *something true about the
world* ("this people has no word for the sea") rather than an authoring hole.
Introducing a `CommonGap` for symmetry's sake would destroy that reading, and
the asymmetry is therefore load-bearing rather than an accident of who wrote
which function first.

The same principle decides the two refusal sites the collapse created. An
**unknown predicate** panics, matching `realize_common`: a missing construction
is an authoring hole and returning a gap would assert something false about the
world. A **`Subject::Pronoun`** returns a gap, and its reason says plainly that
the tongue has no pronoun inventory — not that the people cannot re-mention.

## The cost we accept

**A feature can sit on the clause with no realizer reading it, indefinitely,
and nothing will complain.** `definiteness` has been in that position since The
Interlinear and `evidential` is now in it for Common. The mitigation is the two
tests above and this record; there is no mechanism that counts unread features,
and inventing one would be a roster that must agree with the realizers forever
with nothing forcing agreement — the shape 0094 warns about.
