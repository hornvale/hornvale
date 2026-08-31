# 0416. `Valence` is a closed predication-strategy taxonomy, not an open frame list

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0297](0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md),
[0326](0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md),
[0330](0330-the-corpus-score-is-demonstrated-not-declared.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of `domains/language`'s `Valence` growing from two variants to
five in a single campaign — the exact motion decision 0326 warned about — we
decided that **`Valence` enumerates predication *strategies*, not predicate
frames, and the list is closed at five**, accepting that a language whose
predication strategy Stassen's typology does not name would need this decision
reopened rather than extended.

## Why the growth is not 0326's failure mode

0326 refused a `Valence::Sentential` and left a warning in its own text,
verbatim: *"if a future campaign finds itself adding a variant per predicate,
it has rebuilt `Frame` and should stop."* Three new variants in one campaign
is precisely the shape that warning describes, so the burden is on this
decision to say why it is a different thing.

`Nominal`, `Property`, `Locative` and `Intransitive` are **exactly** Stassen
(1997)'s four intransitive predication strategies — nominal, adjectival,
locational and verbal — plus `Transitive` for the two-argument case. The list
is closed by an external typology rather than by this project's taste. A fifth
*predicate* is one row in `PREDICATE_VALENCE` (0297's mechanism, unchanged: a
predicate's valence is stated once and Common's parts are selected from it). A
fifth *strategy* would need Stassen to be wrong.

The distinction is checkable rather than rhetorical, and the check is the
variant count itself: `Valence` has five variants and the enum's own doc names
the taxonomy as the reason there will be no sixth. A campaign that finds
itself writing `Valence::Sentential`, or `Valence::Ditransitive`, or a variant
named after a verb, is in 0326's failure mode and this record is what tells it
so.

## One uninflected predicate slot serves two strategies

The spec that opened this campaign gave the property valence a
`Part::Complement` and no predicate slot, which meant a property clause had to
carry the property word in **both** the predicate and the object slot — the
right string produced by stating one word twice. That is The Mortise's `m02`
trap in a new costume, and it was caught while verifying a task brief against
the code rather than by re-reading the document.

What ships instead is one new part, `Part::PredicateWord`: the clause's own
predicate resolved through the realizing vocabulary and **not** inflected,
because neither a property word nor a locative adposition takes tense, number
or polarity. The same spec draft also anticipated a separate
`Part::Adposition` for the locative valence. It was never built, because the
difference between a property predication and a locative is the **object
slot**, not the predicate slot:

```
  PROPERTY   [Subject, " ", Copula, " ", PredicateWord,                       Tail, "."]
  LOCATIVE   [Subject, " ", Copula, " ", PredicateWord, " ", Det, Complement, Tail, "."]
  INTRANS    [Subject, " ", Verb,                                             Tail, "."]
```

So `at`/`in`/`under` are rows sharing one part list rather than one
construction each, and `Definiteness` gains no `Bare` variant: *"the road is
old"* comes out right because the clause asserts property predication, not
because a third definiteness value produced the right string while still
asserting *road is-a old*.

## What ships

Five variants, each doc-commented with the Stassen strategy it names and the
closure argument; `PREDICATE_VALENCE` gaining `sleep`, `old` and `under` as
rows; `Valence::binds_object()` as the single statement of which strategies
take an object, `pub(crate)` because every consumer is inside the crate.

Every rung this campaign covers carries a realization witness on
`MERCHANT_WITNESS`'s pattern (0330): a hand-built construction and the Common
surface it actually realizes. A rung whose witness could not be built did not
get its token.

## Consequences we accept

- **The witness is weaker than an equality test, and its doc says so.** Common
  is a limited register: four of this campaign's eight witnesses substitute a
  registered concept for the rung's authored word (`old` for `long`,
  `under`/`tree` for `at`/`gate`, `kill` for `strike`), and one realizes
  *"sleeped"* under the naive regular past the crate pins deliberately. The
  witness proves a construction exists and realizes deterministically, not
  that it says what the rung says.
- **A tongue does not speak two of the five.** `Property` and `Locative`
  return a `TongueGap` rather than a partial surface, because the tongue has
  no ordering slot for an uninflected predicate word — see the deferred halves
  in the idea registry.
