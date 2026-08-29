# 0418. Person is a property of the subject, so a syncretic paradigm needs no canonical row

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0266](0266-an-utterance-is-a-fact.md),
[0353](0353-a-regression-test-is-specified-by-the-mutation-it-must-fail.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of Common's copula and verb paradigms gaining a `Person` axis
and becoming **non-injective** for the first time — `are` names four copula
rows, `were` the same four, a bare verb stem five — we decided that the
backward read **nominates no canonical row**, emitting one candidate per row
and letting the parse narrow them with the subject's own surface, accepting
that a copular construction now costs 24 candidate rows where it cost 8.

## This decision replaces the one the spec anticipated

The spec that opened this campaign specified the opposite: that syncretism
forces the parse to return a canonical row and that the backward read's loss
would be **pinned by a test naming which distinctions are unrecoverable**.
That followed from an assumption that person lives on the clause. It does not.
**`Clause` has no person field at all** — person lives inside
`Subject::Pronoun(Person)`, and a `Subject::Name` or `Subject::Clause` is
third person by definition — so the parse reads person off the **subject** and
uses it to narrow the verb group's candidates.

There is no loss to pin. The round-trip property test did not go red; its
enumeration was **widened** from one person to three and passes at full width.

The distinction the spec got right is worth keeping: the table *is*
non-injective. What it got wrong is that non-injectivity in one table implies
information loss in the sentence. It does not, when a second part of the same
sentence carries the missing feature — which is the identical division of
labour Common already used for `killed`, whose number the verb group cannot
state and the object's plural decides.

## Why the obvious repair is refused

Collapsing `verb_group_forms` to one entry per **distinct form** and picking a
canonical `Person` for each would be a rule made by **table ordering**: an
implicit convention, invisible at the point it decides anything, and one that
rots the moment a row is inserted above another. This project's standing
posture is that an implicit convention gets named or gets removed; here it can
be removed, so it is.

## What ships

`COPULA_PARADIGM` keyed `{Present, Past} × {Sg, Pl} × {Pos, Neg} × {First,
Second, Third}`, `VERB_PARADIGM` the same; `verb_group_forms` emitting one
entry per **row**; `parse_clause_body` narrowing on `nominative_person` over
`PRONOUN_PARADIGM`'s nominatives, with `Subject::person`'s third person for a
name. `am` is the only genuinely new word — the distinct-string set went from
8 to 10 — and the parse search went from 8 to **24** `find` calls per copular
construction, because the search is per row.

Both paradigms are **total** over the widened key, and
`no_verb_group_form_spans_two_tenses_or_polarities` converts a property the
parser had been silently relying on — that a winning row's tense and polarity
are unambiguous — from *obvious while there were eight distinct forms* into an
asserted fact, at exactly the moment the old reasoning stopped carrying it.
The invariant's name changed with its content; three test renames landed with
their `subfloor-roster.tsv` edits in the same commit.

The person filter's non-vacuity is demonstrated rather than asserted (0353):
deleting `h.6 == person` from the narrowing makes *"I are a planet."* parse
successfully and reddens
`an_agreement_violating_sentence_does_not_parse` — traced through the code,
not assumed.

## Consequences we accept

- ***"They is a planet"* is untouched, deliberately.** Agreement is keyed on
  features, and third-person singular's bundle takes `is`. That surface's
  awkwardness comes from the pronoun **inventory** — Common spells 3sg `they`
  because nothing in the ledger assigns gender or animacy — not from
  agreement. Real English gives singular *they* plural agreement, which means
  letting a pronoun's chosen form override its features: a different
  mechanism, deferred with its reason.
- **A three-fold parse-search cost** on every copular construction, paid to
  avoid an ordering-derived convention.
