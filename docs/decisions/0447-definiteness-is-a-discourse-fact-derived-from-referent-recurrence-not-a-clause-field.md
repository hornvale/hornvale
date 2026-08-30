# 0447. Definiteness is a discourse fact, derived from referent recurrence — not a field the caller states

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0296](0296-tense-is-stated-never-derived.md),
[0327](0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md) ·
[The Quoin](../../book/src/chronicle/the-quoin.md)

In the context of `the-ladder.corpus.json`'s `r007` needing two sentences —
*"a stranger waits at the gate. the stranger is a soldier."* — because
definiteness "is not visible inside one clause," we decided that a
referent's definiteness across a **sequence** of clauses is a derived
discourse fact, computed by `realize_common_discourse` from referent
recurrence, and withheld from the caller of the new `DiscourseClause` type
entirely — never a value the caller states per clause, accepting that the
existing `Clause::definiteness` field is unchanged and continues to govern a
single clause's own object slot exactly as before.

## Why this needed a new structure rather than an existing one

`Clause` already carried a `definiteness` field, realized through
`Part::Determiner`, and nothing about a single clause's own shape changes
under this decision. What was missing was anything that tracked a referent
*across* clauses — so before this campaign, a caller could assert `Def` on a
sentence's own first mention of something and nothing would object.

`Coordination` (decision 0327) is the nearest existing multi-clause type and
is not this: it joins clauses into *one* sentence with a shared subject slot.
r007's two sentences are not one sentence — they are two independent
utterances about a recurring referent, which is what `Discourse { clauses:
Vec<DiscourseClause> }` is built to be minimal over: an ordered list plus a
referent id per clause, no discourse-representation theory and no
entity-tracking beyond string equality on `DiscourseClause::referent`.

**The derivation is enforced by what the type withholds, not merely by what
the function computes.** `DiscourseClause` carries a bare `referent: String`
rather than a resolved `Subject` — there is no field a caller could set the
subject's article on even by accident. `discourse_subject_is_repeat_mention`
compares each clause's referent against every *earlier* clause in the
sequence; `realize_common_discourse` computes `"a person"` or `"the person"`
from that alone and only then builds the ordinary `Clause` it hands to the
unmodified `realize_common`. The property this pins could not pass by
stating `Def`/`Indef` twice even if a test tried to.

## A departure from `elide_coordinated_subjects`'s rule, disclosed with its reason

`elide_coordinated_subjects` (The Mortise) compares each clause to the *last
stated* subject — the fix for a real, once-paid-for hazard: comparing to the
first clause instead misattributes `[X, Y, X]`'s third mention to the wrong
referent, since a reader has just read `Y`.

`Discourse` compares against *any earlier* mention instead, deliberately not
importing that rule. Definiteness does not carry the same hazard: once a
referent enters a discourse, every later mention of it is definite
regardless of what intervened (Chafe 1976; Lambrecht 1994) — an intervening
`tree` between two mentions of `person` does not make the second `person`
indefinite again. So `[X, Y, X]` gives `Indef / Indef / Def` here, which
would differ from what `elide_coordinated_subjects`'s rule would give were it
applied to this phenomenon. Applying that rule here would be importing a fix
for a different hazard than the one this structure has. The negative control
this rests on was traced by hand at review: a naive `"position > 0 =>
definite"` rule fails against `[X, Y, X]`, confirming the control
discriminates rather than decorates.

## Relation to decision 0296

0296 states that `number`, `definiteness`, `evidential` and `polarity` are
each "a fact about the [clause's] own content, and each is therefore
recoverable from the clause alone" — true, and unchanged by this decision
for the OBJECT slot and for any `Clause` built outside a `Discourse`. This
decision adds a second, higher-level fact: whether a discourse's own
*subject* is a first or later mention is not recoverable from any one clause
alone, and is derived one level above the single clause `Discourse` was
built to sit over. 0296 refused to give a clause a clock because tense needs
a deictic centre outside it; this decision gives a *discourse* a memory,
because subject-recurrence needs a sequence outside any one clause, for the
identical reason stated at a different scope.

## Consequences we accept

- **`definiteness` (r007) is genuinely two-thirds of its own campaign's
  frontier growth** (19 → 34, decision-adjacent figure recorded in the
  chronicle) — the largest single-rung move this campaign made, which is a
  fact about the graph, not a claim this decision makes.
- **`pronoun-reference` (`r012`, implemented since The Inquest) was unblocked
  as a side effect** — coverage debt paid off by tracking referents at all,
  a different phenomenon from a `null`-`introduces` control rung riding
  along and not to be conflated with one.
- **Only the subject is discourse-tracked.** A clause's object retains its
  own, caller-stated `Definiteness` unchanged — r007's own text has a second
  definite article (*"the gate"*) on an unrelated predicate's object, which
  `DiscourseClause::definiteness` forwards exactly as `Clause::definiteness`
  always has.
