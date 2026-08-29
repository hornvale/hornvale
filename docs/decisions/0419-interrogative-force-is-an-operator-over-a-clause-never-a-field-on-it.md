# 0419. Interrogative force is an operator over a clause, never a field on it

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0266](0266-an-utterance-is-a-fact.md),
[0327](0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md) ·
[The Rail](../../book/src/chronicle/the-rail.md)

In the context of Common needing to ask *"are you a merchant?"*, we decided
that interrogative force is an **operator over** a `Clause` — a free function
taking the clause it questions — and never a `force: Force` field on the
clause, accepting that a caller wanting to *hold* a question has nothing to
hold today.

## Why a field would be a category error, not just churn

Decision 0266's claim is that an utterance **is** a fact: subject, predicate,
object, plus the speaker's features. A polar question is precisely the
utterance that is not — it asserts nothing. Putting `force` on `Clause` would
make the fact-shape claim false for **every** clause in order to serve one,
which is a larger cost than the 105 construction sites it would also churn.

Decision 0327 already settled the shape of the alternative: embedding and
coordination are two operators, a slot and a list. A question is a third — a
wrapper. It is thinner than `Coordination`, which earned its own type by
holding a `Vec`; a single-field wrapper earns nothing today, so
`realize_common_polar_question` stays a free function and the
`PolarQuestion(Clause)` type arrives with the first caller that stores one.

## A lexical verb is refused, loudly

English inverts an auxiliary, and the copula is the only auxiliary Common has.
A construction whose verb group is `Part::Verb` — the transitive and
intransitive frames — does not invert: *"Sleeps the guard?"* and *"Knew you
the woman?"* are not Common, and emitting either is the plausible garbage this
project's realizers refuse on principle. So the operator **panics**, on the
same fail-fast posture `realize_common` takes for an unconstructed predicate
and for the same reason: an authoring hole, not a fact about the world.

What English actually uses there is periphrastic *do*-support. Half of it
already exists — `VERB_PARADIGM` carries `"did not "`/`"do not "`/`"does not "`
— but only the negative half, so reaching the positive by stripping `" not"`
is text surgery. The honest route is a **mood axis on a bidirectional key**,
which is a parse-side change as much as a realize-side one, and it is a later
campaign's.

## The corpus entry this unblocks under-describes itself

`m08` (*"Did you know the woman?"*) declares `polar-question` + `past-tense`,
both now built, and remains unsayable for the reason above. It is the **third**
of twelve merchant entries whose hand-labelled tokens under-describe it, after
`m10` (lexical) and `m02` (adjectival predication) — which is what a frozen
corpus is for: no amount of care at labelling time would have caught it, and
resolving against a real grammar did. Its witness therefore exercises the two
tokens it declares through a past copula question, *"were you a merchant?"*,
and that substitution is recorded in the generated report rather than only in
a task's notes.

`m08` is also a **player** line scored on a **production** capability. The
merchant corpus states no `direction` and decision 0387 forbids inferring one
from `speaker`, so the resolver scores it covered on production. That is the
scoring method behaving exactly as specified, and a real limit on what "6 of
12" means; it goes in the report, not only in a spec.

## Consequences we accept

- **Nothing parses a question.** Parse-side recognition of the interrogative
  is out of scope; the honesty note above stands in its place.
- **A construction whose part list does not end in `Literal(".")` panics
  rather than degrading.** The operator rewrites the terminal literal into
  `"?"`, and a construction ending in anything else would otherwise realize a
  question with a full stop — the same silent degradation this operator
  refuses a lexical verb to avoid. Both panic sites carry
  `#[should_panic(expected = …)]` tests specific enough to distinguish them
  from an incidental panic.
