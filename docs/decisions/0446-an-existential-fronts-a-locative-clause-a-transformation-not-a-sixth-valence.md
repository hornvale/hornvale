# 0446. An existential fronts a locative clause — a transformation, not a sixth `Valence`

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0326](0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md),
[0416](0416-valence-is-a-closed-predication-strategy-taxonomy.md),
[0419](0419-interrogative-force-is-an-operator-over-a-clause-never-a-field-on-it.md) ·
[The Quoin](../../book/src/chronicle/the-quoin.md)

In the context of Common needing to say *"there is a person under the
tree"* — the existential rung `the-ladder.corpus.json` names `r104` — we
decided that an existential is a **transformation over a `Valence::Locative`
clause**, `front_existential`, and never a sixth `Valence` variant, accepting
that the definiteness effect this construction is named for (existentials
resist definite pivots) is expressible through the resulting surface but not
mechanically enforced.

## Why this is not `0416`'s failure mode

`Valence` closed at five variants at The Rail (decision 0416), which quoted
0326's own warning verbatim: *"if a future campaign finds itself adding a
variant per predicate, it has rebuilt `Frame` and should stop."* A sixth
variant for existentials would be exactly that shape. `Valence::Locative`'s
own doc, written at The Rail, anticipated the temptation and refused it in
advance — Freeze (1992)'s claim that existential, locative and possessive
predication are one construction with different arguments fronted is a claim
about the CONSTRUCTION, the doc says, "grants no dispensation for `r019` or
`r104` on its own." This decision is what makes good on that refusal:
existential predication is built, but built as an operator that *consumes* a
locative clause, the same "transformation, not a table row" move
`realize_common_polar_question` makes for interrogative force (decision
0419) and `realize_common_verbless` makes for zero-copula predication, one
argument-structure axis over.

`front_existential` takes a `Valence::Locative` construction's own part
list — `[Subject, " ", Copula, " ", PredicateWord, " ", Determiner,
Complement, ModifierTail, "."]` — drops the subject out of its leading slot,
inserts the dummy pivot `Literal("there")` there instead, and reinserts
`Part::Subject` immediately after the copula. Every part from the adposition
onward is untouched; exactly one literal is added. No row enters
`PREDICATE_VALENCE`; no entry enters `common_constructions`. The spine held
on the merits — verified by two independent people at two tasks, checking
that neither table gained a row, not merely that the enum's variant count
stayed five.

## The definiteness effect is expressible, not enforced, and why

`r104`'s own corpus note states the constraint this construction is named
for: existentials resist definite pivots, and the note names `r007`
(definiteness) as its presupposition for exactly that reason — the
constraint cannot be stated without the category.

It cannot be *enforced* here because `Clause::definiteness` governs the
clause's **object** (the ground — *"under **the** tree"*), never the
**subject** (the pivot the existential fronts). There is exactly one
`Part::Determiner` in this grammar and it reads `spec.object`/
`spec.definiteness`, never `spec.subject`. The pivot is whatever
`Subject::Name` text a caller already resolved — an opaque string this
domain cannot inspect for its own article — so a caller can write
`Subject::Name("the person")` and `realize_common_existential` fronts it
exactly as readily as an indefinite one: *"there is the person under the
tree,"* oddly grammatical Common and ungrammatical English, emitted rather
than refused. Pinned by value at
`a_definite_pivot_renders_rather_than_being_refused`, not merely documented.

The only machinery in this crate that *derives* a subject's article from
referent identity is `Discourse`/`discourse_subject_is_repeat_mention`
(decision 0447) — built by the same campaign, one task earlier. Routing
`realize_common_existential` through it would enforce the effect, and was
deliberately not done: `Discourse` tracks definiteness across a *sequence*
of clauses about a recurring referent, which is a different phenomenon from
a single existential clause's own pivot, and reaching for it here would
widen this rung's dependency on a structure the spec scoped as "consulted,
not built on."

## Consequences we accept

- **A caller can front a definite pivot and get a grammatical-looking,
  semantically odd sentence rather than a refusal.** This is a stated limit,
  not a silent one, and it was found while fixing an unrelated witness typo
  in the plan (`Definiteness::Indef` paired with a `Def`-only expected
  string) — the fix exposed that "enforced" was not reachable through the
  field that exists at all, only through a widened `Discourse` dependency the
  spec had deliberately scoped away.
- **Enforcing the effect is a real design question for a future campaign**,
  not a bug: whether an existential's pivot should be `Discourse`-tracked at
  all, or needs its own narrower derivation, is undecided by this record.
- **Non-`Valence::Locative` predicates are refused, loudly**, by assertion —
  rendering a non-locative clause through this operator would front a
  relation the clause does not have, the "plausible garbage" class every
  transformation in this module refuses rather than emits.
