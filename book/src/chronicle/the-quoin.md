# The Quoin

*A quoin is the dressed cornerstone that ties two walls together, and in
letterpress the wedge that locks type into a chase. Both readings name the
same rung: `existential` sits on the border of reference and predication, and
holds them square.*

Five rungs of the 214-rung capability ladder, chosen by intersecting two
things that do not usually point the same direction — the ladder's own
**frontier** (what The Rail made buildable next) and `the-flood-watch`'s
**demand** (what a real corpus actually asks for) — against a closed,
five-variant `Valence` that adds no sixth member.

```
  r048  spatial-adverbial     "The guard struck her in the marketplace."
  r049  temporal-adverbial    "Everything was fine until last night."
  r171  verbless-clause       "A dead woman in the marketplace, and the
                                gate open all night."
  r007  definiteness          "A stranger waits at the gate. The stranger
                                is a soldier."
  r104  existential           "There is a body in the marketplace."
```

The ladder now reads **19 of 214** (was 11), its frontier **33** (was 20),
the-merchant **8 of 12** (was 6), and `the-flood-watch`'s demand-instance
count **393 of 1128** (was 270). Every one of those four figures matched a
prediction frozen before any grammar was written, on the first run, for
every task — the campaign's own instrument confirming itself rather than a
second implementation of it (see "The preregistration held" below).

**None of those numbers is the one this chronicle leads with.** The ladder is
a declared *production* instrument — every rung it scores is a capability to
generate, never to read — so the composite demand-instance figure above
quietly counts flood-watch lines this campaign never touched: the corpus's
`parse`-side entries, lines the grammar must someday *read*, not say. Split
by direction, computed for the first time at Task 6b:

```
  produce    203 / 638   = 31.8%   <- the half the ladder actually measures
  parse      190 / 490   = 38.8%
  composite  393 / 1128  = 34.8%   <- what the report showed before the split existed
```

**The half this campaign targeted is the *lower* one, and the composite
flatters the result by three points.** Had the chronicle quoted 393/1128
alone, it would have reported a better number than the campaign earned, in a
direction nobody would have thought to check. The produce-side figure —
**203 of 638, 31.8%** — is the one PREREG-4 binds this chronicle to quote,
with the total beside it, never the total alone.

## Five rungs, three operators, one closed spine

The five rungs are not five equally-sized pieces of work. Two — the spatial
and temporal adjuncts — are two arms on one existing extension point
(`common_role_surface`, `Adjunct`), and are the cheapest contact with the
grammar this campaign makes. Two more — `verbless-clause` and `existential`
— are new **operators over a clause**, joining `realize_common_polar_question`
(force), the embedding slot and the coordination list (The Mortise) in a
family this chapter now names explicitly (see the book sweep below). The
fifth, `definiteness`, is the campaign's only genuinely new *structure*: a
discourse seam nothing before this campaign supplied.

**The spine held.** `Valence` opened this campaign at five variants and
closes it at five — verified by parsing the enum body, twice, independently,
by two different people at two different tasks, not by trusting a comment.
Zero rows entered `PREDICATE_VALENCE` for a new construction; zero entries
entered `common_constructions`. `existential` and `verbless-clause` are both
**transformations**, in the precise sense `realize_common_polar_question`
already established: a free function that takes whichever construction a
predicate's existing `Valence` already selected and re-presents it, never a
new row in a lookup table and never a new field on `Clause`.

### `verbless-clause` — a strategy, argued as one rather than assumed

*"the person under the tree."* — zero-copula predication, built as
`strip_copula`: remove `Part::Copula` and the literal space before it from
whichever copula-bearing construction (`CLASSIFY`, `PROPERTY`, `LOCATIVE`)
the clause's predicate already selected.

Read uncharitably, that is deletion, not a strategy — "the nominal
construction with the copula deleted" is exactly the ellipsis analysis the
rung's own corpus note rejects (Stassen 1997, Hengeveld 1992 both treat
zero-copula predication as a strategy in its own right, not answer
ellipsis). The review that cleared this task supplied the decisive argument
neither the plan nor the implementer had stated: the chosen witness — r171
realized at `Valence::Locative`, predicate `UNDER` — **depends on the
generality**. A `Nominal`-only implementation could not have built it. That
is a behavioural fact about what the function can do, not rhetoric about how
it is described, and it is what converts "a subtraction applied three times"
into an honest claim to be one strategy over three valences.

### `existential` — Freeze's construction, fronted rather than declared

*"there is a person under the tree."* Freeze (1992) argues existential,
locative and possessive predication are one construction with different
arguments fronted. `front_existential` takes a `Valence::Locative`
construction's own part list, drops the subject from its leading slot,
inserts the dummy pivot `"there"` there instead, and reinserts the subject
immediately after the copula — every part from the adposition onward
(`PredicateWord`, `Determiner`, `Complement`, `ModifierTail`) untouched.
Exactly one literal is added. `Valence::Locative`'s own doc, written at The
Rail, refused in advance the inference that its existence alone unlocked
this rung; `front_existential` is what makes good on that refusal, built as
Freeze's *transformation*, never as a sixth `Valence` variant.

**The definiteness effect is expressible, not enforced, and the reason is
structural rather than a shrug.** Existentials resist definite pivots (*"there
is the body in the marketplace"* reads oddly), and r104's own note says this
is why it presupposes r007 — the constraint cannot even be *stated* without
the category. But `Clause::definiteness` governs the clause's **object** (the
ground — *"under **the** tree"*), never the subject the existential fronts
(the **pivot**), and the pivot is whatever `Subject::Name` text the caller
already resolved: an opaque string this domain cannot inspect for its own
article. The only machinery anywhere in this crate that *derives* a
subject's article from referent identity is Task 4's `Discourse` — and
spec §3.5 scoped that structure as "consulted, not built on" for this rung
specifically to avoid widening a dependency the spec had not asked for. So a
caller genuinely can write `Subject::Name("the person")` and
`realize_common_existential` will front it exactly as readily as an
indefinite one — pinned by value, not merely stated in prose, at
`a_definite_pivot_renders_rather_than_being_refused`. This was found while
fixing an unrelated typo in the plan's own witness (`Definiteness::Indef`
paired with an expected string that required `Def`); the fix exposed that
one of the two honest branches spec §3.5 offered — *enforced* — was not
reachable through the field that exists at all, only through a widened
dependency the spec had deliberately scoped away. **Expressible-not-enforced
is the recorded outcome of a design constraint the campaign discovered
mid-task, not a corner cut.**

### `definiteness` — a discourse fact, derived, not stated twice

r007's own text is two sentences on purpose: *"A stranger waits at the gate.
The stranger is a soldier."* — because definiteness is not visible inside one
clause. `Clause` already carried a `definiteness` field and Common already
realized it through `Part::Determiner`; what was missing was anything that
tracked a referent *across* clauses, so nothing stopped a caller from
asserting `Def` on a first mention.

`Discourse { clauses: Vec<DiscourseClause> }` closes that gap by
**withholding** the thing it derives: `DiscourseClause` carries a bare
`referent: String` rather than a resolved `Subject`, so the caller cannot
state the subject's definiteness even by accident — there is no field there
to set. `discourse_subject_is_repeat_mention` compares each clause's referent
against *every earlier* clause in the sequence, and `realize_common_discourse`
computes `"a person"` or `"the person"` from that alone before handing the
clause to the ordinary, unmodified `realize_common`. The property under test
could not pass by stating `Def`/`Indef` twice even if the test tried to —
the strongest form of "derived, not asserted."

**One deliberate departure from precedent, disclosed with its reason rather
than made in passing.** `elide_coordinated_subjects` (The Mortise) compares
each clause to the *last stated* subject, because subject elision is
genuinely sensitive to what a reader has just read — comparing to the first
clause instead misattributed `[X, Y, X]`'s third mention to the wrong
referent, a fix paid for once already. `Discourse` compares against *any
earlier* mention instead, because definiteness does not carry that hazard:
once a referent enters a discourse, every later mention of it is definite
regardless of what intervened (Chafe 1976; Lambrecht 1994) — an intervening
`tree` between two mentions of `person` does not make the second `person`
indefinite again. Applying `elide_coordinated_subjects`'s rule here would
import a fix for a different hazard than the one this structure has. The
negative control this claim rests on was traced by hand at review: a naive
`"position > 0 => definite"` rule would fail against `[X, Y, X]`, so the
control genuinely discriminates rather than decorating.

`definiteness` is also the campaign's largest single move on the frontier —
see "Growth is not uniform" below — and unblocked `pronoun-reference`'s
`r012` as a side effect: a rung whose token was implemented two campaigns
ago (The Inquest) but which nothing could cover until something tracked
discourse referents at all. That is coverage debt paid off, a different
phenomenon from a control rung riding along on a `null` `introduces` (see
below), and it very nearly went unrecorded as the latter.

## Growth is not uniform, and per-rung `unblocks` is why the total is readable

PREREG-3 — the criterion that the frontier should *grow*, not shrink — was
amended mid-campaign after Task 0's own measurement broke it as originally
written (see "The preregistration held" below for how). What it binds now is
the **cumulative** endpoint only: 20 → 33 across the five-rung span. Per-step
movement is reported, never asserted, because covering *any* frontier rung
with `unblocks: 0` shrinks the frontier by exactly one mechanically, and 18
of the 20 baseline frontier rungs — including `wh-question`, the rung §1.1
rejected in favour of `verbless-clause` — have `unblocks: 0`. A per-step
criterion could not have told the chosen five from the rejected one; it fires
on nearly anything.

Per rung, what each one alone unblocked (computed by re-running the resolver
with only that token added):

| rung | frontier before → after | unblocks |
|---|---|---|
| `spatial-adverbial` | 20 → 20 (membership swap: r048 leaves, r049 joins) | 1 |
| `temporal-adverbial` | 20 → 20 (r049 leaves, r060 joins) | 1 |
| `verbless-clause` | 20 → **19** (r171 leaves, nothing joins) | 0 |
| `definiteness` | 19 → **34** (the largest single move) | 15 |
| `existential` | 34 → 33 (r104 leaves, nothing joins) | 0 |

`definiteness` alone carries the entire net cumulative gain and then some;
the other four rungs are close to a wash against each other. **One strong
rung's opening can carry a cumulative criterion that five leaves would also
satisfy** — the residual risk PREREG-3's amendment names explicitly, and the
reason this table, not just the pass/fail verdict, belongs in this record.

Three tokens carried a second covered rung besides their own, and the two
kinds must not be conflated: `temporal-adverbial` and `existential` each rode
in with a genuine *control* rung (`r067`, `r105` — `introduces: null`,
presupposed demands already met, no capability of its own). `definiteness`'s
second rung, `r012` (`pronoun-reference`), is not that — it is an
already-implemented token finally unblocked, coverage debt paid rather than
a passenger.

## The merchant corpus moves twice

6 of 12 → 8 of 12: `m02` at `temporal-adverbial` (verbatim — *"everything was
fine until last night"* is the merchant's own line), `m04` at `existential`
(it declares existential, past-tense and temporal-adverbial together, all
three now built). Landing `existential` also shrank the corpus's own
"one-token-short" list from 4 entries to 3, since `m04` left it for
*covered* with nothing replacing it — forcing a third rename of the test
naming that set (see the retrospective).

## The preregistration held, and what that confirms

The Rail's retrospective named its own headline defect: a preregistered
number computed by a *second implementation* of the resolver, rather than by
the resolver itself, agreeing with the real one only by luck. This campaign's
Task 0 built the remedy directly into its own preregistration step: append
the five tokens to `IMPLEMENTED_DEMANDS` on a scratch commit, run the two
live instruments the campaign will later be judged against, record their
output, revert. Every later task's predicted row is that resolver's own
number, not a re-derivation of it.

All four figures — ladder covered, frontier, merchant, flood-watch demand
instances — matched on the first run, at every one of five tasks. **Not one
number was revised.** That is a real result and it is not independent
confirmation of the resolver: Task 0 derived the predictions with the same
resolver that later measured the outcome, so agreement shows the
implementation did what the resolver predicted, not that the resolver is
right about the world. What it does confirm is narrower and still worth
having: the remedy The Rail proposed — derive a preregistered number *with*
the instrument, never *beside* it — generalizes past the one campaign that
discovered the need for it.

**Three genuine defects still originated in this campaign's own plan text —
zero in implementer code — and none of the three is the class PREREG-1 was
built to prevent.** They were a merchant coverage step the plan omitted
entirely (caught in the pre-flight conflict scan before any task ran); a
witness string requiring `sleep` → `slept` when Common's past-tense rule is
the pinned-on-purpose naive `+ed` (`sleeped`); and a witness pairing
`Definiteness::Indef` with an expected string that required `Def`. All three
were caught by pre-dispatch brief verification against the tree, before an
implementer ever saw them — not by review, and not by the mechanism this
campaign was built to test. See the retrospective for the full account,
including a fourth failure this campaign's *reviewers* produced rather than
its planning text.

## What this does not reach

- **Two of the five rungs' relation words never enter tongue rendering.**
  `spatial-adverbial` and `temporal-adverbial` each realize their *concept*
  argument through the tongue lexicon exactly as any other role does, but the
  relation itself (`under`, `at`) is Common-only — stated in
  `IMPLEMENTED_DEMANDS`'s doc, the same posture `epistemic-hedge` and two of
  The Rail's five rungs take. `verbless-clause`, `definiteness` and
  `existential` are Common-only outright: `grammar.rs` carries zero
  `discourse`/`existential` references, verified by grep, not inferred from
  absence of a bug report.
- **Nothing parses a verbless clause, and the loss is sharper than
  `Valence::Locative`'s own.** `verb_group_forms`'s final arm was already
  `else { Vec::new() }` before this campaign touched it — verified rather
  than assumed — so a verbless construction yields no verb surfaces at all,
  and `parse_clause_body`'s search fails regardless of what a `ParseContext`
  registers. This campaign is production-side by its own selection rule
  (§1.1); the loss is stated, not fixed.
- **The produce-side figure this chronicle leads with did not exist as an
  instrument until Task 6b**, built *after* all five implementation tasks
  and after Task 6's own reconciliation, specifically because PREREG-4 named
  a figure `demand_instance_coverage` could not compute. The disclosure that
  the split postdates the implementation work is in the generated artifact
  itself, not only in this chronicle.
- **`witness-set` and `named-entity-list`, the merchant corpus's last two
  tokens, are untouched.** `witness-set` alone carries nine uncovered
  transitive dependencies — The Rail's follow-up #3 ("the cheapest remaining
  path is five tokens") is true at the token level and misleading as a
  price, which this campaign deliberately traded away from.
- **The build-order resolver the ladder's own `shape_notes` calls for is
  still hand-run.** Five rungs were chosen by an ideonomy pass reading the
  frontier and demand tables by eye — that job done by hand, again, a
  campaign after The Rail named the gap and did not close it.
- **The parse hemisphere still has no instrument.** 68 of flood-watch's 139
  entries are `parse`-direction; nothing scores parse capability. This
  campaign is the first to price that gap against a candidate list rather
  than merely note it (§1.1's `wh-question` rejection), but pricing a gap is
  not closing it.

Three decisions: [0446](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0446-an-existential-fronts-a-locative-clause-a-transformation-not-a-sixth-valence.md)
(existential as transformation, not a sixth `Valence`), [0447](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0447-definiteness-is-a-discourse-fact-derived-from-referent-recurrence-not-a-clause-field.md)
(definiteness derived from referent recurrence, not stated per clause), and
[0448](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0448-a-produce-side-demand-instance-statistic-complements-the-composite-and-never-replaces-it.md)
(the produce-side split complements the composite figure, never replaces
it) are this campaign's own; see the retrospective
(`docs/retrospectives/the-quoin.md`) for the process account.
