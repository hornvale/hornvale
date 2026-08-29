# The Quoin — five ladder rungs, one closed taxonomy

**Campaign:** The Quoin · **Branch:** `campaign/the-quoin` · **Date:** 2026-08-29
· **Base:** `6f121ac07`

*A quoin is the dressed cornerstone that ties two walls together, and in
letterpress the wedge that locks type into a chase. Both readings name the
same rung: `existential` sits on the border of reference and predication, and
holds them square.*

---

## 1. What this campaign builds

Five rungs of `sentences/the-ladder.corpus.json`, chosen **frontier ∩
demand**:

| rung | token | flood-watch demand (total / produce) | why it is here |
|---|---|---|---|
| r007 | `definiteness` | 50 / 25 | largest demand on the frontier; sole blocker of r104 |
| r171 | `verbless-clause` | 28 / 20 | largest minted count after the input-surface tokens; best-aligned high-volume token at 71% produce |
| r049 | `temporal-adverbial` | 22 / 11 | merchant `m02` verbatim |
| r048 | `spatial-adverbial` | 20 / 11 | sole blocker of r049 |
| r104 | `existential` | 3 / 2 | the quoin — the only rung touching two regions |

### 1.1 Why these five and not the other fifteen

The frontier carried 20 rungs at `6f121ac07`. Nathan set the selection rule
(frontier ∩ demand) at the campaign's first gate; the specific five came from
an ideonomy pass that **overturned** the pre-pass recommendation, and the
overturning evidence is worth stating because it is a fact about the
instrument rather than a preference.

The pre-pass set swapped `verbless-clause` for `wh-question` (r085) and
reached merchant 10 of 12. Splitting flood-watch demand by `direction` —
which decision 0387 requires the report to do and which nothing had yet done
to a *candidate list* — killed it:

```
  token                total  produce  parse   produce-share
  perfect-aspect           9        9      0     100%
  imperative               7        6      1      86%
  verbless-clause         28       20      8      71%
  existential              3        2      1      67%
  spatial-adverbial       20       11      9      55%
  definiteness            50       25     25      50%
  temporal-adverbial      22       11     11      50%
  wh-question              8        3      5      38%
```

`the-ladder` declares itself a **production instrument** in its own
`production_axis` block. `wh-question` is the worst-aligned candidate against
that axis: five of its eight flood-watch instances are player lines the
grammar must *read*, so building a production-side content question would
score all eight while satisfying three. That is precisely the `m08` defect
`docs/audits/sentence-coverage.md` already confesses to — *"a `parse`-side
line scored on a `produce`-side capability"* — and inheriting it is one
thing, choosing it is another.

**`wh-question` is not thereby deprioritised as a capability.** It stays on
the frontier untouched by this campaign (its deps `r083`, `r006` are already
covered and nothing here changes them), so deferring it costs only time. The
same is true of every rung this campaign declines. The rungs that would have
cost something to defer are the chained ones — r048 → r049, and r007 → r104 —
and both chains are built here.

### 1.2 What the campaign is NOT

- **Not the merchant corpus's completion.** This set takes `the-merchant`
  from 6 of 12 to 8 of 12, not to 10 or 12. The corpus's last two tokens are
  `witness-set` and `named-entity-list`; `witness-set` (r200) carries **nine**
  uncovered transitive dependencies, so The Rail's follow-up #3 ("the cheapest
  remaining path is five tokens") is true at the token level and badly
  misleading as a price. Deliberately traded toward the 139-entry instrument
  and away from the 12-entry one whose own provenance calls it "nearly
  exhausted as a scoping instrument".
- **Not a coverage resolver for `the-flood-watch`.** Still open (The Rail
  follow-up #4).
- **Not the parse hemisphere.** The ladder cannot score it; see §7.
- **Not a build-order resolver.** See §7.

---

## 2. The constraint: `Valence` does not widen

**`Valence` is closed at five variants and this campaign adds none.**

`clause.rs` states it and decision 0326 ratifies it: Stassen (1997)'s four
intransitive predication strategies (`Nominal`, `Intransitive`, `Property`,
`Locative`) plus `Transitive`, "five variants total, ever." The tripwire is
quoted in both places — *"if a future campaign finds itself adding a variant
per predicate, it has rebuilt `Frame` and should stop."*

This binds hardest on `existential`. Freeze (1992) argues locative,
existential and possessive predication are one construction with different
arguments fronted, and `Valence::Locative`'s doc cites exactly that — and then
refuses the inference in advance:

> Freeze's claim is about the CONSTRUCTION, not about this ladder's edges, and
> taking the citation at face value overstates what this valence unlocks. […]
> this variant grants no dispensation for `r019` or `r104` (the existential
> rung) on its own.

So an existential is built as a **transformation over a clause**, not as a
sixth valence. The precedent is `realize_common_polar_question`: a realizer
that takes any clause at any valence and re-presents it, rather than a new
row in `PREDICATE_VALENCE`. `verbless-clause` takes the same posture from the
other side — a construction, not a valence.

**This constraint is falsifiable and the campaign must report it if it
breaks.** If `existential` genuinely cannot be expressed without a sixth
valence, that is a finding about Stassen's taxonomy meeting Freeze's, and the
campaign stops and says so rather than widening the enum quietly. A null here
is a result (decision 0016's discipline).

---

## 3. The five rungs

**Presented in build order, which is not §1's demand order.** The two adjunct
rungs come first because they share one extension point and are the cheapest
contact with the code; `definiteness` follows as the largest piece; and
`existential` is last because §3.3 is its dependency.

Each subsection names the extension point, the precedent it rides, and the
**decision rule** the implementer applies — never a predicted outcome. (The
Quire's five plan-text defects were all imperatives with an outcome smuggled
inside; a branch table cannot be wrong the way a prediction can.)

### 3.1 r048 `spatial-adverbial` — *The guard struck her in the marketplace.*

**Extension point:** `common_role_surface` in `clause.rs` (Common) and
`realize_adjuncts` in `grammar.rs` (tongue).

The `Adjunct` machinery is built: `Adjunct { role: String, argument: Argument }`,
`AdjunctPosition::{Inline, Trailing}`, a realized tail in both Common and
tongue paths. What is absent is any *spatial* role. `common_role_surface` is a
closed match on seven role ids (`moon-count`, `star-class`, `day-length-std`,
and four `occ-*`), all of them astronomy or deep-history roles; an unmatched
role returns `None` and renders as nothing, by the same discipline
`CommonVocabulary::word_for` follows.

So this rung is a role construction, and the open design question is its
**generality**: a hard-coded `("at-site", Argument::Count(v))` arm matching
the seven that exist, or a general adposition surface driven by the role id.

**Decision rule.** Prefer the arm that matches existing shape unless a second
spatial role is needed within this campaign — in which case the two arms are
the evidence for generalising, and the generalisation ships with both readers
present rather than one reader and a forecast. (This is the posture
`Part::PredicateWord` took: its doc reserved a slot for a locative valence
before one existed, and The Rail's Task 5 built it. Reserving is cheap;
generalising on one caller is not.)

**Tongue side.** Talmy (1985, 2000) splits verb-framed from satellite-framed
languages, and a spatial adjunct is where that split surfaces. Whether this
campaign *draws* that parameter is a scope question, not a requirement: the
rung is satisfied by realization, and a drawn typological axis is a separate
capability. **Decision rule:** if the tongue path gaps rather than realizes,
ship the rung Common-only and say so in `IMPLEMENTED_DEMANDS`'s doc, exactly
as `epistemic-hedge` and two of The Rail's five rungs did. A gap that is
stated is not a defect; a gap that is silent is.

### 3.2 r049 `temporal-adverbial` — *Everything was fine until last night.*

**Extension point:** the same machinery as §3.1. Merchant entry `m02`
verbatim.

**The r048 → r049 edge is diachronic, and the corpus says so itself:**

> The dependency on r048 is DIACHRONIC, from Haspelmath's (1997) *From Space
> to Time* […] Stated openly because there is no SYNCHRONIC requirement that a
> language have spatial adjuncts before temporal ones — this is an ordering
> claim about how the category arises, and it is the kind of claim a reviewer
> should be able to reject on its own terms.

This campaign **accepts the edge and builds both**, for a reason unrelated to
the diachrony: the two share one extension point, so building r048 first is
cheaper *as engineering* whatever the typology says. Recorded here so nobody
later reads the sequencing as an endorsement of the implication.

### 3.3 r007 `definiteness` — *A stranger waits at the gate. The stranger is a soldier.*

**Extension point: a discourse seam that does not exist.** This is the
campaign's largest piece and the one place a new structure is added.

`Clause` already carries `definiteness: Definiteness` (`Indef`/`Def`), and
Common already realizes it through `Part::Determiner`. What is missing is what
the rung's own note names: *"Two sentences on purpose: definiteness is not
visible inside one clause."* The feature is caller-supplied per clause and
nothing tracks a referent across clauses, so today a caller could state
`Def` on a first mention and nothing would object.

**Precedent:** `pronoun-reference` (The Inquest) is the same phenomenon with a
different exponent — re-mention realized as a pronoun. Definiteness is
re-mention realized as an article. `Coordination` (`clause.rs`) is the
existing multi-clause structure, but it joins clauses into *one* sentence,
which is not what two sentences with a shared referent are.

**Decision rule.** Build the smallest structure that makes first-mention /
second-mention a *derived* fact rather than a caller's assertion, and keep it
in `domains/language`. The three branches:

- If the structure can be an ordered sequence of clauses plus a referent
  identity the realizer reads → build that; `definiteness` becomes computed,
  not supplied.
- If it requires the caller to still supply `definiteness` and the new
  structure only *checks* the caller → that is a weaker rung. Ship it, and
  state in `IMPLEMENTED_DEMANDS`'s doc that the token is backed by a check
  rather than a derivation.
- If it requires reaching outside `domains/language` for referent identity →
  **stop**. A domain may not depend on a sibling, and a discourse referent
  that only a window can supply is a different campaign.

**The definiteness effect is r104's dependency, not decoration.** Existentials
resist definite pivots (*"there is the body in the marketplace"* is odd), and
r104's note says this is exactly why it presupposes r007: *"the constraint
cannot be stated without the category."* Whatever §3.3 builds must be
something §3.5 can consult.

### 3.4 r171 `verbless-clause` — *A dead woman in the marketplace, and the gate open all night.*

**Extension point:** a Common construction whose `parts` carry **neither**
`Part::Copula` nor `Part::Verb`.

`Part::Verb`'s doc states the current invariant: *"A construction carries
either this or `Part::Copula`, never both: they are the same slot filled two
ways, which is why `verb_group_forms` can enumerate a construction's verb
surfaces by asking which of the two it contains."* A verbless clause carries
neither — a third case.

**`verb_group_forms` already handles it**, and this is verified rather than
assumed: its final arm is `else { Vec::new() }` (`clause.rs:1509-1510`), so a
verbless construction yields no verb surfaces. That is correct forward
behaviour and it is also the parse-side consequence — a construction with no
verb group cannot be found by a parser that locates the verb group first.
Per §1.1 this campaign is production-side, so that is in scope to *state*,
not to fix.

**Not ellipsis.** The rung's note is explicit: *"NOT recoverable from a
preceding question — so not answer ellipsis, and DELIBERATELY not placed under
r166."* Stassen (1997) and Hengeveld (1992) treat zero-copula predication as a
strategy in its own right. Building it as "the nominal construction with the
copula deleted" would encode the analysis the corpus rejects; build it as its
own construction.

**Interaction with the tongue side, which is already half-built:**
`Valence::Nominal`'s doc says a tongue "leaves [the verb slot] empty when it
drew none (a zero-copula tongue)". So tongues already realize verbless
nominal predication as a *drawn* property. Common has no such construction.
**Decision rule:** if the tongue path already produces the rung's shape for a
zero-copula tongue, say so and do not rebuild it; the rung's Common half is
the new work.

### 3.5 r104 `existential` — *There is a body in the marketplace.*

**Extension point:** a transformation over `Valence::Locative`, in the shape
of `realize_common_polar_question`. **Not** a sixth `Valence` variant — §2.

Freeze (1992): existential, locative and possessive predication are one
construction with different arguments fronted. `Valence::Locative` shipped in
The Rail (Task 5) and binds subject, located thing, and the relation between
them via `Part::PredicateWord`.

**Decision rule.**

- If an existential is expressible as a re-presentation of a locative clause
  (a fronted dummy, the pivot in the subject's place) → build it as a realizer
  beside `realize_common_polar_question` and add a row to nothing.
- If it needs a predicate id of its own → that is one row in
  `PREDICATE_VALENCE` at an existing valence, which decision 0326 explicitly
  permits ("a fifth PREDICATE at any of these five is one row").
- If it needs a sixth `Valence` variant → **stop and report** (§2). Do not
  widen the enum.

**The definiteness effect** (§3.3) is the constraint this rung must be able to
state: an existential's pivot is indefinite. Whether it is *enforced* or
merely expressible is a decision for the implementer to make explicit.

---

## 4. Preregistration

**The numbers below are NOT yet preregistered. They are design input, and the
distinction is this spec's most important procedural claim.**

They come from a simulation of the resolver's closure rule whose positive
control reproduces `docs/audits/sentence-coverage.md` at `6f121ac07` exactly —
`(11 covered, 20 frontier, 6/12 merchant, 270 demand instances)`:

| | ladder covered | frontier | merchant | fw demand instances |
|---|---|---|---|---|
| baseline (`6f121ac07`) | 11 / 214 | 20 | 6 / 12 | 270 |
| projected, all five | 19 / 214 | 33 | 8 / 12 | 393 |

The Rail's retrospective, "Do differently next time", first item:

> **Compute a preregistered number with the instrument, not beside it.** If a
> criterion will be judged by a resolver, the prediction is that resolver's
> output on a stubbed input — never a fresh script that reimplements it. Every
> number this campaign got wrong came from the second implementation, and the
> cost of being right was one run.

The table above **is** a second implementation. So:

**PREREG-1.** The plan's first task adds the five tokens to
`IMPLEMENTED_DEMANDS` on a scratch commit and runs the two instruments that
already exist: the assertion
`the_ladder_score_and_frontier_match_the_campaigns_prediction`, and the report
generator `sentence_coverage_report` (which writes
`docs/audits/sentence-coverage.md` itself via `std::fs::write` under
`HV_SENTENCE_REBASELINE=1` — `scripts/regenerate-artifacts.sh:212`,
`gen_sentence_coverage`). It records their output and reverts. Those figures —
not this table — become the preregistered criteria, written into the test's
assertion before any grammar is built.

The stub is a **token-list edit only**. Adding a token to
`IMPLEMENTED_DEMANDS` without its backing test is precisely what §5 forbids
shipping; doing it on a reverted scratch commit to read the instrument is the
opposite act — it is how the campaign learns what to promise. The revert is
not optional and the plan states it as its own step.

**PREREG-2.** The four figures are preregistered *independently*, not as one
composite. A campaign that hits three and misses one has a finding, not a
failure.

**PREREG-3.** The frontier count is expected to move a long way (20 → ~33) and
the direction matters more than the value: the frontier *growing* is the
correct outcome of building foundational rungs, and a shrinking frontier would
mean the campaign built leaves. This is a preregistered *direction*, and it is
the one criterion whose sign is asserted rather than its magnitude.

**PREREG-4.** Per-rung, the report's produce-side demand-instance count is the
figure quoted in the chronicle. Total demand is reported beside it and never
alone — §1.1's whole argument is that the composite hides a direction split.

---

## 5. Coverage discipline

`IMPLEMENTED_DEMANDS` is hand-maintained and its module doc states the rule:

> a token goes in only alongside a test in `domains/language` that realizes a
> clause exercising it, in Common and, where the tongue realizer is the point,
> in a tongue.

**Each of the five tokens ships with its named backing test, and the module
doc gains a sentence naming it** — the existing thirteen each do. A token
added without one moves the score without moving the grammar, which the doc
correctly calls *worse than no instrument, because it would read as
evidence*.

**Common-only is permitted and must be stated.** `epistemic-hedge` is
Common-only "the same posture `classify` takes: no tongue realizer is the
point of this token". Two of The Rail's five rungs shipped Common-only. A rung
that ships Common-only names that in the doc; it does not quietly omit the
tongue half.

---

## 6. Definition of done

- Five tokens in `IMPLEMENTED_DEMANDS`, each with its backing test named in
  the module doc (§5).
- No new `Valence` variant, or a report explaining why the constraint broke
  (§2).
- The four preregistered figures asserted in
  `the_ladder_score_and_frontier_match_the_campaigns_prediction`, derived per
  PREREG-1.
- `docs/audits/sentence-coverage.md` regenerated (`make rebaseline` — it is
  written under `HV_SENTENCE_REBASELINE=1` by `scripts/regenerate-artifacts.sh`,
  and `docs/audits/` is a declared generated path, so an unregenerated edit
  reddens the drift check).
- `docs/audits/type-audit-report.md` regenerated in the **same commit** as any
  `pub`-boundary change — new public items in `clause.rs` will drift it.
- Chronicle entry (`book/src/chronicle/the-quoin.md`), book freshness sweep
  including `book/src/domains/language.md`, and a Confidence Gradient
  re-score if any bet moved (decision 0030).
- Retrospective (`docs/retrospectives/the-quoin.md`) and its row in
  `docs/retrospectives/README.md`.
- Idea-registry rows from §7.
- Chronicle and retrospective written **before** submission to the sluice.

**No corpus file is edited.** All three are frozen; `LADDER_ENTRIES` stays
214, `MERCHANT_ENTRIES` 12, `FLOOD_WATCH_ENTRIES` 139. A campaign that finds
itself wanting to edit a corpus has found something worth reporting and should
report it, not edit.

---

## 7. Captured, not built

Idea-registry rows this campaign files and does not implement:

1. **The build-order resolver.** `the-ladder.corpus.json`'s `shape_notes`
   says computing a build order from a ladder and a corpus together "is CODE —
   a resolver's job — not data (decision 0011)". Nobody has written it. This
   campaign picks five rungs by hand, which is that job done by hand. Set
   aside deliberately: it produces no grammar, and hand-picking is cheaper
   than automating a choice made twice a campaign — but the corpus's own text
   says the automated form is the correct one, and that should be on the
   register rather than in a spec nobody greps.
2. **The parse hemisphere has no instrument.** The ladder is a production
   instrument by declaration; 68 of flood-watch's 139 entries are `parse`. No
   corpus, no ladder and no resolver scores parse capability, and §1.1's
   direction split is the first time the gap has been priced against a
   candidate list.
3. **Depth over breadth.** A rung is not a uniform unit of work — The Rail
   shipped two of five Common-only, and this campaign expects to as well.
   "One rung across every tongue" is an unexplored axis and the honest
   alternative to a rung count.

Carried forward, unscoped here: a coverage resolver tuned to
`the-flood-watch` (The Rail follow-up #4); `witness-set` and
`named-entity-list`, the merchant's last two tokens (§1.2).

---

## 8. Risks

| risk | how it shows | response |
|---|---|---|
| `definiteness` needs referent identity from outside the domain | the discourse structure wants a window's data | §3.3 branch 3 — stop; a domain may not depend on a sibling |
| `existential` needs a sixth valence | the transformation cannot be written | §2 — stop and report; do not widen the enum |
| the campaign is 5 rungs of very unequal size | `definiteness` consumes the budget | shed a *later* rung, never `existential` (§1.1: it is the only chained one whose deferral costs structure) |
| a token enters `IMPLEMENTED_DEMANDS` on optimism | the score moves, the grammar does not | §5 — no token without a named backing test |
| the projected figures get preregistered as-is | a wrong prediction from a second implementation, exactly The Rail's headline defect | PREREG-1 — derive with the instrument, on a stubbed commit, before building |
