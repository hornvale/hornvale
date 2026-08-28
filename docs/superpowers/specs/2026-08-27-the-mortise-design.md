# The Mortise — clause-in-clause connectivity

**Campaign:** The Mortise
**Branch:** `campaign/the-mortise`
**Decision block:** 0326–0335 (main ceiling 0308 at reservation)
**Predecessors:** The Interlinear (`sentences/`), The Scarf (0286), The Inquest (0296, 0297)
**Date:** 2026-08-27

A mortise is the cavity cut into one timber to receive another. The Scarf was a
joint that made two pieces into one length; this one makes a cavity that holds a
second piece whole — and, because a joint is never only one cut, a second way of
setting two pieces side by side.

**Two operators, named apart.** Embedding is a SLOT that holds a clause;
coordination is a LIST at a node. They sound like one idea — "connecting
clauses" — and treating them as one is the trap this spec is written to avoid.
They share a structural insight (both are marked at a BOUNDARY, and that marker
is what keeps the parser's inverse computable) and nothing else.

---

## 1. The ceiling this removes

A `Clause` has no field pointing at another `Clause`. That single absence gates
embedded clauses outright and entangles epistemic hedging — *"I think her name
was Gilda"* is one clause inside another, not one clause with a hedge feature.
Registry row: `LANG-clause-connectivity`.

## 2. The measured case, recomputed

Resolved from `sentences/the-merchant.corpus.json` against
`IMPLEMENTED_DEMANDS` in `cli/tests/suite/sentence_corpus.rs`, computed for this
spec rather than inherited:

```
  today (classify, negation, past-tense, pronoun-reference,
         transitive-frame)                             2 of 12   m05 m10
  + embedded-clause ALONE                              2 of 12   <-- ZERO GAIN
  + epistemic-hedge alone                              3 of 12   +m09
  + embedded-clause + epistemic-hedge                  4 of 12   +m06 +m09
  + coordination                                       5 of 12   +m07   <-- THIS CAMPAIGN
  + temporal-adverbial                                 6 of 12   +m02
```

Coverage is **conjunctive**. Shipping `embedded-clause` on its own moves nothing,
because every corpus line needing it also needs hedging or coordination. This is
the shape that would have made a tense-only campaign land 0→0, and it is why
The Inquest was scoped to four features rather than two.

**This campaign targets 2 → 5** (m06, m07, m09). §9 says why the last row is cut,
and §9.1 says what is cut INSIDE coordination.

## 3. Scope

**In.**

*Embedding.*

1. `Argument::Clause(Box<Clause>)` — object-position embedding, depth-capped.
2. `Subject::Clause(Box<Clause>)` — subject-position embedding, riding the same
   machinery.
3. `Part::Determiner` becomes a no-op when the object is a clause.
4. `know` and `think` as expressible predicates (`PREDICATE_VALENCE` rows;
   concept registration where absent).
5. Subordination strategy as a **drawn** typological axis per tongue.

*Coordination.*

6. A coordination node above the clause — additive, so `realize_common(&Clause)`
   keeps working unchanged.
7. Conjunction presence-and-form as a **drawn** axis per tongue, the second of
   this campaign's two.
8. Subject elision, so a shared subject is stated once.

*The instrument.*

9. The parser follows both operators.
10. **The realization witness** — the corpus score stops being a declaration.
11. A named production caller, or a written declaration of inertness.

**Out**, each with its reason in §9: right-node raising; nominalization and
finiteness alternations; temporal adverbials; relative clauses; indirect
questions (*wh*-embedding); quotation.

## 4. Design

### 4.1 The recursion sites are enum variants, not `Clause` fields

`Argument` gains `Clause(Box<Clause>)`. `Argument`'s own doc states the rule this
follows: *"A new variant is added when a role needs it, never speculatively."*
The role is the clause complement of `know` / `think`.

**Measured, not argued.** The tree holds 65 `Clause { … }` literal construction
sites — 27 in `clause.rs`, 38 outside it. A new **field** on `Clause` costs an
edit at every one of them. A new **`Argument` variant** costs only the exhaustive
match sites, and the compiler enumerates them: `resolve_argument`
(`grammar.rs:203`) is a total match, with 38 `Argument::` references in
`grammar.rs`, 15 in `windows/book/src/lib.rs`, 8 in an almanac test.

`Subject` gains `Clause(Box<Clause>)` on the same argument, and it is cheaper
than it looks. Only **two** places consume a `Subject` structurally —
`tongue_subject` (`grammar.rs:316`) and `Part::Subject` (`clause.rs:782`) — and
both will already hold "how do I realize a nested clause here" logic, built for
the object slot. The other ~75 `Subject::` references are construction sites,
which a new variant does not break.

A side effect that must be decided rather than inherited: `Adjunct` holds an
`Argument` (`Adjunct { role, argument }`), so the moment `Argument` gains a
`Clause` variant, **adjuncts can carry clauses**. This spec **refuses** that: an
adjunct whose argument is a clause is rejected, not realized. Adverbial
subordination is a separate construction with its own boundary marking, and
letting it arrive as an unexamined side effect is how a capability ships without
a decision.

### 4.2 No `Valence::Sentential` — the overturn

The obvious design adds a `Valence::Sentential`. It is wrong here, and the file
says why in its own words.

`know` takes an NP object (*"I didn't know her"*, m10) **and** a clause object
(*"I don't know why he killed her"*, m06). `PREDICATE_VALENCE` is
`&[(&str, Valence)]` resolved through `.find()` — first row wins — so one
predicate cannot carry two valences without re-keying the table on
`(predicate, object-shape)`. `clause.rs:544-552` states the tripwire: *"If a
future campaign finds itself adding a variant per predicate, it has rebuilt
`Frame` and should stop."*

So a clause object rides `Valence::Transitive`. A predicate taking an
NP-or-clause object is **one** argument structure with a category-flexible
object, which is exactly what `Valence` says it sorts predicates into. One table
survives; no agreement test is created that could rot.

The cost is one honest special case: `Part::Determiner` must emit nothing when
the object is a clause, in the one place that already switches on the object's
shape (`clause.rs:807-814`). *"I do not know a he killed her."* is what not doing
this produces.

### 4.3 Depth is bounded, in both directions

`Clause` derives `Clone, Debug, PartialEq` and **not** `Serialize`, so recursion
carries no save-format risk on the struct itself. And `Box<Clause>` is unique
ownership with no `Rc` and no shared borrow, so **a clause graph cannot cycle**:
infinite regress is not a failure mode here. Only unbounded *depth* is.

**Nesting is capped at one level: a clause complement may not itself contain a
clause complement.** The realizer refuses beyond the cap and the parser stops
descending at it.

**The cap is a statement of demonstrated depth, not a safety belt**, and the
distinction matters because the safety reading does not survive examination.
Realization and parsing do become recursive, but every caller in this tree is
repo code — nothing constructs a `Clause` from untrusted input — so a stack
overflow would have to be built deliberately, by hand, ten thousand `Box::new`
deep. That is not a hazard the cap is protecting against.

What the cap actually says is: *one level is the depth this campaign builds,
tests and can show working.* An uncapped `Box<Clause>` ships reach that nothing
constructs and nothing covers, which is the
`LANG-in-character-acts-are-unspeakable` shape — The Deed's seven inert concepts
— arriving through a type rather than through a registry. Refusing past the
tested depth is how the capability stays honest about its own size.

Why one and not some larger N is then immediate: no corpus entry needs two,
nothing in the world constructs one, and a cap above demonstrated need is
authoring a distinction nothing states — the same argument `Clause.number`'s doc
already makes about a second number field. Raising it later is additive and costs
no epoch.

### 4.4 Tense stays absolute — no backshifting

*"I think her name **was** Gilda"* admits two readings: past relative to the
utterance, or past relative to *think* (sequence-of-tense). Decision **0296**
settles it: *tense is stated, never derived; a `Clause` has no clock and must not
acquire one.* A realizer that backshifts an inner clause computes a relation
between two clauses' deictic centres — precisely the machinery 0296 forbids.

**The inner clause's `tense` is absolute and caller-stated, exactly as the
matrix's is.** No realizer reads one to adjust the other.

### 4.5 The inner clause keeps its own grounding

An embedded clause is a full `Clause`, so it carries its own `evidential`,
`number`, `definiteness` and `polarity`. The matrix does not rewrite them.

This is where a per-clause `evidential` first earns its keep. The Scarf's law
(0286) has Common ignoring `evidential` entirely, so *"I heard that she
allegedly…"* — two groundings in one utterance — is a **tongue-only** payoff, and
this spec claims nothing more for it than that the field is not clobbered.

### 4.6 Subordination is drawn, not assumed

`windows/worldgen` already draws constituent order, copula presence, marker depth
and marker position per people. A hardcoded complementizer would make every
tongue subordinate like English — the exact failure `realize_tongue` exists to
prevent.

The strategy is a drawn axis over at least: a **complementizer** (a free boundary
word) and **bare parataxis** (juxtaposition, no marker). Both are attested and
neither is degenerate; a language with no subordinator is not a language that
cannot subordinate.

**This is a save-format contract.** A new axis means a new permanent stream
label, additive, on the pattern The Inquest's polarity axis established. It leads
the flagged section below.

### 4.7 The parser follows

`common_constructions`' doc calls the form↔meaning pairing *"bidirectional by
construction"*. Realizing a shape nothing can parse would break that promise
silently, so the parser extends.

The extension is small because the existing walk already does the right thing:
`parse_common_with_tail` splits at the **earliest** verb-group occurrence, which
for a right-branching complement is the matrix verb. The recursion attaches at
the one place the walk currently gives up — `best_complement.ok_or_else(||
ParseError::UnknownComplement …)` (`clause.rs:1168`) — by attempting a recursive
parse of the unresolved remainder, under the §4.3 depth budget, before reporting
the error.

**A limit that is not closed here, stated so it is not mistaken for one that is:**
the parser already cannot recover `Argument::Pronoun` — it returns
`Argument::Concept` unconditionally (`clause.rs:1188`). Bidirectionality was
already partial before this campaign and remains so after it.

### 4.8 The realization witness

`sentence_corpus.rs`'s module doc names its own weak point: `IMPLEMENTED_DEMANDS`
is *"a hand-maintained declaration and nothing mechanically proves it"*, and
*"a token added on optimism moves the score without moving the grammar — which
would make the instrument worse than no instrument, because it would read as
evidence."*

This campaign supplies the missing half. **For every entry the resolver declares
covered, a committed `Clause` and the Common surface `realize_common` actually
produces for it, recorded beside the corpus's own text.**

It does **not** assert equality with the corpus's English. Common is a limited
register; that assertion would fail for reasons unrelated to grammar, and a
witness that cannot pass teaches nothing. It asserts that a covered entry has a
constructible clause that realizes without panicking, and it records the distance
so a reader can see it.

**It goes red on arrival, and that is the point.** m10 is scored covered today
and `realize_common` panics on it: `PREDICATE_VALENCE` holds only `is-a`, `eat`
and `kill`, and the lookup at `clause.rs:758-767` panics on a miss. A guard that
has never been red proves nothing about what it would catch. §3's `know` row is
what turns it green — one line, declined on purpose by The Inquest, and this is
the campaign that has a reason to add it.

### 4.9 Something must actually say one

`LANG-in-character-acts-are-unspeakable` records this repo's own version of the
failure: The Deed minted 14 concepts and **seven are inert**, verified in the
committed manifest, because three successive tasks each shipped without deciding
who would wire them.

So: this campaign **names a production caller** that emits an embedded clause
into a committed, drift-checked artifact — or it declares the capability
deliberately inert and writes down why. Not deciding is the one outcome
foreclosed.

### 4.10 Coordination is a list at a node, not a slot

`realize_common` takes a `&Clause` and always will. Coordination arrives
**above** it, as an additive node — a coordinated utterance and its own entry
point — so none of the 65 `Clause { … }` construction sites move and no existing
caller changes.

**The conjunction is drawn, on the copula's exact pattern.** `tongue_grammar`
(`grammar.rs:145-156`) draws copula presence *and* form from one stream: 60% of
tongues have a copula, and the ones that do get a form. The conjunction axis is
the same shape — some tongues mark coordination with a word, some juxtapose —
and it costs **one** static stream label, not one per word.

That is worth stating plainly because the intuition runs the other way: a
vocabulary word costs **zero** labels, because a word is a `dynamic(concept)`
value on an axis that already exists (`etymology.rs:433`). A *function* word
costs a label, because a function word's PRESENCE is typological rather than
lexical — the tongue that has no conjunction is not missing a word, it has a
different grammar.

**Coordination decomposes into three tiers at very different prices, and this
campaign builds two.**

```
tier  example                                     needs                  here
----  ------------------------------------------  ---------------------  ----
 1    "It confused me and it upset me."           a list node +          IN
                                                   a drawn conjunction
 2    "Seeing it confused me and upset me."       + subject elision      IN
 3    "Seeing it confused and upset me."          + right-node raising   OUT
```

Tier 3 is the corpus's exact wording, and §9.1 says why it is out.

**The parser's discriminator falls out for free**, which is the symmetry that
justifies shipping the two operators together. A sentence with two verb groups
is embedding *or* coordination, and the marker says which: a complementizer means
*a clause hangs below here*, a conjunction means *a clause sits beside here*.
Boundary markers are what make an inverse computable — the same fact
`common_constructions`' doc already states about the closed construction table,
arrived at from the other side.

## 5. Determinism and save-format

- `Clause` is not `Serialize`; recursion adds no serialized shape.
- **Two** new permanent stream labels: §4.6's subordination strategy and
  §4.10's conjunction. Additive, never renamed; the epoch-suffix rule
  (`settlement/name/v2`) applies to any later regeneration. These are the
  campaign's only one-way doors.
- Vocabulary adds **no** labels. A word is a `dynamic(concept)` value on the
  existing `PROTO_ROOT` axis (`etymology.rs:433`); only a new KIND of draw earns
  a static label.
- Any new concept registration appends a **new** cohort to `EPOCH_COHORTS`,
  never edits an existing one — epoch-first sorting makes a later concept land
  strictly last, displacing nothing. Before that fix `otyugh-kind` alone moved
  65 facts.
- Byte-goldens will move. **The discriminator is whether an existing VALUE
  moved**, not whether the file changed and not whether lines were deleted:
  registering a concept adds its row to the serialized registry, which is benign
  and is not a draw perturbation. The corrected table is
  `2026-08-26-the-inquest-design.md` §8.

## 6. Success criteria (preregistered, before the code that would move them)

1. `Argument::Clause(Box<Clause>)` exists; `resolve_argument` and every other
   exhaustive match handle it; no new `Valence` variant was added.
2. `realize_common` produces an embedded complement with no determiner before it.
3. `realize_tongue` and `realize_tongue_deep` realize an embedded clause using a
   **drawn** subordination strategy, and a tongue that drew parataxis emits no
   marker.
4. A coordinated utterance realizes in Common and in a tongue, with a **drawn**
   conjunction, and a tongue that drew none joins by juxtaposition.
5. A shared subject is stated once (tier 2), and right-node raising is NOT
   attempted — the witness records the surface either way.
6. `parse_common` round-trips a one-level embedded sentence to an equal `Clause`,
   and distinguishes embedding from coordination by the boundary marker.
7. Nesting beyond the §4.3 cap is refused in both directions, by an assertion
   that fails without the cap.
8. The merchant score moves **2 → 5**, and the covered set is exactly
   `m05 m06 m07 m09 m10`. The distance report moves with it.
9. The realization witness exists and covers every covered entry, and its RED
   is demonstrated rather than assumed: the implementer finds a perturbation of
   the grammar that a covered entry depends on, applies it, and records the
   witness failing. **The perturbation is not prescribed here** — a plan author
   outside the code does not know which changes are observable, and a
   non-observable one produces a green that looks like a robust guard. Name the
   property, hunt for the mutation from inside.

   The property: *a covered entry whose construction has been neutralized must
   fail the witness, not pass it.*
10. §4.9 is discharged one way or the other, in writing.
11. **Two** stream labels were added and `stream_labels()` reports every path
    the new draws create — counted, not assumed (§8).

**A falsified prediction is a finding, not a failure.** If §4.6's drawn axis
turns out to be unreachable at the floor the way `Evidential::Inferred` is, that
is the campaign's headline and the null ships.

## 7. Non-goals

Right-node raising (coordination tier 3). Nominalization and finiteness
alternations — no gerunds. Temporal adverbials. Relative clauses. Indirect
questions. Quotation. Adjectival predication. Mood and aspect (`LANG-mood`,
`LANG-aspect`). Closing the parser's pronoun gap. Recovering the adjunct tail.

## 8. Risks and traps

- **A rebuild-from-source inside a layering pipeline is latent until a second
  layer exists.** The evidential arm rebuilt the copula from `grammar.copula`;
  once tense affixed first it would have silently discarded the tense join — no
  panic, no red. A nested clause is a *third* consumer of the same assembly.
  Whatever this campaign adds must **thread** its input, never reconstruct it.
- **`stream_labels()` has no completeness check.** A draw added without its
  roster entry yields a silently incomplete manifest and nothing goes red. This
  campaign adds **two** axes (§4.6, §4.10), and The Inquest hit exactly this: the
  plan named two labels for one axis and the truth was three, the third reached
  through a dynamic leg no roster can see. **Count the paths the draws actually
  create; do not count the labels the spec names.**
- **`cli/tests/suite/lexicon_guard.rs` counts `cell` in doc-comment prose**, and
  linguistics jargon collides with this repo's place vocabulary. Four Inquest
  tasks met it; write **row**.
- **A test rename is a commit-gate change** — `docs/timings/subfloor-roster.tsv`
  selects by exact name. `merchant_coverage_is_two_of_twelve` **must** be renamed
  by criterion 6, so that file changes in the same commit or the test silently
  leaves `gate-commit` while everything stays green.
- **Adjacency, flagged not decided:** `windows/book`'s readout law
  (`lib.rs:3500`) forbids `Evidential::Inferred` at any production site as
  floor-unreachable. An epistemic hedge is a matrix *construction*, not an
  evidential *feature*, so the two are orthogonal and no amendment is proposed —
  but a §4.9 caller working in that file should know the law is there.

## 9. What is cut, and why

### 9.1 Inside coordination: right-node raising and the gerund

m07 is *"Seeing it confused and upset me."* An earlier draft of this spec cut it
whole, pricing it at three mechanisms for one entry. **That pricing was wrong in
two places and is corrected here**, because the correction is the reason the
scope moved:

- **`Subject::Clause` is cheap, not expensive.** Two structural consumers, both
  already gaining nested-clause logic for the object slot (§4.1).
- **"Coordination" is not one mechanism at one price.** It is the three-tier
  ladder in §4.10, and tier 1 is genuinely small.

What is still cut, and stays cut:

**Right-node raising (tier 3).** *"confused and upset me"* shares BOTH the
subject and the object across the two verbs. We ship *"confused me and upset
me"*. Ellipsis is where quality dies rather than merely where effort is: the
rules for what may be dropped are language-specific, and getting them wrong
produces *plausible* garbage rather than obvious garbage — the failure mode that
survives review.

**The gerund.** *"Seeing it"* is a nominalization. It is not load-bearing for
m07's demands, only for m07's exact wording: substitute the subject form and hold
everything else constant — *"**That he killed her** confused me and upset me"* —
and the same three demand tokens are satisfied through the complementizer §4.6
already builds. Nominalization is a real feature and a later one.

**What makes both cuts honest rather than hidden is §4.8.** Without the witness,
m07 scores covered while the grammar quietly produces a different sentence — the
m02 trap exactly. With it, our surface sits next to the corpus's where anyone can
read the distance. The witness and the coordination scope are not independent
decisions, and the spec should not pretend they are.

### 9.2 Temporal adverbials — and this one is a finding

m02 (*would be 6 of 12*) is *"Everything was fine until last night."* Its demands are `past-tense` and
`temporal-adverbial` only. But *"Everything was fine"* is **adjectival**
predication, and `Valence::Nominal` renders `Subject Copula Determiner
Complement` (`clause.rs:656-665`) with `Definiteness` offering only `Indef` and
`Def` and no bare singular (`clause.rs:807-814`). Shipping the token would move
the headline to 6 of 12 while producing *"Everything was a fine."*

The cheap reversal is a trap worth naming: adding a `Definiteness::Bare` variant
would produce the right *string* — but only by modelling an adjective as a
concept and asserting *"Everything is-a fine"*, using the classification relation
for property predication. That is authoring a false distinction to move a number.
The honest reversal is a property-predication valence, which is a real
construction and a real campaign.

m02 is therefore the **second** corpus entry whose hand-authored demand tokens
under-describe it — m10 is the first, and is documented at
`sentence_corpus.rs`'s `MERCHANT_COVERED_IDS`. m10's gap is *lexical*, which the
corpus was founded to keep separate from a grammatical one; m02's gap is
**grammatical**, which is precisely what the score claims to measure. That is a
different and more serious class, and it is the strongest argument for §4.8.

**The corpus is not edited to fix this.** It is frozen data (decision 0016);
retro-labelling entries after seeing what the grammar can reach is a
post-unblinding change, however honest its direction. The witness exposes the
same gap without touching frozen data.

### 9.3 The instrument itself is nearly spent

Computed against the corpus, not estimated. `past-tense` appears in **7 of 12**
entries and is already built, so the remaining blockers are thin and spread:

```
  temporal-adverbial   3        existential         1
  wh-question          2        named-entity-list   1
  polar-question       2
  witness-set          2
```

No single remaining token unlocks more than three entries, and the distance
report shrinks to three one-away rows (m01 `wh-question`, m02
`temporal-adverbial`, m08 `polar-question`). Diminishing returns here are
structural, not a matter of choosing the next feature better. **The successor is
a second corpus, not more merchant features** — recorded now rather than
discovered by the next campaign scoping against a flat instrument.

## 10. Decisions this campaign expects to ratify

From the reserved block 0326–0335:

- A clause complement rides the existing transitive frame; there is no sentential
  valence (§4.2).
- Embedding and coordination are two operators, not one (§4.10) — a slot versus
  a list, sharing only boundary marking.
- Embedding nests one level, by a stated cap (§4.3).
- Subordination strategy and conjunction are drawn per tongue (§4.6, §4.10) —
  two save-format contracts. Vocabulary adds none.
- The corpus score is demonstrated, not declared (§4.8).

Gaps inside the block are fine and cost nothing.
