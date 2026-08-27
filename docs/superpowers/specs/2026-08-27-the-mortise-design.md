# The Mortise — clause-in-clause connectivity

**Campaign:** The Mortise
**Branch:** `campaign/the-mortise`
**Decision block:** 0326–0335 (main ceiling 0308 at reservation)
**Predecessors:** The Interlinear (`sentences/`), The Scarf (0286), The Inquest (0296, 0297)
**Date:** 2026-08-27

A mortise is the cavity cut into one timber to receive another. The Scarf was a
joint that made two pieces into one length; this one makes a cavity that holds a
second piece whole.

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
  + coordination                                       5 of 12   +m07
  + temporal-adverbial                                 6 of 12   +m02
```

Coverage is **conjunctive**. Shipping `embedded-clause` on its own moves nothing,
because every corpus line needing it also needs hedging or coordination. This is
the shape that would have made a tense-only campaign land 0→0, and it is why
The Inquest was scoped to four features rather than two.

**This campaign targets 2 → 4** (m06, m09) with one mechanism. §9 says why the
next two rows are cut.

## 3. Scope

**In.**

1. `Argument::Clause(Box<Clause>)` — object-position embedding, depth-capped.
2. `Part::Determiner` becomes a no-op when the object is a clause.
3. `know` and `think` as expressible predicates (`PREDICATE_VALENCE` rows;
   concept registration where absent).
4. Subordination strategy as a **drawn** typological axis per tongue.
5. The parser follows: a recursive fallback at the existing
   `UnknownComplement` site.
6. **The realization witness** — the corpus score stops being a declaration.
7. A named production caller, or a written declaration of inertness.

**Out**, each with its reason in §9: coordination and `Subject::Clause`;
temporal adverbials; relative clauses; indirect questions (*wh*-embedding);
nominalization and finiteness alternations; quotation.

## 4. Design

### 4.1 The recursion site is an `Argument` variant, not a `Clause` field

`Argument` gains `Clause(Box<Clause>)`. `Argument`'s own doc states the rule this
follows: *"A new variant is added when a role needs it, never speculatively."*
The role is the clause complement of `know` / `think`.

**Measured, not argued.** The tree holds 65 `Clause { … }` literal construction
sites — 27 in `clause.rs`, 38 outside it. A new **field** on `Clause` costs an
edit at every one of them. A new **`Argument` variant** costs only the exhaustive
match sites, and the compiler enumerates them: `resolve_argument`
(`grammar.rs:203`) is a total match, with 38 `Argument::` references in
`grammar.rs`, 15 in `windows/book/src/lib.rs`, 8 in an almanac test.

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

## 5. Determinism and save-format

- `Clause` is not `Serialize`; recursion adds no serialized shape.
- §4.6's drawn axis adds **one new permanent stream label**. Additive, never a
  rename; the epoch-suffix rule (`settlement/name/v2`) applies to any later
  regeneration.
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
4. `parse_common` round-trips a one-level embedded sentence to an equal `Clause`.
5. Nesting beyond the §4.3 cap is refused in both directions, by an assertion
   that fails without the cap.
6. The merchant score moves **2 → 4**, and the covered set is exactly
   `m05 m06 m09 m10`. The distance report moves with it.
7. The realization witness exists and covers every covered entry, and its RED
   is demonstrated rather than assumed: the implementer finds a perturbation of
   the grammar that a covered entry depends on, applies it, and records the
   witness failing. **The perturbation is not prescribed here** — a plan author
   outside the code does not know which changes are observable, and a
   non-observable one produces a green that looks like a robust guard. Name the
   property, hunt for the mutation from inside.

   The property: *a covered entry whose construction has been neutralized must
   fail the witness, not pass it.*
8. §4.9 is discharged one way or the other, in writing.

**A falsified prediction is a finding, not a failure.** If §4.6's drawn axis
turns out to be unreachable at the floor the way `Evidential::Inferred` is, that
is the campaign's headline and the null ships.

## 7. Non-goals

Coordination. Subject-position embedding. Relative clauses. Indirect questions.
Nominalization and finiteness alternations. Quotation. Adjectival predication.
Mood and aspect (`LANG-mood`, `LANG-aspect`). Closing the parser's pronoun gap.
Recovering the adjunct tail.

## 8. Risks and traps

- **A rebuild-from-source inside a layering pipeline is latent until a second
  layer exists.** The evidential arm rebuilt the copula from `grammar.copula`;
  once tense affixed first it would have silently discarded the tense join — no
  panic, no red. A nested clause is a *third* consumer of the same assembly.
  Whatever this campaign adds must **thread** its input, never reconstruct it.
- **`stream_labels()` has no completeness check.** A draw added without its
  roster entry yields a silently incomplete manifest and nothing goes red. Count
  the paths the §4.6 draws actually create.
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

**Coordination and `Subject::Clause` (m07, would be 5 of 12).**
*"Seeing it confused and upset me"* needs three mechanisms, not one: a
nominalized (gerund) subject clause, verb-phrase coordination, and a second
variant in a second enum. That is +1 entry for roughly three mechanisms against
embedding's +2 for one. And coordination is a **different operator** from
embedding — a slot that takes a clause versus a *list* at a node — so shipping
both under the name "connectivity" would ship two mechanisms as one. m07 is the
only entry needing subject-position embedding, so cutting coordination cuts
`Subject::Clause` with it cleanly.

**Temporal adverbials (m02, would be 6 of 12). This one is a finding.**
m02 is *"Everything was fine until last night."* Its demands are `past-tense` and
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

**After this campaign the merchant corpus is close to spent as a scoping
instrument.** `past-tense` covers 7 of 12 and is already built; the remaining
blockers are thinly spread and five tokens are singletons, so no single remaining
token unlocks more than three entries. The successor is a **second corpus**, not
more merchant features. Recorded now rather than discovered next campaign.

## 10. Decisions this campaign expects to ratify

From the reserved block 0326–0335:

- A clause complement rides the existing transitive frame; there is no sentential
  valence (§4.2).
- Embedding nests one level, by a stated cap (§4.3).
- Subordination strategy is drawn per tongue (§4.6) — save-format contract.
- The corpus score is demonstrated, not declared (§4.8).

Gaps inside the block are fine and cost nothing.
