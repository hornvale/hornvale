# The Inquest — who did what to whom, and when

*An inquest is the proceeding that establishes, on the record, what happened:
who acted, upon whom, at what time, and what did not happen. It is the grammar
this campaign builds, and the merchant corpus that measures it is literally a
witness statement about a killing.*

**Status**: spec, awaiting G3 review
**Campaign**: The Inquest
**Decision block**: 0296–0305 (reserved; main ceiling 0286)
**Predecessor**: The Scarf (decision 0286) — one `Clause`, two realizers

---

## 1. Where the project is

The Scarf left one clause type realized by Common and by every generated
tongue. That clause can say exactly one thing: **`X is a Y`**. There is no past
tense, no negation, no sentence in which somebody *does* something to somebody,
and no way to refer back to something already mentioned.

Two consequences, and the second is the sharper one.

**The world knows more than the grammar can say.** The Interlinear's own
flagship, live on `main` right now:

> *Nwamvam **is** the home of the hobgoblins, in the clearing at vertex 18822,
> founded in year 25; it **ended** in year 1600.*

The ledger records that the occupation ended. The copula is unconditionally
present-tense, drawn from `number` alone (`clause.rs`'s `CLASSIFY` construction:
`Part::Copula => match spec.number { Sg => "is", Pl => "are" }`). So the
sentence asserts a present state and then reports its own ending six hundred
years earlier.

**And the tongues are, again, ahead of the syntax.** `paradigm.rs` draws
`tense_depth`, `tense_position`, `number_depth` and `number_position` per
species, and `draw_paradigm_affix_proto` draws each family's proto-affix for a
paradigm axis-value. Verified: `grep -rn 'paradigm_depths\|ParadigmDepths\|
draw_paradigm_affix_proto\|ParadigmCell' --include=*.rs .` returns **nothing
outside `paradigm.rs` itself**. Every tongue already knows whether it marks
tense by prefix, by suffix, by particle or not at all — and nothing has ever
asked it.

## 2. What this campaign delivers

Four grammatical features and one concept:

| | | cost |
|---|---|---|
| **Tense** | `Present`/`Past` on the clause | free — drawn, unwired |
| **Polarity** | `Pos`/`Neg` on the clause | one new additive stream label |
| **Transitivity** | a clause whose predicate is an act, with a real verb | a construction + a tongue verb slot |
| **Pronouns** | a drawn per-tongue inventory; `Argument::Pronoun` | new additive stream labels |
| **`kill`** | `ConceptKind::Act`, a causative of the existing core `die` | a registry addition |

Measured against the frozen merchant corpus:

```
                                              covered
  today (classify only)                       0 of 12
  + tense + polarity                          0 of 12
  + transitivity                              1 of 12
  + pronouns                                  2 of 12
```

**`2 of 12` is the first non-zero score in `sentences/`' history.** The two
lines are *"A guard killed a woman."* and *"I didn't know her."* — the corpus's
central event and the merchant's admission about it. Four further entries land
at exactly **one** missing demand.

## 3. Invariants

### 3.1 Decision 0286 still governs, and this campaign widens it

Each realizer ignores part of the clause; that is the law, not a defect. This
campaign adds two features to the shared clause and does **not** make every
realizer surface both:

```
+------------------+---------------------------+---------------------------+
| feature          | Common                    | a tongue                  |
+------------------+---------------------------+---------------------------+
| tense            | is / was                  | per drawn tense_depth     |
| polarity         | is not / did not          | per drawn polarity_depth  |
| number           | copula + plural           | per drawn number_depth    |
| definiteness     | determiner slot           | IGNORED (still)           |
| evidential       | IGNORED (still)           | predicate-final mark      |
+------------------+---------------------------+---------------------------+
```

`definiteness` remains unread by every tongue and `evidential` unread by
Common. Wiring `number` here retires one `IGNORED (today)` cell The Scarf left
deliberately open.

### 3.2 The output stays anti-symmetric

`realize_common -> String` (total); the tongue realizers return
`Result<_, TongueGap>` (partial). Unchanged, and not to be "fixed": a gap must
always mean something true about the world.

### 3.3 Tense is stated, never derived

Number, definiteness and evidential are properties **of** the clause. Tense is
a **relation to a moment outside it** — it is the first feature requiring a
deictic centre. A `Clause` has no access to speech time and must not acquire
one.

So `Clause.tense` is supplied by the caller, which knows both the fact's
`WorldTime` and the utterance's. The realizer surfaces what it is told. A
future campaign that wants automatic tense adds a *caller-side* helper, never a
clock inside the clause.

### 3.4 A new stream label is additive, not an epoch

Verified in `paradigm_depths`: every draw derives its own stream by label path
(`seed.derive(ROOT).derive(species).derive(GRAMMAR).derive(DEPTH).derive(NUMBER)`).
A new label creates a new independent stream and **perturbs no existing
consumption order**, so no existing world's bytes move.

The cost is real but bounded: a permanent label that may never be renamed
(deliberate regeneration uses an epoch suffix), and a regenerated stream
manifest, which is drift-checked.

## 4. Design decisions

### 4.1 Tense marks PAST; present is zero

`draw_paradigm_affix_proto(seed, family, "tense", "past")` already exists and
draws exactly the marked member. Present is unmarked. This is the typologically
standard shape and the machinery already assumes it — we are not choosing it so
much as reading it off what is drawn.

Polarity follows the same shape: `negative` is marked, positive is zero, under a
new `polarity` axis with its own depth and position draws.

### 4.2 The tense marker attaches to the verb, which is why transitivity is in scope

Noun-class marks the complement; evidential marks the copula, or the predicate
nominal under a zero copula. Tense wants a verb — and **a zero-copula nominal
clause has none.** Several tongues draw no copula at all, so `X is a Y` in past
tense would have nothing to bind to, forcing an arbitrary rule about affixing a
tense marker onto a noun.

`ConstituentOrder` is already `Sov/Svo/Vso/Vos/Ovs/Osv` — built for S-V-O, with
the copula standing in for V. A transitive clause supplies a real verb and a
natural host.

**This is the argument for the larger scope: the extra feature makes the core
feature simpler rather than harder.** A tense-only campaign would have had to
invent a host; this one is handed one.

For a nominal clause in a zero-copula tongue, past tense marks the predicate
nominal if the tongue's `tense_depth` is `Affix`, mirroring exactly what the
evidential already does in that position — the precedent exists and is tested.

### 4.3 The flagship demonstration uses `eat`, not `kill` and not `know`

Verified in `packs.rs`:

- **`eat`** — `ConceptKind::Act`, `ladder_rank: 0`, universal stratum,
  transitive. Every tongue has a real word for it.
- **`know`** — registered, but `packs.rs` states outright that nothing grants
  any action concept exposure, so "every species falls through to the generic
  `Experiential` gap".
- **`kill`** — does not exist. This campaign adds it.

**Consequence, and it must be in the chronicle rather than discovered by a
reader: the two corpus lines this campaign covers will realize in Common and
GAP in every tongue** — for vocabulary reasons, not grammatical ones. The
transitive *grammar* works; those two particular *words* are not yet in any
people's mouth.

That is defensible — the corpus's demand tokens name grammatical capabilities,
and the resolver's own doc says so; lexical coverage is the axis `tropes/` and
the lexicon's own gap reasons already measure. But the flagship must not be
built on a gap, so it is built on `eat`.

### 4.4 `kill` creates no obligation to implement combat

`ConceptKind::Act`'s doc says "the GOAP action roster reconciles against exactly
this class", which reads like a two-way obligation. It is not. Verified at
`cli/src/concepts.rs:178`: the audit walks `Action::all()` and reports actions
that **no concept names**. It is structurally blind to a concept with no action.

So a `kill` concept with no planner action is exactly what the audit permits.
`kill` enters as vocabulary — a causative of the existing core `die` — and the
world gains the ability to *say* it long before anything can *do* it. That is
the intended direction of travel, not a shortfall.

**Followup:** `ConceptKind::Act`'s doc should say which direction its
reconciliation runs. A check asserting *actions ⊆ concepts* reads as total to
the next person.

### 4.5 Pronouns carry person and number only — no gender

The drawn inventory covers `{1,2,3} × {Sg,Pl}` — six cells — through the same
proto-plus-`evolve` machinery every other family-cognate morpheme uses, under
new stream labels.

**No gender.** The corpus line is *"I didn't know her"*, and `her` is an
English distinction this world has no basis for: nothing in the ledger assigns
grammatical gender, and inventing one to satisfy one line would be authoring
rather than deriving. Common renders 3sg as `them`. The result is a controlled
register with slightly awkward phrasing, which is the accepted trade.

### 4.6 Pronouns satisfy The Scarf's gap; they do not reverse it

The Scarf ruled that a tongue gaps on `Subject::Pronoun` **because no tongue
draws a pronoun inventory**. Drawing one falsifies the antecedent; the rule
stays true and simply stops firing. The gap was a placeholder pointing at this
work.

`Argument::Pronoun` is added for the object slot, which does not exist today —
`Argument` is `Concept|Name|Count|Quantity`. Per `Argument`'s own doc, a variant
is added "when a role needs it, never speculatively"; a transitive clause with a
pronoun object is that need.

## 5. Non-goals

- **No mood.** The fifth promised speaker feature stays absent; nothing draws it.
- **No aspect.** Tense is `Present`/`Past`; no progressive, no perfect.
- **No questions**, no embedded clauses, no coordination, no temporal
  adverbials, no existentials — the corpus's remaining blockers.
- **No clause-in-clause.** A `Clause` remains an island; that is the next
  ceiling (§7).
- **No combat.** `kill` is a word, not an action.
- **No GOAP action for `kill`**, and no exposure rule granting any tongue the
  action-suite vocabulary.
- **No gender.**

## 6. Success criteria

1. The Interlinear's flagship renders in **past tense**: the occupation ended,
   so Common says *was*, not *is*.
2. A tongue whose drawn `tense_depth` is `Affix` marks past on the verb, and a
   tongue whose `tense_depth` is `None` does not — both asserted, so the
   drawn value is demonstrably read.
3. A transitive clause realizes in Common and in a tongue, demonstrated on
   `eat`, with the tongue's own constituent order honoured.
4. A negated clause realizes in both, and `parse_common` recovers the polarity —
   the round trip holds.
5. A pronoun subject no longer gaps in a tongue; a pronoun object realizes.
6. `IMPLEMENTED_DEMANDS` gains `past-tense`, `negation`, `transitive-frame`,
   `pronoun-reference`, and the corpus resolver reports **2 of 12** covered,
   with `MERCHANT_COVERED` moved in the same commit.
7. The resolver additionally reports **distance** — how many entries sit at one
   missing demand — because a conjunctive score is a lagging indicator and four
   entries move without the headline number moving.
8. Artifact drift resolves per §8.

## 7. What this does not reach, and why it is worth recording

After this campaign the corpus's remaining blockers are:

```
  temporal-adverbial  3     polar-question   2     witness-set  2
  wh-question         2     epistemic-hedge  2
  embedded-clause     2
```

**The next ceiling is connectivity.** A `Clause` has no field pointing at
another `Clause`, which gates `embedded-clause` directly and entangles
`epistemic-hedge` (*"I think her name was Gilda"* is one clause inside another).
That is a structural change, not a feature, and it is the natural successor.

## 8. Artifact branch table

Written as branches, not predictions.

```
after `PATH=$HOME/.deno/bin:$PATH make rebaseline`, per path:

  book/src/gallery/ UNMOVED     -> EXPECTED. Verified before writing this:
                                   `windows/book` renders only the
                                   self-statement and `world_statement`, and
                                   BOTH are present-tense assertions about what
                                   a people or world IS. The book renders no
                                   occupation clause, so nothing it emits is
                                   past-tense-eligible. Proceed.
  book/src/gallery/ MOVED       -> INVESTIGATE, do not assume it is the point.
                                   An earlier draft of this table said a move
                                   here was EXPECTED "and it is the POINT",
                                   which was wrong. A move means either a word
                                   FORM shifted (a new draw perturbed an
                                   existing stream -- see the byte-golden rows,
                                   this is the same fault) or the book was
                                   deliberately changed, which is out of scope
                                   per the note below.
  book/src/reference/ MOVED     -> EXPECTED: the concept registry gains `kill`
                                   and the stream manifest gains the new
                                   labels. Confirm the new labels are exactly
                                   the ones this spec names and no others, then
                                   commit.
  docs/audits/ MOVED            -> EXPECTED: the pub boundary grew.
                                   Regenerate in the SAME commit; gate-commit
                                   cannot catch this (type-audit `check` is a
                                   lint, the report is an artifact).
  cli/tests/fixtures/ MOVED     -> STOP. Byte-goldens. This campaign adds new
                                   STREAMS but must perturb no existing one
                                   (§3.4). A move here means a new draw was
                                   inserted into an existing stream's
                                   consumption order, which is a save-format
                                   break. Diagnose; do not rebaseline.
  windows/vessel/tests/fixtures/ MOVED -> STOP, same reason.
  book/src/domesday/ MOVED      -> STOP. Nothing here touches the census.
```

**The byte-golden rows are the ones that matter**, and this campaign draws new
morphology — exactly the shape that moves them if a label is added in the wrong
place. Three campaigns visible on the board were bounced this way
(`the-deed`, `the-lexicon`, `the-granary`), each on goldens the artifacts phase
deliberately will not fix: only `make rebaseline-goldens` (`REBASELINE=1`)
writes them, so a green artifacts phase says nothing about them.

**And a moved golden here would NOT be an accept-and-move-on.** For most
campaigns a moved golden means "generation changed, as intended". For this one
generation must NOT change: §3.4's whole claim is that new labels are additive.
A moved golden falsifies that claim, and the correct response is to find which
draw was inserted into an existing stream — not to accept new bytes.

**A visible book artifact is deliberately NOT in scope.** The campaign's proof
is its tests, as The Scarf's was. Making the book render a past-tense line
would mean a new chapter — real scope, and a separate campaign. The consequence
is that this campaign's headline change is invisible in the published book,
which is worth saying in the chronicle rather than leaving a reader to notice.

## 9. Stages

| # | Stage | Deliverable |
|---|-------|-------------|
| 1 | Tense and polarity | `Clause.tense`, `Clause.polarity`; Common surfaces both; the tongue wires `paradigm.rs`'s drawn tense depth/position; a new polarity axis drawn. The Interlinear's flagship says *was*. |
| 2 | Transitivity and `kill` | A transitive construction keyed on an `Act` predicate; the tongue's verb slot; `kill` registered as a causative of `die`. Demonstrated on `eat`. |
| 3 | Pronouns | A drawn six-cell inventory per tongue; `Argument::Pronoun`; The Scarf's pronoun gap stops firing. |
| 4 | The score and the record | `IMPLEMENTED_DEMANDS`, `MERCHANT_COVERED`, the distance report; chronicle, retrospective, decisions, registry, artifacts. |

Stages 1–3 are each independently green. Stage 1 alone fixes a live
contradiction on `main` and is worth landing even if the campaign were cut
short.
