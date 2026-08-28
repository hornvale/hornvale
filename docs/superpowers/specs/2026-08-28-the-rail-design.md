# The Rail — the frontier, and the five rungs standing on it

**Campaign:** The Rail
**Branch:** `campaign/the-rail`
**Decision block:** 0416–0425 (main ceiling 0388 at reservation)
**Predecessors:** The Interlinear (founded `sentences/`), The Inquest, The Mortise,
The Stile (one resolver, three corpora), and this campaign's own freeze commit
`c88a61a58`
**Date:** 2026-08-28

The Stile seated the rungs. A rail is what you actually climb — and what tells
you which rung is next.

---

## 1. Where the ladder stands, re-derived

`sentences/the-ladder.corpus.json` is frozen at 214 rungs (`LADDER_ENTRIES`,
this campaign's own first commit). Resolving it against `IMPLEMENTED_DEMANDS`
by transitive closure of `presupposes`:

- **covered: 1 of 214** — `r001` (`classify`) alone.
- **frontier: 5** — rungs every one of whose dependencies is covered but which
  are not themselves covered:

```
  r002  intransitive-frame     "The guard sleeps."
  r003  property-predication   "The road is long."
  r005  locative-predication   "The merchant is at the gate."
  r011  person-deixis          "I am a merchant. You are a guard."
  r083  polar-question         "Are you a merchant?"
```

Recomputed here rather than carried from the handoff, and it agrees.

**1 of 214 is not a defect.** The merchant corpus reads 5 of 12 and the ladder
reads 1 of 214 because they measure different things: the corpus asks whether
the grammar can say twelve particular sentences, the ladder asks whether it
built the stack underneath them. Almost everything `domains/language` can do
today rests on foundations it never built.

### 1.1 `r002` is the lever, and it is bigger than it looks

`intransitive-frame` sits in the transitive closure of **seven of the eight
tokens already implemented**. Landing it alone moves `negation` (`r013`),
`past-tense` (`r014`) and `transitive-frame` (`r006`) to covered:

```
  +r002 intransitive-frame   -> covered 6 (+5)   frontier 14
  +r003 property-predication -> covered 2 (+1)   frontier  7
  +r005 locative-predication -> covered 2 (+1)   frontier  4
  +r011 person-deixis        -> covered 2 (+1)   frontier  4
  +r083 polar-question       -> covered 2 (+1)   frontier  6
  all five                   -> covered 11       frontier 20
```

**These numbers were wrong in this document until Task 2 ran, and the
correction is recorded here rather than applied silently.** The drafting
session's own script treated a control rung's `null` `introduces` as a demand
token, so `{None} | tokens` was never a subset of the implemented set and all
fifteen control rungs were permanently excluded from *covered*. The resolver
does the right thing — `derived_demands` skips a `null` — so the first task to
land a token reported 6/14 where this table predicted 5/14, and the
implementer used the resolver's answer rather than forcing the prediction,
which is exactly the instruction it was given.

The last row reads **11, not 5**, and the six extra rungs come from two
different mechanisms.

Three are rungs whose own token is **already implemented** and which `r002`
alone holds out of covered: `r006` (`transitive-frame`), `r013` (`negation`)
and `r014` (`past-tense`).

Two are **control rungs** — of the fifteen rungs that introduce no token at
all, `r015` (*"The guard did not sleep."*, negation and past tense composed)
and `r190` (*"You killed her."*, a transitive with a second-person agent in
the past) become covered the moment their presuppositions do. A control rung
is free coverage by construction: it demands only what its ancestors
introduce.

The newly covered set is exactly
`r002 r003 r005 r006 r011 r013 r014 r015 r083 r190`.

**This paragraph has been wrong twice, in opposite directions, and both drafts
were confident.** The first credited the control rungs for a count they had no
part in. The second — written as a *correction* of the first — asserted that
none of the fifteen becomes covered at all, which is the error this note
replaces. Both were produced by the same buggy script; neither was caught by
re-reading, and the resolver settled it on the first task that ran.

### 1.2 `r005` is NOT the three-for-one lever its note suggests

The rung's own note cites Freeze (1992): locative, existential and possessive
predication share one underlying construction across a wide typological range,
"which is why this rung, `r019` and `r104` sit on the same branch". That is
true about the *construction* and false about the *edges*, and the edges are
what the frontier is computed from:

- `r104` (`existential`) presupposes `r005` **and** `r007` (`definiteness`).
- `r019` (`possession-alienable`) presupposes **only** `r007`, and does not
  reference `r005` at all.

The gate on both is `r007`, not the locative construction. Building `r005`
buys `r005`. The cost profile does not improve, and no task in this campaign
is scoped on the assumption that it does.

---

## 2. What the five rungs actually buy, measured

Nobody had measured the five against the **dialogue** corpora. Doing so
re-prices the campaign, so it is stated before any code:

```
                          entries covered      demand-instances met
  the-merchant   now         5 / 12              19 / 30    (63.3%)
                 after       6 / 12  (+m08)      21 / 30    (70.0%)

  the-flood-watch now        0 / 139            175 / 1128  (15.5%)
                 after       0 / 139            270 / 1128  (23.9%)
```

The five rungs move the flood-watch headline **not at all** while meeting 95
more demand instances — a 54% relative gain the binary statistic cannot
express. That is not a wash and not a reason to rescope: `person-deixis` (44
entries), `property-predication` (21) and `locative-predication` (16) are among
that corpus's highest-fanout blockers. Its entries simply demand 4–8 tokens
each, so nothing completes.

**The two instruments disagree and both are right.** The ladder's frontier is
the typologically correct build order; entry-coverage is the wrong statistic
for a corpus whose entries are that deep. The remedy is a **complementary**
statistic, never a replacement — `the-merchant` reading 5 of 12 by its existing
unmodified method is comparable across four campaigns and decision 0016 binds
the scoring method as well as the corpus.

---

## 3. Design

### 3.1 The frontier is published, not recomputed by hand

`docs/audits/sentence-coverage.md` gains a ladder score and a **frontier**
section: the covered count, the frontier rungs with their ids, tokens and
texts, and — for each — how many rungs it would unblock. The generator is the
existing `sentence_coverage_report` test under `HV_SENTENCE_REBASELINE=1`
(The Stile gave that file a real writer; it had none before), so the frontier
is drift-checked through `docs/audits/` like every other artifact.

This is the campaign's name. The ladder stops being an instrument that is read
once at a campaign's start and becomes one that states the build-next list on
every regeneration.

### 3.2 `Valence` widens along Stassen's taxonomy, and that is not decision 0326's tripwire

`Valence` today is `{Nominal, Transitive}` and has exactly **three** structural
consumers — the construction selector in `clause.rs`, the tongue verb slot in
`grammar.rs`, and its own tests. It gains three variants: `Intransitive`,
`Property`, `Locative`.

Decision 0326 (which refused a `Valence::Sentential`) warns, verbatim, that
*"if a future campaign finds itself adding a variant per predicate, it has
rebuilt `Frame` and should stop"*, and 0297 makes the valence-to-parts
selection the mechanism that keeps a new predicate to one row. This is not that
failure mode, and the spec says so out loud rather than leaving a reviewer to
wonder:
`Nominal`/`Property`/`Locative`/`Intransitive` are **exactly** Stassen (1997)'s
four intransitive predication strategies — nominal, adjectival, locational and
verbal. It is a closed typological taxonomy, not an open per-predicate list. A
fifth strategy would need Stassen to be wrong; a fifth *predicate* is a row.

The part lists, in the notation `common_constructions` already uses:

```
  PROPERTY   [Subject, " ", Copula, " ",                    Complement, Tail, "."]
  LOCATIVE   [Subject, " ", Copula, " ", Adposition, " ", Det, Complement, Tail, "."]
  INTRANS    [Subject, " ", Verb,                                       Tail, "."]
```

`PROPERTY` is `CLASSIFY` with `Part::Determiner` removed. That is the honest
fix The Mortise's m02 trap was waiting for: *"The road is long"* comes out
right with **no** `Definiteness::Bare` and **no** modelling of an adjective as
a concept the subject is classified into. The Mortise declined `Bare` because
it produced the right string by asserting *road is-a long*; a property valence
produces the same string by asserting property predication, which is what the
sentence means.

`Part::Adposition` is new and is filled from the predicate the way `Part::Verb`
is, so `at`/`in`/`on` are rows sharing one part list rather than one
construction each. Both directions still read one table: `verb_group_forms`
keys off `Part::Copula` vs `Part::Verb`, and every part list here carries
exactly one of them.

### 3.3 The absent object

An intransitive clause has no object, and `Clause.object: Argument` is
mandatory. Measured:

- `Option<Argument>` — **105** full-literal construction sites across five
  files, every one found by the compiler, none of them subtle. Honest, and a
  large conflict surface against the sixteen live worktrees.
- `Argument::Absent` — **~5** structural match arms.

Take `Absent`. The kernel already spells an objectless assertion
`Value::Flag(true)` — `Fact.object` is mandatory too, and `IS_PERSON`,
`IS_BELIEF`, `IS_NEIGHBOR` and `TIDALLY_LOCKED` are all committed that way
today — so the fact-shape claim decision 0266 makes survives: the utterance is
still a fact, and this is the object that fact carries. **A `Flag(bool)` variant is rejected** rather than merely not chosen —
`Flag(false)` with `Polarity::Pos` and `Flag(true)` with `Polarity::Neg` would
be two spellings of one denial, and the round-trip could not choose between
them.

`Argument`'s own rule is that a variant is added when a role needs it, never
speculatively. `Absent` follows the mechanism and not the rationale — no role
needs it, the *absence of a role* does — and that departure is recorded in the
variant's own doc rather than glossed.

### 3.4 Person deixis introduces Common's first syncretism

`COPULA_PARADIGM` is keyed `(Tense, Number, Polarity)` and
`copula_paradigm_is_total_and_unambiguous` asserts it is injective, because the
parser reads features backwards off the surface form. Person breaks that and
English is the reason: `are` becomes 2sg, 1pl, 2pl and 3pl; `were` the same
four.

The resolution keeps the forward direction total and pins the backward loss:
realization stays a total function of the features, the parse returns a
**canonical** row, and a test states which distinctions are unrecoverable —
exactly the posture the round-trip property already takes toward adjuncts,
which it recovers as surfaces and not as structure. The invariant's name
changes with its content; an assertion that quietly kept the old name over new
behaviour would be worse than the syncretism.

`VERB_PARADIGM` gets the same treatment. The two roughnesses
`common_has_no_person_agreement_and_one_third_person_singular` pins today —
*"I eats the bread"* and *"they is a planet"* — are that test doing its job: it
was written so an irregular fix "arrives as a red test rather than a silent
correction". **This campaign fixes the first and leaves the second exactly as
it is**, and the reason is worth stating because it looks like an oversight.

Person agreement is keyed on FEATURES, and third-person singular's feature
bundle takes `is`. *"They is a planet"* is therefore untouched by adding a
person axis: its awkwardness comes from the pronoun INVENTORY — Common spells
third-person singular `they`, because nothing in the ledger assigns gender or
animacy (spec §4.5) — not from agreement. Real English gives singular *they*
plural agreement, which would make the copula depend on the subject's chosen
form rather than on its features, and that is a different mechanism from the
one this campaign builds. Deferred with its reason, not overlooked.

An earlier draft of this section asserted the surface became *"they are a
planet"*. That was wrong in both directions — it credited this campaign with a
fix it does not make, and it described a mechanism the design does not have.

### 3.5 A polar question is not a fact, so force is an operator

`Clause`'s central claim is that an utterance **is** a fact — subject,
predicate, object plus the speaker's features. A polar question is precisely
the utterance that is not: it asserts nothing. Putting `force: Force` on
`Clause` would make the fact-shape claim false for every clause in order to
serve one, and would cost those 105 construction sites besides.

So interrogative force is an **operator over** a clause, the way `Coordination`
is a list of them. Decision 0266 is what makes this more than a preference —
*an utterance is a fact* is the claim `Clause`'s shape encodes — and decision
0327 settled that embedding and coordination are two operators, a slot and a
list. A question is a third: a wrapper. Common gets
`realize_common_polar_question`; no new `Clause` field, no churn.

It stays a free function rather than a `PolarQuestion(Clause)` newtype for as
long as no caller needs to *hold* a question. `Coordination` earned its type by
holding a `Vec`; a single-field wrapper earns nothing today, and the type
arrives with the first caller that stores one.

**One honesty note that must not pass quietly.** The merchant entry this
unblocks, `m08` (*"Did you know the woman?"*), is a **player** line — something
the grammar must parse, not produce. The merchant corpus states no `direction`
at all (The Stile: all 12 entries are direction-unknown, and inferring
direction from `speaker` is forbidden), so the resolver will score `m08`
covered on a production capability. That is the scoring method behaving as
specified, and it is also a real limit on what "6 of 12" means. It goes in the
report, not only in this spec.

### 3.6 The ladder gets the realization witness the merchant corpus has

`every_covered_entry_realizes_in_common` walks `MERCHANT_WITNESS` and nothing
else. The ladder has no witness and no published score, so adding five tokens
would move it from 1 to 9 covered with nothing mechanical behind any of them —
the exact failure `sentence_corpus.rs`'s own module doc names when it says an
unbacked `IMPLEMENTED_DEMANDS` would be "worse than no instrument, because it
would read as evidence".

Every rung this campaign covers gets a hand-built construction and a recorded
Common surface, on `MERCHANT_WITNESS`'s pattern: not equality against the
rung's authored English (Common is a limited register and could never pass
that), but proof that a constructible clause realizes at all. A rung whose
witness cannot be built is a rung this campaign did not build, and the token
does not go in.

### 3.7 Which rungs get a tongue realizer, and why two do not

All five land in **Common**. Three get a tongue realizer this campaign:

- **`r002` intransitive-frame.** `ConstituentOrder` has six values built around
  three slots (S, V/copula, O). An intransitive clause has two, so the six
  orders project onto SV/VS. That projection is the tongue's half and it is
  mechanical rather than typological — which is why it is cheap, not why it is
  skippable: the lever rung shipping Common-only would leave every tongue
  unable to say the sentence seven of eight existing tokens stand on.
- **`r011` person-deixis.** Agreement depth is a drawn typological axis and the
  tongue is the entire point of having one. `ParadigmDepths` gains
  `person_depth` and `person_position` beside number/tense/polarity.
- **`r083` polar-question.** This is where the prosody finding pays off (see
  §3.8). `TongueGrammar` gains an interrogative strategy: a drawn particle
  form, or `None` for a tongue that questions by intonation alone.

Two ship **Common-only**, with their tongue halves deferred and the reason
stated rather than left as silence — the same posture `classify` and
`epistemic-hedge` already take, and recorded in `IMPLEMENTED_DEMANDS`'s own
backing list:

- **`r003` property-predication** — the tongue half is Stassen's
  adjectival-encoding typology (property words encoded as verbs, as nouns, or
  as a distinct adjective class), a whole drawn axis with its own weights.
- **`r005` locative-predication** — the tongue half is adposition order, which
  Dryer (WALS 85) makes one of the strongest correlations with verb-object
  order, so it is not an independent draw and needs its correlation designed
  rather than a coin flipped.

Each deferral is a followup register entry with its citation, not a shrug.

### 3.8 The interrogative that a text renderer can show

A tongue that draws no interrogative particle marks polar questions by
intonation, which is the commonest strategy cross-linguistically (Ultan 1978;
Dryer, WALS 116) and which a text renderer cannot show. `Phonology::orthography`
is a stated **view**, and punctuation is how writing encodes prosody, so such a
tongue renders its declarative surface plus `?` — a **transcription
convention**, explicitly not a grammatical marker, and named as such where it
is implemented.

This implements `LANG-prosody-needs-a-stated-transcription-convention`
(registered on this branch at `07892eadc`): transcription can express any
contrast the model **draws** and cannot manufacture one it does not.
Interrogative is in the first class. It does **not** close the question of real
alphabets and writing systems, which is stated future work of its own.

### 3.9 New stream labels are additive by construction

Two new drawn axes mean new seed-derivation labels: an interrogative axis under
`grammar`, and person depth/position under the paradigm's own legs. Every axis
in this crate derives its own stream by label path, so adding legs perturbs no
existing consumption order and moves no already-generated world's bytes — the
argument `paradigm_depths` already makes in its own doc for the polarity legs
The Inquest added. Pin-isolation tests are the proof, not the argument.

---

## 4. Non-goals

- **The ladder is not edited.** It is frozen at 214; rung ids are append-only.
  Nothing here adds, removes or renumbers a rung, and no rung's `text`,
  `introduces`, `presupposes` or `note` moves.
- **`the-merchant` still reads 5 of 12 by its existing method**, and 6 of 12
  only because `polar-question` gains a construction. Its existing assertions
  are the regression test for every resolver change here.
- **The flood-watch corpus gets no coverage resolver.** Its `shape_notes` field
  carries a claim 0386/0387 superseded; it is frozen and stays as authored
  (`sentences/README.md`, "Frozen is not the same as measured").
- **No `wh-question`, `temporal-adverbial`, `existential`, `witness-set` or
  `named-entity-list`.** Those are the merchant corpus's cheapest remaining
  wins — six tokens would cover all 12 — and they are a later campaign's, named
  here so the trade is visible rather than implied.
- **No second third-person singular pronoun**, no gender, no animacy (§3.4).
- **No nominalization, no gerund subject** — still out of scope, unchanged from
  The Mortise §9.1.
- **No parse-side recognition of the interrogative** beyond what the existing
  frozen parsing coverage already gives. §3.5's honesty note stands in its
  place.

---

## 5. Success criteria (preregistered)

1. The ladder reads **11 of 214 covered** with all five tokens implemented,
   and the frontier reads **20** — both computed by the resolver, not asserted
   from this document.

   **Corrected from 9/22 after Task 2, and the reason matters more than the
   numbers.** The 9/22 came from a controller script that excluded every
   control rung; the resolver includes two of them (`r015`, `r190`). The
   preregistration was not falsified by the world — it was arithmetic that had
   never been run against the instrument it was predicting. Recorded here
   because a criterion silently edited to match a result is worthless, and
   this one was edited to match the *resolver*, before four of the five tokens
   existed.
2. Each of the five rungs has a **realization witness**: a hand-built
   construction and a recorded Common surface, on `MERCHANT_WITNESS`'s pattern.
   A rung without one does not get its token.
3. **`the-merchant` reads 6 of 12**, covered set `m05 m06 m07 m08 m09 m10`, by
   its existing unmodified scoring method.
4. The complementary demand-instance statistic reads **21 of 30 (70.0%)** for
   `the-merchant` and **270 of 1128 (23.9%)** for `the-flood-watch`. These are
   arithmetic over frozen corpora and a token list, so they are exact
   predictions and a miss means a token did not land or the statistic is
   mis-implemented.
5. `docs/audits/sentence-coverage.md` publishes the ladder score and the
   frontier, and regenerates through `make rebaseline` with a drift check that
   can actually fail — demonstrated by a mutation, not asserted.
6. The copula and verb paradigms are **total** over their widened key, and the
   backward read's loss is pinned by a test that names which distinctions are
   unrecoverable.
7. *"The road is long"* realizes with no determiner, and `Definiteness` gains
   no `Bare` variant.
8. Pin-isolation holds: the two new drawn axes derive their own streams by
   label path and consume nothing from an existing one, proven by the
   pin-isolation tests rather than by this sentence.

   **What moves and what does not, as a branch rather than a prediction.** A
   stream's *consumption order* must not move, and the pin-isolation tests
   are what say so. The *stream manifest* WILL move, because `stream_labels()`
   gains legs. `world-seed-42.json` moves only if a concept is registered,
   and as of Task 0 none is.
9. `Valence` has five variants and no more, and its doc names Stassen's
   taxonomy as the reason the list is closed.

**A falsified prediction is a finding.** If criterion 1 or 4 cannot hold, the
campaign reports the number it got and why, rather than adjusting the
prediction after unblinding.

---

## 6. Decisions expected

From 0416–0425:

- `Valence` is a closed predication-strategy taxonomy, not an open frame list
  (§3.2)
- interrogative force is an operator over a clause, never a field on it (§3.5)
- an intransitive clause's object is `Argument::Absent`, and the kernel's
  `Value::Flag(true)` is why the fact-shape survives (§3.3)
- Common's paradigms admit syncretism, and the backward read's loss is pinned
  rather than designed away (§3.4)
- a tongue that questions by intonation renders a transcription convention, and
  a transcription convention is not a grammatical marker (§3.8)

---

## 7. Traps this campaign will walk into

- **No concept is registered, and that was checked rather than assumed.**
  Every concept the five rungs need is already in `packs::universal_stratum()`
  — `sleep` (`Act`), `old`/`high`/`low`/`great`/`little`/`new` (`Quality`), and
  `under`/`over` (`Quality`, docs "beneath; below" / "above; atop"). So
  `world-seed-42.json` does not move for concept registration, and the
  drafting session's assumption that it would was wrong.

  **The trap is narrowed, not gone.** Registering a concept DOES move that
  golden, `make rebaseline` does not write it, and `gate-commit` does not
  test it — only the merge queue catches it. If any task finds it needs a
  concept that is not already registered, that is the moment this fires.
- **`make rebaseline` cannot see byte-goldens.** `make rebaseline-goldens` is
  the separate accept path. Run both and diff the whole tree.
- **A declared generated path is not a live drift check.** `sentence-coverage.md`
  sat in `generated-paths.txt` with no writer until The Stile. Any new artifact
  here must be shown to have one.
- **Renaming a test is a commit-gate change.** `subfloor-roster.tsv` selects by
  exact name; §3.4 renames an invariant.
- **Absorb main at every plan-stage boundary**, via `make sluice-stage`. The two
  campaigns that landed on 2026-08-27/28 both skipped it and paid with 78- and
  188-commit absorptions.
