# The Confidant — design

**Arc III of The Bridle.** The Hand made a possessed host co-present: it keeps
its own drives and affect while you ride it (decision 0226). Nothing can see
that. `Session::driven_mode()`'s only caller is a test — co-presence is real,
pinned, and invisible to a player. This campaign gives it a voice, and makes
the voice **unreliable in two specific, measurable ways**.

---

## 1. Scope

You ask the body you are wearing how it feels. It answers in its own words.
What it says is not what the arbitration computed, and the difference is the
deliverable.

**In:** a verb; affect surfaced out of `advance_one`; felt states as registered
concepts; the lexical gap (no word for it); the cognitive gap (blind to it);
and the instrument that makes both measurable.

**Out:** the host declining, lying, or telling costly truths — that is
`PLAY-host-may-refuse`, it needs a disposition-toward-rider model, and that
model should derive from The Cant's layers 2-3 rather than be authored a second
time here. Also out: the host volunteering unprompted, which needs a selection
policy, which is the speech budget, which puts the never-measured
`NARR-delivery` layer on the critical path.

---

## 2. Corrections to the brainstorm

### 2.1 A truthful host is a regression, not a simpler first version

An inversion pass overturned the obvious build. If the host reports its
arbitration accurately, testimony is `needs` with pronouns — the omniscient
oracle already shipped, in a first-person costume. `KNOW-needs-oracle` names
that oracle as the defect: `needs` "reports each co-located creature's felt
state through *the same arbitration that drives it*, so the player reads the
truth rather than inferring it."

So the gap is not a refinement to add once the plumbing works. **It is the
entire reason to prefer testimony over the oracle**, and a version without it
has shipped nothing. This governs task ordering: no task may land a truthful
host as an intermediate state and defer the gap to a successor.

### 2.2 This campaign costs no world-generation epoch, and an earlier reading said it did

During the brainstorm I predicted that registering felt-state concepts would
add per-concept, per-culture word draws, changing stream consumption order —
a save-format contract — and therefore cost an epoch.

**Verified, and it is false.** `domains/language/src/accession.rs` orders
concepts by **accession epoch first**, so a new cohort lands strictly last:
*"the one position that provably displaces nothing — so growth becomes additive
by construction."* Two committed tests hold it, and the second is a genuine
negative control:

```
a_later_epoch_concept_is_insertion_stable_from_any_alphabetical_position
the_same_mid_sorting_insertion_at_epoch_zero_still_displaces
```

The measured problem the module was built to fix is recorded there too: of
twelve species kinds added at once under name-ordering, ten were free, `treant`
moved 5 facts and `otyugh` 65.

**The procedure is absolute and this campaign must obey it literally:** append
a new `&[...]` to the end of `EPOCH_COHORTS`; **never edit an existing
cohort**. The module carries a withdrawn exception permitting cohort-0
re-founding, withdrawn after The Wearing exercised it — read that passage
before touching the file.

---

## 3. Design

### 3.1 One pipeline, two stages, two residues

```
  arbitration                     AffectLabel: 6 values, circumplex-grounded
       |
       |-- introspection filter --> what the creature can perceive of itself
       |                            residue = COGNITIVE gap
       |
       |-- lexicon lookup --------> what its culture has a word for
       |                            residue = LEXICAL gap
       |
       +-- utterance -------------> lands in player knowledge as `heard`
```

The player sees the utterance only. The world shows the truth by other means —
she will not look at the water. **The arbitration is never printed to the
player**; the gap is inferred or missed.

### 3.2 Most of this exists

- **`AffectLabel`** (`windows/vessel/src/liveness.rs`) is six values whose docs
  cite the circumplex: `Content`, `Eager`, `Searching`, `Frustrated`, `Lost`,
  `Helpless`. This is the true state and it does not change.
- **`GapReason`** (`domains/language/src/lexicon.rs`) already models *why* a
  culture lacks a word, in four kinds: `Experiential`, `Perceptual`,
  `Unnameable`, `Extradiegetic`. A `TongueGap` realizer and a CLI dictionary
  already recount gaps.
- **The lexicon is already per-culture**, assembled in two passes over that
  culture's concept exposures.
- **The listener end is already cut.** `windows/vessel/src/knowledge.rs`
  excludes `heard` entries from the ground-truth check *"by design, not an
  oversight; callers must not run heard knowledge through this check"* — so a
  false statement can already land in the player's knowledge.
- **The cognitive residue is already computed and discarded.** Arbitration
  ranks drives and keeps the winner; the discarded ranks *are* what the
  creature cannot introspect.

### 3.3 What is new

1. **Affect surfaced out of `advance_one`.** It returns `bool`; the `Intent`
   and `Affect` never escape. `grep` for `affect` on `Session` returns nothing.
   This is the same gap The Hand's final review found, and it is one signature.
2. **Felt states as concepts.** `ConceptKind` has `Substance`, `Living`,
   `Celestial`, `Terrain`, `Social`, `Body`, `Kin`, `Quality`, `Act` — no
   felt-state kind, and zero affect concepts are registered. For a culture to
   lack a word for `Helpless`, felt states must *be* concepts. New kind, new
   cohort, appended per §2.2.
3. **The introspection filter.** A creature perceives its dominant drive and
   not what arbitration suppressed.
4. **The verb**, and realisation of the answer in the host's own tongue.
5. **The instrument** (§3.4).

### 3.4 The instrument is the point, not a by-product

Nathan's stated goal is reproducing basic psychology, social-psychology and
sociological findings, for which **making conceptual deficiency visible** is
central. Three metrics, all as code (decision 0011):

- **`reportable_fraction`** — of the internal states a people's creatures can
  reach, what share has a term at all.
- **`collapse_ratio`** — how many distinct states map to the same nearest word.
  This is conflation, and it is the interesting one: two feelings a people
  cannot tell apart in speech.
- **`misreport_distance`** — circumplex distance between true state and
  reported state, per utterance.

Each is reported per people, so the fifteen modelled peoples are comparable.

---

## 4. Drift

**No world-generation epoch** (§2.2). Expected to move: `World.registry` gains
the new concepts, so every committed world fixture and the generated
concept-registry book page drift and must be rebaselined. Expected **not** to
move: existing words in any language, existing facts, and every census metric
not reading the new concepts.

**The decision rule, not a prediction.** If a rebaseline moves an existing
*word* or a fact unrelated to the new concepts, **STOP** — the cohort was
appended wrongly or an independent universe re-derivation exists (§5.2). If
only registry entries and the concept page move, regenerate and commit in the
same commit.

---

## 5. Risks

### 5.1 Circularity — the instrument cannot validate what it authors

You cannot reproduce a finding you built in. If the mechanism says "no word →
misreport" and the study measures "does absence of a word predict misreport",
the result recovers the constant we typed in. This project already names that
failure as auditing the generator, and The Cant was disciplined about it: it
shipped layer 1 plus *a measurement of its believability*, never a claim to
have validated the Stereotype Content Model whose warmth × competence
projection it uses.

**Therefore:** the instrument makes deficiency visible and measurable; findings
are sought **one step downstream**, in consequences the mechanism does not
specify — whether conflation predicts coordination failure between creatures,
how deficiency is distributed across fifteen peoples when nobody authored the
distribution. **No study in this campaign may preregister a hypothesis whose
truth follows from the authored mechanism.**

### 5.2 Appending a cohort is the operation that exposes universe-rule duplication

`proto_root_universe`'s own doc records a live instance: `windows/lab`'s
`monophyly-goblinoid` metric re-derived the family assignment independently and
built its universe from *every registered concept*, missing the exclusion
filter. It was invisible while the excluded cohort sorted last; **epoch 7 was
the first thing ever to sort after it**, and the metric then reported a
monophyly break on 14 of 1000 seeds where the world was monophyletic.

This campaign appends a cohort. **Before landing it, grep for every independent
re-derivation of the universe rule** and confirm each applies
`proto_root_universe`. An independent re-derivation may legitimately redo the
*draw*; it must not redo the *universe rule*.

### 5.3 The tongue may not be shared

`absorb_common` parses Common. A host whose tongue the player does not share
cannot testify fully, which is `PLAY-host-is-the-voice`'s "reasoned gaps". This
is a feature, but it interacts with the metrics: a `misreport_distance` that
silently folds in *untranslated* utterances measures two things at once.
Separate them or scope the campaign to hosts speaking Common, and say which.

### 5.4 Parallel campaigns

`windows/vessel/` and `domains/language/` are both busy areas. Post a board
`notice` with `polarity=hold-off` before Task 1, absorb main at every stage
boundary, and read the other branches' *chronicles*, not only their diffs.

---

## 6. Decisions to record

- **A host's testimony is fallible by construction, and the gap is the
  deliverable.** A truthful host is the omniscient oracle in first person.
- **Conceptual deficiency is modelled, visible and measured per people.** What
  a mind has no word for is a first-class, inspectable property of a culture.
- **Introspective access is bounded.** A creature reports its dominant drive
  and cannot perceive what arbitration suppressed.
- **Felt states are concepts.** They accede as a cohort, appended, never
  edited into an existing one.

---

## 7. Flagged for G3

1. **Registry growth, and the epoch question I got wrong.** No epoch is
   required (§2.2) — but this reverses what I told Nathan mid-brainstorm, and
   the correction rests on reading `accession.rs` plus two committed tests, not
   on a run. **The first task should reproduce the insertion-stability result
   empirically before the cohort is designed around it.**
2. **Circularity (§5.1)** — the constraint that no study may preregister a
   hypothesis the authored mechanism entails. This is the item most likely to
   be quietly violated later, because the circular study is the easy one to
   write.
3. **Committed fixtures drift** — registry growth moves every world fixture.
   Additive, but it touches keystone identity fixtures.
4. **Scope cut, for the record:** the social gap (`PLAY-host-may-refuse`) is
   deferred to a campaign riding The Cant's layers 2-3. That is a fidelity cut
   and Nathan made it.

---

## 8. Deliberately not in this arc

The host volunteering unprompted; the speech budget; any `NARR-delivery` work;
the disposition-toward-rider model; `PLAY-vacated-host-testifies`;
`PLAY-host-names-you`.

---

## 9. Definition of done

Chronicle, retrospective, freshness sweep, Confidence Gradient re-score if a
bet moved, registry flips (`PLAY-host-is-a-narrator`,
`PLAY-affect-becomes-testimony`, `PLAY-host-is-the-voice`, and F-H8's discharge
from The Hand's retrospective), decision records, and the drift check of §4.

**Also in scope, bundled because it lands in the same files** — The Hand's
deferred minors: the duplicated doc comment on `place_creature_at_me`
(`session.rs` ~1391-1413), `plan.rs:821`'s eaten continuation,
`synthetic.rs`'s inline copy of `PerceptionVector::MANIKIN`, the client test
name still saying `mints` after decision 0227, and F-H1's hardening of the
cross-seed mode pin to assert over 3+ seeds rather than `assert_ne!` on a
chosen pair.

---

## 10. Provenance

Brainstormed 2026-08-24 with Nathan under campaign autopilot. Ledger:
`.superpowers/sdd/decision-ledger.md`. Registry rows: `PLAY-host-is-a-narrator`,
`PLAY-affect-becomes-testimony`, `PLAY-host-is-the-voice`,
`PLAY-host-speech-budget`, `KNOW-needs-oracle`, `NARR-delivery`,
`NARR-delivery-study`. Predecessor: The Hand (decisions 0226-0230). Sibling
program: The Cant (`windows/sentiment`, evaluative beliefs, layer 1 of 3).
