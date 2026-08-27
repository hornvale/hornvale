# The Inquest Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the clause tense, polarity, a transitive frame and pronouns, so the merchant corpus goes from 0 of 12 to 2 of 12 — and so the world stops saying a settlement *is* the home of a people that left six hundred years ago.

**Architecture:** Four stages, each independently green and independently valuable. Stage 1 wires machinery `paradigm.rs` already draws and adds one additive draw. Stage 2 adds a transitive construction, which is what gives a tense marker a verb to attach to. Stage 3 draws a pronoun inventory, which retires the gap The Scarf deliberately left. Stage 4 moves the score and writes the record.

**Tech Stack:** Rust 2024. `domains/language` throughout; `kernel/src/registry.rs` for the `kill` concept; `cli/tests/suite/sentence_corpus.rs` for the score.

**Spec:** `docs/superpowers/specs/2026-08-26-the-inquest-design.md` — read §3 (invariants) before every stage and §4.3 before Stage 2.

## Global Constraints

- **No new crates.** Allowlist is `serde`, `serde_json`, `libm`.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only.
- **Every crate sets `#![warn(missing_docs)]`**; every new `pub` item, field and variant needs a doc comment and a `type-audit:` tag if it exposes a primitive.
- **`cargo fmt` then `make gate-commit` before every commit.**
- **A new stream label is a THREE-part change**, and missing any part ships a silently wrong manifest:
  1. a `pub const` in `domains/language/src/streams.rs`;
  2. an entry in `stream_labels()` at `domains/language/src/lib.rs:998` — a hand-maintained `Vec<(path, description)>`;
  3. a manifest regeneration (`make rebaseline`), committed in the same commit.
- **New labels must be ADDITIVE.** Each draw derives its own stream by label path, so a new label perturbs nothing. **If a byte-golden under `cli/tests/fixtures/` or `windows/vessel/tests/fixtures/` moves, STOP** — that means a draw was inserted into an existing stream's consumption order, which is a save-format break, not a rebaseline.
- **One test run per question.** Capture to a file and grep it; do not re-run the suite to read a second line.
- **The controller verifies each task's brief against the code immediately before dispatching it.** Line numbers below are accurate as of `ab279cc78` and will drift as stages land; trust the dispatch's corrections over this file.

---

## Stage 1 — Tense and polarity

### Task 1: `Tense` and `Polarity` on the clause; Common surfaces both

**Files:**
- Modify: `domains/language/src/clause.rs` (the `Clause` struct at ~100; `Part`; `common_constructions`; `realize_common`; `parse_common_with_tail`; the inline `mod tests`)
- Modify: `windows/book/src/lib.rs`, `windows/almanac/tests/suite/interlinear.rs` (construction sites)

**Interfaces:**
- Produces: `pub enum Tense { Present, Past }`, `pub enum Polarity { Pos, Neg }`, `Clause.tense`, `Clause.polarity`. Stages 2 and 3 read both.

**WARNING — adding two fields breaks every full-literal construction site.** Size it first, and note that `grep -c 'Clause {'` counts the struct DEFINITION and any `-> Clause {` return as matches. Enumerate with `git grep -n 'Clause {'` and classify each line rather than quoting the count.

- [ ] **Step 1: Write the failing tests**

Three, in `clause.rs`'s `mod tests`. The first is the campaign's motivating defect:

```rust
#[test]
fn a_past_clause_says_was() {
    let vocab = CommonVocabulary::default();
    let base = Clause {
        predicate: IS_A.to_string(),
        subject: Subject::Name("Nwamvam".to_string()),
        object: Argument::Concept("home".to_string()),
        number: Number::Sg,
        definiteness: Definiteness::Def,
        evidential: Evidential::Witnessed,
        tense: Tense::Present,
        polarity: Polarity::Pos,
        adjuncts: Vec::new(),
    };
    assert!(realize_common(&base, &vocab).contains(" is "));
    let past = Clause { tense: Tense::Past, ..base.clone() };
    let out = realize_common(&past, &vocab);
    assert!(out.contains(" was "), "past tense must say was: {out}");
    assert!(!out.contains(" is "), "and must not also say is: {out}");
}

#[test]
fn a_negated_clause_says_is_not() {
    // Polarity is a property OF the clause (spec 3.3), unlike tense, so it
    // needs no deictic centre and is recoverable from the surface.
    let vocab = CommonVocabulary::default();
    let neg = Clause { polarity: Polarity::Neg, /* ..as above.. */ };
    let out = realize_common(&neg, &vocab);
    assert!(out.contains("is not"), "got: {out}");
}

#[test]
fn common_round_trips_tense_and_polarity() {
    // The Construction table realizes forward and parses backward. Both new
    // features are recoverable from the surface, unlike `evidential`.
    // Build a clause, realize it, parse it back, assert the two features
    // survive. Use the same ParseContext helper the neighbouring
    // round-trip tests build (read `ctx_from`).
}
```

The third test's body is deliberately unwritten: read `ctx_from` and the existing round-trip test first and follow their construction. Do not invent a `ParseContext`.

- [ ] **Step 2: Run them; confirm they fail to compile**

`cargo test -p hornvale-language --lib clause::tests > /tmp/inq-t1.log 2>&1; echo "exit=$?"` — expect `struct Clause has no field named tense`.

- [ ] **Step 3: Add the two enums and two fields**

```rust
/// When the clause's content stands relative to the utterance.
///
/// **Stated, never derived** (spec 3.3). Number, definiteness and evidential
/// are properties OF a clause; tense is a RELATION to a moment outside it —
/// the first feature requiring a deictic centre. A `Clause` has no access to
/// speech time and must not acquire one, so the caller, which knows both the
/// fact's `WorldTime` and the utterance's, supplies the relation already
/// computed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tense {
    /// Contemporaneous with the utterance.
    Present,
    /// Prior to the utterance.
    Past,
}

/// Whether the clause asserts or denies.
///
/// Unlike [`Tense`] this is an ordinary property of the clause, recoverable
/// from the surface, needing no deictic centre.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Polarity {
    /// The clause asserts.
    Pos,
    /// The clause denies.
    Neg,
}
```

- [ ] **Step 4: Teach the `CLASSIFY` construction**

`Part::Copula` currently reads `spec.number` alone. It must read tense and polarity too. **Do not add a `Part::Negator` variant unless the parse direction needs one** — read `parse_common_with_tail`'s walk over `parts` first and decide from what it requires, then say which you chose and why.

The copula table is `{Present,Past} × {Sg,Pl}` → `is/are/was/were`, with negation appended (`is not`, `were not`).

- [ ] **Step 5: Fix every construction site, then run**

`cargo check --workspace --all-targets` until clean. Choose each site's values by what is true of that clause — a self-statement about what a people IS is `Present`/`Pos`. Then the three tests plus the crate, captured once.

- [ ] **Step 6: Prove the tense test can fail**

Mutate the copula table so `Past` yields `is`, confirm RED, restore **from a `cp` backup** (never `git checkout --`, which would destroy this task's uncommitted work), then `touch` the file — a restore that does not advance the mtime leaves cargo running the mutated build, and the dangerous direction is a false GREEN.

- [ ] **Step 7: fmt, gate, commit**

### Task 2: The tongue reads its drawn tense depth

**Files:** `domains/language/src/grammar.rs`; `domains/language/src/paradigm.rs` (read only)

**Interfaces:** consumes `Clause.tense`; consumes `paradigm_depths(seed, species) -> ParadigmDepths` and `draw_paradigm_affix_proto(seed, family, axis, value, proto_ph)`.

**This is the first consumer `paradigm.rs` has ever had.** Its whole public surface currently has zero callers outside its own file.

- [ ] **Step 1: Write the tests that prove the drawn value is READ**

Two tongues differing only in `tense_depth` must render a past clause differently — `Affix` marks, `None` does not. That is the assertion that makes the wiring non-vacuous; an equality test alone would pass if the field were never read.

Follow `realize_tongue_marks_by_depth` (`grammar.rs`, ~1035 pre-Scarf numbering) for how to build a `TongueMorphology` with synthetic marker forms. Do not invent a fixture.

- [ ] **Step 2: run, confirm red. Step 3: wire it.**

Past marks the **verb**. In a nominal clause the verb is the copula; **under a zero copula it marks the predicate nominal**, exactly as the evidential already does — that path exists and is tested (`grammar.rs` has a zero-copula evidential test; find it and mirror its placement logic rather than inventing one).

**Guard the same way §4.3 of The Scarf's spec requires:** a non-lexical object has no segments and must not be affixed. Guard on the concept id, never on `Marked.segments`, because a `Compound` has `segments: None` too and its panic is deliberate.

- [ ] **Step 4: the shallow-identity guarantee must still hold.** `realize_tongue_deep` with `MorphDepth::None` on every axis must equal `realize_tongue`. If that moves, the wiring changed the unmarked path and is wrong.

- [ ] **Step 5-7: run captured once, fmt, gate, commit.**

### Task 3: Polarity as a new drawn axis

**Files:** `domains/language/src/streams.rs`, `lib.rs` (`stream_labels()` at ~998), `paradigm.rs`, `grammar.rs`

- [ ] **Step 1: the three-part label change.** `POLARITY` and `POLARITY_POSITION` consts; `polarity_depth`/`polarity_position` on `ParadigmDepths`; entries in `stream_labels()`. Preregistered weights alongside the existing `TENSE_DEPTH_WEIGHTS`, with a comment giving the typological reason for the skew — the existing weights all carry one.

- [ ] **Step 2: BEFORE anything else, prove additivity.** Run the byte-golden tests and record them green. Then after the draw lands, run them again.

```
  golden UNMOVED -> the new labels are additive, as spec 3.4 claims. Proceed.
  golden MOVED   -> STOP. A draw was inserted into an existing stream's
                    consumption order. This is a save-format break, not a
                    rebaseline. Find the insertion.
```

This is the single most important check in the campaign and it is cheap.

- [ ] **Step 3-6:** draw the negative marker via `draw_paradigm_affix_proto(seed, family, "polarity", "negative", ph)` — no new function is needed, the axis and value are already `&str` parameters. Realize per depth. Test that a tongue with `polarity_depth: None` renders a negated clause identically to a positive one, and one with `Affix` does not. fmt, gate, commit, regenerate the manifest in the same commit.

---

## Stage 2 — Transitivity and `kill`

### Task 4: A transitive construction in Common

The construction table's own doc says a future predicate "is added HERE, and is bidirectional by construction". This is the first exercise of that promise.

- [ ] Add a construction keyed on an `Act` predicate: `Subject Verb Object`. The verb surfaces from the predicate concept through `CommonVocabulary`, inflected by `Tense` (Task 1 already built the tense machinery; reuse it rather than adding a second path).
- [ ] English past-tense inflection is the naive regular rule (append `ed`), matching the precedent set by `surface_complement`'s naive plural. Every verb the corpus needs is regular; an irregular table is a separate concern from where inflection *lives*.
- [ ] The demonstration verb is **`eat`** — verified `ConceptKind::Act`, `ladder_rank: 0`, universal stratum, every tongue has a real word. **Not `kill`** (does not exist yet) and **not `know`** (registered, but every tongue gaps).
- [ ] Round-trip: `parse_common` must recover a transitive clause.

### Task 5: The tongue's verb slot

- [ ] `ConstituentOrder` is already `Sov/Svo/Vso/Vos/Ovs/Osv` and already orders `(subject, verb, object)` — today the verb is the copula. A transitive clause supplies a lexical verb in that slot. **Read the existing ordering match and extend it; do not write a second one.**
- [ ] Test that a transitive clause honours each of the six orders — the existing exhaustive-order test is the model.
- [ ] Tense now has a real verb host. Assert that: a transitive past clause in an `Affix` tongue marks the **verb**, not the object.

### Task 6: `kill` as a concept

- [ ] Register `kill` as `ConceptKind::Act`, documented as a causative of the existing core `die`.
- [ ] **It creates no obligation to implement combat**, and the reason is worth stating in the registration comment: `cli/src/concepts.rs:178`'s audit walks `Action::all()` and reports actions with **no concept**. It is blind to a concept with no action. Verify that is still true before relying on it.
- [ ] `book/src/reference/` will move (the registry dump). Expected; commit in the same commit.
- [ ] **Followup for Task 10:** `ConceptKind::Act`'s doc in `kernel/src/registry.rs` says "the GOAP action roster reconciles against exactly this class" without naming the direction. A check asserting *actions ⊆ concepts* reads as total. Add the direction.

---

## Stage 3 — Pronouns

### Task 7: Draw a pronoun inventory per tongue

- [ ] Six cells: `{1,2,3} × {Sg,Pl}`. **No gender** (spec §4.5) — `her` is an English distinction this world has no basis for, and inventing one to satisfy one corpus line would be authoring rather than deriving.
- [ ] Drawn through the same proto-plus-`evolve` machinery every family-cognate morpheme uses, under new stream labels. **The three-part label change applies, and so does the byte-golden additivity check from Task 3 Step 2.**
- [ ] Run the golden check before and after. Same branch table.

### Task 8: `Argument::Pronoun`, and the gap stops firing

- [ ] Add `Argument::Pronoun` — `Argument` is currently `Concept|Name|Count|Quantity`. Its own doc says a variant is added "when a role needs it, never speculatively"; a transitive clause with a pronoun object is that need. Cite it in the variant's doc.
- [ ] `tongue_subject` currently returns `TongueGap` for `Subject::Pronoun`, with a reason naming the missing inventory. **That gap must now stop firing** — the inventory exists.
- [ ] **This satisfies The Scarf's rule rather than reversing it.** Decision 0286's neighbour said a tongue gaps *because no tongue draws a pronoun inventory*; drawing one falsifies the antecedent. Say so in the commit message, and delete the gap arm rather than leaving it unreachable.
- [ ] The test The Scarf wrote (`a_tongue_gaps_on_a_pronoun_subject_and_names_the_missing_inventory`) now asserts the opposite of the truth. **Replace it, do not delete it** — the replacement asserts a pronoun subject realizes, and its doc comment records what it used to assert and why that changed.

---

## Stage 4 — The score and the record

### Task 9: Move the corpus score

- [ ] `IMPLEMENTED_DEMANDS` gains `past-tense`, `negation`, `transitive-frame`, `pronoun-reference`. `MERCHANT_COVERED` moves `0` → `2`, **in the same commit**, which is the discipline the file's own doc demands.
- [ ] The two covered entries are *"A guard killed a woman."* and *"I didn't know her."* Assert them by id, not by count alone — a count of 2 could be any two.
- [ ] **Add the distance report** (spec criterion 7): how many entries sit at exactly one missing demand, and which. A conjunctive score is a lagging indicator — four entries move in this campaign without the headline number moving, and an instrument that cannot show that is hiding its own progress.
- [ ] The positive control must still hold: the synthetic `classify`-only entry resolves as covered.

### Task 10: Definition of Done

- [ ] `docs/decisions/0296-*.md` — the ratifiable choice is **tense is stated, never derived**: it is the first clause feature requiring a deictic centre, and a clause must not acquire a clock. Include §4.2's corollary, that the tense host is the verb and that this is why transitivity shipped alongside.
- [ ] `book/src/chronicle/the-inquest.md` + `SUMMARY.md`. Lead with the contradiction it fixes. **State plainly that the two covered lines gap in every tongue for vocabulary reasons** — `kill` is new and unexposed, `know` is registered but no culture has exposure — and that the flagship is built on `eat` for exactly that reason. **Also state that this campaign's headline change is invisible in the published book**, because the book renders only present-tense statements.
- [ ] `docs/retrospectives/the-inquest.md`. Sweep `.superpowers/sdd/` FIRST — it is git-ignored and dies with the worktree. Carry at least: the stale-board-hold-off lesson (a peer notice never decays; check `git merge-base --is-ancestor` before relaying one), and whether the byte-golden additivity check ever fired.
- [ ] Registry rows: mood (the fifth speaker feature, still undrawn); aspect; **clause-in-clause connectivity**, which is the next ceiling and gates `embedded-clause` and `epistemic-hedge`; and the `ConceptKind::Act` doc-direction followup from Task 6.
- [ ] Confidence Gradient re-score if a bet moved; grep before concluding none did.
- [ ] `PATH=$HOME/.deno/bin:$PATH make rebaseline`, then resolve by the spec's §8 branch table. **`book/src/gallery/` should be UNMOVED** — the book renders no past-tense-eligible clause. A move there is a word-form shift and means the same fault as a moved golden.

---

## After Task 10

Do NOT merge. Return to the controlling session for **G6**. The merge goes through the sluice and needs a `Sluice-Headline` trailer in the same block as `Claude-Session`, no blank line between.
