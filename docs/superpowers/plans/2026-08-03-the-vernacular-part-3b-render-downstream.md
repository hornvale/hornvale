# The Vernacular, part 3b — text stops being stored

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Delete `Phenomenon.description`, give Common a declared vocabulary so
no concept id can reach prose, and invert the direction of authority so the
ledger's content stops being downstream of a rendering decision.

**Architecture:** Two realizers exist. `realize_tongue_deep` takes a **concept
id**, resolves it through a species' lexicon, and fails loudly with a
`TongueGap` when there is no word. `realize_common` takes a **word the caller
already chose** — so the author's register has no seam where "is this concept
sayable?" can even be asked. That asymmetry is why every leak this campaign
found sits upstream of the clause layer. 3b closes it from both ends: Common
gets a declared vocabulary with a detectable absence, and `Phenomenon`
stops carrying text at all.

**Tech Stack:** Rust edition 2024, `serde` only (decision 0004). No new
dependencies. `make gate` as the commit gate.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap` / `HashSet`** — `BTreeMap` / `BTreeSet` / `Vec` only.
- **No wall-clock time.**
- Every public item documented; every pub-boundary primitive carries a
  `type-audit:` verdict tag.
- **Zero `TODO` comments in the workspace. Do not add one.**
- Layering: `kernel/` → `domains/*` → `windows/*` → `cli/`. **A domain depends
  on the kernel and nothing else — never a sibling domain.** A window may
  depend on domains and on other windows; a window may not reach back to the
  composition root, so the root fills window-facing structs.
- `cargo fmt` last. Commit every drifted committed artifact in the same commit.
- **Three repo Bash guards:** the raw whole-workspace nextest invocation is
  blocked (use `make gate`); bare `git stash` / `git stash pop` are blocked; two
  test runs in one Bash call are blocked — capture once and grep.
- **`git worktree remove` silently resets the shell's cwd to the main
  checkout.** If you make a throwaway worktree, use `git -C <path>` afterwards
  and echo `pwd` in any command whose output you report.

## What moves and what must not

**Zero committed facts may move, in every task.** Every task here is
rendering. Task 2 changes *how* a concept is chosen without changing *which*
concept results. Baseline:
`.superpowers/sdd/2026-08-02-the-vernacular-part-3a-star-class/baseline-seed-42-post-contour.json`
(11434 facts, village `Godogododaga`). Compare **fact lists**, not whole files.

**Rendered artifacts WILL move, substantially, from Task 4 onward.** Expect
`book/src/gallery/almanac-seed-42*.md`, `book/src/gallery/the-book.md`, and the
possession transcripts to change. The diff is the deliverable, not a nuisance.

## Scope

Implements §2, §3 and §5's plan 3b of
`docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`, including
the authority inversion recorded there on 2026-08-03 and Nathan's 2026-08-03
decision to fix Common's vocabulary inside 3b rather than defer it.

**NOT in scope:** the colour path (3c — `daylight_words`, `twilight_words`,
`class_color` collapsing into `name_color`), `SkyReport`/`ClimateReport`,
branch C's frame abstraction, and the bidirectional lint (stage 4).

## What was verified before writing, so you need not re-check

- **Only two production readers** of `Phenomenon.description`:
  `windows/almanac/src/lib.rs:314` and `cli/src/repl.rs:326`. Everything else is
  test assertions (`domains/astronomy/tests/tier_refinement.rs:93`,
  `domains/astronomy/tests/genesis_properties.rs:421`,
  `domains/astronomy/src/provider.rs:175,1010,1090,1099,1100`,
  `cli/tests/prose_is_not_a_contract.rs:106`).
- **23 producer sites** across `domains/astronomy/src` and `domains/climate/src`.
- **`realize_common`'s production callers are six sites, all in
  `windows/book`** (`lib.rs:345, 382, 1063, 1120, 1282, 2261`), plus one in
  `domains/language/src/clause.rs:598`.
- **Most callers already pass a concept id**: `complement: kind.clone()`,
  `complement: truth_kind.clone()`. Common's "lexeme" for `planet` is the
  string `"planet"` — the identity map works by naming convention and breaks
  silently on hyphenated ids like `celestial-body`.
- **`ClauseSpec.complement`'s doc and type-audit tag contradict each other**
  (`/// The complement concept's Common lexeme.` vs
  `type-audit: bare-ok(identifier-text: complement)`). Both were written
  because both are accidentally true.
- **`species_label(kind)` is `format!("{kind}s")`** — pluralization, not a
  vocabulary lookup. It is a *morphology* leak (the caller doing what
  `ClauseSpec.number` exists for), addressed in Task 5, not Task 3.
- **`class_display(concept) -> Option<&'static str>`** IS a vocabulary lookup
  and folds into Task 3's seam.

## The rendered line, decided

Today's committed gallery line carries **both** signs of the leak:

```
- [0.70] *celestial-body* — a golden sun fixed at zenith
```

`a golden sun fixed at zenith` is stored prose. `*celestial-body*` is a raw
registry key in reader-facing prose — the second sign §3.1 names, shipping
unnoticed because everyone was looking at the prose beside it.

**Nathan's decision (2026-08-03): drop the kind.** The line becomes salience
plus the rendered referent, in the speaker's own words:

```
- [0.70] the sun
```

---

### Task 1: A speaker reaches the almanac — ✅ COMPLETE (`d7ec0d32`)

Delivered `AlmanacContext::speaker: Option<Speaker>` where

```rust
pub struct Speaker {
    pub species: String,
    pub lexicon: hornvale_language::Lexicon,
    pub grammar: hornvale_language::TongueGrammar,
    pub morph: hornvale_language::TongueMorphology,
    pub sky_animate: bool,
}
```

filled at `windows/worldgen/src/lib.rs:7331` from the flagship people, mirroring
`windows/book/src/lib.rs:414-432`. 42 tests pass; gate green (447 s); zero moved
facts; zero gallery drift. The struct originally carried only
`{species, lexicon}`; it was widened mid-task because `realize_tongue_deep`
needs all five.

---

### Task 2: Invert the direction of authority

**Files:**
- Modify: `domains/astronomy/src/star.rs` (add the mass → concept derivation)
- Modify: `domains/astronomy/src/neighborhood.rs` (add the variant → concept map)
- Modify: `domains/astronomy/src/facts.rs:274,508` (commit the derived concept)
- Test: both domain files' inline `mod tests`

**Interfaces:**
- Consumes: `SPECTRAL_CLASSES`, `class_display` from part 3a.
- Produces: `pub fn class_concept_of_mass(mass: f64) -> &'static str` in
  `star.rs`, and `pub fn class_concept(class: NeighborClass) -> &'static str` in
  `neighborhood.rs`. Both **total** — no `Option`, no `.expect()` at the call
  site. Task 3 folds `class_display` into the Common vocabulary and needs these
  to exist first.

**Why this task exists.** Part 3a left display prose as the authority:
`facts.rs` string-matches `class_concept(&system.star.class_name)` to decide what
to commit, guarded by `.expect()`. The ledger's content is downstream of a
rendering decision — the inverse of this campaign's thesis. **Nathan's call,
2026-08-03: invert it.**

It also dissolves part 3a's Critical structurally. That finding existed because
`SPECTRAL_CLASSES` was a lookup the ledger depended on, hand-kept in sync with
the registration list and guarded only by a test. Once the id is derived
directly, that table becomes purely a render table.

- [ ] **Step 1: Write the failing tests**

In `domains/astronomy/src/star.rs`'s `mod tests`:

```rust
/// The concept is derived from mass, not parsed from prose. Same boundaries as
/// `class_name_of_mass`, and the two must agree — the display is now derived
/// from the concept's side of the same physics, not the other way round.
#[test]
fn the_concept_and_the_display_are_derived_from_the_same_mass() {
    for mass in [0.6, 0.79, 0.8, 1.04, 1.05, 1.4] {
        let concept = class_concept_of_mass(mass);
        let display = class_name_of_mass(mass);
        assert_eq!(
            class_display(concept),
            Some(display),
            "mass {mass} derives concept {concept:?} and display {display:?}, which disagree"
        );
    }
}
```

In `domains/astronomy/src/neighborhood.rs`'s `mod tests`:

```rust
/// Every variant maps to a concept totally — no lookup, no Option, so no
/// call site needs an `.expect()`.
#[test]
fn every_variant_derives_a_concept_agreeing_with_its_display() {
    for class in [
        NeighborClass::RedDwarf,
        NeighborClass::SunLike,
        NeighborClass::WhiteDwarf,
        NeighborClass::OrangeGiant,
        NeighborClass::RedGiant,
        NeighborClass::BlueGiant,
    ] {
        assert_eq!(
            crate::star::class_display(class_concept(class)),
            Some(class_name(class)),
            "{} derives a concept whose display disagrees with class_name",
            class_name(class)
        );
    }
}
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cargo test -p hornvale-astronomy --lib derived` — capture and grep.
Expected: FAIL — `class_concept_of_mass` / `class_concept` do not exist.

- [ ] **Step 3: Write the derivations**

`star.rs` — same boundaries as `class_name_of_mass`, returning the id:

```rust
/// The registered concept for a star of this mass — the ledger's own value,
/// derived from the physics rather than parsed back out of a display string.
/// Boundaries are `class_name_of_mass`'s; the two are two views of one
/// decision, and `the_concept_and_the_display_are_derived_from_the_same_mass`
/// pins them together.
/// type-audit: bare-ok(ratio: mass), bare-ok(identifier-text: return)
pub fn class_concept_of_mass(mass: f64) -> &'static str {
    if mass < 0.8 {
        "orange-dwarf"
    } else if mass < 1.05 {
        "yellow-dwarf"
    } else {
        "yellow-white-dwarf"
    }
}
```

`neighborhood.rs` — a total match, mirroring `class_name`:

```rust
/// The registered concept for a neighbour of this class. Total by
/// construction: a new variant fails to compile here, so no call site needs a
/// fallible lookup.
/// type-audit: bare-ok(identifier-text: return)
pub fn class_concept(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red-dwarf",
        NeighborClass::SunLike => "sun-like-star",
        NeighborClass::WhiteDwarf => "white-dwarf",
        NeighborClass::OrangeGiant => "orange-giant",
        NeighborClass::RedGiant => "red-giant",
        NeighborClass::BlueGiant => "blue-giant",
    }
}
```

- [ ] **Step 4: Flip the commit sites and delete the `.expect()`s**

`facts.rs:274` becomes
`Value::Text(crate::star::class_concept_of_mass(system.star.mass.0).to_string())`,
and `:508` becomes
`Value::Text(crate::neighborhood::class_concept(neighbor.class).to_string())`.
**Both `.expect()`s go.**

- [ ] **Step 5: Run the tests and the gate**

`cargo test -p hornvale-astronomy --lib` then
`make gate 2>&1 | tee /tmp/hv-3b-t2.log`.
Expected: PASS.

- [ ] **Step 6: Prove the values are identical**

**Facts must not move.** The ledger must commit exactly the same nine ids it
committed before — this task changes *how* the concept is chosen, never *which*.
Compare fact lists against the baseline: **zero differences.** If any moved, a
boundary or a variant mapping disagrees with part 3a's table, and that is a
finding.

Then confirm the panic is gone: `grep -n "expect" domains/astronomy/src/facts.rs`
should no longer show the two class lookups.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "refactor(astronomy): the id is primary, the display derived

Part 3a left display prose as the authority: facts.rs string-matched
class_concept(&star.class_name) to decide what to commit, guarded by .expect().
The ledger's content was downstream of a rendering decision — the inverse of
this campaign's thesis.

The producer now derives the concept from the physics it already holds: mass for
the star, the NeighborClass variant for a neighbour. Both derivations are TOTAL,
so both .expect()s are gone.

This also dissolves part 3a's Critical structurally rather than guarding it:
SPECTRAL_CLASSES stops being a lookup the ledger depends on and becomes purely a
render table.

Same nine ids committed. Zero facts moved."
```

---

### Task 3: Common gets a declared vocabulary

**Files:**
- Create: `domains/language/src/common_vocab.rs`
- Modify: `domains/language/src/lib.rs` (declare and re-export)
- Modify: `domains/astronomy/src/star.rs` (expose its Common words)
- Test: `domains/language/src/common_vocab.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: Task 2's derivations (so `class_display` can retire as authority).
- Produces:
  - `pub struct CommonVocabulary` with
    `pub fn new() -> Self`,
    `pub fn declare(&mut self, concept: &str, word: &str)`, and
    `pub fn word_for<'a>(&'a self, concept: &'a str) -> Option<&'a str>`.
  - `pub fn common_words() -> &'static [(&'static str, &'static str)]` in
    `domains/astronomy/src/star.rs`, returning the spectral classes'
    id → display pairs.

**The rule that makes absence detectable.** A concept id containing `'-'` is a
*key*, not a word: identity must not apply to it. So `word_for` is:

1. a declared entry, if one exists — the exception table wins;
2. otherwise the id itself, **only if it contains no `'-'`** (`planet`, `moon`,
   `people` — the naming convention already makes these words);
3. otherwise `None`.

`None` is the detectable condition the current design lacks. `celestial-body`
returns `None` instead of silently printing itself.

**Layering matters here.** `domains/language` may not depend on
`domains/astronomy`, so the vocabulary is a **mechanism** that holds no
domain's data. Each domain exposes its own pairs; the composition root
assembles them (Task 4). Do not hardcode astronomy's or settlement's concept
ids inside `domains/language`.

- [ ] **Step 1: Write the failing tests**

Create `domains/language/src/common_vocab.rs`:

```rust
//! Common's vocabulary. Common is the author's register, not a people's
//! tongue: it has no speakers, so it has no `Lexicon`. What it has instead is
//! an id→word map that is *mostly the identity function* — the concept naming
//! convention already yields English words — plus a declared exception for
//! every concept whose id is not a word.
//!
//! The point of declaring it at all is that absence becomes **detectable**. A
//! hyphenated id like `celestial-body` is a key, not a word; before this
//! existed it rendered as itself and shipped into the gallery unnoticed.

#[cfg(test)]
mod tests {
    use super::*;

    /// A single-word id IS its own Common word — the naming convention does
    /// the work, and declaring hundreds of identities would be noise.
    #[test]
    fn a_single_word_id_is_its_own_word() {
        let v = CommonVocabulary::new();
        assert_eq!(v.word_for("planet"), Some("planet"));
        assert_eq!(v.word_for("moon"), Some("moon"));
    }

    /// A hyphenated id is a KEY, not a word. Identity must not apply, or the
    /// key reaches prose — which is exactly the defect that shipped
    /// `*celestial-body*` into the committed gallery.
    #[test]
    fn a_hyphenated_id_has_no_word_by_default() {
        let v = CommonVocabulary::new();
        assert_eq!(
            v.word_for("celestial-body"),
            None,
            "a key must never render as itself"
        );
    }

    /// A declared exception supplies the word the id cannot.
    #[test]
    fn a_declared_word_wins() {
        let mut v = CommonVocabulary::new();
        v.declare("yellow-white-dwarf", "yellow-white dwarf (F)");
        assert_eq!(v.word_for("yellow-white-dwarf"), Some("yellow-white dwarf (F)"));
    }

    /// A declaration may also override an identity — a single-word id whose
    /// Common word differs from the id itself.
    #[test]
    fn a_declaration_overrides_an_identity() {
        let mut v = CommonVocabulary::new();
        v.declare("people", "folk");
        assert_eq!(v.word_for("people"), Some("folk"));
    }
}
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cargo test -p hornvale-language --lib common_vocab`
Expected: FAIL to compile — `CommonVocabulary` does not exist.

- [ ] **Step 3: Write the vocabulary**

```rust
use std::collections::BTreeMap;

/// Common's id→word map: identity for single-word ids, declared entries for
/// everything else, `None` where no word exists.
/// type-audit: bare-ok(identifier-text: keys), bare-ok(prose: values)
#[derive(Clone, Debug, Default)]
pub struct CommonVocabulary {
    declared: BTreeMap<String, String>,
}

impl CommonVocabulary {
    /// An empty vocabulary: pure identity-for-single-word-ids, no exceptions.
    pub fn new() -> Self {
        Self::default()
    }

    /// Declare the Common word for a concept whose id is not one (or whose
    /// word differs from its id). Re-declaring replaces.
    pub fn declare(&mut self, concept: &str, word: &str) {
        self.declared
            .insert(concept.to_string(), word.to_string());
    }

    /// The Common word for `concept`, or `None` when Common cannot say it.
    ///
    /// A caller that gets `None` must **describe** the concept or omit it —
    /// never print the id. Printing the id is the defect this type exists to
    /// make impossible to reach by accident.
    pub fn word_for<'a>(&'a self, concept: &'a str) -> Option<&'a str> {
        if let Some(w) = self.declared.get(concept) {
            return Some(w.as_str());
        }
        if concept.contains('-') {
            return None;
        }
        Some(concept)
    }
}
```

- [ ] **Step 4: Astronomy exposes its own words**

In `domains/astronomy/src/star.rs`, beside `SPECTRAL_CLASSES`:

```rust
/// This domain's Common words: the concepts whose ids are not words, paired
/// with the author's-frame display. The composition root declares these into
/// the `CommonVocabulary`; a domain may not reach into `domains/language`'s
/// map itself.
/// type-audit: bare-ok(identifier-text: ids), bare-ok(prose: displays)
pub fn common_words() -> &'static [(&'static str, &'static str)] {
    SPECTRAL_CLASSES
}
```

**Do not delete `class_display` in this task.** Task 4 migrates its callers;
deleting it here breaks `windows/explain:44` and `windows/book` mid-plan. Add a
doc line saying it is superseded by the vocabulary and slated for removal in
Task 4 — **no `TODO` comment**.

- [ ] **Step 5: Run the tests and the gate**

`cargo test -p hornvale-language --lib common_vocab` then
`make gate 2>&1 | tee /tmp/hv-3b-t3.log`.
Expected: PASS. Nothing consumes the vocabulary yet, so **zero facts moved and
zero artifact drift** — except `docs/audits/type-audit-report.md`, which gains
the new pub boundary. Run `make rebaseline` and commit that drift.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(language): Common gets a declared vocabulary

Common is the author's register, not a people's tongue — no speakers, so no
Lexicon. What it lacked was any declared id->word map at all, so the identity
function served silently: 'planet' rendered as 'planet' by naming convention,
and 'celestial-body' rendered as itself straight into the committed gallery.

CommonVocabulary makes absence DETECTABLE. A hyphenated id is a key, not a
word: identity does not apply to it, and word_for returns None so a caller must
describe the concept instead of printing its key.

The mechanism holds no domain's data — domains/language may not depend on a
sibling. Astronomy exposes its own pairs via common_words(); the root
assembles them next task."
```

---

### Task 4: `realize_common` takes a concept, not a word

**Files:**
- Modify: `domains/language/src/clause.rs` (`ClauseSpec`, `realize_common`)
- Modify: `windows/book/src/lib.rs` (six call sites; `species_label`)
- Modify: `windows/explain/src/lib.rs:44` (drop `class_display`)
- Modify: `domains/astronomy/src/star.rs` (retire `class_display`)
- Modify: `windows/worldgen/src/lib.rs` (assemble the vocabulary)
- Test: `domains/language/src/clause.rs` inline tests; `cli/tests/`

**Interfaces:**
- Consumes: `CommonVocabulary`, `astronomy::star::common_words()` (Task 3);
  Task 2's derivations.
- Produces: `ClauseSpec.complement_concept: String` (renamed from
  `complement`), `realize_common(spec: &ClauseSpec, vocab: &CommonVocabulary)
  -> Result<String, CommonGap>`, and
  `pub struct CommonGap { pub concept: String }`.

**This is the task that closes the asymmetry.** After it, both realizers take a
concept id and both can fail when the register has no word — the tongue path
with `TongueGap`, the author's path with `CommonGap`.

- [ ] **Step 1: Write the failing test**

In `domains/language/src/clause.rs`'s `mod tests`:

```rust
/// Common resolves its complement through the vocabulary, exactly as the
/// tongue path resolves through a lexicon. Symmetry is the point: before this,
/// the caller chose the word and no layer could ask whether the concept was
/// sayable at all.
#[test]
fn common_resolves_its_complement_through_the_vocabulary() {
    let mut vocab = CommonVocabulary::new();
    vocab.declare("yellow-white-dwarf", "yellow-white dwarf (F)");
    let spec = ClauseSpec {
        frame: Frame::Classify,
        subject: Subject::Name("Elthandil".to_string()),
        complement_concept: "yellow-white-dwarf".to_string(),
        number: Number::Sg,
        definiteness: Definiteness::Indef,
        modifiers: vec![],
    };
    assert_eq!(
        realize_common(&spec, &vocab).unwrap(),
        "Elthandil is a yellow-white dwarf (F)."
    );
}

/// An undeclared hyphenated concept GAPS rather than printing its key. This is
/// the test that would have caught `*celestial-body*` shipping to the gallery.
#[test]
fn common_gaps_rather_than_printing_a_key() {
    let vocab = CommonVocabulary::new();
    let spec = ClauseSpec {
        frame: Frame::Classify,
        subject: Subject::Name("X".to_string()),
        complement_concept: "celestial-body".to_string(),
        number: Number::Sg,
        definiteness: Definiteness::Indef,
        modifiers: vec![],
    };
    let err = realize_common(&spec, &vocab)
        .expect_err("an unsayable concept must gap, not render its key");
    assert_eq!(err.concept, "celestial-body");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-language --lib common_` — capture and grep.
Expected: FAIL to compile.

- [ ] **Step 3: Change the field and the signature**

Rename `ClauseSpec.complement` → `complement_concept` and **fix the
contradictory tag**: the field is now genuinely
`type-audit: bare-ok(identifier-text: complement_concept)`, and the doc must say
"The complement **concept id**, resolved through the [`CommonVocabulary`]" — the
old doc said "Common lexeme", which is what made the tag and the doc disagree.

`realize_common` returns `Result<String, CommonGap>`. Resolve
`spec.complement_concept` through the vocabulary; on `None`, return the gap.
The existing article logic (`indefinite_article`) now runs on the **resolved
word**, which is correct — `an` for `elemental` still works, and it now also
works for a declared multi-word display.

- [ ] **Step 4: Fold `species_label` into the realizer**

`species_label(kind)` is `format!("{kind}s")` — the caller doing
pluralization that `ClauseSpec.number: Number` already exists to express.
Move it: `realize_common` pluralizes the resolved word when
`number == Number::Pl`, and the two call sites (`windows/book:385, 1285`) pass
the concept with `number: Number::Pl` instead of a pre-pluralized string.
Delete `species_label`.

**Keep the pluralization rule exactly `{word}s`** — do not add irregular-plural
handling. This task preserves behaviour; a better pluralizer is a separate
concern and the gallery must not move for that reason.

- [ ] **Step 5: Migrate the six book call sites and retire `class_display`**

Each of `windows/book/src/lib.rs:345, 382, 1063, 1120, 1282, 2261` passes a
concept id (most already do) and handles the `Result`. **How a `CommonGap`
surfaces is a judgment call**: the book already collects `tongue_gaps` for the
tongue path, so mirroring that with a `common_gaps` list is the consistent
move — but read the surrounding code and report what you chose. Do **not**
`.unwrap()` a gap into a panic in a render path that the gallery depends on
unless the surrounding code already establishes that a gap there is a violated
invariant.

`windows/explain/src/lib.rs:44` calls `class_display` to render the star class;
point it at the vocabulary. Then delete `class_display` and have
`SPECTRAL_CLASSES` serve only `common_words()`.

- [ ] **Step 6: Assemble the vocabulary at the composition root**

The root builds one `CommonVocabulary`, declares every domain's
`common_words()` into it, and hands it to the windows that render. Follow
`AlmanacContext::place_labels`' precedent — the root fills, the window
receives. **Build it once per world**, not per clause.

- [ ] **Step 7: Gate, then read the artifact diff**

`make gate 2>&1 | tee /tmp/hv-3b-t4.log`, then `make rebaseline`.

**Facts must not move.** Artifacts may. Quote the before/after of
`book/src/gallery/the-book.md`'s affected lines in your report. If the gallery
is byte-identical, say so explicitly — that is the *expected* outcome for the
already-identity-mapped concepts, and it is evidence the migration preserved
behaviour rather than evidence it did nothing.

- [ ] **Step 8: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(language): realize_common takes a concept, not a word

The two realizers were asymmetric in exactly this campaign's dimension:
realize_tongue_deep took a concept id, resolved it through a lexicon, and
gapped loudly when there was no word — while realize_common took a word the
caller had already chosen. So the author's register had no seam where 'is this
concept sayable?' could be asked, which is why every leak this campaign found
sat UPSTREAM of the clause layer.

ClauseSpec.complement becomes complement_concept, resolved through the
CommonVocabulary, and realize_common returns Result<String, CommonGap>. The
field's doc and its type-audit tag stop contradicting each other — it really is
identifier-text now.

class_display retires into the vocabulary. species_label retires into
Number::Pl, where pluralization belonged all along."
```

---

### Task 5: Render a phenomenon from its referent

**Files:**
- Create: `windows/almanac/src/phenomenon_line.rs`
- Modify: `windows/almanac/src/lib.rs` (declare the module; use it at `:314`)
- Test: `windows/almanac/src/phenomenon_line.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: `Speaker` (Task 1), `CommonVocabulary` and the `Result`-returning
  `realize_common` (Task 4), `hornvale_kernel::{Phenomenon, Referent}`.
- Produces: a renderer taking `(&Phenomenon, Option<&Speaker>,
  &CommonVocabulary) -> String` — the reader-facing text for one phenomenon,
  with **no** salience (the caller adds it) and **no** kind (dropped per
  Nathan's decision).

A new file rather than more of `lib.rs`: that file is already large, and this is
one clear responsibility.

**Two registers, structurally different.** Read `windows/book/src/lib.rs:434-450`
— the working precedent.

- **With a speaker: the tongue path.** Build a `TongueClause` whose
  `complement_concept` is `p.referent.concept` and call `realize_tongue_deep`,
  rebuilding the classifier as
  `|c: &str| hornvale_worldgen::noun_class_with_sky(speaker.sky_animate, c)`.
- **With no speaker: the neutral path.** `realize_common` with the vocabulary.

**On either gap, circumlocute — do not go silent and never print the key.**
§3.1 is explicit: an absent word means the thing gets *described*, not refused.
Prefer the neutral rendering of that one concept over dropping the line; a
reader should still learn the phenomenon is there.

**Qualifiers.** `Referent.qualifiers` are themselves registered concepts and
render the same way as the head. If `TongueClause` has no modifier slot, render
the head and **report what happened to the qualifiers** rather than silently
dropping them — a dropped qualifier is a content loss, which is what this
campaign exists to stop.

- [ ] **Step 1: Write the failing test**

```rust
//! One phenomenon, as a reader sees it: rendered from its referent at the
//! moment of reading, never stored. A producer cannot know who is looking
//! (`ObserverContext` carries no species), so a stored string could only ever
//! be neutral or wrong — which is why `Phenomenon` no longer has one.

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Phenomenon, Referent, Venue};
    use hornvale_language::CommonVocabulary;

    fn moon() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            referent: Referent::qualified("moon", &["great"]),
            period_days: Some(27.3),
            salience: 0.7,
            venue: Venue::NightSky,
            // Task 6 deletes this field; it is present only so this file
            // compiles before that task runs.
            description: String::new(),
        }
    }

    /// With no speaker, the line is the neutral Common realization — what an
    /// out-of-world reader gets when the world has no peoples.
    #[test]
    fn a_referent_renders_without_a_speaker() {
        let line = phenomenon_line(&moon(), None, &CommonVocabulary::new());
        assert!(line.contains("moon"), "must name the concept: {line}");
        assert!(
            !line.contains("celestial-body"),
            "a registry key must never reach prose: {line}"
        );
    }

    /// A qualifier reaches the rendering — `great` is a registered concept and
    /// the line must be able to say it.
    #[test]
    fn a_qualifier_reaches_the_line() {
        let v = CommonVocabulary::new();
        let bare = Phenomenon { referent: Referent::of("moon"), ..moon() };
        assert_ne!(
            phenomenon_line(&bare, None, &v),
            phenomenon_line(&moon(), None, &v),
            "a qualified referent must render differently from a bare one"
        );
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-almanac --lib phenomenon_line`
Expected: FAIL — `phenomenon_line` does not exist.

- [ ] **Step 3: Write the renderer**

Per the two-register contract above. Document why the kind is not rendered:

```rust
/// The kind is **not** rendered: it is a registry key, and a key in
/// reader-facing prose is the second sign of the leak this campaign closes
/// (spec §3.1). A reader gets nothing from `celestial-body` that `the moon`
/// does not already tell them.
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p hornvale-almanac --lib phenomenon_line`
Expected: PASS, 2 tests.

- [ ] **Step 5: Wire it in, and drop the kind**

At `windows/almanac/src/lib.rs:314`:

```rust
            doc.push_str(&format!(
                "- [{:.2}] {}\n",
                p.salience,
                phenomenon_line(p, ctx.speaker.as_ref(), &ctx.common_vocab)
            ));
```

(Thread the vocabulary onto `AlmanacContext` the same way Task 1 threaded the
speaker — the root fills it.)

- [ ] **Step 6: Gate, then accept the artifact movement**

`make gate 2>&1 | tee /tmp/hv-3b-t5.log`. The almanac's own render tests will
red on the changed line — read each message; update them to assert the new
shape, **do not delete them**.

Then `make rebaseline` and **read the gallery diff before committing**. Quote
the before/after of `book/src/gallery/almanac-seed-42.md`'s phenomena block.

**Facts must not move.**

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(almanac): a phenomenon is rendered from its referent

The line was '- [0.70] *celestial-body* - a golden sun fixed at zenith', which
carried both signs of the leak at once: stored prose, and a raw registry key in
reader-facing text. It is now salience plus the referent rendered through the
speaker's lexicon — or Common's vocabulary where there is no speaker — with the
kind dropped, machine grouping a reader gains nothing from.

Gallery artifacts move. Facts do not."
```

---

### Task 6: Delete `Phenomenon.description`

**Files:**
- Modify: `kernel/src/phenomena.rs` (the `Phenomenon` struct)
- Modify: all 23 producer sites in `domains/astronomy/src`, `domains/climate/src`
- Modify: `cli/src/repl.rs:326`
- Modify: the test assertions listed under "What was verified before writing"
- Modify: `cli/tests/prose_is_not_a_contract.rs`

**Interfaces:**
- Consumes: Task 5's renderer.
- Produces: `Phenomenon { kind, referent, period_days, salience, venue }`.

**This is the task the campaign has been walking toward.** Text stops existing
inside the sim.

- [ ] **Step 1: Write the failing test**

In `kernel/src/phenomena.rs`'s `mod tests`:

```rust
/// A phenomenon carries no text. A producer cannot know who is looking —
/// `ObserverContext` is {place, time, lens, position} by constitutional design
/// (decision 0003) — so a stored string could only ever be neutral or wrong.
/// Rendering happens where the speaker is known.
///
/// This test is a structural assertion: it fails to COMPILE if the field
/// returns, which is the point.
#[test]
fn a_phenomenon_carries_no_text() {
    let p = Phenomenon {
        kind: "celestial-body".to_string(),
        referent: Referent::of("moon"),
        period_days: None,
        salience: 1.0,
        venue: Venue::NightSky,
    };
    assert_eq!(p.referent.concept, "moon");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-kernel --lib a_phenomenon_carries_no_text`
Expected: FAIL to compile — `missing field 'description'`.

- [ ] **Step 3: Delete the field, then follow the compiler**

Remove `pub description: String` and its `bare-ok(prose: description)` tag.
Then let `cargo check --workspace --all-targets` drive you:

- **Producers** — delete the `description:` line. Nothing else.
- **Task 5's fixture** — remove the temporary `description: String::new()`.
- **`cli/src/repl.rs:326`** — the REPL has no speaker; render with `None` and
  the vocabulary. It needs Task 5's renderer, which is `pub(crate)` in
  `hornvale-almanac` — either promote it to `pub` with a doc and a
  `type-audit:` tag, or give the REPL its own neutral rendering.
  **Prefer promoting**: two renderings of one thing is what this campaign
  exists to remove. Say which you chose.
- **Test assertions** — the seven listed sites assert on prose. Convert each to
  assert on the **referent**, which is what they were always checking.

  **`provider.rs:1090` and `:1099` cannot be converted as-is** — every wanderer
  carries an identical `Referent::qualified("star", &["move"])`, so the referent
  cannot tell inner from outer, or morning from evening (the campaign's own
  recorded followup). Assert what the referent *can* support and say in a plain
  doc comment what the test no longer distinguishes. **No `TODO`.** Record the
  residue in the followup register.
- **`cli/tests/prose_is_not_a_contract.rs`** — its premise was that rewording a
  description moves nothing. With no description, the headline test is
  obsolete. **Do not delete the file**: keep `every_referent_key_is_registered`
  and `a_referent_never_carries_prose`, and replace the reword test with a note
  in the module doc that the defect it guarded is now structurally impossible.
  That is the strongest form of the guarantee and the file should say so.

- [ ] **Step 4: Run the gate**

`make gate 2>&1 | tee /tmp/hv-3b-t6.log`. Read every failure before changing
anything.

- [ ] **Step 5: Measure**

Facts: **zero moved.** Then `make rebaseline` — the REPL is not an artifact, so
expect movement only where Task 5 already moved things, plus
`docs/audits/type-audit-report.md` (the struct lost a tagged primitive).

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add -A
git commit -m "feat(kernel): a phenomenon carries no text

Phenomenon.description is deleted, not relocated. A producer cannot know who is
looking — ObserverContext is {place, time, lens, position} by constitutional
design — so a stored string could only ever be neutral or wrong. That is why
every leak in this campaign's audit clustered on this one field: its TYPE
guaranteed it.

Text now comes into being where the speaker is known. The reword-invariance
battery's headline test is retired because the defect it guarded is
structurally impossible: there is nothing left to reword."
```

---

### Task 7: The cost measurement, and the named speaker

**Files:**
- Modify: `windows/almanac/src/lib.rs` (the document header)
- Modify: `docs/superpowers/specs/2026-08-02-the-vernacular-part-3-design.md`

**Interfaces:** Consumes Tasks 1–6. Produces the readout 3c builds on.

- [ ] **Step 1: Name the speaker in the document**

§3 requires it: *"One world now has as many renderings as it has peoples, and
the committed artifact is a projection that picks one. A projection whose
choice is invisible reads as neutral fact."* Add a header line naming whose
account it is — e.g. `*As reckoned among the <name>.*`. With `speaker: None`,
say nothing rather than inventing a neutral claim.

- [ ] **Step 2: Measure the cost**

§4.2's frozen prediction: **per-world speaker construction keeps `make
rebaseline` within 1.25× its current wall time.** Take the most recent pre-3b
`rebaseline` row in `docs/timings.md` as the baseline; compare against a run
now. Report both numbers and the ratio.

**If it exceeds 1.25×**, the "build the speaker once per world" hoist is
mandatory rather than optional — verify Tasks 1 and 4 actually built the
speaker and the vocabulary once per world, and report what you find.

- [ ] **Step 3: Write the readout into §6**

Give: the cost ratio, whether facts moved (zero), which artifacts moved and how
the phenomena block reads now, the before/after of one gallery line, and
**what Common's vocabulary found** — how many registered concepts have no
Common word, since that number is this campaign's first measurement of its own
remaining leak surface.

**Leave §6's frozen predictions intact** — append alongside, never over.
Decision 0016's preregistration is worthless if the frozen numbers can be
edited after unblinding.

- [ ] **Step 4: Gate and commit**

```bash
make gate 2>&1 | tee /tmp/hv-3b-t7.log
cargo fmt
git add -A
git commit -m "docs(the-vernacular-3): 3b readout — the world says it in its own words"
```

---

## Self-review

**Spec coverage.** §1 (a stored description can never be per-culture) → Task 6's
test doc. §2 (delete, render downstream, speaker from the root) → Tasks 1, 5, 6.
§2's colour channel → **not here**, 3c. §2.1 (the one-way machine) → Task 6
removes the last stored text on the phenomenon path; Task 4 makes the *author's*
register one-way too. §3 (the almanac names its speaker) → Task 7 step 1. §4.1
(the flagship coupling) → made legible by Task 7 step 1's header. §4.2 (the cost
risk) → Task 7 step 2, against the frozen 1.25×. §5's 3b plus the 2026-08-03
authority inversion → Task 2. §6's prediction → Task 7 step 3. Nathan's
2026-08-03 Common-vocabulary decision → Tasks 3 and 4.

**Type consistency.** `Speaker`'s five fields are used as shipped in Task 1.
`CommonVocabulary::{new, declare, word_for}` is consistent across Tasks 3, 4, 5.
`ClauseSpec.complement_concept` and
`realize_common(&ClauseSpec, &CommonVocabulary) -> Result<String, CommonGap>`
are consistent in Tasks 4 and 5. `class_concept_of_mass(f64) -> &'static str`
and `class_concept(NeighborClass) -> &'static str` are total in Task 2 and used
with those signatures at both commit sites. `class_display` is *retired in Task
4*, not Task 3 — Task 3 explicitly forbids deleting it early.

**Sequencing hazards, stated so an implementer does not trip on them:**

1. **Task 5's fixture carries a temporary `description: String::new()`**, deleted
   in Task 6. Deliberate; a reviewer may read it as dead code.
2. **Task 3 must not delete `class_display`** — `windows/explain:44` and
   `windows/book` still call it until Task 4.
3. **Task 4 renames a field on a widely-used struct.** It is the largest task
   here and the one most likely to need a fix round.

**Three things stated as instructions to check rather than assertions**, because
this campaign's briefs have carried wrong claims into implementers' hands twice:

1. **How a `CommonGap` should surface in `windows/book` is a judgment call.**
   Task 4 names the consistent option (mirror `tongue_gaps`) and requires the
   implementer to read the surrounding code and report what they chose.
2. **Whether `TongueClause` has a modifier slot is unconfirmed.** Task 5 says to
   report what happened to the qualifiers rather than drop them silently.
3. **`provider.rs:1090`/`:1099` may be unconvertible** — every wanderer carries
   an identical referent. Task 6 says to assert what it can support and record
   the residue, not to fake it.
