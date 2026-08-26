# The Interlinear — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development to implement this plan task-by-task.
> Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the clause structure a real interlingua — structural role
bindings instead of pre-rendered English — and make Common one realizer among
the tongues, proven by rendering a real seed-42 occupation in Common and in a
generated tongue.

**Architecture:** `ClauseSpec.modifiers: Vec<String>` is replaced by
`adjuncts: Vec<Adjunct>`, where an adjunct binds a **registered predicate** to
an **argument**. How a role surfaces is per-language data: Common gets a role
construction table (ported out of `windows/book`), and a tongue gets its own.
Callers stop composing English and start stating meaning.

**Tech Stack:** Rust 2024, `domains/language` (`clause.rs`, `common_vocab.rs`,
`grammar.rs`), `windows/book`, `windows/almanac`. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-08-24-the-interlinear-design.md` — read
it alongside this plan; the plan argues from it.

**Decision block:** 0266-0275 (reserved 2026-08-24; main ceiling was 0247).

## A planning-time narrowing, proposed and then retracted

**I proposed dropping the spec's `predicate` field on YAGNI grounds and was
wrong.** Recorded rather than quietly reversed, because the reasoning is the
useful part.

The argument was: with `Frame::Classify` the only frame, a `predicate` field
would have no reader. Two facts kill it.

**`"is-a"` and `"instance-of"` already exist as fact predicates** — in
`domains/language/src/account.rs`, in the crate this campaign changes. So
`Frame::Classify` is a hardcoded enum standing in for a relation the same crate
already handles as a string.

**`account.rs` is already fact-shaped.** It is the epistemic account (C4,
LANG-36): the four-filter stack a culture's knowledge passes through — lexicon,
knowledge, ontology, valence — operating on **facts**, treating `is-a` as one
predicate among many, and yielding dispositions like
`Substituted { truth: "planet", theirs: "earth" }` when a culture carves the
world differently. That is the epistemic machinery the program's later
campaigns need, and it already speaks facts.

So `predicate` is not speculative: it makes `clause.rs` agree with `account.rs`.
Dropping it would have shipped this campaign's plumbing while deferring its
thesis — the spec's §2 claim is *an utterance is a fact*, and a struct with a
subject and a complement but no predicate is not a fact, it is a frame with one
relation baked into an enum.

**Therefore `ClauseSpec` becomes fact-shaped, and `Frame` is deleted:**

```
Fact        subject   predicate   object   place/day   provenance
ClauseSpec  subject   predicate   object   adjuncts    speaker features
```

Constructions are keyed by **predicate id**, not by a `Frame` variant.
`Frame::Classify` becomes the predicate `"is-a"`. The complement stops being a
special case and becomes the object — which is *less* machinery than the
version I proposed, not more, because it removes a distinguished slot instead
of adding a field.

The parse risk is unchanged: there is exactly one construction either way.

## Global Constraints

- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting
  uses `total_cmp`. Enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** No new dependencies (`serde`, `serde_json`, `libm`).
- **A domain depends on `hornvale-kernel` and nothing else** — never a sibling
  domain. `domains/language` may not reach into `domains/history`; see Task 8.
- **Every crate sets `#![warn(missing_docs)]`.** Renaming or adding a `pub`
  item moves `docs/audits/type-audit-report.md`; regenerate it in the same
  commit.
- **Rust edition 2024**; `cargo fmt` is the final step before every commit.
- **`git grep -E` has no `\b`.** Use `-P`.
- **Run the suite once, capture, then grep** — never re-run to read a second
  line.
- **Assert the branch before every commit:**
  `git branch --show-current | grep -qx 'campaign/the-interlinear' || exit 1`
- **Absorb `main` at every task boundary** (`make sluice-stage`).
  `campaign/the-confidant` holds a live hold-off on `domains/language`; read its
  **chronicle**, not just its diff, before absorbing.
- **Byte-identity is the headline.** `make rebaseline` and
  `make rebaseline-goldens` must move nothing except where a task says
  otherwise and says why.

---

### Task 1: Found the corpus, frozen before any measurement

Decision 0016: a study freezes its criteria *before* the code that would move
them. The corpus is the program's map and its score must be taken on today's
grammar, not on the grammar this campaign leaves behind.

**Files:**
- Create: `sentences/README.md`
- Create: `sentences/the-merchant.corpus.json`
- Test: `cli/tests/suite/sentence_corpus.rs` (+ declare in `cli/tests/suite.rs`)

**Interfaces:**
- Produces: the corpus JSON shape `{ "name", "provenance", "entries": [ { "id", "speaker", "text", "demands": [..] } ] }`, read by Task 9's resolver.

- [ ] **Step 1: Write the corpus data.** Record Nathan's merchant dialogue
      verbatim, one entry per line of dialogue, each annotated with what it
      demands. Use exactly these demand tokens so Task 9 can count them:
      `transitive-frame`, `past-tense`, `temporal-adverbial`, `negation`,
      `polar-question`, `wh-question`, `epistemic-hedge`, `embedded-clause`,
      `coordination`, `existential`, `pronoun-reference`, `named-entity-list`,
      `witness-set`, `classify` .

```json
{
  "name": "the-merchant",
  "provenance": "Nathan, 2026-08-24, in the brainstorm that opened The Interlinear. Recorded verbatim as an aspirational target; its coverage starts near zero on purpose.",
  "entries": [
    { "id": "m01", "speaker": "player",   "text": "How's it going?",
      "demands": ["wh-question"] },
    { "id": "m02", "speaker": "merchant", "text": "Everything was fine until last night.",
      "demands": ["past-tense", "temporal-adverbial"] },
    { "id": "m03", "speaker": "player",   "text": "What happened last night?",
      "demands": ["wh-question", "temporal-adverbial"] },
    { "id": "m04", "speaker": "merchant", "text": "Last night, there was a death in the marketplace.",
      "demands": ["existential", "past-tense", "temporal-adverbial"] },
    { "id": "m05", "speaker": "merchant", "text": "A guard killed a woman.",
      "demands": ["transitive-frame", "past-tense"] },
    { "id": "m06", "speaker": "merchant", "text": "I don't know why he killed her.",
      "demands": ["negation", "embedded-clause", "pronoun-reference", "epistemic-hedge"] },
    { "id": "m07", "speaker": "merchant", "text": "Seeing it confused and upset me.",
      "demands": ["coordination", "embedded-clause", "pronoun-reference"] },
    { "id": "m08", "speaker": "player",   "text": "Did you know the woman?",
      "demands": ["polar-question", "past-tense"] },
    { "id": "m09", "speaker": "merchant", "text": "I think her name was Gilda.",
      "demands": ["epistemic-hedge", "past-tense", "pronoun-reference"] },
    { "id": "m10", "speaker": "merchant", "text": "I didn't know her.",
      "demands": ["negation", "past-tense", "pronoun-reference"] },
    { "id": "m11", "speaker": "player",   "text": "Did anyone else see this?",
      "demands": ["polar-question", "witness-set"] },
    { "id": "m12", "speaker": "merchant", "text": "Stewie, Frank, Donkey Dick Doug, and 'The Big Bopper' were there.",
      "demands": ["named-entity-list", "witness-set", "past-tense"] }
  ]
}
```

- [ ] **Step 2: Write `sentences/README.md`.** State the three rules this
      corpus inherits, in prose: the corpus is **data** and the resolver is
      **code** (decision 0011); the corpus is **frozen before measurement** and
      its entry count is asserted, so changing it is a deliberate act (decision
      0016); and a verdict is **three-valued** — covered / not yet /
      declared-out-of-scope-with-a-reason, reasonless being a failure. Say
      plainly that a low score is the expected starting state and is not a
      defect.

- [ ] **Step 3: Write the failing freeze test.**

```rust
//! The sentence corpus is frozen: its entry count is asserted, so growing or
//! trimming it is a deliberate act rather than a drift.

use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The merchant corpus's frozen entry count. Changing this number is the
/// deliberate act; changing the corpus without it is the drift.
const MERCHANT_ENTRIES: usize = 12;

#[test]
fn the_merchant_corpus_is_frozen_at_its_authored_size() {
    let text = std::fs::read_to_string(
        repo_root().join("sentences/the-merchant.corpus.json"),
    )
    .expect("the merchant corpus is committed");
    let n = text.matches("\"id\":").count();
    assert_eq!(
        n, MERCHANT_ENTRIES,
        "the corpus moved. If that was deliberate, change MERCHANT_ENTRIES in \
         the same commit and say why in the message; a corpus that drifts \
         under a measurement makes every earlier score incomparable."
    );
}
```

- [ ] **Step 4: Run it and watch it fail for the right reason.**
      `cargo nextest run -p hornvale -E 'test(sentence_corpus)'`
      Expected before the JSON exists: FAIL on `the merchant corpus is committed`.
      After adding the JSON: PASS. **If it passes on the first run, the path is
      wrong** — check `repo_root()` resolved where you think.

- [ ] **Step 5: Declare the module** in `cli/tests/suite.rs`, alphabetically:

```rust
#[path = "suite/sentence_corpus.rs"]
mod sentence_corpus;
```

- [ ] **Step 6: `cargo fmt`, `make gate-commit`, commit.**

**Success:** the corpus exists, is frozen, and no measurement has been taken.

---

### Task 2: The interlingua's argument and adjunct types

**Files:**
- Modify: `domains/language/src/clause.rs`
- Test: in-module `#[cfg(test)]` in the same file

**Interfaces:**
- Produces: `pub enum Argument { Concept(String), Name(String), Count(u64), Quantity(f64) }` and `pub struct Adjunct { pub role: String, pub argument: Argument }`, both consumed by Tasks 3-8.

- [ ] **Step 1: Write the failing test.**

```rust
#[test]
fn an_adjunct_binds_a_registered_predicate_to_an_argument() {
    let a = Adjunct {
        role: "moon-count".to_string(),
        argument: Argument::Count(2),
    };
    assert_eq!(a.role, "moon-count");
    assert_eq!(a.argument, Argument::Count(2));
}
```

- [ ] **Step 2: Run it.** `cargo test -p hornvale-language adjunct_binds`
      Expected: FAIL, `cannot find type Adjunct`.

- [ ] **Step 3: Implement.** Place directly above `ClauseSpec`.

```rust
/// What an adjunct's role is bound to. Deliberately small: these are the
/// argument shapes the ledger's own `Value` already carries, minus the ones
/// no construction needs yet. A new variant is added when a role needs it,
/// never speculatively.
/// type-audit: bare-ok(identifier-text: Concept.0), bare-ok(prose: Name.0), bare-ok(count: Count.0), bare-ok(diagnostic-value: Quantity.0)
#[derive(Clone, Debug, PartialEq)]
pub enum Argument {
    /// A concept id, resolved through the realizing language's vocabulary.
    Concept(String),
    /// An already-resolved proper name, passed through unresolved.
    Name(String),
    /// A whole count, rendered as the language's cardinal.
    Count(u64),
    /// A continuous quantity, rendered at the language's grain.
    Quantity(f64),
}

/// One role binding on a clause: a **registered predicate** bound to an
/// argument. How it surfaces — a preposition, a case affix, a trailing
/// clause, or nothing at all — is the realizing language's business, not the
/// caller's. This is what replaced `modifiers: Vec<String>`, whose English
/// could not cross a language boundary.
/// type-audit: bare-ok(identifier-text: role)
#[derive(Clone, Debug, PartialEq)]
pub struct Adjunct {
    /// The role's predicate id, e.g. `"moon-count"`, `"occ-site"`.
    pub role: String,
    /// What the role is bound to.
    pub argument: Argument,
}
```

- [ ] **Step 4: Run it.** Expected: PASS.
- [ ] **Step 5: `cargo fmt`, `make gate-commit`, commit.**

---

### Task 3: Common's role construction table

Ports `windows/book`'s `fragment_for` into `domains/language`, structurally.
This is the task that ends the leak.

**Files:**
- Modify: `domains/language/src/clause.rs`
- Test: in-module

**Interfaces:**
- Consumes: `Adjunct`, `Argument` (Task 2).
- Produces: `pub enum AdjunctPosition { Inline, Trailing }`, and
  `pub fn common_role_surface(adjunct: &Adjunct, vocab: &CommonVocabulary) -> Option<(AdjunctPosition, String)>`.

- [ ] **Step 1: Write the failing tests.** These three assert **byte-identical
      output to what `windows/book` produces today** — that is the whole point,
      and Task 5 removes the old path only once these pass.

```rust
#[test]
fn common_renders_a_moon_count_exactly_as_the_book_did() {
    let v = CommonVocabulary::default();
    let one = Adjunct { role: "moon-count".into(), argument: Argument::Count(1) };
    let two = Adjunct { role: "moon-count".into(), argument: Argument::Count(2) };
    assert_eq!(
        common_role_surface(&one, &v),
        Some((AdjunctPosition::Inline, "with one moon".to_string()))
    );
    assert_eq!(
        common_role_surface(&two, &v),
        Some((AdjunctPosition::Inline, "with two moons".to_string()))
    );
}

#[test]
fn common_renders_a_star_class_through_the_vocabulary_with_its_article() {
    let mut v = CommonVocabulary::default();
    v.declare("yellow-white-dwarf", "yellow-white dwarf");
    let a = Adjunct {
        role: "star-class".into(),
        argument: Argument::Concept("yellow-white-dwarf".into()),
    };
    assert_eq!(
        common_role_surface(&a, &v),
        Some((AdjunctPosition::Inline, "orbiting a yellow-white dwarf".to_string()))
    );
}

#[test]
fn a_day_length_is_trailing_not_inline() {
    let v = CommonVocabulary::default();
    let a = Adjunct { role: "day-length-std".into(), argument: Argument::Quantity(1.5) };
    assert_eq!(
        common_role_surface(&a, &v),
        Some((AdjunctPosition::Trailing, "its day lasts 1.5 standard days".to_string()))
    );
}

#[test]
fn an_unknown_role_surfaces_as_nothing_rather_than_as_a_key() {
    let v = CommonVocabulary::default();
    let a = Adjunct { role: "not-a-role".into(), argument: Argument::Count(1) };
    assert_eq!(common_role_surface(&a, &v), None);
}
```

- [ ] **Step 2: Run them.** Expected: FAIL, `cannot find function common_role_surface`.

- [ ] **Step 3: Implement.** The role ids are the ledger predicate names
      `windows/book` already uses; import nothing — take them as `&str`
      literals here, because `domains/language` may not depend on a sibling
      domain, and these are the *language's* knowledge of how a role surfaces.

```rust
/// Where a realized adjunct attaches. A language decides this, not a caller:
/// Common puts a day-length in a trailing clause and a moon-count inline, and
/// another tongue may do the opposite.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdjunctPosition {
    /// Inside the clause, after the complement.
    Inline,
    /// After the clause, semicolon-joined.
    Trailing,
}

/// Common's role constructions: how each registered role surfaces in the
/// author's register. `None` means Common has no construction for this role
/// yet — the adjunct renders as nothing rather than leaking its key into
/// prose, which is the same discipline `CommonVocabulary::word_for` follows.
///
/// **These three moved here from `windows/book`'s `fragment_for`.** They lived
/// in a window because `ClauseSpec` could not carry structure, which is also
/// why that window had to duplicate `indefinite_article`. A role's surface is
/// a fact about a language and belongs to the language.
/// type-audit: bare-ok(prose: return)
pub fn common_role_surface(
    adjunct: &Adjunct,
    vocab: &CommonVocabulary,
) -> Option<(AdjunctPosition, String)> {
    match (adjunct.role.as_str(), &adjunct.argument) {
        ("moon-count", Argument::Count(n)) => Some((
            AdjunctPosition::Inline,
            format!(
                "with {} moon{}",
                cardinal(*n),
                if *n == 1 { "" } else { "s" }
            ),
        )),
        ("star-class", Argument::Concept(id)) => {
            let display = vocab.word_for(id);
            Some((
                AdjunctPosition::Inline,
                format!("orbiting {} {display}", indefinite_article(&display)),
            ))
        }
        ("day-length-std", Argument::Quantity(days)) => Some((
            AdjunctPosition::Trailing,
            format!("its day lasts {} standard days", quantity(*days)),
        )),
        _ => None,
    }
}
```

- [ ] **Step 4: Run them.** Expected: 4 PASS.
- [ ] **Step 5: Regenerate the type-audit report** — this added `pub` items:
      `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`
- [ ] **Step 6: `cargo fmt`, `make gate-commit`, commit** (report included).

---

> **Tasks 4 and 5 land in ONE commit.** Deleting `ClauseSpec.modifiers` breaks
> `windows/book`, which constructs `ClauseSpec` and touches `modifiers` in 18
> places — and `make gate-commit` runs `cargo clippy --workspace --all-targets`,
> so a Task 4 that commits alone commits a red tree. They are split here for
> readability, not for landing: a reviewer cannot meaningfully approve one
> without the other. Do both, then commit once.

### Task 4: `ClauseSpec` becomes fact-shaped, and Common realizes it

**Files:**
- Modify: `domains/language/src/clause.rs`
- Test: in-module

**Interfaces:**
- Consumes: `Adjunct`, `AdjunctPosition`, `common_role_surface`.
- Produces: `ClauseSpec { predicate: String, subject: Subject, object: Argument, adjuncts: Vec<Adjunct>, number, definiteness }`. `Frame`, `frame`, `complement_concept` and `modifiers` are **deleted**; `common_constructions()` is keyed by predicate id.

- [ ] **Step 1: Write the failing test.** It pins the *exact* surface the old
      `modifiers` path produced, including the `", "` join and the trailing
      `"; "` — this is the byte-identity contract.

```rust
#[test]
fn adjuncts_reproduce_the_modifier_tail_byte_for_byte() {
    let mut v = CommonVocabulary::default();
    v.declare("yellow-white-dwarf", "yellow-white dwarf");
    let spec = ClauseSpec {
        predicate: "is-a".into(),
        subject: Subject::Name("Hornvale".into()),
        object: Argument::Concept("planet".into()),
        number: Number::Sg,
        definiteness: Definiteness::Indef,
        adjuncts: vec![
            Adjunct { role: "moon-count".into(), argument: Argument::Count(2) },
            Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            },
            Adjunct { role: "day-length-std".into(), argument: Argument::Quantity(1.5) },
        ],
    };
    assert_eq!(
        realize_common(&spec, &v),
        "Hornvale is a planet with two moons, orbiting a yellow-white dwarf; \
         its day lasts 1.5 standard days."
    );
}
```

- [ ] **Step 2: Run it.** Expected: FAIL, `struct ClauseSpec has no field named adjuncts`.

- [ ] **Step 3: Restructure `ClauseSpec`.** Delete `frame`, `complement_concept`
      and `modifiers`; delete `enum Frame`. Add:

```rust
    /// The relation this clause asserts, as a concept id — `"is-a"` for a
    /// classification. The same string a `Fact` would carry, which is the
    /// point: an utterance is a fact, so the clause names its predicate
    /// instead of hiding one relation inside an enum variant.
    /// type-audit: bare-ok(identifier-text: predicate)
    pub predicate: String,
    /// What the predicate relates the subject to.
    pub object: Argument,
```

      Then re-key `common_constructions()` from `frame: Frame` to
      `predicate: &'static str`, with the single existing entry keyed `"is-a"`.

      **`Part::Complement` renders `spec.object`, and every `Argument` variant
      has an answer** — no variant panics, because a clause whose object is a
      quantity is a sentence we cannot say *yet*, not a crash:

      | variant | renders as |
      |---|---|
      | `Concept(id)` | exactly as today — `surface_complement(vocab, id, number)` |
      | `Name(text)` | verbatim, unresolved |
      | `Count(n)` | `cardinal(n)` |
      | `Quantity(x)` | `quantity(x)` |

      Every caller today passes a `Concept`, so no committed byte moves.
      Keep `surface_complement`'s naive plural — number is the next campaign
      and this one must not absorb it.

      Also add:

```rust
    /// Role bindings on this clause. How each surfaces — and whether it
    /// surfaces inline or trailing — is the realizing language's business.
    /// Replaced `modifiers: Vec<String>`, whose pre-rendered English could not
    /// cross a language boundary and had already leaked article selection into
    /// `windows/book`.
    pub adjuncts: Vec<Adjunct>,
```

- [ ] **Step 4: Rewrite the `Part::ModifierTail` arm** in `realize_common`. Note
      the trailing group is joined with `"; "` **before** the construction's
      terminal `"."`, matching what `windows/book` produced.

```rust
            Part::ModifierTail => {
                let mut inline: Vec<String> = Vec::new();
                let mut trailing: Vec<String> = Vec::new();
                for adjunct in &spec.adjuncts {
                    match common_role_surface(adjunct, vocab) {
                        Some((AdjunctPosition::Inline, text)) => inline.push(text),
                        Some((AdjunctPosition::Trailing, text)) => trailing.push(text),
                        None => {}
                    }
                }
                for (i, text) in inline.iter().enumerate() {
                    out.push_str(if i == 0 { " " } else { ", " });
                    out.push_str(text);
                }
                for text in &trailing {
                    out.push_str("; ");
                    out.push_str(text);
                }
            }
```

- [ ] **Step 5: Fix every construction site** the compiler names. Each site
      loses `frame` and `complement_concept` and gains all three of
      `predicate: "is-a".into()`, `object: Argument::Concept(<the old
      complement_concept>)`, and `adjuncts: Vec::new()` — unless a later task
      gives it real adjuncts. `cargo check -p hornvale-language --all-targets`.
- [ ] **Step 6: Run the crate suite.**
      `cargo nextest run -p hornvale-language --no-fail-fast > /tmp/hv-lang.txt 2>&1; echo "exit=$?"`
      then grep it. Expected: PASS.
- [ ] **Step 7: `cargo fmt`, regenerate the type-audit report, `make gate-commit`, commit.**

---

### Task 5: `windows/book` states meaning instead of composing English

**Files:**
- Modify: `windows/book/src/lib.rs`
- Test: existing tests in that file are the guard; add one new.

**Interfaces:**
- Consumes: `Adjunct`, `Argument`, `AdjunctPosition` from `domains/language`.
- Produces: `fragment_for` returns `Option<Adjunct>`; `Fragment` and
  `indefinite_article` are **deleted**.

- [ ] **Step 1: Write the failing test.**

```rust
#[test]
fn the_book_hands_the_language_structure_not_english() {
    let a = fragment_for(MOON_COUNT, &Value::Number(2.0))
        .expect("moon-count has a construction");
    assert_eq!(a.role, MOON_COUNT);
    assert_eq!(a.argument, Argument::Count(2));
}
```

- [ ] **Step 2: Run it.** Expected: FAIL — `fragment_for` still returns `Fragment`
      and still takes a `vocab`.

- [ ] **Step 3: Rewrite `fragment_for`.** It loses its `vocab` parameter: a
      structural adjunct names a concept id, and resolving it is the realizer's
      job.

```rust
/// The construction table: maps a (predicate, object) pair to the **adjunct**
/// it contributes, or `None` if this predicate has no construction yet
/// (leaving it on [`uncovered_predicates`]'s list).
///
/// It no longer renders. How a role surfaces moved to
/// `domains/language::clause::common_role_surface` with The Interlinear — a
/// window composing English is what forced this crate to duplicate
/// `indefinite_article`, and both are gone.
fn fragment_for(predicate: &str, object: &Value) -> Option<Adjunct> {
    match (predicate, object) {
        (MOON_COUNT, Value::Number(n)) => Some(Adjunct {
            role: MOON_COUNT.to_string(),
            argument: Argument::Count(*n as u64),
        }),
        (STAR_CLASS, Value::Text(concept)) => Some(Adjunct {
            role: STAR_CLASS.to_string(),
            argument: Argument::Concept(concept.clone()),
        }),
        (DAY_LENGTH_STD, Value::Number(days)) => Some(Adjunct {
            role: DAY_LENGTH_STD.to_string(),
            argument: Argument::Quantity(*days),
        }),
        _ => None,
    }
}
```

- [ ] **Step 4: Delete `enum Fragment` and `fn indefinite_article`,** and
      rewrite every site the compiler names. The `Modifier`/`Trailing` split
      no longer exists here — every construction returns an `Adjunct` and the
      realizer decides position, so the accumulate-into-two-vectors code at each
      call site collapses into one `adjuncts` vector.
- [ ] **Step 5: `cargo check -p hornvale-book --all-targets`,** then the crate
      suite, captured and grepped.
- [ ] **Step 6: Byte-identity check — this is the task's real assertion.**

```
make rebaseline
git status --short -- book/src/gallery/ book/src/reference/
```

**Branch table, not a prediction:**
- Nothing moved → correct; the port is faithful. Commit.
- A gallery page moved → **STOP and read the diff.** Either the join order
  changed (inline/trailing grouping) or a role's surface differs by a
  character. Fix the surface, do not accept the diff.
- `docs/audits/type-audit-report.md` moved → expected, `pub` items changed.

- [ ] **Step 7: Confirm Common is still total.**
      `cargo nextest run -p hornvale -E 'test(common_is_total)'` — spec §7. Common
      losing totality would mean the realizer started refusing, which is the one
      way this refactor could change meaning rather than form.

- [ ] **Step 8: `cargo fmt`, `make gate-commit`, commit.**

---

### Task 6: The round trip survives — the campaign's real risk

Spec §8: bidirectionality is a *claim* the current design makes on the strength
of one construction. This is its first real test.

**Files:**
- Modify: `domains/language/src/clause.rs` (`parse_common`)
- Test: in-module

**Interfaces:**
- Consumes: everything above.
- Produces: no new public API if the round trip holds. **If it does not, that
  is a finding — record it and stop; do not write a second code path.**

- [ ] **Step 1: Write the failing test.**

```rust
#[test]
fn a_clause_with_adjuncts_round_trips_through_common() {
    let mut v = CommonVocabulary::default();
    v.declare("yellow-white-dwarf", "yellow-white dwarf");
    let spec = ClauseSpec {
        predicate: "is-a".into(),
        subject: Subject::Name("Hornvale".into()),
        object: Argument::Concept("planet".into()),
        number: Number::Sg,
        definiteness: Definiteness::Indef,
        adjuncts: vec![Adjunct {
            role: "moon-count".into(),
            argument: Argument::Count(2),
        }],
    };
    let text = realize_common(&spec, &v);
    let ctx = ParseContext {
        complements: ["planet".to_string()].into_iter().collect(),
        vocabulary: v,
    };
    assert_eq!(parse_common(&text, &ctx).expect("round trips"), spec);
}
```

- [ ] **Step 2: Run it and RECORD what happens.** This step's output is the
      finding, whichever way it goes.
      `cargo test -p hornvale-language round_trips -- --nocapture`

- [ ] **Step 3: Branch on the result.**
  - **Parses, adjuncts recovered** → the claim holds. Note it in the commit
    message and move on.
  - **Parses, adjuncts lost (empty vec)** → the round trip is lossy. Make the
    loss *explicit*: `parse_common` returns a spec with no adjuncts, and its
    doc says so in one sentence naming what a caller may not assume. Add a test
    pinning the lossy behaviour so nobody later mistakes it for a bug. **Do not
    build adjunct recognition in this campaign** — that is the questions
    campaign's problem and it needs the corpus first.
  - **Refuses to parse at all** → the trailing `"; "` broke the terminal-`.`
    walk. Fix the walk. If fixing it requires a second construction path,
    **STOP** and report: that is spec §3's C1 tripwire firing.

- [ ] **Step 4: `cargo fmt`, `make gate-commit`, commit** with the finding in
      the message.

---

### Task 7: A tongue realizes an adjunct, or gaps honestly

**Files:**
- Modify: `domains/language/src/grammar.rs`
- Test: in-module

**Interfaces:**
- Consumes: `Adjunct`, `Argument`.
- Produces: `TongueClause.adjuncts: Vec<Adjunct>` and a tongue role surface
  that gaps rather than emitting Common.

**An asymmetry this task leaves in place, deliberately.** `ClauseSpec` becomes
fact-shaped in Task 4 while `TongueClause` keeps `subject: String` (already
surfaced) and `complement_concept`. They are arguably the same object plus a
speaker, and collapsing them is attractive — but that is a second restructuring
riding on the first, and Task 6 is already this campaign's risk. Record it as a
followup; do not do it here.

- [ ] **Step 1: Write the failing tests.** Two, and the second matters more.
      Both use the helpers that already exist in `grammar.rs`'s test module —
      `tiny_lexicon_with(&[(&str, ExposureClass)])` and `test_phonology()` — and
      the second is modelled directly on the existing
      `realize_tongue_gaps_with_the_lexicon_own_gap_reason`, which is the
      pattern for a `LexEntry::Gap` carrying its own `GapReason`.

```rust
    #[test]
    fn a_tongue_realizes_an_adjunct_whose_concept_it_knows() {
        let lex = tiny_lexicon_with(&[
            ("planet", ExposureClass::Steeped),
            ("yellow-white-dwarf", ExposureClass::Steeped),
        ]);
        let star_word = match lex.entry("yellow-white-dwarf").unwrap() {
            LexEntry::Root { views, .. } => views.roman.clone(),
            other => panic!("expected a Root, got {other:?}"),
        };
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "planet".into(),
            evidential: Evidential::Witnessed,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            }],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let out = realize_tongue(&clause, &g, &lex).expect("both concepts are known");
        assert!(
            out.contains(&star_word),
            "the tongue's own word for the star class must appear: {out}"
        );
        assert!(
            !out.contains("orbiting"),
            "Common's role surface must not leak into a tongue: {out}"
        );
    }

    #[test]
    fn a_tongue_gaps_on_an_adjunct_concept_it_lacks_rather_than_emitting_common() {
        // The COMPLEMENT is known; only the ADJUNCT's concept is missing, so a
        // partial render is the tempting wrong answer. Spec section 4 of this
        // module: renders fully or gaps entirely, never partially.
        let lex = tiny_lexicon_with(&[("planet", ExposureClass::Steeped)]);
        let clause = TongueClause {
            subject: "Vavako".into(),
            complement_concept: "planet".into(),
            evidential: Evidential::Witnessed,
            adjuncts: vec![Adjunct {
                role: "star-class".into(),
                argument: Argument::Concept("yellow-white-dwarf".into()),
            }],
        };
        let g = TongueGrammar {
            order: ConstituentOrder::Svo,
            copula: Some("gha".into()),
            copula_segments: None,
            articles: false,
        };
        let gap = realize_tongue(&clause, &g, &lex).unwrap_err();
        assert_eq!(gap.concept, "yellow-white-dwarf");
        assert!(!gap.reason.is_empty(), "recountable reason required");
    }
```

- [ ] **Step 2: Run them.** Expected: FAIL, `TongueClause` has no `adjuncts`.
- [ ] **Step 3: Implement.** Add `adjuncts: Vec<Adjunct>` to `TongueClause`;
      realize each through the tongue's lexicon. **A tongue that lacks the
      concept gaps the whole clause** — spec §4 of the grammar module already
      mandates "renders fully or gaps entirely, never partially", and an
      adjunct is part of the clause.
- [ ] **Step 4: Run them.** Expected: PASS.
- [ ] **Step 5: `cargo fmt`, regenerate the type-audit report, `make gate-commit`, commit.**

---

### Task 8: The target — a real occupation, both ways

**Files:**
- Create: `windows/almanac/tests/suite/interlinear.rs` (+ declare it in
  `windows/almanac/tests/suite.rs`)
- Modify: `domains/language/src/clause.rs` (Step 2 adds four role constructions
  and their tests)

**Interfaces:**
- Consumes: everything above. Lives in `windows/almanac` because a **window**
  may read a domain; `domains/language` may not reach `domains/history`.

- [ ] **Step 1: Read before writing.** This task's test needs three things
      whose exact shapes are NOT reproduced here on purpose, because a plan's
      code is the one code nothing compiles: how `windows/almanac`'s existing
      suite builds a seed-42 world (copy the pattern from a neighbouring file in
      `windows/almanac/tests/suite/`), how it reads occupation facts
      (`windows/almanac/src/history.rs:193` uses
      `ledger.find(hornvale_history::IS_OCCUPATION)`), and how a tongue's
      lexicon and grammar are obtained for a real species rather than a test
      fixture. Grep each before writing a line.

- [ ] **Step 2: Add Common role constructions** for `occ-people`, `occ-site`,
      `occ-founded` and `occ-ended` to `common_role_surface`, each with its own
      in-module test in `clause.rs` asserting the exact surface, in the style of
      Task 3's three. Choose surfaces that read as prose, not as a data dump —
      e.g. `occ-founded` + `Argument::Count(312)` → `"founded in year 312"`.

- [ ] **Step 3: Write the failing test** in
      `windows/almanac/tests/suite/interlinear.rs`: find one occupation, build
      **one** `ClauseSpec` from its facts, and realize it twice — once through
      Common, once through a real species' tongue.

- [ ] **Step 4: Assert three properties.** Name the property; do not hardcode a
      sentence, because the seed-42 occupation roster is not this campaign's to
      pin.
      1. the Common rendering contains the people's name, the site, and both
         years;
      2. the tongue rendering shares no word with the Common rendering **except
         proper names** — the discriminating check, and the one that fails if
         Common leaks;
      3. a tongue lacking one of the concepts returns `TongueGap` naming it.

- [ ] **Step 5: Declare the module** in `windows/almanac/tests/suite.rs`, run,
      `cargo fmt`, regenerate the type-audit report, `make gate-commit`, commit.

**Success:** the interlinear itself — one structure, two languages, a real
world.

---

### Task 9: The resolver, and the first coverage number

**Files:**
- Modify: `cli/tests/suite/sentence_corpus.rs`
- Create: `docs/audits/sentence-coverage.md`

- [ ] **Step 1: Write the resolver test, WITH A POSITIVE CONTROL.** For each
      corpus entry, a demand is **covered** if this campaign implements it
      (`classify` only), **not yet** otherwise.

      **No merchant entry demands `classify`** — every line of that dialogue
      needs something this campaign does not build — so the merchant corpus
      scores **zero**, honestly. That is the expected result and also a trap: a
      measurement whose only possible answer is zero cannot tell a working
      resolver from a broken one.

      So assert **two** things, and the second is the one that matters:

      1. merchant coverage is exactly 0 of 12;
      2. the resolver reports **covered** for a synthetic entry demanding only
         `classify`, constructed inline in the test — the positive control.

      Without (2) this test passes if `resolve` returns "not yet"
      unconditionally, which is the same defect class as a guard that has never
      gone red.
- [ ] **Step 2: Write the report** to `docs/audits/sentence-coverage.md`: total
      entries, covered, not-yet, and the per-demand tally.

      **Do NOT add it to `docs/generated-paths.txt`.** Nothing in
      `scripts/regenerate-artifacts.sh` writes it, so a drift check over it
      would be silently vacuous — and the precedent is two days old:
      `docs/audits/lexicon-inventory.tsv` is written by a test under
      `HV_LEXICON_REBASELINE=1` and is deliberately not declared. Follow it
      exactly: the report is rewritten under `HV_SENTENCE_REBASELINE=1`, and the
      test otherwise asserts the covered count against a frozen constant, so the
      number is guarded even if the prose file goes stale.
- [ ] **Step 3: Run, `cargo fmt`, `make gate-commit`, commit.**

**Success:** a number that is honestly near zero, and a map of the program.

---

### Task 10: Definition of Done

- [ ] Decision record `docs/decisions/0266-an-utterance-is-a-fact.md` — the
      foundation and why not a role ontology. Use the reserved block.
- [ ] Chronicle `book/src/chronicle/the-interlinear.md`, wired into `SUMMARY.md`.
- [ ] Retrospective `docs/retrospectives/the-interlinear.md`.
- [ ] Book freshness sweep; Confidence Gradient re-scored or explicitly N/A.
- [ ] Idea-registry row for the program's next rungs.
- [ ] `make rebaseline` + `make rebaseline-goldens`; **only** `docs/audits/`,
      `docs/digest/` and this campaign's own pages may differ.
- [ ] Full workspace suite + doctests, captured and grepped.
- [ ] `make vessel-check`, `make world-check`, `make game-check`.
- [ ] `make seam-guard` (needs a clean tree of tracked files).
- [ ] Authored `Sluice-Headline:` trailer, same block as `Claude-Session`, no
      blank line between.
- [ ] `make sluice BRANCH=campaign/the-interlinear REF=<full-sha>`.
