# C3 — The Tongues Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Each placed people's self-statement ("The Vavako are goblins.") renders in its own tongue — drawn constituent order, drawn copula presence, its own lexicon's words — in a new per-volume tongues section, with a per-tongue coverage report that honestly gaps the planet sentence (`no word for planet`).

**Architecture:** `domains/language` gains `grammar.rs`: a `TongueGrammar` (constituent order / copula / articles, drawn from three NEW permanent stream labels) and one parametric realizer `realize_tongue` over a `TongueClause`, lexicalizing through the speaker's `Lexicon` and gapping whole-sentence on any missing word. `windows/book` (gaining a real `hornvale-worldgen` dependency — the windows/lab precedent — for `lexicon_of`/`placed_peoples`) renders the tongues section + gloss and the per-tongue coverage report. Tongues are render-only: the Echo's corpus law stays scoped to Common.

**Tech Stack:** Rust 2024, std-only, serde only. No new crates. Draws via kernel `Seed`/`Stream` only.

## Global Constraints

- **New permanent stream labels (G3-approved, exactly these):** `language/<species>/grammar/constituent-order`, `language/<species>/grammar/copula`, `language/<species>/grammar/articles` — registered in `domains/language::stream_labels()`; once declared, forever (save-format contract).
- **Zero ledger drift; lexicons byte-identical.** No new concepts, no lexeme changes, no committed facts. Grammar params are build-state (0058): drawn at render/composition time, never serialized. Expected artifact drift: the-book.md gallery (tongues sections), the streams-manifest reference page (3 new labels), layering-generated.md (book→worldgen edge), type-audit counters — each in its drifting commit. Anything else drifting = defect.
- **Gap honesty:** a tongue sentence renders fully or not at all; a `Gap` aborts with the recountable reason into the coverage report — never a partial or mixed-language sentence.
- **Render-only:** no parse direction for tongues; the Echo corpus law's scope (Common) is untouched.
- No `HashMap`/`HashSet`; `#![warn(missing_docs)]` + `type-audit:` tags on new pub-boundary primitives (run the check before committing); `cargo fmt` final step every commit. NEVER regenerate censuses locally.

---

## File structure

- Create: `domains/language/src/grammar.rs` — `ConstituentOrder`, `TongueGrammar`, `tongue_grammar(seed, species)`, `TongueClause`, `realize_tongue`.
- Modify: `domains/language/src/lib.rs` — `pub mod grammar;` + 3 new `stream_labels()` entries.
- Modify: `windows/book/src/lib.rs` + `windows/book/Cargo.toml` — tongues section, per-tongue coverage, worldgen dep.
- Regenerated in drifting commits: `book/src/gallery/the-book.md`, the streams-manifest reference page, `book/src/reference/layering-generated.md`, `docs/audits/type-audit-report.md`.

---

### Task 1: `TongueGrammar` — the drawn parameters + streams

**Files:**
- Create: `domains/language/src/grammar.rs`
- Modify: `domains/language/src/lib.rs` (module decl + stream_labels entries)
- Test: `grammar.rs` `#[cfg(test)]`

**Interfaces:**
- Produces: `pub enum ConstituentOrder { Sov, Svo, Vso, Vos, Ovs, Osv }`; `pub struct TongueGrammar { pub order: ConstituentOrder, pub copula: Option<String>, pub articles: bool }` — `copula` is `Some(roman form)` for copula-bearing tongues, the form DRAWN from the tongue's own phonology (one syllable, the `proto_root`/`Namer` fill mechanism — zero authored surface text, per the program thesis); `pub fn tongue_grammar(seed: &Seed, species: &str, ph: &Phonology) -> TongueGrammar`.
- Consumes: kernel `Seed`/`Stream` (the `draw_cascade` derivation-path pattern at `domains/language/src/etymology.rs:160`).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn tongue_grammar_is_deterministic_and_species_keyed() {
    let seed = Seed(42);
    let ph = test_phonology(); // per phonology.rs's own test-constructor pattern
    let a = tongue_grammar(&seed, "goblin", &ph);
    let b = tongue_grammar(&seed, "goblin", &ph);
    assert_eq!(a, b, "same seed+species → same grammar");
    let kobold = tongue_grammar(&seed, "kobold", &ph);
    let hobgoblin = tongue_grammar(&seed, "hobgoblin", &ph);
    // Not all three species may differ on every field, but the draw must
    // be species-keyed: assert the tuple of all three grammars is not
    // three identical values for at least one field across seeds 1..=20
    // (a constant function would fail this).
    let differs = (1..=20).any(|s| {
        let seed = Seed(s);
        let g = tongue_grammar(&seed, "goblin", &ph);
        let k = tongue_grammar(&seed, "kobold", &ph);
        g != k
    });
    assert!(differs, "grammar draws are species-keyed, not constant");
    let _ = (kobold, hobgoblin);
}

#[test]
fn constituent_order_weights_favor_sov_and_svo() {
    // Authored typology (approximate WALS frequencies): SOV+SVO must
    // dominate the draw. Measure over 200 seeds.
    let mut sov_svo = 0;
    for s in 1..=200u64 {
        let g = tongue_grammar(&Seed(s), "goblin", &ph);
        if matches!(g.order, ConstituentOrder::Sov | ConstituentOrder::Svo) {
            sov_svo += 1;
        }
    }
    assert!(sov_svo > 120, "SOV+SVO should dominate (~87% authored): {sov_svo}/200");
}
```

- [ ] **Step 2: Run** `cargo test -p hornvale-language tongue_grammar` → FAIL (module missing).

- [ ] **Step 3: Implement `grammar.rs`.** Module doc states: floor slice of LANG-40's grammaticalization-depth vector — C7 extends this struct, never replaces it. Draw weights (authored typology, approximate WALS): order — SOV 45, SVO 42, VSO 9, VOS 2, OVS 1, OSV 1 (percent); copula present 60 / absent 40; articles present 30 / absent 70. Derivation paths per the etymology pattern:

```rust
use hornvale_kernel::Seed;

/// The six constituent orders of a subject–copula–complement clause.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstituentOrder {
    /// Subject–Object(complement)–Verb(copula).
    Sov,
    /// Subject–Verb–Object.
    Svo,
    /// Verb–Subject–Object.
    Vso,
    /// Verb–Object–Subject.
    Vos,
    /// Object–Verb–Subject.
    Ovs,
    /// Object–Subject–Verb.
    Osv,
}

/// A tongue's drawn surface grammar — the floor slice of the
/// grammaticalization-depth vector (LANG-40): C7 extends this struct.
/// Word order is historically contingent, so these are DRAWN, never
/// derived from culture vectors (spec §3). Build-state (0058): drawn at
/// composition time, never serialized.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TongueGrammar {
    /// Constituent order for predication clauses.
    pub order: ConstituentOrder,
    /// The overt copula's roman form for copula-bearing tongues (drawn
    /// from the tongue's own phonology — never authored), or `None` for
    /// zero-copula tongues.
    pub copula: Option<String>,
    /// Whether the language has articles. The floor realizer renders no
    /// article surface (no article lexeme exists yet — C7's morphology
    /// gives this parameter its surface); the parameter is drawn now
    /// because the stream label is a permanent contract and article-hood
    /// is a fact of the language, not of its current renderer.
    pub articles: bool,
}

/// Draw `species`' tongue grammar from the three permanent grammar
/// streams (`language/<species>/grammar/…`).
/// type-audit: bare-ok(identifier-text)
pub fn tongue_grammar(seed: &Seed, species: &str, ph: &Phonology) -> TongueGrammar {
    let mut order_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("constituent-order")
        .stream();
    let roll = order_stream.range_u32(0, 100);
    let order = if roll < 45 {
        ConstituentOrder::Sov
    } else if roll < 87 {
        ConstituentOrder::Svo
    } else if roll < 96 {
        ConstituentOrder::Vso
    } else if roll < 98 {
        ConstituentOrder::Vos
    } else if roll < 99 {
        ConstituentOrder::Ovs
    } else {
        ConstituentOrder::Osv
    };
    let mut copula_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("copula")
        .stream();
    let copula = if copula_stream.range_u32(0, 100) < 60 {
        // Draw the copula's form from the tongue's own phonology: one
        // syllable via the same fill mechanism proto_root/Namer use
        // (verify the exact helper — a 1-syllable draw from `ph`'s
        // templates using `copula_stream` — and render its roman view via
        // the same path LexEntry words use). Presence and form share the
        // one permanent copula stream.
        Some(draw_copula_form(&mut copula_stream, ph))
    } else {
        None
    };
    let mut articles_stream = seed
        .derive("language")
        .derive(species)
        .derive("grammar")
        .derive("articles")
        .stream();
    let articles = articles_stream.range_u32(0, 100) < 30;
    TongueGrammar {
        order,
        copula,
        articles,
    }
}
```

(Verify `Stream::range_u32`'s exact name/semantics against `etymology.rs`'s usage — inclusive vs exclusive bounds — and adapt the threshold arithmetic if needed; the WEIGHTS are the contract, not the helper name.)

- [ ] **Step 4: `lib.rs`:** add `pub mod grammar;` with a one-line doc; append to `stream_labels()`:

```rust
(
    "language/<species>/grammar/constituent-order",
    "the tongue's drawn constituent order for predication (SOV/SVO dominant, per authored typological weights)",
),
(
    "language/<species>/grammar/copula",
    "whether nominal predication carries an overt copula, and (when it does) the copula\'s one-syllable drawn form from the tongue\'s own phonology",
),
(
    "language/<species>/grammar/articles",
    "whether the tongue has articles (floor: drawn but surfaces no article lexeme until the morphology campaign)",
),
```

- [ ] **Step 5: Run** the tests → PASS. Regenerate the streams-manifest reference page: check `bash scripts/regenerate-artifacts.sh` regenerates it (it runs the CLI `streams` dump); `git diff --stat book/src/reference/` — expect ONLY the streams-manifest page to gain the 3 labels. Commit it WITH this change. `type-audit -- check`; `cargo fmt`; commit: `feat(language): TongueGrammar — drawn constituent order, copula, articles (C3 T1)`.

---

### Task 2: `realize_tongue` — the parametric realizer

**Files:**
- Modify: `domains/language/src/grammar.rs`
- Test: same `#[cfg(test)]`

**Interfaces:**
- Consumes: T1's `TongueGrammar`; `crate::lexicon::{Lexicon, LexEntry}`.
- Produces: `pub struct TongueClause { pub subject: String, pub complement_concept: String }`; `pub fn realize_tongue(clause: &TongueClause, grammar: &TongueGrammar, lexicon: &Lexicon) -> Result<String, TongueGap>`; `pub struct TongueGap { pub concept: String, pub reason: String }`.

- [ ] **Step 1: Write the failing tests** (build a tiny real lexicon via the crate's own machinery — look at `lexicon.rs`'s tests for how they construct one via `build_lexicon` with a small exposure map; reuse that helper pattern rather than mocking):

```rust
#[test]
fn realize_tongue_orders_and_copula() {
    // Grammar fixed by hand (not drawn) to pin each transform:
    let lex = tiny_lexicon_with(&[("goblin-kind", /* Steeped */)]); // per lexicon.rs test pattern
    let word = match lex.entry("goblin-kind").unwrap() {
        LexEntry::Root { views, .. } => views.roman.clone(),
        _ => panic!("root expected"),
    };
    let clause = TongueClause {
        subject: "Vavako".into(),
        complement_concept: "goblin-kind".into(),
    };
    let svo = TongueGrammar { order: ConstituentOrder::Svo, copula: Some("gha".into()), articles: false };
    assert_eq!(realize_tongue(&clause, &svo, &lex).unwrap(), format!("Vavako gha {word}."));
    let sov = TongueGrammar { order: ConstituentOrder::Sov, copula: Some("gha".into()), articles: false };
    assert_eq!(realize_tongue(&clause, &sov, &lex).unwrap(), format!("Vavako {word} gha."));
    let zero_copula = TongueGrammar { order: ConstituentOrder::Svo, copula: None, articles: false };
    assert_eq!(realize_tongue(&clause, &zero_copula, &lex).unwrap(), format!("Vavako {word}."));
}

#[test]
fn realize_tongue_gaps_whole_sentence() {
    let lex = tiny_lexicon_with(&[]); // no entries → concept is a gap
    let clause = TongueClause { subject: "Vavako".into(), complement_concept: "planet".into() };
    let g = TongueGrammar { order: ConstituentOrder::Svo, copula: Some("gha".into()), articles: false };
    let gap = realize_tongue(&clause, &g, &lex).unwrap_err();
    assert_eq!(gap.concept, "planet");
    assert!(!gap.reason.is_empty(), "recountable reason required");
}
```

**The copula surface:** a zero-copula tongue omits it; a copula-bearing tongue renders `grammar.copula`'s DRAWN form (T1 draws it from the tongue's own phonology — zero authored surface text, per the program thesis). The tests below fix the grammar by hand with `copula: Some("gha".into())` as an arbitrary TEST value only — production forms always come from the draw.

- [ ] **Step 2: Run** → FAIL.

- [ ] **Step 3: Implement:**

```rust
/// One nominal-predication clause for a tongue: an already-surfaced
/// subject (autonym / proper name — tongue words already) and the
/// complement as a CONCEPT ID to lexicalize in the speaker's lexicon.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueClause {
    /// The subject, already in surface form.
    pub subject: String,
    /// The complement concept id (e.g. `"goblin-kind"`), lexicalized via
    /// the speaker's lexicon.
    pub complement_concept: String,
}

/// A whole-sentence gap: the tongue could not say this clause.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(prose: reason)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TongueGap {
    /// The concept that failed to lexicalize.
    pub concept: String,
    /// The recountable reason (from the lexicon's gap, or "no entry").
    pub reason: String,
}

/// Realize a nominal-predication clause in a tongue: lexicalize the
/// complement, order the constituents per the grammar, include the copula
/// if the tongue bears one. Renders fully or gaps entirely (spec §4).
/// type-audit: bare-ok(prose)
pub fn realize_tongue(
    clause: &TongueClause,
    grammar: &TongueGrammar,
    lexicon: &Lexicon,
) -> Result<String, TongueGap> {
    let complement = match lexicon.entry(&clause.complement_concept) {
        Some(LexEntry::Root { views, .. }) | Some(LexEntry::Compound { views, .. }) => {
            views.roman.clone()
        }
        Some(LexEntry::Gap { reason }) => {
            return Err(TongueGap {
                concept: clause.complement_concept.clone(),
                reason: format!("{reason:?}"),
            });
        }
        None => {
            return Err(TongueGap {
                concept: clause.complement_concept.clone(),
                reason: "no entry in this lexicon".to_string(),
            });
        }
    };
    let s = clause.subject.as_str();
    let v = grammar.copula.as_deref();
    let o = complement.as_str();
    // Order the present constituents; None copula simply drops out.
    let ordered: Vec<&str> = match grammar.order {
        ConstituentOrder::Sov => [Some(s), Some(o), v],
        ConstituentOrder::Svo => [Some(s), v, Some(o)],
        ConstituentOrder::Vso => [v, Some(s), Some(o)],
        ConstituentOrder::Vos => [v, Some(o), Some(s)],
        ConstituentOrder::Ovs => [Some(o), v, Some(s)],
        ConstituentOrder::Osv => [Some(o), Some(s), v],
    }
    .into_iter()
    .flatten()
    .collect();
    Ok(format!("{}.", ordered.join(" ")))
}
```

(Match `LexEntry::Gap`'s actual `reason` type — if it's the structured `GapReason`, render its existing Display/recounting form instead of `{reason:?}`; check how the dictionary dump prints gap reasons and reuse that path. `views.roman` capitalization: roman views are capitalized (e.g. "Neekoa") — fine as surface words.)

- [ ] **Step 4: Run** → PASS. Also add a small exhaustive-order test: all 6 orders × copula on/off produce 12 distinct-or-documented surface shapes for a fixed clause (assert each expected string explicitly — 12 `assert_eq!`).

- [ ] **Step 5:** `type-audit -- check`; `cargo fmt`; commit `feat(language): realize_tongue — the parametric tongue realizer (C3 T2)`.

---

### Task 3: The Book's tongues section + per-tongue coverage

**Files:**
- Modify: `windows/book/src/lib.rs`, `windows/book/Cargo.toml` (move `hornvale-worldgen` from dev-dependencies to dependencies)
- Test: same file
- Re-pin in this commit: `book/src/reference/layering-generated.md` (new edge), `book/src/gallery/the-book.md` (tongues sections), type-audit report if counters move.

**Interfaces:**
- Consumes: T1/T2 (`tongue_grammar`, `realize_tongue`, `TongueClause`, `TongueGap`); `hornvale_worldgen::{lexicon_of, placed_peoples}` (verify exact signatures — `lexicon_of(world, kind)` and `placed_peoples(world) -> Vec<(&'static str, VillageInfo)>` approx; grep).
- Produces: `BookVolume` gains `pub tongue_lines: Vec<String>` (rendered tongue lines with glosses) and `pub tongue_gaps: Vec<String>` (the per-tongue coverage report lines, e.g. `"Goblinesque: gap — planet (no entry in this lexicon)"`); `render_volume` populates both; the gallery renderer (wherever the-book.md is written — find the CLI's book command) emits a "Tongues" subsection + a coverage note per volume.

- [ ] **Step 1: Write the failing tests:**

```rust
#[test]
fn every_placed_people_self_states_in_its_own_tongue() {
    // The self-statement law (spec §5): autonym + own-kind are Steeped,
    // so every placed people's line renders — no gaps.
    for seed in [1u64, 2, 3] {
        let world = generated(seed);
        let vol = render_volume(&world);
        let peoples = hornvale_worldgen::placed_peoples(&world);
        assert_eq!(
            vol.tongue_lines.len(),
            peoples.len(),
            "seed {seed}: one tongue line per placed people"
        );
        for line in &vol.tongue_lines {
            assert!(line.contains(" ("), "line carries a gloss: {line}");
        }
    }
}

#[test]
fn the_planet_sentence_gaps_in_every_tongue() {
    // The gap law (spec §5): no culture holds `planet`.
    let world = generated(1);
    let vol = render_volume(&world);
    let peoples = hornvale_worldgen::placed_peoples(&world);
    assert_eq!(vol.tongue_gaps.len(), peoples.len(), "one planet gap per tongue");
    for gap in &vol.tongue_gaps {
        assert!(gap.contains("planet"), "the gap names the concept: {gap}");
    }
}

#[test]
fn tongue_lines_are_deterministic() {
    let a = render_volume(&generated(1)).tongue_lines;
    let b = render_volume(&generated(1)).tongue_lines;
    assert_eq!(a, b);
}
```

- [ ] **Step 2: Run** → FAIL (fields missing).

- [ ] **Step 3: Implement.** In `render_volume`, after the instance-of loop: for each `(kind, _)` in `placed_peoples(world)` (this yields deterministic registry order): build the lexicon (`lexicon_of(world, kind)` — or the in-scope variant; if `lexicon_of` needs `WorldComponents`, assemble via the same path windows/book's tests use, or call the public `lexicon_of` which self-assembles — prefer the public self-assembling one and note the per-call cost as acceptable at volume scale), obtain the phonology via `hornvale_worldgen::language_of(world, kind)` (public, verified at worldgen lib.rs:1692), draw `tongue_grammar(&world.seed, kind, &ph)`, resolve the autonym (the collective's committed `name` — already found for the Common line; reuse), and realize `TongueClause { subject: autonym, complement_concept: format!("{kind}-kind") }`. Success → push `format!("{line} (in the {kind} tongue: \"{common_line}\")")` — where `common_line` is that people's Common self-statement (reuse the string built earlier in the loop; restructure the instance-of loop to collect `(kind, autonym, common_line)` tuples first if needed). Failure → still an error: the self-statement must not gap (surface the TongueGap in the test failure).
  Then the planet-gap attempt: for each placed people, `realize_tongue(&TongueClause { subject: world-name…, complement_concept: "planet".into() }, …)` — expect `Err`; push `format!("{kind}: gap — {} ({})", gap.concept, gap.reason)` to `tongue_gaps`. (The subject for this attempt: the planet's committed name; it doesn't matter for the gap path but keep it real.)
  Then find where the CLI writes the-book.md (grep the `book` subcommand in `cli/src/`) and emit, per volume, a `## Tongues` (or the file's existing heading convention) subsection listing `tongue_lines`, then a coverage line per `tongue_gaps` entry. Keep the register dry.

- [ ] **Step 4: Run** `cargo test -p hornvale-book` → PASS; `cargo test -p hornvale` (architecture + CLI, foreground, long timeout) → the architecture test must accept book→worldgen (window→window; if an explicit allowlist exists, add the edge); regenerate artifacts (`bash scripts/regenerate-artifacts.sh`) → expect the-book.md + layering-generated.md (+ type-audit counters) to drift; inspect the tongues sections by eye; commit ALL drift in this commit.

- [ ] **Step 5:** `type-audit -- check`; `cargo fmt`; commit `feat(book): the tongues section — each people self-states in its own language (C3 T3)`.

---

### Task 4: The gate

**Files:** none new (verification task; artifact drift only if T1-T3 missed something — which is a defect to report, not fix).

- [ ] **Step 1:** `bash scripts/regenerate-artifacts.sh && git diff --stat` → EMPTY (everything landed in its drifting commit). Non-empty → BLOCKED with the diff.
- [ ] **Step 2:** `make gate > /tmp/tongues-gate.txt 2>&1; echo "EXIT=$?"` (foreground, timeout 3600000) → EXIT=0; paste the Summary line. Census/lab files in `git status` → BLOCKED.
- [ ] **Step 3:** `cargo fmt` (no-op expected); no commit needed if clean — reply DONE with the gate evidence.

---

## Self-review notes

- **Spec coverage:** §2.1→T1 · §2.2/§4→T2 · §2.3/§5 (self-statement law, gap law, determinism)→T3 · §5 byte-identity + §6 blast radius→T4 + each task's drift discipline · §3 streams/layering→T1/T3.
- **Verify-before-final:** `Stream::range_u32` bounds semantics; `LexEntry::Gap.reason`'s type and its existing display path; `lexicon_of` vs `lexicon_of_in` (self-assembling vs injected `wc`); `placed_peoples` tuple shape; the CLI book-command location; the architecture test's edge handling; the gallery's heading conventions.
- **Type consistency:** `TongueGrammar`/`TongueClause`/`TongueGap`/`realize_tongue` names identical across T1/T2/T3; `BookVolume.tongue_lines`/`tongue_gaps` per T3's interface block.
- **The copula form is DRAWN, never authored** (program thesis: zero authored surface text in generated tongues) — `draw_copula_form(&mut Stream, &Phonology) -> String` is T1's to implement beside the weights; the test-only literal "gha" appears solely as a hand-fixed grammar in T2's unit tests.
