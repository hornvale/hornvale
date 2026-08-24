# The Lexicon Implementation Plan

> **REQUIRED SUB-SKILL:** Use the executing-plans skill to implement this plan task-by-task.

**Goal:** Tab completion and noun-entry mode for the game client — the sim emits the candidate vocabulary with kind tags; the client matches and dispatches text.

**Architecture:** Additive `kind` field on `NounEntry` in `vessel/session/v1` (sim-side), a composed-scope `Lexicon` plus pure completion engine in `hornvale-game-core`, and routing/presentation (`Action::Complete`, `TabStyle::{Hint,Cycle}`, hint line with bolded stem, noun-entry modal) in `clients/game/bin` + `entry.rs`.

**Tech Stack:** Rust 2024, serde, crossterm; clients are OUTSIDE the cargo workspace (own gates: `make game-check`). Spec: `docs/superpowers/specs/2026-08-23-the-lexicon-design.md`.

**Conventions that bind every task:** no new external deps; `BTreeMap`/`Vec` only; every pub item gets a one-line doc comment; `type-audit:` tags on pub-boundary primitives; `cargo fmt` before every commit; run `make game-check` (fmt/clippy/test on both game crates) at each phase checkpoint. Work happens in `.claude/worktrees/the-lexicon`.

**EXECUTION NOTE:** subagent harness kills on >120 s of single-command inactivity — every cargo invocation must stay under ~90 s (scope with `-p <crate>` + name filters; warm with short scoped builds first).

---

## Phase 1 — Sim emits kinds

### Task 1: `NounKind` and `kind` on `focalize::Noun`

**TDD scenario:** new feature — full cycle.

**Files:**
- Modify: `windows/vessel/src/focalize.rs`
- Test: `windows/vessel/src/focalize.rs` (in-module tests)

**Step 1: Failing test**

```rust
#[test]
fn a_noun_defaults_to_unknown_and_with_kind_sets_it() {
    let n = Noun::new("tropical seasonal forest", "forest", "warm.");
    assert_eq!(n.kind, NounKind::Unknown);
    let p = n.clone().with_kind(NounKind::Place);
    assert_eq!(p.kind, NounKind::Place);
}
```

**Step 2:** `cargo test -p hornvale-vessel focalize` → FAIL (no `NounKind`).

**Step 3: Implement**

```rust
/// The coarse kind a completion-capable client may filter on. Closed set;
/// `Unknown` is the honest default where the sim claims nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NounKind {
    /// A living or animate being.
    Creature,
    /// A location: biome, regime descriptor, village.
    Place,
    /// An object or phenomenon that is neither of the above.
    Thing,
    /// No kind claimed.
    Unknown,
}
```

Add `pub kind: NounKind` to `struct Noun`. Keep `Noun::new(display, nameable,
datum)` unchanged, constructing `kind: NounKind::Unknown`; add builder:

```rust
pub fn with_kind(mut self, kind: NounKind) -> Noun { self.kind = kind; self }
```

Tag existing construction sites IN `focalize.rs::render`: biome and
descriptor → `Place`; village → `Place`; sky + sky bodies → `Thing`.
(`session.rs` call sites stay `Unknown` this task.)

**Step 4:** test passes; `cargo test -p hornvale-vessel` green.

**Step 5: Commit** — `feat(vessel): NounKind on focalized nouns`.

### Task 2: `kind` rides the wire additively

**Files:**
- Modify: snapshot module defining `snapshot::NounEntry` (grep `struct NounEntry`)
- Modify: `windows/vessel/src/session.rs` mapping site (~line 1171)

**Step 1:** extend `snapshot::NounEntry`:

```rust
/// Coarse kind for completion-capable clients. Optional on the wire:
/// older mirrors load unchanged (serde default), newer fixtures carry it.
/// Additive on `vessel/session/v1` per the schema discipline.
#[serde(default)]
pub kind: String,
```

Map `focalize::NounKind` → lowercase wire tag at the construction site.

**Step 2:** update vessel fixture tests asserting exact `NounEntry` JSON;
expect small additive diffs.

**Step 3:** `cargo test -p hornvale-vessel` green; commit —
`feat(vessel): noun kind rides session/v1 additively`.

### Task 3: Client schema mirror + fixture regen

**Files:**
- Modify: `clients/game/core/src/schema.rs` (`NounEntry`)
- Regen: committed fixtures via rebaseline

**Step 1:** mirror the field (`#[serde(default)] pub kind: String` + doc).

**Step 2:** `cargo test -p hornvale-game-core` green.

**Step 3:** `make rebaseline`; inspect drift; expect only client fixture files.

**Step 4:** commit — `chore(artifacts): client fixtures carry noun kinds`.

**Phase checkpoint:** `make game-check` green.

---

## Phase 2 — Core lexicon and completion engine

### Task 4: `lexicon.rs` — candidates, scopes, fold

**Full TDD. Create `clients/game/core/src/lexicon.rs`; re-export from lib.rs.**

Failing table-driven test first:

```rust
#[test]
fn the_lexicon_folds_scopes_first_wins() {
    let lex = Lexicon::new(vec![
        Box::new(StaticScope(vec![cand("goblin", Category::Creature)])),
        Box::new(StaticScope(vec![cand("goblin", Category::Thing),
                                 cand("rug", Category::Thing)])),
    ]);
    let names: Vec<_> = lex.candidates().iter().map(|c| c.name.clone()).collect();
    assert_eq!(names, vec!["goblin", "rug"]); // first scope wins the dup
}
```

Implement:

```rust
pub enum Category { Creature, Place, Thing, Unknown }
pub struct Candidate { pub name: String, pub category: Category }
pub trait CandidateSource { fn candidates(&self) -> Vec<Candidate>; }
pub struct CurrentTurnNouns { /* Vec<Candidate>, set from Narration */ }
pub struct Lexicon { sources: Vec<Box<dyn CandidateSource>> }
```

`CurrentTurnNouns::update(&Narration)` maps each `NounEntry` → `Candidate`,
parsing `kind` back to `Category` (unknown strings → `Unknown`). Doc
comments everywhere; type-audit tags consistent with neighbours.
Commit — `feat(game-core): composed candidate lexicon`.

### Task 5: Completion engine

Same module. Full cycle. Failing tests first:

```rust
// unique_prefix_completes -> Completion::Unique(full name)
// ambiguous_prefix_yields_stem_and_matches:
//   ["Vngashngatva","Vngashngakelm"], prefix "vng"
//   -> Prefix { stem: "Vngashnga", matches: [both, input order] }
// case_insensitive_match_preserves_candidate_casing
// no_match_is_none_and_empty_prefix_is_none
```

Implement `complete(prefix: &str, candidates: &[Candidate]) -> Completion`;
`enum Completion { None, Unique(String), Prefix { stem: String, matches:
Vec<String> } }`. Case-insensitive `starts_with`; stem = longest common
prefix over ALL matches computed CHAR-wise; ordering = input order, never
sorted. Commit — `feat(game-core): prefix completion engine`.

---

## Phase 3 — Routing and presentation

### Task 6: `Action::Complete` under Cli focus

Modify `clients/game/bin/src/input.rs`. Flip the totality-table expectation
for `Tab` under Cli; rewrite `tab_is_bound_to_nothing_in_any_focus` into
`tab_completes_only_under_the_cli` (Cli → `Complete`; Map/Walk → `None`);
run failing; add variant + `KeyCode::Tab => Action::Complete` in the Cli arm
only; green; commit — `feat(game-bin): Tab routes Complete under Cli focus`.

### Task 7: Buffer token replace

Modify `clients/game/bin/src/line.rs`. Full cycle:

```rust
#[test]
fn replace_word_at_caret_swaps_only_the_trailing_token() {
    let mut l = Line::new(); l.set("examine vng".into());
    l.replace_word_at_caret("Vngashngatva");
    assert_eq!(l.text(), "examine Vngashngatva");
    assert_eq!(l.caret(), "examine Vngashngatva".len());
}
```

Implement `replace_word_at_caret`: last whitespace boundary ≤ caret, splice;
caret at whitespace boundary = documented no-op. Commit —
`feat(game-bin): caret-token replacement on Line`.

### Task 8: Driver wiring + TabStyle strategies

Modify `clients/game/bin/src/driver.rs`. Driver gains:
`lexicon: Lexicon` refreshed from parsed snapshot `narration.nouns` wherever
`self.cached` refreshes (one site); `hint: Option<Completion>` cleared on any
`Type`/`DeleteBack`/`Submit`; `tab_style: TabStyle` default `Hint`:

```rust
enum TabStyle { Hint, Cycle }
```

`apply` arms: `Complete` under Hint tokenizes buffer word at caret →
`complete()` → Unique replaces token; Prefix extends token to stem AND sets
hint; None no-op; always returns `false`. Under Cycle: state
`cycle: Option<(prefix, Vec<String>, usize)>`, successive Tabs rotate the
token; any other action drops it. Unit-tested, unbound (spec §4.2).
Commit — `feat(game-bin): complete action wired with pluggable style`.

### Task 9: Hint line rendering (weight channel)

Modify `clients/game/core/src/entry.rs` (+ `spread::compose` threading).

1. Failing draw test: row beneath the command line carries bold cells for
   chars `[0..stem_len]`, normal after — via the crate's `Cell` weight field
   (0142 attention channel; module doc states the refinement: typed-vs-
   suggested on completion surfaces, perishable ladder on charts; never
   co-rendered).
2. Overlong lists collapse: fitting matches then `… +N more`.
3. Implement behind `Option<&Completion>`; absent → byte-identical layout
   (existing entry tests staying green untouched IS the regression guard).
Commit — `feat(game-core): bolded-stem hint line under the command row`.

### Task 10: Noun-entry modal for bare `x`

Modify `clients/game/bin/src/driver.rs`. Intercept in `apply`'s `Submit` arm
BEFORE history push: trimmed line == `"x"` → enter modal instead of
dispatching. Modal state `noun_prompt: Option<String>`: prompt renders
`examine:`; edits touch only the modal string; `Enter` dispatches
`examine <noun>` through the SAME path Submit uses; `Esc` cancels restoring
the untouched buffer; completion inside reads the same lexicon. Bare-x entry
costs NO turn. Commit —
`feat(game-bin): bare x prompts for a noun instead of burning a turn`.

---

## Phase 4 — Close

### Task 11: Docs, registry, gate

1. Registry rows CLIENT-tab-completion / CLIENT-noun-entry-for-examine →
   shipped with Where cells; reserved GWIM row if absent.
2. Chronicle `book/src/chronicle/the-lexicon.md`.
3. `make rebaseline` if fixtures moved; drift check clean.
4. `make game-check`; `cargo fmt`; `make gate-commit` for workspace-side changes.
5. Final commit; stage gate / merge per CLAUDE.md process.
