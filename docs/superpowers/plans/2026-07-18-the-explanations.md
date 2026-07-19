# The Explanations (C5) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The causal filter — per-culture folk explanations of the day and
the moons, derived from a closed 12-schema library bound to each culture's
own pantheon, rendered as corpus-law-bidirectional because-clauses in the
Book's chorus.

**Architecture:** Pure substrate + schema/mapping tables in
`domains/language/src/schemas.rs`; `Disposition::Explained` (dial-blind
read-through) in `account.rs`; prior/β/binding derivation in worldgen's
`chorus.rs` (period-match join to `beliefs_held_by`); because-clause
surface + parse in `windows/book`. Spec:
`docs/superpowers/specs/2026-07-18-the-explanations-design.md`
(G3-approved).

**Tech Stack:** Rust edition 2024, std + serde only.

## Global Constraints

- **No epoch, no ledger change, genesis byte-identical.** The ONLY new
  seeded draws are two render-time streams with labels declared in
  `domains/language`'s streams module:
  `language/<species>/schema/<domain>/<fact-shape>` and
  `language/<species>/lexeme/<fact-key>`. New labels are additive-safe.
  No new concepts, predicates, or facts.
- **No new lab metrics** (G3 ruling — census not spent). The six existing
  dial metrics must be **explanation-blind**: they read through
  `Explained` to its `underlying` disposition, so every census/chorus-study
  value stays byte-identical. Task 4 VERIFIES this by re-running the
  50-seed chorus study and requiring a clean diff.
- **Phenomena anonymity is constitutional:** no consumer may learn which
  system produced a phenomenon. The day-explainer binding goes by
  PERIOD-MATCH (the cyclic belief whose re-derived `period_days` matches
  the world's committed `day-length-std` within 1% relative tolerance),
  never by a "sun" identity. Unbindable → don't fire — no synthetic
  agents, ever.
- No `HashMap`/`HashSet`; BTree/Vec; `total_cmp` for float sorts. Every
  pub item doc-commented (`missing_docs`); `type-audit:` tags on new pub
  primitives at introduction. `cargo fmt` before every commit.
- The live file is the authority: verify every consumed signature against
  checked-out code (C4's APIs: `hornvale_language::account::*`,
  `hornvale_worldgen::{accounts_of, chorus_ground, ChorusVoice,
  observability_table}`, `hornvale_religion::{Belief, beliefs_held_by}`).
- Every new Common sentence is corpus-law bidirectional (the Echo gate).
- Tests assert exact values/dispositions/strings, measured from live
  worlds then pinned (the C2 exact-string discipline). No is_ok()-only
  tests.

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-explanations`,
branch `the-explanations`.

**Preregistered derivation numbers (fixed here, before measurement):**

- Prior: every admitted schema starts at weight 1.0, then subsistence
  multipliers — Herding: kinship ×1.5, cycle-return ×1.3; Farming:
  cycle-return ×1.5, balance ×1.3; Fishing: substance-flow ×1.5,
  path-journey ×1.2; Foraging: agentive ×1.5, link ×1.2 — then sociality:
  Hierarchic: agentive ×1.3; Communal: balance ×1.2.
- β: `status_basis` Knowledge → 1.0, Rank → 2.0; +0.5 if `sociality` is
  Hierarchic. Sharpening: `w_i^β`, renormalized, then
  `Stream::weighted_index`. (Roster: goblin/hobgoblin 2.5, bugbear 2.0,
  kobold 1.0.)
- Manner: rank the culture's cyclic beliefs by re-derived period
  ascending — shortest → `Manner::Brisk` ("briskly"), longest →
  `Manner::Slow` ("slowly"), sole/middle → `Manner::Neutral` (no adverb).
- Fact-shapes and admissions: `CyclicEvent` (`day-length-std`) admits
  {agentive, cycle-return, path-journey, balance}; `Count` (`moon-count`)
  admits {kinship, agentive, link}. `Taxonomy`/`Roster` admit none at the
  floor.

---

### Task 1: The substrate, the schema library, and lexicalization (`domains/language/src/schemas.rs`)

Pure, kernel-only, authored build-state — the packs.rs shape (static
tables, closed sets).

**Files:**
- Create: `domains/language/src/schemas.rs`
- Modify: `domains/language/src/lib.rs` (add `pub mod schemas;` +
  re-exports, matching the existing module style)
- Modify: language's streams module (find it: `grep -rn "stream_labels" domains/language/src/`;
  add the two new label families with doc strings — follow the existing
  `(label, doc)` tuple format; labels are templates, document the
  parameterization exactly as other per-species labels do)
- Tests: in-file `#[cfg(test)]`

**Interfaces (produced — later tasks use these EXACT names):**

```rust
pub enum SourceDomain { Motion, Force, Container, Path, Balance, Link, Verticality, Kinship, Flow, Cycle }
pub enum FactShape { CyclicEvent, HighScalarState, Count, Taxonomy, Roster }
pub enum SchemaId { ForceDynamics, Agentive, SubstanceFlow, Container, PathJourney, Balance, LinkSympathy, Kinship, MoralAccounting, CycleReturn, EssenceTelos, /* 12th: */ Verticality }
pub enum SlotKind { Agent, Kin, None }
pub struct Schema { pub id: SchemaId, pub source: SourceDomain, pub shapes: &'static [FactShape], pub slot: SlotKind }
pub fn schema_table() -> &'static [Schema]              // exactly 12 rows, closed
pub fn admitted(shape: FactShape) -> Vec<SchemaId>      // reads the table
pub enum Manner { Brisk, Slow, Neutral }
pub struct LexemeId(pub &'static str)                   // the verb/frame key, closed
pub enum SubFrame { Walking, Mounted, Rowing, Stalking } // from Subsistence (caller maps)
pub fn lexemes_for(schema: SchemaId, sub: SubFrame) -> &'static [LexemeId]
/// β-sharpened weighted draw: weights^β renormalized, then stream.weighted_index.
pub fn select_schema(admitted: &[(SchemaId, f64)], beta: f64, stream: &mut Stream) -> Option<SchemaId>
pub fn select_lexeme(candidates: &'static [LexemeId], stream: &mut Stream) -> Option<LexemeId>
```

Lexeme tables (closed; the agentive/motion read): Walking →
`["walks", "strides"]`, Mounted → `["rides", "drives"]`, Rowing →
`["rows", "steers"]`, Stalking → `["walks", "stalks"]`. Agentless frames
carry no lexeme table (their surface frames are Task 4's closed strings);
`lexemes_for` on an agentless schema returns an empty slice. Kinship/link
(the Count explainers) are frame-only too at the floor.

- [ ] **Step 1: Failing tests** (write all, run, expect fail):

```rust
#[test] fn the_schema_table_is_closed_and_complete() {
    let t = schema_table();
    assert_eq!(t.len(), 12, "LANG-37's closed library");
    // every SchemaId appears exactly once; every row's shapes non-empty.
}
#[test] fn admissions_match_the_preregistered_gates() {
    assert_eq!(admitted(FactShape::CyclicEvent),
        vec![SchemaId::Agentive, SchemaId::PathJourney, SchemaId::Balance, SchemaId::CycleReturn]
        /* order = table order; adjust to the authored row order and pin EXACTLY */);
    assert_eq!(admitted(FactShape::Count),
        vec![SchemaId::Agentive, SchemaId::LinkSympathy, SchemaId::Kinship]);
    assert!(admitted(FactShape::Taxonomy).is_empty());
}
#[test] fn beta_sharpening_is_monotone_and_deterministic() {
    // same seed + same weights -> same pick, twice;
    // beta=1.0 leaves relative weights; a HIGH beta (e.g. 8.0) makes the
    // top-weighted schema win on >=95 of 100 derived streams (measure it:
    // derive 100 streams from fixed labels, count) — the monomania dial
    // demonstrably sharpens. Assert the count, not the idea.
}
#[test] fn selection_is_none_on_empty_admissions() { /* select_schema(&[], ...) == None */ }
#[test] fn lexeme_tables_are_closed_and_subframe_gated() {
    assert_eq!(lexemes_for(SchemaId::Agentive, SubFrame::Mounted).iter()
        .map(|l| l.0).collect::<Vec<_>>(), vec!["rides", "drives"]);
    assert!(lexemes_for(SchemaId::CycleReturn, SubFrame::Walking).is_empty(),
        "agentless schemas have no verb table");
}
```

- [ ] **Step 2:** `cargo test -p hornvale-language schemas 2>&1 | tail -10` → FAIL (module missing).
- [ ] **Step 3:** Implement `schemas.rs` + lib.rs wiring + the stream-label
  declarations. Module doc states: the substrate/two-reads architecture
  (LANG-38's un-collapse), closure (adding a schema is a deliberate
  library change, never a runtime path), and the anti-astrology rationale
  for the draws (C3's copula precedent).
- [ ] **Step 4:** `cargo test -p hornvale-language 2>&1 | tail -5` → all green;
  fmt; `cargo clippy -p hornvale-language --all-targets -- -D warnings`.
- [ ] **Step 5:** Commit — `feat(language): the causal-schema library + lexicalization substrate (C5 T1)`

---

### Task 2: `Disposition::Explained`, dial-blind (`domains/language/src/account.rs`)

**Files:** Modify `domains/language/src/account.rs`; tests in-file.

**Interfaces:**
- Produces: the new variant on the existing `Disposition`:

```rust
/// C5: held AS EXPLAINED — the causal filter fired. Wraps what the
/// knowledge filter said underneath; the DIAL READS THROUGH to
/// `underlying` (explanation-blindness keeps every census value
/// byte-identical — G3 ruling, ledger #3).
Explained {
    underlying: Box<Disposition>,
    schema: crate::schemas::SchemaId,
    agent: Option<String>,
    lexeme: Option<crate::schemas::LexemeId>,
    manner: crate::schemas::Manner,
},
```

- Every measure (`distortion`, `domain_distortion`, `distinctiveness`,
  `recoverability`) resolves an entry's *effective* disposition through a
  single private helper `fn effective(d: &Disposition) -> &Disposition`
  (unwraps `Explained.underlying`, recursively) — one seam, no scattered
  matches.

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn explained_is_dial_blind() {
    // Build the fixture account; wrap its day-length Lost entry in
    // Explained{underlying: that Lost, Agentive, Some("Vamu"), Some walks, Brisk}.
    // Assert distortion, domain_distortion("sky"), recoverability all
    // EXACTLY EQUAL the unwrapped account's values (== on f64, same
    // inputs), and distinctiveness(wrapped, unwrapped) == 0.0 —
    // the dial cannot see explanations AT ALL.
}
#[test] fn explained_survives_identity_never() {
    // identity_params -> account_of yields zero Explained entries
    // (account_of never constructs Explained; only the worldgen assembly
    // does — assert by matches! over the fixture account).
}
```

- [ ] **Step 2:** run → FAIL (variant missing).
- [ ] **Step 3:** Add the variant + `effective` + route all four measures
  through it. The stance/order components read the ENTRY (stance and
  position are not inside `Disposition`) — untouched.
- [ ] **Step 4:** `cargo test -p hornvale-language 2>&1 | tail -5` → all
  green INCLUDING every pre-existing C4 account test unchanged; fmt +
  clippy.
- [ ] **Step 5:** Commit — `feat(language): Disposition::Explained, dial-blind by construction (C5 T2)`

---

### Task 3: Derivation, binding, and assembly (`windows/worldgen/src/chorus.rs`)

**Files:** Modify `windows/worldgen/src/chorus.rs` (+ any new pub
re-exports in `lib.rs`'s existing chorus block only); test
`windows/worldgen/tests/explanations.rs`.

**Interfaces:**
- Consumes: T1 (`schema_table`, `admitted`, `select_schema`,
  `select_lexeme`, `lexemes_for`, `FactShape`, `Manner`, `SubFrame`), T2
  (`Disposition::Explained`), religion (`beliefs_held_by`), culture
  (`subsistence` — find how the composition root already maps a
  settlement's biome to `BiomeClass`+coastal; reuse that path), the
  existing `account_params_of`/`accounts_of`.
- Produces:

```rust
/// The per-(culture, domain) schema prior — preregistered multipliers.
pub fn schema_prior(subsistence: Subsistence, sociality: Sociality, admitted: &[SchemaId]) -> Vec<(SchemaId, f64)>
pub fn beta_of(psych: &PsychVector) -> f64
/// The period-match join: the culture's cyclic beliefs with re-derived
/// periods, sorted ascending (the manner rank), plus the day-match.
pub fn cyclic_beliefs_of(world: &World, species: &str) -> Vec<(hornvale_religion::Belief, f64)>
/// Explanation assembly: wraps qualifying entries of an account.
/// Called from accounts_of after account_of — the voice's entries gain
/// Explained wrappers where a schema fires and binds.
fn explain(world: &World, species: &str, account: &mut Account, params: &AccountParams)
```

Fact-shape source: extend worldgen's `observability_table()` rows — add
`shape: FactShape` to language's `Observability` struct (additive field;
update the C4 fixture tables in language's tests accordingly:
day-length-std → CyclicEvent, moon-count → Count, is-a/star-class →
Taxonomy, instance-of → Roster). This is a cross-crate additive struct
change — compile-fix every construction site, no semantic change to C4
behavior (all C4 tests must stay green untouched in MEANING; only fixture
struct literals gain the field).

Binding rules (the preregistered mechanics):
- Day: fires on the `day-length-std` entry iff a cyclic belief's period
  matches the committed day length within 1% relative tolerance
  (`(p - day).abs() < 0.01 * day`). Agent = that belief's deity (for
  agentive); agentless schemas bind no agent but still require ≥1 cyclic
  belief (a culture with no sky religion explains nothing — ledger #2).
- Moons: fires on a KEPT `moon-count` entry iff any cyclic belief's
  period does NOT match the day (a slower sky-cycle exists to hang the
  story on); agent = the slowest-period cyclic belief's deity.
- Manner: from the explaining belief's rank among the culture's cyclic
  beliefs (shortest Brisk, longest Slow, sole/middle Neutral).
- SubFrame from subsistence: Farming → Walking, Herding → Mounted,
  Fishing → Rowing, Foraging → Stalking.
- Streams: schema `world.seed.derive("language/<species>/schema/sky/<shape>")`,
  lexeme `world.seed.derive("language/<species>/lexeme/<predicate>")` —
  verify the derive-label idiom against how `tongue_grammar` derives its
  streams and copy it exactly.

- [ ] **Step 1: Failing tests** (`generated(seed)` helper as in
  `chorus_params.rs`):

```rust
#[test] fn beta_matches_the_preregistered_roster() { /* 2.5/2.5/2.0/1.0 exact */ }
#[test] fn the_day_binds_by_period_match_never_identity() {
    // seed 1 goblin: cyclic_beliefs_of contains a belief whose period
    // matches day-length-std within 1%; the day entry is Explained with
    // agent == that deity's committed name (compare against the ledger's
    // deity-name fact — measured then pinned exact).
}
#[test] fn unbindable_cultures_stay_plain_lost() {
    // Find or construct the no-cyclic-belief case: scan seeds 1..=10 for
    // a placed people whose pantheon has no day-matched cyclic belief;
    // if none exists, drive `explain` directly with a synthetic empty
    // belief list and assert the day entry stays Lost (not Explained).
    // EITHER path must actually execute the guard (assert on the result).
}
#[test] fn schema_competition_is_real_across_the_roster() {
    // Across seeds 1..=3, collect every culture's day-schema. Assert the
    // set of DISTINCT schemas has len >= 2 OR (if measurement shows all
    // cultures agree) assert the exact single schema and add the
    // documented note that floor priors converge — MEASURE FIRST, pin
    // what is true, never force. (The anti-vacuity law in T5 makes the
    // vacuous case loud.)
}
#[test] fn explanations_are_deterministic() { /* accounts_of twice -> identical Debug */ }
#[test] fn moons_explained_only_where_kept() {
    // seed 2 kobold: moon-count entry Explained{underlying: Kept}.
    // seed 2 goblin: moon-count entry is Lost, NOT Explained (they don't
    // see the moons well enough to owe a story about them).
}
```

- [ ] **Step 2:** run → FAIL.
- [ ] **Step 3:** Implement; doc comments carry the anonymity note
  (period-match, never source identity) and the unbindable guard.
- [ ] **Step 4:** `cargo test -p hornvale-worldgen 2>&1 | tail -5` and
  `cargo test -p hornvale-language 2>&1 | tail -5` (the Observability
  field ripple) → green; fmt + workspace clippy.
- [ ] **Step 5:** Commit — `feat(worldgen): explanation assembly — priors, β, period-match binding (C5 T3)`

---

### Task 4: The because-clause surface + corpus law + fixture verification (`windows/book`, `cli`)

**Files:** Modify `windows/book/src/lib.rs`; regenerate
`book/src/gallery/the-book.md`; tests in-file. NO regen-script changes
(the existing `hornvale book` line covers it).

**Interfaces:**
- Consumes: `ChorusVoice.account` entries now carrying `Explained`;
  T1's closed frames.
- Produces: explanation sentences as additional emic lines; `parse_chorus_line`
  extended to invert them.

Closed surface frames (EXACT strings; head clause is count-aware):

| schema | frame |
|---|---|
| Agentive | `⟨Head⟩ because ⟨Deity⟩ ⟨verb⟩ the sky⟨manner⟩.` (verb 3sg: "walks"/"rides"/…; manner: `", briskly"` / `", slowly"` / empty) |
| CycleReturn | `⟨Head⟩, as all things return.` |
| PathJourney | `⟨Head⟩ because the sky must be crossed.` |
| Balance | `⟨Head⟩ to keep the balance.` |
| Kinship | `⟨Head⟩ because ⟨they are / it is⟩ ⟨Deity⟩'s kin.` |
| LinkSympathy | `⟨Head⟩ because ⟨they answer / it answers⟩ ⟨Deity⟩.` |

Heads: day → `The day returns`; moons → `The moons cross` /
`The moon crosses` (count from the kept value).

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn explanation_lines_render_for_the_measured_seeds() {
    // Render seeds 1..=3; for each culture with an Explained day entry,
    // assert the emic section contains a line starting with "The day returns"
    // matching that entry's schema frame EXACTLY (build the expected
    // string from the account's own Explained fields — self-consistent,
    // then ALSO pin the seed-1 goblin line as a literal string once
    // measured).
}
#[test] fn every_explanation_line_round_trips() {
    // parse_chorus_line inverts each frame (closed tables); rerender
    // byte-identical. Extend the existing every_chorus_line_round_trips
    // to cover the new lines rather than duplicating the walk.
}
#[test] fn the_null_volume_is_untouched() {
    // identity chorus (voice_section with identity account) contains no
    // "because"/"as all things"/"to keep the balance" — and vol.lines
    // (the god's-eye section) is byte-identical to C4's committed values.
}
#[test] fn the_margin_still_carries_the_truth() {
    // seed 1 goblin: day explained AND the margin still contains
    // "its day lasts about 1.5 standard days" — explanation is not
    // recovery (spec §4.6).
}
```

- [ ] **Step 2:** run → FAIL.
- [ ] **Step 3:** Implement render + parse (frame tables are the inverse
  maps; deity names are free tokens validated by position; the kept
  `ParsedLine`/`ChorusDress` machinery gains an `explanation` arm).
- [ ] **Step 4:** Green + fmt + workspace clippy; then regenerate:
  `cargo run -p hornvale -- book > book/src/gallery/the-book.md`;
  `git diff book/src/gallery/the-book.md` must show ONLY added
  explanation lines (any changed pre-existing line = defect, STOP).
  Paste the seed-1 chorus section verbatim in the report.
- [ ] **Step 5: THE FIXTURE VERIFICATION (ledger #3):**
  `cargo run --release -p hornvale -- lab run studies/the-chorus.study.json`
  then `git diff --exit-code book/src/laboratory/generated/the-chorus/` —
  MUST be clean (dial-blindness holds at population scale). If it is not:
  STOP, report BLOCKED with the diff — that is a broken read-through, a
  Critical defect, never a re-pin.
- [ ] **Step 6:** Commit — `feat(book): the because-clause — explanations enter the chorus (C5 T4)`

---

### Task 5: The anti-vacuity pair, the double-dip guard, and the gate

**Files:** Create `windows/lab/tests/the_explanations.rs` (standing
laws; lab reads accounts via worldgen exactly like `the_dial.rs`); modify
`domains/language/src/schemas.rs` tests (double-dip guard); docs touches.

- [ ] **Step 1: The standing laws** (preregistered, in the file header):

```rust
//! C5's anti-vacuity pair (spec §4.5, in lieu of census metrics):
//! L1 lexeme-level: across seeds 1..=5, cultures whose SubFrames differ
//!    must not all surface the same verb lexeme (uncanny-literalism).
//!    If every culture shares one SubFrame (measure first!), the test
//!    asserts THAT finding loudly instead (a documented floor limit).
//! L2 schema-level: across seeds 1..=5, the distinct day-schema count
//!    is >= 2, OR the single winning schema is asserted exactly with the
//!    floor-convergence note. Wired to redden if a future prior change
//!    silently collapses competition.
```

Plus the dead-metaphor double-dip guard (in schemas.rs tests): the
compound-recipe concepts (`compound_recipe` in packs.rs — e.g. `sea`) are
DEAD mappings; assert the explanation pipeline never fires on a lexicon
compound (no `Explained` entry ever targets a concept-classify fact —
structurally true since only day/moon predicates admit schemas; the test
pins `admitted(FactShape::Taxonomy).is_empty()` + a doc note tying it to
LANG-38's live/dead split).

- [ ] **Step 2:** Run the laws → green (or the documented-finding arms).
- [ ] **Step 3:** Full gate: `make gate 2>&1 | tail -6` → **fully green**
  (no census window this campaign); `make census-check 2>&1 | tail -1` →
  ok (nothing census-visible changed — this run PROVES it).
- [ ] **Step 4:** Commit — `test(lab): the explanation laws — anti-vacuity pair + double-dip guard (C5 T5)`

---

## Close checklist (the closing-a-campaign skill owns these)

1. Absorb main + preflight (parallel campaigns daily).
2. **NO census regen** (G3: not spent). `make census-check` green proves
   the fixtures didn't move.
3. Chronicle + retrospective + freshness sweep (language chapter's chorus
   paragraph gains the causal filter; Study 012 unchanged — the dial is
   explanation-blind and the chapter says so if the sweep finds the
   wording implies otherwise) + LANG-37/38 → shipped + LANG-38 Where
   amendment (lexicalization-over-time → etymology engine) + followups
   promoted from the worktree register.
4. G6 hard stop: post-G3 ledger digest.

## Plan self-review (done at write time)

- Spec coverage: §3.1→T1, T2 (Explained), §3.2→T3, §3.3→T3, §3.4→T4,
  §3.5→T5 guard, §4 laws→T2/T3/T4/T5, §5 fixtures→T4 step 5, §7 flips →
  done at G3 commit / close.
- Preregistered numbers live in the header, before any measurement;
  measure-first instructions mark every place a floor value could
  surprise (schema competition, subframe spread) — pin what is true,
  never force.
- Type consistency: SchemaId/FactShape/Manner/SubFrame names match across
  tasks; `Observability.shape` ripple named in T3.
