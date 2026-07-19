# The Doctrine (C6) Implementation Plan

> **COMPLETE (2026-07-19).** 4 tasks + review fixes; census untouched (roster law proven 3x); ledger #9's explanatory-parity refinement supersedes this plan's compressed Harmony grid (the spec's letter governs).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The institution's second account — derived from the folk account
by four preregistered deltas — with the four-state conflict map, the
disclosure rule, and the exoteric/esoteric editions of the Book.

**Architecture:** Mediation column + conflict classifier in
`domains/language` (pure); `doctrine_params_of`/`doctrines_of` in
worldgen's `chorus.rs` (flagship cult-form gate via `flagship_of` +
`cult_form_held_by`); doctrine sections + annotations + reader-gated
esoteric lines in `windows/book`; `--initiate` in `cli`. Spec:
`docs/superpowers/specs/2026-07-19-the-doctrine-design.md` (G3-approved).

**Tech Stack:** Rust edition 2024, std + serde only.

## Global Constraints

- **No epoch. Zero new facts/concepts/predicates** (`cult-form` is
  committed state). The ONLY new draws: two render-time label families
  `language/<species>/doctrine-schema/<domain>/<fact-shape>` and
  `language/<species>/doctrine-lexeme/<fact-key>` — declared in
  language's streams module **and the stream-manifest reference page
  regenerated in the SAME task**
  (`cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest-generated.md`)
  — C5's F1 lesson.
- **The dial-roster law (census keystone):** `accounts_of` returns
  exactly the C4 folk voices — `DoctrineVoice` lives ONLY in
  `doctrines_of`. Proven structurally AND by the 50-seed chorus-study
  fixture byte-identity check (Task 3; a dirty diff is BLOCKED-Critical,
  never a re-pin). NO new lab metrics.
- **Selection bias, never deception:** doctrine params = folk params +
  exactly the four preregistered deltas below; no theory of mind; nothing
  authored per culture. The folk voice's params/account are BYTE-UNCHANGED
  by this campaign (the selection-bias law asserts field equality).
- Anonymity holds: the high-god preference selects among the culture's
  own beliefs; period-match remains the fallback; no source identities.
- No HashMap/HashSet; missing_docs; type-audit tags at introduction;
  fmt before every commit; live files are the authority; exact-value
  tests (measure then pin); every new Common sentence corpus-law
  bidirectional; measure-first arms whose FALLBACK PANICS (the C5 F2
  lesson — a fallback arm that merely asserts the fallback state is a
  plan failure).

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-doctrine`,
branch `the-doctrine`.

**Preregistered numbers (frozen before measurement):**

- Capability delta: doctrine sky-capability = min(folk + 0.25, 1.0).
- β delta: doctrine β = folk β + 0.5.
- Mediation column (authored, global, closed — one value per schema row):
  Agentive 1.5, MoralAccounting 1.5, EssenceTelos 1.4, LinkSympathy 1.2,
  Kinship 1.0, Container 1.0, ForceDynamics 1.0, SubstanceFlow 1.0,
  Verticality 1.0, Balance 0.8, PathJourney 0.6, CycleReturn 0.6.
  Doctrine prior = folk prior × mediation (before β sharpening).
- Agent preference: if the culture's high-god belief is cyclic AND its
  period is compatible with the fact (day: the 1% period-match; moons:
  any non-day-matched cyclic), bind it; else the folk binding rule.
- Folk-verifiability per observability Requirement: `Manifest` →
  verifiable; `SkyGraded{t}` → folk sky-capability ≥ t; `Instrumental` /
  `CrossReferential` / `Taxonomic` → not verifiable.
- Conflict states (language, pure): `Harmony` (effective dispositions
  equal AND (if both Explained) schemas equal), `Contested` (differ ∧
  verifiable), `Mystery` (differ ∧ ¬verifiable), `RevealedClaim`
  (doctrine effective ∈ {Kept, Explained{Kept}} ∧ folk effective is
  Lost).
- Surfaces (closed strings; heads count-aware as in C5):
  - Doctrine heading: `As the priesthood of the ⟨Autonym⟩ teach it`
  - RevealedClaim exoteric: `The moons are counted and known to the
    priesthood.` / singular `The moon is counted and known to the
    priesthood.`
  - Contested counter-annotation: `— though the folk say ⟨folk sentence
    minus its terminal period⟩.`
  - Esoteric (initiated) line: `— ⟨cardinal⟩, as the initiated count.`

---

### Task 1: Mediation column + conflict classifier (`domains/language`)

**Files:**
- Modify: `domains/language/src/schemas.rs` (add `pub mediation: f64` to
  `Schema` + the preregistered values; move `LinkSympathy` to
  `SlotKind::Agent` — the C5 followup, behavior-neutral, and fix the
  enum/table docs to match; add `pub fn conflict_of(...)` — see below)
- Modify: language streams module (the two doctrine label families)
- Modify: `book/src/reference/stream-manifest-generated.md` (regenerate —
  SAME task)
- Tests: in-file

**Interfaces (produced):**

```rust
// On Schema: pub mediation: f64   (type-audit: bare-ok(ratio))
pub enum ConflictState { Harmony, Contested, Mystery, RevealedClaim }
/// Pure classifier per the preregistered table. `folk_verifiable` is the
/// caller-derived flag (worldgen owns the observability row).
pub fn conflict_of(folk: &Disposition, doctrine: &Disposition, folk_verifiable: bool) -> ConflictState
```

`conflict_of` compares EFFECTIVE dispositions (reuse `account::effective`
— make it `pub(crate)` if needed, or route through a small pub helper;
verify the live visibility) and, when both are `Explained`, their
schemas.

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn mediation_values_match_the_preregistered_column() {
    // walk schema_table(), assert each row's mediation EXACTLY per the
    // plan header (12 assertions, no tolerance).
}
#[test] fn link_sympathy_is_agent_bearing_in_the_table() {
    // SlotKind::Agent on the LinkSympathy row (the C5 followup landed);
    // Kinship stays Kin; CycleReturn/PathJourney/Balance stay None.
}
#[test] fn conflict_states_cover_the_preregistered_grid() {
    // Harmony: identical Kept vs Kept; identical Explained schemas.
    // Contested: Explained(PathJourney) vs Explained(Agentive), verifiable=true.
    // Mystery: same pair, verifiable=false.
    // RevealedClaim: folk Lost vs doctrine Kept; folk Lost vs doctrine
    //   Explained{underlying: Kept}.
    // Explained-wrapping transparency: Explained{Kept} vs Kept with same
    //   schema irrelevant -> compares EFFECTIVE (Harmony).
}
```

- [ ] **Step 2:** `cargo test -p hornvale-language 2>&1 | tail -5` → FAIL.
- [ ] **Step 3:** Implement (mediation field ripples to every
  `Schema`-literal construction site in tests; the streams module gains
  the two families; regenerate the manifest page).
- [ ] **Step 4:** language tests green; **worldgen compiles AND its
  `agent_bearing` exception still passes its tests** (do NOT touch
  worldgen here — Task 2 deletes the exception); fmt; clippy for the
  crate; type-audit check.
- [ ] **Step 5:** Commit —
  `feat(language): mediation column + conflict classifier; LinkSympathy slot honest (C6 T1)`

---

### Task 2: The doctrine stack (`windows/worldgen/src/chorus.rs`)

**Files:**
- Modify: `windows/worldgen/src/chorus.rs`
- Test: `windows/worldgen/tests/doctrine.rs`

**Interfaces:**
- Consumes: T1 (`mediation`, `ConflictState`, `conflict_of`,
  LinkSympathy now `SlotKind::Agent`); existing
  `flagship_of(world, species) -> Option<VillageInfo>`,
  `hornvale_religion::cult_form_held_by(world, community: EntityId)`,
  `beliefs_held_by`, the C5 explain machinery.
- Produces:

```rust
/// The institution's voice: the culture, its doctrine params, and the
/// doctrine account (folk ground through the doctrine stack).
pub struct DoctrineVoice { pub kind: String, pub params: AccountParams, pub account: Account }
/// Some iff the flagship's committed cult-form is "organized" (the SOC-1 gate).
pub fn doctrine_of(world: &World, species: &str) -> Option<DoctrineVoice>
/// Every organized culture's doctrine voice, placed_peoples order.
pub fn doctrines_of(world: &World) -> Vec<DoctrineVoice>
/// The four preregistered deltas applied to folk params (pure).
pub fn doctrine_params_of(folk: &AccountParams, /* … */) -> AccountParams
/// Per-fact folk-verifiability from the folk params + observability row.
pub fn folk_verifiable(folk: &AccountParams, predicate: &str) -> bool
```

The doctrine `explain` pass reuses C5's machinery with: the doctrine
label families, the mediation-reweighted prior, doctrine β, and the
high-god agent preference. `agent_bearing`'s LinkSympathy exception is
DELETED (T1 made the table honest) — the slot check reads
`schema_table()` alone now.

- [ ] **Step 1: Failing tests** (`windows/worldgen/tests/doctrine.rs`):

```rust
#[test] fn the_soc1_gate_is_the_flagship_cult_form() {
    // seed 1: every placed culture's flagship is organized (measured) ->
    // doctrine_of is Some for each; doctrines_of len == placed_peoples len.
    // Negative arm: sweep seeds 1..=10 for any species whose flagship
    // cult-form is "folk" -> doctrine_of is None. If the sweep finds NONE,
    // PANIC with a message demanding a synthetic-society unit test be
    // added instead (measure first; the fallback arm fails loudly —
    // C5's F2 lesson) — and add that synthetic test.
}
#[test] fn the_selection_bias_law_field_by_field() {
    // seed 1 goblin: doctrine params vs folk params — sky_capability ==
    // min(folk+0.25, 1.0); (beta is not a params field: assert
    // doctrine_beta_of == beta_of + 0.5 at the fn level); order, stances,
    // holdings, hold_all, world_carving, observability ALL EQUAL folk's
    // (assert_eq field by field). No hidden divergence.
}
#[test] fn doctrine_keeps_what_folk_lose() {
    // seed 1 goblin (folk cap 0.5, doctrine 0.75 >= 0.6): moon-count
    // effective disposition Kept in the doctrine account, Lost in folk.
}
#[test] fn the_high_god_takes_the_day_where_compatible() {
    // Measure seed 1 goblin: if the high-god belief is the day-matched
    // cyclic one, doctrine day agent == high god AND folk agent == same
    // (assert the preference rule's outcome, whichever branch is true —
    // pin the measured deity names exactly).
}
#[test] fn doctrine_voices_never_enter_the_dial_roster() {
    // accounts_of(seed 1) returns exactly placed_peoples-many folk voices;
    // type-level: DoctrineVoice is a distinct type; value-level: the
    // Debug of accounts_of's output is BYTE-IDENTICAL to its output at
    // the pre-C6 commit? Cannot diff across commits in a test — instead:
    // assert accounts_of output contains zero doctrine-labeled streams
    // by re-deriving a folk voice's Debug twice (determinism) and
    // asserting the folk params show NO +0.25 capability (folk goblin
    // sky_capability == 0.5 exactly, unchanged).
}
#[test] fn doctrine_is_deterministic() { /* doctrines_of twice -> identical Debug */ }
```

- [ ] **Step 2:** run → FAIL.
- [ ] **Step 3:** Implement; doc comments carry the selection-bias-not-
  deception note and the SOC-1 gate.
- [ ] **Step 4:** worldgen + language + book tests green (book untouched
  but consumes accounts_of — must stay green unchanged); fmt; workspace
  clippy; type-audit.
- [ ] **Step 5:** Commit — `feat(worldgen): the doctrine stack — four deltas, SOC-1 gate, separate roster (C6 T2)`

---

### Task 3: Doctrine sections + annotations + fixture proof (`windows/book`, artifact)

**Files:**
- Modify: `windows/book/src/lib.rs`; regenerate
  `book/src/gallery/the-book.md`; tests in-file.

**Interfaces:**
- Consumes: `doctrines_of`, `ConflictState`, `conflict_of`,
  `folk_verifiable`; the C5 render/parse machinery.
- Produces: `ChorusSection` gains `pub doctrine: Option<DoctrineSection>`
  where `pub struct DoctrineSection { pub heading: String, pub emic:
  Vec<String>, pub annotations: Vec<String>, pub margin: Vec<String> }`;
  `parse_chorus_line` arms for the counter-annotation and the
  revealed-claim formula.

Rendering: the doctrine paragraph renders the doctrine account through
the SAME `voice_section`-style machinery (fresh referring scope), with
per-entry substitutions: a `RevealedClaim` entry renders the exoteric
formula INSTEAD of the value-bearing fragment; after the emic lines,
`Contested` entries emit `— though the folk say ⟨the folk line for that
entry, sans terminal period⟩.`; then the doctrine account's own truth
margin (existing machinery). Folk sections byte-unchanged.

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn seed_1_doctrine_sections_render() {
    // Every seed-1 culture (all organized) gains a doctrine section;
    // goblin's heading == "As the priesthood of the Vavako teach it";
    // its emic contains the RevealedClaim formula for the moons
    // ("The moons are counted and known to the priesthood.") and a day
    // explanation whose agent is the measured doctrine deity — pin the
    // exact strings after measuring.
}
#[test] fn the_disclosure_law_both_directions() {
    // Across seeds 1..=5: every Contested entry's section carries its
    // counter-annotation; every Mystery entry's section carries NONE.
    // If NO Contested entry exists across those seeds (measure!), PANIC
    // demanding a synthetic Contested unit test — then drive the
    // annotation renderer directly with a synthetic folk/doctrine pair
    // (kobold-style: folk keeps moons + differing schemas) and assert
    // the counter-line's exact text.
}
#[test] fn folk_sections_are_byte_unchanged() {
    // Pin seed-1 folk emic/margin vectors against their pre-C6 committed
    // strings (copy the artifact's current lines as literals) — the
    // doctrine addition may not perturb one byte of the folk rendering.
}
#[test] fn every_doctrine_line_round_trips() {
    // Extend the existing corpus-law walk to doctrine emic + annotations;
    // the counter-annotation inverts by stripping the prefix and
    // re-parsing the embedded folk sentence; the revealed-claim formula
    // inverts to (moon-count, KnowledgeClaim) — closed tables.
}
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement. **Step 4:** green +
  fmt + workspace clippy + type-audit; regenerate the artifact
  (`cargo run -p hornvale -- book > book/src/gallery/the-book.md`), diff
  additive-only (any changed pre-existing line = BLOCKED), paste seed-1's
  full chorus verbatim in the report.
- [ ] **Step 5: The roster-law population proof:**
  `cargo run --release -p hornvale -- lab run studies/the-chorus.study.json`
  then `git diff --exit-code book/src/laboratory/generated/the-chorus/` —
  MUST exit 0 (dirty = BLOCKED-Critical, never re-pin).
- [ ] **Step 6:** Commit — `feat(book): the doctrine sections — disclosure, mystery, revealed claims (C6 T3)`

---

### Task 4: The esoteric edition + `--initiate` + the gate

**Files:**
- Modify: `windows/book/src/lib.rs` (`esoteric_lines`), `cli/src/main.rs`
  (`book --initiate`); tests in-file + `windows/lab/tests/the_doctrine.rs`
  (the standing laws file).

**Interfaces:**

```rust
/// The initiated lines for one world: for each RevealedClaim entry whose
/// (subject, predicate) is in `reader`, the value line. Empty reader ->
/// empty vec (the committed exoteric edition).
pub fn esoteric_lines(world: &World, reader: &BTreeSet<(String, String)>) -> Vec<String>
```

CLI: `hornvale book --initiate` renders the volumes WITH each world's
full fact-set as the reader (the omniscient initiate) to stdout —
compare-only, never redirected into the gallery (assert the committed
artifact is regenerated by the PLAIN `book` path only; grep
regenerate-artifacts.sh to confirm no `--initiate` there).

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn the_esoteric_law_mutation_verified() {
    // Empty reader -> no initiated lines AND the exoteric formula
    // present (seed 1). Reader containing ("Vebe", "moon-count") ->
    // exactly one initiated line "— two, as the initiated count." whose
    // number comes from the LEDGER (mutation arm: pass a reader set with
    // the key but assert against the committed fact value — then verify
    // the mechanism by asserting a WRONG expected value fails: use a
    // second assertion on a synthetic world/entry if driving the helper
    // directly is cleaner. The line's value must equal the ledger's,
    // never any other source.)
}
#[test] fn initiate_edition_supersets_the_committed_artifact() {
    // Render seed 1 plain and with the omniscient reader: the initiated
    // output contains every committed line PLUS the esoteric lines and
    // nothing else differs (set/sequence comparison).
}
```

Standing laws (`windows/lab/tests/the_doctrine.rs`, header preregistered):
the SOC-1 gate law + the dial-roster law restated at the lab level
(accounts_of folk-param stability: goblin sky-capability 0.5 exactly,
seeds 1..=5) — the file exists so a future campaign touching the roster
reddens OUTSIDE worldgen's own tests too.

- [ ] **Step 2:** run → FAIL. **Step 3:** implement.
- [ ] **Step 4:** Full gates, in order, pasting results:
  `make gate 2>&1 | tail -4` (fully green);
  `make census-check 2>&1 | tail -1` (ok);
  `mdbook build book 2>&1 | tail -1`;
  `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` then
  `git status --porcelain` — EMPTY (the C5 F1 lesson: the full artifact
  sweep, not just the pieces this plan names).
- [ ] **Step 5:** Commit — `feat(book,cli): the esoteric edition — reader-gated initiated lines (C6 T4)`

---

## Close checklist (closing-a-campaign owns these)

1. Absorb main + preflight. 2. NO census regen (census-check green is the
proof). 3. Chronicle + retro + freshness (language chapter gains the
doctrine paragraph; the registry: LANG-39 → shipped; LANG-34 pointer
update; the vessel-unlock followup row). 4. G6 hard stop.

## Plan self-review (done at write time)

- Spec §3.1→T2, §3.2→T1, §3.3→T3, §3.4→T4, §4 laws→T2/T3/T4, §5
  determinism→T1 (manifest same-task) + T3 step 5 + T4 step 4.
- Fallback arms PANIC (SOC-1 negative sweep, Contested sweep) — the C5 F2
  lesson is encoded, with synthetic-unit fallbacks demanded explicitly.
- Type consistency: DoctrineVoice/DoctrineSection/ConflictState names
  match across tasks; `mediation` ripples named in T1.
