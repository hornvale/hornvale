# The Diachronic Book (C8) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The Book's time axis — the observation ledger at T, MAP-18's
ladder climbed by witnessed eclipse counts, the prophecy law, and The
Reckoning of Years section with `book --at <day>`.

**Architecture:** Pure derivation in worldgen (`observations_of` +
`ladder_of` over `eclipse_events`); the additive section + `--at` in
`windows/book`/`cli`. Zero draws, zero facts, additive artifact. Spec:
`docs/superpowers/specs/2026-07-19-the-diachronic-book-design.md`
(G3-approved).

**Tech Stack:** Rust edition 2024, std + serde only.

## Global Constraints

- **Zero new draws/streams/facts/concepts/metrics. No epoch. Genesis
  byte-identical. The committed artifact delta is ADDITIVE ONLY** (every
  pre-C8 section byte-identical — the additivity law).
- Census stability verified at the surface task (chorus fixture +
  `make census-check` + the full artifact sweep).
- Every new Common sentence corpus-law bidirectional; tongues absent
  from the section.
- Fallback/vacuous arms PANIC; measure-then-pin; live files are the
  authority; no HashMap/HashSet; missing_docs; type-audit tags (+ report
  regen if tags added); fmt every commit. **State-changing commands get
  their own Bash calls** (the C6 ledger scar).

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-diachronic-book`,
branch `the-diachronic-book`.

**Preregistered (frozen before measurement):**

- Epoch pair: `{StdDays 0.0, StdDays 36525.0}` (day 0, the hundredth
  year). Ladder thresholds: `K_COUNT = 3` witnessed events → `Numbered`
  (organized cults only); `K_PREDICT = 8` witnessed events of ONE
  recurrence class → `Predictive`. Recurrence class = `(moon index,
  EclipseBody)` — the floor's honest class (no saros-search dependency;
  the prediction is the closed-form next event of the most-observed
  class, and the "computation" is narratively the priesthood's records,
  mechanically the exact model).
- Witnessing: `EclipseBody::Solar` (verify the enum's variant names
  live) → public, every placed culture witnesses; lunar → witnessed iff
  folk `sky_capability >= 0.6` (the C4 threshold, read from the
  culture's `AccountParams`).
- Folk (no records): `Counted` at n ≥ 1, never higher. Organized cults
  climb the full ladder on the culture's witnessed set.
- Surfaces (closed strings; count-aware where noted):
  - Epoch headings inside the section: `In the first days` / `In the
    hundredth year`.
  - Empty arm: `The sky keeps no dates to number.` (rendered once per
    epoch with zero witnessed events across all cultures, and for
    eventless worlds at both epochs).
  - Folk counted line: `The sky has darkened, now and again.` (per
    culture at `Counted`+; suppressed at `Unknown`).
  - Numbered line: `The priesthood of the ⟨Autonym⟩ numbers the
    darkenings: ⟨cardinal⟩.`
  - Prediction line: `The next darkening, it teaches, comes on day
    ⟨integer day⟩.` (only at `Predictive`, immediately after that
    culture's Numbered line).
  - Truth margin (italic, per epoch, fires when any culture's knowledge
    falls short of the true count): `In truth, the darkenings of the
    first hundred years number ⟨cardinal⟩.` (epoch-2 wording; epoch 1
    uses `of the first days` — but epoch 1 has zero events, so it fires
    only on the pathological never-expected case; keep the arm and let
    the corpus test exercise the string synthetically).

---

### Task 1: The observation ledger and the ladder (`windows/worldgen`)

**Files:**
- Modify: `windows/worldgen/src/chorus.rs` (the diachronic derivation
  block; re-exports in lib.rs's chorus cluster)
- Test: `windows/worldgen/tests/diachronic.rs`

**Interfaces (produced):**

```rust
/// One culture's witnessed eclipse record by day `at` — pure, derived.
pub struct Observations {
    /// Witnessed events (day, moon, body), ascending by day.
    pub events: Vec<(f64, usize, hornvale_astronomy::EclipseBody)>,
}
pub fn observations_of(world: &World, species: &str, at: hornvale_astronomy::StdDays) -> Result<Observations, BuildError>
pub enum LadderRung { Unknown, Counted, Numbered, Predictive }
/// The rung + (for Predictive) the predicted next event's day.
pub fn ladder_of(world: &World, species: &str, at: hornvale_astronomy::StdDays) -> Result<(LadderRung, Option<f64>), BuildError>
```

Build the event list via `eclipse_events(&system, &calendar, from, until)`
— find how a `FullView`/worldgen reconstructs `system`/`calendar` (the
almanac and the cadence metrics both do; reuse that path). The organized
gate reuses `doctrine_of(world, species).is_some()`. The prediction: scan
`eclipse_events(at, at + horizon)` for the next event of the
most-observed class (horizon: one Metonic-generous window, 10_000.0 std
days, documented; if no event in the horizon, `Predictive` still holds
but the prediction is `None` — the priesthood knows the count, the next
event is beyond its teaching horizon; the surface then omits the
prediction line — an honest arm, asserted).

- [ ] **Step 1: Failing tests** (`generated(seed)` helper as in sibling
  test files):

```rust
#[test] fn observations_at_day_zero_are_empty() { /* every placed culture, seeds 1..=3 */ }
#[test] fn the_accumulation_law() {
    // per culture: |observations(T1)| <= |observations(T2)| for T1 < T2
    // over {0, 10_000, 36_525}; and the T2 set prefixes... (events
    // ascending; the earlier set is a prefix of the later — assert).
}
#[test] fn the_witness_law() {
    // Solar counts equal across cultures of one world regardless of
    // sky-capability (compare a low-cap and high-cap culture's solar
    // subsets — equal); lunar subsets differ exactly when capability
    // straddles 0.6 (seed 2 goblin 0.5 vs kobold 1.0 — goblin's lunar
    // set empty, kobold's not, IF seed 2 has lunar events by 36525 —
    // MEASURE; if no measured seed has a lunar event, drive the witness
    // filter synthetically and PANIC-demand a wider sweep in the live
    // arm).
}
#[test] fn the_ladder_law() {
    // Exact rungs per (seed 1..=5, culture) at both epochs — measure
    // then pin the full table (the landscape idiom from C7). Folk-only
    // cultures (folk-flagship, e.g. seed-3 hobgoblin post-Confluence)
    // never exceed Counted — assert from the pinned table AND
    // structurally (no Numbered without doctrine_of).
}
#[test] fn the_prophecy_law() {
    // Every Predictive culture's predicted day D: eclipse_events(at,
    // at+horizon) contains an event of the predicted class at exactly D
    // (quantize-at-emit tolerance: compare the f64s the API returns
    // directly — same computation, no epsilon). PANIC if NO culture in
    // seeds 1..=5 is Predictive at epoch 2 (the ladder's top must be
    // visible — the preregistered demand).
}
#[test] fn diachronic_is_deterministic() { /* twice -> identical Debug */ }
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement (doc comments carry
  the derived-not-stored posture and the records-vs-memory split).
- [ ] **Step 4:** worldgen + book + language green; fmt; workspace
  clippy; type-audit (+ report if tags).
- [ ] **Step 5:** Commit —
  `feat(worldgen): the observation ledger and the knowledge ladder (C8 T1)`

---

### Task 2: The Reckoning of Years + `--at` + the gate (`windows/book`, `cli`)

**Files:**
- Modify: `windows/book/src/lib.rs` (the section + parse arms),
  `cli/src/main.rs` (`--at` + the committed section), regenerate
  `book/src/gallery/the-book.md`; tests in-file.

**Interfaces:**
- Consumes: T1's `observations_of`/`ladder_of`/`LadderRung`; the
  existing section machinery + `cardinal`.
- Produces: `BookVolume` gains
  `pub reckoning: Vec<ReckoningEpoch>` where
  `pub struct ReckoningEpoch { pub heading: String, pub lines: Vec<String>, pub margin: Vec<String> }`
  (two epochs, the preregistered pair); `parse_chorus_line` arms for the
  four new sentence shapes (closed strings; deity-free — pure table
  inversion).

Rendering per epoch: the empty arm when zero events across cultures;
else per culture in placed order: the folk counted line (Counted+), the
Numbered line (with autonym), the prediction line (Predictive with a
predicted day). The truth margin per epoch when any culture's held count
< the true count (the true count = all events to T, unwitnessed
included).

`--at <day>`: parse as f64 std days; render the volumes with a single
`ReckoningEpoch` at that day (stdout only; the committed artifact always
uses the fixed pair — assert `regenerate-artifacts.sh` passes no
`--at`).

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn the_reckoning_renders_the_epoch_pair() {
    // seeds 1..=3: two epochs; epoch 1 == the empty arm (day 0);
    // epoch 2's lines match the pinned ladder table (T1) — pin the
    // exact seed-1 strings after measuring.
}
#[test] fn the_additivity_law() {
    // Every pre-C8 line of the committed artifact byte-identical: pin
    // the seed-1 chorus/tongue/doctrine section vectors against their
    // current committed literals (the C6/C7 idiom).
}
#[test] fn every_reckoning_line_round_trips() { /* corpus-law walk extended */ }
#[test] fn the_margin_fires_exactly_when_knowledge_falls_short() {
    // A culture at Numbered holds the true count -> no margin from it;
    // folk-only cultures always fall short (their count is qualitative)
    // -> the margin fires on every eventful world with a folk-only or
    // sub-threshold culture. Assert per the pinned table; both arms.
}
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement + `cmd_book` emits
  the section (`### The Reckoning of Years`, `#### ⟨epoch heading⟩`).
- [ ] **Step 4:** regenerate the artifact; `git diff` = additive only
  (any changed pre-existing line = BLOCKED); paste seed-1's section.
- [ ] **Step 5:** Census stability + the full sweep:
  `make census-check` → ok; `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh`
  then `git status --porcelain` empty (post-commit).
- [ ] **Step 6:** `make gate 2>&1 | tail -4` fully green; `mdbook build
  book` clean.
- [ ] **Step 7:** Commit —
  `feat(book,cli): The Reckoning of Years — the Book gains its time axis (C8 T2)`

---

## Close checklist (closing-a-campaign owns these)

1. Absorb main + preflight (the neighborhood is BUSY — expect movement).
2. NO census regen. 3. Chronicle + retro + freshness (language chapter;
LANG-42 partial flip; **the metaplan's §3 marked complete C1–C8, with
UNI-29 named as the horizon**; the vessel `--at` seam followup recorded).
4. G6 hard stop.

## Plan self-review (done at write time)

- Spec §3.1→T1, §3.2→T1, §3.3→T1 (prophecy) + T2 (surface), §3.4→T2,
  §4 laws 1–3+6→T1, 4–5+7→T2. Placeholders: none (all strings and
  thresholds frozen above). Type consistency: Observations/LadderRung/
  ReckoningEpoch consistent across tasks.
- The no-prediction-within-horizon arm and the no-lunar-event measured
  arm both carry explicit honest handling (omit + assert; synthetic +
  panic-demand).
