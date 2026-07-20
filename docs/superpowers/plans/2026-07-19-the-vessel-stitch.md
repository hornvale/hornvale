# The Vessel Stitch Implementation Plan

> **COMPLETE (2026-07-19).** 2 tasks; the mutation arm structural; zero artifacts moved; the cheapest campaign of the year — the program's named seams became stitches.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The `consult` verb — the possessed session reads its world's
Reckoning at its own day and the initiated lines its Knowledge unlocks —
plus the `tell` → `write` rename ("Written in the margin.").

**Architecture:** T1 exposes the C8 single-epoch reckoning as an additive
book accessor; T2 renames the Echo's verb, adds the Knowledge→reader-set
adapter and the `consult` verb, and runs the vessel-check + full gate.
Spec: `docs/superpowers/specs/2026-07-19-the-vessel-stitch-design.md`
(G3-approved as amended).

**Tech Stack:** Rust edition 2024, std + serde only.

## Global Constraints

- **Zero draws/facts/concepts/metrics; no epoch; NO committed-artifact
  change of any kind** (the Book gallery page byte-identical — `consult`
  is a live rendering; verify with the full artifact sweep at T2).
- Session-state only; the purity law (the ledger byte-identical across
  `consult`).
- The rename is TOTAL: no `tell` alias; every test/doc pinning `tell`
  re-pins in T2 with provenance; the response line is exactly
  `Written in the margin.`
- Surfaces (closed strings): consult heading `The Reckoning, at day
  ⟨integer⟩.` (the session's day, truncated like C8's prediction days);
  fallback `The Book holds more for the initiated.`
- Live files are the authority; measure-then-pin; fallback arms panic;
  no HashMap/HashSet; missing_docs; type-audit tags; fmt every commit;
  state-changing commands in their own Bash calls.

**Worktree:** `~/.config/superpowers/worktrees/hornvale/the-vessel-stitch`,
branch `the-vessel-stitch`.

---

### Task 1: The book accessor (`windows/book`)

**Files:** Modify `windows/book/src/lib.rs` (and `cli/src/main.rs` only
if the cutover requires it); tests in-file.

**Interfaces:**
- Produces: `pub fn reckoning_at(world: &World, day: hornvale_astronomy::StdDays) -> ReckoningEpoch`
  — the C8 `--at` single-epoch rendering, factored/exposed. Find the live
  seam: `reckoning_epochs(world)` (private, the fixed pair) and the CLI's
  `--at` path (cli/src/main.rs ~344, ~705) — whichever function the CLI
  path calls per-day becomes (or wraps into) `reckoning_at`,
  behavior-identical, with the CLI cut over to the pub fn. The epoch
  heading for an arbitrary day is whatever the CLI path renders today
  (`At day ⟨N⟩` — verify live and keep EXACTLY; T2's consult heading is
  the VESSEL's own line, not this one).

- [ ] **Step 1: Failing test:**

```rust
/// The accessor law (spec §4.4): reckoning_at is the CLI --at path's own
/// implementation — one function, two callers. Compare against the fixed
/// pair for the preregistered days (identity with reckoning_epochs) and
/// against a fresh arbitrary day for shape.
#[test]
fn reckoning_at_matches_the_fixed_pair_and_renders_arbitrary_days() {
    let world = generated(1);
    let pair = render_volume(&world).reckoning;
    assert_eq!(reckoning_at(&world, hornvale_astronomy::StdDays::new(0.0).unwrap()).lines, pair[0].lines);
    assert_eq!(reckoning_at(&world, hornvale_astronomy::StdDays::new(36525.0).unwrap()).lines, pair[1].lines);
    let mid = reckoning_at(&world, hornvale_astronomy::StdDays::new(20000.0).unwrap());
    assert!(!mid.heading.is_empty() && !mid.lines.is_empty(), "an arbitrary day renders: {mid:?}");
}
```

(Adjust field comparisons to `ReckoningEpoch`'s real shape — headings for
the fixed pair differ from an arbitrary day's by design; compare `lines`
and `margin`, not headings, for the pair-identity arms. Verify live.)

- [ ] **Step 2:** run → FAIL. **Step 3:** factor/expose; cut the CLI
  over; doc comment cites the accessor law + the vessel consumer.
- [ ] **Step 4:** `cargo test -p hornvale-book 2>&1 | tail -3` green
  (all pre-existing tests unchanged); `cargo test -p hornvale --bin
  hornvale 2>&1 | tail -3` (CLI tests) green; fmt; clippy for both
  crates; type-audit.
- [ ] **Step 5:** Commit —
  `feat(book): reckoning_at — the single-epoch accessor, CLI cut over (Vessel Stitch T1)`

---

### Task 2: `write`, the adapter, `consult`, and the gate (`windows/vessel`)

**Files:** Modify `windows/vessel/src/session.rs`,
`windows/vessel/src/knowledge.rs`; tests in the crate's established
locations (find where the `tell` tests live — likely
`windows/vessel/tests/` + in-file).

**Interfaces:**
- Consumes: T1's `reckoning_at`; existing `esoteric_lines(world,
  &BTreeSet<(String, String)>)`, `Knowledge`, `absorb_common`, the
  session's `day: WorldTime`, the verb dispatch match.
- Produces:

```rust
/// Knowledge keys ("subject::predicate") split into the C6 reader-set.
/// Keys without the separator are skipped — and there must be none today
/// (asserted by test; the absorb path always writes the separator).
pub fn reader_set(knowledge: &Knowledge) -> BTreeSet<(String, String)>
// session verbs: "write" (renamed from "tell"; response "Written in the
// margin."), "consult" (no arguments at the floor).
```

`consult` assembly: the heading `The Reckoning, at day ⟨self.day.day
truncated to integer⟩.`, then `reckoning_at(world, day).lines` (+ its
margin lines, italic-free in-session — the vessel is plain text; render
margins prefixed `In truth, …` exactly as the epoch's margin lines read),
then `esoteric_lines(world, &reader_set(&self.knowledge))` — or the
fallback line when that vec is empty.

- [ ] **Step 1: Failing tests:**

```rust
#[test] fn write_is_the_verb_and_the_margin_answers() {
    // A possessed session: "write Vebe is a planet with two moons" ->
    // "Written in the margin."; "tell ..." -> the unknown-verb response
    // (the rename is total). Re-pin every existing tell test with
    // provenance comments (grep the crate for "tell").
}
#[test] fn the_stitch_law_end_to_end() {
    // Fresh session (seed 1): consult -> contains the fallback line and
    // the day-0 reckoning line ("The sky keeps no dates to number.").
    // write the moon sentence -> consult now contains "— two, as the
    // initiated count." AND NOT the fallback.
    // MUTATION ARM: write "...with nine moons" in a fresh session ->
    // consult still renders "— two, ..." (the LEDGER's value, never the
    // heard one) — the heard≠true law, printed.
}
#[test] fn the_day_law() {
    // consult at day 0 -> "The Reckoning, at day 0."; wait enough days
    // (drive the session's wait verb; verify its argument shape live) ->
    // consult's heading day advanced to match self.day exactly.
}
#[test] fn the_purity_law() {
    // Ledger Debug (or fact count + a full serialize) identical before
    // and after consult; Knowledge unchanged by consult too.
}
#[test] fn reader_set_splits_every_key() {
    // absorb a line, then: every key splits on "::" into a 2-tuple;
    // the skipped-key arm is unreachable today (assert none skipped).
}
```

- [ ] **Step 2:** run → FAIL. **Step 3:** implement (rename first —
  smallest diff — then the adapter, then consult).
- [ ] **Step 4:** vessel + book + cli green; fmt; workspace clippy;
  type-audit.
- [ ] **Step 5:** `make vessel-check 2>&1 | tail -3` → green (the
  Casement's whole obligation).
- [ ] **Step 6:** The quiet-posture proof:
  `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` then
  `git status --porcelain` → EMPTY (no committed artifact moved);
  `make census-check 2>&1 | tail -1` → ok; `make gate 2>&1 | tail -4` →
  fully green; `mdbook build book` clean.
- [ ] **Step 7:** Commit —
  `feat(vessel): write and consult — the played game meets the Book (Vessel Stitch T2)`

---

## Close checklist (closing-a-campaign owns these)

1. Absorb main + preflight. 2. No census anything. 3. Chronicle + retro
+ freshness (the Casement/vessel book pages mention `tell` — sweep them
to `write`; LANG-39/LANG-42 Where notes gain the chronicle link; the
observational-unlock followup → LANG-42 registry note). 4. G6.

## Plan self-review (done at write time)

- Spec §3.1→T1, §3.2→T2, §3.3→T2 step 5, §4 laws: accessor→T1,
  stitch/day/purity→T2, additivity→T2 step 6. The rename's re-pin
  obligation and the mutation arm (heard≠true printed) are explicit.
- Type consistency: reader_set/reckoning_at/ReckoningEpoch names match.
