# The Confidant — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> to implement this plan task-by-task.

**Goal:** The host answers when asked, and what it says differs from what it
feels in two specific, measurable ways.

**Architecture:** One pipeline, two stages, two residues. Arbitration produces a
true `AffectLabel`; an introspection filter drops what arbitration suppressed
(the cognitive residue); a per-culture lexicon lookup maps what remains onto the
nearest word the culture has (the lexical residue); the utterance lands in the
player's knowledge as `heard`, which is already outside the ground-truth check.

**Tech Stack:** Rust 2024, `serde`/`serde_json`/`libm` only. `windows/vessel`,
`domains/language`, `kernel/src/registry.rs`, `windows/lab`.

**Spec:** `docs/superpowers/specs/2026-08-24-the-confidant-design.md`

## Global Constraints

- Branch guard before EVERY commit:
  `git branch --show-current | grep -qx 'campaign/the-confidant' || exit 1`
- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock.
- No new dependencies. Every crate sets `#![warn(missing_docs)]`.
- Every `pub`-boundary primitive carries a `type-audit:` tag; adding one moves
  `docs/audits/type-audit-report.md` — regenerate in the SAME commit.
- `make rebaseline` THEN `make rebaseline-goldens` — the second is not implied
  by the first and covers `windows/vessel/tests/fixtures/*.json`.
- `cargo fmt` before every commit. `make gate-commit` green before every commit.
- **Never put backticks in a `git commit` heredoc** — they execute.
- Run the suite ONCE, capture to a file, grep the file.
- Renaming a test is a commit-gate change: `docs/timings/subfloor-roster.tsv`
  selects by EXACT name and a stale id drops tests silently.

## A probe this plan prescribed, which is BLIND (Task 1's finding)

Do not verify a concept-registry change by building seed 42 and diffing the
JSON. That instrument reports IDENTICAL for every input: an unregistered probe
concept never reaches `concept_epoch`, because `concept_epoch` is consulted
only for names already in the world's concept registry (`register_all`), which
is independent of `EPOCH_COHORTS`. Task 1 caught this ONLY because it ran a
positive control and the control also came back empty.

The right instrument drives `assign_proto_roots_with_epoch` directly, as
`domains/language/tests/suite/accession_properties.rs` does.

## A note on this plan's code blocks

Where a signature is quoted below it was **read from the tree** and is exact.
Where a block is marked `SHAPE`, it illustrates intent and its call signatures
are NOT verified — the step that precedes it names the file to read first. **An
implementer who finds the code disagrees with this plan should follow the code
and say so in their report.** In the predecessor campaign every defect that
reached an implementer originated in plan text, not implementer code.

---

## Task 0: Announce (controller runs this, not a subagent)

Post a board `notice` with `polarity=hold-off` on `windows/vessel/` and
`domains/language/`, naming the spec and plan. `domains/language` is the busier
half: The Cant and the lexicon work live there.

---

## Task 1: Prove insertion-stability before designing around it

**This task exists because the spec's central de-risking claim is mine and is
unrun.** Spec §2.2 says appending an accession cohort displaces nothing. That
reading reversed my own earlier prediction, and it rests on reading
`domains/language/src/accession.rs` plus two committed tests — not on a run.

**Files:** read `domains/language/src/accession.rs` (module doc and
`EPOCH_COHORTS`), `domains/language/src/etymology.rs`
(`assign_proto_roots_with_epoch`), `domains/language/src/lexicon.rs`
(`proto_root_universe`).

- [ ] **Step 1: Capture a baseline.**
```bash
cargo run -q -p hornvale -- new --seed 42 --out /tmp/conf-base.json
shasum -a 256 /tmp/conf-base.json
```
- [ ] **Step 2: Append a throwaway cohort.** Add a new `&[...]` to the END of
  `EPOCH_COHORTS` containing one nonsense concept. **Never edit an existing
  cohort** — the module doc is absolute on this and records a withdrawn
  exception that cost a campaign.
- [ ] **Step 3: Rebuild and compare.** Report the fact count and sha of both.
  This is a **decision rule, not a prediction**:
  - Only registry entries for the new concept differ → insertion-stability
    HOLDS. Revert the throwaway, record the evidence, proceed.
  - Any existing WORD changed, or any fact unrelated to the new concept moved →
    **STOP and report.** The spec's §2.2 is wrong and the campaign needs
    re-scoping before Task 3.
- [ ] **Step 4: The positive control.** An unchanged output is consistent with
  "nothing moved" AND with "this comparison cannot see movement". Insert the
  same concept at cohort **zero** and confirm the comparison DOES report
  displacement. If both are identical, the instrument is blind and Step 3
  proved nothing.
- [ ] **Step 5: Grep for independent universe re-derivations.** Spec §5.2:
  `windows/lab`'s `monophyly-goblinoid` metric once rebuilt the universe from
  every registered concept and missed `proto_root_universe`'s exclusion filter;
  it was invisible until a later epoch first sorted after the excluded cohort.
  Find every site that re-derives the universe and confirm each calls
  `proto_root_universe`. Report the list.
- [ ] **Step 6: Revert everything, verify a clean tree, commit the findings**
  as a short note under `docs/` — no code change ships from this task.

---

## Task 2: Affect escapes `advance_one`

**Verified:** `advance_one` returns `bool`; `grep -n "affect" windows/vessel/src/session.rs`
returns nothing, so affect is computed in arbitration and dropped. `Resolution`
carries `intent`, `mode` and `affect` together.

**Files:** `windows/vessel/src/liveness.rs`, `windows/vessel/src/session.rs`.

- [ ] **Step 1: Write the failing test first.** A driven body's affect is
  readable after a tick and is NOT a fixed value. Assert against a discriminating
  property — the same trap The Hand hit twice: `AffectLabel` has six variants, so
  `matches!` over all six is exhaustive and cannot fail. **Assert a specific
  label at a specific state, or that two different situations produce different
  labels.**
- [ ] **Step 2: Run it; capture the BEHAVIOURAL red.** A red from a compile
  error proves nothing about an assertion.
- [ ] **Step 3: Retain affect beside mode.** The Hand already retains
  `driven_mode: Option<Mode>` on `Session`, written where the driven body's walk
  runs. Retain affect by the same route and expose it the same way.
- [ ] **Step 4: Green, then mutation-prove.** Replace the retained affect with a
  constant and show the new test reddens. Use `scripts/mutate.py`, which refuses
  unless the target text is found and unique — a no-op mutation produces a green
  that looks like robustness.
- [ ] **Step 5: `cargo fmt`, `make gate-commit`, commit.**

**Produces:** a `Session` accessor returning the driven body's current
`AffectLabel`. Later tasks consume it.

---

## Task 3: Felt states become concepts

**Verified:** `ConceptKind` (`kernel/src/registry.rs`) has `Substance`,
`Living`, `Celestial`, `Terrain`, `Social`, `Body`, `Kin`, `Quality`, `Act` —
no felt-state kind. Zero affect concepts are registered.

**Depends on Task 1 reporting insertion-stability HOLDS.** If it did not, stop.

- [ ] **Step 1: Add a `ConceptKind` variant** for a felt state, with a doc
  comment distinguishing it from `Quality` the way `Act`'s doc distinguishes
  doing from having.
- [ ] **Step 2: Register the six affect concepts**, one per `AffectLabel`
  variant, appended as a NEW cohort at the end of `EPOCH_COHORTS`.
- [ ] **Step 3: Add a test that the cohort is append-only** — that no existing
  cohort's contents changed. This is the guard the module doc asks for and
  nothing currently enforces it mechanically.
- [ ] **Step 4: Regenerate and apply the §4 decision rule.**
```bash
make rebaseline && make rebaseline-goldens
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```
  - Only registry entries and the concept-registry book page moved → commit the
    drift in this commit.
  - An existing word or an unrelated fact moved → **STOP and report.**
- [ ] **Step 5: `make gate-commit`, commit** including the regenerated artifacts.

---

## Task 4: The lexical gap — the nearest word a culture has

**Depends on Task 3** — the six affect concepts must be registered before a
culture can have or lack a word for one. If they are absent, that is Task 3
incomplete, not a lexicon problem.

**Read first:** `domains/language/src/lexicon.rs` — `build_lexicon`,
`LexEntry`, `ExposureClass`, `GapReason`, `proto_root_universe`. The two-pass
assembly and the `Steeped`/`KnowsOf`/`Unknown` exposure classes are the
mechanism; do not invent a parallel one.

**The property to build:** given a true `AffectLabel` and a culture, produce
either the culture's word for it, or the culture's word for the NEAREST state it
does have a word for, together with the `GapReason` for the missing one.
Distance is over the circumplex — `AffectLabel`'s own docs place each variant on
valence × arousal, so the ordering is authored there, not invented here.

- [ ] **Step 1: Write the failing test.** Two cultures, one internal state, and
  a different word out of each. If you cannot find two authored cultures that
  differ, that is a finding: report it rather than authoring a difference to
  make the test pass.
- [ ] **Step 2: Behavioural red.**
- [ ] **Step 3: Implement the lookup + nearest-word fallback.**
- [ ] **Step 4: Green, then prove the fallback fires.** A test where the culture
  HAS the word and one where it does not, with different outputs.
- [ ] **Step 5: `cargo fmt`, `make gate-commit`, commit.**

---

## Task 4b: A culture can actually hold a feeling-word

**This task exists because Task 4 found the mechanism is unreachable.**
`exposure_of_impl` (`windows/worldgen/src/lib.rs`) never touches
`felt_state_pack()`, so every felt-state concept falls through its catch-all
Gap for every real generated culture. Without this task the lexical gap ships
untested by any world and Task 7's metrics read 0 for all fifteen peoples.

**The rule must DERIVE, not be authored** — spec §5.1. If a table says which
people lacks which feeling-word, then measuring the distribution of deficiency
recovers that table, which is circular. Derive exposure from vectors already
authored for other reasons — `MindVector` (`threat_response`,
`deliberation_latency`, `time_horizon`) and `SocietyVector`
(`domains/species/src/lib.rs`) — so the deficiency distribution is a
consequence rather than a premise.

- [ ] **Step 1: Read `exposure_of_impl`** and how an existing pack earns
  `Steeped` / `KnowsOf` / `Unknown`. Follow that shape exactly.
- [ ] **Step 2: Write the failing test first** — two authored peoples whose
  vectors differ produce different felt-state exposure. Behavioural red.
- [ ] **Step 3: Derive the rule** from the vectors. Keep it simple and state
  the mapping in a doc comment; a reader must be able to see why a people
  with a given psychology has or lacks a given word.
- [ ] **Step 4: Report the DISTRIBUTION across all fifteen peoples**, not a
  summary. **A uniform result is a finding, not a pass** — if every people
  ends up with the same exposure, the mapping is degenerate and you should say
  so rather than proceed.
- [ ] **Step 5: Drift.** New exposures change lexicons, so words appear where
  there were gaps. `make rebaseline` THEN `make rebaseline-goldens`. Existing
  words for OTHER concepts must not move — if one does, STOP and report.
- [ ] **Step 6: `make gate-commit`, commit** with regenerated artifacts.

## Task 5: The cognitive gap — bounded introspection

**Verified property:** arbitration ranks drives and keeps the winner; the
discarded ranks are what the creature cannot perceive about itself. This is the
half Nathan required be SURFACED, not merely modelled.

- [ ] **Step 1: Write the failing test.** A creature with two active drives
  reports only the dominant one, and the suppressed drive is retrievable
  **through the accessor THIS task introduces** while being absent from the
  utterance. Do NOT depend on Task 7's instrument — it does not exist yet.
  Task 7 consumes what you expose here; it does not define it.
- [ ] **Step 2: Behavioural red.**
- [ ] **Step 3: Implement the filter** between arbitration and the lexicon
  lookup.
- [ ] **Step 4: Mutation-prove it.** Neutralise the filter (let everything
  through) and show a test reddens. If nothing reddens, the filter is
  unobserved — that is the exact defect The Hand spent three fix rounds on.
- [ ] **Step 5: `cargo fmt`, `make gate-commit`, commit.**

---

## Task 6: The verb

- [ ] **Step 1: Read how an existing verb is dispatched** in
  `windows/vessel/src/session.rs` (`handle`, the `"npcs" => Turn::Out(...)`
  arm is a worked example) and how `absorb_common` lands a heard statement in
  `windows/vessel/src/knowledge.rs`. Report both signatures before writing.
- [ ] **Step 2: Write the failing test** — asking produces an utterance, and the
  utterance lands in the player's knowledge as `heard`.
- [ ] **Step 3: Behavioural red.**
- [ ] **Step 4: Implement.** The answer is realised in the host's own tongue.
  Per spec §5.3, if the host's tongue is not Common, EITHER separate untranslated
  utterances from misreports in the output OR scope this task to Common-speaking
  hosts — **decide, implement it, and say which you chose and why.**
- [ ] **Step 5: Assert the player is never shown the arbitration.** A test that
  the true `AffectLabel` does not appear in the rendered turn. This is a
  design invariant, not a formatting detail.
- [ ] **Step 6: `cargo fmt`, `make gate-commit`, commit.**

---

## Task 7: The instrument

**Studies are data, metrics are code** (decision 0011). Three metrics, each
per-people so the fifteen modelled peoples are comparable:

- `reportable_fraction` — of reachable internal states, the share with a term.
- `collapse_ratio` — how many distinct states share a nearest word (conflation).
- `misreport_distance` — circumplex distance between true and reported state.

- [ ] **Step 1: Read `windows/lab/` on how a metric is registered** and how
  `lab list-metrics` discovers one. Follow the existing pattern exactly.
- [ ] **Step 2: Write each metric's test first**, including a case that is NOT
  the degenerate one — a metric that reports exactly 100% or exactly 0 across
  every people is usually reporting a default rather than a measurement.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run across the fifteen peoples and report the DISTRIBUTION**,
  not a summary statistic. A uniform value across all fifteen is a finding
  (probably a bug) and must be reported, not smoothed.
- [ ] **Step 5: NO STUDY IS PREREGISTERED IN THIS TASK.** Spec §5.1: a study
  whose hypothesis follows from the authored mechanism recovers the constant we
  typed in. The metrics ship; a study over them is a later, separate act.
- [ ] **Step 6: `cargo fmt`, `make gate-commit`, commit.**

---

## Task 8: The Hand's bundled minors

One dispatch, five independent edits, reviewed as one diff.

- [ ] `windows/vessel/src/session.rs` — the doc comment on
  `place_creature_at_me` is duplicated back to back. **Locate it by CONTENT,
  not by line**: the paragraph beginning "A test seam, not a verb" appears
  twice above the same fn. Tasks 2, 5 and 6 all edit this file first, so any
  line number quoted here is already stale. Delete one copy; verify the
  sibling `place_creature_out_of_my_sight` keeps its own.
- [ ] `windows/vessel/src/plan.rs` — an eaten backslash continuation (a run of
  literal spaces mid-string), one occurrence, pre-existing on main from
  `72784fea1`. Re-derive the line number; it was 821 when found. Confirm it is
  still the only one in that file before and after.
- [ ] `windows/lab/src/synthetic.rs` ~253 — an inline copy of
  `PerceptionVector::MANIKIN`; use the named constant.
- [ ] `clients/game/bin/tests/driver.rs` — a test name saying `mints` after
  decision 0227 says selection. **This crate is OUTSIDE the cargo workspace**,
  so it has no `subfloor-roster.tsv` entry and the rename is safe — but nothing
  in a gate compiles it either. Run `make game-check` after.
- [ ] `windows/vessel/tests/suite/controller_swap.rs` — **TWO** cross-seed pins
  now, both knife-edge, both to harden. F-H1's mode pin: a randomly redrawn
  pair coincides at p≈0.46. And Task 2's affect pin, MEASURED at 20 seeds —
  Eager 12/20, Content 7/20, Frustrated 1/20, only 3 of 6 variants reachable,
  concentration 0.485, so a random pair collides ~49% of the time and the
  hardcoded seed 13 draws from the rare tail. The larger enum bought no
  robustness: the reachable distribution sets the odds, not the variant count.
  Harden BOTH to assert ≥2 distinct values across 3+ seeds rather than
  `assert_ne!` on a chosen pair.
- [ ] `make gate-commit` AND `make game-check`, then commit.

---

## Task 9: Close the campaign

Follow the project's `closing-a-campaign` skill. Reserve a decision block with
`make decision-block NAME=the-confidant` — **never take a number outside it**.
Four decision records per spec §6, chronicle, retrospective, freshness sweep,
Confidence Gradient re-score if a bet moved, registry flips
(`PLAY-host-is-a-narrator`, `PLAY-affect-becomes-testimony`,
`PLAY-host-is-the-voice`, and F-H8's discharge in The Hand's retrospective).

**Carry the accumulated deferred-minor list INTO the final review.** In the
predecessor campaign a task wrote "final review should triage" into the ledger,
nothing carried the list forward, and the item shipped looking exactly like one
that had been consciously accepted.

---

## Status: COMPLETE (2026-08-25)

All nine tasks landed, plus a Task 4b added mid-flight (a culture must be able
to *hold* a feeling-word before the instrument can measure who lacks one), and
a Task 7 reshape that moved the instrument out of the census.

Task 9's close delivered: six decision records (0256–0261, from the reserved
block 0256–0265), the chronicle `book/src/chronicle/the-confidant.md` wired
into `SUMMARY.md`, the retrospective `docs/retrospectives/the-confidant.md`,
a freshness sweep, a Confidence Gradient re-score, and the registry flips.

Registry flips made: `PLAY-affect-becomes-testimony`, `PLAY-host-is-the-voice`
and `PLAY-told-channel-already-fits` → `shipped`; `PLAY-host-is-a-narrator`
kept `elaborated` with the partial landing recorded (one of its three named
channels routed, two still unrouted); `KNOW-needs-oracle` annotated —
`needs` itself is unchanged, the campaign built a fallible sibling beside it.
New backlog row `TOOL-duplicated-rulebook-audit`. The Hand's F-H1, F-H7, F-H8
and F-H11 are discharged in that campaign's own register; F-H10 stays open and
is carried forward.
