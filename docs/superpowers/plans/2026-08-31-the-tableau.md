# The Tableau Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stage a situation declaratively — a goblin with an orange, a drow who wants it, in a room — and let a test build a world without a seed.

**Architecture:** A `Tableau` is a record of optional overrides carried on `PossessOpts` and applied at the two derivation calls a session already makes (`derive_npcs` for the cast, `interior_of` for the chamber). It rides `Session::start_in` over a shared `WorldContext`, so the expensive layers are paid once per process. The builder is the artifact; a serialization sits over it.

**Tech Stack:** Rust 2024, `serde`/`serde_json`. No new crates.

**Spec:** `docs/superpowers/specs/2026-08-31-the-tableau-design.md`

## Global Constraints

- **No new external crates**; no `HashMap`/`HashSet`; no wall-clock time; `#![warn(missing_docs)]`; `cargo fmt` last before every commit.
- **Unspecified means EMPTY for any statable layer** (spec section 5). An inherited cast is the mystery guest this campaign exists to kill.
- **Absent and empty stay distinguishable** in the serialization.
- **No tableau output may enter `docs/generated-paths.txt`.** A golden generated from a staged situation would assert about a world that never existed.
- **A tableau proves the machinery, never the world.** Staged scenes carry `provenance: staged` and are never read as evidence that the world produces anything.
- **`PossessOpts` gains a field additively, with a `Default`**, so no existing construction site changes behaviour.

## Measured facts this plan rests on

Read from the code, not assumed.

| fact | where |
| --- | --- |
| `Session::start_in(ctx, opts)` pays none of the context's cost, and is byte-identical to `start` | `session.rs`'s own doc |
| `Interior` is a hand-buildable builder: `Interior::new()`, `push(kind, within)` | `interior/anchor.rs:132` |
| `Body` is `pub` with `species: String`, `home: Facet` | `body.rs:15` |
| the roster is built at `derive_npcs(world, ctx, &mut ledger, NPC_COUNT, village.id)` | `session.rs:1252` |
| wild bodies are appended after it, and **a wild creature's `village` is `None` and it is still a legitimate target** | `session.rs`, The Hand Task 4 |
| a village is looked up before the roster and fails with `VesselError::NoSettlement` | `session.rs:1116` |
| `check_species_known(world, &village)` runs against the VILLAGE | `session.rs:1130` |

**The sixth row is the plan's most important fact.** The assumption "a body corresponds to a settlement" is *already* false in shipped code, and `PossessTarget::Creature` already resolves wild bodies. A staged body is shaped like a wild one, which is why the cast seam is smaller than spec section 10.3 feared.

---

### Task 1: The cast seam

**Files:** `windows/vessel/src/lib.rs` (`PossessOpts`), `windows/vessel/src/session.rs` (`start_held`), a test in `windows/vessel/tests/suite/`.

**Interfaces:**
- Produces: `pub struct Tableau { pub cast: Vec<StagedBody>, ... }` (grown across tasks) and `PossessOpts { pub tableau: Option<Tableau>, .. }`.
- `StagedBody` carries at minimum a species and a position; the implementer decides its exact fields by reading what `Body` needs to be constructible, and documents any field it *cannot* supply.

- [ ] **Step 1: Write the failing test**

Stage a cast of two creatures of *named, different* species in one room, possess the first, and assert the second is in `sensed.present` with the species asked for.

Assert on **species**, not on labels or ids: the point of the campaign is that the caller chose them. Include a creature whose species is **not** the local settlement's, since "a drow in a world with no drow" is the permissive ratification in section 2 and the thing most likely to be silently refused.

- [ ] **Step 2: Run it to verify it fails**

Expected: FAIL to compile — `PossessOpts` has no `tableau`.

- [ ] **Step 3: Implement**

Add the field with a `Default`, and branch at `session.rs:1252`: with a tableau carrying a cast, build the roster from it instead of calling `derive_npcs`.

**Decision rules, not predictions** — the implementer resolves these against the code:
- `check_species_known(world, &village)` (`session.rs:1130`) tests the VILLAGE's species. If it refuses a staged species -> the check has to be scoped so it applies to a derived driven body and not a staged one; a staged drow in a drow-less world is ratified behaviour, not an error.
- The village lookup at `session.rs:1116` -> leave it. The world a tableau rides is a real derived world and has one; removing it is out of scope and would widen the blast radius for nothing.
- `opts.wild_agents` appends wild bodies after the roster -> with a staged cast, appending them would reintroduce exactly the mystery guest section 5 forbids. A staged cast means the cast, and nothing else.

- [ ] **Step 4: Run to verify it passes.**

- [ ] **Step 5: Prove the seam is load-bearing.**

A green here could come from the derived roster coincidentally containing what was asked for. Mutate: make the tableau branch fall through to `derive_npcs`, and confirm the test reddens. Assert the mutation target exists before substituting, restore afterward, and **re-run to confirm the restore is green** — a stale mutated binary reads exactly like a passing test.

- [ ] **Step 6: `cargo fmt`, clippy, commit.** Regenerate artifacts only if the gate asks; the session snapshot's *shape* has not moved.

---

### Task 2: The chamber seam

**Files:** `windows/vessel/src/session.rs`, `windows/vessel/src/interior/`, a test.

**Interfaces:** `Tableau` gains a chamber override carrying an `Interior` (and its light, if light is separable — the implementer reads `MAP-interior-light`'s shipped form and follows it).

- [ ] **Step 1: Write the failing test**

Build an `Interior` by hand with a geometry the derived one would not produce, stage it, and assert the chamber the session reports is the one built — via the floor plan, not via prose.

Include the **negative** direction, because it is what a light test needs: a chamber staged with no light source is dark, and dark is distinguishable from unspecified.

- [ ] **Step 2: Run to verify it fails.**

- [ ] **Step 3: Implement** at the `interior_of(room, terrain)` call site: with a chamber override, use it; without one, derive exactly as today.

- [ ] **Step 4: Run to verify it passes.**

- [ ] **Step 5: `cargo fmt`, clippy, commit.**

---

### Task 3: The builder, and the file over it

**Files:** a new module under `windows/vessel/src/` (the implementer places it beside the types it composes), plus serialization.

**Interfaces:** `Tableau::new()` and chained setters; `Tableau::from_json(&str) -> Result<Tableau, _>`.

- [ ] **Step 1: Write the failing test**

Two assertions, and the second is the load-bearing one:
- a tableau built by the builder and the same tableau round-tripped through JSON produce the **same session state**;
- **absent and empty are distinguishable**: a tableau with `cast: []` and one with no `cast` key are *both* empty (section 5's rule), but a tableau with `things: []` must not silently become a tableau that inherits things. Pin the distinction the format is most likely to lose.

- [ ] **Step 2: Run to verify it fails.**

- [ ] **Step 3: Implement.** The builder is the real artifact; `from_json` constructs one. There is exactly one construction path — the serialization must not build a session directly.

- [ ] **Step 4: Run to verify it passes.**

- [ ] **Step 5: `cargo fmt`, clippy, commit.**

---

### Task 4: The orange

**Files:** `repertory/the-orange-staged.scene.json` (or the implementer's chosen name), `cli/tests/suite/repertory_corpus.rs`, and whatever CLI surface lets a human watch it.

**Interfaces:** repertory scenes gain `provenance: found | staged`.

- [ ] **Step 1: Write the failing test**

A staged scene: a goblin holding an orange, a drow present, in a room. Its beats assert what The Company's beats wanted and could not get — `/sensed/present/0/label` names a goblin, `/sensed/present/0/carrying` is **non-empty** and names the orange.

That second assertion is the one The Company had to leave as *the channel exists*, because no world it could find had a co-located creature holding anything. Staged, it can finally assert the thing it meant.

- [ ] **Step 2: Run to verify it fails.**

- [ ] **Step 3: Add `provenance` to the scene record and the resolver.**

A `staged` scene resolves its situation from its tableau instead of searching for a witness. **Decision rule:** a scene carrying both a tableau and a `co-located` selector is a corpus error — refuse it loudly at parse time rather than picking one.

- [ ] **Step 4: Give Nathan a way to watch it.**

The exit criterion is that he can *see* it, not that a test passes. Add the CLI path that takes a tableau file and drops into `possess` on it. Keep it thin — it is a front-end over Task 3's builder.

- [ ] **Step 5: Run the whole corpus.** Every scene needs a floor in the commit that adds it.

- [ ] **Step 6: `cargo fmt`, clippy, commit.**

---

### Task 5: The second customer, then close

- [ ] **Step 1: Convert one test.**

Find a test that today builds a world through genesis and asserts on an arrangement it did not choose. Rewrite it against a tableau. **Measure before and after** — wall time for that test alone, captured, not estimated.

**Decision rule:** if the converted test is not materially cheaper, that is a finding and it goes in the chronicle as a null. Do not tune the tableau to win the comparison after seeing it.

- [ ] **Step 2: Chronicle** — `book/src/chronicle/the-tableau.md`, wired into `SUMMARY.md`. Lead with the correction: the blocker was a decision, not the world. Then the diff-not-scratch shape, the hermetic default and why, and the orange.

- [ ] **Step 3: Retrospective** — `docs/retrospectives/the-tableau.md`, indexed. Carry: that a controller's own spec became a constraint nobody re-examined for a full campaign; that `Interior` was already a builder and nobody had looked; that wild bodies already broke the settlement assumption.

- [ ] **Step 4: A `PROC-` row for the provisional-decision gap** (spec section 1): the decision log cannot distinguish a ruling from a stopgap-pending-cost, and one was read as the former.

- [ ] **Step 5: Confidence Gradient.** Bet 4's taste half was described as "currently unreachable in most worlds, because the moment that would be judged does not assemble." A tableau assembles it. **Decision rule:** re-score or annotate to say what changed — staging makes the moment reachable, and whether it is *worth watching* is now answerable by a human for the first time. That is the bet moving, or it is not; say which.

- [ ] **Step 6: Regenerate** (`make rebaseline`, then `make rebaseline-goldens`, then check for goldens in neither), drift-check, commit.

- [ ] **Step 7: Submit** with an authored `Sluice-Headline:` trailer, verified with `sluice_headline_of` before submitting.

---

## Self-review

**Spec coverage.** Section 1 (the correction) -> Task 5 Steps 2-4. Section 3 (diff not scratch) -> Tasks 1-2 ride `start_in`. Section 4 (builder-first) -> Task 3, with the one-construction-path rule stated. Section 5 (hermetic default, absent vs empty) -> Task 1 Step 3's third rule and Task 3 Step 1's second assertion. Section 6 (the three seams) -> Tasks 1-2, things needing none. Section 7 (provenance) -> Task 4 Step 3. Section 8 (exit criteria, both) -> Task 4 Step 4 and Task 5 Step 1. Section 10.1 (no committed artifact) -> Global Constraints. 10.2 (`PossessOpts` additive) -> Task 1 Step 3. 10.3 (settlement assumption) -> the facts table's sixth row, which largely dissolves it. 10.4 (`--out` from a staged session) -> **NOT COVERED BY A TASK.**

**Gap found and closed.** Spec section 10.4 left open whether a staged session may write a saved world, with "refuse" as the conservative default, and no task implemented it. A staged session that can write `possess --out` would put a world that never existed on disk, indistinguishable from a generated one — which is section 10.1's hazard through a different door. **Added to Task 1 Step 3 as a fourth decision rule:** a session carrying a tableau refuses `--out`, and the refusal names the reason.

**Second gap found and closed.** Task 4's beats assert a *non-empty* `carrying`, which The Company could not. Nothing in the plan said the orange must actually be placeable via the tableau — Task 3's builder must therefore carry things-with-holders, not only a cast. Named in Task 3's interface line.

**Type consistency.** `Tableau` grows across Tasks 1-3 and is constructed identically in each; `PossessOpts { tableau: Option<Tableau>, .. }` is the only new field and every existing site keeps its `Default`.
