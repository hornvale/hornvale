# The Company Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put a co-located creature's custody on the wire, turn a repertory witness from a pin into a resolved query, and land `the-orange`'s first two beats honestly.

**Architecture:** `carrying` joins `PresentEntry` (the presence-gated channel) beside `felt`, derived through a holder-parameterised extraction of the existing `carried()` fold so one function still answers "what is in whose hands". The repertory resolver gains a selector it resolves **in-process** against vessel's own API, and drives the beats **out-of-process** through `possess --script` as before.

**Tech Stack:** Rust 2024, `serde`/`serde_json`. No new crates.

**Spec:** `docs/superpowers/specs/2026-08-30-the-company-design.md`

## Global Constraints

- **No new external crates** (`ALLOWED_EXTERNAL`, `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`**; `BTreeMap`/`BTreeSet`/`Vec` only.
- **No wall-clock time**; `Instant` is banned in tests.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a doc line.
- **`cargo fmt` last, before every commit.**
- **Both fields are additive on `vessel/session/v2`. No schema version moves.** If a change would force a version bump, STOP — that is an epoch event and a different campaign.
- **A verdict variant no code can produce is forbidden** (spec section 6). `UNWITNESSED` ships in the same task as the search that emits it.
- **`social` is not touched.** Nothing is added to it and nothing in it is narrowed.

## Measured facts this plan rests on

Established by running commands, not by reading code.

| fact | value |
| --- | --- |
| witnesses with non-empty `sensed.present` | **0 of 24** (12 seeds x {flagship, most-populous-settlement}) |
| the same, after `wait 1 / 5 / 20 / 60` at seed 42 | still 0 |
| **the co-located pair at seed 42** | otyugh `9630022852472602626` and carrion-crawler `9630022852472602627`, both room `633110509`, each `present=1` |
| `/sensed/present/0/label` at that witness | `"a wild carrion-crawler"` |
| `/sensed/present/0/carrying` there | does **not** resolve — this campaign's target |
| `--creature 13226382737635672064` | **refused**, while the other six roster members were accepted |
| world build | ~3.5 s; roster at seed 42 is 7 derived bodies |

**The refusal in row six is not an error to fix here.** The search must treat a refused roster member as *not a witness* and continue.

---

### Task 1: `carried_by`, and custody on the presence-gated channel

**Files:**
- Modify: `windows/vessel/src/snapshot.rs` (`PresentEntry`)
- Modify: `windows/vessel/src/session.rs` (`carried`, and the `PresentEntry` construction near the `here` binding)
- Test: `windows/vessel/tests/suite/` — the file that already exercises presence or carrying; read the suite and put it where its neighbours are

**Interfaces:**
- Produces: `fn carried_by(&self, holder: EntityId) -> Vec<(EntityId, &'static str)>` on the same type that owns `carried`; `PresentEntry.carrying: Vec<CarriedEntry>`.
- `carried(&self)` becomes `self.carried_by(self.agent_entity())` and keeps its signature, so every existing caller is untouched.

- [ ] **Step 1: Write the failing test**

Assert the property, not a fixture: a creature co-located with the driven body, holding a thing, is reported as carrying it on `sensed.present`. Read the neighbouring tests for how a session with a held thing is constructed — The Chattel built that machinery and this test should reuse it rather than invent a second way.

The test must also pin the **negative** direction in the same file, because it is the half that would otherwise rot: a body carrying nothing reports an empty list, not an absent field.

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo nextest run -p hornvale-vessel --test suite -- <your test name>`
Expected: FAIL — `carrying` is not a field of `PresentEntry`.

- [ ] **Step 3: Extract the holder-parameterised fold**

`carried` is currently `held_by(&self.ledger, self.agent_entity(), self.day)` plus noun mapping. Extract the body to `carried_by(holder)` and make `carried` delegate.

**Do not add a second ledger read.** `carried`'s own comment states the rule and it binds this change too: *"a pane that disagreed with the verb about what is in hand would be a worse defect than an absent field, and there is exactly one function here to disagree with."* After this task there is still exactly one.

- [ ] **Step 4: Add the field and derive it**

`PresentEntry` gains `carrying: Vec<CarriedEntry>` with a doc line saying what it is and why it lives here — the reason is `felt`'s reason, and it should cite it: a presence-gated read of another creature's state, which is why it is not on `social`.

Populate it at the `PresentEntry` construction site (the `here` binding) from `carried_by(npc.entity)`.

`PresentEntry` carries a `type-audit:` tag listing its primitives. **Update it** — a new field at a `pub` boundary fails the audit otherwise, and the audit is in the commit gate.

- [ ] **Step 5: Run the test to verify it passes**

Run: `cargo nextest run -p hornvale-vessel --test suite -- <your test name>`
Expected: PASS, both directions.

- [ ] **Step 6: Commit**

`cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; then commit. Do **not** regenerate artifacts yet — Task 2 owns that, and doing it here splits one drift across two commits.

---

### Task 2: The goldens, down all three paths

**Files:**
- Modify: whatever the regeneration writes. Do not guess the list; read it from the commands.

**Interfaces:** none.

This task exists because the repository has **three** regeneration paths and the root `CLAUDE.md` documents one.

- [ ] **Step 1: Regenerate, in this order**

```
make rebaseline
make rebaseline-goldens
```

`make rebaseline` does **not** write `windows/vessel/tests/fixtures/*.json`; `make rebaseline-goldens` does. Running only the first and reading a clean drift check is a false clean.

- [ ] **Step 2: Find the goldens in neither**

`make rebaseline-goldens` is an explicit list of seven scoped invocations. A golden outside that list is accepted only by its own scoped command, and its failure message will still name `make rebaseline-goldens` as though that worked.

Run the full workspace suite and read the failures:

```
cargo nextest run --workspace > /tmp/hv-gold.log 2>&1; echo "exit=$?"
grep -E "FAILED|panicked" /tmp/hv-gold.log
```

**Decision rule:**
- no golden failures -> both paths covered it; go to Step 3.
- a golden test fails and names `REBASELINE=1` -> run **that test's own scoped invocation** (`REBASELINE=1 cargo nextest run -p <crate> --test <target> -- <name>`), not `make rebaseline-goldens` again.
- a golden fails that no `REBASELINE=1` accepts -> STOP and report; that is a real behavioural change, not a stale fixture.

- [ ] **Step 3: Read the diffs before committing**

A moved golden means emitted bytes changed. That is expected here — `carrying` is a new key on every `present` entry. **Confirm the diff is only that.** A snapshot whose `felt`, `label` or `entity` moved is not this change and must be explained before it is committed.

```
git diff --stat
git diff -- clients/game/core/tests/fixtures/ | head -60
```

- [ ] **Step 4: Commit the regeneration**

`git add -A` and commit, with a message naming which of the three paths wrote what.

---

### Task 3: A witness is a resolved query

**Files:**
- Modify: `cli/tests/suite/repertory_corpus.rs`
- Modify: `repertory/the-founding.scene.json`, `repertory/the-orange.scene.json`

**Interfaces:**
- Produces: `enum Selector { Pin { seed, target, day }, CoLocated { seed_from, seed_limit } }` (names are the implementer's; the spec's vocabulary is `co-located`, optionally species-constrained); `fn resolve(selector) -> Option<Witness>`.
- `Witness` keeps its shape — it is what a resolved selector produces and what `run_at` consumes.

The search runs **in-process**: `cli` already depends on `hornvale-vessel` and `hornvale-worldgen`, so the resolver can build a world once and check every roster member without paying a process per member. The beats still run **out-of-process** through `run_at`, which is the real driving surface and must not change.

- [ ] **Step 1: Write the failing test**

A `co-located` selector resolves, at seed 42, to a witness whose `sensed.present` is non-empty. Assert the **property** (present is non-empty), never the entity id — the id is lineage-derived and pinning it is the defect this task exists to remove.

- [ ] **Step 2: Run it to verify it fails**

Expected: FAIL to compile — the selector type does not exist.

- [ ] **Step 3: Implement resolution**

Read how `cmd_possess` builds a world and derives its roster, and use that same path; do not construct a second one. For each roster member, ask whether it has a co-located creature — vessel already answers this (`colocated_entities` is `pub`), so prefer asking vessel over re-deriving positions here.

**A refused roster member is not a witness.** Treat it as a miss and continue; do not propagate it as an error.

Cache the resolution for the duration of a test run, keyed by selector. Re-resolving per beat would multiply an already-expensive search by the beat count.

- [ ] **Step 4: Run it to verify it passes**

- [ ] **Step 5: Move the corpus onto selectors**

The founding scenes keep a pinned witness — they are regression scenes about a specific world and the pin is correct for them. `the-orange` moves to a `co-located` selector.

- [ ] **Step 6: Commit**

---

### Task 4: `UNWITNESSED`, and the split of `the-orange`

**Files:**
- Modify: `cli/tests/suite/repertory_corpus.rs`
- Create: `repertory/two-in-a-room.scene.json`
- Modify: `repertory/the-orange.scene.json`

**Interfaces:**
- Produces: `Verdict::Unwitnessed`; `FLOORS` gains two rows.

`UNWITNESSED` ships **here**, with the search that can emit it — not in Task 3 and not earlier. A variant no code can produce is a declaration, which is the defect the family exists to avoid.

- [ ] **Step 1: Write the failing tests**

Two:
- `two-in-a-room` resolves `AUTHORED` — beat 1 asserts `/sensed/present/0/label`, beat 2 asserts `/sensed/present/0/carrying`.
- a scene whose selector no world satisfies resolves `UNWITNESSED`, **not** `ABSENT`. Build that scene in the test, not in the corpus, with a selector constrained to a species pair that does not co-occur — and bound its search, or the test runs until the seed space is exhausted.

- [ ] **Step 2: Run to verify they fail**

- [ ] **Step 3: Write `two-in-a-room` and add the verdict**

`two-in-a-room` is `control: 40` and carries the mechanism, not the species. Its beats are the two above.

`Verdict::Unwitnessed` returns `"UNWITNESSED"` from `name()`, and `verdict_of` returns it when the selector resolves to nothing — **before** any beat is evaluated, since with no witness there is nothing to run.

- [ ] **Step 4: Update `the-orange`**

Its declaration's stated reason is now false in two directions and must be rewritten to what is true after this campaign: custody is on the wire for a co-located creature, and what is missing is (a) a world putting a drow and a goblin in one room and (b) any verb that addresses another creature.

**Decision rule for its verdict:**
- the species-constrained selector finds no world within its bound -> `UNWITNESSED`; set its floor to that and say so in the chronicle.
- it finds one -> the scene runs; record whatever verdict it produces and do not force it. A found drow-and-goblin world is a finding worth leading the chronicle with.

- [ ] **Step 5: Extend `FLOORS` and run the whole file**

Every scene needs a floor in the commit that adds it.

- [ ] **Step 6: Commit**

---

### Task 5: Close

- [ ] **Step 1: Measure the resolver's new cost** and record it. The search is the new expense; state it next to The Repertory's 43.6 s so the two are comparable.

- [ ] **Step 2: Chronicle** — `book/src/chronicle/the-company.md`, wired into `book/src/SUMMARY.md`. Lead with the finding, not the feature: beat 1 was passing for the wrong reason, and the measurement that showed it (0 of 24). Then custody's home and why, the pin-to-query move, and `UNWITNESSED`.

- [ ] **Step 3: Retrospective** — `docs/retrospectives/the-company.md`, indexed in its README. Carry: the witness-as-golden-string argument; that the fix for The Repertory's never-firing beat was itself asserting against the wrong channel; the three regeneration paths; the refused roster member.

- [ ] **Step 4: Gradient** — bet 4 ("an inhabited moment worth standing in") gained evidence: the world assembles a shared room rarely and not at either default selector. **Decision rule:** if that is a claim about the world rather than the instrument, re-score or annotate bet 4 and say so; if it is only about `--target`, note it in the chronicle and leave the bet alone.

- [ ] **Step 5: Regenerate and drift-check**, all three paths again (Task 2's sequence), then commit.

- [ ] **Step 6: Submit.** `make gate-commit`, push, then `make sluice BRANCH=campaign/the-company REF=<full-sha>` with an authored `Sluice-Headline:` trailer in the range's last paragraph. Verify with `sluice_headline_of` before submitting.

---

## Self-review

**Spec coverage.** Section 2 (beat 1's defect) -> Task 4 Step 3. Section 3 (custody on `PresentEntry`) -> Task 1. Section 4 (query not pin) -> Task 3. Section 5 (selector + in-process search) -> Task 3 Step 3. Section 6 (`UNWITNESSED`) -> Task 4, shipped with its producer. Section 7 (the split) -> Task 4 Steps 3-4. Section 8 (non-goals) -> Global Constraints. Section 9.1 (three regeneration paths) -> Task 2 entire. 9.2 (refused member) -> Task 3 Step 3 and the facts table. 9.3 (no determinism change) -> Global Constraints. 9.4 (search placement) -> settled: in-process in the resolver, because `cli` already depends on vessel and worldgen; a CLI subcommand is a followup, not a requirement.

**Gap found and closed.** Task 4's "a scene no world satisfies" test would search the whole seed space to prove a negative. Step 1 now requires the search be bounded, and the bound is the implementer's to choose against the measured per-seed cost.

**Second gap found and closed.** `PresentEntry` carries a `type-audit:` tag enumerating its primitives; a new `pub` field fails the default-deny audit, which runs in the commit gate. Task 1 Step 4 names it. This is exactly the class of thing that otherwise surfaces as a confusing gate red two tasks later.

**Type consistency.** `carried_by(holder: EntityId) -> Vec<(EntityId, &'static str)>` matches `carried`'s existing return, so the delegation is a body move with no signature change. `Witness` is unchanged in shape across Tasks 3-4 — a resolved selector produces one, and `run_at` consumes one exactly as it does today.
