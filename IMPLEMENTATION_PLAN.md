# The Fetch Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans (recommended) to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make remembered water actor-relative while preserving the existing `Option<Facet>` belief surface and deterministic fold invariants.

**Architecture:** Keep memory as the existing derived `LatestVisit::water_at` set. Change only the geometry phase of the solo belief fold and its incremental twin to use the committed current position. Leave route selection with the existing planner and deliberately do not reuse the home-keyed route memo for position-varying reads.

**Tech Stack:** Rust, Cargo nextest, Hornvale vessel liveness tests, existing golden and audit tooling.

**Spec:** `docs/superpowers/specs/2026-09-06-the-fetch-design.md`

## Global Constraints

- No new ledger predicate, save field, RNG draw, or stream consumption.
- Preserve `believed_water -> Option<Facet>` and all existing public vessel/client interfaces.
- Belief folds use an empty hazard set; only `plan_to_water` receives the real hazard set.
- Equal route lengths use ascending `Facet`; unreachable candidates are excluded.
- Resolve source conflicts before any golden rebaseline; run client checks if a vessel type changes.
- Use focused local checks and `make gate-commit`; stage/merge gates run through the lefford sluice.

### Task 1: Complete measurement and observer inventory

**Status:** Complete — the direct two-room witness measured 2 home and 2
current searches; seed-42/17 integration constructors were documented as
unavailable to the unit probe.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — ignored Fetch probe beside the existing belief tests.
- Modify: `docs/superpowers/ledgers/2026-09-06-the-fetch.md` — measured output and observer ruling.

**Interfaces:**
- Consumes: `test_folds`, `commit_agent_at`, `agent_position`, `believed_water`, `plan_to_room`, and existing possession-shape constructors.
- Produces: a deterministic readout of admission/ranking disagreement and route-search counts; no production behavior change.

- [ ] **Step 1: Extend the ignored probe to use the existing seed-42 and seed-17 possession fixtures.** For each fixture, construct the same remembered-water candidates, read the committed current position, and report `remembered`, `home_admitted`, `current_admitted`, `home_ranked`, `current_ranked`, and the number of direct route searches.
- [ ] **Step 2: Run exactly the probe.** Use `cargo fmt` separately if needed, then run `cargo test -p hornvale-vessel --lib liveness::tests::fetch_probe_compares_home_and_current_water_decisions -- --ignored --nocapture`; capture the exit code and printed rows.
- [ ] **Step 3: Update ledger entry #3 with the exact output and classify each observer as unchanged, value-pinned, transcript, or byte-golden.** Do not infer generated movement from the probe.
- [ ] **Step 4: Run the focused probe again after the ledger edit and commit only the probe/ledger measurement.** Commit message: `test(the-fetch): measure current-position route shape`.

### Task 2: Add red behavior tests

**Status:** Complete — four tests compile and fail against the old home anchor.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — private unit tests next to the existing `believed_water` tests.

**Interfaces:**
- Consumes: Task 1's measured witness; existing `believed_water` and `nearer_to_home` call shapes.
- Produces: failing tests for current admission, ranking, tie-breaking, and incremental/re-derived agreement.

- [ ] **Step 1: Add `believed_water_admits_currently_reachable_memory` using two remembered water rooms where one is current-reachable but home-unreachable.** Assert the result is the current-reachable facet.
- [ ] **Step 2: Add `believed_water_ranks_reachable_memory_from_current_position` with two current-reachable candidates and assert the lower hop count wins.** Add an equal-hop case and assert ascending `Facet` wins.
- [ ] **Step 3: Add an incremental alignment test that commits a position change, updates the incremental belief, and compares it with fresh `believed_water` at the same `WorldTime`.**
- [ ] **Step 4: Run `cargo test -p hornvale-vessel --lib liveness::tests::believed_water_admits_currently_reachable_memory liveness::tests::believed_water_ranks_reachable_memory_from_current_position`.** Confirm the old home anchor fails the admission assertion before implementation.
- [ ] **Step 5: Commit the red tests.** Commit message: `test(the-fetch): pin actor-relative water belief`.

### Task 3: Implement the actor-relative fold

**Status:** Complete — implementation committed as `dca20b44c`; commit gate
passed all audits and 1,298 sub-floor tests.

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — `believed_water`, the private incremental helper, and their callers/docs.

**Interfaces:**
- Consumes: Task 2's failing tests; `agent_position`; `plan_to_room` with `BTreeSet::new()`.
- Produces: current-position admission/ranking with unchanged `Option<Facet>` API and aligned incremental behavior.

- [ ] **Step 1: Obtain `agent_position(ledger, npc, t)` before the route-ranking fold and retain the existing `LatestVisit::water_at` admission order.**
- [ ] **Step 2: Replace only the home reference in the fold's route query with the committed current position.** Keep `None` for unreachable candidates and `(hops, Facet)` ordering.
- [ ] **Step 3: Rename `nearer_to_home` to `nearer_to_current` and pass the same current position used by the tick's frozen read.**
- [ ] **Step 4: Stop passing `RouteMemo` to the position-varying fold path.** Preserve the type and all unrelated home-keyed callers; do not introduce a new cache.
- [ ] **Step 5: Update the function and call-site documentation to state that memory is allocentric and route choice is actor-relative.**
- [ ] **Step 6: Run all named Fetch behavior tests and the existing belief/nearer/shared belief test modules.** Confirm green before formatting.
- [ ] **Step 7: Commit the implementation.** Commit message: `fix(the-fetch): anchor remembered water at current position`.

### Task 4: Verify observers and artifacts

**Status:** Complete — focused belief, errand, affect, and seed-42 snapshot
observers pass; no value fixtures moved.

**Files:**
- Modify: affected value fixtures under `windows/vessel/tests/fixtures/` only when the reviewed diff proves they move.
- Modify: affected trace/golden documentation only when required by observed behavior.

**Interfaces:**
- Consumes: Task 3 implementation and Task 1 observer inventory.
- Produces: reviewed deterministic fixtures with no unrelated artifact drift.

- [ ] **Step 1: Run the focused vessel tests once with `--no-fail-fast` and save the output for inspection.**
- [ ] **Step 2: Run `make gate-commit` and retain its exit code and summary.** This covers format, clippy, audits, freshness checks, and the sub-floor roster.
- [ ] **Step 3: Run the specific affected golden/trace tests named by the failures; inspect each diff by source observer rather than accepting a blanket rebaseline.**
- [ ] **Step 4: If and only if the diff is limited to approved belief-dependent behavior, rebaseline with `REBASELINE=1`, review the byte diff, then rerun without the flag.**
- [ ] **Step 5: Run `make vessel-check` and `make clients-check` if any exported vessel type or constructor changed; otherwise record that the public type surface stayed unchanged.**
- [ ] **Step 6: Commit only reviewed fixture updates.** Commit message: `test(the-fetch): rebaseline actor-relative belief observers`.

### Task 5: Close preparation

**Status:** Ready for G6 — close records and focused documentation checks are
complete; stage or merge submission remains blocked on G6 review.

**Files:**
- Create: `book/src/chronicle/the-fetch.md` using the repository chronicle convention.
- Create: `docs/retrospectives/the-fetch.md` using the repository retrospective convention.
- Modify: `docs/superpowers/ledgers/2026-09-06-the-fetch.md` and this plan with final status/evidence.

**Interfaces:**
- Consumes: all green verification and the reviewed artifact disposition.
- Produces: G6 package ready for the canonical sluice; no direct merge from the Mac.

- [x] **Step 1: Record final test commands, artifact movement, epoch/schema result, and follow-ups in the ledger.**
- [x] **Step 2: Write the chronicle and retrospective from committed evidence, including the route-search measurement and the rejected cache alternative.**
- [x] **Step 3: Run focused documentation consistency checks.**
- [ ] **Step 4: Submit the full tested SHA through `make sluice-stage BRANCH=campaign/the-fetch REF="$(git rev-parse HEAD)"` at the stage boundary.**
- [ ] **Step 5: Stop for G6 review before any merge submission.
