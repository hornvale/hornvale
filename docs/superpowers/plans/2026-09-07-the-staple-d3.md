# The Staple D3 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Determine whether a relation-local return flow produces a differentiated D3 function, and only then implement the approved mechanism if the Task 0 bar survives.

**Architecture:** Add a zero-impact emitted diagnostic sidecar to the existing worldgen `History` result so the probe can observe a relation-local continuous return witness without expanding the save-facing `History::tribute` shape. The sidecar must not mutate stores, functions, population, epoch output, or D2 exchange behavior. If the probe is mixed, add the approved pure production return evaluation, clear all edges from a snapshot, and derive `Trade`, `Cult`, and `Fort` from that evaluation; if either dead pole fires, close D3 as a measured null or uniform rescale.

**Tech Stack:** Rust workspace; existing `windows/worldgen` tests; `cargo nextest`; deterministic `BTreeMap` ordering; sanctioned sluice census path only.

**Spec:** `docs/superpowers/specs/2026-09-07-the-staple-d3-design.md`

## Global Constraints

- Task 0 denominator is standing `History::tribute` relations at `now`; `N == 0` is a probe failure.
- D3 activation requires `0 < C < N`, where `C` counts only derived `Trade`, `Cult`, or `Fort`; existing derived `Mine` is excluded.
- The two dead poles are `C = 0` (sink) and `C = N` (uniform relabeling).
- Derived surfaces read continuous causes, never categorical labels; `Function` is an output.
- Same-epoch relation clearing uses a snapshot and is order-independent.
- Any committed-history change costs an epoch, a sanctioned census re-baseline, and conversion of history-adjacent pins into invariants.
- Do not run censuses locally; submit the sanctioned sluice path when authorized.
- No production return flow or function conversion begins before Task 0's
  measurement and its ledger ruling. The approved diagnostic sidecar is the
  exception: it exists solely to make Task 0 observable and must be zero-impact.

---

## Stage 1: Establish and run the diagnostic witness

**Goal:** Emit a zero-impact, relation-local diagnostic return witness and run Task 0 against it without changing D2 behavior or save-facing tribute records.

**Success Criteria:** The sidecar is deterministic, relation-local, emitted from the bake, excluded from save-facing tribute records, and proven not to mutate stores, functions, population, epoch output, or D2 exchange. The probe uses standing relations only, excludes `Function::Mine`, and fails closed on an empty denominator.

**Tests:** Existing `staple_d2_probe` remains green; a new ignored D3 probe test compiles against the current worldgen test harness.

**Status:** Not Started

### Task 1: Add the D3 probe scaffold

**Files:**
- Create: `windows/worldgen/tests/suite/staple_d3_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs` (register the new module following the existing D2 probe pattern)
- Modify: `docs/audits/campaign-reconciliation.tsv` only if the test is added as a direct campaign record

**Interfaces:**
- Consumes: the existing world-builder and bake test helpers used by `staple_d2_probe.rs`.
- Produces: an ignored, deterministic readout whose report schema contains `relations_total`, `d3_function_relations`, `d3_ratio`, `return_bands`, and `conservation_residuals`.

- [ ] Read `staple_d2_probe.rs` completely and copy its fixed seed/census harness shape without copying its treatment-only denominator.
- [ ] Write the ignored test and report structs first. Assert `relations_total > 0` before computing the ratio; assert the function count is over `{Trade, Cult, Fort}` only.
- [ ] Run the focused test once and capture the compile/test result in the ledger. If the current public API cannot expose the needed return witness, stop this task and record the boundary rather than adding an unapproved public field.
- [ ] Commit the scaffold and its ledger evidence.

### Task 2: Emit the diagnostic return witness and run Task 0

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` and the emitted `History`
  diagnostic type, keeping `History::tribute` save-facing shape unchanged
- Modify: `windows/worldgen/tests/suite/staple_d3_probe.rs`
- Test: focused worldgen bake tests for zero-impact and deterministic witness

**Interfaces:**
- Consumes: standing tribute relation state and continuous bake causes.
- Produces: one diagnostic record per standing relation at `now`, carrying the
  relation identity, protection/goods/legitimacy vector, magnitude bands, and
  explicit diagnostic classification.

- [ ] Define and record witness thresholds and precedence before coding; no
  silent conversion constants.
- [ ] Write failing tests proving the sidecar does not alter stores, function,
  population, epoch output, or D2 exchange results.
- [ ] Emit the sidecar without exposing endpoint labels as inputs and without
  adding it to the save-facing tribute relation records.
- [ ] Run the fixed 200-seed Task 0 probe once, inspect the report once, and
  record exact `N`, `C`, bands, and both dead-pole checks in the ledger.
- [ ] Commit the witness, probe result, and ledger evidence before deciding
  whether the mixed branch exists.

## Stage 2: Resolve the return model from the evidence

**Goal:** Turn the probe’s observed relation-level continuous causes into a precise, approved D3 mechanism without inventing conversion constants silently.

**Success Criteria:** The ledger records one of three outcomes: sink (`C = 0`), uniform relabeling (`C = N`), or mixed (`0 < C < N`) with a gradient/cliff classification. Only the mixed branch proceeds.

**Tests:** The probe has a mutation that changes a continuous return cause and changes classification; a categorical-label mutation does not manufacture activation.

### Task 3: Adjudicate the probe result

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-07-the-staple-d3.md`
- Modify: `docs/superpowers/specs/2026-09-07-the-staple-d3-design.md` if the mixed result requires a clarified return mapping

**Interfaces:**
- Consumes: Task 1’s fixed report and the approved Task 0 criterion.
- Produces: a ledger ruling and, only for the mixed branch, an explicit return vector and precedence contract for implementation.

- [ ] Record the exact `C/N` count and all return bands; do not replace measured counts with estimates.
- [ ] If `N == 0`, record a failed denominator and stop the campaign pending a fixture correction.
- [ ] If `C == 0`, record D3 as a measured sink and stop without production changes.
- [ ] If `C == N`, record D3 as a uniform relabeling and stop without production changes.
- [ ] If `0 < C < N`, specify the continuous component-to-function mapping, tie precedence, and conservation unit in the spec before implementation. Record rejected mappings and the second ideonomy pass in the ledger.
- [ ] Commit the ruling or amended spec.

## Stage 3: Implement the mixed branch only

**Goal:** Add a deterministic, relation-local return flow only if Stage 2 proves the mixed branch.

**Success Criteria:** Same-epoch clearing is conserved and order-independent; functions are derived from continuous return causes; D2 behavior remains intact when the D3 treatment is disabled.

**Tests:** Focused unit tests for return evaluation, snapshot clearing, conservation, order reversal, null `Agrarian`, and precedence; the integrated D3 probe; existing D2 probe and worldgen tests.

### Task 4: Implement return evaluation and clearing

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` near `Tribute`, `Community`, and `collect_tribute`
- Test: `windows/worldgen/src/history_bake.rs` unit-test module for pure clearing and ordering fixtures
- Modify: `windows/worldgen/tests/suite/staple_d3_probe.rs` to consume the implemented readout

**Interfaces:**
- Consumes: the exact return vector and precedence contract produced by Task 2.
- Produces: a private pure evaluator, a simultaneous application step, and the D3 probe report; public `History::tribute` expands only if the approved design requires emitted return facts.

- [ ] Write failing tests for the approved component mapping, nonnegative stores, conservation, reversed relation order, and null-to-`Agrarian` behavior.
- [ ] Implement the smallest pure evaluator over a snapshot of relation state; do not read stores already modified during the current pass.
- [ ] Apply all computed outflows simultaneously, then derive the D3 function from the continuous vector; leave `Mine` on its existing path.
- [ ] Run focused tests and the ignored D3 probe; inspect the report once and use its exit code as the result.
- [ ] Run `cargo fmt --check`, targeted clippy/tests, and `make gate-commit` before any expensive gate submission.
- [ ] Commit the implementation with the probe and ledger evidence.

## Stage 4: Epoch and campaign close preparation

**Goal:** Recalibrate every committed-history consequence before asking the sluice to test or merge the campaign.

**Success Criteria:** Epoch artifacts, census fixtures, and history-adjacent pins are explicitly re-baselined; no local census is run; the campaign is ready for G6 only after canonical verification.

**Tests:** Full local commit gate; sanctioned stage/merge gate through the sluice; census only through its queued canonical path.

### Task 5: Rebaseline and prepare canonical verification

**Files:**
- Modify: committed generated artifacts identified by the stage gate
- Modify: `docs/superpowers/ledgers/2026-09-07-the-staple-d3.md`
- Modify: `docs/superpowers/specs/2026-09-07-the-staple-d3-design.md` status and evidence sections

**Interfaces:**
- Consumes: the green mixed-branch implementation and its measured artifact diff.
- Produces: a complete G6 package for Nathan; no direct push or local census.

- [ ] Enumerate changed epoch artifacts and convert history-adjacent value pins into invariant assertions where the mechanism changes them.
- [ ] Set `REF=$(git rev-parse HEAD)`, verify it is the full SHA, then submit `make sluice-stage BRANCH=campaign/the-staple-d3 REF="$REF"`.
- [ ] If canonical verification requires a census refresh, queue `make sluice-census BRANCH=campaign/the-staple-d3 REF="$REF"` and do not run `scripts/census-run.sh` locally.
- [ ] Record canonical results, rejected alternatives, deferred minors, and follow-ups in the ledger.
- [ ] Stop at G6 for Nathan’s merge/close review; do not declare completion before that review.
