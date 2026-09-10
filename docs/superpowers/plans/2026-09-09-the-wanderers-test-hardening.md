# The Wanderers Test Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task.

**Goal:** Replace four worldgen-coupled test assumptions exposed by The Wanderers with deterministic behavioral fixtures and structural assertions, without changing production behavior or re-pinning incidental generated values.

**Architecture:** Keep the generated-world integration coverage, but move each assertion that protects a local invariant behind the smallest stable seam: a direct genesis observation fixture, a constructed emitter witness, and current-walk errand/rendering observations. Preserve separate integration smoke where it still adds value.

**Tech Stack:** Rust workspace, hornvale-worldgen, hornvale-vessel, nextest, existing test helpers and fixture conventions.

**Spec:** docs/superpowers/specs/2026-09-09-the-wanderers-test-hardening-design.md

## Global Constraints

- Write or strengthen the focused test before changing its helper or production-adjacent test setup.
- Do not change Wanderers generation, stellar topology, anchor admission, or vessel production code to preserve old sample populations.
- Do not replace exact values with new exact values merely to make the held gate green.
- Keep anti-vacuity assertions: every test must still prove that the behavior under test was reached.
- Prefer existing fixture builders and test seams; introduce no new dependency or broad seed sweep.
- Run the narrowest focused test after each task, then the relevant crate suite.
- Every task ends with a focused commit; do not use --no-verify.

## Stage 1: Harden the genesis sky contract

**Goal:** Test the unoccluded genesis observation path and its fact-family relationship without pinning the seed-42 generated pantheon size.

**Files:**

- Modify: windows/worldgen/src/lib.rs
- Test: the existing genesis_observes_an_unoccluded_sky test and nearby helpers

**Steps:**

1. Inspect the existing genesis observation and presentation-observation helpers and identify the smallest seam that can distinguish the unoccluded path from an occluded presentation read.
2. Write a focused failing assertion using a controlled observation fixture or a structural comparison of the emitted belief families.
3. Remove only the incidental exact seed-42 counts from the genesis regression; retain non-empty and relationship assertions that prove genesis observed the intended sky.
4. Run the focused worldgen test red/green, then the worldgen library suite.
5. Commit: test(worldgen): harden genesis sky observation witness

**Success criteria:** The test fails if genesis routes through presentation occlusion, passes with The Wanderers world, and does not depend on the old 145-count population.

## Stage 2: Give the detent an explicit emitter witness

**Goal:** Make the rule-four timeline-copy test construct a shape that necessarily reaches an emitter scan.

**Files:**

- Modify: windows/vessel/tests/suite/the_detent.rs
- Reuse or modify: existing vessel test fixture helpers under windows/vessel/tests/suite/

**Steps:**

1. Trace bench_shape, emitter discovery, and the rule-four assertion to identify the narrowest injectable witness already supported by the vessel test harness.
2. Write a failing fixture-level test or setup assertion that proves the constructed shape contains at least one emitter before the timeline-copy assertion runs.
3. Replace the lucky full-world seed dependency for the rule-four witness with that deterministic fixture, preserving the reachability floor and copied timeline assertion.
4. Run the focused detent test and the vessel suite.
5. Commit: test(vessel): make detent emitter witness deterministic

**Success criteria:** The test exercises at least one emitter on every run, remains sensitive to timeline-copy regressions, and does not rely on seed 28's generated population.

## Stage 3: Separate warrant behavior from generated population

**Goal:** Make warrant tests verify errand registration, producer provenance, step coverage, and rendering for the current walk population rather than treating a historical subject count as the errand contract.

**Files:**

- Modify: windows/vessel/tests/suite/the_warrant.rs
- Reuse or modify: existing walk/fixture helpers in windows/vessel/tests/suite/

**Steps:**

1. Write focused assertions that enumerate the registered errand keys and producer provenance from the current walk result.
2. Replace the current-walk equality against the frozen before-image population with a structural assertion appropriate to the live walk, retaining a separate check for non-empty reachability.
3. Change the rendered recount witness to assert that reached errands produce their registered step/provenance lines, rather than requiring a fixed number of position lines from seed 23.
4. Run each previously failing warrant test red/green, then the full vessel suite.
5. Commit: test(vessel): decouple warrant witnesses from world population

**Success criteria:** The tests fail when errand keys, producer provenance, step coverage, or rendering regress; they do not fail merely because astronomy changes the number of residents reached by the walk.

## Stage 4: Regression verification and resubmission

**Goal:** Verify the hardening as a coherent change and submit the corrected branch to the Sluice.

**Files:**

- Modify: docs/audits/campaign-reconciliation.tsv only if plan status/evidence needs a final update
- Modify: IMPLEMENTATION_PLAN.md only if the repository's active campaign plan requires a status handoff

**Steps:**

1. Run the four formerly failing tests individually and confirm each passes for the campaign branch.
2. Run the complete hornvale-worldgen and hornvale-vessel suites.
3. Run make gate-commit and inspect the final diff/status.
4. Update the campaign audit evidence from “implementation plan pending” to the actual verification state.
5. Push the new full SHA and submit make sluice BRANCH=campaign/the-wanderers REF=<full-sha> to supersede the held request.
6. Read the resulting Sluice status/log; do not declare completion until the canonical merge product is green.

**Success criteria:** Focused tests, crate suites, and local commit gate pass; the new Sluice request names the verified SHA and supersedes the held request without re-pinning any generated-world output.
