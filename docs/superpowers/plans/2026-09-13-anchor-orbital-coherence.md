# Anchor Orbital Coherence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make one explicit-time, typed anchor orbital evaluation authoritative for calendar, insolation, eclipse, and scene consumers.

**Architecture:** Add a public `AnchorState` semantic view and `OrbitalError` result at the astronomy boundary while keeping orbital elements, solver details, and compatibility projections private. Migrate each consumer to a named projection from that state, preserving deep-time forcing as a separate model.

**Tech Stack:** Rust workspace, `hornvale-astronomy`, `hornvale-scene`, `cargo nextest`, existing deterministic math and artifact gates.

**Spec:** `docs/superpowers/specs/2026-09-13-anchor-orbital-coherence-design.md`

## Global Constraints

- Every time-dependent astronomy query requires an explicit typed instant.
- Simulation ticks convert once at the simulation/window boundary; astronomy uses continuous typed time.
- Physical evaluation returns `Result<State, OrbitalError>`; valid-but-empty phenomena remain ordinary event results.
- No N-body integration, new phenomenon family, save-format facts, or silent fallback is part of this campaign.
- Preserve deterministic stream consumption and quantize only at existing emit boundaries.
- Run focused tests while iterating; finish with the repository's required local commit gate.

## File Map

- Modify `domains/astronomy/src/ephemeris.rs`: define the stable anchor state/error surface and keep solver internals private.
- Modify `domains/astronomy/src/calendar.rs`: consume the anchor state for solar phase and season projections.
- Modify `domains/astronomy/src/eclipses.rs`: consume shared anchor state for alignment and shadow geometry.
- Modify `domains/astronomy/src/illuminant.rs` or the existing insolation caller identified during Task 2: consume the shared radius without redefining climate forcing.
- Modify `windows/scene/src/astronomy_at.rs`: convert the explicit `WorldTime` once and project the shared state into scene geometry.
- Modify `domains/astronomy/tests/suite/` and `windows/scene/tests/suite/`: add consumer-independent coherence and failure tests.
- Modify generated artifacts only if focused tests prove an intentional output change; use the repository regeneration workflow.

---

## Stage 1: Physical Contract

**Goal:** Expose a stable typed anchor evaluation without changing consumer behavior yet.

**Success Criteria:** `anchor_state_at(system, instant)` returns a deterministic `Result<AnchorState, OrbitalError>`; invalid inputs and solver failure are descriptive; existing focused astronomy tests remain green.

**Tests:** finite and negative instants, deterministic repeated calls, invalid/non-finite parameters, unsupported validity intervals, degenerate periods, and solver residual/convergence behavior.

### Task 1: Add `AnchorState` and `OrbitalError`

**Files:**
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Create: `domains/astronomy/tests/suite/anchor_state.rs`
- Modify: `domains/astronomy/tests/suite.rs`

**Interfaces:**
- Produces `pub struct AnchorState` with explicit `instant`, frame/epoch metadata, position, velocity, radius, mean longitude, true longitude, and validity metadata.
- Produces `pub enum OrbitalError` with `InvalidInput`, `UnsupportedTime`, `DegenerateOrbit`, and `SolverDidNotConverge` cases, each carrying enough context for a diagnostic message.
- Produces `pub fn anchor_state_at(system: &StarSystem, instant: StdInstant) -> Result<AnchorState, OrbitalError>`.

- [ ] **Step 1: Write the failing contract tests.** Add tests that call the new public function and assert: identical inputs are equal; the returned instant is the requested instant; position/radius are finite for valid seed-42 instants; negative instants are evaluated rather than clamped; malformed elements produce the named error category.
- [ ] **Step 2: Run the focused tests to verify they fail.** Run `cargo nextest run -p hornvale-astronomy --test suite -E 'test(anchor_state_at)'`. Expected: compile failure because the new public types/function do not exist.
- [ ] **Step 3: Implement the minimal typed surface.** Refactor the existing private `anchor_orbital_state_at` path so it validates the explicit instant, evaluates the existing analytic elements, checks the Newton residual after the fixed iteration budget, and maps failures to `OrbitalError`. Keep `OrbitalElements`, `OrbitalFrame`, and anomaly internals private.
- [ ] **Step 4: Run the focused tests to verify they pass.** Run `cargo nextest run -p hornvale-astronomy --test suite -E 'test(anchor_state_at)'`; expected: PASS.
- [ ] **Step 5: Run formatting and the astronomy suite.** Run `cargo fmt --check` and `cargo nextest run -p hornvale-astronomy --test suite`; expected: PASS with no consumer migration failures.
- [ ] **Step 6: Commit.** Run `git add domains/astronomy/src/ephemeris.rs domains/astronomy/src/lib.rs domains/astronomy/tests/suite` and commit with `feat(astronomy): expose typed anchor orbital state`.

---

## Stage 2: Calendar and Insolation

**Goal:** Make calendar solar geometry and instantaneous insolation projections consume `AnchorState`.

**Success Criteria:** Calendar solar phase, anchor radius, and insolation agree with the same evaluated state at every tested instant; deep-time forcing remains semantically separate.

**Tests:** phase/radius agreement, negative-time agreement, locked/retrograde conventions, continuity across a phase wrap, and insolation equality against the state radius.

### Task 2: Migrate calendar and insolation projections

**Files:**
- Modify: `domains/astronomy/src/calendar.rs`
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `windows/scene/src/astronomy_at.rs`: consume the shared anchor
  position because it is the existing consumer of instantaneous illumination.
- Test: `domains/astronomy/tests/suite/sky_conformance.rs`
- Test: `domains/astronomy/tests/suite/calendar_negative_time.rs`
- Test: `windows/scene/tests/suite/astronomy_at.rs`

**Interfaces:**
- Consumes `anchor_state_at(&StarSystem, StdInstant) -> Result<AnchorState, OrbitalError>`.
- Produces named calendar projections that preserve current public return types where valid, but no longer reconstruct anchor position or radius independently.
- Leaves `OrbitalForcing` generation and deep-time parameters unchanged.

- [ ] **Step 1: Write failing coherence tests.** For representative systems and instants, compare calendar solar longitude to `AnchorState::true_longitude` after the documented conversion; compare instantaneous insolation to the state radius formula; include negative, locked, retrograde, and phase-wrap cases.
- [ ] **Step 2: Run the focused tests to verify the migration is absent.** Run `cargo nextest run -p hornvale-astronomy --test suite -E 'test(calendar_anchor_coherence) or test(insolation_anchor_coherence)'`. Expected: FAIL on the new assertions or compile failure for test-only helpers.
- [ ] **Step 3: Route calendar solar geometry through `anchor_state_at`.** Replace the calendar's private duplicate anchor evaluation with the shared result and translate `OrbitalError` at the existing calendar boundary without turning a valid no-event condition into an error.
- [ ] **Step 4: Route instantaneous insolation through the shared radius.** Preserve the existing luminosity and operation order unless the state contract requires a documented change; do not substitute the deep-time forcing envelope for instantaneous radius.
- [ ] **Step 5: Run focused tests and inspect one failure list.** Run `cargo nextest run -p hornvale-astronomy --test suite -E 'test(calendar_anchor_coherence) or test(insolation_anchor_coherence) or test(calendar_negative_time)'`; expected: PASS.
- [ ] **Step 6: Commit.** Run `git add domains/astronomy/src/calendar.rs domains/astronomy/src/ephemeris.rs windows/scene/src/astronomy_at.rs domains/astronomy/tests/suite windows/scene/tests/suite/astronomy_at.rs` and commit with `refactor(astronomy): share anchor state with calendar and insolation`.

---

## Stage 3: Eclipse and Scene Projections

**Goal:** Remove the remaining independent anchor geometry from eclipse and scene orientation consumers while preserving their public event and wire semantics.

**Success Criteria:** Eclipse alignment, shadow geometry, scene position, and surface orientation all derive from the same state; the scene still converts `WorldTime` exactly once at its boundary.

**Tests:** eclipse/calendar alignment, ground-track consistency, scene physical-position consistency, negative-time queries, locked/retrograde scenes, and valid no-eclipse results.

### Task 3: Migrate eclipse and scene consumers

**Files:**
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `windows/scene/src/astronomy_at.rs`
- Modify: `domains/astronomy/tests/suite/eclipse_rhythm_view.rs`
- Modify: `windows/scene/tests/suite/astronomy_at.rs`

**Interfaces:**
- Consumes the stable `AnchorState` result and named frame conversion helpers.
- Produces existing `EclipseEvent`, observer, and scene wire shapes unless an intentional compatibility epoch is approved.
- Preserves `astronomy_at_scene(world, at: WorldTime)` as the window-facing explicit-tick API.

- [ ] **Step 1: Write failing consumer-independence tests.** Add a test that obtains one `AnchorState`, derives expected anchor position/radius, and asserts eclipse alignment and scene geometry match it; add a valid no-eclipse case that returns an empty event set rather than `OrbitalError`.
- [ ] **Step 2: Run the focused tests to verify the old paths disagree or are unproven.** Run `cargo nextest run -p hornvale-astronomy --test suite -E 'test(eclipse_anchor_coherence)'` and the focused scene test command for the new scene assertion. Expected: FAIL until both consumers use the shared state.
- [ ] **Step 3: Migrate eclipse geometry.** Replace calls that reconstruct anchor position/radius with the shared state and map physical errors to the existing eclipse-query boundary; preserve node, moon, recurrence, and no-event semantics.
- [ ] **Step 4: Migrate scene geometry.** Keep the existing `WorldTime` to `StdInstant` conversion at `astronomy_at_scene_in`; pass the resulting instant to the state seam and derive position, orientation, and illumination from named projections.
- [ ] **Step 5: Run focused astronomy and scene tests.** Run the astronomy eclipse suite and the scene astronomy-at suite; expected: PASS.
- [ ] **Step 6: Commit.** Commit `domains/astronomy/src/eclipses.rs`, `windows/scene/src/astronomy_at.rs`, `domains/astronomy/tests/suite/eclipse_rhythm_view.rs`, and `windows/scene/tests/suite/astronomy_at.rs` with `refactor(astronomy): share anchor state with eclipse and scene`.

---

## Stage 4: Qualification and Closure

**Goal:** Prove semantic coherence, determinism, and compatibility, then update only artifacts demonstrated to move.

**Success Criteria:** The full coherence battery passes; intentional output changes are identified and regenerated; docs and audit records describe the shipped boundary.

**Tests:** cross-consumer mutation tripwire, reordered-query determinism, invalid-input diagnostics, bounded historical sweep, full astronomy/scene focused suites, and the local commit gate.

### Task 4: Add the coherence battery and close artifacts

**Files:**
- Create or modify: `domains/astronomy/tests/suite/anchor_coherence.rs`
- Modify: `windows/scene/tests/suite/` relevant astronomy test file
- Modify: `docs/superpowers/ledgers/2026-09-13-anchor-orbital-coherence.md`
- Modify: generated files only when regeneration produces a verified intentional diff

**Interfaces:**
- Consumes all public projections created by Tasks 1–3.
- Produces a deterministic, consumer-independent qualification suite and a documented artifact decision.

- [ ] **Step 1: Write the failing qualification battery.** Cover single-source agreement, explicit-time preservation, frame conversions, determinism under reordered queries, continuity around wraps, negative and bounded future instants, locked/retrograde cases, malformed inputs, out-of-window errors, and valid empty event results.
- [ ] **Step 2: Add the mutation tripwire.** In the test harness, use a deliberately perturbed private projection or a test-only alternate conversion and assert the coherence test detects the disagreement; ensure the mutation changes the target before relying on a red result.
- [ ] **Step 3: Run the battery and record the complete failure list.** Run the focused astronomy and scene suites once with no fail-fast; expected: failures identify remaining consumer drift or contract gaps.
- [ ] **Step 4: Fix only the identified drift.** Keep each fix in the owning consumer or contract file; do not add a generalized multi-body abstraction in this campaign.
- [ ] **Step 5: Verify artifacts.** Run the repository's documented regeneration command for any affected generated output, inspect the resulting diff, and run the relevant golden tests. If no generated output changes, record that result rather than forcing a rebaseline.
- [ ] **Step 6: Update the ledger with measured outcomes.** Record final test counts, any intentional byte changes, and deferred multi-body follow-ups in the committed campaign ledger.
- [ ] **Step 7: Run final verification.** Run `cargo fmt --check`, the required workspace lint/type/placement/plumb checks, the sub-floor test command, and the local commit gate; trust exit codes and do not rerun suites merely to extract a second line.
- [ ] **Step 8: Commit.** Commit the final tests, docs, and verified artifacts with `test(astronomy): qualify anchor orbital coherence`.

## Handoff

After this plan is approved, execute it task-by-task with
`superpowers:subagent-driven-development`, using a fresh implementer and
review checkpoint for each task. The plan is complete when the final local
gate is green and the campaign is submitted to the Sluice for its stage/merge
decision; it is not complete merely when the code compiles.
