# The Staple R3 — Districts Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement a pure, deterministic, cohort-first district projection over typed relation views, with explicit temporal states, refusal behavior, and an optional Pattern-composition adapter.

**Architecture:** Add a narrow relation-assertion envelope at the worldgen/population-facing composition boundary, keep source facts and event history authoritative, and implement R3 as a pure projection module over basis-specific relation views. Use existing topology and Pattern conventions; do not add a kernel graph, a universal social graph, or any epoch-bearing mechanism.

**Tech Stack:** Rust workspace, `hornvale-kernel` value types including `WorldTime`, `hornvale-topology` deterministic graph primitives, `windows/worldgen` composition-root modules, consolidated worldgen integration tests, and existing Pattern composition APIs.

**Spec:** `docs/superpowers/specs/2026-09-08-the-staple-r3-design.md`

## Global Constraints

- Preserve the Murrain boundary: the cohort/population substrate is authoritative; projections never become population truth.
- Keep the R3 projection pure: no seed draws, wall-clock reads, epochs, ledger writes, or mutation of source facts.
- Keep relation meaning owned by its producer; R3 only consumes supported spatial, presence, access, and exchange kinds.
- Preserve directedness, intervals, recurrence, provenance, and basis-specific measures.
- Do not implement households, kinship, partnership, gender, inheritance, biography, cultural institutions, or a general agent scheduler.
- Do not add a universal graph to `kernel` or create sibling-domain dependencies.
- Use deterministic ordered collections and tie-breaks; do not use unordered iteration to define output.
- Add behavior tests before implementation and run targeted formatting, clippy, and nextest checks after each task.

## File map

- `windows/worldgen/src/relation.rs` — shared role-bearing relation assertion envelope and validation.
- `windows/worldgen/src/district.rs` — R3 basis-specific projection, result states, continuity, and deterministic district output.
- `windows/worldgen/src/lib.rs` — expose the two new modules and public readout types at the composition root.
- `windows/worldgen/tests/suite/relation.rs` — envelope validation and deterministic ordering tests.
- `windows/worldgen/tests/suite/district.rs` — synthetic R3 projection cases and invariants.
- `windows/worldgen/tests/suite.rs` — register the two consolidated integration-test modules.
- `docs/superpowers/specs/2026-09-08-the-staple-r3-design.md` — approved design contract.
- `docs/superpowers/plans/2026-09-08-the-staple-r3.md` — staged implementation contract.

### Task 1: Add the relation assertion envelope

**Files:**
- Create: `windows/worldgen/src/relation.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Create: `windows/worldgen/tests/suite/relation.rs`
- Modify: `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Produce public `RelationReference`, `RelationRole`, `RelationInterval { start: WorldTime, end: WorldTime }`, `RelationRecurrence`, `RelationDirection`, `RelationMeasure`, and `RelationProvenance` value types.
- Produce a public `RelationKind` limited initially to `SpatialAdjacency`, `Presence`, `Access`, and `Exchange`.
- Produce public `RelationParticipant { reference: RelationReference, role: RelationRole }`, `RelationAssertion { kind, participants, interval: RelationInterval, recurrence, direction, measure, provenance }`, and `RelationView` with deterministic iteration.
- Produce `RelationAssertion::validate() -> Result<(), RelationError>` and a view constructor that rejects malformed participant counts, reversed `RelationInterval` values, and non-finite measures.
- Keep endpoint/reference types opaque enough to represent loci and aggregate cohorts without importing household or species semantics.

- [ ] Write failing tests for valid binary spatial/access/exchange assertions and aggregate presence assertions.
- [ ] Write failing tests for role-bearing higher-arity assertions being representable but not silently lowered by the relation envelope.
- [ ] Write failing tests for invalid intervals, non-finite measures, unsupported direction combinations, and nondeterministic participant ordering.
- [ ] Run `cargo nextest run -p hornvale-worldgen --test suite -E 'test(relation::)'` and confirm the new tests fail for missing types or validation.
- [ ] Implement the smallest typed envelope and deterministic `RelationView` ordering.
- [ ] Run `cargo fmt --check` and `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`.
- [ ] Run the relation test subset again and commit `feat(worldgen): add typed relation assertion envelope`.

### Task 2: Implement basis-specific relation views

**Files:**
- Modify: `windows/worldgen/src/relation.rs`
- Modify: `windows/worldgen/tests/suite/relation.rs`

**Interfaces:**
- Produce pure filters for spatial, presence, access, and exchange views.
- Produce explicit direction policy types for symmetric, weakly connected, strongly connected, source-reachable, and reciprocal relations.
- Produce a basis-local measure filter; never compare measures from different bases.
- Preserve the original assertion as provenance when a higher-arity assertion is not supported by an R3 view.

- [ ] Add failing tests proving spatial adjacency is symmetric by default while access and exchange preserve direction.
- [ ] Add failing tests for weak versus strong directed connectivity selection.
- [ ] Add failing tests proving distant exchange can remain connected in the exchange view without becoming spatial adjacency.
- [ ] Add failing tests proving unsupported relation kinds and unsupported arities produce explicit refusal metadata rather than pairwise fabrication.
- [ ] Implement deterministic basis views using existing ordered topology conventions.
- [ ] Run targeted relation tests, formatting, and clippy; commit `feat(worldgen): derive basis-specific relation views`.

### Task 3: Implement pure district projection

**Files:**
- Create: `windows/worldgen/src/district.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Create: `windows/worldgen/tests/suite/district.rs`
- Modify: `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Produce `DistrictInterval { start: WorldTime, end: WorldTime }`, `DistrictBasis`, `DistrictConfig`, `DistrictStatus`, `DistrictProjection`, and `DistrictProjectionSet`.
- Produce `project_districts(view: &RelationView, basis: DistrictBasis, interval: DistrictInterval, config: &DistrictConfig) -> DistrictProjectionSet`.
- Produce deterministic membership, boundary, bridge, overlap, and bounded-containment output.
- Produce projection-local identity derived from basis, interval, and canonical anchors; never from random draws or mutable IDs.

- [ ] Add failing synthetic tests for The Row, The Fork, The Reach, The Gate, and The Weave.
- [ ] Add failing tests for singleton refusal, false-bridge separation, bounded acyclic nesting, and explicit overlap.
- [ ] Add failing determinism tests that run the same view/configuration repeatedly and compare complete output.
- [ ] Add failing purity tests proving projection does not consume seed streams, read wall time, mutate source assertions, or write facts.
- [ ] Implement ordered connected-component and directed-reachability projection primitives.
- [ ] Implement explicit `resolved`, `insufficient_evidence`, `contradictory_evidence`, `disconnected`, and `transient_only` statuses.
- [ ] Run the district test subset, formatting, and clippy; commit `feat(worldgen): project evidence-backed districts`.

### Task 4: Add interval, recurrence, dissolution, and recomposition

**Files:**
- Modify: `windows/worldgen/src/district.rs`
- Modify: `windows/worldgen/tests/suite/district.rs`

**Interfaces:**
- Produce `DistrictContinuity` with event continuity, recurrence, transient, dissolved, and recomposed states.
- Produce `compare_districts(previous: &DistrictProjectionSet, current: &DistrictProjectionSet, config: &DistrictConfig) -> DistrictContinuity`.
- Keep fuzzy structural similarity diagnostic-only; it must not establish identity.

- [ ] Add failing tests for The Ring’s recurring seasonal windows.
- [ ] Add failing tests for The Drift’s dissolution and successor projections.
- [ ] Add failing tests proving a one-period candidate is transient or refused according to configured support.
- [ ] Add failing tests proving overlapping member sets do not automatically imply district identity.
- [ ] Implement interval selection, recurrence matching, and explicit continuity relations with stable tie-breaks.
- [ ] Run targeted tests, formatting, and clippy; commit `feat(worldgen): distinguish district continuity and recurrence`.

### Task 5: Add the Pattern composition boundary and cohort readout

**Files:**
- Modify: `windows/worldgen/src/district.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `windows/worldgen/tests/suite/district.rs`

**Interfaces:**
- Produce `compose_district_patterns(projections: &DistrictProjectionSet, config: &DistrictConfig) -> Result<PatternComposition, DistrictStatus>` that accepts only `resolved` district projections.
- Refuse Pattern composition for unsupported, contradictory, or transient-only projections unless configuration explicitly permits a transient readout.
- Preserve aggregate cohort membership without expanding it into persons, households, or biographies.

- [ ] Add failing tests for The Grain: cohort-only input yields aggregate districts and no realized-person facts.
- [ ] Add failing tests proving Pattern composition cannot create a district rejected by the relation projection.
- [ ] Add failing tests for nested district composition and unsupported basis refusal.
- [ ] Implement the smallest adapter over existing `Pattern`, `Attach`, `compose`, and `permits` machinery inside `district.rs`.
- [ ] Run all R3 integration tests plus relevant existing Pattern and graph tests.
- [ ] Commit `feat(worldgen): expose district projections to pattern composition`.

### Task 6: Add campaign probes and documentation readouts

**Files:**
- Modify: `windows/worldgen/tests/suite/district.rs`
- Modify: `book/src/chronicle/the-staple.md` with the R3 chronicle entry at campaign close.
- Modify: `docs/retrospectives/the-staple.md` with the R3 retrospective entry at campaign close.
- Modify: `docs/superpowers/ledgers/2026-09-04-the-staple.md` with the R3 decision entries at campaign close.
- Modify: `book/src/frontier/idea-registry.md` to route `SOC-staple-ladder` to the landed R3 evidence at campaign close.

**Interfaces:**
- Freeze synthetic probe names and expected statuses as test data, not world canon.
- Keep all probe output deterministic and independent of existing species assignments.

- [ ] Add The Hollow, The Flicker, and The Higher-Arity Relation probes.
- [ ] Add a probe summary showing each R3 status is reachable and intentional.
- [ ] Run `cargo nextest run -p hornvale-worldgen --test suite` and doctests relevant to the changed modules.
- [ ] Run `cargo fmt --check`, clippy, type-audit, placement-audit, plumb, and the repository commit gate before stage submission.
- [ ] Update the existing Staple chronicle, retrospective, ledger, and idea-registry row only after the implementation evidence exists.
- [ ] Commit the probe/readout changes separately from the implementation and campaign-close documentation.

## Verification gates

- Every task has a red test before implementation and a targeted green test after implementation.
- R3 remains a pure reading campaign: no epoch, census rebasing, ledger write, or seed stream consumption.
- The full workspace stage gate belongs at the campaign stage boundary; do not substitute repeated local full gates for targeted iteration.
- Generated artifacts touched by the final integration surface must be regenerated through the repository’s canonical process rather than hand-merged.

## Handoff

This plan is intentionally implementation-ready but does not authorize execution by itself. Execution must use `superpowers:subagent-driven-development` or `superpowers:executing-plans`, with the campaign worktree and sluice stage gates handled according to the repository instructions.
