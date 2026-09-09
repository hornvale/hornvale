# The Staple D5 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a read-only Task 0 probe that determines whether existing D2/D4
flows produce a recurrent, comparatively convergent settlement apex.

**Architecture:** Extend `hornvale-worldgen` with pure D5 value types and
comparison helpers that consume already-produced D4 portfolio witnesses and
typed relation summaries. Add an ignored integration probe in the consolidated
worldgen suite that joins live settlements, preserves per-seed denominators,
and reports the preregistered branch table. No bake behavior, stream, save
format, census fixture, or city state changes.

**Tech Stack:** Rust workspace, `hornvale-worldgen`, deterministic ordered
collections, existing D4 portfolio types, consolidated nextest integration
suite, local `make gate-commit`.

**Spec:** `docs/superpowers/specs/2026-09-09-the-staple-d5-design.md`

## Global Constraints

- Count one alive settlement at the observation instant.
- Evaluate each seed independently; pooled totals are descriptive only.
- Preserve raw typed flow vectors, directions, sources, destinations, phase identity, and provenance.
- Keep population, density, throughput, catchment size, settlement age, and relation count as controls, never definitions.
- Distinguish voluntary, coercive, protection-mediated, and imposed flows.
- Require multiple adequate settlements and multiple regimes for a positive candidate.
- Preserve complete-window and recurrence rules; do not adapt the evidence horizon after measurement.
- Explicitly report empty, missing, duplicate, malformed, isolated, zero, disabled, and incomplete branches.
- Do not add a city label, `Seat`, `Notability`, `Function`, specialization state, production rule, movement rule, epoch, or census re-baseline.
- Keep the fixed-roster probe ignored and run it only through the sanctioned campaign boundary.
- Every Rust task ends with focused tests and a commit; run `make gate-commit` before stage submission.

## File Map

- `windows/worldgen/src/d5.rs` owns pure D5 profile, convergence, recurrence,
  control, and verdict types. It does not build worlds or read files.
- `windows/worldgen/src/lib.rs` declares `d5` and re-exports its public
  diagnostic interfaces.
- `windows/worldgen/tests/suite/staple_d5_probe.rs` owns deterministic test
  fixtures, live joins, per-seed reduction, and the ignored fixed-roster
  readout.
- `windows/worldgen/tests/suite.rs` registers the D5 probe module.
- `docs/superpowers/ledgers/2026-09-09-the-staple-d5.md` records task rulings,
  findings, and measurement results as they occur.

### Task 1: Define pure comparative-apex types

**Files:**
- Create: `windows/worldgen/src/d5.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Test: unit tests in `windows/worldgen/src/d5.rs`

**Interfaces:**
- `D5FlowVector` stores ordered inbound/outbound typed magnitudes and source
  counts without collapsing flow kinds.
- `D5ControlValues` stores population, density, throughput, catchment size,
  settlement age, and relation count.
- `D5SettlementProfile` stores settlement identity, raw flow vector, controls,
  phase records, and voluntary/coercive/protection provenance.
- `D5ConvergenceEvidence` stores source diversity, type diversity, directional
  balance, peer rank, recurrence, and explicit adequacy/refusal branches.
- `D5ApexVerdict` contains `NoRealizedApex`, `MeasurementCollapse`,
  `QualifiedFailure`, `TransientApex`, `SeasonalApexCandidate`,
  `PersistentApexCandidate`, and `MixedOrUnderpowered`.
- Pure functions are `d5_convergence_evidence`, `d5_compare_peers`, and
  `d5_apex_verdict`; each accepts prepared observations and returns a value
  without world access, RNG, stream consumption, or mutation.

- [ ] **Step 1: Write failing unit tests for typed flow separation.**

  Add fixture constructors and tests proving that two profiles with equal
  throughput but different source/type composition remain distinguishable;
  coercive flow cannot satisfy voluntary-flow evidence; raw vectors survive
  normalization; and zero or non-finite inputs become explicit inadequate
  branches.

- [ ] **Step 2: Run the focused tests to verify failure.**

  Run: `cargo test -p hornvale-worldgen d5 --lib`

  Expected: FAIL because `d5.rs` and the D5 interfaces do not yet exist.

- [ ] **Step 3: Implement the minimal pure data model.**

  Use fixed typed arrays and `BTreeMap`/ordered vectors where a variable set
  of source or flow identities is required. Preserve raw values beside every
  derived view. Return explicit `Incomplete`, `Unavailable`, or `Malformed`
  evidence rather than treating missing channels as zero.

- [ ] **Step 4: Add peer-comparison and control tests.**

  Prove that a large isolated settlement, a one-type high-throughput hub, and
  an old settlement with no inbound diversity do not pass merely because a
  control is large. Prove that a profile with multiple typed inbound sources
  can produce convergence evidence when its peer comparison is adequate.

- [ ] **Step 5: Run tests and commit.**

  Run: `cargo test -p hornvale-worldgen d5 --lib && cargo fmt --check`

  Expected: PASS.

  Commit: `git add windows/worldgen/src/d5.rs windows/worldgen/src/lib.rs && git commit -m "feat(staple-d5): define comparative apex diagnostics"`

### Task 2: Build the per-seed D5 probe

**Files:**
- Create: `windows/worldgen/tests/suite/staple_d5_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs`
- Modify: `docs/superpowers/ledgers/2026-09-09-the-staple-d5.md`

**Interfaces:**
- The probe consumes the existing worldgen build and D4 portfolio witness;
  it must not add a new bake path.
- `settlement_profiles(world)` returns profiles sorted by stable settlement
  identity and retains every raw typed phase record.
- `compare_seed(seed)` returns one per-seed `D5ApexVerdict` plus counts for
  adequate, incomplete, isolated, coerced, and underpowered units.
- The ignored fixed-roster test reports the same structure for the frozen
  probe seeds and never writes census or save artifacts.

- [ ] **Step 1: Write failing fixture tests for every verdict branch.**

  Cover no convergence, control-variable collapse, coercion-only apparent
  prominence, transient contrast, same-phase recurrence, cross-phase
  recurrence, mixed seeds, missing joins, isolated settlements, zero-flow
  profiles, and a positive fixture with multiple adequate peer settlements.

- [ ] **Step 2: Run the focused probe tests to verify failure.**

  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d5_probe)'`

  Expected: FAIL until the probe module and D5 interfaces are implemented.

- [ ] **Step 3: Implement deterministic live joins.**

  Build the existing world at the probe seed, enumerate alive settlements,
  join each settlement to its same-run D4 witness exactly once, and retain
  explicit refusal records for missing or duplicate identities. Sort before
  reduction and never weight a settlement by population, degree, or flow
  volume when assigning the verdict.

- [ ] **Step 4: Implement recurrence and discrimination reduction.**

  Compare same-phase windows before cross-phase aggregates. Keep raw and
  normalized profiles, controls, flow provenance, and recurrence class in the
  report. Apply the spec’s branch table without inventing an absolute city
  threshold or collapsing all flows into one score.

- [ ] **Step 5: Add the ignored fixed-roster readout.**

  Mark the real-world report with the repository’s existing ignored-probe
  convention and a reason naming the D5 spec and sanctioned boundary. Assert
  only structural invariants in the non-ignored path; print measurements in the
  ignored path rather than pinning them before the evidence exists.

- [ ] **Step 6: Run focused non-roster tests and commit.**

  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d5_probe)'`

  Expected: PASS for all non-ignored D5 tests; the fixed-roster test remains
  skipped.

  Commit: `git add windows/worldgen/tests/suite/staple_d5_probe.rs windows/worldgen/tests/suite.rs docs/superpowers/ledgers/2026-09-09-the-staple-d5.md && git commit -m "test(staple-d5): add comparative apex probe"`

### Task 3: Verify the save-inert and attribution boundaries

**Files:**
- Modify: `windows/worldgen/tests/suite/staple_d5_probe.rs`
- Modify: `windows/worldgen/src/d5.rs`
- Modify: `docs/superpowers/ledgers/2026-09-09-the-staple-d5.md`

- [ ] **Step 1: Add mutation-oriented vacuity tests.**

  Use controlled fixtures or source-level mutation helpers to demonstrate that
  removing source diversity, phase identity, or coercive-flow separation moves
  the relevant verdict or refusal branch. Assert the mutation target is found
  before applying it so a no-op mutation cannot report green evidence.

- [ ] **Step 2: Add save-inert regression checks.**

  Compare the existing emitted history/save-facing fields before and after D5
  reduction. The D5 diagnostic may read D4 evidence but must not change emitted
  bytes, stream positions, or existing D2 behavior.

- [ ] **Step 3: Add attribution and control witnesses.**

  Verify that the report names control-variable collapse separately from
  convergence, reports coercion as qualified evidence, and refuses causal
  wording when only association is available.

- [ ] **Step 4: Run the focused suite and commit.**

  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d5_probe)'`

  Expected: PASS with the fixed-roster measurement still ignored.

  Commit: `git add windows/worldgen/src/d5.rs windows/worldgen/tests/suite/staple_d5_probe.rs docs/superpowers/ledgers/2026-09-09-the-staple-d5.md && git commit -m "test(staple-d5): guard attribution and save boundaries"`

### Task 4: Local gate and campaign handoff

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-09-the-staple-d5.md`

- [ ] **Step 1: Run the complete focused D5/D4 compatibility suite.**

  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d5_probe) or test(staple_d4_probe)'`

  Expected: all selected non-ignored tests pass.

- [ ] **Step 2: Run the local commit gate.**

  Run: `make gate-commit`

  Expected: formatting, clippy, audits, freshness checks, and the sub-floor
  tier pass. Do not run the fixed-roster probe or census locally.

- [ ] **Step 3: Record the implementation review.**

  Add the observed test result, any axis debt, and any review findings to the
  committed D5 ledger. Confirm that the implementation matches every verdict
  branch and that no city mechanism or save-facing fact slipped into the tree.

- [ ] **Step 4: Stop at the campaign boundary.**

  Do not submit a stage gate, run the fixed-roster measurement, regenerate a
  census, merge, or close the campaign from this plan. Those actions require
  the campaign’s next gate decision and the G6 close review.

## Plan self-review

- The spec’s purpose, counted unit, existing evidence path, controls,
  discrimination cases, temporal rules, verdicts, attribution rules, and
  evidence package are covered by Tasks 1–4.
- The plan introduces no new persistence, stream draw, save field, epoch, or
  census artifact.
- Every code-facing interface is named before a later task consumes it.
- There are no TODO/TBD placeholders or unbounded “handle edge cases” steps.
