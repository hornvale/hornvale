# Population Layers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make historical populations, present substrate populations, and player-facing projections explicit and prevent the delve witness from conflating them.

**Architecture:** Add a small read-only worldgen population census over committed occupation records. It reports record-level and occupied-column counts for historical and present layers without changing epidemic behavior. Reuse the existing Lot `Projection` metadata as the projection layer, and add a contract test showing that composite/materialized projections carry provenance without redefining substrate population.

**Tech Stack:** Rust workspace, `cargo test`, nextest-compatible integration suites, existing `OccupationRecord` and Lot `Projection` types.

**Spec:** `docs/superpowers/specs/2026-09-06-the-murrain-design.md`, plus the approved population-layer design from this campaign turn.

## Global Constraints

- The epidemic model remains unchanged in this slice; this is a measurement and naming correction, not post-unblinding parameter retuning.
- Historical occupation records include ended occupations; present substrate counts include only `is_alive()` records.
- Column counts are sets of `core.site` values derived from the selected occupation layer.
- Projection metadata remains non-causal for analytical and composite cases, and causal only where its existing `Projection::consequences_write_back()` contract permits it.
- Preserve the protected unstaged `docs/timings.md` change.

### Task 1: Add the population census readout

**Files:**
- Create: `windows/worldgen/src/population.rs`
- Modify: `windows/worldgen/src/lib.rs` to expose the module and public readout
- Test: `windows/worldgen/tests/suite/population_layers.rs`

**Interfaces:**
- Consumes: `history_emit::occupation_records(&World)` and `OccupationRecord::core.site` / `is_alive()`.
- Produces: `PopulationCensus`, with `historical_occupations`, `present_occupations`, `historical_columns`, and `present_columns`, plus `population_census(&World) -> PopulationCensus`.

- [ ] Write a failing integration test that builds seed 42, calls `population_census`, asserts historical counts include at least one ended occupation, present counts are no greater than historical counts, and both column sets are non-empty.
- [ ] Run the focused test and confirm it fails because the module and function do not exist.
- [ ] Implement the smallest deterministic census using one occupation-record scan and `BTreeSet<Vertex>` for columns; do not introduce a second history decoder.
- [ ] Run the focused test and confirm it passes.
- [ ] Add unit-level assertions for the layer invariant: every present occupation is historical, and every present column is historical.
- [ ] Run `cargo test -p hornvale-worldgen --test suite -- population_layers`.
- [ ] Commit as `feat(worldgen): name historical and present population layers`.

### Task 2: Reconcile the delve witness

**Files:**
- Modify: `windows/worldgen/src/delve_seating.rs`
- Modify: `windows/worldgen/tests/suite/delve_depth.rs` or the existing worldgen suite file only if the new census needs an integration assertion

**Interfaces:**
- Consumes: `population_census(&world)` from Task 1 and the existing grouped occupation records.
- Produces: a delve witness whose count and failure message explicitly name the measured layer.

- [ ] Write or update the focused witness assertion so its expected value is described as the measured historical occupied-underworld-column count, not an unqualified settled-column count.
- [ ] Run the focused delve test and verify the failure, if any, identifies the layer rather than a generic stale number.
- [ ] Replace only the ambiguous terminology/provenance; preserve the one-seat-per-column and tenancy assertions.
- [ ] Run the focused delve test and confirm it passes with the measured layer name.
- [ ] Commit as `test(the-murrain): label delve population witness layer`.

### Task 3: Lock the substrate/projection boundary

**Files:**
- Modify: `windows/lot/tests/suite/murrain.rs`
- Modify: `windows/lot/src/projection.rs` only if a missing assertion requires a narrowly scoped constructor/documentation change

**Interfaces:**
- Consumes: existing `Projection`, `ProjectionMateriality`, `SourceCohort`, and worldgen population census.
- Produces: a test contract that distinguishes aggregate substrate provenance from composite and materialized projections.

- [ ] Add a failing test that constructs a composite and a materialized individual from the same `SourceCohort` and asserts their source cohort is identical while their materiality and write-back permission differ.
- [ ] Run the focused Lot test and confirm it fails only if the intended metadata is absent; do not add redundant production behavior if the existing type already satisfies it.
- [ ] Implement the minimal missing assertion-supporting change, if any.
- [ ] Run the focused Lot test and confirm it passes.
- [ ] Add a short module-level documentation note that projections are views of a substrate, not substitute population counts.
- [ ] Commit as `test(lot): pin population substrate and projection boundary`.

### Task 4: Verification and handoff

**Files:**
- Modify: `IMPLEMENTATION_PLAN.md` statuses
- Modify: `docs/superpowers/ledgers/2026-09-06-the-murrain.md` with the layer finding and follow-up

- [ ] Run focused worldgen and Lot tests once, inspecting the complete output.
- [ ] Run `cargo fmt --check`, the affected package clippy/tests, and `make gate-commit`.
- [ ] Update the plan statuses and record that 26→5 is now named as a layer-specific observation rather than silently re-pinned.
- [ ] Remove `IMPLEMENTATION_PLAN.md` only after every stage is complete, per repository guidance.
- [ ] Do not submit a new stage request until the local evidence and commit contents are reviewed.
