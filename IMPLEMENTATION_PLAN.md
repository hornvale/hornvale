## Stage 1: Name the population layers
**Goal**: Add a worldgen readout that distinguishes historical occupation records, present living occupations, and their occupied settlement columns.
**Success Criteria**: A deterministic seed-42 test reports each quantity separately; no caller uses an ambiguous `settled_columns` label.
**Tests**: Historical records include plague-ended occupations; present counts include only living occupations; column counts are derived from the corresponding occupation set.
**Status**: Complete

## Stage 2: Reconcile the delve witness
**Goal**: Make the delve-seating regression assert the historical or present quantity it actually intends to protect, with provenance tied to the new readout.
**Success Criteria**: The anti-vacuity and seating assertions remain intact, and the test message identifies the measured layer.
**Tests**: Seed-42 delve seating test; focused worldgen suite.
**Status**: Complete

## Stage 3: Connect projections explicitly
**Goal**: Document and test the boundary between the authoritative population substrate and Lot projections, including composite and materialized individuals.
**Success Criteria**: Projection metadata names its source cohort and causal status; population readouts do not treat projected people as substrate counts.
**Tests**: Existing Lot projection tests plus one cross-layer contract test.
**Status**: Complete

## Stage 4: Verification and handoff
**Goal**: Run focused tests, local commit gate, and prepare a stage resubmission only after the semantics are green.
**Success Criteria**: Tests and audits pass; held request is superseded by a commit whose fixture changes are explained by the layer contract.
**Tests**: Focused worldgen/Lot tests, `make gate-commit`.
**Status**: In Progress
