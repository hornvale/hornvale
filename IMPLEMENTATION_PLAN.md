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

# The Planetarium — stage tracker

Detailed tasks: [implementation plan](docs/superpowers/plans/2026-09-10-the-planetarium.md).
Approved scope: [design](docs/superpowers/specs/2026-09-10-the-planetarium-design.md).

The physical anchor-radius prerequisite is approved and included in Task 1.
Execution is authorized; Stage 1 is complete; Task 4 is reviewed and complete, and Task 5 is committed with independent review in progress. This section tracks The Planetarium only; the
inherited tracker above remains unchanged.

## Stage 1: A source-backed moving visual witness
**Goal**: Qualify physical inputs, expose evaluated observations, render a real Bevy scene.
**Success Criteria**: Source agreement and independent library boundaries; actual 4K still and at least two-second moving draft; canonical stage result.
**Tests**: Plan Tasks 1–3: source/CLI agreement, identity, source geometry, document/coordinate tests, GPU inspection.
**Status**: Complete

## Stage 2: Exact time, interaction and authored direction
**Goal**: Make the shared scene seekable and direct the pilot.
**Success Criteria**: Exact seek/playback correspondence, stale-reply rejection, world/scope reset, camera/time controls and frozen edit; canonical stage result.
**Tests**: Plan Tasks 4–5: integer clock boundaries, out-of-order replies, scope changes, shot continuity/cuts and live controls.
**Status**: In Progress

## Stage 3: Complete 4K capture and verifiable packages
**Goal**: Export the full study with complete source and frame provenance.
**Success Criteria**: 300 3840×2160 PNGs, 30fps MP4, verified hashes/records, correct incomplete-run behavior; canonical stage result.
**Tests**: Plan Tasks 6–7: capture state/failures, GPU smoke, corrupted/incomplete packages and real ffmpeg/ffprobe verification.
**Status**: Not Started

## Stage 4: Durable integration, refinement and handoff
**Goal**: Integrate client checks, document the libraries, measure and refine the final film, close through G6.
**Success Criteria**: CPU client gates integrated, performance and visual evidence delivered, final package reviewed, approved candidate landed through the queue.
**Tests**: Plan Tasks 8–9: dependency guard, client checks, repeat-render variance, package verification, canonical gates and human visual review.
**Status**: Not Started
