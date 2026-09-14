# Anchor Orbital Coherence — Stage Tracker

Detailed task plan: `docs/superpowers/plans/2026-09-13-anchor-orbital-coherence.md`

## Stage 1: Physical Contract
**Goal**: Expose a stable typed anchor evaluation and descriptive physical errors.
**Success Criteria**: `anchor_state_at` is deterministic, explicit-time, and returns typed failures.
**Tests**: Anchor contract, negative time, malformed input, convergence, astronomy suite.
**Status**: Complete

## Stage 2: Calendar and Insolation
**Goal**: Route calendar solar geometry and instantaneous insolation through the shared state.
**Success Criteria**: Calendar and insolation agree with anchor state without redefining deep-time forcing.
**Tests**: Phase/radius coherence, negative time, locked/retrograde, continuity.
**Status**: Complete

## Stage 3: Eclipse and Scene Projections
**Goal**: Route eclipse and scene geometry through shared anchor state.
**Success Criteria**: Event and scene consumers agree with physical state while preserving wire semantics.
**Tests**: Eclipse alignment, no-event behavior, scene geometry, explicit tick conversion.
**Status**: In Progress

## Stage 4: Qualification and Closure
**Goal**: Prove coherence, determinism, compatibility, and artifact freshness.
**Success Criteria**: Full battery and final local gate pass; measured artifact changes are recorded.
**Tests**: Mutation tripwire, reordered queries, historical sweep, focused suites, final gate.
**Status**: Not Started
