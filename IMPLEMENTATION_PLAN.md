# The Wanderers — Implementation Status

Detailed plan: `docs/superpowers/plans/2026-09-07-the-wanderers.md`

## Stage 1: Stellar topology
**Goal**: Add single, wide-binary, and bounded close-binary system roots.
**Success Criteria**: Deterministic topology pins, stable single-star behavior, topology-specific anchor admission, loud invalid-pin failures.
**Tests**: Astronomy genesis properties and pin-isolation tests.
**Status**: Not Started

## Stage 2: Phase-aware ephemeris
**Goal**: Add isolated wanderer phases and stellar/wanderer position evaluators.
**Success Criteria**: Stable phases, correct circular ephemerides, visibility and regime behavior.
**Tests**: Astronomy ephemeris, provider, and night-sky tests.
**Status**: Not Started

## Stage 3: Observational events
**Goal**: Derive conjunctions, opposition, retrograde loops, and morning/evening-star observations.
**Success Criteria**: Complete/truncated event windows and honest recurrence behavior.
**Tests**: Event and almanac/provider tests.
**Status**: Not Started

## Stage 4: Scene and catalog contract
**Goal**: Append stellar topology and wanderer elements to `scene/system/v1` and wasm output.
**Success Criteria**: Native/wasm byte identity and current single-star field stability.
**Tests**: Scene shape tests, native CLI golden, world-wasm smoke.
**Status**: Not Started
