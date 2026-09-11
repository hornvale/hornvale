# Eclipse Rhythm and View

The canonical implementation plan is [docs/superpowers/plans/2026-09-10-eclipse-rhythm-and-view.md](docs/superpowers/plans/2026-09-10-eclipse-rhythm-and-view.md).

## Stage 1: Domain observables
**Goal**: Expose structured recurrence and observer results over the shipped eclipse physics.
**Success Criteria**: Recurrence identities and geographic visibility edge cases pass.
**Tests**: Focused astronomy unit and integration tests.
**Status**: Complete

## Stage 2: Scene contract
**Goal**: Produce `scene/eclipses/v3` with recurrence, regions, and optional observer results.
**Success Criteria**: Exact ticks, explicit null/absent semantics, deterministic JSON, and no world mutation.
**Tests**: Scene unit, shape, determinism, and fixture tests.
**Status**: Complete

## Stage 3: Surfaces
**Goal**: Wire the v3 contract through CLI, WASM, almanac, and reference documentation.
**Success Criteria**: Native/WASM agreement and aligned human-readable output.
**Tests**: CLI, WASM, almanac, and documentation checks.
**Status**: Complete

## Stage 4: Artifacts and verification
**Goal**: Review generated output and complete local and canonical gates.
**Success Criteria**: Required artifacts are current and all applicable gates pass.
**Tests**: `make quick`, affected client checks, `make gate-commit`, then queued stage/merge gates.
**Status**: In Progress
