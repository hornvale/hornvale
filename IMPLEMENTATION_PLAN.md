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

# The Planetarium — stage tracker

Detailed tasks: [implementation plan](docs/superpowers/plans/2026-09-10-the-planetarium.md).
Approved scope: [design](docs/superpowers/specs/2026-09-10-the-planetarium-design.md).

The physical anchor-radius prerequisite is approved and included in Task 1.
Execution is authorized; Stages 1–3 have green canonical reports. Tasks 1–9 local implementation and evidence are independently reviewed; whole-branch technical review is approved with documentation corrections recorded. Final canonical stage and census accounting are complete; Nathan approved G6 on 2026-09-11. Canonical landing remains pending. This section tracks The Planetarium only; the
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
**Status**: Complete

## Stage 3: Complete 4K capture and verifiable packages
**Goal**: Export the full study with complete source and frame provenance.
**Success Criteria**: 300 3840×2160 PNGs, 30fps MP4, verified hashes/records, correct incomplete-run behavior; canonical stage result.
**Tests**: Plan Tasks 6–7: capture state/failures, GPU smoke, corrupted/incomplete packages and real ffmpeg/ffprobe verification.
**Status**: Complete

## Stage 4: Durable integration, refinement and handoff
**Goal**: Integrate client checks, document the libraries, measure and refine the final film, close through G6.
**Success Criteria**: CPU client gates integrated, performance and visual evidence delivered, final package reviewed, approved candidate landed through the queue.
**Tests**: Plan Tasks 8–9: dependency guard, client checks, repeat-render variance, package verification, canonical gates and human visual review.
**Status**: In Progress
