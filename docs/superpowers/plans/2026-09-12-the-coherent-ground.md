# The Coherent Ground Integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or `superpowers:executing-plans`. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Put source-owned coherent surface patches on the normal Planetarium render path, add real facet-scale terrain detail, preserve narrow features, and prove the result with actual rendered captures.

**Architecture:** The existing Level-6 worldgen realization remains the semantic source of truth. A camera-driven Bevy patch catalog requests bounded revision-qualified patches, converts them into terrain and feature entities, and atomically replaces fallback geometry only after the visible region is ready. Terrain detail is added in `windows/worldgen` from conditioned relief, inherited hydrology curves, and ridge signals; Planetarium captures real before/after frames and measures the separate source, application, render, and memory costs.

**Tech Stack:** Rust workspace crates, the pinned `clients/visual` Cargo workspace with Rust `1.96.1`, Bevy 0.19 mesh/material/entity APIs, existing source JSON protocols, deterministic terrain/hash-noise helpers, existing Planetarium capture/film infrastructure, cargo-nextest, and the repository's single `tests/suite.rs` integration-test layout.

**Spec:** `docs/superpowers/specs/2026-09-12-the-coherent-ground-integration-design.md`

## Global Constraints

- Level 6 remains the authoritative macro world; no global Level-8 promotion is permitted.
- No save-format, epoch, consumed stream, or new random-stream change is permitted.
- Terrain/worldgen/source own semantic fields; Bevy owns entity and asset lifecycle, not semantic feature decisions.
- Local relief is deterministic hash-labelled noise and must be bounded and boundary-compatible.
- Hydrology geometry realizes inherited channels/rills; it must not reroute flow or create a competing drainage graph.
- A feature that misses terrain vertices must still render through adaptive source-owned strips or an equivalent fragment-visible representation.
- Fallback geometry remains during loading but must not overlap ready patches or z-fight with them.
- Full surface revision, macro face, child path, and render generation participate in stale-request/reply identity.
- Dynamic clouds, precipitation advection, currents, snow evolution, foam animation, and weather-qualified roughness remain out of scope.
- Every integration test is registered below the existing crate `tests/suite.rs`; no new top-level integration-test binary is added.
- Expensive stage/merge gates run through the Sluice on lefford; census regeneration is never run locally.

## File and ownership map

| Area | Files | Responsibility |
|---|---|---|
| Camera selection | `clients/visual/bevy/src/lifecycle.rs`, `clients/visual/planetarium/src/live.rs` | Derive bounded visible addresses, schedule revision-qualified requests, and retain fallback/ready state. |
| Render resources | `clients/visual/bevy/src/lifecycle.rs`, `clients/visual/bevy/src/astronomy/surface.rs` | Insert patch meshes/materials, spawn/retire entities, and convert source-owned fields without semantic invention. |
| Facet evaluation | `windows/worldgen/src/facet.rs`, `domains/terrain/src/facet.rs` | Add deterministic conditioned relief, bed/ridge modifiers, adaptive feature geometry inputs, and boundary-preserving evaluations. |
| Patch documents | `windows/scene/src/lib.rs`, `clients/visual/source/src/protocol.rs`, `clients/visual/bevy/src/documents.rs` | Carry any newly required strip/visibility fields canonically across the source boundary. |
| Tests | existing `tests/suite.rs` files and their `suite/*.rs` modules | Prove each stage behaviorally at its owning layer. |
| Capture proof | `clients/visual/planetarium/src/review.rs`, `capture.rs`, `tests/suite/proof.rs` | Capture actual rendered before/after artifacts and report separated measurements. |
| Durable docs | `docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md`, `docs/audits/campaign-reconciliation.tsv` | Correct status, record measured evidence, and preserve deferred follow-ups. |

---

## Stage 1: Visible vertical slice

**Goal:** One camera-facing source patch travels through request, Bevy application, entity spawn, and a changed rendered frame while the legacy globe remains a safe loading fallback.

**Success criteria:** A test can schedule a patch, apply its reply, observe a patch-owned entity/asset in the Bevy world, and prove the fallback entity is hidden or removed for that covered region. A Planetarium capture records a before/after image digest that differs for the same seed and camera.

### Task 1: Add bounded camera patch selection and catalog state

**Files:**

- Modify: `clients/visual/bevy/src/lifecycle.rs`
- Modify: `clients/visual/planetarium/src/live.rs`
- Test: `clients/visual/bevy/tests/suite.rs`, `clients/visual/bevy/tests/suite/surface.rs`
- Test: `clients/visual/planetarium/tests/suite.rs`, `clients/visual/planetarium/tests/suite/proof.rs`

**Interfaces:**

- Consume `Binding`, `SurfacePatchCacheKey`, `SurfacePatchDocument`, `SceneTarget`, `SceneCatalog`, `OrbitCamera`, and the existing source observation mirror.
- Produce a bounded visible-address selector and a catalog-owned patch state containing desired, pending, ready, and retired addresses for one binding/generation.
- Produce an application path that returns the patch entity identity and stores its mesh/material handles in Bevy-owned collections rather than merely returning them to the caller.

- [ ] **Step 1: Write failing tests for bounded selection and state transitions.** Add tests that assert a camera envelope selects a finite face neighborhood, repeated selection is ordered and byte-stable, a scheduled reply moves `pending` to `ready`, a stale generation is discarded before asset insertion, and a ready patch suppresses fallback coverage.
- [ ] **Step 2: Run the focused tests and verify the behavioral red.** Run `cargo nextest run --manifest-path clients/visual/bevy/Cargo.toml --test suite -E 'test(surface)'`. Expected: the new catalog state/application assertions fail because current lifecycle only returns `SurfaceMeshHandles` and normal scene setup still creates the legacy globe.
- [ ] **Step 3: Implement the bounded catalog and camera selector.** Use the existing camera transform/body envelope to choose one camera-facing macro face plus a bounded neighbor ring; use full revision/cache identity and render generation in state keys; keep address order canonical with integer face/path ordering.
- [ ] **Step 4: Connect source replies to Bevy asset insertion.** Validate the existing document first, call the source-owned mesh/material conversion, insert handles into the existing `SceneCatalog` collections, spawn patch entities with a dedicated component, and make replacement atomic with respect to the current generation.
- [ ] **Step 5: Add fallback handoff behavior.** Keep the globe while the selected patch set is incomplete; once coverage is ready, hide or remove only the covered fallback surface and assert no duplicate patch/fallback coverage remains.
- [ ] **Step 6: Run focused tests and verify green.** Run the Bevy suite filter again and the Planetarium proof filter. Expected: catalog state, stale filtering, asset insertion, fallback handoff, and changed-frame scaffolding pass.
- [ ] **Step 7: Commit the vertical slice.** Run `pwd && git branch --show-current`, `cargo +1.96.1 fmt --check`, then commit the lifecycle and focused tests with `feat: integrate visible surface patches`.

## Stage 2: Conditioned facet geometry

**Goal:** Make refined terrain materially differ from macro interpolation while preserving Level-6 authority, shared boundaries, inherited flow direction, and continuous coast/ridge behavior.

**Success criteria:** For a fixed seed and address, the emitted patch differs from the interpolation-only baseline in bounded areas; boundary samples remain equal across neighbors; channel beds descend toward inherited terminals; ridge detail follows source ridge direction; coast transitions remain continuous.

### Task 2: Add deterministic conditioned relief and hydrology bed shaping

**Files:**

- Modify: `windows/worldgen/src/facet.rs`
- Modify: `domains/terrain/src/facet.rs` if additional source-owned cross-section/curve samples are required
- Test: `windows/worldgen/tests/suite.rs`, `windows/worldgen/tests/suite/facet_context.rs`
- Test: `domains/terrain/tests/suite.rs`, `domains/terrain/tests/suite/facet_properties.rs`

**Interfaces:**

- Consume `SurfaceRealizationContext`, `SurfacePatch`, `FacetFieldSample`, `RealizedCurve`, `ChannelCrossSection`, Level-6 terrain/climate values, and the existing hash-labelled terrain seeds.
- Produce deterministic patch sample evaluation in which `height_m` includes bounded conditioned relief and hydrology bed terms, plus test-visible helpers for relief amplitude, bed incision, and ridge contribution.
- Preserve existing `SurfaceRevision` configuration hashing by including every new calibration constant in the canonical configuration record.

- [ ] **Step 1: Write failing activity and boundary tests.** Add tests comparing the current macro-only reference with the new emitted patch, asserting nonzero but bounded relief away from boundaries, nonzero channel incision at a centerline, downstream bed monotonicity to confluence/lake/ocean terminals, and equal edge/corner samples across neighboring addresses.
- [ ] **Step 2: Run the focused tests and capture the red state.** Run `cargo nextest run -p hornvale-worldgen --test suite -E 'test(facet_context)'` and the terrain facet property filter. Expected: activity tests fail because `compose_sample` currently blends Level-6 values without local relief or bed terms.
- [ ] **Step 3: Implement conditioned relief.** Derive a random-access noise value from stable existing leaf/hash inputs and the canonical world-space position; taper amplitude by macro slope/elevation/coast distance and force shared boundary agreement through the same world-space evaluator on either side.
- [ ] **Step 4: Implement bed, bank, floodplain, terrace, delta, and ridge contributions.** Use signed curve distance, width, local downstream grade, terminal kind, and ridge direction/strength. Clamp each term to documented bounds and preserve the macro water classification and inherited routing graph.
- [ ] **Step 5: Extend revision configuration deterministically.** Add the new algorithm version/configuration values to the existing canonical record and assert that changing any one changes the configuration hash while repeated builds remain byte-identical.
- [ ] **Step 6: Run focused tests and verify green.** Run both filters, then `cargo +1.96.1 fmt --check` for affected clients if shared document types changed. Expected: emitted patches show real local variation, hydrology shaping, and directional ridge structure without seam regressions.
- [ ] **Step 7: Commit conditioned geometry.** Run the branch evidence command and commit with `feat: condition facet terrain from macro structure`.

## Stage 3: Narrow features and mixed-LOD rendering

**Goal:** Ensure rivers, ridges, and coast features remain visible even when they miss terrain vertices, and make adaptive feature geometry participate in ordinary patch replacement and mixed-LOD rendering.

**Success criteria:** A feature whose centerline is deliberately placed between all terrain vertices produces visible render geometry; feature IDs and continuation metadata survive refinement; equal and unequal LOD boundaries have no gaps, overlaps, or fallback z-fighting.

### Task 3: Emit and consume adaptive feature strips

**Files:**

- Modify: `domains/terrain/src/facet.rs`
- Modify: `windows/worldgen/src/facet.rs`
- Modify: `windows/scene/src/lib.rs`
- Modify: `clients/visual/source/src/protocol.rs`, `clients/visual/source/src/lib.rs`
- Modify: `clients/visual/bevy/src/documents.rs`, `clients/visual/bevy/src/astronomy/surface.rs`, `clients/visual/bevy/src/lifecycle.rs`
- Test: terrain/worldgen/scene/source/Bevy existing suite modules for facet/surface behavior

**Interfaces:**

- Consume existing `RealizedCurve`, `FeatureEndpoint`, `FacetAddress`, `stitch_transition`, and `SurfacePatch` contracts.
- Produce canonical patch-local feature strip data containing stable feature identity, adaptive centerline/width samples, signed side/distance data, semantic mask, and continuation metadata.
- Produce Bevy conversion/application that creates feature entities or mesh sections independently of terrain vertex hits and includes them in the same revision/generation lifecycle.

- [ ] **Step 1: Write failing narrow-feature and strip protocol tests.** Construct a curve passing between terrain vertices and assert the scene/source document contains a nonempty strip; assert repeated serialization is byte-identical; assert refinement preserves feature identity and boundary continuation.
- [ ] **Step 2: Run focused tests and verify red.** Run terrain, worldgen, scene, source, and Bevy surface filters. Expected: no strip is emitted and the Bevy mesh contains only the terrain vertex mask.
- [ ] **Step 3: Implement adaptive strip generation.** Subdivide by curve curvature, width, and patch scale; use canonical spherical positions and stable feature order; clip at canonical boundaries while retaining endpoint metadata.
- [ ] **Step 4: Extend the derived protocol.** Add only the source-owned strip fields needed for rendering, preserve schema/revision/binding validation, and reject malformed/nonfinite strip geometry before Bevy sees it.
- [ ] **Step 5: Convert strips into Bevy render assets.** Build ribbon geometry with source-owned semantic masks and material inputs, attach feature identity components, and apply stale generation/revision filtering before insertion.
- [ ] **Step 6: Add mixed-LOD replacement tests.** Assert coarse-to-fine transition topology, strip endpoints, terrain boundaries, and fallback coverage agree with no gaps or overlaps during replacement.
- [ ] **Step 7: Run all focused client tests and commit.** Run `make visual-check-run`, then commit with `feat: preserve narrow surface features across lods`.

## Stage 4: Genuine rendered proof and closeout

**Goal:** Replace synthetic review facts with actual before/after rendered captures, record honest measurements, correct durable campaign status, and submit the fully revalidated campaign.

**Success criteria:** The proof artifacts show a changed after frame with visible patch entities, local terrain detail, narrow features, continuous coasts/ridges, and clean fallback/LOD transitions. Metrics separately identify source generation, mesh/material application, first visible frame, steady-state render time, and peak RSS.

### Task 4: Capture and validate the real Planetarium proof

**Files:**

- Modify: `clients/visual/planetarium/src/review.rs`, `capture.rs`, `tests/suite/proof.rs`
- Modify: `clients/visual/planetarium/src/live.rs` only if capture needs explicit patch-readiness markers
- Create: a committed proof manifest or digest artifact only if the existing capture package requires one
- Modify: `docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md`
- Modify: `docs/audits/campaign-reconciliation.tsv`

**Interfaces:**

- Consume the real Planetarium renderer, film/camera script, patch readiness state, source revision, and capture package/hash conventions.
- Produce before/after frame records whose PNG/content digests and metadata are read back by tests; produce separated timing/RSS fields with no synthetic fallback.
- Update the ledger to state what is actually complete, what remains deferred, and what the measured proof supports about further refinement.

- [ ] **Step 1: Write failing artifact-backed proof tests.** Require distinct before/after frame digests, patch-entity readiness in the after capture, visible narrow-feature evidence, no fallback overlap, and populated generation/application/first-visible/steady-state/RSS measurements.
- [ ] **Step 2: Run the proof filter and verify the red state.** Run `cargo nextest run --manifest-path clients/visual/planetarium/Cargo.toml --test suite -E 'test(proof)'`. Expected: current supplied-booleans proof fails because it has no actual rendered artifacts or patch-readiness evidence.
- [ ] **Step 3: Implement before/after capture orchestration.** Capture the same fixed seed and camera script before patch activation and after the selected patch set is ready; record the source/revision identity, visible patch addresses, fallback state, frame digest, and camera frame.
- [ ] **Step 4: Measure the real lifecycle.** Time source generation, mesh/material application, first visible frame, and steady-state frames separately; sample peak RSS around the proof run; fail if a measurement is synthesized or absent.
- [ ] **Step 5: Implement artifact inspection.** Read the captured PNG/metadata back, compare before/after digests and required visual markers, and retain semantic checks for confluence, basin, coast, corner, and mixed LOD cases.
- [ ] **Step 6: Correct durable status and record expansion decision.** Replace the stale “implementation has not started” ledger text, record the actual measured evidence, state any remaining limitations, and update the reconciliation evidence without claiming more than the captures establish.
- [ ] **Step 7: Run focused proof, full visual checks, and local commit gate.** Run the proof filter, `make visual-check-run`, and `make gate-commit`; inspect `git diff --check` and the capture manifest.
- [ ] **Step 8: Commit proof evidence and submit the stage gate.** Commit with `test: prove visible coherent ground`, push the full SHA, and submit `make sluice-stage BRANCH=campaign/the-coherence REF=<full-sha>`.

### Task 5: G6 review and merge close

**Files:**

- No implementation files; review and queue artifacts only.

- [ ] **Step 1: Dispatch an independent G6 reviewer** with the bounded review package, current proof artifacts, and the latest stage result.
- [ ] **Step 2: Resolve every P1/P2 finding** through a bounded fix task, with a fresh review after each fix round and no merge request while findings remain.
- [ ] **Step 3: Present the post-G3 ledger digest** with save/epoch/stream decisions first, then proof measurements, rejected alternatives, and deferred Living Surface scope.
- [ ] **Step 4: After explicit G6 approval, submit the merge through the Sluice** and use the closing-campaign procedure to verify the landed SHA and remove the completed plan artifact.

## Verification ladder

During implementation, use the cheapest relevant check first:

1. Focused owner-layer tests for the task currently under review.
2. `cargo +1.96.1 fmt --check` and pinned visual Clippy for visual changes.
3. `make visual-check-run` for the separate client workspace.
4. `make gate-commit` before every implementation commit touching Rust.
5. `make sluice-stage BRANCH=campaign/the-coherence REF=<full-sha>` at the Stage 4 boundary.
6. `make sluice BRANCH=campaign/the-coherence REF=<full-sha>` only after G6 approval.

## Plan status

- Stage 1: Not Started
- Stage 2: Not Started
- Stage 3: Not Started
- Stage 4: Not Started
