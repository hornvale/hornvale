# The Coherent Ground Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a deterministic, source-owned facet realization that adds coherent local terrain, rivers, coasts, mountain structure, and blended materials to the Planetarium while preserving Level-6 macro authority.

**Architecture:** `windows/worldgen` composes Level-6 terrain and climate into a lazy `SurfaceRealizationContext`. It emits addressed patches whose persistent feature identities and canonical world-space evaluators survive refinement and mixed LOD. `clients/visual/source` transports derived patch documents, and Planetarium/Bevy schedules, stitches, and renders them; Bevy may add microappearance but never semantic surface features.

**Tech Stack:** Rust workspace crates (`hornvale-terrain`, `hornvale-worldgen`, `hornvale-scene`), the separate `clients/visual` Cargo workspace, existing `serde`/`serde_json` protocols, deterministic kernel geometry/noise primitives, Bevy mesh/material APIs, cargo-nextest, and the repository's single `tests/suite.rs` integration-test layout.

**Spec:** `docs/superpowers/specs/2026-09-11-the-coherent-ground-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md`

## Global Constraints

- Level 6 remains the authoritative macro world.
- Generation is deterministic and independent of request order, cache state, and renderer choice.
- Semantic fields are generated outside Bevy and can be consumed by more than one observer.
- No save-format change, epoch change, new random stream, or global Level-8 macro rewrite is permitted in this campaign.
- Terrain and climate domains own their macro inputs; `windows/worldgen` composes them; clients do not become semantic authorities.
- New hash-derived labels are random-access inputs and do not consume a `Stream`; a consumed stream requires an explicit stream label and save/epoch review.
- Stable IDs, integer topology, field ordering, and canonical serialized protocol values are byte-identical; floating positions, normals, heights, distances, and weights use documented numerical tolerances.
- Existing `channel` and `rill` outputs are inputs to local realization and do not count as geometric containment for refined patches.
- A boundary is never a river termination reason. Headwaters, confluences, terminal basins, outlets, and river mouths have explicit endpoint metadata.
- Persistent feature identity is separate from sampling resolution. Refinement changes samples, not feature identity or topology.
- The ground patch contract contains no cloud or precipitation hooks; dynamic clouds, precipitation advection, currents, snow evolution, foam animation, and weather-qualified roughness belong to The Living Surface.
- Every crate keeps one integration-test binary per crate: add modules below the existing `tests/suite.rs` rather than creating another top-level integration test file.
- Every task ends with a focused test command and a frequent commit. Implementation workers use `superpowers:subagent-driven-development`, and each completed task receives the two-stage review required by that skill.

---

## Verified starting points

The implementation starts from these inspected interfaces and behaviors:

- `domains/terrain/src/provider.rs` defines `GeneratedTerrain`, including vertex elevation, sea level, water classification, downhill targets, channels, lithology, and terrain accessors.
- `domains/terrain/src/drainage.rs` defines `downhill_targets` and `drainage_field` over Level-6 vertices. Its module documentation explicitly says the current field has no sub-vertex river geometry or lake filling.
- `domains/terrain/src/channel.rs` defines `ChannelNetwork::build`, `ChannelNetwork::nearest_line`, `ChannelNetwork::bank_reading`, `channel_half_width`, `band_edges`, `local_slope`, and `meander_at`. It carries one-dimensional spherical polylines and widths, not a refined mesh.
- `domains/terrain/src/branch.rs` defines `Rill`, `rills_of`, `rill_reading`, `vertex_catchment`, and `room_spacing`. Its tests measure geometric spill from overlapping tangent-plane catchment proxies, so the new plan treats rills as inherited attachment/topology input and adds separate realized bed geometry.
- `domains/terrain/src/globe.rs` stores already-derived leaf seeds such as `channel_noise_seed` and `rill_partition_seed`; these are hash-noise inputs and do not consume streams.
- `windows/worldgen/src/lib.rs` is the composition root used by consumers to derive terrain and climate from a `World`. The new context belongs here because terrain and climate domains cannot depend on one another.
- `windows/scene/src/lib.rs` defines `SceneContext::build`, `tiles_scene_in`, `TilesScene`, `TileFields`, and `scene_json`. `tiles_scene_in` currently emits nearest-vertex values into a raster document.
- `clients/visual/source/src/lib.rs` defines `Source`, `Source::open`, `Source::initial_document`, and `Source::observe`; `clients/visual/source/src/protocol.rs` defines `Binding`, `Request`, `Reply`, and `Initial`. Binding validation already rejects the wrong source, scope, world hash, or revision.
- `clients/visual/bevy/src/astronomy/surface.rs` defines `sample`, `filter_field`, `globe_mesh`, `anchor_texture`, `anchor_roughness`, and `cloud_shell`. The current client interpolates source-grid fields and adds presentation noise; it has no semantic patch or feature transport.
- `clients/visual/bevy/src/lifecycle.rs` converts the initial tile document into meshes, textures, roughness, and a static cloud shell. `clients/visual/planetarium/src/capture.rs` records the current interpolated terrain and cosmetic surface treatments.
- Tests are consolidated under `domains/terrain/tests/suite.rs`, `windows/scene/tests/suite.rs`, `clients/visual/source/tests/suite.rs`, `clients/visual/bevy/tests/suite.rs`, and `clients/visual/planetarium/tests/suite.rs`.

## File ownership map

The following files are owned by the listed responsibility. A task may add a
focused new file, but it must not move an existing responsibility across the
layering boundary.

| File | Responsibility in this campaign |
|---|---|
| `domains/terrain/src/facet.rs` | Terrain-only patch primitives, feature IDs/endpoints, canonical spherical address geometry, and deterministic curve/field evaluation from terrain macro inputs. |
| `domains/terrain/src/lib.rs` | Public exports for the terrain facet primitives. |
| `domains/terrain/tests/suite.rs` | Registration of terrain facet property modules; remains the crate's only integration-test binary. |
| `domains/terrain/tests/suite/facet_properties.rs` | Terrain-side tests for boundary evaluation, channel attachments, bed profiles, and deterministic feature realization. |
| `windows/worldgen/src/facet.rs` | Composition of `GeneratedTerrain` and `GeneratedClimate`, revision/configuration identity, and complete `SurfaceRealizationContext`. |
| `windows/worldgen/src/lib.rs` | Public exports of the worldgen facet context and patch contract. |
| `windows/worldgen/tests/suite.rs` | Registration of worldgen facet tests in the existing suite binary. |
| `windows/worldgen/tests/suite/facet_context.rs` | Composition, revision, neighbor-context, and cache/request-order tests. |
| `windows/scene/src/lib.rs` | Derived scene query surface for patch requests and patch JSON; no new terrain or climate authority. |
| `windows/scene/tests/suite.rs` | Registration of scene facet tests in the existing suite binary. |
| `windows/scene/tests/suite/facet_scene.rs` | Protocol-shaped scene serialization and fixture tests. |
| `clients/visual/source/src/protocol.rs` | Versioned patch request/reply wire structs and binding/revision fields. |
| `clients/visual/source/src/lib.rs` | Source-side patch observation and stale binding/revision rejection. |
| `clients/visual/source/tests/suite.rs` | Registration of source protocol tests in the existing suite binary. |
| `clients/visual/source/tests/suite/surface.rs` | Source request/reply, byte stability, and stale-response tests. |
| `clients/visual/bevy/src/documents.rs` | Deserialization of patch documents into client-owned render inputs. |
| `clients/visual/bevy/src/lifecycle.rs` | Patch scheduling, cache lifetime, mixed-LOD render resource ownership, and revision filtering. |
| `clients/visual/bevy/src/astronomy/surface.rs` | Conversion of source-owned patch fields and curves into mesh/material inputs; only microappearance remains client-generated. |
| `clients/visual/bevy/tests/suite.rs` | Registration of Bevy patch tests in the existing suite binary. |
| `clients/visual/bevy/tests/suite/surface.rs` | Mesh seam, narrow-feature footprint, and mixed-LOD conversion tests. |
| `clients/visual/planetarium/src/review.rs` | Before/after visual review capture and explicit latency/memory/frame-time measurements. |
| `clients/visual/planetarium/src/capture.rs` | Updated provenance wording for source-owned coherent ground and deferred weather. |
| `clients/visual/planetarium/tests/suite.rs` | Registration of proof-review contract tests in the existing suite binary. |
| `clients/visual/planetarium/tests/suite/proof.rs` | Proof metadata and acceptance assertions for the selected region and LODs. |

## Stage 1: Addressed patch contract and terrain primitives

**Goal:** Establish deterministic addresses, persistent feature identity, canonical boundary evaluation, and terrain-only realized feature primitives before wiring climate or rendering.

**Success criteria:** A patch address has an explicit child path; edge and corner samples agree independently; feature identity survives refinement; narrow features have source-owned curve/field data; no random stream or save format changes occur.

### Task 1: Add the terrain facet primitives

**Files:**

- Create: `domains/terrain/src/facet.rs`
- Modify: `domains/terrain/src/lib.rs`
- Test: `domains/terrain/tests/suite.rs`
- Test: `domains/terrain/tests/suite/facet_properties.rs`

**Interfaces:**

- Produce `pub struct FacetAddress { pub macro_face: Facet, pub child_path: Vec<u8> }`. Every child digit is `0..=3`; `child_path.len()` is the refinement level; the empty path addresses the whole Level-6 face.
- Produce `pub struct FeatureId { pub kind: FeatureKind, pub macro_anchor: Vertex, pub ordinal: u32 }` and `pub enum FeatureKind { ChannelReach, Confluence, Shoreline, Ridge, MaterialTransition }`. IDs derive only from macro anchors and stable ordinals.
- Produce `pub struct FeatureEndpoint { pub feature: FeatureId, pub side: EndpointSide, pub boundary: Option<BoundaryParameter>, pub terminal: TerminalKind }`, `pub enum EndpointSide { Upstream, Downstream }`, `pub struct BoundaryParameter { pub address: FacetAddress, pub edge: u8, pub t: f64 }`, and `pub enum TerminalKind { Headwater, Confluence, Lake, Ocean, Continuation }`.
- Produce `pub struct RealizedCurve { pub feature: FeatureId, pub points: Vec<[f64; 3]>, pub width: Vec<f64>, pub endpoints: [FeatureEndpoint; 2] }` and `pub struct FacetFieldSample { pub position: [f64; 3], pub height_m: f64, pub normal: [f64; 3], pub material_weights: [f32; 8], pub shoreline_distance_m: f64, pub water_depth_m: f64, pub flow_direction: [f64; 3], pub flow_strength: f64, pub channel_distance_m: f64, pub channel_width_m: f64, pub floodplain_weight: f32, pub bank_weight: f32, pub terrace_weight: f32, pub delta_weight: f32, pub ridge_direction: [f64; 3], pub ridge_strength: f32 }`.
- Produce `pub fn canonical_edge_sample(address: &FacetAddress, edge: u8, t: f64) -> [f64; 3]` and `pub fn canonical_corner_sample(address: &FacetAddress, corner: u8) -> [f64; 3]`. Both are pure world-space evaluations and use the same child addressing on either adjacent face.
- Produce `pub fn feature_sample(curve: &RealizedCurve, position: [f64; 3]) -> (f64, f64)` returning signed distance and interpolated width, so a feature is visible even when no patch vertex lies on it.

- [ ] **Step 1: Write failing tests for address validation, shared edge/corner samples, feature identity, and curve sampling.** Add tests named `child_path_digits_are_bounded`, `adjacent_faces_share_canonical_edge_samples`, `face_corners_are_order_independent`, `refinement_changes_samples_but_not_feature_id`, and `curve_sampling_does_not_require_a_vertex_on_the_feature` in `facet_properties.rs`.
- [ ] **Step 2: Run the focused tests and verify the red state.** Run `cargo nextest run -p hornvale-terrain --test suite -E 'test(facet_properties)'`. Expected: compilation or assertion failures because `facet.rs` and its exports do not exist.
- [ ] **Step 3: Implement the address, identity, endpoint, curve, and canonical sample types.** Use dense integer indices and existing kernel `Facet`, `Vertex`, spherical position, and deterministic math conventions. Reject child digits outside `0..=3` at construction.
- [ ] **Step 4: Run the focused tests and verify the green state.** Run the same nextest command. Expected: all five named tests pass, with no new stream label and no save/epoch file changed.
- [ ] **Step 5: Commit the terrain contract.** Run `cargo fmt --check`, then `git add domains/terrain/src/facet.rs domains/terrain/src/lib.rs domains/terrain/tests/suite.rs domains/terrain/tests/suite/facet_properties.rs && git commit -m "feat: define addressed facet primitives"`.

### Task 2: Realize terrain channels and terminal metadata

**Files:**

- Modify: `domains/terrain/src/facet.rs`
- Modify: `domains/terrain/src/lib.rs`
- Test: `domains/terrain/tests/suite/facet_properties.rs`

**Interfaces:**

- Consume `FacetAddress`, `FeatureId`, `FeatureEndpoint`, `RealizedCurve`, `FacetFieldSample`, `ChannelNetwork`, `Rill`, `rills_of`, `channel_half_width`, and `local_slope` from Task 1 and the existing terrain modules.
- Produce `pub struct TerrainFacetInputs<'a> { pub globe: &'a TectonicGlobe, pub geo: &'a Geosphere, pub channels: &'a ChannelNetwork }`.
- Produce `pub fn realize_channel_curves(inputs: TerrainFacetInputs<'_>, address: &FacetAddress) -> Vec<RealizedCurve>`.
- Produce `pub fn channel_endpoint_kind(inputs: TerrainFacetInputs<'_>, vertex: Vertex, downstream: bool) -> TerminalKind`.
- Produce `pub fn bed_height_profile(curve: &RealizedCurve, terrain: &GeneratedTerrain) -> Vec<f64>` for property tests and later field construction.

- [ ] **Step 1: Add red tests for headwaters, confluences, terminal basins, outlets, mouth attachment, and outside-neighborhood continuation.** Use existing deterministic seed-42 terrain setup from `channel_properties.rs` and `rill_properties.rs`. Assert every curve has a source endpoint, every confluence names one downstream `FeatureId`, terminal sinks use `TerminalKind::Lake`, ocean reaches use `TerminalKind::Ocean`, and unloaded exits use `TerminalKind::Continuation` with a canonical boundary parameter.
- [ ] **Step 2: Run the tests and verify the red state.** Run `cargo nextest run -p hornvale-terrain --test suite -E 'test(facet_properties::headwaters) or test(facet_properties::confluences) or test(facet_properties::terminal) or test(facet_properties::continuation)'`. Expected: the new functions are absent or return no realized curves.
- [ ] **Step 3: Implement inherited routing and realized bed geometry separately.** Read downhill/catchment/channel topology from `GeneratedTerrain` and `ChannelNetwork`; attach rills to their parent rather than rerouting them. Generate centerline, bank, floodplain, terrace, and delta geometry in world space. Emit continuation metadata when a curve exits the requested patch. Do not claim the tangent-plane rill proxy is geometrically contained.
- [ ] **Step 4: Add the bed-profile property.** Sample interior points along each realized curve and assert non-increasing bed height in the inherited downstream direction, allowing only the documented mouth/lake settling tolerance. Test confluence mouths coincide within the geometry tolerance.
- [ ] **Step 5: Run the focused tests and verify green.** Run the command from Step 2, then `cargo nextest run -p hornvale-terrain --test suite -E 'test(facet_properties)'`. Expected: terminal metadata, continuation tokens, confluence attachment, and interior bed-profile checks pass.
- [ ] **Step 6: Commit the terrain realization.** Run `cargo fmt --check`, then `git add domains/terrain/src/facet.rs domains/terrain/src/lib.rs domains/terrain/tests/suite/facet_properties.rs && git commit -m "feat: realize coherent terrain features"`.

## Stage 2: Worldgen composition, fields, and LOD continuity

**Goal:** Compose terrain and climate in the correct window layer, define revision/configuration identity, generate fields and narrow-feature data, and prove same-level, mixed-LOD, face-corner, and refine/coarsen continuity.

**Success criteria:** `SurfaceRealizationContext` is deterministic and independent of request order; fields blend terrain/climate inputs; no cloud or precipitation fields exist; parent-child preservation and unequal LOD stitching have executable tests.

### Task 3: Add the worldgen realization context and revision identity

**Files:**

- Create: `windows/worldgen/src/facet.rs`
- Modify: `windows/worldgen/src/lib.rs`
- Test: `windows/worldgen/tests/suite.rs`
- Test: `windows/worldgen/tests/suite/facet_context.rs`

**Interfaces:**

- Produce `pub struct SurfaceRevision { pub source_revision: String, pub algorithm_version: &'static str, pub configuration_hash: [u8; 32] }`.
- Produce `pub struct SurfaceRealizationContext { pub revision: SurfaceRevision }` with `pub fn build(world: &World) -> Result<SurfaceRealizationContext, SurfaceBuildError>` and `pub fn realize(&self, address: &FacetAddress) -> Result<SurfacePatch, SurfaceBuildError>`.
- Produce `pub struct SurfacePatch { pub revision: SurfaceRevision, pub address: FacetAddress, pub samples: Vec<FacetFieldSample>, pub curves: Vec<RealizedCurve>, pub triangles: Vec<[u32; 3]> }`.
- Produce `pub enum SurfaceBuildError { InvalidAddress(String), RevisionMismatch(String), MissingMacroContext(String), Numeric(String) }` with `Display` and `Error` implementations.
- `SurfaceRealizationContext::build` owns composition of `GeneratedTerrain` and `GeneratedClimate` obtained through existing worldgen APIs. It may call terrain facet functions but adds climate/material composition in `windows/worldgen`.

- [ ] **Step 1: Write failing tests for context construction, stable revision, request-order independence, and cache independence.** Add `same_world_has_same_surface_revision`, `patch_bytes_do_not_depend_on_request_order`, `patch_bytes_do_not_depend_on_cache_eviction`, and `revision_rejects_wrong_address_context` to `facet_context.rs`.
- [ ] **Step 2: Run the tests and verify red.** Run `cargo nextest run -p hornvale-worldgen --test suite -E 'test(facet_context)'`. Expected: missing context, patch, and error interfaces fail compilation.
- [ ] **Step 3: Implement `SurfaceRevision`, `SurfacePatch`, `SurfaceBuildError`, and `SurfaceRealizationContext`.** Derive `configuration_hash` from a canonical serialized algorithm/configuration record, include macro world bytes/source revision, and keep all deterministic state in the context. Do not use cache state, request order, or a new consumed stream.
- [ ] **Step 4: Implement terrain/climate composition.** Build the existing macro contexts once, use terrain curves as routing/geometry inputs, and compute blended material weights from temperature, moisture, elevation, slope, shoreline/water, sediment, and drainage. Do not add cloud, precipitation, or weather-qualified roughness members to `SurfacePatch`.
- [ ] **Step 5: Run focused tests and verify green.** Run the command from Step 2. Expected: all four context tests pass and repeated realization produces equal canonical patch bytes.
- [ ] **Step 6: Commit the composition root.** Run `cargo fmt --check`, then `git add windows/worldgen/src/facet.rs windows/worldgen/src/lib.rs windows/worldgen/tests/suite.rs windows/worldgen/tests/suite/facet_context.rs && git commit -m "feat: compose deterministic surface patches"`.

### Task 4: Add field continuity, mixed LOD, and refine/coarsen proofs

**Files:**

- Modify: `windows/worldgen/src/facet.rs`
- Test: `windows/worldgen/tests/suite/facet_context.rs`
- Create: `windows/scene/tests/fixtures/surface-seed-42-proof.json`
- Test: `windows/scene/tests/suite/facet_scene.rs`

**Interfaces:**

- Consume `SurfacePatch`, `FacetAddress`, `canonical_edge_sample`, `canonical_corner_sample`, `RealizedCurve`, and `SurfaceRevision`.
- Produce `pub fn stitch_transition(coarse: &SurfacePatch, fine: &SurfacePatch) -> Result<Vec<[u32; 3]>, SurfaceBuildError>` and `pub fn aggregate_children(parent: &SurfacePatch, children: &[SurfacePatch]) -> SurfacePatch`.
- `stitch_transition` accepts adjacent patches whose refinement levels differ by one and returns explicit transition topology; it never changes semantic curves or feature IDs.

- [ ] **Step 1: Write red tests for equal edges, face corners, unequal LODs, parent-child preservation, refine/coarsen, and fixture fields.** Add `adjacent_patches_agree_on_fields`, `face_corner_has_one_value`, `unequal_lod_transition_has_no_gap`, `children_preserve_parent_feature_ids`, `refine_then_coarsen_preserves_parent_samples`, and `proof_fixture_contains_required_surface_fields`.
- [ ] **Step 2: Run the tests and verify red.** Run `cargo nextest run -p hornvale-worldgen --test suite -E 'test(facet_context::adjacent) or test(facet_context::face_corner) or test(facet_context::unequal) or test(facet_context::children) or test(facet_context::refine)'`. Expected: missing stitching/aggregation functions or failed seam assertions.
- [ ] **Step 3: Implement canonical world-space field evaluation.** Evaluate borders and corners from shared address/feature inputs, include a derived border ring where needed, and keep feature curves independent of patch vertex placement.
- [ ] **Step 4: Implement mixed-LOD transition topology and aggregation.** Stitch a coarse edge to its corresponding fine child edges, preserve parent feature IDs, and aggregate child samples with the declared numeric tolerances. The operation must be pure and order-independent.
- [ ] **Step 5: Add the proof fixture and scene assertions.** Serialize only canonical fields required by the ground proof: terrain, channel/coast/ridge feature references, material weights, and revision metadata. Include a confluence, terminal basin, coast crossing, face corner, and unequal-LOD adjacency.
- [ ] **Step 6: Run focused tests and verify green.** Run the Stage 2 commands plus `cargo nextest run -p hornvale-scene --test suite -E 'test(facet_scene)'`. Expected: all seam, LOD, aggregation, and fixture tests pass.
- [ ] **Step 7: Commit the LOD contract and fixture.** Run `cargo fmt --check`, then `git add windows/worldgen/src/facet.rs windows/worldgen/tests/suite/facet_context.rs windows/scene/tests/fixtures/surface-seed-42-proof.json windows/scene/tests/suite/facet_scene.rs && git commit -m "test: prove surface patch continuity"`.

## Stage 3: Scene/source protocol and semantic client consumption

**Goal:** Expose derived patches through the existing scene/source boundary and make stale revisions, byte stability, narrow features, and renderer ownership executable.

**Success criteria:** Non-rendering observers can request the same patch; wrong or stale bindings are rejected; source-owned curves and fields reach Bevy; Bevy does not derive semantic rivers, coasts, biomes, or drainage.

### Task 5: Add scene and source patch documents

**Files:**

- Modify: `windows/scene/src/lib.rs`
- Modify: `clients/visual/source/src/protocol.rs`
- Modify: `clients/visual/source/src/lib.rs`
- Test: `windows/scene/tests/suite/facet_scene.rs`
- Test: `clients/visual/source/tests/suite.rs`
- Test: `clients/visual/source/tests/suite/surface.rs`

**Interfaces:**

- Produce in `windows/scene`: `pub struct SurfacePatchQuery { pub address: FacetAddress, pub expected_revision: SurfaceRevision }` and `pub fn surface_patch_scene(context: &SceneContext, query: &SurfacePatchQuery) -> Result<SurfacePatch, SceneError>`.
- Produce in `clients/visual/source/src/protocol.rs`: `pub(crate) struct SurfaceRequest { pub schema: String, pub binding: Binding, pub request_id: u64, pub address: FacetAddressWire, pub expected_revision: SurfaceRevisionWire }`, `pub(crate) struct SurfaceReply<'a> { pub schema: &'static str, pub binding: &'a Binding, pub request_id: u64, pub patch: &'a RawValue }`, `pub(crate) struct FacetAddressWire { pub macro_face: u32, pub child_path: Vec<u8> }`, and `pub(crate) struct SurfaceRevisionWire { pub source_revision: String, pub algorithm_version: String, pub configuration_hash_hex: String }`.
- Produce in `clients/visual/source/src/lib.rs`: `pub fn observe_surface(&mut self, request_json: &str) -> Result<String, SourceError>`. It rejects schema, binding, and expected-revision mismatches with `SourceError::InvalidRequest` and never returns a patch for a stale binding.
- Wire conversion is canonical: child-path order, feature list order, triangle order, and field order are stable before `RawValue` serialization.

- [ ] **Step 1: Write red tests for patch serialization, binding mismatch, stale revision, repeated bytes, and request IDs.** Add `surface_document_is_stable`, `surface_rejects_wrong_binding`, `surface_rejects_stale_revision`, `surface_preserves_request_id`, and `surface_contains_no_weather_hooks`.
- [ ] **Step 2: Run tests and verify red.** Run `cargo nextest run --manifest-path clients/visual/source/Cargo.toml --test suite -E 'test(surface)'`. Expected: missing protocol structs and `observe_surface` fail compilation.
- [ ] **Step 3: Implement scene conversion and canonical JSON.** Reuse `SceneContext`'s cached derived world and the worldgen context; keep patch data derived and outside the save format. Serialize stable IDs, integer topology, and protocol fields byte-for-byte.
- [ ] **Step 4: Implement source observation and stale-response behavior.** Validate the existing `Binding`, compare `expected_revision` to the active `SurfaceRevision`, and return a structured invalid-request error before generation on mismatch. Keep astronomy `Request`/`Reply` behavior unchanged.
- [ ] **Step 5: Run focused tests and verify green.** Run the command from Step 2, then `cargo nextest run -p hornvale-scene --test suite -E 'test(facet_scene)'`. Expected: all source and scene patch tests pass, including no weather hook fields.
- [ ] **Step 6: Commit the source protocol.** Run `cargo fmt --check`, then `git add windows/scene/src/lib.rs clients/visual/source/src/protocol.rs clients/visual/source/src/lib.rs windows/scene/tests/suite/facet_scene.rs clients/visual/source/tests/suite.rs clients/visual/source/tests/suite/surface.rs && git commit -m "feat: expose coherent surface patches"`.

### Task 6: Consume semantic patches in Bevy without moving ownership

**Files:**

- Modify: `clients/visual/bevy/src/documents.rs`
- Modify: `clients/visual/bevy/src/lifecycle.rs`
- Modify: `clients/visual/bevy/src/astronomy/surface.rs`
- Test: `clients/visual/bevy/tests/suite.rs`
- Test: `clients/visual/bevy/tests/suite/surface.rs`

**Interfaces:**

- Produce client-only deserialization types `SurfacePatchDocument`, `SurfacePatchVertex`, `SurfacePatchFeature`, and `SurfacePatchCacheKey { pub revision: String, pub macro_face: u32, pub child_path: Vec<u8> }` in `documents.rs`.
- Produce `pub fn schedule_surface_patch(key: SurfacePatchCacheKey, request: String) -> Result<(), ViewError>` and `pub fn apply_surface_patch(document: &SurfacePatchDocument) -> Result<SurfaceMeshHandles, ViewError>` in `lifecycle.rs`.
- Produce `pub fn surface_mesh(patch: &SurfacePatchDocument, transition: Option<&SurfacePatchDocument>) -> Mesh` and `pub fn narrow_feature_mask(patch: &SurfacePatchDocument, feature: &SurfacePatchFeature, position: [f32; 3]) -> f32` in `astronomy/surface.rs`.
- `apply_surface_patch` rejects a document whose revision differs from the active binding and uses `stitch_transition` output carried by the source; it does not calculate channel routing, coastline identity, drainage, or biome identity.

- [ ] **Step 1: Write red tests for deserialization, stale revision rejection, curve footprint, and unequal-LOD mesh topology.** Add `patch_document_round_trips`, `stale_patch_is_not_applied`, `curve_mask_survives_vertex_miss`, and `mixed_lod_mesh_has_no_boundary_gap` to the existing Bevy surface suite.
- [ ] **Step 2: Run tests and verify red.** Run `cargo nextest run --manifest-path clients/visual/bevy/Cargo.toml --test suite -E 'test(surface)'`. Expected: missing patch document and lifecycle interfaces fail compilation.
- [ ] **Step 3: Implement client document types and revision-aware cache keys.** Deserialize source-owned fields without deriving new semantic fields. Keep cache entries keyed by the full revision plus macro face and child path.
- [ ] **Step 4: Implement patch mesh/material conversion.** Use source-owned height, normal, weights, curve masks, shoreline distance, water depth, and ridge signals. Permit seeded cosmetic grain, foam, lighting, and fine displacement only after semantic fields are consumed.
- [ ] **Step 5: Implement mixed-LOD application and stale filtering.** Build transition topology from the source document and discard stale replies before asset insertion. Preserve the current source reset generation behavior.
- [ ] **Step 6: Run focused tests and verify green.** Run the command from Step 2. Expected: semantic fields round-trip, narrow features render without a vertex hit, stale revisions are discarded, and unequal-LOD meshes have matching boundaries.
- [ ] **Step 7: Commit Bevy consumption.** Run `cargo fmt --check`, then `git add clients/visual/bevy/src/documents.rs clients/visual/bevy/src/lifecycle.rs clients/visual/bevy/src/astronomy/surface.rs clients/visual/bevy/tests/suite.rs clients/visual/bevy/tests/suite/surface.rs && git commit -m "feat: render source-owned surface patches"`.

## Stage 4: Planetarium proof, measurement, and acceptance

**Goal:** Demonstrate the visual improvement on a real region, compare it with the current Planetarium, and measure generation latency, memory, and frame time before expanding scope.

**Success criteria:** The proof region shows coherent channels, coasts, ranges, and blended materials through an orbital-to-close-up move; fixtures cover all specified pathologies; measured costs are recorded; the current and revised renderings receive before/after visual review.

### Task 7: Add the Planetarium proof and measurements

**Files:**

- Modify: `clients/visual/planetarium/src/review.rs`
- Modify: `clients/visual/planetarium/src/capture.rs`
- Test: `clients/visual/planetarium/tests/suite.rs`
- Create: `clients/visual/planetarium/tests/suite/proof.rs`

**Interfaces:**

- Produce `pub struct SurfaceProofMetrics { pub seed: u64, pub patch_count: usize, pub refinement_levels: Vec<u8>, pub generation_latency_ms: Vec<u64>, pub peak_memory_bytes: Vec<u64>, pub frame_time_ms: Vec<f64> }` in `review.rs`.
- Produce `pub fn run_surface_proof(seed: u64) -> Result<SurfaceProofMetrics, String>` and `pub fn compare_surface_review(before: &CapturedFrames, after: &CapturedFrames) -> SurfaceReview`.
- Produce `pub struct SurfaceReview { pub required_features_visible: bool, pub seams_coherent: bool, pub rivers_reach_declared_ends: bool, pub coast_is_continuous: bool, pub mountain_direction_reads: bool, pub biome_transitions_are_blended: bool }`.
- `run_surface_proof` uses one real generated seed/region containing a confluence, terminal basin, coast crossing, face corner, and unequal-LOD adjacency, samples two local refinement levels, and records latency, peak memory, and frame time. It does not establish an unsupported numeric performance target.

- [ ] **Step 1: Write red proof tests for required fixtures and metrics.** Add `proof_region_contains_required_cases`, `proof_records_all_three_cost_dimensions`, `before_after_review_checks_required_features`, and `capture_provenance_names_source_owned_ground`.
- [ ] **Step 2: Run tests and verify red.** Run `cargo nextest run --manifest-path clients/visual/planetarium/Cargo.toml --test suite -E 'test(proof)'`. Expected: missing metrics, review, and proof functions fail compilation.
- [ ] **Step 3: Implement the proof harness.** Reuse existing Planetarium capture/review infrastructure and current binding/source setup. Record measurements for the selected region and LODs; keep the result as review evidence rather than a generated world artifact.
- [ ] **Step 4: Implement before/after review checks.** Compare current interpolated Planetarium frames against source-owned patch frames for visible rivers, continuous coasts, directional ranges, blended biome/material transitions, and seam behavior during orbital-to-close-up movement.
- [ ] **Step 5: Update provenance text.** Replace the current interpolated-ground wording in `capture.rs` with source-owned coherent ground, and retain the explicit statement that dynamic weather remains deferred.
- [ ] **Step 6: Run focused tests and verify green.** Run the command from Step 2. Expected: proof metadata, required fixture coverage, measurements, and before/after review checks pass.
- [ ] **Step 7: Commit the proof slice.** Run `cargo fmt --check`, then `git add clients/visual/planetarium/src/review.rs clients/visual/planetarium/src/capture.rs clients/visual/planetarium/tests/suite.rs clients/visual/planetarium/tests/suite/proof.rs && git commit -m "test: add coherent ground proof review"`.

### Task 8: Run campaign gates and record expansion evidence

**Files:**

- Modify: `docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md`
- Modify: `docs/audits/campaign-reconciliation.tsv`
- Test: existing stage and merge gate rosters from `scripts/lane-sets.tsv`

**Interfaces:**

- Consume all prior task interfaces and the committed proof metrics.
- Produce no runtime interface. Record the observed proof region, fixture results, measurements, and any deferred expansion decision in the campaign ledger.

- [ ] **Step 1: Run the local commit gate.** Run `make gate-commit` in the campaign worktree. Expected: formatting, lint, type/placement/plumb checks, report freshness, and the sub-floor roster pass.
- [ ] **Step 2: Run the complete visual and workspace checks required by the existing rosters.** Run the repository's documented `make sluice-stage BRANCH=<branch> REF=<full-sha>` through the canonical queue after the branch SHA exists; never run census regeneration locally. Expected: the stage gate evaluates the actual merge product and reports green or an actionable failure.
- [ ] **Step 3: Review proof evidence against scope.** Confirm fixtures cover confluences, terminal basins, coast crossings, face corners, unequal LODs, and refine/coarsen; confirm no cloud/precipitation hooks or save/epoch/new-stream changes entered the campaign.
- [ ] **Step 4: Record measurements and the expansion decision.** Add the observed latency, memory, and frame-time results to the committed ledger. Record whether the evidence supports more refinement levels or a broader region; no expansion is accepted from intuition alone.
- [ ] **Step 5: Commit campaign evidence.** Before committing, run `pwd && git branch --show-current` and confirm the campaign worktree and branch. Then run `git add docs/superpowers/ledgers/2026-09-11-the-coherent-ground.md docs/audits/campaign-reconciliation.tsv && git commit -m "docs: record coherent ground proof evidence"`.

## Spec coverage self-review

The plan maps every revised G3 requirement to an implementation task:

- Inherited macro/topological routing versus realized bed geometry: Task 2.
- Headwaters, terminal basins, outlets, mouth attachment, confluences, and outside-neighborhood continuation: Task 2 and Task 4.
- Persistent feature identity versus sampling resolution: Task 1 and Task 4.
- Canonical edge/corner evaluation: Task 1 and Task 4.
- Mixed-LOD stitching and parent-child preservation: Task 4 and Task 6.
- Independent narrow-feature curves/fields/adaptive sampling: Task 1, Task 2, and Task 6.
- `windows/worldgen` composition ownership and dependency layering: Task 3.
- `world_revision`, configuration identity, and stale-response behavior: Task 3, Task 5, and Task 6.
- Hash labels versus consumed streams: Global Constraints, Task 1, and Task 3.
- Exact bytes versus numeric tolerances: Global Constraints, Task 1, Task 3, and Task 5.
- No cloud/precipitation hooks: Global Constraints, Task 3, Task 5, and Task 8.
- Fixtures for confluences, terminal basins, coast crossings, face corners, unequal LODs, and refine/coarsen: Task 4 and Task 7.
- Before/after visual review and latency/memory/frame-time measurements: Task 7 and Task 8.
- Existing scene region system, terrain channel/branch machinery, source binding, and Planetarium/Bevy consumers: Verified starting points plus Tasks 2, 3, 5, 6, and 7.

The plan contains no unassigned implementation names: every new type or function
is introduced in the task that produces it before later tasks consume it. The
plan introduces no cloud or precipitation interface, no new random stream, no
save-format field, and no second integration-test binary in any crate.
