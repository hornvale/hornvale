# The Coherent Ground: Visible Integration and Facet Detail

Status: design amendment after G6 review of the first implementation slice.

This document reopens the Coherent Ground campaign to close four concrete
gaps found after the stage gate passed: source-owned patches are not on the
normal Planetarium render path, facet height remains macro interpolation,
narrow features can miss every mesh vertex, and the visual proof measures
construction rather than visible rendering.

## 1. Goal

Make the existing source-owned surface realization visible and materially
useful in the Planetarium. A close-up must show locally varied terrain,
continuous hydrology, readable ridges and coasts, and blended material
transitions without visible patch seams. The audience's standard is a
convincing fantasy world, not geological correctness.

The Level-6 terrain and climate remain the macro authority. This amendment
adds a rendered local realization; it does not promote local detail into a
new canonical macro world.

## 2. Findings this amendment addresses

The first slice established useful contracts and tests, but its ordinary
render path still builds the original globe mesh. `schedule_surface_patch`
and `apply_surface_patch` are exercised by tests and proof code, not by the
normal visible scene lifecycle. The patch mesh/material result is not
inserted into Bevy assets or spawned as a render entity.

The worldgen evaluator currently derives refined heights primarily by
blending Level-6 corner elevations. Channel bed and ridge helpers exist, but
their results do not modify the emitted terrain field. The renderer applies
feature masks at vertices, so a feature narrower than the sample spacing can
remain invisible even when its source curve is present.

The existing Planetarium proof similarly validates patch construction and
manually supplied review facts. It does not establish that a rendered frame
changed or that the new patch was visible during an orbital-to-close-up move.

## 3. Design

### 3.1 Visible patch lifecycle

The Planetarium receives a revision-qualified surface binding and maintains a
small visible-region patch set. The set is derived from the camera's current
globe-facing region and a bounded ring of neighbors. Each entry is keyed by
the full surface revision, macro face, child path, and render generation.

The lifecycle is:

1. Compute the desired patch addresses from the camera envelope.
2. Schedule missing addresses through the existing source request path.
3. Reject stale replies using the existing binding, revision, and generation
   checks before asset insertion.
4. Convert source-owned samples, triangles, and feature strips into Bevy
   mesh/material assets.
5. Insert the assets and spawn or replace patch entities atomically for the
   current generation.
6. Keep the previous visible set until replacement patches are ready, then
   retire only the superseded entities.

The legacy globe remains a loading/fallback representation. Once the first
valid patch set covers the camera's visible region, that region is rendered by
patch entities. The fallback must never overlap a ready patch in a way that
creates a duplicate surface or z-fighting.

Patch selection is intentionally bounded. It must not request the whole
planet, and it must not make patch generation depend on request completion
order. The renderer may choose a coarser child path farther from the camera
and a finer child path near the camera, subject to the existing one-level
transition contract.

### 3.2 Conditioned facet geometry

The emitted height is a deterministic composition of four terms:

```text
height(position) = macro_height(position)
                  + conditioned_relief(position)
                  + hydrology_bed(position)
                  + ridge_structure(position)
```

`macro_height` is the existing Level-6 evaluation and remains dominant.
`conditioned_relief` is seeded hash noise whose amplitude is bounded by
macro slope, elevation class, coast distance, and neighboring macro heights;
it is zero at shared boundaries or evaluated from shared world-space inputs so
adjacent patches agree.

`hydrology_bed` is derived from the inherited channel/rill skeleton. It uses
the signed distance to realized curves, their width, local downstream grade,
and terminal kind to carve a center bed, banks, floodplain, terrace, and
delta profile. It may not reroute flow or invent a competing drainage graph.

`ridge_structure` uses the existing ridge direction/strength signals plus
conditioned anisotropic noise. It creates directional mountain texture rather
than isolated random bumps, and its amplitude is bounded near coasts and
hydrology so it cannot create implausible basin crossings.

The field evaluator must expose enough intermediate values for tests to prove
that the added terms are active. A test that only observes the helper output
without comparing the emitted patch is insufficient.

### 3.3 Narrow features

Feature identity, routing, and terminal metadata remain source-owned. For
rendering, each visible curve emits a patch-local strip or ribbon with:

- stable feature ID and terminal metadata;
- world-space centerline points and width;
- signed distance or side coordinate;
- source-owned material/semantic mask;
- boundary continuation information.

The strip is sampled adaptively from curve curvature and width, not from the
terrain vertex grid alone. It is clipped or continued at patch boundaries
using the canonical address and feature endpoint. A feature that misses every
terrain vertex must still produce visible geometry or a visible fragment mask.

Bevy may add cosmetic foam, lighting, grain, and displacement after consuming
these fields. It may not decide whether a river, coast, ridge, biome, or
drainage feature exists.

### 3.4 Actual visual proof

The proof harness must capture rendered output at a fixed viewport and camera
script, with the same seed, binding, and revision for both captures. The
before capture uses the fallback/interpolated path; the after capture uses the
ready patch set. Review functions inspect the captured frame artifacts and
the associated source/render metadata.

The proof must establish all of the following:

- the after frame contains visible patch entities and differs from before;
- a narrow feature remains visible when its centerline misses terrain
  vertices;
- rivers retain a readable source-to-terminal direction through a close-up;
- coastlines cross a continuous shoreline rather than a straight binary edge;
- ridge structure has directional continuity across adjacent patches;
- equal-resolution and mixed-LOD boundaries do not show gaps or overlaps;
- patch activation does not leave fallback geometry underneath ready patches.

Measurements are recorded separately for source generation, mesh/material
application, first visible frame, steady-state rendered frame time, and peak
RSS. The proof records actual capture paths or content digests; booleans
supplied by the test are not accepted as visual evidence.

## 4. Ownership and compatibility

- `domains/terrain` owns macro-conditioned feature primitives and curve
  geometry inputs.
- `windows/worldgen` owns composition of terrain and climate into patch
  fields and revision identity.
- `windows/scene` and `clients/visual/source` own the derived patch protocol.
- `clients/visual/bevy` owns patch entity lifecycle and conversion to render
  assets, but not semantic feature decisions.
- `clients/visual/planetarium` owns the camera-driven proof script and capture
  review.

No save-format, epoch, consumed stream, or Level-8 macro change is allowed.
Dynamic clouds, precipitation advection, currents, snow evolution, foam
animation, and weather-qualified roughness remain outside this amendment.

## 5. Staged delivery

1. **Visible vertical slice:** one source patch travels from camera selection
   through request, Bevy asset insertion, entity spawn, and captured frame.
2. **Conditioned relief:** add bounded facet-scale relief, hydrology bed
   carving, and anisotropic ridge structure with boundary and activity tests.
3. **Narrow features and LOD:** add adaptive feature strips, fallback handoff,
   and mixed-LOD patch replacement with no gap or overlap.
4. **Rendered proof and close:** capture before/after frames, record actual
   metrics, correct the ledger/status documentation, and rerun stage/merge
   gates.

Each stage must have focused tests and a review before the next stage. The
implementation plan must name exact files and commands after this design
passes its review gate.

## 6. Acceptance and risks

The campaign is accepted only when the rendered proof demonstrates visible
source-owned patches and the required continuity cases. A green contract
test suite without a changed rendered frame is not sufficient.

The principal risk is performance from too many visible patches or adaptive
strips. Start with one camera-facing patch plus neighbors, measure, and widen
only if the proof needs it. The principal correctness risk is a second
hydrology authority; all local carving must be a geometric realization of
the inherited routing skeleton.

## 7. G3 package status

This amendment incorporates the independent G6 findings. The prior stage
gate's green result remains valid for the contracts it exercised, but it does
not close this amendment's rendered-visibility acceptance criteria.
