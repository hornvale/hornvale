# The Coherent Ground — design

**Date:** 2026-09-11  
**Status:** Revised after Astra review — awaiting G3 approval (campaign-autopilot hard stop).
**Campaign:** The Coherent Ground  
**Predecessor context:** the Planetarium currently presents Level-6-derived
surface data through interpolation and presentation noise; existing terrain
work provides a coarse drainage graph and channel polylines but not a complete
facet-scale surface realization.

## 1. Goal and contract

Give the Planetarium a convincing local surface whose detail is generated at
facet scale while remaining legible as one world. The system is allowed to
invent detail, but not contradictions.

The first campaign must preserve these contracts:

- Level 6 remains the authoritative macro world.
- Generation is deterministic and independent of request order, cache state,
  and renderer choice.
- Semantic fields are generated outside Bevy and can be consumed by more than
  one observer.
- No save-format change, epoch change, new random stream, or global Level-8
  macro rewrite is required for the first slice.
- Renderer noise may alter microappearance only; it may not discover semantic
  rivers, coastlines, drainage divides, or biome identities.

“Correct” here means perceptually coherent: flows join or terminate
meaningfully, coastlines read as continuous forms, terrain transitions do not
form arbitrary tile boundaries, and ranges read as directional landforms.

## 2. Three-scale architecture

### 2.1 Macro world: authority

The existing Level-6 world supplies the slow, world-defining facts:

- elevation and sea-level relation;
- climate and broad biome suitability;
- drainage directions, catchments, outlets, and existing channel skeleton;
- tectonic, crustal, sediment, and lithology signals where available.

These facts constrain realization. A facet patch can add a tributary, bank,
meander, shelf, or local relief feature, but it cannot silently replace the
macro basin structure.

### 2.2 Facet realization: coherent semantics

The new layer derives a local patch on demand from macro authority plus a
stable realization seed. It is the owner of semantic local geometry and
continuous surface fields. It is not a third macro world; it is a conditional
refinement of one.

### 2.3 Renderer: presentation

Planetarium schedules visible patches, requests them, caches them, selects an
LOD, and turns their fields into meshes and materials. The renderer may add
fine displacement, foam, cloud billows, lighting, and temporal animation, but
those effects cannot change the patch's semantic answers.

## 3. Patch identity and addressing

The patch identity is:

```text
{ world_revision, macro_face, refinement_level, time_bucket }
```

`macro_face` identifies a Level-6 icosphere face. `refinement_level` subdivides
that face locally; level 1 has four children and level 2 has sixteen children,
providing a regional close-up roughly comparable to two local subdivision
levels without making the whole planet pay for them. `time_bucket` is reserved
for fields that are explicitly time-qualified; the first ground slice may use
a fixed static bucket.

The realization function must be pure with respect to this identity and its
declared macro inputs. Neighbor context is part of the derivation input, not
an incidental cache lookup. Cache eviction and request order therefore cannot
alter bytes or geometry.

Feature identity and sampling resolution are separate. A river reach, confluence,
shoreline segment, ridge axis, and material transition receive stable semantic
identities derived from macro authority and labeled realization inputs. A patch
and its children are samples of those features at a chosen resolution; changing
`refinement_level` must not create a second river or move an existing mouth.

`macro_face` plus a child path addresses a patch: the empty path is the whole
Level-6 face, and each subsequent base-4 digit selects one of its four
children. The path length is `refinement_level`. Child boundaries use the
same canonical edge and corner evaluator as their parent. An evaluator takes a
world position and the stable feature identity, so a boundary value is computed
from both sides' identical inputs rather than copied from whichever patch was
generated first. A patch may carry a border ring for stitching, but that ring
is derived data and is not part of identity.

Mixed-resolution neighbors are legal. The renderer stitches a coarse edge to
the corresponding fine child edges with explicit transition topology, while
semantic curves and fields continue to be evaluated in world space. Parent
samples are preserved by the child aggregate within the declared numerical
tolerance; refining and then coarsening therefore returns the parent sampling
contract. The first proof must include unequal-LOD neighbors, face corners,
and refine/coarsen cycles.

## 4. Patch data contract

The exact Rust types belong to implementation planning, but the semantic
contract should expose enough information for both terrain and future surface
systems:

- vertex position, height, slope, and normal;
- continuous material weights rather than only one categorical biome;
- shoreline distance, water depth, and shallow-water/shelf weights;
- flow direction and strength;
- channel distance and width;
- floodplain, bank, terrace, and delta weights;
- ridge direction/strength or an equivalent anisotropic mountain signal;
- source-owned semantic feature references needed to render narrow channels,
  shorelines, and ridges even when no patch vertex lies on the feature.

The first implementation should not add fields merely to anticipate every
future renderer. It should prove the fields required by the ground proof
slice, with an additive extension path for living weather.

## 5. Realization laws

### 5.1 Terrain

Construct local height as a conditioned composition:

```text
macro surface
+ low-amplitude, macro-conditioned relief
+ hydrology incisions and banks
+ sediment/floodplain/delta shaping
```

Relief noise is masked and anisotropic where the macro world indicates ranges,
coasts, plains, or channels. It must not be an unbounded independent noise
field that overwhelms the macro surface.

### 5.2 Rivers and drainage

The macro drainage graph and existing channel network own topological routing:
they answer which coarse vertex drains into which, which catchment contains a
reach, and where the coarse path terminates. Facet realization owns the local
bed geometry around that routing. Existing `channel` and `rill` outputs are
inputs to that realization; they do not already provide geometric containment
for arbitrary refined patches.

For this contract, a headwater is a realized channel source whose parent
catchment has no upstream realized channel at that scale. A confluence is a
join whose child reaches share a stable downstream reach identity and whose
mouth positions agree in world space. A terminal basin ends at a macro sink or
identified lake, with no claim of ocean delivery. An outlet reaches the ocean
or a named lake through the macro graph. A river mouth is the final channel
attachment to the shared shoreline field, shelf, or lake boundary; it is not
an arbitrary patch edge.

Derive continuous centerlines, widths, banks, floodplains, terraces, and
deltas around the skeleton. Discharge influences width; stable local variation
can shape meanders and secondary channels. New tributaries may be invented only
when they remain in the parent catchment, join a valid downstream feature, and
satisfy a bounded flow budget. They are visual/local realization, not new macro
drainage authority. Each feature carries endpoint metadata identifying its
upstream source, downstream continuation, and terminal kind. If a continuation
leaves the loaded neighborhood, the patch emits a continuation token anchored
to the canonical feature and boundary position; the neighboring request
resolves that token without requiring the original patch to generate the whole
downstream path.

The realized bed must descend, within tolerance, along its inherited downstream
direction until its declared outlet, lake, or ocean attachment. A boundary is
never a termination reason.

### 5.3 Coasts and water

Derive a continuous shoreline field from elevation versus sea level, then
condition it with shelf depth, sediment/coastal character, river mouths, and
bounded coastal variation. The same field drives shoreline position, shallow
water, beach/sediment material, water depth, and roughness inputs. This keeps
the coast from becoming a hard line in one system and a different line in
another.

### 5.4 Mountains and ranges

Use directional, anisotropic structure informed by macro elevation, tectonic
or crustal signals, and lithology where present. Ridges should have a stable
axis and coherent shoulder/valley relationship across patches. Isotropic noise
blobs are suitable for local texture, not for inventing a mountain system.

### 5.5 Biomes and materials

Compute blended weights from temperature, moisture, elevation, slope,
shoreline/water state, sediment, and drainage context. Hard labels may remain
available for queries, but the visual surface should use weights and smooth
transition rules. This avoids arbitrary categorical borders while preserving
legible biome identity.

## 6. Continuity invariants

The first implementation should make these testable, not merely visual hopes:

1. Adjacent patches agree on boundary position, height, normal, and semantic
   field values within declared numerical tolerances. Canonical edge and corner
   evaluation is the oracle; generated bytes are not compared when floating
   representation may differ.
2. A channel crossing a boundary has matching feature identity and
   endpoint/continuation metadata; it does not appear, vanish, reverse, or jump
   basins at the seam. Confluence mouths coincide within the geometry
   tolerance.
3. Every realized headwater has a valid source; every confluence has one
   downstream identity; every terminal basin reaches its sink; every outlet or
   mouth reaches its declared lake or ocean attachment. A continuation token is
   sufficient evidence when the downstream patch is not loaded.
4. Along each realized channel centerline, bed elevation is non-increasing in
   the inherited downstream direction, with only the configured mouth/lake
   settling tolerance permitted. The test samples interior points, not only
   endpoints, so an uphill river cannot pass on topology alone.
5. Parent-child samples preserve feature identity and canonical boundary values;
   unequal-LOD seams, face corners, and refine/coarsen cycles satisfy the same
   geometry and field tolerances.
6. Material weights are bounded, sum to one within tolerance, and vary without
   arbitrary patch-local category changes.
7. Coastline, shallow-water, beach, and water-depth fields agree about which
   side of the shoreline is land or water; river mouths attach to that same
   shoreline field.
8. Narrow features remain represented by source-owned curves or adaptive field
   samples when no patch vertex lies on them. Their rendered footprint agrees
   with the curve distance field within the chosen sampling tolerance.
9. The same patch request returns the same result regardless of cache state,
   generation order, or neighboring patch request order. Hash-derived labels
   are random-access inputs; consumed streams retain their declared draw order.

These are coherence guarantees, not a claim of geological simulation fidelity.

## 7. Ownership and data flow

```text
Level-6 world + hydrology/channel skeleton
                 |
                 v
        facet realization layer
                 |
        patch fields + geometry
          /                    \
         v                      v
 Planetarium renderer      future observers/tools
         |
    microappearance only
```

Terrain and climate domains own their macro inputs. The composition owner is
`windows/worldgen`, which already composes the generated world and is the
repository layer permitted to combine domain outputs; realization laws must
remain expressed through domain-facing providers rather than a sibling domain
dependency. `clients/visual/source` owns the derived patch request and reply
protocol, analogous to its existing source binding. Planetarium owns
scheduling, LOD, cache lifetime, and mesh or material conversion. No client may
become the authority for semantic surface facts.

`world_revision` is the full source revision/binding identity plus the
realization configuration identity: macro world bytes, algorithm/schema
version, labeled hash seeds, and static calibration parameters. A configuration
change creates a new revision and invalidates older patches. The source rejects
a request whose binding or revision does not match its loaded world; the
renderer discards a reply whose revision is stale for the active observation.
No request-order state, cache state, or thread scheduling may enter the
realization function. New hash-derived labels do not consume a `Stream` and
therefore do not alter existing draw order; any future consumed stream requires
an explicit stream label and save/epoch review.

The contract distinguishes exact bytes from numerical agreement. Stable IDs,
integer topology, field ordering, and serialized protocol values are compared
byte-for-byte where they are emitted as canonical data. Positions, normals,
heights, distances, and weights use documented numerical tolerances and must
also satisfy boundedness and monotonicity properties.

## 8. Campaign scope

### In

- stable patch identity and local subdivision;
- deterministic patch generation with neighbor context;
- conditioned relief and anisotropic ridge detail;
- rivers, tributaries, banks, floodplains, terraces, and deltas;
- continuous coast/shelf/shallow-water fields;
- blended biome/material fields;
- Planetarium LOD, scheduling, caching, and seam-safe mesh consumption;
- focused continuity and determinism tests.

### Out

- global Level-8 macro generation;
- save/epoch changes and new random streams;
- full dynamic cloud simulation;
- precipitation advection, currents, snow evolution, foam animation, and
  weather-driven water roughness;
- a new independent hydrology solver that competes with the macro drainage
  system;
- renderer-specific semantic generation;
- cloud or precipitation hooks in the ground patch contract.

The deferred weather work is a follow-up campaign, “The Living Surface,”
which should consume this spatial contract.

## 9. Proof slice and staged delivery

The first proof uses one real generated seed/region, two local refinement
levels, adjacent patches, unequal-LOD neighbors, and a region containing a
confluence, terminal basin, coast crossing, and face corner. It is viewed both
as a whole globe and during an orbital-to-close-up camera move. Narrow rivers,
shorelines, and ridges must remain visible when their centerline falls between
patch vertices, through source-owned curves, distance fields, or adaptive
geometry. Success means the refined view visibly adds structure while
preserving rivers, coasts, mountain direction, biome transitions, and patch
seams. The proof records generation latency, memory use, and frame time for
the selected region and LODs; those measurements inform expansion without
assuming a performance budget in advance.

Acceptance includes a before/after visual review against the current
Planetarium, plus fixture and property coverage for confluences, terminal
basins, coast crossings, face corners, mixed LODs, and refine/coarsen cycles.
Expansion to more refinement levels or a broader world region requires those
measurements and a review of the visual result.

Implementation planning should break delivery into three to five stages. A
likely decomposition is:

1. patch identity, addressing, and deterministic boundary evaluation;
2. conditioned terrain/material fields and seam tests;
3. hydrology realization and river/coast continuity;
4. Planetarium LOD/caching and proof-slice capture;
5. cleanup, focused review, and campaign gates.

The stage plan must be written only after G3 approval of this design.

## 10. Risks and decisions intentionally deferred

- A patch can be locally plausible yet globally misleading if neighbor context
  is too small. The first proof must measure seam and outlet failures before
  broadening the context window.
- A topology-preserving channel can still have an implausible bed profile or
  miss its rendered footprint. Bed monotonicity and curve-to-sample agreement
  are separate tests and both are required.
- A curve crossing an unloaded boundary can be orphaned if continuation tokens
  are not stable. Tokens must name a persistent feature and canonical boundary
  parameter, not a transient patch index.
- Recursive tributary invention can create visual richness but also clutter;
  cap it by scale, parent discharge, and visible importance.
- More refinement levels improve close-up structure but increase generation,
  memory, and cache pressure. Measure the proof slice before choosing a default
  maximum.
- The exact relationship between existing `channel`/`rill` outputs and the
  patch compiler is unresolved until the implementation audit. Reuse the
  existing skeleton where possible; do not assume its current 1-D output is
  already a complete patch contract.
- Dynamic clouds and precipitation need a time model and ownership decision;
  they remain intentionally outside this ground campaign, with no placeholder
  fields in its patch contract.

## 11. Definition of done for this design phase

- [x] User-approved goal: convincing coherence over geological correctness.
- [x] Level-6 authority and local refinement boundary recorded.
- [x] Semantic facet realization separated from renderer microappearance.
- [x] Scope split between Coherent Ground and Living Surface recorded.
- [x] Proof slice and continuity invariants specified.
- [x] Astra review findings incorporated: hydrology, LOD, narrow-feature,
  ownership/determinism, and acceptance contracts revised.
- [ ] G3 review and approval.
