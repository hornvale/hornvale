# The Coherent Ground — design

**Date:** 2026-09-11  
**Status:** Draft — awaiting G3 review (campaign-autopilot hard stop).  
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
- static cloud/precipitation hooks only if they can be derived without
  pretending that dynamic weather is already implemented.

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

Use the existing channel network as a semantic skeleton. Derive continuous
centerlines, widths, banks, floodplains, terraces, and deltas around it.
Discharge influences width; stable local variation can shape meanders and
secondary channels. New tributaries may be invented only when they remain in
the parent catchment, join a valid downstream feature, and satisfy a bounded
flow budget. They are visual/local realization, not new macro drainage
authority.

A channel may terminate at an identified outlet, lake, or ocean. It must not
end because a patch boundary was reached. Patch boundaries therefore require
endpoint metadata or a shared evaluation rule so both sides agree on the same
continuation.

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
   field values within the chosen tolerance.
2. A channel crossing a boundary has matching endpoint/continuation metadata;
   it does not appear, vanish, reverse, or jump basins at the seam.
3. Flow direction follows the conditioned height/drainage law and ultimately
   reaches a valid outlet, lake, or ocean in the realized neighborhood.
4. Material weights are bounded, sum to one within tolerance, and vary without
   arbitrary patch-local category changes.
5. Coastline, shallow-water, beach, and water-depth fields agree about which
   side of the shoreline is land or water.
6. The same patch request returns the same result regardless of cache state,
   generation order, or neighboring patch request order.

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

Terrain and climate domains own macro inputs and realization laws appropriate
to their boundaries. A scene/visual source layer should own the patch request
and reply shape. Planetarium owns scheduling, LOD, cache lifetime, and mesh or
material conversion. No client may become the authority for semantic surface
facts.

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
- renderer-specific semantic generation.

The deferred weather work is a follow-up campaign, “The Living Surface,”
which should consume this spatial contract.

## 9. Proof slice and staged delivery

The first proof uses one real generated seed/region, two local refinement
levels, and adjacent patches. It is viewed both as a whole globe and during an
orbital-to-close-up camera move. Success means the refined view visibly adds
structure while preserving rivers, coasts, mountain direction, biome
transitions, and patch seams.

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
  they remain intentionally outside this ground campaign.

## 11. Definition of done for this design phase

- [x] User-approved goal: convincing coherence over geological correctness.
- [x] Level-6 authority and local refinement boundary recorded.
- [x] Semantic facet realization separated from renderer microappearance.
- [x] Scope split between Coherent Ground and Living Surface recorded.
- [x] Proof slice and continuity invariants specified.
- [ ] G3 review and approval.

