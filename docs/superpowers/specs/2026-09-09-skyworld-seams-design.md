# Skyworld Seams — design

**Campaign:** The Skyworld Seams · **Branch:** `campaign/skyworld`
**Status:** G3 approved; Tasks 1–4 complete, including review-hardened seam contracts; awaiting the campaign's normal review and close process

## 1. Purpose

The first Skyworld slice proved that a sparse mobile habitat can be generated
and rendered over the existing land/sea surface. This follow-up tests the
boundary between that overlay and the existing biome, terrain, climate, and
composition-root systems before adding organisms, societies, lifecycle
mutation, or new species.

The goal is not to rename world concepts as sugars or replace the biome model.
The goal is to learn whether the current environment axes are sufficient to
feed multiple habitat overlays, and to make the cost and ownership of that
boundary explicit.

## 2. Scope and non-goals

This campaign will:

- define one read-only Skyworld input seam over the already-generated terrain
  and climate values;
- exercise that seam against land, ocean, altitude, moisture, temperature,
  wind, aether, radiation, and environmental feature variation;
- measure whether generation and rendering re-use existing artifacts rather
  than reconstructing terrain or climate per territory or per pixel;
- preserve the fixed `Biome`/`BiomeExpr` taxonomy and the existing `BuildDepth`
  ladder;
- record which quantities are fields, stocks, derived habitat attributes, and
  rendered consequences, so a later Waterworld or Underworld overlay can reuse
  the boundary honestly.

This campaign will not add a new `BuildDepth` rung, serialize Skyworld state,
simulate lifecycle transitions, materialize organisms or species, add
tethering, mutate the atmosphere, or introduce a full fluid/particle field.
It will not clean or reinterpret unrelated uncommitted changes in the primary
checkout.

## 3. Architecture

The existing composition root remains the owner of cross-domain composition.
`skyworld_from(world, terrain, climate, config)` remains the canonical pure
derivation entry point. The campaign may add a small named input/readout seam
beside it, but it will not make the terrain or climate domains depend on
Skyworld and will not hide reconstruction behind a renderer.

The seam has four layers:

1. **Surface substrate:** fixed terrain classification, elevation, ocean mask,
   biome expression, and environmental features.
2. **Atmospheric fields:** stable Skyworld pressure, density, temperature,
   moisture, wind, radiation, aether, and astronomical forcing.
3. **Habitat derivation:** phenotype, stocks, trajectory, adjacency, and the
   four footprints already defined by the first campaign.
4. **Observation:** ordinary and diagnostic render/readout lenses, which may
   consume the derived overlay but never alter or lazily rebuild its inputs.

The seam should be explicit enough that a test can identify which layer owns a
quantity and whether a consumer is reading an existing value or deriving a
new one. If a wrapper would merely rename existing values without clarifying
ownership or cost, keep the direct typed inputs instead.

## 4. Environment-axis experiment

The campaign will build a small deterministic matrix of perturbations over
the existing fixture, varying one environmental concern at a time where the
repository already provides a stable pin or derived value. The probes will
ask:

- does land/ocean classification remain a substrate input rather than a
  second Skyworld biome taxonomy?
- do temperature, moisture, elevation, wind, and environmental features feed
  distinct Skyworld quantities rather than one hidden productivity scalar?
- can high-sky radiation and aether be varied independently while the surface
  biome remains unchanged?
- do volcanic or tectonic features affect coverage/distribution without
  becoming an authored placement rule?
- does changing a surface input alter only the dependent Skyworld fields,
  stocks, footprints, or rendering consequences that should respond?

Every perturbation must prove that it changed the intended input before
asserting a downstream difference. A no-op perturbation is a failed probe,
not a green result.

## 5. Cost and ownership experiment

The campaign will instrument or otherwise count the existing construction
seams sufficiently to establish these properties:

- one already-built terrain and climate artifact can serve all territories;
- rendering a moving territory does not resculpt the planet or derive a dense
  planet-by-time field;
- detail selection changes materialization, not generated Skyworld state;
- repeated queries are byte-identical and do not consume random streams;
- work scales with active territories, requested samples, and rendered pixels,
  not with the product of all vertices and all times.

The measurement will report actual command output and counts in the ledger.
No census refresh is part of this campaign.

## 6. Cross-realm transfer test

The result will identify the smallest shared contract a later Waterworld or
Underworld mobile habitat could use: surface substrate, ambient prerequisite
fields, aggregate stocks, movement/adjacency, propagation channels, and
ordinary/diagnostic observation. It will also name the dimensions that must
remain realm-specific, such as vents, currents, cave energy, aether bands, or
surface projection.

The campaign will not implement those other realms. A shared abstraction is
successful only if it removes a real duplicated rule without erasing the
world-specific behavior the user wants to preserve.

## 7. Determinism and compatibility

All new draws, if any are proved necessary, use separately labeled streams.
Existing stream order, surface biome outputs, `BuildDepth` prefix behavior,
save-format facts, and ordinary renderer output remain byte-stable unless a
measured defect is explicitly promoted to a new epoch decision.

Tests will cover same-seed identity, cross-seed variation, perturbation
non-vacuity, surface preservation, render/readout immutability, and bounded
construction work. Generated audit reports are refreshed only when the gate
requires them; censuses remain queued work and are not run locally.

## 8. Deliverables

- a committed implementation plan with three to five bounded tasks;
- focused seam and cost probes in the existing worldgen suite;
- only the smallest code seam justified by those probes;
- a ledger entry recording measured ownership, cost, reusable contract, and
  rejected abstractions;
- explicit follow-ups for lifecycle, co-evolution, tethering, species, and
  Waterworld transfer rather than accidental scope growth.

## 9. G3 review questions

1. Is a diagnostic seam-and-cost campaign the right next step before adding
   Skyworld organisms, species, or lifecycle mutation?
2. Should the campaign keep `BuildDepth` unchanged while it measures the
   boundary, as specified here?
3. Does the four-layer contract preserve the worldly model while still
   giving later realms a useful shared shape?
