# The Vent — Waterworld pressure test

**Campaign:** The Vent  
**Branch:** `campaign/the-vent`  
**Status:** G3 review requested  
**Precedent:** Skyworld and Skyworld Seams; The Fathom; The Freshwater; The
Underworld climate corpus

## 1. Purpose

This campaign tests whether Skyworld's four-layer habitat contract survives in
water when the ambient prerequisites are currents, pressure, light,
temperature, and hydrothermal chemistry. It is a deliberately small,
read-only, deterministic Waterworld overlay and seam audit.

The campaign preserves the worldly model. “Stocks and flows” is the reasoning
vocabulary; the implementation continues to use terrain, climate, biome,
fields, stocks, currents, corridors, and observations as their own nouns.

## 2. Scope and non-goals

### In scope

- A composition-root Waterworld overlay over already-built terrain and climate.
- Seabed and water-column substrate readouts using existing marine facts.
- Independent ambient fields for light penetration, pressure, temperature,
  salinity, and hydrothermal chemistry.
- Derived, seeded, sparse vent sources whose location is conditioned by
  existing seabed/volcanic context rather than an authored placement list.
- Aggregate plankton, chemosynthetic bloom, nutrient, and kelp/reef-like
  stocks, without individual microbes or organisms.
- Sparse current corridors and a bounded vertical migration/refresh cycle.
- Ordinary consequence-oriented and diagnostic cause-oriented readouts/rendering.
- Tests for independence, non-vacuity, stream compatibility, purity, bounds,
  and absent-overlay compatibility.

### Out of scope

- Sea elves, mermaids, tritons, sea dragons, kraken, turtle-islands, or
  submerging islands.
- Species, metabolism, reproduction, population, culture, GOAP, or lifecycle
  mutation systems.
- A new biome taxonomy or sweeping biome refactor.
- A universal habitat wrapper or renamed “sugar” ontology.
- A full 3D fluid solver, dense planet-by-time cache, or per-microbe model.
- Persistent vent evolution, reef succession, or authored named locations.
- New save facts, a `BuildDepth` rung, or a census refresh.

## 3. Existing seam and ownership

The overlay belongs in `windows/worldgen`, which already composes terrain and
climate for Skyworld and Underworld consumers. It receives direct typed
references to the already-built artifacts and does not make either domain
depend on a sibling.

The substrate is a projection, not a replacement:

- terrain supplies ocean/seabed classification, elevation, water kind,
  drainage, features, and any existing volcanic/tectonic context;
- climate supplies marine `BiomeExpr`, strata/column accessors, temperature,
  insolation, and the deterministic ocean-current field;
- worldgen derives Waterworld-specific ambient fields, vent sources, stocks,
  corridors, migration samples, and observation lenses.

The first implementation must inspect the exact live accessors before coding.
If a requested quantity has no existing source, the spec permits a compact
worldgen-derived proxy only when its provenance is named and its independence
probe mutates the actual source used by that proxy.

## 4. Waterworld model

### 4.1 Substrate

Each sampled water location carries a compact substrate readout containing:

- seabed versus water-column position;
- existing marine `BiomeExpr`/stratum information;
- depth and a bounded depth band;
- the underlying terrain water/seafloor context;
- a stable projection vertex for rendering and adjacency.

No new `Biome` variant is introduced. A vent is a localized source over the
marine substrate, not a new universal biome category.

### 4.2 Ambient fields

`WaterFields` is a derived diagnostic bundle with independently named values:

- `light`: sunlight after depth attenuation;
- `pressure`: a monotone depth proxy;
- `temperature`: the climate temperature at the sampled time/location plus a
  localized vent-temperature contribution where applicable;
- `salinity`: a water-column condition whose source and proxy are documented
  from live terrain/climate inputs;
- `chemistry`: localized hydrothermal potential, distinct from temperature;
- `current`: the existing deterministic ocean-current vector;
- `depth_m` and a bounded depth band.

The bundle is diagnostic state and an input to derived habitat consequences;
it is not a new save-format fact.

The design requires the implementation to distinguish absence from zero. A
location with no vent has no vent source, while its chemistry field may be a
zero baseline. Tests must exercise both cases explicitly.

### 4.3 Vent sources

Vent candidates are selected from derived seabed/volcanic context and an
isolated, explicitly named Waterworld stream. The selection is sparse and
bounded by configuration. Each admitted source has a stable identity, source
strength, thermal contribution, and chemical contribution. Location is not a
fixed authored list.

The stream is consumed in a deterministic order independent of query and
render order. Existing Skyworld and historical worldgen streams are not
reordered. The stream manifest and any generated audit output are updated only
if the implementation adds a live label.

### 4.4 Aggregate stocks

The overlay stores only bounded aggregate values:

```text
sunlight + nutrients -> plankton
vent chemistry       -> chemosynthetic bloom
plankton/nutrients   -> kelp or reef biomass
current transport    -> sparse downstream stock influence
```

These are derived readouts, not individual agents. The formula must keep light,
chemistry, substrate, pressure/temperature suitability, and current transport
as separately testable contributors. A single scalar productivity shortcut is
not acceptable because it cannot answer the campaign question.

### 4.5 Movement and refresh

Current corridors are sparse ordered paths over existing ocean vertices. They
carry stock influence and movement/adjacency affordances; they do not integrate
all ocean cells at every tick.

A vertical migration/refresh cycle is a finite configured sequence of depth
bands or sampled depths. It is a read-only trajectory-like observation: it does
not mutate stocks, vent state, climate, or terrain. Querying a sample is an
exact lookup and consumes no randomness.

## 5. Observation

Ordinary observation shows consequences:

- seabed/water-column character;
- bloom, kelp/reef, and nutrient conditions;
- sparse current corridors and migration bands;
- localized vent presence only at a legible consequence level.

Diagnostic observation shows causes and uncertainty:

- depth, pressure, light, temperature, salinity, chemistry, and current;
- vent source strength and provenance;
- which stock contributions are present;
- bounded-work counters where the diagnostic contract already permits them.

Neither lens may claim that a derived proxy is directly observed physical
truth. Rendering and readout are pure consumers of generated overlay state;
they do not rebuild terrain/climate, draw new vents, or alter stream state.

## 6. Measurement matrix

Every independence probe follows the same protocol: snapshot the intended
source fields, apply one perturbation at the actual consumption site, assert
that the source changed and unrelated hooked sources did not, then assert only
the downstream outputs that the design says depend on it.

The minimum matrix is:

| Source | Required downstream consequence | Required preservation checks |
|---|---|---|
| seabed substrate | substrate/stock suitability or vent admission | water-column identity, unrelated ambient fields |
| water-column/depth band | depth, pressure, light, temperature bands | seabed identity, vent identity |
| vent source | chemistry and chemosynthetic bloom | no change to non-vent locations |
| current | corridor direction and transported stock/migration readout | vent placement and substrate |
| light/insolation | light field and photic plankton contribution | chemistry-driven contribution |
| pressure/depth | pressure field and pressure suitability | light-source identity |
| temperature | temperature field and thermal suitability | chemistry contribution |
| salinity | salinity field and salinity suitability | pressure/light fields |
| chemistry | chemical field and chemosynthetic contribution | light-driven contribution |

Each test must fail if the perturbation is a no-op. Tests that only compare a
final aggregate after changing several coupled inputs are integration tests,
not independence evidence.

## 7. Determinism, streams, and compatibility

- Same seed, same pins, and same configuration produce byte-identical overlay
  state and observations.
- Repeated queries and rendering in different orders produce equal results and
  consume no additional randomness.
- New Waterworld randomness uses separate labels and a fixed consumption order;
  no existing stream is inserted into conditionally executed loops.
- Existing terrain, climate, marine biome/column, Skyworld, and absent-overlay
  behavior remain stable. The compatibility test constructs the pre-overlay
  path and compares its output to the path with Waterworld disabled.
- No save fact or build rung is introduced. Any public API carries the
  repository's type-audit annotations.

## 8. Bounded work and memory

Counters are incremented at actual loops, not inferred from returned vector
lengths. The minimum counters cover:

- substrate scans and vent-candidate evaluation;
- admitted vent sources;
- stock derivation sites;
- corridor candidates and accepted corridor edges;
- migration/refresh samples;
- ordinary and diagnostic observation loops and rendered pixels.

Tests compare inactive versus active overlay, one versus multiple admitted
vents, low versus high configured sample counts, and ordinary versus
diagnostic observation. Claims are limited to measured fixture/configuration
sizes; the campaign does not claim a universal asymptotic or allocation bound.

## 9. Stages and deliverables

1. **Seam inventory and probes:** document live accessors and add non-vacuous
   source-isolation tests before production behavior.
2. **Ambient propagation:** implement independent fields and derived vent
   sources with stream/order tests.
3. **Stocks and movement:** add bounded aggregate stocks, current corridors,
   vertical refresh, adjacency, and loop counters.
4. **Observation and close:** add ordinary/diagnostic outputs, performance
   evidence, documentation, registry follow-ups, local commit gate, and
   queued stage/merge submissions.

## 10. Follow-ups and explicit captures

The campaign will add or update idea-registry entries for deferred sea peoples,
marine creatures, species/metabolism, vent succession, reef fragmentation,
full current-network dynamics, and signal distortion. They are not implied by
the presence of aggregate stocks or a read-only migration cycle.

## 11. G3 review questions

1. Is a compact composition-root overlay the right scale for this Waterworld
   pressure test?
2. Does the five-group model preserve the worldly nouns while making the
   requested independence matrix testable?
3. Are the proposed non-goals and bounded observation/movement seams sufficient
   to avoid premature species, metabolism, fluid, or universal-wrapper work?

