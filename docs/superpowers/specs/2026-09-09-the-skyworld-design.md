# The Skyworld — design

**Campaign:** The Skyworld · **Branch:** `campaign/skyworld`  
**Status:** G3 approved; Tasks 1–3 and Task 4 local integration complete; seam-audit broad review approved through `c1a20ce71`. Close artifacts prepared under `skyworld-seams`; Task 4 canonical stage submission and merge remain pending.
**Predecessors:** The Stratum, The Wanderers, The Seedbed, and the existing
climate/resource basis.

## 1. Purpose

The Skyworld is the first campaign to test a mobile, eventful habitat layer
over the existing planet. It must add substantial aerial habitat without
turning the surface biome model into a catalogue of sky-specific enum values.

The first implementation stops at deterministic generation and rendering. It
does not simulate individuals, populations, culture, research, or GOAP.

The design uses the world's existing worldly nouns—biome, habitat, resource,
wind, current, island, orchard—and treats SugarScape as the shared reasoning
pattern underneath them. Nothing is renamed to `sugar` merely to make the
analogy visible.

## 2. Scope

### 2.1 In scope

- A sparse Skyworld overlay projected over land and ocean surface cells.
- A seeded, world-specific atmospheric profile.
- Luminiferous-aether and high-sky-radiation fields.
- Free-floating sky habitats and their derived temporal adjacency.
- A first mature habitat: a migratory orchard descended from a biological
  sky reef.
- Habitat phenotype, lineage metadata, movement regime, lifecycle stage, and
  initial ecological stocks.
- Wind-driven movement with lunar modulation.
- Local, corridor, and event-shaped influence summaries.
- Rendering of sky bodies, surface projections, routes, exchange envelopes,
  and visible ecological consequences.
- Determinism, bounded work, and byte-stable derived readouts.

### 2.2 Out of scope

- Individual sky plankton, plants, animals, or people.
- Population dynamics, harvesters, settlements, GOAP, or deliberate
  experimentation.
- Cultural knowledge, calendars, religions, spirits, or myths as simulation
  systems.
- Mutable planetary atmospheric profiles.
- Engineered or magical tethering.
- Full co-evolution, mutation, splitting, merging, or collision simulation.
- Waterworld mobile habitats, Overworld mobile phenomena, and Underworld
  ecology as implementations. They remain follow-up pressure tests.
- A general affordance or ability-reservoir rewrite.

Those deferred ideas are recorded in the campaign ledger and frontier registry
so the scope cut does not discard them.

## 3. Organising decisions

### 3.1 The Skyworld is an overlay, not a renamed biome taxonomy

The existing `BiomeExpr` remains the local ecological description of the
surface and column. Skyworld adds a separate habitat layer with its own
position, footprint, trajectory, lineage, and stocks. The renderer may show a
sky habitat alongside the projected surface biome without replacing it.

The first slice must not add a large set of `Biome` variants such as
`SkyForest`, `StormBelt`, or `CloudReef`. Those are derived or cultural names
for combinations of independent properties. A future campaign may add a
formal sky realm or formation only if the overlay pressure test demonstrates
that the existing faceted projection cannot carry the needed readout.

### 3.2 Coverage is drawn, bounded, and world-specific

Sky coverage is a seeded draw over the planet's surface-cell population,
including ocean cells. The total projected coverage has a hard global ceiling
of 10%; the realized fraction is drawn from world conditions and may be much
smaller. The distribution is not uniform:

- mountain, coastal, volcanic, oceanic, and atmospheric conditions bias the
  draw;
- connected archipelagos and isolated territories both occur;
- each projected cell may host no sky territory, one territory, or more than
  one vertical or overlapping influence;
- the surface cell remains present and habitable beneath the overlay.

The percentage measures projected sky-habitat coverage, not atmospheric
volume. Sky habitat therefore adds carrying capacity rather than relabelling
existing surface capacity.

### 3.3 Stable baseline now; mutable atmosphere later

The first slice draws a stable atmospheric profile per world. It may vary by
latitude, season, terrain, circulation, and local weather, but the profile
itself does not change during the campaign.

The conceptual seam is:

```text
stable world baseline + future mutable modifiers
```

Later magic, technology, volcanism, or ecosystem-scale feedback may supply
those modifiers. They are not implemented here.

### 3.4 The first specimen is a free-drifting orchard

The first canonical habitat is one coherent, mature floating island descended
from a wind-fed biological sky reef. It is not initially tethered and has no
permanent settlement.

Its dominant movement driver is wind. Lunar and stellar forcing modulate drift,
stress, and exposure rather than directly dictating every movement. The
orchard's signature output is fruit, but fruit is only one stock in a chain
whose prerequisites include aether, radiation, plankton, moisture, frost
history, and pollination.

### 3.5 Local description and mobile identity stay separate

The overlay carries what a fixed-cell biome cannot:

- identity and lineage
- current position and altitude
- movement law
- lifecycle stage
- stability and stress profile
- exchange and influence footprints
- temporal adjacency

The existing environmental axes remain useful for the habitat phenotype:
physiognomy, energy, water, substrate, light, and—where appropriate—
disturbance. Mobility, stability, tidal susceptibility, and exchange are
overlay properties because they are relational, historical, or time-varying.

## 4. World and habitat model

### 4.1 World-level fields

Each world supplies deterministic fields and profiles for:

- atmospheric pressure and density
- temperature and lapse rate
- moisture and cloud layers
- wind and wind shear
- high-sky radiation
- luminiferous-aether concentration
- lunar and stellar forcing
- optional world-specific thaumic gradients

Luminiferous aether is an upper-atmospheric resource. It is physically light
and buoyant, and the high-sky radiation needed by sky plankton is filtered out
at lower altitudes. The first slice does not decide whether the aether is
magical, physical, or both; it only exposes the fields and their consequences.

### 4.2 Habitat state

A sky territory has compact state for:

- stable identity and lineage
- current position, altitude, and projected footprint
- phenotype and lifecycle stage
- movement regime and forcing sensitivities
- buoyancy, cohesion, flexibility, and recovery characteristics
- exchange envelope and influence channels
- initial ecological stocks

The stability profile is multidimensional. Structural strength and flexibility
are separate: a large island may resist ordinary stress yet fail
catastrophically, while a small raft may bend, shed mass, and recover.

### 4.3 Phenotype dimensions

Sky habitats are described by independent properties rather than one exclusive
biome label:

- substrate: cloud, organic mat, soil, ice, ash, mineral, or mixed
- energy: sunlight, aether, high-sky radiation, storm charge, or thaumic flow
- water: dry, humid, cloud-fed, rain-fed, or saturated
- mobility: drifting, cyclic, current-following, field-following, or mixed
- stability: rigid, flexible, regenerative, or brittle
- exchange: isolated, surface-linked, corridor-linked, or archipelagic
- ecological structure: bare, microbial, rooted, wooded, or orchard-bearing

The visible name of a habitat is a projection of these properties, not their
canonical storage form.

### 4.4 The orchard resource chain

The first specimen uses a small set of fields and stocks:

```text
ambient aether + high-sky radiation + moisture
    -> sky-plankton productivity
    -> fungal and root support
    -> soil fertility and canopy biomass
    -> flowers, pollination, and fruit
```

The orchard may also carry cloud water, detritus, seeds, spores, and animal
forage. Aether and radiation are ambient fields; plankton, soil, water,
biomass, and fruit are renewable or depletable stocks. This follows the
existing Field/Stock distinction rather than creating a second resource
ontology.

### 4.5 Sky-reef origin and lifecycle

The orchard's lineage is represented by a lifecycle that can be rendered and
read, without simulating every transition in the first campaign:

```text
wind bloom
    -> adhesive microbial mat
    -> buoyant raft
    -> pioneer islet
    -> rooted island
    -> mature orchard
    -> senescence, descent, fragmentation, or collapse
```

Sky plankton is free-floating. Wind convergence zones act like aerial tidal
pools: plankton blooms, microbial mats, and flower-like structures form where
the fields overlap. A successful mat captures dust, ash, moisture, and
aether-rich material; roots and fungi later bind the growing substrate.

The first generated orchard is mature, but its lineage retains the origin and
possible successor outcomes for later campaigns.

## 5. Movement, exchange, and adjacency

### 5.1 Deterministic forcing, not fluid simulation

Wind and other flows are represented as coarse deterministic fields or
world-specific functions. A habitat samples the field along its trajectory;
the simulation does not integrate every atmospheric cell at every tick.

The orchard's movement is primarily wind-driven, with lunar modulation and
world-specific local modifiers. A trajectory is a pure function of world
identity, habitat identity, forcing profile, and time slice.

### 5.2 Four footprints

The system keeps these distinct:

1. **Physical footprint:** where the body occupies space.
2. **Exchange footprint:** where rain, spores, organisms, fruit, heat, or
   minerals can cross.
3. **Ecological influence footprint:** where the habitat changes conditions
   without direct contact.
4. **Rendered footprint:** how much of the body or influence the observer sees.

The physical footprint is local. Seeds, spores, rainfall, and wind effects may
travel farther through corridors. The rendered footprint is a view and must not
be treated as world state.

### 5.3 Temporal adjacency

Sky adjacency has two forms:

- vertical adjacency to the current land or sea projection;
- lateral adjacency to reachable sky habitats, wind corridors, pollinator
  routes, or migration paths.

The orchard's projected surface cells change as it moves. The surface cells do
not move and are not replaced. An influence may reach several cells without
turning all of them into sky habitat.

The first slice uses three propagation shapes:

- bounded local kernels for immediate effects;
- structured corridors for windborne seeds, plankton, and routes;
- sparse events for blooms, storms, and collapses.

Long-range broadcast effects remain represented as rare world-level forcing or
readout, not dense per-cell propagation.

## 6. Generation and rendering

### 6.1 Generation sequence

The generator should:

1. derive the world-specific atmospheric and aether/radiation profiles;
2. draw sky coverage and its mixed spatial distribution;
3. draw territories and their phenotype, lineage, movement, and stability
   attributes;
4. construct the orchard specimen and its initial stocks;
5. derive trajectory samples, projected surface cells, exchange envelopes, and
   influence corridors;
6. emit deterministic readout data for the clients.

The implementation should reuse existing climate, astronomy, geosphere, and
resource primitives where their semantics fit. It should not make the climate
provider import a worldgen or astronomy domain merely to expose sky state.

### 6.2 Ordinary rendering

The renderer should show:

- the planet's ordinary land and sea surface;
- sky bodies as an additional layer;
- the orchard's current physical footprint and altitude;
- its surface projection;
- an optional trajectory or historical route;
- exchange and influence envelopes at an appropriate level of detail;
- visible consequences such as shadow, spores, rain, falling seeds, or cloud
  contact when those are part of the selected view.

The ordinary view presents phenomena and consequences, not an omniscient
causal explanation. A diagnostic view may separately show aether, radiation,
wind, and resource fields for calibration and development.

### 6.3 Level of detail

- Planet view: coverage mask, territory centroids, broad corridors, and rare
  events.
- Regional view: body footprint, projected cells, local influence, and route.
- Habitat view: orchard structure, stocks, phenotype, and lifecycle state.

The renderer must not require a full-resolution repaint of the planet whenever
one territory moves. Detailed geometry and influence are materialized at the
view's requested resolution.

## 7. Determinism and performance

- World seed is identity and controls every draw.
- Draw streams are labeled by independent concern: coverage, distribution,
  atmosphere, territory phenotype, lineage, and movement variation.
- Randomness is consumed during generation or explicit time-slice derivation,
  never opportunistically during rendering or cache lookup.
- Stable ordering and explicit tie-breaking are required wherever multiple
  territories, cells, or corridors compete.
- Caches may reduce work but must not alter output, stream consumption, or
  observable ordering.
- Coarse fields, sparse territories, bounded influence queries, and event
  records are preferred to all-cells/all-ticks integration.
- The first slice should carry only a small fixed vector of habitat stocks and
  traits; it should not materialize sky plankton or every plant as an entity.

The performance target is not a particular elapsed time. The target is that
work scales with active territories, sampled trajectory segments, and requested
detail rather than with the full planet multiplied by every temporal sample.

## 8. Testing and probes

The implementation plan must include tests for:

- same seed and inputs produce byte-identical generated sky data and renders;
- different seeds can produce different coverage, topology, profiles, and
  orchard trajectories;
- coverage never exceeds the global 10% projected-cell ceiling;
- land and ocean projections both occur on a valid activation surface;
- clustered and isolated territories are both representable;
- the surface layer remains intact beneath the sky overlay;
- altitude profiles are world-specific and stable within a generated world;
- aether and high-sky radiation are independently available fields;
- sky-plankton productivity requires both fields and moisture;
- orchard resources expose prerequisites rather than an unexplained fruit
  number;
- trajectory queries are deterministic and cache-independent;
- local, corridor, and event propagation remain distinct in readouts;
- rendering can show physical footprint separately from exchange and influence;
- no ordinary renderer path exposes hidden causal state as if it were an
  inhabitant's observation.

Tests must measure nonempty denominators for coverage and activation probes;
zero territories or zero affected cells are probe failures, not successful
absence.

## 9. Flagged for G3 review

- Whether the first wire needs an explicit sky `Realm`/`Formation`, or whether
  the overlay projection is sufficient for the generation/rendering slice.
- The exact seam by which Wanderers' stellar and lunar forcing reaches the
  sky profile without making climate depend on raw astronomy internals.
- The additive scene/readout shape for sky bodies, projections, and corridors.
- The distribution draw's exact coverage prior and how strongly terrain,
  ocean, and atmospheric conditions bias it.
- Whether generated lifecycle state is purely descriptive in this campaign or
  includes a minimal time-slice transition readout.

These are schema-adjacent or fidelity decisions and should be explicit in the
G3 package before planning.

## 10. Deferred follow-ups

The following are deliberately not implementation requirements for this
campaign:

- mutable atmospheric profiles and ecosystem-scale climate feedback;
- splitting, merging, hybridization, and descendant-island inheritance;
- engineered, magical, permanent, semi-permanent, and seasonal tethering;
- full co-evolution among plankton, flora, fauna, and habitats;
- mobile Waterworld habitats, including turtle islands and submerging islands;
- mobile or eventful Overworld and Underworld habitats;
- individual organisms, ability reservoirs, GOAP experimentation, and
  actor-specific affordance discovery;
- deceptive affordances, magical detection, and channel-specific perception;
- emergent calendars, local spirits, animism, religion, and cultural memory;
- surface, marine, and subterranean species that consume aether-derived
  products without accessing the aether field;
- the multi-fragment archipelago successor case.
