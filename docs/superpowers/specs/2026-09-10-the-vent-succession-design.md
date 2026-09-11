# The Vent: Succession — temporal Waterworld overlay

**Campaign:** The Vent: Succession (provisional title; alternate: The Living
Vent)

**Classification:** Architectural continuation campaign.

**Base:** Current `origin/main` at campaign start. The Vent's close artifacts
and merged `windows/worldgen/src/waterworld.rs` seam are the governing
predecessor precedent. This campaign extends that static overlay with a pure
temporal snapshot; it does not recreate or fork the predecessor seam.

## 1. Question

The Vent asked whether explicit marine substrate, ambient fields, aggregate
stocks, sparse currents, and honest observation can exist underwater. This
campaign asks whether that Waterworld overlay can evolve over world time while
remaining deterministic, bounded, query-pure, and explainable.

The experiment is deliberately small. It models vent succession as a derived
temporal overlay over existing terrain and climate. It does not attempt to
simulate a fluid volume, an ecosystem, or individual organisms.

## 2. Scope and non-goals

### In scope

- a stable, seeded vent identity derived from existing seabed/volcanic context;
- a pure succession cycle selected by exact `WorldTime` ticks;
- bounded vent states: absent, nascent, active, weakening, and failed;
- local changes to hydrothermal chemistry and temperature;
- aggregate plankton, chemosynthetic bloom, nutrient, and reef/kelp
  suitability responses;
- bounded current-mediated transport of aggregate influence;
- bounded migration over an ordered candidate ring without moving substrate;
- analytical, bounded stock residue where measurement shows it is necessary;
- ordinary consequence observations and diagnostic cause/provenance views;
- deterministic stream allocation, ordering, purity, and workload evidence.

### Explicitly out of scope

- sea elves, mermaids, tritons, sea dragons, kraken, turtle-islands, or
  submerging islands;
- species, metabolism, reproduction, population, culture, or per-organism
  ecology;
- full 3D fluid or ecology simulation;
- dense planet × time caching or an unbounded event queue;
- authored fixed vent histories or named event lists when derivation suffices;
- a new biome taxonomy or universal habitat wrapper;
- sweeping terrain, climate, biome, current, or rendering refactors without a
  demonstrated seam mismatch;
- new save facts, build-depth rungs, or census refreshes.

Deferred ideas are captured in the campaign ledger and idea registry rather
than silently dropped.

## 3. Governing design

### 3.1 Stable world, temporal snapshot

The composition-root overlay has two conceptual layers:

```text
World + GeneratedTerrain + GeneratedClimate
        |
        v
WaterWorld
  stable substrate projection
  stable admitted vent identities
  stable seeded succession parameters
        |
        +-- at(WorldTime) --> WaterWorldSnapshot
                              vent phase and position
                              ambient fields
                              aggregate stocks
                              current transport
                              observations
```

`WaterWorld` contains no mutable ecological history. `at(WorldTime)` is pure,
does not draw, and does not populate a cache. Repeated and reordered calls
must return byte-identical snapshots and leave the stable overlay unchanged.

The exact public names may follow the live predecessor seam after Stage 1;
the boundary is load-bearing even if names differ.

### 3.2 Vent state and succession

Each admitted vent has a stable source identity, stable substrate anchor,
seeded phase offset, and bounded duration parameters. The phase is derived by
integer operations on `WorldTime` ticks and a fixed succession period. The
state sequence is:

```text
absent -> nascent -> active -> weakening -> failed -> absent
```

The cycle is a bounded renewal model, not a historical event log. The state is
categorical, while continuous phase position within the state remains
available for smooth attenuation and diagnostic explanation.

- **Absent:** no local vent contribution at that source position.
- **Nascent:** chemistry and temperature begin below active strength; a
  candidate-ring position may differ from the stable anchor.
- **Active:** local chemistry and thermal contribution are strongest.
- **Weakening:** chemistry/temperature attenuate and migration may select a
  downstream candidate.
- **Failed:** the source identity and seabed remain present, but the local
  hydrothermal contribution is zero or below the configured detection band.

The exact thresholds and bounds are authored in the spec/plan only after Stage
1 confirms the available time/configuration seam. They must be finite, named,
and tested at exact tick boundaries.

### 3.3 Migration without substrate movement

Each stable vent anchor derives a small, ordered candidate ring from existing
nearby marine vertices. The ring has a fixed maximum size and deterministic
tie-breaking. Current phase selects at most one active influence position;
substrate, water-column identity, and vent identity do not move.

This is a bounded source relocation readout, not particle advection. Current
transport remains a separate channel and cannot rewrite vent admission or
terrain.

### 3.4 Fields and stocks

The snapshot preserves The Vent's explicit marine nouns:

- substrate: seabed/water-column position, marine biome/stratum, depth band,
  and terrain context;
- fields: light, pressure, temperature, salinity, chemistry, and current;
- stocks: plankton, chemosynthetic bloom, nutrients, and reef/kelp
  suitability;
- movement: local influence, bounded current transport, and candidate-ring
  migration;
- observability: ordinary consequences and diagnostic causes.

Local chemistry and temperature are functions of vent state and distance from
the selected position. Light, pressure, salinity, and baseline temperature
retain their independent existing source paths. Stock formulas keep their
contributors separate so tests can perturb one source at a time.

Plankton and chemosynthetic bloom respond to present fields. Nutrient reserve
may include a bounded analytical residue derived from the time since the most
recent active interval. Reef/kelp suitability may consume that residue along
with present substrate, temperature, chemistry, and marine suitability. No
stock is stored per organism or per planet-time cell.

Current transport follows only a configured finite number of ordered marine
vertices and applies bounded attenuation. It changes transported aggregate
influence, not substrate, vent identity, or current direction.

### 3.5 Observation

Ordinary observation reports what is presently consequential: marine
substrate, vent consequence level, bloom and nutrient conditions, reef/kelp
suitability, and current influence. It does not claim to know the source
phase unless the consequence is legible.

Diagnostic observation may report derived phase, phase position, source
provenance, local versus transported contribution, and bounded-work counters.
It must label inferred causes as derived or uncertain. A failed source is not
rendered as erased seabed; an absent or undetected contribution is distinct
from a zero-valued ambient baseline.

Both views are pure projections over a snapshot. Rendering never derives new
vent state, consumes randomness, mutates stocks, or rebuilds terrain/climate.

## 4. Existing seam and ownership

The overlay remains in `windows/worldgen`, the composition root. Domains do not
depend on sibling domains. Stage 1 must verify the exact current signatures
before implementation. The predecessor inventory identifies these relevant
inputs:

- terrain: geosphere, water kind, sea level, elevation, boundaries, edifices,
  and feature context;
- climate: marine biome expression, strata/column, temperature at `WorldTime`,
  insolation, and deterministic ocean current;
- kernel: exact `WorldTime` ticks, `Seed`, ordered vertices, and existing
  deterministic stream primitives.

If direct ocean depth, inverse stratum depth, or retained seafloor context is
absent, the composition root may use a compact named proxy, as The Vent did.
It must not add symmetry-only domain accessors. A source perturbation test must
mutate the actual value consumed by the proxy and prove the source changed
before comparing derived output.

## 5. Determinism and stream contracts

- same seed, pins, configuration, and `WorldTime` produce byte-identical
  snapshots and observations;
- new seeded parameters use isolated, versioned Waterworld labels;
- labels are keyed by stable vertex/source identity, never generation ordinal;
- admission and parameter draws consume in stable vertex order;
- query, migration selection, propagation, and render calls consume no draws;
- no existing stream is reordered or conditionally shortened;
- stream manifest/report changes occur only when the actual command shows drift.

The implementation must record the exact label, key, draw order, and stream
compatibility evidence in the campaign ledger.

## 6. Measurement matrix

Every independence test follows this protocol:

1. identify the exact source value at the consumption seam;
2. assert the perturbation changes that source;
3. assert unrelated source values remain unchanged;
4. assert the documented downstream field or stock changes;
5. assert outputs outside that dependency remain stable.

Required evidence includes:

| Perturbation | Expected independent consequence |
|---|---|
| world time | vent state and downstream local fields change across witnesses |
| vent source strength/state | chemistry and chemosynthetic bloom change |
| vent thermal contribution | local temperature and thermal suitability change |
| depth/water column | pressure and light change while seabed identity remains |
| insolation | light and photic plankton contribution change |
| salinity | salinity field/suitability changes independently |
| nutrient input/residue | nutrient reserve and reef/kelp suitability change |
| current vector | transport/corridor output changes while substrate is stable |
| substrate | reef/kelp suitability or vent admission changes |

The suite must contain nonempty witnesses for inactive, nascent, active,
weakening, and failed states. A test that never changes its source or only
changes a final coupled scalar is not accepted as independence evidence.

## 7. Bounds and performance

Counters increment at the actual loops for substrate inspection, vent
candidate evaluation, phase/position evaluation, stock derivation, candidate
ring selection, current propagation, refresh sampling, ordinary observation,
diagnostic observation, and rendering.

Tests compare disabled/active overlays, low/high ring limits, low/high
propagation limits, and repeated/reordered queries. Claims are limited to the
measured fixture and configuration sizes. The implementation must not allocate
a dense planet × time product; any temporal work is recomputed for the
requested snapshot with finite configured loops.

## 8. Stages

1. **Temporal seam inventory and non-vacuous red probes** — verify current
   accessors, establish witness populations, preserve absent-overlay behavior,
   and add behavioral-red time/source tests.
2. **Deterministic succession and field propagation** — add stable vent
   parameters, exact phase states, candidate-ring migration, and independent
   local chemistry/temperature changes.
3. **Bounded stocks and current-mediated redistribution** — add analytical
   residue only if probes demonstrate its need; add bounded transport and
   actual-loop counters.
4. **Observation, performance evidence, documentation, and close** — complete
   ordinary/diagnostic views, docs and registry captures, local commit gate,
   then request stage/merge through the Sluice.

`IMPLEMENTATION_PLAN.md` is created only after this spec passes G3 review.

## 9. Deferred follow-ups

- persistent multi-cycle vent histories and coupled vent networks;
- reef/kelp fragmentation, merger, and current-network feedback;
- signal/observation distortion caused by water conditions;
- species, metabolism, reproduction, and marine peoples/creatures;
- any universal habitat abstraction, only if measured duplicated rules later
  justify it.

## 10. G3 review package

The approval questions are:

1. Does the stable `WaterWorld` plus pure `WaterWorldSnapshot` boundary keep
   the temporal experiment bounded and query-pure?
2. Are the five states, bounded candidate-ring migration, and analytical stock
   residue sufficient to test succession without smuggling in ecology?
3. Does the source-first measurement matrix provide credible evidence that
   time, vent state, fields, stocks, movement, and observation are independent
   where claimed?
