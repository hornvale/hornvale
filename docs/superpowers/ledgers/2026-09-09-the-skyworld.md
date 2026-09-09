# The Skyworld — decision ledger

Campaign: **The Skyworld** — a generated and rendered mobile habitat overlay.
Branch: `campaign/skyworld`. Autopilot is engaged; G3 spec review and G6
close remain hard stops.

## #1 [G1] — What is the Skyworld's foundational architecture?

**Decision:** use a sparse, seeded Skyworld habitat overlay over the existing
land and ocean surface, with an augmented temporal adjacency graph. Keep local
biome description separate from mobile habitat identity, movement, lineage,
and influence.

**Why:** the existing faceted biome model (`realm : formation : stratum`) is a
good local description but cannot alone represent moving territories,
historical scars, or changing adjacency. The overlay preserves the existing
world model while giving Skyworld and later Waterworld a common habitat
grammar.

**Alternatives discarded:** adding a large family of sky-specific `Biome`
variants would encode combinations as taxonomy; replacing the biome system
would be premature before Skyworld and Waterworld pressure-test the seams; a
planet-wide atmospheric simulation would exceed the campaign's scope and
performance budget.

**Ideonomy passes / overturns:** five successful passes across synchronization,
mutable identity, phenotype, footprint, and propagation. No recommendation
overturned. The passes added physical/exchange/influence/rendered footprints,
lineage memory, corridor propagation, and the distinction between stable
fields and sparse mobile habitats.

**Capture:** architecture is in §§3–5 of the design spec. Reusable
cross-realm habitat ideas are captured in the registry rows added with this
campaign.

## #2 [G2] — What is the first Skyworld specimen and scope?

**Decision:** generate and render one mature, free-drifting orchard descended
from a biological sky reef. Stop before individual organisms, populations,
culture, GOAP, tether engineering, mutable atmosphere, or co-evolutionary
simulation.

**Why:** the orchard exercises world-specific altitude ecology, aether and
radiation fields, plankton productivity, resource stocks, movement,
projection, influence, and rendering without requiring a full society or
agent simulation. Free drift makes tethering a later intervention rather than
a hidden prerequisite.

**Alternatives discarded:** beginning with a tethered orchard would hide the
natural movement and itinerary problem; beginning with an archipelago would
add split/merge/recombination before the single-habitat overlay is understood;
implementing organisms or cultures now would turn a generation/rendering
campaign into a general simulation campaign.

**Ideonomy passes / overturns:** five design passes; no overturn. The passes
added the sky-reef lifecycle, upper-atmospheric dual limitation, independent
stocks, and the stable-baseline/mutable-future seam.

**Capture:** scope and specimen are in §§2, 3.4, 4.4–4.5, and §10 of the
design spec.

## #3 [G2] — How should performance and determinism constrain propagation?

**Decision:** use coarse deterministic fields for continuous forcing, sparse
corridors for structured transport, and discrete events for rare changes.
Keep rendering and caching derived from the same deterministic inputs.

**Why:** this preserves meaningful wind, current, tidal, and migration
patterns without requiring all-cells/all-ticks fluid or particle simulation.
It also keeps stream consumption and output independent of render order or
cache state.

**Alternatives discarded:** full atmospheric integration is out of scope;
pure distance falloff cannot express wind/current corridors; a global dense
influence repaint would make the overlay's cost proportional to the whole
planet at every sample.

**Ideonomy passes / overturns:** one dedicated propagation pass plus the five
earlier passes; no overturn. The pass added the distinction between local
kernels, corridors, itineraries, events, and broadcasts.

**Capture:** performance, determinism, and testing requirements are in §§5,
7, and 8 of the design spec.

## #4 [Q] — What does Stage 1 store for trajectory and propagation?

**Ruling:** keep the brief's `trajectory`, `exchange`, and `influence` fields
in the public overlay model, but generate no trajectory samples, expanded
exchange envelope, corridors, or events in Stage 1. `trajectory` and
`SkyPropagation` therefore begin empty, while `exchange` begins as the
physical footprint; the later movement/propagation stage owns every derived
addition.

**Why:** the task brief makes those fields part of the intended interface,
while the implementer dispatch explicitly forbids implementing orchard
trajectories or propagation in this stage. Storage without derived behavior
preserves the interface and the stage boundary. If this ruling is wrong, the
cost is a small additive initialization change in the later stage, not a
changed draw or surface-biome epoch.

**Alternatives discarded:** omitting the fields would contradict the brief's
public type shape and force a later structural API change; deriving even a
one-sample route or influence kernel now would cross the explicit Stage 1
scope; optional renderer-owned state would violate the overlay's ownership
rule.

**Ideonomy passes / overturns:** one abstraction-lift matrix pass over
informational versus active state and externally driven versus autonomous
behavior; no overturn. The pass exposed the useful boundary as
"storage present / autonomous derivation absent," rather than treating type
existence and simulation behavior as one decision.

**Capture:** this ruling is local to Stage 1 and recorded here; no speculative
follow-up was created because the approved plan already assigns the absent
behavior to later tasks.

## Follow-ups

- Run the first Skyworld implementation as a pressure test of the existing
  `BiomeExpr`/environment-axis seams before considering a sweeping biome
  rewrite.
- Decide at G3 whether sky is represented only by the overlay or also by a
  formal realm/formation projection.
- Coordinate the astronomy-to-climate forcing seam with The Wanderers after
  its merge; consume compact time-slice forcing rather than raw ephemeris
  internals.
- Preserve mutable atmospheric profiles as a later modifier layer over the
  stable generated baseline.
- Add a follow-up campaign for the multi-fragment sky archipelago: splitting,
  merging, inherited traits, recombination, and hybrid habitats.
- Add a follow-up campaign for tethering: natural anchors first, then
  permanent, semi-permanent, seasonal, engineered, and magical tethering.
- Add a co-evolution campaign for free-floating sky plankton, flora,
  pollinators, fauna, disease, and habitat structure.
- Use Waterworld as the second pressure test: turtle islands, submerging
  islands, reef lineages, currents, and living habitats.
- Generalize the overlay for Overworld and Underworld mobile/eventful
  habitats only after a second realm demonstrates the shared shape.
- Keep the ability-reservoir, actor/environment exchange, affordance
  discovery, GOAP experimentation, deception, and channel-specific detection
  ideas in their own future work; existing affordance precedents 0347–0349
  remain authoritative.
- Explore local calendars, spirits, animism, religion, and cultural memory
  only after the world can generate persistent, consequential phenomena.

## Rejected for this campaign

- A universal `sugar` renaming of world concepts.
- Individual plankton or full atmospheric particles.
- A single scalar habitat health value.
- A universal truth-revealing perception or `Detect Magic` mechanism.
- Authored mythology or authored superstition.
- Magic or technology as a current requirement for sky-island formation.
