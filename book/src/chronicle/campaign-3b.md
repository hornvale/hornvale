# Campaign 3b: The Tectonic Globe

**July 2026 · 5 commits (3a, the Geosphere) + 11 commits (3b, the tectonic
globe) · outcome: complete, merged — the world gets a shape**

## What was attempted

Campaign 3 gave the world a shape, in two movements merged under one
chronicle. The first, 3a, built the substrate: a kernel **Geosphere** — an
icosphere region graph, subdivided to level 5 for 10,242 cells — with
stable adjacency, geographic coordinates, and a `CellMap<T>` for typed
per-cell fields. It shipped with no observable output of its own, so its
chronicle was deliberately deferred to here, where its purpose becomes
visible. The second, 3b, is that purpose: plates drawn on the Geosphere,
motion, boundaries classified by relative velocity, elevation from
geometry plus a rule table, sea level from a target ocean fraction, an
unrest field, three ledger-backed pins, the N-seed property battery, and a
new gallery artifact.

## What landed

**The Geosphere.** Subdivision is pure geometry — a base icosahedron
recursively split — so the mesh is seed-independent: every world at a
given level shares the identical graph, computed once per process and
cloned cheaply into each. It joins hierarchical seeding, coherent noise,
fields, the fact ledger, refinement, and the trace protocol's vocabularies
as a seventh kernel capability.

**The land arrived the way the sky did: as a small set of causes, drawn
once and never revised.** A world now carries a coarse planetary surface
— the Geosphere's cells partitioned into 8 to 40 Voronoi plates, each
flagged continental or oceanic, each spinning about its own Euler pole.
Where plates meet, the relative velocity's component along the
great-circle between two cells classifies the boundary: closing makes
collision ranges, coastal ranges and trenches, island arcs; opening makes
rifts and mid-ocean ridges; shear makes transform faults.

Elevation follows from geometry plus a rule table, not simulation.
Continents float at +400 m, abyssal floors sit at −4,000 m, and each
boundary's contribution decays inland by breadth-first graph distance —
shaped by a drawn per-plate *maturity*, so young belts stand sharp and
narrow while old ones lie worn and wide. A few drawn hotspots dome the
interiors. Sea level is then placed at exactly the elevation percentile
that drowns the drawn target ocean fraction. An *unrest* field — high in
young convergent zones, near zero in old interiors — is banked, unconsumed,
for future campaigns.

Nothing is serialized: a globe is re-derived from the world seed through
eight labeled streams, now in the manifest as permanent save-format
contracts. Three pins — `plates`, `ocean-fraction`, `supercontinent` —
round-trip through the ledger like every scenario pin, and the pin-isolation
battery proves a re-affirming pin yields a byte-identical globe. The gallery
gained "The Land of Seed 42", a hand-rolled PPM whose bytes are
drift-checked in CI, and the REPL now answers `map` and `land <lat> <lon>`.
A closing N-seed battery sweeps 64 seeds through every invariant at once —
every cell belongs to exactly one plate, both sides of a boundary agree on
its classification, elevation and unrest stay inside their envelopes, the
ocean fraction lands in its drawn tolerance, genesis is byte-identical on
replay, and convergent continental boundaries stand higher on average than
continental interiors — closing out spec §12's tectonic testing charter.

## What was learned

- **A mesh with no output is still worth its own campaign.** 3a's Geosphere
  had nothing to show on its own — no almanac line, no pin, no artifact —
  and folding its chronicle into 3b's, once 3b gave it something to carry,
  reads truer than inventing a demonstration it never needed.
- **Coarse constrains fine, structurally now, not just by convention.**
  Plate assignment, boundary classification, and elevation are three passes
  over the same Geosphere, each reading only what the last produced, which
  is what let each land in its own task with its own tests instead of one
  monolithic generator.
- **A rule table beats a simulation for a coarse tier.** Elevation from
  geometry, decay, and maturity produces boundary-following ranges and
  worn interiors — the qualitative shape a reader expects — without a
  single iterative solve, keeping generation O(cells) as the spec's risk
  register demanded.

## Deferred, deliberately (Campaign 3c's opening notes)

Climate and biomes — the sky's half of the thesis — read this globe's
elevation as an opaque `CellMap<f64>` without ever importing the terrain
crate. The terrain book *chapter* and a Census of Lands (metrics need
biome fields to be meaningful) wait for the biome map that will recolor
this same elevation. The Vale still hosts the village: settlement,
culture, and religion keep running on the tier-0 seam (spec §8) until a
later local-refinement campaign gives them a cell interior to stand in.

## Artifacts

[The Land of Seed 42](../gallery/elevation-seed-42.md) — the hand-rolled
elevation map, PPM and ASCII, drift-checked in CI alongside every other
committed artifact.
