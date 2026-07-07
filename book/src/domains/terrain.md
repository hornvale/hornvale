# Terrain

**Questions it answers:** What places exist? What is each like — its name,
its biome? (Eventually: what is *between* places, and how does land shape
everything built on it?)

**Tier 0 — one hand-placed vale.** A single place, "the Vale," a temperate
forest, committed to the ledger at genesis as three facts: it exists as a
place, it has a name, it has a biome. Terrain is the first domain that
*writes* — its genesis is the first real exercise of the fact envelope, and
the two predicates it registered (`is-place`, `biome`) were the concept
registry's first domain-contributed vocabulary.

**Why hand-placed is honest.** Tier 0 makes no pretense of generation: the
Vale is authored, exactly one exists, and the `places` query simply reads
the ledger back. What this buys is the *interface* — settlement genesis takes
"a place to stand" as an opaque entity, the almanac lists places without
knowing how many there are, and the REPL's `places` command works — so that
when generation arrives, nothing downstream changes.

**Tier 1 — the tectonic globe (Campaign 3, reviving the vision book's
deleted chunking chapters).** Every new world now carries a second,
independent answer to "what places exist": a coarse planetary surface, not
a region graph but a kernel **Geosphere** — an icosphere subdivided to
10,242 cells, identical for every world at a given subdivision level — with
plates, motion, and elevation drawn and derived on top of it. Nothing here
touches the Vale; the tier-0 seam is a deliberate, documented transitional
choice (spec §8), kept exactly as it was until a later local-refinement
campaign gives settlement a cell interior to stand in. The globe is the
*coarse* half of the coarse-constrains-fine principle: it exists so that
climate (the *fine* half, in the next chapter) has real elevation and
coastline to answer against.

The land arrives the way the sky did: as a small set of causes, drawn once
and never revised. A plate count (8–40) and that many seed points scatter
across the sphere; every cell joins its nearest seed (a spherical Voronoi
diagram), and each resulting plate is flagged continental or oceanic
against a drawn target. Each plate spins about its own Euler pole, giving
every cell a tangent velocity. Where two plates meet, the relative
velocity's component along their shared boundary classifies the contact:
closing makes collision ranges, coastal ranges and trenches, or island
arcs; opening makes rifts or mid-ocean ridges; shearing makes transform
faults. Elevation follows from a rule table, not a simulation — a
continental or oceanic base, plus a boundary contribution that decays
inland by graph distance and is reshaped by a drawn per-belt *maturity* (young
belts stand sharp and narrow, old ones worn and wide), plus a few drawn
hotspots. Sea level is then placed at exactly the elevation percentile that
drowns a drawn target ocean fraction, and an *unrest* field — high near
young convergent boundaries, near zero in old interiors — is banked,
unconsumed, for future campaigns (spec §15).

**The model card.**

- **Drawn (from the seed, or pinned):** plate count; plate seed positions;
  each plate's continental/oceanic flag; each plate's Euler-pole motion;
  hotspot positions; each orogenic belt's maturity; the target ocean
  fraction.
- **Derived (geometry and rule tables, not simulation):** cell→plate
  assignment; per-cell velocity; boundary classification; elevation; sea
  level and the resulting land/ocean mask; the unrest field.
- **Approximated (declared):** a static present-day snapshot — no drift,
  no deep time (that is a later provider behind the same interface); no
  erosion, sediment transport, or isostasy beyond the base rule; boundary
  classification from instantaneous motion, not an accumulated stress
  history; uplift falloff is an analytic decay curve, not a solved stress
  field.

Three pins join the astronomy pins that already steer climate: `--plates
N`, `--ocean-fraction F`, `--supercontinent`. They persist as scenario
facts and obey the same stream-consumption isolation every pin does — a
re-affirming pin reproduces a byte-identical globe. Nothing about the
globe is serialized; a world re-derives it from the seed through eight
labeled streams, permanent save-format contracts in the manifest.

The gallery holds the artifact: [The Land of Seed
42](../gallery/elevation-seed-42.md), a hand-rolled elevation map (PPM and
ASCII), drift-checked in CI like every other committed artifact. Chronicle:
[3b, the tectonic globe](../chronicle/campaign-3b.md).

**The tier ladder ahead:** place names that feel authored rather than
generated, and eventually the region-graph refinement that gives a
habitable cell a walk-around interior. The anti-repetition mandate from the
vision book's dungeon chapter ("every room must feel created with intent")
remains terrain's standing acceptance test.
