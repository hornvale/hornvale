# Crust

**July 2026 · outcome: merged — the guitar pick is dead, measured against
its own preregistered bands, four of six inside and the other two honestly
handed to Sculpting**

## What was attempted

The Measured Coast gave the guitar-pick continents a diagnosis and a number:
a first-order Voronoi partition with one continental flag per plate produces
near-convex regions at a single scale, and Study 010 recorded exactly how
convex, how few, how uniform. Crust was the surgery that diagnosis prescribed
— terrain epoch v2, a deliberate one-way regeneration of every world.

Three things changed at once, because they are one idea. **Continents stopped
being plates.** Continental crust became a per-position *field* — a stateless
function of a point on the sphere, drawn from a set of *cratons* (nuclei with
a centre, a radius, an age) whose lobed kernels sum to a thickness at any
point, with no per-cell draws anywhere. A continent exists now because a
craton exists, not because a Voronoi cell was flagged. **Plates kept their
skeleton but grew heavy tails and ragged edges** — the same drawn seed
positions and Euler motions as v1, but weighted so a few plates dominate and a
fringe are slivers, with stateless noise wandering the boundaries off the
great circles. **Elevation became isostatic** — Airy flotation over crust
thickness, so thick craton interiors ride high and the kernel's taper crosses
sea level as a genuine shelf, giving passive margins for free. On top of that
the canonical grid rose to level 6 (40,962 cells), with a `--globe-level` pin
that makes resolution a first-class axis of world identity rather than a
compile-time constant.

## What landed

The epoch retired what it contradicted. The `plate-kind` stream and the
per-plate continental flag are gone — superseded by the crust field, their
labels never to be reused. Seed 42 kept its recognisable plate skeleton (the
skeleton streams survived untouched, by design, so the before/after is a
controlled experiment) but grew entirely new continents on it. The elevation
map tells the story at a glance: a dozen lobed landmasses with bright craton
cores tapering through foothills to wide shelf-coloured margins, island arcs
tracing the noisy plate boundaries through open ocean — nothing that reads as
a guitar pick anywhere on the globe.

Because the crust fields are pure functions of position, this campaign also
ratified a contract the render lens first hinted at: **identity computes on
the canonical grid; observation samples fields.** Sea level, drainage, land
components, placement, and paleoclimate strata are mesh-bound and compute once
on the world's chosen level; everything pointwise is a `Field` any resolution
may resample. The `--globe-level` pin joins world identity accordingly — the
same seed at a different level is deliberately a different world.

## What was learned — the tuning loop as method

The campaign's real story is not the code; it is that the code was *judged*.
The Measured Coast preregistered six acceptance bands, Earth-anchored, and
Crust was held to them across a tuning loop where every iteration was a
recorded commit with its predictions logged *before* the census that tested
them. That discipline earned its keep twice by being wrong on purpose:

- **Iteration 1** (area-normalised craton budget, raising `PEAK_MIN_KM` so aged
  cratons can still surface) fixed shoreline development and moved the rest,
  but the continents fragmented into a swarm of tiny islands — 50-odd land
  components per world against a band ceiling of 12.
- **Iteration 2** (a smooth tanh lobing remap, a softer plate-size tail, a
  craton-repulsion pass) fixed the plate-size Gini and *refuted two
  hypotheses by direct measurement*: the pinch-off theory (the islands
  survived the smoother rims) and the repulsion theory (separated cratons
  still merged through their overlapping skirts). Refutation, recorded, is
  data.
- A read-only **provenance probe** then found the actual culprit: sea level was
  sitting three to four kilometres deep in the abyssal plain, so every taper
  ring and every noisy plate-boundary uplift surfaced as an islet. The two
  independently-drawn numbers — craton budget and ocean fraction — were a
  supply/demand mismatch.
- **Iteration 3′** coupled them: the craton budget is now *derived* from the
  ocean fraction, matching continental supply to the land quota by
  construction, on every seed. Sea level landed on the taper. Continent count
  collapsed from 54 to 8, largest-continent share from 0.75 to 0.30,
  hypsometry went bimodal — the structural war, won.

Four of six bands sit inside their Earth-anchored ranges. The two that do not
— shoreline development and shelf fraction, both *low*, both coast texture —
are not a tuning failure but a scope boundary: a probe proved Crust's
craton and lobing knobs cannot reach them, because coastline complexity and a
true depositional shelf are products of *erosion and sediment*, which are
Sculpting's mandate, not Crust's. They are recorded openly in Census of Coasts
II and scoped to the next epoch, alongside a metric supersession — continent
count gained an Earth-calibrated 0.5%-of-land size floor (Greenland counts,
Iceland does not), with an unfloored `landmass-count` preserved beside it
forever. The baseline was recorded at two thousand seeds rather than ten: a
probe confirmed the medians match, every verdict margin dwarfs the sampling
noise, and a ten-thousand-seed run is poor value on un-optimised census
infrastructure — a judgement the lab-performance campaign will retire.

Deep Time collected its dividend without a line of paleoclimate code changing:
with a real shelf under a moving sea level, the eustatic swings now flood and
expose continental margins, and the deep-time gallery redrew itself with land
bridges and drowned plains that the cliff-coast world could never have shown.

The tongues and words identity keystones retired here, which is itself a
ratified principle now: a generator that *contradicts* its predecessor is an
epoch, not a coexisting tier, and keystones frozen on pre-epoch terrain
complete their evidentiary job at their own campaigns' merges and retire when
the terrain beneath them is deliberately remade.

## The road ahead

Two campaigns follow, in this order. **Lab Performance** comes first, and this
epoch is why: at level 6 a full-globe census builds tens of thousands of
worlds, and the ones that measure only a single layer still pay for all of
them — a thousand-world language study that never reads terrain, an
un-optimised craton lookup costing hundreds of millions of allocations per
run. Those costs have direct remedies (regenerating only what changed,
building only the layer a study reads), and until they land, every
regeneration of the world is dear. Every saving that campaign banks,
**Sculpting** (terrain epoch v3) then spends across its own tuning season,
where the two bands Crust handed forward finally come inside: erosion carving
coastline complexity, sediment building the shelf.
