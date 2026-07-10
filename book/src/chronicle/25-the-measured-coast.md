# Campaign 25: The Measured Coast

**July 2026 · outcome: complete, merged — a diagnosis given numbers, and the
same globe seen through a sharper lens**

## What was attempted

Every continent this generator has ever drawn has had the same faint
wrongness about it: shorelines that read as polygon edges, landmasses that
look like guitar picks. A brainstorm on continental rendering finally named
the cause rather than the symptom — a first-order Voronoi partition, one
continental flag per plate, produces near-convex regions at a single scale
by construction, because a plate's only relief is a boundary contribution
decaying inward from its edge. The ratified fix is a three-campaign
roadmap: **Crust** will give a plate an interior (a per-cell crust field,
cratons, real margins); **Sculpting** will give that interior relief at more
than one scale (modulated relief, erosion, hotspot trails). This campaign is
neither. Its premise is measurement before surgery — the shape
improvements Crust and Sculpting must eventually show are only real if
there is a preregistered "before" to show them against, recorded honestly
now, before either later campaign changes a single elevation.

So the work here splits into two pieces that never touch the elevation
field at all: instruments to quantify a shape nobody had ever put a number
on, and a lens refinement that makes the *existing* globe look better
without the globe itself moving underneath.

## What landed

**Six instruments for a shape nobody had measured.** A new `shape.rs` module
computes, over the derived tectonic globe and never from serialized state:
shoreline development (coastline length over a compact circle's
circumference — the limnological standard, 1 for a disk, several times that
for a fjorded coast), hypsometric bimodality and shelf fraction (Ashman's D
between land and ocean elevation populations, and the share of cells within
±200 m of sea level), continent count and largest-continent share
(deterministic BFS over land cells, seeded in ascending cell-id order so the
count never depends on iteration order), and the Gini coefficient over
plate cell counts. Each is a pure function over narrow inputs, tested
against hand-built synthetic globes — a hemisphere cap at the shoreline
index's floor, a deliberately crenellated mask scoring higher, hand-pinned
elevation maps for Ashman's D — before it ever touched a generated world.

**The Census of Coasts.** [Study 010](../laboratory/study-010.md) walks all
six metrics over the same 10,000 seeds every prior census has walked, and
every one of the 10,000 worlds has land, ocean, a shoreline, and a plate
roster to measure — no seed drops out the way a handful do on the
settlement- and pantheon-facing censuses. The baseline it recorded: a
shoreline-development median of 6.00 (four to twelve times a compact
circle's ratio, driven by a typical world fragmenting into a dozen-plus
separate landmasses — fragmentation compounding compactness, not
contradicting the near-convex diagnosis); hypsometric bimodality already
qualitatively right at a median of 3.04 (Crust and Sculpting must retain
this, not just improve the rest); a shelf fraction of 0.274, populated
mainly by plate-boundary decay slopes standing in for a real continental
margin; a continent-count median of 14 alongside a largest-continent-share
median of 0.79 (one dominant supercontinent-scale blob plus a scatter of
islands is the modal world); and a plate-size Gini of 0.26, tight and
low — exactly what a same-sized-on-average Voronoi tessellation predicts,
and the number Crust's plate-size work is most directly answerable to. Four
of the five target directions the study records were written into the
spec before the census ran; the fifth (bimodality, retained rather than
increased) is this campaign's own synthesis of the spec's framing, not a
literal quote — a distinction the chapter now draws explicitly rather than
blurring.

**The same world, a sharper look at it.** The elevation map's pixel pass
used to take a single nearest cell's value, so every coastline in every
rendered map was a chain of ~220 km cell silhouettes no matter how the
underlying globe was shaped. The render now interpolates elevation over the
nearest cells (a Gaussian-weighted blend, its width tuned so a cell flipping
which cell is "nearest" near a boundary never produces a visible seam) and
adds bounded, seeded coastal displacement — an amplitude that peaks exactly
at sea level and decays to zero within three envelope widths, so a
land/ocean flip is only ever possible where the interpolated elevation was
already within that band of sea level. This is coarse-constrains-fine
applied literally at the lens: the cell's elevation is still the prior, the
rendered pixel never leaves its envelope, and nothing about the *world*
changes — no `Stream` is ever consumed for the coastline noise (it is
stateless hash-noise under a new `terrain/coast-render` label), and the map
grows from 256 to 1024 pixels wide purely so the added detail is visible.
World JSON, every almanac, and the ASCII REPL map are byte-identical before
and after.

**A fixture that makes "lens-only" a checked claim, not a promise.** Before
any of the above landed, a new test committed the seed-42 default world's
full JSON as a fixture and asserted every future commit in this campaign
reproduces it byte-for-byte. A campaign whose whole contract is "the world
never changes, only the picture of it" needed something stronger than
discipline to enforce that contract — the guard fails loudly the moment
any change, however incidental, touches world identity, rather than relying
on a reviewer noticing a stray drawn value in a diff.

## What was learned

- **A metric is worth writing down even when you already suspect the
  answer.** The near-convex diagnosis was reached by eye, from a brainstorm
  looking at rendered maps. Measuring it turned "yeah, it looks like
  guitar picks" into six numbers with confidence intervals — and one of
  them, hypsometric bimodality, turned out to already be *right*, a finding
  the eye alone had not separately registered. A future campaign can now be
  judged against a baseline instead of a memory of what things used to look
  like.
- **A fixture guard is cheapest built first, not appended later.** Landing
  the world-identity test before any shape or render code existed meant
  every subsequent commit in the campaign carried its own proof of
  lens-only-ness for free; retrofitting the same guard after six commits of
  changes would have meant trusting that nothing had already drifted.
- **A preregistered direction and a literal quote are not the same
  claim.** Four of Study 010's five target directions came straight from
  the spec; the fifth was this chapter's own reasonable extrapolation. The
  first draft called all five quotes. Distinguishing "the spec said this"
  from "we synthesized this, consistent with what the spec implied" costs a
  sentence and keeps the preregistration honest about which claims were
  locked in advance and which were derived afterward.

## Deferred, deliberately

No terrain epoch of any kind: no crust field, no plate-shape change, no
`GLOBE_LEVEL` bump, nothing that alters a single drawn value — this
campaign's entire contract is that the fixture guard above must never fail.
**Crust** comes next, giving a plate an interior; **Sculpting** after that,
giving that interior relief at more than one scale. Both will be judged
against the numbers this campaign recorded, not against vibes.

## Artifacts

[Study 010, the Census of Coasts](../laboratory/study-010.md) — the
six-metric baseline over 10,000 seeds, with its preregistered target
directions. [The Land of Seed 42](../gallery/elevation-seed-42.md) — the
same globe, now rendered at 1024×512 with interpolated, coastally displaced
coastlines. The [Terrain chapter](../domains/terrain.md) carries both
additions in place.
