# 0677. A line-carried feature conserves its length, not its rasterized area

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (autopilot, spec §8.2) ·
**Relates:**
[0124](0124-a-refinement-preregisters-a-conservation-criterion.md) (a refinement
must preregister a conservation criterion — this record decides *which*
criterion a line-carried feature owes),
[0130](0130-a-sub-threshold-watercourse-is-a-narrow-channel.md) (The Ford, which
made the channel network carry every reach as a line — this record generalises
that carrier choice into a measurement rule),
[0121](0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md)
(the neighbouring arithmetic whose −29% revert was measured in area, which is
why area was reached for here),
[0289](0289-the-map-is-layers-with-distinct-cache-keys.md) (the layer and cache
key the rasterization rides on),
[The Hachure](../../book/src/chronicle/the-hachure.md)

In the context of *The Hachure* rendering rivers into the world plate from
the flow graph, and needing to know whether a new rule preserved the world's
hydrology, we decided that **the conserved quantity of a feature carried as a
line is its channel length and its network connectivity — never the number of
cells its rasterization happens to cover** — accepting that this invalidates
comparisons against every raster river count the project has previously
taken, including two of this campaign's own.

## The decision

When a feature's carrier is a polyline, a refinement of how it is drawn is
audited against **length and connectivity**. Its rasterized area is
resolution-dependent by construction: a line covers `O(N)` of an `N × N`
chart, so the fraction of cells it occupies must halve with every doubling of
resolution. A rule whose cell fraction is *flat* across rungs is therefore
not conservative — it is carrying the line as an area, which is the type
error 0130 named.

## Why it needed ratifying

Both of this campaign's Stage-2 candidate rules were measured in raster area
first, and both readings were meaningless in the same way:

| candidate rule | measured against today's rung-6 raster |
| --- | --- |
| channel test at the tile centre | 154 → 6 river tiles (**−96.1%**) |
| within half a tile of any channel | 154 → 2,284 (**+1383%**) |

The second was written up as a falsification and was not one. Today's rule
draws ~0.98% of tiles at **every** rung (0.98 / 0.99 / 0.99 / 0.96 at rungs
6 / 8 / 10 / 13) — dead flat, the signature of the area-carried defect — while
the rejected rule gives 16.47 / 3.94 / 1.00 / 0.14, halving per rung as a
rasterized line must. The baseline was the bug, so conserving against it
would have preserved it.

The general statement, which is the part that outlives this campaign:
**connectivity is a property of the line, not of any point on it**, so no
per-tile query can guarantee it however finely the query is refined. A
segment-versus-footprint intersection produces river *scatter*, because
`ChannelNetwork::nearest_line` returns the nearest line of any size and the
answer flips between a trunk and its tributaries along the trunk's own
length. Walking the polyline gives connectivity by construction; sampling
never does.

## Consequences

- Rivers are rasterized from the polylines inside `draw_terrain_layer`, on
  the terrain layer's own cache key and its never-invalidated lifetime
  (0289) — 11,202 segments for the whole planet against 20,000 nearest-line
  queries for one 200×100 plate under the sampled design. Sampling costs
  screen area and is flat at every rung; rasterizing costs river length in
  view, and so gets cheaper as the reader zooms in.
- A river-density figure quoted without its rung is not a measurement. Any
  future comparison of two rendering rules states the rung and reports the
  trend across rungs, not a single ratio.
- **Which** watercourses a view draws remains a client-side rendering budget
  and is not conserved at all — one client at seven rungs needs seven
  cutoffs. What the sim owes is a named magnitude ladder to cut on, which is
  filed as `MAP-stream-order-is-sim-truth` and is not this record's subject.
