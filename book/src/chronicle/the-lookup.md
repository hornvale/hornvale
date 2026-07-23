# The Lookup

**July 2026 · outcome: merged — world genesis gets ~38% faster without moving
a single committed byte, by replacing two "re-derive it every time" hotspots
with a value looked up once**

## What was attempted

Genesis had grown fast enough to profile honestly. A sampling flamegraph of
building a single world pointed at two places the machine was doing the same
arithmetic over and over — and both turned out to be the *same mistake wearing
two costumes*.

The first: the geosphere, the icosphere mesh every domain computes over, stored
each cell's position as a 3-D point on the unit sphere and, whenever anyone
asked a cell's latitude and longitude, worked them out again from scratch — an
`arcsine` and an `arctangent` per question, thousands of times per world, always
returning the same answer for the same cell. The second: the transport
topology, the graph of natural travel routes, stored its adjacency in a
*balanced tree* keyed by cell — a structure built for sparse, unpredictable
keys — even though its keys were the dense, complete run of every cell from the
first to the last. Every edge added paid for a tree descent the keys never
needed.

## What shipped

Both are the same insight: **a value determined by a dense, contiguous index
need not be a search or a recomputation — it can be a single array lookup.**
The mesh now computes each cell's coordinate once, when the mesh is built, and
hands back the stored answer. The graph now keeps its adjacency in a flat array
indexed directly by cell, which is exactly the representation the rest of the
world already uses for per-cell data — the graph had simply been the one place
that forgot the house style.

The payoff was measured, not hoped: building the canonical world dropped from
about a second to under two-thirds of one, and the graph's edge-insertion,
which had been the single busiest routine in all of genesis, fell to near
nothing. The transcendental arithmetic the mesh had been repeating shrank by a
third.

The discipline that made this safe is the campaign's real content. Hornvale's
worlds are *byte-identical* by contract: the same seed must produce the same
world down to the last digit, on every run and every machine. A performance
change that alters an output is not an optimization, it is a corruption. So
both changes were made **byte-identical by construction** — the cached
coordinate is the very same expression the code used to evaluate, on the very
same stored point, computed once instead of many times; the flat graph iterates
its cells in the identical order the tree did. This was not asserted and hoped;
it was proven: the canonical world's bytes were checked unchanged, forty
further worlds were checked identical between the old code and the new, and the
existing reproducibility guards stayed green throughout.

## What it leaves reserved

The flamegraph named more than it fixed. After these two changes the busiest
remaining cost is memory allocation — the pervasive habit of collecting
intermediate results into fresh lists — and a solar-geometry loop that
recomputes the sky's arithmetic per cell per day. Both are real, and both are
left for a later pass, because they are different shapes of problem than the one
this campaign was about. The rule this campaign leaves behind is narrow and
sharp: a collection keyed by a dense index is an array, not a map; a pure
function of unchanging data is computed once, not on demand. Where the code
already followed that rule, it was fast; the two places it didn't were the two
places worth fixing.
