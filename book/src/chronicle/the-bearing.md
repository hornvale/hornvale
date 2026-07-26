# The Bearing

**July 2026 · outcome: merged — resolving a direction to the cell it lands on
stops scanning three full latitude rings and instead looks only where the
answer can be, making the elevation map render ~2.9× faster without moving a
single rendered byte**

## What was attempted

The world is observed through a mesh: every rendered map, every scene tile,
every step of a walk asks the same small question thousands of times — *given a
direction (or a point) on the sphere, which cell is nearest?* That lookup had
quietly become the most expensive operation in the whole observation layer.
Profiling the elevation-map render found it spending nearly four-fifths of its
time answering that one question, half a million times over — because for each
query it compared the target against every cell in three full latitude bands, a
ring wrapping the entire globe, when the answer was always within a few degrees.

The waste was invisible for a simple reason: it lived in a small helper, and it
only showed itself when the *renderers* were profiled rather than world
generation. World-building had been measured and tuned; the observation surface
that presents the world had not.

## What shipped

The lookup now consults a two-dimensional index — cells filed by latitude band
*and* longitude — and visits only the buckets in a window around the query, a
window that widens toward the poles (where a small step in position spans a
large sweep of longitude) and, close enough to a pole, opens to the full ring
again. The window's width is not a guessed constant: it is measured from the
mesh itself, from the longest edge between neighbouring cells, so it is
guaranteed to be wide enough to contain the true nearest cell at every
resolution the mesh is built at.

The delicate part was leaving the *answer* exactly unchanged. The old scan, on
the rare occasion that two cells are precisely equidistant, broke the tie a
particular way — the first cell it happened to visit. The new index visits cells
in a different order, so it cannot rely on visit order; instead it names the
winner explicitly — the equidistant cell that comes first by band and then by
identity — which is provably the same cell the old scan chose, in any order.
The result is bit-for-bit identical: pinned by a test that checks the new index
against the old scan at every mesh resolution over a dense sweep of the globe,
and confirmed end-to-end by the committed maps regenerating without moving a
byte. The elevation map render fell from about 1.8 seconds to 0.62 — roughly
2.9× — with scene and biome rendering lighter too.

## What it leaves reserved

The lookup is faster but still the largest single cost of a render, because it
still sweeps three full latitude bands vertically even while it now windows
longitude. Tightening the latitude axis, or — for the fixed grid a raster
render always uses — precomputing the pixel-to-cell mapping once and sharing it
across every map, would go further, and both are left as follow-ons. The two
more radical structures the sphere invites — descending the mesh's own
subdivision hierarchy, or adopting an equal-area cell-identifier scheme of the
kind astronomy uses — remain open: each is faster still, but neither reproduces
the existing lookup's exact answer without care, and this campaign's whole
discipline was to change the speed and nothing else.
