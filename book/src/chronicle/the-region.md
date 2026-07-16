# The Region

The map learned to answer *closer*.

`scene/tiles/v1` resamples a whole world onto one global lattice, and a client
that zooms in eventually runs out of it — at the maximum width the lattice
already samples the surface finer than the surface's own resolution, and there
is nothing more to ask for globally. The orrery had built a cube-sphere
quadtree to descend past that point, addressing the sphere by
`face/level/ix/iy` down to metre scale, and it had no producer to feed it.
This campaign built that producer: a **regional** query returning the same
per-tile layers for one quadtree tile's footprint, at higher on-tile sample
density — `scene/tiles-region/v1`, a new schema alongside the global one.

The honest heart of it is a fidelity call, and it is the campaign's spine. The
world's substrate is a per-cell field over a geosphere at the canonical globe
level — roughly forty thousand cells, about a hundred and ten kilometres
apart. That spacing is the physical floor. A regional query cannot invent
sub-cell *physics*; below the cell spacing there is no more truth to sample.
What it can do is two things, and the contract states both plainly. It can
address the sphere as a streamable quadtree instead of one monolithic global
array. And it can **interpolate** the continuous layers between cell samples,
so a mesh built from a zoomed-in tile reads as a smooth surface rather than a
field of hundred-kilometre facets. Interpolation is cosmetic smoothing, not
new fidelity, and the page says so in as many words.

The interpolation splits the layers by their nature. Elevation, unrest,
temperature's mean and swing, moisture — the continuous quantities — are
barycentrically blended across the three geosphere cells whose triangle
surrounds a sample point. Ocean, biome, and plate — the categorical ones —
stay nearest-cell, because a category cannot be averaged: a tile is ocean or
it is not. The blend is transcendental-free, dot products only, so it is
byte-identical across platforms by construction, and it is exact at a cell
centre. A guard test earns its keep here: it asserts that some node's
interpolated elevation actually differs from its nearest cell, so that a
silent regression to nearest-cell — which would quietly defeat the whole
campaign — cannot pass unnoticed.

Temperature carried a small, satisfying theorem. The seasonal evaluator the
client reconstructs — mean plus swing times a sinusoid in the year — *commutes*
with interpolation, because the seasonal period is global: the same phase
factors out of every cell's contribution, so blending the actual temperatures
equals evaluating the blended coefficients. This is what lets the producer
ship a `temperature_grid_region` ground truth that the client's evaluator
reproduces from the quantized layers, and it is what the cross-repo contract
test pins — producer-sourced, never a client reconstruction admiring itself,
the lesson The Isotherm paid for. The commutation is exact only in real
arithmetic; in floating point the two orderings differ by rounding, so the
test asserts tight-approximate agreement, not bit-equality, and says why.

The consumer is the proof, kept deliberately small. The orrery parsed the new
document strictly, extended its temperature evaluator to accept it without
forking, and built a single patch from a tile's address using the same
cube-sphere projection its globe already trusts — then a test confirmed the
patch registers on the globe and seams exactly with its neighbour, sharing the
dyadic edge with no gap. The full camera-driven level-of-detail renderer — the
select, split, merge, and dispose of a live quadtree — is left to its own
campaign; what shipped here is the contract and the proof that the data lands
where it should.

Cutting the `world-wasm-v3` catalog to carry the new export told a quiet truth
about the world underneath. Re-pinning the orrery to v3 shifted The Isotherm's
own global temperature golden by most of a degree at elevation-sensitive
cells — not a regression, but the honest arrival of Sculpting's terrain, which
had reshaped the land since v2 and, through the lapse rate, the temperatures
that ride on it. A catalog release snapshots the whole world as it stands; the
number moved because the ground did.
