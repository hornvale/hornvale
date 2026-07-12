# The Room Mesh

**July 2026 · outcome: merged — the kernel now has a floor beneath the globe:
an addressable, lazily generated, deterministic room substrate that is *the
same icosphere, refined deeper*, so the seam problem never arises**

## What was attempted

Every scale below the canonical grid needed a place to stand. The globe is a
level-6 icosphere — 40,962 cells, each some hundreds of kilometres across — and
that is the resolution at which identity computes: sea level, drainage, land
components, placement. But a person does not live at a hundred-kilometre cell.
The Walk, room variety, a locale window, an object layer — every experiential
campaign that follows needs a unit of *place* finer than a globe cell, and it
needs that unit to be identical on every machine, cheap to summon, and never to
require re-running the planet.

The tempting move — raise the canonical grid to level 7, level 8 — is exactly
wrong. Each level quadruples a *global, mesh-bound, identity-bearing*
computation for the whole sphere, whether or not anyone is looking at a given
triangle. The Room Mesh is the opposite bet: **lazy, per-address, local, zero
global cost.** A room is summoned only when someone looks at it, and summoning
one costs work proportional to its depth, not to the size of the world.

## What a room is

A room is a **triangular face of the same icosphere as the globe, refined
deeper** — emphatically *not* a new coordinate system. `Geosphere` builds the
world by starting from the 20 faces of an icosahedron and calling `subdivide`
six times, each pass splitting every triangle into four by slerping its edge
midpoints onto the unit sphere. A room at refinement depth 7 *is* a level-7
triangle: the identical `subdivide` operation, one pass further, producing
byte-identical geometry. Refining to depth 16, 18, 20 is the same operation
again and again; an L18 room is roughly a 27-metre edge, an L20 room about 7
metres.

Because a room is nothing but "subdivide more," three hard problems dissolve at
once:

- **Seams never misalign.** Two adjacent rooms at different depths share an edge
  that is a subset of the coarser room's edge, refined by the identical midpoint
  slerp. There is no separate fine grid to register against the coarse one — it
  is all one mesh, examined at different depths.
- **The pentagon irregularity stays caged.** The only cells with five neighbours
  instead of six are the 12 original icosahedron vertices. Refinement never
  creates new ones, and — decisively — those 12 points never even touch
  edge-neighbour finding, because every icosahedron edge borders exactly two
  faces.
- **Determinism is free.** A room's geometry is byte-identical to the same
  triangle in a fully-built `Geosphere::new(depth)`. The subdivision spike
  measured this against a level-7 oracle across all 327,680 faces: `max|Δ| = 0`.

## The addressing

A room's identity is an integer address, never a position. `RoomAddr` is a base
face `0..20` and a path of digits `0..4`, one digit per refinement step; the
digit *is* `subdivide`'s child order (0, 1, 2 the three corner children,
3 the inverted centre triangle). The path length is the refinement depth. The
address is keyed to the base icosahedron, so it is independent of the world's
canonical globe level — the coarse cell a room belongs to is simply a prefix of
its path.

The serialized form, `RoomId`, is a frozen save-format contract: a single `u64`
with a self-delimiting bit-grammar. Five bits hold the face; the remaining bits
hold a *pathword* — a leading-1 sentinel followed by two bits per digit, root
digit first. The sentinel makes the word self-delimiting: the path length is
just the position of the highest set bit divided by two, so the empty path (a
whole base face) is the pathword `1`. Every bit is either face or pathword;
there are no unused bits, so the only ways a `u64` can be malformed are
`face ≥ 20` or `pathword = 0`. The natural cap is 29 digits (1 + 2·29 = 59 bits
of pathword), far past the ~L20 useful floor. Packing can fail (a `RoomAddr`'s
`Vec` can out-run the `u64`); unpacking can fail (a forged or corrupt key) — both
directions are checked, never infallible, because the untrusted-save future will
hand this function hostile bytes.

## The O(1) neighbour walk

Adjacency is where "the same icosphere" pays off most sharply. A room's triangle
is represented as an ordered triple of integer **barycentric** coordinates
`(a, b, c)` with `a + b + c = 2^depth` — the exact dyadic coordinates of the
triangle's position within its base face. The neighbour across the edge opposite
corner `W` is the lattice reflection `W' = U + V − W`, a single integer
subtraction. If every coordinate of `W'` stays in range, that is the interior
neighbour. If one goes negative, the edge lies on a base-face boundary: the walk
re-expresses the two shared corners on the adjacent face through a fixed 30-edge
gluing table — derived once from the base icosahedron, preserving the along-edge
parameter — and takes the one inward apex there.

The result is genuinely O(1): no recursion, no descent from the root, no floats.
The neighbour *order* is a contract — `neighbor[i]` is always the room across the
edge opposite corner `i` — so consumers can name and cache their exits stably.
The gluing table and the seam arithmetic were the campaign's keystone risk, and
they were validated exhaustively: against the level-7 oracle, every lazy
neighbour set equals the fully-built face adjacency, adjacency is mutual and
symmetric, every room has exactly three edge-neighbours, and the seam-crossing
count comes out to exactly `30 · 2^L · 2`.

## Coarse-field inheritance

A room does not store climate or elevation; it *inherits* them. Its
depth-`globe_level` ancestor triangle has three corners that are canonical-grid
vertices. Their positions — computed by the same slerp walk, so byte-identical to
the mesh — resolve to three `CellId`s in the already-resident globe, and the
room's centroid has exact barycentric weights within that ancestor triangle. The
weights are integer numerators over `D = 3 · 2^(depth − globe_level)` and sum to
`D`; a consumer blends `Σ (wᵢ / D) · field(cellᵢ)`. (The three corners of a
sub-triangle are dyadic, over `2^d`; the centroid *averages* them, which is where
the factor of three enters — a distinction the design's self-review caught before
it became a wrong denominator.) The weights are time-invariant; the field values
are functions of space *and* time, so seasonal and climatic variation is derived
at sample time and never disturbs a room's identity. Only this one function needs
to see the globe; addressing, geometry, adjacency, and seeding are pure functions
of the integer address and depend on nothing.

Transcendentals — the slerp's `sqrt`/`sin`/`cos`, the `acos` in a bearing — live
*only* in the geometry functions that feed rendering and coordinate display.
Everything a room *contains* is integer or rational, so the platform-libm hazard
that quantization exists to tame never touches identity, adjacency, seeding, or
the inheritance weights.

## The path: spike, then build

The campaign's shape was deliberate. The seam gluing was a keystone risk — get
it wrong and every deep room's neighbours are silently misplaced — so it was
retired *first*, in two throwaway kernel spikes: the O(1) walk and gluing against
the level-7 oracle, and adaptive depth with T-junctions across a seam. Only once
the spikes proved the hard idea sound was the real module built, TDD, in seven
small tasks each compiling and testing green: the primitive extraction and the
`RoomId` packing, then lazy geometry, then the neighbour walk, then the vertical
verbs and seeding, then the inheritance hooks. The spikes were the test design;
they ported directly into the oracle suite. Four ideonomy passes over the design
— loose-ends, room-reference, ecological psychology, and world-change — mostly
*confirmed* the layering and sharpened a handful of contracts, and the spec's own
self-review caught the dyadic-versus-centroid denominator before a line was
written.

## The deliberate non-goals

The Room Mesh is a **substrate**, and its restraint is the point. Four ideonomy
passes converged on the same boundary: this layer is umwelt-neutral locomotion,
and everything experiential rides above it.

- **Reference** — "get the spoon from the kitchen" — is a linguistic and object
  problem. P2 owns neither the naming nor the object; it provides only the three
  handles a *place* stands on: canonical identity (`RoomId`), geographic
  containment (a room's ancestors are exactly its address prefixes), and
  adjacency (for deictic reference). Most rooms are unnamed wilderness.
- **Perception** is not room-local — it is a vista out to the horizon across many
  rooms. P2 exposes the substrate (neighbours, positions, bearing); line-of-sight,
  affordances, place-knowledge, and getting lost are layers above. Room *variety*
  is what will make the world navigable, and it is the next campaign, not this
  one.
- **Change** is a delta overlay keyed by `RoomId`, never a mutation. Geometry and
  identity are immutable; a tunnel is an added edge between two ids, a burn a
  span over a coarse prefix, and most change isn't stored at all — it unfolds as a
  deterministic trajectory evaluated at any time from a single stored event, the
  room-scale echo of the astronomy ephemeris. v1 ships the immutable substrate
  and the stable delta keys, nothing more.

## The road ahead

The floor is laid. The room-scale *variety* layer (what makes a room worth
walking through, and the world navigable) and the palimpsest of change build
directly on this substrate; the locale window and the game's Local-space compose
over it. Adaptive depth and T-junctions — already spike-validated — will arrive
as an additive `neighbors_adaptive` beside the stable uniform walk, which does
not change. The chunked, partitioned ledger and its write authority wait for the
out-of-RAM, multiplayer future. What this campaign settled is the one thing all
of them depend on: below the globe there is now a place to stand, and it is the
same sphere, all the way down.
