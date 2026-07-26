# The Purview

*The first time the walker could see where they were standing.*

Until this campaign the possessed player was told where they were. Every
spatial fact — three ways on, a settlement somewhere off that way, ground
already crossed — arrived as a sentence, and the player assembled a mental
map out of prose. The prose was doing two jobs and doing the second one
badly. The Purview gives the possession a coarse chart: a small, fogged,
deterministic picture of the rooms around the agent, drawn at the scale the
agent walks and sharing one noun catalog with the prose that describes it.

It is the first rung of the tilemap view the rendering strategy has charted
since the scene protocol was designed, and deliberately not the graphical
one. What shipped is the protocol half — `scene/surrounds/v1`, the situated
pole the protocol has named and never built — plus an honest in-process
render of it. A browser client drawing tiles is now pure client work against
a fixed contract, which is the whole reason to build in this order.

## The map is the mesh, and the mesh is triangles

The obvious way to build a locale map is the way every other map in the
project is built: sample the cube-sphere quadtree the atlas and the orrery
already use. That would have been wrong, and the reason is the campaign's
first real finding. A situated observer does not walk the quadtree. They
walk the **room mesh** — the triangular faces of a subdivided icosahedron,
where every step is an edge crossing to one of exactly three neighbours. A
chart whose cells were quadtree tiles would show squares to someone moving
between triangles: the picture would disagree with the walk, which is
precisely the dishonesty the lens discipline forbids.

So the chart is the real lattice. Placement is exact integer barycentric
arithmetic — the kernel gained `RoomAddr::face_lattice()`, exposing what
`bary_triple` had computed privately all along — and no transcendental
touches it. The alternative, placing each cell at the compass bearing of its
exit, fails on a sphere for a reason worth recording: a room's three exits
bucket to three distinct compass points everywhere, but *which* three drifts
with latitude and with the local orientation of the icosahedral face. Ten
observers sampled from the equator to 85° N gave `E, Nw, Sw` in one place and
`N, Se, W` in another. A bearing-placed chart would have distorted the mesh
differently in every part of the world.

Two consequences follow that no one designed. **Zoom is path truncation:** a
room's address is a face plus a path, so a coarser chart is the same builder
run at `path[..len - n]`, and an entire planned aggregation layer evaporated
before it was written. And where a neighbourhood reaches across a base-face
edge, the surface genuinely bends; those cells carry their room id and their
semantics but no coordinate at all, marked `seam`, and the renderer states
them in its caption rather than dropping them or inventing a position. Real
ground with no honest place on a flat chart is a fact about a sphere, not a
bug to paper over.

## Two grains of one lens, made falsifiable

The founding framing of the game arc holds that map and prose are two grains
of *one* lens over one query surface, joined by attention — not two content
pipelines that happen to agree. That is easy to assert and easy to ship
something that merely resembles. This campaign's real deliverable is the
statement that could prove it false: for every turn of a possession, the
nouns `examine` accepts equal the union of the prose's catalog and the
chart's legend; every mark resolves to a datum; and a noun surfaced at both
grains resolves to *the same* datum, the prose's, because prose is primary.

The union is not a formality. On every turn the chart surfaces two nouns the
prose does not and the prose surfaces two the chart does not, and the two
they share carry textually different data — so precedence is a decidable
question rather than a tie. `examine` was widened from "nouns the prose
mentioned" to "nouns this lens surfaced, at either grain", which is the
minimal honest generalization of a contract the vessel already had.

The fog cost nothing to build because it already existed. Every visit writes
a `room/<id>` key into the session's knowledge; a cell not currently sensed
but present in that map is *memory* rather than sight. Nothing is stored,
nothing is committed, no predicate and no epoch — the chart is a pure derived
view, so a possession that draws it a thousand times is byte-identical to one
that never does.

## What the discipline caught

Three findings are worth keeping, because in each case the shipped artifact
looked correct.

A fail-loud fix exposed a **silent mislabel in existing code**: the climate
domain names biomes in kebab-case while the locale window renders them
spaced, and the chart resolved a biome by matching those strings. Every
multi-word biome had been quietly resolving to index zero — on seed 42 all
thirty-one cells reported *ice* for a tropical seasonal forest. Single-word
biomes matched coincidentally, which is why nothing had noticed. The repair
was not the translator that would have made it pass; it was to compare enums
the way the sibling builders already did, deleting the string round-trip that
was the defect class rather than the instance.

Looking at the picture caught what the tests could not. The chart passed
every assertion while rendering a **leaning parallelogram**: the placement
formula did not compensate for the lattice's row offset, so the cell that
should sit directly below its neighbour landed down-and-to-the-right, and the
shear accumulated across the neighbourhood. One `+ w` term cancels it exactly,
and a breadth-first ball reads as the symmetric hexagon it always was.

And mutation testing found two clauses of the campaign's own thesis that
**could not fail**. Reversing the precedence so the chart won a collision left
the whole suite green; so did killing movement entirely, which degenerated the
six-turn walk into six repeats of the first turn with nothing noticing. Both
are now pinned, each verified red under its mutation before being restored.
A test that cannot fail is not a weak test, it is not a test, and only trying
to break it tells you which you have.

What opens here is the rung above: the same document, drawn as tiles in a
browser, with a per-species sense radius deciding how far the purview reaches
and a memory that can be wrong rather than merely stale. The protocol is in
place for all of it. For the first time, someone walking in the world can see
the shape of where they are.
