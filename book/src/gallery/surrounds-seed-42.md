# The Purview of Seed 42

The situated chart `scene/surrounds/v1` describes and `hornvale scene
surrounds --render ascii` draws, addressed outside a possession session --
three genuinely different observers on the world of seed 42, each with the
same 31-cell, radius-4 neighbourhood the possession's own `map` verb draws
from. A CLI producer has no session, so every cell here is `here` or
`sensed` -- never `remembered`. A `remembered` cell is written only by a
consumer that owns a possession and therefore knows where its agent has
walked (see [the schema](../reference/scene-surrounds-v1.md)); this page
cannot draw one honestly, so it says so instead of implying a fog no CLI
query can see.

## Reading the chart

The chart is lattice-aligned, never north-up -- see [the schema's note on
why](../reference/scene-surrounds-v1.md#placement-integer-lattice-not-compass-bearing).
Scale is arc, not metres: the simulation defines no planetary radius, and a
depth-12 room's edge subtends roughly 0.015° -- a radius-4 neighbourhood
spans about one five-thousandth of the globe. `@` is always the observer's
own room. Every other glyph is the `terrain` lens's reading of a cell's
`water` and `relief` fields (a settlement or agent mark, where one stands,
pre-empts the terrain glyph):

| Glyph | Meaning |
|---|---|
| `@` | the observer's own room (`here`) |
| `~` | ocean |
| `=` | salt-basin |
| `+` | river |
| `_` | dry land, `abyss` or `shelf` relief |
| `.` | dry land, `lowland` relief |
| `:` | dry land, `upland` relief |
| `^` | dry land, `highland` relief |
| `A` | dry land, `alpine` relief |
| `#` | a settlement mark |
| `&` | an agent mark |

A `remembered` cell would draw a faded twin of its glyph instead --
`~=+_` fade to `-`, `.` to `,`, `:` to `;`, `^` to `n`, `A` to `a`, `#` to
`o`, `&` to `%` -- but no cell below is ever `remembered`, for the reason
above.

## The flagship settlement

The same ground a possession actually starts on -- room 738918402, the
default when `--room` is omitted. Its radius-4 neighbourhood is all
river/shelf/tropical-seasonal-forest: real ground and zero seams, but a
single biome throughout, which is why the other two observers below earn
their place in this page.

```text
{{#include generated/surrounds-seed-42/flagship.txt}}
```

## A coastline east of Mjoexaenoenoa

Room 897392747 (face 11, depth 12), half a degree east of the settlement
Mjoexaenoenoa (seen from a different angle in [the transport
topology](./connections-seed-42.md), where it reaches a neighbor by
sea-lane as well as by land). Here the neighbourhood itself is split: `~`
ocean glyphs fill the lower-left of the chart below, `_` dry land the upper
rows and the right, meeting close by `@` -- the first chart on this page
where the terrain, not just the observer, reads as ground meeting water.
(The chart is lattice-aligned, not north-up -- see above -- so this is a
description of the picture, not a compass bearing.)

```text
{{#include generated/surrounds-seed-42/coastline.txt}}
```

## A seam, disclosed

Room 724698318 -- latitude -10°, longitude 0° at depth 12 -- lands on base
icosahedron face 14, whose radius-4 neighbourhood reaches across a
different face's edge for 12 of its 31 cells. Those cells carry their room
id, state and semantics same as any other -- they are only unplaceable on a
flat chart, because the surface genuinely bends between two base faces and
no flat coordinate can say by how much without inventing one (see [the
schema's seam section](../reference/scene-surrounds-v1.md#seam-cells-real-ground-with-no-honest-place)).
The renderer states the count in its caption rather than dropping the cells
or fabricating a position for them.

```text
{{#include generated/surrounds-seed-42/seam.txt}}
```
