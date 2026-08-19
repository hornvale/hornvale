# The Purview of Seed 42

The situated chart `scene/surrounds/v2` describes and `hornvale scene
surrounds --render ascii` draws, addressed outside a possession session --
three genuinely different observers on the world of seed 42, each with the
same 31-cell, radius-4 neighbourhood the possession's own `map` verb draws
from. A CLI producer has no session, so every cell here is `here` or
`sensed` -- never `remembered`. A `remembered` cell is written only by a
consumer that owns a possession and therefore knows where its agent has
walked (see [the schema](../reference/scene-surrounds-v2.md)); this page
cannot draw one honestly, so it says so instead of implying a fog no CLI
query can see.

## Reading the chart

The chart is drawn **north-up**: the top of the picture is true north and
`@` is the observer, at the centre. Each cell's box comes from its own
great-circle bearing and distance about the observer, scaled so the band's
outermost ring lands at the edge of the picture and doubled horizontally
because a character cell is about twice as tall as it is wide -- see [the
schema's note on the
projection](../reference/scene-surrounds-v2.md#the-normative-screen-projection).
The gaps between glyphs are not missing ground: they are what a triangular
lattice looks like once it is drawn to true bearing rather than to its own
axes. Scale is arc, not metres: the simulation defines no planetary radius, and a
depth-12 room's edge subtends roughly 0.015° -- a radius-4 neighbourhood
spans about one five-thousandth of the globe. `@` is always the observer's
own room. Every other glyph is the `terrain` lens's reading of a cell's
`water` field, or — on dry land — of its `relief` and `micro` together (a
settlement or agent mark, where one stands, pre-empts the terrain glyph):

| Glyph | Meaning |
|---|---|
| `@` | the observer's own room (`here`) |
| `~` | ocean |
| `=` | salt-basin |
| `+` | river |
| `_` | dry land, lowest impedance |
| `.` | dry land, low impedance |
| `:` | dry land, middling impedance |
| `^` | dry land, high impedance |
| `A` | dry land, highest impedance |
| `#` | a settlement mark |
| `&` | an agent mark |

The five land glyphs are an **ordinal ladder of impedance** -- how hard the
ground is to cross -- and not of relief alone. Since
[The Illumination](../chronicle/the-illumination.md) a cell's rung is its
elevation band plus half its canopy closure plus half its terrain roughness,
each of the two perturbations bounded so that vegetation and unevenness
together raise a cell at most one rung above its bare relief. The flattest,
most open cell of a given band therefore renders exactly as the older
relief-only ladder did.

A `remembered` cell would draw the same glyph, dimmed -- the epistemic
channel is a weight, not a second alphabet -- and the escape-free `terrain`
lens carries no weight channel at all, so it says so in its caption instead.
No cell below is ever `remembered`, for the reason above.

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
(The chart is north-up, so "lower-left" and "upper rows" really are
south-west and north -- but they describe where the *drawn boxes* fall, and
the projection rounds onto a character grid.)

```text
{{#include generated/surrounds-seed-42/coastline.txt}}
```

## A seam, drawn

Room 724698318 -- latitude -10°, longitude 0° at depth 12 -- lands on base
icosahedron face 14, whose radius-4 neighbourhood reaches across a
different face's edge for 12 of its 31 cells. Those cells carry their room
id, state and semantics same as any other, but they carry **no lattice
coordinate**: the surface genuinely bends between two base faces and no flat
coordinate can say by how much without inventing one (see [the schema's seam
section](../reference/scene-surrounds-v2.md#seam-cells-real-ground-and-now-a-place-for-it)).

Under the lattice projection this chart drew nineteen cells and disclosed
twelve it could not place -- the whole eastern side of the picture was
blank. A great-circle bearing and distance are well defined across a face
seam, so all thirty-one draw now, and the caption's `placement:` line counts
them. The eastern cells are visibly on a different lattice from the western
ones, which is the honest picture: they are.

```text
{{#include generated/surrounds-seed-42/seam.txt}}
```
