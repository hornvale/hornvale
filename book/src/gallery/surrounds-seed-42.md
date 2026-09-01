# The Purview of Seed 42

The situated chart `scene/surrounds/v2` describes and `hornvale scene
surrounds --render ascii` draws, addressed outside a possession session --
three genuinely different observers on the world of seed 42, each with the
same 81-cell, radius-4 neighbourhood the possession's own `map` verb draws
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
The gaps between glyphs are not missing ground. **Since The Pavement they
are also not what they were**:
the ground is a cube-sphere quad lattice now, not an icosphere triangle
lattice, so a radius-4 neighbourhood is a dense 9x9 block of 81 rooms rather
than a sparse ring of 31. A character grid cannot hold 81 boxes at their true
bearings without collisions, so two cells sometimes land in one box and the
caption says how many -- read the `placement:` line, which counts drawn and
occluded separately and always sums to 81. Scale is arc, not metres: the
simulation defines no planetary radius, and a depth-13 room's edge subtends
roughly **0.010°** (`√(4π / (6·4¹³))` = 1.767e-4 rad, the side of one of
the 402,653,184 quads that tile the sphere at the walk band) -- a radius-4
neighbourhood spans about a four-thousandth of the way around the globe.
`@` is always the observer's
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

The same ground a possession actually starts on -- room 3733133217 (base
cube face 1, depth 13), the default when `--room` is omitted. Its radius-4
neighbourhood is all river/tropical-seasonal-forest: real ground and zero
seams, but a single biome throughout, which is why the other two observers
below earn their place in this page.

The room id is not the one this paragraph carried before The Pavement
(738918402, base icosahedron face 2 at depth 12). Nothing chose a new
observer: `--room` is still omitted and the flagship settlement is still
Doaba's. The ADDRESS of that ground changed, which is the whole campaign.

```text
{{#include generated/surrounds-seed-42/flagship.txt}}
```

## Open water, and a caption that had stopped being true

Room 3015902083 (base cube face 3, depth 13), at 17.18° N, 103.68° W -- the
same ground the pre-Pavement id 897392747 addressed on base icosahedron
face 11 at depth 12, re-addressed and nothing more.

**This section used to be titled "A coastline east of Mjoexaenoenoa" and
described `~` ocean meeting `_` dry land close by `@`. None of that is
true, and none of it was true before this campaign either.** The chart below
is solid `~` to its edges, legend `bathypelagic`, 2,339 m below sea level,
and the same chart on `origin/main` -- `git show
origin/main:book/src/gallery/generated/surrounds-seed-42/coastline.txt` --
is the same picture. The terrain moved out from under this caption in some
earlier campaign and the prose beside it kept asserting a coastline. The
settlement it names is gone from the gazetteer too.

The Pavement corrects the caption rather than the observer, deliberately.
Re-pointing a showcase at different GROUND is a gallery-content decision, and
this campaign's diff is worth keeping readable as pure re-addressing;
restoring a chart that really does show ground meeting water is open work.
What the chart is good for meanwhile is the thing this page's opening
paragraph now claims: 81 cells, 63 drawn and 18 occluded, summing to the
whole ball.

```text
{{#include generated/surrounds-seed-42/coastline.txt}}
```

## A seam, drawn

Room 2290649216 -- 10.73° S, 44.99° W at depth 13 -- sits a fifth of a room
off the -45° meridian, which is where two faces of the base cube meet. Its
radius-4 neighbourhood reaches across that edge for **36 of its 81 cells**.
Those cells carry their room id, state and semantics same as any other, but
they carry **no lattice coordinate**: the surface genuinely bends between two
base faces and no flat coordinate can say by how much without inventing one
(see [the schema's seam
section](../reference/scene-surrounds-v2.md#seam-cells-real-ground-and-now-a-place-for-it)).

**This is the one observer on the page that could not keep its ground.** It
was room 724698318 -- 10° S, 0° E on base icosahedron face 14 -- and that
point was chosen because it lay on an ICOSAHEDRON face edge. The cube's face
boundaries are elsewhere entirely: they are the great circles where two of
|x|, |y|, |z| are equal, 45° from each face's centre, and 10° S / 0° E is
about 35° inside the +x face. Against a walk-band room roughly 0.010° across
that is thousands of rooms from any seam, so keeping the coordinate would
have kept a section that no longer had a subject. The replacement address is
REACHED rather than chosen -- it is the first candidate a search over the
base faces' edge-descending paths returns, the same search
`windows/scene/src/surrounds.rs` uses to find a seam observer for its own
tests.

A great-circle bearing and distance are well defined across a face seam, so
every one of the 81 cells is placed; 48 of them get their own character box
and 33 share one, which the caption's `placement:` line counts. The eastern
cells are visibly on a different lattice from the western ones, which is the
honest picture: they are.

```text
{{#include generated/surrounds-seed-42/seam.txt}}
```
