# The Lexicon of Place

Hornvale addresses one sphere at every scale, from a continent down to the
corner of a room. This page fixes the words for the parts of that address —
what each one means, which collisions were removed, which were kept on
purpose, and which spellings are frozen forever regardless of what the code
calls them.

It exists because a vocabulary is a prerequisite for a design conversation,
not an ornament on one. A larger question — whether every zoom rung should be
a uniform square grid — stalled on the discovery that the people asking it
could not be sure they meant the same thing by *level*, *band*, or *room*.

**That question is now half-answered, and the vocabulary below is why the
answer could be stated at all.** [The Quadrat](../chronicle/the-quadrat.md)
settled the walking band and the consulted map: every zoom rung the game
client draws *is* a uniform square grid, and a **rung is a facet depth**
([decision 0287](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0287-a-zoom-rung-is-a-mesh-depth.md))
— band B is depth 13, coarsening to globe level 6. (0287's own text says
depth 12 and seven rungs. That was true when it was ratified and is not now:
The Pavement moved the walk band one rung finer when the lattice became a
quad grid, so `walk_depth` is `globe_level + 7` and the ladder runs 6 through
13 — eight rungs. Decisions are append-only, so the record keeps its
numbers; its *rule* — a rung is a facet depth — is what binds, and that is
untouched. Neither figure is tabulated in code: both are derived, which is
why the drift was invisible.) A tile is a facet; the
terrain it shows is read from that facet's corner *vertices*. Both words
below are load-bearing in that one sentence, and they name different
lattices, which is what the stalled conversation could not say. The half still open is the chamber band,
which is a square 4-neighbour lattice of its own and is not a facet depth at
all.

## One sphere, two indexings

The world is an icosphere: an icosahedron subdivided, its vertices projected
onto the unit sphere. Two different things live on it, and confusing them is
the mistake this vocabulary exists to prevent.

| word | what it is | what you do with it |
|---|---|---|
| **Vertex** | a **point** — one vertex of the subdivided icosahedron | sample a field at it: elevation, temperature, moisture, biome |
| **Facet** | a **patch** — one quad of the cube-sphere, at a refinement depth | occupy it; stand in it; walk from it to a neighbour |

Neither is more fundamental — a field is a function on points, and a place is
a region you can be inside, and the world needs both.

**But they are not duals, and this page said for some months that they
were.** The two live on *different lattices*. Vertices are the icosphere's:
a subdivided icosahedron, `10 · 4^L + 2` of them, 40,962 at the canonical
level 6. Facets are the cube-sphere's, a quad grid whose width doubles per
refinement depth. The Pavement moved the base mesh to that quad lattice and
this table kept describing the triangles it replaced — a facet's corners are
**four** vertices now, read through `Facet::corner_weights`, not three
barycentric ones.

The two lattices are also **incommensurate**, which is the part that bites.
At globe level the chart is 256 tiles around a great circle against 363
vertices, so a tile boundary is not a vertex boundary and one vertex can
dominate a whole facet. That is not a defect to be filed down: a facet is
where you *are* and a vertex is where the world was *measured*, and there is
no reason those should coincide. It is the reason a tile must read its
terrain by blending its own four corners rather than snapping to a nearest
sample ([decision 0676](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0676-a-view-may-interpolate-between-its-samples-but-still-may-not-invent-below-them.md)).

The names say which is which, which is the entire point. Before this
vocabulary the two were `CellId` and `RoomAddr`, and neither word carried the
distinction: in GIS and raster convention a *cell* is an area, so calling a
point a cell actively misleads a reader who knows the convention and
mis-teaches one who does not. And `RoomAddr` addresses every depth from the
base icosahedron down, so it called a continent a room.

A **`VertexMap<T>`** holds one `T` per vertex — a dense array indexed by the
vertex, not a map in the associative sense. A **`FacetId`** is a `Facet`
packed into a `u64` for serialization: five bits of base face, a sentinel,
then two bits per refinement step.

## Depth

**Depth** is refinement: how many times the mesh has been subdivided beneath
the base icosahedron. A facet at depth *d* has an edge roughly
2<sup>−*d*</sup> of a base-face edge; depth 0 is one of the twenty faces of
the bare icosahedron, and the cap is 29, where a `FacetId` runs out of bits.

Depth is the only refinement word. Where the code once said *level* for this,
it now says *depth* — but see the four levels below, because two of them
survive and mean other things.

## The three vertical ladders

All three describe "how far down", and they are not the same ladder. Keeping
three words is deliberate; whether these are the *right* three is an open
question, tracked as `PROC-three-ladder-vocabulary` in the idea registry.

| word | crate | what its rungs are | what it answers |
|---|---|---|---|
| **`Band`** | `kernel` | Surface, Undercroft, Shallows, Deeps, Underdeep, Nadir | how deep into the underworld — a habitability ladder |
| **`Stratum`** | `domains/climate` | Surface; Epipelagic…Hadal; `Rock(Horizon)` | where in *this column*, relative to its realm |
| **`Horizon`** | `kernel` (re-exported by `domains/terrain`) | Regolith, Cover, Basement, Roots, Underneath | which rock layer, geologically |

The distinctions that matter:

- **`Band` is a rung you descend to**; `Stratum` and `Horizon` are positions a
  column *has* whether or not anything is standing there. A cave chamber sits
  at a band. A fish sits in a stratum. A rock sample belongs to a horizon.
- **`Stratum` spans both media and `Horizon` does not.** A stratum is
  realm-relative, so it covers the water column (`Epipelagic` down to `Hadal`)
  and the rock column alike; its rock half is `Horizon` directly
  (`Stratum::Rock(Horizon)`), not a mirrored roster of its own. `Horizon` was
  the same forced-duplicate shape `Band` corrected: climate cannot import
  terrain, so before this it carried five variants that named the same rock
  units under their own spellings, kept honest only by a name-by-name test.
  Moving `Horizon` to the kernel (decision 0517 clause (a)) let `Stratum`
  embed it structurally instead, so the two can no longer drift apart by
  construction and the composition root's `stratum_of_band` identity match
  had nothing left to do.
- `Band` was consolidated into the kernel from three separate models — a rung
  enum in terrain, a mirrored zone enum in climate, and a bare rank on a
  chamber address — before this vocabulary was written, and `Horizon`
  followed the same path for the same reason. Renaming either again would
  relitigate a deliberate recent decision without new information.

The registry row notes what is still unsatisfying: `Horizon` is a
soil-science word doing a crustal job, and `Stratum`'s water members are
precisely the pelagic zones and could say so. Neither is fixed here.

## The realm vocabulary

A realm is not an enum of worlds. It is a triple — **medium**, **aperture**,
**strata** — and the sharp move in it is that the discriminator is *access*
rather than *materiality*. What separates the sea from the deep earth is not
what they are made of but how you get in.

An **Anchor** is a named thing with relations and no coordinate: a fireplace,
an alcove, a screen. It is *at* a place without *being* a place, which is why
it needs its own word.

A **Chamber** is a room-scale interior a structure put there. Note what it is
not: an address at chamber depth is a chamber only because something built one
there. Below the walk band an address is **identity, not shape** — its
geometry means nothing and connectivity comes from the structure's own
graph.

## "Level" means four things; two of them survive

This is the collision that is *kept*, so it is worth stating plainly rather
than hoping nobody notices.

| where | what it means | verdict |
|---|---|---|
| `Geosphere` subdivision | how refined the mesh is | **renamed to `depth`** |
| `Facet` path length | how refined this address is | **renamed to `depth`** |
| `ChamberAddr.level` | a screen-filling map — *not a storey* | **kept**: a different and correctly named thing |
| `RegionAddr.level`, `RegionScene.level` | quadtree depth on a **cube** face, a different mesh entirely | **frozen**: it is a serialized field of `scene/tiles-region/v1`, a cross-repo contract |

The fourth is the one to be careful with. It reads like the first — both are
refinement depths — but it is refinement of a *different mesh*, and it is on
the wire, so it cannot be renamed even though renaming it would be tidier.

## The frozen spellings

**Every serialized spelling still says "room" and "cell", and always will.**
After the rename the code says `Facet` while the seed-derivation label says
`"room/face"`. That mismatch is correct and permanent.

The reason is not sentiment. A seed-derivation label is a save-format
contract: the world's names, terrain, settlements and history are all derived
from streams keyed by these strings, so changing one silently produces a
different world from the same seed — every committed almanac, map and census
diverges at once, with nothing crashing. The same argument covers the epoch
keys, the wire schemas that clients read, and the census columns the
calibration batteries assert against.

So the rule has two halves:

- **A constant's NAME may be renamed. Its string VALUE may not.**
- **For a serde field the name *is* the value**, so rename the field and add
  `#[serde(rename = "<old wire name>")]`. Do it that way round rather than
  leaving the field spelled `cell`: an attribute is a visible tripwire, and a
  silently-old field name is exactly what the next sweep renames without
  noticing.

What is frozen:

| spelling | where | why |
|---|---|---|
| `room/face`, `room/child` | kernel stream labels | seed derivation |
| `room/furnishing/v1`, `room/chambers/v1`, `room/chambers/built/v1`, `room/layout/v1/*` | vessel stream labels | seed derivation |
| `room/furnishing`, `room/layout/rectilinear` | epoch keys | save format |
| `cell-id` | a predicate name | the ledger |
| `room/` | the knowledge-key prefix | session snapshots clients read |
| `cell/{}`, `cell/{}/process/{}/block/{}` | ledger subject strings | in every committed world |
| `room`, `cell`, `cells` | serialized JSON keys | `scene/surrounds/v2`, a cross-repo contract |
| `per-cell-diversity`, `cold-built-room-share` | census golden columns | the calibration batteries assert against them |

The knowledge-key prefix deserves a note, because the coupling around it is
subtler than it looks. Three sites share the string `"room/"` — one writes it,
one strips it to recover every facet a session has walked, and one
default-deny validator matches on it. Rename any one of them and tests go red.
Rename all three *consistently* — which is exactly what a tidy-minded rename
campaign does — and only two tests object, both byte-goldens whose documented
remedy is a single rebaseline command. So the failure mode is not a silent
break; it is a red that one command turns green while a changed wire format
ships. It is pinned now by a test that asserts the literal directly and
therefore cannot be rebaselined.

## Why the archive still says "room" and "cell"

The decision records, the retrospectives and the chronicle contain thousands
of uses of the old words in their old senses. **They are not swept, and must
not be.** Each is a record of what was true when it was written; editing them
would make the archive lie about its own history. This page is how a reader
reconciles them: when a 2026-07 chronicle entry says "cell", it means what is
now called a vertex.

## "Room" is retired as a word

Once `RoomAddr` became `Facet`, the obvious next move is to promote `Chamber`
to `Room` — the word is free now, and a chamber *is* a room in the ordinary
sense.

Do not. Every historical reference in this repository uses "room" for the old,
much broader meaning, which ran from a continent to a floor tile. Re-pointing
the word at a narrower thing would silently change what the entire archive
appears to say. The word is spent; leave it spent.
