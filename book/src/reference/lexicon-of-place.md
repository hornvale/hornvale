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

## One sphere, two indexings

The world is an icosphere: an icosahedron subdivided, its vertices projected
onto the unit sphere. Two different things live on it, and confusing them is
the mistake this vocabulary exists to prevent.

| word | what it is | what you do with it |
|---|---|---|
| **Vertex** | a **point** — one vertex of the subdivided icosahedron | sample a field at it: elevation, temperature, moisture, biome |
| **Facet** | a **patch** — one triangular face, at a refinement depth | occupy it; stand in it; walk from it to a neighbour |

They are duals. A facet's corners are three vertices; a vertex is shared by
the facets that meet at it. Neither is more fundamental — a field is a
function on points, and a place is a region you can be inside, and the world
needs both.

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
| **`Stratum`** | `domains/climate` | Surface; Epipelagic…Hadal; Regolith…Underneath | where in *this column*, relative to its realm |
| **`Horizon`** | `domains/terrain` | Regolith, Cover, Basement, Roots, Underneath | which rock layer, geologically |

The distinctions that matter:

- **`Band` is a rung you descend to**; `Stratum` and `Horizon` are positions a
  column *has* whether or not anything is standing there. A cave chamber sits
  at a band. A fish sits in a stratum. A rock sample belongs to a horizon.
- **`Stratum` spans both media and `Horizon` does not.** A stratum is
  realm-relative, so it covers the water column (`Epipelagic` down to `Hadal`)
  and the rock column alike; its rock members deliberately *mirror*
  `Horizon`'s roster, name for name. That mirroring is a shared roster and
  never a shared derivation — climate may not import terrain.
- `Band` was consolidated into the kernel from three separate models — a rung
  enum in terrain, a mirrored zone enum in climate, and a bare rank on a
  chamber address — one day before this vocabulary was written. Renaming it
  again would relitigate a deliberate recent decision without new information.

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
triangle geometry means nothing and connectivity comes from the structure's
own graph.

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
| `room/furnishing/v1`, `room/chambers/v1`, `room/layout/v1/*` | vessel stream labels | seed derivation |
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
