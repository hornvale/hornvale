# Scene Schema: surrounds v2

`scene/tiles/v1` and `scene/tiles-region/v1` both answer a *cartographic*
question: given a patch of the globe, what is there? They are exocentric —
they describe ground, and nobody is standing on it. `scene/surrounds/v2`
answers the *situated* question instead: **an observer is standing in a
particular room; what lies around them, and how much of it do they know?**

That difference is not a matter of zoom. The cartographic schemas address
the world through a cube-sphere quadtree, a grid laid over the planet for
the convenience of sampling. A situated observer does not walk that grid.
They walk the **room mesh** — the triangular faces of a subdivided
icosahedron, the same structure `locale/room/v2` addresses, where every step
is an edge crossing to one of exactly three neighbours. A chart whose cells
were quadtree tiles would show squares to someone moving between triangles:
the picture would disagree with the walk. So this is a **new schema
alongside** the cartographic ones, not a variant of them — a different
address space, a different centre, and a notion of *epistemic state* that
the cartographic poles have no use for.

## The address and the neighbourhood

A surrounds document is centred on one room and extends outward by
breadth-first search over the mesh's edge adjacency:

- `observer` names the centre — its packed `room` id, its base icosahedron
  `face`, its refinement `depth`, and its centroid `latitude`/`longitude`.
- `radius` is the number of BFS rings, `0..=8`.
- `depth` is the refinement depth every cell sits at.

Because each triangle has exactly three edge-neighbours, a ring-`k`
neighbourhood holds

```
1 + 3·k·(k+1)/2   cells:   1, 4, 10, 19, 31, 46, 64, 85, 109
```

The default radius of 4 is therefore 31 cells.

## Placement: integer lattice, not compass bearing

Every cell carries `u`, `v`, `w` and `up`: its position in the observer's
own base-face triangular lattice, **relative to the observer**, together
with the triangle's orientation. These are exact integers.

They have to be. The obvious alternative — placing each cell at the compass
bearing of its exit — fails on a sphere. A room's three exits bucket to
three distinct compass points everywhere, but *which* three depends on
latitude and on the local orientation of the icosahedral face: an observer
near the equator may read `E, Nw, Sw` while one at 85° N reads `E, N, S`.
The lattice's rotation relative to north drifts across the globe, so laying
cells out by bearing would distort the very mesh the chart claims to depict,
and would do it differently in every part of the world.

Integer barycentric coordinates have no such drift. A triangle's *lattice
base point* is the componentwise minimum of its three barycentric corners;
an up-pointing triangle's base sums to `scale − 1` and a down-pointing one's
to `scale − 2`, where `scale = 2^depth`. Edge adjacency falls straight out
of that: an up triangle's three neighbours are the down triangles whose base
is its own with exactly one axis decremented, and a down triangle's are the
up triangles with exactly one axis incremented. No transcendental function
touches placement, so a chart drawn from these coordinates is byte-identical
on every platform.

### The normative screen projection

A renderer that wants a flat picture maps each cell to a character grid by:

```
row        = -w
screen_col = 2·v + (up ? 0 : 1) + w
```

Rows increase downward. Within one row, consecutive `screen_col` values are
edge-adjacent triangles alternating up and down — which is why one glyph per
cell reads as a triangular strip.

The `+ w` term is the part worth explaining. Without it, a cell's
across-the-horizontal-edge neighbour — the one that ought to sit *directly
below* it — lands one column to the right as well as one row down, because
the lattice's rows are themselves offset. Accumulated over a neighbourhood
that shear turns a symmetric hexagonal ball into a leaning parallelogram.
Subtracting the row index cancels it exactly: the cell sits at
`col − row`, its below-neighbour at `(col + 1) − (row + 1)`, the same
column. Same-row neighbours shift by the same amount and stay adjacent.

The mapping is injective, so no two cells can collide on one glyph: for a
fixed row, `2·v + (up ? 0 : 1)` is the even/odd decomposition of an integer
into `(v, up)`, and distinct rows are distinct `w`.

## Seam cells: real ground with no honest place

The lattice is face-local. Two rooms on *different* base icosahedron faces
have no meaningful relative offset — the surface genuinely bends between
them, and no flat coordinate can say by how much without inventing one.

A neighbourhood that reaches across a base-face edge therefore emits those
cells with `seam: true` and `u`, `v`, `w`, `up` all `null`. They are not
dropped: their room id, epistemic state and semantic layers are all present,
and a consumer that works in room ids rather than pictures loses nothing. It
is only the *drawing* that cannot place them, and a renderer is expected to
say so in its caption rather than quietly omit them or fabricate a position.

This is rare — a base face at depth 12 carries 4¹² ≈ 16.7 million triangles
and only the outermost few rings of each are within reach of an edge — which
is precisely why the schema makes it explicit rather than leaving it to be
discovered.

## Epistemic state, and the grain that follows from it

Each cell carries a `state`, and the state governs how much detail the cell
carries. This is the schema's other departure from the cartographic poles: a
tile lattice describes ground uniformly, because the ground does not care
who is looking. A situated document does not, because the observer's
knowledge is part of what is being described.

| `state` | What it means | Layers carried |
|---|---|---|
| `here` | the observer's own room | everything: `biome`, `water`, `relief`, plus `regime`, `temperature_c`, `moisture`, `elevation_m` |
| `sensed` | within the neighbourhood, seen from where the observer stands | the coarse triple only: `biome`, `water`, `relief`; the fine fields are `null` |
| `remembered` | known from having been there, no longer in view | as recorded when it was visited |

A producer with no session — today, only the CLI's `hornvale scene
surrounds` — emits only `here` and `sensed`; it has no basis for
remembering anything. `remembered` is written by a consumer that owns a
possession and therefore knows where its observer has walked. The schema
carries the vocabulary so that both producers speak it, not because every
producer can use all of it. (Unlike `scene/tiles/v1` and
`scene/tiles-region/v1`, `clients/world-wasm` does not export a surrounds
query today. Its scene exports are the four astronomical documents
(`hw_scene_system`, `hw_scene_moons`, `hw_scene_neighbors`,
`hw_scene_eclipses`) and the three terrain-facing ones (`hw_scene_tiles`,
`hw_scene_tiles_selected`, `hw_scene_tiles_region`); none of them is a
surrounds query. A browser client that wants a session-owning `remembered`
overlay would need a new wasm export; the schema's vocabulary is ready for
that even though no producer offers it yet.)

The coarse triple is deliberately what can be *seen across open ground*
rather than what can be measured by standing on it. Reading a room's exact
moisture from four rooms away would be a claim about perception that the
model does not support.

## Marks and the legend

`marks` on a cell are the salience-ranked things standing there — a
settlement today, and whatever a session-owning consumer adds. Each mark
carries a `noun`, a `kind`, a one-line `datum`, and a `salience` rank in
which **lower is more salient**. Marks within a cell are ordered by
`(salience, noun)`.

`legend` is the document's noun catalog: `(noun, datum)` pairs covering
every mark and every terrain class the document surfaced, ordered by `noun`.
It is deliberately the same shape as the noun catalog the prose renderer
produces, so a consumer can offer one vocabulary over both — the map and the
prose being two grains of one lens rather than two descriptions that happen
to agree. A biome's `legend` noun is therefore its **spaced prose name**
(`tropical seasonal forest`), the same noun the prose renderer uses for it —
not the kebab-case identifier `biome_legend` indexes into
(`tropical-seasonal-forest`). `legend` is player-facing text; `biome_legend`
is the machine-readable index catalog. Using the identifier in `legend`
would give a player two different examinable nouns for one biome, one per
grain, defeating the point of a shared catalog (The Margin).

## Legends and ordering

Three catalogs make the document self-describing: `biome_legend` (the biome
catalog in its stable append-only order), `water_legend`, and
`relief_legend`. A cell's `biome`, `water` and `relief` are indices into
them. `relief_legend` is `abyss, shelf, lowland, upland, highland, alpine`,
and its band boundaries are contract:

| `relief_legend` index | Name | Height above sea level (m) |
|---|---|---|
| 0 | `abyss` | < −3000 |
| 1 | `shelf` | −3000 .. 0 |
| 2 | `lowland` | 0 .. 300 |
| 3 | `upland` | 300 .. 1000 |
| 4 | `highland` | 1000 .. 2500 |
| 5 | `alpine` | ≥ 2500 |

Each band is half-open, `[lower, upper)`, against `height_asl_m` — **not**
against `elevation_m`, which is an absolute reading on the planet-independent
isostatic datum whose zero is nowhere near any particular world's sea level.
Banding the absolute reading is what v1 did, and on a world whose sea level
sits near −2936 m it classified almost all land as `shelf`. Changing a
boundary, or the quantity they are measured against, mints
`scene/surrounds/v3`.

`cells` is ordered by ascending packed `room` id — a total order over `u64`
that needs no float comparison and cannot vary between runs.

`orientation` is always the string `"lattice"`. It is present to state
plainly that the chart is lattice-aligned and **not** north-up. A document
that wanted to claim north would have to carry a bearing, and a bearing is
exactly the drifting quantity this schema was built to avoid; a consumer
that needs north can ask the rooms for it.

## Scale is stated in arc, never in metres

The simulation defines no planetary radius. The mesh lives on the unit
sphere, and elevation is the only metric length anywhere in the model. A
depth-12 room's edge subtends roughly 0.015° of arc, and a radius-4
neighbourhood spans roughly 0.07° — about one five-thousandth of the globe.

Any figure in metres would be an illustration conditioned on an assumed
planet size rather than a fact the model holds. Neither this schema nor a
renderer's caption asserts metres per cell.

## Colour, and the eye that computed it

Colour is not a property of a cell. It is the three-way product of a
material's reflectance, the light falling on it, and the sensitivity curves
of whoever is looking — so a colour on the wire is meaningless unless the
document also says *which eye*. Two fields carry that, and both are optional:

- each cell may carry `color`, an `[r, g, b]` triple of bytes;
- the document may carry `sight`, the declaration of the eye and the
  projection those triples came out of.

**An uncoloured document emits neither key at all.** Both are
`skip_serializing_if = "Option::is_none"`, and `sight` was *appended* after
`legend` rather than inserted, so a document produced without a colour layer
is byte-for-byte what it was before this layer existed. That is why colour
did not mint `scene/surrounds/v3`: no observable value moved, and a consumer
that has never heard of `color` reads exactly the bytes it read before. The
committed example, [`scene-surrounds-seed-42.json`](../gallery/scene-surrounds-seed-42.json),
is produced through the uncoloured path and carries neither key.

`sight` is one object with six fields:

| Field | Type | Meaning |
|---|---|---|
| `observer` | string | Whose eyes: a species' `KindId` label (`"bugbear"`), or `"standard"`. **Caller-supplied.** |
| `channels` | integer | How many channels that eye senses with. |
| `chromatic` | integer | How many of those channels carry hue; the rest are achromatic and contribute brightness only. |
| `projection` | string | The registered name of the mapping from signal to sRGB — `"native"`, `"native-anomalous"`, `"yellow-blue"`, or `"none"` when the eye carries no projection. |
| `preserves` | string | What that projection keeps, in words. The caption's load-bearing half. |
| `sun_altitude_deg` | number | The sun's elevation above the horizon, degrees, that lit these colours; quantized at the emit boundary. **Caller-supplied.** |

**Four of the six are overwritten by the builder, and that is why they can
be trusted.** `channels`, `chromatic`, `projection` and `preserves` are read
back off the `Observer` actually used to colour the chart, discarding
whatever the caller put in those slots. A caller can therefore name an eye
and state a sun angle — the two things an `Observer` cannot supply, since a
set of sensitivity curves does not know its own species or what time it is —
but a caller **cannot** make a document claim an arity or a projection its
colours did not actually come from. A consumer may read those four as fact
about the pixels; it must read `observer` and `sun_altitude_deg` as the
producer's assertion.

The projection is named for the reason a map projection is named. Every
projection of a signal onto three screen channels loses something, and the
honest response is not to search for a lossless one but to say which
invariant survives. A two-chromatic-channel eye rendered through
`yellow-blue` emits triples whose red and green components are **equal by
construction** — that is not an artifact to be smoothed away, it is what a
colour space with no red–green axis honestly looks like on a three-channel
screen, and `preserves` says so in the same breath: *"the short-to-long
opposition; the red–green axis is not carried."*

A renderer is expected to surface `preserves` beside the picture rather than
in a footnote. `render_surrounds_ascii`'s `colour` lens does exactly that,
and adds its own disclosure: the tint is **bedrock**, so it is applied only
where the glyph is drawing that ground and withheld from water, from marks,
and from the observer's own cell — with three counts that partition the
chart, so a reader can check the sentence against the picture.

## The document

Every `scene/surrounds/v2` document is one JSON object with these fields,
in this order (field order **is** the JSON key order and is contract):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/surrounds/v2"`. |
| `seed` | integer | The world's seed (u64; JavaScript's plain `JSON.parse` loses precision above 2^53 — use BigInt-aware parsing when the exact seed matters). |
| `day` | number | The day observed (`WorldTime`), quantized at the emit boundary. |
| `observer` | object | Where the observer stands — see the table below. |
| `radius` | integer | Neighbourhood radius, in BFS rings, `0..=8`. |
| `depth` | integer | The refinement depth every cell sits at. |
| `orientation` | string | Always the literal `"lattice"` — the chart is lattice-aligned, never north-up. |
| `biome_legend` | array of string | The biome catalog, stable append-only order; a cell's `biome` indexes into it. |
| `water_legend` | array of string | The water catalog, stable order; a cell's `water` indexes into it. |
| `relief_legend` | array of string | `["abyss", "shelf", "lowland", "upland", "highland", "alpine"]`; a cell's `relief` indexes into it. |
| `sea_level_m` | number | This world's derived sea level, metres on the isostatic datum, quantized. The bands in `relief_legend` are measured from it, so a consumer can re-derive any cell's band from `height_asl_m` alone. |
| `cells` | array of object | The neighbourhood, ascending by packed `room` id — see the cell table below. |
| `legend` | array of object | The chart's noun catalog, ascending by `noun` — see the `LegendEntry` table below. |
| `sight` | object, **key omitted when absent** | The eye this chart was coloured for and what its projection preserves — see "Colour, and the eye that computed it" above. Present only on a document built through the colouring path. |

`observer` is itself an object, in this field order:

| Field | Type | Meaning |
|---|---|---|
| `room` | integer | The observer's packed room id (u64 — see the room-id precision note below). |
| `face` | integer | Base icosahedron face, `0..20`. |
| `depth` | integer | Refinement depth. |
| `latitude` | number | Centroid latitude, degrees, quantized. |
| `longitude` | number | Centroid longitude, degrees, quantized. |

Each element of `cells` is an object, in this field order:

| Field | Type | Meaning |
|---|---|---|
| `room` | integer | Packed room id (u64 — see the precision note below). |
| `u` | integer or null | Lattice offset from the observer on axis 0; `null` on a seam cell. |
| `v` | integer or null | Lattice offset on axis 1; `null` on a seam cell. |
| `w` | integer or null | Lattice offset on axis 2; `null` on a seam cell. |
| `up` | boolean or null | Triangle orientation; `null` on a seam cell. |
| `seam` | boolean | Set when this cell lies on a different base face than the observer. |
| `state` | string | `"here"`, `"sensed"`, or (session-written only) `"remembered"`. |
| `biome` | integer | Index into `biome_legend`. |
| `water` | integer | Index into `water_legend`. |
| `relief` | integer | Index into `relief_legend`. |
| `regime` | string or null | The strangeness overlay's descriptor; `null` when the cell is not `"here"`. |
| `temperature_c` | number or null | Annual-mean temperature, °C, quantized; `null` when the cell is not `"here"`. |
| `moisture` | number or null | Dimensionless moisture index, quantized; `null` when the cell is not `"here"`. |
| `elevation_m` | number or null | Elevation, metres, quantized; `null` when the cell is not `"here"`. |
| `height_asl_m` | number or null | Height above sea level, metres, quantized; signed, negative below; `null` when the cell is not `"here"`. `relief` is banded from this. |
| `color` | array of 3 integers, **key omitted when absent** | The cell's bedrock as it appears to the document's declared eye under the document's declared light — `[r, g, b]`, each `0..=255`. Absent entirely on an uncoloured document. |
| `marks` | array of object | Salience-ranked things standing here, ordered by `(salience, noun)` — see the `Mark` table below. |

Each element of a cell's `marks` (`Mark`) is an object, in this field order:

| Field | Type | Meaning |
|---|---|---|
| `noun` | string | The examinable noun. |
| `kind` | string | `"settlement"` or `"agent"`. |
| `datum` | string | One line about it — what `examine` prints. |
| `salience` | integer | Rank key; lower is more salient. |

Each element of `legend` (`LegendEntry`) is an object, in this field order:

| Field | Type | Meaning |
|---|---|---|
| `noun` | string | The examinable noun. |
| `datum` | string | What `examine` prints for it. |

**Room ids are u64 and can exceed JavaScript's safe integer range.**
`observer.room` and every cell's `room` are packed room ids: a sentinel bit,
two bits per path element, and five bits for the face (see
`RoomAddr::pack` in `kernel/src/room.rs`). Past roughly depth 24 that packed
value exceeds 2^53, the largest integer a JavaScript `Number` can represent
exactly, so a browser client parsing this schema with plain `JSON.parse`
can silently corrupt a room id at deep radii — the same hazard the seed
field carries, and for the same reason. A future browser client (this
schema's stated audience, alongside the CLI) needs BigInt-aware parsing for
`seed` and every `room` field alike.

## Getting one

```
hornvale scene surrounds [--world <path>] [--room <ID> | --depth <D>] [--radius <N>] [--day <D>]
                          [--render json|ascii]
```

This prints one `scene/surrounds/v2` document to standard output. `--world`
defaults to `world.json`. `--room` and `--depth` are mutually exclusive: a
packed room id already carries its own depth baked into its path length, so
combining them is a hard error rather than a silent pick of one over the
other. With no `--room`, the chart centres on the flagship settlement's own
room at `--depth` (default: the walk depth, `globe_level + 6`) — the same
ground a possession starts on; with `--room`, it centres on that exact room
instead, at whatever depth it already carries. `--radius` defaults to 4 (31
cells). `--day` (default 0) selects which day's `here` cell to observe. The
committed example,
[`scene-surrounds-seed-42.json`](../gallery/scene-surrounds-seed-42.json),
is produced this way against the seed-42 sky world.

`--render` defaults to `json`, this schema. `--render ascii` renders the
same document through `hornvale_scene::render_surrounds_ascii`'s `terrain`
lens — the same renderer a possession's own `map` verb draws from, so the
CLI can produce the picture outside a session. The CLI stays on the
`terrain` lens and the uncoloured builder, which is why its committed
artifacts carry no `color` or `sight`; a possession defaults to the
`colour` lens through the possessed agent's own eyes, and its `eyes off`
returns it to exactly this output. The footer's `ways on:`
line is the observer room's own lateral exits (`ExitKind::Edge`), read from
`hornvale_locale` the same way `map` reads them for the walked room.
[The gallery page](../gallery/surrounds-seed-42.md) shows several observers
rendered this way.

## Determinism

Same world, same query, byte-identical document. Floats quantize at the
emit boundary only; ordering is by integer id throughout; no hash-ordered
container appears anywhere in the producer. The committed example,
[`scene-surrounds-seed-42.json`](../gallery/scene-surrounds-seed-42.json),
is regenerated and drift-checked in CI, and byte pins in the producer's own
test suite defend the field order — which, as in every scene schema, **is**
the JSON key order and is contract. A changed meaning mints
`scene/surrounds/v3` alongside this one; it is never renamed and never
reordered in place.

## v1 → v2: what changed and why

`scene/surrounds/v1` banded `relief` against the raw `elevation_m` reading —
an absolute isostatic elevation, not a height above sea level. On a world
whose sea level sits far from zero on that datum (seed 42's is
−2936.17 m), that put almost all land in `shelf`: 8162 of the world's
11,066 land cells, leaving exactly one cell `alpine`. v2 bands against the
new `height_asl_m` field instead, and adds `sea_level_m` to the document so
a consumer can re-derive any cell's band without a second query. Every
observable `relief` value moved, which is why this is a new schema version
rather than a silent correction (decision 0055's additive-or-versioned
rule) — no producer in this repository emits the v1 document any longer,
and `scene/surrounds/v1` was never part of `clients/world-wasm`'s catalog,
so no client outside this repository read the wrong values either.
