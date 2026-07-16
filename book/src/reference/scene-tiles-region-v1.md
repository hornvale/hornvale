# Scene Schema: tiles-region v1

`scene/tiles/v1` describes the whole world at one fixed resolution: an
equirectangular lattice sized in advance and sampled once, everywhere. It
cannot get closer — asking for more tiles just re-samples the same coarse
lattice at a finer angular spacing. `scene/tiles-region/v1` answers a
different question: given one cube-sphere quadtree tile — a single face,
subdivided to some depth, at some column and row — what does *that tile's
footprint* look like sampled at a higher on-tile density than the tile is
wide? It is a **new schema alongside** `scene/tiles/v1`, not a change to
it: the addressing, the grid formula, and the per-node (not per-global-tile)
layers are a different document shape, so they get a different name. It
reuses `scene/tiles/v1`'s per-layer semantics by reference rather than
redefining them.

## The address and the node grid

A regional tile is addressed by five integers: `(face, level, ix, iy,
samples)`.

- `face` is one of the six cube faces, `0..6`.
- `level` is the quadtree depth, `0..=24` — at `level`, a face is divided
  into `2^level × 2^level` tiles.
- `ix`, `iy` are the tile's column and row on the face, each `0..2^level`.
- `samples` (N) is the number of sample quads per tile edge, `1..=256`,
  yielding an **(N+1)×(N+1)** grid of nodes — N+1 nodes span N quads,
  same as sampling N+1 fenceposts for N fence sections.

The node grid is row-major, **row outer, column inner**: node index
`i = row·(N+1) + col`, with `row, col ∈ 0..=N`. Row runs in the tile's
`iy` direction, column in its `ix` direction.

### The normative projection

Every node's position on the sphere comes from one projection, and it is
normative: the Rust producer, this page, and the client's own projection
code all run the identical arithmetic (the same discipline The Isotherm
established for its evaluator — one formula, restated in three homes, never
three interpretations of one idea).

Each cube face has a basis triple `(n, u, v)` — a face normal and two
in-face axes:

```
FACES[6] = { n, u, v } basis triples:
  0: n=( 1, 0,0) u=(0, 1,0) v=(0,0,1)
  1: n=(-1, 0,0) u=(0,-1,0) v=(0,0,1)
  2: n=( 0, 1,0) u=(-1,0,0) v=(0,0,1)
  3: n=( 0,-1,0) u=( 1,0,0) v=(0,0,1)
  4: n=( 0, 0,1) u=( 1,0,0) v=(0,1,0)
  5: n=( 0,0,-1) u=(-1,0,0) v=(0,1,0)

param(index, offset, level) = -1 + 2·(index + offset) / 2^level
a = param(ix, col/N, level)          # ∈ [-1, 1]
b = param(iy, row/N, level)
faceUnit(face, a, b) = normalize( n + a·u + b·v )   # the node's unit vector s
```

`param` maps a tile's integer position plus a fractional in-tile offset
(`col/N` or `row/N`) onto `[-1, 1]` across the whole face; `faceUnit` then
projects the resulting `(a, b)` planar point outward onto the unit sphere
through the face's basis. The result, `s`, is the node's unit position —
the single source of truth for everything sampled at that node.

A node's geographic coordinate — latitude `asin(s_z)`, longitude
`atan2(s_y, s_x)`, in degrees — is **derived**, never primary. **Sampling
uses the unit vector `s` directly**, not a lat/lon round-trip: the producer
looks up the nearest geosphere cell to `s` itself. `s` already *is* the
direction the cell lookup needs, so converting it to a latitude/longitude
and letting the index convert that back to a direction would be redundant,
lossy work for no gain. (Cross-platform byte-identity is not at stake either
way: every transcendental in the sim, including the cell index's own, routes
through the pure-Rust libm crate that makes them byte-identical across
machines — decision 0041 — so the choice here is about avoiding a wasted
round-trip, not about closing a platform gap.)

The document does **not** carry per-node lat/lon or positions — a client
reconstructs node geometry from the address via this same projection.
(Shipping a `positions` array is a natural additive extension if a
consumer ever needs it without recomputing the projection; it is not part
of this schema today.)

## Sampling: nearest-cell discrete, barycentric continuous

For each node's unit vector `s`, the producer locates the nearest geosphere
cell and its local neighborhood, then samples each layer according to its
kind:

| Layer | Sampling | Why |
|---|---|---|
| `ocean`, `biome`, `plate` | nearest-cell | categorical — there is no such thing as interpolating a category |
| `elevation_m`, `unrest`, `t_mean_c`, `t_swing_c`, `moisture` | barycentric over the local cell triangle | continuous — a smooth surface across the tile is more honest than a stair-step of cell values |

Barycentric interpolation is entirely **producer-internal**: the document
ships the already-interpolated arrays, and a client never reimplements the
interpolation itself (contrast the temperature *evaluator* below, which a
client does reconstruct, over already-interpolated inputs). The weights
come from the neighboring cells' full-precision, libm-routed center
positions and the sample position — plain deterministic arithmetic,
quantized only at the document's serialization boundary (decision 0033).
The producer's algorithm — nearest cell plus its geosphere neighbors,
forming the containing spherical triangle, then planar barycentric weights
in the tangent plane — is an implementation detail; the **result** is the
contract, pinned byte-for-byte by the committed golden example below. Same
address against the same world always yields a byte-identical document.

## The document

Every `scene/tiles-region/v1` document is one JSON object with these
fields, in this order (field order is part of the contract):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/tiles-region/v1"`. |
| `seed` | integer | The world's seed (u64; JavaScript's plain `JSON.parse` loses precision above 2^53 — use BigInt-aware parsing when the exact seed matters). |
| `face` | integer | The cube face, `0..6`. |
| `level` | integer | The quadtree depth. |
| `ix` | integer | The tile's column at `level`. |
| `iy` | integer | The tile's row at `level`. |
| `samples` | integer | N; the node grid is `(N+1)×(N+1)`, row-major as described above. |
| `sea_level_m` | number | Sea level, meters — document-level, same scale as `elevation_m`. |
| `season_period_days` | number | The seasonal sinusoid's period, standard days — document-level, self-contained exactly as in v1. |
| `circulation_bands` | integer, **absent when tidally locked** | Circulation cells per hemisphere — same document-level field and absence convention as v1. |
| `biome_legend` | array of string | The biome catalog, in v1's stable append-only order; `biome` indexes into it. |
| `elevation_m` | array of number, `(N+1)²` | Elevation per node, meters — **barycentric**. |
| `ocean` | array of boolean, `(N+1)²` | Ocean flag per node — **nearest-cell**, the categorical coastline truth. |
| `biome` | array of integer, `(N+1)²` | Biome index per node, into `biome_legend` — **nearest-cell**. |
| `plate` | array of integer, `(N+1)²` | Tectonic plate id per node — **nearest-cell**. |
| `unrest` | array of number, `(N+1)²` | Tectonic unrest per node, [0, 1] — **barycentric**. |
| `t_mean_c` | array of number, `(N+1)²` | Annual-mean temperature per node, °C — **barycentric**. |
| `t_swing_c` | array of number, `(N+1)²` | Hemisphere-signed seasonal half-swing per node, °C — **barycentric**. |
| `moisture` | array of number, `(N+1)²` | Dimensionless moisture index per node, [0, 1] — **barycentric**. |

`features` are omitted from this schema: a regional tile is a footprint of
the surface, not a settlement census, and a settlement's global lat/lon
placement does not need per-node interpolation to be reported.

Every per-layer *meaning* — units, sign conventions, the biome legend's
order, moisture being a dimensionless index rather than mm/yr — is exactly
what it is in [`scene/tiles/v1`](./scene-tiles-v1.md); this page does not
restate it. Consult that page's field table for what each layer means; this
page only says how the value at a *node* is derived (address, projection,
and the nearest-cell/barycentric split above) and in what order the fields
appear on the wire.

## Reading temperature over the year

A client reconstructs a node's temperature at any day with the **same
evaluator as v1**, applied to the regional arrays instead of the global
ones:

```
t(node i, day) = t_mean_c[i] + t_swing_c[i] · sin(τ · frac(day / season_period_days))
```

where `day` is absolute standard days (`WorldTime`), `τ = 2π`, and
`frac(x) = x − floor(x)`. The phase gotcha carries over verbatim from v1:
this is `frac(day / season_period_days)`, and it is **never** offset by
`scene/system/v1`'s `year_phase_offset` — that offset is orbital geometry
(where the world sits on its orbit at day 0), while the seasonal sinusoid
is a separate clock that starts at zero phase at day 0 by construction.

### Interpolation commutes with the evaluator

This reconstruction is exact for the interpolated layers, and the reason
is structural, not coincidental: the seasonal period is **global** — every
cell shares the same `season_period_days`, so the phase angle
`θ = τ·frac(day/period)` is a single scalar shared by every cell in the
tile, not something that varies per cell. Because `temperature_at(cell,
day) = mean(cell) + swing(cell)·sin(θ)` is affine in `mean(cell)` and
`swing(cell)` at a fixed `θ`, barycentric-interpolating the *actual
per-cell temperature* over a triangle gives the same result as evaluating
the formula above on the *already-interpolated* `t_mean_c`/`t_swing_c`.
Interpolation and the seasonal evaluator commute — the order they're
applied in doesn't matter. This is what lets the producer ship only two
interpolated layers instead of a temperature-per-day array, and it is
stated normatively here so no consumer invents a different evaluator for
the regional shape, or wonders whether interpolating pre- or
post-evaluation gives a different answer. (They agree in real arithmetic;
producer-side floating-point rounding differs by tight-approximate
tolerance between the two orders of operations, never by more.)

Ice remains a client derivation exactly as in v1 — a coldest-day threshold
read off `t_mean_c`/`t_swing_c` — now smooth across the tile because the
two source layers are themselves interpolated.

## Honesty caveats

- **The resolution floor is about 110 km** — the geosphere's cell spacing.
  Sampling a tile deeper than that spacing does not add new physical
  detail: the query still draws on the same underlying cell samples, and
  interpolation only smooths *between* them. A tile requested well below
  the floor is honest — it is not wrong — but it carries no information a
  coarser tile lacked. Depth is a rendering choice, not a fidelity
  promise.
- **Interpolated elevation can disagree with the nearest-cell coastline.**
  `ocean` is the categorical truth for a node — it says which side of sea
  level the *nearest cell* falls on. `elevation_m` is smoothed relief, and
  near a coast a barycentrically interpolated value can cross
  `sea_level_m` on the opposite side of where `ocean` says the shoreline
  is. A client that wants a crisp, unambiguous shoreline reads `ocean`; one
  that wants smooth bathymetry for shading reads `elevation_m`. The two are
  not silently reconciled — a client using both should expect them to
  disagree at the pixel scale near a coast, by design.

## Stability

`scene/tiles-region/v1` is a save-format-class contract, held to the same
discipline as `scene/tiles/v1` and the rest of the world's on-disk formats:

- Adding a new field stays within `scene/tiles-region/v1` — existing
  consumers that read fields by name are unaffected.
- New fields always append **after** every existing field. Wire order is
  historical accretion order, not a semantic grouping; the field *names*
  are the compatibility contract, not their position on the wire.
- Changing an existing field's meaning, order, or type never happens in
  place. That mints `scene/tiles-region/v2` as a new schema alongside
  `v1`, which keeps emitting exactly what it always emitted.
- The committed example, `gallery/scene-tiles-region-seed-42.json`, is
  regenerated from seed 42 at a fixed address by
  `scripts/regenerate-artifacts.sh`; drift from the committed file shows up
  in the author's `git status` on regeneration. Like its sibling
  `scene-tiles-seed-42.json`, it is **excluded from CI's cross-platform
  strict diff**: its nearest-cell `biome`/`ocean`/`plate` classifications
  ride worldgen values whose last-ULP results are host-libm-local, so
  byte-identity holds per-platform but not necessarily between the CI runner
  and a committed macOS file. Same-platform determinism (same machine, same
  address → byte-identical) is enforced instead by the producer's unit
  tests.

The committed example is generated at address `face = 0, level = 3, ix =
4, iy = 4, samples = 16` against the seed-42 sky world — a 17×17 node grid
(289 nodes).

## Getting one

```
hornvale scene tiles-region --face <n> --level <n> --ix <n> --iy <n> --samples <n> [--world <path>]
```

This prints one `scene/tiles-region/v1` document to standard output.
`--world` defaults to `world.json`. An address outside its bounds — a bad
face, a level, ix, iy, or samples out of range for the schema's limits
above — is refused with a description of which bound it violates, the same
loud-failure discipline as every other pinned generator in this project.
