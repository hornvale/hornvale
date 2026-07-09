# Scene Schema: tiles v1

A scene is the boundary between the simulation and whatever draws it. The
simulation emits deterministic, semantic data — raw quantities, catalog
names, named points — and never touches a pixel; a client turns that data
into a picture however it likes. A scene document says *what an observer
could see*, not *how to render it*: two clients can paint the same scene as
a parchment map and a heightfield mesh and both be right, because neither
drew anything the simulation didn't already say.

`scene/tiles/v1` is the cartographic scene: a world's surface resampled onto
an evenly spaced longitude/latitude lattice, with a value at every tile for
elevation, ocean coverage, biome, tectonic plate, and tectonic unrest, plus
the named settlements sitting on top of it.

## The document

Every `scene/tiles/v1` document is one JSON object with these fields, in
this order (field order is part of the contract — it is fixed, not
incidental):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/tiles/v1"` — the version tag a consumer checks before trusting the rest of the document. |
| `seed` | integer | The world's seed. |
| `width` | integer | Lattice width in tiles. |
| `height` | integer | Lattice height in tiles — always `width / 2`. |
| `sea_level_m` | number | Sea level, in meters, on the same scale as `elevation_m`. |
| `elevation_m` | array of number | Elevation in meters, one entry per tile. |
| `ocean` | array of boolean | Whether the tile is ocean — `true` where elevation is at or below sea level — one entry per tile. |
| `biome` | array of integer | The tile's biome, as an index into `biome_legend`, one entry per tile. |
| `biome_legend` | array of string | The full biome catalog, in stable order — `biome`'s values index into this array. |
| `plate` | array of integer | The tectonic plate id underlying the tile, one entry per tile. |
| `unrest` | array of number | Tectonic unrest at the tile, one entry per tile. |
| `features` | array of object | Named points on the lattice — settlements, described below. |

`elevation_m`, `ocean`, `biome`, `plate`, and `unrest` are the five per-tile
layers: each is an array of exactly `width × height` entries, in the grid
order described below, so tile `i`'s elevation, ocean flag, biome, plate,
and unrest all sit at the same index `i` across their respective arrays.
`biome_legend` currently holds twenty-two entries, from `ice` and `tundra`
through the deep-ocean biomes (`hadal-trench`, `abyssal`, and the rest);
its order is append-only and stable across worlds, so a biome index means
the same thing in every `scene/tiles/v1` document ever produced.

Each entry in `features` is:

| Field | Type | Meaning |
|---|---|---|
| `name` | string | The feature's canonical name. |
| `kind` | string | Either `"settlement"` or `"flagship"`. |
| `latitude` | number | Degrees north. |
| `longitude` | number | Degrees east. |

`features` lists every settlement the world has placed. Exactly one of
them — the world's capital, the single highest-suitability site any species
placed — carries `kind: "flagship"` instead of `kind: "settlement"`, and it
always appears last in the array. A flagship never also appears as a
plain settlement; the same named place is never listed twice.

An excerpt of the committed seed-42 example (arrays elided; see
`gallery/scene-tiles-seed-42.json` for the full document):

```json
{
  "schema": "scene/tiles/v1",
  "seed": 42,
  "width": 256,
  "height": 128,
  "sea_level_m": 409.0054089516639,
  "elevation_m": [ ... 32768 entries ... ],
  "ocean": [ ... 32768 entries ... ],
  "biome": [ ... 32768 entries ... ],
  "biome_legend": ["ice", "tundra", "taiga", ..., "abyssal"],
  "plate": [ ... 32768 entries ... ],
  "unrest": [ ... 32768 entries ... ],
  "features": [
    { "name": "Ngakngek", "kind": "settlement", "latitude": 22.21586468804764, "longitude": -10.214355301392398 },
    ...,
    { "name": "Fnabnget", "kind": "flagship", "latitude": -23.502371327110584, "longitude": 146.0873629471159 }
  ]
}
```

## The grid

The per-tile layers are row-major: the top row comes first, and within a
row, tiles run west to east. Each tile is a pixel center, not a grid
intersection — tile `(px, py)` in a `width × height` lattice sits at:

- **latitude** 90° minus `(py + 0.5) / height × 180°` — the top row centers
  just under the north pole, the bottom row just above the south pole, and
  latitude decreases moving down the array.
- **longitude** `(px + 0.5) / width × 360° − 180°` — the leftmost column
  centers just east of the antimeridian, and longitude increases moving
  across a row.

Height is never given independently — it is always `width / 2`, which keeps
each tile's angular footprint square regardless of resolution.

## Stability

`scene/tiles/v1` is a save-format-class contract, held to the same
discipline as the rest of the world's on-disk formats:

- `biome_legend`'s order is append-only: a biome already in the catalog
  keeps its index forever, so a `biome` value from an old document still
  means the same thing against a newer legend.
- Adding a new field, or a new entry to `biome_legend` or `features`,
  stays within `scene/tiles/v1` — existing consumers that read fields by
  name are unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place. That mints `scene/tiles/v2` as a new schema alongside `v1`, which
  keeps emitting exactly what it always emitted.
- The committed example, `gallery/scene-tiles-seed-42.json`, is regenerated
  from seed 42 and checked byte-for-byte by CI; if the generator's output
  ever drifts from the committed file, the build fails rather than let the
  schema silently change underneath its own example.

## Getting one

```
hornvale scene tiles [--world <path>] [--width <n>]
```

This prints one `scene/tiles/v1` document to standard output. `--world`
defaults to `world.json`; `--width` defaults to 256. Width must be even —
height is always `width / 2` — and must fall between 16 and 1024 inclusive;
anything else is refused with a description of which bound it violates.
