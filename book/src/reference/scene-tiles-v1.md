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
| `seed` | integer | The world's seed. This is a u64; JavaScript consumers parsing the document with plain `JSON.parse` lose integer precision above 2^53, so use BigInt-aware parsing when the exact seed matters. |
| `width` | integer | Lattice width in tiles. |
| `height` | integer | Lattice height in tiles — always `width / 2`. |
| `sea_level_m` | number | Sea level, in meters, on the same scale as `elevation_m`. |
| `elevation_m` | array of number | Elevation in meters, one entry per tile. |
| `ocean` | array of boolean | Whether the tile is ocean — `true` where elevation is below sea level — one entry per tile. |
| `biome` | array of integer | The tile's biome, as an index into `biome_legend`, one entry per tile. |
| `biome_legend` | array of string | The full biome catalog, in stable order — `biome`'s values index into this array. |
| `plate` | array of integer | The tectonic plate id underlying the tile, one entry per tile. |
| `unrest` | array of number | Tectonic unrest at the tile, one entry per tile. Dimensionless, in [0, 1]. |
| `features` | array of object | Named points on the lattice — settlements, described below. |
| `t_mean_c` | array of number, one per tile | Annual-mean temperature at the tile, °C (the climate model's canonical unit; sampled through the same nearest-cell path as every other layer). |
| `t_swing_c` | array of number, one per tile | **Hemisphere-signed** seasonal half-swing, °C: the coefficient of the seasonal sinusoid, `amplitude × sign(source-cell latitude)`. Positive in the north, negative in the south, exactly `0` on tidally locked and zero-obliquity worlds. Signed at the source so clients never apply a hemisphere sign themselves — a tile near the equator may sample a cell on the other side of it, and the sign travels with the data. |
| `season_period_days` | number | The period, in standard days, of the seasonal sinusoid the temperature layers parameterize. On generated-sky worlds this equals `scene/system/v1`'s `year_days`; on constant-sun worlds (which have no system document) it is the tier-0 default year, and this field is the only honest way a client can know it. The evaluator reads one scalar from `scene/system/v1` besides this — `year_phase_offset` — to phase the season on the true orbit (see *Reading temperature over the year*); everything else it needs is here. |
| `circulation_bands` | integer, **absent when tidally locked** | The number of atmospheric circulation cells per hemisphere (Earth-like day → 3). Document-level, not per-tile: the wind model is a pure function of latitude and this count, and the contract does not pretend otherwise. Absence follows the `day_length_days` precedent in [`scene/system/v1`](./scene-system-v1.md). |
| `moisture` | array of number, one per tile | Dimensionless moisture index in [0, 1] — the climate model's own quantity (band base and ocean-proximity floor, dried by an upwind moisture-budget trace on spinning worlds; the substellar model on locked ones). Deliberately **not** mm/yr, and it stays that way: the physical precipitation total arrived as the *separate* `precip_mm_yr` field below (The Rains), never a re-meaning of this one — biomes still classify on this dimensionless index. |
| `precip_mm_yr` | array of number, one per tile | Annual precipitation, mm/yr — an Earth-ranged total mapped from `moisture` (desert `<250`, temperate `~500–1500`, rainforest `>2000`). A documented approximation for legibility (the precipitation lens), not a measured climatology. |
| `snow_fraction` | array of number, one per tile | Fraction of the year's precipitation falling as snow, in [0, 1] — a smooth logistic of the mean temperature across freezing (cold → 1, warm → 0). |
| `precip_regime` | array of integer, one per tile | The seasonal precipitation regime as a small integer: `0` uniform, `1` summer-max, `2` winter-max, `3` monsoon. A categorical label from circulation band, continentality, and hemisphere — **not** a time series (a name, not a curve), so it is independent of the mm total. |
| `cloud_fraction` | array of number, one per tile | Diagnostic cloud fraction in [0, 1] — moisture × uplift, cloudy where moist air rises. Strictly diagnostic (no insolation or albedo feedback); the client shades and may advect it. |
| `weather_propensity` | array of number, one per tile | Climatological storm propensity in [0, 1] (The Firmament) — the slow prior a client animates typed clouds from; see [`hornvale_climate::GeneratedClimate::storm_propensity_at`]. |
| `cloud_type` | array of integer, one per tile | The cloud type at the scene's day, as a small integer: `0` none, `1` cumulus, `2` stratus, `3` nimbostratus, `4` cumulonimbus, `5` cirrus — the weather state's face; see [`hornvale_climate::GeneratedClimate::cloud_type_at`]. |
| `locked` | boolean | Whether the world is tidally locked. On a locked world the seasonal temperature is **not** `t_mean_c + t_swing_c·sin(…)` — `t_swing_c` is `0` there — but a *librating substellar* field the client reconstructs from the world's obliquity and the year phase (see below), so a consumer reads this flag to choose its evaluator. |

`elevation_m`, `ocean`, `biome`, `plate`, and `unrest` are the five per-tile
layers: each is an array of exactly `width × height` entries, in the grid
order described below, so tile `i`'s elevation, ocean flag, biome, plate,
and unrest all sit at the same index `i` across their respective arrays.
`t_mean_c`, `t_swing_c`, `moisture`, the precipitation layers
(`precip_mm_yr`, `snow_fraction`, `precip_regime`, `cloud_fraction`), and
the weather layers (`weather_propensity`, `cloud_type`) are further per-tile
layers of the same shape, appended after `features` (see Stability, below,
on the append-at-end convention); `season_period_days` and
`circulation_bands` are document-level, not per-tile.
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
them — the world's capital, the single highest-population settlement any
species founded — carries `kind: "flagship"` instead of `kind:
"settlement"`, and it always appears last in the array. A flagship never also appears as a
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
  ],
  "t_mean_c": [ ... 32768 entries ... ],
  "t_swing_c": [ ... 32768 entries ... ],
  "season_period_days": 368.05357,
  "circulation_bands": 3,
  "moisture": [ ... 32768 entries ... ],
  "precip_mm_yr": [ ... 32768 entries ... ],
  "snow_fraction": [ ... 32768 entries ... ],
  "precip_regime": [ ... 32768 entries ... ],
  "cloud_fraction": [ ... 32768 entries ... ],
  "weather_propensity": [ ... 32768 entries ... ],
  "cloud_type": [ ... 32768 entries ... ],
  "locked": false
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

## Reading temperature over the year

`t_mean_c` and `t_swing_c` do not carry a temperature at every day — a
client evaluates one from the two layers plus `season_period_days`. This is
the normative evaluator, and it is exact — the same arithmetic the
simulation itself uses to produce `t_mean_c`/`t_swing_c` in the first place:

```
t(tile, day) = t_mean_c[tile] + t_swing_c[tile] · sin(τ · frac(day / season_period_days + year_phase_offset))
```

where `day` is absolute standard days (`WorldTime`), `τ = 2π`,
`frac(x) = x − floor(x)`, and `year_phase_offset` is `scene/system/v1`'s
field of that name (`0` on constant-sun worlds, which have no system
document).

**The seasonal phase is `frac(day / season_period_days + year_phase_offset)`
— it carries the orbital offset.** The season is *caused* by the sun's
declination, which is itself phased on `frac(day / year + year_phase_offset)`
(`Calendar::year_phase`); the two must share a phase or the ice will lead or
lag the sun by that offset. This is the one place the seasonal evaluator
reaches outside this document — for the offset only, a single scalar from
`scene/system/v1` — so a generated-sky client passes it through; a
constant-sun client uses `0`.

Locked worlds are the exception, and read the `locked` flag to take it:
`t_swing_c` is `0`, but the seasonal temperature is not the mean. A tidally
locked world with axial tilt *librates* — its substellar point swings north
and south in latitude by `obliquity · sin(τ · frac(day / season_period_days
+ year_phase_offset))` over the year. The client reconstructs the surface
temperature from that moving substellar point (the day-side substellar
cosine, the night floor, and the elevation lapse), evaluated at each tile's
own centre — the same closed-form evaluator the producer exposes as
`locked_temperature_at_position`, pinned by a producer-sourced golden. The
hot spot, and the ice at the freeze line, track the sun across the year.

**Precision:** producer-side, this formula reproduces the simulation's
`temperature_at` to exact `f64` equality — it is the same arithmetic,
restated, and a contract test pins that restatement. A client, however,
reads `t_mean_c` and `t_swing_c` **after** they have been quantized at the
serialization boundary (8 significant digits, decision 0033), so a client's
evaluator reproduces `temperature_at` only to quantization precision — about
8 significant digits, not exact equality. Do not mistake the quantized
reconstruction for the exact one; they agree closely, not bit-for-bit.

## Prevailing winds

There is no per-tile wind array — the model has no per-tile variance to
ship. Given `circulation_bands` = B (present only on spinning worlds):

```
width      = 90° / B
band(φ)    = min(floor(|φ| / width), B − 1)        # φ = tile latitude, degrees
direction  = easterly (blowing toward −east) when band(φ) is even
             westerly (blowing toward +east) when band(φ) is odd
```

Winds are purely zonal and direction-only — the model carries no speed, so
none is offered; a speed would be invented precision. The direction is
undefined at the exact poles. Even bands are the rising (wet) belts, odd
bands the sinking (dry) belts — stated here because it explains the shape
of the `moisture` layer's belts (wet equator, dry horse latitudes, and so
on) and because the parity will matter to future data modes.

When `circulation_bands` is absent, the world is tidally locked: there is no
banded zonal circulation at all. The climate model instead organizes
moisture around the terminator (the day/night boundary), but this contract
does not promise renderable wind data for that regime — only the tile
layers themselves.

## Ice (a client derivation)

Ice is not a layer — the simulation has no cryosphere yet, so freezing is
something a client derives from the temperature layers, not something the
document states directly:

- The coldest day of the year at a tile evaluates to `t_mean_c − |t_swing_c|`
  (the trough of the sinusoid above, reached once per hemisphere's winter).
- A natural freeze threshold is **≤ 0 °C** — suggested, not contractual; a
  client owns how (or whether) it presents freezing, per decision 0022.
- Seasonal ice falls out for free: evaluate the temperature evaluator above
  at the current day and freeze whatever tile is at or below the threshold.
  The ice edge then advances and retreats with the season, and the two
  hemispheres are opposite-phased (their `t_swing_c` signs differ), exactly
  as `t_swing_c`'s hemisphere signing promises.
- Locked worlds are static: `t_swing_c` is `0`, so nothing advances or
  retreats — a fixed night-side sheet and twilight band fall out of
  `t_mean_c` alone.

If the simulation ever grows a real cryosphere (registry rows
CLIM-cryosphere / CLIM-ice-albedo), a genuine `ice` layer would supersede
this guidance additively — appended as a new field, this section retired,
never a re-meaning of what's here.

## Going closer

For one tile's footprint at higher on-tile sample density, see
[`scene/tiles-region/v1`](./scene-tiles-region-v1.md).

## Stability

`scene/tiles/v1` is a save-format-class contract, held to the same
discipline as the rest of the world's on-disk formats:

- `biome_legend`'s order is append-only: a biome already in the catalog
  keeps its index forever, so a `biome` value from an old document still
  means the same thing against a newer legend.
- Adding a new field, or a new entry to `biome_legend` or `features`,
  stays within `scene/tiles/v1` — existing consumers that read fields by
  name are unaffected.
- New fields always append **after** every existing field — a round that
  adds fields lands them at the tail of the object, after whatever was
  last before it. Wire order is therefore historical accretion order, not a
  semantic grouping (this page's field table groups fields by meaning
  regardless of where they sit on the wire); the *names* are the
  compatibility contract, not their position. This is what makes golden
  diffs tail-only and every future additive round mechanical: a consumer
  that reads fields by name never has to change when a round like this
  one lands.
- Changing an existing field's meaning, order, or type never happens in
  place. That mints `scene/tiles/v2` as a new schema alongside `v1`, which
  keeps emitting exactly what it always emitted.
- The committed example, `gallery/scene-tiles-seed-42.json`, is regenerated
  from seed 42 and checked byte-for-byte by CI; if the generator's output
  ever drifts from the committed file, the build fails rather than let the
  schema silently change underneath its own example. The [atlas
  page](../gallery/atlas.md) renders this same document interactively — the
  schema's first live consumer.

## Getting one

```
hornvale scene tiles [--world <path>] [--width <n>]
```

This prints one `scene/tiles/v1` document to standard output. `--world`
defaults to `world.json`; `--width` defaults to 256. Width must be even —
height is always `width / 2` — and must fall between 16 and 1024 inclusive;
anything else is refused with a description of which bound it violates.
