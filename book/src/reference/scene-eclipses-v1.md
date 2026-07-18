# Scene Schema: eclipses v1

`scene/eclipses/v1` is the dated eclipse events an anchor world's system
produces over a requested day window: for each solar or lunar syzygy, when
it falls, which moon caused it, whether it is total or annular, and — for
solar events only — the shadow's ground track across the globe. It exists
because [`scene/moons/v1`](./scene-moons-v1.md) and
[`scene/system/v1`](./scene-system-v1.md) describe the system's standing
geometry, but an eclipse is not standing geometry — it is a dated
occurrence, and this schema is the first to describe *when things happen*
rather than *what things are*.

## The parameterized window

Every other scene schema in the book so far is a **snapshot**: give it a
world, and it hands back the one document that world has. `scene/eclipses/v1`
is different — it is a **query**, the same shape as
[`scene/tiles-region/v1`](./scene-tiles-region-v1.md)'s addressed tile
request. A client asks for `[from_day, until_day]` and the document echoes
that window back alongside the events found inside it. The window is
**closed** — both endpoints inclusive: an eclipse landing exactly on
`until_day` is returned (the producer filters `day < from || day > until`).

| Field | Type | Meaning |
|---|---|---|
| `from_day` | number | The queried window start, absolute standard days — echoed back unchanged. |
| `until_day` | number | The queried window end, absolute standard days — echoed back unchanged. |

This is the right shape because eclipses are a **temporal series**, not a
fixed-size structure: a world can run for an arbitrary number of standard
days, and enumerating "all eclipses" has no natural end. A client showing
a calendar page, an almanac year, or an orrery's visible time range asks
for exactly the window it is displaying — the document is only ever as
large as what was requested, never the whole world's eclipse history. This
mirrors why `scene/tiles-region/v1` echoes its five-integer address rather
than returning one fixed document: both schemas parameterize because the
underlying question ("which tile?", "which day range?") has no single
answer for a world, only an answer for a query.

## The document

Every `scene/eclipses/v1` document is one JSON object with these fields,
in this order (field order is part of the contract — it is fixed, not
incidental):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/eclipses/v1"` — the version tag a consumer checks before trusting the rest of the document. |
| `seed` | integer | The world's seed. This is a u64; JavaScript consumers parsing the document with plain `JSON.parse` lose integer precision above 2^53, so use BigInt-aware parsing when the exact seed matters. |
| `from_day` | number | The queried window start, echoed back — see above. |
| `until_day` | number | The queried window end, echoed back — see above. |
| `events` | array of object | The dated eclipses inside the closed window `[from_day, until_day]`, day-ascending. |

Each entry in `events` is:

| Field | Type | Meaning |
|---|---|---|
| `day` | number | The syzygy, absolute standard days (`WorldTime`). |
| `moon_index` | integer | Distance-sorted index into the system's moons — the same index `scene/moons/v1` uses. |
| `body` | string | `"solar"` or `"lunar"` — which body is eclipsed: the anchor's star, or the moon itself. |
| `kind` | string | `"total"` or `"annular"` — whether the eclipsing disc fully covers the eclipsed one or leaves a burning ring. |
| `track` | object or null | The shadow's ground track, **solar events only** — see below. |

### The solar-only `track`

`track` is `Some`/present only for `body: "solar"` events; a lunar event
always carries the literal JSON `"track": null`. This is not an
omitted-field convention (contrast a `skip_serializing_if` absence) — the
key is always on the wire, explicitly null, so a consumer sees the absence
rather than inferring it from a missing key. The reason is physical, not
an arbitrary schema choice: a solar eclipse's shadow is a **band**, cast by
the moon onto a limited swath of the globe as the world turns beneath
it — only observers under that band see it. A lunar eclipse's shadow is
the anchor's own umbra falling on the moon, visible from the entire night
side of the world at once; there is no band to describe, because the
"track" would be the whole night hemisphere.

When present, `track` is:

| Field | Type | Meaning |
|---|---|---|
| `center_lat_deg` | number | Latitude of the band's center at mid-event, degrees. |
| `half_width_deg` | number | Half-width of the full-omen band, degrees of latitude. |
| `start_lon_deg` | number | Sub-solar longitude at crossing start, degrees in [-180, 180). |
| `end_lon_deg` | number | Sub-solar longitude at crossing end, degrees. |
| `duration_days` | number | Crossing duration, standard days — the moon's synodic drift across the combined discs. |

## The ground-track latitude is a declared approximation

`center_lat_deg` is not a first-principles projection of the moon's shadow
cone onto the globe — it is a stated approximation, documented on the
producer's `ground_track` function
(`domains/astronomy/src/eclipses.rs`) in its own words:

> The ground track of a dated solar eclipse; `None` for a lunar event — the
> anchor's shadow is one shadow for the whole night side. The latitude
> mapping is a declared approximation (model card): center = solar
> declination + (β/θ)·(90° − |declination|), so a central pass tracks the
> sub-solar latitude and a threshold-grazing one exits at a pole.

In the formula, `β` is the moon's ecliptic latitude at the event (how far
off exact alignment it falls) and `θ` is the solar-eclipse threshold angle
(how far off alignment is still close enough to eclipse at all) — so
`β/θ` is a normalized "how central is this pass" ratio, `0` at dead center
and `1` at the threshold-grazing edge. A dead-central pass (`β/θ = 0`)
tracks the sub-solar latitude exactly (`center_lat_deg = solar
declination`); a pass that just barely clears the eclipse threshold
(`β/θ → 1`) is pushed all the way to the nearer pole
(`center_lat_deg → ±90°`, clamped there). This is a reasonable interpolation,
not a ray-traced shadow projection — it gets the two limiting cases right
and behaves smoothly in between, but a client should not treat
`center_lat_deg` as more precise than that: it is the declared shape of an
approximation, not a re-derivation of the umbra's true footprint.

An excerpt of a `scene/eclipses/v1` document (seed 42, window `[0, 2000]`
standard days; the full document has 50 events — 31 solar, 19 lunar,
drawn from 2 moons):

```json
{
  "schema": "scene/eclipses/v1",
  "seed": 42,
  "from_day": 0.0,
  "until_day": 2000.0,
  "events": [
    {
      "day": 85.982974,
      "moon_index": 0,
      "body": "solar",
      "kind": "total",
      "track": {
        "center_lat_deg": 82.494963,
        "half_width_deg": 2.0,
        "start_lon_deg": -32.693882,
        "end_lon_deg": -58.835236,
        "duration_days": 0.063892372
      }
    },
    {
      "day": 94.34317,
      "moon_index": 0,
      "body": "lunar",
      "kind": "total",
      "track": null
    }
  ]
}
```

The full document is committed at
[`book/src/gallery/scene-eclipses-seed-42.json`](../gallery/scene-eclipses-seed-42.json).

## Derived vs approximated

- **`events`' `day`, `moon_index`, `body`, and `kind` are computed, not
  drawn.** No new random draw backs an eclipse — the events fall out of
  the system's already-drawn orbital elements (each moon's synodic month,
  orbital inclination, and angular size) evaluated forward through the
  requested window. Two identical worlds queried over the same window
  always produce byte-identical events; this is a pure read, consuming no
  genesis draws and committing no new fact.
- **`track` is a declared approximation, stated as such in the producer's
  own doc comment** — see above. `half_width_deg` in particular is not
  derived per-event at all: it is a fixed calibrated constant
  (`TRACK_HALF_WIDTH_DEG = 2.0`, deliberately generous — real totality
  bands are closer to ~1° of latitude, widened here so a band is findable
  at room scale), not a function of the specific eclipse's geometry.

A consumer that treats `center_lat_deg`/`start_lon_deg`/`end_lon_deg` as a
survey-grade shadow path has over-trusted the document: it is a good enough
band for placing a room-scale omen, not a ray-traced eclipse-chaser's
map.

## The u64 seed caveat

`seed` is a u64. JavaScript's `JSON.parse` reads all numbers as IEEE-754
doubles, which lose integer precision above 2^53 (~9 × 10^15) — most
Hornvale seeds are small enough to round-trip safely, but a consumer that
needs the *exact* seed value (rather than treating it as an opaque label)
should parse this field with a BigInt-aware JSON reader rather than
trusting plain `JSON.parse`.

## Stability

`scene/eclipses/v1` is a save-format-class contract, held to the same
discipline as the rest of the scene schemas:

- Adding a new field stays within `scene/eclipses/v1` and appends after
  every existing field; existing consumers that read fields by name are
  unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place — that mints `scene/eclipses/v2` alongside `v1`.
- Changing the ground-track approximation's formula, or replacing it with
  a higher-fidelity projection, is a `v2` change, not a `v1` amendment —
  the model card above is as normative as any field's type.

The committed example,
[`book/src/gallery/scene-eclipses-seed-42.json`](../gallery/scene-eclipses-seed-42.json),
is generated by `hornvale scene eclipses --world <seed-42 generated-sky
world> --from 0 --until 2000` and drift-checked in CI — regenerating it from
the same seed, pins, and window always reproduces the same bytes, because
every field routes through a pure ledger/orbital-element read or the
declared-approximation formula above, none of which touches wall-clock
time or platform-dependent floating point beyond the quantization boundary
(decision 0033).

## Getting one

```
hornvale scene eclipses --from <day> --until <day> [--world <PATH>]
```

This prints one `scene/eclipses/v1` document to standard output. `--world`
defaults to `world.json`; `--from` and `--until` are required. A world with
no generated sky (the tier-0 constant sun, or a sky with no moons to raise
an eclipse) has no eclipses to describe, and the command fails with a
message saying so.
