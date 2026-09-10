# Scene Schema: eclipses v3

`scene/eclipses/v3` is a closed-window query over an anchor world's eclipse
calendar. It carries the system's eclipse recurrence ladder, the dated solar
and lunar events inside the requested window, each event's physical visibility
region, and optional results for one geographic observer.

v3 replaces the pre-alpha v2 contract. It keeps v2's exact integer times and
adds recurrence and observer data as one versioned shape; the producer does
not emit a v2 compatibility shape.

## Query and document

The producer accepts a world, `from` and `until` as `StdInstant` values, and an
optional observer latitude and longitude. The window is closed: an event at
either endpoint is included. Latitude must be finite and inside `[-90, 90]`.
Longitude may be any finite value and is normalized to `[-180, 180)`.

The compact JSON object has these fields in this order:

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always `"scene/eclipses/v3"`. |
| `seed` | integer | The world's `u64` seed. |
| `from` | integer | Closed window start, exact ticks since genesis. |
| `until` | integer | Closed window end, exact ticks since genesis. |
| `observer` | object, omitted | The normalized observer query, present only when supplied. |
| `recurrences` | array | System-wide recurrence records in moon order, solar then lunar per moon. |
| `events` | array | Events in ascending tick order, with moon index as the tie break. |

`WorldTime` has 100,000 ticks per standard day. `from`, `until`, and every
event `day` are explicit `i64` values on the wire. They never pass through a
floating-point representation or the float quantizer.

The recurrence array describes the system and does not depend on the query
window. A one-day query and a thousand-day query over the same world therefore
carry identical recurrence records even though their event arrays differ.

## Recurrence records

Each `recurrences` entry has:

| Field | Type | Meaning |
|---|---|---|
| `moon_index` | integer | Distance-sorted moon index, shared with `scene/moons/v1`. |
| `body` | string | `"solar"` or `"lunar"`, the eclipse family. |
| `draconic_month_days` | number | The moon's return to its node, standard days. |
| `eclipse_year_days` | number | The star's return to the moon's node line, standard days. |
| `cycle` | object | The selected bounded synodic/draconic return. |
| `series_returns` | integer | Estimated returns before the series leaves the eclipse window. |
| `series_lifetime_days` | number | `series_returns × cycle.period_days`. |
| `exeligmos_period_days` | number | Exactly three selected-cycle periods. |
| `exeligmos_node_slip_deg` | number | Node-phase slip accumulated over those three returns. |
| `parade_days_per_year` | number | Signed eclipse-season migration through one civil year. |

`cycle` has `synodic_count`, `draconic_count`, `period_days`, and
`node_slip_deg`. It is the best return found by the astronomy domain's bounded
search, not a claim that every generated moon has Earth's literal 223/242
Saros. Solar and lunar records for one moon share their cycle but may have
different `series_returns` and `series_lifetime_days` because their admitted
node windows differ.

All recurrence numbers are derived from existing orbital and calendar facts.
Every emitted float is rounded to eight significant digits by the standard
scene `f64_field` serializer, at serialization only.

## Events and physical regions

Each `events` entry has:

| Field | Type | Meaning |
|---|---|---|
| `day` | integer | Syzygy instant as exact ticks since genesis. |
| `moon_index` | integer | Distance-sorted moon index. |
| `body` | string | `"solar"` or `"lunar"`. |
| `kind` | string | `"total"` or `"annular"`. Lunar events are total while partiality remains deferred. |
| `region` | string | `"ground-track"` for solar or `"night-hemisphere"` for lunar. |
| `track` | object or null | Approximate solar shadow band; explicitly `null` for lunar events. |
| `observer` | object, omitted | This observer's result, present only when an observer was supplied. |

`region` and `track` describe the event's physical geography. They do not
describe a particular observer. A lunar eclipse has
`region: "night-hemisphere"` and `track: null`: the anchor's shadow falls on
the moon and the event is available to the whole night hemisphere, so there is
no narrow surface band.

A solar `track` contains `center_lat_deg`, `half_width_deg`, `start_lon_deg`,
`end_lon_deg`, and `duration_days`. These floats use the same serialization
quantizer as recurrence values.

The track is a declared approximation. Its center interpolates from the
sub-solar latitude toward a pole according to the moon's normalized ecliptic
latitude, and its half-width is the calibrated `2°` band. The longitude arc is
the directed sweep made during the crossing, including wrap across the
`-180°/180°` seam and full-world coverage when a short day lets the world turn
through at least 360°. Clients may project this emitted region but may not
recalculate whether an observer sees the eclipse.

## Observer presence and visibility

When no observer is requested, the top-level `observer` key and every event's
`observer` key are omitted. This is the wire representation of “no observer
question was asked.”

When an observer is requested, the top-level object echoes normalized
`latitude_deg` and `longitude_deg`, and every event carries an observer result:

| Field | Values | Meaning |
|---|---|---|
| `side` | `"day"`, `"night"` | The observer's hemisphere at the event instant. |
| `visibility` | `"whole-sun"`, `"burning-ring"`, `"bitten"`, `"visible"`, `"unseen"` | The astronomy domain's result for this observer and event. |

For solar events, `whole-sun` and `burning-ring` are the central track tiers,
`bitten` is a day-side observer outside the central track, and `unseen` is the
night side. For lunar events, `visible` means the observer is on the night side
and `unseen` means the observer is on the day side.

An emitted `{"side":"night","visibility":"unseen"}` is therefore distinct
from an omitted observer key. The former answers a supplied query; the latter
records that no query was supplied. The observer object never contains
`region` or `track`, which remain event-scale physical fields.

## Determinism and fixture

The producer is a pure read over the existing world ledger and astronomy
derivations. It adds no draw, stream label, epoch, or save fact and does not
mutate the world. Equal world, window, and observer inputs serialize to
byte-identical JSON. Event order is `(day, moon_index)` ascending and
recurrence order is `(moon_index, solar-before-lunar)`.

The committed producer-generated byte fixture is
`windows/scene/tests/fixtures/eclipses-seed-42.json`. It is generated only
through the scene golden test's `REBASELINE=1` path and is checked without that
flag during ordinary tests.
