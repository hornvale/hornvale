# Scene Schema: system v1

`scene/system/v1` is the orrery scene: a generated world's star system as
orbital elements — the star's class and habitable zone, the anchor world's
orbit and spin, and every moon's orbit. It says nothing about *where*
anything currently sits; a client evaluates position and phase from these
elements and the sim clock's current day, exactly as [`scene/tiles/v1`'s
temperature evaluator](./scene-tiles-v1.md#reading-temperature-over-the-year)
evaluates a temperature from elements rather than storing one per day.

Only a world with a **generated** sky has a system to describe. Tier-0
constant-sun worlds have no orrery — asking one for `scene/system/v1`
fails with a description of why.

## The document

Every `scene/system/v1` document is one JSON object with these fields, in
this order (field order is part of the contract — it is fixed, not
incidental):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/system/v1"` — the version tag a consumer checks before trusting the rest of the document. |
| `seed` | integer | The world's seed. This is a u64; JavaScript consumers parsing the document with plain `JSON.parse` lose integer precision above 2^53, so use BigInt-aware parsing when the exact seed matters. |
| `star` | object | The central star, described below. |
| `world` | object | The anchor world's orbital and rotational elements, described below. |
| `moons` | array of object | Every moon's orbital elements, in generation order, described below. |

`star` is:

| Field | Type | Meaning |
|---|---|---|
| `class_name` | string | Descriptive spectral class name, e.g. `"yellow dwarf (G)"`. |
| `luminosity_rel` | number | Luminosity, in solar luminosities. |
| `hz_inner_au` | number | Habitable-zone inner edge, AU. |
| `hz_outer_au` | number | Habitable-zone outer edge, AU. |

`world` is:

| Field | Type | Meaning |
|---|---|---|
| `orbit_au` | number | Orbital radius, AU. |
| `year_days` | number | Year length, standard days — one full orbit. |
| `day_length_days` | number, **absent when tidally locked** | Solar-day length, standard days. Absent (not zero, not null — the key is omitted entirely) when the world has no spin to speak of. |
| `obliquity_deg` | number | Mean axial obliquity, degrees. |
| `year_phase_offset` | number | The world's orbital phase at day 0, in turns ([0, 1)) — genesis places the world at an arbitrary point on its orbit rather than always starting at periapsis, and this is that placement. Orbital geometry only; see the warning below about what this offset does *not* apply to. |

Each entry in `moons` is:

| Field | Type | Meaning |
|---|---|---|
| `sidereal_days` | number | The moon's sidereal orbital period, standard days. |
| `phase_offset` | number | The moon's synodic-phase offset at day 0, in turns ([0, 1)) — where in its light cycle the moon starts, analogous to `year_phase_offset` but for the moon's phase rather than the world's orbital position. |
| `distance_mm` | number | Orbital distance from the world, megameters. |
| `size_rel` | number | Angular-diameter ratio — the moon's apparent size relative to the reference the size-word vocabulary is built from. |
| `inclination_deg` | number | Orbital inclination to the anchor's orbital plane, in degrees. Above 90° the moon orbits **retrograde** (The Reckoning's captured moons). Appended after `size_rel` per the schema's stability contract. |
| `node_longitude_deg` | number | Ecliptic longitude of the ascending node at genesis, in degrees ([0, 360)). Appended after `inclination_deg` per the stability contract. |

Each moon's surface (radius, gravity, and seeded descriptors) is its own
document: [`scene/moons/v1`](scene-moons-v1.md).

An excerpt of a `scene/system/v1` document (seed 42; two moons):

```json
{
  "schema": "scene/system/v1",
  "seed": 42,
  "star": {
    "class_name": "yellow dwarf (G)",
    "luminosity_rel": 0.70079542,
    "hz_inner_au": 0.79527848,
    "hz_outer_au": 1.1468753
  },
  "world": {
    "orbit_au": 0.97164647,
    "year_days": 368.05357,
    "day_length_days": 0.87987998,
    "obliquity_deg": 0.95930567,
    "year_phase_offset": 0.20941868
  },
  "moons": [
    { "sidereal_days": 15.993805, "phase_offset": 0.85759808, "distance_mm": 307.74439, "size_rel": 1.6350803, "inclination_deg": 4.6667409, "node_longitude_deg": 1.5976041 },
    { "sidereal_days": 32.555, "phase_offset": 0.25842259, "distance_mm": 494.27358, "size_rel": 0.69049995, "inclination_deg": 117.27724, "node_longitude_deg": 193.38776 }
  ]
}
```

On a tidally locked world, `day_length_days` is simply missing from
`world` — the same absent-key convention `scene/tiles/v1`'s
`circulation_bands` follows, and for the same reason: locked worlds have no
solar day to report, not a zero-length one.

## Reading positions and phases

None of the fields above is a live position — a client evaluates one from
the elements plus the sim clock's current day, `t` (absolute standard
days). These four evaluators are normative; they are what the orrery
client implements, and this page is their one specified home. Throughout,
`frac(x) = x − floor(x)`, so every phase below lands in `[0, 1)`.

```
worldPhase(t)   = frac(t / year_days + year_phase_offset)
```

The world's position on its orbit, in turns. `year_phase_offset` enters
here — this is the one place it applies. A client turns this into an angle
with `θ = τ · worldPhase(t)`.

```
synodicDays(i)  = period ≥ year ? ∞ (never laps) : (period · year) / (year − period)
```

The synodic period of moon `i` against the world's year — how many
standard days elapse between successive occurrences of the same
world–moon–star alignment. `period` is that moon's `sidereal_days`; `year`
is `year_days`. A moon whose sidereal period is at least as long as the
year never laps the world from the star's point of view, so its synodic
period is infinite (no beat — a client should treat this as "does not
recur," not as a very large finite number).

```
moonPhase(i, t) = frac(t / synodicDays(i) + phase_offset)
```

Moon `i`'s light-cycle phase at day `t`, using that moon's own
`phase_offset`. `0` is new, `0.5` is full — the conventional phase-naming
midpoint.

```
rotationPhase(t) = day_length_days present ? frac(t / day_length_days) : 0
```

The world's rotation phase at day `t` — how far it has spun since the
start of its current solar day, in turns. On a tidally locked world (no
`day_length_days`), this is defined as `0` at every `t`: the world
presents the same face at every moment, so there is no rotation phase to
report.

### The offset that does and doesn't apply

`worldPhase` and [`scene/tiles/v1`'s seasonal-temperature
evaluator](./scene-tiles-v1.md#reading-temperature-over-the-year) look
almost identical — both divide `t` by a period and take `frac` of the
result — but only one of them adds a phase offset:

- `worldPhase(t) = frac(t / year_days + year_phase_offset)` — **does**
  apply `year_phase_offset`, because it answers "where is the world on its
  orbit," which is orbital geometry, and orbital geometry is exactly what
  that offset places.
- The tiles page's `t(tile, day)` evaluator uses `frac(day /
  season_period_days)` with **no offset at all** — because it answers "how
  far into the seasonal cycle are we," and the climate model's seasonal
  sinusoid is defined to start at zero phase at day 0, independent of
  where the world happens to sit on its orbit that same day.

Reusing `year_phase_offset` in the seasonal-temperature evaluator (or
omitting it from `worldPhase`) is the single most likely mistake a client
makes wiring these two documents together — they share a shape, not a
value.

## Stability

`scene/system/v1` is a save-format-class contract, held to the same
discipline as `scene/tiles/v1`:

- Adding a new field stays within `scene/system/v1` and appends after every
  existing field, exactly as `scene/tiles/v1`'s Stability section
  describes; existing consumers that read fields by name are unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place — that mints `scene/system/v2` alongside `v1`.
- `day_length_days`'s absent-when-locked convention is part of the
  contract, not an implementation detail: a client must branch on the
  key's presence, never on a sentinel value.

## Getting one

```
hornvale scene system [--world <path>]
```

This prints one `scene/system/v1` document to standard output. `--world`
defaults to `world.json`. A world with no generated sky (the tier-0
constant sun) has no system to describe, and the command fails with a
message saying so.
