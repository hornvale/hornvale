# Scene Schema: system v1

`scene/system/v1` is the orrery scene: a generated world's star system as
orbital elements — the star's class and habitable zone, the anchor world's
orbit and spin, and every moon's orbit. It says nothing about *where*
anything currently sits; a client evaluates position and phase from these
elements and the sim clock's current day, exactly as [`scene/tiles/v1`'s
temperature evaluator](./scene-tiles-v1.md#reading-temperature-over-the-year)
evaluates a temperature from elements rather than storing one per day.

Every valid world has a generated sky and therefore a system to describe.

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
| `stellar` | object | Topology-aware stellar root, appended after the legacy fields. |
| `wanderers` | array of object | Astronomy-first sibling bodies, in orbital order. |

The original `schema`, `seed`, `star`, `world`, and `moons` fields retain their
meaning and order. The legacy `star` object remains the primary-star
compatibility view. A topology-aware client uses `stellar` when it needs the
binary root; a single-star consumer can ignore the appended fields.

`stellar` is:

| Field | Type | Meaning |
|---|---|---|
| `topology` | string | `single`, `wide-binary`, or `close-binary`. |
| `primary` | object | Primary mass, class, and luminosity. |
| `companion` | object, absent for `single` | Secondary star and its bounded binary orbit. |
| `combined_luminosity_rel` | number | Sum of the root stars' luminosities. |
| `anchor_hz_inner_au`, `anchor_hz_outer_au` | number | Topology-aware anchor admission envelope, AU. |
| `circumprimary_outer_limit_au` | number, absent except for wide binaries | Outer stability boundary for a circumprimary anchor. |
| `circumbinary_inner_limit_au` | number, absent except for close binaries | Inner stability boundary for a circumbinary anchor. |

Each stellar body has `class_name`, `mass_rel`, and `luminosity_rel`. A
companion's `orbit` has `semi_major_axis_au`, `period_days`, and
`phase_offset`. The binary orbit is circular and coplanar in this version;
the close-binary positions use the two-body barycentric approximation, while
wide-binary anchors remain circumprimary.

Each entry in `wanderers` is:

| Field | Type | Meaning |
|---|---|---|
| `orbit_au` | number | Circular orbital radius in AU. |
| `period_days` | number | Circular orbital period in standard days. |
| `phase_offset` | number | Heliocentric phase at absolute day zero, in turns `[0, 1)`. |
| `class` | string | `rock` or `giant`. |
| `albedo` | number | Bond albedo used by the astronomy model. |
| `synodic_period_days` | number | Synodic period against the anchor; non-finite means no finite recurrence. |
| `max_elongation_deg` | number, absent for outer bodies | Geometric maximum elongation for an inner body. |

The scene contains elements only: it contains no sampled position, current
brightness, event list, or visibility result.

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

For a wanderer `i`, the shared circular evaluator begins with:

```
wandererPhase(i, t) = frac(t / period_days + phase_offset)
wandererPosition(i, t) = orbit_au · (cos(τ · wandererPhase), sin(τ · wandererPhase))
```

The apparent longitude is the angle of the wanderer's position minus the
anchor position. Inner bodies are constrained by `max_elongation_deg`; outer
bodies use opposition geometry. Conjunctions, oppositions, retrograde loops,
and visibility are derived at query time from these positions. A hidden body
is still present in `wanderers` and is never removed from the scene.

For `stellar_positions_at`, a single system has the primary at the origin. A
wide binary keeps the primary at the origin and evaluates the companion on
the emitted circular binary orbit. A close binary evaluates both stars around
their barycenter according to their emitted masses, separation, period, and
phase. These are bounded circular/two-body approximations: eccentricity,
inclination, arbitrary multiplicity, transits, and occultations are outside
v1.

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
defaults to `world.json`. A malformed ledger with no `sky-provider` fact is
not a valid world and fails to load (decision 0737).
