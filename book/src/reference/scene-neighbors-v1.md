# Scene Schema: neighbors v1

`scene/neighbors/v1` is the night sky's two populations: the drawn,
named-by-class notable neighbor stars, and the anonymous background
starfield they sit among. It exists because
[`scene/system/v1`](./scene-system-v1.md) and
[`scene/moons/v1`](./scene-moons-v1.md) describe the anchor world's own
star and moons — nothing else in the sky. This page is that missing
backdrop.

State the boundary up front, because it drives everything below: the sim's
night sky is not only the 2–5 **neighbors** the generator draws as
physical objects — each backed by an `is-neighbor` ledger fact, with its
class, color, distance, and equatorial position each their own facts
(`domains/astronomy/src/facts.rs`). The domain also derives a 100–300-star
**background field** (`hornvale_astronomy::starfield`, seeded from
`world.seed.derive(ASTRONOMY_STREAM_ROOT)`) — the same population
`figures()` clusters into notable sky shapes and the almanac's chart
draws. `starfield.rs`
frames its own output precisely: "a catalog of faint stars derived on
demand, never serialized... texture only — no genesis draws from this
stream." That is the epistemic line a consumer must not blur: a neighbor
is a drawn entity with a chain of facts behind it; a field star is
recomputed from the seed each time this document is built and never
touches the ledger at all. A neighbors-only export would render an
*emptier* sky than the sim actually owns, so `scene/neighbors/v1` carries
both, in one document, clearly separated.

Neighbors are anonymous — the sim names no star (proper names are a
captured future direction, not a gap here). Identity for display is
`color` + `class_name` ("the amber giant"), the almanac's own convention.

Only a world with a **generated** sky has a neighborhood or a starfield to
describe. Tier-0 constant-sun worlds have no orrery and no neighbors;
asking one for `scene/neighbors/v1` fails with a description of why —
mirroring `scene/system/v1` and `scene/moons/v1`'s same refusal.

## The document

Every `scene/neighbors/v1` document is one JSON object with these fields,
in this order (field order is part of the contract — it is fixed, not
incidental):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/neighbors/v1"` — the version tag a consumer checks before trusting the rest of the document. |
| `seed` | integer | The world's seed. This is a u64; JavaScript consumers parsing the document with plain `JSON.parse` lose integer precision above 2^53, so use BigInt-aware parsing when the exact seed matters. |
| `neighbors` | array of object | The notable neighbors, generation order (brightest first) — the same order as the `is-neighbor` ledger entities. |
| `stars` | array of object | The background starfield, derivation order. Not ledger entities; recomputed each time this document is built from `world.seed.derive(ASTRONOMY_STREAM_ROOT)`. |

Each entry in `neighbors` is:

| Field | Type | Meaning |
|---|---|---|
| `index` | integer | Generation index — stable identity, matches the corresponding `is-neighbor` ledger entity's order. |
| `class_name` | string | Prose spectral class (e.g. `"red giant"`), from `hornvale_astronomy::class_name`. |
| `color` | string | The producer's color word (e.g. `"smoldering red"`) — distinct from `class_name`; together they form the almanac's naming convention ("the smoldering red giant"). |
| `distance_ly` | number | Distance in light-years — drawn, 4–80. |
| `brightness_rel` | number | Apparent brightness, relative units — derived as L/d² from the neighbor's luminosity and `distance_ly`. |
| `ra_deg` | number | Right ascension, degrees in [0, 360) — genesis-epoch equatorial. See "The frame convention" below. |
| `dec_deg` | number | Declination, degrees in [-90, 90] — genesis-epoch equatorial. See "The frame convention" below. |

Each entry in `stars` is:

| Field | Type | Meaning |
|---|---|---|
| `ra_deg` | number | Right ascension, degrees in [0, 360) — genesis-epoch equatorial, same frame as the neighbors above. |
| `dec_deg` | number | Declination, degrees in [-90, 90] — genesis-epoch equatorial, same frame as the neighbors above. |
| `magnitude_class` | integer | Brightness class, 1 (brightest) through 5 (faintest) — a dim-heavy distribution, like a real sky. |

An excerpt of a `scene/neighbors/v1` document (seed 42; the full document
has 5 neighbors and 148 field stars):

```json
{
  "schema": "scene/neighbors/v1",
  "seed": 42,
  "neighbors": [
    { "index": 0, "class_name": "red giant", "color": "smoldering red", "distance_ly": 68.232281, "brightness_rel": 0.064437915, "ra_deg": 81.841371, "dec_deg": -65.242947 },
    { "index": 1, "class_name": "sun-like star", "color": "warm yellow", "distance_ly": 4.135297, "brightness_rel": 0.058477201, "ra_deg": 33.465746, "dec_deg": 21.140972 }
  ],
  "stars": [
    { "ra_deg": 18.174019, "dec_deg": 44.505004, "magnitude_class": 4 },
    { "ra_deg": 335.04326, "dec_deg": -0.4632674, "magnitude_class": 4 }
  ]
}
```

The full document is committed at
[`book/src/gallery/scene-neighbors-seed-42.json`](../gallery/scene-neighbors-seed-42.json).

## Derived vs drawn vs recomputed

- **`neighbors` is drawn, ledger-backed physics.** `index`, `class_name`,
  `color`, `distance_ly`, and the RA/dec pair are all the generator's own
  drawn or derived values, surfaced unchanged — each also lives in the
  ledger as `is-neighbor`, `neighbor-class`, `neighbor-color` (see
  `facts.rs`), `neighbor-distance-ly`, `neighbor-ra-deg`, and
  `neighbor-declination-deg` facts. `brightness_rel` is **derived**: L/d²
  from the neighbor's drawn luminosity and drawn distance — not itself a
  separate draw.
- **`stars` is derived texture, not a ledger product.** Every field star
  is recomputed on demand from one seeded stream
  (`world.seed.derive(ASTRONOMY_STREAM_ROOT).derive(streams::STARFIELD)`)
  in fixed order: a count, then per-star declination (sphere-uniform),
  right ascension (uniform), and a magnitude-class roll (dim-heavy). It
  consumes no genesis draws and commits no fact — regenerating this
  document for the same world always reproduces the same 100–300 stars,
  but they were never part of the world's genesis ledger to begin with.
  This is the *exact same population* `figures()` (the sky's notable-shape
  clustering, which merges neighbors and starfield into one bright-star
  catalog) and the almanac's night-sky chart both already draw from — this
  document is the first place that population is serialized rather than
  recomputed per consumer.

A consumer that conflates the two — treating a field star's position as if
it carried the same evidentiary weight as a neighbor's drawn distance or
class — has misread the document: field stars are furniture: real from the
same physics, present in every rendering, but with no identity of their
own beyond a position and a brightness tier.

## The frame convention (normative)

`ra_deg` and `dec_deg`, on both `neighbors` and `stars`, are the world's
**genesis-epoch equatorial coordinates** — exactly the values of the
`neighbor-ra-deg` / `neighbor-declination-deg` ledger facts for neighbors,
and the same convention for field stars. The celestial equator is the
**anchor world's rotational equator**; declination is measured from it.

**Precession is deliberately not exported in v1.** The domain has a
precession-drift reader (`star_equatorial_at`, keyed to `WorldTime`), but
this schema exports only the genesis-epoch snapshot — the Orrery scrubs
human timescales, over which real-world precession (~21 kyr period) is
invisible, so exporting a fixed epoch costs nothing observable while
keeping the document simple. A future schema version may export drifted
coordinates; `v1` does not.

## The consumer transform (normative)

This is the exact statement the Orrery unit-tests against — state it
precisely, because a consumer that gets the rotation axis or direction
wrong will silently misplace every star in the sky.

Build the unit vector

```
(cos(dec) · cos(ra), sin(dec), cos(dec) · sin(ra))
```

in a **y-up equatorial frame** (x toward RA 0/dec 0, y toward the north
celestial pole, z completing the right-handed frame). Rotate this vector
about the **x-axis** — the vernal-equinox direction, RA 0 — by the world's
`obliquity_deg` (carried by
[`scene/system/v1`](./scene-system-v1.md#the-document)). The result is the
**y-up ecliptic scene frame**, the same frame the anchor world's orbit and
moon orbits already live in (orbits in the xz plane).

Two pinned cases fix the rotation's sign and axis unambiguously:

- A star at **dec = 0, ra = 0** sits on the x-axis before rotation, and
  the x-axis is the rotation axis, so it is unchanged by the transform:
  it lands on the vernal-equinox axis in both frames.
- A star at **dec = +90°** (any RA — the pole is degenerate in RA) starts
  at `(0, 1, 0)` in the equatorial frame. Rotating by obliquity `ε` about
  x carries it to `(0, cos ε, sin ε)` — the world's own **spin axis** in
  the ecliptic scene frame, tilted away from ecliptic-north by exactly the
  world's obliquity, which is the whole physical point of the transform:
  the north celestial pole is not the ecliptic pole except on an
  unobliqued world.

## The u64 seed caveat

`seed` is a u64. JavaScript's `JSON.parse` reads all numbers as IEEE-754
doubles, which lose integer precision above 2^53 (~9 × 10^15) — most
Hornvale seeds are small enough to round-trip safely, but a consumer that
needs the *exact* seed value (rather than treating it as an opaque label)
should parse this field with a BigInt-aware JSON reader rather than
trusting plain `JSON.parse`.

## Stability

`scene/neighbors/v1` is a save-format-class contract, held to the same
discipline as `scene/system/v1` and `scene/moons/v1`:

- Adding a new field stays within `scene/neighbors/v1` and appends after
  every existing field; existing consumers that read fields by name are
  unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place — that mints `scene/neighbors/v2` alongside `v1`.
- Exporting precession-drifted coordinates, or any other change to the
  frame convention above, is a `v2` change, not a `v1` amendment — the
  frame convention is as normative as any field's type.

The committed example,
[`book/src/gallery/scene-neighbors-seed-42.json`](../gallery/scene-neighbors-seed-42.json),
is generated by `hornvale scene neighbors --world <seed-42 generated-sky
world>` and drift-checked in CI — regenerating it from the same seed and
pins always reproduces the same bytes, because every field routes through
either a pure ledger read, the L/d² derivation, or the seeded starfield
hash, none of which touches wall-clock time or platform-dependent floating
point beyond the quantization boundary (decision 0033).

## Getting one

```
hornvale scene neighbors [--world <PATH>]
```

This prints one `scene/neighbors/v1` document to standard output. `--world`
defaults to `world.json`. A world with no generated sky (the tier-0
constant sun) has no neighborhood or starfield to describe, and the
command fails with a message saying so.
