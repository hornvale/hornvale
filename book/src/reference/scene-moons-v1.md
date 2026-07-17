# Scene Schema: moons v1

`scene/moons/v1` is each moon's surface: derived radius and gravity, and
four descriptive terrain properties, one entry per moon on a world with a
generated sky. It exists because [`scene/system/v1`](./scene-system-v1.md)
stops at orbital elements — mass, period, distance — and says nothing about
what the moon *looks like*. This page is that missing surface.

State the boundary up front, because it drives every field below: the
generator draws a moon's **mass** and **orbit** as physics (a `Stream` draw,
recorded in the ledger), but it has **no surface model** — no simulated
geology, no cratering physics, no albedo measurement. `radius_km` and
`surface_gravity_ms2` are *derived* from that drawn mass under a
constant-density assumption (§3.2). The other four fields — `albedo`,
`cratering`, `maria_fraction`, `tint` — are *authored procedural detail*:
a deterministic hash of the seed and the moon's index, biased by mass so a
face reads plausibly, but never measured or simulated (§3.3). A consumer
that conflates the two — treating a hash-noise cratering value as if it came
from the same evidentiary chain as the drawn mass — has misread the
document.

Only a world with a **generated** sky has moons to describe. Tier-0
constant-sun worlds have no orrery and no moons; asking one for
`scene/moons/v1` fails with a description of why — mirroring
`scene/system/v1`'s same refusal.

## The document

Every `scene/moons/v1` document is one JSON object with these fields, in
this order (field order is part of the contract — it is fixed, not
incidental):

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always the literal `"scene/moons/v1"` — the version tag a consumer checks before trusting the rest of the document. |
| `seed` | integer | The world's seed. This is a u64; JavaScript consumers parsing the document with plain `JSON.parse` lose integer precision above 2^53, so use BigInt-aware parsing when the exact seed matters. |
| `moons` | array of object | Every moon's surface, in generation order — the same order as [`scene/system/v1`'s `moons` array](./scene-system-v1.md#the-document), so index `i` here describes the same moon as index `i` there. |

Each entry in `moons` is:

| Field | Type | Meaning |
|---|---|---|
| `index` | integer | The moon's generation index — matches the corresponding entry's position in `scene/system/v1`'s `moons` array. |
| `mass_rel` | number | **Derived boundary: drawn.** Mass in lunar masses — the generator's drawn value, surfaced unchanged. |
| `radius_km` | number | **Derived.** Physical radius, km, from mass at an assumed constant lunar density. See §3.2. |
| `surface_gravity_ms2` | number | **Derived.** Surface gravity, m/s², from mass at the same constant-density assumption. See §3.2. |
| `albedo` | number | **Seeded.** Reflectance in [0.04, 0.5]; darkened where `maria_fraction` is high. See §3.3. |
| `cratering` | number | **Seeded.** Cratering intensity in [0, 1]; biased high for small moons. See §3.3. |
| `maria_fraction` | number | **Seeded.** Smooth resurfaced-plains fraction in [0, 1]; biased high for large moons. See §3.3. |
| `tint` | array of 3 numbers | **Seeded.** Near-gray linear-RGB tint, each channel in [0, 1] — deliberately subtle (moons are gray), enough to tell two moons of a world apart. See §3.3. |
| `surface_class` | string | **Derived** from the three seeded descriptors above by the normative classifier in §3.4. |

An excerpt of a `scene/moons/v1` document (seed 42; two moons — the same
world `scene/system/v1`'s excerpt uses):

```json
{
  "schema": "scene/moons/v1",
  "seed": 42,
  "moons": [
    { "index": 0, "mass_rel": 2.2430426, "radius_km": 2274.289, "surface_gravity_ms2": 2.1206102, "albedo": 0.14341409, "cratering": 0.29697289, "maria_fraction": 0.69371891, "tint": [0.75070696, 0.68400633, 0.68077909], "surface_class": "maria-rich" },
    { "index": 1, "mass_rel": 0.69991131, "radius_km": 1542.5793, "surface_gravity_ms2": 1.4383437, "albedo": 0.27950461, "cratering": 0.73712226, "maria_fraction": 0.19465874, "tint": [0.72242237, 0.67958543, 0.69486849], "surface_class": "heavily-cratered" }
  ]
}
```

## §3.2 Derived physics: radius and gravity

`radius_km` and `surface_gravity_ms2` come from `mass_rel` under one
assumption: **constant density, equal to Luna's** (3.34 g/cm³). Volume is
proportional to mass at fixed density, so radius scales as the cube root of
mass; surface gravity, `g = GM/r²`, reduces along the same substitution to
the same cube-root scaling:

```
radius_km            = 1737.4 × mass_rel^(1/3)
surface_gravity_ms2  = 1.62   × mass_rel^(1/3)
```

`1737.4` and `1.62` are Luna's own radius (km) and surface gravity (m/s²) —
the reference anchor. The cube root routes through the kernel's libm-routed
`powf`, so these two fields are cross-platform byte-identical (decision
0041) and safe to keep in the strict drift check alongside the seeded
fields.

**Caveat:** constant density is a simplification, not a measurement. A real
moon's density depends on its composition — an icy moon is markedly less
dense than a rocky one at the same mass, and `radius_km` for such a moon
would be a real physical value, not this one. A differentiated ice/rock
composition model would refine these two fields. When it lands, it arrives
as a new *drawn* field (a genuine `Stream` draw for bulk composition,
recorded in the ledger like mass and orbit already are) — never a silent
re-derivation that changes what `radius_km` means without changing its
name.

## §3.3 Seeded surface descriptors: hash noise, not simulation

`albedo`, `cratering`, `maria_fraction`, and `tint` are **authored
procedural detail, not simulated surface science**. Each is a deterministic
hash of the world's seed and the moon's index — the same value every time
the document is regenerated for that world, but not derived from any
physical process, and not backed by a `Stream` draw. Nothing about
generating them consumes randomness from the world's draw sequence; two
worlds could share every drawn quantity (mass, orbit) and still differ here
only if their seeds differ, because the hash reads the seed directly.

The hash is biased by `mass_rel` so a face reads plausibly, in the same
spirit that a settlement's name reads plausibly without being a linguistic
simulation ("models author, dice roll" — decision 0009): small moons bias
toward cratered highlands (`cratering` pulled up as mass falls); large
moons bias toward resurfaced maria plains (`maria_fraction` pulled up as
mass rises, then damped where `cratering` is already high, so a face is
never simultaneously all-craters and all-maria); `albedo` darkens where
`maria_fraction` is high, since maria are conventionally dark plains. This
bias is narrative plausibility, not measurement — nothing enforces that a
massive moon in some other physical model would in fact be maria-rich.

## §3.4 `surface_class`: the normative classifier

`surface_class` is a stable, append-only word for a consumer that wants a
name rather than a texture — derived purely from the three seeded
descriptors above by the following normative table, checked in this order
(most specific first):

| Condition | `surface_class` |
|---|---|
| `albedo > 0.4` | `bright-icy` |
| else `maria_fraction > 0.4` | `maria-rich` |
| else `cratering > 0.6` | `heavily-cratered` |
| else (none of the above) | `cratered-highland` |

The set of class names is append-only: a new class may join the table in
the future, but an existing class's name and the condition that produces it
never change — a client's `if surface_class == "maria-rich"` stays correct
across regenerations. Adding a class also never re-meanings the fallback:
`cratered-highland` stays the catch-all it is today.

## Stability

`scene/moons/v1` is a save-format-class contract, held to the same
discipline as `scene/system/v1` and `scene/tiles/v1`:

- Adding a new field stays within `scene/moons/v1` and appends after every
  existing field; existing consumers that read fields by name are
  unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place — that mints `scene/moons/v2` alongside `v1`.
- The `surface_class` table (§3.4) is append-only in the same sense: new
  rows may be added, but no existing row's condition or output name
  changes underneath a consumer.

The committed example, `book/src/gallery/scene-moons-seed-42.json`, is
generated by `hornvale scene moons --world <seed-42 generated-sky world>`
and drift-checked in CI — regenerating it from the same seed and pins
always reproduces the same bytes, because the derived fields route through
libm-quantized `powf` and the seeded fields are a pure hash, neither of
which touches wall-clock time or platform-dependent floating point beyond
the quantization boundary (decision 0033).

## Getting one

```
hornvale scene moons [--world <PATH>]
```

This prints one `scene/moons/v1` document to standard output. `--world`
defaults to `world.json`. A world with no generated sky (the tier-0
constant sun) has no moons to describe, and the command fails with a
message saying so.
