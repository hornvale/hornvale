# Scene Schema: moons v1

`scene/moons/v1` is each moon's surface: real formation and density, derived
radius and gravity, and four descriptive terrain properties, one entry per
moon on a world with a generated sky. It exists because
[`scene/system/v1`](./scene-system-v1.md) stops at orbital elements — mass,
period, distance — and says nothing about what the moon *looks like*. This
page is that missing surface.

State the boundary up front, because it drives every field below: the
generator draws a moon's **mass**, **orbit**, and — since The Reckoning —
its **formation mechanism** (`GiantImpact` or `Capture`) as physics, each a
`Stream` draw recorded in the ledger. The formation mechanism fixes the
moon's **bulk density**: a `GiantImpact` moon's density is the *derived*
constant 3.34 g/cm³ (re-accreted mantle debris, no iron core); a `Capture`
moon's density is *drawn* from a different reservoir (rocky 3.0 or icy
1.6 g/cm³). `radius_km` and `surface_gravity_ms2` are *derived* from the
drawn mass and this real density (§3.2) — no assumption. The other four
fields — `albedo`, `cratering`, `maria_fraction`, `tint` — remain *authored
procedural detail*: a deterministic hash of the seed and the moon's index,
biased by mass (and, for `albedo`, also by composition) so a face reads
plausibly, but never measured or simulated (§3.3). A consumer that conflates
the two — treating a hash-noise cratering value as if it came from the same
evidentiary chain as the drawn mass or density — has misread the document.

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
| `radius_km` | number | **Derived.** Physical radius, km, from mass and the moon's real bulk density. See §3.2. |
| `surface_gravity_ms2` | number | **Derived.** Surface gravity, m/s², from that radius and the same real density. See §3.2. |
| `albedo` | number | **Seeded.** Reflectance in [0.04, 0.7]; composition-biased (bright for icy, dark for rocky), darkened where `maria_fraction` is high. See §3.3. |
| `cratering` | number | **Seeded.** Cratering intensity in [0, 1]; biased high for small moons. See §3.3. |
| `maria_fraction` | number | **Seeded.** Smooth resurfaced-plains fraction in [0, 1]; biased high for large moons, composition-damped (not zeroed) for an icy moon — maria are basaltic, which an ice body does not have. See §3.3. |
| `tint` | array of 3 numbers | **Seeded.** Near-gray linear-RGB tint, each channel in [0, 1] — deliberately subtle (moons are gray), enough to tell two moons of a world apart. See §3.3. |
| `surface_class` | string | **Derived** from bulk density and the seeded descriptors above by the normative classifier in §3.4. |
| `density_g_cm3` | number | **Derived boundary: drawn/derived, mechanism-dependent (The Reckoning).** Bulk density, g/cm³ — 3.34 for a `GiantImpact` moon (derived), one of {3.0 rocky, 1.6 icy} for a `Capture` moon (drawn). The physical basis for `radius_km`, `surface_gravity_ms2`, and `surface_class`'s `bright-icy` branch. See §3.2. |
| `formation` | string | **Derived boundary: drawn (The Reckoning).** How this moon formed: `"giant-impact"` or `"capture"` — the stable text form of `hornvale_astronomy::Formation`. |

An excerpt of a `scene/moons/v1` document (seed 42; two moons — the same
world `scene/system/v1`'s excerpt uses):

```json
{
  "schema": "scene/moons/v1",
  "seed": 42,
  "moons": [
    { "index": 0, "mass_rel": 2.2430426, "radius_km": 2274.7776, "surface_gravity_ms2": 2.1241234, "albedo": 0.084477593, "cratering": 0.29697289, "maria_fraction": 0.69371891, "tint": [0.75070696, 0.68400633, 0.68077909], "surface_class": "maria-rich", "density_g_cm3": 3.34, "formation": "giant-impact" },
    { "index": 1, "mass_rel": 0.69991131, "radius_km": 1599.1254, "surface_gravity_ms2": 1.3412141, "albedo": 0.14140231, "cratering": 0.73712226, "maria_fraction": 0.19465874, "tint": [0.72242237, 0.67958543, 0.69486849], "surface_class": "heavily-cratered", "density_g_cm3": 3.0, "formation": "capture" }
  ]
}
```

## §3.2 Derived physics: radius, gravity, and density

`radius_km` and `surface_gravity_ms2` come from `mass_rel` and the moon's
**real bulk density** (`density_g_cm3`) — no assumption. Density is
mechanism-dependent (The Reckoning, `hornvale_astronomy::Formation`): a
`GiantImpact` moon's density is the *derived* constant 3.34 g/cm³
(re-accreted mantle debris, no iron core — exactly why Luna is 3.34 against
Earth's 5.51); a `Capture` moon's density is *drawn* from a different
reservoir entirely (rocky 3.0 or icy 1.6 g/cm³ — a different formation
history than the anchor's own mantle). Radius follows the standard
uniform-sphere relation, and surface gravity substitutes that same relation
into `g = GM/r²`:

```
radius_km            = (3 · mass_kg / (4π · density_kg_per_m3))^(1/3), converted to km
surface_gravity_ms2  = (4/3)π · G · density_kg_per_m3 · radius_m
```

(`mass_kg` and `density_kg_per_m3` are `mass_rel`/`density_g_cm3` converted
to SI; `G` is the Newtonian gravitational constant.) `radius_km` calls
`hornvale_astronomy::radius_km` directly — this crate does not re-derive
that formula. `surface_gravity_ms2` composes the gravitational constant
with that radius and the same density, which is algebraically equivalent to
`GM/r²` but needs no mass-in-kg term (mass cancels: `M = (4/3)πρr³`). The
radius's cube root routes through the kernel's libm-routed `powf`; the
gravity step is plain multiplication (no transcendental) — so both fields
stay cross-platform byte-identical (decision 0041) and safe to keep in the
strict drift check alongside the seeded fields.

**A `GiantImpact` moon at 1.0 lunar mass reproduces Luna almost exactly**:
2274.7776 km at seed 42's 2.243 lunar masses works out to 1737.77 km at
unit mass — 0.37 km (0.02%) above the old assumed-constant anchor of
1737.4 km, because 1737.4 *was* Luna's real observed radius, and 1737.77 is
what the same real mass (7.342 × 10²² kg) and density (3.34 g/cm³) yield
when run through the physics instead of being looked up. Surface gravity
comes out to ≈1.6226 m/s² against the old anchor's 1.62 — the same order of
agreement. **A `Capture` moon can diverge sharply**: at the same mass, an
icy composition (1.6 g/cm³) is `(3.34 / 1.6)^(1/3) ≈ 1.28` times larger in
radius than a `GiantImpact` moon would be — a ~28% difference the old
constant-density formula could never show, because it assumed every moon
was Luna's density regardless of formation.

## §3.3 Seeded surface descriptors: hash noise, not simulation

The four descriptors here are not authored the same way, and the
difference matters for a consumer deciding how much to trust a value:

- `cratering`, `maria_fraction`, and `tint` are **authored procedural
  detail, not simulated surface science**: each is a deterministic hash of
  the world's seed and the moon's index — the same value every time the
  document is regenerated for that world — biased by `mass_rel` so a face
  reads plausibly, and backed by no `Stream` draw of its own.
- `albedo` is **part real, part authored**: its *baseline band* — bright
  ice (0.5–0.7) vs. dark rock (0.1–0.2) — is composition-derived, reading
  the moon's real `density_g_cm3` via `hornvale_astronomy::is_icy`, which
  is itself Stream-backed two steps upstream through `formation` (§3.2).
  Only the *position within that band* is the seed/index hash, and the
  maria-darkening multiplier applied on top is authored bias like the
  other three, not measurement.
- `density_g_cm3`, `radius_km`, `surface_gravity_ms2`, and `formation`
  are real derived or drawn physics, start to finish — not authored at
  all. See §3.2.

Nothing in the hash channel consumes randomness from the world's own draw
sequence — it reads `(seed, index)` directly, not a `Stream`. But two
worlds sharing every drawn quantity — including `formation` and
`density_g_cm3`, both of which **are** drawn or mechanism-derived (§3.2),
contrary to what a skim of this page might otherwise suggest — still
differ in `cratering`/`maria_fraction`/`tint` and in albedo's in-band
position only if their seeds differ; they would differ in albedo's *band*
only if `formation` or `density_g_cm3` differ.

The hash is biased by `mass_rel` so a face reads plausibly, in the same
spirit that a settlement's name reads plausibly without being a linguistic
simulation ("models author, dice roll" — decision 0009): small moons bias
toward cratered highlands (`cratering` pulled up as mass falls); large
moons bias toward resurfaced maria plains (`maria_fraction` pulled up as
mass rises, then damped where `cratering` is already high, so a face is
never simultaneously all-craters and all-maria). For an icy moon,
`maria_fraction` is damped a second time, by a fixed 0.3 multiplier
(review follow-up to The Reckoning): maria are flood basalts, which an ice
body does not have, so a composition-blind fraction let an icy moon read
as basaltic plains on an ice ball. The damping is deliberately not a hard
zero — real icy bodies do carry resurfaced terrain (Europa's chaos
terrain, Enceladus's tiger stripes), just not basaltic maria — so a
reduced residual stays reachable. `albedo` starts from its composition
baseline (above), with the hash perturbing within that band, then darkens
further where the (possibly icy-damped) `maria_fraction` is high, since
maria are conventionally dark plains. The mass bias on
`cratering`/`maria_fraction`, the icy damping factor, and the hash
perturbation within a band all remain narrative plausibility, not
measurement — nothing enforces that a massive moon in some other physical
model would in fact be maria-rich, or that a real icy moon's resurfaced
fraction would land at exactly this damped value.

## §3.4 `surface_class`: the normative classifier

`surface_class` is a stable, append-only word for a consumer that wants a
name rather than a texture — derived from real `density_g_cm3` and the two
seeded descriptors `cratering`/`maria_fraction` by the following normative
table, checked in this order (most specific first):

| Condition | `surface_class` |
|---|---|
| `density_g_cm3 < 2.0` | `bright-icy` |
| else `maria_fraction > 0.4` | `maria-rich` |
| else `cratering > 0.6` | `heavily-cratered` |
| else (none of the above) | `cratered-highland` |

**Since The Reckoning, `bright-icy` keys off real bulk density, not the
seeded `albedo`.** Before density was real, `albedo > 0.4` was a hash-noise
threshold with no physical referent — The Faces shipped the *word* for an
icy moon while the model had no concept of ice. `2.0` g/cm³ sits with wide
margin between the domain's three density constants: icy captures draw
1.6, rocky captures 3.0, giant-impact moons 3.34, so a drawn value can never
land near the line. Density wins the precedence check even under high
`cratering`/`maria_fraction` — an icy moon is `bright-icy` regardless of its
other descriptors.

The set of class **names** is append-only: no class is ever removed or
renamed — a client's `if surface_class == "maria-rich"` stays correct
across regenerations, and a new class may join the table in the future
without touching the rows above it. Adding a class also never re-meanings
the fallback: `cratered-highland` stays the catch-all it is today.

A class's **condition** is a narrower promise than its name. It holds
across ordinary regenerations, with one disclosed exception to date:
`bright-icy`'s condition was re-based, by The Reckoning, from `albedo >
0.4` to `density_g_cm3 < 2.0`. That is not a routine tuning change — it is
the one time this schema has given a class the physical referent it was
missing at introduction. Before density was real, `albedo > 0.4` was
hash noise standing in for a concept (ice) the model could not yet
represent; the class name shipped ahead of anything that could back it.
Once density existed, the condition moved onto it because the old
condition was never actually answering the question `bright-icy` asked.
Barring another campaign supplying a referent a class is missing in the
same way, a condition does not move underneath a consumer once it has one
— this is a disclosed, one-time exception, not an open door.

## Stability

`scene/moons/v1` is a save-format-class contract, held to the same
discipline as `scene/system/v1` and `scene/tiles/v1`:

- Adding a new field stays within `scene/moons/v1` and appends after every
  existing field; existing consumers that read fields by name are
  unaffected.
- Changing an existing field's meaning, order, or type never happens in
  place — that mints `scene/moons/v2` alongside `v1`.
- The `surface_class` table (§3.4) is append-only in the name sense: a
  class is never removed or renamed. A class's condition is stable across
  ordinary regenerations, with one disclosed exception to date —
  `bright-icy`'s condition was re-based from `albedo > 0.4` to
  `density_g_cm3 < 2.0` by The Reckoning, the one time this schema has
  given a class the physical referent it was missing at introduction (see
  §3.4). Absent that kind of referent-supplying campaign, no existing
  row's condition or output name changes underneath a consumer.

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
