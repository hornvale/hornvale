# Scene Schema: astronomy-at v1

`scene/astronomy-at/v1` is an evaluated, native scientific observation at one
exact instant. It is separate from the unchanged [element catalog](scene-system-v1.md).
The producer is `windows/scene`, using the astronomy domain's ephemeris and
calendar; clients do not integrate another orbit or infer an absent body size.

```sh
hornvale scene astronomy-at --world world.json --ticks 1200
```

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | `scene/astronomy-at/v1` |
| `seed` | u64 | World seed; use integer-preserving parsing |
| `ticks` | i64 | Exact requested native tick, including negative instants |
| `ticks_per_std_day` | i64 | 100,000 ticks per standard day |
| `frame` | string | `anchor-centered-system-plane/km/right-handed` |
| `models` | array of string | Ordered model/version and validity disclosures |
| `bodies` | array of object | Anchor, stars, moons and wanderers in native order |
| `lights` | array of object | One source contribution per star |

Each body carries `id`, `kind`, `position_km`, `radius_km` and `body_to_frame`.
IDs are `anchor`, `star:N`, `moon:N`, and `wanderer:N`, stable within the bound
world and source revision. They are not global identities across worlds.
Positions are three finite kilometre coordinates relative to the anchor.
The system plane has +z orbital north; +x is phase zero and +y a quarter turn
forward. The anchor is exactly `[0,0,0]`.

`radius_km` is a physical radius for anchor/moons and explicit JSON `null` for
stars/wanderers. Missing size means point presentation only. `body_to_frame` is
a three-column, right-handed orthonormal basis for the anchor, taking body-local
vectors into the system frame. It is explicit `null` for the other bodies;
no physical moon spin is supplied. A static cosmetic moon orientation does not
become an observation. Numeric geometry is quantized to eight significant digits
only at serialization; tick identity remains exact.

A light carries `star_id`, unit `direction_from_anchor`, `flux_rel` (Earth-relative
inverse-square irradiance) and `luminosity_rel` (solar-relative luminosity at the
requested instant). Direction is from anchor toward the star. Illumination does
not include attenuation, occultation or finite-disc shadows.

## Model and validity limits

Stellar and wanderer orbits reuse the native circular/coplanar model. Wide
binaries are circumprimary; close binaries are barycentric. Moon geometry uses
the existing synodic longitude, inclination and regressing node. A moon without
a finite recurrence cannot be evaluated by this surface and returns an error.
Anchor orientation reconciles the exact calendar equatorial vector with the
system frame using `Rz(pi) Rx(-obliquity)`, including retrograde and locked spin.

The spherical anchor radius uses the declared
`earthlike-rocky-zeng2019-linear/v1` interpolation: 32.5% iron and 67.5% MgSiO3,
restricted to 0.5–2 Earth masses. This is a mass–radius observation, not simulated
interior composition. Moon radius uses the existing native density model. These
radii do not rescale the terrain's angular, river or movement contracts.

Linear stellar brightening must yield finite, positive luminosity and flux;
unusable instants fail explicitly. Bad source data, out-of-range physical
models and unsupported observations do not yield zero-filled geometry. A
successful document is not eclipse qualification or a resolved moon terrain.

## Binding and the visual mirror

The [native visual source](../clients/planetarium.md) wraps this document in
`visual/reply/v1`, adding source ID, scope ID, world-byte SHA-256, source revision,
request ID and the exact requested tick. `visual/request/v1` carries the same
binding and request identity. `visual/initial/v1` supplies that binding plus
static `scene/system/v1`, `scene/moons/v1`, `scene/tiles/v1` and tick units.
A mismatched or stale reply cannot replace the accepted snapshot.

The current view validates the fields it consumes. For the static system catalog
that means schema/seed, world and primary-star object shape, supported topology,
companion presence and star/orbit object shape, moon/wanderer object inventories,
and moon count/index correspondence. Moon radius/albedo/cratering/maria/tint and
tile dimensions, datum, elevation, ocean, biome/legend, snow, cloud, moisture and
mean-temperature arrays receive typed/range/length checks. The system's orbital
elements, stellar class/luminosity and world-calendar fields remain provenance;
the view neither evaluates nor exhaustively validates them. Additional native
fields may remain unconsumed. Omission from this mirror says nothing about what
the authoritative producer supplies.

Dynamic replies validate frame/units, finite geometry, explicit nullable fields,
body kinds/IDs, right-handed bases, and complete stellar light inventory against
the initial world. Client camera/geometry limits are separate from native model
validity. Existing fields retain their meaning/type/order; additive fields may
extend v1, while breaking changes require a new schema version.
