# The Faces — Design

*Moons deserve faces* (hornvale#4). `scene/system/v1` gives each moon orbital
elements only — `sidereal_days`, `distance_mm`, `size_rel`, `phase_offset` —
and the orrery draws them as flat gray spheres. This campaign gives each moon a
face: a per-moon surface document a client can turn into a texture.

The honest boundary is the whole design. The generator knows a moon's **mass**
(drawn, lunar masses) and its orbital geometry; it has **no surface model** —
nothing about composition, cratering, maria, or albedo, and it does not even
compute a physical radius (only apparent size). So the surface document splits
cleanly into two halves, and says which is which: **derived** physical
quantities that follow from the known mass by real physics, and **seeded
procedural** descriptors that have no generator source and are authored by a
deterministic hash, biased by mass so they read plausibly. There is no
elevation lattice: the generator has no moon terrain, and a fabricated
heightfield would be invented precision of exactly the kind The Isotherm and
The Region refused.

## 1. Goal

Ship `scene/moons/v1`: a deterministic, versioned per-moon surface document
(derived physical params + seeded procedural descriptors), a `world-wasm-v4`
export, a reference page + golden, and an orrery consumer that tints/textures
the moons from it. No new seed stream, no invented terrain.

## 2. What the generator knows (the honest boundary)

Per moon, from `hornvale_astronomy::Moon` (via `sky_of(world).system().moons`):
`mass: LunarMasses` (drawn 0.05–2.5), `distance`, `period`,
`angular_diameter_rel`, `tide_rel`, `inclination_deg`, `node_longitude_deg`.
**Mass is the only physical handle on the body itself.** Everything the surface
document derives (radius, gravity) comes from mass; everything it cannot derive
(albedo, cratering, maria, tint) is authored procedurally and labelled as such.

## 3. The contract — `scene/moons/v1` (a new schema)

A per-moon surface document, versioned and additive like the other scene
schemas (decision 0055; the scene-tiles-v1 Stability discipline). One JSON
object, field order fixed:

| Field | Type | Meaning |
|---|---|---|
| `schema` | string | Always `"scene/moons/v1"`. |
| `seed` | integer | The world's seed (u64; BigInt-aware parsing above 2^53). |
| `moons` | array of object | One entry per moon, in generation order (the same order as `scene/system/v1`'s `moons`). |

Each `moons` entry, field order fixed:

| Field | Type | Half | Meaning |
|---|---|---|---|
| `index` | integer | — | The moon's generation index (matches `scene/system/v1`). |
| `mass_rel` | number | derived | Mass in lunar masses (the generator's drawn value, surfaced). |
| `radius_km` | number | **derived** | Physical radius, km — from mass at an assumed constant lunar density (§3.2). |
| `surface_gravity_ms2` | number | **derived** | Surface gravity, m/s² — `1.62 × mass^(1/3)` (§3.2). |
| `albedo` | number | **seeded** | Bond-albedo-like reflectance in [0.04, 0.5], hash-seeded, darkened where maria is high (§3.3). |
| `cratering` | number | **seeded** | Cratering intensity in [0, 1], hash-seeded, biased **high** for small moons (§3.3). |
| `maria_fraction` | number | **seeded** | Fraction of the face that is smooth maria in [0, 1], hash-seeded, biased **high** for large moons (§3.3). |
| `tint` | array of 3 number | **seeded** | A subtle linear-RGB tint in [0, 1]³ near-gray, hash-seeded (§3.3). |
| `surface_class` | string | derived | A descriptive class name computed from the descriptors (§3.4), for clients that want a word not a texture. |

### 3.2 Derived physical params (honest physics)

Both derive from mass alone, at an **assumed constant density equal to Luna's**
(3.34 g/cm³) — stated as an assumption, not a measurement, because the
generator draws no composition. Reference values are Luna's: radius 1737.4 km,
surface gravity 1.62 m/s², mass 1 lunar mass.

- `radius_km = 1737.4 × mass_rel^(1/3)` (constant density ⇒ volume ∝ mass ⇒
  radius ∝ mass^(1/3)).
- `surface_gravity_ms2 = 1.62 × mass_rel^(1/3)` (`g = GM/r²`, and with
  `r ∝ mass^(1/3)` this reduces to `g ∝ mass^(1/3)`).

The page states the constant-density caveat plainly: a differentiated
ice/rock model would refine these, and would arrive as a new drawn
composition field, never a silent re-meaning.

### 3.3 Seeded procedural descriptors (authored, not simulated)

These have **no generator source**. They are a pure function of the world seed
and the moon index via the kernel's hash-noise (`value_noise_2d` /
`fbm_2d`) — **no `Stream` draw, no draw-order effect, no new save-format
stream** (the render-lens "hash-noise only" precedent,
`domains/terrain` streams). Determinism holds by the seed alone; the document
is quantized at emit (decision 0033).

Mass biases each so the face reads plausibly — the physical intuition that
large bodies differentiate and resurface (maria) while small bodies stay
primitive and saturated with craters ("models author, dice roll", decision
0009):

- `cratering` = hash channel, pulled **toward 1** as mass falls (small moons
  are cratered highlands).
- `maria_fraction` = hash channel, pulled **toward higher** as mass rises
  (large moons have volcanic plains); mutually damped against `cratering` so a
  face is not simultaneously all-maria and all-craters.
- `albedo` = hash channel in [0.04, 0.5], reduced where `maria_fraction` is
  high (maria are dark).
- `tint` = three near-gray hash channels; deliberately subtle (moons are
  gray), enough to distinguish two moons of a world.

The exact bias curves are a producer detail (§4), pinned by the golden. The
contract is: deterministic from the seed, quantized, and **labelled procedural**
in the page so no client mistakes them for measured surface science.

### 3.4 `surface_class` (a derived word)

A small, stable classifier over the descriptors for clients that want a name,
not a texture — e.g. `maria_fraction > 0.4 → "maria-rich"`, else
`cratering > 0.6 → "heavily-cratered"`, else `"cratered-highland"`; an icy
high-albedo case (`albedo > 0.4 → "bright-icy"`). The exact thresholds live in
the reference page as a normative table; the set is append-only (a new class
name never re-meanings an old one).

## 4. Producer implementation (hornvale)

- **`domains/astronomy`**: no physics change, no new stream. If a radius/gravity
  helper on `Moon` is worth sharing (`Moon::radius_km()`, `surface_gravity_ms2()`),
  it lands there as a pure derivation from `mass`; otherwise it stays private to
  the scene builder. The hash-noise descriptors are scene-side (presentation),
  not astronomy state.
- **`windows/scene`**: a new `MoonsScene` struct + `moons_scene(world) -> Result<MoonsScene, SceneError>`, mirroring `system_scene` (which already reads `system.moons`). Errors when the world has no generated sky (the constant-sun tier has no moons) — the `system_scene` precedent. Quantized serde on all f64 fields. Descriptors via `hornvale_kernel::value_noise_2d(world.seed, moon_index, channel)`.
- **`clients/world-wasm`**: a new export `hw_scene_moons() -> i32` (0 ok; 2 scene error; -3 no world), mirroring `hw_scene_system`. A **`world-wasm-v4`** release at close (decision 0055; Nathan authorizes the tag push).
- **CLI**: `hornvale scene moons [--world <path>]`, mirroring `scene system`.

## 5. Documentation

- **`book/src/reference/scene-moons-v1.md` — new page**: the document + entry
  field tables; the derived-vs-seeded split stated up front; the §3.2
  derivations with the constant-density caveat; the §3.3 hash-noise framing
  ("authored procedural detail, not simulated surface science; deterministic
  from the seed; no stream draw"); the §3.4 `surface_class` table; a Stability
  section; the committed seed-42 example's provenance.
- **`book/src/gallery/scene-moons-seed-42.json`** — committed golden,
  drift-checked. Seed 42 has two moons.
- **`scene-system-v1.md`**: a one-line cross-link ("each moon's surface:
  `scene/moons/v1`"). SUMMARY entry; `docs_consistency` covers the links.

## 6. Consumer implementation (orrery)

- **Typed parsing** (`src/sim/scene.ts`): a `MoonsScene` type + strict
  `parseMoons`, mirroring `parseSystem`.
- **Moon rendering** (the tracer feature): the system view's gray moon spheres
  become **shaded by the descriptors** — `albedo` sets base brightness, `tint`
  the hue, and a deterministic procedural texture (seeded from seed+index)
  paints `cratering`/`maria_fraction` as a cratered vs. smooth-maria surface.
  Radius from `radius_km` sets true relative moon sizes. Chosen because it
  exercises every field (derived sizes + all four seeded descriptors) in
  production posture, not just a parse.
- **Re-pin**: `CATALOG_VERSION` → `world-wasm-v4`; vendored wasm refreshed in
  the same commit as the parser. (Note: v4 re-pins prior goldens to current
  main, the same catalog-release effect The Region saw with v3 — budget for it.)

## 7. Contract tests

**Producer (hornvale):**
- **Determinism**: same seed → byte-identical document; a rebuilt world reproduces it.
- **Derivation exactness**: `radius_km`/`surface_gravity_ms2` equal the §3.2 formulas over the moons; at `mass_rel = 1` they equal Luna's reference values.
- **Descriptor ranges & bias**: all descriptors in range; over a sample of seeds, small moons read higher `cratering` and large moons higher `maria_fraction` than chance (the mass-bias actually bites) — a calibration-style assertion, not a single-seed fluke.
- **No new stream**: a pin-isolation-style check that building the moons scene consumes **no** `Stream` draws (the descriptors are hash-noise) — the document is a pure read plus hash, so an unpinned world's draw order is unchanged. This is the save-format guard.
- **Sky-tier**: `moons_scene` errors on a constant-sun world.
- **Golden**: committed `scene-moons-seed-42.json`, byte-checked (CI cross-platform handling per the §8 note).

**Consumer (orrery):**
- **The binary is the fixture**: vitest instantiates the vendored `world-wasm-v4`, `hw_new(42)` + `hw_scene_moons`, asserts `parseMoons` accepts the real document (two moons, ranges).
- The moon-shading feature gets unit tests (albedo→brightness, tint, radius→size) + the existing Playwright smoke stays green.

## 8. Consequences / additive discipline

Purely additive: a new schema, a new export, a new CLI subcommand, a new
reference page, one golden. **No `scene/system/v1` change. No new seed stream,
no new concept, no epoch, no census regen** (no worldgen change — the
descriptors are presentation-side hash-noise). `world-wasm` goes v3 → v4. The
golden's CI cross-platform handling follows the scene-tiles precedent
(exclude from the strict diff if its values prove host-libm-sensitive; the
derived params are libm-routed `powf`/`cbrt`, the descriptors are hash-noise —
likely stable, decided during execution by observation).

## 9. Non-goals

- **A moon elevation lattice / heightfield** — invented precision; deferred as
  an explicitly-procedural future tier (idea-registry capture), never claimed
  as simulated terrain.
- **Moon composition / differentiation model** (real ice/rock, varying
  density) — a future *drawn* astronomy field; would refine §3.2 additively.
- **Standing on a moon** (orrery#4 surface view) — a downstream client
  campaign; this ships the data it will need.
- **No new seed stream**; no `scene/system/v1` change.

## 10. Flagged for G3 (autopilot carve-out summary)

1. **Fidelity call (Nathan-decided, ledger #1):** descriptors honest-minimum —
   derived physics from mass + seeded procedural descriptors; no invented
   elevation. The derived/seeded split is stated in the schema and page.
2. **Determinism / save-format:** descriptors are hash-noise (`value_noise_2d`),
   **no `Stream` draw, no new save-format stream, no epoch** — a contract test
   guards zero draw consumption. New schema `scene/moons/v1` alongside
   `system/v1`.
3. **External actions at close (G6):** `world-wasm-v4` tag push + release;
   orrery re-pin (which re-pins prior goldens to current main); ticket close
   (hv#4).
4. **Honesty framing:** the page must label the seeded descriptors as authored
   procedural detail, not measured surface science — the whole point of the
   tier choice.

## 11. Definition of done

Both gates green (hornvale `make gate`; orrery vitest + Playwright); §7 contract
tests on both sides; `scene-moons-v1.md` written and `scene-system-v1.md`
cross-linked; golden committed and drift-checked; `world-wasm-v4` released and
orrery re-pinned (Nathan-authorized at G6); chronicle + retrospective + book
freshness sweep + any Confidence-Gradient re-score per the standard close.
