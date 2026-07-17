# The Real Sky — design

**Date:** 2026-07-17 · **Status:** awaiting G3 review ·
**Registry row:** ORRERY-starfield-backdrop

## 1. Goal

The Orrery's starfield is a lie: `buildStarfield` (orrery
`src/views/system.ts`) scatters `mulberry32`-seeded cosmetic dots on a
shell, while Hornvale draws a real neighbor catalog per world —
class, color, distance, apparent brightness, and equatorial RA/dec
(`domains/astronomy/src/neighborhood.rs`, promoted to first-class ledger
entities by SKY-15) — with no scene export. This campaign ships
`scene/neighbors/v1` (producer + wasm export + goldens + reference page)
and replaces the fake starfield with the world's actual sky. It also
carries one small rider The Reckoning left stranded: `scene/system/v1`
gains each moon's `inclination_deg` and `node_longitude_deg` (additive), so
the Orrery can tilt moon orbits and render seed 42's retrograde captured
moon (inclination 117.277°) honestly, and the moon inspector can say
"capture" once the wasm carries The Reckoning's `formation`/`density_g_cm3`
fields (already in the producer, not yet in any release).

Success criteria: on seed 42 the Orrery shows that world's own neighbors
at their drawn positions, sized/lit by real brightness and tinted by their
real color words; the retrograde moon visibly orbits opposite its sibling;
every artifact byte-identical across regeneration; worlds unchanged
(no new draws, no epoch).

## 2. The contract: `scene/neighbors/v1`

A new sibling of `scene/moons/v1` in `windows/scene` (same document
pattern: `schema`, `seed`, one array in generation order; every f64
quantized at emit, decision 0033).

```
NeighborsScene {
  schema: "scene/neighbors/v1",
  seed: u64,
  neighbors: [NeighborElem, …],  // generation order (brightest first)
  stars: [FieldStarElem, …]      // background starfield, derivation order
}
NeighborElem {
  index: usize,          // generation index (stable identity)
  class_name: String,    // prose spectral class (class_name(class))
  color: String,         // the producer's color word (e.g. "smoldering red")
  distance_ly: f64,
  brightness_rel: f64,   // apparent brightness, L/d² relative units
  ra_deg: f64,           // right ascension, degrees [0, 360)
  dec_deg: f64,          // declination, degrees [-90, +90]
}
FieldStarElem {
  ra_deg: f64,
  dec_deg: f64,
  magnitude_class: u8,   // 1 (brightest) ..= 5 (faintest)
}
```

**Two populations (post-G3 revision, ledger #11, leads the G6 digest):**
the sim's night sky is not only the 2–5 notable neighbors — the domain
also derives a 100–300-star background field
(`hornvale_astronomy::starfield`, seeded from
`world.seed.derive(ASTRONOMY_STREAM_ROOT)`, the exact population
`figures()` clusters and the almanac's chart draws). A neighbors-only
export would render an *emptier* sky than the sim owns; the document
therefore carries both. Field stars are anonymous texture (position +
magnitude class only, the domain's own framing); neighbors are the bright,
colored, class-named objects.

**Frame convention (normative, on the reference page):** RA/dec are the
world's genesis-epoch equatorial coordinates — exactly the values of the
`neighbor-ra-deg` / `neighbor-declination-deg` ledger facts. Precession
drift (`star_equatorial_at`) is deliberately not exported in v1: the
Orrery scrubs human timescales where drift is invisible. A consumer
placing stars in the ecliptic scene frame (orbits in the xz plane) rotates
the (RA, dec) unit vector about the vernal-equinox axis (RA 0) by the
world's `obliquity_deg`, which `scene/system/v1` already carries. The
consumer's transform is unit-tested against pinned cases (dec = +90 lands
on the world's spin axis, a dec = 0/RA = 0 star lands on the equinox
axis).

Neighbors are anonymous — the sim names no star (proper names are a
captured future direction, not a gap here). Identity for display is
`color` + `class_name` ("the amber giant"), the almanac's own convention.

## 3. Producer work (hornvale)

- `windows/scene`: `NEIGHBORS_SCHEMA`, `NeighborElem`, `FieldStarElem`,
  `NeighborsScene`, `neighbors_scene(world)` (pure reads: `sky_of` →
  `system.neighbors`, plus `hornvale_astronomy::starfield(seed.derive(
  ASTRONOMY_STREAM_ROOT))` for the background field — derived on demand
  exactly as the almanac's figures path does; consumes no genesis draws),
  `neighbors_json` — mirroring `moons_scene` exactly, including the
  no-generated-sky error.
- **`scene/system/v1` additive rider:** `MoonElem` appends
  `inclination_deg` and `node_longitude_deg` (both already on
  `hornvale_astronomy::Moon`; pure reads). Appended after every existing
  field per the schema stability contract — the `density_g_cm3`/
  `formation` precedent on moons/v1. Producer goldens re-pinned in the
  same commit (rebaseline-in-the-drifting-commit rule).
- CLI: `hornvale scene neighbors [--world <PATH>]` beside
  `scene system|moons|tiles`.
- wasm: `hw_scene_neighbors()` in `clients/world-wasm`, the
  `hw_scene_moons` pattern.
- Tests: golden determinism + schema pin in `windows/scene/tests/`
  (seed-42 fixture), byte-identity across rebuild, plus a range battery
  (dec ∈ [-90, 90], RA ∈ [0, 360), brightness > 0) — cheap because
  `genesis_properties.rs` already proves the underlying invariants.
- Reference page: `book/src/reference/scene-neighbors-v1.md` (the
  scene-moons-v1 pattern), including the frame convention and the
  consumer transform.
- Cross-repo golden: a **producer-sourced** seed-42
  `scene/neighbors/v1` JSON committed to the Orrery's testdata (The
  Isotherm's lesson: never a JS reconstruction), same for the updated
  system scene with moon inclination fields.

## 4. Consumer work (orrery, branch `the-real-sky`)

- Parser: `parseNeighbors` in `src/sim/scene.ts` (strict, camelCase
  mapping, length/range validation — house style), catalog
  `sceneNeighbors()`, wired through the wasm loader.
- **Starfield**: replace `buildStarfield`'s random cloud with the real
  sky on the backdrop shell — both populations, positioned via the pinned
  equatorial→ecliptic transform. Neighbors: bright points, intensity from
  log-scaled `brightness_rel` (the dynamic range spans decades; the
  mapping is presentation, unit-tested for monotonicity and clamping),
  color from a total map over the producer's color-word vocabulary
  (enumerated from `neighborhood.rs`; unknown word → neutral fallback +
  test that the map covers the enumeration). Field stars: faint neutral
  points graded by `magnitude_class` (1 brightest … 5 faintest). The old
  cosmetic cloud is deleted, not kept as a fallback.
- **Moon orbits**: `moonLocalPosition` tilts the orbit plane by
  `inclination_deg` about the node line (`node_longitude_deg`);
  inclination > 90° yields retrograde motion with no special casing.
  Orbit line rendering follows the same plane. Unit tests: prograde
  unchanged at inclination 0, retrograde reverses sweep direction,
  node line orientation pinned.
- **Inspector cards**: a neighbor star card (class, color, distance,
  brightness) via the existing `InfoCard` machinery if hover/click
  plumbing permits cheaply — else the starfield ships without cards and
  the gap is a followup. Moon card gains `formation` ("giant-impact" /
  "capture"), `density_g_cm3`, and a "retrograde" tag when
  inclination > 90°. (The v4-safe physical-descriptor card lines shipped
  as this campaign's Tier-0 warm-up commit.)
- Re-pin: `wasm:release` moves to **world-wasm-v5** at G6 (release +
  re-pin are Nathan-gated carve-outs); parsers may require the new fields
  outright since the re-pin lands in lockstep (The Faces precedent).

## 5. Determinism and safety

- **No epoch, no new draws, no ledger change**: every export is a pure
  read of already-derived state; worlds, almanacs, and censuses are
  byte-identical. The only byte changes are in scene documents
  (`scene/system/v1` gains two appended moon fields) and their pinned
  goldens, re-pinned in the drifting commit.
- No stream labels added or reordered; pin-isolation batteries
  untouched.
- Quantization at emit only (decision 0033), as with every scene field.

## 6. Testing summary

Producer: scene golden + determinism + range battery; CLI smoke via the
artifact drift check; architecture test already covers the new export's
layering (windows/scene → domains). Consumer: parser fixtures
(producer-sourced), transform pin tests, brightness-mapping monotonicity,
color-map totality, retrograde/prograde orbit tests; `npm test` +
`tsc --noEmit` green; Playwright e2e updated if any existing spec asserts
on the starfield. Visual verification: The Lens's lesson — open the
rendered result, don't trust jsdom; a screenshot of seed 42's sky joins
the G6 evidence.

## 7. Non-goals

- **`scene/eclipses/v1`** (Eclipse Seasons' dated eclipses + ground
  tracks on the clock/globe) — the designated sequel campaign, not here.
- **Figure (constellation) lines** — `figures()` aggregates membership
  away (`Figure` has `member_count`, no member indices), so an export
  needs producer surfacing first; captured as ORRERY-figure-lines.
- **Wanderers in the system view** — already banked as
  ORRERY-sibling-planets; untouched.
- **Precession / proper motion in the export** — static genesis
  coordinates only (SKY-proper-motion stays open).
- **Star proper names** — captured as SKY-neighbor-names.
- **Any client-side re-derivation of producer logic** (Isotherm lesson).

## 8. Flagged for G3

1. **Schema/save-format adjacent (leads per the carve-out rules):** one
   new scene schema (`scene/neighbors/v1`) and two **additive** fields
   appended to `scene/system/v1`'s `MoonElem`. No epoch, no draw-order
   change, no ledger/census impact — worlds byte-identical. The moons/v1
   `density_g_cm3`/`formation` append is the direct precedent. Confirming
   this reading is the main ask.
2. **world-wasm-v5 release + orrery re-pin + origin pushes** — G6
   carve-outs; nothing external happens without Nathan's explicit OK.
3. **Frame convention choice**: exporting raw genesis equatorial RA/dec
   (the ledger's own numbers) and pinning the equatorial→ecliptic
   transform as consumer-side, documented normatively on the reference
   page. Alternative (exporting pre-transformed ecliptic coordinates) was
   rejected: it would mint a second coordinate convention the ledger
   doesn't carry and push producer opinion into presentation space.

## 9. Decisions (promoted from the campaign ledger)

- Separate schema over extending `scene/system/v1` with a neighbors
  array: one concern per document (the moons/v1 precedent); system/v1
  stays orbital elements.
- Figures deferred (see §7) rather than scoped in: membership isn't
  exported by the domain today; dots at real positions already deliver
  the campaign's value.
- Warm-up shipped v4-safe fields only; Reckoning-dependent card lines
  ride this campaign's v5 bump.
