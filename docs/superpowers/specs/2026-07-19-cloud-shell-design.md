# The globe's cloud layer — a texture shell (The Mantle) — design

**Date:** 2026-07-19 · **Status:** awaiting G3 review
**Follows:** The Firmament (Weather Program C4), which shipped the sim's
per-tile `cloud_type` + `weather_propensity` but deferred the globe cloud
*rendering* (billboarded soft-sprite particles read as atmospheric grey haze).
**Repos:** orrery only, + a world-wasm-v11 release (to expose the already-merged
scene fields to the client). Hornvale sim unchanged; worlds byte-identical.

## 0. Why this exists, and the keystone the last attempt missed

The Firmament made the sky *felt* (possession/almanac narrate the weather) but
left the globe unable to *wear* it. The particle attempt failed not only on its
primitive (`THREE.Points` / billboarded sprites — a 1px-line then a size-capped
haze) but on a deeper omission: it rendered clouds as **untextured soft blobs**.
A flat-colored shell would fail the same way. The load-bearing move — the
keystone — is **fbm noise gated by cloud type into an alpha channel**, giving
soft cloud *edges* with per-type character. Get that and any planet-standard
primitive reads as clouds; skip it and any primitive reads as haze.

## 1. The cloud-texture shell (`CLIM-clouds`, presentation)

Replace the particle overlay with the **standard planet-cloud-layer**: a
transparent `THREE.SphereGeometry` shell at a radius just above the globe's
maximum relief, carrying a generated equirectangular cloud texture. The shell's
solid back face plus the opaque globe **occlude the far side** — the halo that
plagued the billboarded sprites cannot form. It composes with whichever lens is
active (it is an overlay, not a lens), toggled by the existing HUD `clouds`
button. This fits established codebase precedent — `moonTexture.ts` already
builds a procedural `THREE.DataTexture` for the moon spheres, and `ocean.ts` a
`CanvasTexture` wave map.

## 2. The texture generator — the heart (`cloudTexture.ts`)

A pure function `TilesScene → RGBA buffer` (unit-tested, no WebGL). For each
texel, map its equirectangular (longitude, latitude) to the nearest tile
(reusing the existing tile-index / nearest-cell lookup), read that tile's
`cloudType` and `weatherPropensity`, and write RGBA where:

- **alpha = fbm(longitude, latitude) gated by a per-type coverage threshold** —
  soft wispy cloud edges, not flat per-tile blocks. `none` (type 0) is fully
  transparent.
- **color and noise character by cloud type:** cumulus (type 1) bright white,
  puffy small-scale; stratus (2) a smooth grey sheet; nimbostratus (3) a dark
  dense deck; cumulonimbus (4) the darkest and densest; cirrus (5) faint,
  high-frequency streaks. Altitude is expressed as texture *style*, not
  geometry.

Tuned for **balanced planet-cloud prominence** (Nathan): typed cloud systems
clearly visible over storm-prone regions, the surface still reading through the
gaps — Earth-from-orbit, not an overcast shroud.

## 3. Animation and the determinism boundary

**Gentle drift** (Nathan): the cloud pattern rotates / offsets its texture
longitude slowly over time — the living globe, waived eyecandy (decisions
0022/0023). The cloud *data* is the sim's deterministic `cloud_type` (the
day-0 snapshot); the texture and its drift are the non-authoritative *picture*
of it, so there is **no golden** (the client generates the texture from the
parsed fields, recomputing nothing the sim pins). During the build, a **lit
shell material** (responding to the scene's existing sun) is evaluated as a
cheap day-night cloud-shading win — adopted only if it is near-free and reads
better; not a requirement.

## 4. Non-goals (deferred garnish — the cleave)

An ideonomy pass sorted the option space: the must-have core is a
noise-masked, type-styled, drifting, occluded shell. Everything else is realism
garnish a single well-textured shell does not need to earn its keep, and is
**deferred**:

- **Cloud shadows** cast onto the surface (a shadow pass / projected mask).
- **Explicit terminator tinting** (sunset-reddening, a crisp day/night cloud
  line) beyond what a lit material gives for free.
- **Altitude-stratified shells** (a separate higher cirrus shell for parallax) —
  per-type texture style already conveys altitude.
- **Per-day cloud evolution** (re-deriving `cloud_type` as the day-clock scrubs
  — the client re-running the sim's weather sampler). Drift suffices; this is
  the known higher-fidelity follow-up.

## 5. Deliverable and scope

- **Orrery:** rewrite `src/views/clouds.ts` from the particle overlay to the
  texture shell; add `src/views/cloudTexture.ts` (the generator). The WIP on the
  `the-firmament` orrery branch (the `parseTiles` weather-field parsing + the
  cloud-type style table) is reused where it fits; the particle render is
  dropped.
- **Release:** world-wasm-**v11** (exposes the already-merged `cloud_type` /
  `weather_propensity` scene fields to the client); orrery re-pins v10 → v11.
- **Hornvale sim:** unchanged. Worlds, ledgers, census all byte-identical (no
  epoch, no census regen).

## 6. Testing

- **Unit (the generator):** `cloudTexture` is a pure `TilesScene → RGBA`
  function — assert the type→style mapping is total over 0..5, `none` is
  transparent, each cloud type yields a distinct color/alpha character
  (cumulonimbus darkest+densest, cirrus faintest), and the mask tracks
  `cloudType`/`weatherPropensity` (mechanism-sensitive: a uniform stub fails).
- **Parse:** `parseTiles` carries `cloudType`/`weatherPropensity` strictly
  (from the Firmament WIP).
- **Visual verification (The Lens rule) — mandatory.** Open the globe PNGs and
  confirm the clouds read as legible, typed, discrete cloud systems over
  storm-prone belts, surface visible through the gaps, drifting. **Apply the
  Firmament retro's discipline: `npm run build` and kill the reused preview
  server before each capture** — the Playwright webServer serves `dist/`, and a
  stale build silently blinds the tuning loop (it cost two iterations last
  time).

## 7. Flagged for G3

1. **Approach:** a cloud-texture shell (planet-standard, occluded) replacing the
   deferred particle overlay; the keystone is fbm-gated-by-type texture
   generation (the difference between clouds and haze). Confirm.
2. **world-wasm-v11 release + orrery re-pin + push** — the G6 carve-out
   (externally visible). No epoch, no census. Byte-identical worlds.
3. **Prominence + motion:** balanced planet clouds, gentle drift (Nathan).
4. **Deferred garnish:** shadows, terminator tinting, altitude shells, per-day
   evolution — flag if any should be pulled in.

## 8. Decisions (promoted from the campaign ledger)

- **Cloud-texture shell, not particles** (G1, ideonomy: negation/substitution/
  dictionary, 1 pass — enriched): a transparent textured shell occludes the far
  side and reads as a planet's cloud layer; the failed particles could not.
- **The keystone is fbm-noise-gated-by-cloud-type alpha** — untextured blobs
  (any primitive) read as haze; this is the non-negotiable core.
- **Balanced prominence, gentle drift** (Nathan) — Earth-from-orbit; waived
  eyecandy; day-0 snapshot types.
- **No golden** (client generates the texture, recomputes nothing pinned).
- **Deferred garnish**: cloud shadows, terminator tinting, altitude
  stratification, per-day evolution.
