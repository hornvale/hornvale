# The Mantle (Globe Cloud Shell) Implementation Plan

> **Status: COMPLETE** (shipped 2026-07-19, world-wasm-v11).
>
> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Render legible, typed, realistic clouds on the orrery globe as a transparent **cloud-texture shell** (replacing The Firmament's deferred billboarded-sprite particles), driven by the sim's per-tile `cloud_type`/`weather_propensity`.

**Architecture:** A transparent `THREE.SphereGeometry` shell just above the globe's max relief carries a generated equirectangular cloud `DataTexture` (mirroring `src/views/moonTexture.ts`). The tile grid is already an equirectangular lat/lon layout, so a pure `cloudTexture(tiles) → RGBA` function maps each texel to its tile, and writes `alpha = fbm(lon,lat)` gated by a per-cloud-type coverage threshold, colored per type. The shell's solid back face + the opaque globe occlude the far side (no halo). It drifts slowly (waived eyecandy). `createClouds` keeps its `{ object3d, setVisible, update }` interface so `globe.ts`/`main.ts`/HUD wiring is unchanged.

**Tech Stack:** TypeScript, three.js (orrery). No new deps. This repo (orrery) only; the sim (`cloud_type`/`weather_propensity` on `scene/tiles/v1`) is already on hornvale main.

## Global Constraints

- **The keystone:** the cloud alpha MUST be fbm noise gated by cloud type — soft wispy edges, not flat per-tile color blocks. Untextured blobs (any primitive) read as haze; this is the non-negotiable core.
- **Balanced planet-cloud prominence** (Nathan): typed cloud systems clearly visible over storm-prone regions, the surface still reading through the gaps — Earth-from-orbit, not an overcast shroud.
- **Gentle drift** (Nathan): the shell rotates / offsets texture longitude slowly. Waived eyecandy (decisions 0022/0023); cloud types are the day-0 snapshot; NO per-day re-derivation.
- **Determinism boundary:** the cloud *data* is the sim's deterministic `cloud_type`; the texture + drift are the non-authoritative *picture* — **no golden**, the client generates the texture from parsed fields.
- **No forbidden-surface edits during implementation:** do NOT edit `package.json`/`public/`/wasm-URL in the impl tasks (the controller re-pins to v11 at G6). `public/hornvale_world.wasm` is gitignored — the controller vendors a v11-equivalent wasm for visual verification.
- **Preserve the `createClouds` interface** `{ object3d: THREE.Object3D, setVisible(on), update(day) }` so `globe.ts`/`main.ts` need no changes (locked-world availability wiring unchanged).
- Run `npx vitest run` (once, never `--watch`) and `npx tsc --noEmit` before each commit.

## File Structure

- `src/sim/scene.ts` — `parseTiles` gains `weatherPropensity: number[]`, `cloudType: number[]` (T1).
- `src/views/cloudTexture.ts` (NEW) — the pure texture generator + a tiny value-noise fbm (T2). The heart; unit-tested.
- `src/views/clouds.ts` — rewritten: particle overlay → texture shell, same public interface (T3).
- `src/views/clouds.test.ts` — rewritten to test the shell/generator (T2/T3).

---

### Task 1: Parse the weather scene fields

**Files:**
- Modify: `src/sim/scene.ts` (`parseTiles`, the `TilesScene` type)
- Modify: `src/sim/scene.test.ts`

**Interfaces:**
- Produces: `TilesScene.weatherPropensity: number[]` (length width×height, each in [0,1]); `TilesScene.cloudType: number[]` (length width×height, integer 0..=5).

- [ ] **Step 1: Write the failing test** (mirror the existing precip-field parse tests in `scene.test.ts`)

```ts
it('parses weather_propensity and cloud_type strictly', () => {
  const t = parseTiles(fixtureWithWeather());
  expect(t.weatherPropensity).toHaveLength(t.width * t.height);
  expect(t.cloudType).toHaveLength(t.width * t.height);
  expect(t.cloudType.every((c) => Number.isInteger(c) && c >= 0 && c <= 5)).toBe(true);
});
```
(Add `weather_propensity`/`cloud_type` arrays to whatever fixture builder the precip parse tests use.)

- [ ] **Step 2: Run to verify it fails** — `npx vitest run scene 2>&1 | tail -12` → FAIL.

- [ ] **Step 3: Extend `parseTiles`** — parse `weather_propensity` with the SAME validator as `moisture` (number array, length width×height) into `weatherPropensity`; parse `cloud_type` with the SAME validator as `precip_regime` (`intArrayInRange(..., 0, 5)`) into `cloudType`. Add both to the `TilesScene` type and the `parseTiles` return.

- [ ] **Step 4: Run** — `npx vitest run scene 2>&1 | tail -6` → PASS; `npx tsc --noEmit` clean.

- [ ] **Step 5: Commit**

```bash
git add src/sim/scene.ts src/sim/scene.test.ts
git commit -m "feat(the-mantle): parseTiles carries weather_propensity + cloud_type"
```

---

### Task 2: The cloud-texture generator (the heart)

**Files:**
- Create: `src/views/cloudTexture.ts`
- Create: `src/views/cloudTexture.test.ts`

**Interfaces:**
- Consumes: `TilesScene` (from Task 1: `width`, `height`, `cloudType`, `weatherPropensity`).
- Produces:
  - `export const CLOUD_TEX_W = 1024; export const CLOUD_TEX_H = 512;`
  - `export function cloudTextureData(tiles: TilesScene): Uint8ClampedArray` — row-major RGBA, `CLOUD_TEX_W*CLOUD_TEX_H*4`. Alpha = fbm gated by cloud type; RGB per type; `none` fully transparent (alpha 0).
  - `export interface CloudStyle { color: [number,number,number]; coverage: number; grain: number }` and `export function cloudStyleFor(cloudType: number): CloudStyle` (the per-type table — reuse The Firmament WIP's colors: cumulus bright, stratus grey sheet, nimbostratus dark, cumulonimbus darkest, cirrus faint high-frequency; total over 0..5, clamps out-of-range to cumulus).

**Design notes for the implementer:**
- The tile grid is already equirectangular: texel `(x,y)` → tile column `Math.min(width-1, Math.floor((x/CLOUD_TEX_W)*width))`, tile row `Math.min(height-1, Math.floor((y/CLOUD_TEX_H)*height))`, tile index `row*width+col`.
- Value-noise fbm (self-contained; eyecandy, no determinism contract): a hash `h(i,j)=fract(sin(i*127.1+j*311.7)*43758.5453)`, bilinear-interpolated, summed over ~4 octaves. Sample it at `(x/scale, y/scale)`.
- `alpha = clamp01((fbm - (1 - coverage)) * SOFTNESS) * baseAlpha` where `coverage` (per type) sets how much of the region fills and `SOFTNESS` sharpens edges — so higher-coverage types (nimbostratus/cumulonimbus) fill more, cirrus is sparse and streaky (use a stretched/anisotropic fbm scale for cirrus `grain`). Modulate coverage upward slightly by `weatherPropensity`.
- Keep overall alpha BALANCED (Nathan): the surface must read through the gaps — cap `baseAlpha` so even dense clouds are ~0.7-0.85, not opaque.

- [ ] **Step 1: Write the failing test** (`cloudTexture.test.ts`)

```ts
import { describe, it, expect } from 'vitest';
import { cloudTextureData, cloudStyleFor, CLOUD_TEX_W, CLOUD_TEX_H } from './cloudTexture';

// Build a tiny TilesScene whose every tile is one cloud type.
function uniform(cloudType: number, propensity = 0.6) {
  const width = 8, height = 4, n = width * height;
  return { width, height, cloudType: Array(n).fill(cloudType), weatherPropensity: Array(n).fill(propensity) } as any;
}

describe('cloudTextureData', () => {
  it('is transparent everywhere over a cloudless (None) world', () => {
    const d = cloudTextureData(uniform(0));
    let maxAlpha = 0;
    for (let i = 3; i < d.length; i += 4) maxAlpha = Math.max(maxAlpha, d[i]!);
    expect(maxAlpha).toBe(0);
  });

  it('produces cloud where cloudType is non-None, with soft (non-binary) edges', () => {
    const d = cloudTextureData(uniform(4)); // cumulonimbus
    let opaque = 0, partial = 0, clear = 0;
    for (let i = 3; i < d.length; i += 4) {
      const a = d[i]!;
      if (a === 0) clear++; else if (a >= 250) opaque++; else partial++;
    }
    expect(opaque + partial).toBeGreaterThan(0);   // there ARE clouds
    expect(partial).toBeGreaterThan(0);            // soft edges exist (not binary) — the keystone
    expect(clear).toBeGreaterThan(0);              // and gaps exist (balanced, surface reads through)
  });

  it('cumulonimbus reads denser+darker than cirrus (typed, mechanism-sensitive)', () => {
    const dark = cloudStyleFor(4), wisp = cloudStyleFor(5);
    const lum = (c: [number,number,number]) => 0.299*c[0]+0.587*c[1]+0.114*c[2];
    expect(dark.coverage).toBeGreaterThan(wisp.coverage);
    expect(lum(dark.color)).toBeLessThan(lum(wisp.color));
  });

  it('cloudStyleFor is total over 0..5 and clamps out of range', () => {
    for (let t = 0; t <= 5; t++) expect(cloudStyleFor(t)).toBeTruthy();
    expect(cloudStyleFor(9)).toEqual(cloudStyleFor(1)); // clamp to cumulus
  });

  it('texture is the declared size', () => {
    expect(cloudTextureData(uniform(1))).toHaveLength(CLOUD_TEX_W * CLOUD_TEX_H * 4);
  });
});
```

- [ ] **Step 2: Run to verify it fails** — `npx vitest run cloudTexture 2>&1 | tail -12` → FAIL (module missing).

- [ ] **Step 3: Implement `cloudTexture.ts`** per the design notes (the per-type style table, the value-noise fbm, the alpha-gating). Balance `baseAlpha`/coverage so the None test is fully transparent and the cumulonimbus test has opaque+partial+clear texels.

- [ ] **Step 4: Run** — `npx vitest run cloudTexture 2>&1 | tail -8` → PASS; `npx tsc --noEmit` clean.

- [ ] **Step 5: Commit**

```bash
git add src/views/cloudTexture.ts src/views/cloudTexture.test.ts
git commit -m "feat(the-mantle): fbm-gated-by-type cloud texture generator (the keystone)"
```

---

### Task 3: The cloud-texture shell

**Files:**
- Modify: `src/views/clouds.ts` (rewrite: particle overlay → texture shell; keep the public interface)
- Modify: `src/views/clouds.test.ts` (rewrite for the shell)

**Interfaces:**
- Consumes: Task 2's `cloudTextureData`/`CLOUD_TEX_W`/`CLOUD_TEX_H`; the globe's radius (the existing `createClouds(tiles, ...)` signature — keep its current parameters; study how `globe.ts` calls it).
- Produces (unchanged public shape): `export function createClouds(...) : { object3d: THREE.Object3D; setVisible(on: boolean): void; update(day: number): void }`.

**Design notes:**
- Build a `THREE.SphereGeometry(shellRadius, 96, 48)` where `shellRadius = GLOBE_RADIUS * (1 + relief-headroom + small ε)` (just above the max-relief surface so it never clips into mountains — study `globe.ts`'s relief scaling for the headroom value).
- Wrap `cloudTextureData(tiles)` in a `THREE.DataTexture(data, CLOUD_TEX_W, CLOUD_TEX_H, THREE.RGBAFormat)` with `texture.needsUpdate = true`, `wrapS = THREE.RepeatWrapping` (for longitude drift), `minFilter/magFilter = THREE.LinearFilter`.
- Material: start with `THREE.MeshBasicMaterial({ map: texture, transparent: true, depthWrite: false })` (unlit). Then EVALUATE a `MeshStandardMaterial` (lit — day/night shading from the scene sun) if it reads better at near-zero cost; the controller confirms visually. Back-face culling default (`side: THREE.FrontSide`) so the far side is hidden by the shell + globe.
- **UV/orientation:** the shell's equirectangular UVs must align with the globe's lon/lat so clouds sit over the right places. Study how `globe.ts`/`worldMesh.ts` map tile lon/lat to sphere position and match the sphere's rotation/UV offset (`SphereGeometry`'s seam is at +X/−Z by default; set `mesh.rotation.y` to align longitude 0). Verify alignment in the controller's visual pass.
- `setVisible(on)` toggles `mesh.visible`. `update(day)` advances a slow longitude drift: `texture.offset.x = (day * DRIFT_RATE) % 1` (RepeatWrapping) — a gentle eyecandy rotation, NOT per-day type re-derivation. `DRIFT_RATE` a small exported tunable.
- Locked-world / availability: keep whatever `createClouds` currently does for the locked case (empty/hidden), so `main.ts`'s `setCloudsAvailable` wiring is unchanged.

- [ ] **Step 1: Write the failing test** (`clouds.test.ts`, rewritten) — assert `createClouds(tiles).object3d` is a `THREE.Mesh` whose material is transparent and carries a `DataTexture` of the right size; `setVisible(false)` hides it; `update(10)` advances `texture.offset.x` from `update(0)` (drift), and does NOT change the texture image data (no per-day re-derivation). Use the existing test's `tiles` fixture builder (extended with weather fields).

- [ ] **Step 2: Run to verify it fails** — `npx vitest run clouds 2>&1 | tail -12` → FAIL.

- [ ] **Step 3: Rewrite `clouds.ts`** to the shell per the design notes. Delete the particle scaffold (`CLOUD_PARTICLES`, `stepParticle`, `weightedIndex`, `writeParticle`, the LineSegments/Points geometry). Keep the `createClouds` signature + return shape.

- [ ] **Step 4: Run** — `npx vitest run 2>&1 | tail -8` (whole suite; fix any `TilesScene` fixture builders that need the two new weather fields as additive filler) and `npx tsc --noEmit`. Expected: green, tsc clean.

- [ ] **Step 5: Commit**

```bash
git add src/views/clouds.ts src/views/clouds.test.ts
git commit -m "feat(the-mantle): render the globe cloud layer as a texture shell (drop particles)"
```

---

### Task 4: Assembly, visual verification, and the v11 release (CONTROLLER)

**Run by the controller, not a fresh implementer.**

- [ ] **Absorb orrery main** into `the-mantle` if it moved; resolve; `npx vitest run` + `tsc` + `npm run build` + Playwright e2e green.
- [ ] **Vendor a v11-equivalent wasm** for visual verification: build the wasm from hornvale main (unchanged — already has the scene fields), `cp` into `public/hornvale_world.wasm`.
- [ ] **VISUAL VERIFICATION (The Lens rule) — the whole point.** Capture globe PNGs and INSPECT by eye: the clouds read as legible, typed, discrete cloud systems over storm-prone belts; the surface reads through the gaps (balanced); the layer drifts; the far side is occluded (no halo); cumulonimbus vs cirrus visibly differ. **Rebuild (`npm run build`) and kill the reused :4173 preview server before EACH capture** (the Firmament stale-build trap). Tune `cloudTexture`'s coverage/alpha + the shell radius/drift by eye until it reads well; evaluate the lit-material option.
- [ ] **G6 package** for Nathan (hard stop): the only carve-out is the world-wasm-v11 release + orrery re-pin + push. On approval: build & publish world-wasm-v11 from hornvale main, re-pin orrery `wasm:release` v10→v11 (verify fetch + sha), push orrery main; then `closing-a-campaign` (chronicle amendment or a short Mantle chronicle, retro, registry `CLIM-clouds` presentation → shipped, memory). No hornvale merge needed (sim unchanged); no epoch, no census.

---

## Notes for the executor

- **The keystone is Task 2's fbm-gated-by-type alpha.** If the visual pass shows haze or flat blocks, the problem is almost always there (coverage/softness/noise-scale), not the shell.
- **Visual tuning is the controller's job** — the implementer makes it correct and green; the controller makes it *read* right by eye (rebuild-before-capture).
- Reuse The Firmament WIP (orrery branch `the-firmament` @70ef728) as a reference for the parse + the per-type color table, but this is a clean `the-mantle` branch — the particle render is dropped, not carried.
