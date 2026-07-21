# The Overworld Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rework the flat map's Pixel-art style into a 16-bit RPG overworld look — a procedural renderer producing a higher-resolution texture with a tight palette, ordered dithering, crafted coastlines, and landform outlines.

**Architecture:** A new pure renderer `overworldRGBA(region, dim)` computes an RGBA buffer at `dim×dim` (≈384², decoupled from the coarse 65×65 region grid): each output pixel maps to its nearest region node for biome/ocean/elevation, then palette + dither + coastline + outline are applied per output pixel. A `DataTexture` wrapper feeds it to the map plane's `'pixel'` style. All deterministic — ordered (Bayer) dither, no `Math.random`.

**Tech Stack:** TypeScript, three.js, Vitest (unit), Playwright (e2e). Orrery repo only.

## Global Constraints

- **Orrery / client-only.** No producer/wasm change (world-wasm stays v12), no scene fields, no epoch. (Determinism waived client-side, 0022/0023 — but see next.)
- **NO `Math.random`** anywhere in the renderer. Dithering is ordered (a Bayer matrix indexed by output `(px,py)`); coastline/outline detection is a pure function of the region field; any noise is positional/hashed deterministically. Every renderer test asserts determinism (same input → identical output).
- **Reworks the pixel map texture ONLY.** The Voxel-2.5D style (The Diorama) and the non-pixel lenses must be untouched. Do not regress `pixelColorFor`'s use by the voxel style.
- **Never hardcode biome/water legend order** — resolve biome/water classes by NAME via `biomeLegend`/`waterLegend` (as `pixelColorFor` and The Freshwater do).
- Tuning consts (palette, Bayer size, dither strength, coastline band width, outline weight) are named `export const`s — the controller iterates them in a screenshot visual pass.
- Run `npm run build` (tsc) + `npm test` (vitest) before every commit; `npx prettier -w` touched files.
- Every commit ends with the trailer:
  `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: Procedural renderer pipeline + palette fill (higher-res), wired

Establishes the end-to-end pipeline: a higher-res texture of palette biome/ocean fill (no dither/coast yet), so every later task visibly improves a live map.

**Files:**
- Create: `src/views/styles/overworld.ts`
- Modify: `src/views/mapTexture.ts` (an `overworldTexture` wrapper)
- Modify: `src/views/mapView.ts` (the `'pixel'` branch mounts `overworldTexture`)
- Test: `src/views/styles/overworld.test.ts`

**Interfaces:**
- `export const OVERWORLD_TEXTURE_DIM = 384;`
- `export function overworldRGBA(region: RegionScene, dim: number): Uint8Array;` — length `dim*dim*4`, row-major, top-down (row 0 = region gy=0, to match `regionPixelTexture`'s `flipY:true` convention).
- `export const OVERWORLD_PALETTE` — per-biome `{ light, dark }` tone pairs + `ocean{shallow,deep}`, `shallows`, `foam`, `outline` tones. Seed the biome tones from `pixelBase.ts`'s `PIXEL_LAND_RGB` (import or re-derive) with a light/dark pair per biome (e.g. ±12% luma).
- `overworldTexture(region): THREE.DataTexture` (mirrors `regionPixelTexture`: NearestFilter, `flipY:true`, `needsUpdate`).

**Nearest-node mapping:** output `(px,py) → nodeCol = min(N, floor(px/dim * (N+1)))`, `nodeRow` likewise, where `N = region.samples`; node index `= nodeRow*(N+1) + nodeCol`.

- [ ] **Step 1: Write the failing test:**

```ts
import { overworldRGBA, OVERWORLD_PALETTE } from './overworld';
describe('overworldRGBA — palette fill', () => {
  it('colors each output pixel by its nearest region node biome', () => {
    const region = miniRegion(/* 2x2: one ocean cell, three of a known biome */);
    const buf = overworldRGBA(region, 8); // 8x8 output
    expect(buf.length).toBe(8 * 8 * 4);
    // a pixel over the ocean cell is an ocean tone; over land is that biome's tone
    expect(pixelAt(buf, 8, 1, 1)).toEqual(OVERWORLD_PALETTE.ocean.shallow /* or deep */);
    expect(alphaAt(buf, 8, 4, 4)).toBe(255);
  });
  it('is deterministic — identical output for identical input', () => {
    const r = miniRegion();
    expect(overworldRGBA(r, 16)).toEqual(overworldRGBA(r, 16));
  });
});
```

- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** `overworldRGBA` (nearest-node palette fill), the palette, `overworldTexture`, and switch the `mapView` `'pixel'` branch to `overworldTexture(region)` (leave the plane/camera/symbols otherwise unchanged). Reuse `pixelColorFor`'s ocean deep/shallow + biome/water resolution logic for WHICH class a node is; the overworld palette supplies the tones.
- [ ] **Step 4: Run it, confirm it passes** + full `npm test` + `npm run build`.
- [ ] **Step 5: Commit** — `feat(overworld): procedural higher-res palette renderer, wired to the pixel map`.

---

### Task 2: Within-biome ordered dithering

**Files:**
- Modify: `src/views/styles/overworld.ts`
- Test: `src/views/styles/overworld.test.ts`

**Interfaces:**
- `export const BAYER_4 /* or 8 */` — the ordered matrix; `export const OVERWORLD_DITHER_STRENGTH`.
- A land output pixel picks `light` vs `dark` of its biome by comparing a per-pixel threshold `bayer(px,py)` against a value keyed off elevation (higher elevation → more `light`, so the dither also reads as relief), scaled by `OVERWORLD_DITHER_STRENGTH`.

- [ ] **Step 1: Write the failing test** — within one biome region, adjacent output pixels alternate light/dark per the Bayer pattern (not a flat single tone); the choice is deterministic in `(px,py)`; a flat-elevation biome still shows the base dither texture (both tones present).

```ts
it('dithers each biome between its light/dark tones by the Bayer matrix', () => {
  const region = flatBiomeRegion('temperate-forest'); // uniform biome + elevation
  const buf = overworldRGBA(region, 16);
  const tones = distinctColors(buf); // should contain BOTH light and dark forest tones
  expect(tones).toContainEqual(OVERWORLD_PALETTE.biome['temperate-forest'].light);
  expect(tones).toContainEqual(OVERWORLD_PALETTE.biome['temperate-forest'].dark);
});
```

- [ ] **Step 2: Run it, confirm it fails** (currently a flat single tone).
- [ ] **Step 3: Implement** the Bayer dither for land fill. Ocean gets its own gentle dither between `ocean.shallow`/`ocean.deep` too (optional, keyed off depth). Confirm determinism test still green.
- [ ] **Step 4: Run it, confirm it passes** + full suite + build.
- [ ] **Step 5: Commit** — `feat(overworld): within-biome ordered (Bayer) dithering`.

---

### Task 3: Crafted coastlines

**Files:**
- Modify: `src/views/styles/overworld.ts`
- Test: `src/views/styles/overworld.test.ts`

**Interfaces:**
- `export const COAST_BAND_PX` (shallows band width in output pixels), `export const FOAM_...`.
- A land/water boundary is detected at output resolution: an output pixel is a **coast outline** if it is land with a water neighbor within 1px (or vice versa); water within `COAST_BAND_PX` of land → **shallows** tone; the water pixel just outside the outline → **foam** (a bright dither). Neighbor = the nearest-node class at `(px±k, py±k)`.

- [ ] **Step 1: Write the failing test** on a mini split region (left half ocean, right half land):

```ts
it('draws an outline on the land/water boundary and a shallows band in the water', () => {
  const region = splitRegion(/* left ocean, right land */);
  const buf = overworldRGBA(region, 32);
  // a column at the boundary carries the outline tone
  expect(columnContains(buf, 32, boundaryCol, OVERWORLD_PALETTE.outline)).toBe(true);
  // water just left of the boundary is the shallows tone, not open-ocean
  expect(pixelAt(buf, 32, boundaryCol - 1, 16)).toEqual(OVERWORLD_PALETTE.shallows);
});
```

- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** coastline detection (shallows band + outline + foam). Keep it O(dim²) with bounded neighbor lookups; precompute a land/water mask per output pixel once if needed for perf.
- [ ] **Step 4: Run it, confirm it passes** + full suite + build.
- [ ] **Step 5: Commit** — `feat(overworld): crafted coastlines — shallows band, outline, foam`.

---

### Task 4: Biome-boundary + land-silhouette outlines

**Files:**
- Modify: `src/views/styles/overworld.ts`
- Test: `src/views/styles/overworld.test.ts`

**Interfaces:**
- `export const OVERWORLD_OUTLINE_BIOME: boolean` / weight. An output land pixel whose nearest-node biome differs from a neighbor's (within 1px) draws the `outline` tone (thin, 1px). The land/water outline from Task 3 already covers the silhouette; this adds INTERnal biome-boundary lines.

- [ ] **Step 1: Write the failing test** on a two-biome land region (left desert, right forest): the boundary column carries the `outline` tone; interiors do not.
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** biome-boundary outline (reuse the neighbor-class machinery from Task 3). Ensure it composes with dither (outline wins over dither on boundary pixels).
- [ ] **Step 4: Run it, confirm it passes** + full suite + build.
- [ ] **Step 5: Commit** — `feat(overworld): biome-boundary outlines`.

---

### Task 5: e2e + perf + final visual pass

**Files:**
- Modify: `e2e/smoke.spec.ts`
- Possibly modify: `src/views/styles/overworld.ts` (const tuning IF the controller's visual pass calls for it — but the controller owns that; the implementer's job is green e2e + measured perf)

- [ ] **Step 1: e2e — pixel style renders.** In the Map view (reach it via `.hud-view` → globe → map, since `#view=map` isn't URL-addressable), select `.hud-map-style` → `pixel`; register `pageerror`+`console.error` → `expect(errors).toEqual([])`; assert the map canvas is present and non-blank (viewport screenshot `.length > 5000`, the Massing/Diorama idiom). Mirror the existing map-style e2e in this file.
- [ ] **Step 2: Perf measurement.** Measure the `overworldRGBA(region, 384)` build cost (it runs on region/style change, not per frame). Report the number. If a rebuild stalls the interaction, lower `OVERWORLD_TEXTURE_DIM` or precompute the land/water mask. Do not pre-optimize without a measured stall.
- [ ] **Step 3: Run the full suite** — `npm run build && npm test && npx playwright test`. All green.
- [ ] **Step 4: Commit** — `test(orrery): e2e overworld pixel map + perf pass`.

---

## Notes for the executor

- **The controller runs a screenshot visual pass after Tasks 1, 2, 3, 4** (build the client, navigate to Map via `.hud-view`, switch to the pixel style, read the PNG) and tunes the named consts. Expect the palette/dither/coast/outline values to change across rounds — keep them cleanly factored as `export const`s. jsdom paints nothing; only the controller's screenshots judge the look.
- **Determinism is a hard constraint, not client-waived here:** every renderer test asserts identical output for identical input; no `Math.random`.
- Reuse `pixelColorFor`'s class-resolution logic (ocean depth, river/salt-basin by legend name, biome by legend name) — the overworld renderer changes the TONES and adds dither/coast/outline, not which class a node is.
