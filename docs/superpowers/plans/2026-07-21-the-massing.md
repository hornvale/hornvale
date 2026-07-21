# The Massing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **STATUS: SHIPPED 2026-07-21 — registry MAP-66.** All 7 tasks merged, whole-branch review clean, Orrery deployed.

**Goal:** Give the Orrery globe a switchable render-**style** layer (Smooth / Voxel / Terraced / Faceted), orthogonal to the data lens, plus a client-only deeper-LOD ceiling so the globe zooms in far further.

**Architecture:** A `GlobeStyle` selects the geometry builder + material used by the globe's per-tile slot builder; the existing `Lens` still supplies color. Style is a pure function of scene data → geometry (no determinism cost — client render). Voxel/Terraced add new geometry builders in `worldMesh.ts`; Faceted is a material flag; Smooth is unchanged. LOD-deepening raises `LOD_CDLOD_MAX_LEVEL` and lowers the globe camera's min distance so deeper region tiles are selected and reachable.

**Tech Stack:** TypeScript, three.js, Vitest (unit), Playwright (e2e). Orrery repo only.

## Global Constraints

- **Orrery / client-only.** No hornvale/producer change, **no wasm release** (world-wasm stays v12), no new scene fields, no epoch. (Determinism waived client-side, decisions 0022/0023.)
- **No `Math.random`** in any placement/geometry/color path — styles are deterministic functions of scene data.
- **Style ⟂ Lens.** Any lens × any style must be valid. Style never reads/changes *what* color a cell is (that's the lens); it changes geometry, shading, and whether color is per-vertex (interpolated) or per-cell (nearest).
- **Voxel keystone:** the exposed **vertical inter-cell wall** (the cliff) is what makes Voxel "voxel." A voxel build with no walls is a failed voxel.
- **All four styles build from region data** (`scene/tiles-region/v1`) at level ≥ `REGION_MIN_LEVEL`, not only base tiles — the finer geometry is where detail lives.
- Match existing house style: `#![warn]`-equivalent TS strictness; run `npm run build` (tsc) + `npm test` (vitest) before every commit; `npx prettier -w` on touched files.
- Every commit ends with the trailer:
  `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: `GlobeStyle` type + `setStyle` + the Faceted style

Establishes the style axis end-to-end with the cheapest style (a material flag), proving the switch before any new geometry.

**Files:**
- Modify: `src/views/globe.ts` (add `GlobeStyle`, `GlobeView.setStyle`, material flat-shading branch)
- Test: `src/views/globe.test.ts` (or the nearest existing globe unit test file; create if none)

**Interfaces:**
- Produces: `export type GlobeStyle = 'smooth' | 'voxel' | 'terraced' | 'faceted';` and `GlobeView.setStyle(style: GlobeStyle): void`.
- Consumes: existing `GlobeView`, the `MeshStandardMaterial({ vertexColors: true })` at `globe.ts:410`, the slot-rebuild path (`rebuildBase` / `buildTileSlot`).

- [ ] **Step 1: Write the failing test** — `setStyle('faceted')` turns on flat shading; `setStyle('smooth')` turns it back off.

```ts
import { describe, it, expect } from 'vitest';
import { createGlobeView } from './globe';
// use the existing test's scene/tiles fixture loader; name it `tiles` here.
describe('GlobeView.setStyle — faceted', () => {
  it('faceted flat-shades the surface material; smooth restores smooth shading', () => {
    const g = createGlobeView(tiles, /* markers */ [], /* ...existing args */);
    const mat = findSurfaceMaterial(g); // the MeshStandardMaterial with vertexColors
    expect(mat.flatShading).toBe(false);
    g.setStyle('faceted');
    expect(mat.flatShading).toBe(true);
    expect(mat.needsUpdate).toBe(true);
    g.setStyle('smooth');
    expect(mat.flatShading).toBe(false);
  });
});
```

- [ ] **Step 2: Run it, confirm it fails** — `npm test -- globe` → FAIL (`setStyle` undefined).
- [ ] **Step 3: Implement** — add the `GlobeStyle` type; hold `let activeStyle: GlobeStyle = 'smooth'`; add `setStyle(style)` to the returned `GlobeView`. For `faceted` vs everything-else, set `material.flatShading = (style === 'faceted')` and `material.needsUpdate = true`. For now, non-faceted geometry is the current smooth build (voxel/terraced land in later tasks — `setStyle` may treat them as smooth-geometry-for-now, or throw `not-yet-implemented`; prefer smooth-for-now so the switch is always safe). Rebuild/repaint slots so flat shading takes effect on already-built tiles.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(globe): GlobeStyle axis + setStyle + faceted (flat-shaded) style`.

---

### Task 2: The Terraced style (quantized displacement)

**Files:**
- Modify: `src/views/worldMesh.ts` (a `quantizeBands` helper + a banded radius path)
- Modify: `src/views/globe.ts` (`buildTileSlot` uses the banded builder for `terraced`)
- Test: `src/views/worldMesh.test.ts`

**Interfaces:**
- Produces: `export function quantizeBands(elevationM: number, bandM: number): number` — snaps an elevation to the floor of its band (`Math.floor(elevationM / bandM) * bandM`).
- Terraced reuses `buildTileGeometry`/`buildRegionTileGeometry` but with the elevation passed through `quantizeBands` before displacement, and flat shading (from Task 1).

- [ ] **Step 1: Write the failing test** for `quantizeBands`:

```ts
import { quantizeBands } from './worldMesh';
describe('quantizeBands', () => {
  it('snaps elevation down to its band floor', () => {
    expect(quantizeBands(0, 200)).toBe(0);
    expect(quantizeBands(199, 200)).toBe(0);
    expect(quantizeBands(200, 200)).toBe(200);
    expect(quantizeBands(-1, 200)).toBe(-200); // below sea level steps down
    expect(quantizeBands(650, 200)).toBe(600);
  });
});
```

- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement `quantizeBands`** and thread an optional `bandM?: number` into the geometry builders (or a wrapping `elevationTransform`): when set, the `radiusAtLatLon` closure applies `quantizeBands(sample, bandM)` to the sampled elevation before scaling. Default (undefined) = today's continuous path (Smooth). In `globe.ts`, `terraced` calls the builder with the campaign's band size (a constant `TERRACE_BAND_M`, tune in the visual pass; start ~250 m) and flat shading.
- [ ] **Step 4: Run it, confirm it passes.** Add a geometry-level test: a terraced tile's distinct radius values are a small finite set (banded), not continuous.
- [ ] **Step 5: Commit** — `feat(globe): terraced style — banded elevation quantization`.

---

### Task 3: Voxel geometry — base tiles

The keystone build. Per-cell extruded blocks with **real walls at height steps**.

**Files:**
- Modify: `src/views/worldMesh.ts` (`buildVoxelTileGeometry`)
- Test: `src/views/worldMesh.test.ts`

**Interfaces:**
- Produces:
  ```ts
  export function buildVoxelTileGeometry(
    tiles: TilesScene,
    tile: TileId,
    radius: number,
    reliefScale: number,
    colorAt: (i: number) => RGB,
    opts: { cellsPerEdge: number; bandM: number },
  ): THREE.BufferGeometry;
  ```
  `cellsPerEdge` is the voxel granularity (bounded, e.g. 48–96 — far below `TILE_QUADS=64`'s vertex lattice is fine; voxels are meant to read chunky). `bandM` quantizes block-top elevation (reuse `quantizeBands`).

**Algorithm (document it in a comment, implement flat-shaded, non-indexed):**
1. Sub-sample the tile into a `cellsPerEdge × cellsPerEdge` grid of cells. Each cell has a center (lat, lon) → unit vector (via the cube-sphere projection already used by `tileGrid`) and a **banded** elevation `e = quantizeBands(sampleElevationBilinear(tiles, lat, lon), bandM)` → radius `r(e)`.
2. **Top face:** the cell's four corners projected to the sphere at radius `r(e)` — two triangles, flat normal, `colorAt(cell)` on all four verts (per-cell flat color; nearest, not blended).
3. **Walls (the keystone):** for each of the 4 edge-neighbors, if the neighbor's banded radius `r_n < r(e)`, emit a vertical quad from this cell's edge at `r(e)` down to `r_n` (two triangles, side-face normal, cell color darkened by a fixed `VOXEL_CLIFF_DARKEN` ~0.75 for read). Tile-boundary neighbors (outside this tile) use this tile's own edge value — a seam one band tall at most, acceptable; note it.
4. No skirt (walls seal the silhouette). Build as a raw `BufferGeometry` with `position`, `normal`, `color` attributes (non-indexed, flat shading).

- [ ] **Step 1: Write the failing test** on a hand-built 2×2 fixture (mock `TilesScene` whose `sampleElevationBilinear` returns a known step — e.g. one cell high, three low):

```ts
describe('buildVoxelTileGeometry', () => {
  it('emits a top face per cell and a wall ONLY where a cell is higher than a neighbor', () => {
    const geom = buildVoxelTileGeometry(stepTiles, tile0, 1, 1,
      () => [1, 1, 1], { cellsPerEdge: 2, bandM: 100 });
    const pos = geom.getAttribute('position');
    // 4 cells × 2 top tris = 8 top tris minimum; the single high cell has
    // walls only toward its LOWER neighbors (2 interior edges) → 2 wall quads.
    expect(triangleCount(geom)).toBe(8 /*tops*/ + 2 * 2 /*two walls*/);
    // no wall between two equal-height cells:
    expect(hasWallBetweenEqualCells(geom)).toBe(false);
  });
  it('colors each cell flat (all of a cell\'s top verts share one color)', () => { /* ... */ });
});
```

- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement `buildVoxelTileGeometry`** per the algorithm. Keep it allocation-light (typed arrays sized up front).
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(worldMesh): buildVoxelTileGeometry — extruded blocks with inter-cell cliffs`.

---

### Task 4: Voxel region geometry + globe wiring

**Files:**
- Modify: `src/views/worldMesh.ts` (`buildVoxelRegionTileGeometry`)
- Modify: `src/views/globe.ts` (`buildTileSlot` + repaint branch on style)
- Test: `src/views/worldMesh.test.ts`, `src/views/globe.test.ts`

**Interfaces:**
- Produces: `buildVoxelRegionTileGeometry(region, radius, reliefScale, colorAt, opts)` — same shape as Task 3 but sourced from a `RegionScene` (use `region.elevation_m[node]` and `regionPatchUnits`; `colorAt(node)` since a region node index IS its color index).
- `globe.ts`: `buildTileSlot` branches on `activeStyle` — `voxel` → voxel builder (base or region variant depending on whether a region patch is cached for the tile), else the existing smooth/terraced/faceted path.

- [ ] **Step 1: Write the failing test** — a region voxel build on a small fixture yields tops + step-walls; and a globe repaint on a living lens (temperature) updates the voxel per-cell colors (the repaint path must handle the voxel color layout, not just the vertex-lattice layout).
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** the region builder; branch `buildTileSlot`; ensure the per-slot `repaint` writes per-cell colors for voxel geometry (store the cell→vertex color mapping on the slot so `repaint` can rewrite it without a full rebuild). Voxel `cellsPerEdge` for region tiles is the same bounded granularity constant.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(globe): voxel style wired through slots + region tiles + living-lens repaint`.

---

### Task 5: HUD Globe Style control

**Files:**
- Modify: `src/ui/hud.ts` (a `<select class="hud-style">` + `onStyle`/`setStyle`, mirroring the lens control)
- Modify: `src/main.ts` (wire `onStyle → globe.setStyle`; URL parity with the lens)
- Test: `src/ui/hud.test.ts`

**Interfaces:**
- `HudCallbacks.onStyle(id: GlobeStyle): void`; `Hud.setStyle(style: GlobeStyle): void` (reflects state on the `<select>`).
- The control is visible only in the Globe view (same visibility rule the lens control uses).

- [ ] **Step 1: Write the failing test** — the style `<select>` lists the four styles, and changing it fires `onStyle` with the selected id; `setStyle('voxel')` sets `select.value = 'voxel'`.
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** the control (copy the lens `<select>` construction idiom at `hud.ts` ~143; class `hud-style`, `name="style-select"`, options from a `GLOBE_STYLES` list). Wire `main.ts`: on change → `globe.setStyle(id)`; if the lens is URL-addressable in `syncUrl`, add `style` with parity, else leave both out.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(hud): globe style dropdown (smooth/voxel/terraced/faceted)`.

---

### Task 6: Deeper LOD ceiling + reachable close zoom

**Files:**
- Modify: `src/views/cubeSphere.ts` (`LOD_CDLOD_MAX_LEVEL`, any dependent bound)
- Modify: `src/views/globe.ts` (globe camera `minDistance` + near clip)
- Test: `src/views/cubeSphere.test.ts`

**Interfaces:**
- No signature changes — constant + camera-limit tuning. The CDLOD selector (`selectTiles`) already requests deeper region tiles once the ceiling is raised.

- [ ] **Step 1: Write the failing test** — at a low camera altitude the CDLOD selector reaches the new deeper max level (e.g. assert `selectTiles(...)` at a near altitude includes a tile at `level > 4`). Pin the new ceiling constant's value in the test so a later change is deliberate.
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** — raise `LOD_CDLOD_MAX_LEVEL` (start at 6; the visual/perf pass in Task 7 may settle it). Lower the Globe view's controls `minDistance` and the camera near-clip together so you can approach close enough to see the deeper tiles without clipping.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(globe): deeper client-side LOD ceiling + reachable close zoom`.

---

### Task 7: e2e + perf pass (the visual/measure gate)

**Files:**
- Modify: `e2e/smoke.spec.ts`
- Possibly modify: `src/views/globe.ts` / `worldMesh.ts` (granularity cap or worker offload IF measured to stall)

- [ ] **Step 1: e2e — style switch.** In the Globe view, select each style from `.hud-style`; assert the globe canvas re-renders without error (viewport screenshot, per the Vantage e2e discipline — element-stability times out on WebGL). Assert no console errors across the switches.
- [ ] **Step 2: e2e — deep zoom.** Wheel-zoom the globe to the new near limit; assert it doesn't crash and a deeper region tile is requested (spy on the region-request path or assert canvas stability at depth).
- [ ] **Step 3: Perf measurement.** Script a deep zoom in Voxel style in a real browser (Playwright + Chrome), capture the per-frame cost of the voxel slot rebuild. **If** a rebuild stalls the frame (The Region's jerkiness class), reduce `cellsPerEdge` and/or move the voxel geometry build into the region worker. Record the measured numbers in the retrospective. Do **not** pre-optimize without a measured stall (YAGNI).
- [ ] **Step 4: Run the full suite** — `npm run build && npm test && npx playwright test`. All green.
- [ ] **Step 5: Commit** — `test(orrery): e2e style-switch + deep-zoom; voxel perf pass`.

---

## Notes for the executor

- **Visual passes are mandatory for the aesthetic tasks** (2, 3, 4, 6): build the client, open it in a real browser, and confirm the look before marking done — jsdom/vitest paint nothing. Voxel must show visible cliffs; Terraced must show steps; Faceted must show facets; deeper zoom must actually reveal more detail. Screenshot each and surface them.
- **Perf is the primary risk** (spec §10). Keep voxel granularity bounded and walls step-gated from the start; only escalate to a worker offload if measured.
- The four styles share one lens color source — never special-case a lens inside a style.
