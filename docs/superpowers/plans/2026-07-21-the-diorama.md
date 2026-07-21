# The Diorama Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the Orrery's map rung a **Voxel-2.5D** style — the region as a flat heightfield of voxel blocks under a fixed-isometric orthographic camera — selectable via a map style switch, with the current flat pixel-art map kept as the other style.

**Architecture:** Reuse The Massing's voxel block/wall core (`worldMesh.ts` `buildVoxelBlocks`/`quantizeBands`/`VOXEL_CLIFF_DARKEN`) re-instantiated on a **flat planar grid** (cell → `(x, z)` position, `y` = banded elevation height) instead of a cube-sphere. `mapView` gains a `MapStyle` switch that swaps both the mounted mesh (voxel heightfield vs today's textured plane) and the camera pose (fixed isometric vs top-down). A HUD control (mirroring The Massing's globe-style dropdown) selects it.

**Tech Stack:** TypeScript, three.js, Vitest (unit), Playwright (e2e). Orrery repo only.

## Global Constraints

- **Orrery / client-only.** No hornvale/producer change, **no wasm release** (world-wasm stays v12), no new scene fields, no epoch. (Determinism waived client-side, 0022/0023.)
- **No `Math.random`** in any geometry/color/placement path.
- **Style ⟂ Lens.** The voxel diorama colors from the SAME lens the pixel map uses (per-region-node lens color, nearest per cell). A style changes geometry/camera, never *what* color a cell is.
- **Pixel path unchanged.** Switching to `'pixel'` restores today's exact map (top-down `OrthographicCamera` + `regionPixelTexture` plane + `mapSymbols`), verbatim.
- **Reuse, don't re-implement.** The block top/wall emission + banding must come from The Massing's shared core (`buildVoxelBlocks` and friends), factored — not copy-pasted. Only the flat-grid corner/height mapping is new.
- **Voxel keystone:** real vertical cliff walls where a cell is higher than a neighbor (strict `<`).
- Run `npm run build` (tsc) + `npm test` (vitest) before every commit; `npx prettier -w` touched files.
- Every commit ends with the trailer:
  `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: Flat-grid voxel heightfield geometry

**Files:**
- Modify: `src/views/worldMesh.ts` (`buildVoxelHeightfieldGeometry` + factoring)
- Test: `src/views/worldMesh.test.ts`

**Interfaces:**
- Produces:
  ```ts
  export function buildVoxelHeightfieldGeometry(
    region: RegionScene,
    colorAt: (nodeIndex: number) => RGB,
    opts: { extent: number; heightScale: number; bandM: number },
  ): THREE.BufferGeometry;
  ```
  A flat diorama in local space: the region's `(samples+1)²` grid laid on the X–Z plane spanning `[-extent/2, extent/2]²`, each cell an extruded block whose top `y = heightScale * quantizeBands(cellElevationM, bandM) / REFERENCE_RADIUS_M` (or a simpler `heightScale * banded / maxRelief` normalization — pick one, document it), with **vertical walls where a cell's banded height exceeds an edge-neighbor's** (strict `<`), per-cell flat color from `colorAt`, cliff darkened by `VOXEL_CLIFF_DARKEN`, flat shading. Non-indexed BufferGeometry (position/normal/color).

**Reuse:** study `buildVoxelBlocks` (the cube-sphere voxel core from The Massing). Prefer factoring its top/wall/band emission so this shares it, differing only in the **corner→position mapping** (planar `(x, height, z)` here vs `faceUnit × radius` there). If `buildVoxelBlocks` can take a `cornerPosition(col, row, height) → [x,y,z]` callback, pass a planar one; if factoring cleanly is infeasible, a small shared `emitBlock`/`emitWall` helper both call is acceptable — but do not duplicate the whole two-pass loop.

- [ ] **Step 1: Write the failing test** on a hand-built region fixture with a known elevation step (reuse the region fixture the existing `worldMesh.test.ts` region tests use; scale its elevation so one cell is a clear band higher than its neighbors):

```ts
describe('buildVoxelHeightfieldGeometry', () => {
  it('emits a top face per cell and a wall ONLY where a cell is higher than a neighbor', () => {
    const geom = buildVoxelHeightfieldGeometry(stepRegion, () => [1, 1, 1],
      { extent: 2, heightScale: 1, bandM: 100 });
    // tops for every cell + walls only at the real step(s); mirror the globe
    // voxel test's triangle-count assertion (worldMesh.test.ts, Task-3 of The Massing)
    expect(triangleCount(geom)).toBe(/* tops */ + /* the step's walls */);
    expect(hasWallBetweenEqualCells(geom)).toBe(false);
  });
  it('lays the grid on the X–Z plane within ±extent/2, height along +Y', () => {
    const geom = buildVoxelHeightfieldGeometry(flatRegion, () => [1,1,1],
      { extent: 4, heightScale: 1, bandM: 100 });
    const pos = geom.getAttribute('position');
    // all X and Z within [-2, 2]; a flat (equal-elevation) region → all tops at one Y
    expect(maxAbsComponent(pos, 'x')).toBeLessThanOrEqual(2 + 1e-6);
    expect(maxAbsComponent(pos, 'z')).toBeLessThanOrEqual(2 + 1e-6);
  });
  it('colors each cell flat (a cell\'s top verts share one color); walls darkened', () => { /* ... */ });
});
```

- [ ] **Step 2: Run it, confirm it fails** — `npm test -- worldMesh` → FAIL.
- [ ] **Step 3: Implement** `buildVoxelHeightfieldGeometry`, factoring the shared block/wall core from `buildVoxelBlocks`. Confirm RED was real via a mutation check (flip `<`→`<=`; the wall assertion must break) as The Massing's Task 3 did.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(worldMesh): buildVoxelHeightfieldGeometry — flat-grid voxel diorama`.

---

### Task 2: MapStyle switch + fixed-isometric camera

**Files:**
- Modify: `src/views/mapView.ts`
- Test: `src/views/mapView.test.ts`

**Interfaces:**
- `export type MapStyle = 'voxel' | 'pixel';`
- `MapView.setStyle(style: MapStyle): void` (default `'voxel'`).
- The isometric camera pose is a `const` (pitch ≈ 35.264°, yaw 45°) applied when `'voxel'` is active; `'pixel'` restores the top-down pose.

**Behavior:**
- `'voxel'`: mount the `buildVoxelHeightfieldGeometry` mesh (with a `MeshStandardMaterial`/`MeshLambertMaterial`, `vertexColors:true`, `flatShading:true`) + a single `DirectionalLight` (so tops read brighter than walls) + an ambient fill; reposition the `OrthographicCamera` to a fixed isometric offset looking at the diorama center, with the frustum extent framing the region (blocks included) without clipping near/far.
- `'pixel'`: today's exact path — `regionPixelTexture` plane + `mapSymbols`, top-down camera at `(0,0,10)`.
- `setRegion` builds via the active style; a lens change (if the map recolors on lens) rebuilds the mesh (the map is a static snapshot — a full rebuild on region/lens change, no per-frame repaint).

- [ ] **Step 1: Write the failing test** — `setStyle('voxel')` mounts a voxel-geometry mesh (has walls / non-plane geometry) and sets the isometric camera pose (camera position is the iso offset, not `(0,0,10)`); `setStyle('pixel')` restores a `PlaneGeometry` mesh + the top-down camera. Assert the camera pose values.
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** the `MapStyle` branch, the light, and the isometric camera pose (extract the pose math into named constants; document the isometric angle). Keep the pixel branch verbatim.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(mapView): MapStyle switch — voxel diorama vs pixel, fixed-isometric camera`.

---

### Task 3: HUD Map Style control + wiring

**Files:**
- Modify: `src/ui/hud.ts`, `src/main.ts`
- Test: `src/ui/hud.test.ts`

**Interfaces:**
- `HudCallbacks.onMapStyle(id: MapStyle): void`; `Hud.setMapStyle(style: MapStyle): void`.
- A `<select class="hud-map-style">` mirroring the globe-style control (`onGlobeStyle`/`setGlobeStyle` from The Massing — study it; note the unrelated pre-existing `RenderStyle` axis, do NOT conflate). Visible per the same rule the globe-style control uses (The Massing found the lens control is always-visible → parity).

- [ ] **Step 1: Write the failing test** — the map-style `<select>` lists `voxel`/`pixel`; changing it fires `onMapStyle` with the id; `setMapStyle('pixel')` sets `select.value='pixel'`. Mirror the globe-style hud test.
- [ ] **Step 2: Run it, confirm it fails.**
- [ ] **Step 3: Implement** the control + `main.ts` wiring (`onMapStyle → mapView.setStyle`); URL parity with the globe style if it's URL-addressable, else none.
- [ ] **Step 4: Run it, confirm it passes.**
- [ ] **Step 5: Commit** — `feat(hud): map style dropdown (voxel / pixel)`.

---

### Task 4: e2e + isometric framing visual pass

**Files:**
- Modify: `e2e/smoke.spec.ts`
- Possibly modify: `src/views/mapView.ts` (camera framing constants, IF the visual pass shows clipping/spill)

- [ ] **Step 1: e2e — map style switch.** In the Map view (`#seed=42&view=map` or select Map from `.hud-view`), select each map style via `.hud-map-style`; assert the map canvas re-renders with no console errors and no throw (viewport screenshot / `pageerror`+`console.error` → `toEqual([])`, mirroring The Massing's Task-7 e2e). Do NOT use pixel baselines.
- [ ] **Step 2: e2e — pixel restores.** Assert switching to `'pixel'` still renders the flat map (canvas non-blank, no error) — the kept path.
- [ ] **Step 3: Visual framing pass (controller-run).** The controller builds the client and screenshots the voxel map to confirm the isometric diorama frames cleanly (blocks visible, no clipping/spill, reads as a tilted relief model). If clipping/spill is seen, adjust the camera frustum/pose constants in `mapView.ts` and re-shoot. (This step is completed by the controller reading PNGs — the implementer's job is that the e2e + build are green and the camera constants are cleanly factored for tuning.)
- [ ] **Step 4: Run the full suite** — `npm run build && npm test && npx playwright test`. All green.
- [ ] **Step 5: Commit** — `test(orrery): e2e map style-switch + pixel-restore`.

---

## Notes for the executor

- **Visual pass is mandatory for the isometric look** (the controller runs it after Task 3/4) — jsdom/vitest paint nothing. The diorama must read as a tilted voxel relief model; the framing constants are the tuning knob.
- Reuse The Massing's voxel core (DRY); the only new geometry is the flat corner/height mapping.
- The voxel diorama and the pixel map share one lens color source — never special-case a lens inside a style.
