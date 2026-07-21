# The Cartographer — Transition Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Pivot the Orrery so the globe is a clean overview and pixel-art RPG detail lives on a new flat rectangular **map** rung reached by zooming in — a discrete crossfade handoff (not a geometric morph).

**Architecture:** Add a third zoom rung (`system → globe → map`). The `map` rung is a 2D orthographic scene rendering the region-tile under the camera as a crisp `NearestFilter` pixel texture (curated biome palette, reused) plus 2D symbol sprites (reused `extract`/`budget`). The globe reverts to a clean overview. `ZoomController` generalizes from a 2-endpoint [0,1] ease to a 3-rung ladder position in [0,2] with three crossfade opacities.

**Tech Stack:** TypeScript, three.js, Vitest, Playwright. Orrery only — no sim/wasm change.

## Global Constraints

- **Orrery-only. No sim/wasm change.** The map renders existing `scene/tiles-region/v1` data.
- **Discrete crossfade handoff, NOT a geometric morph** (keystone). The three rungs each own a renderer/camera; the transition crossfades canvas opacity.
- **Determinism waived client-side (0022/0023)** but placement must be *stable* per `(region, internal-zoom)` — reuse `hash01`, never `Math.random`.
- **Green ≠ correct for rendering.** The controller's screenshot visual pass is the acceptance gate (rebuild + kill `:4173` before each capture; the app serves at `http://127.0.0.1:4173/orrery/` via `node e2e/serve.mjs`). The Idioms shader-gotcha checklist still applies to any shader/texture.
- Sonnet floor for subagents.

---

## STAGE 1 — Globe cleanup (revert pixel-art off the sphere)

### Task 1: Strip the symbol layer + decoration suppressions off the globe

**Files:** Modify `src/views/renderStyle.ts`, `src/views/globe.ts`, `src/main.ts`. Keep (do NOT delete) `src/views/symbols/extract.ts`, `budget.ts`, `styles/pixelBase.ts`, `symbols/symbolLayer.ts` — they relocate to the map rung in Stage 4.

**Interfaces:**
- Consumes: the current `StylePipeline` (with `globe` handle + symbol mounting), `GlobeView` (with `setBaseTreatment`/`mountSymbolLayer`/`unmountSymbolLayer`).
- Produces: a globe with NO symbol layer and NO pixel decorations; `StylePipeline` back to post-process-only.

- [ ] **Step 1: Remove the scene-renderer wiring from `StylePipeline`** (`renderStyle.ts`): delete the `activeSymbolLayer` field, the `buildSymbolLayer`/mount/unmount + `setBaseTreatment` calls in `setStyle`, and the `globe: GlobeStyleTarget` constructor param + `GlobeStyleTarget` interface + the `buildSymbolLayer` import. `setStyle` returns to managing only the post-process pass chain. Remove `base`/`symbolLayer` from the pixel-art style object (it becomes a plain no-op/removed style — see Step 3).
- [ ] **Step 2: Revert `globe.ts` globe-only changes.** Remove: the symbol-layer mount point (`mountSymbolLayer`/`unmountSymbolLayer`/`activeSymbolLayer` + the `activeSymbolLayer.update(...)` call in the frame loop + the `rungForZoom` import), the `setBaseTreatment` glint/waves/water-sphere/`activeMaterial` swap, the `icy && !activeBaseTreatment?.unlit` gate (back to `if (icy)`), and the `flatMaterial`/`activeMaterial` unlit path (back to the single lit `material`). Whether `setBaseTreatment` + `activeBaseTreatment` survive at all depends on Step 4's look decision — default: remove them.
- [ ] **Step 3: Remove the pixel-art scene style from the picker** (`renderStyle.ts` `STYLES`): drop `pixelArtStyle` (the scene renderer). Delete `src/views/styles/pixelBase.ts`'s *use as a globe base* but KEEP the file's `PIXEL_LAND_RGB`/ocean tones/`pixelBaseTreatment` export (Stage 3 imports the palette). If nothing imports `pixelArt.ts` anymore, delete `pixelArt.ts`. Cel/engraving/watercolor filter styles stay.
- [ ] **Step 4: Update `main.ts`** — construct `StylePipeline` without the `globe` arg. Confirm `npx tsc --noEmit` clean, `npm run build` succeeds, `npm test` green.
- [ ] **Step 5: Commit** `revert(cartographer): strip pixel-art symbols + decorations off the globe (pivot stage 1)`.

- [ ] **Step 6 (controller, not a subagent): visual pass + the globe-look decision.** Capture seed 42 globe. Present Nathan two options — (a) the clean smooth relief/biome overview as now restored, (b) a *softened* pixel-biome base (a light, non-hard-edged pixel treatment, no symbols/waves). This is a flagged G-decision (spec §4). Implement whichever Nathan picks (if (b), keep a minimal `setBaseTreatment` + a softened treatment; if (a), the revert stands). Commit the outcome.

**Stage 1 deliverable:** a clean globe overview, green tests, Nathan's look chosen.

---

## STAGE 2 — The map rung + the three-rung transition

### Task 2: Generalize `ZoomController` to three rungs

**Files:** Modify `src/views/zoom.ts`; test `src/views/zoom.test.ts`.

**Interfaces:**
- Produces: `ZoomTarget = 'system' | 'globe' | 'map'`; `RUNG_INDEX: Record<ZoomTarget, number>` = `{system:0, globe:1, map:2}`; `ZoomState = { value: number /* [0,2] ladder pos */, systemOpacity, globeOpacity, mapOpacity }`; `ZoomController` easing `value` toward the target rung's index; `wheelHandoff` returning `'to-globe'|'to-system'|'to-map'|'to-globe-from-map'|null`.
- Consumes: existing `lerp`/`easeInOutCubic`.

- [ ] **Step 1: Write the failing test** (`zoom.test.ts`, add):

```ts
import { ZoomController, wheelHandoff, opacitiesFor } from './zoom';

test('ladder position eases toward the target rung index', () => {
  const z = new ZoomController(1000);
  z.jumpTo('globe');
  expect(z.valueAt(0)).toBeCloseTo(1); // globe = 1
  z.setTarget('map', 0);
  expect(z.valueAt(500)).toBeGreaterThan(1); // easing toward 2
  expect(z.valueAt(1000)).toBeCloseTo(2); // map = 2
});

test('opacities cross-fade the three canvases across the ladder', () => {
  expect(opacitiesFor(0)).toMatchObject({ systemOpacity: 1, globeOpacity: 0, mapOpacity: 0 });
  expect(opacitiesFor(1)).toMatchObject({ systemOpacity: 0, globeOpacity: 1, mapOpacity: 0 });
  expect(opacitiesFor(2)).toMatchObject({ systemOpacity: 0, globeOpacity: 0, mapOpacity: 1 });
  const mid = opacitiesFor(1.5); // globe->map crossfade
  expect(mid.globeOpacity).toBeCloseTo(0.5);
  expect(mid.mapOpacity).toBeCloseTo(0.5);
  expect(mid.systemOpacity).toBe(0);
});

test('wheelHandoff crosses globe<->map at the dolly limits', () => {
  expect(wheelHandoff('globe', -1, 1.0, 1.0, 10)).toBe('to-map');        // wheel-in past floor
  expect(wheelHandoff('map', 1, 9.99, 1.0, 10)).toBe('to-globe-from-map'); // wheel-out past ceiling
  expect(wheelHandoff('system', -1, 1.0, 1.0, 10)).toBe('to-globe');     // unchanged
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- zoom`.
- [ ] **Step 3: Implement.** Replace the 2-state internals:

```ts
export type ZoomTarget = 'system' | 'globe' | 'map';
export const RUNG_INDEX: Record<ZoomTarget, number> = { system: 0, globe: 1, map: 2 };

export interface ZoomState {
  value: number; // ladder position in [0,2]
  systemOpacity: number;
  globeOpacity: number;
  mapOpacity: number;
}

/** Triangular crossfade: each rung's opacity peaks at its index and falls
 * linearly to 0 at the adjacent index. */
export function opacitiesFor(pos: number): Omit<ZoomState, 'value'> {
  const tri = (center: number) => Math.max(0, 1 - Math.abs(pos - center));
  return { systemOpacity: tri(0), globeOpacity: tri(1), mapOpacity: tri(2) };
}
```

In `ZoomController`: `target: ZoomTarget = 'system'`; `valueAt` eases `fromValue → RUNG_INDEX[target]`; `jumpTo(t)` sets `fromValue = RUNG_INDEX[t]`; `stateAt` returns `{ value, ...opacitiesFor(value) }`. Keep the retarget-from-current-value behavior.

`wheelHandoff` (extend):
```ts
export type HandoffIntent = 'to-globe' | 'to-system' | 'to-map' | 'to-globe-from-map' | null;
export function wheelHandoff(view, deltaY, distance, minDistance, maxDistance): HandoffIntent {
  if (view === 'system' && deltaY < 0 && distance <= minDistance * 1.01) return 'to-globe';
  if (view === 'globe' && deltaY > 0 && distance >= maxDistance * 0.99) return 'to-system';
  if (view === 'globe' && deltaY < 0 && distance <= minDistance * 1.01) return 'to-map';
  if (view === 'map'   && deltaY > 0 && distance >= maxDistance * 0.99) return 'to-globe-from-map';
  return null;
}
```

- [ ] **Step 4: Run it, expect PASS + tsc clean.** Other `ZoomState`/`wheelHandoff` callers in `main.ts` will now type-error — that's Task 4's integration; leave `main.ts` for Task 4 (this task is `zoom.ts` + its test only; if tsc must pass here, add the `mapOpacity`/`'map'` handling in `main.ts` minimally in Task 4). Run `npm test -- zoom`.
- [ ] **Step 5: Commit** `feat(cartographer): three-rung ZoomController + globe<->map handoff`.

### Task 3: The `map` rung scaffold (orthographic scene + placeholder quad)

**Files:** Create `src/views/mapView.ts` + `mapView.test.ts`.

**Interfaces:**
- Produces: `createMapView(): MapView` where `MapView = { scene: THREE.Scene; camera: THREE.OrthographicCamera; setRegion(region: RegionScene | null): void; render(renderer: THREE.WebGLRenderer): void; dispose(): void }`. For this task `setRegion` just stores the region and shows a **placeholder** flat quad (a solid-color `PlaneGeometry` + `MeshBasicMaterial`) sized to the region's aspect; Stage 3 replaces the placeholder with the pixel texture.
- Consumes: `RegionScene` from `../sim/scene`.

- [ ] **Step 1: Failing test** — `mapView.test.ts`: `createMapView()` returns a scene with an orthographic camera; after `setRegion(fakeRegion)` the scene has exactly one mesh child; `setRegion(null)` clears it; `dispose()` empties the scene. (Headless — assert scene-graph structure, not pixels.)
- [ ] **Step 2: Run, expect FAIL.**
- [ ] **Step 3: Implement** the orthographic scene + placeholder quad + `setRegion`/`render`/`dispose`.
- [ ] **Step 4: Run, expect PASS + tsc clean.**
- [ ] **Step 5: Commit** `feat(cartographer): map-rung scaffold (orthographic scene + placeholder)`.

### Task 4: Wire the third renderer + the ladder into `main.ts`

**Files:** Modify `src/main.ts` (and `src/ui/hud.ts` only if the view-button cycle needs a third state).

**Interfaces:**
- Consumes: Task 2's `ZoomController`/`wheelHandoff`/`opacitiesFor`, Task 3's `createMapView`, the existing `requestRegion`/`onRegion` worker path, the globe's sub-camera lat/lon (`subsolarPoint` idiom / the camera direction → lat/lon).
- Produces: a working `system ↔ globe ↔ map` ladder with crossfaded canvases.

- [ ] **Step 1:** Add a third `<canvas>` + `THREE.WebGLRenderer` for the map rung (mirror how `systemRenderer`/`globeRenderer` are created + sized + pixel-ratio-capped), stacked in the DOM; construct `createMapView()`.
- [ ] **Step 2:** In the render loop, read `zoom.stateAt(now)`; set each canvas's `style.opacity` from `systemOpacity`/`globeOpacity`/`mapOpacity`; render only rungs with opacity > 0 (skip fully-hidden ones — keep the "only active rung renders" discipline). The system camera dolly uses `Math.min(value, 1)` (the system↔globe segment); the globe↔map segment (value 1→2) is pure crossfade with the globe held at its closest framing.
- [ ] **Step 3:** Handle `wheelHandoff` results: `'to-map'` → compute the globe's sub-camera lat/lon, map it to a `{face,level,ix,iy}` region tile (reuse the cube-sphere mapping the globe's region upgrades already use — find it in `globe.ts`), `requestRegion(tile)`, and `zoom.setTarget('map', now)`; when the region arrives (`onRegion`), call `mapView.setRegion(region)`. `'to-globe-from-map'` → `zoom.setTarget('globe', now)`. Show a loading state (placeholder stays) until the region arrives.
- [ ] **Step 4:** Update the `trueScaleOn`/caption/view-button per-rung records to include `'map'` (a caption like "the flat map: a region, drawn"). Confirm `tsc`/`build`/`npm test` green.
- [ ] **Step 5: Commit** `feat(cartographer): wire the three-rung ladder + map crossfade in main`.

### Task 5: Stage-2 smoke tripwire (e2e)

**Files:** Modify `e2e/smoke.spec.ts`.

- [ ] **Step 1:** Add a test: load `#seed=42&view=globe`, wheel-in repeatedly to trigger `to-map`, wait for the map canvas opacity → 1, assert its screenshot is non-blank (> 5000 bytes); wheel-out, assert the globe returns. (Placeholder quad is fine — this guards the transition, not the art.)
- [ ] **Step 2:** `npm run build && npm run e2e` → PASS.
- [ ] **Step 3: Commit** `test(cartographer): map-rung transition smoke tripwire`.

**Stage 2 deliverable:** zoom into the globe → crossfade to a flat placeholder map → zoom out → back. Green.

---

## STAGE 3 — The flat pixel-art base

### Task 6: `mapTexture.ts` — region grid → curated-palette pixel texture

**Files:** Create `src/views/mapTexture.ts` + `mapTexture.test.ts`.

**Interfaces:**
- Produces: `regionPixelTexture(region: RegionScene): THREE.DataTexture` — one RGBA texel per region node, colored by the reused `PIXEL_LAND_RGB`/ocean depth-tone (import from `./styles/pixelBase`), `NearestFilter` min/mag (hard pixels), `needsUpdate = true`. Also export the pure helper `regionPixelRGBA(region): Uint8Array` for the test.
- Consumes: `RegionScene` (`biome`/`ocean`/`biomeLegend`/`elevation_m`/`sea_level_m`, `(samples+1)²`), the curated palette.

- [ ] **Step 1: Failing test** (`mapTexture.test.ts`): a small fake region (all-ocean and mixed) → `regionPixelRGBA` returns `4·(samples+1)²` bytes; an ocean node is blue-dominant; a `temperate-forest` land node is green-dominant; a node is never near-white (the curated-palette guarantee). Pure — no GPU.
- [ ] **Step 2: Run, expect FAIL.**
- [ ] **Step 3: Implement** `regionPixelRGBA` (walk the grid, reuse the palette resolution from `pixelBase` — refactor `pixelBase` to export a pure `pixelColorFor(src, idx): [r,g,b]` both it and this share) + `regionPixelTexture` wrapping it in a `DataTexture`.
- [ ] **Step 4: Run, expect PASS + tsc clean.**
- [ ] **Step 5: Commit** `feat(cartographer): region -> curated pixel DataTexture`.

### Task 7: Render the pixel texture on the map quad

**Files:** Modify `src/views/mapView.ts` (+ test).

- [ ] **Step 1:** Replace the placeholder material with a `MeshBasicMaterial({ map: regionPixelTexture(region) })` on the quad, sized to the region's aspect; dispose the old texture on `setRegion`. Update the `mapView.test.ts` to assert the mesh's material has a `map` after `setRegion`.
- [ ] **Step 2:** `tsc`/`build`/`npm test` green.
- [ ] **Step 3: Commit** `feat(cartographer): flat pixel-art map base render`.
- [ ] **Step 4 (controller): visual pass** — zoom into the globe on seed 42, confirm the map rung shows a crisp, undistorted flat pixel-art biome map of the region. Tune palette/pixel crispness if needed.

**Stage 3 deliverable:** a crisp flat pixel-art regional map — pixel-art without sphere distortion.

---

## STAGE 4 — Map RPG symbols + internal LOD

### Task 8: 2D symbol placement on the map (reuse extract/budget)

**Files:** Create `src/views/mapSymbols.ts` + test.

**Interfaces:**
- Produces: `buildMapSymbols(region: RegionScene): MapSymbols` where `MapSymbols = { group: THREE.Group; update(internalZoom: number): void; dispose() }`. `update` maps `internalZoom` → a `Rung` (reuse `rungForZoom`/`RUNG_BUDGETS`/`selectByBudget`), runs `extractPeaks`/`extractForests` on the region grid (they already accept a `TilesScene`-shaped object — `RegionScene` has the same `width`/`height`? if not, adapt via a shim), and places **plain 2D sprites** at grid `(x,y)` positions on the map plane — NO billboarding, NO limb cull, NO clearance-float (all sphere artifacts are gone). Wave-marks on ocean nodes, tastefully strided. Reuse the sprite textures from `symbolLayer.ts` (extract the `buildPeakMaterial`/`buildTreeMaterial`/`buildWaveMaterial` into a shared `symbols/sprites.ts`).
- Consumes: `extract.ts`, `budget.ts`, the shared sprite materials.

- [ ] **Step 1: Failing test:** a mixed region → `buildMapSymbols`, `update(nearZoom)` mounts peak/tree/wave sprites (assert by `userData.kind` counts); positions are stable across identical `update` calls (no `Math.random`); a far `update` mounts fewer than a near one.
- [ ] **Step 2: Run, expect FAIL.**
- [ ] **Step 3: Implement**; mount `MapSymbols.group` into the map scene in `mapView.ts`, driven by the map camera's zoom.
- [ ] **Step 4: Run, expect PASS + tsc/build green.**
- [ ] **Step 5: Commit** `feat(cartographer): flat-map RPG symbols (peaks/forests/waves), internal LOD`.

### Task 9: Biome-signature icons + landmark cities (closest internal zoom)

**Files:** Modify `src/views/mapSymbols.ts` (+ test).

- [ ] **Step 1:** At the closest internal rung, add biome-signature icon sprites — volcano (`unrest`+high `elevation`), cactus (desert), mushroom (a forest/damp biome) — chosen per node from the datum, and landmark markers for large `features` (cities). Deterministic selection (`hash01` for any spacing). Test: a region with a high-unrest peak node yields a volcano icon at near zoom; icons absent at far zoom.
- [ ] **Step 2:** `tsc`/`build`/`npm test` green.
- [ ] **Step 3: Commit** `feat(cartographer): biome-signature icons + landmark cities on the flat map`.
- [ ] **Step 4 (controller): visual pass** — the full zoom arc on seed 42 (globe overview → crossfade → flat map → zoom in → forests/mountains/waves → icons/landmarks). Tune sizes/density/icons with Nathan.

**Stage 4 deliverable:** the flat pixel-art RPG map with progressive detail — the campaign's payoff.

---

## After Stage 4
Whole-branch code review (most-capable model), then **G6** (chronicle telling the pivot arc, retrospective, MAP-61 repoint, keystone refreeze if any, memory), then merge + orrery deploy.
