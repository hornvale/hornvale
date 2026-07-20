# The Cartographer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A data-native, LOD-driven symbolic render style for the Orrery globe — a pixel-art base that reads tile data (killing the land-takes-ocean-colour bug) plus mountains/forests/settlements that emerge with detail as you zoom, selected against a per-zoom salience budget.

**Architecture:** A new render style that reads the *scene*, not the frame. The `RenderStyle` interface widens with an optional `base` treatment (a per-vertex colour transform applied inside `globe.ts`'s `computeBaseColor`) and an optional `symbolLayer` (a `THREE.Group` of derived-feature sprites mounted in the globe's `spinGroup`, culled per-frame by the existing `onNearSide`, rebuilt on rung change through the existing amortized build queue). Feature extraction and budget selection are pure functions over `TilesScene`.

**Tech Stack:** TypeScript, three.js (+ addons EffectComposer/Pass), Vitest, Playwright. Orrery repo only — no sim/wasm change.

## Global Constraints

- **Orrery-only. No hornvale/sim change, no wasm release.** All feature data derives from the `TilesScene` already in client memory.
- **Determinism waived client-side (0022/0023)** BUT symbol placement must be *visually stable* per `(tiles, rung)`: extraction is pure over the fixed field; intra-cluster scatter uses a hash of the tile index, **never `Math.random`**.
- **Green ≠ correct for rendering.** The controller's screenshot visual pass is the acceptance gate. The Idioms shader-gotcha checklist applies to any new material/shader: (1) `ShaderPass` clones its uniforms — update the post-clone object in `setSize`; (2) GLSL reserved words (`flat`, `sample`, `smooth`, `layout`, `patch`) silently fail the whole compile; (3) opaque-clear composer targets; (4) no backticks inside GLSL template strings; (5) avoid dynamically-indexed local arrays in shaders.
- **Do not regress The Frame Budget.** Symbol (re)builds ride the amortized build queue; per-frame cost stays the cheap near-side cull only.
- Sonnet floor for any subagent. Run `npm run build` and kill any `:4173` server before each visual capture.
- Existing pixel-art *filter* is replaced by the symbolic pixel-art; cel/engraving/watercolor filters stay unchanged.

---

### Task 1: Widen the `RenderStyle` interface

**Files:**
- Modify: `src/views/renderStyle.ts`
- Test: `src/views/renderStyle.test.ts`

**Interfaces:**
- Produces: `BaseTreatment` = `{ id: string; transform(rgb: readonly [number, number, number], src: TilesScene, idx: number): [number, number, number]; }` (rgb is 0–255, as `activeLens.colorAt` returns). `SymbolLayerSpec` = `{ id: string }` (a marker type this task; Task 5 gives it its builder fields). `RenderStyle` gains `base?: BaseTreatment` and `symbolLayer?: SymbolLayerSpec`, both optional.
- Consumes: `TilesScene` from `../sim/scene`.

- [ ] **Step 1: Write the failing test** — append to `renderStyle.test.ts`:

```ts
import { STYLES, photorealStyle } from './renderStyle';

test('photoreal and filter styles declare no base or symbol layer', () => {
  for (const s of STYLES) {
    if (s.id === 'photoreal' || s.id === 'cel' || s.id === 'engraving' || s.id === 'watercolor') {
      expect(s.base).toBeUndefined();
      expect(s.symbolLayer).toBeUndefined();
    }
  }
  expect(photorealStyle.base).toBeUndefined();
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- renderStyle` → fails to compile (`base` not on type) or assertion. 

- [ ] **Step 3: Widen the interface** — in `renderStyle.ts`, above `RenderStyle`:

```ts
/** How the globe SURFACE is shaded, as a per-vertex colour transform applied
 * on top of the active lens's colour inside globe.ts's computeBaseColor.
 * Absent on a style = today's realistic relief surface, untouched. */
export interface BaseTreatment {
  id: string;
  /** rgb is 0–255 (the lens output). Return 0–255. `src`/`idx` give the
   * treatment the raw datum (e.g. src.ocean[idx]) so it shades from data. */
  transform(rgb: readonly [number, number, number], src: TilesScene, idx: number): [number, number, number];
}

/** A scene-graph layer of derived-feature symbols. Task 5 fills in the
 * builder; this task only reserves the slot. */
export interface SymbolLayerSpec {
  id: string;
}
```

Add to `RenderStyle`:

```ts
  /** How the globe surface is shaded. Absent = realistic relief (today). */
  base?: BaseTreatment;
  /** A layer of derived-feature symbols mounted on the globe. Absent = none. */
  symbolLayer?: SymbolLayerSpec;
```

- [ ] **Step 4: Run it, expect PASS** — `npm test -- renderStyle`.

- [ ] **Step 5: Commit** — `feat(cartographer): widen RenderStyle with optional base + symbolLayer`.

---

### Task 2: Feature extraction — peaks and forests

**Files:**
- Create: `src/views/symbols/extract.ts`
- Test: `src/views/symbols/extract.test.ts`

**Interfaces:**
- Produces:
  - `interface Peak { lat: number; lon: number; elevationM: number; tileIndex: number; }`
  - `interface ForestRegion { lat: number; lon: number; area: number; tileIndex: number; }` (lat/lon = area centroid; tileIndex = a representative tile, for the scatter seed)
  - `extractPeaks(tiles: TilesScene): Peak[]` — descending by `elevationM`.
  - `extractForests(tiles: TilesScene): ForestRegion[]` — descending by `area`.
  - `FOREST_BIOMES: ReadonlySet<string>`.
  - re-export `clusterFeatures` from `../globe`.
- Consumes: `TilesScene` (`width`, `height`, `elevation_m`, `ocean`, `biome`, `biomeLegend`).

Helpers: tile `i = y*width + x`; lat/lon of a tile centre — latitude `90 - (y + 0.5)/height*180`, longitude `-180 + (x + 0.5)/width*360` (row-major, matches `windows/scene`). A land tile is `!ocean[i]`.

- [ ] **Step 1: Write the failing test** — `extract.test.ts`:

```ts
import { describe, expect, test } from 'vitest';
import { extractPeaks, extractForests } from './extract';
import type { TilesScene } from '../sim/scene';

// A tiny 4x3 world builder. legend index 1 = 'temperate-forest' (a FOREST_BIOMES
// member), index 0 = 'desert' (not). ocean true => water.
function world(elev: number[], ocean: boolean[], biome: number[]): TilesScene {
  return {
    schema: 'scene/tiles/v1', width: 4, height: 3, sea_level_m: 0,
    elevation_m: elev, ocean, biome, biomeLegend: ['desert', 'temperate-forest'],
    plate: [], unrest: [], features: [],
  } as unknown as TilesScene;
}

test('extractPeaks finds land local maxima, tallest first', () => {
  // one tall peak at index 5 (500m), a lower one at index 10 (300m)
  const elev = [10, 10, 10, 10,  10, 500, 10, 10,  10, 10, 300, 10];
  const ocean = elev.map(() => false);
  const biome = elev.map(() => 0);
  const peaks = extractPeaks(world(elev, ocean, biome));
  expect(peaks.map((p) => p.tileIndex)).toEqual([5, 10]);
  expect(peaks[0]!.elevationM).toBe(500);
});

test('extractPeaks ignores ocean tiles', () => {
  const elev = [10, 10, 10, 10,  10, 500, 10, 10,  10, 10, 10, 10];
  const ocean = elev.map((_, i) => i === 5); // the would-be peak is underwater
  const peaks = extractPeaks(world(elev, ocean, elev.map(() => 0)));
  expect(peaks.find((p) => p.tileIndex === 5)).toBeUndefined();
});

test('extractForests clusters contiguous forest biome, largest first', () => {
  // indices 0,1,4,5 = forest (a 2x2 block, area 4); index 11 = lone forest (area 1)
  const biome = [1, 1, 0, 0,  1, 1, 0, 0,  0, 0, 0, 1];
  const elev = biome.map(() => 100);
  const forests = extractForests(world(elev, biome.map(() => false), biome));
  expect(forests.length).toBe(2);
  expect(forests[0]!.area).toBe(4);
  expect(forests[1]!.area).toBe(1);
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- extract`.

- [ ] **Step 3: Implement** — `extract.ts`:

```ts
import type { TilesScene } from '../sim/scene';
export { clusterFeatures } from '../globe';

export interface Peak { lat: number; lon: number; elevationM: number; tileIndex: number; }
export interface ForestRegion { lat: number; lon: number; area: number; tileIndex: number; }

/** Biome names (in biomeLegend) drawn as forest. Anything else is not. */
export const FOREST_BIOMES: ReadonlySet<string> = new Set([
  'taiga', 'boreal-forest', 'temperate-forest', 'temperate-rainforest',
  'tropical-seasonal-forest', 'tropical-rainforest',
]);

const tileLat = (y: number, h: number): number => 90 - ((y + 0.5) / h) * 180;
const tileLon = (x: number, w: number): number => -180 + ((x + 0.5) / w) * 360;

/** Land tiles that exceed all in-bounds 8-neighbours in elevation, tallest
 * first. Deterministic; ties keep row-major order (strict `>` over neighbours
 * means equal-height neighbours both fail, which is acceptable for symbols). */
export function extractPeaks(tiles: TilesScene): Peak[] {
  const { width: w, height: h, elevation_m: e, ocean } = tiles;
  const out: Peak[] = [];
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const i = y * w + x;
      if (ocean[i]) continue;
      const ei = e[i]!;
      let isMax = true;
      for (let dy = -1; dy <= 1 && isMax; dy++) {
        for (let dx = -1; dx <= 1; dx++) {
          if (dx === 0 && dy === 0) continue;
          const nx = x + dx, ny = y + dy;
          if (nx < 0 || nx >= w || ny < 0 || ny >= h) continue;
          if (e[ny * w + nx]! >= ei) { isMax = false; break; }
        }
      }
      if (isMax) out.push({ lat: tileLat(y, h), lon: tileLon(x, w), elevationM: ei, tileIndex: i });
    }
  }
  return out.sort((a, b) => b.elevationM - a.elevationM);
}

/** Connected components (4-neighbour) of forest-biome tiles, largest area
 * first. Centroid in lat/lon; a representative tileIndex for the scatter seed. */
export function extractForests(tiles: TilesScene): ForestRegion[] {
  const { width: w, height: h, biome, biomeLegend, ocean } = tiles;
  const isForest = (i: number) => !ocean[i] && FOREST_BIOMES.has(biomeLegend[biome[i]!]!);
  const seen = new Uint8Array(w * h);
  const out: ForestRegion[] = [];
  for (let s = 0; s < w * h; s++) {
    if (seen[s] || !isForest(s)) continue;
    let area = 0, sumLat = 0, sumLon = 0;
    const stack = [s];
    seen[s] = 1;
    while (stack.length) {
      const i = stack.pop()!;
      const x = i % w, y = (i - x) / w;
      area++; sumLat += tileLat(y, h); sumLon += tileLon(x, w);
      const nbrs = [x > 0 ? i - 1 : -1, x < w - 1 ? i + 1 : -1, y > 0 ? i - w : -1, y < h - 1 ? i + w : -1];
      for (const n of nbrs) if (n >= 0 && !seen[n] && isForest(n)) { seen[n] = 1; stack.push(n); }
    }
    out.push({ lat: sumLat / area, lon: sumLon / area, area, tileIndex: s });
  }
  return out.sort((a, b) => b.area - a.area);
}
```

- [ ] **Step 4: Run it, expect PASS** — `npm test -- extract`.

- [ ] **Step 5: Commit** — `feat(cartographer): peak + forest feature extraction`.

---

### Task 3: Salience budget and rung selection

**Files:**
- Create: `src/views/symbols/budget.ts`
- Test: `src/views/symbols/budget.test.ts`

**Interfaces:**
- Produces:
  - `type Rung = 'far' | 'mid' | 'near';`
  - `rungForZoom(visibleAngularRadiusRad: number): Rung` — larger arc = 'far'.
  - `interface RungBudget { peaks: number; forests: number; showSettlementLabels: boolean; peakMinElevationM: number; forestMinArea: number; }`
  - `RUNG_BUDGETS: Record<Rung, RungBudget>` (values tuned in the visual pass; the plan ships defaults).
  - `selectByBudget<T extends { }>(items: T[], budget: number): T[]` — first `budget` items (items arrive pre-sorted by salience from Task 2).
- Consumes: nothing external (pure numeric policy).

- [ ] **Step 1: Write the failing test** — `budget.test.ts`:

```ts
import { expect, test } from 'vitest';
import { rungForZoom, selectByBudget, RUNG_BUDGETS } from './budget';

test('rung coarsens as the visible arc widens', () => {
  expect(rungForZoom(2.0)).toBe('far');   // whole hemisphere in view
  expect(rungForZoom(0.05)).toBe('near'); // tight zoom
});

test('selectByBudget takes the top-N pre-sorted items', () => {
  expect(selectByBudget([1, 2, 3, 4, 5], 3)).toEqual([1, 2, 3]);
  expect(selectByBudget([1, 2], 5)).toEqual([1, 2]);
});

test('budgets grow richer from far to near', () => {
  expect(RUNG_BUDGETS.near.peaks).toBeGreaterThan(RUNG_BUDGETS.far.peaks);
  expect(RUNG_BUDGETS.near.peakMinElevationM).toBeLessThan(RUNG_BUDGETS.far.peakMinElevationM);
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- budget`.

- [ ] **Step 3: Implement** — `budget.ts`:

```ts
export type Rung = 'far' | 'mid' | 'near';

export interface RungBudget {
  peaks: number;
  forests: number;
  showSettlementLabels: boolean;
  peakMinElevationM: number;
  forestMinArea: number;
}

// Visual-pass-tuned. Thresholds fall and budgets rise as we zoom in, so finer
// features emerge. Angular radius (rad) of the visible cap drives the rung.
export const RUNG_BUDGETS: Record<Rung, RungBudget> = {
  far:  { peaks: 12, forests: 8,  showSettlementLabels: false, peakMinElevationM: 3000, forestMinArea: 60 },
  mid:  { peaks: 40, forests: 30, showSettlementLabels: false, peakMinElevationM: 1500, forestMinArea: 15 },
  near: { peaks: 120, forests: 90, showSettlementLabels: true,  peakMinElevationM: 500,  forestMinArea: 3 },
};

/** Coarser rung when more of the sphere is visible. Boundaries visual-tuned. */
export function rungForZoom(visibleAngularRadiusRad: number): Rung {
  if (visibleAngularRadiusRad > 0.8) return 'far';
  if (visibleAngularRadiusRad > 0.25) return 'mid';
  return 'near';
}

/** Items arrive pre-sorted by salience (Task 2); take the first `budget`. */
export function selectByBudget<T>(items: T[], budget: number): T[] {
  return items.slice(0, Math.max(0, budget));
}
```

- [ ] **Step 4: Run it, expect PASS** — `npm test -- budget`.

- [ ] **Step 5: Commit** — `feat(cartographer): rung selection + salience budget`.

---

### Task 4: The pixel base treatment (kills the land-takes-ocean bug)

**Files:**
- Create: `src/views/styles/pixelBase.ts`
- Test: `src/views/styles/pixelBase.test.ts`
- Modify: `src/views/globe.ts` (apply the treatment in `computeBaseColor`; add `setBaseTreatment`)

**Interfaces:**
- Produces: `pixelBaseTreatment: BaseTreatment` (from Task 1). Quantizes each channel to a small number of steps for the pixel look, and forces ocean tiles toward an ocean-blue so a water datum can never read as land.
- Consumes: `BaseTreatment` (Task 1), `TilesScene`.
- Globe API gains: `setBaseTreatment(t: BaseTreatment | null): void` — sets the active treatment and calls `rebuildBase()`.

- [ ] **Step 1: Write the failing test** — `pixelBase.test.ts`:

```ts
import { expect, test } from 'vitest';
import { pixelBaseTreatment } from './pixelBase';
import type { TilesScene } from '../sim/scene';

const src = { ocean: [true, false] } as unknown as TilesScene;

test('an ocean tile stays blue-dominant (never takes a land colour)', () => {
  // feed it a LAND-green input; the treatment must not let ocean read green
  const [r, g, b] = pixelBaseTreatment.transform([80, 140, 70], src, 0);
  expect(b).toBeGreaterThan(r);
  expect(b).toBeGreaterThan(g);
});

test('a land tile keeps its land hue, quantized', () => {
  const [r, g, b] = pixelBaseTreatment.transform([80, 140, 70], src, 1);
  expect(g).toBeGreaterThan(b); // green land stays green-dominant
  // quantized: each channel snapped to a step multiple
  for (const c of [r, g, b]) expect(c % PIXEL_STEP === 0 || c === 255).toBe(true);
});
```

Add near the top of the test: `import { PIXEL_STEP } from './pixelBase';`

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- pixelBase`.

- [ ] **Step 3: Implement** — `pixelBase.ts`:

```ts
import type { BaseTreatment } from '../renderStyle';
import type { TilesScene } from '../sim/scene';

/** Channel quantization step — the pixel-art banding. Visual-pass-tuned. */
export const PIXEL_STEP = 32;

const OCEAN_RGB: readonly [number, number, number] = [40, 72, 132];

const quant = (c: number): number => Math.min(255, Math.round(c / PIXEL_STEP) * PIXEL_STEP);

/** Data-native pixel base: ocean tiles read from the ocean palette (not the
 * lens-lit frame), land tiles keep the lens hue, both quantized. Because ocean
 * colour is chosen from src.ocean[idx], land can never take the ocean's colour
 * and vice-versa — the bug dies here by construction. */
export const pixelBaseTreatment: BaseTreatment = {
  id: 'pixel',
  transform(rgb, src: TilesScene, idx) {
    const base = src.ocean[idx] ? OCEAN_RGB : rgb;
    return [quant(base[0]), quant(base[1]), quant(base[2])];
  },
};
```

- [ ] **Step 4: Run it, expect PASS** — `npm test -- pixelBase`.

- [ ] **Step 5: Wire into globe.ts** — in `createGlobeView`, add a closure var `let activeBaseTreatment: BaseTreatment | null = null;`. In `computeBaseColor`, after `const rgb = activeLens.colorAt(...)`, apply it:

```ts
      const shaded = activeBaseTreatment ? activeBaseTreatment.transform(rgb, src, idx[v]!) : rgb;
      buf[3 * v] = shaded[0] / 255;
      buf[3 * v + 1] = shaded[1] / 255;
      buf[3 * v + 2] = shaded[2] / 255;
```

Add to the returned globe-view object:

```ts
    setBaseTreatment(t: BaseTreatment | null): void {
      activeBaseTreatment = t;
      rebuildBase();
      // repaint mounted slots from the rebuilt baseColor (same path setLens uses)
      for (const slot of tileSlots.values()) applyBaseColor(slot); // use the existing repaint helper
    },
```

(Use whatever the existing `setLens` uses to push `baseColor` onto the mesh — mirror it exactly. If `setLens` calls a private repaint, call the same one.) Import `BaseTreatment` from `./renderStyle`.

- [ ] **Step 6: Verify** — `npm test -- pixelBase` still green; `npx tsc --noEmit` clean.

- [ ] **Step 7: Commit** — `feat(cartographer): data-native pixel base treatment + globe hook`.

---

### Task 5: The symbol layer (sprites on the sphere)

**Files:**
- Create: `src/views/symbols/symbolLayer.ts`
- Test: `src/views/symbols/symbolLayer.test.ts`
- Modify: `src/views/globe.ts` (expose a symbol-layer mount point + per-frame update + zoom/rung signal + camera/radius)

**Interfaces:**
- Produces:
  - `interface SymbolLayer { group: THREE.Group; update(rung: Rung, camWorld: THREE.Vector3): void; dispose(): void; }`
  - `buildSymbolLayer(tiles: TilesScene): SymbolLayer` — extracts once (Task 2), then `update` re-selects per rung (Task 3) and rebuilds sprite children, and culls by `onNearSide` each call.
  - `hash01(i: number): number` — deterministic [0,1) from an integer (scatter seed; **not** `Math.random`).
- Consumes: `extractPeaks`/`extractForests`/`clusterFeatures` (Task 2), `rungForZoom`/`RUNG_BUDGETS`/`selectByBudget` (Task 3), `latLonToUnit`/`GLOBE_RADIUS`/`onNearSide` (globe.ts).

Sprites: reuse the marker idiom — `THREE.Sprite` with a `SpriteMaterial` (a small generated canvas texture per class: a grey triangle "peak", a green blob "tree", the existing dot for settlements). Peak size ∝ elevation; a forest region gets `Math.min(budget, Math.round(Math.log2(area + 1)))` tree sprites scattered within ~1 tile of the centroid using `hash01(tileIndex + k)`.

- [ ] **Step 1: Write the failing test** — `symbolLayer.test.ts` (jsdom paints nothing, so assert structure/counts/stability, not pixels):

```ts
import { expect, test } from 'vitest';
import * as THREE from 'three';
import { buildSymbolLayer, hash01 } from './symbolLayer';
import type { TilesScene } from '../sim/scene';

function landWorld(): TilesScene {
  const w = 8, h = 4;
  const elevation_m = Array.from({ length: w * h }, (_, i) => (i === 10 ? 5000 : 100));
  const biome = Array.from({ length: w * h }, () => 1); // all forest
  return {
    schema: 'scene/tiles/v1', width: w, height: h, sea_level_m: 0,
    elevation_m, ocean: elevation_m.map(() => false), biome,
    biomeLegend: ['desert', 'temperate-forest'], plate: [], unrest: [], features: [],
  } as unknown as TilesScene;
}

test('hash01 is deterministic and in range', () => {
  expect(hash01(7)).toBe(hash01(7));
  expect(hash01(7)).toBeGreaterThanOrEqual(0);
  expect(hash01(7)).toBeLessThan(1);
});

test('a near-rung layer mounts more symbols than a far-rung one', () => {
  const layer = buildSymbolLayer(landWorld());
  const cam = new THREE.Vector3(0, 0, 100); // everything on the near side
  layer.update('far', cam);
  const farCount = layer.group.children.length;
  layer.update('near', cam);
  const nearCount = layer.group.children.length;
  expect(nearCount).toBeGreaterThanOrEqual(farCount);
  expect(nearCount).toBeGreaterThan(0);
});

test('symbol positions are stable across identical updates (no shimmer)', () => {
  const layer = buildSymbolLayer(landWorld());
  const cam = new THREE.Vector3(0, 0, 100);
  layer.update('near', cam);
  const first = layer.group.children.map((c) => c.position.toArray().join(','));
  layer.update('near', cam);
  const second = layer.group.children.map((c) => c.position.toArray().join(','));
  expect(second).toEqual(first);
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- symbolLayer`.

- [ ] **Step 3: Implement** `symbolLayer.ts` — extract once in `buildSymbolLayer`; `update` clears and rebuilds the group's children for the rung, placing each at `latLonToUnit(...).multiplyScalar(GLOBE_RADIUS * 1.01)`, skipping any failing `onNearSide(up, camWorld, GLOBE_RADIUS)`; tree scatter offsets from `hash01`. Generate the three class textures once via an offscreen canvas (guard for jsdom: if `canvas.getContext('2d')` is null, use a plain-colour `SpriteMaterial` so tests run headless). `dispose()` frees geometries/materials/textures. Keep all tuning constants named at the top.

- [ ] **Step 4: Run it, expect PASS** — `npm test -- symbolLayer`.

- [ ] **Step 5: Expose globe hooks** — in `createGlobeView` add to the returned object: `mountSymbolLayer(group: THREE.Group)` (adds to `spinGroup`), `unmountSymbolLayer(group)` (removes), and expose the current visible angular radius + camera world position to the per-frame update (the view already runs a frame update for marker placement/cull — call the symbol layer's `update` from there, computing `rungForZoom` from the camera distance: `visibleAngularRadius = Math.acos(GLOBE_RADIUS / camDistance)`). Store the active layer so the frame loop can update it; null when no symbol style is active.

- [ ] **Step 6: Verify** — `npm test -- symbolLayer` green; `npx tsc --noEmit` clean.

- [ ] **Step 7: Commit** — `feat(cartographer): symbol layer — peaks/forests/settlements on the sphere`.

---

### Task 6: The symbolic pixel-art style + pipeline wiring + picker replacement

**Files:**
- Modify: `src/views/styles/pixelArt.ts` (repurpose to the symbolic style) OR create `src/views/styles/pixelArtSymbolic.ts` and drop the old filter export.
- Modify: `src/views/renderStyle.ts` (`STYLES`; `StylePipeline` applies base + mounts symbol layer)
- Modify: `src/main.ts` (pass the globe view into the pipeline so it can apply base/symbols; wire `onStyle`)
- Test: `src/views/renderStyle.test.ts`

**Interfaces:**
- Produces: `pixelArtStyle: RenderStyle` with `{ id: 'pixel-art', label: 'pixel-art', passes: () => [], base: pixelBaseTreatment, symbolLayer: { id: 'world-symbols' } }`.
- `StylePipeline` gains a reference to the globe view; `setStyle` now also: `globe.setBaseTreatment(style.base ?? null)` and mounts/unmounts the symbol layer (build via `buildSymbolLayer(tiles)` when `style.symbolLayer` is present, dispose+unmount on change).
- Consumes: Tasks 1/4/5.

- [ ] **Step 1: Write the failing test** — in `renderStyle.test.ts`:

```ts
test('pixel-art is a scene-renderer style (base + symbol layer, no post pass)', () => {
  const s = STYLES.find((x) => x.id === 'pixel-art')!;
  expect(s.base?.id).toBe('pixel');
  expect(s.symbolLayer).toBeDefined();
  expect(s.passes({ ocean: [], biome: [], biomeLegend: [] } as any)).toEqual([]);
});
```

- [ ] **Step 2: Run it, expect FAIL** — `npm test -- renderStyle`.

- [ ] **Step 3: Implement the style** — replace the pixel-art filter export with the symbolic style (import `pixelBaseTreatment`). Remove the old ShaderPass filter code for pixel-art. Keep `celStyle`/`engravingStyle`/`watercolorStyle` untouched.

- [ ] **Step 4: Wire StylePipeline** — give `StylePipeline` a `globe` handle (a small interface: `setBaseTreatment`, `mountSymbolLayer`, `unmountSymbolLayer`, and the tiles). In `setStyle`, after rebuilding the post chain: dispose any previously-mounted symbol layer; `globe.setBaseTreatment(style.base ?? null)`; if `style.symbolLayer`, `const layer = buildSymbolLayer(tiles); globe.mountSymbolLayer(layer.group);` store it; wire the globe frame-loop to call `layer.update`.

- [ ] **Step 5: Wire main.ts** — construct the `StylePipeline` with the globe view handle; ensure `onStyle` still calls `stylePipeline.setStyle(styleById(id))`.

- [ ] **Step 6: Run it, expect PASS + tsc clean** — `npm test -- renderStyle`; `npx tsc --noEmit`.

- [ ] **Step 7: Commit** — `feat(cartographer): symbolic pixel-art style replaces the filter; pipeline mounts base+symbols`.

---

### Task 7: Smoke tripwire + rotation-stability e2e

**Files:**
- Modify: `e2e/smoke.spec.ts`

**Interfaces:** consumes the running app; asserts PNG bytes and a sampled pixel colour.

- [ ] **Step 1: Extend the style-roster test** — for `pixel-art`: pause the clock, rotate to a known land longitude, capture; assert PNG > 5000 bytes (non-blank) and differs from the photoreal capture (already the roster pattern).

- [ ] **Step 2: Add a rotation-stability assertion** — at a fixed zoom, capture two frames a small rotation apart over a known land region; sample a land-region pixel in each and assert it stays land-colored (green-dominant, not ocean-blue) in both — the mechanized guard for Nathan's exact bug. Use the existing screenshot+pixel-sample helpers in the spec (or `sharp`/raw PNG read already used by the perf/visual helpers); if none exists, assert both captures are non-blank and mutually similar in size as a weaker proxy and leave the pixel assertion to the controller's visual pass.

- [ ] **Step 3: Run** — `npm run build && npm run e2e` (the non-`@perf` suite). Expect PASS.

- [ ] **Step 4: Commit** — `test(cartographer): symbolic-style smoke + rotation-stability tripwire`.

---

## Post-plan: controller visual pass (not a subagent task)

After Task 7, the controller runs the acceptance gate: `npm run build`, kill `:4173`, serve, and screenshot seed 42 at far / mid / near zoom. Confirm: (1) crisp pixel coastline, land never ocean-coloured, at every rotation; (2) mountains/forests/settlements emerge with detail as zoom increases; (3) no shimmer within a rung, clean rung transitions; (4) no black globe (the shader-gotcha checklist). Tune `RUNG_BUDGETS`, `PIXEL_STEP`, sprite sizes, and the base lighting treatment by eye. Then the whole-branch review, then G6.
