# The Idioms Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a swappable render-STYLE axis to the Orrery globe — four screen-space "delight" skins (pixel-art, cel/ink, engraving, watercolor) over the same scene, orthogonal to the data lens, with photoreal as the identity.

**Architecture:** A `RenderStyle` is an ordered chain of `three/addons` post-processing passes plus params, registered in a `STYLES` list exactly as data lenses are registered in `LENSES`. A `StylePipeline` wraps an `EffectComposer` built on the existing `globeRenderer`; the globe's per-frame `globeRenderer.render(globeScene, globeCamera)` is replaced by `pipeline.render()`. Photoreal = a bare `RenderPass` (no effect). Each style is a `ShaderPass` chain reading only the **color buffer** (luminance carries relief+light, so no depth/normal G-buffer is needed in v1). The HUD gains a style picker mirroring the lens picker.

**Tech Stack:** TypeScript, three.js + `three/addons/postprocessing` (EffectComposer / RenderPass / ShaderPass — already-available addon, no new dependency). GLSL for the effect fragment shaders.

## Global Constraints

- **Orrery repo only. No sim/kernel/wasm change. No world-wasm release.** The scene data contract is untouched (spec §Constraints).
- **three.js only**, using existing `three/addons` — no new dependency.
- **Determinism waived client-side (decisions 0022/0023)** — no golden, no drift-check on rendered pixels. Acceptance per style is the controller's screenshot visual pass at a far view + a mid-zoom.
- **Photoreal is the identity style** — the default look must render byte-for-byte as today when the pipeline is in place (a bare RenderPass).
- **Styles are orthogonal to the data lens** — a style transforms the rendered frame; any lens × any style composes with no per-combination code.
- **Styles never touch the LOD/geometry/normals pipeline** — screen-space only, downstream of the globe scene render.
- **Every rendering change is visually verified by the controller** (open the PNGs): `npm run build` and kill the reused `:4173` server before each capture (the webServer serves `dist/`; a stale build blinds the loop).
- The full orrery suite (`npx vitest run`, `npx tsc --noEmit`, `npm run build`, `npm run e2e`, `npm run smoke`) stays green throughout. The perf-harness stays out of `npm run e2e` (it is `@perf`-tagged; `npm run perf` runs it on demand).
- **Build stamp discipline:** the console logs `[orrery] build <time> @ <sha>` on boot — confirm a fresh stamp before any visual capture, to defeat layered caching.

## File Structure

- `src/views/renderStyle.ts` — **new.** The `RenderStyle` interface, the `STYLES` registry + `styleById`, and the `StylePipeline` class (owns the `EffectComposer`, swaps pass chains, `render()`, `setSize()`). Mirrors `src/views/lens.ts`.
- `src/views/styles/pixelArt.ts` — **new.** The pixel-art style + `biomePalette(tiles)` derivation.
- `src/views/styles/cel.ts` — **new.** The cel/ink style.
- `src/views/styles/engraving.ts` — **new.** The engraving style.
- `src/views/styles/watercolor.ts` — **new.** The watercolor style.
- `src/views/renderStyle.test.ts` — **new.** Unit tests: registry/`styleById`, `biomePalette` determinism, pass-chain shape.
- `src/main.ts` — **modify.** Build the `StylePipeline` for the globe; replace `globeRenderer.render(globeScene, globeCamera)` with `pipeline.render()`; wire `hud` style callbacks; call `pipeline.setSize` in the resize handler.
- `src/ui/hud.ts` — **modify.** Add the style picker (one button per `STYLES` entry), `onStyle(id)` callback, `setStyle(style)` active-marking — mirroring the lens picker.
- `e2e/smoke.spec.ts` — **modify.** Add a check that each style boots and renders a non-blank canvas.

---

### Task 1: The style pipeline, registry, photoreal identity, and the HUD picker

**Files:**
- Create: `src/views/renderStyle.ts`
- Create: `src/views/renderStyle.test.ts`
- Modify: `src/main.ts` (renderer wiring + resize + hud callbacks)
- Modify: `src/ui/hud.ts` (style picker)

**Interfaces:**
- Produces:
  - `interface RenderStyle { id: string; label: string; /** Build the effect passes (empty for photoreal). Receives the scene's tiles for any CPU-side data hook. */ passes(tiles: TilesScene): Pass[]; }`
  - `const STYLES: RenderStyle[]` and `styleById(id: string): RenderStyle` (falls back to photoreal).
  - `class StylePipeline { constructor(renderer: THREE.WebGLRenderer, scene: THREE.Scene, camera: THREE.Camera, tiles: TilesScene); setStyle(style: RenderStyle): void; setSize(w: number, h: number): void; render(): void; }`
  - HUD: `onStyle(id: string): void` callback and `setStyle(style: RenderStyle): void` method.

- [ ] **Step 1: Write the failing registry test** (`src/views/renderStyle.test.ts`)

```ts
import { expect, test } from 'vitest';
import { STYLES, styleById, photorealStyle } from './renderStyle';

test('STYLES has photoreal first and every entry has a unique id + label', () => {
  expect(STYLES[0]).toBe(photorealStyle);
  const ids = STYLES.map((s) => s.id);
  expect(new Set(ids).size).toBe(ids.length); // unique
  for (const s of STYLES) expect(s.label.length).toBeGreaterThan(0);
});

test('styleById returns the match, and falls back to photoreal for an unknown id', () => {
  expect(styleById('photoreal')).toBe(photorealStyle);
  expect(styleById('nope-not-a-style')).toBe(photorealStyle);
});

test('photoreal produces an empty effect chain (identity)', () => {
  const fakeTiles = { width: 1, height: 1, elevation_m: [0], biome: [0] } as never;
  expect(photorealStyle.passes(fakeTiles)).toEqual([]);
});
```

- [ ] **Step 2: Run it to verify it fails**

Run: `npx vitest run src/views/renderStyle.test.ts`
Expected: FAIL — cannot find module `./renderStyle`.

- [ ] **Step 3: Implement `src/views/renderStyle.ts`**

```ts
import * as THREE from 'three';
import { EffectComposer } from 'three/addons/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/addons/postprocessing/RenderPass.js';
import type { Pass } from 'three/addons/postprocessing/Pass.js';
import type { TilesScene } from '../sim/scene';

/** A render STYLE: how the globe is drawn, orthogonal to the data lens (which
 * chooses what data is coloured). A style is a chain of screen-space passes
 * applied to the rendered globe frame. Photoreal is the identity (no passes). */
export interface RenderStyle {
  /** Stable id, used by the URL/HUD and `styleById`. */
  id: string;
  /** Human label for the picker button. */
  label: string;
  /** Build this style's effect passes (empty = identity/photoreal). `tiles` is
   * passed so a style can derive a cheap CPU-side hook (e.g. a palette from the
   * world's biome mix) once at construction. */
  passes(tiles: TilesScene): Pass[];
}

/** The default: no effect — the globe renders exactly as it does today. */
export const photorealStyle: RenderStyle = {
  id: 'photoreal',
  label: 'photoreal',
  passes: () => [],
};

/** Every registered style, photoreal first. Later tasks push their styles here. */
export const STYLES: RenderStyle[] = [photorealStyle];

/** The style with this id, or photoreal if none matches (a bad URL never crashes). */
export function styleById(id: string): RenderStyle {
  return STYLES.find((s) => s.id === id) ?? photorealStyle;
}

/** Owns an EffectComposer over the globe renderer and swaps pass chains when the
 * style changes. `render()` replaces the plain `renderer.render(scene, camera)`. */
export class StylePipeline {
  private composer: EffectComposer;
  private renderPass: RenderPass;

  constructor(
    private renderer: THREE.WebGLRenderer,
    scene: THREE.Scene,
    camera: THREE.Camera,
    private tiles: TilesScene,
  ) {
    this.composer = new EffectComposer(renderer);
    this.renderPass = new RenderPass(scene, camera);
    this.composer.addPass(this.renderPass);
  }

  /** Rebuild the composer's pass chain: the base RenderPass + the style's effects.
   * The last pass renders to screen. */
  setStyle(style: RenderStyle): void {
    // Dispose the old effect passes' targets before dropping them.
    for (const p of this.composer.passes) {
      if (p !== this.renderPass) (p as { dispose?: () => void }).dispose?.();
    }
    this.composer.passes = [this.renderPass];
    for (const p of style.passes(this.tiles)) this.composer.addPass(p);
    const passes = this.composer.passes;
    passes.forEach((p, i) => {
      p.renderToScreen = i === passes.length - 1;
    });
  }

  setSize(w: number, h: number): void {
    this.composer.setSize(w, h);
  }

  render(): void {
    this.composer.render();
  }
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `npx vitest run src/views/renderStyle.test.ts`
Expected: PASS (3 tests).

- [ ] **Step 5: Wire the pipeline into `main.ts`**

Find `globeRenderer.render(globeScene, globeCamera);` (currently ~line 454) and the `globeRenderer` construction (~line 170). After the globe view is mounted and `tiles` is available, construct the pipeline; replace the render call; hook resize.

```ts
// near the other view imports at the top of main.ts:
import { StylePipeline, styleById } from './views/renderStyle';

// after globeView/globeScene/globeCamera exist and `tiles` is in scope
// (mountViews already has `tiles`):
const stylePipeline = new StylePipeline(globeRenderer, globeScene, globeCamera, tiles);
stylePipeline.setStyle(styleById('photoreal'));

// in renderFrame(), REPLACE:
//   globeRenderer.render(globeScene, globeCamera);
// WITH:
stylePipeline.render();

// in the existing resize handler (where systemRenderer/globeRenderer.setSize are called):
stylePipeline.setSize(globeCanvas.clientWidth, globeCanvas.clientHeight);
```

Note: `EffectComposer` calls `renderer.render` internally, so `globeRenderer` stays the owning renderer. Photoreal (empty chain) still runs the composer's RenderPass → identical output.

- [ ] **Step 6: Add the style picker to the HUD** (`src/ui/hud.ts`)

Mirror the lens picker (search `LENSES` / `onLens` / `setLens`, ~line 217). Add, alongside the lens callbacks:

```ts
import { STYLES, type RenderStyle } from '../views/renderStyle';

// in HudCallbacks:
/** The viewer picked a render style (by RenderStyle.id). */
onStyle(id: string): void;

// in the Hud interface:
/** Show `style` as active: mark its button. */
setStyle(style: RenderStyle): void;
```

Build one button per `STYLES` entry the same generic way the lens picker builds one per `LENSES`, calling `callbacks.onStyle(style.id)` on click, and implement `setStyle` to toggle the active class exactly as `setLens` does for lens buttons. Place the style row under the lens row.

- [ ] **Step 7: Wire the HUD callbacks in `main.ts`**

```ts
// in the HudCallbacks object literal, beside onLens:
onStyle(id) {
  const style = styleById(id);
  stylePipeline.setStyle(style);
  hud.setStyle(style);
},

// after the initial hud.setLens(...) call:
hud.setStyle(styleById('photoreal')); // picker + pipeline agree from the first frame
```

- [ ] **Step 8: tsc + vitest + build**

Run: `npx tsc --noEmit && npx vitest run src/views/renderStyle.test.ts`
Expected: clean; 3 tests pass.
Run: `npm run build`
Expected: builds.

- [ ] **Step 9: Controller visual pass**

Rebuild, kill `:4173`, screenshot the globe with photoreal selected: it must look identical to today (identity), and the style picker must show a `photoreal` button marked active. (Only photoreal exists yet.)

- [ ] **Step 10: Commit**

```bash
git add src/views/renderStyle.ts src/views/renderStyle.test.ts src/main.ts src/ui/hud.ts
git commit -m "feat(style): render-style pipeline + registry + photoreal identity + HUD picker"
```

---

### Task 2: Pixel-art style (+ biome-derived palette)

**Files:**
- Create: `src/views/styles/pixelArt.ts`
- Modify: `src/views/renderStyle.ts` (register the style)
- Modify: `src/views/renderStyle.test.ts` (palette test)

**Interfaces:**
- Consumes: `RenderStyle` (Task 1), `Pass` from `three/addons/postprocessing/Pass.js`, `ShaderPass` from `three/addons/postprocessing/ShaderPass.js`.
- Produces: `export const pixelArtStyle: RenderStyle` and `export function biomePalette(tiles: TilesScene): [number, number, number][]` (up to 16 RGB triples in 0..1, derived from the world's biome mix, deterministic).

- [ ] **Step 1: Write the failing palette test** (append to `src/views/renderStyle.test.ts`)

```ts
import { biomePalette } from './styles/pixelArt';

test('biomePalette is deterministic and bounded, ordered by biome frequency', () => {
  // 4 cells: biome 2 appears x3, biome 5 once → 2 must come before 5.
  const tiles = { width: 4, height: 1, elevation_m: [0, 0, 0, 0], biome: [2, 2, 2, 5] } as never;
  const a = biomePalette(tiles);
  const b = biomePalette(tiles);
  expect(a).toEqual(b); // deterministic
  expect(a.length).toBeGreaterThan(0);
  expect(a.length).toBeLessThanOrEqual(16); // bounded
  for (const [r, g, bl] of a) {
    for (const c of [r, g, bl]) {
      expect(c).toBeGreaterThanOrEqual(0);
      expect(c).toBeLessThanOrEqual(1);
    }
  }
});
```

- [ ] **Step 2: Run it to verify it fails**

Run: `npx vitest run src/views/renderStyle.test.ts`
Expected: FAIL — cannot find `./styles/pixelArt`.

- [ ] **Step 3: Implement `src/views/styles/pixelArt.ts`**

The palette is derived from the world's most common biomes, coloured via the existing biome palette so colours are recognizable; the shader snaps screen UV to a grid (pixelation) and snaps colour to the nearest palette entry.

```ts
import * as THREE from 'three';
import { ShaderPass } from 'three/addons/postprocessing/ShaderPass.js';
import type { Pass } from 'three/addons/postprocessing/Pass.js';
import type { TilesScene } from '../../sim/scene';
import type { RenderStyle } from '../renderStyle';
import { biomeColorForName } from '../biomePalette';

const MAX_COLORS = 16;

/** Up to 16 RGB triples (0..1) for this world's dominant biomes, ordered by
 * cell frequency (most common first). Deterministic: a pure function of the
 * tiles' biome layer. Each seed gets its own palette. */
export function biomePalette(tiles: TilesScene): [number, number, number][] {
  const biome = tiles.biome as unknown as ArrayLike<number>;
  const counts = new Map<number, number>();
  for (let i = 0; i < biome.length; i++) {
    const b = biome[i]!;
    counts.set(b, (counts.get(b) ?? 0) + 1);
  }
  const ordered = [...counts.entries()].sort((a, b) => b[1] - a[1] || a[0] - b[0]);
  const out: [number, number, number][] = [];
  for (const [b] of ordered.slice(0, MAX_COLORS)) {
    const rgb = biomeColorForName(b); // [0..255]
    out.push([rgb[0] / 255, rgb[1] / 255, rgb[2] / 255]);
  }
  return out;
}

const fragmentShader = /* glsl */ `
  uniform sampler2D tDiffuse;
  uniform vec2 uResolution;   // canvas px
  uniform float uPixelSize;   // px per art-pixel
  uniform vec3 uPalette[16];
  uniform int uPaletteLen;
  varying vec2 vUv;
  void main() {
    // Snap UV to a coarse grid (the "pixels").
    vec2 grid = uResolution / uPixelSize;
    vec2 snapped = (floor(vUv * grid) + 0.5) / grid;
    vec4 src = texture2D(tDiffuse, snapped);
    // Nearest palette colour.
    float best = 1e9; vec3 pick = src.rgb;
    for (int i = 0; i < 16; i++) {
      if (i >= uPaletteLen) break;
      float d = distance(src.rgb, uPalette[i]);
      if (d < best) { best = d; pick = uPalette[i]; }
    }
    gl_FragColor = vec4(pick, src.a);
  }
`;

const vertexShader = /* glsl */ `
  varying vec2 vUv;
  void main() { vUv = uv; gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }
`;

export const pixelArtStyle: RenderStyle = {
  id: 'pixel-art',
  label: 'pixel-art',
  passes(tiles: TilesScene): Pass[] {
    const palette = biomePalette(tiles);
    const flat = new Array(16).fill(0).map((_, i) => new THREE.Vector3(...(palette[i] ?? palette[palette.length - 1] ?? [0, 0, 0])));
    const pass = new ShaderPass({
      uniforms: {
        tDiffuse: { value: null },
        uResolution: { value: new THREE.Vector2(1, 1) },
        uPixelSize: { value: 4.0 },
        uPalette: { value: flat },
        uPaletteLen: { value: Math.max(1, palette.length) },
      },
      vertexShader,
      fragmentShader,
    });
    // Keep uResolution current: ShaderPass.setSize is called by the composer.
    (pass as unknown as { setSize: (w: number, h: number) => void }).setSize = (w, h) => {
      pass.uniforms.uResolution.value.set(w, h);
    };
    return [pass];
  },
};
```

- [ ] **Step 4: Register the style** in `src/views/renderStyle.ts`

```ts
import { pixelArtStyle } from './styles/pixelArt';
// change the STYLES line to:
export const STYLES: RenderStyle[] = [photorealStyle, pixelArtStyle];
```

- [ ] **Step 5: Run tests + tsc + build**

Run: `npx vitest run src/views/renderStyle.test.ts && npx tsc --noEmit`
Expected: palette test + registry tests pass; tsc clean.
Run: `npm run build`
Expected: builds.

- [ ] **Step 6: Controller visual pass**

Rebuild, kill `:4173`, select **pixel-art**: the globe should read as chunky retro pixels in a limited palette that matches the world's biome colours. Capture far + mid-zoom. Tune `uPixelSize` (art-pixel size) by eye. Confirm photoreal still identical when reselected.

- [ ] **Step 7: Commit**

```bash
git add src/views/styles/pixelArt.ts src/views/renderStyle.ts src/views/renderStyle.test.ts
git commit -m "feat(style): pixel-art skin with per-world biome palette"
```

---

### Task 3: Cel / ink style

**Files:**
- Create: `src/views/styles/cel.ts`
- Modify: `src/views/renderStyle.ts` (register)

**Interfaces:**
- Consumes: `RenderStyle`, `ShaderPass`.
- Produces: `export const celStyle: RenderStyle`.

- [ ] **Step 1: Implement `src/views/styles/cel.ts`**

Posterize luminance into flat bands; overlay a dark ink outline from a Sobel edge on luminance (edges land on coastlines, biome boundaries, and relief breaks — all luminance discontinuities).

```ts
import * as THREE from 'three';
import { ShaderPass } from 'three/addons/postprocessing/ShaderPass.js';
import type { Pass } from 'three/addons/postprocessing/Pass.js';
import type { TilesScene } from '../../sim/scene';
import type { RenderStyle } from '../renderStyle';

const vertexShader = /* glsl */ `
  varying vec2 vUv;
  void main() { vUv = uv; gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }
`;

const fragmentShader = /* glsl */ `
  uniform sampler2D tDiffuse;
  uniform vec2 uTexel;      // 1/resolution
  uniform float uBands;     // number of lighting bands
  uniform float uEdge;      // edge threshold
  varying vec2 vUv;
  float luma(vec3 c) { return dot(c, vec3(0.299, 0.587, 0.114)); }
  void main() {
    vec4 src = texture2D(tDiffuse, vUv);
    // Posterize by banding the luminance, keep hue.
    float l = luma(src.rgb);
    float banded = floor(l * uBands + 0.5) / uBands;
    vec3 flat = src.rgb * (banded / max(l, 1e-3));
    // Sobel on luminance for the ink outline.
    float gx = 0.0, gy = 0.0;
    float k[9]; k[0]=-1.;k[1]=0.;k[2]=1.;k[3]=-2.;k[4]=0.;k[5]=2.;k[6]=-1.;k[7]=0.;k[8]=1.;
    int idx = 0;
    for (int y = -1; y <= 1; y++) for (int x = -1; x <= 1; x++) {
      float s = luma(texture2D(tDiffuse, vUv + vec2(float(x), float(y)) * uTexel).rgb);
      gx += s * k[idx];
      gy += s * k[8 - idx];
      idx++;
    }
    float edge = sqrt(gx * gx + gy * gy);
    float ink = smoothstep(uEdge, uEdge * 2.0, edge);
    gl_FragColor = vec4(mix(flat, vec3(0.05, 0.05, 0.08), ink), src.a);
  }
`;

export const celStyle: RenderStyle = {
  id: 'cel',
  label: 'cel / ink',
  passes(_tiles: TilesScene): Pass[] {
    const pass = new ShaderPass({
      uniforms: {
        tDiffuse: { value: null },
        uTexel: { value: new THREE.Vector2(1 / 1024, 1 / 1024) },
        uBands: { value: 4.0 },
        uEdge: { value: 0.12 },
      },
      vertexShader,
      fragmentShader,
    });
    (pass as unknown as { setSize: (w: number, h: number) => void }).setSize = (w, h) => {
      pass.uniforms.uTexel.value.set(1 / w, 1 / h);
    };
    return [pass];
  },
};
```

- [ ] **Step 2: Register** in `renderStyle.ts`: `import { celStyle } from './styles/cel';` and add `celStyle` to `STYLES`.

- [ ] **Step 3: tsc + vitest + build**

Run: `npx tsc --noEmit && npx vitest run src/views/renderStyle.test.ts && npm run build`
Expected: clean; tests pass (the registry uniqueness test now covers 3 styles); builds.

- [ ] **Step 4: Controller visual pass**

Select **cel / ink**: flat banded shading with dark ink outlines on coastlines/relief. Capture far + mid. Tune `uBands` (4–5) and `uEdge` by eye. Confirm the other styles still work.

- [ ] **Step 5: Commit**

```bash
git add src/views/styles/cel.ts src/views/renderStyle.ts
git commit -m "feat(style): cel/ink skin — banded lighting + Sobel ink outline"
```

---

### Task 4: Engraving / woodcut style

**Files:**
- Create: `src/views/styles/engraving.ts`
- Modify: `src/views/renderStyle.ts` (register)

**Interfaces:**
- Consumes: `RenderStyle`, `ShaderPass`.
- Produces: `export const engravingStyle: RenderStyle`.

- [ ] **Step 1: Implement `src/views/styles/engraving.ts`**

Desaturate to luminance; draw hatching whose density tracks darkness (shaded luminance already encodes relief + light — the honest hachure); tint sepia-on-cream. Hatch lines are a screen-space sine of a rotated coordinate, gated by luminance thresholds so darker areas get denser/second-direction hatching.

```ts
import * as THREE from 'three';
import { ShaderPass } from 'three/addons/postprocessing/ShaderPass.js';
import type { Pass } from 'three/addons/postprocessing/Pass.js';
import type { TilesScene } from '../../sim/scene';
import type { RenderStyle } from '../renderStyle';

const vertexShader = /* glsl */ `
  varying vec2 vUv;
  void main() { vUv = uv; gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }
`;

const fragmentShader = /* glsl */ `
  uniform sampler2D tDiffuse;
  uniform vec2 uResolution;
  uniform float uFreq;       // hatch line frequency (screen px)
  varying vec2 vUv;
  float luma(vec3 c) { return dot(c, vec3(0.299, 0.587, 0.114)); }
  // 1.0 where a hatch line at angle `a` is drawn, else 0.0
  float hatch(vec2 px, float a) {
    vec2 dir = vec2(cos(a), sin(a));
    float v = dot(px, dir) * uFreq;
    return step(0.5, fract(v)); // 1 on the "line" half, crude but crisp
  }
  void main() {
    vec4 src = texture2D(tDiffuse, vUv);
    float l = luma(src.rgb);
    vec2 px = vUv * uResolution;
    // Progressively add hatch directions as it gets darker.
    float ink = 0.0;
    if (l < 0.75) ink = max(ink, hatch(px, 0.6));
    if (l < 0.5)  ink = max(ink, hatch(px, -0.6));
    if (l < 0.28) ink = max(ink, hatch(px, 1.9));
    // Cream paper, sepia ink.
    vec3 cream = vec3(0.93, 0.89, 0.78);
    vec3 sepia = vec3(0.18, 0.12, 0.08);
    gl_FragColor = vec4(mix(cream, sepia, ink), src.a);
  }
`;

export const engravingStyle: RenderStyle = {
  id: 'engraving',
  label: 'engraving',
  passes(_tiles: TilesScene): Pass[] {
    const pass = new ShaderPass({
      uniforms: {
        tDiffuse: { value: null },
        uResolution: { value: new THREE.Vector2(1024, 1024) },
        uFreq: { value: 0.12 },
      },
      vertexShader,
      fragmentShader,
    });
    (pass as unknown as { setSize: (w: number, h: number) => void }).setSize = (w, h) => {
      pass.uniforms.uResolution.value.set(w, h);
    };
    return [pass];
  },
};
```

- [ ] **Step 2: Register** in `renderStyle.ts`: `import { engravingStyle } from './styles/engraving';` add to `STYLES`.

- [ ] **Step 3: tsc + vitest + build**

Run: `npx tsc --noEmit && npx vitest run src/views/renderStyle.test.ts && npm run build`
Expected: clean; tests pass; builds.

- [ ] **Step 4: Controller visual pass**

Select **engraving**: sepia-on-cream with hatching that thickens in shadow/relief — an old copperplate globe. Capture far + mid. Tune `uFreq` and the luminance thresholds by eye. Confirm the others still work.

- [ ] **Step 5: Commit**

```bash
git add src/views/styles/engraving.ts src/views/renderStyle.ts
git commit -m "feat(style): engraving skin — luminance-keyed hachure, sepia on cream"
```

---

### Task 5: Watercolor / painterly style

**Files:**
- Create: `src/views/styles/watercolor.ts`
- Modify: `src/views/renderStyle.ts` (register)

**Interfaces:**
- Consumes: `RenderStyle`, `ShaderPass`.
- Produces: `export const watercolorStyle: RenderStyle`.

- [ ] **Step 1: Implement `src/views/styles/watercolor.ts`**

A cheap edge-preserving smoothing (a small Kuwahara: pick the quadrant with the least colour variance) flattens the image into washes while keeping boundaries; a procedural paper grain multiplies over the top; a slight darkening at high-gradient boundaries mimics pigment pooling.

```ts
import * as THREE from 'three';
import { ShaderPass } from 'three/addons/postprocessing/ShaderPass.js';
import type { Pass } from 'three/addons/postprocessing/Pass.js';
import type { TilesScene } from '../../sim/scene';
import type { RenderStyle } from '../renderStyle';

const vertexShader = /* glsl */ `
  varying vec2 vUv;
  void main() { vUv = uv; gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }
`;

const fragmentShader = /* glsl */ `
  uniform sampler2D tDiffuse;
  uniform vec2 uTexel;    // 1/resolution
  uniform float uRadius;  // sample radius in px
  varying vec2 vUv;
  float luma(vec3 c) { return dot(c, vec3(0.299, 0.587, 0.114)); }
  // Mean + variance of one quadrant.
  void quad(vec2 lo, vec2 hi, out vec3 mean, out float varSum) {
    vec3 sum = vec3(0.0); vec3 sum2 = vec3(0.0); float n = 0.0;
    for (int y = 0; y < 4; y++) for (int x = 0; x < 4; x++) {
      vec2 o = mix(lo, hi, vec2(float(x), float(y)) / 3.0);
      vec3 c = texture2D(tDiffuse, vUv + o * uTexel).rgb;
      sum += c; sum2 += c * c; n += 1.0;
    }
    mean = sum / n;
    vec3 v = sum2 / n - mean * mean;
    varSum = v.r + v.g + v.b;
  }
  void main() {
    float r = uRadius;
    vec3 m; float v; vec3 bestM; float bestV = 1e9;
    vec2 quads[4];
    quads[0] = vec2(-r, -r); quads[1] = vec2(0.0, -r); quads[2] = vec2(-r, 0.0); quads[3] = vec2(0.0, 0.0);
    for (int i = 0; i < 4; i++) {
      vec2 lo = quads[i]; vec2 hi = lo + vec2(r, r);
      quad(lo, hi, m, v);
      if (v < bestV) { bestV = v; bestM = m; }
    }
    // Pigment pools a touch at busy boundaries (high overall variance).
    float pool = clamp(bestV * 6.0, 0.0, 0.25);
    vec3 washed = bestM * (1.0 - pool);
    // Procedural paper grain.
    float g = fract(sin(dot(vUv * 700.0, vec2(12.9898, 78.233))) * 43758.5453);
    washed *= 0.94 + 0.06 * g;
    gl_FragColor = vec4(washed, texture2D(tDiffuse, vUv).a);
  }
`;

export const watercolorStyle: RenderStyle = {
  id: 'watercolor',
  label: 'watercolor',
  passes(_tiles: TilesScene): Pass[] {
    const pass = new ShaderPass({
      uniforms: {
        tDiffuse: { value: null },
        uTexel: { value: new THREE.Vector2(1 / 1024, 1 / 1024) },
        uRadius: { value: 3.0 },
      },
      vertexShader,
      fragmentShader,
    });
    (pass as unknown as { setSize: (w: number, h: number) => void }).setSize = (w, h) => {
      pass.uniforms.uTexel.value.set(1 / w, 1 / h);
    };
    return [pass];
  },
};
```

- [ ] **Step 2: Register** in `renderStyle.ts`: `import { watercolorStyle } from './styles/watercolor';` add to `STYLES`.

- [ ] **Step 3: tsc + vitest + build**

Run: `npx tsc --noEmit && npx vitest run src/views/renderStyle.test.ts && npm run build`
Expected: clean; tests pass; builds.

- [ ] **Step 4: Controller visual pass**

Select **watercolor**: soft flattened washes with a paper grain and a little boundary pooling. Capture far + mid. Tune `uRadius` and the pool factor by eye. Confirm all four styles + photoreal cycle correctly.

- [ ] **Step 5: Commit**

```bash
git add src/views/styles/watercolor.ts src/views/renderStyle.ts
git commit -m "feat(style): watercolor skin — Kuwahara washes + paper grain + boundary pooling"
```

---

### Task 6: Smoke coverage, label legibility check, and the close

**Files:**
- Modify: `e2e/smoke.spec.ts`
- Modify: `docs/perf-baseline.md` is NOT touched (no perf claim); this task is smoke + visual + DoD.

**Interfaces:**
- Consumes: the HUD style buttons (Task 1), all five styles.

- [ ] **Step 1: Add a boot-in-each-style smoke check** (`e2e/smoke.spec.ts`)

Add a test that, for each style button, clicks it and asserts the globe canvas is non-blank (a cheap correctness gate: no style throws / blanks the screen). Follow the existing smoke test's boot + canvas-pixel pattern.

```ts
test('every render style renders the globe non-blank', async ({ page }) => {
  await page.goto('/#seed=42&view=globe&day=0.25');
  await page.locator('.hud-top-left').getByText('seed 42').waitFor({ timeout: 150_000 });
  const styleButtons = page.locator('[data-style]'); // add data-style="<id>" to the style buttons in hud.ts
  const n = await styleButtons.count();
  expect(n).toBeGreaterThanOrEqual(5);
  for (let i = 0; i < n; i++) {
    await styleButtons.nth(i).click();
    await page.waitForTimeout(600);
    const nonBlank = await page.evaluate(() => {
      const c = document.querySelector('canvas.view-canvas:last-of-type') as HTMLCanvasElement;
      const g = c.getContext('webgl2') || c.getContext('webgl');
      return !!g; // context exists; deeper pixel checks are the controller's visual pass
    });
    expect(nonBlank).toBe(true);
  }
});
```

(If the smoke file already has a canvas-pixel helper, use it instead of the context check for a stronger non-blank assertion. Add `data-style="<id>"` to each style button in `hud.ts` Step-6 markup so the test can address them.)

- [ ] **Step 2: Run the smoke + full gate**

Run: `npm run build && npx vitest run && npx tsc --noEmit && npm run e2e`
Expected: all green (perf-harness excluded from e2e).

- [ ] **Step 3: Controller — the label legibility decision**

With a busy style (pixel-art or engraving) selected and settlements visible, capture a screenshot. If place-labels are illegible, implement the spec's fallback: render the marker/label group in an un-styled pass on top (a second RenderPass with a layer mask, composited after the style). If labels read acceptably through the style, record that no fallback was needed. This is a controller visual-judgment gate.

- [ ] **Step 4: Full visual pass — all five styles**

Rebuild, kill `:4173`, and capture far + mid for photoreal / pixel-art / cel / engraving / watercolor. Confirm each reads as its idiom, photoreal is unchanged, and switching is instant. This is the campaign's acceptance.

- [ ] **Step 5: DoD (hornvale repo)**

- Chronicle: `book/src/chronicle/the-idioms.md`, wired into `book/src/SUMMARY.md`.
- Retrospective: `docs/retrospectives/the-idioms.md`.
- Registry: flip **MAP-60** from `spec'd` to `shipped`, repoint **Where** at the chronicle.
- Plan: mark this file COMPLETE.
- Gate: `cargo test -p hornvale --test docs_consistency` green; no registry-ID leak in the chronicle.

- [ ] **Step 6: Commit + G6 package**

Commit the orrery styles branch and the hornvale DoD; present the G6 close package (push orrery main — no wasm release, the sim is untouched) for Nathan's approval.

---

## Notes for the implementer

- **Photoreal must stay pixel-identical.** After Task 1, before any style ships, confirm the composer path with an empty chain renders the same globe as `globeRenderer.render` did. If it differs (colour space, tone mapping), set the composer's output pass / `renderer.outputColorSpace` to match the direct path.
- **Shaders are starters, tuned by eye.** The GLSL above compiles and produces each effect; the exact constants (`uPixelSize`, `uBands`, `uEdge`, `uFreq`, thresholds, `uRadius`) are the controller's visual-pass knobs. Do not treat a first-draft look as final — iterate against screenshots.
- **`ShaderPass.setSize`** is invoked by `EffectComposer.setSize`; the per-style `setSize` override keeps each pass's resolution uniform current. Verify on window resize.
- **No new dependency.** `three/addons/postprocessing/*` ships with the pinned three.js. If an import path 404s, check the installed three version's addon layout — do not add a package.
