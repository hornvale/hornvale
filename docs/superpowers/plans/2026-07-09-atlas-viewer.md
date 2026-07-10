# The Atlas Viewer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An interactive atlas page in the published book — five switchable layers, hover readout, settlement markers, pan/zoom — as a Deno-toolchained TypeScript client under `clients/atlas/`, unit-tested, bundled to one committed `book/src/gallery/atlas.js`, verified by a dedicated CI job.

**Architecture:** Pure logic lives in four tested modules (`scene.ts` validation, `projection.ts` viewport math, `palette.ts` layer→RGBA pixels, `hittest.ts` readout assembly); `main.ts` is thin canvas/DOM glue. The committed bundle is a build artifact in the "models author, dice roll" pattern: built offline by `deno bundle`, drift-checked by a new CI job that never touches the Rust job.

**Tech Stack:** Deno 2.9.2 (pinned — native TS, `deno test`, esbuild-backed `deno bundle --platform browser`). Only external import: `jsr:@std/assert` (tests), pinned by a committed `deno.lock`.

**Spec:** `docs/superpowers/specs/2026-07-09-atlas-viewer-design.md` (governs; cite it on judgment calls). Schema consumed: `scene/tiles/v1` (`book/src/reference/scene-tiles-v1.md`).

## Global Constraints

- The Rust workspace is untouched except: `.github/workflows/ci.yml` (new sibling job only), `book/src/` content, and `docs/`. No Cargo.toml changes anywhere. The Rust `gate` job's steps are byte-identical before/after.
- `clients/atlas/` is outside the Cargo workspace (decision 0023, ratified in Task 1): its toolchain is Deno 2.9.2 exactly; `deno.lock` is committed; no npm, no node_modules.
- The committed bundle must be **reproducible**: same sources + Deno 2.9.2 → same `atlas.js` bytes, independent of the build directory's absolute path (Task 4 proves this; if it cannot be proven, STOP and escalate — the CI freshness check depends on it).
- Palettes (validated 2026-07-09, dataviz method): biome + elevation reuse the raster renders' exact values (given in Task 3); plate uses the reference categorical 8 in fixed slot order, indexed `plate_id % 8`; unrest uses the 8-step orange sequential ramp (monotonic lightness). Identity is never color-alone: the hover readout carries true values (plate id, biome name).
- Book prose rules for `atlas.md` and later docs: book altitude; no process vocabulary or registry IDs (decision numbers allowed).
- Full Rust gate must stay green (`cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`) — these tasks shouldn't affect it, but run it before each commit that touches `book/src` or `ci.yml` anyway. Client gate: `deno fmt --check && deno lint && deno task check && deno task test` from `clients/atlas/`.
- Never `--no-verify`.
- Workspace root: `/Users/nathan/Projects/hornvale/hornvale` (or the worktree). Deno is installed at 2.9.2 (`deno --version` to confirm; `brew install deno` if missing).

---

### Task 1: Decision 0023, scaffold, and `scene.ts`

**Files:**
- Create: `docs/decisions/0023-in-repo-clients-carry-their-own-toolchains.md`
- Modify: `docs/decisions/README.md` (index row)
- Create: `clients/atlas/deno.json`, `clients/atlas/src/scene.ts`, `clients/atlas/src/scene_test.ts`

**Interfaces:**
- Produces: decision 0023; the client scaffold (`deno task check|test|build` working); `parseScene(text: string): TilesScene` (throws `SceneFormatError` with a naming message on any violation) and the `TilesScene`/`Feature` TS interfaces. Tasks 2–4 consume them.

- [ ] **Step 1: The decision record**

`docs/decisions/0023-in-repo-clients-carry-their-own-toolchains.md`:

```markdown
# 0023. In-repo clients carry their own toolchains

**Status:** Accepted (2026-07-09) · **Decider:** Nathan

In the context of building the first Ring-3 client (the atlas viewer) with
real automated tests, facing the no-new-dependencies rule (decision 0004)
and an earlier vanilla-JS sketch of the in-book viewer, we decided that
**Ring-3 clients living in this repository sit under `clients/` — a new
top-level layer outside the Cargo workspace — carry their own dependency
stacks and toolchains (per decision 0022: clients never link the crates),
commit their built artifacts into the book, and are verified by dedicated
CI jobs**, accepting a second toolchain in the repository and built
artifacts in version control.

**Context.** Decision 0022 already places clients outside the workspace
with their own stacks; the binding posture was always *the workspace takes
no client dependencies*, not *clients take none*. The committed-artifact
pattern is decision 0009 ("models author, dice roll") applied to clients:
build offline, commit the output, drift-check its freshness. The owner
chose TypeScript with unit tests over the "no npm, no build step" sketch;
the toolchain is Deno (one pinned binary — native TS, built-in test
runner/formatter/linter, single-file bundling), which keeps the client's
dependency surface near zero.

**Consequence.** `clients/atlas/` is the first citizen; its CI job (pinned
Deno version, fmt/lint/check/test/build, bundle-freshness diff) runs
beside the Rust gate, never inside it. The Rust workspace's rules and CI
steps are untouched. Future in-repo clients follow the same shape; the
registry's viewer row is updated to match reality on merge.

**See also.** Decisions 0022, 0009, 0004; the atlas-viewer spec
(`docs/superpowers/specs/2026-07-09-atlas-viewer-design.md`); the
rendering-strategy spec (Ring 3).
```

Add to `docs/decisions/README.md`'s index table, after the 0022 row:

```markdown
| [0023](0023-in-repo-clients-carry-their-own-toolchains.md) | In-repo clients carry their own toolchains | Accepted |
```

- [ ] **Step 2: Scaffold**

`clients/atlas/deno.json`:

```json
{
  "tasks": {
    "check": "deno check src/",
    "test": "deno test --allow-read src/",
    "build": "deno bundle --platform browser --minify -o ../../book/src/gallery/atlas.js src/main.ts"
  },
  "imports": {
    "@std/assert": "jsr:@std/assert@^1"
  },
  "fmt": { "lineWidth": 100 },
  "lint": { "rules": { "tags": ["recommended"] } }
}
```

(`main.ts` doesn't exist until Task 4 — `deno task build` is expected to fail until then; `check`/`test` operate on what exists.)

- [ ] **Step 3: `scene_test.ts` (red)**

`clients/atlas/src/scene_test.ts`:

```typescript
import { assertEquals, assertThrows } from "@std/assert";
import { parseScene, SceneFormatError } from "./scene.ts";

function validScene(): Record<string, unknown> {
  const tiles = 16 * 8;
  return {
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(50.0),
    ocean: Array(tiles).fill(true),
    biome: Array(tiles).fill(21),
    biome_legend: Array.from({ length: 22 }, (_, i) => `biome-${i}`),
    plate: Array(tiles).fill(0),
    unrest: Array(tiles).fill(0.5),
    features: [{ name: "Test", kind: "flagship", latitude: 0.0, longitude: 0.0 }],
  };
}

Deno.test("a valid document parses", () => {
  const scene = parseScene(JSON.stringify(validScene()));
  assertEquals(scene.width, 16);
  assertEquals(scene.elevation_m.length, 128);
  assertEquals(scene.features[0].kind, "flagship");
});

Deno.test("wrong schema string is rejected by name", () => {
  const doc = validScene();
  doc.schema = "scene/tiles/v2";
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "schema");
});

Deno.test("mismatched layer length is rejected by layer name", () => {
  const doc = validScene();
  (doc.elevation_m as number[]).push(1.0);
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "elevation_m");
});

Deno.test("out-of-range biome index is rejected", () => {
  const doc = validScene();
  (doc.biome as number[])[3] = 22;
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "biome");
});

Deno.test("height must be width over two", () => {
  const doc = validScene();
  doc.height = 9;
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "height");
});

Deno.test("malformed feature is rejected", () => {
  const doc = validScene();
  (doc.features as unknown[]).push({ name: 7, kind: "settlement" });
  assertThrows(() => parseScene(JSON.stringify(doc)), SceneFormatError, "feature");
});

Deno.test("the committed gallery scene parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../book/src/gallery/scene-tiles-seed-42.json", import.meta.url),
  );
  const scene = parseScene(text);
  assertEquals(scene.schema, "scene/tiles/v1");
  assertEquals(scene.width, 256);
  assertEquals(scene.biome_legend.length, 22);
});

Deno.test("the crate's golden fixture parses", async () => {
  const text = await Deno.readTextFile(
    new URL("../../../windows/scene/tests/fixtures/tiles-seed-1-w16.json", import.meta.url),
  );
  assertEquals(parseScene(text).width, 16);
});
```

Run: `cd clients/atlas && deno task test`
Expected: FAIL — `./scene.ts` not found. (First run fetches `@std/assert` and writes `deno.lock` — commit it.)

- [ ] **Step 4: `scene.ts` (green)**

`clients/atlas/src/scene.ts`:

```typescript
/** A named point on the lattice (scene/tiles/v1 `features`). */
export interface Feature {
  name: string;
  kind: string;
  latitude: number;
  longitude: number;
}

/** One scene/tiles/v1 document. Mirrors the schema reference page. */
export interface TilesScene {
  schema: string;
  seed: number;
  width: number;
  height: number;
  sea_level_m: number;
  elevation_m: number[];
  ocean: boolean[];
  biome: number[];
  biome_legend: string[];
  plate: number[];
  unrest: number[];
  features: Feature[];
}

/** A scene document violated the contract; the message names how. */
export class SceneFormatError extends Error {}

const SCHEMA = "scene/tiles/v1";

function fail(message: string): never {
  throw new SceneFormatError(message);
}

function numberArray(doc: Record<string, unknown>, key: string, length: number): number[] {
  const value = doc[key];
  if (!Array.isArray(value) || value.length !== length) {
    fail(`${key} must be an array of ${length} numbers`);
  }
  for (const v of value) if (typeof v !== "number") fail(`${key} holds a non-number`);
  return value as number[];
}

/** Parse and validate a scene/tiles/v1 document; throw SceneFormatError naming any violation. */
export function parseScene(text: string): TilesScene {
  let doc: Record<string, unknown>;
  try {
    doc = JSON.parse(text) as Record<string, unknown>;
  } catch (e) {
    fail(`not JSON: ${e}`);
  }
  if (doc.schema !== SCHEMA) fail(`schema must be ${SCHEMA}, got ${String(doc.schema)}`);
  const width = doc.width;
  const height = doc.height;
  if (typeof width !== "number" || typeof height !== "number" || height * 2 !== width) {
    fail(`height must be width / 2, got ${String(width)}×${String(height)}`);
  }
  if (typeof doc.seed !== "number") fail("seed must be a number");
  if (typeof doc.sea_level_m !== "number") fail("sea_level_m must be a number");
  const tiles = width * height;
  const legend = doc.biome_legend;
  if (!Array.isArray(legend) || legend.some((n) => typeof n !== "string")) {
    fail("biome_legend must be an array of strings");
  }
  const ocean = doc.ocean;
  if (!Array.isArray(ocean) || ocean.length !== tiles || ocean.some((v) => typeof v !== "boolean")) {
    fail(`ocean must be an array of ${tiles} booleans`);
  }
  const biome = numberArray(doc, "biome", tiles);
  if (biome.some((b) => !Number.isInteger(b) || b < 0 || b >= legend.length)) {
    fail("biome holds an index outside the legend");
  }
  const features = doc.features;
  if (!Array.isArray(features)) fail("features must be an array");
  for (const f of features) {
    const feature = f as Record<string, unknown>;
    if (
      typeof feature.name !== "string" || typeof feature.kind !== "string" ||
      typeof feature.latitude !== "number" || typeof feature.longitude !== "number"
    ) {
      fail("feature must have string name/kind and numeric latitude/longitude");
    }
  }
  return {
    schema: SCHEMA,
    seed: doc.seed,
    width,
    height,
    sea_level_m: doc.sea_level_m,
    elevation_m: numberArray(doc, "elevation_m", tiles),
    ocean: ocean as boolean[],
    biome,
    biome_legend: legend as string[],
    plate: numberArray(doc, "plate", tiles),
    unrest: numberArray(doc, "unrest", tiles),
    features: features as unknown as Feature[],
  };
}
```

Run: `deno task test` — Expected: 8 pass. Then `deno fmt` (writes), `deno fmt --check`, `deno lint`, `deno task check` — all clean.

- [ ] **Step 5: Commit**

```bash
git add docs/decisions/0023-in-repo-clients-carry-their-own-toolchains.md docs/decisions/README.md clients/atlas
git commit -m "feat(atlas): decision 0023 + client scaffold + scene/tiles/v1 parser

In-repo clients carry their own toolchains (Deno 2.9.2, pinned);
clients/atlas validates the scene contract client-side, tested against
the committed gallery scene and the crate's golden fixture."
```

---

### Task 2: `projection.ts` + `hittest.ts`

**Files:**
- Create: `clients/atlas/src/projection.ts`, `clients/atlas/src/projection_test.ts`, `clients/atlas/src/hittest.ts`, `clients/atlas/src/hittest_test.ts`

**Interfaces:**
- Consumes: `TilesScene`, `Feature` from `./scene.ts`.
- Produces:
  - `interface Viewport { scale: number; tx: number; ty: number }` (scale 1–16; tx/ty in canvas px)
  - `initialViewport(): Viewport`; `zoomAt(v, cursorX, cursorY, factor, cw, ch): Viewport`; `pan(v, dx, dy, cw, ch): Viewport`; `clampViewport(v, cw, ch): Viewport`
  - `canvasToTile(v, x, y, scene, cw, ch): { px: number; py: number } | null` (null off-lattice)
  - `tileLatLon(scene, px, py): { latitude: number; longitude: number }` (pixel centers, the schema's grid convention)
  - `readout(scene, px, py): string` (one line: lat/lon, elevation m, biome name, plate, unrest, ocean/land)
  - `featureAt(scene, v, x, y, cw, ch): Feature | null` (nearest feature within 8 canvas px)
  - `featureCanvasXY(scene, v, f, cw, ch): { x: number; y: number }`

- [ ] **Step 1: Tests (red)**

`clients/atlas/src/projection_test.ts`:

```typescript
import { assert, assertEquals } from "@std/assert";
import {
  canvasToTile,
  clampViewport,
  initialViewport,
  pan,
  tileLatLon,
  zoomAt,
} from "./projection.ts";
import type { TilesScene } from "./scene.ts";

// Only lattice dims matter for projection.
const scene = { width: 256, height: 128 } as TilesScene;
const CW = 1024, CH = 512; // canvas px: 4 px per tile at scale 1

Deno.test("initial viewport shows the whole lattice", () => {
  const v = initialViewport();
  assertEquals(v.scale, 1);
  assertEquals(canvasToTile(v, 0, 0, scene, CW, CH), { px: 0, py: 0 });
  assertEquals(canvasToTile(v, CW - 1, CH - 1, scene, CW, CH), { px: 255, py: 127 });
});

Deno.test("canvas-tile round trip at zoom", () => {
  let v = initialViewport();
  v = zoomAt(v, CW / 2, CH / 2, 4, CW, CH);
  const t = canvasToTile(v, CW / 2, CH / 2, scene, CW, CH);
  assert(t !== null);
  // The tile under the cursor is unchanged by zooming at the cursor.
  assertEquals(t, canvasToTile(initialViewport(), CW / 2, CH / 2, scene, CW, CH));
});

Deno.test("zoom clamps to 1..16", () => {
  let v = initialViewport();
  v = zoomAt(v, 0, 0, 0.01, CW, CH);
  assertEquals(v.scale, 1);
  for (let i = 0; i < 20; i++) v = zoomAt(v, 0, 0, 2, CW, CH);
  assertEquals(v.scale, 16);
});

Deno.test("pan clamps so the lattice never fully leaves view", () => {
  let v = initialViewport();
  v = zoomAt(v, 0, 0, 4, CW, CH);
  v = pan(v, 1e6, 1e6, CW, CH);
  const c = clampViewport(v, CW, CH);
  assertEquals(c.tx, 0);
  assertEquals(c.ty, 0);
  v = pan(v, -1e7, -1e7, CW, CH);
  const c2 = clampViewport(v, CW, CH);
  assertEquals(c2.tx, CW - CW * c2.scale);
  assertEquals(c2.ty, CH - CH * c2.scale);
});

Deno.test("tile centers follow the schema grid convention", () => {
  const { latitude, longitude } = tileLatLon(scene, 0, 0);
  // px 0, py 0: lon = (0.5/256)*360 - 180; lat = 90 - (0.5/128)*180
  assertEquals(longitude, 0.5 / 256 * 360 - 180);
  assertEquals(latitude, 90 - 0.5 / 128 * 180);
});

Deno.test("the clamp keeps the lattice covering the whole canvas", () => {
  // A consequence of the translation clamp: no in-canvas cursor position is
  // ever off-lattice, at any zoom, after any pan.
  let v = initialViewport();
  v = zoomAt(v, CW / 2, CH / 2, 4, CW, CH);
  for (const [dx, dy] of [[1e6, 0], [-1e7, -1e7], [0, 1e6]]) {
    v = pan(v, dx, dy, CW, CH);
    for (const [x, y] of [[0, 0], [CW - 1, 0], [0, CH - 1], [CW - 1, CH - 1]]) {
      assert(canvasToTile(v, x, y, scene, CW, CH) !== null, `corner ${x},${y} off-lattice`);
    }
  }
});
```

`clients/atlas/src/hittest_test.ts`:

```typescript
import { assert, assertEquals, assertStringIncludes } from "@std/assert";
import { featureAt, readout } from "./hittest.ts";
import { initialViewport } from "./projection.ts";
import { parseScene } from "./scene.ts";

function scene() {
  const tiles = 16 * 8;
  return parseScene(JSON.stringify({
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(250.5),
    ocean: Array(tiles).fill(false),
    biome: Array(tiles).fill(1),
    biome_legend: ["ice", "tundra", ...Array.from({ length: 20 }, (_, i) => `b${i}`)],
    plate: Array(tiles).fill(3),
    unrest: Array(tiles).fill(0.25),
    features: [{ name: "Homestead", kind: "flagship", latitude: 0.0, longitude: 0.0 }],
  }));
}

Deno.test("readout carries every field", () => {
  const line = readout(scene(), 2, 3);
  assertStringIncludes(line, "250.5 m");
  assertStringIncludes(line, "tundra");
  assertStringIncludes(line, "plate 3");
  assertStringIncludes(line, "unrest 0.25");
  assertStringIncludes(line, "land");
});

Deno.test("feature hover finds the flagship near its canvas point", () => {
  const s = scene();
  const v = initialViewport();
  // lat 0, lon 0 → canvas center at scale 1.
  const f = featureAt(s, v, 512, 256, 1024, 512);
  assert(f !== null);
  assertEquals(f.name, "Homestead");
  assertEquals(featureAt(s, v, 512, 300, 1024, 512), null);
});
```

Run: `deno task test` — Expected: FAIL, modules missing.

- [ ] **Step 2: Implement**

`clients/atlas/src/projection.ts`:

```typescript
import type { TilesScene } from "./scene.ts";

/** The pan/zoom state: lattice-to-canvas scale and translation, canvas px. */
export interface Viewport {
  scale: number;
  tx: number;
  ty: number;
}

/** Scale bounds (spec §3): the whole lattice at 1, 16 px-per-base-px at most. */
export const MIN_SCALE = 1;
export const MAX_SCALE = 16;

/** The whole lattice in view. */
export function initialViewport(): Viewport {
  return { scale: 1, tx: 0, ty: 0 };
}

/** Clamp translation so the lattice never fully leaves the canvas. */
export function clampViewport(v: Viewport, cw: number, ch: number): Viewport {
  const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.scale));
  const minTx = cw - cw * scale;
  const minTy = ch - ch * scale;
  return {
    scale,
    tx: Math.min(0, Math.max(minTx, v.tx)),
    ty: Math.min(0, Math.max(minTy, v.ty)),
  };
}

/** Zoom by `factor` keeping the lattice point under the cursor fixed. */
export function zoomAt(
  v: Viewport,
  cursorX: number,
  cursorY: number,
  factor: number,
  cw: number,
  ch: number,
): Viewport {
  const scale = Math.min(MAX_SCALE, Math.max(MIN_SCALE, v.scale * factor));
  const ratio = scale / v.scale;
  return clampViewport(
    { scale, tx: cursorX - ratio * (cursorX - v.tx), ty: cursorY - ratio * (cursorY - v.ty) },
    cw,
    ch,
  );
}

/** Translate by a drag delta. */
export function pan(v: Viewport, dx: number, dy: number, cw: number, ch: number): Viewport {
  return clampViewport({ scale: v.scale, tx: v.tx + dx, ty: v.ty + dy }, cw, ch);
}

/** The tile under a canvas point, or null when off the lattice. */
export function canvasToTile(
  v: Viewport,
  x: number,
  y: number,
  scene: TilesScene,
  cw: number,
  ch: number,
): { px: number; py: number } | null {
  const fx = (x - v.tx) / v.scale / cw;
  const fy = (y - v.ty) / v.scale / ch;
  if (fx < 0 || fx >= 1 || fy < 0 || fy >= 1) return null;
  return { px: Math.floor(fx * scene.width), py: Math.floor(fy * scene.height) };
}

/** The tile center's lat/lon — the schema's grid convention. */
export function tileLatLon(
  scene: TilesScene,
  px: number,
  py: number,
): { latitude: number; longitude: number } {
  return {
    latitude: 90 - (py + 0.5) / scene.height * 180,
    longitude: (px + 0.5) / scene.width * 360 - 180,
  };
}

/** A lat/lon's canvas position under the viewport. */
export function latLonToCanvas(
  v: Viewport,
  latitude: number,
  longitude: number,
  cw: number,
  ch: number,
): { x: number; y: number } {
  const fx = (longitude + 180) / 360;
  const fy = (90 - latitude) / 180;
  return { x: v.tx + fx * cw * v.scale, y: v.ty + fy * ch * v.scale };
}
```

`clients/atlas/src/hittest.ts`:

```typescript
import type { Feature, TilesScene } from "./scene.ts";
import { latLonToCanvas, tileLatLon, type Viewport } from "./projection.ts";

/** Feature hover radius, canvas px. */
const HIT_RADIUS = 8;

/** One readout line for a tile: position, elevation, biome, plate, unrest, water. */
export function readout(scene: TilesScene, px: number, py: number): string {
  const i = py * scene.width + px;
  const { latitude, longitude } = tileLatLon(scene, px, py);
  const water = scene.ocean[i] ? "ocean" : "land";
  return `${latitude.toFixed(1)}°, ${longitude.toFixed(1)}° — ` +
    `${scene.elevation_m[i].toFixed(1)} m, ${scene.biome_legend[scene.biome[i]]}, ` +
    `plate ${scene.plate[i]}, unrest ${scene.unrest[i].toFixed(2)}, ${water}`;
}

/** The nearest feature within the hit radius of a canvas point, or null. */
export function featureAt(
  scene: TilesScene,
  v: Viewport,
  x: number,
  y: number,
  cw: number,
  ch: number,
): Feature | null {
  let best: Feature | null = null;
  let bestD = HIT_RADIUS;
  for (const f of scene.features) {
    const p = latLonToCanvas(v, f.latitude, f.longitude, cw, ch);
    const d = Math.hypot(p.x - x, p.y - y);
    if (d <= bestD) {
      bestD = d;
      best = f;
    }
  }
  return best;
}

/** A feature's canvas position (marker drawing). */
export function featureCanvasXY(
  scene: TilesScene,
  v: Viewport,
  f: Feature,
  cw: number,
  ch: number,
): { x: number; y: number } {
  void scene;
  return latLonToCanvas(v, f.latitude, f.longitude, cw, ch);
}
```

Run: `deno task test` — Expected: all pass (8 + 8). Then fmt/lint/check clean.

- [ ] **Step 3: Commit**

```bash
git add clients/atlas/src
git commit -m "feat(atlas): viewport math and hover readout, tested

Cursor-anchored zoom (1-16x) with edge clamping; tile and feature
hit-testing on the schema's grid convention."
```

---

### Task 3: `palette.ts`

**Files:**
- Create: `clients/atlas/src/palette.ts`, `clients/atlas/src/palette_test.ts`

**Interfaces:**
- Consumes: `TilesScene`.
- Produces: `type Layer = "biome" | "elevation" | "plate" | "unrest" | "ocean"`; `LAYERS: Layer[]`; `layerPixels(scene: TilesScene, layer: Layer): Uint8ClampedArray` (RGBA, `width*height*4`, row-major — `main.ts` wraps it in `ImageData`). Marker colors `SETTLEMENT_MARK`, `FLAGSHIP_MARK`.

- [ ] **Step 1: Tests (red)**

`clients/atlas/src/palette_test.ts`:

```typescript
import { assert, assertEquals } from "@std/assert";
import { LAYERS, layerPixels } from "./palette.ts";
import { parseScene } from "./scene.ts";

function scene(overrides: Record<string, unknown> = {}) {
  const tiles = 16 * 8;
  return parseScene(JSON.stringify({
    schema: "scene/tiles/v1",
    seed: 1,
    width: 16,
    height: 8,
    sea_level_m: 100.0,
    elevation_m: Array(tiles).fill(150.0),
    ocean: Array(tiles).fill(false),
    biome: Array.from({ length: tiles }, (_, i) => i % 22),
    biome_legend: Array.from({ length: 22 }, (_, i) => `b${i}`),
    plate: Array.from({ length: tiles }, (_, i) => i % 13),
    unrest: Array.from({ length: tiles }, (_, i) => (i % 9) / 8),
    features: [],
    ...overrides,
  }));
}

Deno.test("every layer paints every tile opaquely", () => {
  const s = scene();
  for (const layer of LAYERS) {
    const px = layerPixels(s, layer);
    assertEquals(px.length, 16 * 8 * 4);
    for (let i = 3; i < px.length; i += 4) assertEquals(px[i], 255);
  }
});

Deno.test("biome tile 0 wears the ice color", () => {
  const px = layerPixels(scene(), "biome");
  assertEquals([px[0], px[1], px[2]], [235, 235, 245]);
});

Deno.test("deep ocean is darker than shallow", () => {
  const tiles = 16 * 8;
  const elev = Array(tiles).fill(90.0); // 10 m deep
  elev[1] = -5900.0; // 6000 m deep
  const px = layerPixels(scene({ elevation_m: elev, ocean: Array(tiles).fill(true) }), "elevation");
  assert(px[4] < px[0] && px[6] < px[2], "deeper tile is darker");
});

Deno.test("plate colors follow slot order mod 8", () => {
  const px = layerPixels(scene(), "plate");
  // plate 0 → slot 1 (#2a78d6), plate 8 → slot 1 again
  assertEquals([px[0], px[1], px[2]], [0x2a, 0x78, 0xd6]);
  assertEquals([px[8 * 4], px[8 * 4 + 1], px[8 * 4 + 2]], [0x2a, 0x78, 0xd6]);
});

Deno.test("unrest buckets are monotonic in lightness", () => {
  const tiles = 16 * 8;
  const unrest = Array(tiles).fill(0);
  unrest[0] = 0.0;
  unrest[1] = 1.0;
  const px = layerPixels(scene({ unrest }), "unrest");
  const light = px[0] + px[1] + px[2];
  const dark = px[4] + px[5] + px[6];
  assert(dark < light, "unrest 1.0 must be darker than 0.0");
});

Deno.test("ocean layer is two-valued", () => {
  const tiles = 16 * 8;
  const ocean = Array(tiles).fill(false);
  ocean[0] = true;
  const px = layerPixels(scene({ ocean }), "ocean");
  assert(px[0] !== px[4] || px[1] !== px[5] || px[2] !== px[6]);
});
```

Run: `deno task test` — Expected: FAIL, module missing.

- [ ] **Step 2: Implement**

`clients/atlas/src/palette.ts` — the palettes are this client's own (decision 0022; the schema carries no colors). Biome and elevation reproduce the raster renders' values byte-for-byte; plate and unrest were validated with the data-viz method on 2026-07-09 (categorical set: worst adjacent CVD ΔE 24.2; orange ramp: monotonic lightness 0.93→0.35):

```typescript
import type { TilesScene } from "./scene.ts";

/** The five switchable layers. */
export type Layer = "biome" | "elevation" | "plate" | "unrest" | "ocean";
/** Display order for the layer switcher. */
export const LAYERS: Layer[] = ["biome", "elevation", "plate", "unrest", "ocean"];

/** The 22 biome colors, legend order — the biome raster's exact values. */
const BIOME_RGB: [number, number, number][] = [
  [235, 235, 245], // ice
  [170, 175, 155], // tundra
  [70, 105, 80], // taiga
  [160, 180, 100], // temperate-grassland
  [155, 150, 95], // shrubland
  [60, 130, 70], // temperate-forest
  [35, 100, 60], // temperate-rainforest
  [210, 195, 130], // desert
  [180, 165, 85], // savanna
  [90, 150, 65], // tropical-seasonal-forest
  [25, 110, 45], // tropical-rainforest
  [150, 140, 135], // alpine
  [220, 230, 240], // sea-ice
  [230, 150, 160], // coral-reef
  [40, 90, 95], // kelp-forest
  [120, 60, 90], // hydrothermal-vent
  [10, 15, 45], // hadal-trench
  [60, 160, 170], // upwelling
  [70, 140, 200], // epipelagic
  [45, 95, 160], // mesopelagic
  [25, 55, 110], // bathypelagic
  [12, 30, 70], // abyssal
];

/** The reference categorical eight, fixed slot order (validated set). */
const PLATE_RGB: [number, number, number][] = [
  [0x2a, 0x78, 0xd6],
  [0x1b, 0xaf, 0x7a],
  [0xed, 0xa1, 0x00],
  [0x00, 0x83, 0x00],
  [0x4a, 0x3a, 0xa7],
  [0xe3, 0x49, 0x48],
  [0xe8, 0x7b, 0xa4],
  [0xeb, 0x68, 0x34],
];

/** Orange sequential ramp, light (calm) to dark (violent), monotonic lightness. */
const UNREST_RGB: [number, number, number][] = [
  [0xfd, 0xe3, 0xd3],
  [0xf8, 0xc4, 0xa5],
  [0xf2, 0xa2, 0x76],
  [0xeb, 0x68, 0x34],
  [0xd9, 0x59, 0x26],
  [0xb3, 0x43, 0x17],
  [0x8c, 0x30, 0x0d],
  [0x66, 0x21, 0x04],
];

/** Ocean-layer water (the elevation ramp's mid-ocean blue). */
const WATER: [number, number, number] = [45, 95, 160];
/** Ocean-layer land (warm parchment). */
const LAND: [number, number, number] = [225, 215, 190];

/** Settlement marker: near-black dot (a white ring is drawn around it). */
export const SETTLEMENT_MARK = "#1a1a19";
/** Flagship marker: gold. */
export const FLAGSHIP_MARK = "#eda100";

function lerp(a: [number, number, number], b: [number, number, number], t: number) {
  const c = Math.min(1, Math.max(0, t));
  return [
    Math.round(a[0] + (b[0] - a[0]) * c),
    Math.round(a[1] + (b[1] - a[1]) * c),
    Math.round(a[2] + (b[2] - a[2]) * c),
  ] as [number, number, number];
}

/** The elevation raster's exact ramp: ocean blues by depth, land green→tan→brown→white. */
function elevationColor(elevation: number, seaLevel: number): [number, number, number] {
  if (elevation < seaLevel) {
    return lerp([70, 130, 200], [10, 30, 80], (seaLevel - elevation) / 6000);
  }
  const height = elevation - seaLevel;
  if (height < 800) return lerp([60, 140, 70], [150, 160, 90], height / 800);
  if (height < 2500) return lerp([150, 160, 90], [140, 100, 70], (height - 800) / 1700);
  return lerp([140, 100, 70], [245, 245, 245], (height - 2500) / 2500);
}

function tileColor(scene: TilesScene, layer: Layer, i: number): [number, number, number] {
  switch (layer) {
    case "biome":
      return BIOME_RGB[scene.biome[i]];
    case "elevation":
      return elevationColor(scene.elevation_m[i], scene.sea_level_m);
    case "plate":
      return PLATE_RGB[scene.plate[i] % PLATE_RGB.length];
    case "unrest":
      return UNREST_RGB[Math.min(Math.floor(scene.unrest[i] * 8), 7)];
    case "ocean":
      return scene.ocean[i] ? WATER : LAND;
  }
}

/** One layer as RGBA pixels (width × height × 4, row-major, fully opaque). */
export function layerPixels(scene: TilesScene, layer: Layer): Uint8ClampedArray {
  const out = new Uint8ClampedArray(scene.width * scene.height * 4);
  for (let i = 0; i < scene.width * scene.height; i++) {
    const [r, g, b] = tileColor(scene, layer, i);
    out[i * 4] = r;
    out[i * 4 + 1] = g;
    out[i * 4 + 2] = b;
    out[i * 4 + 3] = 255;
  }
  return out;
}
```

Run: `deno task test` — Expected: all pass. fmt/lint/check clean.

- [ ] **Step 3: Commit**

```bash
git add clients/atlas/src
git commit -m "feat(atlas): layer palettes — raster-continuous biome/elevation, validated plate/unrest

The client's own colors (decision 0022): biome and elevation reproduce
the committed rasters; plate wears the validated categorical eight;
unrest an orange ramp, light-calm to dark-violent."
```

---

### Task 4: `main.ts`, the atlas page, the committed bundle

**Files:**
- Create: `clients/atlas/src/main.ts`, `book/src/gallery/atlas.md`
- Create (built): `book/src/gallery/atlas.js`
- Modify: `book/src/SUMMARY.md` (gallery entry), `book/src/reference/scene-tiles-v1.md` (one cross-link sentence)

**Interfaces:**
- Consumes: everything from Tasks 1–3.
- Produces: the shipped page + bundle.

- [ ] **Step 1: `main.ts` (thin glue — no unit tests; browser-verified)**

`clients/atlas/src/main.ts`:

```typescript
/// <reference lib="dom" />
// The atlas viewer's canvas/DOM glue. All logic lives in the tested
// modules; this file only wires events to redraws.

import { parseScene, type TilesScene } from "./scene.ts";
import {
  canvasToTile,
  initialViewport,
  pan,
  type Viewport,
  zoomAt,
} from "./projection.ts";
import { featureAt, featureCanvasXY, readout } from "./hittest.ts";
import { FLAGSHIP_MARK, type Layer, LAYERS, layerPixels, SETTLEMENT_MARK } from "./palette.ts";

const SCENE_URL = "./scene-tiles-seed-42.json";

function fallback(message: string) {
  const holder = document.getElementById("atlas-holder");
  if (holder) {
    holder.innerHTML = `<p><em>${message}</em> The data this page renders is ` +
      `<a href="./scene-tiles-seed-42.json">the committed scene document</a>; static renders ` +
      `are in <a href="./elevation-seed-42.html">the elevation</a> and ` +
      `<a href="./biome-seed-42.html">biome</a> pages.</p>`;
  }
}

async function boot() {
  const canvas = document.getElementById("atlas-canvas") as HTMLCanvasElement | null;
  const readoutEl = document.getElementById("atlas-readout");
  const controls = document.getElementById("atlas-layers");
  if (!canvas || !readoutEl || !controls) return;
  let text: string;
  try {
    const response = await fetch(SCENE_URL);
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    text = await response.text();
  } catch (e) {
    fallback(`The atlas needs to be served over HTTP to load its data (${e}).`);
    return;
  }
  let scene: TilesScene;
  try {
    scene = parseScene(text);
  } catch (e) {
    fallback(`The scene document failed validation: ${e}.`);
    return;
  }

  const cw = canvas.width;
  const ch = canvas.height;
  const ctx = canvas.getContext("2d")!;
  ctx.imageSmoothingEnabled = false;

  // Pre-render each layer once at lattice resolution.
  const layerImages = new Map<Layer, ImageBitmap>();
  for (const layer of LAYERS) {
    const data = new ImageData(layerPixels(scene, layer), scene.width, scene.height);
    layerImages.set(layer, await createImageBitmap(data));
  }

  let activeLayer: Layer = "biome";
  let viewport: Viewport = initialViewport();
  let hoverLine = "hover the map to inspect a tile";
  let dragging = false;
  let dragX = 0;
  let dragY = 0;

  function draw() {
    ctx.clearRect(0, 0, cw, ch);
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(
      layerImages.get(activeLayer)!,
      viewport.tx,
      viewport.ty,
      cw * viewport.scale,
      ch * viewport.scale,
    );
    for (const f of scene.features) {
      const { x, y } = featureCanvasXY(scene, viewport, f, cw, ch);
      if (x < -8 || y < -8 || x > cw + 8 || y > ch + 8) continue;
      ctx.beginPath();
      ctx.arc(x, y, f.kind === "flagship" ? 5 : 3.5, 0, Math.PI * 2);
      ctx.fillStyle = f.kind === "flagship" ? FLAGSHIP_MARK : SETTLEMENT_MARK;
      ctx.fill();
      ctx.lineWidth = 2;
      ctx.strokeStyle = "#ffffff";
      ctx.stroke();
    }
    readoutEl!.textContent = hoverLine;
  }

  let raf = 0;
  function scheduleDraw() {
    if (raf) return;
    raf = requestAnimationFrame(() => {
      raf = 0;
      draw();
    });
  }

  // Layer switcher.
  for (const layer of LAYERS) {
    const label = document.createElement("label");
    const input = document.createElement("input");
    input.type = "radio";
    input.name = "atlas-layer";
    input.value = layer;
    input.checked = layer === activeLayer;
    input.addEventListener("change", () => {
      activeLayer = layer;
      scheduleDraw();
    });
    label.append(input, ` ${layer} `);
    controls.append(label);
  }

  // Hover.
  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    const x = (e.clientX - rect.left) * (cw / rect.width);
    const y = (e.clientY - rect.top) * (ch / rect.height);
    if (dragging) {
      viewport = pan(viewport, x - dragX, y - dragY, cw, ch);
      dragX = x;
      dragY = y;
      scheduleDraw();
      return;
    }
    const feature = featureAt(scene, viewport, x, y, cw, ch);
    if (feature) {
      hoverLine = `${feature.name} (${feature.kind})`;
    } else {
      const tile = canvasToTile(viewport, x, y, scene, cw, ch);
      hoverLine = tile ? readout(scene, tile.px, tile.py) : "—";
    }
    scheduleDraw();
  });

  // Pan.
  canvas.addEventListener("mousedown", (e) => {
    dragging = true;
    const rect = canvas.getBoundingClientRect();
    dragX = (e.clientX - rect.left) * (cw / rect.width);
    dragY = (e.clientY - rect.top) * (ch / rect.height);
  });
  globalThis.addEventListener("mouseup", () => {
    dragging = false;
  });

  // Zoom.
  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const rect = canvas.getBoundingClientRect();
    const x = (e.clientX - rect.left) * (cw / rect.width);
    const y = (e.clientY - rect.top) * (ch / rect.height);
    viewport = zoomAt(viewport, x, y, e.deltaY < 0 ? 1.25 : 0.8, cw, ch);
    scheduleDraw();
  }, { passive: false });

  draw();
}

boot();
```

- [ ] **Step 2: The page**

`book/src/gallery/atlas.md`:

```markdown
# The Atlas of Seed 42

Every pixel below is read, in your browser, from the same committed scene
document the simulation emitted — [`scene-tiles-seed-42.json`](./scene-tiles-seed-42.json),
a `scene/tiles/v1` description of seed 42's surface (see
[the schema reference](../reference/scene-tiles-v1.md)). The simulation
publishes what each tile *is*; everything you see — the colors, the
markers, the zoom — is this page's own choice of presentation
(decision 0022: the simulation emits data; clients render).

Switch layers, hover to inspect any tile, drag to pan, scroll to zoom.
Settlements are ringed dots; the flagship village is gold.

<div id="atlas-holder">
  <div id="atlas-layers" style="margin-bottom: 0.5em;"></div>
  <canvas id="atlas-canvas" width="1024" height="512"
    style="width: 100%; height: auto; border: 1px solid var(--theme-popup-border, #888); cursor: crosshair;"></canvas>
  <p id="atlas-readout" style="font-family: monospace; min-height: 1.5em;">loading…</p>
</div>

<script type="module" src="./atlas.js"></script>
```

`book/src/SUMMARY.md`: add after the scene JSON has no entry — place after "The Meeting of Seed 42" line (end of gallery section):

```markdown
- [The Atlas of Seed 42](./gallery/atlas.md)
```

`book/src/reference/scene-tiles-v1.md`: one sentence, where the committed example is mentioned: "The [atlas page](../gallery/atlas.md) renders this same document interactively — the schema's first live consumer."

- [ ] **Step 3: Build, prove reproducibility, commit the bundle**

```bash
cd clients/atlas
deno task check && deno task test && deno fmt --check && deno lint
deno task build
ls -la ../../book/src/gallery/atlas.js
# Reproducibility: rebuild → identical; rebuild from a copied path → identical.
cp ../../book/src/gallery/atlas.js /tmp/atlas-1.js
deno task build && cmp /tmp/atlas-1.js ../../book/src/gallery/atlas.js && echo REBUILD-IDENTICAL
ROOT="$(git rev-parse --show-toplevel)"
cd /tmp && rm -rf atlas-copy && cp -R "$ROOT/clients/atlas" atlas-copy && cd atlas-copy
deno bundle --platform browser --minify -o /tmp/atlas-2.js src/main.ts
cmp /tmp/atlas-1.js /tmp/atlas-2.js && echo PATH-INDEPENDENT
```

Expected: both `REBUILD-IDENTICAL` and `PATH-INDEPENDENT`. If `PATH-INDEPENDENT` fails (the bundle embeds source paths), inspect `grep -c "clients/atlas" atlas.js` — if paths appear, STOP and report BLOCKED with the evidence (the CI freshness design needs rethinking).

- [ ] **Step 4: Browser verification (best effort) + mdbook**

```bash
mdbook build book
grep -q "atlas.js" book/book/gallery/atlas.html && echo PAGE-WIRED
# Best-effort screenshot if Chrome exists:
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
if [ -x "$CHROME" ]; then
  (cd book/book && python3 -m http.server 8631 &) ; sleep 1
  "$CHROME" --headless --disable-gpu --screenshot=/tmp/atlas-shot.png --window-size=1200,900 \
    --virtual-time-budget=4000 http://localhost:8631/gallery/atlas.html
  kill %1 2>/dev/null
  echo "screenshot at /tmp/atlas-shot.png — attach location in the report"
else
  echo "no Chrome; note in report that visual verification falls to the controller/human"
fi
```

If the screenshot exists, note its path in the report (the controller renders it). Kill any stray `http.server` before finishing.

- [ ] **Step 5: Gates and commit**

Run the client gate (Step 3's first line) and the full Rust gate (`cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings` — book content doesn't affect it, but the commit touches `book/src`).

```bash
git add clients/atlas book/src/gallery/atlas.md book/src/gallery/atlas.js book/src/SUMMARY.md book/src/reference/scene-tiles-v1.md
git commit -m "feat(atlas): the atlas page — five layers, hover, markers, pan/zoom

Thin canvas glue over the tested modules; bundle committed and proven
path-independent; the book gains its first interactive page."
```

---

### Task 5: CI job, chronicle, retrospective, registry flip

**Files:**
- Modify: `.github/workflows/ci.yml` (new sibling job), `docs/vision/idea-registry.md` (RENDER-2), `book/src/SUMMARY.md` (chronicle line)
- Create: `book/src/chronicle/<NN>-the-atlas.md`, `docs/retrospectives/campaign-<NN>.md`

**Interfaces:**
- Consumes: everything above.
- Produces: the campaign's close.

- [ ] **Step 1: The CI job**

In `.github/workflows/ci.yml`, add a sibling job after the `gate` job (mirror the checkout action version the `gate` job uses):

```yaml
  atlas:
    name: Atlas client
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: clients/atlas
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.9.2"
      - name: Format
        run: deno fmt --check
      - name: Lint
        run: deno lint
      - name: Typecheck
        run: deno task check
      - name: Test
        run: deno task test
      - name: Bundle is current (freshness check)
        run: |
          deno task build
          git diff --exit-code ../../book/src/gallery/atlas.js
```

(Adjust `actions/checkout` to whatever version the `gate` job pins.) The `gate` job's steps must be byte-identical before/after — verify with `git diff .github/workflows/ci.yml` showing only the added job.

- [ ] **Step 2: Chronicle + SUMMARY**

**Numbering check first** (the parallel-campaign lesson): `ls book/src/chronicle/ | sort -V | tail -3` and `ls docs/retrospectives/` — take the next free number after whatever main holds at execution time (expected 22 if 21 is the scene window; verify, don't assume). Write `book/src/chronicle/<NN>-the-atlas.md` in the tone and length of the two most recent chronicles: the book's first interactive page; what it means that every pixel is client-side choice over the same committed document a reader can open raw (decision 0022 made visible); the toolchain decision (decision 0023 — a second, pinned toolchain for clients, the workspace untouched); the palettes (continuity with the committed rasters for biome/elevation; the validated categorical/sequential sets for plate/unrest); what the tested-client pattern sets up (the TUI viewer, the game lens's tilemap — as diegetic future, no registry IDs). SUMMARY chronicle line after the current last entry.

- [ ] **Step 3: Registry flip**

`docs/vision/idea-registry.md`, RENDER-2 row: status `raw → shipped`; Where → `[atlas-viewer spec](../superpowers/specs/2026-07-09-atlas-viewer-design.md)`; row text updated to reality per the spec §7, e.g.:

```
| RENDER-2 | In-book web viewer — the atlas: a TypeScript client under `clients/` (decision 0023), unit-tested, bundled to a committed `atlas.js`; five layers, hover readout, markers, pan/zoom over the committed scene JSON; sky-scrubbing awaits a situated-pole scene kind | shipped | med | [atlas-viewer spec](../superpowers/specs/2026-07-09-atlas-viewer-design.md) |
```

Run: `cargo test -p hornvale --test docs_consistency` — PASS.

- [ ] **Step 4: Retrospective**

`docs/retrospectives/campaign-<NN>.md` (same number as the chronicle), one page, the established section shape, process-not-product. Seed material: whatever the reviews actually found, plus: first non-Rust CI job — did the freshness-check design (pinned Deno, path-independence proof) hold; the dataviz-validated palette step; anything learned about testing canvas clients by keeping glue thin.

- [ ] **Step 5: Final sweep, gates, commit**

Run the full CI artifact list (Rust side — unchanged by this campaign, but prove it: `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` after the regen commands), `mdbook build book`, docs_consistency, both gates (Rust + client).

```bash
git add .github/workflows/ci.yml book/src/chronicle/ book/src/SUMMARY.md docs/vision/idea-registry.md docs/retrospectives/
git commit -m "docs(book): chronicle — the atlas; RENDER-2 shipped; atlas CI job; retro"
```

---

## Definition of Done

- [ ] Decision 0023 ratified + indexed; `clients/atlas/` exists with pinned Deno 2.9.2, committed `deno.lock`.
- [ ] `scene.ts`/`projection.ts`/`palette.ts`/`hittest.ts` fully unit-tested (validation against both committed JSON artifacts; zoom/pan clamps; palette totality/monotonicity; hit radii); `main.ts` is glue only.
- [ ] `book/src/gallery/atlas.js` committed, proven REBUILD-IDENTICAL and PATH-INDEPENDENT; `atlas.md` live with switcher/readout/markers/pan/zoom and the fetch-failure fallback.
- [ ] CI gains the `atlas` job; the Rust `gate` job byte-identical; both green.
- [ ] Chronicle + retro (number verified against main at execution time), RENDER-2 → `shipped`, SUMMARY + reference cross-link; docs_consistency green.
- [ ] Visual verification: screenshot rendered and eyeballed (or explicitly handed to the human with serving instructions).
