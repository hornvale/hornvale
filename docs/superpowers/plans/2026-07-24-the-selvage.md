# The Selvage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the Orrery Map rung's voxel cross-tile seam — fix the inverted `dy` sign that glues the wrong tile edges together, and fill every tile's own outer edge with a plinth so no elevation step shows background.

**Architecture:** Two changes in `src/views/`. First, the `(dx, dy) ↔ world` mapping becomes a single exported, style-aware, invertible pair; the three sites that currently encode its sign independently all route through it, and the voxel branch's sign is corrected. Second, `buildVoxelHeightfieldGeometry` gains an optional `floorY` so a cell at the grid's own boundary drops a wall to a shared floor instead of emitting nothing — the builder stays a pure function of one `RegionScene`, with no neighbour data and no rebuild on neighbour arrival.

**Tech Stack:** TypeScript, three.js `^0.166`, Vite, Vitest, Playwright. No new dependencies.

**Spec:** `docs/superpowers/specs/2026-07-24-the-selvage-design.md` (in the `hornvale` repo; this plan's code lives in the `orrery` repo).

## Global Constraints

- **Repo:** all code changes are in `hornvale/orrery`, on branch `the-selvage`, worktree `~/.config/superpowers/worktrees/orrery/the-selvage`. Nothing in the `hornvale` monorepo changes. No wasm rebuild; `world-wasm` stays v12.
- **No new dependencies.** The Orrery is deliberately dependency-free apart from `three`.
- **There is no separate linter — the typecheck IS the lint** (`npm run build` runs `tsc --noEmit` then `vite build`). Do not add a formatter or reformat untouched lines.
- **Every exported item carries a doc comment.** This codebase's comments explain *why*, at length, and reference the campaign that introduced them. Match that density; do not write terse comments into a file whose neighbours are paragraphs.
- **Presentation here is deliberately non-deterministic** (hornvale decision 0022). Do not import byte-identity concerns into a rendering change.
- **`public/hornvale_world.wasm` is gitignored** and must be present or ~15 wasm-fixture tests fail. That is an environment gap, not a regression. The controller places it in the worktree before Task 1; if `npm test` reports failures only in `src/sim/*` wasm-fixture tests, report it and stop rather than "fixing" it.
- **Run the whole test suite once and read it**, rather than re-running to grep a second line: `npm test 2>&1 | tee /tmp/selvage-test.txt`.

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `src/views/mapView.ts` | The map scene: styles, camera, ring mount/unmount, pan clamp, recenter. Owns the `(dx, dy) ↔ world` convention. | Modify — extract the invertible pair, correct the voxel sign, add the plinth constant and wiring |
| `src/views/mapView.test.ts` | Unit tests for the above | Modify — add round-trip + continuity tests, update three tests that encode the old sign |
| `src/views/worldMesh.ts` | Geometry builders shared by globe and map; `buildVoxelHeightfieldGeometry` is the map's | Modify — add the optional `floorY` |
| `src/views/worldMesh.test.ts` | Unit tests for the above | Modify — add plinth tests |

---

## Task 1: The invertible world ↔ tile mapping, with the voxel sign corrected

Two things that must land together: the `(dx, dy) → world` convention becomes a single exported, invertible, style-aware pair, **and** the voxel branch's sign is corrected. They are one task because correcting the sign without first consolidating the three sites that encode it would leave two of them wrong, and consolidating without correcting ships a known-wrong constant.

**Work in the step order given.** Steps 3–5 extract the pair while deliberately *preserving* the old (wrong) sign, and re-run the suite to prove the extraction changed no behaviour; only then does Step 7 correct it. That intermediate state is never committed — the single commit at the end carries the corrected form.

**Files:**
- Modify: `src/views/mapView.ts` (add two exported functions near `MAP_VOXEL_EXTENT`; rewrite `worldPointForOffset`, `clampPan`, `maybeRecenter`)
- Test: `src/views/mapView.test.ts` (add one new `describe` block; update three tests in `describe("camera pan/zoom (The Excursion)")`)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `export function worldPointForTileOffset(style: MapStyle, dx: number, dy: number): [number, number, number]` — after this task, `worldPointForTileOffset("voxel", 0, 1)` returns `[0, 0, MAP_VOXEL_EXTENT]`
  - `export function tileOffsetForWorldPoint(style: MapStyle, x: number, y: number, z: number): { dx: number; dy: number }`
  - test helpers `slopedRegionAt(tile, samples?)` and `meshNamed(view, addr)`, in the new `describe` block — Task 2 reuses both.

  Task 2 does not touch either exported function.

- [ ] **Step 1: Write the failing round-trip test**

Add this at the end of `src/views/mapView.test.ts`, and add `worldPointForTileOffset` and `tileOffsetForWorldPoint` to the existing import from `./mapView` at the top of the file.

```ts
describe("world <-> tile offset mapping (The Selvage)", () => {
  const OFFSETS: Array<[number, number]> = [
    [0, 0],
    [1, 0],
    [0, 1],
    [-1, 0],
    [0, -1],
    [1, 1],
    [-1, -1],
    [2, -3],
  ];

  // The forward map places meshes and re-anchors `controls.target`; the
  // inverse tells `maybeRecenter` which tile the camera is over. Before The
  // Selvage the inverse was open-coded twice with the sign inlined, so the
  // two could drift apart silently. This is the test that stops that.
  test("tileOffsetForWorldPoint inverts worldPointForTileOffset, both styles", () => {
    for (const style of ["voxel", "pixel"] as const) {
      for (const [dx, dy] of OFFSETS) {
        const [x, y, z] = worldPointForTileOffset(style, dx, dy);
        const back = tileOffsetForWorldPoint(style, x, y, z);
        expect(back.dx).toBeCloseTo(dx);
        expect(back.dy).toBeCloseTo(dy);
      }
    }
  });

  // Each style keeps its own plane: voxel's ground is X–Z (Y is height),
  // pixel's quad is X–Y (Z is depth-only). A mapping that leaked a nonzero
  // value onto the off-plane axis would contaminate `controls.target`.
  test("each style leaves its off-plane axis at zero", () => {
    for (const [dx, dy] of OFFSETS) {
      const [, voxelY] = worldPointForTileOffset("voxel", dx, dy);
      expect(voxelY).toBe(0);
      const [, , pixelZ] = worldPointForTileOffset("pixel", dx, dy);
      expect(pixelZ).toBe(0);
    }
  });
});
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/mapView.test.ts 2>&1 | tee /tmp/selvage-t1.txt
```

Expected: FAIL — the import of `worldPointForTileOffset` / `tileOffsetForWorldPoint` does not resolve.

- [ ] **Step 3: Add the exported pair to `mapView.ts`**

Insert immediately after the `MAP_VOXEL_EXTENT` declaration (currently around line 47), before `MAP_VOXEL_BAND_M`:

```ts
/** Which way `+dy` (increasing `iy`, the ring's second tile axis) runs along
 * a style's own second world axis: `+1` for `'voxel'`, `-1` for `'pixel'`.
 *
 * The two styles legitimately disagree, and the reason is worth stating
 * because getting it wrong is exactly the bug The Selvage fixed. The
 * producer builds a region's `(samples+1)²` nodes with
 * `b = param(iy, row/N, level)` (`windows/scene/src/region.rs`), so WITHIN a
 * tile, increasing `row` moves in the INCREASING-`iy` direction. Any
 * consistent layout must therefore run a tile's own `row` axis and the
 * ring's `dy` axis the same way in world space.
 *
 * `'voxel'` lays corner `(row, col)` at `cornerZ = (row/N)*extent - extent/2`
 * (`worldMesh.ts`'s `buildVoxelHeightfieldGeometry`) — increasing `row` runs
 * toward `+z`, so `+dy` must too. `'pixel'` uploads its texture with
 * `flipY = true` (`mapTexture.ts`), putting node row 0 at the TOP — so
 * increasing `row` runs toward `-y`, and `+dy` must too. Same invariant,
 * opposite signs, because the two styles orient the node grid differently.
 *
 * Do not "unify" these into one sign. That reintroduces the seam. */
function secondAxisSign(style: MapStyle): number {
  return style === "voxel" ? 1 : -1;
}

/** The world-space point for a same-face tile offset `(dx, dy)` from the
 * ring's origin tile, under `style`'s axis convention. The single statement
 * of that convention: mesh positions, the symbol overlay's group position,
 * `setStyle`'s `controls.target` re-anchor, and `clampPan`'s world bounds
 * all come from here, and `tileOffsetForWorldPoint` is its stated inverse.
 * `MAP_VOXEL_EXTENT` is the tile pitch for BOTH styles (they are the same
 * numeric size on purpose — see its doc comment). */
export function worldPointForTileOffset(
  style: MapStyle,
  dx: number,
  dy: number,
): [number, number, number] {
  const first = dx * MAP_VOXEL_EXTENT;
  const second = dy * MAP_VOXEL_EXTENT * secondAxisSign(style);
  return style === "voxel" ? [first, 0, second] : [first, second, 0];
}

/** `worldPointForTileOffset`'s inverse: which tile offset a world point sits
 * at, under `style`'s axis convention. Returns fractional offsets — a camera
 * target between two tiles is the normal case, and it is exactly what
 * `mapRing.ts`'s `recenterTarget` hysteresis is written to consume.
 *
 * Exists so `clampPan` and `maybeRecenter` stop open-coding the second-axis
 * sign. Before The Selvage they each inlined a `-`, which meant correcting
 * the forward mapping alone would have left the ring clamping and
 * recentring in the wrong direction — a behaviour no screenshot can show. */
export function tileOffsetForWorldPoint(
  style: MapStyle,
  x: number,
  y: number,
  z: number,
): { dx: number; dy: number } {
  const second = style === "voxel" ? z : y;
  return {
    dx: x / MAP_VOXEL_EXTENT,
    dy: second / (MAP_VOXEL_EXTENT * secondAxisSign(style)),
  };
}
```

**For Steps 3–6 only, type `-1` on the voxel arm** (`style === "voxel" ? -1 : -1`) so the extraction preserves today's behaviour and the existing suite proves it. Step 7 corrects it to `1`. Do not commit the intermediate form.

- [ ] **Step 4: Route the three call sites through the pair**

Replace the existing `worldPointForOffset` closure (currently around lines 375–386) with:

```ts
  /** The world-space point for a same-face tile offset `(dx, dy)` from
   * `originAddr`, under the CURRENT `activeStyle` — a thin binding of the
   * module-level `worldPointForTileOffset` to this view's active style, kept
   * as a closure so call sites inside `createMapView` read unchanged. */
  function worldPointForOffset(dx: number, dy: number): [number, number, number] {
    return worldPointForTileOffset(activeStyle, dx, dy);
  }
```

Replace `clampPan`'s body (currently around lines 489–504) with:

```ts
  function clampPan(): void {
    if (!originAddr || !centerAddr) return;
    const bounds = panBoundsInTiles(centerAddr, originAddr, MAP_RING_RADIUS);
    // Take the world bounds from the forward mapping itself rather than
    // re-deriving them: whether min/max swap on the second axis is a
    // consequence of the style's own sign, not a fact to hand-maintain here.
    const lo = worldPointForOffset(bounds.minDx, bounds.minDy);
    const hi = worldPointForOffset(bounds.maxDx, bounds.maxDy);
    const minX = Math.min(lo[0], hi[0]);
    const maxX = Math.max(lo[0], hi[0]);
    const secondIndex = activeStyle === "voxel" ? 2 : 1;
    const minSecond = Math.min(lo[secondIndex]!, hi[secondIndex]!);
    const maxSecond = Math.max(lo[secondIndex]!, hi[secondIndex]!);
    controls.target.x = Math.min(maxX, Math.max(minX, controls.target.x));
    if (activeStyle === "voxel") {
      controls.target.z = Math.min(maxSecond, Math.max(minSecond, controls.target.z));
    } else {
      controls.target.y = Math.min(maxSecond, Math.max(minSecond, controls.target.y));
    }
  }
```

Replace `maybeRecenter`'s body (currently around lines 510–517) with:

```ts
  function maybeRecenter(): void {
    if (!originAddr || !centerAddr) return;
    const { dx, dy } = tileOffsetForWorldPoint(
      activeStyle,
      controls.target.x,
      controls.target.y,
      controls.target.z,
    );
    const next = recenterTarget(originAddr, centerAddr, dx, dy, RECENTER_HYSTERESIS_FRACTION);
    if (next) recenterTo(next);
  }
```

Also update `clampPan`'s own doc comment (the line reading "converts `mapRing.ts`'s tile-unit bounds to world units and the active style's plane (X–Z for voxel, X–Y for pixel)") to end with: "…via `worldPointForTileOffset`, so it can never disagree with where the meshes actually sit."

- [ ] **Step 5: Run the tests to confirm the extraction changed nothing**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npm test 2>&1 | tee /tmp/selvage-t1.txt
npm run build 2>&1 | tail -5
```

Expected: PASS — the two new tests pass, and **every pre-existing test passes unmodified**. This is the checkpoint that proves the extraction is behaviour-preserving before Step 7 changes behaviour on purpose. If any pre-existing test changed status here, the extraction is wrong; stop and report rather than editing the test. **Do not commit yet.**

- [ ] **Step 6: Write the failing continuity tests**

Add to the `describe("world <-> tile offset mapping (The Selvage)", ...)` block created in Task 1. This needs `RegionScene`, `TileId`, `tileKey`, and `THREE`, all already imported at the top of the file.

```ts
  /** A region whose elevation rises with `row`, so the built geometry has an
   * unambiguous "which end is row N" — the fixture the row-direction
   * assertion below needs. Same shape as the sibling blocks' `fakeRegionAt`. */
  function slopedRegionAt(tile: TileId, samples = 4): RegionScene {
    const n = samples + 1;
    return {
      schema: "scene/tiles-region/v1",
      seed: 42,
      face: tile.face,
      level: tile.level,
      ix: tile.ix,
      iy: tile.iy,
      samples,
      sea_level_m: 0,
      season_period_days: 360,
      circulationBands: 3,
      biomeLegend: ["deep-ocean", "temperate-forest"],
      // row-major: node (row, col) is row*n + col. Elevation depends only on
      // row, rising by a full band per row so the banding cannot flatten it.
      elevation_m: Array.from({ length: n * n }, (_, i) => Math.floor(i / n) * 1000),
      ocean: Array.from({ length: n * n }, () => false),
      biome: Array.from({ length: n * n }, () => 1),
      plate: Array.from({ length: n * n }, () => 0),
      unrest: Array.from({ length: n * n }, () => 0),
    } as unknown as RegionScene;
  }

  function meshNamed(v: ReturnType<typeof createMapView>, addr: TileId): THREE.Mesh {
    const suffix = `${addr.face}:${addr.level}:${addr.ix}:${addr.iy}`;
    const mesh = v.scene.children.find(
      (c) => c instanceof THREE.Mesh && c.name.endsWith(suffix),
    );
    if (!mesh) throw new Error(`no mounted mesh for ${suffix}`);
    return mesh as THREE.Mesh;
  }

  /** A mesh's world-space Z span: its geometry's own bounding box plus
   * wherever the ring mounted it. */
  function worldZSpan(mesh: THREE.Mesh): { min: number; max: number } {
    mesh.geometry.computeBoundingBox();
    const bb = mesh.geometry.boundingBox!;
    return { min: bb.min.z + mesh.position.z, max: bb.max.z + mesh.position.z };
  }

  // Half one of the invariant: WITHIN a tile, increasing `row` must run
  // toward +z under 'voxel'. Asserted through the built geometry (where is
  // the tall end?) rather than by restating cornerZ's formula.
  test("voxel: within a tile, increasing row runs toward +z", () => {
    const addr: TileId = { face: 0, level: 3, ix: 4, iy: 4 };
    const v = createMapView({ requestRegion: () => {} });
    v.setRegion(slopedRegionAt(addr));
    const pos = meshNamed(v, addr).geometry.getAttribute("position");
    let tallestZ = 0;
    let tallestY = -Infinity;
    for (let i = 0; i < pos.count; i++) {
      if (pos.getY(i) > tallestY) {
        tallestY = pos.getY(i);
        tallestZ = pos.getZ(i);
      }
    }
    expect(tallestZ).toBeGreaterThan(0);
  });

  // Half two: ACROSS tiles, +dy must run the same way. Together the two
  // halves are the invariant the producer's `param(iy, row/N, level)`
  // imposes — and their disagreement was the seam. Asserted as adjacency of
  // the two meshes' world-space spans, which is what "continuous" means
  // here; a test comparing worldPointForTileOffset to a literal would pass
  // whichever sign happened to be in the file.
  test("voxel: the dy=+1 neighbour abuts the origin tile's +z edge", () => {
    const origin: TileId = { face: 0, level: 3, ix: 4, iy: 4 };
    const neighbour: TileId = { face: 0, level: 3, ix: 4, iy: 5 };
    const v = createMapView({ requestRegion: () => {} });
    v.beginRegion(origin);
    v.onRegion(tileKey(origin), slopedRegionAt(origin));
    v.onRegion(tileKey(neighbour), slopedRegionAt(neighbour));
    const originSpan = worldZSpan(meshNamed(v, origin));
    const neighbourSpan = worldZSpan(meshNamed(v, neighbour));
    expect(neighbourSpan.min).toBeCloseTo(originSpan.max);
  });
```

- [ ] **Step 7: Run the tests to verify they fail**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/mapView.test.ts 2>&1 | tee /tmp/selvage-t2.txt
```

Expected: the "within a tile" test PASSES (the builder was always right); the "dy=+1 neighbour abuts" test FAILS — `neighbourSpan.min` is about `-3` where `originSpan.max` is about `1`, because the neighbour is mounted on the wrong side.

- [ ] **Step 8: Correct the sign**

In `src/views/mapView.ts`, change `secondAxisSign` to:

```ts
function secondAxisSign(style: MapStyle): number {
  return style === "voxel" ? 1 : -1;
}
```

and delete the `// NOTE: voxel's -1 is the pre-Selvage value…` comment added in Task 1 (its doc comment above already carries the real explanation).

- [ ] **Step 9: Run the tests — expect three pre-existing failures**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/mapView.test.ts 2>&1 | tee /tmp/selvage-t2.txt
```

Expected: both new tests PASS. Three pre-existing tests in the `describe("camera pan/zoom (The Excursion)")` block now fail, because they encode the old sign in their *setup manoeuvre*. This is correct and expected — fix them in Step 5. If any test outside that block fails, stop and report.

- [ ] **Step 10: Update the three tests that encode the old sign**

All three are in `describe("camera pan/zoom (The Excursion)", ...)`.

**5a.** In `test("voxel style: pan clamp and recenter also operate on the world Z axis (second axis)")`, the recenter half sets a negative Z target. It still triggers a recenter (to `iy - 1` instead of `iy + 1`), so the assertion holds, but its comment is now wrong. Replace the comment block reading:

```ts
    // Recenter: moving solidly past the +Z-mapped tile boundary (the
    // negative-Z direction, since positionAt negates this axis) triggers a
    // recenter, mirroring "panning solidly past a tile boundary triggers a
    // recenter" above but on Z instead of X.
```

with:

```ts
    // Recenter: moving solidly past a tile boundary on Z triggers a
    // recenter, mirroring "panning solidly past a tile boundary triggers a
    // recenter" above but on Z instead of X. Since The Selvage, voxel's +dy
    // runs toward +z, so a negative-Z target recenters toward iy-1 — the
    // direction is not what this test is about, only that one happens.
```

**5b.** In `test("setStyle re-anchors controls.target to the current center's world point under the new style's axis convention")`:

Change the manoeuvre from

```ts
    v.controls.target.set(0.7 * MAP_VOXEL_EXTENT, 0, -0.7 * MAP_VOXEL_EXTENT);
```

to

```ts
    v.controls.target.set(0.7 * MAP_VOXEL_EXTENT, 0, 0.7 * MAP_VOXEL_EXTENT);
```

so it still recenters to `(dx=1, dy=1)` — under voxel's corrected sign, `+z` is now the `+dy` direction. `recentered` stays `{ face: 0, level: 3, ix: 5, iy: 5 }` and the `pixel` expectation stays `[1 * MAP_VOXEL_EXTENT, -1 * MAP_VOXEL_EXTENT, 0]`. Change only the voxel expectation, from

```ts
    expect(v.controls.target.toArray()).toEqual([1 * MAP_VOXEL_EXTENT, 0, -1 * MAP_VOXEL_EXTENT]);
```

to

```ts
    expect(v.controls.target.toArray()).toEqual([1 * MAP_VOXEL_EXTENT, 0, 1 * MAP_VOXEL_EXTENT]);
```

**5c.** In `test("setStyle also translates camera.position by the same offset as controls.target, keeping the iso/straight-down pose anchored on the new target")`:

Change the same manoeuvre from `-0.7 * MAP_VOXEL_EXTENT` to `0.7 * MAP_VOXEL_EXTENT` on the Z component. The `pixel` expectations are unchanged. Change only the voxel Z expectation, from

```ts
    expect(v.camera.position.z).toBeCloseTo(ISO_CAMERA_DISTANCE - 1 * MAP_VOXEL_EXTENT);
```

to

```ts
    expect(v.camera.position.z).toBeCloseTo(ISO_CAMERA_DISTANCE + 1 * MAP_VOXEL_EXTENT);
```

- [ ] **Step 11: Run the full suite and the typecheck**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npm test 2>&1 | tee /tmp/selvage-t2-full.txt
npm run build 2>&1 | tail -5
```

Expected: all PASS.

- [ ] **Step 12: Mutation-verify the continuity test**

Temporarily set `secondAxisSign`'s voxel arm back to `-1`, run only the new test, confirm it goes RED, then restore `1`.

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/mapView.test.ts -t "abuts the origin tile" 2>&1 | tail -20
```

Expected with `-1`: FAIL. Expected with `1` restored: PASS. A test that passes under both signs measures nothing — if that happens, stop and report rather than proceeding.

- [ ] **Step 13: Run the map e2e tests**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npm run build && npx playwright test --grep "excursion|diorama|map rung" 2>&1 | tail -15
```

Expected: PASS. These are The Excursion's pan, zoom, clamp, and recenter tests — the regression guard for Task 1's inverse-mapping rework.

- [ ] **Step 14: Commit**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
git add src/views/mapView.ts src/views/mapView.test.ts
git commit -m "fix(map): the voxel ring's +dy runs toward +z, not -z

The producer builds a region's nodes with b = param(iy, row/N, level), so
within a tile increasing row moves in the increasing-iy direction, and a
tile's last node row is bit-identical to its neighbour's first. The voxel
builder honours that (row runs toward +z); the ring's tile placement did
not. Every iy-direction seam therefore joined the wrong two edges, opening
an arbitrary elevation discontinuity where continuity should hold to
within one 250 m band — the wide black band along one screen diagonal.

The pixel style was already consistent (mapTexture uploads with
flipY = true, so its row axis runs toward -y and so does its +dy). The two
styles hold opposite signs for the same invariant, which is why this lives
in secondAxisSign with the reasoning written down.

Three pan/zoom tests encoded the old sign in their setup manoeuvre and
move with it; their pixel-style expectations are untouched."
```

---

## Task 2: The plinth

**Files:**
- Modify: `src/views/worldMesh.ts` (`buildVoxelHeightfieldGeometry`)
- Modify: `src/views/mapView.ts` (one constant + one call-site argument)
- Test: `src/views/worldMesh.test.ts` (add to the existing `describe("buildVoxelHeightfieldGeometry")`)

**Interfaces:**
- Consumes: nothing from Tasks 1–2.
- Produces: `buildVoxelHeightfieldGeometry`'s `opts` gains an optional `floorY?: number`. Omitted, behaviour is exactly as before.

- [ ] **Step 1: Write the failing plinth tests**

Add inside the existing `describe("buildVoxelHeightfieldGeometry", ...)` block in `src/views/worldMesh.test.ts`. `flatHeightfieldRegion`, `triangleCount`, and `hasWallBetweenEqualCells` already exist in that file.

```ts
  // The Selvage. Before it, a cell at the grid's own edge had no in-grid
  // neighbour and emitted no wall — so a real elevation step at a TILE
  // boundary had no geometry filling it and the viewer saw the page
  // background through the world. `floorY` turns that "no wall at the
  // boundary" rule into "wall all the way down at the boundary".
  it("without floorY, a flat region still emits no walls at all (unchanged)", () => {
    const region = flatHeightfieldRegion(4, 1000);
    const geom = buildVoxelHeightfieldGeometry(region, () => [1, 1, 1], {
      extent: 4,
      heightScale: 1,
      bandM: 100,
    });
    // 4x4 cells x 2 triangles per top face, and nothing else.
    expect(triangleCount(geom)).toBe(16 * 2);
  });

  it("with floorY, every boundary cell emits exactly one wall and no interior cell does", () => {
    const samples = 4;
    const region = flatHeightfieldRegion(samples, 1000);
    const geom = buildVoxelHeightfieldGeometry(region, () => [1, 1, 1], {
      extent: 4,
      heightScale: 1,
      bandM: 100,
      floorY: -100,
    });
    // A samples x samples grid has samples^2 top faces. Boundary EDGES (not
    // cells): each of the 4 sides contributes `samples` cell-edges, so
    // 4 * samples wall quads — corner cells contribute two each, which this
    // count already includes. Interior cells are all equal height, so they
    // emit nothing.
    const tops = samples * samples * 2;
    const walls = 4 * samples * 2;
    expect(triangleCount(geom)).toBe(tops + walls);
  });

  it("floorY at or above the lowest cell is lowered rather than silently dropping the wall", () => {
    const samples = 4;
    const region = flatHeightfieldRegion(samples, 1000);
    // The wall guard is a strict `<`, so a floor EQUAL to the cell height
    // would emit nothing (or a degenerate quad). The builder must lower it.
    const geom = buildVoxelHeightfieldGeometry(region, () => [1, 1, 1], {
      extent: 4,
      heightScale: 1,
      bandM: 100,
      floorY: 1e9,
    });
    expect(triangleCount(geom)).toBe(samples * samples * 2 + 4 * samples * 2);
    expect(hasWallBetweenEqualCells(geom)).toBe(false);
  });
```

- [ ] **Step 2: Run the tests to verify they fail**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/worldMesh.test.ts 2>&1 | tee /tmp/selvage-t3.txt
```

Expected: the first test PASSES (existing behaviour); the second and third FAIL — `floorY` is not a recognised option, so no boundary walls are emitted and the triangle count is `32`, not `64`.

- [ ] **Step 3: Implement `floorY` in the builder**

In `src/views/worldMesh.ts`, change `buildVoxelHeightfieldGeometry`'s signature from

```ts
  opts: { extent: number; heightScale: number; bandM: number },
): THREE.BufferGeometry {
  const { extent, heightScale, bandM } = opts;
```

to

```ts
  opts: { extent: number; heightScale: number; bandM: number; floorY?: number },
): THREE.BufferGeometry {
  const { extent, heightScale, bandM } = opts;
```

Then replace the `neighborHeight` helper (currently the block whose comment begins "A cell just outside `[0, N)` (the grid's own edge)…") with:

```ts
  // The Selvage. A cell just outside `[0, N)` is at the grid's own edge and
  // has no in-grid neighbour. Given `floorY`, it takes that shared floor, so
  // the cell emits a wall all the way down — the "plinth" that fills a real
  // elevation step at a TILE boundary (where the neighbouring tile is a
  // separate mesh this builder never sees) and gives the whole ring a solid
  // slab side where no neighbour exists at all. Without `floorY` it falls
  // back to the cell's own height, i.e. no wall — the pre-Selvage rule,
  // preserved so the globe-side and single-tile callers are unaffected.
  //
  // The floor is lowered to sit strictly BELOW every cell if the caller's
  // value does not already: the wall guard below is a strict `<`, so a floor
  // at or above a cell's height would silently emit nothing and reopen the
  // gap. One band is the natural margin — it reads as one more terrace under
  // the lowest, and needs no constant of its own.
  const bandY = (heightScale * bandM) / REFERENCE_RADIUS_M;
  let lowestCell = Infinity;
  for (let i = 0; i < N * N; i++) lowestCell = Math.min(lowestCell, cellHeight[i]!);
  const effectiveFloorY =
    opts.floorY === undefined ? undefined : Math.min(opts.floorY, lowestCell - bandY);
  const neighborHeight = (ownIdx: number, row: number, col: number): number => {
    if (row < 0 || row >= N || col < 0 || col >= N) {
      return effectiveFloorY ?? cellHeight[ownIdx]!;
    }
    return cellHeight[row * N + col]!;
  };
```

The vertex buffer needs no change: `makeVertexWriter(N * N * (6 + 4 * 6))` already reserves one top quad plus four wall quads per cell, which is the maximum the plinth can reach.

- [ ] **Step 4: Update the builder's doc comment**

`buildVoxelHeightfieldGeometry`'s doc comment currently ends a sentence with "…a cell at the grid's own edge has no in-grid neighbor there (no wall, no seam — a region patch has no sibling on this flat diorama to seam against)." That is no longer true — the Map rung mounts a whole ring of siblings. Replace that parenthetical with:

```
 * edge-neighbor's banded height is STRICTLY lower; a cell at the grid's own
 * edge has no in-grid neighbor there and takes `opts.floorY` if given (The
 * Selvage's plinth: a wall down to a shared floor, which fills a real
 * elevation step at a TILE boundary and gives the ring a solid slab side).
 * Omit `floorY` for the pre-Selvage rule — no wall at the grid boundary at
 * all, correct for a diorama mounted alone with no siblings.
```

- [ ] **Step 5: Run the builder tests to verify they pass**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npx vitest run src/views/worldMesh.test.ts 2>&1 | tee /tmp/selvage-t3.txt
```

Expected: all PASS, including the pre-existing `buildVoxelHeightfieldGeometry` tests (which omit `floorY` and must be unaffected).

- [ ] **Step 6: Wire it from `mapView.ts`**

Add this constant immediately after `MAP_VOXEL_HEIGHT_SCALE` (around line 65):

```ts
/** Elevation (m) the voxel diorama's plinth drops to — the underside of the
 * slab (The Selvage). Deep enough to sit below any terrain the producer
 * emits, so the boundary wall is always a full cliff face rather than a
 * partial one; `buildVoxelHeightfieldGeometry` lowers it further on the rare
 * tile that would reach it, so this value can never open a gap, only make the
 * slab thicker or thinner. A first-pass value chosen at a visual pass — the
 * knob to turn if the slab reads too chunky or too papery. */
export const MAP_VOXEL_FLOOR_M = -9000;
```

Then, in `buildVoxelMesh`, change the options object from

```ts
      { extent: MAP_VOXEL_EXTENT, heightScale: MAP_VOXEL_HEIGHT_SCALE, bandM: MAP_VOXEL_BAND_M },
```

to

```ts
      {
        extent: MAP_VOXEL_EXTENT,
        heightScale: MAP_VOXEL_HEIGHT_SCALE,
        bandM: MAP_VOXEL_BAND_M,
        // The plinth (The Selvage): the ring mounts a tile's siblings right
        // up against its edges, so a boundary cell that emitted no wall left
        // a real elevation step showing the page background through it.
        floorY: (MAP_VOXEL_HEIGHT_SCALE * MAP_VOXEL_FLOOR_M) / REFERENCE_RADIUS_M,
      },
```

Add `REFERENCE_RADIUS_M` to the existing `import { buildVoxelHeightfieldGeometry } from "./worldMesh";` line so it reads:

```ts
import { buildVoxelHeightfieldGeometry, REFERENCE_RADIUS_M } from "./worldMesh";
```

- [ ] **Step 7: Add a mapView-level test that the wiring is live**

Add to the `describe("world <-> tile offset mapping (The Selvage)", ...)` block in `src/views/mapView.test.ts`:

```ts
  // The builder's plinth is opt-in, so the map rung must actually pass
  // floorY — a builder that supports it and a caller that omits it looks
  // exactly like the bug. A flat region emits no walls without a floor and
  // exactly one per boundary cell with one, so vertex count is the tell.
  test("voxel: the map rung mounts tiles WITH a plinth", () => {
    const addr: TileId = { face: 0, level: 3, ix: 4, iy: 4 };
    const samples = 4;
    const v = createMapView({ requestRegion: () => {} });
    v.setRegion(slopedRegionAt(addr, samples));
    const count = meshNamed(v, addr).geometry.getAttribute("position").count;
    // Top faces alone would be samples^2 * 6 vertices; the plinth adds at
    // least one wall quad (6 vertices) per boundary cell.
    expect(count).toBeGreaterThan(samples * samples * 6 + 4 * samples * 6 - 1);
  });
```

- [ ] **Step 8: Run the full suite and the typecheck**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
npm test 2>&1 | tee /tmp/selvage-t3-full.txt
npm run build 2>&1 | tail -5
npm run smoke 2>&1 | tail -5
```

Expected: all PASS.

- [ ] **Step 9: Commit**

```bash
cd ~/.config/superpowers/worktrees/orrery/the-selvage
git add src/views/worldMesh.ts src/views/mapView.ts src/views/worldMesh.test.ts src/views/mapView.test.ts
git commit -m "feat(map): plinth the voxel diorama's own outer edge

buildVoxelHeightfieldGeometry only ever drew a wall between cells within
one tile's grid, never on the tile's own edge — correct when a diorama was
mounted alone, wrong once The Excursion put siblings right up against it.
A real elevation step at a tile boundary had no geometry filling it.

Give the builder an optional floorY: a boundary cell drops a wall to that
shared floor instead of emitting nothing. Under the fixed isometric camera
the only visible faces point +x or +z, so at an interior seam the fill
needed is always the farther tile's +edge wall, which the plinth emits;
the surplus below sits behind the nearer tile's own terrain and is
occluded. At the ring's outer boundary nothing is behind it, so it becomes
the slab's side and the diorama gains real thickness.

The builder stays a pure function of one RegionScene: no neighbour data,
no rebuild when an asynchronously-arriving neighbour lands."
```

---

## Controller-only: the visual pass

**Not a subagent task.** A subagent cannot see rendered frames; this is the controller's own work, and it is the verification for the one claim in the spec that rests on rasterisation rather than geometry (§4.1's "no hairline at an equal-height interior seam").

After Task 3, build and screenshot three framings at seed 42 through a throwaway Playwright spec: the 3×3 ring at minimum zoom, mid zoom, and a close-up straddling an interior seam. Confirm: no seam band; no hairline along interior boundaries; the slab reads with real thickness at the ring's outer edge; the terrain is continuous across both seam directions. Compare against the brainstorm-time evidence.

If a hairline appears, the fix is to inset the plinth quad by an epsilon toward the cell centre (see the followup register) — not a change of approach.

Delete the throwaway spec afterwards.

---

## Self-Review

**Spec coverage.**

| Spec section | Task |
|---|---|
| §3 The sign fix is three changes, not one | Task 1, Steps 3–4 (consolidation) and Step 8 (correction) |
| §4 The plinth | Task 2, Steps 1–5 |
| §4.3 The floor value | Task 2, Steps 3 and 6 — the margin is derived as one band rather than a second named constant (a refinement on the spec's `MAP_VOXEL_SLAB_MARGIN`, which would have been an arbitrary number); `MAP_VOXEL_FLOOR_M` is the spec's `MAP_VOXEL_FLOOR_Y` expressed in metres, converted at the call site like every other elevation constant in this file |
| §4.4 Plinth colour | No task — the existing `VOXEL_CLIFF_DARKEN` path already colours every wall this way, and the plinth is emitted through the same `emit` closure. Nothing to change is the correct implementation of "keep the existing convention" |
| §5 Write the invariant down | Task 1, Step 3 (`secondAxisSign`'s doc comment) and Task 2, Step 4 (the builder's) |
| §6 Testing — continuity invariant | Task 1, Steps 6 and 12 (including the mutation check) |
| §6 Testing — round trip | Task 1, Step 1 |
| §6 Testing — plinth | Task 2, Steps 1 and 7 |
| §6 Testing — visual pass | Controller-only section |
| §6 Testing — existing e2e stays green | Task 1, Step 13 |
| §7 Non-goals | No tasks, by construction |

**Placeholder scan.** No TBDs, no "add error handling", no "similar to Task N". Every code step carries the code.

**Type consistency.** `worldPointForTileOffset` and `tileOffsetForWorldPoint` are named identically in both tasks. `secondAxisSign` is private throughout. `floorY` is the option name in the builder, its tests, and the `mapView` call site. `MAP_VOXEL_FLOOR_M` is defined once and used once.

**One known cross-task dependency to watch:** Task 2, Step 7 uses the `slopedRegionAt` and `meshNamed` helpers defined in Task 1, Step 6, and adds its test to the same `describe` block. Execute in order.

**Why the two tasks are not three.** An earlier draft split Task 1 into a pure refactor and a separate sign correction. That forced a placeholder constant (`style === "voxel" ? -1 : -1`) into a committed intermediate state — a construct any reviewer would rightly flag as a defect. Keeping the refactor and the correction in one task preserves the behaviour-preserving checkpoint (Step 5) without ever committing the placeholder.
