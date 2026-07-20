# The Frame Budget (Orrery Performance) Implementation Plan

> **Status: DRAFT.** REQUIRED SUB-SKILL: superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Make the Orrery smooth — every common interaction under the frame budget (no main-thread long-task > 50ms) — by taking the perf-lever scale in order, measured by a committed flamegraph harness.

**Architecture:** The proven hotspot is `globe.ts::buildTiles` — a full O(all-tiles) rebuild (`stitchNormals` string-keyed over every vertex) running ~30× per zoom. The fix, in implementation order (analytic normals first as the enabler): **analytic normals** (kill `stitchNormals`) → **incremental LOD diff + hysteresis + on-settle** (build only changed tiles) → **incremental region swap** → **buffer reuse** → a **measure-driven sweep** of the other interactions (day-scrub, lens-swap, boot).

**Tech Stack:** TypeScript, three.js (orrery only). No new deps. No hornvale/sim change, no wasm release.

## Global Constraints

- **The harness is the acceptance test.** Every perf task reports before/after metrics (buildTiles call-count + JS ms, frame-gap p50/p95, long-task total) from `e2e/perf-harness.spec.ts`. A task that does not move its metric in the right direction is not done.
- **Scene parsing stays strictly correct** — the `parseTiles` validators are never loosened, only made faster where safe. Parse tests stay green.
- **Every rendering change is visually verified** (the controller opens the PNGs): seams at mixed-LOD boundaries, region boundaries, the terminator. **Rebuild (`npm run build`) and kill the reused :4173 server before each capture** (the webServer serves `dist/`; a stale build blinds the loop).
- The full orrery suite (`npx vitest run`, `npx tsc --noEmit`, `npm run build`, Playwright e2e) stays green throughout.
- **No forbidden-surface edits** in impl tasks: no `package.json`/`public/`/wasm-URL. `public/hornvale_world.wasm` is gitignored (vendored for the harness/visual).
- Determinism is waived client-side (0022/0023) — no golden. The render may change subtly (approved: on-settle refinement, analytic-normal lighting).

## File Structure

- `src/views/worldMesh.ts` — analytic normals in the geometry builders; `stitchNormals` removed (T2).
- `src/views/globe.ts` — incremental LOD tile management, hysteresis, on-settle, incremental region swap, buffer reuse (T3–T5).
- `e2e/perf-harness.spec.ts` — the committed profiler (T1).
- New pure-function units get their own `*.test.ts`.

---

### Task 1: Commit the harness + capture the baseline

**Files:**
- Add: `e2e/perf-harness.spec.ts` (already carried into the worktree — clean it up: rename the temp `profile-zoom` test to `perf-harness`, drop any leftover `__bt*` counter reads that reference not-yet-added instrumentation, keep the CDP CPU profile + frame-gap/long-task stats).
- Add: `scripts/analyze-profile.mjs` (the self-time aggregator, carried in as `.superpowers-analyze-profile.mjs` — move it to `scripts/`).
- Add: `docs/perf-baseline.md` (or a committed JSON) — the BEFORE numbers for the zoom interaction, captured now.

- [ ] **Step 1: Clean the harness.** Ensure `e2e/perf-harness.spec.ts` runs standalone (boots seed 42 globe, scripts a deep zoom, captures a `.cpuprofile` + `ZOOM_STATS` with frame-gap p50/p95/max, gaps>50ms, long-task total/max). Remove reads of instrumentation globals that don't exist yet (`__btCount` etc.) OR guard them (`?? 0`).

- [ ] **Step 2: Capture the baseline.** `npm run build` (kill :4173 first); `npx playwright test perf-harness`; record the `ZOOM_STATS` into `docs/perf-baseline.md` with a one-line note ("origin/main a3ff771, zoom-in, headless"). This is the campaign's before-evidence.

- [ ] **Step 3: Commit.**
```bash
git add e2e/perf-harness.spec.ts scripts/analyze-profile.mjs docs/perf-baseline.md
git commit -m "test(perf): committed flamegraph harness + zoom baseline (The Frame Budget)"
```

---

### Task 2: Analytic normals — delete stitchNormals

**Files:**
- Modify: `src/views/worldMesh.ts` (`buildGridGeometry`: replace `computeVertexNormals()` with analytic per-vertex normals; remove `stitchNormals` + `buildFaceGeometry`'s use of it if any)
- Modify: `src/views/globe.ts` (remove the `stitchNormals(tileGeoms)` call in `buildTiles`)
- Add: `src/views/worldMesh.test.ts` cases for the analytic normal
- Remove: `stitchNormals` export (and its `keyAt`) once no caller remains

**Interfaces:**
- Produces: `buildGridGeometry` writes correct per-vertex normals **without** a post-hoc cross-tile stitch. Shared edge vertices between adjacent tiles get the **same** normal by construction (both derive it from the global elevation field at the same (lat, lon)).

**Design notes (the method):** for each surface vertex at `(lat, lon)`, compute the displaced position `P(lat,lon) = unit(lat,lon) * radiusAt(lat,lon)` and the two neighbours `P(lat+δ, lon)` and `P(lat, lon+δ)` for a small fixed `δ` in **degrees** (NOT in-tile grid steps — a fixed lat/lon δ makes two tiles agree on a shared edge). The normal is `normalize(cross(P_lonε − P, P_latε − P))`, flipped to point outward (`dot(n, unit) > 0`). `radiusAt(lat,lon)` samples elevation through the SAME path the position uses (`sampleTile` for a base tile; the region's own field for a region tile) so the normal is a pure function of `(lat, lon)` + the field. Skirt vertices keep copying their edge vertex's normal (unchanged). Pick `δ` ≈ the tile's own vertex spacing or smaller (a tunable const; the controller confirms visually that relief still reads and there are no seams).

- [ ] **Step 1: Write the failing unit test** (`worldMesh.test.ts`)
```ts
// A flat (zero-elevation) patch: every analytic normal must point radially
// outward (equal the unit position), within tolerance.
it('analytic normals on a flat patch are radial', () => {
  const geom = buildTileGeometry(FLAT_TILES, TILE_AT_LEVEL2, 2 /*radius*/, 0 /*reliefScale=flat*/, () => [128,128,128], 0 /*skirt*/);
  const pos = geom.getAttribute('position'), nrm = geom.getAttribute('normal');
  for (let i = 0; i < pos.count; i++) {
    const p = new THREE.Vector3().fromBufferAttribute(pos, i).normalize();
    const n = new THREE.Vector3().fromBufferAttribute(nrm, i);
    expect(n.dot(p)).toBeGreaterThan(0.99); // radial
  }
});
```
(Build a tiny `FLAT_TILES` fixture — a `TilesScene` whose `elevation_m` is all 0 — reuse an existing tiles-fixture helper and zero its elevation.)

- [ ] **Step 2: Run to verify it fails** (or is currently satisfied only via stitch) — `npx vitest run worldMesh 2>&1 | tail -12`.

- [ ] **Step 3: Implement analytic normals** in `buildGridGeometry` per the design notes; remove the `computeVertexNormals()` call (keep the skirt-normal copy). Remove `stitchNormals(tileGeoms)` from `globe.ts::buildTiles`. Delete `stitchNormals`/`keyAt` from `worldMesh.ts` and any test that only asserted the stitch behaviour (note it's superseded).

- [ ] **Step 4: Run tests + tsc** — `npx vitest run 2>&1 | tail -8`, `npx tsc --noEmit`. Green.

- [ ] **Step 5: Measure + VISUAL VERIFY (controller does this at review).** Re-profile: `stitchNormals` gone from the flamegraph. Visual seam check: adjacent same-level tiles + mixed-LOD boundaries show no lighting seams; relief still reads. (Controller step — the implementer notes it's pending the controller's visual pass.)

- [ ] **Step 6: Commit.**
```bash
git add src/views/worldMesh.ts src/views/globe.ts src/views/worldMesh.test.ts
git commit -m "perf(globe): analytic gradient normals — delete the O(all-vertices) stitchNormals"
```

---

### Task 3: Incremental LOD tile management + hysteresis + on-settle

**Files:**
- Modify: `src/views/globe.ts` (`buildTiles`/`reselect` → diff-based; add hysteresis + on-settle)
- Add: a pure `diffTileSets(prev, next)` helper + `src/views/globe.test.ts` (or a `lod.ts` unit)

**Interfaces:**
- Produces: `diffTileSets(prevKeys: Set<string>, next: TileId[]) -> { added: TileId[]; removed: string[]; keptCount: number }` (pure, unit-tested). `reselect` uses it to build only `added`, dispose only `removed`, keep the rest.

**Design notes:**
- Replace the "dispose all + rebuild all" `buildTiles` with per-tile state kept in a `Map<key, TileSlot>` (`{ mesh, geom, idx, colorSrc, baseColor }`). On a leaf-set change: dispose+delete `removed`, build+add `added`, leave `kept`. No global stitch (T2 made normals local). Repaint touches only added tiles (existing ones keep their colours) unless the day/lens changed.
- **LOD hysteresis:** in `selectTiles`/the split test, split at `LOD_SPLIT_FACTOR` but merge only below a larger distance (e.g. `× MERGE_HYSTERESIS`), so a camera jitter at a boundary can't thrash. (Study `selectTiles` in `cubeSphere.ts`; if the split factor lives there, thread a merge factor or apply hysteresis in `reselect` by not merging a tile that split recently.)
- **On-settle refinement:** track camera motion; while the camera moved more than a small epsilon since last frame, defer *refinement* (splitting to finer tiles) — keep the current (coarser) set — and only refine once motion settles for a frame or two. Coarsening (merging when zooming out) can happen immediately (cheap). This holds detail steady during a fling and snaps it in on settle.

- [ ] **Step 1: Write the failing unit test** for `diffTileSets` (a mixed before/after: some kept, one split into 4 → 1 removed + 4 added; one merged → 4 removed + 1 added).

- [ ] **Step 2: Run to verify it fails.**

- [ ] **Step 3: Implement** `diffTileSets` + rewire `reselect`/`buildTiles` to incremental; add hysteresis + on-settle.

- [ ] **Step 4: Run tests + tsc.** Green.

- [ ] **Step 5: Measure + VISUAL VERIFY (controller).** buildTiles now builds O(diff) tiles per LOD change (not O(all)); the harness shows the per-change tile-build count and JS ms drop sharply. Visual: zooming refines correctly, no missing/duplicate tiles, no thrash at boundaries.

- [ ] **Step 6: Commit** `perf(globe): incremental LOD diff + hysteresis + on-settle refinement`.

---

### Task 4: Incremental region integration

**Files:**
- Modify: `src/views/globe.ts` (`onRegion` → single-tile swap)

**Interfaces:**
- Consumes: T3's per-tile `Map<key, TileSlot>` + `tileIndex`/key lookup.
- Produces: `onRegion(key, region)` swaps only that tile's slot to the region geometry (build the one region tile, dispose the old, update the slot, repaint that tile) — no full rebuild, no stitch. (Prototyped in the `perf-zoom` worktree — mirror it onto T3's Map-based state.)

- [ ] **Step 1: Write the failing test** — after `onRegion`, only the target tile's geometry changed (its vertex count matches the region's), and the other tiles' geometries are the same object references (no full rebuild).

- [ ] **Step 2–4:** implement, run, green.

- [ ] **Step 5: Measure + VISUAL VERIFY (controller).** A deep zoom's region stream no longer triggers full rebuilds (harness: buildTiles calls flat as regions arrive; region swaps counted). Visual: region detail fills in tile-by-tile with no seams.

- [ ] **Step 6: Commit** `perf(globe): incremental single-tile region integration`.

---

### Task 5: Buffer / typed-array reuse

**Files:**
- Modify: `src/views/worldMesh.ts` / `globe.ts`

- [ ] Where a rebuilt or repainted tile keeps its vertex count, reuse the existing `BufferAttribute`'s array (write in place + `needsUpdate`) instead of allocating a fresh `Float32Array`/`BufferGeometry`. Measure the GC line in the profile drops. Unit tests stay green; visual unchanged. Commit `perf(globe): reuse tile geometry buffers to cut GC churn`.

---

### Task 6: The measure-driven sweep (CONTROLLER-run loop)

**Run by the controller.** With Part 1 landed, extend the harness to each interaction and descend the scale as the data dictates ("go as deep as it takes"):

- [ ] **Extend the harness** to script + measure: **day-scrub** (advance the clock over a range on a living lens: temperature), **lens-swap** (cycle the lens roster), **boot/parse** (time-to-first-globe + `parseTiles` cost). Capture per-interaction stats + `.cpuprofile`.
- [ ] **For each interaction over the frame budget**, flamegraph → fix the top offender → re-measure → repeat until under budget or clear diminishing returns. Known suspects (fix only if the data confirms):
  - **Day-scrub `repaint`/`setXYZ` per-vertex** → likely the first place the **GPU-shader rung** is warranted (data as a vertex attribute + colormap/uniform in a fragment shader, so a day-tick is a uniform change, not a per-vertex CPU recolour). A real rendering change — its own visual verification.
  - **Draw-call count** → merge/instance tile buffers if the render path (not JS) is the wall.
  - **Boot parse** → tighten `parseTiles` hot loops without loosening validation.
- [ ] Each sweep fix is its own commit with its before/after harness numbers; each rendering change gets a controller visual pass.

---

### Task 7: Assembly, DoD, and close (CONTROLLER)

- [ ] Absorb orrery main if it moved; full suite green (`vitest`/`tsc`/`build`/e2e).
- [ ] **Final before/after evidence:** run the full harness (all interactions) and record the numbers in `docs/perf-baseline.md` alongside the baseline — the campaign's committed proof.
- [ ] **Final whole-branch review** (the perf refactor + the sweep).
- [ ] **DoD:** chronicle `book/src/chronicle/the-frame-budget.md` + SUMMARY; retro `docs/retrospectives/the-frame-budget.md`; registry (a perf/PERF row if one exists, else note in the Region chapter that the LOD path is now incremental); no registry IDs in book prose; docs_consistency green. NO epoch/census/release.
- [ ] **G6 package** for Nathan (hard stop): the only external action is pushing orrery main (no release — the sim/wasm is unchanged; the orrery already pins v11). On approval: FF orrery main + push; `closing-a-campaign` + memory.

---

## Notes for the executor

- **Analytic normals (T2) first** — it removes the global stitch, which is what makes the incremental diff (T3) simple (no stitch-scoping). Then T3 is the big frequency+scope win.
- **The harness is truth.** Headless software-GL inflates `(program)`/render time; judge the JS levers by the render-INDEPENDENT metrics (buildTiles call-count + JS ms), and judge smoothness on Nathan's real hardware via the visual pass, not the headless frame-gap alone.
- The `perf-zoom` worktree holds the investigation + the region-swap prototype for reference; this is a clean branch.
