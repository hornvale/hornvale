# Orrery performance — the frame budget (The Frame Budget) — design

**Date:** 2026-07-20 · **Status:** awaiting G3 review
**Repos:** orrery only. No hornvale change, no wasm release (the sim is untouched).
**Determinism:** the client render is waived eyecandy (decisions 0022/0023) — no
golden, no byte-identity on the visual. Scene *parsing* stays strictly correct.

## 0. Why this exists — the proven root cause

A flamegraph investigation (Playwright + Chrome CDP CPU profile + render-
independent counters) established, with evidence, why zooming the globe janks:
`buildTiles` is a **full O(all-tiles) rebuild** — it disposes every tile mesh,
rebuilds every tile's geometry, and runs `stitchNormals` over *every vertex of
every tile* through a **string-keyed `Map`** (`"${x},${y},${z}"` per vertex) —
and it runs on **~30 frames during a single zoom**, because the CDLOD leaf set
changes every frame as the camera moves (plus once per streamed region patch).
That is **~13 seconds of main-thread JS for one zoom-in**; on real hardware
that JS *is* the jank (rendering is cheap). Measured breakdown: `buildTiles`
~52% of CPU, `stitchNormals` ~35% (its `keyAt` string-building alone ~12%).

The structural shape (an ideonomy abstraction-lift): every hotspot is the CPU
**recomputing per frame** something that is either *unchanged* (should be
reused) or *belongs on the GPU*. That axis — recompute-on-CPU → reuse /
compute-on-GPU — orders the whole campaign.

## 1. The scale of levers, taken in order

The campaign walks the levers from decomposable/surface to fundamental,
each measured against the flamegraph harness (the acceptance rig):

1. **Reuse / pool** — numeric keys, reuse geometry buffers and typed arrays
   where a tile keeps its vertex count, cutting per-rebuild allocation and GC.
2. **Incremental LOD diff + hysteresis** — the core.
3. **Incremental region integration** — single-tile swap on region arrival.
4. **Analytic normals** — eliminate `stitchNormals` entirely.
5. **GPU per-frame (reserve)** — relief displacement + lens colour in shaders.
6. **Worker-offload geometry (fallback)** — build off the main thread.

## 2. Part 1 — the zoom/LOD fix (rungs 1–4)

In `src/views/globe.ts` + `src/views/worldMesh.ts`:

- **Incremental LOD tile management.** Keep per-tile state keyed by tile key
  (mesh, geometry, index arrays, colour source). On a leaf-set change, diff the
  new selection against the current: **dispose only removed tiles, build only
  added (split/merged) tiles, leave kept tiles untouched** — O(diff), not
  O(all). The full rebuild survives only as the first-mount path.
- **LOD hysteresis.** Split at a closer camera distance than merge, so a camera
  jitter at a split boundary cannot thrash split→merge→split (a frequency bug
  the diff alone does not fix).
- **On-settle refinement** (Nathan-approved): during fast continuous motion the
  globe holds the coarser level and refines to full detail when motion settles,
  rather than rebuilding every frame.
- **Analytic normals** (Nathan-approved): compute each vertex's normal from the
  relief-gradient at build time (finite differences of the displaced surface),
  so shared tile edges match **by construction** — **removing `stitchNormals`**
  and its O(all-vertices) string-keyed map. Region tiles (skirts + own normals)
  already needed no stitch; this makes it uniform.
- **Incremental region integration** (prototyped in the `perf-zoom` worktree):
  a region reply swaps only its own tile's geometry in place — no full rebuild,
  no global stitch. Reused here.
- **Buffer / typed-array reuse.** Where a rebuilt tile keeps its vertex count,
  reuse its geometry's buffers rather than allocating fresh, to cut GC.

## 3. Part 2 — the measure-driven sweep (go as deep as it takes)

With Part 1 landed, run the harness across each common interaction against the
frame budget and fix the top offender each surfaces, re-measuring until each
clears the bar or hits clear diminishing returns. The interactions:

- **Zoom** (the primary target above).
- **Day-scrub.** A *living* lens (temperature) and the ice blend recolour
  **every vertex every day-tick** via `setXYZ` — the likely day-scrub hotspot,
  invisible to the zoom fix. Its natural fix is the rung-5 shader-colour lever
  (data as a vertex attribute, colormap in a fragment shader).
- **Lens-swap.** Rebuilds all base colours; candidate for attribute/uniform
  swaps.
- **Boot / parse.** `parseTiles` over 32768 tiles owns time-to-first-globe.
- **Draw-call count.** Many separate tile meshes = many draw calls (a real-GPU
  cost the headless profile hides); candidate for merge/instancing.

**The success bar (Nathan): go as deep as it takes** — the sweep follows the
*data* down the scale toward buttery-smooth, and reaches the GPU-shader rung (5)
only for the specific path the measurements prove the CPU cannot get under
budget (day-scrub colouring the prime suspect). Worker-offload (6) is the
fallback if any *necessary* rebuild still blocks the main thread.

## 4. Correctness and verification

- **Scene parsing stays strictly correct.** The `parseTiles` validators are
  never loosened — only made faster where safe; parse tests stay green.
- **Every rendering change is visually verified.** The seam check for analytic
  normals and incremental diffs (adjacent tiles at mixed LOD, region boundaries,
  the terminator), following the **rebuild-before-capture** discipline (the
  Playwright webServer serves `dist/`; a stale build blinds the visual loop).
- **The full orrery suite stays green** throughout (`vitest`, `tsc`, `build`,
  Playwright e2e).
- **The harness is the acceptance test.** Before/after metrics per interaction
  (buildTiles call-count + JS ms, frame-gap p50/p95, long-task total) are the
  campaign's committed evidence.

## 5. Testing

- **Unit** — the new incremental-LOD diff (added/removed/kept tile sets from a
  before/after selection) and analytic-normal computation are pure functions,
  unit-tested (a mixed-LOD diff yields the right add/remove sets; an analytic
  normal on a flat patch points radially; hysteresis holds through a jitter).
- **Harness** — a committed profiler spec + analysis producing the before/after
  numbers.
- **Visual** — the seam/artifact pass per rendering change.

## 6. Non-goals (the fence)

- **No hornvale/sim change, no wasm release.** Orrery-only; the sim is untouched.
- **No feature changes** — the globe looks and behaves the same (modulo the
  approved on-settle refinement and analytic-normal lighting), only faster.
- **No speculative GPU rewrite up front** — the shader rung is pursued only
  where the sweep's data proves it necessary, not as an opening bet.

## 7. Flagged for G3

1. **Approach:** the scale of levers taken in order (reuse/pool → incremental
   LOD diff + hysteresis → incremental region → analytic normals), then a
   measure-driven sweep that descends to GPU shaders only where the data forces
   it. Confirm.
2. **Visual trade-offs (approved):** on-settle LOD refinement + analytic
   normals; both visually verified for seams.
3. **No carve-outs** — no epoch, no census, no release. Orrery-only, sim
   untouched.
4. **Depth:** "go as deep as it takes" (Nathan) — the sweep is data-bounded,
   not count-bounded.

## 8. Decisions (promoted from the campaign ledger)

- **One campaign, levers up the scale in order** (G1, ideonomy: tree-finding/
  abstraction-lift/scale, 1 pass — enriched): the fundamental axis is
  recompute-on-CPU → reuse/GPU; the decomposable CPU-side rungs ship first, the
  data decides the deep GPU rung.
- **Analytic normals over scoped-stitch** (Nathan): eliminate `stitchNormals`
  rather than bound it — removes the whole O(all-vertices) pass and the
  stitch-seam-bug class.
- **On-settle LOD refinement** (Nathan): hold coarse during fast motion, refine
  on settle.
- **Go as deep as it takes** (Nathan): data-bounded sweep, GPU rung in reserve.
- **Harness is the acceptance test** — before/after metrics per interaction are
  the committed evidence.
