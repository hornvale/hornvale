# The Massing — Design

**Campaign:** The Massing (working name) — a switchable globe render-**style**
layer + client-side deeper detail.
**Registry:** new row (id at CLOSE, per the standing lesson).
**Repos:** Orrery (client-only — **no wasm / producer change**). **Autopilot**
engaged (G3/G6 hard stops).
Second of a program: A view-switch (done, MAP-65) → **B (this)** voxel globe +
detail → C voxel map → D pixel-art map.

---

## 1. Goal

Give the globe **switchable render styles** — Smooth (today), Voxel, Terraced,
Faceted — chosen from a HUD control, and **raise the client-side detail ceiling**
so you can zoom the globe in far deeper than today's `LOD_CDLOD_MAX_LEVEL=4`.
Answers Nathan's two asks together: "a voxel style for the globe" and "zoom in a
lot more; the default LOD is too low."

Nathan is undecided between the voxel looks, so the deliverable is the **switch**,
not a single winner — decide by living with all four.

## 2. Keystone (from the ideonomy lift)

Two findings, from cross-domain re-instantiation on the materiality/cardinality
axes:

1. **The keystone of "voxel" is the exposed vertical inter-cell face** — the
   *cliff* between adjacent cells. Negation test: remove the side walls and a
   voxel globe collapses into "faceted/low-poly." This is what separates the four
   styles (see §4): only Voxel draws real cliffs.

2. **Voxel is an *honest* renderer; the two asks are one coin.** Smooth bilinear
   relief *invents* detail between data samples (a lie about resolution); a voxel
   block shows exactly one cell = one datum. So voxel at today's coarse ceiling
   would look crude — which *is* "the default LOD is too low." Hence **voxel cell
   = LOD data cell**, and deeper LOD ⇒ finer voxels. The detail thread (§5) is
   part of the same campaign, not a separate one. (Voxel honesty is also the
   instrument that later decides whether the *producer's* terrain resolution needs
   deepening — deferred, §7.)

## 3. Architecture — Style is orthogonal to Lens

The globe already separates **what** it shows from how it's colored via the
**Lens** axis (`src/views/lens.ts`: natural / topographic / temperature / moisture
/ unrest / precip / plate — each a `colorAt(tiles, i, day, ctx) → RGB`). This
campaign adds a second, orthogonal axis:

- **Lens** (existing) — *what* to show (recolors; per-vertex/per-node `colorAt`).
- **Style** (new) — *how* to draw the surface geometry + shading.

A `GlobeStyle = 'smooth' | 'voxel' | 'terraced' | 'faceted'` selects the geometry
builder + material used by `buildTileSlot`. Style is a **pure function of scene
data → geometry** (no seed draw, no determinism concern — client render, waived
per decisions 0022/0023). Every style consumes both the base tile data
(`scene/tiles/v1`) and the finer region data (`scene/tiles-region/v1`), so all
four work at every LOD depth; the lens's `colorAt` supplies color for all four
(Voxel/Terraced take it per-cell/nearest; Smooth/Faceted per-vertex). The two
axes are independent: any lens × any style is valid.

## 4. The four styles

The current path (`src/views/worldMesh.ts` `buildTileGeometry` /
`buildRegionTileGeometry`) builds a shared-vertex lattice, displaced by
`sampleElevationBilinear`, with analytic smooth normals, vertex colors, and a
`MeshStandardMaterial({ vertexColors: true })`.

- **Smooth** — **unchanged.** Today's globe (bilinear relief, smooth analytic
  normals, per-vertex lens color). The baseline the others are measured against.

- **Voxel** — each data cell → an **extruded flat-topped block**. Elevation
  quantized into discrete height bands (band count is a style parameter set by the
  visual pass — the chunkiness). Where a cell's band exceeds a lower neighbor's,
  the **vertical wall between them is real geometry** (the keystone). Flat
  per-cell color (nearest lens sample, no bilinear blend), flat shading. Cliffs
  may be shaded slightly darker than tops for read (a fixed factor, not a light
  change). This is the only unmistakable "voxel."

- **Terraced** — the **shared skin**, but the displacement is **quantized into
  bands** → a stepped "rice-terrace" contour. No side walls (the step ramps across
  one lattice cell). Cheap: a quantization in the `radiusAtLatLon` closure + flat
  shading. Reads "contour map," not "blocky."

- **Faceted** — the existing displaced mesh with **flat shading only**
  (`flatShading: true` / per-face normals) → crystalline low-poly. Cheapest: a
  material/normal flag; same geometry as Smooth.

**Voxel granularity vs. perf.** Voxel/Terraced need not consume the full
256-sample region resolution — chunkiness is the aesthetic, and fewer cells means
far less geometry. Voxel sub-samples the tile to a bounded cell count per edge
(the granularity parameter), keeping each block-with-walls tile build cheap while
still looking appropriately blocky. Smooth/Faceted use full resolution.

## 5. Detail thread — client-side deeper LOD

The region-tile machinery (from The Region) already delivers tiles up to
`MAX_REGION_SAMPLES=256` quads/edge at *any* level; the client just caps LOD.
This is **client-only** — no producer/wasm change:

- **Raise the LOD ceiling.** Lift `LOD_CDLOD_MAX_LEVEL` (currently 4) so deeper
  region tiles are selected and built as the camera nears the surface. Target a
  few more levels (e.g. 6–7); the exact ceiling is tuned in execution against perf
  and the point where region tiles stop adding real detail (below the sim's own
  resolution they interpolate — the honest floor).
- **Lower the globe camera's minimum distance** so you can actually get close
  enough to warrant the deeper LOD (the globe view's controls
  `minDistance`/near-clip currently cap how close you approach). Match the near
  clip so close geometry doesn't clip.
- **Deeper region prefetch.** The existing worker fetch + `deliverRegion` path
  extends unchanged to deeper levels; the CDLOD selector requests them.

The sim's terrain resolution is the honest ceiling. Going *past* it (genuinely
finer producer terrain, a wasm release) is **deferred to a follow-up** — and the
voxel honesty is exactly what will show whether it's needed.

## 6. The control (HUD)

Add a **Globe Style dropdown** to the HUD, mirroring the existing lens/`onLens`
idiom (`src/ui/hud.ts` already builds a `hud-view` `<select>` and a lens control):

- `HudCallbacks.onStyle(id: GlobeStyle)`; `Hud.setStyle(style)` to reflect state.
- A `<select class="hud-style">` with the four options; visible only in the Globe
  view (like the lens control). Selecting calls `globe.setStyle(id)`.
- Persist in the URL like the lens, if the lens is URL-addressable (match whatever
  `syncUrl` does for the lens; if the lens isn't in the URL, style isn't either —
  parity, not new surface).

## 7. File structure (Orrery)

- `src/views/worldMesh.ts` — add the geometry builders: `buildVoxelTileGeometry`
  / `buildVoxelRegionTileGeometry` (extruded blocks + walls, quantized bands,
  per-cell flat color), and a quantization hook for **Terraced** (a banded
  `radiusAtLatLon`). Faceted needs no new geometry (material flag).
- `src/views/globe.ts` — a `GlobeStyle` type; `GlobeView.setStyle(style)` that
  rebuilds slots via the style's builder and swaps the material (Faceted →
  `flatShading`); `buildTileSlot` branches on style for geometry + material +
  color-interpolation. Raise the LOD ceiling; lower the camera min-distance/near
  clip for the Globe view.
- `src/views/cubeSphere.ts` — raise `LOD_CDLOD_MAX_LEVEL` (and any dependent
  constant); the selector already requests deeper region tiles.
- `src/ui/hud.ts` — the Globe Style `<select>`, `onStyle`/`setStyle`.
- `src/main.ts` — wire `onStyle → globe.setStyle`; URL parity if applicable.
- Tests: `worldMesh` unit tests for the voxel geometry (a known cell grid → block
  vertices, walls only at real height steps, per-cell color); `hud` tests for the
  style control; `e2e/smoke.spec.ts` — switch style in the Globe view and confirm
  the canvas re-renders (viewport screenshot, per the Vantage e2e discipline), and
  a deeper-zoom smoke check.

## 8. Determinism / save-format

**None touched.** Pure client render styles + a client LOD-ceiling constant.
No new scene fields, no stream draws, no wasm rebuild, no epoch. The world-wasm
stays at **v12** (the Freshwater release). This is the cheapest class of change —
entirely reversible, orrery-only.

## 9. Non-goals / deferred

- **Producer-side finer terrain** (genuinely more detail past the sim's
  resolution; a wasm release) — a follow-up, decided once voxel honesty shows the
  floor.
- **Voxel MAP view** — campaign C (this is the globe; the map's voxel/2.5D
  treatment is next and the voxel geometry here seeds it).
- **Pixel-art map rework** — campaign D.
- **Animated style transitions** — switching styles rebuilds slots (a brief
  rebuild, like a lens swap); no crossfade between styles in v1.
- **Per-style lighting rigs** — all styles share the existing globe light; only
  shading mode (flat vs smooth) and a fixed cliff-darken factor vary.

## 10. Risks

1. **Voxel geometry perf.** Blocks-with-walls are heavier than the shared lattice,
   and The Region campaign already fought main-thread region-geometry rebuild
   jerkiness. Mitigations: bounded voxel granularity (§4 — fewer cells than the
   full 256), walls only at height steps above a threshold, merged per-tile
   geometry. Measure a scripted deep zoom in a real browser (jsdom paints
   nothing); if a rebuild stalls the frame, cap granularity or move the voxel
   build into the region worker. **Perf is the primary execution risk.**
2. **Deeper LOD amplifies (1).** More/finer tiles × voxel cost compounds. Tune the
   ceiling and granularity together against a measured frame budget, not in the
   abstract.
3. **Close-zoom clipping.** Lowering min-distance without matching the near clip
   clips close geometry; adjust together.
4. **Lens × style color interpolation.** Voxel/Terraced take nearest per-cell
   color; a living lens (temperature, day-varying) must still repaint per-cell on
   `repaint` — confirm the per-slot repaint path covers the voxel color layout,
   not just the vertex layout.
5. **Style × region-tile identity.** All four styles must build from region data
   at level ≥ `REGION_MIN_LEVEL`, not just base tiles — the voxel/terraced
   builders need region variants, not only the base-tile ones.

## 11. DoD / registry

- New registry row (id at close), shipped, pointing at the chronicle + this spec;
  note it advances the Orrery view-remake program (B of A–D), cross-link The
  Vantage.
- Chronicle + retro; freshness sweep of any globe-rendering chapter.
- Confidence Gradient: re-score if a rendering/detail bet in `open-questions.md`
  moved (grep the globe/LOD domain before concluding none did).
- Orrery deploy (externally visible — confirm at G6); **no wasm release**.
- Followups: producer-side finer terrain; animated style transitions; voxel-map
  seed for campaign C.
