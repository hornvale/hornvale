# The Cartographer — Design

**Campaign:** The Cartographer (working name)
**Registry:** MAP-61 (new) — realizes the "mark-owning cartographic
renderer / legibility fork" MAP-60 (The Idioms) reserved.
**Repos:** Orrery (`~/Projects/hornvale/orrery`) — client-side only.
Governing docs (this spec, plan, chronicle, retro) in hornvale.
**Constraints:** Orrery-only. No sim change, no wasm release. Determinism
waived client-side (decisions 0022/0023), but symbol placement must be
*visually stable* per (world, zoom). Sonnet floor for subagents.

---

## 1. Goal

A **data-native, LOD-driven symbolic renderer** for the Orrery globe: a
render style that reads the scene *data* and draws *symbols* — a crisp
biome/coastline base plus mountains, forests, and settlements that emerge
with detail as you zoom — instead of post-processing the finished photoreal
frame. First vertical slice: **pixel-art**, the FAR base + the first symbol
tier (mountains, forests, settlements). Engraving, the near/landmark rung,
and water features are designed-in additive tiers, not built here.

## 2. The keystone: cartographic generalization, not a filter

Lift the request until the surface falls away: *map a continuous field over
a manifold to a discrete, salience-ranked set of symbols, where the viewing
scale governs how many symbols and of what kind are admitted.* That is
**cartographic generalization** (selection, simplification, symbolization).

The negation test names what makes this a *different engine* and not another
Idioms skin: **remove salience-budgeted feature selection per zoom and it
collapses back into a filter.** The Idioms styles had no salience, no
selection, no notion of a "feature" — so they could only recolor finished
pixels. That is exactly why pixel-art let land take the ocean's colour: a
post-process pass reading the lit frame has no idea which fragment is land.

Corollary (the one assumption The Idioms model must drop): a symbol engine
**reads the scene, not the frame**. It is not orthogonal-to-lens
post-process; it is a scene renderer.

## 3. What the data provides (and the one gap)

The client already holds a full `scene/tiles/v1` document in memory
(`TilesScene`): per-tile `elevation_m`, `ocean`, `biome` (index into
`biomeLegend`), `plate`, `unrest`, temperature, `moisture`, precip, cloud;
and `features` — named settlements + one flagship, each lat/lon. Every
symbol class in this campaign derives from data **already on the client**:

| Feature class | Derived from | Extraction |
|---|---|---|
| Coastline / land-vs-water base | `ocean`, `biome` | per-fragment (already data-native) |
| Mountains | `elevation_m` | local-maxima peak detection + clustering |
| Forests | `biome` (forest names) | connected-component clustering |
| Settlements | `features` | `clusterFeatures` (exists today) |

**The one gap — water features (rivers / inland lakes / waterfalls) —
is deferred** (decision-ledger #4). The data richly exists in the producer
(`domains/terrain/water.rs`: `WaterKind = Ocean/SaltBasin/River/DryLand` from
flow-accumulation drainage; rivers ~6.7% of seed-42 land; `SaltBasin` =
endorheic salt lake; waterfalls at `WATERFALL_MIN_DRAINAGE`) and is already
surfaced at room scale (`windows/locale` `locale/room/v2` `water` field), but
the **global `scene/tiles/v1` emits only `ocean`**. Surfacing it is a small
producer job (`windows/scene` calls the same provider water classification
per tile) — but it needs a **wasm release** to reach the deployed globe, so
it is its own follow-up, sequenced by Nathan (carve-out). See §9.

## 4. Architecture

### 4.1 The style interface widens

Today `RenderStyle` is post-process only:

```ts
interface RenderStyle { id; label; passes(tiles: TilesScene): Pass[]; }
```

Widen it so a style can also own a **base treatment** and a **symbol layer**,
keeping `passes` for the existing filters:

```ts
interface RenderStyle {
  id: string;
  label: string;
  /** Post-process chain (filters). Empty for photoreal and for pure
   *  scene-renderer styles that need no post pass. */
  passes(tiles: TilesScene): Pass[];
  /** How the globe surface is shaded. Absent = today's realistic relief. */
  base?: BaseTreatment;                 // NEW
  /** A scene-graph layer of derived-feature symbols. Absent = none. */
  symbolLayer?: SymbolLayerSpec;        // NEW
}
```

- **Photoreal / cel / engraving / watercolor** keep `passes`-only (`base`
  and `symbolLayer` absent) — unchanged behaviour.
- **Symbolic pixel-art** provides `base` (pixel treatment) + `symbolLayer`
  (mountains, forests, settlements), and no post pass.

`StylePipeline.setStyle` gains the responsibility to apply/clear the base
treatment on the globe and to mount/unmount the symbol layer, in addition to
rebuilding the post chain it already manages.

### 4.2 The base treatment (pixel-art)

The globe mesh is **already** data-native — `MeshStandardMaterial({
vertexColors: true })` colored per-vertex from each tile's biome via
`computeBaseColor(idx, src)`. The pixel base is therefore a *treatment of the
existing data-native surface*, not a new pipeline and not a post filter:

- **Quantized biome palette:** snap each tile's colour to a small stepped
  per-biome palette (the pixel-art look) — read from `biome`/`ocean`
  directly, so **land is never ocean-coloured** by construction.
- **Crisp coastline:** the `ocean` boolean gives a hard land/water edge
  (no relief-shaded gradient).
- **Legible lighting:** replace the continuous terminator with a 2-step
  day/night (or flat), so shading never bleeds into the palette. Validate
  the exact treatment in the visual pass.

This is the death of the land-takes-ocean bug: the base reads the datum, not
the pixel.

### 4.3 The symbol layer

A `THREE.Group` mounted into the globe's existing `spinGroup` (so symbols
rotate with the planet), following the working settlement-marker pattern:

- **Sprites** (`THREE.Sprite` / `SpriteMaterial`) always face the camera —
  billboarding on the sphere is free, exactly as settlement dots/labels do
  today.
- **Near-side culling** each frame via the existing `onNearSide(upWorld,
  cameraPos, radius)` — symbols on the far hemisphere are hidden. This is
  cheap and already proven for markers.
- **Placement** at `latLonToUnit(lat, lon)` scaled to the surface (existing
  helper).

Symbol classes for v1:

- **Mountains** — a peak sprite at each selected peak; size ∝ elevation.
- **Forests** — a small cluster of tree sprites scattered within each
  selected forest region; count ∝ log(area), scatter by a seeded hash of
  tile index (stable, see §5).
- **Settlements** — reuse `clusterFeatures` + the marker builder; flagship
  emphasized.

### 4.4 Feature extraction (pure functions over `TilesScene`)

All row-major over `width × height`, all deterministic:

- **Peaks** — `extractPeaks(tiles)`: a land tile is a candidate peak if its
  `elevation_m` exceeds all 8 neighbours and exceeds a land-elevation
  percentile threshold; salience = elevation. For coarse rungs, cluster
  candidates within an angular radius into massifs, keeping the highest.
- **Forests** — `extractForests(tiles)`: connected-component (4-neighbour)
  clustering of tiles whose `biomeLegend[biome[i]]` is a forest name
  (`taiga`, `temperate-forest`, `temperate-rainforest`,
  `tropical-seasonal-forest`, `tropical-rainforest`, boreal); each region
  carries centroid (lat/lon) + area; salience = area.
- **Settlements** — `clusterFeatures(tiles.features)` (exists); salience =
  flagship > plain.

### 4.5 The salience budget (the LOD core)

Given the camera's zoom (distance → visible angular radius `α`), pick a
**rung** (far / mid / near) and per-rung, per-class **budgets** and
**thresholds**. Selection: rank each class's features by salience, take the
top-N on the near side within view up to the budget. As zoom increases, `α`
shrinks and thresholds lower (smaller peaks, smaller forests, then
settlement labels) — so the *same* discipline yields "more detail emerges as
you zoom in." One tunable table, `RUNG_BUDGETS`, drives it; values are set by
the visual pass, not guessed in the plan.

### 4.6 Integration with the Frame Budget LOD/build system

The symbol set is recomputed **only when the rung changes** (not per frame);
between changes, symbols only run the cheap near-side cull. When a rung
change triggers a rebuild, it must **ride the existing amortized build queue**
(`drainBuildQueue`, `BUILD_BUDGET_MS`) rather than rebuilding synchronously —
The Frame Budget campaign exists precisely to keep per-frame work bounded, and
a naive symbol rebuild would reintroduce the burst it removed.

## 5. Determinism & placement stability

Client determinism is waived (0022/0023), but *shimmer* is a visual defect.
Every placement is a pure function of `(tiles, rung)`:

- Feature extraction is over the fixed field — deterministic.
- Intra-cluster scatter (trees within a forest) uses `hash(tileIndex)`,
  **never `Math.random`** — identical across frames and rotations.
- Symbols recompute only on rung change; rotating/zooming within a rung
  reuses the same set. Result: no shimmer, no popping beyond the deliberate
  rung transitions.

## 6. Testing

Green ≠ correct for client rendering (The Idioms lesson): the controller's
**screenshot visual pass is the acceptance gate**. But the high-value
*mechanical* correctness lives in pure functions we can test headlessly:

- **Unit (vitest):**
  - `extractPeaks` — a synthetic elevation field with known local maxima →
    exact peak set + salience order.
  - `extractForests` — a field with known forest components → exact
    component count, areas, centroids.
  - salience selection — top-N-by-salience-within-budget correctness,
    including near-side filtering.
  - **base colour regression (Nathan's bug):** the pixel base colour
    function maps a known LAND tile to a land-palette colour and a known
    OCEAN tile to an ocean-palette colour — a direct guard that land can
    never take the ocean's colour.
  - scatter stability — same `(tiles, rung)` → byte-identical sprite
    positions on repeat calls.
- **e2e smoke tripwire (extends the style-roster test):** the symbolic
  pixel-art style renders **non-blank** (>5 KB PNG — a failed material/shader
  compiles to a tiny black frame) and **differs from photoreal**; plus a
  **rotation-stability capture** — at zoom Z, rotate θ vs θ+ε, assert a
  sampled land region stays land-coloured (mechanizes the exact bug).
- **Visual pass (controller):** screenshots at far / mid / near zoom on
  seed 42 showing the base + emerging symbols, judged on-hardware. The
  Idioms **shader-gotcha checklist** applies verbatim (cloned-uniform
  setSize, GLSL reserved words, opaque clear, no backticks, no
  dynamically-indexed local arrays) — any of them renders a black globe
  invisibly to green tests.

## 7. File structure (Orrery)

- `src/views/renderStyle.ts` — widen `RenderStyle`; `StylePipeline` applies
  base + mounts/unmounts symbol layer.
- `src/views/symbols/extract.ts` — pure feature extraction (`extractPeaks`,
  `extractForests`; re-export `clusterFeatures`) + `extract.test.ts`.
- `src/views/symbols/budget.ts` — rung selection + `RUNG_BUDGETS` +
  salience selection + `budget.test.ts`.
- `src/views/symbols/symbolLayer.ts` — build/update the `THREE.Group`
  (peak/tree/settlement sprites), near-side cull, ride the build queue +
  `symbolLayer.test.ts`.
- `src/views/styles/pixelBase.ts` — the quantized data-native base treatment
  + `pixelBase.test.ts` (the base-colour regression).
- `src/views/globe.ts` — hook a base treatment onto the mesh material /
  `computeBaseColor`; expose a symbol-layer mount point in `spinGroup`.
- `src/main.ts` — wire the widened style application.
- `e2e/smoke.spec.ts` — extend the style-roster + rotation-stability test.

## 8. Vertical-slice scope (what ships in this campaign)

**In:** the widened style interface; the pixel-art data-native base (bug
fix); peak + forest + settlement extraction; the salience-budget LOD;
symbols-on-sphere with near-side cull riding the build queue; the test
battery above; the picker replaces the filter pixel-art with the symbolic
one (ledger #5). Cel / engraving / watercolor filters stay as-is.

**Out (designed-in additive tiers, not built here):** §9.

## 9. Non-goals / deferred tiers

- **Water features** (rivers, inland lakes, waterfalls) — needs the
  `windows/scene` producer field + a wasm release (carve-out, Nathan
  sequences). Two followups: (a) producer — `windows/scene` emits a
  per-tile water class; (b) client — a water symbol tier (river polylines,
  lake fills, waterfall icons). The client tier is cheap once the field
  ships.
- **The near / landmark-icon rung** — biome-signature icons (volcano from
  `unrest`+elevation, cactus, giant mushroom), structures (bridges, city
  clusters). The mid rung's *biome-signature icons* are a stretch goal only
  if the slice lands early; the full landmark rung is a later campaign.
- **Symbolic engraving** — the same engine with an engraving symbol atlas +
  palette (Nathan's Obra Dinn "magical zoomable globe"). The intended next
  conversion after pixel-art proves the keystone.
- **Cel / watercolor** conversion from filter to scene-renderer.
- **Flat-map mode** — an equirectangular pixel-map surface (matches the
  references more directly; abandons the globe identity). Registered axis.
- **Sub-cell texture** (MAP-49) as a near-rung base enrichment.

## 10. Risks

1. **Black-render bug class** carries over to any new material/base shader —
   mitigated by the visual pass + the smoke tripwire + the checklist.
2. **Perf on the sphere** — many sprites — mitigated by the budget +
   near-side cull + riding the amortized build queue (do not regress The
   Frame Budget).
3. **Base/terminator interaction** — the pixel base must stay legible under
   day/night; the 2-step-lighting choice is validated in the visual pass.
4. **Mixed-kind picker** (symbolic pixel-art beside filter styles) — an
   accepted v1 interim; the interface generalizes so both coexist cleanly.

## 11. DoD / registry

- New registry row **MAP-61** (this campaign); MAP-60 note repointed to name
  it as the realization of the reserved legibility fork.
- Chronicle `book/src/chronicle/the-cartographer.md` + SUMMARY wiring;
  retrospective `docs/retrospectives/the-cartographer.md`.
- Confidence Gradient: check whether a rendering/legibility bet moves.
- Captured followups (water ×2, engraving, landmark rung, flat-map) in the
  registry / followup register.
