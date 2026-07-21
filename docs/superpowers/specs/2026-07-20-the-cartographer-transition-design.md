# The Cartographer — Transition Design (pivot)

**Supersedes** the design in
`2026-07-20-the-cartographer-design.md` (symbols-on-the-globe). That spec's
keystone — *cartographic generalization, salience-budgeted symbols* — stands;
what changes is **where** the pixel-art idiom lives. This spec is the campaign's
governing design from here.

**Campaign:** The Cartographer · **Registry:** MAP-62 · **Repo:** Orrery
(client-only; no sim/wasm change) · **Autopilot** engaged (G3/G6 hard stops).

---

## 1. The pivot (why)

The visual pass shipped a data-native flat pixel-art *globe*. It fixed the
land-takes-ocean bug and reads like a pixel map — but forcing pixel-art onto a
sphere fights the medium: square pixels **advertise** the projection's
stretch/compression, and the wave/limb jumble was a symptom of exactly that.
Web research confirmed it — the whole industry (Mapbox, MapTiler, Google Maps)
renders a **flat rectangular map when zoomed in** and morphs to a **globe when
zoomed out**; LOD convention is a *simple legible overview* far out, *stereotyped
detail* close in. Nathan's own instinct matched: keep the globe a clean
overview, put the stereotyped pixel-art detail at a flatter, zoomed-in scale.

**Ideonomy keystone (microscopy re-instantiation):** a microscope swaps
*objectives* (4×→40×) with a discrete click — each magnification wants a
different technique — rather than continuously morphing one lens. So the
globe→flat transition is a **discrete idiom-handoff (a clean crossfade between
rungs), not a from-scratch geometric morph.** This is tractable *and* is exactly
what the Orrery's existing rung machinery (`ZoomController`, `wheelHandoff`)
already does between `system` and `globe`.

## 2. The rung ladder

| Rung | Projection | Idiom | Detail |
|---|---|---|---|
| **system** | 3D orbital | bodies in space | existing, untouched |
| **globe** | 3D sphere | **clean stylized biome overview** | coarse — NO waves, NO pixel-fighting-the-ball |
| **map** *(new)* | **2D rectangular** | **pixel-art RPG map** | trees / mountains / waves / biome icons / landmarks, with its own internal zoom-LOD |

## 3. What is reused, reverted, and new

- **Reused (the pivot does not waste the work):** the curated biome palette
  (`pixelBase.ts`'s `PIXEL_LAND_RGB` + ocean depth-tones), `symbols/extract.ts`
  (peaks/forests), `symbols/budget.ts` (rung selection + salience budget), and
  the symbol sprites (`symbolLayer.ts`) — all move to the **map** rung, where a
  flat plane means pixel-art and symbols work without distortion.
- **Reverted off the globe:** the symbol layer, the ocean wave-marks, the
  glint/waves/water-sphere/ice-blend suppression, and (pending §4) the hard
  pixel base. The globe becomes a clean overview again.
- **New:** the **map** rung — a flat region renderer — and the **three-rung
  transition** (`system ↔ globe ↔ map`).

## 4. Rung: globe (clean overview)

The globe returns to a legible planetary overview: smooth stylized biome
shading (the existing `natural` lens / relief globe), the settlement markers,
and the data lenses — but **no symbol layer, no wave-marks, no pixel treatment**.

**Flagged decision (Nathan, at Stage 1's visual pass):** does the globe keep a
*softened* pixel-biome look (the current base minus waves, which Nathan called
"the appropriate level of detail") or revert fully to the smooth photoreal-relief
overview? Research favors smooth on a sphere; Nathan's eye decides on seed 42.
Either way the pixel-art detail is gone from the ball.

## 5. Rung: map (the new flat pixel-art tier)

A 2D rectangular pixel-art map of the region the camera was over when it left
the globe. Its own canvas + orthographic camera (the same twin-renderer idiom
`system`/`globe` already use).

### 5.1 Data
The higher-res **region tiles** (`scene/tiles-region/v1`, `RegionScene`) already
carry per-node `biome` / `ocean` / `elevation_m` / `biomeLegend` over a
`(samples+1)²` grid for a cube-sphere face tile — fetched from the worker via the
existing `requestRegion({face,level,ix,iy})` → `onRegion` path. The map rung
renders the region tile under the sub-camera point; neighbor stitching is a
later refinement (§9).

### 5.2 Base render (pixel-art, flat)
Build a **crisp pixel texture** from the region grid: each node → the curated
`PIXEL_LAND_RGB` / ocean depth-tone (reused verbatim), written into a
`THREE.DataTexture` with `NearestFilter` (hard pixels), mapped onto a quad in
the orthographic scene. A flat grid means **no sphere distortion** — pixel-art
as it is meant to be. This is the payoff.

### 5.3 Symbols (reused engine)
Run `extractPeaks` / `extractForests` on the region grid and place **2D sprites**
(peaks / tree-clusters / wave-marks / biome icons) on the quad — no billboarding,
no limb cull, no floating (the problems were all sphere artifacts). Selection is
the reused `budget.ts` against the map's **internal** zoom.

### 5.4 Internal LOD (the rung ladder, now on the flat map)
Zoom within the map drives the salience budget (the far/mid/near ladder from the
superseded spec, now living here): coarse features first; forests as tree
clusters, then individual trees; biome-signature icons (volcano from
`unrest`+elevation, cactus, mushroom) and landmarks (large cities from
`features`) as you approach. Water features stay the deferred producer tier
(§9).

## 6. The three-rung transition

Extend `ZoomController` from a 2-state (`system`=0 ↔ `globe`=1) to a 3-rung
ladder (`system` ↔ `globe` ↔ `map`), each adjacent pair eased over the existing
`durationMs` crossfade. `wheelHandoff` gains: `globe` + wheel-in past
`minDistance` → `to-map`; `map` + wheel-out → `to-globe`. On `to-map`, the map
rung is initialized to the globe's sub-camera lat/lon (the region under the
camera). The handoff is a **crossfade between the two canvases**, not a geometric
morph (keystone §1). `ZoomTarget` becomes `'system' | 'globe' | 'map'`; the
per-rung state records (`trueScaleOn`, captions, view buttons) extend to include
`map`.

## 7. File structure (Orrery)

- `src/views/zoom.ts` — `ZoomTarget` +`'map'`; 3-rung `ZoomController`;
  `wheelHandoff` globe↔map.
- `src/views/mapView.ts` *(new)* — the flat map rung: orthographic scene, the
  region→pixel `DataTexture` base, symbol placement, internal zoom-LOD.
- `src/views/mapTexture.ts` *(new)* — region grid → curated-palette
  `DataTexture` (+ test).
- `src/views/symbols/extract.ts`, `budget.ts` — unchanged (consumed by the map).
- `src/views/symbols/symbolLayer.ts` — generalized/forked for 2D flat placement
  (drop billboarding/limb-cull); or a thin `mapSymbols.ts` that reuses
  `extract`/`budget` and places plain 2D sprites.
- `src/views/globe.ts` — remove the symbol-layer mount + wave/glint/water
  suppressions; keep `setBaseTreatment` only if Stage 1 keeps a softened base.
- `src/views/renderStyle.ts` — the pixel-art *style* is retired from the globe
  picker; the map rung is a zoom rung, not a style.
- `src/main.ts` — third renderer/canvas + camera for `map`; wire the 3-rung
  ladder, handoff, and region fetch-on-handoff.

## 8. Staging

1. **Globe cleanup.** Strip the symbol layer + wave/glint/water/ice-blend
   suppressions off the globe; resolve the §4 look with Nathan's visual pass.
   Deliverable: a clean globe overview, green tests. *(Reverts most of the
   visual-pass commits; keeps the pure modules.)*
2. **The map rung + transition.** New `ZoomTarget 'map'`, 3-rung
   `ZoomController`/`wheelHandoff`, a third canvas/orthographic camera, and a
   placeholder flat quad showing the region under the camera on handoff.
   Deliverable: zoom into the globe → crossfade to a flat placeholder map → zoom
   out → back. Green tests + a smoke tripwire.
3. **Flat pixel-art base.** `mapTexture.ts`: region grid → curated-palette
   `NearestFilter` `DataTexture` on the quad. Deliverable: a crisp, undistorted
   flat pixel-art biome map of the region. Controller visual pass.
4. **Map RPG symbols + internal LOD.** Reuse `extract`/`budget`; place 2D
   peak/tree/wave/icon sprites; internal zoom drives the salience ladder;
   biome-signature icons + landmark cities at closest zoom. Controller visual
   pass. *(Water features remain the deferred producer tier.)*

## 9. Non-goals / deferred

- **Continuous geometric morph** (globe literally unwrapping to a plane) — the
  discrete crossfade handoff is the keystone; a true morph is out.
- **Region neighbor stitching** on the map (seamless pan across region-tile
  boundaries) — v1 shows the single region under the camera; multi-region pan is
  a refinement.
- **Water features** (rivers / lakes / waterfalls) — still needs the
  `windows/scene` producer field + a wasm release (carve-out, Nathan sequences).
- **Room/locale tier** (`windows/locale`, the ground-level walk) — a deeper rung
  below `map`, out of scope here.
- **Flat-map export / print** — out.

## 10. Risks

1. **Scope** — a whole new view tier is the campaign's largest lever; the
   staging (globe-clean → transition → base → symbols) keeps each stage
   independently shippable and testable.
2. **Region data on handoff** — the worker fetch is async; the map rung must
   handle the fetch-in-flight gracefully (a loading state, not a blank/crash),
   the same discipline the globe's region upgrades already use.
3. **Three-renderer cost** — a third canvas/renderer; only the active rung
   renders (the others pause), as `system`/`globe` already do.
4. **Determinism of invented detail** — icons/landmark selection is client
   authoring (determinism waived, 0022/0023), but placement must be *stable*
   per (region, internal-zoom) — reuse the `hash01`, no `Math.random` rule.

## 11. DoD / registry

- **MAP-62** repointed to this transition design; the superseded symbols-on-globe
  spec is cross-linked as its first iteration.
- Chronicle `book/src/chronicle/the-cartographer.md` tells the whole arc —
  including the pivot (pixel-on-sphere fought the medium; the flat tier is where
  it belongs). Retrospective captures the visual-pass-drove-a-pivot lesson.
- Captured followups (water tier, region stitching, room tier, continuous morph)
  in the registry / followup register.
