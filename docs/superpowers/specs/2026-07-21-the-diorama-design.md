# The Diorama — Design

**Campaign:** The Diorama (working name) — a voxel 2.5D style for the Orrery's
flat map rung.
**Registry:** new row (id at CLOSE, per the standing lesson).
**Repos:** Orrery (client-only — **no wasm / producer change**). **Autopilot**
engaged (G3/G6 hard stops).
Third of a program: A view-switch (MAP-65) → B voxel globe (MAP-66) → **C (this)**
voxel 2.5D map → D pixel-art rework.

---

## 1. Goal

Give the Orrery's **map** rung a **Voxel 2.5D** style: the region rendered as a
flat heightfield of voxel blocks, viewed by the map's orthographic camera **tilted
to a fixed isometric angle** — a tabletop relief-model diorama. The map gains a
**style switch** (Voxel-2.5D, default; Pixel-art, the current flat texture,
kept). Answers Nathan's "a new voxel view for the map — more like a 2.5D map or
orthographic projection."

## 2. Keystone (from the ideonomy pass)

Two findings (dimension-identification + the scope axis):

1. **"2.5D" is defined by a non-zero camera PITCH under orthographic projection.**
   Pitch = 0 is today's flat map; the tilt is what makes block height visible and
   turns a chart into a diorama. (Negation test: remove the tilt and it's the flat
   map again.)
2. **The voxel renderer is scope-parametric.** The Massing (MAP-66) wrapped voxel
   blocks on a *sphere* for the global view; this campaign lays the *same honest
   blocks* on a *flat grid* under a tilted ortho camera for the local view. Globe
   = global voxel, Map = local voxel diorama — **one block/wall builder, two
   projections.** On a flat grid it is *simpler* (no cube-sphere corner projection
   — a plain heightfield). The Massing's honest-renderer keystone (one block per
   datum, real cliff walls) carries over unchanged.

## 3. Architecture — a map style switch, mirroring the globe

The globe gained a style switch in The Massing (`GlobeStyle`, `setStyle`, the
`hud-style` dropdown). The map gets the parallel structure:

- A `MapStyle = 'voxel' | 'pixel'` selects how the map rung renders a region.
- `MapView.setStyle(style)` swaps the mounted mesh (voxel diorama vs the current
  textured plane).
- A HUD control (parallel to the globe's), visible in the **Map** view, calls
  `mapView.setStyle(...)`. Default **Voxel-2.5D** (Nathan wants voxel; pixel-art
  is "not good" and is reworked in campaign D).
- Style is a pure function of scene data → geometry (determinism waived
  client-side, 0022/0023). The active data **lens** still colors both styles
  (Style ⟂ Lens).

## 4. The Voxel-2.5D style

The map's region is a `RegionScene` (`scene/tiles-region/v1`) — a grid of
`(samples+1)²` nodes with per-node elevation + the lens fields. Today
(`mapView.ts`) it becomes a `DataTexture` on a flat `PlaneGeometry` under an
`OrthographicCamera` looking straight down.

Voxel-2.5D instead:
- **Flat heightfield of blocks.** Each region cell → an extruded block; block top
  height = the cell's (banded) elevation, exaggerated; **vertical wall faces where
  a cell is higher than a neighbor** (the keystone cliff). Reuse The Massing's
  block/wall/quantize logic (`buildVoxelBlocks` in `worldMesh.ts`) re-instantiated
  on a **flat planar grid** — the cell corners are plane coordinates
  `(x, z)` with `y = height`, instead of cube-sphere unit vectors × radius. Factor
  the shared block-emission core; the flat-grid corner/height mapping is the new
  part.
- **Per-cell flat color** from the active lens (nearest, not blended), cliff
  darkening (`VOXEL_CLIFF_DARKEN`), flat shading — same as the globe voxel.
- **Fixed isometric camera.** The map's `OrthographicCamera` is pitched to a fixed
  isometric angle (~35.264° elevation, 45° yaw — true isometric) and framed so the
  region's diorama fits the frustum with its raised blocks in view. No rotation,
  no pitch control in v1 (a followup). A single directional light gives the blocks
  readable shading (top brightest, walls darker).

## 5. Pixel-art style (kept)

The current flat pixel-art path (`regionPixelTexture` on a top-down plane +
`mapSymbols`) becomes the `'pixel'` branch of `setStyle`, **unchanged** — so
switching to Pixel-art restores today's exact map (the camera returns to top-down
for that style). Campaign D reworks its quality; this campaign only makes it one
of two styles.

## 6. The control (HUD)

Add a **Map Style** control to the HUD, mirroring the globe-style dropdown
(`onGlobeStyle`/`setGlobeStyle` from The Massing; note the pre-existing unrelated
`RenderStyle` axis — do not conflate). `onMapStyle(id: MapStyle)` /
`setMapStyle(style)`; visible in the Map view (match whatever visibility rule the
globe-style control uses — The Massing found the lens control is *always* visible,
so likely always-visible parity). URL parity with the globe style if it is
URL-addressable; else no new URL surface.

## 7. File structure (Orrery)

- `src/views/worldMesh.ts` — a flat-grid voxel builder
  `buildVoxelHeightfieldGeometry(region, opts)` (or extend `buildVoxelBlocks` with
  a planar corner/height mapping) reusing the shared block/wall core. Returns a
  BufferGeometry (+ an `...Indexed` variant if the map needs living-lens repaint —
  the map is a static region snapshot, so likely NOT needed; confirm).
- `src/views/mapView.ts` — a `MapStyle` type; `setStyle(style)`; the voxel branch
  (build the heightfield mesh + a directional light + the isometric camera pose)
  vs the pixel branch (today's textured plane + top-down camera). The camera pose
  (pitch/yaw/frustum) for the isometric view.
- `src/ui/hud.ts` — the Map Style control (`onMapStyle`/`setMapStyle`).
- `src/main.ts` — wire `onMapStyle → mapView.setStyle`; the region-request/deliver
  path already feeds `mapView.setRegion` (unchanged); URL parity if applicable.
- Tests: `worldMesh` unit tests for the flat-grid voxel geometry (known grid →
  blocks + walls at real steps + per-cell color); `mapView` tests for the style
  swap + isometric camera pose; `hud` tests for the control; `e2e/smoke.spec.ts` —
  switch map style + confirm the map canvas re-renders (viewport screenshot).

## 8. Determinism / save-format

**None touched.** Pure client render style + camera pose. No new scene fields, no
stream draws, **no wasm** (stays v12), no epoch. Orrery-only, fully reversible.

## 9. Non-goals / deferred

- **Camera orbit / adjustable pitch** — v1 is a fixed isometric angle (Nathan's
  choice); orbit is a followup.
- **3D diorama props for symbols** — the map's 2D peak/forest/wave symbols stay a
  pixel-art-map feature; re-instantiating them as 3D blocks/props under voxel is a
  followup. Voxel-2.5D v1 is terrain-only.
- **Pixel-art quality rework** — campaign D.
- **In-map pan/zoom to neighbor regions** — the standing map followup, unchanged.
- **Animated transition** between map styles — a mesh swap, no crossfade.

## 10. Risks

1. **Isometric framing.** Getting the fixed camera pose (pitch, yaw, ortho frustum
   extent, near/far) so the raised diorama fits the frame without clipping and
   reads cleanly is the main tuning task — settle it with a screenshot visual pass
   (the controller reads PNGs, as in The Massing). The block height exaggeration
   interacts with the frustum: tall blocks must not clip the far/near planes or
   spill the frame edge.
2. **Reuse fidelity.** The flat-grid builder must genuinely share The Massing's
   block/wall core (DRY), not re-implement it — but the corner mapping differs
   (planar vs cube-sphere). Factor carefully; a bad factor is worse than a small
   duplication.
3. **Style ⟂ Lens on the map.** The voxel diorama must color from the same lens
   the pixel map uses (`pixelColorFor` / the lens field on region nodes) — a voxel
   block's color = its cell's lens color, nearest.
4. **Pixel path untouched.** Switching to Pixel-art must restore today's exact map
   (top-down camera + textured plane) — the pixel branch is the current code,
   verbatim.

## 11. DoD / registry

- New registry row (id at close), shipped, pointing at the chronicle + this spec;
  advances the Orrery view-remake program (C of A–D), cross-link The Massing.
- Chronicle + retro; freshness sweep of any map-rendering mention.
- Confidence Gradient: re-score if a rendering/detail bet moved (grep before
  concluding none did — likely the same runtime-detail bet The Massing touched).
- Orrery deploy (externally visible — confirm at G6); **no wasm release**.
- Followups: camera orbit/pitch, 3D symbol props, pixel-art rework (D).
