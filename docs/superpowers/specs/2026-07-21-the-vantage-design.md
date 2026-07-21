# The Vantage — Design

**Campaign:** The Vantage (working name) — explicit view switching for the Orrery.
**Registry:** new row (id at CLOSE, per the standing lesson).
**Repos:** Orrery (client-only). **Autopilot** engaged (G3/G6 hard stops).
First of a program: A (this) → B voxel globe + detail → C voxel map → D pixel-art map.

---

## 1. Goal

Make switching between the Orrery's three views — **System**, **Globe**, **Map**
— an **explicit** choice (a dropdown), not a side effect of zooming. The wheel
becomes pure zoom *within* the active view. This retires The Cartographer's
zoom-driven crossfade handoff.

## 2. Keystone (from the ideonomy lift)

A navigation system has two **orthogonal** axes: **which view** (a discrete mode)
and **how close within it** (a continuous zoom). The current design overloads one
input — the wheel — for both, coupling them (wheel-to-a-dolly-limit = switch
view). **Decouple them: the dropdown owns view-selection; the wheel owns zoom.**
This also *removes* complexity (no dolly-limit detection, no handoff intents).

## 3. The control

Add a **3-way view dropdown** to the HUD — `System` / `Globe` / `Map` — replacing
the current single toggle button (`setViewButton`). Selecting an option calls
`applyView(v)`. The dropdown reflects the current view. (A native `<select>` or a
small styled dropdown, matching the HUD's idiom; not a segmented control — Nathan
asked for a dropdown.)

## 4. Switching = explicit crossfade

`applyView(v)` already sets `view` and calls `zoom.setTarget(v, now)`, which
drives the `ZoomController`'s eased crossfade between canvases. Keep that — a
smooth animated transition is good; it is now *triggered explicitly* by the
dropdown, never by the wheel. The `ZoomController`, its `[0,2]` ladder, and the
triangular crossfade opacities are **unchanged**. (A non-adjacent switch —
System→Map — eases through the Globe briefly, a smooth zoom-through; acceptable,
tunable later.)

## 5. Wheel = zoom within the active view

Retire the zoom-driven handoff:
- Delete `maybeHandoff` and its wheel listeners' handoff role; the wheel events
  just drive the active view's `OrbitControls` (they already do — `maybeHandoff`
  was *adding* the rung-crossing on top). Zoom clamps at each view's
  `min/max` distance instead of crossing a rung.
- Delete the map-canvas wheel-out→globe handler.
- `wheelHandoff` in `src/views/zoom.ts` becomes dead — remove it and its tests
  (the `ZoomController` + `opacitiesFor` stay; only the handoff-intent surface
  goes).

## 6. Map region on switch

The map rung renders one region tile. Today the `to-map` handoff computed it from
the globe's sub-camera point, then `requestRegion` + `applyView('map')`. Move
that region-pick **into the switch to Map**: when `applyView('map')` runs (or a
small `enterMap()` it calls), compute the region from the **globe's current
center-facing point** (the existing unspun-camera → `containingTile` logic),
`requestRegion(tile)`, set `pendingMapKey`; `deliverRegion` still routes it to
`mapView.setRegion`. So: orient the globe, pick Map, see that region — the
"look, then go there" intuition, decoupled from zoom. If the globe hasn't been
opened yet (straight System→Map deep link), fall back to a sensible default
region (e.g. face 0 / the flagship's tile) rather than crash.

## 7. File structure (Orrery)

- `src/ui/hud.ts` — replace the view toggle with the 3-way dropdown + an
  `onView(v: ZoomTarget)` callback; a `setView(v)` to reflect state.
- `src/main.ts` — wire `onView` → `applyView`; move the region-pick into the
  Map switch; delete `maybeHandoff` + the map wheel-out handler + the wheel
  listeners' handoff calls (keep the wheel driving OrbitControls).
- `src/views/zoom.ts` — remove `wheelHandoff` + `HandoffIntent`; keep
  `ZoomController` / `opacitiesFor` / `RUNG_INDEX`.
- `e2e/smoke.spec.ts` — the map-rung transition test switches from
  "wheel-in to cross" to "pick Map from the dropdown"; add a
  System/Globe/Map dropdown round-trip.

## 8. Non-goals / deferred (the queued campaigns)

- **In-map pan/zoom** — the map stays a fixed view of one region; panning to
  neighbours + zooming within the map is a followup (needs region stitching).
- **Deeper globe zoom / higher LOD** — campaign B.
- **Voxel globe / voxel map / pixel-art rework** — campaigns B / C / D.
- **Non-adjacent transition tuning** (System→Map jump vs through-globe) — v1
  keeps the smooth through-globe ease.

## 9. Risks

1. **Region fetch on switch** — the region arrives async; the map shows its
   placeholder/last region until it lands (the existing discipline). A rapid
   Globe→Map before the region loads must not crash — the placeholder covers it.
2. **Deep-link / boot into Map** — `AppState.view` is currently `system|globe`;
   if Map becomes URL-addressable, widen it (or keep Map non-deep-linkable and
   default to Globe on a Map deep link, as today's `syncUrl` already narrows).
   v1 may keep Map out of the URL.
3. **Pointer-events** — `setCanvasPointerEvents` already gates input to the
   active rung; confirm it still fires on every dropdown switch (it runs inside
   `applyView`).

## 10. DoD / registry

- New registry row (id at close), shipped, pointing at the chronicle + this spec.
- Chronicle + retro; note this *partially retires* The Cartographer's zoom
  handoff (cross-link, don't relitigate).
- Followups: in-map pan/zoom, Map URL-addressability.
