# The Vantage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Explicit 3-way view switching (System / Globe / Map dropdown); the wheel zooms within a view; retire the zoom-driven handoff.

**Architecture:** The dropdown calls `applyView(v)` (which already triggers the `ZoomController` crossfade). The wheel just drives each view's OrbitControls (clamped, no rung-crossing). The Map switch computes its region from the globe's current center-facing point.

**Tech Stack:** TypeScript, three.js, Vitest, Playwright. Orrery-only, no sim/wasm change.

## Global Constraints

- Orrery-only. No sim/wasm change, no release.
- The `ZoomController`, its `[0,2]` ladder, `opacitiesFor`, `dollyPosition`/`dollyLookAt`, and `applyView`'s crossfade wiring are **unchanged** — only the *trigger* changes (dropdown, not wheel).
- Green ≠ correct: the controller runs a screenshot visual pass at the end (switch all three ways; confirm the crossfade + the Map region). Rebuild + kill `:4173` before capture.
- Sonnet floor for subagents.

---

### Task 1: The 3-way view dropdown (HUD)

**Files:** Modify `src/ui/hud.ts`; test `src/ui/hud.test.ts` (if one exists; else assert via the e2e in Task 4).

**Interfaces:**
- Produces: a `<select>`-based (or small styled) dropdown in `topLeft` with options `System` / `Globe` / `Map` (values `'system'|'globe'|'map'`), replacing the `viewToggle` button (L135). `HudCallbacks` gains `onView(v: ZoomTarget): void` (import `ZoomTarget` from `../views/zoom`); the returned `Hud` gains `setView(v: ZoomTarget): void` reflecting the current selection. Remove `setViewButton` (and its `HudCallbacks`/`Hud` surface) — nothing else should need it after Task 2; if a caller does, update it.
- Consumes: nothing new.

- [ ] **Step 1:** Read `src/ui/hud.ts` around L135 (the `viewToggle` button, its click handler + callback, `topLeft.append`), the `HudCallbacks` interface (L15), and the returned `Hud` object (L313 `setViewButton`).
- [ ] **Step 2:** Replace the `viewToggle` button with a `<select class="hud-view">` (or an `el('select', 'hud-view', '')` per the file's `el` idiom) holding three `<option>`s; on `change`, call `cb.onView(select.value as ZoomTarget)`. Append it where `viewToggle` was.
- [ ] **Step 3:** In the returned `Hud`, replace `setViewButton` with `setView(v)` that sets `select.value = v`.
- [ ] **Step 4:** `HudCallbacks`: replace `onViewToggle` (the button's old callback — find its real name) with `onView(v: ZoomTarget)`; the interface `Hud`: replace `setViewButton` with `setView`.
- [ ] **Step 5:** `npx tsc --noEmit` will now fail in `main.ts` (old callback/`setViewButton` gone) — that's Task 2. Confirm `hud.ts` itself is internally consistent; run `npm test -- hud` if a hud test exists.
- [ ] **Step 6: Commit** `feat(vantage): 3-way view dropdown in the HUD`.

### Task 2: Wire the dropdown; move the region-pick; retire the handoff (main.ts)

**Files:** Modify `src/main.ts`.

**Interfaces:**
- Consumes: Task 1's `onView`/`setView`; the existing `applyView`, `ZoomController`, `containingTile`/`tileKey`, `requestRegion`, `pendingMapKey`, `globeCamera`/`globeControls`, `system`/`day`/`seasonalHoldOn`, `seasonalSpinZ`.

- [ ] **Step 1: Move the Map region-pick into the switch.** Cut the `to-map` block (main.ts ~L409–420: the unspun-camera → `containingTile` → `pendingMapKey` → `requestRegion`) into a helper `function enterMapRegion(): void { … }`. In `applyView(v)`, after setting `view`/`zoom.setTarget`, add: `if (v === 'map') enterMapRegion();`. Guard: if the globe camera/region context isn't ready (a straight boot into map), pick a default tile — `containingTile([0,0,1], 3)` (face-0 pole) or the flagship's tile — rather than crash.
- [ ] **Step 2: Wire the dropdown.** In the `HudCallbacks` object passed to `buildHud`, replace the old view-toggle callback with `onView: (v) => { applyView(v); syncUrl(true); }`. Replace `setViewButtonFor`'s `hud.setViewButton(...)` calls with `hud.setView(view)` (the dropdown reflects the current view on every switch + at boot).
- [ ] **Step 3: Retire the handoff.** Delete `maybeHandoff` (~L396–423) and the two wheel listeners that call it (the `systemCanvas`/`globeCanvas` `addEventListener('wheel', …maybeHandoff…)`). Delete the map-canvas wheel-out handler (`if (view === 'map' && e.deltaY > 0) applyView('globe')`, ~L440). OrbitControls keep their own wheel-zoom, so the wheel still zooms within a view. Remove the now-unused `wheelHandoff` import.
- [ ] **Step 4:** `npx tsc --noEmit` clean; `npm run build` succeeds; `npm test` green.
- [ ] **Step 5: Commit** `feat(vantage): explicit view switching; region-pick on Map switch; retire the wheel handoff`.

### Task 3: Remove the dead handoff surface (zoom.ts)

**Files:** Modify `src/views/zoom.ts`; `src/views/zoom.test.ts`.

- [ ] **Step 1:** Confirm nothing imports `wheelHandoff`/`HandoffIntent` anymore (`grep -rn wheelHandoff src` → only `zoom.ts`/`zoom.test.ts`).
- [ ] **Step 2:** Delete `wheelHandoff` (L137) and `HandoffIntent` (L135) from `zoom.ts`. Keep `ZoomController`, `opacitiesFor`, `RUNG_INDEX`, `dollyPosition`, `dollyLookAt`, `ZoomTarget`, `ZoomState`, and the eases.
- [ ] **Step 3:** Remove the `wheelHandoff` tests from `zoom.test.ts` (the `ZoomController`/`opacitiesFor` tests stay).
- [ ] **Step 4:** `npx tsc --noEmit` clean; `npm test -- zoom` green; `npm run build` succeeds.
- [ ] **Step 5: Commit** `refactor(vantage): remove the dead wheel-handoff surface`.

### Task 4: e2e — dropdown switching

**Files:** Modify `e2e/smoke.spec.ts`.

- [ ] **Step 1:** The existing "map rung" test crosses via wheel-in — rewrite it to switch via the dropdown: load `#seed=42&view=globe`, select `Map` from the `.hud-view` dropdown (`page.locator('.hud-view').selectOption('map')`), wait for the map canvas opacity → 1, assert its element screenshot is non-blank; select `Globe`, assert the globe returns. (If the existing test relied on the wheel handoff, it must now use the dropdown — do not leave a wheel-handoff assertion.)
- [ ] **Step 2:** Add a System→Globe→Map→System round-trip via the dropdown, asserting each canvas's opacity reaches 1 in turn.
- [ ] **Step 3:** Confirm the wheel no longer switches views: on the globe, wheel-in hard and assert the view is still `globe` (the globe canvas stays opaque, map stays 0).
- [ ] **Step 4:** `npm run build && npm run e2e` → PASS.
- [ ] **Step 5: Commit** `test(vantage): dropdown view-switching + wheel-no-longer-switches`.

---

## After Task 4
Controller visual pass (switch all three ways; confirm the crossfade + that Map shows the globe's center region). Then whole-branch review, then G6 — chronicle (note it partially retires The Cartographer's handoff), retro, registry row (id at close), memory; merge + deploy.
