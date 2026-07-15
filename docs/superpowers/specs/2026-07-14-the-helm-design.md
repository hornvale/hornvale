# The Helm — Design

**Date:** 2026-07-14
**Status:** Draft — awaiting G3 review
**Kind:** Campaign spec.
**Parent spec:** `2026-07-14-goldengrove-design.md` (the catalog/planetarium
seam; decisions 0022, 0023, 0055 govern). This campaign is Goldengrove's
first follow-up: the instrument earns its controls.
**Provenance:** Nathan's issue list on the live client (rotation speed,
dead True Scale, no mouse camera, flat world in the system view, no
surface view, unclickable settlements, the rename-to-Orrery question),
plus the campaign retrospective's queued follow-ups #1 and #3.

---

## 1. Goal

One deliverable: the `hornvale/goldengrove` client goes from a scripted
exhibit to a piloted instrument. Every control on screen does what it
says, the camera belongs to the user at both altitudes, the clock keeps
pace with the altitude, the render can drop its two admitted lies on
demand, and anything nameable on screen can be clicked and inspected.

**Scope boundary (load-bearing):** everything in this campaign is pure
client — zero changes to `scene/system/v1`, `scene/tiles/v1`, or the
wasm catalog. Decision 0055 makes scene kinds cross-repo contracts;
the first widening (eccentricity, ORRERY-ellipse-truth) and the
surface-descent data feed are deliberately *not* mixed into a controls
campaign. Hornvale-side changes are docs only (this spec, registry rows,
chronicle at close).

## 2. Diagnosis (what the code says about each report)

The reported issues, traced in `hornvale/goldengrove` source:

1. **"Rotation is stupid fast at planet level."** The two views share one
   clock; the boot default is `yearDays / 12` days per real second
   (`main.ts:26-28` — a year in ~12 s, tuned for the system view). The
   globe spins once per `dayLengthDays`, so the default is ~30
   revolutions per second at the globe — a blur. The top half of the HUD
   speed ladder blurs it too. The HUD's cap API (`setMaxSpeed`) exists and
   is called once with `null`; never per-view.
2. **"True Scale doesn't work."** `onTrueScale() {}` — a documented Task
   10 no-op (`main.ts:263-268`). The HUD plumbing
   (`setTrueScaleLabel/Active`, the two-toggle comment in `hud.ts`) is
   built and waiting.
3. **"No mouse/scroll camera."** There is no pointer handling anywhere;
   both cameras are entirely scripted (`main.ts` positions them, the
   zoom ladder dollies the system camera every frame).
4. **"System view should show the actual planet."** The world in the
   system view is a flat blue `MeshStandardMaterial` sphere
   (`system.ts:141-149`); the real cube-sphere mesh exists one file over
   (`globe.ts`) and is never reused.
5. **"Clickable settlements."** Markers and label sprites render
   (`globe.ts:166-182`) but nothing listens.
6. **Unreported but found (the same disease as #2):** `onReroll`,
   `onShare`, and `onDateJump` are also no-ops; the HUD date display is
   never driven (`setDate` has no caller — it shows "—" forever); the
   boot speed highlight marks "1×" while the actual rate is ~1 mo/s (the
   retro's nit); dead `hud-compass` CSS; unused `moon.ts` module.

## 3. Design principles

1. **One axis, not three features.** Scroll-zoom, the shipped
   system↔globe transition, and the future surface view are the same
   dimension: camera altitude. This campaign makes the existing two-rung
   ladder user-driven end to end; the sequel (§9) extends it below the
   globe. Nothing is built that the descent will have to tear out.
2. **The clock follows the altitude.** A watchable speed at one rung is
   a blur at another; speed defaults, caps, and the user's last choice
   are per-rung state, not globals.
3. **The render admits its lies — and can retract them.** Goldengrove
   ships two captioned distortions (moon rungs, 60× relief). True Scale
   retracts both, and the camera agency from principle 1 is what makes
   the retracted view explorable rather than blank.
4. **Inspection over decoration.** Anything with a name on screen —
   settlement, moon, star, the world — answers a click with its numbers,
   sourced only from the scene documents already in hand.
5. **Boring solutions.** `OrbitControls` from `three/addons` (already
   inside the pinned `three@0.166` — no new runtime dependency), plain
   raycasting, plain DOM for the info card.

## 4. The helm: camera agency and the altitude ladder

**Per-view OrbitControls.** Both views get drag-to-orbit and
wheel/pinch-to-zoom. System view orbits the star (target follows
`dollyLookAt` during transitions); globe view orbits the globe center.
Distance limits per rung: the system rung between "moons resolvable" and
"whole HZ in frame ×2"; the globe rung between "just above the
exaggerated relief" and "whole disc ×3".

**Wheel-through handoff.** Zooming past a rung's limit hands off to the
existing `ZoomController`: wheel *in* on (or near) the world in the
system view triggers the system→globe transition; wheel *out* past the
globe ceiling triggers globe→system. The view-toggle button, deep-link
`#view=`, and the 1.5 s eased cross-fade all stay — the wheel becomes a
third way to ask for the same transition, not a new mechanism. During a
transition user input is ignored (the dolly owns the camera for 1.5 s,
as today).

**Camera state is not URL state.** `#seed/view/day` deep links stay as
shipped; orbit angle and dolly distance are session-local. (A camera
pose in the URL is a capture-worthy later nicety, not this campaign.)

## 5. The clock: per-rung speed policy

- Each rung carries `{defaultStep, maxStep, lastChosen}` over the
  existing `SPEED_STEPS`.
- **System rung:** default stays the shipped year-in-~12s — but snapped
  to its nearest real step (`~1 mo/s`) so the HUD highlight is honest
  from boot (fixes the retro nit).
- **Globe rung:** default `1 hr/s` (one rotation in ~24 s — stately,
  watchable); cap `1 day/s` via the existing `setMaxSpeed` — the blur
  rates are simply not offered at this altitude.
- Toggling views restores that rung's `lastChosen` (or its default) and
  re-caps the HUD; the scrubber and pause state are untouched.

## 6. True Scale: both lies, retractable

One button, per-rung semantics, exactly as the HUD's two-toggle comment
anticipated:

- **System rung:** moon orbits move from the schematic rung ladder to
  true `distanceMm`, and body radii drop to true scale with them. At
  true scale the moons and world all but vanish against the orbit's
  sweep — that is the lesson (ORRERY-scale-honesty), and the §4 camera
  makes it explorable: zoom in and *find* the world. Camera near-plane
  and orbit limits scale with the toggle. Label sprites keep their
  schematic size (billboarded, not shrunk with the bodies) so the
  bodies remain findable.
- **Globe rung:** relief exaggeration 60× → 1×.
- The caption line and the button label state which lie is currently
  retracted (`setTrueScaleLabel`/`setTrueScaleActive`, already built);
  the toggle state is per-rung and session-local.

## 7. The world wears its face; the inspector

**The system-view world becomes the real world.** The flat blue sphere
is replaced by the same six-face cube-sphere build the globe uses
(vertex colors from ocean/biome sampling), at `WORLD_RADIUS`, with
relief displacement off (invisible at that size), spun by
`rotationPhase` — the same face at every altitude. The mesh is built
once from the tiles document and shared/cloned between views.

**Click-to-inspect.** One pointer handler per view raycasts on click
(with a small movement threshold so orbit-drags never fire it) and opens
a plain DOM info card; click-away or `Esc` closes it. Contents come only
from the scene documents plus already-golden derivations:

- **Settlement / flagship:** name, kind, lat/lon, and the local sample
  (elevation, ocean/biome) from the tiles lattice.
- **Moon:** sidereal period, true distance (`distanceMm`), relative
  size, current illuminated fraction and days to next full (computed
  from the golden `moonPhase` — surfacing, not reimplementing).
- **Star:** class, relative luminosity, HZ span.
- **World:** orbit radius, year length, day length (or tidally locked),
  obliquity, current day-of-year.

This ships ORRERY-ephemeris-inspector and subsumes "settlements should
be clickable."

## 8. Dead controls wired; verification grows a browser

- **Reroll:** sets the hash to a fresh random seed — the existing
  hashchange→reload path does the rest.
- **Share:** clipboard-writes the canonical URL, `flashShared()`
  confirms.
- **Date display and date-jump:** driven by a client-derived raw
  calendar — year = `floor(day / yearDays)`, day-of-year, and a local
  clock face from the day fraction (24 "hours" convention already in
  `hud.ts`). No months, honestly labeled day-count reckoning: the
  calendar's month/leap structure lives sim-side and is *not* pulled
  across the schema boundary for a HUD line (§1 scope boundary).
  Date-jump maps (year, day) → ephemeris day and scrubs.
- **Cleanup:** dead `hud-compass` CSS out; `moon.ts` dropped if the
  golden functions live in `ephemeris.ts` (plan-level check); dead
  callbacks all gone — after this campaign, no control on screen is a
  no-op.
- **Playwright smoke (retro follow-up #1):** a real-browser check in CI
  (PR + deploy): boot seed 42, await the rendered canvas, assert HUD
  presence, click a settlement, toggle the view, fail on console
  errors. The camera work is exactly the pixel-adjacent seam the
  base-path 404 proved needs a real browser.

## 9. Non-goals, and the sequel this campaign sets up

- **The descent (surface view / walk / fly)** — the ladder's next rungs
  down. Needs a local-relief data feed beyond the 512-wide global
  equirect (regional re-request at higher `tilesWidth`, or a new
  catalog call — the first real widening under decision 0055's
  discipline). Captured as **ORRERY-surface-descent** in the idea
  registry; explicitly the successor campaign, and §4's ladder is built
  to take the extra rungs.
- **Eccentricity** (ORRERY-ellipse-truth) — stays the queued
  first-schema-change campaign, unchanged.
- **Sibling planets, months-in-HUD, camera-pose deep links, label
  declutter at zoom** — captured, not built.

## 10. The name (G3 decision — Nathan's call)

Nathan is inclined to rename the client **Orrery**. Analysis (ideonomy
pass, dimension-identification + timeline):

- **Now is the cheapest this rename will ever be.** The client is one
  day old; inbound links round to zero. A GitHub repo rename redirects
  git remotes but **Pages URLs do not redirect** — `/goldengrove/` would
  404, so the book/chronicle links and `vite.config.ts` base must move
  with it. Every week of delay raises the cost.
- **The counterargument is the trajectory, not the present.** Today the
  client *is* mostly an orrery, and the name is honest, classic, and
  self-describing. But this campaign builds the altitude ladder and §9
  points it at the surface: a client whose roadmap descends to walking
  the ground outgrows "Orrery" exactly the way it would outgrow
  "Globe." The old `clients/orrery` was killed partly because
  Goldengrove is *more* than the orrery was.
- **Candidates:** **Orrery** (honest today, may under-name tomorrow);
  **Goldengrove** (status quo: poetic lineage, says nothing);
  a name from the project's own window vocabulary (the sim is observed
  through windows; The Casement set the precedent) that names the
  *trajectory* — e.g. **Belvedere**, a structure built to command a
  fine view from any vantage, which is precisely what this client is
  becoming.
- **Recommendation:** if renaming, prefer a trajectory name (Belvedere)
  over Orrery; between Orrery and Goldengrove alone, Orrery — clarity
  beats lineage for a public instrument. Whatever is chosen, execute
  inside this campaign (repo rename, Pages base, book links, README) so
  the cost is paid once, while it is near zero.

## 11. Decisions (promoted from the autopilot ledger)

1. **Scope split** — pure-client controls campaign now; surface descent
   captured as the sequel (needs the catalog widening this campaign
   refuses to mix in).
2. **Calendar stays client-derived** — no scene-schema widening for a
   HUD line; day-count reckoning, honestly labeled.
3. **Per-rung clock policy** — defaults/caps/last-choice are per
   altitude rung; blur rates are not offered where they blur.
4. **No new runtime dependency** — OrbitControls via `three/addons`.
5. **Playwright enters CI** (devDependency) — the retro's queued
   mechanization, landed with the campaign that most needs it.
6. **The rename is executed in-campaign once Nathan rules** (§10) — an
   externally visible act, so it is a G3 item, never auto-resolved.

## 12. Testing

- **Pure math under vitest, as shipped code already does:** speed-policy
  table (per-rung default/cap/restore), calendar derivation round-trips
  (day ↔ year/day-of-year/clock, including year boundaries and the
  tidally-locked null day length), true-scale radius/rung mapping,
  wheel-handoff threshold logic, inspector content assembly from fixture
  scene documents (the golden ephemeris JSON crosses repos, as in the
  moon tests).
- **Interaction seams (raycast hits, OrbitControls wiring, canvas
  cross-fade) under the Playwright smoke** — the seam headless tests
  proved they cannot see.
- The existing golden byte-identity contract is untouched (no sim-side
  change; `make world-check` unaffected).
