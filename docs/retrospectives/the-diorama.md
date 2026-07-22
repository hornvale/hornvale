# Retrospective — The Diorama

The third campaign of the Orrery view-remake program: a Voxel-2.5D style for the
flat map rung, reusing The Massing's voxel core on a flat grid under a fixed
isometric camera. Four tasks, subagent-driven, orrery-only. Process notes.

## What worked

- **Reuse via low-level helpers, not forced parameterization.** Task 1 needed The
  Massing's block/wall logic on a flat grid. The implementer extracted two
  genuinely geometry-agnostic helpers (`makeVertexWriter`, `pushFlatQuad`),
  refactored the globe's wall emission onto them (verified byte-identical by the
  existing suite), and built the planar builder on the same helpers — but
  deliberately did NOT unify the top-face pass, because the sphere's top normal
  (corner-unit-average) and the plane's (constant +Y) genuinely differ. The right
  DRY line is the piece that's *actually* shared, not the largest piece you can
  force through one signature.

- **The scope-parametric insight paid for itself.** Framing the voxel renderer as
  "one builder, two projections (sphere / plane)" made the whole campaign a small
  delta on The Massing rather than a new renderer — and the flat case turned out
  simpler than the curved one. Naming the shared structure up front is what let
  the reuse be a helper extraction instead of a rewrite.

- **The visual pass caught a flat diorama and fixed it.** The geometry was correct
  and every test green, but the first screenshot showed a nearly-flat tilted plane:
  at the inherited relief exaggeration a 1000 m peak rose ~0.5% of the region
  width. The fix was a controller-tuned height scale (120→800), confirmed by a
  second screenshot — the terraces and cliffs then stood up. A passing test cannot
  see "reads as flat"; only eyes on the rendered frame can. The controller owning
  the visual pass (build, screenshot, read the PNG, tune, re-shoot) is what closed
  the gap.

## What bit

- **`#view=map` is not URL-addressable — the first visual pass shot the wrong
  view.** The Vantage kept Map out of the URL; a `#seed=42&view=map` deep link
  silently lands on System. The first screenshot captured the orbital view with
  the map-style dropdown uselessly set. The map is only reachable via the
  `.hud-view` dropdown (Globe first to set a center, then Map). This bit the visual
  pass and is a real constraint the e2e had to honor too — worth remembering for
  every future map-facing test.

- **Task briefs generated into the wrong scratch directory.** Briefs 2–4 were
  written to the *main checkout's* `.superpowers/sdd/`, not the campaign worktree's
  — so a dispatched implementer couldn't find its brief at the path the prompt
  named and had to source the task from the plan instead. It adapted, but the
  handoff should point at a file that exists. Generate briefs into the worktree the
  subagent runs in (or copy them there before dispatch).

- **A three-way naming collision, pre-empted.** The HUD already had two style axes
  — the post-process `RenderStyle` and the globe's `GlobeStyle`. The new `MapStyle`
  made three. Because The Massing's retro had already flagged the `RenderStyle`
  collision, the plan named `onMapStyle`/`setMapStyle`/`hud-map-style` distinctly
  from the start, and every new interface member carries a doc comment enumerating
  all three axes. Prior-campaign lessons, carried into the plan, kept the third
  axis from repeating the second's stumble.

## Follow-ups
- Camera orbit / adjustable pitch (v1 is fixed isometric).
- 3D diorama props for the 2D map symbols (peaks / forests / waves) under voxel.
- The pixel-art map quality rework — the program's final campaign (D).
- Minor cleanups triaged at the whole-branch review (dispose() light lifecycle,
  frustum-value test coverage, the per-cell `emit` closure hoist).
