# Retrospective — The Massing

The second campaign of the Orrery view-remake program: a switchable globe
render-style layer (Smooth / Voxel / Terraced / Faceted) plus a client-side
deeper-LOD ceiling. Seven tasks, subagent-driven, orrery-only. Process notes.

## What worked

- **"Undecided" is an argument for a switch, not a coin-flip.** Nathan couldn't
  say which voxel look he wanted. The wrong response is to pick one and hope; the
  right one is to notice that render-style is an axis (orthogonal to the data
  lens, already an axis in the code) and make it *switchable*, so the choice is
  made by living with all four rather than up front. The gallery was cheaper than
  it looked because two of the four styles (Terraced, Faceted) are near-free
  riders on the geometry the keystone (Voxel) required.

- **The ideonomy lift named the keystone and dissolved a sub-campaign.** One pass
  (cross-domain re-instantiation on materiality/cardinality) found that "voxel"
  is defined by the *exposed vertical inter-cell cliff* — remove the walls and
  it's just low-poly — and that a voxel renderer is an *honest* one: it shows one
  block per datum instead of inventing a gradient. That second point collapsed
  the "zoom in deeper" ask into the same campaign: voxel makes coarse resolution
  visible, so it *needs* the deeper LOD to have something to show. Two asks, one
  coin.

- **Cheapest-style-first proved the architecture before the hard part.** Task 1
  shipped Faceted (a one-flag flat-shade) to establish the `GlobeStyle` axis and
  the switch end-to-end; the keystone voxel geometry didn't land until the switch
  was already known-good. When the hard task came, only the geometry was in doubt.

- **The controller ran the visual pass, not a subagent.** Subagents can't see; a
  green geometry test doesn't prove walls render outward under a `FrontSide`
  material. The controller built the client and drove Playwright to screenshot
  each style — then read the PNGs. The topographic-lens voxel shot showed the
  cliff walls unambiguously. The keystone was confirmed by eyes, on the exact
  render path users get, not inferred from a passing assertion.

- **The perf risk was measured, not guessed.** Voxel geometry was flagged as the
  primary execution risk (blocks-with-walls are heavier; The Region had fought
  main-thread rebuild jerk). Task 7 measured it — 15.5 ms peak tile-build,
  zero frames over budget — and therefore changed nothing. No granularity cap, no
  worker offload, because the measurement said none was needed. YAGNI held because
  a number backed it.

- **The stale-worktree-wasm trap did not recur.** Last campaign (The Vantage) a
  fresh orrery worktree inherited a pre-freshwater wasm and 15 tests failed on a
  phantom. This time the worktree was seeded via `npm run wasm:release`
  (sha256-verified v12) at setup, and the baseline was green before Task 1. The
  lesson held because it was applied at setup, not rediscovered at first failure.

## What bit

- **The spec carried a premise that wasn't true.** The design said the new style
  control should be "visible only in the Globe view, like the lens control" — but
  the lens control isn't view-gated at all; it's always visible. The Task 5
  implementer checked, found the premise false, and mirrored the lens's *actual*
  behavior rather than inventing a gating mechanism the codebase doesn't have.
  Correct call. Worth remembering that a spec's "like the existing X" is a claim
  to verify, not a fact.

- **A stale line-number in a task brief.** The Task 5 brief pointed at
  "`hud.ts` ~143 (the lens control's `<select>`)" — line 143 was actually the
  *view* dropdown, and the lens control is a button row, not a select. The
  implementer followed the real code and documented the discrepancy. Line-number
  pointers in briefs rot; name the thing, not the line.

- **A pre-existing `RenderStyle` axis collided with the obvious names.** `onStyle`
  / `setStyle` were already taken by an unrelated post-process axis in the HUD.
  The reviewer's Task-4 note surfaced it before Task 5, so the implementer used
  `onGlobeStyle` / `setGlobeStyle` from the start instead of discovering the
  collision mid-build. A cross-task heads-up from one review saved the next task a
  rename.

## Follow-ups
- **Glint blowout at the new near-zoom limit:** over ocean, the sun-glint
  specular highlight fills the frame at maximum zoom (any style — a consequence of
  the deeper zoom this campaign added). Not a crash; a legibility fix for a later
  pass (attenuate glint with camera proximity).
- **Producer-side finer terrain:** deferred by design — decide once the voxel
  honesty shows whether the *simulation's* resolution (not the client's) is the
  floor worth deepening.
- **Voxel geometry seeds campaign C** (the voxel map): the block/wall builder
  carries over.
- Tallest-exotic-peak camera graze at max zoom (did not reproduce on seed 42);
  animated transitions between styles (v1 rebuilds slots, no crossfade).
