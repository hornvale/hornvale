# Retrospective — The Frame Budget

A performance campaign: fix the Orrery's zoom jerkiness, then sweep the client's
interactions for further wins. Orrery-only; no sim change, no release. Process
lessons only.

## What worked

- **The render-independent metric caught a fix that lied — at the close, not
  before it.** The campaign's own headline number (a flat, wonderful frame time)
  was a bug: an on-settle gate that suppressed all refinement under a running
  clock, so the globe did zero tile work and looked instantly fast. The frame-gap
  loved it. The render-INDEPENDENT counter — tile-build call-count and JS ms, not
  frame time — read **zero**, and a zero-work "win" is a contradiction, not a
  triumph. That contradiction, plus a camera-trajectory probe showing the camera
  "moving" while dead still, exposed the bug. **A performance metric must isolate
  the thing you can change (JS) from the thing you can't (a headless software
  renderer); when the two disagree, the render-independent one is the truth, and a
  frame time that improves while the counter says nothing ran is the tell.**

- **The bug was caught during the close, because the close re-ran the measurement
  instead of trusting the recorded table.** The before/after table was already
  written and the DoD prose drafted. Re-deriving the number from scratch — rather
  than citing the committed baseline — is what surfaced that it wasn't
  reproducible. The "measure, don't narrate" habit applied to one's *own* prior
  measurement.

- **The honest metric survived; only the framing was wrong.** Once refinement
  actually ran, the same harness gave a real, defensible number on the same JS
  lever: tile-build main-thread work fell ~17× (about fourteen seconds to under
  one over the scripted zoom), full rebuilds became incremental applies, and
  region arrivals became single-tile swaps. The engineering was always sound; it
  was the *measurement of it* that needed the correction.

- **Analytic normals as the enabler, sequenced first.** Deleting the global
  cross-tile normal stitch before the incremental diff was the right order: with
  normals a pure function of position, a single tile builds in isolation, which is
  what makes the diff simple. Sequencing by *enablement*, not by a scale's depth
  number.

- **The whole-branch review earned its keep, and the fixture footgun bit
  twice.** The final review caught that analytic normals — correct for base
  tiles — reintroduced a shading seam across region patches (bounded fields
  whose edge probe is one-sided). Confirming it took a second lesson: the first
  test used a coarse sample count, at which the region cell dwarfs the 0.2°
  normal probe, the probe rounds back to its own node, and every region normal
  comes out degenerately flat — so the patches agreed and the seam vanished from
  the test. Only at the *production* sample count did the seam appear. **A
  fixture that doesn't match the production value coverage can hide the very
  defect under test** — the same trap the signed-zero keystone taught, met again.
  The fix (a `stitchNormals` scoped to the mounted region patches, not the whole
  globe) keeps the campaign's win and restores seam-freeness; both tests now pin
  the production sample count explicitly.

## What was hard / notes

- **The unit test that guarded the fix was vacuous against the real regression.**
  The single-tile-swap test settled the camera by driving three updates at a
  *frozen* day — so the world didn't spin, and the spun-frame settle bug never
  showed. The regression only appears with a *running clock*. Lesson: when a gate
  depends on time advancing, the test must advance time; a fixed-day test of a
  time-dependent gate proves nothing about the live path. The new regression test
  drives a stationary camera over an advancing day and fails on the old gate.

- **Headless software-GL is a persistent confound — and it can invert.** The
  corrected build posts a *worse* headless frame time than the broken one, purely
  because it now draws far more fine detail. Frame-gap is not a cross-version
  metric here; only the render-independent JS counter and the flamegraph's JS
  share are. The controller's visual pass on real hardware is the smoothness
  judge, and that judgement was still owed at close (the headless harness cannot
  render it).

- **A restore step clobbered an uncommitted fix.** Measuring the baseline meant
  checking the old implementation back into the working tree, which overwrote the
  not-yet-committed settle fix (its scratch backup already deleted). Recovered by
  re-applying the two edits from the diff. Lesson: commit a fix before using
  `git checkout <ref> -- <file>` on the same file to A/B against an older revision.

- **The visual pass has to look at the whole lit globe, not a corner.** This
  campaign's own opening move — analytic normals — shipped a rendering regression
  that turned the entire lit hemisphere into black speckle, and it passed every
  visual check the campaign ran, because those checks asked one narrow question
  ("is the tile boundary seam-free?") and answered it on a cropped seam detail.
  A defect invisible in a boundary close-up and glaring on the full illuminated
  planet is exactly the class a boundary-focused pass misses. Before calling a
  render "seen," look at the whole thing, lit.

- **A diagnostic that fails silently blocks everything downstream.** The
  perf-harness — a no-assertion profiling tool — was committed into the
  deploy-gating e2e set with a machine-specific output path; on CI it threw,
  failed the gate, and silently blocked *every* deploy for the campaign's whole
  length. The live site was stale the entire time, and every "fixed" push reached
  no one — the reason real-hardware feedback kept saying "no change." Two rules
  fell out: keep diagnostics OUT of the deploy gate (tag + exclude them), and make
  "which build am I looking at" answerable without trust — a build stamp (build
  time + commit, logged on boot) turns staleness into a glance.

- **Cutting the total is not cutting the spike.** The 17× reduction in
  main-thread work still left visible jerk, because the eye integrates the worst
  *frame*, not the *sum*. Amortizing each change's work across frames — a number
  the headline metric never showed — was what actually made it smooth. When a
  perf number improves but the feel does not, you are measuring the wrong
  aggregate.

## Follow-ups

- **Region-patch normal halo (the proper seam cure)** — ship each streamed
  region patch with a one-row halo of its neighbour's elevation, so the analytic
  normal at a patch edge reads real slope and adjacent patches agree by
  construction (retiring the scoped stitch this campaign kept as the client-only
  stopgap). It lives in the world generator, so it needs a producer change and a
  wasm release — a future campaign, not this one.
- **Real-hardware visual pass** — *done, and it was decisive.* The headless
  harness proved the JS win but could not render smoothness; the real-GPU pass
  found both the per-change hitch (→ amortized build) and the analytic-normals
  shading regression (→ bilinear elevation). It also exposed the silent deploy
  block. The lesson stands as the first note above: a client-render campaign is
  not done until it has been *seen* on real hardware, whole and lit.
- **Draw-call count** — the one real-hardware lever the headless harness cannot
  measure (many per-tile meshes → many draw calls). Reducing it fights the
  per-tile-mesh architecture the incremental diff depends on, so it was left
  un-chased; revisit only if real-hardware profiling shows the render path is the
  wall.
- **Buffer/typed-array reuse** (a planned rung) was skipped: profiles showed GC
  negligible after the diff made rebuilds rare — effort with no measurable payoff.
