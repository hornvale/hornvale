# The Frame Budget

Zooming the globe had grown to a crawl. Since the level-of-detail work landed,
pushing the camera toward the surface froze the Orrery for a second at a time —
the planet lurching between frames rather than gliding. This campaign is a
performance one, no new feature at all: make the globe fast, and prove it with
numbers. The proving turned out to be the hard part, and the honest story is as
much about a measurement that lied as about the code that got faster.

## What the flamegraph proved

The discipline was to measure before touching anything. A profiling harness — a
scripted zoom driven through a real headless browser, its CPU sampled over the
Chrome debug protocol, alongside a counter that isolates the JavaScript cost from
the rendering — turned a vague "it's janky" into a precise indictment. The culprit
was `buildTiles`, the function that assembles the visible tiles' geometry. It was
a **full rebuild**: it disposed every tile's mesh and rebuilt every one from
scratch, and — worst — reconciled the surface normals across *every vertex of
every tile* through a map keyed by a freshly-allocated string of each vertex's
coordinates. And it ran again and again as the set of visible tiles shifted and as
each streamed patch of higher-resolution terrain arrived. Fourteen seconds of
main-thread work across a single zoom; the normal-stitching alone was a third of
each rebuild.

Lifting the shape of the problem one level: every hotspot was the processor
*recomputing, again and again,* something that had not changed. The fix is the
negation of that — compute once, reuse; or never recompute at all.

## Two moves

The first move removes the stitching entirely. It existed because each tile
computed its normals from its own triangles alone, so a shared edge got a
different normal on each side and the light drew a seam — hence the after-the-fact
reconciliation across all tiles. Replace it with **analytic normals**: each
vertex's normal comes from the *slope of the terrain itself*, sampled as a pure
function of latitude and longitude. Two tiles meeting at an edge evaluate the same
function at the same coordinate, so they agree by construction — no reconciliation,
and, crucially, a single tile can now be built in isolation.

That construction holds for the base tiles, which sample one global field. It does
*not* hold for the streamed high-resolution patches: each patch is a bounded grid,
and asked for the slope just past its own edge it has no neighbour's data — it
answers with its edge value, a flat one-sided normal, while the patch on the other
side answers with its real interior slope. The two disagree and the light draws a
crease exactly where the deep-zoom detail is densest. (The whole-branch review
caught this; it surfaces only at the patches' true sample resolution, and a first
test at a coarser one had hidden it behind degenerately-flat normals.) The honest
cure would ship each patch with a one-row halo of its neighbour's data, but that
lives in the world's generator, off-limits to a client-only campaign. So one small
reconciliation survives — the old stitch, scoped now to the mere handful of
high-resolution patches on screen rather than every tile on the globe. The global
cost is gone; a bounded, cheap one remains where the geometry genuinely needs it.

The second move removes the redundancy. Instead of rebuilding every tile when the
visible set changes, **diff** the new set against the old: dispose only the tiles
that left, build only the few that split or merged in, leave the rest untouched. A
streamed terrain patch swaps its one tile in place. Two guards keep it honest — a
hysteresis so a jitter at a split boundary cannot thrash the set, and a rule that
holds coarse detail during a fast fling and sharpens once the motion stops.

## The measurement that lied

That last guard is where the campaign nearly shipped a falsehood. With the diff in
place, the harness reported the zoom running at a flat, wonderful frame time and
the render-independent counter reading **zero** tile work — apparently a total
victory. It was a bug. The settle rule measured the camera's motion in the globe's
*spinning* reference frame; but the world turns every frame under a running clock,
so by that measure the camera was never still, the rule never released, and the
globe **never refined** past its coarsest tiles or streamed a single patch while
time played. The beautiful frame time was the detail being *suppressed*, not
accelerated.

It was caught the way the discipline promises. The render-independent counter read
zero while the frame time looked triumphant — a contradiction that had to mean the
work simply wasn't running. A trajectory probe confirmed it: the camera sat dead
still and the code still called it "moving." The settle test was corrected to key
off the *user's* camera pose, in world space — what a fling actually moves, and
still the instant they stop — leaving the spinning frame for choosing *which*
tiles to show. A regression test now pins it: a stationary camera over a turning
world must still sharpen, and it does.

## The numbers, honestly

With refinement genuinely running again, the same harness measured both the old
code and the new. The honest lever is the **main-thread JavaScript** the tile
work costs, because the headless browser renders in software and its frame times
are dominated by drawing, not by logic — so much so that the *corrected* build
posts a worse frame time than the old one, simply because it now draws far more
fine detail. Read past that to the work itself: over the scripted zoom, the tile
geometry cost fell from about **fourteen seconds of main-thread time to under
one** — four hundred milliseconds per full rebuild became thirty-five per
incremental apply, and a streamed patch that used to rebuild the whole globe now
swaps one tile. On real hardware, where the drawing is free, that seventeenfold
cut in blocking is the glide.

## Where the sweep stopped

With the zoom fixed, the same harness was turned on the globe's other
interactions — scrubbing the day, switching the map lens, the first boot. It found
nothing to fix: each spent under two percent of its time in JavaScript, the rest
being the renderer, which a real graphics card dispatches in a blink. The zoom's
rebuild had been the *only* processor-bound hotspot in the client. So the campaign
stopped where the measurements flattened — not at a quota of fixes, but at the
honest floor where there was no more main-thread work left to win.

## What only real hardware showed

The headless floor was not the end. It could not be: a headless software
renderer measures the wrong thing, and the two things it could not see were both
waiting on a real screen.

The first was that *no dropped-JS is not the same as no jerk*. On real hardware
the zoom was still visibly stepping — because cutting the *total* work had not cut
the *per-change* spike, and the eye sees the spike, not the sum. Each level-of-
detail change still built its whole batch of new tiles in a single frame: at
worst, thirty-six tiles in sixty-nine milliseconds — four dropped frames in one
lurch. The cure was to stop doing it all at once. A change now only *enqueues* the
tiles it wants; a few are built each frame under a time budget, so a big refine
sharpens progressively over about ten frames instead of freezing one, and the
coarse tiles it replaces are held on screen until their finer replacements exist,
so no hole ever opens. The worst single frame fell from sixty-nine milliseconds to
seven. The glide was real this time.

The second was hiding in plain sight, and it took a genuinely embarrassing detour
to find: the globe had been broken for weeks and no one had seen it, because *the
site had never been redeploying*. A profiling harness committed early in the
campaign wrote its output to a hard-coded path that existed only on the author's
machine; on the deploy server it threw, which failed the end-to-end gate, which
silently blocked every deploy. Every fix had been pushed to a repository that
never reached the live page. Once a build stamp was added — the build time and
commit printed to the console on boot — the staleness was obvious in a glance, the
deploy gate was fixed, and the real globe finally appeared on a real screen: a
planet riddled with black speckle. Bisection was merciless. The **analytic
normals** — this campaign's own first move, the one that passed every synthetic
unit test — were the culprit. On the real world's elevation, sampled in hard steps
at the data grid and then exaggerated sixty-fold, the finite difference the normal
is built from spikes at every cell boundary into a near-vertical wall, tilting the
normal to grazing: black where the light should catch, bright fragments where it
should not. The synthetic fixtures — a flat patch, a gentle slope — had never
exercised a real stepped field. The fix restored what the test fixtures had
promised without a stitch: sample the elevation *bilinearly* so the field, and the
gradient the normal is taken from, are continuous. Correct normals, smoother
relief, and — because two tiles still evaluate the same continuous function at a
shared edge — still no cross-tile reconciliation. A performance campaign had shipped
a rendering regression as its opening move and only its own real-hardware pass, at
the very end, caught it.
