# The Sextant

The Orrery feels slow, and it is. This campaign does not make it faster. It
builds the instrument that says, in numbers, *where* the time goes — and
leaves a ceiling behind that a future regression trips over. A sextant does
not move the ship; it fixes your position, which is the prerequisite for
deciding where to steer.

## What seven hundred milliseconds a tile buys

The client's scene reads were measured on one machine, at seed 42, against a
harness that mirrors the Orrery's real call sequence: build a world, ask for
the six scene documents, then ask for a fan of region patches at the LOD
constants the client actually uses. The small documents — the star system,
the moons, the neighbouring stars, the eclipse windows — cost under three
tenths of a millisecond between them. The globe's tile document costs about a
second to build and half a second to serialize. A region patch, the document
a camera move requests, costs about seven hundred milliseconds.

The interesting number is not the seven hundred. It is that it does not
change. Fans of one, eight, and twenty-four tiles cost 687.3, 700.5, and
701.8 milliseconds **per tile** — flat, to within a percent, over a
twenty-four-fold change in the work requested. Cost that is flat in the size
of the request is the arithmetic signature of a computation that is not being
shared: each call pays in full for something the call before it had already
finished.

What it pays for is the world. Every entry point in the scene window opens
with the same two lines — derive terrain from the world, then derive climate
from the world and that terrain — and neither result is kept. Isolated, those
two derivations cost 543.8 and 94.0 milliseconds: **638 milliseconds of fixed
overhead on every scene call, whatever the call was for.** A flamegraph over
a fan of twelve patches measures shares of the whole process: 61.36% in the
terrain derivation, 9.84% in the climate derivation, and 77.77% in the region
calls themselves, the rest being the one-time world build. Divide the first
two by the third to bring them inside a single call and **91.6% of a region
patch is the planet being rebuilt** — a figure the wall clock reaches
independently, since 638 of 702 milliseconds is 90.9%. Roughly sixty-four
milliseconds is the sampling the client actually asked for; 24.9% of the
self-time is one function, the fractal-noise sample that the terrain
derivation calls several million times per world and then discards.

The consumer's shape turns that from a nuisance into the dominant cost. The
Orrery requests one region patch **per level-of-detail tile**, and a camera
move touching twenty-four tiles therefore costs about seventeen seconds — of
which roughly fifteen and a quarter is the same planet, generated
twenty-four times, and thrown away twenty-four times.

## Two levers, measured and set aside

Both plausible alternatives were measured before anything was proposed, so
neither has to be re-argued later.

The **build-depth ladder** is not the lever. Stopping the pipeline at each
rung costs 1.1 ms (astronomy), 562.1 ms (terrain), 1763.3 ms (settlements),
and 1800.7 ms (full). The Orrery needs climate, which first exists at the
settlements rung, so the deepest legal saving — dropping *full* to
*settlements* — is 37 milliseconds. Set against the 638 milliseconds of
re-derivation a scene call already pays, that is 5.8%: the ladder shaves the
tail off a computation whose defect is that it runs at all, two dozen times
per camera move.

The **size-first wasm optimization level** is a real but secondary lever. The
client is compiled with `opt-level = "z"`, and the same measurement under
that profile natively costs about 23% more (866.4 against 702.0 ms/tile). It
is an honest 23%, an order of magnitude smaller than the rebuild, and it
trades directly against the binary the browser has to download.

## Why nothing caught it

The gate ladder had no instrument pointed here, and the reason is
structural rather than an oversight. The world generator carries a committed
profiler, and it measures the **build**. [The Frame
Budget](./the-frame-budget.md) built a profiling harness driven through a
real headless browser, and it measures the **client** — correctly, in
another repository, where it identified the tile-assembly function and cut
its cost seventeenfold. Neither instrument is wrong and neither could see
this: the 91.6% lives in the seam between them, which is *the producer's cost
under the consumer's call pattern*. Measured on the producer side, one
region patch is simply an expensive pure function. Measured on the client
side, it is time spent behind a WebAssembly call that the harness cannot
open.

The two campaigns turn out to be halves of one shape. The Frame Budget's
finding, lifted a level, was that every client-side hotspot was the
processor recomputing something that had not changed; it fixed that on its
own side of the boundary and named one residue it could not reach — the
high-resolution patches' missing halo, which "lives in the world's
generator, off-limits to a client-only campaign." This campaign is that
sentence read from the other direction. The same diagnosis holds on the
producer side, at ten times the magnitude, and the client-only harness could
not have found it.

## What landed, and the guard that could not

Three things landed, all of them measurement. A committed profiler lives in
the crate that owns the scene APIs, shaped like the Orrery's session rather
than like a list of functions — a distinction that turned out to decide the
whole finding, since redundancy is only visible when calls repeat. A
`#[ignore]`d battery in the full gate holds one falsification ceiling per
client-visible operation, set above the measured values so that only a real
regression trips them, and *per tile* rather than per fan so the ceiling does
not depend on how many patches the fixture happens to request. And the
campaign's own profiler run is recorded in the append-only timing ledger,
which is deliberately not drift-checked: a wall time is machine- and
load-specific, so it belongs in a record you read, never in an artifact the
build compares.

The ceilings carry one discipline that is not in their precedent. A budget
that rises whenever it is hit stops being a guard, and the repository has
the evidence: the settlement-graph battery's wall-time budget was
re-baselined upward several times as the world grew, each raise correct and
documented, but nothing in the mechanism marks a raise as unusual. So each
constant here records its measured value, its date, and its host, and
**lowering is free while raising is an explicit reviewed act** — a review
discipline rather than new machinery.

What did not land is the guard that would actually pin the defect. Nothing
here is slow code: a region patch is a clean pure function of the world, and
deriving terrain from a world is the honest way to get terrain from a world.
The cost is emergent — each entry point opens the same way because its
neighbour did — and a fixture that measures only *time* will keep
rediscovering the symptom. The durable guard is structural: *the scene layer
derives terrain at most once per world.* It is deterministic, cannot flake,
and belongs to the same family as the drift check, the type audit, and the
architecture test.

It cannot be written yet. Every scene entry point takes a world and derives
internally, so "derived once" has no seam to observe against — only a
counter existing solely for a test, or the artifact-taking API that the fix
itself introduces. The guard is therefore specified and deferred to the fix,
which is the first point it can be written honestly. That the fix has a
shape is not in doubt: one directory over, the locale window already holds
it. `LocaleContext` performs exactly the same two derivations exactly once
and every room description reuses it, and its doc comment states the reason
in a sentence — *so a locale stays a cheap derived view.* The scene window
has no equivalent, and that absence is the whole finding.
