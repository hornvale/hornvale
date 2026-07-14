# Lab Performance

**July 2026 · outcome: shipped in the middle, trimmed at the edges — worlds
now build only as deep as the question asked of them**

## What was attempted

Between Crust and Sculpting sat a scheduling argument: Sculpting would be
another terrain epoch, another season of tuning runs and refrozen fixtures,
and every one of those runs would pay the same tax — a study that measures
seven coastline metrics still built the whole world, language families,
pantheons, deep time and all. The campaign proposed to measure first and cut
second, in four independent tracks: a committed build profiler to establish
where the time actually goes; a view-typed *build-depth ladder* so
a study builds each world only to the rung its metrics read; a tiering of
the continuous-integration drift regeneration; and two free-riding
correctness batteries over worlds a census already pays for.

## What the profiler said

The measurement track earned its place on the first day. Astronomy — a full
star system, orbits, calendar — costs literally nothing measurable: 0.0% of
a build. Terrain is 24.5%. Climate together with settlements is 49.5%, and
deep time another 26.0% — so a terrain-depth build skips three quarters of
the bill, and an astronomy-depth build skips essentially all of it. Two
planned optimizations died honorably by these numbers: collapsing a
triple-read in the crust code would have bought about three percent of a
build in exchange for restructuring frozen, byte-identity-critical code,
and splitting astronomy off the ladder would have optimized a stage that
was already free. Both were dropped with the measurements recorded in the
plan, which is the profiler doing exactly the job it was built for —
including telling the campaign *no*.

## What landed

**The depth ladder.** `build_world_to(seed, …, depth)` stops the generation
pipeline at a named rung — astronomy, terrain, climate, settlements, full —
and at any rung the committed ledger is a byte-identical *prefix* of the
full build's: stopping early omits later appends and changes nothing it
does commit. Above it, every one of the lab's hundred and ten metrics is
tagged with the rung it reads, each extractor receives the narrowest view
type that rung affords — so under-building is a compile error, not a wrong
number — and the study runner builds each seed's world only to the deepest
rung any selected metric requires. A metamorphic guard re-runs depth-scoped
studies against forced-full builds and demands byte-identical output. The
win lands where the seeds are: a ten-thousand-seed sky census now builds
ten thousand nearly-free worlds instead of ten thousand full ones, and
terrain censuses run about four times cheaper. Studies that select every
metric still honestly build full worlds — the ladder speeds up focused
questions, not all questions.

**The enumerated corner, twice over.** The forty-eight-combination pin
product — every discrete sky, rotation, neighbor and supercontinent pin,
each built twice and byte-compared — had grown from its original four
seconds to over eight minutes as the pipeline deepened. One scoped thread
per combination brought it to about a minute; building only to the terrain
rung (the enumerated pins never reach past it) brought it to sixteen
seconds, with the determinism guarantee unweakened.

## What was trimmed, and why

The campaign's outer tracks were overtaken by decisions that landed while
its middle was shipping. The tiering of the per-push drift regeneration
targeted a per-push CI that no longer exists — the project made GitHub
Actions manual-only once it was clear the free runners were slower than the
development machine, and a manually triggered run is already an explicit
choice to pay full price. The correctness batteries were designed to ride
worlds a local census build already pays for, and census generation has
since moved wholesale to a remote box; the batteries are still wanted, but
against the new shape, and wait in the idea registry rather than being
built stale. Each verdict is recorded in the plan beside the stage it
retires.

The close itself arrived late: the code merged and the campaign then sat
open for a day — no chronicle, no retrospective, its branch idling — while
five other campaigns landed around it. The janitorial pass that produced
this page is recorded in the retrospective as its own lesson.
