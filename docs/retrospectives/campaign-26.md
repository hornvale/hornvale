# Campaign 26 (The Live Orrery) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** None of this campaign's ten task reviews turned up a
finding that restates a prior retrospective's by name — the closest cousin is
Campaign 22's bundle-freshness proof (rebuild from the checked-out source,
rebuild again from a second, differently located copy, diff both against the
committed file), reused here verbatim for `orrery.js` and holding on the first
try, exactly as it did for the atlas. Worth naming as a pattern now proven
twice: a committed client bundle gets the same byte-identity discipline as a
committed raster, and the proof travels unchanged from one client to the next.

**Estimate deltas.** No stage-level estimates were made for this campaign, so
there is nothing to compare against — say so rather than pad.

**Spec vs. reality.** Ten implementation tasks plus this closing one landed
against a plan followed closely enough that no task review found the plan
itself wrong. A short run of small, all-fixed findings surfaced along the way
(scene-builder fallback branches verified unreachable, a stale doc comment, a
star-tint grouping mismatch between the client palette and the terminal
renderer caught and aligned before merge) — none reopened after being closed.
One design question recurred across two tasks and is worth recording because
it was decided once and then correctly left alone: the globe's spin phase has
no sim-emitted day-phase offset to anchor it, so its rotation is a display
convention — an arbitrary seam for the picture, not a claim about which
longitude the simulation calls noon. The task that built the globe flagged
this explicitly for the task that would wire it in, and the wiring respected
the boundary without re-litigating it: the terminator (day and night) is
geometric and always correct; the globe's *particular* meridian facing the
reader at a given instant is not sim data and the book copy says so.

**The two-language golden contract closed the risk before it could open.**
Every prior committed artifact in this book proved itself byte-identical to
its own regeneration — one language, run twice. This campaign put a second
language's arithmetic in that position for the first time: a browser
evaluator computing body positions and phases from the same orbital elements
the simulation's own calendar already evaluates. Two independently correct
implementations can still disagree with each other silently, each internally
consistent and both wrong relative to the other. The fix was the same shape
as every byte-identity check the book already runs, aimed sideways instead of
forward in time: the simulation writes out its own evaluations at a small set
of sampled instants, and the browser's evaluator is checked against those
samples to a tolerance nine orders of magnitude tighter than a pixel could
ever distinguish. It reproduced cleanly across every sampled row on the first
attempt — the contract did its job before a single visual bug had the chance
to prove it necessary.

**Following the atlas's grain made the client nearly free.** Every structural
question this campaign might have had to answer from scratch — how a
second toolchain sits beside the Rust gate without touching it, how a client
splits pure, tested logic from thin DOM glue, how a bundle earns the right to
be committed — had already been answered by the atlas and formalized in
decision 0023. This campaign's scaffold, CI job, and module boundaries
(scene parsing, ephemeris, palette, moon geometry, globe projection, each
independently unit-tested; only the wiring file left untested) are not new
decisions but the same ones applied a second time. The cost of a second
in-repo client turned out to be the cost of writing its logic, not the cost
of inventing its shape.

**Replacing the casts, not duplicating them, kept the gallery honest.** The
two baked terminal recordings did not get a "legacy" corner or a toggle
beside the new page — they were deleted outright, along with the CI lines
that regenerated them, the moment the live client could stand in their
place. The terminal render that produced them stays, because it remains a
legitimate plain-text picture in its own right; only its *committed
recordings* and the book's showcase went. A gallery page that keeps both an
old artifact and its replacement invites the two to drift apart in a reader's
eyes about which one is "real" — deleting the superseded one is what keeps
"the book is merged reality" true rather than aspirational.

**The moon-phase debugging that came before this campaign is what made this
campaign legible.** Two fixes landed on the terminal orrery immediately
before this campaign's spec was written: placing moons by their actual phase
rather than a decoupled clock, and orienting each moon's lit limb toward the
star rather than by a waxing/waning convention. Both were bugs a four-glyph
bucket display could hide — a moon nudged slightly wrong in phase still
rounds to the same glyph most of the time. A continuous, computed terminator
has nowhere to hide that kind of error, which is exactly why the owner asked
for one: the fixed points this campaign needed to pin its golden contract
against were only trustworthy because those two bugs had already been found
and closed on the terminal side first. Debugging the coarse renderer paid
for the fine one's correctness before the fine one existed.
