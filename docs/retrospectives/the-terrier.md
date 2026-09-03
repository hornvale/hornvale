# The Terrier — retrospective

Process, not product. The chronicle carries what was built.

## A cost read off a two-point difference is the whole derivation, not one step

The Rack measured a chamber snapshot at 8.4 ms after `look` (the sighting
memo already held an answer) and 16.3–16.8 ms after `map` or a chamber `go`
(it did not, and derived one). The difference, ~8 ms, was written into a
chronicle, a retrospective, and a registry row as "one shadowcast" — the
step the two readings' shared derivation happens to be *named* for, not the
step that actually cost the money. A campaign was then opened on that row.

The lesson generalises past this one row: **subtracting two timings never
isolates a step, only a derivation.** A derivation of five, ten, or twenty
steps produces exactly one number from that subtraction, and whoever writes
it down will reach for whatever noun the derivation is colloquially called —
here, "the shadowcast", because a sighting *is* a shadowcast in every other
sentence this project writes about it. Nothing about the arithmetic
distinguishes "the shadowcast costs 8 ms" from "the thing this derivation
is for costs 8 ms and eleven other steps ride along for free." Both readings
fit the same two numbers equally well, and only one of them is checkable
without more measurement — which is exactly why it went unchecked for five
weeks and travelled through four committed documents.

**The check that would have caught it costs less than the mistake did.**
`SIGHT_RADIUS` is 4, so the shadowcast visits at most 81 cells; a plausible
per-cell cost of even a few microseconds bounds the whole operation to
fractions of a millisecond by arithmetic alone, before a single `Instant` is
placed. Ten scratch timing prints — inside `derive_sighting`, `chamber_plan`,
`describe_chamber_here`, `enter`, and `brief_of` — took perhaps ten minutes
and found the real cost sitting in `brief::brief_of`, which was
reconstructing the world's *entire* occupation register from the ledger on
every call, two to five times a turn. That function's own doc had prescribed
the fix — hoist the map, do not memoize inside it — since the day it was
written, five weeks before this campaign opened.

The rule this earns a registry row for
([[PROC-a-two-point-difference-names-a-step-not-a-cost]] in the frontier's
idea registry): before a cost derived from a difference carries a step's name
into a committed artifact, decompose the derivation; when a campaign is
opened on a row that was itself a measurement, re-measure the split before
designing anything. This campaign's own opening ruling (ledger entry #1) did
exactly that, and the redirection it produced — from "make the shadowcast
cheaper" to "stop rebuilding a whole-world register per call" — is the entire
reason the fix that shipped moved milliseconds instead of microseconds.

*Task 5 completes this retrospective with the execution-phase findings
(Tasks 1–3's rulings, already recorded in
`docs/superpowers/ledgers/2026-09-03-the-terrier.md`) and adds this file's
line to `docs/retrospectives/README.md`.*
