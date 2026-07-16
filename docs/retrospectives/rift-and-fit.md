# Rift-and-Fit — retrospective

**Completed:** 2026-07-16 (plan `docs/superpowers/plans/2026-07-16-rift-and-fit.md`,
G6-approved, MAP-21)

**The Stage-0 probe-first discipline paid for itself before a single line of
generator code existed.** The spec preregistered an expected ordering for
the three banked rift textures — slivers high, arms moderate, crenulation
small-moderate — and the synthetic-D probe falsified the crenulation leg
outright: it measured co-equal with slivers, not trailing them, because both
mechanisms turn out to be governed by the identical "flip every k-th coastal
cell" shape on opposite sides of the coastline. Building crenulation first
purely because the spec predicted it would be a minor texture would have
been a wasted iteration; measuring first meant the tuning season spent its
budget on the mechanism that actually had leverage, and the false prediction
itself became a real finding about *why* the two mechanisms converge rather
than an embarrassment to bury. This is the same falsification-driven pattern
Sculpting's and Crust's retrospectives both named — three campaigns running
the identical discipline is no longer a coincidence, it is how this project
tunes.

**The perf-budget stop-then-recover ladder worked exactly as designed, and
was worth the ceremony.** The unguarded conjugate clip landed at 2.7× the
per-world budget — a genuine hard stop, not a soft warning — and three
successive, individually-authorized recovery levers (an exact-zero envelope
gate, a provably bit-exact saturation shortcut, and a separately-ruled
redundant-pass deletion) brought it to net *faster* than baseline while
keeping every step byte-identical and every number in the record rather than
letting the final "it's fine now" absorb the intermediate failure silently.
Nathan's explicit ruling to record both the net ship-time effect (negative)
and the rift's own marginal cost (positive, ~7%) rather than let one number
mask the other is worth keeping as the template: a recovered budget is not
license to stop measuring the thing that regressed it.

**The controller finished an epoch commit inline after three consecutive
server failures killed the implementer mid-gate, and that was the right
call, not a shortcut.** By the third 529 the working tree was fully prepared
— every perf lever landed, every re-pin done, nothing left but running the
gate and committing — and a fourth dispatch would have paid a full
re-briefing cost to do work that had no remaining judgment calls in it.
Reserving inline controller action for exactly this shape (mechanical,
well-defined, no decisions left) rather than for anything merely
inconvenient is the distinction that keeps this a narrow exception and not a
habit.

**The tuning season's iteration-by-iteration cadence survived a genuinely
open-ended stop condition.** Unlike a season tuning toward a single known
target, this one carried two open bands with a discovered antagonism between
them (the lever that recovers shelf-fraction smooths away crenulation's
shoreline gain), plus a preregistered escalation to Nathan reserved for
exactly the outcome that happened. Driving it one measured iteration at a
time — baseline, then crenulation, then a shelf-recovery lever plus a
crenulation-ceiling probe — rather than committing to a fixed iteration
count up front is what let the season stop the moment both movers landed in
an acceptable state instead of either overshooting into an unmeasured
regression or under-tuning out of caution.

**The gap between "acceptance floor" and "hard-stop stayer" was not drawn
sharply enough at plan time, and it cost a wasted gate reading.** The
iteration-0 tuning baseline flagged shelf-fraction's regression as though it
were the same category of failure as a broken stayer band, before the
controller ruling reclassified it correctly as a *mover* the season itself
was supposed to address. The plan's own language already distinguished
movers from stayers for the two bands under active tuning, but that
distinction was not carried far enough to cover a band the campaign
regressed *incidentally* (shelf-fraction was never a target of this
campaign's work — it broke as a side effect of the fit clip) rather than one
it opened by design. A plan that tunes toward two named targets should say,
explicitly, what happens to every other previously-closed band the new
machinery might disturb, not leave that classification to be inferred
mid-season.

**The tuning gate does not run the probe that would have caught the
shelf-fraction regression before it shipped in the epoch commit.** The v4
fit clip regressed shelf-fraction out of its band the moment it landed
(Task 6), and nobody knew until Task 10's tuning baseline measured it four
tasks later — the epoch commit's own gate is unit/integration tests and
determinism batteries, not the 100-seed shape-metric probe, so a real,
measurable band regression rode inside a merged commit for the length of a
whole stage before it was caught. It was caught in time, inside the same
campaign, with no harm done — but that is closer to luck than to a
guarantee. A probe-backed band is a real acceptance criterion for a
generator change and deserves a cheaper, faster-turnaround check at the
commit that can break it, not only at the tuning task built to fix bands on
purpose.

## Estimate vs reality

The nine build tasks (probe → assembly frame → fracture network → conjugate
clip → wiring/perf → facts → the roughness-slope metric) tracked the plan's
sequencing without reordering, and the two tasks that hit real trouble —
Task 6's perf stop and Task 10's tuning season — were exactly the two the
plan had already flagged as hard-stop-shaped before execution began, which
is the second campaign running to name that as the accurate way to budget an
epoch: decorate the plan with explicit stop-shaped tasks for exactly the
places a generator change is expected to fight back (a perf-sensitive new
field pass, a metric known to be adversarial), and treat every other task as
close to its estimate, because on this campaign, as on Sculpting before it,
that is exactly what happened. The census-fork decision (whether to run the
v3 canonical regen before or after this campaign) and the ambient-belief
extinction investigation it surfaced both ran as fully parallel threads
without blocking the campaign's own measurement window — a second data
point that a campaign's tuning season and an unrelated census/investigation
thread can share a worktree's ledger without either one waiting on the
other, as long as neither touches the physics the other is measuring.
