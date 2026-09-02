# The Pawl — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-pawl.md); the rulings are in
[the campaign ledger](../superpowers/ledgers/2026-09-01-the-pawl.md); the
measurements are in the spec's §11 and §12.

## Three witnesses reported zero, and none of them had looked

The campaign's design carried six decision rules, each written as a branch
table: run a witness, record which branch fired, act. **Three separate
witnesses reported a comfortable zero before anyone noticed they had also
observed nothing at all**, and all three were caught by the same repair —
count the population the assertion is *about*, and put a floor under the
count.

- **Rule 1** asked whether any drive read runs at an instant earlier than a
  reset of the same creature. First draft: zero offenders. It had also made
  zero lookups, because on the seed it ran, the reads' only production caller
  requires a second creature in the room and the flagship stood alone. With a
  denominator, the answer inverted: six of eighteen reads at past visit days
  hit exactly that divergence.
- **The headline fold-equals-scan sweep** was asserting `0.0 == 0.0` on three
  quarters of its probes — an unfiltered lookup short-circuited every earlier
  instant — and its anti-vacuity guard counted *segments*, which the
  short-circuit also recorded as zero. The guard the vacuous case satisfies is
  worse than no guard, because it reads as coverage.
- **Rule 6** used the spec's own proxy for "past" — an instant before the
  ledger's last committed day. World history predates a possession by about
  seventy-two billion ticks, so 575 of 575 production calls qualified. The
  second counter, an instant strictly before the read's own session day,
  discriminated: 0 of 575 on the live shapes, 9 of 9 on the replay shape.

The generalisable form is not "write better witnesses". It is that **a
zero-valued result and an unexecuted measurement are the same output**, and
only a denominator distinguishes them. Two of these three landed within an
hour of each other, in tasks that had each read the other's report.

## A doc comment answered its author's question, and two tasks believed it

A comment in the fear path said the flagship seed's world has no emitters.
Two consecutive tasks cited it, built conclusions on it, and wrote it into
their own new documentation — four doc sites in the end, two of them added in
the same change that was supposed to be verifying it.

The third task measured it. **Twenty of seventy scans on that very session find
an emitter.** What the seed actually lacks is far narrower: no visited room's
halo ever holds an emitter *at that room's last-visit day*, a condition only
two of sixty-four seeds reach at all.

Every conclusion drawn from the wrong premise survived — the second identity
witness was still required, the seed-42 hash was still blind to that path —
but the mechanism cited for all of them was false, and it propagated because
nobody re-derived it. A comment is an answer to the question its author was
holding; it is evidence about that question, not about yours.

## "Vacuously satisfied" was the finding, and the review said it out loud

The design specified the thirst accumulator's state precisely: a running
integral, a last sighting, a segment start, a last reset, **and** the
checkpoint list. It also wrote a purity clause constraining that read-side
accumulator. What shipped was the checkpoint list alone, re-integrating from
the last reset on every read — and the review recorded that the purity clause
was *vacuously satisfied*, because there was no read-side accumulator to be
impure. The controller approved it.

The phrase was accurate and it was the alarm. A clause that constrains a thing
which does not exist is satisfied by its absence, and the correct reading of
"vacuously satisfied" is **"the object this clause is about was not built"**,
never "this clause passes". It cost the campaign a full readout: five of six
criteria came back not met, and the mechanism behind four of them was the
missing accumulator.

## The cost witness measured the population the fix did not need

Stage 1's own cost witness reported a ratio of 0.889 — the busiest **drinker**,
whose segments per turn fell 1391 to 1238. That is a real measurement of a real
creature, and it is the population for whom the incomplete implementation was
already adequate: a creature that resets often has a short interval to
re-integrate.

The population the fix mattered for is the one that *never* resets, for whom
"the last reset" is genesis. Twenty-one of the fifty creatures on the
instrument are in that state, including the probe the decisive column follows —
selected, by construction, as the creature with the most postings, which is the
one that never arrives anywhere. Nothing measured it until the readout could
not explain itself, and the repair's witness went red on the pre-fix tree
(2,458 of 2,504 reads unbounded) before it went green on the fix.

**Name the population a fix is for, and measure that one.** A witness on a
neighbouring population is not a weaker version of the right witness; it is a
different measurement that happens to be green.

## Four inferences were corrected by measuring them, in three consecutive tasks

- Task 2 corrected a count: 39 construction sites, not the 36 two successive
  resolutions had counted from memory.
- Task 4's rule-6 witness inferred one fold's call counts from another's
  ("same site, same instant"), which was false at two public entry points and
  two walk-path calls; measured, the two differ (520 against 568 on the
  session then, and by a factor of 56 after the roster changed).
- Task 5 corrected the emitter premise above.
- Task 3 corrected a **signature**: the brief passed the accumulator and let it
  derive its own reset, on the plan-text assumption that the reset is committed
  before it is read. It is not — the walk updates it mid-tick as it emits the
  drink and eat facts — so the reset is passed explicitly, and deriving it from
  the committed ledger would have changed behaviour for the rest of that tick.
  A fourth assumption, refuted the same way as the other three.

Every one of those inferences began in **plan text** — a sentence written by
the controller that read as a fact and was never marked as an assumption. The
implementers found all four, each time by running something. The cheapest
available control on plan text is to write the command that would establish
each claim beside it, and this campaign did not.

## Absorbing main twice, and what a clean merge hid

The branch absorbed 202 commits, then 59 more, then 47 at the close. Three
sibling campaigns landed inside that window, and only one of the three
absorptions produced a textual conflict at all — in a generated aggregate,
resolved by regenerating rather than by text-merging.

**Both readouts predate all three absorptions**, and nothing in the
measurement says so on its own. The numbers were taken on the campaign tree at
`57e30acf9`; 308 commits of other people's work landed after the last one was
recorded. The per-call fold costs are properties of the read; **the level and
the whole-tick history share are pre-absorption numbers**, and neither was
re-measured against what lands. That is a scheduling consequence of measuring
at the stage the readout belongs to and absorbing at the close, and the honest
repair is to say it rather than to re-run a six-hour instrument at the merge.

What the clean merges hid was semantic. One sibling changed which creatures a
session's roster holds, and the flagship seed's creature now condenses onto
fresh water, so the residents around it commit no positional facts. That
silently emptied four of this campaign's own witnesses. **All four failed their
own floor assertions rather than passing on an empty set** — which is the
entire argument for the section above, arriving as a dividend rather than as a
lesson.

**The roster change also made the campaign's own witnesses expensive**, and
two rulings came out of that. First, every campaign witness was required to
cost under sixty seconds at the close: four were running at 306-341 seconds
because the flagship seed now ticks 68 residents, and shortening a script is
free only if every denominator stays non-zero and every floor is re-measured on
the short script — which it was, six witnesses, not the four the ledger had
predicted. Second, the fear-fold cost witness was shortened *and its
denominator was named*: at ten of forty turns it cost 863 seconds and failed
on a total that had grown 2.224x, while segments **per past-day replay** moved
only 0.671 to 0.714. The fold's own integration work is flat; what grows is how
many rooms a longer walk leaves an emitter remembering. So the assertion moved
onto the quotient, and the total kept a guard that discriminates — it must grow
strictly slower than the history it would otherwise have walked, which a return
to a whole-history read could not satisfy.

Two more things worth carrying:

- **The identity witnesses were re-recorded main-first.** A checkout of main
  carrying none of the campaign's code was given the same scripts, and the
  merged tree was required to reproduce main's numbers exactly. Taking the
  merged tree's numbers first and then judging whether they looked right would
  have proved nothing about which campaign moved them.
- **A merge message asserted a number the artifact did not carry.** The second
  absorption's message said a type-audit count went 467 to 469; the regenerated
  artifact reads 478, which is neither parent's value — correct, because a
  regenerated aggregate should match neither side. The prose was wrong over a
  right file, and it was caught at review rather than at write.

## The hash constants were a campaign-time instrument, and saying so was the fix

Proving that a migration moves no byte could not use the project's ordinary
drift check, which is structurally blind here — no committed artifact carries a
ticked session ledger. The stand-in was a hash of a whole scripted walk,
asserted against a committed constant and shown to work by mutating a fold.

That is exactly right *during* the migration and exactly wrong after it. The
constant equals the whole walk's behaviour on one seed, so any campaign that
legitimately changes creature behaviour reddens it — a sibling moved all three
without touching this campaign's code — and it is an unwinnable race against a
queue that gates the branch merged with main rather than the branch alone.

The general shape: **an instrument built to compare "before" against "after" has
no meaning once "before" is gone**, and leaving it in place converts a
campaign's own scaffolding into everyone else's standing tax. Retiring it is
not a loss of rigour if what replaces it is a property rather than a value —
determinism across two fresh runs, every floor, and fold-equals-scan against a
verbatim copy of the code that was replaced.

The residue is stated rather than hidden: a constant-free witness guarantees
determinism plus its floors and **nothing more**, and cannot detect a behaviour
change by any means. That sentence lives in the test file, not only here.

## A note on the close's own review

The code half of the close was reviewed while this half was being written, and
that review found two Important issues, both of the same shape as the
campaign's recurring one: a doc comment claiming a seed guard that only one of
the two files carried, and a sweep still using a literal where its own named
constant existed. **Both were fixed** (`1f113f092`), by making the code true
rather than the sentence weaker — and the second turned out to be a no-op in
substance that had to be *measured* rather than assumed, because the sweep in
question makes zero calls at either stride on that seed. Recorded because the
pattern reached the last commit of the campaign, having been named three times
inside it.

The one Important carried out of the previous task also closed: the fear-fold
witness's total guard had a margin recorded nowhere, and it now reads
`history_growth` 2.28x against `segment_growth` 1.348x — the total sitting at
59% of its ceiling, so the guard trips once that fold's integration work grows
1.7x faster than it does today. A guard whose margin is unwritten is a guard
nobody can tell is about to fire.

## An improvement measured against a baseline the criterion did not name

The fear memory's cost is reported as 1.57× down, and that is against a
same-box control measured this week. The frozen criterion asked for a tenfold
fall **from 73-97 milliseconds per call**, and against that number there was no
reduction at all — the fold reads 93 milliseconds per call after the campaign.

Both statements are true and only one of them answers the criterion. The
control was added for a good reason (the frozen tables report no per-run
intercept, and the falsifier is a claim about the intercept), and it is the
better instrument for "did this change help". It is not the instrument the
criterion was written against, and a readout that quotes only the control's
ratio reads as a modest win where the frozen comparison reads as a flat miss.
**Say both, and say which one the criterion asked for** — the campaign's own
verdict table does, and the prose around it initially did not.

## The Confidence Gradient

**A bet moved, and it was re-scored.** `book/src/open-questions.md`'s section on
the substrate being cheap to write and unpriced to read counts *occasions* on
which cost was measured, and had three. This is the fourth and the first to
close a loop. The re-score says three things the numbers do not: there is still
no cost gate, so the base rate of an unwatched dimension is unchanged; the
campaign's own frozen criteria could not see its result, because the ecological
instrument's range does not reach the regime the change lives in; and removing
the largest known cost is what made the *next* one legible, at 84% of the timed
total in a single fold.

## Followups

Promoted verbatim from the campaign's own register.

1. **7b, the typed intention** — the recommended next campaign; anchors in the
   resident store this campaign builds. An epoch.
2. **Hysteresis on drive arbitration** — a tenant of the store; behaviour
   change, its own campaign.
3. **A threshold-crossing tenant** (Penstock §5.7: commit the crossing, never
   poll the predicate) — the store is where the crossing detector's state lives.
4. **Penstock stage 6, row width** — once folds stop re-walking history, fact
   size is the remaining memory term; measure before building.
5. **`agent_scaling.rs` clones the whole `RoomMeshMemo` per tick** (Penstock
   §6.7) — a harness property, still unmeasured; measure it before reading the
   level from that bench.
6. **`PrimaryAfraidMemo` lifetime** — it is per tick by design; with Sustenance
   checkpoints the past-day reads inside it are bounded, but whether the memo
   itself still earns its keep is a stage-3 readout question.
7. **Merge `KnownWater` into `LatestVisit`** — after Task 5 the belief set is
   derivable from the visit lists; one tenant, not two copies of the trail.
8. **A `Diurnal` creature's arbitration winner oscillates with the sun while
   `fatigue_at` reads zero** (Task 5 report) — possibly a latent defect in
   arbitration vs the activity cycle; not this campaign's.
9. **Decision 0237's filtered reset rule for the past-day affect path** —
   deliberately NOT applied (ledger, Task 3); applying it is a behaviour change
   on emitter-bearing worlds and moves census columns; needs its own campaign
   and a census refresh.

Items 1, 2, 7 and the hazard fold's remaining cost carry idea-registry rows;
the rest live here.

## Deferred minors, and where each landed

Every "minor (deferred)" recorded at a task boundary, with its outcome. "In the
ledger" is not a location, so each has one.

**Task 1.** A doc naming the kernel's hash constant wrongly — **fixed at 7a**
(the file the line lived in retired with the constants).

**Task 2.** Docs promising that every tenant advances together and that the
position accessor witnesses divergence, both vacuous with one tenant —
**resolved as a Task 3 ruling** (one advance-all, position asserts equality).
An unsatisfiable brief step, substituted with a standalone-store test —
**resolved as a Task 3 ruling** (the first real threading test). Nested borrow
guards — **resolved as a Task 3 ruling** (one guard per read site). A rejection
path with no fixture fact — **accepted**. An oracle sharing one decode helper
with the code under test — **accepted**, pre-existing and near-zero risk. A
comment claiming a distinction with no observable — **accepted**.

**Task 3.** Three sequential borrow guards where the ruling asked for one —
**accepted** (sequential, so no panic). A report disagreeing with the ledger
about decision 0540 — **resolved**: the ledger governed, and 0540 is now
written. The full suite not re-run after a doc-comment-only edit —
**accepted**. A per-entity map on the production read path where precedent is a
counter — **accepted**. One `pub` item with only integration-test callers
(`Sustenance::resets()`) — **accepted** (it cannot be `pub(crate)`). The
overlay cloned and sorted per call — **accepted**, and superseded in effect by
the Task 5b accumulator.

**Task 4.** A condition documented as exact that is an upper bound —
**accepted**. A fixture's wet/dry assertion mismatching its denominators —
**accepted**. A past-instant sweep with no in-test non-empty floor —
**superseded at 7a**, where every witness's floors were re-measured on the
shortened scripts. "Choose by measuring" not measured, and the belief set
duplicating rooms the trail holds — **carried as a registry row** (the
three-indexes-over-one-predicate row). Two `pub` items with only test callers,
two new argument-count allowances, and a tenant name describing the job rather
than the contents — **accepted**. A shape-4 sweep asserting equality rather
than a floor — **accepted** (3/3 today). A memo with no caller anywhere —
**accepted**, unresolved.

**Task 5.** A 64-seed sweep committed as prose with no committed witness —
**partly resolved at 7a**, where the search length was re-measured and the
probe's output recorded. A scan counter including empty-roster base cases —
**accepted**. A wrong rationale on a map-entry branch — **accepted**. The
three-index redundancy — **carried as a registry row**. Two pre-existing
false-premise doc sites — **fixed in Task 5's own fix round** (four sites
rewritten to the measured mechanism). Two seed-search tests each re-running the
world search — **fixed at 7a**: the search was cut from eight waits to two,
measured rather than estimated, and it still selects the same world. Two new
argument-count allowances — **accepted**. The fear-path file growing +560 net
because a private type's tests must live beside it — **accepted**, not moved.

**Task 5b.** The advance walking to the trail's end rather than to the instant —
**accepted** (a fresh partition pays reset-to-end once). An unkeyed memo input,
stable per entity today — **accepted**, and named in decision 0539. Two witness
counters that are the same expression twice — **fixed in the same fix round**
(declared one measurement). The eviction path having no direct test —
**accepted and disclosed**: it is one of the two things the chronicle's honest
limits say was never measured.

**Task 5c.** A mangled doc line — **fixed at 7a**. A merge message asserting a
count the artifact does not carry — **corrected in the ledger**, artifact left
alone because it was right. The emitter positive control taken on a seed the
search no longer selects — **retired with the constants at 7a**, and named as
stale in the surviving witness's own documentation.

## What this close could not source

Two figures in the campaign's own prose have no committed witness behind them
and are repeated nowhere in the book: the 64-seed replay sweep's counts, and the
per-seed script lengths behind them. They are recorded in the ledger with that
caveat attached, and a future campaign that needs them should re-measure rather
than cite.
