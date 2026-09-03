# The Rack

*A rack is a frame of slots. Everything the walk knows about a body — its
room, its felt state, whether it is on the roll — goes in the slot the body
was given when it joined, and a turn reads across the slots instead of asking
the ledger about each body again.*

## Sixty-seven answers the world already knew

[The Roll](./the-roll.md) put sixty-eight bodies in a possession session where
there had been seven. It did not change what a turn *does*: on every
`snapshot()` — which is every turn, because the client redraws after each one
— the session asked each present body how it felt, and each answer was a fold
over that body's committed history. Thirst integrals, fatigue, a band-shared
water belief, a remembered-hazard scan, an A\* plan home, then a full
arbitration. Sixty-seven of those. And because the snapshot is a shared
reader, it could not reach the session's own memos and rebuilt them cold each
time.

Beside that sat a second kind of asking. Working out who is standing here
meant folding the ledger for each body's room — twice per body inside one
filter, once more for the vantage: a hundred and thirty-seven ledger scans to
answer *who is present*, per turn.

The world already knew all of it. The tick had just computed each creature's
resolution, with its own alarm field, its own mode hysteresis, its own belief,
at its own instant — and then dropped it on the floor. It had just committed
each creature's room. Every question the turn was folding to answer had been
answered a moment earlier by the walk that made it true.

## The shape: the tick writes, the turn reads

The session now owns one struct of arrays. Six columns — the bodies, their
roll keys, the roll mask, each body's room, each body's felt state, and
whether the tick has written that slot yet — all indexed by a `Slot`, all
appended by exactly one method that pushes every column in a single statement.
An index misalignment is not a thing a test catches; it is a thing that cannot
be written.

Each column declares what kind of thing it is, and that declaration is load
bearing. **`position` is a view**: it must equal the ledger fold at every
read, and a disagreement is a bug in whoever wrote it, never an argument for
re-folding. **`felt` is content**: it is a resolution, not a projection of
anything, and a stale-looking value is a true fact about a body that has not
been advanced.

Three scalar side-fields that made the driven body a special case of itself —
its affect, its mode, its suppressed drives — are gone into the driven slot's
`felt`. Possession selects a body; now the data says so too.

## What a creature feels

The felt-state ruling is the campaign's fidelity decision and it leads
everything else. The old stateless read was, in its own doc's words, "the same
arbitration a walk step runs, but stateless" — no alarm, an idle mode, an
empty frontier, the session's day rather than the walk's own instant. It was
adopted by [The Confidant](./the-confidant.md) not because it was the truer
number but because the truer number was unreachable: the tick computed it and
threw it away.

So the rack stores the tick's. **A creature feels what its own last resolution
felt, and between ticks it does not re-feel.** A room hop that advances the
day by a tenth does not make sixty-seven bystanders re-integrate their thirst;
they feel as of their last step, which is what they would say if asked. A body
no tick has walked yet — turn zero, or a resident who joined the roll this
wait — carries a stateless seed until its first tick, and the roster knows
which of the two it is holding.

## The numbers

Per snapshot at seed 42's flagship: **sixty-seven drive folds became zero, and
a hundred and thirty-seven ledger position folds became zero.** Those are not
timings; they are counts of operations, asserted by a test. `needs`, which did
the same work behind a different verb, fell from **27.559 ms to 0.022 ms**.

The wall clock is the more complicated half, and the honest report of it is a
falsified prediction.

**P1 was preregistered as a budget and missed.** The snapshot in the home room
was to come under 3 ms from 32; it reads **4.1–4.9 ms**. In a chamber it was
to come under 3 ms from 44; it reads **8.4–16.8 ms**. Both readings were taken
on a heavily contended box, so both are upper bounds, and both are large
improvements — but neither met the line, and the reason is worth more than the
miss.

**The budget was set below a floor the campaign never touched.** The
pre-change measurement contains its own answer, if you read it row by row
instead of as an average: three of its walk-band rows cost 4.2–4.4 ms while
every other row cost 31–32 ms. Those three are exactly the turns where the
possession had stepped out of its settlement's room, so nothing was present
and the call folded nothing. **4.2 ms was already the fold-free floor of a
walk-band snapshot before this campaign began.** What is left is the spatial
channel and roughly seventy kilobytes of JSON — which the spec assumed was
inside the budget, and is not.

The chamber band prices something else, and the memo is what made it legible.
After `look`, a chamber snapshot costs 8.4 ms; after `map` or a chamber `go`,
16.3–16.8 ms — same chamber, same turn shape. `look` derives a sighting for
its own presence line and the snapshot reuses it; `map` and `go` derive none,
so the snapshot pays for its own. **The difference, about 8 ms, is one
shadowcast** — the largest single item left in a chamber snapshot, and a
better argument for sharing it than anything available before measuring.

## The gate that passed at nine times its ceiling

This campaign's second deliverable was a regression watch, and building it
meant first understanding why the existing one had said nothing while a turn
got ten times more expensive.

`session_cost.rs` bounds a pooled wall-clock median. Its own doc already
admitted that twenty of the fifty samples that median is drawn from exceed the
ceiling individually while the gate passes. That is bluntness, and it is not
the failure. **The failure is that the millisecond assertions are gated to a
host the test no longer runs on.** They fire only on the Mac; since the heavy
tier moved to the canonical Linux box, they fire nowhere.

Measured, at close, by running the same test on the same quiet box in the same
profile against main's tip and against this campaign's:

| reading | main (Roll + Pawl) | The Rack | pre-Roll basis |
| --- | ---: | ---: | ---: |
| pooled turn `handle+snapshot+json` | **81.490 ms** | **17.706–17.959 ms** | 3.906 |
| indoor `snapshot()+json` | **78.416 ms** | **21.384–21.607 ms** | 18.720 |

**The test passed at main's tip, at 81.490 ms, against a 9 ms ceiling.** A
ceiling a ninefold overshoot walks through is not a slack gate; it is a gate
that is not running. The Roll moved this control about twenty-one-fold over
its seven-body basis and nothing that runs ever saw it. The Rack cut the
pooled turn 4.6× and the indoor snapshot 3.6× from there — it regressed
nothing and recovered most of a regression this instrument was built to catch
and could not.

What replaces it is a **count**. A turn's work — drive folds, plan searches,
ledger position folds, shadowcasts, bodies scanned — is counted on the session,
reset at the top of each turn, and asserted per verb class: a snapshot performs
zero folds, a chamber step folds nothing and derives at most one shadowcast, a
wait folds at most the roll's length. A count is identical on every machine,
cannot flap on a loaded box, and reddens on the commit that adds a fold rather
than on the campaign that goes looking. The wall clock stays, demoted to what
it actually measures — the box — with its ceilings re-pinned by this campaign
as an explicit reviewed act rather than a quiet one.

The counters have exactly one live writer each, and that is not tidiness. Once
the turn stopped folding, one counter had no bump site left at all and the
compiler said so. **A counter nothing increments is a zero that cannot fail**,
and the test asserting it would have read green while asserting nothing.

And the count had a blind zone of its own, found at the campaign's final review
and worth more than the fix it prompted. The counters live on the session, so
they measure what the session's own module does — while a walk-band snapshot
builds the chart in a different module, which was folding the ledger once per
creature to place its mark. Sixty-seven more folds a turn, and the test
asserting a snapshot performs none read zero the whole time. **A counted budget
bounds the module it is threaded through, not the verb it is named after.** The
chart now reads the roster's column like everything else; no counter was pushed
into it, because there is nothing left there to count and a counter asserting
the absence of what it was added to measure is the same dead zero as before.

What pins it instead is a test that had to be written, and writing it found the
second half of the problem. The obvious mutation — draw each mark at the
creature's *home* rather than where it stands — was run against the whole
vessel crate first, and **a thousand tests passed**. Nothing anywhere asserted
that a creature's mark on the chart follows the creature, because every chart
test stands at seed 42, where nobody ever leaves home. The deleted fold could
have been returning the wrong room for as long as it had existed and the suite
would have agreed.

## What the world could not be made to say

Three things the campaign learned by running rather than reasoning.

**Seed 42's flagship population never changes room.** Probed at spans of one,
five, thirty, a hundred and three hundred and sixty-five days: **zero
`agent-at` facts** for all sixty-eight bodies. The settlement condenses onto
fresh water, so its residents drink where they stand. Three of the plan's
prescribed test mutations were null there — including the one meant to prove
that the tick's write-back matters at all, which cannot fail in a world where
nobody moves. The view-equals-scan battery now runs seed 42 for the driven
writer and seed 7, where seventy-four of a hundred and two bodies leave home
within thirty days, for the tick's. The Roll had already measured this from
the other side — residents separate in sixteen of sixty-four seeds — and this
is the same fact seen from the direction that costs you a test.

**The obvious per-turn memo would have deleted a narration, silently.** A
sighting derived once per turn, cleared at the top of each turn, looks
correct. But `wait` reads the sighting on *both sides of its own tick* — once
for who could be seen while they were still here, once for who can be seen
having arrived — and a per-turn memo hands the second read the first's answer.
Both halves then agree, and every chamber arrival and departure stops being
narrated. Nothing in the crate asserts on that, and seed 42 would never have
shown it. The memo is keyed instead, on everything a sighting reads, including
a write counter added to the occupancy map for this one purpose. Three
in-module tests caught the first, incomplete key within a single run.

**A view-equals-scan test is only as wide as the writers it exercises.** The
first version of the tick's write-back took the driven body's room from its
own solo walk — and `wait` discards that walk's facts. Under an imposed
controller, which is what possession is, the walk can move the body while the
ledger never records it, and the column disagrees with the world. The test's
script never possessed. Found in review, reproduced at seed 7, and fixed at
the writer: the driven slot's room is now written only where its `agent-at`
fact is committed, so the column follows the ledger by construction rather
than by a second fold agreeing with it.

## Nothing coarse moved

The census, queued at close, moved **zero of its columns** — and a null with
no control is not a result. The control fired on the same commit: the session
snapshot goldens moved sixty-nine and fifty-eight felt-state leaves, and the
seed-42 possession gallery moved two hundred and fourteen lines, every one of
them a felt-state line. **No JSON key other than `felt` moved anywhere**, in
any artifact. No world file, no almanac, no laboratory output, no Domesday
page. The instrument was live; the world did not move.

The client feels it too, and reports the same split verdict. Every movement
turn is faster than its matched pre-campaign turn — `enter` fell from 87.2 ms
to 47.4 ms — and outdoor turns met the 15 ms line at 9.97–14.10 ms, while
`enter` and the chamber turns did not. (That range is the min and max over
every outdoor turn in the reading, rounded half-up to two decimals; the rule
is stated where the numbers live, in the client bench's own Measured block.) Measured contended, reported rather
than tuned.

## Honest limits

**Felt state lags until the next tick.** That is the ruling working, not a
defect, but it is a real property: ask a body how it feels immediately after
walking past it and you get what it concluded at its last step, which may be a
tenth of a day old. A campaign that wanted continuous affect would have to
decide what "continuous" means for a creature that is only advanced when
something advances it.

**A crowd still reads flat, and this campaign neither caused nor cured it.**
After a wait, sixty-three of the sixty-seven present bodies report the same
sentence. The obvious reading is that the ruling flattened them — sixty-seven
co-present residents resolving the same way. It is not: the pre-change gallery
was **already** sixty-three of sixty-seven identical. The ruling changed which
phrase the crowd shares, not how many phrases a crowd has. What produces the
flatness is the bucketing that turns an affect into prose, and whether that
phrasing should carry the affect's object or magnitude so a crowd reads as a
crowd is a live design question, not a finding of this campaign.

**The sighting is still derived once per turn.** Sharing it within a turn was
the cheap half; the chamber's ~8 ms shadowcast is still paid by any turn that
needs one and did not already have it, and that is the largest single item
left.

**The JSON is what remains.** Seventy kilobytes a turn, serialized on every
one. The folds are gone and this is what the budget now hits.

**The wall-clock gate is still calibrated for a machine it does not run on.**
Its ceilings are re-pinned and its docs now carry the table above, but a
Mac-keyed basis on a Linux-only tier is the same vacuity it always was. That
is a standing follow-up in the idea registry, and closing it means
recalibrating a whole constant set together — a campaign, not a task. In the
meantime the counted budget is the instrument that does not have the problem.

**A possessed body's walk facts are still discarded.** Its thirst grows
monotonically and a long possession diverges from its own ledger — pre-existing
since The Coercion, surfaced again here because the driven column's writer had
to be reasoned about, and parked rather than fixed.

## What The Pawl left, and what the rack does with it

[The Pawl](./the-pawl.md) landed in the middle of this campaign, and the two
touch the same file with opposite instincts. The Pawl argued that the seam is
at *read* — a fold store handed out through a shared reader — and built one.
The Rack argues that the seam is at *write* — the tick fills the columns and
the turn only reads them. They are not in conflict, because The Rack retires
the path The Pawl was protecting rather than reopening it: the snapshot no
longer folds through a shared reader at all.

The absorption merged cleanly and the fold store survived intact, threading
through the two seeding sites. It is worth naming what it keys on: **the fold
store is keyed by tick and by seed, and never by turn.** A turn is a thing the
player does; a tick is a thing the world does; and only one of them changes
what a creature has lived through.

One thing the merge did not do quietly. Both campaigns had edited the same row
of the idea registry, and git kept both lines — a clean automatic merge that
produced a duplicate row. The gate caught it. A diff review would not have.
