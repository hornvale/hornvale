# The Pallet

A pallet is the humblest bed — straw on a floor. It is the whole span this
campaign models: a body that would prefer a bed, will take bracken, and can pass
out in the road.

The previous campaign in this line made a body's rest better if the room it was
in held something to lie on. That was a room-level boolean and a single number:
*was there furniture here, yes or no*, times one-and-a-half, for every creature
in every world. This campaign gives the body a **choice**, commits **what it
chose**, and makes **how much that helps** a property of the species doing the
sleeping.

It also finishes a conversion the previous campaign shipped half of, knowingly.

## The rest was measured in the world's day and the sleep was not

A standard day here is a fixed 100,000 ticks. A world's *local* day is its
rotation period, and a legally pinnable world turns anywhere from four standard
hours to a hundred — from a sixth of a standard day to more than four of them.

The rest side was converted first: a rest lasts a quarter of the world's own
day, because a rest is a physical duration and a physical duration is measured
in the day the body actually experiences. The sleep side was left as a published
finding, because converting it was a separate piece of work.

The consequence of fixing one and not the other was measured rather than
predicted, and it was shipped on purpose. On a hundred-standard-hour world in
permanent night, a rest ran 104,166 ticks and a sleep ran exactly 100,000. **The
rest outlasted the sleep** — an inversion of the plain meaning of the two words,
reachable by pinning a legal rotation period and waiting for polar winter. The
sleep did not end because the body was rested; it ended because the scan looking
for the body's next waking moment gave up after one standard day and returned
that.

The inversion was held open by a test that **ran**, asserting the wrong
ordering, so that the day someone converted the sleep path the tree would go red
and the test would have to be deleted. That is what happened. Four quantities
moved onto the local day — the sleep's own floor, the wake scan's step, the
scan's bound, and its give-up fallback — and the ordering is now invariant in
the rotation period, because both sides scale with it.

Deleting that test was the success condition rather than collateral, which is
worth saying plainly: an inverse assertion is a claim with an expiry date, and
the expiry is the point.

The interesting part is what it took to replace it honestly. A test that goes
away and a test that arrives in the same piece of work reads as a *replacement*,
and nothing checks that the new one exercises the same branch. The deleted test
forced the give-up-fallback arm; its first replacement forced the floor arm.
Both are good tests about the same subject at the same extreme, and they take
different arms of the same `match`. The instrument that settles it is not
counting tests but mutating each converted quantity back to its retired
standard-day form and confirming something reddens for each one, separately.

Three of the four needed a fixture nobody would have guessed from a
specification. The wake scan's *step* is the scan's own resolution, so it can
only matter where the retired grid and the finer one **bracket an actual wake
transition** — and a diurnal window is half the day, far too wide for either
grid to miss. A crepuscular window is about 2,124 ticks: narrower than the
retired step. That is verbatim the concern the constant's own documentation had
been asserting for months, and the witness and the original complaint turn out
to be the same sentence.

## The body picks, and it is allowed to pick badly

The choice ranges over the anchors of the room the body is already in, and never
proposes movement. A body in a room with nothing to lie on sleeps on the ground.
That is the intended outcome, not a fallback — creatures wandering out of a room
to find the nearest bed is a different and worse world.

The constraint that shaped the design is stranger than it sounds: **the chooser
must be able to be wrong**. A creature found sleeping somewhere unsafe or
unrestful is a useful signal that something upstream needs tuning — pathing,
drive order, where settlements went, when bodies wake. A chooser that always
takes the best available site cannot produce that signal, because it has defined
the bad outcome out of existence. So the badness has to come from the world
rather than from a randomizer: a room may afford nothing, and the body sleeps
where it is.

This inverts the obvious implementation. The obvious implementation is an argmax
over quality; what shipped is weaker than that on purpose, and any later change
that makes the chooser cleverer owes the same guarantee.

## What may be written down

A creature's persisted position in this world is its **room**. Anything finer
exists only inside the presence bubble while a body is being simulated, and is
never serialized; an anchor has no coordinate at all, and its identity within a
room is positional rather than stored. So *"the body slept at anchor 3"* is not
a thing this world can record, and no part of this campaign tried.

But an anchor is a kind of thing standing somewhere, and the kind is durable.
`bed` is a registered concept and a stable string. So **the kind is recordable
where the anchor is not**, and the campaign records exactly that: a new fact,
`slept-on = bed`, additive beside the existing fact carrying how long the body
was down. A body that slept on bare ground commits nothing at all — absence is
the record for the road, which is the world's ordinary case rather than a gap to
backfill.

The fact carries no place, and that is a ruling rather than an omission. A room
in this world has no entity identity: it is a facet, named by text, and every
other fact carrying a place names a settlement, a community or a person. The
room is not lost — the timeline of where a body was already answers it by day,
and a sleeping fact joins that the same way any other does. Filling the field
with a derived identifier would have spent a declared save-format contract on a
value that re-encodes the kind already in the fact and cannot be inverted back
to a room at all: not merely redundant, but inert.

One consequence falls out with no work. The narrative window replays any
entity's facts against the registry's own predicate documentation, so recording
a kind yields prose about it for free, on a surface nobody had to write.
Recording a bare quality number would not have.

## What a bed is worth depends on who is lying on it

The old multiplier was tagged *universal*, with the reason *"a uniform
multiplier on every rest or sleep act's own rate, bounded rather than derived —
not a species property."*

That reason answers a different question than the one being asked. "Bounded
rather than derived" says where the number came from; "not a species property"
is a negation, defining itself against a category rather than naming an axis.
And the fact settles it: a xorn is ametabolic living stone and gains nothing
whatsoever from a bed.

So the multiplier became a table over the species roster — thirty-nine rows
carrying seven distinct values, derived from two axes the roster already states
rather than from anything invented for the occasion:

- **Insulation.** A body lying on a surface loses heat into it, and only an
  endothermic body pays that bill; an ectotherm's realized metabolic rate
  couples to ambient temperature instead, so insulating it from the floor buys
  it much less.
- **Fit.** A made bed is made by, and for, the body that made it. A settled kind
  is the one that builds, so its bedding fits it and it is habituated to using
  it.

| what the body collects | value | who |
|---|---|---|
| both | 1.50 | the fourteen settled peoples |
| insulation only | 1.35 | wild endotherms under a tonne |
| fit only | 1.30 | the one settled ectotherm |
| neither, but it still lies down | 1.20 | wild ectotherms under a tonne |
| neither | 1.15 | any land kind at or above a tonne |
| neither | 1.05 | the fully marine kinds |
| nothing at all | 1.00 | the ametabolic and the sessile |

**1.50 is the ceiling, not the midpoint**, and that is the load-bearing choice.
It is the exact number the old constant carried, and that constant's own
argument — half again is the plainest reading of *prefer* that a body sleeping
in the road can still live with, where doubling would make a bed a necessity —
was an argument about the **most** a site may be worth. So the table descends
from it and never exceeds it. No creature in any world gains more from a bed
than it did before the table existed, and the peoples the calibration was
actually written for keep their number byte for byte.

The floor is 1.00, meaning *no bonus*, and there is nothing below it. A value
under one would make a furnished room repay less than open ground, turning a
preference into a penalty.

Two blocks of the table are deliberately flat and neither is a placeholder. The
fourteen settled peoples are flat because they are flat in every trait the table
reads — all endothermic, all settled, all between eighteen and a hundred and
thirty-seven kilograms. Separating a bugbear from a goblin would need a softness
or a posture axis the roster does not have, and inventing one to avoid a
repeated number is the worse error. The four rows at the floor reach it by two
different roads: one kind has no metabolism to restore, three have no lying
posture to support, and *no bonus* is the whole of what each of them can
collect.

## The ceiling this campaign did not lift

The grade is still **room-level**, and it will stay there until something larger
changes.

A body that passes out in the street of a cold, built town is repaid exactly as
well as one that found the bed indoors, because the recovery calculation
re-derives a body's history from committed facts, and the only location any fact
carries is the room. Inside one room the two are indistinguishable to the fold,
and no amount of care in the chooser changes that: the chooser's answer lives in
the presence bubble and the fold reads the ledger.

This is a texture failure sitting underneath a mechanism that is entirely
correct, and it is not a rough edge that can be tightened. Lifting it is a
decision about whether fine position is serialized at all, which is a
constitutional question rather than an implementation one.

What changed is that it is now *fixable*. Committing the kind puts the durable
half of the choice in the ledger. A later campaign that wants the fold to know a
bed from a heap of bracken has a fact to read; before this campaign it would
have had to invent one, or reach for the position it is not allowed to keep.

Two further rungs are declared and not built, in the type's own documentation
rather than as a comment nobody will find. *Which* thing a people tends to sleep
on is a matrix of species against thing, and needs edges between kinds that the
object registry does not have. *This one likes a sleeping bag* varies below the
species and would be derived from a body's own lineage rather than stored.
Neither has a number to point at today, which is precisely why they are
sentences rather than tagged constants: inventing a constant so that a tag has
somewhere to live is a worse outcome than admitting the absence.

Threat stays out of the grade entirely, on a distinction worth keeping.
Preference is innate and safety is situational: a body knows by instinct that a
bed out-rests a floor, and cannot know whether a particular site is safe without
being there. That is a separate term for a later campaign, not a component of
this one.

## Two instruments that were green and could not have been otherwise

A committed trace of one world's emotional weather did not move at all, despite
the table changing the multiplier for twenty-five of thirty-nine kinds — every
one of them downward except the fourteen sitting at the ceiling, and including
four of the six species the trace samples.

That null needed a control, and got four. Exaggerating the peoples' row to four,
the wild endotherms' row to four, and then **all seven rungs at once** to four
each left the file byte-identical. Changing the bare-ground baseline itself
reddened it immediately. So the trace is fully sensitive to the site multiplier,
and no body in its window ever takes a furnished bout at all: the only thing in
the world that offers a place to sleep is a fireside bed, which needs a built
and cold room, and the traced world is open ground from end to end. A green run
of that file is not evidence about anything on the furnished path, and the code
now says so at the point where someone would consult it.

The other instrument failed in the opposite direction, and it is the more
useful failure. Adding a fact type means registering it, and the plan's
reassurance was that forgetting is self-checking — the ledger rejects an
unregistered predicate, so a run that commits one fails loudly at the point of
commit.

The ledger does return that error. But **an error is only as loud as its
callers**, and two of them are deaf: both variants of the laboratory's
simulation loop match the failure and break, silently truncating the run. Two
population calibration tests were passing on simulations that had stopped on day
one — in nineteen and fifty milliseconds against a committed fifty-three and a
hundred and forty-four, a ratio nobody notices on a test that fast. It was found
by making the error fatal and re-running: a positive control on the error path,
which is the same discipline an empty diff needs, pointed at a branch instead of
at a file.

The truncation is older than this campaign and was not fixed by it. Changing
what a simulation does when a commit fails is its own question — fail fast, or
carry on and surface the truncation in the result — and a task boundary is no
place to answer it.

A pallet is not a bed. But a body that took one, and a body that lay down in the
road, are now two different entries in the record instead of one.
