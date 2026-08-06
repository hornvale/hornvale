# The Long Age

Hornvale could say how heavy a creature was, what it ate, how it organised
itself, and what weather it tolerated. It could not say that one lived a long
time.

Longevity was not a property here. It was a side effect of mass — lifespan came
out of a scaling law with two inputs, body mass and metabolic class, and there
was no third. The only long-lived things in the world were the heavy ones.
Dragons reached a hundred and sixty years because they weighed two tonnes, not
because anything about a dragon is enduring.

The arithmetic had been read out of the source but never actually run, so this
campaign ran it first. An elf of seven centuries, at sixty kilograms, would have
to mass **976,563 kg**. A thousand tonnes of elf. Every other stated figure
reproduced to four decimal places, which meant the wall was exactly where it had
been described, and the campaign could start.

## Author the input, not the answer

The obvious move is to let a species declare its lifespan and be done. The
species registry says why that is wrong, and it says it by example: what the
registry *authors* is inputs — mass, metabolic class, potency, social form,
climate tolerance. What it *derives* is outputs — lifespan, age at maturity,
reproductive tempo, generation length, pace of life, metabolic rate. An
override on lifespan would not have been a new feature. It would have been the
first place the registry stopped meaning what it means.

So lifespan stays derived, and the law gains a third input.

What that input *is* took a detour through clockmaking. A clock's rate has two
independent controls: the oscillator, and the gear train between the escapement
and the hands. Kleiber's law is the oscillator, and it should stay mass-set —
a creature that burned slowly would be a *cold* creature, which is not what
longevity means. Nobody wants an elf that is cold. What is free is the gear
ratio: beats per lifetime.

That turns out to be the quantity real biology actually varies. Mammals run to
roughly a billion heartbeats whatever their size, and the animals that violate
the invariant are precisely the long-lived outliers — humans, bats, naked mole
rats. A long-lived kind is not a slow oscillator. It is a long gear train.

The authored quantity is therefore dimensionless, multiplies the time laws in
exactly the position the metabolic-class multiplier already occupies, and leaves
the basal metabolic rate untouched. An eleven-times-paced creature lives eleven
times as long, matures eleven times as late, and burns exactly as many watts as
before.

## The wolf interval

There is a constant in the allometry, `MAX_PACE_MULTIPLIER`, which normalises
the pace-of-life reading onto nought-to-one. Its documentation carries an
instruction to whoever comes next: if a future class needs a larger multiplier,
this ceiling must rise with it, rather than silently exceeding one and being
masked by the clamp.

Taken literally, an authored factor above 1.5 requires raising the ceiling. And
raising the ceiling rescales pace-of-life for **every kind in the world** —
moving two committed census columns across two thousand worlds and the pace
headline in every almanac. The instruction, followed literally, destroys the
result the campaign exists to produce.

Tuning has a name for this shape. In meantone temperament, buying pure thirds
forces one interval badly out; you cannot renormalise the wolf away, you can
only decide where to put it. The resolution is the same here: the constant's
real requirement is that saturation must not be *silent*, not that the ceiling
must always move. So the ceiling stays, its documentation is corrected to say it
governs the class component alone, and an authored factor above it saturates
pace-of-life at one — deliberately, statedly, in three places.

A very long-lived kind and a merely long-lived one will read the same. Nothing
distinguishes them today, and the region has no inhabitants.

## Long life is a cost

The interesting half of the result is not that a people can now live for
centuries. It is what the model already believed that would mean.

Language drift in Hornvale is a function of transmission events — a tongue
changes when it is handed from one generation to the next, so drift scales with
community size over lifespan. A settled people with many short-lived speakers
drifts fast and splits into dialects. A dragon, a community of one alive for
centuries, accrues almost no transmissions at all and its tongue is frozen.

That model shipped as a product of sociality and lifespan. It was not one. The
settled row of the product was **constant in lifespan** — every settled people
drew the same drift rate whatever its span, because the branch that consults
lifespan existed only on the solitary arm. `Settled × long-lived` is exactly the
cell an elf was meant to occupy, and the model could neither express the trait
nor notice it.

Both halves are now closed, and what falls out is that seven centuries is a
*cost*. A long-lived people drifts slower, so its daughters barely diverge and
its family tree stays shallow. Its genealogy goes further: at goblin's
twenty-one-year generation length, the median gap between a mother settlement's
founding and its daughter's — fifty years, in seed 42 — reads as two
generations' remove. Stretch the schedule and the same gap becomes **siblings**.
A people that lives for centuries does not found daughter communities through
descendants. It founds them through its own contemporaries, and the founder of
the old town is still alive in the new one.

The elf's boon, in this model, is cultural stasis.

## Nothing changed, which was the point

No world moved. The committed seed-42 world is byte-identical; every gallery
almanac is byte-identical, including all six peoples' life-history lines; both
census fixtures are unchanged; the stream manifest is unchanged, so no epoch is
owed and none was taken. Exactly one committed artifact moved — the type-audit
report, two rows, tracking three new tagged primitives.

That null is the deliverable. It is what makes the channel cheap for the
campaigns that will actually use it: dwarves first, then elves. A capability
that costs nothing to have is a capability the next campaign can spend freely.

The mechanism is that a factor of exactly one is an IEEE-754 no-op, so the
default path is not merely *close* to the old law — it is the same bits. That is
asserted against an inlined copy of the pre-campaign expression rather than
against an epsilon, because "close enough" is precisely the claim a determinism
project cannot afford to make.

## What a green test does not prove

The programme this campaign belongs to carries one shared acceptance criterion:
a trait that can be authored but that nothing reads is worse than one that
cannot be authored at all, because it looks exactly like a result. So every
campaign owes a *mutation* — a demonstration that the readout would report
differently if the axis moved.

The first mutation worked. Reverting the settled arm to its unconditional form
reddens the test, so the arm demonstrably reads lifespan.

The second did not, and the reason is worth more than the test would have been.
The intended target was the genealogy code — but that function looks its species
up **by name, from the canonical registry**, and this campaign deliberately ships
the channel with no occupant. Every row it can reach is the default, which is
bit-identical to what the mutation substitutes. The mutation is not weak; it is
*unobservable*.

The general form: a consumer that resolves its subject by name cannot be shown
to read an authoring channel that nothing yet authors. Only a consumer handed
the row directly can, because a test can fabricate one. The four consumers split
cleanly on that line, and it is a property of the shape of the code rather than
of anyone's diligence.

So the second mutation was re-sited onto the almanac's renderer, which takes the
row directly and does redden — arguably better evidence, since that function
writes into committed pages, and reddening it proves the channel can move an
artifact rather than merely a number. That the genealogy code forwards the
schedule at all remains a *code-reading argument*, and it says so at the test.
The first campaign to author a long-lived kind will close that for nothing.

## What this does not measure

It measures nothing about elves. There are none, and nothing in the world is
long-lived; the roster is exactly as it was. This campaign built a capability
and proved the capability is visible, and those are different claims from
"long life produces a near-frozen family tree" — which needs a roster to be true
of, and gets one two campaigns from now.

The metamorphic case is left a door rather than a room. A creature whose larva
is a different organism needs a *staged* schedule, and the channel is an enum
precisely so that arrives as a new variant instead of a new axis, changing no
consumer's signature. It is not built, it has no consumer, and saying so is the
point: a field nothing reads cannot be observed to be wrong.
