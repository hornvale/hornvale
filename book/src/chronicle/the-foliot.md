# The Foliot

A foliot is the crossbar of a verge escapement: a weighted arm that swings back
and forth, and whose swing decides how fast the whole clock runs. It is not the
escapement. It is the part that *regulates* what the escapement releases, and
moving its weights inward or outward changes the meaning of every hour the
clock will ever strike.

The Escapement gave Hornvale a tick — an exact integer count since genesis,
100,000 to the standard day. What it did not give was any guarantee about how
that tick related to the units the world actually keeps time in. A planet's day
is drawn, not chosen; and a drawn day, measured in ticks, is almost never a
whole number of them.

## Two lattices, and why the second one was not a mistake

`windows/vessel` — the layer a possessed creature acts through — kept its own
tick. It declared its own rate, its own `Ticks` type, and a function called
`days_of` that converted out of its lattice and into the kernel's on every
single action a creature took.

This looks like duplication, and the project's own idea registry recorded it as
a defect: *a live latent defect, one `Session::charge` rounding from
observable.* It was not. The second lattice existed to buy a property the
kernel's could not offer.

`ActivityCycle` — the mechanism that decides when a creature wakes — is keyed
on the **local** day. If a local day is not a whole number of ticks, every dawn
lands between two of them and rounds, and that error beats against the day
cycle over a long run until the creature is waking at dusk. So vessel derived a
lattice in which a whole local day *was* an exact integer: `round(d × B)` ticks
to the day, whatever `d` the world had drawn.

The consequence is a piece of arithmetic worth stating plainly, because the
campaign got it wrong first. A local day is an exact integer of **vessel**
ticks, and `d × B` **kernel** ticks — and `d × B` has a fractional part. So one
vessel tick is not one kernel tick. It is `d·B / round(d·B)` of one, and the
conversion `days_of` performed was doing real work.

## What the crossing actually cost

The campaign opened by asserting that the bridge was pure loss and could be
replaced by the identity. Before acting on that, it measured — and the
measurement refuted it.

Over the day lengths the rotation pin admits and the action costs the cost
function can actually produce: 438 (day length, mass, terrain) combinations
where the round trip differs, each by exactly one tick. Over a wider sweep, the
direction of the error: **213 losses against 211 gains, net −2 ticks**, because
`round(d·B)` sits above `d·B` 651 times and below it 650.

Symmetric noise. Nothing accumulating. Replacing that conversion with the
identity would have *introduced* an error of exactly the size it claimed to
remove, and the commit message would have read plausibly while doing it.

Two sampling errors are worth recording alongside the result, because either
alone yields a confident wrong answer. Sampling action costs only up to the
authored base of 10,000 finds **zero** witnesses and reads as "no defect at
all" — the error scales as `t·ε / round(d·B)`, so a differing tick needs a cost
on the order of a whole local day. Sampling instead to the mass band's ceiling
of 100,000 kg overstates it in the other direction: no authored species exceeds
6,000 kg, so the largest cost any creature can actually incur is 121,709 ticks,
not the 246,000 the band permits.

## Removing the reason, not managing the consequence

Two fixes were available. The narrow one defines vessel's day as `round(d·B)`
kernel ticks and leaves astronomy's day continuous — cheap, contained, and it
relocates the discrepancy rather than removing it: vessel's day boundary would
drift from astronomy's by half a tick per day, about a full day per two hundred
thousand.

The one taken goes to the draw. **A world's rotation period is now stored as an
exact tick count.** A local day divides the kernel lattice exactly, because it
is defined as a whole number of them. There is one lattice, and the conversion
question does not arise.

What makes this cheap is what the change does *not* touch. The draw is
unaltered — the same two values pulled from the same stream in the same order —
and the quantization happens afterwards, where the rotation is constructed. So
no seed label takes an epoch suffix, and every pin-isolation test passes
unmodified. `Calendar::day_length()` still answers in continuous standard days,
now derived exactly from the stored integer, so its thirty-one call sites did
not move.

Three things then collapsed, and each had been real machinery. The scheduler's
crossing between lattices had branched on whether a rotation pin was set,
because a pinned world genuinely put the two rates apart; it is now the
identity. The queue's rescale factor is gone entirely — no multiply, no round,
no variable. And the replay's day-length parameter went **dead**, which is the
cleanest evidence available that a creature's charge no longer consults the
planet at all. One test grew stronger in passing: *a move costs the same
duration on every world* had been asserted to within 0.1%, because the cost
crossed the local lattice and back. It is now asserted as exact equality.

## What an epoch cost, in full

Every world regenerates. The derived day length moves by at most half a tick —
0.432 seconds — and the whole visible consequence is small enough to enumerate.

The committed seed-42 world changes by **one number**: its day length,
0.87987998 to 0.87988 standard days, which is 87,987.998 ticks snapping to
87,988. The eclipse scene moves only in ground-track longitudes, and only as
that predicts — 0.0008° at day 85, growing to 0.017° by day 1908, because a
ground track's longitude *is* the world's rotation phase and a phase error
accumulates linearly. Days, tick counts, latitudes and durations are
byte-identical. A creature's affect trace moves six values out of 411, every
one by 1e-8, the smallest difference the emitted precision can express, with no
label, valence or object changed anywhere.

And then the census, which is where the change stops being small and starts
being interesting.

## The population moved, not the language

Refreshing the thousand-world census moved four aggregates that have nothing
obviously to do with clocks: the mean generated name length for goblins and for
kobolds, the mean name-transparency, and a null-control effect size over name
lengths. Re-running the calibration suite surfaced three more, and the four
homophony means moved together.

The chain is worth following, because "a tick epoch moved goblin name lengths"
reads as a non sequitur. The day length feeds climate's diurnal term. The
diurnal term feeds carrying capacity. Carrying capacity decides which sites can
seat a settlement, and therefore which settlements survive to seat a flagship,
and therefore which peoples exist at all to hold a lexicon. The names are drawn
from the concepts a people compounds over.

The language did not change. The population did — by a handful of worlds out of
a thousand, which is what a shift of five thousandths of a percent in a mean
looks like from the far end of that chain.

The claim those pins exist to witness was re-checked rather than assumed, as
the preregistration discipline requires: bugbear still leads goblin in
homophony by 3.673× and hobgoblin by 3.560×, both far above the 3× line that
would falsify it. Present counts did not move at all. Only the means did.

## What is not done

The real defect in this area was found by verifying the false one. The
scheduler's charge was always exact — it converts once and adds integers. The
**replay** is not: the catch-up path accumulates floating-point days in a loop
while the walk it reconstructs advances on the integer lattice, and the site's
own comment says that divergence "would be a failure by construction."

Converting it was attempted and reverted. It passes 442 of 444 unit tests and
then breaks a shared-clock invariant by 75 ticks — an ordering inversion, not a
rounding. That layer keeps floating-point days as its currency end to end, so
retyping one field leaves a boundary the scheduler's interleaving is sensitive
to. It is recorded, with its measurements, as work for its own campaign rather
than a loose end of this one.
