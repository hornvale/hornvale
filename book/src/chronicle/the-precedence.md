# The Precedence

*Precedence is the question of what comes before what. A queue answers it one
way — who acts next — and a ledger answers it another — what happened when.
For as long as the simulation has had more than one creature moving at a time,
one structure has been answering both, and the two answers were not the same.*

A creature in Hornvale acts by being popped off a queue. The queue is ordered
by the moment each creature *begins* its next action, which is the only
sensible way to decide who gets to move: whoever is free soonest goes next.
Each action then costs time — a bear crosses a room more slowly than a person,
and a drink is quicker than a meal — and the fact the action produces is
stamped with the instant it *finished*.

Those are two different orderings. They coincide only if every creature's
actions cost the same, and the entire point of the action clock is that they do
not. The gap between them is the population's **cost spread**: the difference
between the most expensive act anyone is performing and the cheapest. Popped at
the same instant, a bear setting out to walk and a mouse stopping for a drink
finish ten thousand ticks apart, and the ledger recorded them in the order they
*started*.

So the record of the world was not in chronological order, and had not been
since the shared clock was built.

## The test that should have caught it, and the sentence that stopped it

There was an invariant. It walked the emitted facts and asserted that time ran
forward, and it allowed a slack of exactly one tick. Its comment explained the
slack: creatures that tie at the same rounded tick are separated by their entity
identifiers, so their exact positions within that tick can differ slightly, and
bounding the regression by a tick is the honest form of "one timeline."

That explanation is wrong, and it is wrong in an interesting way. It describes a
real phenomenon — sub-tick disagreement between creatures sharing a tick — and
that phenomenon is not the one occurring. The disorder was never a fraction of a
tick caused by imprecision; it was thousands of ticks caused by arithmetic
working correctly on two quantities nobody had noticed were different. **The
tolerance had never been bounding the quantity that actually varies.**

This is why the defect survived a campaign that went looking for it. A previous
campaign set out to make the simulation's clock exact, on the reasonable theory
that a layer accumulating fractional days while the world advanced in whole
ticks must be drifting. It made the change; the invariant broke by
seventy-five ticks; and because the invariant's own comment said the only
possible disorder was sub-tick imprecision, a seventy-five-tick violation read
as evidence that the change had introduced something new. Three attempts went
looking for what. The campaign deferred the work, recording the ordering
inversion as a cost of the retype.

## Two numbers in a fixture

The defect is reproducible on unmodified code, and the reproduction is smaller
than any of the theories about it.

The test that exercises interleaving builds two creatures sixteen-fold apart in
mass — exactly two-fold apart in tempo — and lists their masses in an array. It
also mints their identifiers in the order that array is written. Since
identifiers are what break a tie in the queue, **the order of two numbers in a
test fixture is the order the queue resolves ties in.**

The published fixture listed the light creature first. So the light creature
held the lower identifier, popped first at every tie, and — being faster —
always finished first too. The two orderings agreed for the one arrangement the
test happened to use.

Reversing the two numbers, and changing nothing else, produces a backward jump
of 9,925 ticks against a one-tick tolerance. On a real world's population — the
one the project's own health census measures — the same defect appears on
sixty-two of the measured ticks, the worst of them out of order by 10,014 ticks,
something over two hours of world time.

The fixture had been sitting in the one configuration where the bug could not
appear.

## What the ledger is for

The fix is to sort each tick's emissions by the instant they occurred, at the
point they are produced.

This looks like a patch on a symptom and is not, for a reason worth stating
plainly: **the emitted order is the only thing the queue's pop order produces
that anything outside can observe.** Creatures in a tick do not watch each
other. Each one's spatial bookkeeping is addressed by its own identity — the
structure has no operation for "who else is here," so a cross-creature read is
not merely absent but inexpressible. Perception is built from the committed
ledger before anyone moves. The in-flight facts of the current tick *are* read
mid-tick, but only a creature's own, and they are sorted before use.

That last detail is the quiet joke of the campaign. Three functions above the
scheduler, the deciding routine already folds committed history together with
the current tick's emissions and sorts them by day, because it needs them in
chronological order. The codebase had solved this exact problem, on this exact
data, and the scheduler had simply never done the same for its own output.

Named properly, the fix is windowed event-time reordering, a shape any stream
processor would recognise: reorder within a window, bounded by a watermark. A
tick is the window and its end is the watermark, and that premise has two halves
— no fact may be dated outside its own window, and consecutive windows may not
overlap. Both are now asserted by tests. The first was written deliberately
*before* the fix, so that it could refute the design rather than ratify it. The
second was missing, and a review caught that the campaign had proved half of its
own premise and treated the other half as obvious.

## What was left undone on purpose

There is a more principled structure available. A discrete-event simulation
conventionally keys its queue on the instant an event *occurs*, not the instant
work on it begins, which makes the emitted order correct by construction and
needs no sort at all. That is the better architecture and it was not built.

It would move every committed trajectory in the project to fix a property
nothing can currently observe, because creatures are mutually blind within a
tick. So it is deferred behind a stated trigger rather than a vague intention:
the moment any creature can observe another's state mid-tick, pop order becomes
load-bearing and the sort stops being sufficient. A test now asserts that
reversing the queue's tie-break changes nothing about what happens — not the
counts, not the destinations, not the reasons recorded for them — and that test
is the tripwire. It will fail, loudly, on the first change that makes the
ordering matter.

The measurement it rests on is worth one sentence, because it is the kind of
null that is easy to under-report: reversing the tie-break leaves the census's
felt-state traces bit-for-bit identical. The whole design stands on creatures
not watching each other, and that is now a fact with a test attached rather than
an assumption with a comment attached.

## The shape of the error, three more times

The campaign's own working produced the same failure it had just diagnosed,
three times, and all three are recorded because the pattern is worth more than
the fix.

Each time, a claim about the code was made from recollection rather than by
reading the code: a defect reported in a draft that had never been written to
disk; a prediction of what a search command would print, which was wrong because
the searched string also appears in prose; a list attributed to a source comment
that has never contained it. None caused damage. Two were caught by the agents
carrying out the work, which noticed that the instruction did not match the
file and said so instead of following it.

The diagnosed defect was a tolerance sized for the wrong quantity — a check
that looked correct and answered a neighbouring question. So were all three of
these. It is the same error at a different altitude, and the campaign that
found it in the code was committing it in its own prose while doing so.

*Stage 1 of three. The retype that this defect was blocking — the layer's
fractional days, which turn out to have been innocent of the inversion they
were blamed for — follows in stages 2 and 3.*
