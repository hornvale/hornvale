# The Holdfast

**August 2026 · outcome: merged — a fifth of a world-generation test
disappeared by noticing that three of four measurements could not change
the answer**

## What was attempted

The campaign began as an accusation: the test suite had blown up again,
apparently by a factor of four, not long after a previous campaign had
halved it. The obvious move was to profile and find the regression.

The profile found something else, and the first useful result of this
campaign was learning that the accusation was mostly false.

## What the clock actually said

A run's wall time is its CPU time divided by the parallelism it achieved,
and the project's own timing ledger records all three. Reading them apart
separates *more work* from *more contention* — and on the machine where
the complaint originated they said opposite things. Between its best day
and its worst, the gate's wall time had risen by a factor of 3.35. But
its CPU time had risen only 1.39×, while its achieved parallelism had
fallen from 10.46 to 4.35 — a factor of 2.40 in the wrong direction, and
`1.39 × 2.40 = 3.35` closes the arithmetic exactly.

Most of the "regression" was a machine that had stopped being quiet.

The same suite measured on the project's canonical Linux box, idle,
achieved a parallelism of 33.67 across forty cores and finished in 350
seconds. Nothing was wrong with the tests. Something was wrong with the
room they were being run in.

That left a real but smaller quarry: the 1.39×.

## A quarter of the suite is in the transcendentals

Sampling the whole workspace — every test binary, two million samples —
gave a distribution with an uncomfortable shape. A quarter of all
execution sat inside `libm`, the pure-Rust implementation of the
elementary functions. `exp` alone accounted for a seventh of the entire
suite.

That is not a defect. Hornvale routes every transcendental through a
software implementation deliberately, because the platform's own `exp`
differs in the final bit between one operating system and another, and a
world that generates differently on a different machine is not a world
but a rumour. The project bought bit-identity and paid for it in speed,
knowingly.

What had never been measured was the size of the bill, and the bill is
not really a `libm` bill at all. **The cost of a function is its price
multiplied by how often you call it, and only the second factor was
ours.**

## Four measurements to find one minimum

Following the calls upward reached a single small function. A species'
suitability for a place is Liebig's law of the minimum: of temperature,
moisture, insolation and elevation, the *worst* axis is the one that
limits, exactly as a crop is limited by whichever nutrient is scarcest
rather than by the average. Each axis is scored by a Gaussian — one call
to `exp` apiece — and the four results are reduced by taking the
smallest.

All four were computed before the smallest was chosen. Across a
forty-thousand-cell globe, every species, and twenty-five climatic eras,
that is tens of millions of exponentials evaluated to answer a question
that frequently only one of them could answer.

Three of the four axes are *floored*: a species' sovereignty — the
buffering that mass and magical potency buy it — sets a value beneath
which those axes cannot score, however hostile the place. Elevation
alone is passed no floor and can score all the way to zero.

So elevation is the only axis that can dip below the others, and when it
does, it is already the minimum. The remaining three exponentials cannot
change the answer.

Evaluating elevation first and returning immediately when it falls at or
below the floor removes three of four `exp` calls on every cell where
elevation binds. The function's own documentation, from an earlier
campaign that measured it while investigating something else, records how
often that is: **elevation binds on one hundred per cent of land for
goblin, gnoll and human.** The fast path is not the exception. It is the
common case, and on those peoples the other three curves were determining
nothing at all.

Measured on the quiet box, interleaved so that neither arm could be
flattered by drift: the test fell from 39.36 seconds to 31.58, a fifth of
it gone, with `exp` down 54.5% and the response evaluation down 60.3%.
Every world generates the same bytes it did before — a forty-seed sweep
agrees hash for hash, and every committed artifact regenerates unchanged.

## The correction that mattered more than the change

The campaign's original plan had a second half, and it was wrong in a way
worth recording, because it would not have failed loudly.

The intent was to hoist the axes that do not vary between eras — to stop
recomputing, twenty-five times, a number that is the same on all
twenty-five occasions. Two of the four axes qualify. The plan named
insolation and *elevation*, on the strength of a comment stating that
elevation does not change across eras.

The comment was true and described a different quantity. Elevation as
*relief* is indeed fixed. But the axis the suitability function reads is
height above **sea level**, and sea level is exactly what an ice age
moves. Freezing it would have silently abolished glacial low-stands —
stopped the sea from falling and exposing continental shelf as dry land —
and the resulting worlds would have been wrong in a way no test asserted
and no byte-comparison against the *changed* code could reveal.

The count survived the correction; the attribution did not. The two
era-invariant axes are insolation and **moisture**.

The general form of the error is worth more than the instance: a doc
comment describes the field it is attached to, not the field some other
structure derives from it, and the derivation is where the era crept back
in.

## What was not done, and why

The hoist was measured rather than assumed, and by the time the
short-circuit had shipped it was no longer worth doing. The two changes
overlap: whenever elevation binds, the short-circuit *already* skips the
two axes the hoist would have cached. Solving for the fire rate from the
measured shift in `exp` gives roughly 73%, leaving the hoist only the
remaining quarter of invocations and about three per cent of the runtime —
against a refactor of the very function a future campaign will restructure
wholesale.

So it was folded forward rather than forced through. **Two optimisations
along the same path do not add up**, and the second one's value has to be
recomputed after the first lands rather than carried over from the
estimate that justified it.
