# The Benchmark

A bug report said a tropical forest was 2936 metres under water. The world was
right; the sentence was wrong; and the reason it was wrong had been sitting in
a doc comment, correctly stated, for two campaigns.

A surveyor's benchmark is a permanent mark of *known* elevation — the physical
admission that a height means nothing until you say what it is a height above.
This campaign put that admission in the type system.

## Sea level is a value, not a zero

Hornvale's elevation field is measured from an **isostatic reference datum**:
0 m is a reference-thickness crust floating at equilibrium, and it means the
same physical thing on every world the generator can produce. Sea level is not
0 m. It is *derived per world* — the elevation at the percentile that satisfies
the pinned ocean fraction — and on seed 42 it sits at **−2936.17 m**.

So the reported room, reading −2936.38 m, is 0.2 m below sea level. It is a
shoreline. Every one of that world's 40,962 cells agrees: land biomes below sea
level, zero; marine biomes above it, zero. There was never a physics defect.

The project already knew this. The Confidence Gradient records, in the middle
of an unrelated argument about crust generation, that a read-only probe once
found "sea level sitting in the abyssal plain." The fact was measured, written
down, and published — and it still did not reach the renderer, because nothing
connected a fact about the terrain field to a function that formats a number.

## The distribution of the mistake

The campaign's most useful finding was where the error was, not what it was.

Every consumer that *computed* with elevation subtracted sea level first, and
every one of them was correct — the substrate proxy, the regime budget, the
biome classifier, the terrain renderer, the lab's whole metric suite. Every
consumer that *showed a number to a person* did not, and every one of them was
wrong.

That is not a coincidence about who wrote which file. A consumer that computes
has to decide what the number means in order to use it; the question is forced.
A consumer that displays can pass the number through without ever asking. The
defect lived exactly where the question was never posed, which is why the fix
could not be a warning about a thing not written.

The consequence was not cosmetic. `scene/surrounds`'s relief bands —
`abyss / shelf / lowland / upland / highland / alpine` — were computed against
the raw reading, so on seed 42 **8162 of 11,066 land cells classified as
`shelf`**, a marine band, and the entire planet held exactly **one** `alpine`
cell instead of 4168. The committed gallery golden's relief histogram was
`{shelf: 31}`: every cell, wrong, published. And the reference page tabulated
the boundaries as contract, so the wrong datum was not merely shipped but
ratified.

## The type the deferral owed

Decision 0044's units doctrine already required that **interval types carry
their datum**. `ReferenceElevation` does — "Reference" *is* the datum. Its
sibling was missing, and the source said why:

> A local intermediate (lapse rate, depth shading) — a height-above-a-datum
> earns its own type only if it crosses a pub boundary.

A stated deferral with a stated trigger. The trigger fired when a later
campaign banded a height into a published schema, and nothing noticed, because
a condition expressed as a property of the code has no watcher. This campaign
is that deferral being collected, not reopened.

## The operator was the wrong shape, and the compiler said so

The design's first form followed `Temperature`/`TempAnomaly` exactly: retype
`Sub for ReferenceElevation` so subtraction yields the new quantity. It is the
shipped precedent, decision 0008's own motivating example, and it is wrong
here.

The compiler produced twenty-one errors, and two settled it. `domains/climate`
computes `elevation.get(cell) - elevation.get(upwind_neighbour)` to get an
orographic rise — a difference between two *places*, with no datum in it
anywhere. Typing the operator's output as a quantity named for sea level would
have made the type system assert something false about every difference not
taken against sea level, and the mechanical repair — inserting `.get()` until
it compiled — would have buried that falsehood rather than surfacing it.

The temperature analogy does not carry. A difference of two temperatures can
only be an anomaly; there is one thing it can mean. A difference of two
elevations is a height above sea level, or a rise between two places, or local
relief detail. **Subtraction is polymorphic in meaning, so the meaning has to
be named at the call** — which is precisely what 0008 says when it asks for
"validating constructors and *named conversions*."

`ReferenceElevation::above(datum) -> SeaLevelHeight`. The workspace blast
radius went from twenty-one compile errors to zero, and enforcement was
untouched, because enforcement never lived in the operator: it lives in the
*parameter type* of the function that consumes a height. `relief_band` cannot
be handed a raw reading, and that is the whole mechanism.

## What a walker is told

The room the report came from now reads:

```
> examine tropical seasonal forest
24.2 °C the year round, moisture 0.70, at sea level.
```

Not "0 m below sea level", which is what the first fix produced by branching on
the raw sign while printing whole metres. A place within half a metre of the
datum is at sea level, and saying so is also the honest treatment of a
shoreline whose height is a three-corner blend and whose sign is not meaningful
at that precision.

`scene/surrounds` is minted **v2**: the bands measure height, the document
carries `sea_level_m` so a consumer can re-derive them, and each cell carries
`height_asl_m` beside the raw reading. The absence of the datum from v1 was the
sharpest part of the defect — it left the one scene kind whose values were
wrong also the one kind a client could not correct. Its two sibling schemas had
published `sea_level_m` all along.

## A guard that was worthless, and how that was found

The self-consistency test written for this — *the emitted band equals the band
of the emitted height* — passed with the defect fully reintroduced.

`height_asl_m` is emitted only on the observer's own cell, so the sweep
inspected exactly one cell; and that cell, the flagship room, sits at −0.2 m
over a −2936.4 m reading, which the band function maps to `shelf` **both ways**.
The single cell the guard checked was the single cell on the planet where the
bug is invisible. It was caught only by putting the bug back and watching
twenty tests stay green.

The replacement derives its probe — the lowest-CellId land cell whose raw
reading is still negative, a description fitting 8162 of the world's 11,066
land cells — and, more importantly, asserts that the two readings *disagree*
before asserting which one the emitter chose. A test that has quietly stopped
discriminating reports green exactly like a test that passes. The anti-vacuity
assertion is the part worth copying.

## What was not done

Banded correctly, 72.7% of seed 42's land sits above 1000 m and 37.7% above
2500 m. That may be right for a bimodal hypsometry or may mean the boundaries
were drawn against Earth's distribution. The boundaries were deliberately left
untouched, so that the datum correction stayed measurable on its own; deciding
whether they suit this world is its own measurement.

The waiver the doctrine expected this type to retire was retired **zero times
out of five**. Each remaining site carries a genuine absolute reading, kept
deliberately beside the new height, because the two answer different questions.
The doctrine now says so, and says the forecast was wrong as a blanket claim:
"elevation" was never one convention to retire. That is Task 2's finding again,
one level up.
