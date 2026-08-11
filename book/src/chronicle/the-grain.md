# The Grain

This campaign set out to make the finest layer of the world vary. It shipped
three things it had not originally planned, reverted the change it was named for,
and its most durable output is a test for a mechanism that does not exist.

That is not a story of a campaign going wrong. The revert was the finding. What
follows is the reasoning, because the reasoning is what generalizes: it turns out
that whether a field's values are *ordered* decides how that field may be
refined, and nothing in the project had noticed.

## A client with nothing local to colour

The occasion was a different campaign. A terminal client renders the world as a
character grid, and a grid has an unused channel — colour — so the plan was to
put biome into it. The plan died on its first measurement.

A walk through the world happens six refinement levels below the canonical grid,
the coarse mesh over which climate and terrain are actually solved. Six levels
means `4^6 = 4096` rooms share one grid cell, and the neighbourhood a walking
observer sees — thirty-one rooms — is about **one hundred and thirty-second of a
single cell**. Measured on the flagship possession of the project's canonical
seed, across those thirty-one adjacent rooms:

| field | distinct values across 31 rooms |
|---|---|
| biome | **1** — tropical rainforest, all thirty-one |
| water | **1** — river, all thirty-one |
| colour | **1** — the same byte triple, all thirty-one |
| relief | 2 of 6 bands |
| temperature, moisture, elevation, height above sea level, regime | **`null` on 30 of 31** |

A colour channel carrying biome would have been one flat wash. The client's own
map already said as much, out loud: its render reported *0 tinted, 31 withheld*
one line beneath prose reading *"Tropical rainforest — buttressed canopy, shaded,
in a hollow."* The chart said the observer stood in open water. The prose said a
rainforest hollow. Both read the same place, and the prose was right.

So the client campaign was deferred and this one opened in its place, with a
brief that looked simple: the fine layer does not vary; make it vary.

## The data existed, and a comment said otherwise

The first thing measured was the second row of that table's bottom line — the
five fields that are `null` on thirty of thirty-one cells. Their doc comments
read *"fine grain, `null` when coarse."*

That sentence is a claim about the model's resolution, and it was false. The
producer computes all five for **every** cell in its build loop and then discards
them on all but the observer's own. The gate is not grain; it is *this cell is not
where you are standing*. The values existed the whole time.

The distinction is not pedantry, and its cost is measurable: the client campaign
had read those comments and concluded that the temperature-by-moisture surface it
wanted was uncomputable below grid resolution, when it was merely unsent. A whole
design was built around the opposite of the truth by a reader doing exactly what a
careful reader should do — believing the comment next to the code.

What makes this worth a ratified decision rather than a fix is the shape of the
error. The comments were not vague. They were *specific about the wrong axis*. A
reader has no way to distinguish a description of physics from a description of
plumbing when both are phrased as physics. So: a field that can be absent must
name the condition that makes it absent, in that condition's own vocabulary.

Two of those five fields were then measured rather than assumed, and the result
went the other way. Temperature across the neighbourhood spans 0.075 °C; moisture
spans 0.0089. They really are blended per room and really are thrown away — and
they are also flat to four decimal places, so emitting them would ship real and
useless variation. The doc comment described a mechanism; nobody had checked the
magnitude. Being right about a mechanism is not evidence about a size.

## What does vary, and it is not what it looks like

One field on the room did vary, strongly. Every room carries a **micro-field**:
four independent axes in `[-1, 1]` — micro-relief from hollow to rise, aspect
from shaded to sunlit, wetness from dry to wet, and canopy openness from closed
to open — each drawn from the room's own address noise, so a walk through
homogeneous biome still changes underfoot. Openness spans **1.977 of its
available 2.0** within thirty-one adjacent rooms. It reached no consumer at all.

Emitting it costs nothing, because the derivation was already happening on every
cell of every chart and being dropped. So the chart gained a `micro` object on
every cell — not optional, on no state, at no radius, deliberately: the field is
a pure function of a room's address and the world seed, so an absent value could
only ever mean *this producer chose not to say*, which is the precise ambiguity
the five gated fields had just demonstrated the cost of.

And then the campaign nearly made its central mistake with it.

The obvious next move was to band the room's water kind from `micro.wetness`.
The evidence looked excellent: rooms with high wetness were described as *"a
stream gully"*, rooms with low wetness as *"buttressed canopy"*. The prose and
the proposed field agreed beautifully.

They agreed because the prose is **rendered from the field**. The descriptor
grammar reads the micro-field before anything else; wetness "predicting" a stream
gully is a definition, not a correlation. The apparent confirmation was a
tautology, discovered one step before it became an implementation.

The deeper problem survives the tautology. The micro-field is address noise. It is
drawn from a seed stream keyed on the room's own address, with **no coupling to
the terrain model whatsoever** — it does not know where the water is, where the
slope faces, or how high the ground stands. It is texture. That is exactly what
makes it valuable to a renderer and exactly what disqualifies it from answering a
question about hydrology, and the schema now says so in as many words, because
this is the one reading of the field that had to be ruled out in writing.

## The centrepiece, built and reverted

The second attempt was better in every respect. Water would be banded from
*blended drainage* — the same three-corner weighted mean the height field already
uses — reusing the existing river threshold and fitting no new constant. It
reused the terrain domain's own classifier rather than minting a second
definition of anything. It was implemented, it passed both of its preregistered
hypotheses, and it passed the commit gate at three thousand three hundred and
fifty tests.

It was reverted. Three reasons, and the third is the one worth keeping.

**It split a documented invariant.** The rule that picks a room's dominant corner
carries its own doc: every categorical field a room reports — biome, water kind,
substrate, and the rock whose reflectance the colour layer reads — names the same
cell, because *"splitting this would let a room be described as granite lowland
and drawn in basalt grey."* Banding water from a blend while the other three still
take the dominant corner **is** that split. The ten flagship rooms the change
"fixed" went from *(river, shelf)* to *(ocean, tropical rainforest)*: the
contradiction did not vanish, it moved onto the invariant. And the invariant had
no test, so it broke in silence with three thousand tests green.

**It broke a calibrated coarse statistic.** The river threshold's own doc records
that its value keeps rivers the minority landform, about 6.7% of the seed's land.
After the change, fresh water at walking depth shrank **29%** — sixty-six river
rooms to forty-seven over a four-thousand-point sweep. Downstream, thirst-driven
fauna movement roughly halved, and a hundred and two lines of committed behaviour
trace moved with it. The constitution's *coarse constrains fine* holds that higher
fidelity refines and never contradicts lower, so a refinement that shrinks a
calibrated coarse quantity is disallowed however good its local behaviour looks.

**The bias was structural, predictable, and predicted.** A threshold is maximally
nonlinear, so classifying a blend is not the area-weighted vote of classifying the
corners. Nearest-corner assignment is a *partition*, and a partition conserves
area by construction; a threshold on a blend does not, and what it loses is
whichever category sits in the thin tails of the underlay. Two hundred and
sixty-seven of the seed's seven hundred river cells sit just above the threshold,
in drainage `[15, 20)`. The measured loss landed exactly there. A measurement
confirming a *predicted* systematic bias is the strongest form of this argument,
and it is why no amount of retuning rescues the mechanism.

## Ordinal, nominal, and the line nobody had drawn

The campaign's original thesis was that the producer used four different
inheritance policies in adjacent lines of one struct literal — inherit the biome,
inherit the water, band the relief from a blend, blend four fields and throw them
away — and that the real rule was *band from the blend, don't inherit a category*,
with water on the wrong side of it.

The documented rule was *categorical versus continuous*, and that rule was
already wrong: relief is categorical too, six named bands, and it bands from a
blend. So far so good. The thesis's error is subtler and only the revert exposed
it.

| | relief | water |
|---|---|---|
| values | ordered — abyss, shelf, lowland, upland, highland, alpine | unordered — ocean, river, salt basin, dry land |
| a blend of the underlay moves a value | at most one band, in the direction the underlay moved | to an arbitrary other category |
| what the operation conserves | the distribution's shape | nothing |

**Relief is ordinal. Water is nominal.** Banding a blend of an ordinal field's
underlay is a bounded, order-preserving error; thresholding a blend of a nominal
field's underlay silently *deletes a category*. The real line is neither
categorical-versus-continuous nor banded-versus-inherited. It is whether the
values are ordered.

Biome stays on the partition for a further reason that is worth keeping separate
so it is not mistaken for a special case: averaging *desert* and *tundra* names no
biome at all. The classification is a lookup over climatic bins, not a ramp, so
there is nothing to band even in principle. Flags — endorheic, terminal sink —
stay for a third reason: a weighted mean of two booleans names nothing.

## The defect was a misread view

One thing remained to explain: if the existing mechanism was correct, why did the
chart look so wrong?

The dominant corner is resolved **per room**, over that room's own three blend
weights. It is not a value copied down from one cell to all four thousand
rooms inside it. It is *categorical nearest-neighbour interpolation* — which is
the correct method for a nominal field, the one that conserves area by
construction, the one the revert restored.

Its apparent flatness at a radius of four rooms is what nearest-neighbour
interpolation looks like when the view is narrower than the interpolation
stencil. That is a fact about the view, not a defect in the field. Which means
the campaign's founding diagnosis — *the fine layer inherits a value from a cell
four thousand times too big* — was describing the picture correctly and the
mechanism incorrectly, and the word that carried the error was **"inherited"**,
sitting in the source where every reader would find it.

The chart's uniformity was never the defect. The chart's *silence about its own
uniformity* was.

## Disclosure as the deliverable

So the honest fix moved to the layer that owned the confusion. A document that
can be built below the resolution of one of its own fields should say which
fields those are.

The chart now carries a resolution disclosure: the canonical grid's level, how
many levels below it this chart sits, and the names of the fields decided at grid
resolution and therefore constant beneath it — biome, colour, and water. A
consumer can caption the difference (*"grid resolution — every room here reads
one coarse cell"*) instead of inferring a contradiction, and the next reader does
not spend a campaign on the diagnosis this one spent itself on.

The project already had this move and had not generalized it. The colour block on
the same schema states what a projection to three screen channels does *not*
carry — *"the red–green axis is not carried"* — so a two-chromatic-channel eye
emitting triples whose red and green components are equal by construction reads
as honest rather than broken. Spatial resolution is the same disclosure about a
different axis.

The list earns its meaning through its exclusions. Relief is absent because it
bands a blend and genuinely varies. The micro-field is absent for the opposite
reason: it is the finest thing the document carries and was never grid-resolution
in the first place. Two exclusions with opposite justifications is the test of
whether such a list says anything at all.

Sub-cell water goes where it belongs: to a hydrology model with an actual flow
graph, the only mechanism that can put a stream *somewhere in particular* inside
a cell. Naming that closes the question rather than leaving it open.

## Caves, and the mark that is coarser than its rooms

The third change is the smallest and is the same shape as the first: something
the simulation already held that reached no consumer. Caves exist in the terrain
domain and reached no field of the chart, so a client could not draw a cave mouth
and — worse — could not distinguish *no caves here* from *caves are not emitted*.

They are now marks, alongside settlements, and they carry an oddity worth
recording. A cave is a **cell-level** affordance: descending into one resolves the
cave from the cell the observer stands on, so the descent already succeeds from
any room of a cave-bearing cell. Marking every room of that cell is therefore
faithful, and marking one arbitrary room would be the lie. That makes `marks` the
one field on the document whose granularity is *mixed* — one grid-resolution kind
beside two per-room kinds — which is exactly why it is absent from the resolution
disclosure. Listing it either way would misstate the document, and a test now
pins that absence so it reads as a decision rather than an oversight.

## A hypothesis about variation cannot see a broken invariant

Two of this campaign's preregistered hypotheses passed and were retired by the
revert, and the manner of their passing is the campaign's sharpest process
lesson.

The first asked whether room water varies within a canonical cell. The second
asked whether the variation is spatially coherent rather than noise — a real test,
which the rejected address-noise mechanism would have failed by construction. Both
were true. Both were measured. And passing them is what exposed the mechanism as
illegal, because a mechanism can vary locally, cohere spatially, and still destroy
a global quantity.

**No local hypothesis can detect a violated global invariant.** That is
structural rather than an oversight in wording: a statement about how a field
behaves within a neighbourhood is silent about the sum of that field over a world.
So a refinement needs a *conservation* criterion beside its variation criteria —
the coarse quantity it must reproduce when aggregated back up.

Two such criteria now exist as tests. The first is the coupling invariant the
dominant corner had documented and nothing had checked: biome, water, substrate
and the colour layer's rock all resolve to one cell, asserted across a
two-hundred-address sweep. The reverted change is the proof it was breakable in
silence.

The second is the conservation criterion itself: aggregating room-level water back
over a canonical cell reproduces that cell's own water kind. Under a partition
this holds *by construction*, which is precisely why it is cheap and precisely
why it is worth having — it is a tripwire for a future mechanism, not a discovery
about the present one. It asserts the strong form, unanimity rather than mere
plurality, because a plurality-conserving criterion would still permit deleting a
category from a minority of cells and a thin landform is a minority everywhere it
exists.

And it carries a second arm, which turned out to be the load-bearing half. Over
the same rooms the test also computes what the reverted mechanism *would* have
assigned, and asserts that the criterion **rejects** it. The first draft of the
test sampled a small contiguous patch per cell, and that arm found zero
violations: across one hundred and thirty-second of a cell the blend barely moves,
so the scan could not see the thing it was built to catch. Rewritten to fan across
the whole cell, it conserves on all eighty cells and two thousand one hundred and
fifty-one rooms sampled, while the reverted mechanism breaks the aggregate form on
eleven of those cells and unanimity on twenty-seven.

A guard nobody has watched trip is indistinguishable from one that cannot.

## What the campaign was for

Three additive emits shipped: a per-room micro-field on every chart cell, a
resolution disclosure on every chart, and cave mouths as marks. None of them
changed a single existing value; each appends a key or a value to an open
vocabulary, so no schema version was minted and no world's bytes moved except by
gaining something at the end. Two invariants that had been comments became tests.
Five doc comments and a schema chapter stopped saying something false.

The change the campaign was named for is not in the tree. What is in the tree is
the reason it is not: a rule about ordered and unordered values that applies to
every future refinement of every categorical field, arrived at by building the
wrong thing carefully enough to measure why it was wrong.

The client campaign that occasioned all of this is now half unblocked. Its plant
thickness, its slope shading, its local texture — all of that is on the wire and
varies room to room. Its water glyphs are not, and will not be until something
models water at a finer resolution than the grid. That is a better position than
the one it started from, which was believing the data did not exist.
