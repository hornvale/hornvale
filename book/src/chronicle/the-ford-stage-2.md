# The Ford, stage 2

[Stage one](./the-ford.md) stopped rivers being a hundred and ten kilometres
wide by changing what carries them: a river became a polyline with a
discharge-derived width, evaluated as a function of position at any depth,
instead of a cell painted with a water class. It moved no consumer. A room in
the middle of the useful room scale — twelve refinement levels below the
canonical grid, twenty-seven metres across — still reported the water of a cell
four thousand times its width, because nothing at room scale had been taught to
ask the network anything.

This stage teaches it to ask — and the interesting decision is what a room is
allowed to *say back*.

**Two room scales appear in what follows, and they are not the same room.**
Twenty-seven metres sits in the *middle of the useful room scale* — L18,
against a useful range of about L16–20, and twelve refinement levels below a
level-6 grid. It is not the floor of the addressing, which goes considerably
deeper: `RoomAddr` packs a path of up to `MAX_DEPTH = 29` digits, twenty-three
levels below the grid, at which point an edge is measured in centimetres. The
room a walker actually stands in sits **six** levels below the grid, not
twelve: an edge of `2.71e-4` radians, about 1.7 kilometres, since each level
halves an edge and six halvings take a hundred-and-ten-kilometre cell to that.
Every measurement below is taken at that walking depth, including the one that
matters most — the crossability criterion's "one room edge" is a step of
roughly 1.7 km, not
of twenty-seven metres.

## The chart, not the verdict

The obvious move was to redefine the room's water field: replace the cell's
class with a transverse class of its own, channel or bank or floodplain or
terrace, and mint a new schema for the change. That is what stage one said
would happen.

It is the wrong shape, and a nautical chart says why. A chart carries
soundings, contours and a stated datum. It does not carry a boolean called
*safe*, because safety is a relation between the water and a particular hull:
the mariner sets a safety contour from their own draught, and two vessels read
one chart to two different answers. A chart that stored *safe* would have
chosen one draught for everybody, silently, at the moment of printing.

So the keystone of this stage is a claim about records rather than about
water:

> A document stores the measured quantity and a legend for reading it. It does
> not store one consumer's classification of that quantity.

A room now carries the signed angular distance to the nearest channel, and the
four band edges that apply at that spot. A walker bands them one way, a
bridge-builder another, a settlement model a third. The ordinal is recovered
by a function anyone can call; it is a convenience, not a stored opinion.

What makes this load-bearing rather than tasteful is time. Water is static
today, but the band edges derive from discharge, so under a seasonality this
campaign deliberately leaves open they move with the flood: the same room is
bank in one season and channel in another **without moving**. A stored
distance stays true across that. A stored class goes stale the instant
discharge varies — it would quietly break the one extension the design was
written to protect.

## Left of downstream

Fordability is a sign change, so the distance has to be signed, and a sign is
only as durable as its referent. Stage one's sign was relative to the winning
polyline's direction of travel, and that direction is an artefact of the order
runs were built in — a quantity that renumbers silently if construction ever
changes, and must therefore never be written down.

Hydrology has already solved this. The banks of a river are **left and right
facing downstream**, and downstream is recoverable from the retained downhill
graph, which is fixed at genesis and stable across builds. The sign now means
what a person standing on a bank would mean by it.

One structural caveat belongs in the record rather than in a footnote: the
referent is guaranteed by **assertion, not by construction**. Nothing in the
shape of the data prevents a future change from reversing a run; a single
test — the one that checks each polyline's vertex order against the downhill
graph — catches it if anything does. That is the right trade, since the
alternative is duplicated geometry carried at runtime forever, but it is a
weaker kind of guarantee than a type, and the next person to edit run
construction should know which kind they are relying on.

## Three keys, and no new schema

The room document gains `channel_distance`, `channel_bands` and `resolution`,
appended after the existing fields, and keeps its `locale/room/v2` tag. A
document built before these keys existed is byte-identical up to the point
where they start.

That claim was checked rather than asserted. A character-level comparison of
all seven regenerated artifacts across the change reports **nineteen
insertions, zero deletions, zero replacements**, every insertion one of the
three new keys — including the backslash-escaped copies that live as JSON
*strings* inside a possession session's record of what it has seen. Nothing
that already existed moved by a byte.

The third key is the least obvious and possibly the most useful. A room at
walking depth sits six refinement levels below the canonical grid, so a field
decided per grid cell is necessarily identical across all four thousand and
ninety-six rooms in that cell — and across some sixteen million of them at the
twenty-seven-metre depth. Now that the same document also carries a
channel reading, it holds fields at *three* different grains at once, and a
reader who guesses which is which will invent a contradiction. So the document
declares it: which fields were decided at grid resolution, which at channel
resolution, and — deliberately — which are neither, because they are the
naming of the place rather than a measurement of it.

This is the third instance of one move in as many campaigns: a projection that
declares what it does not carry, an ordinal that ships its own legend, and now
a room that says which model decided each of its readings. All three are the
same act — *a document declaring the terms on which it may be read* — and
declaring a resolution is the finished answer for the fields it names, not a
step toward refining them.

## A ford is a property of a step

Fordability cannot be a field on a room, and the reason is almost a definition.
A ford is a sign change, and a sign needs two positions to change between. So
crossing is a question about a **pair** of rooms: what does this step do to the
water between them?

The state machine of a walker makes the shape exact. The walker is on the left
bank, in the channel, or on the right bank. The transition left → right
*without an intervening channel room* is possible only when the channel is
narrower than one step — which is precisely what it means for a place to be
crossable. That is the cleanest statement of what a ford is that this campaign
produced, and it also disqualified the first version of the measurement: a
population of "adjacent pairs whose sign differs" is not a set of candidate
crossings, it **is** the ford set, and would have measured one by construction.

A sign change alone is not a crossing either, and the world is emphatic about
it. Signed distance against many open arcs flips wherever the nearest-arc
field is discontinuous and there is no water: beyond every river's source,
beyond every mouth, and along the bisector between two arcs that meet. Those
flips are a fact about the polyline soup, not about rivers. Two measured
properties decide the gate: the flip beyond an endpoint is a **ray, not a
place** — the same flips appear at every probe radius, so no distance
threshold removes it — and the confluence bisector flip lands *inside* the
terrace, so "not dry" does not exclude it either. A crossing therefore
requires the sign to differ **and** at least one of the two rooms to stand
inside its own channel or bank edge. A test now holds a dry-land sign change
in place and asserts the gate excludes it.

A third requirement joined those two at the pre-merge review, and it is the
one a reader is least likely to anticipate: **the two rooms must be reading
the same river.** Each room's distance is measured against whichever polyline
is nearest to *it*, and the sign is expressed in that polyline's own frame —
left of the trunk and right of the tributary are not opposite sides of
anything. A pair straddling two different rivers therefore shows a sign change
whose comparison is meaningless, and the gate would have gone on to price the
step against whichever reach happened to win. On seed 42 at walking depth,
five of the hundred and seventy-one sign-changing steps in the measurement's
own population are such pairs, and every one of them was being counted as a
crossing.

The repair costs something, and the cost is worth stating: what the third
clause establishes is that the two signs *cannot be compared*, not that no
water lies between the rooms. At a confluence — precisely where these pairs
gather — a real crossing whose two rooms happen to select the tributary and
the trunk is now refused. That is a trade rather than a free correction. A
confidently wrong verdict is the worse failure, and a reading knows only its
own winning line, so it cannot tell the two cases apart; distinguishing them
would take a query that reads the network's join topology rather than a
looser gate.

## What was predicted, and what was measured

| | prediction | result |
|---|---|---|
| the sign is a pure function of the seed | 100% agreement | 100% |
| appending is byte-clean | 100% of rooms | 19 insertions, no other change |
| the ordinal is reproducible from what is stored | 100% of rooms | 100%, from the serialized document |
| fords exist and are not everywhere | fraction in [0.10, 0.70] | **not resolved** |

The first three are green, and two of them are worth a sentence about what they
do *not* say.

The purity result is blind to the failure the bank convention exists to
prevent. Both arms of the check build the same seed at the same commit, and
building is already known to be deterministic, so the two networks are
structurally identical and any pure function of them agrees bit-for-bit —
including a function that reads the build order. Reversing every run in the
world leaves the check green. What it establishes is purity, which is worth
having; durability is carried by the vertex-order assertion described above,
the only one whose reference comes from outside the object being checked.

The reproducibility result was narrowed on purpose. A room's band edges come
from the same call that measured its distance, so an in-memory recomputation
would agree by construction and prove nothing. The check therefore recomputes
from the **serialized, quantized** document, which tests something that can
genuinely fail: that the record is self-sufficient, and that rounding to eight
significant digits at the emit boundary never moves a room across a band edge.
It does not. The nearest room to an edge sits just under one part in a
thousand away in relative terms (9.8e-4) — five orders of magnitude clear of
the perturbation — so
this is a comfortable margin rather than a near miss.

## The fourth prediction, and why its number is not a verdict

The campaign's namesake hypothesis was that a useful fraction of the network
offers a crossable profile: between 10% and 70% of transects, on the reasoning
that near zero the walk is walled and near one crossing carries no meaning.
Stage one could not score it because its criterion contained an unstated knob
that slid the answer across the whole interval. This stage stated the
criterion — a channel is crossable where its full width is less than one room
edge at walking depth and its discharge is below the threshold at which
terrain already calls a reach a waterfall — and derived it from the traversal
unit rather than from the water, since a room edge is the granularity at which
a person moves and "narrower than one step" is what crossing means to the
thing doing the crossing.

That is a **late freeze**: chosen with stage one's measurements already in
hand, and carrying less evidential weight than a prediction made before them.
What happened next spent the remainder.

Four instruments produced four readings — **0.0308**, **0.2170**, **0.7059**
and **0.3372** — and **all three instrument changes were made after seeing a
number**. Four instruments have three transitions between them; none of them
was clean. The first reinterpreted the probe separation, after the
reading fell outside the interval, from one room edge per probe to one room
edge between the pair. The second added a requirement that the pair actually
be adjacent on the mesh, which exposed the largest defect of the stage:
**three hundred and seven of three hundred and forty-one pairs were not
adjacent at all**, so the instrument's claim to be measuring a walker's step
was false for ninety per cent of its population, and the surviving thirty-four
read **0.7059 — outside the interval, a falsifying reading**. The third rebuilt
the transect so that adjacency is structural: each is now constructed from a
channel-bearing room and the three mesh steps out of it, so the drop causes
that produced the earlier populations cannot arise, and every sampled vertex
yields a transect. That rebuild was adopted directly after the falsifying
reading.

Two things are true of that sequence and neither rescues it. Every change was
forced by a defect in the previous instrument rather than by the number it
produced — the first made the probe unit agree with the criterion's own unit
and *added* a constraint rather than removing one — and the one genuinely free
parameter of the final rule was written down before it ran. The conclusion
stands anyway:

> The fourth hypothesis was not tested under preregistration and is not
> resolved. 0.3372 at one room edge at walking depth is a measurement of the
> world at a stated step length, not a confirmation of the [0.10, 0.70]
> interval.

The check that remains in the suite is a **witness**: it pins today's reading
and reddens if the world moves under it. It is not a hypothesis test, and the
number should not be quoted as one.

## What the fraction is actually measuring

The attribution is more interesting than the fraction, and it says the
criterion is doing less work than its name suggests.

At walking depth on seed 42, of 341 transects: **115 fordable, 8 impassable,
218 not a crossing at all**. But the crossability criterion's two clauses hold
on **324 of the 341** — ninety-five per cent. Almost all of the gap between
95% and 34% is the crossing gate, not the criterion: where the channel is
wider than every step available, both rooms sit on the same bank and the
geometry refuses before crossability is ever consulted.

The width clause, meanwhile, is **inert on this world** — it holds at 341 of
341 vertices. The widest full channel is 1.89e-4 radians against a walk-depth
room edge of 2.71e-4 — roughly 1.2 km of water against a 1.7 km step, which is
why nothing is ever too wide here — so a conjunction that reads like two
independent tests is a
single-clause criterion wearing a conjunction's clothes, and the discharge half
does all the discrimination. It is not inert in principle: three refinement
levels below walking depth the step shrinks enough for the width clause to
decide the answer on its own, and a control exercises it there, holding the
water constant and turning fordable into impassable on step length alone.

The reading is a snapshot at fixed discharge in the other direction too.
Raising the flow widens the channel and closes the ford, so none of these
numbers is a standing fact about a place.

## A room can now say two things at once

One consequence of putting the grid's answer and the network's answer in the
same document is that the document can contradict itself, and on seed 42 it
does. One room reports its water as *river* — the grid cell it sits in is a
river cell — while its channel distance is 0.0509 radians, roughly twenty times
its own outermost band edge and nearly three canonical cell widths from any
channel.

The two halves were already disagreeing; stage one measured the disagreement
as thirty-nine of seven hundred river cells carrying no polyline at all,
because a river short enough to occupy a single cell never becomes a run. What
changed is that both halves now travel in one record, where a reader can see
them side by side. That is an argument for the design rather than against it —
a contradiction that is visible is a contradiction that can be repaired — but
it is unrepaired today, and it is committed in the walking-band session
snapshots. Either a consumer needs a stated rule for which half wins, or those
cells need lines.

## Where this stops

The room can now be asked where it stands relative to the water and what a
step out of it does to a crossing, and it answers from stored quantities a
reader can re-band for their own question. No consumer has changed its
behaviour: gallery forest is still drawn by dice rather than by bank, the
scene schemas still carry nothing about channels, and the settlement model
still reads the same coarse availability it always did. The band edges the
room publishes are still edges of a static world.

What the stage leaves behind, besides three keys and a traversal query, is a
sharper statement of what remains unknown. The fordable fraction of Hornvale's
rivers is a number this campaign can measure and cannot yet claim to have
predicted.
