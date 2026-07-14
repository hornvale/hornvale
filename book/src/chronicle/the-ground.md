# The Ground

**July 2026 · outcome: merged — the tectonic globe gains a lithology and
pedology substrate: what rock is underfoot, what soil sits on it, and what
the ground does with water, all without moving a single existing byte**

## What was attempted

Crust answered *how high* the land stands; nothing in the world yet answered
*what it is made of*. A world could report its elevation, its plates, its
biomes, and still had no opinion on whether a mountain range was granite or
schist, whether a lowland farmed well or thin over sand, or whether a cave
system was even physically plausible where one might be placed. This
campaign closes that gap — a pure classification layer over fields the
tectonic globe already computes (crust age and thickness, boundary type and
distance, plate motion, drainage), read once and shared by every future
consumer: settlement's workability and fertility, an eventual ore economy,
the walk's underfoot texture, ecology's parent chemistry. Because every
output is *derived*, not drawn, the campaign changes no committed byte —
elevation, drainage, climate, every existing fact stays exactly what it was.

## The material buffer, not a rock-class enum

The load-bearing decision, reached before any taxonomy was drafted: do not
emit a rock class as the primitive. A single class enum is one codebook
compressing a continuous material state, and different consumers want
different codebooks out of the same ground — the mistake a shared enum would
repeat is the same one Hornvale's fields-before-classes doctrine already
guards against everywhere else. Instead each cell carries a small **material
buffer** — a felsic/mafic index, grain size, hardness, carbonate content,
metamorphic grade, porosity, an active/passive/interior margin polarity, a
soil-depth accumulation, a shallow two-layer cover-over-basement column, and
a magical-saturation slot that is identically zero under this campaign's
inert metaphysics. That last axis is a deliberate hook, not a feature: a
future metaphysics-gated overlay (ley-lines, magical materials, mythic
provenance) can refine this same buffer later without an epoch, because the
mundane substrate this campaign ships already *is* that overlay's floor.

Every consumer-facing reading is a pure projection over the buffer, plus
climate where the projection needs it — never a second source of truth.

## The taxonomy, and what water does to it

The fine rock taxonomy — eighteen classes from granite and gabbro through
reef limestone, coal, and alluvium, organized by genesis rather than
listed flat — is the buffer's most legible projection, each class earning
its place by meaning something different to some future consumer (ironstone
is also iron ore; coal ties soil, climate, and deep time together). Nine
soil orders sit alongside it, climate-coupled rather than parent-only:
laterite, podzol, chernozem, aridisol, loam, andosol, leptosol, histosol,
and gley, each carrying a small **fertility vector** — grain, moisture, and
depth suitability — rather than a single scalar, so a later agriculture
consumer can match crop needs the way settlement placement already matches
species niches to land.

Porosity and drainage cross to give a hydrogeology reading — aquifer,
aquitard, spring, or karst — and carbonate crossed with porosity and
drainage gives a continuous cave/karst void-proneness, the one place this
substrate touches the underworld's vertical axis a later campaign will
build out. A walk-facing appearance vector (albedo, hue, grain, hardness
underfoot) and a mineral-prospectivity field round out the projection set —
prospectivity is deliberately a probability field, not point ore bodies;
those need their own draw and are named as a later campaign's down payment,
not built here.

## Surfacing it

None of this substrate was worth building if a reader never saw it. The
almanac gained a "The Ground" section — the dominant rock and soil order
over a world's land, plus notable formations (karst country where karst
hydrology clears a threshold share of land, salt flats wherever an
endorheic basin sits on evaporite, volcanic soils where andosol runs high)
— read alongside [The Land of Seed 42](../gallery/elevation-seed-42.md) in
the gallery. A [lithology map](../gallery/lithology-seed-42.png) joins the
elevation map as a second rendered view of the same globe. Five new census
metrics — dominant rock, dominant soil order, karst fraction, aquifer
fraction, and fertile-land fraction — auto-enroll in the project's standing
1,000-seed census the way every metric does, so this substrate is checkable
at population scale, not just readable at one seed.

## What this deliberately is not

The scope fence was drawn before the first line of code: no consumer gets
rewired to actually read this field yet — settlement, ecology, and the walk
each get a clean substrate to query later, not a forced integration now.
Ore point-deposits (veins, lenses, discrete bodies) are a different object
from an areal probability field and need their own draw; prospectivity is
their down payment, not their arrival. The full stratigraphic column stays
a shallow two-layer approximation until a deposition history exists to earn
more depth. Lithology feeding back into the shape of the land — hard rock
standing proud, permeability reshaping drainage — is explicitly Sculpting's
job, the next terrain epoch, which will consume this campaign's hardness
axis directly. And the metaphysics-gated overlay this buffer's reserved
axis makes room for stays banked, not built, until a magic system exists to
gate it.

## The road ahead

Sculpting inherits a hardness field it did not have to invent. A future
deposits campaign inherits a prospectivity field instead of starting from
nothing. A future underworld campaign inherits real karst instead of a
placeholder. And whenever the project's metaphysics gate opens, the
buffer's reserved axis and the taxonomy's declared openness mean a magical
substrate can layer on top of this one without moving a byte the mundane
world already committed to — the same coarse-constrains-fine discipline
that has held since the tectonic globe first shipped, applied one layer
deeper.
