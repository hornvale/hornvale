# The Pace of Life

**July 2026 · outcome: implementation complete, pending merge — every species
gains a derived life-history profile (basal metabolism, lifespan, maturity,
reproductive tempo) computed from body mass by published scaling laws, with
zero effect on any existing world's bytes**

## What was attempted

A species definition already carried an adult body mass — the multi-species
coexistence work authored it as the down payment its settlement-packing math
needed — but nothing yet asked what that mass predicts about how a creature
*lives*. Real organisms across hugely different lineages fall on a single
fast–slow continuum: heavier bodies run their metabolism more efficiently
per kilogram, live longer, mature later, and reproduce more sparingly, all as
power-law functions of mass with quasi-universal exponents. This campaign
adds that axis as a pure derivation — a life-history reading computed on
demand from a species' existing mass and one small new metabolic-strategy
tag, never stored on the species itself, never drawn from a stream, and
never touched by a seed. Mass stayed exactly where it already lived: the
coexistence stack had already built the kernel's mass type and the authored
per-species weights (kobold 13.6 kg, goblin 18.1 kg, hobgoblin 74.8 kg,
bugbear 132.0 kg — D&D-canon lore figures), and re-authoring it here would
have split one number across two owners for no reason. This layer only
reads it.

## Two laws, and an honest split between discovered and chosen

The scaling *exponents* are the discovered part — Kleiber's law, dating to
1932, holds that basal metabolic rate scales with the ¾ power of mass across
mammals spanning six orders of magnitude; lifespan and age at maturity scale
with the ¼ power, the same "metabolic time" exponent that makes a mouse's
heart and a whale's heart run through roughly the same number of beats in a
lifetime, just at very different tempos. Both exponents are treated as
universal across every species built from ordinary flesh and blood — nothing
this campaign added is free to vary them per species. What *is* chosen is
the absolute scale: the metabolic constant is anchored to a real measured
wattage, but the absolute lifespan and maturity constants are calibrated
worldbuilding — pinned to a single 40 kg reference creature living 60 years
and maturing at 12 — because there is no published fact about how long a
fictional goblinoid ought to live, only a design decision, and the module
says so in its own commentary rather than dressing up a choice as a
discovery.

## The clade coefficient

Mass alone under-determines the picture: a reptile and a mammal of identical
weight do not run the same metabolism, and a single normalizing constant
would quietly average away the difference the roster already needs — the
kobold is reptilian where the three goblinoid peoples are warm-blooded. A
small `MetabolicClass` tag on each species (warm-blooded, cold-blooded, and
two unused seams for a sunlight-eating and a construct-like people still
ahead) selects which normalizing constant a species' curve runs from, while
leaving the curve's *shape* — its exponent — untouched. The cold-blooded
constant does not move one trait in isolation, either: it shifts lifespan,
maturity, and reproductive tempo together, by the same single multiplier, so
a cold-blooded people reads as coherently slow across its whole life
history rather than slow on one axis and ordinary on another.

## Seed 42, concretely

Applied to the current four-species roster, the two laws separate the
goblinoids cleanly by size: the 18.1 kg goblin lives about 49 years, and the
132 kg bugbear, seven times heavier, lives about 81 years — the same curve,
read at two different masses. The kobold is the sharper case. At 13.6 kg it
is the *smallest* of the four, and its cold-blooded metabolism runs at only
about 3 watts — an eighth of what a warm-blooded creature its size would
burn. A metabolism that low would, taken alone, suggest a short and modest
life. Instead the kobold lives about 69 years, longer than the much larger
goblin, because the same clade coefficient that slows its metabolism also
slows its whole life-history clock. The two readings are not in tension —
they are the same coefficient, applied coherently, telling the same story
about a low-throughput creature from two different angles.

## Strictly neutral

Nothing about the world's determinism contract moves. Mass and metabolic
strategy are authored constants, not draws, so there is no new stream and no
new consumption order to preserve; every committed artifact and every world
in the standing thousand-seed census stays byte-for-byte what it was before
this campaign touched the tree. The layer is read-only besides: the almanac
gains a life-history line per species — a pace-of-life headline plus its
concrete lifespan and maturity figures — and the laboratory gains one
metric per trait, reported for the goblin and the kobold so the
warm-blooded/cold-blooded contrast this whole campaign exists to demonstrate
is actually queryable rather than merely described. Nothing reads this
layer back into how a settlement grows, ages, or dies yet — that rewiring,
if it comes, belongs to whichever future campaign wants a demographic
consumer, not this one.

## What this leaves out

The metabolic figures above are *basal*, measured at a reference
temperature — a cold-blooded creature's real, moment-to-moment burn rate
should rise and fall with the climate around it, and that coupling is
named but deliberately not built here, since a species definition has no
business depending on the climate domain. A mortality rate derived from the
inverse of lifespan, a fifth metabolic class for creatures that shift
strategy mid-life, and a per-species override letting a long-lived people
defy the curve entirely (the way real allometry already treats humans as an
outlier) are all left as clearly named seams rather than built speculatively
ahead of a consumer that would use them.

## The road ahead

The life-history reading is a pure function of mass and class, which means
it costs nothing to keep pointwise even as mass itself eventually stops
being a fixed number — a future campaign that lets mass drift over
generational time inherits a life-history curve that already knows how to
follow it, with no change to its own shape. Until then, this campaign hands
off a small, self-contained axis: any future consumer that wants to know
how fast a people's clock runs now has one number to ask.
