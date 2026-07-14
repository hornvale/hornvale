# The Gathering

**July 2026 · outcome: merged — population stops being a number handed to a
place and becomes a field the land itself supports, with settlements read
off it as the places where that support concentrates**

## What was attempted

Every settlement in a Hornvale world, until now, was placed by a static
equilibrium: score each habitable cell by a hand-weighted formula
(freshwater, coast, temperance, minus hostility), scatter spaced sites from
the highest score down, and draw each site's population from its own score.
The model answered *where* settlements would be and never *how many people,
and why there* — population was a free variable, disconnected from any
physical account of what the land could support, and nothing caught a world
whose settlements summed to an absurd total. Worse, there was no field
underneath any of it: the frontier's long-standing vision for population is
a five-part pipeline running from a carrying-capacity prior through a
density field relaxing toward it, settlements condensing out of that field,
history unfolding as the field moves through deep time, and species
dispersing across it — and a static scatter left nothing for any of that to
act on.

This campaign builds the first part of that pipeline, at equilibrium: a
physically-grounded carrying-capacity field, and the condensation that reads
discrete settlements off it as *conserved attractors* of a population flow.
Everything downstream of equilibrium — the field evolving through time, the
founding/growth/abandonment history that would ride on it, species
dispersal — is deliberately out of scope, a later campaign's work. The seam
between them is the whole point: this campaign ships the field; the field is
what history needs to have something to move.

## A field grounded in something real

The carrying-capacity field `K` is closed-form and seed-free — nothing about
it is drawn — built from a **Miami model** net-primary-productivity proxy
(a standard two-line empirical fit combining a temperature response and a
moisture response, taking whichever is scarcer) scaled by freshwater
availability, a coastal bonus, and an aridity penalty that drives K to
exactly zero on ocean and uninhabitable cells. Each species reads its own
copy of the field, its psychology folding in the same way the retired
suitability formula once did: a longer time horizon values reliable water
more, a bolder threat response tolerates harsher ground.

The field's grounding is the campaign's headline claim, and it is
calibration-checked rather than asserted: a preregistered Lab study measures
total supported capacity against absolute latitude across two hundred
generated worlds, and the tropical-and-temperate band comes out supporting
roughly **twenty-seven times** the capacity of the polar band — a
decisive, non-tuned reproduction of the real biomass-by-latitude gradient.
The constants were drafted once from the Miami model's textbook tropical
optimum, measured against that gradient, found already sufficient, and
frozen without adjustment — a negative result would have been published
just as readily (the Laboratory's studies preregister their hypotheses
before the sweep, win or lose).

## Settlements as condensations, not draws

Reading settlements off the field borrows a shape the codebase had already
proven for water: the terrain domain's drainage extraction, which sends
each land cell downhill to its lowest neighbor and accumulates upstream
area along the way. Condensation runs the identical machine with the
comparator flipped — people climb the carrying-capacity gradient the way
water descends elevation. Every cell routes toward its highest-K neighbor;
a cell with no higher neighbor is an *attractor*, a candidate settlement;
every other cell's accumulated flow ends at the attractor its climbing path
leads to, which is that attractor's catchment. A settlement's population is
simply the field summed over its catchment — a readout, never a draw.

This makes conservation a structural property rather than a tuned one.
Summed over every attractor, at no population floor, the sum of settlement
populations equals the sum of the carrying-capacity field exactly, per
species — settlements *partition* the K budget rather than each sampling a
local value, so nothing can leak. The operational world only keeps
attractors whose catchment clears a concentration threshold, which — as in
any real pre-industrial land — leaves roughly half the supported population
dispersed and rural rather than gathered into a named place; that remainder
is accounted for, not lost, and a world-level guard bounds it on both sides
so neither an inflated count nor a collapse toward zero passes silently.
The threshold itself was tuned once, to a settlement count the committed
artifacts and future censuses could carry comfortably, and then frozen: seed
42's level-6 globe settled from 998 settlements at the placeholder threshold
to 182, the average catchment rising from about seven people to about
twenty-two.

One guarantee survives from the retired mechanism's own history. The
founder floor — the pigeonhole rule, authored during the goblinoid family's
founding, that no authored people is ever shoved off the map entirely by a
stronger neighbor — moves with the field it now floors over: every species
keeps its single strongest attractor even when that attractor's catchment
falls below the concentration threshold, and a domain-level invariant
guarantees that floor never commits a population of zero. The *flagship* — every world's first settlement fact,
its capital by convention — is redefined the same way its predecessor was:
the single highest-population attractor across every species that settled,
now a readout of the flow rather than an argmax over suitability scores.

## The deliberate seam

The multi-species texture the retired mechanism approximated with a spacing
rule — no two peoples too close together — is not rebuilt here. This
campaign's condensation runs each species' field independently, which means
two peoples may now condense settlements onto overlapping ground, a
documented and accepted step backward from the old exclusion rule. The
reason is a genuine reframing surfaced mid-campaign: a single scalar
species-strength number understates a much richer question — how home-range
footprint, competitive pressure, and predator-prey coupling should really
divide a shared landscape between coexisting peoples. That question deserves
its own design, building on the field this campaign delivers rather than
being crammed alongside it, and it now has one. This campaign ships the
foundation; the coexistence texture is deliberately somebody else's turn.

## What is fenced out, and where it goes

The field this campaign built is an equilibrium snapshot — `population =
f(carrying capacity)` in closed form, no iteration, no clock. Nothing about
temporal relaxation, founding, growth, fission, or abandonment is coined
here; populating those predicates with nothing yet to say would have
violated the project's own smallest-version discipline. Species dispersal,
the sharp edge where a population shrinks to a single survivor, and the
richer multi-species coexistence texture described above are named and
fenced, each waiting on the campaign whose job they are. What this campaign
settled is the ground floor: a population the land can be measured to
support, and settlements that are honest readouts of it rather than numbers
handed down from outside the simulation.

## The road ahead

A carrying-capacity field now exists, calibration-checked against a real
gradient, and every settlement in every generated world is a conserved
consequence of it rather than a coincidence of tuning. The next campaign in
this pipeline gives that field a clock — relaxation through deep time, and
the founding-to-abandonment history that only becomes tellable once
population is something that moves. The coexistence texture between
species, richer than a single scalar comparison, has its own design waiting
to build on this same foundation. Both inherit a field that already answers
the question the old scatter never could: not just where people live, but
how many, and why there.
