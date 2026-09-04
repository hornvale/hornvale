# The Staple

*A staple is the commodity a place lives on, and it is also the staple town —
the place the law said goods must be brought to. Nothing is a staple town by
itself; it is the name of a relation. A campaign that set out to give a city
its parts found instead that the world has no cities, and that the reason is
not a missing vocabulary but a missing relation.*

This campaign was opened to build the settlement rung of a plan written six
weeks earlier: a settlement stops being one undifferentiated blob and gains
parts — a market, a temple precinct, a waterfront, a wall. The plan stated the
target as a worked example. A `Trade` and `Seat` and `Classical` coastal place
of high population draws docks, warehouse row, market, curia, temple precinct,
uptown villas, tenements, walls and gates; a `Backwater`, `Agrarian`,
`Neolithic` place draws three patterns from the same inventory and is a hamlet.
Two poles, one grammar, and the contrast between them is the whole argument.

The owner's brief asked that the example be checked against the code before
anything was designed. It was, and neither pole exists.

## Four axes, and what they actually hold

Every term in that example is a field on the brief — the small record that is
the only thing generation may read about a place beyond its address and the
seed. So the question has an exact form: over the places a brief can be taken
at, what values do those fields hold? Measured over four thousand and two
occupations across five worlds, alive and ended:

```
  notability        Common on all 4,002                        constant
  function          Agrarian 3,951, Mine 51, nothing else      near-constant
  tech              alive: Classical 1,006 of 1,006            temporal only
  peak_population   maximum ever 86, against a ceiling of 150  never clears
```

`Trade` and `Seat` and `Classical` together is zero on every seed. So is
`Backwater` with `Neolithic`. The example cannot discriminate between its two
poles, and the reason is not that the grammar is wrong but that the index has
no values to index on.

Three different causes sit underneath, and separating them is most of what the
campaign learned. Two of the fields are **hardcoded literals** at the single
site where a community is founded — the engine's own comment says as much, that
it opens every community agrarian, "the engine's default and, before this
campaign, its only reachable value." A third, the technological horizon, *is*
derived, but derived from the year: it is a **world clock rather than a place
axis**, so it separates eras and never two places standing at the same moment,
and exactly one era is ever observable from a world you can walk in. The dead
occupations span all four horizons; the living ones are uniformly the last.
And the fourth is bounded by a single vertex's carrying capacity, at a scale
constant documented as the settlers a maximal-suitability vertex supports.

The corroboration was already in the tree and nobody had gone looking. There
is a function that composes a settlement out of structures — huts, a granary, a
market, a shrine, a temple, a wall, a mineshaft, a longhouse — and **six of its
eight structures are unreachable**, three because they need functions that
never occur and one because it needs a population more than twice the largest
the model has ever produced. Every settlement in every world is one to three
huts and a granary. The idea registry had also already written the sentence,
in a row about species scale: *why every settlement is a hamlet.* What had not
been done was to draw the consequence for the rung that was about to be built
on top of it.

## The river runs into a sink

The owner's reframing is what turned a falsification into a plan. A city is not
a settlement with a larger number in it; it is the apex of a network that
carries food and fuel and stone to it from somewhere, and none of that
somewhere existed. Cities are rivers of people through time, and the stocks and
the flows should be measurable.

Checked against the engine, that is half wrong in the most useful direction.
The deep-history bake already keeps **two stocks** on every community — a
population and a store of accumulated wealth — and **one flow** between them.
A dominant community assesses a subordinate, the subordinate conceals part of
its harvest, and what is left moves. The machinery around it is careful: the
demand is capped at what the land could ever yield, the rate is set at an
eighth of the growth rate so that it binds across the middle of the capacity
curve instead of decoratively at one point, a seasonal harvest curve resolves
banking against winter draw-down, and past a threshold the vassal simply runs.

Read the units, though. The payment subtracts from the subordinate's
*population* and adds to the patron's *wealth*, and the wealth is documented as
never eaten — it feeds raiding strength, and it is annihilated when the
community dies. So people convert to wealth at one to one, and wealth converts
back to nothing. **There is one commodity in this world and the commodity is
people**, and the river of them runs into a reservoir with no outlet.

That single observation reorganized the whole plan. The three functions that
never occur are not labels waiting for a field to be read off at founding; they
are the **return flow** — what a patron sends back down a relation it has taken
from. Protection is a fort. Goods are a market. Legitimacy is a temple. A heart
that only takes is not a heart.

## A wheel, not a ladder

Nine things must become true between a dwelling and a city, and sorting them by
how fast each one operates splits them cleanly in two — a split that turns out
to predict, exactly, which ones cost an epoch.

Some are **readings**: a dwelling belongs to its people, a building's shape has
a reason, a city has districts. These are pure functions over a finished world.
They occur at derivation time, commit nothing, consume no stream, and can be
ordered independently of each other. *The Housemark* later sharpened the
saved-world clause: its derivation stayed pure, but the new `bench` concept
entered the serialized concept registry and therefore took concept accession
epoch 20. A reading need not move a world derivation to owe an accession for
new vocabulary. The others are **dynamics**: worked land, flows, the return
flow, specialization, the city itself, and decay. These give the bake
mechanism, they run at the pace of years and generations and centuries, and
every one of them moves world identity — and they compound, so each one
invalidates the calibration of the one before it.

The consequence worth having is that the district rung needs the dynamics arc
for its *subject* but not for its *machinery*. It can be built and proven
against the hamlets that exist, and simply get richer when cities arrive. The
cheap, visible work never has to wait behind four epochs it does not need.

And the sequence does not end where it was drawn to end. The world has **two
thousand nine hundred and ninety-six dead occupations against one thousand and
six living ones**, with as many as twenty-three ruins stacked beneath a single
living place. A plan that only builds upward describes the minority case.
Closed as a wheel — founding, growth, surplus, subordination, apex, decline,
abandonment, ruin, and refounding on the ruin — the bottom arc is where the
world actually lives, and the vocabulary for reading it was written one
campaign earlier, underground, where tenancy is a tense rather than a flag.

## What the campaign shipped

A document, and one measurement that anyone can retake. The probe is committed
rather than left in a scratch directory, because every number above comes from
it and the alternative is a claim with a date and no way to check it — which is
how the predecessor plan's own cost figures went stale.

Nothing else moved. No mechanism was built, no epoch was taken, and the rung
this campaign was opened to build now has a document explaining what must be
true before it has a subject.
