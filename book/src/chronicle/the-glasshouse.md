# The Glasshouse

A glasshouse is warm for a reason that has nothing to do with how close it
sits to the sun. Hornvale's worlds were cold — a median land temperature of
−11.99 °C across a thousand of them, two-thirds of them ice-dominated — and
the reason was the same one: they had a sun and an orbit and no glass.

This campaign gave them glass. The median land temperature is now −3.65 °C
and the share of ice-dominated worlds fell from 651 in a thousand to 187.
That is the headline, and it is worth saying immediately that the campaign
did **not** meet the target it set itself. It aimed for Earth's +8.6 °C
within five degrees and landed twelve degrees short. Four of its six
preregistered criteria passed, two failed, and the two failures are the more
interesting half of what follows.

## The cold was three defects wearing one coat

A world's temperature in Hornvale came from its insolation — how much light
its star delivers — passed through a latitude profile and a fixed greenhouse
term. Three things were wrong with that arrangement at once, and each of them
had been individually invisible because the others were compensating.

The first was arithmetic. The latitude profile subtracted a term proportional
to the square of the sine of latitude, and over a sphere the area-weighted
mean of that quantity is one third, not one half. The profile that was meant
to average to zero averaged to +10 K. Nothing was obviously wrong with any
world, because a systematic +10 K offset looks exactly like a warmer planet.

The second was a contradiction between two parts of the model that never met.
The habitable zone — the band of orbital radii a world may be drawn into —
is denominated for a planet with a **variable** greenhouse, one that thickens
as the star dims. The temperature model had a **fixed** one. So worlds were
placed in orbits justified by a thermostat they did not possess, and the
outer half of every habitable zone froze.

The third was the ground itself. Land read systematically high — a median
mean elevation of 2267 m — because the continental crust was being scaled
into a budget it under-delivered, which cut the coastline more than a
kilometre below the shelf break. High land is cold land: the lapse rate does
not care why the rock is up there. That median is now 1700 m.

The fix for the second defect is the glass. A carbonate–silicate thermostat
lets a world's greenhouse respond to the light it actually receives, so that
a dimmer star buys a thicker blanket rather than an ice age, with a drawn
residual so that two worlds at the same distance from the same star are not
obliged to have the same climate.

## A constant that could not be fixed the way the plan said

The thermostat has one free parameter — how much of an insolation shortfall
the greenhouse is allowed to cancel. The plan said, correctly in spirit, that
such a constant must be fixed from Earth rather than from Hornvale's own
population, because tuning it until the census looks right is circular.

It could not be. The model says the effective insolation is `1 + k·(S − 1)`,
and Earth's anchor is *at* `S = 1`, where the `k` term vanishes identically.
A single anchor point cannot constrain a slope that passes through it. This
is one line of algebra and nobody had done it, in the spec or the plan or the
review, because "fix this constant from Earth" is the kind of instruction
that sounds like a discipline rather than a claim.

The value settled at 0.30, and it was given a source that is a statement
about Earth rather than a preference: 0.30 asserts that Earth's Archean
global mean was about +8.5 °C — a temperate early Earth, which is inside the
genuinely contested literature range and clear of both a frozen Archean and a
boiling one. Anyone who wants to dispute the constant now has to argue
paleoclimate, which is the property the earlier swept value never had.

It is also worth recording that `k` is **not** the dominant lever, because
the next person to reach for it should know that first. Across its entire
range, from 0.4 to perfect compensation at 0, the median land temperature of
the spinning population moves 7.2 K and the share of worlds with sub-freezing
land falls only from 64% to 41%. Two-fifths stay cold under a *perfect*
thermostat. The cold is a property of how orbits are drawn: the zone spans
0.95 to 1.37 times the square root of luminosity, radius is drawn uniformly
across it, and since insolation goes as luminosity over radius squared, the
luminosity cancels exactly. The population sits at a median insolation of
0.748 by construction, and no thermostat can argue with a measure.

That is why the campaign fell twelve degrees short of Earth, and why the
shortfall is a finding rather than a failure of the fix. The remaining gap is
in the *draw*, not in the physics.

## What the criteria said

Six were frozen before any of this was built. Four held.

The temperature **spread** was the criterion designed to catch a lazy fix — a
strong enough thermostat satisfies a target median by flattening everyone
onto it. The population had to retain at least 70% of its measured 44.59 K
span, and it retained 78%, at 34.92 K. No biome class exceeds half the
population: the largest is now taiga at 25.0%, where ice had been 65.1%.
Earth's own insolation, which used to land at the 88th percentile of the
temperature distribution — an outlier in its own world-generator — now lands
at the 59th, which is what "an ordinary draw" means. And the correlation
between a world's insolation and its temperature, the number that said
climate was merely a restatement of orbit, fell from +0.9227 to +0.3267.

Two failed. The median missed Earth by twelve degrees, for the reason above.
And one column would not move at all.

## The column that would not move

Every world in the census reports a dominant soil order, and on all thousand
of them, before and after, that order is `leptosol` — thin, steep, rocky
soil. Warming the entire population by more than eight degrees moved it not
one world.

The reason it was expected to move was an inference: leptosol is the soil of
high, steep ground, the land was too high, and the land had just been
lowered. The reason it did not move is that the classifier's first question
is not about climate at all. It asks whether the soil is shallower than a
quarter of a metre or the ground drops more than 300 m to a neighbour, and if
either holds it answers leptosol and stops. Every question below that line
reads temperature or moisture; none of them was ever reached.

Measured over twenty worlds and three hundred thousand land cells: **72.1%
of land never reaches the climate ladder.** Not the steep arm, which takes
11.04% — the *depth* arm, which takes 61.09%. And where the ladder does run,
it is perfectly healthy: eight distinct soil orders, the largest at 30.19%.

So the frozen column is not evidence of a broken classifier. It is evidence
that a well-behaved classifier is being answered before it is asked, by a
soil-depth model that puts 61% of all land under 25 cm of soil. That is a
real defect and it is not this campaign's — no arrangement of insolation,
greenhouse, or hypsometry could have moved it, because the branch that
decides is downstream of none of them.

The distinction matters more than the particular soil. A measurement that
refuses to move is usually read as a weak effect. Here it was a *scope error*
wearing that costume, and the only thing that told the two apart was
attributing the branch instead of inferring it.

## Eight and a half sigma, which was one

The campaign inherited a failure described as 8.6 standard deviations from
expectation — 402 caves where 262 were expected. That number was fiction. It
treated the cells of a bucket as independent coin flips, over a field that
the very next assertion in the same battery declares is spatially clustered
at 90% or better. Corrected for the overdispersion that clustering implies,
the excess is 1.09 sigma: nothing at all.

Two assertions eight lines apart had encoded contradictory models of the same
field and both stayed green for a year, because nothing compares one
assertion to another. What finally surfaced the disagreement was not review
but a population change large enough to make the two of them argue out loud.

The same shape appeared again, in a different battery, and was corrected
rather than inherited. A test asserted that every pair of peoples whose
predicted naming styles differ by 0.15 must show that difference in the
world, and justified the strictness by claiming 0.15 was "several sampling
standard errors" at those sample sizes. At the smallest sample the test
accepts — twenty names apiece — the standard error on a difference of two
proportions is 0.158. The gap was **0.95** standard errors. One, not several.

The test now measures each inversion against that pair's own sampling error
and ignores the ones inside it, while failing on any that exceed it. The
threshold is two standard errors, and the reason it can be trusted despite
being chosen while the answer was already known is that it makes no
difference: the two inversions that had been failing the test sit at 0.28 and
0.79 standard errors, so one, two, or three would all have forgiven them.
A threshold whose verdict is invariant across every conventional choice
cannot have been fitted to the outcome.

## A thousand worlds that did not change

One number is worth ending on. The refresh added a genuinely new seeded draw
to the world — the greenhouse residual — and a new draw is the one kind of
addition that can silently re-cut every stream downstream of it, corrupting
every saved world that ever existed. Twenty-nine of the census's sky columns
came back **byte-identical** across the refresh, over all thousand worlds.

That is the stream-isolation contract holding under the only test that
actually examines it: not an argument that the draws are independent, but a
thousand worlds' worth of evidence that they are.
