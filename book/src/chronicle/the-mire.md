# The Mire

Weather was going to swing the world's passable geography with latitude —
more so near the poles, where snow and freeze are supposed to be the whole
story. Two hundred worlds later, both predictions are wrong, and wrong in a
way that teaches something more durable than either would have if it had
merely succeeded.

## What was measured

Weather had never accumulated. The moisture, snow, and temperature fields
were already dense and seeded, but nothing carried yesterday's rain into
today's mud — every read was instantaneous, and a place could not remember
being rained on. This campaign minted the missing quantity, a daily
precipitation rate distributed across the year in proportion to each day's
weather intensity but constrained to sum back to the existing annual
climatology (coarse constrains fine), and used it to drive two accumulating
substrates: surface wetness, which wets in days and dries in days, and
snowpack, which piles up across months. Both are read off a **connection
graph** — the same structure that already decides whether two places are
reachable — through an edge conductance that mud and snow lower and that
frozen ground raises back.

The preregistered study asked three questions of two hundred generated
worlds, evaluated at twelve days spread across a converged annual cycle,
land cells only:

```
H1  Does weather ever move the passable map at a global scale?
      median swing 0.0095        floor 0.05           FALSIFIED
H2  Does that swing grow toward the poles, where the weather is harshest?
      equatorial 0.0224 > temperate 0.0021 > polar 0.0000
      preregistered direction: increasing with latitude    FALSIFIED, REVERSED
H3  Does the daily rate still sum to the annual climatology it was built from?
      4,991 cell-seed samples, 0 violations               CONFIRMED
```

Both predictions about *where the drama would be* were wrong, and wrong in
the same direction. That is the finding, not a failure to find one — several
campaigns before this one have shipped a null as the headline, and this is
another.

## The mechanism: harsh is not the same axis as variable

The prediction had assumed that the coldest, snowiest places would show the
biggest seasonal swing, because they are where weather is most extreme. The
measurement says the opposite, and the reason is simple once stated: **a
polar cell that is permanently frozen has constant conductance.** It does not
have a hard winter and a soft summer; it has one season, all year, every
year. There is nothing left to swing between. An equatorial cell, by
contrast, genuinely alternates — a wet season that softens the ground and a
dry season that firms it back up — and it is that alternation, not the
underlying harshness, that the conductance gate can detect.

The polar median is not merely small. It is **exactly zero**, and that
distinction matters: a mechanism that made high latitudes swing *less* would
plausibly still land on some small positive number. A mechanism that made
high latitudes structurally *unable* to swing lands on zero, on the nose,
because the underlying state genuinely never crosses the passability
threshold in either direction. The measured result is the second shape, not
the first.

The general lesson generalizes past this one campaign: **seasonal variation
lives where conditions alternate, not where they are extreme.** A place that
is always frozen, always parched, or always flooded has no season in the
sense this instrument (or, plausibly, an inhabitant) can register — extremity
without alternation is stasis, and drama needs the alternation.

## The irony: the guard against a monotone penalty caused the zero

Early in the design, there was a real worry that the conductance modifier
would turn out to be a monotone penalty dressed up as physics — weather
always making travel worse, never better, which would have been a cheap
trick rather than a model. The guard against that was to make a frozen mire
**recover** conductance rather than lose it further: hard, frozen ground
travels better than the same ground half-thawed and boggy. That asymmetry
was meant to be a small, deliberate correctness check.

It turned out to be the dominant physics. The frozen-ground-travels-better
asymmetry is the *direct cause* of the polar zero: a permanently frozen cell
sits at its relieved, high-conductance state every day of the sampled year,
never dipping into the low-conductance state a temporary thaw would produce.
The thing added to prove the model was not a disguised penalty turned out to
be the very mechanism that flattens the pole to a constant. A guard aimed at
one failure mode ended up explaining a different, unanticipated result.

## The strongest evidence the null is real

A falsified prediction invites a first question: is the instrument capable
of detecting an effect this size at all, or is it simply insensitive? Two
checks answer that directly.

First, a synthetic probe that forced every land edge in a sampled world to
be fully passable on one day and fully impassable on the next — not a
realistic weather state, a deliberate extreme — registered swings of
0.093–0.181 across the seeds it was run on. That is ten to twenty times the
measured median of 0.0095. The instrument can register an effect of this
magnitude; it did, cleanly, when one was manufactured. It simply is not
seeing one in the real, weather-driven data.

Second, and more telling: across a full year, only about **4% of real
directed land edges ever cross the passability threshold at all** — in one
seed, 9,996 of roughly 256,000 directed edges. The rest are always-above or
always-below the threshold for the entire year, regardless of season. This
is the mechanistic reason the swing is small: most of the graph is not
*sometimes* passable and *sometimes* not, it is *one or the other, always*.
Weather's seasonal signal is real (H3 confirms the underlying quantity
behaves correctly at every one of 4,991 samples), but it rarely pushes a
given edge's conductance across the one threshold this instrument reads.

## Two honest caveats

**H1 measures the largest connected component, not literal edge-by-edge
passability.** "Passable fraction" was read as "fraction of land in the
largest reachable region," which is a defensible operational reading of the
preregistered quantity but not a literal count of which edges are open. A
graph can restructure — some edges flipping in, others flipping out — without
moving the size of its biggest piece by much, and the chosen metric would
under-report that kind of change.

**The largest region is not always the same landmass from day to day.** In
one sampled world, the polar band's daily swing spiked to 0.279 on one day
where every other seed's polar band sat at 0.0 all year — the largest
connected region had, on that day, jumped to a different landmass entirely
rather than grown or shrunk continuously. This is a rare event across the
population, not the typical case, but it means the metric can occasionally
register a landmass-identity switch as if it were a seasonal swing.

## Why the substrate is a recurrence, not a convolution

The moisture and snow substrates are each computed as a forward recurrence —
today's state equals yesterday's state, plus what accumulated, minus what
was lost — rather than as a weighted sum (a convolution) over past weather.
The reason is not stylistic: the amount lost on a given day depends on how
much is *already present* and on whether the ground is frozen, and a
process whose loss rate depends on its own current state is not linear. A
linear operator can be evaluated as a convolution; a state-dependent one
cannot, because there is no fixed decay curve to weight the past against —
snow does not ablate at all below freezing and ablates quickly above it, so
the "same" amount of snow decays at different rates on different days
depending on temperature alone.

Each cell's trajectory is spun up by iterating the recurrence forward,
starting from nothing, until successive simulated years converge to the same
annual shape. Surface wetness converges within a single year; snowpack takes
a few. Some cells never converge at all — the amount accumulates without
bound, year over year, forever. That non-convergence is not a bug in the
iteration; a cell whose snow never stops piling up *is a glacier*, and
recording that a cell failed to converge is the model's honest way of saying
so.

## What this does not settle

The measured swing is on **passability** — whether a route is open at all —
not on **cost**, how much slower or harder a route becomes while still being
open. A large seasonal cost effect could sit entirely beneath this
instrument's threshold-crossing view and be invisible to it: mud that
triples a journey's difficulty without ever closing the road contributes
nothing to a passability count. The high-value next measurement is on cost,
not on component membership.

Separately, the finding that poles do not vary is a finding about **land
only** — the water edges of the connection graph were deliberately left
ungated this campaign, with sea ice held for a later pass. Sea ice is
exactly where high-latitude seasonality would plausibly show up instead:
ships frozen in for the winter and released in the spring, on coastlines
whose land itself never varies because it is permanently frozen. "The poles
do not vary" may be true for land and false for water in the same worlds.
