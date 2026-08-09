# The Fare

The Mire found that weather does not move the world's passable map, and
suspected the instrument. It had measured whether a route was *open*, when
weather's real effect is surely on what a route *costs*. This campaign built
the cost instrument and asked again.

The answer is that weather's effect on travel is real, is large, and is
almost entirely invisible to the average. It is a property of the worst
journeys, not of the typical one — and, against every expectation this
campaign carried into the measurement, it cannot be avoided by choosing a
different road.

## The instrument had to be built one layer lower

The Mire put mud on the edges of a graph. Reading its code closely turned up
the reason that could never have answered the question: in the composition
root, land corridors are found by a least-cost search over the **dry**
traversal-cost field, and weather is applied *afterwards*, by scaling the
resulting edges. Which roads exist is decided weather-blind. Weather only
reweights roads that were already surveyed in fair conditions.

A second fact compounded it. The cost field those roads are planned over
reads elevation slope alone; biome enters it solely as the marine test. **A
bog and a grassland at the same gradient cost the same.** Weather's effect on
travel was not merely unmeasured — it was unrepresentable in the field that
plans routes.

So The Fare routes over a *weathered cost field* of its own, built inside the
study and never committed: the shipped dry field plus a per-cell surcharge
derived from the same substrate state the graph gating already reads. The
surcharge is additive rather than multiplicative, because the slope term
reaches into the thousands and a multiplier would have made weather's
absolute contribution scale with relief — largest on mountains, smallest on
the flat ground roads actually follow. It is anchored on the tabletop
convention that difficult terrain doubles movement, and floored so that
weather can never render a cell impassable: letting it do so would have
smuggled back the very passability threshold The Mire had already measured,
and would have silently dropped the hardest journeys from the sample.

## What was measured

Two hundred worlds. Journeys between deterministically sampled land-cell
pairs at controlled angular separations — 5°, 10°, 20°, 40° — each pair's
seasonal swing being its own maximum-minus-minimum path cost across twelve
days of the converged year, as a fraction of its dry cost.

```
F1  Does weather move the cost of a journey?
      pooled median swing at 40°   0.0037      floor 0.05     FALSIFIED
F2  Does weather change which road is cheapest?
      re-routing fraction at 40°   0.1485      floor 0.10     CONFIRMED
F3  Does the polar zero survive a different instrument?
      equatorial 0.008372 > temperate 0.000324 > polar 0.000000   CONFIRMED
F-mono  Do both grow with the length of the journey?
      F1 x3.49 and F2 x4.95 across 5° to 40°                 CONFIRMED
```

## The median was the wrong statistic, and that is the campaign's lesson

F1 failed, and the failure was preregistered — the spec recorded the
expectation of failure before the run, so that reporting it could not later
look like a floor chosen to be cleared. But "the median journey is
unaffected" was reported, briefly, as "weather does not meaningfully change
what a journey costs." That claim is false, and the instrument that produced
it could not have discovered its own error: **the primary readout computed no
percentile at all.**

Recomputed on the tail, the same two hundred worlds say something else
entirely.

```
band    p50      p90      p99      max
  5°   0.0010   0.0251   0.1215   0.8981
 10°   0.0015   0.0257   0.1097   0.7045
 20°   0.0021   0.0266   0.0962   0.4275
 40°   0.0033   0.0224   0.0653   0.1481
```

The ninety-ninth percentile clears F1's own floor at every measured band. At
40° it is eighteen times the median. The worst single short journey in two
hundred worlds nearly **doubles** in cost between its best day and its worst.

Weather is a catastrophe that happens to one traveller in a hundred, and a
median cannot see a catastrophe. A world in which the typical journey is
untroubled and the unlucky one is ruinous is not a world where weather does
not matter; it is a world where weather is a tail risk. That is what these
numbers describe, and the preregistered statistic was structurally unable to
say so.

## Foresight is nearly worthless, and that was a surprise

The obvious explanation for a small median was that the measurement had
priced weather *after* perfect avoidance. The router re-plans every sampled
day with complete knowledge; a real traveller commits to a road in summer and
walks it in winter. So the same worlds were measured again with the route
fixed — the dry-optimal path computed once, then costed under each day's
weather, with no re-planning permitted.

The difference is almost nothing. Committing to a summer route costs about
**half a percent** more than re-planning daily, rising to 27% only at the
extreme.

The explanation is already in the redundancy measurement: an alternative
route typically costs 11–18% more than the best one. Detouring around bad
ground costs about what crossing it does. Travellers in these worlds do
re-route — F2 says roughly one journey in seven changes its road across the
year — but the re-routing buys them very little. **They are not avoiding the
cost; they are trading it.**

This overturned the campaign's own working hypothesis, which is the more
valuable outcome than confirming it would have been.

## The polar zero is a property of the world

The Mire measured the seasonal swing at high latitude as exactly zero, and
was careful to note that this might be an artifact of a threshold instrument
reading one edge at a time. It is not.

A wholly different instrument — path cost rather than component membership,
geographic pairs rather than land cells, a different sampling frame and a
different arithmetic — returns **exactly 0.000000** in the polar band again,
with the same ordering beneath it. The band was populated; an empty one would
have been reported as absent rather than as zero.

Permanently frozen ground has constant conditions, and constancy is
instrument-independent. The Mire's most surprising finding survives its
strongest available test, and *"variation lives where conditions alternate,
not where they are extreme"* is now a measured property of these worlds
rather than a reading of one instrument.

## What this does not settle

The severity of a bad year. The substrate is spun up to an annual fixed
point, so every year in every world is the same year: there is no drought,
and no exceptional winter. History's worst journeys are made in outlier
seasons, and this model cannot express one. The tail measured here is the
tail of a *typical* year's geography — the spread across places and routes,
not across years. A model that could have a bad year would presumably have a
heavier tail still, and that is the single most promising direction this
campaign leaves open.

Nor is the surcharge's calibration settled on steep ground. An attempt to
measure it here failed by its own construction: asking for the route cell
with the largest surcharge *as a fraction of its own cost* preferentially
selects the cell with the smallest cost, so it reported the flattest cell on
each route rather than the hardest pass. The question — whether an additive
surcharge anchored to flat-ground cost is too small where relief dominates —
remains open and wants a measurement conditioned on terrain rather than
maximised over it.
