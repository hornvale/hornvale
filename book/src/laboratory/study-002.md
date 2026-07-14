# Study 002: The Census of Lands

Ten thousand worlds, unpinned — seeds 0 through 9,999, each carrying not
just a sky but a tectonic globe and, new since Campaign 3c, a full climate
and biome map derived from the two together. The metric registry that once
knew only the sky now knows the land as well: plate count, ocean fraction,
mountain coverage, circulation band count, habitable fraction, tectonic
unrest, and which biome covers the most land — 21 metrics in total, one
registry, one runner, unchanged since Study 001. As before, the census asks
two kinds of question: one with a known answer, checked because an
instrument that lies about what it already knows cannot be trusted about
what it doesn't; and one that had no answer until this run existed.

**A note on scale and provenance.** This chapter's headline numbers come
from a 10,000-seed run of `studies/census-of-lands.study.json`, executed
once by hand at author time — at that size it is not part of CI, the same
arrangement Study 001 uses for `census-of-skies`. The charts embedded below
are *not* that run; they come from `the-census`, the 1,000-seed sibling
study CI reruns and diffs on every build (`book/src/laboratory/generated/the-census/`).
The two studies measure the same generator and land on the same shape at
different sample sizes — small numeric differences between a percentage
quoted in this prose and a bar in a chart below (e.g. 79.5% vs. 80.6% of
worlds landing on three circulation bands) are sampling variance between a
10,000-seed and a 500-seed draw, not drift. Only the `the-census`
charts are drift-checked; if this chapter's 10,000-world numbers ever look
stale next to a future drift chart, that staleness is real but is *this
chapter's* problem to fix by hand, not CI's to catch — the 10k run is
never regenerated automatically.

## The calibration holds

Circulation bands per hemisphere are, by construction, a step function of
the solar day: locked worlds get no bands at all (climate reorganizes
around the substellar point instead); a day of 40 hours or longer gets one
broad band; 20 to 40 hours gets three (Earth's regime — Hadley, Ferrel,
Polar); 10 to 20 hours gets five; anything faster gets seven. This is not a
hypothesis about the census, it is a fact about `band_count_for` — so
`band-count` must equal that function of `day-length-hours` in every world,
and `windows/lab/tests/calibration.rs` asserts it row by row on every CI
build.

Across the 10,000-world census it holds exactly: **7,947 worlds (79.5%)
land on three bands, 1,596 (16.0%) land on five, and 457 (4.6%) are locked
and carry none.** No world in the sample lands on one band or seven — the
day-length draw never reaches the 40-hour or sub-10-hour thresholds those
regimes require, so at this generator's current draw ranges the entire
population lives in the middle two spinning regimes plus tidal lock. That
emptiness is itself informative: it is the same kind of envelope check
Study 001 ran on obliquity and tidal strength, now extended to a
downstream quantity climate derives rather than astronomy draws.

{{#include generated/the-census/the-census-default-band-count.svg}}

## The first unknown number: habitable fraction

Habitability is the non-opinionated answer to "where could a vale-like
place stand": land, above sea level, with a tolerable annual-mean
temperature and enough moisture for reliable liquid water. Nobody knew what
share of a generated world that describes — it is a threshold on three
derived fields intersecting, not a quantity anyone drew or could eyeball
from a single seed. The census answers it:

**Mean habitable fraction across 10,000 worlds: 15.1% of the globe
(median 15.4%, standard deviation 7.9 percentage points).** The full
distribution is unimodal and fairly tight — the great majority of worlds
land between 5% and 30% habitable, with two worlds at the extreme low end
reaching exactly 0% and the single most generous world reaching 40.9%.

{{#include generated/the-census/the-census-default-habitable-fraction.svg}}

What drives the tails is now measurable, not guessed:

- **Mountain coverage is the strongest single driver** (correlation
  −0.40 against habitable fraction across the full census). The ten worlds
  in the bottom decile of habitability average 84% mountain coverage
  against a population median of 14%; the top decile of habitability
  averages only 26%. High peaks are neither warm enough nor wet enough by
  this model's thresholds, so a world dominated by orogeny is a world with
  little room left to be habitable, almost independent of anything else
  about it.
- **Tidal lock roughly halves the average**, but does not touch the true
  extremes. Locked worlds average 8.7% habitable against 15.4% for
  spinning worlds — the substellar desert and the frozen far side both cost
  habitable ground, leaving only the terminator ring — yet no locked world
  in the census falls below 1.6% or above 20.9%. The single emptiest and
  single most generous worlds in the whole sample are both ordinary
  spinning worlds whose mountain coverage happened to land at an extreme;
  locked worlds are pulled down and *narrowed*, not sent to either edge.
  Only 2.5% of the least-habitable tenth of worlds are locked — below the
  4.6% locked worlds' own share of the population — because extreme
  mountain coverage on a spinning world is a harsher fate than tidal lock
  itself.
- **Obliquity shows essentially no relationship** to total habitable
  fraction among spinning worlds (correlation −0.01). This is not the
  contradiction it first looks like: obliquity moves *where and how hard*
  the seasons swing and which latitudes see biome bands migrate — visible
  in the moisture and temperature fields at any given day — but the
  habitability mask is built from the *annual mean*, which a symmetric
  seasonal swing does not shift. A tilted world and an untilted one at the
  same latitude end up equally habitable on average across the year, even
  though their day-to-day experience of that habitability differs sharply.
- **Ocean fraction correlates weakly** (−0.19) — more sea leaves less
  land to be habitable at all, but the effect is muted because the target
  ocean fraction itself only ranges narrowly (50–80% across the whole
  census, §below), leaving mountain coverage the far larger lever.

## The shape of the land

Three more numbers the census makes visible for the first time:

**Ocean fraction** clusters tightly around 50–80% of the globe, split
roughly evenly across [0.5, 0.6), [0.6, 0.7), and [0.7, 0.8) — a direct
readout of the drawn target the tectonic model aims sea level at, now
shown to land inside its intended envelope at scale.

{{#include generated/the-census/the-census-default-ocean-fraction.svg}}

**Mountain coverage** (land above 2,000 m over the sea) is sharply
bimodal, not centered: roughly half of worlds (48%) carry it above 30%,
while most of the rest sit under 5%. There is very little middle ground.
The census does not yet decompose why — that is a two-metric question for
plate count, hotspot draws, or per-belt maturity against mountain coverage,
answerable later from the same rows without regenerating anything, exactly
as Study 001 left the star-class/tidal-lock question open for its
successor.

{{#include generated/the-census/the-census-default-mountain-coverage.svg}}

**Unrest coverage** (the fraction of cells with tectonic unrest above 0.3)
stays low and unimodal — 56% of worlds keep it under 5% of their surface,
essentially all the rest under 10%. Unrest is banked, unconsumed by any
domain in C3 (spec §15); the census is simply the first measurement of a
field waiting for a future architecture, theology, or legend campaign to
read it.

{{#include generated/the-census/the-census-default-unrest-coverage.svg}}

## What grows there: the dominant land biome

**dominant-land-biome** names the single most common land biome by cell
count on each world — a summary of a whole distribution to one categorical
column, with the full mix always available in the CSV for a future study
that wants it. Across the census, **ice dominates 37.8% of worlds**,
**alpine 20.6%**, **tropical seasonal forest 19.4%**, **taiga 9.6%**,
**temperate forest 7.3%**, **tundra 4.9%**, and **savanna** and **desert**
round out the tail at under half a percent each. Ice's plurality does not
mean most *land* is icy — it means that across enough worlds, cold
high-latitude ground is the single category most likely to out-count every
warmer alternative on any one globe, the same distinction Study 001 drew
between the moon-refusal rate and simple childlessness.

{{#include generated/the-census/the-census-default-dominant-land-biome.svg}}

Tidal lock sharpens this further: **among the 457 locked worlds, 410
(90%) are dominated by ice** — the frozen far side simply outweighs the
narrow habitable terminator ring in cell count almost every time — against
just 35% of spinning worlds. Spinning worlds spread across every land
biome the generator can produce; locked worlds funnel overwhelmingly into
one.

## What the census does not yet say

Mountain coverage's bimodal split, and the handful of two-metric questions
noted above it, are exactly the kind of thing this instrument now makes
answerable without a single new world being generated — the runner already
recorded one row per world, with every metric on it, precisely so later
studies can correlate rather than regenerate. For now the land half of the
census stands next to the sky half: built, calibrated against a fact the
code already guaranteed, and honest about the boundary between what it
measures and what it has not yet been asked to explain.
