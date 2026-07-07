# Study 003: The Census of Peoples

Ten thousand worlds, unpinned — the same seeds 0 through 9,999 Study 001 and
Study 002 already walked, each now carrying a scatter of generated
settlements and, on the single flagship among them, an emergent subsistence
mode and caste structure (Campaign 4b). The metric registry grows by seven:
how many settlements a world places, their mean population, the flagship's
subsistence, biome, and coastal access, the size of its role structure, and
what share of the land drains nowhere at all. As with Studies 001 and 002,
the census asks a known-answer question first — because an instrument that
lies about what it already knows cannot be trusted about what it doesn't —
and then the genuinely unknown ones.

**A note on scale and provenance.** This chapter's headline numbers come from
a 10,000-seed run of `studies/census-of-peoples.study.json`, executed once by
hand at author time, exactly as Studies 001 and 002 report their own
10,000-seed headline runs. It is **not** committed and **not** part of CI —
regenerating it is this chapter's responsibility, by hand, the same
arrangement `census-of-lands`/`census-of-skies` use. The charts embedded
below are a different, smaller run: `census-lands-drift`, the 500-seed
sibling study CI reruns and diffs on every build
(`book/src/laboratory/generated/census-lands-drift/`), which now carries the
same seven People metrics alongside the sky and land metrics it already
tracked. Small numeric differences between a percentage quoted in this prose
and a bar in a chart below are sampling variance between a 10,000-seed and a
500-seed draw, not drift — only the `census-lands-drift` charts are
drift-checked.

## The calibration holds

Subsistence is not authored per settlement; it is `subsistence(BiomeClass,
coastal)`, an exact function culture computes from two independent columns
the census already recorded — a settlement's biome and whether its cell
touches the ocean. Forest and grassland always farm; arid, cold, and barren
settlements forage or herd inland, but fish if the coast rescues them. This
is not a hypothesis about the census, it is a fact about the function, so
`flagship-subsistence` must equal that re-derivation from `flagship-biome`
and `flagship-coastal` in every world — and `windows/lab/tests/calibration.rs`
asserts it row by row, over the 500-seed `census-lands-drift` sample, on
every CI build, the sociology sibling of the band-count calibration Study
002 reported.

Across the 10,000-world census it holds exactly, and the exactness surfaces a
finding nobody put there on purpose: **not one flagship, in ten thousand
worlds, forages or herds.** Every flagship in the sample is coastal — a
consequence of the placement suitability formula (Campaign 4a), which gives
a coastal cell a `freshwater` score of 1.0 outright plus its own coastal
bonus, an edge no inland site can match at this generator's current weights.
Since a coast always rescues a marginal `arid`/`cold`/`barren` verdict into
fishing, and forest/grassland settlements farm with or without a coast, the
census's entire 10,000-world flagship population splits into exactly two
subsistence modes: **farming, 69.0% (6,897 worlds), and fishing, 31.0%
(3,101 worlds).** Herding and foraging are legal outputs of the function —
any inland settlement could produce them — but no world's single most
suitable cell ever is one. This is a fact about where suitability's argmax
tends to land, not a limitation of the subsistence function itself, and the
census is what makes it visible.

{{#include generated/census-lands-drift/census-lands-drift-default-flagship-subsistence.svg}}

## The first unknown number: how many settlements, and how big

Nobody knew, before this census, how many settlements a generated world
would place, or how tightly that number would cluster. **Mean settlement
count: 57.7 (median 59, standard deviation 22.1), ranging from 0 to 122.**
Only two worlds in the sample place none at all — and both are exactly the
two worlds Study 002 flagged as reaching 0% habitable fraction, the census's
own cross-check that a world with no habitable land can place no
settlement, regardless of how low the suitability floor is set.
Settlement count correlates strongly with habitable fraction (**+0.89**
across the full census) — the more room the biome map leaves habitable, the
more spaced-apart 12°-separated sites there are room for, exactly the
relationship Campaign 4a's placement model would predict without a single
additional draw.

{{#include generated/census-lands-drift/census-lands-drift-default-settlement-count.svg}}

**Mean population per world is far tighter than the settlement count that
produces it: 353.8 (median 353.6, standard deviation 34.6, over a 151–457
range).** Individual settlements draw population from `40 + 460 × suitability`
jittered ±25% — a wide per-settlement range — but a world's *mean* across
dozens of settlements is an average over many draws, and averages narrow.
The census is the first measurement of how much they narrow at this
generator's scale.

{{#include generated/census-lands-drift/census-lands-drift-default-mean-population.svg}}

## The second unknown number: how stratified a flagship grows

`flagship-structure-size` counts the castes an emergent structure produces —
a stratification proxy running from 2 (a lone worker and a chief) to 6
(every rung the model can grow). Its distribution is not smooth: **37.7% of
flagships hold size 2, 24.7% hold size 3, 36.9% hold size 5, and sizes 4 and
6 are rare (0.6% and 0.02%)** — the middle of the range is nearly empty. The
subsistence split explains why almost completely. **Fishing flagships are
lean nearly without exception: 99.8% hold exactly `[fisher, chief]`** (surplus
in an arid/cold/barren biome, even multiplied by moisture, rarely clears the
0.4 threshold `shaman` needs). **Farming flagships stratify hard: 53.5% grow
the full five-rung `[slave, farmer, artisan, shaman, chief]`, and only 9.8%
stay at size 2.** Structure size correlates only weakly with population
directly (+0.18) — subsistence, not scale on its own, is the dominant lever,
because a farming settlement's fertility (0.9 or 0.7 by biome class) does
almost all of the work of clearing the surplus thresholds that scale alone
cannot.

{{#include generated/census-lands-drift/census-lands-drift-default-flagship-structure-size.svg}}

## The third unknown number: endorheic coverage

Campaign 4a banked a coarse drainage skeleton — flow direction and
accumulated upstream area, with any land cell whose downhill path never
reaches the sea flagged endorheic — as settlement's freshwater substrate,
without ever asking how much of a world's land that flag actually covers.
The census answers it, and the answer is larger than the "interior basin"
framing might suggest: **mean endorheic coverage is 46.8% of land cells**
(median 49.2%, standard deviation 21.9 points, ranging from near 0% to
92.9%). Interior drainage is not a rare edge case at this generator's
current parameters — on the median world, essentially half the land never
drains to the sea by the single-lowest-neighbor rule, banked substrate for
salt lakes and closed watersheds that a future campaign can spend without
regenerating a single world.

{{#include generated/census-lands-drift/census-lands-drift-default-endorheic-coverage.svg}}

## What the census does not yet say

Every settlement outside the flagship is placed, named, and populated, but
carries no committed subsistence or structure — a scope limit of the
composition root (spec §14), not a claim that a Census of the *whole*
scatter would be uninteresting; it is simply not yet a metric this registry
can extract, because it is not yet a fact any settlement but the flagship
commits. Inter-settlement questions — does a fishing hamlet's neighbor tend
to farm or fish itself, does structure size cluster geographically — wait on
that same extension. For now the third leg of the census stands next to the
sky and the land: built, calibrated against a fact the code already
guaranteed, and honest about the boundary between what it measures and what
it has not yet been asked to explain.
