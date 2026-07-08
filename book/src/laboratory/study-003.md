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

**A note on the two-peoples re-baseline.** This chapter's numbers were
first measured against a single-species (goblin-only) default. Campaign
Y2-1 added a second species, kobolds, competing for the same settlement
sites through joint placement — and per that campaign's re-baseline
clause, every number below that placement touches shifted, some
substantially. Updated figures are marked inline; the full per-species
detail — coastal rates, settlement counts, role ladders, and the
competitive-exclusion phenomenon two-species placement turned up — is
[Study 006](study-006.md), not repeated here.

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

Across the 10,000-world census it holds exactly, and — re-run after the
Campaign Y2-0 placement fix — the exactness now surfaces all four legal
outputs of the function, not two. Through Campaign 4b's original run the
placement suitability formula gave a coastal cell a `freshwater` score of
1.0 outright plus its own coastal bonus, an edge no inland site could ever
match; every flagship in that sample was coastal, and herding and foraging
never appeared. Campaign Y2-0 removed that coastal-freshwater conflation
(`fix: remove coastal-freshwater conflation in settlement placement`), so a
cell's freshwater score is no longer a free gift of being coastal. The
100%-coastal invariant broke, but the underlying skew did not: of the 9,997
worlds that place a flagship at all, **9,931 (99.3%) are still coastal and
only 66 (0.7%) are inland** — a coastal cell remains a heavily favored site,
just no longer a guaranteed one. Those 66 inland flagships are exactly where
herding and foraging finally surface: **farming, 87.7% (8,768 worlds);
fishing, 12.1% (1,213 worlds, all of them coastal); foraging, 0.1% (12
worlds); and herding, 0.0% (4 worlds)** — all sixteen herding-or-foraging
flagships are inland, and the other 50 inland flagships still farm (forest
and grassland do not need a coast). Herding and foraging were always legal
outputs of the function; it took a placement fix that let an inland cell
occasionally win the argmax for the census to actually observe one.

**Superseded by the two-peoples re-baseline.** The paragraph above
describes the single-species default; it no longer describes the world a
fresh census generates. Once kobolds compete for sites, they claim almost
every inland cell a lone goblin population would otherwise have won, and
the finding above **fully reverses**: at 10k, **all 9,972 present goblin
flagships are coastal (100.0%, zero inland)**, and the goblin subsistence
split collapses to **farming 85.8% (8,558 worlds) and fishing 14.2% (1,414
worlds) — zero foraging, zero herding**. Nothing changed in
`subsistence(BiomeClass, coastal)` itself; herding and foraging remain
legal outputs of the same function, they simply have no goblin flagship
left to attach to, because kobolds now hold the inland sites that would
have needed them. See [Study 006](study-006.md) for the kobold-side
figures (85.3% coastal — kobolds remain measurably less coastal than
goblins, the campaign's first preregistered hypothesis) and the
competitive-exclusion worlds this reversal sits alongside.

{{#include generated/census-lands-drift/census-lands-drift-default-flagship-subsistence.svg}}

## The first unknown number: how many settlements, and how big

Nobody knew, before this census, how many settlements a generated world
would place, or how tightly that number would cluster. **Mean settlement
count: 56.7 (median 58, standard deviation 21.9), ranging from 0 to 123.**
Three worlds in the sample place none at all. Two are exactly the worlds
Study 002 flags as reaching 0% habitable fraction — the census's own
cross-check that a world with no habitable land can place no settlement,
regardless of how low the suitability floor is set. The third, new since
the Campaign Y2-0 fix, is seed 1009: its habitable fraction is 0.01%, not
exactly zero, but the land it does have is too sparse and fragmented for
even one 12°-separated site to clear the suitability floor at the corrected
weights. Settlement count still correlates strongly with habitable fraction
(**+0.89** across the full census, unchanged from the pre-fix run) — the
more room the biome map leaves habitable, the more spaced-apart
12°-separated sites there are room for, exactly the relationship Campaign
4a's placement model would predict without a single additional draw.

**Updated by the two-peoples re-baseline.** With two species now placing
settlements across the same worlds, total settlement count rises slightly
to **mean 58.4 (median 60, standard deviation 22.4, range 0–124)**, and —
for the first time — splits by species: kobolds place fewer settlements
than goblins on the same worlds (mean 26.8 vs. 31.6). The same three
zero-habitable-fraction seeds (895, 1009, 4322) still place nothing at
all, for either species, and the habitable-fraction correlation is
unchanged in strength (+0.90). Mean population per world moves from 289.2
to **294.9**. Full per-species distributions are
[Study 006](study-006.md).

{{#include generated/census-lands-drift/census-lands-drift-default-settlement-count.svg}}

**Mean population per world is far tighter than the settlement count that
produces it: 289.2 (median 286.3, standard deviation 29.8, over a 133–402
range).** Individual settlements draw population from `40 + 460 × suitability`
jittered ±25% — a wide per-settlement range — but a world's *mean* across
dozens of settlements is an average over many draws, and averages narrow.
The mean itself is lower than the pre-Campaign-Y2-0 run's 353.8, because the
same freshwater fix that let inland cells win a flagship argmax also lowers
the suitability — and so the population draw — of every settlement across
the whole scatter, not only the flagship's site. The census is the first
measurement of how much a world's mean narrows at this generator's scale.

{{#include generated/census-lands-drift/census-lands-drift-default-mean-population.svg}}

## The second unknown number: how stratified a flagship grows

`flagship-structure-size` counts the castes an emergent structure produces —
a stratification proxy running from 2 (a lone worker and a chief) to 6
(every rung the model can grow). Its distribution shifted with the Campaign
Y2-0 placement fix, and shifted toward the top: **15.2% of flagships hold
size 2, 7.4% hold size 3, 4.5% hold size 4, 72.7% hold size 5, and size 6 is
rare (0.1%)** — size 5 is now the overwhelming mode, where the pre-fix run
split more evenly across sizes 2, 3, and 5. The subsistence split still
explains why almost completely. **Fishing flagships are lean nearly without
exception: 99.9% hold exactly `[fisher, chief]`** (surplus in an
arid/cold/barren biome, even multiplied by moisture, rarely clears the 0.4
threshold `shaman` needs). **Farming flagships stratify hard: 82.9% grow the
full five-rung `[slave, farmer, artisan, shaman, chief]`, and only 3.4% stay
at size 2.** The 16 new herding and foraging flagships are uniformly lean —
every one of them holds size 2, the same `[fisher, chief]`-shaped floor as a
fishing settlement, unsurprising for a subsistence mode built around the
same low-surplus arid/cold/barren biomes. Structure size correlates more
with population than the pre-fix run showed (+0.34, up from +0.18) but
subsistence remains the dominant lever, because a farming settlement's
fertility (0.9 or 0.7 by biome class) does most of the work of clearing the
surplus thresholds that scale alone cannot.

**Updated by the two-peoples re-baseline.** Since the herding and foraging
flagships above no longer occur once kobolds compete for inland sites (see
the calibration section above), the distribution shifts again: **17.4%
hold size 2, 8.9% hold size 3, 5.0% hold size 4, 68.7% hold size 5, and
size 6 is not observed at all (0.0%)** in the 10k re-baseline census —
size 6 required exactly the rare herding/foraging-plus-`warrior`
combination that the goblin side no longer produces. The
population–structure-size correlation is essentially unchanged (+0.35).
Kobold structures, absent from this metric entirely (it stays
goblin-pointed for continuity), have their own ceiling and shape recorded
in [Study 006](study-006.md).

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
