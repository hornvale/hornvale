<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Terrain — The Domesday

The solid shape of a world: its plates, its elevation, and the landforms the sculpting pipeline leaves behind.

## Metrics

### `cave-fraction`

Fraction of land vertices with a cave

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.043212421 | 0.10950059 | 0.13043634 | 0.15630252 | 0.29659116 | 0.13457364 |

### `continent-count`

Connected land components at least 0.5% of the world's total land vertices (Task 9 iteration 3's size floor, Earth-calibrated: Greenland is ~1.4% of Earth's land and qualifies, Iceland ~0.07% does not) — the unfloored fringe of sub-floor fragments is preserved separately by landmass-count

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 6 | 7 | 12 | 6.077 |

### `deposit-density`

Fraction of land vertices with an ore deposit (The Lode, spec §5)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.22179258 | 0.31080327 | 0.34434733 | 0.38234423 | 0.52126984 | 0.34840292 |

### `dominant-commodity`

The most common land ore commodity by vertex count (The Lode, spec §5); Absent where no land vertex has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `salt` | 998 | 99.8% |
| `gold` | 2 | 0.2% |

### `dominant-rock`

The most common land rock class by vertex count, spec §4's fine taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `evaporite` | 632 | 63.2% |
| `andesite` | 234 | 23.4% |
| `sandstone` | 72 | 7.2% |
| `gneiss` | 27 | 2.7% |
| `conglomerate` | 20 | 2.0% |
| `granite` | 13 | 1.3% |
| `schist` | 1 | 0.1% |
| `shale` | 1 | 0.1% |

### `hypsometric-bimodality`

Ashman's D between land and ocean elevation populations (Earth is strongly bimodal); Absent when a world lacks land or ocean

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.4063312 | 3.6623659 | 3.8012563 | 3.9369721 | 4.4418667 | 3.7981195 |

### `landmass-count`

Every connected land component regardless of size — the unfloored companion continent-count superseded away from; reported alongside forever

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 21 | 160 | 200 | 241 | 397 | 197.835 |

### `largest-continent-share`

Largest land component's share of all land vertices; Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.17724491 | 0.30966108 | 0.3900553 | 0.52197375 | 0.99200237 | 0.43268579 |

### `mean-depth-to-basement`

Mean depth to crystalline basement over land (m) — the sedimentary archive's thickness.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 17.769476 | 35.704428 | 42.804893 | 50.051638 | 78.691159 | 43.112825 |

### `mean-geothermal-gradient`

Mean geothermal gradient over land (K/km) — the deep's energy base.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 22.686926 | 24.511396 | 24.939487 | 25.32018 | 26.651729 | 24.922329 |

### `mean-land-elevation-m`

Mean elevation above sea level over land vertices, m — the term the lapse rate turns into a temperature penalty. Land is `e >= sea`, matching `mountain-coverage`'s land definition; `mean-land-temperature-c` uses `!is_ocean(vertex)`, which is the same condition (`is_ocean` is `e < sea`), so the two metrics ARE mutually comparable — this is the coupling the campaign's lapse-rate regression rests on. Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1057.5306 | 1509.9738 | 1683.2863 | 1933.1953 | 2935.8547 | 1733.4459 |

### `mean-ore-grade`

Mean ore grade [0,1] over land vertices with a deposit (The Lode, spec §5); 0.0 where no land vertex has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.035138556 | 0.051492498 | 0.056990984 | 0.063094611 | 0.089209342 | 0.057512357 |

### `mountain-coverage`

Fraction of land vertices standing above 2000 m over the sea

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.12983401 | 0.31731985 | 0.38717559 | 0.47777655 | 0.72586081 | 0.3973183 |

### `ocean-fraction`

Fraction of globe vertices below sea level

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.4886236 | 0.55527074 | 0.61629071 | 0.68082613 | 0.76031444 | 0.61688172 |

### `plate-count`

Number of tectonic plates the globe drew or was pinned to

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `27` | 46 | 4.6% |
| `30` | 44 | 4.4% |
| `35` | 41 | 4.1% |
| `15` | 37 | 3.7% |
| `23` | 37 | 3.7% |
| `24` | 35 | 3.5% |
| `34` | 35 | 3.5% |
| `11` | 34 | 3.4% |
| `32` | 34 | 3.4% |
| `33` | 33 | 3.3% |
| `19` | 32 | 3.2% |
| `21` | 32 | 3.2% |
| `16` | 31 | 3.1% |
| `38` | 31 | 3.1% |
| `17` | 30 | 3.0% |
| `28` | 30 | 3.0% |
| `10` | 29 | 2.9% |
| `13` | 29 | 2.9% |
| `12` | 28 | 2.8% |
| `26` | 28 | 2.8% |
| `37` | 28 | 2.8% |
| `39` | 28 | 2.8% |
| `40` | 28 | 2.8% |
| `25` | 27 | 2.7% |
| `8` | 27 | 2.7% |
| `20` | 26 | 2.6% |
| `36` | 26 | 2.6% |
| `29` | 24 | 2.4% |
| `22` | 23 | 2.3% |
| `9` | 23 | 2.3% |
| `31` | 22 | 2.2% |
| `14` | 21 | 2.1% |
| `18` | 21 | 2.1% |

### `plate-size-gini`

Gini coefficient over plate vertex counts (Earth's plate sizes are heavy-tailed; uniform Voronoi scores low)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.25701382 | 0.65178653 | 0.70607485 | 0.74873663 | 0.85340293 | 0.69035659 |

### `sediment-volume`

Total deposited sediment volume proxy: Σ sediment thickness (meters) over every vertex, one vertex-area unit per vertex — the carve's own volume-proxy convention (spec §5): repose's receiver-side gains, routing's floodplain/ playa deposit, the marine wedge/delta fill, and atoll cap material, all summed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 654461.15 | 1114129.1 | 1290364.5 | 1462413.7 | 2103424 | 1292072.3 |

### `unconformity-fraction`

Fraction of land vertices recording a nonconformity (missing time) — the archive's floating gaps.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0081190798 | 0.23935189 | 0.33552498 | 0.43799582 | 0.85426262 | 0.34175567 |

### `unrest-coverage`

Fraction of vertices with tectonic unrest above 0.3

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.00043943167 | 0.013768859 | 0.020335921 | 0.028367755 | 0.061544846 | 0.02148567 |

### `warp-best-lift-erratic`

The Warp's best-class lift for erratic/scatter (spec §5.2) — see `warp-best-lift-spring`'s doc for the shared reading and the comparison it must be made against. The erratic's reading IS the null lift for H2's comparison: whatever it reads is what tuple cardinality alone buys on this seed.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.250576 | 1.7571359 | 1.9342585 | 2.1282313 | 3.4875363 | 1.9644644 |

### `warp-best-lift-overhang`

The Warp's best-class lift for overhang/hollow (spec §5.2) — see `warp-best-lift-spring`'s doc for the shared reading and the comparison it must be made against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 9.6930179 | 12.245395 | 14.968689 | 33.625287 | 11.559045 |

### `warp-best-lift-spring`

The Warp's best-class lift for spring/seep (spec §5.2, H2): the largest `P(Y | sign class) / P(Y)` over sign classes carrying at least 100 land facets — the walker-facing number, "features of this kind are N times as likely where the ground reads like this". **Compare it against `warp-best-lift-erratic` on the SAME seed, never against an absolute bar**: a lift above 1 arises from tuple cardinality alone, and the erratic — whose cause is a constant — is precisely a measurement of how much. `Absent` when the kind never occurs, or when no sign class clears the support floor.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 3.2726932 | 5.02473 | 8.8610375 | 31.128571 | 6.8163801 |

### `warp-best-lift-thicket`

The Warp's best-class lift for thicket/brake (spec §5.2) — see `warp-best-lift-spring`'s doc for the shared reading and the comparison it must be made against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.1179245 | 2.1519539 | 2.4859436 | 2.905307 | 5.5614214 | 2.5834781 |

### `warp-channel-mi-erratic`

The Warp's channel reading for erratic/scatter (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null. The erratic is the instrument's own negative control (spec §5.3): its cause is a CONSTANT, so this reading net of its null must be ~0 on every world, and if it is not, the instrument is crediting noise and that is the finding.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.010219635 | 0.020132102 | 0.024062111 | 0.028275088 | 0.038876719 | 0.02423233 |

### `warp-channel-mi-overhang`

The Warp's channel reading for overhang/hollow (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.025037618 | 0.089223216 | 0.10558373 | 0.11900474 | 0.16377558 | 0.10387985 |

### `warp-channel-mi-spring`

The Warp's channel reading for spring/seep (spec §5.2): discrete mutual information in bits between the walker's SIGN TUPLE — the biome word, the rock word, the steepness word and the wetness word, exactly as `windows/locale`'s room sentence renders them — and whether the kind occurs, over the same land-eligible population the `weft-*` grid metrics read. Distinct from `weft-legibility-mi-spring`, which reads the kind's own HIDDEN macro-state scalar: that is what the world knows, this is what the walker is told. **Never read alone.** A tuple of several hundred classes over ~11,000 facets carries a finite-sample bias of the same order as the signal, so the reading is this number MINUS `warp-channel-null-spring`, and the null is registered beside it for exactly that reason. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0070002208 | 0.029490466 | 0.03764218 | 0.045092648 | 0.078837676 | 0.037240398 |

### `warp-channel-mi-thicket`

The Warp's channel reading for thicket/brake (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.018563237 | 0.06375013 | 0.073401738 | 0.081399846 | 0.10924828 | 0.070550197 |

### `warp-channel-null-erratic`

The Warp's channel null for erratic/scatter (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction. For the erratic — whose cause is a constant — this null is essentially the whole of `warp-channel-mi-erratic`, which is what makes the pair the instrument's own negative control (spec §5.3).

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.010891239 | 0.019921009 | 0.024018462 | 0.027897303 | 0.03978229 | 0.024169837 |

### `warp-channel-null-overhang`

The Warp's channel null for overhang/hollow (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0076892005 | 0.019965682 | 0.024001926 | 0.028661545 | 0.04566027 | 0.024593781 |

### `warp-channel-null-spring`

The permutation null for `warp-channel-mi-spring` (spec §5.2): the same statistic averaged over five cyclic shifts of the occurrence bit vector (1,000 to 5,000 places in vertex order, over the land-only reading vector). A cyclic shift is a permutation, so both marginals are held exactly and every bit of what survives is the estimator's own finite-sample bias at this tuple's cardinality. Subtract it from `warp-channel-mi-spring` to get the reading; a channel MI at its null is bias, not legibility. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0039063673 | 0.013626106 | 0.016437259 | 0.019086729 | 0.030091936 | 0.016347556 |

### `warp-channel-null-thicket`

The Warp's channel null for thicket/brake (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.01081097 | 0.026634783 | 0.033943059 | 0.040493278 | 0.060396147 | 0.033569544 |

### `warp-false-sign-net-erratic`

The Warp's false-sign control for erratic/scatter (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.001324275 | -0.00026381909 | -0.000016115149 | 0.00024089612 | 0.0013844237 | 0.0000019808949 |

### `warp-false-sign-net-overhang`

The Warp's false-sign control for overhang/hollow (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0013313884 | -0.00026393631 | -0.000004659806 | 0.00024173725 | 0.0015467971 | 0.0000061388691 |

### `warp-false-sign-net-spring`

The Warp's false-sign control for spring/seep (spec §5.2/§5.3, H4): the channel reading NET OF ITS OWN NULL for a tuple of pure address noise — the room's `relief`, `aspect` and `openness` micro-habitat axes, each cut at the same threshold the wetness word is cut at. Those three are drawn from the facet's address seed and correlate with nothing the world knows, so the instrument must credit them nothing: spec §7's H4 bars this within +/- 0.002 bits — four standard deviations of this 27-class tuple's own null estimator, amended 2026-09-05 from +/- 0.001, which sat below the estimator's resolution — and a reading outside it is a finding about the INSTRUMENT, not about the world. **The descriptor noun is deliberately not in this tuple**, though spec §5.2's table names it: `windows/locale/src/grammar.rs` exposes no `pub fn`, so the noun's variety draw is unreachable from the lab without rendering a whole document per facet — and its pool is keyed on `(formation, stratum, substrate)`, so it is partly biome-correlated and would have been the weakest member of a control anyway. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0014894406 | -0.00026184759 | -0.000024804762 | 0.00022555866 | 0.0021362221 | -0.0000052432095 |

### `warp-false-sign-net-thicket`

The Warp's false-sign control for thicket/brake (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0010979149 | -0.00023834382 | -0.00002621092 | 0.00024909813 | 0.0015221017 | 0.0000088505262 |

### `warp-found-fraction-erratic`

The Warp's found fraction for erratic/scatter (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading. **Always `Absent` for the erratic, by construction**: its `macro_state` is a constant, so "the share of occurrences standing on a strong cause" names no quantity. Absent here is the honest value, deliberately not 0.0 — a kind with no cause and a kind whose occurrences all miss their cause are different facts.

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `warp-found-fraction-overhang`

The Warp's found fraction for overhang/hollow (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.51724138 | 0.74705882 | 0.78048673 | 0.80516432 | 0.88478582 | 0.77454493 |

### `warp-found-fraction-spring`

The Warp's found fraction for spring/seep (spec §5.2, H1): the share of this kind's occurrences standing on a facet whose own `macro_state` reads at or above 0.5 — how much of what a walker meets was FOUND at a cause rather than extruded by the recipe's noise floor over the other 90-odd per cent of land. This is the number the campaign's premise measurement is about: before The Warp, 333 of seed 42's 403 springs stood on a facet with no cause at all. `Absent` when the kind never occurs on land.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.29411765 | 0.39524751 | 0.45833333 | 0.59223301 | 0.36742574 |

### `warp-found-fraction-thicket`

The Warp's found fraction for thicket/brake (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.27540107 | 0.34892685 | 0.41135371 | 0.56092843 | 0.32763938 |

### `warp-learner-gain-erratic`

The Warp's learner gain for erratic/scatter (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback. This is the negative control: nothing about the erratic's occurrence depends on any sign, so the table cannot BEAT the base rate out of sample, and a reading that did would mean the instrument was crediting noise. It can and does LOSE — seed 42 reads -0.0138 bits/facet, the price of fitting several hundred classes of pure noise on half the land and being scored on the other half. Spec §7's H3 therefore bars this ONE-SIDED at <= 0.001 bits/facet (amended 2026-09-05 from "within +/- 0.001", which no held-out table over ~469 classes could ever meet): the control is that the erratic never GAINS, not that its loss is small.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.020971922 | -0.013138635 | -0.011318296 | -0.0094516726 | -0.0042607527 | -0.011372652 |

### `warp-learner-gain-overhang`

The Warp's learner gain for overhang/hollow (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.011393923 | 0.06731584 | 0.079681029 | 0.092237064 | 0.14472993 | 0.07893952 |

### `warp-learner-gain-spring`

The Warp's learner gain for spring/seep (spec §5.2, H3): the `P(Y | sign class)` table fitted on the EVEN-indexed land facets and scored on the ODD ones, as mean log-loss reduction against the base rate, in bits per facet. Positive means the words a walker is told genuinely help predict this kind on facets the table never saw; NEGATIVE means the table overfits, which is reported rather than clamped. The index is the facet's position in the land-only vector in vertex order, so the two halves interleave across the whole globe rather than splitting it by region. Smoothing is an EQUIVALENT-SAMPLE-SIZE PRIOR TOWARD THE BASE RATE: a class with `n` fit facets and `k` hits predicts `(k + a * p) / (n + a)`, for `a` ten facets and `p` the fit half's own base rate, so a thin or unseen class predicts the base rate and scores exactly zero, never below it. It was Laplace `(k + 1) / (n + 2)` in this metric's first implementation and that was a defect (controller ruling, ledger #10): Laplace is a prior toward 0.5, these kinds occur on 3-14% of land, and with 469 sign classes over ~11,000 facets most classes are thin enough for that prior to dominate — the table lost to the base rate in sample, which was a fact about the prior and not about the world. Read against `warp-oracle-gain-spring`, the same table's in-sample reading. `Absent` on a world with no land-eligible facet, and on one whose fit half carries no occurrence of this kind at all (or nothing but occurrences): there is no base rate to beat.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0019901145 | 0.012043768 | 0.017536193 | 0.023849745 | 0.046415005 | 0.017912091 |

### `warp-learner-gain-thicket`

The Warp's learner gain for thicket/brake (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0064692419 | 0.016935738 | 0.024075015 | 0.029834653 | 0.051789609 | 0.023126507 |

### `warp-max-class-rate-erratic`

The Warp's wallpaper guard for erratic/scatter (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar. For the erratic this is the base rate plus sampling noise, since no sign class has any relationship to it.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.048780488 | 0.070866142 | 0.07826087 | 0.087378641 | 0.14018692 | 0.079616216 |

### `warp-max-class-rate-overhang`

The Warp's wallpaper guard for overhang/hollow (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.35245902 | 0.4109409 | 0.44354839 | 0.56481481 | 0.34608215 |

### `warp-max-class-rate-spring`

The Warp's wallpaper guard for spring/seep (spec §5.2, H5): the largest `P(Y | sign class)` over sign classes carrying at least 100 land facets — the absolute rate behind `warp-best-lift-spring`'s ratio. H5 asks that no class exceed 0.75 on any seed: above that the sign kind has stopped distinguishing a place and become wallpaper, and the reading is a finding about the recipe's high end, not a success. `Absent` when no sign class clears the support floor.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.03030303 | 0.058823529 | 0.10958904 | 0.56910569 | 0.08518315 |

### `warp-max-class-rate-thicket`

The Warp's wallpaper guard for thicket/brake (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.098360656 | 0.26253687 | 0.3029236 | 0.336 | 0.47058824 | 0.2964892 |

### `warp-oracle-gain-erratic`

The Warp's oracle gain for erratic/scatter (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated. The erratic's in-sample reading is the ceiling overfitting alone can reach on this population, which is why it is worth registering even though nothing predicts the erratic.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0076740393 | 0.014356826 | 0.016795314 | 0.01926047 | 0.025747837 | 0.016809136 |

### `warp-oracle-gain-overhang`

The Warp's oracle gain for overhang/hollow (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.021520953 | 0.080640952 | 0.09530568 | 0.10691544 | 0.14977824 | 0.093684725 |

### `warp-oracle-gain-spring`

The Warp's oracle gain for spring/seep (spec §5.2): the SAME `P(Y | sign class)` table as `warp-learner-gain-spring`, fitted and scored on ALL land facets with no split, under the same equivalent-sample-size prior toward the base rate. It is not a legibility reading on its own — an in-sample table always looks better than it is — and it is REPORTED, NEVER GATED. Spec §7's H3 originally asked the learner to reach at least half of this number, and that clause is WITHDRAWN (controller ruling, ledger #10; spec §7's H3 amendment): a ratio needs a denominator whose sign is fixed, and this one has none — under the withdrawn Laplace prior it read NEGATIVE for spring and erratic at seed 42, which would make "at least half of it" satisfiable by being worse. What it is good for is the difference a reader takes, how much of the in-sample reading survives the held-out half. `Absent` under the same conditions as `warp-learner-gain-spring`, over the whole land population rather than a half of it.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0061744051 | 0.025971194 | 0.032670337 | 0.039271418 | 0.066641895 | 0.032447959 |

### `warp-oracle-gain-thicket`

The Warp's oracle gain for thicket/brake (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.015773156 | 0.048685006 | 0.055952326 | 0.061792022 | 0.084035565 | 0.054001233 |

### `weft-coherence-morans-i-erratic`

H2's coherence readout for erratic/scatter (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion. Erratic's short (5-facet) correlation length predicts the LOWEST of the four readings here, not zero: H2 is about spatial texture existing at all, which a short correlation length still gives, unlike H3's macro-state legibility, which erratic is built to score near zero on.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.47012084 | 0.56179154 | 0.5854585 | 0.60765239 | 0.70214861 | 0.58429512 |

### `weft-coherence-morans-i-overhang`

H2's coherence readout for overhang/hollow (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion.

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.00014011361 | 0.8971447 | 0.91523488 | 0.93064154 | 0.99467961 | 0.90993268 |

### `weft-coherence-morans-i-spring`

H2's readout for spring/seep (spec §7): Moran's I over the binary occurrence indicator, sampled along land-eligible walked paths (the same 78-walk, 4,680-step pool `weft-encounter-rate-spring` reads), weighted by within-walk chain adjacency (step `s` and `s+1` of the same walk). **This is a construction-validation (a regression guard against address-hashed speckle), not independent evidence the surface is "coherent" in a stronger sense** — `occurs` thresholds a position-continuous field, so a positive reading is near-guaranteed by construction; the discriminating power lives in `weft_prevalence.rs`'s real-vs-mutant table (real 0.998, mutant 0.209 for this kind). No numeric floor is preregistered for this statistic (spec §7 froze none); a positive reading well clear of zero over a non-degenerate occurs-count is the qualitative claim, checked against `weft-coherence-occurs-count-spring`, the anti-vacuity companion (The Ford's shape) — see that metric's own doc for why a small count makes a high reading here suspect. NOT geosphere vertex adjacency — measured on this tree, that mesh's own spacing is ~106-127 facets per step, 1.9-23x every kind's own correlation length (5-60 facets), so both a sound construction and an address-hashed defect predict `I ~= 0` at that scale — see `weft_morans_i`'s own doc for the full power argument and the discarded vertex-adjacency readings, published in full rather than discarded silently. `Absent` if the walk pool is empty or the indicator has zero variance across every walked step.

n = 856 present, 144 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0000038182367 | 0.96134058 | 0.98063194 | 0.99134559 | 1 | 0.96170497 |

### `weft-coherence-morans-i-thicket`

H2's coherence readout for thicket/brake (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.89596937 | 0.96030295 | 0.96568249 | 0.96973602 | 0.98384739 | 0.96452519 |

### `weft-coherence-occurs-count-erratic`

H2's anti-vacuity companion for erratic/scatter — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 113 | 218 | 265 | 316 | 471 | 268.811 |

### `weft-coherence-occurs-count-overhang`

H2's anti-vacuity companion for overhang/hollow — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 131 | 192.5 | 267 | 609 | 205.323 |

### `weft-coherence-occurs-count-spring`

H2's anti-vacuity companion for spring/seep (The Ford's shape: `channel-band-monotonicity` paired with `channel-transect-dry-reach`): the raw count of WALKED steps (the same pool `weft-coherence-morans-i-spring` computes its chain adjacency over, and `weft-encounter-rate-spring`'s own numerator) where spring/seep occurred. Moran's I's own denominator is `n*p*(1-p)` for a binary indicator at rate `p`, which shrinks toward zero as occurrence becomes very rare (or very common) — so a small reading here is the signal that a neighbouring high Moran's-I reading may be resting on a handful of adjacent hits rather than a genuine spatial process, exactly as `channel-transect-dry-reach` flags a monotonicity score resting on transects truncated before they could fail.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 23 | 64.5 | 124 | 449 | 82.021 |

### `weft-coherence-occurs-count-thicket`

H2's anti-vacuity companion for thicket/brake — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 128 | 577 | 744 | 933 | 1750 | 752.764 |

### `weft-encounter-rate-any`

H1's observation-scoped density number, UNIONED over all four kinds (spec §7): a walked step counts once even if it carries more than one kind's feature. Read beside `weft-existence-density-any` — the two are H1's promised "two numbers, separately reported", and they need not agree: a short-correlation-length kind can be common god's-eye and rare along any one path, or the reverse for a long one.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.075198413 | 0.15969945 | 0.1842833 | 0.210625 | 0.29751773 | 0.18530433 |

### `weft-encounter-rate-erratic`

H1's observation-scoped density number for erratic/scatter (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.022161172 | 0.035854342 | 0.040176794 | 0.044270833 | 0.062345679 | 0.04022236 |

### `weft-encounter-rate-overhang`

H1's observation-scoped density number for overhang/hollow (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.02029703 | 0.029210702 | 0.039885057 | 0.095940171 | 0.030835722 |

### `weft-encounter-rate-spring`

H1's observation-scoped density number for spring/seep (spec §7): features of this kind met per facet of travel, pooled over land-eligible walks sampled the same way `weft_prevalence.rs`'s own `land_eligible_walks` does (60-step walks from 137-spaced starting vertices, kept only if land-eligible throughout). Distinct from `weft-existence-density-spring`'s god's-eye reading — a walker only ever samples the facets on their own path, never the whole grid, so this is the number that actually answers "how often does a traveller meet one".

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.003525641 | 0.010621142 | 0.017635659 | 0.060087719 | 0.011845939 |

### `weft-encounter-rate-thicket`

H1's observation-scoped density number for thicket/brake (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.018390805 | 0.088227513 | 0.11445189 | 0.1395664 | 0.21882353 | 0.11391952 |

### `weft-existence-density-any`

H1's god's-eye density number, UNIONED over all four kinds (spec §7): the fraction of land-eligible facets carrying ANY derived feature. A facet holding two kinds counts once, the same union discipline `channel-band-monotonicity`'s sibling readouts and `site-density`-style metrics use elsewhere. This is the single number H1's `>= 3 orders of magnitude over the placed baseline` claim is actually checked against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.11477962 | 0.16945107 | 0.19137055 | 0.21056557 | 0.26137818 | 0.18933334 |

### `weft-existence-density-erratic`

H1's god's-eye density number for erratic/scatter (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-erratic`. Erratic is the negative control for H3, not for H1: nothing about density predicts erratic should read low here.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034304362 | 0.039272256 | 0.040593579 | 0.041775852 | 0.046473792 | 0.040563806 |

### `weft-existence-density-overhang`

H1's god's-eye density number for overhang/hollow (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-overhang`.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0039958663 | 0.023220641 | 0.029720821 | 0.036022278 | 0.057341961 | 0.029763462 |

### `weft-existence-density-spring`

H1's god's-eye density number for spring/seep (spec §7): the fraction of LAND-ELIGIBLE facets (spec §7's amendment population) carrying a spring/seep occurrence, over a vertex-centred subsample — one representative facet per geosphere vertex (`n = 40,962` at `hornvale_terrain::GLOBE_LEVEL = 6`), a 1-in-9,830 sample of the ~4e8 walk-depth facets on the grid, not every one of them; the same resolution spec §7's own gate-component diagnostic reads at. Distinct from `weft-encounter-rate-spring`, which reads the SAME kind's occurrence along a walked path — H1 is two numbers because discovery is observation-scoped and existence density alone (what The Prospect's H3 measured) answers a different question from what a walker actually meets. No threshold is preregistered for this reading alone; H1's claim is the >= 3-orders-of-magnitude density gain over the placed baseline `site-density`-style metrics measure, read across every kind together.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0010532363 | 0.0082644628 | 0.011653577 | 0.014815321 | 0.026010575 | 0.011582061 |

### `weft-existence-density-thicket`

H1's god's-eye density number for thicket/brake (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-thicket`.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.043057836 | 0.098558549 | 0.12222773 | 0.14202672 | 0.19256077 | 0.11915168 |

### `weft-legibility-mi-erratic`

H3's legibility readout for erratic/scatter (spec §7, allowed to fail) — see `weft-legibility-mi-spring`'s doc for the shared estimator. Erratic is H3's own negative control (contextuality ~0, a CONSTANT `macro_state`) and is predicted LOWEST of the four, near zero: mutual information between any variable and a constant is algebraically zero, so this metric's own construction predicts the near-zero reading before any world is measured. If it does NOT read near zero, the instrument is measuring something other than legibility and that is the finding, per spec §7 and this task's own brief. This piece of the ordering held: erratic's reading (0.000000 at seed 42) stayed lowest through The Warp's re-parameterisation of the sign kinds, even as the ordering elsewhere falsified again — see `weft-legibility-mi-spring`'s doc for the record.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `weft-legibility-mi-overhang`

H3's legibility readout for overhang/hollow (spec §7, allowed to fail) — see `weft-legibility-mi-spring`'s doc for the shared estimator and its falsification record. Overhang was predicted THIRD of the four, gentler than spring/thicket; it now reads 0.076536 at seed 42, ABOVE thicket's 0.038604.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.016780485 | 0.07771766 | 0.093186011 | 0.10606578 | 0.15248762 | 0.091841615 |

### `weft-legibility-mi-spring`

H3's legibility readout for spring/seep (spec §7, allowed to fail): discrete mutual information in bits between spring/seep's own blended macro-state signal (binned into 4 equal-width bins over its documented `[0,1]` range) and whether it occurred, over the land-eligible population (spec §7's amendment: restricting to land-eligible facets makes the shared eligibility gate a constant, so it contributes zero MI by construction and cannot flatter any kind — see the spec's own amendment paragraph for the whole-sphere confound this removes). The preregistered claim WAS the ORDERING `spring > thicket > overhang > erratic`, not a threshold on this reading alone: spring is H3's own sign case ("diagnostic of what is underfoot"), so it was predicted HIGHEST of the four. The Weft measured this ordering FALSE (`thicket > spring`, 2026-09-04); The Warp's re-parameterisation of the sign kinds then moved spring's reading to 0.086464 and overhang's to 0.076536 at seed 42, while thicket (0.038604) and erratic (0.000000) are unchanged, falsifying the ordering again — now on `thicket > overhang`. This metric is a RECORD of that original prediction, not a live claim.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0048572403 | 0.038968242 | 0.055697627 | 0.069805451 | 0.11194024 | 0.0544952 |

### `weft-legibility-mi-thicket`

H3's legibility readout for thicket/brake (spec §7, allowed to fail) — see `weft-legibility-mi-spring`'s doc for the shared estimator and its falsification record. Thicket was predicted SECOND of the four; it now reads 0.038604 at seed 42, BELOW overhang's 0.076536.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0044424108 | 0.033391808 | 0.039764509 | 0.045710456 | 0.072919639 | 0.03880245 |

## Weaknesses found here

### `dominant-commodity`

- **D1**: "salt" holds 998/1000 worlds (99.8%), at or above the 80% threshold

### `weft-coherence-morans-i-overhang`

- **D3**: p25..p75 spans 0.03349683999999997 (3.37% of the 0.99481972361 min..max range), under the 5% bar

### `weft-coherence-morans-i-spring`

- **D3**: p25..p75 spans 0.03000501 (3.00% of the 1.0000038182367 min..max range), under the 5% bar

### `weft-legibility-mi-erratic`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

