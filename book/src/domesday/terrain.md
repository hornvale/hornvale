<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Terrain — The Domesday

The solid shape of a world: its plates, its elevation, and the landforms the sculpting pipeline leaves behind.

## Metrics

### `cave-fraction`

Fraction of land vertices with a cave

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034492274 | 0.081764541 | 0.096868579 | 0.115397 | 0.20860729 | 0.099646079 |

### `continent-count`

Connected land components at least 0.5% of the world's total land vertices (Task 9 iteration 3's size floor, Earth-calibrated: Greenland is ~1.4% of Earth's land and qualifies, Iceland ~0.07% does not) — the unfloored fringe of sub-floor fragments is preserved separately by landmass-count

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 6 | 7 | 12 | 6.092 |

### `deposit-density`

Fraction of land vertices with an ore deposit (The Lode, spec §5)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.24181566 | 0.352657 | 0.38918014 | 0.43282997 | 0.58044349 | 0.39270569 |

### `dominant-commodity`

The most common land ore commodity by vertex count (The Lode, spec §5); Absent where no land vertex has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `salt` | 999 | 99.9% |
| `iron` | 1 | 0.1% |

### `dominant-rock`

The most common land rock class by vertex count, spec §4's fine taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `evaporite` | 476 | 47.6% |
| `gneiss` | 294 | 29.4% |
| `andesite` | 158 | 15.8% |
| `sandstone` | 49 | 4.9% |
| `conglomerate` | 13 | 1.3% |
| `granite` | 10 | 1.0% |

### `hypsometric-bimodality`

Ashman's D between land and ocean elevation populations (Earth is strongly bimodal); Absent when a world lacks land or ocean

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.4084567 | 3.6802133 | 3.8196246 | 3.9550742 | 4.464955 | 3.8156322 |

### `landmass-count`

Every connected land component regardless of size — the unfloored companion continent-count superseded away from; reported alongside forever

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 21 | 159 | 202 | 244 | 388 | 199.747 |

### `largest-continent-share`

Largest land component's share of all land vertices; Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.1587584 | 0.30979371 | 0.38887345 | 0.52104859 | 0.99200276 | 0.43124834 |

### `mean-depth-to-basement`

Mean depth to crystalline basement over land (m) — the sedimentary archive's thickness.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 17.66636 | 36.032146 | 43.133104 | 50.334668 | 83.804049 | 43.505972 |

### `mean-geothermal-gradient`

Mean geothermal gradient over land (K/km) — the deep's energy base.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 22.686095 | 24.500174 | 24.934778 | 25.311537 | 26.652152 | 24.918351 |

### `mean-land-elevation-m`

Mean elevation above sea level over land vertices, m — the term the lapse rate turns into a temperature penalty. Land is `e >= sea`, matching `mountain-coverage`'s land definition; `mean-land-temperature-c` uses `!is_ocean(vertex)`, which is the same condition (`is_ocean` is `e < sea`), so the two metrics ARE mutually comparable — this is the coupling the campaign's lapse-rate regression rests on. Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1063.6052 | 1525.0853 | 1700.2625 | 1951.7233 | 2942.0455 | 1747.9432 |

### `mean-ore-grade`

Mean ore grade [0,1] over land vertices with a deposit (The Lode, spec §5); 0.0 where no land vertex has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034039592 | 0.056138445 | 0.062634712 | 0.069759128 | 0.095971967 | 0.062830804 |

### `mountain-coverage`

Fraction of land vertices standing above 2000 m over the sea

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.13071099 | 0.3202934 | 0.39027332 | 0.48373203 | 0.72677542 | 0.40076979 |

### `ocean-fraction`

Fraction of globe vertices below sea level

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.48867243 | 0.55644256 | 0.61714516 | 0.68331624 | 0.75999707 | 0.61835909 |

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
| 620405.23 | 1077733.1 | 1243488.9 | 1418774.6 | 2065159.6 | 1252118 |

### `unconformity-fraction`

Fraction of land vertices recording a nonconformity (missing time) — the archive's floating gaps.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.00734167 | 0.23918433 | 0.33543373 | 0.43654459 | 0.85466887 | 0.34133031 |

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
| 1.1189581 | 1.7677476 | 1.944595 | 2.1392926 | 3.1738281 | 1.9733415 |

### `warp-best-lift-overhang`

The Warp's best-class lift for overhang/hollow (spec §5.2) — see `warp-best-lift-spring`'s doc for the shared reading and the comparison it must be made against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 10.140024 | 12.153037 | 14.895216 | 42.168844 | 12.766964 |

### `warp-best-lift-spring`

The Warp's best-class lift for spring/seep (spec §5.2, H2): the largest `P(Y | sign class) / P(Y)` over sign classes carrying at least 100 land facets — the walker-facing number, "features of this kind are N times as likely where the ground reads like this". **Compare it against `warp-best-lift-erratic` on the SAME seed, never against an absolute bar**: a lift above 1 arises from tuple cardinality alone, and the erratic — whose cause is a constant — is precisely a measurement of how much. `Absent` when the kind never occurs, or when no sign class clears the support floor.

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 4.7873826 | 7.3169828 | 10.330553 | 24.840333 | 7.8080876 |

### `warp-best-lift-thicket`

The Warp's best-class lift for thicket/brake (spec §5.2) — see `warp-best-lift-spring`'s doc for the shared reading and the comparison it must be made against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.1080156 | 2.1748358 | 2.5089381 | 2.9436745 | 5.6714337 | 2.6082014 |

### `warp-channel-mi-erratic`

The Warp's channel reading for erratic/scatter (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null. The erratic is the instrument's own negative control (spec §5.3): its cause is a CONSTANT, so this reading net of its null must be ~0 on every world, and if it is not, the instrument is crediting noise and that is the finding.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0089921415 | 0.019713208 | 0.023869273 | 0.027605298 | 0.03891071 | 0.023774449 |

### `warp-channel-mi-overhang`

The Warp's channel reading for overhang/hollow (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.028483836 | 0.10752783 | 0.12473126 | 0.14119693 | 0.18990703 | 0.12353305 |

### `warp-channel-mi-spring`

The Warp's channel reading for spring/seep (spec §5.2): discrete mutual information in bits between the walker's SIGN TUPLE — the biome word, the rock word, the steepness word and the wetness word, exactly as `windows/locale`'s room sentence renders them — and whether the kind occurs, over the same land-eligible population the `weft-*` grid metrics read. Distinct from `weft-legibility-mi-spring`, which reads the kind's own HIDDEN macro-state scalar: that is what the world knows, this is what the walker is told. **Never read alone.** A tuple of several hundred classes over ~11,000 facets carries a finite-sample bias of the same order as the signal, so the reading is this number MINUS `warp-channel-null-spring`, and the null is registered beside it for exactly that reason. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0000000000000046703302 | 0.033891695 | 0.043274246 | 0.052765966 | 0.093199512 | 0.042488684 |

### `warp-channel-mi-thicket`

The Warp's channel reading for thicket/brake (spec §5.2) — see `warp-channel-mi-spring`'s doc for the shared estimator and why it is never read without its null.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.018461099 | 0.062864982 | 0.072707531 | 0.080655534 | 0.10776462 | 0.069932851 |

### `warp-channel-null-erratic`

The Warp's channel null for erratic/scatter (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction. For the erratic — whose cause is a constant — this null is essentially the whole of `warp-channel-mi-erratic`, which is what makes the pair the instrument's own negative control (spec §5.3).

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.010186685 | 0.01975399 | 0.023637595 | 0.027420942 | 0.038157328 | 0.023652425 |

### `warp-channel-null-overhang`

The Warp's channel null for overhang/hollow (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.008407995 | 0.021370857 | 0.025604281 | 0.030455979 | 0.051712553 | 0.026132691 |

### `warp-channel-null-spring`

The permutation null for `warp-channel-mi-spring` (spec §5.2): the same statistic averaged over five cyclic shifts of the occurrence bit vector (1,000 to 5,000 places in vertex order, over the land-only reading vector). A cyclic shift is a permutation, so both marginals are held exactly and every bit of what survives is the estimator's own finite-sample bias at this tuple's cardinality. Subtract it from `warp-channel-mi-spring` to get the reading; a channel MI at its null is bias, not legibility. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0000000000000046703302 | 0.013824981 | 0.016856497 | 0.019657127 | 0.032315449 | 0.01662515 |

### `warp-channel-null-thicket`

The Warp's channel null for thicket/brake (spec §5.2) — see `warp-channel-null-spring`'s doc for the shared construction.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.010935354 | 0.025941634 | 0.033145174 | 0.039473084 | 0.058970182 | 0.032918216 |

### `warp-false-sign-net-erratic`

The Warp's false-sign control for erratic/scatter (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.001537693 | -0.00026418573 | -0.000016629551 | 0.00023881191 | 0.0015891527 | -0.0000047772107 |

### `warp-false-sign-net-overhang`

The Warp's false-sign control for overhang/hollow (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0012128733 | -0.00025646863 | -0.00001966214 | 0.00025688055 | 0.0018865699 | 0.0000059684416 |

### `warp-false-sign-net-spring`

The Warp's false-sign control for spring/seep (spec §5.2/§5.3, H4): the channel reading NET OF ITS OWN NULL for a tuple of pure address noise — the room's `relief`, `aspect` and `openness` micro-habitat axes, each cut at the same threshold the wetness word is cut at. Those three are drawn from the facet's address seed and correlate with nothing the world knows, so the instrument must credit them nothing: spec §7's H4 bars this within +/- 0.002 bits — four standard deviations of this 27-class tuple's own null estimator, amended 2026-09-05 from +/- 0.001, which sat below the estimator's resolution — and a reading outside it is a finding about the INSTRUMENT, not about the world. **The descriptor noun is deliberately not in this tuple**, though spec §5.2's table names it: `windows/locale/src/grammar.rs` exposes no `pub fn`, so the noun's variety draw is unreachable from the lab without rendering a whole document per facet — and its pool is keyed on `(formation, stratum, substrate)`, so it is partly biome-correlated and would have been the weakest member of a control anyway. `Absent` only on a world with no land-eligible facet at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0010804825 | -0.00023045323 | -0.000020259229 | 0.00022787078 | 0.0014645933 | 0.0000093714864 |

### `warp-false-sign-net-thicket`

The Warp's false-sign control for thicket/brake (spec §5.2) — see `warp-false-sign-net-spring`'s doc for the shared control tuple and what a non-zero reading would mean.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0012596021 | -0.00024042536 | -0.000028837589 | 0.00022571084 | 0.0024214187 | 0.0000036311173 |

### `warp-found-fraction-erratic`

The Warp's found fraction for erratic/scatter (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading. **Always `Absent` for the erratic, by construction**: its `macro_state` is a constant, so "the share of occurrences standing on a strong cause" names no quantity. Absent here is the honest value, deliberately not 0.0 — a kind with no cause and a kind whose occurrences all miss their cause are different facts.

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `warp-found-fraction-overhang`

The Warp's found fraction for overhang/hollow (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.61643836 | 0.80952381 | 0.8326943 | 0.85309973 | 0.9200492 | 0.82872418 |

### `warp-found-fraction-spring`

The Warp's found fraction for spring/seep (spec §5.2, H1): the share of this kind's occurrences standing on a facet whose own `macro_state` reads at or above 0.5 — how much of what a walker meets was FOUND at a cause rather than extruded by the recipe's noise floor over the other 90-odd per cent of land. This is the number the campaign's premise measurement is about: before The Warp, 333 of seed 42's 403 springs stood on a facet with no cause at all. `Absent` when the kind never occurs on land.

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.28571429 | 0.71428571 | 0.74586777 | 0.77319588 | 1 | 0.73935096 |

### `warp-found-fraction-thicket`

The Warp's found fraction for thicket/brake (spec §5.2) — see `warp-found-fraction-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.2746431 | 0.34903003 | 0.41151631 | 0.57687421 | 0.32718338 |

### `warp-learner-gain-erratic`

The Warp's learner gain for erratic/scatter (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback. This is the negative control: nothing about the erratic's occurrence depends on any sign, so the table cannot BEAT the base rate out of sample, and a reading that did would mean the instrument was crediting noise. It can and does LOSE — seed 42 reads -0.0138 bits/facet, the price of fitting several hundred classes of pure noise on half the land and being scored on the other half. Spec §7's H3 therefore bars this ONE-SIDED at <= 0.001 bits/facet (amended 2026-09-05 from "within +/- 0.001", which no held-out table over ~469 classes could ever meet): the control is that the erratic never GAINS, not that its loss is small.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.020192735 | -0.013020693 | -0.010973314 | -0.0092947803 | -0.0025397394 | -0.011190631 |

### `warp-learner-gain-overhang`

The Warp's learner gain for overhang/hollow (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.017105655 | 0.084160281 | 0.099211751 | 0.11236043 | 0.15572748 | 0.097688817 |

### `warp-learner-gain-spring`

The Warp's learner gain for spring/seep (spec §5.2, H3): the `P(Y | sign class)` table fitted on the EVEN-indexed land facets and scored on the ODD ones, as mean log-loss reduction against the base rate, in bits per facet. Positive means the words a walker is told genuinely help predict this kind on facets the table never saw; NEGATIVE means the table overfits, which is reported rather than clamped. The index is the facet's position in the land-only vector in vertex order, so the two halves interleave across the whole globe rather than splitting it by region. Smoothing is an EQUIVALENT-SAMPLE-SIZE PRIOR TOWARD THE BASE RATE: a class with `n` fit facets and `k` hits predicts `(k + a * p) / (n + a)`, for `a` ten facets and `p` the fit half's own base rate, so a thin or unseen class predicts the base rate and scores exactly zero, never below it. It was Laplace `(k + 1) / (n + 2)` in this metric's first implementation and that was a defect (controller ruling, ledger #10): Laplace is a prior toward 0.5, these kinds occur on 3-14% of land, and with 469 sign classes over ~11,000 facets most classes are thin enough for that prior to dominate — the table lost to the base rate in sample, which was a fact about the prior and not about the world. Read against `warp-oracle-gain-spring`, the same table's in-sample reading. `Absent` on a world with no land-eligible facet, and on one whose fit half carries no occurrence of this kind at all (or nothing but occurrences): there is no base rate to beat.

n = 997 present, 3 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0040592311 | 0.016633479 | 0.024165187 | 0.031479581 | 0.06448449 | 0.02382876 |

### `warp-learner-gain-thicket`

The Warp's learner gain for thicket/brake (spec §5.2) — see `warp-learner-gain-spring`'s doc for the shared split, the smoothing and the fallback.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0065357323 | 0.01709305 | 0.024085075 | 0.029968863 | 0.05499642 | 0.023438022 |

### `warp-max-class-rate-erratic`

The Warp's wallpaper guard for erratic/scatter (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar. For the erratic this is the base rate plus sampling noise, since no sign class has any relationship to it.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.043859649 | 0.071428571 | 0.078431373 | 0.087301587 | 0.12751678 | 0.07998587 |

### `warp-max-class-rate-overhang`

The Warp's wallpaper guard for overhang/hollow (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.4382716 | 0.46381177 | 0.48993289 | 0.58196721 | 0.44425262 |

### `warp-max-class-rate-spring`

The Warp's wallpaper guard for spring/seep (spec §5.2, H5): the largest `P(Y | sign class)` over sign classes carrying at least 100 land facets — the absolute rate behind `warp-best-lift-spring`'s ratio. H5 asks that no class exceed 0.75 on any seed: above that the sign kind has stopped distinguishing a place and become wallpaper, and the reading is a finding about the recipe's high end, not a success. `Absent` when no sign class clears the support floor.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.048780488 | 0.094844623 | 0.14166667 | 0.56097561 | 0.10582536 |

### `warp-max-class-rate-thicket`

The Warp's wallpaper guard for thicket/brake (spec §5.2) — see `warp-max-class-rate-spring`'s doc for the shared reading and H5's 0.75 bar.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.098360656 | 0.26363636 | 0.30350985 | 0.3364486 | 0.48039216 | 0.2986389 |

### `warp-oracle-gain-erratic`

The Warp's oracle gain for erratic/scatter (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated. The erratic's in-sample reading is the ceiling overfitting alone can reach on this population, which is why it is worth registering even though nothing predicts the erratic.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0071204423 | 0.014082815 | 0.0166481 | 0.018786737 | 0.025932295 | 0.016514618 |

### `warp-oracle-gain-overhang`

The Warp's oracle gain for overhang/hollow (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.025788484 | 0.098034725 | 0.1141663 | 0.12768838 | 0.17517359 | 0.11213664 |

### `warp-oracle-gain-spring`

The Warp's oracle gain for spring/seep (spec §5.2): the SAME `P(Y | sign class)` table as `warp-learner-gain-spring`, fitted and scored on ALL land facets with no split, under the same equivalent-sample-size prior toward the base rate. It is not a legibility reading on its own — an in-sample table always looks better than it is — and it is REPORTED, NEVER GATED. Spec §7's H3 originally asked the learner to reach at least half of this number, and that clause is WITHDRAWN (controller ruling, ledger #10; spec §7's H3 amendment): a ratio needs a denominator whose sign is fixed, and this one has none — under the withdrawn Laplace prior it read NEGATIVE for spring and erratic at seed 42, which would make "at least half of it" satisfiable by being worse. What it is good for is the difference a reader takes, how much of the in-sample reading survives the held-out half. `Absent` under the same conditions as `warp-learner-gain-spring`, over the whole land population rather than a half of it.

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.001372715 | 0.029615279 | 0.037912998 | 0.04646227 | 0.078074958 | 0.037234557 |

### `warp-oracle-gain-thicket`

The Warp's oracle gain for thicket/brake (spec §5.2) — see `warp-oracle-gain-spring`'s doc for the shared reading and for why it is reported and never gated.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.015791802 | 0.04855425 | 0.055752582 | 0.061215993 | 0.085272643 | 0.053786762 |

### `weft-coherence-morans-i-erratic`

H2's coherence readout for erratic/scatter (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion. Erratic's short (5-facet) correlation length predicts the LOWEST of the four readings here, not zero: H2 is about spatial texture existing at all, which a short correlation length still gives, unlike H3's macro-state legibility, which erratic is built to score near zero on.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.47030051 | 0.56195234 | 0.58537623 | 0.6072162 | 0.70282649 | 0.58428246 |

### `weft-coherence-morans-i-overhang`

H2's coherence readout for overhang/hollow (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion.

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0001795274 | 0.90404504 | 0.92107513 | 0.93419268 | 0.9946816 | 0.91565158 |

### `weft-coherence-morans-i-spring`

H2's readout for spring/seep (spec §7): Moran's I over the binary occurrence indicator, sampled along land-eligible walked paths (the same 78-walk, 4,680-step pool `weft-encounter-rate-spring` reads), weighted by within-walk chain adjacency (step `s` and `s+1` of the same walk). **This is a construction-validation (a regression guard against address-hashed speckle), not independent evidence the surface is "coherent" in a stronger sense** — `occurs` thresholds a position-continuous field, so a positive reading is near-guaranteed by construction; the discriminating power lives in `weft_prevalence.rs`'s real-vs-mutant table (real 0.998, mutant 0.209 for this kind). No numeric floor is preregistered for this statistic (spec §7 froze none); a positive reading well clear of zero over a non-degenerate occurs-count is the qualitative claim, checked against `weft-coherence-occurs-count-spring`, the anti-vacuity companion (The Ford's shape) — see that metric's own doc for why a small count makes a high reading here suspect. NOT geosphere vertex adjacency — measured on this tree, that mesh's own spacing is ~106-127 facets per step, 1.9-23x every kind's own correlation length (5-60 facets), so both a sound construction and an address-hashed defect predict `I ~= 0` at that scale — see `weft_morans_i`'s own doc for the full power argument and the discarded vertex-adjacency readings, published in full rather than discarded silently. `Absent` if the walk pool is empty or the indicator has zero variance across every walked step.

n = 795 present, 205 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.0000038182367 | 0.97919824 | 0.98957156 | 0.99627564 | 1 | 0.97294044 |

### `weft-coherence-morans-i-thicket`

H2's coherence readout for thicket/brake (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.89596937 | 0.96030822 | 0.96550841 | 0.96963372 | 0.98427386 | 0.96432633 |

### `weft-coherence-occurs-count-erratic`

H2's anti-vacuity companion for erratic/scatter — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 113 | 216 | 264 | 316 | 471 | 267.858 |

### `weft-coherence-occurs-count-overhang`

H2's anti-vacuity companion for overhang/hollow — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 164 | 235.5 | 324 | 728 | 251.29 |

### `weft-coherence-occurs-count-spring`

H2's anti-vacuity companion for spring/seep (The Ford's shape: `channel-band-monotonicity` paired with `channel-transect-dry-reach`): the raw count of WALKED steps (the same pool `weft-coherence-morans-i-spring` computes its chain adjacency over, and `weft-encounter-rate-spring`'s own numerator) where spring/seep occurred. Moran's I's own denominator is `n*p*(1-p)` for a binary indicator at rate `p`, which shrinks toward zero as occurrence becomes very rare (or very common) — so a small reading here is the signal that a neighbouring high Moran's-I reading may be resting on a handful of adjacent hits rather than a genuine spatial process, exactly as `channel-transect-dry-reach` flags a monotonicity score resting on transects truncated before they could fail.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 14 | 68 | 130 | 490 | 86.846 |

### `weft-coherence-occurs-count-thicket`

H2's anti-vacuity companion for thicket/brake — see `weft-coherence-occurs-count-spring`'s doc for the shared reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 109 | 575 | 738.5 | 929 | 1748 | 747.662 |

### `weft-encounter-rate-any`

H1's observation-scoped density number, UNIONED over all four kinds (spec §7): a walked step counts once even if it carries more than one kind's feature. Read beside `weft-existence-density-any` — the two are H1's promised "two numbers, separately reported", and they need not agree: a short-correlation-length kind can be common god's-eye and rare along any one path, or the reverse for a long one.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.082317073 | 0.16515152 | 0.19059172 | 0.21866097 | 0.30683333 | 0.19152817 |

### `weft-encounter-rate-erratic`

H1's observation-scoped density number for erratic/scatter (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.02372549 | 0.035912698 | 0.040149133 | 0.044238683 | 0.064 | 0.040259296 |

### `weft-encounter-rate-overhang`

H1's observation-scoped density number for overhang/hollow (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.025874126 | 0.036394876 | 0.048363095 | 0.11004274 | 0.037953663 |

### `weft-encounter-rate-spring`

H1's observation-scoped density number for spring/seep (spec §7): features of this kind met per facet of travel, pooled over land-eligible walks sampled the same way `weft_prevalence.rs`'s own `land_eligible_walks` does (60-step walks from 137-spaced starting vertices, kept only if land-eligible throughout). Distinct from `weft-existence-density-spring`'s god's-eye reading — a walker only ever samples the facets on their own path, never the whole grid, so this is the number that actually answers "how often does a traveller meet one".

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.0021929825 | 0.010869401 | 0.019023569 | 0.074413146 | 0.012512106 |

### `weft-encounter-rate-thicket`

H1's observation-scoped density number for thicket/brake (spec §7) — see `weft-encounter-rate-spring`'s doc for the shared walk-based reading.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.018390805 | 0.088057743 | 0.11363636 | 0.13896882 | 0.21924883 | 0.11362243 |

### `weft-existence-density-any`

H1's god's-eye density number, UNIONED over all four kinds (spec §7): the fraction of land-eligible facets carrying ANY derived feature. A facet holding two kinds counts once, the same union discipline `channel-band-monotonicity`'s sibling readouts and `site-density`-style metrics use elsewhere. This is the single number H1's `>= 3 orders of magnitude over the placed baseline` claim is actually checked against.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.1144989 | 0.17603611 | 0.19763011 | 0.21704902 | 0.26750893 | 0.19587159 |

### `weft-existence-density-erratic`

H1's god's-eye density number for erratic/scatter (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-erratic`. Erratic is the negative control for H3, not for H1: nothing about density predicts erratic should read low here.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034878203 | 0.039250715 | 0.040511933 | 0.041774822 | 0.046481217 | 0.040565577 |

### `weft-existence-density-overhang`

H1's god's-eye density number for overhang/hollow (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-overhang`.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0050480603 | 0.028681921 | 0.037043064 | 0.044355556 | 0.070868486 | 0.036808741 |

### `weft-existence-density-spring`

H1's god's-eye density number for spring/seep (spec §7): the fraction of LAND-ELIGIBLE facets (spec §7's amendment population) carrying a spring/seep occurrence, over a vertex-centred subsample — one representative facet per geosphere vertex (`n = 40,962` at `hornvale_terrain::GLOBE_LEVEL = 6`), a 1-in-9,830 sample of the ~4e8 walk-depth facets on the grid, not every one of them; the same resolution spec §7's own gate-component diagnostic reads at. Distinct from `weft-encounter-rate-spring`, which reads the SAME kind's occurrence along a walked path — H1 is two numbers because discovery is observation-scoped and existence density alone (what The Prospect's H3 measured) answers a different question from what a walker actually meets. No threshold is preregistered for this reading alone; H1's claim is the >= 3-orders-of-magnitude density gain over the placed baseline `site-density`-style metrics measure, read across every kind together.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.0088267745 | 0.012906628 | 0.016575198 | 0.029032082 | 0.012650507 |

### `weft-existence-density-thicket`

H1's god's-eye density number for thicket/brake (spec §7) — see `weft-existence-density-spring`'s doc for the shared reading and its pairing with `weft-encounter-rate-thicket`.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.04236052 | 0.097568316 | 0.12183695 | 0.14176363 | 0.19179713 | 0.11894228 |

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
| 0.02212451 | 0.098098863 | 0.1154064 | 0.13079065 | 0.18275241 | 0.1138177 |

### `weft-legibility-mi-spring`

H3's legibility readout for spring/seep (spec §7, allowed to fail): discrete mutual information in bits between spring/seep's own blended macro-state signal (binned into 4 equal-width bins over its documented `[0,1]` range) and whether it occurred, over the land-eligible population (spec §7's amendment: restricting to land-eligible facets makes the shared eligibility gate a constant, so it contributes zero MI by construction and cannot flatter any kind — see the spec's own amendment paragraph for the whole-sphere confound this removes). The preregistered claim WAS the ORDERING `spring > thicket > overhang > erratic`, not a threshold on this reading alone: spring is H3's own sign case ("diagnostic of what is underfoot"), so it was predicted HIGHEST of the four. The Weft measured this ordering FALSE (`thicket > spring`, 2026-09-04); The Warp's re-parameterisation of the sign kinds then moved spring's reading to 0.086464 and overhang's to 0.076536 at seed 42, while thicket (0.038604) and erratic (0.000000) are unchanged, falsifying the ordering again — now on `thicket > overhang`. This metric is a RECORD of that original prediction, not a live claim.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.060141803 | 0.082466233 | 0.099666081 | 0.15477143 | 0.079111673 |

### `weft-legibility-mi-thicket`

H3's legibility readout for thicket/brake (spec §7, allowed to fail) — see `weft-legibility-mi-spring`'s doc for the shared estimator and its falsification record. Thicket was predicted SECOND of the four; it now reads 0.038604 at seed 42, BELOW overhang's 0.076536.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0056834778 | 0.033202999 | 0.03986345 | 0.045803703 | 0.07429427 | 0.038831832 |

## Weaknesses found here

### `dominant-commodity`

- **D1**: "salt" holds 999/1000 worlds (99.9%), at or above the 80% threshold

### `weft-coherence-morans-i-overhang`

- **D3**: p25..p75 spans 0.03014764000000003 (3.03% of the 0.9948611274000001 min..max range), under the 5% bar

### `weft-coherence-morans-i-spring`

- **D3**: p25..p75 spans 0.01707740000000002 (1.71% of the 1.0000038182367 min..max range), under the 5% bar

### `weft-legibility-mi-erratic`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

