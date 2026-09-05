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

### `weft-coherence-morans-i-erratic`

H2's coherence readout for erratic/scatter (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion. Erratic's short (5-facet) correlation length predicts the LOWEST of the four readings here, not zero: H2 is about spatial texture existing at all, which a short correlation length still gives, unlike H3's macro-state legibility, which erratic is built to score near zero on.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.47030051 | 0.56195234 | 0.58537623 | 0.6072162 | 0.70282649 | 0.58428246 |

### `weft-coherence-morans-i-overhang`

H2's coherence readout for overhang/hollow (spec §7) — see `weft-coherence-morans-i-spring`'s doc for the shared statistic and its companion.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.77354298 | 0.83170614 | 0.84449584 | 0.85681498 | 0.90200871 | 0.84392416 |

### `weft-coherence-morans-i-spring`

H2's readout for spring/seep (spec §7): Moran's I over the binary occurrence indicator, sampled along land-eligible walked paths (the same 78-walk, 4,680-step pool `weft-encounter-rate-spring` reads), weighted by within-walk chain adjacency (step `s` and `s+1` of the same walk). **This is a construction-validation (a regression guard against address-hashed speckle), not independent evidence the surface is "coherent" in a stronger sense** — `occurs` thresholds a position-continuous field, so a positive reading is near-guaranteed by construction; the discriminating power lives in `weft_prevalence.rs`'s real-vs-mutant table (real 0.998, mutant 0.209 for this kind). No numeric floor is preregistered for this statistic (spec §7 froze none); a positive reading well clear of zero over a non-degenerate occurs-count is the qualitative claim, checked against `weft-coherence-occurs-count-spring`, the anti-vacuity companion (The Ford's shape) — see that metric's own doc for why a small count makes a high reading here suspect. NOT geosphere vertex adjacency — measured on this tree, that mesh's own spacing is ~106-127 facets per step, 1.9-23x every kind's own correlation length (5-60 facets), so both a sound construction and an address-hashed defect predict `I ~= 0` at that scale — see `weft_morans_i`'s own doc for the full power argument and the discarded vertex-adjacency readings, published in full rather than discarded silently. `Absent` if the walk pool is empty or the indicator has zero variance across every walked step.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.79324879 | 0.92252854 | 0.93371809 | 0.94389221 | 0.97408369 | 0.93197547 |

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
| 160 | 368 | 463 | 556 | 860 | 467.112 |

### `weft-coherence-occurs-count-spring`

H2's anti-vacuity companion for spring/seep (The Ford's shape: `channel-band-monotonicity` paired with `channel-transect-dry-reach`): the raw count of WALKED steps (the same pool `weft-coherence-morans-i-spring` computes its chain adjacency over, and `weft-encounter-rate-spring`'s own numerator) where spring/seep occurred. Moran's I's own denominator is `n*p*(1-p)` for a binary indicator at rate `p`, which shrinks toward zero as occurrence becomes very rare (or very common) — so a small reading here is the signal that a neighbouring high Moran's-I reading may be resting on a handful of adjacent hits rather than a genuine spatial process, exactly as `channel-transect-dry-reach` flags a monotonicity score resting on transects truncated before they could fail.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 32 | 179 | 240 | 311 | 597 | 248.946 |

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
| 0.125 | 0.21313559 | 0.2373736 | 0.26169591 | 0.34039216 | 0.23783579 |

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
| 0.031417625 | 0.061578947 | 0.069826295 | 0.077820513 | 0.10710383 | 0.070110843 |

### `weft-encounter-rate-spring`

H1's observation-scoped density number for spring/seep (spec §7): features of this kind met per facet of travel, pooled over land-eligible walks sampled the same way `weft_prevalence.rs`'s own `land_eligible_walks` does (60-step walks from 137-spaced starting vertices, kept only if land-eligible throughout). Distinct from `weft-existence-density-spring`'s god's-eye reading — a walker only ever samples the facets on their own path, never the whole grid, so this is the number that actually answers "how often does a traveller meet one".

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0070175439 | 0.029342723 | 0.036165463 | 0.044259259 | 0.078133333 | 0.037276141 |

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
| 0.17866149 | 0.2267342 | 0.24619952 | 0.2630679 | 0.3099881 | 0.24388642 |

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
| 0.061052452 | 0.069544006 | 0.071499967 | 0.073730369 | 0.083132175 | 0.071589652 |

### `weft-existence-density-spring`

H1's god's-eye density number for spring/seep (spec §7): the fraction of LAND-ELIGIBLE facets (spec §7's amendment population) carrying a spring/seep occurrence, over a vertex-centred subsample — one representative facet per geosphere vertex (`n = 40,962` at `hornvale_terrain::GLOBE_LEVEL = 6`), a 1-in-9,830 sample of the ~4e8 walk-depth facets on the grid, not every one of them; the same resolution spec §7's own gate-component diagnostic reads at. Distinct from `weft-encounter-rate-spring`, which reads the SAME kind's occurrence along a walked path — H1 is two numbers because discovery is observation-scoped and existence density alone (what The Prospect's H3 measured) answers a different question from what a walker actually meets. No threshold is preregistered for this reading alone; H1's claim is the >= 3-orders-of-magnitude density gain over the placed baseline `site-density`-style metrics measure, read across every kind together.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.026663259 | 0.034919894 | 0.037365193 | 0.039519203 | 0.047824791 | 0.037231116 |

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
| 0.00046350264 | 0.0017119447 | 0.002276622 | 0.0028628035 | 0.0066847431 | 0.0023329713 |

### `weft-legibility-mi-spring`

H3's legibility readout for spring/seep (spec §7, allowed to fail): discrete mutual information in bits between spring/seep's own blended macro-state signal (binned into 4 equal-width bins over its documented `[0,1]` range) and whether it occurred, over the land-eligible population (spec §7's amendment: restricting to land-eligible facets makes the shared eligibility gate a constant, so it contributes zero MI by construction and cannot flatter any kind — see the spec's own amendment paragraph for the whole-sphere confound this removes). The preregistered claim WAS the ORDERING `spring > thicket > overhang > erratic`, not a threshold on this reading alone: spring is H3's own sign case ("diagnostic of what is underfoot"), so it was predicted HIGHEST of the four. The Weft measured this ordering FALSE (`thicket > spring`, 2026-09-04); The Warp's re-parameterisation of the sign kinds then moved spring's reading to 0.086464 and overhang's to 0.076536 at seed 42, while thicket (0.038604) and erratic (0.000000) are unchanged, falsifying the ordering again — now on `thicket > overhang`. This metric is a RECORD of that original prediction, not a live claim.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.000014678381 | 0.0041412097 | 0.0060185641 | 0.0077333382 | 0.014382271 | 0.0059722055 |

### `weft-legibility-mi-thicket`

H3's legibility readout for thicket/brake (spec §7, allowed to fail) — see `weft-legibility-mi-spring`'s doc for the shared estimator and its falsification record. Thicket was predicted SECOND of the four; it now reads 0.038604 at seed 42, BELOW overhang's 0.076536.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0056834778 | 0.033202999 | 0.03986345 | 0.045803703 | 0.07429427 | 0.038831832 |

## Weaknesses found here

### `dominant-commodity`

- **D1**: "salt" holds 999/1000 worlds (99.9%), at or above the 80% threshold

### `weft-legibility-mi-erratic`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

