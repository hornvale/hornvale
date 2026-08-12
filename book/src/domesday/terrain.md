<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Terrain — The Domesday

The solid shape of a world: its plates, its elevation, and the landforms the sculpting pipeline leaves behind.

## Metrics

### `cave-fraction`

Fraction of land cells with a cave

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.047391096 | 0.093538767 | 0.1130929 | 0.13024311 | 0.21553567 | 0.11316713 |

### `continent-count`

Connected land components at least 0.5% of the world's total land cells (Task 9 iteration 3's size floor, Earth-calibrated: Greenland is ~1.4% of Earth's land and qualifies, Iceland ~0.07% does not) — the unfloored fringe of sub-floor fragments is preserved separately by landmass-count

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 7 | 8 | 9 | 14 | 8.03 |

### `deposit-density`

Fraction of land cells with an ore deposit (The Lode, spec §5)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.22357035 | 0.33923933 | 0.378212 | 0.40861836 | 0.50716127 | 0.37471118 |

### `dominant-commodity`

The most common land ore commodity by cell count (The Lode, spec §5); Absent where no land cell has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `salt` | 986 | 98.6% |
| `gold` | 14 | 1.4% |

### `dominant-rock`

The most common land rock class by cell count, spec §4's fine taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `shale` | 380 | 38.0% |
| `evaporite` | 363 | 36.3% |
| `gneiss` | 136 | 13.6% |
| `andesite` | 80 | 8.0% |
| `sandstone` | 32 | 3.2% |
| `conglomerate` | 5 | 0.5% |
| `granite` | 4 | 0.4% |

### `hypsometric-bimodality`

Ashman's D between land and ocean elevation populations (Earth is strongly bimodal); Absent when a world lacks land or ocean

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.8362165 | 2.6986145 | 3.0195368 | 3.3327841 | 4.3055866 | 3.0212051 |

### `landmass-count`

Every connected land component regardless of size — the unfloored companion continent-count superseded away from; reported alongside forever

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 15 | 43 | 53 | 68 | 262 | 61.447 |

### `largest-continent-share`

Largest land component's share of all land cells; Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.14091769 | 0.23220416 | 0.29280898 | 0.40229473 | 0.98545654 | 0.33814228 |

### `mean-depth-to-basement`

Mean depth to crystalline basement over land (m) — the sedimentary archive's thickness.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 13.656246 | 25.754862 | 29.547548 | 34.121538 | 75.387361 | 30.406971 |

### `mean-geothermal-gradient`

Mean geothermal gradient over land (K/km) — the deep's energy base.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 23.17138 | 24.972604 | 25.407315 | 25.774525 | 27.286705 | 25.385591 |

### `mean-land-elevation-m`

Mean elevation above sea level over land cells, m — the term the lapse rate turns into a temperature penalty. Land is `e >= sea`, matching `mountain-coverage`'s land definition; `mean-land-temperature-c` uses `!is_ocean(cell)`, which is the same condition (`is_ocean` is `e < sea`), so the two metrics ARE mutually comparable — this is the coupling the campaign's lapse-rate regression rests on. Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1474.2533 | 2099.2926 | 2266.8735 | 2395.9951 | 2745.5761 | 2234.8468 |

### `mean-ore-grade`

Mean ore grade [0,1] over land cells with a deposit (The Lode, spec §5); 0.0 where no land cell has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034691958 | 0.053717289 | 0.060634165 | 0.06732103 | 0.093181046 | 0.060694474 |

### `mountain-coverage`

Fraction of land cells standing above 2000 m over the sea

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.30983127 | 0.50731456 | 0.5445354 | 0.57630851 | 0.65659949 | 0.53623862 |

### `ocean-fraction`

Fraction of globe cells below sea level

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.48864802 | 0.55978712 | 0.62202773 | 0.68321859 | 0.75560275 | 0.62072911 |

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

Gini coefficient over plate cell counts (Earth's plate sizes are heavy-tailed; uniform Voronoi scores low)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.25701382 | 0.65178653 | 0.70607485 | 0.74873663 | 0.85340293 | 0.69035659 |

### `sediment-volume`

Total deposited sediment volume proxy: Σ sediment thickness (meters) over every cell, one cell-area unit per cell — the carve's own volume-proxy convention (spec §5): repose's receiver-side gains, routing's floodplain/ playa deposit, the marine wedge/delta fill, and atoll cap material, all summed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 724076.04 | 1213578.9 | 1399930.5 | 1564906.8 | 2018389 | 1389230.6 |

### `unconformity-fraction`

Fraction of land cells recording a nonconformity (missing time) — the archive's floating gaps.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.016584642 | 0.24965762 | 0.35103133 | 0.43786625 | 0.8346103 | 0.35129525 |

### `unrest-coverage`

Fraction of cells with tectonic unrest above 0.3

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.00043943167 | 0.013964162 | 0.020360334 | 0.028245691 | 0.062887554 | 0.021551267 |

## Weaknesses found here

### `dominant-commodity`

- **D1**: "salt" holds 986/1000 worlds (98.6%), at or above the 80% threshold

