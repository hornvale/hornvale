<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Climate — The Domesday

The temperature and moisture a world's astronomy and terrain resolve into, world over world.

## Metrics

### `band-count`

Circulation bands per hemisphere; 'locked' if tidally locked

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `3` | 791 | 79.1% |
| `5` | 161 | 16.1% |
| `locked` | 48 | 4.8% |

### `dominant-land-biome`

The most common land biome by cell count, kebab-case

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `taiga` | 250 | 25.0% |
| `ice` | 187 | 18.7% |
| `alpine` | 179 | 17.9% |
| `temperate-forest` | 166 | 16.6% |
| `tundra` | 136 | 13.6% |
| `tropical-seasonal-forest` | 59 | 5.9% |
| `temperate-grassland` | 21 | 2.1% |
| `savanna` | 2 | 0.2% |

### `dominant-soil-order`

The most common land soil order by cell count, spec §4's soil taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `leptosol` | 1000 | 100.0% |

### `fertile-land-fraction`

Fraction of land cells whose soil fertility's grain-suitability exceeds 0.6 (The Ground, spec §3/§4)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.039277554 | 0.069334104 | 0.10450931 | 0.21820782 | 0.073636034 |

### `habitable-fraction`

Fraction of cells that are habitable (land, water, tolerable season)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.02866071 | 0.1325863 | 0.17599238 | 0.21786046 | 0.30721156 | 0.17297654 |

### `mean-land-temperature-c`

Annual-mean temperature averaged over land cells, °C; Absent if the world has no land

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -35.378763 | -10.781073 | -3.6490213 | 4.5025003 | 21.844769 | -3.3537411 |

## Weaknesses found here

### `dominant-soil-order`

- **D1**: "leptosol" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `fertile-land-fraction`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.727 (1000 pairs) is dominant (positive)

### `habitable-fraction`

- **D5 strength**: declared moderate tracking obliquity-degrees, but observed |r| = 0.024 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = +0.108 (1000 pairs) is weak — and the sign is backwards: the coupling is positive, not the declared negative

### `mean-land-temperature-c`

- **D5 strength**: declared dominant tracking year-std-days, but observed |r| = 0.081 (1000 pairs) is none
- **D5 strength**: declared weak tracking brightening-per-gyr, but observed |r| = 0.024 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed r = +0.100 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = -0.133 (1000 pairs) is weak (negative)
- **D6**: median -3.6490213000000002 vs Earth's 14 differs by 17.649021, exceeding the band 10

