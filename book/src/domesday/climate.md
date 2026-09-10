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

The most common land biome by vertex count, kebab-case

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `taiga` | 248 | 24.8% |
| `ice` | 190 | 19.0% |
| `alpine` | 179 | 17.9% |
| `temperate-forest` | 165 | 16.5% |
| `tundra` | 136 | 13.6% |
| `tropical-seasonal-forest` | 61 | 6.1% |
| `temperate-grassland` | 19 | 1.9% |
| `savanna` | 2 | 0.2% |

### `dominant-soil-order`

The most common land soil order by vertex count, spec §4's soil taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `leptosol` | 1000 | 100.0% |

### `fertile-land-fraction`

Fraction of land vertices whose soil fertility's grain-suitability exceeds 0.6 (The Ground, spec §3/§4)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.039223377 | 0.069196159 | 0.10508716 | 0.21820782 | 0.073576159 |

### `habitable-fraction`

Fraction of vertices that are habitable (land, water, tolerable season)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.02866071 | 0.13302573 | 0.17576046 | 0.21786046 | 0.30264636 | 0.17287095 |

### `mean-land-temperature-c`

Annual-mean temperature averaged over land vertices, °C; Absent if the world has no land

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -35.378763 | -10.73628 | -3.5418074 | 4.6836426 | 21.844769 | -3.3237308 |

## Weaknesses found here

### `dominant-soil-order`

- **D1**: "leptosol" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `fertile-land-fraction`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.726 (1000 pairs) is dominant (positive)

### `habitable-fraction`

- **D5 strength**: declared moderate tracking obliquity-degrees, but observed |r| = 0.023 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = +0.115 (1000 pairs) is weak — and the sign is backwards: the coupling is positive, not the declared negative

### `mean-land-temperature-c`

- **D5 strength**: declared dominant tracking year-std-days, but observed |r| = 0.087 (1000 pairs) is none
- **D5 strength**: declared weak tracking brightening-per-gyr, but observed |r| = 0.023 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.097 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = -0.125 (1000 pairs) is weak (negative)
- **D6**: median -3.5418073999999997 vs Earth's 14 differs by 17.541807, exceeding the band 10

