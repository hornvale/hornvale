<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# History — The Domesday

How a world's occupation record accumulates: strata, tenure, and how much of it survives to be read back.

## Metrics

### `dominant-hazard`

The most common hazard kind among land-cell vestiges by layer count (The Vestige, spec §9.2); Absent where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `structural` | 1000 | 100.0% |

### `forgotten-fraction`

Over land cells with a non-empty vestige stack, the fraction whose most-dread layer is Forgotten rather than Venerated (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.25316456 | 0.54508197 | 0.61386297 | 0.6972973 | 0.97877984 | 0.63034211 |

### `mean-warning-legibility`

Mean warning_legibility over every land-cell vestige layer (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.029486075 | 0.42898163 | 0.49805199 | 0.55985113 | 0.73718352 | 0.4765754 |

### `vestige-density`

Fraction of land cells with a non-empty vestige stack (The Vestige, spec §9.2)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0016717475 | 0.011608537 | 0.015891635 | 0.020615563 | 0.047098854 | 0.016481957 |

## Weaknesses found here

### `dominant-hazard`

- **D1**: "structural" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

