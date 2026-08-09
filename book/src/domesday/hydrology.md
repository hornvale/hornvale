<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Hydrology — The Domesday

Where water moves and gathers: rivers, lakes, aquifers, and the coasts between land and sea.

## Metrics

### `aquifer-fraction`

Fraction of land cells whose hydrogeology classifies as an aquifer (The Ground, spec §3)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0052304675 | 0.10282625 | 0.14047298 | 0.1829423 | 0.42360194 | 0.14590002 |

### `coast-roughness-slope`

Multi-scale coastline-roughness slope, unbanded: the least-squares slope of ln(shoreline development) against mesh level, measured at L4/L5/L6 by projecting each level's cells onto the canonical L6 land/ocean truth (NearestCellIndex). A companion to shoreline-development, not a replacement — that estimator is unchanged. Positive means roughness concentrates at fine scales, which makes this slope immune to the single-hex land/ocean alternation exploit that inflates shoreline-development without changing the coast's coarse shape; Absent if any of the three levels has no shoreline

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.082636144 | 0.1223304 | 0.13862117 | 0.15750302 | 0.30529804 | 0.1430582 |

### `delta-count`

Count of cells a river-mouth delta lobe raised above sea level (spec §5's top-K discrete deltas) — a cell count, not a mouth count: each of the top-K mouths can raise the mouth cell itself plus up to two adjacent hop-1 ocean cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 4 | 5 | 6 | 9 | 4.896 |

### `endorheic-coverage`

Fraction of land cells that are endorheic (interior-draining)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.083239278 | 0.21006861 | 0.2425422 | 0.27510499 | 0.39043254 | 0.24233448 |

### `hydro-variant-coverage`

Which `Hydro` variants `hydro_at` reads anywhere on this world, as `+`-joined names in `Hydro::ALL` order (The Assay). Replaces `domains/terrain/tests/hydro_witness.rs`'s 8-seed reachability sweep: a variant no world in the census shows is structurally dead, and 1,000 worlds say so with a rate where 8 said so with a flag.

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `aquifer+aquitard+spring+runoff+karst` | 1000 | 100.0% |

### `karst-fraction`

Fraction of land cells whose hydrogeology classifies as karst (The Ground, spec §3)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.063162418 | 0.10394537 | 0.11800779 | 0.13380055 | 0.20915586 | 0.12016418 |

### `rerouted-flow-fraction`

The A→B→C escalation diagnostic (spec §8, preregistered, a permanent census column): the flux-weighted fraction of the world's 20 largest pre-carve rivers' mainstem cells whose downhill target changed across the carve. Thresholds: < 0.10 engine A self-consistent; 0.10-0.30 flag, Nathan decides; > 0.30 A rejected as sole engine, engine B enters evaluation

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.03299079 | 0.069824155 | 0.08252401 | 0.095726441 | 0.18643695 | 0.083565475 |

### `shelf-fraction`

Fraction of cells within the shelf band (±200 m) of sea level — the populated shelf Earth's hypsometry keeps

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.050388165 | 0.073360676 | 0.089656266 | 0.11332454 | 0.29688492 | 0.09827899 |

### `shelf-width-active-median`

Median shelf width over ACTIVE-margin coast land cells: hops seaward from the coast cell, each hop to the deepest ocean neighbor, until depth first exceeds twice the sediment wedge's freeboard cap or 8 hops are spent — spec §8's passive/active shelf asymmetry battery (active median should be narrower than passive); Absent if the world has no active-margin coast

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 2 | 2 | 8 | 1.6986987 |

### `shelf-width-passive-median`

Median shelf width over PASSIVE-margin coast land cells (Passive/Interior/Oceanic, mirroring the carve's own wedge-reach margin split): hops seaward from the coast cell, each hop to the deepest ocean neighbor, until depth first exceeds twice the sediment wedge's freeboard cap or 8 hops are spent — spec §8's passive/active shelf asymmetry battery (passive median should exceed active); Absent if the world has no passive-margin coast

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 2 | 2 | 2 | 4 | 1.854 |

### `shoreline-development`

Shoreline development index: coastline length over the circumference of the circle with the land's area (1 = maximally compact); Absent without a shoreline

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5.142873 | 6.5366794 | 6.9293611 | 7.3889152 | 10.044913 | 7.0229882 |

### `waterfall-count`

Count of waterfall (knickpoint) sites the carve found: land cells where a high-drainage watercourse crosses a sharp PRE-carve induration step (spec §5's derived point observations)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 1 | 4 | 0.516 |

## Weaknesses found here

### `endorheic-coverage`

- **D5 strength**: declared weak tracking mean-land-temperature-c, but observed |r| = 0.017 (1000 pairs) is none

### `hydro-variant-coverage`

- **D1**: "aquifer+aquitard+spring+runoff+karst" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `shelf-fraction`

- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.755 (1000 pairs) is dominant

### `shelf-width-passive-median`

- **D3**: p25..p75 spans 0 (0.00% of the 3 min..max range), under the 5% bar

### `shoreline-development`

- **D5 strength**: declared weak tracking total-tide, but observed |r| = 0.019 (1000 pairs) is none
- **D5 strength**: declared moderate tracking continent-count, but observed |r| = 0.004 (1000 pairs) is none
- **D5 strength**: declared moderate tracking largest-continent-share, but observed |r| = 0.130 (1000 pairs) is weak

### `waterfall-count`

- **D4**: median 0 equals the min (0 .. 4)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.083 (1000 pairs) is none

