<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Demography — The Domesday

How many, and of what structure: the population counts a world's peoples carry.

## Metrics

### `climate-displacement-events`

How many occupations on this world ended in climate-driven migration (`occ-cause` = `migrated`), excluding conquest-relocations — the displacement mechanic's volume (The Assize). Replaces the seed-42 gate in `cli/tests/history_battery.rs` and the twelve-seed panel in `windows/worldgen/tests/history_sundering.rs`. Bimodal: most worlds sit in single digits and a minority run to the hundreds, and a real minority measure zero — a mild deep past, not an inert bake. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 13 | 163 | 2416 | 176.124 |

### `goblin-flagship-population`

The goblin flagship settlement's committed population; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 34 | 40 | 41 | 43 | 34.866 |

### `kobold-flagship-population`

The kobold flagship settlement's committed population; Absent if kobolds placed no settlement

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 28 | 45 | 56 | 70 | 41.225051 |

### `mean-population`

Mean population across every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 7.1967213 | 24.288288 | 29.562857 | 32.065041 | 40.450758 | 27.374198 |

### `peoples-alive-at-bake-end`

M3: how many distinct peoples still hold a live community when the bake ends — the decision-0089 compliance reading

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 8 | 15 | 15 | 15 | 15 | 14.92 |

### `peoples-placed`

How many peoples hold a flagship settlement in this world — the n in the 1/n chance baseline The Namesake §5.1(2) is judged against, published so that verdict is re-derivable from rows.csv without inferring n; Absent if no people is placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 8 | 15 | 15 | 15 | 15 | 14.92 |

### `total-population`

Sum of every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 512 | 5080 | 7357 | 9732 | 18558 | 7251.352 |

## Weaknesses found here

### `climate-displacement-events`

- **D5 strength**: declared moderate tracking habitable-fraction, but observed r = -0.196 (1000 pairs) is weak (negative)

### `peoples-alive-at-bake-end`

- **D3**: p25..p75 spans 0 (0.00% of the 7 min..max range), under the 5% bar
- **D4**: median 15 equals the max (8 .. 15)

### `peoples-placed`

- **D3**: p25..p75 spans 0 (0.00% of the 7 min..max range), under the 5% bar
- **D4**: median 15 equals the max (8 .. 15)

### `total-population`

- **D5 strength**: declared strong tracking fertile-land-fraction, but observed r = +0.244 (1000 pairs) is weak (positive)
- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.267 (1000 pairs) is weak (positive)

