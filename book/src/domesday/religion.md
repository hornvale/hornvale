<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Religion — The Domesday

What peoples believe: pantheons, cults, and the vestiges belief leaves in the world.

## Metrics

### `belief-kind-bugbear`

Sentiment of the bugbear flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 996 present, 4 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 909 | 91.3% |
| `ambient` | 77 | 7.7% |
| `eternal` | 10 | 1.0% |

### `belief-kind-goblin`

Sentiment of the goblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 952 | 95.2% |
| `eternal` | 48 | 4.8% |

### `belief-kind-hobgoblin`

Sentiment of the hobgoblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 951 | 95.2% |
| `eternal` | 48 | 4.8% |

### `belief-kind-kobold`

Sentiment of the kobold flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 925 | 94.2% |
| `eternal` | 57 | 5.8% |

### `blind-attribution-correct`

Whether the fixed structural rule (lunar head, then cyclic share, then size — no lexical input) attributes the kobold pantheon correctly; Absent unless both peoples hold pantheons

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 890 | 90.6% |
| `false` | 92 | 9.4% |

### `crisis-fires`

Whether any placed people holds a live prediction crisis at day 36,525 (the hundredth year, the diachronic battery's preregistered epoch) — The Assay. Replaces `windows/worldgen/tests/diachronic.rs`'s up-to-200-world hunt for a single instance. A crisis needs a Generated sky, an organized flagship's doctrine, >= 8 witnessed events of one recurrence class and a tail miss-run, so it cannot be synthesised — which is why it is a rate here rather than a hand-built behaviour test.

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 659 | 65.9% |
| `false` | 341 | 34.1% |

### `cult-form`

The goblin flagship's pantheon's shared cult form ('organized' or 'folk'); Absent if no goblin beliefs

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 943 | 94.3% |
| `folk` | 57 | 5.7% |

### `cult-form-goblin`

Cult form of the goblin flagship's pantheon (organized/folk); Absent without one

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 943 | 94.3% |
| `folk` | 57 | 5.7% |

### `cult-form-goblin-twin`

Cult form of the goblin-twin flagship's pantheon (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds)

| value | count | share |
|---|---|---|

### `cult-form-kobold`

Cult form of the kobold flagship's pantheon (organized/folk); Absent without one

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 697 | 71.0% |
| `folk` | 285 | 29.0% |

### `head-deity-domain-goblin`

Venue domain of the goblin flagship's head deity: solar, lunar, or ambient; Absent without a goblin pantheon

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `solar` | 1000 | 100.0% |

### `head-deity-domain-goblin-twin`

Venue domain of the goblin-twin flagship's head deity (null control, spec §4); Absent without a goblin-twin pantheon

n = 0 present, 1000 absent (of 1000 worlds)

| value | count | share |
|---|---|---|

### `head-deity-domain-kobold`

Venue domain of the kobold flagship's head deity: solar, lunar, or ambient; Absent without a kobold pantheon

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `lunar` | 879 | 89.5% |
| `solar` | 103 | 10.5% |

### `head-deity-periodicity`

The sentiment tag of the goblin flagship's head deity (the most salient belief): 'eternal', 'cyclic', or 'ambient'; Absent if no goblin beliefs

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 952 | 95.2% |
| `eternal` | 48 | 4.8% |

### `pantheon-cyclic-share-goblin`

Fraction of the goblin flagship pantheon's source phenomena that are periodic (the pick_kobold input the null control needs); Absent without a goblin pantheon

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.9 | 1 | 1 | 1 | 0.93132479 |

### `pantheon-cyclic-share-goblin-twin`

Fraction of the goblin-twin flagship pantheon's source phenomena that are periodic (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `pantheon-size`

Number of beliefs in the goblin flagship's pantheon; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 8 | 11 | 19 | 8.063 |

### `pantheon-size-goblin`

Number of deities in the goblin flagship's pantheon; Absent without one

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 8 | 11 | 19 | 8.063 |

### `pantheon-size-goblin-twin`

Number of deities in the goblin-twin flagship's pantheon (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `pantheon-size-kobold`

Number of deities in the kobold flagship's pantheon; Absent without one

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 7 | 10 | 17 | 7.6496945 |

### `pantheon-verticality`

Whether the goblin flagship's pantheon is ranked (a high god presides) or flat; Absent if there is no goblin flagship pantheon

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `flat` | 1000 | 100.0% |

## Weaknesses found here

### `belief-kind-bugbear`

- **D1**: "cyclic" holds 909/996 worlds (91.3%), at or above the 80% threshold

### `belief-kind-goblin`

- **D1**: "cyclic" holds 952/1000 worlds (95.2%), at or above the 80% threshold

### `belief-kind-hobgoblin`

- **D1**: "cyclic" holds 951/999 worlds (95.2%), at or above the 80% threshold

### `belief-kind-kobold`

- **D1**: "cyclic" holds 925/982 worlds (94.2%), at or above the 80% threshold

### `blind-attribution-correct`

- **D1**: "true" holds 890/982 worlds (90.6%), at or above the 80% threshold

### `cult-form`

- **D1**: "organized" holds 943/1000 worlds (94.3%), at or above the 80% threshold

### `cult-form-goblin`

- **D1**: "organized" holds 943/1000 worlds (94.3%), at or above the 80% threshold

### `head-deity-domain-goblin`

- **D1**: "solar" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `head-deity-domain-kobold`

- **D1**: "lunar" holds 879/982 worlds (89.5%), at or above the 80% threshold

### `head-deity-periodicity`

- **D1**: "cyclic" holds 952/1000 worlds (95.2%), at or above the 80% threshold

### `pantheon-cyclic-share-goblin`

- **D4**: median 1 equals the max (0 .. 1)

### `pantheon-verticality`

- **D1**: "flat" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

