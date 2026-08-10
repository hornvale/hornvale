<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Religion — The Domesday

What peoples believe: pantheons, cults, and the vestiges belief leaves in the world.

## Metrics

### `belief-kind-bugbear`

Sentiment of the bugbear flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 993 present, 7 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 902 | 90.8% |
| `ambient` | 83 | 8.4% |
| `eternal` | 8 | 0.8% |

### `belief-kind-goblin`

Sentiment of the goblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 950 | 95.2% |
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

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 913 | 94.2% |
| `eternal` | 56 | 5.8% |

### `blind-attribution-correct`

Whether the fixed structural rule (lunar head, then cyclic share, then size — no lexical input) attributes the kobold pantheon correctly; Absent unless both peoples hold pantheons

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 879 | 90.7% |
| `false` | 90 | 9.3% |

### `crisis-fires`

Whether any placed people holds a live prediction crisis at day 36,525 (the hundredth year, the diachronic battery's preregistered epoch) — The Assay. Replaces `windows/worldgen/tests/diachronic.rs`'s up-to-200-world hunt for a single instance. A crisis needs a Generated sky, an organized flagship's doctrine, >= 8 witnessed events of one recurrence class and a tail miss-run, so it cannot be synthesised — which is why it is a rate here rather than a hand-built behaviour test.

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 659 | 65.9% |
| `false` | 341 | 34.1% |

### `cult-form`

The goblin flagship's pantheon's shared cult form ('organized' or 'folk'); Absent if no goblin beliefs

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 937 | 93.9% |
| `folk` | 61 | 6.1% |

### `cult-form-goblin`

Cult form of the goblin flagship's pantheon (organized/folk); Absent without one

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 937 | 93.9% |
| `folk` | 61 | 6.1% |

### `cult-form-goblin-twin`

Cult form of the goblin-twin flagship's pantheon (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds)

| value | count | share |
|---|---|---|

### `cult-form-kobold`

Cult form of the kobold flagship's pantheon (organized/folk); Absent without one

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `organized` | 679 | 70.1% |
| `folk` | 290 | 29.9% |

### `head-deity-domain-goblin`

Venue domain of the goblin flagship's head deity: solar, lunar, or ambient; Absent without a goblin pantheon

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `solar` | 998 | 100.0% |

### `head-deity-domain-goblin-twin`

Venue domain of the goblin-twin flagship's head deity (null control, spec §4); Absent without a goblin-twin pantheon

n = 0 present, 1000 absent (of 1000 worlds)

| value | count | share |
|---|---|---|

### `head-deity-domain-kobold`

Venue domain of the kobold flagship's head deity: solar, lunar, or ambient; Absent without a kobold pantheon

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `lunar` | 865 | 89.3% |
| `solar` | 104 | 10.7% |

### `head-deity-periodicity`

The sentiment tag of the goblin flagship's head deity (the most salient belief): 'eternal', 'cyclic', or 'ambient'; Absent if no goblin beliefs

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `cyclic` | 950 | 95.2% |
| `eternal` | 48 | 4.8% |

### `pantheon-cyclic-share-goblin`

Fraction of the goblin flagship pantheon's source phenomena that are periodic (the pick_kobold input the null control needs); Absent without a goblin pantheon

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.85714286 | 1 | 1 | 1 | 0.9122259 |

### `pantheon-cyclic-share-goblin-twin`

Fraction of the goblin-twin flagship pantheon's source phenomena that are periodic (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `pantheon-size`

Number of beliefs in the goblin flagship's pantheon; Absent if there are none

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 8 | 11 | 19 | 8.2104208 |

### `pantheon-size-goblin`

Number of deities in the goblin flagship's pantheon; Absent without one

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 8 | 11 | 19 | 8.2104208 |

### `pantheon-size-goblin-twin`

Number of deities in the goblin-twin flagship's pantheon (null control); Absent without one

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `pantheon-size-kobold`

Number of deities in the kobold flagship's pantheon; Absent without one

n = 969 present, 31 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 8 | 10 | 17 | 7.7708978 |

### `pantheon-verticality`

Whether the goblin flagship's pantheon is ranked (a high god presides) or flat; Absent if there is no goblin flagship pantheon

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `flat` | 998 | 100.0% |

## Weaknesses found here

### `belief-kind-bugbear`

- **D1**: "cyclic" holds 902/993 worlds (90.8%), at or above the 80% threshold

### `belief-kind-goblin`

- **D1**: "cyclic" holds 950/998 worlds (95.2%), at or above the 80% threshold

### `belief-kind-hobgoblin`

- **D1**: "cyclic" holds 951/999 worlds (95.2%), at or above the 80% threshold

### `belief-kind-kobold`

- **D1**: "cyclic" holds 913/969 worlds (94.2%), at or above the 80% threshold

### `blind-attribution-correct`

- **D1**: "true" holds 879/969 worlds (90.7%), at or above the 80% threshold

### `cult-form`

- **D1**: "organized" holds 937/998 worlds (93.9%), at or above the 80% threshold

### `cult-form-goblin`

- **D1**: "organized" holds 937/998 worlds (93.9%), at or above the 80% threshold

### `head-deity-domain-goblin`

- **D1**: "solar" holds 998/998 worlds (100.0%), at or above the 80% threshold

### `head-deity-domain-kobold`

- **D1**: "lunar" holds 865/969 worlds (89.3%), at or above the 80% threshold

### `head-deity-periodicity`

- **D1**: "cyclic" holds 950/998 worlds (95.2%), at or above the 80% threshold

### `pantheon-cyclic-share-goblin`

- **D4**: median 1 equals the max (0 .. 1)

### `pantheon-verticality`

- **D1**: "flat" holds 998/998 worlds (100.0%), at or above the 80% threshold

