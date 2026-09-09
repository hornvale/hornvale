<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Naming — The Domesday

How things are named: the conventions and confusable forms a language's naming layer produces.

## Metrics

### `epithet-honorific-goblin`

Whether every committed goblin deity epithet carries a prepended honorific affix — DETECTED from the committed epithet content: the committed word, case-folded, must end with the independently re-derived honorific-OFF stem and be strictly longer (Rank status basis → honorifics on, spec §7); Absent if goblins hold no pantheon

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `epithet-honorific-kobold`

Whether every committed kobold deity epithet carries a prepended honorific affix — DETECTED from the committed epithet content (see epithet-honorific-goblin); kobold's Knowledge status basis leaves honorifics off, so the committed epithet equals the plain stem and this reads false; Absent if kobolds hold no pantheon

n = 981 present, 19 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 0 | 0.0% |
| `false` | 981 | 100.0% |

### `name-collision-rate`

Fraction of this world's settlement + deity names (across every species) that duplicate another name in the same world — uniqueness is de-facto, not enforced (Task 9), so this MEASURES the collision rate rather than asserting zero. Scope: settlement and deity proper nouns only; epithets are deliberately EXCLUDED (they are descriptive words expected to repeat by design, so they are not collision candidates). Absent if the world has no settlement or deity names

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.076086957 | 0.46180556 | 0.53302349 | 0.58508159 | 0.70855148 | 0.51520032 |

### `name-gloss-true`

Whether every committed settlement name-gloss fact in this world is a truthful composition of that SAME settlement's own re-derived site concepts — up to twelve: the nine toponymic terrain concepts its own vertex offers (hydrography, elevation extrema, landmass size, wetness), The Toponym's characteristic climate variant, the biome, and the presiding sky phenomenon. The Wearing's Task 5 widened this vector past the original biome + presiding pair, and the close merge with The Toponym added the variant. Rather than restate the vector, this metric re-derives it by calling worldgen's own settlement_site_concepts, so there is no hand-maintained parallel definition to go stale — only this sentence, which has now gone stale twice; Absent if no settlement in this world carries a gloss

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `name-length-goblin`

Mean character length of every generated name (settlement, deity, epithet) attributed to goblins in this world; Absent if goblins produced no names

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4.8 | 7.4090909 | 8.2939815 | 9.4130435 | 14.774194 | 8.4968946 |

### `name-length-goblin-twin`

Mean character length of every generated name attributed to the goblin-twin (null control); Absent if it produced no names

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `name-length-kobold`

Mean character length of every generated name (settlement, deity, epithet) attributed to kobolds in this world; Absent if kobolds produced no names

n = 981 present, 19 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.7419355 | 5.6615385 | 6.6428571 | 7.7173913 | 18.857143 | 6.8736843 |

### `name-pattern-signatures`

How many DISTINCT (ElementSource, Author) naming-pattern signatures this world's placed peoples derive from their society vectors (The Namesake §5.1(1); target >= 3); Absent if no placed people carries both psychology vectors

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 7 | 9 | 9 | 9 | 9 | 8.978 |

### `name-people-recoverability`

The share of this world's placed peoples whose naming-pattern signature is unique among them — the structure-alone recoverability of a figure's people (The Namesake §5.1(2); target >= 2x the 1/n_peoples chance baseline); Absent if fewer than two peoples are placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.28571429 | 0.33333333 | 0.33333333 | 0.33333333 | 0.55555556 | 0.33613825 |

### `name-prefix-region-full-stack`

The share of this world's occupation founders whose region-scope render spends every element their name carries — the second, opposite half of The Namesake §5.2(2) (target < 0.50); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.11580217 | 0.39226519 | 0.5060906 | 0.61359223 | 0.95692026 | 0.50512653 |

### `name-prefix-region-scope`

The MEDIAN number of elements this world's occupation founders render in against every other founder in the world (The Namesake §5.2(2); target >= 2); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 2 | 1.002 |

### `name-prefix-settlement-scope`

The share of this world's occupation founders whose name renders in exactly one element against the other founders of their own site (The Namesake §5.2(1); target >= 0.80); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.82131661 | 0.94594595 | 0.9624291 | 0.97536946 | 1 | 0.95992745 |

### `name-syllables-goblin`

Mean syllable count of every generated name (settlement, deity, epithet) attributed to goblins in this world, counted as maximal vowel runs in the committed surface (an orthographic proxy — see the metric's own doc comment for its measured error bound); the reading The Wearing's claim needs, since character length cannot tell shorter words from the same words spelled tighter. Target 2-3; Absent if goblins produced no names

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.8461538 | 2.48 | 2.6666667 | 2.8965517 | 5.6060606 | 2.7094596 |

### `name-syllables-kobold`

Mean syllable count of every generated name attributed to kobolds in this world (see name-syllables-goblin); Absent if kobolds produced no names

n = 981 present, 19 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.1 | 1.9375 | 2.1333333 | 2.3623188 | 7.7272727 | 2.2055805 |

### `name-transparency`

Share of this world's committed settlement names whose surface still contains, verbatim, the modern citation form of EVERY concept its own committed name-gloss names — read from the ledger and the lexicon, never from the naming code. The target is explicitly NOT 1.0 (The Wearing, spec §8): transparency was 100% by construction before this campaign, and that uniformity is the defect — most real toponyms are opaque to their own speakers. A distribution, pinned as a drift witness, never bounded. Absent if no settlement carries a non-empty gloss

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.26027397 | 0.64137931 | 0.72853481 | 0.80237154 | 0.96650718 | 0.71654414 |

### `toponymic-core-size`

How many concepts this world's registry reports in the `toponymic` domain (The Assay). The denominator for `toponymic-roots-won`, kept as its own column so a registry change is distinguishable from a worlds change.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 7 | 7 | 7 | 7 | 7 | 7 |

### `toponymic-roots-won`

How many toponymic-domain concepts reach `ExposureClass::Steeped` for at least one placed people (The Assay). Replaces `windows/worldgen/tests/exposure.rs`'s up-to-9-world sweep for a witness: a concept no world in the census ever steeps is a structurally dead gate, and the census says so as a rate.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 3 | 6 | 7 | 7 | 7 | 6.474 |

## Weaknesses found here

### `epithet-honorific-goblin`

- **D1**: "true" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `epithet-honorific-kobold`

- **D1**: "false" holds 981/981 worlds (100.0%), at or above the 80% threshold

### `name-pattern-signatures`

- **D3**: p25..p75 spans 0 (0.00% of the 2 min..max range), under the 5% bar
- **D4**: median 9 equals the max (7 .. 9)

### `name-people-recoverability`

- **D3**: p25..p75 spans 0 (0.00% of the 0.26984126999999997 min..max range), under the 5% bar

### `name-prefix-region-scope`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 1 equals the min (1 .. 2)

### `toponymic-core-size`

- **D2**: min == median == max == 7 across 1000 worlds
- **D4**: median 7 equals the min (7 .. 7)

### `toponymic-roots-won`

- **D4**: median 7 equals the max (3 .. 7)

