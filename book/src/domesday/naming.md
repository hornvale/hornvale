<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Naming — The Domesday

How things are named: the conventions and confusable forms a language's naming layer produces.

## Metrics

### `epithet-honorific-goblin`

Whether every committed goblin deity epithet carries a prepended honorific affix — DETECTED from the committed epithet content: the committed word, case-folded, must end with the independently re-derived honorific-OFF stem and be strictly longer (Rank status basis → honorifics on, spec §7); Absent if goblins hold no pantheon

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 999 | 100.0% |
| `false` | 0 | 0.0% |

### `epithet-honorific-kobold`

Whether every committed kobold deity epithet carries a prepended honorific affix — DETECTED from the committed epithet content (see epithet-honorific-goblin); kobold's Knowledge status basis leaves honorifics off, so the committed epithet equals the plain stem and this reads false; Absent if kobolds hold no pantheon

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 0 | 0.0% |
| `false` | 982 | 100.0% |

### `name-collision-rate`

Fraction of this world's settlement + deity names (across every species) that duplicate another name in the same world — uniqueness is de-facto, not enforced (Task 9), so this MEASURES the collision rate rather than asserting zero. Scope: settlement and deity proper nouns only; epithets are deliberately EXCLUDED (they are descriptive words expected to repeat by design, so they are not collision candidates). Absent if the world has no settlement or deity names

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.12244898 | 0.47493404 | 0.54180107 | 0.58943089 | 0.7057903 | 0.52512356 |

### `name-gloss-true`

Whether every committed settlement name-gloss fact in this world is a truthful composition of that SAME settlement's own re-derived site concepts — up to twelve: the nine toponymic terrain concepts its own vertex offers (hydrography, elevation extrema, landmass size, wetness), The Toponym's characteristic climate variant, the biome, and the presiding sky phenomenon. The Wearing's Task 5 widened this vector past the original biome + presiding pair, and the close merge with The Toponym added the variant. Rather than restate the vector, this metric re-derives it by calling worldgen's own settlement_site_concepts, so there is no hand-maintained parallel definition to go stale — only this sentence, which has now gone stale twice; Absent if no settlement in this world carries a gloss

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `name-length-goblin`

Mean character length of every generated name (settlement, deity, epithet) attributed to goblins in this world; Absent if goblins produced no names

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4.75 | 7.3673469 | 8.3404255 | 9.35 | 18.224719 | 8.492323 |

### `name-length-goblin-twin`

Mean character length of every generated name attributed to the goblin-twin (null control); Absent if it produced no names

n = 0 present, 1000 absent (of 1000 worlds) — no world reports a value.

### `name-length-kobold`

Mean character length of every generated name (settlement, deity, epithet) attributed to kobolds in this world; Absent if kobolds produced no names

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.6666667 | 5.6833333 | 6.6666667 | 7.7710843 | 20.684932 | 6.9264986 |

### `name-pattern-signatures`

How many DISTINCT (ElementSource, Author) naming-pattern signatures this world's placed peoples derive from their society vectors (The Namesake §5.1(1); target >= 3); Absent if no placed people carries both psychology vectors

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5 | 9 | 9 | 9 | 9 | 8.977 |

### `name-people-recoverability`

The share of this world's placed peoples whose naming-pattern signature is unique among them — the structure-alone recoverability of a figure's people (The Namesake §5.1(2); target >= 2x the 1/n_peoples chance baseline); Absent if fewer than two peoples are placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.22222222 | 0.26315789 | 0.26315789 | 0.26315789 | 0.41176471 | 0.26493212 |

### `name-prefix-region-full-stack`

The share of this world's occupation founders whose region-scope render spends every element their name carries — the second, opposite half of The Namesake §5.2(2) (target < 0.50); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.14273995 | 0.36954798 | 0.47239661 | 0.59375 | 0.85947712 | 0.48356042 |

### `name-prefix-region-scope`

The MEDIAN number of elements this world's occupation founders render in against every other founder in the world (The Namesake §5.2(2); target >= 2); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `name-prefix-settlement-scope`

The share of this world's occupation founders whose name renders in exactly one element against the other founders of their own site (The Namesake §5.2(1); target >= 0.80); Absent if the world has no founders

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.87648456 | 0.94266442 | 0.95721568 | 0.96949891 | 1 | 0.95520504 |

### `name-syllables-goblin`

Mean syllable count of every generated name (settlement, deity, epithet) attributed to goblins in this world, counted as maximal vowel runs in the committed surface (an orthographic proxy — see the metric's own doc comment for its measured error bound); the reading The Wearing's claim needs, since character length cannot tell shorter words from the same words spelled tighter. Target 2-3; Absent if goblins produced no names

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.75 | 2.4651163 | 2.6666667 | 2.8888889 | 6.4494382 | 2.7078075 |

### `name-syllables-kobold`

Mean syllable count of every generated name attributed to kobolds in this world (see name-syllables-goblin); Absent if kobolds produced no names

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.1666667 | 1.9354839 | 2.1464603 | 2.3955224 | 7.5 | 2.2191126 |

### `name-transparency`

Share of this world's committed settlement names whose surface still contains, verbatim, the modern citation form of EVERY concept its own committed name-gloss names — read from the ledger and the lexicon, never from the naming code. The target is explicitly NOT 1.0 (The Wearing, spec §8): transparency was 100% by construction before this campaign, and that uniformity is the defect — most real toponyms are opaque to their own speakers. A distribution, pinned as a drift witness, never bounded. Absent if no settlement carries a non-empty gloss

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.23809524 | 0.62237762 | 0.69773218 | 0.7787234 | 0.96061269 | 0.69484314 |

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
| 4 | 6 | 7 | 7 | 7 | 6.541 |

## Weaknesses found here

### `epithet-honorific-goblin`

- **D1**: "true" holds 999/999 worlds (100.0%), at or above the 80% threshold

### `epithet-honorific-kobold`

- **D1**: "false" holds 982/982 worlds (100.0%), at or above the 80% threshold

### `name-pattern-signatures`

- **D3**: p25..p75 spans 0 (0.00% of the 4 min..max range), under the 5% bar
- **D4**: median 9 equals the max (5 .. 9)

### `name-people-recoverability`

- **D3**: p25..p75 spans 0 (0.00% of the 0.18954248999999998 min..max range), under the 5% bar

### `name-prefix-region-scope`

- **D2**: min == median == max == 1 across 1000 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `toponymic-core-size`

- **D2**: min == median == max == 7 across 1000 worlds
- **D4**: median 7 equals the min (7 .. 7)

### `toponymic-roots-won`

- **D4**: median 7 equals the max (4 .. 7)

