<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Language — The Domesday

How peoples speak: phonology, lexicon, and the divergence between related tongues.

## Metrics

### `cascade-rules-fired-bugbear`

How many DISTINCT sound rules in the bugbear cascade actually fire on at least one lexicon Root. Zero means the etymological layer is inert for this species (The Namesake §5.0); Absent if bugbear is unrostered or minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 1 | 2 | 4 | 1.352 |

### `cascade-rules-fired-goblin`

How many DISTINCT sound rules in the goblin cascade actually fire on at least one lexicon Root. Zero means the etymological layer is inert for this species (The Namesake §5.0); Absent if goblin is unrostered or minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 1 | 2 | 3 | 1.311 |

### `chorus-distinctiveness`

Mean pairwise distinctiveness() across every placed culture's account; Absent if fewer than 2 cultures placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.19013653 | 0.19560904 | 0.19560904 | 0.1963899 | 0.20396825 | 0.19627674 |

### `chorus-distortion`

Mean distortion() over every placed culture's account; Absent if no culture placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.37689934 | 0.38938492 | 0.38938492 | 0.3913986 | 0.41488889 | 0.39168295 |

### `chorus-param-spread`

Mean pairwise absolute difference in sky_capability across every placed culture — the input-side companion to chorus-variance's output-side number; Absent if fewer than 2 cultures placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.11345029 | 0.14492754 | 0.14492754 | 0.14492754 | 0.15372294 | 0.14544438 |

### `chorus-recoverability`

Mean recoverability() over every placed culture's account; Absent if no culture placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.862 | 0.90734266 | 0.91517857 | 0.91517857 | 0.94907407 | 0.9083395 |

### `chorus-sky-calibration`

Kendall tau between per-culture sky_capability and per-culture domain_distortion(..., "sky") over strictly-comparable pairs (both the capability and the distortion differ); expected sign ≤ 0 (distortion falls as capability rises); Absent if fewer than 2 cultures placed or no strictly-comparable pair exists (e.g. every pair ties on sky distortion)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1 | -1 | -1 | -1 | -1 | -1 |

### `chorus-variance`

Population variance of per-culture distortion() — the vacuity number, read against chorus-param-spread: a low variance can mean either every voice is genuinely alike or every voice hit the same floor; Absent if fewer than 2 cultures placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0010002709 | 0.0014819441 | 0.0014819441 | 0.0018085215 | 0.0024694131 | 0.0016377595 |

### `clean-outgroup-kobold`

Whether kobold — the family with no siblings — never coincides with the goblinoid family: for every concept kobold holds as a Root, its recorded proto-root differs from an INDEPENDENT re-draw of the "goblinoid" family proto-root for that same concept (spec §3's clean outgroup); Absent if kobold minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 999 | 99.9% |
| `false` | 1 | 0.1% |

### `confusable-homophony-bugbear`

Count of bugbear core homophone pairs that are CONFUSABLE (both concepts share a semantic domain); the same-domain subset of core-homophony-bugbear, always ≤ it; Absent if bugbear minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `confusable-homophony-goblin`

Count of goblin core homophone pairs that are CONFUSABLE (both concepts share a semantic domain — universal/body/kin — so they compete in one context); the same-domain subset of core-homophony-goblin, always ≤ it; the complement is FREE cross-domain homophony; Absent if goblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `confusable-homophony-hobgoblin`

Count of hobgoblin core homophone pairs that are CONFUSABLE (both concepts share a semantic domain); the same-domain subset of core-homophony-hobgoblin, always ≤ it; Absent if hobgoblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `confusable-homophony-kobold`

Count of kobold core homophone pairs that are CONFUSABLE (both concepts share a semantic domain); the same-domain subset of core-homophony-kobold, always ≤ it; Absent if kobold minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `core-homophony-bugbear`

Count of bugbear homophone pairs where BOTH concepts are core vocabulary (universal + body + kin packs); always ≤ homophony-count-bugbear; Absent if bugbear minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `core-homophony-goblin`

Count of goblin homophone pairs where BOTH concepts are core vocabulary (universal + body + kin packs) — the functional-load-restricted homophony the fix drives to zero; always ≤ homophony-count-goblin; Absent if goblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `core-homophony-hobgoblin`

Count of hobgoblin homophone pairs where BOTH concepts are core vocabulary (universal + body + kin packs); always ≤ homophony-count-hobgoblin; Absent if hobgoblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `core-homophony-kobold`

Count of kobold homophone pairs where BOTH concepts are core vocabulary (universal + body + kin packs); always ≤ homophony-count-kobold; Absent if kobold minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `distinguishable-capacity-bugbear`

Bugbear's distinguishable-syllable capacity (spec §2.3): onset × nucleus × coda fillings; bugbear draws the smallest family inventory; Absent if bugbear is off-roster

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4 | 30 | 64 | 140 | 1536 | 117.922 |

### `distinguishable-capacity-goblin`

Goblin's distinguishable-syllable capacity (spec §2.3): onset × nucleus × coda fillings, a lower bound on distinct syllables (tone folded into the nucleus); Absent if goblin is off-roster

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 6 | 48 | 120 | 240 | 2352 | 215.832 |

### `distinguishable-capacity-kobold`

Kobold's distinguishable-syllable capacity (spec §2.3): onset × nucleus × coda fillings; Absent if kobold is off-roster

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4 | 30 | 65 | 144 | 1620 | 122.234 |

### `divergence-magnitude-bugbear`

Count of DISTINCT proto segments (drawn from the shared goblinoid family proto-phonology) appearing in bugbear's own Root proto-roots that nativize.rs collapses onto an existing bugbear inventory segment (i.e. absent from bugbear's own inventory) — the measured cost of bugbear's nativization under the loudness-drawn inventory (spec §3); Absent if bugbear minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 3 | 4 | 5 | 11 | 4.431 |

### `divergence-magnitude-goblin`

Count of DISTINCT proto segments (drawn from the shared goblinoid family proto-phonology) appearing in goblin's own Root proto-roots that nativize.rs collapses onto an existing goblin inventory segment (i.e. absent from goblin's own inventory) — the measured cost of goblin's nativization under the loudness-drawn inventory (spec §3); Absent if goblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 2 | 3 | 4 | 10 | 2.979 |

### `divergence-magnitude-hobgoblin`

Count of DISTINCT proto segments (drawn from the shared goblinoid family proto-phonology) appearing in hobgoblin's own Root proto-roots that nativize.rs collapses onto an existing hobgoblin inventory segment (i.e. absent from hobgoblin's own inventory) — the measured cost of hobgoblin's nativization under the loudness-drawn inventory (spec §3); Absent if hobgoblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 2 | 3 | 9 | 2.488 |

### `divergence-real`

Whether some concept rooted in ALL THREE goblinoid daughters (goblin, hobgoblin, bugbear) has ≥2 distinct present-day forms — the seed-swept stemmatics guard (spec §3): descent is proven by shared INNOVATIONS, not a shared ancestor alone, so a degenerate family whose daughters are silent aliases of one another must read false; Absent if no concept is rooted in all three

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `exposure-sound-goblin`

Whether the goblin lexicon is exposure-sound: no concept an INDEPENDENT re-derivation of exposure classifies Unknown ever backs a Root entry, and every committed Gap carries a non-empty reason (spec §9.2); Absent if the goblin lexicon has no entries

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `exposure-sound-kobold`

Whether the kobold lexicon is exposure-sound: no concept an INDEPENDENT re-derivation of exposure classifies Unknown ever backs a Root entry, and every committed Gap carries a non-empty reason (spec §9.2); Absent if the kobold lexicon has no entries

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `homophony-count-bugbear`

Count of distinct-concept pairs whose bugbear Root.modern forms coincide (two proto-roots merged onto one surface form) — an observation, not a pass/fail invariant; expected highest among the goblinoid daughters, bugbear drawing the smallest family inventory; Absent if bugbear minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 11 | 20 | 36 | 227 | 27.112 |

### `homophony-count-goblin`

Count of distinct-concept pairs whose goblin Root.modern forms coincide (two proto-roots merged onto one surface form) — an observation, not a pass/fail invariant: homophony is legal and realistic, and this banks the confound L4's reconstruction will fight (homophones read as one word); Absent if goblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 5 | 10 | 76 | 7.423 |

### `homophony-count-hobgoblin`

Count of distinct-concept pairs whose hobgoblin Root.modern forms coincide (two proto-roots merged onto one surface form) — an observation, not a pass/fail invariant; Absent if hobgoblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 1 | 4 | 11 | 87 | 7.665 |

### `homophony-count-kobold`

Count of distinct-concept pairs whose kobold Root.modern forms coincide (two proto-roots landed on one surface form) — an observation, not a pass/fail invariant, banked for the clean-outgroup comparison; Absent if kobold minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 4 | 11 | 98 | 7.868 |

### `homophony-merger-share-bugbear`

Fraction of bugbear colliding surface forms that are MERGERS (≥2 distinct proto-forms) rather than draw-collisions; Absent if bugbear has no collision

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `homophony-merger-share-goblin`

Fraction of goblin colliding surface forms that are MERGERS (colliding roots carry ≥2 distinct proto-forms — the cascade or nativization made the collision after the proto) rather than draw-collisions (one shared proto); Absent if goblin has no collision (an undefined ratio, never reported as 0)

n = 805 present, 195 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `homophony-merger-share-hobgoblin`

Fraction of hobgoblin colliding surface forms that are MERGERS (≥2 distinct proto-forms) rather than draw-collisions; Absent if hobgoblin has no collision

n = 775 present, 225 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `homophony-merger-share-kobold`

Fraction of kobold colliding surface forms that are MERGERS (≥2 distinct proto-forms) rather than draw-collisions; Absent if kobold has no collision

n = 674 present, 326 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `inventory-closure-bugbear`

Whether every bugbear lexicon Root's modern form draws only segments in bugbear's own drawn inventory (spec §2.2's nativization contract); Absent if bugbear minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `inventory-closure-goblin`

Whether every goblin lexicon Root's modern form draws only segments in goblin's own drawn inventory (spec §2.2's nativization contract); Absent if goblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `inventory-closure-hobgoblin`

Whether every hobgoblin lexicon Root's modern form draws only segments in hobgoblin's own drawn inventory (spec §2.2's nativization contract); Absent if hobgoblin minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `inventory-closure-kobold`

Whether every kobold lexicon Root's modern form draws only segments in kobold's own drawn inventory (spec §2.2's nativization contract); Absent if kobold minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `lexicon-regular-family`

Whether every daughter's lexicon (goblin, hobgoblin, bugbear, kobold) is Neogrammarian-regular: every Root's recorded derivation replays byte-identically through evolve, checked for EVERY daughter in this world's roster (spec §9.1, generalized family-wide); Absent if no daughter minted a Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `lexicon-regular-goblin`

Whether every goblin lexicon Root entry's recorded sound-change derivation replays byte-identically through evolve (Neogrammarian regularity, spec §9.1); Absent if the goblin lexicon minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `lexicon-regular-kobold`

Whether every kobold lexicon Root entry's recorded sound-change derivation replays byte-identically through evolve (Neogrammarian regularity, spec §9.1); Absent if the kobold lexicon minted no Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `monophyly-dwarf`

Whether every dwarf daughter's (desert-dwarf, duergar, gully-dwarf, hill-dwarf, mountain-dwarf) Root derivation.proto matches an INDEPENDENT re-draw of the shared "dwarf" family proto-root for that concept (spec §3: cognates share a proto ancestor) — never reading the family proto back from a sibling's own recorded derivation; Absent if no dwarf daughter minted a Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `monophyly-elf`

Whether every elf daughter's (desert-elf, drow, high-elf, sea-elf, snow-elf, wood-elf) Root derivation.proto matches an INDEPENDENT re-draw of the shared "elf" family proto-root for that concept (spec §3: cognates share a proto ancestor) — never reading the family proto back from a sibling's own recorded derivation; Absent if no elf daughter minted a Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `monophyly-goblinoid`

Whether every goblinoid daughter's (goblin, hobgoblin, bugbear) Root derivation.proto matches an INDEPENDENT re-draw of the shared "goblinoid" family proto-root for that concept (spec §3: cognates share a proto ancestor) — never reading the family proto back from a sibling's own recorded derivation; Absent if no goblinoid daughter minted a Root

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 1000 | 100.0% |
| `false` | 0 | 0.0% |

### `phonotactic-validity-goblin`

Whether every generated name (settlement, deity, epithet) attributed to goblins in this world re-validates against the goblin phonology, independently re-derived and re-parsed from the surface string; Absent if goblins produced no names

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 999 | 100.0% |
| `false` | 0 | 0.0% |

### `phonotactic-validity-kobold`

Whether every generated name (settlement, deity, epithet) attributed to kobolds in this world re-validates against the kobold phonology, independently re-derived and re-parsed from the surface string; Absent if kobolds produced no names

n = 984 present, 16 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 984 | 100.0% |
| `false` | 0 | 0.0% |

### `tone-count-goblin`

Size of goblin's realized tone inventory (spec §11): 1 for an atonal people; >1 only for a tone-capable species; Absent if goblin is off-roster

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `tone-count-kobold`

Size of kobold's realized tone inventory (spec §11): 1 for an atonal people; Absent if kobold is off-roster

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

## Weaknesses found here

### `chorus-param-spread`

- **D3**: p25..p75 spans 0 (0.00% of the 0.04027265000000001 min..max range), under the 5% bar

### `chorus-sky-calibration`

- **D2**: min == median == max == -1 across 1000 worlds
- **D4**: median -1 equals the min (-1 .. -1)

### `clean-outgroup-kobold`

- **D1**: "true" holds 999/1000 worlds (99.9%), at or above the 80% threshold

### `confusable-homophony-bugbear`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `confusable-homophony-goblin`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `confusable-homophony-hobgoblin`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `confusable-homophony-kobold`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `core-homophony-bugbear`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `core-homophony-goblin`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `core-homophony-hobgoblin`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `core-homophony-kobold`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `divergence-real`

- **D1**: "true" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `exposure-sound-goblin`

- **D1**: "true" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `exposure-sound-kobold`

- **D1**: "true" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `homophony-merger-share-bugbear`

- **D2**: min == median == max == 1 across 998 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `homophony-merger-share-goblin`

- **D2**: min == median == max == 1 across 805 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `homophony-merger-share-hobgoblin`

- **D2**: min == median == max == 1 across 775 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `homophony-merger-share-kobold`

- **D2**: min == median == max == 1 across 674 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `tone-count-goblin`

- **D2**: min == median == max == 1 across 1000 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `tone-count-kobold`

- **D2**: min == median == max == 1 across 1000 worlds
- **D4**: median 1 equals the min (1 .. 1)

