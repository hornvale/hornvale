<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->

Labels are permanent save-format contracts; regeneration uses epoch suffixes (e.g. `settlement/name/v2`), never renames.

### hornvale-astronomy

| Label | Meaning |
|---|---|
| `astronomy` | root stream for sky genesis |
| `astronomy/star-mass` | main-sequence star mass draw |
| `astronomy/anchor-mass` | anchor world mass draw |
| `astronomy/rotation` | rotation regime and period draw |
| `astronomy/orbit` | anchor orbital distance draw |
| `astronomy/obliquity` | axial tilt draw |
| `astronomy/moon-count` | how many moons |
| `astronomy/moons` | per-moon mass/distance draws (sequential attempts) |
| `astronomy/neighbors` | neighbor class/distance draws |
| `astronomy/forcing` | deep-time orbital forcing |
| `astronomy/phase-offsets` | per-body genesis phase offsets |
| `astronomy/neighbor-positions` | per-neighbor celestial position draws (declination, right ascension) |
| `astronomy/spin-direction` | spin-direction draw: prograde or retrograde |
| `astronomy/moon-inclinations` | per-moon orbital-inclination draws |
| `astronomy/wanderer-count` | how many wandering planets |
| `astronomy/wanderers` | per-wanderer parameter draws, sequential |
| `astronomy/starfield` | background starfield: count + per-star position/brightness (derived on demand) |
| `astronomy/moon-nodes` | per-moon ascending-node longitude draws |
| `astronomy/star-age` | stellar age draw |
| `astronomy/moon-formation` | per-moon formation-mechanism draw (giant impact vs. capture) |
| `astronomy/moon-density` | per-moon density draw (drawn only for captured moons; impact moons still consume it) |
| `astronomy/moon-age` | per-moon age draw (jitter around the planet's age either way) |

### hornvale-climate

*(no seed-derivation streams)*

### hornvale-culture

*(no seed-derivation streams)*

### hornvale-kernel

| Label | Meaning |
|---|---|
| `room/face` | room base face |
| `room/child` | room child descent |

### hornvale-language

| Label | Meaning |
|---|---|
| `language/<species>/phonology/inventory` | per-species phoneme inventory draw under the articulation envelope; for a family's shared proto-language (e.g. goblinoid) a family name occupies the <species> slot — a language with no speakers |
| `language/<species>/phonology/phonotactics` | per-species syllable phonotactic templates (onsets, nuclei, codas) |
| `language/<species>/phonology/tones` | the phonology epoch's tone-inventory draw: which contrastive level tone (High/Low) joins Neutral for a partly-tonal species (tonality → 2 tones); atonal (1) and fully tonal (3) draw nothing here |
| `language/<species>/name/settlement` | (retired at The Words, superseded by name/settlement/v2) per-settlement name (salted by cell id): a bare stem |
| `language/<species>/name/deity` | (retired at The Words, superseded by name/deity/v2) per-deity name (salted by belief id): a bare stem biased toward closed syllables |
| `language/<species>/name/epithet` | (retired at The Words, superseded by name/epithet/v2) per-deity epithet (salted by belief id): a descriptive root, optionally reduplicated and honorific-prefixed |
| `language/<species>/name/settlement/v2` | the glossed settlement name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above |
| `language/<species>/name/deity/v2` | the glossed deity name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above |
| `language/<species>/name/epithet/v2` | the glossed epithet (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the v1 draw above |
| `language/<family>/lexicon/root/v3/<concept>` | per-concept family proto-root, injectively and MERGER-AWARELY assigned (epoch root/v3): the open-addressing draw also rejects a core candidate whose evolved form would merge with an already-placed core concept in any daughter, so core homophony is zero; family == species for a singleton stock. Probe re-draws key a /probe/<n> sub-stream |
| `language/<family>/lexicon/root/v2/<concept>` | (retired by the merger-aware assignment, superseded by root/v3) the injective-but-proto-only family assignment |
| `language/goblin/lexicon/root/<concept>` | (retired at The Branches, superseded by language/goblinoid/lexicon/root/<concept>) pre-Branches per-species goblin proto-root |
| `language/<species>/lexicon/cascade` | the species' 2-4 rule sound-change cascade, applied by evolve() to every proto-root |
| `language/<species>/lexicon/headedness` | the species' drawn compound-joining order (HeadFirst/HeadLast), gating LexEntry::Compound component order |

### hornvale-locale

| Label | Meaning |
|---|---|
| `locale/regime/micro` | room sub-cell micro-field |
| `locale/regime/variety` | room descriptor variety draw |
| `locale/regime/substrate` | room substrate-detail draw |
| `locale/strangeness/place` | world rarity-budget placement pass |

### hornvale-paleoclimate

*(no seed-derivation streams)*

### hornvale-religion

*(no seed-derivation streams)*

### hornvale-settlement

| Label | Meaning |
|---|---|
| `settlement` | root stream for settlement generation |
| `settlement/name` | RETIRED (pre-Tongues): per-settlement generated name, goblin stream. Names now derive under language/<species>/name/settlement. Kept documented for legacy-save continuity; never renamed. |
| `settlement/placement` | RETIRED (the-gathering): per-settlement population against carrying capacity, goblin stream. Population is now the conserved catchment readout of hornvale-demography's flow-condensation, drawing nothing from the seed. Kept documented for legacy-save continuity; never renamed. |
| `settlement/kobold/name` | RETIRED (pre-Tongues): per-settlement generated name, kobold stream (species-qualified; goblin kept settlement/name). Names now derive under language/<species>/name/settlement. Kept documented for legacy-save continuity; never renamed. |
| `settlement/kobold/population` | RETIRED (the-gathering): per-settlement population, kobold stream (species-qualified; goblin kept settlement/placement). Population is now the conserved catchment readout of hornvale-demography's flow-condensation, drawing nothing from the seed. Kept documented for legacy-save continuity; never renamed. |

### hornvale-species

*(no seed-derivation streams)*

### hornvale-terrain

| Label | Meaning |
|---|---|
| `terrain` | root stream for tectonic genesis |
| `terrain/plate-count` | how many plates |
| `terrain/plate-seeds` | per-plate seed positions on the sphere |
| `terrain/plate-motion` | per-plate Euler pole axis and rate draws |
| `terrain/maturity` | per-plate orogenic maturity draws |
| `terrain/hotspots` | hotspot count, positions, and strengths |
| `terrain/ocean-fraction` | target ocean fraction draw |
| `terrain/coast-render` | render-lens coastline noise (hash-noise only; no stream draws) |
| `terrain/cratons` | margin draw (scales the ocean-fraction-derived budget, Task 9 iteration 3'), craton count, then per-craton center/radius/age |
| `terrain/plate-weights` | per-plate heavy-tailed Voronoi weight draws |
| `terrain/plate-edge` | plate-edge noise (hash-noise only; no stream draws) |
| `terrain/lithology` | lithology sub-cell hash-noise (hash-noise only; no stream draws) |

### hornvale-vessel

| Label | Meaning |
|---|---|
| `vessel/agent` | minted agent id draw |
| `vessel/walk` | walker-battery deterministic walk |

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
