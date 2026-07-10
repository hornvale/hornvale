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
| `astronomy/neighbors` | notable-neighbor class/distance draws |
| `astronomy/forcing` | deep-time orbital forcing |
| `astronomy/phase-offsets` | per-body genesis phase offsets |
| `astronomy/neighbor-positions` | per-neighbor celestial position draws (declination, right ascension) |

### hornvale-climate

*(no seed-derivation streams)*

### hornvale-culture

*(no seed-derivation streams)*

### hornvale-language

| Label | Meaning |
|---|---|
| `language/<species>/phonology/inventory` | per-species phoneme inventory draw under the articulation envelope |
| `language/<species>/phonology/phonotactics` | per-species syllable phonotactic templates (onsets, nuclei, codas) |
| `language/<species>/name/settlement` | (retired at The Words, superseded by name/settlement/v2) per-settlement name (salted by cell id): a bare stem |
| `language/<species>/name/deity` | (retired at The Words, superseded by name/deity/v2) per-deity name (salted by belief id): a bare stem biased toward closed syllables |
| `language/<species>/name/epithet` | (retired at The Words, superseded by name/epithet/v2) per-deity epithet (salted by belief id): a descriptive root, optionally reduplicated and honorific-prefixed |
| `language/<species>/name/settlement/v2` | the glossed settlement name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above |
| `language/<species>/name/deity/v2` | the glossed deity name (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the bare-stem v1 draw above |
| `language/<species>/name/epithet/v2` | the glossed epithet (Task 9): composed from the lexicon's roots/compounds under the species' drawn headedness, replacing the v1 draw above |
| `language/<species>/lexicon/root/<concept>` | per-concept proto-root (1-2 syllables, from the phonotactic templates) |
| `language/<species>/lexicon/cascade` | the species' 2-4 rule sound-change cascade, applied by evolve() to every proto-root |
| `language/<species>/lexicon/headedness` | the species' drawn compound-joining order (HeadFirst/HeadLast), gating LexEntry::Compound component order |

### hornvale-religion

*(no seed-derivation streams)*

### hornvale-settlement

| Label | Meaning |
|---|---|
| `settlement` | root stream for settlement generation |
| `settlement/name` | RETIRED (pre-Tongues): per-settlement generated name, goblin stream. Names now derive under language/<species>/name/settlement. Kept documented for legacy-save continuity; never renamed. |
| `settlement/placement` | per-settlement population against carrying capacity |
| `settlement/kobold/name` | RETIRED (pre-Tongues): per-settlement generated name, kobold stream (species-qualified; goblin kept settlement/name). Names now derive under language/<species>/name/settlement. Kept documented for legacy-save continuity; never renamed. |
| `settlement/kobold/population` | per-settlement population, kobold stream (species-qualified; goblin keeps settlement/placement) |

### hornvale-species

*(no seed-derivation streams)*

### hornvale-terrain

| Label | Meaning |
|---|---|
| `terrain` | root stream for tectonic genesis |
| `terrain/plate-count` | how many plates |
| `terrain/plate-seeds` | per-plate seed positions on the sphere |
| `terrain/plate-kind` | continental fraction and per-plate continental rolls |
| `terrain/plate-motion` | per-plate Euler pole axis and rate draws |
| `terrain/maturity` | per-plate orogenic maturity draws |
| `terrain/hotspots` | hotspot count, positions, and strengths |
| `terrain/ocean-fraction` | target ocean fraction draw |
| `terrain/coast-render` | render-lens coastline noise (hash-noise only; no stream draws) |
| `terrain/cratons` | continental budget, craton count, then per-craton center/radius/age |
| `terrain/plate-weights` | per-plate heavy-tailed Voronoi weight draws |
| `terrain/plate-edge` | plate-edge noise (hash-noise only; no stream draws) |

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
