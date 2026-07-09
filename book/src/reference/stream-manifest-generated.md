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
| `language/<species>/name/settlement` | per-settlement name (salted by cell id): a bare stem |
| `language/<species>/name/deity` | per-deity name (salted by belief id): a bare stem biased toward closed syllables |
| `language/<species>/name/epithet` | per-deity epithet (salted by belief id): a descriptive root, optionally reduplicated and honorific-prefixed |

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

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
