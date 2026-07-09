<!-- GENERATED FILE — do not edit. Regenerate with `hornvale concepts`. -->

### Predicates

| Predicate | Functional | Meaning |
|---|---|---|
| `biome` | yes | biome of a place |
| `cell-id` | yes | Geosphere cell id a settlement sits on |
| `cult-form` | yes | the cult form of a belief (organized or folk) |
| `day-length-std` | yes | solar day length in standard days, for spinning worlds |
| `deity-epithet` | yes | a belief's epithet (roman) |
| `deity-epithet-ipa` | yes | a belief's epithet (IPA transcription) |
| `deity-name` | yes | a belief's deity name (roman) |
| `deity-name-ipa` | yes | a belief's deity name (IPA transcription) |
| `derived-from-phenomenon` | yes | phenomenon kind a belief mythologizes |
| `genesis-note` | no | a degradation or refusal recorded during sky genesis |
| `has-caste` | no | a caste present in a settlement |
| `held-by` | no | a community holding a belief |
| `high-god` | yes | the presiding deity of a ranked pantheon |
| `highest-elevation-m` | yes | highest globe cell elevation in meters |
| `is-belief` | yes | subject is a belief |
| `is-place` | yes | subject is a traversable place |
| `is-settlement` | yes | subject is a settlement |
| `latitude` | yes | settlement latitude, degrees |
| `longitude` | yes | settlement longitude, degrees |
| `moon-count` | yes | how many moons the anchor world has |
| `moon-period-std` | no | orbital period of a moon, in standard days |
| `name` | yes | canonical name of an entity |
| `notable-neighbor` | no | a notable neighbor star visible in the night sky |
| `obliquity-degrees` | yes | axial tilt of the anchor world, in degrees |
| `ocean-fraction` | yes | fraction of globe cells below sea level |
| `peopled-by` | yes | the species that peoples a settlement |
| `plate-count` | yes | how many tectonic plates the globe has |
| `population` | yes | population of a settlement |
| `scenario-pin` | no | an experimenter-supplied pin string conditioning genesis |
| `sea-level-m` | yes | sea level in meters |
| `sentiment` | yes | a belief's sentiment (eternal, cyclic, or ambient) |
| `settlement-pin` | no | a settlement scenario pin, round-trippable |
| `sky-provider` | yes | which astronomy provider this world uses (constant or generated) |
| `species-activity-cycle` | yes | when a species is awake: diurnal, nocturnal, crepuscular |
| `species-deliberation-latency` | yes | decision slowness, 0-1 |
| `species-exotic-manner` | yes | exotic manner: none, trill, click, ejective |
| `species-in-group-radius` | yes | how wide 'us' is drawn, 0-1 |
| `species-labiality` | yes | lip-rounding and jaw-closure, 0-1 |
| `species-name` | yes | a species entity's name |
| `species-night-vision` | yes | night-sky acuity, 0-1 |
| `species-sibilance` | yes | sibilance emphasis, 0-1 |
| `species-sky-attention` | yes | sky vs. ground attention, 0-1 |
| `species-sociality-mode` | yes | hierarchic or communal |
| `species-status-basis` | yes | rank, knowledge, or generosity |
| `species-threat-response` | yes | flee 0 ↔ stand 1 |
| `species-time-horizon` | yes | planning depth, 0-1 |
| `species-voice-loudness` | yes | voice-loudness range, 0-1 |
| `species-voicing` | yes | voicing emphasis, 0-1 |
| `species-vowel-space` | yes | vowel-space size, 0-1 |
| `star-class` | yes | the host star's descriptive spectral class |
| `subsistence` | yes | a settlement's subsistence mode |
| `tenet` | yes | the tenet text of a belief |
| `terrain-note` | no | a note recorded during tectonic genesis |
| `terrain-pin` | no | a terrain scenario pin, round-trippable |
| `tidally-locked` | yes | the anchor world is tidally locked (no local day) |
| `year-length-std` | yes | year length in standard days |

### Phenomenon kinds

| Kind | Meaning |
|---|---|
| `ambient` | a pervasive atmospheric condition |
| `celestial-body` | a body visible in the sky |
| `night-star` | a fixed star notable in the night sky |
| `seasonal-cycle` | the annual daylight cycle |

### Concepts

| Concept | Domain | Kind | Meaning |
|---|---|---|---|
| `abyssal` | climate | Terrain | a biome class |
| `alpine` | climate | Terrain | a biome class |
| `bathypelagic` | climate | Terrain | a biome class |
| `coral-reef` | climate | Terrain | a biome class |
| `desert` | climate | Terrain | a biome class |
| `epipelagic` | climate | Terrain | a biome class |
| `goblin-kind` | species | Living | a goblin |
| `god` | religion | Social | a deity |
| `hadal-trench` | climate | Terrain | a biome class |
| `hearth` | settlement | Social | the fire at the center of a home |
| `home` | settlement | Social | one's dwelling |
| `hydrothermal-vent` | climate | Terrain | a biome class |
| `ice` | climate | Substance | frozen water |
| `kelp-forest` | climate | Terrain | a biome class |
| `kobold-kind` | species | Living | a kobold |
| `mesopelagic` | climate | Terrain | a biome class |
| `moon` | astronomy | Celestial | a moon |
| `mountain` | terrain | Terrain | high ground |
| `night` | astronomy | Celestial | the dark half of the day-night cycle |
| `rain` | climate | Substance | liquid precipitation |
| `savanna` | climate | Terrain | a biome class |
| `sea` | terrain | Terrain | a body of salt water |
| `sea-ice` | climate | Terrain | a biome class |
| `shrubland` | climate | Terrain | a biome class |
| `snow` | climate | Substance | frozen precipitation |
| `spirit` | religion | Social | a lesser or unseen supernatural presence |
| `star` | astronomy | Celestial | a fixed point of light in the night sky |
| `stone` | terrain | Substance | rock |
| `sun` | astronomy | Celestial | the sun |
| `taiga` | climate | Terrain | a biome class |
| `temperate-forest` | climate | Terrain | a biome class |
| `temperate-grassland` | climate | Terrain | a biome class |
| `temperate-rainforest` | climate | Terrain | a biome class |
| `tropical-rainforest` | climate | Terrain | a biome class |
| `tropical-seasonal-forest` | climate | Terrain | a biome class |
| `tundra` | climate | Terrain | a biome class |
| `upwelling` | climate | Terrain | a biome class |
