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
| `eccentricity-mean` | yes | mean orbital eccentricity (deep-time forcing) |
| `fossil-shoreline` | no | a fossil shoreline left by deep-time sea-level change |
| `frost-retreat` | no | the deep-time record of deglaciation |
| `genesis-note` | no | a degradation or refusal recorded during sky genesis |
| `glacial-maximum-era` | yes | standard day of peak ice extent |
| `has-caste` | no | a caste present in a settlement |
| `held-by` | no | a community holding a belief |
| `high-god` | yes | the presiding deity of a ranked pantheon |
| `highest-elevation-m` | yes | highest globe cell elevation in meters |
| `is-belief` | yes | subject is a belief |
| `is-place` | yes | subject is a traversable place |
| `is-settlement` | yes | subject is a settlement |
| `latitude` | yes | settlement latitude, degrees |
| `longitude` | yes | settlement longitude, degrees |
| `max-ice-fraction` | yes | land fraction under ice at the glacial maximum |
| `moon-count` | yes | how many moons the anchor world has |
| `moon-period-std` | no | orbital period of a moon, in standard days |
| `name` | yes | canonical name of an entity |
| `name-gloss` | yes | the glossed meaning of an entity's generated name |
| `notable-neighbor` | no | a notable neighbor star visible in the night sky |
| `obliquity-amplitude` | yes | obliquity oscillation amplitude, degrees (moon-coupled) |
| `obliquity-degrees` | yes | axial tilt of the anchor world, in degrees |
| `ocean-fraction` | yes | fraction of globe cells below sea level |
| `peopled-by` | yes | the species that peoples a settlement |
| `plate-count` | yes | how many tectonic plates the globe has |
| `population` | yes | population of a settlement |
| `refugium` | no | a place habitable through the glacial maximum |
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
| `species-tonality` | yes | tonal propensity, 0 atonal ↔ 1 tonal |
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
| `abyssal` | climate | terrain | a biome class |
| `alpine` | climate | terrain | a biome class |
| `bathypelagic` | climate | terrain | a biome class |
| `blood` | language | body | the circulating fluid of a body |
| `blue` | language | quality | the color term for blue |
| `bone` | language | body | the rigid frame of a body |
| `brown` | language | quality | the color term for brown |
| `bugbear-kind` | species | living | a bugbear |
| `child` | language | kin | one's son or daughter |
| `coral-reef` | climate | terrain | a biome class |
| `dark` | language | quality | the color term for black/dark hues |
| `day` | language | celestial | the light half of the day-night cycle |
| `desert` | climate | terrain | a biome class |
| `die` | language | quality | to cease living |
| `earth` | language | terrain | the ground underfoot |
| `eat` | language | quality | to consume food |
| `epipelagic` | climate | terrain | a biome class |
| `eye` | language | body | the organ of sight |
| `fire` | language | substance | flame and heat |
| `foot` | language | body | the walking limb-end |
| `gloom` | language | quality | the deepest, starless dark |
| `goblin-kind` | species | living | a goblin |
| `god` | religion | social | a deity |
| `green` | language | quality | the color term for green |
| `hadal-trench` | climate | terrain | a biome class |
| `hand` | language | body | the manipulating limb-end |
| `hearth` | settlement | social | the fire at the center of a home |
| `hobgoblin-kind` | species | living | a hobgoblin |
| `home` | settlement | social | one's dwelling |
| `hydrothermal-vent` | climate | terrain | a biome class |
| `ice` | climate | substance | frozen water |
| `kelp-forest` | climate | terrain | a biome class |
| `kobold-kind` | species | living | a kobold |
| `light` | language | quality | the color term for white/light hues |
| `many` | language | quality | an indefinitely large count |
| `mesopelagic` | climate | terrain | a biome class |
| `moon` | astronomy | celestial | a moon |
| `mountain` | terrain | terrain | high ground |
| `mouth` | language | body | the organ of eating and speech |
| `name` | language | social | a word that identifies one who bears it |
| `night` | astronomy | celestial | the dark half of the day-night cycle |
| `one` | language | quality | the cardinal number 1 |
| `parent` | language | kin | one's father or mother |
| `rain` | climate | substance | liquid precipitation |
| `red` | language | quality | the color term for red |
| `savanna` | climate | terrain | a biome class |
| `sea` | terrain | terrain | a body of salt water |
| `sea-ice` | climate | terrain | a biome class |
| `shadow` | language | quality | cast dark, distinct from open gloom |
| `shrubland` | climate | terrain | a biome class |
| `sibling` | language | kin | one's brother or sister |
| `sleep` | language | quality | to rest unconscious |
| `snow` | climate | substance | frozen precipitation |
| `spirit` | religion | social | a lesser or unseen supernatural presence |
| `star` | astronomy | celestial | a fixed point of light in the night sky |
| `starlit` | language | quality | dark faintly lit by stars |
| `stone` | terrain | substance | rock |
| `sun` | astronomy | celestial | the sun |
| `taiga` | climate | terrain | a biome class |
| `temperate-forest` | climate | terrain | a biome class |
| `temperate-grassland` | climate | terrain | a biome class |
| `temperate-rainforest` | climate | terrain | a biome class |
| `tree` | language | living | a woody plant |
| `tropical-rainforest` | climate | terrain | a biome class |
| `tropical-seasonal-forest` | climate | terrain | a biome class |
| `tundra` | climate | terrain | a biome class |
| `two` | language | quality | the cardinal number 2 |
| `upwelling` | climate | terrain | a biome class |
| `water` | language | substance | the drinkable liquid |
| `wind` | language | substance | moving air |
| `yellow` | language | quality | the color term for yellow |
