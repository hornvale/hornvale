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
| `astronomy/moon-age` | per-moon age draw (impact: coeval jitter under the planet's age; capture: an independent fraction of it) |

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
| `language/<species>/grammar/constituent-order` | the tongue's drawn constituent order for predication (SOV/SVO dominant, per authored typological weights) |
| `language/<species>/grammar/copula` | whether nominal predication carries an overt copula, and (when it does) the copula's one-syllable drawn form from the tongue's own phonology |
| `language/<species>/grammar/articles` | whether the tongue has articles (floor: drawn but surfaces no article lexeme until the morphology campaign) |
| `language/<species>/grammar/depth/evidential` | C7's depth vector: how deeply evidentiality grammaticalizes (None/Particle/Affix, weighted [60,25,15]) |
| `language/<species>/grammar/depth/noun-class` | C7's depth vector: how deeply noun class grammaticalizes (None/Particle/Affix, weighted [55,15,30]) |
| `language/<species>/grammar/class-position` | C7: which side of the noun the class marker binds when noun-class depth is Particle/Affix (prefix 40 / suffix 60) |
| `language/<species>/grammar/depth/number` | The Residue: the species' drawn Number grammaticalization depth (None/Particle/Affix), independent of evidentiality/noun-class |
| `language/<species>/grammar/depth/tense` | The Residue: the species' drawn Tense grammaticalization depth (None/Particle/Affix) |
| `language/<species>/grammar/number-position` | The Residue: which side of the marked word the Number affix binds |
| `language/<species>/grammar/tense-position` | The Residue: which side of the marked word the Tense affix binds |
| `language/family/<family>/morph/evidential/<value>` | C7: the family's one-syllable evidential-marker proto-form for <value> (witnessed/taught/inferred), drawn once per family and evolved per daughter via its own cascade — the cognate law |
| `language/family/<family>/morph/class/<value>` | C7: the family's one-syllable noun-class-marker proto-form for <value> (animate/inanimate), drawn once per family and evolved per daughter via its own cascade — the cognate law |
| `language/family/<family>/morph/number/plural` | The Residue: the family's Plural affix proto-form, shared by every daughter (family-cognate law) |
| `language/family/<family>/morph/tense/past` | The Residue: the family's Past-tense affix proto-form, shared by every daughter |
| `language/<species>/grammar/numeracy-rung` | The species' drawn numeral-system rung (Subitizing/FullCounting/Decimals) — how far counting words go past the universal subitizing floor |
| `language/<species>/schema/<domain>/<fact-shape>` | C5's causal-schema draw (render-time, `schemas::select_schema`): the β-sharpened pick among the fact-shape's admitted schemas for one culture's account of one (source-domain, fact-shape) pair — `<domain>` and `<fact-shape>` are the salt legs (e.g. `sky`/`cyclic-event` for the day) |
| `language/<species>/lexeme/<fact-key>` | C5's lexeme draw (render-time, `schemas::select_lexeme`): the uniform pick among a fired schema's gate-surviving verb candidates for one explained fact — `<fact-key>` salts by the ground fact's predicate (e.g. `day-length-std`) |
| `language/<species>/doctrine-schema/<domain>/<fact-shape>` | C6's institutional causal-schema draw (The Doctrine, render-time, `schemas::select_schema` again): the β-sharpened pick among the fact-shape's admitted schemas for the SAME culture's doctrine account (folk prior × the authored mediation column, before β) — a sibling stream to `schema/<domain>/<fact-shape>` above, never a shared draw, so the folk and doctrine schema picks can diverge independently |
| `language/<species>/doctrine-lexeme/<fact-key>` | C6's institutional lexeme draw (The Doctrine, render-time, `schemas::select_lexeme` again): the uniform pick among a fired doctrine schema's gate-surviving verb candidates for one explained fact — a sibling stream to `lexeme/<fact-key>` above, salted the same way (the ground fact's predicate) |
| `language/<species>/schema/sky/<shape>/<predicate>` | The Consonance: schema selection for a fact sharing FactShape::CyclicEvent with another predicate (moon-period-ratio vs day-length-std) — the extra predicate leg keeps their streams distinct |

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
| `terrain/microcontinents` | fixed candidate count, then per candidate position/radius/age |
| `terrain/terranes` | terrane count, then per terrane host-craton index/bearing/size/age |
| `terrain/arc-gate` | along-strike island-arc gating noise (hash-noise only; no stream draws) |
| `terrain/relief` | fBm relief-detail noise (hash-noise only; no stream draws) |
| `terrain/rift` | ONE spreading-rate draw; per-seam geometry via hash sub-derivations (seam-{a}-{b}); no other sequential draws |

### hornvale-vessel

| Label | Meaning |
|---|---|
| `vessel/agent` | minted agent id draw |
| `vessel/walk` | walker-battery deterministic walk |

### hornvale-kernel (internal)

| Label | Meaning |
|---|---|
| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |
