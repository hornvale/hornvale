# The Idea Registry

**This is the retrieval surface for Hornvale's speculative ideas — one line
each, scannable and greppable.** The [`frontier.md`](frontier.md) essays hold
the *argument*; this holds the *index*. Consult this table **before proposing
or reopening any idea** — an entry marked `rejected` or `ratified` is a
settled question, and reopening one needs new information, not a fresh
opinion. Then read the linked essay to understand it.

Scope: this registry tracks the **vision/idea pipeline** — the speculative
directions that flow frontier → spec → build. Settled *engineering
constraints* (no `HashMap`, no wall-clock, typed quantities, …) live in the
[decision log](../decisions/) and are not duplicated here; where a vision
idea *became* a decision, its row cross-links the decision instead of
restating it.

## How to read a row

- **ID** — a stable category-prefixed handle (`MAP-7`, `BIO-4`, …). IDs are
  permanent; a superseded idea keeps its ID and flips its status. New ideas
  take the next free number in their category; never renumber.
- **Status** — where the idea sits in the pipeline:
  - `shipped` — built and merged into the world.
  - `ratified (NNNN)` — settled as a decision; the number links the record.
  - `spec'd` — draining into a binding spec; **Where** points at the spec,
    not the essay.
  - `elaborated` — has a full essay in `frontier.md`; awaiting a spec.
  - `raw` — captured but not yet elaborated; a stub, not an argument.
  - `rejected` — considered and set aside; the row says why. **This is the
    anti-relitigation payload — the registry's most important status.**
- **Conf** — the essay's confidence tag, condensed (high / med / low /
  taste-gated), or `—` for settled/process rows.
- **Where** — the essay or record that elaborates it.

When an idea drains into a spec, flip its status and point **Where** at the
spec: the elaboration relocates, the breadcrumb stays, nothing is lost.

---

## The frontier map — the numbered directions

| ID    | Idea                                                                                                                                            | Status          | Conf                 | Where                                                         |
| ----- | ----------------------------------------------------------------------------------------------------------------------------------------------- | --------------- | -------------------- | ------------------------------------------------------------- |
| MAP-1 | The Laboratory — seed/pin sweeps, extracted metrics, drift-checked distributions                                                                | shipped         | —                    | Campaign L0; [frontier §map](frontier.md#the-frontier-map)    |
| MAP-2 | The epistemic layer — player/scholar knowledge as a provenance-tagged second ledger; tier-0 = almanac from a vantage point                      | elaborated      | med (fun open)       | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-3 | Deep language — per-culture proto-languages + deterministic sound-change over historical time; names as fossils                                 | elaborated      | high (mechanics)     | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-4 | Myth phylogenetics — proto-myths propagated with noise, reconstructed by comparative method, scored vs. the true tree                           | elaborated      | med (research)       | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-5 | Models author, dice roll — LMs as offline authoring amplifiers; no runtime model in the sim core                                                | ratified (0009) | —                    | [decision 0009](../decisions/0009-models-author-dice-roll.md) |
| MAP-6 | Paleoclimate as the first history — Milankovitch glacial cycling from orbital elements already held                                             | elaborated      | high (formula)       | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-7 | Population as a field, settlements as condensations — carrying-capacity prior, era-ticked genesis history to the ledger                         | elaborated      | high (calib)         | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-8 | Writing as a culture acquiring its own ledger — oral = phenomena, literate = freezing phenomena into facts; borrowed scripts as contact fossils | elaborated      | high (mechanics)     | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-9 | Contact and exchange — the interaction layer; contact-zone regimes derived from overlap × asymmetry × psychology                                | elaborated      | high (trade formula) | [frontier §map](frontier.md#the-frontier-map)                 |
| MAP-10 | The underworld — the vertical dimension of terrain: cave mouths and karst as cell features first, then a subterranean settlement layer; the deep-dweller venue-vocabulary question the kobolds already pose | raw | med | [frontier §underworld](frontier.md#the-underworld--the-vertical-dimension) |

### Ideas under the interaction layer (MAP-9)

| ID     | Idea                                                                                                                                                                             | Status     | Conf             | Where                                                 |
| ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ---------------- | ----------------------------------------------------- |
| MAP-9a | Slavery from land–labour–coercion (Nieboer–Domar); the structure ladder's slave rung resolved across a contact zone                                                              | elaborated | med              | [frontier §map, item 9](frontier.md#the-frontier-map) |
| MAP-9b | Persecution/genocide concentrate at aligned cleavages (Turchin meta-ethnic frontiers)                                                                                            | elaborated | med              | [frontier §map, item 9](frontier.md#the-frontier-map) |
| MAP-9c | Menageries — a caged captive as prestige display; observed differently by each watching species                                                                                  | elaborated | med              | [frontier §map, item 9](frontier.md#the-frontier-map) |
| MAP-9d | Counterweights as dampers not guarantees — trade (Ricardo), cross-cutting cleavages, religion-as-in-group-tech, superordinate threats, war exhaustion                            | elaborated | high→med         | [frontier §map, item 9](frontier.md#the-frontier-map) |
| MAP-9e | Abolition is emergent — the forces that build exploitation retire it; never a scripted redemption                                                                                | elaborated | med              | [frontier §map, item 9](frontier.md#the-frontier-map) |
| LANG-1 | Language demographics — pidgin/creole as contact archaeology, L2-simplification law, Fishman shift, centrality-based lingua francas, hardware-filtered cross-species acquisition | elaborated | high (mechanics) | [frontier §map, item 3](frontier.md#the-frontier-map) |

---

## The unification thesis

| ID    | Idea                                                                                                                             | Status     | Conf       | Where                                                       |
| ----- | -------------------------------------------------------------------------------------------------------------------------------- | ---------- | ---------- | ----------------------------------------------------------- |
| UNI-1 | Science, magic, theology, roguelike ID are one mechanic — inference over a hidden seeded ruleset                                 | elaborated | — (thesis) | [frontier §unification](frontier.md#the-unification-thesis) |
| UNI-2 | Magic as a second physics — pins promoted from parameter to rule-system selection; comparative metaphysics as controlled science | elaborated | med        | [frontier §unification](frontier.md#the-unification-thesis) |
| UNI-3 | Interventionist/trickster deities as adversarial noise injected into a civilization's inference                                  | elaborated | med        | [frontier §unification](frontier.md#the-unification-thesis) |
| UNI-4 | The undead as a metaphysical-ruleset output — whether death is animate is a magic-physics selection (UNI-2), so skeleton/ghost/lich/vampire are one ruleset's taxonomy, observed per-species (EXP-3) and coupled to the corpse-as-ur-pollutant purity grid (BIO-5) and funerary religion; a phenomenon a culture infers over, never a bestiary | raw | med | [frontier §unification](frontier.md#the-unification-thesis) |
| UNI-5 | Authored transmission media (the "stones") — comms/energy/transport as independent latent graph-construction systems with exact seeded laws, decoupling a modern affordance from its earthly substrate (no thermodynamics/info-theory/tech-tree); realized = pinned world-potential × craft fraction (TECH-1/TECH-2), raw ore emits diagnostic phenomena so discovery is UNI-1 inference perception-gated by EXP-3; optional, tunable, a Lab apparatus (MAP-1/TOOL-4) for counterfactual social history (MAP-9/MAP-7/SOC-1) — UNI-2's controlled science made concrete | elaborated | med | [frontier §unification](frontier.md#the-unification-thesis) |

---

## The expressive-culture cluster

| ID     | Idea                                                                                                                                    | Status     | Conf        | Where                                                              |
| ------ | --------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ----------- | ------------------------------------------------------------------ |
| EXP-1  | One seeded grammar engine, four media (text / image / music / space) from a small per-culture vector                                    | elaborated | — (framing) | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-2  | Music and tuning — the Pythagorean comma as a necessary hidden ruleset; tuning stance as theology and belief-fact                       | elaborated | med         | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-3  | Species perception — photoreceptor profiles make salience = f(phenomenon, observer); different species, different skies and astronomies | shipped    | med         | [Campaign Y2-2 spec](../superpowers/specs/2026-07-07-campaign-y2-2-the-eyes-design.md) |
| EXP-3a | Colour lexicons (Berlin & Kay) as evidence of a species' vision; art palettes on species gamut; drawn scattering regime                 | raw        | med         | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-4  | Humor — benign-violation theory; jokes as the fastest norm-probes; a humourless species has an empty violation set                      | elaborated | taste-gated | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-5  | Drama — narrative as an error-correcting code tuned to a culture's transmission channel; lifespan sets the channel                      | elaborated | med         | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-6  | Architecture — layout as constraint satisfaction over body/environment/structure/threat; sacred placement derivable from theology       | elaborated | high        | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |
| EXP-7  | Phonology, naming grammars & register — feature-bearing phonemes, a per-species articulation envelope, generative naming, and the content→render prose seam; the text arm of EXP-1's four-media engine, one medium lit | spec'd     | high        | [Campaign Y2-3 spec](../superpowers/specs/2026-07-08-campaign-y2-3-the-tongues-design.md) |
| EXP-8 | Martial traditions as culture, not choreography — embodied combat practice fused with dance, ritual, and belief (capoeira, Okinawan te) arising downstream of disarmament under coercion (MAP-9a): a subjugated people denied weapons encodes fighting as art, and the payload is the tradition and its significance, not the moves; the body is BIO-1, the transmission is mentorship and register | raw | med | [frontier §expressive](frontier.md#the-expressive-culture-cluster) |

---

## The species-psychology substrate

| ID    | Idea                                                                                                                                                                                                           | Status     | Conf                     | Where                                                                                               |
| ----- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ------------------------ | --------------------------------------------------------------------------------------------------- |
| PSY-1 | Culture as scaffolding around psychology — a bounded per-species vector (threat response, deliberation latency, sociality mode, in-group radius, time horizon, status basis) upstream of the culture campaigns | elaborated | high value / weak verify | [frontier §psychology](frontier.md#the-species-psychology-substrate--the-layer-beneath-the-cluster) |
| PSY-2 | De-privilege the goblin baseline — identity-vector-at-goblin and unqualified legacy stream labels are a measurement frame, not a design commitment; fix via epoch-suffixed label migration (ADR 0006) + an abstract reference baseline no species need occupy | raw        | high                     | [Campaign Y2-2 spec §11](../superpowers/specs/2026-07-07-campaign-y2-2-the-eyes-design.md) |
| PSY-3 | No default-civilization attractor — the culture stack must land as readily near a non-European analogue (Heian Japan, Ptolemaic Egypt, Classic Maya) as near medieval Europe; the anti-goal generalizes PSY-2 from the goblin baseline to the whole output space, guarded by derive-from-psychology×subsistence×environment rather than templates (SOC-6), eventually a Lab distribution over derived-culture space | raw | med | [frontier §psychology](frontier.md#the-species-psychology-substrate--the-layer-beneath-the-cluster) |

---

## Social structure and the inequality seam

| ID    | Idea                                                                                                                                                       | Status          | Conf        | Where                                                                                      |
| ----- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------- | ----------- | ------------------------------------------------------------------------------------------ |
| SOC-1 | Social structure as orthogonal axes (authority / legitimacy / descent / exchange) with psychology as a viability filter — replacing the single role ladder | elaborated      | high (dir.) | [frontier §social](frontier.md#social-structure-as-orthogonal-axes--cashing-the-substrate) |
| SOC-2 | Gender arrangements computed from subsistence × property × breeding system (Boserup/Alesina, Engels, Hrdy) — not a constant                                | elaborated      | med–high    | [frontier §social](frontier.md#social-structure-as-orthogonal-axes--cashing-the-substrate) |
| SOC-3 | Sex and gender not fixed at two — gender-role count is cultural; biological sex-systems widen once species diverge                                         | elaborated      | med         | [frontier §social](frontier.md#social-structure-as-orthogonal-axes--cashing-the-substrate) |
| SOC-4 | Intersectionality as a native data structure — standing is a vector; disadvantage compounds non-additively (Crenshaw)                                      | elaborated      | med         | [frontier §social](frontier.md#social-structure-as-orthogonal-axes--cashing-the-substrate) |
| SOC-5 | "Race" manufactured by the exploitation regime to legitimate extraction (Fields & Fields racecraft); recorded as truth by biased historiography            | elaborated      | med         | [frontier §social](frontier.md#social-structure-as-orthogonal-axes--cashing-the-substrate) |
| SOC-6 | No alignment axis; species are not moral types; "race" is an output never an input                                                                         | ratified (0021) | —           | [decision 0021](../decisions/0021-no-alignment-axis.md)                                    |

---

## The biological substrate

| ID    | Idea                                                                                                                                                                | Status     | Conf                    | Where                                                                                         |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- | ----------------------- | --------------------------------------------------------------------------------------------- |
| BIO-1 | The body as substrate; "the scaffolding reshapes the body" feedback thesis closing culture-around-psychology into a loop                                            | elaborated | high value / calib core | [frontier §biological](frontier.md#the-biological-substrate--body-reproduction-and-deep-time) |
| BIO-2 | Life-history on one fast–slow axis — body size, offspring number, lifespan, investment covary; drama couples to the slow end                                        | elaborated | high                    | [frontier §biological](frontier.md#the-biological-substrate--body-reproduction-and-deep-time) |
| BIO-3 | Dimorphism and mating system derived (Bateman–Trivers); sex-determination as a species parameter (haplodiploidy → eusociality via Hamilton)                         | elaborated | high                    | [frontier §biological](frontier.md#the-biological-substrate--body-reproduction-and-deep-time) |
| BIO-4 | Gene–culture coevolution — bidirectional era-tick; allele/trait fields under a breeder's equation; lactase/amylase/dark-vision, dated sweeps as calibration anchors | elaborated | high (calib)            | [frontier §biological](frontier.md#the-biological-substrate--body-reproduction-and-deep-time) |
| BIO-5 | Excretion, sanitation, purity — sanitation as a carrying-capacity term; Douglas pollution grid generating latrine/taboo/caste; nightsoil as fertility input         | elaborated | med                     | [frontier §biological](frontier.md#the-biological-substrate--body-reproduction-and-deep-time) |

---

## The biosphere — the living substrate

| ID    | Idea                                                                                                                                                                              | Status | Conf | Where                                                                 |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------ | ---- | --------------------------------------------------------------------- |
| BIO-6 | Flora & funga as fields — vegetation/fungal cover as typed functions over (space × climate) atop the biome prior; the terrestrial phenomena SEQ-4's observation path and the deep-dweller pantheon both want | raw    | med  | [frontier §biosphere](frontier.md#the-biosphere--the-living-substrate) |
| BIO-7 | Fauna on the life-history axis — animals/fish drawn through BIO-2's fast–slow frame; the SRD corpus already spans beasts; predator/prey/forage as fields feeding subsistence and hazard | raw    | med  | [frontier §biosphere](frontier.md#the-biosphere--the-living-substrate) |
| BIO-8 | Domestication & agriculture — the culture layer harnessing biosphere fields; staple crop per biome, the Boserup plough (SOC-2's input), nightsoil (BIO-5), famine as paleoclimate (MAP-6) × carrying capacity (MAP-7) | raw    | high | [frontier §biosphere](frontier.md#the-biosphere--the-living-substrate) |
| BIO-9 | Ethnobotany & the pharmacopoeia — psychoactive/sacred substances as a ritual phenomena source for religion, gated by metabolism (BIO-4) and the purity grid (BIO-5); existence is fields, activity is genetics, sacredness is the culture vector | raw    | med  | [frontier §biosphere](frontier.md#the-biosphere--the-living-substrate) |
| BIO-10 | Paralinguistic vocal signaling — mating/alarm calls and dimorphic voice as a system distinct from speech (a fauna-and-body vocalization layer, BIO-3/BIO-7), feeding music (EXP-2) and register; the vocal-loudness/resonance axis The Tongues bank-authors also lives here | raw    | med  | [frontier §biosphere](frontier.md#the-biosphere--the-living-substrate) |

---

## Technology

| ID     | Idea                                                                                                                                                                        | Status | Conf | Where                                                                       |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------ | ---- | --------------------------------------------------------------------------- |
| TECH-1 | Technology as capability thresholds, not a tech tree — a capability is crossed when biome resources × subsistence × surplus clear a bar; its consequence routes into demography and social structure, never a "tech level" scalar | raw    | high | [frontier §technology](frontier.md#technology-as-capability-thresholds)      |
| TECH-2 | The pyrotechnology ladder — pottery/ceramics gate storage→surplus→the culture rung ladder; kiln temperature gates smelting; metallurgy yields tools, weapons (MAP-9's war-cost damper), and coinage (SOC-1's exchange axis) | raw    | med  | [frontier §technology](frontier.md#technology-as-capability-thresholds)      |
| TECH-3 | Extraction & the underworld's ores — mining/refinement needs the vertical terrain layer (MAP-10) plus ore as a terrain field; refining is a carrying-capacity sink and a contact-zone prize (MAP-9) | raw    | med  | [frontier §technology](frontier.md#technology-as-capability-thresholds)      |
| TECH-4 | Electricity — natural electrical phenomena (lightning, bioelectricity) as a possible future field/phenomenon, and electrical power as a far-future capability threshold (TECH-1), an instance of the ladder like steam, not a node; the "Redstone" buildable-logic-automation reading is rejected as game-as-lens — a construction toy contradicts sim-first (the authored transmission-medium reading is a different idea, UNI-5) | raw | low | [frontier §technology](frontier.md#technology-as-capability-thresholds) |

---

## Tools, verbs, and the Lab

| ID     | Idea                                                                                                                                             | Status          | Conf         | Where                                                                       |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------ | --------------- | ------------ | --------------------------------------------------------------------------- |
| TOOL-1 | Provenance interrogation — the `explain` verb; replay a derivation in trace mode down to the tie-breaking draw                                   | elaborated      | high (small) | [frontier §explain](frontier.md#provenance-interrogation--the-explain-verb) |
| TOOL-2 | Player-facing ethnography — generated only from the player-knowledge ledger, with unreliable-informant provenance; the diff vs. truth is a score | elaborated      | med          | [frontier §explain](frontier.md#provenance-interrogation--the-explain-verb) |
| TOOL-3 | Lab: time as a study axis — metrics sampled along `WorldTime` within one world (population trajectories, founding/abandonment rates)             | raw             | high         | [frontier §map, item 1](frontier.md#the-frontier-map)                       |
| TOOL-4 | Lab: sensitivity studies — hold the seed, vary one pin, measure downstream distributions                                                         | raw             | high         | [frontier §map, item 1](frontier.md#the-frontier-map)                       |
| TOOL-5 | Lab: paired comparisons — tier-0 vs. generated provider on one seed; "refines, never contradicts" as a statistic                                 | raw             | high         | [frontier §map, item 1](frontier.md#the-frontier-map)                       |
| TOOL-6 | Gallery images as hand-rolled pure-std PNG (stored deflate + CRC32) so they render in-browser                                                    | ratified (0018) | —            | [decision 0018](../decisions/0018-gallery-images-are-hand-rolled-png.md)    |
| TOOL-7 | Lab: parallel study runner — std::thread pool (no new deps), results collected by seed index for byte-identity (sequential-equivalence test as the proof), plus a deterministic stderr progress line every N seeds; timings stay at shell/CI level (wall-clock ban) | raw             | high         | [frontier §map, item 1](frontier.md#the-frontier-map)                       |

---

## Sequencing notes

| ID    | Idea                                                                                                                                 | Status     | Conf | Where                                                                                              |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------ | ---------- | ---- | -------------------------------------------------------------------------------------------------- |
| SEQ-1 | Tidal lock is the cheapest alien-religion generator — a motionless sun forces a religion with no celestial cycles                    | elaborated | high | [frontier §year-1 sequencing](frontier.md#two-sequencing-notes-for-the-remaining-year-1-campaigns) |
| SEQ-2 | Hydrology is people-infrastructure, not land-refinement — rivers are the skeleton of settlement, trade, borders, toponymy            | elaborated | high | [frontier §year-1 sequencing](frontier.md#two-sequencing-notes-for-the-remaining-year-1-campaigns) |
| SEQ-3 | The deep-time stack DAG — 7 → 9 → {language demographics, genetic divergence}; racecraft downstream of subjugation; coevolution last | elaborated | —    | [frontier §deep-time stack](frontier.md#sequencing-the-deep-time-stack)                            |
| SEQ-4 | Phenomena-path fidelity — generated worlds observe through tier-0 UniformClimate, anchored at places[0] and genesis day 0; a future Firm-Ground-style campaign upgrades the observation path | raw        | high | [Campaign Y2-2 spec §11](../superpowers/specs/2026-07-07-campaign-y2-2-the-eyes-design.md) |

---

## Process and knowledge architecture

| ID     | Idea                                                                     | Status          | Conf | Where                                                                |
| ------ | ------------------------------------------------------------------------ | --------------- | ---- | -------------------------------------------------------------------- |
| PROC-1 | Campaigns drop the "Year" naming (sequence number + name, forward-only)  | ratified (0017) | —    | [decision 0017](../decisions/0017-campaigns-drop-year-naming.md)     |
| PROC-2 | Campaigns write a one-page retrospective at merge                        | ratified (0020) | —    | [decision 0020](../decisions/0020-campaigns-write-retrospectives.md) |
| PROC-3 | This idea registry — a thin retrieval layer over the frontier essays     | shipped         | —    | this file                                                            |
| PROC-4 | The docs map (`docs/README.md`) — the doc-of-docs and knowledge pipeline | shipped         | —    | [docs/README.md](../README.md)                                       |

---

## Rejected and foreclosed — do not reopen without new information

The registry's core job. Each row is a question that was asked and answered
`no`; a fresh opinion is not new information.

| ID    | Idea                                                                                                     | Status                     | Why                                                                                                                                                                                                                       | Where                                                         |
| ----- | -------------------------------------------------------------------------------------------------------- | -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| REJ-1 | Procedural macros for boilerplate (newtypes, predicate/label registration)                               | rejected → ratified (0019) | `syn`/`quote` breaks the serde-only allowlist; the candidates are save-format contracts whose value is being plain-text; `macro_rules!` is the ceiling                                                                    | [decision 0019](../decisions/0019-no-procedural-macros.md)    |
| REJ-2 | An alignment axis / `evil: bool` / morally-typed species                                                 | rejected → ratified (0021) | conduct is a derived situation; "race" is a manufactured output — an alignment field collapses the whole interaction system into a lookup table                                                                           | [decision 0021](../decisions/0021-no-alignment-axis.md)       |
| REJ-3 | Splitting the frontier into multiple topic files                                                         | rejected                   | the doc's value is its interconnections (the unification thesis is a section); topic files turn edges into rotting cross-file links, and no clean cut avoids the shared spine. Use this registry + drain-to-specs instead | this conversation; [frontier preamble](frontier.md)           |
| REJ-4 | YAML for config files                                                                                    | rejected → ratified (0012) | JSON is already a dependency; a second format is redundant surface                                                                                                                                                        | [decision 0012](../decisions/0012-config-is-json-not-yaml.md) |
| REJ-5 | New runtime dependencies (rand, chrono, clap, thiserror, a PNG/image crate, dimensional-analysis crates) | rejected → ratified (0004) | randomness comes from the kernel `Seed`/`Stream`; time is `WorldTime`; parsing is std-only; images/units are hand-rolled                                                                                                  | [decision 0004](../decisions/0004-no-new-dependencies.md)     |
| REJ-6 | Any ML model in the simulation core                                                                      | rejected → ratified (0009) | models author offline; the runtime is deterministic and seeded (models author, dice roll)                                                                                                                                 | [decision 0009](../decisions/0009-models-author-dice-roll.md) |
