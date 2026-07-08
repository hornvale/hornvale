# Campaign 1 (Year 2): The Peoples — Design

**Date:** 2026-07-07
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-07-year-2-metaplan-design.md` (§5 binds this campaign; Constitution §2 governs)
**Provenance:** The first spine campaign of Year 2, following Campaign Y2-0
(Firm Ground, merged aaa527d). Y2-0 hardened placement and re-baselined every
census; this campaign gives the world a second people and the substrate that
makes peoples differ. Downstream campaigns consume what it ships: The Eyes
(perception vector, observer-dependent salience), The Tongues (language
replaces the vocabulary stopgaps), The Meeting (comparative studies).

---

## 1. Goal

Give the world **peoples, plural**: a species substrate (`domains/species`)
holding authored species definitions; goblins parameterized as the baseline
species; **kobolds** — nocturnal deep-dwellers — as the second; two-species
worlds by default; placement and social structure diverging between the two
for legible, recountable reasons. The deliverable is one almanac showing the
goblin village and the kobold warren on the same globe, visibly different in
where they sit and how they organize, with `why` recounting each difference
to a named psychology dimension.

Explicitly **later** campaigns: perception and observer-dependent salience
(The Eyes — religion stays goblin-flagship-only here), generated language
(The Tongues — this campaign's vocabulary and syllable stopgaps are designed
to be deleted), per-species drawn variation, per-individual variation,
inter-settlement and inter-species politics.

## 2. Design principles

1. **Species are data; the social grammar is code** (the ADR 0011 rhyme:
   studies are data, metrics are code). A species is a point in a closed
   parameter space. Cultural variety comes from growing the generative
   grammar in reviewed Rust — an axis per campaign — never from per-species
   rule tables or species-as-code. This campaign's axis is stratification;
   governance, kinship, specialization, norms, and sacred/spatial order are
   later axes. Rejected alternatives, recorded: per-species `(role,
   condition)` tables in data (a DSL, an interpreter, a save-format contract,
   and the ontology trap with a welcome mat); species as trait impls (every
   future species a code change; the Lab cannot sweep vector space).
2. **Goblin is the baseline; every modulation is identity at the baseline.**
   Scalar dimensions are bounded `f64` with 0.5 ≡ goblin; enum dimensions
   default to the goblin variant. Every downstream formula reproduces today's
   behavior by construction at the baseline vector, making the byte-identity
   contract (§8) mechanical rather than tuned.
3. **D&D 5E is the authoring corpus** (the ADR 0009 pattern — models author,
   dice roll — with fifty years of playtested lore as the model). Authored
   vectors are derived by reading a race's SRD lore and statistics through
   our dimensions; each value carries a one-line lore justification in the
   species model card; where the corpus contradicts itself we follow one
   coherent vision and record which. The legal line: SRD 5.1 is CC-BY-4.0
   and kobolds are in it; we derive parameters with attribution and never
   copy lore prose. The corpus's lasting value: one measurement frame
   spanning peoples and beasts (kobolds, humans, water buffalo,
   tyrannosaurs), which later perception/physiology/ecology campaigns will
   mine — noted in the frontier.
4. **Ontology-trap posture** (the frontier's twice-repeated warning lands
   here): the psychology vector is closed at six dimensions. No extensible
   traits, no inheritance, no per-individual variation. Widening the vector
   requires its own campaign. The perception vector (activity cycle, night
   vision, sky attention) is **banked, not built** — kobold nocturnality is
   recorded in the species model card as authored data awaiting The Eyes.
5. **Layering holds.** `domains/species` depends on `hornvale-kernel` and
   nothing else. Culture and settlement never import species; the composition
   root (`windows/worldgen`) reads the registry and populates per-consumer
   summary structs (the `SocietySummary` pattern). Adding kobolds edits no
   existing domain's interface.

## 3. The species crate

`domains/species` (crate `hornvale-species`, kernel-only):

- **`SpeciesDef { name, psych: PsychVector, vocabulary: RoleVocabulary,
  syllables: Vec<&'static str> }`** — all authored; this campaign draws
  nothing in the species domain (model card: every dimension *authored*).
- **Registry:** `BTreeMap<String, SpeciesDef>` — ordered, deterministic;
  goblin sorts before kobold, so registry order is goblin-first.
- **`PsychVector`**, closed at six:

  | Dimension                    | Type                                 | Goblin (baseline) | Kobold    | 5E derivation (kobold)                                                         |
  | ---------------------------- | ------------------------------------ | ----------------- | --------- | ------------------------------------------------------------------------------ |
  | threat response (flee↔stand) | `f64` [0,1]                          | 0.5               | 0.8       | cowardly in the field but entrenched at home — traps, tunnels, the warren held |
  | deliberation latency         | `f64` [0,1]                          | 0.5               | 0.7       | communal decisions, slow consensus                                             |
  | in-group radius              | `f64` [0,1]                          | 0.5               | 0.2       | insular warrens, tight pack loyalty                                            |
  | time horizon                 | `f64` [0,1]                          | 0.5               | 0.8       | generational works: tunnel complexes, egg-tending                              |
  | sociality mode               | enum `{Hierarchic, Communal}`        | Hierarchic        | Communal  | pack tactics, communal egg-tending                                             |
  | status basis                 | enum `{Rank, Knowledge, Generosity}` | Rank              | Knowledge | trap-cunning and craft esteemed over dominance                                 |

  Kobold scalar values are authored data — Nathan's to retune; the design
  requires only meaningful contrast and the identity property at goblin.
- **`RoleVocabulary`** — pre-language stopgap, English content: worker
  ("digger"), warrior ("warden"), artisan ("shaper"), shaman ("keeper"),
  top rung ("elders", collective) and settlement noun ("warren") for
  kobolds; goblin vocabulary unchanged ("farmer"/"warrior"/"artisan"/
  "shaman"/"chief"/"village"). Deleted by The Tongues.
- **`syllables`** — a small kobold-flavored placeholder pool (authored,
  ~10 syllables, distinct mouth-feel from the goblin pool) so warren names
  don't sound goblin. Deleted by The Tongues.

## 4. Placement

Suitability keeps its shape (`w_f·freshwater + w_c·coast + w_t·temperance −
w_h·hostility`, clamped [0,1]); the weights become per-species derivations,
each identity at baseline and monotone in one dimension:

| Weight           | Goblin | Derivation                                                                   | Kobold |
| ---------------- | ------ | ---------------------------------------------------------------------------- | ------ |
| `w_f` freshwater | 0.45   | `0.45 · (0.5 + time_horizon)`                                                | 0.585  |
| `w_c` coast      | 0.20   | `0.20 · (2 · in_group_radius)`                                               | 0.08   |
| `w_t` temperance | 0.35   | shared — physiological, not psychological; revisited when species get bodies | 0.35   |
| `w_h` hostility  | 0.50   | `0.50 · (1.5 − threat_response)`                                             | 0.35   |

**Joint greedy across species:** score every (habitable cell × species)
pair with that species' weights; sort once (score descending, ties by cell
id then species name); place greedily with the existing 12° minimum
separation enforced **across** species; each species' highest-placed site is
its flagship. The two peoples compete for prime land and visibly avoid each
other — the cheapest honest interaction, no politics machinery. Consequence,
accepted: the goblin scatter in a two-species world differs from a
goblin-only world (kobolds take cells goblins would have had) — real,
recountable, and the `--species goblin` pin preserves the identity contract.

Deliberately *not authored*: a kobold mountain pull. The threat-response
weight does make kobolds mechanically more tolerant of hostile (unrest/
aridity) cells — whatever upland affinity falls out of that is emergent and
census-measurable, not a designed outcome. Authored habitat affinity is
physiology and waits for a body-shaped vector.

## 5. Culture: the modulated rule table

`structure()` gains a `PsychSummary`; every modulation identity at baseline:

- **Slave rung:** requires `status_basis == Rank` in addition to the
  existing gates (surplus > 0.6, population > 300). Communal-Knowledge
  kobolds structurally never enslave.
- **Warrior threshold:** `0.4 · (1.5 − threat_response)` — kobolds cross at
  0.28; their vocabulary renders the rung "warden" (the warren's trap-and-
  tunnel defense, not field war).
- **Artisan gates:** surplus and population thresholds scaled by
  `(1.5 − time_horizon)` — long-horizon kobolds differentiate "shapers"
  earlier.
- **Top rung:** `Hierarchic → "chief"` (singular), `Communal → "elders"`
  (collective) — structural, rendered distinctly in the almanac.
- **Shaman rung:** gate unchanged; kobold vocabulary "keeper". Feeds
  `SocietySummary.has_priesthood` unchanged (the C2 seam).

Culture's rule table remains code; the deliberation-latency dimension is
consumed by no rule this campaign (it exists for contrast and for The Eyes'
salience work) — declared idle in the model card rather than force-fitted.

## 6. Composition root and rendering

`windows/worldgen` reads the species registry, derives per-species placement
weights, runs the joint greedy, and populates `PsychSummary` (culture) per
species-flagship. Culture tier 1 runs per species-flagship; **religion runs
on the goblin flagship only** (unchanged inputs; pantheon divergence is The
Eyes'). The almanac gains a kobold section rendered with the species
vocabulary ("The kobold warren of X, population N… its roles, lowest to
highest: digger, …, elders"). REPL `settlements` lists both peoples;
`why <settlement>` recounts placement and structure through the species
facts.

## 7. Facts, predicates, pins

- Each species mints a world-scoped **species entity** carrying its authored
  vector as committed facts (~8 new predicates: the six dimensions plus
  species-name bookkeeping; exact names settle at the registry review;
  value-kind enforced per ADR 0010, no subject-kind constraints).
- Every settlement gains a **`peopled-by`** fact naming its species — the
  provenance link that lets `why` recount settlement → species → vector.
- **`--species <name>` pin** restricts the placed set; unknown names fail
  loudly listing the registry's known species. Pin-isolation tested like sky
  and terrain pins.
- **Stream labels:** goblins keep every legacy label untouched. Kobolds draw
  names and populations under new species-qualified labels
  (`settlement/kobold/name`, `settlement/kobold/population`) — permanent
  additions to the manifest; no renames, no reordering of existing
  consumption (ADR 0006).

## 8. The byte-identity contract (keystone test)

`hornvale new --seed 42 --species goblin` reproduces pre-C1 main as a
**superset**: the almanac is byte-identical, and the ledger restricted to
pre-C1 predicates is identical — new facts appear only under the new species
predicates (plus the one `settlement-pin` fact recording the `--species` pin
itself, which the comparison also sets aside since the pre-C1 world was
unpinned). Asserted in CI, not in prose. This holds by construction: goblin
≡ the baseline vector (all modulations identity), goblin draws keep legacy
labels, species entities are minted after settlements (settlement entity ids
unchanged) and carry no facts under pre-existing predicates. Universal
`peopled-by` facts (§7) are thereby compatible with the contract: symmetric
provenance for both peoples, additive-only divergence from the fixture. Mechanism: before the first code
change, the plan commits the current seed-42 almanac and world JSON as test
fixtures (`tests/fixtures/pre-species-seed-42.*`); a dedicated CI test
generates `--species goblin` seed 42 and asserts byte-equality against them.

**Re-baseline, once, Firm-Ground-style:** default worlds now carry two
peoples, so committed artifacts and the 10k censuses shift exactly once
more, after all code changes land, as the campaign's final act before the
book close.

## 9. The Lab

Preregistered before any census runs (ADR 0016):

1. **Kobold flagships are less coastal than goblin flagships** (directional;
   the exact rates are pinned as calibration rows after measurement — the
   goblin rate should match Y2-0's 99.3% in goblin-only worlds).
2. **Slave rung ⇔ (`status_basis = Rank` ∧ surplus > 0.6 ∧ population >
   300)** — the sixth exact calibration, asserted row-by-row over the
   500-seed drift study from independent metric columns.
3. **Kobold structures never contain "slave" and always top out at
   "elders"; goblin structures top out at "chief".**

New metrics: per-species settlement count and per-species flagship
coastal/biome/structure-size (existing flagship metrics stay goblin-pointed
for continuity; kobold twins added). Census of Peoples re-run at 10k at the
re-baseline; Study 006 records the two-peoples baseline.

## 10. The book (opens the campaign)

Per book-driven development, the campaign opens with **`book/src/domains/
species.md`**: the why, the vector and its closed-ness, species-are-data /
grammar-is-code, the 5E-as-authoring-corpus method with the CC-BY line and
the model card (every dimension authored, each kobold value with its lore
derivation, deliberation latency declared idle, nocturnality banked for The
Eyes). At close: culture and settlement chapters updated to multi-species,
Study 006, chronicle, freshness sweep (the "goblin-only, declared" notes
across the book all fall), concept-registry review for the new predicates.

## 11. Success criteria

1. Seed 42's almanac shows both peoples: the goblin village (farming,
   chief-topped) and a kobold warren (inland trend, digger-and-elders, no
   slave rung), each recountable by `why` to named vector dimensions.
2. The byte-identity contract (§8) passes in CI.
3. All three preregistered hypotheses (§9) confirmed and pinned; the full
   gate and artifact drift check green.
4. The book carries the species chapter, updated culture/settlement
   chapters, Study 006, and the chronicle — comprehension-gated as always.

## 12. Explicitly deferred

Perception vector and observer-dependent salience (The Eyes); generated
language, real names, register (The Tongues — deletes §3's vocabulary and
syllable stopgaps); comparative religion (kobold pantheons); per-species
drawn variation and per-individual variation; species physiology (bodies,
habitat affinity, the temperance weight's species-dependence); inter-species
politics, trade, conflict; more than two species (the substrate supports N;
the campaign ships two).
