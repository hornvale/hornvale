# Culture

**Questions it answers:** How is a community structured? Who stands where in
it?

**Tier 0 — the caste ladder.** Every settlement receives the same fixed
structure, lowest to highest: slave, fighter, cook, shaman, chief. Readers of
the vision book will recognize this immediately — it is the caste system of
the goblin village in *After the Plague II*, the sociology chapter's founding
memory, transplanted verbatim as the project's first social structure. The
cook's position in that ladder is not a joke; it is fidelity to the source.

**What it emits:** one caste fact per rung, attached to the village, in
order. The order is meaningful (lowest to highest) and preserved by the
ledger's commit ordering — the almanac renders the ladder as given.

**Tier 1 — subsistence and an emergent structure (Campaign 4b).** The fixed
ladder is gone; a settlement's society now grows from two small, exact
functions of what the land around it offers. **Subsistence** — farming,
herding, fishing, or foraging — is a lookup on a coarse `BiomeClass` (forest,
grassland, arid, cold, or barren — culture's own vocabulary; the composition
root maps every `climate::Biome` into it, so culture never imports climate)
crossed with one boolean, coastal access: forest and grassland farm
regardless of coast (a rich hinterland does not need the sea); arid, cold,
and barren settlements forage or herd inland but a coast rescues a marginal
one into fishing. This is deliberately an *exact* function — the Lab's
calibration (below) checks it row by row, the sociology sibling of climate's
band-count calibration. **Structure** is a short, ordered role list built
from three environmental pressures, each in `[0, 1]`: *surplus* (a biome's
fertility times local moisture), *population scale*, and *threat* (tectonic
unrest at the settlement's own cell). The subsistence worker (`farmer`,
`herder`, `fisher`, `forager`) and `chief` always appear; `warrior` appears
above a threat threshold, `artisan` and `slave` above surplus-and-scale
thresholds, `shaman` above a lower surplus threshold alone. A lean forager
camp on a cold, threat-free coast grows just `[forager, chief]`; a rich,
populous farm town grows the full `[slave, farmer, artisan, shaman, chief]`
— the same five-rung shape tier 0 gave every settlement unconditionally, now
earned rather than copied.

**What it emits.** One `subsistence` fact (functional, replacing nothing —
tier 0 never wrote one) and the same `has-caste` fact per rung tier 0 used,
now populated by `structure` instead of a constant array. The predicate
vocabulary is unchanged; only how the castes are chosen changed, so the
almanac and REPL read tier-1 societies through the exact same queries tier 0
answered.

**Two peoples, one function (Campaign Y2-1).** The role vocabulary no
longer belongs to goblins alone. `structure` takes a `PsychSummary` whose
`vocabulary` field supplies the words a species uses for each rung — goblin
keeps the tier-0 words (`farmer`/`herder`/`fisher`/`forager`, `warrior`,
`artisan`, `shaman`, `chief`); kobold substitutes its own (a fixed
`digger` worker rung regardless of subsistence mode, `warden`, `shaper`,
`keeper`, `elders`) — while the thresholds that decide *which* rungs exist
stay the one reviewed function this section describes, now also modulated
by two more psychology inputs: `threat_response` shifts the warrior
threshold, `time_horizon` shifts the artisan threshold, and a
knowledge-based status basis (rather than rank) suppresses the slave rung
outright regardless of surplus or scale. Every modulation is the identity
function at the goblin baseline, so this section's formulas and examples
are unchanged for goblin; kobold's knowledge-based, communal psychology
makes a slave rung structurally unreachable and caps every kobold ladder at
`elders` — never `chief` — a fact [Study 006](../laboratory/study-006.md)
confirms holds without exception across 10,000 worlds. See
[Species](./species.md) for the vector these dimensions come from and the
SRD-derived kobold values themselves.

**The model card.**

- **Drawn:** nothing — culture adds no labeled stream in 4b (`stream_labels()`
  stays empty); subsistence and structure are pure functions of the
  environmental summary the root assembles from terrain and climate.
- **Derived:** subsistence from `(BiomeClass, coastal)`; surplus from
  `fertility(BiomeClass) × moisture`; threat from the flagship cell's
  tectonic unrest; the ordered role list from subsistence, surplus,
  population, and threat.
- **Approximated (declared):** a rule-table sociology, not a simulated
  economy or a solved bargaining/stratification model — thresholds on
  surplus, scale, and threat, chosen to be legible rather than fit to any
  external data; only each species' flagship settlement runs tier-1 culture
  (every other placed settlement, of either species, stays without a
  committed society, an artifact of the composition root's current scope,
  not a claim that they have none); no
  inter-settlement politics, trade, or cultural diffusion; the structure is
  **static** — it is computed once at genesis and never revises as
  population or environment would later change.

The exit-demo cascade: the same seed 7 land, under a spinning sky, places a
farming town of 403 in temperate forest that grows a lean three-rung
`[farmer, shaman, chief]`. Pin the sky to tidally locked and the *same*
seed's flagship relocates entirely: a farming town of 348, also temperate
forest, stratified into the full `[slave, farmer, artisan, shaman, chief]`.
Nothing about culture changed — only the sky did, and though both flagships
now farm (forest farms regardless of coast), the structure still
reorganizes with the move: the locked site's environmental pressures earn
the slave and artisan rungs the spinning site does not, the cascade the
astronomy-to-biome chain promised back in Campaign 3c still reaching all
the way to who a settlement's chief rules over. (Campaign Y2-0's placement
fix re-drew this pair — the pre-fix version, a fishing hamlet against a
farm town, is preserved in the 4b chronicle.) Laboratory: [Study 003, the
Census of Peoples](../laboratory/study-003.md). Chronicle: [4b, Emergent
Society](../chronicle/campaign-4b.md).

**The tier ladder ahead:** structure that revises as a settlement's
environment or population changes rather than freezing at genesis; the
comparative questions two peoples now make askable but that religion does
not yet ask (the hobgoblin-domination questions from the vision book's
comparative-religion chapter — paternalism? hegemony? enslavement? —
waiting on religion itself going two-species); inter-settlement politics
and trade; and the trace protocol's *cause-blind fields* earning their
keep here — a `misery` field to which a caste system contributes, readable
by religion without religion knowing castes exist.
