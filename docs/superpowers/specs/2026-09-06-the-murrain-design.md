# The Murrain — design

**Campaign:** The Murrain (follows The Lot, decisions 0796–0798).

**Status:** implemented; canonical stage gate passed, campaign close artifacts
are being prepared for the merge queue.
**Branch:** `campaign/the-murrain`. **Decision block:** 0856–0865.
**Ledger:** `docs/superpowers/ledgers/2026-09-06-the-murrain.md`.
**Status:** at G3 (spec review).

A *murrain* is a pestilence named from its host. This campaign gives a
Hornvale world its first disease, and it does so under one ontology: **a
pathogen is a species whose niche is another species.** It reuses the
species domain's body-and-niche machinery for the pathogen's presence, the
history bake's connection graph and relocation paths for its spread, and
the Lot's hazard for its consequence — the first named cause of death a
drawn life can carry. Its headline finding is preregistered and derivable
before a line of the producer exists: at the population scale these worlds
reach, a crowd disease cannot persist, so a Hornvale world is a world of the
flux and the consumption and never of the pox.

## 1. Task 0 — the population a pathogen would have to persist in

Nothing below was designed before this was measured. The brief framed the
finding on community size ("communities peak under 90"). The ideonomy pass
(ledger, the organon) moved the unit: a pathogen persists or fades in the
**connected metapopulation** — every community it can reach over the
transport graph before it burns out — so the number the finding rests on is
the summed population of one connected component of the bake's own era
graph, not one community's peak. `windows/worldgen/tests/suite/
murrain_probe.rs` reads that number over the nine cross-seed worlds and all
25 climate eras, through a public accessor added for it
(`hornvale_worldgen::bake_era_graphs`, ledger #7), proxying live population
as the sum of `peak_population` over the occupations alive in the era at the
component's sites — an **upper bound**, since a community spends part of
every tenure below its peak.

```
  seed  occs   peak  p50   largest connected metapopulation      (era, comp) pairs
              (max)        EVER (era, occs)     NOW (era 23)      >=1e3  >=1e4
  ----  ----  -----  ---  -------------------  ----------------  -----  -----
     1  1238    85    13   5544 (20, 211)       5148 (227)          45      0
     2  1847    88    13   9992 (22, 382)       9108 (352)          50      0
     3   712    88    18   9807 (22, 304)       9653 (295)          24      0
     7   656    83    15   7819 (21, 303)       7328 (256)          24      0
    13  1061    86    12   5370 (22, 223)       5047 (201)          37      0
    42  1212    83    13   7056 (21, 277)       6987 (274)          43      0
   100   159    36    12   1460 ( 1,  89)       1061 ( 60)          23      0
   256  1470    86    13   3510 (22, 102)       3417 ( 94)          52      0
   777  1049    81    14   6094 (22, 215)       5509 (198)          43      0
```

Contact structure at the present era (occupied sites, and how many of a
site's traversable neighbours are themselves occupied): mean occupied-degree
3.46–3.94 on the eight growing seeds, 0.97 on seed 100; the maximum
anywhere is 9; 15–29 occupied sites per world have no occupied neighbour at
all.

Three findings, and the first is the one the campaign rests on. **No
connected population in any of nine worlds, in any of 25 eras, reaches ten
thousand.** The largest ever is 9,992 (seed 2, era 22), an upper bound. The
critical community size below which an acute, immunising crowd disease
cannot persist sits in the hundreds of thousands — Bartlett's measles
threshold is ~250,000–500,000, and smallpox, pertussis and mumps are of the
same order — so these worlds are a factor of 25 or more short of the
smallest such bar. The finding is not "under 90 per community"; it is
"under 10,000 per connected world", which is the sharper claim and the one
that decides persistence.

Second, the bar that DOES fall inside these worlds is the chronic one. A
pathogen with a two-year infectious period and a modest reproduction number
— the shape of tuberculosis — has a critical size near four thousand under
the same formula (§4.2), and seven of nine present-day largest components
sit above it by the upper bound (seeds 1, 2, 3, 7, 13, 42 and 777, with
seeds 100 and 256 below). So a Hornvale world is not merely "dysentery, never
smallpox": it is a world in which consumption is endemic in the connected
heartland and absent from the isolated fringe, and which worlds are which is
a derived fact.

Third, the community-size claim the brief made is also true: no community
in nine worlds ever peaks at 90 or above (maxima 81–88; seed 100's is 36),
and the modal community is 12–18 at its peak. The reason it is not the
load-bearing number is that a pathogen does not read one community; it
reads whatever the graph connects.

## 2. What The Murrain is

### 2.1 Two regimes, two ontologies

Disease in a pre-modern world has two shapes, and the registry had already
split them before this campaign existed. `MAP-61` assigns *diffuse*
coupling — "trade / culture / disease" — to an order-independent derived
read over the sparse graph, and *discrete* coupling — "raid / founding /
plague" — to events propagating in the bake. The Murrain builds exactly
that seam (ledger #2):

- **The endemic burden** is *derived*: a read over committed facts and the
  authored catalogue that says which pathogens are present at a site in a
  year and how much of the background mortality each accounts for. It draws
  nothing, commits nothing, and does not move the bake's populations. What
  the inhabitants would perceive of it is a condition of the place — low
  wet ground, a crowded house, bad air — which is what the pre-germ world
  called miasma, and which is exactly the shape a field over place facts
  has.
- **The epidemic** is *baked*: a spillover from a reservoir, a wave over the
  connection graph, deaths, and sometimes a community's end. It draws from
  the bake's own stream, commits dated events, and moves every population
  it touches — which is what makes `CauseOfEnd::Plague` reachable for the
  first time, and what makes a plague-struck community raidable in the
  same epoch.

The reason the endemic half is not baked is arithmetic, not economy. The
bake's `GROWTH_RATE` is a net rate — births minus deaths — and the Lot's
Siler `a2` term is that same background mortality on the individual side.
An explicit endemic drag in the bake would count the same deaths twice and
retune every population-bearing census column to correct a double-count. An
epidemic's excess deaths are the one mortality outside the net rate, so they
are the one that belongs inside the loop.

### 2.2 The ontology, and what it buys

A pathogen is authored as a species row: a `KindId`, a host range over the
peoples, a transmission class, a `ConditionNiche`, and a virulence pair. Two
things follow from making it a species rather than a frequency table.

Its **presence potential** at a vertex in an era is its condition-niche
fit, computed by `tolerance_liebig` over the same era-adjusted substrate the
bake already builds for every people's capacity map — so "where the marsh
fever lives" and "where the pest's reservoir lives" are answered by the one
function that answers "where goblins can live", with no new machinery. A
vector-borne disease is warm and wet and low because its niche says so; a
sylvatic reservoir is dry grassland because its niche says so.

Its **spread** rides what the bake already has. A raid's targets, a
daughter's site and a migrant's refuge are all chosen from
`traversable_neighbors` on the era's graph; an epidemic wave walks the same
edges, and a relocating remnant carries what it has.

### 2.3 What is deliberately NOT built

- **The household lattice.** `BIO-pathogen-as-species` names "the
  connection graph and the household lattice"; no household model exists
  (`SOC-household` is raw), so within-community mixing is homogeneous here
  and the second half is deferred to that row.
- **Sanitation as a capacity term** (`BIO-5`, `DOM-9`'s second clause).
- **Seasonal disease** on the bake's twelve harvest phases.
- **Illness in the standing present** — a possessed body falling ill
  (`PLAY-illness-in-possession`, minted).
- **Disease as something the world's minds react to** — a plague god, a
  purity axis (`KNOW-evaluative-disease-substrate`), miasma as a perceived
  phenomenon (`KNOW-miasma-as-appearance`, minted).
- **Pathogen evolution.** Virulence differs at arrival and once endemic;
  it does not drift.

### 2.4 Population substrate and projections

The world has two population layers, not two competing population truths.
The **population substrate** is the statistically calibrated, causally
authoritative population: it may represent cohorts, distributions, and
connected host populations without materializing every person. Domain views
derive from it for epidemiology, economy, settlement, language, and other
phenomena. The Lot is not the authority for whether a population exists; it
is one consumer of the substrate.

The **projection layer** materializes the substrate for observation and play.
It has four forms: an aggregate readout, a composite case, a materialized
individual, and a salient character selected for interest rather than
statistical typicality. A composite may appear in-world — for example as a
physician's "typical patient" or a historian's representative farmer — but
it is not itself a causal entity. Only aggregate state and materialized
individuals may create persistent world consequences. Every projection
carries its source cohort, selection lens, materiality, sampling bias, and
whether consequences write back to the substrate.

This is a projection boundary, not a realism claim: the substrate is a
deterministic model calibrated for the phenomena Hornvale represents, while
the projection is weighted for salience, legibility, and play. For The
Murrain, persistence and spread are substrate questions; the Lot may later
materialize representative or salient lives affected by them.

## 3. The catalogue (data)

Five pathogen kinds, authored in `domains/species` as a component registry
(`pathogen_registry() -> ComponentStore<KindId, PathogenTraits>`), the same
shape as `biosphere_registry` and its siblings. **The catalogue is frozen
here, before measurement, and its count is asserted** (decision 0016; the
same discipline `tropes/` uses). Names are in the world's own register — the
names a pre-germ people gives a disease — never an Earth pathogen's.

```
  kind              class          hosts     transmission     niche (ConditionNiche)      R0    D (yr)  imm.
  ----------------  -------------  --------  ---------------  --------------------------  ----  ------  ----
  the-flux          environmental  all       fecal–oral       indifferent (everywhere)     n/a   n/a     no
  the-consumption   chronic        all       respiratory      indifferent                  3.0   2.0     no
  the-marsh-fever   vector         all       vector           warm, wet, low               n/a   n/a     no
  the-pest          zoonotic       all       flea/contact     temperate, dry grassland     3.0   0.027   no
  the-pox           crowd          all       respiratory      temperate (origin only)      6.0   0.038   yes
```

- **class** decides which half owns the kind: `environmental`, `chronic`
  and `vector` are endemic-only (derived, §4.3); `zoonotic` and `crowd` are
  epidemic (baked, §4.4). The classes are an enum; the split is by class,
  not by name, so a sixth kind is a data row.
- **hosts** is a `Vec<(KindId, f64)>` over the fifteen peoples; every row
  here is uniform (all peoples, weight 1). The field exists so a later
  campaign can author a people that is spared or especially prone — the
  proneness half of `KNOW-evaluative-disease-substrate` — without a schema
  change.
- **R0, D** feed the critical community size (§4.2) for the classes that
  have one. `the-flux` and `the-marsh-fever` persist in a reservoir that is
  not the host (water; the vector), so they have no critical size; they are
  present wherever their niche fits.
- **imm.** — whether one wave leaves survivors immune. Only `the-pox`. The
  pest gives no lasting immunity worth modelling at epoch grain.

Virulence and attack, epidemic kinds only:

```
  kind        attack A_max  fatality f   spillover weight σ_k
  ----------  ------------  ----------   --------------------
  the-pest    0.60          0.60         1.0
  the-pox     0.95          0.40         0.2
```

Attribution weights, endemic kinds only (§4.5): `the-flux` 0.35 (background)
and 0.55 (infant); `the-consumption` 0.25 (background); `the-marsh-fever`
0.40 × fit (background) and 0.25 × fit (infant); the unnamed residual
(injury, childbirth, the record's silence) 0.30 (background) and 0.20
(infant).

Every number in both tables carries a `plumb:` tag naming this section and
is frozen before unblinding; the ones that may move before it, and the rule
for moving them, are in §8.

## 4. Mechanism

### 4.1 Layering

```
  domains/species        PathogenTraits, PathogenClass, pathogen_registry(),
                         pathogen concepts; ConditionNiche reused as-is
  domains/epidemiology   NEW, kernel-only. The rules, over plain numbers:
                         critical_community_size(r0, d, births_per_year),
                         persists(metapopulation, ccs), wave_reach,
                         outbreak(pop, attack, fatality, susceptible),
                         attribution(...) — and its own predicates
                         (struck-by, outbreak-deaths) and concepts.
  windows/worldgen       the wiring: `plague_bake.rs` (the epidemic step,
                         called from the epoch loop), `bake_era_graphs`,
                         the emit of the two new predicates.
  windows/lot            the endemic derived read (`endemic.rs`:
                         `endemic_burden_at`), and the consumer: Ending
                         gains its causes; the `cause` slot; the odds
                         panel's cause table; the payload.
  windows/lab            six metrics; the census columns.
```

`domains/epidemiology` is a new crate because `DOM-9` named it, because the
rules take numbers and belong to no existing domain (`species` is authored
rows; `history` is the record), and because `history_bake.rs` is 10,950
lines and the epidemic step is a distinct sub-loop with its own tests
(ledger #4). It costs what `PROC-subfloor-roster-new-crate` says it costs:
hand-authored provisional sub-floor roster rows in the crate's first commit.

### 4.2 The critical community size

For a pathogen with reproduction number `R0`, infectious period `D` years,
in a host population with crude birth rate `b` per person-year, the
endemic-equilibrium count of infectives is `N · b · (1 − 1/R0) · D`; the
pathogen fades out when that count falls below a trough constant `c`. So

```
  CCS = c / (b · (1 − 1/R0) · D)
```

`c` is anchored, not authored: it is fixed so that a measles-shaped pathogen
(`R0 = 15`, `D = 8/365.25`, `b = 1/30`) reads `CCS = 250,000`, the low end of
Bartlett's measured range — which gives `c ≈ 170`, and every other kind's
threshold follows from its own row. `b = 1/e₀` at the Lot's calibration
centre (`e₀ = 30`), the same rate §4.2 of The Lot uses for births.

```
  kind              R0    D       CCS        against Task 0's largest (9,992)
  ----------------  ----  ------  ---------  --------------------------------
  the-consumption   3.0   2.0     ~3,800     REACHED on 7 of 9 seeds (upper bound)
  the-pest          3.0   0.027   ~280,000   never — persists in the reservoir instead
  the-pox           6.0   0.038   ~160,000   never — the negative control
```

`persists(metapopulation, ccs)` is the one rule, and it is asked by both
halves: by the derived read to decide where `the-consumption` is endemic,
and by the bake after an epidemic wave to decide whether an epidemic kind
stays. In these worlds the second answer is always no, and the unit test
that exercises the yes branch constructs a metapopulation above the bar —
the branch is real, tested, and unreachable on every seed, which is the
finding.

The metapopulation at a (site, era) is a population-substrate view: the
summed live host population over the occupations alive at the era's sites in
the site's connected component of the era's graph (`reachable_regions` at
the bake's own `conductance > 0` rule). Task 0's summed-peaks proxy was the
upper bound; the implementation must use the authoritative substrate view,
not require the Lot to reconstruct named lives in order to decide whether a
pathogen persists. The Lot's `population_at` reconstruction remains a
projection/materialization input for individual draws and local narrative,
not the epidemiological population authority.

### 4.3 The endemic burden (derived)

`hornvale_lot::endemic::endemic_burden_at(ctx, site, year) -> Vec<(KindId,
Burden)>` reads, for each endemic-class kind:

- `the-flux`: present everywhere; burden weight `0.35 · (0.5 + 0.5 ·
  min(1, N/50))` where `N` is the community's reconstructed population at
  `year` — density-dependent, the one thing every source agrees fecal–oral
  disease is.
- `the-consumption`: present iff `persists(metapopulation(site, era), CCS)`;
  weight 0.25.
- `the-marsh-fever`: weight `0.40 · fit`, `fit = tolerance_liebig(niche,
  substrate(site, era))` — the vector's niche at the site in that era's
  climate, on the substrate the capacity maps were built from.

The read is a window-side derivation over committed facts, the era graphs
(derived, never committed; `hornvale_worldgen::bake_era_graphs`), the
rules crate and the catalogue. It consumes no draw. **It lives in
`windows/lot`, not the composition root**, for a reason the layering
forces: the live population it needs is the Lot's own reconstruction of the
committed integral (`shape.rs`), and `windows/lot` depends on
`windows/worldgen`, so the root cannot call it back (ledger #8). The
`LotContext` computes the era graphs and components once per world and
reuses them across every lot it draws, which is also how the census pays
for it once per world (§6). Moving `shape.rs` down to `domains/history`,
beside the fact it inverts, would let the root own the read; it is a
follow-up, not this campaign's.

### 4.4 The epidemic (baked)

A new step in the epoch loop, `plague_phase(snapshot, era, year)`, runs
after every community has grown (`step_community`) and before
`raid_phases`, so a struck community meets the year's raids already
weakened. For each living community in snapshot order, for each epidemic
kind in catalogue order:

1. **Spillover.** One uniform draw from the bake stream; a spillover
   happens if `u < σ · σ_k · fit`, where `fit` is the kind's niche fit at
   the site in the era and `σ` is the global rate. `σ` is frozen at
   `1.3e-3` per community-epoch per unit fit, derived in §8 from the target
   count of outbreaks, and may move before unblinding under §8's rule.
2. **The wave.** A spillover strikes the origin and every occupied
   community within `R = 2` traversable hops on the era's graph (the bake's
   `traversable_neighbors`, iterated), in ascending vertex order. A wave
   does not cross a component boundary because there is no edge to cross.
3. **The outbreak.** For each struck community: `s` is the susceptible
   share (1 for a non-immunising kind; for `the-pox`, `min(1, (year −
   last_struck)/L)` with `L` the people's lifespan, so a wave inside living
   memory finds few to take); one uniform draw gives the attack fraction
   `a ∈ [0, A_max]`; deaths are `pop · s · a · f`. Population is reduced by
   the deaths. Two facts are committed on the occupation, both dated
   `year`: `struck-by` (Text: the kind) and `outbreak-deaths`
   (Number). Both facts have the minted outbreak-event entity as their
   subject and the struck occupation as their `place`; the place remains the
   location contract, while the event subject is the explicit pair identity.
4. **The ending.** If the death share `s · a · f ≥ φ = 0.30`, the
   community closes as `CauseOfEnd::Plague`, `Ended::Nature`; the
   `struck-by` fact at the same day names the pathogen, which is why
   `Ended` needs no new variant. If the remnant is at least `VIABLE_MIN` and
   `nearest_dest` finds a refuge, the survivors refound there through the
   existing relocation path (`Founding::From`, `lift_portfolio`,
   `carry_portfolio_to`) — the pestilent site is abandoned, which is what
   `vestige.rs` has rendered for a `Plague` ending since before one could
   happen. Otherwise the remnant scatters and the record says so.
5. **Persistence.** After the wave, `persists(metapopulation, CCS_k)` is
   asked of the origin's component. Yes: the kind is endemic in the
   component, and each following epoch it attacks the newborn cohort —
   deaths `pop · b · EPOCH_YEARS · A_max · f`, committed as the same two
   facts — until the component falls below the bar. No: nothing persists;
   the next outbreak needs the next spillover. (The yes branch is the
   unreachable-on-every-seed branch of §4.2; H-M2 constructs it.)

**Draw budget.** One draw per living community per epidemic kind per epoch
(~2 × 250 × 80 = 40,000 per world) plus one per struck community per wave.
All from `history/bake`, in commit order, so the sequence is a function of
the world alone.

**The stream and the epoch.** The bake's label moves `history/bake/v3 →
history/bake/v4` (decision 0006; The Granary's precedent), because
committed history changes — a plague changes who raids whom — whether or
not the consumption order of the old draws did. This is a genesis epoch in
full (§7).

### 4.5 The consumer — the Lot names the cause

Today a hazard death carries no category (`Ending::Hazard`, `cause: None`)
and the three Siler shares appear only in the odds panel. After this
campaign every ending names a cause, and **the hazard's magnitude is
untouched**: disease decides *what a background death is attributed to*,
never *how many there are* (§8 H-M4 pins `e₀` and the survival table
byte-for-byte). This is a fidelity call and leads the G3 flagged section.

```
  band        how the cause is drawn (one hash-expanded uniform, label "cause")
  ----------  --------------------------------------------------------------
  infant      the-flux 0.55 · the-marsh-fever 0.25·fit · unnamed 0.20, normalised
  background  first: violence with probability s/(1+s) — the strife EXCESS over
              a2's baseline is what strife is;
              else: the endemic weights of §4.3 plus unnamed 0.30, normalised
  senescent   of age; no disease is drawn
```

The band itself is what the hazard already implies at the drawn death age
— the same three-term split `hazard_shares` integrates, read at one age
rather than over the whole curve.

Two endings are new:

- `Ending::Outbreak(kind)` — the life's community was struck but not
  ended. At each `struck-by` day inside the life span the life meets a
  discrete hazard `p = deaths / population_at(day)` from the committed
  pair, spliced into the life course exactly as the community-fate hazard
  is (The Lot §4.3).
- `Ending::CommunityFate(Plague)` now names its pathogen from the same
  facts, and the story cites them.

The **`cause` slot** joins the roster (23 non-by-design slots; the "of 22"
at `lot_readout.rs:151,205` and `lot-slots-filled-mean`'s doc move with
it), filled for every dead lot, with sources: the hazard's authored
constants, the endemic read's inputs (the `occ-peak` / `occ-person-years`
facts the population came from; the site's `biome`; the strife field) or
the outbreak facts. The **odds panel** gains a cause table at the
(people, site, year): the integral of the Siler curve split by band and
then by the §4.5 weights, plus the committed epidemic share
(`Σ outbreak-deaths / person-years · e₀`), so the site's "causes" tile
— *smallpox 15%, dysentery 12%, tuberculosis 12%* — has its analogue, read
from the world.

The payload is additive: `ending.cause` is populated on every dead lot,
`ending.kind` gains `"outbreak"`, `odds.causes` is a new array. The exhibit
(`clients/lot`) renders the cause in the Story stage and the table in the
Life stage; `make lot-check` and the byte-identity smoke are unchanged in
kind.

## 5. Facts committed, and their shape

Two new predicates, owned by `domains/epidemiology` and registered by it:

```
  predicate         subject      object          day        cardinality
  ----------------  -----------  --------------  ---------  -----------------------
  struck-by         outbreak     Text (kind)     the year   one per outbreak event
  outbreak-deaths   outbreak     Number          the year   one per outbreak event
```

They carry no `occ-` prefix on purpose: that family is `domains/history`'s,
and these are `domains/epidemiology`'s own predicates on an entity another
domain minted — the shape `has-caste` (culture, on a settlement) already
has. They are **dated events**, the shape `occ-founded` and `occ-ended`
already have, not a trajectory (decision 0797 is about integrals versus paths; an
event is neither). Both facts use the minted outbreak event as `subject`; their
`Fact.place`/location is the struck occupation. The pair is joined on the shared
event subject plus matching occupation place and day; a reader that finds one
without the other has found a defect, and `history_emit`'s round-trip test pins
that both are written together. Expected volume
(§8): ~120–150 events per world, ~300 facts, under 2% of the history facts
a world already carries.

`CauseOfEnd::Plague` gains its first producer and changes nothing in the
enum. Pathogen kinds are registered as concepts (the `kind_concept` pattern
`species` uses), so a struck-by fact's Text object is a registry-checked
name.

## 6. Cost, and the gate it is compared against

The census refuses above **1,650 s** and alarms above **1,320 s**
(`cli/tests/suite/census_duration.rs`, re-set by The Lot); the last two
recorded rows read 1,142 s and 1,186 s (`docs/timings.md`, `cpu_ratio`
28–31 on 40 cores), and The Lot's refused refresh read 1,278 s. The Lot paid a second
census run for not doing this multiplication before submitting, so it is
done here, in the ledger, before any refresh is queued.

Two costs are new per census world: the bake's epidemic step (a per-epoch
loop over living communities — cheap, and inside the existing bake) and the
era-graph derivation the endemic read and two metrics need. The second is
the one to measure: `bake_era_graphs` derives 25 graphs per world and the
Task 0 probe paid it 9 times inside a 37 s run that also built nine worlds.
**Plan Task 1 measures it in isolation on the Mac** and the ledger entry
carries `Δ_wall ≈ Δ_cpu_per_world × 1000 / cpu_ratio` against the 134 s
of alarm headroom and the 464 s of refusal headroom above the 1,186 s row. The `LotContext` already
holds the world's terrain, so the read takes the `bake_era_graphs_from`
form and pays no second sculpt (the `connection_graph_from` precedent).
If it still does not fit, the derived read caches components per era
inside the metric view (one derivation per world, shared by every metric —
the `lot_sample` pattern already does this for the Lot's context) before
anything else is considered.

## 7. Determinism obligations

- **The bake stream bumps** to `history/bake/v4`; no new label, no
  hash-keyed draws in the bake (0796's form is for reader-keyed
  observations).
- **Genesis epoch.** Every committed world moves: `make rebaseline-goldens`,
  `make rebaseline`, the seed-42 keystone fixtures, the vessel session
  fixtures, the possession galleries. A census refresh at close
  (`make sluice-census`), the anomaly evaluable-column witness restated at
  all four sites the test names, the Gnomon injection arms re-authored on
  lefford under the census flock. Old world files refuse to load rather
  than approximate, as every epoch's do.
- **The endemic read draws nothing.** The Lot's cause draw is one more
  hash-expanded uniform per life, label `"cause"` — a source-scan test
  already pins that `windows/lot` never names `Stream`, and it stays that
  way.
- **Quantize at emit only.** `outbreak-deaths` is a `Number` fact and
  quantizes at `Ledger::commit` like every other; the cause table in the
  payload quantizes through `hornvale_kernel::quantize`.
- **No wall clock; no `HashMap`; `total_cmp` with vertex-order tie-breaks**
  in the wave walk and the component read.
- **The pathogen's niche fit** uses `tolerance_liebig` on the era substrate,
  which is `libm`-routed and bit-identical across hosts (decision 0041).

## 8. Preregistered measurement (decision 0016)

Frozen here, before the mechanism is written. Two halves, asserted
separately.

**Mechanism half — unit-pinned, world-independent.**

- **H-M1** The trough constant `c`, *derived* from the measles anchor
  (`CCS = 250,000` at `R0 = 15`, `D = 8/365.25`, `b = 1/30`), lies in
  `[165, 175]`; the function is strictly decreasing in each of `R0`, `D`,
  `b` on a grid; and the catalogue rows read `the-consumption ∈ [3,000,
  5,000]`, `the-pest ∈ [200,000, 400,000]`, `the-pox ∈ [120,000, 200,000]`.
  (The anchor itself is a definition, not a test — asserting it would be a
  tautology.)
- **H-M2** `persists` is true for a constructed metapopulation of
  `2 × CCS` and false at `CCS / 2`, for each catalogue kind that has a CCS
  — both directions, per kind.
- **H-M3** On a constructed graph, a wave from an origin strikes every
  occupied vertex within `R` hops and no vertex beyond `R` or in another
  component; a struck community's population falls by exactly `pop · s · a
  · f`; a death share of `φ` closes it as `Plague` and `φ − ε` does not.
- **H-M4** For every `(lifespan, strife)` on The Lot's H-M2 grid, the
  survival table, `e₀` and `q₁₅` after this campaign are **byte-identical**
  to before it — attribution changes no magnitude. The per-band cause
  weights sum to 1 at every site.
- **H-M5** Same seed, two builds: byte-identical world, byte-identical lot
  payload; the pinned-lot refusal cases of The Lot's H-M4 still hold.
- **H-M6** The catalogue holds exactly five kinds, two of them epidemic; a
  sixth row fails the count assertion.

**Prediction half — over the nine seeds; counts, not ratios.** Read by a
hand-run `probe:` readout (`windows/lot/tests/suite/murrain_readout.rs`,
the Task 0 pattern) while the same quantities become `epidemic-*` and
`lot-*` census columns over 1,000 seeds at the close refresh.

- **H-P1** `the-pox` is endemic at the present in **0 of 9** seeds, and the
  census column `epidemic-crowd-endemic` reads 0 on **1,000 of 1,000**
  worlds. Derived: the largest present metapopulation is 9,653 (upper
  bound) against `CCS ≈ 160,000`. This is the headline, and it is
  non-vacuous because the same column reads a real number
  (`epidemic-largest-metapopulation-now`, expected 1,000–10,000) against a
  real formula, and because H-M2 exercises the branch the worlds never
  reach.
- **H-P2** `the-consumption` is endemic in the largest present component on
  **between 4 and 7 of 9** seeds, and on **neither** seed 100 nor seed 256.
  Derived from Task 0's NOW column against `CCS ≈ 3,800`: seven exceed it
  by the summed-peaks upper bound; at a live population of ~70% of that
  bound, seeds 1, 13 and 777 fall to the margin. Which seeds is the finding.
- **H-P3** `Plague` endings per world lie in **[5, 60]** on each of the
  eight growing seeds and at most **8** on seed 100. Derived, not read:
  `σ = 1.3e-3` over ~20,000 community-epochs at mean fit ~0.3 gives ~8
  spillovers per world for the pest and ~1.6 for the pox; a wave at `R = 2`
  strikes ~15 occupied communities at present-era degree (fewer early);
  the ending fires on `a ≥ φ/f`, i.e. one struck community in six for the
  pest and one in five for the pox. `first-day-occ-cause-plague` is
  non-blank on every growing seed.
- **H-P4** Outbreak events (`struck-by` facts) per world lie in
  **[40, 400]** on the eight growing seeds and at most **60** on seed 100
  (mean occupied-degree 0.97: a wave there barely leaves its origin).
- **H-P5** Of 200 lots per seed, **between 40 and 160** die of a *named*
  disease (`cause` is a catalogue kind) on every seed. Derived: `q₁₅ ≈
  0.43` at the calibration band with the flux taking 0.55 of infant deaths
  gives ~45 of 200 from the infant band alone.
- **H-P6** `lot-slots-filled-mean` on the 23-slot roster is at least
  **15 of 23** on every seed — the new slot fills for every dead lot, so
  the fill rate does not fall.

**What may move, and when.** `σ`, `A_max`, `f` and `φ` may be moved before
unblinding to land the *derivations* in H-P3 and H-P4 where the spec says
they land — every move in the ledger — and never after. The attribution
weights of §3 and §4.5 do not move at all; a wrong-looking cause table is
a finding. The catalogue's count does not move. A falsified prediction is
the finding, not a failure; the chronicle carries it either way.

## 9. Non-goals

§2.3, restated as fences for the plan: no household lattice; no sanitation
or capacity coupling; no phase-level seasonality; nothing in the vessel or
the standing present; no consumer in religion, knowledge or the vestige
beyond what already reads `Plague`; no evolution; no pathogen entities (a
kind is a registry-checked Text, like `occ-people`); no change to the
Siler constants or `GROWTH_RATE`.

## 10. Risks

- **The Plague band is mis-set.** `σ` is the one frozen unknown and the
  band is wide on purpose. Too few endings and the count reads under 5;
  too many and the community-fate share of person-years moves every
  history column. Both are findings; the derivation in H-P3 is what says
  which way, and it is written before the producer.
- **Census cost.** §6. The era graphs are the risk, and the cache is the
  remedy, measured before the refresh is queued.
- **The `Ending` change ripples** through `draw.rs`, `slots.rs`, `json.rs`,
  `narrate.rs`, the readout, the lab metrics and the exhibit's TypeScript
  types. The compiler enumerates the Rust sites; `make lot-check`
  enumerates the client's.
- **A genesis epoch moves everything**, and two campaigns are queued ahead
  of this one on main (`the-culvert`, `the-spillway`); neither touches the
  bake, history, species, lot or the lab metrics (checked at branch time),
  but the stage gate's absorb will regenerate every fixture anyway.
- **The anomaly witness and the Gnomon arms** are lefford work under the
  claim, each with a known procedure and a known failure mode (The Lot's
  ledger #20 and the brief's operational notes); they are plan tasks, not
  afterthoughts.

## 11. Definition of Done

- The catalogue, the rules crate with its roster rows, the epidemic step,
  the endemic read, the Lot's causes, the six metrics; every H-M green;
  the H-P readout run once and recorded verbatim in the ledger.
- Goldens and artifacts rebaselined; the census refreshed on lefford and
  its branch merged; the anomaly witness restated; the Gnomon arms
  re-authored; `make lot-check` green; `docs/timings.md` carries the
  measured census row and the ledger carries the §6 arithmetic beside it.
- Chronicle (`book/src/chronicle/the-murrain.md`), retrospective, the
  freshness sweep — at least: the Lot chronicle's three plague sentences
  (`book/src/chronicle/the-lot.md:272,289,292`),
  `book/src/domesday/history.md`'s plague-metric doc, the windows roster in
  `windows/CLAUDE.md`, and the registry rows `SOC-contact-structure` ("arrives without reference
  to one") and `SOC-sclerosis`'s cause list.
- Registry: `BIO-pathogen-as-species` → shipped with a Where cell naming
  the deferred household half; `DOM-9` → partially shipped (the crate, not
  the sanitation term); `ECON-livelihood`, `KNOW-miasma-as-appearance`,
  `PLAY-illness-in-possession` minted; `SOC-household`, `TOOL-11`,
  `PLAY-possession-r0`, `KNOW-evaluative-disease-substrate` Where cells
  cross-linked.
- Decisions 0856–0859 (§12) ratified; the Confidence Gradient re-scored if
  any bet it carries concerns history's causes of ending (checked at the
  sweep).

## 12. Decisions expected

- **0856 — A pathogen is a species whose niche is another species.** The
  catalogue is species rows; presence is condition-niche fit on the era
  substrate; spread rides the connection graph and the relocation paths;
  the endemic burden is derived and the epidemic is baked (the diffuse /
  discrete split `MAP-61` prescribes).
- **0857 — Persistence is decided on the connected metapopulation, never
  the community.** One critical-size rule, anchored on a measured threshold,
  asked by both halves; a crowd pathogen in the catalogue is the negative
  control, and the branch it never reaches is unit-tested.
- **0858 — Disease attributes background deaths; it does not change their
  number.** The Siler magnitude stays calibrated and byte-identical;
  disease partitions it. A campaign that wants disease to move `e₀` must
  re-derive `GROWTH_RATE` as a gross rate first, and says so here.
- **0859 — An outbreak is a dated event with its deaths.** Two facts share a
  minted outbreak-event subject, both retain the struck occupation as their
  place, and both carry the same day; not an integral, not a trajectory; the
  pair is the record of the event.

## 13. Staging (for the plan)

1. **The catalogue and the rules.** `PathogenTraits` and the five rows;
   `domains/epidemiology` with H-M1, H-M2, H-M6 and its provisional roster
   rows; the census-cost measurement of §6 (Task 1) with its ledger entry.
2. **The epidemic.** `plague_bake.rs`, the two predicates and their emit,
   the stream bump, H-M3, H-M5; goldens and artifacts rebaselined in their
   own commit.
3. **The endemic read and the Lot.** `endemic_burden_at`; `Ending`'s
   causes, the `cause` slot, the odds table, the payload; H-M4; the
   readout and the H-P verdicts; the exhibit's two renderings.
4. **The instrument.** The six metrics; the census refresh; the anomaly
   witness; the Gnomon arms; Domesday.
5. **DoD.** §11.
