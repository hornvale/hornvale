# The Murrain — decision ledger

Campaign: **The Murrain** — disease as a species whose niche is another
species: a pathogen catalogue, an epidemic step in the deep-history bake
over the connection graph, and the first named cause of death a drawn life
can carry.
Opened 2026-09-06 from the arc brief that followed The Lot (landed on main
at `e3f47eb47`, 2026-09-06): *"The next campaign fills the largest hole that
campaign measured."* The Lot's richness-gap table ranks disease the largest
single hole, and the brief recommends a pathogen-as-species design.
Branch: `campaign/the-murrain`. Decision block: **0856–0865** (reserved
2026-09-06 via `make decision-block`; main ceiling 0847 at reservation).

A *murrain* is a pestilence of livestock — a disease named from its host,
which is the campaign's ontology: a pathogen is a species whose niche is
another species.

## The brief's facts, verified before anything was designed

The brief said to verify three things rather than assume them. All three
hold on main at `b096b7d50` (the branch point):

- `CauseOfEnd::Plague` exists (`domains/history/src/record.rs:16`) and
  nothing in `windows/worldgen/src/history_bake.rs` ever assigns it. The
  bake's non-test `close(...)` calls carry `Fled` (lines 2212, 4085),
  `Migrated` (3338, 3655, 4084), `Breached` (3579) and `Famine` (3670) —
  never `Plague`, never `Burned`. Consumers of `Plague` already exist and
  have never fired on a real world: `vestige.rs:157` maps it to
  `HazardKind::Pestilent`, `flesh.rs:509` to unburied bones, the lot's
  `slots.rs:661` to "was emptied by disease", the vessel's `ruin_prose.rs`
  and the almanac's `history.rs:649` render it.
- The census column `first-day-occ-cause-plague` is blank on all 1,000 rows
  of `book/src/laboratory/generated/the-census/rows.csv` (column 69; so is
  `first-day-occ-cause-burned`, column 68; `first-day-occ-cause-famine`
  is blank on 303 and numeric on 697).
- The Lot's own cause-of-death slot is narrower than the brief describes:
  a hazard death carries **no category at all** in the payload
  (`json.rs:234`: `kind: "hazard", cause: None`); the three Siler shares
  (infant / background / senescent) appear only in the odds panel
  (`draw.rs:413–417`), per (people, site), never per life. So the consumer
  this campaign builds for is `Ending::Hazard` with no sub-kind, plus
  `Ending::CommunityFate(Plague)` which no world has ever produced.

Two more facts the design leans on, read from the code rather than the
brief: the bake takes every draw from one sequential stream
(`history/bake/v3`, `domains/history/src/streams.rs`) in commit order; and
a raid's targets, a daughter's site and a migrant's refuge are all chosen
from `traversable_neighbors` on the era's connection graph
(`conductance > 0.0`) — so "spreads over the connection graph" has one
existing rule to reuse, not a new one to author.

## Entries

#1 [G1] — **Which of The Lot's measured silences does the next campaign
fill, and in what shape?** · **Decision: disease, in four parts.** (a) A
catalogue of pathogen kinds authored as species rows in `domains/species`
(host range over the peoples, transmission mode, a `ConditionNiche` for
vector gating, virulence at arrival and once endemic). (b) A pure
persistence-and-outbreak rule in a new kernel-only crate,
`domains/epidemiology` (the name DOM-9 gave it), taking plain numbers — the
pathogen's traits, a community's population, its occupied degree, its
component's metapopulation. (c) An epidemic step in the bake: reservoir
spillover weighted by the wilderness field and the pathogen's niche fit,
arrival by contact over the era's graph or carried by a relocating remnant,
outbreak deaths, the `Plague` ending when the outbreak's death share crosses
a frozen fraction (survivors relocate through the existing path if viable),
and persistence gated by the connected metapopulation against the
pathogen's critical community size. Committed per occupation as dated
arrival facts and one deaths integral (decision 0797's form). (d) The Lot
names the cause: a hazard death draws a category over the site's endemic
burden, a community fate of `Plague` names the pathogen, and the odds panel
grows a per-cause table — the shape of the site's own "causes" tile. ·
**Why:** the brief's recommendation; the richness-gap table ranks disease
the largest single hole and the one the site leans on most (3 of Hari's top
6 causes); `BIO-pathogen-as-species` states the ontology, `SOC-contact-
structure` the compartment, and `MAP-61` already splits disease into
diffuse coupling (a derived fixed point) and discrete coupling (events in
the bake) — the split #2 adopts. · **Alternatives discarded:** households
(`SOC-household`: five silences behind one model, but blocked on BIO-3 /
SOC-2's sex model, so it is two campaigns, not one — a legitimate G1
outcome the brief allowed, declined for size, row unchanged); livelihood
(no economy domain exists; the largest new subsystem of the three;
`ECON-livelihood` minted at the registry pass, not built); a derived-only
disease read with no producer (fills the slot's name but `Plague` stays
unproduced, the world's populations stay unmoved by any epidemic, and every
consumer already wired for `Plague` stays dead). · **Ideonomy: 1 pass
(negation + abstraction-lift, procedure organon, axes distribution /
cyclicity / modularity; the organon is below), no overturn of the
recommendation, one overturn of the brief's framing (#7), seven
enrichments adopted:** (i) two regimes, two ontologies — endemic burden as
a derived field, epidemics as baked events (#2); (ii) the critical size is
the connected metapopulation's, not the community's (#7); (iii) the
reservoir is the wilderness field, not an authored animal; (iv) dispersal
rides the bake's existing relocation paths as well as adjacency; (v)
virulence keyed to time since arrival, which the dated arrival fact carries
for free; (vi) a crowd pathogen in the catalogue as the negative control
the prediction needs; (vii) what the inhabitants perceive is miasma, never
the pathogen — decision 0003's appearances-not-sources, captured as a row,
not built. · **Capture:** `ECON-livelihood` to mint; `BIO-pathogen-as-
species` Where cell at close; new rows `KNOW-miasma-as-appearance` and
`PLAY-illness-in-possession`; the household-lattice half of
`BIO-pathogen-as-species` deferred to `SOC-household` and said so in both
rows.

#2 [Q] — **Is the endemic (non-epidemic) disease burden baked into the
bake's populations or derived at read time?** · **Decision: derived.** The
endemic burden is a read over committed facts (community size, biome, water
proximity, the arrival facts) that the Lot consumes; it does not depress the
bake's populations. Only an outbreak's excess deaths are baked. · **Why:**
`GROWTH_RATE` is a *net* rate — births minus deaths — and the Lot's Siler
`a2` background term is that same background mortality on the individual
side; an explicit endemic drag in the bake would count the same deaths
twice and retune every population-bearing census column to correct a
double-count. An epidemic is the one mortality outside the net rate, so it
is the one that belongs inside the loop. `MAP-61` prescribes exactly this
seam: "diffuse coupling (trade / culture / disease) = an order-independent
fixed-point over the sparse graph; discrete coupling (raid / founding /
plague) = events". · **Alternatives discarded:** bake it (double-count);
leave the endemic half out entirely (the slot then names a cause only on
the rare plague death, and dysentery-and-consumption — the finding's whole
point — never appears in a life). · **Ideonomy: 1 pass** — the G1 pass's
*distribution* axis (concentrated outbreak vs. distributed burden) produced
the split, and a negation of "a pathogen is a species" gave its opposite,
"a disease is a condition of a place" (miasma), which is precisely what a
derived field over place facts is. · **Capture:** flagged for G3 as a
fidelity call (the endemic half never moves the world), leading the
flagged section with #3.

#3 [Q] — **Which stream do the disease draws consume, and is this an
epoch?** · **Decision: the bake's own stream, in commit order, and yes.**
`history/bake/v3` → `history/bake/v4`; the spillover, arrival and outbreak
draws are taken from the same sequential `Stream` every other bake draw
takes, at the point in the epoch loop where they fire. No new stream label.
A genesis epoch in full: byte-goldens rebaselined, artifacts regenerated,
a census refresh at close, the Gnomon injection arms re-authored, the
anomaly evaluable-column witness restated. · **Why:** decision 0006 (an
epoch suffix, never a rename); The Granary bumped v2 → v3 for changed
committed history and The Sundering declared a genesis epoch for the same
reason; decision 0796 sanctions hash-keyed expansion for *reader-keyed
observations* that commit nothing, and these draws drive facts. ·
**Alternatives discarded:** a separate `history/pest` stream (keeps v3's
consumption order intact, but committed history moves anyway — a plague
changes who raids whom — so the epoch is owed regardless, and one loop on
two streams is more contract surface for nothing); per-(community, epoch)
hash-keyed draws (0796's form, for the wrong kind of thing). · **Ideonomy:
1 pass** — the *modularity* axis: one loop, one stream is where the seam
wants to be; the "two streams" variant is the fused-monolith test failing.
· **Capture:** leads the G3 flagged section (save-format / epoch).

#4 [Q] — **Where does the code live?** · **Decision:** pathogen traits and
their registry in `domains/species` (they are species, and `ConditionNiche`
+ `environment_fit` are reused as-is for vector gating); the pure rules —
critical community size, the per-epoch outbreak outcome — in a new
`domains/epidemiology` depending on the kernel only; the wiring in
`windows/worldgen` as a sibling module of the bake (`history_bake.rs` is
10,950 lines and the epidemic step is a distinct sub-loop with its own
tests); the consumer in `windows/lot`. · **Why:** the layering (a domain
never depends on a sibling, so the rules take numbers, not `PathogenTraits`);
`DOM-9` already names the crate and `TOOL-11` and `PLAY-possession-r0` want
its functions; `PROC-subfloor-roster-new-crate` says what a new crate costs
and how to pay it (hand-authored provisional roster rows in the crate's
first commit). · **Alternatives discarded:** rules inside `domains/species`
(authored rows and dynamics in one 7,700-line file); everything inside
`history_bake.rs`; a window (windows read the ledger and render; these
rules make the world). · **Ideonomy: 1 pass** — the *modularity* axis
again; fusing the modules put dynamics in the data crate, which is the
seam the split exists to avoid. · **Capture:** none beyond the plan.

#5 [Q] — **Does the visual companion open for this brainstorm?** ·
**Decision: no.** · **Why:** the brainstorming skill offers it
just-in-time for a question clearer shown than told, to a human who is
present; under autopilot no human sees a question before G3, and none of
this brainstorm's questions was visual — the design is a procedure and two
tables. Nathan's standing preference ("don't ask, just set it up") governs
*how* it opens when a visual question arises, not whether one has. ·
**Alternatives discarded:** open it to an empty room (a browser tab nobody
reads, at token cost). · **Ideonomy: 1 pass** — process, not design; the
one-line negation is the alternative discarded.

#6 [Q] — **Campaign name.** · **The Murrain** — a pestilence named from its
host. Checked unused in chronicles, specs, decisions and branches. ·
**Ideonomy: 1 pass** — abstraction-lift + list, with cardinality and
discovery-vs-invention axes; one existing term selected from the language
rather than a new term invented for the campaign, with no material naming
alternative surfaced. No overturn. · **Capture:** none.

#7 [Q] — **What number does the derivable finding freeze against, and is
it measured before the spec?** · **Decision: the connected metapopulation
— the summed population of every community one pathogen can reach over the
era's graph — measured by a Task 0 probe before the spec, with a public
accessor `hornvale_worldgen::bake_era_graphs` added so the probe reads the
bake's own era graphs rather than a present-day proxy.** · **Why:** the
brief framed the finding on community peaks ("communities peak under 90");
the abstraction-lift's percolation comparator (a fire needs contiguous
fuel; a pathogen needs contiguous susceptibles) shows the unit that
persistence is decided on is the *component*, and The Sundering measured
that glacial low-stands reconnect 755 of 843 refugia — so the largest
metapopulation may sit in an era that is not the present. The Lot's Task 0
(`lot_probe.rs`) is the precedent for measuring the number a spec rests on
before writing it; memory: *measure before calling it a campaign*. The
accessor is a read over derived, never-committed state (the graph "stays
derived", The Sundering), tagged `bare-ok(count: return)`. · **Alternatives
discarded:** the present-era graph via `connection_graph_of` (misses the
reconnection); making `bake_eras` public (exposes an internal triple). ·
**Ideonomy: 1 pass — the framing overturn named in #1.** · **Capture:**
the probe's reading is the Task 0 record below and spec §1.

#8 [G2] — **Spec self-review (2026-09-06), five corrections before G3.** ·
(i) The endemic derived read cannot live in `windows/worldgen`: it needs the
Lot's reconstruction of the committed person-years integral (`shape.rs`),
and `windows/lot` depends on the root, so the root calling it back is a
cycle. **Decision: `hornvale_lot::endemic::endemic_burden_at`**, reading
worldgen's `bake_era_graphs` and the rules crate; moving `shape.rs` down to
`domains/history` beside the fact it inverts is a captured follow-up, not
this campaign's. (ii) The two new predicates were drafted as `occ-struck-by`
/ `occ-outbreak-deaths`; the `occ-` family is history's. **Decision:
`struck-by` and `outbreak-deaths`, owned and registered by
`domains/epidemiology`** on an entity history minted — the `has-caste`
shape. (iii) H-M1 as drafted asserted the anchor the trough constant is
*defined* from — a tautology. **Decision: assert the derived constant's
band and the three catalogue rows' bands instead.** (iv) The endemic
in-bake branch (§4.4 step 5) named no formula; it now attacks the newborn
cohort at `A_max · f`. (v) §6 quoted 1,278 s as a recorded row; it was The
Lot's *refused* refresh — the recorded rows are 1,142 s and 1,186 s, and
the headroom arithmetic is restated against 1,186. · **Ideonomy:** none —
corrections of the draft against the code, not design choices; (i) is the
layering deciding, not a preference. · **Capture:** the `shape.rs`
relocation as a follow-up below.

#9 [Q] — **Does the endemic burden change how many die, or only what they
die of?** · **Decision: only what they die of, this campaign.** The Siler
hazard's magnitude and calibration (The Lot's H-M1) stay byte-identical;
disease partitions the background and infant terms into named causes;
strife's excess over the `a2` baseline is what "violence" means. · **Why:**
the same double-count argument as #2 on the individual side — `a2` already
IS the background mortality, calibrated to the site's `e₀` band; a disease
term on top would move `e₀` off a band chosen from real life tables to
represent exactly the mortality disease causes. Making disease change the
number requires re-deriving `GROWTH_RATE` as a gross rate and `a2` as a
disease-free residual, which is a campaign of its own. · **Alternatives
discarded:** scale `a2` by `(1 + burden − mean burden)` (an authored
multiplier with no measurement behind it — the same argument that removed
the era multiplier from The Lot's hazard); leave attribution out and name
only plague deaths (the flux and the consumption never appear in a life,
which is the finding's whole point). · **Ideonomy: 1 pass** — the negation
"disease only depresses, never ends" from the G1 organon, turned on its
head: here disease neither depresses nor ends, it *names*; the substitution
"attribution → magnitude" is the discarded alternative. · **Capture: this
is a FIDELITY CALL and leads the G3 flagged section** (carve-out: fidelity
cuts always go to Nathan); decision 0858 is written so a later campaign
knows what it must re-derive to lift it.

#10 [Q] — **May composite population cases appear in-world, or only in
analytical views?** · **Decision: both.** A composite may be an in-world
physician's typical patient, historian's representative farmer, or similar
account, while remaining non-causal. A materialized individual is the
explicit boundary at which identity, interaction, and persistent
consequences exist. The substrate remains the statistical authority; the
projection records its source cohort, selection lens, materiality, sampling
bias, and write-back permission. · **Why:** the project already separates
abstract groups from materialized lives, and The Lot's person-years are a
projection draw rather than a census. Allowing composites in both contexts
keeps useful explanatory cases without falsely instantiating one average
person. · **Alternatives discarded:** analytical-only composites (needlessly
restricts in-world medicine, history, and sociology); composites as causal
people (silently turns a summary into an entity). · **Ideonomy: 1 pass** —
negation + scale, with intentionality and animacy axes; the meaningful scale
is aggregate → composite → materialized individual, and no overturn. ·
**Capture:** spec §2.4; the projection boundary leads the next G3 review.

#11 [G5] — **Task 1 review correction: what makes the population substrate
authoritative?** · **Ruling:** the era/site view must reconstruct live
population from committed trajectory information (`founded`, `ended`,
`peak_population`, `person_years`, and the applicable opening value), sharing
one lower-layer implementation with the Lot or moving that reconstruction to
a domain-owned helper. Summed peaks remain the Task 0 upper bound and may not
be published as the epidemiological authority. H-M1 must bind the authored
catalogue rows rather than repeat their numbers as test literals, and H-M2
must exercise both sides of the threshold for consumption, pest, and pox. ·
**Why:** review found the first implementation labeled a peak sum
authoritative, which contradicts spec §4.2 and can change persistence
decisions; the same review found the mechanism tests under-pinned. · **Cost if
wrong:** a shared reconstruction changes the substrate values and may move
the preregistered endemicity predictions, but retaining the proxy would make
the campaign's central population claim false. · **Capture:** fix round 1 of
Task 1; update the plan only if the shared-helper boundary changes.

#12 [G5] — **Task 2 review closure: epidemic facts and ledger identity.** · **Ruling:** the history bake advances to `history/bake/v4`; epidemic work runs through the production epoch helper after growth and before raids; persistence is host-weighted and connected-component-wide, with fixed `attack_max` recurrence and deterministic draw accounting. Outbreak pairs use a minted outbreak-event subject while preserving the struck occupation in `Fact.place`; `struck-by` and `outbreak-deaths` join on that shared event subject, matching place, and day. The v4 keystone and generated artifacts are rebaselined without census changes. · **Why:** review found that a location envelope cannot double as event identity, that manually repeated phase tests could pass while production order drifted, and that stale fixture/prose contracts would make the new identity misleading. · **Capture:** Task 2 fix rounds and final review; source `39087f079`, artifacts `0d37f1ef0`, documentation `7619761e3`.

## Ideonomy — the G1 organon

Tuple picked: operators *negation* + *abstraction-lift*; organon
*procedure*; dimension prompts *distribution*, *cyclicity*, *modularity*.

**Lifted shape.** Strip the goblins, the flux and the ledger and the
candidate is: *a replicator whose niche is a population of hosts, whose
persistence needs a supply of new susceptibles above a threshold set by
host turnover; below the threshold it persists only by an outside reservoir
or by consuming its hosts slowly; above it, it cycles.* Recognisable
elsewhere as a wildfire regime (fuel connectivity sets whether a burn
propagates; a landscape below the percolation threshold has no fire season,
only lightning strikes that die where they land), as an invasion with
propagule pressure (arrival rate from outside sets outbreak frequency), as
rumour propagation (`UNI-16` already says belief spread is SIR), and as the
language and cult-form diffusion the connection graph was built for. The
comparators each hand back a move: percolation says *measure the component,
not the node* (#7); propagule pressure says *the reservoir's density sets
the spillover rate*, and the demography domain already has that density as
`wilderness`; the wildfire says the periodic regime — measles' biennial
cycle, the thing a crowd disease *is* — cannot exist below the threshold,
which is the finding stated in cyclicity terms.

**Opposites of the candidate.** The definitional properties, each negated,
and what the negation turned out to be:

```
property negated                    opposite                       became
----------------------------------  -----------------------------  ---------------------------
a pathogen is a SPECIES             a disease is a condition of    the endemic half as a
                                    a PLACE (miasma)               derived field over place
                                                                   facts (#2); and what the
                                                                   inhabitants perceive
                                                                   (0003: appearances only)
its niche is ANOTHER SPECIES        its niche is the ENVIRONMENT   the environmental-reservoir
                                    (water, soil)                  class, which is exactly what
                                                                   persists at small N
it spreads over the CONNECTION      it spreads NOWHERE (bound to   vector gating by
GRAPH                               its vector's biome); it        ConditionNiche; dispersal
                                    spreads only WITH ITS HOSTS    riding relocate/take_flight/
                                    (carried by the displaced)     migration — five sites the
                                                                   bake already has
it PRODUCES CauseOfEnd::Plague      disease only DEPRESSES; a      the Plague ending fires on
                                    community never ends of it     a death-share threshold,
                                                                   not on VIABLE_MIN (= 2),
                                                                   which a 30-person outbreak
                                                                   never reaches
virulence is AUTHORED per pathogen  virulence EVOLVES (attenuates  two values: at arrival and
                                    once endemic)                  once endemic; the dated
                                                                   arrival fact carries the age
the LOT reads it                    the WORLD reads it (raids,     the bake reads population,
                                    religion, disgust cues)        so raids read it for free;
                                                                   religion and KNOW-* captured
it lives in the BAKE                it lives in the STANDING       PLAY-illness-in-possession,
                                    PRESENT (a possessed body      captured
                                    falls ill)
```

**The procedure.** Order is load-bearing; each step names what must be
true before it runs and what is true after.

```
step  does                                     needs before             leaves after
----  ---------------------------------------  -----------------------  ---------------------------
0     measure the connected metapopulation     nothing                  the number the finding is
      over the nine seeds and 25 eras                                   frozen against (Task 0)
1     freeze the catalogue (data) and the      step 0's number          a prediction with a COUNT:
      critical-size rule (code); preregister                            crowd pathogens persist in
      the finding                                                       0 of 9 worlds; the others do
2     the epidemic step in the bake, one       step 1 (the rule is      Plague endings, arrival
      pathogen class at a time, the crowd      what the step calls)     facts, deaths integrals;
      pathogen last                                                     history/bake/v4; goldens
3     the endemic burden as a derived read     step 2's arrival facts   a per-site cause table
      over committed facts                                              with no draw in it
4     the Lot's category draw and the odds     steps 2 and 3            a named cause on every
      panel's cause table                                               drawn death; readout
5     census refresh, anomaly witness,         step 4 landed            the close
      Gnomon arms, chronicle, retrospective
```

Failure modes that decide the branches: step 0's largest component might
be large enough that some crowd pathogen persists somewhere (then the
prediction is written with that count, not zero); step 2's Plague ending
might never fire (spillover too rare or the death-share bar too high) or
fire everywhere — the preregistered count band on Plague endings per world
is what says which, and it is derived from the spillover rate before the
producer is written, not read off the producer after.

**Enrichments adopted (no overturn of the recommendation):** the seven
listed in #1; the framing overturn in #7.

## Task 0 record

`windows/worldgen/tests/suite/murrain_probe.rs`, run once on the Mac
(37.09 s for nine seeds, 2026-09-06) at the branch point `b096b7d50`, over
`hornvale_worldgen::bake_era_graphs` (added in the same commit). The
verbatim summary rows, one per seed; the full per-era curves are in the
probe's output and the spec's §1 carries the table:

```
== seed 1 ==   occupations 1238  peaks: max 85 p90 37 p50 13  bands <10:468 10-24:421 25-49:290 50-89:59 >=90:0
  largest metapopulation EVER      5544  (era 20, 211 occupations)   NOW  5148  (era 23, 227)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:45  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 315  mean occupied-degree 3.82  max 6  isolates 19
== seed 2 ==   occupations 1847  peaks: max 88 p90 36 p50 13  bands <10:601 10-24:816 25-49:349 50-89:81 >=90:0
  largest metapopulation EVER      9992  (era 22, 382 occupations)   NOW  9108  (era 23, 352)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:50  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 491  mean occupied-degree 3.91  max 6  isolates 15
== seed 3 ==   occupations 712   peaks: max 88 p90 43 p50 18  bands <10:190 10-24:244 25-49:235 50-89:43 >=90:0
  largest metapopulation EVER      9807  (era 22, 304 occupations)   NOW  9653  (era 23, 295)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:24  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 290  mean occupied-degree 3.46  max 7  isolates 20
== seed 7 ==   occupations 656   peaks: max 83 p90 36 p50 15  bands <10:215 10-24:223 25-49:184 50-89:34 >=90:0
  largest metapopulation EVER      7819  (era 21, 303 occupations)   NOW  7328  (era 23, 256)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:24  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 251  mean occupied-degree 3.76  max 7  isolates 19
== seed 13 ==  occupations 1061  peaks: max 86 p90 41 p50 12  bands <10:443 10-24:358 25-49:187 50-89:73 >=90:0
  largest metapopulation EVER      5370  (era 22, 223 occupations)   NOW  5047  (era 23, 201)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:37  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 264  mean occupied-degree 3.55  max 6  isolates 21
== seed 42 ==  occupations 1212  peaks: max 83 p90 37 p50 13  bands <10:424 10-24:475 25-49:255 50-89:58 >=90:0
  largest metapopulation EVER      7056  (era 21, 277 occupations)   NOW  6987  (era 23, 274)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:43  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 397  mean occupied-degree 3.84  max 7  isolates 15
== seed 100 == occupations 159   peaks: max 36 p90 23 p50 12  bands <10:62 10-24:84 25-49:13 50-89:0 >=90:0
  largest metapopulation EVER      1460  (era 1, 89 occupations)     NOW  1061  (era 23, 60)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:23  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 60   mean occupied-degree 0.97  max 5  isolates 29
== seed 256 == occupations 1470  peaks: max 86 p90 35 p50 13  bands <10:555 10-24:600 25-49:254 50-89:61 >=90:0
  largest metapopulation EVER      3510  (era 22, 102 occupations)   NOW  3417  (era 23, 94)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:52  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 391  mean occupied-degree 3.94  max 9  isolates 16
== seed 777 == occupations 1049  peaks: max 81 p90 45 p50 14  bands <10:350 10-24:400 25-49:210 50-89:89 >=90:0
  largest metapopulation EVER      6094  (era 22, 215 occupations)   NOW  5509  (era 23, 198)
  (era, component) pairs at or over a threshold, all 25 eras: >=1000:43  >=10000:0  >=100000:0  >=250000:0
  present contact: occupied sites 344  mean occupied-degree 3.77  max 6  isolates 26
```

One correction the run itself forced: the first cut read the LAST era as
the present and found nothing alive in it, because the bake's final era
begins exactly at the end year. "Present" is the last era that begins
before `history-now`. The spec's §1 findings are read from this second run.

## Follow-ups

- **Move `windows/lot/src/shape.rs` down to `domains/history`**, beside
  `occ-person-years`, so the composition root can own the endemic read and
  any other reader of the reconstructed population (ledger #8 i).

- **Household lattice as contact structure** — the second half of
  `BIO-pathogen-as-species`'s "connection graph and the household lattice".
  No household model exists (`SOC-household` is raw), so within-community
  mixing is homogeneous here; noted in both rows.
- **Miasma as appearance** — the inhabitants' theory of disease is a
  phenomenon (bad air, low wet ground, the crowded house), never the
  pathogen: decision 0003's appearances-not-sources, and the cue half of
  `KNOW-evaluative-disease-substrate`. Row to mint.
- **Illness in possession** — a possessed body catching the flux in the
  standing present (`PLAY-deduction-deepens` names illness as playable).
  Row to mint.

## Task 3: complete

- Source and projection commits: `7d09bc989`, `b101e537a`, `d4772c1f5`, `d5cbd71d2`, `587bd4aa6`, `c84867799`, `291d6a4a`, `e6d9af7c5`, `d9420865`.
- Added a draw-free endemic read over committed substrate facts; Lot causes, endings, payloads, JSON/WASM/client projections, and composite-case semantics remain additive and non-causal.
- Closed review findings for moved-life ending occupation, overlapping pathogen provenance, the real Bake→History A/B/A conversion seam, hazard-cause exclusion, and endemic-input sourcing for hazard-pathogen causes.
- Verification: focused Lot/worldgen tests passed; `make lot-check` (`66 passed, 0 failed`); `make gate-commit` (`1433 + 1297 passed`, all audits green). No census or generated artifact drift; only the two protected pre-existing documentation edits remain unstaged.

## Task 4: laboratory instrumentation and predictions

The first invocation of the H-P readout was an invalid instrument pilot, not
a second prediction sample: `largest_metapopulation_at(present_year)` selected
the terminal era marker that begins exactly at `present_year`, so all nine
largest-present values printed `0.000`. The run printed H-P1 PASS, H-P2 FAIL,
H-P3 FAIL, H-P4 FAIL, H-P5 PASS, H-P6 PASS. No model constant or prediction
bound moved. A focused regression was written RED and fixed so the terminal
marker closes the last live era rather than opening a new empty one; the one
canonical valid readout then ran over the unchanged worlds and bounds.

Canonical command:

```text
HV_TEST_OK=1 cargo nextest run -p hornvale-lab --run-ignored ignored-only --success-output immediate -E 'test(murrain_readout::murrain_readout)'
```

Canonical output, verbatim:

```text
seed   1: largest-now=4359.741 crowd-endemic=false consumption-endemic=true plague-endings=4 outbreak-events=69 named-disease-deaths=75/200 slots-filled-mean=18.805/23
seed   2: largest-now=2839.272 crowd-endemic=false consumption-endemic=false plague-endings=4 outbreak-events=22 named-disease-deaths=72/200 slots-filled-mean=18.635/23
seed   3: largest-now=7756.604 crowd-endemic=false consumption-endemic=true plague-endings=1 outbreak-events=16 named-disease-deaths=80/200 slots-filled-mean=18.955/23
seed   7: largest-now=7882.994 crowd-endemic=false consumption-endemic=true plague-endings=3 outbreak-events=18 named-disease-deaths=66/200 slots-filled-mean=18.835/23
seed  13: largest-now=8389.651 crowd-endemic=false consumption-endemic=true plague-endings=4 outbreak-events=16 named-disease-deaths=75/200 slots-filled-mean=18.745/23
seed  42: largest-now=7465.401 crowd-endemic=false consumption-endemic=true plague-endings=3 outbreak-events=29 named-disease-deaths=76/200 slots-filled-mean=19.000/23
seed 100: largest-now=1021.643 crowd-endemic=false consumption-endemic=false plague-endings=0 outbreak-events=0 named-disease-deaths=60/200 slots-filled-mean=18.270/23
seed 256: largest-now=3307.765 crowd-endemic=false consumption-endemic=false plague-endings=8 outbreak-events=28 named-disease-deaths=71/200 slots-filled-mean=19.085/23
seed 777: largest-now=4724.400 crowd-endemic=false consumption-endemic=true plague-endings=2 outbreak-events=4 named-disease-deaths=71/200 slots-filled-mean=18.790/23
H-P1: PASS
H-P2: PASS
H-P3: FAIL
H-P4: FAIL
H-P5: PASS
H-P6: PASS
```

H-P3 and H-P4 are falsified as preregistered: the plague-ending counts are
below 5 on seven of the eight growing seeds, and outbreak-event counts are
below 40 on seven of eight. They remain findings; no post-unblinding retuning
was made.

### Isolated era-derivation cost

The paired process measurement built the same seed-42 `FullView` in both
arms. The control touched its world/terrain/climate ten times. The measured
arm additionally derived the 25 era graphs, authoritative era population,
and era ecological substrates ten times. `/usr/bin/time -lp` output:

```text
control: real 5.57  user 4.17  sys 1.36
paired:  real 9.55  user 11.90 sys 3.07
internal measured loop: isolated-wall-seconds=3.830868 wall-seconds-per-world=0.383087
```

CPU delta is `((11.90 - 4.17) + (3.07 - 1.36)) / 10 = 0.944 CPU-s/world`.
Against the last canonical 1,186 s census and its `cpu_ratio = 30.70`, the
spec's projection is `1,186 + 0.944 × 1,000 / 30.70 = 1,216.749 s`. That is
103.251 s below the 1,320 s alarm and 433.251 s below the 1,650 s refusal.
The delta row is recorded in `docs/timings.md` as
`murrain-era-derivation-delta-10x`.

Verification: focused Murrain Lab tests (5/5), authored/schema fixture tests
(20/20), worldgen substrate tests (2/2), the Lot suite (48/48), generated-path
tests (10/10), `make census-check`, and `make lot-check` (66/66 client tests)
passed. `make gate-commit` passed all audits and three subfloor chunks (1,434 +
1,389 + 1,297 tests). No census was run and no census fixture was changed.
