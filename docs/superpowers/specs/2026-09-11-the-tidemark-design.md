# The Tidemark — the marine realm and its first tenant

**Campaign:** The Tidemark (provisional title).

**Classification:** Architectural continuation campaign.

**Base:** Current `origin/main` at campaign start (`f22860af3`). Takes up
`WAT-sea-peoples` (raw, high), whose structural parent is `MAP-11` (habitat
medium). Predecessors: The Vent and The Living Vent (the Waterworld overlay);
The Underworld, The Delvers, The Radiation and The Range (the non-surface
peopled realm template).

## 1. The one-sentence claim

A people can live in the sea, and the sea can stop supporting them.

## 2. What already exists — measured, not assumed

Every claim in this section is a command's output, taken at `f22860af3`.

**The realm axis already exists in climate, with three realms.**
`domains/climate/src/facets.rs` defines `Realm { medium, access }` with
`OVERWORLD` (`AirOverRock`/`Default`), `WATERWORLD` (`Water`/`Dive`) and
`UNDERDARK` (`Rock`/`CaveMouth`), and `Realm::strata()` gives the water column
five bands: `Epipelagic`, `Mesopelagic`, `Bathypelagic`, `Abyssal`, `Hadal`.

**The environment basis already carries marine communities.**
`domains/climate/src/axes.rs` assigns full vectors to `reef`, `kelp-forest`,
`vent`, `coral-head`, `kelp-canopy`, `smoker-field`, `vent-plume`,
`abyssal-plain`, and an open-water set in which "LIGHT descends the pelagic
ladder".

**The species realm axis is a partial duplicate of climate's.**
`species::HabitatRealm` is `{ Surface, Subterranean }` — the same axis as
`climate::Realm`, missing exactly the realm climate already names. The
duplication is **forced**: layering is constitutional and a domain may not
depend on a sibling, so `domains/species` cannot read `climate::Medium`. The
two can only be reconciled at the composition root.

**The Waterworld overlay has no non-test caller.**

```
$ grep -rn "waterworld_from(" --include=*.rs .
windows/worldgen/tests/suite/waterworld.rs:51      <- test
windows/worldgen/tests/suite/waterworld.rs:333     <- test
windows/worldgen/tests/suite/waterworld.rs:1326    <- test
windows/worldgen/src/waterworld.rs:405             <- the definition
```

`observe_waterworld`, `observe_waterworld_snapshot` and every construction of
`WaterWorldConfig` are the same: test-only. No `BuildDepth` rung constructs the
overlay, no CLI command reaches it, no window renders it.

**A marine people is already shipped, at the shelf.** `sea-elf` is a settling
people with `SWIM` locomotion, a marine-dominant resource axis and an authored
`(depth, SST)` biome affinity, and it is `Surface` by absence from
`habitat_realm_registry`. Its own rows state the reason: "a settled coastal
people does not live entirely in the water", so "it sleeps ashore, on what it
built. Not `ALREADY_BUOYED`." So `WAT-sea-peoples`' "elves" is partly shipped,
and what is missing is the **obligate** kinds, not a marine people as such.
Note that these rows constrain sea-elf's *residence*; §3.8 separates that from
its reach, which this campaign extends into the column.

**The Delvers' blocker does not bind here.** `habitat_realm_registry`'s own
comment records that two subterranean peoples were added and withdrawn because
"a kind whose identity is DEPTH cannot be expressed by an axis measured in
metres above sea level … They return when the underworld has biomes." The
marine realm already has biomes, strata and an access mode; that condition is
satisfied on arrival.

**The code anticipates a third realm.** `substrate_response`'s doc: "a third
realm variant stops the compiler here rather than falling into a default that
would silently score it as surface."

## 3. Design

### 3.1 `HabitatRealm::Marine`

A third variant. Per its own doc the `match` in `substrate_response` is
exhaustive with no wildcard, so **the compiler enumerates the sites** rather
than the spec guessing at them; the implementer reads the error list. The
registry stays sparse (absence means `Surface`).

`substrate_response(Marine)` follows the subterranean rationale one step
further. Subterranean devotion is 0.8 because "the habitat is the void and not
the floor"; for a marine kind the habitat is the water column and the seabed is
less accountable still, so devotion is lower again. The implementer authors the
three constants against that argument and states it in `plumb:` prose, as the
existing two do.

### 3.2 The availability mask

`per_species_suitability_masked` already carries `species_realm: &[HabitatRealm]`
and gates with an `availability` term that is a presence mask in `{0.0, 1.0}`,
deliberately outside the Liebig minimum because it is not a tolerance. Surface
is always `1.0`; Subterranean is `1.0` where any habitable rung has a cave and
`0.0` otherwise.

Marine mirrors it exactly: `1.0` where the vertex holds a water column, `0.0`
otherwise. The subterranean field is hoisted unconditionally and read only for a
`Subterranean` kind — "the derivation is pure, so this costs one map and no
draws" — and the marine field is built the same way.

### 3.3 Seating: the pelagic ladder is the delve ladder

The subterranean arm scores each rung of `Band::habitation()` and takes the
best. The marine arm scores each of `Realm::WATERWORLD.strata()`'s five bands
and takes the best. This is the campaign's structural bet: **the two ladders are
the same construct at different realms**, which `Stratum`'s own doc already
asserts ("the pelagic zones and (later) the underworld's geological layers are
the same construct at different realms, rather than two parallel ones").

### 3.4 Six marine peoples, and the rule that keeps them distinct

**The rule first, because it is the one the predecessors failed.** The Delvers
authored two subterranean peoples and withdrew them: "a kind whose identity is
DEPTH cannot be expressed by an axis measured in metres above sea level".
`habitat_realm_registry` states the general form — "the trap is not authoring a
subterranean kind, it is distinguishing two kinds by DEPTH, which nothing in the
model can say."

So this campaign's constraint, and it is testable rather than hortatory:

> **No two marine kinds may differ only by stratum.** Every pair must be
> separable on at least one axis the model already carries.

That is what makes six kinds legitimate where the Delvers' two were not, and it
is measured as M5 rather than asserted.

**The axes available**, all shipped: `SocialForm` {Sessile, Solitary,
Gregarious, Settled}, `Sociality` {Hierarchic, Communal}, `StatusBasis` {Rank,
Knowledge, Generosity}, `ActivityCycle` {Diurnal, Nocturnal, Crepuscular},
`ThermalStrategy` {Endothermic, Ectothermic, + the unmodelled third},
`TrophicMode` {Heterotrophic, Phototrophic, Chemotrophic}, `LifeSchedule`
{Allometric, Paced{factor}}, plus `MindVector`, `SocietyVector`,
`PerceptionVector`, `Dispersion`, `Locomotion` and the `EnvironmentNiche`
vector. `ActivityCycle::Crepuscular` is an explicitly reserved empty slot —
"idle this campaign; authored now so a future species is a data change" — so
filling it is invited.

**The slate.** The implementer authors the values; this table fixes the
*discriminating* assignments, and every kind is `HabitatRealm::Marine`.

| kind | SocialForm | Trophic | Thermal | Cycle | Status | the axis that makes it not-another |
|---|---|---|---|---|---|---|
| **triton** | Settled | Hetero | Endo | Diurnal | Rank | the baseline: a hierarchic, martial people of the column |
| **merfolk** | **Gregarious** | Hetero | Endo | **Crepuscular** | Knowledge | settles nothing — follows the shoals |
| **abyssal elf** | Settled | Hetero | **Ecto** | Nocturnal | Rank | the aquatic drow (below); `LifeSchedule::Paced` slow |
| vent commensal | Settled | **Chemo** | Ecto | Nocturnal | Generosity | the only chemotroph; its habitat expires (§4) |
| kelp tender | Settled | **Photo** | (unmodelled) | Diurnal | Generosity | a photosynthetic people, on `treant`/`shrieker` precedent |
| reef mason | Settled | Hetero | Ecto | Diurnal | **Knowledge** | builds substrate; narrow `Dispersion`, high site fidelity |

**The abyssal elf is the drow move, repeated.** Drow's *only* authored
separation from the surface elves is the realm gate — "Drow's `elevation`
response is wood-elf's byte for byte, for exactly that reason" — because the
trap is distinguishing two kinds by depth. The abyssal elf is separated from the
elf family by `HabitatRealm::Marine` and its deep-band niche, and by nothing
else it inherits. That makes it the slate's best-precedented member rather than
its most speculative, and it carries the same hazard drow did not: the deep
bands are the *large* ones, so its held-vertex count needs measuring before it is
safe (M7).

**Merfolk carry the `Gregarious` slot**, so they are a minded people that forms
no fixed place — the `SocialForm` doc's own distinction ("a nomadic band is
social without being sedentary", decision 0068) getting its first marine
instance. Placement must handle a people that settles nothing, which is M2's
two-sided prediction.

`sea-elf` remains `Surface` and is the seventh member of the contrast set, not a
seventh marine kind (§3.6).

### 3.5 The two-way agreement test

New, and the part with no precedent: a test at the composition root pinning
`species::HabitatRealm` against `climate::facets::Realm` as a **bijection**.
Both directions, because a one-directional check is structurally blind to
over-admission and still reads as total. This is what stops the forced
duplicate from drifting, and it is what makes the aerial realm cheap later.

### 3.6 `sea-elf` stays `Surface` in residence, stated rather than defaulted

`HabitatRealm`'s registry is sparse and absence means `Surface`. That default is
correct for every kind today, and it becomes load-bearing the moment a third
variant exists — because the roster **already ships a marine people**.

`sea-elf` is a settling people, carries `SWIM`, is marine-dominant on its
resource axis, and has an authored `(depth, SST)` biome affinity on the shelf.
It is `Surface` today only by absence from the registry, and its own rows say
why that is right rather than accidental: "a settled coastal people does not
live entirely in the water", so "it sleeps ashore, on what it built. Not
`ALREADY_BUOYED`." Its terrestrial residue is the shore it builds on.
`giant-crocodile` is the same question in fauna form — "the roster's stated
amphibious case — land-dominant at 0.6 `ANIMAL_PREY`".

A reader meeting a new `Marine` variant will reasonably assume a sea elf belongs
to it. Acting on that assumption strips the kind of its land habitat outright,
because the availability mask is `{0.0, 1.0}` and has no middle value for a life
lived across two media — which the project solved for these kinds with **niche
weights, not with a realm**, and this campaign does not disturb that.

So both classifications become **explicit and tested**, in the shape
`environment_niche_registry` already uses for its own absence ("absence is
load-bearing, and it is the campaign's positive control").

**This also settles what the new kinds are for.** They are not "a marine
people" — there is one. They are the **obligate** kinds: those that cannot come
ashore, at the far end of the scale from sea-elf's shelf. Sea elves reach their
waters (§3.8); they do not hold them. The realm gate is precisely the
separation between them, and unlike depth it is something the model can say.

### 3.7 The subsistence roster

Six peoples need something to eat, and the roster is thin: the only marine kinds
today are `giant-octopus`, `giant-squid`, `killer-whale` and `reef-shark` — two
cephalopods, a cetacean and a shark, which is a predator guild with no base
under it.

**The line between a named kind and an aggregate stock.** `WaterStocks` already
carries `plankton`, `chemosynthetic_bloom`, `nutrients` and `kelp_reef` as
aggregates over every substrate sample. Those stay the *base*, and this campaign
does not convert them into organisms. A **named kind** is authored when a people
interacts with it as a thing — eats it, competes with it, is eaten by it, or
builds from it. Everything below that line remains a field.

The roster the six peoples require, by trophic position:

- **Primary producers:** a kelp (Sessile, Phototrophic) and a reef-building coral
  (Sessile) — the two that turn `kelp_reef` suitability into something standing.
- **Grazers and filterers:** a bivalve/mollusk bed and an urchin-analogue
  grazer, the pair that makes `urchin-barren` (already a name in the environment
  basis) a reachable state rather than a label.
- **Forage:** a schooling fish — the missing middle of the existing web, and
  what a `Gregarious` pelagic people actually follows.
- **Vent fauna:** a tube-worm analogue (Sessile, Chemotrophic) and a vent
  scavenger, so the vent commensal has a community rather than a bare chemical
  gradient.
- **Scavengers:** a detritivore that closes the loop at depth.

`SocialForm::Sessile` is exactly the slot for the rooted kinds — "Rooted; placed
on the map, never agentified (autotrophs)" — and `treant`, `shrieker` and
`twig-blight` are the terrestrial precedent for flora as kinds.

**The web must close**, and that is M6: every marine people's subsistence must
resolve to a named kind or an aggregate stock, with no dangling requirement.

### 3.8 Reach is not residence

The availability mask is `{0.0, 1.0}` and has no middle value, so "amphibious"
cannot be said as a realm. It can be said as a **capability**, and the project
already has the vocabulary: `Locomotion::SWIM` — "the two requirements a
plan-side gate can ask a body to satisfy" — and climate's `Access::Dive`,
defined as "simply being in the overworld, or diving into water".

This campaign therefore separates two things that a single realm field was
conflating:

- **Residence** — the vertices a kind *holds*, which placement and carrying
  capacity read, and which the coexistence balance governs.
- **Reach** — where a body can *go*: dive to, breathe in, and meet others in.

`sea-elf` is the case that forces the distinction. It already carries `SWIM`,
and it is **deliberately confined to the shelf band** for a measured reason:
`radiation_affinity::the_sea_elf_is_confined_to_the_shelf_band` pins four shelf
classes above the default and five deep classes at or below it, because
"authored to the whole ocean this kind would hold ~27,000 vertices against
wood's ~800; on the shelf band it holds ~1,425". That is MAP-22's
competitive-exclusion problem, measured and pinned.

So sea elves are **truly amphibious in reach and shelf-dwelling in residence**:
they dive the column, breathe water, and meet tritons, merfolk and abyssal elves
where those peoples live, while continuing to hold the shelf and sleep ashore on
what they built. The pinning test stays green, untouched, and the ~34x dominance
it prevents does not return.

**The rule this generalises to**, stated because the aerial realm will need it:
a kind's realm answers where it *lives*, never where it *can go*. Any future
question of the form "but it can travel there" is a locomotion and access
question, not a realm one.

## 4. The expiring habitat

This is what the campaign has that a port of the underworld template would not.

A cave does not cool and clog. A vent does: `VentState` runs `Absent → Nascent →
Active → Weakening → Failed`, the selected source may migrate within a fixed
candidate ring, and `Failed`'s doc records that "the source remains identifiable
after its contribution has failed". So the marine realm is the **first realm
whose habitat quality is time-varying**, and a settlement seated on a vent is
seated on something that can end.

The history domain already has the vocabulary: `OccupationRecord.cause` carries
`Famine`, `Burned`, `Plague`, `Fled` and `Migrated`. A vent entering `Failed`
under an occupied vertex should produce an occupation that ends with a cause,
not a settlement that silently persists on a dead vent.

**Scope limit.** This campaign makes vent phase an *input to seating and an
ending*. It does not model dispersal, larval recruitment, or a marine population
dynamic; see §6.

## 5. Wiring the overlay

`waterworld_from(world, terrain, climate, config)` needs exactly what the
`Settlements` rung of `build_to` already holds — `world`, `terrain` and
`climate` are all live there, and the rung returns at
`if depth <= BuildDepth::Settlements`. The overlay is constructed inside that
rung, before placement reads it, and handed on to `Full`.

`WaterWorldConfig { enabled }` stops being a test-only knob. Whether it survives
as a config at all, or the overlay simply builds whenever the world has marine
vertices, is an implementer's call to make against the rung's existing shape.

## 6. Determinism and save format

**This is additive, and that is verified rather than argued.**

`Seed::derive(label)` is FNV-1a over the label's bytes mixed with the parent,
so **every stream label is independently keyed**. A draw on one label cannot
perturb another's sequence.

`WaterVent` admission derives a *per-vertex sub-stream* —
`seed.derive(WATERWORLD_VENT).derive("vertex/<n>")` — then takes admission,
strength, temperature delta and chemistry from it in that order. Because the
sub-stream is keyed by vertex, vent admission at one vertex cannot shift draws
at another.

`WATERWORLD_VENT = "waterworld/vent/v1"` is **already** a registered label in
`windows/worldgen/src/streams.rs` and already appears in the committed stream
manifest. The Vent paid that cost; this campaign adds no stream label.

Consequently: no epoch suffix, no save-format break, and no reordering of any
existing stream's consumption. What *does* move is every artifact whose content
depends on placement, if a marine people places — which is the intended
observable and is handled by rebaseline, not by an epoch.

**The one open determinism question**, flagged for G3: seating reads a vent's
phase, which is a function of `WorldTime`. Placement must therefore name the
instant it reads. The campaign's position is that this is a pure read at a named
tick and consumes no draws; the implementer verifies it rather than assuming it.

## 7. What is deliberately NOT in this campaign

- **The underworld's trophic half stays THE TENANT's.** This campaign takes the
  *marine* half of the larder metaplan's rung 4 only: the vent commensal weights
  `CHEMOSYNTHATE` and consumes `marine_chemosynthate_supply_field`, which today
  reaches no consumer at all. No underworld kind changes, no `Surface`-realm kind
  gains the weight, and The Winze's unruled symmetric-budget question (metaplan
  §"Rung 3") is untouched. Rationale in ledger #5: the marine supply is shipped
  rung-2 work, so its consumer was queued behind an underworld ceiling it never
  reads.
- **No species, metabolism, reproduction or per-organism ecology.** Explicit
  non-goals in The Vent and The Living Vent; unchanged here.
- **No aerial realm.** `MAP-11`'s third medium stays empty. The agreement test
  in §3.5 is what makes it cheap later; building it now would be speculative.
- **No reef fragmentation, current networks or signal distortion.**
  `WAT-reef-fragmentation` and `WAT-signal-distortion` stay raw.

## 8. Preregistered measurement

Frozen before the code that would move it (decision 0016). Each states both
poles, so neither outcome is a failure.

**M1 — Do the two vent representations coincide?** There are two today:
`climate::Biome::HydrothermalVent` ("a hydrothermal-vent field on a spreading
ridge" — derived, no draw, and the one the live suitability path reads through
`marine_chemosynthate_supply_field`) and worldgen's `WaterVent` (a seeded
admission over seabed samples at threshold 0.25, carrying identity, strength and
succession phase). Report `|A|`, `|B|` and `|A ∩ B|` as **counts** at seeds 42,
7 and 3.

- *Prediction:* they do not coincide — the intersection is under half of the
  smaller set — because one is ridge-derived and the other is a seeded draw over
  seabed vertices.
- *If the prediction holds:* they are different phenomena and must be named
  differently; seating reads `WaterVent` (it is the only one carrying phase) and
  the campaign records that the biome is a separate, coarser thing.
- *If it fails* (large overlap): they are redundant representations of one
  phenomenon, and one must be derived from the other rather than both surviving.
  That is a larger finding than this campaign, and it is reported as the
  headline rather than absorbed.

**M2 — Do the marine peoples place, and do merfolk refuse to?** Count
settlements **per marine kind** at seed 42 at `BuildDepth::Full`.

- *Prediction, two-sided:* each of the five `Settled` kinds places at least one
  settlement and fewer than the surface total; `merfolk`, being `Gregarious`,
  place **exactly zero**, because a kind that forms no fixed place must not
  form one.
- *A zero for a `Settled` kind* means the realm gate admits nothing for it and
  that kind has shipped unreachable. *A non-zero for merfolk* means
  `SocialForm` is not reaching placement, which is a defect in the opposite
  direction and would be invisible to a one-sided floor.
- *Exceeding the surface total* means the marine realm is outcompeting land — a
  placement defect, not a success.

**M3 — Does the habitat expire?** Over a world-time sweep at seed 42, count
vertices whose marine availability is non-zero at one instant and zero at a
later one. The prediction is a non-zero count; a zero count means vent phase
reaches seating in name only, and would falsify §4 — the campaign's headline —
rather than merely underperforming.

**M4 — Do the two straddling kinds keep their land habitat?** Count the
vertices at which `sea-elf` and `giant-crocodile` each have non-zero
availability, before and after `HabitatRealm::Marine` exists, at seed 42.

- *Prediction:* each kind's two counts are **equal** — `Surface` by absence
  before, `Surface` by explicit statement after, so nothing moves.
- *If either pair differs at all*, the new variant has silently reclassified a
  shipped kind, which is the defect §3.6 exists to prevent, and the difference
  names its size. For `sea-elf` this would be visible as a settling people
  losing the shore it builds on.

**M5 — Are the six kinds actually distinct?** For all 15 pairs of marine kinds,
count the model-carried axes on which the pair differs, **excluding stratum**.
Report the per-pair count and the minimum over all pairs.

- *Prediction:* the minimum is at least 1 — no pair is separated by depth alone.
- *If the minimum is 0*, that pair is the Delvers' defect reproduced, and the
  remedy is to merge the two kinds or re-author one, not to argue the depths are
  far apart.

**M6 — Does the subsistence web close?** For each of the six peoples, resolve its
subsistence to either a named kind in the roster or an aggregate `WaterStocks`
field. Count dangling requirements — demands that resolve to neither.

- *Prediction:* zero dangling.
- *If non-zero*, the count names exactly which kinds the roster still owes, and
  the campaign either authors them or moves the demand to an aggregate; it does
  not ship a people that eats something the world does not have.

**M7 — Does the abyssal elf dominate the map?** Count the vertices the abyssal
elf holds at seeds 42, 7 and 1234 — the same three seeds the sea-elf confinement
was measured over — and compare against the elf family's order (wood-elf ~800,
shelf-confined sea-elf ~1,425).

- *Prediction:* the same order as the family, hundreds to low thousands.
- *If it is an order larger*, the deep bands have handed a single kind the map
  the way the whole ocean would have handed it to sea-elf, and the remedy is the
  one that worked there — band confinement with its own pinning test — not a
  tuning of its niche weights after the fact.

**Negative control for M3:** the same sweep with vent phase held constant must
produce a count of zero. Without it, M3's non-zero count could come from any
time-varying term in the stack.

## 9. Open questions for G3

1. **The instant placement reads** (§6). Naming a tick is a determinism-contract
   call and leads this list for that reason.
2. **Which vent representation is authoritative** (§8, M1) — the campaign
   proposes `WaterVent` and measures before committing.
3. **Is the slate in §3.4 the right six?** Tritons, merfolk and abyssal elves
   are yours; the vent commensal, kelp tender and reef mason are functional
   roles that have not earned names yet. The assignments satisfy M5 but are not
   determined by it. The pick I would defend least confidently is the
   `Phototrophic` kelp tender — precedented by `treant` and `shrieker`, but
   never before as a *settling* kind.
4. **The name.** "The Tidemark" is provisional; the branch is `campaign/the-tidemark`.
5. **The reading of the row** (ledger #2): residents of the water column, not
   Bronze Age Collapse raiders.
