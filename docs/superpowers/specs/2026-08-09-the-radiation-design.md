# The Radiation — six elves, and the last campaign in the peoples programme

**Campaign:** C2d, The Radiation. The final campaign of the peoples programme
(`2026-08-03-the-peoples-program-design.md` §4). Nine peoples become fifteen.

**Status:** spec, awaiting G3.

---

## 1. What this campaign is

Six elves: **Wood**, **High**, **Drow**, **Sea**, **Desert**, **Snow**. One
family label carried by six kinds — the largest the roster has held, against
goblinoid's three and dwarf's three.

It is the programme's last campaign for a structural reason rather than a
scheduling one. Three campaigns ran ahead of it to build mechanism the elves
need and could not have been authored without:

- **The Deep Realm (C2a)** gave the underworld a realm and a chamber graph.
- **The Warren (C2w)** gave a kind an authored `HabitatRealm` and gated a
  subterranean kind on whether a cell holds a cave at all — the mechanism Drow
  stands on.
- **The Range** carried that gate to the path that actually decides worlds, and
  added `BiomeAffinity`: a kind may now declare which biomes it belongs in, in
  the vocabulary the model already computes. Its closing sentence names this
  campaign: *"The elves are the point of all this and are not in it."*

The Delvers (C2c) authored the dwarves and, in the middle of doing so,
withdrew two of them. That withdrawal is inherited here unchanged (§7).

**This campaign authors kinds. It builds no new mechanism**, and that is the
first thing about it worth stating: every one of the six is expressible today.
Where a trait is *not* expressible — depth below the surface, time since a
split — the spec says so and does not author a proxy for it.

## 2. What it inherits, and the two premise-checks already paid

### 2.1 A Sea elf needs no new mechanism

Measured on seed 42, before any design was written:

```
  killer-whale  ocean non-zero 29896/29896   land 0   best 61.25 (upwelling)
  giant-squid   ocean non-zero 29896/29896   land 0   best 63.31 (upwelling)
  reef-shark    ocean non-zero 29896/29896   land 0   best 66.42 (upwelling)
  human         ocean non-zero     0/29896   land 11010
```

The capacity machinery is symmetric: a marine-weighted kind gets real capacity
on every ocean cell and a terrestrial kind gets none. **And there is no land
gate anywhere in settlement placement** — established by reading the chain
rather than assuming it. Genesis `admissible` is `Bake::factor(era, c) > 0.0`;
`Bake::factor` is `if era.ice { 0.0 } else { 1.0 }`; and that function's own
doc says `era.ice` is "all-false on every production path". The Tilth removed
the species-blind habitability mask deliberately — *"that land is now settleable
by whichever people can actually feed itself on it, and by nobody else."*

The whole Sea-elf design rests on this one fact, which is why it was measured
rather than reasoned. The Range spent an hour on a confident causal story that a
single premise-check dissolved.

### 2.2 The scale worry was misframed, and the correction is the roster's argument for balance

The campaign's own brainstorm flagged a leading risk: **the ocean is 2.7× the
land** — 29,896 ocean cells against 11,066 — so a sea people would open a
habitat larger than everything the other nine peoples compete over combined.

That number is right and the conclusion drawn from it was wrong, because it used
the wrong denominator. **No elf gets all the land.** The unit that matters is
per-elf habitat, and it was measured over three seeds:

```
  SHELF OCEAN   coral-reef 575 + kelp 388 + upwelling 299 + epipelagic 163
                                                          =  1,425   <- Sea
  deep ocean    bathypelagic 9,952 + sea-ice 8,962 + mesopelagic 5,997
                                                          = 25,640   (not authored)

  temperate-forest                                        =    788   <- Wood, High
  desert                                                  =    241   <- Desert
  tundra + ice                                            =  4,633   <- Snow
  caves (~12 % of 13,894 land cells/seed)                 = ~1,667   <- Drow
```

On the productive shallow band Sea gets **1,425 cells — in family**: larger than
Desert, smaller than Snow. The roster comes out naturally balanced, **241 to
4,633**, a 19× spread with no outlier. The runaway exists only if Sea is
authored to the whole ocean, and it is not (§3.4).

This is recorded as a correction rather than quietly fixed, because the error
shape recurs: The Range's own cost estimate multiplied a world-build by 1000
against a pass that had already paid for it. A real number with the wrong
denominator reads exactly like a finding.

### 2.3 One authored row moved every people, mechanism unestablished

The Range declared a biome affinity on **one** kind and measured the
consequence:

```
  seed 7    total 274 -> 287     gnoll  4 ->   4   (unchanged)
            bugbear 49 -> 153    goblin 48 ->  7    kobold 38 ->  5
            hill-dwarf 2 -> 28   desert-dwarf 51 -> 23
  seed 1    total 265 -> 184     gnoll 61 ->  13
  seed 42   total 145 -> 143     gnoll 20 ->   2
```

The kind whose row was declared does not move on seed 7 at all, and bugbear
triples. Attribution was *proven* — a test rebuilds three seeds with and without
the second occupant's row and asserts the complete list of (people, cell)
placements is identical — but **the mechanism of the cascade was deliberately
not narrated**, because it was not measured.

**This campaign declares six such rows and adds six competitors.** That is the
largest single perturbation to the settlement contest the roster has ever taken,
and it is carried into §9 as the campaign's leading risk rather than discovered
at its close.

## 3. The roster and its authoring

### 3.1 One route: biome affinity, for all six

**Every elf is authored on the biome-affinity route.** Concretely: each elf's
`ConditionNiche` carries an **elevation devotion below its sovereignty floor**,
so the climate curves are computed and then discarded by the Liebig minimum on
every cell, and the authored `BiomeAffinity` carries the differentiation
outside the minimum, where nothing can discard it.

**Never both routes on one kind.** That is the double-count The Range's §3.1a
forbids: a climate curve fitted to approximate a biome *plus* an affinity naming
that biome applies the same preference twice, and the campaign would measure the
sum while attributing it to the affinity. Attribution is this programme's entire
product.

**Satisfiability, checked before the strategy was fixed.** The sovereignty floor
is `0.95 · (1 − exp(−(0.15 · ln m + 1.0 · p)))` (`kernel/src/ecology.rs:287`).
For a plausible elf mass at potency 0:

```
  45 kg   floor 0.4133
  70 kg   floor 0.4477
```

The dwarves' authored elevation devotion of **0.30** clears both with margin, so
the route is available at any elf mass the roster would plausibly draw. The plan
fixes the numbers; the spec fixes only that they must satisfy
`elevation.devotion < sovereignty_floor(mass, potency)` **per elf**, asserted in
a test as The Range did for gnoll and woolly-mammoth, so that a future edit to a
mass cannot silently turn a row into a double count.

**Why the family is not split across both routes for a contrast.** The Delvers
already authored the climate-curve arm and measured it: it binds on 67–91 % of
land and buys almost no separation. That contrast exists in merged work. **Do
not author a people badly to manufacture a control** — the control is already on
the shelf.

### 3.2 The vocabulary, and where it is keyed

`BiomeAffinity` is a sparse `ComponentStore<KindId, BiomeAffinity>` in
`domains/species` — absence means unrestricted — keyed by the biome's **stable
name string**, not by `Biome`, because a domain crate may not depend on a
sibling domain; resolution against the live enum happens at the composition
root. It is a mask in `[0, 1]`, applied *outside* the four-way minimum beside
The Warren's `availability`. Two properties of it are load-bearing here:

- **A uniform affinity is a placement no-op.** The bake's contest ranks each
  people's pool in that people's own units, so a scale-free ranking cannot be
  reordered by a constant. Only the *shape* across biomes carries information;
  the level is gauge.
- **`0.0` is a hard exclusion, not a strong preference**, because the genesis
  pool filters on strictly positive capacity. Any zero in an elf's row is a
  deliberate act.

The authoring instrument is already committed:
`windows/worldgen/tests/fixtures/occupancy.csv`, the per-kind per-biome record
of where each roster kind actually lives (386 rows today). The Range threaded it
with the live affinity store precisely so the elves would be authored against
measured occupancy rather than against intuition. It grows by six kinds here.

### 3.3 The six

| kind | realm | stronghold biomes | differentiation carried by |
| --- | --- | --- | --- |
| Wood | surface | temperate forest | affinity (the ancestor; the proto sits at it) |
| High | surface | **temperate forest — same as Wood** | psyche / society / language only |
| Drow | **subterranean** | surface biomes as Wood's, unchanged | the realm gate alone |
| Sea | surface (marine cells) | coral reef, kelp forest, upwelling, epipelagic | affinity |
| Desert | surface | desert (savanna/shrubland as secondary) | affinity |
| Snow | surface | tundra, ice (taiga secondary) | affinity |

Numbers — the per-biome factors, the masses, the four scalar devotions — are
**not fixed here**. The spec fixes the strategy and the constraints; the plan
and its measurements fix the values.

### 3.4 Sea takes the productive shallow band

Coral reef, kelp forest, upwelling, epipelagic. Not the whole ocean.

This is ecologically right rather than merely convenient: a *settled* people
needs shallow productive water, which is also why human settlement is coastal.
And it reinforces a gradient the model already has instead of inventing one —
`marine_forage_supply_field` grades productivity straight off the biome class
(`Upwelling => 1.0`, `CoralReef | KelpForest => 0.85`, `Epipelagic => 0.45`,
`Mesopelagic => 0.15`, `Bathypelagic => 0.05`, `Abyssal | HadalTrench => 0.02`).
The affinity sharpens that ranking; it does not contradict it.

Sea also escapes the trap Drow sits in, for a precise reason worth stating:
**in the ocean, depth is −(height above sea level)**. The two quantities
coincide, so an elevation curve can honestly say "shallow". Killer whale
(−40 m) and giant squid (~−1263 m) are the authoring precedent. On land they do
not coincide, which is the whole of §3.5.

### 3.5 Drow: the realm gate alone, and a dormant half

Drow is authored **coarsely and deliberately**, as one cave kind whose only
required separation is from *surface* elves. The realm gate does that, and does
it measurably now that The Range carried it to `per_species_capacity_at`.

**No depth is encoded in Drow's elevation curve.** Depth below the surface and
height above sea level are different quantities: a deep chamber under a mountain
sits high above the sea, a shallow cave in a marsh sits low. The Delvers
committed that fake and caught it — duergar authored at a 300 m optimum to mean
*deep* selected lowland marshes, and its toponymy came back as an emergent
finding until one question dissolved it. **The toponymy was reporting the
authoring.** Drow will not repeat it.

**Drow's dark adaptation is authored and dormant, and this is preregistered, not
discovered.** The Warren measured that going underground improves a kind's
moisture (.585 → .787) and insolation (.467 → .840) readings and that the Liebig
minimum never sees the improvement, because the unfloored elevation axis is
scarcer. Generalised: *a non-lethal preference cannot matter while an unfloored
axis is scarcer.* So a cave-dark insolation preference on Drow contributes
**nothing** to its placement today. It is authored anyway — it is true of the
kind, and the two-tier gate/modifier tolerance that would make it bind already
exists in shadow mode (The Tense §3.3), with a tripwire in
`windows/worldgen/tests/warren_readout.rs` that reddens on purpose the day it
starts to bind.

**The cost, stated rather than buried:** Drow is being authored in a frame a
later campaign will change. That is exactly the argument that inserted The
Warren before The Delvers and The Range before this one, and it is accepted here
on Nathan's direct call, for two reasons: Drow's separation from surface elves
is real and measurable *today*, and the project already has the idiom for an
authored-but-dormant trait guarded by a tripwire.

### 3.6 High: the family's only social divergence

High diverges from Wood in **mind and society, not in environment**. It is the
deliberate null control, and it gives the family two clean single-variable
contrasts:

```
  Wood vs High   isolates MIND     (same realm, same biomes)
  Wood vs Drow   isolates REALM    (same biomes, different realm)
```

A roster of six kinds each differing on several axes at once measures nothing.
These two pairs are the reason the roster is legible.

### 3.7 The authoring cost per kind

Eight registries plus an appended accession cohort, and a validation in a ninth
— the count The Delvers corrected against the `human` and `gnoll` commits:

```
  domains/species/src/lib.rs          biosphere, psyche, dispersion, society,
                                      perception, family_of, KIND_CONCEPTS,
                                      habitat_realm (sparse, Drow only),
                                      biome_affinity (sparse, five of six),
                                      + a condition-niche fn
  domains/language/src/lib.rs         articulation, lexicon, family_proto ("elf")
  domains/language/src/accession.rs   an APPENDED epoch cohort
  windows/worldgen/src/components.rs  check_integrity validates, authors nothing
```

Two of these have teeth:

- **`family_proto` for `elf` is mandatory**, because a proto is required for any
  label carried by ≥ 2 kinds and this one is carried by six.
- **The accession cohort is the item most easily missed and the one with
  consequences.** Omitting it changes which proto-root a concept draws
  (commit `ee4e6a00`); `cli/tests/accession.rs` reddens if it is skipped. It is
  a save-format event, not a documentation slip. **Append a cohort; never edit
  one.**

**Landing order is not free.** Landing a biosphere row alone makes `assemble()`
hard-fail workspace-wide ("a Settled kind is missing a peopled component"), so
species and language must land in one commit per kind or per cohort. The
pre-commit hook runs `make quick` workspace-wide regardless of staged paths, so
this is not something a plan can split.

## 4. Per-elf visibility, preregistered

**Which channel each elf's identity is observable in is stated here, before
measurement.** This is the probe-validity ladder applied per-kind instead of
per-campaign, and it exists because without it two of the six read as defects:

```
  Wood / Desert / Snow / Sea   PLACEMENT. The biome affinity moves where they
                               live; the settlement distribution is the probe.

  Drow                         PLACEMENT, via the REALM GATE (~1,667 cells).
                               Its dark-adaptation authoring is DORMANT and
                               contributes nothing — by measurement, not by
                               omission. Guarded by the Warren tripwire.

  High                         NOT PLACEMENT, by design. Its identity lives in
                               psyche, society and language facts only. A
                               placement readout is the wrong instrument for it
                               and will correctly show nothing.
```

Written down in advance, **High is a control**. Discovered afterwards, High is a
failed elf and Drow's dormant half is a bug. The difference between those two
readings is this table and its date.

## 5. Preregistered predictions

Frozen before the code that would move them. A falsified prediction is a
finding; nothing here is retuned after unblinding without saying so in the
chronicle.

### P1 — the rung-5 test: declaring the six moves the committed world

**Axis:** the sha256 of seed 42's committed world (and two further seeds),
before and after the roster lands. **Falsifier:** byte-identical.

This is the programme's rung-5 obligation (metaplan §3, the rung The Range
added), and it is stated as an obligation rather than as a discovery. It should
pass almost by construction — six new `SocialForm::Settled` kinds enter the
bake's roster and compete — and it is recorded so a later reader does not
mistake it for evidence of anything beyond wiring.

Its failure mode is nonetheless real and specific: The Range's commit 1 was
byte-neutral on the shipped roster because the bake filters to `Settled` and
both `Subterranean` kinds are fauna. If P1 falsifies, some part of the assembly
is not admitting the elves at all, and the campaign stops there.

### P1′ — the per-elf rung-5 test

**Axis:** for each of the six, the committed world with that elf's rows present
against the same world with them removed. **Prediction:** *each* elf's presence
changes the committed world on at least one of three seeds. **Falsifier:** some
elf is byte-neutral in both directions — which means that kind is authored and
inert, rung 2 on that kind, and it is a finding about that kind rather than
about the mechanism.

This is the discriminating half of P1, and it is where the roster's two riskiest
kinds are actually tested. Drow is confined to the ~12 % of land holding an
enterable cave and must still clear the roster floor with no allowlist — the
`non_void_roster` risk The Delvers named for two kinds, restated here for one.
Sea competes on water nobody has ever settled.

### P2 — each elf concentrates in its authored biomes

**Axis:** the **share** of that elf's settlements sited in its authored
stronghold biomes, against the same elf's share in an arm with its affinity row
absent, on three seeds. **Falsifier:** the share is flat or falling while the
count also falls, on a majority of seeds.

The share is preregistered rather than the count, following The Range's P1″: a
falling count with a rising share is *success* (relocation), and both falling is
the failure mode. Reported per elf, never pooled — pooling six kinds would hide
exactly the per-kind result §4 exists to make legible.

**High is exempt by design** (§4). It has no stronghold and is predicted to show
no concentration; that is not a falsification of P2.

**A pre-committed diagnosis, so it is a finding and not a retune.** If P2
falsifies for an elf whose habitat is small and contested, the diagnosis is The
Range's P1‴ — a downward-only mask can suppress without relocating — and the
repair is an affinity permitted above 1.0 or complementary occupants vacating
the destination, **not** a retuned constant. The Range pre-committed that repair
and did not need it; if this campaign needs it, that is the first evidence for
it and it is recorded as such.

### P3 — Wood and High do NOT separate in placement (the null control)

**Axes, two of them, because they can disagree:**

- (a) **Capacity fields.** Wood's and High's per-cell capacity fields over land.
  **Prediction:** bit-identical, or (if their biosphere rows differ in mass or
  potency, which moves the sovereignty floor) the highest-correlating pair in
  the entire roster under the pairwise Pearson instrument The Range used.
- (b) **Placements.** The (people, cell) settlement lists.

**Falsifier:** they separate in (a) — Wood and High occupy measurably different
ground — which is **a finding about what else differentiates them**, not a
failure. The campaign then names which authored field carried it (mass through
the floor, dispersion, society feeding back through the contest) and reports it
as the result.

**And an honest limit, stated before measurement:** if Wood's and High's
capacity fields come out bit-identical, (a) is a wiring check with no
information in it. The information is in (b): the bake's contest is not a pure
function of the capacity field — iteration order, tie-breaks, migration and the
raid comparison all participate — so two kinds with identical fields *can* still
place differently. If they do, that is a finding about the contest, and the
campaign reports it as such rather than as a fact about elves.

### P4 — Drow separates from surface elves by the realm gate alone

**Axis:** with Drow's `HabitatRealm::Subterranean` row present, the share of
Drow settlements on cells holding an enterable cave is **1.00** (the gate is a
hard zero elsewhere), and Drow's capacity field is separated from Wood's. With
the row removed by mutation, that separation **disappears** up to Drow's own
biome and curve authoring.

**Falsifiers, two:** Drow's field is not separated from Wood's with the row —
the gate did not reach identity after all, contradicting The Range's repair; or
it is *still* separated without the row, which means something other than the
gate is doing the work and the attribution in §4 is wrong.

**A companion null, preregistered:** Drow's dark-adaptation authoring
contributes **zero** to its placement. Perturbing Drow's insolation devotion
alone leaves the committed world byte-identical. Falsifier: it moves — which
means the two-tier tolerance has started to bind, and the Warren tripwire should
have reddened first.

### P5 — the family's downstream language products, over six daughters

This is the family the language machinery has been waiting for: **six daughters
against goblinoid's three and dwarf's three.** The measured axes are the
cascade's *downstream products*, chosen because the author does not control them
(§6).

- **`monophyly-elf`.** Every elf daughter's `Root.derivation.proto` matches an
  **independent re-draw** of the shared `elf` family proto-root for that
  concept — never read back from a sibling's own recorded derivation.
  **Falsifier:** any daughter mismatches, which would mean the proto is being
  sourced from a sibling and the family is not monophyletic in the way the
  metric claims.
- **Divergence is real at six.** Some concept rooted in **all six** daughters
  has ≥ 2 distinct present-day forms — the stemmatics guard, generalised.
  Descent is proven by shared *innovations*, not by a shared ancestor alone, and
  a family of six silent aliases must read false. **Falsifier:** all six
  coincide on every commonly-rooted concept. A null here is strong precisely
  because six draws have more room to differ than three.
- **Homophony does not leak the sibling count.** Per-daughter homophony counts
  (and the core / confusable subsets) for the six elf daughters are **not
  systematically above** those of the two three-daughter families.
  **Falsifier:** they are — which is a defect in the metric or in the draw,
  since homophony is a within-daughter property and the number of siblings
  should not enter it.
- **Name transparency and blind attribution stay in band.** Census-wide, blind
  attribution stays above its 0.75 floor and name transparency's span does not
  collapse from below (the property that rules out a relapse into the uniformity
  defect that number exists to watch). **Falsifier:** either moves outside its
  band — in which case a language-wide product moved under roster growth, and it
  must be attributed before the campaign closes rather than re-pinned.

**In scope and easy to miss:** `GOBLINOID_DAUGHTERS` and `ALL_DAUGHTERS`
(`windows/lab/src/metrics.rs`) **do not generalise**. The family-monophyly,
inventory-closure and homophony metrics will silently not measure elf unless
they are extended. Silently not measuring is the failure this programme exists
to avoid.

### N1 — a stated null: longevity is silent in language drift

`cascade_regime_of` switches a Settled people onto the slow drift regime at
`LIFESPAN_THRESHOLD_YEARS = 120.0` and is **binary** there. The dwarves already
clear it with a wide margin (a paced schedule at factor 4.0 puts all three near
270 y). **Pacing elves harder than dwarves therefore changes nothing in
language**, and no elf-specific tongue-slowness may be attributed to elves.

Longevity remains legible in life-history — `lifespan`, `age_at_maturity` and
`generation_length` stay linear and unbounded — while `pace_of_life` and
`reproductive_tempo` saturate at exactly 1.0 at that pacing and are
uninformative for any long-lived people. This is recorded because a later reader
looking at long-lived elves and slow-drifting elf tongues will otherwise connect
them, and the connection is not there.

## 6. What this campaign will NOT measure, and why

**The correlation between articulation and environment.** It would be the
obvious headline — *do elves in more distant niches speak more distant
tongues?* — and it is circular. **The same hand authors the articulation vectors
and the environmental niches.** Any correlation between them measures the
authoring convention, not the world. It would be a number produced by a decision
made at authoring time and read back as a discovery.

The project has shipped this exact error once and caught it: duergar's authored
300 m elevation optimum came back as an emergent toponymic finding, and one
question dissolved it. **The toponymy was reporting the authoring.** This spec
refuses the elf-shaped version of the same reading in advance, and the refusal is
what §5's P5 is for — the cascade's *downstream* products (monophyly, homophony,
transparency, blind attribution) are not under the author's control, so they can
carry evidence that an articulation-vs-niche correlation cannot.

**A corollary that binds the chronicle as much as the plan:** any statement of
the form "the Drow tongue is harsh *because* the Underdark is" is authoring
described as physics, and must not appear.

## 7. Non-goals

- **Mountain-dwarf and Duergar.** The Delvers withdrew them and left the return
  condition in the code itself — *"They return when the underworld has
  biomes."* The `Biome` enum has **22 variants** (`domains/climate/src/biome.rs`)
  and **not one is subterranean**. Nathan's call, on a surfaced fork.

  The distinction that scopes this correctly, and that also makes Drow separable
  from them: the trap is not *authoring a subterranean kind*, it is
  *distinguishing two kinds by depth*. Drow is one cave kind and needs only to
  differ from surface elves, which the realm gate does. Mountain and Duergar are
  two cave kinds whose entire mutual distinction is stratum, and nothing in the
  model can say that. Authoring them as subterranean dwarves differing only in
  psyche and society is technically possible — Wood and High are exactly that —
  but it would silently redefine what "mountain" and "duergar" *mean* by
  dropping the strata, which is the fake the campaign that found the gap refused
  to ship.

- **Derived divergence.** The metaplan (§4, §5) specifies the elves' language
  divergence as *computed* from radiation topology — "time-since-split ×
  environmental distance". **Half its inputs do not exist:**
  `time_since` / `split_time` / `divergence_time` match nothing in `domains/` or
  `windows/`, so the temporal factor would have to be **authored**, and a
  derived quantity with an invented factor reads as measurement later. Captured
  as `LANG-split-time-from-history`, whose repair is to read split time off the
  deep-history bake — which already marches epochs — making it discovered
  rather than authored. Not attempted here.

- **A world-derived roster.** `BIO-elf-radiation`. `KindId(pub &'static str)` is
  compile-time, and so are `KIND_CONCEPTS`, `family_of`, and every census metric
  keyed by species name. The programme ships **authored roster** and this
  campaign does not reopen that.

- **The underworld as a place.** Cave biomes, over/under commerce,
  chthonic emergence, subterranean valence. That is `MAP-69`'s own campaign, and
  it is also the campaign that unblocks the two withdrawn dwarves. Bundling any
  of it here would make this campaign's placement movement unattributable.

- **Re-authoring existing peoples' niches.** No shipped kind's `ConditionNiche`
  or affinity row is touched. Moving an existing people's capacity in the same
  change that adds six new ones destroys the attribution of both.

- **Half-elves.** `BIO-17`. A hybrid is a gradient between two discrete registry
  keys; it attacks the data model, and it is a data-model campaign in a roster
  costume.

## 8. LANG-53: a six-daughter star, not a tree

`LANG-53` asks for language-family **topology** — which tongues exist and how
they are related, with dialect families authored from the start. It was
repointed at this campaign by the metaplan (§9), because this is the roster it
has been waiting for.

**The honest deliverable is a star, not a tree.** With no time-since-split, all
six daughters are **equidistant from the proto**. The model can say that six
tongues descend from proto-elvish; it cannot say that Drow split before Snow.
There is no field in which that sentence could be written, and authoring one
would be the derived-divergence error of §7 in its topological costume.

What the star is nonetheless worth: it is **the largest family the project has
built**, and the first big enough for the topology question to be a real one
rather than a hypothetical. A three-daughter family barely distinguishes a star
from a tree; a six-daughter family makes the missing structure conspicuous. The
campaign therefore delivers the star, measures it (§5 P5), and **leaves the tree
blocked** on `LANG-split-time-from-history`, with the six-daughter family
standing as that row's motivating case.

`LANG-53` closes at `partial` rather than `shipped`, and its **Where** cell
carries this spec and the deferred half.

## 9. Costs and flags

**1. The placement cascade is the leading risk, and its mechanism is
unestablished.** §2.3: one authored row redistributed the entire settlement
contest, and the route by which it did so was measured only in magnitude. This
campaign declares six rows *and* adds six competitors. Expect every people's
counts to move; expect a large number of pinned counts and rosters across the
workspace to redden as witnesses of the old world (The Range reddened twenty
tests across five crates from one row). The plan must sequence those as a sweep,
not discover them.

**2. Six kinds in one epoch means no per-kind attribution of world movement.**
The Delvers accepted the same for five, at Nathan's direction, with one regen at
close. It is stated because programme spec §6 uses the *opposite* argument to
refuse bundling goblin's re-characterisation: bundling destroys attribution.
Here the bundle is the campaign's subject rather than a passenger in it, which
is the distinction — but P1′ (§5) is what recovers per-kind evidence, and it is
the reason that prediction is in the package.

**3. The authoring cost, per kind, is eight registries plus the accession
cohort** (§3.7), six times over. The cohort is the item with teeth: omitting it
changes which proto-root a concept draws. Append; never edit.

**4. Census regen on lefford — authorization-gated, and requested at close, not
here.** Two fixtures, and this campaign is the case where both move:

- `the-census` refreshes **wholesale, 1000 of 1000 rows**, because a new
  settling people re-decides settlement placement on every seed. Six of them
  certainly will.
- `census-of-the-meeting` is structurally near-immune to competition — its
  rosters are `goblin-solo` and `goblin-twin-solo`, so a new kind never competes
  in it — but it rewrites **every row textually on a column change**, and this
  campaign adds elf-family language metric columns (§5 P5). So it moves too.

The metaplan budgeted a roster campaign at two wholesale fixture rewrites. The
Range paid one. **This campaign pays both.** Budget ~15 min per census run on
lefford (776 / 887 / 921 s measured 2026-08-09, against decision 0063's "~7
minutes"), and expect ~40 fixture tests red until the regen lands — the gate
cannot be green before it.

**5. Epoch: provisionally none — reasoned, not asserted, and re-checked before
the roster commit lands.** Decision 0084's rule is to declare an epoch only when
a *derivation actually moved a drawn quantity*; an empty epoch declares a
discontinuity that did not occur and charges a permanent manifest row for the
fiction. Against that rule:

- Worlds move — but they move because six new competitors enter a contest, not
  because any existing kind's derivation changed. The same shape as The Range's
  provisional none.
- Six new kinds mint new lexicons and new roots, which are **new draws on new
  subjects**, not re-ordered draws on existing ones. No stream label changes.
- The one genuine hazard is the **accession cohort**. Cohort placement feeds the
  proto-root draw, so *appending* a cohort is safe and *editing or re-founding*
  one is an epoch. `ROOT_EPOCH` stays at `v3` on 0089's precedent unless the
  plan finds a reason it cannot.

**This is the flagged save-format call and it leads the G3 package.** It must be
re-checked against the pin-isolation tests and `cli/tests/accession.rs` before
the first roster commit lands, not assumed at the close.

**6. Drow must clear the roster floor.** A kind confined to ~12 % of land must
still produce a non-void roster on every tested seed **with no allowlist entry**.
If it does not, the trait values need re-authoring — an allowlist entry would be
authoring the failure it exists to detect.

**7. The heavy tier's known reds.** Two tests are red at main for written,
measured, non-Radiation reasons (`scene_cost`, `session_cost`). A *third* heavy
failure during this campaign is this campaign's.

## 10. Definition of Done

Standard, plus:

- Every prediction in §5 reported against its falsifier, including the nulls.
  Ship the null as the headline if that is what the measurement says.
- The per-elf visibility table (§4) reproduced in the chronicle, so a later
  reader meets it before meeting High's empty placement result.
- The elf-family language metrics extended past `GOBLINOID_DAUGHTERS` /
  `ALL_DAUGHTERS`, or the campaign states in the chronicle that elf was not
  measured on them.
- Chronicle entry (`book/src/chronicle/the-radiation.md`) and a book freshness
  sweep; the Confidence Gradient re-scored if this moves one of its bets.
- Retrospective (`docs/retrospectives/the-radiation.md`).
- Frontier bookkeeping: `LANG-53` → `partial`, with the star delivered and the
  tree blocked on `LANG-split-time-from-history` (which gains this family as its
  motivating case); `BIO-elf-radiation` unchanged and re-affirmed as blocked;
  `BIO-kind-authoring-seam` re-checked against what six kinds actually cost;
  `PROC-readout-is-not-identity` reviewed for a status flip now that rung 5 has
  been exercised by a campaign that did not invent it.
- **The programme closes here.** The metaplan's roster table should be corrected
  as part of this campaign's bookkeeping: it reads *five peoples become
  seventeen* with dwarf ×5, and The Delvers shipped dwarf ×3. The shipped
  outcome is **fifteen peoples**, with Mountain-dwarf and Duergar owed to the
  campaign that gives the underworld biomes.

---

## Appendix — where the ledger and the metaplan disagree

Three disagreements were adjudicated while writing this spec. Each is recorded
so that the adjudication is visible rather than silent.

1. **The roster's final size.** Metaplan §5 states seventeen peoples with dwarf
   ×5. The Delvers withdrew Mountain and Duergar, so the programme lands at
   **fifteen**. This spec is written to fifteen and §10 corrects the metaplan.

2. **Derived divergence.** Metaplan §4 assigns this campaign "derived divergence
   and LANG-53", and §5 specifies divergence as computed from time-since-split ×
   environmental distance. Time-since-split does not exist. This spec makes
   derived divergence a **non-goal** (§7) and delivers LANG-53's star half only
   (§8).

3. **The `Biome` variant count.** The campaign's decision ledger records 25
   variants; the enum has **22** (counted from `domains/climate/src/biome.rs`,
   agreeing with The Range's spec §2). The count is corrected here. The claim
   that depended on it — that none of them is subterranean — is unaffected and
   holds.
