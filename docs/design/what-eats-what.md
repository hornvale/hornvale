# What Eats What

**Status:** Design brief (2026-09-13). **Not doctrine and not a spec** — it is
the recorded reasoning behind a campaign that has not been written yet, parked
here so the next session starts from the argument rather than from the
conclusion. Nothing in it binds anything. It is filed in `docs/design/`
because it grew out of one campaign (The Tidemark,
`docs/superpowers/specs/2026-09-11-the-tidemark-design.md`) and governs none of
that campaign's product.

**Registry rows:** `BIO-marine-trophic-split` and `BIO-trophic-basis-is-flat`
are the two existing rows this argument sits on top of; `BIO-trophic-receptors`
is the row it adds. All three are in
`book/src/frontier/idea-registry.md`.

**How to use this document.** §1–§3 are measured fact and should be treated as
established (every claim carries a file:line or a pasted number). §4 is a
proposed design and is *argument*, not fact. §5 is the raw ideonomy record that
produced §4, kept in full because the passes contain material §4 compresses
away. §6 lists what is still open. §7 says what to do first, and in particular
what to measure before believing §4.

---

## 1. The finding

The Tidemark's Task 4 was to author a marine subsistence roster — a kelp, a
coral, a bivalve bed, an urchin grazer, a forage fish, a tube worm, a vent
scavenger, a detritivore — because spec §3.7 described the sea's fauna as "a
predator guild with no base under it."

Before authoring, the web was measured
(`windows/worldgen/tests/suite/marine_web_probe.rs`, committed at `f605b765a`).
Trophic heights over all 49 kinds in `biosphere_registry()`:

```
   black-dragon           height=2.816179   niche: animal prey=1.00
   dire-wolf              height=2.816179   niche: animal prey=1.00
   bugbear                height=2.663734   niche: plant forage=0.15 animal prey=0.85
   gnoll                  height=2.467016   niche: plant forage=0.35 animal prey=0.65
   human                  height=2.266566   niche: plant forage=0.55 animal prey=0.45
   giant-elk              height=2.000000   niche: plant forage=1.00
   giant-crocodile        height=1.618750   niche: animal prey=0.60 marine forage=0.40
   sea-elf                height=1.250000   niche: plant forage=0.10 animal prey=0.15 marine forage=0.75
   ---- every obligate marine kind, without exception ----
   abyssal-elf            height=1.000000   [MARINE]  niche: marine forage=1.00
   kelp-tender            height=1.000000   [MARINE]  niche: photosynthate=0.40 marine forage=0.60
   killer-whale           height=1.000000   [MARINE]  niche: marine forage=1.00
   merfolk                height=1.000000   [MARINE]  niche: marine forage=1.00
   reef-mason             height=1.000000   [MARINE]  niche: marine forage=1.00
   reef-shark             height=1.000000   [MARINE]  niche: marine forage=1.00
   triton                 height=1.000000   [MARINE]  niche: marine forage=1.00
   vent-commensal         height=1.000000   [MARINE]  niche: marine forage=0.25 chemosynthate=0.75
   giant-octopus          height=1.000000   [MARINE]  niche: marine forage=1.00
   giant-squid            height=1.000000   [MARINE]  niche: marine forage=1.00
```

**The sea is trophically flat. The land is not.** A killer whale is computed as
the same kind of object as seaweed.

The internal control is the sharpest evidence: `sea-elf` (1.250) and
`giant-crocodile` (1.619) are the only marine-eating kinds with any height, and
both get it **entirely from the land fraction of their diet**. Height tracks
`MARINE_FORAGE` inversely and exactly.

`predation()` omits all ten marine kinds as keys — absent, not empty-listed.
The harness control (a non-marine predator's non-empty prey list from the same
`web` map) is present and passes, so the emptiness is a result rather than a
broken probe:

```
   black-dragon  eats ["abyssal-elf", "bugbear", ..., "vent-commensal", "wood-elf", "xorn"]   (38 entries)
```

Pairwise Pianka overlap among the ten marine kinds is **1.0000** for eight of
them (all four fauna plus `abyssal-elf`, `merfolk`, `reef-mason`, `triton`),
`0.8321` for `kelp-tender`, and `0.3162`/`0.2631` for `vent-commensal`.

### Why

- `kernel/src/ecology.rs:98` — `MARINE_FORAGE` is a single `Stock` axis, and
  its own doc says marine food-chain *length* "is not yet an emergent property."
- `domains/demography/src/niche.rs:93` — `is_heterotroph` is
  `weight(PLANT_FORAGE) > 0 || weight(ANIMAL_PREY) > 0`. **A killer whale is not
  a heterotroph by this model's own classifier.**
- `domains/demography/src/niche.rs:202-237` — `next_level` gives a supplier
  level to `PLANT_FORAGE` (constant 1) and `ANIMAL_PREY` (recursive) only. Every
  other axis enters the renormalising denominator and contributes no height,
  i.e. is treated exactly as abiotic.
- `domains/demography/src/niche.rs:179` — `predation()` `continue`s past any
  species whose `ANIMAL_PREY` weight is `<= 0.0`.

### The vacuous test this nearly shipped

Task 4's success criterion was "M6 reports zero dangling requirements", where
M6 resolves each marine people's subsistence to a named kind or an aggregate
field. **That test passes today, before any kind is authored**, because
`MARINE_FORAGE`, `PHOTOSYNTHATE` and `CHEMOSYNTHATE` are all aggregate axes with
suppliers wired in worldgen (`marine_forage_supply_field`,
`marine_chemosynthate_supply_field`). It would have passed identically on an
empty roster and a nine-kind roster and never distinguished them.

> **Correction (2026-09-13) — read the frame as well as the fact.**
> `CHEMOSYNTHATE` is an aggregate **on `origin/main`**, the tree this brief was
> written against and the tree every campaign branches from. It is **not** an
> aggregate on `campaign/the-trencher`, which disaggregates it into four
> registered metabolite axes — `HYDROGEN`, `REDUCED_IRON`, `REDUCED_SULPHUR`,
> `METHANE`, all `kind: Field`, all in `v1_basis()`, with `CHEMOSYNTHATE`
> retained as their sum for generalists. Verified in both trees rather than
> relayed: `origin/main` carries ids 0-6 and no `HYDROGEN` constant at all
> (`grep -c 'HYDROGEN'` returns 0; id 0 there is `PHOTOSYNTHATE`), and
> `git merge-base --is-ancestor campaign/the-trencher origin/main` says **NO**.
>
> This does not weaken the proposal below — it sharpens it, and section 4.6
> anticipated it without knowing the work existed. Receptors make the **prey**
> description discriminable; metabolite axes make the **vertex** description
> discriminable. Those are the two halves of one duality: no eater-side
> predicate can express "this chamber has methane but no hydrogen", because that
> is lithology, not appetite. Raised by `campaign/the-trencher`.
>
> One caution that campaign measured and this brief endorses: registering four
> ids does not mean four metabolites **discriminate**. At the median, hydrogen
> spans 0.312-0.423, reduced iron 0.292-0.415, reduced sulphur 0.053-0.556, and
> **methane is flat at 0.025**. Three discriminate spatially; one does not, and
> an axis nothing can usefully weight is the "registered but inert" shape this
> repo keeps rediscovering.

### The second instance, found by the same measurement

Six kinds sit at height 1.000 for a *different* reason, and it is the same
shape: `carrion-crawler`, `otyugh`, `drow`, `duergar`, `kuo-toa` and
`gully-dwarf` are all detritus-dominant, and `is_off_chain` pins anything
detritus-dominant at level 1.0 by convention. A scavenger eating a wolf carcass
should inherit the wolf's height. It is routed through an identity-less axis
instead. See §5.3 — carrion and detritus are different mechanisms wearing one
name.

---

## 2. Two fixes proposed and withdrawn

Both are recorded because each looked right and the reason each fails is
load-bearing for §4.

### 2a. Author the roster anyway

Rejected. Anything placeable at an ocean vertex must sit on `MARINE_FORAGE`
(see 2b), which gives it overlap **1.0000** with the six peoples — measured, not
argued. The "base" would be a perfect competitor of the thing it feeds. Nine
kinds, every coverage ratchet bumped, and no mechanism able to tell them apart
from the peoples.

### 2b. Split `MARINE_FORAGE` into a marine trophic ladder

Proposed: a new `MARINE_PREY` axis (**the draft said id 7; that is wrong now —
see the id correction below**; the basis is append-only, pinned by
`the_basis_ids_are_append_only`), a `marine_prey_supply_field` mirroring
`prey_supply_field`, and a small `next_level` change.

This was seductive because the land's second tier is literally one line:

```rust
// The ANIMAL_PREY supply field (The Vacancy): prey biomass as a
// trophic-transfer fraction of grazable forage.
// ... marine predators eat `MARINE_FORAGE` instead.
pub fn prey_supply_field(geo, forage) -> VertexMap<f64> {
    VertexMap::from_fn(geo, |c| forage.get(c) * PREY_FRACTION)
}
```

**Withdrawn** after §4. The marine flatness is not a missing marine axis; it is
an artifact of expressing diet as *which substance-bucket you draw from* at all.
Under a receptor model the sea gets depth with no new axis, because a reef shark
and an urchin differ in mass band, defence and vertical band.

> **Id correction (2026-09-13).** This section is **withdrawn**, so nothing live
> depends on it — but a reader skimming a withdrawn section for a free axis id
> is exactly the accident that costs an afternoon. On `origin/main` today ids
> 0-6 are taken and the next free id is **7**. `campaign/the-trencher` claims
> 7-10 and is **not merged**, so once it lands the next free id is **11**. Do
> not take an id from this brief; take it from `kernel/src/ecology.rs` in the
> tree you are actually on.

Also recorded, because it was checked and is a hard constraint on any
axis-based fix — from `domains/species/src/lib.rs`, authored by Task 3:

> At an ocean vertex the only supply axes that pay anything are `MARINE_FORAGE`
> and `CHEMOSYNTHATE`. `PLANT_FORAGE`, `ANIMAL_PREY`, `MINERAL` and `DETRITUS`
> are all land-masked at their own supply fields, and `axis_supply_with` is a
> **sum**: a weight on a land axis is not a diversification for a marine kind,
> it is a discount.

So "author the marine base on `PLANT_FORAGE` so it carries height" produces a
kind with zero supply at sea that is never placed. Checked before proposing;
it does not work.

---

## 3. What the existing machinery already gives us

- `predation()` already returns `BTreeMap<predator_id, Vec<prey_id>>` — literally
  who-eats-whom. **It has two production consumers**
  (`domains/demography/src/coexist.rs:455`,
  `windows/sentiment/src/lib.rs:122`); everything else is tests and probes.
  `windows/sentiment` immediately converts it to a named
  `preys_on: BTreeSet<PeopleId>`.
- `trophic_levels()` has two production consumers (`niche.rs:175` inside
  `predation`, `coexist.rs:454`).
- `condition_niche` already expresses "what must be true of a *vertex* for me to
  live here" — the dual of a prey predicate, and the half autotrophs need.
- Per-species-per-vertex carrying capacity already exists
  (`per_species_capacity`).
- `MIN_PREY_RATIO = 0.001` / `MAX_PREY_RATIO = 0.5`
  (`domains/demography/src/niche.rs:26,31`) — one global mass window, prey mass
  over predator mass.

**The seam this design needs already exists and is two call sites wide.** The
change is to `predation()`'s implementation, not its shape.

---

## 4. The proposal: diets as receptors

### 4.0 The move

> A predator does not declare *what* it eats. It declares **what a thing must be
> like** to be food.

Not a list — a receptor. Authoring goes O(N), because each species describes
only itself.

The comparator that makes this concrete is immunological: antibodies do not
enumerate pathogens — there are more pathogens than genes. They bind *shapes*,
combinatorially. A vertebrate covers an unbounded pathogen space with a bounded
genome. That is the scaling trick, and it is the one this needs.

### 4.1 Capability is separable from realization

This distinction is the design's spine, and it came from Nathan correcting a
misreading of the measurement. The black dragon's 38-entry prey list, which
includes every marine people, is **not absurd**:

```
  white dragon vs abyssal elf
    capability   flesh, mass band OK, unarmoured, fleeing   -> ADMITS
    realization  co-present at this vertex?                 -> normally NO
                 ...on Denali                               -> YES, eaten
```

The capability predicate was right the whole time. What is missing is a
co-location filter. That is one filter, not a redesign — and it means the
current derived web is a *capability* statement being read as a *realization*
statement.

### 4.2 Edibility is a lattice, not a taxonomy

A tag system fails on Nathan's case — a 5 g frog tagged "eats insects" eating a
100 kg giant centipede tagged "insect". The diagnosis: **"insect" is a taxonomic
node being asked to do an edibility job.** Taxonomy is a tree and forces one
parent. Edibility is a lattice of independent, mutually incomparable orderings:

```
  axis                what it orders                    a giant centipede is...
  ------------------  --------------------------------  -----------------------
  tissue class        flesh/leaf/wood/detritus/         flesh
                      mineral/chemical
  mass band           relative to the eater             100 kg -> 20,000x a frog
  medium              marine/surface/subterranean/      subterranean
                      aerial
  defence             unarmoured/chitinous/venomous/    chitinous + venomous
                      gregarious/fast
  disposition         sessile/fleeing/fighting          fighting
  vertical band       canopy/ground/seabed/water col.   ground
```

Mass band and clade have **no ordering relation to each other**, so neither may
stand in for the other. A diet predicate naming only one is under-dimensioned by
construction. The frog declares *flesh · 0.001–0.05 of my mass · surface ·
unarmoured · not fighting*; the centipede fails on three axes independently, and
no taxonomy is required.

### 4.3 The five terms

```
  capability     a predicate over prey TRAITS         authored, O(N), per species
  admissibility  capability x bestiary                derived once per world
  realization    admissibility x who is HERE          derived per bake, local
  preference     profitability, + hysteresis,         derived, + sparse authored
                 - taboo                                mask for peoples
  yield          grade (eaten) x recovery (eater)     authored, two O(N) tables
```

Definitions with genus and differentia, because the terms currently do each
other's jobs:

- **Capability** — a predicate over prey *properties* that an eater declares
  about itself. Authored once; never evaluated against a world.
- **Admissibility** — the pairwise relation from evaluating every capability
  against every species' traits. A property of the *bestiary*, not of a world.
- **Realization** — admissibility restricted to species co-present at a vertex,
  weighted by abundance. A property of *a place at a time*.
- **Preference** — an ordering over the admissible set at a vertex, re-ranked as
  abundance moves. The source of prey-switching.
- **Yield** — what one act of eating transfers. Different for whole-body (the
  carcass) and part-taking (the crop).

### 4.4 Why it scales

> The S² blow-up is **global and precomputed**. The per-vertex work is over the
> handful of species actually present.

A thousand species is 10⁶ admissibility pairs computed once — a predicate
evaluation over a handful of trait axes, not a stored matrix. A vertex holds
perhaps a dozen species, so realization is ~144 pairs per vertex per bake. This
scales to thousands of species precisely because the expensive part never
touches the map.

**Rates must not be uniform.** A sessile coral's web changes over geological
time; a migratory forage fish's changes seasonally. Forcing one rate prices
everything at the fastest mover. The load-bearing kernel is three tiers —
capability static, admissibility per world, realization per bake — with
preference-per-season as the first optional extra, and it is the one that buys
prey-switching.

**Resolution is deliberately asymmetric.** The predator queries; the prey is
queried. Making it symmetric (prey fleeing, aggregating, responding) roughly
squares the cost for a second-order effect. Keep the asymmetry, and know it was
kept on purpose.

### 4.5 The design rule that keeps it sparse

> **Gate with capability; weight with yield.** Never express "cannot eat" as
> "yield zero."

Expressing inedibility as a zero weight produces a dense matrix of mostly zeros
— exactly the quadratic authoring this is built to avoid. Sparsity must live in
the predicate.

### 4.6 What this does to the existing machinery

```
  piece                   fate
  ----------------------  ------------------------------------------------
  predation() signature   UNCHANGED - still predator -> prey ids, 2 callers
  mass window             GENERALIZES - one global 0.001..0.5 constant pair
                          becomes a per-species declared band, and applies
                          to WHOLE-BODY acquisition only (see 5.1)
  resource axes           RETREAT to autotrophs, where they are correct:
                          kelp eats light, tube worms eat vent chemistry
  trophic height          DERIVED OUTPUT, not an input - read off the web
  MARINE_PREY axis        NOT NEEDED (see 2b)
  named diet edges        A SMALL OVERRIDE TABLE for narratively load-bearing
                          pairs, O(handful), never O(N^2)
```

---

## 5. The ideonomy record

Six passes, each with a randomly drawn method tuple (`ideonomy-plain`). Kept in
full: the passes contain material §4 compresses away, and the tuples are
recorded so a reader can judge whether an artifact was shaped by its method or
by the author's defaults.

### 5.1 Flora — *organon-construction + substitution; chart; size, intentionality, source*

Substituting one property of "eating" — **the source** — while holding the rest
constant generates the whole acquisition taxonomy, flora included:

```
                  | TAKES THE WHOLE  | TAKES A PART     | TAKES A PRODUCT  | TENDS, THEN TAKES
  ----------------+------------------+------------------+------------------+------------------
  the vertex      | autotrophy       | --               | --               | --
  itself          | (kelp, coral,    |                  |                  |
                  |  tube worm)      |                  |                  |
  ----------------+------------------+------------------+------------------+------------------
  a living        | PREDATION        | grazing,         | nectar, milk,    | herding,
  organism        | (mass window     | browsing,        | eggs, honey      | dairying
                  |  applies)        | parasitism       |                  |
  ----------------+------------------+------------------+------------------+------------------
  a dead          | scavenging       | guild scavenging | --               | --
  organism        | (small carcass)  | (hide/marrow/    |                  |
                  |                  |  bone)           |                  |
  ----------------+------------------+------------------+------------------+------------------
  identity-less   | detritivory      | --               | --               | composting,
  matter          | (field-shaped)   |                  |                  | fungiculture
  ----------------+------------------+------------------+------------------+------------------
  a cultivated    | --               | harvesting       | orcharding,      | AGRICULTURE
  organism        |                  |                  | sap, resin       |
```

**The load-bearing cell boundary is column 1 versus column 2**, and it
generalises the frog/centipede problem:

> The mass window applies to **whole-body** acquisition only. A 5 kg goat
> browsing a 2,000 kg tree is not absurd, because the goat never swallows the
> tree.

Today there is one mass rule for everything, which is why flora cannot be
modelled: any plant big enough to matter is too big to be prey. Splitting the
column splits the rule. Whole-body takes a ratio band; part-taking takes a
**yield fraction**, and the ratio constraint inverts — a bigger tree feeds more
goats.

Empty cells read as typed predictions. The top row is empty across columns 2–4
because a vertex has no parts and makes no products; that emptiness is correct
and confirms the axis. The distinction the chart forced into the open:
**herding is tending a thing you did not make; agriculture is tending a thing
you did** — the `intentionality` axis, and the seam where fauna stop and peoples
begin.

Autotrophy is not an exception to the receptor model but its **dual**: a
predator declares a predicate over other organisms, an autotroph over the
vertex. Hornvale already has the second (`condition_niche`).

### 5.2 Sentients — *negation + cross-domain-reinstantiation; graph; longevity, decomposability, direction*

Negating "a people eats what it hunts", one definitional property at a time:

```
  negate...                        | and you get
  ---------------------------------+---------------------------------------
  eats what it FINDS               | agriculture (eats what it makes)
  eats what it KILLS               | herding, dairying (keeps it alive)
  eats what is HERE                | TRADE (imports)
  eats what is AVAILABLE NOW       | STORAGE (granary, salting, drying)
  the eater ACQUIRES               | tribute, tithe, a parasitic elite
  one species, one diet            | CULTURE (diet varies within a people)
  the population feeds ITSELF      | a breadbasket that feeds others
```

The graph:

```
        wild prey ----kills----> PEOPLE <----imports---- trade partner
                                 ^  |                          ^
            herd ----tends-------+  |                          |
                                 |  +----stores----> granary   |
            crop ----sows--------+  |                     |    |
                                 |  +----exports----------+----+
        neighbours --tributes----+
```

Two edges are structurally unlike anything fauna have, and they are the whole
answer to how sentients differ:

- **`stores` breaks locality in time.** Famine stops being "supply dipped" and
  becomes "supply dipped *for longer than the buffer*" — a stockout with a lag.
- **`imports` breaks locality in space.** Carrying capacity stops being the
  vertex's supply and becomes the supply of everywhere reachable.

Re-instantiated in **logistics** — a domain with no ecology in it — the
translation is exact and the vocabulary is pre-solved: inventory, lead time,
buffer stock, substitution elasticity, stockout. That domain's answer to
fragility is *diversify supply and hold buffer*. Hornvale already has the
failure state: `CauseOfEnd::Famine`.

The three axes name real quantities:

- **longevity** → shelf life. Grain keeps for years, fish for days. A people
  whose only store is perishable is a different civilisation from one with
  granaries.
- **decomposability** → resilience. A diet decomposable into independent sources
  survives losing one; a monoculture is atomic and fails whole. That single axis
  generates the history of agrarian collapse without authoring any of it.
- **direction** → accumulating (storage), decaying (spoilage), oscillating
  (seasonal). A store is the only thing in the model that *accumulates*.

### 5.3 Carrion, scavengers, detritus — *combination + abstraction-lift; state-machine; modularity, animacy, distribution*

```
                  (killed)            (not eaten in time)
     ALIVE ------------------> FRESH KILL ----------------> CARRION
       |                          |                           |
       | (sheds: litter,          | (guild takes              | (decomposes)
       |  moult, dung)            |  its module)              v
       |                          v                        DETRITUS
       +---------------------------------------------------->  |
                                                               | (mineralised)
                                                               v
                                                           NUTRIENTS
                                                               |
                                                    (taken up) |
       +-------------------------------------------------------+
       v
     ALIVE
```

Lifted until the biology falls away, the states differ on one thing: **how much
identity the matter has left.**

```
  state        | identity retained        | shape it wants     | animacy
  -------------+--------------------------+--------------------+-------------
  ALIVE        | full (species, mass,     | an AGENT           | alive
               |  location, behaviour)    |                    |
  FRESH KILL   | species, mass, location  | a point EVENT      | quasi-alive
  CARRION      | mass, location, decaying | a decaying EVENT   | quasi-alive
  DETRITUS     | none - it is a quantity  | a FIELD            | informational
  NUTRIENTS    | none                     | a FIELD            | abstract
```

> **Carrion is receptor-shaped; detritus is field-shaped; the model has one axis
> for both.**

This is the diagnosis for the six kinds pinned at 1.000 in §1. A scavenger
eating a wolf carcass should inherit the wolf's height — the carcass still knows
it was a wolf — but it is routed through an identity-less axis. The
`distribution` axis says the same thing in one line: **a carcass is a point
source, detritus is a field.**

`modularity` opens something new. A carcass is decomposable and scavengers
partition it:

```
  vulture / raven     -> flesh        (fast, aerial, first)
  hyena / bone-eater  -> marrow, bone (late, strong jaw)
  beetle / worm       -> residue      (last, small)
```

Three guilds, one body, **no competition** — they take different modules. The
single-axis model cannot express this at all, and it needs no new species.

Trigger asymmetry: **entering CARRION is easy and leaving it is contested.**
Everything dies; whether a scavenger or decay gets there first is a race, and
that race is a knob — toward scavengers gives a vulture-rich world, toward decay
a fungal one.

The apparently forbidden transition ALIVE → DETRITUS is **not** forbidden:
litter, moult and dung all take it without death. That edge is where flora feeds
the detritus field, and it closes the loop.

### 5.4 Cycle resolution rate — *negation + tree-finding; dictionary; homogeneity, complexity, symmetry*

```
  when                | what it can know             | cost
  --------------------+------------------------------+-------------------------
  authoring time      | capability only              | free
  once per world      | admissibility (who COULD)    | O(S^2) once, S = species
  per world-bake      | realization (who DOES here)  | O(V x s^2), s = species
                      |                              |   PRESENT at a vertex
  per season          | preference re-ranking        | O(V x s)
  per tick            | individual acts              | unaffordable
  on query            | only what is looked at       | lazy, unbounded latency
```

The dictionary this produced is §4.3; the scaling conclusion is §4.4; the
homogeneity, complexity and symmetry readings are the three paragraphs
following it.

### 5.5 The forage cycle — *the earlier pass: abstraction-lift + tree-finding + dimension-identification; cycle + lattice*

```
      abundance high
            |
    (1) preferred prey taken    ->   prey population falls
            |                                  |
            v                                  v
    (4) preferred recovers            (2) preference no longer pays
            ^                                  |
            |                                  v
    (3) predator SWITCHES to next-ranked admissible prey
            |
      abundance low  -----------------------> back to (1)
```

Three things appear that a static diet cannot express:

- **Prey switching.** A generalist rides the cycle; a specialist declares a
  narrow predicate and crashes with its prey. That distinction generates most of
  the ecological character one would otherwise author by hand.
- **Attractor states.** `urchin-barren` stops being a name drawn from a variant
  pool and becomes *a place the cycle gets stuck*: urchins admit kelp, nothing
  admits urchins, phase 3 never fires. The state is produced rather than
  labelled. (Today it is `climate::Variant::UrchinBarren`, one weighted member
  of `Formation::KelpForest`'s pool, drawn by a seeded `weighted_index` keyed
  only on `(seed, vertex)` — nothing ecological gates it, and structurally
  nothing can, since `domains/climate` may not depend on `domains/species`.)
- **The anti-phase is diagnostic.** A predator with no admissible prey at any
  phase is a placement bug the system can *report* — the "dangling requirement"
  M6 was groping for, in a version that can actually fail.

The lattice of §4.2 came from the same pass.

### 5.6 Preference — *cross-domain-reinstantiation + organon-construction; spectrum; cardinality, naturalness, discovery-vs-invention*

```
  FULLY DERIVED                                              FULLY AUTHORED
  |------------|------------------|-------------------|------------------|
  profitability   optimal diet       TRADITION            taboo, sacred
  ranking         breadth            (derived, but        animal, caste
  (energy/        (endogenous        hysteretic)          diet
   handling)       generalist-
                   specialist)
  <--- discovered ------------------------------+------------- invented --->
  <--- natural ----------------------------------+------------ man-made --->
       fauna occupy this band                    peoples occupy this one
```

Re-instantiated in **behavioural ecology**, the derived end is already solved.
Optimal foraging theory ranks prey by profitability (energy ÷ handling time) and
derives **diet breadth** endogenously: include a prey type if its profitability
beats the average return of the set already taken. That single rule gives:

- a predator in a **rich** environment becomes a **specialist** (the marginal
  prey is not worth the handling);
- the same predator in a **poor** environment becomes a **generalist**.

Same species, same authored capability, two diets, because the *place* differs.
No authoring.

**The middle of the spectrum has no standard name and is the interesting
region.** Preference that is derived but carries its own history: a people that
has hunted seals for generations has lower handling cost for seals, raising seal
profitability, reinforcing seal hunting. **Tradition = hysteresis in a derived
preference.** One state variable per people per prey, sparse — and it buys
*two identical peoples in identical environments diverging culturally*.

Conclusion: **not authored or derived — derived base, authored mask, hysteretic
middle.** The taboo layer is *subtractive* and sparse: author only the refusals,
never the rankings. `cardinality` says preference is per-people-per-prey, not
per-species — which is what makes food cultural rather than biological.

### 5.7 Yield — *abstraction-lift + tree-finding; scale; polarity, visibility, connectivity*

Lifted, yield is **a transfer with a loss coefficient**. The most useful
comparator is ore processing:

```
    yield  =  GRADE  x  RECOVERY
              (of        (of the
               the ore)   process)
```

```
  term          | lives on   | example                           | table size
  --------------+------------+-----------------------------------+-----------
  GRADE         | the EATEN  | fat seal >> lean fish >> kelp     | O(N)
                |            | >> wood                           |
  RECOVERY      | the EATER  | a ruminant recovers cellulose;    | O(N)
                |            | a cat does not                    |
  pair residual | the PAIR   | nearly empty - armour/venom are   | ~0
                |            | GATES, not yields                 |
```

**Two O(N) tables, no O(N²) table.** That is the answer to "eater, eaten, or
pair": it factors.

The scale, anchored on real transfer efficiencies:

```
    0%  | inedible                     wood to a carnivore
   ~1%  | lignin, chitin               specialist decomposers only
  ~10%  | classic trophic transfer     the textbook figure
  ~20%  | flesh to flesh               good recovery, ordinary predation
  ~40%  | PRODUCTS: milk, eggs,        evolved to be transferred
        | nectar, honey
  ~90%  | blood, sap                   no structural material to discard
   100% | impossible
```

**The product row sits high precisely because products evolved for transfer.**
Milk is a substance whose entire function is moving energy between organisms.
That is why dairying and beekeeping are disproportionately strong strategies for
a people — and it is the same cell §5.1's chart flagged as the
herding/agriculture column. Two independent passes landing on one cell is the
best evidence available that the decomposition is right.

Two flags:

- **Polarity — yield is anti-symmetric.** The same edge is a gain for the eater
  and a loss for the eaten. If the model books only the gain, prey populations
  never fall and §5.5's cycle cannot turn. **Whether Hornvale currently books
  the loss side is unverified and is the first thing to check.**
- **Visibility — grade is latent.** You cannot see how fat the seal is until you
  catch it. Where risk, skill and lore would enter. Probably out of scope; worth
  not forgetting.

---

## 6. Open questions

1. **Does predation book the loss side against prey populations?** Unverified.
   If it does not, the forage cycle cannot turn and §5.5 is decorative.
2. **What would `coexist::pack` do if the competition term came from diet
   overlap rather than resource-axis overlap?** Unknown; it is the main
   downstream consumer and the main risk.
3. **Flora as prey.** Grazing on a *sessile* thing — is it predation with an
   inverted mass rule, or its own relation? §5.1 says the former; not tested.
4. **Acquisition mode as an axis.** `BIO-trophic-basis-is-flat` proposes
   indexing *mode* (graze/hunt/scavenge/cultivate/extract) rather than
   substance. §5.1's chart columns are that idea arrived at independently. The
   two should be reconciled rather than both built.
5. **Where the carrion/detritus split lands.** Carrion as point events needs an
   event surface that may not exist.
6. **Determinism.** Every structure above must be `BTreeMap`/`BTreeSet`/`Vec`
   and float-ordered by `total_cmp`. Preference hysteresis introduces
   **per-people state that persists across bakes** — the first thing in this
   design that is not a pure function of the seed, and it needs a home in the
   ledger or an explicit re-derivation rule.

---

## 7. What a campaign from this should do first

In order, and the first two before believing anything in §4:

1. **Answer open question 1** by reading the code, not by reasoning. One grep.
2. **Answer open question 2** cheaply — swap the competition term in a probe and
   diff the resulting placements on seed 42.
3. **Re-run `marine_web_probe`** as the regression control for any change to
   `next_level`/`is_heterotroph`. It prints all 49 heights; **if land heights
   move, the change is wrong.** This is the single most useful artifact The
   Tidemark leaves behind for this work.
4. Only then write a spec. It is large enough to need one, and it touches the
   kernel's resource basis, demography, worldgen capacity and every species row
   — so it moves every world and owes a census.

**Do not start by authoring species.** The Tidemark's Task 4 is the cautionary
case: nine kinds were nearly authored under a test that could not fail.
