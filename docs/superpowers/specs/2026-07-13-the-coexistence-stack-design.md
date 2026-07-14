# The Coexistence Stack (MAP-22) — Design

**Status:** APPROVED for execution (2026-07-14). Design captured in the
`2026-07-13` DRAFT, refined and approved 2026-07-14 after two ideonomy passes
on the ecological-role representation (see §1.1–§1.2). This is the next
campaign after *the-gathering* (the carrying-capacity field), and it builds
directly on that field. It operationalizes and refines the MAP-22 frontier
essay (*the biogeography of coexistence*); per decision 0031, this spec binds
over the essay where they differ.
**Date:** 2026-07-13 (approved 2026-07-14)
**Campaign:** the coexistence stack (MAP-22, equilibrium half; the *dynamic*
re-packing over deep time is a later history campaign)

## Problem

*The-gathering* ships a **carrying-capacity field** (`K_s(cell)` per species)
and a deliberately-simple **interim condensation**: each species condenses its
own flow-attractors independently. That interim has three known, documented
limitations, all of which this campaign exists to fix:

1. **Independent per-species condensation produces unrealistic overlap.**
   Four peoples each scatter across the whole globe with no competition, so
   the map is "goblins evenly interspersed with dragons, interspersed with
   sphinxes" — the *1000 bowls of oatmeal* failure. Real distributions are
   shaped by competition for finite space.
2. **No cross-species scale.** Population is headcount, so a settlement can
   hold "524 dragons" (absurd) or "1 myconid" (a colonial organism miscounted
   as an individual). Species differ enormously in body scale, and headcount
   is the wrong currency.
3. **No composition structure.** Every settlement is single-species by
   construction; there are no cosmopolitan cities, no single-species
   metropolises earned against competition, no tiers.

The naive fixes are both wrong, and the design must avoid both extremes:
**hard competitive exclusion** (one winner per cell) manufactures monoculture
— empty dragon-deserts, no coexistence — while **independent overlay** (the
interim) manufactures oatmeal. Nature is neither: it is a **stack** — a
dragon's range *contains* goblin villages which contain mouse-scale life, all
coexisting under constant strife (squirrels contest squirrels, chipmunks, and
birds in the same patch, even sharing a diet). The MAP-22 essay names this
exactly: "commensal tenancy — kobolds in the dragon's shadow; **a cell is a
stack**," and warns that "per-cell fitness-argmax *manufactures* monoculture."

This campaign stays **derived** (closed-form over the per-species K fields, no
history, deterministic). The *dynamic* version — re-running the stack as K
drifts over eras, producing displacement cascades (dragon expands → evicts a
people → they hit the bugbears → the bugbears enslave the goblins) — is the
later **history** campaign. This one is the equilibrium the history re-solves.

## Goal

A derived, deterministic **multi-species coexistence stack** over the
carrying-capacity field: every cell carries a per-species density vector at
multiple scales; discrete settlements condense from it with real composition
(cosmopolitan ↔ mono-species) and mass-based size; and the packing emits three
first-class derived byproducts — a **strife field**, **wilderness**, and
**refugia** — that later campaigns consume.

- A **cell-as-stack**: `density_s(cell)` per species, with big-footprint
  species fractional (a dragon at 0.02/cell = one dragon per basin).
- **Coexistence, not exclusion**: a dominant species takes the lion's share of
  a cell's capacity but never all of it; weak same-guild rivals persist thin;
  genuinely unfit species are absent below a viability floor.
- **One master knob** (the competition temperature β) sliding the whole model
  from monoculture (β→∞) through realism to oatmeal (β→0), calibrated to a
  target per-cell diversity.
- **Mass-based size and species-relative rendering**: no "524 dragons," no
  "1 lonely myconid."
- **Derived byproducts**: strife field (contest zones → cosmopolitan sites,
  ecotones, future war ignition), wilderness (packing fragmentation), refugia
  (the weak's strongholds + recolonization reservoirs).
- **A richer roster.** The equilibrium is only *real* if the species span the
  axes it operates on. This campaign adds ~8–12 axis-spanning species (§1.6)
  so the trophic coupling and cross-scale rendering are live and
  calibratable, not dormant on four comparable goblinoids.
- The bridge in the arc **field (the-gathering) → coexistence stack (this) →
  history (re-packing over deep time)**.

## Design principles

- **Derive, don't evolve — here.** The equilibrium stack is a closed-form
  function of the per-species K fields and the authored species primitives.
  It draws nothing from the seed beyond what the fields already consumed.
  History (the evolved re-pack) is a separate campaign; this one is the prior
  it starts from and is bounded by (coarse constrains fine).
- **Author O(S), derive O(S²).** The ecological relationships between species —
  who competes with whom, who eats whom — are *derived* from a small
  per-species authored vector, never authored as a species-by-species matrix.
  This is the property that keeps the eventual generative-species regime
  (§8) tractable: authoring stays linear in species count.
- **One packing operator, two lifetimes.** Invoked once against today's K =
  this campaign's derived equilibrium. Re-invoked over a drifting K field =
  the history campaign, whose *deltas between successive packs are the
  displacement/migration/subjugation events*. Same shape as `flow` (settlements
  once, abandonment when re-flowed) and tectonics.
- **Bands modular, within-cell sharing atomic.** Each footprint-grain is
  computed independently (parallelizable); within a cell the competition
  sharing is a single coupled normalization over all present species.

## 1. The mechanism

### 1.1 The authored primitive: `(mass, niche)` — and what is derived

Two ideonomy passes (2026-07-14) refined the DRAFT's ecological-role field.
The DRAFT proposed either an ordered `{Producer, Consumer, Predator, Apex}`
enum or a scalar `trophic_level`, with the fallback "derive apex-ness from
mass." All three are rejected, for reasons the passes made concrete:

- **A single "trophic role" field conflates two orthogonal structures.**
  *Trophic level* is a **vertical** partial order (who eats whom — a DAG,
  drives predation) while *guild* is a **horizontal** equivalence relation
  (who competes for the same resource — drives the competition share).
  Predation acts *across* levels; competition acts *within* a level. One
  ordered field cannot encode a partial order and an equivalence relation; the
  DRAFT's single field would have wrongly placed a dragon and a goblin in the
  same competition normalization.
- **Mass does not predict trophic level.** Huge producers (a redwood) and
  tiny apexes (a venomous shrew, a raptor) both exist; a large herbivore
  outmasses most predators. Mass predicts *footprint*, not food-web position.
  The "bigger ⇒ higher trophic level" heuristic is dropped.
- **A bare scalar cannot place off-chain feeders.** A scavenger/detritivore
  eats dead matter from every level; it has no single height in the food
  chain. The linear scalar cannot represent it.

The resolution collapses the representation to a **single authored ecological
primitive** from which every relationship derives. `SpeciesDef` gains exactly
two ecological fields:

- **`mass: Mass`** — a typed newtype (kernel units family). It has **two
  distinct jobs and never a third**: (1) home-range footprint via Kleiber
  (`home_range(mass) → grain`, §1.3), and (2) prey-size selection (the
  predator/prey mass window, §1.4). Mass **never** sets trophic level.
- **`niche: ResourceVector`** — the fundamental-niche **resource-utilization
  vector**: a **sparse vector over an extensible, registered resource-field
  basis**, *not* a fixed-width array over a closed enum. v1 registers a small
  physical basis:
  **{ photosynthate, plant-forage, animal-prey, detritus/carrion, mineral }**
  (an `aquatic`/water-column axis is reserved for later). Each axis couples to
  substrate the world already has: photosynthate → insolation (BIO-15),
  plant-forage → the NPP/carrying-capacity field, animal-prey → lower-tier
  densities, detritus → the off-chain decomposer loop, mineral → terrain.

  A **resource axis is a tappable Field** (the kernel's space×time primitive),
  and the basis is *open* precisely so later campaigns can register more
  fields — a mana field, a dread field, a belief/sanctity field — and a
  species' `niche` simply weights whichever it taps. This is the one
  forward-compat choice that lets **fantasy metabolisms** (magical, psychic,
  theovorous) be a later tier rather than a `SpeciesDef` refactor (§8, and the
  idea-registry row in §6). Two properties ride along:
  - **Each resource axis carries a `field` vs `stock` flag.** A *field*
    resource (photosynthate, and later mana) is ambient — tapped but not
    depleted by consumption, so it drives no bottom-up cap. A *stock* resource
    (animal-prey, detritus, and later souls) is depletable, and *that* is what
    drives the trophic coupling (§1.4). v1 already needs this distinction;
    naming it explicitly is honest now and future-proof.
  - **The zero vector is legal** — a species sustained *outside* the trophic
    economy (a construct/golem, an immortal). No v1 species uses it; the model
    must not forbid it.

Everything else is **derived** from `(niche, mass)`, authored nowhere:

- **guild** = overlap of two species' niche vectors (Pianka / cosine niche
  overlap). Competition is therefore *graded* by overlap — a weighted graph,
  not a hard partition. A named guild, when a fact needs one, is a *rendering*
  (cluster / argmax over the vectors), not an authored category.
- **trophic_level** = the standard fractional trophic level, `1 + Σ(diet
  fraction × prey level)`, from the induced food web. Omnivores land between
  integers (≈2.5); autotrophs at 1; scavengers are off-chain (the detritus
  axis, incomparable in the food-chain order).
- **predation edges** = a species with an active `animal-prey` niche axis
  preys on species whose body falls within its mass window (§1.4).
- **a species registers as a *resource to others* by its own derived trophic
  identity** — an autotroph body contributes to plant-forage supply; a
  heterotroph body contributes to animal-prey supply (mass-windowed). There is
  **no separate "edibility" field**; being a body of mass `M` makes a species
  prey to size-appropriate predators.

This is fewer authored fields than either DRAFT candidate, it is ecologically
honest (omnivores, scavengers, autotrophs all fall out for free), and it is
the representation the generative-species campaign (§8) needs.

The distinction **fundamental vs realized** rides for free and gives us
coarse-constrains-fine: the authored `niche` is the *fundamental* niche (what
a species *can* use); the *realized* role is derived per cell from the
assemblage actually present. A species' realized role in a cell follows a
small state machine — `dormant-apex` (authored high but no prey present) →
`active-apex` (prey present: caps K, casts shadow) → `competitor` →
`refuge-basal` (outcompeted, persists only where dominants are hostility-
zeroed) → `absent`. `dormant-apex` is the **correct resting state**, not dead
code: an apex with `prey_biomass = 0` self-limits (effective K collapses to
the floor) by construction. This dissolves the "trophic coupling ships
dormant on the goblinoids" worry — dormancy is the machine's default, correct
by construction, and activates the instant a predator + its prey coexist.

### 1.2 Footprint → grain → the stack

On the fixed globe mesh the footprint needs no new geometry: footprint =
**cells-per-individual** from `home_range(mass)`, so a big species has sub-unit
per-cell density spread across its range, a small species has high per-cell
density. A cell's state is therefore a **per-species density vector** — the
stack — with big species fractional. (The room mesh, MAP-30, can make the
grain literal later; not required.)

### 1.3 The coexistence share — the K^β temperature (the heart)

Within a guild (species whose niche vectors overlap), a cell's capacity is
**shared** by a temperature-controlled normalized rule — the multinomial-logit
/ Boltzmann occupancy / cellular-radio SINR law, with the denominator weighted
by pairwise niche overlap `w_sj = overlap(niche_s, niche_j)`:

```
  share_s(cell) = capacity(cell) * K_s^β / ( Σ_j w_sj · K_j^β + floor^β )
  density_s     = share_s, zeroed if below the viability floor
```

- **β is the master knob.** β→∞ = winner-take-all (monoculture); β→0 =
  uniform (oatmeal); a tuned interior β = realistic coexistence (a clear local
  dominant, graded rivals, a long thin tail). The `floor` is the noise term
  below which a species is simply absent. This single parameter answers "how
  hard does competition bite," and it is **calibrated to a target per-cell
  diversity** (rank-abundance slope), not chosen by taste. Overlap weights
  shape *who* competes; β alone sets *how hard* — β stays the single knob.
- The sharing is a normalization, so more competitors thin everyone —
  coexistence with dominance, never exclusion. This *is* "constant strife,
  many species in the same place."

### 1.4 Bidirectional trophic coupling

Species are coupled across trophic levels via the derived food web (§1.1):

- **Bottom-up (prey supports apex):** a predator's *effective* K is capped by
  the prey biomass beneath it — `K_pred_eff = K_pred_climate × prey_biomass`,
  where `prey_biomass` sums the densities of species whose body falls in the
  predator's **mass window** (predator/prey mass ratio) and whose trophic level
  is strictly lower. A dragon can only exist where there is enough to eat.
- **Top-down (apex shadows prey):** a predator's presence suppresses (does not
  zero) the density of its prey species — commensal tenancy, the dragon's
  shadow.

Coupling is applied **coarsest-grain-first / highest-level-first** (a
load-bearing order: the prey band must read the apex shadow, so the apex band
packs first). Over time this same coupling makes the *history* campaign
oscillate (predator–prey cycles).

### 1.5 Soft capacity + overflow

Niches fill **logistically** toward capacity (diminishing returns) and spill
excess pressure to neighbours — the `flow` hydrology, made soft. Crowding
produces emigration *pressure* rather than a hard wall; this pressure field is
the seam the history campaign reads for displacement.

### 1.6 Species-relative rendering

The invariant currency is **mass** (an ecological-footprint budget). Headcount
is a species-specific *rendering* of a mass share: `headcount_s = mass_share_s
/ body_mass_s`, then expressed in that species' natural unit — individuals for
goblins, "a lone dragon lairs here" when the share is sub-one, colony-extent
for myconids/hive-folk. This dissolves "524 dragons" and "1 myconid."

### 1.7 Settlements: tier × composition (the periodic grid)

Discrete settlements condense from the stack with **two orthogonal, derived
classifications**:

- **Tier** = total **mass** in the catchment (`Σ_s density_s × mass_s`) →
  Zipf-by-mass. Central-place theory gives the hierarchy a derivable shape.
- **Composition** = the *shape* of the local per-species density vector:
  concentrated (mono-species) ↔ broad (cosmopolitan).

The periodic grid of tier × composition predicts the structure the user
described: a few **cosmopolitan metropolises** (fertile mega-hubs many species
share), **mono-species metropolises** (a niche so rich for one species others
are near-zero there — the dwarven deep-city), and — the grid's non-obvious
prediction — **cosmopolitan *small* settlements only at crossroads** (low mass
but high per-species K-diversity: frontier trading posts). Size distribution is
fractal: settlement sizes Zipfian by mass, each settlement's species mix
Zipfian by the same β family.

### 1.8 The three derived byproducts (first-class outputs)

- **Strife field.** Where several overlapping-guild species have *comparable*
  K (the β denominator is large, no one dominates), contest is maximal. This
  single derived field is simultaneously where cosmopolitan settlements form,
  where ecotones/frontiers lie, and where the history campaign's wars and
  displacement ignite. Contest is highest where fitnesses are *balanced*, not
  where K is highest.
- **Wilderness.** The interstitial capacity no species dominates — the
  packing's *fragmentation* (borrowed from OS slab/buddy allocators) — is not
  waste; it is the untamed marches between territories.
- **Refugia.** Cells where the normally-dominant species falls below the
  viability floor (hostility zeroes its K — underground, alpine, arid) while a
  normally-outcompeted species does not: the weak's strongholds (the dwarf in
  the deep), derivable by comparing each species' K to the floor. Refugia are
  also the **reversibility reservoir** the history campaign recolonizes from
  after a displacement, keeping extinction non-trivial.

## 2. The menagerie (the roster expansion)

The current roster is four goblinoids — goblin, kobold, hobgoblin, bugbear
(`domains/species/src/lib.rs`) — all comparable-scale omnivorous humanoids. On
that roster the trophic coupling and cross-scale rendering are **dormant and
uncalibratable**: no species is a true apex, mass spread is modest, and the
"stack" never stacks. The engine would ship unable to demonstrate its headline
behaviours.

This campaign therefore adds a deliberate **~8–12 axis-spanning menagerie**,
drafted offline by a model and committed as authored `SpeciesDef`s (decision
0009, "models author, dice roll" — generation is an offline authoring tool
whose output is committed and drift-checked). The set spans:

- **scale** (`mass`): from a tiny high-density species to a dragon-tier apex
  at fractional per-cell density;
- **niche**: at least one photosynthate autotroph (BIO-15 flavour), one
  detritus/carrion feeder (off-chain — a myconid clade), one pure animal-prey
  apex predator, plus mixed omnivores;
- **psych / perception / articulation**: genuinely varied — nocturnal,
  keen-night-vision, tonal, exotic-manner, non-hierarchic sociality — so
  language and religion divergence deepen alongside coexistence.

Each new multi-member family needs its proto ancestral articulation vector
(the `every_multi_member_family_has_a_proto` invariant in
`domains/species`). The drafted roster is brought to Nathan for sign-off
**before** it is committed — authoring the world's creatures is a taste call.

The roster expansion's cost is not the authoring (bounded, model-drafted) but
the **downstream blast radius**: the roster feeds phonology, proto-language,
the repl, the Lab (metrics / schema / runner / charts), worldgen, and the
calibration batteries (`calibration.rs`, `gathering_calibration.rs`,
`branches_family_calibration.rs`, `species_worlds.rs`, `depth_ladder.rs`) plus
the 1000-seed census — all of which re-pin on any roster change. That cost is
roughly *fixed per roster-change event*, which is why the expansion is
deliberate (a timid +2 would pay nearly the full cost) and why it is folded
into this campaign's single close-of-campaign census regen.

## 3. Staged construction (attribution-clean)

The engine and the roster are two large changes; landing them together would
muddy calibration (is the diversity off because of β, or the new species?).
The divergence method the project uses on worlds applies to our own build:

- **Stage A — the engine, on the known baseline.** Build the full mechanism
  (§1) and the niche derivation; author `mass`/`niche` for the **4 goblinoids
  only**; calibrate and **freeze β** against that known four-species baseline.
  One variable.
- **Stage B — the menagerie, against a frozen engine.** Draft + commit the
  ~8–12 menagerie (§2); re-measure. The roster is now the *single* new
  variable against a frozen engine. One census regen, at campaign close.

## 4. Architecture

Extends the `demography` crate (kernel-only), plus the two ecological fields on
`SpeciesDef` in `domains/species`. The interim `condense_tagged` (independent
per-species) is replaced by the coexistence packer; the K-field, `flow`,
single-field `condense`, and the founder floor stand.

```
domains/species/
  lib.rs                 SpeciesDef += mass: Mass, niche: ResourceVector;
                         ResourceVector newtype; menagerie species + family protos
domains/demography/
  carrying_capacity.rs   (from the-gathering; unchanged)
  flow.rs                (from the-gathering; reused for soft-overflow)
  niche.rs          NEW  derive guild (overlap) / trophic_level (fractional) /
                         predation edges (mass window) from (niche, mass)
  footprint.rs      NEW  home_range(mass) -> grain
  coexist.rs        NEW  the overlap-weighted K^β share + trophic coupling +
                         soft-capacity; produces the per-cell density stack
  stack_condense.rs NEW  settlements from the stack: tier (mass) + composition
                         vector + species-relative rendering; replaces the
                         interim condense_tagged
  byproducts.rs     NEW  strife field, wilderness, refugia (derived fields)
  render.rs              (extend: stack / strife / refugia debug maps)
windows/worldgen/        rewire settlement genesis to the stack condensation;
                         commit composition + the derived fields' salient facts
```

The composition vector and the derived fields are **in-memory** on the report
(re-derived, not serialized — the `the-gathering` doctrine); only *salient*
summary facts reach the ledger (a settlement's dominant + a bounded,
salience-gated secondary list; a named refugium; a strife frontier) —
mirroring paleoclimate's summary-facts discipline, never per-cell dumps.

## 5. Verification & calibration

Calibration-checked, preregistered (decision 0016):

- **β to per-cell diversity (headline).** Tune β once (Stage A, on the
  goblinoids) so the mean per-cell rank-abundance slope matches a target (a
  few dominant + a graded tail); this is the monoculture↔oatmeal calibration.
  Freeze β. Re-measured, not re-tuned, in Stage B.
- **Zipf-by-mass of settlements.** Now meaningful (size = mass, composition
  real): the settlement mass distribution should be rank-size. Preregistered.
- **Niche-derivation sanity (unit-tested):** an omnivore's derived trophic
  level lands strictly between integers; an autotroph at 1; a detritivore
  off-chain (incomparable); guild overlap is symmetric and in `[0,1]`;
  predation respects the mass window (no predator eats prey outside it).
- **Coexistence invariants (unit-tested):** no cell is monoculture at the
  tuned β unless a single species is truly viable there; no "524 dragons"
  (mass-rendered counts bounded by body mass); predator density ≤ prey-supported
  cap (trophic bottom-up); every species retains its founder refugium.
- **The periodic-grid predictions as Lab checks:** cosmopolitan settlements
  concentrate at high-K multi-species overlaps; mono-species metropolises at
  rich single-species niches; cosmopolitan-small settlements at high-diversity
  low-mass crossroads (the strife field's low-mass cells).
- **Refugia / wilderness sanity:** refugia track hostility-zeroed dominants;
  wilderness fraction is non-trivial and tracks fragmentation.
- **Determinism / cross-platform:** the packer draws nothing new; transcendentals
  (the `^β` power, the overlap cosine) route through `hornvale_kernel::math`;
  deterministic tie-breaks; quantize only at emit.

## 6. New concepts to register (idea-registry rows)

Capture as rows in `book/src/frontier/idea-registry.md` (do not lose these):

- **The resource-utilization niche vector** — one authored per-species vector
  from which guild, trophic level, and predation all derive (author O(S),
  derive O(S²)). Kin to BIO-2.
- **Footprint / mass-scaled home range** (grain of the stack) — kin to BIO-2.
- **The competition temperature β** — the monoculture↔oatmeal master knob (the
  overlap-weighted K^β / logit / Boltzmann / SINR share).
- **Bidirectional trophic coupling** — prey-supported predator cap + apex shadow,
  over the derived food web with mass-windowed prey selection.
- **The strife field** — balanced-fitness contest as a first-class derived
  output feeding cosmopolitanism and MAP-9 conflict ignition.
- **Wilderness from packing fragmentation.**
- **Refugia as derived strongholds + reversibility reservoir.**
- **Metabolism over non-material resource-fields** — the resource basis is an
  open set of tappable Fields; fantasy metabolisms (magical / psychic / belief:
  mana, dread, worship) are later tiers that light up when their field exists,
  inert otherwise (coarse-constrains-fine). Kin to **BIO-14** (the fear-eater
  deified as a devil): this campaign already emits the first non-material such
  field — **the strife field is a proto dread-field** a future psychovore would
  tap, delivered through the religion seam (phenomena consumed without knowing
  their source). Note the same spine governs provider tiers, the dormant-apex
  resting state, and fantasy metabolism alike.
- **Generative species at scale** (the §8 spin-out) — see below.

## 7. Dependencies & prerequisites

- **`mass` + `niche` on `SpeciesDef`** (§1.1) — the BIO-2 down-payment; two
  authored fields (confirmed absent today), plus a `ResourceVector` newtype.
- **The-gathering's carrying-capacity field** — the input this all reads
  (shipped by the predecessor campaign).
- Optional later: **PSY-1** species tolerance (some species won't co-settle
  regardless of K) refines composition; not required for v1.

## 8. Relationship to other campaigns

- **← the-gathering:** consumes its per-species K field; replaces its interim
  condensation.
- **→ history (deep-time re-packing):** re-runs this packer as K drifts;
  reads the strife field (ignition), the overflow-pressure field (displacement),
  and refugia (recolonization). Displacement cascades and MAP-9 contact events
  fall out of the re-pack deltas.
- **→ MAP-9 (contact/exchange):** the strife field is its frontier map; the
  composition vector tells it which peoples meet where.
- **→ fantasy metabolism (future tier):** the open resource-field basis (§1.1)
  lets magical/psychic/theovorous species register their own resource-fields
  (mana, dread, belief) and tap them via `niche`, with no `SpeciesDef`
  refactor. Material ecology is tier-0; each fantasy metabolism is a higher
  tier, inert until its field exists. This campaign ships the prototype: the
  strife field is the first non-material resource-field (a proto dread-field).
  Registered as an idea-registry row (§6).
- **→ generative species at scale (future BIO campaign, spun out here):** the
  regime where `mass`/`niche` become seed-drawn or model-authored-at-scale and
  the Lab's calibration goes *distributional* (statistics over a generated
  population, not per-species exact pins), toward thousands of species. This
  campaign motivates it and builds the O(S)-authored / O(S²)-derived
  representation it needs; it does not build the generator. Registered as an
  idea-registry row (§6).

## 9. Resolved questions (were open in the DRAFT)

- **How literal is the grain?** v1: fractional per-species density on the fixed
  L6 mesh; the room mesh (MAP-30) can make it literal later. Revisit if it
  reads poorly.
- **Trophic role: authored vs derived?** Resolved (§1.1): neither the DRAFT
  enum nor a mass-derived level. Author a single `niche` resource vector;
  derive guild, trophic level, and predation from `(niche, mass)`.
- **β: one global value, or per-guild?** Start global (one knob); allow
  per-guild β only if calibration demands it.
- **Composition facts: how many species per settlement reach the ledger?**
  Dominant + a bounded, salience-gated secondary list, never the full vector.
