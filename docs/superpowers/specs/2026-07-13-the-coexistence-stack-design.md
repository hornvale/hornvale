# The Coexistence Stack (MAP-22) — Design

**Status:** DRAFT (design captured; not yet approved for execution). This is
the next campaign after *the-gathering* (the carrying-capacity field), and it
builds directly on that field. It operationalizes and refines the MAP-22
frontier essay (*the biogeography of coexistence*); per decision 0031, this
spec binds over the essay where they differ.
**Date:** 2026-07-13
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
- The bridge in the arc **field (the-gathering) → coexistence stack (this) →
  history (re-packing over deep time)**.

## Design principles

- **Derive, don't evolve — here.** The equilibrium stack is a closed-form
  function of the per-species K fields. It draws nothing from the seed beyond
  what the fields already consumed. History (the evolved re-pack) is a
  separate campaign; this one is the prior it starts from and is bounded by
  (coarse constrains fine).
- **One packing operator, two lifetimes.** Invoked once against today's K =
  this campaign's derived equilibrium. Re-invoked over a drifting K field =
  the history campaign, whose *deltas between successive packs are the
  displacement/migration/subjugation events*. Same shape as `flow` (settlements
  once, abandonment when re-flowed) and tectonics.
- **Bands modular, within-cell sharing atomic.** Each footprint-grain is
  computed independently (parallelizable); within a cell the competition
  sharing is a single coupled normalization over all present species.

## 1. The mechanism

### 1.1 Footprint → grain → the stack

Each species has a **home-range footprint** scaling with body mass (Kleiber:
metabolic demand → territory scales super-linearly with mass; BIO-2 names this
as the thing that "rescales settlement density"). On the fixed globe mesh this
needs no new geometry: footprint = **cells-per-individual**, so a big species
has sub-unit per-cell density spread across its range, a small species has high
per-cell density. A cell's state is therefore a **per-species density vector**
— the stack — with big species fractional. (The room mesh, MAP-30, can make
the grain literal later; not required.)

**Input required (prerequisite):** a per-species **body-mass / scale** scalar.
Confirmed: `hornvale_species::SpeciesDef` today carries `psych`, `perception`,
`articulation`, `family`, and the social-rung words — but **no mass, scale, or
trophic field**. This campaign needs a small **BIO-2 down-payment**: one
authored `mass` (or `hit_dice`) field per species on `SpeciesDef`.

### 1.2 The coexistence share — the K^β temperature (the heart)

Within a grain-band, a cell's capacity is **shared** among the competing
species by a temperature-controlled normalized rule — the structural shape is
the multinomial-logit / Boltzmann occupancy / cellular-radio SINR law:

```
  share_s(cell) = capacity(cell) * K_s^β / ( Σ_j K_j^β + floor^β )
  density_s     = share_s, zeroed if below the viability floor
```

- **β is the master knob.** β→∞ = winner-take-all (monoculture); β→0 =
  uniform (oatmeal); a tuned interior β = realistic coexistence (a clear local
  dominant, graded rivals, a long thin tail). The `floor` is the noise term
  below which a species is simply absent. This single parameter answers "how
  hard does competition bite," and it is **calibrated to a target per-cell
  diversity** (rank-abundance slope), not chosen by taste.
- The sharing is a normalization, so more competitors thin everyone —
  coexistence with dominance, never exclusion. This *is* "constant strife,
  many species in the same place."

### 1.3 Bidirectional trophic coupling

Species are not independent across bands:

- **Bottom-up (prey supports apex):** an apex species' *effective* K is capped
  by the prey biomass beneath it — `K_apex_eff = K_apex_climate × prey_biomass`.
  A dragon can only exist where there is enough to eat.
- **Top-down (apex shadows prey):** an apex's presence suppresses (does not
  zero) the density of the bands below — commensal tenancy, the dragon's
  shadow.

Coupling is applied **coarsest-grain-first** (a load-bearing order: the prey
band must read the apex shadow, so the apex band packs first). Over time this
same coupling makes the *history* campaign oscillate (predator–prey cycles).

### 1.4 Soft capacity + overflow

Niches fill **logistically** toward capacity (diminishing returns) and spill
excess pressure to neighbours — the `flow` hydrology, made soft. Crowding
produces emigration *pressure* rather than a hard wall; this pressure field is
the seam the history campaign reads for displacement.

### 1.5 Species-relative rendering

The invariant currency is **mass** (an ecological-footprint budget). Headcount
is a species-specific *rendering* of a mass share: `headcount_s = mass_share_s
/ body_mass_s`, then expressed in that species' natural unit — individuals for
goblins, "a lone dragon lairs here" when the share is sub-one, colony-extent
for myconids/hive-folk. This dissolves "524 dragons" and "1 myconid."

### 1.6 Settlements: tier × composition (the periodic grid)

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

### 1.7 The three derived byproducts (first-class outputs)

- **Strife field.** Where several same-grain species have *comparable* K (the
  β denominator is large, no one dominates), contest is maximal. This single
  derived field is simultaneously where cosmopolitan settlements form, where
  ecotones/frontiers lie, and where the history campaign's wars and
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

## 2. Architecture

Extends the `demography` crate (kernel-only). The interim `condense_tagged`
(independent per-species) is replaced by the coexistence packer; the K-field,
`flow`, single-field `condense`, and the founder floor stand.

```
domains/demography/
  carrying_capacity.rs   (from the-gathering; unchanged)
  flow.rs                (from the-gathering; reused for soft-overflow)
  footprint.rs      NEW  home_range(mass) -> grain; species mass input
  coexist.rs        NEW  the K^β temperature share + trophic coupling +
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
summary facts reach the ledger (a settlement's dominant + secondary species; a
named refugium; a strife frontier) — mirroring paleoclimate's summary-facts
discipline, never per-cell dumps.

## 3. Verification & calibration

Calibration-checked, preregistered (decision 0016):

- **β to per-cell diversity (headline).** Tune β once so the mean per-cell
  rank-abundance slope matches a target (a few dominant + a graded tail); this
  is the monoculture↔oatmeal calibration. Freeze β.
- **Zipf-by-mass of settlements.** Now meaningful (size = mass, composition
  real): the settlement mass distribution should be rank-size. Preregistered.
- **Coexistence invariants (unit-tested):** no cell is monoculture at the
  tuned β unless a single species is truly viable there; no "524 dragons"
  (mass-rendered counts bounded by body mass); apex density ≤ prey-supported
  cap (trophic bottom-up); every species retains its founder refugium.
- **The periodic-grid predictions as Lab checks:** cosmopolitan settlements
  concentrate at high-K multi-species overlaps; mono-species metropolises at
  rich single-species niches; cosmopolitan-small settlements at high-diversity
  low-mass crossroads (the strife field's low-mass cells).
- **Refugia / wilderness sanity:** refugia track hostility-zeroed dominants;
  wilderness fraction is non-trivial and tracks fragmentation.
- **Determinism / cross-platform:** the packer draws nothing new; transcendentals
  (the `^β` power) route through `hornvale_kernel::math`; deterministic
  tie-breaks; quantize only at emit.

## 4. New concepts to register (idea-registry rows)

Capture as rows in `book/src/frontier/idea-registry.md` (do not lose these):

- **Footprint / mass-scaled home range** (grain of the stack) — kin to BIO-2.
- **The competition temperature β** — the monoculture↔oatmeal master knob (the
  K^β / logit / Boltzmann / SINR share).
- **Bidirectional trophic coupling** — prey-supported apex cap + apex shadow.
- **The strife field** — balanced-fitness contest as a first-class derived
  output feeding cosmopolitanism and MAP-9 conflict ignition.
- **Wilderness from packing fragmentation.**
- **Refugia as derived strongholds + reversibility reservoir.**

## 5. Dependencies & prerequisites

- **Per-species body mass / scale** (§1.1) — a small BIO-2 down-payment; one
  authored field on `SpeciesDef` (confirmed absent today).
- **Per-species trophic role** (§1.3) — which species are apex/predator vs
  prey/producer, to build the trophic coupling. Confirmed absent on
  `SpeciesDef` today; v1 can derive apex-ness from body mass (bigger = higher
  trophic level), with an authored `diet`/`trophic` field as the higher-fidelity
  option.
- **The-gathering's carrying-capacity field** — the input this all reads
  (shipped by the predecessor campaign).
- Optional later: **PSY-1** species tolerance (some species won't co-settle
  regardless of K) refines composition; not required for v1.

## 6. Relationship to other campaigns

- **← the-gathering:** consumes its per-species K field; replaces its interim
  condensation.
- **→ history (deep-time re-packing):** re-runs this packer as K drifts;
  reads the strife field (ignition), the overflow-pressure field (displacement),
  and refugia (recolonization). Displacement cascades and MAP-9 contact events
  fall out of the re-pack deltas.
- **→ MAP-9 (contact/exchange):** the strife field is its frontier map; the
  composition vector tells it which peoples meet where.

## 7. Open questions

- **How literal is the grain?** Fractional per-cell density on the fixed L6
  mesh vs. actual multi-resolution via the room mesh (MAP-30). v1: fractional
  density on L6; revisit if it reads poorly.
- **Trophic role: authored vs derived?** Simplest v1: derive apex-ness from
  body mass (bigger = higher trophic level); a `diet`/`trophic` authored field
  is the higher-fidelity option.
- **β: one global value, or per-guild?** Start global (one knob); allow
  per-guild β if calibration demands it.
- **Composition facts: how many species per settlement reach the ledger?**
  Likely dominant + a bounded secondary list (salience-gated), never the full
  vector.
