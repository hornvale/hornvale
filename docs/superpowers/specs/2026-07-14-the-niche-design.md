# The Niche — Habitat as Fit (design)

**Status:** DRAFT — for G3 review (design converged over ten ideonomy passes;
not yet approved for execution). Under campaign-autopilot: this is the G3
hard-stop.
**Date:** 2026-07-14
**Campaign:** The Niche (working title). The keystone of the Coexistence Stack's
Phase 2 — it makes the shipped packer's output *biogeographic* and the
menagerie *meaningful*, and it is the substrate for the thousand-species
vision.

## Problem

The Coexistence Stack engine (Phase 1, merged @1bc30ce) packs a per-cell
multi-species density stack from **per-species carrying-capacity fields**
`K_s(cell)`. But those fields are barely differentiated: `carrying_capacity`
uses one hardcoded temperature optimum (22 °C) and NPP response for **every**
species, and `species_carrying_input` only modulates freshwater weight and
hostility tolerance by psychology. So every species' K is near-*proportional*
to the same NPP field.

Observed directly (seed 42, task-C inspection): **all 276 settlements share an
identical composition** (`kobold .56 / goblin .31 / hobgoblin .08 / bugbear
.06`), strife is flat, wilderness 0, refugia 0. The campaign's entire visible
payoff — contested frontiers, refugia, cosmopolitan vs mono settlements —
is **absent**, because the roster species all thrive in the same places. The
engine is provably *correct* (the `pack_produces_spatial_structure_when_
species_differ` guard passes with anti-correlated K); the K fields are simply
niche-blind.

**The `niche` resource vector already on `SpeciesDef` drives guild competition
and trophic level — but not *where a species lives*.** An autotroph and an apex
predator get the same K field. Authoring twelve differentiated species without
this would still produce oatmeal, because they would all peak at 22 °C on the
same NPP field.

The spec §1.1 of the Coexistence Stack already promised the fix — "each niche
axis couples to substrate the world already has: photosynthate → insolation,
plant-forage → NPP, animal-prey → lower-tier densities, … mineral → terrain" —
but it was never implemented. This campaign implements it, and generalizes it
into a habitat model built to scale to thousands of species.

## Goal

Derive each species' carrying-capacity field **from its traits coupled to the
world's environmental fields**, so that where a creature lives falls out of
what it is and where the world affords it — flexible and powerful enough to
sustain thousands of distinct species, from plankton to gods.

- **Biogeography emerges:** composition varies across space (each species
  dominant in its own stronghold), producing real frontiers, refugia,
  endemism, and cosmopolitan crossroads.
- **Scales to thousands:** the world supplies ~10 shared environmental fields;
  each species adds only a small trait vector (author O(S); the O(S²) ecology
  derives, per the shipped engine).
- **From plankton to gods on one axis:** a single *sovereignty* trait carries
  a creature continuously from environment-placed (plankton) through
  domain-holding legendary beasts to omnipresent deities.
- **The packer is unchanged.** It already consumes `per_species_k`; this
  campaign only stops computing that input flatly.

## Design principles

- **Discovered fields, invented coupling.** The environmental fields are
  *discovered* from the already-shipped simulation (sky, climate, terrain/The
  Ground, and the stack itself). A species' *coupling* to them is the authored
  or generated trait. This division is the scalability line: thousands of
  species tap the same ~10 fields.
- **A generic suitability engine, species-first.** `entity-niche ×
  substrate-registry → scored field` is the same shape that already places
  settlements (carrying capacity), soils (The Ground), and biomes (climate).
  Build it entity-agnostic in the kernel; species are its first client;
  cultures, religions, and materials are later clients (a belief thrives where
  conditions favor it — the divergence method).
- **Real skeleton, fantasy flesh.** The math is discovered ecology (Hutchinson's
  n-dimensional niche, Liebig's minimum, Shelford's law of tolerance); the
  fantasy refinements (sovereignty, aversion, mana/dread fields) are invented
  on top — coarse constrains fine.
- **Coarse constrains fine / scale-invariant.** The same primitive runs at the
  room scale (microhabitat), the globe cell (biogeography), and the planetary
  scale (habitability), each constraining the finer.

## 1. The model — habitat as fit

A species is a **point in trait-space**. Its carrying-capacity field is the
product of a **resource-supply** term (is there food/energy here?) and a
**condition-tolerance** term (can I — or do I choose to — live here?):

```
  K_s(cell, t) = ResourceSupply_s(cell, t) × ConditionResponse_s(cell, t)
```

- **Resource fields** are *supply* — monotone, "more is better", and
  **substitutable** (an omnivore lives several ways). Aggregated by a
  **saturating weighted sum** (Type-II functional response — intake plateaus in
  super-abundance):

  ```
  ResourceSupply_s = saturate( Σ_r  uptake_s[r] · resourceField_r(cell,t) )
  ```
  `uptake_s` is the existing `niche` vector (reused — it also drives guild and
  trophic level). An autotroph weights only `photosynthate` → supply tracks
  insolation.

- **Condition fields** are *filters* — unimodal (an optimum with tolerance) and
  **non-substitutable** (perfect moisture cannot cancel lethal cold).
  Aggregated by a **product** over axes (the multivariate Gaussian = Hutchinson's
  niche as an ellipsoid; a per-axis `min`/Liebig is the crisper, box-shaped
  alternative — see Open Questions):

  ```
  ConditionResponse_s = Π_c  response_s,c( condField_c(cell,t) )
  ```

The resource term sets the *magnitude* (how many individuals); the condition
term is a `[0,1]` *fraction of potential realized*. This is the **fundamental
niche**; the shipped packer then *realizes* it through competition.

## 2. The condition response curve — sovereignty × devotion × aversion

Each per-axis response is not a bare Gaussian. It carries the machinery that
lets it describe everything from a cave cricket to an ancient dragon:

```
  response_s,c(x) = floor_s,c
                  + (1 − floor_s,c) · devotion_s,c · gaussian(x; opt_s,c, width_s,c)
                  − aversion_s,c(x)
```

- **`opt`, `width`** — the optimum and tolerance breadth (Hutchinson/Shelford).
- **`floor` = sovereignty (the might knob).** `floor = 0` → a **hard
  constraint** (excluded outside tolerance — a cave cricket dies in sunlight).
  `floor > 0` → a **soft preference** (never excluded, merely denser at the
  optimum — a dragon *prefers* its crag but survives a thicket). The floor is
  raised by a species' **sovereignty** (§3).
- **`devotion` = preference amplitude.** How strongly it favors its optimum
  *given* freedom. High-sovereignty + high-devotion = a domain-holder (the
  dragon on its crag); high-sovereignty + low-devotion = omnipresent-indifferent
  (a cosmic ooze, an ambient force).
- **`aversion`** — a downward trough: places a creature *could* live but shuns
  (undead shun sunlight, fire-things shun water, fey shun iron). Bidirectional
  preference.

## 3. Sovereignty — plankton to god on one axis

**Sovereignty** is the degree to which a creature's location is
*self-determined* (preference) rather than *environment-determined*
(constraint) — abstractly, its **homeostatic buffering capacity**: the
energetic/magical budget to hold internal state against external gradients. It
is **not a free knob** — it grounds in **mass + magical potency** (bigger, or
more magical, → buffers the environment better → higher floors). "Preference is
the luxury of the unconstrained."

One continuous axis spans the whole biota:

```
  sovereignty 0 ─────────────────────────────────────────────► sovereignty 1
  plankton         beast        legendary creature            deity
  (env. places     (semi-       (unconstrained + peaked        (omnipresent;
   it entirely)     chosen)      = a named DOMAIN/lair)         temple-peaked)
```

- **Not every axis is buffer-able.** Sovereignty relaxes *physiological*
  constraints (temperature, aridity). Some constraints stay **hard regardless
  of might** — a storm giant cannot *fit* in a cave, a whale cannot climb a
  mountain. Condition axes are tagged `buffer-able` (floor raised by
  sovereignty) vs `hard` (floor stays 0).
- **Devotion generates place-identity.** A high-sovereignty, high-devotion
  creature's preference peak *is* its iconic domain — the dragon's mountain, the
  giant's cloud-peak, a god's holy site — unifying this model with the religion
  domain at the high end.

## 4. The generative prior — a trade-off manifold, grown by descent

Sampling species with independent random traits yields **super-species**
(generalists that are also specialists) that homogenize the map. Real
trait-space forbids this through **trade-offs** — no free lunch:

```
  peak_amplitude × tolerance_breadth ≈ constant   (can't be high-peak AND broad)
  Σ uptake_weights ≈ a fixed budget               (broad diet ⇒ mediocre at each)
```

- Normal creatures are sampled **on** this manifold (specialist ↔ generalist).
  The **mighty** spend *sovereignty* to buy exemption from it (high-peak *and*
  broad — the legendary regime). Sovereignty is literally "how far off the
  manifold."
- The manifold is the **same one BIO-2's fast–slow life-history continuum**
  lives on.
- **Generation is phylogenetic, not IID.** New species are drawn as
  **descent with mutation** from ancestors — reusing the `family`/proto system
  already on `SpeciesDef` — forming a tree. This yields coherent **adaptive
  radiations** (a clade filling adjacent niches) rather than noise. The manifold
  constrains *where* on trait-space a species may land; phylogeny generates *how
  it got there*.

## 5. The substrate field registry — discovered from the shipped sim

Almost every field this model needs is **already computed** by a shipped
domain. The habitat model is a *consumer*.

```
FIELD            KIND        SOURCE (shipped)                       axis-tag
─────────────────────────────────────────────────────────────────────────────
insolation       resource    astronomy (flux × latitude × season)  —
NPP              resource    climate (Miami temp×moisture)          —
lithic richness  resource    terrain / The Ground (lithology)       —
prey biomass     resource    THE STACK ITSELF (lower-trophic dens.) endogenous
detritus         resource    stack biomass × decay(temperature)     endogenous
─────────────────────────────────────────────────────────────────────────────
temperature      condition   climate (mean_temperature_at)          buffer-able
moisture         condition   climate (moisture_at)                  buffer-able
elevation        condition   terrain (elevation_at)                 hard
unrest/hazard    condition   terrain (tectonic unrest)              buffer-able
seasonality      condition   sky (axial tilt → annual swing)        buffer-able
salinity/medium  condition   terrain/climate (marine vs land)       hard (later)
```

**Structure** (extends `kernel::ecology`'s resource-axis registry):
`SubstrateField { id, label, kind: Resource | Condition(buffer-able|hard),
source, compute: fn(cell, t) -> f64 }`. **Open** — fantasy fields (mana as
resource, magic-saturation as condition, a sanctity optimum) register later;
fantasy metabolism becomes fantasy *habitat* for free.

**The endogenous cell is the gift:** the prey/detritus fields come from the
stack itself, so the bottom-up trophic coupling the packer already does **is
just a substrate field that happens to be endogenous** — one mechanism, not
two (see §7).

## 6. Architecture

```
kernel/src/ecology.rs   (extend)  the generic SUITABILITY ENGINE:
                                  SubstrateField registry (Resource|Condition,
                                  buffer-able|hard); the condition response
                                  (floor/opt/width/devotion/aversion); Niche
                                  scores any entity against the registry.
domains/{climate,terrain,astronomy}
                        (EXPOSE)  per-cell fields they already compute
                                  (temperature, moisture, insolation, elevation,
                                  lithology) as queryable SubstrateField sources.
domains/species/src/lib.rs (extend) SpeciesDef gains: condition_niche
                                  (per-axis opt/width/devotion), sovereignty
                                  (or its inputs: mass — present — + potency),
                                  aversions. `niche` (resource) is reused.
windows/worldgen        (NEW LAYER) assemble the registry; compute per-species
                                  K = supply × condition-response — REPLACES the
                                  flat NPP `species_carrying_input`.
domains/demography      (UNCHANGED) the packer still consumes per_species_k;
                                  the input just gets richer.
```

The whole engine merged in Phase 1 **does not change**. The only new layer is
worldgen (the composition root, where world-fields meet species-traits).

## 7. The fundamental → realized → endogenous loop

`K` of an apex depends on prey density, which is realized by the packer, which
depends on prey K … a fixpoint. It resolves **without iteration**: the trophic
web is a DAG (conditions → producers → herbivores → predators → apex), so
compute K **bottom-up by trophic level** — producer K from abiotic conditions,
then herbivore K reading realized producer density as its prey field, and so
on. This is exactly the highest-level ordering the shipped `couple_trophic`
already uses; the one non-DAG case (mutual predation) is the bounded-pass
fixpoint the trophic-level derivation already handles. (v1 may keep
`couple_trophic`'s post-hoc prey cap; folding prey into an explicit endogenous
resource field is a cleaner refactor, noted for later.)

## 8. Scope — v1 vs the roadmap

**v1 (this campaign) — the load-bearing kernel:**
- Conditions: **temperature, moisture, insolation, elevation**. Resource: **NPP**
  (+ the existing niche-driven guild/trophic). Enough for real latitude /
  altitude / aridity biogeography.
- The full condition response (floor/opt/width/devotion/aversion), sovereignty
  grounded in mass (+ potency slot, 0 for material species), the trade-off
  manifold, the generic suitability engine.
- **Static** (annual-mean fields, `t` fixed). **Authored** species (the
  differentiated menagerie); generative sampling is designed-in but deferred.
- The packer unchanged; worldgen computes the richer K.

**Designed-in (the model already accommodates; deferred):**
- Seasonal `K(cell,t)` + the **migrate / dorm / endure** temporal-strategy axis.
- **Aversion** troughs; **skew** in tolerance curves; **buffer-able/hard** split
  (v1 may ship the split; troughs/skew later).
- **Ontogenetic niche trajectories** (a species as a path through niche-space —
  tadpole→frog) ↔ BIO-2.
- **Dispersal / reachability** (island biogeography — suitability ≠ occupancy
  under barriers) and **multi-habitat** requirements (feed-here/breed-there,
  corridors).
- **Plankton→god continuum** wired to the religion domain.
- **Phylogenetic generation** toward thousands of species.

**Later campaigns:**
- **History:** succession (bare→pioneer→mid→climax, disturbance-reset) + deep-
  time range drift (Milankovitch).
- **Living biosphere:** niche construction (beavers→wetland; keystone
  engineers) and **anthropogenic/commensal ecology** (the peoples' agriculture
  and cities rewrite the fields → rats, weeds, livestock) ↔ the settlement layer.

## 9. Verification & calibration

Preregistered (decision 0016):
- **Biogeography emerges (headline):** with a differentiated roster, per-cell
  composition **varies across space** — the `pack_produces_spatial_structure`
  guard, now driven by real niche-differentiated K rather than a synthetic
  fixture. Mean over settlements: composition entropy > a floor; not a single
  global blend.
- **Frontiers / refugia become non-trivial:** strife has spatial structure
  (contested ecotones), refugia > 0 for outcompeted specialists in their
  strongholds, wilderness fraction non-trivial — the byproducts the goblinoid
  roster left flat.
- **Response-curve invariants (unit-tested):** `response ∈ [0,1]` before
  aversion; `floor=0` excludes outside tolerance; `floor>0` never excludes; a
  high-sovereignty species is present everywhere but peaked at its optimum; a
  `hard` axis ignores sovereignty.
- **Determinism / cross-platform:** the K computation draws nothing from the
  seed; transcendentals (the Gaussian `exp`) route through
  `hornvale_kernel::math`; quantize only at emit.

## 10. Capture — idea-registry rows (the ten-pass roadmap; no idea dies)

To register in `book/src/frontier/idea-registry.md`:
- **Habitat as fit** — K = resource-supply × condition-response; niche×substrate
  suitability. (this campaign)
- **Sovereignty (might) as constraint-relaxation** — a floor that turns
  tolerance into preference; grounded in mass+magic; plankton→god on one axis.
- **Devotion / place-identity** — preference amplitude generates a legendary
  creature's iconic domain; ties to the religion domain at the high end.
- **The trade-off manifold** — specialist↔generalist; the mighty buy exemption;
  shared with BIO-2's fast–slow axis.
- **Phylogenetic species generation** — descent+mutation over the family system;
  adaptive radiations; the path to thousands of species.
- **Temporal habitat & the migrate/dorm/endure axis** — seasonal K; migration.
- **Ontogenetic niche trajectories** — a species as a path through niche-space
  (↔ BIO-2).
- **Ecological succession** — disturbance→pioneer→climax; facilitation (↔ history).
- **Niche construction** — organisms engineer their substrate (keystone species).
- **Anthropogenic / commensal ecology** — civilization rewrites the fields (↔
  settlements).
- **The generic suitability engine** — entity-agnostic niche scoring; reusable
  for cultures/religions/materials.
- **Dispersal & island biogeography** — suitability ≠ occupancy under barriers.

## 11. Relationship to other campaigns

- **← Coexistence Stack:** supplies the differentiated `per_species_k` the merged
  packer consumes; the packer is unchanged.
- **→ the menagerie:** this is what makes ~12 differentiated species produce
  biogeography rather than oatmeal — the campaign's visible payoff.
- **↔ BIO-2:** the trade-off manifold is BIO-2's fast–slow axis; ontogenetic
  trajectories are life-history; BIO-2 is unblocked by Phase 1's `Mass`.
- **↔ religion:** the plankton→god sovereignty continuum; deities as
  omnipresent, temple-peaked habitats.
- **↔ room mesh / locale:** the same suitability primitive at microhabitat scale.
- **↔ The Ground / climate / sky:** the sources of the substrate fields.
- **→ history:** succession and deep-time range drift.
- **→ generative species at scale:** phylogenetic generation.

## Open questions (flagged for G3)

- **Condition aggregation: product-of-Gaussians vs per-axis `min` (Liebig).**
  Product gives smooth ellipsoidal niches; `min` gives crisper box-shaped range
  edges. Recommend product for v1 (standard, differentiable); revisit if edges
  read too soft. *(Not a save-format decision until frozen; leads no epoch.)*
- **Sovereignty: derived from mass+potency, or an authored scalar?** Recommend
  derived (grounded, not a free knob), with an authored override for legendary
  outliers.
- **Does v1 ship the `buffer-able/hard` axis split, or defer with everything
  `hard`?** Recommend shipping the split (cheap, and it is what makes mighty
  creatures read correctly).
- **New per-cell fields to expose (insolation especially) — save-format
  surface?** Exposing already-computed fields is not itself a save-format
  change, but the K formula and its constants become save-format once frozen;
  they lead the first freeze.
