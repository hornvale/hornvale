# The Teeth — predation's approach side

**Campaign:** The Teeth
**Date:** 2026-07-21
**Status:** shipped (2026-07-21)
**Registry:** PSY-10 (the trophic engine)

## The idea

The Quarry built the fear of being eaten and The Wilding gave the meek eyes to feel
it: a wild herbivore now flees the ground where carnivores range. This campaign
builds the *other* pole of the same interaction — the **hunger** of the eater. A
carnivore is *drawn* to where its prey is, exactly as a herbivore is *repelled* from
where its predators are: the same two populations, two dual fields, opposite signs.
The Quarry read carnivore density and made a prey fear it; The Teeth reads **prey**
density and makes a carnivore hunger toward it.

## The unification — it is not a new drive

Predation-approach is the **`ANIMAL_PREY` axis of the existing hunger drive** (The
Provender), finally given its own spatial field. Today `food_value` (liveness.rs)
reads a single cell productivity (`Terrain::forage_value`, an NPP proxy) and scales
it by the creature's whole material diet weight — `weight(PLANT_FORAGE) +
weight(ANIMAL_PREY)` — so a carnivore currently "grazes" plant productivity for its
meat, which is wrong: a carnivore's food is not the biome, it is *other creatures*.
The Teeth routes the two material axes to two different fields:

- **`PLANT_FORAGE`** → `forage_value` (biome NPP) — unchanged.
- **`ANIMAL_PREY`** → a new **prey field** (`prey_pressure`) — the change.
- **`PHOTOSYNTHATE`** → the sun — unchanged.

This is the (diet · PREY) cell of the niche × field cross-product, the twin of The
Provender's (diet · resource) and The Quarry's (threat · hazard). Not a new engine —
a new *field* for an axis that already exists.

## Design

### 1. `worldgen::prey_pressure` — the dual of `predator_pressure`

A new composition-root field, structurally a mirror of `predator_pressure`
(worldgen/src/lib.rs:929): the **realized** density (the coexistence stack, not
carrying capacity — the lesson The Quarry paid for) of the **prey base** summed per
cell and normalized to `[0, 1]`. Where `predator_pressure` sums species with
`ANIMAL_PREY > CARNIVORE_THRESHOLD` (carnivores), `prey_pressure` sums the prey base:
**heterotrophic, non-carnivore, mobile-beast** species (the herbivores and
omnivore-beasts a carnivore hunts) — non-autotroph (plants are not a carnivore's
prey) and `ANIMAL_PREY ≤ CARNIVORE_THRESHOLD` (they are the eaten, not the eaters).
Realized density is competition-thinned, so the field concentrates on genuine wild
prey ground and leaves settled land quiet — the same property that made
`predator_pressure` honest. No new seeded draw (derived from the committed demography
stack); no epoch.

**[FLAG — fidelity]** v1 excludes **peoples** from the prey base (prey = wild beasts
only), so a carnivore is drawn to the *wild*, not toward settlements — symmetric with
The Wilding's wild focus, and it defers the "predators stalk towns" question to the
acute hunt tier. See Flagged Items.

### 2. The `food_value` split, carnivory-weighted and latent

`food_value(niche, terrain, room, day)` gains a prey term for the `ANIMAL_PREY` axis:

```
food_value = weight(PHOTOSYNTHATE)·sun
           + weight(PLANT_FORAGE)·forage_value(room)          // unchanged
           + weight(ANIMAL_PREY)·PREY_LATENT_SCALE·prey_value(room)   // NEW
```

`prey_value(room)` blends the injected `prey_pressure` field (via the corner-blend
`LocaleContext::blend_at` The Quarry added), `0.0` where absent. The term is
**additive** and **`PREY_LATENT_SCALE`-scaled** so that the current settled peoples —
who already clear `EAT_THRESHOLD` in place on their productive, on-water home cells
(The Confluence) and therefore never forage — stay eating in place: `food_value` only
rises, never crossing below the threshold, so their behaviour is **byte-identical**.
The prey term changes a path only for an agent that *forages* on prey-sparse ground —
a wild carnivore beast (the rust-monster) drawn up the prey gradient toward the
herbivore herds. This is The Quarry's dormancy discipline (`PREDATOR_LATENT_SCALE`)
mirrored, and it is verified, not assumed: `new --seed 42` and the seed-42 possession
walk must be byte-for-byte unchanged (the gate's artifact-drift check).

### 3. Injection — the same seam as the predator field

`LocaleTerrain` gains an optional `prey: Option<&CellMap<f64>>`, constructed
alongside the predator field (`with_calendar_and_predators` → extended, or a sibling
constructor), computed once per session/health-sim start via `worldgen::prey_pressure`
— the composition root does the cross-domain fit; the vessel is a consumer. The hot-
path discipline holds: the field is built **once** per generation, never per observe.

## Determinism

Genesis byte-identical: `prey_pressure` is derived from already-committed demography
facts — no seed draw, no new predicate, no epoch. The only behavioural change is the
vessel-layer hunger drive gaining a latent prey term, dormant for the current agents
(verified on seed 42), waking for a foraging wild carnivore. The stream-consumption
order is untouched.

## Success criteria

- `worldgen::prey_pressure` returns a deterministic `[0,1]` field concentrated on wild
  prey ground (a probe: mean, p90, and that it is low on settled cells).
- `new --seed 42` and the seed-42 possession galleries are **byte-identical** (the
  prey term is dormant for the current peoples).
- A **unit test**: `food_value` for a carnivore rises toward prey-dense ground and is
  flat for a pure herbivore (whose `ANIMAL_PREY` weight is ~0).
- An **end-to-end test**: a wild carnivore beast (high `ANIMAL_PREY`) on prey-sparse
  ground *forages toward* a prey-dense cell (the approach, live) — the mirror of The
  Wilding's `a_wild_beast_walks…`; a herbivore does not.
- The **health null-control** holds with the prey term present (a carnivore seeking
  prey is *Searching*, not distress — approach is a seek, never an alarm).

## Reserved (the acute tier — the visible hunt)

Deferred, correct-by-construction, all still PSY-10's body:

- **The kill** — the anti-symmetric outcome (one dies, one eats). v1 builds the
  *symmetric field-approach*; the KILL (a carnivore catching a specific prey NPC,
  a death event, the prey removed and the predator fed) is the acute tier.
- **The visible hunt** — targeting and *pursuing* a specific prey agent (GOAP over a
  moving quarry), acute/survival-tier, versus v1's latent field-graze.
- **Lotka–Volterra** — predator/prey populations rising and falling against each
  other over time.
- **Prey-affect coupling** (captured, immunology channel) — a fleeing or wounded prey
  emits a *stronger* prey signal (the field reads distress, not just density).
- **Optimal-foraging return-discounting** — the draw scaled by density ÷ pursuit cost,
  not density alone; **peoples as prey** (predators drawn toward settlements).

## Flagged items (G3)

1. **[fidelity] Peoples excluded from the v1 prey base.** A carnivore is drawn to the
   wild, not toward settlements. Recommended: exclude for v1 (keeps the predator in the
   wild, symmetric with The Wilding; avoids "predators stalk towns" until the acute
   hunt tier, where a kill's stakes justify it). Reversible: including peoples is
   adding their density to the sum.
2. **[byte-identity] The latent-scale mechanism** — as with The Quarry, the exact
   `PREY_LATENT_SCALE` is set during execution against the seed-42 byte-identity probe,
   not guessed here; the spec commits to *the property* (current peoples byte-identical,
   wild carnivore wakes), not a magic number.
3. **[determinism] No new draw / no epoch** — `prey_pressure` is derived, not drawn;
   genesis stays byte-identical. (Leads the determinism-contract review, though it is a
   no-op there by construction — flagged for completeness.)
