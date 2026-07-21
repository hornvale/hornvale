# The Provender — hunger, the fourth drive

**Status:** spec (G3 review)
**Date:** 2026-07-20
**Campaign:** The Provender (Temperament followup #3, second drive: hunger)
**Precedes:** implementation plan → execution → merge

## Problem

Creatures thirst, shiver, tire, and sleep — but never hunger. They do not eat.
And the substrate for food is already there, unused: a cell's **net primary
productivity** (the Miami proxy `temp_response(temperature) · moisture`, in
demography's carrying-capacity), its **biome**, and every species' **diet** — the
authored `ResourceVector` niche over PHOTOSYNTHATE / PLANT_FORAGE / ANIMAL_PREY /
DETRITUS / MINERAL.

## The core result (from ideonomy)

Hunger is not a new kind of drive — it is **the thirst homeostat with the niche
as its dial.** A resource stock the creature depletes by living, refilled by
contact with a source; thirst and hunger differ only in (the resource axis, the
source, the rate). Two consequences shape the whole design:

- **Read the niche, never a hardcoded "diet type."** The `ResourceVector` is a
  continuous *mix* over axes (an omnivore is 0.5 forage / 0.5 prey; a mixotroph
  part-photosynthesizes) — it already IS the position on the autotroph↔heterotroph
  spectrum. Branching on "herbivore vs carnivore" would re-encode what the niche
  already says.
- **The gate is the niche × the source, not metabolism.** A creature hungers for
  an axis iff its niche weights it AND the world provides it reachably. Metabolism
  sets the *rate* (reuse The Kindling), the niche sets *presence*, the source sets
  *serviceability*. "Ametabolic → no hunger" is the v1 consequence (constructs and
  undead carry no food niche a source serves), not a special case.

## Design

### 1. The food-value field

A cell's food-value FOR A CREATURE is its **niche · the cell's resource
availability**:

```
food_value(cell, niche) = Σ_axis  niche[axis] · availability(cell, axis)
```

`availability` is derived (UNI-20) from what the climate already holds:
- **PHOTOSYNTHATE** → **light** (solar exposure): an autotroph is fed by the sun,
  so this reads `solar_altitude` (The Wakeful Sun) — 0 in the dark. Its feeding
  cycle nests inside the wake cycle: it eats by day, starves at night.
- **PLANT_FORAGE** → the cell's NPP (the Miami proxy) × a forage share.
- **ANIMAL_PREY** → NPP × a prey share (the coexistence forage/prey split).
- **DETRITUS / MINERAL** → reserved seams (a lithovore's mineral source, a
  scavenger's detritus) — no settled species weights them.

Exposed as a `Terrain` hook `food_value(room, niche, day) -> f64`, the sibling of
`temperature`/`solar_altitude`: a default synthetic value for planted/synthetic
test terrains (byte-identical), overridden by `LocaleTerrain` reading the real
climate.

### 2. The hunger drive (stock, Drive #4)

Urgency is a fold over committed **`eaten`** events — time since the last meal,
clamped `[0, 1]` — the structural twin of thirst over `drank`. Its **ceiling is
survival** (like thirst: starving is lethal, unlike comfort/fatigue). Affordance:
**climb the food-value gradient** toward a richer cell (like thermal comfort
climbs its gradient), and **`Eat`** in place when the local food-value clears a
threshold — resetting hunger. A new `Action::Eat`, a new session predicate
`eaten`.

### 3. Rate from metabolism (reuse The Kindling)

The hunger rise rate couples to the creature's metabolic class and its cell
temperature exactly as thirst does (a hot endotherm burns faster → hungers
faster) — the same `rise_at`/path-integral machinery, a second consumer.

### 4. The reserved seams (no hardcoded diet)

Because the drive reads the niche, three futures are correct-by-construction the
moment such a creature becomes an agent: **autotroph → light** (PHOTOSYNTHATE
reads the sun, wake-gated), **lithovore → mineral** (the xorn, once a mineral
source is modelled), **scavenger → detritus**. None is a settled people today, so
none is built — but none needs a special case.

## Architecture

`food_value` is a derived read over the climate (NPP / biome) + the niche — no
stream draw, no stored state. Hunger folds `eaten` (a session predicate like
`drank`). `Action::Eat` joins the candidate set; the tick commits `eaten` when
food-value is sufficient. The metabolic-rate coupling reuses The Kindling's
`rise_at`.

## Determinism

- **No stream draw, no epoch** — reads the authored niche + existing climate
  fields; `eaten` is a session predicate (never genesis).
- **Committed-artifact drift** — NPCs now forage/graze, so the **possession
  galleries** change → regenerated + accepted at close.
- **Behavior change** across walk/health tests (a fourth competing drive); the
  health null-control re-verified (a well-fed world reads no chronic hunger; a
  creature that reaches food does not starve — Searching, not distress).
- **Day-0 walk byte-identical** (hunger is 0 at day 0).

## Model card

Reuses The Kindling (metabolic-rate coupling) and The Wakeful Sun (the
autotroph-light seam). The niche is the diet dial; food is niche-relative and
spatially graded (unlike thirst's binary water). **Active predation is deferred:**
v1 reads prey *availability* (a productivity field) and grazes toward it; hunting
specific creatures (the food web, Lotka-Volterra prey cycles) is the trophic
engine (PSY-10).

## Test plan

- **Unit:** `food_value` = niche·availability per biome/niche (omnivore vs a
  planted autotroph reading light); hunger folds `eaten`; the affordance climbs
  the food gradient / `Eat`s at a rich cell; metabolic rate coupling; the
  niche-gate (empty niche → no hunger).
- **Harness (`lab::synthetic`):** a creature in a barren cell hungers, seeks a
  productive cell, and eats (end-to-end); a well-fed creature never starves.
- **Calibration:** the health null-control holds (real worlds feed themselves).
- **Artifacts:** the possession galleries regenerated.

## Deferred (captured — PSY-10, the trophic engine)

The general **trophic engine**, a sibling of PSY-9's chronobiology engine:
one resource-homeostat, many instances (thirst / hunger / mana); the exotic
trophs (chemo/litho/radio/aether/emotivore/**theophage**); **active predation**
(hunting as GOAP + the food web + Lotka-Volterra); **subsistence modes**
(forage→farm→herd — hunger at society scale, into the culture stack); the
**faith-fed deity** (a god starving when forgotten — UNI-10's second death);
ontogenetic diet shifts (BIO-21). Plus: the `arbitrate` `Disposition` struct
tidy (five campaigns deep).
