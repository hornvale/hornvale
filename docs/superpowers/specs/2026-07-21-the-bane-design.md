# The Bane — the threat niche, per-kind fear

**Status:** shipped (merged 2026-07-21) — see the
[chronicle](../../../book/src/chronicle/the-bane.md) and
[retrospective](../../retrospectives/the-bane.md). Byte-identical (zero gallery
drift; the niche derived from existing data). The thermal-fear harness was
covered by a unit test rather than a scenario — see the retro.
**Date:** 2026-07-21
**Campaign:** The Bane (the threat niche — PSY-11, deepening The Dread + The Mettle)
**Precedes:** implementation plan → execution → merge

## Problem

The Dread gave every creature the same fear: a hazard was a hazard, feared alike
by all. But what a creature dreads is part of what it *is* — a cold-adapted
mammoth fears heat that a warm bugbear shrugs off; a fire-drake is unbothered by
flame that would kill a treant. This campaign gives fear a **niche**: each
creature weights the *kinds* of hazard by its own nature, completing the
diet ↔ threat ↔ sociality niche trilogy.

## The core result (from ideonomy)

An abstraction-lift unified three drives under one shape. Hunger's diet niche,
danger's threat niche, and The Mettle's boldness are all a **signed valuation over
environmental fields** — a charge:

```
   food_value   = diet_niche   · resource_field    (charge always +, approach)
   threat_value = threat_niche · hazard_field       (charge ±, flee if +, drawn if −)
   boldness     = a global scalar on the threat charge
```

Like charges repel (a mortal flees the uncanny); opposite charges attract (a fire
elemental drawn to fire). The threat niche is the same niche·field dot product
hunger already uses, over hazard axes instead of resource axes.

## Design

### 1. The hazard axes and the field

Three hazard axes for v1 — the three The Dread's field already sources:

- **UNCANNY** — the strangeness magnitude (an exotic/cursed place).
- **HEAT** — how far a cell's temperature is *above* the survivable band.
- **COLD** — how far *below*.

A cell's `Hazards { uncanny, heat, cold }` (each in `[0, 1]`) is read from the
climate — the per-axis split of The Dread's scalar `threat_at`. Exposed on
`Terrain` as `hazards(room) -> Hazards` (default all-zero for planted/synthetic
terrains; `LocaleTerrain` reads the real climate). Reserved axes (HOLY/UNHOLY,
POISON, DROWNING, and PSY-10's PREDATOR) are the extensible future — v1 ships a
concrete three-field struct; a general `HazardVector` over a registered
`HazardAxis` basis (the full `ResourceVector` parallel) is the reserved
generalization.

### 2. The threat niche — derived, not authored

Each creature carries a `ThreatNiche { uncanny, heat, cold }`, **derived** from
what it already is (no fresh per-species authoring):

- **HEAT / COLD** derive from the species' **temperature niche**: a creature
  weights the extreme *away from its comfort optimum*. A cold-adapted creature
  (low optimum) weights HEAT high and COLD low; a warm one the reverse. (Concrete
  form: the weight rises as the optimum sits on the opposite side of a neutral
  reference, clamped to `[0, 1]`.)
- **UNCANNY** derives from the **metabolic class**: a metabolizing mortal fears
  the eldritch (weight `1`); an **Ametabolic** creature (a construct, an
  elemental like the xorn) *is* eldritch and does not (weight `0`).

`ThreatNiche` is computed once at NPC derivation (like `boldness` and the diet
`niche`), from the temperature niche and metabolic class already on hand.

### 3. The danger drive reads the niche

`threat_value(niche, hazards) = niche.uncanny·hazards.uncanny +
niche.heat·hazards.heat + niche.cold·hazards.cold` — the niche·field dot. The
Danger drive holds the `ThreatNiche` (like Hunger holds its diet niche) and reads
`threat_value` where it read the scalar: anticipatory urgency is the greatest
`threat_value` over the cell and its neighbours, and The Mettle's boldness scales
the result:

```
effective_urgency = 2·(1 − boldness) × max_neighbourhood threat_value(niche, hazards)   (clamped [0, 1])
```

The flee gradient (`flee_step`) likewise minimizes the creature's own
`threat_value` — so two species on the same ground flee *different* cells, each
its own bane.

### 4. Signed weights reserved (the approach shore)

v1 weights are **≥ 0**: differential *fear*. A creature can weight a hazard `0`
(a red dragon is *fearless* of fire), but not negative — true **attraction**
(*drawn* to the hazard, the drive inverting to climb *toward* it) is **reserved**,
the shared far-shore with The Mettle's reckless pole (both are the "negative
charge"). It lands with predation-approach (PSY-10), when an exotic creature first
becomes an agent to exercise it. So `threat_value ≥ 0` in v1: no sign-flip, no
behavioural inversion.

## Architecture

`ThreatNiche` and `Hazards` are small value types; the niche is derived at
derivation from the temperature niche + metabolic class (no stream draw, no
authoring, no epoch). `Terrain::hazards` replaces the scalar `threat_value` (the
per-axis split of `LocaleContext::threat_at`); the Danger drive reads the dot
product, scaled by boldness. No new predicate, no new `Action`.

## Determinism

- **Genesis byte-identical** — the niche is derived from existing authored data;
  no draw, no epoch, no predicate.
- **Byte-identical possession galleries** — the seed-42 possessed *hobgoblin*
  meets no strange site and no lethal-temperature cell, so every hazard axis is
  `0` and `threat_value` is `0` regardless of its niche. Zero gallery drift.
- **Danger behaviour changes where hazards exist** — the field is reshaped from a
  `max(uncanny, lethal)` scalar to a per-axis weighted sum — so the health
  null-control is re-verified (it only gets richer/cleaner: differential fear,
  still sparse) and the danger harness terrains gain per-axis hazard planting.

## Model card

The threat niche is hunger's diet niche reflected through fear, over hazard axes;
the thermal weights derive from the temperature niche, the uncanny from the
metabolic class. **Deferred:** the general `HazardVector` over a registered
`HazardAxis` basis; the reserved axes (HOLY/UNHOLY, POISON, DROWNING, PSY-10's
PREDATOR); and the **negative-weight attraction / approach** behaviour — the
shared far-shore with The Mettle, landing with predation-approach (PSY-10).

## Test plan

- **Unit:** `threat_value` = niche·hazards per axis; the derived niche (a
  cold-adapted species weights HEAT > COLD; a warm one the reverse; an Ametabolic
  creature weights UNCANNY `0`); a dragon *fearless* of its element (weight `0`)
  feels no fear on a cell hot for a mortal; two species on the same hazard flee
  differently.
- **Harness (`lab::synthetic`):** two creatures on a cell that is HOT — the
  cold-adapted one dreads it, the heat-adapted one does not (per-kind fear end to
  end, the twin of the boldness harness).
- **Calibration:** the health null-control holds.
- **Artifacts:** zero gallery drift.

## Deferred (captured)

To the **PSY-11** row: the general `HazardVector`/`HazardAxis` basis; the reserved
hazard axes (HOLY/UNHOLY, POISON, DROWNING); the negative-weight **attraction /
approach** behaviour (with The Mettle's reckless pole and PSY-10's predator axis).
