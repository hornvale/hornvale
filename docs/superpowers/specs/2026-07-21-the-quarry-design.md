# The Quarry — predator-as-hazard, the first biotic fear

**Status:** shipped (merged 2026-07-21) — see the
[chronicle](../../../book/src/chronicle/the-quarry.md) and
[retrospective](../../retrospectives/the-quarry.md). Two build refinements:
realized density (not carrying capacity) for the field, and a `PREDATOR_LATENT_
SCALE` keeping the current bold peoples dormant (byte-identical) — see the retro.
**Date:** 2026-07-21
**Campaign:** The Quarry (predator-as-hazard — PSY-10's first step, deepening The Dread/Bane)
**Precedes:** implementation plan → execution → merge

## Problem

The Bane gave creatures per-kind fear, but every hazard it feared was *abiotic* —
the uncanny, the heat, the cold, all sourced from the world's climate and its
strange places. Yet the oldest fear of all is of *other creatures*: to be eaten.
A creature should shun the ground where predators lurk. This campaign adds the
first **biotic** hazard — a carnivore-presence field — so a quarry fears predator
territory, and takes the first step toward the food web.

## The core result (from ideonomy)

A negation surfaced the campaign's structure — the **eater-eaten duality**. The
same carnivore field is two things at once: a quarry's *threat* (fear it — a
positive charge) and, read with the opposite sign, a predator's *food* (be drawn
to it — the negative charge The Mettle and The Bane both reserved). One field, two
signs. v1 ships the threat side; the food/approach side lands with the rest of the
trophic engine (PSY-10).

Two dimensions pinned the scope: the predator hazard is **latent** (ambient risk,
not a *visible* hunting predator — a field, not an agent) and **steady** (a static
placement field, not the oscillating Lotka-Volterra population dynamics). So v1 is
a latent, static, threat-side carnivore field; the visible agent-predator and the
dynamics are the reserved rest.

## Design

### 1. The carnivore-presence field (encapsulated in worldgen)

A new `worldgen::predator_pressure(world, wc) -> CellMap<f64>`: it runs
`demography_report` (The Niche's per-cell per-species carrying capacity, the same
fit settlement genesis uses), keeps the **carnivore** species (biosphere diet
niche ANIMAL_PREY-dominant), sums their carrying capacity per cell, and normalizes
to `[0, 1]`. Worldgen is the only layer where demography and the roster meet
(the composition-root constitution), so the vessel never touches demography — it
receives a finished field.

### 2. The PREDATOR hazard axis and its injection

`Hazards` gains a fourth axis, **predator**. The session (and the headless health
sim) compute the field once at start — `WorldComponents::assemble()` +
`predator_pressure(world, wc)` — and **inject** the `CellMap<f64>` into
`LocaleTerrain`, exactly as The Wakeful Sun injected the calendar (a domain/window
cannot reach *up* to demography; inject from where composition happens).
`LocaleTerrain::hazards` reads the predator axis by corner-blending the CellMap per
room, the read `productivity_at` already models. Planted/synthetic test terrains
have no field → predator hazard `0`.

### 3. The threat weight — derived from carnivory (the eater-eaten link)

A creature's **predator** threat weight derives from its own diet niche:
`predator_weight = 1 − carnivory`, where `carnivory = niche.weight(ANIMAL_PREY)`.
A herbivore fears predators fully (`1`); an omnivore half; an obligate apex not at
all (`0` — it *is* a predator). This is The Bane's derive-from-nature pattern,
now linking the **diet niche to the threat niche**: a creature's carnivory sets
both what it eats *and* how much it fears being eaten. (`ThreatNiche` gains a
`predator` field, derived at NPC derivation from the diet niche already on hand.)

The **defendedness** refinement — a mighty, massive creature (a mammoth) fears
predators less even as a herbivore, from its mass/potency — is a reserved seam.

### 4. The danger drive reads it, unchanged in shape

`threat_value` gains the fourth term (`niche.predator · hazards.predator`); the
Danger drive, its flee gradient, and The Mettle's boldness scaling are otherwise
untouched — a quarry now flees toward cells of lower carnivore density, weighs a
predator-ground detour against its thirst, and a bold quarry braves ground a timid
one shuns. No new predicate, no new `Action`.

## Architecture

The demography fit is encapsulated in `worldgen::predator_pressure`; the field is
computed once per session/sim-world and injected into `LocaleTerrain`; the Danger
drive reads the extra axis. No stream draw, no epoch, no new predicate, no
save-format contact.

## Determinism & performance

- **Genesis byte-identical** — `predator_pressure` is a derived read over
  already-committed facts (no seed, no epoch, no predicate).
- **The possession galleries MAY drift** (unlike the abiotic axes, the carnivore
  field is *non-sparse* — most habitable cells carry some carnivore carrying
  capacity — so the possessed hobgoblin's danger may become faintly nonzero near
  predator ground). If the drift stays sub-`act` (the settlement sits where
  *people*, not predators, dominate, so its carnivore field is low), the walk is
  byte-identical; otherwise the galleries regenerate and are accepted, as a new
  field legitimately warrants. Verified at close by regeneration.
- **Performance:** the demography fit costs one extra re-derivation per session and
  per health-sim world. Bounded: the ~2000-world census does *not* run the drive
  sim, so only interactive sessions and the ~5 null-control seeds pay it (the
  harness uses synthetic terrain — no field). Accepted at G1.

## Model card

The carnivore field is the first biotic hazard; the threat weight derives from
carnivory (the diet↔threat link, the eater-eaten duality's threat side).
**Deferred (the rest of the trophic engine, PSY-10):** the **food/approach side**
(a predator *drawn* to prey — the negative charge, with The Mettle/Bane's reserved
approach); the **visible agent-predator** that hunts (GOAP); the **Lotka-Volterra**
population dynamics (the field oscillating over time); and the **defendedness**
refinement (mass/potency lowering a creature's vulnerability).

## Test plan

- **Unit:** `predator_weight = 1 − carnivory` (a herbivore fears, an apex does
  not); the Danger drive's `threat_value` gains the predator term; a quarry on a
  cell of high carnivore density (planted) dreads it, an apex on the same does not;
  a bold quarry's fear is scaled (The Mettle still composes).
- **Integration (worldgen):** `predator_pressure` is deterministic
  (two calls → byte-identical) and non-trivial (some cells carry carnivore mass,
  some none) on seed 42.
- **Harness (`lab::synthetic`):** two creatures on a planted predator-ground cell,
  one herbivorous (dreads it) and one carnivorous (does not) — per-kind biotic
  fear end to end (the twin of the thermal-fear unit test, through the sim).
- **Calibration:** the health null-control holds (settlements sit where people
  dominate; the predator field is low there).
- **Artifacts:** possession galleries regenerated (drift accepted if present).

## Deferred (captured)

To the **PSY-10** row: the food/approach side (predator drawn to prey); the
visible agent-predator (hunting as GOAP); Lotka-Volterra population dynamics; the
defendedness (mass/potency) refinement of vulnerability.
