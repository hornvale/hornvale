# The Dread — danger, the fifth drive

**Status:** spec (G3 review)
**Date:** 2026-07-21
**Campaign:** The Dread (Temperament followup #3, third drive: danger/fear)
**Precedes:** implementation plan → execution → merge

## Problem

Creatures thirst, shiver, tire, sleep, and hunger — but nothing frightens them.
They will walk into a cursed grove or a lethal waste as blithely as a meadow. A
world observed through text needs its creatures to *flinch*: to shun the uncanny,
to route around a hazard on the way to water, to recoil where a place is wrong.
And the substrate is already there, unused: every room carries a **strangeness
magnitude** (the exotic/uncanny overlay, `describe().regime.strangeness`) and a
**habitability** verdict (lethal temperature extremes, the ocean). Nothing yet
lets a creature feel them.

## The core result (from ideonomy)

Danger is the **avoidance twin of hunger** — the same niche·field machinery, run
in reverse. Hunger climbs *toward* a resource; danger flees *from* a hazard:

```
  hunger:  food_value   = diet_niche   · resource_field   (climb toward, +)
  danger:  threat_value = threat_niche · hazard_field      (flee from,   −)
```

Three consequences shape the design (a cross-domain ideonomy pass — immunology,
traffic-potential-fields, finance, theater — and a timeline of the drive's own
cognitive evolution):

- **Danger is a FLOW drive, like thermal.** It senses the threat at the cell it
  occupies each tick; it has no internal stock and — crucially — **no discharge
  event** (fear is not "reset" like thirst by a drink; it simply lifts when the
  threat is gone). So, exactly like thermal, danger commits **no session
  predicate** and adds **no new `Action`** — fleeing is a plain `MoveTo` to the
  safest neighbour.
- **Danger MODULATES the other drives, it does not merely compete with them.**
  The artificial-potential-field reading: danger contributes **negative** utility
  to a move toward a more threatening cell, so a thirsty creature routes *around*
  a hazard to its water rather than straight through it. Danger is the first drive
  whose serviceability is **signed** (not clamped to ≥0).
- **v1's threat is the UNCANNY, not the predator.** There are no predator agents
  yet (active predation is deferred to PSY-10). The honest coarse rung — the
  timeline's bronze-age "taboo / cursed place" — is instinctive avoidance of
  *strange ground*: the `strangeness` magnitude plus lethal habitability. When
  predation ships, a predator agent simply becomes a moving hazard source in the
  same field; **danger is the perception-side of predation, built first.**

## Design

### 1. The hazard field

A cell's threat, exposed as a `Terrain` hook `threat_value(room) -> f64` in
`[0, 1]` — the sibling of `forage_value`:

- **Default `0.0` (safe)** for planted/synthetic test terrains, so worlds that
  plant no hazard are byte-identical and danger stays silent.
- **`LocaleTerrain` overrides** it from the real climate: the room's normalized
  `strangeness` magnitude (the uncanny), raised to lethal where the cell is
  uninhabitable (extreme heat/cold/ocean — `climate::is_habitable`). Exposed as a
  lean `LocaleContext::threat_at(room)`, the sibling of `productivity_at` /
  `temperature_at` (a full-precision compute-path read, never quantized).

v1 is **niche-less**: every metabolizing creature fears the uncanny uniformly.
The per-kind **threat niche** (`threat_value = threat_niche · hazard_field`, with
*negative* weights for attraction — a fire elemental fed by fire, the undead
fearing holy ground) is a reserved seam, exactly as hunger's exotic diets were
(§Reserved). The metabolic gate carries over: an **Ametabolic** creature is
fearless (a construct does not flinch).

### 2. The danger drive (flow, Drive #5)

Urgency is `threat_value(here)` — the felt threat at the current cell, in
`[0, 1]`. Affordance: **flee** — step to the neighbour of *lowest* threat, when
one is strictly safer than here (`flee_step`, the sign-flip of thermal's
`comfort_step`); `None` when boxed in by threat on every side (→ Frustrated). Its
**ceiling is survival** (`1.0`, like thirst/hunger, unlike comfort/fatigue), and
a present threat **overrides the wake-gate** — a threat wakes a sleeping creature,
as dying of thirst does (The Slumber's `survival_override`). No priority table: a
present lethal threat dominates naturally through urgency × serviceability.

### 3. Signed serviceability (the modulation)

`serviceability(MoveTo(n)) = threat(here) − threat(n)` — **unclamped**: positive
for a step to safety, **negative** for a step into worse danger. This is the one
real change to the arbitration contract (thermal clamps this to `.max(0.0)`;
danger does not). The consequence is the payoff: because utility is the weighted
sum over active drives, a move that serves thirst but *raises* threat nets
thirst's gain minus danger's penalty, so the arbitration reshapes the path around
the hazard. When every move is net-negative and nothing is served, the creature
Holds (cornered).

### 4. Affect

Danger reuses the existing circumplex with `object = DriveKind::Danger`: fleeing a
threat it can escape reads **Searching** (moving down the gradient) / **Eager**
(reaching safety); boxed in with nowhere safe reads **Frustrated** (the canonical
high-arousal fear); persistence upgrades to **Helpless** through the existing
scar. The narration seam colours the prose ("recoils from the uncanny ground").

## Architecture

`threat_value` is a derived read over the climate's strangeness + habitability —
no stream draw, no stored state, no epoch. Danger is a flow drive (`Danger<'a>`
holding terrain + day, like `Thermal`), a fifth `Drive` implementor,
`DriveKind::Danger`. It commits **no fact** and adds **no `Action`**. Wired into
`affect_of`, the tick's `arbitrate` drive set, the session felt-phrase prose, and
the health metric's by-cause tally.

## Determinism

- **No stream draw, no epoch, no new predicate** — the leanest drive yet. Reads
  the existing strangeness + habitability fields.
- **Genesis byte-identical** — verified the way The Provender was: regenerating
  all artifacts must drift **only** the possession galleries (creatures now shun
  strange ground), leaving almanacs, maps, scenes, and dumps untouched.
- **Signed serviceability** is the one arbitration-contract change; the
  single-drive `decide` path (thirst only) is unaffected (danger is not in its
  set), so it stays byte-identical.

## Model card

Danger is hunger reflected through approach→avoidance, reusing the niche·field
shape (v1 niche-less) and the gradient walk (inverted). The threat field is the
uncanny (strangeness + lethal habitability); the flee is a repulsive
potential-field term that reshapes the other drives' paths. **Deferred:** active
predation and the predator-as-moving-hazard (PSY-10); the per-kind threat niche
(incl. negative-weight *attraction* — the elemental, the undead, the predator's
inverted fear of its prey); the `boldness` psychology dial (risk appetite,
cower↔brave, with its danger-*approach* extreme); remembered danger (a
`believed_hazard`, the inverted twin of believed-water); and fear-contagion
(reading others' distress affect as a threat cue — a herd bolting together).

## Test plan

- **Unit:** `threat_value` = strangeness/habitability per cell (safe default);
  `flee_step` picks the safest neighbour / `None` when cornered; danger urgency
  reads the cell threat; **signed serviceability** penalizes a step into danger
  (a negative value, the arbitration nuance); the metabolic gate (ametabolic → no
  fear); a thirsty creature **routes around** a hazard cell to reach water it
  would otherwise cross (the modulation, the keystone).
- **Harness (`lab::synthetic`):** a creature on safe ground beside an uncanny cell
  shuns it (end-to-end); a creature cornered by threat on every side reads chronic
  Frustrated (by-cause danger).
- **Calibration:** the health null-control holds (a natural world's creatures are
  not chronically afraid — strangeness is sparse, so danger fires only near exotic
  sites).
- **Artifacts:** the possession galleries regenerated.

## Deferred (captured)

To **PSY-10** (predator-as-hazard, the food web) and a new/extended **PSY** row
for the danger drive's own reserved seams: the threat niche (per-kind fear,
negative-weight attraction), the `boldness` dial, remembered danger, and
fear-contagion. Plus the standing `arbitrate` `Disposition`-struct tidy (now six
drives/dials deep).
