# The Slumber — the rest drive & the wake cycle

**Status:** spec (G3 review)
**Date:** 2026-07-20
**Campaign:** The Slumber (Temperament followup #3, first drive: fatigue/rest)
**Precedes:** implementation plan → execution → merge

## Problem

Creatures never sleep. They forage, drink, and seek comfort around the clock,
because the drive layer has no notion of rest — and `Npc.activity`, the
species' `ActivityCycle` (diurnal / nocturnal / crepuscular), is dead weight:
its own doc calls it *"write-only this slice… retained for the deferred
activity-gating followup (a diurnal NPC seeking water only while awake)."* The
rhythm every living thing keeps — active by day or by night, resting the other
half — is authored per species and never read.

This campaign adds the third drive, **fatigue**, and in doing so activates the
activity cycle: a creature now wakes, tires, and sleeps on its own rhythm, and
seeks its other needs only while awake.

## Design

### 1. The wake state — `is_awake(activity, day)`

A creature's wake state at a moment is a pure function of its `ActivityCycle`
and the *time of day* — the fractional part of `WorldTime.day`. A **diurnal**
creature is awake through the light half of the day, a **nocturnal** one through
the dark half, a **crepuscular** one at the twilight edges. This is an authored
simplification (a fractional-day window, the same altitude as the drive layer's
per-day temperature) — true solar-altitude waking, with latitude and season, is
deferred.

### 2. The fatigue drive (stock, wake-gated accrual)

A third `Drive` beside thirst and thermal. Its urgency is the **accumulated
awake-time since the creature last rested**, normalized and clamped `[0, 1]` —
a stock drive like thirst, but its clock ticks *only while awake* (a sleeping
creature does not tire), which is exactly what binds it to the cycle. Its
**ceiling sits below survival** (like thermal comfort): a mildly thirsty tired
creature sleeps, but a creature dying of thirst does not sleep through it.

- **Affordance:** `Rest`, serviceable at the creature's **home** (its
  rest-place) — a tired creature plans home to sleep, the same shape as thirst
  planning to water.
- **Discharge:** resting commits a `rested` fact (a new session-only predicate,
  like `drank`/`agent-at`), resetting the fatigue clock — an instant reset, and
  the wake-gate (below) keeps the creature home for the rest of the off-phase.

### 3. Wake-gating — seek only while awake

Thirst and thermal are **active only while the creature is awake**. A sleeping
creature does not seek water or a kinder clime — the named followup, realized.
The one **survival override:** thirst past a critical urgency stays active
regardless (a creature *wakes to drink* if it is dying of thirst — soft Maslow,
consistent with the ceilings). Thermal has no override (comfort is not lethal in
this model). Fatigue itself is **not** wake-gated — it is the drive that carries
a creature *into* sleep.

### 4. The emergent rhythm

By day, a diurnal creature is awake: thirst and thermal drive it to forage,
drink, and seek comfort while fatigue climbs. Toward the end of its active phase
fatigue crosses its threshold and wins the arbitration; it heads home and rests,
resetting fatigue. Through the off-phase, thirst and thermal are gated off and
fatigue is spent, so no drive is active — it holds at home, asleep. At the next
wake it rises refreshed and forages again. A nocturnal species runs the same
loop shifted half a day. A creature caught far from home at duskfall is driven
home; a creature dying of thirst wakes to drink.

### 5. Across creature types (who sleeps)

The "who sleeps" question is answered by metabolism, for free — fatigue is a
homeostatic drive, so The Kindling's gate governs it exactly as it governs
thirst and thermal:

- **Ametabolic** (construct / undead / elemental — the xorn and its kin): no
  homeostatic drives, so **no fatigue → never sleeps.** The deathless keep their
  round-the-clock stillness; the wake cycle does not apply (nothing to gate). No
  new code — it falls out of the gate.
- **Metabolizing** (Endotherm / Ectotherm / Autotroph — the four settled peoples,
  dragons, treants, beasts): tire and sleep on their authored `ActivityCycle`
  phase (diurnal / nocturnal / crepuscular).

The four settled peoples are all diurnal or nocturnal, so the fixed-phase model
is correct for all shipping content. Two nuances the model deliberately does NOT
express yet (deferred — no authored species needs them): a **cathemeral /
no-fixed-schedule** creature (a shark-like beast, a being of appetite) that rests
opportunistically rather than on a phase; and an **alive-but-sleepless** creature
(a cursed knight who thirsts but never rests), which would require decoupling
"has fatigue" from "has metabolism." Both are captured as idea-registry rows.

## Architecture

Fatigue is a derived view (UNI-20): a fold over the wake-time integral +
committed `rested` facts, never stored, re-derived by both the tick and
`affect_of` from the same history (the shared-fold discipline The Kindling
established). `is_awake` is pure over `(activity, day)`. `arbitrate` gains an
`awake: bool` input (computed by the caller, like `helpless`) that applies the
wake-gate and the survival override; the `Rest` action joins `Drink`/`MoveTo`
in the candidate set. No new stream draw (reads authored `ActivityCycle`); one
new session predicate `rested`.

## Determinism

- **No stream draw, no epoch** — fatigue is a derived session view; `rested` is
  a session-only predicate (never genesis), like `drank`.
- **Committed-artifact drift — YES (the first of these followups).** Creatures
  now sleep, so the **possession-over-time gallery** (`possession-over-time-seed-42.md`,
  a ~97-day walk) changes and must be **regenerated and accepted at close**. The
  day-0 possession transcript is unaffected (fatigue is 0 at day 0 and the walk
  is a single moment). To verify at implementation: whether the short day-0 walk
  and the seed-42 *live* pane drift, and re-freeze/accept accordingly.
- **Behavior change** across the walk/health tests — NPC movement now has a
  rhythm; behavior-test assertions update, and the **health null-control is
  re-verified** (a sleeping creature that isn't seeking must not read as
  distressed — it is Idle/Content, not blocked).

## Model card

Activates `ActivityCycle`; realizes the "seek only while awake" followup.
`is_awake` is a fractional-day approximation, not true solar altitude (deferred).
Fatigue's ceiling-below-survival makes the wake/sleep tradeoff emerge from the
same soft-Maslow ranges thermal uses — no priority table.

## Test plan

- **Unit:** `is_awake` per cycle across the day; fatigue accrues only while
  awake and resets on `Rest`; the wake-gate suppresses thirst/thermal while
  asleep; the survival override keeps a dying creature seeking water; the
  fatigue affordance plans home; 3-drive arbitration.
- **Harness (`lab::synthetic`):** a creature keeps a day/night rhythm over a
  multi-day sim (active then resting); a creature stranded far from home at
  duskfall is driven toward home.
- **Calibration:** the health null-control holds (sleep is not distress).
- **Artifacts:** the possession-over-time gallery regenerated and reviewed.

## Deferred (captured)

- **Cathemeral / no-fixed-schedule creatures** — a 4th `ActivityCycle` (or an
  opportunistic-rest mode) for a living creature with no consolidated sleep
  phase (a shark-like beast, a being of appetite). Rely-on-the-gate holds for
  now (no authored species needs it).
- **Alive-but-sleepless** — decoupling "has fatigue" from "has metabolism" (a
  per-kind sleep flag), so a cursed sleepless knight who still thirsts is
  expressible independently of the ametabolic gate.
- **True-solar waking** — wake state from solar altitude (latitude, season, the
  real terminator), replacing the fractional-day window.
- **Fatigue as a modulator** — tiredness degrading perception/decision quality,
  not just driving rest.
- **A thermal survival override** — freezing to death in one's sleep.
- **Rest quality** — an unsafe or uncomfortable rest-place giving poorer
  recovery (waits on the danger drive / a safety notion).
- The `arbitrate` `Disposition`/`MindState` struct tidy (now three campaigns
  deep).
