# The Slumber — the rest drive & the wake cycle (the first oscillator)

**Status:** SHIPPED (2026-07-20, chronicle: the-slumber) — the Tier-0
(fractional-day) wake cycle + Process-S fatigue landed; **Tier-1 solar masking
is the registered immediate followup** (the reserved `is_awake` body-swap — see
Deferred). "Tiers refine": the fractional-day window is the coarse truth (active
by day/night), solar refines it (latitude / season / terminator) without
contradiction. Reframed through ideonomy (the two-process model + a general
chronobiology engine); supersedes the v1 window framing.
**Date:** 2026-07-20
**Campaign:** The Slumber (Temperament followup #3, first drive: fatigue/rest)

## Problem

Creatures never sleep — they forage, drink, and seek comfort around the clock —
and `Npc.activity` (the species `ActivityCycle`: diurnal / nocturnal /
crepuscular) is dead weight, *"write-only… retained for the deferred
activity-gating followup."* The rhythm every living thing keeps is authored per
species and never read.

## The model: sleep is two processes, and the clock is one instance of an engine

Chronobiology's canonical account (Borbély's two-process model) factors cleanly,
and the factoring is what makes this generalize:

- **Process S — homeostatic sleep pressure.** A debt that rises while awake and
  discharges while asleep. This is the fatigue drive.
- **Process C — an entrainable phase oscillator.** And every oscillator is three
  numbers: **`(zeitgeber, period, endogeneity)`** — the time-cue it locks to,
  its cycle length, and how much it free-runs without the cue. A circadian wake
  clock is just `(sun, ~1 day, entrained)`.

Lift once more (the abstraction pass) and Process C *is* the agent predicting
*when* conditions are good — the same **active inference** (UNI-1) the cognition
layer already runs. The Slumber ships **one instance** of this engine — the
`(sun, circadian, masked)` wake clock — the way The Kindling shipped the first
instance of `MetabolicClass`-in-the-drive-layer and GOAP the first instance of
the A* engine. The general engine is the registered followup (§6).

## Design

### 1. Process C — the wake oscillator

**SHIPPED as Tier 0** — `is_awake(activity, day)` reads a fractional-day window
per `ActivityCycle` (diurnal awake through the light half, nocturnal the
complement, crepuscular the twilight edges). A sleeping creature jumps through
its off-phase to dawn (`next_awake_day`), and rest is taken **in place** (a
creature sleeps where it is — `Fatigue`'s `home` is a reserved rest-QUALITY
hook), so explorers bed down in the field and a stranded creature is never
fatigue-blocked. The health metric samples each creature at a representative
WAKING moment (`waking_offset`, mid-morning — before the diurnal thermal peak):
distress is a waking state.

**Tier 1 (solar masking) — the registered followup, the reserved body-swap.**
A creature's wake state read from the **real sun**, not a fixed window:
`is_awake(activity, cell, day)` combines the astronomy **daylight model**
(day-length as a function of latitude × season, driven by the drawn obliquity)
with the **diurnal phase** (where in the rotation `day` falls). The sun is *up*
at a cell when the diurnal phase lies inside that latitude-and-season's lit
fraction. **Diurnal** is awake then, **nocturnal** the complement,
**crepuscular** near the terminator crossings (dawn/dusk).

This is **masking** (endogeneity = 0): the creature reads the sun directly, no
persistent internal clock yet. It already buys the realism the window couldn't —
polar summers of endless activity, winter's long dark, ~12h at the equator, dusk
drifting with the season.

**Locked worlds have no day/night** (no rotation, the sun fixed in the sky —
`calendar` reports no daylight cycle). There the solar zeitgeber is *absent*, so
the wake-gate does not fire and the creature falls back to fatigue-only rest — a
natural degenerate case the general `(zeitgeber, …)` framing predicts and a
hardcoded-sun model would mishandle.

### 2. Process S — fatigue as sleep-debt

Fatigue is a stock drive whose urgency is **sleep debt**: it rises gently while
awake and is reset by sleeping (a `rested` event). Crucially, **the daily rest
comes from Process C, not from fatigue** — a creature sleeps because it is *its
off-phase*, not because it hit an exhaustion threshold. Fatigue is the
**backstop**: the debt that accrues only when sleep is *prevented* (kept awake
past the off-phase, or stranded from home at nightfall), eventually forcing rest
even out of phase. Its ceiling sits **below survival** (like thermal comfort),
so a creature dying of thirst does not sleep through it.

This corrects v1's error (a fatigue steep enough to drive a *daily* rest left
creatures perpetually weary, arousal never settling): a gentle debt reset each
night by the wake-gate stays low in normal operation, so calm returns — and the
felt-state test re-pins cleanly.

- **Affordance:** `Rest`, serviceable at **home** (else a step home).
- **Discharge:** a `rested` fact (a new session predicate, like `drank`) resets
  the debt.

### 3. The wake-gate — seek only while awake

Thirst and thermal are **active only while the creature is awake** (Process C
gates them). A sleeping creature does not seek water or comfort. One **survival
override:** thirst past a critical urgency stays active (a creature *wakes to
drink* if dying). Thermal has no override; fatigue is not gated (it carries the
creature *into* sleep).

### 4. The emergent rhythm

By day (or its niche-phase) a creature forages, drinks, and lets sleep-debt
accrue gently; at its off-phase the wake-gate closes, thirst/thermal fall silent,
and it holds at home, asleep, the debt discharging. A creature stranded far from
home at nightfall, or one dying of thirst, is the exception the backstop and the
override handle.

### 5. Across creature types (who keeps which beat)

Metabolism answers *who sleeps* for free (The Kindling's gate): an **Ametabolic**
creature has no homeostatic drives, so no fatigue and no wake cycle — the
deathless keep their round-the-clock stillness. Metabolizers keep the solar wake
clock on their `ActivityCycle` phase.

The deeper generalization (the engine, §6) is what covers the *exotic*: the
**zeitgeber is a per-kind field**, and swapping it spans every animacy —
**alive** (a biological solar clock), **mechanical** (a construct's programmed
free-running cycle, `endogeneity = 1`, no zeitgeber), **informational** (an
undead's death-hour walk, entrained to a *place* + the sun), **abstract** (a
culture's canonical hours / festivals). One machine, one per-kind datum — the
`MetabolicClass` pattern again, now for *rhythm*.

## Architecture

Process C's wake state is a pure read of `(activity, cell-coordinate, day)`
against the astronomy daylight model — no stored state (it's masking). Fatigue
(Process S) is a derived fold over `rested` events, re-derived identically by the
tick and `affect_of` (the shared-fold discipline). `arbitrate` gains an
`awake: bool` (computed by the caller) that applies the wake-gate and the
survival override; `Action::Rest` joins the candidate set. No new stream draw
(reads authored `ActivityCycle` + existing astronomy); one new session predicate
`rested`.

## Determinism

- **No stream draw, no epoch** — fatigue is a derived session view; `rested` a
  session predicate like `drank`; the wake read consumes only existing
  astronomy fields.
- **Committed-artifact drift — YES.** Creatures now sleep on the real solar
  cycle, so the **possession-over-time gallery** (~97-day walk) changes and is
  **regenerated + accepted at close** (the first Temperament followup to drift a
  committed artifact). The day-0 transcript is a single moment; verify it and the
  live pane at implementation.
- **Behavior change** across walk/health tests — NPC movement gains a rhythm;
  assertions re-pin, and the **health null-control is re-verified** (a sleeping
  creature reads Idle/Content, never blocked/distressed).
- **Locked worlds** take the no-solar-zeitgeber branch (fatigue-only) — asserted,
  not left to chance.

## Model card

Ships the `(sun, circadian, masked)` instance of a general oscillator engine.
`is_awake` reads the real daylight model (latitude × season × diurnal phase), not
a fixed window — the true terminator, deferred only in its *endogenous*
refinement (a free-running clock that drifts in caves and jet-lags on travel;
§6). Process S = homeostatic sleep debt; Process C gating = the "seek only while
awake" followup, realized against the sun. The wake/sleep tradeoff emerges from
the same soft-Maslow ceilings thermal uses — no priority table.

## Test plan

- **Unit:** `is_awake` follows the sun by latitude × season (equatorial ~half,
  poles seasonal, locked → no cycle); fatigue rises gently and resets on `Rest`;
  the wake-gate suppresses thirst/thermal while asleep; the survival override
  keeps a dying creature seeking; the fatigue affordance plans home; 3-drive
  arbitration.
- **Harness (`lab::synthetic`):** a creature keeps a day/night rhythm over a
  multi-day sim; a creature stranded from home at nightfall is driven home; a
  locked-world creature rests on fatigue alone.
- **Calibration:** the health null-control holds (sleep ≠ distress).
- **Artifacts:** the possession-over-time gallery regenerated and reviewed.

## Deferred (captured — the general engine)

The registered followup is the **general chronobiology engine** — an oscillator
stack `(zeitgeber, period, endogeneity)` coupled to homeostatic pressures — and
its payoffs, each an idea-registry row:

- **The endogenous oscillator (Process C, Tier 2)** — a free-running clock whose
  phase is a fold over zeitgeber-exposure history, giving **jet lag** (long
  travel), **free-running drift** (caves, dungeons, polar night), and chronotype
  variation. The Lorenz caveat: full-precision phase integration, never
  re-seeded from quantized floats.
- **Other zeitgebers & periods** — moons (lycanthropy, circalunar breeding),
  tides (circatidal foraging), season (hibernation / migration / estivation),
  magic flux (potency at conjunctions), a place/host (an undead's death-hour), or
  none (a construct's programmed cycle).
- **Other gated behaviours** — the periodic-grid of `zeitgeber × gated-behaviour`
  whose empty cells *predict* creatures (a moon-waxed mage, a tidal spawner, a
  clockwork war-idol).
- **Cross-domain reuse** — the same oscillator for cultural canonical-hours /
  festivals, ecological temporal-niche partitioning (diurnal predator vs
  nocturnal prey), and **eclipse-as-false-zeitgeber** (a midday night → omens,
  mis-timed behaviour).
- **Second-order realism** — saturating-exponential sleep pressure, sleep
  inertia, napping/polyphasic sleep, a thermal survival-override (freezing in
  one's sleep).
- The `arbitrate` `Disposition`/`MindState` struct tidy.
