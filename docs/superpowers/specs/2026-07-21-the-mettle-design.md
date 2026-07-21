# The Mettle — boldness, the risk-appetite dial

**Status:** spec (G3 review)
**Date:** 2026-07-21
**Campaign:** The Mettle (the boldness psychology dial — PSY-11, deepening The Dread)
**Precedes:** implementation plan → execution → merge

## Problem

The Dread gave every creature the same fear: threat is threat, and all flee it
alike. But a kobold and a bugbear do not fear a cursed grove the way a timid thing
does — temperament calibrates how much of a hazard a creature *feels*. Two
creatures on the same dangerous ground should not always read the same dread. This
campaign adds the third psychology dial, **boldness**, and lets a creature's mettle
scale the fear The Dread computes.

## The core result (from ideonomy)

Two findings shape the design.

- **Boldness is the banked `threat_response`, wired at a second scope.** The
  species `PsychVector` already carries `threat_response` — "flee 0 ↔ stand 1" —
  authored per people and read today only by the *culture* layer (society's
  flee-vs-stand posture shapes its authority structure). This is precisely the
  pattern `deliberation_latency` and `time_horizon` followed: banked PsychVector
  fields, wired into the drive layer as individual dials in later campaigns.
  Boldness is the natural third — the same datum read at *creature* scope instead
  of *society* scope. No new field, no new authored data.

- **The dial is a full axis, and a negation pass surfaced its missing pole.** A
  naive "boldness reduces fear" model is a one-sided `[0,1]` scale-down —
  steady → fearless. Negating it ("what *amplifies* fear?") reveals a **coward**
  pole a scale-down omits: a creature that feels *more* than the real threat.
  Centering the dial on a baseline gives the full **coward ↔ steady ↔ brave ↔
  fearless** axis at once.

## Design

### 1. The boldness scaling

The Danger drive's urgency is scaled by the creature's boldness:

```
effective_threat = base_threat × 2·(1 − boldness)          (clamped [0, 1])
```

centered on `boldness = 0.5`:

| boldness | factor | region | who |
|---|---|---|---|
| 0.0 | ×2 | **coward** — fears more than is there | (none authored) |
| 0.5 | ×1 | **steady** — feels the threat as it is | goblin, beasts (default) |
| 0.7 | ×0.6 | **bold** | hobgoblin |
| 0.8 | ×0.4 | **bold** | kobold, bugbear |
| 1.0 | ×0 | **fearless** | (none authored) |

Only the **urgency** is scaled — *how much the creature cares*. The signed
serviceability gradient (`threat(here) − threat(neighbour)`, The Dread's
potential-field veto) is the hazard's own physics and stays unscaled; because the
arbitration weights it by the reduced urgency, a bold creature's veto is weaker,
so it will cross ground a timid one flees — the risk appetite falls out of the
existing machinery, no new path logic.

`boldness` threads onto `Npc` from `psyche_registry` (default `0.5` for species
without a psyche entry — the beasts — so they stay steady), exactly as
`deliberation_latency` and `time_horizon` do; the Danger drive carries it beside
its terrain.

### 2. The reserved far shore

The dial's far shore — `factor < 0`, a **reckless** creature with *negative* fear,
*drawn* to the hazard (the moth to the flame, the berserker, the hunter) — is
**reserved**. It needs `boldness > 1`, which no species reaches, and it is the
seed of predation-approach (a predator's inverted fear of its prey), so it lands
naturally with the trophic engine (PSY-10). v1 keeps effective fear `≥ 0`: no
sign-flip, no behavioural inversion — a bold creature fears *less*, never
approaches. A considered alternative — a signed remap making today's bugbears
(0.8) *drawn* to the uncanny — was set aside as a large, unmotivated behavioural
change (no species wants it yet).

## Architecture

`boldness` is an authored per-species scalar (the banked `threat_response`), read
at derivation like the other two dials — no stream draw, no stored state, no
epoch, no new predicate. The Danger drive scales its urgency by it; nothing else
in the arbitration changes.

## Determinism

- **Byte-identical for the goblin baseline** (`0.5 → ×1`) and the beasts (default
  `0.5`).
- **Byte-identical possession galleries.** The seed-42 possessed creature is a
  *hobgoblin* (`0.7 → ×0.6`), but it never wanders near a strange site, so its
  Danger urgency is `0` regardless, and `0 × 0.6 = 0` — the walk is unchanged.
  The artifact drift check must show **zero** drift.
- The health null-control only gets **cleaner** (bolder creatures fear less, so a
  natural world reads *less* danger-distress, and it already read ~none).
- No stream draw, no epoch, no new predicate.

## Test plan

- **Unit:** the scaling `2·(1 − boldness)` at the coward / steady / bold /
  fearless points (a steady 0.5 leaves urgency unchanged; a bold 0.8 quarters a
  threat toward it; a coward 0.0 doubles it, still clamped `[0, 1]`); a bold
  creature's Danger urgency is strictly below a steady one's on the same hazard;
  the boldness threads from `psyche_registry` (a bugbear is bolder than a goblin).
- **Harness (`lab::synthetic`):** two creatures on the same dread-pit, one steady
  and one bold — the bold one reads *less* danger distress (the dial's behavioural
  effect end-to-end).
- **Calibration:** the health null-control holds (unchanged or cleaner).
- **Artifacts:** zero drift (the strongest byte-identity check).

## Deferred (captured)

To the **PSY-11** row (the threat/fear engine): the danger-**approach** extreme
(the reckless far shore — negative fear, drawn to the hazard; the seed of
predation-approach, joining PSY-10); **per-drive boldness** (a creature discounting
its thirst/hunger/social warnings, not only danger); **situational courage**
(boldness that rises with another drive's desperation — partly emergent from the
arbitration already); and **construct programmed-boldness** (a war-golem fearless
by design — the ametabolic own-drive).
