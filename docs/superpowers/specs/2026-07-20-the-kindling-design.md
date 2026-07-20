# The Kindling — metabolism reaches the drive layer

**Status:** SHIPPED (2026-07-20, chronicle: the-kindling)
**Date:** 2026-07-20
**Campaign:** The Kindling (Temperament followup #1, reframed)
**Precedes:** implementation plan → execution → merge

## Problem

The drive layer gives every derived NPC the *same* homeostatic drives —
`Thirst { SUSTENANCE } + Thermal { niche }`, uniformly — and thirst rises at a
flat `rise`/day regardless of the world around it. Two consequences:

1. **No per-creature drive control.** An Ametabolic creature (construct / undead
   / elemental) would thirst like anything else, which is nonsense — it has no
   metabolism to run dry.
2. **Heat does not quicken thirst.** A creature crossing a desert dehydrates at
   exactly the rate of one resting in a temperate glade.

Both are already anticipated by data the drive layer simply never reads. The
**species domain** authors a `MetabolicClass` per species —
`Endotherm | Ectotherm | Autotroph | Ametabolic` — and the `Ectotherm` doc
already carries a *deferred* note: *"realized rate couples to ambient
temperature (spec §10 CAP-1)."* The metabolic model exists; it just hasn't
reached the drive layer.

## Design

### 1. Thread `metabolic_class` onto the NPC

Mirror the existing `temperature_niche` / `deliberation_latency` / `time_horizon`
threading: `derive_npcs` reads `biosphere_registry().get_by_label(species)
.metabolic_class` onto a new `Npc.metabolic_class` field. Reads authored data —
**no stream draw, no epoch.**

### 2. Gate homeostatic drives on metabolism

- **Ametabolic** → **no homeostatic drives** (no thirst, no thermal comfort). An
  undead does not thermoregulate or run dry; it simply persists. With no active
  drive it reads `Content`/`Idle` — an eerie *stillness* that characterizes the
  deathless well (captured as its own emergent note). Its *own* motivations
  (construct integrity, an undead's hunger-for-life, an elemental fed BY its
  element) are a later layer on this seam — deferred.
- **Endotherm / Ectotherm / Autotroph** → thirst + thermal, as today.

**Blast radius: none today.** The four settled peoples are goblin / hobgoblin /
bugbear (Endotherm) and kobold (Ectotherm) — all metabolizing. The Ametabolic
species are the "mighty" (dragons / treant / xorn), never derived as NPCs. So
the gate removes nothing from any current NPC (latent-but-correct); it fires
only when a non-people creature becomes an agent (a future NPC layer, or
`possess` of a mighty kind).

### 3. Couple the thirst rate to temperature — per class (realizes CAP-1's principle)

Thirst becomes a **path-integral of the dehydration rate over the creature's
actual position history**, not a flat `rise × elapsed`:

```
drive(t) = Σ over committed position-segments since last drink:
             rise_at(temp(cell, day), class) × segment_duration      (clamped [0,1])
```

`rise_at` depends on the metabolic class:

- **Endotherm** — temperature-*stable* basal rate, but heat drives evaporative
  loss (sweating): rate accelerates **above** a thermoneutral temperature, holds
  at base below it. *Asymmetric — heat-only.*
- **Ectotherm** — realized rate **tracks ambient temperature** (CAP-1's
  principle): fast in heat, torpid-slow in cold, floored so a frozen ectotherm
  still needs *some* water. *Symmetric.* Emergent ecological signature: ectotherms
  are desert-disadvantaged but cold-advantaged.
- **Autotroph** — base rate, **no coupling** (a deferred seam — plant-folk
  transpiration + a light-hunger drive are their own later work).

### 4. Architecture — one shared fold, byte-identical by construction

Both the drive-tick (the mover) and `affect_of` (the stateless read) compute
thirst through **one shared `drive_at` fold** that reads the committed
`agent-at` history + terrain. No incremental-vs-rederived drift to align (the
discipline `believed_water`/`nearer_to_home` already follow). The `Hold`-jump
optimization survives: a creature held at one cell accrues linearly at that
cell's `rise_at`, so the closed-form next-act jump still applies.

## Determinism

- **No new stream draw** (reads authored `metabolic_class` + existing terrain).
- **No epoch, no save-format change** — thirst is a *derived session view*
  (UNI-20), never genesis; sessions are ephemeral, re-derived from the seed.
- **No committed-artifact drift** — the census, almanacs, and maps never run the
  drive sim. Type-audit counts move (new `Npc` field, `drive_at` signature).
- **Day-0 walk byte-identical** — thirst is 0 at day 0, so coupling is inert
  there.
- **Behavior change (expected):** multi-day session/walk timing shifts for
  metabolizers in non-thermoneutral cells — most visibly **kobold** (ectotherm)
  NPCs, whose rate now tracks ambient heat. Behavior-test assertions
  (`possession_moves`, `walker_battery`) update to the new timing; the health
  **null-control is re-verified** (faster desert thirst must not push a real NPC
  into false distress — likely fine, since real settlements sit near reachable
  water, but checked empirically).

## Model card

Realizes the **temperature-coupling principle of CAP-1** for the *thirst
(water) drive* in the drive layer. The **allometric basal-energy-rate** coupling
in `species/allometry.rs` (CAP-1's other, energetic half) remains deferred —
this campaign does not touch `allometry.rs`. Naming precision: we retire CAP-1's
*behavioral water-need* deferral, not its *energetic* one.

## Test plan

- **Unit:** `rise_at(temp, class)` — endotherm asymmetric (base below
  thermoneutral, accelerating above), ectotherm symmetric+floored, autotroph
  flat; the path-integral fold over a planted position history; **tick == read**
  consistency (the shared fold gives identical thirst to the mover and the
  snapshot); the Ametabolic gate yields an empty drive set.
- **Harness (`lab::synthetic`):** a creature in a hot cell thirsts measurably
  faster than one in a temperate cell (end-to-end, same seed); an Ametabolic
  creature has no drives → never distressed (a new scenario / gate check).
- **Calibration:** the health **null-control holds** across the seed sweep under
  coupling (no new false distress).

## Deferred (captured)

- **Autotroph coupling** — transpiration water-loss + a light-hunger ("bask")
  drive.
- **Ametabolic's own drives** — construct integrity/repair, undead
  hunger-for-life, and the *inverted* elemental (a fire-elemental *fed* by heat,
  a water-elemental by water) — the per-kind drive-authoring layer this gate
  opens.
- **Allometric CAP-1** — the energetic basal-rate coupling in `allometry.rs`.
- Bundling `arbitrate`'s scalar inputs into a `Disposition`/`MindState` struct
  (the too-many-arguments tidy, deferred since #2).
