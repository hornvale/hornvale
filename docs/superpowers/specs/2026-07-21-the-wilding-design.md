# The Wilding — agentify the wild

**Status:** shipped (2026-07-21)
**Date:** 2026-07-21
**Campaign:** The Wilding (derive beast NPCs — waking The Quarry)
**Precedes:** implementation plan → execution → merge

## Problem

The drive layer grew rich — six drives, three dials, three niches, the first
biotic hazard — but only the **peoples** ever became agents. The wild is placed
(demography sets a per-cell density for every beast) yet inert: a mammoth herd, an
owlbear's forest, a dragon's lair all sit on the map as *numbers*, never as
creatures that thirst or forage or fear. And so The Quarry's predator-fear is
built but dormant — nothing alive feels it, because nothing alive is prey. This
campaign agentifies the wild: it derives NPCs for beast species, so herbivores
walk the world and finally *feel* the predator ground they stand near.

## The core result (from ideonomy)

A wild agent is a **representative of a concentration** — a herd's leader, a lone
apex — exactly as a settlement-NPC represents a settlement (The Quickening's
pattern). The demography stack already condenses every species' density into
per-cell concentrations tagged by their dominant species; the beast-dominant ones
are the herds and lairs, waiting to be woken.

And the *polarity* dimension locates the payoff. Wild agents split by trophic
polarity: **herbivores** are prey — they fear predators (the positive charge), so
the moment one becomes an agent **The Quarry wakes**; **carnivores** are predators
— fearless of predators (weight `0`, an apex *is* one), their *drawn-to-prey*
approach the reserved negative charge. v1's live payoff is the herbivore's fear.

## Design

### 1. The wild concentrations (worldgen)

A new `worldgen::wild_concentrations(world, k) -> Vec<(String, [f64; 3])>`: from
`demography_report`'s stack settlements (the coexistence stack condensed per cell),
it keeps the **beast-dominant** ones — those whose dominant species is *not* one of
the peoples (no `psyche_registry` entry) — takes the top `k` by mass, and returns
each beast's species label (the dominant `KindId`) and its cell position. Worldgen
encapsulates the demography (the vessel never reaches into it), reusing the same
fit shape The Quarry's `predator_pressure` established.

### 2. Wild NPCs (vessel)

A new `derive_wild_npcs(world, ctx, ledger, k) -> Vec<Npc>` mints one NPC per wild
concentration, with:

- **Biosphere traits** — species, temperature niche, diet niche, metabolic class
  (read from `biosphere_registry` by the beast's label, exactly as the peopled
  path reads them).
- **A derived threat niche** — including the now-live **PREDATOR** dread (The
  Quarry: a herbivore's `1 − carnivory` is high; an apex's is `0`).
- **The default psyche** — beasts carry no `psyche_registry` entry, so they take
  the existing `.unwrap_or` fallback (boldness `0.5`, deliberation `0.5`, horizon
  `0.5`) — no new authoring.
- **Home** — the concentration's cell room; **resource** — its nearest water.

The session and the health sim append the wild NPCs to the peopled ones, so beasts
live alongside peoples — walking, foraging, fearing.

### 3. What wakes, what waits

A **herbivore** beast (a giant elk, a woolly mammoth) is prey: it fears predator
ground and flees it — **The Quarry, at last, live**. A **carnivore** beast (an
owlbear) is an agent too, but fearless of predators, and its *drawn-to-prey*
approach — the negative charge — is reserved (the approach shore, with The Mettle
and The Bane). The visible predator that *hunts* (GOAP), the mass-based
**defendedness** (a mammoth fears predators less), and the Lotka-Volterra dynamics
are the reserved rest of the trophic engine (PSY-10).

## Architecture

`wild_concentrations` is a derived read over the demography stack (encapsulated in
worldgen); `derive_wild_npcs` mints beast agents through the existing NPC and drive
machinery. No stream draw, no epoch, no new predicate — a beast NPC is the same
`Npc` a settlement produces, only its home and traits come from a concentration
rather than a settlement.

## Determinism

- **Genesis byte-identical** — wild agents are derived from already-committed
  facts (no seed, no epoch, no predicate).
- **The possession galleries WILL drift** — new NPCs appear in `npcs` / `needs` /
  the tick's committed motion, exactly as The Quickening's first NPCs drifted them.
  Regenerated and accepted at close.
- **The health null-control is re-verified** — beasts now fear predators, but a
  herbivore near predator ground *flees* (Searching, not distress); chronic
  distress arises only if cornered, which a natural world avoids. Verified to hold
  (or, if a real transient appears, the prevalence threshold moved with chronicity
  held at `0`, the Wakeful-Sun precedent).
- **Performance** — the wild placement reuses the demography fit The Quarry already
  computes per session/sim-world; no new fit.

## Model card

The wild agent is a representative of a demography concentration, minted through
the peopled path's own machinery; the trophic polarity (prey vs predator) is now on
the board, with the herbivore's predator-fear the live payoff. **Deferred (the rest
of PSY-10):** the visible agent-predator that hunts (GOAP + the approach shore's
drawn-to-prey negative charge); the mass-based defendedness of vulnerability; the
Lotka-Volterra population dynamics; and richer wild behaviour (migration, territory).

## Test plan

- **Integration (worldgen):** `wild_concentrations` is deterministic and returns
  beast species (not peoples) at real positions on seed 42.
- **Unit (vessel):** `derive_wild_npcs` mints NPCs whose species are beasts, with
  biosphere traits and default psyche; a herbivore beast's threat niche carries a
  high predator weight.
- **Harness (`lab::synthetic`):** a herbivore beast on predator ground flees it (The
  Quarry, waking end-to-end) — the payoff.
- **Calibration:** the health null-control holds with wild agents present.
- **Artifacts:** possession galleries regenerated (drift accepted).

## Deferred (captured)

To **PSY-10** (the trophic engine): the visible hunting predator (GOAP) and its
drawn-to-prey approach (the negative charge); mass-based defendedness; Lotka-
Volterra dynamics; wild migration/territory behaviour.
