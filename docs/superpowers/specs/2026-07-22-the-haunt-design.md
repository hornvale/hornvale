# The Haunt — the ground a creature learns to shun

**Campaign:** The Haunt
**Date:** 2026-07-22
**Status:** draft (G3 — awaiting review)
**Registry:** PSY-11 (the threat/fear engine) — the reserved *remembered danger* instance

## The idea

The Dread, The Bane, The Mettle, and The Alarm all shape a creature's fear of the
danger it faces *now*. This campaign gives fear a **past**: a creature remembers the
ground where it was frightened and routes its later journeys *around* it. Where
`believed_water` is a creature's memory of where it found relief — the water it
plans toward though it cannot currently see it — `believed_hazard` is its memory of
where it found dread: the ground it plans *around*, the haunted place it shuns.

PSY-11 reserves this verbatim: *"remembered danger (a `believed_hazard`, the inverted
twin of believed-water — a creature avoids where it was hurt, turning the flow drive
flow+memory)."*

## The unification — the inverted twin of believed-water

`believed_water` (liveness.rs:817) is a pure **fold** over committed facts: among the
water cells a creature has stood in, the nearest to home, re-derived every tick from
its `agent-at` history — belief is fold-over-perceived, no stored state. The Haunt
builds its sign-inverted, cardinality-inverted twin:

|                | `believed_water` (seek)        | `believed_hazard` (shun)          |
|----------------|--------------------------------|-----------------------------------|
| truth folded   | `is_water` (permanent)         | danger-to-this-creature (transient) |
| cardinality    | the nearest **one** (a target) | the **set** of them (a field)     |
| effect         | plan **toward** it             | plan **around** it                |
| belief quality | cannot be wrong (water stays)  | **can be stale** (danger passes)  |

Two asymmetries make the dual more than a mirror. **Seeking is concentrated,
avoiding is distributed:** you drink at the nearest one water, but you shun *every*
bad place — so `believed_hazard` is a set, not a target. And **this is the drive
layer's first fallible belief:** water is permanent, so `believed_water` is never
wrong; danger is a transient flow field, so a memory of it can be stale — the
individual-scale seed of phobia (see §Reserved).

## Design

### 1. `believed_hazard` — the fold over frightening ground

A new fold, the inverted twin of `believed_water`:

```
believed_hazard(frozen, npc, t, terrain) -> BTreeSet<RoomAddr>
  = { cell ∈ npc's agent-at history (day ≤ t)
      : the creature's Danger drive is active there (frightened) }
```

"Frightened at a cell" is re-derivable from committed facts, exactly as
`believed_water` re-derives `is_water`: the creature's own felt threat there — its
`threat_niche · terrain.hazards(cell+neighbours)`, scaled by `boldness` — crossing
`DANGER_ACT`. No new predicate, no stored state, no epoch.

**Staleness — a fold that still forgets.** The precise rule is *a cell is
remembered-dangerous iff the creature's **most recent** visit there was frightened*:
a later safe visit clears the memory (experience corrects a fear it disproves). For
v1's **terrain-sourced** danger (uncanny / thermal / the static predator field), the
hazard at a cell is time-invariant, so this reduces to *visited ∧ still-dangerous* —
the recency bookkeeping is a no-op now and becomes load-bearing only when transient
danger is remembered (§Reserved). v1 therefore reads current danger at each visited
cell; it need not track visit order.

### 2. The planner shuns it — a finite route cost

The load-bearing choice, and the true dual of seeking. Today both planners —
`GoapSpace` (thirst → water) and `NavSpace` (hunger / homing → a room) — assign a
uniform `MoveTo` edge cost of `1` and know nothing of danger (liveness.rs:3155,
3217). The Haunt threads the `believed_hazard` set into both, and a `MoveTo` **into**
a remembered-dangerous cell costs `1 + REMEMBERED_PENALTY` instead of `1`. The A*
then routes *around* remembered-bad ground whenever a detour is cheaper than the
penalty — so a creature that was frightened at a place plans its later paths to water
and home to avoid it, **proactively**, rather than blundering toward it and flinching
one hop at a time (the reactive oscillation the memory-less planner produces when
thirst's shortest path runs through danger the Danger drive then vetoes step by step).

**Why the planner, not the Danger drive.** Remembered danger's whole value is *reach
beyond present sensing* — the planner is where reach lives (`believed_water` drives
planning, not sensing). Adding memory to the Danger flow drive's one-hop horizon
instead would be a near-no-op: a statically-dangerous cell is already sensed at one
hop, so remembering it at the same horizon changes nothing. The Danger flow drive is
therefore **unchanged** (present danger → flee, exactly as now); memory lives at the
planning layer. "Flow + memory" = the flow drive senses the present while the
creature's plans carry the past.

### 3. Latent + finite — byte-identity and survival-override, both for free

- **Additive-latent byte-identity.** The penalty only ever *raises* an edge cost, and
  for a creature whose `believed_hazard` set is **empty** every edge stays `1` and
  every plan is byte-identical. The settled peoples of a generated world are never
  frightened (their Danger drive never crosses `act` on their good ground — verified
  through every prior threat campaign), so their remembered set is empty and seed-42
  is byte-identical *by construction* — the empty-source form (as The Alarm's alarm
  field is empty on seed 42).
- **Survival-override for free.** The penalty is **finite**: a creature whose only
  route to its water runs through remembered-bad ground still takes it (the detour
  costs more than the penalty), so a dying-thirsty creature braves the haunted ravine.
  The flinch is a preference, not a wall — no soft-Maslow special-case needed; the
  finite cost *is* the override.

## Determinism

Genesis byte-identical: `believed_hazard` is a fold over already-committed `agent-at`
facts + the terrain's own danger — no seed draw, no new predicate, **no epoch**, like
`believed_water`. The A* over `u64` edge costs is deterministic with the existing
tie-breaks; the fold's set is `BTreeSet`-ordered. The only behavioural change is the
planner routing around a non-empty remembered set — dormant for the current agents
(empty set, verified on seed 42), waking for a creature that has been frightened
somewhere and must later plan past it. Stream consumption order is untouched.

## Success criteria

- `believed_hazard` returns a deterministic set that is **empty** for a creature never
  frightened, and contains exactly the visited-and-dangerous cells otherwise — a unit
  test on a planted history.
- `new --seed 42` and the seed-42 possession galleries are **byte-identical** (the
  remembered set is empty — the settled peoples are never frightened).
- A **planner unit test**: `plan_to_water` / `plan_to_room` with a non-empty avoid-set
  routes *around* a remembered-dangerous cell when a detour is available, and *through*
  it (braves it) when the detour exceeds the penalty; with an empty set the plan is
  byte-identical to today's.
- An **end-to-end test** (the shun): a creature frightened at a cell on an early trip
  plans a *later* journey to water that detours around that cell, where a
  never-frightened control takes the straight path through it — proactive avoidance,
  live, and it still reaches water (never trapped).
- The **health null-control** holds with remembered danger present (a creature
  detouring around remembered ground still reaches its water and rests — routing is a
  seek, not distress; no false chronicity on natural worlds).

## Reserved (all still PSY-11's body)

- **Transient-danger memory** — remembering where the *alarm field* or a *moving
  predator* frightened it (danger that has since passed), where the most-recent-visit
  staleness rule and the phobia become load-bearing: felt dread at a now-safe
  remembered cell, a fear experience must actively disprove. The alarm field is
  per-tick (unpersisted), so this needs a way to remember transient danger.
- **Time-decay of un-disproven fear** — a scare that fades even without a corrective
  safe visit (a forgetting half-life), atop the revisable most-recent-visit rule.
- **Longer-reach gradient** — remembered danger as a smooth repulsive *field* (a decay
  with distance from remembered cells), so a creature gives haunted ground a wide
  berth, not just avoids the exact cells.
- **Collective / cultural remembered danger** — a herd or a people sharing shunned
  ground: **taboo and haunted places**, the individual flinch scaled to folklore
  (couples religion's sacred/forbidden ground and The Alarm's social channel).
- **The experiential-memory family** — the lattice hub `believed_water` and
  `believed_hazard` share: `believed-food` (where it ate), `believed-comfort` (where
  it was warm) — a general episodic map, one fold shape, many truths.
- **Narrating the flinch** — surfacing the shun in prose ("it gives the ravine where
  the owlbear lurked a wide berth").

## Flagged items (G3)

1. **[design — the corrected seam] The planner route-cost, not a Danger-drive term.**
   The ideonomy passes first proposed a one-hop additive term in the Danger drive;
   grounding against the planner showed that redundant with one-hop sensing for static
   terrain. v1 is the planner route-cost (proactive avoidance, the genuine dual of
   `believed_water`); the Danger-drive felt-dread term is reserved for transient-danger
   memory where it isn't redundant. Recommended as specified; confirm the scope.
2. **[byte-identity] The penalty magnitude.** `REMEMBERED_PENALTY` (a `u64` detour
   budget) is set during execution against the seed-42 byte-identity probe and the e2e
   detour/brave behaviour, not guessed here. Byte-identity holds for the empty set
   regardless of the value (structural, like The Alarm). The spec commits to the
   property (empty-set byte-identical; a frightened creature detours; braves it when
   the detour exceeds the penalty), not a number.
3. **[determinism] No new draw / no epoch.** `believed_hazard` is derived from
   committed facts, not drawn; genesis stays byte-identical. Leads the determinism
   review though it is a no-op there by construction.
