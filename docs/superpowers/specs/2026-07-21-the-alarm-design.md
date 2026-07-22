# The Alarm — fear that spreads through a herd

**Campaign:** The Alarm
**Date:** 2026-07-21
**Status:** draft (G3 — awaiting review)
**Registry:** PSY-11 (the threat/fear engine) — the reserved *fear-contagion* instance

## The idea

The Dread built danger as a flow drive; The Bane gave it a per-kind niche; The
Mettle gave it a boldness dial. All three make a creature fear a hazard *it senses
itself*. This campaign builds the first fear a creature borrows: **a creature reads
nearby distress as a threat cue and flees, even where it senses no hazard of its
own** — the alarm call that ripples through a herd, so the rank at the edge of a
predator's ground bolts and pulls the next rank with it.

PSY-11 reserves this verbatim: *"fear-contagion (reading others' distress affect as
a threat cue — a herd bolts together, the immunology channel; the affect model
already emits felt state)."*

## The unification — it is not a new drive

Fear-contagion is the **externally-driven substitution of the self-driven Danger
drive**. The Danger drive already reads a felt threat over its cell and neighbours
and flees the gradient (`Danger::urgency`, `flee_step`, liveness.rs); today that
threat is sourced only from the *terrain* hazard field (`Terrain::hazards` — the
uncanny, thermal, and injected predator axes). The Alarm adds a **second source for
the same felt threat**: the distress of nearby creatures. Same drive, same flee
behaviour, same boldness scaling — one new additive term in the threat a creature
feels. Not a new engine; a new *field* for a drive that already exists — the exact
shape The Teeth used for hunger and The Quarry for danger.

## Why a field, not agent-to-agent perception

This is the load-bearing architectural choice, and it is already precedented.
PSY-12 records that v1 **deliberately uses "a field not agents"** for
**determinism** — the Social drive is a home-proximity field, and true multi-agent
perception ("attachment — perceive and seek *specific* others") is explicitly
reserved because inter-agent reads within a tick are *order-dependent*. The Alarm
honours that line:

- The drive tick (`DriveMovements::step(frozen)`, liveness.rs:2424) walks each NPC
  to completion in sequence against the **frozen** pre-tick ledger; a creature sees
  neither another creature's live affect nor its within-tick moves (every query is
  `f.subject == npc.entity`-filtered).
- Affect is **immaterial and never committed** — there is no distress in the ledger
  to read.

So contagion cannot read a live neighbour. Instead it is a **per-tick alarm field**,
summed once at tick-start over the frozen population — order-independent by
construction, reload-stable, and structurally identical to `predator_pressure` /
`prey_pressure`. The alternatives (direct mid-tick affect reads; a committed
alarm predicate) are the two things the architecture already refuses: the first is
PSY-12's determinism trap, the second an epoch-triggering save-format change to
persist an ephemeral signal.

## Design

### 1. `alarm_field` — a per-tick distress field over the frozen population

A new field builder (the vessel's sibling of `worldgen::predator_pressure`), but
**dynamic**: rebuilt each tick from the current population rather than baked once at
worldgen. Given the frozen ledger, the `Vec<Npc>`, and the terrain, for each
creature that is **primary-afraid** it stamps a contribution around its cell:

```
alarm(cell) = clamp01( sum over primary-afraid creatures c of
                       stamp(c.position, cell) )
```

- **Primary-afraid** = a creature whose *own* Danger drive is active
  (`affect_of(frozen, c, ...)` yields `object == Some(Danger)` with negative
  valence / urgency ≥ `DANGER_ACT`), where that danger is sourced from the
  **terrain** hazard field only — never from borrowed alarm (see §3, the
  termination guarantee).
- **`stamp`** deposits the emitter's felt-threat magnitude on its own cell and its
  three neighbours (`RoomAddr::neighbors()`), zero elsewhere — the one-hop halo.
- The sum is **clamped to `[0, 1]`** (a stampeding crowd is not infinitely scarier
  than a threshold few — saturation, matching the pressure fields' normalization).

Fear is what is contagious: only the **Danger** drive emits. A creature distressed
by thirst or hunger does not raise the alarm (that is a distinct signal — RESERVED).

### 2. The Danger drive reads it — additive and latent

`Danger` gains an optional `alarm: Option<&CellMap<f64>>`. Its `urgency` folds the
alarm at the read cell into the felt threat, *before* the boldness scaling that
already governs how much fear is felt:

```
base   = max over cell+neighbours of threat_value(niche, terrain.hazards(room))
felt   = base + ALARM_SCALE · alarm(cell)          // NEW additive term
urgency = ( felt · mettle_factor ) .clamp(0,1)     // mettle_factor = 2(1-boldness)
```

The term is **additive** and **`ALARM_SCALE`-latent**: it only ever *raises* felt
threat, so a creature already below `DANGER_ACT` with no primary-afraid neighbours is
**byte-identical by construction** — the current worlds are settled bold omnivores
who rarely reach primary distress, so the alarm field is empty on seed 42 and every
committed artifact is unchanged. Borrowed fear is scaled by the reader's own
`mettle_factor`, so a bold creature shrugs off the herd's panic exactly as it shrugs
off a hazard — The Mettle's dial is reused, no new psychology field. v1
susceptibility is **uniform** (niche-independent): a per-species contagion weight
would need a gregariousness source that does not exist yet (uniform in v1, per The
Belonging), so it is RESERVED — the Bane discipline of shipping the scalar field
before the niche that has no derivation.

### 3. Termination — only primary fear emits (no runaway stampede)

The one failure mode the immunology/SIR framing exposes: if a creature alarmed *by
contagion* itself emitted alarm (secondary transmission), the herd would stampede
forever with no predator — `R0 ≥ 1`, non-terminating, physically false. v1 forbids
it, and the forbidding is **free and self-enforcing**: the alarm field is built by
reading `affect_of` over the frozen population **without** the alarm field, *then*
the tick's Danger drives read *with* it. Since affect is never committed, the frozen
ledger holds no prior alarm, so an emitter's danger is necessarily terrain-sourced —
secondary transmission is impossible by construction. Alarm is a bounded **halo**
around genuine hazards; clear the hazard and the wave collapses on the next tick.
(This is the same termination discipline as `MAX_STEPS` and strict-progress.)

## Determinism

Genesis byte-identical: the alarm field is *derived* from already-committed
`agent-at` / `DRANK` / … facts — no seed draw, no new predicate, **no epoch**. The
field sum is order-independent (addition over a `BTreeMap`-keyed population, clamped
at the emit boundary), so it is reload-stable and platform-stable under the existing
8-significant-digit quantization (the field is a compute-path intermediate, never
serialized; only the `agent-at` moves it influences are committed, and those already
quantize). The stream-consumption order is untouched. The only behavioural change is
the vessel-layer Danger drive gaining a latent alarm term, dormant for the current
agents (verified on seed 42), waking for a creature beside genuine distress.

## Success criteria

- `alarm_field` returns a deterministic `[0,1]` field that is **empty** when no
  creature is primary-afraid, and non-empty (a one-hop halo) around a creature whose
  Danger drive is active — a unit test on a planted scenario.
- `new --seed 42` and the seed-42 possession galleries are **byte-identical** (the
  alarm term is dormant — the settled peoples never reach primary danger distress).
- A **unit test**: `Danger::urgency` rises for a calm creature placed adjacent to a
  primary-afraid neighbour and is unchanged with no such neighbour; a bold creature's
  rise is scaled down by its `mettle_factor`.
- An **end-to-end test** (the herd bolts): two creatures — one on genuine hazard
  ground (primary-afraid, fleeing), one on safe ground one hop away with *no* hazard
  of its own — and over successive ticks the second flees too, pulled by the alarm,
  then settles once separated from the distress. The wave is bounded and terminates
  (no perpetual stampede with the hazard removed).
- The **health null-control** holds with the alarm term present: a herd fleeing a
  real predator is *Searching / fleeing*, not a false distress signal, and no
  perpetual-panic chronicity appears on natural worlds.

## Reserved (all still PSY-11's body — the engine, more users)

- **The damped wave (secondary transmission)** — borrowed alarm re-emits at a per-hop
  decay `< 1` (`R0 < 1`), so fear propagates *across* a large herd away from the
  hazard yet still terminates. The richer stampede; needs the decay/termination proof
  v1's primary-only rule gets for free.
- **Reassurance / calm-contagion (the proseasis, Gunkel's positive dual)** — a settled
  creature *damping* neighbours' fear (the alarm term goes negative). The reserved far
  shore, symmetric with The Bane's attraction and The Mettle's recklessness.
- **Per-species susceptibility** — an ALARM axis in `Hazards` + a `ThreatNiche` weight
  (a skittish herd animal catches panic readily; a solitary predator barely), once
  gregariousness has a real per-species derivation (couples to PSY-12).
- **Non-fear contagion** — thirst/hunger/thermal distress as its own (distinct) cue;
  and **fear-from-carcass** (a killed prey's death emits alarm — couples to The
  Teeth's reserved kill).

## Flagged items (G3)

1. **[byte-identity] The latent-scale mechanism.** As with The Quarry
   (`PREDATOR_LATENT_SCALE`) and The Teeth (`PREY_LATENT_SCALE`), the exact
   `ALARM_SCALE` is set during execution against the seed-42 byte-identity probe, not
   guessed here. The spec commits to *the property* — current agents byte-identical, a
   creature beside genuine distress wakes — not a magic number. (Note: because emission
   requires a *primary-afraid* neighbour and the seed-42 peoples never reach primary
   danger distress, the field is expected to be **empty** on seed 42 regardless of
   scale, making byte-identity structural rather than scale-tuned — even stronger than
   The Teeth's additive guarantee.)
2. **[determinism] No new draw / no epoch.** The alarm field is derived from committed
   facts, not drawn; genesis stays byte-identical. Leads the determinism-contract
   review though it is a no-op there by construction — flagged for completeness.
3. **[design — termination] Primary-only emission for v1.** The choice that guarantees
   termination (no self-sustaining panic) also *bounds* the v1 wave to one hop beyond
   genuine hazard ground — a real but modest effect. The full cross-herd wave is
   RESERVED behind a damping proof. Recommended: ship primary-only (terminating by
   construction, physically honest); reversible (the reserved wave adds a decayed
   re-emission term). Confirm this is the right v1 scope.
4. **[performance] The per-tick field rebuild.** Unlike predator/prey (baked once),
   `alarm_field` rebuilds each tick and calls `affect_of` per creature at tick-start —
   O(N) affect reads per tick. The hot-path discipline (memory
   [[phenomena-seam-is-a-hot-path]]) says re-time `new --seed 42` after wiring; if the
   O(N) tick-start pass regresses generation, the emitter set can be pre-filtered
   cheaply (only creatures on non-zero terrain-hazard cells can be primary-afraid).
