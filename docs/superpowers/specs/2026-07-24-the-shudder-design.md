# The Shudder — the phobia made visceral

**Campaign:** The Shudder
**Date:** 2026-07-24
**Status:** draft (G3)
**Registry:** PSY-11 (the threat/fear engine) — the reserved *visceral felt
phobia*, the felt half of The Phantom

## The idea

The Phantom gave a creature a fear that outlives its cause: it remembers where
a herd's alarm frightened it and its plans bend around that ground long after
the herd is gone and the cell is safe. But the fear is **planner-only**. Stand
the creature on the phantom cell and it feels nothing — it routes around a place
that, to its own body, is unremarkable. The avoidance is a fact about its route
table, not about its inner life.

This campaign makes the dread **felt**: a Danger-drive term reading the ground
that *memory*, not the present, marks as dangerous, so a creature arriving on the
haunted clearing feels the old terror rise. And — because the drive that feels it
is the drive that flees — the creature steps off. Then, having stood there and
come to no harm, it loses the fear: The Phantom's staleness rule, which the
avoidance had been keeping the creature away from triggering, finally fires.

Fear that outlives its cause, felt in the body, and undone by the one experience
that can undo it.

## The load-bearing constraint: only the transient subset

`believed_hazard` mixes two provenances:

- **static** — cells whose *terrain* frightens the creature (The Haunt). This
  set is **non-empty for wild fauna on seed 42**: The Haunt shipped as *scoped
  drift*, not byte-identity, precisely because beasts roam hazardous ground and
  remember it.
- **transient** — cells whose terrain alone did *not* frighten, tipped over
  `DANGER_ACT` only by a herd's re-derived alarm (The Phantom). This set is
  **empty on seed 42**: it requires a primary-afraid emitter, and the settled
  peoples on their good ground never reach primary distress.

A felt term reading the *whole* memory would therefore drift the canonical world.
A felt term reading only the *transient* subset is byte-identical **by
construction**. Isolating that subset cleanly is the campaign's real work — and
it turns out the existing fold already separates them.

## Design

### 1. `HazardMemory` — the fold returns both sets

`believed_hazard_memo` already branches on provenance. Its **terrain shortcut**
asks `feels_frightening(terrain_threat, 0.0, boldness)` first: if terrain alone
frightens, the cell is shunned and the alarm re-derivation is skipped. Every cell
that reaches the line below it is, by definition, a cell terrain did *not*
frighten — so a cell shunned *after* that point is shunned *because of* a
remembered alarm. The transient subset is not a new computation; it is the
already-taken second branch, recorded instead of discarded.

```rust
/// The Haunt/Phantom memory, split by PROVENANCE.
pub struct HazardMemory {
    /// Every remembered-frightening cell — the planner's route-cost set
    /// (exactly today's `believed_hazard`).
    pub shunned: BTreeSet<RoomAddr>,
    /// The TRANSIENT subset with the remembered alarm magnitude at each cell:
    /// ground the creature's own terrain reading calls safe, that a herd's
    /// panic once made frightening. EMPTY whenever no emitter exists.
    pub dread: BTreeMap<RoomAddr, f64>,
}
```

`hazard_memory_memo(...) -> HazardMemory` becomes the one fold;
`believed_hazard` and `believed_hazard_memo` stay as thin wrappers returning
`.shunned`, so both existing callers and the existing unit tests are untouched.

**Where `dread` is written — exactly one line.** In the emitter-bearing branch,
where the fold already computes `alarm` and asks
`feels_frightening(terrain_threat, alarm, boldness)`: when that is true, record
`(cell, alarm)`. The **emitter-free fast path returns before this point**, so an
emitter-free world produces an empty map without executing a single new
instruction — which *is* the byte-identity proof.

### 2. `Danger.dread` — the remembered alarm in the live drive's slot

The drive gains a field mirroring The Alarm's:

```rust
pub struct Danger<'a> {
    …
    /// The per-tick ALARM field (sensed: present, external).
    pub alarm: Option<&'a BTreeMap<RoomAddr, f64>>,
    /// The remembered DREAD map (believed: past, internal) — the transient
    /// subset of this creature's hazard memory. `None` ⇒ no phobia,
    /// byte-identical.
    pub dread: Option<&'a BTreeMap<RoomAddr, f64>>,
}
```

Both are read at the creature's **own cell** and summed into the one additive
alarm slot before the boldness scaling — no new constant, because the dread *is*
an alarm term:

```
urgency = ( threat_field(position)                              // terrain, unchanged
          + ALARM_SCALE · ( borrowed(position) + dread(position) ) )
          × mettle_factor(boldness)                             // clamped [0,1]
```

Feeding the remembered alarm back through the same `feels_frightening` formula
that recorded it reproduces exactly the verdict that created the memory: **the
memory and the feeling agree**, the invariant `liveness.rs` already states of
`frightened_at` and the live drive.

**The asymmetry with the alarm, and why dread also needs an affordance.** The
alarm halo always lies within one hop of terrain that genuinely frightens its
emitter, so an alarmed creature always has a terrain gradient to flee down — which
is why The Alarm needed no `serviceability` term. A phantom cell is *now-safe
ground*, possibly far from any hazard: a creature dreading it has nowhere to
flee by terrain alone, so a dread term in `urgency` only would leave it with no
positive-utility action — `Hold`, labelled `Lost`, counted as **distress** by
`lab/health.rs`. So `serviceability` and `flee_step` read the same combined
field:

```
felt_threat_at(room) = threat_at(room) + ALARM_SCALE · dread(room)
```

`serviceability(MoveTo(n)) = felt_threat_at(position) − felt_threat_at(n)`, and
`flee_step` picks the least-felt-threat neighbour over the same field. With
`dread: None` both reduce to today's expressions exactly. The creature steps off
the haunted ground rather than freezing on it — dread as **wariness**, an errand
with an outlet, not a pathology. (Precedent: `loneliness_from_plan` makes the
social drive dormant rather than distressed when home is unreachable — "an
unreachable home is not a distress but a relocation.")

### 3. Where the dread is wired — and where it deliberately is not

- **`DriveMovements::step`** (the mover) — `dread: Some(&memory.dread)`, from the
  once-per-creature fold it already runs. The behaviour half.
- **`affect_of` / `affect_of_memo`** (the narration and health-metric read) —
  `dread: Some(&memory.dread)`, from the fold it already runs at
  `liveness.rs:2747`. The felt half: without this, `Affect` never carries the
  dread and the campaign would ship a second behavioural term rather than a
  feeling. (PSY-11 already records one instance of that wart — "v1's mover-only
  signal leaves a fleeing creature narrated *calm but moving*"; we do not add a
  second.)
- **The emission read is dread-free automatically.** `alarm_field_memo` builds
  emission through `emitter_arousal` → `affect_of(band = &[])`; an empty band
  gives `hazard_memory(roster = &[])`, whose emitter scan is empty, whose `dread`
  map is therefore empty. **One structural fact — the bandless replay — buys
  three guarantees at once:** the re-derivation terminates (The Phantom's
  recursion break), seed 42 stays byte-identical, and a dread-afraid creature can
  never raise an alarm. Superstition does not spread in v1; it stays reserved.
  `alarm_field_memo`'s cheap terrain gate therefore remains an *exact* necessary
  condition for the read it guards, and its doc comment is sharpened to say so —
  so a later edit cannot open contagion silently.

### 4. The design principle: the fear must stay falsifiable

Two candidate terms were rejected by one argument, worth naming because it will
govern the rest of PSY-11's body:

**Every term added to a creature's fear must preserve the existence of an
experience that disproves it.**

- *Self-reinforcing dread* (own dread feeding back into `believed_hazard`) would
  make a visit to the cell frightening *because* the creature is frightened
  there — the memory could never be cleared, and The Phantom's shipped
  `believed_hazard_clears_a_disproven_phantom` would go red.
- *Anticipatory dread* (felt at the cell **and its neighbours**, mirroring
  `threat_field`) is felt from *outside* the cell, so avoidance prevents the
  contact that would disprove it. Its loop never closes.

Contact dread closes the loop: felt on arrival, discharged by the flee step,
disproven by the `agent-at` fact that arrival commits. This is why the reserved
anticipatory dread and the reserved **time-decay** must ship *together* —
a forgetting half-life is the only other thing that can close that loop.

## Determinism

Genesis byte-identical: `Danger`, `hazard_memory`, and `affect_of` live only in
the vessel's session tick, never in worldgen. No seed draw, no new predicate, no
epoch, no serialized quantity — `dread` is a per-read `BTreeMap` folded from
already-committed `agent-at` facts plus a pure replay over the frozen ledger.
Iteration is `BTreeMap`/`BTreeSet` throughout (order-independent); float
comparison keeps the existing `total_cmp` tie-breaks. Stream consumption order is
untouched.

**Byte-identity is structural, not scale-tuned** (the additive-latent pattern,
fourth recurrence): the dread map is *empty* on seed 42 because the emitter-free
fast path returns before recording anything — there is no scale to get wrong.
This is inherited from The Phantom's finding that seed 42 has no primary-afraid
emitter; it is asserted, not assumed, by the success criteria below.

## Cost

The fold is unchanged — `dread` records a value the branch already computed, and
the emitter-free path returns before it. The drive reads two `BTreeMap` lookups
per cell instead of one. `affect_of` runs the fold it already ran.

The **hypothesis** is therefore no measurable change; it is not a claim, and it
is not settled by reasoning. **Time it on the health battery, not the possession
walk** — The Phantom's sharpest lesson is that a feature whose cost scales with
sim length must be measured on the longest sim in the suite, and its
byte-identical, possession-walk-clean version still detonated there.

## Success criteria

- **The dread map is the transient subset.** A planted history where a
  primary-afraid neighbour alarmed the creature at a cell whose terrain is safe
  yields that cell in `dread`; a cell frightening for its *terrain* appears in
  `shunned` and **not** in `dread` — a unit test on both provenances.
- **`dread` is empty with an empty roster** — the bandless replay, the recursion
  base case and the contagion block in one assertion.
- **The dread is felt.** A creature standing on a transient-hazard cell reads
  `Affect { object: Some(Danger), arousal ≥ DANGER_ACT }` through `affect_of`,
  on ground whose *present* terrain threat is below `act` — fear of nothing
  present. A never-alarmed control on the same cell reads `Content`.
- **The dread is discharged, not endured.** The same creature's arbitration
  yields `Intent::Do(MoveTo(n))` off the phantom cell (not `Hold`), and its
  affect label is **not** a distress label (`Lost` / `Frustrated` / `Helpless`).
- **The dread extinguishes on contact.** After the tick in which it stood there,
  the cell is absent from both `shunned` and `dread` — The Phantom's staleness
  rule, now reachable because the creature actually arrives.
- **A dread-afraid creature emits no alarm.** `alarm_field` over a population
  containing a dread-afraid creature on safe terrain is empty at its cell.
- **Byte-identity, verified not assumed.** `new --seed 42`, the seed-42
  possession galleries, and every committed artifact are byte-identical
  (`git diff --exit-code` over the regenerated artifact set).
- **The health null-control holds.** `lab/health.rs` prevalence, chronicity and
  the `danger` by-cause fraction are unchanged on the null-control seeds; any
  movement is investigated before merge, not absorbed.
- **No regression on the health battery's wall-clock** (the campaign's timing
  probe).

## Reserved (all still PSY-11's body)

The concentrated v1 — one cell, one mind, one moment — has exactly three
distribution axes, and each is a reserved follow-up:

- **Across space: anticipatory dread** — the longer-reach gradient PSY-11 already
  reserves (dread felt approaching the cell, a wide berth). **Ships with
  time-decay or not at all** — it opens a loop only a forgetting half-life can
  close.
- **Across minds: superstition → collective taboo** — a dread-afraid creature
  emitting alarm, so a phobia crosses a herd and becomes haunted ground.
  Structurally impossible in v1 (the bandless emission read).
- **Across time: time-decay of un-revisited fear**, and the fading discount on
  the dread magnitude it implies.

Plus, from the ideonomy passes:

- **Entity-keyed dread** — fear of a remembered *individual* rather than a place;
  the seam where PSY-11's memory meets SOC-9's enmity edges.
- **Kind-keyed dread** — one bad cliff teaching fear of *all* cliffs; the
  generalization that makes a phobia portable.
- **The sanctuary** — the proseasis: a cell remembered as where fear *lifted*,
  read as comfort. The memory form of The Alarm's reserved reassurance, and a
  member of PSY-11's reserved experiential-memory family.

## Flagged items (G3)

1. **[scope — the fork Nathan named] Dread makes the creature FLEE, not merely
   feel.** Recommended (ledger #1): the drive that feels fear is the drive that
   flees it, and giving dread a serviceability term is what keeps it out of the
   distress metric — a dread with no outlet reads as `Lost` and arms the health
   alarm. Confirm.
2. **[determinism] `affect_of` becomes dread-aware.** Byte-identity rests on the
   inherited claim that seed 42 has no primary-afraid emitter (The Phantom). The
   structural argument is airtight given that premise, and the premise is
   re-verified empirically by the artifact drift check in T1 — it is **not**
   assumed at spec time.
3. **[risk] The health battery is the real exposure surface.** Its wild fauna
   *do* contain primary-afraid emitters, so unlike seed 42 the dread map there
   may be non-empty and the metric may genuinely move. The criterion is
   *unchanged, or investigated before merge* — not "explained away."
4. **[principle] "The fear must stay falsifiable" (§4).** Proposed as a named
   design principle governing the rest of PSY-11. Should it be ratified as a
   decision record, or left in the spec?
5. **[scope] No superstition contagion in v1.** A dread-afraid creature never
   emits an alarm — structural, not a guard. Confirm this is the intended scope
   line.
