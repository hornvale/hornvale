# The Phantom — fear of a danger that has passed

**Campaign:** The Phantom
**Date:** 2026-07-22
**Status:** draft (G3 — awaiting review)
**Registry:** PSY-11 (the threat/fear engine) — the reserved *transient-danger memory*, the capstone of The Haunt + The Alarm

## The idea

The Haunt gave fear a memory, but only of *permanent* danger: the ground it shuns is
ground still dangerous (a static terrain hazard), so avoiding it is merely prudent.
This campaign gives fear a memory of danger that has **passed** — a creature that was
frightened where a herd once bolted goes on shunning that ground long after the alarm
has died and the cell is safe again. It is the first fear of *nothing* — the phobia,
and the individual-mind seed of superstition.

PSY-11 reserved this in both parents: *"transient-danger memory (remembering where the
alarm / a moving predator frightened it — danger that has passed — where the staleness
rule and the felt-dread-at-a-now-safe-cell phobia become load-bearing; needs a way to
remember unpersisted transient danger)."*

## The unification — the memory recovers what was felt, not what remains

The Haunt's `believed_hazard` folds a creature's `agent-at` history and, at each visited
cell, asks `frightened_at(cell)` — its felt terrain threat there. Terrain danger is
time-invariant, so "was frightened" equals "is still dangerous," and the memory can read
the *present* terrain. The Alarm added a danger that is **not** time-invariant: the
`alarm_field`, a per-tick halo of a herd's contagious fear, which rises when a neighbour
panics and is gone the next tick. The Haunt could not remember it — the field is never
committed.

But it is **re-derivable**. `alarm_field(frozen, npcs, terrain, day)` (liveness.rs:2522)
is a pure function of committed positions and static terrain, so its value at any *past*
day is recomputable from the frozen ledger. The Phantom generalizes the memory's probe
across time:

```
frightened_at(cell, day) = ( threat_field(cell, terrain)                      // The Haunt (time-invariant)
                           + ALARM_SCALE · alarm_at(cell, day) )              // NEW: the alarm as it WAS on `day`
                           × mettle_factor(boldness)  ≥  DANGER_ACT
```

`believed_hazard` folds over the creature's visited **(cell, day)** pairs (the `day` was
always there — every `agent-at` fact carries it) and asks `frightened_at(cell, day)`.
This is the **generalizing abstraction**: every danger source — static terrain (Haunt),
transient alarm (Phantom), and the reserved live-predator and diurnal-terrain hazards —
is recovered by the same move, *evaluate the full felt threat at a past (cell, day)*.
No new predicate, no epoch: the transient danger is remembered by re-deriving it, exactly
as `believed_water` re-derives `is_water`.

## Design

### 1. `frightened_at(cell, day)` — the alarm as it was, re-derived

`frightened_at` gains a `day` and the population roster, and adds the re-derived alarm at
`(cell, day)` to the terrain threat before the boldness scaling. `alarm_at(cell, day)`
reads `alarm_field(frozen, roster, terrain, day)` at `cell` — the halo of whichever
creatures were **primary-afraid** on that past day, reconstructed from their committed
positions.

**The recursion trap and its fix (load-bearing).** `alarm_field(day)` calls `affect_of`,
which builds the `Perceived` view *including* `believed_hazard` (the planner memory). If
`believed_hazard` re-derives `alarm_field`, that re-enters `affect_of` → `believed_hazard`
→ `alarm_field` → ∞. The fix is the exact primary-only discipline The Alarm used one
level down, lifted to the memory layer: **the alarm-field replay uses terrain-only
memory** (an empty roster → no transient term), so the field build never re-enters the
transient path. Mechanically, `believed_hazard(…, roster)` with `roster = &[]` *is* the
terrain-only Haunt memory, and `alarm_field`'s internal `affect_of` already passes an
empty band — so the recursion breaks by construction. The transient memory is a *higher*
layer, read only by the live planner, never fed back into the field.

### 2. `believed_hazard` — most-recent-visit staleness, now load-bearing

`believed_hazard(ledger, npc, t, terrain, roster)` folds the `agent-at` history and, for
each cell, keeps the frightened-status of the creature's **most recent** visit: a cell is
shunned iff the *last* time it stood there it was frightened. A later safe visit **clears**
the fear — experience disproving it. This rule was specified but *inert* in The Haunt
(static terrain makes every visit's verdict identical); The Phantom makes it live, because
a cell alarm-frightened on day `t₁` and safely revisited on `t₂ > t₁` is no longer shunned.
The generalized set is read by The Haunt's **existing planner route-cost** — unchanged —
so a creature's plans bend around ground where a herd once panicked, even now-safe ground:
the phobia, in behaviour.

The v1 phobia is **planned-around, not felt**: the creature routes around the phantom cell
but the Danger flow drive is unchanged (it senses only the present). Making the dread
*visceral* — a Danger-drive memory term, so the creature feels fear standing on now-safe
ground — is reserved (§Reserved), because a felt term reading the whole `believed_hazard`
would drift seed 42 (The Haunt's static set is non-empty for fauna); doing it safely means
reading only the transient subset, a distinct follow-up.

### 3. Byte-identical, bounded, and cheap on the canonical world

- **Byte-identity (the empty-source form).** The transient term requires a **primary**-
  afraid emitter, and seed 42 has none — The Alarm shipped byte-identical for exactly this
  reason. So `alarm_at(cell, day)` is `0` everywhere on seed 42, `frightened_at(cell, day)`
  collapses to The Haunt's terrain-only `frightened_at(cell)`, most-recent-visit collapses
  to any-visit (terrain is time-invariant), and `believed_hazard` returns The Haunt's exact
  set. `new --seed 42` and the seed-42 possession galleries are **byte-identical**.
- **Bounded without decay.** The shunned set is bounded by the cells the creature actually
  *visited-and-was-alarmed-on* — a finite subset of its finite history; it cannot shun
  ground it never walked. With the finite planner penalty (never trapped) and the most-
  recent-visit clearing, v1 needs no forgetting clock (time-decay is reserved).
- **Cheap, guarded.** A creature needs only the alarm at its *own* cell on each day it
  stood there, and `alarm_field(day)` is empty whenever no one was primary-afraid at `day`
  — always, on seed 42 — so the transient fold short-circuits to an emptiness check on the
  canonical world (free) and costs only where alarm emission happens. Re-time the
  possession walk; short-circuit on the empty field.

## Determinism

Genesis byte-identical: `believed_hazard` and `alarm_field` live only in the vessel's
session tick, never in worldgen. The transient memory is a fold over already-committed
`agent-at` facts + a re-derived (pure-over-frozen) alarm field — **no seed draw, no new
predicate, no epoch.** The re-derivation is deterministic (a pure replay); the roster
iteration accumulates into `BTreeMap`/`BTreeSet` (order-independent); the recursion is
broken by construction (terrain-only replay). Stream consumption order is untouched.

## Success criteria

- `frightened_at(cell, day)` returns The Haunt's terrain verdict when the alarm field is
  empty at `day`, and additionally fires on a cell whose *re-derived* alarm at `day` pushed
  the creature over `act` — a unit test on a planted history with a primary-afraid emitter.
- `believed_hazard` **clears** a cell whose most-recent visit was safe though an earlier
  visit was alarm-frightened (the staleness rule, live) — a unit test.
- `new --seed 42` and the seed-42 possession galleries are **byte-identical** (the transient
  set is empty — no primary-afraid emitter).
- An **end-to-end test** (the phantom): a creature alarm-frightened at a cell on an early
  tick (a primary-afraid neighbour beside it) later plans a journey that **detours around
  that now-safe cell** (the neighbour long gone), where a never-alarmed control goes
  straight through; and once the creature safely revisits the cell, the detour ceases (the
  fear cleared) — proactive avoidance of a danger that has passed, then its disproof.
- The **health null-control** holds (a creature detouring around a phantom still reaches
  water; routing is a seek, not distress).

## Reserved (all still PSY-11's body)

- **The visceral phobia** — a Danger-drive felt-dread term reading the *transient subset*
  of `believed_hazard`, so a creature feels fear standing on now-safe remembered ground
  (byte-safe because the transient subset is empty on seed 42). The Haunt's reserved
  "felt-dread-at-a-now-safe-cell," now with a safe home.
- **Time-decay of un-revisited fear** — a forgetting half-life atop the revisable rule, so
  a phantom fades even without a corrective safe visit.
- **The generalized `frightened_at(cell, day)` absorbing new sources** — the reserved
  live-moving-predator's acute proximity (its position at `t` is committed) and diurnal /
  seasonal terrain danger (`terrain.hazards` at day `t`), each dropping into the same probe
  with no new mechanism.
- **Superstition → collective taboo** — the individual phantom (a true memory, a false
  present inference) shared across a herd or people becomes taboo & haunted ground (The
  Haunt's reserved collective form + The Alarm's social channel + religion).

## Flagged items (G3)

1. **[design — the recursion fix] Terrain-only replay.** The alarm-field re-derivation
   must use terrain-only memory (empty roster) or it recurses forever. It breaks by
   construction (empty roster ⇒ terrain-only ⇒ no re-entry), the same primary-only
   discipline as The Alarm. Recommended as specified; confirm.
2. **[scope] Planner-only phobia (felt-dread reserved).** v1 makes the phantom *planned-
   around*, not *felt*, to stay byte-identical (a felt term reading the full memory would
   drift seed 42's static fauna set). The visceral felt phobia — reading only the transient
   subset — is the natural next campaign. Confirm v1 stops at planned-around.
3. **[performance] The re-derivation cost.** Re-deriving the alarm at past visit-days is
   O(roster × history) worst case; guarded by the empty-field short-circuit (free on seed
   42). Set against the possession-walk timing during execution; the fallback is to cache
   the per-day field within a fold and to skip days with no primary-afraid emitter.
4. **[determinism] No new draw / no epoch.** The transient memory is re-derived from
   committed facts; genesis stays byte-identical. Leads the determinism review, a no-op
   there by construction.
