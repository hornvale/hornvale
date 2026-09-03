# 0627. The emitter scan advances over new sightings through a read-side verdict index, and is still not a tenant

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:**
[0538](0538-the-trail-is-a-resident-index-not-a-cached-hub.md);
[0539](0539-a-past-instant-read-resumes-from-the-reset-checkpoint.md);
[0626](0626-a-terrain-verdict-is-held-for-the-session-keyed-by-room.md);
[The Detent spec](../superpowers/specs/2026-09-02-the-detent-design.md) §2.2,
§2.3; [ledger](../superpowers/ledgers/2026-09-02-the-detent.md) #4

In the context of an emitter scan that rebuilt itself every tick over every
room every roster member had ever stood in, facing The Pawl's ruling that
`Alarm` is **not** a tenant (nothing a `LedgerFold::absorb` may read can supply
a terrain verdict), we decided that the scan **advances over new sightings
through a read-side verdict index** — a per-entity memo of a pure predicate,
advanced from the trail by a consumed-prefix cursor — accepting that the index
is state the store holds without `absorb` ever seeing it.

## It does not reopen the tenancy question, and the distinction is exact

`FrighteningGround` lives in the resident store and is keyed by entity, but it
is **not a `LedgerFold`**: it is outside `advance()` and `position()`, and
`absorb` still sees only facts. What it holds is the *read*'s answer, not the
ledger's: per `(entity, room)`, whether `threat_field(room, threat_niche) ×
mettle_factor(boldness) ≥ DANGER_ACT`. Over a fixed terrain that is a constant,
so keeping it is a memo of a pure predicate applied at read — exactly what The
Pawl already said `KnownWater::water_at`'s `is_water` is, with the one
difference that the answer is kept rather than re-asked. Its state is a pure
function of `(ledger prefix, terrain)`, discardable at any instant, and rebuilt
on demand; it carries the same discard-and-rebuild schedule every Pawl tenant
carries.

Its shape:

```
FrighteningGround, per entity:
  consumed:    usize                    trail entries already judged
  frightening: Vec<(WorldTime, Facet)>  frightening rooms, ascending by FIRST visit
  judged:      BTreeSet<Facet>          every room judged, either verdict
```

A past `t` is served without a rebuild or a filter: a room is in the scan's
domain at `t` iff its first visit is `≤ t`, so `frightening` sorted by first
visit answers "the frightening rooms at `t`" as a `partition_point` prefix.

The terrain-ownership rule is
[0626](0626-a-terrain-verdict-is-held-for-the-session-keyed-by-room.md)'s,
unchanged and shared: one index per `(LocaleContext, predator field)`, the same
sentence `SustenanceMemo` and `GroundHazards` carry, held by the same
two-terrain aliasing test — which observes the aliasing it forbids rather than
assuming it impossible.

## Byte-identity was proven site by site, not assumed

The emitter-free read's old domain ("some visit ≤ t was frightening") equals the
index's ("first visit ≤ t and the room is frightening") because a room's visit
days are ascending and its first is the minimum. The scan's `ever` and its halo
union are the same sets in a different insertion order into a `BTreeSet`. The
one shared predicate `feels_frightening(threat, 0.0, boldness)` agrees with the
scan's old `threat × mettle ≥ DANGER_ACT` on **every** `f64`, not merely on the
30,401-point sweep, because the clamp can only move a value across 1.0 or 0.0
and the threshold is 0.3. The emitter path's arithmetic, its ordering and its
witness-first call are untouched. The verbatim pre-rewrite bodies are kept as
FOLD-equals-SCAN oracles and compared at every position and every third
position on two shapes.

## Consequence

Measured on the 50-agent seed-42 shape at tick 60: `hazards()` calls per tick
fell from **44,694 to 3,168**, and a repeated per-creature read from 1,089 to
**0** — the index answers without asking the terrain at all, which a memo alone
could not do. The scan's judged rooms per tick grow **2.42× slower** than the
roster's distinct rooms. Completing the design's prefix read on the
emitter-free path (moving the O(distinct-rooms) latest-visit map below the early
return, where only the emitter path pays for it) took the fold's elasticity from
0.245 to **0.04**, against a same-box control of 0.91.

The remaining 3,168 calls are attributed rather than left as a residue:
`alarm_field_memo`'s per-member gate 450, the scan's per-member home judgement
450, and **2,268 (72%)** the walk's own `Danger::urgency` per-step sampling in
`advance_one`'s decide loop. The fear fold is now 28% of the tick's terrain
questions; the live drive's per-step sampling is the rest, and this campaign
deliberately does not touch it.

The cost accepted is a second unbounded structure — 4,665 entries and ~215 KB
at 200 ticks on the 50-agent shape — which nothing here evicts, and which is
Penstock stage 4's to bound.

**See also.** [The Detent chronicle](../../book/src/chronicle/the-detent.md);
[The Detent spec](../superpowers/specs/2026-09-02-the-detent-design.md) §11.3,
§12.3.
