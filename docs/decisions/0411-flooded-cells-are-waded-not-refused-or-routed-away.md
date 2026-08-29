# 0411. Flooded cells are waded, keyed on whether the space is worked

**Status:** Accepted (2026-08-28) · **Decider:** Nathan, superseding an
earlier in-campaign choice at the Task 0 stop · **Relates:**
[0406](0406-the-underworld-is-a-band-not-a-fold.md) (the `submerged` band
this rule declines to route through) ·
[The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of a measurement showing 58.7%–73.4% of every rung flooded, we
decided that **wet cells are walkable (you wade) and drowned cells are a
separate, rare, deferred category — never that flooded cells are impassable,
and never that they route into the `submerged` band** — accepting that
drowned-rung diving and swimming ship as a designed seam, not a built
mechanism, this campaign.

## Context

Three candidate rules were measured against 60 real seeds through the shipped
generator, deriving depth and water table exactly as production does (never
an invented input): impassable, wading, and routing into `submerged`.
`reach % (Flooded passable)` was **100.0% at every rung** — flooding fills
whole partition-tree leaves and never severs the tree, so connectivity was
never actually at risk.

**The implementer's first choice (rule 3, routing to `submerged`) was
self-defeating and is recorded rather than quietly replaced, because the
reasoning error is reusable.** `submerged` has no lateral geometry —
`session.rs` refuses `go` there because a bearing under water has nowhere to
go — so routing 58.7%–73.4% of every rung there would have made it
*unwalkable*, behaviourally identical to the impassable rule the same
measurement had just rejected, at a higher machinery cost. A rule that names
a mechanism must be checked against what that mechanism can currently do,
not against what its name suggests.

**Nathan's correction, at the Task 0 stop, was categorical rather than a
different pick from the same three options: "wet" and "drowned" are two
categories and the code had one.** A cave with water in it and air above it
is a damp underworld — `domains/terrain/src/water_table.rs` was deliberately
calibrated for exactly this drowned-share, and the measured 58.7%–73.4% is
that calibration showing through, not a defect. A chamber flooded floor to
ceiling is a different thing: not enterable sideways at all, only dived into
from above, and is deliberately out of scope.

## The rule

Wetness keys on `LeafStyle.worked` (already drawn per leaf, already
compounded down a descent by inertia): a worked leaf is drained, a natural
leaf is wet. Movement is a **mode**, not a boolean — one seam answers "how
can this body move through this cell", returning `Walk` or `Wade` today,
`Swim`/`Fly` reserved for later. `UNDERGROUND_LATERAL_REFUSAL` is deleted.

## Consequences

- **Target: drowned cells under 5% of rungs**, entered only vertically
  (`dive`/`surface`), which this campaign does not implement — it needs an
  amphibious or carried-item capability model The Chattel is building in
  parallel, and building a second one here is the failure this defers.
- **Drow-tier descents come out dungeon-dry, wild caves wet, fungal gardens
  wetter still** — from a dial (`worked`) already turning, not a new one.
- **The rejected `submerged`-routing branch is not deleted from the record**:
  a future campaign reaching for "route it to the band that already has a
  refusal" should read why that failed here before repeating it.
