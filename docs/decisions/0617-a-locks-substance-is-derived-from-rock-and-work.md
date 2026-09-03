# 0617. A lock's substance is derived from rock and work; the pattern chooses where, never what

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0568](0568-cycle-density-is-derived-not-authored.md) (cycle
density is derived from rock and workmanship),
[0616](0616-a-gate-is-a-requirement-on-a-way.md),
[0620](0620-the-cycle-pattern-inventory-is-a-frozen-corpus.md) ·
[The Brattice](../../book/src/chronicle/the-brattice.md)

In the context of a cycle pattern telling the stamping pass *where* on a realm
to put a requirement, facing the question of what that requirement is made of,
we decided that **a requirement's SUBSTANCE is derived from the rock and the
work — a door needs a maker, a sump needs water-cut rock, a chute needs a floor
below — and the pattern chooses only the placement** — accepting that a pattern
row whose substance no descent can supply is filtered out before the draw, so
some realms draw from a smaller admissible set than others, and accepting that
a lava tube's loops carry no natural passage lock at all.

## Context

Decision 0568 settled the sibling question one level up: how many loops a level
gets comes from the rock and the work, inside Dormans' authored legibility
clip, and is never a dial. The same argument applies to what a loop's gates are
made of, and the frontier's own line on a traversal grammar states it as the
condition of the whole idea being on the right side: gates are defensible "for
as long as [they] are derived — from ecology, from hydrology, from what a
people cut — and not hand-placed."

A draw was considered for which capability a natural lock demands (`Swim` or
`Fly`, per placement) and refused: a draw where a derivation exists is exactly
the "for fun" constant 0568 refused.

## Decision

One table, read from inputs the plan already has (the cave kind and the
character of the place):

```text
  on a PASSAGE, worked (drow tier, or made when a campaign writes one)
                                        -> Key: a door, and its key
  on a PASSAGE, natural, karst | fracture -> Mode(Swim): a sump
  on a PASSAGE, natural, lava tube        -> inadmissible
  on a STAIR (the asymmetric way), any rock -> Mode(Fly): a chute
```

The reasons are the rock's. Karst and fracture systems are cut by water, so a
passage below the water line is a sump; a lava tube is one dry conduit, so its
loops stay open; a chute is a hole in a floor, which every rock has; a door is
a made thing, and in a wild cave there is nobody to have hung one. `worked` is
the same term 0568's budget uses, so the two derivations cannot disagree about
which places are worked.

No gate is hand-placed and no gate is drawn where a derivation exists. The one
draw the pass makes is which pattern a realm gets.

## Consequence

- **Filtering happens before the draw, so the draw count is data-independent.**
  A `Key` row is inadmissible in a natural descent and a natural-passage row is
  inadmissible in a lava tube; both are removed from the candidate set before
  the index is drawn, and the draw is made even when one row or none is
  admissible. The plan's degrees-of-freedom identity therefore gains exactly
  `realms`, as an equality, not an estimate.
- **The panel's own descents carry zero doors, and that is the derivation
  working.** The production walk enters every cave as a wild one, and a wild
  cave is never worked, so no door row is ever admissible there. Doors exist —
  872 of 874 worked descents on the panel's first seed carry one — and nobody
  can walk to one until a campaign gives the walk a worked place to stand in.
  This is disclosed on the committed page rather than smoothed over.
- **`worked()` is an exhaustive match over the character roster**, not a
  `matches!` against one variant, so a sixth character fails to compile here
  rather than inheriting "unworked" from a wildcard and quietly losing its
  doors.
- A same-floor one-way passage has no natural substance and is therefore not
  expressible: a scree slope wants a substance the realizer lacks, and is a
  captured idea rather than an unusable enum arm.

## See also

- [The Brattice design](../superpowers/specs/2026-09-02-the-brattice-design.md) §3.3.
- [The Brattice ledger](../superpowers/ledgers/2026-09-02-the-brattice.md) #2.
- [The Brattice chronicle](../../book/src/chronicle/the-brattice.md).
