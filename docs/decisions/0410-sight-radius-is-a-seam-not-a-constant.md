# 0410. Sight radius is a seam, not a constant

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0409](0409-fog-is-a-seen-bitset-per-rung.md) (what a reach
change does and does not retroactively affect) ·
[The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of `chamber_sources` hardcoding `radius: SIGHT_RADIUS` at
three call sites that look interchangeable and are not, we decided that
**exactly one of those three — the implicit torch at the possession's own
cell — is replaced by a named function answering "how far can this body see
from here, right now"**, accepting that the hearth's throw and daylight
through a doorway keep the constant they already had.

## Context

Nathan, at the design stop: *"the implicit light source is extremely
temporary… eventually I imagine we will be able to have lanterns, torches,
candles, magic spells, magic staves with glowing orbs on the end."* Routing
all three `SIGHT_RADIUS` sites through the body's reach would mean a lantern
in your hand brightens every hearth and doorway in the building — a fire's
throw and an opening's spill are properties of the fire and the opening, not
the body carrying it, and pouring that conflation into a second band now
would make carried light a two-band change later.

## The rule

One seam answers the reach question; today it returns the torch's constant,
later it reads whatever the body carries. The other two `chamber_sources`
sites are untouched.

## Consequences

- **A future lantern is a body swap at zero schema cost** — the same move
  The Wanting made reserving `world_view` for belief: the parameter existed
  from the start, so adding a real light model changes what the seam
  returns, not who calls it or what shape the caller expects.
- **A change in reach interacts with decision 0409's bitset exactly once,
  at write time**: earlier-set bits keep whatever reach was in force when
  they were set, so a brighter light later never retroactively illuminates
  what a dimmer one already passed by in the dark.
