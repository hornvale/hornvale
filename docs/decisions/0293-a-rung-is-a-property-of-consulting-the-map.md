# 0293. A rung is a property of consulting the map, not of the walker

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot; the campaign
controller's Ruling 29, which names the property and deliberately leaves the
mechanism to the implementer) · **Relates:**
[0160](0160-walk-is-the-clients-third-focus-and-its-default.md) (the focus split
this rides on), [0292](0292-centre-on-arrival-anchor-on-gesture.md);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a client where the same raster now draws both the walking view
and the consulted map, we decided that **in `Focus::Walk` the plate always shows
the band the player is standing in** — a zoom rung is state belonging to a map
consultation, and it does not follow the walker out of the map.

## Context

This decision exists because a claim ran ahead of its evidence and would have
shipped. Un-gating the raster for the walk band, the campaign controller wrote:
*"the perception overlay already paints the observer and marks into the plate, so
`@` and creatures survive."* That is true **only at band B**, and it was asserted
without checking the other rungs.

The reachable consequence was six keystrokes from the opening screen — `map ↵`,
`-`, `Esc` — after which the walking plate's only source is the world: no `@`, no
creatures, no cursor, no strip, while the entry pane beside it narrates the
player's immediate surroundings. **The picture and the prose described different
places.** Before the change the state was unreachable, because the walk view's
fallback always centred the observer.

## Consequences

- **A rung is scoped to the map focus.** Leaving the map returns the walking view
  to its own band unconditionally, so no map gesture can leave the walker looking
  at the wrong scale.
- **A reader's map zoom does not survive leaving the map.** That is the accepted
  cost; the escape hatch, if it is ever wanted, is `enter_map` remembering the
  previous consultation's rung, which is additive and registered rather than
  shipped.
- **The property was named and the mechanism was not prescribed.** Three
  candidate shapes were offered and the implementer picked and justified one — a
  deliberate division, because the controller had just been wrong about this
  exact code and the implementer had the tree in front of it.

## See also

Spec §4.3; `clients/game/bin/src/driver.rs` (`at_walk_band_rung`).
