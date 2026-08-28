# 0294. A mode gesture is not a fetch

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (the founding defect
report) · **Relates:**
[0160](0160-walk-is-the-clients-third-focus-and-its-default.md) (what a focus
is), [0022](0022-sim-emits-data-clients-render.md);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a client where typing bare `map` both entered the map focus
*and* sent `map` to the sim as a verb, we decided that **entering a client focus
is a mode gesture and sends no verb** — the acknowledgement is the focus
changing.

## Context

`driver.rs` already carried a comment arguing that `map` is a mode gesture and
not a fetch. The code simply did not do what the comment said: bare `map` called
`self.handle("map")` **before** `enter_map()`, so the sim answered a mode change
with a picture, and that picture — a pre-formatted ASCII chart — arrived in the
prose channel and was mangled by the pane's word-wrapper. That is the second of
the three defects this campaign was opened for, and its first cause.

The precedent for the acknowledgement was already in the same file:
`recentre`'s own doc states it for itself — *"the acknowledgement is the map
redrawing."*

## Consequences

- **Only the BARE form changes, and that is load-bearing rather than
  conventional.** `map out N` still returns the sim's own picture, which is the
  diagnostic path that once caught a wrong client projection by rendering the
  client and the sim side by side over the identical thirty-one facets. Removing
  every route to the sim's picture would delete that comparison. The bare/argument
  split preserves it for free.
- **A turn is not consumed by looking at a map.** Whether that matters depends on
  a turn economy this client does not yet have; it is the correct default either
  way.
- **What we give up:** nothing measurable. The gesture was already documented as
  a gesture; this record makes the code agree with its own comment and states the
  rule so the next focus added does not have to rediscover it.

## See also

Spec §4.2; `clients/game/bin/src/driver.rs` (the `map` arm).
