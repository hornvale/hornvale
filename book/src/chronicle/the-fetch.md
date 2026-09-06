# The Fetch

Remembering water and being able to act on that memory had been joined to the
wrong place. `believed_water` derived its candidates from committed visits, but
asked whether each was reachable from the creature's home. A creature away from
home could therefore hold a remembered water room that it could reach from where
it stood and still be told it knew no water.

The smallest witness held two remembered rooms. The home-anchored and
current-position folds each admitted one room, but they admitted different
ones. The probe made two direct route searches from home and two from the
current position: `2 home / 2 current`. That is a route-count witness, not a
timing claim.

## What changed

The Fetch kept remembered water allocentric: it is still derived from the
committed visit history. It made the choice actor-relative. Both the fresh
belief fold and its incremental counterpart now rank remembered water from the
creature's committed current position, excluding unreachable rooms and keeping
the existing `(hop count, Facet)` ordering. The public result remains
`Option<Facet>`.

The current-position path deliberately does not reuse `RouteMemo`. That memo
is home-keyed; applying it to a moving position would change its key population
without a boundedness or ownership argument. The campaign retained direct
searches and recorded their count instead of introducing a cache incidentally.

## What the observers said

Implementation landed in `dca20b44c`, the route-count probe in `7ceea074b`,
and observer verification in `38ea7618d`. Ten belief tests passed, as did the
errand, affect, and seed-42 snapshot observers. The implementation commit gate
passed its audits and **1,298** sub-floor tests. No value fixture moved, and no
`REBASELINE=1` run was needed.

The result is deliberately narrow: it changes the geometry used to choose among
already remembered water, not the memory facts, public vessel types, or client
surface. No schema or epoch artifact was introduced by this close record.

## What remains

A position-keyed route cache remains a separate design question. It needs its
own key, ownership, and boundedness evidence before it can replace the direct
current-position searches.
