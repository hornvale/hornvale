# 0141. Compass navigation is an overlay, never the graph

**Status:** Accepted (2026-08-16) · **Decider:** Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0077](0077-zoom-in-the-room-mesh-is-path-truncation.md),
[0105](0105-water-keeps-bands-rock-becomes-a-graph.md),
[0116](0116-possession-is-a-parameter-not-a-fixture.md)

In the context of `go <dir>` offering eight compass points over a geodesic
triangular lattice that has exactly three edges per cell — so five of eight
directions refused from the seed-42 flagship, and *which* three worked
depended on the orientation of the triangle underfoot — we decided that
**a player-facing heading is an overlay resolved against the adjacency
graph, never a modification of it**, because every alternative that makes
all eight directions work by touching the graph changes what adjacency
means for consumers that are not players, and adjacency is load-bearing for
A\*, `hops_between`, ecology and settlement fitting.

## Context

**The defect was not that a direction was missing. It was that the compass
vocabulary over-admitted.** A triangle has three edge-neighbours, so at most
three of eight compass buckets can resolve from any one cell — the bound is
arithmetic and holds at every address, on every seed, forever. The verb
accepted all eight tokens, dispatched all eight correctly, and then refused
five of them with a sentence. There was no sequence of `go e` that walked
east: the walker took whichever edge happened to bucket as east, and the
next cell's opposite orientation offered a different set, so a held heading
decayed into a zigzag with no memory of where it had been going.

This is the mirror of the parity contract
[The Blocking](../../book/src/chronicle/the-blocking.md) shipped — *every
destination the render depicts must be reachable by a named command* — and
[The Handle](../../book/src/chronicle/the-handle.md) had already recorded
that the contract is one-directional and structurally blind to
over-admission. This is the over-admission, in the oldest verb in the game.

**Three repairs were available and two of them are graph edits.** Adding
edges (a 12-neighbour lattice) or reweighting them both make `go e` work by
changing what "adjacent" means — and every non-player consumer would inherit
it silently: path costs move, ecology's spread changes, settlement fitting
sees a different neighbourhood. The third keeps the graph fixed and puts the
memory in the *walker* instead.

## The decision

> **Compass navigation is an overlay, never the graph.** A player-facing
> heading resolves to an edge of the geodesic adjacency; it never adds,
> removes or reweights one. The three-edge graph stays the single definition
> of adjacency for every consumer that is not a player.

Concretely, a possession carries a **rhumb course** — a held bearing plus a
*reckoned point* that advances exactly along the loxodrome and is **never
re-seeded from the cell the walker lands on**. Each step advances the
reckoned point by one derived step length along the constant bearing, and
the walk takes whichever of the three real edges lands nearest that point.
The course is state on the walker; the graph never learns it exists.

The non-re-seeding rule is the whole design. A course re-seeded from the
walked cell each step is the memoryless variant the campaign exists to
avoid, and it passes almost every obvious assertion — a re-seeded point is
still one step ahead of its cell, so "the reckoned point advances" and "the
reckoned point differs from the cell" are both vacuous against it. Two
separate tests in this campaign were written that way and caught by
mutation rather than by reading.

## Consequences

- **All eight directions resolve**, from every cell, and a held heading
  survives the orientation flip that alternates underfoot.
- **The three-edge adjacency graph is untouched.** `RoomAddr::neighbors` is
  not edited by this campaign; A\*, `hops_between`, ecology and settlement
  fitting never saw a change. This is the property the decision exists to
  protect, and it is checkable by the diff.
- **The lateral refusal is gone.** `go` performed no passability check at
  all — water was already walkable — so the only refusal the verb carried
  was the compass one, and it had no reason to exist once every direction
  resolved. The player-facing listing now says what is true:
  *"No direction here is closed; the nearest ground lies E, NW, SW."*
- **A rhumb at a pole winds rather than terminates, and that is correct.**
  A loxodrome at constant non-cardinal bearing winds infinitely around a
  pole in finite distance; a discrete lattice renders the smallest circle it
  has, three cells ringing the pole. No termination rule was invented,
  because inventing one would be inventing a defect to fix. `POLE_LIMIT`
  clamps the reckoned point just short so resolution stays well-defined.
- **No epoch.** No new stream label, no new draw, no world moves. The
  committed `vessel/session/v2` golden drifted in exactly one leaf value out
  of 888 — `.narration.prose` — with all 133 key paths identical, so the
  schema did not move; a sentence did. `v2` stays `v2`.
- **A documented limitation ships, and it is the campaign's main finding.**
  The carried course holds its *meridian* invariant everywhere, but the
  **walked cell** cannot be held within one step length of the ideal rhumb,
  and the error is **unbounded** — 172.6 step-lengths at 2,000 steps,
  growing linearly at ~0.086 per step. The mechanism is the local triad's
  alignment, not latitude: where one triad offers an edge at bearing exactly
  0.00° and the other symmetric ±65.35°, the walk closes a 4-cycle and stays
  bounded forever; where the near-north edges are asymmetric (+18.0°,
  −47.35°), no combination points north and bias accumulates without limit.
  A player walking a long due-north line drifts off their meridian at a rate
  set by the ground they cross. See spec §7.1, which records this **post-hoc
  with the preregistered hypothesis text untouched** (0016).
- **Whether a bias-correcting resolution would bound the error everywhere is
  a different algorithm, not a fix to this one**, and is deliberately not
  attempted here. Registry row `NAV-bias-correcting-resolution`.
- **North-up charting is not unblocked by this.** `SurroundsCell` carries no
  latitude, longitude or bearing, so no client can project north-up from
  what the wire hands it. That is a per-cell-position problem and it belongs
  to the successor appearance campaign; row
  `NAV-north-up-needs-per-cell-position`.

## See also

`windows/vessel/src/course.rs` (`rhumb_advance`, `step_length_rad`,
`nearest_neighbour`, `bearing_of`, `Course`, `POLE_LIMIT`, `normalize_lon`),
`windows/vessel/src/session.rs` (`go`'s resolution
arm, the `course()` accessor, `back` clearing the course),
`windows/vessel/tests/course_properties.rs` (H1 and its positive control,
direction fidelity, the H3 attractor control, `wrapped_lon_diff_deg`),
`docs/superpowers/specs/2026-08-16-the-rhumb-design.md` §3, §7.1, §11,
[the chronicle](../../book/src/chronicle/the-rhumb.md).
