# The Rhumb

A *rhumb* is a course of constant bearing — the word comes through Spanish
*rumbo* from Greek *rhombos*, a spinning top, a thing that turns about a fixed
axis. Its other name is the **loxodrome**, *loxos* + *dromos*, the slanting
run. On a Mercator chart it is a straight line, which is the whole reason
Mercator drew his chart that way; on the globe itself it is a spiral that
crosses every meridian at the same angle and winds infinitely into the pole
without ever arriving.

This campaign is about holding such a course over ground that is made of
triangles.

## Eight names, three doors

Hornvale's walk band is a geodesic lattice: every room is a triangular face of
a refined icosahedron, and a triangle has exactly three edges. The `go` verb
offered eight compass points. Five of them refused.

That is not a bug in the sense of a mistake in a line of code — it is an
arithmetic certainty that had been sitting in plain sight. Three edges cannot
serve eight names, at any address, on any seed, forever. Measured from the
seed-42 flagship: `e`, `sw` and `nw` moved; `n`, `ne`, `se`, `s` and `w` were
answered with a sentence. Worse than the count was its instability. Which
three worked depended on the orientation of the triangle underfoot, and the
lattice alternates orientation between neighbours, so the set of working
directions changed with every step. There was no sequence of `go e` that
walked east. A player who typed it twice went somewhere, and then somewhere
else, and the two somewheres were not in a line.

This is the exact mirror of a contract [The Blocking](./the-blocking.md)
shipped — *every destination the render depicts must be reachable by a named
command* — and of the limit [The Handle](./the-handle.md) recorded when it
widened that contract: the check runs in one direction only, and is
structurally blind to **over-admission**. Here was the over-admission, in the
oldest verb in the game, wearing the most ordinary word in it.

## Three repairs, two of which are not about navigation at all

The obvious fixes edit the graph. Give each cell twelve neighbours instead of
three; or keep three and reweight them so a compass point can pick a
best-effort edge. Both make `go e` work. Both also change what *adjacent*
means — and adjacency is not the player's private vocabulary. A\* reads it,
`hops_between` reads it, ecology spreads along it, settlement fitting scores
against it. A change made to satisfy a compass would arrive silently in all of
them.

So the campaign took the third road, and
[decision 0141](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0141-compass-navigation-is-an-overlay.md)
states it:

> **Compass navigation is an overlay, never the graph.** A player-facing
> heading resolves to an edge of the geodesic adjacency; it never adds,
> removes or reweights one.

The memory goes in the walker, not in the world. A possession now carries a
**course**: a held bearing, and a *reckoned point* — a latitude and longitude
that advances exactly along the loxodrome, one derived step length per move.
Each step, the walk takes whichever of the three real edges lands nearest that
reckoned point. The reckoned point is **never re-seeded** from the cell the
walker actually reached.

That last clause is the design. Everything else is arithmetic. A course
re-seeded from the walked cell each step is dead reckoning in name only — it is
the memoryless zigzag the campaign exists to remove — and it is remarkably good
at passing tests, because a re-seeded point is still one step ahead of its
cell. "The reckoned point advances" and "the reckoned point differs from the
cell" are both true of the broken version. Two assertions written exactly that
way passed under a deliberate re-seed mutation before anyone noticed, and were
caught by mutating the code rather than by reading it.

All eight directions now resolve, from every cell, and a held heading survives
the orientation flip.

## What the lattice will not do, and this is the finding

The campaign preregistered a hypothesis with two clauses. One held. One is
false, and the false one is worth more.

**The meridian invariant holds, everywhere.** On a due-north course the carried
reckoned point never leaves its meridian — to within 5.68 × 10⁻¹⁴ degrees of
floating-point wobble in a modulo chain, constant across forty steps and not
accumulating. This is the dead-reckoning property, and it is the half that
distinguishes the design from the thing it replaced.

**The one-step bound on the walked cell is false, and the error is unbounded.**
The prediction was that the cell the walker actually occupies would stay within
one step length of the ideal rhumb. Measured on correct code, it exceeds that
at 27 of 40 steps at some addresses — and it does not settle. It reaches 8.8
step-lengths at step 99, 44.0 at step 499, and **172.6 at step 1,999**, growing
linearly at roughly 0.086 per step. This is not a transient.

The mechanism was mis-diagnosed twice before it was measured. It was first read
as an equatorial effect, then as a latitude effect, and neither survives
contact with an eighty-address sweep: two addresses at latitude *exactly* zero
show no exceedances at all, four between 31.7° and 58.3° show 19 of 40, and one
fixture's sibling at the *identical* latitude on a different base face is among
the worst in the set. What actually governs is the local triad's alignment, and
because the lattice alternates orientation the walker meets two triads on
alternate steps:

- Where one triad offers an edge at bearing **exactly 0.00°** and the other
  offers symmetric **±65.35°**, the two steps cancel. The walk closes into a
  four-cycle with zero net bias and stays bounded — 0.851 step-lengths,
  forever.
- Where the two near-north edges are **asymmetric** — +18.0° and −47.35°, whose
  midpoint is −14.7° — no combination of available edges points north. Every
  step pays a small directed tax, and nothing ever refunds it.

What this retires is a belief, not a feature: that a discrete triangular
tiling can track a continuous rhumb to within a cell everywhere. It cannot,
and the residual is a property of the ground rather than of the navigation.
Compass navigation works; every direction resolves; the course holds its
bearing. But a player walking a long due-north line drifts off their meridian
at a rate set by the ground they cross, and that is now a documented property
of the world instead of a surprise waiting in it.

The preregistered text was left exactly as frozen. The result is recorded
beneath it, marked post-hoc, because editing a hypothesis to match its outcome
is precisely what freezing one prevents
([decision 0016](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0016-studies-preregister-hypotheses.md)).
The falsification also arrived by an uncomfortable route, and the record says
so: it surfaced when a test fixture was moved from an address where the
assertion failed to one where it passed. That is metric-chasing by the standard
test — the move would not have been made had the assertion passed — and it was
caught only because the change was disclosed in full and a reviewer re-derived
the mechanism instead of accepting the one it was given.

## The pole, which is behaving correctly

A rhumb at any non-cardinal bearing winds infinitely around a pole in finite
distance. That is the curve, not an artifact of anything. A discrete lattice
cannot render infinite winding, so it renders the smallest circle it has —
three cells ringing the pole — and cycles among them.

It would have been easy to call that a defect and legislate a termination rule.
The campaign declined, on the grounds that the walker is doing exactly what a
compass-follower does, and that inventing a rule here would be inventing a
defect in order to fix it. A clamp stops the reckoned point just short of the
pole so that resolution stays well-defined, and the narration discloses the
clamp once when it happens. Nothing else was added.

## What did not move

The three-edge adjacency graph. `RoomAddr::neighbors` is untouched by this
campaign, which is a claim the diff can be asked to verify rather than a
promise. A\*, `hops_between`, ecology and settlement fitting never saw a
change, and no world moved: no new stream label, no new draw, no epoch. The
committed session golden drifted in exactly **one leaf value out of 888** —
the narration prose — with all 133 of its key paths identical on both sides.
The schema did not move; a sentence did.

The sentence it moved to is the campaign in one line. The old refusal is gone
entirely, because once every direction resolves there is nothing left to
refuse — `go` never checked passability in the first place, so the compass was
the only thing it had ever turned anyone away for. What the walker is told now
is simply true:

> No direction here is closed; the nearest ground lies E, NW, SW.

## The chart still points the way it always did

One task was planned and deliberately not done. The chart was to be turned
north-up, so that the direction a player reads and the direction a player
types agree. It cannot be done yet, and the reason is upstream of the
projection: the cells that go out on the wire carry no latitude, no longitude
and no bearing. A client has nothing to rotate. Adding position to the cell is
the first move of the successor campaign, which will be rewriting how that
chart is drawn in any case — and drawing it twice was the thing this campaign
was sequenced first to avoid.
