# The Crosscut

*A crosscut is a tunnel driven across the vein to join two drifts. It is the
part of a mine that turns two dead ends into a loop, and a mine drives one
because air has to flow: a working with no loop cannot breathe.*

Before this campaign, every scale of the underworld was a tree or a path. A
branch was levels 1..n in a line. A level was a binary partition tree whose two
children were joined by exactly one passage per split — `n` leaves, `n − 1`
passages, cyclomatic number zero, measured over two hundred seeds before a line
of new code was written. Every level had exactly one stairway down and one up,
and the descent verb relied on that count to know where a stairway landed.

That is precisely the structure Joris Dormans' essay on cyclic generation opens
by rejecting: a dungeon that "essentially is a branching tree" and therefore
"will have many dead ends and force the player to track back frequently." The
underworld arrived there honestly. Each campaign built the minimal next thing,
and a tree is always the minimal next thing — it is what you get when you make
sure everything is connected and stop. The minimal next thing after that is the
one primitive none of them needed: take an edge and replace it with two paths.

## Two readings of one skeleton

Dormans supplies the structural half and Alexander the inhabited one, and the
useful discovery is that they are describing the same object. Dormans notes
that cycles were "inspired by previous research in architecture, urban
planning" and that "in most cities, buildings, and parks, you can go around in
circles" — which is *A City Is Not a Tree* restated for level design. His
lock-and-key cycle and Alexander's intimacy gradient are one skeleton read by
two people. The intruder walks the loop and meets gates. The resident walks the
same loop and passes from public to private along it. Neither reading is
possible on a graph with no loop in it, which is why structure had to come
first and why this campaign ships no gate and no resident at all.

## The grammar has exactly two operations

A descent's plan is grown before any level is carved — the whole five-rung
descent at once, not one floor at a time, because a cycle that cannot leave its
floor is a much smaller idea. Each level is a coarse grid of cells derived from
that rung's extent (eight cells to a side, so the shallowest rung is five by
three and the deepest seven by four); a plan node is a grid cell that will
become a carved region, and a plan edge is a way through.

Growth starts with a **spine**: an entrance on the west edge of the top rung, a
path across each level to a drawn stairway cell, a stairway down to the same
cell one rung below, and so on to a terminus at the bottom. That is one long
series composition and it touches every level once. It is also, by itself,
exactly the tree the campaign set out to replace.

Then two operations, and only two:

- **Series — lengthen a path.** Replace an edge between two regions with a
  path of two or more hops through cells nothing has claimed yet. This makes
  room for later loops to attach and makes their two sides differ in length.
- **Parallel — split an edge into two paths.** Take an existing edge, walk a
  short way along it, and find a second, disjoint route between the same two
  ends. Now there are two ways round, and the two ways round are a *realm*.

Nesting is not a third operation. Applying the parallel move to an edge that
already lies inside a realm produces Dormans' nested cycle for free, and the
tree of compositions that results *is* the realm tree — which is why "which
cycle owns this room" has exactly one answer, and why the graph is planar with
a grid embedding without anyone having to check.

The second path may descend. A worked example, small enough to hold in the
head: a corridor of three regions on rung 2, `a — b — c`. Apply the parallel
move to `a — b`, taking `b — c` as the far side, so the two ends are `a` and
`c`. The existing side is the two hops that were already there. For the new
side, the plan descends a stairway at `a`, runs two regions along rung 3, and
climbs a second stairway back up at `c`. The result is a loop you can walk in
either direction, one half of which is a floor below the other — down one
stairway, along, and up a different one. Dormans' cross-floor cycle, and it
costs nothing to embed because both stairways land on cells that were free on
both rungs.

How many loops a level gets is not a dial. The base comes from the rock,
because the rock made the routes: karst is a maze because water dissolves many
ways through it, a lava tube is one conduit, a fracture system sits between. A
worked place adds one, because a working must ventilate — intake and return —
and a dead-end drift is the unventilated one. The campaign's own name is that
argument. The only authored number in the whole grammar is the clip Dormans
gives for legibility, two to five cycles per level, and it is cited as his.

## "By construction" was a claim, not a proof

The design asserted four properties by construction. Three of them were:
planarity, reachability from the entrance, and at most one innermost realm per
region — each a straightforward consequence of the two operations, each pinned
by a test rather than measured.

The fourth was **every level has at least one loop of its own**, and it was
false. A sweep of four hundred seeds across twelve vertices and every
combination of rock and workmanship — seventy-two thousand levels — found
twenty-four with no loop at all. The mechanism is that a cross-floor cycle
anchored on one level spends cells on the level *below*, and the floor below
then has nowhere left to close a loop of its own. The property had been
reasoned from the algebra of the two operations, where it does hold, and
asserted about the geometry they run on, where it does not.

The first repair was a count: refuse a cross-floor cycle unless the floor below
would keep enough free cells afterwards. It cut twenty-four to nine and cost
almost nothing — cross-floor share moved from 0.2987 to 0.2982 — and the nine
that survived it are the interesting part. They were not short of cells. Two
cross-floor landings had *enclosed* a level's own passages away from its free
cells: there was room, and no route to it. A quantity cannot see that.

What made the claim true is a **capability** test rather than a count. Every
move that spends a level's cells asks a different question first: if this
level has no loop of its own yet, is there still at least one same-floor loop
it could close? A cross-floor cycle that would answer no falls through to a
same-floor one; a lengthening move that would answer no is refused. With the
invariant in place the exhaustive fallback pass — one deterministic sweep,
first loop that fits, no draws — succeeds by construction, and the starved
count is zero of seventy-two thousand. The cost is about two and a half points
of cross-floor share, 29.8% down to 27.3%, and nothing else measurable.

The lesson is not about caves. A property claimed *by construction* is a claim
about the whole space, and a sweep costs an afternoon.

## The repair that carved through its own walls

The most valuable thing the plan buys is the wall. Two adjacent regions with no
edge between them keep solid rock between them — the player's "no way through
here" — and a partition tree could never say that, because siblings were always
joined. It is the headline property of the realization, and the campaign nearly
shipped without it.

Writing a stairway onto a cell can sever the region it lands in: the cell it
overwrites may have been the only way across. The realizer therefore repairs
connectivity after every stair write, by searching for a route around the new
stairway. The first version searched *the whole level*. It found routes, as
asked. Where the nearest route lay through a neighbouring region the plan had
deliberately left unlinked, it carved a corridor straight through that wall —
repairing a real defect by silently destroying the property the campaign
existed to create.

Nothing was red. Every connectivity test passed, because the repair made things
*more* connected, and connectedness was all anything asserted. The review found
it by measuring rather than by reading: on 609 of 1,620 sampled levels —
**37.6%** — a pair of regions the plan left unlinked had a walkable route
between them anyway. The fix is one argument: scope the repair search to the
placing region's own rectangle, so it can only ever route within the region it
is repairing. And the property now has a test of its own, which flood-fills
between every grid-adjacent pair of regions the plan left unjoined and fails if
the flood ever arrives — captured red against the unscoped version before the
fix was restored.

## Stairs pair by coordinate

With several stairways to a level, the descent verb needs to know which landing
belongs to which stairway. The answer is that a stairway's two ends share a
cell: a downward stair at a coordinate on one rung is the same physical
stairway as the upward stair at the same coordinate one rung below. The verb
stops counting and starts reading a place.

The rejected alternative was ordinal — the k-th stair down pairs with the k-th
stair up. That is the id-as-offset defect one scale up: insert a stairway
anywhere and every landing after it moves. Rung extents grow with depth but
share their origin, so the intersection two adjacent rungs need is always
non-empty, and that is asserted for every rung pair the ladder admits rather
than assumed. A landing cell is made walkable even where the carve left it
rock, because a stairway has a foot.

One guarantee genuinely weakened, and it is worth naming rather than burying: a
level is no longer guaranteed to be connected *within itself*. A cross-floor
cycle's landing corridor can join the rest of its floor only through its
stairways — which is the design working, not failing. The guarantee is over the
whole descent, and the tests that asserted the per-level form were rescoped to
the scope the plan actually promises.

## Four readouts, frozen before the code

The verdict words were fixed in the design, before any of this existed, and the
numbers below are read off a witness page regenerated from the seeds every
time. Three seeds, every cave-bearing vertex of each: 874, 1,681 and 1,266
descents.

```text
                             seed 42     seed 7    seed 1234
  loop share (median)         0.1233     0.1111      0.1042   vs 0.50  FALSIFIED
  cycle membership (median)   0.8548     0.8533      0.8511   report only
  cross-floor share           0.9622     0.9405      0.9423   vs 0.25  PASSED
  semilattice overlap         0.306      0.286       0.281    report only

  anchored loops per level, median:      wild cave   drow tier
                            lava tube        1           2
                            fracture         2           3
                            karst            3           4
       lava tube < fracture < karst                      PASSED
       drow tier > wild cave, within every kind          PASSED
```

**The headline is the falsified one.** Loop share asks, of every region other
than the entrance, whether there are two edge-disjoint routes home — Menger's
criterion, the region lying in the entrance's two-edge-connected component.
Predicted at least 0.50; measured about 0.11.

The honest reading is that the metric is blunter than the design saw, and the
bluntness was disclosed while the code was being written rather than after the
number came in. A cave has one physical doorway, on the west edge of the top
rung. Whenever no loop happens to attach at that doorway, the doorway is a
bridge, and *every* region in the entire descent fails a test phrased as "two
routes back to the entrance" — the whole five-rung descent reads zero, however
many loops it has. About forty per cent of seeds land that way.

The frozen prediction was allowed to fall rather than be redefined; that is
what preregistration is for, and a metric's bluntness is itself a finding. What
the campaign did instead was print a second number beside it, clearly marked
report-only because it was added after the behaviour was seen and so cannot be
a prediction: the share of regions lying on at least one loop, regardless of
where the entrance is. That is **0.85** on all three seeds. Read together the
two say something precise: the levels are full of loops, and the front door is
a bridge.

**The passed ones need a caveat of their own.** Cross-floor cycles were
predicted in at least a quarter of descents — the multi-floor claim of the
whole program, made falsifiable — and occur in 94–96% of them, four times the
headroom the prediction asked for. The density ordering passed on both halves,
on every seed. But look at what the medians *are*: exactly the budget the rule
computes, in every cell of that table. The grammar reaches its target on
essentially every level, so the readout confirms that the budget is derived and
reached; it does not independently witness that the world varies the way the
rule claims. The derivation is real. The measurement of it collapsed onto the
parameter it was meant to test, and saying so is cheaper than discovering it
later.

The semilattice overlap is Alexander's criterion measured for the first time —
among regions on any loop, the share lying in two or more realms, which is a
region on the shared side of a nested cycle. About 0.29. There is no threshold
because nothing yet says what a good number would be; this is the first column
of what will eventually be a series.

## The plan describes the place a player stands in

One correction was made after the design was approved, and it is the kind worth
recording because the error was invisible from the documents. The design keyed
the plan to a *run* of the chamber lattice and said it refined the lattice's
passage function. Reading the descent code while writing the implementation
plan showed the walked object is something else: entering a cave builds one
level per habitation rung under a single vertex, and reads no run, no branch
and no entrance at all. The lattice's edge half has no walking consumer, which
was already on record.

A plan keyed to a run would have been correct about the lattice and would have
described a place nobody stands in. So the plan is keyed to the seed and the
vertex and covers the five rungs the descent actually generates. Wiring the
walk to the lattice is a real gap; it is a campaign, not a paragraph, and this
one did not widen into it.

## What is deliberately not here

No gates. The plan exports each realm's length class — Dormans' four
combinations of a long and a short side — and stamps nothing with it; locks,
keys, valves, danger and secrets are the next campaign, and the asymmetric
stairway that goes down but not up is the one that turns a loop into a puzzle.
No residents: the intimacy gradient, the common heart, the hoarder placed by
ecology and the first made chamber a player can reach are the campaign after
that, and the depth and realm attributes they will read are computed and
exported here and read by nothing. No junctions: a cycle that goes down one
branch, across into another cave system, and up again would be a shortcut
between two different cycles, which is exactly what a series-parallel grammar
cannot express — a different grammar or a third composition rule, not a bigger
budget. No vaults, no prose for what a cycle looks like underfoot, and no
camera that follows you across a level now large enough to walk off the edge
of the plate.

What is here is the skeleton, and the two readings it was chosen to carry.
