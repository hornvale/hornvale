# The Pavement

The walk band was a lie of a particular kind: not a wrong answer, but a
vocabulary larger than the ground beneath it. A player typed one of eight
compass words, the parser understood all eight, the dispatch arm handled all
eight — and the ground offered three edges. Which three depended on the
orientation of the triangle underfoot, so no sequence of `go e` walked east.

[The Rhumb](./the-rhumb.md) found that and repaired it the way the project's
own decision record told it to: keep the graph, put the heading in the walker,
dead-reckon a course across a triangular lattice. The repair worked, in the
sense that all eight words began to resolve. It also produced the sharpest
negative result in the Confidence Gradient — 172.6 step-lengths of drift at
2,000 steps, growing linearly — and the honest conclusion that no checkable
contract could fix it, because both sides were behaving correctly. It was the
tiling declining to represent a continuous curve.

That conclusion was right about the tiling it was written under. It carried a
premise it did not examine.

## The justification that was checkable and false

Decision 0141 had ruled the room graph off-limits, and gave a reason: editing
room adjacency would silently change ecology, settlement fitting and path
costs. This is the right *kind* of reason — it names a blast radius, and a
save-format contract deserves that caution.

It was also false, and falsifiable in one command. At 0141's own commit, a grep
for the room-adjacency accessor across every domain crate returns nothing. It
returns nothing today. Room-face adjacency has only ever been read by the layer
that draws the player: the domains model the world on a vertex mesh and never
ask a room who its neighbours are.

So the ground had been movable for four months, and a bet had been scored as
structurally unmechanizable on the strength of a constraint that did not exist.
The check that would have caught it costs one command and nobody ran it,
because the decision record read as settled — which is what a decision record
is for, and is exactly why a superseded one is dangerous rather than merely
stale.

## Two meshes, one icosphere, and which of them moved

The confusion worth clearing first is that Hornvale has always had two meshes
over the same sphere, in a primal/dual relation: the **vertex** mesh (a level-6
geosphere, 40,962 vertices, degree histogram `{5:12, 6:40950}`) is the field
substrate, where terrain and climate live; the **facet** mesh is the occupancy
lattice, where rooms live. Fields are resolution-free by decision 0038 and
sample at a position, not at an index.

Only the second one moved. `Facet` keeps its `{face, path}` address space, its
packing, its parent/child descent and the zoom ladder of decision 0077 exactly
as they were; what changes is the base geometry the path descends into — from
20 icosahedral triangles to 6 cube faces. A quad subdivides into four quads
precisely as a triangle subdivides into four triangles, so the addressing
algebra is untouched. The genesis layer still runs on the icosphere, which is
why the worlds do not move.

## The projection, which the specification got wrong in the direction of a win

A cube-sphere's naive construction normalizes a point on the cube to the unit
sphere. It is one line, it is obvious, and it distorts: the ratio of largest to
smallest cell area is **5.2x**, converging analytically to `3√3 ≈ 5.196`. The
icosphere it would have replaced measures 1.5–2x.

The campaign's own specification named that projection while claiming an
improvement in distortion, and would have shipped a mesh measurably worse than
the one it replaced. The error is instructive because it was not a slip in
arithmetic — it was an inference from the *addressing* change to a *geometric*
consequence, made without evaluating the function. Running it takes seconds and
settles it; reasoning about it produces a confident wrong answer.

The fix is the standard remedy, a tangent warp applied to each face coordinate
before projection:

```text
a' = tan(a · π/4)
```

which trades the naive scheme's 5.2x for **1.41x** globally. The number that
actually matters to a player is local rather than global, and it is much
better: adjacent cells differ in area by 1.024x at depth 6, 1.003x at depth 9,
falling as `1/N`, which extrapolates to roughly **1.0002x** at the walk band.
Standing anywhere in the world, the ground under your feet is square to two
parts in ten thousand.

One detail earned its own special case. `tan(π/4)` in double precision returns
one unit in the last place below 1.0, so a face edge computed through the warp
lands a hair inside the cube rather than on it, and two faces disagree about
who owns the seam by 1.11e-16. That is not a precision preference in a project
whose contract is byte-identity: `warp(±1.0)` returns exactly `±1.0`, and the
seams are watertight by construction.

## Eight directions, and what a diagonal costs

The lattice is eight-connected, with the four edge-neighbours always first in a
pinned order. That ordering is a save-format contract, and roughly thirty call
sites index into the prefix; it is now asserted rather than assumed.

A diagonal has to cost more than an orthogonal step or the shortest path
between two points becomes a staircase. The exact cost is `√2`, which is not
available to an integer metric, so the question is which rational to use. The
continued-fraction convergents of `√2` are 1, 3/2, 7/5, 17/12, 41/29; the
campaign preregistered a tolerance of 0.5% and **17/12** is the smallest
convergent inside it, at +0.173%.

Measuring the lattice rather than trusting the ideal is what made that number
honest. On the real mesh the diagonal is **1.411786** edges, not 1.414214,
because a tangent-warped cube-sphere is not a perfect square lattice. So the
zigzag exploit an unweighted diagonal would have permitted was 41.18%, and the
error of 17/12 is measured against the mesh's own diagonal rather than against
Pythagoras. A side effect worth recording: `gcd(12, 17) = 1`, so the octile
metric *removed* cost ties from the pathfinder rather than creating them.

## The corner rule

Eight-way movement raises the question every roguelike answers somehow: may you
cut a corner? The dislike of sneaky diagonals through walls is real, and so is
the absurdity of an open field you cannot cross diagonally.

The rule is that a diagonal is refused only when **both** flanking orthogonals
are impassable — one open flank permits it. This is not a compromise between
two tastes; it is the geometric statement of when the diagonal passes through
solid matter. Both flanks blocked is a wall corner. One flank open is a doorway
taken cornerwise. The check reads the flanks and never the destination, which is
what keeps it from becoming a second, quieter passability rule.

Underground, tunnels stay orthogonal-only, and this is a positive claim rather
than a conservative default: a tunnel is a *carved* passage, and a diagonal
tunnel would require two carved cells meeting at a corner with no shared face —
not a passage a digger could make. The surface band's diagonals are open ground,
which is a different thing.

## The world did not move

The whole change is to the addressing of where you stand, and the evidence is a
byte comparison rather than an argument: a seed-42 world built on `main` and one
built on this branch are byte-identical, `sha256 e70ca3d0…`, 21,635 facts on
both sides, reproduced independently twice. A generated world carries 118
predicates and not one of them is address-carrying. The genesis layer never
asked a room who its neighbours were, which is the same fact that made decision
0141's premise false, seen from the other side.

## Two findings worth more than the feature

**A stream draw keyed on an array's length.** The chamber growth routine rotated
its search order by a seeded draw, modulo `HEADINGS.len()`. The array was about
to grow from four entries to eight. Widening it would have silently changed
every generated world for the same seed, and no guard in the project could have
seen it: determinism tests compare a seed against itself, never against history.
The modulus is now an explicit constant with the orthogonal count's own name.
The general shape is the one decision 0102 exists for — a draw keyed on a
generation-time incidental rather than a fixed lattice position — and an array
length is an unusually good disguise for one.

**An epoch is a measuring instrument.** Five tests in this campaign turned out
to be resting on distinctions the triangular mesh did not have. Thirty-one
placed rooms happened to equal thirty-one cells. Three distinct chart symmetries
coincide on a triangular diamond. Eight cube corners and twenty-four
rooms-at-corners are different quantities, and the campaign's own plan collapsed
them. A parameter cancelled out of a sweep entirely. And a water-width control
asserted on the width of the reach it was transecting, while the gate it was
testing prices the width of whichever channel each of the two rooms
independently wins — six distinct channels in one eight-neighbourhood, so a
single grid vertex was never an identity for "the water" at all.

None of these is findable by mutation, because nothing was wrong yet. A test
naming one of two coincident quantities silently asserts both, and stays green
for as long as they coincide. Changing the representation pulls them apart. That
makes a geometry change, a refactor, or a units migration the cheapest available
audit of what a suite actually pins — and its failures should be budgeted as
findings rather than as breakage. In this campaign the majority of them were
tests passing for the wrong reason, and one was a live gameplay defect wearing a
green suite.

## The compass, which this campaign broke and then fixed

Eight real edges do not by themselves give you eight honest compass words. On a
cube-sphere the local grid is rotated relative to true north almost everywhere —
that is what it means to wrap a sphere in six squares — so at each room the
eight neighbours have to be matched to the eight names, and the matching is a
choice.

The choice made here was a greedy one: score every (word, neighbour) pair by how
far the word's nominal bearing sits from the neighbour's true bearing, sort, and
accept each pair whose word and neighbour are both still unclaimed. It is the
obvious rule, it is cheap, and it guarantees exactly one property —
**cardinality**. Every neighbour receives exactly one word.

It bounds nothing. Each acceptance consumes a word *and* a neighbour, so the
final pair is forced: whatever word is left is stapled to whatever neighbour is
left, however absurd. At one seed-42 room the result was this:

```text
go N    off by   0.0°      go S    off by  19.4°
go Ne   off by   5.5°      go Sw   off by  21.1°
go Se   off by  21.1°      go W    off by  22.7°
go Nw   off by  14.7°      go E    off by 156.1°
```

Seven words excellent, one pointing nearly backwards. `go e` walked west, and
`look` reported `E` as open, so the prose and the movement agreed on the same
falsehood. That is precisely the "one-turn observable falsehood" decision 0141
existed to prevent, reintroduced by the decision that supersedes it — and the
distribution is why it survived: a rule that is wrong everywhere gets noticed,
while a rule that is perfect seven times out of eight reads as correct.

Measured over 12,696 rooms: 5.96% carried a word more than 45° off, 0.82% more
than 90°, worst case 156.1°. It concentrated at the face seams, 17.8% of the
outermost lattice ring.

**No test in the workspace could see it, and that is the finding rather than the
bug.** Every assertion about the rose checked that each neighbour received
exactly one word — the one thing greedy always achieves. Nothing anywhere
measured an angle. The campaign had replaced a 45°-bucket rule that was wrong by
at most 22.5° by construction but sometimes emitted a duplicated letter; so it
traded a *visible* inconsistency for an *invisible* one and measured only the
first.

The repair is to stop deciding pair by pair and solve the whole eight-by-eight
assignment at once, which is a textbook problem with a cheap answer: 11.6 µs per
call against greedy's 5.9. Worst error over the same populations falls from
**156.5° to 34.6°**, and no room anywhere carries a word more than 45° off —
so every word now names the correct octant, which greedy could not promise.

**One axis got worse, and it is inherent rather than accidental.** Steps that do
not invert — where `go e` followed by `go w` fails to return you — rose from
0.67% to 1.55% of sampled pairs. A per-room rule cannot avoid this: each room
assigns its words without consulting its neighbour's assignment, and where the
rose rotates between two adjacent rooms the reverse of *east from here* is
simply not *west from there*. Perfect invertibility and bounded per-room
accuracy cannot both hold on a curved surface under a local rule. Given the
choice, a step that lands one room from where you expected is a smaller lie than
a word that points 156° wrong, and 184 of the 190 failures are off by exactly
one word. Both numbers are now pinned two-sided, so an improvement has to be
banked rather than absorbed in silence.

## What the rhumb leaves behind

`course.rs` is deleted, and its suite is reported **dissolved** rather than
passing. Those tests measured drift from an ideal rhumb line. There is no longer
an approximation to drift from, so they do not have a result; they have a
retired subject. The distinction matters because a dissolved test quietly
converted into a passing one is how a project forgets which guarantees it
actually holds.
