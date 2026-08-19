# The Adit

A chamber is a bucket, not a place. `ChamberAddr` addresses which of up to
four interchangeable habitats exists at a given depth under a given
surface cell — enough for population and capacity math, nothing a player
could stand in. Two campaigns had looked at that gap and deferred it. The
Lantern went looking for something to light underground and found there
was no view to light: *"Standing in a cave chamber, the pane shows a
chart of the country overhead — which is odd."* The Underworld gave every
chamber real conditions — a depth rung, a rock stratum, a water-table
state, a routed temperature, a named community — and closed with the same
sentence unchanged: *"the underworld's own chart is unresolved... the
pane still shows the country overhead."*

This campaign builds the shape the chart needs. A recursive
partition-tree scaffold — BSP-of-BSP, a rectangle that is either one leaf
or split into two children each independently leaf-or-split, bounded by a
minimum region span and a hard depth-2 ceiling — carved by four
content generators (a cellular-automata cave, a drunkard's-walk tunnel,
and one partitioned-rooms carver tuned two ways) keyed to `CaveKind` and
`ChamberOrigin`. Water-table flooding and rung-to-rung stairs layer on
top. Everything is `FRAME`-tier: derived fresh from a seed on every call,
nothing serialized, no epoch, no save-format cost. It does not chart
anything — that is a later Delving campaign's job — but for the first
time, a chamber has a shape to chart.

## Two determinism defects, found before a line of production code existed

The pre-flight scan this project runs before a plan's first task is
touched is supposed to catch exactly this shape of thing, and this
campaign is the case where it did.

The plan's leaf-carving design called `carve(algorithm, rect, seed, cells)`
per leaf, each call deriving its own fresh stream from the top-level seed
and a per-algorithm label. Tracing it against the precedent it was meant
to follow (`lattice::allocate`'s and `lattice::grow`'s own "derive once,
thread `&mut Stream` through every draw" discipline, cited in the plan's
own findings section) surfaced the defect the citation should have
prevented: two leaves sharing an algorithm within one composite level
would derive the *identical* stream from the *identical* seed and draw
*identical* content. Not a rare edge case — the ordinary case for any
level whose partition tree splits at all.

The same trace, one level up, found the same shape in `generate_descent`:
every rung of one descent called `generate_level_with_water` with the same
top-level `seed`, so two rungs under one entrance would restart their own
generation from identical stream state and produce correlated, near-
duplicate levels.

Both were fixed in the plan text itself, before Task 3 or Task 6 were ever
dispatched — `carve` retooled to take an already-derived `&mut Stream`,
threaded once per algorithm family across every leaf that draws it;
`generate_descent` given its own `UNDERWORLD_LEVEL_DESCENT` stream,
drawing a fresh per-rung `Seed` rather than reusing the descent's own. Both
fixes carry regression tests that assert the failure mode directly — two
carves sharing one advancing stream must differ; two rungs with identical
extent, origin, kind and depth must still differ — rather than trusting
the restructuring by inspection.

## The scaffold left a wall where the spec promised a seam

`region::cut`, the partition-tree's own splitting function, leaves a
permanent one-cell wall gap between a `Split` node's two children. Nothing
in the original design carved through it. A composite level — any level
whose tree splits at all — was therefore geometrically **disconnected**:
two regions occupying one level, walled off from each other, contradicting
the campaign's own keystone framing of what a composite level is supposed
to be — "two regions stitched together... at a visible seam."

This was found reviewing the task that finally tried to test connectivity
directly (Task 8's integration test), not by any of the seven task reviews
that came before it, each of which correctly verified its own task against
its own brief and had no reason to suspect the brief's premise. The fix —
Task 9, added mid-execution — is a post-order walk of the partition tree:
recurse into both children of a `Split` first, so each side is already
internally connected by the time its parent joins them, then connect the
nearest pair of walkable cells across the two sides. One nearest-pair
corridor per split, and the whole tree is connected by induction.

## The algorithm that was never going to pass its own test

Wiring a real connectivity check for Task 8 surfaced a second, deeper gap.
`carve_cellular_cave` — the Karst leaf algorithm, a standard fill-then-
smooth cellular automaton — has no connectivity guarantee of its own. Fed
a fresh grid at 45% fill and run four smoothing passes, it reliably settles
into more than one disconnected cavern. Measured directly against the
shipped algorithm: **5, 7 and 11** disconnected components across three
Karst levels, on a leaf algorithm that had shipped through two prior task
reviews unmodified.

It had shipped unmodified because nothing had exercised it in a
connectivity-sensitive test. Task 9's own 20-seed sweep, added one task
earlier for exactly this property, hardcoded `CaveKind::Fracture` — every
leaf in that sweep was a partitioned-rooms style, connected by
construction via its own room-chaining corridors. `CellularCave` and
`Tunneler` were never in the sample. The instrument that would have caught
this was built and pointed somewhere else.

Task 10 fixed the algorithm — a post-process that flood-fills every
component of a leaf's carved cells and joins the nearest pair, repeated
until one remains, reusing Task 9's own `nearest_pair`/`connect_cells`
directly rather than reimplementing them — and the final whole-branch
review widened Task 9's sweep to all six `CaveKind`x`ChamberOrigin`
combinations, with a positive control asserting the sweep actually
produces composite (multi-leaf) levels rather than passing by never
exercising the property it claims to guard.

## What shipped, and what it is not

Nine tasks, then one added while fixing a review finding, then one more
added while fixing the fix — a plan that grew by 25% during execution,
entirely from tracing the plan's own claims against the code rather than
from anything a task's own review missed. `windows/vessel/src/
underworld_level/` is a new module: `region.rs` (the scaffold, zero
references to caves or chambers — genuinely reusable, per the campaign's
own stated goal of leaving that door open for a later settlement
generator), `carve.rs` (the four leaf styles), and the orchestration in
`mod.rs`. Nothing outside it changed except two lines in `lib.rs`.

It does not chart anything. `the_underground_band_folds_into_walk_as_map_
does`, the test that has pinned the surface-fold for two prior campaigns,
is untouched — the `map` verb still shows the country overhead when you
descend. It does not wire movement — `delve`/`climb` are untouched, and a
possession still teleports to a single point rather than walking a level.
Both are named, in order, as the next two campaigns in the program this
one opens: The Delving.
