# The Deep Realm

The world has an inside now. You can stand on a hillside, find a cave mouth,
and go down into it; the rock around you has a name and a depth; and if you can
go down, so can whatever is already there.

It also has a shape that is entirely borrowed. The underworld's character —
how large a cave system is, how deep it runs, how much there is to find —
turns out to be a re-expression of one terrain field and a fair coin. That is
the campaign's headline, it was measured rather than suspected, and it is the
more useful of the two results.

## A cave asks where, and the answer was a graph

The predecessor question was whether the underground is band-shaped like the
sea. Every ocean cell holds the same five pelagic zones in the same order, so a
diver's depth is a *band* and nothing needs a name. The Stratum ruled on that
and unified the two columns, which required assuming rock behaves the same way.

Rock does not. Most cells have no void at all; the voids that exist are sparse,
scattered, and connected sideways. A band at a coordinate cannot say *three
chambers at odd depths under one cell, none under its neighbour, and a tunnel
between them* — and that sentence is the whole of what an underworld is. Water
keeps its bands. Rock became a graph.

The graph is addressed rather than stored. A chamber is a place in a fixed
lattice — which cell, which entrance, which depth band, which slot — and both
whether it exists and what is in it are pure functions of that address. Nothing
is written down. This is the same trick surface rooms already use, in a sibling
address space that leaves the existing one untouched.

Addressing it that way dissolved the problem the design had named as its one
genuinely hard part. If chamber A derives its neighbours from A's address and B
derives its from B's, the two must agree, or passages become one-way at random
— deterministically, invisibly, until someone tries to walk back. But an edge
between lattice addresses is just *two adjacent addresses that both exist*, and
adjacency over a lattice is symmetric because subtraction is. There is nothing
to keep in sync, and a passage cannot be one-way without adjacency itself being
written asymmetrically.

That symmetry is not a technicality. It is why the underworld is frightening.
Every description here says *descend*, and the structure does not care: if you
can go down, things can come up.

## An address must name a place

The campaign's stated highest risk was that an address might accidentally
encode *the fourth chamber the generator made* rather than *the chamber at this
depth under this entrance*. The difference only shows up later, and then it
shows up everywhere: any change to branching or budget silently relocates every
recorded fact, and each one lands on the wrong room or on none.

The project has met this wall three times before, and the fix's own history is
the warning. Deity names were once derived from an entity id, so a name moved
whenever mint order moved, and cutting that dependency cost a naming epoch. The
function that replaced it carries no entity id at all, deliberately, and it is
the pattern this lattice copies.

The subtler form of the same mistake arrived twice more during the work, both
times wearing a place's clothes.

The measured world only ever produces three of the five depth bands — no cave
is ever a shallow pocket, and none reaches the deepest rock. It is therefore
very natural to index a chamber's depth against *the bands that actually
happen*. That is a construction step: the open question of how depth is
calibrated could change the realized set to four bands or five, and every
address below the insertion would quietly move. Indexed against the permanent
ladder instead, the same change moves only which addresses are *occupied*.

The second form was one level further down. A chamber's derived stream is keyed
on a string, and the first draft spelled the depth band by calling the debug
formatter on it. The intent was right — name the band, never number it — but a
debug implementation is a diagnostic convenience, not a promise. Someone
writing a prettier one to clean up a log would have re-keyed every chamber in
every world, and no test could have seen it. The spelling is now a stated table.

## What was measured

Over thirty worlds — 469,122 land cells, 55,947 caves — the chamber graph
reads:

```
  chambers per cave     median 5, p25 4, p75 7, max 15, mean 5.68
  by depth band, measured against theory:
    Cover     ( 8 addresses)  mean 4.0096  sd 1.4148  cv 0.3529
              Binomial( 8, ½) mean 4.0000  sd 1.4142  cv 0.3536
    Basement  (12 addresses)  mean 5.9955  sd 1.7352  cv 0.2894
              Binomial(12, ½) mean 6.0000  sd 1.7321  cv 0.2887
    Roots     (16 addresses)  mean 8.0047  sd 1.9920  cv 0.2489
              Binomial(16, ½) mean 8.0000  sd 2.0000  cv 0.2500
```

Mean, standard deviation *and* coefficient of variation agree with theory to
three or four decimal places in every band that occurs. The chamber count is
not approximately binomial. It **is** `Binomial(4(rank+1), ½)`, with nothing
else in it.

The consequence is the finding. Given a cave's depth band, its graph carries no
information about where it is. Two caves in the same band have statistically
identical graphs whatever their rock, their climate, their elevation, or their
kind — the lattice never reads what sort of cave it is sitting under. A karst
dissolution cavity and a fault void at the same depth are the same shape.

The preregistered falsification asked whether the underworld could be walked
while *nothing about it differed by place — same depth, same shape, same
contents everywhere*. Read clause by clause, two of the three fail. Depth
differs by place, honestly, out of terrain's own measurement. Shape does not.
Contents do not: a chamber's rock is a function of its band and its origin is
always *found*, because this campaign built the seam for digging and no digger.

So the fixed lattice bought edge symmetry and address stability, and it bought
them at a real price: it contributes no character of its own. Everything the
underworld has, terrain gave it.

## Why that is a useful answer rather than a disappointing one

Because it names its own successor, and the naming is now quantitative.

The predecessor campaign left one defect deliberately unfixed: how deep a cave
runs is still decided by the same number that decides whether the cave exists
at all. Those two questions want opposite calibrations, and the coupling was
recorded as an open problem with no consumer to argue for it. This campaign
declined to fix it first — on the grounds that a field nothing reads cannot be
observed to be wrong, and that calibrating before a consumer exists is
calibrating against a guess.

The measurement is the argument that was missing. The only place-character the
underworld possesses arrives through a three-valued depth budget, and that
budget is welded to the existence gate. Splitting the weld is no longer a
tidiness proposal; it is the one lever with measured leverage.

## The cave you cannot get to

Two numbers sit slightly apart from the rest, and both are about a player
rather than a model.

Roughly **half of all caves are sealed** — their entrance address holds no
chamber, so the void is real and there is no way in. This is not a defect. The
aperture scale reserves its lowest rung for exactly this, on the grounds that a
void nobody can reach must still exist, because it is what a later dig *finds*.
Digging into nothing would be creating rooms out of thin air. But the fraction
had never been *stated*, and stating it changed what the descent verb had to
say: refusing with *"there is no cave here"* at a cell that visibly has one is
not a refusal, it is a bug wearing a message. There are three answers now — no
cave, a sealed one, or a way down.

The second number is starker. Measured from each world's flagship settlement to
the nearest cave one can actually enter: a median of four terrain cells, a
ninetieth percentile of twenty-five, and **seven of thirty worlds where no
enterable cave is reachable from the flagship at all**. An implementer walking
the mesh directly covered sixty-four terrain cells in eight thousand steps and
found none, in a world holding six hundred and twenty-eight.

Nothing was tuned to improve this. The two available levers — how common caves
are, and how tightly they cluster — belong to the terrain model that had just
been calibrated against five preregistered criteria, and moving one to make
this number prettier is precisely the metric-chasing the project forbids. The
underworld is real and, in about a quarter of worlds, out of reach. That is a
statement about the world.

## The xorn was not faking what we thought

The campaign predicted that two creatures were approximating cave-dark with
surface weather, and that scoring them against real subterranean conditions
would collapse their surface suitability. Half of that held.

The rust monster behaves exactly as predicted: its fit underground is roughly
two and a half times its fit on the surface once the approximation is removed.
It has no supernatural potency, so it is genuinely placed by its environment,
and its environment was being described with proxies — a near-zero insolation
optimum standing in for *dark*, a below-sea-level elevation standing in for
*underground*.

The xorn did not move at all: a ratio of 1.02, flat to within noise. The reason
is structural rather than a calibration miss. Its potency buys a large
sovereignty floor and its devotions to every climate axis are deliberately
near-zero, so no curve on any axis can move its fit in either direction. **The
xorn was never faking darkness. It was ignoring climate altogether**, and the
low insolation optimum was decoration on a creature that no environment was
scoring. The prediction was right about the mechanism and wrong about which
creature had it.

## What this leaves

A chamber's content is its own latest recorded difference, else its
address-derived default — and this campaign ships the seam with no writer, so
every chamber is *found* and none is *made*. The rule that a made chamber never
reverts to found is stated and tested, and has no live caller; that is said out
loud in the code rather than left for someone to discover, because the
predecessor campaign's dominant lesson was that a field nothing reads cannot be
seen to be wrong.

Existence, too, is a content lens only. An override can change what a chamber
*is*, never whether it is there. A dwarven hall exists because someone cut it,
which is a different verb than this campaign has, and the campaign that brings
a shovel will need that door opened.
