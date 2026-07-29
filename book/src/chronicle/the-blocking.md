# The Blocking

The tutorial everyone learns from runs the wrong way.

Every roguelike text teaches the same chapter: partition a rectangle, call the
partitions rooms, then walk back through them scattering monsters and treasure.
Map first, contents after. Hornvale has the opposite problem, and had it before
this campaign started. The contents already exist. A structure's chambers, what
each one holds, which ones connect — all of that is derived from committed
history, and it existed for two campaigns before anything could draw it. What was
missing was the map.

So this campaign runs the chapter backwards, and the reversal is not a
cleverness. It changes what the code is *for*, and therefore how you are allowed
to judge it. **A generator is judged by variety; an embedder is judged by
fidelity.** A dungeon generator that produces the same four rooms every time has
failed at its job. A floor-plan embedder that produces a different plan for the
same house has failed at *its* job, in the opposite direction. The literature for
the thing we actually need is rectangular duals and orthogonal graph drawing, not
dungeon generation, and the goal there is stated as a constraint: given an
adjacency graph, produce a subdivision whose regions have exactly those
adjacencies. Exactly those. Not more.

## The number that makes fidelity checkable

"It may add no information beyond what the graph leaves free" is the kind of
discipline that sounds rigorous and decays into a slogan, because nobody can say
whether it held. This campaign made it a number.

The embedder reports its **residual degrees of freedom** — how many independent
choices it actually made — and the checker compares that against how much freedom
the specified graph leaves. For the rectilinear method a chain of *n* chambers
leaves *n*−1 cut positions free, and the embedder reports 0, 1, 2, 3 at one, two,
three and four chambers. For the grower it is two draws per chamber, and the
embedder reports exactly 2*n*. Not "within budget". **Exact**, at every chamber
count, over two thousand seeds. A budget that merely bounds the number would have
passed a solver quietly inventing less than its ceiling; an equality catches
invention in both directions, and being *under* the budget is a finding too,
because it means the seed is not filling freedom the graph genuinely left.

The rule passed on its first run and was never touched again. That is worth
stating plainly, because it is unusual here and it is the point: the discipline
was designed to be measurable before it was implemented, so the implementation had
nowhere to hide.

Two draws at one chamber are spent and ineffective — a single blob floods the whole
extent whatever cell it starts from — and the rule counts *draws*, not *effects*.
That gap is recorded rather than papered over. Narrowing the budget to fit the
observation would have meant re-cutting a measurement to match its result, which is
the move this campaign refused three separate times.

## A wall is a cell

Halfway through, the owner overruled the model.

The design had a wall as a **non-adjacency**: every cell is floor, and a wall is a
property of the *boundary* between two cells — an edge you may not cross. That is
tidy, it is what the graph literature assumes, and it produced a render nobody
could read. A 1:1 picture of such a lattice draws no walls at all, ever, because a
boundary has nowhere to live on a grid of cells: every cell is floor, so every
glyph is floor. The picture had to double to twice-plus-one on each axis, odd
positions cells and even positions the gaps between them, and the campaign was
about to inherit that coordinate mapping and its whole off-by-one family.

The replacement is one sentence. **A wall occupies a cell.** The render is 1:1.

Two of the arguments for it were the owner's: fixed-width top-down maps want 1:1,
and a graphical tilemap draws a wall as a *tile* — often a wider one, in 2.5D — so
the cell model is the conventional one everywhere this is heading. The concession
he offered along with it turns out not to be a concession at all. A cell here is
about a metre, since a chamber is roughly eight cells across, and this world models
neolithic through classical building: turf, cob and rubble-stone walls genuinely
run half a metre to two metres thick, and Icelandic turf walls reached two. A wall
with thickness is *more* faithful, not less, and it is checkable against the
technology horizons the brief already carries.

The argument neither of us had made arrived from negating the premise. If a wall has
no extent, it can carry nothing. If it has extent, **it can carry a payload** — and
two anchor kinds that shipped campaigns ago had been standing nowhere. A screen
"affords nothing, shapes sightlines": that is a partition, which is a wall. An
alcove is "a recess off the main space": that is literally a passable wall cell. And
the pattern for a fire attaches *within* an alcove, which means the world has been
describing a **fireplace** since The Hearth, with no geometry to make it legible.
The reification did not need a new pattern to justify itself. It made an existing
composition visible, which is the shape good news takes in this project.

It also paid two debts and retired a third. The doubled render's coordinate mapping
is gone. The next campaign gets *blocking cells*, which is what its already-measured
shadowcast timings assumed. And because a wall cell costs real space, the extent
became `cols × side + (cols + 1)` per axis — ten by ten at one chamber, nineteen by
nineteen at four — so every chamber count fits an eighty-column terminal and a
height relaxation granted a task earlier became moot.

The tidiest-sounding cost was overstated and got corrected: the claim that the
totality invariant is lost. It is not lost, it is re-founded one type up. A total
map from cell to *owner* becomes a total map from cell to *kind* — equally total,
strictly more informative — and the arithmetic identity it replaced (region areas
summing to the extent's area) was satisfiable by overlapping rectangles that happen
to total correctly, which is why a second quadratic overlap check had to sit beside
it. A map keyed by cell cannot double-count. The change **deleted** a test.

The real cost is narrower and sharper, and it is where this could have gone wrong
quietly: if walls were simply *absent* from the ownership map, "no owner" would mean
"outside the plan **or** a wall" — two facts in one value, which is precisely the
defect class that had already bitten once. An explicit cell kind is what keeps
absence meaning one thing and wall-ness a positive fact.

And the vocabulary is closed at three kinds — floor, wall, threshold — on purpose.
If the substance of this campaign turned out to be *how many* cell kinds exist, the
lattice would be a tile catalogue, and the pattern-language discipline would have
been violated one band below where it was written. A window is an anchor standing at
a wall cell, never a kind of cell. The only kinds that may ever join are states a
cell *transitions into*: rubble, and barred.

## The eighth rule, earned rather than found

The parent program listed seven checker rules. This model earns an eighth, and it is
not a bonus — it is the price of the reification, paid out loud.

Under the boundary model, connectivity was guaranteed by construction: regions tiled
the plan and doorways linked them, so there was nowhere for a walker to be stranded.
Walls as cells **can seal a pocket of floor**, and a sealed pocket is a room a player
can see on the plan and never enter. So the grower now claims a cell only when no
differently-owned neighbour touches it and never takes a claimed cell back, which
makes reachability hold by argument; and rule 8 is what makes the argument
falsifiable.

Two of the seven also changed form, and both got stronger. The wall law stopped being
a claim about a separately-derived set of cell pairs and became a claim about the
world: two floor cells of different chambers are never adjacent. And closure stopped
being tautological. Under the boundary model it asserted the contrapositive of the
wall derivation's own exemption condition, read back off the same ownership map — it
could not fail, and it passed without ever being able to. It now asserts that the
plan's **outer ring is entirely wall**: the building is enclosed, which an embedder
could genuinely fail to do and which the boundary model had nothing to say about.

A rule that a type already guarantees is not a check. It is documentation wearing an
assertion's clothes, and this campaign wrote two of them before noticing the pattern.

## The plan is as big as what outlives its tenants

How big is a house? The tempting answer reads the brief: a regional seat should get a
grander plan than a waypoint. It is the wrong answer, and the reason generalizes past
floor plans.

`notability` describes the **alive** occupation — how prominent this place is *now*,
with people in it. A building's shell outlives its occupants; masonry is the most
persistent thing about a place. Derive floor area from a living fact and a building
**shrinks when its people leave**. Nothing built stands at a ruin today, so the shrink
is currently unreachable, which is exactly why the rule is written down while it is
still free rather than after the first ruin has a footprint.

So the extent is a pure function of **chamber count**: the plan is as big as the rooms
it must hold, plus the fabric between them. It reads no brief field and spends no draw,
which is what keeps the freedom count equal to the cut positions alone — the coarse
constraint on the fine layer is a *constraint*, not a die roll.

Grandeur did not vanish; it moved to where it belongs. A hall is grand because it holds
a high seat. Expressing that as floor area instead would be the catalogue reading of the
same fact.

The richer version has a home and a condition. Its most interesting form is technology
as a **material span cap** — a pre-industrial room is as wide as a roof beam can span,
so technology should *bound* a chamber's width rather than reward it — and it belongs
with the ruin signature the brief omits on purpose, because a durable extent and a span
cap are the same question about what a shell remembers. Captured, not built.

## Differentiation is redistribution, not accumulation

The previous campaign's headline was that a player can walk between a structure's
chambers, and its own chronicle admitted the result was thin: four doors onto one room.
Both chambers of the seed-42 dwelling read *a small room, holding a doorway, an alcove, a
water jar and a screen* — the same sentence, twice, because the composer took two
booleans and no address.

They now read differently:

```text
> enter
[chamber 193703028372802, day 0]
A small room, holding a doorway and a screen.
Ways on: out, further in.
> enter further in
[chamber 193703027969442, day 0]
A small room, holding a doorway and an alcove.
Ways on: out.
```

**Neither chamber gained anything.** Look again: between them they hold a doorway, a
screen and an alcove, where before each held all four things. The water jar is gone from
both. Differentiation here is **redistribution**, not accumulation — a chamber takes a
*role*, and a role admits a subset. The threshold chamber is the one you walk into and
it holds a screen. The hearthroom holds the alcove. That is the whole mechanism, and its
substance is which roles a brief admits and which patterns complete which, not how long a
list of patterns exists.

The best evidence that this is a grammar and not a table is where the fire burns. No role
withholds it. The fire is admitted by every role — and it still appears in exactly one
room, because it *requires* an alcove and only the hearthroom admits one. Nobody wrote the
rule "fires belong in hearthrooms". It falls out of two independent facts, and the bed by
the fire is confined one link further along the same chain: a fireside bed needs a fire,
which needs an alcove, which one role admits. In this transcript no fire burns at all,
because the house sits in tropical seasonal forest and warmth is not a problem there. The
grammar decides *where*; the world decides *whether*.

And a fire within an alcove, now that a wall is a cell, is a recess in a wall with a fire
in it. The word for that is a fireplace. It has been the composition all along.

## The floor you can read, and walk

```text
> map
[plan: chamber 193703027969442, 2 of 2]
###################
#..........#......#
#..........#......#
#..........#......#
#..........#......#
#..........+@.....#
#..........#......#
#..........#......#
#..........#......#
###################
  legend: . the floor, # a wall, + a doorway, @ you
```

Nineteen by ten, two chambers of one grid, one dividing wall, one doorway carved into it
at the cell the link is realized through, and the mark standing beside the doorway rather
than on it — because standing *on* it would overdraw the glyph and a plan that hides its
own doorway is a lie the parity test exists to catch.

Every noun in that legend answers to `examine`. Every destination it depicts is reachable
by a command you could have typed. That is not a courtesy; it is the campaign's
contract, and it is the half of the accessibility requirement that can be *tested*
today with no client in existence. The other half is structural and is the one that
actually holds: a pane input **synthesizes a command**. An arrow key emits `go n` and the
existing verb runs, so parity cannot drift, because there is one implementation. The cost
is accepted deliberately and permanently — any future pane capability must first be a
verb. Nothing expressible only by pointing, ever. The screen-reader sibling's hardest
lesson is that retrofitted parity fails while deriving both channels from one model
works, which makes accessibility here an *architecture* rather than a feature.

## `go north`, indoors, and why that is not a flip-flop

The Lintel made compass movement refuse indoors and corrected four documents to say so.
This campaign makes `go n` mean one cell north.

That reads like a reversal because it is one, and the history should not be smoothed. The
refusal was **correct** for a chamber with no interior: chamber addresses are identity, not
shape, so *north* between two chambers is a question the model could not answer and was
right not to pretend to. This campaign creates the thing the refusal was about the absence
of. What changed is the *inference*, not the law: the band law says lateral movement never
changes band, and a cell step stays inside the chamber band, so the law is untouched. And
`back` stays refused indoors, because it retraces a walk-band trail whatever the interior
looks like.

The alternative — a second verb, `step n`, preserving `go`'s newer meaning — was rejected
on parity grounds. Two movement vocabularies for one action is worse for a player and
worse for a pane, since an arrow key should emit the verb a player would actually type.

## The epoch that did not happen

The approved risk package for this campaign **led** with byte-identity breaking. The
health battery would become the gate rather than a check, for the first time in this
program; a census re-pin was likely and would need authorization. That was the largest
thing about the plan.

It did not happen, and the plan is why. The epoch was written down as a *prediction* with
three named outcomes and a measurement, rather than as an assumption — and the measurement
came back **re-pin**. Exactly one committed file moved: the possession transcript. No
metric golden moved, no census golden moved, and `make gate` came back green as a
**check**. `room/furnishing` stays at v1.

Three facts, each found by reading source rather than reasoning about it, explain the
gap between the prediction and the result. The furnishing label had exactly one occurrence
in the whole workspace — its own declaration — so nothing drew from it and a bump would
re-mint nothing. The band a creature stands in for warmth is the *locale*, not the
chamber, so the chamber composer is read only by the chamber renderer, which commits
nothing. And the pattern selector iterates its inventory in order and filters, so
*appending* role-gated patterns leaves every existing selection byte-identical.

Declining the bump is the substance of this, and it needed vocabulary the spec had blurred
into one word. Negating each defining property of "epoch" in turn produced five distinct
things:

- a **re-pin** — transcripts move, no metric golden moves; not an epoch;
- an **epoch** — a metric or census golden moves; the battery becomes the gate;
- an **empty** epoch — a bump on a label with no draw site, which is exactly what
  `room/furnishing/v2` would have been here;
- a **latent** epoch — an inventory grown behind closed gates;
- an **undeclared** epoch — a derivation that moved with no bump, and the only one of the
  five that is simply wrong.

An empty epoch is not free caution. It declares a discontinuity that did not occur, at the
cost of a permanent manifest row, and it poisons the baseline that the first *genuine* bump
will diff against. Saying the bytes stopped being reproducible when they did not is the same
defect as failing to say so when they did, pointed the other way.

**The latent condition holds at the same time, and separately.** Five patterns were appended
behind a flag that no live composition opens, so no live read reaches them. The
discontinuity is **deferred, not absent**, and a deferred epoch nobody writes down is an
undeclared one — so the gate that opens it is now written down. It is the first mark
committed *inside* a chamber. Today the session's committed facts carry no place at all; on
the day one carries a chamber address, a chamber's composition becomes an input to committed
history and all five patterns become an epoch retroactively. The pattern inventory's own doc
comment used to say flatly that appending a pattern *is* an epoch. It was over-strict the
moment role gating landed, and an over-strict warning is one that gets ignored, which is
precisely how an undeclared epoch ships.

Stated plainly because the polarity is easy to get backwards: an epoch is not only a cost. It
is the one mechanism by which a world frozen by its own goldens is allowed to improve.
Avoiding one is not automatically the win — and this campaign avoided one only by finding
that there was nothing to declare.

## A world records what it was derived under

The parent program asked for the player-facing consequence of an epoch to be *stated rather
than discovered*: history survives, but remembered places rearrange. Nothing stated it,
because nothing recorded which epoch a world was made under. Now a saved world carries a
stamp.

The stamp is not a counter. It is the set of **versioned labels and their versions**, ten
rows for a seed-42 world, keyed on the label with its version segment stripped — so a bump
is a *value* change on a stable key, which is what lets a reload name what moved instead of
reporting one key vanishing and another appearing. Unversioned labels are correctly absent:
unversioned means structural means can never differ.

Two things about it are worth keeping. It lives on the world and is written by the one place
that can see every crate's labels, rather than being committed as a fact at genesis — a
stamp entity at genesis would shift every entity id minted after it and move every artifact
in the project that mentions one, a byte-identity break far larger than the one this campaign
was braced for, taken for metadata.

And it nearly shipped lying. The manifest roster deliberately keeps **retired** labels as
rows, marked only in their prose, and one retired language label is listed *after* its live
successor. Both strip to the same key, so a last-wins insert would have stamped the retired
version — in every world, silently, with the mechanism built to make epochs honest lying from
its first commit and the lie surfacing at the exact moment it was first needed. The fix is
that the highest version for a key wins, compared numerically, which is sound precisely
because epoch suffixes bump and never rename. The cause of the defect is worth more than the
fix: the stamp was designed by reading the manifest *renderer*, and a renderer is right to
show retired rows. A data source was inferred from a presentation layer without asking whether
every row it displays is live.

## What is safe to say afterwards

A structure now has a floor you can see and walk across, one metre at a time, and its
chambers are no longer four doors onto one room. What it does not have is sight: nothing
occludes anything, and the plan shows the whole structure rather than what you could see
from where you stand. That is the next campaign, and it inherits blocking cells rather than
blocked edges, which is the whole reason the model changed when it did.

The eight rules hold over generated structures. The residual freedom is exact. The plan is
enclosed, its floor is reachable, and its doorways and thresholds name each other in both
directions. And the largest risk the campaign was approved on turned out not to be a risk at
all — which is only knowable because the plan was written to measure it instead of to
survive it.
