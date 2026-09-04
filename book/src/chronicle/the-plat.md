# The Plat

*A plat is the surveyor's drawing of a parcel: not the ground, and not a
picture of the ground, but the ground divided and each piece named for what it
is for. Nothing about the land changes when a plat is drawn over it. What
changes is that the land can now be talked about.*

Two campaigns built the underworld's skeleton and the first of its two
readings. The first grew a series-parallel graph of regions under every cave
system — a plan, whole before a single level was carved — and exported, on
every region, how many hops it sat from the entrance and which cycle owned it.
The second stamped locks and keys and one-way drops onto that graph and read
the hop count exactly once, to decide which end of a loop an intruder should
be sent around. Everything else it exported went unread. The graph had a
puzzle on it and nobody in it.

This campaign is the second reading, and the whole of it follows from one
sentence: **use follows form.** A people that settles a cave does not design
the cave. The rock drew the chambers and the grammar drew the ways between
them; the people walks in and assigns use — the shallowest room is the door,
the room every path passes near is the hall, the deepest room is theirs alone.
That assignment is a *function of the plan*. It adds no draw, consumes no
stream, and rewrites no edge, so not one byte of any plan moves; and it is a
reading, so the same plan can carry it and the intruder's puzzle at once
without either knowing about the other.

## The reading, and the one place it needs a rule rather than a measurement

Per level, the plan already carries a depth on every node. Two of the three
roles fall straight out of it. The **entry** is the shallowest node — the
transition between outside and inside, and the node a possession actually
arrives at, being the plan's mouth on the top level and the foot of the
spine's stair on every level below. The **sanctum** is the deepest, and it is
never the entry: over six thousand levels the depth range within a level was
at least two on every one of them.

The **heart** is the interesting one, and the first draft of it was wrong.

The obvious reading of "the common hall" is the hub — the most-connected room.
Measured before anything was designed, over twelve hundred plans, that reading
does not discriminate: *every* level has a node of degree three or more, and
the typical level has three to six of them. "The heart is the hub" names most
of the floor. Alexander's own criterion for the pattern is a different and
much sharper one — the common areas belong "at the center of gravity of all
the spaces the group occupies" — and a center of gravity on a graph is an
exact object: the **median**, the node minimizing the summed distance to every
other node.

That is the rule the reading uses, with one exclusion stated as a rule rather
than discovered as a tendency. Without it the median lands on the arrival node
on between a fifth and a third of levels, because the entry is where the
spine comes in and the spine is what everything hangs off. Alexander is
explicit that there is always a transition between the outside and the heart,
so the entry is struck from the candidates by fiat, and the heart is the
median of what remains. It is unique on 71–79% of levels; on the rest a
two-way tie is broken by degree, then by shallowness, then by identifier. It
is absent — a level with an entry and a sanctum and no hall — where the median
*is* the sanctum, which happens on a tenth of a percent of levels overall and
on 6.9% of the small levels a lava tube produces.

A fourth thing is read off the stairs rather than the rooms. A node that is
the lower end of one stairway and the upper end of another is a **landing**:
you come down into it and you leave it downward, and Alexander's staircase-as-
a-stage is a property of exactly that node. The previous campaign had removed
a whole pattern row that wanted a "landing hall", because the *class of loop*
it selected on never occurs. The landing is not a property of a loop. It is a
property of a node, and it occurs on between 14% and 45% of levels depending
on the rock.

## A floor is usually in pieces, and that is what decided the metric

The first heart metric summed distances *within a level*. It survived a
measurement — the numbers it produced were unremarkable — and it was wrong,
and what caught it was a picture.

Drawing one real level, seed 0's second cave system at its second rung, put a
four-node island on the screen: four regions with ways between them and no way
at all to the other eleven regions of that floor, reachable only by climbing a
stair to the level above and coming back down a different one. Under a
within-level metric that island's own middle node has a summed distance of
four, and every node on the connected mainland has a summed distance far
larger. The island wins. The metric would have crowned, as the common hall of
a twelve-room floor, a cul-de-sac four rooms across.

The predecessor had in fact disclosed the underlying fact — it noted that a
level is no longer guaranteed to be connected within itself, and treated that
as the design working rather than failing, which it is. What nobody had done
was print how often. Across the panel's plans a floor is one connected piece
only 23% of the time in karst and 33% in fracture; 39% of karst floors are in
*three* pieces. Only lava tube, whose levels are small, is usually whole, and
even there it is 58%.

So the metric is the plan's, not the level's: shortest paths through the whole
descent, stairs included, summed over the level's own nodes. A resident
walking from one wing to another does climb the stair and come back down, and
that distance is real. Under the corrected metric the heart is the arrival
node on 0% of levels rather than 22–29%, and it sits in the shallower half of
its level's depth range on about three quarters of them.

The general form is worth stating, because the histogram that missed this was
not a bad histogram: **a summary statistic over a population cannot show you
that the population's members have the wrong shape.** One drawing of one real
member did.

## Made comes from the ledger, at the moment someone walks in

The reading names rooms in any cave. It is *narrated* only where a people cut
the place, and until this campaign no walked cave was ever cut.

The material was all there. A chamber has an origin — found or made — and
there was a function that computed it from a world's history, whose own
documentation opened with the sentence "nothing in the shipped path calls
this, and that is the whole disclosure." Every caller handed the walk an empty
map instead, so every rung of every descent was found rock, no descent was
worked, and therefore no door could be admissible anywhere a player could
stand. The previous campaign's panel reported doors on 872 of 874 worked
descents and **zero** on the descents the walk actually builds. Both numbers
were correct.

The repair is that a column's origins are derived from the committed ledger at
the moment the possession enters it. For the vertex being descended, the
ledger's occupation records are read, each occupying people's environment
niche is seated against the same three inputs the bake uses — the cave, the
geothermal gradient, the water table — and the rung it seats at is marked
made. Every other rung is found rock, which is exactly today's answer, so a
column where nobody settled is entered byte for byte as it was.

There are now two writers of the same fact at two grains: the bake-side one
over a whole world's history, which the capacity probe reads, and the
walk-side one over a single column, which a verb can afford. They are pinned
to agree — on three seeds, the set of `(vertex, rung)` pairs one marks equals
the set the other marks — because a duplicated derivation that nothing
compares is a divergence with a date on it rather than a possibility.

On the three panel seeds the ledger holds 26, 3 and 5 settled underworld
columns, and every one of them is seated at the top or second habitation rung.
That is the fact that makes a single descent sufficient: a possession that can
walk down two rungs can reach every cut place a world has.

## Workmanship joins the plan per level, and this is not an epoch

The grammar's cycle budget already asks whether a descent is worked; it was
answered by the descent's *character*, which the walk hardcodes to a wild
cave. It now asks per level, and the answer is the clause the density decision
reserved two campaigns ago in as many words: a level is worked if the
character is worked **or** its origin is made. The gate pass reads the same
term — a key row is admissible for a loop only if the level its lock would sit
on is worked, because a door needs a maker where the door is, and for the one
loop that spans two floors that is the upper one.

That is an input the grammar reads, not a change to the grammar, and the
difference matters because a descent key's identity is its position in the
plan, which makes the plan a save-format contract: a saved world names a key
by where the plan put it, so moving the plan invalidates the world. The
distinction is defensible and it is not self-evident, so it was argued rather
than assumed. No draw is added and no selection rule changes. The one input
that changed had exactly one reachable value before this campaign — found,
everywhere — and for that value the output is pinned byte-identical over the
nine hundred plans a digest was taken across before any grammar file was
touched. A plan at a column with a made rung is a plan that *no saved world
has ever held*, because the walk could not produce one. It is a new
population, not a moved one.

## A cut place outlives its people

The reading came within one boolean of being keyed to whether anyone still
lives there, and the better answer was to key it to whether anyone ever did.

The ledger's occupation record already carries an end. A column is made if a
people cut it, whether or not that people is still there; whether it *is*
still there decides only the tense. So the vocabulary comes in pairs, and the
past-tense half was free:

> This is the entry of a cut place; the rock is squared where it was worked.
>
> This was the entry of a cut place, long empty; the squared rock has dulled.

Eight sentences over four roles and two tenses, plus one clause a landing adds
in either tense — a stair comes down into it and another leaves it. Every noun
any of them names — entry, hall, chamber, stair — answers `examine` with the
same sentence, which is the standing rule that a thing you can be told about
is a thing you can look at. The sentence is *appended* to what a room already
says rather than woven into it, so a wild rung reads byte for byte as it did
before, and that is asserted rather than hoped.

The ruin is the reading's own argument for itself in miniature. Nothing was
built for it. It is the same rooms, the same roles, the same graph, read
against a field the ledger was already keeping.

## The hoarder moves from a corner of the grid to the end of the plan

Something already lived in the deep rungs. A previous campaign derives a
rung's dominant inhabitant from its own substrate and energy budget — who it
is and whether there is one at all are the energy field's answer, drawn by
nobody — and then placed it at "the last standable cell in ascending order",
a rule chosen so that the creature would not be standing at the stairs when
you arrived. That is a rule about the grid, not about the place.

It now stands in the sanctum's region: the first standable cell of the room
the plan says is innermost, which is the mirror of the rule that stands the
possession on the first standable cell of the entry's region. Who and whether
did not move an inch. Only where, and where is now a structural derivation
rather than an artifact of iteration order — which is precisely the condition
the design had set for a creature to count as having fallen out of the world
rather than having been placed in it.

It sits on whatever lies there. The things whose location fact names the
sanctum's region are folded into the datum the mark carries, listed the way a
floor is listed: *a xorn moves in the dark here, drawn to iron-bearing stone,
sitting on: a key.* No new kind of thing, no new predicate, no identity — a
fold over facts that already existed. In a cut place the same creature is
narrated as **kept** rather than as moving in the dark, one word keyed to
tenancy, because a people that cut the place did not leave its innermost
chamber to whatever wandered in.

It holds nothing. A creature that holds a key, in a world with no verb for
taking a key off a creature, makes the only lock in the game unopenable — so
the ceiling on this creature's materiality is not a design preference but a
consequence of a verb that does not exist yet. It sits on the hoard; it does
not own it. And the hoard is usually what the possession itself left there,
because a key the grammar placed at the sanctum is a thing lying there by
construction, and that happens on 13–23% of levels that carry a key.

## Nine verdicts in the frozen words

Three predictions were frozen before the code, each naming the population it
measures **from** and the quantity it measures **to** — the predecessor's own
lesson, whose gate yield read a perfect 1.0 on every seed because its
denominator excluded the population that mattered.

```text
                                                seed 42     seed 7   seed 1234
  heart decile <= 5   (floor 0.6667)        24/26 0.9231  3/3 1.000  4/5 0.8000
  doors on made rungs (floor 0.50)          16/26 0.6154  2/3 0.6667 3/5 0.6000
  nested realms smaller (band 0.50-0.6667)  0.5842        0.5798     0.5709
```

Nine of nine passed, with nothing tuned. The heart sits in the shallow half of
its level on nine of ten made levels, which is the intimacy gradient measured
rather than asserted. Nested circulation realms shrink going in on 57–58% of
nestings, present and weak, as predicted — a majority and under two thirds.

The door share carries the campaign's headline, and it carries it as a
movement rather than a level. Beside each seed's figure the page prints the
same rungs' door share under the plan the walk would have built yesterday, and
that figure is **0 on every seed**: not a small number, a structurally
impossible one. Sixteen of seed 42's twenty-six cut rungs now hang a door, and
the panel prints what the number moved from.

Two audit commands had to start building a world's history rather than only
its terrain to say any of this, since the origins are the ledger's. Timed
after the fact: 32.9 seconds and 11.2 seconds for a seed, each including the
now-heavier build.

## What is deliberately not here, and one thing that is not here by accident

No residents stand in the cut rungs. A people is seated *at* a rung by the
ledger and there is nobody in the room: putting a body there wants a home kind
keyed to a column and a rung, chamber seating into a carved level, and a place
on the turn roster, which is a mechanism campaign rather than a paragraph. The
reading tells you a hall is a hall; it does not put anyone in it.

No patroller. The gate stamp that marks a route dangerous is still realized by
nothing, and the reason it was not realized here is a distinction rather than a
deferral: the creature a dangerous route wants is a creature *in a passage*,
which is a body that moves, and the creature this campaign sites is a sitter
at the end of the plan. They are two creatures, and the second one needs the
underground roster the first one does not.

No carving by role. The heart is not made larger or more worked than its
neighbours, because a region's size is the plan's and the plan is upstream of
the reading; and a made level's leaves already lean worked at a base chance of
0.85 against found rock's 0.10, so the lever would have moved nothing visible.

And the walk still does not read the lattice. It reads the ledger's origin for
the column and it still reads no run, no branch, no junction, and no drawn
character — a descent under a vertex where the lattice says a drow tier was
cut, but where no people ever settled, is still entered as a wild cave. The
debt is narrowed and named, not closed.

**One absence was found rather than chosen.** The past-tense reading is
exercised, and it is exercised through the test seam rather than by walking
to a ruin, because seed 42 has no ruin a possession can reach. Of its 26 cut
columns, 24 are still inhabited and 2 are abandoned — but only 5 of the 26
have a cave mouth that is both open and unbarred, which is the population a
walk can actually enter, and all 5 of those are inhabited. The sentences are
correct and pinned. What is unproven on this world is that a player can ever
*arrive* at one, and the reason is a property of the barrier draw crossed with
the ledger's occupation spans rather than of anything the reading does.

What is here is the second reading the skeleton was chosen to carry: the same
graph, with an intruder's puzzle on it and a resident's house in it, and
neither one knowing the other is there.
