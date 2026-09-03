# The Brattice

*A brattice, in a mine, is a partition hung across a working to make the air go
the long way round. Nothing about the working changes for the air: the loop is
still a loop. What changes is which way through is open, and to whom.*

The campaign before this one gave the underworld somewhere to go around and
nothing in the way. Every descent became a series-parallel graph of regions,
grown whole before a single level was carved; a cycle could put one of its two
paths a floor below the other; two adjacent regions the plan left unlinked kept
solid rock between them. It exported three attributes and read none of them:
each realm's length class, each region's hop distance from the entrance, and
which cycle owns it. A loop with nothing on it is a corridor you may walk
either way, which is a nice property and not yet a place.

This campaign puts something in the way, and the whole design follows from one
sentence: **the same loop, walked with less.** A resident holds every key and
walks the cycle in either direction. An intruder holds nothing and must go the
long way to fetch what the short way demands. Dormans' lock-and-key cycle is
the difference between those two walks, and everything below is an attempt to
say that difference exactly once.

## A gate is four things, and the design's work is keeping them apart

The temptation is to make a gate a thing. It is not; it is four things that
happen to be in the same place, and every one of them lives somewhere else:

- the **requirement** is on the plan — an attribute of a way, saying what a
  body must hold or be to take it;
- the **place** is in the rock — a cell of the carved level, a squeeze or deep
  water or the lip of a hole;
- the **object** is in the ledger — a door, a thing with an identity and a
  state, which may be absent entirely;
- the **judgment** is the walk's — the moment a particular body tries a
  particular bearing and is told yes or no.

The first draft of the design collapsed two of these. It proposed a `Door` cell
kind, which is a place named after an object, and the building lattice next
door already knows better: it draws thresholds as cells and windows as
*anchors*, because "a window is an anchor at a wall cell, never a cell kind."
The correction was made before any code existed, and it is the most consequential
thing in the campaign. Every passage now records a crossing cell, gated or not —
a cave that narrows where two chambers meet — and that cell is a `Threshold`
unless the passage is a sump, in which case it is deep water, because one cell
has one kind and a drowned squeeze is drowned. A door is a thing hung in a
threshold. So a natural cave gets squeezes and no doorways, which is what a
natural cave has, and the same machinery that hangs a door on a threshold could
later hang one against bare rock: the tomb's false door, an
object with no way behind it, admissible without a new class because the object
was never the place.

The one pair among the four that is *not* independent is requirement and
judgment. The walk never reads the plan; it reads the rock and the ledger. So a
proof about the plan is only a proof about the walked level while the two
agree, and the campaign owes a **realization witness** — a test that asserts,
for six hundred plans and every level of each, that each passage realizes
exactly one crossing of exactly the kind its requirement names, and that
nothing of the kind exists that no requirement asked for. In both directions.
A count that only went one way would let the realizer invent water.

## Why the stamping happens last

The grammar's lengthening move deletes an edge and splices a chain into every
realm path that carried it. An attribute keyed on an edge during growth would
therefore be orphaned by the next move that ran. So gates are stamped on the
*finished* plan, by a pass that runs after growth and touches no edge the
grammar might still rewrite. A plan with every gate ignored is exactly the
previous campaign's plan, which is the property that let the whole thing be
added without migrating anything.

That ordering paid for itself immediately, in a way nobody planned. The
previous campaign froze each realm's **length class** — Dormans' four
combinations of a long side and a short side — at the moment the realm was
created, and then went on splicing chains into both of its paths. The exported
class described a graph that no longer existed: at creation a realm's first
path holds at most three edges, and after growth it reaches sixteen. One of the
four classes, *long-short*, was therefore unreachable — zero of four thousand
four hundred and twelve realms measured — and four rows of the pattern
inventory selected on it were dead data nobody could have noticed by reading.

Nothing had read the class before, so nothing was wrong; the defect existed
only from the moment something wanted to use it. Recomputing the class after
growth, by the identical rule, makes it describe the realized paths, and it is
the only change this campaign made to the previous one's grammar apart from one
deferred repair taken deliberately (below).

The way the defect was found is worth stating plainly, because the project has
now met it several times: there *was* a test on the length class, and it passed
throughout. It tested the classifying **function** against hand-supplied
lengths, where the function is correct. It never asked what classes the plans
actually carried, and the answer was three of four, forever.

## The pattern says where; the world says what

A cycle pattern is a placement rule and nothing more: which side of a realm,
near the shared endpoint or far from it, symmetric or asymmetric, a key or a
capability. It never says *what* the requirement is made of. The substance is
derived from the rock and from the work, on exactly the terms the previous
campaign used for how many loops a level gets:

```text
  a passage in a WORKED place        -> a door, and its key
  a passage in karst or a fracture   -> a sump: standing water, and you swim it
  a passage in a lava tube           -> nothing; the row is inadmissible
  the up half of a stairway, any rock -> a chute: you fall down it, and fly up
```

The reasons are the rock's own. Karst and fracture systems are cut by water, so
a passage below the water line is drowned. A lava tube is one dry conduit and
its loops stay open. A hole in a floor is available to every kind of rock there
is. And a door is a made thing: in a wild cave there is nobody to have hung
one. The term for "worked" is the same term the cycle budget already used, so
the two derivations cannot disagree about which places are worked.

No gate is hand-placed, and no gate is drawn where a derivation exists. The one
draw the pass makes is *which pattern* a realm gets, and it is made even when
only one row is admissible, or none — so the number of draws depends on the
number of realms and never on the data, which is what keeps the plan's
degrees-of-freedom identity an exact equality rather than an estimate.

## The drop is a stairway with its up half omitted

The previous campaign settled that a stairway's two ends share a coordinate: a
down-stair at a cell on one floor is the same physical stairway as the up-stair
at that cell on the floor below. Its record closed by noting the consequence —
that the asymmetric twin is now expressible as *omitting the up half of a pair*
rather than as a special case. This campaign takes that sentence literally. A
chute writes the lip as its own cell kind on the upper floor and writes **no**
up-stair beneath it; the landing is made standable and reconnected exactly as
any stair foot is. `down` takes it and reads "you let yourself down the chute";
`up` from beneath it is refused, unless the body flies, and then it reads "you
fly up the chute". There is no new verb. The entire asymmetry lives in what
`up` will do.

And this is where the taxonomy's "valve versus asymmetric" axis dissolves. It
is not a property stamped on the edge. To a dragon the chute is asymmetric —
free down, costly up. To everything else it is a valve — free down, closed. It
is one gate seen by two bodies, which is the project's existing rule that an
affordance is a relation between an object and a body, carried into traversal.
There is deliberately no `Shut` way in the model at all: nothing natural in a
cave is impassable one way for *every* body, so the variant would be a reserved
seam with no constructor, which the project refuses. A true one-way passage on
one floor — a scree slope — wants a substance the realizer does not have, and
is a captured idea rather than an empty enum arm.

## Nine rows, frozen, and the tenth that could not exist

The inventory of cycle patterns is data and the resolver is code, on the same
split the project uses for its trope corpora: a constant table, one row per
pattern, each citing its source, its length is asserted by a test, and changing
it is a deliberate act. Ten rows were frozen at the design review. Nine
shipped.

The row that went is `the-landing-hall`, which wanted a realm whose two paths
are both short and which crosses a floor. It drew zero times. The first reason
given for that was wrong, and the review said so: "a cross-floor lower path has
at least three edges, so both paths cannot be short" does not follow, because a
two-edge path against a three-edge one classifies as short-short under the
frozen rule.

The true argument is geometric, and it survives the post-growth recompute
above, which the first one would not have. A cross-floor realm's lower path
lands on the *same grid squares* as the realm's own endpoints — its two stairs
move no square — so both paths are unit-step walks between one pair of squares.
A grid is bipartite, so the two lengths are congruent modulo two; and a spliced
detour replaces one edge with another walk between the same two squares, so it
moves a length by an even amount and the congruence survives every growth move.
Short-short requires the two lengths within one of each other and not both at
least three; equal parity turns "within one" into "equal", which forces both to
be at most two — while the lower path holds two stairs and at least one hop, so
it is at least three. The cell is empty. The lemma is *witnessed* as well as
argued, by a sweep over eighteen hundred plans that looks for the shape
directly and fails loudly if it ever finds one.

The rule that removed the row is the inventory's own: a pattern nothing selects
is dead data, not coverage. That is now mechanical rather than a one-time
measurement — a sweep asserts that every remaining row is applied somewhere, so
a row that goes dead under a later grammar change reds a test instead of
sitting in a healthy-looking table. Which is exactly the failure this campaign
found by hand, and could only have found by hand.

One more row moved rather than went. The chute was specified on a realm's
*short* side, and a side named by length turned out to be the wrong axis
entirely: under the commonest class, the short side is the same-floor path,
which has no stairway to hang a drop on. Seven hundred and seventeen of the
row's eight hundred and twenty-three draws were refused for want of a stairway,
while the row sat in the table looking alive. The drop belongs on the path that
*descends*, whatever its length. Naming the side by geometry instead of by
length takes the row to eight hundred and twenty-three applications out of
eight hundred and twenty-three.

## Solvability is the round trip

A body holding nothing, able only to walk and wade, must be able to reach the
terminus and every key from the entrance, on every descent, by construction.
This is not a prediction; it is an invariant, enforced by the stamping pass
itself: each placement is made tentatively, the reachability is recomputed, and
a placement that breaks it is unstamped and recorded as a refusal. Dormans'
safety rule — the key before the lock — is that reachability stated per lock.
The reachability is computed over the product graph of position and keys held,
which is small because a descent carries a handful of keys, and one checker
serves the placement pass, the guard test and the readout.

The design as approved asked only that the terminus and every key be reachable
*from* the entrance, and the review of the readout task caught what that
misses. A chute is free downward. Take one down into a realm whose upper path a
nested sump later blocks, and the default body can get in and cannot get out.
That is a trap, and Dormans' "unknown return path" never means an *absent* one
— it means the return differs. So solvability is now the **round trip**, and
the pass refuses any placement that would strand the body.

The consequence was measured rather than reasoned about: on the panel's first
seed the change converts exactly one chute from placed to refused. One
placement, in eight hundred and seventy-four descents, that would have shipped
a cave you could fall into and not leave.

## Four readings, and what they measured from and to

The readouts were frozen in the design before the code existed, and each says
what it measures *from* and *to* — the previous campaign's lesson about a
metric whose bluntness was invisible until it was stated that way.

**Gate yield** runs from the realms whose draw selected an admissible pattern
to the realms whose pattern applied in full, per descent, and takes the panel
median. Predicted at least 0.70. Measured **1.0000 on all three seeds** —
PASSED. Falling below would have meant the grammar's geometry leaves most
patterns no room, which no amount of table-tuning should hide; the geometry
leaves them room.

**Detour cost** runs from the default body's shortest round trip through the
gated graph to the same round trip ungated, over descents carrying at least one
realized requirement. Predicted at least 1.10. Measured **1.2143, 1.2069 and
1.2000** — PASSED. It is a round trip and not a descent precisely because a
chute costs nothing on the way down and everything on the way back; a one-way
measure would have read every chute as free.

**Solvability** is a guard and never a number to be admired: **874 of 874,
1681 of 1681, 1266 of 1266**, and again over a sanctioned sweep of fourteen
thousand four hundred plans, which is large enough to be slow enough that it
runs at merge cadence rather than at commit cadence.

The report-only lines carry the campaign's one honest awkwardness. Sumps and
chutes are everywhere — three to six thousand of each per panel seed — and
**doors number zero**, on every seed. That is correct, and it is the disclosure
the frozen wording points at. A door needs a maker, so a door row is
inadmissible in a wild cave; the panel's own descents are wild caves because
the production walk hardcodes one for every cave a player enters. On worked
descents the doors are there — **872 of 874** worked descents carry at least
one — and the plan carries them for every worked descent in the world. Nobody
can walk to one yet. Giving the walk a worked place to stand in is the next
campaign's first job, and this is the program's own ordering rather than an
oversight: it is flagged on the page, in the design, and here.

The previous campaign's four numbers moved once, by a digit or two, and the
attribution is a revert. A repair it had deferred — a growth move that tested
its own invariant against the passage set as it stood *before* the move rather
than after — was taken here, and with that single call reverted the panel is
byte-identical to its baseline. The gate pass alone moves nothing, which is
what it promised. The repair was taken now rather than later because of the
next section.

## The plan grammar became a save-format contract

Every plan, gate and level in this campaign is derived on entry and discarded
on exit; none of it is stored. But a descent key's **identity** is a function
of where the plan put it — the vertex, the level, the grid square — and picking
one up commits a custody fact whose subject is that identity. From the first
world saved holding a descent key, a change to the plan's draws or to the
pattern selection would leave that world naming a key that no longer exists.

So the plan grammar is a save-format contract from here on, and a later change
to it is a real epoch with a version suffix, never a silent edit. The previous
campaign's design could say "any new draw changes plan bytes — allowed, because
no world reads them." A world does now, and that sentence has stopped being
true. Storing descent custody as session-only state was considered and refused:
custody is already an observable on the session snapshot, and two custody rules
for one kind of key would be exactly the verb-times-object table the object
system exists to avoid.

There is a second, smaller movement of the same family, and it is worth naming
because it is a rule rather than an incident. Adding a `door` kind registers a
**concept**, and the concept registry's accession log is append-only: a new
accession epoch, a row in the proto-root tables and the solitary-tongue
lexicon, and the seed-42 world file moves because the registry is serialized
into it. Every one of those diffs is additive — one line inserted, one count
incremented, nothing re-sorted — which is the accession discipline working as
designed. The previous instance of this cost was recorded for a brazier; two
instances make it a rule rather than a one-off, and the cost of a thing kind is
now stated where the kinds are.

## Two earlier chronicles overstate, and this one says so

Chronicles are not edited after they merge; a record that outlives its subject
is corrected in the open rather than quietly.

The Chattel's chronicle says six verbs shipped "with `lock` and `unlock`
beside them." Neither verb exists, and neither is added here. Unlocking is a
side effect of `open`: a door with the right key in custody opens, and opening
it unlocks it first. Closing does not re-lock it, which is that campaign's own
ruling (0399) and which a test of this one's still pins.

The Gallery's chronicle says swimming and flight are "designed and not
implemented, on purpose, so a future campaign extends one enum variant rather
than inventing a second capability model." That is now discharged, and by
exactly the route it predicted. Deep water reports its own movement mode, which
is the first thing in the tree to reach the reserved `Swim` variant; a body's
locomotion is a sparse component keyed on its species — six kinds swim, three
fly, and absence is the default — and the walk composes the mode the cell wants
against the locomotion the body has. A shark crosses a sump the default body
was refused at, narrated as swimming; a dragon takes a chute upward. Both are
walked, in a test, by a possessable body: the project's rule is that a
capability nothing can reach is not a capability, and a registry row is not a
walk.

## What is deliberately not here

Danger and secrecy are **stamped and realized by nothing** — exported and read
by no one, exactly as the previous campaign exported its length class. A
dangerous route wants a creature to place, which is the next campaign's. A
hidden shortcut wants a passage that reads as wall until it is found, which is
a render change and a search affordance, not a plan attribute. A collapsing
gate wants a record that a body crossed a particular cell, which no fact
records and which would make a level cell a committed subject. Each of the
three is a captured idea with the reason it is not built.

Nor is there a requirement of the *known* kind — a word, a face — though the
mechanism's half exists; nor a door that reads the world rather than the
traverser. That last is the boundary of the whole model, and it is worth
stating as a limit rather than a gap: a requirement here is a predicate on the
traverser's own state at the threshold, and on nothing else. That is what makes
solvability a proof over positions and keys. A door that opens at moonrise, a
gate the tide shuts, a plate that needs two bodies, a corridor that remembers
which way you came — none of them is a gate in this model, and the first of
them is the cheapest and most valuable thing the project could build next,
because the sky is already deterministic and the proof would have to become
"reachable at some hour."

And no player reaches a door. The plan hangs them in every worked descent in
the world; the walk enters every cave as a wild one. That gap closes when the
walk learns to read what a place was made by, which is a campaign and not a
paragraph.

What is here is the difference between two walks, made structural: a loop that
is still a loop, with one side of it closed to a body without the key.
