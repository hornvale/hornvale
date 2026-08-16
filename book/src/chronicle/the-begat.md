# The Begat

A metric became 44% more expensive without one line of its code changing.

The Glasshouse re-centred the temperature baseline, and the world it left
behind was larger in every direction that matters to history: habitable
fraction up a third, fertile land nearly doubled, the founding tree 17% denser.
The census that reads that world went from 882 seconds to 980. Almost nothing
in that increase was breadth. Two thirds of it was one column,
`history-myth-hop-median`, doing the same thing it had always done to a world
that had outgrown it.

## A cost that was superlinear in the thing that grew

The founding tree is the record of who was founded from whom: every community
carries at most one parent, the settlement it fled or split from, and the
roots are those founded at a site rather than from another community. Reading
a claim's inheritance depth means asking, for each community, which of its
ancestors witnessed the event.

The old reading answered that question from the wrong end. To find everything
descending from a community it walked *every* node's ancestry upward and
tested whether the community appeared in it — a list and a set allocated per
node, per query — and the median asked for that twice per node. The cost was
quadratic in communities and linear in depth. A tree 17% denser therefore
bought a bill 44% larger, and would have kept doing so, compounding, every
time the world grew.

The correction is to stop re-deriving what the data already knows. Each
community has one parent, so the parent map inverts, once, into a map of
children; and the question "everything descending from here" becomes a walk
*down* that map, carrying the hop count out with it rather than reconstructing
it afterwards. The measured cost of the column fell from 1.708 CPU-seconds per
world to a figure too small for a forty-world panel to resolve — an upper
bound of about 0.022, and consistent with zero.

## The obvious form of the fix was unsafe

The repair had been sketched in advance, in the tripwire that caught the
regression, as a *children and depth* map built once. Half of that is right,
and the wrong half is instructive.

Depth is not defined on a cycle. The ancestry walk carries a guard for exactly
that case — it stops when it revisits a node — and a depth map has no way to
express what the guard does, so it would have diverged silently on precisely
the input the guard exists to protect. A faster column with a different value
is not a speedup; it is a change to what the world says about itself.

What the walk does instead needs no assumption about cycles at all. Because
every community has at most one parent, the upward path from any node is a
single deterministic sequence, and the guard's stop-on-repeat means that
sequence enumerates each reachable ancestor exactly once. So *"this ancestor
appears in that node's ancestry"* and *"that node is reachable downward from
this ancestor"* are the same statement, and the downward walk computes it
exactly. The hop count follows from the same property: the downward path is
unique, and the one longer route — around a cycle back through the starting
node — is closed off by treating that node as already visited. The question
the sketch made urgent, whether the data can contain a cycle at all, turned
out never to need an answer.

## Two campaigns, one tree, and a collapse that was correctly refused

The Retelling was meanwhile making content vary along the same tree, and had
found the same quadratic from the other side. Its repair was a memo of each
node's ancestor *set*, built once, serving a membership question: is this
community an ancestor of that one?

The two campaigns met in a single file, and the structures looked redundant.
They are provably interchangeable — asking whether A is an ancestor of D is
exactly asking whether D is among A's descendants — so one could have been
folded into the other on correctness grounds alone.

It should not be, and the reason is not correctness but the shape of the
traffic. The children map answers *few large* questions: sweep an entire
subtree, once per witness. The ancestor memo answers *many small* ones: a
membership test per community per event, and a quadratic number per event
where divergent lines are counted. Serving the second from the first would
mean a fresh subtree walk, and a fresh allocation, for every one of those
tests — the very pattern this campaign had just removed, reappearing one level
up. They are opposite directions with opposite access patterns, and the tree
carries both.

The converse worry — that building the memo puts an allocation back on the
construction path, paid for every world the census touches — was answered the
same way, by measurement rather than argument: about six thousand node visits
per world, against the millions the old reading cost, and no detectable
movement in the column's price.

## What the column is worth is what it still says

A metric that gets faster and changes its value has not been optimised; it has
been replaced. So the claim this campaign had to earn was byte-identity, and
it was established by recomputation rather than reasoning: the column, rebuilt
over the census's own thousand-seed panel, against the committed census, on
every seed. It matched on all thousand — and again, separately, after the two
campaigns' structures were brought together, because a property proved of one
branch is not inherited by what results from joining it to another.

An empty difference is also what a broken comparison produces. So the
comparison was made to fail on command first: perturb one value of the
thousand, confirm it reports exactly one disagreement, and only then read the
zero as evidence.

## What the confirmation actually rested on

The other campaign then re-ran its own readout against the joined tree, and
the result was not merely unchanged: it was identical to the last digit, a
rank correlation of 0.6622007159632117, with the whole hearsay suite passing.

The reason that is a stronger result than it first appears is worth recording,
because the first account of it — including the one offered in this campaign's
favour — was wrong. It was said that the confirmation was a formality, since
the membership memo and its construction had been shown byte-identical on both
sides. That check was real but narrow. It did not cover the twenty-two lines
this campaign changed *inside the other's file*, where the walk that produces
their readout lives: the derivation there was rewritten to take its hop count
from the downward walk instead of re-deriving each node's ancestry.

So the readout did not survive an untouched dependency. It survived a
**reimplemented** one. What licensed the conclusion was the other campaign's
own verification: the ancestry walk hashed identical on both sides, and the
reimplemented descendant query was shown to preserve its ordering contract —
it sorts pairs whose identifiers are unique, so mapping away the hop count
leaves the sequence ascending exactly as before. A bit-identical readout over
a rewritten dependency is a much better piece of evidence than a bit-identical
readout over an unchanged one, and it is only available because someone
checked the thing the narrow test had not.

## A price recorded once is a price that rots

The column had carried a cost claim since it was registered — about 0.16
seconds per world, comfortably under a stated threshold. It was wrong by more
than tenfold, and the threshold it invoked has never existed anywhere in the
project except in the sentence invoking it.

The deeper fault is not the number but its shape. A cost per world is a ratio,
and this cost was superlinear in a quantity the world can grow: the denominator
moved. Even a figure correct on the day it was written would have decayed the
moment the founding tree thickened, which is exactly what happened. Nothing
re-measures such a claim, because a metric has nowhere to record what it costs
— only a comment, and comments do not fail.
