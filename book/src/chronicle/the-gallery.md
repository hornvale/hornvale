# The Gallery

A gallery, in a mine, is the passage a miner actually walks — as distinct
from the shaft that merely gets them there. Twelve tasks made the underworld
one of these: a player descends, walks a real generated cave, sees where
they have been, meets something living in it, and climbs back out, with the
client's own pane showing the cave the whole time rather than the country
overhead. The campaign's own record turned out to need the same discipline
the cave does — walked, not merely charted from above — which is most of
what this chronicle is about.

## A rule that argued against itself before Nathan corrected the argument

The flooded-cell question looked, at the design stop, like it might sink the
whole campaign: a measurement swept 60 real seeds through the shipped
generator and found 58.7%–73.4% of every rung underwater. Three rules were on
the table — refuse a flooded cell outright, wade through it as if it were
floor, or route it into the existing `submerged` band, which already has its
own verbs and its own refusal.

The measurement's own numbers made the choice look easy: wading held
connectivity at 100.0% on every rung, while refusing flooded cells left only
21 of 60 seeds fully reachable. The implementer picked the third option
anyway — route to `submerged` — reasoning from the branch's *name* rather
than from what that branch could do. It could not do much: `submerged` has
no lateral geometry at all, and the session already refuses `go` there for
exactly that reason. Routing most of every rung's cells into a band that
cannot be walked laterally is not a fix; it is the impassable rule wearing a
costume, at a higher price.

That is worth recording past the immediate fix, because the reasoning error
is reusable in a way a wrong number never is. A rule that names a mechanism
has to be checked against what that mechanism can currently do, not against
what its name suggests it should be able to do — the same category of
mistake, one level more abstract, that a stale comment makes when it
describes what code used to do rather than what it does now.

Nathan's own correction, at the stop, was not a fourth pick from the same
three options. It rejected the frame the measurement had been run inside:
"wet" and "drowned" are two different categories, and the code had exactly
one. A cave with water in it and air above it is not a defect to route
around — `domains/terrain/src/water_table.rs` had been calibrated for
exactly this drowned-share for a whole prior campaign, on purpose. What
needed inventing was the *other* category: a chamber flooded floor to
ceiling, entered only from above, rare by design and explicitly deferred.
Wetness ended up keyed on a field that already existed and already varied
per leaf — `worked`, already compounded down a descent by inertia — so a
drow-tier hall comes out dungeon-dry and a wild cave comes out wet from a
dial that was already turning, not a new one built for the occasion.
Movement itself shipped as a *mode*, not a boolean, so the swimming and
flight this defers slot into the same seam later rather than needing their
own special case.

## The failure that came back inside its own fix

This campaign's most-repeated finding was not a design choice at all: a
comment or a test's own name describing what code used to do, left standing
after the code changed underneath it. It surfaced eleven times across twelve
tasks, and the two most instructive instances are the ones where the *remedy
itself* was what went stale.

Task 6 caught its own reviewer's finding and generalized it correctly: fixing
one stale comment about *what* happens is not the same discipline as
catching a comment about *when* something happens, because the second kind
goes stale in a file the change never touches — adjacent to the thing the
change was about, not adjacent to the diff. The remedy carried forward was
"after changing when something happens, grep the whole crate for prose
describing when it happens." That remedy worked, once. Task 10's own sweep
of its own change searched for `per keypress`/`per redraw`/`every keypress`/
`every redraw` and missed a line that said the scene "is re-derived" —
true words for a different mechanism, invisible to a grep built from the
words a fresh reader reaches for rather than the words the stale sentence
actually used. Task 11's sweep, one task later, having *just* been told
this, searched for "no creature is placed" and missed a line reading "no
creature arm at all" — the identical failure, immediately, in the very act
of fixing the previous instance of it.

The remedy that finally held is not a better grep habit; it is a different
*source* for the search terms. A spec that quotes the exact wording of the
defect it fixes — and this campaign's spec did, verbatim, for the sentence
Task 11 needed — hands the sweep its own search string. Grep what the spec
says the old prose said, not what you would have written it as. Task 12
used exactly this method to find and fix the two remaining known instances,
plus one more in a file no task on this campaign had otherwise opened.

## An honest null, and the 90x it produced

Task 10 was scoped as a parse fix: profiling from a prior campaign had shown
a 24x cost difference tied to the walk band's redraw path, and the plan's
working theory was that re-parsing the snapshot on every keypress was the
cause. The fix shipped, was benchmarked properly — a committed benchmark,
release build, real load — and moved the number by about 5%. Not 24x. The
task said so plainly rather than shipping the smaller number as if it had
been the target, and the honest gap between "shipped a fix" and "solved the
problem Nathan actually named" is what earned a second round rather than a
close.

The redirected measurement found the real cost: `Session::purview(0)`
itself, called fresh on every redraw regardless of whether the keypress had
moved anything. Caching that result behind the same once-per-turn
invalidation point an existing precedent already used — proven safe first,
by checking that nothing mutates session state outside that one point —
took the same benchmark from 1.1626 ms to 0.0129 ms per keypress. Ninety
times, not the 5% the first round alone would have reported as done. The
review that followed did not accept the number on its size; it checked that
a 90x speedup was work *avoided*, the intended kind, rather than work
silently *skipped* — by poisoning the cache to a value a live call would
never produce and confirming the discriminating test actually reads the
cache rather than falling through to a fresh call that would have hidden
the bug.

## A formula changed under a name that made it look suspicious

Task 11 shipped the campaign's inhabitants — derived from a chamber's own
rock and energy rather than a spawn table — and along the way replaced the
formula gating a species' fitness for a place, a Liebig minimum, with a
weighted blend keyed on each species' own niche profile. The implementer's
own framing of the change was by outcome: it made one named creature
mechanically unable to outrank another. That is exactly the shape decision
0016's preregistration discipline exists to be suspicious of — a formula
retuned to rescue a result someone wanted, after the fact.

The framing turned out to be misleading about the reasoning underneath it,
not about the conclusion. The `min` operator's failure was structural: it can
only shrink a score, so a species already capped by an unrelated pathology on
one axis can never rise on a different one, no matter how well-suited it is
along that axis — which made the spec's own requirement, that two chambers
with different dominant energy sources be able to hold different creatures,
unwitnessable at any real chamber. The weights themselves were not new
knobs; they were each species' own pre-authored niche value. And where a
species carries no such weight, the new formula collapses back to the exact
old one — correct biology falling out of the shape rather than a special
case bolted beside it. The review did not take this on the strength of the
argument alone: it hand-recomputed the disputed scores from source against
three decimal places, and confirmed the one pathology the argument leaned on
had been on the books for a different species, weeks before this task, for
an unrelated reason. The doctrine held; the summary had just undersold it.

## Closing the ledger honestly

The last task's job was to make the project's own records agree with what
had actually shipped, and the closest call in it was the systems-corpus
re-score. Three catalogue items had been marked against the old fold, each
citing the same registry row as their blocker; that row is now `shipped`,
and the audit's own generator would have kept reporting a resolved gap as
open forever if nobody edited the corpus's authored verdicts by hand — the
drift check has no way to see that a *citation* went stale rather than a
file.

Re-scoring found the render half genuinely delivered — a real chart, drawn
from the level the player is actually standing in, with lit, remembered and
never-seen cells distinguishable in monochrome — but thinner than the
catalogue's first pass would have credited. The pane anchors a level's own
top-left corner to a fixed-width plate and drops anything past it with no
camera-follow; every rung past the first is wider than that fixed plate, so
a deep cavern — the one catalogue item that names depth explicitly — can
walk its own player marker off the visible screen with nothing on screen to
say so. That is not a new defect this campaign introduced; it is the same
risk the spec named and priced as "the most likely place scope grows"
before a line of this campaign was written, left unresolved because nothing
downstream forced it back into view. It is recorded now rather than
inherited silently by whichever campaign reaches for this corpus next.

## What stays open

Cave prose — speleothems, lava, named formations — waits on a vocabulary
that tells rock types apart in words, unblocked now rather than built.
Drowned rungs, dive-entry, and swimming as a mode are designed and not
implemented, on purpose, so a future campaign extends one enum variant
rather than inventing a second capability model beside one already being
built elsewhere. The submerged band still folds into `walk`; nothing here
touches it. And the pane's own viewport — what a player sees once a cave
outgrows the screen it is drawn on — is now a named row rather than a risk
nobody wrote down.
