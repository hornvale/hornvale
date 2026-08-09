# The Warren

The world had an inside, and two creatures authored to live in it, and a
function that said what the air down there was like. Nothing called the
function.

So a rust monster — a cave-dark, damp-loving, iron-eating thing whose whole
description is *underground* — was being asked how well it liked the sunlight
on the hillside above it. Not as an approximation. As the actual question the
model asked before deciding where it lived.

## The half-connected campaign

The predecessor gave two creatures honest subterranean niches. It removed a
low-insolation curve that had been standing in for *dark* and replaced it with
darkness; it removed a below-sea-level elevation standing in for *underground*
and replaced it with a real reading of a real chamber. It measured the
improvement carefully and correctly, and shipped a byte-identical world.

It measured it **by hand**. The function that describes a chamber's environment
was public, documented, and called exactly once — from a test.

The project has a name for this. Its own ladder of probe-validity puts
*inexpressible* at the bottom and *genuinely measured* at the top, and second
from the bottom sits **expressible but unread**: a trait you can author that no
system consults. The ladder exists because that rung is the one a careful
campaign walks into by accident, and it warns that a number at that rung looks
exactly like a result. This is that rung, arriving in the campaign immediately
before the one that depended on it, in a programme that wrote the warning down.

It is not a failure of the earlier work. It is the consumer half, deferred
honestly and then found where the ladder said it would be.

## A kind declares its realm; the world decides where that realm is

The fix has two halves, and the second is the one that is easy to miss.

The first is that a kind says where it lives. That could have been derived
instead — a creature whose insolation optimum is zero and whose moisture
optimum is 0.90 is obviously subterranean, and today that rule would classify
both of them perfectly. It would also be circular. Those values read as
*underground* only because the previous campaign authored them to mean
underground; deriving the realm back out of them would reinstate the proxy that
campaign spent itself removing. So the realm is authored, and the claim becomes
falsifiable in the other direction: a kind can now be declared subterranean and
turn out to fit badly there, which is a finding rather than a contradiction in
terms.

The second half is that **declaring a realm is worth nothing without saying
where the realm exists.** Caves are not everywhere. They are a twelve percent
minority of land cells, and about half of those are sealed. A creature scored
against cave conditions on *every* land cell would draw habitat from ten
thousand places with no void in them at all — a strictly worse model than the
sunlit one it replaced, and worse in a way that would have looked like success,
because the fit numbers would have gone up.

So a subterranean kind is scored against the chamber, and multiplied by whether
this particular cell has a chamber to be scored in.

## What the measurement said, and then said differently

The prediction was that a rust monster's fit would improve underground and a
xorn's would not move. Over twenty-five worlds, measured before this branch
absorbed the two campaigns that landed beside it, both halves held:

```
  rust monster    2.557x     (measured by hand, one campaign earlier: ~2.5)
  xorn            0.979x     (measured by hand, one campaign earlier:  1.02)
```

Reproducing that *asymmetry* through an entirely different code path was the
result: a wiring change that merely made numbers move would have moved both.
The xorn does not move because nothing environmental can move it — its potency
buys a large sovereignty floor and its devotions to every axis are near-zero.
It was never faking darkness; it was ignoring climate altogether.

Then the branch absorbed main, and the number became **exactly 1.000**.

## The minimum that cannot see the improvement

A campaign landing alongside this one replaced the product of four condition
tolerances with **Liebig's law of the minimum**: a creature's fit is set by its
scarcest requirement, not by the product of all of them. That is the better
model, and it is not what broke this.

What broke it is that three of the four axes are floored by the creature's
sovereignty — the buffer that potency and mass buy against environmental
constraint — and the fourth, elevation, is not. The function's own
documentation states the consequence before anyone had a reason to care:
*a floored axis can never bind, so whichever axis is left bare becomes the
sole determinant wherever it dips below the others' floor.*

Measured on a rust monster, over the cave-bearing cells of seed 42:

```
                  temperature   moisture   insolation   elevation    min
  surface            0.7327      0.5850      0.4670       0.2498    0.2498
  subterranean       0.7327      0.7865      0.8399       0.2498    0.2498
                                 +0.20       +0.37        unchanged

  the creature's sovereignty floor:  0.4663
```

Going underground does precisely what it was built to do. Dampness improves by
a fifth; darkness by more than a third. And the minimum never sees either,
because elevation sits at a quarter — below the floor, and therefore below
every other axis — and a chamber is recorded at the elevation of the ground
above it. The predecessor campaign chose that deliberately: a literal
metres-below-surface offset needs a depth coordinate the world does not have.

So the campaign ships **half its mechanism live**. The gate works. The
substrate swap is real on the axes it moves and invisible in the number those
axes feed.

This is the second prediction this campaign got wrong, and the more useful one.
The first — that world identity would move — was wrong about a consequence.
This one is wrong about the mechanism, and being wrong about it has produced
the most precise statement anyone has of what the tolerance model currently
cannot express: **a non-lethal preference cannot matter while an unfloored axis
is scarcer.** A two-tier replacement that separates lethal gates from
preferences already exists, written by the same campaign that introduced the
minimum, and is deliberately not yet binding. The day it binds, a test in this
campaign turns red on purpose and says so.

## The gate, which does work

The half that survives is the half that was harder to argue for and easier to
overlook:

```
  land cells with non-zero fit    390,813  ->  46,993      (100.0% -> 12.0%)
```

Twelve percent is the cave fraction. A creature that lives underground is no
longer drawing habitat from three hundred and forty thousand cells with no void
in them, and no amount of supply anywhere recovers them — the gate is a hard
zero rather than a bounded tolerance, which is what makes it the first thing in
this model that can *select* rather than merely modulate.

## No world changed, and one creature moved house

The campaign predicted world identity would move. It did not — not in one seed
of twenty-five.

The reason is a structural fact worth stating plainly: **settlement genesis
packs only peopled species.** Which creature thrives where is computed live,
every time; only peopled species commit a place to the ledger. So re-scoring
fauna cannot move a world's identity. It moves only the derived views that ask
the question at read time.

Three of those moved, and they are the whole visible result:

```
-  [565] a wild xorn
-  [566] a wild carrion-crawler
+  [565] a wild carrion-crawler
+  [566] a wild giant-elk
```

The xorn is gone from that hillside. There is no cave there. A giant elk stands
where it used to be.

## The eighty-eight percent that was never a drift

The affect trace re-pinned at a hundred and eighty-eight lines, which reads as
a large behavioural change and is not one. Read by species rather than by line,
it is three unrelated things wearing one diff:

```
  bugbear, kobold, gnoll        0 lines differ
  otyugh, hobgoblin, human,     1-4 lines, and NOT ONE label change
    goblin
  rust monster                 40 lines, 34 of them label changes  <- the target
  carrion-crawler              36 lines, 35 of them label changes  <- a neighbour
  xorn leaves; giant elk enters                                    <- a re-index
```

Six of nine species are untouched. The apparent collapse in the file's label
frequencies — *Content* falling from fifty-four to nineteen — is almost entirely
an artefact of comparing different creatures at the same index once the xorn
drops out and everything below it shifts up one.

Only two species genuinely moved. One is the creature this campaign aimed at.
The other, a carrion crawler, is not subterranean and its own scoring did not
change — but its *neighbours* did, and a scavenger that suddenly finds a giant
elk where a xorn used to be has a different hunger and a different fear. That
explanation follows the exact path an earlier campaign documented from niche
through the coexistence fit to the shared predator-prey fields and out into
every other creature's affect. It has not been walked through the packer by
hand, so it is written down here as the most likely account and not as a cause.

A hundred and eighty-eight lines was never the measurement. Before believing a
number, ask what its denominator mixes.

## Two numbers moved in two thousand worlds

The census is a hundred and eighty-odd metrics over a thousand worlds, twice.
Exactly two of them moved, and they moved in opposite directions.

```
  per-cell-diversity      mean 2.2739 -> 1.9574   (-0.3165)
  composition-variance    mean 0.4089 -> 0.4336   (+0.0247)
```

Both are the **gate's** doing, not the substrate's — which is the useful thing
about measuring after the mechanism was found to be half-inert. Removing two
creatures from the eighty-eight percent of land that has no cave in it is
sufficient, on its own, to produce the entire census effect.

The first is a **fall**: species per cell dropped by about fourteen percent.
That number was never diversity. It was two animals being counted in places
that have no room for them, and the model has been slightly too crowded for as
long as they have been in it.

The second is the one worth keeping. Composition variance — how much the mix of
species differs from one place to the next — went **up**. A creature that is
everywhere adds the same thing to every cell and flattens the comparison; a
creature present in one cell in eight makes two cells different. **Confining a
species made the world's places less alike.**

That is a better argument for the change than any fitness ratio would have
been, and it is the argument that survived the mechanism failing. The ratio
said the model scores a cave creature against a cave, which is merely correct
and, as it turns out, currently invisible. The variance says the world got more
textured, and that happened anyway.

## What this leaves

The dwarves. Two of the five are meant to live underground, and until this
campaign the only way to author them would have been a low-insolation curve on
a surface cell — the precise fake the previous campaign spent itself removing,
about to be recreated in the campaign that inherited its work. Now there is a
realm to declare and a world that knows where that realm is.

And a prediction, already implied by the twelve percent: a subterranean people's
habitat is sparse and patchy in a way no surface people's is. Dwarves should
cluster hard, and be absent from most of the map, and the places they are found
should be places with holes in them. That is not this campaign's claim to make.
It is the next one's, and it is now measurable rather than assertable.
