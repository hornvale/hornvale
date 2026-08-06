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

## What the measurement said

Twenty-five worlds. The prediction was that the rust monster's fit would
improve underground and the xorn's would not move, and the second half is the
one that mattered.

```
  rust monster    2.557x     (measured by hand, one campaign earlier: ~2.5)
  xorn            0.979x     (measured by hand, one campaign earlier:  1.02)
```

The xorn does not move because nothing environmental can move it: its potency
buys a large sovereignty floor and its devotions to every climate axis are
near-zero, so no curve on any axis changes its fit in either direction. It was
never faking darkness. It was ignoring climate altogether.

Reproducing that **asymmetry** through an entirely different code path is the
result. A wiring change that merely made numbers move would have moved both.

The range collapsed as predicted, and by almost exactly the right amount:

```
  land cells with non-zero fit    390,813  ->  46,993      (100.0% -> 12.0%)
```

Twelve percent is the cave fraction. The creature gained fitness where it lives
and lost it everywhere it should never have been scored.

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
  per-cell-diversity      mean 3.6998 -> 3.0603   (-0.6395)
  composition-variance    mean 0.3807 -> 0.4093   (+0.0286)
```

The first is the obvious one and it is a **fall**: species per cell dropped by
about seventeen percent, because two creatures stopped being counted in the
eighty-eight percent of land that has no cave in it. That number was not
diversity. It was two animals being scored in the wrong frame, and the model
has been slightly too crowded for as long as they have been in it.

The second is the one worth keeping. Composition variance — how much the mix of
species differs from one place to the next — went **up**. A creature that is
everywhere adds the same thing to every cell and flattens the comparison; a
creature that is present in one cell in eight makes the two cells different.
**Confining a species made the world's places less alike.**

That is a better argument for the change than the fitness ratio is. The fitness
ratio says the model now scores a cave creature against a cave, which is merely
correct. The variance says the world got more textured as a result, which is
the thing the correctness was for.

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
