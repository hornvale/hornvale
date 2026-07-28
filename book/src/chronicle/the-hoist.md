# The Hoist

A world's terrain is a pure function of its seed and its pins. That is the
whole architecture in one sentence: a world is a seed plus a ledger, and
everything else — the sculpted globe, the derived climate — is recomputed
on demand from those two things. Nothing needs to be stored because
everything can be rebuilt.

The trouble with a value that *can* always be rebuilt is that it tends to
get rebuilt.

[The Single Sculpt](./the-single-sculpt.md) found the composition root
sculpting the same globe nine times per world and taught the pipeline to
keep what it made. [The Local Census](./the-local-census.md) found the
measurement layer doing the same thing and closed six more leaks, dropping
the per-world cost from 285 CPU-seconds to under six. Both campaigns worked
the same seam, and both left one piece of it untouched, recorded in the idea
registry as "Fix D" with a note that it would change a signature.

This is that piece.

## The shape of the leak

The composition root builds a world in rungs — sky, then terrain, then
settlements, then the deep past — and each rung is a byte-identical prefix
of the ones below it. At any rung past the first it sculpts the terrain
exactly once and threads that single value through everything downstream.
The internal discipline was already perfect.

Then the function returned, handing back the world and dropping the globe on
the floor.

One stack frame above, the measurement layer's view chain asked for the
terrain of the world it had just built, and the only supported way to ask
was `terrain_of` — which sculpts it again, from scratch, from the ledger the
build had just written. The climate rung then derived its climate from that
second globe. Every row of the thousand-world census is built to full depth,
so every row paid for two worlds' worth of geology to look at one.

A profile put a number on it. Of all samples taken across a census run,
30.9% sat inside the sculpting routine, and they divided almost exactly in
half: 15.16% reached it through `terrain_of`, 14.86% through the build
itself. A near-perfect split is what duplicated work looks like from the
outside — two callers, one job, no way to tell them apart by cost because
they are doing the identical thing.

## Why the obvious fix is the wrong one

The tempting repair is to stop re-deriving: put the terrain in the world, or
memoize it, and let consumers fetch it.

Both fail, and they fail for reasons worth stating, because the reasons are
the architecture. Storing the globe in the world contradicts seed-plus-
ledger directly — the ledger is the durable representation precisely because
it is small, and a world that carries its own terrain is no longer a seed
plus a ledger but a snapshot. Memoizing is subtler: it puts mutable global
state in the determinism path, keyed by a value that is not monotone, and it
grows without bound across a census that visits a thousand distinct seeds.

`terrain_of` is not redundant machinery. It is the claim "everything else is
re-derived deterministically" made executable, and it has real callers: the
command line and the almanac open a *saved* world and have no build in hand.
They must re-derive, and they must keep being able to.

So the fix is not to remove re-derivation. It is to stop *forcing* it on the
one caller that already had the answer.

## What changed

The build now hands back what it made. Alongside the world it returns the
terrain it sculpted and the climate it derived, each present exactly when
the requested depth actually built it — absent, not stale, when the rung
never produced one. The old entry point keeps its exact signature as a thin
wrapper that discards them, so every existing caller is untouched and the
new path is purely additive.

The view chain then threads those values into the helpers that were built to
receive them. This is the half of the idiom that already existed: The Local
Census shipped eight functions that take a pre-built globe or climate as an
argument rather than re-deriving it. What was missing was any supported way
to *obtain* one. The consumer side had been built; the producer side had
not. That asymmetry is why this piece was the one left over.

Where a rung genuinely built no artifact, the old derivation remains as the
fallback — so the change is not merely faster on the census path, it is
never slower on any path.

Widening the return type had a side effect the campaign did not plan: it
forced an audit of every call site, and one of them turned out to have the
same disease. A standalone history entry point built a world to terrain
depth — sculpting the globe — and then immediately called `terrain_of` to
sculpt it again. Nobody had noticed, because from the outside the function
simply took a while.

## The proof

Since the pipeline is pure, none of this can change what a world *is* — the
kept globe is the same object the discarded rebuild would have produced. But
"cannot" is a claim, and the campaign's real work was making it checkable.

The dependency is narrower than it looks. The build sculpts from the pins it
was handed; `terrain_of` reconstructs pins by parsing them back out of the
ledger. The two agree only if that round trip is exact — so the guard
compares the hoisted globe against the re-derived one under non-default
pins, where a lossy round trip would show, as well as under the defaults the
census uses. (Under the defaults it cannot diverge at all: default pins
commit no facts, so there is nothing to parse back.)

Then the arithmetic held up its end. The census rows came back byte-for-byte
identical — the same hash they had before the campaign began. The sculpting
routine's share of the profile fell from 30.9% to 21.2%, which is what one
sculpt instead of two predicts once the total shrinks. And `terrain_of`,
which had accounted for 15.16% of every sample, appears in the census
profile exactly zero times.

The census got 24% faster, and the worlds did not move.
