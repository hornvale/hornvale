# The Retainer

**July 2026 · outcome: merged — walking a world becomes ~2.4× faster (5.4 s →
2.3 s) by deriving its terrain and climate once per session instead of rebuilding
them on every glance, without moving a single committed byte**

## What was attempted

Of everything the world can do, the slowest turned out to be the most
human-facing: *possessing* a settler and walking around took over five seconds
to run a ten-line stroll. That is the one place latency is felt directly — a
person waiting between looking and seeing — and it was the one major entry point
never profiled. Profiling it found almost none of the time spent on the walk
itself. It was spent rebuilding the planet.

Two redundancies compounded. A world is defined as a seed and a ledger;
everything else — the tectonic terrain, the climate over it — is *derived* on
demand, which is elegant until the same derivation runs many times for one
question. First, the climate-deriver quietly rebuilds the terrain inside itself,
so every place that asked for both terrain and climate built the terrain twice.
Second, and worse, the routine that describes the *sky* rebuilds the entire
terrain and climate every time it is called — because it ends its report with
the weather over the settler's home — and the walk calls it on every single
observation. Five glances, five planets rebuilt.

## What shipped

The fix is a principle, not a cache: **derive once per unit of work, and reuse
within it.** The tempting shortcut — a global memory of built worlds — was
rejected deliberately, because the Laboratory builds each of its thousands of
worlds exactly once, so such a memory would help the census nothing while
threatening to exhaust memory, and it would quietly become a second source of
truth beside the seed and the ledger, which the constitution forbids.

Instead, the reuse is *scoped*. Everywhere both terrain and climate were needed,
the terrain is now derived once and the climate built from it — a seam the code
already had, merely unused. And the sky report gained a companion that accepts
the already-derived providers, so a possession session — which already holds its
world's terrain and climate — hands them in rather than rebuilding them each
glance. The walk's per-observation cost collapsed from a planetary rebuild to a
weather lookup.

Nothing observed changed. The rebuilt providers were always identical to the
reused ones — the same pure function of the same seed — so the campaign proved
its non-movement the only way that counts for a rendering-and-narration change:
the committed possession transcripts, the almanacs, the maps, and every other
artifact regenerate byte-for-byte. What was two seconds of redundant computation
is simply gone.

## What it leaves reserved

The same "derive once" seam quietly helped the Laboratory too: its per-world view
had been building terrain twice, and no longer does. But the deeper reuse — a
built world carrying its derived providers with it, so that even loading a world
and then describing it need not derive twice — was left for later, as it would
touch the contract of what building a world returns. And with the redundancy
removed, the honest remaining cost of a walk is now legitimate: one terrain
build, the room mesh underfoot, and — newly the largest share — the little
homeostatic lives of the settlement's people, ticking through their needs. That
is real work, not waste, and a different campaign's to quicken.
