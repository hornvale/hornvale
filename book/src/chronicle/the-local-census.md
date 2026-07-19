# The Local Census

A census is Hornvale's largest act of self-measurement: a thousand worlds
built from a thousand seeds, each interrogated by every metric the laboratory
knows, the answers folded into a single table the book publishes and the
drift-check guards. It is also, until this campaign, something the project's
own hardware could not afford to run. A single all-metric census world cost
roughly two hundred and eighty-five seconds of processor time; a full census
ran for hours, and so it ran on a rented cloud machine, once per campaign,
paid for in dollars and in the lag between a change to world-generation and
the fresh numbers that confirmed it.

The cost was not the census's to bear. [The Single Sculpt](./the-single-sculpt.md)
had already found, and reconnected, the place where *genesis* re-sculpted its
terrain four times over; but the laboratory's metric path was never on that
map. The terrain of a world is expensive to make and, by design, a pure
function of its seed — which is exactly why it is wasteful to make it more
than once. This campaign is the story of finding, and stopping, every place
the census made it again.

The first was easy to see. One metric checked that every settlement's glossed
name was truthful to its own site, and to do so it walked every settlement and
rebuilt the entire climate — a full terrain sculpt — for each one. On a world
of a hundred and eighty settlements, that is a hundred and eighty sculpts
where the world already held exactly one. Threading the laboratory's
already-built climate into the observation, rather than sculpting a fresh
globe per settlement, cut the per-world cost from two hundred and eighty-five
seconds to a hundred and five. A clean two-and-three-quarters times, and it
was tempting to stop there.

That temptation was the trap. A profile taken *after* that fix showed the
terrain sculpt was still ninety-one percent of the whole cost — the first fix
had not removed the sculpting so much as *unmasked* it. The census had been
re-sculpting the globe in half a dozen more places, each buried inside a
worldgen helper the metric called by name, none of them visible to anyone
reading the metric's own source. A grep of the laboratory could not have
found them; only the profile could. So the campaign became a loop: read the
flamegraph, find the function whose subtree was widest, thread the pre-built
terrain and climate into it, prove the census rows came out byte-for-byte
identical, and profile again.

The loop ran six times. The lexicon metrics — a dozen of them — each rebuilt a
species' whole vocabulary, and rebuilding a vocabulary re-sculpted the globe to
classify which concepts the people had lived near. The chorus metrics rebuilt
every culture's account of the world, once per placed people, sculpting each
time; and rebuilt every people's cyclic beliefs, sculpting again to re-observe
the sky. The demography metrics rebuilt the whole population from a freshly
sculpted terrain. Each fix was the same small idiom — reuse the value already
in hand instead of deriving it afresh — and each, verified against a clean
build of the unchanged code, changed not a single byte of any metric's answer.

The last sculpt was the most surprising, because it was not in the laboratory
at all. It was in *genesis*. Naming a world — asking the dominant people for
their word for "earth," and asking each people for its word for itself —
builds those peoples' lexicons, and building a lexicon re-sculpted the globe.
Genesis was doing this six to eight times, for naming alone, on every world it
ever built, having already sculpted and *kept* the very terrain the naming
needed a few stages earlier. Threading that kept globe into the naming left
every world's name unchanged — the seed-42 world, its almanac, and the whole
committed gallery came out identical to the byte — and made not only the
census but every `new` command and every almanac render measurably faster.

The arithmetic at the end: two hundred and eighty-five processor-seconds a
world fell to under six, a little under fifty times faster, and the full
census — the thing that had needed a rented machine and hours — came home to
run on the authoring hardware in roughly five minutes. The discipline that got
there is worth naming, because the campaign nearly stopped at the first fix and
would have left nine-tenths of the cost in place. A performance number is a
claim about where the time goes, and the only honest way to make that claim is
to measure it. Every time this campaign guessed, it guessed wrong; every time
it profiled, it found the next real thing. The redundant sculpting is gone
because the flamegraph, not the intuition, decided what to cut.
