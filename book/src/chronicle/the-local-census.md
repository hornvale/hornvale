# The Local Census

A census is Hornvale's largest act of self-measurement: a thousand worlds
built from a thousand seeds, each interrogated by every metric the laboratory
knows, the answers folded into a single table the book publishes and the
drift-check guards. It is also, until this campaign, something the project's
own hardware could not afford to run. A single all-metric census world cost
roughly two hundred and forty-five seconds of processor time; a full census
ran for hours, and so it ran on a rented cloud machine, once per campaign,
paid for in dollars and in the lag between a change to world-generation and
the fresh numbers that confirmed it.

The cost was not the census's to bear. [The Single Sculpt](./the-single-sculpt.md)
had already found, and reconnected, the place where *genesis* re-sculpted its
terrain four times over; but the laboratory's metric path was never on that
map. And it was worse there. One metric — the check that every settlement's
glossed name is truthful to its own site — walked every settlement in the
world, and for each one rebuilt the entire climate from scratch to re-derive
what that settlement observes. On a world of a hundred and eighty settlements,
that is a hundred and eighty full terrain sculpts where the world already held
exactly one, sitting in scope, already built. Two smaller religion metrics
paid the same toll a handful more times, and the view chain re-derived one
climate it had just built. The redundant sculpting was four fifths of the
whole cost.

The fix was the one the earlier campaign had already made idiomatic: thread
the pre-built value instead of re-deriving it. The laboratory's view of a
world already carries its climate. Two new observers take that climate and
reuse it, rather than sculpting a fresh globe per observation; the metric
builds its climate once, before the loop, and hands it to every settlement.
The plain observers stay, unchanged, for the standalone callers that ask a
single world a single question. Nothing about what the census *measures*
changed — a direct before-and-after comparison of the generated rows, seed
for seed, is byte-for-byte identical, and the metamorphic guard that already
watched the metric path stayed green. The redundant work simply stopped: two
hundred and forty-five processor-seconds a world fell to a hundred and five,
and the full census came home from the cloud to run in roughly ninety minutes
on the machine that authors the worlds.

There is a discipline in the number that did *not* move. Threading the climate
left a smaller redundancy behind — the sources built around that climate were
still assembled once per settlement rather than once per world — and the
obvious next step was to collapse that too, with a handle that built the
sources a single time. It was built, measured, and thrown away: it bought
nothing. The sculpt had been the entire cost; the assembly around it was
already cheap. The residual hundred seconds is honest work — the metrics
themselves, and the one world-build that must happen — not waste waiting to be
found. The campaign kept the fix that the measurement justified and reverted
the one it did not, which is the only way a performance number stays true.
