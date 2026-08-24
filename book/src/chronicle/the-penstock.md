# The Penstock

A ledger and an index disagree about what a fact is *for*. To the ledger a fact
is a thing that happened, filed in the order it happened; to an index it is a
point in a space you might later want to search. Hornvale has kept both since
the query engine landed — an append-only `Vec<Fact>` and three permutation
indexes over it, keyed on subject, predicate and object. What it had never had
was a way to ask the one question those indexes are best at answering: *every
fact about this subject under this predicate.*

The public surface offered the first such fact and the last — `value_of` and
`latest_value_of` — and nothing in between. So a caller wanting all of them
wrote the obvious thing:

```rust
for f in ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity) { … }
```

which reads every `agent-at` fact in the world and discards all but one
creature's. Run once per creature per tick, that is quadratic in the length of
a session, and it is the shape of a defect that cannot be caught by any budget
set against realistic data. At the scale of a committed test world — 12,534
facts — it costs thirteen milliseconds and nobody notices. At a million facts
it costs forty-eight seconds, per tick.

The fix was a method, not a subsystem. `Ledger::facts_of` descends the
subject-predicate index and returns the postings in commit order, and the two
call sites that mattered became one line each. Measured against the same
synthetic ledger, the sweep that took 48,288 ms takes 44.

## Why the campaign stopped there

The interesting part is what did *not* get built. The design that motivated
this work is a cache — a working set of materialised views over the ledger,
lazily filled and dropped when cold. Before writing it, the session measured
what it would buy. Against the *corrected* baseline, a materialised view saved
a further 2.5×, and that figure already included rebuilding the view from
scratch on every use. The lifecycle machinery — budgets, eviction, hysteresis —
was competing for the space between eighteen and forty-four milliseconds.

So the campaign became instruments instead: three of them, built to decide
whether the rest of the program is worth building.

The first measures **scaling as a slope, not a duration**. A wall-time ceiling
cannot catch an accidental quadratic — the defect above would have passed one
forever — but the exponent of a log-log fit can, on any machine, deterministically.
It sweeps agent count rather than ledger size, because that is the dimension
where the two shapes separate: an unindexed scan costs A·n = A²H, an indexed
one A·(log n + H). Measured across two runs, scan came in at 2.18 and 2.01,
`facts_of` at 1.21 and 0.94 — the predicted two-against-one, unprompted.

The second is a **gate on facts committed per agent per tick**, deterministic
where a timing budget could never be, and therefore able to fail a build.

The third splits a tick's cost into **query, plan and commit**, and answers the
question the whole program hangs on: does planning dominate? It does not. Both
directly measured terms are flat per agent while total tick cost is superlinear
— 1.43 fitted, and 2.17 across the segment from a hundred agents to two hundred,
which is to say near-quadratic exactly where it matters. The residual is
unmeasured and the conclusion is an argument from elimination, which the spec
says out loud.

## The falsifier that fired

The counter came back at 0.94 facts per agent per tick, and it did not fall.
Over forty ticks and again over a hundred, it held flat.

That matters more than any of the speed figures, because the ledger is
append-only by constitution. A rate that does not decay, multiplied by
unbounded time, is an unbounded ledger — a thousand agents over ten thousand
ticks is some nine million facts, and nothing about the arithmetic stops.

Tracing it produced the campaign's best finding, and it exonerates everyone.
The rule has always been that *only the discrete divergence commits; the smooth
routine stays derived* — a creature's position is the latest committed
`agent-at` fact, **else its derived schedule**. That worked because a fixed
two-point schedule was the default a divergence could be measured against. Then
drives arrived, and replaced the schedule with something better. No campaign
broke the rule: **the ground it stood on was removed.** With no default to
diverge from, every step is a divergence, and every step commits.

The repair is already written down elsewhere in the same design, under a
different heading. A plan re-derives from a committed intention, because the
planner is deterministic — so commit *"resolved to go to the spring on day
twelve"* once, and recompute the forty steps between intentions rather than
storing them. Log-bounding and plan-caching turn out to be one idea approached
from two directions, and the saving is the mean path length.
