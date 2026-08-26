# The Tailrace

A tailrace is the channel below the wheel — the works nobody designs with any
enthusiasm, because it handles water that has already done its job. It is also
the part that decides whether the wheel can keep turning: back the tailrace up
and the spent water pushes against the very blades it just drove.

Hornvale's fact ledger is append-only by constitution. Facts accumulate, and
the folds that read them are recomputed from the beginning on every read. So
the ledger is a tailrace with no channel: history that has already done its
work, still standing against the wheel. This campaign is the read side of
draining it, and it is worth being blunt about what that does and does not
mean. **It removes no fact.** Committed bytes at the end of a session are
byte-identical before and after, by construction and by the drift check. What
it delivers is the precondition every plan to shrink the log silently assumes
and none of them had: that nothing depends on the raw history being there.

## The argument the program had not made

The standing case for bounding the log was about storage. Commit rate is flat
rather than falling ([The Penstock](./the-penstock.md) measured 0.94 facts per
agent per tick and watched it hold across a hundred ticks), and a non-summable
rate multiplied by unbounded time is an unbounded ledger. That argument is
correct, and it is a projection: it describes a world nobody has run yet.

There is a second argument, and unlike the first it is measurable today. Six
folds in the creature-drive stack walk an agent's entire committed `agent-at`
trail on every evaluation, and they run per agent per tick. So **the log costs
CPU while it sits there, before it ever costs a byte too many** — and total
session cost is quadratic in session length, not linear.

Neither existing instrument could see it. One sweeps *agent count* at a fixed
twenty ticks, which makes a history term invisible by construction. The other
sweeps ticks and measures facts committed, never time. So the campaign opened
by building the missing instrument: fifty agents held fixed, two hundred ticks
in bands of twenty, the deterministic columns (facts committed per band,
navigation-cache searches) carried alongside as a control. Those controls do
not move — 1,117 to 1,150 facts, 550 to 556 searches, band to band. The
workload is identical; only the history grows.

Against that, the decisive column times one fold alone. `drive_at` is
proportional to the history it walks: elasticity 0.86 to 1.24 across four runs
against a 2.48× growth in history, at 1.89 to 3.33 microseconds per call per
fact of history. One call costs three quarters of a millisecond to a
millisecond at 322 facts, and it is called more than once per creature per
tick. Read against the whole tick, the history term is **70 to 80 per cent of
it**.

## Looking for a linear term and finding a quadratic one

The campaign's preregistered model was affine — `cost = C + k·h`, a fixed floor
plus a per-fact slope — and its hypothesis was that `k > 0`. That is what a
whole-history fold predicts, and it is what the in-situ instrument measured.

Then a synthetic sweep, built to widen the range from 2.48× to about a
thousandfold so that the floor `C` would become identifiable at all, disagreed
with itself. At low depth it read an exponent near one. At ten thousand facts
it read 2.09. Both readings were correct, and reconciling them is the finding
the campaign did not set out to make.

`integrate_thirst` sums the thirst rise over the segments between an agent's
sightings, and to find the position holding at the start of each segment it
scans the sightings vector backwards from its end. That inner scan is
O(history), inside an outer loop over the sightings since the agent last drank.
So the true cost is

```
O(H + S·H)
```

where `H` is the agent's whole `agent-at` history and `S` the postings since its
last `drank`. Every `drank` fact resets `S`; nothing bounds it otherwise. Write
the single-reset case as `a·H + b·H²` and its elasticity is `(a + 2bH)/(a + bH)`,
which tends to 1.0 well below the crossover `H₀ = a/b`, reads exactly 1.5 at it,
and tends to 2.0 above. The two instruments were sampling opposite sides of one
bend.

Interpolating the crossing from the synthetic sweep's own local elasticities
puts it at **H ≈ 146 to 189 facts** — two independent re-measurements
disagreeing by about thirty per cent, which is the honest range rather than the
single figure an earlier draft asserted. At roughly half an `agent-at` posting
per agent per tick, a couple of hundred facts is a couple of hundred ticks. An
agent enters the quadratic regime early in an ordinary session, not late.

The obvious escape is that agents drink, so `S` stays small in practice. The
roster says otherwise. At the final band the fifty agents' `drank` counts run
min 0, median 9, max 47 — and **23 of them, forty-six per cent, have never
drunk at all in two hundred ticks.** For nearly half the population `S` is
simply unbounded over the run's horizon, and the "never drinks" regime the
synthetic bench isolated as a deliberate edge case is the regime they are
actually in.

Three findings that arrived separately turned out to be one causal chain, and
the link was found by measuring rather than by arguing. The probe agent — chosen
as the roster's maximum-history member, which is by construction the one that
keeps walking — knows of **zero** water rooms at every band of every run. It
cannot drink because it believes in no water source, not because its thirst never
crosses threshold; which is also why it never arrives anywhere, and therefore
why it accumulates the most postings. No believed water, so never drinks, so `S`
unbounded, so the quadratic regime is its production regime. That is a second,
independent argument for the primitive, and the parent program had made neither
half of it.

## A fold advances; it is not invalidated

The shipped mechanism is one kernel module, `kernel/src/fold.rs`: a trait
implemented by the accumulated state itself, and a holder carrying that state
alongside the ledger position it is valid at.

The interesting part is which existing thing it is *not*. [The
Forebay](./the-forebay.md) had just shipped a general store for a derived value
with a ledger-validity class, and it looks like exactly the right home. It is
not, for a structural reason rather than a marginal one. That class is a **memo**:
an entry goes stale once a fact touching a watched dependency commits after the
entry's recorded position, and a stale entry is evicted on read and recomputed
from scratch. Here the watched dependency is *this agent's own position*, which
the tick commits every time the agent moves. The entry would be stale every
tick, every read a miss, and every recomputation the whole-history walk the
cache existed to avoid — a cache with a structurally guaranteed hundred per cent
miss rate.

Invalidation and accumulation are different operations, not two policies over
one operation. A memo asks *is this still true?*; a fold asks *what does this
become?* There is nowhere in the first to put an update function, and adding one
would change what the type is. So the primitive sits beside the store rather
than inside it, and the store is left untouched (decision 0236).

The other design rule the campaign had to name is what bounds the drives at all.
An accumulator at the log's frontier cannot answer a query about a past instant,
and three of the six folds are asked exactly that. What saves them is that each
has a reset event — `drank`, `eaten`, `rested` — so a past read is served from
the last reset at or before it. **The reset event is the checkpoint** (decision
0237), and it is why these folds are bounded rather than merely cheaper.

The ledger's write path gains no hook. The tick already holds the facts it is
about to commit, so the caller advances the fold explicitly and `Ledger::commit`
is unchanged. Nothing entered the save: a fold state is session state,
re-derivable from the ledger, the way the navigation cache and the mesh memo
already are.

## The tests were checked by breaking the code, and two of them needed it

A primitive whose whole job is *the incremental answer equals the batch answer*
is worth exactly as much as the tests that pin it, so every property was
verified by applying a mutation that still type-checks and requiring the
intended test to go red. Two of those mutations found things no amount of
reading had.

The first is a test comparing advancing in two steps against rebuilding from
scratch — the obvious shape for a fold-equals-scan property, and green. An
off-by-one in the skip count left it green while its sibling went red. The reason
is structural: `rebuild` is *implemented by calling* `advance_to`, so a bug
inside `advance_to` is applied identically to both sides of that comparison and
cancels. Both sides absorbed the same facts, both missed the same one, and both
agreed. The test named for folding from scratch was comparing a function against
itself.

The suite was not vacuous — the sibling test reaches the same state through the
per-fact door, an independent path, and does catch it — so the defect was in the
naming and in the advice. That advice mattered more than the test: the module's
own documentation tells every future tenant it owes a fold-equals-scan property
and points at `rebuild` to make writing one cheap. A tenant following that
pointer would write a test that cannot catch the bug class it exists for. The
fix renames the test to the composition property it actually pins, keeps it, and
marks the independent one as the oracle to copy.

The second is subtler and comes from the chaos harness — discard the accumulated
state at every legal opportunity, rebuild it from the ledger, and require that no
schedule of discards is observable. The most aggressive schedule discards at
*every* position, and it is the one that gives the least signal about the per-fact
absorb step: the state is thrown away and replaced immediately after every single
absorb, so a bug confined to that step never survives to the assertion. A
mutation confined to it passed the every-position schedule and was caught by the
sparser every-third-position one. Both schedules are pinned, and the more
aggressive-looking one is not the more diagnostic.

## The instruments were wrong more often than the code

No defect shipped in the primitive. Nine landed in the instruments measuring it,
and every one was caught by running the thing rather than by reading it.

The regression was fitted against the wrong x-axis. Facts are indexed on subject
and predicate, so a fold walks only its own agent's postings — but genesis
commits about twelve and a half thousand facts before the walk starts, so the
ledger length moved 1.55× while the actual driver moved 2.6×. Regressing on a
near-constant produced a log-log slope of 1.69 for a relationship that is close
to linear, and that number was reporting the fit's conditioning rather than the
world. It also produced the campaign's most flattering wrong diagnosis: the same
column disagreed about the *sign* across runs, which was attributed to a loaded
box. Contention was real and was not the cause; with the correct x-axis the same
column is stable across four runs at the same load. "The box is noisy" would have
justified escalating to another machine and changing nothing.

Alongside that: an affine relation with a large intercept has no single power-law
exponent at all, so the log-log helper was deleted rather than reported beside
the fit, because a wrong number that can be read off an instrument will be. A
monotonicity p-value was hardcoded to the probability of a clean sweep and
printed 0.0039 for a five-of-eight run. A column-header fix silently failed to
apply, and the table printed eight labels over ten columns with every value one
heading to the left — in a patch whose other five edits all asserted on their
targets, and the un-asserted one is the one that failed. A one-agent proxy chosen
by position had no position history at all, which is how the never-drinks finding
was stumbled into. A calibration yardstick moved nineteen per cent while a band
ran 3.5× out of line on raw wall time, because a single-threaded integer loop does
not capture what a loaded box does to a memory-bound workload. And with history
sampled from 130 upward the fitted floor extrapolated negative, so a share line
computed from it announced that the history term was 106.5 per cent of the total.

One shape recurs across them and it is the generalisable part: **an instrument's
own conditions are part of its output.** A fit's conditioning, an exclusion's
declaration, a yardstick's blind spot and a patch's application are all results,
and an instrument that does not report them will report the ones it does report
confidently and wrongly.

The same lesson has a second form, found twice. A ratio in the spec was
unreconstructable — the stated range's upper end had no derivation anywhere.
Correcting it turned up two neighbouring ranges wrong in exactly the same way,
and correcting those turned up two more. An eyeballed range is a **habit, not an
incident**, so one wrong range is grounds to audit every range written by the
same hand in the same pass. Both audits found something; the third came back
clean, which is the right answer to report rather than to manufacture.

## What is now true, and what is deliberately not

The measured position is that the read side exists and has no tenant. Stage 7 of
the parent program was carveable in two incompatible ways for as long as it
existed — abstain at the commit site, or compact after it — and this campaign
argues the ordering between them is forced rather than chosen. Compaction cannot
be safe while anything folds raw history, which is this campaign. And between
them sits a step nobody had named: each step's `provenance` is authored prose —
*"went down to the river it knew (thirst)"*, *"fled the uncanny ground (fear)"* —
and it is rendered, in the historiography window and in the REPL. Dropping steps
before the intention carries its own compositional *why?* would delete readable
content. That makes it a fidelity cut and a separate decision, not an
optimisation.

So what shipped is the primitive, its ten pinned properties, and the two
instruments that size the problem — and none of the six folds has been migrated
onto it yet. That half was held off by a concurrent campaign rewriting the same
functions to put the world's clock on exact integer ticks; it landed while this
one was closing, and the way is now clear. The honest headline is that nothing
got faster. What changed is that the argument for making it faster is now a
measurement with a number on it, in two independent forms, one of which nobody
had predicted.
