# The Begat — retrospective

**Merged:** 2026-08-16 · **Program:** census cost, following The Glasshouse's epoch

## The brief's hard part dissolved, and its easy part was the trap

This campaign arrived with its diagnosis already done and a warning attached:
the rewrite must be proven byte-identical, and *"today's `ancestry` has a cycle
guard that a naive depth map would not reproduce — establish whether cycles can
occur before assuming they cannot."*

That framing put the work in the wrong place. Establishing whether the founding
data can contain a cycle is an empirical question about a generator, answerable
only by reading every path that writes `occ-founded-from` and trusting that
reading to stay true. It was never needed. Choosing a **downward walk** instead
of a depth map makes the equivalence structural: each node has at most one
parent, so the guard's stop-on-repeat means the ancestry walk enumerates each
reachable ancestor exactly once, which makes upward membership and downward
reachability the same predicate. The proof does not mention cycles because it
does not depend on them.

**The lesson is about the shape of a proof obligation, not about lineage.** A
blocker phrased as *"establish that the data never does X"* invites an audit of
the producer. Ask first whether the consumer can be written so that X does not
matter. A structural equivalence survives a change to the generator; an audit
of the generator does not.

Meanwhile the part of the brief nobody flagged — the projected fix, *"a
`children`/`depth` map built once"* — was the actual hazard, and it was written
into the one place a future reader would go to find out what was owed. Half a
correct prescription reads exactly like a whole one.

## Every claim I wrote about my own mechanism needed running, and one was false

Three of this campaign's claims were checked by execution rather than review,
and the ratio is the finding: **one of the three was wrong, and it was the one
that felt most like understanding.**

- I wrote into the shipped doc comment that breadth-first order was
  load-bearing — that a depth-first walk "would report whichever `k + nL` it
  happened to walk" on a cycle. It sounded like insight. I then ran a
  depth-first variant against the shipped one over every test shape:
  **0 differing pairs.** The sentence was true about an irrelevant property;
  the single-parent invariant makes the downward path unique, so traversal
  order cannot matter. Corrected in place, with the measurement recorded next
  to it.
- The detail that *is* load-bearing — seeding the visited set with the query
  node — I confirmed by removing it: all three equivalence tests go red, on the
  cyclic shapes specifically.
- I flagged a second function as reintroducing the same quadratic, then read it
  properly and withdrew: it needs the ordered *path*, not just a hop count, so
  the walk there is not redundant. Withdrawing a flag costs a paragraph;
  shipping a "fix" to code that did not need one costs a review cycle and a
  behaviour risk in someone else's campaign.

**Generalisation:** a mechanism claim you are confident enough to write into a
doc comment is exactly the one to run a falsifier against, because confidence
is what stops anyone else checking it.

## An empty diff needed a positive control, and got one

The whole correctness case was a comparison returning zero differences over a
thousand seeds. An empty diff and a broken comparison are the same observation.
So the comparison was made to fail on command before its zero was read:
perturb one value, confirm exactly one disagreement, then trust it. The column
was also checked for triviality — ten distinct values, no empties — since a
constant column matches a constant column.

This is the open-questions bet's own clause arriving on a new path, and worth
recording as a confirming instance: *a negative result from an instrument
nobody has seen fire is not evidence.*

**And the property did not inherit.** Byte-identity established on this branch
said nothing about the object produced by joining it to The Retelling's, whose
changes touched the same derivation. It was re-established, in full, on the
joined result. A proof about a branch is not a proof about what lands.

## Measuring one metric without a per-metric instrument

There is no per-metric timing in the lab and adding one would mean putting a
clock into a determinism-critical path. The substitute cost nothing and is
reusable: **run two studies over the same seed panel — one selecting only the
metric under test, one selecting a single cheap ledger-scan metric — and
difference them.** Both build identical worlds, so the build cost cancels and
the remainder is the metric.

Two details that made it trustworthy:

- **The control is the evidence, not the treatment.** The control moved 91.66 →
  91.65 CPU-seconds across the change. That near-zero is what licenses
  attributing the treatment's 68-CPU-second fall to the metric rather than to
  the box, and it is the number to report first.
- **Report CPU-seconds, and state the load.** The box ranged from load 4.5 to
  73 across this campaign, entirely from other sessions. Wall time was
  meaningless; CPU-seconds held 0.9–1.8% spread at low load and 4–6% at high.
  A measurement taken at load 14.7 was still usable for a 68-second effect and
  useless for a 0.9-second one — so the second was re-taken at load 4.5 with
  ten interleaved repetitions.

**A 1.8σ result was reported as a bound, not a detection.** The joined
structures measured +0.88 ± 0.50 CPU-seconds over forty worlds. That is
suggestive and not established, and it was the resolution's central question,
so calling it a clean zero would have been the convenient error. It is stated
as "either a small real cost or still zero, bounded under 1.5% of what was
removed" — and it agrees with an independent accounting from the other campaign
(~6,200 node visits per world).

## A cost claim was wrong twice over, and the second way is the structural one

The metric's registered comment claimed ~0.16 s/world "well under the 0.5
s/world KEEP threshold". Measured: 1.708 CPU-s/world, **10.7×** the claim. And
the threshold does not exist — "KEEP threshold" appears nowhere in this
repository except the sentence citing it, and the file it cites contains no
occurrence of "threshold" at all. A claim invented both its measurement and its
standard, and nothing could ever have contradicted it.

**The transferable half is that a scalar was the wrong *shape*.** The cost was
superlinear in a quantity the world grows, so "per world" has a denominator
that moves. A correct 0.16 would have rotted too. This is the same failure as a
committed baseline being a claim with a date, one axis over: the number was not
merely stale, it was a *type* that cannot express the thing it measures.

Filed rather than fixed: a metric has nowhere structural to record its cost —
the registry entry carries name, doc, summary, domain, role and extractor, and
no cost field — so the claim lands in a comment, and comments do not fail. The
durable fix is to emit per-metric cost as a drift-checked artifact from the
census that already sweeps every metric over every world, which is the same
promotion the type-audit report and the seam-guard roster have already had.

## Two checks, one mechanism: measuring a set you do not own

This campaign found gate blindness from the inside — a green commit gate over
1,281 tests containing zero tests from the crate being changed — while the
merge queue's operator found the same hole from the outside. Both instances,
and the cost claim above, share a shape worth naming: **the check was measuring
a set it did not own.** The commit gate does not own its roster; the per-world
cost claim did not own its denominator. Neither check was broken, and neither
could fail, because the question had changed underneath it. Two instances is
not a law, and it is recorded as an observation with its sample size stated.

## Process notes

- **`git add -A` cannot tell a regenerated artifact from scratch output**, because
  `make rebaseline` and a scratch study run both write into
  `book/src/laboratory/generated/`. Eight probe files landed in a commit this way
  and were removed in the next. Stage generated directories by explicit path.
- **A campaign handed a diagnosis should verify it, and this one held.** The
  attribution (real work, not contention — `cpu_ratio` 32.10 → 32.35 while CPU
  rose 12.1%) was confirmed from the committed ledger before any code was
  touched. Verifying cost minutes; inheriting it would have risked the whole
  campaign being pointed at the wrong column.
- **A blocked instruction is not a failed one.** The brief asked for the census
  budget constant to be ratcheted 1050 → 900 as part of this work. It was
  deliberately left alone: the ratchet is settled by a census reading on the
  canonical box *after* this lands, and the latest row is still the pre-fix
  979.539 s, so moving it here would only have reddened the tripwire. The
  condition was restated in both directions where the constant lives.
- **The new tests are still gate-invisible.** The crate's roster coverage was
  restored by the campaign this one merged with, but tests added *by* this
  campaign have no recorded baseline duration and are therefore excluded from
  the commit gate by design. They enter it when the roster is next rewritten —
  which is the mechanism's own first execution.
