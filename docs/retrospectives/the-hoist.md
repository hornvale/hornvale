# Retrospective — The Hoist

One page of process, not product. The product is chronicled; this is what
the campaign learned that the code does not record.

## Grep-reading a god-file predicts control flow badly — in both directions

The plan was written against `windows/worldgen/src/lib.rs` read by `grep`
and `sed`, because at ~6k lines it does not fit in a reading. It made two
predictions about the function it was about to change, and got both wrong,
in opposite directions:

- It predicted **three** return sites. There are **four** — the
  `Settlements` rung has its own early return that the greps never
  surfaced.
- It predicted a **borrow trap** at the final return (`geo` borrows
  `terrain` at :4756, so moving the terrain out might not compile) and
  flagged the remedy inline. Non-lexical lifetimes end the borrow before
  the move; the trap does not exist.

Cost of the false negative: one compiler error, one edit, thirty seconds.
Cost of the false positive: a paragraph of plan nobody needed. **The
compiler is the cheap oracle for this class of question, and the plan should
lean on it rather than trying to out-read it.** The useful move is not more
careful grepping — it is writing the change and letting `cargo` enumerate
the sites, then folding what it found back into the plan.

## Widening a return type is a free audit of every call site

The campaign's stated target was the lab view chain. Changing `build_to`'s
return type forced every caller to be visited, and one of them —
`history_for`, inside worldgen itself — turned out to have the identical
defect: build to terrain depth, then immediately re-sculpt with
`terrain_of`. Nobody had noticed, because from the outside it just took a
while.

This is worth generalizing. A breaking change is normally counted as pure
cost, and the additive-sibling pattern exists to avoid it. But the *audit* a
breaking change compels is a discovery mechanism: it visits exactly the set
of places that use the thing you now understand is misused. The campaign got
the audit without paying the breakage, by changing the private function's
type while keeping the public wrapper's signature — the sites got visited,
the callers did not get broken. Worth reaching for deliberately next time,
not just stumbling into.

Measured consequence: the prediction was 15-20%, the result was 24.3%, and
the difference is almost entirely that third site.

## A harness bug can masquerade as a campaign-failing result

The first A/B run reported **`DIVERGED`** on the byte-identity check — the
single result that, taken at face value, kills this campaign outright.

It was two bugs in my own shell, not in the code under test. The baseline
binary's timings had been redirected into a stderr file, and the baseline
run wrote its rows into the worktree while the comparison read from the main
checkout — so `cmp` was comparing against a file that had never been
written, and reported difference. The same run also contained a contaminated
timing (56.47 s at 2454% CPU, against a clean mean of 44 s) from competing
load.

Both were caught by looking at *how* the result was produced rather than
accepting it, and the tell in each case was internal inconsistency: a "diff"
with no diff output, a timing whose CPU percentage did not match its
neighbours. **A verdict from a harness deserves the same scrutiny as a
verdict from the code — especially when it is the verdict that would end the
campaign.** Check the harness actually produced its inputs before believing
its conclusion.

## I bypassed the census lock, and only noticed from a stray process

The authorized regen was launched as `HV_CENSUS=1 bash
scripts/regenerate-artifacts.sh`. That is the command the docs quote, and it
is the wrong entry point when anyone else might be working:
`scripts/census-run.sh` takes `/tmp/hv-census.lock` so that concurrent
census runs **queue** instead of contending; `regenerate-artifacts.sh` takes
no lock at all.

The other machine had triggered a census over SSH at ~18:53, into its own
scratch worktree (`hornvale-census-wt`). Mine started at 18:56 and the two
ran on top of each other for seven minutes on the same 40 cores.

Nothing broke, and nothing could have: determinism means CPU contention
cannot change a byte, the two runs wrote to different trees, and the
zero-diff verdict is therefore sound. What it cost was a *measurement* — the
regen's census phase came in at 6m57s against this morning's 5m53s, which
reads as an 18% regression from a campaign that made the census 24% faster.
It is contention, not physics, and the only reason that is known rather than
alarming is that a `ps` run for an unrelated reason turned up a `hornvale`
process at 2668% CPU that nobody in this session had started.

**Two lessons.** Prefer `scripts/census-run.sh` over
`regenerate-artifacts.sh` for any authorized regen — the lock exists for
exactly this, and the queue costs nothing when uncontended. And when a
timing contradicts a well-measured result, suspect the box before suspecting
the code: the probe A/B (clean, alternating, disjoint sets) said -24.3% and
was right; the single contaminated regen number said the opposite and was
noise.

## The ideonomy pass earned its keep on a decision that felt settled

G1 looked like a formality: obviously the build should return what it built.
The negation operator, applied to "return the terrain", produced
*store the terrain in the World* and *memoize it* as siblings of the chosen
design — and holding those next to the constitution is what surfaced the
framing the spec then hung on: **`terrain_of` is not redundant machinery to
be removed, it is "everything else is re-derived deterministically" made
executable.**

Without that, the plausible next step was a design that made the double
sculpt impossible by making re-derivation unnecessary — which is to say, by
quietly deleting the architecture's central claim in order to speed up a
census. The pass did not overturn the decision; it produced the non-goal
that kept the decision honest. That is the documented case for running the
pass on the answers that feel obvious.

## What went smoothly, and why

No absorption drama: the branch was cut from `main`'s tip and closed the
same day, so `make preflight` was GO on the first try with zero divergence.
That is the stage-boundary cadence working by construction rather than by
discipline — worth noting because the campaigns that suffer here are the
long-lived ones, and the lesson is about duration, not diligence.

Golden-pin discipline held: no pin drifted, because nothing moved. The
zero-diff census regen is the evidence, and for a campaign that only deletes
duplicated work, "the regen changed nothing" is the entire correctness
argument rather than a formality.
