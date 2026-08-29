# The Precedence — retrospective

Three stages, all merged or submitted. Process lessons, not product; the
product is decision
[0376](../decisions/0376-the-ledgers-chronology-is-not-the-pop-order.md).

Written from the campaign's scratch ledger — 14 pre-G3 entries and 13
rulings — rather than from memory of the campaign, because the scratch is
git-ignored and dies with the worktree.

## The headline is not the defect. It is that the defect had been measured wrong twice.

The Foliot spent three attempts on a retype, watched a monotonicity invariant
break by 75 ticks, and deferred the work attributing the break to its own
change. That attribution was wrong. The inversion was a pre-existing
emission-ordering defect, and the campaign that came to fix the clock was
innocent of it.

Reproducing it took **swapping two numbers in a test fixture**. The fixture
mints entity ids in the order its masses are listed, so the list order *is*
the queue's tie-break order, and the published arrangement was the one where
the bug could not appear. On unmodified `main`, with nothing retyped, the
other arrangement produces a 9,925-tick backward jump against a one-tick
tolerance.

**Generalisable:** when a test fixture constructs the identifiers it also
depends on for ordering, the fixture's own construction is a variable. Vary
it. Three attempts reasoned about the failure; one measurement in the other
arrangement settled it in minutes.

## A tolerance is a claim, and this one was false

The invariant allowed `d >= prev - tick`, justified in-comment as sub-tick
`f64` noise between creatures tied at the same rounded tick. That describes a
real phenomenon. It was not the one occurring — the divergence is the *cost
spread* between two different actions, thousands of ticks wide.

This is why exactness did not help: the tolerance had never been bounding the
quantity that varies, so making days exact could not make it unnecessary. Two
campaigns read a 75-tick violation against a comment that said only sub-tick
disorder was possible, and both concluded the change had introduced something
new.

**Generalisable:** a tolerance names the quantity it believes varies. When a
violation exceeds it by orders of magnitude, the first hypothesis should be
that the comment is wrong, not that the change is.

## Six times I asserted something about the code from recollection

This is the campaign's real lesson and it is uncomfortable, because the defect
above *is this same error* — a check that looks correct and answers a
neighbouring question — and I committed it six times while documenting it.

| # | what I claimed | what was true | caught by |
|---|---|---|---|
| 1 | plan text contained a superseded draft | it existed only in a rejected heredoc; the file was clean | me, on reading the file |
| 2 | `grep -c 'sort_by_key'` prints `0` after deleting the call | prints `2` — the string is also in comments | the implementer |
| 3 | the tie-break guard's key was sufficient | it omitted the destination room and provenance, blind to routing regressions | the reviewer |
| 4 | a boundary-condition list lived in a test comment | it lived in a review report; no such list existed | the implementer |
| 5 | `anticipation_lead`'s `horizon` is a DURATION | it is a RATIO, already tagged `bare-ok(ratio)` | the classifier |
| 6 | "if the two fatigue shapes disagree, STOP — that would be a finding" | they disagree for **56.7%** of inputs; the branch was premised on a certainty being an anomaly | the implementer |

Five of six were caught by agents, not by me. The one I caught, I caught by
opening the file.

**#6 is the one worth generalising.** It was a *branch condition* — the very
device this project uses to avoid predictions. A branch table whose branches
rest on a false premise is still a prediction wearing a table's clothes.
Writing "if X, do A; if not-X, do B" does not verify that X is possible.

**#5 has the sharpest form:** I asserted the wrong classification *inside the
sentence warning against classifying by identifier rather than by use*. It was
caught only because the dispatch told the agent to classify by reading, and the
agent applied that rule to my claim as well as to the code. **A guard that
binds its author is worth more than one that does not.**

## The same error, one level up, in our own new code

`Ruling 11`. Four folds moved to `fold(WorldTime::GENESIS, WorldTime::max)`,
and the comments justified the change by signedness — citing decision 0126,
"negative days are legal, so a numeric zero was never the right identity for a
signed instant."

But `GENESIS` **is** `ticks: 0`. The fold is byte-for-byte the old identity and
a pre-genesis fact still reads as genesis. Behaviour was preserved, which is
correct for a retype; the prose claimed a fix that was not made.

That is the founding defect of this campaign, reproduced by this campaign,
three stages after diagnosing it. The fix was the comments, not the code.

**Generalisable:** a comment that cites a principle is asserting the code
enacts it. Check that it does.

## What the stop conditions bought

Three tasks existed only to be allowed to fail. All three earned their place:

- **Task 1.1** (the closed-window premise, written *before* the sort) passed —
  but it could have refuted the design, and writing it first is why that would
  have been cheap rather than a rewrite.
- **Task 2.1** (a throwaway spike, whose only product was a branch verdict)
  proved Stage 3's premise instead of assuming it. It committed nothing.
- **Task 3.1** (classification before retyping) caught error #5 above.

And the shape paid off unplanned at Stage 3, where the implementer hit a stop
condition — the golden moved by more than the predicted one tick — and stopped
instead of rebaselining. That is the entire value of the device.

## The spike's "one tick" was itself a masked measurement

Stage 2 reported the golden moving by exactly one tick, `1206633 → 1206634`.
The Foliot had reported the same. Both were reading **the first row of a
staircase**: `assert_eq!` panics on first mismatch, so neither could see past
fact 14. The real profile is 66 of 80 rows moving — 0 ticks, then +1, then +2.

CLAUDE.md already documents this shape ("the reporter can only ever name the
first"), and it still defeated two successive campaigns' measurements. It is
apparently not enough to know about it; the collect-all-diffs instrument has to
be reached for *by default* when a golden moves, not after the first number
looks explicable.

## A plausible mechanism, refuted by arithmetic

The implementer explained the staircase as drift removal: an accumulating float
clock's error grows with the number of charges, so a staircase is the expected
shape. The *shape* of that argument is right. The magnitude is impossible:

```
f64 ULP at day 36.7            = 7.11e-10 ticks
worst-case drift, 80 charges   = 2.84e-08 ticks
needed to move a rounded tick  = ~0.5 ticks
```

Short by a factor of 1.76e+07. The real cause was quantization *placement* —
`hold_step`'s jump is exactly `N + 2/3` ticks, so snapping each jump to the
lattice shifts the clock by `k - round(2k/3)`. Proven exclusively: a variant
carrying the discarded remainder reproduces all 80 old values.

I nearly accepted the wrong story. It was internally coherent, told confidently,
by an agent that had been right about everything else. **A ULP estimate is
cheap and settles this class of question outright** — it should be the reflex
whenever "floating-point drift" is offered as an explanation for a discrete
move.

## The agents were better than the instructions four times

Worth recording because it is the opposite of the usual failure mode:

- refused my predicted `grep` output and substituted an exact-string match;
- looked for a list I had described, did not find it, and reported that rather
  than inventing one to append to;
- measured before choosing on the fatigue alignment, discovering my branch
  condition could not fire;
- verified a new pin by **negative control** — reverting the fix, confirming
  deterministic failure, restoring, diffing against a saved copy — unasked.

The reviewers matched it: one re-derived every seam site from the code rather
than trusting the classification table; one performed the negative control
itself rather than trusting the report; one found the tie-break key lossy in
exactly the direction it existed to detect.

**Generalisable:** dispatches that name the *property* and leave the
*instrument* to the implementer outperformed dispatches that prescribed the
command. Every one of my six errors was in a prescribed instrument or a
prescribed fact; none was in a named property.

## Process friction worth recording

- **The sluice skill is stale in a misleading way.** It documents a subject
  check and describes the merge headline as *inferred*; the live script
  **refuses** without an authored `Sluice-Headline:` trailer. Cost: one refused
  submission, one extra commit, one extra push. Corrected on this branch.
- **I had that answer in memory and did not open it.** The
  `hornvale-sluice-mechanics` memory states the trailer rule and its
  final-block requirement exactly, and its index line was in context from
  session start. I read the skill, acted on the skill, was refused. Reading the
  fuller source I already held would have cost one tool call.
- **Landing stage 1 mid-campaign was right.** Its absorption cost a real
  conflict (a generated decision index, resolved by regeneration) and a 311-second
  gate. Carrying that through two more stages would have paid it twice more.
- **Golden-pin discipline held**; no pin was deferred to close.

## Deferred, each with a home

- `TOOL-hold-step-lattice-snapping-biases-the-clock` — the systematic +1 tick
  per 3 Hold jumps that Nathan accepted deliberately, with the cost named.
- `TOOL-fatigue-pin-restates-rather-than-calls` — the new fatigue pin's mover
  side is a hand-copied literal of `decide_step`'s formula, not a call into it,
  so the two could drift without either reddening. Disclosed in the test's own
  comment; the weakest of the campaign's guards and should not be counted equal
  to the others.
- `TOOL-worktree-freshness-script-syntax-error` — errors on every
  `make worktree-take`; the worktree is still created correctly.
- `TOOL-vessel-begin-complete-event-queue` — the discrete-event-simulation
  structure deferred behind a named trigger, with
  `the_queues_tie_break_decides_nothing_but_order` as its tripwire.
- **`fatigue_at`'s fold** is now a fifth `GENESIS`-identity site without the
  behaviour-preserving disclaimer the other four carry. Cosmetic; noted so a
  reader comparing the five is not puzzled.
- **`position_at`'s `f64` day** is the one INSTANT surface the retype left
  untouched without an explanation in the code. Pre-existing and lossless, but
  the campaign's own rule applies to it and nothing says why it is exempt.
