# Retrospective — The Confusion

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-confusion.md): a raid readout
migrated from a thirty-world battery onto the thousand-world census, and the
battery deleted.

This was not a planned campaign. It came out of the last three open items on The
Delvers' close, and every one of its lessons is about how a wrong belief
travelled.

## An inherited diagnosis is a hypothesis, and a diagnosis in a `panic!` is the
## most persuasive one there is

The failing guard's message asserted that `Ended::By(raider)` no longer named
exactly one occupation record. That sentence had never been measured. It was
written when the guard was *built*, describing the failure its author expected to
one day catch; when the guard finally fired it fired for an unrelated reason and
reported the expected one.

**It was believed three times before anyone tested it.** It went into a merge
report as an established finding. It then survived an idea-generation pass that
built an elaborate structure on top of it — a confusion limit borrowed from radio
astronomy, a limit-of-quantitation analogy from analytical chemistry, a
recommendation to abandon per-raider attribution in favour of distributional
statistics. All of that was careful reasoning about a defect that did not exist.

The measurement that settled it took one probe and about a hundred seconds. The
gap was 381 records; the victims outside the frozen population were 381 records.

The rule that would have caught it earlier is not "distrust error messages" —
error messages are usually right, which is exactly the problem. It is narrower:
**when a failure message explains its own cause, ask who measured that.** A
message written at build time is a prediction. A message written after a
diagnosis is a finding. They are indistinguishable in the source, and the
difference is the whole thing.

This project has now recorded this lesson three times, and the third instance was
in the retrospective of the very campaign whose test message misled this one.

## A repair that restores correctness says nothing about whether the thing should exist

Having diagnosed the defect correctly, the immediate next move was to propose a
two-line fix: freeze the denominator to match the frozen numerator, restore the
green, move on. It was correct. It was also useless — the preregistration those
readouts served had been discharged campaigns earlier, so the repair would have
carefully restored a thirty-world computation that re-answers a settled question
forever.

That was caught by a one-line question from outside — *why don't we change that
to a census test?* — and not by the person holding all the context. Having the
diagnosis in hand actively worked against asking the larger question, because the
diagnosis felt like progress.

**Debugging pulls toward preserving the thing containing the bug.** The check to
add: once a defect is understood, before repairing it, ask what the guard is
still buying. A discharged preregistration buys nothing, and its cost is paid on
every run.

## The frozen population had one honest half and one dishonest half

The retired file froze its measurement population deliberately and documented
why, in strong terms: widening it *"would silently change what every assertion
below measured, which is the exact failure a frozen population must never
suffer."* That reasoning is right.

But it froze only the numerator. The denominator was a world-wide tally kept
equivalent by a stated premise rather than by a filter. So the freeze was real on
one side and notional on the other, and the notional side is where it broke.

**Freezing a population means freezing both sides of every comparison that uses
it.** A premise that keeps two quantities comparable is a filter you have decided
not to write down, and it expires without telling you.

## What the small sample could not have told us

Before the census ran, a twelve-world probe put the victim rate at 0.014-0.373
and the raids-per-raider ratio at 1.00-1.03. The census reads 0.000-0.450 and
1.0000-1.1086 — **both tails missed**, including the existence of worlds where
nobody raids at all, of which there are three in a thousand.

Worth stating because it is this migration's own argument arriving as evidence
rather than as a rationale, and because the probe was not careless. Twelve worlds
cannot resolve a three-in-a-thousand event; no amount of care changes that. When
the deliverable is a distribution, sample size is not a cost-quality tradeoff, it
is a question of whether the instrument can see the thing at all.

## What went well

- **The probe froze both branches before running**, with different predicted
  numbers rather than different stories, so the result could not be read after
  the fact as having predicted whichever happened.
- **The probe was kept**, and for a stated reason rather than as a memento: it is
  the only place the ledger-side proxy is checked against the bake's own counter,
  which no census metric can reach because the tally is discarded before a metric
  ever sees the world.
- **Three columns, not six.** Each new census column is permanent cost on every
  future regen. The drawn-disposition and gate-open columns were declined because
  they re-derive a private draw serving a discharged question, and the
  raids-initiated count was declined because it is arithmetically identical to
  the victim rate at world scale.
- **Every new metric was mutation-proved before it was trusted** — the fold is a
  pure function over records, and three separate mutations each produced a real
  RED on assertions rather than a compile error.

## Follow-ups

- **The offence column is nearly a rescaling of the defence column** (median
  ratio 1.02) and its whole value sits in the tail. If a later campaign gives
  raiders multiple targets, that ratio is the number to watch; if it stays at
  1.02 indefinitely, the column is a candidate for retirement on the same
  reasoning that retired the battery.
- **`BakeCensus` is unreachable from a census metric.** `History::tally` is
  discarded after the history is emitted, so the census-wide invariant can ask
  whether each reference resolves but never whether the set of them matches what
  the bake counted. That second question is why the probe survives, and closing
  the gap properly would mean committing the tally.
- **The heavy tier runs wall-clock cost ceilings on a box the heavy tier is
  saturating.** Two of its assertions are millisecond budgets, and both failed on
  the canonical box during this run: scene genesis at 13187 ms against a 13000 ms
  ceiling, and a possessed turn at 9.5 ms against 8 ms. Neither is a regression.
  The same genesis measured **4829 ms on a quiet machine** against the same
  ceiling — the canonical box was at loadavg 20, running the very battery whose
  timings it was recording. One of the two had already been failing on two
  unrelated branches, which is what a structural false positive looks like from
  the outside: everybody's problem, nobody's fault, and quietly discounted every
  time. This is the same blind spot the suite-timing alarm documents for itself
  (it cannot see ordinary load), reappearing in a tier that has no such guard at
  all. A cost ceiling either needs a quiet box or needs to stop being wall-clock.
- **Two of The Delvers' three open items remain**: a third history gate still
  samples a three-order-of-magnitude distribution once at seed 42, and the
  occupancy readout's committed fixture is owed a regeneration whose
  preregistered claim must be re-checked rather than re-pinned. The census
  argument for the first is stronger now, not weaker.
