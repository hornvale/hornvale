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
  saturating.** Two of its assertions are millisecond budgets and both failed on
  the canonical box during this run: scene genesis at 13187 ms against a 13000 ms
  ceiling, and a possessed turn at 9.5 ms against 8 ms. Neither is a regression —
  the same genesis measures **4829 ms on a quiet machine** against the same
  ceiling, with the box at loadavg 20 running the very battery whose timings it
  was recording.

  **A first draft of this entry claimed the project had no answer for that. It
  does, and the correction belongs here rather than in a quiet edit.**
  `session_cost.rs`'s own module doc says: *"Read a red run as contention before
  suspecting the code… `scene_cost.rs`'s documented failure mode — all metrics
  inflating together by roughly the same factor — is the machine, not a
  regression. A real regression is local."* That is a real discriminator and it
  was written down before this campaign existed.

  **CORRECTION (The Assize, 2026-08-08): the claim above — that `scene_cost`
  asserts on the first budget it checks and so never measures the four metrics
  that would settle it — was wrong, and had never been checked against the log
  it was written about.** `cli/tests/scene_cost.rs` takes all five measurements,
  prints all five (`:319-325` before The Assize's edits below moved the lines),
  and asserts only afterwards (`:327-349`). The very heavy-run log this bullet
  describes has all five, printed before any assertion ran
  (`heavy-20260808T163452Z-442429.log`, lefford):

  ```text
  genesis              13187.1 ms (budget 13000)
  SceneContext::build    1277.5 ms (budget 2700)
  tiles(512)+json       4207.4 ms (budget 8700)
  small docs+json          2.6 ms (budget 5.2) [12712 B]
  region per tile        266.7 ms (budget 420)
  test scene_api_cost_is_bounded_on_seed_42 ... FAILED
  ```

  That is the FAILED line coming *after* all five, not the panic-at-genesis
  this bullet claimed. This is the same failure this retrospective's own
  subject names — an inherited diagnosis, read as a finding without being
  tested — recurring a second and third time inside the document that first
  named it: this bullet's own "first draft… does have an answer" correction
  was itself untested, and stayed uncorrected until The Assize re-read the file.

  **The sharper finding is not that the evidence was reachable — it always
  was — but that the documented discriminator gives the WRONG answer when
  applied to it.** Read against each metric's own measured basis rather than
  its budget, the run above shows `genesis` at 2.09x while the four other
  metrics sit at 0.96–1.29x. *"A real regression is local: one or two metrics
  move and the rest hold"* — the rule this file quoted approvingly —
  classifies that shape as a regression. It is not one: a quiet box builds
  the identical world in 3947.9 ms against the same 13000 ms ceiling. The
  correct diagnosis was reached only by overruling the documented
  discriminator with a quiet-box re-measure, not by applying it.

  The rule failed because it tested the wrong property. The five metrics have
  different resource profiles: `genesis` is the only one that sculpts terrain
  across a large grid, and the other four run against a `World` and
  `SceneContext` already built in memory. A saturated runner starves the
  bandwidth-bound sculpting phase and leaves the cache-resident ones alone —
  contention here is *expected* to be local to `genesis`, not spread evenly.
  Uniformity was never the right test; it only looked right because the one
  incident on record for it happened to saturate the whole box evenly.

  `cli/tests/scene_cost.rs` and `cli/tests/session_cost.rs` now name each
  budget's basis as a constant, print a ratio-to-basis per metric, and compute
  a verdict from it: `genesis` (or `Session::start`) is the contention-
  sensitive metric, the rest are a control set, and any control moving past a
  measured tolerance reads as the code rather than the box. See
  `scene_cost.rs`'s module doc for the corrected rule and its own worked
  counter-example. No ceiling moved.
- **Two of The Delvers' three open items remain**: a third history gate still
  samples a three-order-of-magnitude distribution once at seed 42, and the
  occupancy readout's committed fixture is owed a regeneration whose
  preregistered claim must be re-checked rather than re-pinned. The census
  argument for the first is stronger now, not weaker.
