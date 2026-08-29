# 0426. The heavy tier is a phase of the queue again

**Status:** Accepted (2026-08-28) · **Decider:** Nathan · **Amends:**
[0148](0148-a-merge-runs-four-phases-and-probes-run-by-hand.md)'s phase list ·
**Relates:** [0086](0086-the-heavy-tier-runs-on-the-canonical-box.md),
[0132](0132-three-gates-named-for-the-campaign-moment.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md),
[0139](0139-main-advances-only-through-the-lock.md)

In the context of a heavy tier that has cost 3.45x less since The Governor cut
it, and that spent nine days red because nothing dispatched it, we decided that
**`heavy` returns to the chamber's MERGE phase list — `artifacts outboard gate
clients heavy`, and the stage list is left alone** — accepting that a merge
grows from ~1100 s to ~1550 s (+41%) on the one strictly serial box, paid by
every campaign in the queue behind it.

`seam-guard` does **not** come back, and 0148's other two rulings — the `probe:`
token class and the three batteries moved into it — stand untouched. This
amends exactly one clause of one ruling.

## The measurement, and why it licenses reopening 0148

Two heavy runs on lefford, before and after this campaign's cost work:

```text
                       BEFORE (2f8faf243)   AFTER (da03b576a)
nextest wall             1551.631 s           449.219 s      3.45x
timed.sh wall            1622.132 s           499.572 s
tests                      118                  63
FAILURES                    10                   0
cpu_ratio                   13.88                20.87
```

Provenance: `/tmp/hornvale-heavy/runs.tsv` and the `heavy-*.log` files under
`/tmp/hornvale-heavy/` on lefford; the BEFORE run is
`heavy-20260828T145124Z-548769.log`. The AFTER run is the first green heavy run
since 2026-08-16.

A four-phase merge on the queue currently costs about 1100 s. Summing the
`sluice:*` rows in `docs/timings.md` for the four consecutive merges of
2026-08-28 gives 1129.954, 1014.162, 1227.633 and 1146.355 s; the queue's own
recent totals run 1021–1236 s. Adding a ~450–500 s `heavy` phase takes a merge
to roughly 1550 s.

**This is the fact 0148 could not have had.** 0148 measured `heavy` at a
1965.0 s mean over 21 chamber runs and `seam-guard` at 1017.5 s, together
**80.5%** of a 3704 s six-phase merge. On that evidence removing them was
plainly right, and the four-phase merges that immediately followed (630.7,
640.2, 634.5 s) confirmed it. The ratio has since moved by a factor 0148 had no
way to anticipate: `heavy` is now **~29%** of a would-be 1550 s merge, not
53% of a 3704 s one. Nothing in 0148's reasoning was careless; its input
changed.

## What removing the dispatcher actually cost

0148 named the trade honestly — "a heavy-tier regression will now be found by
whoever next runs those commands". What it could not price was how long that
would be. The answer is measured, not estimated:

- The tier ran only when a human remembered, and **no bare `heavy` row reached
  `docs/timings.md` between 2026-08-05 and this campaign** — 27 runs invisible
  to the tier's own ledger. (Twenty-one `sluice:heavy` rows do exist, spanning
  2026-08-16 to 2026-08-19 — 2, 6, 6 and 7 by day. Most predate 0148's landing
  at 2026-08-19T19:10:55Z; exactly one, at 20:03:41Z, follows it, from the run
  0148's own record describes as gated under the four phases it proposed. Those
  are the *phase's* rows; the by-hand `make heavy-remote` path, which is what
  0148 left as the only dispatcher, wrote none — its rows were stranded in a
  scratch worktree, which is residual 2 below.)
- It accumulated **ten failures** that nobody saw until The Sources ran it by
  hand at close, and then nearly attributed nine of them to itself.
- Task 10 named the cause of the last one: `eeaa011fd`, The Granary's
  `history/bake/v3` epoch bump on 2026-08-24. That commit deliberately moved
  when raids fire within a year, which moved a shipped world value, which left
  `disposition_calibration::the_sub_floor_raider_reading_is_pinned_as_a_witness`
  reading `(10, 60)` against a pin of `(12, 60)` for four days. The Granary
  passed every gate it was asked to pass. It re-pinned several other witnesses
  and never saw this one, because nothing showed it.

That is the shape of the defect: **a campaign can move shipped world values,
pass every gate, and leave the tier red for the next campaign to inherit and
mis-attribute.** It is an author/inheritor asymmetry, not a cost problem.

## The four forms, decided against measurement

| form | what it fixes | why not |
|---|---|---|
| **every merge** | the asymmetry, universally | ~+41% per non-prose landing, ~45 h/month of the serial box — **chosen** |
| stage gate | the asymmetry, for campaigns that submit one | opt-in, so the failure being fixed is "nobody ran it" — and it would red predictably on the census fixture (below) |
| conditional on a world-code predicate | the asymmetry, where the predicate fires | a new mechanism blind in the silent direction; the ~10 h/month it saves is real, and is not the reason it loses |
| scheduled | nothing at merge time | `scripts/scheduled/` was written and **never installed**; standing guidance already forbids closing a campaign on "the nightly was empty" |

**The conditional form was the serious rival. It is NOT answered by
arithmetic, and the first draft of this record said it was.** Its real argument
is symmetry, not cost — but *every merge* delivers symmetry strictly more
completely, so the comparison reduces to what conditionality saves. The
population, measured over the 160 landings on `main` carrying a
`Sluice-Headline` trailer (159 inter-landing ranges, 2 empty):

```text
  100  (62.9%)  touch kernel/, domains/ or windows/   -> heavy runs under EITHER form
   29  (18.2%)  prose-only                            -> heavy skipped under EITHER form
   28  (17.6%)  neither                               -> the only ranges a predicate would exempt
```

The prose-only skip already exists (`scripts/sluice-phases.sh` drops `heavy`,
`clients` and `seam-guard` from a candidate whose every path is hand-written
prose) and applies under every form, so a conditional predicate would exempt
heavy on about **one landing in six** beyond what is already exempt.

**THE DENOMINATOR, AND THE ERROR IT CAUSED.** The `Sluice-Headline` trailer was
introduced by `2aa07bd38` on **2026-08-17**. All 160 landings therefore fall
between 2026-08-17 and 2026-08-28 — **11.3 days**, and a `--since=2026-07-01`
filter selects the trailer's entire lifetime rather than a two-month window.
The first draft normalised these counts over two months and stated the saving
as "about 2 h/month". The denominator it used was **5.4x too long** (two
months, ~60.9 days, against the 11.327 days actually spanned), so every
per-month figure in it was **~5.3x low**:

```text
  form                                        first draft   corrected (159 over 11.3 days)
  chosen (128 non-prose landings)              ~9 h/month     ~45 h/month
  conditional (100 landings)                   ~7 h/month     ~35 h/month
  saving forgone by declining conditional      ~2 h/month     ~9.8 h/month
```

**The saving being declined is roughly the size the first draft claimed for the
whole cost of the option being chosen.** A reader comparing 2 against 9 reaches
the opposite intuition from one comparing 10 against 45. This is recorded
rather than silently corrected because it is a recurring failure mode in this
project — a real command, correctly run, answering a narrower question than the
claim attached to it — and because the correction changes which argument
carries the decision.

One caveat, stated as uncertainty and explicitly **not** as a rescue: 14
landings/day over 11.3 days is a busy stretch with several campaigns running in
parallel, and it may not be the steady state. It is the only tempo actually
measured, so it is the one used; a quieter month costs proportionally less.

**So the decision does not rest on cost, and must not be read as if it does.**
Roughly 10 h/month of serial-box time is a real saving to forgo. What survives
the correction untouched is a **correctness** argument, and it is the one doing
the work:

**Seven of the 63 `heavy:` tests live in `cli/tests/suite/`**, and the tier's
harness — `.config/nextest.toml`'s serialization and sized-sweep pins,
`scripts/gate-full-heavy.sh`, `cli/tests/fixtures/heavy-roster.txt` — is
outside `kernel/domains/windows` entirely. A predicate written from the
layering diagram would exempt exactly the changes most able to break the tier,
and nothing would say so. That is a silent-direction failure in a new
mechanism, introduced to fix a campaign whose whole subject is silent gaps
accruing where nothing reports them. **The trade this decision actually makes
is ~10 h/month of the one serial box in exchange for not building a predicate
that can be wrong without saying so** — a defensible trade, and a much narrower
one than the first draft described.

**Stage gate and scheduled both lose on the same word: opt-in.** A campaign
that does not submit a stage gate gets no heavy, and a timer nobody installs
runs nothing. The observed failure is not "the check was too expensive to run",
it is "nobody ran it".

## The merge only — and the stage gate deliberately not

`heavy` goes on `merge_phases`, last. `stage_phases` is untouched. **A first
draft of this decision put it on both and called that a restoration. It was
not, and the phrase was doing real work in the wrong direction.**

**`heavy` has never been a stage-gate phase.** At `3163ceb2c^` — 0148's parent,
the last commit of the six-phase era — the two lists read:

```text
merge_phases="artifacts outboard gate seam-guard clients heavy"
stage_phases="artifacts outboard gate clients"
```

0148 shrank the merge list *down to* the stage list; it did not take `heavy`
off both. So merge-only reproduces the pre-0148 arrangement exactly, and
merge+stage would have been new scope wearing the word "restoration".

**And the stage gate is the one place this test cannot go.**
`fixture_staleness::census_fixtures_match_a_probe_of_live_seeds` is
heavy-rostered, runs a live probe over the census seeds, and compares it to the
committed fixtures by exact equality — its own panic message reads "worldgen
changed but the census fixture was not regenerated". The census is refreshed
**once per campaign, at the pre-merge close**. So a campaign that moves any
census metric would red that test on *every* stage gate from its first moved
value until close: ~475 s of the one serial box each time, for a reason that is
expected, benign, and not fixable at that moment. **A gate that reds
predictably for a known-benign reason trains people to ignore it — which is
exactly the disease this decision was written to cure.** Shipping the cure and
the disease in one change would have been a poor trade.

The merge has no such problem, and the reason is a genuine alignment rather
than luck: by merge time the census *has* been refreshed, so the cadence the
test assumes and the moment a merge occupies are the same moment.

**What is given up, stated rather than implied.** 0148's phase-list identity
goes — the two lists differ again, by exactly `heavy`. That identity was a
consequence in 0148, not its design ("differ only in the push" is a claim about
the *object gated*, the real merge product, and that is unchanged). And a
campaign can now pass every stage gate and still meet a heavy failure at merge.
The cost of that is bounded and self-attributing: one merge attempt, ~1550 s,
main untouched, the queue row `held` with the failing phase named, and
`make heavy-remote REF=<sha>` available to anyone who wants the answer sooner.
`scripts/test-sluice.sh` asserts the divergence is exactly `heavy` — "stage plus
heavy equals merge" — so it cannot widen unnoticed.

## This changes what the roster ratchet means

`cli/tests/fixtures/heavy-roster.txt` and
`heavy_tier.rs::the_heavy_roster_is_exactly_this_fixture` were built this
campaign to close the gap the spec's §4 names — that the `heavy:` tag was
**unpriced**, so any campaign could add a 900-second battery and nothing would
charge it. Under 0148's phase list the ratchet could only ever make an addition
*visible*: a tag nobody's gate ran cost nobody anything.

Under this decision it prices one. Appending a line to that fixture now charges
every subsequent merge for the test it admits, on the one serial box, forever. That is the coupling §4 wanted and could not
have while the tier ran by hand. The ratchet is checked in both directions, so
a removal is equally deliberate.

**It still prices membership, not duration** — §4 chose a frozen roster over a
wall-clock budget on purpose, because a committed baseline is a claim with a
date and roster membership does not decay. The consequence is stated under
residuals below.

## Residuals this decision knowingly ships

Named here rather than left in a task report, because each is a thing a future
reader would otherwise reconstruct wrongly.

1. **`threads-required = 30` on a 40-core box.** The `# class: sized-sweep`
   table in `.config/nextest.toml` reserves 30 of 40 slots, so its three
   members cannot co-schedule with each other (30 + 30 > 40). The 449.219 s run
   does not expose that cost — it is inside the number — and it has not been
   measured beyond it. If the sized-sweep membership grows, this is the first
   place the tier's wall time will move non-linearly.

2. **Task 7's `docs/timings.md` mirror fix is unverifiable before this merge,
   by construction.** `make heavy-remote` runs the *canonical checkout's* copy
   of `scripts/heavy-run.sh`, which is `main`'s; the dispatched ref only ever
   reaches the scratch worktree, after that script has started. So a fix riding
   on a branch cannot be exercised by dispatching it. Confirmed rather than
   assumed: after the AFTER run above, lefford's canonical `docs/timings.md`
   was unmodified and `grep -c mirror scripts/heavy-run.sh` there returned 0.
   **The first real test is the first `make heavy-remote` after this lands.**
   The gated path does not depend on it — a chamber `heavy` phase writes its
   `timed.sh sluice:heavy` row in the chamber's own worktree, which the phase
   loop commits with the merge product — so this residual bounds the by-hand
   path only.

3. **The `clients` phase has no roster ratchet.** It is now the largest phase in
   a four-phase merge (488–556 s in the 2026-08-28 rows, above `gate`), and The
   Quadrat added roughly 100 tests and +53% CPU-seconds to it in a single merge
   with nothing remarking on it. §4's argument about an unpriced tag applies to
   it word for word. This campaign did not fix it.

## What this costs, stated rather than implied

Every non-prose **landing** pays ~450–500 s more on a strictly serial box —
call it ~475 s, between the tier's own 449.219 s nextest wall and the 499.572 s
`timed.sh` wall that includes its build, since the chamber reaches `heavy` with
a tree the `gate` phase has already warmed. The cost falls on bystanders in the
queue, not only on the campaign that caused it.

At the tempo measured over 2026-08-17..28 — 159 landings in 11.327 days, of
which 128 are non-prose: **14.0 landings/day overall, 11.3 of them non-prose**
— that is roughly **45 h/month** of serial-box time, against ~35 h/month
under the conditional form that was declined — so the saving forgone is about
**10 h/month**. Stage gates contribute **nothing** to these figures and
correctly so: they never land, and `heavy` is deliberately not one of their
phases.

0133's accepted cost — "a stage gate queuing behind an hour of heavy or census
work is an accepted cost, not a bug" — is what absorbs this; submission is
asynchronous, so the wait costs queue position, not attention. But 45 h/month
is a standing commitment on a box that also serves stage gates, censuses and
by-hand heavy runs, and it is stated here at its true size precisely because
the first draft of this record stated it at a fifth of that.

**One thing this buys back, found while checking the change rather than
argued for it.** `book/src/laboratory/` is a declared path in
`docs/generated-paths.txt`, but nothing in `scripts/regenerate-artifacts.sh`
writes the `the-history/` subtree — `history_battery` does, and it is
`heavy:`-tagged. Under 0148 the drift check over that subtree was therefore
**vacuous**: no phase regenerated it, so `git diff --exit-code` compared it
against itself forever while the artifact rotted. Restoring `heavy` to the
merge list makes that check mean something again. The safety of letting it
author on every merge was checked rather than assumed: `history_battery` is the
tier's only committed-artifact writer, nothing reads `the-history` as a
reference so there is no self-ratifying loop, its preregistered floors assert
*before* the write so an out-of-floor movement reddens the phase instead of
landing, and auto-commit is the established `authors=yes` contract every other
authoring phase already runs under.

**A red heavy tier now holds the box.** That is the point, and it is only
tolerable because the tier is green: 63/63 at `da03b576a`, the first green run
since 2026-08-16. A tier gated from a red base would blame each landing for its
predecessor's failure — precisely the mis-attribution this decision exists to
end. **The window is the argument as much as the arithmetic is**: a green tier
can be gated where a red one cannot, and nothing keeps it green except gating
it.

**The strongest arguments against this that survived.** Two, and the second
only became visible once the denominator was fixed.

*The affordability number is the one thing nothing ratchets.* The roster
freezes membership, deliberately (§4 chose that over a wall budget because a
committed baseline is a claim with a date and membership does not decay), so a
single admitted test may cost minutes. If the tier drifts back toward 1500 s
this decision's arithmetic inverts and 0148's holds again.

*And ~10 h/month of the one serial box is a real thing to decline.* The
conditional form is not free of merit; it is rejected because its predicate
would be blind in a silent direction, not because its saving is small. If
someone later builds a predicate that keys on the tier's own harness as well as
on world-generating code — and can demonstrate the blind spot closed — the cost
argument would be theirs to make and this record should not be read as having
foreclosed it.

Neither is a reason to decline now. Both are reasons to revisit with the same
discipline, on a fresh measurement, rather than on the memory of this one.

## See also

- Spec `docs/superpowers/specs/2026-08-28-the-governor-design.md` §1 (the
  life-cycle joint), §4 (the unpriced tag), §7 (the four forms).
- `scripts/sluice-run.sh` — the two phase lists; `scripts/lane-sets.tsv` — the
  set roster; `scripts/sluice-phases.sh` — the prose-only skip.
- `cli/tests/fixtures/heavy-roster.txt` and
  `cli/tests/suite/heavy_tier.rs::the_heavy_roster_is_exactly_this_fixture`.
