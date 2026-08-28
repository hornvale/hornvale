# 0426. The heavy tier is a phase of the queue again

**Status:** Accepted (2026-08-28) · **Decider:** Nathan · **Amends:**
[0148](0148-a-merge-runs-four-phases-and-probes-run-by-hand.md)'s phase list ·
**Relates:** [0086](0086-the-heavy-tier-runs-on-the-canonical-box.md),
[0132](0132-three-gates-named-for-the-campaign-moment.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md),
[0139](0139-main-advances-only-through-the-lock.md)

In the context of a heavy tier that has cost 3.45x less since The Governor cut
it, and that spent nine days red because nothing dispatched it, we decided that
**`heavy` returns to the chamber's phase list — on the merge and on the stage
gate alike, `artifacts outboard gate clients heavy`** — accepting that a merge
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

- The tier ran only when a human remembered, and **no `heavy` row reached
  `docs/timings.md` between 2026-08-05 and this campaign** — 27 runs invisible
  to the tier's own ledger.
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
| **every merge** | the asymmetry, universally | costs the queue ~+41% per non-prose landing — **chosen** |
| stage gate | the asymmetry, for campaigns that submit one | opt-in; the failure being fixed is "nobody ran it" |
| conditional on a world-code predicate | the asymmetry, where the predicate fires | a new mechanism that fails silently; and it buys little |
| scheduled | nothing at merge time | `scripts/scheduled/` was written and **never installed**; standing guidance already forbids closing a campaign on "the nightly was empty" |

**The conditional form was the serious rival, and it was answered by
arithmetic.** Its real argument is symmetry, not cost — but *every merge*
delivers symmetry strictly more completely, so the comparison reduces to what
conditionality saves. Measured over the 160 landings on `main` carrying a
`Sluice-Headline` trailer since 2026-07-01 (159 inter-landing ranges, 2 empty):

```text
  100  (62.9%)  touch kernel/, domains/ or windows/   -> heavy runs under EITHER form
   29  (18.2%)  prose-only                            -> heavy skipped under EITHER form
   28  (17.6%)  neither                               -> the only ranges a predicate would exempt
```

The prose-only skip already exists (`scripts/sluice-phases.sh` drops `heavy`,
`clients` and `seam-guard` from a candidate whose every path is hand-written
prose), and it applies under every form. So a conditional predicate would
exempt heavy on about **one landing in six** beyond what is already exempt:
roughly 28 × 475 s ≈ 3.7 h of queue time over two months, about **2 h/month**.

Against that saving, the predicate's failure mode is silent and its blind spot
is concrete: **seven of the 63 `heavy:` tests live in `cli/tests/suite/`**, and
the tier's harness — `.config/nextest.toml`'s serialization and sized-sweep
pins, `scripts/gate-full-heavy.sh`, `cli/tests/fixtures/heavy-roster.txt` — is
outside `kernel/domains/windows` entirely. A predicate written from the
layering diagram would exempt exactly the changes most able to break the tier,
and nothing would say so. This campaign's whole subject is silent gaps
accruing where no mechanism reports them; buying two hours a month by adding
one is a poor trade.

**Stage gate and scheduled both lose on the same word: opt-in.** A campaign
that does not submit a stage gate gets no heavy, and a timer nobody installs
runs nothing. The observed failure is not "the check was too expensive to run",
it is "nobody ran it".

## Merge and stage alike, not merge only

0148 made the merge and the stage gate identical — "the stage gate is this
script with the push turned off" — and that property is worth more than the
stage gate's cost saving. If only the merge ran `heavy`, a campaign could pass
every stage gate it submitted and then discover a heavy failure at the single
most expensive moment available: holding the serial box, in the queue, with
other work behind it. Both lists get `heavy`, appended last, which is exactly
where 0148 found it (`artifacts outboard gate seam-guard clients heavy`) and
for the reason recorded there: a phase that costs half the run goes after the
cheap phases that might have gone red first.

## This changes what the roster ratchet means

`cli/tests/fixtures/heavy-roster.txt` and
`heavy_tier.rs::the_heavy_roster_is_exactly_this_fixture` were built this
campaign to close the gap the spec's §4 names — that the `heavy:` tag was
**unpriced**, so any campaign could add a 900-second battery and nothing would
charge it. Under 0148's phase list the ratchet could only ever make an addition
*visible*: a tag nobody's gate ran cost nobody anything.

Under this decision it prices one. Appending a line to that fixture now charges
every subsequent merge and every subsequent stage gate for the test it admits,
on the one serial box, forever. That is the coupling §4 wanted and could not
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

Every non-prose landing pays ~450–500 s more on a strictly serial box, and the
cost falls on bystanders in the queue, not only on the campaign that caused it.
Over the last two months' landing rate that is roughly **9 h/month** of queue
time, against about 7 h/month under the conditional form that was declined.
0133's accepted cost — "a stage gate queuing behind an hour of heavy or census
work is an accepted cost, not a bug" — is what absorbs this; submission is
asynchronous, so the wait costs queue position, not attention.

**A red heavy tier now holds the box.** That is the point, and it is only
tolerable because the tier is green: 63/63 at `da03b576a`, the first green run
since 2026-08-16. A tier gated from a red base would blame each landing for its
predecessor's failure — precisely the mis-attribution this decision exists to
end. **The window is the argument as much as the arithmetic is**: a green tier
can be gated where a red one cannot, and nothing keeps it green except gating
it.

**The strongest argument against this that survived.** The number that makes
this affordable — 449 s — is exactly the quantity nothing ratchets. The roster
freezes membership, deliberately, and a single test admitted through it may
cost minutes. If the tier drifts back toward 1500 s, this decision's arithmetic
inverts and 0148's holds again. That is not a reason to decline now; it is a
reason to revisit with the same discipline, on a fresh measurement, rather than
on the memory of this one.

## See also

- Spec `docs/superpowers/specs/2026-08-28-the-governor-design.md` §1 (the
  life-cycle joint), §4 (the unpriced tag), §7 (the four forms).
- `scripts/sluice-run.sh` — the two phase lists; `scripts/lane-sets.tsv` — the
  set roster; `scripts/sluice-phases.sh` — the prose-only skip.
- `cli/tests/fixtures/heavy-roster.txt` and
  `cli/tests/suite/heavy_tier.rs::the_heavy_roster_is_exactly_this_fixture`.
