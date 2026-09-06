# Campaign The Warp — retrospective

**Close:** 2026-09-05, awaiting G6 · **Ledger:**
[`2026-09-05-the-warp.md`](../superpowers/ledgers/2026-09-05-the-warp.md)
(12 entries plus the Task 6 calibration and Task 7 readout sections) ·
**Chronicle:** [the-warp](../../book/src/chronicle/the-warp.md) ·
**Decisions:** 0776, 0777, 0778, 0779

## The headline: a 1.2-second probe, run before the spec was written, answered the question the campaign was proposed to answer

The predecessor left a registry row with an explicit fork — its falsified
legibility ordering was either a property of the world or an artefact of a
four-bin estimator — and an explicit prohibition against retuning a recipe to
settle it. The obvious campaign shape was to build a finer estimator and go
look.

Instead the first thing built was a probe: tabulate every rendered sign at
seed 42 against every kind's occurrence, over the predecessor's own
population, each reading beside a lagged null, with the predecessor's own
estimator as one row of the table for a positive control. It ran in **1.2
seconds** and it settled the fork outright. A nineteen-class biome word ranks
the thicket above the spring exactly as four bins did, and **333 of 403
springs stood on a facet with no cause at all** — a base-rate effect in the
recipe, nothing to do with the estimator.

That reordered the whole campaign. The design's §1 is *measured*, not
asserted; the campaign that would have been "build a better estimator" became
"the ceiling is a recipe property, so the recipe moves"; and the answer was
taken before any recipe had been touched, which is the only ordering under
which the answer is worth anything. Three of the campaign's four decisions
descend from that one table.

**The lesson generalises past this campaign.** The probe cost 1.2 seconds
against a campaign of eight tasks. A cheap measurement of the *premise* is not
a preliminary — it can change what the campaign is.

## Plan-text defects: seven, one per task, and near-zero in implementer code

The predecessor's retrospective recorded ten controller defects and
approximately zero implementer ones. This campaign is the same shape at
smaller scale, which is worth saying plainly rather than treating as a
coincidence: writing an instruction about code you have not opened is where
the defects come from, and it did not stop being true because the last
campaign noticed.

1. **Task 1 — an unmintable citation.** The brief's doc-comment text cited
   "decision 0778", a number this campaign would not mint until Task 8. A
   source-side ratchet fails on a cite that does not resolve, so the brief's
   verbatim text could not pass the gate. Ruling: until the numbers exist,
   Rust doc comments cite the campaign ledger entry. (The same brief's
   `assert!(STEEP_LO < STEEP_HI)` also tripped a lint against asserting on
   constants — a real check the brief had not anticipated.)
2. **Task 2 — the wrong land predicate.** The brief's test filtered by
   `is_ocean`. Ice sheets are not ocean and are also not the land arm of the
   wetness word, so the brief's filter let them into a population its own
   assertion excluded, and the test failed. The exact partition already
   existed under another name; the implementer found it by running the
   brief's literal text and reading the failure.
3. **Task 3 — a test walk that never went anywhere.** The brief's afloat test
   walked `COMPASS[(step * 3) % 8]`. Those eight direction vectors sum to
   zero, so the walk orbits its start facet forever. Two hundred steps, never
   afloat, and the test's own "did we find one?" assertion fired for the wrong
   reason — reporting on the walk rather than on the render. A single straight
   eastward walk reaches water at step 18.
4. **Task 4 — a brief that contradicted itself, and a vacuous claim.** Its
   interface block declared two accessors crate-private while its own tests
   called them from an external integration-test crate; nothing could satisfy
   both halves. Separately, the design's sentence "zero cause implies zero
   prevalence" is **true and vacuous on land**: blended products have no exact
   zeros, so the population it quantifies over is empty — 0 facets of 11,218.
   The non-vacuous statement is silence *below the lower step edge*, which
   holds over 10,600 facets for the spring and 6,044 for the overhang, and was
   killed by mutation before being trusted.
5. **Task 5 — a cost experiment that could not see its own subject.** The
   brief's A/B compared a study carrying the predecessor's metrics against one
   carrying this campaign's. Both studies build the **same shared pool**, so
   neither arm could see the pool's added cost at all; the honest measurement
   is the pre-task registry against all thirty-two new columns, and it came out
   at 0.331 CPU-seconds per world against a budget of 0.25, with an ablation
   attributing **100%** of it to one map read nobody had suspected.
6. **Task 6 — "the highest passing rung" was the wrong criterion**, and a
   between-kind bar was worse. See below; it is the campaign's best
   instrument lesson.
7. **Task 7 — a `cell`-named local helper.** In this repo `cell` means a mesh
   vertex, and a lexicon guard says so. A table-row helper called `cell` is a
   two-character defect with a real cost: the word is load-bearing everywhere
   else.

Not one of the seven was found by re-reading the brief. Five fell out of an
implementer executing the brief's own text and reading the failure; one (the
unmintable cite) fell out of the gate; one (the calibration criterion) fell out
of a measured ladder that the criterion itself had asked for. Re-reading checks
a claim against the model that produced it, and the model is what was wrong —
which is exactly what the predecessor's retrospective said, one campaign ago.

## A bar a kind can pass by disappearing is defective

The frozen predictions carried a clause requiring the spring's channel
information to exceed the overhang's. Calibration round 3 satisfied it — by
cutting the overhang's reliability to 0.16 and its frequency twelvefold, while
the spring's own reading did not move by a single digit (0.086464, in every
round of the whole campaign).

Mutual information in bits scales with the event's own entropy, so a
between-kind ordering rewards rarity in the *other* kind. The clause was
withdrawn from the gate on the calibration seed, before any measurement seed
existed. The readout later showed it would have failed on all four seeds
anyway, raw and normalised — so the withdrawal was a correction of a defective
bar, not the rescue of an uncomfortable reading, and the readout is what
proves that rather than the argument.

**The sharpest artefact the campaign produced is the confirmation, not the
failure.** At the 0.16 rung — the rung that existed only to satisfy the
withdrawn clause — the predecessor's whole falsified ordering came back
*exactly as it had been preregistered*. A prediction confirmed by making a
different kind twelve times rarer is not being tested by its confirmation.

The successor ruling has the same shape. Fix round 1 was told to take the
**highest** reliability that held every band, and chose 0.75. That criterion
was itself withdrawn: the highest passing rung is by construction the one with
the least headroom under the bound that stopped it (1.17× against the
wallpaper guard, versus 1.62× at the middle rung), so a measurement seed
running seventeen percent hot would have failed for a reason about the margin
rather than about the world. Three rungs of one constant were measured in one
day, and the middle one shipped.

**Next time: a preregistered bar must not be satisfiable by removing
something.** Ask, of every frozen comparison, "what is the cheapest way to
make this true?" — and if the answer is "shrink the other side", the bar is
not a bar.

## Both edges below one half, or the metric measures the constant

The found fraction counts occurrences standing on a cause of 0.5 or more. A
step function whose lower edge sits at or above 0.5 makes that reading
**1.000 by construction** — the metric would be measuring the authored
constant rather than the world. An edge pair of (0.45, 0.55) was available and
predicted 0.955; it was rejected on that reasoning rather than on its numbers.

This is the same family as the campaign's other instrument corrections and
worth naming as a class: **a calibration constant that sits inside the metric's
own definition can make the metric unfalsifiable.** The check is to ask what
the metric would read if the world were replaced by noise.

## Three instrument corrections, all before the readout, all with their confounds inline

Each of these would have been a rescue if made afterwards, and each is
ordinary if made before with the figures published:

- **The learner shrank toward one half.** On a three-percent event over 469
  classes that predicts nearly every thin class at fifty-fifty and pays about
  4.6 bits a miss; the spring's held-out gain read **−0.049**, an artefact of
  the prior. Shrinking toward the base rate instead makes an uninformative
  table score exactly zero and never below.
- **Two noise-control bars sat below the null's own spread.** The permutation
  null of a discrete mutual information has a standard deviation as well as a
  mean — 0.001967 bits at 469 classes — so a bar of ±0.001 would have failed
  the control about a third of the time by chance. Reset to four null standard
  deviations, frozen from the calibration seed. The χ² prediction of the
  null's *mean*, 0.030091, matched the instrument's reading of 0.03009126 to
  five places, which is what licensed using the same approximation for the
  spread.
- **A one-sided control was written two-sided.** "Erratic learner gain within
  ±0.001" is unreachable by construction: a held-out table over hundreds of
  classes of noise **must** lose to the base rate. The control is that it never
  *gains*.

Every one of the three was found by an implementer measuring rather than by a
reviewer reading, and each is recorded in the frozen document with the
confounding number beside it.

## A shared append-only file will collide, and it will collide at the worst moment

The first stage gate came back red on exactly one test:
`campaign_reconciliation_covers_every_campaign_record`. Another campaign had
landed a row in the campaign-reconciliation table while this branch also had
one, and the chamber's merge product carried only one of the two. No code was
at fault; the whole expensive run bought one line of a TSV.

Two things follow. **Absorb main immediately before any stage submission** —
not "at every boundary" in the abstract, but in the same sitting. And **an
append-only shared registry is a guaranteed conflict site**, so it should be
the first thing checked, not the last thing discovered.

The same file bit again, differently, at close: a **hand-authored** file
living inside a directory whose other contents are machine-authored was
clobbered by an absorption, because the reflex for that directory is
"regenerate, never text-merge". Mixing authorship inside one declared
directory removes the reader's ability to apply one rule to it.

## `make type-audit-report` is a check, not a writer

Resolving the close absorption's audit-report conflicts by running
`make type-audit-report` and `make plumb-report` looks like regeneration and is
not: those targets regenerate to a **temp file**, diff, and fail on drift. The
writers are the underlying `cargo run … > docs/audits/…` redirects. This is
the same shape the repository's own guidance warns about for the digest
renderer, one directory over, and it was still walked into.

Worse, the merge commit then landed carrying stale aggregates *through a green
gate* — the check ran against a tree the hook subsequently changed — and the
staleness was only caught by running the check again after the merge
commit existed. **After resolving a generated aggregate by regeneration, run
its check once more against the landed commit.**

## Sequencing a campaign that moves the census

This campaign is the first to move *existing* census columns mid-flight. The
census sentinel is a live test whose entire job is to redden at the commit
that moves the census, and its waiver file is documented as being for
cross-host divergence only, with "empty is the expected state" written on it.
So it was left red on purpose from Task 4 to close, and a stage gate on this
branch reds on it — which the gate ladder's own documentation predicts for any
world-touching campaign, and which is easy to misread as a defect at the
moment it appears.

The ordering that works: finish the last world-moving task, then request the
census, absorb its branch, then stage-gate, then do the documentation task.
Anything else refreshes the reference twice or gates against a reference the
world no longer agrees with.

## Where the effort actually went

Task 6 — calibration — took **three fix rounds**, and none of them was about
code. Round 1 corrected which criterion picks a constant. Round 2 applied the
replacement. Round 3 was pure record: four findings, every one a sentence
somewhere describing a superseded reading — a comment claiming an ordering
"does hold" using the 0.16 rung's numbers, a witness justification naming the
wrong failing relation, "13 walks" where the number is 78, and a ledger
section missing its own constants table.

Three readings of one constant were taken in one day, and each reading staled
every sentence written against the previous one. That is the predecessor's
"after a correction, re-derive every number downstream" lesson arriving in a
new costume: here the correction was *iterated*, so the staling happened three
times, and the third round found sites the second had not.

**The remedy that worked** was keeping the superseded sections rather than
editing them, each with a superseding banner naming what it no longer governs
and what it still does. Three readings either side of a change are the
campaign's evidence for the change; deleting the first two would have
destroyed the argument that the withdrawn clause was defective.

## What worked, and is worth keeping

- **The premise was measured before it was designed**, and the probe was
  committed rather than run and discarded.
- **A recorded golden taken on the unchanged tree, proven discriminating by a
  one-unit perturbation before it was trusted.** The two control kinds' output
  is byte-identical across a recipe change, and that is a fact rather than an
  inference.
- **A calibration instrument that made the search cheap by algebra.** The
  found fraction is a ratio in which the reliability cancels, so the step
  edges alone decide whether the band is reachable and the reliability only
  sets frequency. Two independent knobs instead of a two-dimensional search.
- **The failure shipped as the headline.** One of seventy-two bar-instances
  failed; no constant and no bar moved after the reading, and the failing
  assertion is left failing because it is the record.
- **A guard file that scans its own source** for the four measurement seeds
  and fails if one is named there — the two-seed discipline mechanised rather
  than promised.
- **Exact zeros got witnesses.** When the spring's walk-band floors dropped to
  zero, a floor of zero can only be satisfied — so a witness asserts the exact
  zero beside them rather than leaving a row that cannot fail.

## Estimate deltas

| | planned | actual |
| --- | --- | --- |
| tasks | 8 | 8; **five review fix rounds** (Tasks 4, 5, and three on 6) plus one pre-review correction round on Task 5 |
| registered metrics | "up to 24" | **32** (eight readouts × four kinds) |
| added census cost | ≤ 0.25 CPU-s/world | **0.331**, accepted rather than subsampled |
| preregistered bar-instances | — | **72; 71 held** |
| decisions | 0776–0785 reserved | **4 minted** (0776–0779) |
| epochs / stream labels / draws | 0 | 0 |

## A close-walk finding about the ledger itself

Eight operational rulings — the unminted-decision cite rule, the
surface-branch ruling on the locale's free function, the golden-not-recorder
ruling, the stage-gate move, the census-before-stage-gate sequencing, the
SUMMARY order, the by-hand injection run, and the TSV absorb rule — were made
contemporaneously in the task ledger and reached the committed ledger only as
a backfilled entry (#13) at the close walk. They should have been written to
`docs/superpowers/ledgers/` as they occurred; the task ledger is scratch and
dies with the worktree. The retrospective records this as the campaign's own
instance of the pattern The Cartulary was founded to remove.

## Do differently next time

1. **Measure the premise first.** 1.2 seconds changed what the campaign was.
   A probe over the *existing* world is the cheapest instrument available and
   is almost never the first thing anyone builds.
2. **Ask of every frozen bar: what is the cheapest way to make this true?**
   If the answer is "make something else rarer" or "move a constant that
   appears in the metric's own definition", the bar is not a bar.
3. **Never write "the highest passing X".** The highest passing rung is by
   construction the one with the least headroom; say which margin you want.
4. **Run the brief's literal text before believing it.** Most of this
   campaign's controller defects surfaced the moment an implementer executed
   the words rather than read them — none surfaced from re-reading the brief.
5. **Absorb main in the same sitting as a stage submission**, and check the
   known shared append-only files first.
6. **A directory with one authorship rule must not hold a file with another.**
   A hand-authored file inside a machine-authored directory will be
   regenerated over.
7. **A `*-report` target is usually a check.** Confirm what writes the
   artifact before treating a target's name as an instruction, and re-run the
   check after the commit that was supposed to fix it.
8. **Keep superseded calibration sections, banner them, and never edit them.**
   The readings either side of a change are the evidence for the change.
9. **When a claim's population might be empty, count it.** "Zero cause implies
   zero prevalence" was true, unfalsifiable, and quantified over nothing.
10. **An A/B whose two arms build the same shared object cannot price that
    object.** The cost design compared one metric family against another, and
    both studies constructed the same pool — so neither arm could see the
    pool's cost at all. To price a registration, measure all-new against none,
    and ablate inside the winner to attribute.
11. **Derive a preregistered bar from the null's spread before freezing it.**
    A permutation null has a standard deviation as well as a mean; a bar
    tighter than that spread fails by chance at a rate nobody intended. Compute
    the spread first — here the χ² approximation gave it in closed form — and
    set the bar as a multiple of it.
12. **Never shrink a rare event's predictor toward one half.** Laplace's prior
    is toward 0.5, which on a three-percent event over hundreds of classes
    predicts nearly every thin class at even odds and reports a negative gain
    that is an artefact of the prior. Shrink toward the base rate, so an
    uninformative class scores exactly zero and never below.

## Deferred minors, and where each landed

| item | disposition |
| --- | --- |
| the steepness boundary test does not pin the exact saturation points (`>=` versus `>`) | accepted close minor; the three-band behaviour either side is pinned, only the two exact boundary values are not |
| a prose comment names "±0.33" instead of the exported threshold constant | accepted close minor; the constant is now public and the prose is the last copy of the literal |
| the corner/expression/moisture assembly is repeated at three call sites in the locale window | accepted close minor; a shared helper is a refactor with no behavioural claim behind it |
| the test-side afloat predicate and the render's vantage gate are two predicates that disagree at ~0.09% of facets | recorded in the ledger and at the assertion, with the exact conflict count (28 at the shipped constants) and its non-monotone history 35 → 27 → 30 → 28 |
| a file-wide lint allowance in the control-golden test, and duplicated literals in the same file | accepted close minor; the file is a single-purpose fixture recorder |
| the control golden's own doc overstates its protection against a golden-rebaseline run | corrected in the doc; the rule "never rebaseline goldens before calibration re-pins" is stated where the golden is |
| a soft-step degenerate case (`hi == lo`) is guarded only by a test outside the commit gate | accepted close minor; the constants are authored and the gate would catch a change to them |
| no test asserts that the laboratory's sign tuple and the room sentence's own clause agree end-to-end (the vessel duplicates two derivation lines) | deferred with a reason: the single-implementation rule (decision 0778) is what makes them agree, and a render-and-re-derive test covers the prose half |
| the instrument's noise controls run only under an explicit ignored-test flag | accepted by convention, and it is the plan's own instruction: they are probes, not gates |
| the permutation null's identity case (a shift congruent to the population size) is unguarded | accepted close minor; the five shifts are authored constants far from that value |
| the spring's spatial-autocorrelation reading is *absent* on the walk pool, so its address-hash guard is vacuous for that kind | recorded in the ledger with its cause (the walk band's largest spring cause is ~0.244 under a 0.35 step edge) |
| with a zero floor the sign kinds carry no address-hashable noise term, so the real-versus-mutant table cannot move their rows | corrected in place at the table rather than left to inference; those rows now discriminate macro-state continuity, which is a different real property |
| the readout's "metric absent" path never fired on five seeds, so it is a guard with no positive control | recorded in the ledger; a successor adding a seed may be the first to exercise it |
| the lift bar's own denominator varies 1.52× across seeds, and the seed that failed is the seed with the largest one | recorded in the frozen document's outcome paragraph and in the ledger, explicitly **not** acted on; a successor must derive a steadier denominator before measuring |
| a campaign's uncommitted timing row sweeps into whatever commit follows it | accepted; it is how the ledger stays durable, and this campaign's rows land with the documentation commit |
| a census refresh that grows the registry cannot deliver through the queue (ledger #12): the gate's own injection-fixture witness is red the moment the census gains columns, and the fixtures' authoring script refuses the delivery's own dirty tree | filed as registry row `TOOL-census-delivery-and-injection-fixtures-deadlock`; broken by hand this time with an ungated intermediate object plus a clean re-authoring worktree, never `--no-verify` |

Four follow-ups were filed as registry rows rather than deferred here: a
species-gated body that speaks its inference, remote signs (a neighbouring
facet's word, the channel band, rumors), the diagnostic verb that prints two
raw fields the room sentence never says, and the relief axis whose real cause
has existed since an earlier campaign and which nothing reads. A fifth —
co-occurrence legibility, information *between* kinds — is recorded in the
ledger and was never preregistered.
