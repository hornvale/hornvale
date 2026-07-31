# The Contour — readout (Task 7)

**This is the adjudication of spec §4.2's frozen predictions.** The baseline
(`docs/superpowers/plans/the-contour-baseline.md`, measured at `c405a5e2`,
mechanism absent) is unedited and is the comparator throughout. The live
measurement is taken at commit `71f2c433` (`feat(the-contour): wire
position-aware conflict into the dominance tests`), the tip of this branch
at readout time, which is the last commit before this one. No constant was
retuned and no threshold was adjusted to produce these numbers; per the
task brief, the eight currently-failing drifted-pin tests were left alone.

## Commands run

```
cargo run -p hornvale -- lab run studies/the-contour.study.json      # seeds 1..=30
cargo run -p hornvale -- lab run <scratch copy, seeds 1..=100>       # replication
```

The 100-seed run used a scratch copy of `studies/the-contour.study.json`
with `"seeds": {"from": 1, "count": 100}` (schema is `{from, count}`, not
`{from, to}` — same correction the baseline task recorded); the committed
study file was never touched. **No baseline exists at 100 seeds** — Task 5
recorded only seeds 1..=30 — so the 100-seed run below is reported as an
internal replication (does the 30-seed pattern hold at 3.3× the sample?),
not as a second paired baseline/live diff. This mirrors how *The Tumult*
and *The Tithe* used their 100-seed replications: against a fixed reference
figure, not a second full baseline run.

---

## Prediction 1 — M3 rises

> A second contest axis increases the number of peoples surviving to bake
> end, and the effective diversity, against the shipped baseline. If M3 does
> not move, the multi-axis thesis is wrong as built.

**FALSIFIED.** M3 did not rise. At 30 seeds it moved fractionally *down*:

| | n | mean | distribution |
|---|---|---|---|
| baseline (30) | 30 | 3.9667 | `5`×23, `4`×1, `0`×6 |
| live (30) | 30 | 3.9333 | `5`×22, `4`×2, `0`×6 |

The entire delta is **one seed** (seed 12: `5` → `4`); every other seed's
peoples-count is byte-identical to the baseline, including the exact set of
extinction seeds (see the stratified section below). This is not "roughly
flat with noise in both directions" — it is one world losing one people and
nothing else moving, in either direction, across all 30 seeds.

At 100 seeds (replication only, no baseline pair):

| | n | mean | distribution |
|---|---|---|---|
| live (100) | 100 | 4.08 | `5`×80, `4`×2, `0`×18 |

The 100-seed mean (4.08) sits marginally above the 30-seed live mean
(3.93), but that is a stratification artifact, not a rise signal — see
below: it is driven entirely by the ratio of extinction to survival seeds
in the wider sample, not by any additional people surviving inside a
world that was already going to have some survive.

## Prediction 1 — M3, stratified

The baseline is bimodal (6/30 seeds total extinction, 23/30 full survival,
1/30 partial) and stays bimodal live. Pooling the two regimes into one mean
hides which one moved, so both are reported separately.

**Extinction-seed count and identity** — unchanged:

| | seeds extinct (peoples = 0) | which seeds |
|---|---|---|
| baseline (30) | 6 | 6, 9, 18, 20, 22, 29 |
| live (30) | 6 | 6, 9, 18, 20, 22, 29 |

The mechanism rescued **zero** extinction seeds and caused **zero** new
extinctions. Whatever the second contest axis is doing, it is not touching
which worlds go to zero.

**Surviving-world distribution** (peoples > 0 only):

| | n | mean | distribution |
|---|---|---|---|
| baseline (30) | 24 | 4.9583 | `5`×23, `4`×1 |
| live (30) | 24 | 4.9167 | `5`×22, `4`×2 |
| live (100) | 82 | 4.9756 | `5`×80, `4`×2 |

Surviving-world mean moved from 4.9583 to 4.9167 at matched sample size —
down, not up, and the direction is consistent with the pooled figure. The
100-seed surviving mean (4.9756) is closer to ceiling than either 30-seed
figure, but with no 100-seed baseline to pair it against this cannot be
read as a rise; it just says the near-ceiling regime is the norm at any
sample size, baseline included (23/30 full survival there too).

**Conclusion on prediction 1: falsified as stated.** A "rise driven by
rescuing extinction seeds" did not occur (the extinction set is identical),
and a "rise in worlds that already survived" did not occur either (the
surviving mean went down by one people in one seed). The second axis moved
one seed's outcome, in the direction opposite the prediction.

---

## Prediction 2 — M2 is the open question; a heavier tail vs. staying geometric

> A move toward a heavier entity-size tail supports the sigmoid wager. M2
> staying geometric while M3 rises is the more interesting result... it
> would localise the missing term in per-community deviation.

**M2 did not move toward a heavier tail. It stayed in the same shape as the
baseline**, and since M3 did not rise either (prediction 1), the
"interesting" branch of prediction 2 does not apply as written — that
branch was conditioned on M3 rising, which did not happen. What actually
obtains is the plainer case: neither axis moved.

### Shape comparison, 30 seeds (matched pair)

| stat | baseline (30) | live (30) |
|---|---|---|
| n present / absent | 24 / 6 | 24 / 6 |
| min | 0.007618 | 0.007411 |
| q25 | 0.010598 | 0.012126 |
| median | 0.013738 | 0.014350 |
| q75 | 0.020531 | 0.019888 |
| q90 | 0.052582 | 0.037757 |
| max | 0.082405 | 0.089595 |
| mean | 0.022241 | 0.021926 |
| stdev | 0.020415 | 0.021104 |
| CV (stdev/mean) | 0.918 | 0.963 |
| max/median | 6.00 | 6.24 |

Mean and median both sit within a few percent of the baseline value —
essentially flat. Coefficient of variation ticked up slightly (0.918 →
0.963), and the maximum rose modestly (0.0824 → 0.0896, +8.7%, both driven
by different seeds — the baseline's max was seed 19, unchanged at
0.082405; the live max is seed 27, up from 0.0713 to 0.0896). But **q90
fell** (0.0526 → 0.0378) — a signal in the opposite direction from "heavier
tail." A genuine shift toward a heavier tail should push the upper
quantiles up together; here one outlier grew while the broad upper
quantile shrank. That is not the signature of a distributional shift, it
is seed-level noise in a 24-point sample.

### Replication at 100 seeds (internal check, no baseline pair)

| stat | live (100) |
|---|---|
| n present / absent | 82 / 18 |
| median | 0.015815 |
| mean | 0.024396 |
| q90 | 0.061243 |
| max | 0.129252 |
| CV | 1.005 |
| max/median | 8.17 |

Max/median rose further at 100 seeds (8.17 vs 6.24 at 30, vs 6.00 baseline
at 30), which is at least directionally consistent with a heavier tail —
but without a 100-seed baseline this cannot be distinguished from what a
larger sample of an unchanged geometric-ish distribution does on its own
(a bigger sample just has more chances to draw an extreme outlier). The
q90/median ratio, a more robust tail indicator than max/median, is 3.87 at
100 seeds live versus 3.83 at 30 seeds baseline and 2.63 at 30 seeds live
— no consistent direction across the three readings.

**Conclusion on prediction 2: stayed geometric.** Mean, median, and IQR are
essentially unchanged from baseline; there is no order-of-magnitude
outlier, no bimodality, and no consistent upward shift across every tail
quantile — the one metric that moved cleanly (max/median) moves inside a
noise band a single outlier seed can produce. This is the "stayed
geometric" branch of prediction 2, but because M3 did *not* rise
(prediction 1), it is not the informative "stayed geometric while M3 rose"
case the spec called more interesting — it is the null case §4.3 names
explicitly (see below).

---

## Prediction 3 — M1 roughly unchanged

> This campaign adds no conduction medium, so a large move in M1 would
> indicate the mechanism is doing something other than what §2 describes.

**Instrument gap, stated plainly per the task brief: `cascade_sizes` /
`BakeCensus.cascade_hist` is not a registered lab metric.** Confirmed again
at this commit — `cargo run -p hornvale -- lab list-metrics | grep -i
cascade` returns only an unrelated substring hit inside
`homophony-merger-share-goblin`'s doc text, exactly as the baseline
recorded. M1 **cannot be adjudicated through the instrument**, and per the
brief this is reported as a gap rather than skipped or substituted.

**Hand-obtained supplementary reading** (clearly not a registered metric —
no `Absent` semantics, no drift check, no test coverage of its own): a
scratch test mirroring `history_tumult.rs` / `history_tithe.rs`'s own
instrument exactly (`history_for` + `cascade_sizes`, pooled over the same
seed samples) was written, run, and then deleted — it was never committed.
It was run twice: once at this branch's tip (`71f2c433`, mechanism
present) and once in an isolated worktree checked out at the frozen
baseline commit `c405a5e2` (mechanism absent), so the "baseline" figure
below is a fresh measurement at the exact frozen commit, not a
transcription from *The Tithe*'s own retrospective — though it reproduces
that retrospective's numbers exactly, which is a useful cross-check on its
own (`hist [634, 36, 0×10]`, `raided 7183` at 1..=30 — identical to *The
Tithe*'s own post-absorption figure).

| sample | hist (bin0, bin1, 0×10) | raided | S (secondaries) | P (conquests) | **σ** |
|---|---|---|---|---|---|
| baseline, 1..=30 | `[634, 36]` | 7183 | 706–742 | 6441–6477 | **0.109–0.115** |
| live, 1..=30 | `[773, 52]` | 7915 | 877–929 | 6986–7038 | **0.125–0.133** |
| baseline, 1..=100 | `[1889, 98]` | 22255 | 2085–2183 | 20072–20170 | **0.103–0.109** |
| live, 1..=100 | `[2366, 157]` | 23774 | 2680–2837 | 20937–21094 | **0.127–0.136** |

**This is a real, modest move, not zero, and it replicates directionally
at both sample sizes.** σ rose roughly 15% at 30 seeds and roughly 23–27%
at 100 seeds. The `raided` count itself rose 10.2% at 30 seeds and 6.8% at
100 seeds — the 10.2% figure at 30 seeds matches the brief's cited
prior measurement of raid-evictions almost exactly, which is a useful
independent cross-check on this hand-obtained reading's soundness.

**What did not move: the shape.** Both baseline and live, at both sample
sizes, occupy exactly two bins (`bin0` = cascades of size 1, `bin1` =
size 2–3) with **zero cascades of size ≥4** anywhere, out of totals ranging
from 670 to 2523 pooled cascades. The hard cutoff *The Tumult* found and
*The Tithe* confirmed a second time is still there, unmoved, at this
commit — sub-critical and geometric in shape, whatever the exact σ.

**Adjudication: partially confirmed, with the caveat spelled out rather
than rounded away.** "Roughly unchanged" is a fair description of the
*shape* (still two bins, still a hard cutoff, still nowhere near *The
Tithe*'s own >2× headline move off σ≈0.051). It is not a fair description
of the *magnitude* — σ moved by a consistent, non-trivial ~15–27% in the
same direction at both sample sizes, tracking a real ~7–10% rise in raid
volume. Because this reading is hand-obtained rather than instrument-
verified, it is reported with lower confidence than M2/M3/M4 and should
not be treated as dispositive on its own — but it does not support reading
M1 as flat, and a reader relying only on "no large move" would be
overstating what this shows. Whether a ~20% σ move is "large" in prediction
3's sense is a judgment call this readout declines to make unilaterally;
the numbers are reported so the campaign owner can make it.

---

## M4 — already-adjudicated context

M4 is not one of the three frozen predictions; it was already adjudicated
**pre-mechanism**, on the baseline run itself: mean **−0.020**, 95% CI
**[−0.069, +0.029]** — centered on zero, withdrawing spec §2.2's "frontier"
reading (defensible ground is not, in aggregate, correlated with poor
ground). That conclusion stands and is not reopened here.

For completeness: M4 *was* re-measured live (30 seeds) as part of running
the study, and it lands at mean **−0.0200**, 95% CI **[−0.0692, +0.0293]**
— indistinguishable from the pre-mechanism figure to three decimal places.
This is worth one line because M4 is not a pure function of terrain alone:
`spearman_defensibility_capacity` builds its connection graph via
`connection_graph_of`, which reads the world's **committed settlements**
(`hornvale_settlement::all_settlements`), and settlement placement is
exactly what the mechanism can perturb through raid/settle outcomes. Per-
seed values did shift by a few percent for most surviving seeds (e.g. seed
1: 0.00947 → 0.01034; seed 27: −0.2904 → −0.2904 is unchanged since that
seed's outcome didn't move) — so M4 is not literally invariant to the
mechanism — but the aggregate stayed null. This is supplementary, not a
re-adjudication.

---

## §4.3's null — does it obtain?

> If M3 does not move and M2 does not move, the conclusion on the record
> is: a second contest axis, uncorrelated with the first and entering at
> the decision point, is not sufficient to hold diversity open in this
> world. That is a real finding about 0089 clause 1's chosen mechanism, and
> it would send the sequence back to design rather than forward to The
> Appraisal. It is not a reason to add a third mechanism inside this
> campaign.

**Both conditions are met.** M3 did not move (prediction 1: falsified, and
in the direction opposite the prediction at that). M2 did not move
(prediction 2: stayed geometric — mean, median, and IQR within a few
percent of baseline, no consistent tail-heaviness signal). **§4.3's null
therefore obtains, and its stated conclusion is this readout's headline: a
second contest axis, uncorrelated with the first and entering at the
decision point, is not sufficient to hold diversity open in this world.**
This is a finding about decision 0089 clause 1's chosen mechanism, sends
the sequence back to design rather than forward to The Appraisal, and is
not a reason to add a third mechanism inside this campaign.

The one thing the null does *not* explain cleanly is M1's modest σ rise
(prediction 3's caveated finding above): the mechanism visibly did
*something* — more raids, a higher branching ratio, one seed's peoples
count dropping — it simply was not enough of the right *kind* of something
to move diversity. That the mechanism is measurably live (M1, and the
per-seed M2/M4 churn documented above) while M3 and M2 stayed flat is what
makes this a null about *sufficiency*, not a null about the mechanism
being inert or miswired.

---

## Instrument gaps (summary)

- **M1 has no registered lab metric.** `cascade_sizes` /
  `BakeCensus.cascade_hist` exist only as internal `hornvale-worldgen`
  types. A hand-obtained reading was taken (above) via a scratch test that
  mirrors *The Tumult*/*The Tithe*'s own instrument, run and then deleted
  — never committed, and explicitly lower-confidence than a registered
  metric.
- **M2 measures peak population, not bake-end population** (spec §2.2
  amendment 3): `OccupationRecord::peak_population` is a high-water mark
  that never falls; there is no end-state population accessor in the data
  model. `largest-holding-share` therefore reads "largest peak share among
  communities alive at bake end," not a literal simultaneous snapshot.
- **M4 reads present-day terrain, not the bake's own final era** (spec
  §2.4 amendment 4): `bake_history_from` computes and discards its own
  final-era `(ConnectionGraph, capacity)`; substituting present-day terrain
  is a different, honestly-labelled reading of the same structural
  question. As noted above, "present-day" here is not terrain-only — it
  also reads present-day committed settlements, so M4 is sensitive to the
  mechanism's placement effects even though it is billed as a terrain
  check.
- **M3's registered metric is a raw count, not an effective-diversity
  index.** The spec's framing for M3 mentions "the effective diversity
  (the same reading `coexist.rs` uses in space, computed here in time)" as
  well as the count; only the count (`peoples-alive-at-bake-end`) is
  actually registered. This readout adjudicates prediction 1 on the count
  alone, as the baseline did.

---

## Trust in this readout

- The primary (30-seed) comparison is a matched pair against the frozen
  baseline: same seeds, same study, same schema, one mechanism commit
  apart. High confidence.
- The 100-seed run is an unpaired replication (no 100-seed baseline was
  ever captured) — used here only to check whether the 30-seed pattern
  holds at 3.3× sample, per precedent, not as a second independent
  baseline/live diff.
- M1's numbers are hand-obtained outside the metric-registry system: no
  `Absent` handling, no drift check, no committed test. They were cross-
  checked against *The Tithe*'s own published retrospective figures (the
  baseline reproduction matched exactly) and against the brief's
  independently-cited raid-eviction delta (+10.2% at 30 seeds, matched
  exactly), which gives reasonable confidence the extraction method itself
  is sound — but the reading remains outside the instrument and is
  reported with that caveat throughout.
- No constant was retuned, no threshold adjusted, and no seed re-rolled to
  chase a better number anywhere in this readout. The one prediction that
  moved cleanly (M3) moved against the prediction, and it is reported that
  way as the headline, per the task's standing instruction.

## Commit

```
git add docs/superpowers/plans/the-contour-readout.md
git commit -m "test(the-contour): the readout, adjudicated against the frozen predictions"
```

---

## Addendum: the null decomposes, and M3 was built as half of itself

*Added after an ideonomy pass on the readout, before the campaign closed. It
changes nothing about the numbers above; it changes what they license.*

### The null is two claims, not one

Writing the prediction in power-analysis notation exposes a required slot this
campaign never filled — **headroom**:

```
metric        peoples_alive ∈ {0,1,2,3,4,5}   DISCRETE, BOUNDED, max = roster size
baseline      P(5) = 0.767     <- ALREADY AT THE CEILING
              P(0) = 0.200     <- at the floor
              P(4) = 0.033
prediction    "M3 rises"
HEADROOM      ZERO for 76.7% of the probability mass
```

"M3 rises" was close to unfalsifiable upward: in 23 of 30 seeds the metric
*could not* rise, because five settled peoples is the roster and all five were
already alive. So the null above must be split, because its two halves do not
have the same evidential status:

1. **"A second contest axis does not rescue worlds from extinction."**
   **Strong.** Six extinction seeds at baseline, six live, the **identical seed
   set** (6, 9, 18, 20, 22, 29). That is a detectable effect measured as
   exactly zero, and it is the null §4.3 anticipated.

2. **"A second contest axis does not improve diversity in surviving worlds."**
   **Untested.** The instrument is saturated at its ceiling in 23 of the 24
   surviving worlds. This readout does not license this claim in either
   direction.

Reporting these as one verdict, as the sections above do, overstates what was
measured. The sections stand as written; this addendum is the correction.

### M3 was specified as two metrics and built as one

Spec §4.1 asks for both halves, verbatim:

> **M3 — peoples-diversity at bake end.** Count of peoples with a live
> community, **and the effective diversity** (the same reading `coexist.rs`
> uses in space, computed here in time).

Only the count was registered. The effective-diversity half was never built —
though `domains/demography/src/byproducts.rs::strife` already computes exactly
that reading, and `coexist.rs` documents it at ≈2.4 in space at β = 2.0.

**A count measures presence; decision 0089 is about diversity.** A world with
five peoples where one holds 95% of everything is monoculture with survivors,
and no count can tell those apart. The metric with headroom was specified,
exists in the codebase, and was not wired up — and neither the Task 4 brief nor
the Task 4 review caught it, because both checked M3 against what was built
rather than against what was specified.

This is the campaign's sharpest process finding and it is the fourth instance
of one pattern: **a claim frozen against an instrument nobody verified could
carry it** (amendment 3, amendment 4, the absent M1 metric, and now M3's
missing half).

### Why the diversity measurement is deliberately NOT run here

Building the effective-diversity half and re-running would be *executing* the
preregistration rather than amending it — the metric was specified before any
code existed. That argument is sound and it is not the argument being followed.

**Running a new measurement immediately after a disappointing one has the shape
of metric-chasing even when the logic is clean.** *The Tithe* amended five
times and every amendment had a clean local justification. So the effective-
diversity reading is deferred to the successor campaign, where it gets a fresh
preregistration, both branches informative, **and its headroom declared in
advance** — with no disappointing number sitting behind it.

The null above ships as this campaign's result, unsoftened.
