# 0138. A preregistered criterion may be restated when its estimator is wrong, never when its result is inconvenient

**Status:** Accepted (2026-08-14) · **Decider:** Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0137](0137-the-craton-clamp-is-a-budget-not-a-limit.md)

In the context of two of The Hollow's preregistered cave criteria (H1 and H4)
failing after The Glasshouse's terrain epoch, where in both cases the *property*
the criterion existed to protect was intact and the *statistic* used to test it
was invalid, we decided to **restate both criteria rather than widen, skip, or
re-pin around them**, because decision 0016's freeze exists to stop a
prediction being rescued after its outcome is known — and a criterion whose
estimator is provably mis-specified is not a prediction being rescued, it is an
instrument being repaired.

## The line this decision draws

0016 forbids retuning a criterion to rescue a falsified prediction. It does not
forbid correcting a criterion that never measured what it claimed to. The
distinction is testable, and both restatements below had to pass the same
three-part test before being authorised:

1. **The property is independently verified intact** — by a measurement other
   than the failing statistic.
2. **The defect is in the estimator, demonstrated** — not asserted, and not
   inferred from the failure itself.
3. **The restated criterion is re-proved against the defect the original
   existed to catch** — in the same sitting, by injected failure.

A restatement that cannot show all three is a retune, and is refused.

## H1 — a pooled share is not a reachability test

H1 demanded each `CaveKind` hold ≥5% of pooled caves. The Hollow's actual
defect was that `LavaTube` and `Fracture` were **unreachable** ("Karst 100%,
others 0%"); the 5% share was a *proxy* for reachability, and the proxy broke
when the mix legitimately moved. The terrain epoch raised mean land crust
25.73 → 29.87 km, leaving less low-silica volcanic substrate, and lava tubes
became fracture caves — a substitution, not a decline: caves barely moved and
`Fracture` gained what `LavaTube` lost.

`LavaTube` still occurs in **30 of 30 worlds**. H1 is now that statement.
30/30 is not a threshold fitted to the data — it is the maximum, and the
definitional statement of reachability. It is **strictly stronger** than the
retired floor at detecting The Hollow's real defect: a kind confined to a few
worlds passes a pooled share test and fails this one.

## H4 — a binomial tolerance over a spatially smooth field

H4 demanded each probability bucket's realized hit rate fall within 25% of its
nominal. Two buckets failed at +53.4% and +29.0%, which on cell counts reads
8.64 and 5.95 sigma.

**The gate is not decalibrated.** `uniformize` maps the noise field onto a
uniform correctly: over all 473,318 land cells every 5%-wide bin of `U` holds
4.72–5.14%, mean(U) = 0.50005. The harness reconstruction still matches
production, pinned by `cave_at_agrees_with_the_kind_first_gate`.

**The sigma was fictitious.** The 25% bound is a claim about a rate's
precision, applied as though the bucket's cells were independent Bernoulli
draws. They are not — and H5, four lines below H4 in the same battery,
*asserts* ≥90% clustering, i.e. that the field is spatially smooth by design.
Measured overdispersion across the 30 worlds is chi2/df = 5.5–48.8 in every
bucket, against 1.0 for independent cells. Out-of-sample across three disjoint
30-seed sets, `Karst`'s mean(U) reads +0.1, **−24.5**, and −7.1 sigma over
~390,000 cells each, and `Fracture` flips sign twice; no real coupling
behaves that way. H4 and H5 were in direct tension and H5 states the intended
physics.

**What the failure actually was.** Both buckets are ~93% `LavaTube` (`Karst` is
exactly 0 in both, in all 30 worlds), so they are the population the epoch
thinned by 76%. What remained was dominated by one world: seed 3 supplied 36%
and 30% of their cells and 90% and >100% of their excess — the other 29 worlds
are collectively negative in the second. Excluding seed 3 the buckets read
+8.2% and −14.5%. Under the correct variance model the pooled excess is 1.09
and 0.72 sigma. Those buckets were **empty** at the battery's founding commit
`34cfaeb7`, so the rule had only ever run on dense, spatially diffuse
populations where cell-count precision is roughly adequate.

**The restated criterion** is two arms:

- **H4a (aggregate)** — the whole-globe calibration claim, where geography
  averages out and the power is. Bound 0.10, set from the measured ~3.9%
  between-world relative SE (5% would sit 1.25 sigma from zero and fire on
  geography); a second trigger fails on 3-sigma significance below that bound.
- **H4b (per-bucket)** — fails only when a bucket is both >25% off nominal
  (the original bound, untouched) and ≥3 sigma against a cluster-robust
  standard error taking the **world** as the sampling unit.

Neither the 0.25 bound nor the 500-cell floor was widened, and no seed was
re-pinned; all three would have silenced the symptom and left the estimator
wrong.

## Consequences

**A weakened criterion must be re-proved by injected failure, in the same
sitting.** H4b was written first, shipped green, and looked right — and a
mutation test (`GATE_NOISE_MEAN` 0.5003 → 0.5100, a ~0.13-SD decalibration
firing the gate 11.85% hot) **passed it**, while the original H4 would have
caught it. The restatement had traded a false positive for a false negative and
nothing in the green run said so. H4a exists to close that gap and fails the
mutation. This is the durable half of this decision: *fixing an over-firing
criterion is not done until the injected defect goes red again.* The mutation
must also be shown to have taken effect (here 48,316 → 55,080 caves) before its
verdict means anything — a no-op mutation produces the same green.

**An instrument states its resolution, including where it is blind.** H4a
resolves an aggregate decalibration of ≥10% with 30 worlds and is blind below
that; half the motivating mutation would slip through. That limit is recorded
in the code beside the bound, and the honest way to sharpen it is more seeds,
not a lower bound.

**This decision is not a general licence.** It authorises two named
restatements on stated evidence. Any future appeal to it must produce its own
three-part test — property verified intact by other means, estimator defect
demonstrated, restated criterion re-proved against injected failure — and a
campaign that cannot is looking at a finding, which decision 0016 says to
ship as the headline.
