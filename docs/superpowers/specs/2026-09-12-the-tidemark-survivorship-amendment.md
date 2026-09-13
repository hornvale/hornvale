# The Tidemark — survivorship instrument amendment (preregistration)

**Status:** FROZEN. This document is committed **before** any of the code it
describes exists. Its predictions are stated before measurement, and the git
history is the proof — the structural form of decision 0016's freeze, rather
than a promise to have frozen.

**Subject:** `windows/worldgen/tests/suite/survivorship_probe.rs`, the
instrument behind The Winze §5.2.

**Occasion:** absorbing `origin/main` into `campaign/the-tidemark` put 49 kinds
in the roster (39 + four Underworld peoples + six marine peoples) and
`the_separation_survives_conditioning_on_tenure` went red: stratified z 1.326
against `Z_SUPPORTS` 1.96, down from 3.197.

## 1. What is NOT in question

`BREACH_FREE_PATH_M` is not touched. `Z_SUPPORTS` is not touched. The module's
own "WHAT WOULD CHANGE THE VERDICT" fixes the response to a collapse in
advance — *report it, **never** tune the constant to separate the groups
again* — and that governs here. This amendment changes **how tenure is held
fixed**, which is a different object from either constant.

## 2. The defect, stated independently of the result

The bake runs 0–2000 years in 25-year epochs, so a working's tenure ranges over
**1–80 epochs**. `STRATA` is a frozen five-bucket literal whose top bucket is
`21+`:

| stratum | epochs spanned | share of the tenure range | pairs today | share of pair mass |
|---|---|---|---|---|
| 1 | 1 | 1% | 34 | 4% |
| 2–3 | 2 | 3% | 22 | 3% |
| 4–8 | 5 | 6% | 222 | 28% |
| 9–20 | 12 | 15% | 185 | 23% |
| **21+** | **60** | **75%** | **342** | **42%** |

**A bucket spanning 21–80 epochs does not hold tenure fixed.** Depth accrues as
`cut × epochs`, so depth varies roughly fourfold inside that one bucket from
tenure alone — which is precisely the confound the stratification exists to
remove. The instrument fails at its stated job there, and **it fails whether or
not the test passes**: this is a defect in the instrument's specification, not
an interpretation of its current output.

It was latent while the top bucket was small. It became load-bearing when the
population moved into it: 59% of breached workings are now `21+`, against 12%
of ordinarily-ended ones.

**This is the class this campaign is already filing a PROC row about** — a
frozen bucket scheme beside a population that grew into its open end. The
instrument did not rot because anyone edited it. It rotted because the world
moved and the literal could not.

## 3. Why repairing it is not rescuing the claim

Because **it can go either way, and both ways are reported.** A stratification
that actually holds tenure fixed may confirm the collapse or may restore the
separation. The repair is specified here, before its result is known, and the
predictions below are committed in the same breath.

If the repair were chosen *because* it restores significance, that would be
tuning in methodological clothing. The protection against that is this
document's position in the git history, and the fact that §5 names what each
outcome means before either is observed.

## 4. The amendment

**R1 — tenure strata derive from the data; the RULE is frozen, not the cut
points.** Replace the five-bucket literal with **quintiles of the pooled tenure
distribution** over the panel's ended workings (breached ∪ ordinary), cut on
the pooled sample so the two groups are stratified identically. Ties in integer
epochs go to the lower stratum; a quintile that would be empty for either group
contributes zero pairs and is skipped, exactly as a thin literal bucket does
today.

Why quintiles rather than more literal buckets: a finer literal scheme has the
same defect with a higher ceiling, and would need re-cutting every time the
world moves. A frozen *rule* over a derived cut cannot drift out of range.
(Note the contrast with this campaign's cohort-guard finding, where deriving
the population **destroys** the guard because the literal is the only
independent witness. Here the literal is a binning convenience, not a witness,
so deriving it costs nothing and buys scale-invariance.)

**R2 — a censoring sensitivity arm, reported beside the primary, never
replacing it.** The 89 still-open workings are excluded today and are the
deepest population (q3 1171 m against breached 802 m). Excluding them is
defensible — they have no cause of ending — but it means the statistic is
computed on completed cases with the deep survivors censored out. Add a second
readout comparing **breached against all non-breached (ordinary ∪ still-open)**
under the same R1 strata, and report both.

R2 does not change what §5.2 claims. It measures how much the exclusion is
doing, which is currently unknown.

## 5. Predictions, with both poles named

**P1 (R1, the primary).** Under quintile strata the stratified z **rises above
1.96**.
- *Reasoning, stated so it can be wrong:* the top bucket currently merges 21–80
  epochs and carries 42% of the pair mass; holding tenure genuinely fixed should
  recover the signal the hazard implies, since `P(breach) = 1 − exp(−depth/3000)`
  makes depth the only driver and tenure the only confound.
- *If P1 holds:* §5.2's claim survives on a 49-kind roster, and the red was an
  instrument artifact. The finding is then about the instrument, and the
  campaign reports that a preregistered invariant was nearly lost to a frozen
  bucket scheme.
- *If P1 fails* (z stays at or below 1.96 under strata that do hold tenure
  fixed): §5.2's claim is **unsupported at 49 kinds**, on an instrument that can
  now be trusted to say so. That is the stronger finding of the two, and it is
  reported as such — with a decision record, and `refuted` is the registry
  status decision 0131 opened for exactly this.

**P2 (R2, the sensitivity arm).** Including still-open as non-breached
**weakens** the separation relative to the primary but does not reverse its
direction.
- *Reasoning:* still-open are deeper than ordinarily-ended at every quantile, so
  adding them raises the non-breached depths. Their tenure is `(2000 − founded)`
  and therefore spread across strata by founding date rather than piled at the
  top.
- *If the direction reverses:* the exclusion was carrying the result, and that is
  a finding about §5.2's design that outranks P1 either way.

## 6. What this amendment does not do

- It does not touch the physics, the panel, the seeds, or either constant.
- It does not change `Z_SUPPORTS`, so a pass still means what it meant.
- It does not resolve the right-censoring question. R2 measures its size; a
  hazard model over censored data is the correct instrument for §5.2's claim and
  belongs to The Winze, not to this campaign. Recorded as a follow-up, not
  attempted here.
- It does not re-run the panel on more seeds. The panel is E.9's frozen twelve
  and stays that.

---

## 7. Outcome (recorded 2026-09-12, after measurement)

**The predictions above are unedited and stay unedited.** This section is
appended beneath them. A preregistration whose predictions are revised once the
result is known measures nothing, so what follows is the result *against* what
§5 said in advance, never a restatement of §5.

Measured at `031584af1` (R1 and R2 implemented) on the frozen twelve-seed panel,
49-kind roster:

| readout | AUC | z |
|---|---|---|
| pooled, breached vs ordinarily-ended | 0.7959 | 5.244 |
| **R1 primary**, stratified on tenure quintiles | 0.4977 | −0.034 |
| **R2 arm**, breached vs all non-breached, same cuts | 0.3946 | −1.803 |
| R2 arm, pooled (unstratified) | 0.6810 | 3.323 |

### P1 — FAILED, and the pole's stated consequence was NOT taken

§5 predicted the stratified `z` would **rise above 1.96** under strata that hold
tenure fixed. It did not: it fell to **−0.034**, the P1-fails pole. The pooled
statistic did not move.

§5's own text says that pole means §5.2's claim is "unsupported at 49 kinds …
with a decision record, and `refuted` is the registry status decision 0131
opened for exactly this." **The decision record was written and it declined the
`refuted` status** — see
`docs/decisions/0959-the-winze-5-2-conditioned-claim-is-unevaluated-not-refuted.md`.
The null is produced by four observations: the five quintiles carry breached
n's of 1, 3, 5, 4 and 19, and the two strata resting on one and three hold 20%
of the pair mass and the two most extreme AUCs (0.144 and 0.282). A negative
verdict decided by one observation is not a negative verdict, so §5.2's
conditioned claim is recorded **UNEVALUATED**.

This is a limit §5 did not anticipate, and it is worth naming as such rather
than filing under P1: §5 assumed that an instrument which *does* hold tenure
fixed would thereby be able to answer, and the panel's size is a separate
constraint from the stratification's validity. The panel is denominated in
*mines*; the conditioned question is denominated in *breaches* — spec amendment
E.4.2's mis-sizing, documented before any of the three campaigns involved
existed. Measured yield is 2.67 breaches per seed, so ten breached per stratum
needs ~19 seeds against today's twelve. **Extending the panel belongs to The
Winze**, not to this campaign; `PANEL` is untouched.

`windows/worldgen/tests/suite/survivorship_probe.rs` therefore **gates** the
conditioned assertion on `MIN_BREACHED_PER_STRATUM = 10` rather than asserting
or deleting it. Below the floor the readout prints as UNEVALUATED; at or above
it, the original assertion arms itself at the unchanged `Z_SUPPORTS`, with no
further human action. The **pooled** assertion is untouched and still has full
teeth.

### P2 — SPLIT: the pooled half held, the conditioned half reversed

§5 predicted that including the still-open workings as non-breached would
**weaken** the separation without reversing its direction.

- **Pooled: confirmed.** AUC 0.7959 → 0.6810, weaker and still above 0.5.
- **Conditioned: reversed.** AUC 0.4977 → **0.3946**, below 0.5, with the
  pair-weighted stratum-median direction going 215 supporting against 1441
  opposing.

§5 says of a reversal that "the exclusion was carrying the result, and that is a
finding about §5.2's design that outranks P1 either way." That reading is
tempered by the same power limit: the conditioned arm is cut on the same thin
strata, so it is no better evidenced than the primary it qualifies. What
survives is the direction of the concern, not a measured magnitude — the
exclusion is not *manufacturing* a conditioned result, since removing it makes
the conditioned readout worse rather than better.

### What §6 said it would not do, and did not

The physics, the panel, the seeds, `Z_SUPPORTS` and `BREACH_FREE_PATH_M` are all
unchanged. The right-censoring question is unresolved: R2 measured the
exclusion's size, and a hazard model over censored data remains the correct
instrument for §5.2's claim. Both that and the panel extension are handed to The
Winze.
