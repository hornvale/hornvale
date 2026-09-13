# 0959. The Winze §5.2's conditioned claim is unevaluated, not refuted

**Status:** Accepted (2026-09-12) · **Decider:** Nathan · **Campaign:** The Tidemark

## Context

The Winze §5.2 makes two claims about breached workings, and
`windows/worldgen/tests/suite/survivorship_probe.rs` has been asserting both:

1. **Pooled** — breached workings are deeper than ordinarily-ended ones.
2. **Conditioned** — breached workings are deeper *for their tenure*, i.e. the
   pooled gap is not merely composition.

The Tidemark added six marine peoples; Underworld Peoples had added four
subterranean ones. Neither campaign alone moves this test — both were measured
green on their own branch tip. On the merged 49-kind roster the conditioned
assertion went red at stratified z 1.326 against `Z_SUPPORTS` 1.96.

The module's own "WHAT WOULD CHANGE THE VERDICT" names that outcome in advance
and fixes the response — *report it, **never** tune `BREACH_FREE_PATH_M`* — so
the question was never whether to tune, but what the red actually means.

## What was measured

Investigation found the stratification could not hold tenure fixed. The bake
runs 1–80 epochs and `STRATA`'s top bucket was `21+`: 75% of the tenure range in
one bucket, carrying 42% of the pair mass, with depth varying roughly fourfold
inside it from tenure alone. That is a failure at its stated job **whether or
not the test passes** — a latent specification defect that became load-bearing
only when 59% of breached workings migrated into that bucket.

A repair was preregistered before any of its code existed
(`docs/superpowers/specs/2026-09-12-the-tidemark-survivorship-amendment.md`,
committed at `750be0be2`), replacing the frozen literal with quintiles of pooled
tenure — the *rule* frozen, the cut points derived. Both outcomes were named in
advance.

Under strata that do hold tenure fixed, the conditioned statistic went to the
null: stratified AUC 0.4977, z −0.034. The pooled statistic did not move
(AUC 0.7959, z 5.244).

**But the null is produced by four observations:**

| stratum | breached n | ordinary n | pairs | % of mass | AUC |
|---|---|---|---|---|---|
| Q1 e≤2 | 1 | 52 | 52 | 8% | 0.144 |
| Q2 e≤6 | 3 | 26 | 78 | 12% | 0.282 |
| Q3 e≤10 | 5 | 29 | 145 | 22% | 0.600 |
| Q4 e≤21 | 4 | 26 | 104 | 16% | 0.481 |
| Q5 e>21 | 19 | 15 | 285 | 43% | 0.575 |

The two strata resting on one and three breached workings carry 20% of the pair
mass and hold the two most extreme AUCs. Pair-weighted AUC over the three strata
with breached n ≥ 4 is **0.5635**; over all five it is **0.4976**.

## Decision

**§5.2's conditioned claim is recorded as UNEVALUATED. It is not refuted, and
the registry status does not become `refuted`.**

A negative verdict decided by one and three observations is not a negative
verdict. `refuted` (decision 0131) asserts something the data cannot carry here.

**The root cause is sample size, and it was recorded before this campaign
existed.** The module's own doc states that spec amendment E.4.2 "fixed the
wrong quantity: it set its stopping threshold on **mines** as a proxy for
**breaches**, and its stated conversion assumed a breach fraction near a third
against a measured 13.3%." The panel is denominated in mines; the conditioned
question is denominated in breaches. Measured yield is **2.67 breaches per
seed** (32 across E.9's twelve), so five-way stratification needs:

| breached per stratum | breaches | seeds |
|---|---|---|
| ~10 | 50 | ~19 |
| ~20 | 100 | ~38 |
| ~30 | 150 | ~56 |

The pooled test requires no stratification and is unambiguous at n = 32.

**Consequences:**

- The probe **asserts** the pooled separation and **reports** the conditioned
  readout with its per-stratum n's visible, rather than asserting a claim on
  four observations. This is not a relaxation of `Z_SUPPORTS`, which is
  untouched; it is declining to assert what the panel cannot bear.
- `BREACH_FREE_PATH_M`, `Z_SUPPORTS`, `Z_DECIDES`, `DELVE_M_PER_PERSON_EPOCH`,
  `PANEL` and `CONTROL` are all unchanged.
- **Extending the panel belongs to The Winze**, not to The Tidemark. The
  arithmetic above is handed over; the seed count is E.9's to amend.

## Consequences not resolved here

- **Right-censoring.** The 89 still-open workings are the deepest population
  (q3 1171 m against breached 802 m) and are excluded. The preregistered
  sensitivity arm measured the exclusion's size (AUC 0.3946 conditioned,
  0.6810 pooled) rather than correcting it. Mann–Whitney over completed cases
  is not the right instrument for a censored survival question; a hazard model
  is. That is The Winze's to take up.
- **The repair's own limitation.** Pooled quintiles are dominated by the
  ordinary group (148 of 180 ended), so they are effectively ordinary-tenure
  quintiles and the breached pile into Q5. A design balancing the breached group
  would be better. It was **not** re-cut after the result was seen — re-cutting
  after a null to obtain a different null is fitting under another name — and it
  would not change the verdict, since no stratification supports five strata at
  n = 32.

## Why this is recorded rather than fixed quietly

Two campaigns, each green alone, together moved a third campaign's preregistered
invariant. No gate saw it, because each gate saw a green product. The finding
that survives is not about either campaign: it is that **a conditioned claim was
being asserted on a panel sized for an unconditioned one**, and the mis-sizing
was documented at E.4.2 before any of the three campaigns existed.
