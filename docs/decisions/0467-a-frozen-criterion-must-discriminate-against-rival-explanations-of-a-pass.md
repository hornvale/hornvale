# 0467. A frozen criterion must discriminate against the rival explanations of a pass

**Status:** Accepted (2026-08-30) · **Decider:** Nathan · **Extends:**
[0016](0016-studies-preregister-hypotheses.md)

In the context of preregistered measurement, facing a criterion that carefully
enumerated three ways its comparison could come out and never asked what *else*
could produce the favourable one, we decided that **a frozen criterion must name
the rival explanation of a pass and the statistic that separates it, not only
the ways the measurement can fail**, accepting that this makes preregistration
more expensive to write and requires thinking about the mechanism before the
data exists.

## Context

The Winze froze a branch table over the depth distributions of breached versus
ordinarily-ended delvings:

```text
indistinguishable        -> the survivorship claim is FALSE, the mechanism is decoration
deeper, with overlap     -> the claim holds
deeper, no overlap       -> investigate; the hazard became a threshold in disguise
```

The measurement landed on row two and nothing was tuned. But **a pooled
comparison of two depth distributions cannot separate the claim the campaign was
making from a much weaker one.** *"Breached delvings sit at their own maximum
without being selected for depth"* and *"breach is a tenure lottery and depth is
a bystander"* produce **identical pooled distributions**. Breached median tenure
is 17.5 epochs against 3.0, so the weak reading was live and large.

The statistic that closes it — conditioning on tenure, under which the
separation attenuates from AUC 0.8654 to 0.7599 and **survives**, direction
holding in every stratum — is not in the criterion. It reached the task through
a dispatch that named the property without prescribing the statistic. Had the
stratified result collapsed, the criterion as written would have reported a
pass on a mechanism that was decoration.

## What was decided

A preregistered criterion is not complete when it enumerates outcomes. It is
complete when, for the outcome that would be read as support, it also states:

1. **the rival explanation** — the other mechanism that would produce that same
   outcome; and
2. **the statistic that separates them**, computable from the same run.

Where no such statistic exists, the criterion says so, and the pass is
preregistered as consistent-with rather than as support.

## Consequence

**A branch table over outcomes is not a discriminating test, and both look like
rigour.** That resemblance is the whole hazard: enumerating three outcomes reads
as careful, and is careful about exactly half the question. The failure is
invisible to re-reading, because re-reading checks a criterion against the model
that produced it.

**This is a sibling failure to writing a rule about a proxy.** The same campaign
froze a panel rule whose stopping threshold was set on *mines* as a stand-in for
*breaches*, with a 7× error in its only conversion: applied literally it stops
at three breaches and would have made the comparison unmeasurable **while
reporting itself satisfied**. There the rule was written about the wrong
quantity; here about the wrong question. The shared operational remedy is
recorded with them: **execute a preregistered rule against a dry run before
freezing it** — freezing a rule nobody has run is freezing an untested program.

**What this costs.** Preregistration gets harder, and it gets harder in the
place where a campaign is least equipped — before the mechanism has produced any
data. That cost is accepted because the alternative is cheaper only in
appearance: a criterion that cannot discriminate still consumes the same
canonical-box time, still ships a committed witness, and reports a verdict
nobody can rely on.

**What is not changed.** 0016 stands unaltered: the freeze still happens in the
campaign's spec, before the code that would move it, and nothing mechanical
compares a result to it. This decision constrains what the frozen text must
contain, not when it is written or what enforces it.

## See also

- Spec `docs/superpowers/specs/2026-08-19-the-winze-design.md` §5.2, E.9, E.10.2.
- `windows/worldgen/tests/suite/survivorship_probe.rs` — the witness, including
  "what would change the verdict".
- The Winze retrospective.
