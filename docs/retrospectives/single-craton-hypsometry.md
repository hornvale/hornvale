# Single-Craton Hypsometry — retrospective

**Completed:** 2026-07-14 (plan
`docs/superpowers/plans/2026-07-14-single-craton-hypsometry.md`)

**A pre-registered STOP rule fired, and it paid for itself immediately.**
The calibration task swept `SHELF_BREAK_LAND_FACTOR` (κ) over a fixed grid
against the whole-sphere `shelf_fraction > 0.02` floor the design spec
carried in, with the rule written down before the sweep ran: if no κ
clears the floor, stop and report rather than keep widening the grid or
quietly loosening the bound to make a number turn green. It fired — best
case 18/40 seeds at κ = 2.0, and the shortfall wasn't sweep-shaped, it was
geometric (a ~8.7%-of-sphere continent cannot put 2% of the *whole sphere*
within a narrow band of its own shoreline, no matter what multiplier scales
its shelf break). Without the rule the natural next move is exactly the
failure mode the STOP rule exists to prevent: nudge κ a little further,
call 18/40 "close enough," and ship a test bound nobody actually derived
from physics. The rule cost one extra sweep pass and returned a correct
diagnosis instead of a plausible-looking wrong one.

**The supplementary measurement is what turned an impasse into a
one-question decision, not the STOP itself.** Stopping only tells you the
current bound is unreachable; it does not tell you whether the world is
wrong or the metric is. The calibration task took one more step before
reporting: score the same sweep against a land-normalized ratio
(`shelf_land_ratio`, shelf cells over land cells) instead of the
whole-sphere fraction, at the untuned κ = 1.0. That single supplementary
run supplied everything the decision needed in one pass — the ratio
overlapped and at the median beat the default-world population's, with
Ashman's D confirming real bimodal separation, so the continent was never
thin on shelf, only being measured against a yardstick sized for a much
bigger one. Nathan's ruling (denominator wrong, not the world) took
minutes because the evidence was already assembled; a report that had
stopped at "no κ works" would have handed back an open design question
instead of a bounded either/or.

**The general lesson, restated for reuse:** a pre-registered STOP rule is
only half the safety property. The other half is that the task doesn't
stop at "the number doesn't work" — it spends one more measurement asking
*why*, along the one axis (here: the metric's own normalization) that a
human can rule on without re-deriving the whole investigation themselves.
A STOP that reports a bare failure defers the cost to the reader; a STOP
that reports a failure plus its diagnosis defers only the decision.
