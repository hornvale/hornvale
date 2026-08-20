# The Touchstone — retrospective

**Merged:** 2026-08-19 · **Program:** Myth, campaign 6

Process lessons only. The results are in
[the chronicle](../../book/src/chronicle/the-touchstone.md). This was a
*measure* campaign, not a mechanism one: it built the instrument the last three
myth campaigns needed and did not have, and preregistered the discrimination it
had to pass. Unusually for this thread, the substrate did not move under it —
`origin/main` never left the branch's merge-base — so the readout needed no
re-derivation at the close. That absence is itself a lesson, recorded below.

## The one substantive defect was in the controller's plan text, caught by arithmetic not reading

The recurring myth-thread pattern held for a fourth campaign: every defect that
mattered originated in *my* text, and the one that mattered here was caught by
reasoning about real numbers rather than by reading the prose.

The plan offered the **accumulation-rule swap** (additive ↔ multiplicative) as
a cheap positive control — a change the aggregate would supposedly miss. It is
not a dissociation case at all. The Undertow had already measured that swap
moving the aggregate 11 → 43, which is the aggregate working correctly and
loudly. Offered as the positive arm, it would have preregistered the instrument
against a stimulus the *aggregate already sees*, so a touchstone that lit up
would have proven nothing the aggregate does not. The self-review caught it
before execution and corrected it: the only valid positive control is the
**selection-rule swap** (prior 41.9% held-telling churn against ≤4/100
aggregate), and the plan's Tasks 1 and 4 were fixed to use it.

The transferable rule is the thread's standing one, now on its fourth instance:
**a control's premise is a claim about numbers, and the cheapest place to check
it is the number, not the sentence.** "The aggregate misses this" is falsifiable
from a figure a predecessor already measured — and the predecessor had.

## The negative control was mutation-verified by the controller, not trusted from a report

The negative control rests on a code theorem: under `Contact::Descent`, a
holder whose whole ancestry is one people pays a zero crossing penalty on every
step under both `Crossing` arms, so the two arms produce bit-identical claims.
A theorem asserted by a test that never fails is indistinguishable from a
vacuous one, and this thread has shipped vacuous controls before.

Rather than accept the implementer's "mutation-proven" claim from the Task 1
report, the controller re-ran the mutation directly: deleting the `from == to`
guard in `crossing_penalty` and confirming the negative-theorem test reddens
genuinely — `Free` and `ContactWeighted` diverging on same-people steps (rung
0 → 1, remembered day 6300.0 → 6250.0), then restoring the guard and confirming
green again. The provable-zero floor is non-vacuous, and it was the controller
who held the evidence for that, not a relayed sentence.

The rule: **a "mutation-proven" claim in a subagent report is a hypothesis
about a test, and the verification is cheap enough to repeat.** A negative
control is the one place a vacuous green is invisible by construction — it is
*supposed* to pass — so it is the one place the controller should re-fire the
mutation personally.

## Substrate drift, disclosed and not rescued

The Undertow measured the selection swap moving the *value* of the held telling
at 41.9% of holders. On this tree the strongest value-change is recency's
38.37% — below 40%, and below the prior figure. The substrate has moved since
41.9% was measured.

This was recorded as a finding at the freeze, not repaired. The frozen success
floor stayed at 0.20; it was **not** lowered to flatter the drifted number, and
the discrimination clears the unlowered floor more than threefold (62.64% held-
telling churn). The dissociation *property* reproduced robustly; only the exact
magnitude drifted. This is the thread's own "a committed baseline is a claim
with a date" — and the discipline that matters is that the disclosure landed in
the same freeze that carried the number, before any measurement code existed to
tune against it.

## A tested-nodes / untested-seam gap, found in review and closed with a test

Task 2's traced walk carried one piece of genuinely new logic with no
`derive.rs` analogue: the `Carrier::Descent`-before-`Carrier::Seam` tie-break
for a hearer reachable both as a founding-tree child and as a raid peer. It was
documented as deliberate but exercised by no test — the agreement battery could
not see it (`Claim` carries no `Carrier`), and the existing route fixture had no
doubly-reachable hearer. A future inversion of that tie-break would have
mis-attributed `carrier` silently, and `route_changed` compares crossings
including the carrier.

Review caught it, and the fix added a fixture (a child that is also a raid peer)
asserting `crossings[0].carrier == Descent`, mutation-proven by reordering the
enum discriminants and watching the new test redden. This is the classic
"tested nodes, untested seam" shape: every node in the walk had assertions, and
the one new *edge* between two behaviours had none. **New logic with no upstream
analogue is a seam by definition; the fact that the surrounding nodes are all
tested is exactly what makes the seam easy to miss.**

## The extract-to-`tests/common` ruling avoided a fourth copy

The positive control needs the enumerate/select/`Selection`/`Candidates`
machinery, which Task 1 had ported into its probe — a third copy of code that
already lived in `probe_tiebreak_rules.rs` and `probe_argmin_defect_crossing_arms.rs`.
Task 4 needed it too. The ruling was to **move** Task 1's copy into
`tests/common/mod.rs` as `pub` and point both consumers at it, rather than mint
a fourth copy, with a mandatory byte-identity re-run of Task 1's frozen
signature after the move to prove the relocation was behaviour-preserving (it
was, to the digit). Full consolidation of the three surviving copies is banked
as a `TOOL-` row rather than attempted inside a measurement campaign.

The rule: **a relocation is safe in a way a reimplementation is not, and the
review rubric treats verbatim duplication as a defect** — so when a fourth
consumer appears, move the shared code to a common seam and re-verify identity,
rather than either copying again or rewriting.

## The measurement-consistency posture was correct, and turned out moot

The controller ruled that main absorption be deferred until *after* Task 4's
readout, because the Touchstone is a preregistered measurement: Task 1 froze the
control populations on one substrate, and Task 4 measured the discrimination on
that same substrate. Absorbing between freeze and readout would have moved the
physics under the measurement (the "never absorb mid-measurement" exception).

It turned out moot — `origin/main` never moved off the branch's merge-base, so
the frozen substrate *is* current main and the readout needed no re-derivation.
But the posture was correct independent of the outcome, and the Undertow is the
proof: it ran 137 commits without absorbing, a predecessor moved settlement
placement, and a textually-clean merge silently invalidated every number it
had written. The lesson the Undertow paid for is *when* to absorb, not *whether*
— and "finish the readout, then absorb once and re-derive" is the rule that
would have saved it. This campaign followed it and drew a lucky no-op; the next
one should follow it regardless.

## The thesis was confirmed by a cross-check, not asserted

The touchstone's positive tail is 63.77%, which slightly *exceeds* Task 1's
route-blind `Claim`-diff of 62.64% over the same population. The tempting error
is to read a higher number as over-counting and quietly reconcile it. The honest
reading is the opposite: the `Claim` carries the day, rung and hops but not the
route or width, so the surplus 36 holders are exactly the ones whose winning
witness or accumulated width moved while their `Claim` did not — the instrument
seeing what a `Claim`-diff *cannot*, which is the entire reason it was built.
The day and hops channels reproduce the `Claim`-diff to the digit (1,219 and
1,990), which is what licenses the reading: the two channels that *should* match
do match exactly, so the excess is provably confined to the two channels outside
the `Claim`.

The rule: **when a new instrument reads higher than the old one it was meant to
subsume, the finding is which channel carries the difference, not the difference
itself.** A surplus confined to exactly the channels the old instrument was blind
to is confirmation; a surplus bleeding into the shared channels would have been
a bug. Name the denominator of the difference before calling it either.

## What did not need doing, and why that is worth a line

No `docs/decisions/` record was minted. The spec's §8 decisions plus the
idea-registry rows are the durable record for this campaign, and there was
active decision-number contention on the board — minting a number to race two
other sessions for it is exactly the duplicate-`0134` shape the merge queue
exists to prevent. If a constitutional decision is later judged warranted (the
"measure before mechanism" sequencing rule is a candidate), it should be minted
against the merged tree, not raced for during the close.
