# 0492. A preregistered hypothesis needs a preregistered instrument

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0016](0016-studies-preregister-hypotheses.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md), Task 5, §8/H2

In the context of this campaign's spec preregistering H2 ("committing does
not change what gets written into the ledger") alongside the prediction "I
expect not to be able to measure it," and then building no instrument to try,
we decided **preregistering an expectation of failure is not the same act as
preregistering a hypothesis, and a spec that does both while building nothing
guarantees an "unmeasurable" report regardless of whether the hypothesis is
true or false** — accepting that this names a methodological error in this
campaign's own spec, not merely a limitation of Task 5's execution of it.

## Context

H2 is falsifiable in principle: a preregistered, blinded twin-campaign study
(same decisions, one arm committing to a permanent ledger, one arm writing to
ephemeral scratch, independent judges scoring candour without knowing which
arm produced which text) could settle it. What this campaign's spec actually
rendered unfalsifiable was its own *operationalization* — writing "I expect
not to measure this" into §8 and stopping there. A prediction paired with an
admission that it was not expected to be tested is unfalsifiable by
construction, independent of whether the underlying claim is true.

A cheap instrument existed and went unused: this campaign's own scratch
task reports (the old, git-ignored regime) against their matching committed
ledger entries (the new regime) — same task, same author, minutes apart.
Applied post hoc to Task 5 itself, it found one confounded observation
consistent with H2 — the task's own most self-critical line survived in
scratch and did not survive into the committed ledger, while mechanical
content carried through verbatim. Not a confirmation: an unblinded, single,
post-hoc pair cannot distinguish "committed, therefore softened" from
"second pass, therefore tightened, and the second pass happens to be the
committed one." Reported with that limitation stated, not sold past it.

## The rule

When a spec preregisters a hypothesis it expects to be unable to measure,
that expectation is itself a design decision that needs justifying, and
"nothing was built" is a choice, not a fact about the hypothesis. Preregister
the instrument, or preregister that none is planned and why that is
acceptable for this campaign's scope — but do not let the second read as the
first.

## Consequences

- This decision does not retroactively build the twin-campaign study H2
  actually needs; it is banked so a future campaign proposing to measure
  candour-under-commitment does not repeat the same unfalsifiable shape.
- The retrospective carries this as a process lesson because it belongs
  there, not filed as "structural, not a defect" — the earlier framing this
  decision corrects.
