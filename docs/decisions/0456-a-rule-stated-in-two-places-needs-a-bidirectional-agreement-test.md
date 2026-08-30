# 0456. A rule stated in two places needs a bidirectional agreement test

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0148](0148-a-merge-runs-four-phases-and-probes-run-by-hand.md),
[0426](0426-the-heavy-tier-is-a-phase-of-the-queue-again.md) ·
[The Attestation](../../book/src/chronicle/the-attestation.md)

In the context of `scripts/lane-sets.tsv` declaring itself "THE SINGLE SOURCE
OF TRUTH" for which phases belong to which job kind, and `scripts/sluice-
run.sh` restating that as two hand-written literal lists nothing checked
against it, we decided that **a rule stated in two places is only as sound as
the test comparing the two copies, and a one-sided test — each item in list A
has a row in the roster — is not that test**, accepting that writing the
two-way version costs one more assertion than the one-way version it
replaces.

## Context

`cli/tests/suite/lane_sets.rs`'s existing check asserted that every phase
named in `sluice-run.sh`'s `merge_phases`/`stage_phases` has a matching row in
the roster. That is satisfied by a phase sitting in the *wrong* list — and is
exactly how decisions 0148 and 0426 moved `heavy` between the two lists, in
opposite directions, four months apart, with nothing objecting either time.
The roster's own `rung` column already states which phases belong to `stage`
and which to `merge`; nothing compared it to the lists it was meant to govern.

## The rule

`the_phase_lists_and_the_roster_rungs_agree_both_ways`
(`cli/tests/suite/lane_sets.rs`) computes the set the roster's `rung` column
*implies* for each list and diffs it against the list's actual membership, in
both directions: a phase the rungs imply but the list omits, and a phase the
list carries that no rung implies. It deliberately does not assert *order* —
the roster is ordered by rung, the chamber by execution sequence, and the two
are transposed on purpose (`artifacts` must run before `gate`, which the
roster does not encode).

Proved discriminating both ways before landing: moving `heavy`'s rung to
`campaign` without touching the list failed, naming `heavy` as "in the list
but not implied by any rung"; deleting `heavy` from `merge_phases` without
touching the roster failed the other direction, naming `heavy` as "in the
roster's rungs but not in the list" — reproducing the real historical defect
exactly.

## Consequences

- The next time a phase moves between rungs, the two files disagreeing is a
  build failure, not a silent divergence caught only by whoever next reads
  both files side by side.
- The check's own doc comment states the direction it enforces and what it is
  blind to (order), which the campaign later held itself to when reviewing a
  Minor on this task: a check whose stated blindness is not quite true is the
  same defect this campaign exists to remove, shipped in its own flagship
  fix.
- The shape generalizes: any rule this project states in two independently
  editable places (a roster and a restatement of it, a schema and a parser)
  is a candidate for the same both-directions test, not a one-sided
  membership check.
