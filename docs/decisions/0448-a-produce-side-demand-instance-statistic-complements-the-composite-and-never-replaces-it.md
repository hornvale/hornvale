# 0448. A produce-side demand-instance statistic complements the composite, and never replaces it

**Status:** Accepted (2026-08-29) · **Decider:** Nathan (autopilot) ·
**Relates:** [0016](0016-studies-preregister-hypotheses.md),
[0387](0387-an-absent-direction-is-unknown-never-inferred.md),
[0388](0388-the-ladder-is-a-production-instrument-and-parse-robustness-is-a-separate-axis.md),
[0421](0421-the-demand-instance-statistic-complements-the-entry-score-and-never-replaces-it.md) ·
[The Quoin](../../book/src/chronicle/the-quoin.md)

In the context of a preregistration criterion (PREREG-4) requiring the
chronicle to quote `the-flood-watch`'s produce-side demand-instance figure
beside its composite total, and no such split existing anywhere in
`demand_instance_coverage` when the criterion was written, we decided to
build `demand_instance_coverage_by_direction` as its own reviewed task, ship
it publishing the split **beside** the existing composite in
`docs/audits/sentence-coverage.md`, and disclose in the artifact itself that
the split was computed after the campaign's implementation work — accepting
that the number the split reveals (31.8% produce-side, against a 34.8%
composite) is a finding the campaign selected its own rungs against but
could not, until this decision, actually report.

## Why the composite was flattering, not merely incomplete

Decision 0421 (The Rail) already established that a demand-instance count
complements an entry score rather than replacing it. This decision is one
level deeper: the demand-instance count itself composites two populations
that decision 0387 already requires the report to distinguish —
`parse`-direction entries (player lines the grammar must *read*) and
`produce`-direction entries (lines it must *generate*). The ladder is a
declared production instrument (decision 0388), so a campaign selecting
rungs from it — as this one explicitly did, rejecting `wh-question` at a
3-produce/5-parse ratio in favour of `verbless-clause` at 20/28 — is
optimizing the produce-side figure specifically. The composite the resolver
already published (393 of 1128, 34.8%) counts 490 parse-side demand
instances this campaign never touched, and per-entry demand density turns
out to be genuinely higher on the produce side (9.0 vs 7.2, a corpus
property independent of any code this campaign wrote) — so the composite
was not merely a different number, it was a *better* number than the
campaign earned, in a direction nobody would have thought to question.

## Why build it rather than report the gap

Two honest options existed when Task 6 found the instrument could not
produce PREREG-4's figure: report the gap and quote the composite (costing
nothing, fully disciplined under the campaign's own PREREG-2 —
independently-scored criteria), or build the split and disclose that it
postdates unblinding. The second was chosen because the first would have let
the chronicle commit the exact error the campaign exists to expose: a
composite the campaign selected its rungs against, quoted as if it were the
figure that mattered.

Guard rails were applied because post-hoc instrumentation is what
preregistration exists to restrain: the change is a **reporting** rule, not
a prediction, so building it falsifies nothing and rescues no number; it
shipped as its own reviewed task, separable from the Definition-of-Done task
that needed it; and both the generated artifact and this decision state
plainly that the split was computed after all five implementation tasks and
after the campaign's own reconciliation — not before, and not blind to the
result it would report.

## What ships

`demand_instance_coverage_by_direction(entries) -> (produce, parse)`, each a
`(met, total)` pair, filtering the same `demand_covered` predicate
`demand_instance_coverage` already uses by each entry's own `Direction`.
Published in `docs/audits/sentence-coverage.md` beside the existing
composite line, with the postdating disclosure stated once (on the corpus's
first section in document order) and pointed to, never restated, from the
section carrying PREREG-4's own figure — so the fact cannot drift against a
second copy of itself. Independently re-derived from the raw corpora by a
standalone script that does not call the new function, confirming no demand
token is double-counted and that direction totals match the corpus's
declared 71-produce/68-parse split.

## Consequences we accept

- **A chronicle quoting this corpus's demand-instance coverage must quote the
  produce-side figure with the composite beside it, never the composite
  alone** — the discipline decision 0421 already states for entry-score vs.
  demand-instance count now applies one level down, between the composite
  and its direction split.
- **The split is not itself a preregistered criterion under decision 0016** —
  it is a reporting instrument built to satisfy one, after the fact it
  reports was already fixed by five completed tasks, and the record does not
  pretend otherwise.
- **The parse-side figure (38.8%) is published but not optimized for** — this
  campaign selected no rung against it, and a future campaign building
  toward the parse hemisphere would be the first to test whether that half
  moves the way this one moved the produce side.
