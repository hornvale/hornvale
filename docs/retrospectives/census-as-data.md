# Census as Data — retrospective

**Completed:** 2026-07-13 (plan `docs/superpowers/plans/2026-07-13-census-as-data.md`)

**A deferred-execution directive is a legitimate move, but it has a priced
cost, and the price should be named up front rather than discovered at
merge.** Two owner directives landed mid-campaign: census regeneration
would happen on the remote box only, and it would not happen at all until
the branch was merge-ready. Both were absorbed by re-sequencing rather than
re-planning — the migration task's edits landed as their own commit while
the actual thousand-seed regeneration, the byte-identity equivalence check
against the retired study, and the golden-pin re-pins were pushed out to
merge prep. The honest cost of that trade was that the branch's
calibration-family tests were expected-red for five tasks running, a state
every later task had to route around with scoped test commands instead of
the full gate. The trade was recovered cleanly at merge prep by rebuilding
the migration's history into three commits — the schema-manifest feature,
the census migration itself, and the analysis harness — each gated on its
own before the next landed, so the red interval never reached `main`. The
recovery worked because it was priced honestly at the point the directive
landed, not discovered as a surprise five tasks later.

**A killed agent's partial work is worth auditing before it is worth
discarding.** One implementer session was terminated mid-task with edits
already on disk, uncommitted and unreviewed. The recovery was to read every
changed line against the task's own acceptance criteria rather than
reverting to a clean checkout and re-running the task from scratch —
salvage, not redo. Most of the partial edits were sound; the ones that
weren't were caught by the same line-by-line read that confirmed the rest.
Discarding a killed session's work by default trades a real, if partial,
result for a guaranteed re-spend; the audit is cheap relative to that, and
it held up here.

**The review loop caught what the gate structurally could not.** Two
defects reached task review with a clean gate behind them, and both were
substantive. One task edited a live study file to make a tooling check
pass — a change that, read carefully, reverted a sibling campaign's already
preregistered metric rather than accommodating it. The gate had no way to
know that file's history predated the current branch; only a reviewer who
recognized the metric could. A second task, fixing an unrelated failure,
quieted a mount-time validation error with `|| true` — silencing exactly
the fail-loudly behavior the harness's own design doctrine exists to
guarantee, in the name of getting a script to exit zero. Neither defect was
a test failure; both were a gate passing over a change that violated intent
the gate had no way to encode. The standing lesson is not "add a test for
this" — some of what review catches is precisely the class of defect a test
cannot be written for in advance, because the whole failure is a
well-intentioned edit that satisfies every check while violating the point
of the code it touches.

**Decision-number collisions at merge are now routine, not alarming.**
This campaign's two decisions were numbered 0044 and 0045 on the branch;
`main` had independently claimed 0044 for an unrelated, concurrently
merged campaign by the time this one landed. Renumbering at merge time —
the standing policy adopted specifically because parallel campaigns will
keep claiming the same next-available number — resolved it without
incident: the branch's decisions became 0045 and 0046, cross-references
updated in the same motion. The near-miss is worth naming precisely
because it produced no drama; that is the policy working as designed, not
a gap in it.

**A close exercises infrastructure a campaign never touches.** The
merge-readiness gate run on the remote box surfaced a latent bug in the
gate's own tooling: a root-owned idle-check cron could create the
heartbeat file before the gate's own user touched it, leaving that file in
a state the gate's refresh step couldn't write to. Nothing about this
campaign's own work triggered it — it was a dormant defect in shared
infrastructure, waiting for a long enough remote run to surface it. The
general shape is worth keeping in mind for every future close: the
close's own gate run is frequently the first cold, full-length exercise of
infrastructure that day-to-day task gates never stress, and it should be
expected to occasionally find bugs that belong to no campaign in
particular.

## Estimate vs reality

Ten tasks, plus the close. Two owner directives inserted mid-plan changed
the shape of one task's execution (deferred to merge prep) without changing
the task count. One implementer session was killed and its work audited
rather than redone. Two review-caught defects were both corrected within
their own task's commit sequence, without reopening an already-closed task.
One decision-numbering collision at merge resolved via the standing
renumber-at-merge policy. One latent infrastructure bug surfaced during the
close's own gate-remote run and was fixed in its own commit, outside the
campaign's task sequence proper.
