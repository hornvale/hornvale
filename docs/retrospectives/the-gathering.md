# The Gathering — retrospective

**Completed:** 2026-07-13 (name-only designation per decision
`slugs-not-numbers`; plan `docs/superpowers/plans/2026-07-13-the-gathering.md`)

**TDD caught a real, previously-undetected infinite loop before it ever
shipped.** The flow-accumulation task's brief supplied a literal tie-break
for K-plateau cells that could form a two-cell cycle whenever two adjacent
cells shared an exact K value — and the task's own zero-K test fixture
(every non-zero cell at `K = 1.0`) built exactly that plateau. Red-green
development caught it immediately: the memoised path-trace hung and had to
be SIGKILLed at sixty seconds rather than failing an assertion. The fix
generalizes past this one bug — seed the tie-break at the cell's own id
(`best_id = c.0`) so `(K, id)` is a strict total order, every routed edge
strictly increases in that order, and the routing graph is acyclic by
construction, which a second reviewer proved algebraically rather than by
re-running the test longer. The brief's code was wrong in a way review of
the prose would not have caught; only running it did. The general lesson:
when a plan supplies example code for anything path-dependent (accumulation,
traversal, memoization), treat it exactly as untrusted as freehand
implementation — "the plan said so" is not evidence it terminates.

**An ideonomy-driven mid-campaign reframe produced a cleaner scope split
than the original plan had.** The spec as first written tried to land both
the carrying-capacity field *and* a fuller multi-species coexistence
mechanism (footprint-scaled ranges, competitive sharing, trophic coupling)
in one campaign. A brainstorming pass surfaced that the coexistence question
is a much richer design than a single strength scalar could hold, and that
cramming it in would mean tuning a rank-size calibration against an interim
model the richer design would immediately replace. The decisive move was
splitting it live: this campaign narrows to the field and its interim
condensation (accepting a documented, temporary loss of cross-species
spacing), and the coexistence stack drains into its own spec, building on
this campaign's field rather than being built alongside it. The result reads
as an obviously better scope than the one that shipped it — an
after-the-fact insight that came out of the tools rather than out of
pushing through the original plan.

**Absorbing main at only the stage boundary, not more often, let eighty-one
commits accumulate before the reconcile.** The campaign's one stage boundary
absorption caught main's state cleanly (a two-commit, zero-overlap delta).
Everything after that ran to completion before the next absorb was even
attempted, and by then main had moved eighty-one commits, including two
other campaigns closing (a census restructuring that renamed the very study
this campaign's calibration read from, and a kernel-units migration touching
the same file this campaign's worldgen wiring touched). The reconcile that
followed was accordingly a major operation — a dedicated merge pass, not a
small absorption — where the stage-boundary doctrine (absorb often, so
semantic drift lands next to its cause) is explicit that a late-arriving
external restructuring is exactly the failure mode small, frequent
absorptions are meant to prevent. The campaign's own long single-session
stretch (composition-root integration through calibration, with no natural
stage boundary in between) is what let the gap open; a future long-running
task-chain should treat "no stage boundary yet" as a reason to check
preflight anyway, not a license to skip it.

**A dispatch pointed at a brief path in the shared main checkout, and a
parallel session had already overwritten it.** One task's dispatch supplied
the brief's location as a path in the primary checkout rather than the
worktree; a concurrently-running campaign on a different branch had, in the
interim, regenerated that same path with its own brief. The implementer
read the wrong campaign's instructions, detected the mismatch from context
(the brief's content didn't match the spec or prior task history), and
recovered by reconstructing the correct scope from the spec and git history
rather than proceeding on bad instructions. The fix generalizes: SDD
scratch files (briefs, task reports, progress logs) belong in the worktree
from the moment they are generated, never in a shared checkout any parallel
session might also be writing to, and a dispatch should point at — and a
review should verify — worktree-relative paths as a matter of course, not
only when something has already gone wrong.

**A calibration campaign's book close depends on a census the campaign
itself invalidates, and that dependency needs to be named up front.** This
campaign's calibration added four new columns to the all-metrics census
study; regenerating that census is policy-gated to a remote path this
campaign could not exercise mid-session. The result is a campaign that
finished its code, its calibration, and its book chapters fully green except
for a known, documented, and explicitly accepted red patch — three test
files reading a now-stale census fixture — deferred to a follow-up
regeneration rather than blocking the merge on an unrelated infrastructure
dependency. Naming that seam explicitly, rather than letting a campaign's
own success quietly depend on an unstated external step, is what let this
one close on schedule instead of stalling on someone else's queue.
