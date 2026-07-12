# The Fast Gate — retrospective

**Completed:** 2026-07-11 (name-only designation per decision 0026; plan
`docs/superpowers/plans/2026-07-11-test-system-quick-wins.md`)

**Measurement killed every plausible fix before it shipped.** The session
opened with three reasonable-sounding remedies for worktree timeouts —
symlinked/shared target dirs, debug-info trimming, build prewarming — and
sixteen seconds of stopwatch overturned the entire framing: cold builds
were never the cost. Two of the three remedies were implemented before
being measured (profile trim, prewarm-as-remedy) and one had to be
reverted the same hour. The durable pattern: for performance work, the
baseline measurement is step zero, not a validation step — an A/B that
takes ten minutes is cheaper than one reverted commit. The rejected fixes
are recorded (memory + commit bodies) so they are not re-proposed.

**Reviewers caught plan-authored defects twice — the plan is not
evidence.** The gate-fast mapping table in the plan itself under-selected
for `windows/almanac` (worldgen genuinely depends on almanac; the plan
author assumed no window had incoming edges). A task-scoped reviewer
verified the mapping against the real Cargo.tomls rather than against the
brief and found it. Separately, the enumeration test shipped asserting the
exact 48/0 built/refused split its own brief said to measure-not-force —
the implementer echoed the plan's summary numbers into assertions, and the
reviewer applied the brief's text against the code. Both fixes were
one-commit. Lesson: instruct reviewers to check briefs against *reality*
(dependency graphs, ADR semantics), and never let a reviewer inherit the
plan's assumptions as ground truth.

**The registry-first grep paid for itself in one session.** Three of the
twenty ideonomy directions were already registry rows from earlier
sessions (TOOL-5, TOOL-21, TOOL-22) and became extensions instead of
duplicates; the slug-ID convention (decision 0026) meant the parallel
homophony session's simultaneous 20-row LANG batch merged with zero
collisions. One honesty note: the recording commit's body says "seventeen
new rows" — it was eighteen (one direction split into an extension plus a
new row), so the batch recorded twenty-one entries for twenty ideas.
History not rewritten; noted here.

**Crash recovery via the progress ledger worked as designed.** VS Code
died mid-task-4-review. The `.superpowers/sdd/progress.md` ledger plus
`git log` restored exact position in one read; the only re-dispatched work
was the interrupted reviewer itself. The ledger's cost is one appended
line per task; its value was the difference between resuming and
re-deriving five tasks of context.

**Absorption cadence at mini-campaign scale.** The branch lived one day
and met main only at close (three unabsorbed commits, caught mechanically
by `make preflight`, absorbed cleanly). For a single-day batch this is
acceptable; the stage-boundary absorption rule exists for campaigns whose
stages span main's movement, and the preflight's NO-GO is the mechanism
that keeps "acceptable" from silently becoming "habitual."

**Subagent-driven development fit this shape well.** Five independent
tasks, five implementers, five task-scoped reviews, two fix loops, one
whole-branch review that re-ran every measurement and reproduced all of
them. The controller's context stayed on coordination; the one structural
lesson is that review dispatches should name concrete external risks (the
dependency graph, ADR 0016 semantics) — both catches came from exactly
those named-risk instructions.
