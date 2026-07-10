# Campaign 25 (The Measured Coast) — retrospective

**Merged:** 2026-07-09

**Recurring findings.** None new — no reviewer flagged an issue this
campaign had already been warned about in a prior retrospective. One
small, contained duplication surfaced instead: the render lens
(`domains/terrain/src/render.rs`) needed the same angular-distance helper
already private to the shape metrics (`shape.rs`), and the plan deliberately
kept two private copies rather than sharing one across independent modules
in the same crate. That is a defensible call for two ten-line leaf
functions with no third caller in sight, but it is exactly the kind of
"cheap to duplicate the first time" debt Campaign 21's retrospective named
in a different corner of the codebase — worth a glance if a third copy ever
wants to exist.

**Estimate deltas.** The Census of Coasts' full 10,000-seed run took 4m46s
wall time (20m42s user, multi-core) against a same-scale study's ~3m40s
estimate carried over from the prior task — a real but modest overrun, not
one that changed the plan's shape. Every other task landed within its
brief's implicit scope on the first pass; no task needed a second attempt
at its core implementation.

**Spec vs. reality.** The campaign's own spec (§2) proposed a fourth
committed study, `census-coasts-drift.study.json`, as the CI-drift half of
the new metrics — the house pattern every prior census family follows. But
by the time the metrics landed (Task 4), all three of CI's existing
`all`-metric studies (`census-lands-drift`, `census-of-the-meeting`,
`census-of-skies`) already carried the six new columns for free, because
they select `"metrics": "all"` rather than a named subset. A fourth study
would have re-run an identical build under a different name — caught during
planning, before a line of the redundant study's JSON was ever written, and
folded into this task as a one-paragraph spec amendment instead of surviving
as an unused artifact somebody had to notice and delete later. This is the
inverse of the more common failure mode (a plan under-specifying something
reality then has to patch); here the plan over-specified, and the fix was
subtraction rather than addition.

**Do differently next time.** The fixture-guard-first ordering (Task 1
landed the `lens_purity` world-identity test before any shape or render
code existed) paid for itself exactly as intended: every one of the five
following tasks carried its own proof that world identity hadn't drifted,
and the guard never once needed to be consulted or debugged because nothing
ever tripped it. Continue reserving a task-zero slot for a fixture guard on
any future campaign that makes an explicit "nothing here changes X"
contract — the value is in the guard existing *before* the tempting code
does, not in retrofitting it once six commits already need trusting.
