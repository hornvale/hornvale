# 0132. Three gates, named for the campaign moment

**Status:** Accepted (2026-08-14) · **Decider:** Nathan

In the context of a commit gate that had come to run a merge-gate workload at
commit frequency, we decided to split it into three gates — one per moment in
a campaign's life, not one per machine or one per bundle of suites — and to
retire the one tool that had tried to buy iteration speed by scoping tests
instead of the build.

## The problem

`docs/timings.md` shows the full workspace gate running **417 times in a
month at an average 423 s — 49.0 h of Mac time** — while `make quick`, the
cheap lint-only half, ran 15 times at an average 16 s (0.1 h). The same 417
calls, made against the cheap half instead of the expensive one, cost **1.8 h**
instead of 49.0. Every commit was paying for a merge-gate's worth of
assurance whether or not the commit needed it.

`make gate-fast`, documented as "ITERATION ONLY", was supposed to be the
cheaper alternative for that frequency. Measured rather than assumed: **381 s
against the full gate's 423 s** — a 10% saving (n=4, all the evidence there
is). It scoped *tests* to changed crates, but could not scope the *build*,
which is where nextest's cost actually sits — compiling and linking test
binaries dominates wall time, and a subset of tests still requires building
every binary that might contain one. `gate-fast` never delivered the speed it
promised, and the purpose split below leaves it no job to do.

## The ruling

**Three gates, named for the moment in a campaign each one gates, not for the
machine it runs on or the suites it bundles:**

```
  make gate-commit     local, seconds         every commit
  make gate-stage      lane, minutes          each plan-stage boundary
  make gate-campaign   lane, tens of minutes  before merging the campaign
```

Naming by moment, not by machinery, follows the aviation-checklist
convention: a before-takeoff checklist is named for the phase of flight, not
for the subsystems it happens to touch, so the caller who needs to know *when*
to run it never has to first learn *what* it contains. `gate-stage` and
`gate-campaign` still bundle several suites — the roster lives in
`scripts/lane-sets.tsv`, the single source of truth so this record does not
duplicate it — but a caller only needs to know which moment they are at.

**Prefix, not suffix.** `gate-commit`/`gate-stage`/`gate-campaign`, matching
the existing `gate`/`gate-fast`/`gate-full`/`gate-remote` family, so `make
help`'s alphabetical sort keeps the family together.

**`gate-fast` is retired.** It is deleted rather than repointed, on the
measurement above: it bought ~10% over the full run, could not reach the
actual cost driver, and the purpose split gives its intended job — fast,
partial, continuous feedback — to `gate-commit` instead, which reaches a
different tier of tests entirely (the ones recorded fast in the committed
timing baseline) rather than a subset of the same tier.

**`make gate` and `make ci` become refusing signposts, not aliases.** Each
prints the three replacement commands and exits non-zero, rather than
silently resolving to one of them. Aliasing `gate` to the commit gate would
change what 417 monthly calls mean without telling anyone: a caller expecting
full-workspace coverage would receive lints and a fast test tier instead, with
no warning that anything had changed. This is the same shape the project's own
bash guard already uses when it intercepts a bare whole-workspace test
invocation and names the project's real targets instead of running it.

## Consequences

- The commit gate's coverage is bounded by design, not merely by convention:
  a test with no recorded baseline duration is excluded from it and enters on
  the next green stage gate, which measures it and rewrites the baseline.
  Coverage is the stage gate's job; the commit gate is a speed tier.
- `gate-stage` and `gate-campaign` dispatch rather than run in place — where
  they run, and under what serialization, is the question decision 0133
  answers.
- Every caller who typed `make gate` or `make ci` from habit is told the
  replacement immediately rather than silently getting a narrower gate.

## Supersedes

Nothing. `gate-full` is superseded in substance — `gate-campaign` strictly
contains what it covered — but no prior record named `gate-full` as its
subject, so there is nothing here to mark superseded; its retirement is
recorded as a consequence of this ruling instead.

## See also

`docs/superpowers/specs/2026-08-14-the-staff-design.md` §1, §2, §2.0;
`scripts/lane-sets.tsv` (the set roster); decision
[0040](0040-nextest-is-the-gate-runner.md) (the original ~4-minute
budget this record's arithmetic is measured against).
