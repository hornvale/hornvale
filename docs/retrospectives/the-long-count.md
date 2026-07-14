# Retrospective: The Long Count

**Closed:** 2026-07-14 · **Outcome:** merged (re-scoped) · one-page, process
lessons only (decision 0020).

## What happened

Spec approved in the morning; thirteen minutes later a parallel session
committed the *Eclipse Seasons* spec claiming the identical `moon-nodes`
stream and dated-eclipse core. The collision surfaced not at spec time but
at **worktree-creation time** — a `git log` after `git worktree add` showed
the foreign spec commit as this branch's parent. Nathan adjudicated within
the hour (cede the eclipse core), the spec was amended, the 12-task plan
rewritten to 6 tasks, and the re-scoped campaign executed same-day under
subagent-driven development: 6 implementer dispatches, 6 task reviews, 3
fix passes, one final whole-branch review, all clean.

## Lessons

1. **Same-day spec collisions are now the live failure mode, not merge-day
   ones.** The preflight machinery guards commits and merges; nothing
   guards two brainstorms claiming one registry row within minutes. The
   cheap check that would have caught it: before writing a spec, `git log
   --since=today --oneline main -- docs/superpowers/specs/` plus a grep of
   the target registry rows in freshly-landed specs. Worth mechanizing into
   the brainstorm entry ritual (registry-first now needs to be
   *main-tip*-first too).
2. **Ceding cleanly beat racing.** The adjudication cost one spec amendment
   and a plan rewrite (~1,100 plan lines deleted); racing would have cost a
   save-format collision on a permanent stream label. The deciding factor
   was that the other spec was *deeper* on the contested ground —
   ownership followed depth, and the shallower campaign kept everything
   non-overlapping plus a follow-up seam (standstills) the deeper spec had
   explicitly deferred.
3. **Plan code samples are schematics, not gospel.** Three review findings
   traced to plan snippets: a decision-record block that didn't match the
   decision-log template, kernel Ledger API signatures that had drifted,
   and a test fixture name that didn't exist. The implementers' correct
   move each time was repo-over-plan; the plan-writing lesson is to copy
   templates and signatures from the tree verbatim, or point at them
   rather than transcribing.
4. **Stage-boundary absorption was missed — and mattered less than usual
   only by luck.** The branch met main for the first time at close
   (24 commits, three-way conflicts in metrics.rs/worldgen/type-audit
   report). The campaign ran a single day, but The Ground merged mid-flight
   and its census metrics collided with ours in the registry-count pin.
   Same-day campaigns still owe at least one mid-flight absorption.
5. **The census-staleness precedent held its shape** (identical to
   The Self-Describing Sky's close): metrics moved the schema 117→124
   across two campaigns; the 32 fixture-reconstruction tests stay red until
   ONE shared AWS regen after both this and Eclipse Seasons land. Sharing
   the regen was the right call; the follow-through now lives with Eclipse
   Seasons' close.
6. **Autopilot's G5 worked as specced on its first real run here** —
   seven ledgered decisions, two hard stops honored (G3 pre-dated the
   overlay; G6 presented and approved), zero mid-execution questions to
   Nathan that precedent could answer.

## Follow-ups

Carried in the worktree's `.superpowers/sdd/followups.md`, headline items:
standstill facts on Eclipse Seasons' seam; retrograde-mirror and skip-path
tests; sightline-display unification onto the committed fact; the
Degrees-family type-audit adoption point (wave-1 debt grew 139→148).
