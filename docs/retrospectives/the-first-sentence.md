# Retrospective — The First Sentence (C1)

One page of process, not product. C1 was the first campaign of the Self-Writing
Book program, executed subagent-driven (10 tasks + fixes), gate-full green.

## What the final whole-branch review caught that per-task review could not

Every task passed its own spec+quality review, yet the whole-branch review (run
on the most capable model, as the method prescribes) found two real defects, and
both lived at *seams between tasks*:

- **Dead API across a T6→T7 seam.** T6 built `uncovered_predicates`; T7 built the
  CLI; neither wired them, so the spec's coverage report was invisible dead code.
  No single-task reviewer could see it — each task was internally complete.
- **An injected-components inconsistency.** The planet stage re-assembled the
  *canonical* components instead of the `wc` already in scope, breaking the Lab's
  solo-roster path. Only a reviewer holding the whole build flow could spot it.

Lesson, reinforcing the method: the final whole-branch review is not ceremony —
it is the only reviewer positioned to see cross-task seams, and it earns its
(more expensive) model.

## The type-audit blind spot is real and recurred

Tasks 2 and 3 added `pub`-boundary primitives without `type-audit:` tags. Neither
the per-task reviews nor `make gate` caught it — type-audit is CI-only. It
surfaced only because a *later* task happened to run the tool. The standing trap
([[type-audit-invisible-to-make-gate]]) held exactly as recorded. Cheapest
mitigation for future campaigns: a reviewer instruction to run `type-audit check`
whenever a task adds a pub-boundary primitive, since the gate will not.

## Two old lessons re-earned

- **Golden re-pins belong in the drifting commit, not the close.** The plan
  wrongly deferred them to the last task; the review and
  [[rebaseline-golden-pins]] corrected it — `world-seed-42.json` was re-pinned at
  T1 (registry) and T4 (facts), keeping the branch green throughout.
- **Registry-first is not placed-first, again.** The dominant-people measurement
  first ranked over the *registry*, not placed peoples, and named a world after
  `bugbear` — the canonical case from [[registry-first-is-not-placed-first]]. The
  regression test now reproduces it.

## The close ran into a parallel-campaign collision

C1 executed start-to-finish without a stage-boundary absorption, and by close
main had moved **21 commits** across two campaigns (the-individuation, the-real-
sky). The absorption was mostly mechanical (a `from_stores` arity drift, a
`SUMMARY` reorder, regenerated artifacts), but it surfaced a **semantic
collision preflight cannot score**: the-individuation shipped a canonical
`instance-of`/`kind_of` kernel classification at the same time C1 added `is-a` —
two classification predicates, the [[the-faces-shipped-the-moons-ticket]] class.
Resolved by coexistence (functional immutable class vs mutable roster-kind), with
the unification decision deferred to C2 by owner ruling. For a fast campaign the
absence of a mid-flight absorption was cheap; for a slower one it would not have
been.

## Tooling notes

- `task-brief` wrote to the *main checkout's* scratch, not the worktree, when run
  from the wrong cwd (T2's implementer fell back to the plan doc). Run brief
  generation from inside the worktree.
- The close gate starved under a load average near 50 from parallel sessions —
  contention, not failure ([[worktree-gate-timeouts]]). The long timeout is the
  guard.
