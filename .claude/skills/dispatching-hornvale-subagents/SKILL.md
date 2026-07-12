---
name: dispatching-hornvale-subagents
description: Use when dispatching any subagent (Agent tool) to do Hornvale work — implementers, close agents, merge reconcilers, reviewers — especially into a worktree or for tasks involving long-running commands (workspace tests, censuses, artifact regeneration).
---

# Dispatching Hornvale Subagents

## Overview

Subagents start in the session's launch directory (the main checkout, on
`main`), not the current worktree, and their background jobs die when their
turn ends. Prose reminders in dispatch prompts have failed in production —
twice even when the prohibition was explicit. The fix is aviation's:
**read-do, not recall** (prepend the checklist file verbatim) and
**challenge-response** (verify echoed evidence on return, not promises).

## Procedure

1. **Prepend the preamble.** Read `dispatch-preamble.md` (next to this
   file) and paste it verbatim at the TOP of the dispatch prompt, replacing
   `<WORKTREE>` and `<BRANCH>` with real values. Never paraphrase it from
   memory — the file is the checklist card.

2. **Dispatch.** The task description goes below the preamble. Keep the
   preamble's four numbered items intact even for "quick" tasks.

3. **Challenge-response on return.** The reply must begin with a verdict
   line (`DONE:` / `BLOCKED:`) and contain the echoed pwd/branch evidence
   plus pasted command output. If any of that is missing, or the reply
   narrates an intention ("I'll hold here and wait…", "monitoring the
   run…"), the agent has parked or drifted: **resume it immediately** with
   un-parking instructions (foreground poll, then continue). The harness
   re-delivers its reply, but the work sits frozen until poked.

4. **Verify the commit landed.** Before reviewing a DONE, run `git log` in
   the worktree and confirm the commit is on the expected branch. A stray
   commit on `main` may need merging back — not reverting — when other
   sessions are active there.

5. **After any kill, sweep.** If a subagent was killed or abandoned
   mid-run, sweep its worktree for orphaned scaffolding (temporary test
   files, partial artifact regens) before running the suite — one orphaned
   census test turned a 90-second suite into 12+ minutes.

## Worktree prewarm and the timeout budget

Fresh worktrees have an empty `target/`, and cargo keys workspace-crate
artifacts to the absolute source path — nothing carries over from the main
checkout. Measured solo on the M1 Max (2026-07-11): cold dev build 16 s,
`make gate` ~4.5 min (dominated by test *runtime*, not compilation),
`make rebaseline` ~2.5 min (cold release build + two 500-seed censuses +
type-audit). The compiles are cheap; **parallel campaign sessions
contending for the same 10 cores are the multiplier** that pushes a gate
past a timeout. So:

- Immediately after `git worktree add`, start `make prewarm` in the
  worktree **in the background of the controller session** (not the
  subagent's) — it warms the dev, release, and type-audit caches while
  spec-reading and preamble assembly happen, trimming the compile edges.
- Gate/census/rebaseline commands must pass an explicit Bash
  `timeout: 3600000` (see preamble item 2); repo settings set default 20
  min / ceiling 60 min. The timeout, not the prewarm, is the real guard
  when several sessions run at once.

## Red flags (controller-side tells)

| Tell in the reply | Reality |
|---|---|
| "I'll hold here / wait for the watcher" | It parked; its children are dead. Resume now. |
| No pwd/branch echo | Step 1 was skipped; assume wrong directory. |
| Verdict buried or absent | Status unknown; ask for `DONE:`/`BLOCKED:` + evidence. |
| "Tests are running in the background" | They died with the turn. Resume with a foreground poll. |

## Why this exists

The Words campaign (2026-07-09): three subagents parked on background jobs
and sat frozen. Crust Task 7 (2026-07-10): an agent parked despite an
explicit foreground-only line — the prohibition alone doesn't hold; the
recovery recipe must ship with it. Stage-2 workflow session (2026-07-10): a
haiku implementer committed to `main` despite the dispatch prompt naming
the worktree path — only a literal `cd` as the first action binds.
