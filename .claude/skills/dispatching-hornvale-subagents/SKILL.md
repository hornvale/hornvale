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

## Model floor: NO haiku, ever

Haiku-tier subagents are **banned for all Hornvale work** (Nathan,
2026-07-14). The precipitating incident (The Speakable): a haiku fix agent
found itself in the wrong tree, silently **re-implemented the feature on
`main`** to make its test compile, committed there (b9d5ebb, reverted as
fbaaf44), and returned a DONE reply whose pwd/branch evidence did not
match where its commit landed — the preamble and challenge-response both
held and it still happened. This is the second haiku wrong-tree commit
(Stage-2 workflow session, 2026-07-10). Minimum tier for ANY dispatch —
including "trivial" fixes, which is exactly where haiku was being reached
for — is sonnet. Trivial exact-content changes are cheaper done inline by
the controller than dispatched at all.

## Procedure

1. **Verify the brief against the code.** See the next section. This is a
   named, non-optional step of the task loop, not a judgement call about
   whether this particular task looks risky.

2. **Prepend the preamble.** Read `dispatch-preamble.md` (next to this
   file) and paste it verbatim at the TOP of the dispatch prompt, replacing
   `<WORKTREE>` and `<BRANCH>` with real values. Never paraphrase it from
   memory — the file is the checklist card.

3. **Dispatch.** The task description goes below the preamble. Keep the
   preamble's four numbered items intact even for "quick" tasks. Fold step
   1's findings in as the controller's resolution of ambiguity — SDD's
   dispatch item (4) — so the implementer gets the corrected claim rather
   than rediscovering the defect.

4. **Challenge-response on return.** The reply must begin with a verdict
   line (`DONE:` / `BLOCKED:`) and contain the echoed pwd/branch evidence
   plus pasted command output. If any of that is missing, or the reply
   narrates an intention ("I'll hold here and wait…", "monitoring the
   run…"), the agent has parked or drifted: **resume it immediately** with
   un-parking instructions (foreground poll, then continue). The harness
   re-delivers its reply, but the work sits frozen until poked.

5. **Verify the commit landed.** Before reviewing a DONE, run `git log` in
   the worktree and confirm the commit is on the expected branch. A stray
   commit on `main` may need merging back — not reverting — when other
   sessions are active there.

6. **After any kill, sweep.** If a subagent was killed or abandoned
   mid-run, sweep its worktree for orphaned scaffolding (temporary test
   files, partial artifact regens) before running the suite — one orphaned
   census test turned a 90-second suite into 12+ minutes. **Before killing a
   subagent that stalled mid-edit, preserve its partial diff (`git diff` to
   a scratch file, or `git stash`) rather than discarding it, and never hand
   a half-applied edit to a successor agent** — The Beacon killed a stalled
   fix agent and started the next one fresh from the last clean commit,
   which was safe but threw away a correct shape the agent had already
   found, costing the next round the same reasoning over again.

## Step 1 in full: verify the brief against the code

**Do this once per task, immediately before dispatching that task — never
in a batch at plan-authoring time.** That timing is the whole reason it
works. Verifying five tasks' claims while writing the plan means reasoning
about code you have not opened, for tasks whose predecessors have not run
yet. Verifying *one* task's claims, one task ahead, with the tree in the
state the implementer will actually find it, is a different and much
easier job. Plan-time verification keeps failing on scope, not on
diligence — so this step buys its accuracy by being late and narrow.

Budget about three minutes of grep. Measured on the task that produced
this section: three minutes found two defects, one of them silent.

Work the brief's claims, not its prose:

1. **Every named identifier gets grepped.** Types, variants, functions,
   helpers, constants, file paths. A plan is the one code nothing
   compiles, and a plausible-but-absent idiom rides straight into the
   implementation. Real instance: a test sketch specified `BuildDepth::Full`
   where the neighbouring `person_promotion.rs` actually uses a plain
   `build_world` helper. Both look right; only one exists.
2. **Every "the N sites are …" claim gets re-derived from the observable.**
   Grep the output string, the error text, the predicate name — not the
   function you happened to open. A brief that enumerates call sites is
   asserting completeness, and completeness is what enumeration gets wrong.
3. **Name the readers, then classify them.** If the task changes a value,
   list every consumer and sort them into *prose-only* and
   *contract-bearing* (seed derivation, save-format labels, stream
   consumption order, anything quantized at emit). Real instance: a plan
   was silent on two whole classes of reader, one of which fed a
   seed-derivation leg-string — a miss there changes every settlement's
   mind and **nothing goes red**. Prose defects are visible; contract
   defects are not, so the classification is the point of the step.
4. **Read every imperative for the outcome hiding inside it.** "Run X and
   confirm the diff is empty" asserts the diff will be empty. See
   `campaign-autopilot`'s "Imperative mood hides assertions" — the same
   failure, caught here one task later instead of five tasks earlier.

**Outputs.** Fold corrections into the dispatch as the controller's
resolution of ambiguity. If a finding contradicts the plan rather than
merely sharpening it, ledger it and fix the plan text — the next task
reads that file too.

**If this step balloons past ~10 minutes, stop.** A brief that cannot be
checked against the code in ten minutes is a plan defect, not a
verification workload; escalate rather than absorbing it silently.

## Review packages: use the repo's capped script

Use `scripts/review-package.sh PLAN_FILE BASE HEAD`, **not** the SDD
plugin's `scripts/review-package`. Identical arguments and output layout;
it additionally caps any single file's diff at 50 KB, emitting that file's
metadata plus a bounded sample and writing the complete text to a sibling
`.full.diff` the reviewer opens only if it has reason to.

This is not a nicety. One task's raw package measured **2.77 MB, of which
99% was `cli/tests/fixtures/world-seed-42.json`** — 21,720 lines of epoch
churn that told the reviewer nothing. Committed artifacts churn wholesale
in this repo, so the uncapped script hands reviewers six figures of tokens
of noise on a routine task. `BASE` is the commit recorded before
dispatching the implementer — never `HEAD~1`, which silently drops all but
the last commit of a multi-commit task.

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
| No pwd/branch echo | Step 2 was skipped; assume wrong directory. |
| The brief names a type you did not grep | Step 1 was skipped. A plan's code is the one code nothing compiles. |
| A review package over ~200 KB | The uncapped plugin script was used; regenerate with `scripts/review-package.sh`. |
| Verdict buried or absent | Status unknown; ask for `DONE:`/`BLOCKED:` + evidence. |
| "Tests are running in the background" | They died with the turn. Resume with a foreground poll. |

## Why this exists

The Words campaign (2026-07-09): three subagents parked on background jobs
and sat frozen. Crust Task 7 (2026-07-10): an agent parked despite an
explicit foreground-only line — the prohibition alone doesn't hold; the
recovery recipe must ship with it. Stage-2 workflow session (2026-07-10): a
haiku implementer committed to `main` despite the dispatch prompt naming
the worktree path — only a literal `cd` as the first action binds.
