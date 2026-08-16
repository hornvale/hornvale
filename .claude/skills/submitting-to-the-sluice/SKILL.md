---
name: submitting-to-the-sluice
description: Use when a Hornvale campaign's work is complete on its branch and ready to merge — asking the sluice (the serial merge queue on the canonical box) to take it, rather than merging by hand.
---

# Submitting to the Sluice

## Overview

The sluice is a durable, strictly serial merge queue (decision 0133's
lane, absorbed into a queue — see `scripts/sluice-queue.sh`,
`scripts/sluice-mouth.sh`, `scripts/sluice-run.sh`). Submitting to it is
not "run `make sluice`" — that is one step of three, and the order of
those three steps is load-bearing, not a convenience ordering (G3-amendment
#8, `.superpowers/sdd/2026-08-15-the-sluice/progress.md`).

**Durable before doorbell.** `make gate-commit` → push → enqueue (`make
sluice`) → THEN, and only then, nudge whoever operates the queue. The
wire (Claude Code's cross-session messaging) stores nothing — if the nudge
*is* the request, an operator that is dead, mid-compaction, or an hour
into another merge loses it silently and the campaign has no way to tell.
The queue entry is the actual request; the nudge is a latency
optimisation on top of it, never a substitute for it. Reordering these —
messaging first "in case the enqueue is slow" — reintroduces exactly the
failure this ordering exists to prevent: a nudge with nothing durable
behind it.

## The three steps, in order

1. **`make gate-commit`.** Must pass. This is the only local, per-commit
   gate; the sluice's own chamber re-runs the full workspace suite plus
   artifacts/outboard/clients/heavy phases as `sluice-run.sh`'s roster
   (`scripts/lane-sets.tsv`'s `integration` row), so `gate-commit` here is
   about catching the cheap, obvious break before spending a queue slot on
   it — not a substitute for the chamber's own gate.

2. **Push the branch, then enqueue.**

   ```bash
   git push origin HEAD:refs/heads/<branch>          # ordinary push, NEVER force
   ref="$(git rev-parse HEAD)"                        # full 40-char SHA
   make sluice BRANCH=<branch> REF="$ref"
   ```

   `make sluice` (`scripts/sluice-request.sh`) validates the ref is a full,
   hex-pure 40-char SHA (rejecting a string that merely *starts* with a hex
   digit — `[0-9a-f]*` is a glob, not a length-anchored check, and this
   script had that exact bug once) and is actually on a remote branch, then
   **refuses the submission if the commit's own subject is not a real
   headline** before it ever ssh's anywhere:

   ```bash
   headline="$(git log -1 --format=%s "$ref")"
   case "$headline" in
       ""|wip|WIP|fixup!*|squash!*|.|tmp|temp|TODO) refuse ;;
   esac
   ```

   This is mechanical, not advisory — `sluice-request.sh` runs this itself
   on every submission, using the exact same subject
   `sluice-run.sh`'s own `--no-ff` merge will later use as the merge
   commit's subject (`headline="${HV_SLUICE_HEADLINE:-$(git log -1
   --format=%s "$sha")}"`), which `tools/census/history.sh` then reads as
   the permanent census epoch label (`git log --follow --first-parent
   main`). Only the campaign that authored the commit knows whether that
   subject is fit to become a permanent artifact label; an operator
   triaging the queue later must never be the one inventing or silently
   accepting a placeholder one. If your submission is refused this way,
   amend HEAD's message (`git commit --amend`, if that commit is yours to
   rewrite, or add a small final commit with a real subject) and push
   again — do not try to route around it with an env var; nothing in the
   request path threads a caller-supplied override through, deliberately.

   Once past both checks, `sluice-request.sh` ssh's to the canonical box
   and calls `sluice-queue.sh add` there under its own lock. **If this
   step fails, stop. Do not proceed to step 3.** A failed enqueue prints
   `sluice-request: remote enqueue FAILED — nothing was queued.` on
   stderr and — this is the property that makes the ordering rule
   mechanical rather than aspirational — never prints the
   `read it back with 'make sluice-status'` line that a step-3 nudge
   should be conditioned on. `scripts/test-sluice.sh`'s "request:
   durable-before-nudge" section pins this by running the real script
   against a mocked failing remote and asserting that line is absent, then
   demonstrates the assertion is non-vacuous by mutating the script to
   swallow ssh's exit code and confirming the same test goes red on the
   mutant. The headline refusal and the hex-purity check get the same
   treatment: a real "wip"-subject commit (minted locally with `git
   commit-tree`, never touching a real branch) is refused by the real
   script and accepted by a mutant with the check deleted; a 40-char,
   hex-*starting* but not hex-*pure* string is refused by the real script
   and would have passed the original, glob-based check.

3. **Only once step 2 printed a request id: nudge.** The queue entry is
   already durable at this point — nudging is purely about latency,
   letting an operator who happens to be free act on the request sooner
   than their next poll of `make sluice-status`. Two channels, not
   mutually exclusive:
   - **The wire** (Claude Code's own cross-session messaging), if you know
     of a live operator session — it delivers straight into that
     session's turn, which is the fastest path when it applies.
   - **The board** (`make board-post KIND=notice NOTE='sluice: queued
     req-... for <branch> at <short-sha>' PATHS='scripts/sluice-queue.sh'`)
     — durable, cross-host, and readable by an operator session that does
     not exist yet or is between turns. Post here even after a successful
     wire nudge: the wire stores nothing, so the board post is what
     survives if the live session compacts or ends before acting.

   If you don't know of a live operator, the board post alone is
   sufficient — the queued request is the real artifact; nobody is
   required to be nudged for the sluice to eventually process it.

## Ordering, and why there is no cleverer one

**FIFO, no priority tiers** — the same rule decision 0133 gave the lane,
unchanged for the queue (G3-amendment #8). There is no scheduling
optimisation available: every merge invalidates every other queued
candidate's merge product against the new `main`, so total re-merge cost
is order-independent regardless of how the queue is ordered. Do not ask
for or expect priority.

**One asymmetry exists, and it is not a priority tier — it is a cost
difference already built into the mouth/chamber split.**
`scripts/sluice-mouth.sh` bounces a genuine merge **conflict** in
milliseconds, before the candidate ever takes the chamber's shared claim,
and the queue advances past it immediately — go absorb `main` and
resubmit. A **red inside the chamber** (a failed phase, a dirty tree after
all phases) holds instead, because by then the candidate has already
consumed the one serial resource every other queued request is waiting
behind. Neither of these is something the submitting skill causes or
controls; they are named here so a campaign reading `make sluice-status`
understands why one held request can sit differently than another.

## Checking on a submitted request

```bash
make sluice-status                  # queued / running / held / landed, FIFO order
make sluice-log [JOB=<id>]          # a finished chamber job's own log
```

A `held` row needs a human: a merge conflict (absorb `main` and resubmit —
a new request, not a retry of the same one, since the sha changed) or a
chamber-side failure (read the log named above).

## Common mistakes

- Nudging before the enqueue is confirmed — the exact failure the
  durable-before-nudge ordering exists to prevent.
- Assuming `make gate-commit` passing says anything about the headline —
  it is a different property (compiles/lints/sub-floor tests vs. "this
  subject is fit to become a permanent census epoch label") and
  gate-commit does not look at commit messages at all. `sluice-request.sh`
  enforces the headline itself now, so a bad subject is refused at
  submission time either way — but a campaign that expects `gate-commit`
  to have already covered it will be confused by the refusal.
- Force-pushing to update a submission. Push an ordinary fast-forward
  commit and submit a new request (the queue coalesces a same-branch,
  ancestor-superseding resubmission automatically — see
  `scripts/sluice-queue.sh`'s header on ancestry-based coalescing); never
  rewrite history to "fix" a request already in flight.
- Assuming `REF` can be a branch name. It must be a full 40-char SHA —
  `sluice-request.sh` refuses anything else, because a branch name can
  drift under a `reset --hard` on the far end.
