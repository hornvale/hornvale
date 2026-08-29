---
name: submitting-to-the-sluice
description: Use when a Hornvale campaign needs the canonical box — either at a plan-stage boundary (a stage gate) or when work is complete and ready to merge — asking the sluice (the serial queue on the canonical box) to take it, rather than gating or merging by hand.
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

## Two kinds of request, one queue

Since Task 12 (decision 0140) the queue takes **both** things a campaign
needs the canonical box for, and they differ in exactly one respect:

| | `make sluice-stage BRANCH=… REF=…` | `make sluice BRANCH=… REF=…` |
|---|---|---|
| when | every plan-stage boundary | work is complete |
| phases | `artifacts outboard gate clients` | **the same four, PLUS `heavy`** — see below |
| merges main+branch in the chamber | yes | yes |
| pushes | **never** | yes, the exact SHA it tested |
| terminal state | `reported` | `landed` |
| headline refused if junk | no — nothing permanent carries it | yes |

Everything below applies to both unless it says otherwise.

**THE MERGE RUNS ONE PHASE MORE THAN THE STAGE GATE: `heavy`, last (decision
0426, 2026-08-28).** This paragraph has now been wrong in two different
directions, so read the script rather than any prose about it:

```
merge_phases="artifacts outboard gate clients heavy"
stage_phases="artifacts outboard gate clients"
```

The history, because it is the reason to distrust a remembered phase list: the
table once said a merge runs "all six, `heavy` last"; decision 0148 made that
false by shrinking the merge list down to the stage list; this paragraph then
said flatly that "`heavy` is not a chamber phase at all", which decision 0426
made false again after The Governor cut the tier 3.52x. `seam-guard` is the one
that is genuinely not a chamber phase — it runs only from `make seam-guard`.

**`heavy` is a merge phase and NOT a stage-gate phase, and that asymmetry is
deliberate.** It compares a live probe against the committed census fixtures,
which are refreshed once per campaign at pre-merge close — so on a stage gate
it would red predictably for the whole middle of any world-touching campaign.
It has in fact never been a stage phase; 0426 restores the pre-0148 layout.

**Consequence when choosing between the two: a green stage gate no longer buys
a merge's ENTIRE phase coverage.** It buys four fifths of it. A merge can still
red on `heavy` after every stage gate passed. If you want that answer earlier,
`make heavy-remote REF=<full-sha>` is the by-hand dispatch — it is a
diagnostic you read, not a gate, so a census-fixture red in it is expected
mid-campaign and is not a reason to stop.

**A prose-only candidate skips `heavy` (and `clients`).** `scripts/sluice-phases.sh`
drops them when every changed path is hand-written prose, so a docs-only merge
does not pay the tier's ~465.8 s (decision 0426 derives that figure next to its
inputs; nothing else restates it).

**Why this matters when choosing between them.** From 0148 until 0426 the
difference was the push and nothing else, and this paragraph said so. Since
0426 the difference is the push **plus `heavy`**: a green stage gate on an
ancestor SHA buys four of a merge's five phases, not all of them. Do not
reason that going straight to merge "adds the full suite" — it adds the push
and the heavy tier, which is a real but bounded increment. The stage gate's
real value is unchanged: its refusal is free, and it leaves `main` untouched by
construction rather than by a phase passing. (The original warning here was
learned by The Forebay reasoning from a stale row and telling its decider the
wrong thing — which is why this paragraph now names what changed instead of
being quietly rewritten.)
 A stage gate is
not a lesser instrument: it gates the same real merge product, which is what
makes it worth queueing behind an hour of someone else's heavy run. What it
replaced (`make gate-stage`) tested a bare branch tip and could go green on
a branch that would not survive contact with `main`.

`make gate-stage` and `make preflight` are refusing signposts now; so are the
five `make lane*` targets. If you find yourself reaching for one, the answer
is `make sluice-stage`.

## Step 0, for a MERGE only: the campaign must already be closed

**A merge submission is the LAST act of a campaign, not the first act of
finishing one.** `closing-a-campaign` puts the Definition-of-Done artifacts
**on the branch (its step 3)** and only then submits (its step 6), and that
order is load-bearing: the chronicle entry, the retrospective, the book
freshness sweep, the Confidence-Gradient re-score and the registry flips all
belong to the merge product being gated. Submit before writing them and the
campaign owes a SECOND merge to carry them — a second queue slot, a second
~1000 s chamber run, and a window in which `main` holds a campaign the book
does not describe.

This has happened. The Penstock (2026-08-23) submitted its merge with the
code complete and reviewed, landed clean at `132854255`, and only then
discovered it owed a chronicle and a retrospective. Nothing was lost, but
the close cost a whole second trip through a strictly serial queue that
every other campaign was also waiting in.

So before `make sluice` — not before `make sluice-stage`, which is exempt
because it pushes nothing and a campaign mid-flight legitimately has no
chronicle yet — check:

- [ ] chronicle entry written and wired into `book/src/SUMMARY.md`
- [ ] retrospective in `docs/retrospectives/`
- [ ] book freshness sweep done; Gradient re-scored or explicitly N/A
- [ ] registry rows flipped and repointed
- [ ] scratch swept (`.superpowers/sdd/` dies with the worktree)

If any box is unticked, you are not ready to merge — you are ready to run
`closing-a-campaign`, which will bring you back here.

**A stage gate has no such precondition.** That asymmetry is the whole
reason this section says "for a MERGE only": a stage request is exactly the
instrument for a campaign that is *not* finished.

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
   make sluice       BRANCH=<branch> REF="$ref"      # to merge
   make sluice-stage BRANCH=<branch> REF="$ref"      # to gate only
   ```

   `make sluice` (`scripts/sluice-request.sh`) validates the ref is a full,
   hex-pure 40-char SHA (rejecting a string that merely *starts* with a hex
   digit — `[0-9a-f]*` is a glob, not a length-anchored check, and this
   script had that exact bug once) and is actually on a remote branch, then
   — for a **merge** request only — **refuses the submission unless the
   range carries an authored `Sluice-Headline:` trailer** before it ever
   ssh's anywhere. The subject of HEAD, or of any other commit in the
   range, does **not** satisfy this — a real headline used to be inferred
   from the tip commit's subject, and that inference failed on four of the
   first four real merges (doubled once, branch-path-leaked twice,
   redundantly prefixed once — the full account is in
   `scripts/sluice-headline.sh`), so the rule is now a trailer, not
   position:

   - Write **only the text that landed**, one line, as a trailer:

     ```
     Sluice-Headline: every instant in the walk is a WorldTime (stage 3)
     ```

     Do not prefix it with `merge(...): ` yourself — `sluice-run.sh`
     composes `merge(<campaign>): <your text>` as the actual merge
     commit's subject (stripping a `merge(...): ` prefix defensively if
     you added one anyway, so it can never double).
   - It can sit on **any commit in the range** (`origin/main..REF`), not
     necessarily the last one, and a later commit does not displace it —
     `git log` reads newest-first and the newest non-empty trailer wins.
   - It **must be in that commit message's last paragraph**. This reads
     git's own trailer parser, which only ever looks at the final block:

     ```
     Sluice-Headline: what landed      <- STRANDED, ignored
                                        <- this blank line breaks it
     Claude-Session: https://…
     ```

     Keep it adjacent to any other trailers, no blank line between.
   - Verify before pushing:

     ```bash
     git log -1 --format='%(trailers:key=Sluice-Headline,valueonly)' <sha>
     # or, across the whole range (newest non-empty wins):
     git log --format='%(trailers:key=Sluice-Headline,valueonly)' origin/main..HEAD
     ```

   This is mechanical, not advisory: `tools/census/history.sh` reads the
   merge commit's subject as the permanent census epoch label (`git log
   --follow --first-parent main`) whenever that merge moves the census, so
   only the campaign that authored the range knows whether a label is fit
   to be permanent; an operator triaging the queue later must never be the
   one inventing or silently accepting a placeholder. A `stage` request is
   exempt — its merge commit is discarded with the chamber's worktree, so
   no label it carries can ever land, and refusing a mid-campaign commit
   with no trailer at a plan-stage boundary would block the one thing a
   stage gate is for. If your merge submission is refused this way, add
   the trailer to any commit in the range (a small final commit is fine)
   and push again — do not try to route around it with an env var;
   nothing in the request path threads a caller-supplied override through,
   deliberately (`HV_SLUICE_HEADLINE` is a test seam for driving
   `sluice-run.sh` directly, not a caller-facing override of the mouth's
   refusal).

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
   treatment: a real "wip"-subject commit with **no `Sluice-Headline:`
   trailer** (minted locally with `git commit-tree`, never touching a real
   branch) is refused by the real script and accepted by a mutant with the
   check deleted; a 40-char, hex-*starting* but not hex-*pure* string is
   refused by the real script and would have passed the original,
   glob-based check.

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
make sluice-status                  # queued / running / held / landed / reported, FIFO order
make sluice-log [JOB=<id>]          # a finished chamber job's own log
```

A stage request ends `reported`, never `landed` — `landed` would claim `main`
moved, and a stage gate never pushes. Read its log for the verdict; the last
line of a green one is `STAGE REPORT`.

A `held` row needs a human: a merge conflict (absorb `main` and resubmit —
a new request, not a retry of the same one, since the sha changed) or a
chamber-side failure (read the log named above).

## Common mistakes

- Nudging before the enqueue is confirmed — the exact failure the
  durable-before-nudge ordering exists to prevent.
- Assuming `make gate-commit` passing says anything about the headline —
  it is a different property (compiles/lints/sub-floor tests vs. "this
  range carries an authored `Sluice-Headline:` trailer fit to become a
  permanent census epoch label") and gate-commit does not look at commit
  messages at all. `sluice-request.sh` enforces the headline itself now,
  so a missing or misplaced trailer is refused at submission time either
  way — but a campaign that expects `gate-commit` to have already covered
  it will be confused by the refusal.
- Writing a fine commit *subject* and assuming that satisfies the check.
  It does not — only a `Sluice-Headline:` trailer does, and only when it
  sits in that commit message's last block.
- Force-pushing to update a submission. Push an ordinary fast-forward
  commit and submit a new request (the queue coalesces a same-branch,
  ancestor-superseding resubmission automatically — see
  `scripts/sluice-queue.sh`'s header on ancestry-based coalescing); never
  rewrite history to "fix" a request already in flight.
- Assuming `REF` can be a branch name. It must be a full 40-char SHA —
  `sluice-request.sh` refuses anything else, because a branch name can
  drift under a `reset --hard` on the far end.
