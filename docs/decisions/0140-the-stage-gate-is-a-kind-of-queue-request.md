# 0140. The stage gate is a kind of queue request, not a dispatch path

**Status:** Accepted (2026-08-16) · **Decider:** Nathan · **Amends:**
[0139](0139-main-advances-only-through-the-lock.md)'s consequence
"`gate-stage` and `gate-commit` are unchanged"; retires the remainder of
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md)'s dispatch layer

In the context of a merge queue that had landed a real merge and now stood
beside the lane it was meant to replace, facing the fact that leaving
`gate-stage` its own dispatch path would keep the entire asynchronous layer
alive to serve one caller, we decided that **a stage gate is a queue entry
with `kind=stage`** — same mouth, same chamber, same claim, same real
main+branch merge, running the `stage`-rung phases and never pushing —
accepting that a stage gate now queues behind whatever else holds the box and
that there is no longer any way to dispatch a single set on demand.

## Context

0139 said `gate-stage` was unchanged, and that was right at the time: it is
cheap relative to the integration set and buys author confidence at plan-stage
boundaries. What it did not weigh is that `gate-stage` was the last caller of
`lane-dispatch.sh`, and therefore the last reason for the shared scratch
worktree, the detached `setsid` job, the `jobs.tsv` that was a job's only
record, and the five `make lane*` targets. Deleting everything the merge queue
replaced would have deleted almost nothing while that one path remained.

The enemy was never scripting; it was **asynchrony in service of an absent
caller**. Every defect the lane produced in its first two days came from that
layer: an unparseable claim, an orphan-on-kill that released the claim while
39 cores were still running, `seam-guard` refusing five of six runs on a tree
an earlier set had dirtied, and a sub-floor roster that never once landed a
byte. A stage gate requested from a Mac is an absent caller by definition.

The absorption also makes the stage gate answer a better question. It used to
gate a **branch tip** — the same mismatch 0139 named for `gate-campaign`, just
earlier in a campaign's life. As a queue entry it gates the actual
`main`+branch merge, which is what a plan-stage boundary is really asking
about.

## Consequences

- **`make sluice-stage BRANCH=<branch> REF=<full-sha>`** replaces `make
  gate-stage REF=<full-sha>`. The argument shape changed, deliberately: a
  stage gate now tests a merge, so it needs to know what is being merged. That
  is why `gate-stage` is a refusing signpost rather than an alias — an alias
  would fail obscurely on the far end instead of clearly at the call.
- **One column, not a second code path.** `kind` (`merge` | `stage`) in the
  queue TSV, and one branch at the push step in `scripts/sluice-run.sh`,
  placed *after* the single failure gate so `kind=stage` can never launder a
  red run into a green one. Coalescing is scoped to the kind as well as the
  branch, or a stage request would silently supersede a queued merge. The
  entry's terminal state is `reported`, kept separate from `landed` because
  reusing `landed` would assert that `main` moved when it did not.
- **The headline refusal does not apply to a stage request.** It exists
  because a merge commit's subject becomes a permanent census epoch label; a
  stage run's merge commit is discarded with the chamber's worktree, so no
  subject it carries can become one. Refusing a mid-campaign `wip` commit that
  a plan-stage boundary legitimately sits on would block the one thing a stage
  gate is for.
- **`make preflight` retires with it**, and its halves are not lost: ancestry
  becomes the mouth's `git merge-tree` on the actual merge (strictly better
  than the proxy it replaces), both-sides-added slugs become an add/add
  conflict there, duplicate idea-registry row IDs redden
  `cli/tests/docs_consistency.rs` in the chamber's `gate` phase against the
  object that lands, and the board's hold-off advisory moves to
  `scripts/sluice-request.sh` — the moment work now asks to integrate. Its
  unmechanizable half ("read the other branches' chronicles") stays human.
- **We give up on-demand single-set dispatch.** `make lane SET=<set>` is gone.
  The two sets the chamber does not run keep their own entry points
  (`make heavy-remote`, `scripts/census-run.sh`); anything else is an operator
  resident on the box running that set's own command from
  `scripts/lane-sets.tsv`. This is the cost of "one session managing one
  machine" and is accepted, not overlooked.
- **"The lane" now names the mutex, not a job system.** The `flock` claim on
  the canonical box survives untouched — decisions 0081/0086/0133 are about
  the box, not about who is watching it — and `scripts/lane-sets.tsv`'s
  `where` column still reads `lane` for exactly that reason.
- **A side effect worth its own sentence: the sub-floor roster can update
  itself for the first time.** `ci-record` refused whenever *any* live claim
  existed, and every serialized path runs it as a descendant of the claim
  holder, so it refused on every run and `docs/timings/subfloor-roster.tsv`
  had one hand-authored commit in its whole history. It now asks
  `contending_holder()` — a claim held by our own ancestor is the job we are
  part of, not contention — and the chamber commits the rewritten roster with
  the merge product.

## See also

`docs/superpowers/specs/2026-08-15-the-sluice-merge-queue-design.md` §3a;
decision [0139](0139-main-advances-only-through-the-lock.md);
decision [0133](0133-nontrivial-checks-run-in-one-serial-lane.md);
decision [0132](0132-three-gates-named-for-the-campaign-moment.md).
