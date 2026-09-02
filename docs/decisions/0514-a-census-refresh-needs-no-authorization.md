# 0514. A census refresh needs no per-run authorization

**Status:** Accepted (2026-09-01) · **Decider:** Nathan · **Supersedes:** the
census half of `campaign-autopilot`'s "Census regen / AWS spend" carve-out ·
**Relates:** [0063](0063-census-regen-is-local-again.md),
[0079](0079-census-goldens-are-authored-on-one-enforced-host.md),
[0133](0133-nontrivial-checks-run-in-one-serial-lane.md),
[0139](0139-main-advances-only-through-the-lock.md),
[0146](0146-the-census-worktree-path-is-resolved-not-passed.md)

In the context of a campaign needing its once-per-campaign census refresh at
pre-merge close, facing a standing carve-out that made every census run a
hard stop for explicit authorization, we decided that **running a census
refresh is ordinary campaign work that needs no per-run permission**,
accepting that a session may consume ~15 minutes of the canonical box's
serial claim without asking first.

## Context

The carve-out was written on 2026-07-14, when a census was genuinely
hazardous to dispatch. Every property that made it so has since been closed
by a different decision, and the carve-out outlived all of them:

| the hazard | what closed it |
|---|---|
| ran on metered AWS spot hardware | 0063 retired AWS; the census runs on the project's own box |
| could run on a non-canonical host and commit values that drift-check green forever | 0079 enforces the host; `census-run.sh` fails closed on the hostname |
| could grab the box out from under a queued merge | 0133 put every expensive job behind one serial claim; `kind=census` (2026-08-24) put the census under the same FIFO |
| wrote its worktree inside the repo, deletable by `git clean -fdx` | 0146 anchored the default to the main worktree and refuses a relative override at the gate |
| could land moved goldens un-gated | `census-run.sh` pushes a `census/<ref>-<stamp>` BRANCH and never `main`; 0139 makes the chamber the only route main advances by |

Cost also stopped being the argument. Read from `docs/timings.md` rather than
from prose: the six most recent runs are 853.214–1090.941 s — tightly
clustered at roughly fifteen minutes, cpu_ratio 32–34 on 40 cores. The
5 h 20 m outlier the guidance warns about (19,207.751 s, The Rill) was a
regression that two later campaigns fixed; it is history, not a budget.

## Decision

A session may dispatch `make sluice-census BRANCH=<branch> REF=<full-sha>`
without asking. It is queued work like a stage gate or a merge.

**What does NOT change, because none of it was the thing being authorized:**

1. **A census still never pushes `main`.** It commits the regenerated goldens
   on the canonical box, pushes a `census/<ref>-<stamp>` branch, and prints
   the `make sluice` line. That restraint is deliberate — census goldens are
   what the calibration batteries assert against, so landing them un-gated
   would move the reference without anything checking the world still agrees
   with it.
2. **Landing moved goldens is still gated.** The census branch goes through
   the merge queue like any other candidate, and a campaign close is still a
   hard stop (`campaign-autopilot` G6).
3. **The host guard stands.** 0079 is unaffected; an off-host run is still
   refused, and "local" in 0063 still means *not AWS*, not *whatever box you
   are sitting at*.
4. **A run that moves nothing pushes no branch and says so.** A null is a
   result.

## Consequences

The carve-out's other half — **AWS spend** — is retired outright rather than
relaxed, because 0063 deleted the thing it governed. `scripts/aws-gate/` and
`make regen-remote` are abandoned; there is no AWS spend for a carve-out to
gate. Leaving the clause in place named a hazard that could not occur, which
is its own failure mode: a policy listing a dead risk trains readers to skim
the list that also contains the live ones.

What remains hard-stopped is unchanged and now reads as three real items
rather than four-with-a-ghost: fidelity cuts and accuracy tradeoffs;
destructive or externally visible actions; and the G3/G6 stops themselves.

**The general shape, stated because this decision is an instance of it.** A
carve-out is a claim that some action carries a risk the ordinary process
cannot absorb. When the process grows a mechanism that absorbs it — a host
guard, a serial claim, a queue, a fail-closed default — the carve-out becomes
friction that no longer buys anything, and nothing in the carve-out's own
text will say so. Five decisions closed this one's hazards one at a time and
none of them thought to go back and check what had been justified by their
absence. Worth a sweep whenever a decision closes a class of risk: grep for
the policies that cite it.
