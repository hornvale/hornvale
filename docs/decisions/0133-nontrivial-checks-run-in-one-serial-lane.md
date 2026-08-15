# 0133. Nontrivial checks run in one serial lane

**Status:** Accepted (2026-08-14) · **Decider:** Nathan · **Refines:**
[0086](0086-the-heavy-tier-runs-on-the-canonical-box.md),
[0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)

In the context of decision 0132 splitting one gate into three, where only
the seconds-scale commit gate stays cheap enough to run on a developer's own
machine, we decided that **the two gates costing minutes or more — the stage
gate and the campaign gate — dispatch to one strictly serial lane on the
canonical box**, taking the same shared claim a heavy run or a census already
takes, with a hard host lock and no override.

## Context

Decision 0086 placed campaign worktrees and the (then-single) commit gate on
the Mac, and the heavy tier and censuses on the canonical box, because the
canonical box's other jobs are long and a latency-sensitive gate does not
belong queued behind them. Decision 0081 gave the canonical box one shared
claim for heavy writers, taken at the write seam, and in the same breath
carved gates out of it: a gate **advises rather than blocks** against that
claim, because "a developer waiting twelve minutes to start a four-minute
gate is a worse experience than the contention, and a gate is not a
measurement."

Once 0132 exists, that carve-out no longer has a subject. `gate-commit` is
the only gate left that runs on a developer's own machine, and it is
seconds-scale — the collision the advisory carve-out was written to avoid
does not arise for it. `gate-stage` and `gate-campaign` are minutes-to-tens-
of-minutes jobs that read and, in several suites, **write** committed
artifacts — exactly the shape 0086 already requires to run on the canonical
box.

Before specifying a queue, the ordering guarantee it would need was checked
rather than assumed: two trials on the canonical box — six spaced waiters,
then eight simultaneous waiters against a held lock — were granted the lock
strictly in arrival order, 8/8 both times. The existing lock primitive
already delivers a strict serial queue; no separate ticket-spool runner was
required.

## The ruling

**One lane, one shared claim, first-come-first-served, no exceptions and no
priority tiers.** `gate-stage` and `gate-campaign` dispatch to it exactly as
a heavy run or a census does, taking the same claim rather than a separate
one — the binding constraint is the machine, and there is one canonical box.

**This amends 0086's placement table.** The table read: campaign worktrees
and the commit gate on the Mac; the heavy tier and censuses on the canonical
box. It now reads: campaign worktrees and `gate-commit` on the Mac;
`gate-stage`, `gate-campaign`, the heavy tier, and censuses all on the
canonical box, behind the one lane. `gate-commit` gets no host guard, for the
same reason 0086 gave it none: it writes nothing host-sensitive.

**This reverses 0081's advisory carve-out for the gates.** The carve-out was
calibrated against a gate that ran locally in minutes and only occasionally
collided with a census on the same box. That gate no longer exists after
0132; the gates that replaced it for anything costing minutes or more are
lane jobs by construction, and a lane job takes the claim like any other
heavy writer. 0081's general blocking behaviour for heavy writers is
untouched — only the exception carved out for gates is removed, because
nothing left qualifies for it.

**The host lock is hard, with no force override.** If the canonical box is
unreachable, there is no stage or campaign gate anywhere — the guard fails
closed rather than falling back to an uncontrolled local run. There is
deliberately no override flag: an override that exists gets used under
deadline, and the entire point of one lane is one comparable result set. The
recovery path is to fix the canonical box, or to change the roster in a
reviewable commit — the same posture the canonical-host guard already takes
for censuses.

## The two costs, accepted rather than engineered around

**A stage or campaign gate can queue behind an hour or more of unrelated
work.** With one lane and no priority tiers, a several-minute stage gate may
land behind a long heavy run or a census. This is the arithmetic 0081
declined to accept for a *local* four-minute gate; it is accepted here
because dispatch to the lane is asynchronous — a caller gets a job id back
immediately, not a blocked terminal — and because 0132 already removed the
frequent, latency-sensitive caller (every commit) from the lane entirely.
Only the two gates that already ran in minutes, at plan-stage and campaign
boundaries, are affected.

**Canonical-box unavailability is a hard stop, not a degraded mode.** There
is no local fallback for `gate-stage` or `gate-campaign`. This is accepted
because a fallback would reintroduce exactly the cross-host divergence 0079
and 0063 already ruled out for goldens, and because the recovery is cheap
(fix the box, or edit one file in a commit) compared to the risk of a second,
uncontrolled code path silently becoming the one people actually use.

## Consequences

- The commit gate is unaffected: it stays local, host-unguarded, and
  seconds-scale.
- `gate-stage` and `gate-campaign` inherit every property the lane already
  gives heavy runs and censuses: a bounded wait, evidence written per job
  (log plus an outcome row on every exit path, including a signal), and
  queue introspection (who holds the claim, who is waiting, reading a
  finished job back) without needing to block on it.
- The one committed test-duration baseline this campaign was already
  chasing — previously forked across every host a gate happened to run
  on — now has exactly one host to fork from, because every gate above the
  commit tier runs in the same place every time.
- 0079 and 0063 are untouched; this record strengthens their premise (one
  canonical box for anything that writes or measures at scale) rather than
  revisiting it.

## See also

`docs/superpowers/specs/2026-08-14-the-staff-design.md` §2, §2.1, §6, §8;
decision [0079](0079-census-goldens-are-authored-on-one-enforced-host.md);
decision [0063](0063-census-regen-is-local-again.md).
