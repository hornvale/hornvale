---
name: closing-a-campaign
description: Use when a Hornvale campaign's implementation is complete and it is being merged and closed — before declaring the campaign done, removing its worktree, or writing its final summary.
---

# Closing a Campaign

## Overview

The Definition of Done lives in prose (CLAUDE.md Process, decisions 0013
and 0020) and is owed at exactly the moment momentum is highest and
diligence lowest. This skill is the surgical timeout: pause at the
boundary and walk the list. Create a todo per numbered step; a campaign is
not done until every one is checked or explicitly N/A.

## The walk (order is load-bearing)

1. **Check the branch still merges.** `make preflight` is retired
   (decision 0139): the merge queue's mouth (`scripts/sluice-mouth.sh`)
   tests the actual merge with `git merge-tree` rather than ancestry as a
   proxy, so a conflict is reported in milliseconds when you submit and the
   queue advances past your request. On a bounced request: merge main INTO
   the branch, re-run `make gate-commit` there, push, and submit again — a
   new request, not a retry, since the sha changed. Repeat every time main
   moves; parallel sessions are the norm here, not the exception. If this
   is the branch's first meeting with main since the campaign began, the
   stage-boundary absorption cadence (CLAUDE.md Process) was missed —
   record that in the retrospective. The half no tool scores is unchanged
   and still yours: read the other branches' chronicles, not just their
   diffs.

2. **Sweep what's still scratch before it dies — and do it BEFORE writing
   the retrospective, not after.** `.superpowers/sdd/` is git-ignored and
   per-worktree, so everything left in it evaporates at step 7's teardown.
   Since The Cartulary, that is a narrower set than it used to be: the
   decision ledger is no longer in this scratch tree at all. It lives at
   `docs/superpowers/ledgers/<slug>.md`, committed as each ruling occurs
   (`campaign-autopilot`'s "The decision ledger" section), so it has
   already survived by the time you reach this step — nothing to sweep,
   because there is nothing left of it here to lose. **What is still
   scratch, and still needs this sweep**, is everything the ledger was
   never the home for: the vendored plugin's own `progress.md` (task
   state, fix rounds, resume-after-compaction material — its own separate
   job, deliberately not mirrored, see spec §4a of The Cartulary), every
   implementer's report, every review package, every mutation proof.

   Walk the whole directory and route every item, rather than summarizing
   from memory of the campaign:

   ```bash
   ls .superpowers/sdd/ .superpowers/sdd/*/
   grep -nE 'deferred|parked|minor|BLOCKED|follow-?up' .superpowers/sdd/*/progress.md
   ```

   - **Process lessons and overturned estimates** → the retrospective.
   - **Speculative directions and unfinished work** → idea-registry rows,
     each carrying the measurement that motivates it rather than a hunch.
     A row with a number is worth several without.
   - **A `progress.md` line that reads like a ruling, deferred minor, or
     parked finding that never made it into the committed ledger** → that
     is a discipline miss, not a routine finding: ledger it now (as a
     backfilled entry — see `campaign-autopilot`'s note on how those
     differ from a live one) and say in the retrospective that it should
     have been ledgered contemporaneously instead of found here.
   - **Ledger entries made after the G3 stop** → Nathan has not seen these
     yet (the ledger being committed does not mean it was reviewed); they
     lead the G6 digest and the material ones get promoted into the spec's
     decisions section or a decision record.

   **The check that catches what this step misses:** for each item still
   found in scratch, name the committed file and line it landed in. "It's
   covered in the chronicle" is not a location. The Ell's close promoted
   nine items and still lost six, including a newly-introduced kernel
   float newtype deriving `PartialOrd` with no `total_cmp` companion —
   found only because a reviewer was asked, explicitly, to list what had
   *not* survived. Ask a reviewer that question; the answer is
   consistently worth more than its findings on the diff. **This check is
   why the ledger became durable in the first place — it kept finding the
   same loss** — but it still applies in full to everything that remains
   scratch-shaped.

3. **DoD artifacts, on the branch, before merging:**
   - **Chronicle entry** — `book/src/chronicle/<slug>.md`, name-only
     designation (decision `0026-slugs-not-numbers`), wired into the
     book's `SUMMARY.md`.
   - **Retrospective** — `docs/retrospectives/<slug>.md` (decision 0020):
     one page, process lessons, not product.
   - **Book freshness sweep** — re-read every chapter describing what the
     campaign changed and fix the lag; the book may never lag merged
     reality (decision 0013). Generated reference pages regenerate via
     `make rebaseline`, not by hand.
   - **Confidence Gradient** — if the campaign resolved or moved a bet in
     `book/src/open-questions.md`, re-score that chapter (decision
     0030).
   - **Registry flips** — idea-registry rows the campaign shipped,
     spec'd, or rejected flip status and repoint **Where**; never delete
     a row.
   - **Plan hygiene** — `IMPLEMENTATION_PLAN.md` deleted once all stages
     are done; the spec/plan files note completion.

4. **Verify golden-pin discipline held.** Pins should have been re-pinned
   in the commits that drifted them, not deferred to now. If the gate is
   red at close, that is a missed re-pin to chase back, not a value to
   quietly accept here.

5. **Refreeze keystone fixtures from main's tip** at merge, so identity
   invariants measure exactly this campaign's delta — stale pre-campaign
   freezes alias other campaigns' physics into yours. These are the frozen
   identity fixtures (e.g. `cli/tests/fixtures/world-seed-42.json`), a
   different thing from step 4's golden pins: pins re-pin in the drifting
   commit, keystones refreeze at merge.

6. **Submit to the merge queue — and note that this is step SIX, after
   step 3's artifacts, not before them.** Submitting first is the common
   failure and it is not free: the campaign then owes a second merge to
   carry the chronicle and retrospective, which is a second slot in a
   strictly serial queue plus a window where `main` holds a campaign the
   book does not describe. The Penstock (2026-08-23) did exactly this.
   `submitting-to-the-sluice` now carries the same precondition as its own
   step 0, so either skill catches it. (`make sluice BRANCH=<branch>
   REF=<full-sha>`; see the `submitting-to-the-sluice` skill for the
   load-bearing order of gate-commit → push → enqueue → nudge). Do not
   fast-forward main by hand: the chamber gates the merge *product* — the
   full suite, the artifact regeneration and its drift check, the outboard
   and client suites, seam-guard and the heavy tier — and pushes exactly
   the sha it tested. A branch tip that gated green is not evidence about
   the object that lands (decision 0139).

7. **Memory.** Record what the close learned that the repo does not
   (process lessons, overturned estimates); prune memory entries the
   campaign made stale.

## Quick reference — what a merged campaign owes

| Artifact | Where | Authority |
|---|---|---|
| Chronicle entry | `book/src/chronicle/<slug>.md` + SUMMARY | decision 0013 |
| Retrospective | `docs/retrospectives/<slug>.md` | decision 0020 |
| Freshness sweep | every chapter the campaign staled | decision 0013 |
| Gradient re-score | `book/src/open-questions.md` (if a bet moved) | decision 0030 |
| Registry flips | `book/src/frontier/idea-registry.md` | registry header rules |
| Keystone refreeze | `cli/tests/fixtures/` etc., from main's tip | merge-time discipline |
| Scratch promotion | `.superpowers/sdd/` → retrospective + registry rows | step 2; the scratch is git-ignored and dies with the worktree |

## Common mistakes

- Declaring done from the final summary without the walk — the summary is
  step 8, not a substitute for steps 1–7.
- Re-scoring nothing because "no bet obviously moved": grep the campaign's
  domains in `open-questions.md` before concluding that.
- Hand-editing generated book pages during the sweep instead of
  regenerating them.
- Writing the retrospective from memory of the campaign instead of from
  the scratch, then tearing down the worktree — the material is gone and
  nobody can tell what was lost, because the only record of it was in the
  directory just deleted.
- Treating a red golden pin at close as noise to re-pin silently — it
  means a commit shipped without its re-pin; note it in the retrospective.

When a mechanical DoD gate ships (registry row PROC-7), the artifact
existence checks move to CI; this walk keeps the judgment half — whether
the sweep actually happened and the chronicle is worth reading.
