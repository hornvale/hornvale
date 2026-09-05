---
name: closing-a-campaign
description: Use when a Hornvale campaign's implementation is complete and it is being merged and closed — before declaring the campaign done, releasing its worktree, or writing its final summary.
---

# Closing a Campaign

## Overview

The Definition of Done lives in prose (AGENTS.md Process, decisions 0013
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
   stage-boundary absorption cadence (AGENTS.md Process) was missed —
   record that in the retrospective. The half no tool scores is unchanged
   and still yours: read the other branches' chronicles, not just their
   diffs.

2. **Route what's still scratch before it dies, AND separately read what's
   now durable — two actions, not one sweep, and do both BEFORE writing the
   retrospective, not after.** Since The Cartulary, step 2 covers two halves
   that no longer share a discovery mechanism, and treating them as one
   sweep is exactly the failure mode this campaign exists to remove: an
   instruction (route post-G3 ledger entries) whose original discovery
   command (`ls`/`grep` over `.superpowers/sdd/`) quietly stopped reaching
   its subject the day the ledger moved out of that tree. A closer who runs
   only that command sees no ledger file and concludes there is nothing to
   route — which is false, not merely incomplete.

   **A. What dies when the worktree is next recycled — still needs sweeping
   out of `.superpowers/sdd/`.** `.superpowers/sdd/` is git-ignored and
   per-worktree, so everything left in it evaporates. This is narrower than
   it used to be: the decision ledger is gone from this tree entirely (see
   B). What remains here, and only here, is the vendored plugin's own
   `progress.md` (task state, fix rounds, resume-after-compaction material
   — its own separate job, deliberately not mirrored, see spec §4a of The
   Cartulary), every implementer's report, every review package, every
   mutation proof.

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

   **B. What survives recycling — still needs READING, not rescuing.** The
   decision ledger, `docs/superpowers/ledgers/<slug>.md`, is committed as
   each ruling occurs (`campaign-autopilot`'s "The decision ledger"
   section), so nothing here is at risk of being lost. But committed is not
   the same as reviewed, and nobody has read it end to end just because git
   has a copy:

   ```bash
   cat docs/superpowers/ledgers/<slug>.md
   ```

   - **Every entry made after the G3 stop** → Nathan has not seen these
     yet; they lead the G6 digest, and the material ones get promoted into
     the spec's decisions section or a decision record.
   - **Every deferred minor recorded anywhere in the ledger, at any task
     boundary, not only those made after the G3 stop** → a home, even if
     that home is one line in the retrospective's deferred-minors section
     stating its outcome (fixed in a later task, accepted as-is, or carried
     forward as a registry row). This is the bullet Task 4's rewrite of
     this step deleted with no replacement (The Cartulary final review,
     finding I1): half A above only catches a minor that never reached the
     ledger at all, and the bullet just above only catches entries made
     after G3 — a minor ledgered *before* G3, which is the ordinary case
     for a routine task-boundary finding, was a home for neither. Read the
     whole ledger for "Deferred minor" headings, not only its tail, and
     name where each one landed. A minor sitting only in the ledger is not
     routed — "it's in the ledger" is not a location, any more than "it's
     covered in the chronicle" was.

   **The check that catches what this step misses:** for each item from
   either half, name the committed file and line it landed in. "It's
   covered in the chronicle" is not a location. The Ell's close promoted
   nine items and still lost six, including a newly-introduced kernel
   float newtype deriving `PartialOrd` with no `total_cmp` companion —
   found only because a reviewer was asked, explicitly, to list what had
   *not* survived. Ask a reviewer that question; the answer is
   consistently worth more than its findings on the diff. **This check is
   why the ledger became durable in the first place — it kept finding the
   same loss** — but it still applies in full to everything that remains
   scratch-shaped (half A), and to whether half B's post-G3 entries
   actually got read here rather than assumed reviewed because they were
   committed.

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
   fast-forward main by hand: the chamber gates the merge *product* — its
   phases are `artifacts outboard gate clients heavy`, read from
   `scripts/sluice-run.sh` rather than restated from memory — and pushes
   exactly the sha it tested. (`seam-guard` is NOT among them: decision
   0148 took it off the phase list and 0426, which restored `heavy`,
   deliberately did not restore it. It runs only when a human types `make
   seam-guard`.) A branch tip that gated green is not evidence about
   the object that lands (decision 0139).

7. **Memory.** Record what the close learned that the repo does not
   (process lessons, overturned estimates); prune memory entries the
   campaign made stale.

8. **Release the worktree — LEAVE IT IN THE POOL. Do not `git worktree
   remove` it.** `.Codex/worktrees/` is a recycled pool since The Sexton,
   not one directory per campaign. `make worktree-take NAME=<next>` reuses
   the best merged candidate, keeps its warm `target/`, and sweeps
   `.superpowers/sdd/` itself on the way in — so the correct close is to
   leave the worktree clean, unlocked, and where it is. Removing it
   destroys the warm `target/` the pool exists to preserve;
   `scripts/worktree-take.sh`'s own header states the cost: *"73 branches
   went through this repo in one month against 3 live worktrees, each
   carrying 6-29 GB of `target/` and each new one paying a full cold build
   (a measured 771 s). The cost is DESTROYING worktrees, not lacking a
   cache."*

   **Removal is correct for genuine orphans only** — a worktree parked on
   an unexpected ref, a dangling registry entry, a regeneration worktree
   left somewhere it should not be. A merged pool member is not an orphan.

   **This step exists because the skill itself caused the mistake it now
   warns about.** Until 2026-08-31 nothing here was step 7's "teardown"
   despite three references to one, step 7 was *Memory*, and the
   front-matter said the skill applies "before … removing its worktree" —
   so the instrument that runs the close still described removal as the
   close's action, years after the convention changed. The Cartulary's
   closer removed a merged worktree while holding a memory that said not
   to: **a superseded convention living inside a procedural skill beats a
   memory that contradicts it**, because the skill is what is being
   executed. That is the general hazard worth carrying — check the
   procedure, not just what you know.

## Quick reference — what a merged campaign owes

| Artifact | Where | Authority |
|---|---|---|
| Chronicle entry | `book/src/chronicle/<slug>.md` + SUMMARY | decision 0013 |
| Retrospective | `docs/retrospectives/<slug>.md` | decision 0020 |
| Freshness sweep | every chapter the campaign staled | decision 0013 |
| Gradient re-score | `book/src/open-questions.md` (if a bet moved) | decision 0030 |
| Registry flips | `book/src/frontier/idea-registry.md` | registry header rules |
| Keystone refreeze | `cli/tests/fixtures/` etc., from main's tip | merge-time discipline |
| Scratch promotion | `.superpowers/sdd/` (plugin `progress.md`, reports, reviews, mutation proofs) → retrospective + registry rows | step 2A; scratch is git-ignored and is swept when the worktree is recycled |
| Ledger review | `docs/superpowers/ledgers/<slug>.md`, read for post-G3 entries → spec decisions section / decision record; every deferred minor in it → retrospective, with its outcome | step 2B; committed at write time, but unreviewed until read here |

## Common mistakes

- Declaring done from the final summary without the walk — the summary
  comes after step 8, and is not a substitute for steps 1–8.
- Re-scoring nothing because "no bet obviously moved": grep the campaign's
  domains in `open-questions.md` before concluding that.
- Hand-editing generated book pages during the sweep instead of
  regenerating them.
- Writing the retrospective from memory of the campaign instead of from
  the scratch, then releasing the worktree — the material is gone the next
  time it is recycled and nobody can tell what was lost, because the only
  record of it was in the directory just swept.
- **Removing a merged campaign worktree** instead of leaving it in the
  pool. See step 8; this is the one an up-to-date reader still gets wrong,
  because it was correct until The Sexton.
- Treating a red golden pin at close as noise to re-pin silently — it
  means a commit shipped without its re-pin; note it in the retrospective.

When a mechanical DoD gate ships (registry row PROC-7), the artifact
existence checks move to CI; this walk keeps the judgment half — whether
the sweep actually happened and the chronicle is worth reading.
