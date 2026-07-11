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

1. **Preflight the integration.** From the campaign branch: `make
   preflight`. On NO-GO: merge main INTO the branch, re-run the full gate
   there, re-run the preflight. Repeat every time main moves — parallel
   sessions are the norm here, not the exception. If this preflight is the
   branch's first meeting with main since the campaign began, the
   stage-boundary absorption cadence (CLAUDE.md Process) was missed —
   record that in the retrospective.

2. **DoD artifacts, on the branch, before merging:**
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
     `the-confidence-gradient-is-re-scored-not-frozen`).
   - **Registry flips** — idea-registry rows the campaign shipped,
     spec'd, or rejected flip status and repoint **Where**; never delete
     a row.
   - **Plan hygiene** — `IMPLEMENTATION_PLAN.md` deleted once all stages
     are done; the spec/plan files note completion.

3. **Verify golden-pin discipline held.** Pins should have been re-pinned
   in the commits that drifted them, not deferred to now. If the gate is
   red at close, that is a missed re-pin to chase back, not a value to
   quietly accept here.

4. **Refreeze keystone fixtures from main's tip** at merge, so identity
   invariants measure exactly this campaign's delta — stale pre-campaign
   freezes alias other campaigns' physics into yours. These are the frozen
   identity fixtures (e.g. `cli/tests/fixtures/world-seed-42.json`), a
   different thing from step 3's golden pins: pins re-pin in the drifting
   commit, keystones refreeze at merge.

5. **Full gate + artifact drift on the merged result** (`make gate`, then
   `git diff` after `make rebaseline` if worldgen moved). Only then
   fast-forward main.

6. **Memory.** Record what the close learned that the repo does not
   (process lessons, overturned estimates); prune memory entries the
   campaign made stale.

## Quick reference — what a merged campaign owes

| Artifact | Where | Authority |
|---|---|---|
| Chronicle entry | `book/src/chronicle/<slug>.md` + SUMMARY | decision 0013 |
| Retrospective | `docs/retrospectives/<slug>.md` | decision 0020 |
| Freshness sweep | every chapter the campaign staled | decision 0013 |
| Gradient re-score | `book/src/open-questions.md` (if a bet moved) | decision `the-confidence-gradient-is-re-scored-not-frozen` |
| Registry flips | `book/src/frontier/idea-registry.md` | registry header rules |
| Keystone refreeze | `cli/tests/fixtures/` etc., from main's tip | merge-time discipline |

## Common mistakes

- Declaring done from the final summary without the walk — the summary is
  step 7, not a substitute for steps 1–6.
- Re-scoring nothing because "no bet obviously moved": grep the campaign's
  domains in `open-questions.md` before concluding that.
- Hand-editing generated book pages during the sweep instead of
  regenerating them.
- Treating a red golden pin at close as noise to re-pin silently — it
  means a commit shipped without its re-pin; note it in the retrospective.

When a mechanical DoD gate ships (registry row PROC-7), the artifact
existence checks move to CI; this walk keeps the judgment half — whether
the sweep actually happened and the chronicle is worth reading.
