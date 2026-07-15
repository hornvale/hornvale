# Retrospective — The Helm

**Campaign:** the Orrery client's controls — camera agency, per-rung
clock, True Scale, inspector, the rename, Playwright in CI.
**Scale:** one session, brainstorm → G3 → plan → 10 tasks → final review
→ G6, second full-autopilot campaign end to end; zero mid-campaign
questions to Nathan (two hard stops, one of them his one-line rename
ruling).

## What worked

- **The per-task review loop caught three real interaction bugs the
  plan's own code carried** — a paused date line that went stale on
  jump/scrub, a mid-transition wheel that could whipsaw the zoom off a
  frozen camera pose, and a camera clamp that parked the user exactly on
  the handoff trigger after a true-scale toggle. All three were
  cross-feature seams (clock × HUD, handoff × transitions, scale ×
  handoff) — exactly the class per-task implementers can't see and
  reviewers primed with "trace the lifecycle" questions can. Controller
  applied each fix inline (a few lines each) and re-reviewed; no fix
  wave, no per-finding fixers.
- **Implementers licensed to disprove the plan used the license well,
  four times.** A test tolerance the plan set at float64 precision
  against float32 storage; a test asserting a field the plan's own
  implementation never populated; a temporal-dead-zone trap in the
  plan's "call applyView at mount"; a vitest/Playwright test-glob
  collision. Each came back disclosed, reasoned, and minimal — the
  Goldengrove lesson (plans are transcription aids, not oracles)
  holding at its second campaign.
- **Rename-day economics were real.** Renaming one day post-launch cost
  one task: a `gh repo rename`, four client files, two book links, and
  one deploy watch. Pages URLs don't redirect, so every week of delay
  would have grown the broken-link surface from ~zero.
- **The retro-to-mechanization loop closed in one campaign.**
  Goldengrove's #1 follow-up (real browser in CI) shipped here as Task 2
  and then *guarded the rest of this same campaign* — the e2e ran under
  the renamed base path from Task 2 onward, exactly the bug class it
  exists for.

## What failed, and the lessons

- **The plan's example code shipped two of its own bugs into tasks**
  (the stale date line, the missing `controls.enabled` guard). Complete
  code in plans buys cheap transcription, but the plan author is the
  single point of failure for cross-feature seams that don't exist yet
  when each task is written. The review loop is the net — budget for it;
  a plan-as-complete-code campaign without per-task reviewers would have
  shipped both.
- **"Hands-on verify" steps in a plan executed by headless agents are
  dead letters.** Three tasks said `npm run dev` and eyeball it; no
  agent could. The e2e covered the assertable half, and the controller
  deferred the feel half (damping, snap vs ease, the true-scale hunt) to
  Nathan's first minute — which is honest, but the plan should have said
  that from the start instead of writing steps nobody could run.
- **Worktree ≠ sibling checkout.** The client's `wasm:local` script
  assumes `../hornvale`; from the campaign worktree that path pointed at
  nothing. Cost a few minutes and an absolute-path copy. Client-repo
  scripts that reach for sibling checkouts should take an env override.

## Follow-ups

1. **ORRERY-surface-descent** (registry, raw/high): the ladder's lower
   rungs; blocked on a local-relief data feed — the first wasm-catalog
   widening under decision 0055, alongside ORRERY-ellipse-truth.
2. Accepted minors from the final review, all cosmetic-to-narrow:
   date-jump doesn't clamp day-of-year to year length; `scale.ts`
   mirrors five schematic constants from `system.ts` (tests pin them);
   true-scale-off re-frame snaps rather than eases; a near-unreachable
   shared-`downAt` mid-gesture window; far-side label sprites raycast to
   the near globe (the captured label-declutter item).
3. Camera-pose deep links (orbit angle/distance in the URL) — captured,
   not built.
