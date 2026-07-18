# The Shadow Track — retrospective

Process lessons from shipping `scene/eclipses/v1` plus its Orrery consumer
(day-scrubber marks + globe ground-track band). Product is in the chronicle;
this is how the work went.

## What worked

- **The visual pass caught what four green gates could not.** Both opus
  whole-branch reviews returned clean on the band's *logic* — geometry unit
  tests passed, it mounted on the right node, tsc and the full suites were
  green — and the band was still invisible in the running app, occluded by
  the globe's 60× relief exaggeration. Only opening the screenshots at the
  campaign's close found it. This is The Lens rule paying off *at the
  producer campaign's own close*, which is exactly where the confidence
  gradient said the check belonged. Budget the visual pass as a real step,
  not a rubber stamp: it found the single most important defect of the
  campaign.

- **Producer-first, two-repo cadence held.** The same shape as The Real Sky
  and The Wandering Sun: land the producer scene + wasm export, hand the
  freshly built binary to the client worktree as a dev-loop copy, build the
  consumer against it, and defer the versioned release to the G6 carve-out.
  The per-task subagent + task-review loop stayed green through eight tasks
  with no rework beyond the two final-review items.

## What to carry forward

- **A plan mandate is not exempt from review.** The plan called for a
  committed producer-sourced golden by reflex — "The Isotherm rule" — and
  the whole-branch review found it pinned nothing and contradicted the
  client's own documented convention. The golden discipline is for values
  the client *recomputes*, not documents it *parses*; for a parse, the wasm
  fixture test is already the end-to-end contract. The lesson is procedural:
  when a review finding conflicts with a plan mandate, that is the human's
  call, and the finding was right this time. Reflexively importing a
  discipline from a neighboring campaign without checking whether its
  precondition holds is how dead artifacts get committed.

- **Diagnose an invisible render by mechanism, not by staring.** The band
  read as absent from the default camera; the temptation was to conclude it
  was on the far side (an eclipse falls at the sub-solar point, which the
  camera does not track). Orbiting the camera a full turn and *still* not
  seeing it ruled that out and pointed at occlusion — a radius below the
  exaggerated terrain — which the relief-exaggeration constant then
  confirmed. The systematic move (rotate to falsify the far-side hypothesis)
  beat guessing.

- **Standalone visual scripts are scratch, and belong in the worktree.**
  The screenshot + orbit scripts must run from the client worktree so
  `@playwright/test` resolves, and they are verification tooling, not
  product — kept out of the commit, with the evidence screenshots preserved
  separately for the G6 package. This matched prior sessions' footgun notes
  and cost no time.

## Absorption cadence

The branch met main once during the campaign (the plan-stage absorption at
close): 15 commits of The Quickening plus a terrain refactor, absorbed with
only a generated `type-audit-report.md` conflict, regenerated. Byte-identity
made the semantic-collision risk low, and the full gate on the merged result
confirmed it. No mid-campaign absorption was needed because the campaign is
additive and touches no shared physics.
