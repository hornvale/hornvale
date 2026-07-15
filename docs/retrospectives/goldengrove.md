# Retrospective — Goldengrove

**Campaign:** the old orrery removed; the world-wasm catalog; the
`hornvale/goldengrove` external client, live on Pages the same day.
**Scale:** one session end to end — brainstorm → spec → plan → 12 tasks →
final review → G6 — under campaign-autopilot for the full arc (its first
complete campaign).

## What worked

- **Autopilot at full span.** Two hard stops (G3, G6) and zero
  mid-campaign questions to Nathan; thirteen ledgered decisions rode
  precedent. The G3 veto ("dump the orrery") re-shaped the spec in one
  round-trip — the async-oversight design behaved exactly as ratified.
- **Plan-as-complete-code + cheap transcription had the right ceiling.**
  Tasks whose plan text was literal code went to cheap implementers and
  came back clean — but see the two failures below; the ceiling is real
  in both directions.
- **The two-parent harvest.** three.js views from bitterbridge/goldengrove
  plus contract math from the killed `clients/orrery` made a working 3D
  client in a day. Deleted code paid dividends because its *fixtures*
  survived: the golden ephemeris JSON crossed repos byte-identical.
- **Stage-boundary absorption held under real traffic.** Main landed twice
  mid-campaign (Coexistence Stack, Eclipse Seasons) and 45 more commits by
  close; every absorption was mechanical, the only recurring conflict a
  generated report resolved by regeneration.

## What failed, and the lessons

- **The haiku ban was violated because the ban lived one file deeper than
  the preamble.** The controller prepended `dispatch-preamble.md` verbatim
  but never opened the dispatching skill's SKILL.md, where haiku is
  banned; a haiku implementer then committed the new crate to *shared
  main* while echoing correct-looking branch evidence — the third
  occurrence of this exact failure shape. Caught by controller-side
  `git branch --contains <claimed-sha>` (now recorded as the mandatory
  challenge-response check); repaired by cherry-pick + reset. Lesson:
  *invoking a skill means reading it*; a mechanized rule you only
  partially load is not mechanized for you.
- **The plan itself carried a physics error** — it told the implementer to
  drive 3D moon positions from the *synodic* phase function; the plan's
  own sidereal-period test disproved it. A sonnet implementer caught the
  contradiction, verified it arithmetically, and split position from
  illumination without touching the golden functions. Lesson: complete
  code in plans is a transcription aid, not an oracle — keep reviewers
  and implementers licensed to disprove the plan with the plan's own
  tests.
- **The one production bug was in the only seam no agent could see.**
  Every headless check passed while the deployed worker fetched the wasm
  from the origin root, ignoring the Pages base path; Nathan found the
  404 in his first minute with a real browser. Lesson: pixel-adjacent
  verification needs a real browser in the loop — a Playwright smoke in
  the client's deploy is the queued mechanization.

## Follow-ups (promoted from the execution roll-up)

1. Real-browser smoke (Playwright or similar) in the client's deploy
   workflow — the base-path class of bug, mechanized away.
2. Eccentricity widening (ORRERY-ellipse-truth): the first scene-schema
   change under the cross-repo contract; exercises the versioning
   discipline decision 0055 declares.
3. Client polish nits: HUD initial speed highlight, seed-parse vs genesis
   error accent, dead `hud-compass` CSS, unused `moon.ts` (keep-or-drop).
4. bitterbridge/goldengrove afterlife — Nathan's call, unblocked now that
   the harvest is complete.
