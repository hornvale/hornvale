# Retrospective — The Firmament

Weather Program campaign 4: drawn weather — a sampled synoptic weather-state
field (Clear/Fair/Overcast/Rain/Storm + cirrus) whose cloud type is the
state's face, felt at a place and day (possession/almanac), level-0
byte-identical. The globe's typed-cloud *rendering* was deferred at the
visual pass. Process lessons only.

## What worked

- **The multi-stage review caught a real defect on every pass — including
  ones the implementer, the controller, and the plan's own mutation-verify
  step all missed.** T1's smoothness test was *vacuous* (it only counted
  3-rung state jumps, which a mid-propensity cell never makes, so it passed
  even for a fully decorrelated phase); the implementer surfaced it honestly
  via mutation-verify, and the fix was a self-calibrating measure on the
  mechanism (adjacent-day deltas ≪ far-day deltas). Then the whole-branch
  reviewer proved `CloudType::Cirrus` was **structurally unreachable** — the
  precursor window sat outside the clear-sky threshold, so the overlay was
  dead code only the test's synthetic input masked. A reachability test over
  the *real* composition is now the guard.

- **The Lorenz guard-rail held by construction.** Weather is a pure function
  of `(cell, day, seed)` — a drifting-Fbm phase over a climatological
  propensity — so nothing integrates, and the level-0 byte-identity claim
  was independently re-verified (two seeds, world JSON unchanged before and
  after the seed was threaded into the climate provider).

- **The felt-weather payoff landed cleanly** and validated the cirrus fix
  end-to-end: possession renders *"the sky is clear but for high cirrus"* —
  the exact variant that had been dead code.

## What was hard (the cloud rendering, and how it went wrong)

- **The visual pass caught a legibility failure, then a stale build masked
  the fix for two iterations.** The clouds first drew as 1px hairlines (the
  Gyre's line-width cap). Converted to `THREE.Points` sprites — but the
  Playwright capture server serves the built `dist/`, and
  `reuseExistingServer` kept a *stale* build, so the Points conversion AND
  the first size-tuning passes were all rendering the old LineSegments. Two
  wasted iterations chasing "the constants do nothing" before realising the
  capture never saw the new code. **Lesson: when visually verifying an
  orrery change through Playwright, rebuild (`npm run build`) and kill the
  reused server before each capture — the served artifact is `dist/`, not
  live source.**

- **Billboarded soft sprites read as atmospheric haze, not discrete clouds.**
  Once the build was fresh, tuning size/count/threshold/lift across ~5 more
  passes never crisped the clouds into legible typed systems on a
  sixty-times-relief globe — they stayed a soft grey shell. After two
  rendering approaches and ~7 tuning passes (well past the 3-attempts rule),
  the right move was to STOP and bring the quality/effort tradeoff to
  Nathan rather than keep grinding or quietly ship haze.

- **Nathan's call: ship felt-weather, defer the globe clouds.** The campaign
  became hornvale-only — the sim ships the scene fields (producer ready), the
  orrery consumer and a *proper* cloud-rendering approach (textured sprites /
  crisper puffs / limb-fade / occlusion, not billboarded soft points) are a
  dedicated follow-up, bundled with the world-wasm-v11 release when the
  consumer ships. No epoch, no census, no release this campaign.

## Process notes

- **A "total projection" enum variant can be structurally unreachable.** The
  guard is a reachability test over the *real* composition, not hand-built
  inputs — hand-built `(Clear, cirrus=true)` masked the dead path.
- **`git checkout <file>` to revert a mutation wipes uncommitted edits.** Hit
  again this campaign (reverting a size mutation lost re-applied fixes);
  revert mutations by editing back, not `git checkout`, when the working tree
  holds unsaved work.
- **An implementer re-pinned the byte-test fixture but not the drift-checked
  gallery scene export** — caught in controller review; the gallery artifact
  would have failed CI's freshness gate. Regenerating touches both the test
  fixture AND the gallery export.
- **The orrery task brief lived in the hornvale worktree** (the `task-brief`
  script writes there); for cross-repo tasks, copy the brief into the target
  worktree or embed the spec in the dispatch prompt.

## Follow-up

- **Globe typed-cloud rendering** — the deferred half. Scene fields
  (propensity + cloud type) already ship; the follow-up is the orrery
  consumer with a rendering approach that yields legible, typed, discrete
  clouds, plus the world-wasm-v11 release. WIP reference exists on the orrery
  `the-firmament` branch (parse + style table + sprite scaffold).
- **Weather-gods** (storminess climatology → religion) and **disturbance →
  biome feedback** remain the sequel epoch and the level-2 tier.
