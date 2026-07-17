# The Real Sky — retrospective

Process lessons, not product. The product is in the chronicle.

## What went well

- **Producer-first task order held its promise.** The plan sequenced the
  five hornvale tasks (schema → CLI/gallery → reference page → MoonElem
  rider → wasm) ahead of the four orrery tasks, and made Task 6's brief
  preflight-check that the fresh binary was already in the consumer
  worktree. The orrery's fixture strategy tests the *vendored binary*, not
  a committed JSON copy, so the consumer literally cannot start honestly
  before the producer ships a binary — the plan made that dependency
  explicit instead of leaving it to be discovered mid-task.
- **Verify-the-generator paid at the whole-branch tier, not the task tier.**
  Every per-task review was clean or near-clean, but the two Opus
  whole-branch reviews each caught a cross-cutting issue no single-task
  reviewer could see: the producer review re-derived that the exported
  starfield seed is byte-identical to the almanac's `figures()` population
  (not a second, differently-seeded field), and the consumer review found
  the pole-lean seam between the new star shell and the pre-existing world
  axis. Both required holding two files at once that no task brief spanned.

## What the close learned that the tasks did not

- **The visual check is not optional, and it found the campaign's most
  important fact.** Every unit test passed — 297 of them — because they
  instantiate the wasm directly in Node, bypassing the browser boot path.
  Opening the actual rendered frame (The Lens's standing lesson) revealed
  the browser app does not boot at all under its deployed sub-path: the
  wasm 404s. Systematic debugging traced it to `vite base: './'` (set two
  days earlier, in an unrelated orrery commit) feeding an unchanged
  `catalogUrl('./', origin)` that resolves to a rootless URL, with the one
  base value that actually ships — `'./'` — being the one `worker.test.ts`
  never covers. **The Real Sky did not cause it and reproduces on main; it
  also breaks production.** The campaign's e2e discipline surfaced a live
  pre-existing regression that every green unit suite had hidden. Logged as
  its own follow-up, not folded into this branch.

- **A pre-existing render seam becomes newly visible when you add the thing
  that would collide with it.** The world-axis lean has been in the orrery
  since before this campaign, asserted in tests only on its `y` component,
  never its `z`. It was inert because there was no sky pole to disagree
  with. Adding the real sky created the counterpart, and the seam appeared.
  The lesson for close discipline: when a campaign introduces the *first*
  consumer of a convention, re-check the pre-existing code that already
  used a neighbouring one — the collision is invisible until both exist.

## What to carry forward

- **Two-repo campaigns need two whole-branch reviews, one per repo.** The
  producer contract and the consumer render are different review surfaces
  with different binding constraints (determinism/byte-identity on one
  side, faithful-transform/no-re-derivation on the other). A single
  reviewer spanning both would have diluted both lenses.
- **Cross-repo goldens stay a standing liability.** The orrery's two
  climate goldens drifted not from anything this campaign did but from
  hornvale climate work that merged after the last world-wasm release,
  surfacing the moment a fresh binary was vendored. The Isotherm lesson
  (producer-sourced goldens) prevents the goldens from being *wrong*, but
  not from being *stale*: any campaign that vendors a fresh binary inherits
  the accumulated producer drift of every campaign since the last release,
  and must re-pin it. Worth a mechanism — a release-time golden refresh —
  rather than paying it per campaign.
