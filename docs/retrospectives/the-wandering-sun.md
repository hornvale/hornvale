# The Wandering Sun — retrospective

Process lessons, not product. The product is in the chronicle.

## What went well

- **The bug found the campaign, and the diagnosis was measured, not
  reasoned.** The whole campaign started from a user observation ("the ice
  is offset by about the obliquity on a 22° world"). The first instinct —
  a spinning-world seasonal-phase story — was *wrong for the seed in
  question*: seed 8 is tidally locked, a different mechanism entirely. What
  caught the error was measuring the actual field (ice-cap centre-of-mass
  vs the anti-solar point across the year) instead of trusting the first
  plausible model. The correct mechanism (locked substellar libration) fell
  out of the measurement, and the spinning-phase bug turned out to be a real
  *sibling* worth folding in — but only because the measurement separated
  the two.

- **The visual check earned its keep twice.** Once at diagnosis (the
  argmax of the temperature field was a red herring — the `cos^0.3`
  plateau makes a flat top, so the hottest single node barely moves while
  the whole field tilts; the field *asymmetry* is the real signal, and only
  looking at the profile rather than the peak revealed the libration was
  working). Once at close (the temperature lens across the year shows the
  hot spot migrating hemisphere to hemisphere — the payoff the golden test
  proves numerically but the eye confirms).

## What the close and execution learned

- **A closed-form cross-repo reconstruction must agree on the sampling
  point, not just the formula.** The producer's golden nearest-snapped map
  tiles to icosphere cells; the client evaluated at tile centres. For every
  prior cross-repo value the client read a *pre-computed* scene layer, so
  the snap was invisible. The first value the client *recomputed* from a
  closed-form function of position diverged by up to a degree. The Isotherm
  bet ("closed-form functions cross the seam faithfully, pinned by a
  producer golden") holds — but this campaign sharpened it: the golden must
  be sampled at the same positions the client reconstructs, or it pins the
  wrong thing. Resolution: a position-based producer evaluator sampled at
  tile centres. The implementer correctly *escalated the mismatch rather
  than fudging constants to force the match* — the discipline that keeps a
  golden meaningful.

- **`make gate` does not check the type-audit report; regenerate it in the
  commit that adds pub items.** The mid-execution producer fix added
  `pub` items with type-audit tags but did not regenerate
  `docs/audits/type-audit-report.md`. The gate stayed green (the report is
  CI-checked, not gate-checked — a known trap), and the staleness only
  surfaced at the close's drift check. A commit that adds a pub-boundary
  primitive owes the report regen in the same commit, not at close.

- **Two type-audit merge conflicts in one campaign, both on the generated
  report.** Absorbing main twice (parallel sessions are the norm) conflicted
  on `docs/audits/type-audit-report.md` both times — a generated file
  resolves by regenerating from the merged tree, never by hand-merging.
  Worth a mechanical `merge=union`/regenerate hook so the generated report
  stops being a recurring merge cost.

## What to carry forward

- **Producer-first, two-repo, holds up again.** Same shape as The Real
  Sky: five producer tasks (climate → scene → golden), four client tasks,
  one controller assembly, two whole-branch reviews (one per repo). The
  producer-fix mid-stream (the tile-center evaluator) slotted in as a
  controller-driven task without disturbing the sequence.

- **The release-time golden refresh is still owed.** The seed-42 climate
  goldens (`climate-triples`, `region-temperature`) predate this campaign's
  producer, so their equivalence tests ran at `yearPhaseOffset=0` behind a
  clearly-commented bridge. They are re-pinned at the world-wasm-v6 bump —
  the same release-cadence debt every campaign that vendors a fresh binary
  inherits. A release-time refresh step (regenerate every producer-sourced
  golden when cutting a world-wasm release) would retire this recurring
  line item once.
