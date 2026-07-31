# Retrospective — The Shuttle

Process lessons only; the product story is the chronicle.

## What worked

- **Stage the acceptance evidence before touching code.** Task 1 rendered
  the seed-42 world and all three `book` lenses with the pre-change binary
  into scratch, so every later "byte-identical" claim was a `cmp` against a
  genuinely independent artifact, not a same-commit comparison. This should
  be the house rule for any byte-identity campaign: the reference is staged
  *first*, from the *old* binary, or the proof is theater.
- **Count, don't trust.** Twice, an instrumented counter beat a doc
  comment: `doctrine_from` claimed a threading its callee didn't perform
  (found by counting sculpts per render — 16, not 1), and Task 6b's first
  doctrine.rs migration was *worse* than baseline until measurement showed
  the battery was world-building-bound. The Occlusion's "read the output"
  generalizes: instrument the property you're claiming.
- **Adversarial per-task review earned its cost every single round.** Four
  of five tasks came back with a real Important: a wrapper that could
  misclassify "person" on an error path, a degraded path that discarded
  pure-ledger complements, a false performance comment over a surviving
  per-pair lookup, tautological equivalence tests offered as evidence.
  None were visible in green test output.

## What to change

- **A delegating wrapper's equivalence test is a tautology.** The moment
  `x_of` delegates to `x_from`, `assert_eq!(x_of(..), x_from(..))` cannot
  fail. Write it anyway (it reddens if someone forks the bodies later) but
  label it a drift guard and put the acceptance weight on cross-binary
  artifacts. Two review rounds were spent forcing this honesty; next
  campaign can start there.
- **A wall-time criterion over a *test* is ambiguous.** "coherence < 30 s"
  can be met by fixing the product path or by migrating the test's own
  callers. Both happened here, in that order, with the migration ledgered
  as an explicit mid-execution decision (#7) — that sequencing (product
  first; test-caller migration as a named, reviewed deviation with
  assertions untouched) should be the rule, because the dishonest version
  is invisible in a green suite.
- **Flat profiler share ≠ removable cost.** The spec predicted Stage 2
  from a 13 % flat `cell_share` share; most of it was irreducible
  arithmetic under the order constraint. When a criterion hangs on a flat
  self-time number, split it into avoidable overhead vs. essential work
  *in the spec*, or expect the prediction to falsify.
- **Spec contracts need a verb-level checklist at plan time.** "assemble()
  happens once per entry" was a single sentence inside §2; the plan's
  self-review mapped *sections* to tasks and the sentence slipped. It cost
  nothing here (assemble sculpts nothing) but the failure mode is generic:
  extract every MUST-verb from the spec into the plan's Global Constraints
  and check them off, not the section headings.

## Deviations recorded

- Task 6b (worldgen test-battery migration to `_from` callers) — ledger #7,
  mid-execution scope addition serving an explicit spec criterion.
- Spec §2's assemble-once-per-entry: **not implemented**; per-readout
  `WorldComponents::assemble()` remains. No sculpt, targets met anyway;
  follow-up below.
- Task 7's internal helper takes `(floor, floor_pow)` not the plan's
  literal `(beta, floor_pow)` — the plan's signature was unimplementable
  without adding a float op; adjudicated correct in review.

## Follow-ups (promoted from the worktree register before teardown)

- Scene-window build caching — same disease, client-facing surface; the
  `surrounds_scene_in` pair already ships (the Casement line owns it).
- Census metric sweep for residual `*_of` forms (release-speed sculpts).
- Re-profile the language tier post-Stage-1 (terrain canopy is gone; what
  is the new top?).
- Thread `WorldComponents::assemble()` once per entry, or formally accept
  its per-readout cost (the dropped spec sentence).
- Unthreaded `Session::start` sites: `LocaleContext::build`,
  `predator_pressure`, `prey_pressure`, `wild_concentrations` — need new
  worldgen/locale surface; `sky_of` per consult via `true_event_count`.
- Harmonize `render_volume`'s sculpt-failure fallback to
  `parse_context_with_voices`'s degrade-only-the-sculpted-part shape.
- Lab health battery: A* ~28 % share; nav-space precompute is unexplored.
- `make ci` load-blindness (Timekeeper open follow-up) matters more now
  that gates run on the 40-core box alongside campaigns.
