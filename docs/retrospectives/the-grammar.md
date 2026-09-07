# The Grammar — retrospective

*Process lessons. The chronicle carries the product story; the spec carries
the boundary; the campaign ledger carries the design rulings.*

**Stage gate:** green at `e2ab208faf5d2142283ac35cbdf81fbf10b9f1fc`
(`req-e2ab208faf5d-20260906T235645Z`, all stage phases, 1,531 seconds).
Merge remains pending.

## The fix wave introduced the last load-bearing defect

The whole-branch review correctly found that empty and all-zero distributions
were being accepted for applicable reproduction. That collapsed “not
measured” into “measured as zero,” so the fix required positive-weight
offspring, survival, care, role and hybrid measurements whenever reproduction
was possible.

The rule was too broad. Ordinary reproduction does not imply that hybrid
compatibility was queried, but the fix required a hybrid distribution whenever
an ordinary pathway existed. An ordinary profile with no hybrid partners was
therefore rejected by the boundary created to admit it. The scratch review
named this as the residual blocker after its one permitted fix wave; the
separately authorized correction shipped in `61401a616` with an explicit
`hybrid_applicable` fact and a worldgen regression for ordinary reproduction
without partners.

The lesson is narrower than “review fixes need review.” Applicability belongs
to the measurement, not to the enclosing object. A group of sibling
distributions can share validation machinery while still answering independent
questions; inferring all of their applicability from one nearby count creates
a plausible rule with the wrong domain.

## Finite inputs do not guarantee finite outputs

Task 3 needed two review rounds for the same arithmetic family. Every authored
weight was finite and non-negative, yet summing sufficiently large weights
overflowed before normalization. After that was refused, a weighted care mean
could still overflow from finite entries. The final implementation checks both
the distributions and every derived scalar before constructing the handoff.

The first fix was not incomplete because it missed another input validator. It
was incomplete because validation stopped at the input boundary while the
contract's promise — a finite social handoff — concerns the output boundary.
When arithmetic derives public values, validate the promised values as well as
their ingredients.

## Name the evidence a test actually observes

The plan and first Task 5 report called a structural repeatability check
“cross-seed.” The production summary accepts no seed and performs no draw; the
test varied selection labels that were only metadata. Review corrected the
name to `seedless_structural_handoff_repeats_without_realization_draws` instead
of selling a stronger result than the instrument could produce.

That correction preserved the architecture: actual cross-seed variation
belongs to the future population-realization layer. The useful process rule is
to describe a determinism test from the entropy its production path really
consumes, not from seed-like values arranged around the call.

## Cheap boundary checks found expensive-looking mistakes

The preflight scan corrected the demography test layout and the plan's task
headings before implementation. Task 1 review then added duplicate-validation
and authored-order coverage without changing production behavior. Task 4's
first commit gate caught an unrostered full-world build in a zero-drift test;
using the existing seed-42 fixture kept the proof while avoiding another
expensive build site. The next check caught the fixture reconstruction's
required lint annotation. These were small corrections because the repository
made the boundary visible early.

The adapter's zero-drift proof is intentionally limited: it establishes that
the opt-in reproductive substrate is independent of existing artifacts today.
It cannot prove non-interference once a future bake actually consumes the
substrate. The later integration must replace or strengthen that witness at the
point where behavior becomes live.

## Deferred minors and follow-up outcomes

Every item from the committed ledger and the campaign scratch has an explicit
outcome:

- The ledger's probe-shape follow-up is closed. The eight named probes are
  frozen as test fixtures and are asserted absent from the canonical species
  registry; the spec's implemented-shape section and The Grammar chronicle are
  their permanent homes.
- The ledger's compatibility follow-up is closed at the substrate level.
  Compatibility is a tested directional relation with typed assistance and no
  genus tree or species-name allowlist. Canonical hybrid populations remain
  outside this campaign, as the updated BIO-3 registry row states.
- The ledger's magical-transition follow-up remains deferred until a magic
  campaign can define cost, access, reversibility and realization. The BIO-3
  registry row carries that seam; no separate raw row is needed because this
  is unfinished scope of the existing BIO-3 boundary, not a new idea.
- Task 2's two coverage minors — independent missing-role cases and additional
  successful/assisted combinations for mixed assistance requirements — are
  accepted as-is. Existing tests cover the required role failures, assistance
  precedence and retained requirements; review found no behavior gap.
- Task 4's stronger zero-drift witness is carried forward in the BIO-3 row as
  a condition on activating population realization. The present independent
  adapter/non-interference proof remains the correct witness while the adapter
  is inert.
- Task 5's true cross-seed experiment is carried forward with population
  realization in the BIO-3 row. The shipped claim remains seedless structural
  repeatability only.
- The scratch residual requiring hybrid applicability independent from
  ordinary reproduction is closed by `61401a616`, with both demography and
  worldgen regressions. It is not left as a ledger-only warning.
- Task 4's remaining scope notes — no authored reproductive registry, no live
  bake integration, no cohorts, and no social simulation — are divided between
  the BIO-3 and SOC-2 registry rows. BIO-3 is shipped only as a substrate;
  SOC-2 remains elaborated with its successor contract shipped.

No Confidence Gradient row mentions the BIO-3/SOC-2 boundary or its
reproductive invariants, so no score moved. No census was run or needed: the
adapter is opt-in and the campaign deliberately changed no generated world
behavior.

## Do differently next time

When strengthening missing-data validation, write one applicability predicate
per measurement family before sharing the validator. When a test is described
as cross-seed, inspect the production signature and identify the draw it
actually varies. And after a final fix wave, exercise the ordinary case nearest
every new exceptional requirement: here, “reproduces, no hybrid partners” was
the smallest example and would have exposed the regression immediately.
