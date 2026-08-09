# The Pigment — retrospective

Process lessons, not product. The product is the chronicle.

## The headline: mutation testing found eight tests that could not fail

Every task in this campaign was executed by a subagent under a standing
instruction to **mutate its own deliverable before reporting** and paste the
mutated run. That instruction was added after Task 1, and it changed the
campaign.

Eight tests shipped — or nearly shipped — that passed for the wrong reason.
Five were caught by the implementing agent in its own work, which is the
outcome worth designing for; three by the controller reviewing afterwards.

| # | The test | Why it could not fail |
|---|---|---|
| 1 | `area_mixing_is_the_weighted_arithmetic_mean` | Asserted `0.5` for `0.25·0.2 + 0.75·0.6` — real-number arithmetic. In binary the answer is one ULP lower, and the *only* way to reach `0.5` is the fused `mul_add` the campaign forbids. The test was, precisely, a test that the implementation was fused. |
| 2 | `the_standard_observer_projects_to_srgb_but_a_synthetic_one_does_not` | `to_srgb` refuses on two conditions; the fixture was five-channel, so the arity check always answered and the `srgb_native` flag was never reached. Flipping every observer to sRGB-native left the whole suite green. |
| 3 | The containment test for `Star::t_eff` | Generated the same star twice and compared fields — a *determinism* test wearing a containment test's name. It passes identically whether or not `t_eff` leaks into physics. |
| 4 | `a_felsic_rock_is_brighter_than_a_mafic_one` | Moved silica *and* rock class together, so `is_mafic_dominated` alone satisfied it and `buf.silica` could be dropped from the felsic path undetected. |
| 5 | `the_mixture_keeps_its_components_for_the_texture_layer` | Asserted `integrate().get()[0] >= 0.0`, which `integrate`'s own clamp makes unfalsifiable. |
| 6 | `brown_is_a_darker_red` | "Darker than red" and "leans long" are true of *any* plausible brown. |
| 7 | Claim 1, as planned | Hardcoded `PackDepths` literals, so flattening `pack_depths` would not have reddened the claim that exists to trace to it. |
| 8 | The terrain-lens byte goldens | They build through the uncoloured builder, so the tinting arm can never fire on them. They prove the lens does not tint an *uncoloured* scene — trivially true. |

**The generalisation.** Six of the eight are the same shape: an assertion
whose *precondition* is doing the work instead of the claim. The cure is
mechanical — name the endpoints, vary one axis at a time, and check that the
fixture can actually reach the branch under test. Number 8 is the nastiest,
because the failing artifact would have been committed *by* the defect and so
the golden would have pinned the wrong bytes as correct.

This confirms memory `a-passing-test-can-pass-by-the-wrong-path` and
`mutate-the-deliverable-tests` hard enough that the mutation instruction
should probably be standing policy in the dispatch preamble rather than
per-campaign prompt text.

## Plan-authored code is the one code nothing compiles — again

The plan's snippets carried four defects into execution: `Seed::new` (does not
exist; it is `Seed(42)`), `RockClass::Limestone` (it is `ReefLimestone`),
`MaterialBuffer::default()` (no `Default` derive), and `place_latlon` used
from an integration test where it is `pub(crate)`. Every one was caught at
compile time and cost a round trip.

Two lints bit *every single task*: `needless_range_loop` and
`assign_op_pattern` on `for b in 0..BANDS { out[b] = out[b] + … }`. After Task
1 they went into the plan's Global Constraints and the later dispatches
pre-empted them, which worked — but they should have been there from the
start, because the shape is unavoidable in a banded-spectrum campaign.

**Lesson:** grounding snippets on a real repo helper (which the plan did do,
by file:line) is necessary and not sufficient. The remaining gap is
constructors and enum variants, which no amount of citing a *nearby* line
catches.

## A preregistered claim was ill-posed, and that is my error to own

Claim 2b was frozen after Task 4 measured that the illuminant dims about
eightfold as well as reddening. The worry was legitimate: naming compares in
signal space, so a dimmed sample drifts toward the dark exemplar for reasons
unrelated to hue, which would have made claim 2 satisfiable by "everything is
dark at dusk."

It could never have discriminated anything. Task 6's correction — deciding
achromatic terms on luminance and hue terms on chromaticity, both
self-calibrated against exemplars under the current light — makes naming
*exactly* invariant to uniform illuminant rescale. Peak-normalization is such
a rescale. Claims 2 and 2b were the same experiment, always.

The sequencing is the lesson. 2b was written **between** the measurement that
motivated it (Task 4) and the correction that mooted it (Task 6). Nothing
re-derived whether the guard was still needed once the thing it guarded
against had been designed out. **A preregistered claim should be re-checked
for well-posedness whenever the mechanism it constrains changes** — not just
for whether it still *matters*, but for whether it can still *fail*.

It was retired as ill-posed rather than reported null, because "null" asserts
a measurement that never happened. What it accidentally proved — colour
constancy — became a property test and a documented constraint on campaign 2.

## Measuring before preregistering worked

The spec's risk 3 said the illuminant range might never move a colour name,
and required that be measured *before* the claim was preregistered. Task 4
measured it, and the answer changed what got frozen. That sequence —
measure, then freeze — is the reason the campaign has a real finding rather
than a green test.

The related discipline also held under pressure: when claim 2 came back null,
nothing was retuned. Not an exemplar, not a scattering constant, not either
naming threshold. The sample was not swapped after unblinding. The 46% sweep
is recorded as exploratory and is kept visibly separate from what was
predicted. The implementing agent proposed the swap and correctly declined to
make the call itself.

## The design correction came from the repo, not from cleverness

The spec's naming design ("nearest exemplar in signal space") was wrong, and
measurement killed it. The fix was already in the data model: `color_pack`'s
hue ladder puts `dark`/`light` at rank 1 because Berlin & Kay's stage I is
achromatic, and `PackDepths` had carried `hue` and `luminance` as separate
fields since long before this campaign.

**Lesson:** when a design fails on measurement, read the data model it sits on
before inventing a replacement. The structure that would have prevented the
mistake was documented and shipped; the spec simply did not consult it.

## What the artifact goldens do and do not cover

Two blind spots surfaced, both worth carrying forward:

- The terrain-lens goldens render *uncoloured* scenes, so they cannot catch a
  regression that tints the terrain lens (row 8 above).
- `windows/locale`'s max-weight-corner rule — which grid cell a room inherits
  from — has **no in-crate test**. Breaking it left all 35 locale tests green;
  only committed artifacts caught it, and the scene crate's new colour test is
  the first in-crate assertion that does. Captured as
  `PIGMENT-locale-corner-rule`.

Both are instances of the same thing: an artifact pin proves a *rendering* is
current, never that it *covers* anything (memory
`artifact-coverage-is-not-artifact-freshness`).

## Process notes

- **The type-audit report drifted invisibly for eight tasks.** `make gate`
  runs the type-audit `check`, not the `report`, so a campaign of
  pub-boundary additions accrues report drift no gate can see. It surfaced
  only because Task 3's agent went looking. Making the regen an explicit
  numbered plan step (rather than an assumption inside "run rebaseline")
  is what got it done.
- **Spending lefford's `make ci` free pass deliberately** — on clean main,
  before any of this campaign's ~40 tests existed — is what makes the
  campaign's duration cost measurable by subtraction. First-run-never-alarms
  is documented; *choosing where to spend it* is not automatic, and it is a
  one-shot resource per host.
- **The duration alarm fired 59 times, and every one was contention.** The
  close's `make ci` reported 59 per-test regressions at 2.0–2.5×, *all* in
  `hornvale-worldgen`. A uniform multiplier across one crate's entire test
  set is the signature of contention rather than a code change — a real
  regression hits specific tests by specific amounts. Worldgen is where it
  shows because its tests are the long CPU-bound world-building batteries,
  which get descheduled under load while short tests finish before it bites.

  **Attributed by A/B rather than by argument.** The same five
  `history_tithe` tests, run under the same nextest `ci` profile on a quiet
  box: `origin/main` 2.19–2.93 s, `the-pigment` 2.90–3.12 s, recorded
  baseline 2.94–3.10 s. The campaign is indistinguishable from the baseline
  and the alarm's 6.3–8.3 s figures are unreproducible. Nothing was
  re-recorded; the guard's refusal to write a contended baseline was
  correct and the baseline stands.

  **The mechanism is The Timekeeper's documented blind spot (1), reproduced
  independently.** The guard only asks whether a *census claim* is held, so
  a parallel agent session is invisible to it. Worse than the documented
  case: the box was verifiably quiet at launch (loadavg 0.68 sustained over
  three checks a minute apart) and another session began work *during* the
  16-minute run — loadavg was 38 by the time it finished. **A pre-flight
  quiet check is necessary and not sufficient**, which strengthens the
  standing candidate fix: suppress when loadavg exceeds core count, sampled
  *during* the run rather than only at its start.

- **Check the claim before a long run, not after it refuses.** The first
  close attempt burned a 13-minute `make ci` that `ci-record` correctly
  declined, because another campaign's `heavy-run.sh` had held the box for
  57 minutes already. One `bash scripts/census-run.sh status` would have
  said so. The gate run immediately before it was also contended — its
  result stands, since correctness does not care about load, but its
  timing is meaningless.

- **The two-cargo-runs guard fired once mid-mutation-test** and the agent's
  follow-up run reported a stale tree's result. It caught this by grepping
  the source for its own mutation marker before trusting the output. Worth
  doing every time: after a blocked or retried command, verify *what tree you
  are actually measuring*.
- **Sequential dispatch was the right call.** Tasks 3, 5 and 6 were mutually
  independent and could have run in parallel, but they share one worktree's
  git index; the isolation that would make it safe costs a merge per task.
  More importantly, three of the eight defects above were fixed in the *plan*
  between tasks, which parallel execution would have missed.

## Follow-ups

| # | Item |
|---|------|
| F1 | `PIGMENT-locale-corner-rule` — locale's max-weight-corner rule needs an in-crate property test. |
| F2 | The terrain-lens byte goldens cannot catch a tinting regression; consider a coloured-scene golden. |
| F3 | Make "mutate your own deliverable before reporting" standing dispatch-preamble policy, not per-campaign prompt text. |
| F4 | `red`'s exemplar renders as `#987A3E`, a muted ochre nobody would call red — the observer's medium channel is 0.72-sensitive where red rises. Harmless for naming (which compares signals) but any visualization needs a caption. |
| F5 | The colour lens draws a flat wash at walking depth because the chart's own fields are equally flat there. Not a colour defect; revisit if room-level lithology ever gains sub-grid resolution. |
| F6 | **The duration baseline carries no rows for this campaign's ~45 new tests.** They are all sub-second and fold into `<below-floor>`, and no suite-level shift was reported, so nothing regressed — but the first clean `make ci` after merge will legitimately bump that row's summed seconds and count. That is a normal record, not a regression to investigate. |
| F7 | Strengthen The Timekeeper's blind-spot-1 follow-up: sample load *during* the run, not only at launch. This campaign's close was launched on a verifiably quiet box (loadavg 0.68 sustained) and still measured 59 contended tests, because another session started mid-run. |
