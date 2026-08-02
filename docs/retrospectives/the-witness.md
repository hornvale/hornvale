# Retrospective — The Witness

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-witness.md): three branches that
could never fire, repaired, and the guard that makes the shape — a check
whose input is authored rather than derived — detectable the next time it
recurs.

## What went wrong, and what would have prevented it

- **Every number this campaign authored rather than measured was wrong.**
  The porosity-coefficient sweep range (`k_g ∈ [0.10, 0.30]`), specified in
  the spec before any measurement, sat entirely below the analytic minimum
  (`≈0.414`) needed to reach the gate it was meant to cross — the sweep came
  back cleanly BLOCKED rather than silently near-missing, which is the
  version of this mistake worth having. The first repair pass then measured
  its own porosity distribution at `Geosphere::new(4)`, a resolution the
  production model does not run at; re-measured at the real level (6) with
  the real land-cell filter, the distribution was a different shape entirely
  (clastic porosity is one value across p25–p95, not the spread the level-4
  sweep showed). Both of Task 5's calibration guesses — the threshold's
  position in the band, and the constant's justification — had to be
  re-derived once the population was corrected. Four failures, one root
  cause: a number written down before the measurement that would justify it
  existed.

- **A floor-only test let 69.64% of land become `Aquifer` without reddening
  anything.** Task 5's world-derived guard asserted only that `Aquifer` and
  `Spring` were *reachable* — exactly the property the campaign's own
  headline evidence (a dead census column) says matters. It does not, by
  itself, say anything about whether a repaired branch now eats the world.
  The corrected guard added a ceiling alongside the floor (aquifer bounded
  to a loose band, not a point), and the ceiling is what would have caught
  the first threshold's real defect immediately rather than at the next
  measurement pass. A reachability proof is half of an assertion about a
  branch; the other half is a bound on how much of the world it claims.

- **The campaign committed its own defect shape at least four times, inside
  the campaign whose subject is exactly that shape.** A placed-species roster
  derived with a fixed-width `grep -B 20` lookback silently returned four
  species instead of five, because the fifth's registration happened to put
  a field two lines past the window — invisible until an entry was shaped
  differently, which is precisely "a check passes for years while the thing
  it checks is impossible." A plan snippet called `hornvale_language::
  staple_pack()`, an accessor that was never real; the actual repair reads
  `hornvale_climate::Crop::catalog()`, and the plan's own instruction to
  "verify the real accessor name" was there and was skipped once. The wear
  funnel's first cut measured rung 3 as the *combined* cascade-plus-reduction
  effect, named the imprecision honestly in its own doc comment, and still
  had to be sent back — disclosure of an instrument's imprecision is not a
  substitute for fixing what it measures when the hypothesis under test
  depends on the distinction. And a `#[ignore]`d test whose own doc comment
  said "it comes back with the staple repair" sat un-discharged one task
  after the staple repair landed, found only because a later step happened
  to reread it. None of the four hid; each was caught by a step already in
  the plan or by a reviewer reading the prior step's own words back to it.

- **The epoch answer was reversed after G3, by reading the ratified records
  instead of the approved package.** The spec, approved at G3, named two
  epoch bumps — `name/settlement/v4` and `lexicon/root/v4` — reasoned from
  decision 0089's headline sentence. Writing the plan a day later, re-reading
  0089 and 0083 directly rather than the spec's summary of them, found a
  cheaper and better-founded answer: one bump, on the leg whose algorithm
  actually changed, because `Namer::glossed_name` draws nothing and
  `lexicon/root` had already been through this exact mistake three days
  earlier and been withdrawn. The reversal was flagged to the owner rather
  than landed quietly, but the underlying lesson stands: an approval gate
  freezes a decision, not a citation, and the citation is worth re-checking
  against the source even after the gate has closed.

- **The stage-boundary absorption cadence was missed.** The branch absorbed
  `main` once, at roughly 199 incoming commits, rather than at each plan
  stage as the standing rule asks. Nothing broke — the merge resolved
  cleanly and no semantic collision was found the hard way — but one of the
  numbers this campaign's own preregistered prediction depended on (the wear
  funnel's baseline) had been measured *before* that absorption and could not
  be compared against a readout measured after it, because the absorbed tree
  carried an unrelated reseed of its own. The baseline had to be re-measured
  on the merged tree specifically to keep it comparable to the readout — a
  forced re-measurement, not a convenience, and exactly the failure mode
  "never absorb mid-measurement" exists to prevent. It did not reach that
  failure mode this time only because the re-measurement was caught before
  publication.

## What worked

- **Checking a decision number against `origin/main`, not the local tree,
  caught a collision the local check could not see.** Decision 0094 — the
  keystone this campaign reached independently, from a different defect —
  was ratified by a different campaign one day after this one wrote its own
  version of the same principle. Both a duplicate decision record and a
  silent overwrite were live risks; neither happened, because the numbering
  step fetched `origin/main` and re-derived the next free number from it
  rather than trusting what the local worktree already had on disk.

- **A guard was mutation-verified rather than trusted on sight, twice, and
  the one time a guard was reported passing pre-fix, it was reported that
  way rather than weakened to manufacture a red.** The roster-parity guard
  was checked by injection — remove a staple, confirm the guard names it,
  restore it, confirm green — before being trusted. The rule-witness guard
  (every `RuleKind` witnessed firing) was expected to fail on the pre-fix
  tree and instead passed, because it tests a different property (an inert
  *kind*, not an inert *position*) than the one Task 7 repaired. The honest
  report is what surfaced the deeper finding underneath it — that two rule
  kinds are structurally inert for the entire currently-shipped bestiary, for
  reasons unrelated to the bug this campaign set out to fix — rather than a
  weakened assertion that would have hidden it.

- **Measuring the merge-confounded and re-measured H1 result side by side,
  and publishing both with the smaller, correctly-isolated figure as the
  headline**, rather than quietly keeping the larger cross-tree number. The
  campaign's own gates account for roughly half of the improvement a naive
  comparison would have credited them with; the other half was an unrelated
  reseed absorbed in between. Reporting the smaller number as the real one is
  the discipline decision 0016 exists to enforce, applied to the campaign's
  own instrument rather than to a rival's.

## Is a witness guard a seed hunt? (decision 0093)

Three of this campaign's guards — every `Hydro` variant, every `RuleKind`
firing, roster parity — sweep a small, fixed set of seeds looking for a real
derivation that exercises a checklist entry. Decision 0093, ratified the day
after this campaign's own guards landed, by a different campaign, holds that
"a test that sweeps seeds to find a property instance is doing the census's
job badly and the synthetic's job expensively" and prescribes a hand-built
world instead. Read carelessly, that could look like exactly what these
guards do.

It is not the same question. 0093's target sweeps seeds to *locate* an
instance and then tests an already-understood *behaviour* on it — a job a
hand-built world does at zero build cost, because the behaviour under test
does not care how the instance arrived. This campaign's guards instead assert
*reachability of the real, unmodified derivation itself* — and a hand-built
world cannot answer that question, because a hand-built input is precisely
what let all three of this campaign's bugs ship unnoticed. A synthetic
`MaterialBuffer` can encode a porosity/carbonate pairing the real pipeline
never produces; that is not a cheaper way to ask "can the real pipeline
produce this," it is a different question with a different answer. Where the
real generator genuinely cannot supply a witness — no shipped species can
host tonogenesis or a vowel shift — the rule-firing guard falls back to
exactly 0093's own prescription: one hand-built probe species, built once,
never placed in a world, standing in for a case the real bestiary cannot
currently produce. That the same guard needed both a real-generator sweep
and a synthetic probe, for two different checklist entries in the same test,
is itself evidence the two are genuinely different instruments rather than
one dressed as the other.

One real tension is left unresolved rather than papered over: the campaign's
own opening evidence is that the census *already* answers a reachability
question at zero incremental cost (`aquifer-fraction` was a dead column
across 1000 seeds, visible without running anything). If reachability is
answerable for free once a census exists, an 8-seed, production-resolution
sweep running inside every `make gate` is a more expensive way to ask a
question the census could answer as a side effect of work already being
funded — at the cost of only catching a regression once a campaign, at
census-regen time, rather than on every commit. Neither decision resolves
that trade, and this campaign does not resolve it either; it is recorded as
**F19** below rather than decided unilaterally.

## Confidence Gradient

**No bet moved.** The campaign's territory was grepped directly against
`book/src/open-questions.md` — `aquifer`, `porosity`, `spring`, `hydro`,
`tonogenesis`, `cascade`, `lexicon`, `toponym`, `opacif` — and every hit was
either unrelated (two "cascade" hits describe a conflict-collapse mechanism,
not sound change) or already scored by a prior campaign (the toponymy-quality
bet The Wearing re-scored, which this campaign's own subject does not touch
further). Checked, no re-score owed.

## Follow-ups

Carried from `.superpowers/sdd/followups.md` (F14–F18, promoted verbatim
before the worktree that held them dies), plus this retrospective's own F19 and F20,
and this campaign's discharge of The Wearing's F5, F7 and F13:

| | |
|---|---|
| **F5** | **DISCHARGED.** `Hydro::Spring`/`Aquifer` were unreachable because `porosity > 0.5` was a carbonate-scale threshold sitting inside the `Karst` branch that pre-empted it, not because the formula lacked terms (the first hypothesis, falsified by a blocked sweep). Repaired with a grain term for dynamic range, a clastic-scale threshold placed mid-band (`0.46`, aquifer 16.4% of land), and `Spring` re-derived as a descending contact at the provider rather than a pointwise drainage gate that was never reachable anyway (land drainage maxes at 219 against the old 500 threshold, now deleted). |
| **F7** | **DISCHARGED.** A `Tonogenesis` drawn before any merger is provably the identity (`evolve` opens with no pending conditioning); the wear cascade's 1–2 rule budget could spend its entire content on exactly that draw. Repaired by making `draw_rule` position-aware, in the shared draw path so both the historical and wear cascades are correct alike. A second finding widened the fix: every shipped species is atonal and no shipped species' vowel space clears the threshold `VowelShift` needs, so both kinds were decoration regardless of position; `draw_rule` now also excludes a kind the drawing phonology cannot host. |
| **F13** | **DISCHARGED, third recurrence, now guarded.** The lab's independent exposure roster had not learned six staple-crop rules a recent campaign added, reading `exposure-sound-{goblin,kobold}` false on 767/759 of 1000 worlds — the worlds were correct throughout. Repaired, and followed by a roster-parity guard (mutation-verified) that asserts the lab considers the same concept set worldgen can classify while continuing to compute its own answer for each, closing the only failure mode this duplicate has suffered three campaigns running. |
| **F14** | Terrain has no stampable epoch label, so terrain output can move with nothing in `derived_under` to record it. *(Opened during this campaign, checking whether F5 owed an epoch — it does not, and the reason is structural: `versioned_labels()` collects only `/vN`-suffixed labels, and terrain has none.)* Whether that is a gap or a deliberate scoping of the save-format stamp to drawn rather than derived quantities is itself the open question, and it is a save-format design decision, not a repair. |
| **F15** | `Karst` should probably gate on drainage, not porosity — dissolution needs flow, and gating karst on porosity (which dissolution itself produces) is close to circular. An ideonomy pass on F5 recommended making this change alongside F5's own repair and was deliberately overturned: moving both sides of one branch in the same campaign would have made this campaign's own measurement uninterpretable. Worth its own measurement, cheap to fold into a future terrain campaign already paying for a census regen. |
| **F16** | The `spring ⊆ river` disclosure this campaign retired (F5 made `Spring` an independent reading rather than a `Karst`-as-proxy read of `river`) raises a question the deletion itself does not answer: `spring` was a near-duplicate of `river` for its entire life, so anything tuned or calibrated while that was true may have been reading `river` twice without knowing it. Worth a grep at a future close rather than an assumption. |
| **F17** | Are the terrain thresholds calibrated against the range their input actually realises, or against an imagined one? F5's real defect was a threshold set against an imagined range rather than a measured one, and that is a class, not an instance — cheaply checkable by the same sweep that diagnosed this one, applied to `classify_rock`'s porosity/silica gate, `COAL_SOIL_DEPTH_MIN`, the grain gates, `RIVER_MIN_DRAINAGE`, `OROGEN_REACH`. Two constants were checked this way already and cleared (`AQUITARD_MAX_POROSITY`, `KARST_MIN_POROSITY`), so the audit is not a foregone conclusion. Scoped out of this campaign deliberately; wants its own measurement per constant. |
| **F18** | How much of a prior campaign's headline (an isolate diverges less than a settled family, tracking sociality × lifespan) survives removing the two dead rule kinds F7 found? Before this campaign's phonology-hosting gate, the frozen isolate's tiny wear budget was disproportionately likely to spend its one draw on a rule that could never fire, manufacturing spurious conservatism that pointed the same direction as the real effect — so a result that checked only sign and margin could not tell them apart. Three of the four originally preregistered seeds still support the claim comfortably post-repair; the fourth, closest to the boundary already, does not. Live question, not a refutation; wants a wide seed sweep under the post-repair roster, which is its own measurement and its own campaign. |
| **F19** | *(New.)* This campaign's three witness guards sweep seeds inside `make gate` to prove branch/rule reachability — a question the census already answers for free, once it exists, as a side effect of work already funded (this campaign's own opening evidence is a dead census column, read at zero incremental cost). Decision 0093 prefers a synthetic instrument over a seed sweep wherever a synthetic instrument can answer the same question; for a *reachability* claim about the unmodified real derivation, it structurally cannot (see this retrospective's "Is a witness guard a seed hunt?" section) — but that argument does not by itself settle whether these three specific guards should live in `make gate` at all, versus becoming census-derived assertions read once per regen. Neither decision resolves this; open rather than decided unilaterally. |
| **F20** | *(New, at close.)* **The heavy tier's wall-clock budgets are contention-blind, and it runs them in parallel with its own heavy tests.** `scene_api_cost_is_bounded_on_seed_42` failed the heavy run at `f449ea1c` with `genesis took 13412.2 ms, over the 13000 ms ceiling` — nextest reported *19 tests still running* at that moment. Re-run alone on a quiet lefford (loadavg 0.55) the same test passes in 7.5 s with **genesis at 4257.7 ms against the 13000 ms budget** — 33% of ceiling, a 3.1x contention inflation. This is The Timekeeper's documented blind spot #1 recurring in a different harness: `make ci` grew a contention guard (it suppresses its alarm when a census claim is held), and `heavy-run.sh` has no equivalent, so a timing assertion inside the heavy tier is structurally unreliable and its red is uninformative. Candidate fixes, none chosen here: give the wall-clock budget tests `test-threads = 1`, move them out of the heavy tier into a quiet-box-only target, or teach them the same load check `make ci` needs. Until then: **distrust a heavy-tier timing failure and re-run it isolated before believing it.** |
