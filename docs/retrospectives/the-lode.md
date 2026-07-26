# The Lode — retrospective

**Completed:** 2026-07-22 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-07-22-the-lode-design.md`, plan
`docs/superpowers/plans/2026-07-22-the-lode.md`, nine tasks: FEATURES seed →
caves → ore deposits → laterite@worldgen → purity guard → features lens →
census metrics → almanac → close). Campaign 2 of the subsurface arc, straight
after The Deep. Ran under campaign-autopilot.

**Ideonomy's payoff was a unification, not a list.** Four passes on the
taxonomy converged with no overturn, but the highest-value output was the
symmetry-prompt finding that a cave (a void) and an ore vein (a concentration)
are anti-symmetric outputs of one fluid-flow substrate — which turned "caves"
and "deposits" from two independent derivations into one shared gate, and
handed us co-location for free. The passes also reframed the deposit taxonomy's
primary axis from *commodity* to *genetic process* (the same commodity via
different processes is a different deposit, at a different depth and age) and
established grade as a distribution rather than a scalar. A list of ores would
have missed all three.

**Every task-review earned its place; two caught real defects the tests
hid.** Task 2's review found that the `belt_weight` fix passed its two literal
assertions but violated the intended invariant (interior-as-floor) for ~11% of
cells at production scale — a bug that no existing test surfaced, caught by a
reviewer who built a throwaway harness at `GLOBE_LEVEL 6`. Task 3's review
found that `RockClass::Ironstone` is unreachable on land (BIF classifies only
on the ocean floor, and `deposit_at` is land-only), so *iron* — a spec-named
ore and the workhorse metal — could never appear, despite a passing unit test
that hand-built an `Ironstone` the real pipeline cannot produce on land. The
fix (iron via exhumed ancient-cratonic BIF, where the great iron ranges
actually sit) is better geology than the original. **Lesson: a unit test that
constructs an input the production pipeline can't produce is worse than no
test — it certifies a dead path.** Reviewers verifying reachability, not just
"does the function return the right thing for this input," is what caught it.

**The plan's own brief carried three defects the implementers fixed.** The
`belt_weight` formula contradicted its own test (Task 2); two type-audit tag
sets under-specified return/param positions (Tasks 2, 3). All three trace to a
plan authored before the code existed — the same class as The Deep's `count`
tag error. The type-audit tool being default-deny caught the tag gaps
mechanically; the formula/test contradiction needed a human (the implementer)
to notice the numbers disagreed. A pre-flight plan review that *ran* the
example assertions against the example code would have caught the last one.

**The census coupling compounds across an arc.** Adding census metrics makes
the fixture-schema calibration tests red until the census golden is
regenerated — a carve-out done out of band. The Deep opened that debt (3
metrics); The Lode added to it (4 more). Merging while it stands red is a
deliberate, owner-authorized choice, but the lesson is that a multi-campaign
arc touching the census should plan the regen cadence, not let the debt
accumulate silently across campaigns.

**Scope notes for the record.** No `open-questions.md` Confidence-Gradient bet
moved — The Lode's ore/cave domain isn't one of that chapter's tracked bets
(grepped ore/deposit/mining/cave/mineral); the gradient update that *was* due
is the registry flip of MAP-39/TECH-3/DOM-14. Magical ores stayed reserved
(metaphysics gate, The Ground §8), and prospecting-as-cultural-knowledge stayed
deferred (the EXP-7 seam) — the derived neutral-truth stance The Ground set.
