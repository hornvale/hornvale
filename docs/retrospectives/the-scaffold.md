# The Scaffold — retrospective

**Completed:** 2026-08-01 (slug-named per decision 0026; spec
`docs/superpowers/specs/2026-08-01-the-scaffold-design.md`, plan
`docs/superpowers/plans/2026-08-01-the-scaffold.md`, three tasks: the
`BakeId`/`EntityId` type split → `layer_key`, the material comparator →
measurement, artifacts, and the gate). Ran under campaign-autopilot.

**A measurement with no predicted value is still worth preregistering —
it froze the *definition*, and that is what made three drift checks
possible later.** M1 had an explicit "no predicted value" (an earlier draft's
5–25% band was discarded as a guess wearing a prediction's clothes). What
stayed frozen was the comparator being measured and the two decoder paths to
measure it on — enough that Task 3 could compute the spec's own
pre-implementation numbers from a description alone, reproduce them exactly
post-implementation, and treat any divergence as evidence the built
comparator was not the specced one. A measurement without a predicted value
is not a measurement without a contract.

**"Confirmed by regenerating to scratch" is a claim about one run, not a
property of the code — and needed a second empirical check to resolve,
not a shrug.** The Task 3 brief (built from Task 2's own "M8" note)
expected `history-seed-42.md` and `vestige-seed-42.png`/`.md` to move on
`make rebaseline`; none of the three did. Rather than accept the diff (or,
worse, its absence) at face value, two throwaway probes traced each
non-move to a specific, checkable cause: the showcase site's hardcoded cell
happened to sit among the *unaffected* 280 sites, not the reordered 19; and
the vestige PNG's `most_dread` picks by a `dread` value with a
first-occurrence tie-break, which never fired across any of the 19 reordered
sites in this particular world. Both are genuine properties of *this* world
at *this* comparator, not bugs — but "the spec's verification section said
it would move" and "it moved" are different claims, and only the second was
actually checked before this campaign. Lesson: a predicted artifact move is
itself a claim that wants the same verify-before-assert discipline
(`[[PROC-20]]`) as any other generated-output claim, even when the
prediction comes with a traced code path attached — tracing the path proves
the *dependency exists*, not that it *fires* for this data.

**A throwaway probe is cheaper than it looks, and cheaper than trusting
inference.** The spec's own M1 caveat named the risk directly: the almanac's
`layers_at` fraction "should match" the worldgen path's "but that is an
inference, not a measurement." Two small test files (built, run with
`--nocapture`, then deleted before commit) turned that inference into six
confirmed numbers and, separately, into the exact reason three artifacts
didn't move. Neither probe touched anything that survives the commit —
`layers_at`/`Layer` were made `pub` only for the probe's duration and
reverted with `git checkout --` before staging, confirmed by a clean `git
status` immediately after. The pattern is reusable: when a private decoder
needs measuring from outside its crate, widen its visibility for the
lifetime of one probe rather than either skipping the measurement or
permanently exporting internals the campaign doesn't otherwise need public.

**The type split's hardest defect was found by review, not by the gate.**
Task 2's fix round shows the shape: four of six review findings landed on
`layer_key` itself (a `to_bits()` precondition violated by any negative or
NaN day nothing in the type system rules out; an overclaimed "total order"
that only holds because of an invariant one layer up, in `windows/worldgen`,
that `domains/history` was silently leaning on; a "material facts only"
doc claim that undersold the `founded_from` tie-break's real role). None of
these failed a test before review — `make gate` was green throughout. The
review discipline of *reading* the diff rather than trusting a green suite
(the same discipline `[[PROC-20]]` names) is what caught them, and each fix
tightened a doc claim to match what the code actually guarantees rather than
changing behavior.

**Scope notes for the record.** No `open-questions.md` Confidence-Gradient
bet moved — grepping identity/entity/stratigraphy/mint-order/palimpsest
found only incidental prose, never a tracked bet. Two followups promoted to
the idea registry rather than left in `.superpowers/sdd/` (git-ignored, dies
with the worktree): the `AgentId`→`EntityId` handle-confusion cousin at
`windows/vessel/src/session.rs:693`, and the type-audit day-tagging
convention gap (`bare-ok(count)` used where decision 0028 specifies
`pending`) surfaced during Task 1 review. A third deferred finding — three
bare-`f64` `assert_eq!`s in `domains/history/tests/record.rs`, against the
Global Constraints' float-comparison ban but exempted here because the
values are exact small integers with no transcendental in the path and the
file's pre-existing asserts already do the same — was judged not worth its
own registry row; fixing three in isolation would leave the file internally
inconsistent, and the honest fix is a file-wide sweep some future campaign
may or may not find worthwhile. `book/src/laboratory/generated/
the-history/summary.md` still needs the once-per-campaign census refresh
(`scripts/census-run.sh`) before it reflects `layer_key` — correctly left to
the controller rather than run from inside a task.
