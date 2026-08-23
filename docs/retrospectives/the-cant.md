# The Cant — retrospective

**Merged:** 2026-08-20 · **Program:** Myth, campaign 8

Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-cant.md). The Cant is a *measure*
campaign; it shipped `hornvale-sentiment` (layer 1 of evaluative beliefs) and
landed a null — the derivable axes alone produce no admiration. No world is
built, no epoch, no save-format change; the readout is a pure species-catalogue
read.

## The plan's own text was the defect source; the API survey was the fix

Not one defect this campaign originated in an implementer's code. Six
originated in the plan's *interface* prose, and a single up-front code survey
caught all six before Task 1 was dispatched:

- `KindId` was written as an enum; it is a `&'static str` newtype, and the 15
  peoples enumerate through `society_registry()`, not a `PEOPLES` array.
- The plan told the implementer to edit a "rank table and allowlist" in
  `cli/tests/architecture.rs`. There is no such table — layer is derived from
  the crate's path and the workspace glob picks up `windows/*` automatically.
  The only forced edit is regenerating the layering golden. The file is also at
  `cli/tests/suite/architecture.rs`, not the path the plan named.
- `SPECIES_MASS_KG`, `IN_GROUP_RADIUS`, `SPECIES_LABIALITY` were treated as data;
  they are fact-*predicate* strings. The data lives on `BiosphereTraits`,
  `SocietyVector`, and (in a different crate) `ArticulationVector`.
- The articulation vectors are in `hornvale-language`, not `hornvale-species` —
  a missing crate dependency the plan never named.
- **`in_group_radius` was inverted.** The plan's Task-2 weight law said "insular
  = max in_group_radius"; the field is documented "insular 0 ↔ expansive 1", so
  insular is the *minimum*. Followed literally, the weight-vector's central
  property would have been backwards and every downstream judgment wrong in a
  way no test the plan specified would have caught.

The lesson is the one this project keeps relearning, now with a clean control:
**verify the brief against the code one task ahead, immediately before
dispatch.** Three minutes of grep per task, against the tree in the state the
implementer will actually find it, caught what re-reading the plan a fifth time
never would — because a plan is the one code that never compiles, and a
plausible-but-absent identifier rides straight into the implementation.

## A cross-task interface gap surfaces only at the next task's dispatch

Task 1's `PeopleTraits` bundled `SocietyVector` (which carries `in_group_radius`)
but not `MindVector` (which carries `threat_response`). Task 2's weight law needs
`threat_response`. The gap was invisible at Task 1 — its own tests were green and
complete — and invisible in the plan, which assumed a single "psychology" the
substrate splits across two registries. It surfaced only when the controller
verified Task 2's brief against the Task-1 code and found the field absent.
Adding it broke all three full-literal `PeopleTraits {}` construction sites, as
adding a struct field always does. This is the general shape: a per-task review
cannot see a gap that spans tasks; the one-task-ahead verification is where it is
caught, and it is cheap only because it is late and narrow.

## Measure-first held under pressure: the null was reported, not rescued

The believability readout's most at-risk floor ("it likes") failed at runtime —
0 of 210 pairs reach admiration. The correct handling was structurally
tempting to get wrong: the plan said "assert all five and run → PASS", and a red
assertion cannot be committed, so the path of least resistance is to nudge a
constant until the floor clears. It was not taken. The implementer converted only
that floor to a printed measurement, left the other four as hard assertions, and
touched no `src/` constant — confirmed at close by the diff's file list (only the
two test files changed). The null is the campaign's headline, exactly as the
spec anticipated it might be. **A falsified structural floor is a finding, not a
failure**, and the discipline that protects it is: freeze the criteria before the
code that could move them, and never retune after unblinding without saying so.

The one process wrinkle: the implementer self-adjudicated the encoding of the
null (converting the floor itself) rather than stopping for a controller
round-trip, as the brief's fallback literally instructed. The outcome matched
what the controller would have ruled, and the controller had pre-supplied the
expected phrasing, so it cost nothing here — but the general rule stands that an
implementer's job on a falsified floor is to stop and report, not to decide the
encoding.

## The null is near-boundary — name the free parameters, not just the result

"Zero admiration" reads as a hard substrate wall; the warmest pair missing by
0.048 says otherwise. The distinction is load-bearing and it is easy to lose: the
warmth baseline and the classification threshold were Task-2 *free* parameters,
so the null is as much a statement about where the neutral point was set as about
what the axes can be warm about. Reporting the closest-miss number alongside the
count is what keeps the finding honest; a bare "0/210" would have overstated it.

## Deferred (recorded here so they are not lost)

Minor findings, none merge-blocking, carried out of the scratch ledger:

- **`SIGNATURE` is positionally coupled to `Axis::ALL`** (`judgment.rs`): a
  reorder of the `Axis` enum would silently misalign every projection signature,
  where `weight_vector` matches on the axis value and is reorder-safe. Harden by
  matching, or add a const-assert. Follow-up.
- **warmth/competence are unbounded** (observed −5.4 … +3.3) but tagged
  `bare-ok(ratio)` and documented around 0/1 baselines; the tag class reads as
  `[0,1]`. Harmless (classification only compares to the threshold), but loose.
- **No end-to-end test of `catalog()`'s real predation wiring** — the direction
  test hand-sets `preys_on`. A silent regression in the `catalog()` →
  `demography::predation` wiring would redden nothing. Add a "≥1 real predation
  edge is DietPredation-asymmetric" test.
- **Mutual-predation precedence** in `diet_predation_distance`: an else-if means a
  pair that preys on each other gets only the predator-narrowing shift, violating
  the doc's "prey widens". Moot for the current roster; fix if a mutual edge is
  ever authored.

## The close itself tripped the cwd trap — again

Writing the DoD artifacts, the controller authored all six files against the
**main checkout's** absolute paths, not the campaign worktree's, because the
format-gathering reads (existing chronicles, the registry, the gradient) had
been run there. `docs_consistency` even passed — against the main checkout's
edited copy — so nothing looked wrong until `git commit -- <paths>` in the
worktree reported the files did not exist. The recovery was clean only because
the four edited files were byte-identical between the branch base and main
(verified before copying), so the edits transferred without pulling in any other
campaign's content; the two new files were pure new content. Had the bases
differed, a naive copy would have dragged another campaign's registry/gradient
changes onto the branch. The standing lesson holds and is cheap: **re-anchor cwd
to the worktree before any Write/Edit meant for the branch**, and a green
verification run in the wrong checkout is not evidence about the right one.

## What did not go wrong, worth recording

The stage-boundary absorption cadence was not exercised — the campaign ran in a
single session across three tasks with no intermediate stage gate. This is
usually a flag (a branch's first meeting with main is where semantic drift
surfaces), but here it was safe by inspection: the campaign is a pure additive
new crate, and main's advance over the interval (The Stylus, The Portolan, The
Ell's `Fact.day` epoch, The Compendium) touched neither `domains/species` nor
`domains/language`'s authored model cards — the only inputs the readout depends
on — so the readout numbers are stable against the merge product. The Ell's epoch
does not reach a crate that never mentions `Fact` or `WorldTime`.
