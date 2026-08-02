# Retrospective — The Vernacular (parts 1–2)

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-vernacular.md).

## The headline: nine confident, checkable, wrong claims

Across two plans, **nine claims were asserted with confidence, were checkable
by one command, were not checked, and were wrong.** Five were the controller's.

| # | Claim | Disproved by |
|---|---|---|
| 1 | Task 1's fixture ordering was "the brief's own verified ordering" | The values failed the codomain assertion the moment referents became load-bearing |
| 2 | Artifact drift was "deferred by standing campaign convention" | `git show --stat` — both prior tasks re-recorded it in their own commit |
| 3 | Spec §4: `Phenomenon.description` served a fourth "dedup key" role | One grep — the dedup is on `SkyReport`, in a test asserting prose *should* differ |
| 4 | The 77-vs-73 delta came from predicates The Watershed added | Building a worktree at the older commit and rerunning — all six predicates predated it |
| 5 | Followup #11 described a production prose-read | It is inside `#[cfg(test)]` |
| 6 | `origin/main` was fully absorbed | Local `main` was 113 commits ahead, unpushed |
| 7 | `exposure_from` draws its universe from the packs | Its body iterates `world.registry.concepts()` |
| 8 | A guard "proves the invariant directly" | Deleting the exclusion left all 588 tests green |
| 9 | Spec §3.1: 23 `Gap` uses are absorbing `Unnamed` cases | Sampling 8 found 7 correctly `Gap` |

**Two reached source comments.** #7 was written into `accession.rs` as the
justification for an epoch cohort — the misapprehension that caused a bug,
preserved on disk in the file the next engineer opens. #3's sibling landed on
`Phenomenon` itself, the type the campaign is named after. Both were caught by
reviewers, neither by the author.

### The rule worth keeping

> **A causal claim spanning two commits is verified by rerunning the
> measurement at the older commit, not by reasoning about what changed.**

That one sentence would have caught #4 and #9 outright, and #1, #2 and #7 by
the same reflex. It is cheap: build a worktree at the old commit, rerun, read.

### The rule that is new, and less obvious

> **Ledgering a correction is not propagating it.**

#7 was found by a reviewer, recorded correctly in the decision ledger — and
then the *next* task's brief was written without it. So the brief predicted the
wrong fixture would change and omitted the surface where the defect actually
shipped, which cost a fix round. A correction must be carried into every later
brief, not merely into the record. The ledger is a memory aid, not a
distribution mechanism.

## What the review machinery caught that the work did not

The subagent loop earned its cost, and specifically at scopes the work could
not see itself:

- **A defect only whole-branch scope could find.** `Void::Unnamed` did not
  survive save/load; every guard the campaign wrote built its world in-process,
  so four task reviews were structurally blind to it, and a *published* page
  shipped the wrong claim as live output.
- **A vacuous guard reported as a proof** — found by deleting the thing it
  guarded and watching nothing red.
- **A latent species-dependent divergence** no test could have caught, found by
  reading control flow rather than running anything.
- **A golden snapshotting a broken path**, not merely a superseded one: 22
  duplicate proto forms under a doc claiming to mirror a page it no longer
  mirrored.

The technique that did the work, four times over: **break it, watch it red,
revert, prove a clean tree.** Adopted as the default way to verify any guard
this campaign added.

### Reviews can be confidently wrong too

Findings #1 and #2 were *reviewers* explaining away real defects with plausible
premises. A review is not an oracle. The dispatch prompts were amended
mid-campaign to say: *if you are about to accept something because a convention,
a decision record, or a prior review says so, run the command first.* Reviews
after that amendment found more, and cited evidence rather than authority.

## What went right, structurally

- **Splitting by what moves.** Part 1 moved zero facts by construction; part 2
  likewise; part 3 (the `star-class` value change) is separated precisely
  because it moves them and owes an epoch. Every task had a crisp numeric gate,
  and "zero facts moved" was verifiable at every step rather than at the end.
- **Preregistration worked as designed.** The prediction was frozen in the spec
  before the code, and the readout was re-measured as a *matched pair* on one
  tree rather than comparing a pre-merge number to a post-merge one — which is
  how #4 was caught at all.
- **Two subagents improved on their instructions and said why.** One replaced a
  requested "hoist above the pack loop" with a final unconditional overwrite
  (strictly stronger — a hoist remains vulnerable to later inserts); one
  declined to invent a conversion that could not compile and reported the
  brief's error instead. Both were right. Dispatch prompts should keep inviting
  that rather than demanding literal compliance.

## What to do differently

1. **Write the "verify at the older commit" rule into the dispatch preamble.**
   It is currently tribal knowledge held by whoever read this page.
2. **A brief's file list is a hypothesis, not a specification.** Twice a list
   missed the surface where a defect actually shipped. Ask implementers to grep
   for siblings before finishing, as the later dispatches did.
3. **Gate wording must match what the code can do.** The plan demanded a
   seed-42 world `diff` printing `IDENTICAL` for a task that *registers
   concepts* — impossible, because `World` serializes the registry. The right
   gate is "no fact moved," and it took a task failing its own stated gate to
   notice.
4. **Guards that build their world in-process cannot see the save boundary.**
   Any invariant on registry-derived state needs a round-trip assertion. This
   campaign's Critical is the standing example.

## Absorption cadence

Main was absorbed three times mid-campaign (17, 113, and 1 commits) at
plan-stage boundaries, never mid-measurement. The 113-commit absorption moved
**zero** committed facts, which is worth recording as evidence that the
stage-boundary cadence keeps semantic drift next to its cause. One artifact —
`scene-tiles-region-seed-42.json` — was found stale *on main*, its schema having
gained two fields without a regeneration; surfaced by this campaign's rebaseline,
not caused by it.
