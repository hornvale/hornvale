# The Hearsay — retrospective

**Merged:** 2026-08-14 · **Program:** Myth, campaign 1 of 4

## The count: four things that could not go red, all in controller text

Every defect this campaign produced was in *my* text — spec or plan — and not
one was in implementer code. All four were **things that could not fail**, and
**not one was caught by reading**. Every one was caught by mutation: breaking
the code and watching what stayed green.

1. **A preregistered hypothesis, true by construction.** `independent_witnesses`
   returned 1 for every event on every world, so the ratio was `1/N` against a
   predicted median of 0.5. It passed G3.
2. **A test asserting on an entity never committed to its fixture.** Deleting
   the genealogy filter from `descendants_of` — which leaks cross-lineage
   holders into every claim set — left all ten tests green.
3. **A correct rule with zero coverage.** The minimum-hops rule had one
   implementing line; deleting it broke nothing.
4. **An operationalisation that scored its own motivating scenario zero.**
   Twice, in two different ways (§5, §6.3).

The transferable lesson is not "review harder." Three reviewers read these and
found nothing; the same reviewers found all four by mutating. **Adversarial
mutation is the technique that works here and careful reading is the one that
does not**, and that holds for controller text specifically, because a plan is
the one code nothing compiles.

## The estimate that was wrong in the useful direction

The campaign was scoped as "the smallest of the four" and it was — but only
after the headline was replaced. The original measurement was unfalsifiable, and
the honest cost of discovering that was one spec revision plus one deferral, not
the rewrite it looked like. What made it cheap was **tracing the metric over a
hand-built six-node instance**, which settled in seconds what three reviewed
documents had not.

## The one that generalises past this project

**A measurement that requires the mechanism a campaign has excluded is a scope
error, not a hard measurement** — and it reads as merely ambitious right up
until you trace it and find it constant. Independence needs a claim arriving by
two routes; two routes is diffusion; diffusion was this campaign's explicitly
stated first non-goal. The hypothesis was a campaign-2 hypothesis wearing
campaign-1 clothes.

One level deeper, found by two ideonomy passes after the third failure:
corroboration is **semantic** and this campaign is **structural**. It needs
accounts that could differ, and content was carried unchanged by construction.
Every topological measure was a proxy for a property with no variance. The check
that would have caught all of it, in one question: **what would have to VARY for
this number to move?** If the answer is something the design holds constant, the
metric is a proxy and will fail differently with each patch.

## Post-unblinding discipline held, and it cost something

The count of operationalisation changes on "corroboration" reached three. H4 was
left **refuted at 0.0588** rather than re-scoped; H5 stayed **refuted at 0.4632**
rather than re-run on the antichain measure that would have scored it higher.
Both refusals were deliberate: a concept that has needed three corrections has
not been measured, it has been chased. The correct measure is recorded
(`KNOW-divergence-antichain`) for campaign 2 to freeze cleanly, by someone who
has not seen these numbers.

H5's spec section also carries a disclosure that its 0.50 threshold was derived
from a seed-42 exploratory pass, so it was never an independent test on that
seed — only a prediction of generality. Writing that down cost nothing and is
the difference between a result and a rehearsal.

## Cross-campaign findings

**Registering one metric restaged another campaign's fixtures.** The known rule
is that nine studies declare `"metrics": "all"`. The real blast radius is wider:
it includes **any fixture any campaign has frozen against the registry**,
including The Gnomon's injection fixtures, which are deliberately *absent* from
`docs/generated-paths.txt` and therefore covered by no drift check and no
`make rebaseline`. Nothing would have caught it except their reader test going
red in a full gate. Both refreshes ran on lefford in one sitting.

**A parallel-campaign collision preflight cannot see.** The Gnomon and this
campaign each added metrics to one registry; the merge produced a count pin
(203→222 vs 203→204) where neither side was right alone. Resolved to 223 and
**verified against the live registry rather than by arithmetic** — the
arithmetic had a one-in-three chance of producing a number that compiled and
lied.

**I broke main and did not find it.** A hooks guide I merged restated the
declared generated-path list, which `generated_paths.rs` forbids; main went red
the moment it landed and, with no CI, stayed red invisibly. It surfaced because
a Task 6 implementer ran the full gate, hit 45 failures, and checked the 45th
against `origin/main` instead of assuming it was its own. Another session had
found and fixed it independently in the same window. **Ask implementers to
verify an unexplained failure against `origin/main` before assuming authorship**
— it cost one command and caught a repo-wide red.

**The post-merge hook I shipped missed the case it was written for.** It fired
only when a merge *touched* a generated path; absorbing The Axes changed the
artifact on both sides such that the merge result equalled our tip, so the diff
was empty and the hook was silent while the merged *code* staled the report.
The hook's own header had described this blind spot in prose. **Documenting a
hole is not closing it.** Fixed to also fire on any `.rs` change in the range,
proved in both directions — and my first negative control was wrong (a range
spanning the whole merge), which is its own small lesson about controls.

## Deferred, with homes

- **`Lineage::ancestry`'s cycle guard has no test that builds an actual cycle.**
  Defensive code for a state the committed ledger cannot produce; a hand-built
  one can. `windows/hearsay/src/lineage.rs`.
- **`derive.rs` hand-rolls the same-content/downgraded/one-more-hop
  construction** instead of calling the already-tested `Claim::inherited_by`.
  Results coincide; it is duplication, not a defect.
  `windows/hearsay/src/derive.rs`.
- **H3/H5 are heavy-battery readouts, not census columns** — deliberate, so the
  refresh stayed at one added column. If a later campaign wants them per-world,
  it registers them against a census that has caught up.

## Process notes

- The worktree was created with `git worktree add` before I read that worktrees
  are now a **recycled pool** (`make worktree-take`). One extra worktree on
  disk; teardown should use the pool's path.
- Stage-boundary absorption held: main was absorbed twice mid-campaign (The
  Gnomon, then The Axes), not once at close. Both produced real conflicts. The
  cadence earned its keep.
- `docs/timings.md` went dirty after nearly every subagent run, because the
  pre-commit hook self-records. Harmless, but it aborted a fast-forward once
  and cost a confusing gate run against a stale tree.
