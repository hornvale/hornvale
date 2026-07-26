# The Selvage — retrospective

Process lessons, not product. Product lives in
[the chronicle](../../book/src/chronicle/the-selvage.md).

## The headline: look at the thing before designing for it

The campaign arrived with a diagnosis attached. *The Excursion* had seen the
defect, reasoned about it correctly, and named the cause in its own close:
the voxel builder never draws a wall on a tile's own outer edge. That
diagnosis was true, was written into a registry row and a chronicle, and was
the smaller half of the problem.

What overturned it was not analysis. It was running the client and taking a
screenshot **before writing any design**, then noticing that the gaps
appeared along one screen diagonal and not the other. A missing wall is
direction-agnostic; the asymmetry was the tell, and it was free. Ten minutes
of looking reframed the whole campaign.

The general form: **an inherited diagnosis is a hypothesis, not a finding**,
even when it is your own, even when it is written down, and even when it
turns out to be partly right. A prior campaign that names a defect in passing
was not trying to be exhaustive about it. Re-observe before designing.

This is the same instrument [The Turning](the-turning.md) and
[The Gyre](the-gyre.md) credit, moved earlier in the pipeline: those two used
the visual pass as the *closing* gate that catches what green tests can't.
Here it ran as the *opening* move and changed what got built. It is cheaper
there.

## Prototype the fix before speccing it, then revert

Both the diagnosis and the proposed remedy were tested in throwaway builds
during the brainstorm — one character changed to confirm the sign, then a
fifteen-line plinth to confirm the geometry closed the seams without a
hairline — each screenshotted, then reverted. The spec was written against
measured behaviour rather than predicted behaviour.

This directly served the autopilot rule about verifying claims before writing
them. The rule was written for generated-artifact and tool-behaviour claims;
it generalises to rendering claims, and rendering is where "this should look
fine" is least trustworthy. The cost was about twenty minutes; the spec's
§4.1 would otherwise have shipped an unverified rasterisation claim as the
argument for the whole approach.

## Ideonomy on the "obvious" question paid for itself

The sign question looked settled: find the inverted sign, flip it, done. The
overlay requires a pass anyway. The pass — a cross-product of *places this
codebase declares an axis convention* against *ways an axis convention can be
wrong* — found that the sign lived in **three** places, two of them
open-coded inverses of the first.

Without it, the campaign would have shipped a one-character fix that made the
picture correct and left the tile-fetching logic travelling the wrong
direction: a defect no screenshot can show, in a feature whose entire
verification story is screenshots. This is the overlay's documented
first-campaign failure mode (skipping the pass on the confident-looking
questions) not happening, and it is worth recording as evidence the rule
earns its cost.

## The plan's own self-review caught a plan-mandated defect

The first plan split the work into a pure refactor and a separate sign
correction — tidy, and it forced a placeholder expression
(`style === "voxel" ? -1 : -1`) into a committed intermediate state. That is
a construct the review rubric treats as a defect, mandated by the plan
itself, which would have put a reviewer and the plan in conflict for no gain.

The pre-execution scan caught it before any work was dispatched. Merging the
two tasks kept the behaviour-preserving checkpoint (extract with the old
sign, prove the suite is unchanged, then correct) without ever committing the
placeholder. **A staging device that only exists to make a diff readable is
not worth a defect in the history** — put it in the step order, not in a
commit.

## What the whole-branch review is for, confirmed twice running

*The Excursion*'s close recorded that its final whole-branch review found a
real bug invisible to all seven per-task reviews, because every one tested
from the same starting state, and concluded: keep that review mandatory even
when every gate is green.

It happened again. Two tasks, both approved by their own reviewers, both
correct. The final pass found three defects — all pre-existing, none a
regression — and, more usefully, **falsified a premise in this campaign's own
spec.** The argument for the plinth's sufficiency rests on the camera being
fixed; the pan clamp shears it without bound. The approach survives only
because an unrelated belt-and-braces choice (emit all four edges, not the two
visible ones) happens to cover the gap.

Two things follow. First, the per-task reviewers structurally cannot check a
spec's premises against the surrounding code — they are handed the spec as
ground truth, which is the same failure shape [The Turning](the-turning.md)
recorded when four reviewers checked a formula against a spec that was itself
wrong. Second, **a hedge taken for a hypothetical reason should be recorded
as load-bearing the moment the hypothetical turns out to be real**, or the
next person economises it away.

The three findings were also reframed by the reviewer from three symptoms
into **one item** — four places write the camera's aim point, one of them
also moves the camera — which is the same defect shape this campaign spent
its first half fixing on a different axis. A follow-up register that lists
symptoms separately invites three partial fixes; one that names the root
invites the right one.

## Smaller notes

- **Absorption cadence: not exercised.** Neither repo's main moved during the
  campaign, so the branch never needed to absorb. Recorded so it is not
  mistaken for the cadence having been skipped — there was nothing to absorb.
- **The dual-worktree convention held again** for an orrery-only campaign:
  code on the orrery branch, chronicle/retrospective/registry/mirrored
  spec-and-plan on a separate hornvale branch, merged independently.
- **Subagents did not park.** Both implementers and all three reviewers
  returned foreground evidence on the first ask. Two implementers hit the
  same heredoc parse error building a multi-line commit message and both
  recovered the same way (`git commit -F`); worth putting the `-F` form in
  the dispatch preamble rather than letting each one rediscover it.
- **A per-task reviewer upgraded a prior Minor into a strength.** Task 1's
  reviewer flagged the pan clamp's derive-bounds-from-the-forward-mapping
  form as "more indirection than a direct sign branch, but a sound
  tradeoff." The final review disagreed in the other direction: a direct
  sign branch *is* the hand-maintained fact that caused the bug. Carrying
  Minor findings forward for the final pass to re-triage is what let that
  correction happen.
