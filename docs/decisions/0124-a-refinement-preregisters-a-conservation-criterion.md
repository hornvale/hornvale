# 0124. A refinement preregisters a conservation criterion, not only variation criteria

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md),
[0039](0039-epochs-replace-tiers-refine.md),
[0097](0097-assert-the-robust-half-measure-the-fragile-half.md),
[0119](0119-an-instruments-silence-means-the-claim-held.md),
[0121](0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md)

In the context of *The Grain* shipping a sub-cell refinement that passed every
hypothesis it had preregistered and was nonetheless illegal, facing the fact that
both hypotheses asked about *local variation* while the violated property was
*global conservation*, we decided that **a change presented as a refinement of a
coarser field must preregister a conservation criterion alongside its variation
criteria, and must land that criterion as a test** — accepting one more written
hypothesis and one more test per refinement as the price of the failure mode this
prevents.

## The failure, in one sentence

H1 asked whether room water varies within a canonical cell. H2 asked whether the
variation is spatially coherent rather than noise. Both were true, both were
measured, both passed — and passing them is what exposed the mechanism as
illegal, because a mechanism that varies locally and coherently while destroying
29% of a calibrated coarse quantity satisfies every local question anyone thought
to ask.

**No local hypothesis can detect a violated global invariant.** This is
structural, not an oversight in wording: a statement about how a field behaves
within a neighbourhood is silent about the sum of that field over the world. The
suite agreed — 3350 tests green over a change that broke a documented invariant
and a calibrated statistic.

## The decision

1. **A refinement's preregistration states a conservation criterion**: the coarse
   quantity the refinement must reproduce when aggregated back up. For a
   categorical field the natural form is *aggregating the fine values over one
   coarse cell reproduces that cell's own value*. For a rate or a count it is the
   coarse statistic the constant behind it was calibrated against.
2. **The criterion lands as a test, in the same change.** A conservation criterion
   that holds "by construction" is exactly the one worth writing, because its
   value is as a tripwire for the *next* mechanism, and it is cheap precisely
   because today's code satisfies it trivially. This project's own finding is that
   a documented invariant with no test is a comment.
3. **The test proves itself discriminating.** It carries a second arm that applies
   the rejected or hypothetical mechanism to the same sample and asserts the
   criterion *rejects* it. A conservation test that has never been watched fail is
   indistinguishable from one that cannot.
4. **Assert the strong form where it is available.** A plurality-conserving
   criterion still permits deleting a category from a minority of cells, which is
   the whole of the 29% loss — a thin landform is a minority everywhere it exists.
   Unanimity implies the aggregate; the aggregate does not imply unanimity.
5. **The criterion binds the mechanism, not the campaign.** It survives into the
   spec as a standing requirement on any future implementation, and a later
   campaign that supplies real sub-model detail inherits it rather than
   renegotiating it.

## Consequences

- **H5 exists as a test.** `room_water_is_conserved_when_aggregated_over_a_canonical_cell`
  (`windows/locale/src/lib.rs`) asserts that every room a canonical cell owns
  reports that cell's water kind, and separately that the reverted
  blended-threshold mechanism *violates* the criterion — measured on seed 42 over
  80 cells and 2151 rooms: the partition conserves on all of them, the reverted
  mechanism breaks the aggregate form on 11 cells and unanimity on 27.
- **The second arm earned its place immediately.** The first draft of that test
  sampled a radius-4 patch per cell and its tripwire arm found *zero* violations —
  across 1/132 of a cell the blend barely moves, so the scan could not see what it
  was built to catch. Without the arm, that draft would have shipped as a
  guard that guarded nothing.
- **This narrows [0016](0016-studies-preregister-hypotheses.md) rather than
  replacing it.** 0016 requires that hypotheses be frozen before the code that
  would move them; this record says what a *refinement's* set of hypotheses must
  contain. Nothing mechanical compares a result to a preregistration, so this is a
  discipline on the spec plus a test in the tree — the two halves 0016 already
  splits the freeze across.
- **A falsified conservation criterion is a finding, not a failure.** The
  refinement here was reverted and the reasoning shipped as the deliverable; that
  is the intended outcome, not a fallback.

## See also

`The Grain` spec §8 and its [chronicle](../../book/src/chronicle/the-grain.md);
the registry row `GRAIN-local-hypotheses-miss-global-invariants`;
[the retrospective](../retrospectives/the-grain.md).
