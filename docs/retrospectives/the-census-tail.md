# The Census Tail — retrospective

Process, not product. This campaign used the timing ledger to reduce the
automated test suite's long tail while leaving the census instrument's cadence
and committed reference untouched.

## Read wall and CPU together

The recent rows separated two different problems. A test with a long wall
time but little CPU was a scheduling or serial-work candidate; a test with a
large CPU total but a smaller wall time was already parallelising and needed a
different argument. Sorting only by wall time would have sent work toward the
wrong bottleneck. The useful unit was an independent seed panel whose result
could be reconstructed in the original order.

## The safe optimization shape

`seed_sweep::map_seeds` became the common shape: scope threads over the seeds,
return one result per seed, then collect by offset. This made the performance
change explicit and kept determinism structural. The campaign repeatedly
checked focused tests and ran the commit gate; the unchanged seed order and
green byte-level witnesses were more valuable than a faster but newly tolerant
assertion.

## What did not belong in the closeout

The census is now close enough to its practical floor that another refresh
would mostly spend canonical-box time for calibration, not improve the suite's
everyday path. The remaining `resident_folds` work is incomplete, and the
`radiation_readout` conversion was not revalidated against the final absorbed
main. Both are preserved in the campaign worktree's recovery stash and should
be measured as their own follow-up. No partial optimization is part of this
candidate.

## Do differently next time

Start by grouping timing rows by execution shape, not merely ranking them. Keep
the timing ledger separate from code changes, and isolate unverified work as
soon as it appears. Finally, treat the canonical queue as part of the
measurement: a local test result can guide a patch, but only the queued stage
and merge runs answer whether the candidate survives contact with main.
