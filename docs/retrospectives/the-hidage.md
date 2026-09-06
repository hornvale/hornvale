# The Hidage — retrospective

*Process lessons. The chronicle carries the product story; decisions 0826 and
0827 carry the verdict and the criterion it was read off.*

The arc's first probe-only campaign since The Staple itself: one committed
measurement, one verdict, no mechanism. The rung it was opened to clear the
way for is struck.

## A metaplan lagged the code twice in the same arc, and the second time it was the correction that was stale

The Staple's own close found, during its book sweep, that settlement genesis
reads a catchment while the history bake discards it — and wrote that finding
into the metaplan as the restatement of D1: *this rung is not "add a
catchment", it is "make the two halves of the model agree"*. That correction
was itself out of date on its first clause. Since an earlier campaign the bake
has been the settlement provider outright, and the catchment code's only
caller is the laboratory's report accessor, which no build stage runs. There
are no two live halves. Both are single-vertex reads and the watershed exists
only as an instrument.

Nothing about this was hard to see; it took one grep for the callers before
any design was drafted. **The lesson is where the check goes in the order.**
A premise inherited from the arc's own governing document reads as settled,
and the more recently it was corrected the more settled it reads. Verify it
against the tree at the START of the rung, not at the close of the one that
wrote it — a document's freshness is a property of the day it was edited, not
of how carefully it was edited.

## The preregistered falsifier could not have fired, and it took a re-instantiation to see that

The brief named the death criterion: *if catchment accumulation is spatially
flat, every catchment sums alike and this is a uniform rescale in disguise.*
It is a plausible sentence, it was frozen in advance in the correct way, and
it was dead on arrival. Re-instantiating the mechanism in hydrology — the flow
algorithm is the terrain drainage algorithm with the gradient flipped, and
drainage-basin areas are heavy-tailed by construction — showed that a flatness
statistic would return "not flat" on any field the algorithm is run over.

That is the guard-that-cannot-go-red shape this repository has recorded
several times, arriving here in a new costume: not a test that cannot fail,
but a **preregistration** whose falsifier the mechanism under test cannot
produce. Preregistration is the discipline that is supposed to catch
metric-chasing, and it does nothing at all about this: the criterion was
frozen honestly, before the data, and it would have been reported as met
without ever having been at risk.

The general form, now ratified as decision 0826: **a preregistered falsifier
must be one the mechanism can actually produce, and the cheapest way to find
out is to re-instantiate the mechanism in a domain that has already studied
it.** The replacement — a count against a bar the code already carries, with
two dead poles — is what actually fired. The abandoned statistic was kept as a
characterization with its own stated prediction, and the readout then
confirmed the argument for abandoning it: heavy-tailed on all five seeds.

## Two plan-text defects, both in the controller's own code, both caught because the plan named the property

The pattern is the same one recorded a campaign earlier and it repeated
exactly. First, a task brief prescribed a hand-built test fixture — four peaks
on a small icosphere, expected to produce four basins — and it produced two,
because one peak's tail tilted its neighbours into the largest basin's
drainage. Second, a helper's closing assertion compared `Some(cur)` against
`expected.or(Some(cur))`, which is a comparison of a value with itself
whenever the flow field has no attractor to offer — precisely the
zero-capacity sites the run had just surfaced.

Neither cost more than one attempt, and the reason is worth naming twice. The
plan stated the **property** the fixture had to have rather than only the
construction, so the implementer moved the peaks and kept the assertion
untouched instead of weakening it. And the dispatch asked the reviewer, by
name, whether the assertion could pass vacuously — a question aimed at a
specific failure mode finds it; a request to "review the diff" does not.

## A defect can corrupt a statistic without moving it

The readout was re-taken after that vacuous assertion was fixed, and **not one
printed number changed** — the path-length medians and maxima were
byte-identical on every seed. The spurious zero-length entries the bug
introduced had landed among genuine zero-length entries (a settlement that is
its own attractor), so they never shifted a median or a maximum.

Which means the defect was invisible in the exact output it corrupted. Had the
response been "re-check the numbers" rather than "repair the assertion", the
check would have come back clean and the bug would have shipped. **An
assertion is repaired because it is dishonest, not because its output looks
wrong.**

## A `make worktree-take` race, and a predicate that cannot tell two states apart

This campaign's worktree was recycled out from under it four seconds after it
was taken, by another session's take. The script judges a pool member
recyclable when its branch is merged and its tree is clean — and a
**just-taken** worktree sits at `origin/main` with a clean tree, which is
indistinguishable from a finished one under that predicate. Recovered by hand
(a different pool member, the script's own steps, `git worktree repair` for
the stale registry entry) and posted to the board as a technique; the fix is
filed as a follow-up rather than made here, because it is a `scripts/` change
that should carry its own test.

The general shape: **a liveness predicate written from the finished state
cannot see the freshly-initialized one**, because initialization is
deliberately built to look finished.

## The stage gate was submitted at the plan-stage boundary, and it queued

The Cruck's retrospective recorded, as a discipline miss, that no stage gate
was submitted at any plan-stage boundary and the branch first met main at
close. This campaign submitted one at the Task 1 boundary, as the cadence
asks. The first request was **refused at the mouth** — a merge conflict on
`docs/audits/campaign-reconciliation.tsv`, where both this branch and main had
appended rows — so the box was never taken; main was absorbed locally with a
union resolution and the resubmission is what actually queued, behind a census
already running on the one serial claim and one stage gate ahead of it. The
campaign's records were written while it waited, and it reported green — all
stage phases rc=0 in 1345 s — before the close was submitted.

Stated plainly and without complaint, because the cost is the design working
as intended: a stage gate that queues costs queue position, not attention, and
a docs-only close is exactly the work that can proceed beside one. The thing
worth recording is that the wait is real and should be planned around — submit
at the boundary and pick up the next task, rather than submitting and
blocking.

## Bookkeeping

- **Two decisions minted** from the reserved block 0826–0835: 0826 (the
  criterion's form, inherited by D2–D6's probes) and 0827 (the verdict).
  0828–0835 go unspent.
- **No epoch, no census, no world state moved.** The probe commits nothing,
  draws under no label and adds no `pub` item. Two tracked files moved outside
  the records themselves: `docs/timings.md` (the campaign's gate-commit rows)
  and `docs/digest/decisions-in-force.md`, which the records commit
  regenerated because two decision records were added to it.
- **No Confidence Gradient re-score.** The gradient was grepped on the
  invariant rather than the wording (hamlet, catchment, city, the capacity
  constant) and every hit read; the nearest is the entity-size-heaviness bet,
  which this verdict corroborates without moving — a uniform rescale of a
  ceiling is not the persistent per-entity multiplier that bet is about, and
  this campaign measured no entity sizes.
- **Deferred minors:** seven rows in the campaign ledger's table, six
  accepted or deferred and one recording work already done, all written as
  they occurred rather than promoted at close. Task 0: `median`'s even-length
  branch has no test (deferred to the final review); `gini` guards
  `mean <= 0.0` where `== 0.0` is the stated case, unreachable on non-negative
  inputs (accepted). Task 1: `multi_people_attractor_sites` counts per
  `(vertex, people)` P1 entry rather than per distinct vertex, so a vertex
  hosting two peoples counts twice (accepted as consistent with every sibling
  P1 statistic, and stated in the chronicle in exactly those terms). Task 3:
  the chronicle's epigraph reads as a stronger null than was measured
  (accepted — the body is precise, an epigraph is a stance); the chronicle
  does not quote the caveat's literal MIXED wording, which decision 0827
  carries in full (accepted); the `SOC-staple-ladder` compaction to fit the
  600-character cap dropped a clause that survives elsewhere (accepted); and a
  review ruling that minors which are FALSE STATEMENTS in permanent records
  are promoted rather than deferred (fixed in fix round 1).
- **Predictions:** four preregistered, two held, two failed, none retuned. The
  failure of the size-tracks-capacity prediction is the finding the arc
  carries forward; it is reported as the headline of that section rather than
  buried under the two that held.
