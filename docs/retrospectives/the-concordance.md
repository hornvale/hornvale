# Retrospective — The Concordance (ECS Campaign 4: the query engine)

One page of process, not product. The product is chronicled; this is what the
close learned that the code does not record.

## What went well

- **The metaplan did its job.** Campaign 4's index shapes, interning, and the
  INDEX≡SCAN keystone were all fixed months earlier in the program metaplan and
  the pressure-test. The brainstorm's real work was the five crux items the
  metaplan left open (integration surface, lifecycle, interning placement, scope
  fold/defer, the save-format line), not re-deriving the architecture. A
  well-specified metaplan turns a campaign spec into a small number of genuine
  decisions.
- **Measuring the call surface before choosing the API.** Grepping the 62 files
  / 303 call-sites of the ledger API up front made the "extend in place vs. wrap"
  decision obvious and turned it into a zero-churn change. Cheap measurement
  retired an architecture debate.
- **A clean landing, for once.** Main never moved during execution (base and tip
  both `2eb570f`) — no absorptions, no keystone refreeze, a true fast-forward.
  Worth noting precisely because it is rare here; the parallel-session churn that
  dominated the campaign-3 close simply did not happen this time.

## What the reviews caught that the tasks did not

- **The keystone was not total until the whole-branch review.** Every per-task
  review approved the INDEX≡SCAN property test, but the test committed only
  entity-reference objects, so it never exercised numeric objects — and the one
  place the index disagreed with the scan was signed zero (`total_cmp` orders
  `-0.0 < 0.0`; the scan's IEEE equality does not). A keystone property is only
  as strong as the value-space its generator covers. **Lesson:** when a property
  test asserts "X equals Y over random inputs," audit the *generator's* coverage
  of the value space as rigorously as the assertion — an Entity-only generator
  silently exempts the numeric edge cases where two orderings are most likely to
  disagree. Fixed by adding numeric objects (including both zero spellings) to
  the generator plus a targeted regression.
- **A tautological test slipped in from the plan.** The plan's own Task-1 test
  compared a function's output to a second call of the same function, and its
  construction happened to make the index's key order coincide with commit order
  — so it would have passed even with the order-preserving sort deleted. The
  per-task review caught it. **Lesson:** plan-authored test code is a starting
  point, not evidence its assertions bite; a review that treats "the plan
  mandated it" as a downgrade would have shipped a test that verifies nothing.

## Process miss to carry forward

- **Never transcribe the live-census command into a local-run plan step.** The
  plan's artifact-freshness step was copied from CI's shape and included
  `lab run studies/the-census.study.json` — which is the ~1–2 h live 1000-seed
  census that must never run locally (CLAUDE.md). The executing agent started it,
  caught it after ~31 minutes, killed it cleanly (no partial writes), and redid
  the freshness check via `scripts/regenerate-artifacts.sh` (census-skipped, the
  actual local command). No harm done, but the plan line was a latent trap.
  **Lesson:** CI's artifact list and the local freshness command are not the same
  command; local plans use `regenerate-artifacts.sh`, and any plan step that
  names `the-census` for a *local* run is a bug. The plan was corrected at close.

## Confidence Gradient

No open-questions bet was re-scored. The query engine is infrastructure *beneath*
the "refinement at scale" bet (generating detail consistent with a large
committed ledger), not a resolution of its taste-gated half; it removes the
`O(n²)` commit obstacle the census-scale work would have hit, but that is a
substrate milestone tracked by the UNI-22 registry row, not a movement in a
generation/taste bet. Grepped the ledger/query/index/ECS terms in
`open-questions.md` before concluding this.
