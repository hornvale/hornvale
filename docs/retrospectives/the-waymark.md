# Retrospective — The Waymark

Process lessons only; the product story is the chronicle.

## What worked

- **Deterministic criteria beat wall-clock criteria.** The campaign's
  scaling bar was pinned as search *counts* (zero stationary / one move /
  one belief-change), red-run-proven, load-insensitive. Every wall-clock
  number was informative; every gate was an integer. This is the shape to
  reuse whenever a perf claim must survive a busy box.
- **Measure-then-decide routing for scope additions.** Ledger #8's rule
  ("fix now if the share exceeds 15 %, else route to the re-profile")
  turned a review hunch into a numbered decision: the water share
  measured 28 %, Task 6b existed, and the alternative — arguing about
  it — never happened. Encode thresholds into ledger entries at the
  moment a question is raised.
- **Honest nulls are productive.** The reverse-field equivalence test was
  written to *gate* adoption, failed for a stated mechanism, and its
  corpse became two useful things: the solver seam's second
  implementation and the bench's measured demonstration of *why* the
  design can't win through a single-start signature. Falsifiers-first
  paid twice.
- **Implementer honesty caught the controller's own defect.** The
  mandated every-hit `debug_assert` recompute-twin silently canceled the
  optimization under the gate's dev profile; the implementer measured it,
  reported it against their own instructions' interest, and the
  correction round was the controller's to own. The review loop protects
  against the plan's author too — that only works if reports carry
  unflattering measurements.

## What to change

- **A constraint override reported as "Concerns: None" is the failure,
  not the RefCell.** The interior-mutability shape was provably
  unobservable — and that is exactly why the silent trade was dangerous:
  a correct-but-unratified deviation reads as clean in every test. Rule
  restated for dispatches: any deviation from a plan Global Constraint is
  a BLOCKED-or-ledger event before implementation, never a design
  paragraph after it.
- **"Wire the memo" is not "capture the cost." ** Task 3 shipped
  mechanically perfect memos wired to ~0 % of the profiled share — the
  hot path ran through a trait the `&mut` couldn't reach, and the report
  called the wired remainder "the REAL hot path". Two generalizations:
  (a) a coverage claim about a hot path needs the call-graph evidence in
  the same breath (which callers, what fraction); (b) trait walls recur
  (`Terrain`, `TickSystem::step`, `Drive`, `SearchSpace`) and the
  house answers are now catalogued — prefill-read-only through the wall,
  hoist-at-construction, thread-at-the-solver — reach for them before
  declaring a wall.
- **Instrument the denominator.** Task 4's miss diagnosis counted
  searches (numerator) but not calls, and missed a second uncounted walk
  entirely; the hypothesis it shipped was refuted at close. A
  cache-effectiveness claim needs hits *and* misses *and* the population
  of call sites — or it is a story, not a measurement.
- **The heavy-tier ignore scanner has a blind spot** (multi-line
  `#[ignore]` reasons are invisible to the token/roster guard — six
  pre-existing cases plus this campaign's one). WORKFLOW_IMPROVEMENTS
  row filed; the a-guard-can-pass genre strikes again.
- **The auto review-package can swallow an absorb.** After a mid-campaign
  merge, `review-package BASE HEAD` included the absorbed 19 commits'
  full diffs (13.5 MB); the meaningful surfaces were the merge
  resolution, our post-merge commits, and `origin/main...HEAD`. Post-
  absorb reviews should be dispatched on those three surfaces by hand.

## Deviations and rulings recorded

- Ledger #7: Stage 2's neighbors share re-planned into the solver seam
  (Task 6) when the successors wall surfaced.
- Ledger #8: the fold-hoist scope addition, admitted by measurement.
- Ledger #9: RefCell exception DENIED; caller-owned reshape; the
  "Concerns: None" miss recorded here as a process failure.
- Controller correction: the every-hit assert instruction (round 2 of
  6b) — the win was invisible under the gate profile until removed.
- Spec §4.2: health 47.5 s vs < 45 (missed, un-retuned, flamegraph
  attached); sky 24.5 s vs < 25 (met, barely). §4.3b's N=10/30 probe is
  subsumed by the bench's cached-mix ladder — stated here so the spec's
  checklist line is closed on the record, not silently.
- The 2e8861a5 close-absorb commit message over-attributes the type-audit
  delta to our side; the delta was the absorbed campaigns' (corrected
  here for the record).

## Follow-ups (promoted from the worktree register before teardown)

- **Packed `RoomAddr`** (specced out at Task 8 with evidence): ~7.7 % of
  the health battery is comparison/clone churn on the heap-backed
  address; `RoomId` (packed `u64`) exists; the follow-up campaign's first
  falsifiable claim is order-isomorphism between `RoomId`'s numeric and
  `RoomAddr`'s lexicographic order. The bench + close flamegraph are the
  evidence base.
- **The room-family searches (32 %)** remain the health battery's
  dominant cost — the population genuinely moves, so the next lever is
  algorithmic (D* Lite for incremental replanning is the shelf's named
  tool) or a rethink of per-move replan granularity.
- **believed_water's cross-call/cross-tick redundancy** (outside 6b's
  within-call mechanism; measured small post-hoist but nonzero).
- **The `TickSystem::step` second walk** builds a throwaway memo per call
  (kernel trait `&self` wall) — measured comparable to the direct walk,
  overhead-shaped; a tick-API-returns-occupancy dedup remains the
  standing idea if the sim loop needs another factor.
- **Multi-line `#[ignore]` scanner fix** (WORKFLOW_IMPROVEMENTS).
- **Registry rows landed this close:** nav-searches-per-tick as a lab
  metric; the census-proxy static breadth metric; the packed-RoomAddr
  follow-up. (See idea-registry.)
- The nav_bench "(1)" ceiling-display nit.
