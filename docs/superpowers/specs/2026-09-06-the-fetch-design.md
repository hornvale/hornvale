# The Fetch — belief follows the creature

**Campaign:** The Fetch  
**Status:** G3 draft (awaiting Nathan)  
**Ledger:** `docs/superpowers/ledgers/2026-09-06-the-fetch.md`  
**Decision block:** 0866–0875

## 1. Purpose

The Surmise deliberately made remembered water nearest to `npc.home`, and
reserved nearest-to-current as a follow-up. That choice makes belief stable,
but it also makes reachability an admission test: water remembered from the
creature's current position can disappear before the planner ever sees it.
The Tidings exposed the stranded case, and The Fetch completes the reserved
follow-up.

The campaign changes the reference used by the belief fold, not the facts that
constitute memory. A creature still remembers only fresh-water rooms it has
stood in. It chooses the remembered room reachable in the fewest hops from
where it is now.

## 2. Design

### 2.1 One fold, two phases

`believed_water` keeps its existing `Option<Facet>` signature. Its remembered
candidate set is unchanged: `LatestVisit::water_at` over committed `agent-at`
history at or before `t`, intersected with current water truth. The fold then
obtains `agent_position(ledger, npc, t)` and ranks every candidate from that
position with the existing empty-hazard route semantics. An unreachable
candidate is excluded; reachable candidates are ordered by `(hop_count, Facet)`.
Empty candidates still return `None`.

The `Option<Facet>` surface remains load-bearing: `Thirst::proposal` plans to
the selected facet, affect derives known/blind labels from `is_some()`, and
the errand distinction remains stable as a shape even though its population
changes.

### 2.2 Incremental alignment

The tick's incremental fold, currently named `nearer_to_home`, becomes the
same actor-relative operation. It receives the current committed position as
the reference and applies the identical `(hops, Facet)` ordering and
unreachable handling as `believed_water`. The caller computes that position
from the frozen ledger at the same world time used for the belief read.

The private function is renamed to `nearer_to_current`; no compatibility
surface requires the old name. The key
invariant is that a belief updated while walking equals re-derivation from the
committed visit history at that instant.

### 2.3 Route ownership

`RouteMemo` is not used for the position-varying fold. Its existing key is
`(home, water, budget)` in practice; changing the caller to current position
would turn it into `(position, water, budget)` without an invalidation or
boundedness argument. The current-position fold calls the same pure
`plan_to_room(&position, &candidate, budget, &BTreeSet::new())` directly.

The implementation must expose a deterministic search-count witness in tests
or an equivalent existing instrumentation. It must not claim a wall-clock
budget. If the measured route-search count reveals a separate optimization is
needed, that is a follow-up design, not permission to smuggle a position cache
into this campaign.

## 3. Invariants and non-goals

- Memory remains a pure derived view over committed `agent-at` facts; no new
  ledger predicate, save field, RNG draw, or stream consumption is added.
- The planner continues to receive the real hazard set only at
  `plan_to_water`; the belief fold always uses an empty avoid set.
- Equal hop counts choose ascending `Facet`, in both folds.
- `shared_believed_water` keeps its current-position pooling behavior and must
  not accidentally become a second implementation of the solo fold.
- No return-type widening, vessel-type change, client ABI change, or domain
  dependency is in scope.
- Deception, stale water truth, probabilistic perception, and a rich remembered
  water map remain follow-ups.

## 4. Observable consequences

The following observers may change when their inputs contain home/current
disagreements:

1. `believed_water` fixtures for admission, nearest selection, and ties.
2. Incremental mid-walk belief tests and decision traces recording the facet.
3. Errand `water-known`/`water-blind` population and affect labels, because
   `is_some()` can change without changing remembered facts.
4. Possession and liveness transcripts whose routes depend on the selected
   facet.

Ledger-hash determinism witnesses retired to determinism-only are not value
catchers for this behavior. Byte-goldens are rebaselined only after the source
diff is reviewed on its merits.

## 5. Acceptance tests

- A remembered water room reachable from current but not home is admitted and
  selected; the inverse remains excluded when current cannot reach it.
- Two remembered reachable rooms select the current-nearest room, with the
  ascending-`Facet` tie-break.
- The incremental fold and a fresh `believed_water` fold agree after each
  committed step, including a mid-walk position change.
- The result is deterministic, per-agent isolated, and stable after
  serialize/reload.
- Existing observer fixtures are updated only when the focused behavior proves
  they move; no broad golden rewrite is accepted without a readable diff.
- The deterministic probe records route-search counts over the seed-42 and
  seed-17 possession shapes, or the repository's current equivalent fixtures,
  without using wall-clock timing.

## 6. Artifact and epoch boundary

The design predicts no new serialized schema or concept accession. Whether
transcripts or value goldens move is an implementation observation. A diff
limited to belief-dependent behavior is eligible for rebaseline after review;
ledger facts, census metrics, or unrelated world artifacts are a stop condition
requiring a new ruling.

## 7. Delivery stages

1. Add the real-shape deterministic measurement and observer inventory.
2. Add failing tests for current-position admission, ranking, and fold
   alignment.
3. Implement the smallest production change and remove home-keyed memo use from
   this path.
4. Run focused and commit gates, inspect moved fixtures, and rebaseline only
   approved value changes.
5. Submit the completed campaign through the stage/merge queue with a
   chronicle and retrospective.
