# Route-cache follow-up — decision ledger

Campaign: `campaign/the-route-cache`.

## #1 [G1] — measure before introducing a moving-position cache

**Question.** Does the current-relative water fold have a bounded, reusable `(current position, remembered water)` key population that warrants a session-lived cache?

**Decision.** Do not add a persistent position-keyed route cache. The current position is part of the query's semantic input, and the observed key population is still growing at the end of the available workload. A future optimization may evaluate a per-decision one-to-many distance field, whose lifetime is naturally bounded by one decision and whose output is the nearest candidate from the actor's current position. That is a different mechanism and needs its own measurement/spec.

**Evidence.** The new ignored diagnostic `the_culvert::route_cache_probe_reports_current_key_population_curve` ran on `origin/main` after The Fetch landed:

```text
wait  asked  current_dest_cum
   1     31               14
   2    141               41
   3    224               88
   4    265               97
   5    323              144
   6    367              199
   7    393              272
   8    412              348
   9    424              373
  10    444              462
  11    464              527
  12    485              649
```

The existing Fetch diagnostic supplies the longer-shape caution: a current-relative curve previously plateaued and resumed, so a short plateau cannot establish saturation. The present run therefore increases confidence in exclusion but does not claim mathematical unboundedness.

The companion home-keyed witness on the same landed baseline reports 485 asks over 201 distinct home pairs. That comparison is useful for scale, but it does not make the home memo a valid substitute: the current-relative query has a different `from` input and its first step is directional even when a distance is symmetric.

**Alternatives discarded.**

- A session-lived `(current, destination, budget)` map: excluded because the observed working set keeps expanding and no capacity or eviction policy can be chosen without changing the measured semantics of reuse.
- Reusing `RouteMemo`: excluded because it is deliberately home-keyed and caches a pure hop-count query for a different caller boundary.
- A global cache: rejected as a larger key space with an unnecessary invalidation/epoch problem; no evidence supports that scope.

**Ideonomy.** One pass, zero overturns. Substitution, cross-domain reinstantiation, tree-finding, state-machine, and spectrum operations pointed to explicit query dependencies, deterministic eviction requirements, and the per-decision distance-field alternative. The pass changed the question from “what capacity should the cache have?” to “does a cache have a stable working set at all?”

**Capture.** This follow-up closes without production changes. If route cost becomes material, open a separate campaign for a per-decision distance field with a fresh measurement boundary; do not revive this cache design by assumption.

## Verification

```text
cargo test -p hornvale-vessel --test suite culvert_sweep_collapses_calls_onto_distinct_pairs -- --nocapture
=> 1 passed; 0 failed

cargo test -p hornvale-vessel --test suite route_cache_probe_reports_current_key_population_curve -- --ignored --nocapture
=> 1 passed; 0 failed
```
