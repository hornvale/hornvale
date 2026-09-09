# The Route Cache — retrospective

Process, not product. The product is the decision recorded in
[the chronicle](../../book/src/chronicle/the-route-cache.md); the measured
numbers and alternatives are in the
[campaign ledger](../superpowers/ledgers/2026-09-07-the-route-cache.md).

## Measurement changed the question

The tempting question was “what cache capacity should current-relative route
lookups use?” The probe showed that capacity was the wrong first question. The
cumulative key population rose from 14 to 649 in twelve waits, and the
longer Fetch curve had already demonstrated that a temporary plateau does not
prove saturation. We therefore rejected a session-lived cache without
inventing an eviction policy.

## Existing machinery was not evidence of reuse

`RouteMemo` is a good cache for its actual contract: a pure home-keyed
hop-count query with an explicit budget. Its existence did not justify
threading moving positions into the same map. The current-relative query also
needs directional route semantics when the consumer eventually takes a first
step, so distance symmetry cannot erase the source from the key.

## The small diagnostic was enough

The new ignored `probe:` test reused the existing seed-17 possession shape,
reported each wait's asked count and cumulative current-relative keys, and
asserted only a non-empty denominator. It did not pin a workload-dependent
endpoint or turn a measurement into a standing performance promise. The
repository's frozen ignore-reason roster caught the initial unclassified
reason; changing it to the canonical `probe:` class fixed the integration
contract without weakening the roster.

## Deferred direction

The remaining idea is a per-decision one-to-many distance field, not a cache
with a guessed lifetime. It needs a new campaign if route cost becomes
material. This campaign deliberately leaves that question open rather than
carrying an unmeasured capacity or invalidation rule forward.
