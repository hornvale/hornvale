# The Wanderers Test Hardening — Decision Ledger

Campaign branch: campaign/the-wanderers

## #1 [G4] — What should change when worldgen moves a witness?

**Question.** Should the four held-gate failures be fixed by re-pinning the
new generated values, or by separating local behavioral contracts from
world-derived sample populations?

**Decision.** Keep the Wanderers production behavior and harden the tests at
their actual seams: direct unoccluded genesis observation, a constructed
detent emitter witness, and structural warrant assertions over the current
walk population.

**Why.** The failures are attributable to the campaign's legitimate stellar
topology and anchor-admission changes. Re-pinning counts or lucky seeds would
preserve symptoms while leaving the tests unable to distinguish a broken
behavior from a changed generated world.

**Deferred.** Exact generated-world population contracts remain valid only
when held by an explicit fixture or worldgen artifact test, not by unrelated
errand, rendering, or emitter reachability tests.

## #2 [G5] — What did the canonical regression reveal?

**Question.** Should the two history_emit cardinality failures be re-pinned
after the Sluice merge product changed the live occupation corpus?

**Decision.** Keep the layer_key and material-order assertions, but make the
live-corpus checks structural: every observed tie must match all material
facts and predecessor coordinates, and the material key must change at least
one live multi-layer ordering relative to the legacy key.

**Why.** The tests' own re-pin history shows that settlement and occupation
changes repeatedly move these counts without changing the key contract.
Exact tie and per-seed order counts therefore test incidental corpus shape,
not the behavior named by either witness. The corrected tests retain
anti-vacuity floors and leave the direct hand-built ordering test as the
stable mechanism witness.

**Evidence.** The canonical merge reported two failures: 3 ties instead of
the pinned 1, and live order changes [(42, 0), (7, 1), (1000, 5)] instead of
the pinned [(42, 0), (7, 1), (1000, 1)]. Both corrected focused tests pass
locally.
