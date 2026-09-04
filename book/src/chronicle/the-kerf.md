# The Kerf

A kerf is the width a saw removes. The Kerf removed one resident index from
the water-belief path without changing a committed byte: the belief now reads
the first element of `LatestVisit`'s already-sorted per-room visit list.

The deleted `KnownWater` tenant had held one first-visit instant per water
room. `LatestVisit` already held every visit for that room in ascending order,
so its first element supplied the identical admission rule. The read remained
`O(rooms)`, its order remained `Facet` order, and `Trail` plus `LatestVisit`
remained distinct because each changes a read's asymptotic class. That rule is
[decision 0726](../../decisions/0726-a-resident-index-earns-its-keep-only-by-changing-a-read-class.md): a resident index earns its state only when a
read is asymptotically cheaper than on its parent.

At the deterministic band-10 reading (seed 42, 50 agents, 200 ticks), the
removed row was exactly 4,665 entries and an estimated 247,245 held bytes —
29.6% of the former three-index estimate. Trail stayed 6,219 / 329,607 and
LatestVisit stayed 6,219 / 259,677 in all three AFTER runs. The cold advance
probe read 60.98, 61.14, and 62.63 ns/fact. The single-agent belief probes
varied with the Mac's load, as the instrument documents; they are retained as
a measured readout, not a claim about the whole roster.

The campaign-time hash pins did their job before the cut and retired at close
under decision 0541. Their dated constants and controls remain in the test
module and ledger. What remains live is stronger in the durable direction:
each Kerf seed takes two fresh runs that must agree, each run holds its reach
floors, and the independent fold-equals-scan witnesses cover the real and
descending-order shapes. Fresh-run agreement proves determinism and reach; it
does not freeze future behaviour to a campaign-era number.

The helper shared by `rooms_at` and `water_at` is deliberately not elevated
to a repository rule. It is private, has one caller family, and no second
independent site established a reusable interface. The next instance should
derive its own boundary before a decision binds one.
