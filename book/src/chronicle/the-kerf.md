# The Kerf

A kerf is the width a saw removes. The Kerf removed one resident index from
the water-belief path without changing a committed byte: the belief now reads
the first element of `LatestVisit`'s already-sorted per-room visit list.

The deleted `KnownWater` tenant had held one first-visit instant per visited
room. `LatestVisit` already held every visit for that room in ascending order,
so its first element supplied the identical admission rule. The read remained
`O(rooms)`, its order remained `Facet` order, and `Trail` plus `LatestVisit`
remained distinct because each changes a read's asymptotic class. That rule is
[decision 0756](../../decisions/0756-a-resident-index-earns-its-keep-only-by-changing-a-read-class.md): a resident index earns its state only when a
read is asymptotically cheaper than on its parent.

At the deterministic band-10 reading (seed 42, 50 agents, 200 ticks), the
removed row was exactly 4,665 entries and an estimated 247,245 held bytes —
29.6% of the former three-index estimate. Trail stayed 6,219 / 329,607 and
LatestVisit stayed 6,219 / 259,677 in all three AFTER runs. The cold advance
probe read 60.98, 61.14, and 62.63 ns/fact. The single-agent belief probes
varied with the Mac's load, as the instrument documents; they are retained as
a measured readout, not a claim about the whole roster.

The campaign-time hash pins did their job before the cut and retired at close
under decision 0541. They were minted on 2026-09-04 at merge base
`f20fdbecb`: seed 17 was `0x7394_8823_9689_ce2a` and seed 11 was
`0xd4e4_a793_ed70_6478`. Control A made `KnownWater::absorb` a no-op: it moved
seed 17 to `0xb214_b641_3e99_986e` and seed 11 to
`0xa05a_0e2d_c0bc_7748`, while leaving the seed-42 fixed script at
`0xc566_d07e_4d76_ffbd` and the seed-6 emitter's ledger and hazard digests at
`0x5d3d_7682_36e1_16c9` and `0x92dd_c47a_37de_3d9e`.

Control B flipped first-visit keeping to latest-visit keeping. It moved none
of those four script hashes, but it reddened all four real-shape
FOLD-equals-SCAN sweeps: a hash is therefore a weaker instrument than the
direct set comparison. Control C emptied the min-keeping arm; it reddened the
descending-order fixture alone while the real-shape sweeps and hash witnesses
stayed green. Together, B and C distinguish the comparison's sense from the
branch's firing rather than treating either null as proof of behaviour.

What remains live is the durable witness: each of seeds 17 and 11 takes two
fresh runs and requires their hashes to agree. Both runs must derive at least
two bodies, commit `agent-at` sightings, and make belief reads; seed 17 also
requires a belief read at a past instant, while seed 11 deliberately does not
pretend to have one. The independent fold-equals-scan witnesses still cover
the real and descending-order shapes. Fresh-run agreement proves determinism
with reach; it does not freeze future behaviour to a campaign-era number.

The helper shared by `rooms_at` and `water_at` is deliberately not elevated
to a repository rule. It is private, has one caller family, and no second
independent site established a reusable interface. The next instance should
derive its own boundary before a decision binds one.
