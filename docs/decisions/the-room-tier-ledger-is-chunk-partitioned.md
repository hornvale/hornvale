# The room-tier ledger is chunk-partitioned

**Status:** Proposed (2026-07-11) · **Decider:** Nathan

In the context of room-scale generation (the P2 subdivision,
`docs/design/room-scale/`) turning one ~240 km cell into up to ~10⁹ addressable
rooms, facing the fact that today a `World` is a single ledger serialized to one
JSON and that planetary-scale play would accrete far more consequences than a
load-everything blob can bear — while the save format is a contract the
constitution treats as catastrophic to change late — we propose that **room-tier
consequences are stored in a ledger partitioned by address-prefix chunk**,
separate from the small canonical planetary ledger, with the partition-key prefix
length a frozen contract set before first release.

**Context.** A room is a pure function of its global address, so nothing about
rooms needs persisting *except* deltas (Facts about what happened). Those deltas
are sparse — they exist only where play has touched — so an untouched world stays
"just a seed." Partitioning by **address prefix** (not a second lat/long grid,
which would re-import the seam-mismatch trap P2 exists to avoid) aligns chunks
with the subdivision hierarchy: room → chunk is prefix truncation, and chunk
adjacency is room adjacency at a shorter prefix. The planetary tier
(astronomy/terrain/climate/settlements — today's ~230 KB ledger) stays
monolithic; only the room tier partitions.

**Consequence.** "A world is a seed plus a ledger" widens to "a seed plus a
*spatially partitioned* ledger" — not a break: the planetary tier is unchanged and
pristine chunks cost nothing. The **partition-key prefix length becomes a
permanent save-format contract** (like the seed labels, decision 0006): changing
it re-buckets every delta, so it is chosen once, deliberately, and versioned by
epoch suffix if ever changed. Keeping the *logical* partition (a rebuildable
index) separate from the *physical* on-disk layout preserves freedom to evolve
file formats without touching the contract. v1 may serialize monolithically so
long as the ledger API is partition-ready — the contract is reserved before it is
relied upon.

**Open.** The exact prefix length `k` (chunk granularity) is unset — it trades
index fan-out against per-chunk delta volume and is fixed when the locale window's
room-depth range is chosen. This record reserves the decision; it does not yet set
`k`. It is `Proposed` and gated on the P2 subdivision work being adopted.

**See also.** `docs/design/room-scale/p2-subdivision-design.md` §10 (the design);
decision 0007 (the seed is a world's identity); decision 0006 (seed labels are
permanent contracts — the model for freezing the partition key); decision
`serialized-floats-are-quantized-for-cross-platform-determinism` (the other
save-boundary discipline); the Constitution's "a world is a seed plus a ledger."
