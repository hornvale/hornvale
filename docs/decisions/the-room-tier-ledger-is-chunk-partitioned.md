# The room-tier ledger is chunk-partitioned

**Status:** Proposed (2026-07-11) · **Decider:** Nathan

In the context of room-scale generation (the P2 subdivision,
`docs/design/room-scale/`) turning one ~240 km cell into up to ~10⁹ addressable
rooms, facing the fact that a `World` is today a single JSON ledger that
planetary-scale play would outgrow — while the save format is a contract the
constitution treats as catastrophic to change late — we propose that **the
room-tier ledger is partitioned by room address, freezing only the address as its
logical key while its physical segmentation stays adaptive and explicitly
non-contract**, and that the near-term single-authority sim preserve three cheap
invariants that keep a future multi-writer deployment reachable without a rewrite.

**Context.** A room is a pure function of its global address, so only *deltas*
(Facts) persist, and only where play has touched — an untouched world stays "just a
seed." Freezing a fixed chunk size `k` would optimize the wrong quantity (uniform
*area*, when a ledger's cost is *weight*); mature spatial-log systems split on
density instead. So the logical key is the **full room address** — already frozen by
`RoomId` / decision 0006 — and physical grouping is an **event-sourced LSM store**
(split / compact / snapshot, chosen by *why* a segment grew), rebuildable from the
ledger, never referenced by content, and therefore free to evolve. The planetary
tier (astronomy/terrain/climate/settlements — today's ~230 KB ledger) stays
monolithic; only the room tier partitions. Concurrency is spatially local: the
chunk is the **write-authority unit** (a Domain-Driven-Design aggregate root), so
disjoint play never conflicts.

**Consequence.** "A world is a seed plus a ledger" widens to "a seed plus a
*spatially partitioned, adaptively segmented* ledger" — a widening, not a break; the
only frozen save-format surface is the room address we already own (no chunk size is
a contract). The near-term sim keeps **three invariants** so multiplayer stays
reachable: (1) model a consequence as *proposed action → validated committed fact*;
(2) keep the chunk as the aggregate / authority boundary; (3) keep determinism
absolute (it gifts cheap forking and rollback). Untrusted-client validation,
ATC-style authority handoff, cross-chunk sagas, rollback netcode, and a per-chunk
consistency spectrum are **reserved** for a multiplayer / Living-Globe future —
nothing to build now, foreclosed only if the three invariants are violated.

**Open.** The split / compact / snapshot threshold is tunable runtime policy,
measured against real play density, not fixed here. This record stays `Proposed`,
gated on the P2 subdivision work being adopted.

**See also.** `docs/design/room-scale/p2-subdivision-design.md` §10–§11 (the design);
decision 0007 (the seed is a world's identity); decision 0006 (seed labels are
permanent contracts — the model for what "frozen" means); decision
`serialized-floats-are-quantized-for-cross-platform-determinism` (the Lorenz
guard-rail governs lossy snapshots); the Constitution's "a world is a seed plus a
ledger."
