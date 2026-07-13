# 0037. The room-tier ledger is chunk-partitioned (only if it outgrows memory)

**Status:** Proposed (2026-07-11; re-scoped 2026-07-12 to defer to The Walk
§3.6) · **Decider:** Nathan

In the context of room-scale generation (the P2 subdivision,
`docs/design/room-scale/`) and The Walk metaplan independently designing the same
event ledger, facing the fact that **The Walk §3.6 is the authoritative near-term
model** — a hand-rolled, *in-memory* temporal triple store (`Vec<Fact>` +
`BTreeMap` indexes), bounded by the memory economy (frontier MEM-1), persisted as
today's `World { seed, registry, ledger }` JSON, with *no* database library and
*no* LSM machinery — we re-scope this record: **the near-term room-tier ledger is
The Walk's in-memory model; address-prefix chunk-partitioning is reserved only for
the deferred case where a long-running world's history outgrows RAM**, which §3.6
itself names as a future bespoke-format decision.

**Context.** The two designs converged on the same *shape* — an append-only log of
`Fact`s plus derived indexes, event-sourced, everything else re-derived — because
the constitution (coarse-constrains-fine + determinism) forces it. They differ only
on *scale and machinery*: The Walk keeps it in memory and bounds it diegetically
(the memory economy prices and forgets facts); this record's earlier framing
reached for on-disk partitioning and LSM compaction. The Walk is right for the near
term, and §3.6 already sets aside the out-of-RAM case as the exact scenario
partitioning would answer. So partitioning is not near-term architecture; it is the
*eventual* form, and this record reserves it rather than mandating it.

**Consequence.** Near-term, follow The Walk §3.6 (in-memory `Vec<Fact>` +
`BTreeMap`, MEM-1 for boundedness, single-JSON persistence) — build nothing on-disk
or LSM-shaped now. Regardless of tier, the **only frozen save-format contract is
the room address** (`RoomId`, decision 0006); no chunk size is ever a contract, and
the physical form (in-memory now, address-partitioned later) evolves freely because
it is rebuildable from the log. If and when a world outgrows RAM, the address prefix
is the natural partition key and the split/compact/snapshot policy (P2 §10) applies
— but that is a future decision to *ratify then*, with real data, not now.

**Open.** Whether the out-of-RAM case ever arrives at all (MEM-1 may keep every
world in memory indefinitely). This record stays `Proposed`, subordinate to The
Walk §3.6, and is revisited only if a long-running deployment measures a ledger that
will not fit.

**See also.** `docs/superpowers/specs/2026-07-11-the-walk-metaplan-design.md` §3.4,
§3.6 (the authoritative near-term ledger; store-irreversible-derive-reversible);
frontier MEM-1 (the memory economy that bounds it); frontier MAP-20 (the
runtime-vs-knowledge boundary, resolved by §3.4); `docs/design/room-scale/p2-subdivision-design.md`
§10 (the partitioning design, now scoped to the deferred case); decision 0007 (the
seed is a world's identity); decision 0006 (permanent contracts — the model for
what "frozen" means); decision
0033 (the Lorenz
guard-rail governs lossy snapshots).
