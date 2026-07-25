# 0071. One snapshot per commit; panes are pure projections

**Status:** Accepted (2026-07-25) · **Decider:** Nathan

In the context of the vessel client needing map, HUD and log panes, facing the
choice between per-pane queries against the port and a single structured emit, we
decided that **each committed turn emits one versioned snapshot and every pane is
a pure function of it** — accepting that the snapshot schema becomes a
save-format-class contract.

**Context.** The wasm ABI was prose-only while `Session` already exposed the
structured reads in Rust, so every pane was blocked on a producer-side emit
rather than on client work. The borrowed shape is database MVCC's snapshot
isolation.

**Consequence.** Pane incoherence — a map from turn *T* beside stats from turn
*T+1* — becomes impossible by construction rather than by discipline. A new pane
costs zero API surface, which is what makes a large API expansion survivable, and
panes unit-test with no wasm at all. The snapshot must carry **provenance per
datum** (known / sensed / felt), not merely values.

The cost accepted is that the schema joins the six `scene/*` kinds as a versioned
contract: epoch suffix, never renamed. This is `UNI-20` re-instantiated one ring
outward.

**See also.** The Rose Window metaplan §3.4; [The Snapshot
chronicle](../../book/src/chronicle/the-snapshot.md), which shipped it;
`CLIENT-one-snapshot` in the idea registry.
