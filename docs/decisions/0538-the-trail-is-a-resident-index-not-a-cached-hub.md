# 0538. The trail is a resident index, not a cached hub

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0236](0236-a-fold-advances-it-is-not-invalidated.md);
[0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md);
[The Tailrace spec](../superpowers/specs/2026-08-24-the-tailrace-design.md)
§10; [The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §2.4

In the context of The Tailrace having ruled that `agent_sightings` — the
per-call rebuild of one agent's whole position timeline — must be **deleted
rather than cached**, because its output is `O(history)` however cheaply it is
kept, we decided that the resident **Trail** tenant is a different object and
is admissible: an append-only, absorbed-once, binary-searchable index of one
entity's committed sightings. The hub is still deleted; nothing rebuilds a
timeline per call.

## The distinction, stated so it cannot be read as a reversal

The Tailrace's ruling is about an object *rebuilt per call*. Caching such a
rebuild bounds nothing: the first read still pays `O(history)`, and every
consumer downstream still receives a whole timeline.

Trail is the opposite construction. Each `agent-at` fact is absorbed exactly
once, in commit order, at `O(1)`; nothing is ever recomputed; and consumers do
not receive the timeline — they receive a *range*, found by binary search on
the day. Its memory is a strict subset of the ledger's own, and it is
discardable at any instant like every other tenant.

## Why one index rather than three

Three tenants need the same ordered view at once — the sustenance integrals'
range reads, the alarm scan's positions for other roster members, and
catch-up's replay. Keeping one index and letting them share it is what
justifies holding an `O(trail)`-sized resident object at all; three private
copies of the same predicate would not be justified, and where the campaign
found one it recorded the redundancy as work still owed rather than shipping a
fourth.

## Consequence

`build_emitter_scan` no longer rebuilds every roster member's timeline inside
a per-agent call, which removes one of the two factors in that call's
`O(agents × history)` shape. The campaign makes no separate claim about roster
scaling; the readout reports what moved.

The cost accepted is a resident index whose size grows with the session's own
committed history. That is the same growth the ledger has, and the campaign
takes it knowingly in exchange for removing a per-call walk of it.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Tailrace chronicle](../../book/src/chronicle/the-tailrace.md).
