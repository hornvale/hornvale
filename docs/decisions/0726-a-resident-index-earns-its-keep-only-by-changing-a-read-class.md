# 0726. A resident index earns its keep only by changing a read class

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Kerf · **Relates:**
[0538](0538-the-trail-is-a-resident-index-not-a-cached-hub.md),
[0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md),
[0541](0541-a-campaign-time-hash-constant-witness-retires-at-close.md)

In the context of three resident indexes over the same `agent-at` predicate —
`Trail`, `LatestVisit`, and `KnownWater` — we decided that **a resident index
earns its state only when some read it serves is asymptotically cheaper on it
than on its parent index**. Equal-class convenience is not a reason to keep a
second resident tenant.

## The comparison

`Trail` earns its place over the ledger: `prefix_len` is `O(log h)` rather
than `O(h)`. `LatestVisit` earns its place over `Trail`: per-room
`latest_at` is `O(r log v)` rather than `O(h)`. `KnownWater` earned nothing
over `LatestVisit`: its only read, `water_at`, is `O(r)` on both, because the
first visit is `days[0]` in the already-sorted visit list.

The Kerf therefore removes `KnownWater`. At the 50-agent, 200-tick band-10
instrument it removes 4,665 held entries and the estimate 247,245 held bytes;
the remaining Trail and LatestVisit rows are byte-for-byte unchanged. It also
removes one tenant's `advance` from every resident-store read.

## Consequence

This is a criterion, not a claim that every useful view must be resident.
An ephemeral projection can still be the right interface. A future resident
tenant over an existing tenant must name the read whose asymptotic class it
changes; otherwise it is deleted or left as a read.

**See also.** [The Kerf chronicle](../../book/src/chronicle/the-kerf.md);
[The Kerf spec](../superpowers/specs/2026-09-04-the-kerf-design.md) §1.2,
§11; [ledger](../superpowers/ledgers/2026-09-04-the-kerf.md) #1–#3.
