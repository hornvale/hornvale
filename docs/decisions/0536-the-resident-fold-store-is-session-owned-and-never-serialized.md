# 0536. The resident fold store is session-owned and never serialized

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0069](0069-fine-position-is-never-serialized.md);
[0236](0236-a-fold-advances-it-is-not-invalidated.md);
[0537](0537-a-reader-never-observes-a-fold-behind-its-ledger.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §0, §2.1

In the context of giving `kernel/src/fold.rs`'s `LedgerFold` its first
tenants — six history-proportional reads in `windows/vessel/src/liveness.rs`,
each of which would otherwise grow its own private accumulator and its own
private answer to "when does this advance" — we decided that the folds are
**tenants of one session-owned resident store, not six caches**, and that the
store holds only what the ledger already determines and is **never
serialized**.

## What the store is

`ResidentFolds` lives on `Session`, beside the navigation cache and the mesh
memo, and is equally ownable by the benches that drive the tick directly. It
is keyed by `EntityId`; a tenant is a `LedgerFold` implementation plus the key
it lives under, and the store knows nothing about what a tenant's state means.
A future tenant — arbitration hysteresis, a typed intention's anchor, a
threshold-crossing detector — is a new state type, not a new store.

This is the first concrete instance of the layer the frontier names as the
adaptive cache over derived data: **the ledger stays the only stored truth,
and the store buys time, never truth.**

## Never serialized, and why that is structural rather than a policy

The store has no `Serialize`, nothing in `World` reaches it, and a save
written mid-session contains nothing from it. Discarding it at any instant is
unobservable — pinned per tenant by the two chaos schedules the primitive
already carries (discard and rebuild at every position; at every third), and
by the whole store at a turn boundary.

That is what makes the store safe to hold at all. A resident value that could
be written would make its own eviction observable and would put a performance
mechanism upstream of world content, which is the reason [0069](0069-fine-position-is-never-serialized.md)'s
two-tier position law separates snapshot-for-speed from commit-for-truth. This
record **refines the reading** of that law — it names a third thing which is
neither a committed fact nor a snapshot, but a resident derivation — and
amends nothing in it.

## Consequence

There is no memory budget and no eviction policy, because a fold's state is
bounded by the history it folds and is a strict subset of the ledger's own
size. There is nothing to migrate in a save format, and no world file changes
by one byte. The cost accepted is that a fresh session pays one cold advance
over the committed history the first time each tenant is read — measured, and
far below what the per-call rebuild it replaces cost every tick.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Tailrace spec](../superpowers/specs/2026-08-24-the-tailrace-design.md) §7.
