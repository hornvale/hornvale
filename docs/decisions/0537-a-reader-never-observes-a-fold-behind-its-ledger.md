# 0537. A reader never observes a fold behind its ledger

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0236](0236-a-fold-advances-it-is-not-invalidated.md);
[0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §2.2, §3
rule 3

In the context of a resident store whose tenants must be current with the
ledger at every read, facing three candidate seams — a hook on the write path,
an advance driven by the session after each commit, or an advance taken lazily
on read — we decided that **the seam is at READ**: every read of a tenant
first advances it to the ledger it is given, and `Ledger::commit` keeps the
no-hook contract [0236](0236-a-fold-advances-it-is-not-invalidated.md) gave it.

## Why read, and not commit

Advancing is idempotent in position: absorbing from position `p` to the
ledger's end costs `O(new facts)` and absorbs nothing twice, so a second read
within a turn, a re-read after a snapshot, and a second evaluation of the same
walk all cost nothing extra.

That last case is not hypothetical, and it is what rules the other two seams
out. The vessel's `wait` evaluates one walk **twice** — once for the occupancy
read and once for the facts it commits. A fold advanced per *evaluation* would
double-absorb. A fold advanced per *commit* would need the hook that was
deliberately refused. Advance-on-read is the only one of the three that is
correct under both.

## `&self` readers use interior mutability, never a throwaway

The per-turn snapshot reads through `&self` and today constructs throwaway
memos because it cannot reach the session-lived ones. **A throwaway store is
refused**: rebuilding a fold per call is the whole-history walk the store
exists to remove, on a path that runs once per committed turn for every
creature. The sanctioned mechanism is a `RefCell` around the store, advanced
on read. A `RefCell` holds nothing the ledger does not already determine, and
the project's type ban covers hashing containers and wall-clock time, not
interior mutability.

## Consequence

Every read site takes **one** borrow guard and pulls every tenant it needs
from it — nested borrows in one expression panic at run time rather than at
compile time, so the read API is shaped so that a site never needs two. One
private `advance` moves **all** tenants together and the store's position
accessor asserts they agree, so a tenant cannot silently lag while its
neighbours are current.

The cost accepted is that reading one tenant pays the others' absorb. That is
`O(new facts)` each, which is the cost the design already accepts, and it buys
a single answer to "where is this store" instead of one per tenant.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Tailrace chronicle](../../book/src/chronicle/the-tailrace.md).
