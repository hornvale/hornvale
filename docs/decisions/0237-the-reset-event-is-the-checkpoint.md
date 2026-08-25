# 0237. The reset event is the checkpoint

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Relates:**
[0236](0236-a-fold-advances-it-is-not-invalidated.md);
[The Tailrace spec](../superpowers/specs/2026-08-24-the-tailrace-design.md)
§3, §9

In the context of an incremental ledger fold whose only general answer to
"what was the state at an earlier position" is `Folded::rebuild_upto` — an
O(position) walk from the start of the ledger — we decided that **a
past-position read is served from the last reset at or before it, and that
`kernel/src/fold.rs` exposes `Folded::resume` for a tenant to do this rather
than trying to know where a tenant's checkpoints are itself.**

## The problem `rebuild_upto` alone leaves open

A fold is one-directional: it accumulates forward, and it does not go
backwards. `Folded::rebuild_upto` answers a query about an earlier position
correctly, but it does so by refolding from the ledger's start every time,
which is exactly the O(history) walk this campaign exists to remove. Left
as the only tool, a tenant with a natural checkpoint — a `drank` fact
zeroing a thirst integral, say — would still pay the full history cost on
every such query, because nothing in the primitive knows the checkpoint
exists.

## Why the primitive does not know where checkpoints are

A checkpoint is a **reset event**: a fact after which the fold's state is
known by construction, independent of anything absorbed before it. Whether
such an event exists, and which fact predicate marks it, is a property of
the tenant's semantics — thirst resets on `drank`, hunger resets on `ate`,
fatigue resets on `slept` — and none of that is visible to `kernel/src/fold.rs`,
which only ever sees `LedgerFold::absorb` being called with a `&Fact` it does
not interpret. Teaching the primitive to recognize a reset predicate would
mean either a registry of tenant-specific rules living in the kernel (a
layering violation — kernel crates depend on nothing above them) or a
generic "reset" convention imposed on every tenant, whether or not its
domain actually has one.

## The rule

A past-position read is served from **the last reset at or before it** —
found by the tenant, since only the tenant's domain semantics can name a
reset predicate — and then advanced forward from there. `Folded::resume`
is the seam that makes this cheap: it accepts a state the caller asserts is
valid at a given position and continues folding from it, so a tenant that
tracks its own checkpoints pays only the distance from the nearest one, not
the distance from the ledger's start. `rebuild_upto` remains the correct
general-purpose fallback for a tenant with no checkpoint to offer, or for
building the very checkpoint `resume` will later consume.

## What this bounds, and what it does not

This is what makes stage 3's three drive accumulators (thirst, hunger,
fatigue) genuinely bounded rather than merely incremental: each has a reset
event, so a query against any of them costs the distance since the last
reset, not the distance since world genesis. It does not, by itself, bound
a fold with no reset semantics at all — `believed_water`, `hazard_memory_memo`,
and `build_emitter_scan` (stage 4) may or may not have one, and that is a
question stage 4 answers, not this decision.
