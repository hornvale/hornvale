# 0367. The latch is monotone — a passage opens and never re-closes

**Status:** Superseded by [0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md) (2026-08-29) · **Decider:** Nathan (autopilot; ruled at
the spec stop, and the campaign is named for it) · **Relates:**
[0366](0366-passage-state-is-derived-never-stored.md) (the fold this
constrains) ·
[The Latch](../../book/src/chronicle/the-latch.md)

In the context of restricted passage arriving before containers do, we decided
that **a cleared passage stays clear — clearing is the only transition, and
re-closing is deferred to arc IV.c**, accepting a world whose reachability
graph can only ever improve.

## Context

Re-closing is not hard to write; it is hard to write *once*. A closing act
belongs with doors, lids, and containers, because they are the same mechanism
seen from three angles, and a `passage-closed` predicate shipped now would be
designed against one of the three. The Offer's own retrospective already
files durable objects, containers, and restricted passage as arriving
together for exactly this reason.

Monotonicity also buys something concrete right now. The campaign's spec named
`RoomMeshMemo` and `HomeNavCache` as its likeliest real cost: caches over
reachability, facing a graph that has just become time-varying. Under a
monotone latch a stale cache **under-reaches** — it routes the long way round —
rather than routing a body through a barrier. That is a degradation, not a
correctness failure, and it is why no cache-invalidation task was needed. (It
turned out to be moot on a second ground as well: `delve` is not an `Action`
variant and `liveness.rs` never mentions it, so the catch-up replay never sees
this verb at all.)

## The rule

`effective_state` short-circuits: if any committed `passage-cleared` fact
names the address at or before the day asked about, it returns `Open` without
consulting `barrier_of` at all. No code path lowers a barrier's state.

The type agrees with the rule by construction: `Open` is the top of
`BarrierState`'s derived `Ord` (`Sealed < Warded < Thin < Open`), so the fold
can never produce a state below what the seed drew.

## Consequences

- **This is a deferral with a name, not an omission.** A future campaign
  adding `passage-closed` must decide what a *pair* of open/close facts folds
  to — almost certainly ledger-order, the way The Coercion's
  `possessed-by`/`possession-ended` pair already resolves — and that decision
  supersedes this one rather than extending it.
- **Only `Thin` yields to the clearing verb**, which is a separate choice made
  from inside the code and pinned by its own test. `Sealed` has no rubble for
  an act of clearing to move; `Warded`'s shipped refusal already tells the
  player "you cannot force it," so a verb that then forced it would contradict
  rendered prose. The monotonicity rule says a barrier never goes *down*; it
  does not say every barrier goes *up*.
- **A monotone latch cannot express a trap.** A passage that closes behind you
  is a real design object this forecloses for now, and it is worth naming
  because it is the first thing anyone will want.
