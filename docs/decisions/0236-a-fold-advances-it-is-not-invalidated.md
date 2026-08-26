# 0236. A fold advances; it is not invalidated

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Relates:**
[0237](0237-the-reset-event-is-the-checkpoint.md);
[The Tailrace spec](../superpowers/specs/2026-08-24-the-tailrace-design.md)
§3, §9

In the context of `windows/vessel`'s drive stack folding an agent's entire
committed `agent-at` trail on every tick — a cost `kernel/src/derived.rs`'s
existing memo, `Derived` with `Validity::Ledger`, cannot remove — we decided
that **an incremental ledger fold is a distinct primitive from a memo, not a
third `Validity` policy**, because invalidation and accumulation are
different operations rather than two ways of doing the same one.

## Why `Derived` is the wrong shape here

`Derived<Validity::Ledger>` watches a `DepKey` and evicts its cached value
once a fact touching that key commits after the entry's recorded position.
That is correct for a value read far more often than its dependency
changes: most reads hit a valid entry, and a miss pays one recomputation.

The drive stack's dependency is touched on **every** tick — a fold over
`(agent, agent-at)` watches exactly the fact the tick commits each time the
agent moves. Under `Validity::Ledger` semantics the entry would be stale
every tick, so every read would be a miss, and the "recomputation" a miss
triggers is the whole-history walk the cache existed to avoid. A cache with
a structurally guaranteed 100% miss rate is not a degraded cache; it is not
a cache at all, and no tuning of `Derived` fixes it, because the defect is
in what operation the type performs, not in a parameter of it.

## The rule

A dependency that changes on every read needs a value that **advances**
with each change instead of being **invalidated** by it. `Derived` answers
"is my cached answer still good?"; the incremental fold answers "given one
more fact, what does my answer become?" Those are different contracts, so
`kernel/src/fold.rs`'s `LedgerFold` trait and `Folded<S>` holder are a
sibling module to `derived.rs`, not a variant added to it — there is
nowhere in `Derived` to put an update function, and adding one would change
what the type is rather than extend it.

## What follows

Any future dependency touched every tick — not just the three drives this
campaign's stage 2 targets — should reach for `LedgerFold`/`Folded`, not for
a new `Validity` variant. A memo is still the right instrument for a value
whose dependency changes rarely relative to how often it is read; this
decision narrows when to reach for one, it does not retire `Derived`.
