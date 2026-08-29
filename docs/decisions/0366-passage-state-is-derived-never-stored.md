# 0366. Passage state is derived from seed plus ledger, never stored

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot; the
mechanism the spec was written around) · **Relates:**
[0001](0001-determinism-is-constitutional.md) (a world is a seed plus a
ledger), [0346](0346-an-affordance-is-derived-never-committed.md) (the same
move one rung down the axis),
[0069](0069-fine-position-is-never-serialized.md) (why the address is a
chamber, not an anchor), [0126](0126-fact-day-is-a-typed-world-time.md) (the
typed day the fold filters on) ·
[The Latch](../../book/src/chronicle/the-latch.md)

In the context of a cave passage whose barrier must bar descent until an act
clears it, we decided that **the barrier's state is a pure function of the
seed and the committed ledger, and only the CHANGE is written**, accepting
that every read costs a scan of one predicate's facts rather than a field
lookup.

## Context

The obvious build is a mutable field: give the chamber a `barrier` the verb
writes. It is one line, and it is wrong in a way this project has already
paid for once. A world here is a seed plus a ledger and everything else is
re-derived (0001); a stored barrier is a second source of truth for something
the seed already answers, and the two can disagree.

The derived form falls out of machinery that already exists.
`hornvale_worldgen::barrier_of` gives any `ChamberAddr` its seeded state, and
had no production consumer before this campaign — it was built by The Deep
Realm and read only by tests. `effective_state`
(`windows/vessel/src/passage.rs`) folds the committed `passage-cleared` facts
over it: `Open` if any such fact names this address at or before the day being
asked about, otherwise whatever the seed drew.

**The address rides in the fact's OBJECT, as `Value::Text`, not as an entity
subject.** This follows `agent_at_fact`'s own precedent — places are values in
this ledger, never minted entities — so no entity is created and nothing new
is serialized. `addr_key` is the injective rendering that makes it work: no
field's textual form can contain the `/` separator, so the key splits
unambiguously back into four tokens and two distinct addresses can never share
one.

## The rule

A precondition that reads world state reads it through a **fold over
committed facts**, evaluated at the instant being asked about. It does not
read a stored flag, and it does not cache the fold's result anywhere a replay
could observe.

The `day' <= day` filter is not decoration. It is the same discipline
`last_fact_day_at_or_before` uses in the liveness walk, and it is precisely
what distinguishes this from a mutable flag: a fold over the *whole* history
would look chronologically past the instant being asked about, so a replayed
past would see doors that "had not been opened yet" standing open. A mutable
flag cannot be asked the question at all.

## Consequences

- **The subject is not consulted, and that is the design.** `Ledger::find`
  takes only a predicate, and the fold's closure touches `f.object` and
  `f.day` and never `f.subject` — so any body's clearing fact opens the
  passage for everyone. That is what makes this the axis's 90% rung (committed
  world facts) rather than a private daybook entry at 70%.
- **No epoch, no save-format contract, no golden movement.** Nothing new is
  serialized, so `cli/tests/fixtures/world-seed-42.json` did not move — checked
  rather than assumed, because that byte-golden is written only by
  `make rebaseline-goldens` and neither guarding test is in the subfloor
  roster, which is the shape that bounced The Offer from the chamber. The
  predicate is registered **per-session**, following `AGENT_AT`'s own
  precedent, which is why genesis is untouched.
- **A read costs a scan.** `effective_state` walks one predicate's facts on
  every call. That is affordable now because the predicate is nearly empty; a
  campaign that puts thousands of clearing facts in a ledger and reads them per
  turn should measure before assuming it still is. Nothing caches this today,
  deliberately — a cache over a time-varying fold is exactly where the
  replay-correctness the `<= day` filter buys would be lost again.
