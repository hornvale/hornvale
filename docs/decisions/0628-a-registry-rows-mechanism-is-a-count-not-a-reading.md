# 0628. A registry row's mechanism is a count, not a reading

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:**
[0016](0016-studies-preregister-hypotheses.md);
[0011](0011-studies-are-data-metrics-are-code.md);
[The Pawl retrospective](../retrospectives/the-pawl.md);
[The Detent spec](../superpowers/specs/2026-09-02-the-detent-design.md) §1;
[ledger](../superpowers/ledgers/2026-09-02-the-detent.md) #1

In the context of an idea-registry row that names the mechanism behind a
measured cost, facing a row whose mechanism was **read from the code** and
would have sent the next campaign to build the wrong thing, we decided that
**before a spec names a mechanism for a measured cost, that mechanism is
counted on the shape the criterion is measured on** — and that a count of zero
needs its denominator — accepting that a campaign's first hour goes to
instrumentation rather than to design.

## Context — what the row said, and what a sixty-second count said

`TOOL-hazard-affect-cross-tick-memo` was written at The Pawl's close to carry
that campaign's one unmoved criterion forward. It said the remaining 93 ms/call
was "per tick, for every visited room × every emitter, the emitter's affect at
that room's latest-visit day is re-evaluated", and The Pawl's chronicle called
that mechanism "legible from the code rather than merely suspected". It is a
plausible reading of the code, written by someone who had just spent a campaign
in it.

Before this campaign was chosen, the fold was **counted** on the instrument the
criterion is measured on — `session_length_scaling`'s own construction, seed 42,
50 derived agents, one resident store — with a `Terrain` wrapper counting every
`hazards()` call and the store's own witness read before and after each probe:

```
seed 42, 50 agents, one hazard_memory_memo call on the max-history probe
                     tick 15   tick 30   tick 60   tick 100   tick 200
  terrain.hazards()   14,004    16,758    22,302    29,097     43,164
  alarm_replays            0         0         0          0          0
```

The named mechanism is reached **zero times**, at every band, on the shape the
criterion runs on. What the cost actually is: static terrain re-sampled per
tick, ~95% of it in the emitter scan's second pass. A cross-tick affect memo
would have moved the criterion by nothing.

The zero is only meaningful because it carries denominators — 22,302 samples
taken, roster and probe room counts non-zero, and a second seed where an
emitter *is* found in every scan and the replay count is still zero. A bare
zero beside no denominator is what put the wrong mechanism in the row in the
first place.

## The row is corrected in place, and the original is kept as history

The row is not superseded and not deleted. It is corrected **in place**, with
its original claim kept in the cell as history, because the row's job is to
tell the next reader what to build and a rejected mechanism is part of that
answer: knowing the affect memo was measured to zero is worth as much as
knowing what the cost is. This is the registry's own convention (a row is a
living pointer, not an append-only record) and it is why the correction does
not need a superseding decision of its own.

## The rule

Before a spec names a mechanism for a measured cost:

1. **Count it, on the instrument the criterion is measured on** — not on a
   nearby shape, and not by reading. Different callers reach different
   functions; The Pawl's own rule-6 inference across shapes was wrong by a
   factor of 56.
2. **Give the count a denominator.** A zero with an unasserted floor cannot be
   distinguished from an instrument that was not wired in.
3. **Where the two disagree, the count governs**, and the reading is recorded
   as what it was.

This does not make reading code worthless — the reading is what generates the
hypothesis, and here it also supplied the *shape* of the answer (something in
this fold is O(history)). It makes reading insufficient as the evidence a
committed artifact cites.

## Consequence

The campaign that followed took the row's **target** and replaced its
**mechanism**, and the criterion it could not have moved moved by three orders
of magnitude. A registry row now costs an hour of instrumentation before it can
name a cause; the alternative price, paid once already, is a campaign built on
a mechanism that is reached zero times.

This is the second consecutive campaign whose named mechanism was wrong until
counted, and both were named in prose by people who had the code in front of
them. Read that as a statement about reading code as evidence, not about either
author.

**See also.** [The Detent chronicle](../../book/src/chronicle/the-detent.md);
[The Detent retrospective](../retrospectives/the-detent.md).
