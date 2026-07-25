# 0070. Wounds commit; health folds — no HP counter anywhere

**Status:** Accepted (2026-07-25) · **Decider:** Nathan

In the context of Hornvale eventually needing a vitality model, facing the fact
that hit points are the datum every tutorial stores and mutates in place, we
decided that **injuries are committed facts and vitality is a fold over them** —
accepting that no stored, mutable health value may exist anywhere.

**Context.** Vitality is the one slot a roguelike is substantially *about* and
Hornvale has none, so it is where the architecture is likeliest to be violated by
reflex. The notation already determines the answer: every other derived row in
the system is a fold.

**Consequence.** Health becomes the fifth instance of one pattern — drive, belief,
affect, inventory, health — and inherits decision 0069's best property for free: a
wound is entity-keyed and *placeless*, so it survives room transitions exactly as
grievance does. What we give up is the convenience of reading or writing a single
number: every consumer folds the injury facts, and any performance problem this
creates must be solved by memoization of a monotonically stable quantity (the
pattern The Phantom established), never by caching a mutable counter.

Ordered before any combat work, deliberately: combat built first would invent a
counter.

**See also.** The Rose Window metaplan §3.3; decision
[0014](0014-fact-day-stays-bare-option.md) on ledger-shaped state; `UNI-20`
(one ledger, many derived views) and `CLIENT-vitality-folds` in the idea registry.
