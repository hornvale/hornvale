# 0541. A campaign-time hash-constant witness retires at close

**Status:** Accepted (2026-09-02) · **Decider:** Nathan · **Relates:**
[0139](0139-main-advances-only-through-the-lock.md);
[The Pawl spec](../superpowers/specs/2026-09-01-the-pawl-design.md) §5

In the context of proving that a migration moved no committed byte — where the
generated-artifact drift check is structurally blind, because no committed
artifact carries a ticked session ledger — we decided that a **hash of the
whole walk's output, asserted against a committed constant, is a
campaign-time instrument that retires when the campaign closes**, and that
what survives is the witness without its constant.

## Why the constant is right during a migration and wrong after it

While the pre-migration code still exists, the constant is exactly the right
tool: it is the drift check's stand-in, and it can be shown to work by
mutating a fold and watching the hash move. This campaign did that on its
first task and again on the emitter-bearing world at the fifth.

After the migration there is nothing left to diverge from, and the constant's
meaning inverts. It equals *the whole walk's behaviour on one seed*, so any
campaign that legitimately changes creature behaviour reddens it — one sibling
campaign moved all three of this campaign's constants without touching a line
of its code. That is a standing tax on every other campaign, and it is an
unwinnable race besides: the queue gates `main` merged with the branch, so a
constant can be correct at submission and stale at landing.

## What a retired witness keeps

Three things, each of which is a property of the walk rather than of a
committed number:

1. **Determinism** — the fixed script is run twice on two fresh sessions of
   the same seed, and the two are required to agree. Two hashes taken from one
   session would agree because they are the same string.
2. **Every non-vacuity floor** — the counters that prove the witness reached
   the path it claims to witness, asserted on *both* runs, so a witness that
   quietly stopped witnessing cannot hide behind an agreeing hash.
3. **Fold equals scan on the real session**, against a verbatim copy of the
   pre-migration scan held in the test file.

The campaign-time constants and their positive controls are recorded as dated
history — in the module's own documentation and in the chronicle — with an
explicit statement that nothing re-checks them.

## Consequence

A constant-free witness guarantees determinism plus its floors and **nothing
more**: it cannot detect a behaviour change, by mutation or otherwise, and no
control can make it able to. That is stated where the witness lives rather
than left for a reader to infer from an absence.

A future migration of these same folds must mint its own constants against the
code it is about to replace, which is exactly what it should do.

**See also.** [The Pawl chronicle](../../book/src/chronicle/the-pawl.md);
[The Pawl retrospective](../retrospectives/the-pawl.md).
