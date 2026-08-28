# 0329. Subordination strategy and conjunction are drawn per tongue, not assumed

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Relates:**
[0327](0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md);
[The Mortise](../../book/src/chronicle/the-mortise.md)

In the context of realizing an embedded clause and a coordinated utterance in
a drawn tongue, not just in Common, we decided both the **subordination
strategy** (how a clause complement's boundary is marked) and the
**conjunction** (how two coordinated clauses' boundary is marked) are typed
axes `tongue_grammar` draws per people — never a hardcoded complementizer or
coordinator.

## Why hardcoding either would be wrong

`windows/worldgen` already draws constituent order, copula presence and form,
and marker depth and position per people. A hardcoded complementizer or
conjunction would make every tongue subordinate and coordinate like English —
the exact failure `realize_tongue` exists to prevent, and the same argument
0296's tense-host corollary and 0286's per-realizer ignorance already make for
other axes.

Both axes admit a genuinely zero-marker outcome and it is not degenerate:
**bare parataxis** (juxtaposition, no subordinator) and **bare juxtaposition**
(no conjunction) are both attested typologically. A tongue that draws neither
is not a tongue that cannot subordinate or coordinate — it has a different
grammar, the same reading `Evidential`'s zero member and `Tense::Present`'s
zero member already establish for this crate.

## What ships

Two new permanent stream labels — `SUBORDINATOR` (Task 5) and `CONJUNCTION`
(Task 6) — each drawn on the copula's exact pattern: one stream produces both
presence and, conditional on presence, form. 60/40 precedent (the copula) does
not transfer to either axis; both are drawn 50/50 because no literature-backed
skew was cited for either and neither strategy is degenerate — a stated choice
with a reason, not an unexamined default.

## Consequences we accept

**Two save-format contracts, additive and permanent.** Neither label is ever
renamed; a later change to either axis is a new epoch (`grammar/subordinator/
v2`), never a repoint of the existing one.

**A vocabulary word costs zero labels; a function word costs one, and the
asymmetry is real, not an oversight.** A word is a `dynamic(concept)` value on
the existing `PROTO_ROOT` axis, so the tongue's vocabulary for *know*/*think*
costs nothing new. A function word's **presence** is typological rather than
lexical, which is why the tongue that has no conjunction earns a static label
the way `COPULA` does, and a lexical addition never will.
