# 0409. Fog is a seen-bitset per rung, not a re-folded trail

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0408](0408-visibility-is-a-state-never-a-shade.md) (the states
this bitset backs) · [The Gallery](../../book/src/chronicle/the-gallery.md)

In the context of choosing what fog-of-war stores, we decided that **each
rung carries one bitset of seen cells (one bit per cell of its extent),
OR'd with the shadowcast at every arrival**, accepting session-only
lifetime (fog dies at `climb`) over a design that stored a trail of
`(cell, reach)` pairs and re-derived the seen-set from it.

## Context

The draft design stored a trail and folded the seen-set out of it on demand,
citing `UNI-20`'s "nothing stored that re-derives." That citation is
circular: a seen-set does not re-derive from the world — no `(seed,
address)` yields it, because it is playthrough history, not world truth. It
re-derives only from the trail, which is itself stored playthrough state.
The draft traded one stored object for a larger, slower one and called the
result a derivation.

The draft's second argument — that storing a set "bakes the reach in at
write time" — is true, and is the *correct* semantics, not a bug: a lantern
acquired later must not retroactively illuminate chambers already walked
past in the dark, and a bitset cannot exhibit that hazard by construction.

## The rule

**Every arrival** at a cell — `delve`'s placement, a lateral step, a stairs
landing — ORs its shadowcast into the rung's bitset. "Arrival", not "step":
marking on lateral steps alone would leave the chamber you descend into, and
any rung you enter and immediately leave by stairs, unrecorded. A whole
descent (five rungs, largest 60×34) costs ~1.2 KB.

## Consequences

- **Monotone by construction**, not by a test that could fail: bits are only
  ever set, and the type has no clear/unset/remove operation at all.
- **Correct across a change in reach for free**: a bit is set with whatever
  reach was in force at that moment; acquiring a lantern later sets more
  bits without touching earlier ones.
- **Retracing ("have I walked this way before, in what order") is a
  different question this does not answer.** If wanted later, a trail is
  added *beside* the bitset, not instead of it — the two answer different
  questions and neither derives the other.
- **Fog's lifetime is session-only, named rather than claimed**: persisting
  ~1.2 KB later is a serialization decision, not a redesign, which is a
  direct consequence of choosing the smaller representation now.
