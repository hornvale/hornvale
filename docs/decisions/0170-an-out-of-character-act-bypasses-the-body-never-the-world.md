# 0170. An out-of-character act bypasses the body, never the world — and it may commit, stamped

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Relates:**
[0169](0169-mood-is-a-property-of-the-action-and-the-sigil-is-a-namespace.md)
(what makes an act out-of-character) ·
[0171](0171-a-players-acts-are-not-filtered-out-of-a-saved-world.md) (what
happens to what it commits)

In the context of an operator needing to observe and act on a world through a
body that may be asleep, dominated, or otherwise not obeying, we decided that
**an out-of-character act skips the body-state gate and nothing else** — it
obeys every rule of the world, and when it changes world-state it commits with
**operator provenance** rather than commiting silently or not at all.

## Context

The tempting simplification is that out-of-character means *read-only*. It
does not, and `provoke`/`soothe` are the precedent that settles it: they
already existed, already changed a creature's disposition, and already stamped
`"player: <verb>"` on the fact. They are operator instruments that write.

Making out-of-character read-only would have meant either deleting a shipped
capability or carving an exception around it on day one. The honest rule
admits the write and constrains it instead.

**The stamp is load-bearing, not hygienic.** It is the only thing that can
distinguish operator-imposed world-state from simulation-produced world-state
in a saved world, which is what makes such a world auditable at all. This is
the one place where the driver *is* recorded, and the asymmetry with
[0168](0168-the-effect-of-an-act-belongs-to-the-body-not-the-driver.md) is
deliberate: 0168 governs an act the body performs, where the driver is
irrelevant; this governs an act performed *on* the world past the body, where
the driver is the only interesting fact about it.

## Consequences

- The gate is consulted for in-character acts only. Session control
  (`release`, `quit`, `exit`) is exempt in both registers — a body that cannot
  be released is a hung game, not a simulated predicament.
- An out-of-character act **charges no time by default**, with `!wait` the
  named exception: observing a state you cannot act in requires a clock you
  can still advance. Without it, being asleep is indistinguishable from the
  program having hung.
- `!wait` moves the clock by the span the player named — it is not an action
  *cost*. `base_ticks` returns `Ticks(0)` for every out-of-character variant
  deliberately, and no out-of-character path may reach `cost_ticks`, whose
  `.max(1)` floor would otherwise charge a second, silent tick on top of the
  named span. This is enforced structurally (no such call site exists), not by
  the `debug_assert` that documents it.
- The gate stands in front of **acts**, not tokens. A nonsense word is not an
  act and is refused as an unknown verb in either body state; anything else
  would tell a player that a retired verb is a real one being blocked.
