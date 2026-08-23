# 0168. The effect of an act belongs to the body, not the driver

**Status:** Accepted (2026-08-22) · **Decider:** Nathan · **Relates:**
[0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)
(the same principle stated as a requirement on *types*, and not yet met) ·
[0069](0069-fine-position-is-never-serialized.md) (which effects a body may
commit at all)

In the context of a possessed body performing acts a creature also performs,
we decided that **an act's cost and its committed consequence are functions of
the body performing it and of nothing else — never of what chose the act**,
because a world in which a player's walk is cheaper, faster, or differently
recorded than a creature's walk is a world with two physics.

## Context

This is the keystone the arc was built to make true in code, and it was
already half-built before the arc began: `clock::cost_ticks(action, mass_kg,
terrain_factor)` was written keyed on the body, exhaustive by action variant,
taking **no driver parameter** — and had simply never had a player routed
through it. The arc's work was the routing, not the model.

The body's mass comes from `clock::mass_for_species`, the function The Tackle
extracted precisely so a second inline derivation could not appear. A bear
crosses a room more slowly than a person does, whoever is steering.

## Consequences

- An in-character act consults the body-state gate, charges
  `cost_ticks` against the body's own mass, and commits — the same three steps
  in the same order whether a drive or a keyboard selected it.
- **The committed fact is built by the same constructor**, not merely by the
  same shape: `liveness::agent_at_fact` was widened to `pub(crate)` and both
  producers call it. Field set, predicate and object arity are identical by
  construction rather than by agreement, so they cannot drift apart.
- **No field identifies the driver**, and a test asserts the negative.
- The rule is about *effects*, not about *provenance narration*. Provenance is
  prose describing why an act happened, and a possessed body genuinely has a
  different answer to that question — see 0167, which measures how far that
  goes and records it as a gap rather than a nicety.
- The natural inverse holds too and is worth stating: an act's cost may not
  depend on the driver **in either direction**. A player's act is not
  discounted for being a player's, and not surcharged either.
