# 0227. Possession selects a body; it does not mint one

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0229](0229-one-body-type.md) (what it selects *from*) ·
[0069](0069-fine-position-is-never-serialized.md) (why the merged type carries
no position) · [0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)

In the context of merging the possessed body and the derived creature into one
type, we decided that **a session is a roster of bodies plus an index —
`Session { bodies: Vec<Body>, driven: usize }` — so possession is the
selection of an existing creature and never the minting of a new entity**,
retiring `AgentId` and the `vessel/agent` stream label with it.

## Context

Possession used to mint. `agent_entity()` was `EntityId::new(self.agent.id.0)`:
a possessed body's identity came from a separate, seed-derived `AgentId` draw
taken at `Session::start`.

Meanwhile `mint_flagship` picks the most-populous settlement and
`ordered_for_derivation` **hoists the home settlement to index 0**. So the
minted body and derived creature #0 already shared a settlement, a species and
a home. They were two representations of one villager, kept apart only by
being different types — and the moment the types merged (0229) the duplicate
became visible as a twin standing where the player stands.

Selection removes the duplicate at its source rather than filtering it at
every read.

## Consequences

- **`driven` is a real field that is read, not an assumption that it is `0`.**
  `possess --seed 42` resolves to `driven = 0` because of the existing hoist,
  and possessing any creature is `driven = i` — which is precisely the Arc II
  acceptance test ("a creature on player input") needing no new mechanism.
  `driven: usize` generalises to a controller *map* in Arc III without the body
  type changing.
- **An implementation that preserved `driven == 0` by swapping the chosen body
  into slot 0 was rejected.** It is user-visible, not merely inelegant: NPC
  display handles are 1-based positions in `other_bodies`, so a swap silently
  renumbers every handle between the old and new slots depending on which
  creature you chose. `other_bodies` filters by index instead, order preserved,
  pinned by `possessing_a_creature_does_not_renumber_other_bodies_handles`.
- **"Possessed" is not a field on `Body`.** It is a fact about the session.
  Putting it on the body would recouple the driver to the thing driven —
  exactly what decision 0168 separates.
- **The roster shrinks by one entity rather than growing.** A session holds
  `k` derived creatures, one of which is driven, where it used to hold `k` plus
  a separately-minted agent.
- **`AgentId` and the `vessel/agent` stream label are retired.** A stream label
  is a permanent contract, so this is a deliberate save-format act, recorded
  rather than incidental. It is narrower than an epoch: `mint_flagship` was
  called only from `Session::start` and drew from a fresh, position-keyed
  stream taken *after* genesis, so no seed generates a different world. Genesis
  byte-identity was verified rather than assumed — 12,534 facts, identical
  either side.
- **The cost this bought, discovered by removing the duplicate:** the twin was
  the *only* thing guaranteeing a creature co-located with a fresh possession.
  `derive_npcs` derives one creature per settlement, and the player now *is*
  that creature. A chamber-creature seed search that hit ~19 of 24 seeds before
  the merge hits 0 of 64 after it. Twenty-one tests had been passing on an
  artifact of the duplication rather than on world behaviour. Ruled by Nathan
  at the time: add a documented test seam so those tests state their
  co-location fixture, and do **not** change world population inside a
  refactor — a settlement of eighty people containing one simulated creature is
  a design question owed its own campaign and its own measurement, since The
  Penstock measured tick cost superlinear in agent count (1.43 fitted, 2.17
  across 100→200 agents).
