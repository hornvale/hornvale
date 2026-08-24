# 0229. One body type

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Supersedes the gap
recorded in:** [0167](0167-a-driver-is-interchangeable-and-a-possessed-body-is-a-creature.md)
(which stated this as a must-fix and deferred it) · **Relates:**
[0227](0227-possession-selects-a-body-it-does-not-mint-one.md),
[0228](0228-a-controller-is-a-parameter-of-the-tick.md),
[0069](0069-fine-position-is-never-serialized.md)

In the context of decision 0167's deferred must-fix, we decided that
**`Npc` and `Agent` collapse into one type, `Body` (`windows/vessel/src/body.rs`) —
a creature and a possessed body differ in who is driving and in nothing
else.**

## Context

0167 measured the gap and left it open. A creature was an `Npc`: entity, home,
resource, species, activity, temperature niche, deliberation latency, time
horizon, metabolic class, diet niche, boldness, threat niche, mass, label. A
possessed body was an `Agent`: id, species, perception, position, village.
**No conversion existed anywhere in the tree** — no `From`, no accessor, no
bridge — and 23 functions in the creature layer took `&Npc` or `&[Npc]`, none
of which a possessed body could be passed to.

(0167 says 21. The true count is 23; the 21 came from counting `grep` lines and
was approximately right by accident.)

You cannot swap controllers between two things that are not the same kind of
thing, so 0228 is unbuildable until this is closed.

**Every field is derivable from `(species, settlement)`**, which both
constructors already had: `derive_npcs` reads the biosphere and psyche
registries by species label and takes home/resource from the settlement, and
`mint_flagship` starts from a `VillageInfo`. So one constructor serves both and
the two entry points become arguments to it. This was verified before the
deletion rather than after: the flagship body and derived creature #0 were
asserted equal field by field, which is what licensed removing one of them.

## Consequences

- **`position` is deliberately absent from `Body`.** `Npc` had none — position
  is the latest committed `agent-at` with a `home` fallback — while `Agent`
  carried one explicitly. The merge unifies on ledger-derived, which is what
  0069 already says a persisted position *is*. `self.agent.position = …`
  disappears; committing the fact becomes the position update.
- **`village` is `Option<VillageInfo>`, not a `VillageInfo`.** The spec claimed
  every field derives from `(species, settlement)` and that was checked against
  `derive_npcs` only. `derive_wild_npcs` exists and its creatures have no
  settlement — beasts, not villagers — so a total field forces a fabricated
  `{ id: entity, name: "the wild", population: 0 }`, which is a lie twice over:
  it conflates a creature with a settlement, and it states a real-looking
  population no consumer can distinguish from a measurement. It also leaks,
  because 0227 makes any creature possessable and a possessed beast would put
  "the wild" into player-facing prose. `Option` is the honest type: a body from
  a settlement has a village and a beast does not.
- **The rename is an identifier rename and nothing else.** `Npc` derived only
  `Clone + Debug` — no `serde` — so no bytes moved. Independently reproduced in
  two detached worktrees: 12,534 facts, 3,205,503 bytes, identical sha256
  either side, and zero string literals changed across a 140 KB diff.
- **Lowercase `npc`/`npcs` field and function names were deliberately kept.**
  The type is what had to be one; the local vocabulary for "the ones you are
  not driving" is still accurate, because `other_bodies` excludes the driven
  body by construction.
- **What 0167 listed as missing is now present.** A possessed body has drives,
  has affect, appears in occupancy, and is a legal argument to every function
  that takes a body. The one thing that deliberately did *not* change is
  "who else is here": `colocated_npcs`/`sensed_npcs` exclude `driven` so the
  player's own felt state is not reported among the others'. That exclusion is
  a placeholder that says so in a comment naming its successor — a
  component-shaped exclusion served by an indexed query — and it is Penstock-
  lineage work, not this arc's.
