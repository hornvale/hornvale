# 0547. A resident is a living person, derived on demand and never generated

**Status:** Accepted (2026-09-01, ratified at G3) · **Decider:** Nathan · **Campaign:** The Roll

In the context of giving every settlement as many inhabitants as its committed
`population`, facing the choice between minting them into the world ledger at
genesis and deriving them on demand into the session ledger, we decided to
**derive** them — as ordinary *people*, in `domains/person`'s existing
vocabulary — accepting that no world file ever names a resident and that the
almanac must call the derivation if it wants to list one.

## Context

Before this campaign a settlement of eighty held exactly one simulated
creature, minted as `Lineage { parent: village.id, role: "npc", ordinal: 0 }`.
Deriving `population` of them is a change of degree; where they *live* is a
change of kind.

Genesis minting would have been an epoch: new entities and new facts on every
world, the keystone golden moved, a census rebaseline, and metric extraction —
95% of a census's cost — paying for ~7,400 new entities on 1,000 worlds. It
also answers "who lives here" for 264 settlements the player will never visit.

## The rule

- **Identity is lineage.** `Lineage { parent: settlement, role: "npc",
  ordinal: i }` for `i in 0..population`, minted through
  `Ledger::reuse_or_mint_entity`, whose own doc names this case. **Ordinal 0
  is unchanged** — it is the same `EntityId` the one pre-campaign body had,
  and still the body possession selects (decision 0227). A saved session from
  before this campaign reloads and *gains* residents; it never loses or
  renumbers the one it had.
- **The facts land on the SESSION ledger**, never the world's. The world
  fixture, every almanac, every census column and the whole gallery stay
  byte-identical, which spec §8's M4 asserts and the campaign's own
  rebaseline confirmed.
- **A resident is a person, in the word that already exists.** `is-person`,
  `person-born` and a drawn name — `domains/person`'s registered predicates,
  the same ones a founder carries. Founders (minted at genesis, all dead by
  `history-now` on seed 42) and residents (derived on demand, alive) are one
  kind of thing at two times, and one vocabulary covers both. A second word
  for the same concept was considered and refused (ledger #3).
- **The ledger wins.** Where a `name` or a `person-born` fact already exists
  for a resident, the derivation reads it rather than committing its own.
  This is not tidiness: `person-born` was computed as `now − age`, so a second
  derivation of the same settlement on a later day would commit a *different*
  value against a functional predicate and panic — a saved world re-possessed
  on a later day is exactly that call (Task 4 ruling).
- **Names are drawn by the founder path's own namer**
  (`hornvale_language::Namer`), seeded per resident, asserted distinct within
  a settlement, with the ordinal appended on collision rather than a resident
  dropped: a resident's count is coarse truth and it cannot be unremembered.
  Wild bodies are not named; a herd's members are interchangeable.

## Consequences

- **Any window may call the derivation** — it is a pure function of (world,
  settlement) — which is what "sim first, game as lens" requires of a thing
  the player can meet. The almanac could list a village's residents tomorrow
  without a world regeneration.
- **Nothing in the world ledger knows a resident's name.** A tool that reads
  only committed world facts sees the population count and no people. That is
  the accepted cost of not paying an epoch.
- **`person-born` and `is-person` finally have a living population**, which
  `SOC-person-death-unrendered` records as the cheapest route to a reader for
  predicates no window read.
- **A new stream label**, `settlement/resident/v1`, is a save-format contract
  from the day it landed: name, then birth day, then the deviations, in that
  order.

## See also

- `docs/superpowers/specs/2026-09-01-the-roll-design.md` §3.1, §3.4, §3.5.
- Decisions 0051 and 0127 (identity by lineage), 0227 (possession selects a
  body), 0050 (entity-hood for genesis collections — which points the other
  way on its face and governs genesis, not this), 0025 (one owner for names).
- `book/src/chronicle/the-roll.md`.
