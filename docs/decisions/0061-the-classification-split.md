# 0061. The classification split: `is-a` and `instance-of` coexist

**Status:** Accepted (2026-07-17) · **Decider:** Nathan (C2, The
Predicates & the Grammar)

In the context of the ECS Individuation campaign (Campaign 5) landing
`instance-of` as a second classification predicate alongside C1's `is-a`
(decision
0060) — and `kernel/src/world.rs` shipping both with a
comment noting "C2 decides their long-term relationship" — we decided
that **the two predicates are permanently distinct and both stay**:
`is-a` is the functional, immutable concept-class written once at
genesis, and `instance-of` is the non-functional, mutable roster-kind
that can be reassigned over sim time. Neither subsumes the other, and
The Book reads both.

**Context.** By the time C2 opened, the kernel already carried two
classification predicates with adjacent names and an unresolved
relationship:

- `is-a` (`kernel::world::IS_A`, decision
  0060): functional — an
  entity has exactly one class — contradiction-checked, and intended for
  fixed concept-classes assigned once at genesis (planet, star, moon,
  and now `world_entity`/planet-identity per this campaign's T2).
- `instance-of` (`kernel::world::INSTANCE_OF`, the Individuation
  campaign): non-functional — a kind can change over sim time (goblin →
  lich, corpse, awakened beast) — latest-wins via `Ledger::kind_of`, each
  change committed as a new day-stamped fact rather than overwriting the
  old one.

The Individuation campaign shipped `instance-of` in shadow posture
(genesis mints zero instances, so it was byte-identical and needed no
census regen) specifically so this relationship question could be
answered later, by the campaign that actually needed to render both
kinds of classification in prose. C2 is that campaign: T1–T5 wired the
Book's construction table to read `is-a` for the planet's concept-class
("Vebe is a planet") and `instance-of` for placed-people collectives
named by autonym ("The Vavako are goblins"), and both sentences had to
come out of the same Fact graph without either predicate standing in
for the other.

We considered unifying onto a single predicate — collapsing `is-a` and
`instance-of` into one classification relation and letting call sites
distinguish mutability by convention — and rejected it. The two
predicates serve different consumers under different contracts:

- `is-a`'s consumer is contradiction-checking (decision
  0010's
  value-kind enforcement) and PROC-15's coverage metric (decision
  0060), both of which depend on an entity having *exactly one*,
  *permanent* class. A functional predicate that could also be
  reassigned would silently break every downstream assumption built on
  "committed once, never contradicted."
- `instance-of`'s consumer is roster-kind reassignment — the whole
  point of the Individuation campaign was that a goblin can become a
  lich and the ledger must keep both facts, latest-wins, rather than
  either overwrite history or refuse the change. Forcing that through a
  functional predicate would mean either violating functionality
  (multiple live `is-a` facts on one subject) or discarding the history
  the campaign was built to preserve.

A single merged predicate would have had to carry both contracts
simultaneously — functional-and-contradiction-checked for some subjects,
mutable-and-latest-wins for others — which is not one relation but two
wearing one name. Keeping them separate costs one more predicate for
every reader (windows/book's construction table, `single_writer_check`'s
exemption list, PROC-15's coverage sweep) to know about, but that cost is
already paid: both predicates exist in the kernel today, both have
working consumers, and the campaigns that shipped them (C1 and
Individuation) never collided on a subject.

**Decision.**

- `is-a` (`kernel::world::IS_A`) stays the functional, immutable
  concept-class predicate: one entity, one class, written once at
  genesis, contradiction-checked. It answers "what permanent kind of
  thing is this" (planet, star, moon).
- `instance-of` (`kernel::world::INSTANCE_OF`) stays the non-functional,
  mutable roster-kind predicate: latest-wins via `Ledger::kind_of`, each
  reassignment a new fact rather than an overwrite. It answers "what is
  this entity currently rostered as" (species, settlement-kind,
  deity-kind, material) and survives a kind change that `is-a` could
  never represent.
- Both are kernel-core predicates (`KERNEL_CORE_PREDICATES`), registered
  by `World::new`, and both are legitimate inputs to The Book's
  construction table: C2 wires `is-a` for the planet's opening sentence
  and `instance-of` for placed-people collectives in the same
  aggregation pass, with neither predicate standing in for the other.
- This is the resolution the Individuation campaign deferred: the two
  predicates are not a transitional state pending unification: they are
  the intended, final shape, because they encode two genuinely different
  contracts (permanent classification vs. mutable roster membership)
  that no single predicate can honor at once.

**Consequence.** The kernel carries two classification predicates
permanently, and every future consumer (a new domain minting entities, a
future Book construction, PROC-15's coverage sweep) must know which
contract its subject needs — functional/genesis-only, or
mutable/roster — and register the matching predicate rather than reach
for whichever one is already in scope. The cost is one more distinction
to teach; the alternative (a merged predicate silently carrying both
contracts) would have been a correctness hazard for both consumers, not
a simplification. Nothing in the ledger or the Book's aggregation
changes as a result of this decision: it ratifies behavior both
campaigns already shipped, so it costs no code and no re-pin.

**See also.** Decision
0060 (`is-a` is a kernel-core classification
predicate); the Individuation campaign (`instance-of`, `Ledger::kind_of`,
latest-wins roster reassignment); `kernel/src/world.rs` (`IS_A`,
`INSTANCE_OF`, `KERNEL_CORE_PREDICATES` — the doc comments this decision
resolves); `windows/book` (the construction table reading both
predicates); decision
0010 (predicate schema: value-kind enforced,
subject-kind refused — the contradiction-checking contract `is-a`
depends on).
