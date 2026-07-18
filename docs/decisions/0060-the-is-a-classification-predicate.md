# 0060. `is-a` is a kernel-core classification predicate

**Status:** Accepted (2026-07-17) · **Decider:** Nathan (C1, The First
Sentence)

In the context of The Self-Writing Book needing to render sentences like
"Elthandil is a planet" purely from committed Facts — and the world's
entities already carrying a `KindId` in memory (the ECS component-registry
substrate from The Dissolution campaign) with no ledger-visible trace of
it — we decided that **`is-a` is a kernel-core functional predicate**,
registered by `World::new` beside `name`, and that **an `is-a` Fact is the
ledger projection of an entity's `KindId`**: classification is a fact the
world asserts about itself, not metadata a window infers from a component
lookup.

**Context.** Before this decision, an entity's kind (planet, settlement,
star, neighbor, moon, …) was knowable only by asking the ECS registry — a
lookup keyed by `KindId`, invisible to anything that reads only the
ledger's Facts. The Book's rendering engine, by design, only ever reads the
Fact graph (metaplan §1: content selection over Phenomena/Facts,
`A.isA(B)` *is* a Fact — subject/predicate/object — not a side-channel
query into the ECS). Without a ledger-visible classification, the first
sentence the whole program exists to produce would require either a
special-cased escape hatch out of the Fact graph (breaking the "the world
is a seed plus a ledger" contract) or leaving classification permanently
unrenderable — a real, unrenderable predicate that PROC-15's coverage
metric (metaplan §7) would then have to explain away rather than count.
Neither was acceptable: the constitution's trace protocol is the only
cross-domain communication channel, and PROC-15 exists precisely to make
"the world model has this gap" visible, not to grow blind spots it can't
see. Registering `is-a` where `name` already lives — as a kernel-core
predicate created in `World::new`, before any domain wires in — makes
classification exist for *every* world unconditionally, exactly like
having a name.

**Decision.**

- `is-a` (`kernel::world::IS_A`) is registered by `World::new`
  alongside `name` (`kernel::world::NAME`),
  functional (an entity has exactly one class), object a `Value::Text`
  KindId label — a kernel-core concept, not a per-domain registration.
- An `is-a` Fact — `(entity, is-a, <kind-label>)` — is the ledger's
  authoritative projection of that entity's `KindId`. Wherever worldgen
  mints an entity with a known kind (planets, settlements, and onward as
  later campaigns extend coverage), it commits the matching `is-a` Fact in
  the same genesis pass that assigns the `KindId`, so the two can never
  diverge.
- This makes classification queryable exactly like every other Fact —
  through `Ledger::find`/`find_all` on the `is-a` predicate — which is what
  lets `windows/book` discover "the planet" (and, as the program extends,
  every other classified entity) without any special-cased escape hatch
  out of the trace protocol.
- This is also what makes the PROC-15 coverage metric meaningful: it
  reports the fraction of committed fact-predicates that have a
  construction and a Common realization, and `is-a` is deliberately made
  the first predicate that fraction can equal 1 against, rather than a
  structural gap baked in from the start.

**Consequence.** The ledger-purist reading of "a world is a seed plus a
ledger" now covers classification, not just naming — nothing about an
entity's kind lives only in the ECS side-channel once that entity's
genesis commits its `is-a` Fact. Every subsequent Book-program campaign
(the predicates in C2 onward) builds on a ledger that already demonstrates
the pattern: a modeling fact that exists in memory is not "real" to The
Book until it is also committed as a Fact. The cost is symmetric and
accepted: every entity-minting site that wants Book coverage must remember
to commit its `is-a` Fact in the same pass it assigns a `KindId`, or that
entity silently regresses PROC-15's coverage number — a gap the report is
built to surface, not hide.

**See also.** `docs/superpowers/specs/2026-07-17-the-self-writing-book-program-metaplan-design.md`
§§1, 7; `kernel/src/world.rs` (`World::new`, `IS_A`,
`every_world_registers_is_a`); `windows/book/src/lib.rs` (the first
consumer, discovering classified entities via `ledger.find(IS_A)`);
decision
0050 (entity-hood for collections, the discovery-flag idiom
`is-a` generalizes); decision
0003 (the trace protocol is the only
cross-domain channel).
