# Six Principles

Every design question in Hornvale is settled by appeal to six principles.
When a decision in year three seems hard, it gets answered by these, not by
mood. The full design spec lives in the repository
(`docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`); this
chapter is the working summary.

## 1. Sim first, game as lens

Hornvale is a multiscalar simulation of invented worlds. The text adventure
is one *client* of the simulation's query surface — eventually the most
important client, but never the owner of any state. Anything a player can
experience must be a query any tool could make. The practical test: the REPL,
the almanac generator, and (someday) the game must all be able to ask the
same question and get the same answer.

## 2. Coarse constrains fine, never the reverse

The single invariant that governs space, time, and fidelity. A finer-grained
answer — a higher-fidelity provider, a closer look, a later elaboration — may
**add detail** to a coarser answer but may never **contradict** it. This is
what keeps seams closed: between map chunks, between the statistical past and
the observed present, between a cheap implementation of a system and the
expensive one that replaces it. Break it once and the world visibly tears.

## 3. Determinism is constitutional

Seed + committed facts → the same world, forever, on every machine. This is
simultaneously:

- the **infinite world** mechanism (nothing needs to be stored that can be
  re-derived),
- the **save format** (a save file is a seed plus a ledger of facts),
- the **test strategy** (byte-identical output is asserted in CI), and
- the **science story** (controlled experiments: same world, but the moon is
  tidally locked — every difference downstream is attributable).

Consequences ripple everywhere: no wall-clock time exists anywhere in the
system, no unordered collection ever touches serialized output, and all
randomness flows from one hierarchical seeding scheme.

## 4. Every domain is a provider behind a fidelity-agnostic interface

Domains — astronomy, climate, settlement, religion — expose *query
vocabularies*, not mechanisms. Multiple implementations per domain answer the
same questions at different fidelity: the trivial sun ("always up, always
noon") and a full ephemeris answer the same query with different richness.
Downstream systems consume salience-ranked phenomena and never learn which
implementation answered. The interfaces are the project's central
intellectual artifact; each co-evolves with a vision-book chapter.

## 5. Nothing exists until it's observable

Every system, from its first week of life, must be interrogable through the
REPL and must contribute to the almanac. No abstraction ships without a
window. Corollary, adopted after Campaign 1a: no campaign is *complete* until
it has produced artifacts — images, documents, graphs — demonstrating what
was built. The Gallery in this book is where they live.

## 6. Domains depend only on the kernel, never on each other

All cross-domain communication flows through the kernel's trace protocol.
Adding a new domain must never require editing an existing one. Agility and
clean extensibility outrank elegance, performance, and completeness — this
principle exists because its violation is how previous projects died.
