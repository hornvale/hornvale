# 0058. The Book is a derived view; grammar is build-state, never world-state

**Status:** Accepted (2026-07-17) · **Decider:** Nathan (G3, The Self-Writing
Book program metaplan)

In the context of launching The Self-Writing Book program — an engine that
renders Hornvale's own Fact graph into surface prose (`hornvale book`) — and
needing to decide whether the natural-language grammar that does that
rendering is *world-state* (part of what a saved world commits to) or
*build-state* (part of how the engine reads a world, like a provider), we
decided that **The Book is a derived view over the fact graph, and the
grammar/schemas/source-domain substrate that render it are build-state**:
authored and versioned like code, reconstructed at load exactly as tier-0
providers are, and never serialized into the ledger. A grammar change
drift-checks the committed Book artifact but **never epochs a world save**.

**Context.** Hornvale's determinism contract (Constitution; kernel docs)
already draws this line for providers: `ConstantSun` and a generated star
system are both valid tier-0/tier-1 readings of the same seed, and neither
is stored — a world is a seed plus a ledger, and everything else, including
which provider renders it, is re-derived deterministically at load. The
metaplan's architecture section extends this to language: a `ClauseSpec` is
the language-neutral semantic contract (frame, participants, features), and
a per-language grammar is a pure function `ClauseSpec → String`. Nothing
about *how* Common or Goblinesque phrases "Elthandil is a planet" changes
what is true about Elthandil — the `is-a` Fact
(`kernel::world::IS_A`) is the same
world-state regardless of which realizer reads it. Treating the grammar as
world-state would mean every wording refinement, every new realization
table, every fixed article-agreement bug forces an epoch suffix (decision
0039) on saves that made zero new seeded draws — an enormous, unwarranted
blast radius for what is, in every case, a rendering change.

**Decision.**

- The Book (`hornvale book`, rendered by `windows/book`) is a **derived
  view**: CLI-emitted, byte-identity drift-checked in CI exactly like the
  seed-42 almanacs, never hand-authored and never an mdBook-native section.
- The grammar (`ClauseSpec → String` realizers), the entry-schemas,
  aggregation/referring-expression rules, and the source-domain substrate
  for metaphor (later campaigns) are **build-state**: authored, committed,
  and reconstructed at render time — the same category as tier-0/tier-1
  providers, never part of `World { seed, registry, ledger }`.
- A grammar change updates the committed Book artifact (and drift-checks
  it), but **never epochs a world save**. Only new seeded draws are
  epoch-triggering, and the standing principle is to **minimize** them:
  prefer deriving any linguistic parameter (register, grammaticalization
  depth, word order) from existing world-state — a culture's already-drawn
  fields — over introducing a fresh stream draw whose sole purpose is
  prose.
- "Models author, dice roll" applies here as everywhere else: the grammar
  is an offline-authored, committed table; realization at render time is
  deterministic and seeded only through the kernel's existing `Seed`/
  `Stream`, never through a new ad hoc draw invented for language.

**Consequence.** Every future campaign in the program (C2–C8: predicates,
tongues, the chorus, explanations, doctrine, deep grammar, the diachronic
Book) can grow the grammar — new realization tables, new per-language
realizers, new filter stacks — without touching a single existing world's
epoch. The only way a Book-program campaign forces an epoch is if it also
adds new *seeded draws* to world-state (e.g., a genuinely new per-culture
parameter that must vary the world, not just its telling) — and that is a
deliberate, separately-justified choice, not a side effect of improving
prose. This mirrors decision
0039 (epochs replace tiers, refine
never contradicts) and decision
0033's quantization boundary: the
rendering/formatting layer is free to change; the seed-derived substance is
not.

**See also.** `docs/superpowers/specs/2026-07-17-the-self-writing-book-program-metaplan-design.md`
§§2, 4, 5 (items 1 and 5), §8 flag 1; decision
0039 (epochs replace
tiers); decision
0009 (models author, dice roll); `kernel/src/world.rs`
(`World::new`, the `IS_A` predicate this program's first Fact renders).
