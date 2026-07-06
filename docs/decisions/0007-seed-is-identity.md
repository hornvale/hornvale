# 0007. The seed is a world's identity

**Status:** Accepted (2026-07-05) · **Decider:** Nathan

In the context of world generation that can fail a pin (e.g. a requested moon
the physics won't grant), facing the temptation to silently retry with a
nearby seed until it works, we decided that **the seed *is* the world's
identity — generation never retries across seeds; pins fail loudly with the
physical reason**, accepting that some (seed, pin) combinations are simply
impossible and say so.

**Context.** Retrying across seeds would make the seed a mere suggestion rather
than an identity, breaking reproducibility and the "same world, one variable
changed" experimental story. Searching for a seed that satisfies some criterion
is a legitimate but *separate* and *explicit* operation.

**Consequence.** Pins raise `GenesisError` with the physical reason; refusals
are recorded as genesis-note facts in the world's ledger. Seed search is the
explicit `scout` verb, never an implicit fallback. Unpinned generation may
degrade gracefully (e.g. fewer moons than drawn) but pinned generation fails
loudly.

**See also.** Campaign 2 spec §4; `CLAUDE.md` "Determinism"; chronicle 2b.
