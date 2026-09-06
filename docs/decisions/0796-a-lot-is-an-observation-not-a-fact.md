# 0796. A lot is an observation, not a fact

**Status:** Accepted (2026-09-05) · **Decider:** Nathan

In the context of a window that draws one representative life out of a world's
whole history, facing the rule that a window may never draw world-state, we
decided that **a lot's randomness is the reader's — `LotIndex(u64)` is an input
on the same footing as a world seed, and every choice in a life is a pure
hash-expansion of `(world.seed, index, <choice label>)` with no kernel
`Stream`, no seed label, no manifest row and nothing committed** — accepting
that a lot has the shelf life of an observation rather than a fact: the same
`(seed, index)` draws the same life only until the ledger it reads changes.

**Context.** `windows/CLAUDE.md` forbids a window from drawing because a window
that draws "has quietly become a domain with no registry entry and no
pin-isolation test" — and the hazard in that sentence is *commitment*: a domain
in disguise writes into the saved world, where a missing pin-isolation test
means a silently corrupted save format. A lot writes nothing. It moves no
seeded value, consumes no draw, and appears in no world file. So the hazard the
rule guards is absent, and the machinery that guards it would promise a
save-format contract for a value that is never saved. The sanctioned form
already existed: `hornvale_history::flesh::persona_of` is bit arithmetic over
its arguments alone, trivially total and deterministic, and the lot's
expansions are the same shape. The reader's key is the permalink — `<seed>/<index>`
for a drawn lot, `<seed>/<index>?year=<Y>&site=<V>` for a *picked* one, where
two of the choices are pinned and the index still drives the rest. See the
campaign ledger's entry #5.

**Consequence.** No `streams.rs` label, no stream-manifest row, and no
pin-isolation test for `windows/lot`; the guarantee is held instead by a
source-scan test asserting the crate never names `Stream`, and by a
byte-identity test on the `lot/life/v1` payload across builds and across the
wasm boundary. What is given up is durability of identity: a lot is not a
permanent name for a person the way a founder's is. When the bake changes, the
same permalink draws a different life, and that is correct — the lot is a
reading of the ledger, and the ledger moved. The pattern generalizes to any
reader-keyed derivation that commits nothing.

**See also.** Spec
[`2026-09-05-the-lot-design.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-09-05-the-lot-design.md)
§3; ledger
[`2026-09-05-the-lot.md`](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/ledgers/2026-09-05-the-lot.md)
#5; `windows/CLAUDE.md` (what a window may not do);
[0007](0007-seed-is-identity.md) (the seed is a world's identity);
[0022](0022-sim-emits-data-clients-render.md);
[The Lot](../../book/src/chronicle/the-lot.md).
