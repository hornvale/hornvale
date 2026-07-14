# 0051. Procedural names are salted by stable identity, never by a global mint counter

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of The Self-Describing Sky campaign minting one ledger
entity per notable neighbor (decision
0050), facing a defect that surfaced only
during execution — every generated deity's name changed for every seed,
though nothing about the world's physics had — we decided that **a
procedural name generator must be salted by something stable about what it
names, never by a global entity-id or mint-order counter**, accepting that
this forces a second, unplanned naming epoch inside the same campaign.

**Context.** `windows/worldgen`'s `LanguageDeityNamer` passed a belief
entity's `EntityId` straight into `hornvale_language::glossed_name(kind,
salt, …)` as the per-name seed. That id is assigned by the ledger's
monotonic entity-mint counter, which counts *every* entity minted during
genesis, not just beliefs. Astronomy genesis runs before religion genesis,
so when the neighbor epoch started minting one entity per neighbor, every
belief entity's id shifted downstream by however many neighbors a world
has — and with it, every deity name for every seed, silently, because the
coupling was never named as a coupling. The world stayed fully
deterministic throughout (same seed still produced the same, self-
consistent output); the fragility was that a *name* depended on global
mint *order*, which any future addition anywhere earlier in the genesis
sequence can perturb again.

**The precise defect: the belief id was doing two unrelated jobs.**
(1) It **keys** the committed `name-gloss` fact to its belief entity
(`name_gloss_fact(EntityId(salt), gloss)`) — correct, and necessary so
`recount`/`explain` can find a deity's gloss later. (2) It **seeded** the
name itself, passed into `glossed_name` — the bug. Two jobs sharing one
value meant a change aimed at (1)'s domain (minting more entities
elsewhere) silently broke (2).

**The fix.** Separate the two jobs. The belief id keeps keying the gloss
fact, unchanged. The name seed is re-derived from the belief's **semantic
identity** instead:

```
name_seed = seed.derive("religion/deity/v2")
                .derive(species)            // per-species namer context
                .derive(&phenomenon.kind)   // what the deity is OF
                .derive(&index.to_string()) // rank among this call's members
                .stream().next_u64()
```

`species` and `phenomenon.kind` ground the name in what the deity is
actually about (the property the namer's own code comment already wanted);
`index` — the phenomenon's rank in the physics-derived salience list, not
any ledger id — disambiguates members sharing a kind (two moons, two
same-colour neighbours) and stays stable under entity-id shifts because the
phenomena list is derived from the providers, not read back from the
ledger. `description` is deliberately excluded: it can carry a float, and
folding a float into a name seed reopens the exact cross-platform
quantization surface decision
0033 exists to close.

**Decision.** Procedural-name generators take their seed from a value that
is invariant to unrelated entity-minting elsewhere in genesis — a semantic
key (kind, species, rank, or similar) — never from an `EntityId` or any
other counter whose value depends on how many *other* things a world
happens to generate. Where an id is also needed to key a fact back to its
owning entity, that keying use and the naming-seed use must be visibly
separate values, even when, as here, they start from the same caller-
supplied argument.

**Consequence.** Deity names changed once more, for every seed, under the
`religion/deity/v2` epoch label — a name-only rebaseline, no physics moved.
The fix is localized to `windows/worldgen`'s namer and its tests;
`domains/religion::genesis` is untouched (it still passes the belief id,
now used only as the gloss key), and the `DeityNamer` trait signature is
unchanged. The keystone test (`windows/worldgen`) asserts the invariant
directly: the same phenomenon and rank produce the same name under two
different `salt` values, the exact property the old scheme violated.

**See also.** `docs/superpowers/specs/2026-07-13-the-self-describing-sky-design.md`
§§8–9 (the fuller narrative: how the coupling was found, the two-jobs
diagnosis, the ideonomy pass that ruled out reordering as a fix); decision
0050 (the neighbor epoch whose entity
minting exposed this); decision
0039 (epochs vs. tiers — why this is a
`/v2` label, never a rename); `book/src/chronicle/the-self-describing-sky.md`.
