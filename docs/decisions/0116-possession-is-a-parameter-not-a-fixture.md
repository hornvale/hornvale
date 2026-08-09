# 0116. Possession is a parameter, not a fixture

**Status:** Accepted (2026-08-09, G6) · **Decider:** Nathan · **Relates:**
[0007](0007-seed-is-identity.md),
[0022](0022-sim-emits-data-clients-render.md),
[0114](0114-a-native-client-drives-across-the-linker-and-reads-across-the-serializer.md)

> **Corrected before it landed.** The first draft of this record claimed the
> campaign shipped possession of "the existing resident of the first
> settlement, possessed rather than minted", and that this closed a doctrine
> gap. Both halves were false: both shipped targets **mint**. The whole-branch
> review caught it while the record was still on the campaign branch, so it was
> repaired in place rather than landing wrong and being superseded on its first
> day. Decisions are append-only **once merged**; this one had not merged.

In the context of a native client that needed to answer "who is being
possessed?" and a long-standing separate proposal for a world-viewing client,
we decided that **`focalized` (whose senses filter the world) and `commanded`
(whose body executes verbs) are independent parameters of one mechanism**, that
the saturated grid of their values names every viewing mode the project has
proposed, and therefore that **the world viewer is a cell of that grid rather
than a separate product**.

## The grid

```
                  | focalized = none    | focalized = the      | focalized = a
                  | (cartographic)      | commanded agent      | DIFFERENT agent
------------------+---------------------+----------------------+---------------------
commanded = NONE  | the world viewer    | attract mode / the   | the scholar or
                  |                     | autonomous observer  | ethnographer vantage
------------------+---------------------+----------------------+---------------------
commanded = a     | -- (a body with     | SHIPPED: both of     | --
MINTED agent      | no eyes)            | this campaign's      |
                  |                     | targets              |
------------------+---------------------+----------------------+---------------------
commanded = an    | --                  | free possession      | an NPC's own
EXISTING creature |                     | (NOT BUILT)          | purview, played
```

Every filled cell names something the project had already proposed
independently, under its own name, as its own future campaign. Three of the
nine are structurally empty (a body with no eyes), and that is a property of the
model rather than a gap in it.

**This campaign shipped exactly one cell**, parameterised twice. It did not
reach the `commanded = an EXISTING creature` row at all.

## The decision

`PossessOpts` carries a possession *target* — a field, read by `Session::start`
and `Session::start_in` alike, not an argument either one takes. The Quire
ships two values, and **both mint**:

- **`Flagship`** — the default, unchanged, `mint_flagship` exactly as before,
  byte-identical against every committed gallery transcript and client fixture.
  The flagship is the first `is-settlement` fact in the ledger, which is what
  `village_info` returns — **not** the largest settlement. On seed 42 the
  flagship is Googo (pop. 68) while the most populous is Toa (pop. 84).
  (`book/src/reference/scene-tiles-v1.md` describes `kind: "flagship"` as "the
  world's capital, the single highest-population settlement any species
  founded", which that observation contradicts. The disagreement predates this
  campaign and is recorded in the idea registry, not resolved here.)
- **`MostPopulousSettlement`** — an agent minted at the world's most-populous
  settlement, ranked population-descending then id-ascending. Deterministic and
  seed-stable.

Both arms call `mint_at`, which resolves a species, computes a position from
the settlement's committed lat/lon, and derives a **fresh** `AgentId` from a
seed stream. They differ only in *which settlement*. Neither adopts an agent
`derive_npcs` already produced.

So what the campaign made selectable is the **settlement the commanded agent is
minted at**. That is a real and useful parameter — it is the seam every other
cell of the grid attaches to — and it is smaller than "possession became free".

## Why this is worth a record rather than an implementation note

**It converts a product decision into a parameter decision.** The world viewer
had been carried as a competing deliverable — a separate client with its own
dependencies, driving the CLI as a subprocess. Read as a cell, it is the same
client with `commanded` set to nothing, and building the target parameter once
is what stops the viewer being built twice. Decision 0114 independently killed
the subprocess design on cost grounds; this record explains why nothing is lost
by killing it.

**It names a doctrine gap, and LEAVES IT OPEN.** The client's own brief states
flatly that *you possess a creature already living in the world — you do not
create a character*. `Session::start` called `mint_flagship`, which finds the
one settlement, resolves its species and **creates** an agent there;
`derive_npcs` then populated neighbours around that creation. The possessed
creature was invented, not discovered, and the doctrine said otherwise.

**Adding the target did not close that gap.** It made the minting *site* a
parameter; the possession is still a mint. Closing the gap means selecting an
agent the world already derived and homing a session onto it — a different and
larger piece of work (an identity for a derived NPC that survives the session
boundary, and a session that starts from one rather than from a settlement).
It is not started. It is carried forward as `RENDER-possession-still-mints` in
the idea registry, and the `commanded = an EXISTING creature` row above is
empty on purpose. **A future campaign reading this record should treat free
possession as unbuilt.**

## Consequences

- **The default must remain byte-identical, and this was held to, not assumed.**
  A field added to `PossessOpts` with a `Default` is invisible to every
  `::default()` caller — including the cost gates a parallel campaign rewrote
  during this one — so the compiler would *not* have caught a wrong default. It
  was verified by reading the `Default` impl and by the artifact drift check
  over `book/src/gallery/possession-*.md`, twice, across two absorptions.
- **A new target is a new seed-ordering surface.** A target's selection rule has
  to be deterministic and seed-stable. `MostPopulousSettlement` shares
  `ordered_for_derivation`'s **comparator** (population descending, then id
  ascending) but not its resulting **order** — `ordered_for_derivation` hoists
  the home settlement to the front, so the two disagree on their first element.
  Sharing the comparator is what keeps this from being a new tie-break rule; a
  third target must state its own.
- **The name has to say what it selects.** The variant shipped review as
  `FirstSettlement` while selecting the *most-populous* settlement — and "the
  first settlement" is literally what the *other* variant, `Flagship`, uses
  (`village_info`). It was renamed before merge. A target name that describes
  its sibling is a defect in a record like this one, not just in code.
- **The hoist and the parameter are complementary, and only together do they
  pay.** Decision 0114's client holds one session per process, so the 46.5×–49.8×
  repossession speedup measured in The Quire is a capability, not a saving. It
  becomes a saving in exactly the cells above — attract mode, free possession, a
  spectator following a different creature — because each of them switches the
  possessed body without leaving the world.

## See also

[The Quire spec §7](../superpowers/specs/2026-08-08-the-quire-design.md) (whose
body carries the same original error, with a correction note beside it);
[The Quire chronicle](../../book/src/chronicle/the-quire.md);
`windows/vessel/src/lib.rs` (`PossessTarget`, `PossessOpts::target`);
`windows/vessel/tests/possess_target.rs` (`both_targets_mint_a_fresh_agent`).
