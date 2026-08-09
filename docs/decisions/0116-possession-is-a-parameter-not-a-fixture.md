# 0116. Possession is a parameter, not a fixture

**Status:** Accepted (2026-08-09, G6) · **Decider:** Nathan · **Relates:**
[0007](0007-seed-is-identity.md),
[0022](0022-sim-emits-data-clients-render.md),
[0114](0114-a-native-client-drives-across-the-linker-and-reads-across-the-serializer.md)

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
commanded =       | -- (a body with     | TODAY: mint_flagship | --
minted flagship   | no eyes)            | + derive_npcs        |
------------------+---------------------+----------------------+---------------------
commanded = an    | --                  | free possession      | an NPC's own
EXISTING creature |                     |                      | purview, played
```

Every filled cell names something the project had already proposed
independently, under its own name, as its own future campaign. Three of the
nine are structurally empty (a body with no eyes), and that is a property of the
model rather than a gap in it.

## The decision

`Session::start` takes a possession *target*. The Quire ships two values:

- **`Flagship`** — the default, unchanged, `mint_flagship` exactly as before,
  byte-identical against every committed gallery transcript and client fixture.
- **`FirstSettlement`** — the existing resident of the first settlement,
  possessed rather than minted, deterministic and seed-stable.

That is the middle column. The campaign built none of the other cells and built
the parameter that makes them reachable.

## Why this is worth a record rather than an implementation note

**It converts a product decision into a parameter decision.** The world viewer
had been carried as a competing deliverable — a separate client with its own
dependencies, driving the CLI as a subprocess. Read as a cell, it is the same
client with `commanded` set to nothing, and building the target parameter once
is what stops the viewer being built twice. Decision 0114 independently killed
the subprocess design on cost grounds; this record explains why nothing is lost
by killing it.

**It closes a doctrine gap that had gone unnoticed.** The client's own brief
states flatly that *you possess a creature already living in the world — you do
not create a character*. `Session::start` called `mint_flagship`, which finds
the one settlement, resolves its species and **creates** an agent there;
`derive_npcs` then populated neighbours around that creation. The possessed
creature was invented, not discovered, and the doctrine said otherwise. Adding
the target is not a new feature; it is closing the gap between what Hornvale
says it does and what it does.

## Consequences

- **The default must remain byte-identical, and this was held to, not assumed.**
  A field added to `PossessOpts` with a `Default` is invisible to every
  `::default()` caller — including the cost gates a parallel campaign rewrote
  during this one — so the compiler would *not* have caught a wrong default. It
  was verified by reading the `Default` impl and by the artifact drift check
  over `book/src/gallery/possession-*.md`, twice, across two absorptions.
- **A new target is a new seed-ordering surface.** "The first NPC found anywhere
  in the world" needs a definition that does not presuppose the flagship, and it
  must be deterministic and seed-stable. `FirstSettlement`'s ordering rule is
  part of this record's surface; a third target must state its own.
- **The hoist and the parameter are complementary, and only together do they
  pay.** Decision 0114's client holds one session per process, so the 46.5×–49.8×
  repossession speedup measured in The Quire is a capability, not a saving. It
  becomes a saving in exactly the cells above — attract mode, free possession, a
  spectator following a different creature — because each of them switches the
  possessed body without leaving the world.

## See also

[The Quire spec §7](../superpowers/specs/2026-08-08-the-quire-design.md);
[The Quire chronicle](../../book/src/chronicle/the-quire.md);
`windows/vessel/src/lib.rs` (`PossessOpts::target`).
