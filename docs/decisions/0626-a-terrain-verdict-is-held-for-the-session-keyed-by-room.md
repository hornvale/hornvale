# 0626. A terrain verdict is held for the session, keyed by room, with its terrain's identity supplied by ownership

**Status:** Accepted (2026-09-03) · **Decider:** Nathan · **Relates:**
[0206](0206-a-derived-values-key-is-its-validity.md);
[0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md);
[0537](0537-a-reader-never-observes-a-fold-behind-its-ledger.md);
[0627](0627-the-emitter-scan-advances-through-a-read-side-verdict-index.md);
[The Detent spec](../superpowers/specs/2026-09-02-the-detent-design.md) §2.1,
§5; [ledger](../superpowers/ledgers/2026-09-02-the-detent.md) #2, #3

In the context of a fear path that asked `terrain.hazards(room)` 44,694 times
on a single 50-agent tick to commit 31 facts, facing the measured fact that
every one of those samples is a pure function of the room, we decided that the
verdict is **taken once and held for the session** — a `Derived<Facet,
Hazards>` at `Validity::Pure`, keyed by the room alone, with the terrain's
identity supplied by **ownership** rather than by the key — accepting a
structure that nothing in this campaign evicts.

## Context

`Terrain::hazards(&self, room)` takes no `day` by contract ("a slow field, so
it takes no `day`"); `LocaleContext::hazards_at` reads the climate's geosphere,
the nearest-vertex index and the regime budget; and the session's predator
field is computed once at `Session::start`. Nothing in a session mutates any of
them. So the field being re-sampled every tick had not moved since the session
began — the cost was repetition, not computation.

`GroundHazards` is the `Pure` class's second concrete instance in the tree
(after The Forebay's `RoomMeshMemo`) and its **first in `windows/vessel`**,
which is what makes it the world-derived half of the adaptive-cache layer whose
ledger-derived half [0536](0536-the-resident-fold-store-is-session-owned-and-never-serialized.md)
built. It is owned by the `Session` beside `mesh_memo` and the resident store,
and equally ownable by the two benches and the lab — each of which rebuilds
`LocaleTerrain` per tick and would find a terrain-scoped memo cold on every
tick the criteria are measured over. It is read through interior mutability and
filled on read, because the rooms a tick will ask about are the union of every
roster member's visited rooms and their halos, which nothing knows before the
reads run.

## Why the key is the room and not a fingerprint

[0206](0206-a-derived-values-key-is-its-validity.md) makes key-completeness the
typed obligation: the key must carry every parameter the derivation reads. The
derivation reads the room, the `LocaleContext` and the predator field. A
`LocaleContext` is not a key-sized value — folding it in would mean a stable
hash of a large struct, computed per lookup, to establish a property ownership
already guarantees.

So the second road is taken, and it is one the resident layer already walks:
**one memo per `(LocaleContext, predator field)`**, owned by the object that
owns both, documented on the type in the same sentence `SustenanceMemo` already
carries for the temperature field ("one `LocaleContext` per store; a caller
that must change the field builds a new store"). 0206's own text allows it —
"world-derived" is `Pure` with the world's identity folded in, and here the
identity is the owner. The rule adds no assumption the store did not already
make.

## Two tests hold it, and one of them observes the aliasing it forbids

Ownership is a discipline, so it is pressured rather than asserted:

- **Chaos eviction.** `Derived::evict_all` after every read, thirty reads of
  `hazard_memory_memo` on the bench probe with a fresh `PrimaryAfraidMemo` each
  time and the same memo — every result equal to the first, byte for byte. The
  eviction test also asserts the memo's reach against an independently computed
  comparator (the union over every roster member of visited rooms, their
  neighbours, home and its neighbours) rather than printing a length.
- **The two-terrain test.** Two terrains over *different* predator fields, each
  with its own memo, must answer as themselves; two terrains *sharing* one memo
  alias. The test demonstrates the aliasing rather than assuming it impossible,
  which is what makes the ownership rule a claim with a witness.

## The one thing that would make it wrong, and why it cannot happen silently

A hazard that depends on the **day** — a future seasonal heat or cold. That
changes `Terrain::hazards`'s signature, and the memo's key is the first thing
the compiler refuses. A campaign that instead makes the predator field move per
tick must move the memo to `Validity::Ledger` or rebuild it per tick, and the
type doc says so. The failure mode this decision is most exposed to is
therefore a compile error, not a wrong world.

## Consequence

The fear path's terrain questions are answered from one session-lifetime fill:
on the 60-tick shape the memo recorded 11,149 misses against 1,934,552 hits —
99.4% — and a repeated read takes **zero** field samples. The fold's final-band
cost fell from 93.841 ms/call to 0.096 ms/call.

The cost accepted is memory that nothing releases: 18,902 rooms and ~1.455 MB
at 200 ticks on the 50-agent shape, growing monotonically with decelerating
increments. That figure is measured and reported without a threshold
(the spec's M1), because Penstock stage 4 — the lifecycle — is the stage that
gets an opinion about it, and this is the number it enters on.

**See also.** [The Detent chronicle](../../book/src/chronicle/the-detent.md);
[The Detent spec](../superpowers/specs/2026-09-02-the-detent-design.md) §11.4,
§12.4.
