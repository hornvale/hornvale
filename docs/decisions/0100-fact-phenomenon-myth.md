# 0100. Fact, phenomenon, myth — the three registers, and what may be written where

**Status:** Accepted (2026-08-04) · **Decider:** Nathan · **Relates:**
[0003](0003-trace-protocol-is-the-only-cross-domain-channel.md),
[0007](0007-seed-is-identity.md),
[0037](0037-the-room-tier-ledger-is-chunk-partitioned.md),
[0069](0069-fine-position-is-never-serialized.md),
[0098](0098-hornvale-is-single-player.md)

In the context of wanting a world rich enough to hold a burned-out goblin
village, a legend about the gold still in it, and the deed of the player who
went and found out — while generating in finite time — we decided that **world
data occupies three registers with different coherence obligations: *fact*
(committed, contradiction-checked), *phenomenon* (derived, coherent by
construction), and *myth* (derived, fallible, and required to carry a
holder)** — accepting that myth may contradict fact and other myth, and that
this is the feature rather than a defect to be checked away.

## The line already exists; this record names its consequences

The governing principle is **store-irreversible-derive-reversible** (The Walk
§3.6, cited by [0037](0037-the-room-tier-ledger-is-chunk-partitioned.md)), and
the bound on the stored half is frontier `MEM-1`'s memory economy, whose melt
path — **fact → phenomenon → myth** — supplies this record's vocabulary. What
was missing was an operational test and the rules at the seams.

**The test.** *Could I recompute this from the seed alone?* If yes, it is not a
fact. Stated for use: **commit consequence, derive presence.** A site nothing
ever happened to needs no facts; a site something happened *to* must be
committed, because an event is not a function of its address.

## The registers

```
  FACT        what is TRUE     committed; globally coupled; contradiction-
                               checked against the concept registry; the only
                               register that costs storage and bake time
                               "hill dwarf presence 300 years ago"
                               "quartz veins, sulfide-rich ores"

  PHENOMENON  what is THERE    derived from (seed, address) or from
                               (seed, address, play ledger); coherent by
                               construction; free; evictable
                               "gold ore", "giant spider habitat",
                               "an abandoned hill dwarf mine"

  MYTH        what is SAID     derived; free; evictable; NOT required to be
                               coherent, with or without fact or other myth
                               "the dwarves fled the spiders"
                               "there's gold in there still"
```

These are an *epistemic* classification and are orthogonal to the trace
protocol's *channel* classification (0003: Facts, Phenomena, Fields). Where the
two touch they agree: a Field is a derived statistical prior and sits in the
phenomenon register. Myth is new, and has no channel today.

## The rules at the seams

1. **No upward writes.** Myth reads fact; fact never reads myth. Phenomenon
   reads fact; fact never reads phenomenon. The dependency is acyclic, the same
   shape as `kernel → domains → windows → cli`, and it is *why* the derived
   registers are cheap: no feedback means no global recomputation.

2. **Every myth carries a holder.** Attribution is the entire difference between
   richness and a bug: *"the dwarves say X, the goblins say Y"* is texture;
   bare `X ∧ ¬X` is a defect. A myth without a holder is malformed.

3. **Never commit half a related pair.** Derive a mine and its legend
   *together* and referential integrity is free — both are functions of the same
   premises, so they cannot disagree. Commit one and derive the other and you
   own an invalidation problem permanently.

4. **Committed facts reference derived things by address, never by identity.**
   This generalises [0069](0069-fine-position-is-never-serialized.md)'s *"no
   saved state may ever point into the fine layer"*: *"what was at locale X"* is
   committable, *"spider #4718"* is not. `RoomId` is the handle.

5. **Never commit a balance.** Anything computable from other committed facts is
   a cache with no invalidation story. **Never commit an address** either — a
   derived site's locale is a function of (seed, cell, index), so committing it
   is storing a balance. Commit only a *displacement*, when history actually
   moved a site.

## Two ledgers and a cache

Partitioned by **authorship**, because evictability turns out to be exactly
co-extensive with seed-authorship:

```
  seed-authored + committed  ->  THE WORLD LEDGER    irreducible, coherent
  seed-authored + derived    ->  THE CACHE           evict freely
  act-authored  + committed  ->  THE PLAY LEDGER     never evict
  act-authored  + derived    ->  THE CACHE           evict; needs the play
                                                     ledger to recompute

  state = derive(seed) + replay(play ledger)
  the cache is NEVER authoritative
```

**Two ledgers rather than one**, because merging them would make a played world
irreproducible from its seed and break [0007](0007-seed-is-identity.md). **Flat
rather than nested**, because 0037 already ruled that address-prefix
partitioning waits for a world that measurably outgrows RAM — *"a future
decision to ratify then, with real data, not now."* Ledgers are flat and
addressed; **knowledge** is what is scoped, not storage.

A player acting on a derived thing **promotes** it into the referenced world.
Under [0099](0099-worlds-are-version-locked.md) that promotion need only survive
its own version, so no derivation function becomes a permanent contract.

## This closes `KNOW-unchecked-store`

That row asks whether it is deliberate that the world's fact ledger is
contradiction-checked while the knower's store is not checked at all, noting the
asymmetry *"deserves an explicit answer either way."* **It is deliberate.** One
channel is designed to exclude falsehood and the other to admit it —
`KNOW-observer-built` already ships that split, exempting heard entries
*"because telling can transfer a false belief."* Myth inherits the unchecked
store; fact keeps the check. The register a datum occupies determines which
contract applies, and a contradiction inside the myth register is never silently
tolerated so much as *correctly held*, because holders differ.

## Consequences

- **The ledger structurally cannot host myth**, so this is not a stylistic
  preference. Contradiction-checking would reject exactly the fallible legends
  that make the world worth walking through.
- **Player deeds become legends through the same melt path** that handles the
  Pleistocene — one mechanism for deep history and last Tuesday. They must
  propagate as *myth*, never as fact, which is what keeps rule 1 intact and
  what makes fame arrive distorted by distance and retelling.
- **`biome`, `latitude` and `longitude` on every settlement are stored
  balances** — verified: `let coord = geo.coord(s.cell)` and
  `biome: climate.biome_at(s.cell)` in `windows/worldgen/src/lib.rs`. Removing
  them is a save-format change and is not in this record's scope, but they are
  the standing example of rule 5 and should not be reproduced.
- **`KNOW-observer-built`'s three named absences are now partly specified.**
  Its missing *provenance* is rule 2's holder; its missing *forgetting* is
  `MEM-1`'s melt. Its missing contradiction check is answered above:
  deliberately absent.

## See also

Frontier `MEM-1` (the memory economy and the melt path this vocabulary comes
from); `UNI-30` (the living present); `KNOW-observer-built`, `KNOW-entry-contract`,
`KNOW-unchecked-store` (the knowledge store this record rules on); The Walk
§3.6 (store-irreversible-derive-reversible).
