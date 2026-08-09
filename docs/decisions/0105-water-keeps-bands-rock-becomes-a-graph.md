# 0105. Water keeps bands, rock becomes a graph — supersedes The Stratum's D3 for the rock realm only

**Status:** Accepted (2026-08-06) · **Decider:** Nathan · **Relates:**
[0026](0026-slugs-not-numbers.md),
[0094](0094-a-deliberate-duplicate-shares-its-roster-never-its-derivation.md),
[0101](0101-geometry-and-society-are-separate-vocabularies.md),
[0102](0102-one-per-cell-was-an-index-artifact.md),
[0104](0104-a-threshold-must-know-its-variates-distribution.md)

In the context of *The Deep Realm* building an inhabitable underworld, facing
The Stratum's D3 ruling that strata become inhabitable via a **band** and not an
address, we decided that **the rock realm is a graph of addressed chambers while
the water realm keeps its bands** — accepting a second, sibling address space as
the price, and explicitly **not** incurring the objection D3 actually raised.

## What D3 ruled, and why it was right

> **D3 — Strata become inhabitable via a band, not an address.** Leaves
> `RoomAddr` (save-format class) untouched and unifies the water column with the
> rock column. *Discarded:* a `RoomAddr` depth index (touches addressing).

That reasoning is sound, and it stays sound **for water**. Every ocean cell holds
the same five pelagic zones, in the same order, at the same depths. A band is
the right construct because the column is genuinely uniform: what varies between
two ocean cells is the *content* of a zone, never which zones exist. Diving is a
continuous movement through a medium, and a band names how far down you are.

D3's discard was also correct on its own terms. A `RoomAddr` depth index touches
a save-format-class type, and paying that cost to express something band-shaped
would have been a bad trade.

## What D3 assumed, and where the assumption fails

D3 unified the two columns, which required assuming the rock column is
band-shaped too. **It is not**, and the difference is structural rather than a
matter of resolution:

```
  WATER                              ROCK
  every cell has all five zones      most cells have no void at all
  zones are contiguous               voids are sparse and scattered
  zones stack vertically             passages run sideways
  you move BETWEEN zones             you move between CHAMBERS, through
                                       the gaps in a solid
  a zone at a coordinate is a fact   three chambers at odd depths under one
                                       cell, none under its neighbour, and a
                                       tunnel between them
```

Bands at a coordinate cannot express the last row. A band answers *"how deep am
I?"*; the underworld's questions are *"from here, where can I go?"* and *"what
is in this chamber?"* Both are node questions, and a node needs a name.

## What we decided

**The rock realm is a graph.** A chamber is a node addressed in a fixed lattice,
a passage is an edge, and both existence and content are pure functions of the
address. Nothing is stored.

**`BandKind` demotes from navigation to description.** In water a stratum is
something you move *between*; in rock it says how deep you are and what
surrounds you. The same type, read two ways, in two realms.

**Water is untouched.** `Realm::WATERWORLD` keeps its five pelagic strata and
its `dive`/`surface` verbs, unchanged.

## D3's actual objection is not incurred

This is the load-bearing half of the ruling, and it is a fact about the code
rather than an argument: **`RoomAddr` is untouched.** `ChamberAddr` is a
*sibling* address space — a separate type, in `windows/worldgen`, carrying no
`Serialize`/`Deserialize` at all. D3 discarded a `RoomAddr` depth index because
it touches addressing; C2a adds no depth index to `RoomAddr` and does not widen
it. The thing D3 was protecting is exactly as protected as it was before.

## The costs we accept

**A second address space.** Two ways of naming a place now exist, and a reader
must know which realm they are in. Mitigated by the sibling relationship being
explicit and by the two never meeting in one type.

**An address that will become a permanent key.** Nothing in C2a serializes a
`ChamberAddr`, but the moment the override seam has a writer, a dug chamber's
fact is keyed by one forever. The format was therefore settled with the care a
stream label gets rather than deferred: an address names a **place** (cell,
entrance, band, slot), never a construction step, and its `band` component
indexes the **permanent** `BandKind` ladder rather than the bands a particular
world happens to realize. The derived stream's key spells that band **by name**,
never by index, because an index is a declaration position and a name is not.

**A duplicate roster.** `hornvale_climate::Stratum` mirrors
`hornvale_terrain::BandKind` because a domain may not depend on a sibling
(0094: a shared roster, never a shared derivation). A test in `cli/` is the only
thing keeping them aligned.

## What it bought, and what it did not

It dissolved the spec's one named hard problem. Deriving A's neighbours from A's
address and B's from B's, and needing them to agree, is genuinely difficult — a
fixed lattice makes an edge *"two adjacent addresses that both exist"*, and
adjacency over a lattice is symmetric by construction, so there is nothing to
keep in sync.

It did not buy any character of its own, and the campaign measured that rather
than assuming it. Chamber count per cave is **exactly** `Binomial(4(rank+1),
0.5)` — mean, standard deviation and coefficient of variation all match theory
to three or four decimals in every band that occurs. Given the depth band, the
graph carries no place information at all: two caves in the same band have
statistically identical graphs whatever their rock, climate, elevation, or kind.
**Everything the underworld has, terrain gave it.**

That is a real limitation and it is recorded here rather than in a footnote,
because it is the evidence for what comes next: the only place-character the
underworld has arrives through a three-valued depth budget that is still welded
to the existence gate it is drawn from.

## Alternatives rejected

**Extend `RoomAddr` with a depth index.** D3 already rejected this and its
reasoning survives intact: it touches a save-format-class type to express
something that does not need it.

**A 3D void field.** A field answers *"is point (x, y, z) rock?"*, and nothing
in the codebase ever asks that. It would have cost a `value_noise_3d` primitive
in the kernel — a permanent save-format contract — plus a `z` on the
`Serialize`d `Position`. Both costs vanish with the volume.

**A uniform band column for rock, keeping D3 whole.** This is the option D3
implicitly chose, and it fails on the row bands cannot express: sparse voids,
absent under most cells, connected sideways.
