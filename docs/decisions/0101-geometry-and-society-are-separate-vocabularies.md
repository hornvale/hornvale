# 0101. Geometry and society are separate vocabularies — locale/site/chamber, community/settlement

**Status:** Accepted (2026-08-04) · **Decider:** Nathan · **Supersedes:**
[0082](0082-locale-chamber-place.md) · **Relates:**
[0037](0037-the-room-tier-ledger-is-chunk-partitioned.md),
[0048](0048-flow-condensation-replaces-the-suitability-scatter.md),
[0069](0069-fine-position-is-never-serialized.md),
[0100](0100-fact-phenomenon-myth.md)

In the context of a settlement model that had conflated *how much ground a
people covers* with *who that people is*, facing a vocabulary in which one word
named two scales and another named both a band and its supertype, we decided
that **geometry and society are separate vocabularies: a *cell* contains
*locales* contain *sites* contain *chambers*; a *community* is one polity and a
*settlement* is the derived footprint of all communities contiguous at locale
grain** — accepting prose churn across the book and doc comments, and that
"place" is retired as a term of art.

## The conflation this exists to remove

`Community` in the deep-history bake carries a people, a lineage, a memory and
tribute relations. It was emitted **1:1** as `is-settlement` at a cell. So a
settlement *was* a cell, which forced one-community-per-cell, which manufactured
a ~112 km minimum separation that nobody designed — see
[0102](0102-one-per-cell-was-an-index-artifact.md).

Two different things wore one name: **a polity** (who these people are) and **a
footprint** (how much ground they cover). Separated, the question that motivated
this record answers itself with no new machinery: *two communities whose
footprints touch are one settlement.* Goblins and hobgoblins in distinct
neighbourhoods and goblins and hobgoblins in abutting settlements stop being
different cases — they are the same fact read at two grains.

## The vocabularies

```
GEOMETRY -- bands, nested, defined by what a body can do in them
  cell      the worldgen quantum; the maximum extent of one settlement
  locale    the walk band; the grain at which footprints merge
  site      one scene: a castle floor, a hamlet, a town quarter
  chamber   one room

SOCIETY -- entities
  people        a species-as-culture (goblin, hobgoblin)
  community     ONE polity: a people, a lineage, a memory, tribute ties
  settlement    DERIVED: the footprint of all communities contiguous at
                locale grain
  quarter       one community's share of a shared settlement
  rank          DERIVED: camp / hamlet / village / town / city
```

Three deliberate choices:

- **`site`, not `place`.** "Place" is load-bearing across the codebase and book
  (`is-place`, and 0082's supertype sense) and cannot be narrowed to the
  roguelike-floor tier safely. `site` is free and already reads correctly in
  `StrangeSite`, `occ-site`.
- **`settlement` becomes derived.** A contiguity readout, not a committed fact.
  This is what lets a metropolis span many locales while two hamlets one locale
  apart read as one town — and it is [0100](0100-fact-phenomenon-myth.md)'s rule
  5 applied (a footprint is computable from its communities' addresses).
- **`community` keeps its name**, because the bake already uses it. The change
  is only that it stops being emitted 1:1 with a cell.

**Rank is an attribute, not a type.** One entity type carries a derived rank
that changes as population and function change, so a village growing into a city
is the *same* entity re-ranked, with history continuous across the change — the
move stratigraphy makes in separating a rock body from its rank. And rank is
derived from **function, not size**: a city is a place that serves a hinterland,
which the bake's existing tribute/patron/vassal state already expresses.

## What 0082 got right and keeps

- **`RoomAddr`, `RoomId` and `room_from_text` are still not renamed.** An
  address is correct at every band; the type is the address space, not one scale
  within it. 0082's reasoning stands and 0037 makes `RoomId` the one frozen
  save-format handle.
- **`locale` and `chamber` keep their meanings and their scales** (the walk band
  and its ninth refinement).
- **The failure mode 0082 named is the reason this record exists.** Size was
  *"the product of two constants in different crates that never meet"*, and no
  test could catch it. That pathology produced a second casualty in the
  settlement separation rule, which is why the bands below are stated with their
  scales attached.

## Scales, and the caveat that must travel with them

```
  cell      L6   ~112 km    40,962 per planet
  locale    L12  ~1.74 km   4,095 per cell   (walk_depth = globe_level + 6)
  site           sub-locale
  chamber   L21  ~3.3 m
```

**These metres assume an Earth radius the sim does not define.** The mesh is on
the unit sphere and elevation is the only metric length in the model; the locale
schema already forbids asserting metres per cell. Quote the figures as
*"at Earth radius"* or quote the angular measure. The *ratios* (2⁶ between cell
and locale) are exact and radius-free.

## Consequences

- **"Place" is retired from new prose, new doc comments, new player-facing
  strings and new spec sections.** Existing `is-place` facts and identifiers keep
  their names; chronicles written before this record are not rewritten.
- **A settlement is no longer a ledger subject.** Anything that read
  `is-settlement` as an entity must read communities and derive the footprint.
  This is a behaviour change with a wide blast radius and belongs to its own
  campaign, not to this record.
- **0048's accepted regression is reframed rather than resolved.** That record
  logged that per-species condensation *"loosens the old cross-tag spacing
  rule (two species may now co-occupy geography)"*, deferring real exclusion to
  the MAP-22 coexistence stack. Under this vocabulary co-occupation is not a
  loosening at all — it is two communities in one settlement, which is the
  intended reading.
- **The vocabulary is now stated in one place with its scales**, so the two
  constants finally meet on the page.

## See also

[The Lintel spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-27-the-lintel-design.md)
§§2–5 (0082's origin); frontier `SOC-settlement-tiers` (commit the contingent,
derive the regular — the lower lattice this vocabulary makes expressible);
`SOC-dense-settlement`.
