# 0177. A map's frame is a fact about the world, a view of the world is a lens, and co-location is not discovery

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0123](0123-disclose-a-resolution-rather-than-refine-a-field.md) (disclose a
resolution rather than refine a field — the same discipline applied to a
lost cell, here applied to a lost sample), [0142](0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md)
(a lost axis is declared — here a lost *area*, the projection's clamp)

In the context of *The Portolan, part II* building a whole-planet Mercator
chart and, by amendment, a discovery layer gating what the chart names, we
decided three things together, because all three were forced by the same
campaign and the third depends on the first two holding:

## The decision

1. **A map's frame is a fact about the world, not about the graticule.** The
   line a projection holds level is derived from the world's own committed
   physics — the geographic equator on a spinning world, the terminator on a
   tidally locked one — never fixed to an arbitrary geographic grid. A fixed
   frame is not merely inelegant here: on a locked world it puts the only
   habitable ring half inside the polar clamp, discarding the one band
   anyone can live on. What a projection is forced to discard must always be
   the ground nobody occupies, and that is a claim about the world, decided
   once at load from committed facts, not a rendering preference decided
   once per campaign.
2. **A view of the world is a lens, not a band.** The character occupies a
   band (`Spatial`'s `Walk`/`Chamber`); a map is something they *consult*.
   A whole-world view therefore adds no variant to the session schema and
   carries no per-turn payload — the client renders it from the sim state it
   already holds, and declares that the content is its own responsibility
   rather than the wire's, the same caller discipline `Source::Look`
   established. Zoom stops where the data stops: a map may disclose its own
   resolution but may never invent detail below it.
3. **Co-location is not discovery.** Standing in the area that contains a
   thing is not the same fact as having met the thing, for any feature whose
   extent is not the ground itself. A cell being *visited* (a fact about
   where the possession has physically been, propagating upward from room to
   coarser cell and no further) and a feature being *discovered* (a fact
   about what the possession has actually encountered) are two different
   predicates, and a design that lets one satisfy the other has broken this
   rule regardless of how it got there — including a "fix" for a sparse map
   that lets a visited cell disclose its own undiscovered contents.

## Why together

Clauses 1 and 2 are the load-bearing argument of the campaign's original
spec (§11) and were never in question. Clause 3 was added by amendment,
after 275 commits of `clients/game` history had passed and a reassessment
first proposed replacing the whole design with a dead-reckoned, error-
accumulating map — which Nathan refused *for the player* while ruling that
labels, not geometry, are what discovery gates. Clause 3 depends on clauses
1 and 2 holding: it is only because the map's geometry is complete and
truthful from the first turn (clause 2) and its frame is a fixed, physically
derived fact rather than something that could itself leak information
(clause 1) that "discovery gates the label, never the ground" is a coherent
rule to state at all. A discovery layer built against an incomplete or
world-derived-but-mutable geometry would have had nowhere stable to attach
the distinction.

## Consequences

- The projection's central line and clamp are computed once at world load
  from committed rotation facts and do not move as the player does;
  re-centring is an explicit command, never an ambient behaviour, so the map
  holds still unless asked to move (§3.2).
- The world map is unrepresentable in `vessel/session/v2` by design — no
  schema change, no cross-repo consequence, and H7 (the map costs nothing in
  the ledger) is a direct test of clause 2 holding.
- **Visitedness and discovery are two mechanisms with two different
  propagation rules, and neither may absorb the other.** Visitedness
  propagates upward only (walking a room marks every coarser cell containing
  it, and no sibling); discovery does not propagate at all — encountering
  one feature never discovers a neighbour, however close.
- Terrain-borne landmarks (a volcano's cone, a coastline) and point sites
  (a settlement, a cave mouth) satisfy clause 3 by two different encounter
  conditions, because for the former the ground and the feature are the same
  object and for the latter they are not — the asymmetry is the rule applied
  twice, not a special case.
- A feature is never drawn and then hidden: an undiscovered point site is
  simply not rendered, and an undiscovered landmark renders as terrain,
  unnamed. Suppression-after-render is refused outright, since it is where a
  client would learn to lie about what it knows.
