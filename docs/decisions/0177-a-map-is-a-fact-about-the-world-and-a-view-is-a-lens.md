# 0177. A map's frame is a fact about the world, and a view of the world is a lens

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0123](0123-disclose-a-resolution-rather-than-refine-a-field.md) (disclose a
resolution rather than refine a field — the same discipline applied to a
lost cell, here applied to a lost sample), [0142](0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md)
(a lost axis is declared — here a lost *area*, the projection's clamp),
[0178](0178-co-location-is-not-discovery.md) (co-location is not discovery —
split out at final review; depends on this record's clause 2 for an
implementation reason, not a logical one — see that record's own "Why
separate" section)

In the context of *The Portolan, part II* building a whole-planet Mercator
chart, we decided two things together, both load-bearing for §11 of the
campaign's own spec and never in question on their own:

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

## Consequences

- The projection's central line and clamp are computed once at world load
  from committed rotation facts and do not move as the player does;
  re-centring is an explicit command, never an ambient behaviour, so the map
  holds still unless asked to move (§3.2).
- The world map is unrepresentable in `vessel/session/v2` by design — no
  schema change, no cross-repo consequence, and H7 (the map costs nothing in
  the ledger) is a direct test of clause 2 holding.
- Clause 2's completeness (the whole map is drawn, truthfully, at every
  zoom rung) is what [0178](0178-co-location-is-not-discovery.md) depends
  on for its own consequences to be implementable without suppression
  machinery — see that record.
