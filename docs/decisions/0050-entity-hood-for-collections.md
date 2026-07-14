# 0050. Entity-hood for collections: collections become entities, singletons stay flat

**Status:** Accepted (2026-07-13) · **Decider:** Nathan

In the context of the sky's ledger growing a full fact surface (The
Self-Describing Sky campaign) and needing a rule for *how* to commit a group
of like things — one star, one anchor world, but several neighbor stars and
several moons — facing the choice between flat parallel-list facts (as the
shipped moon facts already do) and minting a ledger entity per item (as
settlements already do), we decided that **a collection of a kind becomes a
set of ledger entities; a singleton stays a flat fact on the world entity**,
accepting that this campaign leaves an explicit, temporary asymmetry between
neighbors (entities) and moons (still flat).

**Context.** Before this campaign, the ledger had two live precedents
pulling in opposite directions: settlements (many per world) are minted
entities carrying `is-settlement` plus their own facts, discovered by
scanning for the flag; moons (also many per world) are three parallel
Number-fact lists (`moon-period-std`, `moon-tide-rel`,
`moon-inclination-degrees`) associated by position in a deterministic
distance sort. Neighbors were worse than either: a single collapsed
`notable-neighbor` `Value::Text` blob per neighbor, with no queryable
structure inside it at all.

Extending the sky's fact surface forced the choice explicitly. The rule: a
collection whose members can each be usefully addressed on their own —
queried, distinguished, extended with new per-member facts later without
widening every consumer's parsing — is a collection of entities. A quantity
that is inherently singular on a world (one star, one anchor world) has no
such need and stays a flat fact; minting a one-member "collection" would be
ceremony with no payoff. Neighbors, freshly designed from a clean slate,
took the entity form: each neighbor mints an entity carrying `is-neighbor`
(a `Flag`) plus `neighbor-class`, `neighbor-distance-ly`,
`neighbor-brightness-rel`, `neighbor-declination-deg`, and
`neighbor-ra-deg`, discovered exactly like settlements (scan for the flag;
brightest-first order is preserved by mint order).

Moons did not get the same treatment this campaign, and that is a
deliberate, named exception rather than an oversight: promoting them to
entities would have been a second epoch on already-shipped facts
(`moon-period-std`/`moon-tide-rel`/`moon-inclination-degrees`), for a
collection this campaign was not otherwise touching. The three new moon
facts this campaign adds (`moon-mass-lunar`, `moon-distance-mm`,
`moon-angular-size-rel`) extend the existing flat, position-associated
pattern instead, so no epoch was forced where one wasn't already needed. The
debt is explicit: a future campaign may promote moons to entities under
their own epoch, at which point this record's rule is what licenses it.

**The precise `insolation-rel` definition**, recorded here because the
entity-hood question and the new insolation fact shipped in the same
campaign: `insolation-rel` is the top-of-atmosphere stellar flux at the
anchor's orbit, relative to Earth's — `L / a²` with `L` in solar
luminosities and `a` in AU, Earth = 1 by construction. It is a **global,
annual-mean, genesis-time scalar**: it does not carry the seasonal
(obliquity-driven) or deep-time (eccentricity-driven) variation the
existing forcing-parameter facts already model, and the `explain sky`
narrator labels it as such rather than implying more precision than the
number carries. `hornvale_astronomy::insolation_rel(star, anchor)` is the
single function that computes it; both the committed fact and
`windows/worldgen`'s climate input call it, so the two readings cannot
diverge.

**Decision.**

- A collection (multiple like things per world) becomes ledger entities,
  each carrying a discovery flag and its own facts.
- A singleton (one per world) stays a flat fact on the world entity.
- Promoting an already-shipped flat collection to entities is itself an
  epoch (decision
  0039), not a free refactor; it is deferred
  until a campaign has independent reason to touch that collection.
- `insolation-rel` is defined precisely as above wherever it is cited.

**Consequence.** Neighbors and settlements now share one discovery idiom;
moons remain the one flat collection in the sky's fact surface, a named
debt rather than a silent inconsistency. A future moons-to-entities
campaign inherits this record as its licensing decision rather than needing
to re-litigate the entity-vs-flat question from scratch.

**See also.** `docs/superpowers/specs/2026-07-13-the-self-describing-sky-design.md`
§§2, 4, 5, 7; decision
0039 (epochs vs. tiers, the reason
moons were grandfathered rather than migrated in place);
`domains/settlement::genesis` (the entity-discovery idiom neighbors now
share); `book/src/chronicle/the-self-describing-sky.md`.
