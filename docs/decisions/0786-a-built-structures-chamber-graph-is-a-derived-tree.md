# 0786. A built structure's chamber graph is a derived tree over a structure-band grammar, never drawn

**Status:** Accepted (2026-09-05) · **Decider:** Nathan (G3 / autopilot) ·
**Campaign:** The Cruck · **Relates:**
[0566](0566-a-place-is-a-graph-before-it-is-a-map.md) (the shared pattern
grammar, one band finer),
[0746](0746-a-dwellings-culture-is-a-derived-admission-signature.md) (the
housemark this grammar reads),
[0069](0069-fine-position-is-never-serialized.md) (the chamber interior is
derived), [0666](0666-the-enterability-gate-is-a-site-not-built.md) (which
sites are built at all)

In the context of giving a building a reason for its form, we decided that
**a built structure's chamber graph — how many chambers, what each is for,
and which opens into which — is derived from the brief by one ordered
structure-band inventory walked once**, accepting that the seed keeps only
the freedom the derivation leaves it: which facets the chambers stand at,
and where the walls fall inside the plan.

Before this, the count was drawn uniformly from `1..=4`, the links were
always the path `(0,1),(1,2),(2,3)`, and a chamber's role was read off its
index — so a cold hall and a warm hut with the same draw were the same
corridor with the same rooms in the same order. Now five rules
(threshold, hearthroom, hall, workroom, store) carry an attachment of `Root`
or `Beside(role)`, and the attachment is read as a justified permeability
graph: cold nests rooms on the hearth, authority sets depth, threshold
posture decides who reaches the workroom. A rule whose parent role is absent
is **refused**, never re-attached to the root — the structure band's
admission is a `requires`, not the interior composer's hub fallback.

**Accepted cost.** The count is no longer drawn, so the single-chamber built
dwelling disappears: a hearthroom is always admitted and every built
structure therefore has a fire, where a one-chamber cold dwelling used to
have none. `MAX_CHAMBERS = 4` stays, so the inventory's order is a priority
order and a Seat with a business would keep its hall and drop its store.
The wild path is deliberately **not** given a grammar: for a cave the
derivation runs the other way — the rock made the form and a people reads it
— so a cave keeps its drawn chain by argument rather than by omission, and a
Plat-style reading of that chain is a later campaign's subject.

**See also.** [The Cruck design](../superpowers/specs/2026-09-04-the-cruck-design.md)
§§1, 3, 3.2, 3.5 and [campaign ledger](../superpowers/ledgers/2026-09-04-the-cruck.md)
#1, #2, #4 and #12.
