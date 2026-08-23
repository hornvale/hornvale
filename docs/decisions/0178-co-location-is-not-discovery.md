# 0178. Co-location is not discovery

**Status:** Accepted (2026-08-23) · **Decider:** Nathan · **Relates:**
[0177](0177-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md)
(a map's frame is a fact about the world, and a view of the world is a lens —
split from this record at final review, see "Why separate" below)

In the context of *The Portolan, part II*'s discovery-layer amendment, gating
what a whole-world map NAMES rather than what it draws, we decided that:

## The decision

**Co-location is not discovery.** Standing in the area that contains a thing
is not the same fact as having met the thing, for any feature whose extent
is not the ground itself. Nathan's own framing, carrying no cartography in
it at all:

> "Just being in the area where a thing was buried doesn't imply any
> knowledge of the buried thing any more than going to Paris means you've
> visited the Catacombs or going to southwest Colorado means you've visited
> Mesa Verde."

A cell being **visited** (a fact about where the possession has physically
been, propagating upward from room to coarser cell and no further) and a
feature being **discovered** (a fact about what the possession has actually
encountered) are two different predicates, and a design that lets one
satisfy the other has broken this rule regardless of how it got there —
including a "fix" for a sparse map that lets a visited cell disclose its own
undiscovered contents.

## Why separate

This record was originally clause 3 of
[0177](0177-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md),
bundled on the reasoning that the map's completeness and fixed frame are
what make "discovery gates the label, never the ground" implementable
without suppression machinery. Final review overturned the bundling: this
is a claim about what a possession KNOWS, equally true of hearsay, prose, or
conversation — none of which have a complete geometry or a derived
projection frame at all. Nathan's own framing above carries no cartography.
And the timeline runs the wrong way for a logical dependency: this rule
arrived by amendment WHILE 0177's clause 2 (map completeness) was still
under active reassessment, including a serious proposal to replace it
outright with a dead-reckoned, error-accumulating map — the opposite of one
clause resting on the other's settled ground. Three registry rows
(`PLAY-ruins-have-no-artifact`, `CLIENT-npc-dead-reckoned-map`,
`CLIENT-hearsay-placed-features`) already cite this rule independently of
the map campaign that first stated it.

**The true, narrower relationship kept:** 0177's clause 2 (the map is
geometrically complete and truthful from the first turn) is what makes this
rule IMPLEMENTABLE without a suppression pass — a discovery layer built
against an incomplete or world-derived-but-mutable geometry would have had
nowhere stable to attach the distinction. That is an implementation
dependency, not a logical one, and does not make this a corollary of 0177.

## Consequences

- **Visitedness and discovery are two mechanisms with two different
  propagation rules, and neither may absorb the other.** Visitedness
  propagates upward only (walking a room marks every coarser cell containing
  it, and no sibling); discovery does not propagate at all — encountering
  one feature never discovers a neighbour, however close.
- Terrain-borne landmarks (a volcano's cone, a coastline) and point sites
  (a settlement, a cave mouth) satisfy this rule by two different encounter
  conditions, because for the former the ground and the feature are the same
  object and for the latter they are not — the asymmetry is the rule applied
  twice, not a special case.
- A feature is never drawn and then hidden: an undiscovered point site is
  simply not rendered, and an undiscovered landmark renders as terrain,
  unnamed. Suppression-after-render is refused outright, since it is where a
  client would learn to lie about what it knows.
- Generalizes past any one map: a future consumer of "where has this agent
  been" (hearsay, an NPC's remembered map, a historical record) may never
  treat that fact as knowledge of what was there, only of where the agent
  stood.
