# 0072. Derived geometry is causal

**Status:** Accepted (2026-07-25) · **Decider:** Nathan

In the context of a room's fine layer being a set of relations among affordances,
with coordinates *solved* from those relations for display and local movement,
facing the question of whether relations the solver produces incidentally may
affect outcomes, we decided that **derived geometry is causal** — accepting that
the placement algorithm thereby becomes a determinism contract requiring an epoch
to change.

**Context.** The mapping is asymmetric: relational → grid is a solve (many squares
satisfy "near the couch"), grid → relational is a classification. So the round
trip is not the identity — a creature placed near the couch may *also* turn out to
be behind the pillar. Those incidental relations are the emergent payoff:
accidental cover, blocked sightlines, awkward seating, being overheard because of
where one happened to stand. The alternative — outcomes read only *asserted*
relations — would keep layout purely presentational and freely retunable, at the
price of a world whose geometry can never surprise anyone.

**Consequence.** The placement algorithm joins the frozen derivations; retuning it
is an epoch, not a tweak, and the visual tuning pass is no longer free. History is
protected by the existing discipline rather than by a new mechanism: when geometry
matters, the *fact* is committed (a conversation happened; these three could hear
it), so a later epoch cannot retro-change what already happened. **Worlds are
reproducible within an epoch, not across one.** The player-facing corollary is
that across a layout epoch, history survives while remembered places rearrange —
events are durable, rooms are not.

This also ratifies the metaplan's §3.5 on firmer ground than it originally
claimed: the sim adjudicates play not because entities must stand on an
authoritative grid (decision 0069 dissolves that), but because outcomes are the
sim's. A decorative client-side grid remains admissible; an adjudicating one does
not.

**See also.** The Rose Window metaplan §3.5 and Amendment 1 §1a.5; decisions
[0069](0069-fine-position-is-never-serialized.md) and
[0073](0073-epoch-granularity-is-declared.md);
`CLIENT-room-grid-authority` and `CLIENT-relational-fine-layer` in the idea
registry.
