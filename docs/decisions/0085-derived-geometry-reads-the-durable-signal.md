# 0085. Derived geometry reads the durable signal, not the living one

**Status:** Accepted (2026-07-28) · **Decider:** Nathan · **Refines:**
[0072](0072-derived-geometry-is-causal.md)

In the context of deriving a place's physical geometry from what macro history
knows about it, facing a brief whose every descriptive axis describes the
*currently alive* occupation, we decided that **derived geometry reads only
signals that outlive the occupants** — accepting a flatter, less expressive
derivation now in exchange for one that does not contradict itself the moment a
place is abandoned.

**Context.** The Blocking needed to know how big a structure's floor plan is.
The obvious answer was to scale it by what the world already knows: a regional
`Seat` should get a grander hall, a populous community bigger rooms, a
Classical-horizon builder wider spans. Every one of those signals is on the
brief and none of them is stored — they are derived per read.

They are also all properties of the **alive** occupation. `brief_of` fills
`function`, `tech`, `notability` and `people` from the one occupation whose
`ended` is `None`, and leaves them absent otherwise. So a floor area derived
from `notability` makes a building **shrink when its people leave**, and a room
width derived from `tech` makes a hall narrow when its masons die. The masonry
does not move. A shell is the most persistent thing about a place, and those
axes are among the least.

**The decision, stated as the rule rather than as the case.** A derivation that
produces something *physical and persistent* may read only signals that are
themselves persistent. Where the only available signal describes the living
occupation, the honest derivation is the flatter one that omits it.

The Blocking's extent is therefore a pure function of the **chamber count** and
nothing else — no brief field, and no seed draw either, since a coarse
constraint that consumes randomness is another generator. `peak_population` is
excluded for a second and independent reason worth keeping: it already governs
how *many* buildings a settlement has (`structures_of`), so reading it again
here would double-count one signal.

**Why this is better and not merely safer.** Grandeur did not go missing; it
moved to where the pattern language already put it. A hall is grand because it
holds a high seat — because of what the room *contains* — which is the reading
`CLIENT-language-not-catalogue` demands one band up. Expressing the same fact as
floor area would have been the catalogue reading: a number standing in for a
composition.

**The window, and why the rule is written while it is free.** No structure
stands at a ruin today. `structure_at` returns nothing unless `brief.built`, and
`built` is membership in the settlement-territory set — committed
`is-settlement` entities, not ended occupation records. So the shrinking
building is currently **unreachable**, and this record costs nothing to obey.

That is the argument for writing it now rather than later, and it is the same
shape as [0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md)'s
chamber-granularity window: a rule adopted while it is free is a rule; adopted
after the first ruin is reoccupied, it is a migration. The Vestige's residue
makes reoccupation plausible, and the failure mode is quiet — a self-consistent
world in which abandoned buildings are systematically smaller than inhabited
ones, with nothing to make the reader suspect the geometry rather than the
history.

**Consequence — what a future campaign must bring to widen this.** A *durable*
signal about a dead occupation is exactly what the brief deliberately omits: the
ruin signature (`cause`, `ended_by`, the ages). The campaign that adds those
fields may then derive geometry from a place's **peak** rather than its present,
and a great hall's ruin can read as a great hall's footprint. Two banked ideas
belong to that campaign and not to this one:

- **Tech as a material span cap.** A pre-industrial room is as wide as a roof
  beam can span, so `tech` should *bound* a chamber's width rather than reward
  it — neolithic and bronze cannot roof a wide hall, classical can. This is the
  correct model and it is still a living-occupation read until the signature
  exists, so it waits (`CLIENT-span-cap`).
- **A durable extent** proper (`CLIENT-durable-extent`).

**Scope.** This is not confined to floor plans. It applies to any derivation
whose output is meant to persist beyond the occupation that prompted it, which
is why it is recorded as a law rather than as a note on `extent_for`. The
registry governs nothing
(decision [0031](0031-the-frontier-is-published-in-the-book.md)), so the two rows
above are captures, not commitments.

**See also.** [0072](0072-derived-geometry-is-causal.md) (why geometry is causal
at all, which is what makes a contradictory derivation expensive rather than
cosmetic); [0084](0084-an-epoch-is-declared-only-when-a-derivation-moved.md)
(the sibling window);
[The Blocking spec](https://github.com/hornvale/hornvale/blob/main/docs/superpowers/specs/2026-07-28-the-blocking-design.md)
§3.4; [The Blocking chronicle](../../book/src/chronicle/the-blocking.md).

Ratified at *The Blocking*'s merge gate.
