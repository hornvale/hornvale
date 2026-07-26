# 0076. The scene protocol's situated pole is egocentric and knowledge-limited

**Status:** Accepted (2026-07-26) · **Decider:** Nathan · **Extends:**
[0022](0022-sim-emits-data-clients-render.md),
[0055](0055-external-clients-consume-a-versioned-wasm-catalog.md)

In the context of building the scene protocol's long-named, never-built
*situated pole*, facing the question of what distinguishes it from the
cartographic poles already shipped, we decided that **a situated scene is
egocentric and carries the observer's epistemic state**, accepting that it
is therefore not a general-purpose view of ground and cannot be reused as
one.

**Context.** The rendering-strategy spec named two poles — cartographic (a
tile lattice over the globe) and situated (an observer at a place and time) —
and five scene kinds shipped on the cartographic and temporal side before any
on the situated one. When the situated pole finally had a consumer, the
tempting shape was "a tile lattice, but small and centred on you", which would
have made it a sixth cartographic kind wearing a different name.

Two things actually distinguish it, and both were load-bearing enough to
settle here.

**It is egocentric, and that is what makes it non-redundant.** Negating the
property gives an allocentric north-up regional chart — which is already
shipped twice, as the atlas and as the Orrery's Map rung. A situated document
is addressed *from* an observer: coordinates are relative, and the centre is a
room rather than a tile.

**It carries epistemic state, which a cartographic document has no use for.**
A tile lattice describes ground uniformly because the ground does not care who
is looking. A situated document does not: each cell reports whether it is the
observer's own room, sensed from where they stand, or merely remembered — and
the grain of detail it carries follows from that state. A producer with no
session emits only the first two; `remembered` is written by a consumer that
owns a possession. The schema carries the whole vocabulary so both producers
speak it, not because every producer can use all of it.

**Consequence.** The situated pole is a distinct schema family, not a
parameterisation of the cartographic one, and future situated kinds (a sky
from a point, a social neighbourhood) inherit this shape rather than the tile
lattice's. The cost is that a client wanting plain ground around a coordinate
should use `scene/tiles-region/v1`, not this; the benefit is that "what does
this observer know" is expressible at all, which the cartographic poles cannot
say. Per 0055 the schema is a cross-repo contract the moment a second repo
parses it: additive-or-versioned only.

Ratified at *The Purview*'s merge gate, closing the "decisions to promote"
item its spec flagged in advance.
