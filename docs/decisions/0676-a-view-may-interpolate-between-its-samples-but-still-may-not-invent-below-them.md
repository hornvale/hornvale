# 0676. A view may interpolate between its samples, but still may not invent below them

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (autopilot, spec §8.1) ·
**Amends:** [0196](0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md)
clause 2 · **Relates:**
[0121](0121-ordinal-fields-may-band-a-blend-nominal-fields-must-partition.md)
(which field kinds may be blended at all — this record adopts that split
wholesale rather than restating it),
[0287](0287-a-zoom-rung-is-a-mesh-depth.md) (which made 0196's clause
structural rather than policed, and is untouched here),
[The Hachure](../../book/src/chronicle/the-hachure.md)

In the context of *The Hachure* finding the world plate drawing flat
hundred-kilometre slabs at every rung below the terrain mesh, and of the fix
being to read elevation **between** the mesh's samples rather than snapping
to the nearest one, we decided to **amend 0196 clause 2's closing sentence** —
"a map may disclose its own resolution but may never invent detail below it" —
to distinguish two operations it had bundled into one prohibition, accepting
that the sentence as written forbade a class of refinement that asserts
nothing the world does not already hold.

## The decision

Clause 2's closing sentence is replaced by two:

> A map may disclose its own resolution. It may **interpolate** between its
> samples — a refined reading must lie inside the convex hull of the samples
> it is read from, so it can never assert a value its own data do not
> bracket. It may **not invent**: a reading carrying information not derivable
> from those samples requires its own ratification, and is not licensed by
> this clause.

The hull condition is the whole of the permission, and it is checkable rather
than aspirational. Interpolation never leaves the hull of its inputs; where
the consumer bands the result, banding is monotone in the interpolated
quantity, so the band of a blended reading is bracketed by the least and
greatest band of that tile's own corners. That is "coarse constrains fine"
(the Constitution's provider-tier rule) stated for a view instead of a
provider: a refined reading may sharpen what its samples say and may never
contradict them.

**Which fields may be interpolated is not decided here.** 0121 already
settled it — ordinal and continuous fields may band a blend, nominal fields
must partition — and this record neither widens nor narrows it. Elevation is
continuous and may be blended; `WaterKind` is nominal and may not, and the
−29% fresh-water revert 0121 records is what happens when it is.

## Why the amendment is recorded rather than absorbed

0287 made 0196's clause **structural**: a tile *is* a facet at the rung's
depth, so a view could not render finer than its datum even if it wanted to.
Nothing would have failed had this refinement simply shipped — no gate
enforces the sentence, because the ladder's shape enforced it instead. That
is exactly why it is written down. A structural guarantee that quietly
acquires an exception is the shape of a constraint nobody can later find the
edge of, and the campaign that shipped this one nearly called the ratified
behaviour a regression on the strength of implementation comments alone.

## Consequences

- The world plate reads `TileTerrain::height_asl` from a bilinear blend at the
  tile's own facet depth, and
  `every_blended_reading_stays_inside_its_own_samples` is the hull condition
  under test rather than under description.
- **Invention is still prohibited, and the campaign that asked for it stopped
  here.** The Hachure's Stage 3 — a coherent noise field adding detail below
  the mesh, which Nathan authorised as a fidelity tradeoff on 2026-09-02 —
  is deferred to its own campaign (`MAP-coherent-detail-field`). It will need
  a record of its own under the second sentence above; this one does not
  license it.
- 0196 clause 1 (the frame is a fact about the world) is untouched, as is
  0287's ladder in full. No rung moves, no tile is finer than its facet, and
  the twelve pentagon points stay unreachable.
