# 0129. A sub-threshold watercourse is a narrow channel, not an absent one

**Status:** Accepted (2026-08-12) · **Decider:** Nathan · **Relates to:**
[0039](0039-epochs-replace-tiers-refine.md),
[0123](0123-disclose-a-resolution-rather-than-refine-a-field.md),
[0124](0124-a-refinement-preregisters-a-conservation-criterion.md)

In the context of the channel network rendering only the ~6.7% of land cells
that clear `RIVER_MIN_DRAINAGE`, while `downhill` and `drainage` are computed
for **every** land cell, facing the fact that the world builds a complete
space-filling flow tree and draws one fifteenth of it, we decided that
**`channel_half_width` is unconditional in `drainage` and run construction is
gated on being a *reach* — land with a downhill target — not on being a
`WaterKind::River`**, accepting a network roughly 16x larger in vertices, a
`~51x` larger level-5 byte golden, and a proportionally more expensive
`transverse_at` query.

## What is superseded, and why it was right

`channel_half_width` returned exactly `0.0` below `RIVER_MIN_DRAINAGE`,
commented *"a sub-threshold trickle is not a channel."* Every band edge derives
from that half-width, so a sub-threshold line would have read `Dry` at its own
centre and carried no bank, no floodplain and no terrace — a line drawn and
then classified as nothing. Against a network that only ever asked the question
about river cells, the short-circuit was unreachable defensive code that stated
a real invariant: **do not emit a degenerate line**. That is why it was right,
and it is why removing it is safe only in company with the widening. A width law
that returned zero for a rendered reach would be a defect either way.

Tier 1 reverses the *classification*, not the invariant: a creek is a **narrow**
channel. The floor the world can actually produce is one land cell's own runoff
(`drainage` is a land-cell count and every land cell drains at least itself), so
the narrowest reach anywhere is `a·edge·√1`, measured at `7.35e-6` rad at the
canonical level 6 — positive, and about one part in 2,600 of a cell edge.

## Two things this does NOT do

1. **It does not widen `WaterKind::River`.** `water::classify` is untouched and
   `RIVER_MIN_DRAINAGE` still decides what a cell is *named*. A cell may carry a
   rendered watercourse and classify `DryLand`; The Ford already documented
   ~49.6% disagreement between `water_kind == River` and `transverse_at ==
   Channel`, and this deliberately widens it. The two questions — *is there
   water here* and *is this a river* — were always different, and rendering the
   whole flow tree is what makes them visibly so.
2. **It does not add a draw, a stream label, or a field.** Nothing upstream
   changed. The branches already existed and were discarded at render time; this
   is derivation, not invention, and the frozen level-6 band-edge fixture
   (`rill-width-law-seed-42-level-6.txt`, captured before the campaign began)
   stayed green through it, which is the evidence that no width moved.

## What it buys back

Removing the zero-return removes the **one part of the width law that was not
scale-free**. The threshold compares against a count, so a fixed physical
drained area cleared it one level down and not at this one; the law's answer
therefore changed with the grid. It no longer does, at any discharge, and Tier 2
inherits no exception — which matters precisely because Tier 2 subdivides below
cell scale, where a count-valued threshold has no defensible value.

## Costs accepted

- **Query cost.** `ChannelNetwork::nearest_line` is linear in total vertices, so
  every `transverse_at` gets ~16x more expensive on the canonical grid (883 →
  14,606 vertices on seed 42 at level 6). No index was added; if the gate's
  channel-reading tests become the suite's cost centre, that is the lever.
- **Fixture size.** `channel-network-seed-42-level-5.txt` goes 79 → 3,890 lines
  (~290 KB). It is the repo's only per-vertex topology witness and re-baselining
  it is what forced the guard order in Task 3: the pre-change witnesses
  (`the_network_renders_every_river_cells_downhill_edge`,
  `the_meander_field_is_pinned`) were written and watched green **before** the
  re-baseline, because a fixture regenerated after a change witnesses the change
  rather than judging it.
- **A larger rendered world at a sub-linear gain.** Rendering ~15x more cells
  bought only ~5.5x more channel area (`channel-land-fraction` 3.35e-4 → 2.14e-3
  on seed 42), because width goes as `√Q` and the added reaches carry the
  smallest discharges. That is the width law working as designed, and it is why
  the whole flow tree still occupies about a fifth of one percent of the land.
