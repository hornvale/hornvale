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

- **Query cost, and it is the SHIPPED READ PATH, not only the gate.**
  `ChannelNetwork::nearest_line` is linear in total vertices, so every
  `transverse_at` / `bank_reading` gets ~16x more expensive on the canonical
  grid (883 → 14,606 vertices on seed 42 at level 6). `windows/locale`'s
  `describe` (`lib.rs:820`), `crossing_between` (`:941-942`, twice per query)
  and `transverse_of` (`:1010`) each call `bank_reading` **per room**, so the
  walk and possess loops pay it on every room a player enters — this is a
  player-visible cost, not a test-suite one. An earlier draft of this record
  named only "the gate's channel-reading tests", which was true and
  incomplete; the incomplete form is the more durable error, because nothing
  contradicts it.

  **Nothing bounds it, and nothing can as things stand.** Wall-clock time is
  banned workspace-wide including in test code, so no perf guard can exist to
  catch a regression here; the only visible signal is `make ci`'s duration
  alarm, which is whole-suite and per-test rather than per-query. Measured
  proxy: `hornvale-locale::water_reading` went 1.53 s → 12.27 s. Tiers 2 and 3
  multiply the vertex count again, so a spatial index over `polylines` — a
  bounding-cone or cell-bucket prefilter before the linear scan — is the lever,
  and it should be priced before the next widening rather than after.
- **Fixture size.** `channel-network-seed-42-level-5.txt` goes 79 → 3,890 lines
  (~290 KB). It is the repo's only per-vertex topology witness and re-baselining
  it is what forced the guard order in Task 3: the pre-change witnesses
  (`the_network_renders_every_river_cells_downhill_edge`,
  `the_meander_field_is_pinned`) were written and watched green **before** the
  re-baseline, because a fixture regenerated after a change witnesses the change
  rather than judging it.
- **A larger rendered world at a sub-linear gain in AREA — but not in reach.**
  Two different quantities move at two different rates, and conflating them is
  the trap this bullet exists to close. Measured on seed 42 at the canonical
  level 6, walk-depth rooms (`globe_level + 6` = 12), denominator
  `20·4¹² × land_fraction` = 90,648,246 land rooms:

  | quantity | before | after | ratio |
  |---|---|---|---|
  | **containment** — a walk-depth room some polyline passes through | 0.0845% | **1.237%** | **14.6×** |
  | **centroid reads `Channel`** — what a consumer actually sees | 0.0284% | **0.1716%** | 6.0× |
  | `channel-land-fraction` (the channel-tube area integral) | 3.354e-4 | 2.143e-3 | 6.4× |

  Containment scales with `Σ1` — the count of rendered cells — so it tracks the
  ~15x growth in the flow tree almost exactly. Area scales with `Σ√Q`, and every
  added reach carries the smallest discharge in the world, so it grows only
  ~5.5x. Both are correct; a prediction about one is not evidence about the
  other. (`RoomAddr` is a **face** of the icosphere and `Geosphere::cell_count`
  is its **dual**, so `20·4^d` is the room count and `10·4^L + 2` the cell
  count — a factor of ~2 apart. Denominators stated because that duality has
  already produced one 2x disagreement between two measurements of this.)

- **The gap between those two rows is what Tier 2 exists to close, and it
  widened.** Of the rooms a channel passes through, the share reading `Channel`
  at their centroid fell **0.3366 → 0.1387**: **86% of the rooms a channel now
  runs through do not read as water where a walker stands**, against 66% before.
  A headwater half-width is 7.35e-6 rad against a 2.83e-4 rad walk-depth room
  edge — **1/38 of a room**. Tier 1 put a channel in fourteen times as many
  rooms and made each one harder to notice; recovering the difference is a
  sub-cell rendering problem, not a width-law one, and the number to recover is
  1.237%, not 0.214%.
