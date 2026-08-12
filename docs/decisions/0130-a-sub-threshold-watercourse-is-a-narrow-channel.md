# 0130. A sub-threshold watercourse is a narrow channel, not an absent one

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
  level 6, at walk depth (`globe_level + 6` = 12). Rows 1 and 2 count rooms
  against `20·4¹² × land_fraction` = **90,648,246 land rooms**; row 3 is an area
  ratio in steradians. Rooms at one depth are near-equal-area, which is what
  makes the three percentages comparable at all — the per-row denominator is
  stated because they are not the same denominator:

  **THREE DIFFERENT QUANTITIES ANSWER "HOW MUCH WATER DOES A ROOM SEE", AND
  THEY DIFFER BY UP TO 14x.** Every row below is stated as a percentage of the
  same land, so they are directly comparable; each names what is counted, over
  what denominator, and who reads it. Quote the row, never the bare number.

  | # | quantity | what is counted | denominator | before | after | ratio |
  |---|---|---|---|---|---|---|
  | 1 | **containment** | walk-depth rooms some polyline passes through | 90,648,246 land rooms | 0.0845% | **1.237%** | **14.6×** |
  | 2 | **room coverage (census)** | walk-depth rooms whose **centroid** reads `Transverse::Channel` | 90,648,246 land rooms | 0.0284% | **0.1716%** | 6.0× |
  | 3 | **room coverage (integral)** | `channel-land-fraction`: the channel-tube **area** `Σ arc × width` | land area, steradians | 0.0335% | **0.2143%** | 6.4× |

  **Rows 2 and 3 are two estimates of the SAME quantity**, one by counting rooms
  and one by integrating area; they agree to ~25%, the gap being the integral's
  per-line sum double-counting where channels overlap (`windows/lab`'s
  `the_analytic_channel_area_matches_the_sampled_one` brackets exactly this).
  **Row 1 is a different quantity** and is the one a subdivision can act on.
  Row 3 is the shipped census column, so its raw fractions are `3.354e-4` and
  `2.143e-3`; the percentages above are those values, and reading `2.143e-3` as
  though it were row 2 is the specific confusion this table exists to stop.

  Containment scales with `Σ1` — the count of rendered cells — so it tracks the
  ~15x growth in the flow tree almost exactly. Both coverage rows scale with
  `Σ√Q`, and every added reach carries the smallest discharge in the world, so
  they grow only ~6x. All three are correct; a prediction about one is not
  evidence about another. (`RoomAddr` is a **face** of the icosphere and
  `Geosphere::cell_count` is its **dual**, so `20·4^d` is the room count and
  `10·4^L + 2` the cell count — a factor of ~2 apart. Denominators stated
  because that duality has already produced one 2x disagreement between two
  measurements of this.)

- **The gap between row 1 and row 2 is what Tier 2 exists to close, and it
  widened.** Of the rooms a channel passes through (row 1), the share reading
  `Channel` at their centroid (row 2) fell **0.3366 → 0.1387**: **86% of the
  rooms a channel now runs through do not read as water where a walker stands**,
  against 66% before. A headwater half-width is 7.35e-6 rad against a 2.83e-4
  rad walk-depth room edge — **1/38 of a room**. Tier 1 put a channel in
  fourteen times as many rooms and made each one harder to notice; recovering
  the difference is a sub-cell rendering problem, not a width-law one.

  **So the number to recover is row 1's containment, 1.237% — not row 2's
  centroid coverage, 0.1716%, and not row 3's area integral, 0.2143%.**
  The trap, named because this record walked into it: **0.2143% and 0.1716% are
  the same quantity measured two ways**, so quoting the integral where the
  census belongs looks right and is off by 25%, while quoting *either* where
  containment belongs is off by **7x** and sets a subdivision's target an order
  of magnitude low. Say which row. `channel_half_width`'s "What Tier 2
  inherits" section carries the same statement, and the two agree.
