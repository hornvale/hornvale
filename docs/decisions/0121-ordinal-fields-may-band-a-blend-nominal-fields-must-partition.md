# 0121. An ordinal field may band a blend; a nominal field must take a partition

**Status:** Accepted (2026-08-11) · **Decider:** Nathan · **Relates:**
[0038](0038-identity-computes-on-the-canonical-grid.md),
[0039](0039-epochs-replace-tiers-refine.md),
[0104](0104-a-threshold-must-know-its-variates-distribution.md),
[0105](0105-water-keeps-bands-rock-becomes-a-graph.md)

In the context of *The Grain* asking how a room should get a categorical field
from the coarse cells around it, facing a struct literal that used four
different inheritance policies in adjacent lines and a plausible rule
("categorical fields inherit, continuous fields blend") that its own code
already contradicted, we decided that **the governing property is whether the
field's values are ORDERED**: an *ordinal* field may be banded from a blend of
its continuous underlay, and a *nominal* field must be assigned by a partition
of space and never by thresholding a blend — accepting that a nominal field
therefore stays exactly constant across any view narrower than its partition,
and that the honest response to that flatness is to disclose it
([0123](0123-disclose-a-resolution-rather-than-refine-a-field.md)) rather than
to invent detail.

## Why the obvious rule was wrong

`windows/locale`'s own doc comments describe the split as categorical versus
continuous. The code does not implement that split. `relief` is categorical —
six named bands — and it is computed as `relief_band(blended height_asl_m)`,
one line below `water`, which takes the dominant corner. Both are categories;
they are produced by opposite mechanisms; both are correct.

The distinction that actually separates them:

| | `relief` | `water` |
|---|---|---|
| values | ordered (`abyss < shelf < lowland < …`) | unordered (`Ocean`, `River`, `SaltBasin`, `DryLand`) |
| a blend of the underlay moves a value | at most one band, in the direction the underlay moved | to an arbitrary other category |
| what the operation conserves | the distribution's shape | nothing |

For an ordinal field the band index is a monotone function of the underlay, so
banding a blend is a bounded, order-preserving error — the same property that
makes [0104](0104-a-threshold-must-know-its-variates-distribution.md)'s warp
safe. For a nominal field there is no order to preserve, and the threshold that
produces the category is maximally nonlinear: `classify(blend(x))` is **not** the
area-weighted vote of `classify(x)` over the corners. Nearest-corner assignment
*is* a partition, and a partition conserves area by construction; a threshold on
a blend does not, and what it loses is whichever category occupies the thin
tails of the underlay.

## Measured, on a build that was reverted

Water was banded from blended `Globe::drainage`, reusing the existing
`RIVER_MIN_DRAINAGE` and fitting no new constant (`dd523ab2`). It passed both of
its preregistered hypotheses, passed the commit gate at 3350 tests, and was
reverted (`76068e6a`):

- **Fresh water shrank 29%** at walking depth — River rooms 66 → 47 over a
  4000-point sweep — against a constant whose own doc records that 15.0 "keeps
  rivers the minority landform (~6.7% of seed-42's land)".
- **The loss landed where theory said it would.** 267 of seed 42's 700 River
  cells sit in drainage `[15, 20)`, and an area-weighted mean of a channelized
  statistic dilutes exactly those below threshold. A measurement confirming a
  *predicted* systematic bias is why no retuning rescues the mechanism.
- **Thirst-driven fauna movement halved** (`766 stirred` → `389 stirred`) and 102
  lines of committed affect trace moved. The constitution's *coarse constrains
  fine* says higher fidelity refines, never contradicts, lower — so a refinement
  that shrinks a calibrated coarse quantity is disallowed however good its local
  behaviour looks.

## The decision

1. **Ordinal fields may band a blend.** `relief` stays as it is. A new ordinal
   field may follow it, and its doc says which continuous underlay it bands.
2. **Nominal fields take a partition.** Today that partition is the room's
   **dominant corner** — the greatest of its own three blend weights, tie-broken
   to the lowest `CellId`. `water`, `biome`, `substrate`, the rock the colour
   layer reads, and the cave a room reports all use it, and
   [`dominant_corner`]'s documented invariant that they all name the *same* cell
   is now enforced by a test rather than asserted in a comment.
3. **Biome would stay on the partition even if it were ordinal**, and the reason
   is worth stating separately so it is not read as a special case of this rule:
   averaging *desert* and *tundra* names no biome at all. The classification is a
   lookup over Whittaker bins, not a ramp, so there is no underlay to band even
   in principle.
4. **Flags take the partition too, for a third reason**: a weighted mean of two
   booleans names nothing. `endorheic` and terminal-sink status are read off the
   dominant corner.
5. **A future sub-cell mechanism for a nominal field must be a partition of the
   sub-cell space**, not a re-reading of a coarse field. For water that means a
   flow graph that can put a stream *somewhere in particular* inside a cell —
   registered as `MAP-64`, and named here so the question is closed rather than
   left open.

## Consequences

- **A nominal field is exactly constant below its partition, and that is now a
  documented property rather than an apparent defect.** At walking depth a
  31-room neighbourhood reports one biome and one water kind, and this is
  categorical nearest-neighbour interpolation seen below its own stencil — a fact
  about the view, not about the field. `dominant_corner` is evaluated **per
  room**, which is what makes it interpolation rather than inheritance; the word
  "inherited" was in the source and misled the campaign that read it.
- **A refinement's preregistration needs a conservation criterion**, because
  both of the hypotheses that passed here asked only about local variation. That
  is [0124](0124-a-refinement-preregisters-a-conservation-criterion.md), and the
  conservation test it requires is in
  `windows/locale/src/lib.rs`'s `room_water_is_conserved_when_aggregated_over_a_canonical_cell`.
- **`RIVER_MIN_DRAINAGE` was not touched, and must not be** to change a sub-cell
  rendering. It is calibrated against a documented canonical-level distribution;
  moving it to fix a fine-layer reading is the tail wagging the dog.
- **This does not settle rock**, which [0105](0105-water-keeps-bands-rock-becomes-a-graph.md)
  already routed to a graph rather than to bands. The two records agree: bands
  are for ordered quantities.

## See also

`The Grain` spec §§3, 5 and its [chronicle](../../book/src/chronicle/the-grain.md);
the reverted mechanism at `dd523ab2` and its revert at `76068e6a`; the registry
rows `GRAIN-ordinal-may-band-nominal-must-partition` and
`GRAIN-water-is-a-point-sample`.
