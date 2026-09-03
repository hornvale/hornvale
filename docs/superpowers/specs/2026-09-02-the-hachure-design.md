# The Hachure — sub-vertex detail in the world plate

**Status**: design, awaiting G3 review
**Date**: 2026-09-02
**Campaign**: the-hachure

Hachures are the cartographic technique for rendering relief with fine
strokes. This campaign is about the strokes: making the world plate resolve
detail at and below the terrain mesh's own scale, instead of magnifying a
handful of samples into flat slabs.

---

## 1. The problem, as measured

Opening the game shows a screen of three or four flat colour blocks, with
water glyphs painted across dry land. Both are one defect.

All figures below are measured, seed 42, on `e5fc0e920`. The harness is
`clients/game/bin/examples/rung_detail_probe.rs` and
`windows/locale/examples/micro_coherence_probe.rs`, carried on this branch.

`Driver::start` sets `window.depth = BAND_B_RUNG` (13) — `driver.rs:828` —
and `enter_map` does not change it, so the default view *and* the map both
open at the finest rung on the ladder. At rung 13 the virtual chart is
32,768 × 32,661 tiles. Terrain is defined on the `Geosphere`'s vertices,
which at `GLOBE_LEVEL = 6` number 40,962 — a minimum edge of `0.0172992`
rad, i.e. **363 samples around a great circle, ~110 km apart**.

Distinct terrain vertices addressed by a 120 × 40 plate, centred on a river
vertex:

| rung | chart | distinct vertices on screen |
|---|---|---|
| 6 | 256 × 255 | 2140 |
| 9 | 2048 × 2041 | 47 |
| 11 | 8192 × 8165 | 5 |
| **13 (default)** | 32,768 × 32,661 | **1** |

At the default rung the entire screen is one terrain vertex. That is the
"three colours".

`terrain_at_tile` (`plate.rs:1482`) resolves a tile by taking
`facet.ancestor(grid_level)` and then the **nearest corner vertex** of that
quad. Both elevation and water class are therefore piecewise-constant over
the vertex's whole footprint. Since `WaterKind` is a per-vertex nominal
field, a river — a one-vertex feature — is magnified into a slab: at rung 11,
3,340 of 4,800 tiles drew `"`; at rung 13, 4,800 of 4,800. That is the
"water on land tiles", and it is the same defect seen through a categorical
field instead of a continuous one.

## 2. Root cause: the default rung is seven rungs below the data

**A correction, stated first because an earlier draft of this spec got it
backwards.** That draft claimed the chart's width was "tuned to the wrong
lattice" and called it "a regression with a paper trail". That is **false**,
and acting on it would have broken a ratified invariant.

`plate::virtual_dims` derives chart width from `base_facet_arc_rad()` =
`FRAC_PI_2`, the cube's quarter turn, giving `4·2^depth` tiles around a great
circle. That is deliberate and ratified: **decision 0287** holds that *a zoom
rung is a refinement depth of the facet tree* — "a tile at rung `d` is a facet
at depth `d`". A tile **is** a facet, by decision. Re-deriving the chart's
width from the vertex lattice would make a tile no longer a facet and would
forfeit the three structural properties 0287 buys: a tile can never be finer
than its datum, the projection frame never re-pegs (so MAP-70's shimmer
cannot occur), and sampling runs lat/lon → facet with no lattice traversal
(so the base-face seams and the twelve pentagon points are unreachable).

What is true, and is the actual root cause:

**`Driver::start` sets the default rung to `BAND_B_RUNG`, the FINEST rung on
the ladder** (`driver.rs:828`), and `enter_map` does not reset it
(`driver.rs:1531`). The ladder has seven rungs, globe level 6 up to band B,
and the client opens at the far end of it. Terrain is defined at globe level
6 and `terrain_at_tile` resolves every rung through
`facet.ancestor(grid_level)`, so the datum shown is always the grid's. Seven
rungs of magnification over one datum is what puts a single vertex on the
whole screen.

**This flatness is therefore the ratified behaviour, not a defect in it.**
Decision 0196 clause: a view "may render coarser than the world's resolution
but **may never invent detail below it**." 0287 makes that structural rather
than policed. The picture is exactly what those two decisions specify. What
the report is really asking is for that policy to change — which §4.4 does
explicitly, as an amendment, rather than by accident.

**A real, smaller finding survives the correction.** The two lattices *are*
incommensurate:

- `Geosphere` — subdivided **icosahedron** (`kernel/src/geosphere.rs`),
  `10·4^L + 2` vertices. **Terrain data lives here.** At L6: 40,962 vertices,
  min edge `0.0172992` rad = **363 around a great circle**.
- `Facet` — tangent-warped **cube-sphere** quads (`kernel/src/room.rs`).
  **Rungs and tiles live here.** At depth 6: **256 around a great circle**.

So rung 6 carries 256 tiles against 363 vertex samples and genuinely
**undersamples the data by 1.4188×**. 0287's "each rung is a real data
resolution" assumed the two lattices agreed at the shared depth; they do not.
The 0287-compatible consequence is not to re-tune the chart but to note that
**rung 7 (512 tiles) is the coarsest rung that does not undersample** — which
is a statement about which rung to open at, not about what a tile is.

**Two documentation drifts found while checking this**, both worth fixing in
passing:

- `book/src/reference/lexicon-of-place.md` describes a facet as "one
  **triangular** face" whose "corners are three vertices". Since The Pavement
  facets are cube-sphere **quads** and `Facet::corner_weights` returns
  **four** corners.
- Decision 0287 and the lexicon both say band B is depth **12**;
  `plate.rs:204` says `BAND_B_RUNG = 13`.

## 3. Constraints inherited

These are settled and this design does not reopen them.

- **Decision 0121** (`GRAIN-ordinal-may-band-nominal-must-partition`) —
  whether a field's values are *ordered* decides how it may be refined.
  Ordinal fields (elevation, relief band) may band a blend: a blend moves an
  ordinal value at most one band and conserves the distribution's shape.
  Nominal fields (`WaterKind`) must **partition**: thresholding a blend
  deletes a category, measured at −29% fresh water on the build that was
  reverted.
- **Decision 0123** (`GRAIN-disclose-resolution-not-refine-it`) — where a
  view is finer than the model behind a field, the document says so rather
  than the field inventing detail. Constrains what may be *claimed*, not what
  may be *drawn*: see §6.
- **Decision 0124** (`GRAIN-local-hypotheses-miss-global-invariants`) — a
  refinement needs a **conservation criterion** beside its variation
  criteria. Two local hypotheses passed on a mechanism that was nonetheless
  illegal, because the violated property was global.
- **The Ford** (`MAP-ford-subcell-water`, shipped) — river-as-area is a type
  error. Rivers are carried as polylines with a discharge-derived width, and
  the signed distance to one bands into channel / bank / floodplain /
  terrace. Its keystone: *a polyline with a width function is the same object
  at 110 km and at 27 m*, so coarse-constrains-fine is satisfied vacuously.
  `GeneratedTerrain::transverse_at(position)` is the public query.
- **Decision 0002 / the dependency allowlist** — `serde`, `serde_json`,
  `libm` only. Any noise is ours to write, in `kernel/src/noise.rs`.
- **Decision 0033** — quantize at emit only, never in the compute path.
- **`CLIENT-map-is-invitation-not-data-dump`** (Nathan's brief, raw) — the
  map is an invitation, not a data dump. The cursor carries the detail, not
  the glyph. Colour is the expressive channel.

## 4. Design

Four stages, ordered so each is independently shippable and each earlier one
de-risks the next.

### Stage 0 — open the map at a rung where terrain resolves

`virtual_dims` is **not touched**. A tile stays a facet (decision 0287).

The map's entry rung stops being "whatever the walk band left in
`window.depth`" and becomes an explicit choice: **rung 7**, the coarsest rung
that does not undersample the vertex lattice (512 tiles against 363 vertex
samples). `enter_map` sets it; leaving the map restores band B for the walker,
exactly as `leave_the_map` already does.

This is a handful of lines and it is the single highest-value change in the
campaign, because it is the one that actually answers the report: the default
view stops being seven rungs of magnification over one datum.

**It is fully compatible with 0196 and 0287** — nothing is invented, nothing
is re-tuned, the ladder is unchanged. It only stops *opening* at the end of
it.

Open question for review: whether the walk band's own default view should
also move. It is the same `window.depth` field, and §7 keeps "what the walk
view should be" out of scope, so the conservative choice is to move only the
map's entry rung and leave the walker at band B.

**AS BUILT (2026-09-03), Stage 0 is two changes, not one.** Moving the entry
rung removed the `@` from the opening view, because
`compose_perception_layer` returned early unless `at_walk_band_rung()` — which
is decision 0293's own documented defect ("no `@`… the picture and the prose
described different places"). So off band B the overlay now paints the
OBSERVER ALONE: a mark on a merged tile claims a placement a coarse rung
cannot resolve, while "you are in this tile" is true at every rung. See
ledger #12-#14.

**And Stage 0 exposed a defect it deliberately does not fix**: zoom is
cursor-anchored (0292) and each rung doubles the chart, so climbing seven
rungs compounds sub-tile rounding by 128x and loses the observer. Nothing had
ever zoomed seven consecutive rungs, because `enter_map` used to land on band
B directly. Ledger #15; needs a registry row.

### Stage 1 — blend elevation at the tile's own facet depth

Replace the nearest-corner snap with the bilinear blend `windows/locale`
already uses for its continuous fields: `Facet::corner_weights` at the
tile's **own** depth (not the grid-level ancestor), exact integer weights,
no float drift, no transcendental.

Measured effect at rung 11: `^` tiles 461 → 1212, with a smooth gradient
boundary replacing a hard block edge. Measured conservation at rung 6: ocean
area **−0.2%** (the coastline barely moves); land redistribution is `,`→`;`
and a new `` ` `` band, both **one-band ordinal moves**, which 0121 permits
explicitly.

Water class is untouched in this stage — it stays a dominant-corner
partition. Stage 1 alone fixes the flat slabs at rungs 6–11.

### Stage 2 — rivers from the flow graph, as lines

The map stops reading `water_kind_at(vertex)` for rivers and starts reading
the polyline carrier The Ford already built.

**Two candidate rules are already falsified and are recorded here so they are
not re-proposed.** Both were measured at rung 6 against today's raster:

| candidate rule | river tiles at rung 6 | verdict |
|---|---|---|
| `Transverse::Channel` at the tile's position | 154 → 6 (**−96.1%**) | rejected |
| within half a chart tile of a channel line | 154 → 2284 (**+1383%**) | rejected |

The first fails because `Transverse::Channel` is the true hydraulic channel
width (metres), and at rung 6 a chart tile is ~110 km, so almost no tile
centre lands inside a channel. The second fails because half a rung-6 tile
is ~55 km and most land is within 55 km of *some* channel.

**The rule is a segment-versus-tile-footprint intersection**, not a radius
test: a tile is a river tile if any channel segment crosses its footprint,
with the discharge-derived width taking over once tiles are narrower than the
channel. This is scale-continuous by construction and needs no crossover
constant.

**The conservation criterion is channel LENGTH and network CONNECTIVITY, not
raster area.** This is the correction that matters. Area is the wrong
invariant for a line — the raster area of a polyline is resolution-dependent
by construction, which is why both falsified rules produced alarming
percentages that mean nothing. Length is resolution-invariant. Today's
rung-6 `"` tiles are additionally *disconnected* clumps, so the current
picture is not a baseline to conserve.

### Stage 3 — one coherent detail field, many consumers

Below the mesh's ~110 km floor, detail comes from a **position-sampled
coherent noise field**, and the same field is read by both the map and the
room prose.

**One draw per discrete dimension.** Each axis — relief, aspect, wetness,
openness — gets its **own** field with its **own** `stream_labels!` entry.
Never one field feeding two axes; never an offset sample off a neighbouring
field (an offset stays correlated unless the offset is large relative to
feature size, a trap that passes a unit test and fails a map). FBM octaves
*within* one field are structure inside one dimension and are fine.

The failure this rule prevents is not cosmetic: if relief and wetness shared
a field, every hollow would be damp and every rise dry, everywhere, forever
— which reads as a law of physics nobody put in the sim.

**One dimension, many consumers, is the goal and is the opposite thing.**
`micro.relief` and the map's rendered height perturbation read the *same*
relief field, so "you are standing in a hollow" and the dip visible on the
map are one hollow. Built independently they would be two unrelated
inventions that contradict each other, each locally "right".

**Only the height is perturbed.** Per 0121, elevation is ordinal and may take
noise; `WaterKind` is nominal and may not. The coastline crenellates for free
as a consequence, because the coast is where the noised height crosses sea
level — no separate coastline work, and the legal path is also the
better-looking one.

**Noise primitive.** `kernel/src/noise.rs` has bilinear value noise and
`Fbm`, both **2D**. Sampling 2D noise by lat/lon pinches at the poles and
seams at the antimeridian, so this needs a 3D variant sampled on the unit
sphere position — a small kernel addition inheriting the existing
determinism tests, no new dependency. Value-noise FBM is adequate for
cosmetic detail at this scale; Simplex's advantages are in higher dimensions
and directional artifacts we will not see here.

## 5. Determinism and save format

**Stages 0–2 and the map-side noise are not an epoch.**
`domains/terrain/src/streams.rs` already carries the precedent:

```
COAST_RENDER = "coast-render" => "render-lens coastline noise (hash-noise only; no stream draws)"
```

A position-sampled hash-noise field consumes no sequential `Stream` draws, so
adding one perturbs nothing any existing world drew. New fields follow that
convention exactly, each with its own label.

**Stage 3's `micro_field` change IS an epoch, and it is the one item needing
explicit sign-off.** `micro.wetness` is emitted — the gallery, three vessel
snapshots, two game-core fixtures — and the four-axis draw order off
`LOCALE_MICRO` is a documented save-format contract (`micro.rs:110-131`).
Replacing four sequential draws with four position-sampled fields changes
every world ever generated.

Per repository convention, this is an **epoch suffix, never a rename**:
`LOCALE_MICRO` is retired (recorded in the streams module's retired list, as
`terrain`'s `"plate-kind"` already is) and four new labels are minted. Worlds
written before the flip are regenerated from seed and pins.

A second, real gain the epoch buys: the current four axes are sequential
draws off one stream, so they are independent but **ordered** — adding a
fifth axis today would shift the existing four. Four separately-labelled
fields make each axis independently seeded, permanently.

## 6. Preregistered measurement

Frozen before the code, per decision 0016. Each stage carries a **variation**
criterion and a **conservation** criterion; 0124 exists because the second is
the one that gets forgotten.

**H1 (Stage 0).** Opening the map at rung 7 strictly increases distinct
terrain vertices addressed by a fixed plate, against the measured rung-13
baseline of **1**.
*Conservation*: none required — nothing is re-derived, re-tuned or invented;
the same `virtual_dims` and the same `terrain_at_tile` run at a different
rung. This is the one stage with no conservation arm, and the reason is that
it changes no field.

**H2 (Stage 1).** Bilinear blending strictly increases distinct rendered
bands per plate at rungs 6–11.
*Conservation*: ocean fraction within 1% of today (measured: −0.2%); no land
tile moves more than one relief band relative to the nearest-corner answer.

**H3 (Stage 2).** Segment-intersection river rendering produces a
**connected** channel network at every rung 6–13, where today's rung-6
raster is disconnected.
*Conservation*: total channel length within 5% across rungs 6–13 — a
resolution-invariant quantity, unlike raster area. Cross-checked against the
census's `channel-land-fraction` at the matched rung.

**H4 (Stage 3).** The coherent field's lag-1 spatial autocorrelation over
adjacent rooms exceeds 0.5 on every axis. Today's measured baseline is
**−0.0109 east–west and +0.0477 north–south** — statistically
indistinguishable from white noise, which is why the existing per-room detail
renders as static rather than terrain.
*Conservation*: the field is zero-mean, and ocean fraction, channel length,
and the relief-band histogram over the whole globe all stay within tolerance
of the committed census. **This is the criterion that discharges the
"micro details are too small to affect macro" assumption rather than
assuming it** — 0124 records that exact assumption passing two local
hypotheses while being false.

**H5 (Stage 3, prose).** `micro.relief` and the map's height perturbation
agree in sign at every sampled room — one hollow, not two.

A falsified prediction is a finding, not a failure. In particular, if H4's
conservation arm fails, the noise amplitude is reduced or the stage is
dropped; the constant is not retuned after unblinding to rescue it.

## 7. Out of scope

- **`MAP-49` / `MAP-erosion-style`** — gradient-aligned erosion filtering as
  a render style. Elaborated, deferred, its own campaign. Stage 3's field is
  deliberately the simpler thing.
- **`MAP-subcell-hydrology`** — rejected on arithmetic (0121's neighbour
  row); The Ford dissolved the need by changing the carrier. Stage 2 uses
  that carrier and does not refine flow accumulation.
- **The walk band's own renderer.** This campaign makes the plate correct at
  band-B rung; whether the walk view should be a plate at all is a separate
  question.
- **Ladder ceiling.** Not changed. Stage 0–3 make the existing rungs honest
  rather than removing them.

## 8. Decisions needing ratification

1. **Amend decision 0196's "may never invent detail below it".** Stages 1
   and 3 both render below the datum's resolution — Stage 1 by interpolation
   (already sanctioned for continuous fields by 0121, which post-dates and
   qualifies 0196), Stage 3 by a bounded coherent noise field. 0287 made
   0196's clause *structural* rather than policed, so changing it is a
   deliberate amendment and must be recorded as one, not absorbed silently.
   **This is the constitutional item; the epoch below is merely expensive.**
2. **The conserved quantity of a line-carried feature is its length, not its
   rasterized area.** Generalizes The Ford's type-error ruling into a
   measurement rule, and would have prevented both falsified Stage-2 rules.
3. **One noise field per discrete dimension, each with its own stream
   label.** Nathan's ruling, 2026-09-02.
4. **`LOCALE_MICRO` epoch** — requires explicit sign-off.
5. **Documentation drift** (§2): the lexicon's triangular-facet description,
   and the band-B depth disagreement between 0287/the lexicon (12) and
   `plate.rs` (13). Neither is this campaign's to decide, but both are this
   campaign's to report.
