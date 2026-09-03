# Decision ledger — The Hachure (2026-09-02)

Campaign-autopilot ledger. Entries are written as rulings occur; the G3
package (spec review) digests everything up to that stop.

---

#1 [Q] — **Which client and which view is the reported defect in?**
· Decision: the terminal game's world plate (`clients/game/bin/src/plate.rs`),
at `Driver::start`'s default `window.depth = BAND_B_RUNG` — which is both the
walk-band default view and what `enter_map` opens, since `enter_map` does not
reset depth.
· Why: read from source (`driver.rs:828`, `driver.rs:1531`) and confirmed by
reproducing the reported picture exactly with a probe.
· Alternatives discarded: `clients/atlas`, `clients/vessel` — neither has a
"default zoom" the report matches.
· ideonomy passes: 0 (backfilled — a factual localization, not a design
choice; no pass was run and none was warranted).
· Capture: none needed.

#2 [G1] — **What should the client draw at rungs finer than its data?**
· Decision: adopt "draw the vector features" (approach B) over "truth in
zoom" (A, clamp only) or "procedural refinement" (C) as the *initial*
approach; C subsequently promoted to Stage 3 by Nathan (see #6).
· Why: A leaves the default screen sparse and fixes nothing the reporter
asked for; C was a fidelity carve-out not mine to take unilaterally. B draws
only what the sim already knows.
· Alternatives discarded: A (insufficient), C (deferred pending Nathan —
subsequently authorized).
· ideonomy passes: 1 / overturns: 1 (material reframe, not a reversal).
Tuple: dimension-identification + abstraction-lift, list organon, prompts
size / predictability / intentionality. The lift produced the reframe the
whole design now rests on: *a renderer reconstructing a sampled field above
its Nyquist limit*, whose cartographic answer is not "interpolate harder" but
"change what you draw" — large-scale maps draw rivers as **lines**, not as a
per-cell raster class. Restated: the defect is not that the raster is coarse,
it is that **a linear feature is stored as a raster class, and magnifying a
raster class turns a line into a slab.** The `size` prompt additionally
established that the right rule is a function of cells-per-vertex rather than
a constant — carried into Stage 2's scale-continuous formulation.
· Capture: reframe recorded in spec §2 and §4.2.

#3 [Q] — **Is refining the plate's water field relitigating a closed
question?**
· Decision: no. `GRAIN-water-is-a-point-sample` rejected two cures
(`micro.wetness` address noise; blended-drainage thresholding, reverted at
−29% fresh water) and explicitly names the flow graph as the accepted route
for sub-cell water. `MAP-ford-subcell-water` shipped that flow graph. The
world plate simply never became a consumer.
· Why: read the full `GRAIN-*` family and `MAP-ford-subcell-water` in
`book/src/frontier/idea-registry.md` before proposing anything, per the
standing "do not relitigate without new information" rule.
· Alternatives discarded: proceeding without reading the registry — which
would have re-proposed a rejected cure.
· ideonomy passes: 0 (backfilled — a precedent lookup with an unambiguous
answer; the answer changed the plan's framing, not its direction).
· Capture: constraints recorded in spec §3; the two rejected cures recorded
in spec §4.2 so they are not re-proposed.

#4 [G2] — **Conservation criterion for the Stage-2 water refinement.**
· Decision: conserve **channel length and network connectivity**, not raster
area.
· Why: this ruling was forced by measurement, not reasoning. Two candidate
rules were implemented and measured at rung 6 against today's raster —
`Transverse::Channel` gave **−96.1%** river cells and a half-cell radius test
gave **+1383%**. Both numbers are alarming and both are meaningless: the
raster area of a polyline is resolution-dependent by construction. Area is
the wrong invariant for a line, which is The Ford's own type-error ruling
carried into the measurement. I had reached for area only because the −29%
revert was measured in it.
· Alternatives discarded: raster-area conservation (wrong invariant);
conserving against today's rung-6 raster (which is additionally
*disconnected*, so not a baseline worth preserving).
· ideonomy passes: 1 / overturns: 1 — the `predictability` axis, re-applied
to "what does this refinement conserve", is what surfaced that the two
falsified rules shared a bad denominator rather than a bad numerator.
· Capture: both falsified rules recorded in spec §4.2 with their measured
numbers; promoted to a ratification candidate (spec §8.2).

#5 [Q] — **What is the terrain mesh's actual resolution?**
· **SUPERSEDED IN PART BY #11** — the measurement below stands; the
"Consequence" paragraph's conclusion (that the chart is tuned to the wrong
lattice, and that Stage 0 should re-derive its width) is retracted as
contrary to decision 0287. Left in place as the contemporaneous record.
· Decision: 40,962 vertices at `GLOBE_LEVEL = 6`, min edge `0.0172992` rad —
**363 samples around a great circle, ~110 km**. My earlier figure of 256 was
wrong.
· Why: **Nathan challenged the number and was right.** 256 is the *chart's*
width, which `virtual_dims` derives from the cube-sphere **facet** lattice
(`FRAC_PI_2 / 2^depth`); terrain lives on the icosphere **vertex** lattice
(`Geosphere` = subdivided icosahedron, `10·4^L + 2`). I read the chart's own
derivation and mistook it for the data's resolution. Verified by running
`Geosphere::new(6).vertex_count()` rather than by re-reading the code.
· Consequence, and the reason this entry matters beyond the correction: the
two lattices are **incommensurate**, and the chart is tuned to the wrong one.
At rung 6 the chart is 256 cells against 363 vertices — **1.4188× coarser
than the data**, i.e. the zoomed-out map discards ~40% of the detail it
already holds. This is Stage 0, and it did not exist in the plan before the
challenge.
· Alternatives discarded: none — this was a factual error, corrected.
· ideonomy passes: 0 (a measurement, not a design choice).
· Capture: spec §2 and Stage 0; ratification candidate (spec §8.1), because
the stale "363×362" comments in `plate.rs` would otherwise invite someone to
re-revert it.
· **Process lesson for the retrospective**: I stated a resolution figure
derived from the consumer's arithmetic rather than measured from the
producer. `evaluate-the-curve-not-the-constant` in its exact form — the
number was two function calls away the whole time.

#6 [Q] — **May sub-mesh detail be invented (procedural noise), or must the
map draw only what the sim knows?**
· Decision: invented detail is authorized, as Stage 3, with a conservation
criterion.
· Why: **Nathan's call, and it is a fidelity tradeoff, which the autopilot
carve-out list reserves to him.** His rationale: local details below ~100 km
are micro enough not to have dramatic macro effects. Recorded as given.
· The one caution attached, and it is not a hedge: decision 0124
(`GRAIN-local-hypotheses-miss-global-invariants`) records *that exact
assumption* passing two local hypotheses while being false — the violated
property was global (conservation of a calibrated coarse statistic) and no
local hypothesis asks about it. So the assumption is not rejected, it is
**discharged by measurement**: H4's conservation arm (spec §6) checks ocean
fraction, channel length and the relief-band histogram against the committed
census, with a zero-mean field.
· Alternatives discarded: drawing only sim-known detail (Nathan overruled,
with reasons); deferring C to an idea-registry row (my prior recommendation).
· ideonomy passes: 1 / overturns: 0 — enrichment only. The `intentionality`
axis surfaced that Stage 3's field and Stage 1's blend are the same
dimension at two layers, which is what produced the unification in #7.
· Capture: spec §4.4, §6 (H4).

#7 [G2] — **Should the map's height perturbation and `micro.relief` be one
field or two?**
· Decision: **one field, two consumers.**
· Why: built independently they are two unrelated inventions that contradict
each other while each is locally "right" — the map says ridge, the prose says
hollow. One field makes "you are standing in a hollow" and the dip visible on
the map the same hollow, and makes the epoch worth paying once.
· Alternatives discarded: independent fields (contradiction); deriving one
from the other by offset (still correlated — see #8).
· ideonomy passes: 1 / overturns: 0.
· Capture: spec §4.4, §6 (H5).

#8 [Q] — **How are the noise dimensions separated?**
· Decision: **one draw per discrete dimension**, each axis its own field with
its own `stream_labels!` entry. Never one field feeding two axes; never an
offset sample off a neighbouring field. FBM octaves within one field are
fine.
· Why: **Nathan's ruling, 2026-09-02**, stated as a hard constraint. The
failure it prevents is worse than noise: shared fields would make every
hollow damp and every rise dry, everywhere — which reads as a law of physics
nobody put in the sim.
· Note recorded alongside: the current four axes are sequential draws off one
`LOCALE_MICRO` stream, so they are independent but **ordered** — adding a
fifth axis today shifts the existing four. Separately-labelled fields fix
that permanently, which is a real gain the epoch buys beyond coherence.
· Alternatives discarded: offset sampling off one field (correlated unless
the offset is large relative to feature size — a trap that passes a unit test
and fails a map).
· ideonomy passes: 0 (backfilled — Nathan stated this as a ruling, not a
question put to me; no pass was run).
· Capture: spec §4.4, ratification candidate (spec §8.3).

#9 [G2] — **Epoch scope.**
· Decision: Stages 0–2 and the map-side noise are **not** an epoch. Only
Stage 3's `micro_field` change is.
· Why: `domains/terrain/src/streams.rs` already carries `COAST_RENDER`
("render-lens coastline noise (hash-noise only; no stream draws)"). A
position-sampled hash-noise field consumes no sequential `Stream` draws, so
adding one perturbs nothing any existing world drew. This narrowed the epoch
from "everything" to one struct in `windows/locale`, and I had it wrong in
the earlier framing.
· Alternatives discarded: treating the whole campaign as an epoch
(over-broad, and would have made Stages 0–2 far more expensive than they are).
· ideonomy passes: 0 (backfilled — settled by reading an existing convention).
· Capture: spec §5; the epoch is spec §8.4, the one G3 sign-off item.

#10 [G2] — **Noise primitive.**
· Decision: a **3D** value-noise/FBM variant sampled on the unit-sphere
position, added to `kernel/src/noise.rs`. Not Simplex, not a crate.
· Why: the dependency allowlist (decision 0002) admits `serde`,
`serde_json`, `libm` only, so any noise is ours to write regardless.
`kernel/src/noise.rs`'s existing `value_noise_2d`/`Fbm` are **2D**, and
sampling 2D noise by lat/lon pinches at the poles and seams at the
antimeridian — so the existing primitive cannot be used as-is even though it
exists. Value-noise FBM is adequate for cosmetic detail at this scale;
Simplex's advantages are in higher dimensions and directional artifacts not
visible here.
· Alternatives discarded: 2D noise sampled by lat/lon (pole/seam artifacts);
2D noise per cube face (discontinuous at face edges); an external Simplex
crate (allowlist).
· ideonomy passes: 0 (backfilled — a constraint lookup with one surviving
option).
· Capture: spec §4.4.

#11 [G2] — **Stage 0 as first specified was ILLEGAL, and the §2 root-cause
claim was false. Both are retracted.**
· What the first draft said: that `virtual_dims` deriving chart width from
the cube-sphere facet lattice was "a regression with a paper trail", and that
Stage 0 should re-derive it from the icosphere vertex lattice.
· What is actually true: **decision 0287** (Accepted 2026-08-27, Nathan)
ratifies that *a zoom rung is a refinement depth of the facet tree* — "a tile
at rung `d` is a facet at depth `d`". A tile **is** a facet, by decision.
Re-deriving the width from the vertex lattice would break that and forfeit
the three structural properties 0287 names: no tile finer than its datum, no
frame re-peg (MAP-70 shimmer), and no lattice traversal (so base-face seams
and the twelve pentagon points stay unreachable).
· And the deeper one: **decision 0196** holds that a view "may render coarser
than the world's resolution but may never invent detail below it", which 0287
makes structural rather than policed. **So the flat slabs are the ratified
behaviour, not a defect in it.** The campaign is asking to change that
policy, which is a constitutional amendment and now leads the ratification
list (spec §8.1) ahead of the epoch.
· How it was caught: not by review of the spec. The **commit gate refused the
commit** — `lexicon_guard::no_vertex_sense_cell_comes_back`, a ratchet on the
word "cell", failed on my two probe files. Chasing that failure led to
`book/src/reference/lexicon-of-place.md`, which cites 0287 in its second
paragraph. **A lint about vocabulary surfaced a constitutional error in the
design.** Nothing in the design review would have found it; I had read
`plate.rs`'s implementation comments and never grepped `docs/decisions/` for
the ladder.
· Revised Stage 0: `virtual_dims` untouched; the map's *entry rung* moves
from band B to rung 7. Smaller, fully 0196/0287-compatible, and it is the
change that actually answers the report.
· Alternatives discarded: re-deriving chart width (illegal, 0287);
silently rewriting §2 without recording the retraction (the spec was already
written and would have carried a false claim into the plan).
· ideonomy passes: 0 (a correction against a ratified decision, not a design
choice with alternatives).
· Capture: spec §2 opens with the retraction; spec §8.1 is now the 0196
amendment; spec §8.5 records two documentation drifts found on the way (the
lexicon's triangular-facet description, stale since The Pavement made facets
quads; and band-B depth 12 in 0287/the lexicon vs 13 in `plate.rs:204`).
· **Process lesson for the retrospective, and it is the campaign's main one
so far.** I read the implementation and its comments, and inferred intent
from them. `plate.rs`'s comments describe *what a fix round did*; they do not
say *what was ratified*. The decision log is the record of intent and I did
not grep it for "rung" or "zoom" before calling a ratified design a
regression. **Registry-first is already the documented habit
(`docs/CLAUDE.md`); decisions-first belongs beside it.** The near-miss cost
nothing only because a vocabulary lint happened to sit in the way.

---

## Stage 0 — implementation (2026-09-03)

#12 [G5] — **Stage 0 needed a second change to be shippable: the observer
marker.**
· What happened: moving the entry rung off band B removed the `@` from the
opening view, because `compose_perception_layer` returned early unless
`at_walk_band_rung()`. Caught by a test written for it, after ten unrelated
tests went red on the rung change — **none of those ten would have caught
it**, because every one of them asserts about band B.
· Why it is not scope creep: **decision 0293's own Context is this exact
defect.** It records the claim "the perception overlay already paints the
observer and marks into the plate, so `@` and creatures survive" being
asserted *without checking the other rungs*, and the resulting view having
"no `@`… the picture and the prose described different places". Landing
Stage 0 without the marker would have re-created a documented, ratified-
against defect.
· Decision: off band B the overlay paints the OBSERVER ALONE.
`draw_perception_layer` is already rung-agnostic, so the change is one
`retain(|p| p.here)`.
· Alternatives discarded: relaxing the gate wholesale (would paint MARKS on a
merged tile — the placement claim a coarse rung cannot resolve); leaving the
gap and reporting it (knowingly shipping 0293's defect).
· ideonomy passes: 0 (the split follows from an existing ratified decision).
· Capture: two tests — `the_map_shows_the_observer_at_the_rung_it_opens_at`,
`off_band_b_the_overlay_paints_the_observer_and_no_marks`.

#13 [G5] — **Overturned a task-level refusal:
`a_coarse_rung_draws_no_perception_overlay`.**
· That test asserted a coarse rung draws NOTHING, reasoning that "every one
of [the 31 facets] collapses onto the observer's single tile and an overlay
drawn there would claim to place facets it had merged".
· Decision: keep the reasoning in full, narrow the conclusion to the MARKS.
The reasoning was only ever about placement claims; "you are in this tile" is
true at every rung. Renamed
`a_coarse_rung_draws_the_observer_and_no_marks`, and given a non-vacuity arm
(the band must carry marks for "no marks" to mean anything).
· Why this is not the mistake of #11: I checked first. `docs/decisions/`
carries 0290 (which crate draws the layer), 0292 (centre on arrival, anchor
on gesture) and 0293 (a rung belongs to the consultation) — **none gates the
overlay by rung**, and the test's own doc says "Task 7 owns what a coarse rung
shows", i.e. a task choice. A ratified decision would have stopped this.
· ideonomy passes: 0 (a scope narrowing against stated reasoning).

#14 [G5] — **A guard that Stage 0 made VACUOUS, retargeted rather than
deleted.** `leaving_the_map_at_a_coarse_rung_returns_the_walker_to_their_own_band`
discriminated "the walker got their band back" by `Source::Chart` being
ABSENT at a coarse rung and PRESENT after the exit. #12 makes it present on
both sides.
· Counting does not rescue it either, and this is worth recording because it
looks like it should: seed 42's flagship band carries a mark on the
observer's own facet and nowhere else, and `here` outranks a mark on the same
box, so band B and a coarse rung both paint exactly one `@`.
· Decision: discriminate on the rung PLUS the observer being on the plate.
The old comment rejected asserting the rung alone because a fix could "reset
the rung and leave the window in the arctic corner" — which is exactly what
the picture half rules out, so the conjunction is **strictly stronger** than
what it replaces.
· ideonomy passes: 0.

#15 [G5] — **A pre-existing `apply_zoom` defect that Stage 0 exposed, and
which Stage 0 does NOT fix.**
· Symptom: climbing seven rungs with `Action::Zoom(1)` does not keep the
observer under the cursor. Measured: the strip at band B after a zoom climb
reads "an unnamed sea" instead of the observer's own landmass, and in another
test the observer landed at row 8347 against a window origin of 7819 on a
52-row plate.
· Cause: zoom is cursor-anchored (decision 0292, "anchor on gesture") and each
rung doubles the chart, so sub-tile rounding compounds — 2^7 = 128x by band B.
· **Not caused by this stage**: `apply_zoom` is untouched. Nothing had ever
zoomed seven consecutive rungs before, because `enter_map` landed on band B
directly, so no test could see it.
· Decision: leave it. Fixing zoom anchoring is not Stage 0's job and would be
an unreviewed behaviour change to a ratified gesture. The two test helpers set
the rung directly instead, and say why in their docs.
· Capture: **needs an idea-registry row** — carried as a Stage 0 follow-up
rather than filed silently, because it is a real defect a reader can hit
(zoom in six times from the map and your position drifts off screen).
· ideonomy passes: 0 (a finding, not a decision).

---

## Stage 1 — implementation (2026-09-03)

#16 [G5] — **STAGE 1'S HEADLINE PREDICTION IS FALSIFIED. Blending buys no
extra relief BANDS below the grid.**
· Preregistered (spec §6, H2): "bilinear blending strictly increases distinct
rendered bands per plate at rungs 6-11."
· Measured, 120x40 plate at BAND_B_RUNG, eight inland locations, seed 42:

| | snapped | blended |
|---|---|---|
| distinct grid vertices | 1-4 | — |
| distinct relief bands | 1-2 | **1-2** |
| distinct heights | 1-4 | **612-3,860** |

· Cause: `hornvale_scene::relief_band`'s rungs are hundreds of metres wide
(0, 300, 1000, 2500 m). Within one ~110 km sample a real height ramp almost
never crosses one, so the quantizer discards the refinement.
· **What survives.** The gain at rungs 6-11 is real and was measured earlier
in the campaign (rung 11: `^` 461 -> 1212, a gradient replacing a block
edge), because a plate there spans several samples and the blend smooths
BETWEEN them. What is false is the claim about below-grid rungs, which is
where the reported defect lives.
· Decision: ship Stage 1 for what it does, and record the null rather than
retuning `relief_band`'s floors to rescue the prediction. Those floors are
load-bearing for a shipped wire field (`scene/surrounds/v2`) and
`windows/scene/src/surrounds.rs` says so in terms: "Do not retune the floors
below to make a picture look better."
· **What it points at instead**: the height field is present and rich, and
the band is what throws it away. That is
`CLIENT-map-is-invitation-not-data-dump`'s own brief — "less about specific
glyphs, more about what colour can hint" — i.e. a colour ramp WITHIN a band,
reading `TileTerrain::height_asl`. Carried as a Stage 1 follow-up, not
built here.
· ideonomy passes: 0 (a measurement outcome, not a choice between options).
· Capture: spec §6 needs H2 marked falsified; the colour-ramp direction needs
a registry row.

#17 [G5] — **Decision 0121's "a blend moves a value at most one band" is not
a blend-versus-snap bound, and asserting it as one failed.**
· What happened: the conservation test's first form asserted exactly 0121's
phrase against the snapped reading and measured a move of **2** bands at row
60, col 9.
· Why: at `GLOBE_RUNG` a tile IS its facet, so `Facet::corner_weights` is the
exact four-way tie and the blend is the plain mean of four corners. A mean of
four values sits more than one band from the NEAREST of them whenever the
four span three bands — over mountains, they do.
· **0121's ruling is untouched** and is not relitigated: ordinal fields may
band a blend, nominal fields must partition. What does not survive is reading
its one-band phrase as a bound on blend-vs-snap, because that is not the
comparison it describes.
· Decision: assert the bound that is provable and is what conservation
actually needs — **a blended reading lies inside the convex hull of its own
samples**. Interpolation never leaves the hull of its inputs and `relief_band`
is monotone in height, so `band(blend)` is bracketed by the least and
greatest band of the tile's own corners. That is "coarse constrains fine"
stated exactly: a refined reading can never assert relief its samples do not
bracket. Holds at both ends of the ladder, measured.
· The nominal half is asserted too, though this stage does not touch it:
re-deriving `water` or `ocean` from the blend is a one-line edit away and is
precisely the -29%-fresh-water revert 0121 records.
· ideonomy passes: 0.
· Capture: `every_blended_reading_stays_inside_its_own_samples`; spec §6's H2
conservation arm should be restated in hull terms.

#18 [G5] — **`TileTerrain` lost its `Eq` derive, deliberately.** `height_asl`
is float-backed and has no total equality; deriving one would be a lie about a
quantity read off a blend. Nothing consumes the struct through a
`BTreeSet`/`BTreeMap` — it is produced by `terrain_at_tile` and read
field-by-field — so `PartialEq` is the whole requirement.
· ideonomy passes: 0.

#19 [PROC] — **Four test drafts passed against unfixed code before one
discriminated, and the reason is worth carrying.** Each asserted about the
relief BAND: (1) "adjacent tiles differ by at most one band" — a constant
window satisfies it, largest jump 0; (2) the same over a facet whose corners
span two bands — the span was between DIAGONAL corners and a midline crossing
swaps ADJACENT ones; (3) the same over a facet with an adjacent pair spanning
bands 2..=4 — the window still drew one band, because **one icosphere vertex
can dominate a whole cube facet**, the two lattices being incommensurate; (4)
"a plate shows more than one band" — true already at some locations, since
flatness is location-dependent (1-4 vertices per plate).
· The lesson is not "write better tests". It is that **the band is a lossy
quantization of the thing being refined**, so every band-shaped assertion was
measuring the quantizer rather than the refinement. The fix was to stop
guessing and MEASURE first (#16), then assert on the observable the
measurement named.
· For the retrospective, alongside
`tests-whose-input-collapses-to-one-value`: a fifth instance, with a new
cause — the observable was downstream of a quantizer coarse enough to erase
the signal.

---

## Stage 2 — implementation (2026-09-03)

#20 [G2] — **The spec's own Stage 2 rule is FALSIFIED: no per-tile sample can
draw a river.**
· Spec §4.2 called for "a segment-versus-tile-footprint intersection". Built
and measured: it produces river SCATTER. `ChannelNetwork::nearest_line`
returns the nearest line of ANY size, so along a trunk the nearest line flips
to a small tributary and back and the trunk breaks into dashes — the same
failure the lab's transect docs already record ("a different river became the
nearest and truncated it").
· The general statement, which is the part worth keeping: **connectivity is a
property of the line, not of any point on it**, so no per-tile query can
guarantee it however the query is refined.
· Decision: rasterise the polylines. Walking the line gives connectivity by
construction.
· ideonomy passes: 0 (forced by measurement).

#21 [Q] — **Three candidate rules measured before one was chosen, and the
second's "failure" was my own error.**
· `Transverse::Channel` at the tile centre: 154 → 6 river tiles at rung 6
(−96.1%). Correct answer to "is this POINT in the channel", wrong question.
· Within half a tile of any channel: 154 → 2,284 (+1383%) — **reported as a
falsification and it was not.** Today's rule draws ~0.98% of tiles at EVERY
rung (0.98/0.99/0.99/0.96 at rungs 6/8/10/13), dead flat; a rasterised LINE
must cover `O(N)` of an `N x N` chart, so the fraction has to halve per rung,
which the half-tile rule does exactly (16.47/3.94/1.00/0.14). **The flatness
is the signature of the area-carried defect** — The Ford's type error — so I
was conserving against the bug.
· Selection by discharge: required, not a refinement. Drawing the whole
network is 89.5% of LAND tiles at rung 6. Measured table in
`RIVER_DRAWN_ABOVE_LAND_FRACTION`'s doc.
· ideonomy passes: 0 (a measurement sequence).

#22 [G2] — **Where the selection threshold lives — Nathan corrected me, and
the half I had wrong matters more.**
· I claimed the threshold "belongs in the client, not the sim". Nathan: there
is a One True Vision of where the creeks are.
· Both are true of different things. **Which watercourses exist and how big
each is: sim.** **Which of them a given view draws: client**, because it is a
rendering budget — one client at seven rungs needs seven cutoffs, and a Unity
client at metre scale needs none.
· The half I had wrong: if the sim hands over 4,158 UNRANKED lines, every
consumer invents its own ranking and they disagree about what a creek *is*.
The sim owes a NAMED magnitude ladder (Strahler order); the client owes only a
cutoff on it. Filed as `MAP-stream-order-is-sim-truth` for a SOON/NEXT
campaign, at Nathan's request.
· A defect in my sketch independent of the boundary: "32 upstream cells" is a
raw VERTEX count, so it silently means a different-sized river the moment
`GLOBE_LEVEL` moves — the grid-dependence `branch::vertex_catchment` exists to
normalise away. Now expressed as a fraction of land.
· ideonomy passes: 0 (Nathan's ruling, adopted with one narrowing).

#23 [G2] — **Rivers ride the tile cache rather than composing per redraw —
Nathan's steer, and it is also the correct layering.**
· I had framed "no new cache" as a virtue. Nathan: lean into the cache for
tight redraws.
· He is right, and the resolution is better than a new cache: the channel
network is fixed at genesis and selection is a pure function of the rung, so a
river has EXACTLY the terrain layer's cache key and its never-invalidated
lifetime (decision 0289). Rasterising inside `draw_terrain_layer` makes it
free on redraw rather than merely cheap, and adds no key to invalidate.
· Cost, measured: 11,202 segments for the whole planet's network against
20,000 nearest-line queries for ONE 200x100 plate under the sampled design.
Sampling costs screen AREA and is flat at every rung; rasterising costs river
length IN VIEW and so gets cheaper as the reader zooms in.
· ideonomy passes: 0.

#24 [G5] — **A wrap bug my own two tests could not see, caught by the tile
cache's byte-identity invariant.**
· `plate_position` wraps a column into `[0, virtual_w)` relative to the
window's origin, so a point just LEFT of the window reads as nearly a whole
chart to its right. Anchoring the seam correction on the segment's FIRST
endpoint then dragged the second across the planet: a segment entering a
32-wide tile from the left had `a` at 244 and `b` at 11, read as a 233-column
straddle, pushed `b` to 267, and was rejected as off-tile.
· **Why my tests were blind to it**: both draw a full-width plate at origin 0,
where the wrap never arises. `tiles::composing_matches_the_uncached_draw_at_
every_shipped_rung` compares a composed plate against an uncached one and
reported cached `~` against uncached `"` at rung 6, (43, 10).
· Fix: `near_window` brings each endpoint to the representative nearest the
plate's middle before the segment is closed.
· **Lesson for the retrospective**: an existing invariant test found a defect
in new code that the new code's own purpose-built tests structurally could
not, because they shared a blind spot (full-width windows). Worth pairing with
`reduced-fixtures-delete-defect-preconditions`.
· ideonomy passes: 0.

---

## Campaign close (2026-09-03)

#25 [Q] — **Nathan's ruling: land Stages 0–2 now; Stage 3 becomes its own
campaign.**
· Question put to him at resume, with the epoch's blast radius measured
rather than quoted: Stage 3's `LOCALE_MICRO` epoch was spec §8.4's one
explicit sign-off item, and §8.4 is the only thing standing between this
branch and a merge.
· Decision: **land Stages 0–2, defer Stage 3.** The map fix the report asked
for is built and green; the epoch rides with a later campaign.
· The measurement that framed the choice, taken before the question was
asked rather than inferred: `micro.wetness` reaches four vessel fixtures,
two scene fixtures, three game-core fixtures, the gallery and the room
prose — but **no census metric reads `micro` at all** (`grep -n micro
windows/lab/src/metrics.rs` returns one comment about microseconds and
nothing else). So the epoch would not have moved a census golden or forced a
refresh. It is cheaper than the spec implied, and Nathan deferred it anyway,
which is the right shape: the reason to defer was never cost.
· Alternatives discarded: building Stage 3 with the epoch (Nathan's call
against); building the map-side noise field WITHOUT the epoch — rejected on
the merits as well as by the ruling, because it forfeits #7's whole point
and would leave the map's hollow and the prose's hollow as two unrelated
inventions, the exact contradiction Stage 3 exists to prevent.
· ideonomy passes: 0 — this was a carve-out question put to Nathan, not a
decision taken under autopilot. Recorded as given.
· Capture: decisions 0676 / 0677 / 0678 written; `MAP-coherent-detail-field`
filed as the successor campaign's row; spec §4.4, §6 (H4/H5) and §8 amended
to say Stage 3 is deferred rather than pending.

#26 [G2] — **Which of the five §8 items became records, and why two did
not.**
· **0676** (amend 0196's "may never invent detail below it"): written, and
NARROWED from what the spec asked for. The spec wanted one amendment
licensing both interpolation (Stage 1) and invention (Stage 3). With Stage 3
deferred, licensing invention would ratify a capability nothing implements
and nobody has reviewed — so 0676 licenses interpolation only, bounded by
the convex hull of the reading's own samples, and says in terms that the
second sentence does not license Stage 3. The deferred campaign owes its own
record.
· **0677** (a line-carried feature conserves length, not rasterized area):
written as specified. Stage 2 shipped; the rule is backed by measurement.
· **0678** (one noise field per discrete dimension): written as specified
even though nothing implements it, because it is Nathan's ruling rather than
a campaign's choice — leaving it unrecorded would make the successor
campaign rediscover or relitigate it. Its consequences section states
plainly that `micro_field` still violates it and that the fix is an epoch
this campaign did not take.
· **§8.4** (the `LOCALE_MICRO` epoch): NOT written. Deferred with Stage 3;
there is nothing to ratify.
· **§8.5** (documentation drift): not a decision — reported, and the half
that is unambiguously stale is fixed in this campaign. See #27.
· ideonomy passes: 1 / overturns: 1 — the pass on "write all five as
specified" surfaced that 0676 as drafted would have ratified Stage 3's
invention clause on the strength of a spec section whose subject Nathan had
just deferred. Ratifying a capability out from under its own deferral is the
`a-clause-vacuously-satisfied` shape pointed the other way: a live clause
with no implementation to constrain it.
