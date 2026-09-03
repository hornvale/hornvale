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
