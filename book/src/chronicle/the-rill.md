# The Rill

[The Ford](./the-ford.md) established that a river is a **line, not an area**,
and that a polyline carrying a width function is the same object at a hundred
and ten kilometres and at twenty-seven metres. It then drew one line in
fifteen.

That was not a limitation of the data. `downhill` is defined for every land
cell that has anywhere to go, and flow accumulation is computed for all of
them: the world already holds a complete, space-filling drainage tree over its
land, and a single threshold — `RIVER_MIN_DRAINAGE` — decides that roughly
6.7% of it is worth rendering. Below the threshold, the width law returned
**exactly zero**, so a sub-threshold trickle had no channel, no bank and no
floodplain, and read as dry at its own centre.

This campaign draws the other fourteen fifteenths, and then goes below the
cell, where no data exists at all and structure has to be *generated* rather
than derived. The first attempt at that generation was built, measured, and
falsified, and its replacement is the most transferable thing here.

## The keystone was rewritten mid-flight

The campaign began with this: *the coarse flow graph is a boundary condition,
not a suggestion*. A room, the reasoning went, is a triangle of the same
icosphere refined deeper, so the coarse flow field can be lowered onto rooms
the way any coarse constraint is lowered onto a finer mesh — a parent flows out
through one edge, the child on that edge is the outlet, and the remaining
children form a tree draining into it.

**It is a mesh error, and it is exact rather than approximate.** Cells are the
icosphere's **vertices**; rooms are its **faces**. A `downhill` edge joins two
adjacent vertices, and an edge of a triangulation is shared by exactly two
faces. So a coarse flow edge runs **along a room's boundary and never through
it**. There is no canonical lift of the flow graph onto rooms, and "a parent
flows out through one edge" is a property of a *cell* silently transferred onto
a *room*.

The construction was built anyway, honestly, and measured on three worlds:

| | the coarse graph | the invented lift |
|---|---:|---:|
| land whose flow reaches the sea | 74–82% | **26–31%** |
| land stranded more than six cells inland | 9–13% | **28–35%** |
| basin count, seed 42 | 1,373 | **2,694** |
| interior faces terminating outside their own coarse basin | — | **6.0–7.5%** |

Higher fidelity is supposed to refine lower fidelity and never contradict it.
This contradicted it by fifty points of coastal delivery — and **every local
invariant the design shipped with passed while it did so**. The guards asserted
one-step properties: that a face's outflow agrees with its parent's, that
descent is monotone across a single step. All of them held. Nothing tested
whether the *composition* of those steps arrived anywhere the coarse graph
would have gone.

The diagnosis, once stated, is a sentence about what refines and what does not:

> **Subdivide the scalar, never the direction. A tributary's direction is not
> computed — it is inherited from what it is attached to.**

Drainage is a **scalar** — a number standing on a node. Refining a scalar means
**partitioning it by area**, and that is canonical: the parts sum to the whole,
so the fine answer cannot disagree with the coarse one, by construction rather
than by test. Flow is a **direction** — an arrow on an edge. Moving a direction
field from one mesh to its dual requires a transfer operator, **and there is no
canonical one**. Choose wrongly and conservation breaks, which is precisely
what the table above measures.

The substitution also explains a symptom that had looked unrelated. Routing a
direction out of *every* face gives *every* face a channel, which is why the
first construction reported that ~74% of land rooms carried a reach and ~5–6%
of all land sat inside one — against a preregistered ceiling of 0.5%.
**Saturation is the signature of having refined the wrong quantity.**
Partitioning the scalar instead gives almost every sub-area a tiny share, and
because rendered width goes as the square root of accumulation, a tiny share
renders as nearly nothing. Only concentrations become visible watercourses.

So a sub-cell watercourse is now a **branch attached to a rendered polyline**.
Its direction is inherited from the attachment — the un-transferable quantity is
never transferred, so the operator with no canonical form is never needed. Its
drainage is a share of the catchment the coarse cell already accounts for, cut
recursively into two parts whose sum is the parent's, in exact integer
arithmetic with no tolerance anywhere. And it may not cross a coarse divide,
because it may not leave the cell whose drainage it is a share of. **Basins
refine; they cannot be rerouted.**

That last property is now measured rather than argued: for every branch, the
coarse cell whose catchment it is a share of is the coarse cell its trunk chain
terminates in, on **41,415 of 41,415** branches — with two deliberate
mutations, one driving the agreement to zero and a subtler one to 80.37%, to
establish that the assertion can go red at all.

## The tree that already existed

Two repairs had to land before any of that, and the first is a defect The Ford
shipped and documented.

A run stopped *before* a non-river downhill target, and runs shorter than two
cells were then discarded — so a river cell whose downhill neighbour was not
itself a river, and which had no river flowing in, became a one-cell run and
vanished. Thirty-nine of seed 42's seven hundred river cells therefore carried
no polyline at all. A run now **includes the cell it drains into**, every run
reaches its outlet, and the count of uncovered river cells falls **39 → 0**, on
both of the independent tests that measured it.

That closes a contradiction The Ford's stage 2 shipped in its own committed
records. Seed 42's session room stands on cell 7449 — one of the thirty-nine —
and its stored reading was a signed distance of **−5.09 × 10⁻²** radians: a
room whose own field said *river* reporting water nearly three cell widths
away, because its river had never become a line. It now reads **+8.66 × 10⁻⁵**,
standing on its own channel.

The second repair reverses a deliberate choice. `channel_half_width` returned
exactly zero below the drainage threshold, commented *"a sub-threshold trickle
is not a channel"*. **A creek is a narrow channel, not an absent one** — which
is a small sentence with a large consequence, since every band edge derives
from that half-width, so a sub-threshold line would have carried no bank and no
terrace and read dry where it ran. That reversal is recorded as decision 0130.

With both in place, the network stops being a selection and becomes a
rendering. On seed 42 it goes from 183 polylines and 883 vertices to **3,606
polylines and 14,606 vertices**. The land cells with a downhill target and no
line drop from 10,300 / 17,389 / 10,690 to **0 / 0 / 0** across three worlds,
and the rendered edge set is exactly *equal* to the land flow tree — not merely
a superset, which is the half that would catch a network inventing edges of its
own. The fraction of walk-depth rooms a channel passes through rises from
**0.0845% to 1.237%**.

## The width law was already scale-free

The campaign opened by asserting that the width law was broken: that because
accumulated drainage is an upstream *cell count*, the law would render a
sub-cell stream at cell-scale widths, and the count therefore had to become a
physical drained area.

**Measurement falsified it, and the reason is geometric necessity rather than
coincidence.** Cells tile the sphere, so `N` cells have mean area `4π/N`, and a
locally hexagonal tiling has nearest-neighbour spacing
`d = √(2/√3)·√A = 1.0746·√A`. Measured across levels 4 through 7, the ratio
`cell_spacing / √(4π/cell_count)` is **1.07824 at every level** — the hexagonal
packing constant, 0.34% off from the twelve pentagons and the curvature. So

```
  w = a · edge · √count
    = a · 1.0746 · √A_cell · √count
    = (a · 1.0746) · √(A_cell · count)
    = (a · 1.0746) · √(drained area)
```

`cell_edge` **already carries the count-to-area conversion**. Multiplying by
area while keeping the edge would have applied the grid factor twice, rescaling
every width in the world by `√(N₆/N_L)` — a doubling at level 5, and **one
sixty-fourth at level 12**. The change written to remove a scale error would
have introduced one, sign-flipped.

What the campaign built instead is the invariant itself, promoted from an
assumption to an asserted property: for a fixed physical drained area, the
rendered width is the same whichever level's units express it. `w(count, edge)`
equals `w(4·count, edge/2)` across six doublings on 661 real seed-42 inputs,
**bit-identically** — exact rather than merely within quantization, because the
scalings are powers of two and `√(4c) = 2√c` exactly.

One part of the model is genuinely *not* scale-free, and saying so was the more
useful half. `RIVER_MIN_DRAINAGE` compares a **count**, so a trickle that is no
channel at one level *is* one at the next. A subdivision inheriting that
constant unchanged sprouts new headwaters at every level — which fails the
network's self-similarity while every individual width remains correct, and
looks entirely plausible in a render.

## What a hundredfold refinement actually bought

Below the cell, the partition cuts a catchment recursively until the pieces
fall below a minimum. The resulting counts are bounded by a derived number
rather than by hope: a level-6 cell's catchment is 8,191.6 times the minimum,
so an even cut yields exactly 8,192 leaves, and the drawn cut's size-biased
overshoot of 1.5376 predicts 12,596 — against **12,570–12,602 measured**, an
agreement of 0.05%.

The fraction of walk-depth rooms a watercourse passes through goes from
**1.237% to 89.6%** (89.63 / 89.81 / 89.40 across three worlds). That number
survives the obvious objection because the same instrument, run against the
trunks alone, returns **1.2375 / 1.2112 / 1.2527%** — reproducing the coarse
answer to four significant figures before being asked the new question. And the
climb is genuinely the subdivision's: the coarsest octave alone reads 0.9870%,
and the remaining eighty-eight points arrive across the finer octaves. Delete
the subdivision and the figure collapses to about one per cent.

**And yet the campaign's most important measurement is a scale, not a
fraction.** A sub-cell valley's outer edge — where terrace gives way to dry —
sits at a median of **2.780 × 10⁻⁶ radians**. A walk-depth room's edge is
**2.831 × 10⁻⁴ radians**. The room is about **102 times** wider than the
feature inside it.

> Tier 2 puts a rill **in** a room. It does not put the room inside the rill's
> floodplain.

Two independent instruments, in two different windows, say this. The
containment figure says it from the other side: over the sampled coarse cells,
89.6% of rooms contain a watercourse while the channel area across those same
cells is **0.63% of land**. Those two reconcile exactly, and it is worth
watching them do it — 3.67 × 10⁻⁴ radians of line per contained room is about
1.25 room edges, at a mean channel occupancy of 0.70% of a room, so
0.896 × 0.0070 = 0.0063. **Both figures are the sampled population**, which is
what makes the identity meaningful; extrapolated to the whole world the channel
area is 0.67%, agreeing with the sampled figure to about 6%. Nearly every room
has a stream somewhere in it, and nearly none of any room is water.

## The prediction that failed first: Horton's ratios

A drainage network that is space-filling and self-similar should obey Horton's
laws. Under Strahler ordering, the **bifurcation ratio** `R_b` — how many
streams of one order feed each stream of the next — sits between 3 and 5 in
real river networks, and the **length ratio** `R_l` between 1.5 and 3.5. Those
are the campaign's only preregistered intervals whose reference lies entirely
outside this codebase.

**`R_b` = 2.9867 / 2.9867 / 2.8191, against a preregistered [3.0, 5.0]. A miss
on all three worlds.** `R_l` = 1.805 / 1.766 / 1.711, inside its interval.
Nothing was retuned to rescue it, and no constant moved after the numbers were
seen.

The trunk-only network *would* have passed — 3.58 to 7.14. It is reported as
context and was never substituted as the criterion, because changing the
population after unblinding to one that passes removes a constraint rather than
adding one, which is the textbook shape of a rescue.

The hypothesis also carried a mandatory falsification, because the first
construction had made `R_b` untestable: a generator that splits every element
*k* ways by rule drives the bifurcation ratio to *k* as arithmetic, and the
falsified design's ratios converged on 4.0 across three worlds for reasons
having nothing to do with hydrology. Holding the partition's seeded freedom
constant now moves the ratios by 21–28%, so the shipped statistic is at least
responsive to the draw.

**But the estimator is structurally blind, and that is the deeper finding.**
The geometric mean of *consecutive* order ratios **telescopes**: the product of
`S_w / S_{w+1}` over all orders is just `S_1 / S_max`, so

```
  R_b = (leaves / top-order segments) ^ (1/(k−1))
```

and nothing else. Every interior order cancels. The consequences are visible in
the live runs: seeds 42 and 7 agree to **six significant figures** because their
basins happen to have near-identical node counts (1,007,752 against 1,007,776),
and seed 1234 differs *only* because its forest has two maximal-order segments
where the others have one. **The entire across-seed spread of the shipped
statistic is a top-of-tree counting artifact.**

The provenance of that blindness is worth recording. The first attempt used the
arithmetic mean of the ratios; the campaign mandated the **geometric** mean,
because Horton's laws are geometric and the arithmetic mean is not their
estimator. That is true, and the classical estimator is a log-regression over
*all* orders rather than a ratio of the endpoints. **The repair of one estimator
defect installed a second of the same family.**

Whether a regression estimator would also miss is not known. The interior
per-order ratios sit near 2.68 — below the floor, which suggests it would — but
that was not measured, and the campaign will not claim it.

## The prediction that failed second: the world is still too wet

Channel area as a fraction of land was preregistered inside **[0.005%, 0.5%]**,
an interval whose ceiling exists precisely to catch *a river that is still
effectively as wide as the cell carrying it*. Real streams and rivers cover
roughly 0.3–0.6% of continental land.

**Measured: 0.6686 / 0.6766 / 0.6715%, a factor of 1.34 above the ceiling.** A
second breach of the same interval, shipped as a finding, with the width
coefficient untouched.

The prediction was nonetheless substantially borne out, and the two halves
belong together. The falsified routing design measured 5.4–5.9% — about
**eleven times** the ceiling, and ten times real continental land. Attaching
branches instead of routing a field took that to 1.34×, an eightfold
improvement, and the remaining excess has a diagnosis rather than a mystery:
every octave of subdivision contributes about the same 6.3 × 10⁻⁶ steradians,
so landing inside 0.5% means rendering roughly five fewer octaves. That is a
decision about the minimum catchment worth drawing, traded directly against the
89.6% containment — a design question, not a coefficient to nudge.

Trunks alone account for 0.20% of land, against 0.2143% measured independently
at cell scale: the instruments cross-certify each other before either is
believed on the new quantity.

## The consumer, and the third failed prediction

A room's descriptor drew its wetness from address noise. Four axes were drawn
off one label, none of them consulting the world, while the *same document*
carried real climate moisture blended from the room's corner cells. So a room
could render "damp" in a desert and "dry" on a riverbank, and the variety
clause and the habitat clause could contradict each other inside one sentence.

With a space-filling network in hand, wetness becomes a **budget and an
allocation**: climate supplies water per cell, and position relative to the
local watercourse redistributes it. That is the shape of the Topographic
Wetness Index without needing its upslope integral, because the network already
says where the water is.

The prediction was that **a walk gets damper as it descends** — over sampled
descending walks, wetness non-decreasing in at least 80% of steps, referenced
against the elevation the walk descends, which lies entirely outside the
wetness computation.

**Measured: 215 of 420 steps, 0.5119, against a 0.80 floor. Falsified.** The
baseline it replaced scored 210/420 = 0.5000, so the emitted axis barely moved
at all.

**The model is not what failed.** The grounded value alone scores 276/420 =
0.6571 — well clear of chance. What the emitted axis cannot show is that the
retained address draw is **three orders of magnitude larger than the signal**:
the median descending step moves the grounded value by 2.238 × 10⁻⁴ on a
two-unit axis. That draw is kept deliberately, because removing it would shift
a different axis in every room of every world, which is a save-format break
rather than a style choice.

And no tuning rescues it, which is verifiable where "nothing was retuned" is
only assertable: driving the local-variation constant to zero makes the emitted
fraction converge on the grounded fraction, 0.6571 — still below 0.80. Three
post-hoc alternatives fail too. Adding the coarse trunk changes nothing, and
for a stated reason with its own control: **zero of 448 walk rooms** fall
inside a trunk band, because the walks seed at branch heads and therefore
sample headwater terrain, while **6 of 400** rooms in an unselected land sample
do. Longer walks of 64, 256 and 1,024 rooms give 0.4906 / 0.4937 / 0.5018.

**The allocation is not inert, and the precise statement matters.** It acts on
34 of 448 walk rooms and **reverses 34 of 420 step verdicts** — but the
reversals cancel. Over all steps, the grounded and supply-only arms both score
276/420; in scope, grounded reads 214/345 = 0.6203 against supply-only's
216/345 = 0.6261, very slightly *worse*. So the honest sentence is *the
allocation acts and buys nothing*, never *the allocation is inert*, and the
surviving claim — that wetness now tracks the world better than a coin — is
stated on the **climate-supply term, which predates this campaign**.

That converges with the scale measurement rather than competing with it. A term
that is zero on 414 of 448 rooms and near-random on the other 34 cannot carry
an ordering, and the reason it is zero on 414 of them is that the room is a
hundred times wider than the valley inside it.

What the change *does* buy is attributable in the other direction. Across 1,048
rooms, 484 moved and the net movement is toward dry: damp gained 22 rooms and
lost 222, because a uniform draw had been making a third of every world damp
regardless of climate. The rooms that stayed damp are defensible one by one —
21 rooms drier than the median moisture still read damp, and every one of them
is asserted to stand inside its own watercourse's valley.

The last prediction is a **witness** rather than a test: no sampled room may
render a riparian variety clause together with a "dry" habitat clause. It was
19 rooms; it is now **0 of 60**, and the sweep is backed by an exhaustive match
over the variant set, so a new variant cannot silently escape it.

## The eight predictions, plainly

Three were expected to be witnesses or near-certainties; three were genuine
tests of the construction; two were tests of the world against an external
standard, and both of those are the campaign's headline nulls.

| | prediction | interval | result | kind |
|---|---|---|---|---|
| R-1 | the network is space-filling — every land cell with a downhill target appears in some run | 100%, ≥ 3 seeds | **met.** 10,300 / 17,389 / 10,690 uncovered → 0 / 0 / 0; strengthened to set *equality* with the land flow tree | hypothesis test — watched red first |
| R-2 | every run reaches its outlet | all runs | **met.** 39 → 0 uncovered river cells on seed 42; no terminal-sink remainder | hypothesis test — watched red first |
| R-3 | the width law is invariant under a change of level | equal within quantization, ≥ 6 doublings | **met, bit-exactly.** 661 seed-42 inputs × 6 doublings, no difference at all | hypothesis test — it fails under a change of the law's *form*, and survives a change of its *coefficient* |
| R-4 | the fine network reproduces the coarse network's basins | ≥ 99%, ≥ 3 seeds | **met.** 41,415 / 41,415 = 100.0000%, with two positive controls | hypothesis test — replaced three local invariants that passed while the network diverged |
| R-5 | the network obeys Horton's laws | `R_b` ∈ [3.0, 5.0]; `R_l` ∈ [1.5, 3.5]; ≥ 3 seeds | **`R_b` MISSED: 2.9867 / 2.9867 / 2.8191.** `R_l` met: 1.805 / 1.766 / 1.711 | hypothesis test — external reference; see the estimator caveat above |
| R-6 | the world does not become implausibly wet | channel area ∈ [0.005%, 0.5%] of land | **BREACHED, 1.34×: 0.6686 / 0.6766 / 0.6715%** | hypothesis test — second breach of the same interval; the coefficient was not retuned |
| R-7 | a walk gets damper as it descends | ≥ 80% of steps | **FALSIFIED: 0.5119** (grounded alone: 0.6571) | hypothesis test — reference outside the computation |
| R-8 | the contradiction is unreachable | 0 occurrences | **achieved: 0 of 60**, from 19 | **witness** — both clauses read one number by construction; it exists to catch a regression that reintroduces two sources |

## What the census says

The measurement instrument's thousand-world census was refreshed on the
canonical host, and the result is a blast radius stated as evidence rather than
as a claim: **exactly three columns of 203 moved, across all 1,000 worlds** —
the channel land fraction, the transect dry reach, and the un-truncated band
monotonicity. Nothing else moved at all. The shipped band-monotonicity axis did
not move.

**Channel connectivity did not move either, and that one is not reassurance.**
The column walks each watercourse downstream and asks whether it stays in
water across every junction — but its continuation test asks whether the next
cell is classified a *river*, while the network now renders every reach,
which is a strictly wider set. A tributary ending on a sub-threshold trunk
therefore stops the walk *before* the junction is ever examined, and the walk
is scored intact without having been tested. The mechanism is certain; the
proportion of walks it affects has not been measured and no figure for it
appears here or in the metric's own description. **A column that did not move
because it became vacuous is not the same reassurance as a column that did not
move because nothing broke**, and the repair — asking whether the next cell is
carried by any run, rather than what class it is — moves a census column, so
it is left to work that can pay for a refresh.

Three things a reader needs in order to read the rest correctly.

**The census was authored at the revision that absorbed the trunk branch
(`d8ed9bd6`), while the branch itself advanced two commits further to
`0442ac83`.** Neither later commit can move a row. The first is test-only plus
printed figures; the second is a performance change proven byte-identical
across 96 distinct networks — both arms' output sharing one checksum, with a
live probe passing against the committed fixture. The gap is stated rather than
left to be noticed.

**And this campaign's drift sits on top of three metrics that arrived from
elsewhere.** The trunk had refreshed the census and widened the metric set
immediately before the absorption; the three columns above are The Rill's, and
the three new columns are not.

One movement inside that set was predicted by nobody. The **un-truncated**
companion to band monotonicity — the anti-vacuity check that exists to say how
much work truncation is doing — read 64/64 with every world at exactly 1.0 at
The Ford, and now reads **12 / 64 with none reaching 1.0**. The shipped axis is
unaffected at 1.0. The cause is density rather than a broken repair: truncation
fires when a *different* river becomes the nearest, and with 3,606 polylines
where there were 183, a transect walking away from its own line now meets an
unrelated neighbour far sooner — about 24% are truncated before their own
channel's profile reaches dry. The truncated metric is the correct one, since a
transect that has entered another river's domain says nothing about the first
river's profile. What moved is that truncation went from doing nothing to doing
a quarter of the work, and nobody was watching that number.

The census also became much more expensive, and both halves of that belong on
the record. The refresh took **19,207.751 seconds — five hours and twenty
minutes — 11.2× the 1,718.995 seconds it cost immediately before this
campaign**. Roughly nine tenths of the growth was one function the metric
registry computed three times: three metrics reading three fields of one
identical sweep. Computing it once per world recovered **3.014×**, with peak
memory *falling* 2.2%, so there was no trade to make. That still leaves a
future census at about 1.8 hours — **~3.7× what this campaign found it** — and
the remaining cost is structural rather than incidental.

## A field whose meaning moved without its schema

One consequence deserves to be stated rather than discovered.

The room's emitted wetness is an existing key, of an existing type, in an
existing range, at an existing position in an unchanged schema. Its **shape**
did not move: a leaf-by-leaf comparison across every regenerated artifact finds
no added leaves, no removed leaves and no key-order differences. Additivity, the
discipline that governs cross-repository schemas, is satisfied — and satisfied
by measurement, not by assertion.

**What moved is what the number means.** It was address noise, explicitly
documented as *not hydrology*, with consumers warned not to band a water class
from it. It is now the room's climate moisture allocated by its position
relative to the nearest watercourse, wherever the room is bare ground under
open air — and still the old draw at sea, on ice and in the rock column, where a
river's proximity governs nothing.

No epoch was minted, and the reasoning is deliberate: an epoch is the tool for a
*shape* change, and minting one here would have re-derived every world's
identity to describe a change no parser can see. But a consumer keyed on the old
meaning — treating the axis as decoration uncorrelated with terrain — is now
wrong in a way no version check will tell it. The reference page that carried
the old prohibition carries the new meaning instead, which is the only place the
change is visible to a reader who never sees this chronicle.

## Where this stops

The world's drainage network is now the network the world already computed:
every land cell that drains anywhere has a line, every line reaches its outlet,
and below the cell each line carries a bounded, conserving, seeded partition of
its own catchment down to a stated minimum. A room can be asked what
watercourse it stands beside and how far, at any depth, and its descriptor's
wetness is an answer about the world rather than about its address.

What the campaign did not get is the thing its two external standards were
asked for. The bifurcation ratio sits below the range real rivers occupy, on an
estimator now known to see only the top of the tree; the world's channels cover
a third more of its land than the ceiling allows; and the walk that was supposed
to grow damper as it descends does not, because the room is a hundred times
wider than the valley it contains.

That last number is the campaign's real result, and it points at the next
question rather than closing this one. A rill exists in every room and occupies
almost none of it. Making a walk feel wet is not a matter of drawing more lines;
it is a matter of the walker's stride being shorter than the water is wide.
