# The Weft

The **warp** is the set of macro fields already running continuously through
the world — elevation, temperature, moisture, induration, drainage, sampled at
40,962 points and blended smoothly between them. The **weft** is fine detail
crossing them at the resolution a walker actually occupies. This campaign wove
one, measured it, and its central prediction about it was wrong.

The prediction was **legibility**, and it was frozen before any of the code
that could move it. Four kinds of derived feature were built, three of them
tied to real macro causes and one — the erratic, a lone boulder — deliberately
tied to nothing, as a negative control. If the derived surface is legible, a
kind's features should carry information about the conditions that produced
them, and the ordering of that information was written down in advance:

> spring > thicket > overhang > erratic

Measured over the land-eligible surface of seed 42, in bits of mutual
information between a facet's local macro state and its features:

| kind | measured |
| --- | ---: |
| thicket | 0.038604 |
| spring | 0.007812 |
| overhang | 0.002497 |
| erratic | 0.000000 |

**Thicket, not spring.** Two of the three pairwise relations survive — the
erratic reads zero, and spring still outscores overhang — and the one that
fails is the pair the ordering was really about, because spring was the
designated sign case: a water source diagnostic of the rock and drainage
underfoot, the kind a knowledgeable traveller ought to be able to predict. It
came second to the thicket, which is texture.

Nothing was retuned to rescue it. The reading is reported as the result, and
the test that pins it asserts the relations that survived and stays silent
about the one that did not, so a later campaign that moves a constant will not
find a green assertion quietly protecting a falsified claim.

**(2026-09-05) The fork this reading left open is closed, and these numbers no
longer describe the shipped world.** [The Warp](./the-warp.md) measured, before
it designed anything, that **333 of these 403 springs stood on a facet with no
cause at all** — the ordering was a base-rate effect in the recipe, not an
artefact of a four-bin estimator, and a nineteen-class biome word ranks the
thicket above the spring exactly as the four bins did. It then
re-parameterised the two sign kinds, and on the same statistic and the same
seed the spring reads **0.086464** where it reads 0.007812 here — an 11.1×
rise — while the overhang reads **0.076536** against 0.002497. The thicket and
the erratic are untouched at 0.038604 and 0.000000. So the ordering above is
falsified again at the shipped constants, but by a different relation:
`spring > overhang > thicket > erratic`, in which `thicket > overhang` fails
where the Weft's `thicket > spring` did. The table above is this campaign's
measurement and stands as its record; the current world's readings are in the
successor's chapter.

The erratic's zero is not a near-miss that happened to land well. Its macro
state is a literal constant, so its mutual information with anything is
algebraically zero rather than empirically small — the control could not have
scored otherwise, which is exactly what makes the other three numbers mean
something.

## The control fired once, and that is what it was for

Before the measurement above, the same estimator was run over the whole sphere
rather than over land. On that population the erratic read **0.01974** and
spring read **0.01857** — the negative control beating the sign case.

A control that outscores the thing it is a control for is either a finding
about the world or a defect in the instrument, and here it was the second. Each
kind had gained an eligibility gate: every one returns exactly zero prevalence
on ineligible ground, so *occurrence implies land* holds with certainty, for
every kind, uniformly. On a whole-sphere population that shared gate is itself
a macro correlation, and an information estimator credits it to every kind
alike — including the one whose job is to earn nothing. Ranked on the gate
component alone, the four kinds read thicket 0.07198, overhang 0.03929, erratic
0.01974, spring 0.01857: the sign case placed **last**, for a reason with
nothing to do with legibility.

The remedy is a restriction rather than an adjustment. Computed over
land-eligible facets only, land-eligibility is a constant across the
population, and its contribution is zero by construction — not small, not
estimated. The restriction was written into the frozen prediction *before* the
measurement ran, with the confounding figures stated inline so a later reader
can check the reasoning rather than take it. A prediction narrowed after seeing
an uncomfortable number is a rescue; narrowed before, with the confound
measured and published, it is the ordinary correction of an instrument.

## What the surface is

A derived feature is a pure function of seed and position. It is generated at
observation time, at walkable scale, and stored nowhere — which is what lets
there be an unbounded number of them, against the placed tier's ceiling of one
facet in 4,915.

A kind is three things and nothing else: a **prevalence recipe**, three
**scalars**, and — for kinds that afford something — a bundle of components.
The recipe reads macro state at a facet and mixes it with position-continuous
noise:

$$
\text{prevalence} = c \cdot \text{macro} + (1 - c) \cdot \text{noise}
$$

The mixing weight $c$ is the kind's **contextuality**. At $c = 1$ a kind is
pure macro state — wallpaper, following the coarse fields exactly. At $c = 0$
it is free noise, correlated with nothing the world knows. The erratic sits
near zero deliberately, and its own documentation says so in the imperative:
this kind must never be improved by tying it to a real cause, because the
moment it is, the legibility measurement stops being able to discriminate at
all.

The second scalar is a **correlation length**, expressed in facets of travel,
and it is separate from frequency. It decides whether a kind reads as ground
cover, as rhythm, or as landmark — a motif repeating below the eye's span is
texture, at it is rhythm, above it is pattern. The third is **abundance**,
which sets how often the thresholded prevalence actually fires.

The noise is sampled as the mean of three orthogonal coordinate-plane slices of
a three-dimensional field, not as a two-dimensional field over latitude and
longitude. That choice is not aesthetic. A latitude/longitude projection has a
seam at the antimeridian and distortion at the poles, and both would have
appeared in this campaign's own spatial-correlation statistic as anomalies
attributable to the mechanism rather than to the projection — the measurement
would have been poisoned by the coordinate system it was taken in.

Four kinds shipped:

| kind | macro cause | contextuality | what it is for |
| --- | --- | --- | --- |
| spring / seep | carbonate × channelized drainage | high | the sign case — water diagnostic of what is underfoot |
| overhang / hollow | induration × slope | medium | affordance — shelter from weather, a place to build a fire |
| thicket / brake | temperature × moisture | high | texture, aimed at biome monotony |
| erratic / scatter | none | near zero | the negative control |

**(2026-09-05) Two of these four rows now use a different parameterisation.**
[The Warp](./the-warp.md) regrouped the spring's and the overhang's recipes as
a **reliability** against a soft step in the cause plus a **floor** against
noise, with both floors authored to zero — algebraically the same two degrees
of freedom, rotated onto the quantities a walker-side readout measures.
Contextuality and abundance survive unchanged for the thicket and the erratic,
which are that campaign's non-regression controls and whose derived output it
proved byte-identical.

## Density, which was the deliverable

The union of the four kinds occupies **0.256285** of land-eligible facets —
2,875 of 11,218 — roughly one facet in four. Per kind, on the same population:
spring 403, overhang 843, thicket 1,517, erratic 428.

**(2026-09-05) The sign kinds are rarer now.** [The Warp](./the-warp.md) traded
frequency for found-ness on the spring and the overhang, whose existence
densities at this same seed went 0.0359 → 0.0134 and 0.0752 → 0.0215 — 2.7×
and 3.5× rarer. The union fell to **0.195222** of land-eligible facets, about a
quarter below the figure above; the thicket and the erratic, untouched, are
unmoved to five places. The density result and the multiple below are not
disturbed by that, but the per-kind counts in this section describe the world
as this campaign shipped it rather than the world today.

The placed tier, measured immediately before this campaign, sits at one
enterable site per ~84,200 land facets. The derived surface is therefore about
**21,500×** denser, clearing by more than a full order of magnitude the
three-orders-of-magnitude claim frozen in advance.

Two numbers were reported rather than one, because observation-scoped
discovery makes them different questions. Existence density is god's-eye: what
fraction of the surface carries something. Encounter rate is walked: how often
a traveller meets one. Over 78 walks of 60 steps each, the union encounter rate
reads 0.307692, and the two agree per kind to within about 30% relative —
close, but not identical, which is the expected shape for two genuinely
different samples of one process rather than evidence that either is redundant.

## The coherence check is a guard, not a discovery

The surface was also checked for spatial autocorrelation, and it is worth
being precise about what that check can show, because the first draft of this
record overstated it.

Occurrence is a threshold applied to a position-continuous field at each kind's
own correlation length. A positive short-lag autocorrelation is therefore
close to guaranteed *by construction*. The statistic is a **regression guard**
against the surface silently becoming address-hashed speckle — a real hazard,
since hashing a facet's address is the obvious cheap way to draw per-facet
noise and produces something that looks fine until it is measured — and it is
not independent evidence that the surface is coherent. It reads 0.90, 0.81,
0.96 and 0.59 for the four kinds — no mutant Moran's I was measured, so this
statistic's own hashed-baseline is unknown, and for thicket the margin is
known to be thin on a related check: at contextuality 0.90 (above the
shipped 0.85), an address-hashed mutant's lag-1 prevalence autocorrelation
reads 0.955, above the 0.95 threshold that same guard uses. Whether Moran's I
would discriminate as cleanly is not established by this reading alone.

The evidence that actually discriminates was taken separately, by mutating the
mechanism into the defect and re-measuring. Real prevalence autocorrelation
against the address-hashed mutant, same walks, same facets:

| kind | real | hashed |
| --- | ---: | ---: |
| spring | 0.998 | 0.209 |
| overhang | 0.982 | 0.089 |
| thicket | 0.99994 | 0.890 |
| erratic | 0.868 | **−0.025** |

The erratic separates most cleanly of the four, and that is not luck: with
almost no macro term, its signal is nearly all the noise term, so destroying
the noise's continuity destroys the correlation outright. The kind built to
score zero on legibility is the kind with the most power on this axis.

The autocorrelation statistic had to be re-scaled once before it could say
anything. It was first computed over adjacency on the coarse point lattice —
but one step there spans roughly 106 to 127 facets, which is between two and
twenty-three correlation lengths depending on the kind. At that separation a
sound construction and a hashed one make the *same* prediction, near zero, and
a statistic whose competing hypotheses agree is not a test. It was recomputed
over adjacency along a walk, at the resolution the kinds actually vary. The
discarded readings were published rather than deleted, and both statistics rank
the four kinds in the same order, which is the signature of a wrong-scale
correction rather than a rescue.

## A measurement made against a constant nobody had checked

One constant in the overhang's recipe was chosen by analogy — an order of
magnitude gentler than a neighbouring slope constant — and never checked
against real ground. Seed 42's land slope distribution has a median nearly
double it, which put the saturating term above 0.95 for most of the land and
above 0.998 for the top quartile: the slope half of "induration × slope" was
doing almost nothing, and the kind had quietly collapsed to bare induration.

Reusing the world's existing "maximally rugged" ceiling instead gives a graded
response across the real distribution, from 0.08 at the tenth percentile to
0.985 at the ninety-ninth. It also moved the overhang's own occurrence count by
**−24.5%**, from 1,117 to 843, in the same change that measured it — a fix that
invalidates the numbers that justified it, which is a thing worth noticing
before quoting either set.

## The other half: a world that could not mention its own dead

The campaign's other half needed no new mechanism, because the information
already existed and nothing read it.

A survey of what a walker actually perceives found that **a walker never
perceived a ruin at all** — not thinly, not at low salience, but not at all.
Dead occupations exist as committed facts, the almanac reads them, the
laboratory measures them; the walk band was silent. You could stand on a dead
civilisation and be told the biome and the weather.

That silence was deliberate and documented: the walker's brief carried only the
fields something read, and the ruin signature was named as absent *on purpose*,
waiting for the campaign that first needed it. This was that campaign. The
brief gained the cause, the ending, and the ages already computed at world
generation, and the walk band gained one sentence. At seed 42's flagship — the
same facet the previous campaign quoted to show how little the world said —
the world now says:

> Tropical seasonal forest — buttressed canopy, sun-warmed, damp, on a rise —
> in the lands of Doaba. … You can enter the settlement of Doaba. **Something
> ended here: flight.**

One step north, where the settlement's own sentence falls silent, the ruin's
does not, and a derived feature has joined it:

> Tropical seasonal forest — a liana tangle, sun-warmed, damp, in a hollow —
> in the lands of Doaba. … **Something ended here: flight. A dense thicket
> presses close around you.**

The ruin clause is a second sentence beside the settlement clause rather than a
replacement for it, because a facet can hold both: a living town standing on a
dead one is an ordinary outcome of a world whose settlements are grown by a
history run rather than placed, and one must never gate out the other.

A ruin is also the strongest source of legibility in the world, and structurally
so. A derived feature is evidence about *present conditions*; a ruin is evidence
about a *past event*, which is the one thing a derived feature cannot be. The
tier axis is about when a thing is generated, never about what kind of thing it
is — generated at world-generation time on the coarse lattice, where it can
affect and be affected by the rest of the world, or generated at observation
time at walkable scale, where until it is derived it can participate in nothing.
The same kind of thing may legitimately exist at both.

## A ruin still stands on a point, and the reason is not a shortcut

A settlement's territory is a set of places; a ruin ought to inherit one. It
does not, and the campaign chose not to give it one.

The record an occupation leaves carries a single point, not a set. The one
territory notion the world has is computed per people and is explicitly
alive-only. So a ruin's extent could not be *recovered* from what the world
already states — it would have to be **invented**, by dilating the point by a
ring, and inventing world-shape inside a presentation change is the wrong place
to do it. The reason is now written where the next reader will meet it, beside
four earlier corrections in the same file each cleaning up a draft that had
assumed the region variant existed. This design document was the fifth.

## Nothing derived is ever committed

The whole tier's cost claim rests on derived features staying outside the
record, and that is pinned by a live walk rather than by a fixture: 300 steps
through dense derived ground, reading the surface twice at every facet, commits
only the facts a walk always commits. Proving the guard could fail took two
attempts — the first mutation was silently absorbed by the record's own
duplicate detection and passed falsely. A mutation the system deduplicates is
not a mutation.

Between derivation and use, features live in a **residency window**: a bounded
disc of facets around the observer, filled at the leading edge and evicted at
the trailing edge as it moves, holding nothing when the observer stands still.
The cost of movement was estimated in advance at about 84 facets per step and
measured at **21** — a straight-line step moves the square along one axis, so
exactly one edge strip enters and one leaves. A diagonal step, the expensive
case, costs 41. The estimate had computed the window's whole perimeter rather
than the change across a step, and the real cost is two to four times cheaper
than the design claimed.

## The first census found a tax, and the profile found its multiplication

The first canonical census after the semantic close consumed **33,709.137 CPU
seconds**, **19.9%** above the pre-Weft band maximum. That number reopened the
campaign rather than becoming a tolerated cost. A merge-base control made the
attribution unusually clean: the 22 Weft metrics explain **5,469 of the 5,598
added CPU-seconds (97.7%)**, while every other campaign change together measures
+1.0%, inside the matched runs' noise. Both census studies select every metric,
so the new readers are paid across 2,000 rows, not the 1,000 an early source
comment assumed.

The profile's first estimate was that about **72% of the Weft bill** was
recoverable duplicate work. It was a call-path attribution, not a measured
speedup: the grid pool computed a facet's four corner weights, then each of its
four kind evaluations computed the same weights again — twenty nearest-index
scans where four suffice. Passing the already-owned value through the call
boundary measured **627.01 → 399.90 user CPU-seconds (-36.2%)** on matched
150-world panels, with wall down 33.5%, mean peak RSS up 0.9%, and the grid
symbol falling from 10.021% to 3.51% of study cycles.

The walk pool had the same smaller shape. Reusing its prepared weights projects
**419.800 CPU-seconds (1.372% of a 30,599.470-second full census)** recovered;
all eight population-matched pairs improved and the named symbol fell from
1.69% to 0.69%, although individual-pair extrapolations cross the 1% decision
floor. The next apparent shortcut dissolved under measurement: exact reverse
lookup hit only 42 of 163,848 corner queries. The strongest real cache then
measured **0.601%** with peak RSS up 5.73%/6.25%, so it was removed and the
intervention stopped rather than turn several sub-floor operations into an
unattributable batch.

Every default and meeting-population comparison retained all 249 metric columns
byte-for-byte. The complete canonical census then measured **29,563.783 user
CPU-seconds**, down **4,145.354 (-12.3%)** from the pre-intervention run, and
**1,060.274 seconds wall**, down 10.8%. Both 1,000-row tables retained all 22
Weft columns exactly. G6 accepted the result and stopped the intervention on
the measured residual rule, not on whether scheduler-sensitive wall time had
crossed back below its alarm.

The refresh also forced two neighbouring instruments to tell the truth. The
Domesday anomaly surface now pins 139 evaluable and 51 excluded columns over
the 249-metric census. The Gnomon's canonical injection battery was re-authored
on lefford; its current 69/120 recall witness is not folded into the older
series, because ablating the Weft family restores 72/120 and proves the new
ranked surface itself displaced three hits. Its bar did not move and its
standing verdict remains “cannot tell.”

## What the next campaign inherits

A surface that is dense, and a measurement saying it is not legible in the way
predicted. Those are separable results, and the second is the interesting one.

The bet as originally framed is that a derived surface can be dense *and*
legible at once — that noise interacting with macro features produces places
that feel found rather than extruded. "Feels found" has an exact meaning here,
inherited from the world's own stance on doubt: in a deterministic world every
uncertainty is incomplete observation rather than a die roll, so a place feels
found precisely when a knowledgeable observer could have predicted it from
visible signs and a naive one could not. That is what the falsified ordering was
measuring, and the honest reading of the result is that today's derived surface
carries the most information where it was designed to carry the least
consequence.

The inverted campaign is the natural successor and is deliberately not this
one: start from what a walker should be able to infer, and let frequency fall
out of it. This campaign built the density and made the bet falsifiable; the
next one can start from a measured baseline instead of an intuition, and from a
statistic already known to be sensitive to the difference between a real
surface and a hashed one.

One thing the walker still cannot be told is a rumor. Rumors were designed here
and built nowhere: a rumor descends from a real observation worn by
transmission, so it goes stale, vague or misattributed, but never false — the
error is "not what you were promised", never "nothing at all". The invariant
that would hold that promise — every rumor names a region, and the region is
never empty — was frozen alongside the other three and then **withdrawn before
measurement**, because the campaign builds no producer of rumors and a test of
it would have passed by finding none to check. A guard that is green because
its population is empty is worse than no guard, and this project has shipped
that shape enough times to recognise it early. The invariant is deferred with
its producer, and restoring it is the first task of whichever campaign builds
one.
