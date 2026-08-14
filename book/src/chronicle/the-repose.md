# The Repose

A *repose interval* is the quiet between eruptions. It is the quantity that
made Pompeii possible: not ignorance of volcanoes, but the gap between a
mountain that acts every two thousand years and a people that remembers for
one hundred. This campaign gave Hornvale that gap — a hazard field, a mountain
with a name, an event stream, and a memory that decays — and then measured
whether the worlds it already generates place their settlements as though the
gap were there.

The answer is a null, and it is the interesting kind. Settlements **do**
over-occupy violent ground, and on the shipped roster neither of the two
mechanisms the design named is what puts them there. That scope clause is
load-bearing and is carried everywhere below: one of the two channels is read
by 0.4% of the settlements measured, so this instrument cannot acquit it — only
report that it explains nothing here.

## What the world gained, and what it did not

Four derived objects, all at the composition root, none of them stored:

- a **hazard field**, `hazard(cell)`, a pure function of tectonic unrest, the
  nearest boundary kind, and the presence of a volcanic edifice, returning a
  recurrence interval;
- a **volcano identity** keyed on the edifice's source contact rather than on
  the cell you happen to ask about, so every cell of one edifice answers with
  the same mountain — on seed 42 at the canonical mesh, 360 edifice cells
  resolve to **187 objects**, where without an identity rule the same ground
  would have minted up to 360 names. Read that as 187 *identities*, not 187
  free-standing peaks: the key is a contact cell, and 173 of the 187 contacts
  abut another contact on the same plate, so one continuous stretch of arc —
  physically a ridge of coalesced cones — resolves to a chain of separately
  named volcanoes rather than to one. A settlement's horizon along such an arc
  can hold on the order of ten of them. It is a deliberate scale match (an L6
  cell is roughly 120 km against real arc spacing of 50–100 km), and a
  successor modelling a settlement's relationship to "its mountain" must not
  assume that relationship is 1:1;
- an **event stream**, drawn for a `(seed, cell, window)` query, with Poisson
  inter-event times and magnitudes from an authored law;
- **knownness**, a people's awareness of its own ground: set to one by an
  eruption, halved every two generations, and discarded entirely past ten
  half-lives.

Nothing was committed to any ledger. No stream that already existed gained a
draw. The seed-42 almanac, the seed-42 scene export and the seed-42 world are
byte-identical to what they were before the campaign began, and everything
`make rebaseline` rebuilds came back unchanged. Two parts of that carry real
weight, because they are regenerated from **live worlds** rather than copied:
the gallery, rebuilt from three seed-42 worlds, and the chorus study, rebuilt
from fifty (seeds 0–49, six metrics, rows and charts). Fifty-three worlds
generated afresh, not one byte moved.

What that run does **not** evidence is the census. `regenerate-artifacts.sh`
runs the chorus study unconditionally but keeps the two censuses' `rows.csv`
behind an opt-in flag, so those rows were never re-derived; and
`book/src/domesday/` is a pure read over them, so its stillness is derivative
of a file nothing recomputed. (Their `schema.json` *is* re-derived every run,
from the live metric registry — but that builds no world and says nothing
about the numbers.) Supporting the stronger claim would take a census refresh
on the canonical box and a diff of its rows, which is a separate act from this
one. A mountain now has a name, and no world knows it yet.

## The magnitude law was authored, and recovering it proved nothing about the world

This has to be said before the numbers, because the numbers are seductive.

The eruption and earthquake magnitudes are **put in by hand**. Seismic
magnitudes come from a truncated Gutenberg–Richter law with the *b*-value
written into the source as `1.0`; eruption sizes come from a VEI-shaped law
whose mean has a closed form. Drawing two hundred thousand events and fitting
them back returns:

| statistic | recovered | authored |
|---|---|---|
| *b*, busiest cell (199,891 events) | 0.99935 | 1.0 |
| *b*, quietest cell (49,842 events) | 1.00162 | 1.0 |
| mean recurrence, busiest cell | 276.495 y | 276.346 y |
| mean recurrence, quietest cell | 20,063.6 y | 19,999.9 y |
| mean VEI, one edifice (50,190 eruptions) | 2.61744 | 2.62004 |

Every number in the right-hand column was chosen by a person. The match is an
**implementation check**: it says the sampler carries the authored value
through the pipeline without corrupting it. It says nothing whatever about
volcanoes. A chronicle reporting "Hornvale's earthquakes follow
Gutenberg–Richter" would be reporting its own input back to itself.

The reason this posture was chosen rather than a generative one is on the
record. A branching aftershock model would have made the campaign's central
statistic the same criticality exponent that two previous campaigns predicted
and both falsified. An authored law is the analytically-solvable regime: what
must come out is known in closed form, so a mismatch has exactly one reading.
Every tolerance above was derived before the runs rather than sized to the
observations — the *b*-value's own maximum-likelihood standard error at two
hundred thousand samples is 0.22%, so the 0.02 band the test asserts is nine
standard errors wide, and the observed deviations sit eight to thirty times
inside it.

One caution belongs beside the recurrence rows. A perfectly regular clock
ticking at the authored rate passes the interval test unchanged, because the
mean of the gaps telescopes. Poisson-ness is a separate claim and is asserted
separately.

## The design's motivating premise was wrong in both halves

The campaign was specified around a sentence about the shipped code: *a world's
most fertile and most mineral-rich ground is its most tectonically violent
ground, and nothing in the model charges for it.* Both clauses failed on
contact.

**Fertility never reaches siting.** Fresh volcanic parent rock does classify as
andosol, and andosol is documented as very fertile. But soil classification has
five call sites in the workspace and not one of them lies on the path that
places a settlement. The reward the design assumed was being paid is not wired
to a payee.

**And the fertility is mostly not where the violence is.** Measured across
thirty seeds, the land-area-weighted andosol share falls by an order of
magnitude as unrest rises — though not step by step:

| unrest decile | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|---|---|
| andosol share | 0.009151 | 0.006607 | 0.006012 | 0.007355 | 0.006397 | 0.003927 | 0.002973 | 0.001793 | 0.000386 | **0.000000** |

Exactly zero in the most violent decile, and 0.009151 in the calmest — but the
series rises at decile 2 → 3, so it is not monotone, and the first draft of
this paragraph said it was. The mechanism is legible in the classifier:
andosol requires volcanic parent rock **and** a mean temperature above 5 °C,
and on high ground the high-unrest cells run colder.

**The direction is not universal, and the exception is where almost everyone
lives.** Stratified by elevation band, the two upper bands carry the fall —
0.009338 → 0.000000 in 1000–2500 m and 0.013843 → 0.000000 in 2500 m+, and
between them they hold 75.8% of the land, which is why they set the pooled
shape. The two lower bands run the other way: each reads exactly zero at
decile 0 and *rises* into an interior peak — 0.000587 at decile 5 in 0–250 m,
0.000527 at decile 6 in 250–1000 m — before collapsing to zero at decile 9.
The 0–250 m band holds **65.5% of all settlements**. So the anti-correlation
is a pooled and upper-band effect, not a law of the world, and the sentence
"within any elevation band the high-unrest ground runs colder" is more than
this table supports.

Two honest qualifications on that reversal. The lower bands' shares are an
order of magnitude smaller than the upper bands' throughout — the whole rise
is from 0.000000 to about 0.0006, against upper-band values near 0.009 and
0.014 — so the *sign* disagrees while the *magnitude* stays near the floor.
And both lower bands still reach exactly zero at decile 9, as every band does.
The claim is that the direction is not universal, not that the lowlands hold
much andosol; they do not.

The premise still did not merely fail to connect: where the land is, the
fertility runs against the violence. It just does not do so everywhere, and
least of all on the ground people actually settle.

**And unrest is already charged for.** A hostility term multiplies carrying
capacity by `(1 − unrest)` on every cell. The model the campaign was written to
correct had been pricing the hazard all along — as a flat penalty with no
memory, but pricing it.

## The bands were smuggling in Earth intuition

Before the exposure numbers can be read, one thing about the instrument. The
readout stratifies by elevation, and its bands were first named *lowland*,
*upland*, *highland*, *montane*. Those names are false here. Hornvale's
elevations are measured against an isostatic datum on which sea level sits far
below zero — at seed 42, −2,936 m — so height above sea level runs to several
kilometres on ordinary continental ground, and an absolute 2,500 m cut takes
nearly half the land:

| band | land cells | land share | settlement share |
|---|---|---|---|
| 0–250 m | 50,792 | 11.5% | 65.5% |
| 250–1000 m | 55,764 | 12.7% | 14.4% |
| 1000–2500 m | 140,937 | 32.0% | 13.0% |
| 2500 m+ | 193,222 | 43.8% | 7.0% |

The band called *montane* is the **largest** band by area, not a mountain
fringe. Had the labels survived, this chronicle would have reported an effect
"strongest in montane terrain" — a claim about mountains the data cannot
support. The bands now carry their metre ranges and nothing else.

## Settlements do over-occupy violent ground

The statistic is a settlement's share of a stratum divided by that stratum's
share of the land: an exposure ratio, one per unrest decile per elevation band,
pooled over thirty seeds — 26,146 settlements on 440,715 cells of settleable
land.

The spec asked one sentence — *do settlements over-occupy high-unrest ground
relative to the land base rate?* — so here is the unstratified answer to it,
which is the cleanest result the campaign has:

| unrest decile | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|---|---|
| exposure ratio | 0.868 | 0.911 | 0.931 | 0.932 | 0.953 | 0.981 | 0.997 | 1.009 | 1.091 | **1.328** |

**Strictly monotone across all ten deciles**, crossing unity between decile 6
and decile 7, and rising ×1.53 end to end. The land base is almost perfectly
even across the deciles by construction (44,044 to 44,149 cells each), so this
is a settlement-side signal, not a denominator artifact. Yes: settlements
over-occupy high-unrest ground.

That single number is a mixture over elevation, and stratifying it is what the
spec's confound control asks for — but note before reading the strata that the
unstratified series is the *stronger* statement of the headline, not a weaker
one. It is monotone where no stratum is, and it is unambiguous about
direction, while in both upper bands the absolute occupancy stays below unity
even at decile 9. Taking the rise from the calmest decile to the most violent,
band by band:

| band | decile 0 | decile 9 | rise |
|---|---|---|---|
| 0–250 m | 5.587 | 5.464 | **×0.978** |
| 250–1000 m | 0.992 | 1.559 | **×1.572** |
| 1000–2500 m | 0.321 | 0.827 | **×2.578** |
| 2500 m+ | 0.073 | 0.392 | **×5.395** |

Flat in the lowest band; steepening with elevation above it, to more than
five-fold in the highest. The first-to-last rise is large and one-directional
in three bands. It is *not* monotone step-to-step — two bands dip at
intermediate deciles — and the difference matters enough to state, because the
first draft of this sentence claimed monotonicity the table does not contain.

The first version of this table was also wrong for a larger reason, recorded
here because it was briefly believed. The land denominator was filtered to
settleable land while the settlement numerator was not, so marine settlements
fell into the lowest band by default: one aquatic people alone was 51.9% of the
pooled sample. Correcting it removed 33,544 of 59,690 settlements. The
signature was visible in the published fixture without re-running anything —
three peoples reading *exactly* 100.0% of one band with exact zeros in the
other three, beside one people distributing normally.

## Neither modelled channel carries it, on the shipped roster

Unrest reaches settlement siting through exactly two live paths: the hostility
penalty, which suppresses capacity as unrest rises, and a mineral-prospectivity
reward, which weights unrest at 0.3 in the ore field that some peoples seek. If
the gradient above is the risk/reward tension the design assumed, severing both
should flatten it.

Severing both **steepens** it, or leaves it where it was:

| band | baseline | hostility off | mineral off | **both off** |
|---|---|---|---|---|
| 0–250 m | ×0.978 | ×1.109 | ×0.976 | **×1.109** |
| 250–1000 m | ×1.572 | ×1.880 | ×1.554 | **×1.873** |
| 1000–2500 m | ×2.578 | ×2.751 | ×2.506 | **×2.660** |
| 2500 m+ | ×5.395 | ×5.836 | ×5.125 | **×5.459** |

Expressed as the share of the baseline's excess over unity that survives both
ablations: **152.6%** in the 250–1000 m band, **105.2%** in 1000–2500 m,
**101.5%** in 2500 m+. Nothing is retained *less* than fully. The lowest band
is left out of that series deliberately — its baseline rise is below unity, so
a share-of-excess would divide by a negative number and mean nothing.

So the effect is real and it is carried by **neither channel the design
named**. The hostility penalty is not merely failing to explain the gradient;
it is *opposing* it, and removing it lets the gradient show more clearly.
Something else that co-varies with unrest is siting these settlements, and this
campaign does not know what it is. That is the honest end of the measurement:
the finding is *unattributed by this instrument on this roster*.

**The mineral arm is untested, not refuting.** Only two kinds in the whole
roster weight the mineral axis at all. One of them places nothing in the
settleable-land population; the other holds 104 of 26,146 settlements, 0.398%.
Severing a channel that 99.6% of the settled population does not read is a
measurement about the roster, not about the channel. It fired as a positive
control — the harness is not blind, 193 settlements vacated and 206 founded —
but its ~6% contribution to the gradient carries no attributive weight.

**The two channels do not compose additively, and the sign of the error is not
even stable.** Predicting the combined arm by subtracting the two single-arm
deltas from baseline gives ×1.862 where the direct measurement reads ×1.873 in
the 250–1000 m band (super-additive), and ×5.566 against ×5.459 in the highest
(sub-additive), against a rounding envelope of about ±0.003. No correction
factor could have rescued an inferred residual, which is exactly why the
residual was measured with a fourth arm instead of computed from the other
three.

## One band changes direction

In the 0–250 m band the ablation moves the ratio across unity: **×0.978 →
×1.109**. Below one, low-lying settlements slightly *avoid* violent ground;
above one, they prefer it. It is the only band where severing the modelled
channels changes the **direction** of the relationship rather than its
magnitude, which says the shipped model was suppressing a latent lowland
attraction rather than merely damping it.

The claim is small and is stated with its limits: both values sit close to
unity, it is a single pooled statistic over thirty seeds with no dispersion
measured across them, and no prediction was registered that covers it.

## The Pompeii shape, measured

Knownness is set to one when an eruption occurs, halved every two generations,
and treated as gone past ten half-lives. It is permitted to contradict the
hazard field outright: a people may be wrong about its mountain, and the whole
point is the case where it is.

Of one people's 25,510 settlements across the sweep, **409 stand on a volcanic
edifice. Eighty-nine of them remember an eruption; 320 — 78.2% — do not.** That
people's memory half-life is 31.4 years against eruption intervals authored at
two hundred to five thousand. Four fifths of everyone living on a volcano in
these worlds has no category for what they are living on.

The other four peoples in the readout report zero remembering — and the zero
means **no mountain**, not forgetting. None of them places a single settlement
on an edifice anywhere in the sweep. That distinction is now carried by the
published columns rather than left to a reader's inference, and the fix that
put it there was itself a finding: the memory column originally shipped
*diluted* by stratum population, in which form it said lowlands remember better
than highlands (0.0058 against 0.00097). Conditioned on having a mountain at
all, it reverses — **0.0488 against 0.0649**, the highest conditional memory in
the table. The lowland number was large because lowland strata hold more
settlements per volcano, not because anyone there remembers more. A memory
statistic was very nearly published that was really a siting statistic.

## What was deliberately not asked

Species differ enormously in how long they live, and therefore in how long they
remember: the readout's five peoples span a memory horizon from 314 years to
2,819. The obvious hypothesis — the long-lived remember their mountains, the
short-lived forget them — was **not** registered, and was not added when the
opportunity arose.

The design had declined it for a stated reason: the species trait that carries
a deliberately-slow life schedule shipped with no occupant, so a memory
half-life derived from generation length would in practice have been measuring
body mass. **That reason expired during the campaign.** Two intervening
campaigns landed nine occupants on that schedule — three dwarven kinds and six
elven ones — so the axis is live and the coupling is real.

The ruling stood anyway. Adding a hypothesis after watching its axis come alive
is precisely the post-hoc move that preregistration exists to forbid, and the
fact that it would now be a *better* prediction is what makes it tempting and
what makes it inadmissible. It is recorded as an observation and left for a
successor to register in advance.

It would have had no power in any case. The longest-remembering people in the
readout carries a 2,819-year horizon — nine times the reach of the people that
actually remembers anything — and holds no settlement on any edifice. The
comparison has no sample on one of its sides.

## What this leaves

A hazard field that nothing yet fears, a mountain with a name nobody says, and
a measured gradient with no identified cause. The campaign that was supposed to
find out whether Hornvale rewards violent ground found that it does, that on
the shipped roster neither of the mechanisms anyone had thought of accounts
for it, and that the soil the whole premise rested on is not merely
disconnected from siting but — across most of the land — sits in the wrong
places entirely.

*Accounts for* rather than *is innocent of*: an acquittal is a claim this
measurement is not powered to make. Hostility was tested and opposes the
effect, which is a real result. The mineral channel was only shown to be
unread by 99.6% of the settled population, which is a fact about the roster.
The gradient is unattributed by this instrument on this roster; naming its
cause is open.

The design listed three possible readings of its central statistic and called
all three informative. It got a fourth: the effect is real, and the model does
not contain it.
