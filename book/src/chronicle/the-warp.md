# The Warp

The **warp** is the set of macro fields already running continuously through
the world — elevation, temperature, moisture, induration, drainage, carbonate,
sampled at 40,962 points and blended smoothly between them. The **weft** is
the fine detail crossing them at the resolution a walker occupies. The
previous campaign wove the weft and measured whether it tracks the warp; that
prediction was falsified. This campaign asks the question from the other end:
not what the world knows, but what a walker is *told*, and whether a
knowledgeable one could work back from the telling to the feature.

Three things follow from asking it that way. The room sentence gained two
words. The measuring instrument moved to the walker's side of the channel and
grew a null. And two of the four derived feature kinds were re-parameterised
so that their occurrences stand where their signs are — at a price in
frequency that is reported rather than hidden.

## What a walker now reads

At seed 42's flagship facet, the same one two previous campaigns quoted to
show how little the world said:

> Tropical seasonal forest — buttressed canopy, sun-warmed, damp, on a rise —
> in the lands of Doaba. … Something ended here: flight. **Underfoot, pale
> limestone; the ground is level.**

Two clauses, two new words, and both are functions of the world rather than of
the room's address. The **rock word** is the rock class at the room's dominant
corner vertex — categorical nearest-corner, never blended, because a nominal
field must partition rather than average. It is a per-vertex word and reads
the same across a vertex's roughly four thousand rooms; that is a disclosure
of the model's resolution, not a defect, and the alternative — inventing
sub-vertex geology to make the word vary — would be a document finer than the
thing it documents. The **steepness word** cuts the blended slope, saturated
exactly as the overhang's own recipe saturates it, into *level*, *sloping* and
*steep*.

Both clauses fall silent afloat: a walker does not stand on the sea floor.

The two words were chosen rather than invented. The overhang's cause is
induration times slope, and induration is a property of the rock class — so
with the rock word and the steepness word a walker holds both halves of an
overhang's sign, as they already held both halves of a spring's, limestone and
wet.

## The premise was measured before it was designed

Every claim in the paragraph above began as a number rather than an intuition.
A probe tabulated every sign the walk-band sentence renders at seed 42 against
each kind's occurrence, over the previous campaign's own land-eligible
population — 11,218 facets, carrying 403 springs, 843 overhangs, 1,517
thickets and 428 erratics — and ran in 1.2 seconds.

Each reading is paired with a **lagged null**: the same signs against the
occurrence bits of the facet a thousand places later in vertex order. A cyclic
shift is a permutation, so both marginals survive and only the link between
them is broken; whatever the estimator would report from cardinality alone is
then read off rather than argued about. The previous campaign's own estimator
is one row of the table and reproduces its published readings exactly, which
is the positive control that this is the same population.

In bits of mutual information with occurrence, net of null:

| sign (classes) | spring | overhang | thicket | erratic |
| --- | ---: | ---: | ---: | ---: |
| biome word (19) | 0.0032 | 0.0009 | 0.0356 | 0.0000 |
| rock class (16) — *not yet rendered* | 0.0035 | 0.0005 | 0.0142 | 0.0002 |
| wetness word (3) | 0.0006 | 0.0001 | 0.0125 | 0.0002 |
| relief, aspect, openness (3 each) | ≤ 0.0002 | ≤ 0.0002 | ≤ 0.0002 | ≤ 0.0002 |
| everything visible then (177) | 0.0025 | 0.0023 | 0.0390 | −0.0009 |
| with the rock word added (271) | 0.0061 | 0.0026 | 0.0381 | 0.0005 |
| the world's own ceiling (cause, 4 bins) | 0.0078 | 0.0025 | 0.0386 | 0.0000 |

Three readings matter. **The thicket was already at its ceiling** — its causes
are temperature and moisture, and those *are* the biome word, which is why it
won the previous campaign's ordering. **The spring reached a third of its
ceiling** from what was rendered and four fifths once the rock word was added,
so the rock word was worth adding and is the whole of what rendering can buy.
**The three address-noise axes sat at their nulls**, as noise must. And the
whole sentence read as a single tuple of 5,840 classes measures 0.15 to 0.37
bits against a null of the same size — a number that would have looked like
legibility and is bias.

## The finding the campaign was built on

The same probe binned each kind's own cause into four equal bins and counted
where its occurrences actually stood.

| kind | facets in the lowest cause bin | occurrences there | share of occurrences on a cause ≥ 0.5 |
| --- | ---: | ---: | ---: |
| spring | 10,754 | **333 of 403** | **0.077** |
| overhang | 7,399 | 474 of 843 | 0.057 |
| thicket | 5,901 | 392 of 1,517 | 0.427 |

**Three hundred and thirty-three of four hundred and three springs stood on a
facet with no cause at all.** The spring's cause class is 464 facets of
11,218 — about four percent of land — and the recipe's floor term ran over the
other ninety-six. An observer told the whole truth about carbonate and
drainage would still have been unable to predict ninety-two percent of the
world's springs.

That closes a question the previous campaign left open. Its falsified ordering
had two candidate explanations: a real property of the world, or an artefact
of a coarse four-bin estimator finding more usable structure in temperature
and moisture than in carbonate and drainage. It was the first. The
nineteen-class biome word — nearly five times the resolution — ranks the
thicket above the spring exactly as the four bins did. What produced the
ordering is signal detection's base-rate effect: a small false-alarm rate over
a large population outnumbers a high hit rate over a small one. And a
base-rate effect lives in the recipe, which is the only place a campaign can
move it.

The question was answered before this campaign's design existed, with no
recipe touched to reach the answer. That ordering matters: the recipes did
move later, under their own frozen readout, and an answer taken afterwards
would have been worthless.

## Five words for one idea

*Legible* had been doing too much work, so the campaign split it.

- **Ceiling legibility** — what the *world* knows: information between a
  facet's hidden cause and its features. The previous campaign's statistic.
- **Channel legibility** — what the *walker is told*: information between the
  rendered sign tuple and occurrence, always net of null. Bounded above by the
  ceiling, because rendering cannot manufacture information the world does not
  hold.
- **Learned legibility** — what a walker can *acquire*: the same table fitted
  on half the world and scored on the other half.
- **Found** — a kind's occurrence is found when its cause was there to be
  read, and **extruded** when none was. The share of a kind's occurrences that
  are found is, exactly, signal detection's positive predictive value.
- **Told** — the failure *above* found: a sign that merely restates its
  feature is wallpaper. Naming it turns the target from a floor into a band,
  which is why the frozen predictions carry an upper bound as well as a lower
  one.

A **sign**, finally, is a rendered, world-determined, at-facet, discrete
token. Negating each of those four properties names something the campaign
deliberately excludes: the hidden cause (not rendered), the **false sign**
(not world-determined — the descriptor noun and the relief, aspect and
openness axes, which are address noise wearing causal semantics), the remote
sign (not at the facet — a neighbouring facet's word, or a rumor), and the raw
diagnostic datum (not discrete — the numbers the `examine` verb prints, which
the room sentence never says).

The false signs were measured rather than deleted. They are the control that
says the instrument is honest.

## Every reading has a null

Thirty-two readouts were registered — eight families across four kinds — and
each channel number is reported beside the average of five cyclic shifts at
one thousand, two thousand, three thousand, four thousand and five thousand
places.

The null is not decoration; it is what makes the bars derivable. A χ²
approximation predicts a permutation null's mean at `(K − 1) / (2 · n · ln 2)`
for `K` classes over `n` facets, which at seed 42's 469-class sign tuple gives
**0.030091 bits**. The instrument's causeless control read **0.03009126**.
That agreement is the positive control that the null is the null.

The same approximation gives the null a *spread* as well as a mean —
0.001967 bits for the sign tuple, 0.000464 for the 27-class false-sign
tuple — and that is what the first draft of the frozen predictions got wrong.
Its bars for the noise controls were ±0.001, below the null's own standard
deviation, so the control would have failed by chance roughly a third of the
time. A control that fires by noise is not a control. The bars were reset to
four null standard deviations, computed from seed 42 and then frozen rather
than recomputed per seed, before any measurement seed was built.

Two more instrument corrections were made in the same window, and the ordering
is the whole of their legitimacy — each was made on the calibration seed,
before a single measurement seed existed, with the confounding figures
published inline.

The **learner** first shrank thin sign classes toward one half. On a
three-percent event spread over 469 classes that predicts nearly every sparse
class at fifty-fifty and pays about 4.6 bits for each miss: the spring's
held-out gain read **−0.049**, an artefact of the prior rather than a property
of the world. Shrinking toward the *base rate* instead, with an
equivalent-sample-size weight, makes an uninformative table score exactly zero
and never below it by construction. The spring's gain became +0.018 on the
same world, and nothing else moved.

The **oracle** was withdrawn as a denominator. Smoothed, an in-sample table
over a three-percent event reads negative; unsmoothed, its in-sample log-loss
reduction is algebraically the raw channel information, which is the quantity
the null exists to correct. The oracle is still computed and printed; nothing
is measured against it. What replaced it is the ratio of learner gain to
channel information net of null, which is a real question — how much of what
is there can a finite observer actually get — and which the readout answers.

The instrument's cost was measured rather than assumed, and the assumption was
wrong. The sign columns were supposed to be three cheap map reads per facet;
an ablation attributed **all** of the added 0.331 CPU-seconds per world to one
of them, the wetness sign's nearest-channel query, with the biome, rock and
steepness reads free. That is over the third-of-a-second budget the design set
itself. It was accepted rather than trimmed: sampling every second facet would
halve the population, double the null's bias and widen its spread from 0.0020
to 0.0035 bits — blunting the controls to save about twenty seconds of
measurement wall time. The budget was written for a cost it did not foresee,
and the instrument's resolution is what the campaign is for.

## The inversion

The previous campaign's recipe multiplies an **abundance** by a mixture of a
**cause** term and a noise term, weighted by a **contextuality**. Regrouped,
that is

```
prevalence = reliability · cause + floor · noise
```

— the same two degrees of freedom, rotated onto the two quantities the readout
actually measures. **Reliability** is how often the feature pays where its
cause saturates. **Floor** is how often it appears where no cause is. The
campaign authors those two directly, per kind, and lets the frequency be
whatever the world's cause geography makes it.

Both sign kinds' floors were set to **zero**, and stayed there: no frozen band
ever required lifting one, which the calibration protocol demanded be stated
if it had.

A zero floor is not enough on its own, and the cause distribution says why.
Even with the noise term multiplied out, a response proportional to the cause
would still put more features on the 10,754 facets whose spring cause is near
nothing than on the 122 where it saturates — the low bin is simply that much
larger. A sign has to be concentrated where a *level set* is, because a level
set is what a word names. So the two sign kinds read their cause through an
authored soft step whose edges are the same thresholds the words cut at: the
class a walker can name is the class the feature lives in. Which facets inside
that class actually carry the feature is still decided by a decorrelated
position-continuous draw at the kind's own correlation length — the original
guard against wallpaper, untouched.

The thicket and the erratic keep the previous campaign's expression exactly,
character for character, rather than an algebraically equal regrouping. The
two forms can differ in the last representable unit, and an occurrence is a
comparison against that value, so a regrouping alone could flip a facet. Both
are the campaign's non-regression controls, and a byte-golden recorded on the
unchanged tree — before the recipe moved — proves their derived output
identical afterwards. The golden was itself proven discriminating by a
one-unit perturbation before it was trusted.

## Calibration, including the part that was withdrawn

Eight constants were set on seed 42 alone, against bands frozen in advance,
before any measurement seed was built.

The instrument that set the step edges is worth naming, because it made the
search cheap. The expected found fraction is a ratio of two sums of the step
function over the cause distribution — and the reliability *cancels out of
it*. So the edge pair alone decides whether the found-fraction band is
reachable, and the reliability afterwards sets only the frequency. Its own
prediction was checked against a measured reading before it was used.

One discipline choice constrained the whole search. Both lower edges were kept
strictly below 0.5. The found fraction counts occurrences standing on a cause
of 0.5 or more, so a step whose lower edge sits at or above that reads
**1.000 by construction** — the metric would be measuring the constant instead
of the world. An edge pair of (0.45, 0.55) was available for the spring and
predicted a found fraction of 0.955. It was rejected for that reason, not on
its numbers.

Four rounds moved the found fractions from 0.567 and 0.301 to the high
sevenths, and three of the four frozen bands were met on the second. The
fourth was not a band about a kind at all, and that is the part worth
recording.

**A bar a kind can pass by disappearing is defective.** The frozen predictions
included a clause requiring the spring's channel information to exceed the
overhang's. Information in bits scales with the event's own entropy, so a
between-kind ordering can be satisfied by making the *lower* kind rarer — and
round three did exactly that, cutting the overhang's reliability to 0.16 and
its frequency twelvefold to clear a clause about the spring, while the
spring's own reading did not move by a digit. The clause was withdrawn from
the gate on the calibration seed, before any measurement seed existed. Each
kind is now judged on its own legibility, and the ordering is reported both
raw and normalised by each kind's entropy.

The overhang was then re-calibrated on its own bands alone, and the ladder is
the clearest thing the campaign has to say about why:

| overhang reliability | found | best-class lift | learner gain | max class rate | density | verdict |
| ---: | ---: | ---: | ---: | ---: | ---: | --- |
| 0.16 | 0.77143 | 19.176 | 0.01893 | 0.11966 | 0.00624 | passes |
| **0.50** | 0.77178 | 21.484 | 0.06954 | 0.46154 | 0.02148 | **chosen** |
| 0.75 | 0.76068 | 20.487 | 0.09436 | 0.64103 | 0.03129 | passes |
| 0.90 | 0.75962 | 20.513 | 0.11710 | **0.76068** | 0.03708 | wallpaper |

At 0.90 a sign class of at least a hundred facets carries an overhang
seventy-six percent of the time, past the wallpaper bound of 0.75 — so 0.75 is
the measured ceiling rather than a preference, and the bound binds at about
0.888. The middle of the passing range was taken rather than the top, for two
reasons. It leaves 1.62× of headroom under the wallpaper bound against 1.17×
at 0.75, and a measurement seed whose hottest class ran seventeen percent
above seed 42's would otherwise have failed for a reason about the margin
rather than about the world. And a feature present on three saturated facets
in four is close to being its cause's restatement: one in two is the
found-not-told band the campaign exists for.

**The spring is identical in all three rungs.** Its ceiling-legibility reading
is 0.086464 at every one; the entire movement is the overhang's. Read against
the previous campaign's 0.007812 on the same statistic and the same seed, the
spring rose **11.1×** — which is the comparison the campaign actually claims,
and the reason a between-kind comparison in bits is now printed rather than
enforced. At the 0.16 rung the previous campaign's whole falsified ordering
came back *exactly as it had been preregistered*. A prediction confirmed by
making a different kind twelve times rarer is not being tested by its
confirmation.

The eight constants as shipped: the spring pays 0.95 of the time where its
cause saturates, over a step from 0.35 to 0.55; the overhang pays 0.50, over
a step from 0.35 to 0.65; both floors are zero.

## The readout

Four seeds — 13, 7, 1 and 100 — were built once, after the constants were
frozen, and every band was read against them. Seed 42 was printed beside them
and gated nothing. The calibration file carries a test that scans its own
source and fails if any of the four measurement seeds is named there.

Counting each band on each seed as one instance, the readout applies eighteen
per seed, **seventy-two in all. Seventy-one held.**

| band | seed 13 | seed 7 | seed 1 | seed 100 | min / median |
| --- | --- | --- | --- | --- | --- |
| spring found ≥ 0.60 | 0.78161 | 0.79399 | 0.68519 | 0.77273 | min 0.68519, med 0.77717 |
| overhang found ≥ 0.60 | 0.84471 | 0.87199 | 0.74348 | 0.84773 | min 0.74348, med 0.84622 |
| thicket found in [0.30, 0.55] | 0.37933 | 0.33436 | 0.46534 | 0.43659 | med 0.40796 |
| spring lift ≥ 2× the control's | **4.759 vs 5.253** | 5.410 vs 4.556 | 5.551 vs 3.462 | 7.607 vs 3.890 | ratio min **1.812**, med 2.791 |
| overhang lift ≥ 2× the control's | 11.422 | 12.495 | 27.707 | 10.266 | ratio min 4.350, med 5.382 |
| spring learner > 0 | 0.00455 | 0.03162 | 0.02817 | 0.03802 | min 0.00455, med 0.02990 |
| overhang learner > 0 | 0.10793 | 0.10945 | 0.04797 | 0.11476 | min 0.04797, med 0.10869 |
| thicket learner > 0 | 0.03485 | 0.03394 | 0.02940 | 0.02514 | min 0.02514, med 0.03167 |
| control learner ≤ 0.001 | −0.01098 | −0.00912 | −0.01443 | −0.01106 | worst −0.00912 |
| control channel net ≤ 0.008 | 0.00134 | −0.00142 | 0.00023 | −0.00015 | worst 0.00134 |
| false-sign net within ±0.002 | 0.00037 | 0.00043 | 0.00088 | 0.00053 | worst 0.00088 of 16 |
| max class rate ≤ 0.75 | 0.46635 | 0.51073 | 0.42991 | 0.47630 | worst 0.51073 of 16 |

**The one that did not hold is the headline.** On seed 13 the spring's
best-class lift reads **4.759** against a bar of **5.253** — twice the
causeless control's 2.626 — a ratio of **1.812** where 2.000 was frozen.
Ninety-one percent of the bar, short by half a unit of lift.

Nothing was retuned. No constant moved and no bar moved after the reading, and
the failing assertion is left failing, because it is the record.

The failure reads as one coherent story rather than two coincidences. Seed 13
is where the spring is rarest — an existence density of 0.00606, the lowest of
the five worlds and 2.2× below seed 100's — and it is simultaneously the seed
where the *control's* lift is largest, 2.626 against 1.731 to 2.278 elsewhere.
Both halves of the ratio moved against the spring on the same world, and the
mechanism is the same one in both: the fewest occupied facets is where a lift
ratio against a cardinality-driven null is least favourable.

A fact about the bar's own denominator was recorded and deliberately not acted
on. The control's lift varies **1.52×** across the four seeds, so the bar it
multiplies is itself seed-dependent by half again, and the seed that failed is
the seed with the largest denominator. That is an observation for a successor
to derive a steadier denominator from *before* measuring — never a licence to
move this one after.

The withdrawn ordering clause was read out too, and **would have failed on all
four seeds**, raw and normalised. Withdrawing it on the calibration seed was
therefore not the rescue of one reading: the ordering holds nowhere, in either
form, and the overhang is simply the more legible of the two kinds — its
induration and its slope *are* the rock word and the steepness word, while the
spring's drainage is no rendered word at all.

The learner recovers most of what the channel carries. Its gain as a share of
channel information net of null reads 0.568 to 0.978 for the spring, 0.960 to
1.025 for the overhang, and 0.649 to 0.729 for the thicket. The same quotient
for the causeless control is printed and is meaningless: its net is within
noise of zero, so the ratio is a division by nearly nothing. The control's
real verdict is the *sign* of its learner gain, which is negative on every
seed — a table fitted on noise must lose to the base rate, and does.

## What the frequency turned out to be

Nothing set it. Over the land-eligible population:

| kind | seed 13 | seed 7 | seed 1 | seed 100 | seed 42 |
| --- | ---: | ---: | ---: | ---: | ---: |
| spring | 0.00606 | 0.01207 | 0.01457 | 0.01972 | 0.01337 |
| overhang | 0.04083 | 0.04087 | 0.01552 | 0.04640 | 0.02148 |
| thicket | 0.11057 | 0.11744 | 0.13918 | 0.16629 | 0.13523 |
| erratic | 0.04027 | 0.03922 | 0.04345 | 0.03880 | 0.03815 |

The two untouched kinds sit in the narrow bands their unchanged recipes
predict, which is the non-regression result stated as a distribution rather
than as a single equality. Against the previous campaign's figures rescaled to
the same population, the spring at seed 42 is **2.7× rarer** and the overhang
**3.5× rarer**. That is what "found rather than extruded" costs.

The other half of the cost is what a walker actually meets. Existence density
is a god's-eye number; the encounter rate over seventy-eight walks of sixty
steps is the walked one, and for the spring it reads **0.00000 on seeds 13 and
42** and 0.01305, 0.01577 and 0.00247 on the other three. So the spring is
encounterable in the walk band on three of the four measurement seeds and
absent from it on two of five worlds.

Seed 42's zero is not a rounding artefact and has an exact cause. The walk
band is seventy-eight sixty-step walks — seventy-eight *locations*, since
sixty adjacent facets cover one small patch — and the largest spring cause
anywhere in that sample is about 0.244, below the 0.35 at which the step
opens. On that world the found-fraction band and walk-band visibility of the
spring are **mutually exclusive**: recovering a nonzero walked reading needs a
lower edge under 0.244, and at the edges that give one, the found fraction
falls back to the 0.567 that failed. The frozen band governs, and the silence
is recorded rather than tuned away.

Recording it correctly took some care. The occurrence-count and spread floors
that guarded the spring's walk-band statistics were dropped to zero, and a
witness now asserts the exact zero beside them — because a floor of zero can
only be satisfied, so without the witness the row would be a vacuous pass. The
autocorrelation arm asserts the constant-zero series rather than comparing a
not-a-number against a threshold, which is false and would have reported
"autocorrelation too low" for a series that has none. A window battery that
used the spring as its probe kind moved to the thicket behind one named
constant, rather than loosening three real non-vacuity checks into assertions
that cannot fail.

And one claim was corrected in place rather than left to inference. With a
floor of zero the noise term is multiplied out of the sign kinds entirely, so
the address-hashing mutation those batteries are built on cannot move their
numbers at all. Their rows now discriminate the continuity of the *macro
state* — a real property, and a different one. The thicket and the erratic
keep the original expression and the original guarantee.

## What the next campaign inherits

A world that says what is underfoot, an instrument that measures what that
buys, and one bar that did not clear.

**A body that speaks its inference.** The signs are now visible to every
walker, and a held-out table over them pays. What does not exist is a mouth: a
body whose species is steeped in the concept saying *limestone country; there
will be water below*. That is the diegetic form of this campaign's oracle
observer, and it is a further campaign — this one made legibility reachable
with the player's own head, deliberately.

**Remote signs.** Every sign here is read at the facet the walker stands on.
A neighbouring facet's word, the channel band a step away, and eventually a
rumor are all *remote* signs, degraded by distance or by transmission. The
learner could tabulate the previous step's words on the same walks for almost
nothing.

**Relief still has a cause nothing reads.** The relief axis — *on a rise*, *in
a hollow* — is an address draw, and sub-vertex height has been available as a
real cause for it since an earlier campaign. Grounding it would make a sign of
something no kind currently reads, so it waits for a kind that does.

**The walk-band question, unresolved.** A found sign is one whose occurrences
concentrate where its cause is; a met sign is one a sixty-step walk actually
passes. On seed 42 those two are in direct conflict for the spring, and the
campaign chose the first because it was the one frozen in advance. Whether the
walk band should be sampled differently, or the spring's cause geography
widened, or the trade simply accepted, is a design question this campaign
states rather than settles.

**A denominator that moves.** The bar the spring failed is twice a control's
own lift, and that control's lift varies by half again across four worlds. A
successor that wants a steadier comparison must derive one *before* measuring
anything with it.
