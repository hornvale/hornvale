# The Hollow

A cave asks three questions — does one exist here, of what kind, and how deep
does it go — and in this world all three were asking the same field.

The campaign that intended to build an underworld measured the ground it was
going to build on first. Over thirty worlds and 469,122 land cells, a quarter
of one per cent of the land held a cave, every one of them was the same kind,
every one of them stopped at the same depth, and three worlds in thirty held no
cave at all. The thousand-world census had been publishing the same finding
since the model shipped: 999 worlds in the lowest bucket it can report. Seed
42's almanac read *"0% of the land is cave country."*

That is a substrate nothing can be built on, so the underworld was set down and
the substrate was fixed instead.

## The concentration

Three defects, and they were symptoms of one thing.

Existence was gated on `carbonate × porosity × wetting` — a sound dissolution
model, and the right one for limestone. Kind was then asked *after* the gate
had already passed, and asked it inverted: the lava-tube branch required
carbonate to be low, and so did the fracture branch. A cave could therefore
only be born on a cell where the limestone test had already succeeded, and two
of the three kinds had been unreachable code since the day they were written.
They were not unreachable for want of eligible rock — 26.5% of land is
mafic enough to have drained a tube and 59.9% sits near enough a plate contact
to be faulted. The ordering forbade them, not the geology.

Depth read carbonate a third time, through `1 + proneness × 3` truncated to an
integer. Reaching the third band needs a proneness of two-thirds; the fourth
needs exactly 1.0. The quantity's *theoretical* ceiling was 0.573 and its
measured maximum 0.5073. Every cave in every world sat at band two, and that
was the arithmetic working exactly as written. A range that exceeds its own
input's ceiling is not a rare outcome; it is an unsayable one.

**And nothing was reading the answers.** The depth field was constructed,
declared, documented, and read by no code anywhere. The kind was read in
exactly one place — a map palette — and a map that draws one colour is not
obviously drawing one colour. Three structural defects survived a campaign, a
census metric, an almanac line, and a committed picture, because the fields
they corrupted had no consumer to be wrong in front of.

## The dominant defect was none of those three

The presence gate compared a computed probability against a four-octave
fractal-noise sample. Noise of that construction is a sum of three interpolated
lattices; its marginal is near-Gaussian and massed around one half, not spread
uniformly over the unit interval. Comparing a probability against it is not a
Bernoulli trial — it is a threshold against a bell, and the model had been
calling the result a probability since it was written.

Measured over 655,488 samples across sixty-four worlds, the field's marginal is
mean **0.500274**, standard deviation **0.076443**, skew −0.010, excess
kurtosis −0.059. The gate's probability never exceeded 0.4132 anywhere on land,
so it operated *entirely inside the left tail of that bell*. A nominal 0.325
fired at 0.011 — twenty-nine times low.

The sibling code proves the point without meaning to. The ore point process
runs the identical gate and reads perfectly healthy, with a deposit somewhere
on more than nine tenths of every world's land. It reads healthy because the
areal ores — evaporites and placers, laid down in beds rather than placed at
points — skip the noise test entirely. The census's dominant commodity is salt
in 98.6% of worlds, and salt is an areal ore. **The features that appeared
reliably were exactly the ones that never asked the gate.**

## The repair

The sibling ore model already had the right shape and had never had a dead
branch, for a structural reason: it chooses the kind *first*, from the fields
that kind's genesis actually requires, and only then asks whether one is
present. Caves now do the same.

Each kind computes its own proneness from what its process needs. Dissolution
keeps `carbonate × porosity × wetting`, unchanged, because it was never the
part that was wrong. A lava tube needs mafic rock, extrusive texture, and young
crust, because a roofed void in vesicular basalt is destroyed continuously by
collapse and burial — terrestrial tubes are a Holocene feature where karst
caves last for millions of years. A fault void needs stress and rock competent
enough to hold an aperture open. Selection is the strongest of the three rather
than a priority ladder, so the mix follows the fields instead of following a
hand-chosen order, and no branch can be dead because each kind is selected *by*
the very field it is then gated on.

Existence is then gated on the selected kind's own proneness, against a noise
sample warped through the measured marginal's cumulative distribution. The warp
is **monotone**, and monotone is the whole argument: the noise field was doing
two jobs at once, setting the presence rate and making caves *cluster*, and it
was doing the second one beautifully — 96.7% of cave cells already had a caved
neighbour. A monotone transform preserves the spatial ordering exactly. The
marginal is corrected and the clustering cannot move, by construction rather
than by measurement. It is applied at the cave's own call site and nowhere
else, because the two other readers of that field are not defective and would
break if it changed underneath them.

Depth reads the cell's stratigraphic column, and is typed as a named band
rather than a count — the form the ore model had already established for the
same question. Dissolution works the sedimentary cover and reaches the basement
contact where thin cover sits on ancient rock. A tube is the flow it drained
out of and never leaves the cover. Faults cut crystalline rock, and strong ones
reach the roots. A band derived from bands cannot reproduce a count derived
from a ratio.

## What the numbers did

```
                     before        after
prevalence          0.2554%      11.9259% of land
caveless worlds       3 / 30         0 / 30
Karst               100.00%        40.84% of caves
LavaTube              0.00%        17.58%
Fracture              0.00%        41.58%
band Cover          100.00%        40.03% of caves
band Basement         0.00%        36.12%
band Roots            0.00%        23.85%
clustering          96.7446%      98.5218%
gate, worst bucket   29x low       within 5.6% of nominal
```

The thousand-world census moved with it. The floor bucket that held 99.9% of
worlds now holds **none**; 32.4% of worlds fall between five and ten per cent
of land, 67.3% between ten and twenty, exactly one above twenty, and none above
thirty. All five preregistered criteria pass.

The clustering guard is the one worth dwelling on, because it was the campaign's
falsifier rather than its trophy. It was frozen as the fragile claim asserted
against the robust one: if the warp had not been monotone, or if the noise's
spatial structure had not survived it, the central argument for the whole
approach would have been false and the design would have had to be reconsidered.
It went *up*, from 96.74% to 98.52%, because the same field now places many more
features and a denser point set has more neighbours.

## A sentence woke up after two weeks asleep

Seed 42's almanac went from *"0% of the land is cave country"* to six per cent,
and gained a line beside it that had never once fired since the day it was
written:

> 203 cells hold both cave and ore — the deep worked twice.

The co-location claim was the organizing idea of the campaign that placed these
features: a cave and a vein are two faces of one subsurface fluid flow, so cave
country and ore country coincide by construction. The prose to say so was
written, shipped, and drift-checked, and it had been waiting on caves that did
not exist. Nothing was wrong with it. It simply had nothing to describe.

## The headline is a partial null

The disease was three questions sharing one field. This campaign gave **kind**
its own fields and moved **depth's base band** onto the column, and it did not
fix the third. The deepening step — whether a process is strong enough to reach
one band further down — still reads the same scalar that existence is gated on.
"How likely is a cave here" and "how deep does it go" are still being asked of
one number.

They want opposite calibrations, and this is not a matter of tuning. Fracture's
proneness must peak at or above the deepening threshold or the deepest band
never occurs at all and the depth criterion fails outright. But any *smooth*
field peaking that high puts a broad plateau over the 9.4% of land that sits
directly on a plate contact, and that plateau floods fracture's share of the
mix. The two requirements are coupled, and the thing coupling them is that one
scalar is answering two questions.

That is also what explains the campaign's one missed target without any appeal
to tuning. Karst was aimed at 45–70% of caves and landed at **40.84%** — and
Karst's total possible mass, the sum of its own proneness over every land cell,
is 24,242 cells, **5.17% of the land**. Carbonate in this world is effectively
a binary flag: 133,293 land cells sit below 0.1, 20,956 sit at exactly 0.7, and
none lie between. The shipped model realizes 22,846 of those 24,242 cells —
**94% of the arithmetic ceiling**. Karst is not being suppressed; it is at its
cap. The target was unreachable on the day it was written, and computing the
ceiling before writing the target would have shown that.

Two of three concentrations dissolved, the third named and measured and left
standing, with the reason it could not be dissolved from inside this campaign's
scope. That is the finding, and it is stated as one rather than as a shortfall.

## Two disclosures

The project's rule is that a constant retuned after the results are seen must be
*said*. Both of these are stronger than a retune, and both are said here.

**A formula was rewritten after the first readout, not merely recalibrated.**
The fracture model, written fresh by this campaign, reproduced the campaign's
own bug class twice over. It multiplied competence by one minus metamorphic
grade — and metamorphic grade in this world is *defined* as a decreasing
function of distance from a plate contact, so the two terms were anti-correlated
by construction. On a fault, the most faulted place a world has, fault-void
proneness was exactly zero. And its maximum over all land was ~0.393, below the
0.5 threshold that reads it, so the deepest band was not rare but unreachable —
a ceiling beneath its own threshold, which is precisely the defect the campaign
exists to remove. Metamorphic grade records peak burial pressure and
temperature, a rock's *history* rather than its present rheology; gneiss and
quartzite are among the most brittle rocks at cave depth. A second term gave
every continental interior a third of maximum fault stress, because it reused a
weighting function whose floor is correct for ore belts and wrong for faults: a
term that cannot fall below 0.3 cannot say *far from any fault*.

Each change carries a mechanism argument true independently of any target. "A
fault-void model returning zero proneness at the fault is wrong" is true whatever
the readout says. The test for whether this was metric-chasing is what happened
to the settings that had only a number to recommend them: a sweep of more than
280 parameter combinations does contain settings that hit the Karst target, and
they were **declined**, because reaching them required a stress reach that
degenerates to the contact cell alone and a survival exponent nothing argues for.

**The gate-calibration readout compares against a bucket's mean rather than its
midpoint.** The plan wrote midpoint; the shipped test reads the mean, and under
the midpoint reading the criterion would be red. The mean is the correct
estimator and this is decisive rather than a preference: for independent
Bernoulli trials with probabilities `pᵢ`, the expected number of hits is
`Σpᵢ`, so the expected *rate* is exactly the mean. A midpoint is a proxy, valid
only where a bucket's interior is evenly spread. The lowest bucket holds 298,276
of 469,122 land cells massed near zero; its mean nominal probability is 0.01484
against a midpoint of 0.025, and the gate fires at 0.01525. Against the mean
that is an agreement of 2.8%; against the midpoint it reads as a 39% miss. The
midpoint would have failed a correctly-calibrated gate. The threshold was not
touched, the change was made after the numbers were seen, and both readings are
printed side by side so the difference stays visible.

## What this leaves

The point-ore half of the same gate defect is untouched and deliberately so. It
is masked by the areal bypass rather than fixed by it, so gold, gems and copper
are presently suppressed the way caves were, and nothing has yet measured by how
much. Fixing it moves four census columns and every deposit's grade and tonnage,
which is a materially larger disturbance for a model showing no visible symptom.

The cave taxonomy remains three lithologic processes and no environmental ones —
no sea caves, no glacial or periglacial voids, no biogenic ones. A cave's
chemistry can differentiate it; its setting cannot.

And the underworld this was built for can now be built. A chamber graph reads a
depth budget, and until this campaign there was one depth in every world.
