# The Domesday

The Domesday Book of 1086 is remembered as a survey, but its contemporaries
remembered it as an *ordeal*. It went into every shire and asked, of every
holding, what it was worth — and the answers were not flattering, because an
honest inventory of a kingdom is mostly an inventory of what the kingdom does
not have. This campaign built the same instrument for Hornvale: a generated
survey of the thousand-world census that says what the generator actually
produces, and where it is thin.

The project sprinted end to end early — star generation through walking around
a room — and that sprint accrued a particular kind of debt: real things that
should have real effects, whose effects were never wired or never measured.
Nothing in the repo could see that debt in aggregate. Individual campaigns
found individual instances; the census held 191 measurements across a thousand
worlds and nobody had ever read all of them at once.

## What was already there, and the four things that were not

Almost all the machinery existed. A thousand rows by 194 columns sat committed
at `book/src/laboratory/generated/the-census/rows.csv`; every column already
carried a `kind`, a `doc`, a build `rung`, and summary buckets; the Lab already
rendered distributions deterministically; decision 0032 already established
that the gate reads the committed fixture rather than rebuilding it.

Four things were missing, and they were the whole campaign.

**Summary statistics.** The renderer emitted distributions and nothing else —
no minimum, maximum, mean, median, or quartile for any of the numeric metrics.
A distribution over declared buckets cannot answer *is this metric frozen*.

**A subject axis.** `rung` is a build depth, not a subject: 105 of the metrics
land in `full`, mixing religion, language, and naming into one undifferentiated
pile. Metric names carry no usable prefix — `vestige-density`, `pantheon-size`,
`cult-form` — so nothing could group them by what they are about.

**Comparators.** There was no world, real or imagined, to measure against. A
median land temperature is a number; a median land temperature *twenty-six
degrees below Earth's* is a finding.

**Weakness detection.** Nothing mechanically flagged a degenerate distribution.

## The acceptance test was an existing finding

One registry row already carried a defect of exactly the shape the survey was
meant to surface, measured from this same census: median land temperature
−11.9 °C, 651 of 1,000 worlds ice-dominant, and correlations against mean land
temperature that all sat near zero — ocean fraction r = −0.000, obliquity
r = +0.041, orbital period r = −0.245. *The climate is not merely cold; it is
near-uninfluenced by its own astronomy.*

That is a serious generator defect, and it was buried in a row of a table. The
spec made finding it the campaign's acceptance test rather than an
illustration, and named the exact routes: an off-comparator detector must fire
on the temperature against Earth, a declared-strength detector must fire on the
correlation, and the degeneracy detector must **not** fire on ice dominance,
because 65.1 % is not a degenerate distribution and lowering the bar to 60 % so
that it would have been metric-chasing. All three held.

## Eight detectors, frozen before the data

The detectors were preregistered with their thresholds, in the spec, before the
code that would move them. Two of the eight are worth describing because their
design changed under argument rather than under measurement.

**Roles.** The first draft had a degeneracy detector firing on 39 of the 58
categorical metrics, which would have made the weakness section unreadable —
the falsification clause's predicted failure, arriving before a line of code
was written. The cause was not a bad threshold. It was that a metric asserting
an invariant (*this language's phonotactics are valid*, `true` on every world by
design) and a metric measuring a property expected to vary are different kinds
of claim, and the census had no way to say which was which. Every metric now
declares a `role` — `descriptor` or `invariant` — and the degeneracy detectors
fire only on descriptors. An invariant that *does* vary is a stronger finding
than any degeneracy, so it got its own detector: a claim the data contradicts
outranks a distribution that is merely narrow.

**A declared class, not a threshold.** The mis-declared-strength detector was
first specified as a minimum absolute correlation of 0.50, chosen with full
knowledge that the measured value was 0.245 and would therefore fire. No
justification could make that number credibly independent — the value was
measured before the judgement was frozen, which is a phase-order violation
whatever the arithmetic says. The fix was not a better number but a different
*source*: an expectation now declares a strength **class** (dominant, strong,
moderate, weak, none), the observed correlation is mapped to a conventional
effect-size band, and the detector fires on the class mismatch. The finding is
that something declared dominant is observed weak, which is a statement neither
side of the comparison can be tuned to produce.

## The prediction that was falsified

One preregistered criterion said the degeneracy detector would fire on at most
ten descriptor metrics after the role split. It fires on **twenty-seven**. The
split did real work — forty unsplit down to twenty-seven — but far less than
predicted, because the ceiling had been set without ever measuring the
post-split distribution.

The twenty-seven were inspected before anything was ruled, and they are
overwhelmingly real. Ten metrics do not vary *at all* across a thousand worlds:
the dominant soil order is `leptosol` on every world, every pantheon is `flat`,
every goblin head deity is `solar`, every dominant hazard is `structural`. So
the detector was right and the prediction was wrong. The threshold was not
moved and the ceiling was retired rather than raised to a number that would
have flattered the outcome.

Two of the twenty-seven are not defects at all — `tidally-locked` is false on
95.2 % of worlds and `refused-a-moon` on 95.3 %, both correct physics. A
declared *expected-skew* class would separate those out, exactly as `role`
separates invariants. It was deliberately not added, because adding a
count-reducing mechanism after seeing the count is the same phase-order
violation that had already cost the strength detector its threshold. It is a
follow-on, to be preregistered before the next run or not at all.

## What the survey found

113 detector firings over 73 distinct metrics — 38 % of the census. The raw
count is larger than the metric count because the detectors overlap by
construction: a frozen metric's median trivially equals its minimum, so every
frozen finding is also a rail finding. The rendered pages group by metric so
that no reader counts one weakness three times.

Three findings are worth the chronicle.

**The census never measured the thing the climate conclusion is about.** Of the
twenty-three astronomy metrics, not one is insolation, stellar luminosity, or
orbital distance. The facts exist — `anchor-orbit-au` and `insolation-rel` are
registered predicates committed to every world's ledger, sitting in the same
block as `brightening-per-gyr`, which the census *does* read. The conclusion
that climate is near-uninfluenced by its astronomy was drawn from a dataset
that never contained the influence. The conclusion may well be right; the
evidence for it is one orbital-period proxy, and the survey's job is to say so.

**A calibration metric that is arithmetically forced to its extreme.** The
chorus sky-calibration metric is a Kendall tau, valid over [−1, +1], and it
reads exactly **−1.000 on all one thousand worlds**. It is not a sentinel leak.
It is a rank correlation between a culture's sky capability and its sky-domain
distortion — and distortion in that domain is the fraction of sky facts *lost*,
while a sky fact is lost precisely when the culture's capability falls below the
fact's threshold. The two series are coupled through the same comparison, so
every strictly-comparable pair is discordant and the coefficient saturates. The
metric cannot distinguish one world from another, and the preregistered study
that reads it is scoring a tautology. This is the Confidence Gradient's
cannot-fire family, found for the first time in a *metric* rather than in a
test.

**Nothing about a world reaches its creatures.** Fourteen biology metrics are
frozen across all thousand worlds — lifespan, age at maturity, generation
length, basal metabolic rate, pace of life, reproductive tempo, perceptual hue
depth, each for two species. That is not a coincidence of seeds. Life history
is a pure allometric function of authored mass, metabolic class, and schedule,
so a thousand different worlds produce physiologically identical creatures. The
frozen value is the symptom; the finding is that the world is not an argument.

## What it cost, and what it must never do

The survey reads the committed CSV and builds no world. It renders in
**0.15 seconds** — the gate pays nothing measurable for it, which is the whole
reason it can be regenerated on every rebaseline and drift-checked like any
other artifact. A test asserts the module never constructs a world, and that
test began life as a quine: it scanned its own file for forbidden words, found
them, and could therefore never fail. Relocating it and proving it red by
injecting a violation was the campaign's first instance of a shape it would
produce three more times.

The drift check has its own hazard, inherited from The Digest: `git diff
--exit-code` against a path with no index entry is silently vacuous, so a new
generated directory must be added to the index in the commit that introduces
it, and nothing in the regeneration script guards that. The survey's directory
was added, and the check was then proved able to fail — one value in the census
mutated, the drift check red, the rendered maximum moved to exactly the injected
number, the mean shifted by exactly the arithmetic that value implies, and a
third detector fired on the widened range. Restoring the value from a backup
returned the tree byte-clean.

## A gap in the world is rendered; an error in the instrument is fixed

The campaign's sharpest principle came out of nearly shipping a lie.

An early annotation pass, classifying every metric by subject, produced a
`Hydrology` domain with zero metrics. The standing rule — a gap discovered
while building the instrument gets *rendered*, not quietly repaired, because an
absence fixed in passing is one the instrument never learns to see — said to
publish the empty chapter. Review overturned it. Water is measured: twelve
hydrology metrics were sitting under `Terrain`, filed there by mechanical
inheritance from the build rung. Rendering the empty page would have asserted
something false about the project, in the one artifact whose entire purpose is
being trustworthy.

So the rule needed a distinction it did not have. **A gap in the world is
rendered. An error in the instrument is fixed.** No insolation metric, a frozen
reproductive tempo, a domain crate nothing measures — those are the world, and
they are the findings. A metric filed under the wrong subject is the
instrument, and publishing it faithfully publishes a mistake. Sixteen metrics
moved. The finding worth keeping was never the empty chapter; it was that
mechanical rung-inheritance misfiled sixteen metrics, which is a lesson about
the annotation method and not about water.

## The verdict on the falsification clause

The spec bound the campaign in advance: if the detectors fired on so many
metrics that the output was unreadable — or on so few that known defects slipped
through — the campaign would report the threshold calibration as its headline
and ship the raw survey *without* the weakness section, rather than tuning
thresholds to look good.

Read end to end, the survey is readable. Each domain page carries its own
weakness section, grouped by metric, and the largest of them lists
twenty-two metrics — long, but a list a person finishes. Six of the twelve
domains carry three findings or fewer. The two extremes are informative rather
than noisy: `Terrain` has exactly one weakness across eighteen metrics, and
`Biology` has one on every metric it has, which is itself the finding about
biology. The clause is not triggered. The weakness section ships.

Twenty-seven degeneracy findings over 191 metrics is what *make weaknesses
visible* looks like when there are many weaknesses, and a survey that found
nothing would have been the failure.
