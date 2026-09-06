# The Lot

*A lot is what chance draws, and a person's lot is the life they were dealt.
Both senses are the campaign. It draws one representative life from everyone
who ever lived in a seeded world and tells it from the committed record alone —
so that every sentence the story cannot say is a measurement rather than a
missing feature.*

## The source, read once

Any Human Ever draws one life from the hundred billion humans who have ever
lived, in four staged reveals. **When** is a birth year drawn off the human
population curve, log-scaled because on a linear axis all of history is a spike
at the right edge. **Where** is a place drawn off a population map for that
year. **Life** is the demographic odds at that people, place and year — life
expectancy, died before five, died before fifteen, causes of death, marriage
and children — and then one life drawn from those odds as a timeline. **Story**
is a templated biography with about two dozen tiles, each carrying its sources.
One seed pre-rolls the whole human; the permalink is that seed. And the rule
everything else follows from: **a tile is absent, never invented.** Where no
source covers the region and the era, the site says nothing, on the stated
ground that an invented figure would be worse than silence.

That rule is, in this world, already constitutional. The explain window
narrates a world by reading only committed facts and never the running system,
and it does so *because* that is what validates the record as sufficient. So
the analogue is not a feature borrowed from a website. It is an instrument: a
slot the story cannot fill is a fact about the world's own record — nothing in
it would let anyone say this about an individual — and the count of those slots
is the campaign's headline number.

It is also the **dual of the census**. The census aggregates the record into
two hundred columns over a thousand worlds; the lot disaggregates one world
into one row. Same instrument family, opposite direction, and the lot sees a
resolution the census cannot: whether the record can say anything about a
*person*, as opposed to a settlement, a people or a sky.

## Four kinds of individual

The world already held three kinds of individual, and this campaign added a
fourth. The distinction is worth stating exactly, because three of the four are
uncommitted and it is easy to read that as three of a kind.

```
  kind             who drives it   committed?   where it lives           span
  ---------------  --------------  -----------  -----------------------  ----------------
  founder          the world       YES          the person domain        a dated life
  roster member    the world       no           the vessel window        the present
  possessed body   the reader      no           the vessel window        the present
  lot              the reader      no           the lot window           a whole life,
                                                                          usually dead
```

A founder is the only one the world *remembers*. A roster member and a
possessed body exist in the standing present, with a body and a position. A lot
is a whole life laid across two thousand years of deep history, and it is
usually over before the world you can walk in begins. When a lot's community
descends from a founder the story says so, and that one sentence cites a fact
the world committed about somebody else.

## The population a life is drawn from

Nothing about the draw was designed before the shape of the draw population was
measured. A probe over nine seeds read the committed occupation facts only —
founded, ended, peak population, people, cause — and reported the shape of the
person-years a lot would be drawn from, proxied as tenure times peak, which is
an upper bound because a community spends part of every tenure below its peak.

```
  seed  occs  alive/ended   Q1     Q2     Q3     Q4     ended  25-49  >=50   curve 0 -> 1900
  ----  ----  -----------  -----  -----  -----  -----  -----  -----  -----  ----------------
     1  1238   313 /  925  0.115  0.210  0.295  0.380  0.301  0.591  0.286   1679 ->  8945
     2  1847   508 / 1339  0.093  0.180  0.310  0.417  0.335  0.512  0.276   1512 -> 12784
     3   712   287 /  425  0.113  0.198  0.295  0.394  0.138  0.725  0.203   1602 ->  9728
     7   656   250 /  406  0.139  0.236  0.295  0.331  0.125  0.584  0.288   1931 ->  7539
    13  1061   262 /  799  0.146  0.226  0.284  0.343  0.249  0.479  0.383   2043 ->  7248
    42  1212   390 /  822  0.105  0.197  0.300  0.398  0.182  0.579  0.251   1636 -> 10754
   100   159    60 /   99  0.235  0.257  0.257  0.252  0.051  0.366  0.000    882 ->  1061
   256  1470   385 / 1085  0.079  0.120  0.285  0.516  0.253  0.480  0.298   1135 -> 10846
   777  1049   331 /  718  0.109  0.182  0.290  0.420  0.219  0.518  0.346   1786 -> 10245
```

Three findings, and the first is the one the whole exhibit rests on. **Eight of
nine worlds are still growing at the present moment**: the last five-hundred-year
quarter holds a third to a half of all person-years against eight to fifteen
percent in the first, and the summed peaks of the living communities rise by a
factor of three and a half to ten across the span. So the site's sentence —
almost everyone was born recently — is true of these worlds too, and for the
same reason. That was not obvious in advance. A world that had reached its
carrying capacity would have read flat, and **seed 100 does**: a
159-occupation world that saturated at sixty living communities by year four
hundred and never moved again, whose quarters read 0.235, 0.257, 0.257, 0.252.
Because one of the nine worlds tells the opposite story, the exhibit computes
its hint text from the world in front of it instead of copying the site's
sentence.

Second, the modal life is lived in a community whose peak was between
twenty-five and forty-nine — forty-eight to seventy-two percent of person-years,
against a guess of "under twenty-five" the design had been written with. The
hamlet register was right; the number inside it is two or three dozen, not a
handful. Median peak across seeds is twelve to eighteen, and the largest
community any of these nine worlds ever produced held eighty-eight.

Third, five to thirty-four percent of person-years were lived in communities
that had ended by the present, so the community's own recorded fate is
load-bearing on eight seeds and nearly idle on the ninth. That share is of
person-years. Whether a *life* witnesses its community's end is a different
question, and the readout below answers it very differently.

## The mechanism

**One new fact.** The record keeps a community's founding year, its ending year
and its peak population — and no trajectory at all, which is stated in the
source beside the one other field that ever needed one. So the bake now tallies
the integral of each community's live population over its tenure and commits it
as one number per occupation. Between communities the draw weight is then
exact. The tally's *sampling convention* is part of the fact and had to be
fixed before the fact meant anything: credits taken at each site that opened or
grew a community double-counted across a same-epoch handoff, because the epoch
loop grows every living community first and closes some of the same ones
afterwards. One sweep at the end of each epoch, over every community alive at
that moment, replaced six per-site credits, and made the bound exact — a
record's person-years cannot exceed its peak plus a half, times the epochs it
survived. Two hundred and twenty of seed 42's 1,212 occupations carry zero
under that rule; none of them contains a whole year, so no birth could have sat
in one anyway.

**A mortality model, where there was none.** A life's length comes from a Siler
hazard — a falling exponential for infancy, a constant background term, and a
rising Gompertz for senescence — read in *scaled* age, so that a people's
allometric lifespan sets the age axis and one measured input differentiates a
kobold's curve from a bugbear's rather than four authored ones. The background
term is modulated by the site's strife field, the one measured field that
already means how dangerous it is to live somewhere. The era deliberately does
not enter: the site's own tables show life expectancy barely moving across
pre-modern eras, and an era multiplier would have been an authored number with
no measurement behind it. Strife is measured; the era is not; only the measured
thing modulates. The five constants were frozen in the design before any life
was drawn, against a target band taken from the site's own sources, and the
implementation measured a life expectancy at birth of **32.79 years** and a
died-before-scaled-fifteen probability of **0.4298** at the anchor — a
sixty-year-lifespan people at zero strife — inside the preregistered bands of
[28, 36] and [0.35, 0.45]. No constant was moved.

**The community's own fate, as a discrete hazard.** This is where the world is
richer than the site. A life may sit across the year its community ended, and
the record says how it ended, who ended it, and where the survivors went. So at
that year the life meets a discrete hazard: it ends there with the probability
the bake's own loss constants imply, or it continues in the daughter community
the record names, or — where the record names none — it ends with the community
and the story says the record does not know where the people went. The
catastrophe that struck a life is a dated event with a cause and a successor,
not a regional frequency table.

**A shape with the right area, and a clamp that says what it does not know.**
Within one community the lens needs a population curve whose maximum is the
committed peak and whose area is the committed integral, and it fits a
rise-then-plateau. The rise starts at the population the community opened with
— and *that* is an assumption, not a fact. Of the five places the bake opens a
community from another one, four pass the survivors' population and only the
daughter colony passes the small founding constant the design had assumed, and
the record cannot tell the two apart. So the fitted rise overshot the committed
peak on three of seed 42's 1,212 communities, and it overshot for precisely the
communities with the most eventful histories. The resolution is a clamp rather
than a wider tolerance: whenever the fitted apex would exceed the committed
peak, the shape is rendered flat at area over tenure. The committed peak is an
upper bound the reconstruction always honours, and the payload names which of
the three shapes it used.

**The draw is an observation, not a fact.** Its randomness is the reader's. A
lot index is an input on the same footing as a world seed, and every choice in
a life is pure hash arithmetic over the world seed, the index and a label for
the choice — no stream, no seed label, nothing consumed and nothing committed.
The permalink is the seed and the index; a *picked* lot, where the reader
chooses a year or a place, pins two of the choices and lets the index drive the
rest. The consequence is honest and worth stating: the same permalink draws the
same life only until the record it reads changes.

**Twenty-six questions, four of them silent on purpose.** The narrator asks
every slot of every life. A slot resolves to committed facts, or to a derived
read over committed facts, or to a silence, which is rendered as a plain
sentence in the prose and as a null with a reason in the payload. It is never
filled. Four slots — sex, marriage and children, work and income, literacy and
height — are declared silent *by design*, because no domain carries a model
behind them, and the fill rate is reported over the other twenty-two so the
number measures the world rather than the lens's own restraint. Every sentence
carries the facts it read, numbered, with the list at the end of the story.

Three of the slot sources named in the design did not exist as written, and
they were caught before any code was written by grepping the readers rather
than trusting the field names. Two of them are fields the bake writes as
absent and nothing ever commits; the third names phenomena rather than facts.
All three were re-sourced. A slot whose source does not exist reports a silence
that is real for the wrong reason, and nothing in the output distinguishes it
from one that is real.

## The readout

Six predictions were frozen in the design before the mechanism existed, and run
once over the nine seeds at two hundred lots each after it was complete and
before any constant was touched.

```
                                   s1     s2     s3     s7    s13    s42   s100   s256   s777
  born-last-quarter share       0.470  0.515  0.540  0.460  0.420  0.500  0.255  0.590  0.500
  median scaled age at death    12.01  14.51   8.95  15.79   9.43   9.77  14.32  10.97  10.21
  witness-community-end share   0.035  0.075  0.015  0.015  0.030  0.020  0.015  0.045  0.025
  silent-subsistence share      0.880  0.920  0.930  0.885  0.810  0.890  0.760  0.920  0.945
  mean filled slots (of 22)     17.91  18.02  18.11  17.82  17.79  18.02  17.45  18.18  18.07
  souls-ever / (person-yrs/30)  0.654  0.632  0.891  0.766  0.733  0.694  0.492  0.757  0.886
```

Four passed. A birth is skewed to the last quarter of the span on every growing
seed, 0.42 to 0.59, and flat on seed 100 at 0.255 — the site's sentence holds
here, for the same reason, and the exhibit says so per world. Subsistence and
standing are silent for 76 to 95% of lots. The mean fill rate is 17.5 to 18.2
slots of twenty-two. And the world's total lives integrate to within a factor
of two of what the stationary birth rate says they should.

**Two were falsified, and they are the findings worth carrying.**

The median scaled age at death is 8.95 to 15.79, and on three seeds it is under
ten, against a predicted band of ten to thirty-five. **A representative life in
these worlds is a child's, more often than the design expected.** That is not a
defect in the model; it is what the model says once the infant term is
calibrated to the target band's own died-before-fifteen probability of about
0.43 and the strife term is added on top. The site's own thesis is that most
lives ever lived were poor and brief; these worlds are harsher than the
prediction written from that thesis.

The share of lots that witness their own community's ending is 0.015 to 0.075,
below the predicted three-percent floor on five of the eight growing seeds. The
prediction was derived from the person-years figure above — on the growing
seeds, twelve to thirty-four percent of person-years sit in communities that
ended — and the
derivation counted **person-years rather than lives**. A life of ten to thirty
years, laid against tenures of fifty to two hundred, rarely overlaps the single
year the community ends. The community-fate hazard is real, dramatic, and rare,
and the two statistics measure different things.

No constant was moved after unblinding.

## What a lot cannot yet say

Read against one drawn life on the site — a blacksmith born on the Ganges Plain
in 813, dead of a stroke at sixty-two — the comparison is not close in either
direction. That life carries some forty attributes and twenty-nine sources
drawn from fifty-four tables keyed on region, era and sex. Every attribute is a
table roll, and the tables are real. This world's lot is the inverse shape: few
tables, a running world, and a life read off what the world actually did.

```
  facet of that life          this world today                    status
  --------------------------  ----------------------------------  ---------------------
  birth year / place, by pop  per-community, 15 peoples, 2000 y   HAVE, richer
  a hamlet of about seventy   committed peak (modal 25-49)        HAVE
  dwelling                    derived structures: hut, longhouse  HAVE, coarse
  language                    a real phonology and family tree    HAVE, richer
  name                        a namer in that tongue              HAVE
  religion                    deity, epithet, cult form, tenet    HAVE, richer
  climate epoch               orbital forcing and seasonality     HAVE, richer
  migration                   a dated event with a destination    HAVE, richer
  conflict                    raids, named by their attacker      HAVE (community-level)
  famine                      an ending with a cause              HAVE (community-level)
  eruptions                   hazard events over the life's span  HAVE
  eclipses                    computed at the site's longitude    HAVE (a different sky)
  travel radius               the connection graph                HAVE, richer
  social standing             a caste ladder, living places only  HAVE, thin
  subsistence                 a culture fact, living places only  HAVE, thin
  diet                        the species' resource niche         HAVE, very thin
  --------------------------  ----------------------------------  ---------------------
  disease                     no producer for the plague ending   ABSENT
  cause of death              nothing per individual              ABSENT (the Siler
                                                                    hazard is the first)
  sex                         no species carries a model          ABSENT
  marriage, children,         no household model for anyone but   ABSENT
  siblings, parents' deaths     founders
  height                      species mass, no per-body spread    ABSENT (cheap)
  literacy                    reading and writing are inert       ABSENT
  income, wage, poverty       no economy                          ABSENT
  a trade                     every community farms               ABSENT
  funerary rite               one terrain-feature mention         ABSENT (derivable)
  comets, guest stars         none                                ABSENT
```

**Disease is the largest single hole, and it is the one the site leans on
most.** For that blacksmith's place and era the site's cause table reads
smallpox 15%, dysentery 12%, tuberculosis 12%, degenerative 9%, birth
complications 8%, injury 8%, other 36%. This world has a plague ending in its
vocabulary of how a community dies, and a laboratory metric that counts it, and
nothing anywhere ever assigns it — only flight, migration, breach and famine
are ever produced, and the nine-seed probe finds no plague share on any seed.
The shape the absence wants is a pathogen modelled as a *species* whose niche
is another species, spreading over the connection graph and the household
lattice, and there is a derivable finding waiting inside it: crowd diseases
need contiguous populations in the hundreds of thousands, and these communities
peak under ninety. **A world of this world's scale would be a
dysentery-and-tuberculosis world, and could never be a smallpox one.** The
Siler hazard yields an age at death and a category — infant, background,
senescent, or the community's own fate — and never a named disease, and the
slot table leaves the cause of death as a category on purpose so that a disease
domain can later fill it without moving the lens.

## The exhibit, and what it costs

The canonical surface is the text. The lot window's prose and its payload are
what the simulation guarantees byte for byte; the committed gallery page of ten
lives from seed 42 is text; the browser exhibit consumes the same payload
through a wasm boundary and is free to be theatre. The four-stage reveal, the
log-linear axis switch, the crosshair hopping between populated sites, the
timeline assembling event by event, and the pick-your-own year and place are
all the client's; every number it shows is a field of a payload it was handed.
Its gate is a byte-identity smoke test comparing the first lot of seed 42
across the boundary against the same lot from the command line.

One thing the exhibit deliberately does not show is the *odds* the life was
drawn against. The window computes and can print them — life expectancy, the
died-before-maturity probability, the hazard's three parts, the site's strife —
but there is no export for them across the boundary, so the Life stage shows a
life course rather than a mortality profile. The payload the exhibit reads
carries the drawn events; the distribution behind them is a command-line
surface with no browser consumer yet.

The exhibit was first built into the shared world catalog, and that was wrong
for a measurable reason. The catalog carries a size gate that exists for a
released download, and the lot took it from 414,739 to 490,067 bytes gzipped
on the authoring machine — 6.4% under a 524,288-byte gate — while the
canonical machine's older
compressor emits some 11 to 13% larger output, which projects the same artifact
over the gate on the one machine that enforces it. The gate's own comment
forbids raising it to buy room. So the lot took its own crate, on the precedent
of the possession exhibit, which has had exactly this posture since the book's
deploy was retired: **459,474 bytes gzipped, ungated, never committed, and dark
until someone builds it locally.** The catalog reverted to 414,531 bytes
gzipped against the 414,739 it carries on the main line — the same weight it had
before the campaign, and its full headroom restored for whatever needs it next.

Six columns joined the census — the world's total lives ever, the median scaled
age at death, the witness share, the silent-subsistence share, the mean filled
slots, and the last-quarter birth share — so that the nine-seed readout above
becomes a thousand-seed one at the next refresh. Until that refresh lands, the
committed census fixture reads as predating those six columns, which is the
additive case and is green by design.

**What those six columns cost, measured rather than estimated.** On the
authoring machine, with this branch's release binary, generating a world takes
2.42 s, drawing one life from it takes 0.92 s — of which about 0.90 s is
assembling the reading context — and drawing two hundred takes 4.18 s. So the
six columns cost roughly four seconds of processor time per census world, or
about four thousand processor-seconds over a thousand-seed census, against the
last recorded run's twenty-seven thousand. That is about fifteen percent, and
the next refresh will confirm or correct it in the timings ledger, which is the
only place a cost of this kind should ever be read from. About 0.9 s of the
four re-derives terrain, climate, the demography report and the sky — all of
which the measurement instrument's own view already holds and does not hand
over, which makes roughly a fifth of that cost recoverable by a seam that does
not exist yet.

**The refresh confirmed it, and the confirmation tripped a gate.** The
thousand-seed census that followed cost 3,661 more processor-seconds on its
main study than the run before it — the estimate above was within nine
percent — and 136 more seconds of wall, 1,142 to 1,278, on a box no busier
than before. That crossed the census's refusal ceiling of 1,200 seconds, a
tripwire written for a single run that jumps by a third, not for a twelve
percent step with a named cause. The ceiling and its alarm threshold were
re-set together, to 1,650 and 1,320 seconds, following the method the last
raise had recorded, and the refresh was run again. The ledger names what is
recoverable: about a fifth of the six columns' cost, roughly two percent of
the census, not a way back under the old number.

## What a life reads like

Seed 42, lot 0. Born in year 1752 at Raaxora, a temperate-forest site at
sixteen degrees of latitude, dead in 1783 at thirty-two. A kobold, called Xaararo,
in a community of about forty people, founded in year 950 by settlers from a
place two centuries older, whose founder descended seven generations from the
mother community's founder, and which was still standing when the life ended.
About two hundred and twenty-seven thousand lives have been lived in that world
between year zero and year two thousand, and 48.8% of them were born in its
last five hundred years. Every clause above cites a fact. None of that person
existed; every number about them did.
