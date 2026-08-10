# The Armature

An armature is the wire skeleton a sculptor builds a figure on. It is invisible
in the finished work, and you learn whether it was there when the clay sags.
Hornvale's causal armature is the set of claims of the form *this quantity
should move when that one does* — the star's period reaching the mean
temperature, the mountains reaching the settlements, the habitable ground
reaching the population. None of it had ever been written down. The world was
sculpted over a frame nobody had drawn.

The Domesday, the campaign before this one, built a survey that reports what
does not vary. It cannot report **what fails to cause what**, because a
non-relationship is only visible against a declared relationship, and the
project held exactly one declared relationship: a single row in
`studies/expectations.json`. This campaign wrote twenty-nine more, froze them
before looking, and measured them once.

## The rule that made the result mean anything

Every expectation was written from physics against metric *names and doc
strings only*. No correlation was computed while authoring — not by the
controller, not by the implementer who transcribed the table, not by the
reviewer who checked the transcription. The thirty rows were frozen in the
spec's own commit, before a line of implementation existed, and the commit that
first reported a result is seven commits later along the campaign's own
history.

This is not fastidiousness for its own sake. A frame authored with the data in
view is a *description*, and no amount of disclosure converts a description
back into a prediction. The Domesday had already paid for that lesson: its
strength detector was first specified as a minimum correlation of 0.50, chosen
with the measured value of 0.245 already known, and the number had to be thrown
out and replaced by a declared *class* mapped to a conventional effect-size
band — a comparison neither side can be tuned to produce.

One honest qualification, because the campaign's own subject is when values
become visible: the first measured value appeared two tasks before the
measurement task. Renaming the detector's finding strings turned the
live-acceptance test red by design, and its failure message printed row one's
actual result. Nothing was compromised — the row had been frozen in the spec
commit and the plan forbade revising it — but a campaign about phase order
should say when its own phases leaked.

## The detector was blind to sign

The mis-declared-strength detector computed a Pearson `r`, took its absolute
value, mapped that to a band, and compared the band against the declared class.
It never looked at the sign. A quantity coupled *backwards* at exactly the
declared strength passed silently — a wire on the wrong terminal, reported as a
wire correctly attached.

So an expectation gained a `direction` of `positive`, `negative`, or `none`,
with a load-time rule that a declared `none` requires a direction of `none` (a
claim of no relationship has no sign), and the detector gained a distinct,
more serious finding for a link that runs the wrong way. The sign is suppressed
when the observed band is `none`: the sign of a near-zero `r` is noise, and
reporting it would manufacture findings.

## What the census said

Of the thirty rows, five stayed silent, nineteen fired as a strength mismatch,
six fired as unmeasurable, and **none** fired as a direction mismatch.

Both rows that declared *no* relationship were silent — mean land temperature
against day length at `r = +0.018` over 952 paired worlds, and total population
against the plate-size Gini at `r = +0.020`. The frame invented no coupling
that the implementation does not have, which is the one direction in which
these two rows can fail and the reason a frame whose author is also its subject
needs them.

### Zero backwards links is zero out of three

The direction finding requires the observed band to *match* the declared band
and to be something other than `none`. Only three rows in thirty ever reached a
sign comparison: karst fraction against mean land temperature (`+0.111`,
declared weak positive), population-weighted absolute latitude against mean
land temperature (`+0.394`, declared moderate positive), and climate
displacement events against habitable fraction (`−0.334`, declared moderate
negative). All three matched. **"Zero backwards links" is zero of three, not
zero of thirty**, and publishing the count without that denominator would have
been the project's documented top failure mode — the right measurement under
the wrong attribution.

It would also have been false in substance, because three *other* rows do run
backwards and said nothing about it. Shelf fraction against ocean fraction was
declared a moderate **positive** link and measures **`r = −0.755`** — the
second-tightest coupling in the whole frame, and it runs the wrong way.
Habitable fraction against mountain coverage, declared moderate negative,
measures `+0.151`; shoreline development against largest-continent share,
declared moderate negative, measures `+0.130`. Each of these has the wrong band
*and* the wrong sign, so each routed to the strength branch — whose detail text
omitted the sign entirely, while a comment three lines above it stated that the
sign is withheld only "when nothing was measured". The code withheld it
unconditionally. A measured sign is now always reported, and named as backwards
when it contradicts the declaration.

### Six rows the census cannot test

The six biology rows all fired, exactly as the spec predicted — and the
prediction is not thereby confirmed, because they fired for a reason that is
not evidence about biology at all. Each of those six metrics holds **one
distinct value across all thousand worlds**. Life history here is a pure
allometric function of authored mass, metabolic class and schedule, so the
input to the correlation is a constant column and no correlation exists to
compute. The claim *a colder world costs more to thermoregulate in* was not
refuted; it was **untestable on this census**.

That distinction is now a finding of its own. A declared link whose inputs
cannot support a correlation reports `D5 unmeasurable`, naming which side is
frozen and at what value, rather than falling silent — because silence in this
instrument means *the claim held*, and an untestable claim is the one thing
silence must not be allowed to mean.

### The severed-wire candidates

The honest headline is in the strength column. Ten of the nineteen strength
findings measure `|r| < 0.1`; **seven of those ten were declared moderate**.
They cluster in three places.

*Climate does not read the land.* Mean land temperature against ocean fraction
is `−0.0005` — not weak, effectively nil, against a declared albedo-and-
thermal-inertia coupling. Against mountain coverage it is `−0.041`: no lapse
rate reaches the mean. Habitable fraction against obliquity is `+0.058`: the
tilt does not reach habitability.

*Settlement reads one scalar and nothing else.* Settlement count tracks
habitable fraction at `+0.230` — weak where strong was declared, but present.
Against mountain coverage it is `+0.015` and against ocean fraction `−0.085`.
Terrain reaches settlement only through a single habitability number, and not
at all through the shape of the ground.

*Hydrological form does not read terrain form.* Waterfall count against
mountain coverage is `+0.083` — knickpoints without gradient. Shoreline
development against continent count is `+0.004`.

These readings need a positive control, and the frame supplies one. Standing
tribute relations track settlement count at `+0.954`; fertile land fraction
tracks mean land temperature at `+0.730` (declared moderate — the author was
too *modest* there); shelf fraction tracks ocean fraction at `|r| = 0.755`. The
instrument sees strong couplings where they exist, so a reading of `0.000` is a
statement about the simulation and not an artifact of the method.

## The verdict on the falsification clause

The spec bound the campaign in advance: *if the frame fires on nearly every row,
the likely cause is that the author's physics is wrong rather than the world's —
report that as the headline and publish the frame with its failures rather than
quietly pruning rows.* Twenty-five of thirty fired. The frame is published
whole; not one row was deleted, edited, or reclassified after unblinding.

The clause is triggered, and its diagnosis is **half right**, which is more
interesting than either pole it anticipated. The failures do not form one
population. Six are about the census's reach rather than anyone's physics.
Four are strength over-claims with the declared sign intact — orbital period
against temperature at `−0.245` where dominant was declared, and three
population-and-settlement rows declared strong that measure between `+0.226`
and `+0.307`. Those are the author being wrong about *how strongly this
simulation should express a real effect*, which the spec named in advance as
the frame's least defensible part, and they are calibration error rather than
discovery. Three are sign errors the author owns: shelf fraction against ocean
fraction is almost certainly a definitional mistake about what a shelf fraction
is a fraction *of*, and the other two are weak enough to sit near noise. Two
are under-claims — the same calibration error with its sign reversed. Fertile
land fraction against temperature at `+0.730` and standing tribute relations
against settlement count at `+0.954` were both declared merely moderate; the
author was too modest, and these are the rows that serve above as the frame's
positive controls. A frame that only ever over-claimed would be a frame tuned
to flatter itself, so the two rows pointing the other way are worth counting
rather than filing under good news.

The remaining ten are structurally different, and the clause's arithmetic does
not explain them. An over-claim gets the sign right and the magnitude wrong. A
severed wire produces no signal at all — and **seven declared moderate**
couplings measuring below `0.1`, in three coherent clusters that each name a
subsystem boundary, is a statement about the simulation. The other three of the
ten were declared only *weak*, so their fall to `< 0.1` is a short one; they are
counted here for completeness and not offered as evidence of anything severed.
Six, four, three, two and ten: the twenty-five firings partition without
remainder. *(That partition is the author's judgement, not a measurement; the
measurement is the table.)* Two
of those clusters were already suspected from the other side: the survey before
this one found that the census never contained an insolation column, so the
standing conclusion that climate is uninfluenced by its astronomy rested on one
orbital-period proxy. The frame did not resolve that. It did establish that
climate is uninfluenced by its *terrain* as well, measured directly, on the
drivers the census does hold.

One further piece of evidence deserves its weight because it predates the
result. The implementer who transcribed the frame flagged five rows as
physically questionable before any measurement — a two-hop inference from
stellar brightening to temperature, two rows treating a U-shaped
thermoregulation cost as monotone, and two assuming a world-level habitable
fraction reaches an individual's reproductive tempo with no density term — and
transcribed all five unchanged, which is the correct behaviour. Four of the
five turned out to be biology rows the census cannot test either way. The
fifth, brightening against temperature, fired at `+0.049`. Recorded doubt
that survives the measurement is worth more than any amount of it afterwards.

## What the campaign actually caught

It went looking for severed links in the world and found four could-not-fire
defects in the instrument: two in the measurement path, one caught at plan time,
and one committed by the repair of that third.

The correlation function documented that it returns nothing when either column
is constant, and guarded on the variance being at or below zero. The guard was
**defeated by arithmetic**: `sum()` accumulates left to right, so a thousand
copies of one value leave the mean about `1e-13` off the constant and the
variance at roughly `3.4e-22` of pure rounding residue — strictly greater than
zero. The function returned a correlation computed entirely from summation
noise, in violation of its own contract, on precisely the six rows the campaign
was built to interrogate. The fix tests constancy exactly and keeps the
variance guard as a backstop rather than a replacement.

The second was the sign withheld against its own comment, described above.

A third had been caught at plan time: the survey renderer carried a frozen
roster of detector names, `D1` through `D8`, so a new detector would have been
computed and then dropped before rendering.

A fourth arrived inside the third one's repair, and is the cleanest miniature of
the whole principle. Deriving the roster from the findings fixed the stale
literal and introduced its mirror: a detector that fires nothing has no finding
to derive a name from, so it lost its row altogether. Absence in that table then
meant either *no such detector* or *this detector ran and every claim it checks
held* — the two meanings decision 0114, this campaign's own, forbids sharing a
channel.
`D7 | 0` disappeared from the published index, and `D5 direction | 0`, the
headline null, had never appeared in it at all. The renderer now publishes the
union of a declared roster and the observed names, the one arrangement that can
show a zero without ever hiding a count: a roster gone stale can drop a zero
row, never a finding.

The Domesday found four defects of this family inside this same module. That is
now eight, in the one part of the codebase whose entire purpose is finding
weaknesses, found by two consecutive campaigns each of which knew about the
family before it started. Knowing the failure mode confers no immunity to it,
and an instrument built to see what does not fire is not thereby able to see
itself.

## What is left standing

The frame is data and it is frozen; the detector is code and it changed twice.
That separation is what let a defective instrument be repaired mid-campaign
without any suspicion of tuning — `studies/expectations.json` is byte-identical
to the commit that froze it, and the tally after both repairs (five silent,
nineteen strength, zero direction, six unmeasurable) is the tally the frame
earns.

What the frame cannot yet say is whether a near-zero reading means the wire is
absent or merely thin, and it takes no position on which of the seven
candidates is worth a campaign. That is deliberate: repairing a link found by
this instrument needs its own preregistration, and a repair designed while
reading the number it is meant to move is the phase-order violation this
campaign exists to avoid.
