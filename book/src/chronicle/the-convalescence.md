# The Convalescence

The Temperament gave the world's minds an instrument. Simulate a population's
affect forward forty days, read each creature's felt state at every tick, reduce
the whole to a family of numbers — how many are in distress now, how many are
persistently stuck, how long a spike takes to break, decomposed by cause and by
species — and you have the epidemiology of a world's minds. Its value was never a
number that varies. It was a number that stays quiet: a regression alarm, armed by
reading zero on every healthy world, that would fire the moment the cognition
layer broke in a way that left a mind stuck.

An alarm is only ever as good as the quantity it reads, and the arresting thing
about this one is that it read precisely the quantity its design had named. The
Temperament's design introduced the family member by member, and against
chronicity — the fraction of creatures persistently stuck — it wrote, in as many
words, **the bug alarm**. The control bounded chronicity at zero. Held against
that line, the instrument was faithful.

Held against the next paragraph, it was not, because the design also said what
the alarm was *for*, and there it described something else: "a spike that
*recovers* (short half-life) is a novel/extreme world event (a frost, a drought)
the creatures adapt to — legitimate; a spike that *persists* (no recovery,
elevated chronicity) is a bug — an unsatisfiable need or an
unreachable-but-should-be-reachable resource." That is a **conjunction**, and
recovery is its discriminator: the single quantity separating a hard world from a
broken sim. The evidence battery, several pages on, states the signal the same
way — persistence *and* no recovery. So one document names one quantity the alarm
in its list and argues for a different one in its prose, and it supplies a reason
only for the second.

What had drifted, then, was not the instrument from its definition. It was the
definition from itself: a one-line label against the paragraph meant to explain it
and the battery meant to test it. That is a heavier finding than a stale
assertion, because it cannot be settled by looking anything up. A campaign that
finds a spec disagreeing with itself has to decide which half governs. This one
chose the reasoning over the label, on the grounds that the reasoning says *why*
— recovery is what tells a hard world from a broken sim — and the label says only
what. Chronicity reads the first conjunct alone, *long*, so a bound on it fires
on episodes that recovered, which the same sentence calls legitimate. The control
asserted half of a conjunction. It bounded *length* and never asked about *fate*.

The gap is not academic, because a creature carries six drives and each has its
own rhythm. Thirst climbs over five or six days and resets on a drink; thermal
discomfort comes and goes with the cell it stands in; fatigue runs on the diurnal
cycle. Two of those rhythms, phase-shifted by a single tick, can *weld*: a
four-tick block of helplessness, one frustrated tick where the second need takes
over, another four-tick block — nine consecutive ticks of distress, over any
threshold worth setting, in a creature that then walks back into contentment and
stays there. Nothing was broken. The world was merely varied, which is the thing
the project has spent several campaigns making it. The alarm fired anyway.

The obvious repair is to conjoin the two numbers the metric already published:
demand a long run *and* no recovery. It is worse than the drift it fixes, and the
reason is a scope mismatch rather than an arithmetic one. Chronicity is counted
**per creature**; the recovery half-life is a **population mean**. Put one
genuinely stuck creature among nine who recover, all ten carrying a long run, and
the population reads chronicity at its ceiling while the recovery half-life comes
back as a number — `Some(9.0)`. Nine ticks is not a healthy figure beside the two
to four the measured worlds read; it is elevated. But the conjunction never looks
at the figure. It asks only whether *any* recovery happened at all — `Some`
against `None` — and a number, however poor, answers yes. So a conjunction taken
at population scope pronounces that population well, and the one creature that
never recovers disappears behind the nine that did. That trades a false alarm for
a *silent* one. A bug alarm may be noisy; it may not be silent, and this is the
direction in which an instrument fails without anyone noticing. So the conjunction
has to be evaluated inside a single creature's trace, where both halves describe
the same mind.

The pull toward the population-scope version is not hypothetical. The earlier
campaign that demoted prevalence from bound to diagnostic had already written the
conjunction down, in the comment standing directly above the assertion that later
failed — and written it at population scope, chronicity above zero together with a
recovery half-life of `None`. It had both halves right and the frame wrong. Which
is the reason that comment could not simply be promoted into the check it
described: transcribed as written, it would have installed the masking case as the
alarm.

What resolves it is a two-by-two, and the useful part is that its four cells are
not equally meaningful. A distress episode has a length and a fate:

|                | recovered                      | still open at the end |
| -------------- | ------------------------------ | --------------------- |
| **short**      | a blip — life in a varied world | *censored* — silent   |
| **long**       | a hard patch — legitimate       | **the bug signal**    |

Short and recovered is a momentary block, a warm afternoon; long and recovered is
the frost the creatures adapt to, which the design calls legitimate in as many
words; long and still open is the unsatisfiable need, or the
unreachable-but-should-be-reachable resource, that the metric exists to catch. The
fourth cell is the
interesting one. A short run still open when the trace ends is **right-censored**
— it might have recovered one tick after the fortieth, and the trace cannot say
which. So it does not alarm. That asymmetry is deliberate: in the open column the
discriminator is the run's *length*, not its openness, and the campaign left a
test standing guard over the cell precisely so a later reader does not tidy it
away.

The alarm is therefore that one cell — a distress run past the chronic threshold
that never ended, counted per creature — and chronicity becomes what it always
described: a diagnostic, still computed, still reported, no longer the thing that
fires.

The measured consequence is the part that matters most, because it is what makes
this a sharpening rather than a loosening. Across the five-seed sweep the control
holds on, the two quantities are *indistinguishable*:

```text
seed  0: stuck 0.0000 chronicity 0.0000 prevalence 0.2600 recovery Some(4.0)
seed  1: stuck 0.0000 chronicity 0.0000 prevalence 0.3125 recovery Some(4.04)
seed  2: stuck 0.0000 chronicity 0.0000 prevalence 0.1050 recovery Some(3.4)
seed  7: stuck 0.0000 chronicity 0.0000 prevalence 0.1800 recovery Some(2.56)
seed 42: stuck 0.0000 chronicity 0.0000 prevalence 0.1400 recovery Some(3.8333333333333335)
```

Chronicity reads zero on every measured world, exactly as the new alarm does. The
old bound and the new one evaluate identically everywhere the project can
currently look, which means no observable slack was created and nothing was
relaxed to let anything through; what changed is which of two coincident readings
the control is *entitled to* when they eventually part company. No real seed has
yet produced the long-but-recovered pattern at all — the welded nine-tick run
above is a planted trace, not a sighting.

That places the burden of proving the alarm can still fire exactly where it
belongs: not on the healthy worlds, which prove only silence, but on the metric's
own synthetic harness, where scenarios drive the real drive loop into genuine
distress and the metric scores its own output. A creature stranded past its plan
budget from the only water it believes in gives up and never recovers: the alarm
reads one. A creature gripped by a blistering thermal cell that later breaks
recovers, and it reads zero. Between them they pin both directions, on the sim's
behaviour rather than on hand-typed affect. An alarm armed by staying quiet needs
somewhere it is known to be loud, and now it has one, named.

Moving a bound off a preregistered control is also the exact shape of an
illegitimate repair, and the shape does not change just because the reasoning is
honest — so the reasoning is written down where someone auditing the project for
that move would look: decision
[0080](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0080-chronicity-is-a-diagnostic-the-alarm-is-stuck.md)
records the semantics above, the discipline the demotion was held to, and the
generalization that a bound demotion occasioned by blocked work owes an entry
where one occasioned by ordinary drift does not.

The instrument scores the world; nothing but the instrument changed here. Every
world is byte-for-byte what it was — no new draw, no new predicate, no epoch — and
the population health family gained one member. But a self-scoring metric is a
claim the project makes about itself, and a claim that fires on the legitimate
case is not a weaker claim than one that doesn't. It is a different claim. This
campaign made the metric say the thing its design had said all along.
