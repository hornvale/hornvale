# The Tolerance

A review of the campaign that authored humans noticed something small: humans
were given a threat response of 0.5, on the argued ground that people genuinely
both flee and stand, and the deep-history engine gates raiding at 0.6. So no
human raids, in any world, ever.

The number was defensible. The sentence was not. *"Humans do not raid"* is only
a **sayable** sentence because every human in every world is the same human —
one row in a registry serving at once as the description of a type and as its
sole exemplar. Economics named this figure a long time ago and its standing
critique is *predictive*: it tells you in advance which phenomena a one-agent
model cannot produce, rather than letting you discover them one accident at a
time. Deviance, the exceptional individual, sorting — the bold to the frontier
and the cautious to the interior — lineage drift, and above all **selection**,
which needs tails to act on. A point has no tails.

A manikin is the reference figure a tailor fits cloth against. A *tolerance* is
the spread the tailor is permitted around it. This project had the first and not
the second.

## A location and a spread

The change is one number per vector. Every psychological scalar a people carries
keeps its authored value, which now explicitly means the **mean**, and gains a
dispersion beside it: the standard deviation of a population around that mean.
The frame is stated rather than implied, because a datum whose frame is implicit
drifts in meaning as the model grows — that was the defect the previous campaign
removed one level up, and repeating it one level down would have been careless.
Zero reproduces the old model exactly, which is what makes the layer falsifiable
at all.

**The distribution is a uniform on ±√3σ, clamped to the axis.** The √3 is not
decoration: a uniform of half-width `a` has variance `a²/3`, so a half-width of
`√3σ` makes the standard deviation exactly `σ` and the authored number mean what
it says. Naming the family matters more than it looks, because it qualifies the
claim this chapter opened with. **Selection needs tails, and a uniform has none
either** — it has hard bounds, and beyond them the probability is exactly zero
rather than merely small. What a uniform supplies is *spread*: enough range for
a threshold to sort one people into two groups, which is the whole presenting
problem. What it does not supply is *rare extremes*, and that is the same
constraint this project's own confidence map already records against a different
bet — rare tails need asymptotes, not clamps, and every saturating bound in this
world is presently a clamp. A campaign that needs the exceptional individual
rather than merely the varied one will have to change the family, not the
parameter.

Nine kinds carry a row. The three dragons are narrowest (0.08 around the mind),
then hobgoblin (0.10), kobold (0.12), bugbear (0.20), gnoll (0.22), goblin
(0.25), and human widest at 0.35. That ordering is the campaign's one real
theoretical claim about the roster: **variability is itself a species trait**,
and what makes a generalist a generalist is psychological breadth rather than
ecological breadth. A eusocial insect has near-zero behavioural variance. A
species that lives everywhere does not.

The draw is one per **settlement**, not one per individual. That is where the
consumer that exposed the bug actually sits: raiding is a decision a community
makes, not a decision a person makes, and it is also the level selection acts
on. The individual tier is deferred, and its deferral is recorded rather than
implied.

## The key, which is where this campaign could have gone silently wrong

A per-settlement draw needs something to key on, and the obvious choice is
catastrophic. Entity identifiers are minted sequentially, so keying a draw on
one means that inserting a single earlier entity anywhere in the world silently
reshuffles the psychology of every community in it — deterministic,
reproducible, and wrong in a way no test would name. The rule that an identifier
may be stored, compared and looked up but **never read for its value** was
ratified by an earlier campaign, and this is the first time it prevented
something rather than described something.

The key that was used instead is the occupation record's `(site, founded year)`
pair: where a community was founded and when. Both are committed facts, both are
immutable once written, and neither moves when an unrelated entity is inserted
earlier. The independence is proved rather than asserted — a mutation that swaps
the key for an identifier reddens the test with materially different vectors.

The pair is not unique over a world's whole history: two communities can occupy
the same cell in the same year at different points in the deep past, and about
**8.9 %** of occupation records collide with another on the key. Among
communities *alive at the end of the bake* the collision rate is **zero**, and
that is structural rather than lucky: the engine's live index is keyed by cell,
so an opening onto an occupied cell is always immediately preceded by that
occupant's closing. Two living settlements cannot share a mind.

## What a bounded axis does to a symmetric spread

Every one of these scalars lives in `[0, 1]`, and a symmetric spread around a
mean near a boundary does not fit inside it. The draw clamps, and clamping is
not neutral: it moves the realized mean away from the boundary and compresses
the realized variance.

The instinct is that a people authored at the middle is safe and a people
authored at an extreme is not. That instinct is wrong, and the campaign's own
disclosure had to be corrected mid-flight to say so. What protects a people is
not being **centred**; it is being **symmetric with respect to the bounds**.
Human sits at 0.5 with a half-width that spans the entire axis, so it clamps
8.8 % of its draws at each end — and the two displacements cancel to a net shift
of exactly zero. Gnoll at 0.85 clamps on one side only, loses 0.030 of its mean
and **36 %** of its variance; bugbear at 0.80 loses 0.021 and 18 %.

The correction mattered beyond bookkeeping, because it let the campaign's third
prediction be argued **structurally** instead of arithmetically: clamping is
monotone and idempotent on the unit interval, so for any threshold strictly
inside it, a draw exceeds the threshold exactly when its clamped value does. The
gate cannot be moved by the clamp at all. That is a proof rather than a table,
and it is true of thresholds this campaign never measured.

## The readout

Three hypotheses were frozen in the specification before any code existed. A
fourth was withdrawn at specification review, also before any code existed: it
predicted that raiding would track the *interannual variance* of local resource
supply rather than its level, and it cannot run, because the supply field takes
no time parameter and the one available daily trajectory is a periodic year by
construction — seasonal amplitude, not unpredictability. Preregistering it
anyway would have been exactly the trap this program exists to catch.

The remaining three were measured over 19,996 occupation records across thirty
seeds, of which 6,041 were alive at the end. Six of the thirty worlds ended with
no settlement alive at all, which is ordinary for this engine and was expected.

**The first — "the mean survives" — was mis-specified rather than
disconfirmed.** The mean of the *drawn disposition* does survive: the only
material shifts are gnoll's −0.030 and bugbear's −0.021, both predicted exactly
by the clamp table, and human moves +0.007 with its two bounds cancelling. But
the mean of the *gated outcome* cannot survive, and not because the draw is
biased. **Thresholding a distribution is not thresholding its mean.** A people
authored below the gate used to clear it never; it now clears it on some
fraction of its settlements, and a people authored above used to clear it always
and now does not. Human went 0 → 0.423, goblin 0 → 0.360, gnoll 1 → 0.833,
bugbear 1 → 0.771, hobgoblin 1 → 0.785, kobold 1 → 0.983. That is a structural
consequence of the design and is in fact the third hypothesis arriving early; it
is reported as a mis-specification because it is one, not retuned away.

**The second — "the variance appears where authored" — came out in three parts,
and the third part is the most interesting thing the campaign found.** The
specification named its own falsification condition as *nothing moves*, and that
is decisively not met: between-settlement variance in the drawn disposition went
from **exactly zero** for all six settling peoples to a range of 0.010 to 0.113,
ordered by authored dispersion. The ordering half is testable on exactly one of
the four measured columns and passes there, with one discordant pair out of
fifteen — and that pair *is* the clamp, made checkable by its own numbers:
unclamped, gnoll's spread is **0.0484** against bugbear's **0.0400**, in the
authored order; clamping then costs gnoll **36 %** of its variance against
bugbear's **18 %**, which is exactly enough to invert them. The discordance is
the clamp table's arithmetic, not a defect in the draw.

The other three columns cannot test it at all, and the reason is arithmetic
rather than experimental. Gate-open, raid-initiated and raid-victim are all
**binary** per settlement, so the variance of each is exactly `p(1−p)` — pinned
to its own mean, and monotone in `p` because every rate here is below one half.
Ordering those columns by variance is ordering them by *rate*, which is a
different claim wearing the same statistic's clothes. This was measured rather
than argued: the largest departure from `p(1−p)` across the three columns is
3.9 × 10⁻¹⁵, 1.7 × 10⁻¹⁴ and 9.2 × 10⁻¹⁵ respectively. **Only the
drawn-disposition column can test the hypothesis, and only it was allowed to.**

**The third — "raiding becomes a fraction, not a flag" — is confirmed on the
gate.** Goblin 0.360 < human 0.423 < hobgoblin 0.785, strictly inside the open
interval, exactly as predicted. It inverts marginally on the *live* initiator
rate (human 0.1128 against goblin 0.1183) because an actual raid is a
conjunction: a community must clear the gate **and** be able to win the fight it
is contemplating.

And the count that carries the whole campaign: across those thirty seeds,
**humans initiated 262 raids and goblins 232**, out of 4,978 raids in total. Both
peoples are authored at 0.5, below a gate of 0.6. Before this campaign that
number was not small — it was structurally impossible. "Humans do not raid" is
no longer a sentence this world can be made to say.

## The world got quieter, not smaller

The most alarming figure the campaign produced was seed 42's deep history
halving: 919 occupation records down to 459. A world losing half its history to
a psychology change deserves suspicion.

The decomposition dissolves it. Live settlements went 232 → 188, down 19 %.
Ruins went 687 → 271, down 61 %. The living world barely moved; the **dead
layers** collapsed. That is precisely the signature of shortened cascades.
Raiding is an avalanche amplifier — each displaced remnant re-enters the same
rule, and a bold remnant carries the chain onward where a timid one settles
vacant ground. Turning four peoples from *certain* raiders into 79–98 % raiders
introduces a per-hop termination probability into what used to be a certainty,
and the length of an avalanche is exquisitely sensitive to its branching ratio.
Seed 1's settlement count went *up*. It is seed variation, which is what a draw
is supposed to produce.

The layer's behavioural neutrality at zero spread was proved rather than argued:
seed 42 rebuilt with every dispersion forced to zero produces a ledger
byte-identical to the pre-campaign fixture, at 14,561 facts. Everything that
moved is the draw, and nothing is the plumbing.

## A proof of the wrong thing

The specification asked for a mutation step: a test that goes red if the
dispersion parameter is ignored. Three tests were written, and they establish
that human's authored 0.35 produces a between-settlement variance of
0.1131030021 over 2,305 records while the same run with the dispersion zeroed
produces **exactly** 0.0, with every settlement drawing its authored location
bit-for-bit. Gate-open rates: 42.34 % against 0.00 %.

The review then ran three mutations against those tests, and the third one
matters. Handing *every* people a fabricated dispersion of 0.15 — a number no
author ever wrote — left all three tests **passing**, while the proof's own
output printed "authored sigma = 0.3500" beside worlds built with a different
sigma. The tests proved that the draw function reads its argument. They did not
prove that the authored value is the argument it receives. Closing that meant
extracting the configuration assembly into something a unit test could reach and
requiring a red against exactly that mutation, which now produces `left: 0.15
right: 0.2`.

A related instruction, issued by this campaign's own coordinator, was **wrong on
floating-point grounds and correctly declined**. The instruction was to assert
that the zeroed variance equals zero for every people. That holds for human and
goblin, whose authored location is 0.5, and fails for the other four at around
10⁻²⁷ — every settlement does draw the location bit-for-bit, but a mean computed
as a sum divided by a count does not round-trip for a non-dyadic location over
four thousand terms. The residue is in the estimator, not in the draw. What
shipped instead asserts exact per-draw equality against the authored location,
which proves the point-collapse claim directly rather than inferring it from an
aggregate statistic carrying summation noise. It is strictly stronger than what
was asked for.

## Two theoretical commitments, one of them only on paper

The society vector is now read explicitly as a **grid/group instrument**: how
rule-bound a life is on one axis, how bounded "us" is on the other, and four
biases at the corners, each carrying published predictions about cosmology, risk
and stance toward outsiders. The point of adopting the frame is that those
predictions become things to *derive from a position* rather than things to
author per people — adding a people becomes placing it on two axes rather than
inventing its culture. The commitment is deliberate and it is expensive to
reverse, which is why it is written down where a reader will meet it.

It is also, today, only documentary. No consumer reads a quadrant. Both axes are
per-people constants, so a quadrant term would have added nothing to the
between-settlement variance this campaign was measuring, and shipping it anyway
would have been an unpreregistered behavioural change with no measurement
attached. The frame is recorded so that the campaign that wants it inherits a
reading instead of inventing one.

The second commitment is what the campaign did **not** do, and it is stated
plainly because the design named three terms and only one was built. Warlikeness
was to become a function of structural pressure, drawn disposition, and
grid/group position, at which point the raid gate would stop borrowing a
*defensive* axis (flee ↔ stand) to decide a *proactive* question. Only the
disposition term exists. The gate still reads threat response. The mismatch is
narrowed — it is now per-settlement rather than per-people — but it is not
dissolved, and the structural term turned out to be unreachable from where the
history is baked: the strife field exists only downstream of the very history it
would have to inform. That needs a different design, not a different call site.

## A stale sentence, and where staleness comes from

Fourteen committed prose sites asserted, in one phrasing or another, that a
named people is an extreme of an authored axis — sentences like "the goblin
never raids anyone" that were true when written and are now false. Retiring them
took three sweeps of six, four and four, and each found sites the previous one
could not reach, because the first two were aimed at a phrasing rather than at a
claim. The third stopped guessing at phrasings and derived the ground truth from
the authored registries instead, printing the true extremes beside every
candidate so each could be judged against the source rather than against memory.

A fifteenth turned up later still, on a *different* invariant: a sentence
asserting that a species is never a distribution of its own. It carried no
people's name and no number, so nothing keyed to the extremal claim could see
it. Two premises went stale here, and enumerating them is its own step.

The best part is the part nobody was looking for. The claim that four values
were reachable on one axis went stale at **five** when a *previous* campaign
added a people whose time horizon of 0.2 is shorter than the bugbear's 0.3.
Bugbear stopped being the short extreme one campaign before this one touched it,
and nothing noticed. A campaign that widens a roster silently invalidates every
extremal claim anyone has ever written about it.

## What is owed

**The heavy-tier calibration battery went red, as designed.** It partitions the
roster into raiders and abstainers from authored means and asserts that an
abstainer re-seats its founding settlement rarely. Measured on the shipped
draw, the goblin re-seats on **52.1 %** of worlds against a bound of 0.25 — and
the bound was deliberately not retuned, because retuning it would rescue a
prediction whose *premise* this campaign destroyed. There is no longer a
non-raiding partition to bound. The number was never the thing to fix, and the
failure is the record of what changed rather than a defect to clear. (The
campaign's own estimate before the run was about 38 %; the measurement came in
half again as high, which is worth stating because the estimate was a
calculation and the measurement was not.)

Its failure text has been corrected to say what the partition now asserts. Its
bounds have not been touched.

One other heavy-tier failure has **open attribution and is not claimed here**: a
climate readout reporting that weather-gated conductance did not move world
topology. It was green before this branch existed and this campaign never
touched the file, but another campaign landed in between and changed the
placement gate, and the available data does not separate the two. Two further
failures — a scene-cost ceiling exceeded by 0.8 % and a census-fixture probe —
were already red on `main` before this branch existed and belong to neither this
campaign nor its readout.

The individual tier and the slow feedback edges are deferred by design. The
edges are the interesting half: sustained raiding should shift what earns
standing in a society, and doctrine should sanctify the shift and then **not**
decay when the pressure that caused it does. That hysteresis is what would give
belief its own autonomy — the capacity to outlive its cause and be wrong about
its own world — and it deserves a preregistration spanning centuries of
simulated time rather than a corner of someone else's campaign.
