# The Confidence Gradient

Not everything in Hornvale is equally understood, and this book would mislead
if it presented the settled and the speculative in the same voice. This
chapter is the standing map of which is which — and, unlike the world it
describes, it is meant to be **re-scored as campaigns resolve its questions**,
not written once and left.

The honest axis is not *how established the pattern is* — precedent was never
the thing at risk. It is **whether the world can grade itself on the claim, or
whether the claim rests on a human's judgment.** A bet the Laboratory can
score — generate the evidence, measure it, drift-check the number — is a bet
the project can drive to a verdict on its own. A bet that resolves only
against taste stays open for a structural reason, not for lack of effort.
Confidence still runs bottom-up; but the gradient it runs along is
*checkability*, and any claim about the top of the stack made with the
assurance of the bottom should be distrusted.

One sharpening, learned the hard way (The Named, 2026-07-16): **the
drift-check is not the checkable part — the anchor is.** A drift check pins
output against change and has no opinion about whether the output was ever
right, so a wrong number drift-checks green forever and every regeneration
re-ratifies it. The bets below are self-scorable because their metrics are
measured against something outside the generator (Earth's shoreline
development index, a null control, a preregistered threshold); the
drift-check only stops the answer moving once found. Where a committed
artifact has no external anchor — a rendered page, say — drift-checking it
buys stability and nothing else. The Named's defect sat in a drift-checked
artifact, in plain English, for eight days of green runs.

A second sharpening, and a different failure than the first (The Siding,
2026-07-29): **a check is worth only the configurations it actually runs in.**
The Named's lesson was that a drift check has no anchor; this one is that a
drift check can have an anchor and still never fire, because the command that
*regenerates* an artifact and the command that *verifies* it are never invoked
together. The census sat stale for 139 commits — wrong in three missing
columns and twenty-one drifted metrics — while every gate ran green, because
`make gate` `#[ignore]`s the tier that rebuilds it and `make rebaseline` never
touches it. The Sounding's wall-clock timings sat inside the drift-checked
tree for the mirror-image reason: nothing that rewrites them is ever followed
by the diff. Both artifacts were anchored and both were unobserved. The same
campaign wrote two checks into its own spec that could not have failed — a
zero-diff over a tree containing nanosecond timings, and a claim-status probe
that answered "no" from the wrong machine — and caught both only by running
them. So the gradient has a floor beneath checkability: a claim is only as
checkable as the *pairing* of its generator with its verifier, and an unpaired
check scores as unchecked no matter how good its anchor.

The successor campaign (The Timekeeper, 2026-07-30) built the instrument that
was supposed to close that gap and produced eight more instances of the same
shape *from its own plan text*, four of them inside the machine built to
detect them — including a duration alarm that compared each run against itself
and so could never fire. The count is now sixteen across the two campaigns, so
the floor needs stating as a practice rather than an observation: **the only
thing that reliably distinguishes a check that fires from one that does not is
making it fail on command.** A mutation step — corrupt the input, require the
red — found the never-firing alarm; five further findings came from a final
review that ran the system instead of reading it. Reviewing a check against
its specification cannot catch a specification that disagrees with itself, and
one of these did: the contention guard was wired backwards against a rationale
written three lines above it, and passed review as faithful to the plan.

The Sexton (2026-08-13) is the first campaign to **pair** one of those
generators with a verifier in the everyday command, and the result argues the
floor is right. The census is no longer checked only by the tier `make gate`
ignores: a three-world, all-metric sentinel now runs *inside* the commit gate
against the committed census, for about fifteen CPU-seconds. Within hours it
had verified three other campaigns' byte-identity claims — The Millrace's, The
Fathom's, The Holdfast's — on the census path, which is exactly the path a
worldgen campaign cannot cheaply check for itself. That is the pairing the
floor asked for, and it fires on a schedule nobody has to remember.

The same campaign also supplies the sharpest instance yet of the floor's own
failure mode, and it is worth scoring honestly against the bet. Decision 0130's
channel work added **467 CPU-seconds** to the commit gate and made one test the
slowest in the workspace at 187 s — unnoticed for the same structural reason,
because the instrument that would have seen it (`make ci`) had run **nine times
against `make gate`'s 368**. An unpaired check scores as unchecked; so does a
paired one nobody runs. The Sexton folded the alarm into the gate, taking that
instrument from nine samples a month to 368.

**Score: the bet moves toward checkable, and the practice sharpens.** Four more
instances of a check that could not fire arrived from this campaign's own plan
text — an extractor whose grep matched zero of 3,449 real events, a
scratch-sweep test that passed without entering the code under test, a
worktree enumeration that would have switched the main checkout, and a status
read from a file that reported green when the file was empty. All four were
caught by making the check fail on command. But three of the campaign's own
*measuring instruments* also lied — a sampler counting its own `grep`, a
false-negative `grep`, a planted violation that was not one — which extends
the practice by one clause: **make it fail on command, and run the positive
control, because a negative result from an instrument nobody has seen fire is
not evidence.**

**A note from The Staff (2026-08-14), amended by The Sluice (2026-08-15),
since this score is read against an instrument this passage names by a label
that no longer exists.** Both halves of Sexton's pairing — the census
sentinel and the duration alarm — lived inside the single gate this chapter
calls `make gate`, which decision 0132 has since split by purpose into
`gate-commit` (local, every commit) and a **stage gate** on the canonical
box, run at each plan-stage boundary. The sentinel costs 12–25 CPU-seconds
depending on host, above the one-second floor that defines `gate-commit`'s
test tier, so it no longer runs at every commit; the duration alarm moved
with it, because both live inside the same `cargo nextest run --workspace`
invocation, which is now the stage gate's body rather than `gate-commit`'s.
Decision 0139 then moved the stage gate again without changing what it runs:
it is a request to the canonical box's serial merge queue now (`make
sluice-stage`), taking the same claim and running the same phases as a merge
does, but reporting instead of pushing. The pairing itself survives —
generator and verifier still run together, just inside the slower gate — but
the **frequency** this passage's score leans on does not automatically
survive with it: nobody has yet counted how often the stage gate runs in a
month, so "368" is not the number to read off this passage anymore, and no
replacement has been measured. Score this bet as still paired, at a
currently unmeasured frequency — not as reconfirmed at the old one.

A third campaign extends the tally in a way that narrows the diagnosis. The
Repertoire (2026-07-31) built a capability probe that touches no world state,
draws no seed and commits no fact — and produced the same family anyway, from
its own plan text: a coverage ratchet that read `REBASELINE=0` and an empty
`REBASELINE=` as permission to rewrite the artifact it was guarding, and a
resolver whose unknown-requirement branch returned *satisfied* rather than
*blocked*, inverting the default-deny posture its own spec had set. Both were
found by mutation — tamper with the input, require the red — and neither by
reading. So the pattern is not a property of measurement code, or of
determinism-critical paths, or of instruments that watch themselves. It is a
property of **plans written as literal code listings**, which get reviewed for
faithful transcription and not for whether the predicate they contain is the
one the spec asked for.

The Ballast (2026-08-15) adds a fourth shape, and it is the one this practice
does not reach. Every instance above is a check whose *predicate* was wrong —
a grep matching nothing, a branch returning satisfied, a guard wired
backwards. Mutation finds all of them, because mutation asks whether a check
can fire. The commit gate's test roster failed differently: its predicate was
correct and it fired reliably, for every test on its list. The list had simply
gone short. One crate had no entry at all, so none of its tests ran anywhere,
and the gate reported green about everything it was looking at while looking
at less than it claimed to.

Making that check fail on command would never have found it. Corrupt the
roster and the gate reddens exactly as designed; the check is not broken.
**Mutation proves a check can fire; it says nothing about whether the check is
pointed at everything.** An allow-list has the additional property that it
gets *faster* as it goes blinder, so the symptom of its decay is
indistinguishable from the improvement it was built to deliver — and the
mechanism documented to keep it current, which wrote its updates to scratch
that the next job erased, had never once run to completion in the file's
entire two-commit history.

**Score: the bet holds, and the practice gains a second half.** Making a check
fail on command remains necessary and remains the cheapest thing that works.
It is not sufficient, because it verifies the check against itself. A check
also needs an *enumeration* it is answerable to — a list of what ought to be
covered, with an absence from that list treated as a failure rather than as
silence. The first practice catches a check that lies. Only the second catches
a check that was never asked.

A fourth campaign puts the sharpening where the *repair* is. The Collation
(2026-08-06) produced the same shape from its own plan text — a spec promising
one test asserting a generated matrix's per-column figures equal the per-corpus
reports' own, and a plan listing that asserted only that the rendered document
*contains* each corpus id and each denominator, which any cell reading
`(217/409)` satisfies. Review caught that. The replacement was a whole-file
byte comparison of the matrix against its own committed copy, **proved to fire
by a mutation**, and it passed a second review on that proof — while still
being unable to fail for the reason the spec named, because the two figures
came from duplicated tally code and rebaselining accepts both documents in the
same pass. The whole-branch review caught the second one, and settled it by
running the finding's own scenario: perturb one renderer, rebaseline, and watch
the byte check go green while a cross-derivation test stays red. So making a
check fail on command is necessary and is **not** sufficient: the mutation has
to be the failure the check was promised against, and a check written to
replace one that could not fire inherits the burden of proof rather than the
credit.

A fifth campaign closes a corner instead of finding a new instance of one.
The Assay (2026-08-07) is a worked instance of decision 0097's middle
corner — the check that cries wolf, sitting beside the check that cannot
fire (this chapter's original floor) and the drift check with no anchor
(The Named). Three gate-resident tests were sweeping up to 200 worlds each
to answer an existence claim — does a live prediction crisis occur
*somewhere*, does every `Hydro` variant appear *somewhere*, does some world
win every toponymic concept — the exact shape 0097 names as deciding on
whichever single draw happens to sit nearest a hunt's break condition.
Moving them onto the census's 1,000-world fixture, as a coverage table and
a measured rate rather than a boolean the next campaign's true change could
flip, is what 0097 §2 prescribed and had not yet been built. The mechanism
this campaign shipped to make that safe — a tripwire that rebuilds three
fixed seeds every commit and compares them against the fixture — was itself
proven by mutation before anything moved onto it, per this chapter's own
standing practice: a corrupted fixture cell turns it red, naming the metric,
the seed, and both values; restoring the cell turns it green again. The
same campaign also produced a sixth instance of a lesson this chapter has
tracked under other names since The Timekeeper — a claim of absence
asserted from an incomplete search, this time by the controller itself,
mid-sentence while cataloguing five other agents making the identical
error, caught only because the dispatch had told the correcting agent to
verify rather than comply. See `docs/retrospectives/the-reassay.md` for the
full account, including a naming collision with an unrelated, already-shipped
campaign that shares this one's title — itself one more claim of absence
("the name is free") nobody checked.

A sixth campaign adds a corner none of the five describes. The Digest
(2026-08-08) set out to codify what the project holds about itself, and its
central task was halted early on the conclusion that the intended finding did
not exist: a rule forbidding numbered identifiers in the idea registry was
enforced by a gate check that passed, against a frozen fixture that the live
corpus matched exactly, with zero identifiers outside the list. Every
observation was correct. The conclusion was wrong. The fixture was written
seventeen days *after* the rule took effect, and in that interval the corpus
grew from 171 numbered identifiers to 403 — so the freeze grandfathered 232
violations and the check has been certifying conformance to them ever since.
This is not a check that cannot fire, nor one without an anchor, nor one whose
generator and verifier are never paired: it fires, it is anchored, it is
paired, and it is green for the right reasons. The defect is that its
**baseline was cut after the damage**, which is invisible in current state by
construction and recoverable only from history. So the floor gains a question
to ask of any conforms-to-a-frozen-list check: *when was the list written, and
what was already true when it was?* The campaign's delta view answers it by
reading git rather than the working tree, which is the only place the answer
lives. The retraction itself is worth recording — the wrong conclusion was
reached twice in one day by the same reasoning, first from a 1,055-commit-stale
timing baseline, and was overturned by a human disagreeing with an artifact
that looked authoritative.

Two campaigns reached this thread independently on the same day, from opposite
ends of it — one from a check that could not fire, one from a check that fired
wrongly. That they collided here, in a merge, is itself the strongest evidence
the chapter offers about how common the family is.

A seventh campaign finds the family in a place none of the six had looked. The
Domesday (2026-08-08) built a generated survey of the thousand-world census
whose whole purpose is finding weaknesses, and produced three unenforced guards
*inside the detector module itself*: a rendered sentence whose stated rule was
false as worded, a hand-frozen roster of measured crates with no live guard
beside a partner roster that had one, and a boundary test that re-implemented
the membership filter in its own body instead of calling the code it tested, so
emptying the real function left it green. A fourth, caught earlier, was a quine
— a test asserting the survey never builds a world, implemented by scanning its
own source for forbidden identifiers, which were present in that file by
definition. All four were found by an independent reimplementation that *ran*
the system rather than reading it. Knowing this failure mode confers no immunity
to it, which is now demonstrated rather than suspected.

The same campaign extends the family beyond checks entirely, and that is the new
corner. A **metric** can be unable to fire. The chorus sky-calibration metric is
a Kendall tau over [−1, +1] that reads exactly −1.000 on all one thousand
worlds, because it correlates a culture's sky capability against its sky-domain
distortion — and distortion in that domain is the fraction of sky facts lost,
while a sky fact is lost precisely when capability falls below the fact's
threshold. The two series are coupled through the same comparison, so every
strictly-comparable pair is discordant and the coefficient saturates by
construction. It is anchored, paired, drift-checked, current, and read by a
preregistered study — and it carries no information about any world. So the
floor gains a second question, asked of measurements rather than of checks:
*can this metric take a different value on a different world, and if not, what
is the study reading it actually scoring?*

An eighth campaign, landing the same day, moves the finding from the check to the *order of its
repair*. The Assize (2026-08-08) inherited a queued task reading "mechanize the
prose discriminator" — the cost batteries' rule for telling a contended run
from a regression, written in a module doc and applied by hand. Measured
against the run it was written for, the rule gives the **wrong answer**:
`genesis` at 2.09x its recorded basis with four control metrics at 0.96-1.29x
reads, under "a real regression is LOCAL", as a regression — while a quiet box
builds the same world in 3948 ms against a 13000 ms ceiling. The rule fails
because the five metrics have different resource profiles, only one of them
sculpting terrain, so a saturated runner starves exactly one and uniformity was
never the right test. Executing the task as written would have promoted a wrong
predicate from prose into code, where it would carry the authority of having
been computed. **A criterion earns mechanization by being correct, not by being
written down**; and the corrected version paid at once, finding a real localised
cost increase on its first run and bisecting it to a single commit while four
named sibling candidates moved it by zero.

The same campaign supplies the corner's other half, from its own repair. The
mechanized verdict then gave a confident wrong answer on a *second* battery,
because the two files' bases had been measured on different machines and the
heavy tier runs on only one of them — a ratio across two machines measures the
machines. The fix is the shape this chapter keeps arriving at: a check that
**states its own applicability** and declines rather than computing, printing
"bases were measured on aarch64-10, this is x86_64-40" and suppressing only the
ratio-derived claim while the raw milliseconds stand.

Two smaller instances from the same campaign are worth the floor's attention
because neither is a test. A committed, published artifact carried a headline
line reading `0 migration events (floor 5). PASS` — a verdict string never
wired to any check at all, which is this chapter's original floor in its purest
form (a check that cannot fire, with no check behind it) and which stayed
invisible for as long as the number it reported was healthy. And a rank-order
bound adopted to replace a rotted threshold turned out to be **unfalsifiable by
the only two mutation controls its file carries** — the correlation moves 0.831
to 0.127 under one of them and never crosses zero. It was retained, relabelled
in place as a directional record rather than a proven guard, and the tempting
threshold that *would* have made it fire was refused because it would have been
chosen for firing. Disclosing an unfalsifiable check beats quietly shipping one,
which is this chapter's practice working rather than a new failure.

A ninth campaign returns to the module the seventh worked in, knowing what the
seventh found there, and finds two more. [The Armature](./chronicle/the-armature.md)
(2026-08-09) set out to declare thirty causal links across the census — *this
quantity should move when that one does* — freeze them from physics before any
correlation was computed, and measure once. The first defect is arithmetic
defeating a guard that reads correctly. The correlation function documents that
it returns nothing when either column is constant, and tests that by asking
whether the variance is at or below zero; but `sum()` accumulates left to right,
so a thousand copies of one value leave the mean about `1e-13` off the constant
and the variance at roughly `3.4e-22` of rounding residue — **strictly greater
than zero**. The function returned correlations computed entirely from summation
noise, on precisely the six rows the campaign existed to interrogate, and the
survey published them. Nothing about that guard is wrong as written; it is
wrong as *evaluated*, which no amount of reading it catches. The second is a
comment disagreeing with the code beneath it in the direction that flatters the
result: the detector's strength branch withheld the observed sign
unconditionally, three lines below a comment stating that the sign is withheld
only "when nothing was measured", so three measured backwards couplings — the
worst of them `r = −0.755` against a declared *positive* link — rendered as bare
strength mismatches beside a headline reading *zero backwards links*.

Both were found by a reviewer who left the repository's own tooling and
re-derived every published number in another language, and one was confirmed by
**reverting the repair** to check that the resulting red was behavioural rather
than a compile error. That count is now seven instances in a single module
across two consecutive campaigns, each of which knew the family by name before
it began. The floor's practice therefore gains a narrower question, aimed at
the residue that mutation testing does not reach: *is this guard's predicate
true of the values it will actually see, or only of the values it describes?*
A zero-variance test is exactly right about mathematics and exactly wrong about
floating-point summation, and the gap between those two is where this one
lived.

The same campaign contributes the family's most useful positive result, and it
belongs here rather than with the failures. Silence from an instrument had been
carrying two meanings — *the claim held* and *the claim could not be tested* —
and the second is indistinguishable from the first at the point of reading.
Six declared links now report `D5 unmeasurable`, naming the frozen column and
its single value, rather than passing quietly; and a sign, once measured, is
never withheld. **An instrument's silence must mean exactly one thing**, and
everything else it might have meant needs its own name. That is the cannot-fire
family stated as a design rule instead of as a caution, and it is the first time
this chapter can offer one.

A tenth campaign moves the family off checks and metrics entirely, onto the
data. The Particular (2026-08-10) registered four predicates for individual
persons, one of which — the day a person died — is committed only once that day
has passed. It has never been committed. Across three seeds and 364 promoted
founders the count is zero, because the arithmetic deriving the death day
subtracts a maturity in *days* from a founding day in *years* and compares the
sum against a present in years; the gating condition is unsatisfiable for every
species in the roster by an order of magnitude. The predicate is registered,
documented, hand-tested on both of its branches, and counted by the capability
probe as vocabulary the world holds. The live-world test that walks every person
and asserts death follows birth contains a conditional that has never once been
entered, and stayed green throughout. So the floor gains a third question, asked
of vocabulary rather than of checks or measurements: *does any world actually
produce this?* Nothing in the suite asks it. The instance was found by scoring a
preregistered prediction numerically at the close, which is the only step in the
campaign that computed a figure the tests did not already assert.

The same campaign supplies a smaller lesson about preregistration itself. Its
size bound — the ledger grows by no more than 2.1% — was **falsified** at 6.25%,
while the exact-count identity beside it held to the fact on every seed. Both
readings are correct and together they localise the cause: the mechanism did
what it was specified to do, and the estimate was computed against a world that
had since been rebuilt, its fact count falling by a factor of three and a half
in the interval. A ratio frozen before the code is only as durable as its
denominator, and a prediction expressed as a *fraction of current state* silently
re-aims itself every time that state moves. Freezing the numerator as an identity
is what made the falsification readable instead of merely disappointing.

An eleventh campaign repairs the tenth's finding and, in doing so, measures how
much of the family the repair itself contained. [The Ell](./chronicle/the-ell.md)
(2026-08-11) moved the unit boundary that made a death uncommittable: the
history bake keeps reasoning in years, and what crosses into the ledger is days,
converted at named functions rather than at inline divides. That created sixteen
unit crossings, and the campaign swept them by mutating each one in turn against
the whole gate. **Five of the sixteen had no guard at all.** The most
consequential was not on anyone's list: the crossing that feeds the census's own
name renderer, whose blast radius is a committed census value the commit gate
never rebuilds — and whose two plausible existing guards cannot catch it *by
construction*, one because it counts zero-gap edges (zero is zero in any unit)
and the other because it keys a map on the year form, which is invariant under
any injective rescaling. Both tests are correct, both are green, and neither is
about the quantity that moved. So the floor gains a fourth question, asked of
conversions rather than of checks, measurements or vocabulary: *which test goes
red if this crossing is deleted?* — answered by deleting it, not by reading the
suite.

Two smaller instances from the same campaign sharpen what a guard has to be
compared against. An invariance test that had been green for months compared a
reconstructed record against a committed one — by comparing **one fixture
against another fixture**. When the ledger's unit moved, the module's stated
premise became false and all four of its tests stayed green, because both sides
were wrong the same way; the sibling case in the same sweep went red on its own,
because there the two sides were production and fixture. And a collision guard
written to detect a new failure shape turned out to be **entailed by the key it
guards**: the key folds the parent's coordinates, so any colliding pair
necessarily has equal parent coordinates and the assertion cannot fail while the
key is what it is. It was kept and relabelled as a tripwire for a future
narrowing — the honest description of what it can do — rather than deleted or
left claiming more than it holds.

**No bet in the map below moved.** The Ell repairs a unit boundary and a derived
key; it resolves no open question about the world, raises nothing from taste-gated
to self-scorable, and leaves every score in this chapter where it stood. Recorded
explicitly, because a campaign that changes the save format and every founder's
name looks from the outside like it should have moved something, and decision
0030's sweep is answered by a statement either way rather than by silence.

A twelfth campaign contributes the family's densest single instance and, with
it, the first useful statistic about *detection*.
[The Compendium](./chronicle/the-compendium.md) (2026-08-15) built one
resolver — the anchor parser and audit in `cli/src/systems.rs`, most of it
resolution logic rather than rendering or its own inline tests — whose
entire purpose is noticing when a citation stops being true, and produced
**four separate false-cleans inside it**: a symbol match that accepted any
name it was a prefix of; a fallible
operator inside a loop, so the guard's count propagated an empty result and
could never fire; an exact string comparison against a status vocabulary whose
real cells carry qualifiers, emphasis and transition arrows, leaving roughly a
fifth of the rows it guards permanently unfalsifiable; and a citation of a test
that is compiled but never run, which the resolver called resolved. Three are
the same category error wearing different faces — treating a syntactic
coincidence as a semantic fact — and each was made after the previous one had
been found and fixed.

The statistic is in *who found them*. One by review, one by the implementer
using the tool rather than testing it, one by the controller reading a task
ahead, and one by accident while hunting better evidence for an unrelated
verdict. **Four detection mechanisms, each of which found exactly one.** Every
prior entry in this thread argues that a particular check could not fire; this
one argues something narrower and more actionable about the searching:
redundant detection is not redundant when each detector has a different blind
spot, and dropping any one of these four as duplicative would have shipped a
false-clean in an instrument whose whole claim is that the citation is the
evidence. The third defect was also, on inspection, the controller's own — a
rule generalized from a single real row whose status happened to be the one
unqualified form — which is this chapter's standing lesson that a correct
observation and a false generalization are routinely the same sentence.

**No bet in the map below moved.** The Compendium ships an instrument and one
reading of it; it resolves no open question about the world and re-scores
nothing here. Its one finding *about* the world is a render gap rather than a
sim one: the first capability in an external catalogue this project cannot
replicate is that catalogue's own first page — entities carrying their own
appearance — and it reads **absent**, because the shipped character-grid
client draws one glyph for the possession and one for everything else in view,
terrain and marks alike. The catalogue's *refusals* are a separate and smaller
set, each tracing to a ratified decision rather than to a deficiency; they
confirm existing positions rather than moving a bet.

A thirteenth campaign contributes two corners the practice does not reach,
and a correction to a score written above. [The Sluice](./chronicle/the-sluice.md)
(2026-08-16) built a serial merge queue and produced **fourteen** defects from
its own plan text — the largest single tally this thread has recorded, from
the same source every other tally names. Two were guards described as live
that nothing could redden, and one was a check written against a situation
that cannot occur: it grepped for git's default merge subject `Merge branch …`
against a chamber that merges a bare identifier into a detached head, where the
default is `Merge commit '<sha>' into HEAD`. Those belong to the family already
described. The two below do not.

**The first corner is a check whose predicate is right and whose model of the
world is wrong.** A static lint scanned one file for commands that would write
to the real remote. It was defeated three times, and not once by a missing
pattern: first a denylist that could not enumerate the shell's syntax, then a
hand-rolled word boundary that matched nothing at all, then a line-continuation
joiner that inserted a space where the shell deletes the backslash outright —
so a command split across two lines in an unusual place really executes as a
force-push while the auditor reports zero violations. Mutation does not find
this. The check fires; it fires reliably; it is pointed at everything it claims
to be pointed at. What is wrong is its *model of the language it reads*, and
each of the three looked correct by inspection. The lint was deleted rather
than patched a fourth time, because by then it had acquired a worse property
than the thing it protected — an unbalanced brace in the audited file closed
the test wrapper early, so the audited content executed while the audit
reported clean. The replacement is a runtime hook that refuses the operation
rather than a reader that predicts it. So the floor gains a fifth question,
asked of any check that parses rather than executes: *whose model of this
language is this check using, and has that model been made to disagree with
the real one on purpose?*

**The second corner is a generator with no verifier at all, inside the gate.**
The project's whole-world tier authors four committed artifacts. Two of the
four tests compare what they build against a committed copy and fail when they
differ; the other two only write. They assert nothing about what they wrote, so
both artifacts went stale through a run reporting eighty passes out of eighty.
This is The Siding's unpaired check with the pairing broken at the other end:
there, the generator and the verifier existed and were never invoked together;
here the verifier does not exist, and its absence is concealed by a green
number in the same run that produced the drift. **A test that authors an
artifact must also assert it**, and an authoring test that asserts nothing is
not a weak check but a zero one wearing a passing test's clothes.

**And a score in this chapter needs correcting.** The passage above credits
The Ballast with building the missing half of the commit gate's roster loop —
a copy of a green run's list to durable storage, carried back by a person. It
never moved a byte, and neither published explanation of *why* was the cause.
The list has one commit in its entire history. The blocker sat a step earlier
than anyone had looked: the command that writes the list refuses whenever the
machine's exclusive claim is held, and every serialized path in this project
runs that command as a descendant of the process holding the claim — so the
one thing that produces the list declined to, in the one environment where
nothing else was running, and every mechanism downstream faithfully carried an
unchanged file. Two campaigns diagnosed the symptom correctly and the cause
wrongly, each building a remedy for the step it had found. The floor's second
half — a check needs an enumeration it is answerable to — is unchanged and was
right. What is added is narrower and aimed at repairs: **a remedy verified only
at the step it was built for cannot tell you the pipeline ever ran**, and the
cheapest thing that would have settled it, in either campaign, was reading the
file's own history and finding one hand-written entry.

**No bet in the map below moved.** The Sluice changes how work reaches merged
reality; it resolves no open question about the world, raises nothing from
taste-gated to self-scorable, and leaves every score below where it stood. Its
one finding about the world arrived sideways, through a survivor the merge
queue held on: a unit conversion from founding years to ledger days whose
surrounding tests asserted only *ordering*, which a uniform rescaling cannot
disturb. That is [The Ell](./chronicle/the-ell.md)'s question — *which test
goes red if this crossing is deleted?* — answered once more in the negative,
and closed with a cross-check rather than a threshold.

## What the world can already check itself on (high confidence)

**The kernel substrate.** Hash-based seeding, coherent noise, append-only
event-sourced storage, deterministic serialization, triple-shaped facts.
Decades of precedent, thousands of implementations, and now stress-tested
under nine domains and five windows without cracking. This layer was
deliberately chosen to be boring, and the choice paid. The substrate has
since grown a shared typed-unit vocabulary — the elevation datum and the
temperature pair, ratified as standing doctrine (decision
0044) — and the way it landed belongs at this
confidence tier for the same structural reason: each migration's central
claim, *this changed nothing*, was scored by the world itself — every
committed artifact regenerated byte-identical — rather than by anyone's
judgment ([The Datum](./chronicle/the-datum.md),
[Temperature](./chronicle/temperature.md)).

That confidence is well placed and slightly too narrow, and
[The Benchmark](./chronicle/the-benchmark.md) says where. "This changed
nothing" is a strong claim about a migration, scoreable by the world — and it
is silent about whether the migration was *complete*. The elevation wave
introduced `ReferenceElevation` and left its sea-level-relative sibling
unbuilt on a stated condition; the condition was later met, nothing watched
for it, and the gap held a real defect that banded three quarters of a world's
land into a marine relief class and published it. A vocabulary can be
byte-identically correct at every step and still be missing the term that
would have made a wrong reading unsayable. So the tier is right about what it
measures and should not be read as covering coverage: the substrate scores
*changes* against itself honestly, and does not yet score its own gaps.

The same substrate now scores its
own *completeness*: [The Correspondence](./chronicle/the-correspondence.md) made
every modeled concept account for its manifestation across the lexical,
perceptual, and cognitive ledgers or record a typed void, so a drift-checked
trial balance — not a reviewer's memory — reports what the world models but
cannot yet name, perceive, or think.

That instrument sat at zero for its whole life until
[The Vernacular](./chronicle/the-vernacular.md) put the first entries in it: nine
concepts declaring, in the registry rather than in a comment, that a star's
spectral class is real and that no culture here can name it. The sharpening is
worth the tier it sits at, because it is about the *instrument* and not the
reading. A trial balance that can report a class and never has is not yet known
to work — and this one, once exercised, immediately found that its own claim
evaporated across a save boundary and that the language layer was minting words
for concepts the registry had just declared unnameable. Confidence here rests on
a ledger having been *made to answer*, not on its having been built.

**The divergence method** — once the year-one research bet, now the project's
own instrument of proof. Generate two worlds differing in a single pin, hold
everything else, and measure whether the downstream culture differs *legibly*.
Year 1 varied the sky: the same land and society under a spinning sky crowned
the cyclic [Wheel-Turner](./gallery/the-gods-seed-42.md), and under a tidally
locked twin crowned the eternal Still Crown — a world with no seasons to
mythologize ([Campaign 5](./chronicle/campaign-5.md); a near-upright locked
world still has none, but [The Wandering Sun](./chronicle/the-wandering-sun.md)
later gave a *tilted* locked world its own libration season, so the Still
Crown reads now as the zero-obliquity limit rather than the whole locked
case). Year 2 inverted it,
varying the observer and holding the sky: two species differing only in their
authored parameter vectors grew different languages and religions, verified by
a 500/500 null control and a blind-attribution metric pinned honest at 0.875
([The Meeting](./chronicle/18-the-meeting.md)). This is no longer *the actual
research*; it is how the research checks itself, and it is applied afresh to
every new layer. [The Pigment](./chronicle/the-pigment.md) applied it to
colour and got the sharpest instance yet, because the observer parameter is a
single scalar: two peoples differing only in night vision descend Berlin &
Kay's ladder to different depths, so the same iron-rich outcrop under the
same light is *yellow* to a goblin and *red* to a kobold — neither holding
the word *brown* that is actually nearest. Self-scorable, and already scored:
the census pins mean hue-depth at 4 and 2 respectively, and flattening the
derivation reddens the claim.

*Sharpened by [The Beholding](./chronicle/the-beholding.md) (2026-08-07),
which pushed the same single scalar past the word and into the picture.* Two
observers derived from nothing but night vision now emit different pixels
from the same rock in the same light, and the pixels are captioned with what
the projection dropped. What makes this a sharpening rather than a repeat is
the shape of its central claim: the prediction that a dichromat separates red
from green less than a trichromat does was frozen **false**, on a measurement
taken before any code existed, with a standing instruction to ship the null.
It came true — 0.0541 against 0.0680 — without a single constant moving,
because the falsification had correctly diagnosed the *metric* rather than
the model. A chromaticity that counts an achromatic channel makes every eye
with a rod a trichromat. That is the divergence method turned on its own
instrument, which is a stronger result than another confirmation would have
been, and it lowers rather than raises the confidence owed to any
observer-varying claim whose metric has not itself been probed.

*[The Lantern](./chronicle/the-lantern.md) (2026-08-08) paid that debt on the
very next claim, and the payment is the reason the tier holds.* It set out to
show that a rod-dominant eye sees where a human does not, and the claim held at
the model level — at an illuminance of `1.6e-6` a human's emitted colour is
`[0, 0, 0]` and a kobold's is not, with the kobold's three slots **equal**,
which is what proves the pixel came from the achromatic path rather than from a
cone channel that happened to survive. But two constants in that term turned out
to be load-bearing, and both are the metric-not-model failure this bet was just
warned about. Normalizing each observer's rod by *its own* curve would have
divided a species' night vision straight back out and rendered a kobold
pixel-for-pixel identical to a human — the divergence computed correctly and
attributed to nothing. And at unit gain the rod's image falls below one screen
count *everywhere in its own regime*, so the term would have shipped green and
changed no pixel anywhere. Both were caught by probing the instrument rather
than by reading the result, which is the practice the previous paragraph asked
for; neither was visible in a passing test. The bet stays where it is, and what
this adds is a second worked instance of the same discipline rather than a
second confirmation.

The same campaign supplies the sharper caution about *populations*. Its material
claim — that two settlements on different bedrock produce visibly different
walls — held across 1505 settlements over eight seeds, at a median of 41 `u8`
steps out of 255. The tenth percentile is 1, and every sampled flagship
settlement stands on the same rock class, plausibly because the biggest
settlements go where the rivers are ([The Confluence](./chronicle/the-confluence.md)).
So the population varies and the head of it does not, and a possession always
starts at the head. A divergence claim measured over a population is not
automatically a claim about what anyone will see.

**Population has a physically-grounded, self-checking prior.** Every
settlement used to carry a population number a formula handed it, with no
account of what the land could support and nothing to catch an absurd
total. A carrying-capacity field, closed-form and seed-free, now stands
under every settlement in every world, and its central claim — that
supported capacity tracks the real biomass-by-latitude gradient — is
exactly the kind of bet this chapter cares about: preregistered before the
sweep, measured across two hundred generated worlds (the tropical-and-
temperate band supports roughly 27× the polar band), and frozen only after
the measurement confirmed it, not before. A second guarantee is checked
even more tightly: settlements condense as attractors of a population flow
over that field, so the sum of every settlement's population equals the
sum of the field exactly, by construction, not by tuning ([The
Gathering](./chronicle/the-gathering.md)). This is a genuine promotion, not
a full resolution — see "The standing horizon," below, for the half that
is still ahead.

*Re-scored by [The Vacancy](./chronicle/the-vacancy.md).* The self-checking
half got sharper and the modelling half got narrower, and both belong in the
score. Sharper: a committed readout of where each kind actually lives, plus a
rule that no kind may have zero capacity everywhere, caught four species — the
three chromatic dragons and the owlbear — that had zero carrying capacity on
every cell of every world and had been in the registry, absent from creation,
for four campaigns. A prior that can catch that about itself is doing the work
this section credits it for. Narrower: capacity is a supply term spanning orders
of magnitude multiplied by a condition product bounded in the unit interval, so
an authored ecological niche can only modulate the primary-production signal,
never select against it. A species authored for a particular climate can be
genuinely present there and still rank below species with no affinity for it —
measured, on a people authored for desert that has no desert at all. The
gradient claim and the conservation guarantee are untouched; what is now known
to be beyond the prior is *placing a species where its traits say it belongs*.

*Partially re-scored up by [The Warren](./chronicle/the-warren.md), which
supplies the first counterexample to the "can only modulate, never select"
half.* The reasoning above is sound about the **condition product** —
four tolerances multiplied together, each bounded in the unit interval, cannot
overcome a supply term spanning orders of magnitude. But it silently assumes
that a species' traits reach capacity *only* through that product. A realm does
not. A kind that declares itself subterranean is scored against the chamber
rather than the hillside **and multiplied by whether the cell holds a cave at
all** — a hard zero, not a bounded tolerance, on eighty-eight percent of land.
Measured over twenty-five worlds: 390,813 land cells with non-zero fit fall to
46,993, and no supply magnitude anywhere recovers the excluded ones.

So the sentence needs a qualifier rather than a reversal. *An authored
tolerance* can only modulate. *An authored realm* selects, absolutely, and is
the first mechanism in the model that places a species where its traits say it
belongs by excluding everywhere else. The scope of the win is narrow and worth
stating: it is one binary axis, carried by two fauna kinds, and it does nothing
for the desert-authored people that started this paragraph — a surface kind
still has no gate to be excluded by. Whether that generalises past caves is
open, and is the first thing a campaign placing a *people* underground will
find out.

*Re-scored down by [The Keeping](./chronicle/the-keeping.md), which contradicts
the sentence immediately above.* The gradient claim is **not** untouched — not
because the gradient is wrong, but because the measurement offered for it could
not have disconfirmed it. The polar term of that ratio is exactly zero often
enough that the metric floors it at one percent of a baseline unit to avoid a
division by zero, so a ratio computed against a floored zero is largely a
statement about the floor. The figure is recorded in the metric's own
documentation, one line from the claim it undermines. Two further problems ride
along: roughly one world in twenty is tidally locked, and a locked world's warmth
is organised around the point beneath its star rather than by latitude, so a
tropical-versus-polar comparison on those worlds samples hot and cold ground
alike and reports almost no gradient — they sit inside the pinned average, in
exactly the failure mode this section claims clearance from. And the productivity
field is not the published model its own documentation cites: that model rises
monotonically with temperature and never reaches zero, while the implementation
is a symmetric tent that reaches zero a little above freezing, which is why no
world is inhabited cold.

What survives untouched is the **conservation** guarantee — the sum of every
settlement's population equalling the sum of the field is by-construction
arithmetic, not a measured bet, and nothing here touches it. What is demoted is
the *evidential standing* of the gradient claim, which is a subtler and more
uncomfortable thing than being wrong: the reading itself is plausible, sitting
inside the band the published model predicts from theory alone. It was the
evidence that was not evidence. This chapter's own standard — preregistered,
measured, frozen only after confirmation — was met in form and not in substance,
and the campaign that found it was looking for something else entirely.

The Vacancy's re-score deserves credit here for seeing the symptom first: it
recorded that capacity is *"a supply term spanning orders of magnitude multiplied
by a condition product bounded in the unit interval,"* so an authored niche *"can
only modulate the primary-production signal, never select against it."* That is
the same defect, named a campaign early. The Keeping supplies the cause — the base
field takes the scarcer of its two limits while the layer above it multiplies four
tolerances together, so one half of the model obeys the law of the minimum and the
other half does not — and measures the resulting compression at roughly fourfold.

*Re-scored again by [The Tilth](./chronicle/the-tilth.md) and
[The Tense](./chronicle/the-tense.md), which move the claim sideways rather than
up or down.* The prior is now a strictly finer object than the one this section
was written about, in two independent ways: capacity carries a **species** index
(a cell is worth an amount *to someone*, so an authored niche can select rather
than only modulate — the defect The Vacancy named and The Keeping traced) and an
**era** index (so a glacial maximum makes ground poor instead of switching it
off). Three mutually inconsistent oracles for the word "habitable" — an era mask,
a capacity test, and a separate refugia rule, the first two disagreeing over
roughly half of all land — collapsed to one (decision 0107).

None of that is yet a promotion, and the reason is worth stating precisely. What
the arc bought is that the model can now *express* the thing it was previously
unable to say; what it did not buy is evidence that the values are right. The
gradient claim's evidential standing, which The Keeping demoted, is untouched
here — the floored-polar-term problem and the tidally-locked worlds inside the
pinned average are both exactly as they were. And the change has a measured cost
that no one predicted: replacing a gate with a continuous squeeze **compresses
the variance between worlds**. A seed that had been permanently dead now carries
36 communities across 70 sites; the flagship seed fell from 209 settlements to
122, its chief settlements losing a third to a half of their people. Dead worlds
live and rich worlds thin.

The thousand-world census puts a number on the lower half at the close, and it
is larger than the anecdote suggested: **231 of a thousand worlds could not seat
a goblin flagship before, and one cannot after.** A quarter of the sample
crossed from nameless to peopled, which is a real gain in how much of the seed
space is worth visiting, and it is the half of this trade that is unambiguously
good. The same census found flagships moving decisively inland — 73% coastal to
22% — which nothing predicted and which no bet in this chapter had claimed
either way.

Whether the middle those worlds are converging on is
the right middle is a question about the scale constant and the response curves,
and it is open — but it is now *separable* from the structure, which it was not
before, because the gate and the scale used to be the same knob.

Two known defects are named and unstarted rather than fixed. Capacity reads each
cell's **mean** temperature, and by Jensen's inequality that misestimates any
nonlinear response — overestimating near the optimum, underestimating in the
tails, and the tails are where refugia live. And `per_species_capacity` computes
a **fundamental** niche (could this species live here alone) while the bake reads
it as a **realized** one (does this species live here); ecology has kept those
apart since Hutchinson, and competition exists downstream without ever feeding
back.

*Re-scored sideways again by [The Delvers](./chronicle/the-delvers.md)
(2026-08-07), which measured which layer of the prior actually does the
selecting — and then found that selecting is not the same as separating.* Three
results, in the order they arrive. First, **which axis binds is an authoring
choice, not a model constraint**, and it has a closed form. The condition
response floors its buffer-able axes at the sovereignty floor and passes
elevation a literal zero, so elevation is the limiting axis on every cell of
every world exactly when a kind's authored elevation devotion falls below that
floor — no terrain enters the derivation. *(That flooring structure is now
**pinned** rather than merely observed:
[The Axes](./chronicle/the-axes.md) added
`exactly_one_axis_is_unfloored_and_it_is_the_undercutter`, after finding that
the existing agreement test compares the fast path against a reference which
hardcodes the same structure — so flooring elevation in both leaves it green.
The closed form above is a statement about code that can change; it now fails
loudly if it does. The bet itself is unmoved.)* Confirmed in both directions over
three seeds: the two dwarves authored below their floors are elevation-bound on
100.00% of land, and the one authored above binds there on 8.64–31.59%. That
reproduces from arithmetic alone the earlier measurement that elevation binds
everywhere for goblin, gnoll and human, and reclassifies it: the climate axes
were silent because of how the roster had been written, not because the model
cannot hear them. Second, the same kind authored above its floor has its
temperature or moisture curve binding on **67–91% of land** while both
below-floor dwarves read exactly 0.00% on every climate axis — so an authored
climate niche demonstrably can select, which the paragraphs above had left as an
open question for a surface kind. Third, and the reason this is a sideways move
rather than a promotion: decomposing capacity into its two factors and
correlating the supply factor alone across kind pairs returns
**0.99935–0.99996 on all nine measurements** — over this family the supply term
is very nearly kind-independent, so *every scrap* of per-kind spatial structure
comes from the tolerance layer. That is a sharper statement than "an authored
tolerance can only modulate," and it is deliberately narrower than it sounds:
the correlation is scale-invariant, so it measures how supply **sorts** cells,
not how large it is, and the standing claim that supply's magnitude drowns the
niche is neither confirmed nor discharged by it.

**What that bought, and what it did not, is the finding.** The kind whose
climate niche actually binds is the one *least* separated from its neighbour —
capacity correlation 0.86–0.98 against the hill dwarf, above the frozen
threshold on two seeds of three, a refuted prediction pinned as a witness so a
later separation reddens rather than passing silently. Meanwhile the pair
differing in nothing but an elevation optimum, 150 m against 900 m, separates to
0.69–0.76. **Binding and differentiating are not the same property**, and
nothing in this chapter had distinguished them before; a niche can be read, be
correctly coupled, dominate the limiting product, and still leave two peoples
ecological synonyms. The prior's honest position is therefore that it can now
place a species where its traits say it belongs, and still cannot be relied on
to place two species *differently* on that basis.

One thing this campaign was expected to settle and did not: the question left
above — whether a realm's hard gate generalises past caves, "the first thing a
campaign placing a *people* underground will find out" — is **still open**. Two
subterranean dwarves were authored and then cut mid-campaign, because they had
been given a *low elevation above sea level* to mean *deep*, and depth below the
surface and height above the sea are different quantities: a chamber under a
mountain sits high, and the curve as written selected lowland marshes. The
roster that shipped is entirely of the surface. Placing a people underground now
waits on the underworld being declared as **places** — biomes, the way the sea's
depth layers already are — rather than as a coordinate pushed through a
tolerance curve.

*Re-scored twice by [The Range](./chronicle/the-range.md) (2026-08-08), once
against a claim this chapter already makes and once against The Delvers'
result. The two moves are independent and are kept apart deliberately.*

**First, and this is a confidence-lowering event about how a claim got here:
the realm sentence above was asserted before it was true.** *"An authored realm
selects, absolutely"* was a true statement about a **readout** and a false one
about the **world**. The gate reached `per_species_suitability`, whose only
production caller is a demography report — its own comment says the figure is
never serialized and never identity — while the function that decides where
settlements actually go took no realm parameter and applied no realm gate at
all. Declaring a peopled kind `Subterranean` moves that readout from 99.49% to
5.62% of land and moves the committed seed-42 world by **zero bytes**: the same
hash, the same 7,764 facts, the same flagship village, with three positive
controls run before the null was believed. The Range repaired the identity
path, so the sentence is true now, and nothing about the twenty-five-world
exclusion measurement was wrong — it measured a quantity no world reads.

What should lose confidence is the *procedure that produced the sentence*. The
measurement behind it is the one the peoples programme mandates, and it scored
the top rung of the programme's own probe-validity ladder, whose fourth and
highest rung reads "the readout differentiates the axis". A mechanism can be
authorable, read, correctly coupled and demonstrably differentiating, and still
never touch a world; until this campaign the programme had no rung in which
that sentence could be said. There is now a fifth — **reaches world identity**,
scored by perturbing the axis and asking whether the committed world changed.
Rung four remains necessary. It had simply been reading as sufficient.

**Second, and this sharpens The Delvers rather than reversing it: a preference
applied *outside* the limiting product both binds and differentiates.** The
Range added a biome affinity — a per-kind, per-biome multiplier applied beside
the realm mask rather than folded in as a fifth tolerance axis — and froze two
predictions before the rows existed. Both were confirmed. The arid share of
gnoll's settlements rose from 0.000 to 0.500, against a baseline captured by
running the test red on an empty registry, so a downward-only mask **relocates**
rather than merely thinning. And mean pairwise Pearson correlation between
gnoll's capacity field and the other peopled kinds' **fell on all three seeds a
previous campaign published** — 0.851 → 0.795, 0.790 → 0.706, 0.857 → 0.806,
with all twenty-four individual pairs down, the instrument cross-checked
against a previously published pair value to six places.

The honest reading is not that The Delvers was wrong. It is that the two
campaigns applied a preference at **different points in the same pipeline**.
The Delvers' climate niche sits *inside* the Liebig minimum, where it competes
with an elevation axis passed a literal zero rather than the sovereignty floor,
and a factor the minimum discards cannot separate anything. The Range's biome
affinity multiplies the product from outside, where it cannot be discarded. So
the candidate this chapter now holds is that **where in the pipeline a
preference is applied decides whether it can differentiate, not merely whether
it binds** — a structural property, not a fact about niches.

It is held as a candidate and not as a law, on two grounds. It rests on one
mechanism and two authored occupants. And the alternative reading survives the
evidence: the falls are real but modest, gnoll still correlating at 0.71–0.93
with most peoples afterwards, so the peoples may be so alike in their surviving
tolerances that no factor of this kind could pull them far apart. Pearson is
scale-invariant besides, so this measures how the fields **sort** cells, not how
large they are — the same scope limit The Delvers' own supply-factor correlation
carries.

**A third thing was measured and is deliberately left without a cause.** One
authored row redistributed the entire placement. On seed 7 the total moved 274
→ 287 while gnoll — the kind whose affinity was declared — stayed at 4 and
bugbear went 49 → 153. The attribution is proven rather than argued: the
campaign's second occupant is fauna, chosen so that exactly one *peopled* kind's
placement could move, and a test rebuilds three seeds with and without its row
and asserts the complete list of (people, cell) placements is identical. So
every movement is gnoll's row alone. **Why one row moves everyone is not
established, and no mechanism is narrated here.** What the chapter should carry
is the magnitude: authoring an ecological preference for one kind is not a local
edit to that kind, and a future occupant should expect to move every people's
numbers.

*Re-scored up by [The Radiation](./chronicle/the-radiation.md) (2026-08-10),
which found that the evidence above was measured through an undeviced constant.
The bet was right; the number that scored it understated it.*

**The relocation half is confirmed far more strongly than it was scored, and the
caveat attached to it does not survive.** The affinity row's fallback level — the
factor a kind still takes on ground that is not its country — had never been
derived from anything; it entered the code as illustrative example text and was
adopted as a constant. Derived instead from the kind's own sovereignty floor, so
that a row states a *shape* and never a *level*, the same gnoll row that had been
read as *20 settlements to 2 at an arid share of 0.500* reads **13 settlements to
40 at a share of 0.825**. The count *rises* while the share rises. The caveat
built on the old arm — nine settlements removed for every one relocated, a
downward-only mask that suppresses rather than moves — was a property of the
constant and not of the mask, and it is corrected in place in
[The Range's own chapter](./chronicle/the-range.md). The two descriptive seeds
move the same way (seed 7: 67 → 31 at 0.645; seed 1234: 7 → 6 at 1.000).

**What should lose confidence is a claim this chapter's tier is built to catch:
a justification true of one consumer, silent about the rest.** The level was
defended as *gauge* — a uniform factor cannot reorder a kind's own ranking of
cells, which is true, and was the only property anyone checked. The same factor
multiplies the capacity that becomes a settlement's population, and the history
bake's volume is a function of population, so the level is gauge for one consumer
and load-bearing for the next one downstream. Measured on two arms differing in
nothing but the level, a uniform mask on **one kind of thirty-nine** removes
13.7% of a world's facts. Six rows at the old level took seed 42's history from
552 occupation records to 193 and breached four fidelity floors; derived, the same
six give 704, above the 552 measured with no such rows at all. The general form is
worth carrying past this bet: **a claim that is true and incomplete is more
durable than one that is false, because nothing contradicts it.**

**And the third measurement above — one row redistributing everyone's placement,
deliberately left without a cause — now has half a cause.** Not the row's shape:
its *level*, multiplying outside the limiting product, scaling every occupant's
capacity and therefore every settlement's population and every history the bake
grows from it. That is why one row moved a world. It does not explain the whole
magnitude, and the chapter still holds the standing warning: an ecological
preference authored for one kind is not a local edit to that kind.

**The subterranean question this bet left open is answered.** The Delvers
withdrew two underground dwarves and the section above recorded that placing a
people underground waited on the underworld being declared as *places*. It does
not, for one kind. [The Radiation](./chronicle/the-radiation.md)'s drow settles
exclusively at cave mouths — the share of its settlements on a cell holding an
enterable cave is exactly `1.000000` on all three tested seeds, with no allowlist
— and a five-arm factorial attributes about 94% of its separation from a surface
sibling to the realm gate alone, with the closure arm bit-identical, so no
unenumerated third difference exists. What is **not** answered is the question the
withdrawal was actually about: distinguishing *two* underground kinds by depth.
Drow needs only to differ from surface elves. Mountain-dwarf and Duergar differ
from each other by stratum alone, and the biome vocabulary still has no
subterranean variant, so they remain owed.

**A finding about the contest, not about elves, and it is new.** Two peoples
authored to share a mass and an affinity row have capacity fields that are
**bit-identical** over eleven to nineteen thousand land cells — and they settle on
wholly *disjoint* sets of cells, in different numbers, on every seed. The bake is
not a pure function of the capacity field; iteration order, tie-breaks, migration
and the raid comparison all participate. This chapter has assumed the field
decides placement wherever it reasons about placement at all. It constrains
placement; it does not determine it, and that is now measured with the field held
constant to the bit.

*Re-scored again by [The Muster](./chronicle/the-muster.md) (2026-08-12), which
found that the one consumer still believed exempt is not exempt either.*

**The exemption above does not survive, and the correction sharpens the general
form rather than weakening it.** The paragraph two above concedes that the level
is gauge for *one* consumer — a kind's own ranking of cells — and load-bearing
downstream. The concession was too generous. A change to the level is not a
uniform factor at all: the constructor holds a stronghold at exactly `1.00`
while pulling every lower rung down, so it changes the ladder's **contrast**
rather than its scale, and the factor then reweights biome against every other
condition inside the per-cell limiting product. Measured on seed 42 with every
authored shape held fixed, **all seven kinds carrying a shaped row have their
own cell ranking changed**, and one kind's argmax — the cell the placement
routine would choose as a stronghold — moves outright, with 5 of its top 50
cells surviving. **All eleven row-less kinds are bit-identical**, which is the
control: with no row the factor is `1.0` at every level, and there the level
genuinely is gauge.

So two claims had been wearing one sentence. *A uniform rescale of a whole row
cannot reorder that kind's own ranking* is true and is what this chapter says.
*Changing the level preserves ranking* is false, because the constructor is not
a uniform rescale. For a kind with a shaped row the level is load-bearing in all
four of its consumers and gauge in none. The general form above — a claim true
and incomplete is more durable than one that is false — now has a checkable
successor, ratified as a decision: **when a quantity is described as gauge, name
the transformation it is gauge under, and name the consumers checked.** The
short form named neither, which is exactly how it stayed unfalsified across two
campaigns while being wrong about four consumers out of four.

**And the split this bet's successor asked for is refuted, by its own
instrument.** The open question left standing was whether the level should
become two numbers, one per consumer. A preregistered sweep, its rule frozen
before any measurement, says no — with the caution that the *first* arm said
yes. Varying the level globally collapses the shipped per-kind spread to one
scalar and manufactures the opposition it then reports, because at a scale
factor of one a uniform level simply **is** the no-affinity world. Scaling each
kind's shipped level instead — ordering preserved, and the shipped
configuration reproducing byte-identically at a scale factor of one — satisfies
every band simultaneously with the shipped values interior to the satisfied set.
One quantity, correctly valued. The transferable half is a control, not a
result: **for any one-scalar sweep over a per-kind quantity, ask whether the
shipped configuration reproduces byte-identically somewhere on the grid; if it
cannot, the sweep is not interpolating the shipped world.**

**The phenomena interface generalizes.** The bet that one salience-ranked
observation interface could serve religion, perception, and historiography
without any consumer learning which system produced a phenomenon has held
across every domain that has tested it. One caveat corrects the original
forecast: *room description* was expected to ride the phenomena channel too,
and instead took a cleaner road — the semantic query surface, where the sim
emits quantities and the client renders them ([The Scene
Window](./chronicle/21-the-scene-window.md)). The interface is more general
than feared; it is also not the only interface, and that turned out to be the
right shape. The bet has now been confirmed on the *producer* side as well:
[The Elements](./chronicle/the-elements.md) added a wholly new source class —
climate's felt weather — through a `Domain`-trait roster that lets any domain
contribute observations without editing the composition root or a sibling, so
the stream is no longer sky-bound and religion can grow weather-gods where the
land is harsh.

The bet has now also been tested on the channel's *payload*, and the original
shape was wrong in one respect. A phenomenon carried a `description` string
alongside its salience, and the forecast treated that as harmless — prose the
consumer could ignore. It was not harmless, for a reason the interface's own
design implies: an observer context deliberately carries no species, so a
producer cannot know who is looking, and a stored sentence could only ever be
culture-neutral or wrong. The field's *type* guaranteed a leak that no amount of
producer discipline could close. [The Vernacular](./chronicle/the-vernacular.md)
deleted it and moved rendering to the windows, where a speaker is known — and
found the string had been serving as a **sort key**, so its removal reordered
tied phenomena and, through a positional join, moved two deities' periods. The
test written to prove the description was not load-bearing had compared the
gloss *after* the ordering ran, and so had never looked at order at all.

What the bet gets right is confirmed and sharpened: the channel generalizes
because it carries *what was observed*, not *how to say it*. What it got wrong
was assuming a description could ride along inertly. It could not, and the
correction is that a phenomenon now carries a referent and no text. One
qualification stands unresolved: `SkyReport` and `ClimateReport` still carry
domain-resident prose of the same shape, so the guarantee is currently true of
the phenomena channel rather than of the simulation.

The scene seam has since crossed a repository boundary: an
external client now consumes the same documents through a versioned wasm
catalog, byte-identical across platforms
([Goldengrove](./chronicle/goldengrove.md), decision 0055). It has also
started carrying *parameterized* quantities the client evaluates over time —
per-tile temperature elements a viewer reconstructs across the year, with
the seasonal evaluator documented normatively in one place and pinned by a
producer-sourced contract test on both sides of the boundary
([The Isotherm](./chronicle/the-isotherm.md)). The seam holds not just for
static quantities but for the small closed-form functions of them, which is
the more demanding form of the same bet — and
[The Wandering Sun](./chronicle/the-wandering-sun.md) added a second such
function (a locked world's librating-substellar temperature) and, in doing
so, sharpened what "the same function on both sides" requires: not just the
same formula but the same *point of evaluation*. Every earlier cross-seam
value the client read pre-computed off a scene layer, so the producer's
nearest-cell snapping was baked in and invisible; the first value the client
*recomputed* from position diverged from the golden by up to a degree,
because it evaluated at the tile centre while the golden had snapped to a
mesh cell. A closed-form function crosses the seam faithfully only when both
sides sample it at the same coordinate — the fix was a position-based
producer evaluator sampled at the tile centres the client uses, and the
lesson is that the golden must pin the client's computation, geometry
included, not merely its arithmetic. A refinement arrived from the
consumer's side, and it sharpens what the bet does *not* buy: **the seam
holding is not the same as the data being drawn.** A rendering debt had
accumulated silently across three producer campaigns — four layers shipped
and parsed and, in the sharpest case, fully evaluated, with
`circulation_bands` feeding a tested, normatively specified wind evaluator
that rendered no pixels at all. The seam was working perfectly while a
quarter of what crossed it went unseen. That the sim emits a quantity a
client faithfully receives says nothing about whether anyone ever looks at
it, and the discipline this bet still lacks is the check — at a producer
campaign's close, not a campaign later — that some consumer draws what was
just shipped ([The Lens](./chronicle/the-lens.md)).

The mirror image of that caveat arrived next, and it closes the pair: a
consumer can also draw a distinction the producer never made. *The Faces*
shipped `scene/moons/v1` with `bright-icy` as a surface class, selected off a
hash-derived albedo — the client rendering the **word** for an icy moon while
the model held no concept of ice, and deriving every moon's radius from an
**assumed** constant lunar density because composition was never drawn. *The
Reckoning* then drew composition for real, and for the length of one campaign
the repository held **two answers for one quantity**: an icy captured body is
~28% larger at its true density than the contract reported. Unified rather
than deferred ([The Reckoning](./chronicle/the-reckoning.md)). So the bet's
honest statement now has two failure modes on the same axis, and neither is a
seam failure: **the seam holding says nothing about whether the data is drawn
(The Lens), and nothing about whether the data is grounded (The Reckoning).**
A schema is a contract about *shape*, and both campaigns found that shape is
the easy half — a field can be faithfully transported, correctly parsed,
beautifully rendered, and still refer to nothing. The check this bet lacks is
therefore larger than The Lens made it look: not only *does some consumer draw
this?* but *does the producer actually know what it is asserting?*

[The Shadow Track](./chronicle/the-shadow-track.md) took the interface across
a fifth layer — `scene/eclipses/v1`, a *parameterized temporal* query in the
shape of tiles-region: a client asks for a day window and receives that
world's dated eclipses with their solar ground tracks. Two of the bet's open
disciplines got exercised rather than merely restated. The Lens's "does some
consumer draw this?" check was run at the *producer* campaign's own close, and
it earned its keep: the shadow band shipped, parsed, and unit-passed, yet
rendered nothing a viewer could see — it sat at a radius just above the sphere,
beneath the globe's sixty-times-exaggerated mountains, occluded from every
camera angle. jsdom and the geometry unit tests could not see an occlusion; a
screenshot could, and the fix (lift the band above the tallest exaggerated
peak) is a change no non-visual gate would have prompted. And the golden
discipline sharpened in the other direction: the campaign's plan mandated a
committed producer-sourced golden by reflex, but the whole-branch review found
it pinned nothing and contradicted the client's own documented convention. The
distinction the earlier campaigns had blurred is that a golden is the right
instrument only for a value the client *recomputes* (the climate and ephemeris
re-derivations); for a scene document the client merely *parses*, the
end-to-end fixture that reads the real wasm binary **is** the contract, and a
second committed copy only adds a thing to drift. The seam generalizes; the
check that it is *seen* is now practiced at the source; and the golden is
calibrated to the one case that needs it.

[The Turning](./chronicle/the-turning.md) sharpened the *seen* check one turn
further, and in the harder direction. Its diurnal temperature crosses the seam
as another recomputed closed-form function (the client reads a per-cell
amplitude and re-derives the waveform, golden-pinned at the tile centre it
evaluates) — but the finding was about review, not transport. The waveform
shipped physically wrong: it keyed the day/night phase to the *global* fraction
of the day, so the whole planet pulsed in unison instead of a warm band
sweeping per longitude. It passed the Task-1 implementer, its per-task reviewer
(who verified the formula matched the brief *line by line* — and it did), and
three further reviews, because **every one of them checked the code against the
spec, and the spec's formula was itself the error.** Only the producer
campaign's own visual pass — the globe run forward, the lens pulsing the entire
hemisphere at once — caught it. So the check the bet still lacks is larger than
"does some consumer draw this?": it is *does the drawn thing look like the
phenomenon?* A formula can be internally consistent, faithfully transported,
correctly parsed, and **physically wrong**, and no review that treats the
specification as ground truth will see it. The visual pass is the only reviewer
that checks the model against reality rather than against the plan — which makes
it, for physical fields, not a courtesy at the end but the gate that closes the
loop.

[The Gyre](./chronicle/the-gyre.md) carried the seam across a boundary it had
not yet crossed: the first **vector** field (an ocean current, two tangent
components per tile, where every prior layer was a scalar) and the first the
client does not merely colour but **advects** — particles swept along the field
as motion, the living globe's first real animation over a
deterministic keyframe. The interface held: the client's tangent frame is the
exact inverse of the producer's, so the flow points where the sim says. And the
*seen* check earned its keep a second time, in a gentler register — not a
physics error this time but a legibility one: the field shipped correct and
nearly invisible (faint one-pixel specks), and only the screenshot showed that
"transported and parsed" is still not "read." The bet's honest statement now
carries three failure modes on one axis — the data can go undrawn (The Lens),
ungrounded (The Reckoning), or drawn-but-illegible (The Gyre) — none a seam
failure, all invisible to everything but a human looking at the picture.

[The Selvage](./chronicle/the-selvage.md) added a fourth, and it is the one
that reaches furthest back toward the producer. A map tile's samples crossed
the seam correctly, were parsed correctly, and were drawn correctly — and the
client still assembled two adjacent tiles at the wrong edges, because the
*geometric convention* that makes them assemblable appears nowhere in the
document they arrive in. The producer walks a parameter across a tile to lay
out its rows, and that same parameter counts the tiles; so a tile's row axis
and the tile grid's own axis must run the same way, and a tile's last row of
samples is bit-identically its neighbour's first. Every word of that is true
of the contract and none of it is *in* the contract: the client had to
re-derive it from the producer's source, got the sign backwards on one axis,
and produced a discontinuity that could not exist on the real planet. So the
data can also go **drawn-but-mis-assembled** — each document faithful, the
composition of two documents wrong. The bet is unharmed (nothing crossed the
seam incorrectly) but its scope is now clearer: a versioned scene document
carries values and says nothing about the geometry that relates one document
to the next, and a consumer holding several at once is re-deriving that
relationship whether or not anyone wrote it down. The check this suggests is
cheap and not yet practiced — a contract that ships more than one tile should
state its own adjacency convention, rather than leaving each consumer to
reconstruct it from the generator.

[The Snapshot](./chronicle/the-snapshot.md) moved the seam onto an axis it had
never been tested on. Every layer before it was a **query**: a client asks the
world about a place or a day window and receives a document describing what is
there. The session document (`vessel/session/v2` today, `v1` when The Snapshot
shipped it) is an **emit** — one document per committed turn of
an interactive session, and not a view *of* the world but a view *from* an
agent inside it, which means the interesting part is what it must withhold. It
carries the redaction boundary in its own shape: channels grouped by how the
agent came to hold what it holds, rather than fields tagged with provenance, so
a pane that reads one channel does not decline to look outside it but cannot.
The seam held on the new axis, and was proved the honest way — by moving the
one pane that already existed off the prose interface and onto the document,
where it printed the same bytes it printed before, rather than by adding a
second pane that nothing could contradict.

The campaign also refines the golden rule [The Shadow
Track](./chronicle/the-shadow-track.md) had just sharpened, and the tension is
worth stating rather than smoothing: that campaign concluded a committed golden
is the right instrument only for a value the client *recomputes*, and this one
committed a golden for a document its client merely *parses*. The distinction
that survives both is not who reads the artifact but what the artifact pins.
The Shadow Track's dead golden pinned a *client contract* the end-to-end wasm
fixture already pinned better. This one pins something no client-side test can
reach: the session document is declared save-format-class, so a change in what
its bytes *mean* is an epoch event, and the committed fixture is the tripwire
that makes such a change arrive as a reviewable diff instead of arriving
silently — its own failure message names the epoch decision. An in-process
determinism test catches nondeterminism; only a golden held across code changes
catches a meaning that moved. It earned its keep on the day it was written, by
exposing a negative zero that had been folded into every unprovoked NPC's
grievance for months and was invisible to the equality test that guarded it.
And it carries a tie no end-to-end fixture could express: the newest channel in
the newest schema asserted byte-identical against the *oldest* committed golden
in the book, the published seed-42 possession transcript. The end-to-end check
exists too — the wasm smoke driver asserts the schema tag, every channel, and
narration equality against the real binary — so the campaign holds two
instruments rather than one duplicated. The cost The Shadow Track warned of was
still paid: the fixture needed a rebaseline during the final fix wave, and any
second copy of a document must be regenerated whenever the document
deliberately changes, with a rubber-stamped regeneration as the standing
hazard. Here the regeneration *was* the instrument working, and what forced it
belongs to this bet's own tally. A schema is a contract about shape, and shape
does not include **range**: the agent id was faithfully declared, faithfully
transported, faithfully parsed, and quietly truncated on arrival, because
JSON's one numeric type is a float and the producer's identifier is a
full-width 64-bit integer — wrong by 296 for seed 42, and wrong for nearly
every world. It printed correctly, which is why it survived three separate
per-task reviews: the integer, the WebAssembly boundary, and the TypeScript
annotation each sat in a different file, and no single-file review holds two at
once. The three failure modes above are invisible to everything but a human
looking at the picture; this fourth one is invisible to anything smaller than
the whole seam — and unlike them it is *mechanizable*, since nothing in the
ladder yet checks that an integer crossing into JSON fits a double. So the rule
the two campaigns jointly support: **a golden for a value the client
recomputes, or for a save-format-class document whose meaning changes are epoch
events; the end-to-end fixture for a document the client merely parses.** Where
neither holds, a second committed copy is only a thing to drift.

**Re-scored by [The Panes](./chronicle/the-panes.md) (2026-08-06): the bet
The Snapshot deliberately declined to take has now been taken.** That campaign
proved the emit seam by moving an *existing* pane onto the document, and said
plainly why — a second pane that nothing could contradict would have proved
nothing. The Panes added the second pane. It is the harder direction, because
the redaction boundary is only as real as the first consumer that could have
violated it and did not: a map pane is precisely the pane most tempted to
reach outside its channel for world truth, and the shape of the schema is what
stops it. The channel carries **semantic content, never a picture** — cells,
not glyphs — so the sim never learns how anything is drawn, and the client
renders from one document rather than from two sources that could disagree.
Both panes are now pure functions of one snapshot, which is the structural
form of the claim rather than a discipline anyone must keep.

Two things sharpen the score rather than merely confirming it. First, the cost
was **measured, not asserted**: a session-level benchmark this campaign built
prices the emit at 1.249 ms against a 0.173 ms baseline, and the payload
growth is band-dependent in a way a single figure hides — 2.73× out of doors,
1.17× indoors. The bet's premise is that the emit is cheap enough to pay every
turn; that is now a number rather than an expectation, and it paid down a
re-measurement another campaign had left owed. Second, the bet's *weakest*
seam showed itself at the merge, not during the work. A tagged union over
bands is an enumeration of another part of the sim's state space, and a
parallel campaign added a band to that space while this one ran. The merge was
textually clean; nothing in either campaign's documents mentions the other's
surface; the two agreed only because both happened to guard on the same
condition. **The generalisable lesson is that an emit whose shape mirrors
sim state inherits that state's growth, and no gate asks whether the mirror is
still total.** The seam held, and it held for a reason no test had stated —
which is the kind of pass worth recording as a narrower confidence, not a
wider one.

**Re-scored again by [The Sighting](./chronicle/the-sighting.md) (2026-08-07):
the redaction boundary stopped being merely structural and started
withholding.** Both prior tests proved the boundary by building panes that
*could* have reached outside their channel and did not; nothing had yet
required the sim to remove something a pane would otherwise have shown. This
campaign does, and the score improves for a reason that is not the one the bet
anticipated. The withholding turned out to be far harder to make **total** than
to make correct: the narrowing predicate was right in its first commit and
still leaked four times, through `examine`, a needs report, a provoke line, and
a tick's motion narration. Every leak was a surface that *narrated* a creature
rather than one that *returned* one, so none of them appeared in any
enumeration of the channel's readers. The generalisable form is that
**introducing an invariant silently promotes every existing reader of the
underlying data into a potential violation of it, and nothing in the repository
enumerates that set** — the emit seam's own shape does not help, because the
last mile of every channel is prose, and prose cannot be audited for what it
happens to mention. The structural claim survives and is now load-bearing; what
narrows is the confidence that a structurally-correct boundary is
automatically an *observed* one.

Two smaller corrections the same campaign forces on this chapter's arithmetic.
The bet's cost premise has been priced through the **actual** wasm boundary for
the first time — a turn measures 1.57–1.78× native, not the 3.6–3.8× every
derived browser figure here was multiplied by, so the seam is roughly twice as
cheap in the browser as this chapter had assumed. And the payload growth this
campaign added is eleven bytes, against a per-turn derivation cost of about
3.7 ms in release: the bytes were never the term worth watching, the derivation
is.

Every test of this bet so far has pushed on the *producer* side — new source
classes, new layers, new document shapes. [The Vigil](./chronicle/the-vigil.md)
pushed on the **observer** side instead, and the interface took it without
modification. The observation machinery — the lens, the characteristic hour,
the salience ranking — was authored for settling peoples, every one of which
has a place, a society, and neighbours. A dragon has none of these: it never
settles, so the exposure classifier that feeds it finds no biome, no
neighbouring kind, no hearth. It nonetheless observes through exactly the same
path, and the ranking it gets back is visibly its own — lunar eclipses outrank
the sun for a crepuscular creature with a dark-adapted eye. That the consumer
never learns which system produced a phenomenon is the half of this bet that
was already well tested; that the *observer* need not be a member of a society
for the interface to describe it is the half that had never been exercised,
because until now every observer was.

[The Purview](./chronicle/the-purview.md) added the sixth scene kind and the
first **egocentric** one — and with it the first document that carries an
*epistemic* field, since a situated scene describes what an observer knows and
not merely what is there. The structural news is not the schema, though, but
the scheduling: this is the first layer where the producer and a consumer that
draws it shipped in the **same campaign**, which made the "does some consumer
draw this?" check the bet has been asking for since The Lens not a discipline
to remember but one that could not be deferred. It paid immediately and in the
direction nobody was watching. The chart resolved a biome by matching the
climate domain's kebab-case name against the locale window's spaced one, and
every multi-word biome had been quietly resolving to index zero — on seed 42
all thirty-one cells reported *ice* for a tropical seasonal forest. Single-word
biomes matched by coincidence, which is why months of green tests had said
nothing. That is a **producer-side** error in a seam that had already shipped,
found only because something finally drew it; the repair was to compare enums
rather than strings, deleting the round-trip that was the defect class. The
visual pass earned its keep a second time in the same campaign, on the render
rather than the data: the chart passed every assertion while drawing a leaning
parallelogram, because the screen projection did not cancel the lattice's row
offset. So the tally of ways a faithful seam still fails gains a sixth, and it
is the mildest-sounding and the most general — **the picture can misstate the
geometry of a document that is entirely correct**, and no test written against
the document can see it, because the document is not what is wrong.

The bet itself is unmoved and, if anything, better supported: six kinds across
cartographic, temporal, orrery, session-emit, and now situated poles, none of
which required the interface to change. What keeps sharpening is the ledger of
things the seam holding does *not* buy — and the one lesson that now recurs
often enough to be a rule rather than an anecdote is that every entry on that
ledger was found by a human looking at output, never by a test.

[The Occlusion](./chronicle/the-occlusion.md) confirms that rule from outside
the scene-document program entirely, and in the plainest register available:
its four defects were found not in a picture but in **prose**, by building the
CLI and reading what it said. The almanac had been opening for most of the
project's life by naming five stars beneath a flat overcast — a sentence that
contradicts itself inside its own span — and `possess` printed `Ways on: SE,
N, SW.` and then answered `No verb 'se'`. Neither is visible in a diff, because
in both cases each half is correct in isolation: the compass parser accepted
the token and the dispatch arm that reaches it was simply absent; the weather
was computed correctly and appended after the sky was already described. A
codebase with zero TODOs across a hundred thousand lines, a default-deny type
audit, and a 2,319-test gate said nothing about either. So the visual pass
generalizes to a **legibility pass**: for a project whose deliverable is prose
about a world, reading the output is a distinct instrument from testing it, and
the same one that catches a leaning parallelogram catches a sky that argues
with itself.

The campaign also put a sharper edge on what "verified" buys. Its spec claimed
the change could not reach the save format, and *checked* that claim — the sky
report carries no serializer, confirmed by reading the derive rather than
assuming it. The check was sound and the conclusion was false, because the
exposure did not run through serialization but through genesis, where a
people's gods are derived from the sky they observe. Wiring occlusion into the
observation path cost seed 42 twenty-three of its forty-eight deities while
every gate stayed green, since the gate pins facts against the current build
and not against history. What caught it was the cheapest possible instrument,
and one no schema discipline implies: build the world before and after, and
compare the bytes. The ledger's entries were all *found by looking*; this one
adds that a determinism claim is only as good as the route it was checked
along, and that the total check — same seed, both binaries, `cmp` — costs
ninety seconds and subsumes the clever ones.

[The Sextant](./chronicle/the-sextant.md) adds a seventh entry to that ledger,
and it is the first one an instrument found rather than a human looking. A
scene document can be faithful, grounded, drawn, legible, and correctly
assembled with its neighbours — and still be **ruinously expensive to ask
for**, because a versioned schema is a contract about a document's *contents*
and says nothing about the cost of producing one. Every terrain-facing entry
point in the scene window re-derived terrain and climate from the world and
kept neither, so each such document carried about 638 ms of fixed setup;
measured against the
Orrery's real call pattern, which requests one regional document per
level-of-detail tile, **91.6% of a scene call was the planet being rebuilt**
and a single camera move spent roughly fifteen seconds generating the same
world two dozen times. ([The Cistern](./chronicle/the-cistern.md) closed that
the following day — the derivation now happens once per world, and a region
patch measured 11.1× cheaper. The entry stays on the ledger because the
*failure mode* is what it records, not the defect's lifetime. [The
Winnowing](./chronicle/the-winnowing.md) then took the residual The Cistern
named: the globe document's cost is no longer redundant *derivation* but sheer
*volume*, and the schema said nothing about that either. A caller may now name
the per-tile layers it will read — the eight the Orrery's parser actually
extracts are 46.3% of the bytes — which is the same lesson one turn on. A
contract about contents says nothing about the cost of producing a document,
and it says nothing about which parts of one a consumer will use.) The bet is
untouched — nothing crosses the seam incorrectly,
and the interface required no change to be measured. What sharpens is the
same scope lesson The Selvage drew about geometry, transposed to cost: a
document describes itself and not its relationship to the *other* documents a
consumer holds, and a consumer's calling pattern is exactly such a
relationship. So this entry is also the ledger's counter-example to its own
recurring rule. Six failure modes were found by a human reading output; this
one is invisible to reading — every document is correct — and visible only to
a fixture shaped like the consumer's session, since redundancy is a property
of a sequence of calls and cannot appear in any one of them.

[The Pyx](./chronicle/the-pyx.md) adds an eighth entry, and it generalizes the
ledger's own recurring rule one step further. The Cartographer's lesson was
that a determinism claim is only as good as the *route* it was checked along.
The Pyx's is that it is only as good as the **apparatus** it was checked on:
every check in this repository regenerates on the canonical box and compares
against a golden authored on the canonical box, so the machine sits on both
sides of the comparison and cannot be what the comparison detects. In the
vocabulary metrology uses for exactly this distinction, the project had been
enforcing *repeatability* and describing it as *reproducibility*. The audit
that closed the gap found nothing wrong — a full census reproduced on its
authoring host eleven days later with zero bytes different, two clean builds
of one commit hashed identically, and a forty-world all-metric probe was
byte-identical between x86_64/Linux and aarch64/Darwin, including the one
seed whose count decision 0063 had recorded two
machines disagreeing on. The bet on deterministic serialization is
**strengthened, and for the first time by evidence from outside the machine
that authors the goldens**. What sharpens is the scoring instrument rather
than the claim: the cheapest sufficient check turned out to be a comparison of
*binaries* rather than of outputs, which nobody had tried and which the
campaign's own frozen prediction said would not work.

That instrument then needed a correction of its own, and the correction
belongs on this ledger as much as the entry does. Two builds of one commit in
two directories hashed identically on the canonical box, and the campaign
generalized from that single host to a property of the toolchain. Repeating
the comparison on the second machine produced two *different* binaries, each
carrying the absolute path it was built in — a path written in deliberately by
ordinary code asking where its own source tree is, not by debug information as
first supposed. So the oracle is real but conditional: it holds when both
machines build at the same absolute path, which an image supplies for free and
an ad-hoc checkout does not. The entry's shape is therefore the ledger's rule
turned on the ledger's own author — a claim verified on one apparatus is a
fact about that apparatus, and the campaign that had just finished saying so
in prose went on to forget it in a decision record within the hour.

[The Twin](./chronicle/the-twin.md) closes that correction and sharpens the
bet itself. Holding the compiler, the system library, and the build directory
fixed, two machines that share almost nothing else — different processors,
kernels, and operating systems, one of them an appliance that cannot be logged
into — produced the same binary to the byte; and two *different* system
libraries produced the same forty worlds, even compiled the old way, where the
operation everyone suspected leaves the program and enters the library. So the
long-standing explanation for the one recorded cross-machine disagreement is
eliminated, and with it the machine itself. **What determines the output is
the environment, not the host** — which is the strongest form this bet has
been stated in, and the first version of it supported by a comparison in which
only one thing varied. Every earlier cross-machine check in this project moved
five things at once, which is why the disagreement of nineteen July was
observable for four months and diagnosable for none of them. It is still
unexplained. The space it can hide in is now small enough to name: how a build
chooses its compiler, given that this project's pin is silently conditional on
the directory you invoke it from.

**Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): one
entry on that ledger is now mechanized, and the move that mechanized it is
worth more than the check.** The entries above are all forms of *the drawn thing
does not match the thing* — undrawn, ungrounded, illegible, mis-assembled,
misstated geometry — and the standing lament is that only a human noticed. The
Blocking's parity contract turns one sub-class into a test: **every noun the
render depicts must answer to `examine`, and every destination it depicts must
be reachable by a named command.** That is precisely the class of defect that had
shipped one campaign earlier, where `look` named a water jar and `examine`
denied it, and it is checkable because the render and the command language are
required to derive from *one* model rather than to agree by vigilance. The
structural half is what makes the tested half possible: a pane input
**synthesizes a command** — an arrow key emits `go n` and the existing verb runs
— so there is one implementation and no second path to drift from. The accepted
cost is permanent and is the reason this is a bet moving rather than a feature
landing: any future pane capability must first be a verb, so nothing will ever be
expressible only by pointing.

The honest scope: this does not close the ledger, it converts one row. A plan
whose every glyph answers can still be *ugly*, and legibility remains
taste-checked — the campaign's own render had to be reworked once because a model
that was faithful drew no walls at all, which no assertion caught and a human
reading the picture did. What changed is that "the render depicts something the
command language denies" has stopped being a thing a human must remember to look
for.

**Re-scored again by [The Handle](./chronicle/the-handle.md) (2026-08-06), which
refutes that last sentence and narrows the row.** The parity contract was real
and it held — for the floor plan it was written against. It said nothing about
the other four surfaces that name things: the room's own prose, the sky, the
chart legend, and the underworld. All four were denying nouns they had just
printed, and the way it came to light was a human reading a transcript and
saying so, which is exactly what the sentence claimed had stopped being
necessary. Six of the seven significant words in the starting room's catalog did
not resolve.

The correction is about the *scope of a mechanization*, not its value. A check
converts the row it covers and leaves the rest of the ledger reading as though
it were covered too — which is the more dangerous state, because the lament that
"only a human noticed" gets quietly retired while remaining true everywhere the
check does not reach. The Handle widens the check from one render to every
catalog surface, and it also finds the limit of the wider version: the check
asserts that *declared* nouns resolve, and is structurally blind to
over-admission, which a mutation demonstrated and only a separate,
opposite-facing test caught. So the row now reads: parity is mechanized in both
the plan and the prose, in one direction, and the second direction is held by
regression tests rather than by a rule.

**Re-scored by [The Quire](./chronicle/the-quire.md) (2026-08-09): the seam
gets its first consumer that structurally *cannot* cheat, and its first
finding of incompleteness rather than of error.** Every earlier test of this
bet was in-tree. Both browser panes are compiled beside the sim; the wasm shim
links it. Their discipline was real and it was a discipline. The Quire's render
crate is outside the cargo workspace and has **no dependency on the simulation
at all**, so the methods a client must not reach — whether a creature would
turn hostile, its grievance against you, the knowledge store — are not
discouraged but absent: there is no symbol to reach. The same move applies one
level down, where the client's hand-written mirror of the schema simply does
not declare the channel the schema itself warns is world truth, so the
forbidden pane cannot be built rather than must not be. This is the first time
the redaction boundary has been enforced by what was *not written down*, and it
is a stronger form of the claim than any prior test produced.

What that strength bought was a negative result, which is the news. A client
that renders only the emitted contract is standing evidence that the contract
is renderable — and this one found a place where it is not. The session's
spatial channel splits `walk` from `chamber`, where `walk` means *not inside a
built structure*; being submerged and being underground both fold into it.
Underground, a pane drawing the exits from the document drew the surface's
exits, while the prose in the same document said *out*. Nothing on the wire
distinguishes the two states except the literal word inside the prose, which no
consumer may parse. So the ledger above gains an entry of a kind it did not
have: not the drawn thing failing to match the thing, but the **document
correctly describing less than a consumer needs, with no defect anywhere to
point at**. Its resolution is instructive too — the pane was deleted, because
the client never needed the exits: command parsing lives in the sim, the key
mapping sends its verb unconditionally, and an invalid move is answered with a
sentence. A consumer's requirement for a channel should be checked before the
channel is designed.

One older row recurs and one instrument for it becomes mechanizable. The
recurrence: the outdoor chart was geometrically wrong — the sim's own render of
the identical thirty-one cells is five dense rows and the client drew nine
sparse sheared ones — under seventeen green tests and four mutation proofs,
which is The Purview's leaning parallelogram a second time in a different
codebase. Twice is a structural property of picture-versus-property, not an
anecdote. The instrument: the repair pinned the client's projection against the
sim's own ASCII rendering of the same document, byte for byte, and the indoor
plate was pinned the same way pre-emptively and passed. Where the repository
already contains a second, independent renderer of a document, "does the
picture state the document's geometry" stops being taste and becomes a
comparison — which is the first time a row on this ledger has been converted by
something other than a human remembering to look.

**Terrain shape has Earth-anchored, self-checking acceptance bands, and the
one that stayed open resolved by superseding its own instrument rather than
closing under it.** The Measured Coast preregistered six Earth-anchored
shape metrics (shoreline development, hypsometric bimodality, shelf
fraction, continent count, largest-continent share, plate-size Gini) before
any generator change, exactly the kind of bet this chapter cares about: the
Laboratory generates the evidence, measures it, and drift-checks the number,
with no human judgment call about whether a continent "looks right." Crust's
epoch closed four of six by direct measurement, refuting two of its own
predictions along the way (the tanh-lobing pinch-off hypothesis, the craton-
repulsion hypothesis) before a read-only probe found the actual cause
(sea level sitting in the abyssal plain). Sculpting's epoch closed a fifth —
shelf-fraction, via a wave-cut coastal-erosion mechanism the tuning season
built only once measurement showed the band demanded it — and left the
sixth, shoreline-development, on an honest open verdict: every mechanism the
spec banked for it is now built, the metric moved a real +8% under
measurement, and it still sat below its floor. A dedicated diagnostic
instrument established *why* the floor was hard to reach — the estimator is
not saturated, but the floor's own anchor was partly built on coastline
noise a since-removed generator had produced — and handed the open band
forward, with its evidence, to a named future campaign.

That campaign, rift-and-fit, resolved the question, but not by closing the
band. Its own fitted, continental-scale rift moved the metric the wrong way
(lower than Sculpting's, not higher), which turned out to confirm the
diagnostic rather than refute it: the estimator rewards single-hex-scale
coastline texture almost exclusively, so a large-scale geometric fit was
never going to move it much, and the one lever that does move it (cell-scale
texture) hits a hard fit-verification ceiling before it can close the gap.
The campaign then measured the real planet through the exact same,
unchanged estimator for the first time — and Earth's own coastline scored
*below* the floor every generated world had been held to. A floor no real
planet clears is not a floor; it is the contaminated anchor the diagnostic
had already flagged, now proven. The band was superseded, not closed: an
Earth-anchored range now serves as a sanity floor rather than an acceptance
gate, and single-scale coastline complexity stepped down from this
project's headline shape criterion to a tripwire against degenerate output.
The more interesting bet this surfaced is banked, not built — a coastline
score is the wrong shape for a pass/fail band in the first place, because a
coast is a variable meant to *vary and drive something else* (habitat edge,
harbor geometry, a maritime-versus-continental cultural split) rather than
sit at a constant the Laboratory checks once and forgets. This is the
chapter's discipline working exactly as intended, one step further out: a
bet stays open for a structural reason, and when it resolves, it can resolve
by convicting the instrument instead of the world. See
[Crust](./chronicle/crust.md), [Sculpting](./chronicle/sculpting.md),
[Rift-and-Fit](./chronicle/rift-and-fit.md), and [The Census of Coasts
IV](./laboratory/census-of-coasts-iv.md).

[The Threshold](./chronicle/the-threshold.md) exercised this discipline on a
bet of its own and got the answer the discipline exists to make possible: **no**.
It froze, before a line of code, the claim that a cold creature with a fire in
its house would suffer the cold measurably less than one without — and then
failed to find it, four times over. What makes that worth recording is not the
failure but its shape. Each null was designed to kill one candidate explanation
and did: the fire was too faint (so it was recalculated from an energy balance —
envelope, infiltration, hearth power, the radiant crowding that is why people
sit close — the argument written down and committed *before* anything was
measured again); then the creature never reached the fire (so it was taught to
cross the room, and did); then the instrument could not see where the creature
stood (so it was taught to look). Warmth, walk, and witness each eliminated in
turn, the remaining explanation is not about the machinery at all: **the
creatures who live where it is cold are either already within their own
tolerance, or forty to eighty degrees beyond anything a domestic fire could
offer.** There is nobody in between for a hearth to save.

Two things follow for this chapter. The first is that a preregistered
prediction is only as good as the *sequence* of measurements behind it — a null
that eliminates nothing is a wasted run, and four that each eliminate something
are a result. The second is subtler and concerns the anchor: this bet's own
warning is that a drift check pins output against change and has no opinion
about whether the output was ever right. Here the analogue bit at the level of
the *instrument* — an acceptance protocol verified byte-identity with a command
that could not, by construction, reflect the layer being changed, and four
stages of evidence were vacuous before anyone noticed. The check that a
measurement can move at all belongs beside the measurement, not after it.

[The Millrace](./chronicle/the-millrace.md) supplies the sharpest measurement
of that cost this chapter has, and it is a count rather than an argument. One
of the census columns this chapter counts among the world's self-checks —
`channel-connectivity`, which asks whether a tributary's junction with its
trunk stays inside the channel band — was asking its continuation question
with a stricter test than the network's own, so **82.83% of its walks ended
before reaching the join they existed to test** and scored intact without ever
testing one. The column read 1.0000. Repaired, on sixty-four worlds, it still
reads 1.0000 — the value was right and the claim behind it was empty, which is
the exact failure a drift check cannot see. The campaign then went looking for
the same shape in its own work and found it **five more times**, twice inside
the repair for the first instance and once in the file whose own documentation
is a warning about it. Two consequences for this chapter's confidence
accounting. A column's *value* being stable across campaigns is evidence about
the world only if something independent establishes the column can move at all;
and the discipline that catches these is not review but **mutation** — every
one of the six was settled by neutralising the code under test and watching
whether anything went red. Nothing in the standing gate does that for a
Laboratory metric.

[The Mire](./chronicle/the-mire.md) exercised the same discipline on a bet
about weather and world structure that no earlier chapter entry had staked,
and it too came back **no** — a double falsification rather than a single
one. It froze, before any code existed, that a weather-gated modifier on the
connection graph's edge conductance (mud and snowpack lowering it, frozen
ground raising it back) would move the passable fraction of the world's
connection graph by a global, latitude-graded amount: at least a 5% median
swing across two hundred generated worlds (the systemic-effect bet), growing
toward the poles where weather is harshest (the where-it-shows-up bet).
Neither held. The measured median swing is **0.95%**, an order of magnitude
under the floor, and the swing that does exist runs backward: equatorial
cells swing furthest (0.0224), temperate cells less (0.0021), and polar
cells swing **exactly zero** — not merely small, zero on the nose, across
every sampled seed.

The mechanism is the durable part, and it generalizes past this one
measurement: **seasonal variation lives where conditions alternate, not
where they are extreme.** A permanently frozen polar cell has one season,
all year, so it never crosses the conductance threshold in either
direction; an equatorial cell, wet in one season and dry in the next, is
exactly the alternation the instrument can detect. Extremity without
alternation is stasis. Two checks confirmed the null was real rather than
the instrument being blind: a synthetic all-or-nothing probe (every land
edge fully open one day, fully closed the next) registered swings ten to
twenty times the measured median when an effect of that size was
deliberately manufactured, and across a full year only about 4% of real
land edges ever cross the passability threshold at all — most of the graph
is simply always-open or always-closed, regardless of season, which is the
mechanistic reason the systemic swing is small. This null joins the
chapter's growing record of preregistered predictions that came back no —
alongside the fire-warmth bet above and the conflict-cascade criticality
bet below — each recorded as a finding rather than a failure, because a
chapter that only reports confirmations is measuring taste, not the world.

What the null does not settle is stated in the chronicle rather than
smoothed over. The measured quantity is **passability** — whether a route
is open at all — not **cost**, how much slower or harder a route becomes
while it stays open; a large seasonal cost effect could sit entirely
beneath this instrument's threshold-crossing view and be invisible to it.
And the result is a claim about **land only**: water edges were
deliberately left ungated this campaign, so "the poles do not vary" may be
true for land and false for the sea ice that borders it, on coastlines
whose land itself never varies because it is permanently frozen.

*Re-scored sideways by [The Fathom](./chronicle/the-fathom.md) (2026-08-13),
which did not settle the underworld question above but corrected the premise
the sentence rests on.*

**"The way the sea's depth layers already are" was more generous to the sea
than the sea deserved.** That clause reads as though the marine model were a
finished thing to copy. It was not. The sea had the *vocabulary* — five pelagic
strata, each pairing a community with a depth — and it did not have the
*enumeration*: the accessor returned exactly one stratum per cell, the one its
floor lies in, so the water standing above that floor was unaskable. A cell over
a vent reported the vent and nothing about the kilometre of open water above it.
Declaring the underworld as places was therefore never going to be a matter of
copying a working pattern sideways; the pattern had to be built first, and this
campaign built it.

Two things fell out of asking the question for the first time, and both lower
confidence in the sea as an exemplar. **Seed 42 has no cell whose floor reaches
`Abyssal` or `Hadal`** — column heights come back `{1: 1749, 2: 6669,
3: 21478}` over 29,896 ocean cells, so two of the five pelagic strata never
occur as a floor in the flagship world at all, and the deepest arm of the marine
classifier is unreachable there. And **8,916 of 9,695 sea-ice cells (92%) carry
a stratum below the epipelagic** — ice filed four kilometres down, because the
classifier picks sea ice on surface temperature with no depth condition while
taking its stratum from the floor. Neither is a defect this campaign introduced;
both are things that could not be seen while one value came back per cell, and
both were left standing deliberately rather than repaired, because the campaign's
acceptance criterion was that no world byte move.

So the underworld question is **still open**, exactly as stated above, and the
route to it is one step longer than the chapter thought: the column had to
become askable before anything could be declared in it. What is now settled is
only that asking is possible. Whether a realm's hard gate generalises past caves
remains the thing a campaign placing a people underground will find out.

## Precedented but nontrivial (moderate confidence)

- **Lazy retrospective generation** — committing detail only on observation,
  consistent with a statistical prior. *Caves of Qud* and *Ultima Ratio
  Regum* prove pieces of this can work; nobody has done it against a
  fields-plus-ledger substrate at this scope, and the observe-then-commit loop
  is not yet built. Its self-scorable half is named below.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27): the bet
  moves halfway, and only halfway.** The phrase names two mechanisms, and the
  campaign shipped exactly one of them. *Derive-on-demand* now exists at the
  finest band the world has: a chamber's existence, its interior and its prose
  are a pure function of the derived brief, the address and the seed, computed
  when a player walks in and discarded when they leave. Against ~4^9 candidate
  addresses under a single locale, that is the statistical-prior half working at
  its intended ratio — the overwhelming majority of the space is never
  materialized because existence is a predicate rather than a given, and it is
  asserted by test that it stays that way. *Commit-on-observation* does **not**
  exist and was deliberately excluded: The Lintel commits nothing at all, which
  is precisely what preserves byte-identity — the player's position has never
  been a committed datum, so descent needed no schema change and no epoch.
  Promotion-on-touch — the write half, where an observed detail is *kept* — and
  the delta store it implies remain unbuilt, and are the harder half, since they
  are where a lazily generated world can begin to contradict its own prior. So
  the bet's confidence in *derivation* is materially higher than it was, and its
  confidence in the *loop* is unchanged.
- **Coarse constrains fine.** The design principle — a `ConstantSun` and a
  generated star system are both valid; higher fidelity refines and never
  contradicts lower — *shipped*, and holds from astronomy through religion's
  tiers. Crust sharpened it into a stated contract (decision
  0038): the terrain quantities that are
  *pointwise* — crust thickness and age — are stateless `Field`s any grid may
  resample, while the *mesh-bound* ones (sea level, drainage, placement)
  compute once on the world's canonical grid. So the pointwise half of the
  substrate is now genuinely resolution-free: the render lens samples the
  elevation field below cell scale, and the crust field byte-agrees across
  nested grid levels. The *Dwarf Fortress* move it is sometimes conflated with —
  runtime level-of-detail, refining an *active region* on the fly with the seams
  kept invisible — was the mesh-bound half the field/grid line isolated as the
  remaining work, and [The Room Mesh](./chronicle/the-room-mesh.md) has now laid
  its foundation. A room is a triangular face of the *same* icosphere refined
  deeper, so a level-7 room literally *is* a level-7 triangle: the seam problem
  that made active-region refinement look risky is dissolved structurally, not
  patched, and the dissolution was oracle-validated to `max|Δ| = 0` across all
  327,680 faces of a level-7 globe. Local detail is now summonable per-address
  at arbitrary depth for zero global cost, through an O(1) integer neighbour walk
  and coarse-field inheritance hooks. What is *not* yet built is the layer that
  consumes this substrate: the runtime active-region swap itself, its delta
  store, and the spike-validated adaptive-depth walk that lifts the uniform-depth
  restriction — all deferred, all resting now on a substrate that exists. The bet
  has moved from *no mechanism* to *mechanism shipped, composition pending*.
  [The Region](./chronicle/the-region.md) shipped the first cross-repo
  realization of the pointwise half — a regional tile query
  (`scene/tiles-region/v1`) that samples and barycentrically interpolates the
  continuous fields at arbitrary on-tile density, fed to a client that builds a
  registered globe patch from a tile's address. In doing so it drew the honest
  line the phrase *resolution-free* had blurred: the fields are free to *sample
  and smooth* below cell scale, but carry no sub-cell *information* — the
  ~110 km canonical cell is the physics floor, and interpolation beneath it is
  cosmetic, not fidelity. The active-region swap the bet still awaits inherits
  that boundary: it can refine geometry indefinitely, but never invent physics
  the cells do not hold. [The Massing](./chronicle/the-massing.md) deepened the
  *client* half of that consumption — the globe's level-of-detail ceiling lifted
  (a purely client-side reach for the finer region tiles the substrate already
  served) and the camera's own floor lowered to meet it — and, more pointedly,
  gave the cosmetic-versus-fidelity boundary a *renderer that shows it*. A voxel
  globe draws one block per cell and no gradient between, so the ~110 km floor
  reads as the visible edge of a block rather than hiding inside a smooth slope:
  the honest instrument for the very question of whether the cells' own
  resolution — not the client's — is the next floor worth deepening. That
  producer-side deepening stays deferred; what The Massing added is the
  instrument to judge when it is owed.
  **Re-scored by [The Lintel](./chronicle/the-lintel.md) (2026-07-27):** the
  substrate now has a *second occupied band*. A possessed body can stand at
  nine refinements below the walk band — one address space, a longer path — so
  the uniform-depth restriction is lifted in the narrow case the two-band
  vocabulary defines, and band changes are confined to visible thresholds
  precisely to avoid the thrashing an automatic adaptive-depth walk would
  reintroduce. This does **not** breach the ~110 km physics floor the row draws:
  a chamber's content derives from the *committed occupation history* of its
  walk-band ancestor, not from interpolating fields beneath cell scale. The
  distinction is worth keeping sharp — refining geometry below the floor stays
  cosmetic, while refining *what is recorded to be there* is fidelity the
  ledger already holds. The runtime active-region swap and its delta store are
  still unbuilt.
  **Re-scored by [The Blocking](./chronicle/the-blocking.md) (2026-07-28): the
  principle now has a *number*, at the finest band, and that is the largest
  movement this row has had.** "Higher fidelity refines and never contradicts
  lower" has always been checked by *agreement* — a field resampled at two grid
  levels must byte-agree — which tests that the fine layer does not disagree with
  the coarse one. It says nothing about the fine layer **inventing**. The floor
  plan is the first fine layer whose entire content is a lowering of a coarse
  structure (an anchor graph of chambers and links, itself derived from committed
  history), so the question became answerable in the other direction: how much did
  the fine layer add? The embedder reports its **residual degrees of freedom** and
  the checker compares that number against how much freedom the graph leaves free
  — and it is *exact*, not merely bounded, at every chamber count over two
  thousand seeds. Being **under** budget is a finding too, since it means the seed
  is not filling freedom the graph genuinely left. That converts the principle
  from a design intention into a measured property of one derivation, and it drew
  a line the phrase had left implicit: a plan's extent derives from chamber count
  alone and *spends no draw*, because a coarse constraint that consumes randomness
  is not a constraint, it is another generator.
  **Re-scored by [The Grain](./chronicle/the-grain.md) (2026-08-11): the
  principle now has *teeth*, measured — and this row had been reading it as a
  permission when half of it is a prohibition.** Every re-score above asks what a
  fine layer may *add*: agreement across grid levels, then residual degrees of
  freedom. The Grain asked what a fine layer may *subtract*, and got a number. A
  sub-cell refinement of one nominal field — a room's water kind, banded from a
  three-corner blend of the drainage the cells already hold, fitting no new
  constant and inventing no new physics — passed both of its preregistered local
  hypotheses and the whole commit gate, and **destroyed 29% of the world's fresh
  water at walking depth**, halving thirst-driven fauna movement. "Refines and
  never contradicts" forbade it, and nothing in the suite noticed, because every
  check the principle had was a check on *agreement* and this was a failure of
  *conservation*. The general result is sharp enough to be a rule rather than an
  anecdote: whether a field's values are **ordered** decides whether it may be
  refined by banding a blend at all. An ordinal field (relief) may — a blend moves
  it at most one band and conserves the distribution's shape. A nominal field
  (water, biome) may not — a threshold is maximally nonlinear, so classifying a
  blend is not the area-weighted vote of classifying the corners, and it deletes
  whichever category sits in the thin tails. Nearest-corner assignment is a
  *partition* and conserves area by construction, which is why the existing
  mechanism was already correct, and why the campaign's founding diagnosis (a
  suspiciously flat chart) turned out to describe the *view* rather than the
  field. Two consequences for this row. First, the ~110 km floor it has drawn
  three times is now known to be **asymmetrically** crossable: geometry may refine
  beneath it cosmetically, *what is recorded* may refine beneath it as fidelity
  (The Lintel), and a *category* may not be re-derived beneath it at all. Second,
  the honest response to a view finer than a field's model is neither refinement
  nor silence but **disclosure** — the chart now declares which of its fields are
  decided at grid resolution and are therefore constant beneath it, exactly as its
  colour block already declares what a projection does not carry. The bet's
  confidence is unchanged; what moved is that the principle stopped being a design
  intention with one positive measurement and became a constraint with a
  documented violation, a rule that predicts such violations in advance, and a
  conservation test in the gate.
  **Re-scored by [The Ford](./chronicle/the-ford.md) (2026-08-11): the field/grid
  dichotomy this row has been reasoning inside gains a third category, and the
  ~110 km physics floor is no longer where sub-cell *information* stops.** Decision
  0038 split terrain quantities into *pointwise* fields any grid may resample and
  *mesh-bound* quantities computed once on the canonical grid, and everything since
  has treated that split as exhaustive — which is why refining beneath the cell has
  kept reading as cosmetic. A river is neither. It is **feature-bound**: carried on
  a polyline with a discharge-derived width, evaluated as a function of position at
  any depth, and *the same object* at 110 km and at 27 m rather than interpolated
  between them. That is genuine information below the cell floor, on the producer
  side, and it arrived **without** refining the mesh — the adaptive-refinement
  route this row has been waiting on is not what delivered it. The reason
  generalizes past water: the quantity did not need finer resolution, it needed to
  stop being mesh-bound at all, and no amount of subdivision could have
  substituted, since a linear interpolant on a simplex attains its extrema at its
  vertices and so admits no sub-cell valley for a river to occupy. So the bet's
  confidence rises in a direction it was not pointing — the floor is a property of
  a quantity's *carrier*, not of the grid, and any quantity whose carrier matches
  its dimensionality escapes it. What is unchanged is what the row was actually
  tracking: the runtime active-region swap, its delta store, and adaptive-depth
  refinement of the quantities that legitimately remain mesh-bound are all still
  unbuilt. What is newly open is how many other quantities are miscarried the way
  rivers were.

  **The two re-scores above landed on the same day from two campaigns that did
  not see each other, and they must be read together.** The Grain's rule is a
  prohibition on *mechanism*: a nominal field may not be re-derived beneath the
  floor by thresholding a blend of corner values, because a threshold is
  maximally nonlinear and deletes whatever category sits in the thin tails. The
  Ford does not use that mechanism — it does not blend anything; it evaluates a
  signed distance to an emitted polyline, so the question stops being "what
  class does this interpolated value fall in" and becomes "where is this point
  relative to a feature". On mechanism, therefore, they do not collide, and The
  Grain's rule is arguably the sharpest available statement of *why* changing
  the carrier was the right move rather than refining the field.
  **What is genuinely open is conservation, and it is open in The Ford's
  direction.** The Grain's measured harm was a loss of area — 29% of the world's
  fresh water gone at walking depth. The Ford's channel occupies ~0.026% of land
  where the cell-scale river class occupies ~6.3% of land cells, and 39 of 700
  river cells carry no polyline at all. That is not a defect of The Ford's stage
  1, which moved no consumer and left `river_proximity` and the toponymic gates
  untouched by asserted invariant; it is the question its stage 2 inherits, now
  under decision 0124's requirement that a refinement preregister a conservation
  criterion — which The Ford's own spec, frozen before 0124 existed, did not do.
  The bet's honest state: the floor is crossable in more ways than this row
  once assumed, and each way owes a different proof.
  **Answered, in part, by [The Ford's stage 2](./chronicle/the-ford-stage-2.md)
  (2026-08-11): the inherited conservation obligation was not met — it was
  declined, by not making a refinement claim at all, and the row should record
  which of those two things happened.** Stage 2 was expected to redefine the
  room's water field and mint a new schema. It did neither. The room instead
  gained the *quantity* — a signed distance to the channel and the band edges
  that apply at that spot — appended as trailing keys, with the existing water
  field, its mechanism, the availability predicate and the toponymic gates all
  held fixed by asserted invariant. Nothing coarse was re-derived, so nothing
  had to be conserved, and the byte-cleanliness of the append was checked
  rather than asserted: nineteen insertions and no other change across seven
  regenerated artifacts. That is a third way past the floor, distinct from both
  re-scores above — not refining a field, and not changing a carrier either,
  but **adding a measurement beside the field and letting the consumer set the
  cut**. The Grain's disclosure answer is what makes it legible: the room now
  declares which of its fields were decided at grid resolution and which by the
  channel, so a reader can tell a flat field from a broken one without guessing.
  **What this does not do is close the conservation question, and stage 2
  produced its sharpest witness.** A seed-42 room reports its water as *river*
  while standing twenty times its own outermost band edge from any channel —
  the grid's answer and the network's answer, contradicting each other inside
  one document, because a river short enough to occupy a single cell never
  becomes a polyline. The disagreement is not new; what is new is that it is
  now visible in a single record instead of split across two subsystems that
  never met. A contradiction a reader can see is the precondition for repairing
  it, and the repair — a stated rule for which half wins, or lines for those
  cells — is owed by a later stage. The bet's confidence is unchanged.
  What moved is the menu: crossing the floor by *addition and disclosure* costs
  no conservation proof, and is available to any quantity willing to travel
  beside the coarse field rather than replacing it.
  **Re-scored by [The Rill](./chronicle/the-rill.md) (2026-08-13): the
  principle was *measured failing*, at a magnitude no previous campaign had
  produced, and the repair converts it from a rule a design must respect into
  a property of what kind of quantity is being refined.** Every re-score above
  argues about what a fine layer may add, subtract or disclose. The Rill asked
  the mechanical question underneath all of them — *which quantity is being
  refined* — and got a dichotomy with a measurement on each side. A **scalar**
  refines by area partition: the parts sum to the whole, so the fine answer
  cannot disagree with the coarse one, by construction rather than by test. A
  **direction** refines by a transfer operator between a mesh and its dual, and
  **there is no canonical one**. The campaign built the direction lift first,
  in good faith, and it delivered 26–31% of land to the sea where the coarse
  graph delivers 74–82%, doubled the basin count, and sent 6.0–7.5% of interior
  faces to a terminus outside their own coarse basin. Rebuilt as a partition of
  the scalar with directions *inherited from attachment* rather than computed,
  basin agreement is 41,415 of 41,415. Three consequences for this row. First,
  the row's own long-standing framing was subtly wrong about the mesh: it has
  read "a level-7 room literally *is* a level-7 triangle" as licence to lower
  coarse structure onto rooms, and cells are the icosphere's **vertices** while
  rooms are its **faces**, so a coarse flow edge runs *along* a room's boundary
  and never through it — the primal/dual distinction is load-bearing and the
  active-region swap this row still awaits inherits it. Second, **every local
  invariant held while the composed one failed**, which is the sharpest
  statement yet of what a refinement owes: agreement checked one step at a time
  is not agreement, and the reference must be the coarse graph's *composed*
  answer. Third, the harm has a visible signature — routing a direction out of
  every element gives every element a channel, and the campaign's world came
  out with 5–6% of its land underwater against a 0.5% ceiling. **Saturation is
  the signature of having refined the wrong quantity**, and it is cheap to look
  for. The bet's confidence rises: it now has a mechanical test (is this
  quantity a scalar or a direction?) that predicts violations before they are
  built, alongside The Grain's test (is this field ordinal or nominal?) that
  predicts them for values. What is unchanged is that both tests were bought by
  building the violation first.


## Genuinely open — split by whether the world can grade itself

The remaining low-confidence bets do not sit at one altitude. Each has a
**self-scorable half** the Laboratory could close on its own, wrapped around a
**taste-gated half** that waits on a human read. Naming the seam is most of the
progress.

The split is no longer hypothetical: The Chorus drove one bet of exactly
this shape to a verdict. Whether a derived cultural account differs from
ground truth *as a worldview* (not merely in vocabulary) looked
taste-gated until it was decomposed into distinctiveness ×
recoverability; the preregistered known-groups gate then separated the
uncanny pole from the gibberish pole from the shipped voices on every
measured world ([Study 012](./laboratory/study-012.md)). The residue that
stayed taste-shaped is exactly what the decomposition predicted:
*is it pleasant to read* — a far smaller surface than *is the worldview
right*. That is the template the bets below should expect: the
self-scorable half closes by instrument, and the taste half shrinks to
its honest size.

1. **Refinement at scale.** Generating detail consistent with fields *and* a
   large committed ledger, with aesthetic requirements on top, is constraint
   satisfaction plus taste — and the two halves have very different horizons.
   *Consistency* is self-scorable today: a generated detail either violates a
   committed fact or a field prior or it does not, and that is a metric, not a
   judgment. *Aesthetic quality* is taste, and it is the half that is years
   away and may need ideas that don't exist yet. The honest move is to build
   the consistency tier — checkable now — and stop letting the taste half make
   the whole problem look untouchable.
   **Re-scored by [The Wearing](./chronicle/the-wearing.md) (2026-07-29): the
   taste half shrank, exactly along the template this section's preamble
   predicts.** *A generated place name is too long to say and too uniform to
   believe* reads like pure taste, and had been treated that way. Decomposed,
   most of it was not. **Length** was already instrumented and the instrument
   was being ignored — the metric's declared buckets stopped at 10 characters
   and every world in a thousand-seed census overflowed them, silently, for
   several campaigns; a declared bucket range nothing enforces is an intent, not
   a check. **Syllable count** is a second instrument the campaign had to add,
   because character length cannot separate *shorter words* from *the same
   words spelled tighter* and the diagnosis turned on precisely that
   distinction. **Transparency** is the interesting one: the property that made
   the names read as generated was not any name's opacity but the *uniformity*
   of their readability — 650 of 650 names fully glossable, by construction —
   so the metric that closes it is a **distribution witness whose target is
   explicitly not its maximum**, and whose comment records that a drift back
   toward 1.0 is a regression. Three self-scorable readings where the honest
   prior expectation was one human read. What did **not** move is the residue
   the decomposition leaves: whether a given name is *pleasant*, and whether a
   world's toponymy reads as inherited rather than issued, are still a human's
   call, and the campaign's own success criterion for that half was written
   down as the owner's judgement rather than as a number. So this row's
   confidence in *scoring aesthetic constraints* is materially higher than "the
   half that is years away" allowed; its confidence in *closing* them is
   unchanged. The complementary lesson is a caution for the whole gradient: an
   instrument only scores a bet if something reads it. This one existed,
   drift-checked green, and measured a failure nobody was told about.

   **The Watershed sharpened that caution into its harder form (2026-07-31).**
   There, the instrument was read constantly — and was *wrong*.
   `exposure-sound` reported false on roughly three quarters of all worlds
   because the Laboratory's deliberately hand-maintained duplicate of the
   exposure rules had not learned a rule an earlier commit added. The worlds
   were correct throughout. It was the second such lapse in eleven days, and
   the campaign least able to notice was the one whose central mechanism the
   metric measures. So the caution generalizes: *an instrument scores a bet
   only if something reads it AND the instrument is itself current*, and
   nothing in this repo reddens when a deliberate duplicate falls behind.

   **The Domesday adds the third clause (2026-08-08), and it is the one that
   bites hardest.** *An instrument scores a bet only if it measures the quantity
   the bet is about.* The census's most-cited climate finding — that the climate
   is not merely cold but near-uninfluenced by its own astronomy — was drawn
   from twenty-three astronomy metrics, not one of which is insolation, stellar
   luminosity, or orbital distance. The facts exist and are committed to every
   world's ledger; `anchor-orbit-au` and `insolation-rel` sit in the same
   registration block as `brightening-per-gyr`, which the census does read. The
   evidence for *astronomy does not drive climate* is therefore a single
   orbital-period proxy, and the driver itself was never in the dataset. The
   conclusion may well survive measurement — the survey takes no position on
   that — but its current standing is weaker than the sentence it produced, and
   the first campaign to read all 193 metrics at once is what made that visible.
   The same survey found the complementary gap in the other direction: fourteen
   biology metrics are frozen across all thousand worlds because species life
   history is a pure allometric function of authored mass, class and schedule,
   so no world quantity reaches any creature's physiology. Between the two, the
   census's coverage of *what influences what* is materially thinner than its
   193-metric breadth suggested.

   **[The Armature](./chronicle/the-armature.md) (2026-08-09) put a number on
   that thinness, and the number is worse than the sentence above allowed.**
   Thirty causal links were declared from physics, frozen before any
   correlation was computed, and measured once: five silent, nineteen
   mis-strengthed, six unmeasurable. Ten of the nineteen measure `|r| < 0.1`
   and **seven of those ten were declared moderate** — mean land temperature
   against ocean fraction at `−0.0005`, against mountain coverage at `−0.041`;
   habitable fraction against obliquity at `+0.058`; settlement count against
   mountain coverage at `|r| = 0.015`. So the previous entry's finding generalizes
   past astronomy: climate is uninfluenced by its **terrain** as well, measured
   directly against drivers the census does hold, and settlement reaches the
   land only through one habitability scalar. These readings are not an
   artifact of a blunt instrument — the same frame measures `+0.951` between
   standing tribute and settlement count, and `+0.730` between fertile land and
   temperature, so it sees couplings where they exist. The frame was
   re-measured against the replacement census The Signet's epoch landed, with
   `studies/expectations.json` unchanged, and every count in this entry held.

   Two cautions temper the re-score, and both sharpen it rather than soften it.
   The six biology rows fired as **unmeasurable**, not as refutations: their
   metrics hold one distinct value across a thousand worlds, so the census
   cannot test those claims at all, and the spec's prediction that all six would
   fire came true for a reason that is not evidence about biology. And
   twenty-five of thirty rows firing triggers the campaign's own falsification
   clause, which says a frame that fires nearly everywhere indicts its author.
   That indictment is half right: four failures are strength over-claims with
   the declared sign intact, and three are sign errors the author owns. The
   seven near-zero readings are structurally different — an over-claim gets the
   sign right and the magnitude wrong, while a severed wire produces no signal
   at all — and that partition is a judgement laid over a measurement, which is
   why the frame was published whole rather than pruned to the rows that
   flattered it.

   That campaign also moved the self-scorable half in both directions at once.
   Sonority sequencing made pronounceability a property held **by
   construction** rather than measured after the fact — reverse-sonority
   onsets no language uses are no longer drawable, at zero entropy cost, since
   ordering a template consumes the same draws as picking one. But the
   transparency witness *fell* over 1000 worlds (0.816 → 0.793) while rising
   at the reference seed, which is the distribution witness earning its keep:
   a single-world reading would have recorded the opposite. Neither movement
   touches the taste half, which is unchanged.

2. **Emergent economics that don't degenerate.** The mermaid-bone-farm
   problem: static value tables meeting exploitable production collapse into
   absurdity, and most game economies are faked precisely because real ones
   misbehave. Here too the bet splits. *Degeneracy* is self-scorable — an
   exploit detector is a Lab study: run the production loops, measure whether
   any yields unbounded value divergence. Whether prices flood, crash, and
   recover *legibly* is the partly-taste remainder. The economics campaign
   still begins with a literature phase (experimental economics, auction
   theory, virtual-world economics), but that phase now designs the apparatus
   that would falsify the claim, rather than standing between the project and
   knowing how it will grade itself.

3. **Historiography worth reading.** The systems half is no longer
   architecture-less: any entity's committed facts already replay into a
   derivation, physical deep time now lays down glacial strata and fossil
   shorelines ([Deep Time](./chronicle/deep-time.md)), and the past is being
   made queryable so that `why <ghost-town>` can recount the ice age that
   emptied it. Its measurable properties — focalization, sparsity,
   unreliability — are the guardrails. But *worth reading* is honestly
   taste-gated, and this is the one place the project refuses to fake a metric:
   *Dwarf Fortress* generates accurate history that glazes eyes, and no amount
   of architecture guarantees the sparse, focalized, unreliable account with a
   teller that would not. The human read is the real gate, and it is allowed
   to withhold a pass.

## The standing horizon

Year 1 varied the world and held the observer; Year 2 varied the observer and
held the sky. The current research varies **time**: two worlds identical at
genesis but differing only in their deep-time forcing, and a legibly different
present — different glacial strata, fossil shorelines, refugia, ghost-town
lineages, and history-derived myths — every divergence recountable through the
event ledger to its cause in the past. The falsifiability teeth are the same
shape as before: a blind-attribution metric over many thousands of worlds,
against a zero-forcing null control whose present must be indistinguishable
from its own genesis. It is a bet at the top of the checkability gradient, and,
like the two before it, it is allowed to fail.

**A partial rescore, now that population has a field to vary.** The
carrying-capacity field promoted above (see the high-confidence tier) is an
*equilibrium* snapshot — `population = f(carrying capacity)` in closed
form, no iteration, no clock — and equilibrium is not the same claim as
"vary time." What remains open splits cleanly along the same
checkability line this whole chapter runs on. Giving the field a clock —
temporal relaxation, and the founding/growth/fission/abandonment history
that only becomes tellable once population moves — **has now landed**: *The
Living Community* (the living-community engine's first campaign) grows the
present world as the last frame of a coarse forward history run over the
capacity field, now ticked per-era by paleoclimate, so settlements found,
grow, migrate, and end across ~2000 years and leave standing ruins. That is
the *placement* half of the vary-time horizon; the deeper bet above — a
blind-attribution metric over thousands of worlds against a zero-forcing null
control whose present is indistinguishable from its genesis — is not thereby
settled and stays open at the top of the checkability gradient. A second,
narrower piece is not a time question at all: today's
condensation runs each species' field independently, which loosens the old
rule keeping different peoples' settlements apart. Packing multiple
species onto one landscape properly — footprint-scaled home ranges, a
tunable competition temperature, predator-prey coupling — **has now
landed**: the coexistence stack packs a multi-species density stack (with a
frozen competition temperature), and *The Niche* gave each species a
niche-differentiated carrying-capacity field, so composition varies across
space rather than resolving to one global blend — seed-42's
identical-everywhere settlements broke into distinct regions with
structured strife along their ecotones. It resolved to the
moderate-confidence tier as predicted; it was architecture, not taste. The
goblinoid roster also showed the model's honest limit: same-resource
species differentiate only ~two ways on climate alone, so the fuller payoff
(strongholds, refugia, a creature that owns the cold) was thought to wait on
a roster with distinct resource niches. *The Menagerie* (the entity-component
program's first campaign) built that roster — sixteen kinds spanning
photosynthate to apex predator — and found the limit lies deeper than the
roster: carrying capacity is `supply × fitness`, but supply is a **single**
net-primary-productivity field scaled per species by uptake, so a resource
niche changes a species' *magnitude* everywhere, never its *place*. Only the
climate term is spatial, and climate alone differentiates ~two ways however
many creatures compete. The stronghold payoff waited, precisely, on
**per-axis spatial resource fields** (minerals in the mountains, prey where
the prey is) and a way to read dominance as resource captured rather than
headcount. *The Demesne* (Stage 1 of a named Living-Biomes arc) shipped the
**abiotic half** of exactly that: photosynthate, forage, and mineral became
real per-cell supply fields, dot-producted against each kind's uptake vector
at the existing carrying-capacity site, and the rank-restoration paid off
wherever an abiotic specialist could reach it — seed-42's distinct material
dominants rose 2→4, and the pure-mineral xorn went from a noise-level single
settlement to the single largest domain on the world (≈29k cells), a
mineral-eater owning its mountains as the place-identity model intends. Two
pieces stay open,
both measured rather than asserted: the **prey axis** (dragons and predators
still hold no place) waits on Stage 2's trophic food-web field, and — newly
surfaced by the shipped half — the four small **peoples** do not diversify at
all, because their authored niches carry zero weight on any axis this stage
spatialized, leaving them competing on one shared forage number. That last was
named its own open design question (an order-independent territory force — a
field or a fixed point, never a cumulative tally); the preregistered
`≥6`-distinct-dominants test — a *material*-dominance target spanning all
sixteen kinds — stays honestly `#[ignore]`d as its remaining target. **The
peoples half of that question has since been answered, but from the other
axis.** *The Living Community* separates the four near-identical goblinoids not
by a spatial resource force at all but by **history**: history-first placement
grows them at different founding sites and displaces them along different
climate paths, so they end up holding distinct territories (region-of-influence
overlap 0.055 on seed 42, every sampled seed under 0.06) — diversity by *time*
where the spatial axes had none to give. It resolved to the checkable tier;
it was architecture, not taste. So the rescore moves the **abiotic** stronghold
debt from open to **banked and measured**, holds the prey-stronghold debt
against Stage 2, moves the **peoples-territory** bet from open to **answered by
history-first placement** (a time resolution, not the anticipated spatial one),
and — the field having gained its clock — narrows the vary-time debt to its
blind-attribution core, still open. *The Sundering* (the connection graph's
second slice) then gave that peoples-territory resolution a second, *dynamic*
mechanism: routing the history bake over a time-varying connection graph — a sea
that falls with the ice, opening land bridges, and rises to drown them — so
peoples are confined to the landmasses they can reach. Seed-42's four peoples
resolve onto four sea-bound landmasses, three holding only a subset of them, and
the territory overlap held (0.0466). It stays in the checkable tier; what it did
*not* resolve — the *volume* of the diaspora, throttled by the world's ample
vacant land and by peoples settling glacially-stable ground — it handed, with
measurements, to conflict-as-criticality.

**And conflict-as-criticality has now been tested, and the bet lost.** This
chapter has to be able to say that, or its scores are decoration. The wager was
that organised conflict, once it emerged rather than being floored, would
**self-organize to criticality** — that the size distribution of cascading
displacement would be a power law, the signature of a system holding itself at
its own critical point. It was always a bet at the *top* of the checkability
gradient in the good sense: preregistered, instrument-gradable, adjudicable by
the Laboratory without a human read. *The Tumult* built it and graded it.
Conflict does now emerge — seed 42, which never crowds, resolves 76 conquests
driven by coveted value rather than by density, and the map gains population
rather than losing it. The distribution is **not** a power law and is not close
to one: pooled over a hundred seeds and 2974 conquests, nothing chains beyond
size three, the support spans 0.48 decades against a preregistered threshold of
about 1.5, the per-octave decay is roughly 46-fold where a heavy tail falls two-
to fourfold, and the branching ratio measures **σ ≈ 0.051** against a critical
value of 1 — stable to three figures across a 3.3× change of sample. Geometric
with a hard cutoff, deeply sub-critical. No constant was tuned toward the
hypothesis at any point.

The honest rescore is therefore: **the criticality bet moves from open to
falsified for the mechanism as built** — not "partially confirmed", not
"promising". What it does *not* move is the underlying question, and the
distinction is the useful part. Two builds now bracket it from opposite sides.
The first, a crowding sandpile, had a **drive and no dissipation**, so every
avalanche ran to the depth cap — an artifact, not a tail. The second has
**dissipation and no accumulation**: each hop of a cascade costs real
population, every victim is weaker than whoever displaced it, and a chain dies
within a hop or two, with nothing stored *between* relaxations whose release
could make a large event. Criticality needs both terms, and the missing one has
a name and a shape — a standing dominance relation that concentrates value into
a topple-able structure, whose collapse frees a whole subordinate network at
once. So the residual bet is narrower and better armed than the original: not
"does conflict self-organize?" but "does accumulation-plus-dissipation
self-organize, on this world, at this resolution?" — with a measured null
result to beat rather than a prior to defend.

Two notes for the gradient itself. First, this is the chapter's second bet
driven to a verdict by instrument rather than by taste, after The Chorus — and
the **first whose verdict was no**, which is the more informative of the two
outcomes and the one a confidence map exists to be able to record. Second, the
falsification cost roughly one campaign and produced a sharper successor
question; the alternative — shipping the mechanism and narrating it as
criticality — would have cost nothing and taught nothing, and the drift-check
would have re-ratified the narration every time it ran. A bet is only worth
placing at this altitude if losing it is allowed to be published as a loss.

**The successor question has now been asked, and it lost too.** The residual
bet stated just above — *does accumulation-plus-dissipation self-organize, on
this world, at this resolution?* — was the whole mandate of *The Tithe*, which
built the missing term: a raid whose prize is *mobile* subordinates rather than
evicts, a patron collects tribute from a vassal it cannot fully see, and what
it collects banks in a store of wealth that feeds strength without ever
entering the pressure that kills. That is a literal accumulator rather than a
metaphorical one, and it works — the structure forms at volume, patrons survive
collecting, and a dominant grows without moving. **The shape of the violence
did not change.**

Two things moved and they must not be conflated. **σ roughly doubled**, from
≈ 0.051 to **0.109–0.115** pooled over thirty seeds and 7183 conquests, and to
0.103–0.109 over a hundred seeds and 22 255 — the same factor on both samples,
which makes it a real effect of accumulation rather than sample noise. That is
a genuine result and this chapter should say so. But **σ ≈ 0.1 is not σ ≈ 1**,
and every reading of *shape* is unmoved: the support still spans **0.48
decades** against the preregistered ≈ 1.5, the per-octave decay is still
**17.6-fold** where a heavy tail falls two- to fourfold, and **not one cascade
exceeds three displacements in roughly twenty-two thousand conquests**. Still
geometric with a hard cutoff, still deep in the sub-critical regime. No
constant was tuned toward the hypothesis at any point, and the last mechanism
the campaign added had its predictions **written into the spec before its code
existed** — including, explicitly, that revolts firing while the distribution
stayed geometric would be a *stronger* falsification than the standing null.
Revolts fired. The distribution stayed geometric. **That is the branch the
preregistration named as the harder one to explain away, and it is the branch
that happened.**

The honest rescore is therefore: **the criticality bet moves from falsified for
the mechanism as built to falsified a second time, against a mechanism built
specifically to answer the first falsification's diagnosis.** Not "progress
toward"; not "trending". The right way to hold it is that the *diagnosis* has
narrowed, not that the *bet* has improved. Two builds bracketed the answer as
drive-without-dissipation and dissipation-without-accumulation; a third
supplied accumulation and moved the number without moving the family, which
eliminates "nothing is stored" as the explanation. What is left is
**conduction**. A revolt frees exactly one vassal — collapse-release, where a
fallen patron's entire network is freed at once, was a stated non-goal — and
the relation graph is a set of one-level stars, because a vassal may not itself
take a vassal, and depth was the other stated non-goal. A patron's failure has
nowhere to propagate. An avalanche needs a medium, and this world does not yet
have one.

So the residual bet narrows again and is now nearly bare: not "does conflict
self-organize", not "does accumulation self-organize", but **"does a
*connected* accumulating structure self-organize?"** — with two named,
already-specified levers as its remaining content and two measured nulls behind
it. What that costs the gradient is worth stating. A bet that loses twice in a
row, each time to an instrument, each time with the mechanism built rather than
argued about, is more expensive to keep than to drop; the case for asking a
third time rests entirely on each null having eliminated a *different*
candidate, so that the third question is materially different from the first
two rather than a rephrasing of them. **If the connected version also comes
back geometric, the right conclusion is that this world does not sit at a
critical point, and this chapter should record that as settled rather than
open.**

### The third ask is being spent elsewhere (2026-07-29)

That test — *materially different, or a rephrasing?* — has now been applied,
and the answer is that the connected-cascade question **does not clear its own
bar by much**, while a different question clears it easily. This chapter is
therefore rescored: the criticality bet is **not** being asked a third time in
the form above, and the depth-and-collapse-release levers are **deferred, not
refuted**.

The reason is that three campaigns have been measuring the size distribution of
**events** — how long a cascade of displacements runs — while the property the
project actually wants from its history is a distribution over **entities**:
how large the largest polity gets, how unequal holdings become, whether an
empire is a thing a world can produce at all. Those are different variables with
different mechanisms and different literatures. Event-size criticality is
Bak–Tang–Wiesenfeld, and the conduction diagnosis is correct on its own terms.
Entity-size heaviness is Gibrat and Kesten — a random *multiplicative* factor
against a reflecting lower barrier — which is the standard account of Zipf's law
for city sizes and of the empire-area distributions. **Hornvale has never
measured it, and the bake has no empire-size metric at all.**

Reading the mechanism against that second literature explains the two nulls
without appealing to conduction, and the reading was verified in source rather
than reasoned about. A Kesten process needs a per-entity random multiplier that
persists. The bake's strength is `(population + stores × 0.5) × tech_weight`.
Population is logistic, so its growth is *anti*-proportional to its size near
capacity. Stores decay at 0.95 per epoch to a fixed point set by inflow, and are
destroyed on a community's closure. `tech_weight` takes four values capping at
3.0, is driven by absolute year, and its per-people head start is a draw in
[0, 300) years against era boundaries at 400/900/1400 — so **the world's only
irreversible advantage provably converges to zero relative value at year 1400**.
Every multiplier in the model is shared, capped, or mean-reverting, and no two
communities of one people differ in any authored dimension at all. A model with
no persistent per-entity multiplicative heterogeneity cannot produce a heavy
entity-size tail, and would not do so even with a conduction medium added.

*Re-scored in part by [The Tolerance](./chronicle/the-tolerance.md) (2026-08-05),
which voids one clause of the paragraph immediately above and leaves the more
important one standing.* That campaign made a people a distribution rather than
a point: each settlement now draws its own threat response from its
people's authored mean and dispersion, keyed on where and when it was founded
and fixed for the life of the community. So the clause **"no two communities of
one people differ in any authored dimension at all" is no longer true** — two
towns of one people, on different ground in different centuries, hold genuinely
different temperaments, and the between-settlement variance in that dimension
went from exactly zero to 0.010–0.113 depending on the people. The heterogeneity
is persistent and per-entity, which is two of the three properties a Kesten
process wants.

The third it does not have, and that is the part this chapter must not round
away. The drawn quantity enters the model as a **gate on a decision** — a
community above the threshold may take the initiative, one below it may not —
not as a **multiplier on strength**. Strength is still
`(population + stores × 0.5) × tech_weight`, and every term in it is still
shared, capped, or mean-reverting; nothing about the disposition draw multiplies
anything. A heterogeneous *propensity to act* changes which communities move and
therefore how the history branches, but it does not give a community a
persistent random factor on its own growth, which is the specific thing the
literature says a heavy entity-size tail requires. So the correct rescore is
narrow: **the diagnosis loses its "no heterogeneity exists" clause and keeps its
"no multiplicative heterogeneity exists" clause**, and the entity-size
prediction is unchanged. Nothing here was measured against M2 — The Tolerance
preregistered variance and rate hypotheses, not a size distribution — so this is
a correction to the *argument*, not a new reading of the *bet*. A campaign that
wants to test the Kesten account now has a cheaper route to it than it did: the
authoring pattern for per-entity variation exists and is proven, and what
remains is to point one at a multiplicative term instead of a threshold.

One detail sharpens this rather than softening it, and it is the same point
clause 3 above makes about asymptotes. The new heterogeneity is drawn from a
**uniform** on ±√3σ, clamped to the axis — so it is not merely
non-multiplicative, it is *bounded*, and the probability of a settlement
exceeding its people's support is exactly zero rather than small. Per-entity
variation now exists in this world; **rare** per-entity variation still does
not. The build constraint this chapter already owes a successor campaign is
unchanged, and one more mechanism now sits inside its scope.

**What replaces the bet is narrower, and it is a different shape of claim.**
Not a power law: a **sigmoid**. The wager is that annihilation, coexistence and
domination lie on one saturating response, that the middle is where nearly every
world sits, and that both extremes are **reachable but rare** — a world with no
goblins, and a world under one government, each possible and each unusual. This
is preregisterable, it is falsifiable in both directions, and it is a claim
about a distribution the Laboratory can compute over seeds rather than about a
scaling exponent that needs 1.5 decades of support to be well-posed at all.

It also carries a structural requirement the previous framing never surfaced,
recorded as [decision 0096](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0096-diversity-is-terminal-and-rubberbanding-is-multi-axis.md)
clause 3: rare extremes need **asymptotes, not clamps**. Hornvale's saturating
bounds are presently clamps — population against capacity, `tech_weight` against
3.0, `coexist.rs`'s viability `FLOOR` — and the probability of exceeding a clamp
is exactly zero at any input. On the current response forms the tails are not
rare; they are impossible. That is a build constraint, not a tuning target, and
it is the first thing a successor campaign owes this chapter.

**Confidence: low, and deliberately so.** The sigmoid has not been measured, the
claim that per-world conditions vary widely enough to reach either tail is
**unverified**, and this chapter should not be read as predicting the result. The
one thing it does now assert with the same confidence as the two falsifications
above it is the diagnosis: **this world's history evaluates every people on a
single scalar axis, and on one axis weakness is absolute.** That is checkable in
forty lines of source, it is the standing charge decision 0096 opens, and it is
why the third ask is being spent on a second axis rather than on a deeper graph.

One further note the campaign earns a place for, because it bears on how much
any of the above should be trusted. *The Tithe* amended its own specification
**five times, four of them following a disappointing measurement** — and that
count is disclosed in the spec, in the adjudicating test's own documentation,
and in the chronicle, because a reader who meets only the final histogram has
been misled about how it was arrived at. The protection taken was
preregistration of the last amendment. The lesson for this chapter is that a
confidence score is only as good as the disclosure attached to the measurement
under it: the number here is a falsification, which is the direction that
*cannot* be manufactured by adding mechanisms, and that asymmetry is the reason
the rescore is trustworthy despite the amendment count.

### The sigmoid's first axis is measured, and the null is in (2026-08-02)

*The Contour* built the cheapest test of the sigmoid wager's own diagnosis —
a second contest axis, uncorrelated with strength, entering at the raid
dominance test — deliberately touching no authored species data, so that if
it moved nothing the two costlier campaigns behind it (*The Appraisal*,
*The Deviation*) would be worth reconsidering for one campaign's price rather
than three. It moved nothing. Both of the wager's own preregistered halves
are now measured, matched against a frozen thirty-seed baseline, and both are
null:

- **M3 (peoples-alive-at-bake-end) fell, fractionally, rather than rising.**
  The entire thirty-seed delta is one world losing one people; every other
  seed's count is byte-identical to baseline, including the exact set of six
  extinction seeds. The mechanism rescued zero worlds from total extinction
  and caused zero new ones.
- **M2 (the entity-size distribution — the sigmoid's own headline variable)
  stayed geometric.** Mean, median and IQR sit within a few percent of
  baseline at both thirty and a hundred seeds; the one statistic that moved
  cleanly (max/median) moves inside the band a single outlier seed produces,
  not a distributional shift.

Per §4.3 of the spec, both conditions being met is the null the chapter
above already named as the informative branch: **a second contest axis,
uncorrelated with the first and entering at the decision point, is not
sufficient to hold diversity open in this world** — a finding about decision
0096 clause 1's *chosen mechanism*, not about the axiom, and one that sends
the sequence back to design rather than forward to *The Appraisal*.

**The null itself decomposes, and the decomposition is the part this chapter
must not round away.** `peoples-alive-at-bake-end` is discrete and bounded at
five, the roster's own size, and the baseline sits at that ceiling in 76.7%
of worlds already. "M3 rises" was close to unfalsifiable *upward*: in
twenty-three of thirty seeds the metric could not rise, because all five
peoples were already alive. So the null is really two claims of unequal
strength. **"Does not rescue worlds from extinction" is strong** — six
extinction seeds at baseline, six live, the identical seed set, a detectable
effect measured at exactly zero. **"Does not improve diversity in surviving
worlds" is untested**, because the instrument is saturated at its ceiling in
twenty-three of the twenty-four surviving worlds. The spec asked for a second
half of M3 — the effective-diversity reading `coexist.rs` already computes in
space, which would have headroom inside an all-five-peoples world that a bare
count cannot see — and only the count was ever wired up. Building that half
now, immediately after a disappointing count, would have the *shape* of
metric-chasing even with clean logic behind it, so it is deliberately
deferred to whichever campaign answers this chapter next, with its own
headroom declared in the preregistration before any code exists.

**Rescore.** The sigmoid wager's confidence stays **low**, but the character
of the "low" has changed, and the gradient should say so precisely: it was
*unmeasured* when the bet above was struck; it is now *measured on one axis
and null there*, with the other, headroom-bearing axis still unmeasured
rather than merely undiscussed. That is a materially weaker position for the
multi-axis thesis than "unmeasured" was, and a materially stronger one than
"falsified outright" would be — decision 0096 clause 1 is not itself
falsified by one mechanism's failure to move one metric, but it has now spent
its cheapest test and has one clean finding to show for it: position, alone,
is not the term that holds diversity open here.

**Re-measured after the epoch (2026-08-02).** Position-aware conflict draws
no new stream, but it changes every world's committed history, and the
`history/bake` label was bumped to `/v2` to say so honestly (decision 0006).
That re-mints every draw a second time on top of the mechanism's own effect,
so the numbers above were re-measured on a fresh matched pair taken entirely
on the post-epoch derivation rather than trusted to still describe the
shipped world. Neither null moved: M3 is still falsified, M2 still stayed
geometric, and the extinction set is unchanged in both identity and size
across the epoch — the strongest form the "does not rescue from extinction"
half of the decomposition above can take. **This rescore is unchanged and
stands as written.** One thing about *how* the null holds did shift: where
the pre-epoch reading found a single seed accounting for the whole M3 delta,
the post-epoch reading finds two seeds moving in opposite directions that
cancel exactly — the mechanism is visibly live at the individual-world
level, it simply does not net into more diversity. Full numbers:
`docs/superpowers/plans/the-contour-baseline-v2.md`.

**The saturation half of that decomposition has now been tested, by a campaign
that shares none of its nouns, and it survives.** The argument above turns on a
number that was never a property of the world: the diversity count is bounded by
*the roster's own size*, so with five peoples in play "M3 rises" was close to
unfalsifiable upward. The obvious reading of that is that the ceiling was too
low — that a larger roster would hand the instrument the headroom it lacked.
[The Delvers](./chronicle/the-delvers.md) (2026-08-07) raised the ceiling by half
again, from six settling peoples to nine, and **the saturation did not move**.
Across the same thousand-world census: before, 971 of 1000 worlds ended the bake
holding every one of the six, mean 5.961; after, 967 of 1000 hold every one of
the nine, mean 8.956. Three more peoples, three more survivors, and the share of
worlds pinned against the ceiling fell by four tenths of a percent.

That is a finding about the **instrument**, not about the mechanism — no second
contest axis was added and nothing here retests decision 0096. But it closes off
the cheapest hope this chapter had for the untested half of the null. The count
was not saturated because the roster was small; it is saturated because
extinction is rare in this world at any roster size, and a metric bounded by the
roster will therefore sit on its bound however far the bound is moved. The
effective-diversity reading the spec asked for and never wired up is now the
*only* way to test "does not improve diversity in surviving worlds," rather than
one of two, and enlarging the roster is struck off as an alternative. Its
headroom must still be declared in a preregistration before any code exists.

A related bound was found to have the same defect and was repaired in passing.
The coexistence calibration's ceiling on per-cell claimed diversity had been
frozen as a bare `3.0`, justified in its own text as comfortably below
undifferentiated sharing — where the diversity reading approaches *the species
count*. So `3.0` was never an absolute quantity either; it was three quarters of
a four-species roster, with the dependency compiled into a literal and invisible
until a second roster size existed. It is now derived from the live count and
reproduces `3.0` exactly at a roster of four, which is the strongest form this
repair can take — a no-op at the roster the bound was written for. The floor
stays absolute, because monoculture drives the reading to one however many
peoples exist; only the ceiling ever scaled. This is a post-unblinding change to
a preregistered bound, made deliberately, and it re-derives the bound's *rule*
rather than fitting its *value*.

*Extended by [The Radiation](./chronicle/the-radiation.md) (2026-08-10), which
raised the ceiling a second time.*

**A third data point, and the saturation still does not move.** The roster went
from nine settling peoples to **fifteen**. On the thousand-world census this
campaign regenerated, mean peoples alive at the end of the bake went **8.916 →
14.854** — 99.1% of the ceiling before, 99.0% after. Six more peoples, six more
survivors, and the share of the bound the metric sits on is flat to a tenth of a
percent. The reading offered above as an explanation now has three roster sizes
behind it rather than two: the count is not saturated because the roster is
small, it is saturated because **extinction is rare in this world at any roster
size**. Enlarging the roster was already struck off as an alternative; it is now
struck off with a two-thirds-larger roster and no movement at all.

The census's own per-cell diversity column — a different reading on a different
study from the calibration bound below, and not to be confused with it — moved
**1.96543 → 1.96710**, a rise of nine hundredths of a percent under a
two-thirds-larger roster. Which is the same story in a second instrument: adding
six peoples to the world changes almost nothing about how many of them share a
cell.

The margin is the part worth keeping. Re-measured on the roster that actually
shipped, the mean per-cell claimed diversity is **3.0101** — so the retired
literal would have failed by one hundredth, which is what a compiled-in
dependency looks like when it rots: not a loud failure that names its cause, but
a hair over a line, in exactly the shape most likely to be read as noise and
quietly re-pinned. Against the derived ceiling the same reading is 25% of
undifferentiated sharing where the original band permitted 75%, so the world is
if anything more differentiated than the bound was written to allow. Both halves
of that sentence are only sayable because the bound was re-derived rather than
re-fitted.

*Extended by [The Muster](./chronicle/the-muster.md) (2026-08-12), which found
that the re-derived ceiling cannot be reached at all, and that the instrument
enforcing this bound had been reading an empty store.*

**A derived bound can rot in the opposite direction, and this one has.** The
repair above replaced a literal `3.0` with three quarters of the peopled count,
which was correct and which the roster's growth has since carried to **13.5**.
The reading it bounds cannot exceed the number of kinds actually *present* in a
cell, and the mean claimed cell holds **6.4351** of the eighteen — pooled over
177 336 claimed cells across five seeds. **The ceiling now asks for more
coexistence than the world puts in a cell at all**, so the band is one-sided by
construction: it can only ever be failed from below, and no perturbation of the
quantity it was written to watch can reach its upper edge. A literal rots by
falling behind its dependency; a derived bound rots by outrunning what the
world can produce, and neither failure announces itself.

**And the guard enforcing it was reading nothing.** The test that asserts this
band built its own component set with two stores left empty — the biome
affinities and the habitat realms — both of which are sparse and read through a
default on absence, so an empty store did not raise; it silently supplied the
null hypothesis. Every authored affinity row in the registry scored identically
inside the one test whose purpose was to notice when they changed. With both
stores live the same five worlds read **2.1128** against **2.3734** blind, and
the decomposition matters more than the total: a third of that movement is
interaction between a *single* realm row and the affinity rows, so the affinity
contribution alone is 7.2% rather than the 11% a naive attribution would claim.

**What that costs this chapter's confidence is narrower than it looks, and
sharper.** A repaired guard that stays green proves nothing, so it was
mutation-proven — and the proof is an existence proof rather than a
demonstration that the band tracks the quantity. At today's roster the level
alone cannot cross either edge at **any** value; widening the rows to reach
every kind is necessary and nowhere near sufficient; and of three assignments
differing only in which kind holds which ground, all three move the reading by a
similar amount and only one crosses the floor — the *most* differentiated of the
three does not. The bound is a real instrument again. It is not yet a sensitive
one, and the difference is now written down where it will be read.

### A self-scorable bet was scored, and the instrument did not clear its own bar (2026-08-14)

This chapter's axis is **whether the world can grade itself on a claim**, and
its standing hope is that the self-scorable half of a bet closes by instrument
while the taste half shrinks to its honest size. *The Gnomon* is the first
campaign to run that procedure on an instrument rather than on a world, and the
result sharpens the axis in a direction the chapter had not written down.

The anomaly report is maximally self-scorable by construction: it ranks a
world's census columns by how deep each sits in the thousand-world
distribution, and every input is committed and drift-checked. Its usefulness
claim was preregistered as recall@10 ≥ 0.60 against a label the census cannot
supply — perturb one generative constant, rebuild twenty worlds, and ask
whether the columns the perturbation demonstrably moved surface in the top ten.
**Measured: 0.5667 over 120 pairs. Falsified.** All three controls held; the
figure is published a second way (0.35, excluding the two arms whose
perturbation left the census's observed range entirely and could not fail to
rank) because the second reading is the less flattering one.

**The rescore is not to the report's confidence but to the chapter's own
premise.** "The Laboratory can score this" was being carried as though it
implied "and the score will be good". It does not. Being self-scorable makes a
bet *decidable*; it says nothing about which way it decides, and a
self-scorable instrument can be scored and found wanting exactly as readily as
a self-scorable world claim can. That is the mechanism working, not failing —
but the chapter had only ever illustrated the axis with bets that closed
favourably, and one that closed against itself is the more informative
illustration.

Two specifics worth keeping, because both are about instrument design rather
than about this instrument:

- **A tail rank measures unusualness, not change.** These are different
  quantities and the campaign is the first thing to make the difference cost
  something. One injection moved exactly one column in every world and the
  moved value remained an ordinary value in nineteen of twenty; the world was
  different and it was not *strange*. Any future instrument that ranks by
  extremity inherits this gap.
- **The control that passes can be near-unfalsifiable while the headline
  fails.** The campaign's held-out calibration check passed comfortably
  (in-census share 0.7050, held-out 0.7500, ratio 1.0638 against a tolerance of
  2) — and 70.5% of census worlds already carry a column at the flagging depth,
  so a stationary distribution passes it while flagging nothing useful. It was
  preregistered as a control and explicitly not a usefulness measure, which is
  the only reason its green cannot be read as vindication. **Declaring which of
  a campaign's hypotheses is allowed to count as support, before either is
  measured, is what kept this pair honest.**

### An effect can be self-scorable, confirmed, and unattributable (2026-08-14)

The entry above sharpened this chapter's axis by showing that a self-scorable
bet can be scored *against itself*. *The Repose* sharpens it again from a
direction that is neither a pass nor a failure, and the chapter had no bin for
it: **the instrument confirmed the effect and could not find its cause.**

The question was as self-scorable as they come, and was frozen before any
geohazard code existed: do settlements over-occupy high-unrest ground relative
to the land base rate, stratified by elevation, pooled over thirty seeds? Every
input is generated, every comparison is arithmetic on committed counts, and the
readout ships as a drift-checked artifact. It answered cleanly: flat in the
lowest elevation band, and rising with unrest above it — ×1.572, ×2.578, ×5.395
from the calmest decile to the most violent.

Then the design's own disambiguating arm — added precisely because a bare
reading cannot separate *a true null* from *the mechanism was never wired* —
severed both channels through which unrest is known to reach settlement siting,
and the gradient did not flatten. It held at or above baseline in every band:
152.6%, 105.2% and 101.5% of the baseline's excess survived. **Neither modelled
channel carries the effect the instrument measured.** One of them, the hostility
penalty, actively opposes it.

**What this adds to the axis.** Self-scorability was being carried as a property
of a *claim*; this campaign shows it is really a property of a **statistic**.
The statistic here was scorable and was scored. The attribution — *which part of
the model produces it* — is a different question with a different instrument, and
that instrument returned "none of the ones we named". A world that can grade
itself on an outcome cannot thereby grade itself on a mechanism, and a campaign
that conflates the two will report a confirmed effect as though it were a
confirmed explanation.

The honest scope clause is short enough to carry: *unattributed by this
instrument on this roster*. Both halves of it do work. The mineral channel could
not have been refuted here whatever the data said — only two kinds in the roster
read it at all, and they hold 0.398% of the settled population, so severing it
tests the roster and not the channel. **An ablation on a channel almost nobody
reads is a null with no power, and saying so is the difference between a finding
and a claim.** The chapter's standing hope is that the self-scorable half of a
bet closes by instrument; this is a case where it closed, and left a strictly
larger open question behind it than it started with.

### A third category, between self-scorable and taste-gated (2026-08-14)

This chapter splits a bet into a **self-scorable half** the Laboratory can close
and a **taste-gated half** that waits on a human read, and treats naming the
seam as most of the progress. *The Hearsay* ran both halves of one bet at once
and found the split incomplete: there is a third kind of half, and it is the one
that wastes the most time, because it is indistinguishable from the first until
you trace it.

The bet was whether a derived world's committed history carries a legible
epistemic structure — who could know what, and on what grounds. Half of it
closed by instrument, cleanly. Transmission depth is a pure function of the
committed founding tree: 7,778 (event, holder) pairs on seed 42, a median of
four inheritance steps between the community that witnessed an ending and the
community holding the claim, 15.4% beyond ten steps, reaching the tree's full
depth of 21. Two predictions were frozen before the code and both were refuted,
which is the self-scorable half behaving exactly as this chapter hopes.

The other half — whether the communities holding a story are *independent*
sources — looked equally self-scorable. Every input is committed. The question
is pure structure over a tree. No taste is involved anywhere in it. It resisted
three operationalisations, each plausible, each reviewed, each wrong in a
different way, and the third scored its own motivating scenario at zero.

The cause is not difficulty and not taste. **The world lacks the degree of
freedom the concept is about.** Corroboration is agreement between accounts that
could have differed; in a campaign that carries content unchanged by
construction, no two accounts *can* differ, agreement is constant-true, and
every structural measure is a proxy for a quantity with no variance. The
measures were computing redundancy accurately and calling it corroboration.

So the axis gains a third position, and its diagnostic is a question rather than
a category:

- **Self-scorable** — the instrument exists or can be built, and the world
  varies along the axis being measured.
- **Not-yet-scorable** — the instrument is trivial and the world *does not vary*
  along that axis at all. Looks self-scorable, because all the data is committed
  and the computation is easy. Fails as a constant, or worse, as a plausible
  number that never moves.
- **Taste-gated** — the world varies, the instrument is buildable, and the
  reading is a judgment.

The question that separates the first from the second, asked before any formula
is written: **what would have to vary for this number to move?** If the answer
is something the design holds constant, the bet is not self-scorable yet, and no
amount of instrument work will make it so. It is waiting on a mechanism, not on
a measurement.

The practical consequence for this chapter is that a bet can be *demoted* by
this test without anyone having been wrong about it — the Myth program's
corroboration half moves from self-scorable to not-yet-scorable, and its
precondition is now named and dated: it waits on distortion, which campaign 2
supplies. The half that closed did so on the first honest attempt, and both
halves lived inside a single sentence when the campaign opened.

### A criterion can fail and confirm its own hypothesis (2026-08-15)

*The Glasshouse* froze six criteria before writing a line of the physics they
would grade, and four of them passed. The two that failed are the reason this
entry exists, because the chapter has been treating a preregistered set as
something that passes or fails *as a set*, and this campaign shows the useful
structure is finer than that.

The hypothesis was that a cold, uniform population had three separable causes
and was not an attractor. Two criteria tested the *mechanism* — that the
population would retain its temperature spread rather than being flattened onto
a target, and that no biome class would dominate. Both passed, and the first was
designed to be the one a lazy fix fails: a strong enough thermostat can hit any
median by collapsing everyone onto it. The spread held at 34.92 K against a
31.2 K floor, and the largest biome class fell from 65.1% to 25.0%.

One criterion tested a *magnitude*: land the median within five degrees of
Earth's +8.6 °C. It missed by twelve. And the same measurement that failed it
said why, in a form the criterion itself could not have expressed. The habitable
zone is denominated in the square root of luminosity while insolation goes as
luminosity over radius squared, so luminosity cancels exactly and a uniform draw
in orbital *radius* puts the population at a median insolation of 0.748 by
construction. Across the entire range of the thermostat's free constant — up to
*perfect* compensation — the median moves 7.2 K and two-fifths of worlds stay
cold. The shortfall is a property of the draw's measure, and no climate model
can argue with a measure.

**What this adds to the axis.** A preregistered set is not one bet; it is a
mechanism claim and a level claim wearing the same coat. When the mechanism
criteria pass and the level criterion fails, the honest reading is not "the
campaign half-worked" — it is *the model is right and something upstream sets
the level*, which is a strictly more useful result than a clean pass would have
been. The way to keep that reading available is to write at least one criterion
that a successful fix could fail, and at least one that only a wrong model
could.

### A zero can be a scope error wearing a null's clothes (2026-08-15)

The entry above concerns a number that moved less than hoped. This one concerns
a number that did not move **at all**, and the distinction turned out to be the
whole finding.

One of the six criteria asked that the census's dominant soil order stop being
frozen at a single value. It was `leptosol` on all thousand worlds before the
campaign and `leptosol` on all thousand after — after a change that moved the
median land temperature 8.3 K, halved the median land elevation, and cut
ice-dominated worlds by two-thirds. Read as a measurement, that is an emphatic
null: the strongest available intervention, and a response of exactly zero.

It was not a null. The soil classifier's first question asks whether the soil is
shallower than a quarter of a metre or the ground drops more than 300 m to a
neighbour, and answers `leptosol` if either holds. Every question below that line
reads temperature or moisture. Measured over 307,588 land cells, **72.1% never
reach those questions** — 61.09% by depth alone. The classifier that the
criterion was grading was, for three cells in four, never consulted.

**What this adds to the axis.** The chapter already distinguishes a
*self-scorable* bet from a *not-yet-scorable* one by asking what would have to
vary for the number to move. This is the same question asked one layer lower and
it needs its own name, because the failure looks different: here the world
varies, the instrument is correct, and the statistic is still constant — because
the quantity being varied is **downstream of the branch that decides**. A null
of this kind is indistinguishable from a weak effect by inspection, and the only
thing that separated them was attributing the branch instead of inferring it.
The instruction that follows is cheap and general: when a statistic refuses to
move under a large intervention, find the line that decides it before concluding
anything about the effect size.
