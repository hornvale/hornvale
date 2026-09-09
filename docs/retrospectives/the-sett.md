# The Sett — retrospective

Process, not product. The campaign's findings are in
`book/src/chronicle/the-sett.md`; its rulings, measurements and every defect
named below are in `docs/superpowers/ledgers/2026-09-06-the-sett.md`. Two
records came out of it: decision 0906 (what the walk band draws) and
decision 0907 (what a preregistered check has to name).

## The headline number

**Eleven plan defects. Every one of them in the controller's text; none in
an implementer's code.** Six were caught by the pre-dispatch verification
step before a task was ever handed out; five were caught by implementers who
measured the world before changing it. Not one was caught by re-reading the
plan.

That distribution is the campaign's main finding about itself, and it is not
new — it is the fourth or fifth campaign to produce it. What *is* new is
that this one produced enough instances of one shape to see the shape
clearly, and to say what actually kills it.

## The shape: an assertion that cannot fail on the case it names

Four of the eleven were assertions that would have passed **before** the
change they were written to witness.

| defect | the assertion | why it could not fail |
|---|---|---|
| 1 | three tests over a uniform per-face sweep | a uniform sweep at any usable density never crosses a face seam, and the seam is where a wrong raster falls apart |
| 7 | *"the box left of the mark is the facet the compass calls W"* — the campaign's whole claim | sited at a start at latitude −4°, where a compass step and a projected column already coincide; all four cardinals agreed under the old raster, and the stronger form measured 778 of 780 boxes agreeing |
| 9 | the overlay test, sited on the **west** neighbour | at that start N, E and W all agree between the two rasters; only **south** discriminates |
| 11 | *"the miss count on the second draw is about the box count, because the call never consults the memo"* | a call that never consults a memo registers neither a hit nor a miss; the measured value was 0, not 72, and the clause after "because" refutes the clause before it |

A fifth (defect 4) is the adjacent variant: a non-vacuity guard placed in a
test module that could never construct the context the guard reads, so the
guard could not fire at all. A guard that cannot fire is worse than an
absent one, because it reads as coverage.

Decision 0907 records the pattern, with two more instances from two earlier
campaigns. **What matters here is the remedy, and it is mechanical.**

### Naming the pattern does not stop the pattern

Defect 9 was written **after** defects 7 and the two inherited instances had
been diagnosed, **in the same document that diagnosed them**, by the session
that diagnosed them. The diagnosis was four sections above the defect. The
task was then substantially rewritten with the diagnosis in hand, and the
siting survived the rewrite — because the rewrite was correcting *scope*,
and the siting was not what was being looked at.

Defect 11 is later still: it was written after the pattern had been named
four times and had a decision record open on it.

So the rule cannot be "remember this", and a checklist item saying "check
your assertions discriminate" would have been read and passed over five
times. **The only thing that killed any of these was running the assertion
against the unfixed code and watching it fail.** Every red-then-green step
in this repository's plans already asks for that; the four defects above are
what happens when a plan states the assertion but the step's expected
failure is never actually taken.

**For a future campaign:** when a task's assertion is the campaign's claim,
the dispatch must ask for the *pre-change measurement as a deliverable* —
the number, in the report — not for a red-then-green step that an
implementer can satisfy by writing the test after the code. Four of these
were caught precisely because the dispatch told the implementer the plan had
been wrong before, and the implementer measured the old behaviour before
touching anything. That instruction is cheap and it worked every time it was
given.

## A correction's blast radius went unswept three times

| defect | the correction | what it stranded |
|---|---|---|
| 3 | defect 1 widened the test population to include cube corners | two assertions written against the narrow population, asserting properties the campaign had *already ratified as failing* at corners |
| 5 | defect 4 moved a test out of one file and rewrote the task's **Files** section | a step heading three screens below still naming the old location |
| 9 | the placement enumeration shrank and re-scoped Task 4 | the assertion's siting, which the rewrite never looked at |

And a fourth of the same family that is not a plan defect at all: the
campaign's reconnaissance probe (S12) was written by the same session that
had twice warned, in the plan text, *"for an even width there is one more
box to the left of the anchor than to the right — do not assume symmetry"*.
The probe assumed symmetry, printed the symmetric box count on the same line
as the asymmetric label, and that line was read a dozen times without the
disagreement registering.

**The generalisation is not "re-read the document."** Re-reading is what
failed. A correction changes a *claim*, and every other place that claim
appears is downstream of it — including places that state it in a different
grammatical form, in a different file, or as a warning the author then
violates. The sweep has to be on the claim.

**For a future campaign:** after any correction to a plan, name the claim
that changed in one sentence and grep for *the claim*, not for the text
edited. If the correction widened a population, every assertion over that
population is in the blast radius. If it moved a file, every location
reference is. Writing the warning is not applying the warning.

## The instrument that was not committed, and the one that is

The predecessor campaign's three measurement probes appear in no commit —
`git log --diff-filter=A` over all three names returns nothing. The
measurements its decisive decision rested on had no reproducible instrument,
so **every inherited figure had to be re-derived from scratch** before this
campaign could write a line of specification. That re-derivation is what
found the previous campaign's frozen prediction had never been run as
written, and it is what found the option that eventually shipped.

The re-derivation was worth it. Paying for it twice would not be.

This campaign's own instrument is committed, and deliberately not as a
probe: `clients/game/bin/tests/rose.rs` asserts the geometry properties —
the corner fold, the pole fold, the coherence, the clearing distances —
against the **shipped** raster rather than against a probe's
re-implementation of it. Its two throwaway probes were then deleted, with
each of their outputs checked off against where that finding now lives (the
chronicle, the module doc, or a committed test) rather than deleted on the
assumption that nothing durable was in them. One durable *shape* — reporting
memo hits and misses beside the wall time — was carried into the committed
bench.

**For a future campaign:** a probe that establishes a decision is evidence,
and evidence that only exists in a transcript is not evidence a successor
can use. Either promote it into an assertion over the shipped object, or
commit it. Deleting it silently transfers its full cost to whoever inherits
the question.

## The option that won was invisible to the measurement that chose against it

The predecessor campaign measured two candidate rasters carefully, on the
right axes, and chose correctly on the evidence it had. The option that
shipped here was never on its list — and could not have been found by taking
more of the same measurements, because **the winning option is
byte-identical to the loser on the four equatorial faces**: 18,432 of 18,432
words agree. Every equatorial measurement returns the same number for both.
They differ only on the polar caps, which are the other third of the world.

That is not a diligence failure. It is a property of the sampling: a
comparison run on a region where two candidates coincide cannot see a third
that also coincides there.

**For a future campaign:** before reading any comparison of options, ask *on
what fraction of the domain can these options possibly disagree*, and make
sure the population includes it. A number taken where the candidates are
identical is not weak evidence; it is no evidence, and it is
indistinguishable from strong evidence in the report.

The same question, asked of an *assertion* rather than of a comparison, is
decision 0907. They are the same discipline pointed at two different
artifacts.

## A figure relayed into a committed record without being re-derived

An implementer reported, in its own summary, that a file's guarded token
count had gone from 44 to 43. That number was copied into the campaign's
committed ledger and **an instruction for a later task was built on top of
it** — rebaseline the inventory down to 43.

The count was 44 the whole time. Running the guard's own tokenizer against
the committed file gives 44. There was never any slack.

The failure mode is worth keeping because of how quietly it would have
landed. The guard fails only on *growth*, so it passes at 43 or 44 and
nothing would have contradicted the claim. And a rebaseline **writes the
inventory from the tree**, not from the claim — so the corrective action
would have "succeeded", reported success, and proved nothing. A number that
is wrong, unfalsifiable by the tool it describes, and self-ratifying when
acted on.

**For a future campaign:** a number that is about to become an instruction
gets re-derived from the instrument, not copied from a report. Relaying a
subagent's figure into a durable record is authorship, and it inherits the
author's obligations.

## Two agreeing reconnaissance passes are not two pieces of evidence

The specification stated that a function's unreachability was *"re-confirmed
independently"* — two agents, separately dispatched, both concluding it was
dead. Both were wrong, and the plan accordingly ordered the deletion of live
code (six tests red under mutation, two of them irreplaceable guards) while
protecting a genuinely dead sibling. It was wrong **in both directions at
once**.

Both agents traced the driver, because the driver is what the question was
framed around. Neither enumerated the callers of the call site's own
function — and the crate has a second public entry point that takes exactly
the branch the driver cannot. The word "independently" made one blind spot,
sampled twice, read as corroboration.

The closely related defect 6 is the same method producing the opposite
error: a gap was *claimed* by reading what a function consumes rather than
establishing who calls it, and folded into a task that needed nothing. It
made the plan too big; the same reasoning applied to a function that really
did need changing would have made it too small, silently.

**For a future campaign:** independence is a property of the *instrument*,
not of the worker. Two agents handed the same framing sample the same blind
spot. If a second pass is meant to corroborate, it has to be given a
different question — here, "enumerate every caller of this function",
which is a mechanical query, rather than "confirm this is unreachable",
which is an invitation to re-derive the first agent's argument. And
**reading what a function consumes never tells you when it runs.**

The implementer refused the deletion instruction and proved the reachability
by mutation rather than by argument. That refusal is the campaign's single
most valuable implementer action, and it happened because the dispatch said
the plan had been wrong before.

## What the preregistration bought, including the part that was not carried out

The cost prediction was **falsified at 25×** — 9.71 ms against a frozen
1.0–2.0 ms band. That is the headline, and it stays the headline: the
post-fix figure of 3.98 ms is a separately labelled remediation
measurement, not the prediction coming true, and the ledger says so in its
own words rather than relying on a reader's charity.

The falsification was more useful than a confirmation would have been,
because the *branch table* was frozen alongside the prediction. A prediction
can be wrong; a table covering the responses to each outcome band cannot.
The ">5 ms" row's response was **stop and find out what is being paid per
frame before optimising anything**, and following it found that 65% of the
redraw was one line that had nothing to do with the change under test — a
cost the *old* path pays too, which a cache had been hiding since the
campaign that introduced it.

Had the result landed one band lower, the frozen response would have been an
incremental fill — and the same measurement shows that would have attacked
6% of the problem while looking like it worked. That is recorded (S24)
rather than quietly skipped: **a preregistered response that is not carried
out is exactly as much a finding as a falsified prediction**, and skipping
one silently is how a preregistration decays into decoration.

**For a future campaign:** preregister the branch table, not just the
number. And when a band's frozen response is superseded by the measurement
that placed you in that band, write down that it was superseded and why.

## One inherited defect worth carrying forward on its own

A committed test asserted that a plate's mesh-search count is bounded by the
mesh rather than by the screen. It was true of the searches it could *see*.
An earlier campaign then added a code path that bypassed the memo, and
therefore bypassed the counter the test reads: the plate was running 8,200
searches while the test observed 8, a factor of 1,025, green the whole time.

**The expensive path had not stopped happening. It had stopped being
counted.** A test that measures work through an instrument the work no
longer passes through reports zero and reads as a strong result.

The restated assertion has three parts, two of which were previously
invisible, and the third — *a redraw of the same plate must cost nothing* —
is the one that would have caught it.

## What went right, briefly

- **Implementers measured before changing.** Five of the eleven defects were
  caught this way, and one implementer took a positive control on a green
  result and found the parameter it was testing was load-bearing after all.
- **Positive controls found a test that could not fail.** A memo-bounding
  test's own doc claimed it caught a clear-every-frame mutation; under that
  mutation it stayed green, because an entry count cannot distinguish a memo
  refilled every frame from one filled once. The hit counter can, and the
  test now asserts on it.
- **A scope refusal was ratified rather than argued.** The known-but-unfixed
  projection defect stays unfixed, with its blast radius measured and
  recorded, because fixing the client's copy alone would make two pictures
  of one neighbourhood disagree. That is now a registry row with numbers in
  it, not a note in a transcript.
- **The corner presentation came from an ideonomy overturn**, not from
  precedent: the inherited design preferred a repeated box to a blank one,
  and the re-instantiation inverted it — a blank at the one place the
  surface actually folds is a local honest feature, and the simulation
  already had the sentence for it.
