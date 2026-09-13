# The Cadastre — retrospective

Process, not product. The product is in
[the chronicle](../../book/src/chronicle/the-cadastre.md): a corpus completed
from a forty-one-item sample to its closed three-hundred-and-one-item
population, and the rule that makes closure mandatory for every future corpus
drawn from a linked catalogue. This is about how that was built, and its
defects cluster unusually tightly: **almost every one is a claim about
something the session had read but not interrogated.**

## 1. A constant's definition is domain-neutral; only its callers say what it is for

The campaign's worst error was reading `TechHorizon` — the four-rung
technology ladder — and concluding it was Hornvale's model of what living
peoples can do. Two separate arguments were built on that reading: a
five-hundred-year selection threshold derived from where the ladder tops out,
and a verdict-vocabulary argument about what the world "deliberately lacks".
Both went into the spec. Both survived three gates.

The correction came from Nathan, and verifying it cost one command. Every
consumer of the horizon is occupation- or vestige-side: it labels an abandoned
settlement's era, dates a past occupation by year alone, stamps what a ruin
leaves behind. **There is no living-world technology model at all.**

What was read: `tech_for`, `tech_weight`, and the enum's own definition. What
was never read: **a single caller.** The definition says *"Stone-tool,
pre-metal"* — a description of a technology level, domain-neutral, applicable
to anything. It is the call sites and only the call sites that say the subject
is the dead.

This is a lesson the project already holds — *a constraint read off a
construction site is a hypothesis* — reproduced exactly by a session that had
it available. Two things make it worth restating rather than filing as a
repeat. First, the error is **invisible to review**, because the definition
supports the wrong reading perfectly well; a reviewer checking the argument
against the definition finds agreement. Second, and decisively: **two
independent scoring agents reproduced it from the code alone**, without having
seen the correction, each scoring items `present` against a mechanism that
dates ruins. Three readers in one campaign, misled in the same direction, is
not three careless readers. It is a property of the code — a type whose name
and doc comment do not name its domain — and the finding belongs to the type,
not to the sessions.

The operational rule: **when a constant's meaning decides an argument, read a
caller before writing the argument.** Not the definition, not the helper
function, not the doc comment. One caller.

## 2. A de-risking probe against ground truth that already existed killed a ratified design for the price of one script

The campaign's first ruling adopted a census of the whole catalogue — around
fifteen hundred items. The obvious economy for scoring it was a keyword sieve
over the project's idea registry. Before adopting the sieve, it was run blind
against the previous campaign's forty-one hand-scored items, whose six
`deferred` verdicts are a real answer key someone else had produced. It
recovered three of six.

That measurement withdrew a decision that had already been ratified, in the
same session that ratified it, and it cost one script and a few minutes. Three
properties made it cheap, and they are the transferable part:

- **The ground truth already existed.** It was not built for the probe; it was
  a neighbouring campaign's committed output. A probe that must first
  construct its own answer key is a different and far more expensive object,
  and usually gets skipped.
- **The probe ran before the plan was written**, so what it invalidated was a
  paragraph rather than six tasks of executed work.
- **The verdict was disqualifying for a reason recall alone would not give.**
  Fifty percent recall reads as "weak, maybe tunable". The finding was that
  *every miss landed on the flattering answer* — an instrument whose error is
  unbiased is noisy; one whose error runs entirely toward the result the
  campaign wants is broken. The direction, not the rate, is what closed it.

The second-order ruling is the one most likely to be forgotten: **tuning the
sieve against those six was refused too.** Six positives is the only answer key
this campaign would ever have, and fitting a threshold to it consumes the
control. That refusal is worth more than the measurement, because it is the
step a session under cost pressure will always be tempted to take.

**Look for a pre-existing answer key before designing a method.** If a
neighbouring campaign hand-produced the output your cheap method is meant to
approximate, the probe is nearly free and it is decisive in one direction.

## 3. A pre-flight dependency table that models data flow is blind to a gate that spans two tasks

The plan sequenced Task 4 (discharge the sibling corpus's cross-corpus
obligation) after Task 3 (score the new items). Task 3's implementer had to do
Task 4's entire deliverable in order to finish Task 3, and flagged it as
outside its brief.

It was unavoidable. Moving sixty-four items off `absent` made the widened
corpus cite nine registry rows no sibling had ever mentioned; the family's
cross-corpus rule then reddens the sibling; and the commit gate runs that check
over both corpora. **There is no commit in which Task 3 is done and Task 4 is
not.** The two tasks cannot occupy separate commits, and no amount of
re-ordering fixes that — only merging them does.

The plan had a pre-flight dependency scan, and **it scanned this exact pair and
passed it.** The row reads: *"clean; T4 Step 1 derives them from the resolver,
not from T3's report."* That is a correct statement about the interface. Task 4
does not consume Task 3's output; the data flow is genuinely clean. The
question the table never asks is whether the two endpoints can be *green at
the same time in separate commits* — which is a question about the gate, not
about the data.

The generalisation: **a dependency table models what flows between tasks, and
a gate is not a flow.** A gate is a predicate over the whole tree, evaluated at
every commit, and it can bind two tasks that exchange nothing. The additional
column a dependency scan needs is not "does B read A's output" but **"is there
a commit boundary at which A is complete and B is not, and does anything red
there?"**

## 4. Rejecting an anchor reopens a verdict; it does not decide one

Auditing the six scoring batches produced two rules: an anchor into the
ruin-dating machinery cannot support `present`, and a registry row that plans
the acquisition *mechanism* rather than naming any capability cannot support
`deferred`. Between them they invalidated the anchors on nineteen items.

The cheap move — and the one that felt like bookkeeping — is to write `absent`
into those nineteen slots, since the anchor that held them above `absent` is
gone. It was refused, and the nineteen were dispatched for an independent
re-score with an explicit instruction that `absent` was **not** the expected
answer.

**Fourteen came back `deferred`**, on anchors that survive both rules. A
controller-written `absent` would have been wrong on fourteen of nineteen —
and wrong *invisibly*, because a searched `absent` and an unsearched one are
typographically identical, sitting inside an artifact whose entire worth is
that its verdicts were searched for.

The distinction is exact and generalises past this family: **an anchor is a
justification, not a verdict.** Removing the justification returns the item to
the unscored state it was in before anyone looked; it does not carry it to the
default. Any rule that invalidates evidence must reopen the conclusions that
evidence supported, and the session that authored the rule is the worst
possible one to re-decide them — especially here, where both rules moved items
toward the campaign's own flattering result. That asymmetry was recorded rather
than argued away; the re-score is what kept it from being a thesis confirming
itself.

## 5. Twice, a confirming number measured the instrument rather than the world

Both instances are the same defect at different altitudes, and the second
happened *after* the first had been written down.

**The parser.** The spec offered, as evidence the source catalogue was stable,
that a third independent fetch had reproduced the prior count of 1,484 exactly.
The fetch was independent; the parser was not — the same ASCII-only slug
pattern was reused, and it silently drops two real items (one with a comma in
its slug, one with an `ö`). The true count is 1,486. Re-running the same
pattern could only ever confirm the pattern.

**The control.** A blind re-score of the forty-one arc items returned exactly
thirty-five `absent` and six `deferred` — the target aggregate. It had read it:
the idea registry carries a row restating that scored tally, and the scoring
procedure *requires* every scorer to read that file. The aggregate confirmed
nothing. The item sets differ on four.

The shared shape: **a number offered as reassurance was produced by the thing
being checked.** The registry case is structurally worse than the parser case,
because the spoiler sits in a file that cannot be withheld — a blind control in
this family is not currently constructible, and that is recorded as a defect in
the registry's own conventions rather than in this campaign. A row that
restates a corpus's scored tally is a permanent contaminant of every future
control over that corpus.

The practical test is cheap: **before quoting an agreement, name what would
have had to differ for it to disagree.** "A second fetch of the same page,
parsed the same way" answers nothing. "A second scorer who could read the
answer" answers nothing.

## 6. Two smaller ones worth keeping

**An estimate stated beside the decision not to measure it reads, later, as a
measurement.** The predecessor's provenance says *"the full transitive closure
was not measured and is not proposed; it would plainly exceed 80"* — the first
clause is exactly honest and the second does not follow from it. Measured, the
arcs' closure is seventy-seven, inside the very band the sentence cites as the
reason to skip it. The lesson is not about carelessness; it is that the
disclaimer and the number sit in one sentence, and readers carry the number.

**The plan's self-review found two gaps of one shape, and both were spec
requirements the plan restated instead of implementing**: no task wrote the
corpus's `provenance` (six tasks said "record it in provenance"; none created
it), and the edge-completeness success criterion was verified by nothing —
closure guarantees no dropped edges, so the plan asserted it rather than
checking it. Both are the project's recorded *clause vacuously satisfied*
pattern. The self-review question that found them is worth reusing verbatim:
**for each numbered requirement in the spec, name the step that implements it —
not the step that mentions it.**

## What it did not get wrong

The resolver, the loader and the report were untouched; nothing in this
campaign's code was defective, which is the same shape the predecessor
reported. The defects are in prose, in plans, and in what was inferred from
reading rather than running. That is now two consecutive campaigns in this
family, and it is a strong enough prior to act on: **on a corpus campaign,
budget the review effort against the specification and the selection rule, not
against the code.**
