# The Convalescence — retrospective

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-convalescence.md).

A small campaign: one new member of a Lab metric's report, one bound moved, four
tests. Almost all of its value is in *how* the finding was reached and in what
made a bound demotion safe to land, and both generalize past this metric.

## The finding came from the originating spec, not from the failing test

The visible symptom was a red assertion in a calibration test. The instinct a red
assertion produces is to study the assertion, the world it runs on, and the value
it read — all of which are downstream. What actually resolved it was going back to
the *design document that defined the quantity* and reading the paragraph the test
was implementing. That paragraph stated the signal as a conjunction, twice. The
test asserted one conjunct.

Nothing in the failing test's own output could have revealed that, because the
test agreed with itself: the code, the assertion, and the value it produced were
all consistent.

The honest version of the finding is a step more awkward than "the test drifted."
That same §8 *labels* chronicity "the bug alarm" in its family bullet list, in as
many words — so the control was faithful to the spec's label and unfaithful to the
spec's reasoning, and the disagreement was not between the test and its mandate
but *inside the mandate*. The campaign resolved it toward the reasoning, because
the discriminator paragraph and the evidence battery both state the conjunction
and both give the reason recovery matters, while the label gives none. Only
reading the section end to end shows that there was a choice to be made at all.

**The rule:** when a preregistered control fails, the first document to open is the
one that defined what the control is for — not the control. And read the whole of
it: a one-line label and the paragraph explaining it can point in different
directions, and when they do, the fix is an argument about the spec rather than a
lookup in it.

## A comment and its assertion had drifted apart, and that is greppable

The strongest evidence for the diagnosis was already sitting in the file. The
comment block above the failing assertion described the alarm as the conjunction,
in prose — and the assertion below it checked half of that.
*the-living-community* had written that comment while narrowing a *different*
bound, got the shape of the philosophy right in words, and did not carry it all
the way into the check.

It got the shape right and the **scope** wrong, which is the sharper half of the
lesson. The comment (`windows/lab/tests/health_calibration.rs:84-85`) spells the
alarm out as "`chronicity > 0` and `recovery_ticks == None`" — a conjunction
between a per-creature fraction and a population mean, which is exactly the
masking-prone form this campaign rejected on scope grounds (below). So the prose
was not a correct check merely left unwritten. It was a *different* wrong check,
one an implementer could have transcribed faithfully into an assertion and ended
up worse off than the drift they were repairing. **A comment that outruns its
assertion is a lead, not a specification** — it tells you where to look, and it
still has to be checked against the spec before being promoted.

That is a specific and recognizable failure mode: **a comment that states a
stronger invariant than the assertion beneath it.** It is worse than an absent
comment, because it makes a reader (and a reviewer) believe the check is stronger
than it is — the comment supplies the confidence and the assertion supplies the
coverage, and nothing forces them into contact. It is also *findable*: an
assertion whose neighbouring comment contains a conjunction ("and", "AND", "as
well as", "never ... while") but whose expression contains none is worth a look.

Related: this is not the first recent campaign whose real defect was a test not
checking what it claimed — *The Waterline*'s retrospective names two of its own,
and concluded that a test that passes looks identical to a test that works. This
campaign adds the variant where the comment says the right thing and the assertion
does not. The pattern is not that tests are missing. It is that tests *look*
correct at exactly the point where they are weakest.

## A red control that blocks work is the highest-risk moment in the process

The demotion of a bound, at the moment a red bound is in someone's way, is exactly
the shape of rationalized test-loosening. Recognizing that is not enough — the
feeling of being sure one's reasoning is honest is available to a person whose
reasoning is not. What made this landable was **structural**, and it is worth
naming as a reusable procedure:

1. **Justify from the defining spec alone.** The argument for the new bound cites
   the design document that created the metric and nothing else. If the argument
   needs the blocked work to be persuasive, it is not an argument.
2. **Validate on the metric's own synthetic scenarios.** The alarm must be shown
   still *live* — a scenario where it fires — not merely still green. Green is what
   loosening buys.
3. **Never consult the blocked work's numbers.** Not to sanity-check, not to
   confirm. A number seen is a number that can steer.
4. **Verify the unblocking last, separately, and as a consequence.** It is not a
   success criterion. If the fix had turned out to unblock nothing, nothing in the
   campaign would have changed.
5. **Measure whether anything was actually loosened.** Here the old and new bounds
   read identically on every measured world — which is a *finding*, and the single
   most load-bearing sentence in the record.
6. **Write it down where an audit looks, under its own title.** A test comment is
   invisible to the question "did this project ever loosen a bound to unblock
   work?" A clause inside an entry about something else is better but still only
   reachable by someone already content-grepping for it — which is what the
   precedent here got: decision 0073 records the `prevalence` demotion as its
   "pin invariants, not values" example, under a title about epoch granularity. A
   dedicated entry is reachable from the index (decision 0080). Findability is a
   spectrum, and an entry's title is most of it.

Steps 1–4 are discipline; 5 and 6 are what make the discipline *checkable by
someone else later*, which is the only kind that survives.

The corollary that generalizes: **a bound demotion occasioned by blocked work owes
a decision entry of its own; one occasioned by ordinary drift is adequately served
by a clause wherever it comes up.** The precedent this campaign extended got the
latter, and that was proportionate to its circumstances. Circumstances determine
not just whether the record exists but how findable it has to be.

## "These two published numbers agree" is not a composition rule

The obvious fix — conjoin the two numbers the metric already reported — was wrong,
and wrong in the dangerous direction: it would have converted a false positive
into a false *negative*. The reason was a **scope** mismatch, not an arithmetic
one. One number was per-creature, the other a population mean, and conjoining
across that boundary lets one bad member hide behind the aggregate.

Nothing about either number's name, type, or range signalled the mismatch. Both
are ratios in the same struct with the same units. **Before combining two
aggregates, state what each one is aggregated over, out loud.** If the answers
differ, the conjunction is not the one you think you are writing.

The generalizable check on the *direction* of a wrong alarm: for anything whose
purpose is to detect a fault, ask which way it fails. Noisy is a cost; silent is a
defect. They are not symmetric, and a fix that trades one for the other is a
regression wearing a repair's clothes.

## What went smoothly, and why

The 2×2 (length × fate) was written down before the code, and it did two things
cheaply: it produced the test list mechanically — one test per cell — and it made
the deliberately-silent cell an *explicit design element* with a test guarding it,
rather than an omission a future reader would try to fix. **Enumerating a small
product space is a cheap way to turn "did we handle everything?" into a table you
can point at.**
