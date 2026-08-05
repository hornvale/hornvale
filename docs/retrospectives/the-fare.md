# Retrospective — The Fare

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-fare.md): weather's effect on
travel is a tail phenomenon that median statistics erase, it cannot be routed
around, and The Mire's polar zero survives an independent instrument.

## The dominant lesson: a frozen statistic can falsify a true hypothesis

The Mire's retrospective found that every one of its defects originated in
plan text. This campaign's central defect originated one level higher — in
the **preregistration itself**.

F1 was frozen as a *pooled median*. The word "median" was never argued for; it
was inherited from the predecessor's convention and written into the freeze
without anyone asking what shape the phenomenon was likely to have. Weather
catastrophes are tail events. A median is the one statistic guaranteed not to
see one. The floor was cleared by the ninety-ninth percentile at every band
and missed by the median at all of them — so the campaign preregistered an
instrument that could only return one answer, and then reported that answer
as a property of the world.

The mechanical failure is worse than the conceptual one: **the primary readout
computed no percentile at all.** Not "reported the median and buried the tail"
— the tail did not exist as a number anywhere in the geographic frame. The
secondary settlement-frame readout happened to carry a `max`, and that stray
figure (0.111894 pre-merge, 0.114233 after absorbing main, against a floor of
0.05) is the only reason the error was catchable at all.

It was not caught by review, by the implementer, or by the controller. It was
caught by the project owner reading the conclusion and saying it could not be
true — *"Donner Party, McFly?"* Domain intuition applied to a stated
conclusion found in one sentence what four reviews and a preregistration
had not.

**What to do differently:** when freezing a hypothesis, freeze a *distribution*,
not a point. Report p50/p90/p99/max as a matter of course, and state which one
the floor gates and why. If the phenomenon could plausibly be heavy-tailed,
a median floor needs an explicit argument, not an inherited convention.

## The refuted hypothesis was worth more than the confirmed one

Offered two explanations for the small median — that the tail was hidden, and
that the router was dodging weather with perfect foresight — this campaign
measured both rather than picking the plausible one.

The first was right. The second was **wrong, and its refutation is the better
finding**: committing to a summer route costs about half a percent more than
re-planning daily. Foresight is nearly worthless, because alternatives cost
11–18% more and detouring costs about what enduring costs.

That result exists only because a hypothesis the controller found persuasive
was written down as a testable quantity instead of asserted as an
explanation. It would have been very easy to publish "F1 is small because the
router avoids weather" as a mechanism; it is false.

## Six defects, and where they came from

| # | Defect | Origin |
|---|---|---|
| 1 | The pilot measured a fixed-day markup, not the spec's seasonal swing | plan text |
| 2 | `SAMPLE_DAYS` declared and never read — the signal the above was wrong | plan text |
| 3 | Adjacent pairs read redundancy `1.0` by construction | plan text |
| 4 | The sweep returned no predecessors, so path identity was unmeasurable | plan text |
| 5 | F1 frozen on a median against a tail phenomenon | **the freeze** |
| 6 | E3 selected the flattest cell, not the hardest pass | **the fix for 5** |

All six originated upstream of implementation. Two are notable beyond the
count. Defect 5 is the first in this project's recent record to originate in a
*preregistration* rather than in a plan — a document specifically written to
be careful. Defect 6 originated in the correction for defect 5, drafted
quickly under the pressure of a discovered error: asking for the route cell
with the largest surcharge *as a fraction of its own cost* preferentially
selects the cell with the smallest cost. The recurring maximum of exactly
1.875 across every band gave it away — `30/16`, a near-flat cell at full
surcharge.

**A correction written in a hurry is not exempt from the discipline that
caught the thing it corrects.**

## A reader's question outperformed four reviews

Twice. The terrain-weighting question — *"do we have terrain weighting of
edges for travel purposes?"* — surfaced the campaign's central design fact:
corridors are surveyed on the dry field and weather applied afterwards, so
measuring re-routing over the weathered graph would have had its ceiling set
by the instrument. That moved the whole measurement one layer down, after the
spec had already been written and self-reviewed.

The Donner objection then overturned the headline.

Neither came from the review machinery, which was working well and caught
much else — a vacuous keystone under mutation, an unquantified redundancy
artifact, a prose overclaim about monotonicity, and an empty-band hypothesis
about the polar zero that turned out to be answerable from the code. Reviews
verify that the work does what it says. They do not ask whether what it says
could be true of the world.

## The hedge against a parallel campaign was tested within hours

Mid-campaign the sampling frame was moved off settlement pairs and onto
deterministically sampled land-cell pairs. Two reasons were given: it removed
a dependency on The Keeping, which was about to re-place every settlement, and
it controlled route length, which this campaign's central claim depends on.

The Keeping then landed — fourteen commits, including the world-identity step
— while this campaign was still open. The re-run is the cleanest possible
verdict on the hedge: **every geographic figure is byte-identical at the full
two-hundred-seed population**, across F1, F2, F3 and both F-mono arrays, while
the settlement-frame statistics moved by 1.5–2.1%.

A design decision taken against a hypothetical was tested against the real
event within hours of being written, and held. The transferable part is not
"re-frame away from settlements" — it is that when a measurement's sampling
frame is coupled to something another campaign owns, that coupling is worth
paying to remove *even when* the parallel campaign looks stalled. The Keeping
was stopped by its own Task 0 when the hedge was taken; it landed anyway.

**And the reason given for the re-frame was wrong while the decision was
right.** The stated rationale — that settlements sit on river basins and so
under-sample the marginal ground where weather bites — was falsified by the
pilot: settlement-frame F1 came in roughly *double* the geographic figure, not
half. High-capacity basins are the moist ground, and routes between them
follow the lowlands where mud lives. The decision survived on its other leg,
route-length control, which was always the stronger argument. A right call for
a wrong reason is still worth auditing, because the wrong reason will be
reached for again.

## Re-measuring found almost nothing, and was still correct

The Mire's published figures were re-measured rather than pasted, on the
reasoning that The Generalist and then The Keeping had changed the peoples
roster and settlement placement, which are upstream of what H1 reads.

They had barely moved. H1 measured 0.0095 exactly as published; H2's temperate
and polar bands matched exactly; only the equatorial band differed, 0.0225
against a published 0.0224. The hypothesis that motivated the re-measurement
was essentially wrong.

It was still the right procedure, for two reasons. The 0.45% equatorial gap is
real and would otherwise have been pinned wrong — and it can no longer be
attributed, since nothing distinguishes "step B moved it" from "the chronicle
rounded it." Pinning against a re-derived number rather than a quoted one costs
one run and removes that ambiguity permanently. More importantly, "the numbers
did not move" is only knowable by looking; assuming it would have produced the
same pin with none of the confidence.

## Operational findings

- **A knowingly-red gate was inherited and is being repaired.** The Mire's
  preregistered test asserts hypotheses its own chronicle records as
  falsified, so `make gate-full` has been red since it merged. A red gate stops
  being a signal for everyone downstream. Both campaigns' falsified hypotheses
  are converted from *claims* to **pinned witnesses** — asserting the measured
  falsification with a tolerance, so the test reddens if the number moves
  rather than standing permanently red. A falsified hypothesis should be
  pinned, not left failing.
- **Re-measure before pinning.** The Mire's published figures predate The
  Generalist landing humans on the peoples roster, which changes settlement
  placement, which is upstream of what H1 reads. Pinning against pasted
  chronicle numbers would have re-pinned a claim rather than a witness, and
  could have frozen a stale figure.
- **Fix rounds and scope changes are different things and must be counted
  separately.** Task 3 took three fix rounds and two scope changes. The
  scope changes were the controller revising requirements, not an implementer
  failing to converge; counting them against the five-round cap would have
  tripped an escalation to a more capable model for no reason.
- **`type-audit check` passing does not mean the committed report is fresh.**
  After absorbing main, `check` exited 0 — correctly, since 0103 required no
  new tags here — and that was reported as "the type audit is clean." The
  committed `docs/audits/type-audit-report.md` was nonetheless stale, because
  `check` is a lint and the report is a separate drift-checked artifact. The
  pre-commit hook caught it. The two questions are different and only one of
  them was asked.
- **Two plugin versions of the same helper script shipped incompatible
  argument orders**, and `find | head -1` selected between them
  nondeterministically across invocations. Pin the version path.

## Confidence Gradient

`book/src/open-questions.md` was checked against this campaign's territory.
No bet moved: nothing there stakes a claim on travel cost, route choice, or
the statistics of weather's effect on either.

## Follow-ups

- **There are no bad years.** The substrate spins up to an annual fixed point,
  so every year is the same year and an exceptional winter is inexpressible.
  History's worst journeys happen in outlier seasons. This is the single most
  promising direction the campaign leaves open, and it is the one cause of the
  small median that remains untested. (`CLIM-spin-up-assumes-periodicity`.)
- **The surcharge's calibration on steep ground is unmeasured**, because the
  measurement built for it selected on the wrong axis. Wants a surcharge
  fraction conditioned on terrain — cells in the top decile of dry cost —
  rather than maximised over it.
- **The cost field has no ground-softness term.** Promoting the weathered
  field into production would change which corridors exist in every world.
  (`CLIM-cost-field-has-no-softness`.)
- **F4 moved to the wiring campaign**, which is where its warrant always
  belonged.
