# The Action Clock — retrospective

Process lessons only. The product is in
[the chronicle](../../book/src/chronicle/the-action-clock.md).

An unusual shape: six tasks built in one session, a preregistered control fired
at the merge gate, the campaign **parked** rather than pushed through, a
different campaign repaired the instrument, and this one resumed and closed
against a baseline re-frozen from a main that had moved a hundred and twelve
commits in between. Most of the lessons are about the park and the resumption.

## A campaign may not repair the instrument that is judging it

This is the lesson the campaign exists to teach, and it held under real
pressure. The failing control's own comment defined the alarm as a conjunction
while asserting only half of it, so a two-line edit — one that could be argued
for entirely from the control's own documented philosophy, and which a later
campaign did in fact make — would have turned the gate green.

The argument for making it was strong and, as it turned out, *correct*. That is
exactly what makes the case instructive. The reason to refuse was not that the
edit was wrong; it was that the campaign was not the party entitled to make it.
A measurement cannot certify itself, and "my change made the alarm fire, so I
will now adjust the alarm" is indistinguishable from the failure mode
preregistration exists to prevent — no matter how good the argument, and
especially when the argument is good.

**The rule:** when a preregistered control fires, the set of admissible
responses does not include editing the control. Park, hand the instrument to a
campaign that has no stake in its reading, and come back. The cost here was one
extra campaign boundary. The benefit is that the repair now stands on its own
evidence rather than on a green gate someone needed.

## Parking left the evidence in a gitignored file, and the commit that meant to fix that did not

The park commit's message says, in as many words, "Records the full measurement
table durably, since the ledger is gitignored scratch." Its diff is four
insertions and one deletion — the status header alone. It also points the reader
at a "§11" that does not exist in the spec, and the resumption prompt referred to
a "§10a" that does not exist either.

The measurement table — the five-seed baseline, the readout, the seven
predictions, the whole investigation — never left `.superpowers/sdd/`, which is
scratch by rule and gitignored by construction. It survived only because the
worktree was not cleaned in the interval. Had the worktree been removed, a
campaign parked *specifically so its evidence would outlive the session* would
have lost exactly that evidence.

Two failures compounded. The intended write did not happen, and the commit
message asserted that it had — so the record of the failure looked like the
record of a success, and a reader had no signal to check. Notably, a hook landed
in the same session to refuse staged `.superpowers/` paths; the durable copy had
to be authored into the spec by hand, and that step is the one that was skipped.

**The rule:** when a campaign parks, the durable write is a *deliverable*, not a
commit-message claim. Verify it the same way any generated-artifact claim gets
verified — read back the committed file and confirm the content is in it, before
writing the sentence that says it is. A commit message describing a diff is not
evidence about that diff.

## Re-freeze the baseline; do not reason about whether it moved

The resumption instruction was explicit about re-freezing, and the instinct to
skip it would have been reasonable: the parked campaign already had a five-seed
table, the intervening repair was to a metric this campaign does not compute,
and the predictions were about creature walks.

Re-measuring found the assumption wrong in every direction. Prevalence had moved
on all five seeds (seed 42 from 0.2225 to 0.1400). The by-cause breakdown had
inverted on seed 42 from thirst-dominated to hunger-dominated. The species
roster had changed — a giant-goat displacing a giant-elk on two seeds, a
hobgoblin displacing a goblin in seed 42's derived set — so the eight distinct
tempi the first readout reported were seven, over a different set of species.
The report struct had gained a field. And the health battery's own wall-clock
had fallen from 446 s to 266 s on main's own work, which means the campaign's
originally-measured 35% speedup was mostly *not the campaign*: re-measured
against the correct baseline it is about 6%.

Every one of those would have been silently wrong in the close, and the
speedup figure would have been a straightforwardly false claim in the chronicle.

**The rule:** a baseline is invalidated by *elapsed main*, not by whether the
intervening changes look topically related. The question "did anything land that
could plausibly affect my measurement?" cannot be answered by reading commit
subjects — five code campaigns were sitting behind an artifact-shaped tip
commit. The cost of re-freezing was two test runs.

## The refuted prediction confirmed on its own terms, which was worth checking for

The lazy resumption would have been: the metric has been corrected, the alarm is
now `stuck`, `stuck` reads zero, the gate is green, close. That reasoning reaches
the right verdict by a route that never tests the campaign's actual claim.

Measuring the *preregistered* quantity rather than the *current* one showed
something stronger: chronicity itself reads 0.0 on all five seeds now, so
prediction 4 confirms on its original terms and the close does not depend on
decision 0080 at all. The knife-edge had been a property of one moment's
physics, and main moved seed 42 off it before the repair ever applied.

**The rule:** when a metric changes underneath a preregistered prediction, report
the prediction against what it originally named, and report the new quantity
beside it. Re-pointing a prediction at whatever is convenient to measure now is
the same move as editing it, one step removed — and here it would have thrown
away the better result.

## The absorption cadence was missed, and the park is only half an excuse

This branch's first meeting with main was at close, 112 commits later — the
stage-boundary absorption cadence did not run. Part of that is defensible: the
campaign spent most of the interval parked, and absorbing mid-park would have
been absorbing mid-measurement, which the cadence itself carves out.

The undefensible part is the beginning. Six tasks were built before the park,
across a session in which main was moving, and none of those task boundaries
absorbed. The merge at close was clean by luck of file-disjointness — main never
touched `liveness.rs` in the whole range — not by discipline.

The related check did pay off: diffing the *full range* rather than the tip
showed five code campaigns behind an artifact-shaped tip commit, which is the
second time on this repo that a documentation-shaped tip has concealed
substantial code.

## A green pin is not a measured value

Prediction 3 was that per-agent tempo spreads across species rather than
clustering into two buckets — the check designed to catch a defaulted trait. The
test that pins it asserts `distinct > 1`, which two buckets satisfy perfectly
well. The pin passing is therefore not evidence for the prediction it was
written to protect.

Reporting it required a throwaway probe that printed the actual per-species
tempi (seven species, seven distinct values, 0.664 through 1.592). The same
applied to the Ametabolic control: "the test passes" was available, but the
prediction claimed byte-identity, and only a cross-tree comparison of the actual
trace could confirm it.

**The rule:** for a preregistered prediction, report the measured value, not the
green checkmark of the test that guards it. The two come apart most often
exactly where the pin was written loosely on purpose.

## Follow-ups

- **The Threshold reconciliation is still owed.** It has not landed
  (`origin/the-threshold` is four commits ahead of main), and it restructures the
  same function. Task 3's hoist was shaped so its occupancy work applies inside
  the single-step function while this campaign's queue applies outside it;
  whichever campaign lands second owes the reconciliation, and it is now The
  Threshold.
- **The `distinct > 1` assertion in `derived_npcs_carry_their_species_body_mass`
  should assert the continuum it was written for**, not a floor two buckets
  clear.
- **`CLIENT-four-clocks`' bubble-clock floor is still an unmeasured
  extrapolation** in the direction that now matters. Its own caveat says the
  4.75 ms no-op floor "rises by an unmeasured amount once per-tick behaviour
  exists — re-measure, do not extrapolate." Per-tick behaviour now exists; the
  re-measurement wants a session-level benchmark this campaign did not build.
