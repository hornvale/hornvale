# The Cupel — retrospective

**Merged:** 2026-08-20 · **Program:** Myth, campaign 7

Process lessons only. The result is in
[the chronicle](../../book/src/chronicle/the-cupel.md). This was a *measure*
campaign; it settled `KNOW-derived-vs-constant-penalty` with a null and a
mechanism. The substrate did not move under it — main advanced (The Tally) but
touched no physics, and the readout reproduced byte-for-byte on the merge
product.

## A negative control can be vacuous against its own canonical mutation

The campaign's sharpest lesson, and it was invisible to everything except the
one check reserved for the controller.

The readout's provable-zero floor asserted `negative_tail == 0` over a
sub-population, and both task reviewers confirmed the assertion existed and its
sub-population was non-empty. The final whole-branch review is not what caught
it either. What caught it was the controller re-firing the mutation the control
exists to survive — deleting the `from == to` guard in `crossing_info` — and
watching `negative_tail` *stay* zero.

The cause: the sub-population filter (`crossings.is_empty()`, which **my plan
text specified**) was *downstream of the guard the mutation deletes*. Removing
the guard makes same-people steps record as crossings, which moves exactly the
holders the control cares about out from under the filter — the population fell
2541 → 303 and the survivors were still trivially identical. A test that
redefines its own population when you perturb the thing it guards cannot fail.

The rule, now paid for twice in this thread (the Touchstone re-fired its
mutation personally for the same reason): **a negative control's sub-population
must be defined by a fact upstream of the mutation it must survive.** Here that
is a ledger fact — the holder's people-homogeneous ancestry, or better a
descent-only walk where no step can cross a boundary at all — not a value the
mutated code computes. And the check that finds this is not a reviewer reading
the assertion; it is the controller running the mutation, which is why that step
is reserved for the controller and not delegated.

## Both of my fixes-from-outside-the-code were wrong; the fix-from-inside was right

The defect above took *three* proposals to close, and the shape of which two
failed is the lesson.

1. My plan's original filter (`crossings.is_empty()`) — guard-coupled, vacuous.
2. My *fix* for it (people-homogeneous ancestry on the existing seam arms) —
   also wrong, and it reddened on real, unmutated code (`negative_tail = 0.29`
   over 3587 holders). A holder's founding ancestry can be one people while its
   *winning route* round-trips out through another people via a raid seam and
   back, paying a real penalty. I proposed this filter from outside the code,
   reasoning about ancestry without tracing what a winning route can do.
3. The implementer's correction — a *separate descent-only walk*, where no step
   crosses a boundary by construction — was right, and it is the Touchstone's
   own construction.

Two controller guesses from outside the code, both plausible, both wrong; one
search from inside, correct. This is the thread's standing rule stated from the
other side: **do not prescribe a fix from outside the code any more than a
mutation.** Name the property the control must have — a sub-population provably
magnitude-blind and stable under the mutation — and let the implementer find the
construction that has it.

## A preregistration's bound can rest on an unstated assumption; catch it before unblinding

The plan first hard-asserted the primary day tail `< 5%`, justified by the
probe's P5 upper bound of 4.685%. Before the readout ran, the bound turned out
to assume something it never stated: P5 measured how near each holder's *derived*
width sits to a rung boundary, which bounds a day change only when *the same
route wins in both arms*. A constant penalty can change which route wins, and a
route change can move a rung P5 never covered. So a day tail above 5% could be a
real finding, and hard-failing on it would fail the test on the truth.

The ruling — report the day tail against the bands, assert only the controls and
non-vacuity — was made pre-unblinding, so it corrected a logical error in the
preregistration rather than rescuing a result. The mutation run later drove the
day tail to 7.76% and the readout *printed the >5% reading rule and stayed
green*, exactly as the ruling intended. **A preregistered numeric bound inherits
every assumption of the calculation behind it; name those assumptions before the
bound becomes an assertion, not after.**

## The substrate probe reshaped the finding, and caught its own defect

Run before any hypothesis was frozen, the Task 0 probe found the finding was not
what the thread had feared. The Undertow worried the derivation was inert because
edge counts *concentrate*; the probe found they vary widely (eight values) and
that the derivation dies for a different reason — the belief ladder is coarser
than the penalty. The freeze was written on that, not on a guess.

The probe also caught a defect in itself. Its first day-channel bound checked
only the *upward* rung boundary and reported 0.11%; the two-sided bound (a
penalty change can push a width down across a boundary too) reported 4.685%.
Freezing "≈0" would have been wrong and the readout's 1.34% would have read as a
bar quietly moved. **A one-sided bound is a common and invisible error; a
distance-to-boundary is two-sided.**

## Two smaller items, recorded so they are not lost with the scratch

- **Matched-mean is matched *discount*, not matched *penalty*, pooled across
  worlds.** `D_panel = 1/mean(1/(1+edges))` matches the mean discount factor;
  because `span(FINEST)` varies across worlds but is constant within one, it
  matches the mean *penalty* exactly only per world (`D_world`). The chronicle
  states which "mean" each estimator matches; the finding holds under both
  (1.34% vs 0.86%). A phrase like "matched mean penalty" is slightly loose — the
  formula governs.
- **A brief that names a test by the wrong filter wastes a round.** The Task 1
  brief said `-E 'test(traced_walk)'`; the agreement battery's test *function*
  is `traced_agrees_with_the_shipped_walk_holder_for_holder`, so the filter
  matched nothing and the implementer corrected it to `binary(traced_walk)`.
  `test(...)` filters by test name, not file name.

## Deferred, with homes

- **Deferred minor:** the fine-ladder control's module doc could state in one
  clause that it is mechanism-isolation, not dose-matched realism. Final review
  triaged it not-must-fix; left to a future editor.
- Three forward levers, each an idea-registry row rather than a hunch, because
  the campaign *measured* where the myth thread's next lever is:
  `KNOW-belief-ladder-quantizes-people-effects` (finer memory),
  `KNOW-evaluative-beliefs` (contestable, subjective claims), and
  `KNOW-dialect-distance-loss` (transmission loss by linguistic distance, whose
  raw material the language domain already computes and nothing consumes).
