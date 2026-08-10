# Retrospective — The Armature (the declared causal frame, 2026-08-09)

Process lessons only; the product story is
[the chronicle](../../book/src/chronicle/the-armature.md). Decision 0020 governs
the form.

Six tasks (1, 2, 3, 4, 4b, 4c), ten commits, one measurement. The campaign
closed with the frame byte-identical to the commit that froze it and with two
defects repaired in the instrument that measured it.

## 1. A defect in the plan's own text, caught by an implementer — again

The plan supplied a test fixture for the sign-suppression branch: a driver
column of `[1.0, 1.0, 1.0, 1.0001]`, with the plan's own comment describing it
as "near-zero correlation" so that the observed band would land in `none` and
the sign would be withheld.

It computes `r = 0.7746`. That is the *dominant* band. The branch the fixture
existed to exercise was never reached, and the test would have passed for a
reason unrelated to the feature it was named after. The implementer caught it,
substituted `[2.0, 4.0, 1.0, 3.0]` (`r = 0.0` exactly), and the reviewer
independently recomputed both values before accepting the substitution.

This is the same headline The Domesday and The Digest both wrote: **the defect
originated in the controller's plan text, not in implementer code.** The
specific mechanism is worth isolating, because it is not the usual one. This
was not a wrong predicate transcribed faithfully — it was a *hand-computed
oracle asserted without computing it*. Four numbers close together read as
uncorrelated to the eye, and `1.0001` among three copies of `1.0` reads as
noise. It is a perfect rank ladder. **A plan that supplies a fixture must
supply the arithmetic that makes the fixture the right one, or supply neither
and let the implementer derive both.** Stating the intended outcome in a
comment is the part that makes the error survive review: the comment tells the
reviewer what to expect, and the reviewer then checks the code against the
comment rather than the comment against the arithmetic.

## 2. Both scope extensions came from reviewers, not from the plan

The plan had four tasks. The campaign shipped six. Tasks 4b and 4c exist
entirely because review found things the plan did not anticipate — and in both
cases what review found was a defect that made a *published number wrong*, not
a code-quality issue.

- **4b.** The reviewer, re-deriving every tally in Python from the census and
  the frame rather than reading the rendered pages, found that the correlation
  function's zero-variance guard was defeated by summation residue and that six
  published findings were computed from noise.
- **4c.** The reviewer read the strength branch's comment against its code and
  found the comment said the sign is withheld only when nothing was measured
  while the code withheld it unconditionally — which meant a published
  `r = −0.755` backwards link rendered as a bare strength mismatch beside a
  headline reading "zero backwards links".

Neither is a review nit. Each was worth a task. The pattern to carry forward:
**a measurement campaign's review must re-derive the published numbers by an
independent route, not check that the code matches the plan.** Both findings
came from a reviewer who left the repo's own tooling and recomputed in another
language; neither would have come from reading the diff. Task 4b's reviewer
went further and *reverted the fix* to confirm the red was behavioural rather
than a compile error, which is the mutation discipline applied to a repair
rather than to a guard.

The corollary for planning is uncomfortable and should be said plainly: the
plan's four tasks were the right four tasks for the feature, and the campaign
still needed a 50% scope extension to be *correct*. Budgeting a measurement
campaign at its feature size underestimates it.

## 3. Pre-measurement doubt, recorded, is admissible; afterwards it is not

Task 3's implementer transcribed thirty rows and flagged five as physically
questionable — one two-hop inference, two treating a U-shaped cost as monotone,
two assuming a world-level scalar reaches an individual's tempo with no density
term — and transcribed all five **unchanged**. That was the correct behaviour
under a spec that forbids revising the frame, and it is also what made the
doubt usable: because it was written into the ledger before any correlation
existed, it could be cited in the verdict as evidence about the author's physics
rather than dismissed as hindsight.

Generalize it: **when a blinded transcription task disagrees with what it is
transcribing, the disagreement is data — capture it in the ledger and change
nothing.** An implementer who silently "fixes" a questionable row destroys the
preregistration; one who argues about it burns the schedule; one who transcribes
and records has produced a second, independent measurement for free.

## 4. Two commits were deliberately red, and that broke bisect

Commits `168ef9b8` and `560c4d38` each ship a red commit-gate test. This was
plan-sanctioned and correct in the small: renaming the detector's finding
strings in Task 2 necessarily invalidated the live-acceptance test's pinned
counts, and re-pinning them before Task 4 would have *been* the measurement,
performed early and by the wrong task.

The cost is real and was not budgeted: `git bisect` over this branch cannot
distinguish the sanctioned red from a genuine one, and any future bisect that
lands on either commit gets a false positive. The blinding requirement and
commit-by-commit greenness are in genuine tension here — there is no arrangement
of these tasks that has both. What would have cost nothing is **saying so in
the commit messages**, so the archaeology is available where the bisect stops
rather than only in a scratch ledger that dies with the worktree.

## 5. Autopilot handled the routine half and correctly refused the rest

Six decisions were ledgered. Two are worth reporting on as process.

Decision #5 (add a distinct `D5 unmeasurable` outcome rather than let the six
biology rows fall silent) was resolved under autopilot against the campaign's
*own spec* as precedent — §1's "a severed link becomes a finding rather than an
absence" — which is the strongest kind of precedent available and the reason it
did not need to reach Nathan. The ideonomy pass overturned the first instinct:
fixing the guard alone would have converted a visible wrong number into an
invisible absence, making the instrument worse while looking like a correction.

Decision #6 split correctly along a line worth naming. The minimal fix (report
a measured sign) was taken under autopilot because the ideonomy pass showed it
to be the *symmetric completion* of a rule already in force — the sign is
suppressed exactly when it was not measured — rather than a new policy. The
taxonomy question it exposes (should a row that is both wrong-band and
wrong-sign be promoted to `D5 direction`?) was **not** taken, because that is a
change to a vocabulary the spec deliberated, and deliberating it after seeing
which rows it would move is the phase-order violation the whole campaign is
about. It is flagged for the merge stop.

**The reusable rule: a fix is autopilot-eligible when it completes a rule
already in force; a fix that changes the rule is not, even when it is obviously
better.**

## 6. Smaller items, recorded

- **A duplicate test that could not fail independently.** Task 4b shipped a
  test (`d5_does_not_report_unmeasurable_when_both_columns_vary`) that was a
  verbatim copy of an older one — same fixture, same assertions — so it could
  never go red unless the older test did. Caught in review and replaced in 4c
  with the guard actually wanted: 999 identical values plus one differing by
  `1e-9` must still report a strength mismatch, which probes the boundary of the
  exact-equality check rather than restating a case already covered.
- **Vacuous-today prose in a finding.** The unmeasurable finding said "frozen
  at *v* across *n* worlds" using the *paired* count. All six of today's cases
  pair 1000 of 1000 so the wording was harmless, but row #4 already pairs only
  952 — a future metric with absences could be constant among paired rows,
  vary globally, and be published as "frozen". Changed to "paired worlds"
  before it could be true.
- **A test that passed before the feature existed.** `d5_is_silent_when_
  strength_and_sign_both_match` was green against the pre-implementation code,
  because the old strength-only detector was silent on any band match. Mutation
  testing showed it *is* a real regression guard against the new branch, so it
  was kept — but it never discriminated feature-absent from feature-present, and
  a test written for a new branch should be run against the old code and
  required to fail.
- **A blinding rule the plan made stricter than the spec.** Task 1's reviewer
  raised the campaign's only Important finding on the blinding rule, correctly
  reading the *plan*. The spec binds only the task that writes the frame; the
  plan generalized it to three tasks. The plan's own constraints say the spec
  governs, so nothing was violated — but a plan that tightens a spec's rule
  without saying it is tightening it will produce exactly this false positive,
  and a reviewer's time is the cheapest thing it costs.
- **A pre-existing typographic split, extended rather than resolved.** Two
  finding strings used ASCII `--` while a third used a real em dash, and the
  book has no smart-typography preprocessor, so `--` rendered literally on nine
  published lines. Task 4c noticed and deferred it; the close fixed it. Worth
  noting only because "match the surrounding style" and "the surrounding style
  is inconsistent" is a loop that terminates at the close or never.
- **A repair that committed the mirror of the defect it removed.** The
  whole-branch review, at the merge stop, found that replacing the renderer's
  frozen `D1..D8` literal with a roster *derived from the findings* had deleted
  every zero row: a detector that fires nothing has no finding to derive a name
  from. `D7 | 0` left the published index and `D5 direction | 0` — this
  campaign's headline null — never entered it, so absence in that table again
  carried two meanings, the exact conflation the campaign's own decision
  forbids. The fix is the union of a declared roster and the observed names.
  Two lessons, both already in the project's ledger and both re-earned here:
  swapping one half of a two-sided invariant for the other half is not a fix,
  and **the repair of a defect deserves the same review as the defect** — this
  one shipped through a task review and a whole-branch review before being
  caught. A guard is what makes the difference: the zero-row test was required
  to fail against the interim renderer before it was believed.

## What went right, briefly

The blinding held on both sides — neither implementer nor reviewer read a
census value during authoring, and the reviewer verified the frame field by
field against the spec, 30 of 30. The measurement happened once. Every tally
published was independently re-derived in another language by someone who had
not written it. The frame was never edited: `git diff` against the freezing
commit is empty at close. When the instrument turned out to be broken, the
repair was made to the instrument and the frame was left alone — which is only
possible because studies are data and metrics are code, and this campaign is
the clearest return that separation has paid.
