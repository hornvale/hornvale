# Retrospective — The Beholding

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-beholding.md): channels gain
roles, projections are named and declare what they preserve, a species'
perception vector derives its own eye, the scene document carries a `sight`
block, and a possession colours its chart through the possessed agent's eyes.

Nine tasks, sixteen commits (three spec, one plan, twelve implementation),
four fix rounds across four reviewed tasks. Four tasks reviewed clean on the
first pass.

---

## The dominant lesson: six guards shipped green and incapable of failing, and every one originated in plan text

This is the fourth consecutive campaign to report that no review finding
traced to an implementer's error. It is no longer an observation about a
particular plan; it is the base rate, and this campaign is the cleanest
instance yet because the defect class was *homogeneous*. Every one of the six
was the same failure: **a probe that cannot see the thing it tests.**

Implementers transcribed the plan's test bodies faithfully — which is exactly
why the authoring errors survived. A faithful transcription of a blind probe
is a blind probe. And each one was green, so nothing in the gate ladder had
anything to say.

| task | the guard | why it could not fail |
|---|---|---|
| 1 | standard-observer byte identity | `u8` rounding absorbed the 1-ULP shift the derived-norms mutation produced; the guard existed to catch exactly that mutation |
| 2 | the human row derives the standard observer | every probe reflectance was spectrally **flat**, and flat cancels curve shape exactly, so wrong curves in the right arm survived |
| 5 | no lens name leaks into output | the assertion looked for a literal `"[lens: terrain"` that could no longer appear once the default lens changed — it had gone **silently vacuous** before this campaign touched it |
| 7 | colour withheld where the glyph is not ground | two of the three withholding rules (the observer's own cell, the creature-mark overlay) had **no test at all**; mutations left 54/54 green |
| 8a | markup injection cannot form a tag | the fixture gave `<` and `img` **different colours**, so the run-length renderer put them in separate spans where they could never combine — the test passed against an `innerHTML` implementation |
| 8b | the caption reports the document's declaration | `sightOf`, the wire parser feeding the caption, had **zero coverage**; mutating it to `return null` left 59/59 green |

**Only reviewers explicitly told to mutate rather than read found any of
them.** A reviewer checking that the code matches the plan cannot find a
wrong plan. Every one of these was found by perturbing the implementation and
observing that nothing went red.

### The most transferable sub-lesson: coverage at every node is not coverage of the path

8b deserves separating from the list because its shape is different from the
other five, and more general than colour.

`parseSnapshot`, `sightOf` and `renderInto` each had unit tests. All passed.
The **seam** between them — a real JSON document parsed, its sight block
extracted, the caption rendered from the result — was never exercised by
anything. So a parser that returned nothing at all was invisible: the
caption test hand-built its own `Sight` object and never asked the parser for
one.

Three green unit tests over three functions is not evidence that the
composition works, and the composition is what ships. The fix was not a
fourth unit test; it was making **one existing test drive the whole path**,
after which the `return null` mutation reddens both the unit test and the
caption test together.

Generalised: *when a plan specifies a test per function, ask what test
covers the arrows between them.* A hand-built fixture in the middle of a
pipeline is the signature of this defect — it is exactly the seam a real
input would have had to cross.

### Naming a defect class forward demonstrably works — for one of its two shapes

The campaign ran an accidental experiment on its own dominant lesson.

After Task 2's finding, the vacuity warning ("does this probe actually
discriminate? prove it before trusting a green") was carried into every
subsequent **dispatch prompt**, not merely into the review checklist. The
evidence that this worked:

- **Task 3** came back clean on the first review pass, with the implementer
  having *proactively* verified that its dimmer-light probe discriminates
  (10/10 cells moved) and having checked that forcing `color = None` reddens
  at the anti-vacuity guard rather than silently at a count.
- **Task 8's first vacuous fixture was caught by the implementer, not a
  reviewer.** They verified empirically that the injection test passed
  against an `innerHTML` implementation, diagnosed the different-colours
  cause, and fixed it before submitting.

That is a real result: the cost is one paragraph in a dispatch prompt, and it
moved a defect from review-time to authoring-time twice.

But it did not prevent Tasks 7 and 8's other findings, and the reason is
worth stating precisely. **The warning addresses a weak probe; it does not
address a missing one.** "Prove your assertion discriminates" is a question
you can only ask about an assertion that exists. Task 7's two gaps and 8b's
were *absences* — rules and functions with no test pointed at them at all —
and no amount of scrutiny of the written tests would surface them. Those
needed the other half of the discipline: enumerate what the change made
checkable, then check the enumeration against what was actually tested.

Two shapes, two different countermeasures. Do not expect the cheap one to
cover both.

## A grep-derived plan is only as complete as the grep — again, and worse

The plan said **one** site asserted the lens string. There were **nine live
ones plus a stale generated artifact**, and one of the nine had already gone
silently vacuous — it asserted a leaked `[lens: terrain` marker that could
never appear again once the default lens changed, so it had stopped being
able to fail some time before this campaign began.

Three things about how the tenth site was found:

- **The grep could not see it.** `book/src/gallery/possession-seed-42.md` is a
  *generated* artifact holding a recorded transcript; it was found by
  reasoning about the change's reach, not by searching for a string.
- The implementer's verification grep **omitted `.txt` and `.json`**, so it
  could not have seen the scene-layer ASCII fixtures either. The reviewer
  traced those to an independent code path (a literal `"terrain"` argument,
  not routed through the session), so nothing was live — but the sweep
  methodology had a hole that happened not to bite.
- The vacuous assertion was repaired to a **lens-agnostic** marker rather
  than renamed to the new lens, and the reviewer proved the repair **both
  ways**: fabricated a colour-lens leak, confirmed the repaired assertion
  reddens on it, and confirmed the old assertion stays green on the same
  leak. That is the standard — a repair to a vacuous guard needs its own
  discriminating evidence, or you have replaced one green with another.

## A confirmed hypothesis can be confirmed at the wrong point

H4 predicted that a low sun reddens the chart. The task's test passed, and
reported a dramatic R:B ratio of 31.0 at "dusk" against 1.206 at noon.

The dusk probe sat at **−13.442°** — below the horizon — where the
attenuation model clamps to its maximum airmass. The test therefore
confirmed the clamp, not Rayleigh attenuation, which is a materially weaker
claim than the hypothesis makes. The implementer flagged this themselves; the
reviewer resolved it by **re-measuring**, scanning day fractions to find a
genuine above-horizon low sun (6.911°, R:B 1.917) and publishing the ladder
that shows the effect is real but strongly non-linear near the horizon.

Two process points:

1. **The number a chronicle publishes should be the number a shipped test
   asserts.** The reviewer's re-measurement lived only in the review file. At
   the close the shipped probe was moved to the measured point and given an
   explicit above-the-horizon precondition, so the published pair is now
   pinned by a test that fails if the world moves under it. A verified claim
   that no failing test can defend is a claim with a shelf life.
2. **A green H-test proves the inequality, not the sentence.** "A low sun
   reddens the chart" and "a sun below the horizon hits the airmass clamp"
   are both consistent with `d > n`. Only asking *where the probe sits*
   distinguishes them, and conformance review does not ask that.

## The positive lesson: a probe that needs no world build falsified three claims before they were written down

Before the spec asserted that a bugbear confuses red and green, a probe over
the seven authored hue exemplars — no world build, no terrain, seconds to run
— was pointed at the candidate derivation. It falsified three things that
would otherwise have entered the design document as confident prose:

- a tiered eye derived from a tiered gate gives three species one identical
  swatch set;
- signal distance is dominated by brightness, so it measures luminance and
  not hue;
- the candidate dichromat does not confuse red and green at all, because the
  retained rod channel carries the distinction.

The third one reshaped the entire campaign: it is what produced channel
roles, which is the campaign's keystone. **The cheapest measurement in the
project reordered its most expensive design decision**, and it was cheap
precisely because it was aimed at authored data rather than at generated
worlds.

The transferable practice: before writing a claim about a derivation into a
spec, ask whether the derivation can be exercised against **authored
fixtures alone**. If it can, run it first. A spec-time probe costs one test
file and buys a design.

### And freeze the diagnosis, not just the prediction

H3 was frozen **false**, with a standing instruction to ship the null and not
to retune the merge. It then came true — 0.0541 against 0.0680 — with no
constant moved.

That could easily have read as a rescue. It does not, and the reason is that
the falsifying measurement also **named the mechanism**: the rod channel
carries green's 520 nm peak, and a chromaticity that counts every channel
makes every eye with a rod a trichromat. The repair was the mechanism the
falsification had already pointed at, built as the campaign's enabling
change.

**Freezing a prediction protects against metric-chasing; freezing the
diagnosis is what makes a later repair legible as a repair.** Without the
recorded cause, "we changed the metric and now it passes" is
indistinguishable from tuning. The spec now carries both the original false
verdict and the outcome beneath it, unedited — the honesty of the record is
the finding, not a footnote to it.

## Adjudications worth recording

- **A re-export outside the brief's file list** (`pub use
  hornvale_kernel::KindId` in `domains/species`) was adjudicated **necessary**
  rather than scope creep: the brief's own verbatim test body required it.
  When an implementer must edit outside their file list to compile a test the
  brief dictated, the brief is the thing that was wrong.
- **A new test-only npm dependency** (`linkedom`, because Deno has no
  `document`) was adjudicated **warranted and contained**: zero hits in the
  built bundle, no existing test in the tree obtains a document, and the Deno
  2.9.2 pin for bundle determinism was untouched. Clients are outside the
  workspace dependency allowlist, so this is permitted — but it is still a
  new dependency and was reviewed as one.
- **Two "the mutation did not redden where predicted" concerns** were
  adjudicated non-defects, one of them because the guard turned out to be
  *stronger* than the plan expected. Worth noting against the standing rule
  that a mutation must redden where predicted: the rule catches real
  problems, and it also produces false alarms that need adjudicating rather
  than fixing.

## Deferred minors, promoted from the campaign ledger

These were reviewed, judged not worth a fix round, and would otherwise have
died with the worktree.

- **Task 4.** Three `#[allow(clippy::field_reassign_with_default)]` in
  `purview.rs` tests — adjudicated justified, since the bodies were verbatim
  from the brief. Still, a brief that dictates a body that needs a lint
  allowance has dictated the wrong body.
- **Task 4.** The H4 probe's comment claimed "a little before dawn" for a
  point 13.4° *below* the horizon — the same comment-overstates-the-code
  class as Tasks 1 and 2. **Fixed at the close** along with the probe itself.
- **Task 8.** An unmatched open parenthesis in a commit message body.
- **Pre-existing, found in passing.** `book/src/reference/scene-surrounds-v2.md`
  never documented the `color` cell field that The Pigment added — the
  reference page had been one field stale for a campaign. Fixed at the close
  alongside `sight`. A hand-authored reference page has no drift check, so
  nothing was ever going to notice.

## Process notes

- **The freshness sweep's prescribed grep found nothing; reasoning found
  three pages.** The brief's grep (`no colour|colourless|has no observer|…`)
  returned only false positives. The stale pages were the surrounds schema
  reference (two undocumented fields), the live-possession gallery page
  (which describes the panes and never mentioned colour or the `eyes` verb),
  and the Confidence Gradient's divergence-method bet. None of them contains
  a phrase a grep for absence-of-colour would match, because a page goes
  stale by *not saying* something.
- **Artifact regeneration after absorbing main was clean**, and the two
  artifacts the spec predicted would not move did not move: the gallery scene
  JSON and the three committed ASCII charts both go through the CLI's
  uncoloured path, which is the `skip_serializing_if` discipline holding.
- **The autopilot ledger** is in the campaign's SDD scratch; the spec and
  merge stops were both honoured, and this close is the second of them.
