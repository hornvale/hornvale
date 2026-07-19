# The Turning — retrospective

Process lessons from shipping the diurnal temperature term (weather program
campaign 1) across hornvale + orrery. Product is in the chronicle.

## The one that matters: the visual pass caught a bug five reviews couldn't

The diurnal waveform shipped planet-synchronized instead of per-longitude —
a genuine physics error in the model I authored in the spec/plan. It
survived the Task-1 implementer, the Task-1 reviewer (who verified the
formula matched the brief *line by line* — and it did), and three more task
reviews, because **they were all checking the same thing: does the code
match the specified formula?** None checked the physics the formula was
supposed to encode. The error only surfaced when the controller opened the
screenshots at assembly and saw the whole globe pulse in unison.

The lesson sharpens The Lens rule. It is not only that jsdom can't see
layout — it's that **a spec's formula can be internally consistent and
physically wrong, and every review that treats the spec as ground truth
will pass it.** The visual pass is the only reviewer that checks the code
against *reality* instead of against the plan. Budget it as a real gate,
and when the deliverable is a physical model, treat "does this look like the
phenomenon?" as a first-class review question, not a nicety.

Corollary for plan authors: the formula in a plan is a claim, not a
given. Had the Task-1 brief asked for a property test that *two longitudes
at the same instant sit in opposite phase*, the bug would have died at Task
1 instead of assembly. The anti-regression test that eventually caught it is
exactly that test — written after the fact. Write it before.

## What worked

- **The A2 decision paid off.** Extending `temperature_at` with a clean
  `diurnal` seam module (rather than pulling the kernel Field-trait refactor
  forward) kept the campaign small and let the per-longitude fix land as a
  one-function change threaded through, not a re-architecture. The ideonomy
  pass that reframed A-vs-B as a scope confusion was worth the ten minutes.
- **Zero-mean as the byte-identity lever.** Designing the diurnal term to
  integrate to zero over a rotation meant the whole campaign — including the
  model bug and its fix — never moved a census byte or a frozen world. The
  invariant did the work; no epoch, no regen, no AWS.
- **The two-repo producer-first cadence** held again: land the producer,
  hand the client a dev-loop binary, pin the client's reconstruction with a
  producer-sourced golden, defer the release to G6.

## What to watch

- **A background fix agent died to a network error mid-gate**, leaving its
  change uncommitted. The controller finished it inline rather than
  re-dispatching into a flaky link. Worth remembering that a killed agent
  leaves *work in the tree*, not just an absent commit — check
  `git status` before assuming nothing happened.
- **Per-task reviewers asked to "verify the formula matches the brief" will
  do exactly that.** For model/physics work, the reviewer prompt should also
  ask "is the modelled behaviour physically right?" — the one framing that
  would have caught this without a screenshot.

## Absorption cadence

The branch met main three times (Predicates/Tongues/Foresight, then The
Correspondence at close), each a clean or type-audit-only conflict; the
concept-registration refactor (The Correspondence, Stage 2) touched
`domains/climate/src/lib.rs` where the diurnal module was wired, but the two
changes sat in different regions and auto-merged. Additive, byte-orthogonal
work absorbs cheaply.
