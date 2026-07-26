# Retrospective — The Shudder

**Campaign:** The Shudder (PSY-11, the visceral felt phobia)
**Shipped:** 2026-07-25 · T1 `0573f507`, T2 `6ea6ea7d`, T3 `8db525e0`,
T4 `331a9dd0`
**Outcome:** byte-identical on every real world (artifacts clean, and the
population health report is bit-identical pre/post on all five null-control
seeds); gate green on the merged tree (2039 passed).

## What worked

- **The load-bearing distinction was already in the code, unrecorded.** The
  spec's hardest requirement — isolate the *transient* subset of the hazard
  memory without touching the static one — turned out to need no new
  computation at all: the fold's terrain shortcut already branches on exactly
  that question, and everything falling through it is transient *by
  construction*. Recording the branch instead of discarding it made the
  isolation free and made byte-identity structural (the emitter-free fast path
  returns before a single dread entry can be written). The generalizable move:
  before designing a way to compute a distinction, check whether an existing
  optimization is already making it for other reasons — a shortcut taken for
  cost often encodes a semantic split for free.

- **An ideonomy pass reframed the design fork from spatial to temporal, and
  that reframing decided it.** The stated fork was "dread at the creature's own
  cell, or also at its neighbours?" Rendering the phobia as a *cycle* showed
  the real axis was **persistence**: contact dread is felt on arrival and
  disproven by that same arrival — the loop closes — while anticipatory dread
  is felt from outside the cell and therefore prevents the contact that would
  disprove it, so it can only be closed by a forgetting clock that does not
  exist yet. That produced both the decision and a spec-level constraint on the
  reserved follow-up (anticipatory dread and time-decay must ship together).
  A second pass generalized it into the campaign's named principle: **every
  term added to a creature's fear must preserve the existence of an experience
  that disproves it** — which then killed a second candidate (self-reinforcing
  dread) with the same argument.

- **The precedent chain answered the fork Nathan flagged as a genuine design
  question.** "Should felt dread make the creature flee, or merely be felt?"
  looked like taste. It was not: `loneliness_from_plan` had already settled the
  underlying policy — a felt state with no actionable outlet is made *dormant*
  rather than allowed to register as distress ("an unreachable home is not a
  distress but a relocation"). Applied here it is decisive, because a phantom
  cell is now-safe ground with no terrain gradient, so a dread without its own
  gradient would read as `Lost` and arm the health alarm. The autopilot's
  "answer from precedent" step is worth running even on questions that present
  as open aesthetic choices.

## What the campaign taught

- **A plan's test fixtures need the same adversarial reading as its code, and
  this plan's did not get it.** Two of the three fixtures I wrote were inert,
  and both failures were *thematic*, not clerical. The felt-dread test gave its
  creature a safe revisit after the emitter left — which is precisely the
  disproof The Phantom's staleness rule performs, so the memory was empty and
  there was nothing left to feel. I had written the campaign's own extinction
  mechanism into the setup of the test meant to observe the thing before it
  extinguishes. Separately, the fixture days fell at night, where a Diurnal
  emitter pursues Fatigue rather than Danger and emits nothing at all. The
  implementer diagnosed both by probe before changing anything, and hardened a
  third test that passed *before* the feature existed (an empty alarm field for
  want of an empty memory — a tautology, not a tripwire). The check to add: for
  every new test, ask what makes it fail, then confirm it fails for that reason
  *today*.

- **Wall-clock on a box running parallel campaign sessions is not evidence at
  this resolution — and I ledgered a conclusion from it.** A measured 364 s →
  522 s on the health battery looked like a clean 1.43× regression, was
  attributed to the intended behaviour change, and went into the ledger as a
  falsified cost hypothesis. It did not reproduce: a confirming run came back
  *faster* than pre-campaign, and the output is bit-identical on every
  null-control seed, which forecloses any per-world cost on the emitter-free
  worlds. The lesson is not "measure twice" but something sharper: **a
  performance claim needs a mechanism that survives the bit-identical result.**
  Identical output plus a fast path that returns before the new work is a
  structural proof of no-cost that no stopwatch on a loaded machine can
  overturn. I retracted the entry; it should not have been written from one
  measurement.

- **The absorb tip lied again, and the habit caught it again.** `origin/main`'s
  tip was `chore(census): re-pin the census goldens` — reading as artifact-only
  churn. Behind it were 32 commits and two entire campaigns (The Retainer, The
  Snapshot), including +270 lines in `windows/vessel/src/session.rs`, the same
  crate this campaign edits. Diffing the *whole range* rather than the tip is
  now twice-vindicated; the pattern is that a regen commit is exactly what a
  closing campaign puts on top, so an artifact-shaped tip is weak evidence of
  an artifact-shaped range.

- **The census debt resolved itself, but only because another session paid
  it.** The commit gate was red on `origin/main` when this campaign reached
  close — 32 failures, all one stale census fixture, owed by The Vestige, which
  registered four census metrics without re-pinning. Nathan authorized the
  regen; by the time it would have run, a parallel session had already landed
  it. Nothing was wasted, but the underlying gap stands: nothing in the commit
  gate re-runs the census, so this debt accrues silently and is discovered by
  whichever campaign happens to close next. That campaign then has to prove the
  red is not its own — three separate ways, in this case — before it can
  proceed.

- **Absorption cadence held.** The branch met main at the plan boundary and
  again at close; the stage-boundary cadence was not missed, and the only merge
  conflict was in a generated file (the type-audit report), resolved by
  regenerating rather than hand-merging.

## Follow-ups

Recorded on the PSY-11 registry row: anticipatory dread (**ships with
time-decay or not at all**), superstition contagion, time-decay and the fading
discount, entity-keyed dread (the seam to SOC-9's enmity edges), kind-keyed
dread, and the sanctuary (a cell remembered as where fear lifted — the
proseasis of the phantom). Unpromoted: dread-specific narration wording in
`felt_phrase`, and dread on the greedy gradients.
