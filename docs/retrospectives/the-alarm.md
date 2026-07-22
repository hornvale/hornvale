# The Alarm — retrospective

One page, process not product. Built fear-contagion (PSY-11's reserved instance): a
per-tick **alarm field** summed over the frozen population's primary-afraid
creatures, read additively + latently into the existing `Danger` drive. A calm
creature catches a cornered neighbour's fear and flees, then settles once out of the
one-hop halo. Single session, main static throughout — no stage-boundary absorptions
were owed (nothing moved on main between the Default-builder commit and close).

## What worked

- **The autonomy-axis reframing collapsed a would-be new drive.** The G1 ideonomy
  pass (substitution + dimension-id) applied the *autonomy* prompt and reframed
  fear-contagion as "the externally-driven substitution of the self-driven Danger
  drive." That one sentence settled the biggest structural question before any code:
  contagion is not a new drive with its own arbitration and urgency model — it is a
  *second source* for the threat the Danger drive already feels. The same lesson The
  Teeth learned from the combination operator (predation-approach was hunger's PREY
  axis, not a new drive), reached here from a different operator. Before building a
  new engine, ask which existing engine's *input* the new thing actually is.

- **The abstraction-lift caught a non-termination bug before it was written.** Pass 2
  lifted the idea to "signal propagation through a network" and took PSY-11's own
  "immunology channel" at its word, comparing against SIR epidemic models. That
  comparator forced the question the field framing was hiding: does borrowed fear
  *re-emit*? If yes, `R0 ≥ 1` — a stampede that outlives its predator, non-
  terminating, physically false. The fix (build the field from an alarm-free affect
  read, so only primary fear emits) makes `R0 < 1` *by construction*, not by tuning.
  A failure mode designed out at the whiteboard is worth ten caught in a test; the
  cross-domain lift is what surfaced it, and it would not have surfaced from staring
  at the Danger drive alone.

- **Additive-latent paid off a fourth time — and structurally, not by tuning.** The
  Quarry/Wilding/Teeth each waking a reserved seam byte-identically via an additive,
  threshold-gated term is now four campaigns deep (memory:
  additive-latent-byte-identity-pattern). The Alarm's is the strongest yet: because
  *emission* requires a primary-afraid neighbour and the seed-42 peoples never reach
  primary danger distress, the alarm field is **empty on seed 42 regardless of
  `ALARM_SCALE`**. Byte-identity is a property of the *emitter set being empty*, not
  of a carefully-chosen scale — there was no number to tune to preserve it.

## What the campaign taught

- **A field-aware mover silently changes what a downstream reader measures — check
  the reader.** T2 wired the alarm field into `DriveMovements::step`, and the health
  metric's `run_simulation` moves creatures *through that same tick*. So the health
  sim's movement became field-aware the moment T2 landed — even though its affect
  *sample* stayed alarm-free. The implementer's vessel-only test pass would have
  missed any distress this introduced; the null-control (a ~271s lab battery) had to
  be run explicitly against the field-aware mover to confirm `chronicity == 0` still
  held. It did — a fleeing herd samples as *Searching*, not distress — but the lesson
  is the seam: **when you make a shared mechanism read a new signal, re-run every
  consumer of that mechanism's output, not just the code you touched.** The health
  metric is not a committed fixture, so this cost only a test run, not a rebaseline —
  but the check was not optional, and it was nearly skipped.

## A follow-up worth promoting

The behaviour/affect split this campaign shipped — the tick *mover* reads the alarm
field but the post-tick *felt-state* read (`affect_of`, used by narration and the
health metric) stays alarm-free — leaves a small incoherence: a creature narrated
mid-flight from borrowed fear reads "calm but moving." It is correct enough (a
successfully fleeing creature *is* Searching) and it was the right conservative v1
(an alarm-aware felt-state read risks the health baseline for only narration polish),
but it is a real reserved refinement, now recorded on PSY-11. The general shape is
worth naming for the next drive that adds a mover-side signal: **decide deliberately
whether the felt-state read should see it too, and if not, record the incoherence
rather than discovering it in a narration bug.**
