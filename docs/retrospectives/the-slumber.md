# The Slumber — retrospective

One page, process not product. The Slumber added the fatigue drive and woke the
dormant activity cycle: a Tier-0 (fractional-day) wake-gate + Process-S
sleep-debt, framed as the first instance of a general chronobiology engine.

## What worked

- **A grounding question, then ideonomy, reshaped the campaign — twice over.**
  Nathan's "how would we model a circadian rhythm most realistically?" turned a
  fractional-day window into the two-process model; then "run it through
  ideonomy" (three method tuples) lifted *that* into a general
  `(zeitgeber, period, endogeneity)` oscillator engine that slots constructs,
  undead, and cultures in without special-casing. **Lesson (recurring this
  campaign-arc): the owner's realism question is worth more than a page of
  design, and ideonomy on an already-good answer enriches rather than reverses —
  the general engine came back "yes, and it's the same shape as MetabolicClass."**

- **The fallout drove the design — every red test was a design signal, not a
  chore.** Perpetual weariness → fatigue is sleep *debt*, not a daily driver.
  The ignorant agent that never found water → creatures sleep *in place*, not
  tethered home. The health metric blind at midnight → sample at a *waking*
  moment. Each fix was more correct, not just greener. **Lesson: when wiring a
  cross-cutting mechanic, let the broken tests tell you what the model got
  wrong; resist patching them to pass.**

- **"Tiers refine" made shipping honest, not a retreat.** The fractional-day
  wake cycle is the coarse truth; solar (latitude/season/terminator) refines it
  without contradiction. Shipping Tier-0 and registering Tier-1 is the
  constitution's own "coarse constrains fine," so the merge matches the spec's
  intent even though the headline realism is deferred.

## What to watch

- **A cross-cutting mechanic has a long tail.** The wake-gate touched
  exploration, the health metric's sampling, the diurnal thermal peak, the
  possession galleries — none obvious from the spec. Budget a live-wiring stage
  as its own campaign-sized effort, distinct from the mechanism.

- **Stage 1/2a "mechanism-only" WIP looked done but wasn't coherent.** The
  fatigue drive unit-tested green while the *system* was incoherent (unbounded
  fatigue) until the wake-gate bounded it live. A drive is not done until the
  thing that regulates it is wired.

- **An absorbed campaign's artifact miss surfaced here.** The-chorus study
  (sky-calibration) had drifted from the Consonance merge and wasn't
  regenerated; this campaign's close brought it current. Regenerate *all*
  artifacts at close, not only the ones you expect to have touched.

## Followups (captured to spec + registry)

- **Tier-1 solar masking** — the reserved `is_awake` body-swap against the
  astronomy daylight model (`solar_altitude_at`), latitude × season, locked
  worlds → fatigue-only.
- **The general chronobiology engine** — the `(zeitgeber, period, endogeneity)`
  oscillator stack; the endogenous free-running clock (jet lag, cave-drift,
  polar night); other zeitgebers (moons/tides/magic); the periodic-grid of
  predicted creatures; cross-domain reuse (canonical hours, temporal niches,
  eclipse-as-false-zeitgeber).
- **Rest quality** — `Fatigue.home` reserved: a safe den restoring more than an
  exposed field camp (waits on a safety notion).
- **Second-order** — saturating-exponential sleep pressure, napping, sleep
  inertia; the `arbitrate` `Disposition` struct tidy (four campaigns deep).

## Confidence Gradient

Checked `open-questions.md`: The Slumber sits in the cognition/drive layer,
beneath the world-generation bets that chapter scores, and moves none (same
scope note as [The Temperament](../../book/src/chronicle/the-temperament.md)). No
re-score.
