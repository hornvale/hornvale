# The Mettle — retrospective

One page, process not product. Added the third psychology dial, boldness, scaling
The Dread's felt threat — reusing the banked `PsychVector.threat_response` at
creature scope, centered on a steady baseline so the goblin (and the possession
walk) stay byte-identical.

## What worked

- **The dial was already banked — a third scope on an existing datum.** No new
  `PsychVector` field, no new authored data. `threat_response` was written per
  people and read only by the culture layer; the drive layer read the same datum
  at creature scope, exactly as `time_horizon` and `deliberation_latency` were
  wired in earlier campaigns. **Lesson (repeating, and worth trusting): before
  adding a per-species datum, check whether one authored for another layer
  already carries the meaning at a different scope. Twice now the psychology dials
  came free this way.**

- **A negation pass changed the model, not just decorated it.** The naive
  "boldness reduces fear" is a one-sided `[0,1]` scale-down (steady → fearless).
  Negating the definitional property — *what amplifies fear?* — surfaced the
  coward pole a scale-down omits, which reframed the dial as an axis about a
  *center* rather than a slider from full to none. Centering the scaling on `0.5`
  (`× 2(1 − boldness)`) landed the full coward↔fearless axis in one line AND made
  the goblin baseline inert. Without the pass, this ships as a lesser model with
  no coward and a goblin that already dampens.

- **Scaling the urgency, not the gradient, kept the win free.** Boldness scales
  only how much a creature *cares* about a hazard (its Danger urgency); the signed
  serviceability gradient — the hazard's physics — is untouched. Because the
  arbitration already weights the gradient by the urgency, the risk-appetite
  behaviour (a bold creature crosses ground a timid one flees) fell out of the
  existing machinery with no new path logic. The blast radius was one multiply.

- **Byte-identity was predictable in advance, and held.** The spec predicted zero
  gallery drift by a specific argument — the possessed hobgoblin is bold (0.7) but
  never nears a strange site, so its danger urgency is `0` regardless and `0 ×
  0.6 = 0`. The regeneration confirmed it: zero drift. Predicting the determinism
  outcome from the mechanism, then confirming it, is the discipline these
  drive-layer campaigns run on.

## What to watch

- **Only the goblinoid peoples carry a real boldness; the beasts default to
  steady.** `threat_response` lives in `psyche_registry`, which covers the four
  playable peoples (goblin steady, hobgoblin/kobold/bugbear bold); every beast
  falls to the `0.5` default and is inert. So the dial's variety is currently
  narrow (all authored values sit in `[0.5, 0.8]` — no coward, no fearless in the
  wild). The coward pole and the fearless pole exist in the mechanism but have no
  occupant until a timid or fearless species is authored.

## Followups (captured to PSY-11)

- The **danger-approach** reckless far shore (`× < 0`, negative fear — the moth,
  the berserker, the hunter's inverted fear of its prey), the seed of
  predation-approach, landing with PSY-10; **per-drive boldness** (discounting the
  thirst/hunger/social warnings, not only danger); **situational courage**
  (boldness rising with another drive's desperation — partly emergent from the
  arbitration already); and **construct programmed-boldness** (a war-golem
  fearless by design — the ametabolic own-drive).

## Confidence Gradient

Checked `open-questions.md`: a dial in the cognition/drive layer beneath the
world-generation bets, moving none. No re-score.
