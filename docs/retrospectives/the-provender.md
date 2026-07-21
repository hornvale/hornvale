# The Provender — retrospective

One page, process not product. Added the fourth drive, hunger, by reusing the
thirst homeostat parameterized by each species' diet niche, with food read as a
graded productivity field over the climate.

## What worked

- **Ideonomy dissolved the hard question instead of answering it.** The campaign
  opened on a genuine puzzle — how does an autotroph, or a lithovore, hunger, and
  what gates whether a creature eats at all? A few ideonomy passes reframed it:
  hunger is thirst with the niche as its dial, and the gate is niche × source,
  not metabolism. That reframe deleted an entire branch of design (no
  "herbivore/carnivore/autotroph" type switch) and made three exotic diets
  correct-by-construction seams rather than future special cases. **Lesson: run
  the pass on the question that feels like it needs a taxonomy — the win is
  usually a dimension that makes the taxonomy unnecessary.**

- **The Kindling's integral was a second consumer, not a copy.** Hunger's urgency
  is literally `integrate_thirst` with different params. Because that path
  integral was written pure over occupancy + terrain + class, hunger got
  temperature-coupling (a hot creature hungers faster) for free, and the tick and
  the read compute it identically by construction. Two campaigns' worth of
  determinism discipline paid out as a one-line reuse.

- **The default-that-preserves-old-behavior pattern, a third time.** As solar
  altitude defaulted to the fractional sun and thermal coupling collapsed at
  thermoneutral, `Terrain::forage_value` defaults to a food-rich cell — so every
  planted/synthetic test terrain feeds its creatures and hunger stays quiet,
  and only the live `LocaleTerrain` (real NPP) and the deliberately-barren
  harness change behavior. The regression surface shrank to exactly the worlds
  that should move. The whole library fell out with two vessel tick tests to
  re-green (a missing `eaten` registration) and zero genesis drift.

## What to watch

- **Hunger's urgency rides on the drive struct, not the shared view.** Thirst and
  fatigue surface their folded urgency on `Perceived`; hunger carries its own
  (like the flow drive Thermal), which kept the field off ~35 test literals. It
  was the right call for churn, but it means "the felt drives" are now split
  across two homes. If a fifth stock drive lands, reconsider whether `Perceived`
  should just carry them all — the split is a mild readability tax, deferred.

- **A productivity proxy now lives in two places.** `LocaleContext::productivity_at`
  computes a Miami NPP proxy from its own climate, deliberately not depending up
  into demography's carrying-capacity (a sibling consumer). They are independent
  by design — one grades cells for a forager, the other sets population — but if
  the two ever need to agree, that's a refactor to a shared kernel-level proxy,
  not a coincidence to rely on.

- **The tick's Hold-jump is still thirst-shaped.** The closed-form "jump to the
  next act-crossing" uses thirst's rate; a creature Held by hunger alone
  recomputes each step and terminates via the step guard, but does not jump. No
  real world hits it (food is reachable near settlements); the harness food-desert
  proves the path. Captured, not fixed.

## Followups (captured to PSY-10)

- The general trophic engine: active predation (hunting as GOAP + the food web +
  Lotka-Volterra prey cycles), the exotic trophs (chemo/litho/radio/aether/
  emotivore/theophage), subsistence modes (forage → farm → herd), and the
  faith-fed deity. The `arbitrate` `Disposition`-struct tidy (five campaigns deep
  in individually-audited arguments) rides along.

## Confidence Gradient

Checked `open-questions.md`: this sits in the cognition/drive layer beneath the
world-generation bets, moving none. No re-score.
