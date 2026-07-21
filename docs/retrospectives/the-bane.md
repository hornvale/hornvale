# The Bane — retrospective

One page, process not product. Gave fear a niche: each creature weights the kinds
of hazard (uncanny/heat/cold) by its nature, completing the diet↔threat↔sociality
niche trilogy. `threat_value = threat_niche · hazard_field`, the derived twin of
`food_value`.

## What worked

- **The abstraction-lift found a real unification, not just an analogy.** Lifting
  hunger, danger, and boldness to "a signed valuation over environmental fields"
  wasn't decoration — it directly shaped the code. The threat niche *is* hunger's
  `niche·field` dot product over hazard axes, and the flee gradient *is* the
  forage/comfort gradient sign-flipped. Naming the shared shape told me exactly
  which existing code to mirror, and the campaign came out structurally identical
  to The Provender because it *is* the same structure. **Lesson: when a lift finds
  that two shipped drives are the same shape, the third is a fill-in-the-template
  job, and the template is already tested.**

- **The niche derived from existing data — zero authoring.** The combination pass
  showed the HEAT/COLD threat weights correlate with the temperature niche and the
  UNCANNY weight with the metabolic class. So the whole niche is a pure function of
  two data every creature already carried; not one of 20 species needed a
  hand-authored threat vector. This is the difference between a tractable campaign
  and a slog, and it came straight out of the ideonomy.

- **Reusing the old scalar as one axis kept the churn small and byte-identity
  provable.** The Dread's scalar `threat_value` became a per-axis `hazards`, but
  the test/harness terrains plant their old scalar into the UNCANNY axis — and a
  mortal niche weights UNCANNY `1`, so every pre-Bane danger test reads identically
  and passed unchanged. Only the *new* thermal tests plant heat/cold. The field
  reshaped under the drive without a single old assertion moving.

- **Byte-identity was predictable and held again.** The possessed hobgoblin walks
  temperate ground, so every hazard axis is `0` and `threat_value` is `0`
  regardless of niche — zero gallery drift, as the spec predicted from the
  mechanism. Fourth drive-layer campaign in a row where the determinism outcome
  was argued in advance and confirmed by regeneration.

## What to watch

- **The active payoff is thermal, and thermal hazards are rare where creatures
  live.** Heat/cold hazard is `0` until an annual-mean temperature past `±(HOT/
  COLD)_DANGER_C` — thresholds set above ordinary discomfort (thermal's job), so
  they fire only near desert/ice extremes, which settlements avoid. So the
  goblinoid NPCs' fear differs by kind only at the world's edges. That is correct
  (extremes are the dangerous places), but it means the dial's variety is mostly
  latent in a temperate-settled world. The uncanny axis remains the main *active*
  hazard, now niche-gated (a future aberration NPC that doesn't fear it would be
  the first to show the gate biting).

- **No dedicated thermal harness scenario.** The per-kind thermal fear is proven
  by a unit test that drives the real `Danger` urgency
  (`two_species_read_the_same_hot_cell_differently`), and the danger-through-the-
  sim path is already proven by The Mettle's steady-vs-bold harness (the same
  arbitration path). Adding a thermal harness would have needed extending
  `SyntheticTerrain` to plant per-axis hazards for coverage the two together
  already give. If a future campaign makes thermal fear behaviourally load-bearing
  (a migrating herd fleeing a heat wave), add the harness then.

## Followups (captured to PSY-11)

- The general **`HazardVector` over a registered `HazardAxis` basis** (the full
  `ResourceVector` parallel) when more than three axes are needed; the reserved
  axes (**HOLY/UNHOLY, POISON, DROWNING**, and PSY-10's **PREDATOR**); and the
  **negative-weight attraction / approach** behaviour — the shared far-shore with
  The Mettle's reckless pole, landing with predation-approach (PSY-10).

## Confidence Gradient

Checked `open-questions.md`: a drive-layer refinement beneath the world-generation
bets, moving none. No re-score.
