# The Quarry — retrospective

One page, process not product. Added the first biotic hazard — a carnivore-presence
field (`worldgen::predator_pressure`, injected into `LocaleTerrain`) — as a PREDATOR
axis on The Bane's `Hazards`; the dread derives from carnivory (the eater-eaten
link). The largest integration of any drive campaign.

## What worked

- **Encapsulating the demography fit in worldgen kept the layering clean.** The
  vessel never touches demography; it receives a finished `CellMap` from
  `worldgen::predator_pressure` and injects it into `LocaleTerrain` exactly as The
  Wakeful Sun injected the calendar. The composition-root did the cross-domain glue;
  the drive layer stayed a consumer. That the injection pattern already existed
  (calendar) made a worldgen↔vessel field-hand-off routine rather than novel.

- **The eater-eaten derivation is the campaign's real content.** `predator_weight
  = 1 − carnivory`: the same authored number (animal-prey diet weight) reads forward
  as appetite and backward as terror. It links the diet niche to the threat niche
  with no new authoring and completes the ideonomy's negation (the duality: threat
  vs food, one field two signs).

- **Density, not capacity, was the fix for a real failure — and the failure taught
  the model.** The first cut used carrying capacity and lit up all fertile land,
  which is exactly where settlements sit (fertile = prey-rich = predator-rich), so
  settled NPCs fled and the null-control tripped (transient *fatigue*, from the
  disrupted rest — chronicity stayed 0, the alarm never fired). Switching to the
  coexistence stack's *realized* density — competition-thinned near settlements,
  home-range-spread for apexes — concentrated the field on genuine wilderness. **The
  lesson generalizes: for "where is X dangerous," realized density beats carrying
  capacity; capacity is potential, and potential is broad.**

## What to watch

- **v1 is dormant for every current agent — by calibration, and honestly so.** The
  living peoples are bold omnivores; even on dense predator ground their derived,
  latent-scaled dread stays below the danger `act`, so worlds are byte-identical and
  the possession galleries did not drift. The predator fear is *wired and correct*
  but wakes only for a vulnerable creature — a timid herbivore — the moment one
  becomes an agent, exactly as The Bane's exotic threat-niches wait for their
  dragons. The unit tests prove it on a constructed herbivore; the harness scenario
  was folded into that unit test rather than a sim run, since no *live* agent
  exercises it (the same reason The Bane's thermal harness was a unit test). If a
  vulnerable species is ever peopled or a beast is agentified, the field wakes and
  a harness should follow.

- **The `PREDATOR_LATENT_SCALE` is a calibration knob to revisit.** It was set to
  keep the current bold peoples dormant (byte-identity). When the reserved
  refinements land — the *visible* hunting predator (acute, survival-tier), the
  defendedness term (a massive creature fears less), or a vulnerable people — the
  scale and the latent/acute split want reconsidering, likely by making the *field*
  drive a mild routing deterrent while the *agent* predator drives acute flight.

- **Perf: one demography fit per session and per health-sim world.** Bounded and
  accepted; the census (which does not run the drive sim) is untouched. Watch it if
  the drive sim is ever run over many worlds.

## Followups (captured to PSY-10)

- The **food/approach side** (a predator drawn to prey — the negative charge, with
  The Mettle/Bane's reserved approach shore); the **visible agent-predator** that
  hunts (GOAP), acute and survival-tier; the **Lotka-Volterra** population dynamics
  (the field oscillating over time); and the **defendedness** (mass/potency)
  refinement of vulnerability.

## Confidence Gradient

Checked `open-questions.md`: a drive-layer + demography-reuse campaign beneath the
world-generation bets, moving none. No re-score.
