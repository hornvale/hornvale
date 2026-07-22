# The Teeth — retrospective

One page, process not product. Built predation's approach side: `worldgen::
prey_pressure` (the dual of `predator_pressure`) injected into the hunger drive's
`ANIMAL_PREY` axis, so a carnivore is drawn up the prey gradient. A near-mirror of
The Quarry, and the smoother for it.

## What worked

- **The combination operator collapsed a would-be new drive.** The G1 ideonomy pass
  ran the niche × field cross-product and the (diet · PREY) cell landed exactly on
  hunger's prey axis — so predation-approach was *not* a new drive but a new field
  for an axis that already existed. That reframing saved a whole drive's worth of
  scaffolding (a `Drive` impl, arbitration wiring, an urgency model). The lesson
  generalizes: before building a new thing, run the cross-product of the dimensions
  you already have — the cell you were about to build may already be occupied.

- **Additive-latent gave byte-identity *for free*, stronger than The Quarry's.** The
  Quarry tuned a `PREDATOR_LATENT_SCALE` so current agents stayed below the danger
  act-threshold — byte-identity contingent on a specific number. The Teeth's prey
  term is *additive* (it only ever raises `food_value`), so a creature that already
  eats where it stands keeps doing so **regardless of the scale**: byte-identity is
  structural, not tuned. The possession galleries did not drift at all, at
  `PREY_LATENT_SCALE = 1.0`. When a new signal can be shaped as "only raises the
  value an agent acts *above* a threshold on," below-threshold agents are byte-
  identical by construction — reach for that shape.

- **A dual is not a mirror in its distribution.** I first pinned `prey_pressure`
  with `contains(&0.0)`, copied from `predator_pressure`'s test — but prey
  (herbivores, wide-ranging) are not sparse the way carnivores are, so *no* cell
  reads exactly zero (min ≈ 0.005, mean ≈ 0.044). The probe caught it and the
  assertion became "a gradient exists" (`min < 0.2`). Structural duals can have very
  different distributions; measure the new field before copying the old one's pins.

## What the campaign taught

- **A new `Terrain` trait method taxes every test-terrain constructor.** Adding
  `prey_value` meant threading a `prey` map through `PlantedTerrain`'s six named
  constructors *and* three inline struct literals — and the mechanical `perl` pass
  under-matched, because the inline literals close with an 8-space `};` while the
  constructors close with a 12-space `}`. Two passes and a compile error found the
  stragglers. This is the same tax `SyntheticTerrain` charged in The Wilding. The
  durable fix (deferred, not blocking): give the test terrains a `Default`-based
  builder so a new field is one struct-field default, not N constructor edits. Filed
  as a followup.

## A follow-up worth promoting

The additive-latent pattern has now paid off twice (The Quarry's hazard, The Teeth's
prey draw) as the byte-identity discipline for waking a reserved seam. It is worth
naming explicitly in the drive-layer's own guidance (a `windows/vessel` note): **a
new field-driven signal wired dormant should be additive and threshold-gated, so the
current agents are byte-identical by construction and only a below-threshold
(foraging / fleeing) agent wakes it.** That is now the third campaign to rediscover
it; the next should inherit it.
