# The Niche

The coexistence stack could pack many species into one cell, but it packed them
from carrying-capacity fields that were all the same shape. Every species read
one hardcoded temperature optimum and the same net-primary-productivity proxy,
so each field was very nearly proportional to every other. Packed together on
seed 42, all 276 settlements came out with an identical composition — kobolds
just over half, then goblins, then a thin tail of hobgoblin and bugbear,
everywhere, ocean to ice. The machine was correct and the world was oatmeal.

The Niche makes the field remember what the creature *is*.

## Habitat as fit

A species' carrying capacity becomes the product of two terms — is there food
here, and can it (or does it choose to) live here:

```
K_s(cell) = ResourceSupply_s(cell) × Π_axis  response_s,axis(field_axis(cell))
```

The resource term is the shipped NPP proxy, saturating (intake plateaus in
super-abundance) and weighted by the species' uptake. The condition term is a
product over four environmental axes the world already computes — temperature,
moisture, insolation, elevation — each scored through a curve that is not a bare
Gaussian:

```
response(x) = floor + (1 − floor) · devotion · gaussian(x; optimum, width)
```

The Gaussian is Hutchinson's niche: an optimum and a tolerance breadth. The two
coefficients around it are the fantasy the model was built to carry.

**Sovereignty** is the `floor`. At floor zero the axis is a hard constraint — a
cave cricket dies in sunlight, excluded outside its tolerance. Above zero it is
a soft preference — never excluded, merely denser near the optimum. The floor is
not a free knob; it is derived from mass and magical potency, a saturating map
that reads a creature's homeostatic buffering budget. A mouse is placed by its
environment; a dragon buffers the environment and prefers its crag; the axis
runs continuously from plankton to god. Not every axis yields to it: elevation
stays hard regardless of might, because sovereignty relaxes physiology, not
geometry.

**Devotion** is the peak amplitude — how strongly a creature favours its optimum
given the freedom to. It is what gives even an unconstrained creature a home.

Insolation had to be built. Only a single global scalar (`L/a²`) shipped, so the
per-cell field is computed as the annual mean of the daily-mean insolation over
the orbit — obliquity-aware, every transcendental routed through the kernel's
portable `math` so the field is bit-identical on every platform. The whole
computation draws nothing from the seed: a world's habitat is a pure function of
its committed sky, climate, and terrain crossed with authored traits.

## What emerged, and what it cost to see

Given the four peoples differentiated climate niches, the oatmeal broke.
Composition now varies across space — one region hobgoblin, another goblin, with
spatially structured strife along the ecotones between — and a
`composition-variance` metric reads it directly: zero when every settlement
shares one blend, positive when they diverge. On seed 42 it reads 0.065, and the
biogeography-emerges guard holds.

It broke only two ways, not four. The four goblinoids are ecologically almost the
same animal — all omnivores drawing on one shared NPP base, all within a factor
of ten in mass — so climate preference is the only lever they have, and the
sovereignty floor keeps even an ill-suited species present everywhere. Real
strongholds, refugia, a creature that owns the cold: those need species that
differ on the axes the engine actually exploits — distinct *resource* niches
that partition the food web, disparate masses, potency. The habitat model is the
mechanism; the menagerie is the payoff, and the menagerie is where the campaign
found its edge.

## The edge: a struct that had become a world

Authoring a dozen creatures spanning autotroph to dragon meant asking a fungus to
carry a psychology vector, a proto-language, and the words for its chieftain. The
descriptor of a *people* had quietly become the descriptor of every *entity*, and
the biosphere half (mass, niche, condition, potency) was a small island in a sea
of fields only a settling, speaking people ever reads. It is the inheritance-
versus-composition problem that motivates entity-component systems, and it does
not belong to one campaign.

So The Niche closes here, at the validated critical path: the habitat model works
and the oatmeal is broken. The menagerie and the cutover of settlement genesis
onto the differentiated stack fold forward into an entity-component program —
which, on inspection, is not a foreign machine to bolt on but the convergence of
systems Hornvale already runs: the append-only Fact ledger, the derived-view
query engine, the chunk-partitioned room ledger, and the component split this
campaign forced. An entity is the set of components it carries; most entities are
silence on most staves. That score is the next thing to write.
