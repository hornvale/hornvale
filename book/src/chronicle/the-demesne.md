# The Demesne

**July 2026 · outcome: shipped — the habitat model's resource supply becomes
a field per axis instead of one shared number, and a rock-eating creature
holds real ground for the first time; the world's peoples do not, and that
gap is named rather than hidden**

## What was attempted

A creature's place in the world is supposed to follow from what it eats.
The habitat model gives every kind a resource *niche* — a weighted stance
over several distinct kinds of sustenance: sunlight-fixed plant matter,
grazable forage, animal prey, dead organic matter, and mineral wealth. Two
kinds with different niches should therefore want different ground: a
grazer wants meadow, a mineral-eater wants ore-bearing rock. But the actual
carrying-capacity calculation that decides how much of a kind a given patch
of land can support was, until this campaign, fed only one number — a
single measure of overall plant productivity, the same number for every
kind everywhere. A niche's distinctive weighting could only ever *rescale*
that one number up or down. It could never make the number peak somewhere
else.

The consequence, measured on the seed-42 world before this campaign: two
peoples — goblins and hobgoblins — held every one of 108 settlements. Every
other kind in the roster, kobolds and bugbears among the peoples, and a
dozen fauna including a mineral-eating creature and a tree-dwelling giant
among the wildlife, held not one. Not because the world had no room for
them, but because the arithmetic that was supposed to tell different diets
apart could only tell them apart by *how much*, never by *where*.

The Demesne restores that missing dimension. The supply a patch of land
offers is no longer one shared number; it is a small bundle of numbers, one
per resource kind, each built from the terrain and climate that actually
produce it — plant productivity from the existing climate model, forage as
a documented fraction of that productivity, mineral wealth from the
prospectivity map [The Ground](./the-ground.md) already computed, dead
matter treated as a broad ambient supply. A kind's carrying capacity is now
the honest dot product of its niche against that whole bundle, evaluated
cell by cell — the same formula the habitat model always specified, finally
given the vector of inputs it was designed for rather than a single
borrowed scalar.

## What emerged, measured

With supply restored to a vector, a mineral specialist's niche can finally
point somewhere the plant-productivity specialists never competed for. On
the same seed-42 world, a creature whose diet is almost entirely mineral —
a burrowing elemental — went from holding a single, statistically
meaningless settlement to dominating 29,136 cells, the largest domain any
kind holds on that world. The count of kinds that materially dominate any
ground at all rose from two to four. This was never authored: the same
preregistered measurement that recorded the two-kind baseline before the
fix ran again after it, against a threshold set from theory before the
result was known, precisely so the campaign could not quietly grade its own
homework.

This is the payoff the resource-partitioning model was always supposed to
deliver, arriving for the first time: a creature holds ground *because of
what it eats and where that food is*, not because it happens to share the
one productivity number every other kind is scored against.

## What did not move, and why that is being said plainly

Two honest gaps came out of the same measurement, and both are named here
rather than smoothed over.

**The peoples still do not diversify.** All four of the world's small
peoples — goblins, hobgoblins, kobolds, bugbears — draw their niches almost
entirely from forage and animal prey, and none of them weight mineral,
plant productivity, or dead matter at all. Restoring supply to a vector
only creates new *places to differ* along the axes a niche actually reaches
for; a roster whose members all reach for the same one or two axes still
collapses onto the same ground, no matter how spatial the supply becomes.
Worse, the animal-prey axis is deliberately left at zero this stage (its
real field is the next stage's work, described below), so for this
campaign the peoples effectively compete on forage alone — a single shared
number once again, just a smaller one than before. The goblin-and-hobgoblin
world persists, and the fix is not more tuning of this campaign's
constants; it is that the peoples' own authored diets do not yet reach into
the axes that now vary in space. Getting a diverse, evenly-distributed set
of peoples across the world in a way that is deterministic and does not
depend on the order anything is computed in is its own unresolved design
question, deliberately left open rather than forced.

**The tree-dwelling giant holds no ground either, for an unrelated
reason.** A creature's home range — how much land one individual needs —
scales with its body mass, and the tree-giant is roughly a thousand times
heavier than the small woody-growth specialist it competes against for the
same plant-productivity niche. That size penalty overwhelms the resource
advantage the vector supply gives it long before the two ever meet on
equal terms. This is a body-mass calibration question the resource-axis fix
does not touch, identified precisely and left for its own pass rather than
disguised as solved.

Both gaps were checked by actually running the assertions that would prove
them wrong, not by declining to look: forcing the ignored tests to execute
confirms each fails exactly as described, so the honest accounting is
current, not stale.

## Why this is Stage 1, not the whole story

The habitat model's resource basis has five axes; this campaign gave three
of them (plant productivity, forage, mineral) a real spatial field and left
one (dead matter) at a flat ambient constant, by design. The fifth —
animal prey, the food an actual predator eats — was deliberately held back.
Building it honestly means deriving how much grazing-animal biomass a patch
of land can support from the same plant supply herbivores draw down, and
that is a genuinely different kind of computation: a two-level ecological
one, not another spatial lookup. Predators and the world's dragons still
hold no ground on this world, for exactly the reason a predator should hold
no ground before there is anything represented for it to hunt. That is not
a partial success; it is the next campaign's clearly-named subject, sitting
directly downstream of this one — the map of resources this campaign built
is the surface the prey layer will be computed over.

## What's reserved

- **The animal-prey field and the food web it enables** — the direct
  sequel. Once herbivore biomass is a real spatial quantity, predators gain
  a place to want, the peoples' forage-and-prey niches gain a second real
  axis to differ along, and the settlement count this campaign left
  depressed (81 rather than the historical 108, because prey dropped out of
  every peopled kind's resource credit) has its real fix.
- **Richer, denser biomes** — many plants and animals sharing the same
  landscape, seasonal cycles, predator-prey dynamics, migration — once the
  food web exists to give them a shape to cycle through.
- **A design for territory among the peoples** — not a tuning knob on this
  campaign's constants, but its own question: how an evenly-spread set of
  distinct peoples can emerge in a way that never depends on the order
  anything is computed in.
- **A real dead-matter field** — this stage treats it as broadly ambient;
  a spatial decomposer economy is a refinement for later.

The mineral-eater holding real ground on this world, for the first time, is
this campaign's whole claim. Everything the resource-partitioning model was
always meant to deliver beyond that — a full, densely differentiated
menagerie of peoples and predators each holding their own place — still
waits on the stages this one was built to make possible.
