# Deep Time

Two campaigns ago the sky learned to drift: its tilt wanders on a slow
sinusoid, its orbit breathes in and out of a circle, and its axis wheels
through a precession cycle — three orbital elements moving over a million
years where every previous campaign had frozen them at genesis. Nothing yet
looked back along that drift. A world's present was still, in a real sense,
its only moment: climate, biome, and habitability were all read off the sky
as it stands today, and the world's oldest fact was whatever the almanac
said at day zero. This campaign gives the world a past. It re-runs climate
across that same million years of orbital drift, and reads out of it the
marks a real glacial history leaves on a living globe.

## The driver: a caloric summer, not a thermostat

The mechanism a world's climate actually answers to is not "how tilted is
the axis" in isolation — it is the old Milankovitch–Köppen insight that
what matters for an ice age is how warm high-latitude *summers* run,
because a cool summer is what lets the previous winter's snow survive into
the next one. The caloric-summer index this campaign computes is exactly
that quantity, in closed form: it rises with the world's own obliquity
above its own long-run mean, and rises again with eccentricity scaled by
the sine of precession — the "climatic precession" term that lets an
elongated, precisely-aimed orbit make one hemisphere's summers warmer than
the other's even at fixed tilt. All three orbital elements enter, and the
index is measured as an *anomaly* against the world's own baseline tilt,
not a fixed Earth-like reference: a cold-tilted world and a hot-tilted one
are each judged against their own normal, so the same driver applies
however a given world's sky happened to be drawn.

## The memory: an ice sheet with hysteresis

A single number — global ice volume — carries the climate's memory across
that million years, and it does not move in lockstep with the driver. Ice
grows slowly whenever the caloric-summer index sits below a cold threshold,
and melts rapidly whenever it rises above a warm one; between the two
thresholds sits a dead band where the ice sheet simply holds its ground.
That asymmetry — slow to build, fast to collapse — is what produces the
glacial *sawtooth* real ice cores show, rather than a smooth sinusoid
tracking the driver directly, and it is the mechanism by which a weak,
slow 100,000-year eccentricity wobble can end up dominating the visible
record even though it is the smallest of the three orbital terms: the ice
sheet's own nonlinearity, not the size of the forcing, decides which cycle
wins.

Ice volume in turn drives two further effects, both proportional to how
much ice has accumulated: a global cooling offset, standing in for the
albedo an ice sheet adds to the planet's reflectivity, and a fall in sea
level, standing in for the ocean water an ice sheet locks away on land.
Grow the ice and the world cools and the sea retreats; melt it and both
reverse.

## Where the cold lands: an absolute snowline

A single global cooling number would, applied on its own, either freeze
the whole world at once or leave it entirely bare — every cell would carry
the identical offset, so there would be nothing to distinguish a
Vale from a pole. What actually decides whether a given patch of land
glaciates is an absolute reading: the present temperature field already
varies with latitude, warm at the equator and cold toward the poles, and
a cell glaciates once that varying field, cooled by the era's offset,
crosses a fixed freezing point. The uniform offset does not paint the
whole map white; it pushes a real, latitude-shaped snowline outward and
back as the millennia pass, exactly as a real glaciation advances from the
poles rather than falling everywhere simultaneously.

What the world actually keeps is not "this cell was ever cold enough" but
*advance beyond the present* — the cells a given era iced that are not
already iced today. A world that runs cold at the poles by nature is not
thereby credited with a glacial history it never had; only the additional
reach a real glaciation would have added counts. That distinction is what
keeps a world with no orbital drift honest (below), and it holds regardless
of how cold that world's present climate already happens to run.

## The strata

Marched across the million-year window and re-sampled at a few dozen
climate eras, the driver, the ice sheet, and the snowline diagnostic
together leave three durable marks on the present globe:

- An **ice-extent envelope** — the union, across every era, of the land
  that glaciation reached at some point in the past: places a walker
  standing on the present relief would never guess had once lain under
  ice.
- A **fossil shoreline** — the band of land swept by the sea's retreat and
  return as ice locked up water and let it go again: never permanently
  dry, never permanently drowned, a tide-mark left by a sea level that
  moved for reasons that have nothing to do with the shape of the land
  beneath it.
- **Refugia** — the places that stayed habitable straight through the
  coldest moment the world experienced, the ground a people's ancestors
  could have stood on even at the depth of an ice age.

A world's glacial maximum — the single coldest era of its million years —
is recorded as a day and a fraction of land iced; a representative fossil
shoreline and a representative refugium each earn a short, place-tagged
line; and if any of it happened at all, the world now carries a plain
narrative fact: the frost retreated. That fact is what lets the world's own
memory speak its glacial past back in the same voice it uses for anything
else it remembers — a committed truth, not a phrase written once and never
revisited.

## The zero-forcing proof

A world whose sky was pinned to hold its orbital elements perfectly still
is the standing control for this whole campaign, decided before a line of
the model was written: flat forcing must produce a flat glacial record,
provably, not merely by inspection. It does. With no drift in obliquity,
eccentricity, or precession, the caloric-summer index sits exactly at zero
every era — not approximately, exactly, by construction — which means it
never leaves the dead band, ice volume never moves off zero, and every
era's diagnosed climate is bit-for-bit the world's own present. Nothing
advances beyond what the world already shows today, so the ice-extent
envelope is empty, the fossil-shoreline band collapses to the sea's single
present stand, refugia are simply "everywhere habitable now," and the
frost-retreated fact is never committed at all. A world with no history to
read shows none — the cleanest evidence this campaign's physics does what
it claims, for every seed that carries the pin.

## A deliberate departure from the original plan

The campaign that first set this year's course sketched a simpler model: a
lagged, threshold-driven pass over the orbital elements, chosen for its
cheapness rather than its fidelity. Building toward it, the case for the
real Milankovitch–Köppen mechanism — the caloric-summer index, the
nonlinear ice sheet, the hysteresis that produces the sawtooth — was strong
enough that this campaign set that simpler plan aside in favor of the model
above. It costs more: a genuine forward march through a million years of
history rather than a single lookup, and a nonlinear integrator whose
order of operations is now itself part of what a saved world must
reproduce exactly. What it buys is a glacial record that answers to the
real physical driver, not a stand-in for it — the difference between a
world whose ice ages are dice-rolled to look plausible and one whose ice
ages fall out of the same orbital mechanics that drive them on a real
world.

## What stayed still, on purpose

The solid earth does not move. Plate tectonics, mountain-building, and the
slow rebound of land once an ice sheet's weight lifts are all out of reach
here; elevation is exactly what terrain generated at the world's founding,
and paleoclimate only ever varies temperature, ice, and the level of the
sea across that fixed relief. A fossil shoreline moves in this campaign
because the sea rose and fell — never because the ground itself rose or
sank, as a real one sometimes also records.

Nor is the ice sheet spatial. What this campaign diagnoses is a single
global volume, translated cell by cell into an advance-or-not verdict
through the snowline; it is not a per-cell ice field with its own local
growth, its own melt, its own margin lagging the forcing by years or
centuries at a given latitude. That richer picture — an ice sheet with real
geometry, not a diagnosed shadow of one number — is left open on purpose:
the strata this campaign commits are facts, and the record a future,
fully-spatial tier would produce is free to refine what this one found
without ever contradicting it or asking any of today's consumers to
change. A coarser answer today, a finer one later, both honest about which
they are.

## Artifacts

[The Deep Time of Seed 42](../gallery/paleo-seed-42.md) — the fossil
shoreline, the refugia, and the ice-extent envelope drawn over seed 42's
present relief, regenerated and drift-checked on every build. `hornvale
paleo-map` renders the same strata for any world, to the terminal or to a
picture. The seed-42 almanac carries a Deep Time section reporting the
glacial maximum in the world's own prose. The [astronomy
chapter](../domains/astronomy.md) notes paleoclimate as the first consumer
of the drift it ships; the [paleoclimate
chapter](../domains/paleoclimate.md) carries the full model card.
