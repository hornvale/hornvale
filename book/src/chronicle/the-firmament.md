# The Firmament

The climate had a mean but no moods. Every prior campaign of the Weather
Program gave the smooth analytic sky another average — a diurnal pulse, a
current, a rainfall total — but a place's weather on a *particular day* was
not a thing the world could tell you. The Firmament gives the sky its
weather: a sampled synoptic state, read at a place and a day, that a person
standing in the world can feel. It is the fourth campaign of the Weather
Program, and the one that makes the climate *felt* rather than merely
described.

## A state, sampled, never stepped

A place's sky is a small state machine — clear, fair, overcast, rain, storm,
with a high cirrus overlay — and the campaign's whole discipline is *how* it
decides which state a cell is in. It does not integrate the atmosphere
forward in time; a forward-integrated storm is chaotic, and a world that
saves and reloads from a quantized ledger could never reproduce it (the
Lorenz guard-rail the whole program obeys). Instead the state is a
deterministic **sample**. Each cell carries a slow climatological
*propensity* — storm-prone where the air is moist, rising, warm, and near
the sea; clear-prone where it is dry, subsiding, and cool — a pure blend of
fields already computed. Over that prior rides a fast **drifting-Fbm phase**:
a single static noise field sampled in a frame that slides with the day, so
the pattern appears to march west-to-east and the sampled state slides
smoothly from one day to the next. The state machine's forbidden
transitions — a sky *builds* toward a storm, a storm *clears* through rain,
neither blinks — are not enforced by a rule; they *emerge* from the
smoothness of the noise in time. It is a pure function of place, day, and
seed. Nothing steps.

That the cloud a sky wears is the state's own face — cumulonimbus is the
storm, stratus is the overcast, cirrus the high thin precursor over an
otherwise clear sky — means "realistic clouds" required no separate cloud
model: the type is a projection of the state. (A subtle failure hid here:
the precursor cirrus, as first written, could never actually occur — its
window sat just outside the clear-sky threshold, so the overlay was
unreachable dead code. The fix made cirrus a genuine subset of the clear
state, and a reachability test now proves it can be produced by the real
composition, not merely constructed by hand.)

## The weather you stand under

The payoff is felt weather, and it lives where a person is. Possession
narrates the sky overhead — *"the sky is clear but for high cirrus"*, *"torn
by storm, towering thunderheads overhead"* — read at the possessed place and
the current day, so the sky changes as the day advances. The almanac gains a
weather sentence at its sample sites. None of this writes a single fact to
the ledger: weather is a pure observation read, and so the campaign is
**level zero** — every world, every ledger, every census figure is
byte-identical to before. The climate learned to have moods without changing
anything a biome, a settlement, or a history depends on. The two heavier
couplings the design mapped — the persistent storminess that would grow
*weather-gods* in religion, and the disturbance that would move biomes — are
deliberately deferred, each a sequel, so this campaign could stay a clean
observation layer.

## What the globe still owes

The world now produces, for every tile, a storm propensity and the cloud
type overhead — the data a client needs to paint real weather onto the
living globe. Painting it *well* proved to be its own problem. The first
rendering drew clouds as hairlines, which a graphics card caps at a single
pixel wide, so they were invisible except where they stacked at the planet's
edge; redrawn as soft round sprites, they swung the other way and became an
atmospheric grey haze around the globe rather than discrete cloud systems.
The honest verdict was that legible, typed clouds on a sixty-times-relief
globe want a rendering approach this campaign had not budgeted for. So the
felt weather ships — the half a person experiences — and the globe's typed
clouds wait for a campaign that can give them the picture they deserve. The
sky has weather now; the map does not yet wear it.
