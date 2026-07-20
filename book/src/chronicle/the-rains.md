# The Rains

The moisture the biomes read had always been a guess made once. A cell's
wetness came from its circulation band, its distance to the nearest sea, and
a single upwind glance for the shadow of a mountain — an honest sketch, but a
static one, blind to where the water had actually come from. The Rains
replaces that one-glance guess with a **budget**: air that picks up water
over the sea, carries it downwind, and rains it out as it rises and as it
travels, so that a continental interior dries because the water ran out
getting there. It is the third campaign of the Weather Program, and the first
to change the field the world is built on rather than add a layer beside it —
so it is an epoch.

## A budget, not a glance

The model traces the air backwards. From each land cell on a spinning world,
walk *upwind* against the prevailing band wind, a bounded number of steps
toward the sea, carrying a running store of precipitable water `W`. Every
step over ocean adds evaporation, scaled by local warmth and capped — the sea
is where water enters the air. Every step where the terrain rises along the
wind lifts the air and rains some of `W` out — the windward slope wets, and
because the store is now depleted the leeward side is dry, a rain shadow that
*compounds* over successive ranges. The rising belts rain a baseline out, and
the store decays a little with every overland step, so the reach from the
coast is finite and interiors dry with distance. What survives the walk is
the water available to that cell; normalized, it is the moisture the biomes
have always read.

The calibration is the physics. An early tuning made the trace too greedy and
dried the world into near-lifelessness — habitability fell by four fifths,
and dozens of hand-authored placements, the goblins and their doctrines,
lost the ground they stood on. The correction was structural, not a knob: the
budget does not *set* the wetness but *modulates* it. The tuned base floor —
band wetness plus an ocean-proximity bonus — is preserved, and the trace
supplies only a drying term subtracted from it, a dimensionless dryness in
`[0,1]`. The world stays habitable because its floor is intact; the rains
carve the deserts into it rather than starting from drought. One bounded,
deterministic trace per cell, every transcendental routed through the shared
library, no solver and no iteration — the acyclic honest version of the
single-pass approximation the model card had confessed to for so long.

## What rains, and where it does not

On the moisture the trace now produces, the campaign hangs the readings a
weather has: a real annual total in millimetres, a validating typed quantity
joining the astronomical units and standard days; a rain-or-snow split from
the temperature field, smooth across freezing; and a seasonal regime —
uniform, summer-max, winter-max, monsoon — as a categorical label from
circulation band and continentality, a name rather than a time series. A
diagnostic cloud fraction, moisture times uplift, marks where moist air
rises. All of it is additive and derived; the biomes keep classifying on the
dimensionless moisture, now transport-fed, so no classifier was rewritten.

The fences are deliberate. The clouds are strictly diagnostic — they dim no
sunlight and warm no night, because that feedback is a loop, and a loop needs
the relaxation machinery this campaign does not build. A locked world, which
has no band winds to walk against, keeps its substellar moisture model
untouched and byte-identical, which also bounds the epoch's churn. And the
phases between rain and snow — sleet, freezing rain, hail — wait for a later
refinement that reads a vertical temperature column; they are named and
shelved, not attempted.

## The cascade, and the merge

Because moisture is the field biomes read, changing it moves everything
downstream of a biome: the deserts spread, the forests retreat to the wet
margins, and settlement, species, culture, and language re-derive on the new
map. That is the definition of an epoch, and it is why every committed
fixture moved at once. This one landed into a crowded main — three other
campaigns had reshaped the same seed between its start and its close — and
the merge was where the epoch met them. The largest settlement's headcount at
the reference seed could not be either branch's pinned value; it had to be
re-measured on the combined world, where a freshwater settlement term and a
new moisture field both pull at once. A seed's kobold, no longer prosperous
enough on the drier map to organize a priesthood, dropped out of two
cross-campaign measurements; its language, re-derived, changed one verb. None
of these were bugs — they were the epoch's cascade reaching exactly as far as
an epoch should, read off the merged tree rather than argued from either half.

## Watching it rain

The client makes the field legible. The orrery reads the annual millimetres
off each tile into a precipitation lens — arid tan through wet green — where
the rain shadows read directly: the dry heart of a continent, the wet coast
that feeds it. Above it drifts a cloud overlay, the cloud fraction shaded
white and swept along the same winds as the currents, seeded and faded and
re-seeded, the living-globe drift once more — bright legible streaks, the
lesson the last campaign paid in specks already learned. Snow whitens the
poles where the split turns cold. And the biome lens, side by side with the
moisture beneath it, shows the epoch plainly: a pale arid interior grading to
green margins, the same shape in every layer, because it is the same field
seen four ways.
