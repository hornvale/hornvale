# The Lens

For three campaigns the producer ran ahead of the view.

`scene/tiles/v1` had grown to carry a world's elevation, ocean, biome, plate
partition, tectonic unrest, moisture index, and the two coefficients of a
seasonal temperature — and the orrery drew about half of it. `moisture` was
parsed into a typed field with no consumer anywhere in the client. `plate`
and `unrest` had shipped since the schema's first version and the client had
never learned to read them at all. The sharpest case was `circulation_bands`,
which feeds `windAt()`: a fully implemented, tested, normatively specified
evaluator that had rendered **zero pixels** since The Isotherm shipped it.
The debt was emergent rather than designed — every campaign to date pushed
the producer forward, and only temperature ever found a renderer, as the ice
overlay. So the cheapest payoff on the board was not new data. It was drawing
the data already being sent, and this campaign wrote no producer code at all.

Strip the graphics away and a render mode is a pure function from a tile and
a day to a colour. The claim that organizes everything else is that the
realistic view *already was one*: `ocean ? elevationColor : biomeColorForName`,
hardcoded in the mesh builder. It is not a privileged ground truth that data
modes decorate; it is a lens that happens to look like a photograph. So it
registers as `natural` beside the others and receives no special case, and
there is no base mode — only a registry, each lens owning its own colormap,
legend, and caption, so the HUD renders any registered lens generically and
never learns what a lens *is*. That is the constitutional rule about domains —
adding one must never require editing an existing one — re-instantiated
client-side. Six lenses shipped (`natural`, `topographic`, `temperature`,
`moisture`, `unrest`, `plate`) plus a prevailing-wind overlay, and a seventh
would cost one file.

The plate lens is where the map-making got interesting. Seed 42 has sixteen
plates; the validated palette has eight slots. One colour per plate is
therefore impossible, and cycling `plate % 8` would not merely look bad — it
would *lie*, because two adjacent plates drawing the same colour read as one
plate. The way out is to notice what a plate id actually is: an arbitrary
per-world label, with no meaning across worlds, no order, and no magnitude.
Colour's job here is **separation, not identity** — the classical
cartographic answer, and it arrives with a theorem attached. Build the
adjacency graph from the tile lattice, colour it greedily in degeneracy
(smallest-last) order, and a planar map is guaranteed to need at most six
colours, which removes the need for any cycling fallback. Seed 42 came out in
**four** — the four-colour theorem turning up unannounced inside a render
loop — and the boundaries are inked as a second encoding.

The palette beneath it was computed, not chosen. A colour-vision validator
checks palette-order-adjacent pairs, which is the wrong test for a map: in a
choropleth *any* two regions can share a border, so all twenty-eight pairs
were measured against the application's actual background. Blue and violet
are **ΔE 2.5 apart under protanopia** — effectively the same colour. They are
perfectly safe in the palette's canonical slot order, where they are never
neighbours, and catastrophic on a map, where adjacency is the world's choice
and not the designer's. Violet and magenta were excluded on that measurement,
not on taste.

Three honesty constraints shaped the roster more than the colours did.
`moisture` is labelled "moisture", never "rainfall": the producer's spec is
explicit that the layer is a dimensionless index and that a mm/yr calibration
would be invented precision — the friendly, legible name is the dishonest
one, and it would launder that invention into the most-read affordance in the
UI. Ice *and* the ocean gate to `natural`, because a data mode does not carry
decorative presentation; the translucent sea veils about three quarters of
seed 42, and the tiles beneath it hold real data — sea temperature, moisture,
the plate id under the water, unrest along the oceanic boundaries. And the
`unrest` caption discloses what the producer's own model card declares about
the field: derived and approximated, a static present-day snapshot classified
from instantaneous plate motion, with no accumulated stress history behind
it.

What the campaign learned, it learned by looking. Three command-line gates
ran green — the test suite, the typechecker, a headless browser smoke — over
a HUD that was invisible and unclickable in a real browser, built without the
one class that grants `position: absolute`, so it painted behind the canvas
and only programmatic clicks ever reached it. They ran green over a
temperature map that rendered beige: a ±40 °C clamp parks nearly every tile
of a habitable world at the pale midpoint, and the globe's directional light
*multiplies* vertex colours, so it tints as much as it dims — a near-white
midpoint under a warm G-class star renders tan. The palette validator that
had cleared that midpoint checks a colour against a *background*; nothing
checks it against a multiplier. Tightened to ±30 °C the lens resolves into a
real field, on a world whose median surface temperature measures −9 °C, which
is simply what a cold world looks like; the percentile fit that would have
stretched the range to fill every world was declined, because colours that
stretch to fit stop meaning a fixed temperature. And an ice test that had
passed since it was written turned out to assert nothing, proven by mutation:
killing ice everywhere passed 240 of 240, including the test named for it.
jsdom has no layout, no paint order, and no hit-testing. For a campaign whose
deliverable is pixels, rendering it and looking at it is not ceremony — it is
the only gate that sees the thing.
