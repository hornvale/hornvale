# The Cartographer

The Idioms left a door open. Its render-style layer gave the Orrery globe a
gallery of screen-space skins — pixel-art, engraving, watercolor — but it drew
a line it would not cross: those were *filters*, transformations of the finished
frame, and it named the thing on the far side of the line the **mark-owning
cartographic renderer**, the map that draws itself from the data rather than
recoloring a photograph of it. The Cartographer set out to build that.

## The keystone, and the first build

Lift the ambition until the surface falls away and the shape underneath has a
name: *map a continuous field over a manifold to a discrete, salience-ranked set
of symbols, where the viewing scale governs how many symbols and of what kind
are admitted.* That is **cartographic generalization** — the cartographer's
oldest problem, selection and simplification and symbolization under a scale
budget. The negation test says what makes it an *engine* and not another skin:
remove salience-budgeted feature selection per zoom and it collapses back into a
filter. A filter reads finished pixels and has no idea which one is land; an
engine reads the datum and *knows*.

The first build proved the keystone directly on the globe. A data-native base
colored each tile from its own biome and ocean flag — which by construction
killed the bug that had dogged the pixel-art filter, where land quietly took the
ocean's colour because a post-process pass snapping to a palette could not tell
them apart. On top of the base went the symbols: mountain peaks from the local
maxima of the elevation field, forests from connected components of the
forest-biome tiles, wave-marks strewn across the sea, all selected against a
per-zoom budget and drawn as sprites on the sphere.

## The visual pass overturns the medium

It worked, and it was wrong. Zooming and rotating, the wave-marks piled into a
jumbled, detached band at the globe's limb; the pixels stretched and sheared as
the surface curved away. This was not a bug to fix but a medium fighting back.
Pixel-art is a *flat* idiom, and a square pixel on a sphere advertises the
projection's every distortion — the stretch at the poles, the shear at the limb
— far louder than smooth shading ever would. A search of how others solve it
returned a single, unanimous answer: the whole industry of interactive maps —
Mapbox, MapTiler, Google — renders a **flat rectangular map when you are zoomed
in** and morphs to a **globe when you are zoomed out**. Overview far, detail
near, and the detail is flat because flat is where symbols live.

So the campaign pivoted, and the pivot is its lesson. What had been built as
"pixel-art on the globe" was two idioms at two scales mashed into one. Pulled
apart: the globe reverts to a clean, smooth biome overview — a legible planet,
no pixels fighting the ball — and the pixel-art detail moves to a new place.

## The discrete handoff

The right transition was not obvious. The naive reading of the industry pattern
is a continuous morph — the globe literally unwrapping into a plane as you
descend. But a microscope does not morph; it *swaps objectives* with a discrete
click, because each magnification wants a different technique, not a blend of
two. The globe→flat transition is the same: a discrete idiom-handoff, a clean
crossfade between two renderers, each drawing its own scale in its own way. This
was not only the tractable choice — it was already the machinery the Orrery used
between its system and globe rungs. A two-rung zoom controller became a
three-rung one, a ladder position eased along `[0, 2]` with a triangular
crossfade lighting each canvas as its rung comes into view.

## The flat map, where pixel-art finally works

The new **map** rung is a two-dimensional orthographic scene. Zoom into the
globe past its floor and the sub-camera point selects a region tile — the same
higher-resolution patch the globe's own level-of-detail already fetches — and
that region is drawn flat: each node colored by the curated biome palette into a
`NearestFilter` texture, hard pixels edge to edge, on a quad that fills the view.
No sphere, no distortion; pixel-art as it is meant to be seen. The symbols that
had fought the curve now sit still on the plane — peaks and forest-clusters and
wave-marks placed straight from the region grid, and, at the closest detail, the
stereotyped icons of a game-map: a volcano where the crust is both high and
unquiet, a cactus in the desert, a mushroom in the rainforest. Everything the
sphere had rejected — the extraction, the budget, the palette, the sprite
glyphs — transferred to the flat tier unchanged. The pivot wasted nothing.

The map is a first version, and honest about it: which region you land on is
luck on an ocean-heavy world, the detail does not yet deepen as you pan within
the map (the flat rung has no controls of its own yet), and the settlements the
globe names are not yet drawn there. Those are the next rungs to climb. But the
architecture is the one the medium wanted all along — a clean planet you hold at
arm's length, and a flat, drawn, stereotyped world map you fall into when you
lean close.
