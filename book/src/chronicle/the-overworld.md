# The Overworld

The flat map worked and was not good. The Cartographer had settled that pixel-art
belongs on a plane, not a sphere, and the map rung drew one: a region as a texture
of biome color, a crisp nearest-neighbor grid, a few symbols floating over it. But
it read as what it was — a low-resolution biome fill. Flat blocks of color, hard
blocky coastlines, a washed palette, sparse marks on empty water. Nothing about it
said *craft*. It said *data*.

This campaign asked what a *good* pixel-art map is made of, and the honest answer
is: not one thing. Decompose it and seven components fall out — the base fill, the
texture *inside* the fill, the coastline, the palette, the resolution, the
outlines, the symbols — and the old map had only the first and the last. The
missing five are where the 16-bit overworld lives, the Final Fantasy and Dragon
Quest maps that are the canonical reference. And underneath them sits a single
technique doing most of the work: **dithering**. Ordered dithering adds texture
inside a biome, foams a coastline, harmonizes a palette's banding, and fakes
detail the data does not carry — one move, four payoffs. It is also, being an
ordered matrix indexed by pixel position, perfectly deterministic, which the
constitution demands: no die is rolled to place a dot.

## Procedural craft

The deeper problem is that we cannot draw the map. A pixel artist hand-places
every tile; the sim ships numbers and the client must render them the same way on
every machine, forever. So the craft has to be *procedural* — a program that
emulates, per output pixel, what a hand would do: quantize to a tight palette,
dither each biome between two tones biased by elevation so the texture reads as
relief, detect the land-water boundary and lay a shallows band and a dark shoreline
and a fringe of foam, trace a thin line where two biomes meet. To have room for
that craft the renderer draws to its own texture, finer than the coarse grid of
data beneath it: the detail is invented at render resolution, honestly, from the
coarse truth.

All of that is built. The palette, the Bayer dither, the crafted coastlines, the
biome outlines — each a pure, deterministic function, each tested on known pixels,
each composing over the last. The mechanism for procedural overworld craft exists
and is correct.

## The look is not the mechanism

What the campaign did *not* finish is the look. A renderer can be provably correct
and still not beautiful, and this one, on its first pass, is not yet: the palette
reads pale, the fine render resolution turns the dither into a whisper where a
16-bit map wants a chunky declaration, and the default regions the map opens onto
are open ocean or flat desert — the least flattering ground a landscape renderer
could be asked to draw. These are tuning, not architecture: named constants, a
resolution number, a table of tones, all waiting for the eye that decides when
punchy is punchy enough. That eye is the owner's, not the author's, and that
judgement is deliberately deferred — the renderer ships as the alternate map style,
behind the switch, with the smooth voxel diorama still the default, so nothing
regresses while the palette is dialed in later.

This is the fourth and final turn of the Orrery's remaking — view-switch, voxel
globe, voxel map, and now the pixel map's bones. The program that set out to change
how the Orrery is seen has changed all four of its faces; the last one is drawn but
not yet coloured to satisfaction, and knows it.
