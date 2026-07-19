# The Single Sculpt

The terrain of a Hornvale world is expensive to make. Plates drift, boundaries
collide, crust thickens and thins, rivers cut and lay down sediment — the whole
tectonic pipeline runs to turn a seed into a globe. It is also, by design, a
*pure* function of that seed: run it twice and you get the same globe, byte for
byte. Which is exactly why it was wasteful to run it, during a single world's
genesis, four separate times — and, when rendering that world's almanac, seven
more.

The waste was not a bug in any one place; it was a *disconnection*. Terrain is
built once and then flows through the middle of genesis — the climate stage
sculpts it, and the stages after reuse what that stage hands them. But the flow
had two broken edges. The terrain stage that commits the world's landform facts
sculpted a globe, harvested its facts, and then *threw the globe away*, forcing
the next stage to build an identical one from scratch. And the deep-time stage,
reaching for the same terrain to read its ancient glaciations off, could not see
the copy already in scope and sculpted its own. Each rebuild produced a globe
bit-for-bit identical to one that already existed a few lines above.

The fix was to reconnect the flow, not to cache it. Hornvale already had the
right idiom — small helpers that take a pre-built value instead of re-deriving
it, used wherever the composition root threads a climate or an era through a
loop. This campaign extended that idiom to the leaking sites: the terrain stage
now *keeps* the globe it sculpts; a `climate_from` and a `paleoclimate_from`
accept that globe rather than rebuilding it; and the almanac's dozen line-
renderers each gained a variant that takes the terrain and climate the render
already holds. The plain `terrain_of` and `climate_of` remain, unchanged, for
the standalone callers that genuinely build a world once and ask a single
question.

Because the pipeline is pure, none of this could change what a world *is* — the
kept globe is provably the same object the discarded rebuild would have
produced, so the proof of correctness is simply that every committed artifact
regenerates unchanged. It did. Genesis dropped from four terrain sculpts to one
and grew a third faster; the almanac dropped from seven to one and rendered in
under a third of the time. The world is identical to the byte; only the making
of it got quicker.

It is a small campaign with a plain moral. A value that is pure and expensive
should be built once and passed by hand, not rebuilt wherever it happens to be
needed — and the cheapest way to honor that is to notice where a thing is
already flowing, and simply not cut the wire.
