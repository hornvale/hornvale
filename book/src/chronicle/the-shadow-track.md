# The Shadow Track

Eclipse Seasons had already made a world's eclipses real: dated events, each
with a ground track swept across the surface and a recurrence ladder behind
it. None of it had a client contract. The Orrery scrubs a year of sky time
and draws the globe, and it could not show a single eclipse — the events
existed in the producer and nowhere a viewer could reach them. The Shadow
Track is the seam that carries them out: a new scene schema, and a consumer
that marks the year's eclipses on the day scrubber and lays each solar
eclipse's shadow band on the globe where the sim says it fell.

## A query, not a snapshot

Every scene schema before this one is a snapshot — give it a world, receive
the one document that world has. Eclipses are a series in time, so
`scene/eclipses/v1` is a *query* instead, the same shape the addressed
tile scene already established: a client asks for a closed day window
`[from, until]` and the document echoes the window back with the dated
eclipses inside it, day-ascending. Each event carries its day, which moon
cast it, whether it is solar or lunar, whether total or annular, and — for
solar events only — a ground track: the latitude band the umbra swept, the
sub-solar longitudes where the crossing began and ended, its duration. Lunar
events carry an explicit null track, because the anchor's shadow at a lunar
eclipse falls over the whole night hemisphere, not along a swept band; they
are clock marks, not globe bands, by construction.

The whole thing is a pure read. `eclipse_events` and `ground_track` are
closed-form over the already-derived star system and calendar; the scene
layer computes nothing new, draws no random stream, and changes no ledger
fact. Every float quantizes at the emit boundary like every other scene, so
the documents are byte-identical across platforms. No epoch, no census
regeneration — the only new bytes in the world are the new documents
themselves.

## The marks and the band

On the client the events take two forms. The day scrubber, an unmarked
ribbon until now, gains a tick per eclipse in the displayed year, shaped and
colored by body and kind; clicking one opens a card naming the eclipse.
The placement math is a pure function — day over the scrubber's extent — so
the one thing jsdom cannot see, whether the marks are actually visible and
clickable over the slider track, is the only thing left for the eye.

The globe gets the shadow band: for a solar eclipse whose day the clock is
near, a semi-transparent strip along the ground track's latitude band,
spanning the sub-solar longitude arc, glued to the rotating surface at the
same geographic coordinates the settlement markers use. It is a static band
in this version — a solar eclipse's crossing lasts hours against a year-long
scrubber, so what a scrub-to-the-mark reveals is where the shadow was, not
its motion; the animated west-to-east sweep is a deliberate later campaign.

## What the eye caught

The band nearly shipped invisible. Its geometry was correct — unit tests
confirmed every vertex fell inside the right latitude band and along the
right arc — and it mounted on the right node, riding the planet's spin like
the markers. But it sat at a radius just barely above the sphere, and the
globe exaggerates relief sixtyfold so that mountains read at all. The band
was beneath the mountains: occluded from every camera angle, only its limb
fragments ever showing. No unit test and no headless DOM can see an
occlusion; the campaign's own closing visual pass could, and did. Lifting
the strip above the tallest possible exaggerated peak makes it clip-free
from any angle — it reads now as the umbral shadow hovering over the track,
which is the honest picture anyway, since a shadow falls from above. The
defect was exactly the kind the producer-side "did anyone draw this?"
discipline exists to catch, run this time at the source rather than a
campaign later.

The closing review sharpened a second habit. The plan had called, by
reflex, for a committed producer-sourced golden of the eclipse document —
the cross-repository contract discipline the earlier scene campaigns
established. But that discipline was built for values the client
*recomputes*, where a golden pins the client's arithmetic against the
producer's. An eclipse document the client merely *parses* is already
proved end-to-end by the fixture test that reads the real compiled binary;
a second committed copy pins nothing and only adds a thing to drift. The
golden is the right instrument for a re-derivation and the wrong one for a
faithful read, and the seam is better for knowing the difference.
