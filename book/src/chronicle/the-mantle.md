# The Mantle

The Firmament made the sky felt but left the globe unable to wear it — its
closing line was a debt: *"the sky has weather now; the map does not yet wear
it."* The Mantle pays it. It is a client campaign, no sim change at all: the
world already emits, for every tile, the kind of cloud overhead. What was
missing was a way to *draw* it that read as clouds rather than haze.

## Why the first attempt could not have worked

The Firmament's deferred cloud render drew each cloud as a billboarded soft
sprite, and the honest verdict at its visual pass was that the result was an
atmospheric grey shell, not weather. The instinct is to blame the primitive —
the particles, the graphics card's one-pixel line cap — and the primitive was
indeed wrong. But an ideonomy pass negating the shell's properties one at a
time found a deeper cause: the failure was rendering clouds as *untextured
blobs of colour*. A flat-coloured shell would have failed identically. The
load-bearing move — the keystone the whole campaign turns on — is **fbm noise
gated by cloud type into an alpha channel**: soft, irregular cloud *edges*
with a character that differs by type. Get that, and a planet-standard
primitive reads as clouds; skip it, and any primitive reads as haze.

## A texture on a shell

So the render is the standard planet cloud layer, the same shape the code
already used for its moons: a transparent sphere just above the globe's
exaggerated relief, wearing a generated equirectangular texture. The tile grid
is itself a latitude-longitude field, so the texture generator is a pure
function of it — for each texel, read the tile's cloud type and its storm
propensity, and write an alpha that is fractal noise thresholded against a
per-type coverage: dense and dark for a cumulonimbus, a broad grey sheet for
stratus, faint high stretched streaks for cirrus, nothing at all where the sky
is clear. The alpha is capped so even the heaviest deck stays translucent —
the surface must read through the gaps, a cloudy planet from orbit rather than
an overcast shroud. The shell's own solid far hemisphere, hidden behind the
opaque globe, occludes the back side, so the halo that plagued the sprites
cannot form. It drifts slowly, a longitude offset advancing with the clock —
the living globe, claiming nothing the sim pins, deterministic data worn as
non-deterministic weather.

## The map wears it now

The visual pass — this time disciplined by the last campaign's own lesson,
that the capture server serves a built bundle and a stale one silently blinds
the tuning loop — showed banded cloud systems with soft textured edges,
gathered over the storm-prone latitudes the propensity field marks, the
continents and biomes reading plainly through the breaks between them. The
grey where the weather is genuinely overcast is not a defect but the honest
face of a rain-band; the fair-weather patches read white, the storm cores
dark. The planet wears its weather. The debt is paid.
