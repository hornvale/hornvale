# The Isotherm

The map learned to answer *when*.

`scene/tiles/v1` already carried the shape of a world — elevation, ocean,
biome, plate, unrest — but every value was timeless. A tile was a place,
never a place *in its year*. This campaign added the climate the tickets
had been asking for: a mean temperature and a seasonal swing per tile, a
moisture index, the world's circulation-band count, and the length of its
year — five fields appended to a document whose field order is a contract,
so they went at the end, and the schema stayed `v1`. Nothing about a world
changed; the sim already knew all of this. The work was to *say* it, and to
say it precisely enough that a second repository could build against the
words alone.

The seasonal swing is where the contract earned its keep. Temperature over
the year is `mean + swing·sin(τ·frac(day/period))` — a single sinusoid the
sim already computed and threw away at the almanac's edge. The tempting
shape was to ship it as an amplitude and let each client re-apply the
hemisphere sign, but a client only knows a *tile's* latitude, and a tile
near the equator can sample a cell on the other side of it; the sign and
the data would disagree exactly where seasons invert. So the swing ships
**pre-signed** — positive north, negative south, zero on a locked or
untilted world — and the client never touches a hemisphere again. The same
instinct made the document self-contained: the seasonal period rides in the
tiles document itself, not borrowed from the system scene, because a
constant-sun world has tiles and no system at all. Winds took the honest
minimum — one band count and a documented derivation, not a per-tile vector
field pretending to a variance the model does not have. Ice took none of
the schema: it is a client derivation from the two temperature fields, a
freeze line the viewer draws, because the sim has no cryosphere yet and a
schema should not promise one.

The consumer is the proof. The orrery re-pinned to a fresh `world-wasm-v2`
catalog, parsed the five fields strictly, and grew a seasonal-ice overlay
on the globe — tiles whitening below freezing, advancing and retreating
with the clock, opposite-phased across the hemispheres, and dark on the
night side because the ice is blended under the honest terminator, not over
it. On seed 42 the ice barely moves, and that is correct: seed 42's world
is tilted less than a degree, so its seasons are a third of a Celsius deep.
Pin a real obliquity and the ice breathes with the year. The render tells
the truth the physics holds.

The contract test is the campaign's spine, and the final review is where it
was won. The first cut of the cross-repo equivalence test froze its golden
temperatures by running the *client's* formula over the wasm layers, then
checked the client against them — a mirror admiring itself. It passed, and
it proved nothing. The fix gave the producer a `temperature_grid` that
samples the sim's actual `temperature_at` per tile, and pinned the golden
to *those* numbers, so the orrery now witnesses the Rust seasonal shape and
would break if either side's formula drifted. The values did not change —
the transcription had been faithful all along — but a test that cannot fail
for the right reason is not a contract test, and the whole campaign was
about contracts. The evaluator now lives, identical, in three places the
book names normatively: the Rust that computes it, the page that documents
it, and the TypeScript that reconstructs it.
