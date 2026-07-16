# Goldengrove

The orrery died today, twice, and was reborn somewhere better.

The terminal orrery — the ANSI schematic that drew the star, the
habitable band, and the moons at their phases into a grid of glyphs — and
the live in-book orrery that succeeded it were both removed this campaign,
verb and renderer and client and committed bundle together. Not because
they were wrong: the ANSI render taught the workspace that a solar system
was worth seeing, and the live client built the two scene kinds and the
golden ephemeris contract everything since stands on. They were removed
because they were never quite at home. A drift-checked book exhibit must
be small, deterministic, and dependency-free, and "serious eyecandy" keeps
pulling the other way. The tension was resolved by moving the picture out
of the house.

Where it moved is [Goldengrove (since renamed Orrery)](https://hornvale.github.io/orrery/) —
a separate repository in the organization, the first true external client
the sim has ever had. The arrangement is the observatory and the
planetarium: the observatory grinds the mirror and publishes a catalog;
the planetarium is a different building that consumes it. Concretely, the
catalog is `clients/world-wasm` — Hornvale's worldgen and scene windows
compiled to a half-megabyte wasm module behind seven `extern "C"` exports,
built and released by CI under versioned tags (decision 0055). The
planetarium fetches a pinned release, runs genesis *in the browser* for
any seed in the URL, and renders `scene/system/v1` and `scene/tiles/v1`
in three.js: the system with its true periods and phase offsets, the
globe with its real elevation and biomes and a day/night terminator
computed honestly from obliquity and year phase — frozen, truthfully, on
tidally locked worlds.

The boundary between the two buildings is the campaign's real product.
Everything inside the hornvale repository stays deterministic,
serde-only, drift-checked; everything in the client is licensed glamour —
free cameras, starfield sprinkle, client-side randomness that never
re-enters the ledger (decision 0022's split, now with a repository line
drawn along it). The seam is held by byte-identity: the golden smoke
generates seed 42 through the wasm and asserts both scene documents equal
the native CLI's output byte for byte — and the Linux-built release
binary reproduces a macOS-built native's JSON exactly, which is the
libm and quantization decisions (0033, 0041) paying their rent a
repository away from where they were minted.

Some things deliberately survived. The star chart and the atlas are
different instruments and keep their posts. `scene/system/v1` outlived
its first consumer — built for the old client, it is exactly the
catalog's food. And the old client's ephemeris mathematics crossed over
whole: the same golden fixture that pinned a Deno test now pins a vitest
one in the new repository, so the two-language contract became a
two-repository contract without losing a digit.

The render still tells one knowing lie — orbits draw as circles traversed
uniformly, because the scene kind does not yet carry the eccentricity the
sim computes. That widening is queued as the first deliberate schema
change under the new cross-repo contract. The orrery is dead; long live
the orrery, with a URL you can hand to anyone.
