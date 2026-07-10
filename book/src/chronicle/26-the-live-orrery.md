# Campaign 26: The Live Orrery

**July 2026 · outcome: complete, merged — the gallery's first computed,
animated picture**

## What was attempted

The terminal orrery, three campaigns back, drew the system from above and
recorded a year of it — but the recording was baked: a terminal's frames,
timed and fixed, with each moon standing in for a whole quarter of its month
under one of four glyphs. A world whose moons had just been fixed to face
their star correctly deserved better than four buckets holding a continuous
motion. This campaign asks the harder question directly: not a better
recording, but a picture that computes itself, live, in a reader's own
browser — smoothly animated, scrubbable to any instant of the year, drawing
the world not as a schematic dot but as its own real globe of continents.

## What landed

**A new kind of thing for the simulation to emit.** Alongside the tile
lattice a reader can already fetch and pan across, the simulation now emits
a system's orbital elements as their own committed document — a star's
class and habitable zone, a world's orbit and year and day and tilt, each
moon's period and phase and place in line. Nothing about positions over
time, no colors, no glyphs: elements, the same posture the tile lattice
already took toward its own numbers. A reader's browser is trusted to turn
those elements into motion; the simulation's job ends at the numbers.

**A page that computes instead of plays back.** A dependency-free client,
sibling to the book's map viewer, reads that document together with the
world's committed terrain and draws the whole system on a canvas: a
glowing star tinted by its class, the world's orbit swept to its year's
exact phase, and each moon rendered as a true computed disc — its dark and
lit hemispheres divided by a terminator that always faces the star, never
approximated to a quarter-month bucket. The world itself is drawn as an
orthographic globe wearing its own generated continents, spinning at its
real day's rate, tilted at its own obliquity, a geometric line of day and
night sweeping across it as it turns. A reader can play the year, pause it,
scrub to any point, and change the speed — the still schematic finally
becomes a thing to watch rather than only look at.

**Two languages agreeing on one clock.** The moment a second language
computes a body's position from the same elements the simulation already
evaluates in its own calendar, there are two places the math could quietly
drift apart. Rather than trust the two to agree by construction, the
simulation now writes out a small set of sampled instants — its own
positions and phases at each one — and the browser's evaluator is checked
against those samples directly, to a tolerance far finer than a pixel could
ever show. The two implementations are pinned to each other, not merely
hoped to match.

**The old recordings retire; the render stays.** The two baked recordings
that once played in the gallery are gone, replaced outright by the live
page rather than kept alongside it. The terminal render that drew them is
untouched — a legitimate plain-text picture in its own right, no longer
the book's showpiece. The gallery page itself changed only in what sits
behind it: the same star, the same moons, the same world, now drawn by
arithmetic instead of remembered frames.

## What was learned

- **A protocol proves its shape by being read twice.** The tile lattice's
  contract survived a second, quite different reader without bending: a
  document built for tiles and one built for orbital elements sit side by
  side, fetched by the same page, each answering only to its own shape.
  Nothing about the elements document needed to anticipate a canvas, a
  globe, or a scrub bar — it only needed to state the physics plainly and
  let the reader decide what to draw.
- **A continuous quantity deserves a continuous picture.** Four glyphs
  standing in for a moon's whole month was always a compromise the terminal
  demanded, not a truth about the moon. Once the client could compute a
  disc's terminator directly from the same phase the calendar already
  tracks, the four buckets simply had no reason to survive — the choice
  was never between a nicer glyph and a plainer one, but between an
  approximation and the real curve underneath it.
- **A test can pin a browser to a calendar it never runs.** Two languages
  computing the same physics is a standing invitation for them to drift
  apart quietly, each internally consistent and mutually wrong. Sampling
  the simulation's own evaluations and checking the browser's arithmetic
  against them closed that gap before it could ever open — the same
  discipline a byte-identical rebuild already gave every other artifact in
  the book, applied to a second language instead of a second run.

## Deferred, deliberately

The exocentric top-down view is not the only pole this world has been
waiting on: an observer standing at a place, looking up at a moving sky,
remains unbuilt, and this campaign is careful not to foreclose it — the
orbital-elements document is a distinct kind from whatever a situated sky
will eventually need, not a stand-in for it. The globe's spin is a display
convention, an arbitrary seam chosen for the picture, not a claim about
which longitude the simulation calls noon; a tidally locked world simply
holds one face toward its star rather than receiving a fabricated spin.
Deep time's slow drift in tilt and orbit is shown at its year-round mean,
not animated across ages, and a moon's distance from its world is drawn in
schematic proportion, never true astronomical scale.

## Artifacts

[The Orrery of Seed 42](../gallery/orrery-seed-42.md) — the system, computed
live in the browser from the simulation's own committed data, playable,
scrubbable, and speed-adjustable. The [astronomy
chapter](../domains/astronomy.md) carries the live client's place beside the
terminal render it retired from the gallery.
