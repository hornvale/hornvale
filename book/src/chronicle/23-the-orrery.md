# Campaign 23: The Orrery

**July 2026 · outcome: complete, merged — the first Hornvale picture that
moves, and the first that carries a color**

## What was attempted

Two campaigns earlier the sky acquired a past: its tilt, its orbit, and its
moons stopped being frozen at genesis and began to drift, cycle, and swing over
deep time. A campaign after that gave the world a scene to emit — the numbers
of its surface, for something else to draw. But between the two sat a plain
want the workspace had never answered: to simply *look* at the system, in
color, and to watch it move. Every picture drawn so far was still — a map, a
chart, a single frozen night sky. A world whose moons now cycle on their real
synodic months and whose seasons breathe over a year deserved to be seen doing
it.

This campaign draws that, and answers the harder question hiding underneath —
how a project that will never link a graphics or video library makes a video
at all. The answer keeps the seam the rest of the project already lives by: the
simulation emits deterministic bytes, and anything heavier lives outside it. So
the workspace hand-rolls the one container it needs — a terminal recording — and
leaves the transcode to open-source tools beyond its edge.

## What landed

**A sky with a temperature.** The star chart's night sky, until now, was
plotted in plain digits: a star was a rank, not a color. The renderer now
carries its own small palette — a spectral class mapped to a terminal color,
hot blue-white through cool red — and tints every star by it. The mapping is
the renderer's alone, a paint decision that belongs to whoever is drawing and
to no one upstream: the domain still keeps the prose it always did, a
"smoldering red star" for the almanac to read aloud, while the chart takes the
same star's class and chooses a hue. The anchor sun is classified from the
spectral letter it already announces, so the world at the center of the picture
is colored by the same rule as the neighbors scattered around it.

**A system you can see from above.** The orrery is a top-down schematic drawn
straight from a world at a chosen instant: the star at the center in its class
color, a dotted ring marking the habitable zone, the world set on its orbit at
the exact phase of its year, and each moon in a tight ring around it wearing the
glyph of its current synodic phase — new, waxing, full, waning. It is a
function of time and nothing else, so evaluating it across a span *is* the
animation: the world walks its orbit, the moons turn through their faces, and —
run across deep time — the whole geometry drifts as the forcing dictates. The
schematic fits itself to any system, because the world's orbit is scaled to a
fixed fraction of the frame no matter how wide or tight the real orbit is, so
nothing ever wanders off the edge.

**A recording the world writes itself.** A render evaluated frame by frame over
a time range is fed into a small new encoder that writes an asciinema
recording — a plain text-and-JSON transcript of a terminal session, a format
the workspace can produce with the two serialization tools it already allows and
nothing more. Its timing is synthetic: frame *k* is stamped at a fixed interval,
never at a wall clock, so the same seed and the same span produce the same
recording to the byte, every time. That determinism is the whole point — the
recording joins the book's gallery as a committed artifact and is checked for
drift on every build exactly like a map or a chart. A moving picture is now just
another deterministic file the world is on the hook to reproduce.

**A year, playing in the book.** The book carries one such recording: a full
year of seed 42's system, the world circling and the moons cycling, embedded on
its own gallery page and played in the browser by a self-contained player
committed alongside it — a static asset, not a dependency the workspace builds
against. From the same recording, an open-source tool outside the workspace
produces an mp4 for anywhere a terminal player will not go. The picture is made
in the workspace; the transcode happens beyond its edge; the seam holds.

## What was learned

- **A video need not be a video dependency.** The instinct that emitting frames
  and stitching them externally is a kludge was right — but the fix was not to
  pull a codec into the workspace. It was to hand-roll the one lightweight
  container the animation actually needed (a terminal recording is just timed
  text), let it be a deterministic committed artifact like every other picture,
  and leave the heavy transcode to tools that already exist outside. The
  constitutional line and the good ergonomics turned out to be the same line.
- **A recording must speak the raw terminal's discipline, not the cooked
  one's.** The first recording looked right when printed live and broke when
  played back, because a normal terminal quietly turns a line feed into a
  carriage-return-and-line-feed and a raw replay does not. A committed animation
  is played through a bare terminal emulator, so it has to carry the full line
  ending itself — the kind of gap that a clean build never reveals and only
  watching the picture does.
- **Color is paint, and paint belongs to the drawer.** Keeping the spectral
  *class* in the world and the *color* in the renderer meant the same star could
  be a red word in the almanac and a red glyph in the chart without the domain
  ever learning what red looks like — the same separation that lets the world
  emit a scene and a client choose how to draw it.

## Deferred, deliberately

The interactive picture — a roguelike view of a system a person can steer,
scrub, and inhabit — is not here and is not meant to be: it lives outside the
workspace, in its own repository with its own dependencies, driving the world as
a subprocess. This campaign builds the *in-process* half — the still renders and
the self-made recording — and stands as the reference the situated scene will
later formalize for that external client to read. No mp4 or animated-image
encoder inside the workspace; the recording is the artifact, and the transcode
is someone else's job. And no new bodies in the sky: the orrery draws the star,
the world, and the moons the domain already holds, and nothing it does not.

## Artifacts

[The Orrery of Seed 42](../gallery/orrery-seed-42.md) — a year of the system in
motion, embedded and played in the book, regenerated and drift-checked on every
build. `hornvale orrery` — one frame to the terminal, or a whole span to a
recording. The color star map, joining the star chart's night sky. The
[astronomy chapter](../domains/astronomy.md) carries the renders' place in the
tier ladder.
