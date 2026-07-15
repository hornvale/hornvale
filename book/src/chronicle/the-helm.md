# The Helm

The planetarium got its controls today, and its right name.

Goldengrove shipped as an exhibit: a scripted camera, one playback rate
tuned for the year and catastrophic for the day, and a row of buttons of
which a suspicious number did nothing at all. That was honest scoping —
the campaign that built it spent its budget on the seam, the catalog, and
the two views — but an instrument you cannot steer is a film, not an
instrument. This campaign was the difference: every control on screen now
does what it says, and the camera belongs to the person watching.

The organizing idea came out of an ideonomy pass over the issue list:
scroll-to-zoom, the shipped system-to-globe transition, and the someday
first-person surface view are not three features. They are one axis —
**camera altitude** — and the client already had a two-rung ladder with a
scripted elevator between the rungs. So the work was to hand the ladder
to the user: orbit and zoom freely at either rung, and wheeling past a
rung's limit rides the same eased transition the view-toggle button
always drove. The wheel became a third way to ask an existing question.
The ladder's lower rungs — down through low flight to standing on the
terrain — are captured as the sequel, and everything built here is shaped
to take them.

The clock followed the altitude. The old default swept a year in twelve
seconds, which at the globe meant thirty rotations per second of blur;
the fix was not a slower app but a *per-rung* speed policy — each
altitude has its own default, its own cap, and its own memory of what
you last chose, and the blur rates are simply not offered where they
blur. The same shape repaired True Scale, which had been a button wired
to nothing: each rung retracts its own admitted lie. The globe drops its
sixty-fold relief exaggeration to honest one-to-one; the system view
moves the moons from their legible schematic rungs out to their true
distances — where, at astronomical scale, the bodies all but vanish
against the orbit's sweep. That vanishing is the lesson the toggle
teaches, and the new camera is what makes it explorable: zoom in and go
find the world. One honesty held firm in the implementation: the scene
documents carry no absolute body radii, so true scale draws bodies at
*stated reference radii* — Earth's, Sol's, Luna's — and the caption says
so, because a render that admits its lies does not get to invent data to
retract them.

Two smaller returns of the same instinct: the flat blue marble in the
system view was replaced by the same cube-sphere surface the globe wears
— one world, one face, at every altitude, spun by its real rotation
phase — and everything named on screen became inspectable. Click a
settlement and get its ground truth sampled from the tiles; click a moon
and get its period, distance, and days to next full, computed from the
same golden ephemeris that positions it. The numbers were always behind
the pixels; now the pixels answer.

The rename rode along. The client is **Orrery** now —
[hornvale.github.io/orrery](https://hornvale.github.io/orrery/) — chosen
over the campaign-codename it launched under, and executed the first day
after launch because GitHub Pages URLs do not redirect and a rename only
gets more expensive. And the campaign's one process mechanization closed
the loop the last retrospective opened: a Playwright smoke now boots the
real client in a real browser, under the real deployed sub-path, in CI
before every deploy — because the only production bug Goldengrove
shipped was in exactly the seam no headless check could see.
