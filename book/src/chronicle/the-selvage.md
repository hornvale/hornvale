# The Selvage

*The Excursion* closed by naming a defect it had exposed but not caused: in
the Orrery's isometric voxel map, a wide dark band ran along a tile boundary,
and a viewer could see straight through the world to the page behind it. The
diagnosis was recorded with the follow-up — the voxel renderer only ever
draws a wall face between two cells *inside* one tile's own grid, never on a
tile's own outer edge, so a real elevation step at a seam has no geometry
filling it. Nine tiles mounted side by side were the first thing in this
client that could expose it.

That diagnosis was correct. It was also the smaller half.

This campaign began by looking at the thing before designing anything for it,
and the picture disagreed with the story. A missing wall should show a gap
wherever two tiles disagree in height — which is to say, along both directions
the tile grid runs. The dark band appeared along one direction only. The
other was clean.

## Which way a row runs

A region arrives from the simulation as a square lattice of sample points,
laid out row by row. Which direction is "increasing row" is not a free
choice: the producer builds the lattice by walking a parameter from one edge
of the tile to the other, and that parameter is the same one that counts the
tiles themselves. Increasing row within a tile therefore moves in exactly the
direction that increasing tile index moves between them. The two must run the
same way in the rendered world, or the tiles are being glued together at the
wrong edges.

Better still, the arithmetic that places a tile's *last* row of samples and
the arithmetic that places its neighbour's *first* row produce the identical
number — not approximately, identically. Adjacent tiles genuinely share their
boundary line. Whatever a seam ought to look like, it ought to be continuous
to within the thickness of one elevation terrace.

The map's voxel renderer honoured that convention faithfully. The code that
decided where to *put* each tile of the ring ran it backwards. So every seam
in that one direction joined a tile's far edge to its neighbour's far edge —
two lines that are nowhere near each other on the real planet — and the
resulting cliff was not a missing wall at all but an invented one, a
discontinuity that should never have existed at any height. The other
direction had no such error, which is why it looked fine, and why the dark
band ran along a single screen diagonal.

The other map style, the flat pixel-art one, had been correct the whole time.
Its texture is uploaded with its rows flipped, which puts the first row at the
top of the image, so its rows run *downward* and its tiles stack downward to
match. The two styles hold **opposite signs for the same underlying rule**,
because they orient the same lattice differently — and that is exactly the
kind of fact that looks like an inconsistency to tidy up, right up until
tidying it up puts the seam back. It is now written down where the two
conventions meet, with the reason.

## Three places, not one

The instinct on finding an inverted sign is that the fix is one character.
Structured idea-expansion, run over the cross-product of *every place this
codebase declares an axis convention* against *every way an axis convention
can be wrong*, said otherwise. The rule was stated in one place and then
**re-derived by hand in two more** — once by the logic that stops the camera
panning off the edge of the loaded neighbourhood, and once by the logic that
decides which tile the camera has drifted over and therefore which tiles to
fetch next. Both had the negation written into them inline.

Correcting only the first would have moved the tiles to the right places and
left the camera fetching the neighbourhood *behind* the direction it was
travelling. That failure is invisible in a picture. Everything would look
continuous and correct; you would simply find, on panning far enough, that
the world had stopped arriving.

So the rule became a single statement with a stated inverse, and all three
places now read it rather than restate it. That consolidation, not the sign
itself, is the durable part.

## A plinth, not a stitch

With the seams joining the right edges, the defect *The Excursion* had
actually named came into view at its true scale: scattered dark specks a
terrace or two deep, wherever a genuine cliff happened to land on a boundary.

The obvious repair is to stitch — to teach each tile about its neighbour so
it can draw the exact wall the step requires. It is even unusually tractable
here, because a tile already holds the shared boundary line and could compute
its neighbour's edge without asking anyone. The design considered it, and
rejected it for something blunter: every cell at the edge of its own tile now
drops a wall all the way down to a common floor, whether or not anything is
next to it.

The blunter answer wins on two counts. It closes every case the precise one
does, because the camera is fixed: from a single unchanging angle, the only
seam that can show a gap is one where the farther tile stands higher, and the
wall that fills it is exactly the wall the farther tile now drops. Everything
below is hidden behind the nearer tile's own ground. And it keeps each tile a
thing that can be built alone, knowing nothing about its neighbours — which
matters because the neighbours arrive whenever they arrive, and a renderer
that needed them would have to rebuild finished tiles every time another one
landed.

The side effect is the better half of the result. Where the neighbourhood
*ends*, nothing hides those walls, so the whole nine-tile block now reads as a
slab of crust sitting on a table, with real thickness and cut sides, instead
of the sheet of paper it was before. That was not a compromise accepted; it
was the shape asked for, once the choice was put plainly.

## The premise that turned out to be false

The argument above has a load-bearing clause: *because the camera is fixed*.
Reading the finished work end to end — the one pass with standing to ask
whether a thing holds from every starting condition, rather than from the one
condition each piece was tested in — found that it isn't.

The camera's controller keeps its position and its aim point in step by
computing the distance between them, moving the aim, then restoring the
distance. The logic that stops a pan at the edge of the loaded neighbourhood
moves the aim point directly and leaves the position where it was. Every
frame a drag presses against that limit, the gap between them grows a little,
and the "fixed" isometric angle quietly shears. It is unbounded, it is
reproducible against the edge of a cube face, and nothing in the machinery
that disables rotation ever sees it, because nothing is rotating.

The plinth survives this anyway — but only by luck of a decision made for a
different reason. The design had chosen to drop walls on all four edges of
every tile rather than only the two the fixed camera can see, on the
belt-and-braces grounds that the geometry should stay correct if the viewing
angle ever changed. It changes. That hedge is now the only thing standing
between a sheared camera and holes in the world, which is worth knowing
before anyone economises the two invisible walls away.

The shear is not this campaign's to fix — it, and two siblings of the same
shape, were already there. All three come from one root: four separate places
move the camera's aim point, and only one of them remembers to bring the
camera with it. Which is, precisely, the shape of defect this campaign spent
its first half repairing on a different axis.
