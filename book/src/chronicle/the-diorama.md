# The Diorama

The map had always been flat. The Cartographer had made it — a region seen from
directly overhead, a curated pixel texture on a plane, the orthographic camera
pointed straight down like a hand pressing a chart to a table. It was a map in the
oldest sense: the world reduced to a plane you look down upon. And it was already
orthographic, already the projection a relief model wants. It only lacked two
things: height, and a tilt to see the height by.

The Diorama gives it both. The same voxel blocks The Massing wrapped around the
globe are here laid on a flat grid — one block per cell, the real cliff faces
between elevation bands — and the camera is pitched off the vertical to a fixed
isometric angle. What was a chart becomes a tabletop relief model: a region you
look at rather than down upon, its mountains standing up off the plane, its rivers
sunk below the land in visible steps.

## One renderer, two projections

The deep economy of it is that the voxel renderer turned out to be
*scope-parametric*. On the globe (The Massing) the blocks were wrapped onto a
sphere — each cell's corners projected through the cube-sphere mapping, raised
along the radius. Here the corners are just points on a plane and the height is
just a coordinate. Same block, same wall, same honest one-datum-per-cell
discipline; only the mapping from cell to position differs — a curved global view
and a flat local one, drawn by one builder. The flat case is, if anything,
*simpler*: no curvature, no projection, a plain heightfield. The globe is the
world seen whole; the diorama is the world seen close. They are the same blocks at
two scales.

## What "2.5D" actually is

Strip the idea down and the defining property is not the blocks — the flat map
already had cells — but the *pitch*. A voxel heightfield viewed from straight
overhead is still a flat map; you cannot see that anything stands up. Tilt the
orthographic camera off the vertical and height becomes visible for the first
time. That non-zero pitch, under a projection with no perspective, *is* the "2.5D"
— the half-dimension between the plan and the perspective view. The angle is fixed
here, a stable isometric that keeps the map readable as a chart even as it gains
depth; freeing it to orbit is a later choice.

The height took a tuning the model could not supply on its own. At the relief
exaggeration inherited from first principles the diorama read as a flat tilted
plane — a thousand metres of mountain rose half a percent of the region's width,
invisible. The exaggeration had to be pushed far past physical truth before the
terraces stood up and the cliffs caught the light, the same bargain the globe
strikes: relief on a rendered world is a legibility choice, not a measurement.
Only a screenshot could say when it was enough.

This is the third turn of the Orrery's remaking — after the view switch and the
voxel globe — and it leaves one style behind it: the flat pixel-art map, kept as
the diorama's alternate, still awaiting the rework that is the program's last step.
