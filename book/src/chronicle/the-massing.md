# The Massing

The globe had always been a smooth thing. Elevation sampled from the terrain
field, interpolated between samples, draped over a sphere as continuous relief
with analytic normals — a photograph's worth of smoothness, and a photograph's
worth of a lie. Between any two data points the surface invented a gradient it
had no evidence for. Zoom in far enough and the invention showed: not as
blockiness, which would at least be honest, but as a smooth hill that the
simulation never actually computed.

The Massing gives the globe other ways to be drawn. Not other things to *show* —
that is the lens's job, and the lens (natural, topographic, temperature, and the
rest) only ever changed color. This is a second, orthogonal axis: **how** the
surface is built, chosen from a dropdown beside the lens. Four styles now answer
that question, and the world can be turned to face whichever one reads best.

## Four ways to hold the ground

**Smooth** is the globe as it was — kept exactly, byte for byte, so nothing is
lost by gaining the others. **Faceted** flat-shades that same mesh into
crystalline low-poly, a single material flag. **Terraced** quantizes the
elevation into bands before it displaces the sphere, and the surface steps like
rice terraces — a contour map given depth. And **Voxel** rebuilds the ground
entirely: each data cell becomes an extruded block, and wherever a cell stands
higher than its neighbor, the wall between them is drawn as real geometry.

That wall is the whole point. It is what separates a voxel world from a merely
faceted one: the *exposed vertical face*, the cliff, the place where the
discretization stops being hidden and becomes something you can see the edge of.
Remove the walls and voxel collapses back into low-poly. Keep them and the globe
admits, honestly, how much it actually knows — one block, one datum, no gradient
between them that the simulation didn't compute.

## One coin, two faces

This honesty is why the campaign's second half — *zoom in much deeper* — was not
a separate campaign at all. A voxel globe at a coarse level of detail looks
crude, because it is showing you, faithfully, that the detail is coarse. The
smooth globe hid that; the voxel globe cannot. So the two asks were one coin: the
render that makes resolution visible, and the deeper zoom that gives it more
resolution to show. The ceiling on how close the camera may fall was lifted, and
the camera's own floor lowered to meet it — derived from the same level-of-detail
constants that govern when a tile subdivides, not from a fresh guess. The finer
tiles that the region machinery could already produce, but the client had been
declining to ask for, are now requested as you descend.

None of this touched the simulation. No new number crosses the save boundary, no
seed is drawn differently, no world is serialized any other way. The styles are
pure functions of the scene the producer already ships — the client's own way of
looking, waived from determinism because nothing downstream depends on how a
pixel was lit. The voxel honesty does leave one question hanging, deliberately:
now that the globe will show you exactly where its resolution runs out, we will
be able to see whether the *simulation's* own terrain resolution — not the
client's — is the next thing worth deepening. That is a question for a later
campaign, and the instrument to answer it now exists.

This is the second turn of the Orrery's remaking, after The Vantage taught it to
switch views by choice rather than by zoom. The voxel map, and the reworked
pixel-art renderer, wait behind it.
