# The Vantage

The Cartographer had taught the Orrery to fall from the globe into a flat map by
zooming past a threshold — wheel in far enough and the sphere crossfaded to the
region beneath the camera. It worked, but it conflated two things that are not
the same. Zooming is a continuous act — *how close am I* — and it is answered by
a single number, the camera's distance. Choosing a view is a discrete act —
*which representation am I looking at* — and it deserves a name, not a distance.
Overloading the wheel to do both coupled them: you could not zoom the globe all
the way in to study it without being flung into the map, and you could not reach
the map without a zoom you might not want.

## Two axes, pulled apart

The Vantage separates them. **Which view** — System, Globe, Map — is now an
explicit choice, a dropdown; **how close within it** is the wheel, and only the
wheel, clamped to each view's own limits. The dropdown owns the discrete axis;
the wheel owns the continuous one; neither reaches into the other. The smooth
crossfade the ladder animates between views is kept — a good transition is worth
having — but it is now *triggered* by the choice, never by a zoom that wandered
into a dolly limit. With that, a whole apparatus fell away: the detection of
dolly floors and ceilings, the handoff intents, the special wheel listeners that
watched for a crossing. The navigation got simpler by getting more explicit.

## Where the map looks

One question the old design had answered by accident: which region does the map
show? Under the zoom-handoff it was wherever the camera happened to be pointing
when you crossed the threshold. The Vantage keeps that intuition but decouples it
from the zoom — when you pick Map, it renders the region at the globe's current
center-facing point, the surface you had turned to face. Orient the globe, then
choose Map, and you arrive where you were looking. (If you jump straight to Map
without ever opening the globe, it falls back to a default tile rather than
faltering.)

This is the first step of a larger turn in how the Orrery will be seen — a voxel
globe, a voxel map, a reworked pixel-art renderer all wait behind it — and it is
deliberately the foundation: settle *how you move between the views* before
rebuilding what lives inside them.
