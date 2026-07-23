# The Excursion

The Orrery's flat Map view had a camera that could not move. It framed exactly
one region tile, fixed, and if a visitor tried to look past its edge there was
nothing to see — not ocean, not a neighboring valley, just the render's own
empty background. The four-campaign view-remake program that built the map
(view-switching, the voxel globe, the voxel diorama, the pixel-art renderer)
had left this as a named followup from its very first campaign: pan and zoom
were promised, then deferred while the views themselves got built. This
campaign paid that debt.

Nathan's own first reaction to the natural framing — "just clamp the camera to
the one tile" — was to push back: *we're on a globe, how is there empty space
to pan into?* He was right that the world has no edge. The map's renderer did:
it had only ever mounted one tile's data, so panning past it wasn't reaching a
literal boundary of the sphere, it was reaching the boundary of what had been
asked for. The fix was not a wall. It was to ask for more.

## A ring, not a wall

The map now keeps a small same-face neighborhood of tiles mounted around
wherever the camera is looking — nine tiles at launch, the current one plus
its eight neighbors — fetched eagerly through the same worker bridge the globe
already uses for its own deep-zoom terrain. A wider halo of tiles stays
cached-but-unmounted one ring further out, so panning back and forth across a
boundary doesn't refetch what was just there. Crossing into a neighbor
recenters the ring; crossing toward the edge of the cube face it's on — the
one boundary that is real, where the neighboring tile isn't a same-face
neighbor but a different face of the cube entirely — clamps, because no
adjacency table exists yet to say which face continues where. That absence is
deliberate and named: cube-corner adjacency is a harder problem than cube-edge
adjacency, and stitching across a face seam is its own future campaign,
building on this one's ring machinery rather than blocking it.

The one design decision the whole feature turns on is keeping two different
questions separate. *Where does a tile sit on screen* is answered once, from
the tile that was centered when the region was first opened, and never
answered again — every tile's position is anchored to that one fixed point for
the life of the visit. *Which tiles currently count as "the ring"* is a
different, moving answer, recomputed every time the camera crosses a
boundary. Collapsing those two into one question — "recenter everything on
wherever the camera is now" — is the natural first instinct and the wrong one:
it would mean re-zeroing the coordinate frame under an in-progress mouse drag,
which is exactly the kind of ground shifting under your feet that breaks a
drag gesture mid-motion. Keeping them apart means recentering is pure
bookkeeping — which tiles to fetch, mount, or forget — and never touches the
camera at all.

## What two passes of ideonomy found

The design went through two rounds of structured idea-expansion before a line
of code was written, and neither was a formality. The first, framing the
whole feature as a cycle (open a region, orient the ring, roam the camera,
hit a boundary, depart), surfaced five things a straight-line spec would have
shipped without: the ring should fetch its whole neighborhood immediately, not
lazily on approach, or zooming out right after arriving would show blank
tiles at the very edges the feature exists to fill; a boundary crossing needs
a hysteresis margin, the same shape of fix the globe's own level-of-detail
system already uses to stop a camera sitting on a threshold from thrashing;
and the symbol overlay — the peaks and forests drawn over the pixel-art
style — should only ever appear on the one centered tile, because its density
budget was tuned assuming exactly one tile's worth of them, not nine.

The second pass, lifting the whole design to its bare shape — a bounded
working set with a moving focus, hard-stopped at a real topological
boundary — and dropping it into an unrelated domain (a river's floodplain
against a canyon wall it cannot cross) came back with two more findings the
first pass missed. A boundary that only exists because nothing was ever
fetched past it needs to become an *active* constraint once fetching exists,
or the camera can still wander into a blank frame at the ring's own new edge.
And a cache that only ever adds entries, never large-flow the crowd out, will
grow without bound over a long session — the fix was the two radii, a small
hot ring of what's actually drawn and a slightly larger warm halo of what's
merely remembered, with real eviction past the second one.

## What the whole-branch view caught that no single task could

Seven tasks landed clean, most on their first review. The one substantive bug
in the whole campaign was invisible to every one of those seven reviews for
the same reason: each ran its tests from a freshly-opened region, where the
camera's saved look-at point and the tile grid's own coordinate origin happen
to coincide. Switching map style, or leaving the map and coming back after
having panned around first, is the only path where they diverge — and on
that path, the camera's remembered aim point was never being reset to match
the new frame. A style switch after panning left the view skewed off-axis; a
fresh visit inheriting a stale aim point could immediately recenter itself
onto the wrong tile, once, silently, before a single frame had rendered.

Nobody building or reviewing one task at a time could have found this — there
was no single task where the bug lived, only a seam between four of them that
only the full branch, read end to end, could expose. Finding it, and finding
that the first attempt to fix it corrected the camera's aim but not its
position (leaving a small, real angular skew rather than the wrong-tile
recenter), was the final whole-branch review's whole reason for existing.

## What the eye caught that the tests couldn't

A person watched the finished feature run in a real browser — screenshotting
both styles, panning, zooming out until the whole ring came into view. It worked: nine tiles of real, different terrain
appeared where one had been, in both the flat pixel-art style and the
isometric voxel diorama, and the ocean's leftover wave-mark sprites (a
holdover from before the pixel renderer grew its own coastlines) were
genuinely gone. But the zoomed-out voxel view also showed something no test
had a way to catch: at one tile boundary, where the neighboring region sits at
a different elevation than its neighbor, there's a visible dark step — a real
gap in the geometry, because the voxel renderer only ever draws a wall face
between two adjacent cells *inside* one tile's own grid, never on a tile's
own outer edge. A single mounted tile could never have shown this; there was
never a neighbor for its edge to disagree with. The ring is the first thing
in this client that could expose it, and it does. It is left as a named
follow-up rather than folded into this campaign, on the same reasoning the
face-boundary clamp already established: stitching two independently-built
tiles together at their seam is a different, larger problem than loading them
side by side.
