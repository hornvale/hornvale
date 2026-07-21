# The Freshwater

The world had rivers all along. Deep in the terrain domain, genesis has long
run a flow-accumulation over the sculpted globe and classified every cell —
ocean, salt basin, river, dry land — and marked where a high-drainage
watercourse crosses a scarp into a waterfall. But none of it reached the eye.
The scene the client draws — the tile lattice, the region patch — emitted a
single water bit: *is this ocean?* Everything fresh was computed and then left
in the dark. The Freshwater carried it out.

## The posture: render, never derive

The temptation, when a client wants rivers, is to let the client find them —
trace the elevation downhill, accumulate flow, draw the result. That path is a
trap. It would put a second, unauthoritative hydrology beside the sim's own, and
worse, it would be non-deterministic client work standing in for a derived
truth. The discipline is the same one that governs the whole seam: **the sim
ships numbers, the client renders them.** So the water is a *pure read* — the
scene asks the terrain provider for each tile's water class and drainage, and
for its waterfall sites, and emits them. No new draw, no re-derivation; the
stream manifest does not gain a single label, because nothing new is rolled.

Two properties fell out of the datum's nature and simplified everything. Water
is **static** — a river is a feature of the sculpted ground, not a thing that
moves within a world's life — so it is emitted once, cached with the tiles, no
animation to drive. And a river's **connectivity is implicit in adjacency**: the
scene emits per-cell classes, not a flow graph, but river cells sit next to
river cells, so drawing the cells draws the thread. (A smooth vector river from
an emitted flow-graph is a real refinement, deferred.)

## The additive append

The scene schemas already carry a stability contract — fields are appended,
never reordered, so an old consumer reading a new document simply ignores what
it doesn't know. The water fields joined on those terms: a per-tile water class
indexing a small self-describing legend (ocean / salt-basin / river / dry-land),
a per-tile drainage magnitude, and a list of waterfall points. Integers and
names are exact; drainage and the waterfall coordinates pass the same
quantize-at-emit boundary every scene float already crosses, so the same seed
yields byte-identical scenes as before, plus the new keys. The containment test
that decides which waterfalls belong to a region patch was written
transcendental-free — the algebraic inverse of the cube-sphere projection, dot
products and a division — so it, too, is identical on every platform. Then a new
world-wasm carried the enriched scene to the client.

## Full on the map, major on the globe

Where the water appears was the one real choice, and it followed the shape the
Orrery had just settled into. The globe is an overview held at arm's length; the
flat map is where you fall in for detail. So the map draws the **full** hydrology
— every river cell as a saturated flowing blue keyed to its drainage, every salt
lake as a distinct still teal, clearly neither ocean nor river — while the globe
draws only the **major** water: rivers whose drainage clears a high bar, the
biggest endorheic lakes, a few dozen tiles of the many hundreds, present enough
to say *this world is watered* without cluttering the clean planet. On seed 42
that is ninety-six great rivers and two dozen lakes standing in for the six
hundred and the forty-four the map shows in full.

The waterfalls are emitted and waiting; they are sparse enough that this world's
canonical globe carries none, and drawing them well is the next small climb. But
the rivers run, on both scales, drawn from the water the world knew all along.
