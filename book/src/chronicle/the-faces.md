# The Faces

The moons got faces — and a boundary drawn honestly through the middle of each one.

`scene/system/v1` gives the orrery a moon as four orbital numbers: sidereal
period, phase, distance, apparent size. Nothing about the body itself. So the
client drew every moon the same way — a flat gray sphere, distinguishable only
by how fast it moved and how large it loomed. This campaign gave each moon a
surface document a client can turn into a face: `scene/moons/v1`, a new schema
alongside the system one.

The honest heart of it is where the campaign lives. The generator draws a
moon's **mass** and its **orbit** — those are physics, recorded in the ledger.
It has no surface model at all: no composition, no cratering history, no
albedo measurement, and it does not even compute a physical radius. So a
surface document cannot be uniformly truthful, and the design refuses to
pretend otherwise. It splits each moon's entry cleanly into two halves and
labels which is which.

The **derived** half follows from the one physical handle the generator has.
Assume a moon shares Luna's density — a stated assumption, not a measurement,
because composition is never drawn — and mass fixes everything else by
geometry. Constant density makes volume proportional to mass, so radius scales
as the cube root of mass: `radius_km = 1737.4 · mass^(1/3)`, anchored on
Luna's radius. The same cube root falls out of surface gravity, because
`g = GM/r²` with `r ∝ mass^(1/3)` reduces to `g ∝ mass^(1/3)`, so
`surface_gravity = 1.62 · mass^(1/3)` in Luna gravities. Two honest numbers
from one drawn one. The page states the caveat in the same breath: a real
ice-and-rock differentiation model would refine these, and it would arrive as
a new *drawn* composition field — never a silent re-meaning of what these
already say.

The **seeded** half has no generator source whatever. Albedo, cratering,
maria fraction, and a near-gray tint are not simulated and are not measured;
they are *authored* by a deterministic hash of the world seed and the moon's
index, through the kernel's `value_noise_2d` — the same hash-noise the render
lens uses for cosmetic sub-cell texture. This is the load-bearing decision.
Because the descriptors are a pure hash and not a `Stream` draw, the document
consumes **no** random draws: it adds no save-format stream, shifts no draw
order, needs no epoch. A guard test builds the document and asserts the world
is byte-identical before and after — a surface read that cannot, by
construction, disturb genesis. Mass biases the hash so a face reads
plausibly — small moons pull toward saturated cratering, large ones toward
resurfaced maria, the two mutually damped so no face is both at once — but the
bias is authorship, not evidence, and the page says so: procedural detail, not
surface science. A small append-only classifier reduces the descriptors to a
word — `bright-icy`, `maria-rich`, `heavily-cratered`, `cratered-highland` —
for a consumer that wants a name rather than a texture.

The contract crosses two repositories. The producer emits the document from
`windows/scene`, through a `scene moons` CLI subcommand and a `hw_scene_moons`
wasm export, with a drift-checked seed-42 golden pinning every quantized value.
The orrery parses it strictly, and its system view stops drawing gray discs:
albedo sets each moon's base brightness, tint its hue, the physical radius its
true relative size, and a procedural texture — seeded, like the producer's
descriptors, from seed and index alone, so it is identical across every render
of a world — paints cratered speckle against smooth maria. The catalog steps
to `world-wasm-v4`. The two sides meet in a test that instantiates the real
binary, runs genesis for seed 42, and parses what `hw_scene_moons` actually
emits — the binary is the fixture, so producer and consumer cannot drift apart
unnoticed.

What the campaign deliberately did not build stays on the record. There is no
moon elevation lattice — a fabricated heightfield would be exactly the invented
precision The Isotherm and The Region refused, and it is captured instead as a
future explicitly-procedural tier for the day a client genuinely needs a
walkable moon. And a moon's *face* here is appearance, not identity: the
descriptors give a body a look, not the drawn markings, retrograde motion, or
captured-versus-co-formed origin that would make a moon a nameable character
for myth and lexicon. That deeper individuation remains open. This campaign
shipped the visible half of it — honestly labelled as such.
