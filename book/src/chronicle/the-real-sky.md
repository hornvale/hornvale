# The Real Sky

The orrery's night sky was a lie of convenience. Behind the star and its
world hung four hundred points of light scattered by `mulberry32(seed)` — a
cosmetic cloud, seeded only so it would not flicker between frames, bearing
no relation to the sky the world actually has. Yet the sky the world
actually has was sitting fully derived in the ledger, unexported. The
generator draws a handful of notable neighbor stars — their spectral class,
distance, apparent brightness, and equatorial position — and, beneath them,
a background field of one to three hundred fainter stars. The almanac reads
this catalog; the constellation-finder clusters it. The client had never
been given it. This campaign closed that gap: a new scene document,
`scene/neighbors/v1`, and an orrery that paints the world's own stars where
the world actually places them.

## Two populations, one honest split

The document carries the sky in the two forms the sim holds it. The
**neighbors** are drawn physics: two to five bright stars, each backed by
`is-neighbor` facts in the ledger — class, distance, brightness,
right ascension, declination. The **field stars** are derived texture: the
hundred-odd faint points the generator produces on demand from the
astronomy stream and never serializes, the exact population the
constellation-finder draws its members from. The export surfaces both, and
the reference page states which is which, because a consumer that treats a
field star's magnitude with the same weight as a neighbor's drawn distance
has misread the sky. Neither population carries a name: the sim names no
star, so a neighbor's identity for display is its colour and class — "the
amber giant" — the almanac's own convention.

The load-bearing property is that the whole document is a pure read. The
neighbors are already in the ledger; the field is recomputed from the
world's astronomy seed, the same derivation the almanac performs. Nothing
is drawn. The export adds no stream, shifts no draw order, needs no epoch —
worlds stay byte-identical, and the only new bytes in the world are the two
appended moon fields below.

## Where a star is, and which frame says so

A star's position is two angles in a frame, and the campaign's one real
decision was which frame to publish. The ledger holds each neighbor's
**genesis-epoch equatorial** coordinates — right ascension and declination
measured against the world's own rotational equator — and that is what the
document carries, unchanged. It does not pre-rotate them into the orrery's
ecliptic scene frame, because that would mint a second coordinate
convention the ledger never speaks, and push a rendering opinion into the
data. Instead the reference page pins the transform the consumer must
apply, precisely enough to test against: build the equatorial unit vector
`(cos δ cos α, sin δ, cos δ sin α)`, then rotate it about the
vernal-equinox axis by the world's obliquity, and the celestial pole lands
at `(0, cos ε, sin ε)`. The orrery implements exactly that, and unit-tests
the two diagnostic cases so a sign error cannot slip in unseen.

## The moons lean, and one leans the wrong way

Riding with the sky came a smaller correction the moons had been owed since
The Reckoning. `scene/system/v1` described each moon by four orbital
numbers and said nothing of the plane it orbits in, so the client drew every
moon on a flat circle in the world's equator. But a moon has an inclination
and a node, both drawn, both already in the ledger — and since The Reckoning
a captured moon may orbit **retrograde**, its inclination above ninety
degrees. Seed 42 has exactly such a moon, tilted a hundred and seventeen
degrees. The campaign appended the inclination and the ascending node to the
system document — additive, after every existing field, no epoch — and the
orrery now tilts each moon's orbit about its node line. Retrograde motion
falls straight out of the geometry: an inclination past ninety sweeps the
moon the other way, with no special case. The moon inspector, which the
campaign's warm-up had already taught to read `scene/moons/v1`'s mass,
radius, gravity and albedo, gained the last of the physical story — how the
moon formed, its bulk density, and a plain "retrograde" on the card of any
moon that earns it.

## What the render still owes

Opening the actual rendered frame, rather than trusting the tests, is a
discipline this project learned the hard way, and it paid twice here. It
confirmed the real starfield paints where it should — sparse and true, not
the old even scatter. It also caught what a unit test structurally cannot: the
new star shell's celestial pole and the orrery's pre-existing rendered world
axis lean opposite ways along the frame's third axis. At seed 42 the
obliquity is barely a degree and the seam is invisible; on a sharply tilted
world it would open. The starfield's pole is the one faithful to the
published transform; reconciling the older world-axis render against it is
work the sky's arrival newly makes visible, and left as a marked
follow-up — the honest end of a campaign that spent its whole length
refusing to draw a sky the world does not have.
