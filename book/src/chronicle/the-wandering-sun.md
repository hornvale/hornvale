# The Wandering Sun

A tidally locked world was supposed to have no seasons. Its star hangs
fixed in the sky, one face held perpetually to the light, and the orrery
drew it that way: a hot substellar point pinned to the equator, an ice cap
frozen at the antipode, neither moving as the year turned. On seed 8 —
locked, and tilted nearly twenty-two degrees — that pinned ice sat
conspicuously off from the world's dark hemisphere, offset by about the
obliquity. The sky said one thing, the ice another.

The sky was right. A tidally locked world with axial tilt does not sit
still: over one orbit its sub-solar point *librates*, swinging north and
south in latitude by the full obliquity. The illumination modelled that
libration correctly. The climate did not — it read the substellar point
from a hard constant, `[1, 0, 0]`, the equator, obliquity nowhere in the
locked-world temperature at all. So the hot spot and the ice never moved,
while the terminator swept past them twice a year. The offset the eye
caught was exactly that: a static field beneath a moving sun.

## One phase, two worlds

The fix is a single principle applied on both sides of the rotation
regime: the seasonal temperature must be phased on the *true orbital
season*, `year_phase = frac(day/year + year_phase_offset)` — the same phase
the sun's declination already uses. Two symptoms of one omission fell to
it. Spinning worlds had been phasing their seasonal swing on
`frac(day/year)`, dropping the genesis year-phase offset, so their
temperature season led the sun by that offset — invisible on a
near-upright world like seed 42, a real lag on a tilted one. Locked worlds
had no seasonal term at all. Both now follow `year_phase`: the spinning
sinusoid gains its offset, and the locked substellar point becomes a
function of the day, its latitude tracing `obliquity · sin(year_phase)`
across the year. The hot spot climbs to one tropic at the solstice and
falls to the other, and the ice, which is only where the temperature
crosses freezing, tracks it.

The change is carefully contained. Only the temperature's *seasonal*
evaluation consults the moving substellar point; the annual mean, the
moisture field, and the insolation baseline keep the fixed equatorial one,
because the libration averages to the equator over a year and their annual
means are unchanged. That containment is what keeps the change a
rebaseline rather than an epoch: no draw moves, no ledger fact changes, and
the one census metric that touches temperature reads the annual mean, which
is byte-identical. Spinning worlds' scene tiles are byte-identical too —
their mean and swing coefficients never carried the phase.

## The seam and the position

Because the sim holds no cryosphere, the ice is a client derivation: the
orrery reconstructs the locked world's temperature itself, pinned against a
producer-sourced golden — the cross-repo contract discipline The Isotherm
established. Reproducing the librating substellar in the client exposed a
subtlety that discipline had not yet had to name. The producer's tile grid
samples temperature by snapping each map tile to the nearest cell of its
icosphere mesh; the client, working from the flat tile lattice, evaluates
at the tile's own centre. For a value the client reads pre-computed off a
scene layer the difference is invisible — the snap is baked in. For a value
the client *recomputes* from a closed-form function of position, the two
positions diverge, and the reconstruction missed the golden by up to a
degree. The seam holds for closed-form functions only when both sides
evaluate them at the same point. The resolution keeps the golden and the
client honest by construction: a position-based producer evaluator, sampled
at the tile centres the client actually uses, so the two agree exactly and
the pin stays tight.

## Watching it turn

The instrument that makes the fix visible is the second half of the
campaign. The globe's clock had capped its fast rates — a planet that spins
once a day is a blur at a month a second — so the seasonal cycle, which
moves once a *year*, could never be watched. The cap is lifted, and at the
fast rates the diurnal spin freezes: the planet holds a face while the year
runs forward beneath it, the sub-solar point drifting north and south, the
ice line breathing, the temperature field redistributing. On a locked
world, which never spun, only the sun wanders. Run seed 8 forward under the
temperature lens and the hot spot slides from one hemisphere to the other
and back — the wandering sun the world was always meant to have.
