# The Wakeful Sun

[The Slumber](./the-slumber.md) gave creatures a day, but a day by fiat: its
wake cycle was a fractional-day window, a creature awake for the light half of
an abstract rotation regardless of where on the world it stood or what season it
was. That was the coarse tier, and it was always meant to be refined. This
campaign swapped its body for the real thing — the wake cycle now reads the
actual sun.

The refinement rides a single new seam. The terrain a creature senses grew a
`solar_altitude` reading — the sun's height above its horizon, degree by degree,
or nothing at all on a tidally locked world that has no day. The default is the
old fractional sun, so the planted terrains of a thousand tests are unmoved; but
a live world's terrain now reaches into the astronomy that was already there —
the solar declination oscillating with the drawn obliquity, the sunrise equation
at a room's own latitude — and returns the true altitude. A creature is awake
when its sun is up, or down, or near the horizon, according to whether it keeps
the day, the night, or the twilight.

From that one read the geography of sleep falls out. Near the equator the day
and night divide evenly all year. Toward the poles they stretch and pull with
the season, until past the polar circles a diurnal creature meets a sun that
never rises — and, finding no waking in its whole scan of the coming day, sleeps
on through the long dark, a hibernation no one authored, waking only if thirst
turns to dying and the survival override drags it up to drink. A world locked
face-to-its-star, with no rotation to carry a terminator, has no such cycle at
all: there the wake-gate cannot fire, and its creatures rest on fatigue alone.

What is quietly satisfying is how little had to move. The calendar is built once
when a session or a health simulation begins and handed to the terrain; the wake
read changes underneath, and everything above it — the arbitration, the fatigue
debt, the rest-in-place, the health metric's waking sample — carries on unaware
that the clock it consults now keeps real time. The eighty-eight tests of the
drive layer passed without a touch; a single line of the health null control
learned that under a real sun and a turning climate a healthy world catches the
occasional momentary blip, and that this is life in a varied world, not an
alarm. The wake cycle is now the coarse truth made fine, and the general engine
it is the first instance of — the oscillators keyed to moons and tides and magic,
the free-running clocks that will drift and jet-lag — still waits, its first
hand now sweeping in real solar time.
