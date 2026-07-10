# Campaign 20: Firm Ground II

**July 2026 · outcome: complete, merged — the sky given a past, the observer
given a place, and the ground the rest of Year 3 stands on**

## What was attempted

The Year-3 metaplan opens on the premise that the world has a past, but the
sky as built had none: a single obliquity, a single circular orbit, and a
single day-zero phase, drawn once at genesis and then frozen for the entire
life of the world. Worse, whatever that frozen sky held, the almanac read it
from nowhere in particular — position-blind, the same planet-wide report
regardless of where anyone actually stood. Two deficits, and both needed
fixing before anything could be built on top: a domain reading deep time
would have almost nothing to read, and an observer meant to be honestly
wrong from a real vantage point had no vantage point to be wrong from. This
campaign retired both at once, because both change almanac output and only
one re-baseline should absorb the churn. It was staged to build no
consumer — the point was to lay the two seams and prove them, not to spend
them.

A numbering note belongs in the record. This campaign was planned as
Campaign 19, but a parallel star-chart campaign reached the chronicle first
under that number while this one was still in flight. The chronicle is
forward-only and authoritative — a claimed number is never reused, only the
next one taken — so this campaign renumbers to Campaign 20, and the
reconciliation with that parallel campaign's work is part of what landed.

## What landed

**The sky acquired a past.** Obliquity, eccentricity, and precession — the
Milankovitch triad — stopped being one-time draws and became slow
oscillations over absolute standard days, each anchored so that its value at
`t = 0` equals exactly its old genesis draw. The present sky is
byte-unchanged; deep time is the only place the drift is visible. Obliquity
follows `ε(t) = ε₀ + A_ε·(sin(2π·t/P_ε + φ_ε) − sin φ_ε)`, with eccentricity
and precession built the same way at their own near-real periods. The
obliquity amplitude is not independent of the rest of the sky: a large
stabilizing moon damps the wobble, and a moonless world's tilt swings
through a wider range over deep time — one moon draw, two coupled
consequences. Eccentricity is the genuinely new element: until now
obliquity was the *only* seasonal driver, so a world drawn with zero tilt
had no seasons at all; an eccentric orbit gives even that world an apsidal
season and therefore a year. And genesis day 0, which had quietly been an
unintended grand alignment — every moon new, the year and the day both
starting from zero together — now carries a drawn phase offset per body, so
day 0 reads as an ordinary day instead of a suspicious coincidence.

**The observer acquired a place.** `ObserverContext` gained a real position
on the globe, and the default almanac now observes from the flagship's own
cell rather than from an averaged nowhere. Daylight length stopped being one
planet-wide number and started following the sunrise equation
`cos H₀ = −tan φ · tan δ`: flat at the equator, widening toward the poles,
running to true polar day and polar night at the extremes. And visibility
itself became placed: a spinning world's sky still turns beneath every
observer, so everyone eventually sees everything, but a tidally locked
world's sky does not turn at all, so an observer's hemisphere is a life
sentence — the day side sees the sun and nothing else, the night side sees
the moons and the stars and never the sun. Because the night hemisphere is
uninhabitable, every locked settlement lands on the day side, so a locked
world's pantheon now comes out sun-only by construction, not by authorial
choice. That consequence was not free: it measurably lowered blind
attribution on the divergence census the prior year's capstone had
established, and the campaign owns that shift openly as an honest
re-baseline rather than a quiet one.

**Reconciled with a parallel campaign.** The star-chart campaign landed on
the shared trunk mid-execution, having independently fixed a real physics
defect — the moon's illumination cycle had been read off its sidereal
(orbital) period rather than the longer synodic period an observer actually
watches. This campaign's own per-body phase offset had to compose with that
fix rather than collide with it: moon phase is now read as
`(t / synodic_period + offset).fract()`, the synodic correction and the
genesis phase offset applied together, in that order, with tests asserting
the phase only ever advances. The two campaigns' stream labels, property
batteries, and fixtures were merged rather than one overwriting the other.

**The single re-baseline.** Every committed artifact that either half
touched — the three seed-42 almanacs, the stream-manifest and
concept-registry reference dumps (gaining the new forcing and phase-offset
stream labels and the new eccentricity and obliquity-amplitude predicates),
and the drift and divergence studies (gaining a new obliquity-range metric)
— regenerated once, together, so the diff tells one coherent story instead
of scattering across commits. A second full regeneration left every one of
them byte-identical, proving the re-baseline itself introduced no
nondeterminism.

## What was learned

- **Anchoring drift at the present is what makes a re-baseline affordable.**
  Because every oscillation was built to equal its old genesis draw exactly
  at `t = 0`, adding deep-time behavior to three orbital elements did not
  retroactively change a single already-shipped world's present sky. The
  churn the re-baseline absorbed was real, but it came entirely from the
  *new* things — eccentricity, latitude, hemisphere culling — never from the
  old ones quietly moving underneath.
- **A lowered blind-attribution number is a finding, not a regression.**
  Locked-world pantheons becoming sun-only by construction was a genuine
  consequence of taking the observer's hemisphere seriously, and it changed
  a number a prior campaign had already published. Naming that shift and
  re-baselining against it openly kept the record honest; quietly patching
  the study to preserve the old number would have hidden a real physical
  fact about locked worlds behind a stale statistic.
- **The chronicle's forward-only order is worth defending even mid-flight.**
  Losing the number 19 to a parallel campaign cost nothing but a rename,
  because nothing downstream had cited this campaign by number yet. Waiting
  until close to discover the collision, instead of reserving the number
  early, is what kept the fix cheap.

## Deferred, deliberately

This campaign built no consumer of either seam, on purpose. [Deep Time
(Campaign 24)](./24-deep-time.md) later reads the orbital forcing this
ships; the Year-4 epistemic layer still awaits the placed vantage. Also
untouched: per-body altitude and azimuth (a body stays up-or-down with the
whole hemisphere, never placed at a point in the sky), twilight, each
neighbor's coordinates as seen from a real place rather than an idealized
one, wandering bodies, constellations, stellar variability, binary hosts,
sky colour from spectral class, and retrograde rotation. The bright scope
lines stand: the sky *drifts* the elements it already holds and gains no new
bodies; the observer sees a hemisphere, not an altitude-azimuth sky.

## Artifacts

[The Astronomy chapter](../domains/astronomy.md) — now at tier 2, with the
moving sky and the placed observer as their own sections and the model card
carrying every new quantity in exactly one column: the drift laws and the
moon-coupling as derived and approximated, each forcing element's mean,
amplitude, and phase as drawn, and the observer's position as derived from
placement. [The Sky of Seed 42](../gallery/the-sky.md) and [The Gods of Seed
42](../gallery/the-gods-seed-42.md) — the spinning and locked almanacs,
regenerated against the placed observer and drift-checked on every build.
The `census-lands-drift` study — the obliquity-range metric, holding the
moon-coupling calibration over every build alongside the five prior rows.
