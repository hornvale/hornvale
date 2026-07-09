# Astronomy

**Questions it answers:** What does the sky look like from here, now? What
celestial phenomena would an observer notice, how often do they recur, and
how much attention do they demand?

**Tier 0 — the constant sun.** A golden sun hangs fixed at zenith; it has
never been seen to move. That is the entire implementation, and it is not a
placeholder joke: it is the *Zork* model of a sky, and its phenomena output —
a single celestial body, maximally salient, with no period — is already
meaningful input downstream. What religion develops under an eternal noon is
a legitimate question, and seed 42's goblins have an answer.

**What it emits.** Astronomy contributes no facts and touches no fields at
tier 0; it exists purely as a phenomena source. This makes it the cleanest
example of the trace protocol's read side: everything downstream knows the
sky only as *salience-ranked phenomena with periodicities and character* —
never as orbital mechanics.

**Tiers 1–2 are live (Campaign 2).** New worlds default to the generated
sky: an anchor-first star system (main-sequence star with a derived
habitable zone; a world placed in it by construction; moons admitted past
stability inequalities; notable neighbor stars), a **calendar** translating
absolute time into the world's own days, seasons, and moon phases (tidally
locked worlds truthfully have no local-day column), and a time-varying sky
whose phenomena carry real periodicities. Everything is **pinnable** for
controlled experiments (`--moons 2+1`, `--rotation locked`, `--obliquity
none`, `--neighbor blue-giant`), pins live as facts in the world's ledger,
refusals become genesis-note facts, and `scout` searches seeds explicitly —
the seed itself is never a means. The generator's model card (derived vs
approximated vs drawn) lives in the Campaign 2 spec; chronicles:
[2a](../chronicle/campaign-2a.md), [2b](../chronicle/campaign-2b.md).

**The star chart.** Every neighbor's position, once drawn, is worth looking
at rather than only tabulating: `hornvale star-chart` renders the fixed
sky as a plain-text chart and a full-color planisphere pair, plus a
phase-cycle strip per moon. [The Night Sky of Seed
42](../gallery/star-chart-seed-42.md) is the domain's committed gallery
artifact, regenerated and drift-checked on every build.

## The moving sky (Firm Ground II)

Everything above draws a single obliquity, a single circular orbit, and a
single day-zero phase, then holds them fixed for the life of the world.
Firm Ground II lets three of those constants breathe: obliquity,
eccentricity, and precession — the Milankovitch triad — each become slow
oscillations over absolute standard days rather than one-time draws,
anchored so that at `t = 0` every one of them equals exactly its genesis
value. The present sky is byte-unchanged; the drift only shows up in deep
time.

Obliquity follows `ε(t) = ε₀ + A_ε·(sin(2π·t/P_ε + φ_ε) − sin φ_ε)`, a
sinusoid whose amplitude and phase are drawn and whose period `P_ε` sits
near the real ~41,000-year cycle, expressed in standard days. Eccentricity
and precession are built the same way, oscillating with periods near
~100,000 and ~21,000 years respectively. Eccentricity is genuinely new:
until now every orbit was circular and obliquity was the *only* seasonal
driver, so a zero-tilt world had no seasons at all. An eccentric orbit adds
a second, tilt-independent driver — the world's distance from its star now
varies over the year — so even a world drawn with zero obliquity has a
year, an apsidal season swinging between near and far.

The obliquity amplitude is not drawn independent of the rest of the sky: a
large stabilizing moon damps the wobble, while a moonless world's tilt
swings through a wider range over deep time. One moon draw now has two
coupled consequences — the same stabilizing figure the domain already
computes for tidal strength also bounds how far obliquity is allowed to
wander.

The last piece has nothing to do with drift: genesis day 0, until now, was
an unintended grand alignment — every moon new, the year and the day both
beginning at zero together. Each body now carries its own drawn phase
offset, so day 0 reads as an ordinary day rather than a suspicious
coincidence.

None of this adds a body to the sky. The forcing drifts the elements the
domain already holds — the sun, the moons, the neighbors are exactly as
many as before — and nothing yet reads the deep-time drift: paleoclimate
consumes it in Campaign 20.

## The placed observer (Firm Ground II)

Every almanac so far has observed from nowhere in particular — position-
blind, reporting one planet-wide sky no matter where anyone actually
stood. `ObserverContext` now carries a position on the globe, and the
default almanac observes from the flagship's own cell, so the sky it
reports is the sky of a real place.

Position changes daylight first. Daylight length now follows the sunrise
equation `cos H₀ = −tan φ · tan δ`, where `φ` is the observer's latitude
and `δ(t)` is the sun's declination, itself set by the current obliquity.
The old planet-wide daylight number is gone: the equator keeps a flat half
day year-round, daylight length swings wider approaching the poles, and
the true polar day and polar night appear at the extremes. The almanac now
reports the flagship's own daylight range rather than an average of
nowhere.

Position also changes which bodies are visible at all. A placed observer
sees only what ever rises where they stand. On a spinning world that is
everything — the whole sky turns beneath a rotating observer, so every
body rises and sets in its turn regardless of longitude. On a tidally
locked world, though, the sky itself does not turn, so an observer's
hemisphere is a life sentence: the day side sees the sun and nothing else,
the night side sees the moons and stars and never the sun. This has an
honest and slightly grim consequence for locked worlds — because the night
hemisphere is uninhabitable, every settlement lands on the day side, so a
locked world's pantheon ends up sun-only by construction, not by choice.

What the observer does not yet get is a position *within* the visible
hemisphere: a body is up or down with the hemisphere as a whole, not
placed at an altitude and azimuth the way a real horizon would place it.
That refinement, and how meaning-making reacts to a real vantage point at
all, is later work.

## The model card

Every quantity the generator touches sits in exactly one column — mirrored
here from the Campaign 2 spec at the close ritual, so the book states
plainly what is physics, what is approximation, and what is dice.

**Derived (real formulas):** stellar luminosity (mass–luminosity, L = M^3.5);
the habitable zone (0.95√L–1.37√L AU); every orbital period (Kepler III —
the year from the orbit, each moon's month from its distance); a moon's
*synodic* month — the illumination cycle an observer actually watches,
converted from that orbital (sidereal) period against the year length —
which is what moon phase and the count of months in a year read, never the
sidereal period itself; moon angular diameters; relative tidal strengths
(m/d³, Luna = 1); neighbor apparent brightness (inverse square); day/night
geometry from rotation, obliquity, and season, now resolved to the
observer's own day/night hemisphere rather than a planet-wide clock; the
moon-coupling of the obliquity amplitude (a large stabilizing moon damps
the wobble); latitude daylight from the sunrise equation.

**Approximated (declared):** circular orbits **at genesis** — every world
still starts at `t = 0` on a circular orbit, but eccentricity now oscillates
over deep time, so "circular" is a starting condition, not a permanent one;
no orbital evolution, resonance, or N-body effects; seasonal daylight as a
smooth sinusoid in obliquity and year phase; neighbor stars observational-only
(no gravity, no radiation); no eclipses yet (the angular diameters exist, so
tier 3 can derive them); the Milankovitch drift laws themselves (slow
sinusoids at fixed, near-real periods, with no coupled climate feedback
driving them); ever-visible hemisphere culling (a body is up or down with
the observer's hemisphere, never placed at an altitude); the substellar
point fixed on the prime meridian for tidally locked worlds.

**Drawn from the seed (or pinned):** star mass; anchor mass and orbital
distance (within the zone); rotation regime and period; obliquity; moon
count, masses, and distances; neighborhood size, classes, distances, and —
as of the star chart — each neighbor's fixed celestial position (declination
uniform on the sphere, right ascension uniform around the circle), drawn
from its own seed stream so every previously generated sky keeps its exact
class, distance, and brightness while gaining a place to plot; and — as of
Firm Ground II — each forcing element's mean, amplitude, and phase
(obliquity's amplitude and phase, eccentricity's mean, amplitude, and phase,
precession's phase) and each body's genesis phase offset (the year, the
day, and each moon's), all on their own labeled streams so every draw above
is untouched.

Promoting a drawn quantity to a derived one is an **epoch bump**, never a
silent change — saved worlds must keep the skies they were born under.

**Derived from placement:** the observer's position — the flagship's cell
coordinate — is not drawn at all; it falls out of wherever settlement placed
the flagship, deterministic because placement is.

**The tier ladder ahead:**

3. Realistic multi-body configurations: binary suns, moons in resonance,
   eclipses (the angular diameters already exist), wanderers, rings,
   constellations as perceived from the surface, and each neighbor's
   coordinates plotted as seen from a real place rather than an idealized
   one — plus the observer's own per-body altitude and azimuth (so a body
   is placed at a point in the sky, not merely up or down with a
   hemisphere), and twilight. Physics promotions (drawn → derived) continue
   arriving as epoch bumps.

At every tier the query stays the same; only the richness of the answer
changes. A world configured with the tier-0 provider remains a valid,
interesting world forever — that is what fidelity-agnostic means, and the
constant-sun world of seed 42 remains in the gallery as proof.
