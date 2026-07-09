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
geometry from rotation, obliquity, and season.

**Approximated (declared):** circular orbits; no orbital evolution,
resonance, or N-body effects; seasonal daylight as a smooth sinusoid in
obliquity and year phase; neighbor stars observational-only (no gravity, no
radiation); no eclipses yet (the angular diameters exist, so tier 3 can
derive them).

**Drawn from the seed (or pinned):** star mass; anchor mass and orbital
distance (within the zone); rotation regime and period; obliquity; moon
count, masses, and distances; neighborhood size, classes, distances, and —
as of the star chart — each neighbor's fixed celestial position (declination
uniform on the sphere, right ascension uniform around the circle), drawn
from its own seed stream so every previously generated sky keeps its exact
class, distance, and brightness while gaining a place to plot.

Promoting a drawn quantity to a derived one is an **epoch bump**, never a
silent change — saved worlds must keep the skies they were born under.

**The tier ladder ahead:**

3. Realistic multi-body configurations: binary suns, moons in resonance,
   eclipses (the angular diameters already exist), wanderers, rings,
   constellations as perceived from the surface — and physics promotions
   (drawn → derived) arriving as epoch bumps.

At every tier the query stays the same; only the richness of the answer
changes. A world configured with the tier-0 provider remains a valid,
interesting world forever — that is what fidelity-agnostic means, and the
constant-sun world of seed 42 remains in the gallery as proof.
