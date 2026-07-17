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

**The moving sky in color.** The system view — the star in its spectral
tint, the habitable zone, the world on its orbit, each moon at its synodic
phase and lit limb — now lives in [Goldengrove (since renamed Orrery)](https://hornvale.github.io/orrery/),
an external 3D client built against the same committed `scene/system/v1`
and `scene/tiles/v1` documents (decision 0022 — the sim emits bytes,
clients render pixels).

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
many as before — and the [paleoclimate domain](./paleoclimate.md) is the
first consumer of the deep-time drift: it re-runs climate across a million
years of that same forcing and reads a glacial history out of it.

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

## The night sky becomes an instrument (Night Sky Instrument)

Everything above tells an observer *that* a body is up; this campaign gives
the fixed stars a placed, epoch-honest position and adds two new kinds of
body that actually move against them. Three pieces land as one derivation
trio. `sky_position.rs` gives `precession_at` — computed since Firm Ground
II, read by nothing — its first reader: apparent coordinates now drift
between epochs, re-projected each time through the epoch's own obliquity,
so a star's charted position and its position twenty thousand years later
genuinely differ. `night_sky_at` is the unified derived view a placed
observer's night now holds in one query — which neighbors are visible,
which never set (circumpolar) or never rise, and whether a pole star
currently exists (a system-level fact, independent of the observer's
latitude, since precession retires and crowns pole stars as the epoch
turns). And `heliacal_events` recovers the oldest naked-eye instrument
there is: a star's annual disappearance behind the sun's glare and its
reappearance at dawn, gated by a class-graded arcus-visionis threshold (a
blue giant surfaces through brighter twilight than a red dwarf), founding a
stellar calendar family beside the solar and lunar ones.

The sky also stops being fixed-stars-only. Wandering siblings — 0 to 4
Kepler-orbiting worlds, drawn on their own stream or pinned with
`--wanderers` — trace real orbits against the fixed background, inner ones
swinging through a morning-star/evening-star cycle bounded by their
maximum elongation, outer ones looping retrograde near opposition. And the
fixed background itself gained texture and structure: a derived starfield
(100–300 faint field stars, dim-heavy, never committed — pure texture) sits
behind the notable neighbors, and `figures.rs` clusters the unified
catalog — notable neighbors plus field stars at or brighter than a declared
magnitude floor — by angular separation into unnamed figures (structural
description only; naming is deferred to cultures, a spec non-goal), flagging
the ones straddling the ecliptic band as the zodiac for one boolean — all
three, like the starfield, computed on demand and committed nowhere.

Every one of these is an instance of the same law: a derivation must answer
*honestly* for the regime it is asked about, never quietly substitute a
coarser default. A locked world has no local day, so its sky is frozen, its
heliacal events are empty, and it has no sky band at all — not an
approximation of one. Spin direction flips which way the sky wheels but
never touches a single heliacal date. And precession moves the
epoch-referenced half of the sky (apparent coordinates, pole-star
standing) while leaving the orbital-mechanical half (wanderer periods, the
year itself) exactly as it was — because those quantities take no epoch as
an argument at all. `domains/astronomy/tests/night_sky_regimes.rs` pins
this regime × feature matrix directly: three seeds, four regimes, cheap
enough for the commit gate.

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
the wobble); latitude daylight from the sunrise equation; and — as of the
Night Sky Instrument — each wanderer's orbital period (Kepler III,
`P = 365.25 · √(a³/M)` from its drawn semi-major axis and the star's mass)
and its synodic period against the anchor (`1/P_syn = |1/P_w − 1/P_anchor|`);
every fixed star's *apparent* equatorial position at any epoch (genesis
coordinates carried through the genesis ecliptic, drifted by the equinox
offset `Δψ(t) = precession_at(t) − precession_at(0)`, re-projected at the
epoch's own obliquity); the pole-star verdict (the brightest star within
10° of either celestial pole — `POLE_STAR_MAX_SEPARATION_DEG` — a
system-level fact, identical from every latitude); the sky band (Day above
the horizon, Twilight down to 12° below it — `TWILIGHT_DEPTH_DEG`, the
classical astronomical-twilight midpoint — Night deeper); and each
neighbor's heliacal rising and setting fractions, from a 400-sample scan
of the year against its class's arcus-visionis threshold — 7° for blue
and red giants, 9° for orange giants and sun-like stars, 11° for white
and red dwarfs (brighter surfaces cut through brighter twilight; all
three thresholds sit inside the 12° twilight band). As of The Long
Count, the calendar also answers the ground half of the alignment
question: the solstice-sunrise azimuth at any latitude and epoch, its
drift between two epochs (which *is* the obliquity wobble, seen from the
ground), and the dating inverse — given a stale sightline, the most
recent epoch that would have cut it — the sky as archaeological clock,
with each settlement's founding azimuth committed as a fact.

**Approximated (declared):** circular orbits **at genesis** — every world
still starts at `t = 0` on a circular orbit, but eccentricity now oscillates
over deep time, so "circular" is a starting condition, not a permanent one;
no orbital evolution, resonance, or N-body effects (semi-major-axis
migration is now *declined by decision 0054*, not merely unbuilt — an
isolated habitable world's orbit has no honest reason to drift);
secular stellar brightening at b = 0.10·M^2.5 per gigayear (Sol-calibrated,
scaled by main-sequence lifetime; anchored so the present loses nothing,
while the habitable zone stays a genesis-epoch derivation); the
solstice-rise azimuth as cos az = sin ε(t)/cos φ, refraction and horizon
dip ignored; seasonal daylight as a
smooth sinusoid in obliquity and year phase; neighbor stars observational-only
(no gravity, no radiation); as of Eclipse Seasons, dated solar and lunar
eclipses read off a regressing node line — the nodal regression period from
the lunar-theory leading term, `P_node = (4/3)·Y²/(P_sid·cos i)` (Earth
check: ~17.9 yr against the true 18.61, the leading term alone, no higher
harmonics); the moon's ecliptic latitude at any syzygy from the small-angle
inclined-orbit form, `β = i·sin(L_moon − Ω)`; the event-time sun's angular
size from a first-order apsidal scaling of the mean diameter, evaluated
live rather than cached; the lunar shadow threshold as a declared fraction
of the solar one (`LUNAR_SHADOW_FACTOR = 0.64`, Luna–Sol-calibrated to
~1.5 umbral lunar eclipses/year, shipped untuned); a solar eclipse's ground
track, whose center latitude maps the ecliptic-latitude miss onto the
sub-solar latitude and whose half-width is a declared constant
(`TRACK_HALF_WIDTH_DEG = 2.0`); and the recurrence ladder — draconic month,
eclipse year (Luna check ≈ 346 days), the best synodic/draconic
near-commensurability up to 300 synodic months (fed TRUE Luna inputs the
search recovers the saros, 223 synodic ≈ 242 draconic ≈ 6585.3 days, though
a generated world's own derived node period may honestly land its best
cycle on an octon-class commensurability instead), how many returns a
cycle's family survives before the drifting node walks it out of the
eclipse window, and the parade — how many days a year the eclipse seasons
migrate backward through the calendar (Luna check ≈ 19); the Milankovitch drift laws themselves (slow
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
is untouched; and — as of Eclipse Seasons — each moon's ascending-node
longitude, on its own labeled stream (`astronomy/moon-nodes`) appended
after every existing draw.

Promoting a drawn quantity to a derived one is an **epoch bump**, never a
silent change — saved worlds must keep the skies they were born under.

**Approximated, added by the Night Sky Instrument:** a wanderer's apparent
brightness is Bond albedo times cross-section over squared distance
(inverse-square, real), but its varying elongation-phase angle is not
Kepler-integrated per query — a declared sinusoidal stand-in
(`phase = (t / synodic_period + year_phase_offset + index · 0.37) mod 1`,
the `0.37` an arbitrary per-wanderer rotation that draws nothing new from
the stream) that gives each wanderer a plausible synodic beat without a
second orbital integration; the heliacal scan's circumpolar/never-rises
classification is evaluated once per neighbor at the query epoch rather
than per sample across the scanned year, since precession and obliquity
forcing move on kiloyear timescales and cannot flip that classification
within one year; and the derived starfield and figures are pure
background texture — sphere-uniform, magnitude-graded, clustered by
angular separation — never committed, never a genesis draw the way a
notable neighbor is. The figure-clustering thresholds are a declared
**reference-observer convention**, frozen by a 1000-seed census rather
than picked: a 7.0° single-link separation (`FIGURE_SEPARATION_DEG`), a
magnitude-class-4 brightness floor for admission
(`FIGURE_MAGNITUDE_FLOOR`), and a three-member minimum
(`FIGURE_MIN_MEMBERS`), which the census reads out as a median of 6
figures per sky, 6.4% of worlds with no figure at all, 66.5% with at
least one figure straddling the ecliptic, and a largest observed figure
of 13 members.

**Drawn, added by the Night Sky Instrument:** wanderer count (0–4, at
authored weights 10/25/30/25/10% for 0 through 4, or pinned via
`--wanderers`); each wanderer's region (inner at 40%, outer at 60%),
semi-major axis (inner: uniform over 0.25–0.75 of the anchor's orbit;
outer: log-uniform over 1.8–20× it), class (inner always rock; outer
giant 60% / rock 40%), and Bond albedo (uniform 0.1–0.7); the background
starfield's count (uniform 100–300) and each field star's declination
(sphere-uniform), right ascension (uniform), and magnitude class (1
brightest through 5 faintest, dim-heavy at 5/10/20/30/35%) — all on their
own labeled streams (`wanderer-count`, `wanderers`, `starfield`), appended
after every existing draw so no previously generated sky moves.

**Derived from placement:** the observer's position — the flagship's cell
coordinate — is not drawn at all; it falls out of wherever settlement placed
the flagship, deterministic because placement is.

**Ages and origins, added by The Reckoning.** Until this campaign the star's
brightening clock had no zero point — `brightening_per_gyr` knew how fast the
star brightens but never how far along it already was — and each moon's
mass, distance, inclination, age, and density floated free of one another:
four independent draws standing in for what should be one story. The
Reckoning draws the star an age and each moon a **formation mechanism**
(`{GiantImpact, Capture}`; co-accretion is deliberately absent — it needs a
massive circumplanetary disk, a giant-planet mechanism the terrestrial
anchor does not have, and fission is discredited) that now drives that
moon's age, density, and orbital regularity. This section's biggest claims
are admissions, stated plainly rather than left implicit in the code.

**Derived, added by The Reckoning:** the planet's age,
`planet_age = max(0, star_age − 0.05 Gyr)` — terrestrial accretion finishes
within ~30–100 Myr of the star, so the gap is under 1% and modelled only so
the number exists and is honest about its own precision; a coeval
(`GiantImpact`) moon's age, the planet's age less a small drawn jitter
(Luna comes out 4.51 Gyr against Earth's 4.54); a moon's bulk radius from
its mass **and a real density**, `r = (3M/4πρ)^{1/3}`, rather than from mass
and an assumed density — the honesty upgrade the parked Moons campaign was
blocked on; and the ZAMS-luminosity back-derivation, `L_ZAMS = L / (1 +
b·age)` — anyone wanting the star's luminosity at the start of its
main-sequence life, before the brightening this age now dates, recovers it
by inverting `luminosity_at`'s own genesis anchoring. Nothing in the sim
consumes it today; it exists for a future consumer to call.

**Drawn, added by The Reckoning:** the star's age, a guard-railed fraction
of its main-sequence lifetime — `age = U(0.05, 0.95) · min(t_MS(M), T_MAX)`,
off its own stream (`astronomy/star-age`) — bounding the star away from both
the pre- and post-main-sequence edges, where none of this model's physics
applies (`T_MAX = 15 Gyr` is a **bound, not a cosmology**: capping at 13.8
Gyr would have imported this world's origin from ours as a side effect of a
generator constant, silently settling a metaphysical question the project
deliberately leaves open); each moon's formation mechanism, drawn *after*
the distance sort from its own stream (`astronomy/moon-formation`),
weighted by distance (declared below) — so masses and distances never move
in any world, and a world whose moons all draw `GiantImpact` is
byte-identical to the pre-campaign generator; and, for a `Capture` moon
specifically, its density class (rocky 3.0 or icy 1.6 g/cm³, its own
stream), its age (decoupled from the planet's), and its inclination
(irregular, `U(20, 160)°`, retrograde past 90°).

**Approximated, added by The Reckoning — and *why* each is an
approximation, not merely that it is:**

- `t_MS = 10 Gyr · M^-2.5` is the existing Sol-calibrated scaling already
  implicit in `brightening_per_gyr`, made explicit here — not a
  stellar-structure model.
- **`L = M^3.5` stays the genesis-epoch approximation it already was, and
  age does not correct it.** This is the campaign's central decision, not
  an oversight: `Star::luminosity` feeds the habitable zone, which feeds
  orbit admission, which feeds insolation and every climate derivation
  downstream. Letting a drawn age correct luminosity toward a proper
  ZAMS-plus-brightening value would move every world's habitable zone,
  orbit, and climate — an epoch across the entire simulation, not a
  campaign about moons. So age describes pre-genesis history only, and the
  corrected value stays one division away for anyone who wants it (the
  ZAMS back-derivation above), unused by the sim itself. A deliberate
  containment, stated here rather than left implicit.
- The distance weighting `p_capture = clamp(frac³, 0.02, 0.85)` is a
  **plausibility rule, not a population synthesis** of satellite
  formation — it encodes only the physical intuition that an impact child
  forms close and tidally recedes while irregular satellites are distant.
  But it carries something a plausibility rule usually lacks: an empirical
  anchor. It is calibrated so that a moon at Luna's real distance (384.4 Mm
  from Earth, `frac ≈ 0.386` of this model's admitted range) reads as a
  `GiantImpact` child in roughly 94% of draws. A linear map in `frac` failed
  that same check first — it called the real Earth–Moon system, this
  model's own `GiantImpact` exemplar, a capture 39% of the time, the model
  contradicting its own example — which is what forced the cube: cubing
  suppresses the middle of the range hard (`0.386³ ≈ 0.057`) while leaving
  both ends fixed.
- Densities are **two representative classes** (rocky 3.0, icy 1.6 g/cm³),
  not a composition model.
- **Capture is modelled as an outcome, not an event.** There is no
  encounter dynamics, no binary-exchange capture, no post-capture orbital
  evolution — the generator draws that a body *was* captured and gives it
  the statistics of an irregular satellite (wide, inclined, often
  retrograde). A real capture requires energy dissipation — tidal,
  gas-drag, or three-body — that this model does not simulate; it authors
  the outcome and rolls the dice on which bodies it happened to.
- **A captured moon's age is bounded `[0.05, 0.95] · planet_age` — an
  approximation, not a derivation.** A captured body can, in reality, be
  *older* than the planet it now orbits — Triton most likely predates
  Neptune's final assembly. The bound is a modelling choice made for
  simplicity; it is not a physical requirement that a capture event cannot
  postdate its own body's formation, and it is declared as a choice here
  rather than left to read as physics.

**Eclipse geometry, corrected by The Reckoning.**
`moon_ecliptic_latitude_deg`, `node_crossing_chance`, and `series_returns`
used to compute a moon's ecliptic latitude with the small-angle form
`β = i·sin(u)`, linear in the inclination `i`. That was honest for the
`[0, 10)°` band every moon occupied before this campaign, but `Capture`
draws inclinations up to 160°, and the small-angle form does not know the
90° ceiling: at `i = 160°` it reported a latitude of 160°, which is not a
physically possible ecliptic latitude (the maximum is 90°). All three now
use the **exact spherical form**, `β = asin(sin i · sin u)`, bounded by
`±min(i, 180−i)` for any inclination — which also restores a symmetry the
small-angle form silently broke: an orbit at `i` and one at `180 − i` now
eclipse identically, as physics requires (`sin i = sin(180 − i)`). This is a
rare direction of travel for this model card: a quantity that moved from
**approximated** toward **derived**, not the reverse.

**The-node-period sign, clarified (not changed) by this campaign.** A
moon's nodal-regression period (`moon-node-period-days`) has always been
signed in code — negative for `i > 90°`, where a retrograde orbit's nodes
regress the opposite way from a prograde one's — but the registered
predicate's description did not say so, which read to a consumer as an
undocumented negative "period." The registration now states the sign; the
physics, and the value, are unchanged.

**The tier ladder ahead:**

3. Realistic multi-body configurations: binary suns, moons in resonance,
   rings, and each neighbor's coordinates plotted as seen from a real place
   rather than an idealized one — plus the observer's own per-body altitude
   and azimuth (so a body is placed at a point in the sky, not merely up or
   down with a hemisphere). Physics promotions (drawn → derived) continue
   arriving as epoch bumps. (Wandering siblings and named star figures
   shipped this campaign — see above; twilight, as the shared `SkyBand`
   threshold the heliacal instrument and the day/night prose both read
   from, shipped alongside them. A wanderer-synodic calendar family,
   wanderer transits, and per-species figure catalogs are the open
   deepenings the [idea registry](../frontier/idea-registry.md) tracks.)

At every tier the query stays the same; only the richness of the answer
changes. A world configured with the tier-0 provider remains a valid,
interesting world forever — that is what fidelity-agnostic means, and the
constant-sun world of seed 42 remains in the gallery as proof.
