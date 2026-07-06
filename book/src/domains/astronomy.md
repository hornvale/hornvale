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

**Behind the curtain (as of Campaign 2a):** the star-system generator now
exists — star, habitable anchor, stability-checked moons, notable neighbors,
all pinnable for controlled experiments — but no world consumes it yet.
Worlds still see the constant sun until Plan 2b wires the generator into
genesis and gives it a calendar. See the
[Campaign 2a chronicle](../chronicle/campaign-2a.md).

**The tier ladder ahead** (Campaign 2 climbs the first rungs):

1. A 24-hour cycle: sunrise, sunset, visibility effects — the sky gains a
   period.
2. Seasons, lunar phases, and a derived calendar — the almanac gains its
   namesake content.
3. Realistic multi-body configurations: binary suns, moons in resonance,
   eclipses, wanderers, rings, constellations as perceived from the surface.

At every tier the query stays the same; only the richness of the answer
changes. A world configured with the tier-0 provider remains a valid,
interesting world forever — that is what fidelity-agnostic means.
