# Climate

**Questions it answers:** What are the conditions here — temperature, air,
weather? What atmospheric phenomena would an observer notice?

**Tier 0 — uniform mildness.** Eighteen degrees, everywhere, forever; the
air is warm, still, and unchanging. Like the constant sun, this is a real
answer, not an absent one — and climate demonstrates the *low* end of the
salience scale. Its one phenomenon, the ambient air, carries a salience of
0.15: present, ignorable, and yet in principle mythologizable. A culture that
has never known wind is a culture that could be astonished by one.

**What it emits.** No facts, no fields at tier 0 — a pure phenomena source,
like astronomy, and deliberately so: the two tier-0 "field-like" domains
exercise the trace protocol's read side while the fact-committing domains
(terrain onward) exercise its write side.

**Tier 1 — climate and biomes (Campaign 3c).** Climate stops being a
phenomena-only domain and becomes the field infrastructure's first heavy
user, and biomes arrive as its first queryable payoff. `GeneratedClimate`
still depends on `hornvale-kernel` and nothing else — never
`hornvale-terrain` or `hornvale-astronomy` directly — so terrain's
elevation and astronomy's sky reach it only as bare kernel types
(`CellMap<f64>`, `f64`) and climate-owned enums (`SeafloorFeature`,
`RotationRegime`), mapped once at the composition root
(`windows/worldgen`), the only place all three domains meet. The tier-0
`UniformClimate` keeps feeding the social cascade unchanged (spec §8); the
biome map is an *additional* capability, reconstructed on demand exactly as
the tectonic globe is.

**Circulation, from rotation alone.** The number of atmospheric circulation
cells per hemisphere is a heuristic step function of the solar day: slow
spin (≥ 40 h) yields one giant cell, Earth's rate (20–40 h) yields three
(Hadley/Ferrel/Polar), faster spins yield five, then seven. Each band
carries a prevailing wind — by convention (declared below) equatorial
easterly trades, mid-latitude westerlies, polar easterlies — and rising
(equatorial and ~60°) bands run wet while sinking (~30° horse-latitude and
polar) bands run dry. A **tidally locked** world is a distinct regime
entirely: no bands, no latitude organization — climate reorganizes around
the substellar/antistellar axis instead, a single day–night overturning
cell.

**Temperature and moisture as real fields.** Temperature falls with
latitude (spinning worlds) or with distance from the substellar point
(locked worlds), cooled further by a lapse rate above sea level, and
carries a seasonal term set by obliquity and damped near oceans (the
biome field itself uses the *annual mean*; the seasonal swing exists for
the REPL and almanac's day-level queries). Moisture starts from the
circulation band, rises near oceans, and falls in a mountain's lee: a
single upwind trace along the prevailing wind finds the highest barrier a
parcel crossed and dries the leeward cell in proportion. From temperature,
moisture, elevation, and (for the sea) depth, surface temperature, and
seafloor feature, a **biome** field falls out — a Whittaker lookup on land
(tundra, taiga, temperate and tropical forests, savanna, desert,
grassland, shrubland, rainforest, with ice and alpine specials), and a
parallel marine classification (depth bands from epipelagic to hadal,
coral and kelp in warm and cold shallows, sea ice, hydrothermal vents at
spreading ridges, trenches at subduction zones, upwelling zones from
wind × coastline). A **habitability** mask — land, with liquid water, in a
tolerable temperature band — is the non-opinionated answer to "where could
a vale-like place stand," and it is where the Lab's genuinely unknown
number lives (the next chapter). None of this is committed to the ledger:
biome and habitability are *fields*, recomputed from the same seed every
time, so the tier-0 `biome` *fact* that the Vale holds has no registry
conflict with the globe's biome *field*.

**The astronomy cascade (spec §7) — the exit-demo thesis.** Every arrow
from sky to biome map is derived or physical, not authored: stellar
**luminosity** and orbital distance set the insolation, and so the
temperature baseline; the habitable-zone placement (Campaign 2) is what
makes liquid water — and so oceans and moisture — possible at all;
**obliquity** sets the seasonal amplitude, from placid to violent, and
(via the almanac/REPL's day-level query) which biome bands migrate;
**rotation period** sets the band count, so a fast-spinning world shows
many narrow climate stripes and a slow one shows a few broad zones;
**rotation direction** would set which mountain flank catches the wet
wind (fixed prograde in C3, see below); and **tidal lock** reorganizes the
entire map around the substellar point. The same tectonic globe, under
different skies, yields legibly different biome maps — Earth's own
climate belts are the chain's answer key. The showpiece pair lives in the
gallery: [The Biomes of Seed 42](../gallery/biome-seed-42.md) (spinning,
latitude-banded) and [its tidally-locked
twin](../gallery/biome-seed-42-locked.md) (a substellar desert, a frozen
far side, a habitable terminator ring) — the same land seed, reorganized
by nothing but the sky above it.

**The model card.**

- **Drawn:** essentially nothing — climate adds no labeled stream in C3
  (`stream_labels()` stays empty); every quantity below is read from
  astronomy and terrain or derived from them.
- **Derived:** band count and prevailing-wind direction from rotation
  period; the temperature field from insolation, latitude/substellar
  geometry, and elevation; the moisture field from circulation band, ocean
  proximity, and the rain-shadow trace; the biome field from temperature,
  moisture, elevation, and seafloor feature; the habitability mask from
  biome-adjacent thresholds on temperature and moisture.
- **Approximated (declared):** analytic circulation bands standing in for
  fluid dynamics — no Navier–Stokes, no Coriolis solve; **rotation
  direction is fixed prograde** (astronomy draws no spin-direction bit;
  retrograde worlds are deferred, and this does not affect any exit
  criterion); no ocean currents (the ocean is a thermal buffer and
  moisture source only, never a circulation); no cloud or albedo feedback;
  seasons as a smooth sinusoid in obliquity and year phase, not a solved
  radiative balance; a single-pass rain shadow (one upwind trace, not an
  iterated moisture budget).

Chronicle: [3c, Climate & Biomes](../chronicle/campaign-3c.md). Laboratory:
[Study 002, the Census of Lands](../laboratory/study-002.md).

**The tier ladder ahead:** weather as day-to-day variation on top of the
mean fields, ocean currents and cloud feedback promoting the declared
approximations above to derived quantities (each such promotion an epoch
bump, never a silent change), and the local-refinement campaign that lets
a habitable cell host a walk-around interior.
