# Campaign 3c: Climate & Biomes

**July 2026 · 16 commits · outcome: complete, merged — Campaign 3 (The
Land) closes with a queryable global biome map**

## What was attempted

Campaign 3b gave the world a shape: a tectonic globe, its elevation banked
as an opaque `CellMap<f64>` no other domain could see into. Campaign 3c
gave that shape weather, and weather a name for what it grows. A new
tier-1 climate provider reads the globe's elevation and sea level, plus
three scalar readings off the generated sky, and derives temperature and
moisture fields, a biome field, and a habitability mask — all recomputed
on demand, none of it ever written to the ledger. It closes Campaign 3's
thesis: the same tectonic globe, under different skies, now produces
legibly different biome maps, with Earth's own climate belts standing as
the answer key.

## What landed

**The astronomy cascade, made visible.** Every arrow from sky to biome map
is derived or physical, wired once at the composition root
(`windows/worldgen::climate_of`). Stellar luminosity and orbital distance
set an insolation baseline; obliquity sets the seasonal amplitude;
rotation period sets the circulation band count (a heuristic step function
of the solar day — slow spins get one broad band, Earth's rate gets
three, faster spins get five or seven); and tidal lock discards bands
entirely, reorganizing climate around the substellar point instead. The
elevation field earns a lapse-rate cooling and a single-pass rain-shadow
trace along the prevailing wind. From temperature, moisture, elevation,
and (for the sea) depth, surface temperature, and a newly-exposed seafloor
boundary feature, a `Biome` field falls out: a Whittaker lookup on land
(twelve classes, ice and alpine as specials) and a parallel marine
classification (ten classes — reefs and kelp in shallow warmth and cold,
sea ice, hydrothermal vents on spreading ridges, hadal trenches at
subduction zones, wind-driven upwelling). A habitability mask — land,
water, a tolerable annual mean — is the non-opinionated answer to "where
could a place stand," pre-wiring the embark seam without pinning the Vale
to any coordinate on the map.

**The biome map, the campaign's deliverable.** A hand-rolled PPM and ASCII
renderer, in the First Light and elevation-map tradition, turns the biome
field into a committed, drift-checked gallery artifact. The exit-demo pair
is the same land seed, seed 42, under two skies: [spinning, latitude-banded
biomes](../gallery/biome-seed-42.md) against [a tidally-locked twin](../gallery/biome-seed-42-locked.md)
whose map reorganizes entirely around a substellar desert, a frozen far
side, and a narrow habitable terminator ring. Same globe, same elevation,
same sea level — only the sky changed, and the map changed with it. The
REPL answers `biome <lat> <lon>` and `biomes`; the almanac's Land section
grows two new lines naming the circulation-band count and the habitable
percentage.

**The Lab grows a land half.** Seven new metrics — plate count, ocean
fraction, mountain coverage, band count, habitable fraction, unrest
coverage, dominant land biome — join the existing fourteen sky metrics in
one unified registry, one runner, unchanged. The band-count⇔rotation
calibration reproduces its known step function exactly across all 10,000
worlds in the author-time `census-of-lands` study: 79.5% land on three
bands, 16.0% on five, 4.6% (the locked worlds) on none, with the one- and
seven-band regimes never triggered at this generator's current draw
ranges. The genuinely new number — habitable fraction, unknown until this
census existed — averages **15.1% of the globe**, driven down and
compressed (not extremized) by tidal lock, driven down hardest by extreme
mountain coverage, and essentially untouched by obliquity, which moves
seasons rather than the annual mean the habitability mask reads. Full
analysis: [Study 002, the Census of Lands](../laboratory/study-002.md).
`census-drift` (500 seeds) is renamed `census-lands-drift`, still the one
CI-checked drift study, now covering both halves of the registry.

**Three openers from the Campaign 2 close, finally landed.** `Mm` becomes
`Megameters` — the last abbreviated unit type, cleaned up before climate
could add to the confusion. `HabitableZone` becomes a real newtype with an
`inner < outer` invariant enforced at construction (never `Option` —
anchor-first genesis makes a zone-less world unrepresentable), and
climate's insolation baseline is its first consumer outside astronomy.
`Sky::calendar()` gives the composition root a single accessor for the
generated sky's year length, `None` for the tier-0 constant sun — climate
reads it without ever matching on `Sky` variants itself.

## What was learned

- **A field-only domain can still deepen cleanly.** Climate never wrote a
  fact at tier 0 and writes none at tier 1 either — biome and
  habitability are queryable fields, recomputed every time, so the tier-0
  `biome` *fact* the Vale holds and the globe's biome *field* never
  collide in the concept registry. Depth can arrive without the write
  side ever being exercised.
- **Coarse constrains fine, across two domains now, not one.** Terrain's
  elevation and boundary classification, computed with no knowledge that
  climate exists, are exactly what climate needed — the composition root
  maps `terrain::BoundaryKind` to `climate::SeafloorFeature` and
  `astronomy::Rotation` to `climate::RotationRegime` in one file, and
  neither domain crate imports the other.
- **An analytic model can still carry a real physical thesis.** No fluid
  dynamics, no iterated moisture budget, no Coriolis solve — a step
  function, a lookup table, and one upwind trace are enough to make
  Earth's climate belts, a substellar desert, and a rain shadow all show
  up where a reader expects them, at the coarse tier the census needed to
  stay a tens-of-seconds author-time run over 10,000 worlds.
- **Rotation direction, fixed prograde, cost nothing this campaign.**
  Astronomy draws no spin-direction bit; every prevailing wind follows the
  Earth convention (equatorial easterlies, mid-latitude westerlies). It is
  a declared, documented approximation in the model card, not a silent
  gap — and it did not touch either exit-demo pair (spinning vs. locked;
  fast vs. slow rotation), so nothing here narrowed what Campaign 3 set
  out to prove.

## Deferred, deliberately (spec §15)

Evolved, time-stepped tectonics wait for the deep-time campaign, behind
the same static-provider interface. Erosion, sediment transport, and
isostasy beyond the base rule; ocean currents; cloud and albedo feedback —
all declared approximations in the model cards, all promotable later as
epoch bumps, never silent changes. Hydrology (rivers, lakes, endorheic
basins) waits for a hydrology tier or local refinement, since lakes are
often sub-cell; the substrate it will need — continental rifts, arid
interior lows — is already classified. Marine ecology and
creature/culture placement (productivity-peak migrations, the aphotic
deep, a reef-dwelling culture, something old in a hadal trench) wait for
the ecology and settlement campaigns; C3 banks the substrate fields
(depth, sea-surface temperature, productivity proxies via upwelling, vent
and trench flags, unrest) they will consume. Local refinement — fine
terrain, place names, the walk-around scale, a literal place for
settlement to stand — is the immediately following campaign; the Vale
keeps its tier-0 seam untouched until then.

## Artifacts

[The Biomes of Seed 42](../gallery/biome-seed-42.md) and [its
tidally-locked twin](../gallery/biome-seed-42-locked.md) — the exit-demo
pair, hand-rolled PPM and ASCII, drift-checked in CI. [Study 002: The
Census of Lands](../laboratory/study-002.md) — the band-count calibration
and the habitable-fraction number, over 10,000 worlds.
