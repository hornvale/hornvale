# Paleoclimate

Every domain so far has read the sky as it is *today*. Paleoclimate is the
first to read it across deep time: it re-runs climate over the million
years the sky's orbital forcing already drifts through, and extracts the
durable marks a real glacial history leaves on the present globe — an
ice-extent envelope, a fossil shoreline, and the refugia that stayed
habitable through the coldest moment a world experienced. Kernel-only, like
every domain: it reads climate's outputs as bare fields (a temperature at a
cell, a habitability mask, the present relief) rather than importing
climate itself, so the boundary between domains never bends.

## The mechanism

**A caloric-summer index.** The classic Milankovitch–Köppen driver: how
warm a high-latitude summer runs, because a cool summer is what lets the
prior winter's snow survive. It rises with the world's own obliquity above
its own long-run mean — an anomaly against the world's baseline tilt, not a
fixed Earth-like reference, so the index reads exactly zero whenever a
world sits at its own normal with no eccentricity, whatever that normal
happens to be. It rises again with eccentricity scaled by the sine of
precession, the climatic-precession term. All three orbital elements enter.

**A nonlinear global ice volume, marched with hysteresis.** One number
carries the memory: ice grows slowly below a cold threshold on the index,
melts fast above a warm one, and simply holds its ground in the dead band
between. The asymmetry produces the glacial sawtooth and is the mechanism
that lets the weak, slow eccentricity term end up dominating the visible
record over the faster obliquity and precession terms — the ice sheet's
own nonlinearity, not the raw size of a forcing term, decides which cycle
wins.

**Albedo cooling and eustatic sea level, both proportional to ice
volume.** A global temperature offset (more ice, more cooling) and a
falling sea level (more ice, more locked-up ocean water) both derive
directly from the same single volume number.

**An absolute snowline, not an anomaly threshold.** A cell glaciates when
its *absolute* temperature — the present field's real latitude structure,
plus the era's uniform cooling offset — drops below a fixed freezing
point. Comparing an absolute reading against a fixed threshold, rather than
comparing the uniform offset against a fixed anomaly cutoff, is what lets
one global cooling number move a real, latitude-shaped snowline instead of
flipping the entire globe glaciated or bare all at once.

**Strata are ice *advance* beyond the present**, not raw glaciation: a cell
counts only if a given era iced it and the present world does not. This is
what keeps the null control (below) structurally exact regardless of how
cold a world's present poles already run, and it is what the ice-extent
envelope, the fossil shoreline, and the refugia are all built from.

## The determinism story

The model draws **no new randomness** — every quantity above is a
closed-form function of the orbital forcing astronomy already computes, so
paleoclimate consumes no seed stream of its own and needs none. Two
orderings are the save-format contracts this campaign adds: the era-tick
order (which day each of the coarse climate re-runs lands on) and the
ice-integration order (the sequence the fine-step ice march consumes its
forcing samples in), both defined in exactly one place so a saved world's
deep-time record can never drift from how it was first computed.

The **zero-forcing pin** is this campaign's preregistered null control: a
world whose orbital elements are held perfectly still must show a flat
caloric-summer index (exactly zero, every era, by construction), which
never leaves the ice sheet's dead band, which means every era's diagnosed
climate is bit-for-bit the present's own. Nothing advances beyond what the
world already shows, so the ice-extent envelope is empty, the fossil-
shoreline band collapses to the sea's single present stand, refugia read as
everywhere currently habitable, and the narrative "the frost retreated"
fact is never committed. This holds structurally, for every seed under the
pin — not tuned to hold, but true by the shape of the equations themselves.

## Typed quantities

`Temperature` (an absolute reading) and `TempAnomaly` (a difference from
the present) are distinct types, not two uses of the same bare number: a
`TempAnomaly` can only be produced by subtracting two `Temperature` values
(or from a computed offset, such as the albedo cooling), so it is
impossible to accidentally hand an absolute temperature to code that
expects a difference, or vice versa — a mistake earlier iterations of this
model actually made. The pair was born in this crate as
`Celsius`/`TempAnomaly` and proved sturdy enough that it was promoted to
the kernel as the shared temperature vocabulary every domain can speak
([Temperature](../chronicle/temperature.md)); this crate now imports it.
`IceVolume` (a dimensionless fraction in `[0, 1]`) and `SeaLevelChange`
(metres of eustatic rise or fall — a deep-time delta, distinct from the
elevation datum) remain the coherent quantities this crate keeps as its
own.

## The model card

**Derived:** the orbital elements feeding the caloric-summer index
(obliquity, eccentricity, precession) come from astronomy's forcing,
unmodified. The causal chain is closed-form at every step, with disjoint
inputs — orbital elements → caloric-summer index → ice volume → both the
albedo cooling offset and the eustatic sea-level fall (the index depends only
on the orbital elements, the sea-level change only on ice volume) — and no
randomness anywhere (see *Drawn*, below). The index and the couplings do
carry authored sensitivity coefficients, declared next.

**Approximated — tuned, and disclosed as such.** Every constant is a
calibrated effective parameter, not a physical measurement: the
caloric-summer index's own sensitivity coefficients (`K_OBLIQUITY` = 1.0 and
`K_PRECESSION` = 40.0, weighting axial tilt against climatic precession), the
ice model's growth and melt thresholds on that index, its growth and melt
rates, the eustatic coefficient, and the diagnostic's freezing threshold.
Two are worth naming plainly: `FREEZE_C` (−10 °C, the absolute snowline)
and `ALBEDO_GAIN_C` (42 °C, the full-ice cooling offset) were tuned
together against a handful of generated skies until a typical world's
glacial maximum advanced ice over roughly a quarter of its land — a
realistic Last Glacial Maximum extent. `ALBEDO_GAIN_C` in particular is
large because the model applies a single *global* cooling offset with no
regional ice-albedo feedback: a real ice sheet cools itself further by
reflecting more sunlight once it exists, a local amplifier this model does
not represent, so the uniform offset needs roughly twice the real world's
global cooling to reach the same glacial extent. Effective cooling is
`ALBEDO_GAIN_C × ice_volume`, and ice volume peaks well below 1 even at a
world's coldest era, so the gain has to be large to compensate for both the
missing local feedback and the fact that it is being multiplied by a
fraction, not a full unit. A future spatial-ice tier (below), with real
per-cell ice-albedo feedback, is what would let these constants shrink back
toward physically measured values. Separately: `max-ice-fraction`, the
fact a world commits, is the fraction of *land where ice advanced beyond
the present* at the glacial maximum — not total ice cover, and not
comparable to a real-world glaciated-area statistic without that
distinction in mind.

**Drawn:** none. This is a notable property of the whole domain: ice
dynamics are entirely derived from orbital forcing that astronomy already
draws, so paleoclimate introduces no seed stream of its own, and a deep-
time pin costs nothing in stream-isolation obligations — there is nothing
to isolate.

**Authored:** the model's structural choices (hysteresis over a smooth
response, an absolute rather than anomaly-only snowline, advance-beyond-
present rather than raw glaciation as the strata unit) are authored
decisions, not measurements or draws, defended above.

## Diagnostic extent, not a spatial ice sheet

What this campaign diagnoses at each era is a single **global** ice volume,
translated cell by cell into an advance-or-not verdict through the
snowline threshold — not a per-cell ice field with its own local growth,
melt, and margin lag. This is the accuracy/complexity point chosen
deliberately over a fully spatial model: volume carries the memory, and
extent is read off the same forcing rather than integrated independently
per cell. A later, fully spatial tier — real ice geometry with local
dynamics — is left open as a refining tier in the same coexisting-tiers
pattern the rest of the cascade already uses: because every downstream
consumer binds to the committed facts, not to how this domain computed
them, and because this tier draws no randomness of its own, a spatial tier
can slot in later with no new save-format stream contracts and no consumer
changes required.

Chronicle: [Deep Time](../chronicle/deep-time.md).
