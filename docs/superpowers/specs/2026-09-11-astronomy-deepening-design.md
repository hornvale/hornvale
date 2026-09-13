# Astronomy Deepening — Design

**Date:** 2026-09-11  
**Status:** Brainstorm-approved; awaiting written-spec review  
**Scope:** Physical astronomy substrate, persistent comets, stellar variability, and naked-eye cultural sky inputs

## 1. Goal

Strengthen Hornvale's astronomy as a causal physical substrate for human-scale
history. The model should make recurring and rare sky phenomena emerge from a
small set of coherent celestial elements, while leaving observation and
cultural interpretation to downstream layers.

The model promises coherent results across a bounded historical window of
roughly tens of thousands of years around genesis. It does not promise exact
orbital behavior over geological or billion-year timescales.

## 2. Non-goals

- No N-body simulation or unrestricted orbital integrator.
- No asteroid population, asteroid impacts, or fireball system.
- No guaranteed supernovae, comets, or other narrative fireworks.
- No host-star instability: the inhabited anchor's local star remains stable.
- No telescope-only constellation system.
- No universal cultural constellation catalogue.
- No requirement to instantiate or store thousands of background stars.

## 3. Physical architecture

The domain is organized around three layers:

```text
genesis elements -> analytic body state at time t -> observer/event readouts
```

Genesis owns stable identities and physical/orbital parameters. Ephemeris
functions own positions, velocities, illumination, and phases. Event and
observation functions project that state into eclipses, visibility, showers,
and transients. No downstream readout changes the physical state.

The substrate is a thin hierarchical Keplerian model. It supports the
existing single/binary stellar roots, anchor, moons, and wanderers, and adds
small bodies through the same state-evaluation seam. It uses bounded analytic
approximations, with explicit frame, epoch, validity-window, and degeneracy
rules. Existing calendar, eclipse, node-regression, and forcing consumers
must share one authoritative orbital geometry; the current circular
ephemeris/eccentricity split must be resolved before new comet work.

The substrate does not model strong mutual perturbations, scattering, close
encounters, arbitrary orbital exchange, or long-term chaotic evolution.

## 4. Persistent comets and meteor showers

The first small-body family is a few persistent, genesis-drawn comets,
separate from planetary wanderers. A comet carries a stable identity, orbital
elements, epoch/phase, nucleus and albedo parameters, baseline activity, and
debris-stream parameters.

Activity is stable in kind but varies deterministically by apparition:

```text
comet identity + return index -> activity multiplier -> brightness and tail
```

The model does not carry mutable comet health. Fragmentation, disruption, and
interstellar passages remain deferred.

Visibility is emergent and tiered:

```text
latent -> naked-eye -> great comet
```

The tiers derive from physical brightness, geometry, activity, darkness, and
observer conditions. A great comet is never seeded as a narrative guarantee.

Meteor showers are derived from a comet's persistent debris stream, not from
the comet's current proximity. A first implementation supports stable
recurring streams. It derives the stream crossing, radiant, velocity,
seasonal timing, and local rate from relative orbital geometry. Local
visibility additionally reads observer latitude/longitude, local time,
daylight, horizon, atmospheric attenuation, and moonlight. Dense stream
clumps and outburst years are a follow-up.

Asteroids and impacts are outside this family, now and unless a later
downstream requirement reopens them.

## 5. Physically modeled stars

All notable catalog stars become physically modeled objects rather than
observational labels. Their mass, age, evolutionary stage, effective
temperature, luminosity, distance, position, variability, and possible fate
must remain mutually coherent. Spectral class is a derived description of
physical state, not an independent visual adjective.

The host star remains stable under the world premise. Neighbor stars may have
rare naturally timed transients, but the generator never forces one into the
historical window.

The first variable-star slice supports deterministic intrinsic periodic
variability. It changes both brightness and spectrum through the existing
banded blackbody illuminant and atmospheric-lighting path. Eclipsing binaries
are a near-term follow-up, derived from binary geometry and observer position.

Terminal events such as novae and supernovae are allowed only when the
star's physical state places them inside the historical window. Visibility
and hazard are separate outputs: a visible supernova is not automatically a
local catastrophe.

## 6. Star populations and identity

The sky has two populations:

- a modest catalog of tens of individually modeled stars, with stable IDs;
- a larger deterministic background field, generated lazily from seed and
  sky region, without a stored roster or per-star lifecycle.

Lazy background generation must be independent of query order, requested
detail, and traversal path. Brightness semantics must be shared by star
generation, visibility, and figure clustering.

Before catalog expansion, the figure pipeline must be made compatible with
stable star/member IDs and its assumptions about brightness must be audited.
Any changed distribution is a deliberate calibration change, not decoration.

## 7. Observer and cultural boundary

Astronomy owns physical sky state and observer geometry. Species supplies the
existing `activity_cycle`, `night_vision`, and `sky_attention` traits.
Perception composes them into a deterministic naked-eye detection threshold;
it does not create an individual-variation model.

The threshold is hard at the species level, while physical modifiers vary
continuously with twilight, atmosphere, moonlight, and geometry. Diurnal,
crepuscular, and nocturnal species therefore receive genuinely different
candidate skies.

Culture owns noticing, grouping, naming, and interpretation. There is no
universal constellation set. Constellation candidates come only from stars
available to that species and observer; different cultures may group the same
stars differently. Telescope-only stars are excluded.

The composition boundary remains kernel/trace/windows-based: astronomy does
not depend on sibling domains, and species does not own astronomical physics.

## 8. Sequencing

1. Reconcile orbital frames, epochs, eccentricity handling, and validity
   limits across calendar, ephemeris, eclipses, forcing, and scene consumers.
2. Add persistent comets and shared debris-stream encounters.
3. Establish stable modeled-star identities and observer visibility.
4. Add intrinsic periodic variables using the existing spectral illuminant.
5. Add culture-owned naked-eye constellation derivation.
6. Add eclipsing binaries, terminal stellar events, and debris outbursts as
   separate bounded follow-ups.

## 9. Testing and qualification

The physical substrate must test frame consistency, epoch anchoring, exact
determinism, bounded historical evaluation, negative times, high
eccentricity, retrograde/locked cases, and explicit handling of degenerate
periods.

Comet tests must cover identity stability, apparition variation,
latent/naked-eye/great tier ordering, stream crossings, radiant derivation,
and observer-specific rates. The same shower queried from different observers
must share its physical encounter while differing only in visibility.

Catalog tests must cover stable IDs, query-order-independent lazy generation,
brightness consistency, and the updated figure member model.

Perception tests must cover activity-cycle differences, night-vision
thresholds, continuous twilight/moonlight modifiers, and the separation of
physical visibility from cultural selection.

## 10. Design decisions

- The physical spine is hierarchical Keplerian, not N-body.
- Human historical time is the accuracy target.
- Persistent comets are modeled objects; meteor showers are derived debris
  encounters.
- Visibility tiers are emergent and rare; no guaranteed spectacle.
- Intrinsic variable stars precede eclipsing binaries.
- Notable catalog stars are physically modeled; background stars are lazy.
- Naked-eye perception is species-based; individuals are out of scope.
- Constellations are culture-owned and species/observer-filtered.
