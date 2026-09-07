# The Wanderers Instrument — Design

**Date:** 2026-09-07  
**Status:** Approved at G3  
**Parent contracts:** Campaign 2 — The Sky; The Night Sky Instrument;
`scene/system/v1`; the world-wasm catalog contract.

## 1. Goal

Generalize the stellar root before finishing the planetary layer around the
already-shipped sibling planets. Hornvale will expose deterministic systems
with a single host, a wide binary, or a bounded close binary, then place the
inhabited anchor and observable wanderers in the appropriate orbital regime.
The first payoff is observational: multiple suns where physically admitted,
planetary conjunctions, opposition, retrograde loops, morning/evening-star
behavior, calendar marks, and reader-facing almanac vocabulary.

This campaign does not create independent sibling terrains, climates,
settlements, languages, or cultures. The anchor remains the only fully
simulated world. A future campaign may promote a sibling body to a full world
without changing the astronomy-first contract defined here.

## 2. Current state and gap

`domains/astronomy/src/wanderers.rs` already generates 0–4 sibling bodies.
Each has an orbital radius, Kepler-derived period, rock/giant class, albedo,
inner-body maximum elongation, anchor-relative synodic period, and a closest-
approach brightness approximation. `StarSystem` owns these bodies; the fact
layer and sky provider already expose their existence and basic salience.

The stellar root is currently singular: `StarSystem` owns one `Star`, the
anchor is admitted around that star, and every downstream evaluator assumes
one illuminant and one gravitational center. The missing seam is therefore
twofold: the model cannot represent a binary architecture, and
`windows/scene::system_scene` emits only the star, anchor world, and moons.
The existing system scene cannot carry wanderers to the wasm catalog or to the
external orrery. The current body model also lacks a genesis phase, so it
cannot define a unique instantaneous position at an arbitrary time.

## 3. Design principles

1. **One physical source of truth.** Genesis owns body parameters;
   ephemeris functions own derived positions and events; scene serialization
   carries elements, never sampled positions.
2. **Anchor-first remains constitutional.** Wanderers decorate the habitable
   anchor and have no gravitational, radiative, terrain, or cultural effect
   on it in this campaign.
3. **Observation is a derived layer.** A body may be physically present
   without being visible at a particular time, altitude, or elongation. The
   observation functions return that distinction rather than deleting the
   body.
4. **Additive cross-repo contracts.** Existing `scene/system/v1` fields keep
   their meaning and order. New fields append at the end. A change to an
   existing field would require `v2` and is outside this design.
5. **Seed identity and draw isolation.** The new phase draw receives its own
   appended stream label. Existing star, anchor, moon,
   neighbor, forcing, and existing wanderer fields must remain isolated from
   the new phase source.

## 4. Architecture

### 4.1 Stellar architecture

Introduce a stellar-root abstraction above the planetary layer. The first
supported topologies are:

- **Single:** the current behavior; one star is the gravitational and primary
  illuminant.
- **Wide binary:** two bound stars with a circumprimary anchor. The companion
  receives a deterministic wide orbit and is visible as a moving second sun,
  but its gravity and radiative contribution to anchor climate are declared
  negligible in this first approximation.
- **Close binary:** two bound stars with a circumbinary anchor. The anchor
  orbits the barycenter, and both stars contribute to instantaneous
  illumination. The binary orbit is deterministic and stable by construction.

The root carries stellar masses, luminosities, binary semi-major axis, period,
phase, and topology. Existing single-star values remain the single variant
rather than being duplicated as a special code path. Planet placement uses a
topology-specific stability envelope: circumprimary admission for wide
systems and circumbinary admission for close systems. A rejected pin fails
loudly; genesis never silently moves the anchor across a topology boundary.

The first close-binary slice does not support circumsecondary planets,
arbitrary N-body interactions, stellar evolution, or an unrestricted number of
stars. Those are separate topology expansions.

Multi-source daylight becomes a derived evaluator. It returns the contribution
of each star and the combined illuminance; climate integration and local prose
may initially use the declared envelope rather than a full atmospheric light
model. A single-star world remains behavior-compatible wherever the new
topology fields are absent.

### 4.2 Astronomy domain

Extend `Wanderer` with a `phase_offset` in turns, representing the body's
heliocentric phase at absolute day zero. Add a dedicated appended stream label
for phase values, after the existing per-wanderer parameters, so the current
orbit/class/albedo draws retain their order and values. The phase is a body
parameter, not a fact a culture knows at genesis, so it is carried to scene
output but not added as a new ledger predicate.

Add pure evaluators:

- `wanderer_phase_at(wanderer, day)` — circular heliocentric phase;
- `stellar_positions_at(system, instant)` — positions of the stars in the
  selected topology and the barycentric or circumprimary frame;
- `wanderer_position_at(system, index, instant)` — position in the shared
  planetary frame relative to the relevant star or barycenter;
- `anchor_relative_longitude_at(system, index, instant)` — the apparent
  geocentric longitude used by observation;
- `wanderer_events(system, from, until)` — conjunction/opposition and
  retrograde-loop intervals for the requested span;
- `wanderer_visibility(system, latitude, instant)` — a derived observation
  result, including the inner-body glare bound and outer-body opposition
  behavior.

The first implementation keeps the already-declared circular, coplanar
approximation for planetary wanderers and uses bounded two-body stellar
orbits. Eccentricity, arbitrary orbital inclination, transits, occultations,
stellar evolution, and N-body effects remain explicit follow-ups; no formula
silently implies those features.

### 4.3 Scene contract

Append a `wanderers` array to `scene/system/v1`. Each `WandererElem` carries
the minimum elements a client needs to reproduce the shared evaluator:

- `orbit_au`;
- `period_days`;
- `phase_offset`;
- `class` (`rock` or `giant`);
- `albedo`;
- `synodic_period_days`;
- `max_elongation_deg` when the body is inner, absent for outer bodies.

The scene contains no current angle, brightness, or event list. Those values
are functions of the elements and the requested time. New fields append after
the existing `moons` field, in the fixed order `stellar`, then `wanderers`,
preserving the current v1 document.

`book/src/reference/scene-system-v1.md` becomes normative for the evaluator,
including phase normalization, inner versus outer geometry, and the explicit
circular/coplanar approximation. The native scene JSON and the wasm catalog
must call the same Rust producer path; the wasm smoke remains a byte-identity
check, not an independent reimplementation.

The appended `stellar` object describes topology and both stars' orbital
elements; the existing `star` object remains the primary-star compatibility
view for single-star consumers. A topology-aware consumer branches on the new
object.

### 4.4 Observation and almanac layer

The astronomy provider and almanac consume the pure evaluators. A wanderer is
reported as:

- an inner body's morning-star or evening-star appearance when the elongation
  and twilight conditions admit it;
- an outer body's opposition/conjunction state;
- a retrograde-loop event when apparent longitude reverses and later resumes;
- a conjunction/opposition calendar mark with body index and absolute time.

These are derived views or phenomena, not new persistent world facts. Proper
names remain outside the astronomy domain: cultures and language name sky
objects later.

## 5. Data flow

```text
seed + sky pins
    -> astronomy genesis
       -> StarSystem { stellar, anchor, moons, neighbors, wanderers }
          -> stellar topology + pure ephemeris at absolute time
             -> observation/events/almanac
             -> scene/system/v1 elements
                -> native CLI and world-wasm
                   -> external orrery client
```

The external orrery is not present in this checkout, so this campaign owns
the producer contract, reference page, wasm/native golden, and a client
handoff note. Client implementation is a separate consumer task against the
versioned scene document.

## 6. Error handling and degenerate regimes

- A malformed or missing phase element is a scene build/schema error; it is
  never replaced by zero or a random fallback.
- A zero synodic rate yields no finite recurrence; callers report a
  non-repeating alignment rather than a huge fabricated period.
- Inner bodies use their maximum-elongation bound for glare; outer bodies use
  opposition geometry. No body is deleted merely because it is currently
  hidden.
- A retrograde interval is emitted only when the apparent-longitude
  derivative changes sign and later returns to its forward direction. A
  tangent or a truncated query window is not promoted to a complete loop.
- Locked anchors retain the existing honest calendar behavior; wanderer
  positions remain evaluable, but solar-day language is absent.

## 7. Testing

Tests will be behavior-oriented and layered:

- phase generation is deterministic and isolated from existing parameter
  draws;
- Kepler period/orbit and synodic formulas match the existing model card;
- phase normalization is stable across negative and large absolute days;
- inner bodies never report an elongation beyond their geometric bound;
- outer bodies can enter opposition and produce a retrograde interval;
- no complete retrograde loop is reported for a truncated query window;
- locked, zero-obliquity, and retrograde-anchor regimes remain honest;
- two native scene serializations for the same seed are byte-identical;
- `scene/system/v1` retains all existing fields byte-for-byte and appends the
  `stellar` object followed by `wanderers` after moons;
- world-wasm scene output is byte-identical to native scene output;
- the seed-42 reference and the scene-system reference page agree on the
  emitted wanderer count and fields.

Heavy distribution calibration is not part of the first delivery. If the
event thresholds require calibration, it becomes a separate study with
committed data rather than an unmeasured constant hidden in the provider.

## 8. Stages and boundaries

### Stage 1: Stellar topology

**Goal:** Establish the single/wide-binary/close-binary root and
topology-specific anchor admission, while preserving single-star behavior.

**Success criteria:** Topology pins and generated systems are deterministic;
single-star fixtures remain stable; close-binary systems expose a barycentric
anchor orbit and two illuminants; wide-binary systems expose a moving
companion; impossible topology/orbit combinations fail loudly.

### Stage 2: Phase-aware ephemeris

**Goal:** Give every existing wanderer a deterministic phase and a pure
position/relative-longitude evaluator.

**Success criteria:** Existing wanderer fields remain draw-isolated; phase
and position tests pass; no scene or client artifact changes yet.

### Stage 3: Observational events

**Goal:** Derive conjunction/opposition, retrograde loops, visibility, and
morning/evening-star vocabulary from the evaluator.

**Success criteria:** Event tests cover complete versus truncated intervals,
the provider/almanac can read the events, and no new world facts or proper
names are introduced.

### Stage 4: Scene and catalog contract

**Goal:** Append wanderer elements to `scene/system/v1`, regenerate the
reference/golden artifacts, and verify native/wasm byte identity.

**Success criteria:** The schema page, native CLI, wasm catalog, and golden
fixtures agree; the client handoff documents every evaluator field and
approximation.

### Deferred branches

- **Elliptical/inclined wanderer orbits** (`ORRERY-ellipse-truth`): requires
  additional elements and a larger two-language contract.
- **Arbitrary multiple-star systems:** N-body dynamics and unrestricted
  multiplicity are outside the bounded topology vocabulary.
- **Close circumsecondary planets:** S-type planets in close binaries require
  a second stability/admission regime and are deferred.
- **Wanderer transits and occultations** (`SKY-transits`): requires disc
  intersection geometry with the sun and moons.
- **Full sibling-world promotion:** a separate worldgen campaign.
- **Per-species sky catalogs** (`SKY-figures-per-species`): belongs to the
  perception/culture seam.

## 9. G3 flags

1. **Binary-topology epoch.** Adding stellar architecture changes the root
   derivation and anchor-admission contract. G3 must confirm that this is
   accepted as the campaign's primary astronomy epoch.
2. **Epoch-bearing phase draw.** The phase stream is additive and isolated,
   but it changes the derived scene and any committed artifact that exposes
   it. G3 must confirm that this astronomy epoch is accepted.
3. **Circular/coplanar fidelity cut.** The first instrument intentionally
   keeps the current model-card approximation. The omitted eccentricity and
   inclination are visible physical simplifications, not accidental gaps.
4. **Cross-repo client boundary.** The producer can be completed and tested
   in this repository, but the external orrery consumer is not in the
   checkout and cannot be updated in the same change.
