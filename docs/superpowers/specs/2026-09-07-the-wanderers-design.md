# The Wanderers Instrument — Design

**Date:** 2026-09-07  
**Status:** Draft for G3 review  
**Parent contracts:** Campaign 2 — The Sky; The Night Sky Instrument;
`scene/system/v1`; the world-wasm catalog contract.

## 1. Goal

Finish the solar-system layer around the already-shipped sibling planets.
Hornvale will expose a deterministic system in which the inhabited anchor is
one body among observable wanderers, and a consumer can evaluate those bodies
at a requested simulation time. The first payoff is observational: planetary
conjunctions, opposition, retrograde loops, morning/evening-star behavior,
calendar marks, and reader-facing almanac vocabulary.

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

The missing seam is that `windows/scene::system_scene` emits only the star,
anchor world, and moons. The existing system scene therefore cannot carry the
wanderers to the wasm catalog or to the external orrery. The current body
model also lacks a genesis phase, so it cannot define a unique instantaneous
position at an arbitrary time.

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

### 4.1 Astronomy domain

Extend `Wanderer` with a `phase_offset` in turns, representing the body's
heliocentric phase at absolute day zero. Add a dedicated appended stream label
for phase values, after the existing per-wanderer parameters, so the current
orbit/class/albedo draws retain their order and values. The phase is a body
parameter, not a fact a culture knows at genesis, so it is carried to scene
output but not added as a new ledger predicate.

Add pure evaluators:

- `wanderer_phase_at(wanderer, day)` — circular heliocentric phase;
- `wanderer_position_at(system, index, instant)` — position in the shared
  orbital plane relative to the host star;
- `anchor_relative_longitude_at(system, index, instant)` — the apparent
  geocentric longitude used by observation;
- `wanderer_events(system, from, until)` — conjunction/opposition and
  retrograde-loop intervals for the requested span;
- `wanderer_visibility(system, latitude, instant)` — a derived observation
  result, including the inner-body glare bound and outer-body opposition
  behavior.

The first implementation keeps the already-declared circular, coplanar
approximation. Eccentricity, orbital inclination, transits, occultations, and
N-body effects remain explicit follow-ups; no formula silently implies those
features.

### 4.2 Scene contract

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
are functions of the elements and the requested time. Field order appends
after the existing `moons` field, preserving the current v1 document.

`book/src/reference/scene-system-v1.md` becomes normative for the evaluator,
including phase normalization, inner versus outer geometry, and the explicit
circular/coplanar approximation. The native scene JSON and the wasm catalog
must call the same Rust producer path; the wasm smoke remains a byte-identity
check, not an independent reimplementation.

### 4.3 Observation and almanac layer

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
       -> StarSystem { anchor, moons, neighbors, wanderers }
          -> pure ephemeris at absolute time
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
- `scene/system/v1` retains all existing fields byte-for-byte and appends
  wanderers after moons;
- world-wasm scene output is byte-identical to native scene output;
- the seed-42 reference and the scene-system reference page agree on the
  emitted wanderer count and fields.

Heavy distribution calibration is not part of the first delivery. If the
event thresholds require calibration, it becomes a separate study with
committed data rather than an unmeasured constant hidden in the provider.

## 8. Stages and boundaries

### Stage 1: Phase-aware ephemeris

**Goal:** Give every existing wanderer a deterministic phase and a pure
position/relative-longitude evaluator.

**Success criteria:** Existing wanderer fields remain draw-isolated; phase
and position tests pass; no scene or client artifact changes yet.

### Stage 2: Observational events

**Goal:** Derive conjunction/opposition, retrograde loops, visibility, and
morning/evening-star vocabulary from the evaluator.

**Success criteria:** Event tests cover complete versus truncated intervals,
the provider/almanac can read the events, and no new world facts or proper
names are introduced.

### Stage 3: Scene and catalog contract

**Goal:** Append wanderer elements to `scene/system/v1`, regenerate the
reference/golden artifacts, and verify native/wasm byte identity.

**Success criteria:** The schema page, native CLI, wasm catalog, and golden
fixtures agree; the client handoff documents every evaluator field and
approximation.

### Deferred branches

- **Elliptical/inclined wanderer orbits** (`ORRERY-ellipse-truth`): requires
  additional elements and a larger two-language contract.
- **Wanderer transits and occultations** (`SKY-transits`): requires disc
  intersection geometry with the sun and moons.
- **Full sibling-world promotion:** a separate worldgen campaign.
- **Per-species sky catalogs** (`SKY-figures-per-species`): belongs to the
  perception/culture seam.

## 9. G3 flags

1. **Epoch-bearing phase draw.** The phase stream is additive and isolated,
   but it changes the derived scene and any committed artifact that exposes
   it. G3 must confirm that this astronomy epoch is accepted.
2. **Circular/coplanar fidelity cut.** The first instrument intentionally
   keeps the current model-card approximation. The omitted eccentricity and
   inclination are visible physical simplifications, not accidental gaps.
3. **Cross-repo client boundary.** The producer can be completed and tested
   in this repository, but the external orrery consumer is not in the
   checkout and cannot be updated in the same change.
