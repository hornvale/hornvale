# Eclipse Rhythm and View — Design

**Date:** 2026-09-10  
**Status:** G3 approved; implementation and final local review complete; canonical Sluice gate pending.
**Parent work:** Eclipse Seasons, already shipped on `main`  
**Related references:** `2026-07-14-eclipse-seasons-design.md`, `2026-07-18-the-shadow-track-design.md`, `book/src/reference/scene-eclipses-v3.md`

## 1. Goal

Eclipse Seasons already supplies the physical core: a drawn lunar node,
closed-form dated solar and lunar eclipses, approximate solar ground tracks,
observer sight tiers, and recurrence helpers. The next campaign makes that
core a complete observable instrument by exposing its recurrence structure and
its observer result as first-class outputs.

The campaign is intentionally a refinement of the existing model. It does
not add arbitrary N-body dynamics, cultural interpretation, mythology, social
response, or unrelated sky phenomena.

## 2. Boundary

The campaign has two coupled deliverables.

### Eclipse Rhythm

The astronomy domain will expose a structured recurrence summary for every
admitted moon and for each applicable eclipse family. It will cover:

- eclipse-year length and draconic-month length;
- the best bounded synodic/draconic return and its node-phase slip;
- the estimated lifetime of that eclipse series;
- the three-return exeligmos period, its signed residual terrestrial
  surface-longitude shift, and its separately named orbital node-phase slip;
- the backward season-parade rate;
- coincidence summaries for worlds with multiple moons.

The existing `best_cycle` result remains a world-derived analogue, not a claim
that every generated world has Earth's literal 223/242 Saros. The true Luna
inputs continue to be the calibration case for recognizing the Saros ratio.
All recurrence values remain pure functions of existing world facts and
typed time.

### Eclipse View

The existing `solar_eclipse_sight` and `lunar_eclipse_seen` functions will be
joined by one explicit observer-facing result. An observer is a geographic
latitude and longitude in the world's existing surface convention. The result
will state the event's applicable sight tier, whether the observer is on the
day or night side, and the geographic region responsible for that result.

For solar events, the physical region remains the current approximation: a
latitude band around the ground-track center swept across a bounded longitude
arc. For lunar events, the region is the night hemisphere rather than a
ground-track band. Boundary behavior, longitude wrapping, polar locations,
retrograde rotation, and tidally locked worlds will be explicit and tested.

## 3. Scene contract

The active eclipse wire contract becomes `scene/eclipses/v3`. This is a
pre-alpha replacement of `scene/eclipses/v2`; no compatibility adapter is
required.

The v3 document remains a closed-window query with exact tick bounds. Event
enumeration uses those snapped wire bounds, so equal emitted windows have
equal event sets. It adds
structured recurrence records and an optional observer query. Each event
contains its exact tick, moon index, body, kind, ground-track region where
applicable, and the observer result when an observer was supplied. A solar
track retains its signed unwrapped sweep and whether it covers every
longitude; wrapped endpoints alone are not the region contract. The document
also carries the query window's multi-moon coincidence-day summary.

The contract must distinguish these three cases:

1. no observer was requested;
2. an observer was requested and sees the event at a specific tier;
3. the event has no ground-track band because it is lunar.

The producer owns all simulation decisions. A client may project the emitted
band or region into its own globe layout, but it may not re-derive whether an
observer sees an eclipse.

The native CLI will accept an optional observer latitude and longitude for the
eclipse query. The world WASM catalog will expose the equivalent observer-aware
query with explicit numeric arguments and boundary validation. The existing
world seed and pins remain outside the observer parameters.

## 4. Almanac and reference output

The almanac will render the structured recurrence data without pretending that
an approximate generated-world cycle is Earth's exact Saros. It will identify
the moon and eclipse family where that distinction matters, include the
exeligmos's surface closure separately from its node-phase slip, include the
multi-moon coincidence count, and preserve an honest no-event result for an
empty query window. Central-track observer prose is event-wide and does not
present the event-midpoint side as the local passage side.

The scene reference page will be rewritten for v3. It will define the time
window, tick units, geographic coordinate convention, event ordering,
visibility vocabulary, region semantics, recurrence fields, null behavior,
and the approximation boundary of the ground track.

No cultural or interpretive prose is added. Existing physical phenomenon
lines remain available to later consumers through the trace protocol.

## 5. Data flow and determinism

The data flow remains:

```text
existing Moon node + calendar + forcing + typed observer
                         |
                  astronomy derivations
                         |
             scene v3 / almanac / WASM
```

There are no new genesis draws and no changed stream labels. The existing
`moon-nodes` draw remains the sole Eclipse Seasons input. No new epoch is
needed because the campaign adds derived views over already committed facts.

Event enumeration remains closed-form over syzygies; the campaign will not
introduce a sampled time integrator. Angular sizes and forcing-dependent
quantities are evaluated at the event's own instant. Fine positions remain
derived at query time and are never saved.

Every serialized floating-point value is quantized only at the emission
boundary. Native and WASM outputs must remain byte-identical for equal seed,
pins, window, and observer inputs. Stream consumption order and the existing
world-generation bytes remain unchanged.

## 6. Testing

The implementation will add behavior tests for:

- recurrence identities, including the explicit three-return exeligmos;
- per-moon and per-family ordering and deterministic tie breaks;
- observer visibility at day/night, track edges, longitude wrap, poles,
  retrograde rotation, and locked worlds;
- lunar events returning night-side visibility without a solar track;
- scene v3 shape, optional observer presence, exact ticks, and invalid input;
- native/WASM byte identity for identical queries;
- almanac rendering for multiple moons, empty windows, and observer contexts;
- unchanged world-generation output and unchanged stream manifests.

The existing census metrics remain the campaign's population-level eclipse
instrument. No new census metric is required unless implementation evidence
shows that a new aggregate is necessary to distinguish a promised behavior.
Scene and almanac contract changes are validated through their own fixtures and
goldens rather than by turning every derived field into a census column.

## 7. Risks and decisions

The largest risk is mixing a physical geographic region with a particular
observer's result. The v3 schema will carry both concepts explicitly so a
consumer does not infer one from the other.

The WASM ABI is a second boundary risk. Observer coordinates will be explicit
arguments with the same finite/range validation as the native query; they will
not be encoded in pins or smuggled through arbitrary JSON.

The current implementation uses a calibrated, approximate ground-track band.
This campaign may clarify and expose that approximation, but it will not
quietly turn it into umbral cartography. Partiality grading, animated shadow
motion, lunar surface shading, standstills, transits, tidal braking, variable
stars, equation-of-time work, and aurorae remain separate follow-ups.

## 8. Definition of done

- `scene/eclipses/v3` is documented, produced natively, and exported by WASM.
- Recurrence and observer results are available from the astronomy API.
- The almanac presents the recurrence ladder and observer result where a
  context supplies one.
- Native/WASM determinism and exact-tick behavior are covered by tests.
- Existing genesis streams, save facts, and census world-generation behavior
  remain unchanged.
- The Eclipse Rhythm and View reference page and frontier rows describe the
  shipped boundary and its deferred follow-ups.
