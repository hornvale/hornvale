# Anchor Orbital Coherence — Design

**Date:** 2026-09-13  
**Status:** Approved; anchor slice qualified
**Scope:** One authoritative, explicitly timed anchor-orbit evaluation shared by calendar, eclipse, insolation, and scene consumers

## 1. Goal

Make the anchor world's evaluated orbital state the authoritative physical
source for all present-time astronomy consumers. A query supplies an explicit
typed instant, receives a typed state or a descriptive error, and each consumer
projects that state into its own semantic vocabulary.

The first campaign is deliberately anchor-only. Its purpose is to prove the
shared contract at the highest-fan-out body before generalizing it to moons,
wanderers, comets, or binary companions.

## 2. Non-goals

- No N-body integration or unrestricted orbital solver.
- No redesign of deep-time forcing parameters until their relationship to
  instantaneous orbit is measured and specified.
- No new save-format facts for evaluated states; the state remains derived
  from committed genesis facts and the requested instant.
- No new phenomenon family.
- No silent compatibility fallback, genesis-time clamp, or ambient session time.

## 3. Physical architecture

```text
genesis facts
  -> private AnchorOrbit elements, frame, epoch, validity
  -> evaluate_at(explicit instant)
  -> Result<AnchorState, OrbitalError>
       |
       +-> calendar: solar longitude and season
       +-> eclipse: alignment and shadow geometry
       +-> insolation: instantaneous stellar flux
       +-> scene: position and surface orientation
```

The evaluator owns element solving, anomaly iteration, normalization, and
validity checks. Those implementation details remain private. The stable
typed state surface exposes only semantic quantities needed by consumers:

- explicit evaluated instant;
- frame and epoch metadata;
- position and velocity;
- orbital radius;
- mean and true longitude;
- validity metadata where relevant.

The public surface must not expose raw element storage or make callers repeat
coordinate conversions. A projection is read-only: it may interpret the
state, but it cannot alter the physical state or establish a competing source
of orbital truth.

Deep-time forcing remains a separate climate model in this slice. The campaign
may document or test its boundary, but it must not equate a slowly varying
forcing envelope with the instantaneous anchor state without a later decision.

## 4. Time and errors

Every time-dependent astronomy query requires an explicit typed instant. No
query reads ambient session time, defaults to zero, or silently substitutes
genesis. Simulation-facing windows may accept `WorldTime` or an explicit tick;
the conversion to astronomy's continuous typed instant occurs once at that
boundary. Astronomy-facing APIs accept the resulting typed astronomical time.

The physical seam returns `Result<State, OrbitalError>`. Errors distinguish at
least invalid/non-finite input, unsupported or out-of-window time, degenerate
orbital parameters, and solver non-convergence. A valid sky with no eclipse or
other phenomenon is not an error: event APIs return their ordinary empty
result.

Out-of-window evaluation answers the requested instant honestly by returning
an error. Caller-specific presentation may explain unavailability, but no
caller silently clamps or invents a fallback.

## 5. Invariants

The implementation must preserve these properties:

1. **Single source:** calendar, eclipse, insolation, and scene projections all
   derive from the same evaluated anchor state.
2. **Time honesty:** the requested instant is explicit and preserved through
   every projection.
3. **Frame honesty:** vectors carry a documented frame; conversions happen at
   named boundaries.
4. **Determinism:** identical seed, pins, and instant produce identical state
   and projection values.
5. **Continuity:** small time changes produce bounded state changes except at
   explicitly documented angular wrap boundaries.
6. **Validity:** unsupported or degenerate evaluations fail descriptively.
7. **Compatibility:** legacy scene bytes change only where the new physical
   meaning is intentionally adopted and rebaselined.

## 6. Qualification battery

Tests must compare independent consumer readouts, not merely confirm that all
call the same helper:

- calendar solar longitude agrees with anchor true longitude after the
  documented frame conversion;
- eclipse alignment uses the same anchor position as the calendar;
- insolation uses the same instantaneous orbital radius;
- scene position and surface orientation agree with the physical state;
- negative, zero, and bounded future instants behave consistently;
- locked and retrograde worlds preserve their declared conventions;
- invalid periods, non-finite values, degenerate parameters, and out-of-window
  instants fail descriptively;
- repeated and reordered queries remain deterministic;
- an intentional private projection perturbation makes the coherence battery
  fail, proving the tests detect semantic drift rather than only shared
  plumbing.

The battery must also preserve the existing distinction between valid physical
state and absent event: no eclipse is a valid event result, while an orbit that
cannot be evaluated is an `OrbitalError`.

## 7. Migration order

1. Introduce the private anchor-orbit evaluator and typed `AnchorState`.
2. Add `OrbitalError` and route explicit-time validation through the seam.
3. Route calendar and insolation through the state projection.
4. Route eclipse geometry through the state projection while preserving event
   semantics.
5. Route scene position and orientation through named state projections.
6. Add the cross-consumer qualification battery.
7. Regenerate affected artifacts only after tests identify intentional output
   changes, and record any compatibility epoch explicitly.

## 8. Future extension

Once the anchor contract is qualified, the same shape may be generalized to
other bodies. That extension is not automatic: each body family must specify
its own frame, epoch, validity horizon, degeneracies, and observer/event
projections. The anchor campaign succeeds when it makes that next decision
evidence-based rather than speculative.
