# Anchor Orbital Coherence — Decision Ledger

## #1 [G1] — What should the orbital-coherence campaign optimize?

**Decision:** Use one numerical orbital evaluator internally and explicit
semantic projections at consumer boundaries.

**Why:** The recent astronomy work already spans physical bodies, observation,
culture, and rendering. A single physical source prevents disconnected
phenomena, while semantic projections preserve each consumer's meaning.

**Alternatives discarded:** Numerical agreement alone would expose internals
without defining meaning; semantic agreement alone would permit duplicated
physics.

**Ideonomy passes / overturns:** 2 / 0. The first pass identified the
physics/perception/culture spectrum; the second recast the design as a
genesis-to-state-to-projection state machine.

**Capture actions:** Written design spec; future generalization to other body
families remains explicitly deferred.

## #2 [Q] — How much orbital state should be public?

**Decision:** Expose a small stable typed state surface; keep element solving
and compatibility projections private.

**Why:** Consumers need one shared source without being coupled to solver
internals. The typed surface contains position, velocity, radius, phase, frame,
epoch, and validity metadata.

**Alternatives discarded:** Raw public state invites unstable coupling;
entirely private state encourages duplicated conversions.

**Ideonomy passes / overturns:** 1 / 0. A tree pass separated one-body state
from the larger hierarchy of host, anchor, moons, wanderers, comets, and stars.

**Capture actions:** Stable-state section in the design spec.

## #3 [Q] — What time must astronomy queries accept?

**Decision:** Every time-dependent query requires an explicit typed instant.
Simulation ticks convert once at the simulation boundary; the astronomy layer
uses continuous typed time rather than a raw integer tick.

**Why:** This preserves the repository's distinction between whole-tick
simulation time and continuous astronomy, and prevents hidden ambient time or
implicit genesis defaults.

**Alternatives discarded:** Raw ticks at the physics layer falsely discretize
orbital evaluation; ambient/default time hides causality.

**Ideonomy passes / overturns:** 1 / 0. The procedure pass exposed the
conversion boundary as a required step with an explicit precondition.

**Capture actions:** Time contract in the design spec; verify exact existing
`WorldTime`/astronomy conversion signatures during planning.

## #4 [Q] — How should physical evaluation failure differ from absent events?

**Decision:** Physical evaluation returns `Result<State, OrbitalError>`;
valid-but-empty phenomena remain ordinary event results.

**Why:** An unsupported instant or degenerate orbit is materially different
from a valid sky in which no eclipse occurs. Descriptive errors preserve that
distinction for callers and diagnostics.

**Alternatives discarded:** `Option` loses cause; degraded status-bearing
values risk normalizing physically dubious answers.

**Ideonomy passes / overturns:** 1 / 0. The procedure pass separated physical
evaluation failure from the later observation/event decision.

**Capture actions:** Error model and qualification battery in the design spec.

## #5 [G2] — What should the first vertical slice be?

**Decision:** Start with anchor-only coherence across calendar, eclipse,
insolation, and scene orientation/position.

**Why:** The anchor has the greatest downstream fan-out and the smallest
bounded scope. It proves the contract against real consumers before any
generalized multi-body abstraction is committed.

**Alternatives discarded:** All-body-first risks speculative abstraction;
event-first qualifies symptoms while leaving duplicated physical causes.

**Ideonomy passes / overturns:** 1 / 0. Abstraction-lift identified the
bounded transformation `elements + instant -> state + validity -> projection`.

**Capture actions:** Anchor-first architecture, migration order, and explicit
future extension boundary in the design spec.

## #6 [G4] — Is the approved spec sufficiently decomposed for execution?

**Decision:** Proceed with the four-stage plan: physical contract, calendar /
insolation migration, eclipse / scene migration, and qualification / closure.

**Why:** Each stage has a bounded owner, an independently testable result, and
the plan preserves the spec's anchor-only scope and explicit-time boundary.

**Alternatives discarded:** A single generalized all-body implementation was
rejected as speculative; a test-only plan was rejected because it would leave
the duplicated physical seams in place.

**Ideonomy passes / overturns:** 1 / 0. Abstraction-lift confirmed the
elements-plus-instant transformation and its decomposable body-family shape.

**Capture actions:** Detailed implementation plan and root stage tracker;
execution uses fresh implementers with review after each task.
