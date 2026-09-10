# The Vent: Succession — campaign ledger

Campaign: architectural continuation of The Vent. Provisional titles: “The
Vent: Succession” / “The Living Vent”. Branch: `campaign/the-vent-succession`.

## #1 [G1] — How should a temporal Waterworld overlay evolve?

**Decision:** Use a stable, seeded vent identity with a bounded candidate ring.
World time derives one of five bounded states—absent, nascent, active,
weakening, failed—from source parameters and a fixed succession period. The
selected candidate may migrate within the ring; local fields and stocks read
the selected source, while current transport is a separate bounded influence.

**Why:** This preserves The Vent’s worldly composition-root overlay while
adding temporal change without authored histories, dense planet-by-time cache,
unbounded events, or per-organism ecology. The Vent’s close ledger explicitly
deferred persistent vent evolution and reef succession; the existing climate
and worldgen precedents support pure time-derived fields and bounded replay.

**Alternatives discarded:** A static anchor cannot express migration; authored
event histories violate derived behavior and add data maintenance; a full
fluid/ecology model exceeds the pressure test; a universal habitat wrapper
would erase marine meanings.

**Ideonomy passes / overturns:** Two passes. Pass 1 used dimension-identification,
substitution, negation, map, and procedure; it added separate local and
transported channels and a failed-but-present source distinction. Pass 2 used
tree-finding, negation, substitution, timeline, and chart; it added phase
timeline semantics, bounded candidate-ring migration, and the observation
distinction between source cause and downstream consequence. No overturn.

**Capture actions:** Record the phase/memory choices in the approved spec and
implementation plan; add follow-ups for full current dynamics, reef
fragmentation, signal distortion, and species/metabolism. The existing
`WAT-vent-succession` registry row remains the idea anchor until the spec
supersedes it.

## Follow-ups

- Decide whether temporal memory belongs only to vent influence or also to
  plankton/reef aggregates after the non-vacuous probes measure the need.
- Preserve explicit marine nouns; do not introduce a universal habitat type.
- Verify every generated-artifact, tool, timing, and cost claim with its
  command output before placing it in the spec or plan.

## Task 1 — complete

**Evidence:** `fa4ffe78a` added the stable/dynamic boundary, non-vacuous
source inventory, disabled-overlay stability checks, and the ignored
behavioral-red temporal probe. The probe measured a real climate source change
from `22.777682501657186` to `22.78167180589651` after ten standard days, then
failed because the static Waterworld field stayed at the genesis value. The
focused suite passed 9 tests with 1 intentionally ignored probe; the commit
gate passed with `47.996s` wall time and the commit hook recorded a second
`40.977s` run in `docs/timings.md`. The task reviewer approved spec
compliance and code quality with no findings.

**Scope ruling:** The ignored red is load-bearing evidence and remains until
Task 2 connects time; it is not a skipped requirement. The timing row is
preserved as append-only campaign evidence even though the hook produced it
after the implementation commit was staged.

## #2 [G2] — How much temporal state should succession retain?

**Decision:** Use a fixed derived cycle of absent, nascent, active, weakening,
failed, and absent again. Seeded phase offsets and bounded duration weights are
generated once per admitted vent; exact `WorldTime` ticks select the current
phase. Migration selects from a fixed ordered candidate ring, while substrate
and source identity remain stable. Stocks are instantaneous except for a
bounded analytical residue based on time since the relevant active interval.

**Why:** The cycle makes time change observable and repeatable while avoiding
mutable state, authored histories, dense planet-by-time caching, and an
unbounded event queue. The analytical residue gives nutrient and reef
suitability a limited temporal memory without simulating organisms.

**Alternatives discarded:** Stateless fields would make succession only a
visual label; stored per-vent histories would introduce replay/state-management
cost; per-cell stock history would create the forbidden dense temporal product;
per-organism memory is outside the worldly pressure test.

**Ideonomy passes / overturns:** One focused pass for this design question,
using negation, dimension-identification, cross-domain re-instantiation,
spectrum, and atlas. It found no overturn; it added the distinction between
present field response and bounded residue, and between local phase and
transported influence.

**Capture actions:** Specify exact phase boundaries, candidate-ring bounds,
and residue formula only after the seam inventory identifies the available
configuration and time units. Test phase transitions at exact tick boundaries.

## #3 [G2] — Where should temporal Waterworld state live and how is it measured?

**Decision:** Split the overlay into stable `WaterWorld` data and a pure
`WaterWorldSnapshot` derived by `at(WorldTime)`. The composition root consumes
the existing terrain/climate accessors; snapshot queries derive phase, fields,
stocks, transport, and observations without mutation or caching.

**Why:** The split makes stable identity and present consequence explicit,
keeps exact kernel time on the query boundary, and gives tests a real seam for
source perturbation. The Vent's measured accessor inventory supplies the
precedent; its source-isolation protocol supplies the test direction.

**Alternatives discarded:** Rebuilding and mutating the entire overlay per
query obscures purity; storing snapshots by time creates a dense temporal
cache; putting the model in a domain violates sibling-domain layering; making
rendering derive fields would make observations a second producer.

**Ideonomy passes / overturns:** One focused combination/spectrum pass. No
overturn. It added the categorical-state/continuous-phase-position split and
the explicit distinction between derived natural history and authored events.

**Capture actions:** The spec must state the static/dynamic API boundary and
the source-first perturbation protocol. The plan must make Stage 1 red probes
behavioral against existing accessors before introducing the new snapshot.

## #4 [G4] — How should the approved spec be executed?

**Decision:** Execute four reviewable stages: seam inventory and behavioral-red
probes; deterministic succession and independent local fields; bounded stocks
and current-mediated redistribution; observation, performance evidence,
documentation, and close. Use one focused task boundary per stage and commit
each stage before the next.

**Why:** The current merged Waterworld seam already supplies substrate, fields,
vents, stocks, propagation, and observation. The plan can therefore extend
those files incrementally, preserving existing names and isolating failures by
temporal concern. The stage ordering follows the approved spec and The Vent's
source-first test discipline.

**Alternatives discarded:** A rewrite of the static overlay would mix a known
green predecessor with the new temporal experiment; one large task would make
stream/order and non-vacuity regressions hard to localize; a new domain or
universal wrapper would violate layering and the scope decision.

**Ideonomy passes / overturns:** One G4 decomposition pass using
decomposability, direction, and source-first evidence. No overturn. It added
the explicit static/dynamic API boundary, exact tick-boundary tests, and an
independent absent-overlay compatibility task.

**Capture actions:** Plan saved under `docs/superpowers/plans/`; the unrelated
root `IMPLEMENTATION_PLAN.md` remains untouched. Execution uses
subagent-driven development with review checkpoints, local `make gate-commit`,
and queued Sluice stage/merge requests only.

## Task 1 — Temporal seam inventory and behavioral red

The approved stable/dynamic boundary now exists without an additional
execution ruling: `WaterWorld` retains stable substrate and admitted
`WaterVent` source data, while the pure `WaterWorld::at` call returns a
`WaterWorldSnapshot`. Stage 1 deliberately delegates to the predecessor's
genesis-derived fields, stocks, vent activity, and propagation. It adds no
draw, label, cache, temporal formula, or admission change. `VentState` names
the five approved categorical states for Stage 2; the Stage 1 snapshot reports
the predecessor's admitted vents as active.

The seed-42 inventory produced these non-vacuous witnesses:

- substrate 83,997; seabed 29,679; open column 54,318;
- open-column/seabed depth distinctions 54,318 across 3 marine depth bands;
- marine vertices 29,679, with a separate non-marine witness;
- nonzero marine currents 29,677;
- marine boundaries 2,219; edifices 403; terrain-feature contexts 29,667.

The behavioral-red command was:

```text
cargo test -p hornvale-worldgen --test suite waterworld::temporal_red::future_climate_temperature_reaches_the_present_field_readout -- --ignored --nocapture
```

It exited 101 after compiling and running the assertion. The exact consumed
source at vertex 0 changed from `22.777682501657186` °C at genesis to
`22.78167180589651` °C at ten standard days, then the downstream assertion
failed because both static snapshot fields remained `22.777682501657186` °C.
This is the intended behavioral red; Stage 2 owns connecting that input and
removing the ignore.

The normal focused command was:

```text
cargo test -p hornvale-worldgen --test suite waterworld:: -- --nocapture
```

It exited 0 with 9 passed, 0 failed, and the one documented temporal red
ignored. A preceding attempt did not count as the red because the newly added
depth-band inventory used `BTreeSet<Stratum>` and failed to compile;
`Stratum` is not `Ord`. Replacing that test-only set with stable first-seen
vector membership produced the behavioral evidence above.
