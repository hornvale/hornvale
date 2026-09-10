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

## Task 2 — deterministic succession and independent local fields

The stable overlay now derives a 100-standard-day succession cycle entirely
from existing stable inputs. Its exact integer-tick intervals are 20 days
absent, 15 nascent, 30 active, 20 weakening, and 15 failed. Each admitted
source stores a phase offset derived from the existing seeded strength,
temperature, and chemistry values; the existing seeded
`waterworld/vent/v1` admission/strength/temperature/chemistry draw order is
unchanged. No new stream label or draw was earned.

The candidate ring contains the stable anchor and at most four one-hop marine
vertices, sorted by vertex key. Nascent, active, and weakening select at most
one ring entry; absent and failed select none. A red test caught the first
builder truncating after the final sort, which could evict a high-key anchor.
The corrected builder limits neighbours before adding the anchor and sorting.
Migration therefore changes only the snapshot influence position, never the
stable source or substrate vectors.

Local thermal and chemistry terms are separate. Thermal contribution is
additive; chemistry uses a bounded availability blend. Source-isolation tests
first prove the chosen source mutation, then require exactly one local field
to change and require light, pressure, salinity, current, and the other vent
term to remain equal. A first chemistry witness was rejected because its
selected seabed already had edifice chemistry at the bounded ceiling; the
replacement explicitly requires open chemistry capacity. Failed-state source
mutations leave snapshot fields unchanged while the source and seabed remain
present.

The implementation ruling received one ideonomy pass using organon-construction
as a cycle over reversibility and materiality. No overturn. It confirmed two
closure constraints: the physical anchor/source identity spans every state,
and only the informational state plus local influence disappear and renew.
No follow-up was created; stocks/residue/transport remain Task 3 and rendering
remains Task 4.

Behavioral reds were observed before their corresponding production behavior:

- exact-boundary succession failed `left: Active`, `right: Absent`;
- chemistry and thermal source tests each failed because the changed source
  reached no local field;
- neutralising migration selection failed with both adjacent states at
  `Some(Vertex(85))`;
- the all-source anchor assertion failed against the truncate-after-sort ring;
- bounded chemistry failed until local contributions used the bounded blend.

The focused green commands reported 2 succession, 3 field, 1 migration, and 1
determinism test passing. The former ignored temporal probe is now ordinary and
passed with the same real climate witness: vertex 0 changed from
`22.777682501657186` to `22.78167180589651` degrees Celsius after ten standard
days. The corrected complete Waterworld run reported 18 passed, 0 failed, 0
ignored. Genesis state witness counts in enum order were `[103, 105, 193, 134,
88]` (623 admitted sources total). A source-first cross-seed test held a
shared admitted anchor constant, proved its seeded source fields changed, and
then proved its phase offset changed.

Stream compatibility was checked with:

```text
cargo run --quiet -p hornvale -- streams | diff -u book/src/reference/stream-manifest-generated.md -
```

It exited 0 with empty output: the generated manifest is byte-identical and
`windows/worldgen/src/streams.rs` is unchanged.

## Task 2 review correction

The first review rejected the vertex-only phase offset as not seed-derived.
The correction uses the three already-consumed seeded vent draws, mixed into
the fixed cycle range, without adding a draw or changing stream order. The
second review found no remaining implementation issue; it required this
ledger correction so the durable record names the actual source inputs and
the corrected evidence.

## Task 3 — bounded temporal stocks and current transport

Snapshot stocks are now derived from present fields and stable substrate
inputs as four separate bounded aggregates: plankton from light,
chemosynthetic bloom from chemistry, nutrient reserve from terrain features
plus local and transported availability, and reef/kelp suitability from
seabed identity, temperature, chemistry, and nutrients. Local source
influence and transported influence remain separate aligned values. The four
source-first reds first proved that depth, vent chemistry, terrain nutrients,
and the substrate/temperature/chemistry witness changed; with snapshot stocks
still copied from genesis, their downstream values remained respectively
`0.378...`, `0`, `0.25`, and `1` on both sides. The production derivation made
each downstream assertion change while retaining finite `[0, 1]` bounds.

The required memory probe compared one stable source in active, weakening,
and failed states. Its instantaneous local stock strictly declined and reached
zero at failed, so present stocks already distinguish all three states. No
analytical residue term, cache, or additional stream was earned.

Current transport is an analytical snapshot pass, not a fluid solver or event
queue. Each active source follows existing current fields through a stable
neighbour order for at most three hops with `0.5` attenuation. The source
perturbation test first changes an actual current vector, then requires the
transported aggregate to change while substrate and vent identities remain
equal. Repeated and reordered snapshot/transport queries remain pure.

Counters are incremented at the candidate-neighbour inspection, ambient-field
refresh, stock derivation, and propagation-neighbour inspection loops.
Observation remains zero because Task 4 owns that loop. The bounded cost probe
measured one source/one hop at 6 propagation inspections and two sources/three
hops at 36, with accepted samples bounded by `sources * hops`; the active
snapshot refreshed and derived exactly one row per substrate row, while the
disabled overlay reported all-zero counters.

The focused command
`cargo test -p hornvale-worldgen --test suite waterworld::` reported 29 passed,
0 failed, 0 ignored. The stream-manifest diff exited 0 with empty output, so
no stream label or draw-order change was introduced.

### Task 3 review correction

The first Task 3 review rejected four pieces of evidence. The memory probe had
compared the local influence input rather than a downstream aggregate; the
transport attenuation read global `current[0]` rather than the chosen edge;
stock tests did not freeze unrelated outputs; and the snapshot copied a
stable construction counter while its cost comparison changed source count,
hop count, and attenuation together.

The corrected memory probe pins one stable vent to one candidate position and
compares its downstream nutrient and reef/kelp aggregates across active,
weakening, and failed snapshots. Both aggregates strictly decline across all
three states. That downstream evidence, rather than the local input, earns the
no-residue ruling.

Transport now normalizes each candidate edge, chooses by current/edge dot
product with stable vertex tie-breaking, rejects a best alignment at or below
zero, and attenuates by `configured attenuation * clamp(alignment, 0, 1)`.
Because the edge is unit length, alignment carries both direction and current
magnitude while the clamp bounds the multiplier. The source-first tests prove
that zero current transports nothing and that increasing a current along one
negative-x marine edge increases transported influence without changing the
chosen vertex, substrate, or vent identity.

Each stock source test now freezes unrelated fields and stocks. The light test
allows only depth-derived field companions while requiring only plankton to
change among stocks; chemistry allows bloom and reef/kelp; terrain nutrients
allow nutrients and reef/kelp; and the reef/kelp test perturbs substrate,
temperature, and chemistry independently.

Snapshot counters now increment in the present refresh, phase/source,
propagation, and stock loops. The seed-42 snapshot measured 623 phase/source
rings, 83,997 refreshes, 7,038 propagation candidates, and 83,997 stock rows.
One source measured 6 versus 18 propagation candidates at one versus three
hops; at three hops, one versus two sources measured 18 versus 36. Each pair
changes one bound only. The corrected focused suite reported 29 passed, 0
failed, and 0 ignored.

## Task 4 — observation and close evidence

The temporal snapshot observer is a pure consumer of stable source data and a
`WaterWorldSnapshot`. Ordinary output reports present marine substrate,
aggregate stocks, and current transport without naming causes. Diagnostic
output adds the five-state phase counts, stable vent provenance, local versus
transported stock split, and explicitly labels phase/source as inferred and
uncertain at observation scale. Absent contribution, failed contribution, and
zero ambient baseline are named separately.

The focused Waterworld suite reported 32 passed, 0 failed, and 0 ignored,
including repeated and reordered ordinary/diagnostic calls and nonzero
observation-loop counters. `make docs-tests` reported 75 passed, 0 failed.
The stream manifest diff and generated audit freshness checks exited 0. The
implementation commit was `c1354be25`; its required `make gate-commit`
completed with 1,425 sub-floor tests passed and `wall=228.694s ... rc=0`.
